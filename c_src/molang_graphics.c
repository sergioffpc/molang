#include <errno.h>
#include <glob.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <png.h>

#include <GLES3/gl31.h>

#include "molang.h"

typedef struct {
    GLuint  vertex_buffer;
    GLint   vertex_buffer_size;
    GLuint  vertex_array;
    GLuint  texture_buffer;

    GLfloat position[2];
    GLfloat velocity[2];
    GLfloat direction[2];
} graphics_object_t;

#define GRAPHICS_OBJECTS_CAPACITY  512
static graphics_object_t **graphics_objects = NULL;

#define GRAPHICS_BUFFER_CAPACITY   512
static uint32_t *graphics_buffer = NULL;
static size_t    graphics_buffer_length = 0;

static GLuint graphics_program;

static int glob_errfunc(const char *epath, int eerrno)
{
    (void) epath;
    (void) eerrno;
    L("unable to find pathnames matching the pattern: %s: %s\r\n", epath, strerror(eerrno));
    abort();
}

#ifndef NDEBUG
static const char *gl_type_to_str(const GLenum type)
{
    switch (type) {
        case GL_BOOL:
            return "bool";
        case GL_INT:
            return "int";
        case GL_FLOAT:
            return "float";
        case GL_FLOAT_VEC2:
            return "vec2";
        case GL_FLOAT_VEC3:
            return "vec3";
        case GL_FLOAT_VEC4:
            return "vec4";
        case GL_FLOAT_MAT2:
            return "mat2";
        case GL_FLOAT_MAT3:
            return "mat3";
        case GL_FLOAT_MAT4:
            return "mat4";
        case GL_SAMPLER_2D:
            return "sampler2D";
        case GL_SAMPLER_3D:
            return "sampler3D";
        case GL_SAMPLER_CUBE:
            return "samplerCube";
        case GL_SAMPLER_2D_SHADOW:
            return "sampler2DShadow";
        default:
            L("invalid GL type\r\n");
            abort();
    }
}
#endif

static GLuint compile_shader(const char *pattern, GLuint shader_type)
{
#define GRAPHICS_SHADER_SOURCE_MAX_FILES   256
    GLchar *source[GRAPHICS_SHADER_SOURCE_MAX_FILES];
    GLint source_len[GRAPHICS_SHADER_SOURCE_MAX_FILES];

    glob_t globbuf;
    globbuf.gl_offs = 1;

    switch (glob(pattern, GLOB_ERR | GLOB_BRACE | GLOB_TILDE_CHECK, glob_errfunc, &globbuf)) {
        case GLOB_NOSPACE:
            L("out of memory: %s\r\n", pattern);
            abort();
        case GLOB_ABORTED:
            L("read error: %s\r\n", pattern);
            abort();
        case GLOB_NOMATCH:
            L("no found matches: %s\r\n", pattern);
            abort();
    }

    if (globbuf.gl_pathc >= GRAPHICS_SHADER_SOURCE_MAX_FILES) {
        L("maximum shader sources exceeded\r\n");
        abort();
    }

    for (size_t i = 0; i < globbuf.gl_pathc; i++) {
        FILE *fp;
        if ((fp = fopen(globbuf.gl_pathv[i], "rb")) == NULL) {
            L("unable to open file: %s: %s\r\n", globbuf.gl_pathv[i], strerror(errno));
            abort();
        }

        fseek(fp, 0, SEEK_END);
        source_len[i] = ftell(fp);
        fseek(fp, 0, SEEK_SET);

        source[i] = malloc(source_len[i]);
        if (source[i] == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        size_t count = fread(source[i], sizeof(char), source_len[i], fp);
        if (count != (size_t) source_len[i] && ferror(fp)) {
            L("unable to read file: %s\r\n", strerror(errno));
            abort();
        }

        fclose (fp);
    }

    GLuint shader = glCreateShader(shader_type);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glShaderSource(shader, globbuf.gl_pathc, (const GLchar **) source, source_len);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glCompileShader(shader);

    GLint success = 0;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (success == GL_FALSE) {
        GLint log_size = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_size);

        GLchar *error_log = malloc(log_size);
        if (error_log == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        glGetShaderInfoLog(shader, log_size, &log_size, &error_log[0]);

        L("unable to compile shader from source: %s: %s\r\n", pattern, error_log);
        abort();
    }

    for (size_t i = 0; i < globbuf.gl_pathc; i++) {
        free(source[i]);
    }

    globfree(&globbuf);

    return shader;
}

void molang_graphics_initialize(int width, int height)
{
#ifndef NDEBUG
    struct gl_param {
        GLchar  *name;
        GLenum   target;
        GLenum   type;
    } gl_params[] = {
        { .target = GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS,           .name = "GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS",         .type = GL_INT },
        { .target = GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS,          .name = "GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS",        .type = GL_INT },
        { .target = GL_MAX_COMPUTE_UNIFORM_BLOCKS,                  .name = "GL_MAX_COMPUTE_UNIFORM_BLOCKS",                .type = GL_INT },
        { .target = GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS,             .name = "GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS",           .type = GL_INT },
        { .target = GL_MAX_COMPUTE_UNIFORM_COMPONENTS,              .name = "GL_MAX_COMPUTE_UNIFORM_COMPONENTS",            .type = GL_INT },
        { .target = GL_MAX_COMPUTE_ATOMIC_COUNTERS,                 .name = "GL_MAX_COMPUTE_ATOMIC_COUNTERS",               .type = GL_INT },
        { .target = GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS,          .name = "GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS",        .type = GL_INT },
        { .target = GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS,     .name = "GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS",   .type = GL_INT },
        { .target = GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS,          .name = "GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS",        .type = GL_INT },
        { .target = GL_MAX_3D_TEXTURE_SIZE,                         .name = "GL_MAX_3D_TEXTURE_SIZE",                       .type = GL_INT },
        { .target = GL_MAX_ARRAY_TEXTURE_LAYERS,                    .name = "GL_MAX_ARRAY_TEXTURE_LAYERS",                  .type = GL_INT },
        { .target = GL_MAX_COLOR_TEXTURE_SAMPLES,                   .name = "GL_MAX_COLOR_TEXTURE_SAMPLES",                 .type = GL_INT },
        { .target = GL_MAX_COMBINED_ATOMIC_COUNTERS,                .name = "GL_MAX_COMBINED_ATOMIC_COUNTERS",              .type = GL_INT },
        { .target = GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS,    .name = "GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS",  .type = GL_INT },
        { .target = GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS,            .name = "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS",          .type = GL_INT },
        { .target = GL_MAX_COMBINED_UNIFORM_BLOCKS,                 .name = "GL_MAX_COMBINED_UNIFORM_BLOCKS",               .type = GL_INT },
        { .target = GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS,      .name = "GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS",    .type = GL_INT },
        { .target = GL_MAX_CUBE_MAP_TEXTURE_SIZE,                   .name = "GL_MAX_CUBE_MAP_TEXTURE_SIZE",                 .type = GL_INT },
        { .target = GL_MAX_DEPTH_TEXTURE_SAMPLES,                   .name = "GL_MAX_DEPTH_TEXTURE_SAMPLES",                 .type = GL_INT },
        { .target = GL_MAX_DRAW_BUFFERS,                            .name = "GL_MAX_DRAW_BUFFERS",                          .type = GL_INT },
        { .target = GL_MAX_ELEMENTS_INDICES,                        .name = "GL_MAX_ELEMENTS_INDICES",                      .type = GL_INT },
        { .target = GL_MAX_ELEMENTS_VERTICES,                       .name = "GL_MAX_ELEMENTS_VERTICES",                     .type = GL_INT },
        { .target = GL_MAX_FRAGMENT_ATOMIC_COUNTERS,                .name = "GL_MAX_FRAGMENT_ATOMIC_COUNTERS",              .type = GL_INT },
        { .target = GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS,          .name = "GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS",        .type = GL_INT },
        { .target = GL_MAX_FRAGMENT_UNIFORM_COMPONENTS,             .name = "GL_MAX_FRAGMENT_UNIFORM_COMPONENTS",           .type = GL_INT },
        { .target = GL_MAX_FRAGMENT_UNIFORM_VECTORS,                .name = "GL_MAX_FRAGMENT_UNIFORM_VECTORS",              .type = GL_INT },
        { .target = GL_MAX_FRAGMENT_UNIFORM_BLOCKS,                 .name = "GL_MAX_FRAGMENT_UNIFORM_BLOCKS",               .type = GL_INT },
        { .target = GL_MAX_FRAMEBUFFER_WIDTH,                       .name = "GL_MAX_FRAMEBUFFER_WIDTH",                     .type = GL_INT },
        { .target = GL_MAX_FRAMEBUFFER_HEIGHT,                      .name = "GL_MAX_FRAMEBUFFER_HEIGHT",                    .type = GL_INT },
        { .target = GL_MAX_FRAMEBUFFER_SAMPLES,                     .name = "GL_MAX_FRAMEBUFFER_SAMPLES",                   .type = GL_INT },
        { .target = GL_MAX_INTEGER_SAMPLES,                         .name = "GL_MAX_INTEGER_SAMPLES",                       .type = GL_INT },
        { .target = GL_MAX_RENDERBUFFER_SIZE,                       .name = "GL_MAX_RENDERBUFFER_SIZE",                     .type = GL_INT },
        { .target = GL_MAX_SAMPLE_MASK_WORDS,                       .name = "GL_MAX_SAMPLE_MASK_WORDS",                     .type = GL_INT },
        { .target = GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS,          .name = "GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS",        .type = GL_INT },
        { .target = GL_MAX_TEXTURE_IMAGE_UNITS,                     .name = "GL_MAX_TEXTURE_IMAGE_UNITS",                   .type = GL_INT },
        { .target = GL_MAX_TEXTURE_LOD_BIAS,                        .name = "GL_MAX_TEXTURE_LOD_BIAS",                      .type = GL_INT },
        { .target = GL_MAX_TEXTURE_SIZE,                            .name = "GL_MAX_TEXTURE_SIZE",                          .type = GL_INT },
        { .target = GL_MAX_UNIFORM_BUFFER_BINDINGS,                 .name = "GL_MAX_UNIFORM_BUFFER_BINDINGS",               .type = GL_INT },
        { .target = GL_MAX_UNIFORM_BLOCK_SIZE,                      .name = "GL_MAX_UNIFORM_BLOCK_SIZE",                    .type = GL_INT },
        { .target = GL_MAX_UNIFORM_LOCATIONS,                       .name = "GL_MAX_UNIFORM_LOCATIONS",                     .type = GL_INT },
        { .target = GL_MAX_VARYING_COMPONENTS,                      .name = "GL_MAX_VARYING_COMPONENTS",                    .type = GL_INT },
        { .target = GL_MAX_VARYING_VECTORS,                         .name = "GL_MAX_VARYING_VECTORS",                       .type = GL_INT },
        { .target = GL_MAX_VERTEX_ATOMIC_COUNTERS,                  .name = "GL_MAX_VERTEX_ATOMIC_COUNTERS",                .type = GL_INT },
        { .target = GL_MAX_VERTEX_ATTRIBS,                          .name = "GL_MAX_VERTEX_ATTRIBS",                        .type = GL_INT },
        { .target = GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS,            .name = "GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS",          .type = GL_INT },
        { .target = GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS,              .name = "GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS",            .type = GL_INT },
        { .target = GL_MAX_VERTEX_UNIFORM_COMPONENTS,               .name = "GL_MAX_VERTEX_UNIFORM_COMPONENTS",             .type = GL_INT },
        { .target = GL_MAX_VERTEX_UNIFORM_VECTORS,                  .name = "GL_MAX_VERTEX_UNIFORM_VECTORS",                .type = GL_INT },
        { .target = GL_MAX_VERTEX_OUTPUT_COMPONENTS,                .name = "GL_MAX_VERTEX_OUTPUT_COMPONENTS",              .type = GL_INT },
        { .target = GL_MAX_VERTEX_UNIFORM_BLOCKS,                   .name = "GL_MAX_VERTEX_UNIFORM_BLOCKS",                 .type = GL_INT },
        { .target = GL_MAX_VIEWPORT_DIMS,                           .name = "GL_MAX_VIEWPORT_DIMS",                         .type = GL_INT },
        { .target = GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET,           .name = "GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET",         .type = GL_INT },
        { .target = GL_MAX_VERTEX_ATTRIB_BINDINGS,                  .name = "GL_MAX_VERTEX_ATTRIB_BINDINGS",                .type = GL_INT },
    };

    L("OpenGL ES parameters:\r\n");
    for (size_t i = 0; i < (sizeof(gl_params) / sizeof(struct gl_param)); i++) {
        struct gl_param *param = &gl_params[i];
        switch (param->type) {
            case GL_INT:
            {
                GLint value;

                glGetIntegerv(param->target, &value);
                MOLANG_GRAPHICS_LIBRARY_ERROR();

                L("\t%s %d\r\n", param->name, value);
            }
                break;
        }
    }
#endif

    const size_t objects_size = GRAPHICS_OBJECTS_CAPACITY * sizeof(graphics_object_t *);
    graphics_objects = aligned_alloc(PAGE_SIZE, objects_size);
    if (graphics_objects == NULL) {
        L("unable to allocate memory: %s\r\n", strerror(errno));
        abort();
    }
    explicit_bzero(graphics_objects, objects_size);

    const size_t buffer_size = GRAPHICS_BUFFER_CAPACITY * sizeof(uint32_t);
    graphics_buffer = aligned_alloc(PAGE_SIZE, buffer_size);
    if (graphics_buffer == NULL) {
        L("unable to allocate memory: %s\r\n", strerror(errno));
        abort();
    }
    explicit_bzero(graphics_buffer, buffer_size);

    graphics_buffer_length = 0;

    glClearColor(0, 0, 1, 0);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glClearDepthf(1);

    glViewport(0, 0, width, height);

#define GRAPHICS_VERTEX_SHADER_SOURCE_FILES     "../priv/shader/*.vert"
    const GLuint vert_shader = compile_shader(GRAPHICS_VERTEX_SHADER_SOURCE_FILES, GL_VERTEX_SHADER);
#define GRAPHICS_FRAGMENT_SHADER_SOURCE_FILES   "../priv/shader/*.frag"
    const GLuint frag_shader = compile_shader(GRAPHICS_FRAGMENT_SHADER_SOURCE_FILES, GL_FRAGMENT_SHADER);

    graphics_program = glCreateProgram();
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glAttachShader(graphics_program, vert_shader);
    glAttachShader(graphics_program, frag_shader);
    glLinkProgram(graphics_program);

    GLint link_status = GL_FALSE;
    glGetProgramiv(graphics_program, GL_LINK_STATUS, &link_status);
    if (link_status == GL_FALSE) {
        GLint log_size = 0;
        glGetProgramiv(graphics_program, GL_INFO_LOG_LENGTH, &log_size);

        GLchar *error_log = malloc(log_size);
        if (error_log == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        glGetProgramInfoLog(graphics_program, log_size, &log_size, &error_log[0]);

        L("unable to link program: %s\r\n", error_log);
        abort();
    }

    /* Validate the shader program before using it.  Only do this during
     * initialization because it is quite computationally expensive.  */
    glValidateProgram(graphics_program);

    GLint validate_status = GL_FALSE;
    glGetProgramiv(graphics_program, GL_VALIDATE_STATUS, &validate_status);
    if (validate_status == GL_FALSE) {
        GLint log_size = 0;
        glGetProgramiv(graphics_program, GL_INFO_LOG_LENGTH, &log_size);

        GLchar *error_log = malloc(log_size);
        if (error_log == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        glGetProgramInfoLog(graphics_program, log_size, &log_size, &error_log[0]);

        L("unable to link program: %s\r\n", error_log);
        abort();
    }

#ifndef NDEBUG
    GLint attached_shaders;
    glGetProgramiv(graphics_program, GL_ATTACHED_SHADERS, &attached_shaders);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    GLint active_attrs;
    glGetProgramiv(graphics_program, GL_ACTIVE_ATTRIBUTES, &active_attrs);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    GLint active_uniforms;
    glGetProgramiv(graphics_program, GL_ACTIVE_UNIFORMS, &active_uniforms);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    L("OpenGL ES program: %d link_status=%d validate_status=%d attached_shaders=%d active_attrs=%d active_uniforms=%d\r\n",
        graphics_program, link_status, validate_status, attached_shaders, active_attrs, active_uniforms);

    for (int i = 0; i < active_attrs; i++) {
#define GRAPHICS_PROGRAM_ACTIVE_ATTRIBUTES_NAME_MAX_LENGTH   256
        GLchar attr_name[GRAPHICS_PROGRAM_ACTIVE_ATTRIBUTES_NAME_MAX_LENGTH];
        GLsizei attr_name_len;
        GLint attr_size;
        GLenum attr_type;
        glGetActiveAttrib(graphics_program
                         ,i
                         ,GRAPHICS_PROGRAM_ACTIVE_ATTRIBUTES_NAME_MAX_LENGTH
                         ,&attr_name_len
                         ,&attr_size
                         ,&attr_type
                         ,attr_name);

        if (attr_size > 1) {
            for (int j = 0; j < attr_size; j++) {
                char *attr_array_name;
                asprintf(&attr_array_name, "%s[%d]", attr_name, j);
                int attr_loc = glGetAttribLocation(graphics_program, attr_array_name);
                L("\tattribute: %d name=%s type=%s location=%d\r\n", i, attr_array_name, gl_type_to_str(attr_type), attr_loc);
                free(attr_array_name);
            }
        } else {
            int attr_loc = glGetAttribLocation(graphics_program, attr_name);
            L("\tattribute: %d name=%s type=%s location=%d\r\n", i, attr_name, gl_type_to_str(attr_type), attr_loc);
        }
    }

    for (int i = 0; i < active_uniforms; i++) {
#define GRAPHICS_PROGRAM_ACTIVE_UNIFORMS_NAME_MAX_LENGTH   256
        GLchar uniform_name[GRAPHICS_PROGRAM_ACTIVE_UNIFORMS_NAME_MAX_LENGTH];
        GLsizei uniform_name_len;
        GLint uniform_size;
        GLenum uniform_type;
        glGetActiveUniform(graphics_program
                          ,i
                          ,GRAPHICS_PROGRAM_ACTIVE_UNIFORMS_NAME_MAX_LENGTH
                          ,&uniform_name_len
                          ,&uniform_size
                          ,&uniform_type
                          ,uniform_name);

        if (uniform_size > 1) {
            for (int j = 0; j < uniform_size; j++) {
                char *uniform_array_name;
                asprintf(&uniform_array_name, "%s[%d]", uniform_name, j);
                int uniform_loc = glGetUniformLocation(graphics_program, uniform_array_name);
                L("\tuniform: %d name=%s type=%s location=%d\r\n", i, uniform_array_name, gl_type_to_str(uniform_type), uniform_loc);
                free(uniform_array_name);
            }
        } else {
            int uniform_loc = glGetUniformLocation(graphics_program, uniform_name);
            L("\tuniform: %d name=%s type=%s location=%d\r\n", i, uniform_name, gl_type_to_str(uniform_type), uniform_loc);
        }
    }
#endif
}

void molang_graphics_terminate()
{
    GLint shader;

    glGetShaderiv(GL_VERTEX_SHADER, GL_SHADER_TYPE, &shader);
    glDetachShader(graphics_program, shader);
    glDeleteShader(shader);

    glGetShaderiv(GL_FRAGMENT_SHADER, GL_SHADER_TYPE, &shader);
    glDetachShader(graphics_program, shader);
    glDeleteShader(shader);

    glDeleteProgram(graphics_program);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(graphics_objects);
}

/*
 * Check to see if a file is a PNG file using png_sig_cmp().  png_sig_cmp()
 * returns zero if the image is a PNG, and nonzero otherwise.
 *
 * The function check_if_png(), returns nonzero (true) if the file can be
 * opened and is a PNG, and 0 (false) otherwise.
 *
 * If this call is successful, and you are going to keep the file open,
 * you should call png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK); once
 * you have created the png_ptr, so that libpng knows your application
 * has read that many bytes from the start of the file.  Make sure you
 * don't call png_set_sig_bytes() with more than 8 bytes read or give it
 * an incorrect number of bytes read, or you will either have read too
 * many bytes, or you are telling libpng to read the wrong number of
 * magic bytes.
 *
 * Many applications already read the first 2 or 4 bytes from the start
 * of the image to determine the file type, so it would be easiest just
 * to pass the bytes to png_sig_cmp(), or even skip that if you know
 * you have a PNG file, and call png_set_sig_bytes().
 */
#define PNG_BYTES_TO_CHECK  4
static int check_if_png(FILE *fp)
{
    png_byte buf[PNG_BYTES_TO_CHECK];

    /* Read in some of the signature bytes.  */
    if (fread(buf, 1, PNG_BYTES_TO_CHECK, fp) != PNG_BYTES_TO_CHECK) {
        return 0;
    }

    /* Compare the first PNG_BYTES_TO_CHECK bytes of the signature.
     * Return nonzero (true) if they match.  */
    return !png_sig_cmp(buf, 0, PNG_BYTES_TO_CHECK);
}

uint32_t molang_graphics_image_create(const char *filename)
{
    /* Open the prospective PNG file.  */
    FILE *fp;
    if ((fp = fopen(filename, "rb")) == NULL) {
        L("unable to open PNG file: %s: %s\r\n", filename, strerror(errno));
        abort();
    }

    if (!check_if_png(fp)) {
        L("invalid PNG signature: %s\r\n", filename);
        abort();
    }

    /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  */
    png_structp png_ptr;
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Allocate/initialize the memory for image information.  */
    png_infop info_ptr;
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Set error handling if you are using the setjmp/longjmp method (this is
     * the normal method of doing things with libpng).  REQUIRED unless you
     * set up your own error handlers in the png_create_read_struct() earlier.  */
    if (setjmp(png_jmpbuf(png_ptr))) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Set up the input control if you are using standard C streams.  */
    png_init_io(png_ptr, fp);

    /* If we have already read some of the signature.  */
    png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK);

    /* If you have enough memory to read in the entire image at once,
     * and you need to specify only transforms that can be controlled
     * with one of the PNG_TRANSFORM_* bits (this presently excludes
     * quantizing, filling, setting background, and doing gamma
     * adjustment), then you can read the entire image (including
     * pixels) into the info structure with this call.  */
    const int png_transforms = PNG_TRANSFORM_STRIP_16 | PNG_TRANSFORM_PACKING | PNG_TRANSFORM_EXPAND;
    png_read_png(png_ptr, info_ptr, png_transforms, NULL);

    png_uint_32 width;
    png_uint_32 height;
    int bit_depth;
    int color_type;
    int interlace_method;
    png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, &interlace_method, NULL, NULL);

    L("load filename=%s width=%d height=%d bit_depth=%d color_type=%d interlace_method=%d\r\n"
           ,filename
           ,width
           ,height
           ,bit_depth
           ,color_type
           ,interlace_method);

    png_bytepp row_pointers = png_get_rows(png_ptr, info_ptr);

    size_t row_bytes = png_get_rowbytes(png_ptr, info_ptr);
    uint8_t *data_buffer = malloc(row_bytes * height);
    if (data_buffer == NULL) {
        L("unable to allocate memory: %s\r\n", strerror(errno));
        abort();
    }

    /* Align bytes as OpenGL expects them.  */
    for (png_uint_32 i = 0; i < height; i++) {
        memcpy(data_buffer + (row_bytes * i), row_pointers[i], row_bytes);
    }

    /* Clean up after the read, and free any memory allocated.  */
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    fclose(fp);

    GLint format;
    switch (color_type) {
        case PNG_COLOR_TYPE_RGBA:
            format = GL_RGBA;
            break;
        case PNG_COLOR_TYPE_RGB:
            format = GL_RGB;
            break;
        case PNG_COLOR_TYPE_GRAY:
            format = GL_LUMINANCE;
            break;
        case PNG_COLOR_TYPE_GRAY_ALPHA:
            format = GL_LUMINANCE_ALPHA;
            break;
        default:
            L("unsupported PNG color type: %d\r\n", color_type);
            abort();
    }

    GLuint texture;
    glGenTextures(1, &texture);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glBindTexture(GL_TEXTURE_2D, texture);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    glTexImage2D(GL_TEXTURE_2D
                ,0
                ,format
                ,width
                ,height
                ,0
                ,format
                ,GL_UNSIGNED_BYTE
                ,data_buffer);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(data_buffer);

    glGenerateMipmap(GL_TEXTURE_2D);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glBindTexture(GL_TEXTURE_2D, 0);

    return texture;
}

void molang_graphics_image_destroy(uint32_t image_handler)
{
    glDeleteTextures(1, &image_handler);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
}

uint32_t molang_graphics_object_create(uint32_t image_handler)
{
    for (uint32_t i = 0; i < GRAPHICS_OBJECTS_CAPACITY; i++) {
        graphics_object_t *object = graphics_objects[i];
        if (object == NULL) {
            object = malloc(sizeof(graphics_object_t));
            if (object == NULL) {
                L("unable to allocate memory: %s\r\n", strerror(errno));
                abort();
            }

            const GLfloat vertices[] = {
                 0.0,  0.5, 0.0,
                 0.5, -0.5, 0.0,
                -0.5, -0.5, 0.0,
            };

            glGenBuffers(1, &(object->vertex_buffer));
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindBuffer(GL_ARRAY_BUFFER, object->vertex_buffer);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
            MOLANG_GRAPHICS_LIBRARY_ERROR();

            glGetBufferParameteriv(GL_ARRAY_BUFFER, GL_BUFFER_SIZE, &(object->vertex_buffer_size));

            glGenVertexArrays(1, &(object->vertex_array));
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindVertexArray(object->vertex_array);
            MOLANG_GRAPHICS_LIBRARY_ERROR();

#define GRAPHICS_VERTEX_ATTRIB_VERTEX_LOC 0
            glEnableVertexAttribArray(GRAPHICS_VERTEX_ATTRIB_VERTEX_LOC);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glVertexAttribPointer(GRAPHICS_VERTEX_ATTRIB_VERTEX_LOC, 3, GL_FLOAT, GL_FALSE, 0, NULL);
            MOLANG_GRAPHICS_LIBRARY_ERROR();

            glBindVertexArray(0);
            glBindBuffer(GL_ARRAY_BUFFER, 0);

            object->texture_buffer = image_handler;

            graphics_objects[i] = object;
            L("load object address=0x%p vertex_buffer=%d vertex_buffer_size=%db vertex_array=%d texture_buffer=%d\r\n"
                ,(void *) object
                ,object->vertex_buffer
                ,object->vertex_buffer_size
                ,object->vertex_array
                ,object->texture_buffer);

            return i;
        }
    }

    L("unable to allocate object\r\n");
    abort();
}

void molang_graphics_object_destroy(uint32_t object_handler)
{
    graphics_object_t *object = graphics_objects[object_handler];
    glDeleteBuffers(1, &(object->vertex_buffer));
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glDeleteVertexArrays(1, &(object->vertex_array));
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(object);
    graphics_objects[object_handler] = NULL;
}

void molang_graphics_object_position(uint32_t object_handler, float x, float y)
{
    (void) object_handler;
    (void) x;
    (void) y;
}

void molang_graphics_object_velocity(uint32_t object_handler, float x, float y)
{
    (void) object_handler;
    (void) x;
    (void) y;
}

void molang_graphics_object_direction(uint32_t object_handler, float x, float y)
{
    (void) object_handler;
    (void) x;
    (void) y;
}

void molang_graphics_renderer_append(uint32_t object_handler)
{
    if (graphics_buffer_length == GRAPHICS_BUFFER_CAPACITY) {
        L("unable to append object to renderer\r\n");
        abort();
    }

    graphics_buffer[graphics_buffer_length++] = object_handler;
}

void molang_graphics_renderer_flush()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glUseProgram(graphics_program);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    for (size_t i = 0; i < graphics_buffer_length; i++) {
        const uint32_t object_index = graphics_buffer[i];
        const graphics_object_t *object = graphics_objects[object_index];
        if (object != NULL) {
            glBindVertexArray(object->vertex_array);
            MOLANG_GRAPHICS_LIBRARY_ERROR();

            glDrawArrays(GL_TRIANGLES, 0, 3);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
        }
    }

    graphics_buffer_length = 0;
}
