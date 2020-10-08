#include <errno.h>
#include <glob.h>
#include <string.h>

#include "molang.h"

typedef struct {
    GLuint  vertex_buffer;
    GLuint  vertex_array;
    GLuint  texture_buffer;

    GLfloat position[2];
} graphics_renderer_object_t;

#define GRAPHICS_RENDERER_OBJECTS_CAPACITY  512
static graphics_renderer_object_t **graphics_renderer_objects = NULL;

static GLuint graphics_renderer_program;

static int glob_errfunc(const char *epath, int eerrno)
{
    (void) epath;
    (void) eerrno;
    L("unable to find pathnames matching the pattern: %s: %s\r\n", epath, strerror(eerrno));
    abort();
}

static GLuint compile_shader(const char *pattern, GLuint shader_type)
{
#define GRAPHICS_RENDERER_SHADER_SOURCE_MAX_FILES   256
    GLchar *source[GRAPHICS_RENDERER_SHADER_SOURCE_MAX_FILES];
    GLint source_len[GRAPHICS_RENDERER_SHADER_SOURCE_MAX_FILES];

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

    if (globbuf.gl_pathc >= GRAPHICS_RENDERER_SHADER_SOURCE_MAX_FILES) {
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

void molang_graphics_renderer_initialize(int width, int height)
{
#ifndef NDEBUG
    struct gl_param {
        GLchar  *name;
        GLenum   target;
        GLenum   type;
    } params[] = {
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

    for (size_t i = 0; i < (sizeof(params) / sizeof(struct gl_param)); i++) {
        struct gl_param *param = &params[i];
        switch (param->type) {
            case GL_INT:
            {
                GLint value;

                glGetIntegerv(param->target, &value);
                MOLANG_GRAPHICS_LIBRARY_ERROR();

                L("%s %d\r\n", param->name, value);
            }
                break;
        }
    }
#endif

    const size_t objects_size = GRAPHICS_RENDERER_OBJECTS_CAPACITY * sizeof(graphics_renderer_object_t *);
    graphics_renderer_objects = aligned_alloc(PAGE_SIZE, objects_size);
    if (graphics_renderer_objects == NULL) {
        L("unable to allocate memory: %s\r\n", strerror(errno));
        abort();
    }

    glClearColor(0, 0, 1, 0);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glClearDepthf(1);

    glViewport(0, 0, width, height);

#define GRAPHICS_RENDERER_VERTEX_SHADER_SOURCE_FILES    "../priv/shader/*.vert"
    const GLuint vert_shader = compile_shader(GRAPHICS_RENDERER_VERTEX_SHADER_SOURCE_FILES, GL_VERTEX_SHADER);
#define GRAPHICS_RENDERER_FRAGMENT_SHADER_SOURCE_FILES  "../priv/shader/*.frag"
    const GLuint frag_shader = compile_shader(GRAPHICS_RENDERER_FRAGMENT_SHADER_SOURCE_FILES, GL_FRAGMENT_SHADER);

    graphics_renderer_program = glCreateProgram();
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glAttachShader(graphics_renderer_program, vert_shader);
    glAttachShader(graphics_renderer_program, frag_shader);
    glLinkProgram(graphics_renderer_program);

    GLint success = GL_FALSE;
    glGetProgramiv(graphics_renderer_program, GL_LINK_STATUS, &success);
    if (success == GL_FALSE) {
        GLint log_size = 0;
        glGetProgramiv(graphics_renderer_program, GL_INFO_LOG_LENGTH, &log_size);

        GLchar *error_log = malloc(log_size);
        if (error_log == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        glGetProgramInfoLog(graphics_renderer_program, log_size, &log_size, &error_log[0]);

        L("unable to link program: %s\r\n", error_log);
        abort();
    }

    /* Validate the shader program before using it.  Only do this during
     * initialization because it is quite computationally expensive.  */
    glValidateProgram(graphics_renderer_program);

    success = GL_FALSE;
    glGetProgramiv(graphics_renderer_program, GL_VALIDATE_STATUS, &success);
    if (success == GL_FALSE) {
        GLint log_size = 0;
        glGetProgramiv(graphics_renderer_program, GL_INFO_LOG_LENGTH, &log_size);

        GLchar *error_log = malloc(log_size);
        if (error_log == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }

        glGetProgramInfoLog(graphics_renderer_program, log_size, &log_size, &error_log[0]);

        L("unable to link program: %s\r\n", error_log);
        abort();
    }
}

void molang_graphics_renderer_terminate()
{
    GLint shader;

    glGetShaderiv(GL_VERTEX_SHADER, GL_SHADER_TYPE, &shader);
    glDetachShader(graphics_renderer_program, shader);
    glDeleteShader(shader);

    glGetShaderiv(GL_FRAGMENT_SHADER, GL_SHADER_TYPE, &shader);
    glDetachShader(graphics_renderer_program, shader);
    glDeleteShader(shader);

    glDeleteProgram(graphics_renderer_program);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(graphics_renderer_objects);
}

void molang_graphics_renderer_draw()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glUseProgram(graphics_renderer_program);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    for (uint32_t i = 0; i < GRAPHICS_RENDERER_OBJECTS_CAPACITY; i++) {
        graphics_renderer_object_t *object = graphics_renderer_objects[i];
        if (object != NULL) {
            glBindVertexArray(object->vertex_array);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glDrawArrays(GL_TRIANGLES, 0, 3);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
        }
    }
}

uint32_t molang_graphics_renderer_object_create(uint32_t image_handler)
{
    for (uint32_t i = 0; i < GRAPHICS_RENDERER_OBJECTS_CAPACITY; i++) {
        graphics_renderer_object_t *object = graphics_renderer_objects[i];
        if (object == NULL) {
            object = malloc(sizeof(graphics_renderer_object_t));
            if (object == NULL) {
                L("unable to allocate memory: %s\r\n", strerror(errno));
                abort();
            }

            const GLfloat vertices[] = {
                0, 0, 0,
                0, 1, 0,
                1, 1, 0,
                1, 0, 0,
            };
            glGenBuffers(1, &(object->vertex_buffer));
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindBuffer(GL_ARRAY_BUFFER, object->vertex_buffer);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindBuffer(GL_ARRAY_BUFFER, 0);

            glGenVertexArrays(1, &(object->vertex_array));
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindVertexArray(object->vertex_array);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
#define GRAPHICS_RENDERER_VERTEX_ATTRIB 0
            glEnableVertexAttribArray(GRAPHICS_RENDERER_VERTEX_ATTRIB);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindBuffer(GL_ARRAY_BUFFER, object->vertex_buffer);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glVertexAttribPointer(GRAPHICS_RENDERER_VERTEX_ATTRIB, 3, GL_FLOAT, GL_FALSE, 0, NULL);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindVertexArray(0);

            object->texture_buffer = image_handler;

            graphics_renderer_objects[i] = object;

            return i;
        }
    }

    L("unable to allocate object\r\n");
    abort();
}

void molang_graphics_renderer_object_destroy(uint32_t object_handler)
{
    graphics_renderer_object_t *object = graphics_renderer_objects[object_handler];
    glDeleteBuffers(1, &(object->vertex_buffer));
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glDeleteVertexArrays(1, &(object->vertex_array));
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(object);
    graphics_renderer_objects[object_handler] = NULL;
}

void molang_graphics_renderer_object_position(uint32_t object_handler, float x, float y)
{
    graphics_renderer_object_t *object = graphics_renderer_objects[object_handler];
    object->position[0] = x;
    object->position[1] = y;
}
