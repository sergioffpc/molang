#include <errno.h>
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

static GLuint graphics_renderer_vertex_shader;
static GLuint graphics_renderer_fragment_shader;
static GLuint graphics_renderer_shader_program;

void molang_graphics_renderer_initialize()
{
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

    const char *vertex_shader_source = "#version 300 es     \n"
        "in vec3 v_position;                                \n"
        "void main() {                                      \n"
        "   gl_Position = vec4(v_position, 1.0);            \n"
        "}                                                  \n";
    graphics_renderer_vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glShaderSource(graphics_renderer_vertex_shader, 1, &vertex_shader_source, NULL);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glCompileShader(graphics_renderer_vertex_shader);
    // TODO CHECK ERRORS USING

    const char *fragment_shader_source = "#version 300 es   \n"
        "out vec4 f_color;                                  \n"
        "void main() {                                      \n"
        "   f_color = vec4(0, 1, 0, 1.0);                   \n"
        "}                                                  \n";
    graphics_renderer_fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glShaderSource(graphics_renderer_fragment_shader, 1, &fragment_shader_source, NULL);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glCompileShader(graphics_renderer_fragment_shader);
    // TODO CHECK ERRORS USING

    graphics_renderer_shader_program = glCreateProgram();
    MOLANG_GRAPHICS_LIBRARY_ERROR();
    glAttachShader(graphics_renderer_shader_program, graphics_renderer_vertex_shader);
    glAttachShader(graphics_renderer_shader_program, graphics_renderer_fragment_shader);
    glLinkProgram(graphics_renderer_shader_program);
    // TODO CHECK ERRORS USING
}

void molang_graphics_renderer_terminate()
{
    glDetachShader(graphics_renderer_shader_program, graphics_renderer_vertex_shader);
    glDetachShader(graphics_renderer_shader_program, graphics_renderer_fragment_shader);

    glDeleteShader(graphics_renderer_vertex_shader);
    glDeleteShader(graphics_renderer_fragment_shader);

    glDeleteProgram(graphics_renderer_shader_program);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(graphics_renderer_objects);
}

void molang_graphics_renderer_draw()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glUseProgram(graphics_renderer_shader_program);
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

            glGenVertexArrays(1, &(object->vertex_array));
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindVertexArray(object->vertex_array);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glEnableVertexAttribArray(0);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glBindBuffer(GL_ARRAY_BUFFER, object->vertex_buffer);
            MOLANG_GRAPHICS_LIBRARY_ERROR();
            glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, NULL);
            MOLANG_GRAPHICS_LIBRARY_ERROR();

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
