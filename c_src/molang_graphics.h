#ifndef __MOLANG_H_GRAPHICS__
#define __MOLANG_H_GRAPHICS__ 1

#include <stdint.h>
#include <stdlib.h>

#include <GLES3/gl31.h>

#ifndef NDEBUG
#define MOLANG_GRAPHICS_LIBRARY_ERROR() do {                                                                        \
    switch (glGetError()) {                                                                                         \
        case GL_INVALID_ENUM:                                                                                       \
            L("an unacceptable value is specified for an enumerated argument\r\n");                                 \
            abort();                                                                                                \
        case GL_INVALID_VALUE:                                                                                      \
            L("a numeric argument is out of range\r\n");                                                            \
            abort();                                                                                                \
        case GL_INVALID_OPERATION:                                                                                  \
            L("the specified operation is not allowed in the current state\r\n");                                   \
            abort();                                                                                                \
        case GL_INVALID_FRAMEBUFFER_OPERATION:                                                                      \
            L("the framebuffer object is not complete\r\n");                                                        \
            abort();                                                                                                \
        case GL_OUT_OF_MEMORY:                                                                                      \
            L("there is not enough memory left to execute the command\r\n");                                        \
            abort();                                                                                                \
    }                                                                                                               \
} while (0)
#else
#define MOLANG_GRAPHICS_LIBRARY_ERROR()
#endif

#define GRAPHICS_ERL_DRV_IMAGE_CREATE_FN    0x00
#define GRAPHICS_ERL_DRV_IMAGE_DESTROY_FN   0x01

#define GRAPHICS_ERL_DRV_OBJECT_CREATE_FN       0x10
#define GRAPHICS_ERL_DRV_OBJECT_DESTROY_FN      0x11
#define GRAPHICS_ERL_DRV_OBJECT_POSITION_FN     0x12
#define GRAPHICS_ERL_DRV_OBJECT_VELOCITY_FN     0x13
#define GRAPHICS_ERL_DRV_OBJECT_DIRECTION_FN    0x14

extern void     molang_graphics_initialize (int width, int height);
extern void     molang_graphics_terminate  (void);

extern uint32_t molang_graphics_image_create    (const char *file_name);
extern void     molang_graphics_image_destroy   (uint32_t image_handler);

extern uint32_t molang_graphics_object_create       (uint32_t image_handler);
extern void     molang_graphics_object_destroy      (uint32_t object_handler);
extern void     molang_graphics_object_position     (uint32_t object_handler, float x, float y);
extern void     molang_graphics_object_velocity     (uint32_t object_handler, float x, float y);
extern void     molang_graphics_object_direction    (uint32_t object_handler, float x, float y);

extern void     molang_graphics_renderer_append (uint32_t object_handler);
extern void     molang_graphics_renderer_flush  (void);

#endif
