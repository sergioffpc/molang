#ifndef __MOLANG_H__
#define __MOLANG_H__ 1

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <AL/al.h>
#include <GLES3/gl3.h>

#ifndef NDEBUG
#define L(fmt, ...) do {                                                                                            \
    struct timespec ts;                                                                                             \
    clock_gettime(CLOCK_MONOTONIC, &ts);                                                                            \
    fprintf(stderr, "[%ld.%ld] %s:%s:%d " fmt, ts.tv_sec, ts.tv_nsec, __FILE__, __func__, __LINE__, ##__VA_ARGS__); \
} while (0)
#else
#define L(fmt, ...)
#endif

#ifndef NDEBUG
inline void MOLANG_AUDIO_LIBRARY_ERROR()
{
    switch (alGetError()) {
        case AL_INVALID_NAME:
            L("invalid name parameter\r\n");
            abort();
        case AL_INVALID_ENUM:
            L("invalid parameter\r\n");
            abort();
        case AL_INVALID_VALUE:
            L("invalid enum parameter value\r\n");
            abort();
        case AL_INVALID_OPERATION:
            L("illegal call\r\n");
            abort();
        case AL_OUT_OF_MEMORY:
            L("unable to allocate memory\r\n");
            abort();
    }
}
#else
#define MOLANG_AUDIO_LIBRARY_ERROR()
#endif

#ifndef NDEBUG
inline void MOLANG_GRAPHICS_LIBRARY_ERROR()
{
    switch (glGetError()) {
        case GL_INVALID_ENUM:
            L("an unacceptable value is specified for an enumerated argument\r\n");
            abort();
        case GL_INVALID_VALUE:
            L("a numeric argument is out of range\r\n");
            abort();
        case GL_INVALID_OPERATION:
            L("the specified operation is not allowed in the current state\r\n");
            abort();
        case GL_INVALID_FRAMEBUFFER_OPERATION:
            L("the framebuffer object is not complete\r\n");
            abort();
        case GL_OUT_OF_MEMORY:
            L("there is not enough memory left to execute the command\r\n");
            abort();
    }
}
#else
#define MOLANG_GRAPHICS_LIBRARY_ERROR()
#endif

#define MOLANG_AUDIO_ERL_DRV_BUFFER_CREATE_FN   0x00
#define MOLANG_AUDIO_ERL_DRV_BUFFER_DESTROY_FN  0x01

#define MOLANG_AUDIO_ERL_DRV_EMITTER_CREATE_FN              0x10
#define MOLANG_AUDIO_ERL_DRV_EMITTER_DESTROY_FN             0x11
#define MOLANG_AUDIO_ERL_DRV_EMITTER_PLAY_FN                0x12
#define MOLANG_AUDIO_ERL_DRV_EMITTER_PAUSE_FN               0x13
#define MOLANG_AUDIO_ERL_DRV_EMITTER_STOP_FN                0x14
#define MOLANG_AUDIO_ERL_DRV_EMITTER_LOOPING_FN             0x15
#define MOLANG_AUDIO_ERL_DRV_EMITTER_POSITION_FN            0x16
#define MOLANG_AUDIO_ERL_DRV_EMITTER_VELOCITY_FN            0x17
#define MOLANG_AUDIO_ERL_DRV_EMITTER_DIRECTION_FN           0x18

#define MOLANG_AUDIO_ERL_DRV_LISTENER_POSITION_FN       0x20
#define MOLANG_AUDIO_ERL_DRV_LISTENER_VELOCITY_FN       0x21
#define MOLANG_AUDIO_ERL_DRV_LISTENER_ORIENTATION_FN    0x22

extern uint32_t molang_audio_buffer_create  (const char *file_name);
extern void     molang_audio_buffer_destroy (uint32_t emitter_handler);

extern uint32_t molang_audio_emitter_create     (uint32_t buffer_handler);
extern void     molang_audio_emitter_destroy    (uint32_t emitter_handler);
extern void     molang_audio_emitter_play       (uint32_t emitter_handler);
extern void     molang_audio_emitter_pause      (uint32_t emitter_handler);
extern void     molang_audio_emitter_stop       (uint32_t emitter_handler);
extern void     molang_audio_emitter_looping    (uint32_t emitter_handler, bool looping);
extern void     molang_audio_emitter_position   (uint32_t emitter_handler, float x, float y);
extern void     molang_audio_emitter_velocity   (uint32_t emitter_handler, float x, float y);
extern void     molang_audio_emitter_direction  (uint32_t emitter_handler, float x, float y);

extern void     molang_audio_listener_position      (float x, float y);
extern void     molang_audio_listener_velocity      (float x, float y);
extern void     molang_audio_listener_orientation   (float x, float y);

#define GRAPHICS_ERL_DRV_IMAGE_CREATE_FN    0x00
#define GRAPHICS_ERL_DRV_IMAGE_DESTROY_FN   0x01

extern uint32_t molang_graphics_image_create    (const char *file_name);
extern void     molang_graphics_image_destroy   (uint32_t image_handler);

extern void     molang_graphics_renderer_initialize (void);
extern void     molang_graphics_renderer_terminate  (void);

extern void     molang_graphics_renderer_draw   (void);

extern uint32_t molang_graphics_renderer_object_create   (uint32_t image_handler);
extern void     molang_graphics_renderer_object_destroy  (uint32_t object_handler);
extern void     molang_graphics_renderer_object_position (uint32_t object_handler, float x, float y);

#endif