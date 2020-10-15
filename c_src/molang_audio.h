#ifndef __MOLANG_H_AUDIO__
#define __MOLANG_H_AUDIO__ 1

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <AL/al.h>

#ifndef NDEBUG
#define MOLANG_AUDIO_LIBRARY_ERROR() do {                                                                           \
    switch (alGetError()) {                                                                                         \
        case AL_INVALID_NAME:                                                                                       \
            L("invalid name parameter\r\n");                                                                        \
            abort();                                                                                                \
        case AL_INVALID_ENUM:                                                                                       \
            L("invalid parameter\r\n");                                                                             \
            abort();                                                                                                \
        case AL_INVALID_VALUE:                                                                                      \
            L("invalid enum parameter value\r\n");                                                                  \
            abort();                                                                                                \
        case AL_INVALID_OPERATION:                                                                                  \
            L("illegal call\r\n");                                                                                  \
            abort();                                                                                                \
        case AL_OUT_OF_MEMORY:                                                                                      \
            L("unable to allocate memory\r\n");                                                                     \
            abort();                                                                                                \
    }                                                                                                               \
} while (0)
#else
#define MOLANG_AUDIO_LIBRARY_ERROR()
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

#endif
