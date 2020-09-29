#ifndef __MOLANG_H__
#define __MOLANG_H__ 1

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef NDEBUG
#define L(fmt, ...) do {                                                                                            \
    struct timespec ts;                                                                                             \
    clock_gettime(CLOCK_MONOTONIC, &ts);                                                                            \
    fprintf(stderr, "[%ld.%ld] %s:%s:%d " fmt, ts.tv_sec, ts.tv_nsec, __FILE__, __func__, __LINE__, ##__VA_ARGS__); \
} while (0)
#else
#define L(fmt, ...)
#endif

#define MOLANG_AUDIO_ERL_DRV_BUFFER_CREATE_FN       0x00
#define MOLANG_AUDIO_ERL_DRV_BUFFER_DESTROY_FN      0x01

#define MOLANG_AUDIO_ERL_DRV_EMITTER_CREATE_FN      0x10
#define MOLANG_AUDIO_ERL_DRV_EMITTER_DESTROY_FN     0x11
#define MOLANG_AUDIO_ERL_DRV_EMITTER_PLAY_FN        0x12
#define MOLANG_AUDIO_ERL_DRV_EMITTER_STOP_FN        0x13
#define MOLANG_AUDIO_ERL_DRV_EMITTER_POSITION_FN    0x14
#define MOLANG_AUDIO_ERL_DRV_EMITTER_VELOCITY_FN    0x15
#define MOLANG_AUDIO_ERL_DRV_EMITTER_DIRECTION_FN   0x16

#define MOLANG_AUDIO_ERL_DRV_LISTENER_POSITION_FN   0x20
#define MOLANG_AUDIO_ERL_DRV_LISTENER_VELOCITY_FN   0x21

extern uint32_t molang_audio_buffer_create  (const char *file_name);
extern void     molang_audio_buffer_destroy (uint32_t emitter_handler);

extern uint32_t molang_audio_emitter_create     (uint32_t buffer_handler);
extern void     molang_audio_emitter_destroy    (uint32_t emitter_handler);
extern void     molang_audio_emitter_play       (uint32_t emitter_handler);
extern void     molang_audio_emitter_stop       (uint32_t emitter_handler);
extern void     molang_audio_emitter_position   (uint32_t emitter_handler, float x, float y);
extern void     molang_audio_emitter_velocity   (uint32_t emitter_handler, float x, float y);
extern void     molang_audio_emitter_direction  (uint32_t emitter_handler, float x, float y);

extern void     molang_audio_listener_position  (float x, float y);
extern void     molang_audio_listener_velocity  (float x, float y);

#define GRAPHICS_ERL_DRV_TEXTURE_BUFFER_CREATE_FN   0x00

#endif