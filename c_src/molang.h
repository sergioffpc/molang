#ifndef __MOLANG_H__
#define __MOLANG_H__ 1

#include <stdio.h>
#include <time.h>

#include "molang_audio.h"
#include "molang_graphics.h"

#ifndef NDEBUG
#define L(fmt, ...) do {                                                                                            \
    struct timespec ts;                                                                                             \
    clock_gettime(CLOCK_MONOTONIC, &ts);                                                                            \
    fprintf(stderr, "[%ld.%ld] %s:%s:%d " fmt, ts.tv_sec, ts.tv_nsec, __FILE__, __func__, __LINE__, ##__VA_ARGS__); \
} while (0)
#else
#define L(fmt, ...)
#endif

#endif
