ERL_SRC_DIR := src
ERL_INCLUDE_DIR := $(ERL_SRC_DIR)

C_SRC_DIR := c_src
C_INCLUDE_DIR := $(C_SRC_DIR)
BUILD_DIR := ebin

CC := gcc
CFLAGS := -pedantic-errors -Wall -Wextra -Werror
CPPFLAGS := -I $(C_INCLUDE_DIR)/ -D LEVEL1_DCACHE_LINESIZE=$(shell getconf LEVEL1_DCACHE_LINESIZE) -D PAGE_SIZE=$(shell getconf PAGE_SIZE)
LDFLAGS := -fpic -shared -pthread
LDLIBS := -lrt

ERLC := erlc
ERLCFLAGS := -Wall -Werror -I $(ERL_INCLUDE_DIR)/

.PHONY: debug
debug: CPPFLAGS += -g
debug: all

.PHONY: release
release: CPPFLAGS += -DNDEBUG -O3
release: all

.PHONY: all
all: molang_audio.so molang_graphics.so
	$(ERLC) $(ERLCFLAGS) -o $(BUILD_DIR)/ $(ERL_SRC_DIR)/*.erl
	@cp $(ERL_SRC_DIR)/molang.app.src $(BUILD_DIR)/molang.app

molang_audio.so: $(C_SRC_DIR)/molang.h $(C_SRC_DIR)/molang_audio.c $(C_SRC_DIR)/molang_audio_erl_drv.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -Wl,--whole-archive -lei -Wl,--no-whole-archive -o $(BUILD_DIR)/$@ $^ $(LDFLAGS) $(LDLIBS) \
		$(shell pkg-config --libs openal flac)

molang_graphics.so: $(C_SRC_DIR)/molang.h $(C_SRC_DIR)/molang_graphics.c $(C_SRC_DIR)/molang_graphics_erl_drv.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -Wl,--whole-archive -lei -Wl,--no-whole-archive -o $(BUILD_DIR)/$@ $^ $(LDFLAGS) $(LDLIBS) \
		$(shell pkg-config --libs egl gl libpng x11)

.PHONY: clean
clean:
	@rm -f $(BUILD_DIR)/*
