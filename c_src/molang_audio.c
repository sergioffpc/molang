#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <AL/al.h>
#include <FLAC/stream_decoder.h>

#include "molang.h"

typedef struct {
    uint8_t *data_buffer;
    size_t   data_size;
    size_t   data_cursor;

    FLAC__uint64    total_samples;
    FLAC__uint32    sample_rate;
    FLAC__uint32    channels;
    FLAC__uint32    bits_per_sample;
} audio_stream_client_data_t;

static FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder __attribute__((unused)), const FLAC__Frame *frame, const FLAC__int32 * const buffer[], void *client_data)
{
    audio_stream_client_data_t *data = (audio_stream_client_data_t*) client_data;

    if (data->total_samples == 0) {
        L("FLAC audio file without samples\r\n");
        return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
    }

    for (FLAC__uint32 sample = 0; sample < frame->header.blocksize; sample++) {
        for (FLAC__uint32 channel = 0; channel < frame->header.channels; channel++) {
            switch (data->bits_per_sample) {
                case 8:
                    *((uint8_t *) &(data->data_buffer[data->data_cursor])) = (uint8_t) buffer[channel][sample];
                    data->data_cursor += sizeof(FLAC__int8);
                    break;
                case 16:
                    *((uint16_t *) &(data->data_buffer[data->data_cursor])) = (uint16_t) buffer[channel][sample];
                    data->data_cursor += sizeof(FLAC__int16);
                    break;
                default:
                    return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
            }
        }
    }

    return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

static void metadata_callback(const FLAC__StreamDecoder *decoder __attribute__((unused)), const FLAC__StreamMetadata *metadata, void *client_data)
{
    audio_stream_client_data_t *data = (audio_stream_client_data_t*) client_data;

    /* In a valid FLAC file there will always be one STREAMINFO block, followed by zero or more other metadata blocks.  */
    if(metadata->type == FLAC__METADATA_TYPE_STREAMINFO) {
        data->total_samples = metadata->data.stream_info.total_samples;
        data->sample_rate = metadata->data.stream_info.sample_rate;
        data->channels = metadata->data.stream_info.channels;
        data->bits_per_sample = metadata->data.stream_info.bits_per_sample;

        data->data_size = (FLAC__uint32) (data->total_samples * data->channels * (data->bits_per_sample / 8));
        data->data_buffer = aligned_alloc(PAGE_SIZE, data->data_size);
        if (data->data_buffer == NULL) {
            L("unable to allocate memory: %s\r\n", strerror(errno));
            abort();
        }
        data->data_cursor = 0;
    }
}

static void error_callback(const FLAC__StreamDecoder *decoder __attribute__((unused)), FLAC__StreamDecoderErrorStatus status, void *client_data __attribute__((unused)))
{
    (void) status;
    L("error decoding FLAC audio file: %s\r\n", FLAC__StreamDecoderErrorStatusString[status]);
    abort();
}

uint32_t molang_audio_buffer_create(const char *filename)
{
    FLAC__StreamDecoder *decoder = 0;
    FLAC__StreamDecoderInitStatus init_status;

    if ((decoder = FLAC__stream_decoder_new()) == NULL) {
        L("unable to create FLAC decoder\r\n");
        abort();
    }

    FLAC__stream_decoder_set_md5_checking(decoder, true);

    audio_stream_client_data_t client_data = { 0 };
    init_status = FLAC__stream_decoder_init_file(decoder, filename, write_callback, metadata_callback, error_callback, &client_data);
    if (init_status != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
        L("unable to initialize FLAC decoder: %s\r\n", FLAC__StreamDecoderInitStatusString[init_status]);
        abort();
    }

    if (!FLAC__stream_decoder_process_until_end_of_stream(decoder)) {
        L("unable to decode FLAC file: %s: %s\r\n", filename, FLAC__StreamDecoderStateString[FLAC__stream_decoder_get_state(decoder)]);
        abort();
    }

    FLAC__stream_decoder_finish(decoder);
    FLAC__stream_decoder_delete(decoder);

    ALuint buffer;
    alGenBuffers(1, &buffer);
    MOLANG_AUDIO_LIBRARY_ERROR();

    ALenum format;
    if (client_data.channels == 1 && client_data.bits_per_sample == 8) {
        format = AL_FORMAT_MONO8;
    } else if (client_data.channels == 1 && client_data.bits_per_sample == 16) {
        format = AL_FORMAT_MONO16;
    } else if (client_data.channels == 2 && client_data.bits_per_sample == 8) {
        format = AL_FORMAT_STEREO8;
    } else if (client_data.channels == 2 && client_data.bits_per_sample == 16) {
        format = AL_FORMAT_STEREO16;
    } else {
        L("unsupported audio format\r\n");
        abort();
    }

    L("load filename=%s total_size=%ld total_samples=%ld sample_rate=%d channels=%d bits_per_sample=%d\r\n"
           ,filename
           ,client_data.data_size
           ,client_data.total_samples
           ,client_data.sample_rate
           ,client_data.channels
           ,client_data.bits_per_sample);

    /*
     * Fills a buffer with audio data, the pre-defined formats are PCM data.
     *
     * 8-bit PCM data is expressed as an unsigned value over the range 0 to 255, 128 being an audio output level
     * of zero.  16-bit PCM data is expressed as a signed value over the range -32768 to 32767, 0 being an audio
     * output level of zero.  Stereo data is expressed in interleaved format, left channel first.
     */
    alBufferData(buffer, format, client_data.data_buffer, client_data.data_size, client_data.sample_rate);
    MOLANG_AUDIO_LIBRARY_ERROR();

    free(client_data.data_buffer);

    return buffer;
}

void molang_audio_buffer_destroy(uint32_t buffer_handler)
{
    /* Buffers which are attached to a source can not be deleted.  */
    alDeleteBuffers(1, &buffer_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

uint32_t molang_audio_emitter_create(uint32_t buffer_handler)
{
    ALuint source;
    alGenSources(1, &source);
    MOLANG_AUDIO_LIBRARY_ERROR();

    alSourcei(source, AL_BUFFER, buffer_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();

    return source;
}

void molang_audio_emitter_destroy(uint32_t emitter_handler)
{
    /* A playing source can be deleted, the source will be stopped and then deleted.  */
    alSourceStop(emitter_handler);

    /* Detach the source from the buffers.  */
    alSourcei(emitter_handler, AL_BUFFER, 0);

    alDeleteSources(1, &emitter_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_play(uint32_t emitter_handler)
{
    /*
     * The playing source will have its state changed to AL_PLAYING.  When called on a source
     * which is already playing, the source will restart at the beginning.  When the attached
     * buffer(s) are done playing, the source will progress to the AL_STOPPED state.
     */
    alSourcePlay(emitter_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_pause(uint32_t emitter_handler)
{
    alSourcePause(emitter_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_stop(uint32_t emitter_handler)
{
    alSourceStop(emitter_handler);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_looping(uint32_t emitter_handler, bool looping)
{
    alSourcei(emitter_handler, AL_LOOPING, looping);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_position(uint32_t emitter_handler, float x, float y)
{
    alSource3f(emitter_handler, AL_POSITION, x, y, 0);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_velocity(uint32_t emitter_handler, float x, float y)
{
    alSource3f(emitter_handler, AL_VELOCITY, x, y, 0);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_emitter_direction(uint32_t emitter_handler, float x, float y)
{
    alSource3f(emitter_handler, AL_DIRECTION, x, y, 0);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_listener_position(float x, float y)
{
    alListener3f(AL_POSITION, x, y, 0);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_listener_velocity(float x, float y)
{
    alListener3f(AL_VELOCITY, x, y, 0);
    MOLANG_AUDIO_LIBRARY_ERROR();
}

void molang_audio_listener_orientation(float x, float y)
{
    ALfloat orientation[] = {x, y, 0};
    alListenerfv(AL_ORIENTATION, orientation);
    MOLANG_AUDIO_LIBRARY_ERROR();
}
