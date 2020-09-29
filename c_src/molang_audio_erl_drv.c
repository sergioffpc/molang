#include <ei.h>
#include <erl_driver.h>
#include <errno.h>
#include <string.h>

#include <AL/alc.h>

#include "molang.h"

typedef struct {
    ErlDrvPort  port;
} audio_erl_drv_data_t;

static ErlDrvData audio_erl_drv_start(ErlDrvPort port, char *buffer __attribute__((unused)))
{
    /* PORT_CONTROL_FLAG_BINARY means data is returned as a binary.  */
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    audio_erl_drv_data_t *data = (audio_erl_drv_data_t *) driver_alloc(sizeof(audio_erl_drv_data_t));
    if (data == NULL) {
        L("unable to allocate driver memory\r\n");
        abort();
    }
    data->port = port;

    ALCdevice *alc_device = NULL;
    const ALCchar *alc_device_name = NULL;
    alc_device = alcOpenDevice(alc_device_name);
    if (alc_device == NULL) {
        L("unable to open device: %s\r\n", alc_device_name);
        abort();
    }

    ALCcontext *alc_context = NULL;
    const ALCint *alc_context_attrs = NULL;
    alc_context = alcCreateContext(alc_device, alc_context_attrs);
    alcMakeContextCurrent(alc_context);

    ALCint alc_major_version = 0;
    alcGetIntegerv(alc_device, ALC_MAJOR_VERSION, sizeof(ALCint), &alc_major_version);

    ALCint alc_minor_version = 0;
    alcGetIntegerv(alc_device, ALC_MINOR_VERSION, sizeof(ALCint), &alc_minor_version);

    L("OpenAL version: %d.%d\r\n", alc_major_version, alc_minor_version);
    L("OpenAL default device specifier: %s\r\n", alcGetString(alc_device, ALC_DEFAULT_DEVICE_SPECIFIER));
    L("OpenAL device specifier: %s\r\n", alcGetString(alc_device, ALC_DEVICE_SPECIFIER));
    L("OpenAL extensions: %s\r\n", alcGetString(alc_device, ALC_EXTENSIONS));

    return (ErlDrvData) data;
}

static void audio_erl_drv_stop(ErlDrvData data)
{
    ALCcontext *alc_context = alcGetCurrentContext();
    ALCdevice *alc_device = alcGetContextsDevice(alc_context);

    alcMakeContextCurrent(NULL);

    alcDestroyContext(alc_context);
    alcCloseDevice(alc_device);

    driver_free((char *) data);
}

static void audio_erl_drv_output(ErlDrvData data, char *buffer, ErlDrvSizeT length __attribute__((unused)))
{
    int index = 0;

    EI_ULONGLONG fn = 0;
    ei_decode_version(buffer, &index, NULL);
    ei_decode_ulonglong(buffer, &index, &fn);
    switch (fn) {
        case MOLANG_AUDIO_ERL_DRV_BUFFER_CREATE_FN:
        {
            char filename[255];
            ei_decode_version(buffer, &index, NULL);
            ei_decode_string(buffer, &index, filename);

            uint32_t buffer_handler = molang_audio_buffer_create(filename);

            ei_x_buff x;
            ei_x_new_with_version(&x);
            ei_x_encode_ulonglong(&x, buffer_handler);

            driver_output(((audio_erl_drv_data_t *) data)->port, x.buff, x.index);

            ei_x_free(&x);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_BUFFER_DESTROY_FN:
        {
            EI_ULONGLONG buffer_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &buffer_handler);

            molang_audio_buffer_destroy(buffer_handler);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_CREATE_FN:
        {
            EI_ULONGLONG buffer_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &buffer_handler);

            uint32_t emitter_handler = molang_audio_emitter_create(buffer_handler);

            ei_x_buff x;
            ei_x_new_with_version(&x);
            ei_x_encode_ulonglong(&x, emitter_handler);

            driver_output(((audio_erl_drv_data_t *) data)->port, x.buff, x.index);

            ei_x_free(&x);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_DESTROY_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            molang_audio_emitter_destroy(emitter_handler);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_PLAY_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            molang_audio_emitter_play(emitter_handler);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_STOP_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            molang_audio_emitter_stop(emitter_handler);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_POSITION_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            double x = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &x);

            double y = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &y);

            molang_audio_emitter_position(emitter_handler, x, y);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_VELOCITY_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            double x = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &x);

            double y = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &y);

            molang_audio_emitter_velocity(emitter_handler, x, y);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_EMITTER_DIRECTION_FN:
        {
            EI_ULONGLONG emitter_handler = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_ulonglong(buffer, &index, &emitter_handler);

            double x = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &x);

            double y = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &y);

            molang_audio_emitter_direction(emitter_handler, x, y);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_LISTENER_POSITION_FN:
        {
            double x = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &x);

            double y = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &y);

            molang_audio_listener_position(x, y);
        }
            break;
        case MOLANG_AUDIO_ERL_DRV_LISTENER_VELOCITY_FN:
        {
            double x = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &x);

            double y = 0;
            ei_decode_version(buffer, &index, NULL);
            ei_decode_double(buffer, &index, &y);

            molang_audio_listener_velocity(x, y);
        }
            break;
        default:
            L("specified parameter is not valid\r\n");
            abort();
    }
}

static ErlDrvEntry audio_erl_drv_entry = {
    /* Called at system start up for statically linked drivers, and after loading for dynamically loaded drivers.  */
    NULL,
    /* Called when open_port/2 is invoked, return value -1 means failure.  */
    audio_erl_drv_start,
    /* Called when port is closed, and when the emulator is halted.  */
    audio_erl_drv_stop,
    /* Called when we have output from erlang to the port.  */
    audio_erl_drv_output,
    /* Called when we have input from one of the driver's handles.  */
    NULL,
    /* Called when output is possible to one of the driver's handles.  */
    NULL,
    /* Name supplied as command in open_port.  */
    "molang_audio",
    /* Called before unloading the driver.  */
    NULL,
    /* Reserved - used by emulator internally.  */
    NULL,
    /* "ioctl" for drivers - invoked by port_control/3.  */
    NULL,
    /* Handling of timeout in driver.  */
    NULL,
    /* Called when we have output from erlang to the port.  */
    NULL,
    NULL,
    /* Called when the port is about to be closed, and there is data in the driver queue that needs to be flushed
     * before 'stop' can be called.  */
    NULL,
    /* Works mostly like 'control', a synchronous call into the driver.  */
    NULL,
    NULL,
    /* Extended marker, should always be set to indicate driver versioning.  */
    ERL_DRV_EXTENDED_MARKER,
    /* Major version, should always be set to this value.  */
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    /* Minor version, should always be set to this value.  */
    ERL_DRV_EXTENDED_MINOR_VERSION,
    /* Driver flags, see documentation.  */
    0,
    /* Reserved - used by emulator internally.  */
    NULL,
    /* Called when a process monitor fires.  */
    NULL,
    /* Called on behalf of driver_select when it is safe to release 'event'.  A typical unix driver would
     * call close(event).  */
    NULL,
    /* Called when the port is closed abruptly.  Specifically when erl_crash_dump is called.  */
    NULL
};

DRIVER_INIT(molang_audio)
{
    return &audio_erl_drv_entry;
}
