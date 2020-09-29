#include <ei.h>
#include <erl_driver.h>
#include <pthread.h>
#include <stdbool.h>
#include <string.h>

#include <EGL/egl.h>
#include <X11/Xlib.h>

#include "molang.h"

typedef struct {
    ErlDrvPort   port;

    bool         is_running;

    Display     *x11_display;
    Window       x11_window;
} graphics_erl_drv_data_t;

static void *graphics_erl_drv_loop(void *arg)
{
    graphics_erl_drv_data_t *data = (graphics_erl_drv_data_t *) arg;

    EGLDisplay egl_display = eglGetCurrentDisplay();
    EGLSurface egl_surface = eglGetCurrentSurface(EGL_DRAW);

    uint64_t frame_count = 0;
    while (data->is_running) {
        struct timespec t0;
        clock_gettime(CLOCK_MONOTONIC, &t0);

        while (XPending(data->x11_display)) {
            XEvent event;
            XNextEvent(data->x11_display, &event);

            switch (event.type) {
                case KeyPress:
                break;
                case KeyRelease:
                break;
                case MotionNotify:
                break;
                case ButtonPress:
                break;
                case ButtonRelease:
                break;
            }
        }

        // draw

        eglSwapBuffers(egl_display, egl_surface);

        struct timespec t1;
        clock_gettime(CLOCK_MONOTONIC, &t1);
        uint64_t frame_delta = ((t1.tv_sec - t0.tv_sec) * 1000000000 + (t1.tv_nsec - t0.tv_nsec)) / 1000000;

        frame_count++;

        L("frame=%ld time=%ldms\r\n", frame_count, frame_delta);
    }

    return 0;
}

static ErlDrvData graphics_erl_drv_start(ErlDrvPort port, char *buffer __attribute__((unused)))
{
    /* PORT_CONTROL_FLAG_BINARY means data is returned as a binary.  */
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    graphics_erl_drv_data_t *data = (graphics_erl_drv_data_t *) driver_alloc(sizeof(graphics_erl_drv_data_t));
    if (data == NULL) {
        L("unable to allocate driver memory\r\n");
        abort();
    }
    data->port = port;

    const char *egl_extensions = eglQueryString(EGL_NO_DISPLAY, EGL_EXTENSIONS);
    if (!egl_extensions) {
        L("EGL_EXT_client_extensions is unsupported\r\n");
        abort();
    }

    if (!strstr(egl_extensions, "EGL_EXT_platform_x11")) {
        L("EGL_EXT_platform_x11 extension is unsupported\r\n");
        abort();
    }

    const char *x11_display_name = NULL;
    data->x11_display = XOpenDisplay(x11_display_name);
    if (data->x11_display == NULL) {
        L("unable to open display: %s\r\n", x11_display_name);
        abort();
    }

    EGLDisplay egl_display;
    egl_display = eglGetDisplay(data->x11_display);
    if (egl_display == EGL_NO_DISPLAY) {
        L("unable to get EGL display\r\n");
        abort();
    }

    if (!eglInitialize(egl_display, NULL, NULL)) {
        L("unable to initialize EGL\r\n");
        abort();
    }

#ifndef NDEBUG
    const char *egl_client_apis = eglQueryString(egl_display, EGL_CLIENT_APIS);
    const char *egl_vendor = eglQueryString(egl_display, EGL_VENDOR);
    const char *egl_version = eglQueryString(egl_display, EGL_VERSION);

    L("EGL client API names: %s\r\n", egl_client_apis);
    L("EGL vendor: %s\r\n", egl_vendor);
    L("EGL version: %s\r\n", egl_version);
    L("EGL extensions: %s\r\n", egl_extensions);
#endif

    EGLint egl_config_attrs[] = {
        EGL_BUFFER_SIZE,        32,
        EGL_RED_SIZE,            8,
        EGL_GREEN_SIZE,          8,
        EGL_BLUE_SIZE,           8,
        EGL_ALPHA_SIZE,          8,

        EGL_RENDERABLE_TYPE,    EGL_OPENGL_ES2_BIT,
        EGL_SURFACE_TYPE,       EGL_WINDOW_BIT,

        EGL_NONE,
    };

    EGLConfig egl_config;
    EGLint egl_config_count;
    if (!eglChooseConfig(egl_display, egl_config_attrs, &egl_config, 1, &egl_config_count)) {
        L("unable to specify required EGL attribute properties\r\n");
        abort();
    }
    if (egl_config_count == 0) {
        L("unable to match EGL configuration\r\n");
        abort();
    }

    XVisualInfo x11_visual_info_template;
    if (!eglGetConfigAttrib(egl_display, egl_config, EGL_NATIVE_VISUAL_ID, (EGLint*) &x11_visual_info_template.visualid)) {
        L("unable to query configuration attributes\r\n");
        abort();
    }

    XVisualInfo *x11_visual_info;
    int x11_visual_info_count;
    x11_visual_info = XGetVisualInfo(data->x11_display, VisualIDMask, &x11_visual_info_template, &x11_visual_info_count);
    if (!x11_visual_info) {
        L("no visual structures match the template\r\n");
        abort();
    }

    Colormap x11_colormap;
    x11_colormap = XCreateColormap(data->x11_display, RootWindow(data->x11_display, 0), x11_visual_info->visual, AllocNone);
    if (x11_colormap == None) {
        L("unable to create a colormap of the specified visual type\r\n");
        abort();
    }

    const Window x11_root_window = DefaultRootWindow(data->x11_display);
    const int width = 1920;
    const int height = 1080;

    XSetWindowAttributes x11_window_attrs = {
        .event_mask     = KeyPressMask | KeyReleaseMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
        .colormap       = x11_colormap,
        .border_pixel   = 0,
    };
    int x11_window_attrs_mask = 0;
    x11_window_attrs_mask = CWEventMask | CWColormap | CWBorderPixel;

    data->x11_window = XCreateWindow(data->x11_display, x11_root_window, 0, 0, width, height,  0, x11_visual_info->depth, InputOutput, x11_visual_info->visual, x11_window_attrs_mask, &x11_window_attrs);
    if (!data->x11_window) {
        L("unable to create X11 window\r\n");
        abort();
    }

    XSizeHints x11_window_hints = {
        .flags	    = PMinSize | PMaxSize,
        .min_width  = width,
        .max_width  = width,
        .min_height = height,
        .max_height = height,
    };
    XSetWMNormalHints(data->x11_display, data->x11_window, &x11_window_hints);

    XMapWindow(data->x11_display, data->x11_window);

    EGLSurface egl_surface;
    egl_surface = eglCreateWindowSurface(egl_display, egl_config, data->x11_window, NULL);
    if (egl_surface == EGL_NO_SURFACE) {
        L("unable to create EGL window surface\r\n");
        abort();
    }

    EGLint egl_context_attrs[] = {
        EGL_CONTEXT_CLIENT_VERSION, 2,
        EGL_NONE
    };

    EGLContext egl_context;
    egl_context = eglCreateContext(egl_display, egl_config, EGL_NO_CONTEXT, egl_context_attrs);
    if (egl_context == EGL_NO_CONTEXT) {
        L("unable to create EGL rendering context\r\n");
        abort();
    }

    eglMakeCurrent(egl_display, egl_surface, egl_surface, egl_context);
    eglSwapInterval(egl_display, 1);

    data->is_running = true;

    pthread_t thread;
    switch (pthread_create(&thread, NULL, graphics_erl_drv_loop, data)) {
        case EAGAIN:
            L("insufficient resources");
            abort();
        case EINVAL:
            L("invalid settings");
            abort();
        case EPERM:
            L("no permission");
            abort();
    }

    return (ErlDrvData) data;
}

static void graphics_erl_drv_stop(ErlDrvData data)
{
    graphics_erl_drv_data_t *data_ptr = (graphics_erl_drv_data_t *) data;

    data_ptr->is_running = false;

    EGLDisplay egl_display = eglGetCurrentDisplay();
    EGLSurface egl_surface = eglGetCurrentSurface(EGL_DRAW);
    EGLContext egl_context = eglGetCurrentContext();

    eglMakeCurrent(egl_display, egl_surface, egl_surface, EGL_NO_CONTEXT);

    eglDestroySurface(egl_display, egl_surface);
    eglDestroyContext(egl_display, egl_context);
    eglTerminate(egl_display);

    XDestroyWindow(data_ptr->x11_display, data_ptr->x11_window);
    XCloseDisplay(data_ptr->x11_display);

    driver_free((char *) data);
}

static void graphics_erl_drv_output(ErlDrvData data, char *buffer, ErlDrvSizeT length __attribute__((unused)))
{
    int index = 0;

    EI_ULONGLONG fn = 0;
    ei_decode_version(buffer, &index, NULL);
    ei_decode_ulonglong(buffer, &index, &fn);
    switch (fn) {
        case GRAPHICS_ERL_DRV_TEXTURE_BUFFER_CREATE_FN:
        {
            char filename[255];
            ei_decode_version(buffer, &index, NULL);
            ei_decode_string(buffer, &index, filename);

            uint32_t buffer_handler = 0;

            ei_x_buff x;
            ei_x_new_with_version(&x);
            ei_x_encode_ulonglong(&x, buffer_handler);

            graphics_erl_drv_data_t *data_ptr = (graphics_erl_drv_data_t *) data;
            driver_output(data_ptr->port, x.buff, x.index);

            ei_x_free(&x);
        }
            break;
        default:
            L("specified parameter is not valid\r\n");
            abort();
    }
}

static ErlDrvEntry graphics_erl_drv_entry = {
    /* Called at system start up for statically linked drivers, and after loading for dynamically loaded drivers.  */
    NULL,
    /* Called when open_port/2 is invoked, return value -1 means failure.  */
    graphics_erl_drv_start,
    /* Called when port is closed, and when the emulator is halted.  */
    graphics_erl_drv_stop,
    /* Called when we have output from erlang to the port.  */
    graphics_erl_drv_output,
    /* Called when we have input from one of the driver's handles.  */
    NULL,
    /* Called when output is possible to one of the driver's handles.  */
    NULL,
    /* Name supplied as command in open_port.  */
    "molang_graphics",
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

DRIVER_INIT(molang_graphics)
{
    return &graphics_erl_drv_entry;
}
