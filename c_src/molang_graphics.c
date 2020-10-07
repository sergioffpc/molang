#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <png.h>

#include "molang.h"

/*
 * Check to see if a file is a PNG file using png_sig_cmp().  png_sig_cmp()
 * returns zero if the image is a PNG, and nonzero otherwise.
 *
 * The function check_if_png(), returns nonzero (true) if the file can be
 * opened and is a PNG, and 0 (false) otherwise.
 *
 * If this call is successful, and you are going to keep the file open,
 * you should call png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK); once
 * you have created the png_ptr, so that libpng knows your application
 * has read that many bytes from the start of the file.  Make sure you
 * don't call png_set_sig_bytes() with more than 8 bytes read or give it
 * an incorrect number of bytes read, or you will either have read too
 * many bytes, or you are telling libpng to read the wrong number of
 * magic bytes.
 *
 * Many applications already read the first 2 or 4 bytes from the start
 * of the image to determine the file type, so it would be easiest just
 * to pass the bytes to png_sig_cmp(), or even skip that if you know
 * you have a PNG file, and call png_set_sig_bytes().
 */
#define PNG_BYTES_TO_CHECK  4
static int check_if_png(FILE *fp)
{
    png_byte buf[PNG_BYTES_TO_CHECK];

    /* Read in some of the signature bytes.  */
    if (fread(buf, 1, PNG_BYTES_TO_CHECK, fp) != PNG_BYTES_TO_CHECK) {
        return 0;
    }

    /* Compare the first PNG_BYTES_TO_CHECK bytes of the signature.
     * Return nonzero (true) if they match.  */
    return !png_sig_cmp(buf, 0, PNG_BYTES_TO_CHECK);
}

uint32_t molang_graphics_image_create(const char *filename)
{
    /* Open the prospective PNG file.  */
    FILE *fp;
    if ((fp = fopen(filename, "rb")) == NULL) {
        L("unable to open PNG file: %s: %s\r\n", filename, strerror(errno));
        abort();
    }

    if (!check_if_png(fp)) {
        L("invalid PNG signature: %s\r\n", filename);
        abort();
    }

    /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also supply the
    * the compiler header file version, so that we know if the application
    * was compiled with a compatible version of the library.  */
    png_structp png_ptr;
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Allocate/initialize the memory for image information.  */
    png_infop info_ptr;
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Set error handling if you are using the setjmp/longjmp method (this is
     * the normal method of doing things with libpng).  REQUIRED unless you
     * set up your own error handlers in the png_create_read_struct() earlier.  */
    if (setjmp(png_jmpbuf(png_ptr))) {
        L("unable to initialize PNG decoder\r\n");
        abort();
    }

    /* Set up the input control if you are using standard C streams.  */
    png_init_io(png_ptr, fp);

    /* If we have already read some of the signature.  */
    png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK);

    /* If you have enough memory to read in the entire image at once,
     * and you need to specify only transforms that can be controlled
     * with one of the PNG_TRANSFORM_* bits (this presently excludes
     * quantizing, filling, setting background, and doing gamma
     * adjustment), then you can read the entire image (including
     * pixels) into the info structure with this call.  */
    const int png_transforms = PNG_TRANSFORM_STRIP_16 | PNG_TRANSFORM_PACKING | PNG_TRANSFORM_EXPAND;
    png_read_png(png_ptr, info_ptr, png_transforms, NULL);

    png_uint_32 width;
    png_uint_32 height;
    int bit_depth;
    int color_type;
    int interlace_method;
    png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, &interlace_method, NULL, NULL);

    L("load filename=%s width=%d height=%d bit_depth=%d color_type=%d interlace_method=%d\r\n"
           ,filename
           ,width
           ,height
           ,bit_depth
           ,color_type
           ,interlace_method);

    png_bytepp row_pointers = png_get_rows(png_ptr, info_ptr);

    size_t row_bytes = png_get_rowbytes(png_ptr, info_ptr);
    uint8_t *data_buffer = malloc(row_bytes * height);
    if (data_buffer == NULL) {
        L("unable to allocate memory: %s\r\n", strerror(errno));
        abort();
    }

    /* Align bytes as OpenGL expects them.  */
    for (png_uint_32 i = 0; i < height; i++) {
        memcpy(data_buffer + (row_bytes * i), row_pointers[i], row_bytes);
    }

    /* Clean up after the read, and free any memory allocated.  */
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    fclose(fp);

    GLint format;
    switch (color_type) {
        case PNG_COLOR_TYPE_RGBA:
            format = GL_RGBA;
            break;
        case PNG_COLOR_TYPE_RGB:
            format = GL_RGB;
            break;
        case PNG_COLOR_TYPE_GRAY:
            format = GL_LUMINANCE;
            break;
        case PNG_COLOR_TYPE_GRAY_ALPHA:
            format = GL_LUMINANCE_ALPHA;
            break;
        default:
            L("unsupported PNG color type: %d\r\n", color_type);
            abort();
    }

    GLuint texture;
    glGenTextures(1, &texture);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glBindTexture(GL_TEXTURE_2D, texture);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    glTexImage2D(GL_TEXTURE_2D
                ,0
                ,format
                ,width
                ,height
                ,0
                ,format
                ,GL_UNSIGNED_BYTE
                ,data_buffer);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    free(data_buffer);

    glGenerateMipmap(GL_TEXTURE_2D);
    MOLANG_GRAPHICS_LIBRARY_ERROR();

    glBindTexture(GL_TEXTURE_2D, 0);

    return texture;
}

void molang_graphics_image_destroy(uint32_t image_handler)
{
    glDeleteTextures(1, &image_handler);
    MOLANG_GRAPHICS_LIBRARY_ERROR();
}
