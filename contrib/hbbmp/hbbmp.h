/*
 * BMP image library for Harbour
 *
 * Copyright 2025 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _HBBMP_H_
#define _HBBMP_H_

#include "hbdefs.h"
#include "hbbmp.ch"

HB_EXTERN_BEGIN

#define HB_BMP_DEPTH_DEFAULT        1
#define HB_BMP_DPI_DEFAULT          72
#define HB_BMP_INCHES_PER_METRE     39.3701

#define HB_BMP_FILEHEADER_SIZE      14
#define HB_BMP_INFOHEADER_MINSIZE   12

typedef struct _HB_BMPHEADER
{
   uint8_t  signature   [ 2 ];      /* "BM" */
   uint8_t  file_size   [ 4 ];      /* the size of the BMP file in bytes */
   uint8_t  reserved1   [ 2 ];      /* reserved, depends on the application, if created manually can be 0 */
   uint8_t  reserved2   [ 2 ];      /* reserved, depends on the application, if created manually can be 0 */
   uint8_t  bmpoffset   [ 4 ];      /* the offset of the byte where the bitmap image data (pixel array) can be found */

   uint8_t  headersize  [ 4 ];      /* the size of this header, in bytes (40) */
   uint8_t  width       [ 4 ];      /* the bitmap width in pixels (signed integer) */
   uint8_t  height      [ 4 ];      /* the bitmap height in pixels (signed integer) */
   uint8_t  planes      [ 2 ];      /* the number of color planes (must be 1) */
   uint8_t  bitsperpixel[ 2 ];      /* the number of bits per pixel (color depth): 1, 4, 8, 16, 24 or 32 */
   uint8_t  compression [ 4 ];      /* the compression method, 0 uncompressed */
   uint8_t  bitmapsize  [ 4 ];      /* the image size (the size of the raw bitmap data), 0 can be used for uncompressed bitmaps */
   uint8_t  hresolution [ 4 ];      /* the horizontal resolution of the image (pixel per metre, signed integer) */
   uint8_t  vresolution [ 4 ];      /* the vertical resolution of the image (pixel per metre, signed integer) */
   uint8_t  clrused     [ 4 ];      /* the number of colors in the color palette, or 0 to default to 2^n */
   uint8_t  clrimportant[ 4 ];      /* the number of important colors used, or 0 when every color is important; generally ignored */
} HB_BMPHEADER, * PHB_BMPHEADER;

typedef struct _HB_BMPOS2HEADER
{
   uint8_t  headersize  [ 4 ];      /* the size of this header, in bytes (12) */
   uint8_t  width       [ 2 ];      /* the bitmap width in pixels (signed integer) */
   uint8_t  height      [ 2 ];      /* the bitmap height in pixels (signed integer) */
   uint8_t  planes      [ 2 ];      /* the number of color planes (must be 1) */
   uint8_t  bitsperpixel[ 2 ];      /* the number of bits per pixel (color depth): 1, 4, 8, 16, 24 or 32 */
} HB_BMPOS2HEADER, * PHB_BMPOS2HEADER;

typedef struct _HB_BMPPALETTEITM
{
   uint8_t  blue;
   uint8_t  green;
   uint8_t  red;
   uint8_t  alpha;
} HB_BMPPALETTEITM, * PHB_BMPPALETTEITM;

typedef struct _HB_BMPINFO
{
   int      error;
   int      width;
   int      height;
   int      rowlen;
   HB_BOOL  fromtop;
   int      dpi;
   int      depth;
   int      clrused;
   HB_BMPPALETTEITM palette[ 256 ];
   uint8_t* data;
} HB_BMPINFO, * PHB_BMPINFO;

extern HB_EXPORT PHB_BMPINFO  hb_bmp_new( int width, int height, int depth, int dpi, int * piError );
extern HB_EXPORT PHB_BMPINFO  hb_bmp_frombitmap( const uint8_t * bitmap, int align, int width, int height, int depth, int dpi, const int * palette, int colors, int * piError );
extern HB_EXPORT PHB_BMPINFO  hb_bmp_copy( PHB_BMPINFO pBMP );
extern HB_EXPORT PHB_BMPINFO  hb_bmp_decode( const uint8_t * data, HB_SIZE size, int * piError );
extern HB_EXPORT uint8_t *    hb_bmp_encode( PHB_BMPINFO pBMP, HB_SIZE * pnSsize );
extern HB_EXPORT void         hb_bmp_free( PHB_BMPINFO pBMP );
extern HB_EXPORT uint8_t *    hb_bmp_bitmapptr( PHB_BMPINFO pBMP, HB_SIZE * pnSize );
extern HB_EXPORT void         hb_bmp_seterror( PHB_BMPINFO pBMP, int error );
extern HB_EXPORT int          hb_bmp_error( PHB_BMPINFO pBMP );
extern HB_EXPORT int          hb_bmp_width( PHB_BMPINFO pBMP );
extern HB_EXPORT int          hb_bmp_height( PHB_BMPINFO pBMP );
extern HB_EXPORT int          hb_bmp_depth( PHB_BMPINFO pBMP );
extern HB_EXPORT void         hb_bmp_colorreset( PHB_BMPINFO pBMP );
extern HB_EXPORT HB_MAXINT    hb_bmp_color( PHB_BMPINFO pBMP, int r, int g, int b, int a );
extern HB_EXPORT HB_BOOL      hb_bmp_color2rgb( PHB_BMPINFO pBMP, HB_MAXINT clr, int * r, int * g, int * b, int * a );
extern HB_EXPORT HB_BOOL      hb_bmp_putpixel( PHB_BMPINFO pBMP, int x, int y, HB_MAXINT clr );
extern HB_EXPORT HB_MAXINT    hb_bmp_getpixel( PHB_BMPINFO pBMP, int x, int y );
extern HB_EXPORT void         hb_bmp_line( PHB_BMPINFO pBMP, int x1, int y1, int x2, int y2, HB_MAXINT clr );
extern HB_EXPORT void         hb_bmp_rect( PHB_BMPINFO pBMP, int x, int y, int width, int height, HB_MAXINT clr, HB_BOOL fFill );

extern HB_EXPORT PHB_BMPINFO  hb_bmpParam( int iParam, HB_BOOL fError );
extern HB_EXPORT void         hb_bmpParamFree( int iParam );
extern HB_EXPORT void         hb_bmpReturn( PHB_BMPINFO pBMP );

HB_EXTERN_END

#endif  /* _HBBMP_H_ */
