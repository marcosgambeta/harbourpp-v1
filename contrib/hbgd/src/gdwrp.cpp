/*
 * GD graphic library low-level (client API) interface code.
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 */

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>

/* NOTE: Do some initialization required by the GD headers. */
#if defined(HB_OS_WIN)
#if !defined(WIN32)
#define WIN32
#endif
#if !defined(BGDWIN32)
#define BGDWIN32
#endif
#endif

#include "gd.h"
#include "gdfontt.h"
#include "gdfonts.h"
#include "gdfontmb.h"
#include "gdfontl.h"
#include "gdfontg.h"

#define HB_GD_VERS(ma, mi, mu)                                                                                         \
  (GD_MAJOR_VERSION > ma ||                                                                                            \
   (GD_MAJOR_VERSION == ma && (GD_MINOR_VERSION > mi || (GD_MINOR_VERSION == mi && GD_RELEASE_VERSION >= mu))))

#define IMAGE_JPEG 1
#define IMAGE_GIF 2
#define IMAGE_PNG 3
#define IMAGE_WBMP 4
#define IMAGE_GD 5

/* Internal function for handling pointers
 *
 * Code of Przemyslaw Czerpak for gdImagePtr
 * adapted also to gdFontPtr
 */

/* gdImage */

/* gdImage destructor, it's executed automatically */
static HB_GARBAGE_FUNC(hb_gdImage_Destructor)
{
  /* Retrieve image pointer holder */
  auto ptr = static_cast<gdImagePtr *>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (*ptr) {
    /* Destroy the image */
    gdImageDestroy(*ptr);

    /* Set pointer to nullptr to avoid multiple freeing */
    *ptr = nullptr;
  }
}

static const HB_GC_FUNCS s_gcGDimageFuncs = {hb_gdImage_Destructor, hb_gcDummyMark};

/* Function returns gdImage pointer or nullptr when wrong variable is
   passed or gdImage was freed before */
static gdImagePtr hb_parGdImage(int32_t iParam)
{
  auto ptr = static_cast<gdImagePtr *>(hb_parptrGC(&s_gcGDimageFuncs, iParam));

  return ptr ? *ptr : nullptr;
}

static void *hb_isGdImage(int32_t iParam)
{
  return hb_parptrGC(&s_gcGDimageFuncs, iParam);
}

/* Function create in HVM stack return value item with gdImage pointer */
static void hb_retGdImage(gdImagePtr im)
{
  auto ptr = static_cast<gdImagePtr *>(hb_gcAllocate(sizeof(gdImagePtr), &s_gcGDimageFuncs));

  *ptr = im;

  hb_retptrGC(static_cast<void *>(ptr));
}

#if 0
/* Function returns PHB_ITEM with gdImage pointer */
static HB_ITEM *hb_gdImageItemNew( gdImagePtr im )
{
   auto ptr = static_cast<gdImagePtr*>(hb_gcAllocate( sizeof(gdImagePtr), &s_gcGDimageFuncs));

   *ptr = im;

   return hb_itemPutPtrGC(nullptr, static_cast<void*>(ptr));
}
#endif

/* gdFont */

/* gdFont destructor, it's executed automatically */
static HB_GARBAGE_FUNC(hb_gdFont_Destructor)
{
  /* Retrieve Font pointer holder */
  auto ptr = static_cast<gdFontPtr *>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (*ptr) {
    /* Destroy the Font */
#if 0
      /* Do nothing, GD handles it directly and gdFontDestroy() not exists */
      gdFontDestroy(*ptr);
#endif

    /* Set pointer to nullptr to avoid multiple freeing */
    *ptr = nullptr;
  }
}

static const HB_GC_FUNCS s_gcGDfontFuncs = {hb_gdFont_Destructor, hb_gcDummyMark};

/* Function returns gdFont pointer or nullptr when wrong variable is
   passed or gdFont was freed before */
static gdFontPtr hb_parGdFont(int32_t iParam)
{
  auto ptr = static_cast<gdFontPtr *>(hb_parptrGC(&s_gcGDfontFuncs, iParam));

  return ptr ? *ptr : nullptr;
}

static void *hb_isGdFont(int32_t iParam)
{
  return hb_parptrGC(&s_gcGDfontFuncs, iParam);
}

/* Function create in HVM stack return value item with gdFont pointer */
static void hb_retGdFont(gdFontPtr font)
{
  auto ptr = static_cast<gdFontPtr *>(hb_gcAllocate(sizeof(gdFontPtr), &s_gcGDfontFuncs));

  *ptr = font;

  hb_retptrGC(static_cast<void *>(ptr));
}

#if 0
/* Function returns PHB_ITEM with gdFont pointer */
static HB_ITEM *hb_gdFontItemNew( gdFontPtr font )
{
   auto ptr = static_cast<gdFontPtr*>(hb_gcAllocate(sizeof(gdFontPtr), &s_gcGDfontFuncs));

   *ptr = font;

   return hb_itemPutPtrGC(nullptr, static_cast<void*>(ptr));
}
#endif

static void *LoadImageFromHandle(HB_FHANDLE fhandle, int32_t sz)
{
  auto iptr = hb_xgrab(sz);

  hb_fsReadLarge(fhandle, iptr, static_cast<HB_SIZE>(sz));

  return iptr;
}

static void *LoadImageFromFile(const char *szFile, int32_t *sz)
{
  void *iptr;
  HB_FHANDLE fhandle;

  if ((fhandle = hb_fsOpen(szFile, FO_READ)) != FS_ERROR) {
    /* get lenght */
    *sz = static_cast<int32_t>(hb_fsSeek(fhandle, 0, FS_END));
    /* rewind */
    hb_fsSeek(fhandle, 0, FS_SET);

    /* Read file */
    iptr = hb_xgrab(*sz);
    hb_fsReadLarge(fhandle, iptr, static_cast<HB_SIZE>(*sz));

    hb_fsClose(fhandle);
  } else {
    /* File error */
    iptr = nullptr;
    *sz = 0;
  }

  return iptr;
}

static void SaveImageToHandle(HB_FHANDLE fhandle, const void *iptr, int32_t sz)
{
  hb_fsWriteLarge(fhandle, iptr, static_cast<HB_SIZE>(sz));
}

static void SaveImageToFile(const char *szFile, const void *iptr, int32_t sz)
{
  HB_FHANDLE fhandle;

  if ((fhandle = hb_fsCreate(szFile, FC_NORMAL)) != FS_ERROR) {
    /* Write Image */
    SaveImageToHandle(fhandle, iptr, sz);

    hb_fsClose(fhandle);
  }
}

static void GDImageCreateFrom(int32_t nType)
{
  gdImagePtr im = nullptr;
  int32_t sz;
  void *iptr;

  if (HB_ISCHAR(1)) {
    /* Retrieve image from file name */
    iptr = LoadImageFromFile(hb_parc(1), &sz);
  } else if (hb_isGdImage(1) && HB_ISNUM(2)) {
    /* Retrieve image size */
    sz = hb_parni32(2);

    /* Retrieve image pointer + size */
    iptr = hb_parGdImage(1);
  } else if (HB_ISNUM(1) && HB_ISNUM(2)) {
    /* Retrieve image size */
    sz = hb_parni32(2);

    /* retrieve image from handle */
    iptr = LoadImageFromHandle(hb_numToHandle(hb_parnintdef(1, 0 /* std input */)), sz);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }

  if (iptr && sz) {
    /* Create Image */
    switch (nType) {
    case IMAGE_JPEG:
      im = gdImageCreateFromJpegPtr(sz, iptr);
      break;
    case IMAGE_GIF:
      im = gdImageCreateFromGifPtr(sz, iptr);
      break;
    case IMAGE_PNG:
      im = gdImageCreateFromPngPtr(sz, iptr);
      break;
    case IMAGE_WBMP:
      im = gdImageCreateFromWBMPPtr(sz, iptr);
      break;
    case IMAGE_GD:
      im = gdImageCreateFromGdPtr(sz, iptr);
      break;
    }

    /* Return image pointer */
    hb_retGdImage(im);

    /* Free memory */
    hb_xfree(iptr);
  }
}

static void GDImageSaveTo(int32_t nType)
{
  if (hb_isGdImage(1)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t sz = 0;
    void *iptr = nullptr;
    int32_t level = 0, fg = 0;

    /* Get file name or an output handler or NIL it I want a return string */
    if (!(HB_ISNIL(2) || HB_ISCHAR(2) || HB_ISNUM(2))) {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "Second argument must be NIL or numeric or a string.", HB_ERR_FUNCNAME, 2,
                           hb_paramError(2));
      return;
    }

    /* Retrieve compression level */
    /* check if is numeric */
    if (!(HB_ISNIL(3) || HB_ISNUM(3))) {
      hb_errRT_BASE_SubstR(EG_ARG, 0, "Third argument must be NIL or numeric.", HB_ERR_FUNCNAME, 1, hb_paramError(3));
      return;
    }

    if (nType == IMAGE_JPEG) {
      /* check range */
      level = hb_parnidef(3, -1);
      if (!(level >= -1 && level <= 95)) {
        hb_errRT_BASE_SubstR(EG_ARG, 0, "Compression level must be -1 (default) or a value between 0 and 95.",
                             HB_ERR_FUNCNAME, 1, hb_paramError(3));
        return;
      }
    } else if (nType == IMAGE_PNG) {
      /* check range */
      level = hb_parnidef(3, -1);
      if (!(level >= -1 && level <= 9)) {
        hb_errRT_BASE_SubstR(EG_ARG, 0, "Compression level must be -1 (default) or a value between 0 and 9.",
                             HB_ERR_FUNCNAME, 1, hb_paramError(3));
        return;
      }
    } else if (nType == IMAGE_WBMP) {
      if (!HB_ISNUM(3)) {
        hb_errRT_BASE_SubstR(EG_ARG, 0, "Foreground color nedeed", HB_ERR_FUNCNAME, 1, hb_paramError(3));
        return;
      }
      fg = hb_parni32(3);
    }

    switch (nType) {
    case IMAGE_JPEG:
      /* Get image Ptr */
      iptr = gdImageJpegPtr(im, &sz, level);
      break;
    case IMAGE_GIF:
      /* Get image Ptr */
      iptr = gdImageGifPtr(im, &sz);
      break;
    case IMAGE_PNG:
      /* Get image Ptr */
      iptr = gdImagePngPtrEx(im, &sz, level);
      break;
    case IMAGE_WBMP:
      /* Get image Ptr */
      iptr = gdImageWBMPPtr(im, &sz, fg);
      break;
    case IMAGE_GD:
      /* Get image Ptr */
      iptr = gdImageGdPtr(im, &sz);
      break;
    }

    /* If i get a file name */
    if (HB_ISCHAR(2)) {
      SaveImageToFile(hb_parc(2), iptr, sz);
    }

    /* Write to file handle */
    else if (HB_ISNUM(2)) {
      /* Write to std output or to a passed file */
      HB_FHANDLE fhandle = hb_numToHandle(hb_parnint(2));

      if (fhandle == FS_ERROR || fhandle == 0) {
        fhandle = 1; /* std output */
      }

      /* Write Image */
      SaveImageToHandle(fhandle, iptr, sz);
    }
    /* Return image as string) */
    else {
      /* Return as string */
      hb_retclen(static_cast<const char *>(iptr), static_cast<HB_SIZE>(sz));
    }

    /* Free memory */
    gdFree(iptr);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* ************************* WRAPPED FUNCTIONS ****************************** */

HB_FUNC(HB_GD_VERSION)
{
  hb_storni32(GD_MAJOR_VERSION, 1);
  hb_storni32(GD_MINOR_VERSION, 2);
  hb_storni32(GD_RELEASE_VERSION, 3);
}

HB_FUNC(GDVERSION)
{
#if HB_GD_VERS(2, 0, 34)
  char szVer[30];
  hb_snprintf(szVer, sizeof(szVer), "GD Version %s", GD_VERSION_STRING);
  hb_retc(szVer);
#elif HB_GD_VERS(2, 0, 33)
  hb_retc_const("GD Version 2.0.33");
#else
  hb_retc_const("GD Version 2.0.28");
#endif
}

HB_FUNC(GDVERSIONNUMBER)
{
#if HB_GD_VERS(2, 0, 34)
  hb_retni(GD_MAJOR_VERSION * 1000 + GD_MINOR_VERSION * 100 + GD_RELEASE_VERSION);
#elif HB_GD_VERS(2, 0, 33)
  hb_retni(2033);
#else
  hb_retni(2028);
#endif
}

/* IMAGE CREATION, DESTRUCTION, LOADING AND SAVING */

/* gdImagePtr gdImageCreate(sx, sy) */
HB_FUNC(GDIMAGECREATE)
{
  if (HB_ISNUM(1) && HB_ISNUM(2)) {
    auto sx = hb_parni32(1);
    auto sy = hb_parni32(2);

    hb_retGdImage(gdImageCreate(sx, sy));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Alias of GDCreate() */
HB_FUNC_TRANSLATE(GDIMAGECREATEPALETTE, GDIMAGECREATE) /* gdImagePtr gdImageCreatePalette(sx, sy) */

/* gdImageCreateTrueColor(sx, sy) */
HB_FUNC(GDIMAGECREATETRUECOLOR)
{
  if (HB_ISNUM(1) && HB_ISNUM(2)) {
    auto sx = hb_parni32(1);
    auto sy = hb_parni32(2);

    hb_retGdImage(gdImageCreateTrueColor(sx, sy));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* gdImageCreateFromJpegPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromJpeg( char *szFile ) */
HB_FUNC(GDIMAGECREATEFROMJPEG)
{
  GDImageCreateFrom(IMAGE_JPEG);
}

/* gdImageCreateFromGifPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromGif( char *szFile ) */
HB_FUNC(GDIMAGECREATEFROMGIF)
{
  GDImageCreateFrom(IMAGE_GIF);
}

/* gdImageCreateFromPngPtr(int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromPng( char *szFile ) */
HB_FUNC(GDIMAGECREATEFROMPNG)
{
  GDImageCreateFrom(IMAGE_PNG);
}

/* gdImagePtr gdImageCreateFromWBMPPtr (int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromWBMP ( char *szFile ) */
HB_FUNC(GDIMAGECREATEFROMWBMP)
{
  GDImageCreateFrom(IMAGE_WBMP);
}

/* gdImagePtr gdImageCreateFromGdPtr (int size, void *data) */
/* implementation: gdImagePtr gdImageCreateFromGd ( char *szFile ) */
HB_FUNC(GDIMAGECREATEFROMGD)
{
  GDImageCreateFrom(IMAGE_GD);
}

/* original: void gdImageJpeg(gdImagePtr im, FILE *out) */
/* implementation: void gdImageJpeg(gdImagePtr im, char *szFile) */
HB_FUNC(GDIMAGEJPEG)
{
  GDImageSaveTo(IMAGE_JPEG);
}

/* original: void gdImageGif(gdImagePtr im, FILE *out) */
/* implementation: void gdImageGif(gdImagePtr im, char *szFile) */
HB_FUNC(GDIMAGEGIF)
{
  GDImageSaveTo(IMAGE_GIF);
}

/* original: void gdImagePngEx(gdImagePtr im, FILE *out, int level) */
/* implementation: void gdImagePng(gdImagePtr im, char *szFile [, int level] ) */
HB_FUNC(GDIMAGEPNG)
{
  GDImageSaveTo(IMAGE_PNG);
}

/* original: void gdImageWBmp(gdImagePtr im, FILE *out) */
/* implementation: void gdImageWBmp(gdImagePtr im, char *szFile, int fg) */
HB_FUNC(GDIMAGEWBMP)
{
  GDImageSaveTo(IMAGE_WBMP);
}

/* original: void gdImageGD(gdImagePtr im, FILE *out) */
/* implementation: void gdImageGD(gdImagePtr im, char *szFile) */
HB_FUNC(GDIMAGEGD)
{
  GDImageSaveTo(IMAGE_GD);
}

#if defined(HB_LEGACY_LEVEL4)

/* After Przemek changes on hb_*ptr() functions, this is a void function held only
   for compatibility with GD library. */
/* gdImageDestroy(gdImagePtr im) */
HB_FUNC(GDIMAGEDESTROY)
{
}

#endif

/* DRAWING FUNCTIONS */

/* void gdImageSetPixel(gdImagePtr im, int x, int y, int color) */
HB_FUNC(GDIMAGESETPIXEL)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);
    auto color = hb_parni32(4);

    gdImageSetPixel(im, x, y, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
HB_FUNC(GDIMAGELINE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x1 = hb_parni32(2);
    auto y1 = hb_parni32(3);
    auto x2 = hb_parni32(4);
    auto y2 = hb_parni32(5);
    auto color = hb_parni32(6);

    gdImageLine(im, x1, y1, x2, y2, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageDashedLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
HB_FUNC(GDIMAGEDASHEDLINE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x1 = hb_parni32(2);
    auto y1 = hb_parni32(3);
    auto x2 = hb_parni32(4);
    auto y2 = hb_parni32(5);
    auto color = hb_parni32(6);

    gdImageDashedLine(im, x1, y1, x2, y2, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* original: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC(GDIMAGEPOLYGON)
{
  if (hb_isGdImage(1) && HB_ISARRAY(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pPoints = hb_param(2, Harbour::Item::ARRAY);
    auto pointsTotal = static_cast<int32_t>(hb_arrayLen(pPoints));
    auto color = hb_parni32(3);

    /* Max Points of polygon */
    auto points = static_cast<gdPoint *>(hb_xgrab(sizeof(gdPoint) * pointsTotal));

    for (auto i = 0; i < pointsTotal; i++) {
      auto pPoint = hb_arrayGetItemPtr(pPoints, i + 1);
      if (pPoint->isArray()) {
        points[i].x = hb_arrayGetNI32(pPoint, 1);
        points[i].y = hb_arrayGetNI32(pPoint, 2);
      } else {
        points[i].x = 0;
        points[i].y = 0;
      }
    }

    /* Draw a polygon */
    gdImagePolygon(im, static_cast<gdPointPtr>(points), pointsTotal, color);

    hb_xfree(points);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* original: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC(GDIMAGEOPENPOLYGON)
{
#if HB_GD_VERS(2, 0, 33)
  if (hb_isGdImage(1) && HB_ISARRAY(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pPoints = hb_param(2, Harbour::Item::ARRAY);
    auto pointsTotal = static_cast<int32_t>(hb_arrayLen(pPoints));
    auto color = hb_parni32(3);

    /* Max Points of polygon */
    auto points = static_cast<gdPoint *>(hb_xgrab(sizeof(gdPoint) * pointsTotal));

    for (auto i = 0; i < pointsTotal; i++) {
      auto pPoint = hb_arrayGetItemPtr(pPoints, i + 1);
      if (pPoint->isArray()) {
        points[i].x = hb_arrayGetNI32(pPoint, 1);
        points[i].y = hb_arrayGetNI32(pPoint, 2);
      } else {
        points[i].x = 0;
        points[i].y = 0;
      }
    }

    /* Draw a polygon */
    gdImageOpenPolygon(im, static_cast<gdPointPtr>(points), pointsTotal, color);

    hb_xfree(points);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}

/* void gdImageRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
HB_FUNC(GDIMAGERECTANGLE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x1 = hb_parni32(2);
    auto y1 = hb_parni32(3);
    auto x2 = hb_parni32(4);
    auto y2 = hb_parni32(5);
    auto color = hb_parni32(6);

    gdImageRectangle(im, x1, y1, x2, y2, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* original: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
/* implementation: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int color) */
HB_FUNC(GDIMAGEFILLEDPOLYGON)
{
  if (hb_isGdImage(1) && HB_ISARRAY(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pPoints = hb_param(2, Harbour::Item::ARRAY);
    auto pointsTotal = static_cast<int32_t>(hb_arrayLen(pPoints));
    auto color = hb_parni32(3);

    /* Max Points of polygon */
    auto points = static_cast<gdPoint *>(hb_xgrab(sizeof(gdPoint) * pointsTotal));

    for (auto i = 0; i < pointsTotal; i++) {
      auto pPoint = hb_arrayGetItemPtr(pPoints, i + 1);
      if (pPoint->isArray()) {
        points[i].x = hb_arrayGetNI32(pPoint, 1);
        points[i].y = hb_arrayGetNI32(pPoint, 2);
      } else {
        points[i].x = 0;
        points[i].y = 0;
      }
    }

    /* Draw a filled polygon */
    gdImageFilledPolygon(im, static_cast<gdPointPtr>(points), pointsTotal, color);

    hb_xfree(points);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageFilledRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
HB_FUNC(GDIMAGEFILLEDRECTANGLE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x1 = hb_parni32(2);
    auto y1 = hb_parni32(3);
    auto x2 = hb_parni32(4);
    auto y2 = hb_parni32(5);
    auto color = hb_parni32(6);

    gdImageFilledRectangle(im, x1, y1, x2, y2, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color) */
HB_FUNC(GDIMAGEARC)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8)) {
    gdImagePtr im = hb_parGdImage(1);

    auto cx = hb_parni32(2);
    auto cy = hb_parni32(3);
    auto w = hb_parni32(4); /* width and height */
    auto h = hb_parni32(5);
    auto s = hb_parni32(6); /* starting and ending degree */
    auto e = hb_parni32(7);
    auto color = hb_parni32(8);

    gdImageArc(im, cx, cy, w, h, s, e, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageFilledArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color[, int style]) */
HB_FUNC(GDIMAGEFILLEDARC)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8)) {
    gdImagePtr im = hb_parGdImage(1);

    auto cx = hb_parni32(2);
    auto cy = hb_parni32(3);
    auto w = hb_parni32(4); /* width and height */
    auto h = hb_parni32(5);
    auto s = hb_parni32(6); /* starting and ending degree */
    auto e = hb_parni32(7);
    auto color = hb_parni32(8);
    auto style = hb_parnidef(9, gdNoFill);

    gdImageFilledArc(im, cx, cy, w, h, s, e, color, style);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageFilledEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
HB_FUNC(GDIMAGEFILLEDELLIPSE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);

    auto cx = hb_parni32(2);
    auto cy = hb_parni32(3);
    auto w = hb_parni32(4);
    auto h = hb_parni32(5);
    auto color = hb_parni32(6);

    gdImageFilledEllipse(im, cx, cy, w, h, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageFillToBorder(gdImagePtr im, int x, int y, int border, int color) */
HB_FUNC(GDIMAGEFILLTOBORDER)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);
    auto border = hb_parni32(4);
    auto color = hb_parni32(5);

    gdImageFillToBorder(im, x, y, border, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Disabled, because there is a .prg implementation which
   works with all gd lib versions. [vszakats] */
#if 0
/* void gdImageEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
HB_FUNC(GDIMAGEELLIPSE)
{
#if HB_GD_VERS(2, 0, 35)
   if( hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) )
   {
      gdImagePtr im = hb_parGdImage(1);

      auto cx    = hb_parni32(2);
      auto cy    = hb_parni32(3);
      auto w     = hb_parni32(4);
      auto h     = hb_parni32(5);
      auto color = hb_parni32(6);

      gdImageEllipse(im, cx, cy, w, h, color);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
#endif
}
#endif

/* void gdImageFill(gdImagePtr im, int x, int y, int color) */
HB_FUNC(GDIMAGEFILL)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);
    auto color = hb_parni32(4);

    gdImageFill(im, x, y, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetAntiAliased(gdImagePtr im, int color) */
HB_FUNC(GDIMAGESETANTIALIASED)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    gdImageSetAntiAliased(im, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetAntiAliasedDontBlend(gdImagePtr im, int c, int dont_blend) */
HB_FUNC(GDIMAGESETANTIALIASEDDONTBLEND)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);
    auto dont_blend = hb_parni32(3);

    gdImageSetAntiAliasedDontBlend(im, color, dont_blend);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetBrush(gdImagePtr im, gdImagePtr brush) */
HB_FUNC(GDIMAGESETBRUSH)
{
  if (hb_isGdImage(1) && hb_isGdImage(2)) {
    gdImagePtr im = hb_parGdImage(1);
    gdImagePtr brush = hb_parGdImage(2);

    gdImageSetBrush(im, brush);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetTile(gdImagePtr im, gdImagePtr tile) */
HB_FUNC(GDIMAGESETTILE)
{
  if (hb_isGdImage(1) && hb_isGdImage(2)) {
    gdImagePtr im = hb_parGdImage(1);
    gdImagePtr tile = hb_parGdImage(2);

    gdImageSetTile(im, tile);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* original: void gdImageSetStyle(gdImagePtr im, int *style, int styleLength) */
/* implementation: void gdImageSetStyle(gdImagePtr im, int *style) */
HB_FUNC(GDIMAGESETSTYLE)
{
  if (hb_isGdImage(1) && HB_ISARRAY(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pStyles = hb_param(2, Harbour::Item::ARRAY);
    auto styleLength = static_cast<int32_t>(hb_arrayLen(pStyles));

    /* Max numbery of Styles */
    auto styles = static_cast<int32_t *>(hb_xgrab(sizeof(int32_t) * styleLength));

    for (auto i = 0; i < styleLength; i++) {
      styles[i] = hb_arrayGetNI32(pStyles, i + 1);
    }

    /* Set style */
    gdImageSetStyle(im, static_cast<int32_t *>(styles), styleLength);

    hb_xfree(styles);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetThickness(gdImagePtr im, int thickness) */
HB_FUNC(GDIMAGESETTHICKNESS)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto thickness = hb_parni32(2);

    /* Return previous */
    hb_retni(im->thick);

    gdImageSetThickness(im, thickness);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageAlphaBlending(gdImagePtr im, int blending) */
HB_FUNC(GDIMAGEALPHABLENDING)
{
  if (hb_isGdImage(1) && HB_ISLOG(2)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t blending = hb_parl(2) ? 1 : 0;

    gdImageAlphaBlending(im, blending);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSaveAlpha(gdImagePtr im, int saveFlag) */
HB_FUNC(GDIMAGESAVEALPHA)
{
  if (hb_isGdImage(1) && HB_ISLOG(2)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t saveFlag = hb_parl(2) ? 1 : 0;

    gdImageSaveAlpha(im, saveFlag);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSetClip(gdImagePtr im, int x1, int y1, int x2, int y2) */
HB_FUNC(GDIMAGESETCLIP)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x1 = hb_parni32(2);
    auto y1 = hb_parni32(3);
    auto x2 = hb_parni32(4);
    auto y2 = hb_parni32(5);

    gdImageSetClip(im, x1, y1, x2, y2);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* original: void gdImageGetClip(gdImagePtr im, int *x1P, int *y1P, int *x2P, int *y2P) */
/* implementation: array gdImageGetClip(gdImagePtr im) */
HB_FUNC(GDIMAGEGETCLIP)
{
  if (hb_isGdImage(1)) {
    int32_t x1, y1, x2, y2;

    /* Get clipping rectangle */
    gdImageGetClip(hb_parGdImage(1), &x1, &y1, &x2, &y2);

    /* Return clipping rectangle value in an array */
    auto pClipArray = hb_itemArrayNew(4);
    hb_itemPutNI32(hb_arrayGetItemPtr(pClipArray, 1), x1);
    hb_itemPutNI32(hb_arrayGetItemPtr(pClipArray, 2), y1);
    hb_itemPutNI32(hb_arrayGetItemPtr(pClipArray, 3), x2);
    hb_itemPutNI32(hb_arrayGetItemPtr(pClipArray, 4), y2);

    /* return array */
    hb_itemReturnRelease(pClipArray);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* QUERY FUNCTIONS */

/* int gdImageColorsTotal(gdImagePtr im) */
HB_FUNC(GDIMAGECOLORSTOTAL)
{
  if (hb_isGdImage(1)) {
    hb_retni(gdImageColorsTotal(hb_parGdImage(1)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageAlpha(gdImagePtr im, int color) */
HB_FUNC(GDIMAGEALPHA)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    hb_retni(gdImageAlpha(im, color));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageRed(gdImagePtr im, int color) */
HB_FUNC(GDIMAGERED)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    hb_retni(gdImageRed(im, color));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageGreen(gdImagePtr im, int color) */
HB_FUNC(GDIMAGEGREEN)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    hb_retni(gdImageGreen(im, color));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageBlue(gdImagePtr im, int color) */
HB_FUNC(GDIMAGEBLUE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    hb_retni(gdImageBlue(im, color));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageSX(gdImagePtr im) */
HB_FUNC(GDIMAGESX)
{
  if (hb_isGdImage(1)) {
    /* Get Image Width in pixels */
    hb_retni(gdImageSX(hb_parGdImage(1)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageSX(gdImagePtr im) */
HB_FUNC(GDIMAGESY)
{
  if (hb_isGdImage(1)) {
    /* Get Image Height in pixels */
    hb_retni(gdImageSY(hb_parGdImage(1)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageGetPixel(gdImagePtr im, int x, int y) */
HB_FUNC(GDIMAGEGETPIXEL)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);

    /* Get Color of a pixel */
    hb_retni(gdImageGetPixel(im, x, y));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageBoundsSafe(gdImagePtr im, int x, int y) */
HB_FUNC(GDIMAGEBOUNDSSAFE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);

    /* Get if pixel in Clipping region */
    hb_retl(gdImageBoundsSafe(im, x, y) != 0);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageGetInterlaced(gdImagePtr im) */
HB_FUNC(GDIMAGEGETINTERLACED)
{
  if (hb_isGdImage(1)) {
    hb_retl(gdImageGetInterlaced(hb_parGdImage(1)) != 0);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageGetTransparent(gdImagePtr im) */
HB_FUNC(GDIMAGEGETTRANSPARENT)
{
  if (hb_isGdImage(1)) {
    hb_retl(gdImageGetTransparent(hb_parGdImage(1)) != 0);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageTrueColor(gdImagePtr im) */
HB_FUNC(GDIMAGETRUECOLOR)
{
  if (hb_isGdImage(1)) {
    hb_retl(gdImageTrueColor(hb_parGdImage(1)) != 0);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageTrueColorToPalette (gdImagePtr im, int ditherFlag, int colorsWanted) */
HB_FUNC(GDIMAGETRUECOLORTOPALETTE)
{
  if (hb_isGdImage(1) && HB_ISLOG(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t ditherFlag = hb_parl(2) ? 1 : 0;
    auto colorsWanted = hb_parni32(3);

    /* Converts a truecolor image to a palette-based image */
    gdImageTrueColorToPalette(im, ditherFlag, colorsWanted);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* gdImagePtr gdImageCreatePaletteFromTrueColor(gdImagePtr im, int ditherFlag, int colorsWanted) */
HB_FUNC(GDIMAGECREATEPALETTEFROMTRUECOLOR)
{
  if (hb_isGdImage(1) && HB_ISLOG(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t ditherFlag = hb_parl(2) ? 1 : 0;
    auto colorsWanted = hb_parni32(3);

    /* Converts a truecolor image to a palette-based image and return the image */
    hb_retGdImage(gdImageCreatePaletteFromTrueColor(im, ditherFlag, colorsWanted));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImagePalettePixel(gdImagePtr im, int x, int y) */
HB_FUNC(GDIMAGEPALETTEPIXEL)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);

    /* Get Color of a pixel */
    hb_retni(gdImagePalettePixel(im, x, y));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageTrueColorPixel(gdImagePtr im, int x, int y) */
HB_FUNC(GDIMAGETRUECOLORPIXEL)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    gdImagePtr im = hb_parGdImage(1);

    auto x = hb_parni32(2);
    auto y = hb_parni32(3);

    /* Get Color of a pixel */
    hb_retni(gdImageTrueColorPixel(im, x, y));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageGetThickness(gdImagePtr im) */
HB_FUNC(GDIMAGEGETTHICKNESS)
{
  if (hb_isGdImage(1)) {
    hb_retni(hb_parGdImage(1)->thick);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* FONTS AND TEXT-HANDLING FUNCTIONS */

/* gdFontPtr gdFontGetSmall(void) */
HB_FUNC(GDFONTGETSMALL)
{
  hb_retGdFont(gdFontGetSmall());
}

/* gdFontPtr gdFontGetLarge(void) */
HB_FUNC(GDFONTGETLARGE)
{
  hb_retGdFont(gdFontGetLarge());
}

/* gdFontPtr gdFontGetMediumBold(void) */
HB_FUNC(GDFONTGETMEDIUMBOLD)
{
  hb_retGdFont(gdFontGetMediumBold());
}

/* gdFontPtr gdFontGetGiant(void) */
HB_FUNC(GDFONTGETGIANT)
{
  hb_retGdFont(gdFontGetGiant());
}

/* gdFontPtr gdFontGetTiny(void) */
HB_FUNC(GDFONTGETTINY)
{
  hb_retGdFont(gdFontGetTiny());
}

/* void gdImageChar(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
/* void gdImageString(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
HB_FUNC(GDIMAGESTRING)
{
  if (hb_isGdImage(1) && hb_isGdFont(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISCHAR(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);
    gdFontPtr font = hb_parGdFont(2);

    auto x = hb_parni32(3);
    auto y = hb_parni32(4);
    auto color = hb_parni32(6);

    /* Write string */
    gdImageString(im, font, x, y, reinterpret_cast<unsigned char *>(const_cast<char *>(hb_parc(5))), color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC_TRANSLATE(GDIMAGECHAR, GDIMAGESTRING)

/* void gdImageCharUp(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
/* void gdImageStringUp(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
HB_FUNC(GDIMAGESTRINGUP)
{
  if (hb_isGdImage(1) && hb_isGdFont(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISCHAR(5) && HB_ISNUM(6)) {
    gdImagePtr im = hb_parGdImage(1);
    gdFontPtr font = hb_parGdFont(2);

    auto x = hb_parni32(3);
    auto y = hb_parni32(4);
    auto color = hb_parni32(6);

    /* Write string */
    gdImageStringUp(im, font, x, y, reinterpret_cast<unsigned char *>(const_cast<char *>(hb_parc(5))), color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC_TRANSLATE(GDIMAGECHARUP, GDIMAGESTRINGUP)

/* char *gdImageStringFTEx(gdImagePtr im, int *brect, int fg, char *fontname, double ptsize, double angle, int x, int y,
 * char *string, gdFTStringExtraPtr strex) */
/* implementation: cError := gdImageStringFTEx( im, aRect, int fg, cFontname, nPtSize, nAngle, x, y, cString,
 * nLinespacing, nCharmap, nResolution ) */
HB_FUNC(GDIMAGESTRINGFTEX)
{
  if ((HB_ISNIL(1) || hb_isGdImage(1)) && HB_ISARRAY(2) && HB_ISNUM(3) && HB_ISCHAR(4) && HB_ISNUM(5) && HB_ISNUM(6) &&
      HB_ISNUM(7) && HB_ISNUM(8) && HB_ISCHAR(9)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pRect = hb_param(2, Harbour::Item::ARRAY);
    auto fgcolor = hb_parni32(3); /* foreground color */
    auto fontname = hb_parc(4);
    auto ptsize = hb_parnd(5); /* point size */
    auto angle = hb_parnd(6);  /* angle value in radians */
    auto x = hb_parni32(7);
    auto y = hb_parni32(8);
    auto string = hb_parc(9);

    gdFTStringExtra extra;
    int32_t flags = 0; /* Extended flags */

    /* defaults */
    double linespacing = 1.05;
    int32_t charmap = gdFTEX_Unicode;
    int32_t resolution = 96;

    int32_t aRect[8];
    char *err;

    /* Retrieve rectangle array */
    for (auto i = 0; i < 8; i++) {
      aRect[i] = hb_arrayGetNI32(pRect, i + 1);
    }

    /* Retrieve line spacing */
    if (HB_ISNUM(10)) {
      linespacing = hb_parnd(10);
      flags |= gdFTEX_LINESPACE;
    }

    /* Retrieve charmap */
    if (HB_ISNUM(11)) {
      charmap = hb_parni32(11);
      flags |= gdFTEX_CHARMAP;
    }

    /* Retrieve resolution */
    if (HB_ISNUM(12)) {
      resolution = hb_parni32(12);
      flags |= gdFTEX_RESOLUTION;
    }

    if (flags != 0) {
      extra.flags = flags;
      extra.linespacing = (flags & gdFTEX_LINESPACE ? linespacing : 1.05);
      extra.charmap = (flags & gdFTEX_CHARMAP ? charmap : gdFTEX_Unicode);
      extra.hdpi = (flags & gdFTEX_RESOLUTION ? resolution : 96);
      extra.vdpi = (flags & gdFTEX_RESOLUTION ? resolution : 96);
    }

    /* Write string */
    err = gdImageStringFTEx(im, &aRect[0], fgcolor, const_cast<char *>(fontname), ptsize, angle, x, y,
                            const_cast<char *>(string), (flags != 0 ? &extra : nullptr));
    if (!err) {
      /* Save in array the correct text rectangle dimensions */
      auto pArray = hb_itemArrayNew(8);
      for (auto i = 0; i < 8; i++) {
        hb_itemPutNI32(hb_arrayGetItemPtr(pArray, i + 1), aRect[i]);
      }
      hb_itemCopy(pRect, pArray);
      hb_itemRelease(pArray);
    }
    hb_retc(err);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* char *gdImageStringFTCircle(gdImagePtr im, int cx, int cy, double radius, double textRadius, double fillPortion, char
 * *font, double points, char *top, char *bottom, int fgcolor) */
HB_FUNC(GDIMAGESTRINGFTCIRCLE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISCHAR(7) &&
      HB_ISNUM(8) && (HB_ISNIL(9) || HB_ISCHAR(9)) && (HB_ISNIL(10) || HB_ISCHAR(10)) && HB_ISNUM(11)) {
    gdImagePtr im = hb_parGdImage(1);

    auto cx = hb_parni32(2);
    auto cy = hb_parni32(3);
    auto radius = hb_parnd(4);
    auto textRadius = hb_parnd(5);
    auto fillPortion = hb_parnd(6);
    auto fontname = hb_parc(7);
    auto points = hb_parnd(8);
    auto top = hb_parcx(9);
    auto bottom = hb_parcx(10);
    auto fgcolor = hb_parni32(11); /* foreground color */

    /* Write string */
    hb_retc(gdImageStringFTCircle(im, cx, cy, radius, textRadius, fillPortion, const_cast<char *>(fontname), points,
                                  const_cast<char *>(top), const_cast<char *>(bottom), fgcolor));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdFontCacheSetup (void) */
HB_FUNC(GDFONTCACHESETUP)
{
  /* This function initializes the font cache for freetype text output functions */
  hb_retl(gdFontCacheSetup() != 0);
}

/* void gdFontCacheShutdown (void) */
HB_FUNC(GDFONTCACHESHUTDOWN)
{
  /* This function initializes the font cache for freetype text output functions */
  gdFontCacheShutdown();
}

HB_FUNC(GDFONTGETWIDTH)
{
  if (hb_isGdFont(1)) {
    hb_retni(hb_parGdFont(1)->w);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(GDFONTGETHEIGHT)
{
  if (hb_isGdFont(1)) {
    hb_retni(hb_parGdFont(1)->h);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* COLOR HANDLING FUNCTIONS */

/* int gdImageColorAllocate(gdImagePtr im, int r, int g, int b) */
HB_FUNC(GDIMAGECOLORALLOCATE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);

    /* return color */
    hb_retni(gdImageColorAllocate(im, r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageColorDeallocate(gdImagePtr im, int color) */
HB_FUNC(GDIMAGECOLORDEALLOCATE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    gdImageColorDeallocate(im, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorAllocateAlpha(gdImagePtr im, int r, int g, int b, int a) */
HB_FUNC(GDIMAGECOLORALLOCATEALPHA)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);
    auto a = hb_parni32(5); /* alpha */

    /* return color */
    hb_retni(gdImageColorAllocateAlpha(im, r, g, b, a));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorClosest(gdImagePtr im, int r, int g, int b) */
HB_FUNC(GDIMAGECOLORCLOSEST)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);

    /* return color */
    hb_retni(gdImageColorClosest(im, r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorClosestAlpha(gdImagePtr im, int r, int g, int b, int a) */
HB_FUNC(GDIMAGECOLORCLOSESTALPHA)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);
    auto a = hb_parni32(5); /* alpha */

    /* return color */
    hb_retni(gdImageColorClosestAlpha(im, r, g, b, a));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* gdImageColorClosestHWB(gdImagePtr im, int r, int g, int b) */
HB_FUNC(GDIMAGECOLORCLOSESTHWB)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);

    /* return color */
    hb_retni(gdImageColorClosestHWB(im, r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorExact(gdImagePtr im, int r, int g, int b) */
HB_FUNC(GDIMAGECOLOREXACT)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);

    /* return color */
    hb_retni(gdImageColorExact(im, r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorResolve(gdImagePtr im, int r, int g, int b) */
HB_FUNC(GDIMAGECOLORRESOLVE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);

    /* return color */
    hb_retni(gdImageColorResolve(im, r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdImageColorResolveAlpha(gdImagePtr im, int r, int g, int b, int a) */
HB_FUNC(GDIMAGECOLORRESOLVEALPHA)
{
  if (hb_isGdImage(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5)) {
    gdImagePtr im = hb_parGdImage(1);

    auto r = hb_parni32(2); /* RGB */
    auto g = hb_parni32(3);
    auto b = hb_parni32(4);
    auto a = hb_parni32(5); /* alpha */

    /* return color */
    hb_retni(gdImageColorResolveAlpha(im, r, g, b, a));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageColorTransparent(gdImagePtr im, int color) */
HB_FUNC(GDIMAGECOLORTRANSPARENT)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto color = hb_parni32(2);

    /* Set transparent color (to define no transparent color set -1) */
    gdImageColorTransparent(im, color);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdTrueColor(int red, int green, int blue) */
HB_FUNC(GDTRUECOLOR)
{
  if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3)) {
    auto r = hb_parni32(1); /* RGB */
    auto g = hb_parni32(2);
    auto b = hb_parni32(3);

    /* return color */
    hb_retni(gdTrueColor(r, g, b));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* int gdTrueColorAlpha(int red, int green, int blue, int alpha) */
HB_FUNC(GDTRUECOLORALPHA)
{
  if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4)) {
    auto r = hb_parni32(1); /* RGB */
    auto g = hb_parni32(2);
    auto b = hb_parni32(3);
    auto a = hb_parni32(4); /* alpha */

    /* return color */
    hb_retni(gdTrueColorAlpha(r, g, b, a));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* COPY AND RESIZING FUNCTIONS */

/* void gdImageCopy(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h) */
HB_FUNC(GDIMAGECOPY)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parni32(3);
    auto dstY = hb_parni32(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto w = hb_parni32(7);
    auto h = hb_parni32(8);

    gdImageCopy(dst, src, dstX, dstY, srcX, srcY, w, h);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageCopyResized(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH,
 * int srcW, int srcH) */
HB_FUNC(GDIMAGECOPYRESIZED)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8) && HB_ISNUM(9) && HB_ISNUM(10)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parni32(3);
    auto dstY = hb_parni32(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto dstW = hb_parni32(7);
    auto dstH = hb_parni32(8);
    auto srcW = hb_parni32(9);
    auto srcH = hb_parni32(10);

    gdImageCopyResized(dst, src, dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageCopyResampled(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH,
 * int srcW, int srcH) */
HB_FUNC(GDIMAGECOPYRESAMPLED)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8) && HB_ISNUM(9) && HB_ISNUM(10)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parni32(3);
    auto dstY = hb_parni32(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto dstW = hb_parni32(7);
    auto dstH = hb_parni32(8);
    auto srcW = hb_parni32(9);
    auto srcH = hb_parni32(10);

    gdImageCopyResampled(dst, src, dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageCopyRotated(gdImagePtr dst, gdImagePtr src, double dstX, double dstY, int srcX, int srcY, int srcW, int
 * srcH, int angle) */
HB_FUNC(GDIMAGECOPYROTATED)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8) && HB_ISNUM(9)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parnd(3);
    auto dstY = hb_parnd(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto srcW = hb_parni32(7);
    auto srcH = hb_parni32(8);
    auto angle = hb_parni32(9);

    /* Perform rotation */
    gdImageCopyRotated(dst, src, dstX, dstY, srcX, srcY, srcW, srcH, angle);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageCopyMerge(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int pct)
 */
HB_FUNC(GDIMAGECOPYMERGE)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8) && HB_ISNUM(9)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parni32(3);
    auto dstY = hb_parni32(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto w = hb_parni32(7);
    auto h = hb_parni32(8);
    auto pct = hb_parni32(9);

    gdImageCopyMerge(dst, src, dstX, dstY, srcX, srcY, w, h, pct);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageCopyMergeGray(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int
 * pct) */
HB_FUNC(GDIMAGECOPYMERGEGRAY)
{
  if (hb_isGdImage(1) && hb_isGdImage(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6) && HB_ISNUM(7) &&
      HB_ISNUM(8) && HB_ISNUM(9)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    auto dstX = hb_parni32(3);
    auto dstY = hb_parni32(4);
    auto srcX = hb_parni32(5);
    auto srcY = hb_parni32(6);
    auto w = hb_parni32(7);
    auto h = hb_parni32(8);
    auto pct = hb_parni32(9);

    gdImageCopyMergeGray(dst, src, dstX, dstY, srcX, srcY, w, h, pct);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImagePaletteCopy(gdImagePtr dst, gdImagePtr src) */
HB_FUNC(GDIMAGEPALETTECOPY)
{
  if (hb_isGdImage(1) && hb_isGdImage(2)) {
    gdImagePtr dst = hb_parGdImage(1);
    gdImagePtr src = hb_parGdImage(2);

    gdImagePaletteCopy(dst, src);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSquareToCircle(gdImagePtr im, int radius) */
HB_FUNC(GDIMAGESQUARETOCIRCLE)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto radius = hb_parni32(2);

    hb_retGdImage(gdImageSquareToCircle(im, radius));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageSharpen(gdImagePtr im, int pct) */
HB_FUNC(GDIMAGESHARPEN)
{
  if (hb_isGdImage(1) && HB_ISNUM(2)) {
    gdImagePtr im = hb_parGdImage(1);

    auto pct = hb_parni32(2);

    gdImageSharpen(im, pct);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* MISCELLANEOUS FUNCTIONS */

/* int gdImageCompare(gdImagePtr im1, gdImagePtr im2) */
HB_FUNC(GDIMAGECOMPARE)
{
  if (hb_isGdImage(1) && hb_isGdImage(2)) {
    /* Compare images - if return != 0 check value for infos */
    hb_retni(gdImageCompare(hb_parGdImage(1), hb_parGdImage(2)));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* void gdImageInterlace(gdImagePtr im, int interlace) */
HB_FUNC(GDIMAGEINTERLACE)
{
  if (hb_isGdImage(1) && HB_ISLOG(2)) {
    gdImagePtr im = hb_parGdImage(1);

    int32_t interlace = hb_parl(2) ? 1 : 0;

    gdImageInterlace(im, interlace);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#if HB_GD_VERS(2, 0, 33)

static void AddImageToFile(const char *szFile, const void *iptr, int sz)
{
  HB_FHANDLE fhandle;

  if ((fhandle = hb_fsOpen(szFile, FO_READWRITE)) != FS_ERROR) {
    /* move to end of file */
    hb_fsSeek(fhandle, 0, FS_END);

    /* Write Image */
    SaveImageToHandle(fhandle, iptr, sz);

    hb_fsClose(fhandle);
  }
}

#endif

/*BGD_DECLARE(void *) gdImageGifAnimBeginPtr(gdImagePtr im, int *size, int GlobalCM, int Loops); */
/* implementation: (void *) gdImageGifAnimBegin( gdImagePtr im, cFile | nHandle, int GlobalCM, int Loops); */
HB_FUNC(GDIMAGEGIFANIMBEGIN)
{
#if HB_GD_VERS(2, 0, 33)
  if (hb_isGdImage(1) && (HB_ISCHAR(2) || HB_ISNUM(2) || HB_ISNIL(2)) && HB_ISNUM(3) && HB_ISNUM(4)) {
    gdImagePtr im = hb_parGdImage(1);

    auto GlobalCM = hb_parni32(3); /* global color map */
    auto Loops = hb_parni32(4);

    int32_t size;
    void *iptr = gdImageGifAnimBeginPtr(im, &size, GlobalCM, Loops);

    /* Check if parameter is a file name or a handle */
    if (HB_ISCHAR(2)) {
      SaveImageToFile(hb_parc(2), iptr, size);
    } else {
      SaveImageToHandle(hb_numToHandle(hb_parnintdef(2, 1 /* std output */)), iptr, size);
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}

/*BGD_DECLARE(void *) gdImageGifAnimAddPtr(gdImagePtr im, int *size, int LocalCM, int LeftOfs, int TopOfs, int Delay,
 * int Disposal, gdImagePtr previm); */
/* implementation: (void *) gdImageGifAnimAdd(gdImagePtr im, cFile | nHandle, int LocalCM, int LeftOfs, int TopOfs, int
 * Delay, int Disposal, gdImagePtr previm); */
HB_FUNC(GDIMAGEGIFANIMADD)
{
#if HB_GD_VERS(2, 0, 33)
  if (hb_isGdImage(1) && (HB_ISCHAR(2) || HB_ISNUM(2) || HB_ISNIL(2)) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) &&
      HB_ISNUM(6) && HB_ISNUM(7) && (hb_isGdImage(8) || HB_ISNIL(8))) {
    gdImagePtr im = hb_parGdImage(1);
    gdImagePtr previm = hb_parGdImage(8);

    auto LocalCM = hb_parni32(3);
    auto LeftOfs = hb_parni32(4);
    auto TopOfs = hb_parni32(5);
    auto Delay = hb_parni32(6);
    auto Disposal = hb_parni32(7);

    int32_t size;
    void *iptr = gdImageGifAnimAddPtr(im, &size, LocalCM, LeftOfs, TopOfs, Delay, Disposal, previm);

    /* Check if parameter is a file name or a handle */
    if (HB_ISCHAR(2)) {
      AddImageToFile(hb_parc(2), iptr, size);
    } else {
      SaveImageToHandle(hb_numToHandle(hb_parnintdef(2, 1 /* std output */)), iptr, size);
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}

/*BGD_DECLARE(void *) gdImageGifAnimEndPtr(int *size); */
/* implementation: gdImageGifAnimEnd(cFile | nHandle); */
HB_FUNC(GDIMAGEGIFANIMEND)
{
#if HB_GD_VERS(2, 0, 33)
  if (HB_ISCHAR(1) || HB_ISNUM(1) || HB_ISNIL(1)) {
    int32_t size;
    void *iptr = gdImageGifAnimEndPtr(&size);

    /* Check if 1st parameter is a file name or a handle */
    if (HB_ISCHAR(1)) {
      AddImageToFile(hb_parc(1), iptr, size);
    } else {
      SaveImageToHandle(hb_numToHandle(hb_parnintdef(1, 1 /* std output */)), iptr, size);
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}
