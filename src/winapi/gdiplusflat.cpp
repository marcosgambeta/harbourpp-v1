//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <gdiplus.h>
#include <vector>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

#define wa_ret_GpStatus(x) hb_retni(x)

//#define wa_par_REAL(n) hb_parnd(n)
//#define wa_stor_REAL(x, n) hb_stornd(x, n);

#define wa_par_ARGB(n) hb_parnl(n) // TODO: check type
#define wa_stor_ARGB(x, n) hb_stornl(x, n); // TODO: check type

#define wa_par_GpAdjustableArrowCap(n)            static_cast<GpAdjustableArrowCap*>(hb_parptr(n))
#define wa_par_GpBitmap(n)                        static_cast<GpBitmap*>(hb_parptr(n))
#define wa_par_GpBrush(n)                         static_cast<GpBrush*>(hb_parptr(n))
#define wa_par_GpCachedBitmap(n)                  static_cast<GpCachedBitmap*>(hb_parptr(n))
#define wa_par_GpColorMatrix(n)                   static_cast<ColorMatrix*>(hb_parptr(n))
#define wa_par_GpColorPalette(n)                  static_cast<ColorPalette*>(hb_parptr(n))
#define wa_par_GpCustomLineCap(n)                 static_cast<GpCustomLineCap*>(hb_parptr(n))
#define wa_par_GpEncoderParameters(n)             static_cast<EncoderParameters*>(hb_parptr(n))
#define wa_par_GpFont(n)                          static_cast<GpFont*>(hb_parptr(n))
#define wa_par_GpFontCollection(n)                static_cast<GpFontCollection*>(hb_parptr(n))
#define wa_par_GpFontFamily(n)                    static_cast<GpFontFamily*>(hb_parptr(n))
#define wa_par_GpGraphics(n)                      static_cast<GpGraphics*>(hb_parptr(n))
#define wa_par_GpHatch(n)                         static_cast<GpHatch*>(hb_parptr(n))
#define wa_par_GpImage(n)                         static_cast<GpImage*>(hb_parptr(n))
#define wa_par_GpImageAttributes(n)               static_cast<GpImageAttributes*>(hb_parptr(n))
#define wa_par_GpLineGradient(n)                  static_cast<GpLineGradient*>(hb_parptr(n))
#define wa_par_GpMatrix(n)                        static_cast<GpMatrix*>(hb_parptr(n))
#define wa_par_GpMetafile(n)                      static_cast<GpMetafile*>(hb_parptr(n))
#define wa_par_GpPath(n)                          static_cast<GpPath*>(hb_parptr(n))
#define wa_par_GpPathGradient(n)                  static_cast<GpPathGradient*>(hb_parptr(n))
#define wa_par_GpPathIterator(n)                  static_cast<GpPathIterator*>(hb_parptr(n))
#define wa_par_GpPen(n)                           static_cast<GpPen*>(hb_parptr(n))
#define wa_par_GpPoint(n)                         static_cast<GpPoint*>(wa_get_ptr(n))
#define wa_par_GpPointF(n)                        static_cast<GpPointF*>(wa_get_ptr(n))
#define wa_par_GpPropertyItem(n)                  static_cast<PropertyItem*>(hb_parptr(n))
#define wa_par_GpRect(n)                          static_cast<GpRect*>(wa_get_ptr(n))
#define wa_par_GpRectF(n)                         static_cast<GpRectF*>(wa_get_ptr(n))
#define wa_par_GpRegion(n)                        static_cast<GpRegion*>(hb_parptr(n))
#define wa_par_GpSolidFill(n)                     static_cast<GpSolidFill*>(hb_parptr(n))
#define wa_par_GpStringFormat(n)                  static_cast<GpStringFormat*>(hb_parptr(n))
#define wa_par_GpTexture(n)                       static_cast<GpTexture*>(hb_parptr(n))
#define wa_par_IStream(n)                         static_cast<IStream*>(hb_parptr(n))
#define wa_par_WmfPlaceableFileHeader(n)          static_cast<WmfPlaceableFileHeader*>(hb_parptr(n))

#define wa_par_GpColorAdjustType(n)               static_cast<ColorAdjustType>(hb_parni(n))
#define wa_par_GpColorChannelFlags(n)             static_cast<ColorChannelFlags>(hb_parni(n))
#define wa_par_GpColorMatrixFlags(n)              static_cast<ColorMatrixFlags>(hb_parni(n))
#define wa_par_GpCombineMode(n)                   static_cast<CombineMode>(hb_parni(n))
#define wa_par_GpCompositingMode(n)               static_cast<CompositingMode>(hb_parni(n))
#define wa_par_GpCompositingQuality(n)            static_cast<CompositingQuality>(hb_parni(n))
#define wa_par_GpCoordinateSpace(n)               static_cast<GpCoordinateSpace>(hb_parni(n))
#define wa_par_GpCustomLineCapType(n)             static_cast<CustomLineCapType>(hb_parni(n))
#define wa_par_GpDashCap(n)                       static_cast<GpDashCap>(hb_parni(n))
#define wa_par_GpDashStyle(n)                     static_cast<GpDashStyle>(hb_parni(n))
#define wa_par_GpDitherType(n)                    static_cast<DitherType>(hb_parni(n))
#define wa_par_GpEmfPlusRecordType(n)             static_cast<EmfPlusRecordType>(hb_parni(n))
#define wa_par_GpEmfType(n)                       static_cast<EmfType>(hb_parni(n))
#define wa_par_GpFillMode(n)                      static_cast<GpFillMode>(hb_parni(n))
#define wa_par_GpFlushIntention(n)                static_cast<GpFlushIntention>(hb_parni(n))
#define wa_par_GpGraphicsContainer(n)             static_cast<GraphicsContainer>(hb_parni(n))
#define wa_par_GpGraphicsState(n)                 static_cast<GraphicsState>(hb_parni(n))
#define wa_par_GpHatchStyle(n)                    static_cast<GpHatchStyle>(hb_parni(n))
#define wa_par_GpHistogramFormat(n)               static_cast<HistogramFormat>(hb_parni(n))
#define wa_par_GpInterpolationMode(n)             static_cast<InterpolationMode>(hb_parni(n))
#define wa_par_GpLinearGradientMode(n)            static_cast<LinearGradientMode>(hb_parni(n))
#define wa_par_GpLineCap(n)                       static_cast<GpLineCap>(hb_parni(n))
#define wa_par_GpLineJoin(n)                      static_cast<GpLineJoin>(hb_parni(n))
#define wa_par_GpMatrixOrder(n)                   static_cast<GpMatrixOrder>(hb_parni(n))
#define wa_par_GpPaletteType(n)                   static_cast<PaletteType>(hb_parni(n))
#define wa_par_GpPenAlignment(n)                  static_cast<GpPenAlignment>(hb_parni(n))
#define wa_par_GpPixelFormat(n)                   static_cast<PixelFormat>(hb_parni(n))
#define wa_par_GpPixelOffsetMode(n)               static_cast<PixelOffsetMode>(hb_parni(n))
#define wa_par_GpRotateFlipType(n)                static_cast<RotateFlipType>(hb_parni(n))
#define wa_par_GpSmoothingMode(n)                 static_cast<SmoothingMode>(hb_parni(n))
#define wa_par_GpStringAlignment(n)               static_cast<StringAlignment>(hb_parni(n))
#define wa_par_GpStringDigitSubstitute(n)         static_cast<StringDigitSubstitute>(hb_parni(n))
#define wa_par_GpStringTrimming(n)                static_cast<StringTrimming>(hb_parni(n))
#define wa_par_GpTextRenderingHint(n)             static_cast<TextRenderingHint>(hb_parni(n))
#define wa_par_GpUnit(n)                          static_cast<GpUnit>(hb_parni(n))
#define wa_par_GpWarpMode(n)                      static_cast<WarpMode>(hb_parni(n))
#define wa_par_GpWrapMode(n)                      static_cast<GpWrapMode>(hb_parni(n))
#define wa_par_MetafileFrameUnit(n)               static_cast<MetafileFrameUnit>(hb_parni(n))

using namespace Gdiplus;
using namespace Gdiplus::DllExports;

static std::vector<GpPoint> GpPointArrayToVector(const PHB_ITEM pArray);
static std::vector<GpPointF> GpPointFArrayToVector(const PHB_ITEM pArray);
static std::vector<GpRect> GpRectArrayToVector(const PHB_ITEM pArray);
static std::vector<GpRectF> GpRectFArrayToVector(const PHB_ITEM pArray);
static std::vector<REAL> REALArrayToVector(const PHB_ITEM pArray);
static std::vector<ARGB> ARGBArrayToVector(const PHB_ITEM pArray);
static std::vector<BYTE> BYTEArrayToVector(const PHB_ITEM pArray);

///////////////////////////////////////////////////////////////////////////////
// AdjustableArrowCap functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateAdjustableArrowCap(REAL,REAL,BOOL,GpAdjustableArrowCap**)
HB_FUNC( WAGDIPCREATEADJUSTABLEARROWCAP )
{
  GpAdjustableArrowCap * p{};
  wa_ret_GpStatus(GdipCreateAdjustableArrowCap(wa_par_REAL(1), wa_par_REAL(2), wa_par_BOOL(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap*,REAL)
HB_FUNC( WAGDIPSETADJUSTABLEARROWCAPHEIGHT )
{
  wa_ret_GpStatus(GdipSetAdjustableArrowCapHeight(wa_par_GpAdjustableArrowCap(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap*,REAL*)
HB_FUNC( WAGDIPGETADJUSTABLEARROWCAPHEIGHT )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetAdjustableArrowCapHeight(wa_par_GpAdjustableArrowCap(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap*,REAL)
HB_FUNC( WAGDIPSETADJUSTABLEARROWCAPWIDTH )
{
  wa_ret_GpStatus(GdipSetAdjustableArrowCapWidth(wa_par_GpAdjustableArrowCap(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap*,REAL*)
HB_FUNC( WAGDIPGETADJUSTABLEARROWCAPWIDTH )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetAdjustableArrowCapWidth(wa_par_GpAdjustableArrowCap(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap*,REAL)
HB_FUNC( WAGDIPSETADJUSTABLEARROWCAPMIDDLEINSET )
{
  wa_ret_GpStatus(GdipSetAdjustableArrowCapMiddleInset(wa_par_GpAdjustableArrowCap(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap*,REAL*)
HB_FUNC( WAGDIPGETADJUSTABLEARROWCAPMIDDLEINSET )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetAdjustableArrowCapMiddleInset(wa_par_GpAdjustableArrowCap(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetAdjustableArrowCapFillState(GpAdjustableArrowCap*,BOOL)
HB_FUNC( WAGDIPSETADJUSTABLEARROWCAPFILLSTATE )
{
  wa_ret_GpStatus(GdipSetAdjustableArrowCapFillState(wa_par_GpAdjustableArrowCap(1), wa_par_BOOL(2)));
}

// GpStatus WINGDIPAPI GdipGetAdjustableArrowCapFillState(GpAdjustableArrowCap*,BOOL*)
HB_FUNC( WAGDIPGETADJUSTABLEARROWCAPFILLSTATE )
{
  BOOL b{};
  wa_ret_GpStatus(GdipGetAdjustableArrowCapFillState(wa_par_GpAdjustableArrowCap(1), &b));
  wa_stor_BOOL(b, 2);
}

///////////////////////////////////////////////////////////////////////////////
// Bitmap functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateBitmapFromStream(IStream*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMSTREAM )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromStream(wa_par_IStream(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromFile(GDIPCONST WCHAR*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMFILE )
{
  GpBitmap * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateBitmapFromFile(HB_PARSTR(1, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromStreamICM(IStream*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMSTREAMICM )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromStreamICM(wa_par_IStream(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromFileICM(GDIPCONST WCHAR*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMFILEICM )
{
  GpBitmap * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateBitmapFromFileICM(HB_PARSTR(1, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromScan0(INT,INT,INT,PixelFormat,BYTE*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMSCAN0 )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromScan0(wa_par_INT(1), wa_par_INT(2), wa_par_INT(3), wa_par_GpPixelFormat(4), (BYTE*) hb_parptr(5), &p));
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromGraphics(INT,INT,GpGraphics*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMGRAPHICS )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromGraphics(wa_par_INT(1), wa_par_INT(2), wa_par_GpGraphics(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromDirectDrawSurface(IDirectDrawSurface7*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMDIRECTDRAWSURFACE )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromDirectDrawSurface((IDirectDrawSurface7*) hb_parptr(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromGdiDib(GDIPCONST BITMAPINFO*,VOID*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMGDIDIB )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromGdiDib((GDIPCONST BITMAPINFO*) hb_parptr(1), (VOID*) hb_parptr(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromHBITMAP(HBITMAP,HPALETTE,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMHBITMAP )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromHBITMAP(wa_par_HBITMAP(1), wa_par_HPALETTE(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateHBITMAPFromBitmap(GpBitmap*,HBITMAP*,ARGB)
HB_FUNC( WAGDIPCREATEHBITMAPFROMBITMAP )
{
  HBITMAP p{};
  wa_ret_GpStatus(GdipCreateHBITMAPFromBitmap(wa_par_GpBitmap(1), &p, wa_par_ARGB(3)));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromHICON(HICON,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMHICON )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCreateBitmapFromHICON(wa_par_HICON(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateHICONFromBitmap(GpBitmap*,HICON*)
HB_FUNC( WAGDIPCREATEHICONFROMBITMAP )
{
  HICON p{};
  wa_ret_GpStatus(GdipCreateHICONFromBitmap(wa_par_GpBitmap(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateBitmapFromResource(HINSTANCE,GDIPCONST WCHAR*,GpBitmap**)
HB_FUNC( WAGDIPCREATEBITMAPFROMRESOURCE )
{
  GpBitmap * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateBitmapFromResource(wa_par_HINSTANCE(1), HB_PARSTR(2, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCloneBitmapArea(REAL,REAL,REAL,REAL,PixelFormat,GpBitmap*,GpBitmap**)
HB_FUNC( WAGDIPCLONEBITMAPAREA )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCloneBitmapArea(wa_par_REAL(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_GpPixelFormat(5), wa_par_GpBitmap(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCloneBitmapAreaI(INT,INT,INT,INT,PixelFormat,GpBitmap*,GpBitmap**)
HB_FUNC( WAGDIPCLONEBITMAPAREAI )
{
  GpBitmap * p{};
  wa_ret_GpStatus(GdipCloneBitmapAreaI(wa_par_INT(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_GpPixelFormat(5), wa_par_GpBitmap(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipBitmapLockBits(GpBitmap*,GDIPCONST GpRect*,UINT,PixelFormat,BitmapData*)
HB_FUNC( WAGDIPBITMAPLOCKBITS )
{
  wa_ret_GpStatus(GdipBitmapLockBits(wa_par_GpBitmap(1), wa_par_GpRect(2), wa_par_UINT(3), wa_par_GpPixelFormat(4), (BitmapData*) hb_parptr(5)));
}

// GpStatus WINGDIPAPI GdipBitmapUnlockBits(GpBitmap*,BitmapData*)
HB_FUNC( WAGDIPBITMAPUNLOCKBITS )
{
  wa_ret_GpStatus(GdipBitmapUnlockBits(wa_par_GpBitmap(1), (BitmapData*) hb_parptr(2)));
}

// GpStatus WINGDIPAPI GdipBitmapGetPixel(GpBitmap*,INT,INT,ARGB*)
HB_FUNC( WAGDIPBITMAPGETPIXEL )
{
  ARGB a{};
  wa_ret_GpStatus(GdipBitmapGetPixel(wa_par_GpBitmap(1), wa_par_INT(2), wa_par_INT(3), &a));
  wa_stor_ARGB(a, 4);
}

// GpStatus WINGDIPAPI GdipBitmapSetPixel(GpBitmap*,INT,INT,ARGB)
HB_FUNC( WAGDIPBITMAPSETPIXEL )
{
  wa_ret_GpStatus(GdipBitmapSetPixel(wa_par_GpBitmap(1), wa_par_INT(2), wa_par_INT(3), wa_par_ARGB(4)));
}

// GpStatus WINGDIPAPI GdipBitmapSetResolution(GpBitmap*,REAL,REAL)
HB_FUNC( WAGDIPBITMAPSETRESOLUTION )
{
  wa_ret_GpStatus(GdipBitmapSetResolution(wa_par_GpBitmap(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipBitmapConvertFormat(GpBitmap*,PixelFormat,DitherType,PaletteType,ColorPalette*,REAL)
#if 0
HB_FUNC( WAGDIPBITMAPCONVERTFORMAT )
{
  wa_ret_GpStatus(GdipBitmapConvertFormat(wa_par_GpBitmap(1), wa_par_GpPixelFormat(2), wa_par_GpDitherType(3), wa_par_GpPaletteType(4), wa_par_GpColorPalette(5), wa_par_REAL(6)));
}
#endif

// GpStatus WINGDIPAPI GdipInitializePalette(ColorPalette*,PaletteType,INT,BOOL,GpBitmap*)
#if 0
HB_FUNC( WAGDIPINITIALIZEPALETTE )
{
  wa_ret_GpStatus(GdipInitializePalette(wa_par_GpColorPalette(1), wa_par_GpPaletteType(2), wa_par_INT(3), wa_par_BOOL(4), wa_par_GpBitmap(5)));
}
#endif

// GpStatus WINGDIPAPI GdipBitmapApplyEffect(GpBitmap*,CGpEffect*,RECT*,BOOL,VOID**,INT*)
#if 0
HB_FUNC( WAGDIPBITMAPAPPLYEFFECT )
{
  VOID * p{};
  INT i{};
  wa_ret_GpStatus(GdipBitmapApplyEffect(wa_par_GpBitmap(1), (CGpEffect*) hb_parptr(2), static_cast<RECT*>(wa_get_ptr(3)), wa_par_BOOL(4), &p, &i));
  hb_storptr(p, 5);
  hb_storni(i, 6);
}
#endif

// GpStatus WINGDIPAPI GdipBitmapCreateApplyEffect(GpBitmap**,INT,CGpEffect*,RECT*,RECT*,GpBitmap**,BOOL,VOID**,INT*)
#if 0
HB_FUNC( WAGDIPBITMAPCREATEAPPLYEFFECT )
{
  GpBitmap * b1 = (GpBitmap*) hb_parptr(1);
  GpBitmap * b2{};
  VOID * p{};
  INT i{};
  wa_ret_GpStatus(GdipBitmapCreateApplyEffect(&b1, wa_par_INT(2), (CGpEffect*) hb_parptr(3), static_cast<RECT*>(wa_get_ptr(4)), static_cast<RECT*>(wa_get_ptr(5)), &b2, wa_par_BOOL(7), &p, &i));
  hb_storptr(p, 8);
  hb_storni(i, 9);
}
#endif

// GpStatus WINGDIPAPI GdipBitmapGetHistogram(GpBitmap*,HistogramFormat,UINT,UINT*,UINT*,UINT*,UINT*)
#if 0
HB_FUNC( WAGDIPBITMAPGETHISTOGRAM ) // TODO:
{
  wa_ret_GpStatus(GdipBitmapGetHistogram(wa_par_GpBitmap(1), wa_par_GpHistogramFormat(2), wa_par_UINT(3), UINT*, UINT*, UINT*, UINT*));
}
#endif

// GpStatus WINGDIPAPI GdipBitmapGetHistogramSize(HistogramFormat,UINT*)
#if 0
HB_FUNC( WAGDIPBITMAPGETHISTOGRAMSIZE )
{
  UINT ui{};
  wa_ret_GpStatus(GdipBitmapGetHistogramSize(wa_par_GpHistogramFormat(1), &ui));
  wa_stor_UINT(ui, 2);
}
#endif

///////////////////////////////////////////////////////////////////////////////
// Brush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCloneBrush(GpBrush*,GpBrush**)
HB_FUNC( WAGDIPCLONEBRUSH )
{
  GpBrush * p{};
  wa_ret_GpStatus(GdipCloneBrush(wa_par_GpBrush(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeleteBrush(GpBrush*)
HB_FUNC( WAGDIPDELETEBRUSH )
{
  wa_ret_GpStatus(GdipDeleteBrush(wa_par_GpBrush(1)));
}

// GpStatus WINGDIPAPI GdipGetBrushType(GpBrush*,GpBrushType*)
HB_FUNC( WAGDIPGETBRUSHTYPE )
{
  GpBrushType bt{};
  wa_ret_GpStatus(GdipGetBrushType(wa_par_GpBrush(1), &bt));
  hb_storni(bt, 2);
}

///////////////////////////////////////////////////////////////////////////////
// CachedBitmap functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateCachedBitmap(GpBitmap*,GpGraphics*,GpCachedBitmap**)
HB_FUNC( WAGDIPCREATECACHEDBITMAP )
{
  GpCachedBitmap * p{};
  wa_ret_GpStatus(GdipCreateCachedBitmap(wa_par_GpBitmap(1), wa_par_GpGraphics(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipDeleteCachedBitmap(GpCachedBitmap*)
HB_FUNC( WAGDIPDELETECACHEDBITMAP )
{
  wa_ret_GpStatus(GdipDeleteCachedBitmap(wa_par_GpCachedBitmap(1)));
}

// GpStatus WINGDIPAPI GdipDrawCachedBitmap(GpGraphics*,GpCachedBitmap*,INT,INT)
HB_FUNC( WAGDIPDRAWCACHEDBITMAP )
{
  wa_ret_GpStatus(GdipDrawCachedBitmap(wa_par_GpGraphics(1), wa_par_GpCachedBitmap(2), wa_par_INT(3), wa_par_INT(4)));
}

///////////////////////////////////////////////////////////////////////////////
// CustomLineCap functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateCustomLineCap(GpPath*,GpPath*,GpLineCap,REAL,GpCustomLineCap**)
HB_FUNC( WAGDIPCREATECUSTOMLINECAP )
{
  GpCustomLineCap * p{};
  wa_ret_GpStatus(GdipCreateCustomLineCap(wa_par_GpPath(1), wa_par_GpPath(2), wa_par_GpLineCap(3), wa_par_REAL(4), &p));
  hb_storptr(p, 5);
}

// GpStatus WINGDIPAPI GdipDeleteCustomLineCap(GpCustomLineCap*)
HB_FUNC( WAGDIPDELETECUSTOMLINECAP )
{
  wa_ret_GpStatus(GdipDeleteCustomLineCap(wa_par_GpCustomLineCap(1)));
}

// GpStatus WINGDIPAPI GdipCloneCustomLineCap(GpCustomLineCap*,GpCustomLineCap**)
HB_FUNC( WAGDIPCLONECUSTOMLINECAP )
{
  GpCustomLineCap * p{};
  wa_ret_GpStatus(GdipCloneCustomLineCap(wa_par_GpCustomLineCap(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapType(GpCustomLineCap*,CustomLineCapType*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPTYPE )
{
  CustomLineCapType clct{};
  wa_ret_GpStatus(GdipGetCustomLineCapType(wa_par_GpCustomLineCap(1), &clct));
  hb_storni(clct, 2);
}

// GpStatus WINGDIPAPI GdipSetCustomLineCapStrokeCaps(GpCustomLineCap*,GpLineCap,GpLineCap)
HB_FUNC( WAGDIPSETCUSTOMLINECAPSTROKECAPS )
{
  wa_ret_GpStatus(GdipSetCustomLineCapStrokeCaps(wa_par_GpCustomLineCap(1), wa_par_GpLineCap(2), wa_par_GpLineCap(3)));
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapStrokeCaps(GpCustomLineCap*,GpLineCap*,GpLineCap*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPSTROKECAPS )
{
  GpLineCap v1{};
  GpLineCap v2{};
  wa_ret_GpStatus(GdipGetCustomLineCapStrokeCaps(wa_par_GpCustomLineCap(1), &v1, &v2));
  hb_storni(v1, 2);
  hb_storni(v2, 3);
}

// GpStatus WINGDIPAPI GdipSetCustomLineCapStrokeJoin(GpCustomLineCap*,GpLineJoin)
HB_FUNC( WAGDIPSETCUSTOMLINECAPSTROKEJOIN )
{
  wa_ret_GpStatus(GdipSetCustomLineCapStrokeJoin(wa_par_GpCustomLineCap(1), wa_par_GpLineJoin(2)));
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapStrokeJoin(GpCustomLineCap*,GpLineJoin*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPSTROKEJOIN )
{
  GpLineJoin v{};
  wa_ret_GpStatus(GdipGetCustomLineCapStrokeJoin(wa_par_GpCustomLineCap(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetCustomLineCapBaseCap(GpCustomLineCap*,GpLineCap)
HB_FUNC( WAGDIPSETCUSTOMLINECAPBASECAP )
{
  wa_ret_GpStatus(GdipSetCustomLineCapBaseCap(wa_par_GpCustomLineCap(1), wa_par_GpLineCap(2)));
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapBaseCap(GpCustomLineCap*,GpLineCap*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPBASECAP )
{
  GpLineCap v{};
  wa_ret_GpStatus(GdipGetCustomLineCapBaseCap(wa_par_GpCustomLineCap(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetCustomLineCapBaseInset(GpCustomLineCap*,REAL)
HB_FUNC( WAGDIPSETCUSTOMLINECAPBASEINSET )
{
  wa_ret_GpStatus(GdipSetCustomLineCapBaseInset(wa_par_GpCustomLineCap(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapBaseInset(GpCustomLineCap*,REAL*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPBASEINSET )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetCustomLineCapBaseInset(wa_par_GpCustomLineCap(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetCustomLineCapWidthScale(GpCustomLineCap*,REAL)
HB_FUNC( WAGDIPSETCUSTOMLINECAPWIDTHSCALE )
{
  wa_ret_GpStatus(GdipSetCustomLineCapWidthScale(wa_par_GpCustomLineCap(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetCustomLineCapWidthScale(GpCustomLineCap*,REAL*)
HB_FUNC( WAGDIPGETCUSTOMLINECAPWIDTHSCALE )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetCustomLineCapWidthScale(wa_par_GpCustomLineCap(1), &r));
  wa_stor_REAL(r, 2);
}

///////////////////////////////////////////////////////////////////////////////
// Effect functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateEffect(GDIPCONST GUID,CGpEffect**)
#if 0
HB_FUNC( WAGDIPCREATEEFFECT ) // TODO:
{
  CGpEffect * p{};
  wa_ret_GpStatus(GdipCreateEffect(GDIPCONST GUID, &p));
  hb_storptr(p, 2);
}
#endif

// GpStatus WINGDIPAPI GdipDeleteEffect(CGpEffect*)
#if 0
HB_FUNC( WAGDIPDELETEEFFECT )
{
  wa_ret_GpStatus(GdipDeleteEffect((CGpEffect*) hb_parptr(1)));
}
#endif

// GpStatus WINGDIPAPI GdipGetEffectParameterSize(CGpEffect*,UINT*)
#if 0
HB_FUNC( WAGDIPGETEFFECTPARAMETERSIZE )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetEffectParameterSize((CGpEffect*) hb_parptr(1), &ui));
  wa_stor_UINT(ui, 2);
}
#endif

// GpStatus WINGDIPAPI GdipSetEffectParameters(CGpEffect*,GDIPCONST VOID*,UINT)
#if 0
HB_FUNC( WAGDIPSETEFFECTPARAMETERS )
{
  wa_ret_GpStatus(GdipSetEffectParameters((CGpEffect*) hb_parptr(1), (GDIPCONST VOID*) hb_parptr(2), wa_par_UINT(3)));
}
#endif

// GpStatus WINGDIPAPI GdipGetEffectParameters(CGpEffect*,UINT*,VOID*)
#if 0
HB_FUNC( WAGDIPGETEFFECTPARAMETERS ) // TODO:
{
  wa_ret_GpStatus(GdipGetEffectParameters(CGpEffect*,UINT*,VOID*));
}
#endif

///////////////////////////////////////////////////////////////////////////////
// Font functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateFontFromDC(HDC,GpFont**)
HB_FUNC( WAGDIPCREATEFONTFROMDC )
{
  GpFont * p{};
  wa_ret_GpStatus(GdipCreateFontFromDC(wa_par_HDC(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateFontFromLogfontA(HDC,GDIPCONST LOGFONTA*,GpFont**)
HB_FUNC( WAGDIPCREATEFONTFROMLOGFONTA )
{
  GpFont * p{};
  wa_ret_GpStatus(GdipCreateFontFromLogfontA(wa_par_HDC(1), static_cast<GDIPCONST LOGFONTA*>(wa_get_ptr(2)), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateFontFromLogfontW(HDC,GDIPCONST LOGFONTW*,GpFont**)
HB_FUNC( WAGDIPCREATEFONTFROMLOGFONTW )
{
  GpFont * p{};
  wa_ret_GpStatus(GdipCreateFontFromLogfontW(wa_par_HDC(1), static_cast<GDIPCONST LOGFONTW*>(wa_get_ptr(2)), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateFont(GDIPCONST GpFontFamily*,REAL,INT,Unit,GpFont**)
HB_FUNC( WAGDIPCREATEFONT )
{
  GpFont * p{};
  wa_ret_GpStatus(GdipCreateFont((GDIPCONST GpFontFamily*) hb_parptr(1), wa_par_REAL(2), wa_par_INT(3), wa_par_GpUnit(4), &p));
  hb_storptr(p, 5);
}

// GpStatus WINGDIPAPI GdipCloneFont(GpFont*,GpFont**)
HB_FUNC( WAGDIPCLONEFONT )
{
  GpFont * p{};
  wa_ret_GpStatus(GdipCloneFont(wa_par_GpFont(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeleteFont(GpFont*)
HB_FUNC( WAGDIPDELETEFONT )
{
  wa_ret_GpStatus(GdipDeleteFont(wa_par_GpFont(1)));
}

// GpStatus WINGDIPAPI GdipGetFamily(GpFont*,GpFontFamily**)
HB_FUNC( WAGDIPGETFAMILY )
{
  GpFontFamily * p{};
  wa_ret_GpStatus(GdipGetFamily(wa_par_GpFont(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetFontStyle(GpFont*,INT*)
HB_FUNC( WAGDIPGETFONTSTYLE )
{
  INT i{};
  wa_ret_GpStatus(GdipGetFontStyle(wa_par_GpFont(1), &i));
  hb_storni(i, 2);
}

// GpStatus WINGDIPAPI GdipGetFontSize(GpFont*,REAL*)
HB_FUNC( WAGDIPGETFONTSIZE )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetFontSize(wa_par_GpFont(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipGetFontUnit(GpFont*,Unit*)
HB_FUNC( WAGDIPGETFONTUNIT )
{
  GpUnit u{};
  wa_ret_GpStatus(GdipGetFontUnit(wa_par_GpFont(1), &u));
  hb_storni(u, 2);
}

// GpStatus WINGDIPAPI GdipGetFontHeight(GDIPCONST GpFont*,GDIPCONST GpGraphics*,REAL*)
HB_FUNC( WAGDIPGETFONTHEIGHT )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetFontHeight(wa_par_GpFont(1), wa_par_GpGraphics(2), &r));
  wa_stor_REAL(r, 3);
}

// GpStatus WINGDIPAPI GdipGetFontHeightGivenDPI(GDIPCONST GpFont*,REAL,REAL*)
HB_FUNC( WAGDIPGETFONTHEIGHTGIVENDPI )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetFontHeightGivenDPI(wa_par_GpFont(1), wa_par_REAL(2), &r));
  wa_stor_REAL(r, 3);
}

// GpStatus WINGDIPAPI GdipGetLogFontA(GpFont*,GpGraphics*,LOGFONTA*)
HB_FUNC( WAGDIPGETLOGFONTA )
{
  wa_ret_GpStatus(GdipGetLogFontA(wa_par_GpFont(1), wa_par_GpGraphics(2), static_cast<LOGFONTA*>(wa_get_ptr(3))));
}

// GpStatus WINGDIPAPI GdipGetLogFontW(GpFont*,GpGraphics*,LOGFONTW*)
HB_FUNC( WAGDIPGETLOGFONTW )
{
  wa_ret_GpStatus(GdipGetLogFontW(wa_par_GpFont(1), wa_par_GpGraphics(2), static_cast<LOGFONTW*>(wa_get_ptr(3))));
}

// GpStatus WINGDIPAPI GdipNewInstalledFontCollection(GpFontCollection**)
HB_FUNC( WAGDIPNEWINSTALLEDFONTCOLLECTION )
{
  GpFontCollection * p{};
  wa_ret_GpStatus(GdipNewInstalledFontCollection(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipNewPrivateFontCollection(GpFontCollection**)
HB_FUNC( WAGDIPNEWPRIVATEFONTCOLLECTION )
{
  GpFontCollection * p{};
  wa_ret_GpStatus(GdipNewPrivateFontCollection(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipDeletePrivateFontCollection(GpFontCollection**)
HB_FUNC( WAGDIPDELETEPRIVATEFONTCOLLECTION )
{
  GpFontCollection * p = wa_par_GpFontCollection(1);
  wa_ret_GpStatus(GdipDeletePrivateFontCollection(&p));
}

// GpStatus WINGDIPAPI GdipGetFontCollectionFamilyCount(GpFontCollection*,INT*)
HB_FUNC( WAGDIPGETFONTCOLLECTIONFAMILYCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetFontCollectionFamilyCount(wa_par_GpFontCollection(1), &i));
  hb_storni(i, 2);
}

// GpStatus WINGDIPAPI GdipGetFontCollectionFamilyList(GpFontCollection*,INT,GpFontFamily**,INT*)
#if 0
HB_FUNC( WAGDIPGETFONTCOLLECTIONFAMILYLIST ) // TODO:
{
  wa_ret_GpStatus(GdipGetFontCollectionFamilyList(wa_par_GpFontCollection(1), wa_par_INT(2), GpFontFamily**, INT*));
}
#endif

// GpStatus WINGDIPAPI GdipPrivateAddFontFile(GpFontCollection*,GDIPCONST WCHAR*)
HB_FUNC( WAGDIPPRIVATEADDFONTFILE )
{
  void * str{};
  wa_ret_GpStatus(GdipPrivateAddFontFile(wa_par_GpFontCollection(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
}

// GpStatus WINGDIPAPI GdipPrivateAddMemoryFont(GpFontCollection*,GDIPCONST void*,INT)
#if 0
HB_FUNC( WAGDIPPRIVATEADDMEMORYFONT ) // TODO:
{
  wa_ret_GpStatus(GdipPrivateAddMemoryFont(wa_par_GpFontCollection(1), GDIPCONST void*, wa_par_INT(3)));
}
#endif

///////////////////////////////////////////////////////////////////////////////
// FontFamily functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateFontFamilyFromName(GDIPCONST WCHAR*,GpFontCollection*,GpFontFamily**)
HB_FUNC( WAGDIPCREATEFONTFAMILYFROMNAME )
{
  GpFontFamily * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateFontFamilyFromName(HB_PARSTR(1, &str, nullptr), wa_par_GpFontCollection(2), &p));
  hb_strfree(str);
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipDeleteFontFamily(GpFontFamily*)
HB_FUNC( WAGDIPDELETEFONTFAMILY )
{
  wa_ret_GpStatus(GdipDeleteFontFamily(wa_par_GpFontFamily(1)));
}

// GpStatus WINGDIPAPI GdipCloneFontFamily(GpFontFamily*,GpFontFamily**)
HB_FUNC( WAGDIPCLONEFONTFAMILY )
{
  GpFontFamily * p{};
  wa_ret_GpStatus(GdipCloneFontFamily(wa_par_GpFontFamily(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetGenericFontFamilySansSerif(GpFontFamily**)
HB_FUNC( WAGDIPGETGENERICFONTFAMILYSANSSERIF )
{
  GpFontFamily * p{};
  wa_ret_GpStatus(GdipGetGenericFontFamilySansSerif(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipGetGenericFontFamilySerif(GpFontFamily**)
HB_FUNC( WAGDIPGETGENERICFONTFAMILYSERIF )
{
  GpFontFamily * p{};
  wa_ret_GpStatus(GdipGetGenericFontFamilySerif(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipGetGenericFontFamilyMonospace(GpFontFamily**)
HB_FUNC( WAGDIPGETGENERICFONTFAMILYMONOSPACE )
{
  GpFontFamily * p{};
  wa_ret_GpStatus(GdipGetGenericFontFamilyMonospace(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipGetFamilyName(GDIPCONST GpFontFamily*,WCHAR[LF_FACESIZE],LANGID)

// GpStatus WINGDIPAPI GdipIsStyleAvailable(GDIPCONST GpFontFamily*,INT,BOOL*)
HB_FUNC( WAGDIPISSTYLEAVAILABLE )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsStyleAvailable(wa_par_GpFontFamily(1), wa_par_INT(2), &b));
  wa_stor_BOOL(b, 3);
}

// GpStatus WINGDIPAPI GdipFontCollectionEnumerable(GpFontCollection*,GpGraphics*,INT*)
#if 0
HB_FUNC( WAGDIPFONTCOLLECTIONENUMERABLE )
{
  INT i{};
  wa_ret_GpStatus(GdipFontCollectionEnumerable(wa_par_GpFontCollection(1), wa_par_GpGraphics(2), &i));
  wa_stor_INT(i, 3);
}
#endif

// GpStatus WINGDIPAPI GdipFontCollectionEnumerate(GpFontCollection*,INT,GpFontFamily**,INT*,GpGraphics*)

// GpStatus WINGDIPAPI GdipGetEmHeight(GDIPCONST GpFontFamily*,INT,UINT16*)
HB_FUNC( WAGDIPGETEMHEIGHT )
{
  UINT16 ui{};
  wa_ret_GpStatus(GdipGetEmHeight(wa_par_GpFontFamily(1), wa_par_INT(2), &ui));
  wa_stor_UINT16(ui, 3);
}

// GpStatus WINGDIPAPI GdipGetCellAscent(GDIPCONST GpFontFamily*,INT,UINT16*)
HB_FUNC( WAGDIPGETCELLASCENT )
{
  UINT16 ui{};
  wa_ret_GpStatus(GdipGetCellAscent(wa_par_GpFontFamily(1), wa_par_INT(2), &ui));
  wa_stor_UINT16(ui, 3);
}

// GpStatus WINGDIPAPI GdipGetCellDescent(GDIPCONST GpFontFamily*,INT,UINT16*)
HB_FUNC( WAGDIPGETCELLDESCENT )
{
  UINT16 ui{};
  wa_ret_GpStatus(GdipGetCellDescent(wa_par_GpFontFamily(1), wa_par_INT(2), &ui));
  wa_stor_UINT16(ui, 3);
}

// GpStatus WINGDIPAPI GdipGetLineSpacing(GDIPCONST GpFontFamily*,INT,UINT16*)
HB_FUNC( WAGDIPGETLINESPACING )
{
  UINT16 ui{};
  wa_ret_GpStatus(GdipGetLineSpacing(wa_par_GpFontFamily(1), wa_par_INT(2), &ui));
  wa_stor_UINT16(ui, 3);
}

///////////////////////////////////////////////////////////////////////////////
// Graphics functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipFlush(GpGraphics*,GpFlushIntention)
HB_FUNC( WAGDIPFLUSH )
{
  wa_ret_GpStatus(GdipFlush(wa_par_GpGraphics(1), wa_par_GpFlushIntention(2)));
}

// GpStatus WINGDIPAPI GdipCreateFromHDC(HDC,GpGraphics**)
HB_FUNC( WAGDIPCREATEFROMHDC )
{
  GpGraphics * p{};
  wa_ret_GpStatus(GdipCreateFromHDC(wa_par_HDC(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateFromHDC2(HDC,HANDLE,GpGraphics**)
HB_FUNC( WAGDIPCREATEFROMHDC2 )
{
  GpGraphics * p{};
  wa_ret_GpStatus(GdipCreateFromHDC2(wa_par_HDC(1), wa_par_HANDLE(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateFromHWND(HWND,GpGraphics**)
HB_FUNC( WAGDIPCREATEFROMHWND )
{
  GpGraphics * p{};
  wa_ret_GpStatus(GdipCreateFromHWND(wa_par_HWND(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateFromHWNDICM(HWND,GpGraphics**)
HB_FUNC( WAGDIPCREATEFROMHWNDICM )
{
  GpGraphics * p{};
  wa_ret_GpStatus(GdipCreateFromHWNDICM(wa_par_HWND(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeleteGraphics(GpGraphics*)
HB_FUNC( WAGDIPDELETEGRAPHICS )
{
  wa_ret_GpStatus(GdipDeleteGraphics(wa_par_GpGraphics(1)));
}

// GpStatus WINGDIPAPI GdipGetDC(GpGraphics*,HDC*)
HB_FUNC( WAGDIPGETDC )
{
  HDC dc{};
  wa_ret_GpStatus(GdipGetDC(wa_par_GpGraphics(1), &dc));
  hb_storptr(dc, 2);
}

// GpStatus WINGDIPAPI GdipReleaseDC(GpGraphics*,HDC)
HB_FUNC( WAGDIPRELEASEDC )
{
  wa_ret_GpStatus(GdipReleaseDC(wa_par_GpGraphics(1), wa_par_HDC(2)));
}

// GpStatus WINGDIPAPI GdipSetCompositingMode(GpGraphics*,CompositingMode)
HB_FUNC( WAGDIPSETCOMPOSITINGMODE )
{
  wa_ret_GpStatus(GdipSetCompositingMode(wa_par_GpGraphics(1), wa_par_GpCompositingMode(2)));
}

// GpStatus WINGDIPAPI GdipGetCompositingMode(GpGraphics*,CompositingMode*)
HB_FUNC( WAGDIPGETCOMPOSITINGMODE )
{
  CompositingMode cm{};
  wa_ret_GpStatus(GdipGetCompositingMode(wa_par_GpGraphics(1), &cm));
  hb_storni(cm, 2);
}

// GpStatus WINGDIPAPI GdipSetRenderingOrigin(GpGraphics*,INT,INT)
HB_FUNC( WAGDIPSETRENDERINGORIGIN )
{
  wa_ret_GpStatus(GdipSetRenderingOrigin(wa_par_GpGraphics(1), wa_par_INT(2), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipGetRenderingOrigin(GpGraphics*,INT*,INT*)
HB_FUNC( WAGDIPGETRENDERINGORIGIN )
{
  INT v1{};
  INT v2{};
  wa_ret_GpStatus(GdipGetRenderingOrigin(wa_par_GpGraphics(1), &v1, &v2));
  hb_storni(v1, 2);
  hb_storni(v2, 3);
}

// GpStatus WINGDIPAPI GdipSetCompositingQuality(GpGraphics*,CompositingQuality)
HB_FUNC( WAGDIPSETCOMPOSITINGQUALITY )
{
  wa_ret_GpStatus(GdipSetCompositingQuality(wa_par_GpGraphics(1), wa_par_GpCompositingQuality(2)));
}

// GpStatus WINGDIPAPI GdipGetCompositingQuality(GpGraphics*,CompositingQuality*)
HB_FUNC( WAGDIPGETCOMPOSITINGQUALITY )
{
  CompositingQuality cq{};
  wa_ret_GpStatus(GdipGetCompositingQuality(wa_par_GpGraphics(1), &cq));
  hb_storni(cq, 2);
}

// GpStatus WINGDIPAPI GdipSetSmoothingMode(GpGraphics*,SmoothingMode)
HB_FUNC( WAGDIPSETSMOOTHINGMODE )
{
  wa_ret_GpStatus(GdipSetSmoothingMode(wa_par_GpGraphics(1), wa_par_GpSmoothingMode(2)));
}

// GpStatus WINGDIPAPI GdipGetSmoothingMode(GpGraphics*,SmoothingMode*)
HB_FUNC( WAGDIPGETSMOOTHINGMODE )
{
  SmoothingMode v{};
  wa_ret_GpStatus(GdipGetSmoothingMode(wa_par_GpGraphics(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetPixelOffsetMode(GpGraphics*,PixelOffsetMode)
HB_FUNC( WAGDIPSETPIXELOFFSETMODE )
{
  wa_ret_GpStatus(GdipSetPixelOffsetMode(wa_par_GpGraphics(1), wa_par_GpPixelOffsetMode(2)));
}

// GpStatus WINGDIPAPI GdipGetPixelOffsetMode(GpGraphics*,PixelOffsetMode*)
HB_FUNC( WAGDIPGETPIXELOFFSETMODE )
{
  PixelOffsetMode v{};
  wa_ret_GpStatus(GdipGetPixelOffsetMode(wa_par_GpGraphics(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetTextRenderingHint(GpGraphics*,TextRenderingHint)
HB_FUNC( WAGDIPSETTEXTRENDERINGHINT )
{
  wa_ret_GpStatus(GdipSetTextRenderingHint(wa_par_GpGraphics(1), wa_par_GpTextRenderingHint(2)));
}

// GpStatus WINGDIPAPI GdipGetTextRenderingHint(GpGraphics*,TextRenderingHint*)
HB_FUNC( WAGDIPGETTEXTRENDERINGHINT )
{
  TextRenderingHint v{};
  wa_ret_GpStatus(GdipGetTextRenderingHint(wa_par_GpGraphics(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetTextContrast(GpGraphics*,UINT)
HB_FUNC( WAGDIPSETTEXTCONTRAST )
{
  wa_ret_GpStatus(GdipSetTextContrast(wa_par_GpGraphics(1), wa_par_UINT(2)));
}

// GpStatus WINGDIPAPI GdipGetTextContrast(GpGraphics*,UINT*)
HB_FUNC( WAGDIPGETTEXTCONTRAST )
{
  UINT v{};
  wa_ret_GpStatus(GdipGetTextContrast(wa_par_GpGraphics(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipSetInterpolationMode(GpGraphics*,InterpolationMode)
HB_FUNC( WAGDIPSETINTERPOLATIONMODE )
{
  wa_ret_GpStatus(GdipSetInterpolationMode(wa_par_GpGraphics(1), wa_par_GpInterpolationMode(2)));
}

// GpStatus WINGDIPAPI GdipGetInterpolationMode(GpGraphics*,InterpolationMode*)
HB_FUNC( WAGDIPGETINTERPOLATIONMODE )
{
  InterpolationMode v{};
  wa_ret_GpStatus(GdipGetInterpolationMode(wa_par_GpGraphics(1), &v));
  hb_storni(v, 2);
}

// GpStatus WINGDIPAPI GdipGraphicsSetAbort(GpGraphics*,GdiplusAbort*)
#if 0
HB_FUNC( WAGDIPGRAPHICSSETABORT )
{
  wa_ret_GpStatus(GdipGraphicsSetAbort(wa_par_GpGraphics(1), (GdiplusAbort*) hb_parptr(2)));
}
#endif

// GpStatus WINGDIPAPI GdipSetWorldTransform(GpGraphics*,GpMatrix*)
HB_FUNC( WAGDIPSETWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipSetWorldTransform(wa_par_GpGraphics(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetWorldTransform(GpGraphics*)
HB_FUNC( WAGDIPRESETWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipResetWorldTransform(wa_par_GpGraphics(1)));
}

// GpStatus WINGDIPAPI GdipMultiplyWorldTransform(GpGraphics*,GDIPCONST GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipMultiplyWorldTransform(wa_par_GpGraphics(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslateWorldTransform(GpGraphics*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATEWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipTranslateWorldTransform(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScaleWorldTransform(GpGraphics*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALEWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipScaleWorldTransform(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotateWorldTransform(GpGraphics*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATEWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipRotateWorldTransform(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipGetWorldTransform(GpGraphics*,GpMatrix*)
HB_FUNC( WAGDIPGETWORLDTRANSFORM )
{
  wa_ret_GpStatus(GdipGetWorldTransform(wa_par_GpGraphics(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetPageTransform(GpGraphics*)
HB_FUNC( WAGDIPRESETPAGETRANSFORM )
{
  wa_ret_GpStatus(GdipResetPageTransform(wa_par_GpGraphics(1)));
}

// GpStatus WINGDIPAPI GdipGetPageUnit(GpGraphics*,GpUnit*)
HB_FUNC( WAGDIPGETPAGEUNIT )
{
  GpUnit u{};
  wa_ret_GpStatus(GdipGetPageUnit(wa_par_GpGraphics(1), &u));
  hb_storni(u, 2);
}

// GpStatus WINGDIPAPI GdipGetPageScale(GpGraphics*,REAL*)
HB_FUNC( WAGDIPGETPAGESCALE )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetPageScale(wa_par_GpGraphics(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetPageUnit(GpGraphics*,GpUnit)
HB_FUNC( WAGDIPSETPAGEUNIT )
{
  wa_ret_GpStatus(GdipSetPageUnit(wa_par_GpGraphics(1), wa_par_GpUnit(2)));
}

// GpStatus WINGDIPAPI GdipSetPageScale(GpGraphics*,REAL)
HB_FUNC( WAGDIPSETPAGESCALE )
{
  wa_ret_GpStatus(GdipSetPageScale(wa_par_GpGraphics(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetDpiX(GpGraphics*,REAL*)
HB_FUNC( WAGDIPGETDPIX )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetDpiX(wa_par_GpGraphics(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipGetDpiY(GpGraphics*,REAL*)
HB_FUNC( WAGDIPGETDPIY )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetDpiY(wa_par_GpGraphics(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipTransformPoints(GpGraphics*,GpCoordinateSpace,GpCoordinateSpace,GpPointF*,INT)
#if 0
HB_FUNC( WAGDIPTRANSFORMPOINTS )
{
  wa_ret_GpStatus(GdipTransformPoints(wa_par_GpGraphics(1), wa_par_GpCoordinateSpace(2), wa_par_GpCoordinateSpace(3), wa_par_GpPointF(4), wa_par_INT(5)));
}
#endif

// GpStatus WINGDIPAPI GdipTransformPointsI(GpGraphics*,GpCoordinateSpace,GpCoordinateSpace,GpPoint*,INT)
#if 0
HB_FUNC( WAGDIPTRANSFORMPOINTSI )
{
  wa_ret_GpStatus(GdipTransformPointsI(wa_par_GpGraphics(1), wa_par_GpCoordinateSpace(2), wa_par_GpCoordinateSpace(3), wa_par_GpPoint(4), wa_par_INT(5)));
}
#endif

// GpStatus WINGDIPAPI GdipGetNearestColor(GpGraphics*,ARGB*)
HB_FUNC( WAGDIPGETNEARESTCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetNearestColor(wa_par_GpGraphics(1), &argb));
  wa_stor_ARGB(argb, 2);
}

// HPALETTE WINGDIPAPI GdipCreateHalftonePalette(void)
HB_FUNC( WAGDIPCREATEHALFTONEPALETTE )
{
  wa_ret_HPALETTE(GdipCreateHalftonePalette());
}

// GpStatus WINGDIPAPI GdipDrawLine(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWLINE )
{
  wa_ret_GpStatus(GdipDrawLine(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipDrawLineI(GpGraphics*,GpPen*,INT,INT,INT,INT)
HB_FUNC( WAGDIPDRAWLINEI )
{
  wa_ret_GpStatus(GdipDrawLineI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipDrawLines(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWLINES )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawLines(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawLinesI(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWLINESI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawLinesI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawArc(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWARC )
{
  wa_ret_GpStatus(GdipDrawArc(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipDrawArcI(GpGraphics*,GpPen*,INT,INT,INT,INT,REAL,REAL)
HB_FUNC( WAGDIPDRAWARCI )
{
  wa_ret_GpStatus(GdipDrawArcI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipDrawBezier(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWBEZIER )
{
  wa_ret_GpStatus(GdipDrawBezier(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_REAL(9), wa_par_REAL(10)));
}

// GpStatus WINGDIPAPI GdipDrawBezierI(GpGraphics*,GpPen*,INT,INT,INT,INT,INT,INT,INT,INT)
HB_FUNC( WAGDIPDRAWBEZIERI )
{
  wa_ret_GpStatus(GdipDrawBezierI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_INT(7), wa_par_INT(8), wa_par_INT(9), wa_par_INT(10)));
}

// GpStatus WINGDIPAPI GdipDrawBeziers(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWBEZIERS )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawBeziers(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawBeziersI(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWBEZIERSI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawBeziersI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawRectangle(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWRECTANGLE )
{
  wa_ret_GpStatus(GdipDrawRectangle(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipDrawRectangleI(GpGraphics*,GpPen*,INT,INT,INT,INT)
HB_FUNC( WAGDIPDRAWRECTANGLEI )
{
  wa_ret_GpStatus(GdipDrawRectangleI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipDrawRectangles(GpGraphics*,GpPen*,GDIPCONST GpRectF*,INT)
HB_FUNC( WAGDIPDRAWRECTANGLES )
{
  std::vector<GpRectF> vec = GpRectFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawRectangles(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawRectanglesI(GpGraphics*,GpPen*,GDIPCONST GpRect*,INT)
HB_FUNC( WAGDIPDRAWRECTANGLESI )
{
  std::vector<GpRect> vec = GpRectArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawRectanglesI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawEllipse(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWELLIPSE )
{
  wa_ret_GpStatus(GdipDrawEllipse(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipDrawEllipseI(GpGraphics*,GpPen*,INT,INT,INT,INT)
HB_FUNC( WAGDIPDRAWELLIPSEI )
{
  wa_ret_GpStatus(GdipDrawEllipseI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipDrawPie(GpGraphics*,GpPen*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWPIE )
{
  wa_ret_GpStatus(GdipDrawPie(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipDrawPieI(GpGraphics*,GpPen*,INT,INT,INT,INT,REAL,REAL)
HB_FUNC( WAGDIPDRAWPIEI )
{
  wa_ret_GpStatus(GdipDrawPieI(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipDrawPolygon(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWPOLYGON )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawPolygon(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawPolygonI(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWPOLYGONI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawPolygonI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawPath(GpGraphics*,GpPen*,GpPath*)
HB_FUNC( WAGDIPDRAWPATH )
{
  wa_ret_GpStatus(GdipDrawPath(wa_par_GpGraphics(1), wa_par_GpPen(2), wa_par_GpPath(3)));
}

// GpStatus WINGDIPAPI GdipDrawCurve(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWCURVE )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurve(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawCurveI(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWCURVEI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurveI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawCurve2(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT,REAL)
HB_FUNC( WAGDIPDRAWCURVE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurve2(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipDrawCurve2I(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT,REAL)
HB_FUNC( WAGDIPDRAWCURVE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurve2I(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipDrawCurve3(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT,INT,INT,REAL)
HB_FUNC( WAGDIPDRAWCURVE3 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurve3(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipDrawCurve3I(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT,INT,INT,REAL)
HB_FUNC( WAGDIPDRAWCURVE3I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawCurve3I(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipDrawClosedCurve(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWCLOSEDCURVE )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawClosedCurve(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawClosedCurveI(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWCLOSEDCURVEI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawClosedCurveI(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawClosedCurve2(GpGraphics*,GpPen*,GDIPCONST GpPointF*,INT,REAL)
HB_FUNC( WAGDIPDRAWCLOSEDCURVE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawClosedCurve2(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipDrawClosedCurve2I(GpGraphics*,GpPen*,GDIPCONST GpPoint*,INT,REAL)
HB_FUNC( WAGDIPDRAWCLOSEDCURVE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawClosedCurve2I(wa_par_GpGraphics(1), wa_par_GpPen(2), vec.data(), wa_par_INT(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipGraphicsClear(GpGraphics*,ARGB)
HB_FUNC( WAGDIPGRAPHICSCLEAR )
{
  wa_ret_GpStatus(GdipGraphicsClear(wa_par_GpGraphics(1), wa_par_ARGB(2)));
}

// GpStatus WINGDIPAPI GdipFillRectangle(GpGraphics*,GpBrush*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPFILLRECTANGLE )
{
  wa_ret_GpStatus(GdipFillRectangle(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipFillRectangleI(GpGraphics*,GpBrush*,INT,INT,INT,INT)
HB_FUNC( WAGDIPFILLRECTANGLEI )
{
  wa_ret_GpStatus(GdipFillRectangleI(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipFillRectangles(GpGraphics*,GpBrush*,GDIPCONST GpRectF*,INT)
HB_FUNC( WAGDIPFILLRECTANGLES )
{
  std::vector<GpRectF> vec = GpRectFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillRectangles(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillRectanglesI(GpGraphics*,GpBrush*,GDIPCONST GpRect*,INT)
HB_FUNC( WAGDIPFILLRECTANGLESI )
{
  std::vector<GpRect> vec = GpRectArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillRectanglesI(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillPolygon(GpGraphics*,GpBrush*,GDIPCONST GpPointF*,INT,GpFillMode)
HB_FUNC( WAGDIPFILLPOLYGON )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillPolygon(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4), wa_par_GpFillMode(5)));
}

// GpStatus WINGDIPAPI GdipFillPolygonI(GpGraphics*,GpBrush*,GDIPCONST GpPoint*,INT,GpFillMode)
HB_FUNC( WAGDIPFILLPOLYGONI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillPolygonI(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4), wa_par_GpFillMode(5)));
}

// GpStatus WINGDIPAPI GdipFillPolygon2(GpGraphics*,GpBrush*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPFILLPOLYGON2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillPolygon2(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillPolygon2I(GpGraphics*,GpBrush*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPFILLPOLYGON2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillPolygon2I(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillEllipse(GpGraphics*,GpBrush*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPFILLELLIPSE )
{
  wa_ret_GpStatus(GdipFillEllipse(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipFillEllipseI(GpGraphics*,GpBrush*,INT,INT,INT,INT)
HB_FUNC( WAGDIPFILLELLIPSEI )
{
  wa_ret_GpStatus(GdipFillEllipseI(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipFillPie(GpGraphics*,GpBrush*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPFILLPIE )
{
  wa_ret_GpStatus(GdipFillPie(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipFillPieI(GpGraphics*,GpBrush*,INT,INT,INT,INT,REAL,REAL)
HB_FUNC( WAGDIPFILLPIEI )
{
  wa_ret_GpStatus(GdipFillPieI(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_REAL(7), wa_par_REAL(8)));
}

// GpStatus WINGDIPAPI GdipFillPath(GpGraphics*,GpBrush*,GpPath*)
HB_FUNC( WAGDIPFILLPATH )
{
  wa_ret_GpStatus(GdipFillPath(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_GpPath(3)));
}

// GpStatus WINGDIPAPI GdipFillClosedCurve(GpGraphics*,GpBrush*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPFILLCLOSEDCURVE )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillClosedCurve(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillClosedCurveI(GpGraphics*,GpBrush*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPFILLCLOSEDCURVEI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillClosedCurveI(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipFillClosedCurve2(GpGraphics*,GpBrush*,GDIPCONST GpPointF*,INT,REAL,GpFillMode)
HB_FUNC( WAGDIPFILLCLOSEDCURVE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillClosedCurve2(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4), wa_par_REAL(5), wa_par_GpFillMode(6)));
}

// GpStatus WINGDIPAPI GdipFillClosedCurve2I(GpGraphics*,GpBrush*,GDIPCONST GpPoint*,INT,REAL,GpFillMode)
HB_FUNC( WAGDIPFILLCLOSEDCURVE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipFillClosedCurve2I(wa_par_GpGraphics(1), wa_par_GpBrush(2), vec.data(), wa_par_INT(4), wa_par_REAL(5), wa_par_GpFillMode(6)));
}

// GpStatus WINGDIPAPI GdipFillRegion(GpGraphics*,GpBrush*,GpRegion*)
HB_FUNC( WAGDIPFILLREGION )
{
  wa_ret_GpStatus(GdipFillRegion(wa_par_GpGraphics(1), wa_par_GpBrush(2), wa_par_GpRegion(3)));
}

// GpStatus WINGDIPAPI GdipDrawImage(GpGraphics*,GpImage*,REAL,REAL)
HB_FUNC( WAGDIPDRAWIMAGE )
{
  wa_ret_GpStatus(GdipDrawImage(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_REAL(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipDrawImageI(GpGraphics*,GpImage*,INT,INT)
HB_FUNC( WAGDIPDRAWIMAGEI )
{
  wa_ret_GpStatus(GdipDrawImageI(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_INT(3), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawImageRect(GpGraphics*,GpImage*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPDRAWIMAGERECT )
{
  wa_ret_GpStatus(GdipDrawImageRect(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipDrawImageRectI(GpGraphics*,GpImage*,INT,INT,INT,INT)
HB_FUNC( WAGDIPDRAWIMAGERECTI )
{
  wa_ret_GpStatus(GdipDrawImageRectI(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6)));
}

// GpStatus WINGDIPAPI GdipDrawImagePoints(GpGraphics*,GpImage*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPDRAWIMAGEPOINTS )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawImagePoints(wa_par_GpGraphics(1), wa_par_GpImage(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawImagePointsI(GpGraphics*,GpImage*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPDRAWIMAGEPOINTSI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawImagePointsI(wa_par_GpGraphics(1), wa_par_GpImage(2), vec.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipDrawImagePointRect(GpGraphics*,GpImage*,REAL,REAL,REAL,REAL,REAL,REAL,GpUnit)
HB_FUNC( WAGDIPDRAWIMAGEPOINTRECT )
{
  wa_ret_GpStatus(GdipDrawImagePointRect(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_GpUnit(9)));
}

// GpStatus WINGDIPAPI GdipDrawImagePointRectI(GpGraphics*,GpImage*,INT,INT,INT,INT,INT,INT,GpUnit)
HB_FUNC( WAGDIPDRAWIMAGEPOINTRECTI )
{
  wa_ret_GpStatus(GdipDrawImagePointRectI(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_INT(7), wa_par_INT(8), wa_par_GpUnit(9)));
}

// GpStatus WINGDIPAPI GdipDrawImageRectRect(GpGraphics*,GpImage*,REAL,REAL,REAL,REAL,REAL,REAL,REAL,REAL,GpUnit,GDIPCONST GpImageAttributes*,DrawImageAbort,VOID*)
HB_FUNC( WAGDIPDRAWIMAGERECTRECT ) // TODO: parameters 13 and 14
{
  wa_ret_GpStatus(GdipDrawImageRectRect(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_REAL(9), wa_par_REAL(10), wa_par_GpUnit(11), wa_par_GpImageAttributes(12), nullptr, nullptr));
}

// GpStatus WINGDIPAPI GdipDrawImageRectRectI(GpGraphics*,GpImage*,INT,INT,INT,INT,INT,INT,INT,INT,GpUnit,GDIPCONST GpImageAttributes*,DrawImageAbort,VOID*)
HB_FUNC( WAGDIPDRAWIMAGERECTRECTI ) // TODO: parameters 13 and 14
{
  wa_ret_GpStatus(GdipDrawImageRectRectI(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_INT(7), wa_par_INT(8), wa_par_INT(9), wa_par_INT(10), wa_par_GpUnit(11), wa_par_GpImageAttributes(12), nullptr, nullptr));
}

// GpStatus WINGDIPAPI GdipDrawImagePointsRect(GpGraphics*,GpImage*,GDIPCONST GpPointF*,INT,REAL,REAL,REAL,REAL,GpUnit,GDIPCONST GpImageAttributes*,DrawImageAbort,VOID*)
HB_FUNC( WAGDIPDRAWIMAGEPOINTSRECT ) // TODO: parameters 11 and 12
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawImagePointsRect(wa_par_GpGraphics(1), wa_par_GpImage(2), vec.data(), wa_par_INT(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_GpUnit(9), wa_par_GpImageAttributes(10), nullptr, nullptr));
}

// GpStatus WINGDIPAPI GdipDrawImagePointsRectI(GpGraphics*,GpImage*,GDIPCONST GpPoint*,INT,INT,INT,INT,INT,GpUnit,GDIPCONST GpImageAttributes*,DrawImageAbort,VOID*)
HB_FUNC( WAGDIPDRAWIMAGEPOINTSRECTI ) // TODO: parameters 10 and 11
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipDrawImagePointsRectI(wa_par_GpGraphics(1), wa_par_GpImage(2), vec.data(), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_INT(7), wa_par_GpUnit(8), wa_par_GpImageAttributes(9), nullptr, nullptr));
}

// GpStatus WINGDIPAPI GdipDrawImageFX(GpGraphics*,GpImage*,GpRectF*,GpMatrix*,CGpEffect*,GpImageAttributes*,GpUnit)
#if 0
HB_FUNC( WAGDIPDRAWIMAGEFX )
{
  wa_ret_GpStatus(GdipDrawImageFX(wa_par_GpGraphics(1), wa_par_GpImage(2), wa_par_GpRectF(3), wa_par_GpMatrix(4), wa_par_CGpEffect(5), wa_par_GpImageAttributes(6), wa_par_GpUnit(7)));
}
#endif

#ifdef __cplusplus

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestPoint(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST PointF&,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestPointI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Point&,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestRect(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST RectF&,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestRectI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Rect&,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

#endif

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestPoints(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST PointF*,INT,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileDestPointsI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Point*,INT,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

#ifdef __cplusplus

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestPoint(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST PointF&,GDIPCONST RectF&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestPointI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Point&,GDIPCONST Rect&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestRect(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST RectF&,GDIPCONST RectF&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestRectI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Rect&,GDIPCONST Rect&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestPoints(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST PointF*,INT,GDIPCONST RectF&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

// GpStatus WINGDIPAPI GdipEnumerateMetafileSrcRectDestPointsI(GpGraphics*,GDIPCONST GpMetafile*,GDIPCONST Point*,INT,GDIPCONST Rect&,Unit,EnumerateMetafileProc,VOID*,GDIPCONST GpImageAttributes*)

#endif

// GpStatus WINGDIPAPI GdipSetClipGraphics(GpGraphics*,GpGraphics*,CombineMode)
HB_FUNC( WAGDIPSETCLIPGRAPHICS )
{
  wa_ret_GpStatus(GdipSetClipGraphics(wa_par_GpGraphics(1), wa_par_GpGraphics(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipSetClipRect(GpGraphics*,REAL,REAL,REAL,REAL,CombineMode)
HB_FUNC( WAGDIPSETCLIPRECT )
{
  wa_ret_GpStatus(GdipSetClipRect(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_GpCombineMode(6)));
}

// GpStatus WINGDIPAPI GdipSetClipRectI(GpGraphics*,INT,INT,INT,INT,CombineMode)
HB_FUNC( WAGDIPSETCLIPRECTI )
{
  wa_ret_GpStatus(GdipSetClipRectI(wa_par_GpGraphics(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_GpCombineMode(6)));
}

// GpStatus WINGDIPAPI GdipSetClipPath(GpGraphics*,GpPath*,CombineMode)
HB_FUNC( WAGDIPSETCLIPPATH )
{
  wa_ret_GpStatus(GdipSetClipPath(wa_par_GpGraphics(1), wa_par_GpPath(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipSetClipRegion(GpGraphics*,GpRegion*,CombineMode)
HB_FUNC( WAGDIPSETCLIPREGION )
{
  wa_ret_GpStatus(GdipSetClipRegion(wa_par_GpGraphics(1), wa_par_GpRegion(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipSetClipHrgn(GpGraphics*,HRGN,CombineMode)
HB_FUNC( WAGDIPSETCLIPHRGN )
{
  wa_ret_GpStatus(GdipSetClipHrgn(wa_par_GpGraphics(1), wa_par_HRGN(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipResetClip(GpGraphics*)
HB_FUNC( WAGDIPRESETCLIP )
{
  wa_ret_GpStatus(GdipResetClip(wa_par_GpGraphics(1)));
}

// GpStatus WINGDIPAPI GdipTranslateClip(GpGraphics*,REAL,REAL)
HB_FUNC( WAGDIPTRANSLATECLIP )
{
  wa_ret_GpStatus(GdipTranslateClip(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipTranslateClipI(GpGraphics*,INT,INT)
HB_FUNC( WAGDIPTRANSLATECLIPI )
{
  wa_ret_GpStatus(GdipTranslateClipI(wa_par_GpGraphics(1), wa_par_INT(2), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipGetClip(GpGraphics*,GpRegion*)
HB_FUNC( WAGDIPGETCLIP )
{
  wa_ret_GpStatus(GdipGetClip(wa_par_GpGraphics(1), wa_par_GpRegion(2)));
}

// GpStatus WINGDIPAPI GdipGetClipBounds(GpGraphics*,GpRectF*)
HB_FUNC( WAGDIPGETCLIPBOUNDS )
{
  wa_ret_GpStatus(GdipGetClipBounds(wa_par_GpGraphics(1), wa_par_GpRectF(2)));
}

// GpStatus WINGDIPAPI GdipGetClipBoundsI(GpGraphics*,GpRect*)
HB_FUNC( WAGDIPGETCLIPBOUNDSI )
{
  wa_ret_GpStatus(GdipGetClipBoundsI(wa_par_GpGraphics(1), wa_par_GpRect(2)));
}

// GpStatus WINGDIPAPI GdipIsClipEmpty(GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISCLIPEMPTY )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsClipEmpty(wa_par_GpGraphics(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipGetVisibleClipBounds(GpGraphics*,GpRectF*)
HB_FUNC( WAGDIPGETVISIBLECLIPBOUNDS )
{
  wa_ret_GpStatus(GdipGetVisibleClipBounds(wa_par_GpGraphics(1), wa_par_GpRectF(2)));
}

// GpStatus WINGDIPAPI GdipGetVisibleClipBoundsI(GpGraphics*,GpRect*)
HB_FUNC( WAGDIPGETVISIBLECLIPBOUNDSI )
{
  wa_ret_GpStatus(GdipGetVisibleClipBoundsI(wa_par_GpGraphics(1), wa_par_GpRect(2)));
}

// GpStatus WINGDIPAPI GdipIsVisibleClipEmpty(GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLECLIPEMPTY )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleClipEmpty(wa_par_GpGraphics(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipIsVisiblePoint(GpGraphics*,REAL,REAL,BOOL*)
HB_FUNC( WAGDIPISVISIBLEPOINT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisiblePoint(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3), &b));
  wa_stor_BOOL(b, 4);
}

// GpStatus WINGDIPAPI GdipIsVisiblePointI(GpGraphics*,INT,INT,BOOL*)
HB_FUNC( WAGDIPISVISIBLEPOINTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisiblePointI(wa_par_GpGraphics(1), wa_par_INT(2), wa_par_INT(3), &b));
  wa_stor_BOOL(b, 4);
}

// GpStatus WINGDIPAPI GdipIsVisibleRect(GpGraphics*,REAL,REAL,REAL,REAL,BOOL*)
HB_FUNC( WAGDIPISVISIBLERECT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRect(wa_par_GpGraphics(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), &b));
  wa_stor_BOOL(b, 6);
}

// GpStatus WINGDIPAPI GdipIsVisibleRectI(GpGraphics*,INT,INT,INT,INT,BOOL*)
HB_FUNC( WAGDIPISVISIBLERECTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRectI(wa_par_GpGraphics(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), &b));
  wa_stor_BOOL(b, 6);
}

// GpStatus WINGDIPAPI GdipSaveGraphics(GpGraphics*,GraphicsState*)
HB_FUNC( WAGDIPSAVEGRAPHICS )
{
  GraphicsState gs{};
  wa_ret_GpStatus(GdipSaveGraphics(wa_par_GpGraphics(1), &gs));
  wa_stor_DWORD(gs, 2);
}

// GpStatus WINGDIPAPI GdipRestoreGraphics(GpGraphics*,GraphicsState)
HB_FUNC( WAGDIPRESTOREGRAPHICS )
{
  wa_ret_GpStatus(GdipRestoreGraphics(wa_par_GpGraphics(1), wa_par_GpGraphicsState(2)));
}

// GpStatus WINGDIPAPI GdipBeginContainer(GpGraphics*,GDIPCONST GpRectF*,GDIPCONST GpRectF*,GpUnit,GraphicsContainer*)
HB_FUNC( WAGDIPBEGINCONTAINER )
{
  GraphicsContainer gc{};
  wa_ret_GpStatus(GdipBeginContainer(wa_par_GpGraphics(1), wa_par_GpRectF(2), wa_par_GpRectF(3), wa_par_GpUnit(4), &gc));
  hb_storni(gc, 5);
}

// GpStatus WINGDIPAPI GdipBeginContainerI(GpGraphics*,GDIPCONST GpRect*,GDIPCONST GpRect*,GpUnit,GraphicsContainer*)
HB_FUNC( WAGDIPBEGINCONTAINERI )
{
  GraphicsContainer gc{};
  wa_ret_GpStatus(GdipBeginContainerI(wa_par_GpGraphics(1), wa_par_GpRect(2), wa_par_GpRect(3), wa_par_GpUnit(4), &gc));
  hb_storni(gc, 5);
}

// GpStatus WINGDIPAPI GdipBeginContainer2(GpGraphics*,GraphicsContainer*)
HB_FUNC( WAGDIPBEGINCONTAINER2 )
{
  GraphicsContainer gc{};
  wa_ret_GpStatus(GdipBeginContainer2(wa_par_GpGraphics(1), &gc));
  hb_storni(gc, 2);
}

// GpStatus WINGDIPAPI GdipEndContainer(GpGraphics*,GraphicsContainer)
HB_FUNC( WAGDIPENDCONTAINER )
{
  wa_ret_GpStatus(GdipEndContainer(wa_par_GpGraphics(1), wa_par_GpGraphicsContainer(2)));
}

// GpStatus WINGDIPAPI GdipComment(GpGraphics*,UINT,GDIPCONST BYTE*)

///////////////////////////////////////////////////////////////////////////////
// GraphicsPath functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreatePath(GpFillMode,GpPath**)
HB_FUNC( WAGDIPCREATEPATH )
{
  GpPath * p{};
  wa_ret_GpStatus(GdipCreatePath(wa_par_GpFillMode(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreatePath2(GDIPCONST GpPointF*,GDIPCONST BYTE*,INT,GpFillMode,GpPath**)
HB_FUNC( WAGDIPCREATEPATH2 )
{
  std::vector<GpPointF> vec1 = GpPointFArrayToVector(hb_param(1, Harbour::Item::ARRAY));
  std::vector<BYTE> vec2 = BYTEArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  GpPath * p{};
  wa_ret_GpStatus(GdipCreatePath2(vec1.data(), vec2.data(), wa_par_INT(3), wa_par_GpFillMode(4), &p));
  hb_storptr(p, 5);
}

// GpStatus WINGDIPAPI GdipCreatePath2I(GDIPCONST GpPoint*,GDIPCONST BYTE*,INT,GpFillMode,GpPath**)
HB_FUNC( WAGDIPCREATEPATH2I )
{
  std::vector<GpPoint> vec1 = GpPointArrayToVector(hb_param(1, Harbour::Item::ARRAY));
  std::vector<BYTE> vec2 = BYTEArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  GpPath * p{};
  wa_ret_GpStatus(GdipCreatePath2I(vec1.data(), vec2.data(), wa_par_INT(3), wa_par_GpFillMode(4), &p));
  hb_storptr(p, 5);
}

// GpStatus WINGDIPAPI GdipClonePath(GpPath*,GpPath**)
HB_FUNC( WAGDIPCLONEPATH )
{
  GpPath * p{};
  wa_ret_GpStatus(GdipClonePath(wa_par_GpPath(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeletePath(GpPath*)
HB_FUNC( WAGDIPDELETEPATH )
{
  wa_ret_GpStatus(GdipDeletePath(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipResetPath(GpPath*)
HB_FUNC( WAGDIPRESETPATH )
{
  wa_ret_GpStatus(GdipResetPath(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipGetPointCount(GpPath*,INT*)
HB_FUNC( WAGDIPGETPOINTCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPointCount(wa_par_GpPath(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetPathTypes(GpPath*,BYTE*,INT)

// GpStatus WINGDIPAPI GdipGetPathPoints(GpPath*,GpPointF*,INT)

// GpStatus WINGDIPAPI GdipGetPathPointsI(GpPath*,GpPoint*,INT)

// GpStatus WINGDIPAPI GdipGetPathFillMode(GpPath*,GpFillMode*)
HB_FUNC( WAGDIPGETPATHFILLMODE )
{
  GpFillMode fm{};
  wa_ret_GpStatus(GdipGetPathFillMode(wa_par_GpPath(1), &fm));
  hb_storni(fm, 2);
}

// GpStatus WINGDIPAPI GdipSetPathFillMode(GpPath*,GpFillMode)
HB_FUNC( WAGDIPSETPATHFILLMODE )
{
  wa_ret_GpStatus(GdipSetPathFillMode(wa_par_GpPath(1), wa_par_GpFillMode(2)));
}

// GpStatus WINGDIPAPI GdipGetPathData(GpPath*,GpPathData*)

// GpStatus WINGDIPAPI GdipStartPathFigure(GpPath*)
HB_FUNC( WAGDIPSTARTPATHFIGURE )
{
  wa_ret_GpStatus(GdipStartPathFigure(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipClosePathFigure(GpPath*)
HB_FUNC( WAGDIPCLOSEPATHFIGURE )
{
  wa_ret_GpStatus(GdipClosePathFigure(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipClosePathFigures(GpPath*)
HB_FUNC( WAGDIPCLOSEPATHFIGURES )
{
  wa_ret_GpStatus(GdipClosePathFigures(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipSetPathMarker(GpPath*)
HB_FUNC( WAGDIPSETPATHMARKER )
{
  wa_ret_GpStatus(GdipSetPathMarker(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipClearPathMarkers(GpPath*)
HB_FUNC( WAGDIPCLEARPATHMARKERS )
{
  wa_ret_GpStatus(GdipClearPathMarkers(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipReversePath(GpPath*)
HB_FUNC( WAGDIPREVERSEPATH )
{
  wa_ret_GpStatus(GdipReversePath(wa_par_GpPath(1)));
}

// GpStatus WINGDIPAPI GdipGetPathLastPoint(GpPath*,GpPointF*)
HB_FUNC( WAGDIPGETPATHLASTPOINT )
{
  wa_ret_GpStatus(GdipGetPathLastPoint(wa_par_GpPath(1), wa_par_GpPointF(2)));
}

// GpStatus WINGDIPAPI GdipAddPathLine(GpPath*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHLINE )
{
  wa_ret_GpStatus(GdipAddPathLine(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipAddPathLineI(GpPath*,INT,INT,INT,INT)
HB_FUNC( WAGDIPADDPATHLINEI )
{
  wa_ret_GpStatus(GdipAddPathLineI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5)));
}

// GpStatus WINGDIPAPI GdipAddPathLine2(GpPath*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPADDPATHLINE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathLine2(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathLine2I(GpPath*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPADDPATHLINE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathLine2I(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathArc(GpPath*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHARC )
{
  wa_ret_GpStatus(GdipAddPathArc(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipAddPathArcI(GpPath*,INT,INT,INT,INT,REAL,REAL)
HB_FUNC( WAGDIPADDPATHARCI )
{
  wa_ret_GpStatus(GdipAddPathArcI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_REAL(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipAddPathBezier(GpPath*,REAL,REAL,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHBEZIER )
{
  wa_ret_GpStatus(GdipAddPathBezier(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_REAL(9)));
}

// GpStatus WINGDIPAPI GdipAddPathBezierI(GpPath*,INT,INT,INT,INT,INT,INT,INT,INT)
HB_FUNC( WAGDIPADDPATHBEZIERI )
{
  wa_ret_GpStatus(GdipAddPathBezierI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), wa_par_INT(7), wa_par_INT(8), wa_par_INT(9)));
}

// GpStatus WINGDIPAPI GdipAddPathBeziers(GpPath*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPADDPATHBEZIERS )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathBeziers(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathBeziersI(GpPath*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPADDPATHBEZIERSI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathBeziersI(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathCurve(GpPath*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPADDPATHCURVE )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurve(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathCurveI(GpPath*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPADDPATHCURVEI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurveI(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathCurve2(GpPath*,GDIPCONST GpPointF*,INT,REAL)
HB_FUNC( WAGDIPADDPATHCURVE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurve2(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipAddPathCurve2I(GpPath*,GDIPCONST GpPoint*,INT,REAL)
HB_FUNC( WAGDIPADDPATHCURVE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurve2I(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipAddPathCurve3(GpPath*,GDIPCONST GpPointF*,INT,INT,INT,REAL)
HB_FUNC( WAGDIPADDPATHCURVE3 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurve3(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipAddPathCurve3I(GpPath*,GDIPCONST GpPoint*,INT,INT,INT,REAL)
HB_FUNC( WAGDIPADDPATHCURVE3I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathCurve3I(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_REAL(6)));
}

// GpStatus WINGDIPAPI GdipAddPathClosedCurve(GpPath*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPADDPATHCLOSEDCURVE )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathClosedCurve(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathClosedCurveI(GpPath*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPADDPATHCLOSEDCURVEI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathClosedCurveI(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathClosedCurve2(GpPath*,GDIPCONST GpPointF*,INT,REAL)
HB_FUNC( WAGDIPADDPATHCLOSEDCURVE2 )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathClosedCurve2(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipAddPathClosedCurve2I(GpPath*,GDIPCONST GpPoint*,INT,REAL)
HB_FUNC( WAGDIPADDPATHCLOSEDCURVE2I )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathClosedCurve2I(wa_par_GpPath(1), vec.data(), wa_par_INT(3), wa_par_REAL(4)));
}

// TODO: GdipAddPathClosedCurve3

// TODO: GdipAddPathClosedCurve3I

// GpStatus WINGDIPAPI GdipAddPathRectangle(GpPath*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHRECTANGLE )
{
  wa_ret_GpStatus(GdipAddPathRectangle(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipAddPathRectangleI(GpPath*,INT,INT,INT,INT)
HB_FUNC( WAGDIPADDPATHRECTANGLEI )
{
  wa_ret_GpStatus(GdipAddPathRectangleI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5)));
}

// GpStatus WINGDIPAPI GdipAddPathRectangles(GpPath*,GDIPCONST GpRectF*,INT)
HB_FUNC( WAGDIPADDPATHRECTANGLES )
{
  std::vector<GpRectF> vec = GpRectFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathRectangles(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathRectanglesI(GpPath*,GDIPCONST GpRect*,INT)
HB_FUNC( WAGDIPADDPATHRECTANGLESI )
{
  std::vector<GpRect> vec = GpRectArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathRectanglesI(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathEllipse(GpPath*,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHELLIPSE )
{
  wa_ret_GpStatus(GdipAddPathEllipse(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5)));
}

// GpStatus WINGDIPAPI GdipAddPathEllipseI(GpPath*,INT,INT,INT,INT)
HB_FUNC( WAGDIPADDPATHELLIPSEI )
{
  wa_ret_GpStatus(GdipAddPathEllipseI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5)));
}

// GpStatus WINGDIPAPI GdipAddPathPie(GpPath*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPADDPATHPIE )
{
  wa_ret_GpStatus(GdipAddPathPie(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipAddPathPieI(GpPath*,INT,INT,INT,INT,REAL,REAL)
HB_FUNC( WAGDIPADDPATHPIEI )
{
  wa_ret_GpStatus(GdipAddPathPieI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_REAL(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipAddPathPolygon(GpPath*,GDIPCONST GpPointF*,INT)
HB_FUNC( WAGDIPADDPATHPOLYGON )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathPolygon(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathPolygonI(GpPath*,GDIPCONST GpPoint*,INT)
HB_FUNC( WAGDIPADDPATHPOLYGONI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipAddPathPolygonI(wa_par_GpPath(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipAddPathPath(GpPath*,GDIPCONST GpPath*,BOOL)
HB_FUNC( WAGDIPADDPATHPATH )
{
  wa_ret_GpStatus(GdipAddPathPath(wa_par_GpPath(1), wa_par_GpPath(2), wa_par_BOOL(3)));
}

// GpStatus WINGDIPAPI GdipAddPathString(GpPath*,GDIPCONST WCHAR*,INT,GDIPCONST GpFontFamily*,INT,REAL,GDIPCONST RectF*,GDIPCONST GpStringFormat*)
HB_FUNC( WAGDIPADDPATHSTRING )
{
  void * str{};
  wa_ret_GpStatus(GdipAddPathString(wa_par_GpPath(1), HB_PARSTR(2, &str, nullptr), wa_par_INT(3), wa_par_GpFontFamily(4), wa_par_INT(5), wa_par_REAL(6), wa_par_GpRectF(7), wa_par_GpStringFormat(8)));
  hb_strfree(str);
}

// GpStatus WINGDIPAPI GdipAddPathStringI(GpPath*,GDIPCONST WCHAR*,INT,GDIPCONST GpFontFamily*,INT,REAL,GDIPCONST Rect*,GDIPCONST GpStringFormat*)
HB_FUNC( WAGDIPADDPATHSTRINGI )
{
  void * str{};
  wa_ret_GpStatus(GdipAddPathStringI(wa_par_GpPath(1), HB_PARSTR(2, &str, nullptr), wa_par_INT(3), wa_par_GpFontFamily(4), wa_par_INT(5), wa_par_REAL(6), wa_par_GpRect(7), wa_par_GpStringFormat(8)));
  hb_strfree(str);
}

// GpStatus WINGDIPAPI GdipFlattenPath(GpPath*,GpMatrix*,REAL)
HB_FUNC( WAGDIPFLATTENPATH )
{
  wa_ret_GpStatus(GdipFlattenPath(wa_par_GpPath(1), wa_par_GpMatrix(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipWindingModeOutline(GpPath*,GpMatrix*,REAL)
HB_FUNC( WAGDIPWINDINGMODEOUTLINE )
{
  wa_ret_GpStatus(GdipWindingModeOutline(wa_par_GpPath(1), wa_par_GpMatrix(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipWidenPath(GpPath*,GpPen*,GpMatrix*,REAL)
HB_FUNC( WAGDIPWIDENPATH )
{
  wa_ret_GpStatus(GdipWidenPath(wa_par_GpPath(1), wa_par_GpPen(2), wa_par_GpMatrix(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipWarpPath(GpPath*,GpMatrix*,GDIPCONST GpPointF*,INT,REAL,REAL,REAL,REAL,WarpMode,REAL)
HB_FUNC( WAGDIPWARPPATH )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipWarpPath(wa_par_GpPath(1), wa_par_GpMatrix(2), vec.data(), wa_par_INT(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7), wa_par_REAL(8), wa_par_GpWarpMode(9), wa_par_REAL(10)));
}

// GpStatus WINGDIPAPI GdipTransformPath(GpPath*,GpMatrix*)
HB_FUNC( WAGDIPTRANSFORMPATH )
{
  wa_ret_GpStatus(GdipTransformPath(wa_par_GpPath(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipGetPathWorldBounds(GpPath*,GpRectF*,GDIPCONST GpMatrix*,GDIPCONST GpPen*)
HB_FUNC( WAGDIPGETPATHWORLDBOUNDS )
{
  wa_ret_GpStatus(GdipGetPathWorldBounds(wa_par_GpPath(1), wa_par_GpRectF(2), wa_par_GpMatrix(3), wa_par_GpPen(4)));
}

// GpStatus WINGDIPAPI GdipGetPathWorldBoundsI(GpPath*,GpRect*,GDIPCONST GpMatrix*,GDIPCONST GpPen*)
HB_FUNC( WAGDIPGETPATHWORLDBOUNDSI )
{
  wa_ret_GpStatus(GdipGetPathWorldBoundsI(wa_par_GpPath(1), wa_par_GpRect(2), wa_par_GpMatrix(3), wa_par_GpPen(4)));
}

// GpStatus WINGDIPAPI GdipIsVisiblePathPoint(GpPath*,REAL,REAL,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEPATHPOINT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisiblePathPoint(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpGraphics(4), &b));
  wa_stor_BOOL(b, 5);
}

// GpStatus WINGDIPAPI GdipIsVisiblePathPointI(GpPath*,INT,INT,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEPATHPOINTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisiblePathPointI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_GpGraphics(4), &b));
  wa_stor_BOOL(b, 5);
}

// GpStatus WINGDIPAPI GdipIsOutlineVisiblePathPoint(GpPath*,REAL,REAL,GpPen*,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISOUTLINEVISIBLEPATHPOINT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsOutlineVisiblePathPoint(wa_par_GpPath(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpPen(4), wa_par_GpGraphics(5), &b));
  wa_stor_BOOL(b, 6);
}

// GpStatus WINGDIPAPI GdipIsOutlineVisiblePathPointI(GpPath*,INT,INT,GpPen*,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISOUTLINEVISIBLEPATHPOINTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsOutlineVisiblePathPointI(wa_par_GpPath(1), wa_par_INT(2), wa_par_INT(3), wa_par_GpPen(4), wa_par_GpGraphics(5), &b));
  wa_stor_BOOL(b, 6);
}

///////////////////////////////////////////////////////////////////////////////
// HatchBrush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateHatchBrush(GpHatchStyle,ARGB,ARGB,GpHatch**)
HB_FUNC( WAGDIPCREATEHATCHBRUSH )
{
  GpHatch * p{};
  wa_ret_GpStatus(GdipCreateHatchBrush(wa_par_GpHatchStyle(1), wa_par_ARGB(2), wa_par_ARGB(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipGetHatchStyle(GpHatch*,GpHatchStyle*)
HB_FUNC( WAGDIPGETHATCHSTYLE )
{
  GpHatchStyle hs{};
  wa_ret_GpStatus(GdipGetHatchStyle(wa_par_GpHatch(1), &hs));
  hb_storni(hs, 2);
}

// GpStatus WINGDIPAPI GdipGetHatchForegroundColor(GpHatch*,ARGB*)
HB_FUNC( WAGDIPGETHATCHFOREGROUNDCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetHatchForegroundColor(wa_par_GpHatch(1), &argb));
  wa_stor_ARGB(argb, 2);
}

// GpStatus WINGDIPAPI GdipGetHatchBackgroundColor(GpHatch*,ARGB*)
HB_FUNC( WAGDIPGETHATCHBACKGROUNDCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetHatchBackgroundColor(wa_par_GpHatch(1), &argb));
  wa_stor_ARGB(argb, 2);
}

///////////////////////////////////////////////////////////////////////////////
// Image functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipLoadImageFromStream(IStream*,GpImage**)
HB_FUNC( WAGDIPLOADIMAGEFROMSTREAM )
{
  GpImage * p{};
  wa_ret_GpStatus(GdipLoadImageFromStream(wa_par_IStream(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipLoadImageFromFile(GDIPCONST WCHAR*,GpImage**)
HB_FUNC( WAGDIPLOADIMAGEFROMFILE )
{
  GpImage * p{};
  void * str{};
  wa_ret_GpStatus(GdipLoadImageFromFile(HB_PARSTR(1, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipLoadImageFromStreamICM(IStream*,GpImage**)
HB_FUNC( WAGDIPLOADIMAGEFROMSTREAMICM )
{
  GpImage * p{};
  wa_ret_GpStatus(GdipLoadImageFromStreamICM(wa_par_IStream(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipLoadImageFromFileICM(GDIPCONST WCHAR*,GpImage**)
HB_FUNC( WAGDIPLOADIMAGEFROMFILEICM )
{
  GpImage * p{};
  void * str{};
  wa_ret_GpStatus(GdipLoadImageFromFileICM(HB_PARSTR(1, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCloneImage(GpImage*,GpImage**)
HB_FUNC( WAGDIPCLONEIMAGE )
{
  GpImage * p{};
  wa_ret_GpStatus(GdipCloneImage(wa_par_GpImage(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDisposeImage(GpImage*)
HB_FUNC( WAGDIPDISPOSEIMAGE )
{
  wa_ret_GpStatus(GdipDisposeImage(wa_par_GpImage(1)));
}

// GpStatus WINGDIPAPI GdipSaveImageToFile(GpImage*,GDIPCONST WCHAR*,GDIPCONST CLSID*,GDIPCONST EncoderParameters*)

// GpStatus WINGDIPAPI GdipSaveImageToStream(GpImage*,IStream*,GDIPCONST CLSID*,GDIPCONST EncoderParameters*)

// GpStatus WINGDIPAPI GdipSaveAdd(GpImage*,GDIPCONST EncoderParameters*)
HB_FUNC( WAGDIPSAVEADD )
{
  wa_ret_GpStatus(GdipSaveAdd(wa_par_GpImage(1), wa_par_GpEncoderParameters(2)));
}

// GpStatus WINGDIPAPI GdipSaveAddImage(GpImage*,GpImage*,GDIPCONST EncoderParameters*)
HB_FUNC( WAGDIPSAVEADDIMAGE )
{
  wa_ret_GpStatus(GdipSaveAddImage(wa_par_GpImage(1), wa_par_GpImage(2), wa_par_GpEncoderParameters(3)));
}

// GpStatus WINGDIPAPI GdipGetImageGraphicsContext(GpImage*,GpGraphics**)
HB_FUNC( WAGDIPGETIMAGEGRAPHICSCONTEXT )
{
  GpGraphics * p{};
  wa_ret_GpStatus(GdipGetImageGraphicsContext(wa_par_GpImage(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetImageBounds(GpImage*,GpRectF*,GpUnit*)
HB_FUNC( WAGDIPGETIMAGEBOUNDS )
{
  GpUnit u{};
  wa_ret_GpStatus(GdipGetImageBounds(wa_par_GpImage(1), wa_par_GpRectF(2), &u));
  hb_storni(u, 3);
}

// GpStatus WINGDIPAPI GdipGetImageDimension(GpImage*,REAL*,REAL*)
HB_FUNC( WAGDIPGETIMAGEDIMENSION )
{
  REAL r1{};
  REAL r2{};
  wa_ret_GpStatus(GdipGetImageDimension(wa_par_GpImage(1), &r1, &r2));
  wa_stor_REAL(r1, 2);
  wa_stor_REAL(r2, 3);
}

// GpStatus WINGDIPAPI GdipGetImageType(GpImage*,ImageType*)
HB_FUNC( WAGDIPGETIMAGETYPE )
{
  ImageType it{};
  wa_ret_GpStatus(GdipGetImageType(wa_par_GpImage(1), &it));
  hb_storni(it, 2);
}

// GpStatus WINGDIPAPI GdipGetImageWidth(GpImage*,UINT*)
HB_FUNC( WAGDIPGETIMAGEWIDTH )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetImageWidth(wa_par_GpImage(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipGetImageHeight(GpImage*,UINT*)
HB_FUNC( WAGDIPGETIMAGEHEIGHT )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetImageHeight(wa_par_GpImage(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipGetImageHorizontalResolution(GpImage*,REAL*)
HB_FUNC( WAGDIPGETIMAGEHORIZONTALRESOLUTION )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetImageHorizontalResolution(wa_par_GpImage(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipGetImageVerticalResolution(GpImage*,REAL*)
HB_FUNC( WAGDIPGETIMAGEVERTICALRESOLUTION )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetImageVerticalResolution(wa_par_GpImage(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipGetImageFlags(GpImage*,UINT*)
HB_FUNC( WAGDIPGETIMAGEFLAGS )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetImageFlags(wa_par_GpImage(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipGetImageRawFormat(GpImage*,GUID*)

// GpStatus WINGDIPAPI GdipGetImagePixelFormat(GpImage*,PixelFormat*)
HB_FUNC( WAGDIPGETIMAGEPIXELFORMAT )
{
  PixelFormat pf{};
  wa_ret_GpStatus(GdipGetImagePixelFormat(wa_par_GpImage(1), &pf));
  hb_storni(pf, 2);
}

// GpStatus WINGDIPAPI GdipGetImageThumbnail(GpImage*,UINT,UINT,GpImage**,GetThumbnailImageAbort,VOID*)
HB_FUNC( WAGDIPGETIMAGETHUMBNAIL ) // TODO: parameters 5 and 6
{
  GpImage * p{};
  wa_ret_GpStatus(GdipGetImageThumbnail(wa_par_GpImage(1), wa_par_UINT(2), wa_par_UINT(3), &p, nullptr, nullptr));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipGetEncoderParameterListSize(GpImage*,GDIPCONST CLSID*,UINT*)

// GpStatus WINGDIPAPI GdipGetEncoderParameterList(GpImage*,GDIPCONST CLSID*,UINT,EncoderParameters*)

// GpStatus WINGDIPAPI GdipImageGetFrameDimensionsCount(GpImage*,UINT*)
HB_FUNC( WAGDIPIMAGEGETFRAMEDIMENSIONSCOUNT )
{
  UINT ui{};
  wa_ret_GpStatus(GdipImageGetFrameDimensionsCount(wa_par_GpImage(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipImageGetFrameDimensionsList(GpImage*,GUID*,UINT)

// GpStatus WINGDIPAPI GdipImageGetFrameCount(GpImage*,GDIPCONST GUID*,UINT*)

// GpStatus WINGDIPAPI GdipImageSelectActiveFrame(GpImage*,GDIPCONST GUID*,UINT)

// GpStatus WINGDIPAPI GdipImageRotateFlip(GpImage*,RotateFlipType)
HB_FUNC( WAGDIPIMAGEROTATEFLIP )
{
  wa_ret_GpStatus(GdipImageRotateFlip(wa_par_GpImage(1), wa_par_GpRotateFlipType(2)));
}

// GpStatus WINGDIPAPI GdipGetImagePalette(GpImage*,ColorPalette*,INT)
HB_FUNC( WAGDIPGETIMAGEPALETTE )
{
  wa_ret_GpStatus(GdipGetImagePalette(wa_par_GpImage(1), wa_par_GpColorPalette(2), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipSetImagePalette(GpImage*,GDIPCONST ColorPalette*)
HB_FUNC( WAGDIPSETIMAGEPALETTE )
{
  wa_ret_GpStatus(GdipSetImagePalette(wa_par_GpImage(1), wa_par_GpColorPalette(2)));
}

// GpStatus WINGDIPAPI GdipGetImagePaletteSize(GpImage*,INT*)
HB_FUNC( WAGDIPGETIMAGEPALETTESIZE )
{
  INT i{};
  wa_ret_GpStatus(GdipGetImagePaletteSize(wa_par_GpImage(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetPropertyCount(GpImage*,UINT*)
HB_FUNC( WAGDIPGETPROPERTYCOUNT )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetPropertyCount(wa_par_GpImage(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipGetPropertyIdList(GpImage*,UINT,PROPID*)

// GpStatus WINGDIPAPI GdipGetPropertyItemSize(GpImage*,PROPID,UINT*)

// GpStatus WINGDIPAPI GdipGetPropertyItem(GpImage*,PROPID,UINT,PropertyItem*)

// GpStatus WINGDIPAPI GdipGetPropertySize(GpImage*,UINT*,UINT*)
HB_FUNC( WAGDIPGETPROPERTYSIZE )
{
  UINT ui1{};
  UINT ui2{};
  wa_ret_GpStatus(GdipGetPropertySize(wa_par_GpImage(1), &ui1, &ui2));
  wa_stor_UINT(ui1, 2);
  wa_stor_UINT(ui2, 3);
}

// GpStatus WINGDIPAPI GdipGetAllPropertyItems(GpImage*,UINT,UINT,PropertyItem*)

// GpStatus WINGDIPAPI GdipRemovePropertyItem(GpImage*,PROPID)

// GpStatus WINGDIPAPI GdipSetPropertyItem(GpImage*,GDIPCONST PropertyItem*)
HB_FUNC( WAGDIPSETPROPERTYITEM )
{
  wa_ret_GpStatus(GdipSetPropertyItem(wa_par_GpImage(1), wa_par_GpPropertyItem(2)));
}

// GpStatus WINGDIPAPI GdipFindFirstImageItem(GpImage*,ImageItemData*)

// GpStatus WINGDIPAPI GdipFindNextImageItem(GpImage*,ImageItemData*)

// GpStatus WINGDIPAPI GdipGetImageItemData(GpImage*,ImageItemData*)

// GpStatus WINGDIPAPI GdipImageSetAbort(GpImage*,GdiplusAbort*)

// GpStatus WINGDIPAPI GdipImageForceValidation(GpImage*)
HB_FUNC( WAGDIPIMAGEFORCEVALIDATION )
{
  wa_ret_GpStatus(GdipImageForceValidation(wa_par_GpImage(1)));
}

///////////////////////////////////////////////////////////////////////////////
// Image codec functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipGetImageDecodersSize(UINT*,UINT*)
#if 0
HB_FUNC( WAGDIPGETIMAGEDECODERSSIZE )
{
  UINT ui1{};
  UINT ui2{};
  wa_ret_GpStatus(GdipGetImageDecodersSize(&ui1, &ui2));
  wa_stor_UINT(ui1, 1);
  wa_stor_UINT(ui2, 2);
}
#endif

// GpStatus WINGDIPAPI GdipGetImageDecoders(UINT,UINT,ImageCodecInfo*)

// GpStatus WINGDIPAPI GdipGetImageEncodersSize(UINT*,UINT*)
#if 0
HB_FUNC( WAGDIPGETIMAGEENCODERSSIZE )
{
  UINT ui1{};
  UINT ui2{};
  wa_ret_GpStatus(GdipGetImageEncodersSize(&ui1, &ui2));
  wa_stor_UINT(ui1, 1);
  wa_stor_UINT(ui2, 2);
}
#endif

// GpStatus WINGDIPAPI GdipGetImageEncoders(UINT,UINT,ImageCodecInfo*)

///////////////////////////////////////////////////////////////////////////////
// ImageAttributes functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateImageAttributes(GpImageAttributes**)
HB_FUNC( WAGDIPCREATEIMAGEATTRIBUTES )
{
  GpImageAttributes * p{};
  wa_ret_GpStatus(GdipCreateImageAttributes(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipCloneImageAttributes(GDIPCONST GpImageAttributes*,GpImageAttributes**)
HB_FUNC( WAGDIPCLONEIMAGEATTRIBUTES )
{
  GpImageAttributes * p{};
  wa_ret_GpStatus(GdipCloneImageAttributes(wa_par_GpImageAttributes(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDisposeImageAttributes(GpImageAttributes*)
HB_FUNC( WAGDIPDISPOSEIMAGEATTRIBUTES )
{
  wa_ret_GpStatus(GdipDisposeImageAttributes(wa_par_GpImageAttributes(1)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesToIdentity(GpImageAttributes*,ColorAdjustType)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESTOIDENTITY )
{
  wa_ret_GpStatus(GdipSetImageAttributesToIdentity(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2)));
}

// GpStatus WINGDIPAPI GdipResetImageAttributes(GpImageAttributes*,ColorAdjustType)
HB_FUNC( WAGDIPRESETIMAGEATTRIBUTES )
{
  wa_ret_GpStatus(GdipResetImageAttributes(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesColorMatrix(GpImageAttributes*,ColorAdjustType,BOOL,GDIPCONST ColorMatrix*,GDIPCONST ColorMatrix*,ColorMatrixFlags)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESCOLORMATRIX )
{
  wa_ret_GpStatus(GdipSetImageAttributesColorMatrix(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_GpColorMatrix(4), wa_par_GpColorMatrix(5), wa_par_GpColorMatrixFlags(6)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesThreshold(GpImageAttributes*,ColorAdjustType,BOOL,REAL)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESTHRESHOLD )
{
  wa_ret_GpStatus(GdipSetImageAttributesThreshold(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesGamma(GpImageAttributes*,ColorAdjustType,BOOL,REAL)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESGAMMA )
{
  wa_ret_GpStatus(GdipSetImageAttributesGamma(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_REAL(4)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesNoOp(GpImageAttributes*,ColorAdjustType,BOOL)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESNOOP )
{
  wa_ret_GpStatus(GdipSetImageAttributesNoOp(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesColorKeys(GpImageAttributes*,ColorAdjustType,BOOL,ARGB,ARGB)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESCOLORKEYS )
{
  wa_ret_GpStatus(GdipSetImageAttributesColorKeys(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_ARGB(4), wa_par_ARGB(5)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesOutputChannel(GpImageAttributes*,ColorAdjustType,BOOL,ColorChannelFlags)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESOUTPUTCHANNEL )
{
  wa_ret_GpStatus(GdipSetImageAttributesOutputChannel(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_GpColorChannelFlags(4)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesOutputChannelColorProfile(GpImageAttributes*,ColorAdjustType,BOOL,GDIPCONST WCHAR*)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESOUTPUTCHANNELCOLORPROFILE )
{
  void * str{};
  wa_ret_GpStatus(GdipSetImageAttributesOutputChannelColorProfile(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), HB_PARSTR(4, &str, nullptr)));
  hb_strfree(str);
}

// GpStatus WINGDIPAPI GdipSetImageAttributesRemapTable(GpImageAttributes*,ColorAdjustType,BOOL,UINT,GDIPCONST ColorMap*)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESREMAPTABLE )
{
  std::vector<ColorMap> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<ColorMap*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  wa_ret_GpStatus(GdipSetImageAttributesRemapTable(wa_par_GpImageAttributes(1), wa_par_GpColorAdjustType(2), wa_par_BOOL(3), wa_par_UINT(4), vec.data()));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesWrapMode(GpImageAttributes*,WrapMode,ARGB,BOOL)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESWRAPMODE )
{
  wa_ret_GpStatus(GdipSetImageAttributesWrapMode(wa_par_GpImageAttributes(1), wa_par_GpWrapMode(2), wa_par_ARGB(3), wa_par_BOOL(4)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesICMMode(GpImageAttributes*,BOOL)
#if 0
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESICMMODE )
{
  wa_ret_GpStatus(GdipSetImageAttributesICMMode(wa_par_GpImageAttributes(1), wa_par_BOOL(2)));
}
#endif

// GpStatus WINGDIPAPI GdipGetImageAttributesAdjustedPalette(GpImageAttributes*,ColorPalette*,ColorAdjustType)
HB_FUNC( WAGDIPGETIMAGEATTRIBUTESADJUSTEDPALETTE )
{
  wa_ret_GpStatus(GdipGetImageAttributesAdjustedPalette(wa_par_GpImageAttributes(1), wa_par_GpColorPalette(2), wa_par_GpColorAdjustType(3)));
}

// GpStatus WINGDIPAPI GdipSetImageAttributesCachedBackground(GpImageAttributes*,BOOL)
HB_FUNC( WAGDIPSETIMAGEATTRIBUTESCACHEDBACKGROUND )
{
  wa_ret_GpStatus(GdipSetImageAttributesCachedBackground(wa_par_GpImageAttributes(1), wa_par_BOOL(2)));
}

///////////////////////////////////////////////////////////////////////////////
// LinearGradientBrush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateLineBrush(GDIPCONST GpPointF*,GDIPCONST GpPointF*,ARGB,ARGB,GpWrapMode,GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSH )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrush(wa_par_GpPointF(1), wa_par_GpPointF(2), wa_par_ARGB(3), wa_par_ARGB(4), wa_par_GpWrapMode(5), &p));
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipCreateLineBrushI(GDIPCONST GpPoint*,GDIPCONST GpPoint*,ARGB,ARGB,GpWrapMode,GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSHI )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrushI(wa_par_GpPoint(1), wa_par_GpPoint(2), wa_par_ARGB(3), wa_par_ARGB(4), wa_par_GpWrapMode(5), &p));
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipCreateLineBrushFromRect(GDIPCONST GpRectF*,ARGB,ARGB,LinearGradientMode,GpWrapMode,GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSHFROMRECT )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrushFromRect(wa_par_GpRectF(1), wa_par_ARGB(2), wa_par_ARGB(3), wa_par_GpLinearGradientMode(4), wa_par_GpWrapMode(5), &p));
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipCreateLineBrushFromRectI(GDIPCONST GpRect*, ARGB, ARGB, LinearGradientMode, GpWrapMode, GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSHFROMRECTI )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrushFromRectI(wa_par_GpRect(1), wa_par_ARGB(2), wa_par_ARGB(3), wa_par_GpLinearGradientMode(4), wa_par_GpWrapMode(5), &p));
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipCreateLineBrushFromRectWithAngle(GDIPCONST GpRectF*,ARGB,ARGB,REAL,BOOL,GpWrapMode,GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSHFROMRECTWITHANGLE )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrushFromRectWithAngle(wa_par_GpRectF(1), wa_par_ARGB(2), wa_par_ARGB(3), wa_par_REAL(4), wa_par_BOOL(5), wa_par_GpWrapMode(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCreateLineBrushFromRectWithAngleI(GDIPCONST GpRect*,ARGB,ARGB,REAL,BOOL,GpWrapMode,GpLineGradient**)
HB_FUNC( WAGDIPCREATELINEBRUSHFROMRECTWITHANGLEI )
{
  GpLineGradient * p{};
  wa_ret_GpStatus(GdipCreateLineBrushFromRectWithAngleI(wa_par_GpRect(1), wa_par_ARGB(2), wa_par_ARGB(3), wa_par_REAL(4), wa_par_BOOL(5), wa_par_GpWrapMode(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipSetLineColors(GpLineGradient*,ARGB,ARGB)
HB_FUNC( WAGDIPSETLINECOLORS )
{
  wa_ret_GpStatus(GdipSetLineColors(wa_par_GpLineGradient(1), wa_par_ARGB(2), wa_par_ARGB(3)));
}

// GpStatus WINGDIPAPI GdipGetLineColors(GpLineGradient*,ARGB*)
#if 0
HB_FUNC( WAGDIPGETLINECOLORS ) // TODO: parameter 2 is a array
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetLineColors(wa_par_GpLineGradient(1), &argb));
  wa_stor_ARGB(argb, 2);
}
#endif

// GpStatus WINGDIPAPI GdipGetLineRect(GpLineGradient*,GpRectF*)
HB_FUNC( WAGDIPGETLINERECT )
{
  wa_ret_GpStatus(GdipGetLineRect(wa_par_GpLineGradient(1), wa_par_GpRectF(2)));
}

// GpStatus WINGDIPAPI GdipGetLineRectI(GpLineGradient*,GpRect*)
HB_FUNC( WAGDIPGETLINERECTI )
{
  wa_ret_GpStatus(GdipGetLineRectI(wa_par_GpLineGradient(1), wa_par_GpRect(2)));
}

// GpStatus WINGDIPAPI GdipSetLineGammaCorrection(GpLineGradient*,BOOL)
HB_FUNC( WAGDIPSETLINEGAMMACORRECTION )
{
  wa_ret_GpStatus(GdipSetLineGammaCorrection(wa_par_GpLineGradient(1), wa_par_BOOL(2)));
}

// GpStatus WINGDIPAPI GdipGetLineGammaCorrection(GpLineGradient*,BOOL*)
HB_FUNC( WAGDIPGETLINEGAMMACORRECTION )
{
  BOOL b{};
  wa_ret_GpStatus(GdipGetLineGammaCorrection(wa_par_GpLineGradient(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipGetLineBlendCount(GpLineGradient*,INT*)
HB_FUNC( WAGDIPGETLINEBLENDCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetLineBlendCount(wa_par_GpLineGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetLineBlend(GpLineGradient*,REAL*,REAL*,INT)
#if 0
HB_FUNC( WAGDIPGETLINEBLEND ) // TODO: parameters 2 and 3 are array
{
  REAL r1{};
  REAL r2{};
  wa_ret_GpStatus(GdipGetLineBlend(wa_par_GpLineGradient(1), &r1, &r2, wa_par_INT(4)));
  wa_stor_REAL(r1, 2);
  wa_stor_REAL(r2, 3);
}
#endif

// GpStatus WINGDIPAPI GdipSetLineBlend(GpLineGradient*,GDIPCONST REAL*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETLINEBLEND )
{
  std::vector<REAL> vec1 = REALArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  std::vector<REAL> vec2 = REALArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetLineBlend(wa_par_GpLineGradient(1), vec1.data(), vec2.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipGetLinePresetBlendCount(GpLineGradient*,INT*)
HB_FUNC( WAGDIPGETLINEPRESETBLENDCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetLinePresetBlendCount(wa_par_GpLineGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetLinePresetBlend(GpLineGradient*,ARGB*,REAL*,INT)

// GpStatus WINGDIPAPI GdipSetLinePresetBlend(GpLineGradient*,GDIPCONST ARGB*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETLINEPRESETBLEND )
{
  std::vector<ARGB> vec1 = ARGBArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  std::vector<REAL> vec2 = REALArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetLinePresetBlend(wa_par_GpLineGradient(1), vec1.data(), vec2.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipSetLineSigmaBlend(GpLineGradient*,REAL,REAL)
HB_FUNC( WAGDIPSETLINESIGMABLEND )
{
  wa_ret_GpStatus(GdipSetLineSigmaBlend(wa_par_GpLineGradient(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipSetLineLinearBlend(GpLineGradient*,REAL,REAL)
HB_FUNC( WAGDIPSETLINELINEARBLEND )
{
  wa_ret_GpStatus(GdipSetLineLinearBlend(wa_par_GpLineGradient(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipSetLineWrapMode(GpLineGradient*,GpWrapMode)
HB_FUNC( WAGDIPSETLINEWRAPMODE )
{
  wa_ret_GpStatus(GdipSetLineWrapMode(wa_par_GpLineGradient(1), wa_par_GpWrapMode(2)));
}

// GpStatus WINGDIPAPI GdipGetLineWrapMode(GpLineGradient*,GpWrapMode*)
HB_FUNC( WAGDIPGETLINEWRAPMODE )
{
  GpWrapMode wm{};
  wa_ret_GpStatus(GdipGetLineWrapMode(wa_par_GpLineGradient(1), &wm));
  hb_storni(wm, 2);
}

// GpStatus WINGDIPAPI GdipGetLineTransform(GpLineGradient*,GpMatrix*)
HB_FUNC( WAGDIPGETLINETRANSFORM )
{
  wa_ret_GpStatus(GdipGetLineTransform(wa_par_GpLineGradient(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipSetLineTransform(GpLineGradient*,GDIPCONST GpMatrix*)
HB_FUNC( WAGDIPSETLINETRANSFORM )
{
  wa_ret_GpStatus(GdipSetLineTransform(wa_par_GpLineGradient(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetLineTransform(GpLineGradient*)
HB_FUNC( WAGDIPRESETLINETRANSFORM )
{
  wa_ret_GpStatus(GdipResetLineTransform(wa_par_GpLineGradient(1)));
}

// GpStatus WINGDIPAPI GdipMultiplyLineTransform(GpLineGradient*,GDIPCONST GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYLINETRANSFORM )
{
  wa_ret_GpStatus(GdipMultiplyLineTransform(wa_par_GpLineGradient(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslateLineTransform(GpLineGradient*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATELINETRANSFORM )
{
  wa_ret_GpStatus(GdipTranslateLineTransform(wa_par_GpLineGradient(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScaleLineTransform(GpLineGradient*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALELINETRANSFORM )
{
  wa_ret_GpStatus(GdipScaleLineTransform(wa_par_GpLineGradient(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotateLineTransform(GpLineGradient*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATELINETRANSFORM )
{
  wa_ret_GpStatus(GdipRotateLineTransform(wa_par_GpLineGradient(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

///////////////////////////////////////////////////////////////////////////////
// Matrix functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateMatrix(GpMatrix**)
HB_FUNC( WAGDIPCREATEMATRIX )
{
  GpMatrix * p{};
  wa_ret_GpStatus(GdipCreateMatrix(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipCreateMatrix2(REAL,REAL,REAL,REAL,REAL,REAL,GpMatrix**)
HB_FUNC( WAGDIPCREATEMATRIX2 )
{
  GpMatrix * p{};
  wa_ret_GpStatus(GdipCreateMatrix2(wa_par_REAL(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCreateMatrix3(GDIPCONST GpRectF*,GDIPCONST GpPointF*,GpMatrix**)
HB_FUNC( WAGDIPCREATEMATRIX3 )
{
  GpMatrix * p{};
  wa_ret_GpStatus(GdipCreateMatrix3(wa_par_GpRectF(1), wa_par_GpPointF(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateMatrix3I(GDIPCONST GpRect*,GDIPCONST GpPoint*,GpMatrix**)
HB_FUNC( WAGDIPCREATEMATRIX3I )
{
  GpMatrix * p{};
  wa_ret_GpStatus(GdipCreateMatrix3I(wa_par_GpRect(1), wa_par_GpPoint(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCloneMatrix(GpMatrix*,GpMatrix**)
HB_FUNC( WAGDIPCLONEMATRIX )
{
  GpMatrix * p{};
  wa_ret_GpStatus(GdipCloneMatrix(wa_par_GpMatrix(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeleteMatrix(GpMatrix*)
HB_FUNC( WAGDIPDELETEMATRIX )
{
  wa_ret_GpStatus(GdipDeleteMatrix(wa_par_GpMatrix(1)));
}

// GpStatus WINGDIPAPI GdipSetMatrixElements(GpMatrix*,REAL,REAL,REAL,REAL,REAL,REAL)
HB_FUNC( WAGDIPSETMATRIXELEMENTS )
{
  wa_ret_GpStatus(GdipSetMatrixElements(wa_par_GpMatrix(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), wa_par_REAL(7)));
}

// GpStatus WINGDIPAPI GdipMultiplyMatrix(GpMatrix*,GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYMATRIX )
{
  wa_ret_GpStatus(GdipMultiplyMatrix(wa_par_GpMatrix(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslateMatrix(GpMatrix*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATEMATRIX )
{
  wa_ret_GpStatus(GdipTranslateMatrix(wa_par_GpMatrix(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScaleMatrix(GpMatrix*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALEMATRIX )
{
  wa_ret_GpStatus(GdipScaleMatrix(wa_par_GpMatrix(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotateMatrix(GpMatrix*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATEMATRIX )
{
  wa_ret_GpStatus(GdipRotateMatrix(wa_par_GpMatrix(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipShearMatrix(GpMatrix*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSHEARMATRIX )
{
  wa_ret_GpStatus(GdipShearMatrix(wa_par_GpMatrix(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipInvertMatrix(GpMatrix*)
HB_FUNC( WAGDIPINVERTMATRIX )
{
  wa_ret_GpStatus(GdipInvertMatrix(wa_par_GpMatrix(1)));
}

// GpStatus WINGDIPAPI GdipTransformMatrixPoints(GpMatrix*,GpPointF*,INT)
#if 0
HB_FUNC( WAGDIPTRANSFORMMATRIXPOINTS )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipTransformMatrixPoints(wa_par_GpMatrix(1), vec.data(), wa_par_INT(3)));
  // TODO: copy values from vector to array
}
#endif

// GpStatus WINGDIPAPI GdipTransformMatrixPointsI(GpMatrix*,GpPoint*,INT)
#if 0
HB_FUNC( WAGDIPTRANSFORMMATRIXPOINTSI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipTransformMatrixPointsI(wa_par_GpMatrix(1), vec.data(), wa_par_INT(3)));
  // TODO: copy values from vector to array
}
#endif

// GpStatus WINGDIPAPI GdipVectorTransformMatrixPoints(GpMatrix*,GpPointF*,INT)

// GpStatus WINGDIPAPI GdipVectorTransformMatrixPointsI(GpMatrix*,GpPoint*,INT)

// GpStatus WINGDIPAPI GdipGetMatrixElements(GDIPCONST GpMatrix*,REAL*)

// GpStatus WINGDIPAPI GdipIsMatrixInvertible(GDIPCONST GpMatrix*,BOOL*)
HB_FUNC( WAGDIPISMATRIXINVERTIBLE )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsMatrixInvertible(wa_par_GpMatrix(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipIsMatrixIdentity(GDIPCONST GpMatrix*,BOOL*)
HB_FUNC( WAGDIPISMATRIXIDENTITY )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsMatrixIdentity(wa_par_GpMatrix(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipIsMatrixEqual(GDIPCONST GpMatrix*,GDIPCONST GpMatrix*,BOOL*)
HB_FUNC( WAGDIPISMATRIXEQUAL )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsMatrixEqual(wa_par_GpMatrix(1), wa_par_GpMatrix(2), &b));
  wa_stor_BOOL(b, 3);
}

///////////////////////////////////////////////////////////////////////////////
// Metafile functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipGetMetafileHeaderFromEmf(HENHMETAFILE,MetafileHeader*)

// GpStatus WINGDIPAPI GdipGetMetafileHeaderFromFile(GDIPCONST WCHAR*,MetafileHeader*)

// GpStatus WINGDIPAPI GdipGetMetafileHeaderFromStream(IStream*,MetafileHeader*)

// GpStatus WINGDIPAPI GdipGetMetafileHeaderFromMetafile(GpMetafile*,MetafileHeader*)

// GpStatus WINGDIPAPI GdipGetHemfFromMetafile(GpMetafile*,HENHMETAFILE*)
HB_FUNC( WAGDIPGETHEMFFROMMETAFILE )
{
  HENHMETAFILE p{};
  wa_ret_GpStatus(GdipGetHemfFromMetafile(wa_par_GpMetafile(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateStreamOnFile(GDIPCONST WCHAR*,UINT,IStream**)
HB_FUNC( WAGDIPCREATESTREAMONFILE )
{
  IStream * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateStreamOnFile(HB_PARSTR(1, &str, nullptr), wa_par_UINT(2), &p));
  hb_strfree(str);
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateMetafileFromWmf(HMETAFILE,BOOL,GDIPCONST WmfPlaceableFileHeader*,GpMetafile**)
HB_FUNC( WAGDIPCREATEMETAFILEFROMWMF )
{
  GpMetafile * p{};
  wa_ret_GpStatus(GdipCreateMetafileFromWmf(wa_par_HMETAFILE(1), wa_par_BOOL(2), wa_par_WmfPlaceableFileHeader(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipCreateMetafileFromEmf(HENHMETAFILE,BOOL,GpMetafile**)
HB_FUNC( WAGDIPCREATEMETAFILEFROMEMF )
{
  GpMetafile * p{};
  wa_ret_GpStatus(GdipCreateMetafileFromEmf(wa_par_HENHMETAFILE(1), wa_par_BOOL(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateMetafileFromFile(GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPCREATEMETAFILEFROMFILE )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateMetafileFromFile(HB_PARSTR(1, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateMetafileFromWmfFile(GDIPCONST WCHAR*,GDIPCONST WmfPlaceableFileHeader*,GpMetafile**)
HB_FUNC( WAGDIPCREATEMETAFILEFROMWMFFILE )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipCreateMetafileFromWmfFile(HB_PARSTR(1, &str, nullptr), wa_par_WmfPlaceableFileHeader(2), &p));
  hb_strfree(str);
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateMetafileFromStream(IStream*,GpMetafile**)
HB_FUNC( WAGDIPCREATEMETAFILEFROMSTREAM )
{
  GpMetafile * p{};
  wa_ret_GpStatus(GdipCreateMetafileFromStream(wa_par_IStream(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipRecordMetafile(HDC,EmfType,GDIPCONST GpRectF*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILE )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipRecordMetafile(wa_par_HDC(1), wa_par_GpEmfType(2), wa_par_GpRectF(3), wa_par_MetafileFrameUnit(4), HB_PARSTR(5, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipRecordMetafileI(HDC,EmfType,GDIPCONST GpRect*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILEI )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipRecordMetafileI(wa_par_HDC(1), wa_par_GpEmfType(2), wa_par_GpRect(3), wa_par_MetafileFrameUnit(4), HB_PARSTR(5, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 6);
}

// GpStatus WINGDIPAPI GdipRecordMetafileFileName(GDIPCONST WCHAR*,HDC,EmfType,GDIPCONST GpRectF*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILEFILENAME )
{
  GpMetafile * p{};
  void * str1{};
  void * str2{};
  wa_ret_GpStatus(GdipRecordMetafileFileName(HB_PARSTR(1, &str1, nullptr), wa_par_HDC(2), wa_par_GpEmfType(3), wa_par_GpRectF(4), wa_par_MetafileFrameUnit(5), HB_PARSTR(6, &str2, nullptr), &p));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipRecordMetafileFileNameI(GDIPCONST WCHAR*,HDC,EmfType,GDIPCONST GpRect*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILEFILENAMEI )
{
  GpMetafile * p{};
  void * str1{};
  void * str2{};
  wa_ret_GpStatus(GdipRecordMetafileFileNameI(HB_PARSTR(1, &str1, nullptr), wa_par_HDC(2), wa_par_GpEmfType(3), wa_par_GpRect(4), wa_par_MetafileFrameUnit(5), HB_PARSTR(6, &str2, nullptr), &p));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipRecordMetafileStream(IStream*,HDC,EmfType,GDIPCONST GpRectF*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILESTREAM )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipRecordMetafileStream(wa_par_IStream(1), wa_par_HDC(2), wa_par_GpEmfType(3), wa_par_GpRectF(4), wa_par_MetafileFrameUnit(5), HB_PARSTR(6, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipRecordMetafileStreamI(IStream*,HDC,EmfType,GDIPCONST GpRect*,MetafileFrameUnit,GDIPCONST WCHAR*,GpMetafile**)
HB_FUNC( WAGDIPRECORDMETAFILESTREAMI )
{
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipRecordMetafileStreamI(wa_par_IStream(1), wa_par_HDC(2), wa_par_GpEmfType(3), wa_par_GpRect(4), wa_par_MetafileFrameUnit(5), HB_PARSTR(6, &str, nullptr), &p));
  hb_strfree(str);
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipPlayMetafileRecord(GDIPCONST GpMetafile*,EmfPlusRecordType,UINT,UINT,GDIPCONST BYTE*)
HB_FUNC( WAGDIPPLAYMETAFILERECORD )
{
  wa_ret_GpStatus(GdipPlayMetafileRecord(wa_par_GpMetafile(1), wa_par_GpEmfPlusRecordType(2), wa_par_UINT(3), wa_par_UINT(4), static_cast<GDIPCONST BYTE *>(hb_parptr(5))));
}

// GpStatus WINGDIPAPI GdipSetMetafileDownLevelRasterizationLimit(GpMetafile*,UINT)
HB_FUNC( WAGDIPSETMETAFILEDOWNLEVELRASTERIZATIONLIMIT )
{
  wa_ret_GpStatus(GdipSetMetafileDownLevelRasterizationLimit(wa_par_GpMetafile(1), wa_par_UINT(2)));
}

// GpStatus WINGDIPAPI GdipGetMetafileDownLevelRasterizationLimit(GDIPCONST GpMetafile*,UINT*)
HB_FUNC( WAGDIPGETMETAFILEDOWNLEVELRASTERIZATIONLIMIT )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetMetafileDownLevelRasterizationLimit(wa_par_GpMetafile(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipConvertToEmfPlus(GDIPCONST GpGraphics*,GpMetafile*,BOOL*,EmfType,GDIPCONST WCHAR*,GpMetafile**)
#if 0
HB_FUNC( WAGDIPCONVERTTOEMFPLUS )
{
  BOOL b{};
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipConvertToEmfPlus(wa_par_GpGraphics(1), wa_par_GpMetafile(2), &b, wa_par_GpEmfType(4), HB_PARSTR(5, &str, nullptr), &p));
  hb_strfree(str);
  wa_stor_BOOL(b, 3);
  hb_storptr(p, 6);
}
#endif

// GpStatus WINGDIPAPI GdipConvertToEmfPlusToFile(GDIPCONST GpGraphics*,GpMetafile*,BOOL*,GDIPCONST WCHAR*,EmfType,GDIPCONST WCHAR*,GpMetafile**)
#if 0
HB_FUNC( WAGDIPCONVERTTOEMFPLUSTOFILE )
{
  BOOL b{};
  GpMetafile * p{};
  void * str1{};
  void * str2{};
  wa_ret_GpStatus(GdipConvertToEmfPlusToFile(wa_par_GpGraphics(1), wa_par_GpMetafile(2), &b, HB_PARSTR(4, &str1, nullptr), wa_par_GpEmfType(5), HB_PARSTR(6, &str2, nullptr), &p));
  hb_strfree(str1);
  hb_strfree(str2);
  wa_stor_BOOL(b, 3);
  hb_storptr(p, 7);
}
#endif

// GpStatus WINGDIPAPI GdipConvertToEmfPlusToStream(GDIPCONST GpGraphics*,GpMetafile*,BOOL*,IStream*,EmfType,GDIPCONST WCHAR*,GpMetafile**)
#if 0
HB_FUNC( WAGDIPCONVERTTOEMFPLUSTOSTREAM )
{
  BOOL b{};
  GpMetafile * p{};
  void * str{};
  wa_ret_GpStatus(GdipConvertToEmfPlusToStream(wa_par_GpGraphics(1), wa_par_GpMetafile(2), &b, wa_par_IStream(4), wa_par_GpEmfType(5), HB_PARSTR(6, &str, nullptr), &p));
  hb_strfree(str);
  wa_stor_BOOL(b, 3);
  hb_storptr(p, 7);
}
#endif

// UINT WINGDIPAPI GdipEmfToWmfBits(HENHMETAFILE,UINT,LPBYTE,INT,INT)
#if 0
HB_FUNC( WAGDIPEMFTOWMFBITS )
{
  wa_ret_UINT(GdipEmfToWmfBits(wa_par_HENHMETAFILE(1), wa_par_UINT(2), LPBYTE, wa_par_INT(4), wa_par_INT(5)));
}
#endif

///////////////////////////////////////////////////////////////////////////////
// PathGradientBrush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreatePathGradient(GDIPCONST GpPointF*,INT,GpWrapMode,GpPathGradient**)
HB_FUNC( WAGDIPCREATEPATHGRADIENT )
{
  std::vector<GpPointF> vec = GpPointFArrayToVector(hb_param(1, Harbour::Item::ARRAY));
  GpPathGradient * p{};
  wa_ret_GpStatus(GdipCreatePathGradient(vec.data(), wa_par_INT(2), wa_par_GpWrapMode(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipCreatePathGradientI(GDIPCONST GpPoint*,INT,GpWrapMode,GpPathGradient**)
HB_FUNC( WAGDIPCREATEPATHGRADIENTI )
{
  std::vector<GpPoint> vec = GpPointArrayToVector(hb_param(1, Harbour::Item::ARRAY));
  GpPathGradient * p{};
  wa_ret_GpStatus(GdipCreatePathGradientI(vec.data(), wa_par_INT(2), wa_par_GpWrapMode(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipCreatePathGradientFromPath(GDIPCONST GpPath*,GpPathGradient**)
HB_FUNC( WAGDIPCREATEPATHGRADIENTFROMPATH )
{
  GpPathGradient * p{};
  wa_ret_GpStatus(GdipCreatePathGradientFromPath(wa_par_GpPath(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetPathGradientCenterColor(GpPathGradient*,ARGB*)
HB_FUNC( WAGDIPGETPATHGRADIENTCENTERCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetPathGradientCenterColor(wa_par_GpPathGradient(1), &argb));
  wa_stor_ARGB(argb, 2);
}

// GpStatus WINGDIPAPI GdipSetPathGradientCenterColor(GpPathGradient*,ARGB)
HB_FUNC( WAGDIPSETPATHGRADIENTCENTERCOLOR )
{
  wa_ret_GpStatus(GdipSetPathGradientCenterColor(wa_par_GpPathGradient(1), wa_par_ARGB(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientSurroundColorsWithCount(GpPathGradient*,ARGB*,INT*)

// GpStatus WINGDIPAPI GdipSetPathGradientSurroundColorsWithCount(GpPathGradient*,GDIPCONST ARGB*,INT*)
HB_FUNC( WAGDIPSETPATHGRADIENTSURROUNDCOLORSWITHCOUNT )
{
  std::vector<ARGB> vec = ARGBArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  INT i = wa_par_INT(3);
  wa_ret_GpStatus(GdipSetPathGradientSurroundColorsWithCount(wa_par_GpPathGradient(1), vec.data(), &i));
  wa_stor_INT(i, 3);
}

// GpStatus WINGDIPAPI GdipGetPathGradientPath(GpPathGradient*,GpPath*)
HB_FUNC( WAGDIPGETPATHGRADIENTPATH )
{
  wa_ret_GpStatus(GdipGetPathGradientPath(wa_par_GpPathGradient(1), wa_par_GpPath(2)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientPath(GpPathGradient*,GDIPCONST GpPath*)
HB_FUNC( WAGDIPSETPATHGRADIENTPATH )
{
  wa_ret_GpStatus(GdipSetPathGradientPath(wa_par_GpPathGradient(1), wa_par_GpPath(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientCenterPoint(GpPathGradient*,GpPointF*)
HB_FUNC( WAGDIPGETPATHGRADIENTCENTERPOINT )
{
  wa_ret_GpStatus(GdipGetPathGradientCenterPoint(wa_par_GpPathGradient(1), wa_par_GpPointF(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientCenterPointI(GpPathGradient*,GpPoint*)
HB_FUNC( WAGDIPGETPATHGRADIENTCENTERPOINTI )
{
  wa_ret_GpStatus(GdipGetPathGradientCenterPointI(wa_par_GpPathGradient(1), wa_par_GpPoint(2)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientCenterPoint(GpPathGradient*,GDIPCONST GpPointF*)
HB_FUNC( WAGDIPSETPATHGRADIENTCENTERPOINT )
{
  wa_ret_GpStatus(GdipSetPathGradientCenterPoint(wa_par_GpPathGradient(1), wa_par_GpPointF(2)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientCenterPointI(GpPathGradient*,GDIPCONST GpPoint*)
HB_FUNC( WAGDIPSETPATHGRADIENTCENTERPOINTI )
{
  wa_ret_GpStatus(GdipSetPathGradientCenterPointI(wa_par_GpPathGradient(1), wa_par_GpPoint(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientRect(GpPathGradient*,GpRectF*)
HB_FUNC( WAGDIPGETPATHGRADIENTRECT )
{
  wa_ret_GpStatus(GdipGetPathGradientRect(wa_par_GpPathGradient(1), wa_par_GpRectF(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientRectI(GpPathGradient*,GpRect*)
HB_FUNC( WAGDIPGETPATHGRADIENTRECTI )
{
  wa_ret_GpStatus(GdipGetPathGradientRectI(wa_par_GpPathGradient(1), wa_par_GpRect(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientPointCount(GpPathGradient*,INT*)
HB_FUNC( WAGDIPGETPATHGRADIENTPOINTCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPathGradientPointCount(wa_par_GpPathGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetPathGradientSurroundColorCount(GpPathGradient*,INT*)
HB_FUNC( WAGDIPGETPATHGRADIENTSURROUNDCOLORCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPathGradientSurroundColorCount(wa_par_GpPathGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetPathGradientGammaCorrection(GpPathGradient*,BOOL)
HB_FUNC( WAGDIPSETPATHGRADIENTGAMMACORRECTION )
{
  wa_ret_GpStatus(GdipSetPathGradientGammaCorrection(wa_par_GpPathGradient(1), wa_par_BOOL(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientGammaCorrection(GpPathGradient*,BOOL*)
HB_FUNC( WAGDIPGETPATHGRADIENTGAMMACORRECTION )
{
  BOOL b{};
  wa_ret_GpStatus(GdipGetPathGradientGammaCorrection(wa_par_GpPathGradient(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipGetPathGradientBlendCount(GpPathGradient*,INT*)
HB_FUNC( WAGDIPGETPATHGRADIENTBLENDCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPathGradientBlendCount(wa_par_GpPathGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetPathGradientBlend(GpPathGradient*,REAL*,REAL*,INT)

// GpStatus WINGDIPAPI GdipSetPathGradientBlend(GpPathGradient*,GDIPCONST REAL*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETPATHGRADIENTBLEND )
{
  std::vector<REAL> vec1 = REALArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  std::vector<REAL> vec2 = REALArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetPathGradientBlend(wa_par_GpPathGradient(1), vec1.data(), vec2.data(), wa_par_INT(4)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientPresetBlendCount(GpPathGradient*,INT*)
HB_FUNC( WAGDIPGETPATHGRADIENTPRESETBLENDCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPathGradientPresetBlendCount(wa_par_GpPathGradient(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipGetPathGradientPresetBlend(GpPathGradient*,ARGB*,REAL*,INT)

// GpStatus WINGDIPAPI GdipSetPathGradientPresetBlend(GpPathGradient*,GDIPCONST ARGB*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETPATHGRADIENTPRESETBLEND )
{
  std::vector<ARGB> vec1 = ARGBArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  std::vector<REAL> vec2 = REALArrayToVector(hb_param(3, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetPathGradientPresetBlend(wa_par_GpPathGradient(1), vec1.data(), vec2.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientSigmaBlend(GpPathGradient*,REAL,REAL)
HB_FUNC( WAGDIPSETPATHGRADIENTSIGMABLEND )
{
  wa_ret_GpStatus(GdipSetPathGradientSigmaBlend(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientLinearBlend(GpPathGradient*,REAL,REAL)
HB_FUNC( WAGDIPSETPATHGRADIENTLINEARBLEND )
{
  wa_ret_GpStatus(GdipSetPathGradientLinearBlend(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientWrapMode(GpPathGradient*,GpWrapMode*)
HB_FUNC( WAGDIPGETPATHGRADIENTWRAPMODE )
{
  GpWrapMode wm{};
  wa_ret_GpStatus(GdipGetPathGradientWrapMode(wa_par_GpPathGradient(1), &wm));
  hb_storni(wm, 2);
}

// GpStatus WINGDIPAPI GdipSetPathGradientWrapMode(GpPathGradient*,GpWrapMode)
HB_FUNC( WAGDIPSETPATHGRADIENTWRAPMODE )
{
  wa_ret_GpStatus(GdipSetPathGradientWrapMode(wa_par_GpPathGradient(1), wa_par_GpWrapMode(2)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientTransform(GpPathGradient*,GpMatrix*)
HB_FUNC( WAGDIPGETPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipGetPathGradientTransform(wa_par_GpPathGradient(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipSetPathGradientTransform(GpPathGradient*,GpMatrix*)
HB_FUNC( WAGDIPSETPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipSetPathGradientTransform(wa_par_GpPathGradient(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetPathGradientTransform(GpPathGradient*)
HB_FUNC( WAGDIPRESETPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipResetPathGradientTransform(wa_par_GpPathGradient(1)));
}

// GpStatus WINGDIPAPI GdipMultiplyPathGradientTransform(GpPathGradient*,GDIPCONST GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipMultiplyPathGradientTransform(wa_par_GpPathGradient(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslatePathGradientTransform(GpPathGradient*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATEPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipTranslatePathGradientTransform(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScalePathGradientTransform(GpPathGradient*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALEPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipScalePathGradientTransform(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotatePathGradientTransform(GpPathGradient*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATEPATHGRADIENTTRANSFORM )
{
  wa_ret_GpStatus(GdipRotatePathGradientTransform(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipGetPathGradientFocusScales(GpPathGradient*,REAL*,REAL*)
HB_FUNC( WAGDIPGETPATHGRADIENTFOCUSSCALES )
{
  REAL r1{};
  REAL r2{};
  wa_ret_GpStatus(GdipGetPathGradientFocusScales(wa_par_GpPathGradient(1), &r1, &r2));
  wa_stor_REAL(r1, 2);
  wa_stor_REAL(r2, 3);
}

// GpStatus WINGDIPAPI GdipSetPathGradientFocusScales(GpPathGradient*,REAL,REAL)
HB_FUNC( WAGDIPSETPATHGRADIENTFOCUSSCALES )
{
  wa_ret_GpStatus(GdipSetPathGradientFocusScales(wa_par_GpPathGradient(1), wa_par_REAL(2), wa_par_REAL(3)));
}

///////////////////////////////////////////////////////////////////////////////
// PathIterator functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreatePathIter(GpPathIterator**,GpPath*)
HB_FUNC( WAGDIPCREATEPATHITER )
{
  GpPathIterator * p{};
  wa_ret_GpStatus(GdipCreatePathIter(&p, wa_par_GpPath(1)));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipDeletePathIter(GpPathIterator*)
HB_FUNC( WAGDIPDELETEPATHITER )
{
  wa_ret_GpStatus(GdipDeletePathIter(wa_par_GpPathIterator(1)));
}

// GpStatus WINGDIPAPI GdipPathIterNextSubpath(GpPathIterator*,INT*,INT*,INT*,BOOL*)
HB_FUNC( WAGDIPPATHITERNEXTSUBPATH )
{
  INT i1{};
  INT i2{};
  INT i3{};
  BOOL b{};
  wa_ret_GpStatus(GdipPathIterNextSubpath(wa_par_GpPathIterator(1), &i1, &i2, &i3, &b));
  wa_stor_INT(i1, 2);
  wa_stor_INT(i1, 3);
  wa_stor_INT(i1, 4);
  wa_stor_BOOL(b, 5);
}

// GpStatus WINGDIPAPI GdipPathIterNextSubpathPath(GpPathIterator*,INT*,GpPath*,BOOL*)
HB_FUNC( WAGDIPPATHITERNEXTSUBPATHPATH )
{
  INT i{};
  BOOL b{};
  wa_ret_GpStatus(GdipPathIterNextSubpathPath(wa_par_GpPathIterator(1), &i, wa_par_GpPath(3), &b));
  wa_stor_INT(i, 2);
  wa_stor_BOOL(b, 4);
}

// GpStatus WINGDIPAPI GdipPathIterNextPathType(GpPathIterator*,INT*,BYTE*,INT*,INT*)
HB_FUNC( WAGDIPPATHITERNEXTPATHTYPE )
{
  INT i1{};
  BYTE b{};
  INT i2{};
  INT i3{};
  wa_ret_GpStatus(GdipPathIterNextPathType(wa_par_GpPathIterator(1), &i1, &b, &i2, &i3));
  wa_stor_INT(i1, 2);
  wa_stor_BYTE(b, 3);
  wa_stor_INT(i2, 4);
  wa_stor_INT(i3, 5);
}

// GpStatus WINGDIPAPI GdipPathIterNextMarker(GpPathIterator*,INT*,INT*,INT*)
HB_FUNC( WAGDIPPATHITERNEXTMARKER )
{
  INT i1{};
  INT i2{};
  INT i3{};
  wa_ret_GpStatus(GdipPathIterNextMarker(wa_par_GpPathIterator(1), &i1, &i2, &i3));
  wa_stor_INT(i1, 2);
  wa_stor_INT(i2, 3);
  wa_stor_INT(i3, 4);
}

// GpStatus WINGDIPAPI GdipPathIterNextMarkerPath(GpPathIterator*,INT*,GpPath*)
HB_FUNC( WAGDIPPATHITERNEXTMARKERPATH )
{
  INT i{};
  wa_ret_GpStatus(GdipPathIterNextMarkerPath(wa_par_GpPathIterator(1), &i, wa_par_GpPath(3)));
  wa_stor_INT(i, 3);
}

// GpStatus WINGDIPAPI GdipPathIterGetCount(GpPathIterator*,INT*)
HB_FUNC( WAGDIPPATHITERGETCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipPathIterGetCount(wa_par_GpPathIterator(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipPathIterGetSubpathCount(GpPathIterator*,INT*)
HB_FUNC( WAGDIPPATHITERGETSUBPATHCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipPathIterGetSubpathCount(wa_par_GpPathIterator(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipPathIterIsValid(GpPathIterator*,BOOL*)
HB_FUNC( WAGDIPPATHITERISVALID )
{
  BOOL b{};
  wa_ret_GpStatus(GdipPathIterIsValid(wa_par_GpPathIterator(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipPathIterHasCurve(GpPathIterator*,BOOL*)
HB_FUNC( WAGDIPPATHITERHASCURVE )
{
  BOOL b{};
  wa_ret_GpStatus(GdipPathIterHasCurve(wa_par_GpPathIterator(1), &b));
  wa_stor_BOOL(b, 2);
}

// GpStatus WINGDIPAPI GdipPathIterRewind(GpPathIterator*)
HB_FUNC( WAGDIPPATHITERREWIND )
{
  wa_ret_GpStatus(GdipPathIterRewind(wa_par_GpPathIterator(1)));
}

// GpStatus WINGDIPAPI GdipPathIterEnumerate(GpPathIterator*,INT*,GpPointF*,BYTE*,INT)

// GpStatus WINGDIPAPI GdipPathIterCopyData(GpPathIterator*,INT*,GpPointF*,BYTE*,INT,INT)

///////////////////////////////////////////////////////////////////////////////
// Pen functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreatePen1(ARGB,REAL,GpUnit,GpPen**)
HB_FUNC( WAGDIPCREATEPEN1 )
{
  GpPen * p{};
  wa_ret_GpStatus(GdipCreatePen1(wa_par_ARGB(1), wa_par_REAL(2), wa_par_GpUnit(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipCreatePen2(GpBrush*, REAL, GpUnit, GpPen**)
HB_FUNC( WAGDIPCREATEPEN2 )
{
  GpPen * p{};
  wa_ret_GpStatus(GdipCreatePen2(wa_par_GpBrush(1), wa_par_REAL(2), wa_par_GpUnit(3), &p));
  hb_storptr(p, 4);
}

// GpStatus WINGDIPAPI GdipClonePen(GpPen*,GpPen**)
HB_FUNC( WAGDIPCLONEPEN )
{
  GpPen * p{};
  wa_ret_GpStatus(GdipClonePen(wa_par_GpPen(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeletePen(GpPen*)
HB_FUNC( WAGDIPDELETEPEN )
{
  wa_ret_GpStatus(GdipDeletePen(wa_par_GpPen(1)));
}

// GpStatus WINGDIPAPI GdipSetPenWidth(GpPen*,REAL)
HB_FUNC( WAGDIPSETPENWIDTH )
{
  wa_ret_GpStatus(GdipSetPenWidth(wa_par_GpPen(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetPenWidth(GpPen*,REAL*)
HB_FUNC( WAGDIPGETPENWIDTH )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetPenWidth(wa_par_GpPen(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetPenUnit(GpPen*,GpUnit)
HB_FUNC( WAGDIPSETPENUNIT )
{
  wa_ret_GpStatus(GdipSetPenUnit(wa_par_GpPen(1), wa_par_GpUnit(2)));
}

// GpStatus WINGDIPAPI GdipGetPenUnit(GpPen*,GpUnit*)
HB_FUNC( WAGDIPGETPENUNIT )
{
  GpUnit u{};
  wa_ret_GpStatus(GdipGetPenUnit(wa_par_GpPen(1), &u));
  hb_storni(u, 2);
}

// GpStatus WINGDIPAPI GdipSetPenLineCap197819(GpPen*,GpLineCap,GpLineCap,GpDashCap)
HB_FUNC( WAGDIPSETPENLINECAP197819 )
{
  wa_ret_GpStatus(GdipSetPenLineCap197819(wa_par_GpPen(1), wa_par_GpLineCap(2), wa_par_GpLineCap(3), wa_par_GpDashCap(4)));
}

// GpStatus WINGDIPAPI GdipSetPenStartCap(GpPen*,GpLineCap)
HB_FUNC( WAGDIPSETPENSTARTCAP )
{
  wa_ret_GpStatus(GdipSetPenStartCap(wa_par_GpPen(1), wa_par_GpLineCap(2)));
}

// GpStatus WINGDIPAPI GdipSetPenEndCap(GpPen*,GpLineCap)
HB_FUNC( WAGDIPSETPENENDCAP )
{
  wa_ret_GpStatus(GdipSetPenEndCap(wa_par_GpPen(1), wa_par_GpLineCap(2)));
}

// GpStatus WINGDIPAPI GdipSetPenDashCap197819(GpPen*,GpDashCap)
HB_FUNC( WAGDIPSETPENDASHCAP197819 )
{
  wa_ret_GpStatus(GdipSetPenDashCap197819(wa_par_GpPen(1), wa_par_GpDashCap(2)));
}

// GpStatus WINGDIPAPI GdipGetPenStartCap(GpPen*,GpLineCap*)
HB_FUNC( WAGDIPGETPENSTARTCAP )
{
  GpLineCap lc{};
  wa_ret_GpStatus(GdipGetPenStartCap(wa_par_GpPen(1), &lc));
  hb_storni(lc, 2);
}

// GpStatus WINGDIPAPI GdipGetPenEndCap(GpPen*,GpLineCap*)
HB_FUNC( WAGDIPGETPENENDCAP )
{
  GpLineCap lc{};
  wa_ret_GpStatus(GdipGetPenEndCap(wa_par_GpPen(1), &lc));
  hb_storni(lc, 2);
}

// GpStatus WINGDIPAPI GdipGetPenDashCap197819(GpPen*,GpDashCap*)
HB_FUNC( WAGDIPGETPENDASHCAP197819 )
{
  GpDashCap dc{};
  wa_ret_GpStatus(GdipGetPenDashCap197819(wa_par_GpPen(1), &dc));
  hb_storni(dc, 2);
}

// GpStatus WINGDIPAPI GdipSetPenLineJoin(GpPen*,GpLineJoin)
HB_FUNC( WAGDIPSETPENLINEJOIN )
{
  wa_ret_GpStatus(GdipSetPenLineJoin(wa_par_GpPen(1), wa_par_GpLineJoin(2)));
}

// GpStatus WINGDIPAPI GdipGetPenLineJoin(GpPen*,GpLineJoin*)
HB_FUNC( WAGDIPGETPENLINEJOIN )
{
  GpLineJoin lj{};
  wa_ret_GpStatus(GdipGetPenLineJoin(wa_par_GpPen(1), &lj));
  hb_storni(lj, 2);
}

// GpStatus WINGDIPAPI GdipSetPenCustomStartCap(GpPen*,GpCustomLineCap*)
HB_FUNC( WAGDIPSETPENCUSTOMSTARTCAP )
{
  wa_ret_GpStatus(GdipSetPenCustomStartCap(wa_par_GpPen(1), wa_par_GpCustomLineCap(2)));
}

// GpStatus WINGDIPAPI GdipGetPenCustomStartCap(GpPen*,GpCustomLineCap**)
HB_FUNC( WAGDIPGETPENCUSTOMSTARTCAP )
{
  GpCustomLineCap * p{};
  wa_ret_GpStatus(GdipGetPenCustomStartCap(wa_par_GpPen(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipSetPenCustomEndCap(GpPen*,GpCustomLineCap*)
HB_FUNC( WAGDIPSETPENCUSTOMENDCAP )
{
  wa_ret_GpStatus(GdipSetPenCustomEndCap(wa_par_GpPen(1), wa_par_GpCustomLineCap(2)));
}

// GpStatus WINGDIPAPI GdipGetPenCustomEndCap(GpPen*,GpCustomLineCap**)
HB_FUNC( WAGDIPGETPENCUSTOMENDCAP )
{
  GpCustomLineCap * p{};
  wa_ret_GpStatus(GdipGetPenCustomEndCap(wa_par_GpPen(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipSetPenMiterLimit(GpPen*,REAL)
HB_FUNC( WAGDIPSETPENMITERLIMIT )
{
  wa_ret_GpStatus(GdipSetPenMiterLimit(wa_par_GpPen(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetPenMiterLimit(GpPen*,REAL*)
HB_FUNC( WAGDIPGETPENMITERLIMIT )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetPenMiterLimit(wa_par_GpPen(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetPenMode(GpPen*,GpPenAlignment)
HB_FUNC( WAGDIPSETPENMODE )
{
  wa_ret_GpStatus(GdipSetPenMode(wa_par_GpPen(1), wa_par_GpPenAlignment(2)));
}

// GpStatus WINGDIPAPI GdipGetPenMode(GpPen*,GpPenAlignment*)
HB_FUNC( WAGDIPGETPENMODE )
{
  GpPenAlignment pa{};
  wa_ret_GpStatus(GdipGetPenMode(wa_par_GpPen(1), &pa));
  hb_storni(pa, 2);
}

// GpStatus WINGDIPAPI GdipSetPenTransform(GpPen*,GpMatrix*)
HB_FUNC( WAGDIPSETPENTRANSFORM )
{
  wa_ret_GpStatus(GdipSetPenTransform(wa_par_GpPen(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipGetPenTransform(GpPen*,GpMatrix*)
HB_FUNC( WAGDIPGETPENTRANSFORM )
{
  wa_ret_GpStatus(GdipGetPenTransform(wa_par_GpPen(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetPenTransform(GpPen*)
HB_FUNC( WAGDIPRESETPENTRANSFORM )
{
  wa_ret_GpStatus(GdipResetPenTransform(wa_par_GpPen(1)));
}

// GpStatus WINGDIPAPI GdipMultiplyPenTransform(GpPen*,GDIPCONST GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYPENTRANSFORM )
{
  wa_ret_GpStatus(GdipMultiplyPenTransform(wa_par_GpPen(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslatePenTransform(GpPen*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATEPENTRANSFORM )
{
  wa_ret_GpStatus(GdipTranslatePenTransform(wa_par_GpPen(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScalePenTransform(GpPen*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALEPENTRANSFORM )
{
  wa_ret_GpStatus(GdipScalePenTransform(wa_par_GpPen(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotatePenTransform(GpPen*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATEPENTRANSFORM )
{
  wa_ret_GpStatus(GdipRotatePenTransform(wa_par_GpPen(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipSetPenColor(GpPen*,ARGB)
HB_FUNC( WAGDIPSETPENCOLOR )
{
  wa_ret_GpStatus(GdipSetPenColor(wa_par_GpPen(1), wa_par_ARGB(2)));
}

// GpStatus WINGDIPAPI GdipGetPenColor(GpPen*,ARGB*)
HB_FUNC( WAGDIPGETPENCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetPenColor(wa_par_GpPen(1), &argb));
  wa_stor_ARGB(argb, 2);
}

// GpStatus WINGDIPAPI GdipSetPenBrushFill(GpPen*,GpBrush*)
HB_FUNC( WAGDIPSETPENBRUSHFILL )
{
  wa_ret_GpStatus(GdipSetPenBrushFill(wa_par_GpPen(1), wa_par_GpBrush(2)));
}

// GpStatus WINGDIPAPI GdipGetPenBrushFill(GpPen*,GpBrush**)
HB_FUNC( WAGDIPGETPENBRUSHFILL )
{
  GpBrush * p{};
  wa_ret_GpStatus(GdipGetPenBrushFill(wa_par_GpPen(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipGetPenFillType(GpPen*,GpPenType*)
HB_FUNC( WAGDIPGETPENFILLTYPE )
{
  GpPenType pt{};
  wa_ret_GpStatus(GdipGetPenFillType(wa_par_GpPen(1), &pt));
  hb_storni(pt, 2);
}

// GpStatus WINGDIPAPI GdipGetPenDashStyle(GpPen*,GpDashStyle*)
HB_FUNC( WAGDIPGETPENDASHSTYLE )
{
  GpDashStyle ds{};
  wa_ret_GpStatus(GdipGetPenDashStyle(wa_par_GpPen(1), &ds));
  hb_storni(ds, 2);
}

// GpStatus WINGDIPAPI GdipSetPenDashStyle(GpPen*,GpDashStyle)
HB_FUNC( WAGDIPSETPENDASHSTYLE )
{
  wa_ret_GpStatus(GdipSetPenDashStyle(wa_par_GpPen(1), wa_par_GpDashStyle(2)));
}

// GpStatus WINGDIPAPI GdipGetPenDashOffset(GpPen*,REAL*)
HB_FUNC( WAGDIPGETPENDASHOFFSET )
{
  REAL r{};
  wa_ret_GpStatus(GdipGetPenDashOffset(wa_par_GpPen(1), &r));
  wa_stor_REAL(r, 2);
}

// GpStatus WINGDIPAPI GdipSetPenDashOffset(GpPen*,REAL)
HB_FUNC( WAGDIPSETPENDASHOFFSET )
{
  wa_ret_GpStatus(GdipSetPenDashOffset(wa_par_GpPen(1), wa_par_REAL(2)));
}

// GpStatus WINGDIPAPI GdipGetPenDashCount(GpPen*,INT*)
HB_FUNC( WAGDIPGETPENDASHCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPenDashCount(wa_par_GpPen(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetPenDashArray(GpPen*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETPENDASHARRAY )
{
  std::vector<REAL> vec = REALArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetPenDashArray(wa_par_GpPen(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipGetPenDashArray(GpPen*,REAL*,INT)

// GpStatus WINGDIPAPI GdipGetPenCompoundCount(GpPen*,INT*)
HB_FUNC( WAGDIPGETPENCOMPOUNDCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetPenCompoundCount(wa_par_GpPen(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetPenCompoundArray(GpPen*,GDIPCONST REAL*,INT)
HB_FUNC( WAGDIPSETPENCOMPOUNDARRAY )
{
  std::vector<REAL> vec = REALArrayToVector(hb_param(2, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetPenCompoundArray(wa_par_GpPen(1), vec.data(), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipGetPenCompoundArray(GpPen*,REAL*,INT)

///////////////////////////////////////////////////////////////////////////////
// Region functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateRegion(GpRegion**)
HB_FUNC( WAGDIPCREATEREGION )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCreateRegion(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipCreateRegionRect(GDIPCONST GpRectF*,GpRegion**)
HB_FUNC( WAGDIPCREATEREGIONRECT )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCreateRegionRect(wa_par_GpRectF(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateRegionRectI(GDIPCONST GpRect*,GpRegion**)
HB_FUNC( WAGDIPCREATEREGIONRECTI )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCreateRegionRectI(wa_par_GpRect(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateRegionPath(GpPath*,GpRegion**)
HB_FUNC( WAGDIPCREATEREGIONPATH )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCreateRegionPath(wa_par_GpPath(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCreateRegionRgnData(GDIPCONST BYTE*,INT,GpRegion**)

// GpStatus WINGDIPAPI GdipCreateRegionHrgn(HRGN,GpRegion**)
HB_FUNC( WAGDIPCREATEREGIONHRGN )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCreateRegionHrgn(wa_par_HRGN(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipCloneRegion(GpRegion*,GpRegion**)
HB_FUNC( WAGDIPCLONEREGION )
{
  GpRegion * p{};
  wa_ret_GpStatus(GdipCloneRegion(wa_par_GpRegion(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipDeleteRegion(GpRegion*)
HB_FUNC( WAGDIPDELETEREGION )
{
  wa_ret_GpStatus(GdipDeleteRegion(wa_par_GpRegion(1)));
}

// GpStatus WINGDIPAPI GdipSetInfinite(GpRegion*)
HB_FUNC( WAGDIPSETINFINITE )
{
  wa_ret_GpStatus(GdipSetInfinite(wa_par_GpRegion(1)));
}

// GpStatus WINGDIPAPI GdipSetEmpty(GpRegion*)
HB_FUNC( WAGDIPSETEMPTY )
{
  wa_ret_GpStatus(GdipSetEmpty(wa_par_GpRegion(1)));
}

// GpStatus WINGDIPAPI GdipCombineRegionRect(GpRegion*,GDIPCONST GpRectF*,CombineMode)
HB_FUNC( WAGDIPCOMBINEREGIONRECT )
{
  wa_ret_GpStatus(GdipCombineRegionRect(wa_par_GpRegion(1), wa_par_GpRectF(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipCombineRegionRectI(GpRegion*,GDIPCONST GpRect*,CombineMode)
HB_FUNC( WAGDIPCOMBINEREGIONRECTI )
{
  wa_ret_GpStatus(GdipCombineRegionRectI(wa_par_GpRegion(1), wa_par_GpRect(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipCombineRegionPath(GpRegion*,GpPath*,CombineMode)
HB_FUNC( WAGDIPCOMBINEREGIONPATH )
{
  wa_ret_GpStatus(GdipCombineRegionPath(wa_par_GpRegion(1), wa_par_GpPath(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipCombineRegionRegion(GpRegion*,GpRegion*,CombineMode)
HB_FUNC( WAGDIPCOMBINEREGIONREGION )
{
  wa_ret_GpStatus(GdipCombineRegionRegion(wa_par_GpRegion(1), wa_par_GpRegion(2), wa_par_GpCombineMode(3)));
}

// GpStatus WINGDIPAPI GdipTranslateRegion(GpRegion*,REAL,REAL)
HB_FUNC( WAGDIPTRANSLATEREGION )
{
  wa_ret_GpStatus(GdipTranslateRegion(wa_par_GpRegion(1), wa_par_REAL(2), wa_par_REAL(3)));
}

// GpStatus WINGDIPAPI GdipTranslateRegionI(GpRegion*,INT,INT)
HB_FUNC( WAGDIPTRANSLATEREGIONI )
{
  wa_ret_GpStatus(GdipTranslateRegionI(wa_par_GpRegion(1), wa_par_INT(2), wa_par_INT(3)));
}

// GpStatus WINGDIPAPI GdipTransformRegion(GpRegion*,GpMatrix*)
HB_FUNC( WAGDIPTRANSFORMREGION )
{
  wa_ret_GpStatus(GdipTransformRegion(wa_par_GpRegion(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipGetRegionBounds(GpRegion*,GpGraphics*,GpRectF*)
HB_FUNC( WAGDIPGETREGIONBOUNDS )
{
  wa_ret_GpStatus(GdipGetRegionBounds(wa_par_GpRegion(1), wa_par_GpGraphics(2), wa_par_GpRectF(3)));
}

// GpStatus WINGDIPAPI GdipGetRegionBoundsI(GpRegion*,GpGraphics*,GpRect*)
HB_FUNC( WAGDIPGETREGIONBOUNDSI )
{
  wa_ret_GpStatus(GdipGetRegionBoundsI(wa_par_GpRegion(1), wa_par_GpGraphics(2), wa_par_GpRect(3)));
}

// GpStatus WINGDIPAPI GdipGetRegionHRgn(GpRegion*,GpGraphics*,HRGN*)
HB_FUNC( WAGDIPGETREGIONHRGN )
{
  HRGN p{};
  wa_ret_GpStatus(GdipGetRegionHRgn(wa_par_GpRegion(1), wa_par_GpGraphics(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipIsEmptyRegion(GpRegion*,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISEMPTYREGION )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsEmptyRegion(wa_par_GpRegion(1), wa_par_GpGraphics(2), &b));
  wa_stor_BOOL(b, 3);
}

// GpStatus WINGDIPAPI GdipIsInfiniteRegion(GpRegion*,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISINFINITEREGION )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsInfiniteRegion(wa_par_GpRegion(1), wa_par_GpGraphics(2), &b));
  wa_stor_BOOL(b, 3);
}

// GpStatus WINGDIPAPI GdipIsEqualRegion(GpRegion*,GpRegion*,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISEQUALREGION )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsEqualRegion(wa_par_GpRegion(1), wa_par_GpRegion(2), wa_par_GpGraphics(3), &b));
  wa_stor_BOOL(b, 4);
}

// GpStatus WINGDIPAPI GdipGetRegionDataSize(GpRegion*,UINT*)
HB_FUNC( WAGDIPGETREGIONDATASIZE )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetRegionDataSize(wa_par_GpRegion(1), &ui));
  wa_stor_UINT(ui, 2);
}

// GpStatus WINGDIPAPI GdipGetRegionData(GpRegion*,BYTE*,UINT,UINT*)

// GpStatus WINGDIPAPI GdipIsVisibleRegionPoint(GpRegion*,REAL,REAL,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEREGIONPOINT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRegionPoint(wa_par_GpRegion(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpGraphics(4), &b));
  wa_stor_BOOL(b, 5);
}

// GpStatus WINGDIPAPI GdipIsVisibleRegionPointI(GpRegion*,INT,INT,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEREGIONPOINTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRegionPointI(wa_par_GpRegion(1), wa_par_INT(2), wa_par_INT(3), wa_par_GpGraphics(4), &b));
  wa_stor_BOOL(b, 5);
}

// GpStatus WINGDIPAPI GdipIsVisibleRegionRect(GpRegion*,REAL,REAL,REAL,REAL,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEREGIONRECT )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRegionRect(wa_par_GpRegion(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_GpGraphics(6), &b));
  wa_stor_BOOL(b, 7);
}

// GpStatus WINGDIPAPI GdipIsVisibleRegionRectI(GpRegion*,INT,INT,INT,INT,GpGraphics*,BOOL*)
HB_FUNC( WAGDIPISVISIBLEREGIONRECTI )
{
  BOOL b{};
  wa_ret_GpStatus(GdipIsVisibleRegionRectI(wa_par_GpRegion(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_GpGraphics(6), &b));
  wa_stor_BOOL(b, 7);
}

// GpStatus WINGDIPAPI GdipGetRegionScansCount(GpRegion*,UINT*,GpMatrix*)
HB_FUNC( WAGDIPGETREGIONSCANSCOUNT )
{
  UINT ui{};
  wa_ret_GpStatus(GdipGetRegionScansCount(wa_par_GpRegion(1), &ui, wa_par_GpMatrix(3)));
  wa_stor_UINT(ui, 3);
}

// GpStatus WINGDIPAPI GdipGetRegionScans(GpRegion*,GpRectF*,INT*,GpMatrix*)

// GpStatus WINGDIPAPI GdipGetRegionScansI(GpRegion*,GpRect*,INT*,GpMatrix*)

///////////////////////////////////////////////////////////////////////////////
// SolidBrush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateSolidFill(ARGB,GpSolidFill**)
HB_FUNC( WAGDIPCREATESOLIDFILL )
{
  GpSolidFill * p{};
  wa_ret_GpStatus(GdipCreateSolidFill(wa_par_ARGB(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipSetSolidFillColor(GpSolidFill*,ARGB)
HB_FUNC( WAGDIPSETSOLIDFILLCOLOR )
{
  wa_ret_GpStatus(GdipSetSolidFillColor(wa_par_GpSolidFill(1), wa_par_ARGB(2)));
}

// GpStatus WINGDIPAPI GdipGetSolidFillColor(GpSolidFill*,ARGB*)
HB_FUNC( WAGDIPGETSOLIDFILLCOLOR )
{
  ARGB argb{};
  wa_ret_GpStatus(GdipGetSolidFillColor(wa_par_GpSolidFill(1), &argb));
  wa_stor_ARGB(argb, 2);
}

///////////////////////////////////////////////////////////////////////////////
// StringFormat functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateStringFormat(INT,LANGID,GpStringFormat**)
HB_FUNC( WAGDIPCREATESTRINGFORMAT )
{
  GpStringFormat *p{};
  wa_ret_GpStatus(GdipCreateStringFormat(wa_par_INT(1), wa_par_LANGID(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipStringFormatGetGenericDefault(GpStringFormat**)
HB_FUNC( WAGDIPSTRINGFORMATGETGENERICDEFAULT )
{
  GpStringFormat * p{};
  wa_ret_GpStatus(GdipStringFormatGetGenericDefault(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipStringFormatGetGenericTypographic(GpStringFormat**)
HB_FUNC( WAGDIPSTRINGFORMATGETGENERICTYPOGRAPHIC )
{
  GpStringFormat * p{};
  wa_ret_GpStatus(GdipStringFormatGetGenericTypographic(&p));
  hb_storptr(p, 1);
}

// GpStatus WINGDIPAPI GdipDeleteStringFormat(GpStringFormat*)
HB_FUNC( WAGDIPDELETESTRINGFORMAT )
{
  wa_ret_GpStatus(GdipDeleteStringFormat(wa_par_GpStringFormat(1)));
}

// GpStatus WINGDIPAPI GdipCloneStringFormat(GDIPCONST GpStringFormat*,GpStringFormat**)
HB_FUNC( WAGDIPCLONESTRINGFORMAT )
{
  GpStringFormat * p{};
  wa_ret_GpStatus(GdipCloneStringFormat(wa_par_GpStringFormat(1), &p));
  hb_storptr(p, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatFlags(GpStringFormat*,INT)
HB_FUNC( WAGDIPSETSTRINGFORMATFLAGS )
{
  wa_ret_GpStatus(GdipSetStringFormatFlags(wa_par_GpStringFormat(1), wa_par_INT(2)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatFlags(GDIPCONST GpStringFormat*,INT*)
HB_FUNC( WAGDIPGETSTRINGFORMATFLAGS )
{
  INT i{};
  wa_ret_GpStatus(GdipGetStringFormatFlags(wa_par_GpStringFormat(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatAlign(GpStringFormat*,StringAlignment)
HB_FUNC( WAGDIPSETSTRINGFORMATALIGN )
{
  wa_ret_GpStatus(GdipSetStringFormatAlign(wa_par_GpStringFormat(1), wa_par_GpStringAlignment(2)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatAlign(GDIPCONST GpStringFormat*,StringAlignment*)
HB_FUNC( WAGDIPGETSTRINGFORMATALIGN )
{
  StringAlignment sa{};
  wa_ret_GpStatus(GdipGetStringFormatAlign(wa_par_GpStringFormat(1), &sa));
  hb_storni(sa, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatLineAlign(GpStringFormat*,StringAlignment)
HB_FUNC( WAGDIPSETSTRINGFORMATLINEALIGN )
{
  wa_ret_GpStatus(GdipSetStringFormatLineAlign(wa_par_GpStringFormat(1), wa_par_GpStringAlignment(2)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatLineAlign(GDIPCONST GpStringFormat*,StringAlignment*)
HB_FUNC( WAGDIPGETSTRINGFORMATLINEALIGN )
{
  StringAlignment sa{};
  wa_ret_GpStatus(GdipGetStringFormatLineAlign(wa_par_GpStringFormat(1), &sa));
  hb_storni(sa, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatTrimming(GpStringFormat*,StringTrimming)
HB_FUNC( WAGDIPSETSTRINGFORMATTRIMMING )
{
  wa_ret_GpStatus(GdipSetStringFormatTrimming(wa_par_GpStringFormat(1), wa_par_GpStringTrimming(2)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatTrimming(GDIPCONST GpStringFormat*,StringTrimming*)
HB_FUNC( WAGDIPGETSTRINGFORMATTRIMMING )
{
  StringTrimming st{};
  wa_ret_GpStatus(GdipGetStringFormatTrimming(wa_par_GpStringFormat(1), &st));
  hb_storni(st, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatHotkeyPrefix(GpStringFormat*,INT)
HB_FUNC( WAGDIPSETSTRINGFORMATHOTKEYPREFIX )
{
  wa_ret_GpStatus(GdipSetStringFormatHotkeyPrefix(wa_par_GpStringFormat(1), wa_par_INT(2)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatHotkeyPrefix(GDIPCONST GpStringFormat*,INT*)
HB_FUNC( WAGDIPGETSTRINGFORMATHOTKEYPREFIX )
{
  INT i{};
  wa_ret_GpStatus(GdipGetStringFormatHotkeyPrefix(wa_par_GpStringFormat(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatTabStops(GpStringFormat*,REAL,INT,GDIPCONST REAL*)
HB_FUNC( WAGDIPSETSTRINGFORMATTABSTOPS )
{
  std::vector<REAL> vec = REALArrayToVector(hb_param(4, Harbour::Item::ARRAY));
  wa_ret_GpStatus(GdipSetStringFormatTabStops(wa_par_GpStringFormat(1), wa_par_REAL(2), wa_par_INT(3), vec.data()));
}

// GpStatus WINGDIPAPI GdipGetStringFormatTabStops(GDIPCONST GpStringFormat*,INT,REAL*,REAL*)

// GpStatus WINGDIPAPI GdipGetStringFormatTabStopCount(GDIPCONST GpStringFormat*,INT*)
HB_FUNC( WAGDIPGETSTRINGFORMATTABSTOPCOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetStringFormatTabStopCount(wa_par_GpStringFormat(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatDigitSubstitution(GpStringFormat*,LANGID,StringDigitSubstitute)
HB_FUNC( WAGDIPSETSTRINGFORMATDIGITSUBSTITUTION )
{
  wa_ret_GpStatus(GdipSetStringFormatDigitSubstitution(wa_par_GpStringFormat(1), wa_par_LANGID(2), wa_par_GpStringDigitSubstitute(3)));
}

// GpStatus WINGDIPAPI GdipGetStringFormatDigitSubstitution(GDIPCONST GpStringFormat*,LANGID*,StringDigitSubstitute*)
#if 0
HB_FUNC( WAGDIPGETSTRINGFORMATDIGITSUBSTITUTION )
{
  StringDigitSubstitute sds{};
  wa_ret_GpStatus(GdipGetStringFormatDigitSubstitution(wa_par_GpStringFormat(1), LANGID*, &sds));
  hb_storni(sds, 3);
}
#endif

// GpStatus WINGDIPAPI GdipGetStringFormatMeasurableCharacterRangeCount(GDIPCONST GpStringFormat*,INT*)
HB_FUNC( WAGDIPGETSTRINGFORMATMEASURABLECHARACTERRANGECOUNT )
{
  INT i{};
  wa_ret_GpStatus(GdipGetStringFormatMeasurableCharacterRangeCount(wa_par_GpStringFormat(1), &i));
  wa_stor_INT(i, 2);
}

// GpStatus WINGDIPAPI GdipSetStringFormatMeasurableCharacterRanges(GpStringFormat*,INT,GDIPCONST CharacterRange*)
HB_FUNC( WAGDIPSETSTRINGFORMATMEASURABLECHARACTERRANGES )
{
  std::vector<CharacterRange> vec{};
  auto pArray = hb_param(3, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<CharacterRange*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  wa_ret_GpStatus(GdipSetStringFormatMeasurableCharacterRanges(wa_par_GpStringFormat(1), wa_par_INT(2), vec.data()));
}

///////////////////////////////////////////////////////////////////////////////
// Text functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipDrawString(GpGraphics*,GDIPCONST WCHAR*,INT,GDIPCONST GpFont*,GDIPCONST RectF*,GDIPCONST GpStringFormat*,GDIPCONST GpBrush*)
HB_FUNC( WAGDIPDRAWSTRING )
{
  void * str{};
  wa_ret_GpStatus(GdipDrawString(wa_par_GpGraphics(1), HB_PARSTR(2, &str, nullptr), wa_par_INT(3), wa_par_GpFont(4), wa_par_GpRectF(5), wa_par_GpStringFormat(6), wa_par_GpBrush(7)));
  hb_strfree(str);
}

// GpStatus WINGDIPAPI GdipMeasureString(GpGraphics*,GDIPCONST WCHAR*,INT,GDIPCONST GpFont*,GDIPCONST RectF*,GDIPCONST GpStringFormat*,RectF*,INT*,INT*)
HB_FUNC( WAGDIPMEASURESTRING )
{
  INT i1{};
  INT i2{};
  void * str{};
  wa_ret_GpStatus(GdipMeasureString(wa_par_GpGraphics(1), HB_PARSTR(2, &str, nullptr), wa_par_INT(3), wa_par_GpFont(4), wa_par_GpRectF(5), wa_par_GpStringFormat(6), wa_par_GpRectF(7), &i1, &i2));
  hb_strfree(str);
  wa_stor_INT(i1, 8);
  wa_stor_INT(i2, 9);
}

#ifdef __cplusplus

// GpStatus WINGDIPAPI GdipMeasureCharacterRanges(GpGraphics*,GDIPCONST WCHAR*,INT,GDIPCONST GpFont*,GDIPCONST RectF&,GDIPCONST GpStringFormat*,INT,GpRegion**)

#endif

// GpStatus WINGDIPAPI GdipDrawDriverString(GpGraphics*,GDIPCONST UINT16*,INT,GDIPCONST GpFont*,GDIPCONST GpBrush*,GDIPCONST PointF*,INT,GDIPCONST GpMatrix*)

// GpStatus WINGDIPAPI GdipMeasureDriverString(GpGraphics*,GDIPCONST UINT16*,INT,GDIPCONST GpFont*,GDIPCONST PointF*,INT,GDIPCONST GpMatrix*,RectF*)

///////////////////////////////////////////////////////////////////////////////
// TextureBrush functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipCreateTexture(GpImage*,GpWrapMode,GpTexture**)
HB_FUNC( WAGDIPCREATETEXTURE )
{
  GpTexture * p{};
  wa_ret_GpStatus(GdipCreateTexture(wa_par_GpImage(1), wa_par_GpWrapMode(2), &p));
  hb_storptr(p, 3);
}

// GpStatus WINGDIPAPI GdipCreateTexture2(GpImage*,GpWrapMode,REAL,REAL,REAL,REAL,GpTexture**)
HB_FUNC( WAGDIPCREATETEXTURE2 )
{
  GpTexture * p{};
  wa_ret_GpStatus(GdipCreateTexture2(wa_par_GpImage(1), wa_par_GpWrapMode(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCreateTexture2I(GpImage*,GpWrapMode,INT,INT,INT,INT,GpTexture**)
HB_FUNC( WAGDIPCREATETEXTURE2I )
{
  GpTexture * p{};
  wa_ret_GpStatus(GdipCreateTexture2I(wa_par_GpImage(1), wa_par_GpWrapMode(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCreateTextureIA(GpImage*,GDIPCONST GpImageAttributes*,REAL,REAL,REAL,REAL,GpTexture**)
HB_FUNC( WAGDIPCREATETEXTUREIA )
{
  GpTexture * p{};
  wa_ret_GpStatus(GdipCreateTextureIA(wa_par_GpImage(1), wa_par_GpImageAttributes(2), wa_par_REAL(3), wa_par_REAL(4), wa_par_REAL(5), wa_par_REAL(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipCreateTextureIAI(GpImage*,GDIPCONST GpImageAttributes*,INT,INT,INT,INT,GpTexture**)
HB_FUNC( WAGDIPCREATETEXTUREIAI )
{
  GpTexture * p{};
  wa_ret_GpStatus(GdipCreateTextureIAI(wa_par_GpImage(1), wa_par_GpImageAttributes(2), wa_par_INT(3), wa_par_INT(4), wa_par_INT(5), wa_par_INT(6), &p));
  hb_storptr(p, 7);
}

// GpStatus WINGDIPAPI GdipGetTextureTransform(GpTexture*,GpMatrix*)
HB_FUNC( WAGDIPGETTEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipGetTextureTransform(wa_par_GpTexture(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipSetTextureTransform(GpTexture*,GDIPCONST GpMatrix*)
HB_FUNC( WAGDIPSETTEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipSetTextureTransform(wa_par_GpTexture(1), wa_par_GpMatrix(2)));
}

// GpStatus WINGDIPAPI GdipResetTextureTransform(GpTexture*)
HB_FUNC( WAGDIPRESETTEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipResetTextureTransform(wa_par_GpTexture(1)));
}

// GpStatus WINGDIPAPI GdipMultiplyTextureTransform(GpTexture*,GDIPCONST GpMatrix*,GpMatrixOrder)
HB_FUNC( WAGDIPMULTIPLYTEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipMultiplyTextureTransform(wa_par_GpTexture(1), wa_par_GpMatrix(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipTranslateTextureTransform(GpTexture*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPTRANSLATETEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipTranslateTextureTransform(wa_par_GpTexture(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipScaleTextureTransform(GpTexture*,REAL,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPSCALETEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipScaleTextureTransform(wa_par_GpTexture(1), wa_par_REAL(2), wa_par_REAL(3), wa_par_GpMatrixOrder(4)));
}

// GpStatus WINGDIPAPI GdipRotateTextureTransform(GpTexture*,REAL,GpMatrixOrder)
HB_FUNC( WAGDIPROTATETEXTURETRANSFORM )
{
  wa_ret_GpStatus(GdipRotateTextureTransform(wa_par_GpTexture(1), wa_par_REAL(2), wa_par_GpMatrixOrder(3)));
}

// GpStatus WINGDIPAPI GdipSetTextureWrapMode(GpTexture*,GpWrapMode)
HB_FUNC( WAGDIPSETTEXTUREWRAPMODE )
{
  wa_ret_GpStatus(GdipSetTextureWrapMode(wa_par_GpTexture(1), wa_par_GpWrapMode(2)));
}

// GpStatus WINGDIPAPI GdipGetTextureWrapMode(GpTexture*,GpWrapMode*)
HB_FUNC( WAGDIPGETTEXTUREWRAPMODE )
{
  GpWrapMode wm{};
  wa_ret_GpStatus(GdipGetTextureWrapMode(wa_par_GpTexture(1), &wm));
  hb_storni(wm, 2);
}

// GpStatus WINGDIPAPI GdipGetTextureImage(GpTexture*,GpImage**)
HB_FUNC( WAGDIPGETTEXTUREIMAGE )
{
  GpImage * p{};
  wa_ret_GpStatus(GdipGetTextureImage(wa_par_GpTexture(1), &p));
  hb_storptr(p, 2);
}

///////////////////////////////////////////////////////////////////////////////
// uncategorized functions
///////////////////////////////////////////////////////////////////////////////

// GpStatus WINGDIPAPI GdipTestControl(GpTestControlEnum,void*)

///////////////////////////////////////////////////////////////////////////////
// auxiliary functions
///////////////////////////////////////////////////////////////////////////////

static std::vector<GpPoint> GpPointArrayToVector(const PHB_ITEM pArray)
{
  std::vector<GpPoint> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<GpPoint*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  return vec;
}

static std::vector<GpPointF> GpPointFArrayToVector(const PHB_ITEM pArray)
{
  std::vector<GpPointF> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<GpPointF*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  return vec;
}

static std::vector<GpRect> GpRectArrayToVector(const PHB_ITEM pArray)
{
  std::vector<GpRect> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<GpRect*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  return vec;
}

static std::vector<GpRectF> GpRectFArrayToVector(const PHB_ITEM pArray)
{
  std::vector<GpRectF> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<GpRectF*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  return vec;
}

static std::vector<REAL> REALArrayToVector(const PHB_ITEM pArray)
{
  std::vector<REAL> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<REAL>(hb_arrayGetND(pArray, i + 1)));
    }
  }
  return vec;
}

static std::vector<ARGB> ARGBArrayToVector(const PHB_ITEM pArray)
{
  std::vector<ARGB> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<ARGB>(hb_arrayGetNL(pArray, i + 1)));
    }
  }
  return vec;
}

static std::vector<BYTE> BYTEArrayToVector(const PHB_ITEM pArray)
{
  std::vector<BYTE> vec{};
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<BYTE>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  return vec;
}
