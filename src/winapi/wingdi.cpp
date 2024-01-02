/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINGDIAPI int WINAPI AddFontResourceA(LPCSTR)
*/
HB_FUNC( WAADDFONTRESOURCEA )
{
  wa_ret_int(AddFontResourceA(wa_par_LPCSTR(1)));
}

/*
WINGDIAPI int WINAPI AddFontResourceW(LPCWSTR)
*/
HB_FUNC( WAADDFONTRESOURCEW )
{
  wa_ret_int(AddFontResourceW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WAADDFONTRESOURCE )
{
  void * str1;
  wa_ret_int(AddFontResource(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINGDIAPI WINBOOL WINAPI AnimatePalette(HPALETTE hPal,UINT iStartIndex,UINT cEntries,CONST PALETTEENTRY *ppe)
*/

/*
WINGDIAPI WINBOOL WINAPI Arc(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WAARC )
{
  wa_ret_BOOL(Arc(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI BitBlt(HDC hdc,int x,int y,int cx,int cy,HDC hdcSrc,int x1,int y1,DWORD rop)
*/
HB_FUNC( WABITBLT )
{
  wa_ret_BOOL(BitBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_DWORD(9)));
}

/*
WINGDIAPI WINBOOL WINAPI CancelDC(HDC hdc)
*/
HB_FUNC( WACANCELDC )
{
  wa_ret_BOOL(CancelDC(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI Chord(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WACHORD )
{
  wa_ret_BOOL(Chord(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_int(9)));
}

/*
WINGDIAPI int WINAPI ChoosePixelFormat(HDC hdc,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/
HB_FUNC( WACHOOSEPIXELFORMAT )
{
  wa_ret_int(ChoosePixelFormat(wa_par_HDC(1), static_cast<CONST PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI HMETAFILE WINAPI CloseMetaFile(HDC hdc)
*/
HB_FUNC( WACLOSEMETAFILE )
{
  wa_ret_HMETAFILE(CloseMetaFile(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI CombineRgn(HRGN hrgnDst,HRGN hrgnSrc1,HRGN hrgnSrc2,int iMode)
*/
HB_FUNC( WACOMBINERGN )
{
  wa_ret_int(CombineRgn(wa_par_HRGN(1), wa_par_HRGN(2), wa_par_HRGN(3), wa_par_int(4)));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileA(HMETAFILE,LPCSTR)
*/
HB_FUNC( WACOPYMETAFILEA )
{
  wa_ret_HMETAFILE(CopyMetaFileA(wa_par_HMETAFILE(1), wa_par_LPCSTR(2)));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileW(HMETAFILE,LPCWSTR)
*/
HB_FUNC( WACOPYMETAFILEW )
{
  wa_ret_HMETAFILE(CopyMetaFileW(wa_par_HMETAFILE(1), wa_par_LPCWSTR(2)));
}

HB_FUNC( WACOPYMETAFILE )
{
  void * str2;
  wa_ret_HMETAFILE(CopyMetaFile(wa_par_HMETAFILE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINGDIAPI HBITMAP WINAPI CreateBitmap(int nWidth,int nHeight,UINT nPlanes,UINT nBitCount,CONST VOID *lpBits)
*/
HB_FUNC( WACREATEBITMAP )
{
  wa_ret_HBITMAP(CreateBitmap(wa_par_int(1), wa_par_int(2), wa_par_UINT(3), wa_par_UINT(4), static_cast<CONST VOID *>(hb_parptr(5))));
}

/*
WINGDIAPI HBITMAP WINAPI CreateBitmapIndirect(CONST BITMAP *pbm)
*/

/*
WINGDIAPI HBRUSH WINAPI CreateBrushIndirect(CONST LOGBRUSH *plbrush)
*/
HB_FUNC( WACREATEBRUSHINDIRECT )
{
  wa_ret_HBRUSH(CreateBrushIndirect(static_cast<CONST LOGBRUSH *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HBITMAP WINAPI CreateCompatibleBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WACREATECOMPATIBLEBITMAP )
{
  wa_ret_HBITMAP(CreateCompatibleBitmap(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI HBITMAP WINAPI CreateDiscardableBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WACREATEDISCARDABLEBITMAP )
{
  wa_ret_HBITMAP(CreateDiscardableBitmap(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI HDC WINAPI CreateCompatibleDC(HDC hdc)
*/
HB_FUNC( WACREATECOMPATIBLEDC )
{
  wa_ret_HDC(CreateCompatibleDC(wa_par_HDC(1)));
}

/*
WINGDIAPI HDC WINAPI CreateDCA(LPCSTR pwszDriver,LPCSTR pwszDevice,LPCSTR pszPort,CONST DEVMODEA *pdm)
*/

/*
WINGDIAPI HDC WINAPI CreateDCW(LPCWSTR pwszDriver,LPCWSTR pwszDevice,LPCWSTR pszPort,CONST DEVMODEW *pdm)
*/

/*
WINGDIAPI HBITMAP WINAPI CreateDIBitmap(HDC hdc,CONST BITMAPINFOHEADER *pbmih,DWORD flInit,CONST VOID *pjBits,CONST BITMAPINFO *pbmi,UINT iUsage)
*/

/*
WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrush(HGLOBAL h,UINT iUsage)
*/
HB_FUNC( WACREATEDIBPATTERNBRUSH )
{
  wa_ret_HBRUSH(CreateDIBPatternBrush(wa_par_HGLOBAL(1), wa_par_UINT(2)));
}

/*
WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrushPt(CONST VOID *lpPackedDIB,UINT iUsage)
*/

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WACREATEELLIPTICRGN )
{
  wa_ret_HRGN(CreateEllipticRgn(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgnIndirect(CONST RECT *lprect)
*/
HB_FUNC( WACREATEELLIPTICRGNINDIRECT )
{
  wa_ret_HRGN(CreateEllipticRgnIndirect(static_cast<CONST RECT *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HFONT WINAPI CreateFontIndirectA(CONST LOGFONTA *lplf)
*/

/*
WINGDIAPI HFONT WINAPI CreateFontIndirectW(CONST LOGFONTW *lplf)
*/

/*
WINGDIAPI HFONT WINAPI CreateFontA(int cHeight,int cWidth,int cEscapement,int cOrientation,int cWeight,DWORD bItalic,DWORD bUnderline,DWORD bStrikeOut,DWORD iCharSet,DWORD iOutPrecision,DWORD iClipPrecision,DWORD iQuality,DWORD iPitchAndFamily,LPCSTR pszFaceName)
*/
HB_FUNC( WACREATEFONTA )
{
  wa_ret_HFONT(CreateFontA(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_DWORD(6), wa_par_DWORD(7), wa_par_DWORD(8), wa_par_DWORD(9), wa_par_DWORD(10), wa_par_DWORD(11), wa_par_DWORD(12), wa_par_DWORD(13), wa_par_LPCSTR(14)));
}

/*
WINGDIAPI HFONT WINAPI CreateFontW(int cHeight,int cWidth,int cEscapement,int cOrientation,int cWeight,DWORD bItalic,DWORD bUnderline,DWORD bStrikeOut,DWORD iCharSet,DWORD iOutPrecision,DWORD iClipPrecision,DWORD iQuality,DWORD iPitchAndFamily,LPCWSTR pszFaceName)
*/
HB_FUNC( WACREATEFONTW )
{
  wa_ret_HFONT(CreateFontW(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_DWORD(6), wa_par_DWORD(7), wa_par_DWORD(8), wa_par_DWORD(9), wa_par_DWORD(10), wa_par_DWORD(11), wa_par_DWORD(12), wa_par_DWORD(13), wa_par_LPCWSTR(14)));
}

HB_FUNC( WACREATEFONT )
{
  void * str14;
  wa_ret_HFONT(CreateFontW(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_DWORD(6), wa_par_DWORD(7), wa_par_DWORD(8), wa_par_DWORD(9), wa_par_DWORD(10), wa_par_DWORD(11), wa_par_DWORD(12), wa_par_DWORD(13), HB_PARSTR(14, &str14, nullptr)));
  hb_strfree(str14);
}

/*
WINGDIAPI HBRUSH WINAPI CreateHatchBrush(int iHatch,COLORREF color)
*/
HB_FUNC( WACREATEHATCHBRUSH )
{
  wa_ret_HBRUSH(CreateHatchBrush(wa_par_int(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI HDC WINAPI CreateICA(LPCSTR pszDriver,LPCSTR pszDevice,LPCSTR pszPort,CONST DEVMODEA *pdm)
*/

/*
WINGDIAPI HDC WINAPI CreateICW(LPCWSTR pszDriver,LPCWSTR pszDevice,LPCWSTR pszPort,CONST DEVMODEW *pdm)
*/

/*
WINGDIAPI HDC WINAPI CreateMetaFileA(LPCSTR pszFile)
*/
HB_FUNC( WACREATEMETAFILEA )
{
  wa_ret_HDC(CreateMetaFileA(wa_par_LPCSTR(1)));
}

/*
WINGDIAPI HDC WINAPI CreateMetaFileW(LPCWSTR pszFile)
*/
HB_FUNC( WACREATEMETAFILEW )
{
  wa_ret_HDC(CreateMetaFileW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WACREATEMETAFILE )
{
  void * str1;
  wa_ret_HDC(CreateMetaFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINGDIAPI HPALETTE WINAPI CreatePalette(CONST LOGPALETTE *plpal)
*/

/*
WINGDIAPI HPEN WINAPI CreatePen(int iStyle,int cWidth,COLORREF color)
*/
HB_FUNC( WACREATEPEN )
{
  wa_ret_HPEN(CreatePen(wa_par_int(1), wa_par_int(2), wa_par_COLORREF(3)));
}

/*
WINGDIAPI HPEN WINAPI CreatePenIndirect(CONST LOGPEN *plpen)
*/
HB_FUNC( WACREATEPENINDIRECT )
{
  wa_ret_HPEN(CreatePenIndirect(static_cast<CONST LOGPEN *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HRGN WINAPI CreatePolyPolygonRgn(CONST POINT *pptl,CONST INT *pc,int cPoly,int iMode)
*/

/*
WINGDIAPI HBRUSH WINAPI CreatePatternBrush(HBITMAP hbm)
*/
HB_FUNC( WACREATEPATTERNBRUSH )
{
  wa_ret_HBRUSH(CreatePatternBrush(wa_par_HBITMAP(1)));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WACREATERECTRGN )
{
  wa_ret_HRGN(CreateRectRgn(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgnIndirect(CONST RECT *lprect)
*/
HB_FUNC( WACREATERECTRGNINDIRECT )
{
  wa_ret_HRGN(CreateRectRgnIndirect(static_cast<CONST RECT *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HRGN WINAPI CreateRoundRectRgn(int x1,int y1,int x2,int y2,int w,int h)
*/
HB_FUNC( WACREATEROUNDRECTRGN )
{
  wa_ret_HRGN(CreateRoundRectRgn(wa_par_int(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6)));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceA(DWORD fdwHidden,LPCSTR lpszFont,LPCSTR lpszFile,LPCSTR lpszPath)
*/
HB_FUNC( WACREATESCALABLEFONTRESOURCEA )
{
  wa_ret_BOOL(CreateScalableFontResourceA(wa_par_DWORD(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_LPCSTR(4)));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceW(DWORD fdwHidden,LPCWSTR lpszFont,LPCWSTR lpszFile,LPCWSTR lpszPath)
*/
HB_FUNC( WACREATESCALABLEFONTRESOURCEW )
{
  wa_ret_BOOL(CreateScalableFontResourceW(wa_par_DWORD(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4)));
}

HB_FUNC( WACREATESCALABLEFONTRESOURCE )
{
  void * str2;
  void * str3;
  void * str4;
  wa_ret_BOOL(CreateScalableFontResource(wa_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
WINGDIAPI HBRUSH WINAPI CreateSolidBrush(COLORREF color)
*/
HB_FUNC( WACREATESOLIDBRUSH )
{
  wa_ret_HBRUSH(CreateSolidBrush(wa_par_COLORREF(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteDC(HDC hdc)
*/
HB_FUNC( WADELETEDC )
{
  wa_ret_BOOL(DeleteDC(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteMetaFile(HMETAFILE hmf)
*/
HB_FUNC( WADELETEMETAFILE )
{
  wa_ret_BOOL(DeleteMetaFile(wa_par_HMETAFILE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteObject(HGDIOBJ ho)
*/
HB_FUNC( WADELETEOBJECT )
{
  wa_ret_BOOL(DeleteObject(wa_par_HGDIOBJ(1)));
}

/*
WINGDIAPI int WINAPI DescribePixelFormat(HDC hdc,int iPixelFormat,UINT nBytes,LPPIXELFORMATDESCRIPTOR ppfd)
*/
HB_FUNC( WADESCRIBEPIXELFORMAT )
{
  wa_ret_int(DescribePixelFormat(wa_par_HDC(1), wa_par_int(2), wa_par_UINT(3), static_cast<LPPIXELFORMATDESCRIPTOR>(winapi_get_ptr(4))));
}

/*
WINSPOOLAPI int WINAPI DeviceCapabilitiesA(LPCSTR pDevice,LPCSTR pPort,WORD fwCapability,LPSTR pOutput,CONST DEVMODEA *pDevMode)
*/

/*
WINSPOOLAPI int WINAPI DeviceCapabilitiesW(LPCWSTR pDevice,LPCWSTR pPort,WORD fwCapability,LPWSTR pOutput,CONST DEVMODEW *pDevMode)
*/

/*
WINGDIAPI int WINAPI DrawEscape(HDC hdc,int iEscape,int cjIn,LPCSTR lpIn)
*/
HB_FUNC( WADRAWESCAPE )
{
  wa_ret_int(DrawEscape(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCSTR(4)));
}

/*
WINGDIAPI WINBOOL WINAPI Ellipse(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WAELLIPSE )
{
  wa_ret_BOOL(Ellipse(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI int WINAPI EnumFontFamiliesExA(HDC hdc,LPLOGFONTA lpLogfont,FONTENUMPROCA lpProc,LPARAM lParam,DWORD dwFlags)
*/

/*
WINGDIAPI int WINAPI EnumFontFamiliesExW(HDC hdc,LPLOGFONTW lpLogfont,FONTENUMPROCW lpProc,LPARAM lParam,DWORD dwFlags)
*/

/*
WINGDIAPI int WINAPI EnumFontFamiliesA(HDC hdc,LPCSTR lpLogfont,FONTENUMPROCA lpProc,LPARAM lParam)
*/

/*
WINGDIAPI int WINAPI EnumFontFamiliesW(HDC hdc,LPCWSTR lpLogfont,FONTENUMPROCW lpProc,LPARAM lParam)
*/

/*
WINGDIAPI int WINAPI EnumFontsA(HDC hdc,LPCSTR lpLogfont,FONTENUMPROCA lpProc,LPARAM lParam)
*/

/*
WINGDIAPI int WINAPI EnumFontsW(HDC hdc,LPCWSTR lpLogfont,FONTENUMPROCW lpProc,LPARAM lParam)
*/

/*
WINGDIAPI int WINAPI EnumObjects(HDC hdc,int nType,GOBJENUMPROC lpFunc,LPARAM lParam)
*/

/*
WINGDIAPI WINBOOL WINAPI EqualRgn(HRGN hrgn1,HRGN hrgn2)
*/
HB_FUNC( WAEQUALRGN )
{
  wa_ret_BOOL(EqualRgn(wa_par_HRGN(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI Escape(HDC hdc,int iEscape,int cjIn,LPCSTR pvIn,LPVOID pvOut)
*/
HB_FUNC( WAESCAPE )
{
  wa_ret_int(Escape(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCSTR(4), static_cast<LPVOID>(hb_parptr(5))));
}

/*
WINGDIAPI int WINAPI ExtEscape(HDC hdc,int iEscape,int cjInput,LPCSTR lpInData,int cjOutput,LPSTR lpOutData)
*/
HB_FUNC( WAEXTESCAPE )
{
  wa_ret_int(ExtEscape(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCSTR(4), wa_par_int(5), const_cast<LPSTR>(hb_parc(6))));
}

/*
WINGDIAPI int WINAPI ExcludeClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WAEXCLUDECLIPRECT )
{
  wa_ret_int(ExcludeClipRect(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI HRGN WINAPI ExtCreateRegion(CONST XFORM *lpx,DWORD nCount,CONST RGNDATA *lpData)
*/

/*
WINGDIAPI WINBOOL WINAPI ExtFloodFill(HDC hdc,int x,int y,COLORREF color,UINT type)
*/
HB_FUNC( WAEXTFLOODFILL )
{
  wa_ret_BOOL(ExtFloodFill(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_COLORREF(4), wa_par_UINT(5)));
}

/*
WINGDIAPI WINBOOL WINAPI FillRgn(HDC hdc,HRGN hrgn,HBRUSH hbr)
*/
HB_FUNC( WAFILLRGN )
{
  wa_ret_BOOL(FillRgn(wa_par_HDC(1), wa_par_HRGN(2), wa_par_HBRUSH(3)));
}

/*
WINGDIAPI WINBOOL WINAPI FloodFill(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WAFLOODFILL )
{
  wa_ret_BOOL(FloodFill(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI FrameRgn(HDC hdc,HRGN hrgn,HBRUSH hbr,int w,int h)
*/
HB_FUNC( WAFRAMERGN )
{
  wa_ret_BOOL(FrameRgn(wa_par_HDC(1), wa_par_HRGN(2), wa_par_HBRUSH(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI int WINAPI GetROP2(HDC hdc)
*/
HB_FUNC( WAGETROP2 )
{
  wa_ret_int(GetROP2(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetAspectRatioFilterEx(HDC hdc,LPSIZE lpsize)
*/
HB_FUNC( WAGETASPECTRATIOFILTEREX )
{
  wa_ret_BOOL(GetAspectRatioFilterEx(wa_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI COLORREF WINAPI GetBkColor(HDC hdc)
*/
HB_FUNC( WAGETBKCOLOR )
{
  wa_ret_COLORREF(GetBkColor(wa_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetDCBrushColor(HDC hdc)
*/
HB_FUNC( WAGETDCBRUSHCOLOR )
{
  wa_ret_COLORREF(GetDCBrushColor(wa_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetDCPenColor(HDC hdc)
*/
HB_FUNC( WAGETDCPENCOLOR )
{
  wa_ret_COLORREF(GetDCPenColor(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetBkMode(HDC hdc)
*/
HB_FUNC( WAGETBKMODE )
{
  wa_ret_int(GetBkMode(wa_par_HDC(1)));
}

/*
WINGDIAPI LONG WINAPI GetBitmapBits(HBITMAP hbit,LONG cb,LPVOID lpvBits)
*/
HB_FUNC( WAGETBITMAPBITS )
{
  wa_ret_LONG(GetBitmapBits(wa_par_HBITMAP(1), hb_parnl(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI GetBitmapDimensionEx(HBITMAP hbit,LPSIZE lpsize)
*/
HB_FUNC( WAGETBITMAPDIMENSIONEX )
{
  wa_ret_BOOL(GetBitmapDimensionEx(wa_par_HBITMAP(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI UINT WINAPI GetBoundsRect(HDC hdc,LPRECT lprect,UINT flags)
*/
HB_FUNC( WAGETBOUNDSRECT )
{
  wa_ret_UINT(GetBoundsRect(wa_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2)), wa_par_UINT(3)));
}

/*
WINGDIAPI WINBOOL WINAPI GetBrushOrgEx(HDC hdc,LPPOINT lppt)
*/
HB_FUNC( WAGETBRUSHORGEX )
{
  wa_ret_BOOL(GetBrushOrgEx(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthA(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthW(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidth32A(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/
HB_FUNC( WAGETCHARWIDTH32A )
{
  INT lpBuffer;
  wa_ret_BOOL(GetCharWidth32A(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), &lpBuffer));
  wa_stor_INT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidth32W(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/
HB_FUNC( WAGETCHARWIDTH32W )
{
  INT lpBuffer;
  wa_ret_BOOL(GetCharWidth32W(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), &lpBuffer));
  wa_stor_INT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatA(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/
HB_FUNC( WAGETCHARWIDTHFLOATA )
{
  FLOAT lpBuffer;
  wa_ret_BOOL(GetCharWidthFloatA(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), &lpBuffer));
  wa_stor_FLOAT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatW(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/
HB_FUNC( WAGETCHARWIDTHFLOATW )
{
  FLOAT lpBuffer;
  wa_ret_BOOL(GetCharWidthFloatW(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), &lpBuffer));
  wa_stor_FLOAT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsA(HDC hdc,UINT wFirst,UINT wLast,LPABC lpABC)
*/
HB_FUNC( WAGETCHARABCWIDTHSA )
{
  wa_ret_BOOL(GetCharABCWidthsA(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), static_cast<LPABC>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsW(HDC hdc,UINT wFirst,UINT wLast,LPABC lpABC)
*/
HB_FUNC( WAGETCHARABCWIDTHSW )
{
  wa_ret_BOOL(GetCharABCWidthsW(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), static_cast<LPABC>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsFloatA(HDC hdc,UINT iFirst,UINT iLast,LPABCFLOAT lpABC)
*/
HB_FUNC( WAGETCHARABCWIDTHSFLOATA )
{
  wa_ret_BOOL(GetCharABCWidthsFloatA(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), static_cast<LPABCFLOAT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsFloatW(HDC hdc,UINT iFirst,UINT iLast,LPABCFLOAT lpABC)
*/
HB_FUNC( WAGETCHARABCWIDTHSFLOATW )
{
  wa_ret_BOOL(GetCharABCWidthsFloatW(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), static_cast<LPABCFLOAT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI int WINAPI GetClipBox(HDC hdc,LPRECT lprect)
*/
HB_FUNC( WAGETCLIPBOX )
{
  wa_ret_int(GetClipBox(wa_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI GetClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WAGETCLIPRGN )
{
  wa_ret_int(GetClipRgn(wa_par_HDC(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI GetMetaRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WAGETMETARGN )
{
  wa_ret_int(GetMetaRgn(wa_par_HDC(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI HGDIOBJ WINAPI GetCurrentObject(HDC hdc,UINT type)
*/
HB_FUNC( WAGETCURRENTOBJECT )
{
  wa_ret_HGDIOBJ(GetCurrentObject(wa_par_HDC(1), wa_par_UINT(2)));
}

/*
WINGDIAPI WINBOOL WINAPI GetCurrentPositionEx(HDC hdc,LPPOINT lppt)
*/
HB_FUNC( WAGETCURRENTPOSITIONEX )
{
  wa_ret_BOOL(GetCurrentPositionEx(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI GetDeviceCaps(HDC hdc,int index)
*/
HB_FUNC( WAGETDEVICECAPS )
{
  wa_ret_int(GetDeviceCaps(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI int WINAPI GetDIBits(HDC hdc,HBITMAP hbm,UINT start,UINT cLines,LPVOID lpvBits,LPBITMAPINFO lpbmi,UINT usage)
*/

/*
WINGDIAPI DWORD WINAPI GetFontData (HDC hdc,DWORD dwTable,DWORD dwOffset,PVOID pvBuffer,DWORD cjBuffer)
*/
HB_FUNC( WAGETFONTDATA )
{
  wa_ret_DWORD(GetFontData(wa_par_HDC(1), wa_par_DWORD(2), wa_par_DWORD(3), static_cast<PVOID>(hb_parptr(4)), wa_par_DWORD(5)));
}

/*
WINGDIAPI DWORD WINAPI GetGlyphOutlineA(HDC hdc,UINT uChar,UINT fuFormat,LPGLYPHMETRICS lpgm,DWORD cjBuffer,LPVOID pvBuffer,CONST MAT2 *lpmat2)
*/

/*
WINGDIAPI DWORD WINAPI GetGlyphOutlineW(HDC hdc,UINT uChar,UINT fuFormat,LPGLYPHMETRICS lpgm,DWORD cjBuffer,LPVOID pvBuffer,CONST MAT2 *lpmat2)
*/

/*
WINGDIAPI int WINAPI GetGraphicsMode(HDC hdc)
*/
HB_FUNC( WAGETGRAPHICSMODE )
{
  wa_ret_int(GetGraphicsMode(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetMapMode(HDC hdc)
*/
HB_FUNC( WAGETMAPMODE )
{
  wa_ret_int(GetMapMode(wa_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetMetaFileBitsEx(HMETAFILE hMF,UINT cbBuffer,LPVOID lpData)
*/
HB_FUNC( WAGETMETAFILEBITSEX )
{
  wa_ret_UINT(GetMetaFileBitsEx(wa_par_HMETAFILE(1), wa_par_UINT(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WAGETMETAFILEA )
{
  wa_ret_HMETAFILE(GetMetaFileA(wa_par_LPCSTR(1)));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WAGETMETAFILEW )
{
  wa_ret_HMETAFILE(GetMetaFileW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WAGETMETAFILE )
{
  void * str1;
  wa_ret_HMETAFILE(GetMetaFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINGDIAPI COLORREF WINAPI GetNearestColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WAGETNEARESTCOLOR )
{
  wa_ret_COLORREF(GetNearestColor(wa_par_HDC(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI UINT WINAPI GetNearestPaletteIndex(HPALETTE h,COLORREF color)
*/
HB_FUNC( WAGETNEARESTPALETTEINDEX )
{
  wa_ret_UINT(GetNearestPaletteIndex(wa_par_HPALETTE(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI DWORD WINAPI GetObjectType(HGDIOBJ h)
*/
HB_FUNC( WAGETOBJECTTYPE )
{
  wa_ret_DWORD(GetObjectType(wa_par_HGDIOBJ(1)));
}

/*
WINGDIAPI UINT WINAPI GetOutlineTextMetricsA(HDC hdc,UINT cjCopy,LPOUTLINETEXTMETRICA potm)
*/

/*
WINGDIAPI UINT WINAPI GetOutlineTextMetricsW(HDC hdc,UINT cjCopy,LPOUTLINETEXTMETRICW potm)
*/

/*
WINGDIAPI UINT WINAPI GetPaletteEntries(HPALETTE hpal,UINT iStart,UINT cEntries,LPPALETTEENTRY pPalEntries)
*/

/*
WINGDIAPI COLORREF WINAPI GetPixel(HDC hdc,int x,int y)
*/
HB_FUNC( WAGETPIXEL )
{
  wa_ret_COLORREF(GetPixel(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI int WINAPI GetPixelFormat(HDC hdc)
*/
HB_FUNC( WAGETPIXELFORMAT )
{
  wa_ret_int(GetPixelFormat(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetPolyFillMode(HDC hdc)
*/
HB_FUNC( WAGETPOLYFILLMODE )
{
  wa_ret_int(GetPolyFillMode(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetRasterizerCaps(LPRASTERIZER_STATUS lpraststat,UINT cjBytes)
*/

/*
WINGDIAPI int WINAPI GetRandomRgn (HDC hdc,HRGN hrgn,INT i)
*/
HB_FUNC( WAGETRANDOMRGN )
{
  wa_ret_int(GetRandomRgn(wa_par_HDC(1), wa_par_HRGN(2), wa_par_int(3)));
}

/*
WINGDIAPI DWORD WINAPI GetRegionData(HRGN hrgn,DWORD nCount,LPRGNDATA lpRgnData)
*/

/*
WINGDIAPI int WINAPI GetRgnBox(HRGN hrgn,LPRECT lprc)
*/
HB_FUNC( WAGETRGNBOX )
{
  wa_ret_int(GetRgnBox(wa_par_HRGN(1), static_cast<LPRECT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI HGDIOBJ WINAPI GetStockObject(int i)
*/
HB_FUNC( WAGETSTOCKOBJECT )
{
  wa_ret_HGDIOBJ(GetStockObject(wa_par_int(1)));
}

/*
WINGDIAPI int WINAPI GetStretchBltMode(HDC hdc)
*/
HB_FUNC( WAGETSTRETCHBLTMODE )
{
  wa_ret_int(GetStretchBltMode(wa_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetSystemPaletteEntries(HDC hdc,UINT iStart,UINT cEntries,LPPALETTEENTRY pPalEntries)
*/

/*
WINGDIAPI UINT WINAPI GetSystemPaletteUse(HDC hdc)
*/
HB_FUNC( WAGETSYSTEMPALETTEUSE )
{
  wa_ret_UINT(GetSystemPaletteUse(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetTextCharacterExtra(HDC hdc)
*/
HB_FUNC( WAGETTEXTCHARACTEREXTRA )
{
  wa_ret_int(GetTextCharacterExtra(wa_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetTextAlign(HDC hdc)
*/
HB_FUNC( WAGETTEXTALIGN )
{
  wa_ret_UINT(GetTextAlign(wa_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetTextColor(HDC hdc)
*/
HB_FUNC( WAGETTEXTCOLOR )
{
  wa_ret_COLORREF(GetTextColor(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointA(HDC hdc,LPCSTR lpString,int c,LPSIZE lpsz)
*/
HB_FUNC( WAGETTEXTEXTENTPOINTA )
{
  wa_ret_BOOL(GetTextExtentPointA(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointW(HDC hdc,LPCWSTR lpString,int c,LPSIZE lpsz)
*/
HB_FUNC( WAGETTEXTEXTENTPOINTW )
{
  wa_ret_BOOL(GetTextExtentPointW(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

HB_FUNC( WAGETTEXTEXTENTPOINT )
{
  void * str2;
  wa_ret_BOOL(GetTextExtentPoint(wa_par_HDC(1), HB_PARSTR(2, &str2, nullptr), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
  hb_strfree(str2);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32A(HDC hdc,LPCSTR lpString,int c,LPSIZE psizl)
*/
HB_FUNC( WAGETTEXTEXTENTPOINT32A )
{
  wa_ret_BOOL(GetTextExtentPoint32A(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32W(HDC hdc,LPCWSTR lpString,int c,LPSIZE psizl)
*/
HB_FUNC( WAGETTEXTEXTENTPOINT32W )
{
  wa_ret_BOOL(GetTextExtentPoint32W(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

HB_FUNC( WAGETTEXTEXTENTPOINT32 )
{
  void * str2;
  wa_ret_BOOL(GetTextExtentPoint32(wa_par_HDC(1), HB_PARSTR(2, &str2, nullptr), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
  hb_strfree(str2);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointA(HDC hdc,LPCSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/
HB_FUNC( WAGETTEXTEXTENTEXPOINTA )
{
  INT lpnFit;
  INT lpnDx;
  wa_ret_BOOL(GetTextExtentExPointA(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), wa_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  wa_stor_INT(lpnFit, 5);
  wa_stor_INT(lpnDx, 6);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointW(HDC hdc,LPCWSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/
HB_FUNC( WAGETTEXTEXTENTEXPOINTW )
{
  INT lpnFit;
  INT lpnDx;
  wa_ret_BOOL(GetTextExtentExPointW(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), wa_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  wa_stor_INT(lpnFit, 5);
  wa_stor_INT(lpnDx, 6);
}

HB_FUNC( WAGETTEXTEXTENTEXPOINT )
{
  void * str2;
  INT lpnFit;
  INT lpnDx;
  wa_ret_BOOL(GetTextExtentExPoint(wa_par_HDC(1), HB_PARSTR(2, &str2, nullptr), wa_par_int(3), wa_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  wa_stor_INT(lpnFit, 5);
  wa_stor_INT(lpnDx, 6);
  hb_strfree(str2);
}

/*
WINGDIAPI int WINAPI GetTextCharset(HDC hdc)
*/
HB_FUNC( WAGETTEXTCHARSET )
{
  wa_ret_int(GetTextCharset(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetTextCharsetInfo(HDC hdc,LPFONTSIGNATURE lpSig,DWORD dwFlags)
*/

/*
WINGDIAPI WINBOOL WINAPI TranslateCharsetInfo(DWORD *lpSrc,LPCHARSETINFO lpCs,DWORD dwFlags)
*/

/*
WINGDIAPI DWORD WINAPI GetFontLanguageInfo(HDC hdc)
*/
HB_FUNC( WAGETFONTLANGUAGEINFO )
{
  wa_ret_DWORD(GetFontLanguageInfo(wa_par_HDC(1)));
}

/*
WINGDIAPI DWORD WINAPI GetCharacterPlacementA(HDC hdc,LPCSTR lpString,int nCount,int nMexExtent,LPGCP_RESULTSA lpResults,DWORD dwFlags)
*/

/*
WINGDIAPI DWORD WINAPI GetCharacterPlacementW(HDC hdc,LPCWSTR lpString,int nCount,int nMexExtent,LPGCP_RESULTSW lpResults,DWORD dwFlags)
*/

/*
WINGDIAPI DWORD WINAPI GetFontUnicodeRanges(HDC hdc,LPGLYPHSET lpgs)
*/

/*
WINGDIAPI DWORD WINAPI GetGlyphIndicesA(HDC hdc,LPCSTR lpstr,int c,LPWORD pgi,DWORD fl)
*/
HB_FUNC( WAGETGLYPHINDICESA )
{
  WORD gi;
  wa_ret_DWORD(GetGlyphIndicesA(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), &gi, wa_par_DWORD(5)));
  wa_stor_WORD(gi, 4);
}

/*
WINGDIAPI DWORD WINAPI GetGlyphIndicesW(HDC hdc,LPCWSTR lpstr,int c,LPWORD pgi,DWORD fl)
*/
HB_FUNC( WAGETGLYPHINDICESW )
{
  WORD gi;
  wa_ret_DWORD(GetGlyphIndicesW(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), &gi, wa_par_DWORD(5)));
  wa_stor_WORD(gi, 4);
}

HB_FUNC( WAGETGLYPHINDICES )
{
  void * str2;
  WORD gi;
  wa_ret_DWORD(GetGlyphIndices(wa_par_HDC(1), HB_PARSTR(2, &str2, nullptr), wa_par_int(3), &gi, wa_par_DWORD(5)));
  wa_stor_WORD(gi, 4);
  hb_strfree(str2);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointI(HDC hdc,LPWORD pgiIn,int cgi,LPSIZE psize)
*/
#if 0
HB_FUNC( WAGETTEXTEXTENTPOINTI )
{
  WORD giIn;
  wa_ret_BOOL(GetTextExtentPointI(wa_par_HDC(1), &giIn, wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
  wa_stor_WORD(giIn, 2);
}
#endif

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointI (HDC hdc,LPWORD lpwszString,int cwchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthI(HDC hdc,UINT giFirst,UINT cgi,LPWORD pgi,LPINT piWidths)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsI(HDC hdc,UINT giFirst,UINT cgi,LPWORD pgi,LPABC pabc)
*/
HB_FUNC( WAGETCHARABCWIDTHSI )
{
  WORD gi;
  wa_ret_BOOL(GetCharABCWidthsI(wa_par_HDC(1), wa_par_UINT(2), wa_par_UINT(3), &gi, static_cast<LPABC>(winapi_get_ptr(5))));
  wa_stor_WORD(gi, 4);
}

/*
WINGDIAPI int WINAPI AddFontResourceExA(LPCSTR name,DWORD fl,PVOID res)
*/
HB_FUNC( WAADDFONTRESOURCEEXA )
{
  wa_ret_int(AddFontResourceExA(wa_par_LPCSTR(1), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

/*
WINGDIAPI int WINAPI AddFontResourceExW(LPCWSTR name,DWORD fl,PVOID res)
*/
HB_FUNC( WAADDFONTRESOURCEEXW )
{
  wa_ret_int(AddFontResourceExW(wa_par_LPCWSTR(1), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

HB_FUNC( WAADDFONTRESOURCEEX )
{
  void * str1;
  wa_ret_int(AddFontResourceEx(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
  hb_strfree(str1);
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExA(LPCSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WAREMOVEFONTRESOURCEEXA )
{
  wa_ret_BOOL(RemoveFontResourceExA(wa_par_LPCSTR(1), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExW(LPCWSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WAREMOVEFONTRESOURCEEXW )
{
  wa_ret_BOOL(RemoveFontResourceExW(wa_par_LPCWSTR(1), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

HB_FUNC( WAREMOVEFONTRESOURCEEX )
{
  void * str1;
  wa_ret_BOOL(RemoveFontResourceEx(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
  hb_strfree(str1);
}

/*
WINGDIAPI HANDLE WINAPI AddFontMemResourceEx(PVOID pFileView,DWORD cjSize,PVOID pvResrved,DWORD *pNumFonts)
*/

/*
WINGDIAPI WINBOOL WINAPI RemoveFontMemResourceEx(HANDLE h)
*/
HB_FUNC( WAREMOVEFONTMEMRESOURCEEX )
{
  wa_ret_BOOL(RemoveFontMemResourceEx(wa_par_HANDLE(1)));
}

/*
WINGDIAPI HFONT WINAPI CreateFontIndirectExA(CONST ENUMLOGFONTEXDVA *)
*/

/*
WINGDIAPI HFONT WINAPI CreateFontIndirectExW(CONST ENUMLOGFONTEXDVW *)
*/

/*
WINGDIAPI WINBOOL WINAPI GetViewportExtEx(HDC hdc,LPSIZE lpsize)
*/
HB_FUNC( WAGETVIEWPORTEXTEX )
{
  wa_ret_BOOL(GetViewportExtEx(wa_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetViewportOrgEx(HDC hdc,LPPOINT lppoint)
*/
HB_FUNC( WAGETVIEWPORTORGEX )
{
  wa_ret_BOOL(GetViewportOrgEx(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetWindowExtEx(HDC hdc,LPSIZE lpsize)
*/
HB_FUNC( WAGETWINDOWEXTEX )
{
  wa_ret_BOOL(GetWindowExtEx(wa_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetWindowOrgEx(HDC hdc,LPPOINT lppoint)
*/
HB_FUNC( WAGETWINDOWORGEX )
{
  wa_ret_BOOL(GetWindowOrgEx(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI IntersectClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WAINTERSECTCLIPRECT )
{
  wa_ret_int(IntersectClipRect(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI InvertRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WAINVERTRGN )
{
  wa_ret_BOOL(InvertRgn(wa_par_HDC(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI WINBOOL WINAPI LineDDA(int xStart,int yStart,int xEnd,int yEnd,LINEDDAPROC lpProc,LPARAM data)
*/

/*
WINGDIAPI WINBOOL WINAPI LineTo(HDC hdc,int x,int y)
*/
HB_FUNC( WALINETO )
{
  wa_ret_BOOL(LineTo(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI MaskBlt(HDC hdcDest,int xDest,int yDest,int width,int height,HDC hdcSrc,int xSrc,int ySrc,HBITMAP hbmMask,int xMask,int yMask,DWORD rop)
*/
HB_FUNC( WAMASKBLT )
{
  wa_ret_BOOL(MaskBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_HBITMAP(9), wa_par_int(10), wa_par_int(11), wa_par_DWORD(12)));
}

/*
WINGDIAPI WINBOOL WINAPI PlgBlt(HDC hdcDest,CONST POINT *lpPoint,HDC hdcSrc,int xSrc,int ySrc,int width,int height,HBITMAP hbmMask,int xMask,int yMask)
*/

/*
WINGDIAPI int WINAPI OffsetClipRgn(HDC hdc,int x,int y)
*/
HB_FUNC( WAOFFSETCLIPRGN )
{
  wa_ret_int(OffsetClipRgn(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI int WINAPI OffsetRgn(HRGN hrgn,int x,int y)
*/
HB_FUNC( WAOFFSETRGN )
{
  wa_ret_int(OffsetRgn(wa_par_HRGN(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PatBlt(HDC hdc,int x,int y,int w,int h,DWORD rop)
*/
HB_FUNC( WAPATBLT )
{
  wa_ret_BOOL(PatBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_DWORD(6)));
}

/*
WINGDIAPI WINBOOL WINAPI Pie(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WAPIE )
{
  wa_ret_BOOL(Pie(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI PlayMetaFile(HDC hdc,HMETAFILE hmf)
*/
HB_FUNC( WAPLAYMETAFILE )
{
  wa_ret_BOOL(PlayMetaFile(wa_par_HDC(1), wa_par_HMETAFILE(2)));
}

/*
WINGDIAPI WINBOOL WINAPI PaintRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WAPAINTRGN )
{
  wa_ret_BOOL(PaintRgn(wa_par_HDC(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyPolygon(HDC hdc,CONST POINT *apt,CONST INT *asz,int csz)
*/

/*
WINGDIAPI WINBOOL WINAPI PtInRegion(HRGN hrgn,int x,int y)
*/
HB_FUNC( WAPTINREGION )
{
  wa_ret_BOOL(PtInRegion(wa_par_HRGN(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PtVisible(HDC hdc,int x,int y)
*/
HB_FUNC( WAPTVISIBLE )
{
  wa_ret_BOOL(PtVisible(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI RectInRegion(HRGN hrgn,CONST RECT *lprect)
*/
HB_FUNC( WARECTINREGION )
{
  wa_ret_BOOL(RectInRegion(wa_par_HRGN(1), static_cast<CONST RECT *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI RectVisible(HDC hdc,CONST RECT *lprect)
*/
HB_FUNC( WARECTVISIBLE )
{
  wa_ret_BOOL(RectVisible(wa_par_HDC(1), static_cast<CONST RECT *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI Rectangle(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WARECTANGLE )
{
  wa_ret_BOOL(Rectangle(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI RestoreDC(HDC hdc,int nSavedDC)
*/
HB_FUNC( WARESTOREDC )
{
  wa_ret_BOOL(RestoreDC(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI HDC WINAPI ResetDCA(HDC hdc,CONST DEVMODEA *lpdm)
*/

/*
WINGDIAPI HDC WINAPI ResetDCW(HDC hdc,CONST DEVMODEW *lpdm)
*/

/*
WINGDIAPI UINT WINAPI RealizePalette(HDC hdc)
*/
HB_FUNC( WAREALIZEPALETTE )
{
  wa_ret_UINT(RealizePalette(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceA(LPCSTR lpFileName)
*/
HB_FUNC( WAREMOVEFONTRESOURCEA )
{
  wa_ret_BOOL(RemoveFontResourceA(wa_par_LPCSTR(1)));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceW(LPCWSTR lpFileName)
*/
HB_FUNC( WAREMOVEFONTRESOURCEW )
{
  wa_ret_BOOL(RemoveFontResourceW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WAREMOVEFONTRESOURCE )
{
  void * str1;
  wa_ret_BOOL(RemoveFontResource(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINGDIAPI WINBOOL WINAPI RoundRect(HDC hdc,int left,int top,int right,int bottom,int width,int height)
*/
HB_FUNC( WAROUNDRECT )
{
  wa_ret_BOOL(RoundRect(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7)));
}

/*
WINGDIAPI WINBOOL WINAPI ResizePalette(HPALETTE hpal,UINT n)
*/
HB_FUNC( WARESIZEPALETTE )
{
  wa_ret_BOOL(ResizePalette(wa_par_HPALETTE(1), wa_par_UINT(2)));
}

/*
WINGDIAPI int WINAPI SaveDC(HDC hdc)
*/
HB_FUNC( WASAVEDC )
{
  wa_ret_int(SaveDC(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI SelectClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WASELECTCLIPRGN )
{
  wa_ret_int(SelectClipRgn(wa_par_HDC(1), wa_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI ExtSelectClipRgn(HDC hdc,HRGN hrgn,int mode)
*/
HB_FUNC( WAEXTSELECTCLIPRGN )
{
  wa_ret_int(ExtSelectClipRgn(wa_par_HDC(1), wa_par_HRGN(2), wa_par_int(3)));
}

/*
WINGDIAPI int WINAPI SetMetaRgn(HDC hdc)
*/
HB_FUNC( WASETMETARGN )
{
  wa_ret_int(SetMetaRgn(wa_par_HDC(1)));
}

/*
WINGDIAPI HGDIOBJ WINAPI SelectObject(HDC hdc,HGDIOBJ h)
*/
HB_FUNC( WASELECTOBJECT )
{
  wa_ret_HGDIOBJ(SelectObject(wa_par_HDC(1), wa_par_HGDIOBJ(2)));
}

/*
WINGDIAPI HPALETTE WINAPI SelectPalette(HDC hdc,HPALETTE hPal,WINBOOL bForceBkgd)
*/
HB_FUNC( WASELECTPALETTE )
{
  wa_ret_HPALETTE(SelectPalette(wa_par_HDC(1), wa_par_HPALETTE(2), wa_par_BOOL(3)));
}

/*
WINGDIAPI COLORREF WINAPI SetBkColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WASETBKCOLOR )
{
  wa_ret_COLORREF(SetBkColor(wa_par_HDC(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetDCBrushColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WASETDCBRUSHCOLOR )
{
  wa_ret_COLORREF(SetDCBrushColor(wa_par_HDC(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetDCPenColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WASETDCPENCOLOR )
{
  wa_ret_COLORREF(SetDCPenColor(wa_par_HDC(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI int WINAPI SetBkMode(HDC hdc,int mode)
*/
HB_FUNC( WASETBKMODE )
{
  wa_ret_int(SetBkMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI LONG WINAPI SetBitmapBits(HBITMAP hbm,DWORD cb,CONST VOID *pvBits)
*/

/*
WINGDIAPI UINT WINAPI SetBoundsRect(HDC hdc,CONST RECT *lprect,UINT flags)
*/

/*
WINGDIAPI int WINAPI SetDIBits(HDC hdc,HBITMAP hbm,UINT start,UINT cLines,CONST VOID *lpBits,CONST BITMAPINFO *lpbmi,UINT ColorUse)
*/

/*
WINGDIAPI int WINAPI SetDIBitsToDevice(HDC hdc,int xDest,int yDest,DWORD w,DWORD h,int xSrc,int ySrc,UINT StartScan,UINT cLines,CONST VOID *lpvBits,CONST BITMAPINFO *lpbmi,UINT ColorUse)
*/

/*
WINGDIAPI DWORD WINAPI SetMapperFlags(HDC hdc,DWORD flags)
*/
HB_FUNC( WASETMAPPERFLAGS )
{
  wa_ret_DWORD(SetMapperFlags(wa_par_HDC(1), wa_par_DWORD(2)));
}

/*
WINGDIAPI int WINAPI SetGraphicsMode(HDC hdc,int iMode)
*/
HB_FUNC( WASETGRAPHICSMODE )
{
  wa_ret_int(SetGraphicsMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetMapMode(HDC hdc,int iMode)
*/
HB_FUNC( WASETMAPMODE )
{
  wa_ret_int(SetMapMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI DWORD WINAPI SetLayout(HDC hdc,DWORD l)
*/
HB_FUNC( WASETLAYOUT )
{
  wa_ret_DWORD(SetLayout(wa_par_HDC(1), wa_par_DWORD(2)));
}

/*
WINGDIAPI DWORD WINAPI GetLayout(HDC hdc)
*/
HB_FUNC( WAGETLAYOUT )
{
  wa_ret_DWORD(GetLayout(wa_par_HDC(1)));
}

/*
WINGDIAPI HMETAFILE WINAPI SetMetaFileBitsEx(UINT cbBuffer,CONST BYTE *lpData)
*/

/*
WINGDIAPI UINT WINAPI SetPaletteEntries(HPALETTE hpal,UINT iStart,UINT cEntries,CONST PALETTEENTRY *pPalEntries)
*/

/*
WINGDIAPI COLORREF WINAPI SetPixel(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WASETPIXEL )
{
  wa_ret_COLORREF(SetPixel(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelV(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WASETPIXELV )
{
  wa_ret_BOOL(SetPixelV(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelFormat(HDC hdc,int format,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/
HB_FUNC( WASETPIXELFORMAT )
{
  wa_ret_BOOL(SetPixelFormat(wa_par_HDC(1), wa_par_int(2), static_cast<CONST PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(3))));
}

/*
WINGDIAPI int WINAPI SetPolyFillMode(HDC hdc,int mode)
*/
HB_FUNC( WASETPOLYFILLMODE )
{
  wa_ret_int(SetPolyFillMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI StretchBlt(HDC hdcDest,int xDest,int yDest,int wDest,int hDest,HDC hdcSrc,int xSrc,int ySrc,int wSrc,int hSrc,DWORD rop)
*/
HB_FUNC( WASTRETCHBLT )
{
  wa_ret_BOOL(StretchBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_int(9), wa_par_int(10), wa_par_DWORD(11)));
}

/*
WINGDIAPI WINBOOL WINAPI SetRectRgn(HRGN hrgn,int left,int top,int right,int bottom)
*/
HB_FUNC( WASETRECTRGN )
{
  wa_ret_BOOL(SetRectRgn(wa_par_HRGN(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINGDIAPI int WINAPI StretchDIBits(HDC hdc,int xDest,int yDest,int DestWidth,int DestHeight,int xSrc,int ySrc,int SrcWidth,int SrcHeight,CONST VOID *lpBits,CONST BITMAPINFO *lpbmi,UINT iUsage,DWORD rop)
*/

/*
WINGDIAPI int WINAPI SetROP2(HDC hdc,int rop2)
*/
HB_FUNC( WASETROP2 )
{
  wa_ret_int(SetROP2(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetStretchBltMode(HDC hdc,int mode)
*/
HB_FUNC( WASETSTRETCHBLTMODE )
{
  wa_ret_int(SetStretchBltMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI UINT WINAPI SetSystemPaletteUse(HDC hdc,UINT use)
*/
HB_FUNC( WASETSYSTEMPALETTEUSE )
{
  wa_ret_UINT(SetSystemPaletteUse(wa_par_HDC(1), wa_par_UINT(2)));
}

/*
WINGDIAPI int WINAPI SetTextCharacterExtra(HDC hdc,int extra)
*/
HB_FUNC( WASETTEXTCHARACTEREXTRA )
{
  wa_ret_int(SetTextCharacterExtra(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetTextColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WASETTEXTCOLOR )
{
  wa_ret_COLORREF(SetTextColor(wa_par_HDC(1), wa_par_COLORREF(2)));
}

/*
WINGDIAPI UINT WINAPI SetTextAlign(HDC hdc,UINT align)
*/
HB_FUNC( WASETTEXTALIGN )
{
  wa_ret_UINT(SetTextAlign(wa_par_HDC(1), wa_par_UINT(2)));
}

/*
WINGDIAPI WINBOOL WINAPI SetTextJustification(HDC hdc,int extra,int count)
*/
HB_FUNC( WASETTEXTJUSTIFICATION )
{
  wa_ret_BOOL(SetTextJustification(wa_par_HDC(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateColors(HDC hdc)
*/
HB_FUNC( WAUPDATECOLORS )
{
  wa_ret_BOOL(UpdateColors(wa_par_HDC(1)));
}

/*
WINGDIAPI BOOL WINAPI GdiRegisterDdraw(PGDIREGISTERDDRAWPACKET pPacket, GDIMARSHALLOC *ppfnGdiAlloc)
*/

/*
WINGDIAPI ULONG WINAPI GdiMarshalSize(VOID)
*/

/*
WINGDIAPI VOID WINAPI GdiMarshal(DWORD dwProcessIdTo, HGDIOBJ hGdiObj, PVOID pData, ULONG ulFlags)
*/

/*
WINGDIAPI HGDIOBJ WINAPI GdiUnmarshal(PVOID pData, ULONG ulFlags)
*/

/*
WINGDIAPI WINBOOL WINAPI AlphaBlend(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,BLENDFUNCTION ftn)
*/
HB_FUNC( WAALPHABLEND )
{
  wa_ret_BOOL(AlphaBlend(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_int(9), wa_par_int(10), *static_cast<BLENDFUNCTION*>(winapi_get_ptr(11))));
}

/*
WINGDIAPI WINBOOL WINAPI GdiAlphaBlend(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,BLENDFUNCTION ftn)
*/
HB_FUNC( WAGDIALPHABLEND )
{
  wa_ret_BOOL(GdiAlphaBlend(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_int(9), wa_par_int(10), *static_cast<BLENDFUNCTION*>(winapi_get_ptr(11))));
}

/*
WINGDIAPI WINBOOL WINAPI TransparentBlt(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,UINT crTransparent)
*/
HB_FUNC( WATRANSPARENTBLT )
{
  wa_ret_BOOL(TransparentBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_int(9), wa_par_int(10), wa_par_UINT(11)));
}

/*
WINGDIAPI WINBOOL WINAPI GdiTransparentBlt(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,UINT crTransparent)
*/
HB_FUNC( WAGDITRANSPARENTBLT )
{
  wa_ret_BOOL(GdiTransparentBlt(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HDC(6), wa_par_int(7), wa_par_int(8), wa_par_int(9), wa_par_int(10), wa_par_UINT(11)));
}

/*
WINGDIAPI WINBOOL WINAPI GradientFill(HDC hdc,PTRIVERTEX pVertex,ULONG nVertex,PVOID pMesh,ULONG nMesh,ULONG ulMode)
*/

/*
WINGDIAPI WINBOOL WINAPI GdiGradientFill(HDC hdc,PTRIVERTEX pVertex,ULONG nVertex,PVOID pMesh,ULONG nMesh,ULONG ulMode)
*/

/*
WINGDIAPI WINBOOL WINAPI PlayMetaFileRecord(HDC hdc,LPHANDLETABLE lpHandleTable,LPMETARECORD lpMR,UINT noObjs)
*/

/*
WINGDIAPI WINBOOL WINAPI EnumMetaFile(HDC hdc,HMETAFILE hmf,MFENUMPROC lpProc,LPARAM lParam)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI CloseEnhMetaFile(HDC hdc)
*/
HB_FUNC( WACLOSEENHMETAFILE )
{
  wa_ret_HENHMETAFILE(CloseEnhMetaFile(wa_par_HDC(1)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileA(HENHMETAFILE hEnh,LPCSTR lpFileName)
*/
HB_FUNC( WACOPYENHMETAFILEA )
{
  wa_ret_HENHMETAFILE(CopyEnhMetaFileA(wa_par_HENHMETAFILE(1), wa_par_LPCSTR(2)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileW(HENHMETAFILE hEnh,LPCWSTR lpFileName)
*/
HB_FUNC( WACOPYENHMETAFILEW )
{
  wa_ret_HENHMETAFILE(CopyEnhMetaFileW(wa_par_HENHMETAFILE(1), wa_par_LPCWSTR(2)));
}

HB_FUNC( WACOPYENHMETAFILE )
{
  void * str2;
  wa_ret_HENHMETAFILE(CopyEnhMetaFile(wa_par_HENHMETAFILE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileA(HDC hdc,LPCSTR lpFilename,CONST RECT *lprc,LPCSTR lpDesc)
*/
HB_FUNC( WACREATEENHMETAFILEA )
{
  wa_ret_HDC(CreateEnhMetaFileA(wa_par_HDC(1), wa_par_LPCSTR(2), static_cast<CONST RECT *>(winapi_get_ptr(3)), wa_par_LPCSTR(4)));
}

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileW(HDC hdc,LPCWSTR lpFilename,CONST RECT *lprc,LPCWSTR lpDesc)
*/
HB_FUNC( WACREATEENHMETAFILEW )
{
  wa_ret_HDC(CreateEnhMetaFileW(wa_par_HDC(1), wa_par_LPCWSTR(2), static_cast<CONST RECT *>(winapi_get_ptr(3)), wa_par_LPCWSTR(4)));
}

HB_FUNC( WACREATEENHMETAFILE )
{
  void * str2;
  void * str4;
  wa_ret_HDC(CreateEnhMetaFile(wa_par_HDC(1), HB_PARSTR(2, &str2, nullptr), static_cast<CONST RECT *>(winapi_get_ptr(3)), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str4);
}

/*
WINGDIAPI WINBOOL WINAPI DeleteEnhMetaFile(HENHMETAFILE hmf)
*/
HB_FUNC( WADELETEENHMETAFILE )
{
  wa_ret_BOOL(DeleteEnhMetaFile(wa_par_HENHMETAFILE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI EnumEnhMetaFile(HDC hdc,HENHMETAFILE hmf,ENHMFENUMPROC lpProc,LPVOID lpParam,CONST RECT *lpRect)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WAGETENHMETAFILEA )
{
  wa_ret_HENHMETAFILE(GetEnhMetaFileA(wa_par_LPCSTR(1)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WAGETENHMETAFILEW )
{
  wa_ret_HENHMETAFILE(GetEnhMetaFileW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WAGETENHMETAFILE )
{
  void * str1;
  wa_ret_HENHMETAFILE(GetEnhMetaFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileBits(HENHMETAFILE hEMF,UINT nSize,LPBYTE lpData)
*/

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionA(HENHMETAFILE hemf,UINT cchBuffer,LPSTR lpDescription)
*/
HB_FUNC( WAGETENHMETAFILEDESCRIPTIONA )
{
  wa_ret_UINT(GetEnhMetaFileDescriptionA(wa_par_HENHMETAFILE(1), wa_par_UINT(2), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionW(HENHMETAFILE hemf,UINT cchBuffer,LPWSTR lpDescription)
*/
HB_FUNC( WAGETENHMETAFILEDESCRIPTIONW )
{
  wa_ret_UINT(GetEnhMetaFileDescriptionW(wa_par_HENHMETAFILE(1), wa_par_UINT(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileHeader(HENHMETAFILE hemf,UINT nSize,LPENHMETAHEADER lpEnhMetaHeader)
*/

/*
WINGDIAPI UINT WINAPI GetEnhMetaFilePaletteEntries(HENHMETAFILE hemf,UINT nNumEntries,LPPALETTEENTRY lpPaletteEntries)
*/

/*
WINGDIAPI UINT WINAPI GetEnhMetaFilePixelFormat(HENHMETAFILE hemf,UINT cbBuffer,PIXELFORMATDESCRIPTOR *ppfd)
*/
HB_FUNC( WAGETENHMETAFILEPIXELFORMAT )
{
  wa_ret_UINT(GetEnhMetaFilePixelFormat(wa_par_HENHMETAFILE(1), wa_par_UINT(2), static_cast<PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(3))));
}

/*
WINGDIAPI UINT WINAPI GetWinMetaFileBits(HENHMETAFILE hemf,UINT cbData16,LPBYTE pData16,INT iMapMode,HDC hdcRef)
*/

/*
WINGDIAPI WINBOOL WINAPI PlayEnhMetaFile(HDC hdc,HENHMETAFILE hmf,CONST RECT *lprect)
*/
HB_FUNC( WAPLAYENHMETAFILE )
{
  wa_ret_BOOL(PlayEnhMetaFile(wa_par_HDC(1), wa_par_HENHMETAFILE(2), static_cast<CONST RECT *>(winapi_get_ptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI PlayEnhMetaFileRecord(HDC hdc,LPHANDLETABLE pht,CONST ENHMETARECORD *pmr,UINT cht)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI SetEnhMetaFileBits(UINT nSize,CONST BYTE *pb)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI SetWinMetaFileBits(UINT nSize,CONST BYTE *lpMeta16Data,HDC hdcRef,CONST METAFILEPICT *lpMFP)
*/

/*
WINGDIAPI WINBOOL WINAPI GdiComment(HDC hdc,UINT nSize,CONST BYTE *lpData)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextMetricsA(HDC hdc,LPTEXTMETRICA lptm)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextMetricsW(HDC hdc,LPTEXTMETRICW lptm)
*/

/*
WINGDIAPI WINBOOL WINAPI AngleArc(HDC hdc,int x,int y,DWORD r,FLOAT StartAngle,FLOAT SweepAngle)
*/
HB_FUNC( WAANGLEARC )
{
  wa_ret_BOOL(AngleArc(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_DWORD(4), wa_par_FLOAT(5), wa_par_FLOAT(6)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyPolyline(HDC hdc,CONST POINT *apt,CONST DWORD *asz,DWORD csz)
*/

/*
WINGDIAPI WINBOOL WINAPI GetWorldTransform(HDC hdc,LPXFORM lpxf)
*/

/*
WINGDIAPI WINBOOL WINAPI SetWorldTransform(HDC hdc,CONST XFORM *lpxf)
*/

/*
WINGDIAPI WINBOOL WINAPI ModifyWorldTransform(HDC hdc,CONST XFORM *lpxf,DWORD mode)
*/

/*
WINGDIAPI WINBOOL WINAPI CombineTransform(LPXFORM lpxfOut,CONST XFORM *lpxf1,CONST XFORM *lpxf2)
*/

/*
WINGDIAPI HBITMAP WINAPI CreateDIBSection(HDC hdc,CONST BITMAPINFO *lpbmi,UINT usage,VOID **ppvBits,HANDLE hSection,DWORD offset)
*/

/*
WINGDIAPI UINT WINAPI GetDIBColorTable(HDC hdc,UINT iStart,UINT cEntries,RGBQUAD *prgbq)
*/

/*
WINGDIAPI UINT WINAPI SetDIBColorTable(HDC hdc,UINT iStart,UINT cEntries,CONST RGBQUAD *prgbq)
*/

/*
WINGDIAPI WINBOOL WINAPI SetColorAdjustment(HDC hdc,CONST COLORADJUSTMENT *lpca)
*/
HB_FUNC( WASETCOLORADJUSTMENT )
{
  wa_ret_BOOL(SetColorAdjustment(wa_par_HDC(1), static_cast<CONST COLORADJUSTMENT*>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetColorAdjustment(HDC hdc,LPCOLORADJUSTMENT lpca)
*/
HB_FUNC( WAGETCOLORADJUSTMENT )
{
  wa_ret_BOOL(GetColorAdjustment(wa_par_HDC(1), static_cast<LPCOLORADJUSTMENT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI HPALETTE WINAPI CreateHalftonePalette(HDC hdc)
*/
HB_FUNC( WACREATEHALFTONEPALETTE )
{
  wa_ret_HPALETTE(CreateHalftonePalette(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI StartDocA(HDC hdc,CONST DOCINFOA *lpdi)
*/

/*
WINGDIAPI int WINAPI StartDocW(HDC hdc,CONST DOCINFOW *lpdi)
*/

/*
WINGDIAPI int WINAPI EndDoc(HDC hdc)
*/
HB_FUNC( WAENDDOC )
{
  wa_ret_int(EndDoc(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI StartPage(HDC hdc)
*/
HB_FUNC( WASTARTPAGE )
{
  wa_ret_int(StartPage(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI EndPage(HDC hdc)
*/
HB_FUNC( WAENDPAGE )
{
  wa_ret_int(EndPage(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI AbortDoc(HDC hdc)
*/
HB_FUNC( WAABORTDOC )
{
  wa_ret_int(AbortDoc(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI SetAbortProc(HDC hdc,ABORTPROC lpProc)
*/

/*
WINGDIAPI WINBOOL WINAPI AbortPath(HDC hdc)
*/
HB_FUNC( WAABORTPATH )
{
  wa_ret_BOOL(AbortPath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI ArcTo(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WAARCTO )
{
  wa_ret_BOOL(ArcTo(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI BeginPath(HDC hdc)
*/
HB_FUNC( WABEGINPATH )
{
  wa_ret_BOOL(BeginPath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI CloseFigure(HDC hdc)
*/
HB_FUNC( WACLOSEFIGURE )
{
  wa_ret_BOOL(CloseFigure(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI EndPath(HDC hdc)
*/
HB_FUNC( WAENDPATH )
{
  wa_ret_BOOL(EndPath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI FillPath(HDC hdc)
*/
HB_FUNC( WAFILLPATH )
{
  wa_ret_BOOL(FillPath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI FlattenPath(HDC hdc)
*/
HB_FUNC( WAFLATTENPATH )
{
  wa_ret_BOOL(FlattenPath(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetPath(HDC hdc,LPPOINT apt,LPBYTE aj,int cpt)
*/

/*
WINGDIAPI HRGN WINAPI PathToRegion(HDC hdc)
*/
HB_FUNC( WAPATHTOREGION )
{
  wa_ret_HRGN(PathToRegion(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyDraw(HDC hdc,CONST POINT *apt,CONST BYTE *aj,int cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI SelectClipPath(HDC hdc,int mode)
*/
HB_FUNC( WASELECTCLIPPATH )
{
  wa_ret_BOOL(SelectClipPath(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetArcDirection(HDC hdc,int dir)
*/
HB_FUNC( WASETARCDIRECTION )
{
  wa_ret_int(SetArcDirection(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI SetMiterLimit(HDC hdc,FLOAT limit,PFLOAT old)
*/
HB_FUNC( WASETMITERLIMIT )
{
  FLOAT old;
  wa_ret_BOOL(SetMiterLimit(wa_par_HDC(1), static_cast<FLOAT>(hb_parnd(2)), &old));
  wa_stor_FLOAT(old, 3);
}

/*
WINGDIAPI WINBOOL WINAPI StrokeAndFillPath(HDC hdc)
*/
HB_FUNC( WASTROKEANDFILLPATH )
{
  wa_ret_BOOL(StrokeAndFillPath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI StrokePath(HDC hdc)
*/
HB_FUNC( WASTROKEPATH )
{
  wa_ret_BOOL(StrokePath(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI WidenPath(HDC hdc)
*/
HB_FUNC( WAWIDENPATH )
{
  wa_ret_BOOL(WidenPath(wa_par_HDC(1)));
}

/*
WINGDIAPI HPEN WINAPI ExtCreatePen(DWORD iPenStyle,DWORD cWidth,CONST LOGBRUSH *plbrush,DWORD cStyle,CONST DWORD *pstyle)
*/

/*
WINGDIAPI WINBOOL WINAPI GetMiterLimit(HDC hdc,PFLOAT plimit)
*/
HB_FUNC( WAGETMITERLIMIT )
{
  FLOAT limit;
  wa_ret_BOOL(GetMiterLimit(wa_par_HDC(1), &limit));
  wa_stor_FLOAT(limit, 2);
}

/*
WINGDIAPI int WINAPI GetArcDirection(HDC hdc)
*/
HB_FUNC( WAGETARCDIRECTION )
{
  wa_ret_int(GetArcDirection(wa_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetObjectA(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WAGETOBJECTA )
{
  wa_ret_int(GetObjectA(wa_par_HANDLE(1), wa_par_int(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI int WINAPI GetObjectW(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WAGETOBJECTW )
{
  wa_ret_int(GetObjectW(wa_par_HANDLE(1), wa_par_int(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI MoveToEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WAMOVETOEX )
{
  wa_ret_BOOL(MoveToEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI TextOutA(HDC hdc,int x,int y,LPCSTR lpString,int c)
*/
HB_FUNC( WATEXTOUTA )
{
  wa_ret_BOOL(TextOutA(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCSTR(4), wa_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI TextOutW(HDC hdc,int x,int y,LPCWSTR lpString,int c)
*/
HB_FUNC( WATEXTOUTW )
{
  wa_ret_BOOL(TextOutW(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCWSTR(4), wa_par_int(5)));
}

HB_FUNC( WATEXTOUT )
{
  void * str4;
  wa_ret_BOOL(TextOut(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), HB_PARSTR(4, &str4, nullptr), wa_par_int(5)));
  hb_strfree(str4);
}

/*
WINGDIAPI WINBOOL WINAPI ExtTextOutA(HDC hdc,int x,int y,UINT options,CONST RECT *lprect,LPCSTR lpString,UINT c,CONST INT *lpDx)
*/

/*
WINGDIAPI WINBOOL WINAPI ExtTextOutW(HDC hdc,int x,int y,UINT options,CONST RECT *lprect,LPCWSTR lpString,UINT c,CONST INT *lpDx)
*/

/*
WINGDIAPI WINBOOL WINAPI PolyTextOutA(HDC hdc,CONST POLYTEXTA *ppt,int nstrings)
*/

/*
WINGDIAPI WINBOOL WINAPI PolyTextOutW(HDC hdc,CONST POLYTEXTW *ppt,int nstrings)
*/

/*
WINGDIAPI HRGN WINAPI CreatePolygonRgn(CONST POINT *pptl,int cPoint,int iMode)
*/
HB_FUNC( WACREATEPOLYGONRGN )
{
  wa_ret_HRGN(CreatePolygonRgn(static_cast<CONST POINT *>(winapi_get_ptr(1)), wa_par_int(2), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI DPtoLP(HDC hdc,LPPOINT lppt,int c)
*/
HB_FUNC( WADPTOLP )
{
  wa_ret_BOOL(DPtoLP(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2)), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI LPtoDP(HDC hdc,LPPOINT lppt,int c)
*/
HB_FUNC( WALPTODP )
{
  wa_ret_BOOL(LPtoDP(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2)), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI Polygon(HDC hdc,CONST POINT *apt,int cpt)
*/
HB_FUNC( WAPOLYGON )
{
  wa_ret_BOOL(Polygon(wa_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI Polyline(HDC hdc,CONST POINT *apt,int cpt)
*/
HB_FUNC( WAPOLYLINE )
{
  wa_ret_BOOL(Polyline(wa_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), wa_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyBezier(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WAPOLYBEZIER )
{
  wa_ret_BOOL(PolyBezier(wa_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), wa_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyBezierTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WAPOLYBEZIERTO )
{
  wa_ret_BOOL(PolyBezierTo(wa_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), wa_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolylineTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WAPOLYLINETO )
{
  wa_ret_BOOL(PolylineTo(wa_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), wa_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI SetViewportExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/
HB_FUNC( WASETVIEWPORTEXTEX )
{
  wa_ret_BOOL(SetViewportExtEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WASETVIEWPORTORGEX )
{
  wa_ret_BOOL(SetViewportOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetWindowExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/
HB_FUNC( WASETWINDOWEXTEX )
{
  wa_ret_BOOL(SetWindowExtEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WASETWINDOWORGEX )
{
  wa_ret_BOOL(SetWindowOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI OffsetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WAOFFSETVIEWPORTORGEX )
{
  wa_ret_BOOL(OffsetViewportOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI OffsetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WAOFFSETWINDOWORGEX )
{
  wa_ret_BOOL(OffsetWindowOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI ScaleViewportExtEx(HDC hdc,int xn,int dx,int yn,int yd,LPSIZE lpsz)
*/
HB_FUNC( WASCALEVIEWPORTEXTEX )
{
  wa_ret_BOOL(ScaleViewportExtEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), static_cast<LPSIZE>(winapi_get_ptr(6))));
}

/*
WINGDIAPI WINBOOL WINAPI ScaleWindowExtEx(HDC hdc,int xn,int xd,int yn,int yd,LPSIZE lpsz)
*/
HB_FUNC( WASCALEWINDOWEXTEX )
{
  wa_ret_BOOL(ScaleWindowExtEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), static_cast<LPSIZE>(winapi_get_ptr(6))));
}

/*
WINGDIAPI WINBOOL WINAPI SetBitmapDimensionEx(HBITMAP hbm,int w,int h,LPSIZE lpsz)
*/
HB_FUNC( WASETBITMAPDIMENSIONEX )
{
  wa_ret_BOOL(SetBitmapDimensionEx(wa_par_HBITMAP(1), wa_par_int(2), wa_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetBrushOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WASETBRUSHORGEX )
{
  wa_ret_BOOL(SetBrushOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI int WINAPI GetTextFaceA(HDC hdc,int c,LPSTR lpName)
*/
HB_FUNC( WAGETTEXTFACEA )
{
  wa_ret_int(GetTextFaceA(wa_par_HDC(1), wa_par_int(2), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINGDIAPI int WINAPI GetTextFaceW(HDC hdc,int c,LPWSTR lpName)
*/
HB_FUNC( WAGETTEXTFACEW )
{
  wa_ret_int(GetTextFaceW(wa_par_HDC(1), wa_par_int(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINGDIAPI DWORD WINAPI GetKerningPairsA(HDC hdc,DWORD nPairs,LPKERNINGPAIR lpKernPair)
*/

/*
WINGDIAPI DWORD WINAPI GetKerningPairsW(HDC hdc,DWORD nPairs,LPKERNINGPAIR lpKernPair)
*/

/*
WINGDIAPI WINBOOL WINAPI GetDCOrgEx(HDC hdc,LPPOINT lppt)
*/
HB_FUNC( WAGETDCORGEX )
{
  wa_ret_BOOL(GetDCOrgEx(wa_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI FixBrushOrgEx(HDC hdc,int x,int y,LPPOINT ptl)
*/
HB_FUNC( WAFIXBRUSHORGEX )
{
  wa_ret_BOOL(FixBrushOrgEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI UnrealizeObject(HGDIOBJ h)
*/
HB_FUNC( WAUNREALIZEOBJECT )
{
  wa_ret_BOOL(UnrealizeObject(wa_par_HGDIOBJ(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GdiFlush(void)
*/
HB_FUNC( WAGDIFLUSH )
{
  wa_ret_BOOL(GdiFlush());
}

/*
WINGDIAPI DWORD WINAPI GdiSetBatchLimit(DWORD dw)
*/
HB_FUNC( WAGDISETBATCHLIMIT )
{
  wa_ret_DWORD(GdiSetBatchLimit(wa_par_DWORD(1)));
}

/*
WINGDIAPI DWORD WINAPI GdiGetBatchLimit(void)
*/
HB_FUNC( WAGDIGETBATCHLIMIT )
{
  wa_ret_DWORD(GdiGetBatchLimit());
}

/*
WINGDIAPI int WINAPI SetICMMode(HDC hdc,int mode)
*/
HB_FUNC( WASETICMMODE )
{
  wa_ret_int(SetICMMode(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI CheckColorsInGamut(HDC hdc,LPVOID lpRGBTriple,LPVOID dlpBuffer,DWORD nCount)
*/

/*
WINGDIAPI HCOLORSPACE WINAPI GetColorSpace(HDC hdc)
*/
HB_FUNC( WAGETCOLORSPACE )
{
  wa_ret_HCOLORSPACE(GetColorSpace(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetLogColorSpaceA(HCOLORSPACE hColorSpace,LPLOGCOLORSPACEA lpBuffer,DWORD nSize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetLogColorSpaceW(HCOLORSPACE hColorSpace,LPLOGCOLORSPACEW lpBuffer,DWORD nSize)
*/

/*
WINGDIAPI HCOLORSPACE WINAPI CreateColorSpaceA(LPLOGCOLORSPACEA lplcs)
*/

/*
WINGDIAPI HCOLORSPACE WINAPI CreateColorSpaceW(LPLOGCOLORSPACEW lplcs)
*/

/*
WINGDIAPI HCOLORSPACE WINAPI SetColorSpace(HDC hdc,HCOLORSPACE hcs)
*/
HB_FUNC( WASETCOLORSPACE )
{
  wa_ret_HCOLORSPACE(SetColorSpace(wa_par_HDC(1), wa_par_HCOLORSPACE(2)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteColorSpace(HCOLORSPACE hcs)
*/
HB_FUNC( WADELETECOLORSPACE )
{
  wa_ret_BOOL(DeleteColorSpace(wa_par_HCOLORSPACE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileA(HDC hdc,LPDWORD pBufSize,LPSTR pszFilename)
*/
HB_FUNC( WAGETICMPROFILEA )
{
  DWORD BufSize;
  wa_ret_BOOL(GetICMProfileA(wa_par_HDC(1), &BufSize, const_cast<LPSTR>(hb_parc(3))));
  wa_stor_DWORD(BufSize, 2);
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileW(HDC hdc,LPDWORD pBufSize,LPWSTR pszFilename)
*/
HB_FUNC( WAGETICMPROFILEW )
{
  DWORD BufSize;
  wa_ret_BOOL(GetICMProfileW(wa_par_HDC(1), &BufSize, reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
  wa_stor_DWORD(BufSize, 2);
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileA(HDC hdc,LPSTR lpFileName)
*/
HB_FUNC( WASETICMPROFILEA )
{
  wa_ret_BOOL(SetICMProfileA(wa_par_HDC(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileW(HDC hdc,LPWSTR lpFileName)
*/
HB_FUNC( WASETICMPROFILEW )
{
  wa_ret_BOOL(SetICMProfileW(wa_par_HDC(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2)))));
}

/*
WINGDIAPI WINBOOL WINAPI GetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WAGETDEVICEGAMMARAMP )
{
  wa_ret_BOOL(GetDeviceGammaRamp(wa_par_HDC(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI SetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WASETDEVICEGAMMARAMP )
{
  wa_ret_BOOL(SetDeviceGammaRamp(wa_par_HDC(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI ColorMatchToTarget(HDC hdc,HDC hdcTarget,DWORD action)
*/
HB_FUNC( WACOLORMATCHTOTARGET )
{
  wa_ret_BOOL(ColorMatchToTarget(wa_par_HDC(1), wa_par_HDC(2), wa_par_DWORD(3)));
}

/*
WINGDIAPI int WINAPI EnumICMProfilesA(HDC hdc,ICMENUMPROCA lpProc,LPARAM lParam)
*/

/*
WINGDIAPI int WINAPI EnumICMProfilesW(HDC hdc,ICMENUMPROCW lpProc,LPARAM lParam)
*/

/*
WINGDIAPI WINBOOL WINAPI UpdateICMRegKeyA(DWORD reserved,LPSTR lpszCMID,LPSTR lpszFileName,UINT command)
*/
HB_FUNC( WAUPDATEICMREGKEYA )
{
  wa_ret_BOOL(UpdateICMRegKeyA(wa_par_DWORD(1), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3)), wa_par_UINT(4)));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateICMRegKeyW(DWORD reserved,LPWSTR lpszCMID,LPWSTR lpszFileName,UINT command)
*/
HB_FUNC( WAUPDATEICMREGKEYW )
{
  wa_ret_BOOL(UpdateICMRegKeyW(wa_par_DWORD(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_UINT(4)));
}

/*
WINGDIAPI WINBOOL WINAPI ColorCorrectPalette(HDC hdc,HPALETTE hPal,DWORD deFirst,DWORD num)
*/
HB_FUNC( WACOLORCORRECTPALETTE )
{
  wa_ret_BOOL(ColorCorrectPalette(wa_par_HDC(1), wa_par_HPALETTE(2), wa_par_DWORD(3), wa_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI wglCopyContext(HGLRC,HGLRC,UINT)
*/
HB_FUNC( WAWGLCOPYCONTEXT )
{
  wa_ret_BOOL(wglCopyContext(wa_par_HGLRC(1), wa_par_HGLRC(2), wa_par_UINT(3)));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateContext(HDC)
*/
HB_FUNC( WAWGLCREATECONTEXT )
{
  wa_ret_HGLRC(wglCreateContext(wa_par_HDC(1)));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateLayerContext(HDC,int)
*/
HB_FUNC( WAWGLCREATELAYERCONTEXT )
{
  wa_ret_HGLRC(wglCreateLayerContext(wa_par_HDC(1), wa_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglDeleteContext(HGLRC)
*/
HB_FUNC( WAWGLDELETECONTEXT )
{
  wa_ret_BOOL(wglDeleteContext(wa_par_HGLRC(1)));
}

/*
WINGDIAPI HGLRC WINAPI wglGetCurrentContext(VOID)
*/
HB_FUNC( WAWGLGETCURRENTCONTEXT )
{
  wa_ret_HGLRC(wglGetCurrentContext());
}

/*
WINGDIAPI HDC WINAPI wglGetCurrentDC(VOID)
*/
HB_FUNC( WAWGLGETCURRENTDC )
{
  wa_ret_HDC(wglGetCurrentDC());
}

/*
WINGDIAPI PROC WINAPI wglGetProcAddress(LPCSTR)
*/

/*
WINGDIAPI WINBOOL WINAPI wglMakeCurrent(HDC,HGLRC)
*/
HB_FUNC( WAWGLMAKECURRENT )
{
  wa_ret_BOOL(wglMakeCurrent(wa_par_HDC(1), wa_par_HGLRC(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglShareLists(HGLRC,HGLRC)
*/
HB_FUNC( WAWGLSHARELISTS )
{
  wa_ret_BOOL(wglShareLists(wa_par_HGLRC(1), wa_par_HGLRC(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsA(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WAWGLUSEFONTBITMAPSA )
{
  wa_ret_BOOL(wglUseFontBitmapsA(wa_par_HDC(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsW(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WAWGLUSEFONTBITMAPSW )
{
  wa_ret_BOOL(wglUseFontBitmapsW(wa_par_HDC(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SwapBuffers(HDC)
*/
HB_FUNC( WASWAPBUFFERS )
{
  wa_ret_BOOL(SwapBuffers(wa_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontOutlinesA(HDC,DWORD,DWORD,DWORD,FLOAT,FLOAT,int,LPGLYPHMETRICSFLOAT)
*/

/*
WINGDIAPI WINBOOL WINAPI wglUseFontOutlinesW(HDC,DWORD,DWORD,DWORD,FLOAT,FLOAT,int,LPGLYPHMETRICSFLOAT)
*/

/*
WINGDIAPI WINBOOL WINAPI wglDescribeLayerPlane(HDC,int,int,UINT,LPLAYERPLANEDESCRIPTOR)
*/

/*
WINGDIAPI int WINAPI wglSetLayerPaletteEntries(HDC,int,int,int,CONST COLORREF *)
*/

/*
WINGDIAPI int WINAPI wglGetLayerPaletteEntries(HDC,int,int,int,COLORREF *)
*/

/*
WINGDIAPI WINBOOL WINAPI wglRealizeLayerPalette(HDC,int,WINBOOL)
*/
HB_FUNC( WAWGLREALIZELAYERPALETTE )
{
  wa_ret_BOOL(wglRealizeLayerPalette(wa_par_HDC(1), wa_par_int(2), wa_par_BOOL(3)));
}

/*
WINGDIAPI WINBOOL WINAPI wglSwapLayerBuffers(HDC,UINT)
*/
HB_FUNC( WAWGLSWAPLAYERBUFFERS )
{
  wa_ret_BOOL(wglSwapLayerBuffers(wa_par_HDC(1), wa_par_UINT(2)));
}

/*
WINGDIAPI DWORD WINAPI wglSwapMultipleBuffers(UINT,CONST WGLSWAP *)
*/

/*
void GetRValue(rgb) (macro)
*/
HB_FUNC( WAGETRVALUE )
{
  wa_ret_int(GetRValue(wa_par_COLORREF(1)));
}

/*
void GetGValue(rgb) (macro)
*/
HB_FUNC( WAGETGVALUE )
{
  wa_ret_int(GetGValue(wa_par_COLORREF(1)));
}

/*
void GetBValue(rgb) (macro)
*/
HB_FUNC( WAGETBVALUE )
{
  wa_ret_int(GetBValue(wa_par_COLORREF(1)));
}
