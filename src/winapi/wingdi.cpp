/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbwinuni.h"
#include "winapi.h"

/*
WINGDIAPI int WINAPI AddFontResourceA(LPCSTR)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEA )
{
  winapi_ret_int(AddFontResourceA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI int WINAPI AddFontResourceW(LPCWSTR)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEW )
{
  winapi_ret_int(AddFontResourceW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_ADDFONTRESOURCE )
{
  void * str;
  winapi_ret_int(AddFontResource(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI WINBOOL WINAPI AnimatePalette(HPALETTE hPal,UINT iStartIndex,UINT cEntries,CONST PALETTEENTRY *ppe)
*/

/*
WINGDIAPI WINBOOL WINAPI Arc(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WINAPI_ARC )
{
  winapi_ret_BOOL(Arc(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI BitBlt(HDC hdc,int x,int y,int cx,int cy,HDC hdcSrc,int x1,int y1,DWORD rop)
*/
HB_FUNC( WINAPI_BITBLT )
{
  winapi_ret_BOOL(BitBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HDC(6), winapi_par_int(7), winapi_par_int(8), winapi_par_DWORD(9)));
}

/*
WINGDIAPI WINBOOL WINAPI CancelDC(HDC hdc)
*/
HB_FUNC( WINAPI_CANCELDC )
{
  winapi_ret_BOOL(CancelDC(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI Chord(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WINAPI_CHORD )
{
  winapi_ret_BOOL(Chord(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9)));
}

/*
WINGDIAPI int WINAPI ChoosePixelFormat(HDC hdc,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/
HB_FUNC( WINAPI_CHOOSEPIXELFORMAT )
{
  winapi_ret_int(ChoosePixelFormat(winapi_par_HDC(1), static_cast<CONST PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI HMETAFILE WINAPI CloseMetaFile(HDC hdc)
*/
HB_FUNC( WINAPI_CLOSEMETAFILE )
{
  winapi_ret_HMETAFILE(CloseMetaFile(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI CombineRgn(HRGN hrgnDst,HRGN hrgnSrc1,HRGN hrgnSrc2,int iMode)
*/
HB_FUNC( WINAPI_COMBINERGN )
{
  winapi_ret_int(CombineRgn(winapi_par_HRGN(1), winapi_par_HRGN(2), winapi_par_HRGN(3), winapi_par_int(4)));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileA(HMETAFILE,LPCSTR)
*/
HB_FUNC( WINAPI_COPYMETAFILEA )
{
  winapi_ret_HMETAFILE(CopyMetaFileA(winapi_par_HMETAFILE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileW(HMETAFILE,LPCWSTR)
*/
HB_FUNC( WINAPI_COPYMETAFILEW )
{
  winapi_ret_HMETAFILE(CopyMetaFileW(winapi_par_HMETAFILE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_COPYMETAFILE )
{
  void * str;
  winapi_ret_HMETAFILE(CopyMetaFile(winapi_par_HMETAFILE(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI HBITMAP WINAPI CreateBitmap(int nWidth,int nHeight,UINT nPlanes,UINT nBitCount,CONST VOID *lpBits)
*/
HB_FUNC( WINAPI_CREATEBITMAP )
{
  winapi_ret_HBITMAP(CreateBitmap(winapi_par_int(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_UINT(4), static_cast<CONST VOID *>(hb_parptr(5))));
}

/*
WINGDIAPI HBITMAP WINAPI CreateBitmapIndirect(CONST BITMAP *pbm)
*/

/*
WINGDIAPI HBRUSH WINAPI CreateBrushIndirect(CONST LOGBRUSH *plbrush)
*/
HB_FUNC( WINAPI_CREATEBRUSHINDIRECT )
{
  winapi_ret_HBRUSH(CreateBrushIndirect(static_cast<CONST LOGBRUSH *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HBITMAP WINAPI CreateCompatibleBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WINAPI_CREATECOMPATIBLEBITMAP )
{
  winapi_ret_HBITMAP(CreateCompatibleBitmap(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI HBITMAP WINAPI CreateDiscardableBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WINAPI_CREATEDISCARDABLEBITMAP )
{
  winapi_ret_HBITMAP(CreateDiscardableBitmap(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI HDC WINAPI CreateCompatibleDC(HDC hdc)
*/
HB_FUNC( WINAPI_CREATECOMPATIBLEDC )
{
  winapi_ret_HDC(CreateCompatibleDC(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_CREATEDIBPATTERNBRUSH )
{
  winapi_ret_HBRUSH(CreateDIBPatternBrush(winapi_par_HGLOBAL(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrushPt(CONST VOID *lpPackedDIB,UINT iUsage)
*/

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WINAPI_CREATEELLIPTICRGN )
{
  winapi_ret_HRGN(CreateEllipticRgn(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgnIndirect(CONST RECT *lprect)
*/
HB_FUNC( WINAPI_CREATEELLIPTICRGNINDIRECT )
{
  winapi_ret_HRGN(CreateEllipticRgnIndirect(static_cast<CONST RECT *>(winapi_get_ptr(1))));
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
HB_FUNC( WINAPI_CREATEFONTA )
{
  winapi_ret_HFONT(CreateFontA(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_DWORD(6), winapi_par_DWORD(7), winapi_par_DWORD(8), winapi_par_DWORD(9), winapi_par_DWORD(10), winapi_par_DWORD(11), winapi_par_DWORD(12), winapi_par_DWORD(13), ( LPCSTR ) hb_parc(14)));
}

/*
WINGDIAPI HFONT WINAPI CreateFontW(int cHeight,int cWidth,int cEscapement,int cOrientation,int cWeight,DWORD bItalic,DWORD bUnderline,DWORD bStrikeOut,DWORD iCharSet,DWORD iOutPrecision,DWORD iClipPrecision,DWORD iQuality,DWORD iPitchAndFamily,LPCWSTR pszFaceName)
*/
HB_FUNC( WINAPI_CREATEFONTW )
{
  winapi_ret_HFONT(CreateFontW(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_DWORD(6), winapi_par_DWORD(7), winapi_par_DWORD(8), winapi_par_DWORD(9), winapi_par_DWORD(10), winapi_par_DWORD(11), winapi_par_DWORD(12), winapi_par_DWORD(13), ( LPCWSTR ) hb_parc(14)));
}

HB_FUNC( WINAPI_CREATEFONT )
{
  void * str;
  winapi_ret_HFONT(CreateFontW(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_DWORD(6), winapi_par_DWORD(7), winapi_par_DWORD(8), winapi_par_DWORD(9), winapi_par_DWORD(10), winapi_par_DWORD(11), winapi_par_DWORD(12), winapi_par_DWORD(13), HB_PARSTR(14, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI HBRUSH WINAPI CreateHatchBrush(int iHatch,COLORREF color)
*/
HB_FUNC( WINAPI_CREATEHATCHBRUSH )
{
  winapi_ret_HBRUSH(CreateHatchBrush(winapi_par_int(1), winapi_par_COLORREF(2)));
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
HB_FUNC( WINAPI_CREATEMETAFILEA )
{
  winapi_ret_HDC(CreateMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HDC WINAPI CreateMetaFileW(LPCWSTR pszFile)
*/
HB_FUNC( WINAPI_CREATEMETAFILEW )
{
  winapi_ret_HDC(CreateMetaFileW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_CREATEMETAFILE )
{
  void * str;
  winapi_ret_HDC(CreateMetaFile(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI HPALETTE WINAPI CreatePalette(CONST LOGPALETTE *plpal)
*/

/*
WINGDIAPI HPEN WINAPI CreatePen(int iStyle,int cWidth,COLORREF color)
*/
HB_FUNC( WINAPI_CREATEPEN )
{
  winapi_ret_HPEN(CreatePen(winapi_par_int(1), winapi_par_int(2), winapi_par_COLORREF(3)));
}

/*
WINGDIAPI HPEN WINAPI CreatePenIndirect(CONST LOGPEN *plpen)
*/
HB_FUNC( WINAPI_CREATEPENINDIRECT )
{
  winapi_ret_HPEN(CreatePenIndirect(static_cast<CONST LOGPEN *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HRGN WINAPI CreatePolyPolygonRgn(CONST POINT *pptl,CONST INT *pc,int cPoly,int iMode)
*/

/*
WINGDIAPI HBRUSH WINAPI CreatePatternBrush(HBITMAP hbm)
*/
HB_FUNC( WINAPI_CREATEPATTERNBRUSH )
{
  winapi_ret_HBRUSH(CreatePatternBrush(winapi_par_HBITMAP(1)));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WINAPI_CREATERECTRGN )
{
  winapi_ret_HRGN(CreateRectRgn(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgnIndirect(CONST RECT *lprect)
*/
HB_FUNC( WINAPI_CREATERECTRGNINDIRECT )
{
  winapi_ret_HRGN(CreateRectRgnIndirect(static_cast<CONST RECT *>(winapi_get_ptr(1))));
}

/*
WINGDIAPI HRGN WINAPI CreateRoundRectRgn(int x1,int y1,int x2,int y2,int w,int h)
*/
HB_FUNC( WINAPI_CREATEROUNDRECTRGN )
{
  winapi_ret_HRGN(CreateRoundRectRgn(winapi_par_int(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6)));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceA(DWORD fdwHidden,LPCSTR lpszFont,LPCSTR lpszFile,LPCSTR lpszPath)
*/
HB_FUNC( WINAPI_CREATESCALABLEFONTRESOURCEA )
{
  winapi_ret_BOOL(CreateScalableFontResourceA(winapi_par_DWORD(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceW(DWORD fdwHidden,LPCWSTR lpszFont,LPCWSTR lpszFile,LPCWSTR lpszPath)
*/
HB_FUNC( WINAPI_CREATESCALABLEFONTRESOURCEW )
{
  winapi_ret_BOOL(CreateScalableFontResourceW(winapi_par_DWORD(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4)));
}

HB_FUNC( WINAPI_CREATESCALABLEFONTRESOURCE )
{
  void * str2;
  void * str3;
  void * str4;
  winapi_ret_BOOL(CreateScalableFontResource(winapi_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
WINGDIAPI HBRUSH WINAPI CreateSolidBrush(COLORREF color)
*/
HB_FUNC( WINAPI_CREATESOLIDBRUSH )
{
  winapi_ret_HBRUSH(CreateSolidBrush(winapi_par_COLORREF(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteDC(HDC hdc)
*/
HB_FUNC( WINAPI_DELETEDC )
{
  winapi_ret_BOOL(DeleteDC(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteMetaFile(HMETAFILE hmf)
*/
HB_FUNC( WINAPI_DELETEMETAFILE )
{
  winapi_ret_BOOL(DeleteMetaFile(winapi_par_HMETAFILE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteObject(HGDIOBJ ho)
*/
HB_FUNC( WINAPI_DELETEOBJECT )
{
  winapi_ret_BOOL(DeleteObject(winapi_par_HGDIOBJ(1)));
}

/*
WINGDIAPI int WINAPI DescribePixelFormat(HDC hdc,int iPixelFormat,UINT nBytes,LPPIXELFORMATDESCRIPTOR ppfd)
*/
HB_FUNC( WINAPI_DESCRIBEPIXELFORMAT )
{
  winapi_ret_int(DescribePixelFormat(winapi_par_HDC(1), winapi_par_int(2), winapi_par_UINT(3), static_cast<LPPIXELFORMATDESCRIPTOR>(winapi_get_ptr(4))));
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
HB_FUNC( WINAPI_DRAWESCAPE )
{
  winapi_ret_int(DrawEscape(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINGDIAPI WINBOOL WINAPI Ellipse(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_ELLIPSE )
{
  winapi_ret_BOOL(Ellipse(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
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
HB_FUNC( WINAPI_EQUALRGN )
{
  winapi_ret_BOOL(EqualRgn(winapi_par_HRGN(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI Escape(HDC hdc,int iEscape,int cjIn,LPCSTR pvIn,LPVOID pvOut)
*/
HB_FUNC( WINAPI_ESCAPE )
{
  winapi_ret_int(Escape(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), ( LPCSTR ) hb_parc(4), static_cast<LPVOID>(hb_parptr(5))));
}

/*
WINGDIAPI int WINAPI ExtEscape(HDC hdc,int iEscape,int cjInput,LPCSTR lpInData,int cjOutput,LPSTR lpOutData)
*/
HB_FUNC( WINAPI_EXTESCAPE )
{
  winapi_ret_int(ExtEscape(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), ( LPCSTR ) hb_parc(4), winapi_par_int(5), ( LPSTR ) hb_parc(6)));
}

/*
WINGDIAPI int WINAPI ExcludeClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_EXCLUDECLIPRECT )
{
  winapi_ret_int(ExcludeClipRect(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINGDIAPI HRGN WINAPI ExtCreateRegion(CONST XFORM *lpx,DWORD nCount,CONST RGNDATA *lpData)
*/

/*
WINGDIAPI WINBOOL WINAPI ExtFloodFill(HDC hdc,int x,int y,COLORREF color,UINT type)
*/
HB_FUNC( WINAPI_EXTFLOODFILL )
{
  winapi_ret_BOOL(ExtFloodFill(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_COLORREF(4), winapi_par_UINT(5)));
}

/*
WINGDIAPI WINBOOL WINAPI FillRgn(HDC hdc,HRGN hrgn,HBRUSH hbr)
*/
HB_FUNC( WINAPI_FILLRGN )
{
  winapi_ret_BOOL(FillRgn(winapi_par_HDC(1), winapi_par_HRGN(2), winapi_par_HBRUSH(3)));
}

/*
WINGDIAPI WINBOOL WINAPI FloodFill(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WINAPI_FLOODFILL )
{
  winapi_ret_BOOL(FloodFill(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI FrameRgn(HDC hdc,HRGN hrgn,HBRUSH hbr,int w,int h)
*/
HB_FUNC( WINAPI_FRAMERGN )
{
  winapi_ret_BOOL(FrameRgn(winapi_par_HDC(1), winapi_par_HRGN(2), winapi_par_HBRUSH(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINGDIAPI int WINAPI GetROP2(HDC hdc)
*/
HB_FUNC( WINAPI_GETROP2 )
{
  winapi_ret_int(GetROP2(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetAspectRatioFilterEx(HDC hdc,LPSIZE lpsize)
*/
HB_FUNC( WINAPI_GETASPECTRATIOFILTEREX )
{
  winapi_ret_BOOL(GetAspectRatioFilterEx(winapi_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI COLORREF WINAPI GetBkColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETBKCOLOR )
{
  winapi_ret_COLORREF(GetBkColor(winapi_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetDCBrushColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETDCBRUSHCOLOR )
{
  winapi_ret_COLORREF(GetDCBrushColor(winapi_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetDCPenColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETDCPENCOLOR )
{
  winapi_ret_COLORREF(GetDCPenColor(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetBkMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETBKMODE )
{
  winapi_ret_int(GetBkMode(winapi_par_HDC(1)));
}

/*
WINGDIAPI LONG WINAPI GetBitmapBits(HBITMAP hbit,LONG cb,LPVOID lpvBits)
*/
HB_FUNC( WINAPI_GETBITMAPBITS )
{
  winapi_ret_LONG(GetBitmapBits(winapi_par_HBITMAP(1), hb_parnl(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI GetBitmapDimensionEx(HBITMAP hbit,LPSIZE lpsize)
*/
HB_FUNC( WINAPI_GETBITMAPDIMENSIONEX )
{
  winapi_ret_BOOL(GetBitmapDimensionEx(winapi_par_HBITMAP(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI UINT WINAPI GetBoundsRect(HDC hdc,LPRECT lprect,UINT flags)
*/
HB_FUNC( WINAPI_GETBOUNDSRECT )
{
  winapi_ret_UINT(GetBoundsRect(winapi_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2)), winapi_par_UINT(3)));
}

/*
WINGDIAPI WINBOOL WINAPI GetBrushOrgEx(HDC hdc,LPPOINT lppt)
*/
HB_FUNC( WINAPI_GETBRUSHORGEX )
{
  winapi_ret_BOOL(GetBrushOrgEx(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
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
HB_FUNC( WINAPI_GETCHARWIDTH32A )
{
  INT lpBuffer;
  winapi_ret_BOOL(GetCharWidth32A(winapi_par_HDC(1), winapi_par_UINT(2), winapi_par_UINT(3), &lpBuffer));
  winapi_stor_INT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidth32W(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/
HB_FUNC( WINAPI_GETCHARWIDTH32W )
{
  INT lpBuffer;
  winapi_ret_BOOL(GetCharWidth32W(winapi_par_HDC(1), winapi_par_UINT(2), winapi_par_UINT(3), &lpBuffer));
  winapi_stor_INT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatA(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/
HB_FUNC( WINAPI_GETCHARWIDTHFLOATA )
{
  FLOAT lpBuffer;
  winapi_ret_BOOL(GetCharWidthFloatA(winapi_par_HDC(1), winapi_par_UINT(2), winapi_par_UINT(3), &lpBuffer));
  winapi_stor_FLOAT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatW(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/
HB_FUNC( WINAPI_GETCHARWIDTHFLOATW )
{
  FLOAT lpBuffer;
  winapi_ret_BOOL(GetCharWidthFloatW(winapi_par_HDC(1), winapi_par_UINT(2), winapi_par_UINT(3), &lpBuffer));
  winapi_stor_FLOAT(lpBuffer, 4);
}

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsA(HDC hdc,UINT wFirst,UINT wLast,LPABC lpABC)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsW(HDC hdc,UINT wFirst,UINT wLast,LPABC lpABC)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsFloatA(HDC hdc,UINT iFirst,UINT iLast,LPABCFLOAT lpABC)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsFloatW(HDC hdc,UINT iFirst,UINT iLast,LPABCFLOAT lpABC)
*/

/*
WINGDIAPI int WINAPI GetClipBox(HDC hdc,LPRECT lprect)
*/
HB_FUNC( WINAPI_GETCLIPBOX )
{
  winapi_ret_int(GetClipBox(winapi_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI GetClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_GETCLIPRGN )
{
  winapi_ret_int(GetClipRgn(winapi_par_HDC(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI GetMetaRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_GETMETARGN )
{
  winapi_ret_int(GetMetaRgn(winapi_par_HDC(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI HGDIOBJ WINAPI GetCurrentObject(HDC hdc,UINT type)
*/
HB_FUNC( WINAPI_GETCURRENTOBJECT )
{
  winapi_ret_HGDIOBJ(GetCurrentObject(winapi_par_HDC(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI WINBOOL WINAPI GetCurrentPositionEx(HDC hdc,LPPOINT lppt)
*/
HB_FUNC( WINAPI_GETCURRENTPOSITIONEX )
{
  winapi_ret_BOOL(GetCurrentPositionEx(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI GetDeviceCaps(HDC hdc,int index)
*/
HB_FUNC( WINAPI_GETDEVICECAPS )
{
  winapi_ret_int(GetDeviceCaps(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI int WINAPI GetDIBits(HDC hdc,HBITMAP hbm,UINT start,UINT cLines,LPVOID lpvBits,LPBITMAPINFO lpbmi,UINT usage)
*/

/*
WINGDIAPI DWORD WINAPI GetFontData (HDC hdc,DWORD dwTable,DWORD dwOffset,PVOID pvBuffer,DWORD cjBuffer)
*/
HB_FUNC( WINAPI_GETFONTDATA )
{
  winapi_ret_DWORD(GetFontData(winapi_par_HDC(1), winapi_par_DWORD(2), winapi_par_DWORD(3), static_cast<PVOID>(hb_parptr(4)), winapi_par_DWORD(5)));
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
HB_FUNC( WINAPI_GETGRAPHICSMODE )
{
  winapi_ret_int(GetGraphicsMode(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetMapMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETMAPMODE )
{
  winapi_ret_int(GetMapMode(winapi_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetMetaFileBitsEx(HMETAFILE hMF,UINT cbBuffer,LPVOID lpData)
*/
HB_FUNC( WINAPI_GETMETAFILEBITSEX )
{
  winapi_ret_UINT(GetMetaFileBitsEx(winapi_par_HMETAFILE(1), winapi_par_UINT(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WINAPI_GETMETAFILEA )
{
  winapi_ret_HMETAFILE(GetMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WINAPI_GETMETAFILEW )
{
  winapi_ret_HMETAFILE(GetMetaFileW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_GETMETAFILE )
{
  void * str;
  winapi_ret_HMETAFILE(GetMetaFile(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI COLORREF WINAPI GetNearestColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_GETNEARESTCOLOR )
{
  winapi_ret_COLORREF(GetNearestColor(winapi_par_HDC(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI UINT WINAPI GetNearestPaletteIndex(HPALETTE h,COLORREF color)
*/
HB_FUNC( WINAPI_GETNEARESTPALETTEINDEX )
{
  winapi_ret_UINT(GetNearestPaletteIndex(winapi_par_HPALETTE(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI DWORD WINAPI GetObjectType(HGDIOBJ h)
*/
HB_FUNC( WINAPI_GETOBJECTTYPE )
{
  winapi_ret_DWORD(GetObjectType(winapi_par_HGDIOBJ(1)));
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
HB_FUNC( WINAPI_GETPIXEL )
{
  winapi_ret_COLORREF(GetPixel(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI int WINAPI GetPixelFormat(HDC hdc)
*/
HB_FUNC( WINAPI_GETPIXELFORMAT )
{
  winapi_ret_int(GetPixelFormat(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetPolyFillMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETPOLYFILLMODE )
{
  winapi_ret_int(GetPolyFillMode(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetRasterizerCaps(LPRASTERIZER_STATUS lpraststat,UINT cjBytes)
*/

/*
WINGDIAPI int WINAPI GetRandomRgn (HDC hdc,HRGN hrgn,INT i)
*/
HB_FUNC( WINAPI_GETRANDOMRGN )
{
  winapi_ret_int(GetRandomRgn(winapi_par_HDC(1), winapi_par_HRGN(2), winapi_par_int(3)));
}

/*
WINGDIAPI DWORD WINAPI GetRegionData(HRGN hrgn,DWORD nCount,LPRGNDATA lpRgnData)
*/

/*
WINGDIAPI int WINAPI GetRgnBox(HRGN hrgn,LPRECT lprc)
*/
HB_FUNC( WINAPI_GETRGNBOX )
{
  winapi_ret_int(GetRgnBox(winapi_par_HRGN(1), static_cast<LPRECT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI HGDIOBJ WINAPI GetStockObject(int i)
*/
HB_FUNC( WINAPI_GETSTOCKOBJECT )
{
  winapi_ret_HGDIOBJ(GetStockObject(winapi_par_int(1)));
}

/*
WINGDIAPI int WINAPI GetStretchBltMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETSTRETCHBLTMODE )
{
  winapi_ret_int(GetStretchBltMode(winapi_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetSystemPaletteEntries(HDC hdc,UINT iStart,UINT cEntries,LPPALETTEENTRY pPalEntries)
*/

/*
WINGDIAPI UINT WINAPI GetSystemPaletteUse(HDC hdc)
*/
HB_FUNC( WINAPI_GETSYSTEMPALETTEUSE )
{
  winapi_ret_UINT(GetSystemPaletteUse(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetTextCharacterExtra(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCHARACTEREXTRA )
{
  winapi_ret_int(GetTextCharacterExtra(winapi_par_HDC(1)));
}

/*
WINGDIAPI UINT WINAPI GetTextAlign(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTALIGN )
{
  winapi_ret_UINT(GetTextAlign(winapi_par_HDC(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetTextColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCOLOR )
{
  winapi_ret_COLORREF(GetTextColor(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointA(HDC hdc,LPCSTR lpString,int c,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTPOINTA )
{
  winapi_ret_BOOL(GetTextExtentPointA(winapi_par_HDC(1), ( LPCSTR ) hb_parc(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointW(HDC hdc,LPCWSTR lpString,int c,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTPOINTW )
{
  winapi_ret_BOOL(GetTextExtentPointW(winapi_par_HDC(1), ( LPCWSTR ) hb_parc(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

HB_FUNC( WINAPI_GETTEXTEXTENTPOINT )
{
  void * str;
  winapi_ret_BOOL(GetTextExtentPoint(winapi_par_HDC(1), HB_PARSTR(2, &str, nullptr), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
  hb_strfree(str);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32A(HDC hdc,LPCSTR lpString,int c,LPSIZE psizl)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTPOINT32A )
{
  winapi_ret_BOOL(GetTextExtentPoint32A(winapi_par_HDC(1), ( LPCSTR ) hb_parc(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32W(HDC hdc,LPCWSTR lpString,int c,LPSIZE psizl)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTPOINT32W )
{
  winapi_ret_BOOL(GetTextExtentPoint32W(winapi_par_HDC(1), ( LPCWSTR ) hb_parc(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

HB_FUNC( WINAPI_GETTEXTEXTENTPOINT32 )
{
  void * str;
  winapi_ret_BOOL(GetTextExtentPoint32(winapi_par_HDC(1), HB_PARSTR(2, &str, nullptr), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
  hb_strfree(str);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointA(HDC hdc,LPCSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTEXPOINTA )
{
  INT lpnFit;
  INT lpnDx;
  winapi_ret_BOOL(GetTextExtentExPointA(winapi_par_HDC(1), ( LPCSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  winapi_stor_INT(lpnFit, 5);
  winapi_stor_INT(lpnDx, 6);
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointW(HDC hdc,LPCWSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/
HB_FUNC( WINAPI_GETTEXTEXTENTEXPOINTW )
{
  INT lpnFit;
  INT lpnDx;
  winapi_ret_BOOL(GetTextExtentExPointW(winapi_par_HDC(1), ( LPCWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  winapi_stor_INT(lpnFit, 5);
  winapi_stor_INT(lpnDx, 6);
}

HB_FUNC( WINAPI_GETTEXTEXTENTEXPOINT )
{
  void * str;
  INT lpnFit;
  INT lpnDx;
  winapi_ret_BOOL(GetTextExtentExPoint(winapi_par_HDC(1), HB_PARSTR(2, &str, nullptr), winapi_par_int(3), winapi_par_int(4), &lpnFit, &lpnDx, static_cast<LPSIZE>(winapi_get_ptr(7))));
  winapi_stor_INT(lpnFit, 5);
  winapi_stor_INT(lpnDx, 6);
  hb_strfree(str);
}

/*
WINGDIAPI int WINAPI GetTextCharset(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCHARSET )
{
  winapi_ret_int(GetTextCharset(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_GETFONTLANGUAGEINFO )
{
  winapi_ret_DWORD(GetFontLanguageInfo(winapi_par_HDC(1)));
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

/*
WINGDIAPI DWORD WINAPI GetGlyphIndicesW(HDC hdc,LPCWSTR lpstr,int c,LPWORD pgi,DWORD fl)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointI(HDC hdc,LPWORD pgiIn,int cgi,LPSIZE psize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointI (HDC hdc,LPWORD lpwszString,int cwchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthI(HDC hdc,UINT giFirst,UINT cgi,LPWORD pgi,LPINT piWidths)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharABCWidthsI(HDC hdc,UINT giFirst,UINT cgi,LPWORD pgi,LPABC pabc)
*/

/*
WINGDIAPI int WINAPI AddFontResourceExA(LPCSTR name,DWORD fl,PVOID res)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEEXA )
{
  winapi_ret_int(AddFontResourceExA(( LPCSTR ) hb_parc(1), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

/*
WINGDIAPI int WINAPI AddFontResourceExW(LPCWSTR name,DWORD fl,PVOID res)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEEXW )
{
  winapi_ret_int(AddFontResourceExW(( LPCWSTR ) hb_parc(1), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

HB_FUNC( WINAPI_ADDFONTRESOURCEEX )
{
  void * str;
  winapi_ret_int(AddFontResourceEx(HB_PARSTR(1, &str, nullptr), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
  hb_strfree(str);
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExA(LPCSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEEXA )
{
  winapi_ret_BOOL(RemoveFontResourceExA(( LPCSTR ) hb_parc(1), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExW(LPCWSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEEXW )
{
  winapi_ret_BOOL(RemoveFontResourceExW(( LPCWSTR ) hb_parc(1), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
}

HB_FUNC( WINAPI_REMOVEFONTRESOURCEEX )
{
  void * str;
  winapi_ret_BOOL(RemoveFontResourceEx(HB_PARSTR(1, &str, nullptr), winapi_par_DWORD(2), static_cast<PVOID>(hb_parptr(3))));
  hb_strfree(str);
}

/*
WINGDIAPI HANDLE WINAPI AddFontMemResourceEx(PVOID pFileView,DWORD cjSize,PVOID pvResrved,DWORD *pNumFonts)
*/

/*
WINGDIAPI WINBOOL WINAPI RemoveFontMemResourceEx(HANDLE h)
*/
HB_FUNC( WINAPI_REMOVEFONTMEMRESOURCEEX )
{
  winapi_ret_BOOL(RemoveFontMemResourceEx(winapi_par_HANDLE(1)));
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
HB_FUNC( WINAPI_GETVIEWPORTEXTEX )
{
  winapi_ret_BOOL(GetViewportExtEx(winapi_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetViewportOrgEx(HDC hdc,LPPOINT lppoint)
*/
HB_FUNC( WINAPI_GETVIEWPORTORGEX )
{
  winapi_ret_BOOL(GetViewportOrgEx(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetWindowExtEx(HDC hdc,LPSIZE lpsize)
*/
HB_FUNC( WINAPI_GETWINDOWEXTEX )
{
  winapi_ret_BOOL(GetWindowExtEx(winapi_par_HDC(1), static_cast<LPSIZE>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetWindowOrgEx(HDC hdc,LPPOINT lppoint)
*/
HB_FUNC( WINAPI_GETWINDOWORGEX )
{
  winapi_ret_BOOL(GetWindowOrgEx(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI int WINAPI IntersectClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_INTERSECTCLIPRECT )
{
  winapi_ret_int(IntersectClipRect(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI InvertRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_INVERTRGN )
{
  winapi_ret_BOOL(InvertRgn(winapi_par_HDC(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI WINBOOL WINAPI LineDDA(int xStart,int yStart,int xEnd,int yEnd,LINEDDAPROC lpProc,LPARAM data)
*/

/*
WINGDIAPI WINBOOL WINAPI LineTo(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_LINETO )
{
  winapi_ret_BOOL(LineTo(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI MaskBlt(HDC hdcDest,int xDest,int yDest,int width,int height,HDC hdcSrc,int xSrc,int ySrc,HBITMAP hbmMask,int xMask,int yMask,DWORD rop)
*/
HB_FUNC( WINAPI_MASKBLT )
{
  winapi_ret_BOOL(MaskBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HDC(6), winapi_par_int(7), winapi_par_int(8), winapi_par_HBITMAP(9), winapi_par_int(10), winapi_par_int(11), winapi_par_DWORD(12)));
}

/*
WINGDIAPI WINBOOL WINAPI PlgBlt(HDC hdcDest,CONST POINT *lpPoint,HDC hdcSrc,int xSrc,int ySrc,int width,int height,HBITMAP hbmMask,int xMask,int yMask)
*/

/*
WINGDIAPI int WINAPI OffsetClipRgn(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_OFFSETCLIPRGN )
{
  winapi_ret_int(OffsetClipRgn(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI int WINAPI OffsetRgn(HRGN hrgn,int x,int y)
*/
HB_FUNC( WINAPI_OFFSETRGN )
{
  winapi_ret_int(OffsetRgn(winapi_par_HRGN(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PatBlt(HDC hdc,int x,int y,int w,int h,DWORD rop)
*/
HB_FUNC( WINAPI_PATBLT )
{
  winapi_ret_BOOL(PatBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_DWORD(6)));
}

/*
WINGDIAPI WINBOOL WINAPI Pie(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WINAPI_PIE )
{
  winapi_ret_BOOL(Pie(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI PlayMetaFile(HDC hdc,HMETAFILE hmf)
*/
HB_FUNC( WINAPI_PLAYMETAFILE )
{
  winapi_ret_BOOL(PlayMetaFile(winapi_par_HDC(1), winapi_par_HMETAFILE(2)));
}

/*
WINGDIAPI WINBOOL WINAPI PaintRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_PAINTRGN )
{
  winapi_ret_BOOL(PaintRgn(winapi_par_HDC(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyPolygon(HDC hdc,CONST POINT *apt,CONST INT *asz,int csz)
*/

/*
WINGDIAPI WINBOOL WINAPI PtInRegion(HRGN hrgn,int x,int y)
*/
HB_FUNC( WINAPI_PTINREGION )
{
  winapi_ret_BOOL(PtInRegion(winapi_par_HRGN(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PtVisible(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_PTVISIBLE )
{
  winapi_ret_BOOL(PtVisible(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI RectInRegion(HRGN hrgn,CONST RECT *lprect)
*/
HB_FUNC( WINAPI_RECTINREGION )
{
  winapi_ret_BOOL(RectInRegion(winapi_par_HRGN(1), static_cast<CONST RECT *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI RectVisible(HDC hdc,CONST RECT *lprect)
*/
HB_FUNC( WINAPI_RECTVISIBLE )
{
  winapi_ret_BOOL(RectVisible(winapi_par_HDC(1), static_cast<CONST RECT *>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI Rectangle(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_RECTANGLE )
{
  winapi_ret_BOOL(Rectangle(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI RestoreDC(HDC hdc,int nSavedDC)
*/
HB_FUNC( WINAPI_RESTOREDC )
{
  winapi_ret_BOOL(RestoreDC(winapi_par_HDC(1), winapi_par_int(2)));
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
HB_FUNC( WINAPI_REALIZEPALETTE )
{
  winapi_ret_UINT(RealizePalette(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceA(LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEA )
{
  winapi_ret_BOOL(RemoveFontResourceA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceW(LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEW )
{
  winapi_ret_BOOL(RemoveFontResourceW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_REMOVEFONTRESOURCE )
{
  void * str;
  winapi_ret_BOOL(RemoveFontResource(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI WINBOOL WINAPI RoundRect(HDC hdc,int left,int top,int right,int bottom,int width,int height)
*/
HB_FUNC( WINAPI_ROUNDRECT )
{
  winapi_ret_BOOL(RoundRect(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7)));
}

/*
WINGDIAPI WINBOOL WINAPI ResizePalette(HPALETTE hpal,UINT n)
*/
HB_FUNC( WINAPI_RESIZEPALETTE )
{
  winapi_ret_BOOL(ResizePalette(winapi_par_HPALETTE(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI int WINAPI SaveDC(HDC hdc)
*/
HB_FUNC( WINAPI_SAVEDC )
{
  winapi_ret_int(SaveDC(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI SelectClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_SELECTCLIPRGN )
{
  winapi_ret_int(SelectClipRgn(winapi_par_HDC(1), winapi_par_HRGN(2)));
}

/*
WINGDIAPI int WINAPI ExtSelectClipRgn(HDC hdc,HRGN hrgn,int mode)
*/
HB_FUNC( WINAPI_EXTSELECTCLIPRGN )
{
  winapi_ret_int(ExtSelectClipRgn(winapi_par_HDC(1), winapi_par_HRGN(2), winapi_par_int(3)));
}

/*
WINGDIAPI int WINAPI SetMetaRgn(HDC hdc)
*/
HB_FUNC( WINAPI_SETMETARGN )
{
  winapi_ret_int(SetMetaRgn(winapi_par_HDC(1)));
}

/*
WINGDIAPI HGDIOBJ WINAPI SelectObject(HDC hdc,HGDIOBJ h)
*/
HB_FUNC( WINAPI_SELECTOBJECT )
{
  winapi_ret_HGDIOBJ(SelectObject(winapi_par_HDC(1), winapi_par_HGDIOBJ(2)));
}

/*
WINGDIAPI HPALETTE WINAPI SelectPalette(HDC hdc,HPALETTE hPal,WINBOOL bForceBkgd)
*/
HB_FUNC( WINAPI_SELECTPALETTE )
{
  winapi_ret_HPALETTE(SelectPalette(winapi_par_HDC(1), winapi_par_HPALETTE(2), winapi_par_BOOL(3)));
}

/*
WINGDIAPI COLORREF WINAPI SetBkColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETBKCOLOR )
{
  winapi_ret_COLORREF(SetBkColor(winapi_par_HDC(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetDCBrushColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETDCBRUSHCOLOR )
{
  winapi_ret_COLORREF(SetDCBrushColor(winapi_par_HDC(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetDCPenColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETDCPENCOLOR )
{
  winapi_ret_COLORREF(SetDCPenColor(winapi_par_HDC(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI int WINAPI SetBkMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETBKMODE )
{
  winapi_ret_int(SetBkMode(winapi_par_HDC(1), winapi_par_int(2)));
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
HB_FUNC( WINAPI_SETMAPPERFLAGS )
{
  winapi_ret_DWORD(SetMapperFlags(winapi_par_HDC(1), winapi_par_DWORD(2)));
}

/*
WINGDIAPI int WINAPI SetGraphicsMode(HDC hdc,int iMode)
*/
HB_FUNC( WINAPI_SETGRAPHICSMODE )
{
  winapi_ret_int(SetGraphicsMode(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetMapMode(HDC hdc,int iMode)
*/
HB_FUNC( WINAPI_SETMAPMODE )
{
  winapi_ret_int(SetMapMode(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI DWORD WINAPI SetLayout(HDC hdc,DWORD l)
*/
HB_FUNC( WINAPI_SETLAYOUT )
{
  winapi_ret_DWORD(SetLayout(winapi_par_HDC(1), winapi_par_DWORD(2)));
}

/*
WINGDIAPI DWORD WINAPI GetLayout(HDC hdc)
*/
HB_FUNC( WINAPI_GETLAYOUT )
{
  winapi_ret_DWORD(GetLayout(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_SETPIXEL )
{
  winapi_ret_COLORREF(SetPixel(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelV(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WINAPI_SETPIXELV )
{
  winapi_ret_BOOL(SetPixelV(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_COLORREF(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelFormat(HDC hdc,int format,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/
HB_FUNC( WINAPI_SETPIXELFORMAT )
{
  winapi_ret_BOOL(SetPixelFormat(winapi_par_HDC(1), winapi_par_int(2), static_cast<CONST PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(3))));
}

/*
WINGDIAPI int WINAPI SetPolyFillMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETPOLYFILLMODE )
{
  winapi_ret_int(SetPolyFillMode(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI StretchBlt(HDC hdcDest,int xDest,int yDest,int wDest,int hDest,HDC hdcSrc,int xSrc,int ySrc,int wSrc,int hSrc,DWORD rop)
*/
HB_FUNC( WINAPI_STRETCHBLT )
{
  winapi_ret_BOOL(StretchBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HDC(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9), winapi_par_int(10), winapi_par_DWORD(11)));
}

/*
WINGDIAPI WINBOOL WINAPI SetRectRgn(HRGN hrgn,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_SETRECTRGN )
{
  winapi_ret_BOOL(SetRectRgn(winapi_par_HRGN(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINGDIAPI int WINAPI StretchDIBits(HDC hdc,int xDest,int yDest,int DestWidth,int DestHeight,int xSrc,int ySrc,int SrcWidth,int SrcHeight,CONST VOID *lpBits,CONST BITMAPINFO *lpbmi,UINT iUsage,DWORD rop)
*/

/*
WINGDIAPI int WINAPI SetROP2(HDC hdc,int rop2)
*/
HB_FUNC( WINAPI_SETROP2 )
{
  winapi_ret_int(SetROP2(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetStretchBltMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETSTRETCHBLTMODE )
{
  winapi_ret_int(SetStretchBltMode(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI UINT WINAPI SetSystemPaletteUse(HDC hdc,UINT use)
*/
HB_FUNC( WINAPI_SETSYSTEMPALETTEUSE )
{
  winapi_ret_UINT(SetSystemPaletteUse(winapi_par_HDC(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI int WINAPI SetTextCharacterExtra(HDC hdc,int extra)
*/
HB_FUNC( WINAPI_SETTEXTCHARACTEREXTRA )
{
  winapi_ret_int(SetTextCharacterExtra(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI COLORREF WINAPI SetTextColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETTEXTCOLOR )
{
  winapi_ret_COLORREF(SetTextColor(winapi_par_HDC(1), winapi_par_COLORREF(2)));
}

/*
WINGDIAPI UINT WINAPI SetTextAlign(HDC hdc,UINT align)
*/
HB_FUNC( WINAPI_SETTEXTALIGN )
{
  winapi_ret_UINT(SetTextAlign(winapi_par_HDC(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI WINBOOL WINAPI SetTextJustification(HDC hdc,int extra,int count)
*/
HB_FUNC( WINAPI_SETTEXTJUSTIFICATION )
{
  winapi_ret_BOOL(SetTextJustification(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateColors(HDC hdc)
*/
HB_FUNC( WINAPI_UPDATECOLORS )
{
  winapi_ret_BOOL(UpdateColors(winapi_par_HDC(1)));
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

/*
WINGDIAPI WINBOOL WINAPI GdiAlphaBlend(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,BLENDFUNCTION ftn)
*/

/*
WINGDIAPI WINBOOL WINAPI TransparentBlt(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,UINT crTransparent)
*/
HB_FUNC( WINAPI_TRANSPARENTBLT )
{
  winapi_ret_BOOL(TransparentBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HDC(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9), winapi_par_int(10), winapi_par_UINT(11)));
}

/*
WINGDIAPI WINBOOL WINAPI GdiTransparentBlt(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,UINT crTransparent)
*/
HB_FUNC( WINAPI_GDITRANSPARENTBLT )
{
  winapi_ret_BOOL(GdiTransparentBlt(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HDC(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9), winapi_par_int(10), winapi_par_UINT(11)));
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
HB_FUNC( WINAPI_CLOSEENHMETAFILE )
{
  winapi_ret_HENHMETAFILE(CloseEnhMetaFile(winapi_par_HDC(1)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileA(HENHMETAFILE hEnh,LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_COPYENHMETAFILEA )
{
  winapi_ret_HENHMETAFILE(CopyEnhMetaFileA(winapi_par_HENHMETAFILE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileW(HENHMETAFILE hEnh,LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_COPYENHMETAFILEW )
{
  winapi_ret_HENHMETAFILE(CopyEnhMetaFileW(winapi_par_HENHMETAFILE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_COPYENHMETAFILE )
{
  void * str;
  winapi_ret_HENHMETAFILE(CopyEnhMetaFile(winapi_par_HENHMETAFILE(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileA(HDC hdc,LPCSTR lpFilename,CONST RECT *lprc,LPCSTR lpDesc)
*/
HB_FUNC( WINAPI_CREATEENHMETAFILEA )
{
  winapi_ret_HDC(CreateEnhMetaFileA(winapi_par_HDC(1), ( LPCSTR ) hb_parc(2), static_cast<CONST RECT *>(winapi_get_ptr(3)), ( LPCSTR ) hb_parc(4)));
}

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileW(HDC hdc,LPCWSTR lpFilename,CONST RECT *lprc,LPCWSTR lpDesc)
*/
HB_FUNC( WINAPI_CREATEENHMETAFILEW )
{
  winapi_ret_HDC(CreateEnhMetaFileW(winapi_par_HDC(1), ( LPCWSTR ) hb_parc(2), static_cast<CONST RECT *>(winapi_get_ptr(3)), ( LPCWSTR ) hb_parc(4)));
}

HB_FUNC( WINAPI_CREATEENHMETAFILE )
{
  void * str2;
  void * str4;
  winapi_ret_HDC(CreateEnhMetaFile(winapi_par_HDC(1), HB_PARSTR(2, &str2, nullptr), static_cast<CONST RECT *>(winapi_get_ptr(3)), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str4);
}

/*
WINGDIAPI WINBOOL WINAPI DeleteEnhMetaFile(HENHMETAFILE hmf)
*/
HB_FUNC( WINAPI_DELETEENHMETAFILE )
{
  winapi_ret_BOOL(DeleteEnhMetaFile(winapi_par_HENHMETAFILE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI EnumEnhMetaFile(HDC hdc,HENHMETAFILE hmf,ENHMFENUMPROC lpProc,LPVOID lpParam,CONST RECT *lpRect)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WINAPI_GETENHMETAFILEA )
{
  winapi_ret_HENHMETAFILE(GetEnhMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WINAPI_GETENHMETAFILEW )
{
  winapi_ret_HENHMETAFILE(GetEnhMetaFileW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_GETENHMETAFILE )
{
  void * str;
  winapi_ret_HENHMETAFILE(GetEnhMetaFile(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileBits(HENHMETAFILE hEMF,UINT nSize,LPBYTE lpData)
*/

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionA(HENHMETAFILE hemf,UINT cchBuffer,LPSTR lpDescription)
*/
HB_FUNC( WINAPI_GETENHMETAFILEDESCRIPTIONA )
{
  winapi_ret_UINT(GetEnhMetaFileDescriptionA(winapi_par_HENHMETAFILE(1), winapi_par_UINT(2), ( LPSTR ) hb_parc(3)));
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionW(HENHMETAFILE hemf,UINT cchBuffer,LPWSTR lpDescription)
*/
HB_FUNC( WINAPI_GETENHMETAFILEDESCRIPTIONW )
{
  winapi_ret_UINT(GetEnhMetaFileDescriptionW(winapi_par_HENHMETAFILE(1), winapi_par_UINT(2), ( LPWSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_GETENHMETAFILEPIXELFORMAT )
{
  winapi_ret_UINT(GetEnhMetaFilePixelFormat(winapi_par_HENHMETAFILE(1), winapi_par_UINT(2), static_cast<PIXELFORMATDESCRIPTOR *>(winapi_get_ptr(3))));
}

/*
WINGDIAPI UINT WINAPI GetWinMetaFileBits(HENHMETAFILE hemf,UINT cbData16,LPBYTE pData16,INT iMapMode,HDC hdcRef)
*/

/*
WINGDIAPI WINBOOL WINAPI PlayEnhMetaFile(HDC hdc,HENHMETAFILE hmf,CONST RECT *lprect)
*/
HB_FUNC( WINAPI_PLAYENHMETAFILE )
{
  winapi_ret_BOOL(PlayEnhMetaFile(winapi_par_HDC(1), winapi_par_HENHMETAFILE(2), static_cast<CONST RECT *>(winapi_get_ptr(3))));
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
HB_FUNC( WINAPI_ANGLEARC )
{
  winapi_ret_BOOL(AngleArc(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_DWORD(4), winapi_par_FLOAT(5), winapi_par_FLOAT(6)));
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

/*
WINGDIAPI WINBOOL WINAPI GetColorAdjustment(HDC hdc,LPCOLORADJUSTMENT lpca)
*/

/*
WINGDIAPI HPALETTE WINAPI CreateHalftonePalette(HDC hdc)
*/
HB_FUNC( WINAPI_CREATEHALFTONEPALETTE )
{
  winapi_ret_HPALETTE(CreateHalftonePalette(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_ENDDOC )
{
  winapi_ret_int(EndDoc(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI StartPage(HDC hdc)
*/
HB_FUNC( WINAPI_STARTPAGE )
{
  winapi_ret_int(StartPage(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI EndPage(HDC hdc)
*/
HB_FUNC( WINAPI_ENDPAGE )
{
  winapi_ret_int(EndPage(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI AbortDoc(HDC hdc)
*/
HB_FUNC( WINAPI_ABORTDOC )
{
  winapi_ret_int(AbortDoc(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI SetAbortProc(HDC hdc,ABORTPROC lpProc)
*/

/*
WINGDIAPI WINBOOL WINAPI AbortPath(HDC hdc)
*/
HB_FUNC( WINAPI_ABORTPATH )
{
  winapi_ret_BOOL(AbortPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI ArcTo(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WINAPI_ARCTO )
{
  winapi_ret_BOOL(ArcTo(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_int(9)));
}

/*
WINGDIAPI WINBOOL WINAPI BeginPath(HDC hdc)
*/
HB_FUNC( WINAPI_BEGINPATH )
{
  winapi_ret_BOOL(BeginPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI CloseFigure(HDC hdc)
*/
HB_FUNC( WINAPI_CLOSEFIGURE )
{
  winapi_ret_BOOL(CloseFigure(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI EndPath(HDC hdc)
*/
HB_FUNC( WINAPI_ENDPATH )
{
  winapi_ret_BOOL(EndPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI FillPath(HDC hdc)
*/
HB_FUNC( WINAPI_FILLPATH )
{
  winapi_ret_BOOL(FillPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI FlattenPath(HDC hdc)
*/
HB_FUNC( WINAPI_FLATTENPATH )
{
  winapi_ret_BOOL(FlattenPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetPath(HDC hdc,LPPOINT apt,LPBYTE aj,int cpt)
*/

/*
WINGDIAPI HRGN WINAPI PathToRegion(HDC hdc)
*/
HB_FUNC( WINAPI_PATHTOREGION )
{
  winapi_ret_HRGN(PathToRegion(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyDraw(HDC hdc,CONST POINT *apt,CONST BYTE *aj,int cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI SelectClipPath(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SELECTCLIPPATH )
{
  winapi_ret_BOOL(SelectClipPath(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI int WINAPI SetArcDirection(HDC hdc,int dir)
*/
HB_FUNC( WINAPI_SETARCDIRECTION )
{
  winapi_ret_int(SetArcDirection(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI SetMiterLimit(HDC hdc,FLOAT limit,PFLOAT old)
*/
HB_FUNC( WINAPI_SETMITERLIMIT )
{
  FLOAT old;
  winapi_ret_BOOL(SetMiterLimit(winapi_par_HDC(1), static_cast<FLOAT>(hb_parnd(2)), &old));
  winapi_stor_FLOAT(old, 3);
}

/*
WINGDIAPI WINBOOL WINAPI StrokeAndFillPath(HDC hdc)
*/
HB_FUNC( WINAPI_STROKEANDFILLPATH )
{
  winapi_ret_BOOL(StrokeAndFillPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI StrokePath(HDC hdc)
*/
HB_FUNC( WINAPI_STROKEPATH )
{
  winapi_ret_BOOL(StrokePath(winapi_par_HDC(1)));
}

/*
WINGDIAPI WINBOOL WINAPI WidenPath(HDC hdc)
*/
HB_FUNC( WINAPI_WIDENPATH )
{
  winapi_ret_BOOL(WidenPath(winapi_par_HDC(1)));
}

/*
WINGDIAPI HPEN WINAPI ExtCreatePen(DWORD iPenStyle,DWORD cWidth,CONST LOGBRUSH *plbrush,DWORD cStyle,CONST DWORD *pstyle)
*/

/*
WINGDIAPI WINBOOL WINAPI GetMiterLimit(HDC hdc,PFLOAT plimit)
*/
HB_FUNC( WINAPI_GETMITERLIMIT )
{
  FLOAT limit;
  winapi_ret_BOOL(GetMiterLimit(winapi_par_HDC(1), &limit));
  winapi_stor_FLOAT(limit, 2);
}

/*
WINGDIAPI int WINAPI GetArcDirection(HDC hdc)
*/
HB_FUNC( WINAPI_GETARCDIRECTION )
{
  winapi_ret_int(GetArcDirection(winapi_par_HDC(1)));
}

/*
WINGDIAPI int WINAPI GetObjectA(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WINAPI_GETOBJECTA )
{
  winapi_ret_int(GetObjectA(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI int WINAPI GetObjectW(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WINAPI_GETOBJECTW )
{
  winapi_ret_int(GetObjectW(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINGDIAPI WINBOOL WINAPI MoveToEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_MOVETOEX )
{
  winapi_ret_BOOL(MoveToEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI TextOutA(HDC hdc,int x,int y,LPCSTR lpString,int c)
*/
HB_FUNC( WINAPI_TEXTOUTA )
{
  winapi_ret_BOOL(TextOutA(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), ( LPCSTR ) hb_parc(4), winapi_par_int(5)));
}

/*
WINGDIAPI WINBOOL WINAPI TextOutW(HDC hdc,int x,int y,LPCWSTR lpString,int c)
*/
HB_FUNC( WINAPI_TEXTOUTW )
{
  winapi_ret_BOOL(TextOutW(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), ( LPCWSTR ) hb_parc(4), winapi_par_int(5)));
}

HB_FUNC( WINAPI_TEXTOUT )
{
  void * str;
  winapi_ret_BOOL(TextOut(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), HB_PARSTR(4, &str, nullptr), winapi_par_int(5)));
  hb_strfree(str);
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
HB_FUNC( WINAPI_CREATEPOLYGONRGN )
{
  winapi_ret_HRGN(CreatePolygonRgn(static_cast<CONST POINT *>(winapi_get_ptr(1)), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI DPtoLP(HDC hdc,LPPOINT lppt,int c)
*/
HB_FUNC( WINAPI_DPTOLP )
{
  winapi_ret_BOOL(DPtoLP(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2)), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI LPtoDP(HDC hdc,LPPOINT lppt,int c)
*/
HB_FUNC( WINAPI_LPTODP )
{
  winapi_ret_BOOL(LPtoDP(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2)), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI Polygon(HDC hdc,CONST POINT *apt,int cpt)
*/
HB_FUNC( WINAPI_POLYGON )
{
  winapi_ret_BOOL(Polygon(winapi_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI Polyline(HDC hdc,CONST POINT *apt,int cpt)
*/
HB_FUNC( WINAPI_POLYLINE )
{
  winapi_ret_BOOL(Polyline(winapi_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), winapi_par_int(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyBezier(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WINAPI_POLYBEZIER )
{
  winapi_ret_BOOL(PolyBezier(winapi_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), winapi_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolyBezierTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WINAPI_POLYBEZIERTO )
{
  winapi_ret_BOOL(PolyBezierTo(winapi_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), winapi_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI PolylineTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/
HB_FUNC( WINAPI_POLYLINETO )
{
  winapi_ret_BOOL(PolylineTo(winapi_par_HDC(1), static_cast<CONST POINT *>(winapi_get_ptr(2)), winapi_par_DWORD(3)));
}

/*
WINGDIAPI WINBOOL WINAPI SetViewportExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_SETVIEWPORTEXTEX )
{
  winapi_ret_BOOL(SetViewportExtEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_SETVIEWPORTORGEX )
{
  winapi_ret_BOOL(SetViewportOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetWindowExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_SETWINDOWEXTEX )
{
  winapi_ret_BOOL(SetWindowExtEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_SETWINDOWORGEX )
{
  winapi_ret_BOOL(SetWindowOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI OffsetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_OFFSETVIEWPORTORGEX )
{
  winapi_ret_BOOL(OffsetViewportOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI OffsetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_OFFSETWINDOWORGEX )
{
  winapi_ret_BOOL(OffsetWindowOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI ScaleViewportExtEx(HDC hdc,int xn,int dx,int yn,int yd,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_SCALEVIEWPORTEXTEX )
{
  winapi_ret_BOOL(ScaleViewportExtEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), static_cast<LPSIZE>(winapi_get_ptr(6))));
}

/*
WINGDIAPI WINBOOL WINAPI ScaleWindowExtEx(HDC hdc,int xn,int xd,int yn,int yd,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_SCALEWINDOWEXTEX )
{
  winapi_ret_BOOL(ScaleWindowExtEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), static_cast<LPSIZE>(winapi_get_ptr(6))));
}

/*
WINGDIAPI WINBOOL WINAPI SetBitmapDimensionEx(HBITMAP hbm,int w,int h,LPSIZE lpsz)
*/
HB_FUNC( WINAPI_SETBITMAPDIMENSIONEX )
{
  winapi_ret_BOOL(SetBitmapDimensionEx(winapi_par_HBITMAP(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPSIZE>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI SetBrushOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/
HB_FUNC( WINAPI_SETBRUSHORGEX )
{
  winapi_ret_BOOL(SetBrushOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI int WINAPI GetTextFaceA(HDC hdc,int c,LPSTR lpName)
*/
HB_FUNC( WINAPI_GETTEXTFACEA )
{
  winapi_ret_int(GetTextFaceA(winapi_par_HDC(1), winapi_par_int(2), ( LPSTR ) hb_parc(3)));
}

/*
WINGDIAPI int WINAPI GetTextFaceW(HDC hdc,int c,LPWSTR lpName)
*/
HB_FUNC( WINAPI_GETTEXTFACEW )
{
  winapi_ret_int(GetTextFaceW(winapi_par_HDC(1), winapi_par_int(2), ( LPWSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_GETDCORGEX )
{
  winapi_ret_BOOL(GetDCOrgEx(winapi_par_HDC(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI FixBrushOrgEx(HDC hdc,int x,int y,LPPOINT ptl)
*/
HB_FUNC( WINAPI_FIXBRUSHORGEX )
{
  winapi_ret_BOOL(FixBrushOrgEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), static_cast<LPPOINT>(winapi_get_ptr(4))));
}

/*
WINGDIAPI WINBOOL WINAPI UnrealizeObject(HGDIOBJ h)
*/
HB_FUNC( WINAPI_UNREALIZEOBJECT )
{
  winapi_ret_BOOL(UnrealizeObject(winapi_par_HGDIOBJ(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GdiFlush(void)
*/
HB_FUNC( WINAPI_GDIFLUSH )
{
  winapi_ret_BOOL(GdiFlush());
}

/*
WINGDIAPI DWORD WINAPI GdiSetBatchLimit(DWORD dw)
*/
HB_FUNC( WINAPI_GDISETBATCHLIMIT )
{
  winapi_ret_DWORD(GdiSetBatchLimit(winapi_par_DWORD(1)));
}

/*
WINGDIAPI DWORD WINAPI GdiGetBatchLimit(void)
*/
HB_FUNC( WINAPI_GDIGETBATCHLIMIT )
{
  winapi_ret_DWORD(GdiGetBatchLimit());
}

/*
WINGDIAPI int WINAPI SetICMMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETICMMODE )
{
  winapi_ret_int(SetICMMode(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI CheckColorsInGamut(HDC hdc,LPVOID lpRGBTriple,LPVOID dlpBuffer,DWORD nCount)
*/

/*
WINGDIAPI HCOLORSPACE WINAPI GetColorSpace(HDC hdc)
*/
HB_FUNC( WINAPI_GETCOLORSPACE )
{
  winapi_ret_HCOLORSPACE(GetColorSpace(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_SETCOLORSPACE )
{
  winapi_ret_HCOLORSPACE(SetColorSpace(winapi_par_HDC(1), winapi_par_HCOLORSPACE(2)));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteColorSpace(HCOLORSPACE hcs)
*/
HB_FUNC( WINAPI_DELETECOLORSPACE )
{
  winapi_ret_BOOL(DeleteColorSpace(winapi_par_HCOLORSPACE(1)));
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileA(HDC hdc,LPDWORD pBufSize,LPSTR pszFilename)
*/
HB_FUNC( WINAPI_GETICMPROFILEA )
{
  DWORD BufSize;
  winapi_ret_BOOL(GetICMProfileA(winapi_par_HDC(1), &BufSize, ( LPSTR ) hb_parc(3)));
  winapi_stor_DWORD(BufSize, 2);
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileW(HDC hdc,LPDWORD pBufSize,LPWSTR pszFilename)
*/
HB_FUNC( WINAPI_GETICMPROFILEW )
{
  DWORD BufSize;
  winapi_ret_BOOL(GetICMProfileW(winapi_par_HDC(1), &BufSize, ( LPWSTR ) hb_parc(3)));
  winapi_stor_DWORD(BufSize, 2);
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileA(HDC hdc,LPSTR lpFileName)
*/
HB_FUNC( WINAPI_SETICMPROFILEA )
{
  winapi_ret_BOOL(SetICMProfileA(winapi_par_HDC(1), ( LPSTR ) hb_parc(2)));
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileW(HDC hdc,LPWSTR lpFileName)
*/
HB_FUNC( WINAPI_SETICMPROFILEW )
{
  winapi_ret_BOOL(SetICMProfileW(winapi_par_HDC(1), ( LPWSTR ) hb_parc(2)));
}

/*
WINGDIAPI WINBOOL WINAPI GetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WINAPI_GETDEVICEGAMMARAMP )
{
  winapi_ret_BOOL(GetDeviceGammaRamp(winapi_par_HDC(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI SetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WINAPI_SETDEVICEGAMMARAMP )
{
  winapi_ret_BOOL(SetDeviceGammaRamp(winapi_par_HDC(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI ColorMatchToTarget(HDC hdc,HDC hdcTarget,DWORD action)
*/
HB_FUNC( WINAPI_COLORMATCHTOTARGET )
{
  winapi_ret_BOOL(ColorMatchToTarget(winapi_par_HDC(1), winapi_par_HDC(2), winapi_par_DWORD(3)));
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
HB_FUNC( WINAPI_UPDATEICMREGKEYA )
{
  winapi_ret_BOOL(UpdateICMRegKeyA(winapi_par_DWORD(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), winapi_par_UINT(4)));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateICMRegKeyW(DWORD reserved,LPWSTR lpszCMID,LPWSTR lpszFileName,UINT command)
*/
HB_FUNC( WINAPI_UPDATEICMREGKEYW )
{
  winapi_ret_BOOL(UpdateICMRegKeyW(winapi_par_DWORD(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), winapi_par_UINT(4)));
}

/*
WINGDIAPI WINBOOL WINAPI ColorCorrectPalette(HDC hdc,HPALETTE hPal,DWORD deFirst,DWORD num)
*/
HB_FUNC( WINAPI_COLORCORRECTPALETTE )
{
  winapi_ret_BOOL(ColorCorrectPalette(winapi_par_HDC(1), winapi_par_HPALETTE(2), winapi_par_DWORD(3), winapi_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI wglCopyContext(HGLRC,HGLRC,UINT)
*/
HB_FUNC( WINAPI_WGLCOPYCONTEXT )
{
  winapi_ret_BOOL(wglCopyContext(winapi_par_HGLRC(1), winapi_par_HGLRC(2), winapi_par_UINT(3)));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateContext(HDC)
*/
HB_FUNC( WINAPI_WGLCREATECONTEXT )
{
  winapi_ret_HGLRC(wglCreateContext(winapi_par_HDC(1)));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateLayerContext(HDC,int)
*/
HB_FUNC( WINAPI_WGLCREATELAYERCONTEXT )
{
  winapi_ret_HGLRC(wglCreateLayerContext(winapi_par_HDC(1), winapi_par_int(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglDeleteContext(HGLRC)
*/
HB_FUNC( WINAPI_WGLDELETECONTEXT )
{
  winapi_ret_BOOL(wglDeleteContext(winapi_par_HGLRC(1)));
}

/*
WINGDIAPI HGLRC WINAPI wglGetCurrentContext(VOID)
*/
HB_FUNC( WINAPI_WGLGETCURRENTCONTEXT )
{
  winapi_ret_HGLRC(wglGetCurrentContext());
}

/*
WINGDIAPI HDC WINAPI wglGetCurrentDC(VOID)
*/
HB_FUNC( WINAPI_WGLGETCURRENTDC )
{
  winapi_ret_HDC(wglGetCurrentDC());
}

/*
WINGDIAPI PROC WINAPI wglGetProcAddress(LPCSTR)
*/

/*
WINGDIAPI WINBOOL WINAPI wglMakeCurrent(HDC,HGLRC)
*/
HB_FUNC( WINAPI_WGLMAKECURRENT )
{
  winapi_ret_BOOL(wglMakeCurrent(winapi_par_HDC(1), winapi_par_HGLRC(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglShareLists(HGLRC,HGLRC)
*/
HB_FUNC( WINAPI_WGLSHARELISTS )
{
  winapi_ret_BOOL(wglShareLists(winapi_par_HGLRC(1), winapi_par_HGLRC(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsA(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WINAPI_WGLUSEFONTBITMAPSA )
{
  winapi_ret_BOOL(wglUseFontBitmapsA(winapi_par_HDC(1), winapi_par_DWORD(2), winapi_par_DWORD(3), winapi_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsW(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WINAPI_WGLUSEFONTBITMAPSW )
{
  winapi_ret_BOOL(wglUseFontBitmapsW(winapi_par_HDC(1), winapi_par_DWORD(2), winapi_par_DWORD(3), winapi_par_DWORD(4)));
}

/*
WINGDIAPI WINBOOL WINAPI SwapBuffers(HDC)
*/
HB_FUNC( WINAPI_SWAPBUFFERS )
{
  winapi_ret_BOOL(SwapBuffers(winapi_par_HDC(1)));
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
HB_FUNC( WINAPI_WGLREALIZELAYERPALETTE )
{
  winapi_ret_BOOL(wglRealizeLayerPalette(winapi_par_HDC(1), winapi_par_int(2), winapi_par_BOOL(3)));
}

/*
WINGDIAPI WINBOOL WINAPI wglSwapLayerBuffers(HDC,UINT)
*/
HB_FUNC( WINAPI_WGLSWAPLAYERBUFFERS )
{
  winapi_ret_BOOL(wglSwapLayerBuffers(winapi_par_HDC(1), winapi_par_UINT(2)));
}

/*
WINGDIAPI DWORD WINAPI wglSwapMultipleBuffers(UINT,CONST WGLSWAP *)
*/
