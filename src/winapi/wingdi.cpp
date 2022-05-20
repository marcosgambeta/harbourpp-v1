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

/*
WINGDIAPI int WINAPI AddFontResourceA(LPCSTR)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEA )
{
  hb_retni(AddFontResourceA( ( LPCSTR ) hb_parc(1) ));
}

/*
WINGDIAPI int WINAPI AddFontResourceW(LPCWSTR)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEW )
{
  hb_retni(AddFontResourceW( ( LPCWSTR ) hb_parc(1) ));
}

/*
WINGDIAPI WINBOOL WINAPI AnimatePalette(HPALETTE hPal,UINT iStartIndex,UINT cEntries,CONST PALETTEENTRY *ppe)
*/

/*
WINGDIAPI WINBOOL WINAPI Arc(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WINAPI_ARC )
{
  hb_retl(( WINBOOL ) Arc( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9) ));
}

/*
WINGDIAPI WINBOOL WINAPI BitBlt(HDC hdc,int x,int y,int cx,int cy,HDC hdcSrc,int x1,int y1,DWORD rop)
*/
HB_FUNC( WINAPI_BITBLT )
{
  hb_retl(( WINBOOL ) BitBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), static_cast<HDC>(hb_parptr(6)), hb_parni(7), hb_parni(8), ( DWORD ) hb_parnl(9) ));
}

/*
WINGDIAPI WINBOOL WINAPI CancelDC(HDC hdc)
*/
HB_FUNC( WINAPI_CANCELDC )
{
  hb_retl(( WINBOOL ) CancelDC( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI Chord(HDC hdc,int x1,int y1,int x2,int y2,int x3,int y3,int x4,int y4)
*/
HB_FUNC( WINAPI_CHORD )
{
  hb_retl(( WINBOOL ) Chord( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9) ));
}

/*
WINGDIAPI int WINAPI ChoosePixelFormat(HDC hdc,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/

/*
WINGDIAPI HMETAFILE WINAPI CloseMetaFile(HDC hdc)
*/
HB_FUNC( WINAPI_CLOSEMETAFILE )
{
  hb_retptr(CloseMetaFile(static_cast<HDC>(hb_parptr(1))));
}

/*
WINGDIAPI int WINAPI CombineRgn(HRGN hrgnDst,HRGN hrgnSrc1,HRGN hrgnSrc2,int iMode)
*/
HB_FUNC( WINAPI_COMBINERGN )
{
  hb_retni(CombineRgn( static_cast<HRGN>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)), static_cast<HRGN>(hb_parptr(3)), hb_parni(4) ));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileA(HMETAFILE,LPCSTR)
*/
HB_FUNC( WINAPI_COPYMETAFILEA )
{
  hb_retptr(CopyMetaFileA(static_cast<HMETAFILE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINGDIAPI HMETAFILE WINAPI CopyMetaFileW(HMETAFILE,LPCWSTR)
*/
HB_FUNC( WINAPI_COPYMETAFILEW )
{
  hb_retptr(CopyMetaFileW(static_cast<HMETAFILE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINGDIAPI HBITMAP WINAPI CreateBitmap(int nWidth,int nHeight,UINT nPlanes,UINT nBitCount,CONST VOID *lpBits)
*/

/*
WINGDIAPI HBITMAP WINAPI CreateBitmapIndirect(CONST BITMAP *pbm)
*/

/*
WINGDIAPI HBRUSH WINAPI CreateBrushIndirect(CONST LOGBRUSH *plbrush)
*/

/*
WINGDIAPI HBITMAP WINAPI CreateCompatibleBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WINAPI_CREATECOMPATIBLEBITMAP )
{
  hb_retptr(CreateCompatibleBitmap(static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3)));
}

/*
WINGDIAPI HBITMAP WINAPI CreateDiscardableBitmap(HDC hdc,int cx,int cy)
*/
HB_FUNC( WINAPI_CREATEDISCARDABLEBITMAP )
{
  hb_retptr(CreateDiscardableBitmap(static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3)));
}

/*
WINGDIAPI HDC WINAPI CreateCompatibleDC(HDC hdc)
*/
HB_FUNC( WINAPI_CREATECOMPATIBLEDC )
{
  hb_retptr(CreateCompatibleDC(static_cast<HDC>(hb_parptr(1))));
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
  hb_retptr(CreateDIBPatternBrush(static_cast<HGLOBAL>(hb_parptr(1)), static_cast<UINT>(hb_parni(2))));
}

/*
WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrushPt(CONST VOID *lpPackedDIB,UINT iUsage)
*/

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WINAPI_CREATEELLIPTICRGN )
{
  hb_retptr(CreateEllipticRgn(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateEllipticRgnIndirect(CONST RECT *lprect)
*/

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
  hb_retptr(CreateFontA(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), ( DWORD ) hb_parnl(6), ( DWORD ) hb_parnl(7), ( DWORD ) hb_parnl(8), ( DWORD ) hb_parnl(9), ( DWORD ) hb_parnl(10), ( DWORD ) hb_parnl(11), ( DWORD ) hb_parnl(12), ( DWORD ) hb_parnl(13), ( LPCSTR ) hb_parc(14)));
}

/*
WINGDIAPI HFONT WINAPI CreateFontW(int cHeight,int cWidth,int cEscapement,int cOrientation,int cWeight,DWORD bItalic,DWORD bUnderline,DWORD bStrikeOut,DWORD iCharSet,DWORD iOutPrecision,DWORD iClipPrecision,DWORD iQuality,DWORD iPitchAndFamily,LPCWSTR pszFaceName)
*/
HB_FUNC( WINAPI_CREATEFONTW )
{
  hb_retptr(CreateFontW(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), ( DWORD ) hb_parnl(6), ( DWORD ) hb_parnl(7), ( DWORD ) hb_parnl(8), ( DWORD ) hb_parnl(9), ( DWORD ) hb_parnl(10), ( DWORD ) hb_parnl(11), ( DWORD ) hb_parnl(12), ( DWORD ) hb_parnl(13), ( LPCWSTR ) hb_parc(14)));
}

/*
WINGDIAPI HBRUSH WINAPI CreateHatchBrush(int iHatch,COLORREF color)
*/
HB_FUNC( WINAPI_CREATEHATCHBRUSH )
{
  hb_retptr(CreateHatchBrush(hb_parni(1), static_cast<COLORREF>(hb_parnl(2))));
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
  hb_retptr(CreateMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HDC WINAPI CreateMetaFileW(LPCWSTR pszFile)
*/
HB_FUNC( WINAPI_CREATEMETAFILEW )
{
  hb_retptr(CreateMetaFileW(( LPCWSTR ) hb_parc(1)));
}

/*
WINGDIAPI HPALETTE WINAPI CreatePalette(CONST LOGPALETTE *plpal)
*/

/*
WINGDIAPI HPEN WINAPI CreatePen(int iStyle,int cWidth,COLORREF color)
*/
HB_FUNC( WINAPI_CREATEPEN )
{
  hb_retptr(CreatePen(hb_parni(1), hb_parni(2), static_cast<COLORREF>(hb_parnl(3))));
}

/*
WINGDIAPI HPEN WINAPI CreatePenIndirect(CONST LOGPEN *plpen)
*/

/*
WINGDIAPI HRGN WINAPI CreatePolyPolygonRgn(CONST POINT *pptl,CONST INT *pc,int cPoly,int iMode)
*/

/*
WINGDIAPI HBRUSH WINAPI CreatePatternBrush(HBITMAP hbm)
*/
HB_FUNC( WINAPI_CREATEPATTERNBRUSH )
{
  hb_retptr(CreatePatternBrush(static_cast<HBITMAP>(hb_parptr(1))));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgn(int x1,int y1,int x2,int y2)
*/
HB_FUNC( WINAPI_CREATERECTRGN )
{
  hb_retptr(CreateRectRgn(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4)));
}

/*
WINGDIAPI HRGN WINAPI CreateRectRgnIndirect(CONST RECT *lprect)
*/

/*
WINGDIAPI HRGN WINAPI CreateRoundRectRgn(int x1,int y1,int x2,int y2,int w,int h)
*/
HB_FUNC( WINAPI_CREATEROUNDRECTRGN )
{
  hb_retptr(CreateRoundRectRgn(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6)));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceA(DWORD fdwHidden,LPCSTR lpszFont,LPCSTR lpszFile,LPCSTR lpszPath)
*/
HB_FUNC( WINAPI_CREATESCALABLEFONTRESOURCEA )
{
  hb_retl(( WINBOOL ) CreateScalableFontResourceA( ( DWORD ) hb_parnl(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4) ));
}

/*
WINGDIAPI WINBOOL WINAPI CreateScalableFontResourceW(DWORD fdwHidden,LPCWSTR lpszFont,LPCWSTR lpszFile,LPCWSTR lpszPath)
*/
HB_FUNC( WINAPI_CREATESCALABLEFONTRESOURCEW )
{
  hb_retl(( WINBOOL ) CreateScalableFontResourceW( ( DWORD ) hb_parnl(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4) ));
}

/*
WINGDIAPI HBRUSH WINAPI CreateSolidBrush(COLORREF color)
*/
HB_FUNC( WINAPI_CREATESOLIDBRUSH )
{
  hb_retptr(CreateSolidBrush(static_cast<COLORREF>(hb_parnl(1))));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteDC(HDC hdc)
*/
HB_FUNC( WINAPI_DELETEDC )
{
  hb_retl(( WINBOOL ) DeleteDC( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteMetaFile(HMETAFILE hmf)
*/
HB_FUNC( WINAPI_DELETEMETAFILE )
{
  hb_retl(( WINBOOL ) DeleteMetaFile( static_cast<HMETAFILE>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteObject(HGDIOBJ ho)
*/
HB_FUNC( WINAPI_DELETEOBJECT )
{
  hb_retl(( WINBOOL ) DeleteObject( static_cast<HGDIOBJ>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI DescribePixelFormat(HDC hdc,int iPixelFormat,UINT nBytes,LPPIXELFORMATDESCRIPTOR ppfd)
*/

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
  hb_retni(DrawEscape( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), ( LPCSTR ) hb_parc(4) ));
}

/*
WINGDIAPI WINBOOL WINAPI Ellipse(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_ELLIPSE )
{
  hb_retl(( WINBOOL ) Ellipse( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ));
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
  hb_retl(( WINBOOL ) EqualRgn( static_cast<HRGN>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI int WINAPI Escape(HDC hdc,int iEscape,int cjIn,LPCSTR pvIn,LPVOID pvOut)
*/
HB_FUNC( WINAPI_ESCAPE )
{
  hb_retni(Escape( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), ( LPCSTR ) hb_parc(4), static_cast<LPVOID>(hb_parptr(5)) ));
}

/*
WINGDIAPI int WINAPI ExtEscape(HDC hdc,int iEscape,int cjInput,LPCSTR lpInData,int cjOutput,LPSTR lpOutData)
*/
HB_FUNC( WINAPI_EXTESCAPE )
{
  hb_retni(ExtEscape( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), ( LPCSTR ) hb_parc(4), hb_parni(5), ( LPSTR ) hb_parc(6) ));
}

/*
WINGDIAPI int WINAPI ExcludeClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_EXCLUDECLIPRECT )
{
  hb_retni(ExcludeClipRect( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ));
}

/*
WINGDIAPI HRGN WINAPI ExtCreateRegion(CONST XFORM *lpx,DWORD nCount,CONST RGNDATA *lpData)
*/

/*
WINGDIAPI WINBOOL WINAPI ExtFloodFill(HDC hdc,int x,int y,COLORREF color,UINT type)
*/
HB_FUNC( WINAPI_EXTFLOODFILL )
{
  hb_retl(( WINBOOL ) ExtFloodFill( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), static_cast<COLORREF>(hb_parnl(4)), static_cast<UINT>(hb_parni(5)) ));
}

/*
WINGDIAPI WINBOOL WINAPI FillRgn(HDC hdc,HRGN hrgn,HBRUSH hbr)
*/
HB_FUNC( WINAPI_FILLRGN )
{
  hb_retl(( WINBOOL ) FillRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)), static_cast<HBRUSH>(hb_parptr(3)) ));
}

/*
WINGDIAPI WINBOOL WINAPI FloodFill(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WINAPI_FLOODFILL )
{
  hb_retl(( WINBOOL ) FloodFill( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), static_cast<COLORREF>(hb_parnl(4)) ));
}

/*
WINGDIAPI WINBOOL WINAPI FrameRgn(HDC hdc,HRGN hrgn,HBRUSH hbr,int w,int h)
*/
HB_FUNC( WINAPI_FRAMERGN )
{
  hb_retl(( WINBOOL ) FrameRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)), static_cast<HBRUSH>(hb_parptr(3)), hb_parni(4), hb_parni(5) ));
}

/*
WINGDIAPI int WINAPI GetROP2(HDC hdc)
*/
HB_FUNC( WINAPI_GETROP2 )
{
  hb_retni(GetROP2( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI GetAspectRatioFilterEx(HDC hdc,LPSIZE lpsize)
*/

/*
WINGDIAPI COLORREF WINAPI GetBkColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETBKCOLOR )
{
  hb_retnl( ( COLORREF ) GetBkColor( static_cast<HDC>(hb_parptr(1)) ) );
}

/*
WINGDIAPI COLORREF WINAPI GetDCBrushColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETDCBRUSHCOLOR )
{
  hb_retnl( ( COLORREF ) GetDCBrushColor( static_cast<HDC>(hb_parptr(1)) ) );
}

/*
WINGDIAPI COLORREF WINAPI GetDCPenColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETDCPENCOLOR )
{
  hb_retnl( ( COLORREF ) GetDCPenColor( static_cast<HDC>(hb_parptr(1)) ) );
}

/*
WINGDIAPI int WINAPI GetBkMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETBKMODE )
{
  hb_retni(GetBkMode( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI LONG WINAPI GetBitmapBits(HBITMAP hbit,LONG cb,LPVOID lpvBits)
*/
HB_FUNC( WINAPI_GETBITMAPBITS )
{
  hb_retnl( ( LONG ) GetBitmapBits( static_cast<HBITMAP>(hb_parptr(1)), ( LONG ) hb_parnl(2), static_cast<LPVOID>(hb_parptr(3)) ) );
}

/*
WINGDIAPI WINBOOL WINAPI GetBitmapDimensionEx(HBITMAP hbit,LPSIZE lpsize)
*/

/*
WINGDIAPI UINT WINAPI GetBoundsRect(HDC hdc,LPRECT lprect,UINT flags)
*/

/*
WINGDIAPI WINBOOL WINAPI GetBrushOrgEx(HDC hdc,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthA(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthW(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidth32A(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidth32W(HDC hdc,UINT iFirst,UINT iLast,LPINT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatA(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/

/*
WINGDIAPI WINBOOL WINAPI GetCharWidthFloatW(HDC hdc,UINT iFirst,UINT iLast,PFLOAT lpBuffer)
*/

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

/*
WINGDIAPI int WINAPI GetClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_GETCLIPRGN )
{
  hb_retni(GetClipRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI int WINAPI GetMetaRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_GETMETARGN )
{
  hb_retni(GetMetaRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI HGDIOBJ WINAPI GetCurrentObject(HDC hdc,UINT type)
*/
HB_FUNC( WINAPI_GETCURRENTOBJECT )
{
  hb_retptr(GetCurrentObject(static_cast<HDC>(hb_parptr(1)), static_cast<UINT>(hb_parni(2))));
}

/*
WINGDIAPI WINBOOL WINAPI GetCurrentPositionEx(HDC hdc,LPPOINT lppt)
*/

/*
WINGDIAPI int WINAPI GetDeviceCaps(HDC hdc,int index)
*/
HB_FUNC( WINAPI_GETDEVICECAPS )
{
  hb_retni(GetDeviceCaps( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI int WINAPI GetDIBits(HDC hdc,HBITMAP hbm,UINT start,UINT cLines,LPVOID lpvBits,LPBITMAPINFO lpbmi,UINT usage)
*/

/*
WINGDIAPI DWORD WINAPI GetFontData (HDC hdc,DWORD dwTable,DWORD dwOffset,PVOID pvBuffer,DWORD cjBuffer)
*/
HB_FUNC( WINAPI_GETFONTDATA )
{
  hb_retnl( ( DWORD ) GetFontData( static_cast<HDC>(hb_parptr(1)), ( DWORD ) hb_parnl(2), ( DWORD ) hb_parnl(3), static_cast<PVOID>(hb_parptr(4)), ( DWORD ) hb_parnl(5) ) );
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
  hb_retni(GetGraphicsMode( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI GetMapMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETMAPMODE )
{
  hb_retni(GetMapMode( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI UINT WINAPI GetMetaFileBitsEx(HMETAFILE hMF,UINT cbBuffer,LPVOID lpData)
*/
HB_FUNC( WINAPI_GETMETAFILEBITSEX )
{
  hb_retni(( UINT ) GetMetaFileBitsEx( static_cast<HMETAFILE>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)), static_cast<LPVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WINAPI_GETMETAFILEA )
{
  hb_retptr(GetMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HMETAFILE WINAPI GetMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WINAPI_GETMETAFILEW )
{
  hb_retptr(GetMetaFileW(( LPCWSTR ) hb_parc(1)));
}

/*
WINGDIAPI COLORREF WINAPI GetNearestColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_GETNEARESTCOLOR )
{
  hb_retnl( ( COLORREF ) GetNearestColor( static_cast<HDC>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ) );
}

/*
WINGDIAPI UINT WINAPI GetNearestPaletteIndex(HPALETTE h,COLORREF color)
*/
HB_FUNC( WINAPI_GETNEARESTPALETTEINDEX )
{
  hb_retni(( UINT ) GetNearestPaletteIndex( static_cast<HPALETTE>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ));
}

/*
WINGDIAPI DWORD WINAPI GetObjectType(HGDIOBJ h)
*/
HB_FUNC( WINAPI_GETOBJECTTYPE )
{
  hb_retnl( ( DWORD ) GetObjectType( static_cast<HGDIOBJ>(hb_parptr(1)) ) );
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
  hb_retnl( ( COLORREF ) GetPixel( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3) ) );
}

/*
WINGDIAPI int WINAPI GetPixelFormat(HDC hdc)
*/
HB_FUNC( WINAPI_GETPIXELFORMAT )
{
  hb_retni(GetPixelFormat( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI GetPolyFillMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETPOLYFILLMODE )
{
  hb_retni(GetPolyFillMode( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI GetRasterizerCaps(LPRASTERIZER_STATUS lpraststat,UINT cjBytes)
*/

/*
WINGDIAPI int WINAPI GetRandomRgn (HDC hdc,HRGN hrgn,INT i)
*/
HB_FUNC( WINAPI_GETRANDOMRGN )
{
  hb_retni(GetRandomRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)), hb_parni(3) ));
}

/*
WINGDIAPI DWORD WINAPI GetRegionData(HRGN hrgn,DWORD nCount,LPRGNDATA lpRgnData)
*/

/*
WINGDIAPI int WINAPI GetRgnBox(HRGN hrgn,LPRECT lprc)
*/

/*
WINGDIAPI HGDIOBJ WINAPI GetStockObject(int i)
*/
HB_FUNC( WINAPI_GETSTOCKOBJECT )
{
  hb_retptr(GetStockObject(hb_parni(1)));
}

/*
WINGDIAPI int WINAPI GetStretchBltMode(HDC hdc)
*/
HB_FUNC( WINAPI_GETSTRETCHBLTMODE )
{
  hb_retni(GetStretchBltMode( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI UINT WINAPI GetSystemPaletteEntries(HDC hdc,UINT iStart,UINT cEntries,LPPALETTEENTRY pPalEntries)
*/

/*
WINGDIAPI UINT WINAPI GetSystemPaletteUse(HDC hdc)
*/
HB_FUNC( WINAPI_GETSYSTEMPALETTEUSE )
{
  hb_retni(( UINT ) GetSystemPaletteUse( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI GetTextCharacterExtra(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCHARACTEREXTRA )
{
  hb_retni(GetTextCharacterExtra( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI UINT WINAPI GetTextAlign(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTALIGN )
{
  hb_retni(( UINT ) GetTextAlign( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI COLORREF WINAPI GetTextColor(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCOLOR )
{
  hb_retnl( ( COLORREF ) GetTextColor( static_cast<HDC>(hb_parptr(1)) ) );
}

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointA(HDC hdc,LPCSTR lpString,int c,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPointW(HDC hdc,LPCWSTR lpString,int c,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32A(HDC hdc,LPCSTR lpString,int c,LPSIZE psizl)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentPoint32W(HDC hdc,LPCWSTR lpString,int c,LPSIZE psizl)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointA(HDC hdc,LPCSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetTextExtentExPointW(HDC hdc,LPCWSTR lpszString,int cchString,int nMaxExtent,LPINT lpnFit,LPINT lpnDx,LPSIZE lpSize)
*/

/*
WINGDIAPI int WINAPI GetTextCharset(HDC hdc)
*/
HB_FUNC( WINAPI_GETTEXTCHARSET )
{
  hb_retni(GetTextCharset( static_cast<HDC>(hb_parptr(1)) ));
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
  hb_retnl( ( DWORD ) GetFontLanguageInfo( static_cast<HDC>(hb_parptr(1)) ) );
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
  hb_retni(AddFontResourceExA( ( LPCSTR ) hb_parc(1), ( DWORD ) hb_parnl(2), static_cast<PVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI int WINAPI AddFontResourceExW(LPCWSTR name,DWORD fl,PVOID res)
*/
HB_FUNC( WINAPI_ADDFONTRESOURCEEXW )
{
  hb_retni(AddFontResourceExW( ( LPCWSTR ) hb_parc(1), ( DWORD ) hb_parnl(2), static_cast<PVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExA(LPCSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEEXA )
{
  hb_retl(( WINBOOL ) RemoveFontResourceExA( ( LPCSTR ) hb_parc(1), ( DWORD ) hb_parnl(2), static_cast<PVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceExW(LPCWSTR name,DWORD fl,PVOID pdv)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEEXW )
{
  hb_retl(( WINBOOL ) RemoveFontResourceExW( ( LPCWSTR ) hb_parc(1), ( DWORD ) hb_parnl(2), static_cast<PVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI HANDLE WINAPI AddFontMemResourceEx(PVOID pFileView,DWORD cjSize,PVOID pvResrved,DWORD *pNumFonts)
*/

/*
WINGDIAPI WINBOOL WINAPI RemoveFontMemResourceEx(HANDLE h)
*/
HB_FUNC( WINAPI_REMOVEFONTMEMRESOURCEEX )
{
  hb_retl(( WINBOOL ) RemoveFontMemResourceEx( static_cast<HANDLE>(hb_parptr(1)) ));
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

/*
WINGDIAPI WINBOOL WINAPI GetViewportOrgEx(HDC hdc,LPPOINT lppoint)
*/

/*
WINGDIAPI WINBOOL WINAPI GetWindowExtEx(HDC hdc,LPSIZE lpsize)
*/

/*
WINGDIAPI WINBOOL WINAPI GetWindowOrgEx(HDC hdc,LPPOINT lppoint)
*/

/*
WINGDIAPI int WINAPI IntersectClipRect(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_INTERSECTCLIPRECT )
{
  hb_retni(IntersectClipRect( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ));
}

/*
WINGDIAPI WINBOOL WINAPI InvertRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_INVERTRGN )
{
  hb_retl(( WINBOOL ) InvertRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI LineDDA(int xStart,int yStart,int xEnd,int yEnd,LINEDDAPROC lpProc,LPARAM data)
*/

/*
WINGDIAPI WINBOOL WINAPI LineTo(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_LINETO )
{
  hb_retl(( WINBOOL ) LineTo( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI MaskBlt(HDC hdcDest,int xDest,int yDest,int width,int height,HDC hdcSrc,int xSrc,int ySrc,HBITMAP hbmMask,int xMask,int yMask,DWORD rop)
*/
HB_FUNC( WINAPI_MASKBLT )
{
  hb_retl(( WINBOOL ) MaskBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), static_cast<HDC>(hb_parptr(6)), hb_parni(7), hb_parni(8), static_cast<HBITMAP>(hb_parptr(9)), hb_parni(10), hb_parni(11), ( DWORD ) hb_parnl(12) ));
}

/*
WINGDIAPI WINBOOL WINAPI PlgBlt(HDC hdcDest,CONST POINT *lpPoint,HDC hdcSrc,int xSrc,int ySrc,int width,int height,HBITMAP hbmMask,int xMask,int yMask)
*/

/*
WINGDIAPI int WINAPI OffsetClipRgn(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_OFFSETCLIPRGN )
{
  hb_retni(OffsetClipRgn( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI int WINAPI OffsetRgn(HRGN hrgn,int x,int y)
*/
HB_FUNC( WINAPI_OFFSETRGN )
{
  hb_retni(OffsetRgn( static_cast<HRGN>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI PatBlt(HDC hdc,int x,int y,int w,int h,DWORD rop)
*/
HB_FUNC( WINAPI_PATBLT )
{
  hb_retl(( WINBOOL ) PatBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), ( DWORD ) hb_parnl(6) ));
}

/*
WINGDIAPI WINBOOL WINAPI Pie(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WINAPI_PIE )
{
  hb_retl(( WINBOOL ) Pie( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9) ));
}

/*
WINGDIAPI WINBOOL WINAPI PlayMetaFile(HDC hdc,HMETAFILE hmf)
*/
HB_FUNC( WINAPI_PLAYMETAFILE )
{
  hb_retl(( WINBOOL ) PlayMetaFile( static_cast<HDC>(hb_parptr(1)), static_cast<HMETAFILE>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI PaintRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_PAINTRGN )
{
  hb_retl(( WINBOOL ) PaintRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI PolyPolygon(HDC hdc,CONST POINT *apt,CONST INT *asz,int csz)
*/

/*
WINGDIAPI WINBOOL WINAPI PtInRegion(HRGN hrgn,int x,int y)
*/
HB_FUNC( WINAPI_PTINREGION )
{
  hb_retl(( WINBOOL ) PtInRegion( static_cast<HRGN>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI PtVisible(HDC hdc,int x,int y)
*/
HB_FUNC( WINAPI_PTVISIBLE )
{
  hb_retl(( WINBOOL ) PtVisible( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI RectInRegion(HRGN hrgn,CONST RECT *lprect)
*/

/*
WINGDIAPI WINBOOL WINAPI RectVisible(HDC hdc,CONST RECT *lprect)
*/

/*
WINGDIAPI WINBOOL WINAPI Rectangle(HDC hdc,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_RECTANGLE )
{
  hb_retl(( WINBOOL ) Rectangle( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ));
}

/*
WINGDIAPI WINBOOL WINAPI RestoreDC(HDC hdc,int nSavedDC)
*/
HB_FUNC( WINAPI_RESTOREDC )
{
  hb_retl(( WINBOOL ) RestoreDC( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
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
  hb_retni(( UINT ) RealizePalette( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceA(LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEA )
{
  hb_retl(( WINBOOL ) RemoveFontResourceA( ( LPCSTR ) hb_parc(1) ));
}

/*
WINGDIAPI WINBOOL WINAPI RemoveFontResourceW(LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_REMOVEFONTRESOURCEW )
{
  hb_retl(( WINBOOL ) RemoveFontResourceW( ( LPCWSTR ) hb_parc(1) ));
}

/*
WINGDIAPI WINBOOL WINAPI RoundRect(HDC hdc,int left,int top,int right,int bottom,int width,int height)
*/
HB_FUNC( WINAPI_ROUNDRECT )
{
  hb_retl(( WINBOOL ) RoundRect( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7) ));
}

/*
WINGDIAPI WINBOOL WINAPI ResizePalette(HPALETTE hpal,UINT n)
*/
HB_FUNC( WINAPI_RESIZEPALETTE )
{
  hb_retl(( WINBOOL ) ResizePalette( static_cast<HPALETTE>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)) ));
}

/*
WINGDIAPI int WINAPI SaveDC(HDC hdc)
*/
HB_FUNC( WINAPI_SAVEDC )
{
  hb_retni(SaveDC( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI SelectClipRgn(HDC hdc,HRGN hrgn)
*/
HB_FUNC( WINAPI_SELECTCLIPRGN )
{
  hb_retni(SelectClipRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)) ));
}

/*
WINGDIAPI int WINAPI ExtSelectClipRgn(HDC hdc,HRGN hrgn,int mode)
*/
HB_FUNC( WINAPI_EXTSELECTCLIPRGN )
{
  hb_retni(ExtSelectClipRgn( static_cast<HDC>(hb_parptr(1)), static_cast<HRGN>(hb_parptr(2)), hb_parni(3) ));
}

/*
WINGDIAPI int WINAPI SetMetaRgn(HDC hdc)
*/
HB_FUNC( WINAPI_SETMETARGN )
{
  hb_retni(SetMetaRgn( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI HGDIOBJ WINAPI SelectObject(HDC hdc,HGDIOBJ h)
*/
HB_FUNC( WINAPI_SELECTOBJECT )
{
  hb_retptr(SelectObject(static_cast<HDC>(hb_parptr(1)), static_cast<HGDIOBJ>(hb_parptr(2))));
}

/*
WINGDIAPI HPALETTE WINAPI SelectPalette(HDC hdc,HPALETTE hPal,WINBOOL bForceBkgd)
*/
HB_FUNC( WINAPI_SELECTPALETTE )
{
  hb_retptr(SelectPalette(static_cast<HDC>(hb_parptr(1)), static_cast<HPALETTE>(hb_parptr(2)), ( WINBOOL ) hb_parl(3)));
}

/*
WINGDIAPI COLORREF WINAPI SetBkColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETBKCOLOR )
{
  hb_retnl( ( COLORREF ) SetBkColor( static_cast<HDC>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ) );
}

/*
WINGDIAPI COLORREF WINAPI SetDCBrushColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETDCBRUSHCOLOR )
{
  hb_retnl( ( COLORREF ) SetDCBrushColor( static_cast<HDC>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ) );
}

/*
WINGDIAPI COLORREF WINAPI SetDCPenColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETDCPENCOLOR )
{
  hb_retnl( ( COLORREF ) SetDCPenColor( static_cast<HDC>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ) );
}

/*
WINGDIAPI int WINAPI SetBkMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETBKMODE )
{
  hb_retni(SetBkMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
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
  hb_retnl( ( DWORD ) SetMapperFlags( static_cast<HDC>(hb_parptr(1)), ( DWORD ) hb_parnl(2) ) );
}

/*
WINGDIAPI int WINAPI SetGraphicsMode(HDC hdc,int iMode)
*/
HB_FUNC( WINAPI_SETGRAPHICSMODE )
{
  hb_retni(SetGraphicsMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI int WINAPI SetMapMode(HDC hdc,int iMode)
*/
HB_FUNC( WINAPI_SETMAPMODE )
{
  hb_retni(SetMapMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI DWORD WINAPI SetLayout(HDC hdc,DWORD l)
*/
HB_FUNC( WINAPI_SETLAYOUT )
{
  hb_retnl( ( DWORD ) SetLayout( static_cast<HDC>(hb_parptr(1)), ( DWORD ) hb_parnl(2) ) );
}

/*
WINGDIAPI DWORD WINAPI GetLayout(HDC hdc)
*/
HB_FUNC( WINAPI_GETLAYOUT )
{
  hb_retnl( ( DWORD ) GetLayout( static_cast<HDC>(hb_parptr(1)) ) );
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
  hb_retnl( ( COLORREF ) SetPixel( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), static_cast<COLORREF>(hb_parnl(4)) ) );
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelV(HDC hdc,int x,int y,COLORREF color)
*/
HB_FUNC( WINAPI_SETPIXELV )
{
  hb_retl(( WINBOOL ) SetPixelV( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), static_cast<COLORREF>(hb_parnl(4)) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetPixelFormat(HDC hdc,int format,CONST PIXELFORMATDESCRIPTOR *ppfd)
*/

/*
WINGDIAPI int WINAPI SetPolyFillMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETPOLYFILLMODE )
{
  hb_retni(SetPolyFillMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI WINBOOL WINAPI StretchBlt(HDC hdcDest,int xDest,int yDest,int wDest,int hDest,HDC hdcSrc,int xSrc,int ySrc,int wSrc,int hSrc,DWORD rop)
*/
HB_FUNC( WINAPI_STRETCHBLT )
{
  hb_retl(( WINBOOL ) StretchBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), static_cast<HDC>(hb_parptr(6)), hb_parni(7), hb_parni(8), hb_parni(9), hb_parni(10), ( DWORD ) hb_parnl(11) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetRectRgn(HRGN hrgn,int left,int top,int right,int bottom)
*/
HB_FUNC( WINAPI_SETRECTRGN )
{
  hb_retl(( WINBOOL ) SetRectRgn( static_cast<HRGN>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5) ));
}

/*
WINGDIAPI int WINAPI StretchDIBits(HDC hdc,int xDest,int yDest,int DestWidth,int DestHeight,int xSrc,int ySrc,int SrcWidth,int SrcHeight,CONST VOID *lpBits,CONST BITMAPINFO *lpbmi,UINT iUsage,DWORD rop)
*/

/*
WINGDIAPI int WINAPI SetROP2(HDC hdc,int rop2)
*/
HB_FUNC( WINAPI_SETROP2 )
{
  hb_retni(SetROP2( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI int WINAPI SetStretchBltMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETSTRETCHBLTMODE )
{
  hb_retni(SetStretchBltMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI UINT WINAPI SetSystemPaletteUse(HDC hdc,UINT use)
*/
HB_FUNC( WINAPI_SETSYSTEMPALETTEUSE )
{
  hb_retni(( UINT ) SetSystemPaletteUse( static_cast<HDC>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)) ));
}

/*
WINGDIAPI int WINAPI SetTextCharacterExtra(HDC hdc,int extra)
*/
HB_FUNC( WINAPI_SETTEXTCHARACTEREXTRA )
{
  hb_retni(SetTextCharacterExtra( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI COLORREF WINAPI SetTextColor(HDC hdc,COLORREF color)
*/
HB_FUNC( WINAPI_SETTEXTCOLOR )
{
  hb_retnl( ( COLORREF ) SetTextColor( static_cast<HDC>(hb_parptr(1)), static_cast<COLORREF>(hb_parnl(2)) ) );
}

/*
WINGDIAPI UINT WINAPI SetTextAlign(HDC hdc,UINT align)
*/
HB_FUNC( WINAPI_SETTEXTALIGN )
{
  hb_retni(( UINT ) SetTextAlign( static_cast<HDC>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetTextJustification(HDC hdc,int extra,int count)
*/
HB_FUNC( WINAPI_SETTEXTJUSTIFICATION )
{
  hb_retl(( WINBOOL ) SetTextJustification( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateColors(HDC hdc)
*/
HB_FUNC( WINAPI_UPDATECOLORS )
{
  hb_retl(( WINBOOL ) UpdateColors( static_cast<HDC>(hb_parptr(1)) ));
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
  hb_retl(( WINBOOL ) TransparentBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), static_cast<HDC>(hb_parptr(6)), hb_parni(7), hb_parni(8), hb_parni(9), hb_parni(10), static_cast<UINT>(hb_parni(11)) ));
}

/*
WINGDIAPI WINBOOL WINAPI GdiTransparentBlt(HDC hdcDest,int xoriginDest,int yoriginDest,int wDest,int hDest,HDC hdcSrc,int xoriginSrc,int yoriginSrc,int wSrc,int hSrc,UINT crTransparent)
*/
HB_FUNC( WINAPI_GDITRANSPARENTBLT )
{
  hb_retl(( WINBOOL ) GdiTransparentBlt( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), static_cast<HDC>(hb_parptr(6)), hb_parni(7), hb_parni(8), hb_parni(9), hb_parni(10), static_cast<UINT>(hb_parni(11)) ));
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
  hb_retptr(CloseEnhMetaFile(static_cast<HDC>(hb_parptr(1))));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileA(HENHMETAFILE hEnh,LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_COPYENHMETAFILEA )
{
  hb_retptr(CopyEnhMetaFileA(static_cast<HENHMETAFILE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI CopyEnhMetaFileW(HENHMETAFILE hEnh,LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_COPYENHMETAFILEW )
{
  hb_retptr(CopyEnhMetaFileW(static_cast<HENHMETAFILE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileA(HDC hdc,LPCSTR lpFilename,CONST RECT *lprc,LPCSTR lpDesc)
*/

/*
WINGDIAPI HDC WINAPI CreateEnhMetaFileW(HDC hdc,LPCWSTR lpFilename,CONST RECT *lprc,LPCWSTR lpDesc)
*/

/*
WINGDIAPI WINBOOL WINAPI DeleteEnhMetaFile(HENHMETAFILE hmf)
*/
HB_FUNC( WINAPI_DELETEENHMETAFILE )
{
  hb_retl(( WINBOOL ) DeleteEnhMetaFile( static_cast<HENHMETAFILE>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI EnumEnhMetaFile(HDC hdc,HENHMETAFILE hmf,ENHMFENUMPROC lpProc,LPVOID lpParam,CONST RECT *lpRect)
*/

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileA(LPCSTR lpName)
*/
HB_FUNC( WINAPI_GETENHMETAFILEA )
{
  hb_retptr(GetEnhMetaFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINGDIAPI HENHMETAFILE WINAPI GetEnhMetaFileW(LPCWSTR lpName)
*/
HB_FUNC( WINAPI_GETENHMETAFILEW )
{
  hb_retptr(GetEnhMetaFileW(( LPCWSTR ) hb_parc(1)));
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileBits(HENHMETAFILE hEMF,UINT nSize,LPBYTE lpData)
*/

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionA(HENHMETAFILE hemf,UINT cchBuffer,LPSTR lpDescription)
*/
HB_FUNC( WINAPI_GETENHMETAFILEDESCRIPTIONA )
{
  hb_retni(( UINT ) GetEnhMetaFileDescriptionA( static_cast<HENHMETAFILE>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)), ( LPSTR ) hb_parc(3) ));
}

/*
WINGDIAPI UINT WINAPI GetEnhMetaFileDescriptionW(HENHMETAFILE hemf,UINT cchBuffer,LPWSTR lpDescription)
*/
HB_FUNC( WINAPI_GETENHMETAFILEDESCRIPTIONW )
{
  hb_retni(( UINT ) GetEnhMetaFileDescriptionW( static_cast<HENHMETAFILE>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)), ( LPWSTR ) hb_parc(3) ));
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

/*
WINGDIAPI UINT WINAPI GetWinMetaFileBits(HENHMETAFILE hemf,UINT cbData16,LPBYTE pData16,INT iMapMode,HDC hdcRef)
*/

/*
WINGDIAPI WINBOOL WINAPI PlayEnhMetaFile(HDC hdc,HENHMETAFILE hmf,CONST RECT *lprect)
*/

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
  hb_retptr(CreateHalftonePalette(static_cast<HDC>(hb_parptr(1))));
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
  hb_retni(EndDoc( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI StartPage(HDC hdc)
*/
HB_FUNC( WINAPI_STARTPAGE )
{
  hb_retni(StartPage( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI EndPage(HDC hdc)
*/
HB_FUNC( WINAPI_ENDPAGE )
{
  hb_retni(EndPage( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI AbortDoc(HDC hdc)
*/
HB_FUNC( WINAPI_ABORTDOC )
{
  hb_retni(AbortDoc( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI SetAbortProc(HDC hdc,ABORTPROC lpProc)
*/

/*
WINGDIAPI WINBOOL WINAPI AbortPath(HDC hdc)
*/
HB_FUNC( WINAPI_ABORTPATH )
{
  hb_retl(( WINBOOL ) AbortPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI ArcTo(HDC hdc,int left,int top,int right,int bottom,int xr1,int yr1,int xr2,int yr2)
*/
HB_FUNC( WINAPI_ARCTO )
{
  hb_retl(( WINBOOL ) ArcTo( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), hb_parni(8), hb_parni(9) ));
}

/*
WINGDIAPI WINBOOL WINAPI BeginPath(HDC hdc)
*/
HB_FUNC( WINAPI_BEGINPATH )
{
  hb_retl(( WINBOOL ) BeginPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI CloseFigure(HDC hdc)
*/
HB_FUNC( WINAPI_CLOSEFIGURE )
{
  hb_retl(( WINBOOL ) CloseFigure( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI EndPath(HDC hdc)
*/
HB_FUNC( WINAPI_ENDPATH )
{
  hb_retl(( WINBOOL ) EndPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI FillPath(HDC hdc)
*/
HB_FUNC( WINAPI_FILLPATH )
{
  hb_retl(( WINBOOL ) FillPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI FlattenPath(HDC hdc)
*/
HB_FUNC( WINAPI_FLATTENPATH )
{
  hb_retl(( WINBOOL ) FlattenPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI GetPath(HDC hdc,LPPOINT apt,LPBYTE aj,int cpt)
*/

/*
WINGDIAPI HRGN WINAPI PathToRegion(HDC hdc)
*/
HB_FUNC( WINAPI_PATHTOREGION )
{
  hb_retptr(PathToRegion(static_cast<HDC>(hb_parptr(1))));
}

/*
WINGDIAPI WINBOOL WINAPI PolyDraw(HDC hdc,CONST POINT *apt,CONST BYTE *aj,int cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI SelectClipPath(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SELECTCLIPPATH )
{
  hb_retl(( WINBOOL ) SelectClipPath( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI int WINAPI SetArcDirection(HDC hdc,int dir)
*/
HB_FUNC( WINAPI_SETARCDIRECTION )
{
  hb_retni(SetArcDirection( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetMiterLimit(HDC hdc,FLOAT limit,PFLOAT old)
*/

/*
WINGDIAPI WINBOOL WINAPI StrokeAndFillPath(HDC hdc)
*/
HB_FUNC( WINAPI_STROKEANDFILLPATH )
{
  hb_retl(( WINBOOL ) StrokeAndFillPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI StrokePath(HDC hdc)
*/
HB_FUNC( WINAPI_STROKEPATH )
{
  hb_retl(( WINBOOL ) StrokePath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI WidenPath(HDC hdc)
*/
HB_FUNC( WINAPI_WIDENPATH )
{
  hb_retl(( WINBOOL ) WidenPath( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI HPEN WINAPI ExtCreatePen(DWORD iPenStyle,DWORD cWidth,CONST LOGBRUSH *plbrush,DWORD cStyle,CONST DWORD *pstyle)
*/

/*
WINGDIAPI WINBOOL WINAPI GetMiterLimit(HDC hdc,PFLOAT plimit)
*/

/*
WINGDIAPI int WINAPI GetArcDirection(HDC hdc)
*/
HB_FUNC( WINAPI_GETARCDIRECTION )
{
  hb_retni(GetArcDirection( static_cast<HDC>(hb_parptr(1)) ));
}

/*
WINGDIAPI int WINAPI GetObjectA(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WINAPI_GETOBJECTA )
{
  hb_retni(GetObjectA( static_cast<HANDLE>(hb_parptr(1)), hb_parni(2), static_cast<LPVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI int WINAPI GetObjectW(HANDLE h,int c,LPVOID pv)
*/
HB_FUNC( WINAPI_GETOBJECTW )
{
  hb_retni(GetObjectW( static_cast<HANDLE>(hb_parptr(1)), hb_parni(2), static_cast<LPVOID>(hb_parptr(3)) ));
}

/*
WINGDIAPI WINBOOL WINAPI MoveToEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI TextOutA(HDC hdc,int x,int y,LPCSTR lpString,int c)
*/
HB_FUNC( WINAPI_TEXTOUTA )
{
  hb_retl(( WINBOOL ) TextOutA( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), ( LPCSTR ) hb_parc(4), hb_parni(5) ));
}

/*
WINGDIAPI WINBOOL WINAPI TextOutW(HDC hdc,int x,int y,LPCWSTR lpString,int c)
*/
HB_FUNC( WINAPI_TEXTOUTW )
{
  hb_retl(( WINBOOL ) TextOutW( static_cast<HDC>(hb_parptr(1)), hb_parni(2), hb_parni(3), ( LPCWSTR ) hb_parc(4), hb_parni(5) ));
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

/*
WINGDIAPI WINBOOL WINAPI DPtoLP(HDC hdc,LPPOINT lppt,int c)
*/

/*
WINGDIAPI WINBOOL WINAPI LPtoDP(HDC hdc,LPPOINT lppt,int c)
*/

/*
WINGDIAPI WINBOOL WINAPI Polygon(HDC hdc,CONST POINT *apt,int cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI Polyline(HDC hdc,CONST POINT *apt,int cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI PolyBezier(HDC hdc,CONST POINT *apt,DWORD cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI PolyBezierTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI PolylineTo(HDC hdc,CONST POINT *apt,DWORD cpt)
*/

/*
WINGDIAPI WINBOOL WINAPI SetViewportExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI SetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI SetWindowExtEx(HDC hdc,int x,int y,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI SetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI OffsetViewportOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI OffsetWindowOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI WINBOOL WINAPI ScaleViewportExtEx(HDC hdc,int xn,int dx,int yn,int yd,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI ScaleWindowExtEx(HDC hdc,int xn,int xd,int yn,int yd,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI SetBitmapDimensionEx(HBITMAP hbm,int w,int h,LPSIZE lpsz)
*/

/*
WINGDIAPI WINBOOL WINAPI SetBrushOrgEx(HDC hdc,int x,int y,LPPOINT lppt)
*/

/*
WINGDIAPI int WINAPI GetTextFaceA(HDC hdc,int c,LPSTR lpName)
*/
HB_FUNC( WINAPI_GETTEXTFACEA )
{
  hb_retni(GetTextFaceA( static_cast<HDC>(hb_parptr(1)), hb_parni(2), ( LPSTR ) hb_parc(3) ));
}

/*
WINGDIAPI int WINAPI GetTextFaceW(HDC hdc,int c,LPWSTR lpName)
*/
HB_FUNC( WINAPI_GETTEXTFACEW )
{
  hb_retni(GetTextFaceW( static_cast<HDC>(hb_parptr(1)), hb_parni(2), ( LPWSTR ) hb_parc(3) ));
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

/*
WINGDIAPI WINBOOL WINAPI FixBrushOrgEx(HDC hdc,int x,int y,LPPOINT ptl)
*/

/*
WINGDIAPI WINBOOL WINAPI UnrealizeObject(HGDIOBJ h)
*/
HB_FUNC( WINAPI_UNREALIZEOBJECT )
{
  hb_retl(( WINBOOL ) UnrealizeObject( static_cast<HGDIOBJ>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI GdiFlush(void)
*/
HB_FUNC( WINAPI_GDIFLUSH )
{
  hb_retl(( WINBOOL ) GdiFlush());
}

/*
WINGDIAPI DWORD WINAPI GdiSetBatchLimit(DWORD dw)
*/
HB_FUNC( WINAPI_GDISETBATCHLIMIT )
{
  hb_retnl( ( DWORD ) GdiSetBatchLimit( ( DWORD ) hb_parnl(1) ) );
}

/*
WINGDIAPI DWORD WINAPI GdiGetBatchLimit(void)
*/
HB_FUNC( WINAPI_GDIGETBATCHLIMIT )
{
  hb_retnl( ( DWORD ) GdiGetBatchLimit() );
}

/*
WINGDIAPI int WINAPI SetICMMode(HDC hdc,int mode)
*/
HB_FUNC( WINAPI_SETICMMODE )
{
  hb_retni(SetICMMode( static_cast<HDC>(hb_parptr(1)), hb_parni(2) ));
}

/*
WINGDIAPI WINBOOL WINAPI CheckColorsInGamut(HDC hdc,LPVOID lpRGBTriple,LPVOID dlpBuffer,DWORD nCount)
*/
HB_FUNC( WINAPI_CHECKCOLORSINGAMUT )
{
  hb_retl(( WINBOOL ) CheckColorsInGamut( static_cast<HDC>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), static_cast<LPVOID>(hb_parptr(3)), ( DWORD ) hb_parnl(4) ));
}

/*
WINGDIAPI HCOLORSPACE WINAPI GetColorSpace(HDC hdc)
*/
HB_FUNC( WINAPI_GETCOLORSPACE )
{
  hb_retptr(GetColorSpace(static_cast<HDC>(hb_parptr(1))));
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
  hb_retptr(SetColorSpace(static_cast<HDC>(hb_parptr(1)), static_cast<HCOLORSPACE>(hb_parptr(2))));
}

/*
WINGDIAPI WINBOOL WINAPI DeleteColorSpace(HCOLORSPACE hcs)
*/
HB_FUNC( WINAPI_DELETECOLORSPACE )
{
  hb_retl(( WINBOOL ) DeleteColorSpace( static_cast<HCOLORSPACE>(hb_parptr(1)) ));
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileA(HDC hdc,LPDWORD pBufSize,LPSTR pszFilename)
*/
HB_FUNC( WINAPI_GETICMPROFILEA )
{
  hb_retl(( WINBOOL ) GetICMProfileA( static_cast<HDC>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), ( LPSTR ) hb_parc(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI GetICMProfileW(HDC hdc,LPDWORD pBufSize,LPWSTR pszFilename)
*/
HB_FUNC( WINAPI_GETICMPROFILEW )
{
  hb_retl(( WINBOOL ) GetICMProfileW( static_cast<HDC>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), ( LPWSTR ) hb_parc(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileA(HDC hdc,LPSTR lpFileName)
*/
HB_FUNC( WINAPI_SETICMPROFILEA )
{
  hb_retl(( WINBOOL ) SetICMProfileA( static_cast<HDC>(hb_parptr(1)), ( LPSTR ) hb_parc(2) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetICMProfileW(HDC hdc,LPWSTR lpFileName)
*/
HB_FUNC( WINAPI_SETICMPROFILEW )
{
  hb_retl(( WINBOOL ) SetICMProfileW( static_cast<HDC>(hb_parptr(1)), ( LPWSTR ) hb_parc(2) ));
}

/*
WINGDIAPI WINBOOL WINAPI GetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WINAPI_GETDEVICEGAMMARAMP )
{
  hb_retl(( WINBOOL ) GetDeviceGammaRamp( static_cast<HDC>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI SetDeviceGammaRamp(HDC hdc,LPVOID lpRamp)
*/
HB_FUNC( WINAPI_SETDEVICEGAMMARAMP )
{
  hb_retl(( WINBOOL ) SetDeviceGammaRamp( static_cast<HDC>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI ColorMatchToTarget(HDC hdc,HDC hdcTarget,DWORD action)
*/
HB_FUNC( WINAPI_COLORMATCHTOTARGET )
{
  hb_retl(( WINBOOL ) ColorMatchToTarget( static_cast<HDC>(hb_parptr(1)), static_cast<HDC>(hb_parptr(2)), ( DWORD ) hb_parnl(3) ));
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
  hb_retl(( WINBOOL ) UpdateICMRegKeyA( ( DWORD ) hb_parnl(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), static_cast<UINT>(hb_parni(4)) ));
}

/*
WINGDIAPI WINBOOL WINAPI UpdateICMRegKeyW(DWORD reserved,LPWSTR lpszCMID,LPWSTR lpszFileName,UINT command)
*/
HB_FUNC( WINAPI_UPDATEICMREGKEYW )
{
  hb_retl(( WINBOOL ) UpdateICMRegKeyW( ( DWORD ) hb_parnl(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), static_cast<UINT>(hb_parni(4)) ));
}

/*
WINGDIAPI WINBOOL WINAPI ColorCorrectPalette(HDC hdc,HPALETTE hPal,DWORD deFirst,DWORD num)
*/
HB_FUNC( WINAPI_COLORCORRECTPALETTE )
{
  hb_retl(( WINBOOL ) ColorCorrectPalette( static_cast<HDC>(hb_parptr(1)), static_cast<HPALETTE>(hb_parptr(2)), ( DWORD ) hb_parnl(3), ( DWORD ) hb_parnl(4) ));
}

/*
WINGDIAPI WINBOOL WINAPI wglCopyContext(HGLRC,HGLRC,UINT)
*/
HB_FUNC( WINAPI_WGLCOPYCONTEXT )
{
  hb_retl(( WINBOOL ) wglCopyContext( static_cast<HGLRC>(hb_parptr(1)), static_cast<HGLRC>(hb_parptr(2)), static_cast<UINT>(hb_parni(3)) ));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateContext(HDC)
*/
HB_FUNC( WINAPI_WGLCREATECONTEXT )
{
  hb_retptr(wglCreateContext(static_cast<HDC>(hb_parptr(1))));
}

/*
WINGDIAPI HGLRC WINAPI wglCreateLayerContext(HDC,int)
*/
HB_FUNC( WINAPI_WGLCREATELAYERCONTEXT )
{
  hb_retptr(wglCreateLayerContext(static_cast<HDC>(hb_parptr(1)), hb_parni(2)));
}

/*
WINGDIAPI WINBOOL WINAPI wglDeleteContext(HGLRC)
*/
HB_FUNC( WINAPI_WGLDELETECONTEXT )
{
  hb_retl(( WINBOOL ) wglDeleteContext( static_cast<HGLRC>(hb_parptr(1)) ));
}

/*
WINGDIAPI HGLRC WINAPI wglGetCurrentContext(VOID)
*/
HB_FUNC( WINAPI_WGLGETCURRENTCONTEXT )
{
  hb_retptr(wglGetCurrentContext());
}

/*
WINGDIAPI HDC WINAPI wglGetCurrentDC(VOID)
*/
HB_FUNC( WINAPI_WGLGETCURRENTDC )
{
  hb_retptr(wglGetCurrentDC());
}

/*
WINGDIAPI PROC WINAPI wglGetProcAddress(LPCSTR)
*/

/*
WINGDIAPI WINBOOL WINAPI wglMakeCurrent(HDC,HGLRC)
*/
HB_FUNC( WINAPI_WGLMAKECURRENT )
{
  hb_retl(( WINBOOL ) wglMakeCurrent( static_cast<HDC>(hb_parptr(1)), static_cast<HGLRC>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI wglShareLists(HGLRC,HGLRC)
*/
HB_FUNC( WINAPI_WGLSHARELISTS )
{
  hb_retl(( WINBOOL ) wglShareLists( static_cast<HGLRC>(hb_parptr(1)), static_cast<HGLRC>(hb_parptr(2)) ));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsA(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WINAPI_WGLUSEFONTBITMAPSA )
{
  hb_retl(( WINBOOL ) wglUseFontBitmapsA( static_cast<HDC>(hb_parptr(1)), ( DWORD ) hb_parnl(2), ( DWORD ) hb_parnl(3), ( DWORD ) hb_parnl(4) ));
}

/*
WINGDIAPI WINBOOL WINAPI wglUseFontBitmapsW(HDC,DWORD,DWORD,DWORD)
*/
HB_FUNC( WINAPI_WGLUSEFONTBITMAPSW )
{
  hb_retl(( WINBOOL ) wglUseFontBitmapsW( static_cast<HDC>(hb_parptr(1)), ( DWORD ) hb_parnl(2), ( DWORD ) hb_parnl(3), ( DWORD ) hb_parnl(4) ));
}

/*
WINGDIAPI WINBOOL WINAPI SwapBuffers(HDC)
*/
HB_FUNC( WINAPI_SWAPBUFFERS )
{
  hb_retl(( WINBOOL ) SwapBuffers( static_cast<HDC>(hb_parptr(1)) ));
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
  hb_retl(( WINBOOL ) wglRealizeLayerPalette( static_cast<HDC>(hb_parptr(1)), hb_parni(2), ( WINBOOL ) hb_parl(3) ));
}

/*
WINGDIAPI WINBOOL WINAPI wglSwapLayerBuffers(HDC,UINT)
*/
HB_FUNC( WINAPI_WGLSWAPLAYERBUFFERS )
{
  hb_retl(( WINBOOL ) wglSwapLayerBuffers( static_cast<HDC>(hb_parptr(1)), static_cast<UINT>(hb_parni(2)) ));
}

/*
WINGDIAPI DWORD WINAPI wglSwapMultipleBuffers(UINT,CONST WGLSWAP *)
*/
