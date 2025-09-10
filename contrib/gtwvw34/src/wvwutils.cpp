/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW draw functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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

#include "hbgtwvw.hpp"

#include <hbapifs.hpp>

/*
removed from GTWVT, so we remove it from here also. I really don't like doing it...
*/
HB_FUNC(WVW_DELETEOBJECT)
{
  hb_retl(DeleteObject(hbwapi_par_raw_HGDIOBJ(1)));
}

HB_FUNC(WIN_SELECTOBJECT)
{
  hbwapi_ret_raw_HANDLE(SelectObject(hbwapi_par_raw_HDC(1), hbwapi_par_raw_HGDIOBJ(2)));
}

HB_FUNC(WIN_GETDC)
{
  hbwapi_ret_raw_HANDLE(GetDC(hbwapi_par_raw_HWND(1)));
}

HB_FUNC(WIN_RELEASEDC)
{
  hb_retl(ReleaseDC(hbwapi_par_raw_HWND(1), hbwapi_par_raw_HDC(2)));
}

/*
Additions to GTWVW developed by SOLUCIONES PERCEPTIVAS
*/

HB_FUNC(WVW_GBCREATE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  HWND hWnd = nullptr;

  if (wvw_win) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    int iOffTop = HB_ISARRAY(9) ? hb_parvni(9, 1) : -1;
    int iOffLeft = HB_ISARRAY(9) ? hb_parvni(9, 2) : -1;
    int iOffBottom = HB_ISARRAY(9) ? hb_parvni(9, 3) : 1;
    int iOffRight = HB_ISARRAY(9) ? hb_parvni(9, 4) : 1;

    void *hCaption;
    hb_retni(hb_gt_wvw_ButtonCreate(wvw_win, iTop, iLeft, iBottom, iRight, HB_PARSTR(6, &hCaption, nullptr), hb_parc(7),
                                    static_cast<HB_UINT>(hb_parni(7)), hb_param(8, Harbour::Item::EVALITEM), iOffTop,
                                    iOffLeft, iOffBottom, iOffRight, HB_ISNUM(10) ? hb_parnd(10) : 1 /* dStretch */,
                                    hb_parl(11) /* bMap3Dcolors */,
                                    BS_TEXT | BS_GROUPBOX | WS_OVERLAPPED | hb_parni(13) /* nStyle */, &hWnd));
    hb_strfree(hCaption);
  } else {
    hb_retni(0);
  }

  hbwapi_stor_HANDLE(hWnd, 12);
}

/*
BS_TEXT | BS_GROUPBOX | WS_OVERLAPPED | WS_GROUP
*/

HB_FUNC(WVW_RBCREATE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  HWND hWnd = nullptr;

  if (wvw_win && HB_ISEVALITEM(8)) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    int iOffTop = HB_ISARRAY(9) ? hb_parvni(9, 1) : -2;
    int iOffLeft = HB_ISARRAY(9) ? hb_parvni(9, 2) : -2;
    int iOffBottom = HB_ISARRAY(9) ? hb_parvni(9, 3) : 2;
    int iOffRight = HB_ISARRAY(9) ? hb_parvni(9, 4) : 2;

    void *hCaption;
    hb_retni(hb_gt_wvw_ButtonCreate(wvw_win, iTop, iLeft, iBottom, iRight, HB_PARSTR(6, &hCaption, nullptr), hb_parc(7),
                                    static_cast<HB_UINT>(hb_parni(7)), hb_param(8, Harbour::Item::EVALITEM), iOffTop,
                                    iOffLeft, iOffBottom, iOffRight, HB_ISNUM(10) ? hb_parnd(10) : 1 /* dStretch */,
                                    hb_parl(11) /* bMap3Dcolors */, BS_AUTORADIOBUTTON | hb_parni(13) /* nStyle */,
                                    &hWnd));
    hb_strfree(hCaption);
  } else {
    hb_retni(0);
  }

  hbwapi_stor_HANDLE(hWnd, 12);
}

HB_FUNC(WVW_SETCONTROLTEXT)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto hWnd = hb_gt_wvw_FindControlHandle(wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);

  if (hWnd) {
    void *hText;
    SetWindowText(hWnd, HB_PARSTRDEF(3, &hText, nullptr));
    hb_strfree(hText);

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_MOUSE_COL)
{
  if (hb_gt_wvw_GetMainCoordMode()) {
    auto wvw_top = hb_gt_wvw_win_top();

    if (wvw_top) {
      hb_retni(hb_gt_wvw_GetMouseX(wvw_top) + hb_gt_wvw_ColOfs(wvw_top));
      return;
    }
  } else {
    auto wvw_win = hb_gt_wvw_win_cur();

    if (wvw_win) {
      hb_retni(hb_gt_wvw_GetMouseX(wvw_win));
      return;
    }
  }

  hb_retni(0);
}

HB_FUNC(WVW_MOUSE_ROW)
{
  if (hb_gt_wvw_GetMainCoordMode()) {
    auto wvw_top = hb_gt_wvw_win_top();

    if (wvw_top) {
      hb_retni(hb_gt_wvw_GetMouseY(wvw_top) + hb_gt_wvw_RowOfs(wvw_top));
      return;
    }
  } else {
    auto wvw_win = hb_gt_wvw_win_cur();

    if (wvw_win) {
      hb_retni(hb_gt_wvw_GetMouseY(wvw_win));
      return;
    }
  }

  hb_retni(0);
}

HB_FUNC(WVW_ADDTOOLTIPEX) /* changed by MAG */
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win) {
    int iStyle = TTS_ALWAYSTIP;
    INITCOMMONCONTROLSEX icex{};

    /* Load the tooltip class from the DLL. */
    icex.dwSize = sizeof(icex);
    icex.dwICC = ICC_BAR_CLASSES;

    if (!InitCommonControlsEx(&icex)) {
    }

#if 0
      if( lToolTipBalloon ) {
         iStyle |= TTS_BALLOON;
      }
#endif

    if (!wvw->hWndTT) {
      wvw->hWndTT = CreateWindow(TOOLTIPS_CLASS, nullptr, iStyle, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                                 CW_USEDEFAULT, nullptr, nullptr, GetModuleHandle(nullptr), nullptr);
    }
    if (wvw->hWndTT) {
      void *hText;

      TOOLINFO ti{};

      ti.cbSize = sizeof(ti);
      ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
      ti.hwnd = wvw_win->hWnd;
      ti.uId = reinterpret_cast<UINT_PTR>(wvw_win->hWnd);
      ti.hinst = GetModuleHandle(nullptr);
      ti.lpszText = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTRDEF(3, &hText, nullptr)));

      hb_retl(static_cast<bool>(SendMessage(wvw->hWndTT, TTM_ADDTOOL, 0, reinterpret_cast<LPARAM>(&ti))));

      hb_strfree(hText);
      return;
    }
  }

  hb_retl(false);
}

/*
wvw_CreateImageList(array, cx, cy, nGrow, flags)
*/
HB_FUNC(WVW_CREATEIMAGELIST)
{
  auto pArray = hb_param(1, Harbour::Item::ARRAY);

  if (pArray) {
    auto ulLen = static_cast<int>(hb_arrayLen(pArray));

    HIMAGELIST himl =
        ImageList_Create(hb_parni(2), hb_parni(3), static_cast<UINT>(hb_parnidef(5, ILC_COLOR)), ulLen, hb_parni(4));

    for (auto ul = 1; ul <= ulLen; ++ul) {
      auto hbmp = static_cast<HBITMAP>(hbwapi_arrayGet_HANDLE(pArray, ul));
      ImageList_Add(himl, hbmp, nullptr);
      DeleteObject(hbmp);
    }

    hbwapi_ret_raw_HANDLE(himl);
  } else {
    hbwapi_ret_raw_HANDLE(nullptr);
  }
}

HB_FUNC(WVW_IMAGELIST_ADD)
{
  hb_retni(ImageList_Add(hbwapi_par_raw_HIMAGELIST(1), hbwapi_par_raw_HBITMAP(2), nullptr));
}

HB_FUNC(WVW_IMAGELIST_ADDMASKED)
{
  hb_retni(ImageList_AddMasked(hbwapi_par_raw_HIMAGELIST(1), hbwapi_par_raw_HBITMAP(2), hbwapi_par_COLORREF(3)));
}

HB_FUNC(WVW_GETBITMAPSIZE)
{
  auto aMetr = hb_itemArrayNew(3);
  BITMAP bm;

  GetObject(hbwapi_par_raw_HBITMAP(1), sizeof(bm), static_cast<LPVOID>(&bm));

  hb_arraySetNL(aMetr, 1, bm.bmWidth);
  hb_arraySetNL(aMetr, 2, bm.bmHeight);
  hb_arraySetNI(aMetr, 3, bm.bmBitsPixel);

  hb_itemReturnRelease(aMetr);
}

HB_FUNC(WVW_OPENIMAGE)
{
  HGLOBAL hG = nullptr;

  if (hb_parl(2) /* lString */) {
    auto nFileSize = static_cast<SIZE_T>(hb_parclen(1));
    hG = GlobalAlloc(GPTR, nFileSize);
    if (hG) {
      memcpy(hG, hb_parcx(1), nFileSize);
    }
  } else {
    PHB_FILE fhnd =
        hb_fileExtOpen(hb_parcx(1), nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK, nullptr, nullptr);
    if (fhnd) {
      auto nFileSize = static_cast<SIZE_T>(hb_fileSize(fhnd));
      hG = GlobalAlloc(GPTR, nFileSize);
      if (hG) {
        hb_fileReadAt(fhnd, hG, nFileSize, 0);
      }
      hb_fileClose(fhnd);
    }
  }

  if (hG) {
    IPicture *pPicture = nullptr;
    IStream *pStream = nullptr;

    if (CreateStreamOnHGlobal(hG, FALSE, &pStream) == S_OK && pStream) {
      OleLoadPicture(pStream, 0, FALSE, HB_ID_REF(IID_IPicture), reinterpret_cast<LPVOID *>(&pPicture));
      HB_VTBL(pStream)->Release(HB_THIS(pStream));
    }

    GlobalFree(hG);

    if (pPicture) {
      HBITMAP hBitmap = nullptr;

      if (HB_VTBL(pPicture)->get_Handle(HB_THIS_(pPicture) reinterpret_cast<OLE_HANDLE *>(&hBitmap)) == S_OK &&
          hBitmap) {
        hbwapi_ret_raw_HANDLE(CopyImage(hBitmap, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG));
      } else {
        hbwapi_ret_raw_HANDLE(nullptr);
      }

      HB_VTBL(pPicture)->Release(HB_THIS(pPicture));

      return;
    }
  }

  hbwapi_ret_raw_HANDLE(nullptr);
}

HB_FUNC(WVW_OPENBITMAP)
{
  PHB_FILE fhnd = hb_fileExtOpen(hb_parcx(1), nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK | FXO_NOSEEKPOS,
                                 nullptr, nullptr);

  HBITMAP hbm = nullptr;

  if (fhnd) {
    BITMAPFILEHEADER bmfh;
    BITMAPINFOHEADER bmih;
    HGLOBAL hmem1;

    hb_fileRead(fhnd, &bmfh, sizeof(bmfh), -1); /* Retrieve the BITMAPFILEHEADER structure. */
    hb_fileRead(fhnd, &bmih, sizeof(bmih), -1); /* Retrieve the BITMAPFILEHEADER structure. */

    /* Allocate memory for the BITMAPINFO structure. */
    hmem1 = GlobalAlloc(GHND, sizeof(BITMAPINFOHEADER) + (static_cast<SIZE_T>(1) << bmih.biBitCount) * sizeof(RGBQUAD));
    if (hmem1) {
      HGLOBAL hmem2;

      auto lpbmi = static_cast<LPBITMAPINFO>(GlobalLock(hmem1));

      /* Load BITMAPINFOHEADER into the BITMAPINFO  structure. */
      lpbmi->bmiHeader.biSize = bmih.biSize;
      lpbmi->bmiHeader.biWidth = bmih.biWidth;
      lpbmi->bmiHeader.biHeight = bmih.biHeight;
      lpbmi->bmiHeader.biPlanes = bmih.biPlanes;

      lpbmi->bmiHeader.biBitCount = bmih.biBitCount;
      lpbmi->bmiHeader.biCompression = bmih.biCompression;
      lpbmi->bmiHeader.biSizeImage = bmih.biSizeImage;
      lpbmi->bmiHeader.biXPelsPerMeter = bmih.biXPelsPerMeter;
      lpbmi->bmiHeader.biYPelsPerMeter = bmih.biYPelsPerMeter;
      lpbmi->bmiHeader.biClrUsed = bmih.biClrUsed;
      lpbmi->bmiHeader.biClrImportant = bmih.biClrImportant;

      /* Retrieve the color table.
         1 << bmih.biBitCount == 2 ^ bmih.biBitCount */
      switch (bmih.biBitCount) {
      case 1:
      case 4:
      case 8:
        hb_fileRead(fhnd, lpbmi->bmiColors, (static_cast<SIZE_T>(1) << bmih.biBitCount) * sizeof(RGBQUAD), -1);
        break;

      case 16:
      case 32:
        if (bmih.biCompression == BI_BITFIELDS) {
          hb_fileRead(fhnd, lpbmi->bmiColors, 3 * sizeof(RGBQUAD), -1);
        }
        break;

      case 24:
        break;
      }

      /* Allocate memory for the required number of bytes. */
      hmem2 = GlobalAlloc(GHND, (bmfh.bfSize - bmfh.bfOffBits));
      if (hmem2) {
        HDC hDC = hbwapi_par_raw_HDC(2);

        LPVOID lpvBits = GlobalLock(hmem2);

        /* Retrieve the bitmap data. */
        hb_fileRead(fhnd, lpvBits, bmfh.bfSize - bmfh.bfOffBits, -1);

        if (!hDC) {
          hDC = GetDC(0);
        }

        /* Create a bitmap from the data stored in the .bmp file.  */
        hbm = CreateDIBitmap(hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS);

        if (!hbwapi_is_HANDLE(2)) {
          ReleaseDC(0, hDC);
        }

        GlobalUnlock(hmem2);
        GlobalFree(hmem2);
      }

      GlobalUnlock(hmem1);
      GlobalFree(hmem1);
    }

    hb_fileClose(fhnd);
  }

  hbwapi_ret_raw_HANDLE(hbm);
}

/*
wvw_CreateFont(cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,
               lStrikeout, nCharSet, nQuality, nEscapement)
*/
HB_FUNC(WVW_CREATEFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_top = hb_gt_wvw_win_top();

  if (wvw && wvw_top) {
    LOGFONT lf{};

    lf.lfEscapement = hb_parnl(10) * 10;
    lf.lfOrientation = 0;
    lf.lfWeight = hb_parnl(4);
    lf.lfItalic = static_cast<BYTE>(hb_parl(5));
    lf.lfUnderline = static_cast<BYTE>(hb_parl(6));
    lf.lfStrikeOut = static_cast<BYTE>(hb_parl(7));
    lf.lfCharSet = static_cast<BYTE>(hb_parnidef(8, wvw_top->CodePage));
    lf.lfOutPrecision = 0;
    lf.lfClipPrecision = 0;
    lf.lfQuality = static_cast<BYTE>(hb_parnidef(9, DEFAULT_QUALITY));
    lf.lfPitchAndFamily = FF_DONTCARE;
    lf.lfHeight = hb_parnldef(2, wvw_top->fontHeight);
    lf.lfWidth = hb_parnldef(3, wvw_top->fontWidth < 0 ? -wvw_top->fontWidth : wvw_top->fontWidth);

    if (HB_ISCHAR(1)) {
      HB_ITEMCOPYSTR(hb_param(1, Harbour::Item::STRING), lf.lfFaceName, HB_SIZEOFARRAY(lf.lfFaceName));
      wvw_top->fontFace[HB_SIZEOFARRAY(lf.lfFaceName) - 1] = TEXT('\0');
    } else {
      HB_STRNCPY(lf.lfFaceName, wvw_top->fontFace, HB_SIZEOFARRAY(lf.lfFaceName) - 1);
    }

    hbwapi_ret_raw_HANDLE(CreateFontIndirect(&lf));
  } else {
    hbwapi_ret_raw_HANDLE(nullptr);
  }
}

HB_FUNC(WVW_SELECTFONT)
{
  CHOOSEFONT cf;
  LOGFONT lf;

  cf.lStructSize = sizeof(cf);
  cf.hwndOwner = nullptr;
  cf.hDC = nullptr;
  cf.lpLogFont = &lf;
  cf.iPointSize = 0;
  cf.Flags = CF_SCREENFONTS | (HB_ISOBJECT(1) ? CF_INITTOLOGFONTSTRUCT : 0);
  cf.rgbColors = RGB(0, 0, 0);
  cf.lCustData = 0;
  cf.lpfnHook = nullptr;
  cf.lpTemplateName = nullptr;
  cf.hInstance = nullptr;
  cf.lpszStyle = nullptr;
  cf.nFontType = SCREEN_FONTTYPE;
  cf.nSizeMin = 0;
  cf.nSizeMax = 0;

  /* Display the CHOOSEFONT common-dialog box. */
  if (ChooseFont(&cf)) {
    auto aMetr = hb_itemArrayNew(9);

    /* Create a logical font based on the user's selection and
       return a handle identifying that font. */
    auto hfont = CreateFontIndirect(cf.lpLogFont);

    hbwapi_arraySet_HANDLE(aMetr, 1, hfont);
    HB_ARRAYSETSTR(aMetr, 2, lf.lfFaceName);
    hb_arraySetNL(aMetr, 3, lf.lfWidth);
    hb_arraySetNL(aMetr, 4, lf.lfHeight);
    hb_arraySetNL(aMetr, 5, lf.lfWeight);
    hb_arraySetNI(aMetr, 6, lf.lfCharSet);
    hb_arraySetNI(aMetr, 7, lf.lfItalic);
    hb_arraySetNI(aMetr, 8, lf.lfUnderline);
    hb_arraySetNI(aMetr, 9, lf.lfStrikeOut);

    hb_itemReturnRelease(aMetr);
  }
}

HB_FUNC(WVW_SETBITMAPRESOURCEID)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto iBitmapType = hb_parni(2);

    if (iBitmapType == 0) {
      TBADDBITMAP tbab;

      tbab.hInst = nullptr;
      tbab.nID = reinterpret_cast<UINT_PTR>(hbwapi_par_raw_HBITMAP(3));

      hb_retni(static_cast<int>(
          SendMessage(wvw_win->hToolBar, TB_ADDBITMAP, static_cast<WPARAM>(1), reinterpret_cast<WPARAM>(&tbab))));
    } else { /* system bitmap */
      int iOffset;

      switch (iBitmapType) {
      case 1:
        iOffset = wvw_win->iStartStdBitmap;
        break;
      case 2:
        iOffset = wvw_win->iStartViewBitmap;
        break;
      case 3:
        iOffset = wvw_win->iStartHistBitmap;
        break;
      default:
        iOffset = 0;
      }

      hb_retnint(static_cast<HB_UINT>(hb_parni(4)) + iOffset);
    }
  } else {
    hb_retni(0);
  }
}

HB_FUNC(WVW_DRAWBITMAP)
{
  HDC hDC = hbwapi_par_raw_HDC(1);
  HDC hDCmem = CreateCompatibleDC(hDC);
  auto dwraster = static_cast<DWORD>(hb_parnldef(3, SRCCOPY));
  HBITMAP hBitmap = hbwapi_par_raw_HBITMAP(2);
  BITMAP bm;
  auto nWidthDest = hb_parni(6);
  auto nHeightDest = hb_parni(7);

  SelectObject(hDCmem, hBitmap);
  GetObject(hBitmap, sizeof(bm), static_cast<LPVOID>(&bm));
  if (nWidthDest && (nWidthDest != bm.bmWidth || nHeightDest != bm.bmHeight)) {
    StretchBlt(hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem, 0, 0, bm.bmWidth, bm.bmHeight, dwraster);
  } else {
    BitBlt(hDC, hb_parni(4), hb_parni(5), bm.bmWidth, bm.bmHeight, hDCmem, 0, 0, dwraster);
  }

  DeleteDC(hDCmem);
}

HB_FUNC(WVW_WINDOW2BITMAP)
{
  HWND hWnd = hbwapi_par_raw_HWND(1);
  bool fFull = hb_parl(2);
  HDC hDC = fFull ? GetWindowDC(hWnd) : GetDC(hWnd);
  HDC hDCmem = CreateCompatibleDC(hDC);
  HBITMAP hBitmap;
  RECT rc;

  if (fFull) {
    GetWindowRect(hWnd, &rc);
  } else {
    GetClientRect(hWnd, &rc);
  }

  hBitmap = CreateCompatibleBitmap(hDC, rc.right - rc.left, rc.bottom - rc.top);
  SelectObject(hDCmem, hBitmap);

  BitBlt(hDCmem, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, 0, 0, SRCCOPY);

  DeleteDC(hDCmem);
  DeleteDC(hDC);

  hbwapi_ret_raw_HANDLE(hBitmap);
}

/*
wvw_SetMaxBMCache([nMax])
Get/Set maximum user-bitmap cache (default is 20, minimum is 1).
Returns old setting of maximum user-bitmap cache.

Description:
To minimize bitmap loading operation, wvw_DrawImage() caches bitmap once
it reads from disk.
Ie., subsequent wvw_DrawImage() will use the bitmap from the memory.
When the maximum number of cache is used, the least recently opened bitmap
will be discarded from the cache.

Remarks:
There is no way to discard a specific bitmap from the cache.
If you want to control bitmap caching manually, use wvw_LoadPicture()
instead.

Example:
wvw_SetMaxBMCache(1)  :: this will cache one bitmap only
wvw_SetMaxBMCache(50) :: allows up to 50 bitmap stored in the cache
*/
HB_FUNC(WVW_SETMAXBMCACHE)
{
  auto wvw = hb_gt_wvw();

  if (wvw) {
    hb_retni(wvw->a.iMaxBMcache);

    if (HB_ISNUM(1)) {
      wvw->a.iMaxBMcache = HB_MAX(hb_parni(1), 0);
    }
  } else {
    hb_retni(0);
  }
}

/*
wvw_NumBMCache()
Returns current number of user-bitmap cache.
*/
HB_FUNC(WVW_NUMBMCACHE)
{
  auto wvw = hb_gt_wvw();

  hb_retni(wvw ? wvw->a.iBMcache : 0);
}

/* Miscellaneous xHarbour callable functions */
/* Budyanto Dj. <budyanto@centrin.net.id> */

/* TIMER */

/*
wvw_SetTimer([nWinNum], nInterval)
set timer event for every nInterval millisecond
(effective only if WVW_TIMER() function exists)
eg. it can be useful to update clock on status bar
returns .T. if successful
*/

/* 2004-06-02: WARNING: WVT is slightly different */
HB_FUNC(WVW_SETTIMER)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw->a.pSymWVW_TIMER && wvw_win) {
    SetTimer(wvw_win->hWnd, WVW_ID_BASE_TIMER + wvw_win->nWinId, hbwapi_par_UINT(2), nullptr);

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*
wvw_KillTimer([nWinNum])
kill the timer event handler for window nWinNum
returns .T. if successful
*/
HB_FUNC(WVW_KILLTIMER) /* 2004-06-02: WARNING: WVT is slightly different */
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw->a.pSymWVW_TIMER && wvw_win) {
    KillTimer(wvw_win->hWnd, WVW_ID_BASE_TIMER + wvw_win->nWinId);

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*
wvw_GetPaintRect(nWinNum)   nWinNum is 0 based
returns array of paint pending rect {top, left, bottom, right}
WARNING:
unlike WVT, top maybe > bottom
            left maybe > right
in these cases, no paint request is pending
(in WVT these is reflected in {0,0,0,0})
*/
HB_FUNC(WVW_GETPAINTRECT)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto info = hb_itemArrayNew(4);
  RECT rc{};

  if (wvw_win) {
    rc = wvw_win->rPaintPending;
  }

  hb_arraySetNL(info, 1, rc.top);
  hb_arraySetNL(info, 2, rc.left);
  hb_arraySetNL(info, 3, rc.bottom);
  hb_arraySetNL(info, 4, rc.right);

  hb_itemReturnRelease(info);
}

HB_FUNC(WVW_SETPOINTER)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    HCURSOR hCursor;

    switch (hb_parni(2)) {
    case 1:
      hCursor = LoadCursor(nullptr, IDC_ARROW);
      break;
    case 2:
      hCursor = LoadCursor(nullptr, IDC_IBEAM);
      break;
    case 3:
      hCursor = LoadCursor(nullptr, IDC_WAIT);
      break;
    case 4:
      hCursor = LoadCursor(nullptr, IDC_CROSS);
      break;
    case 5:
      hCursor = LoadCursor(nullptr, IDC_UPARROW);
      break;
    case 6:
      hCursor = LoadCursor(nullptr, IDC_SIZE);
      break;
    case 7:
      hCursor = LoadCursor(nullptr, IDC_ICON);
      break;
    case 8:
      hCursor = LoadCursor(nullptr, IDC_SIZENWSE);
      break;
    case 9:
      hCursor = LoadCursor(nullptr, IDC_SIZENESW);
      break;
    case 10:
      hCursor = LoadCursor(nullptr, IDC_SIZEWE);
      break;
    case 11:
      hCursor = LoadCursor(nullptr, IDC_SIZENS);
      break;
    case 12:
      hCursor = LoadCursor(nullptr, IDC_SIZEALL);
      break;
    case 13:
      hCursor = LoadCursor(nullptr, IDC_NO);
      break;
    case 14:
      hCursor = LoadCursor(nullptr, IDC_HAND);
      break;
    case 15:
      hCursor = LoadCursor(nullptr, IDC_APPSTARTING);
      break;
    case 16:
      hCursor = LoadCursor(nullptr, IDC_HELP);
      break;
    default:
      hCursor = LoadCursor(nullptr, IDC_ARROW);
    }

    SetClassLongPtr(wvw_win->hWnd, GCLP_HCURSOR, reinterpret_cast<LONG_PTR>(hCursor));
  }
}

/*
wvw_LoadPicture(nSlot, cFilePic)
*/
HB_FUNC(WVW_LOADPICTURE)
{
  auto wvw = hb_gt_wvw();

  int iSlot = hb_parni(1) - 1;
  auto pPicture = hb_gt_wvw_LoadPicture(hb_parcx(2));

  auto fResult = false;

  if (wvw && pPicture && iSlot >= 0 && iSlot < static_cast<int>(HB_SIZEOFARRAY(wvw->a.pPicture))) {
    if (wvw->a.pPicture[iSlot]) {
      hb_gt_wvw_DestroyPicture(wvw->a.pPicture[iSlot]);
    }

    wvw->a.pPicture[iSlot] = pPicture;

    fResult = true;
  }

  hb_retl(fResult);
}

/*
wvw_LoadFont(nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
             nCharSet, nQuality, nEscapement)
*/
HB_FUNC(WVW_LOADFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_top = hb_gt_wvw_win_top();

  int iSlot = hb_parni(1) - 1;

  if (wvw && wvw_top && iSlot >= 0 && iSlot < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hUserFonts))) {
    LOGFONT lf;

    lf.lfEscapement = hb_parnl(11) * 10;
    lf.lfOrientation = 0;
    lf.lfWeight = hb_parnl(5);
    lf.lfItalic = static_cast<BYTE>(hb_parl(6));
    lf.lfUnderline = static_cast<BYTE>(hb_parl(7));
    lf.lfStrikeOut = static_cast<BYTE>(hb_parl(8));
    lf.lfCharSet = static_cast<BYTE>(hb_parnidef(9, wvw_top->CodePage));
    lf.lfOutPrecision = 0;
    lf.lfClipPrecision = 0;
    lf.lfQuality = static_cast<BYTE>(hb_parnidef(10, DEFAULT_QUALITY));
    lf.lfPitchAndFamily = FF_DONTCARE;
    lf.lfHeight = hb_parnldef(3, wvw_top->fontHeight);
    lf.lfWidth = hb_parnldef(4, wvw_top->fontWidth < 0 ? -wvw_top->fontWidth : wvw_top->fontWidth);

    if (HB_ISCHAR(2)) {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), lf.lfFaceName, HB_SIZEOFARRAY(lf.lfFaceName));
      wvw_top->fontFace[HB_SIZEOFARRAY(lf.lfFaceName) - 1] = TEXT('\0');
    } else {
      HB_STRNCPY(lf.lfFaceName, wvw_top->fontFace, HB_SIZEOFARRAY(lf.lfFaceName) - 1);
    }

    auto hFont = CreateFontIndirect(&lf);
    if (hFont) {
      if (wvw->a.hUserFonts[iSlot]) {
        DeleteObject(wvw->a.hUserFonts[iSlot]);
      }

      wvw->a.hUserFonts[iSlot] = hFont;

      hb_retl(true);
      return;
    }
  }

  hb_retl(false);
}

/*
wvw_LoadPen(nSlot, nStyle, nWidth, nRGBColor)
*/
HB_FUNC(WVW_LOADPEN)
{
  auto wvw = hb_gt_wvw();

  int iSlot = hb_parni(1) - 1;

  if (wvw && iSlot >= 0 && iSlot < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hUserPens))) {
    auto hPen = CreatePen(hb_parni(2), hb_parni(3), hbwapi_par_COLORREF(4));

    if (hPen) {
      if (wvw->a.hUserPens[iSlot]) {
        DeleteObject(wvw->a.hUserPens[iSlot]);
      }

      wvw->a.hUserPens[iSlot] = hPen;

      hb_retl(true);
      return;
    }
  }

  hb_retl(false);
}

/* End of drawing primitives */

/* Utility functions. A natural extension copied and modified from GTWVT */

/*
wvw_ChooseFont(cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout)
*/
HB_FUNC(WVW_CHOOSEFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_top = hb_gt_wvw_win_top();

  auto aRet = hb_itemArrayNew(8);

  LOGFONT lf{};
  auto iPointSize = 0;

  if (wvw && wvw_top) {
    CHOOSEFONT cf{};

    if (HB_ISNUM(2)) {
      iPointSize = -MulDiv(hb_parni(2), GetDeviceCaps(wvw_top->hdc, LOGPIXELSY), 72);
    }

    lf.lfHeight = iPointSize;
    lf.lfWidth = hb_parnl(3);
    lf.lfWeight = hb_parnl(4);
    lf.lfItalic = static_cast<BYTE>(hb_parl(6));
    lf.lfUnderline = static_cast<BYTE>(hb_parl(7));
    lf.lfStrikeOut = static_cast<BYTE>(hb_parl(8));
    lf.lfCharSet = DEFAULT_CHARSET;
    lf.lfQuality = static_cast<BYTE>(hb_parnidef(5, DEFAULT_QUALITY));
    lf.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(1)) {
      HB_ITEMCOPYSTR(hb_param(1, Harbour::Item::STRING), lf.lfFaceName, HB_SIZEOFARRAY(lf.lfFaceName));
      lf.lfFaceName[HB_SIZEOFARRAY(lf.lfFaceName) - 1] = TEXT('\0');
    }

    cf.lStructSize = sizeof(cf);
    cf.hwndOwner = wvw_top->hWnd;
    cf.hDC = nullptr;
    cf.lpLogFont = &lf;
    cf.iPointSize = 0;
    cf.Flags = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
    cf.rgbColors = RGB(0, 0, 0);
    cf.lCustData = 0;
    cf.lpfnHook = nullptr;
    cf.lpTemplateName = nullptr;
    cf.hInstance = nullptr;
    cf.lpszStyle = nullptr;
    cf.nFontType = SCREEN_FONTTYPE;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;

    if (ChooseFont(&cf)) {
      iPointSize = -MulDiv(lf.lfHeight, 72, GetDeviceCaps(wvw_top->hdc, LOGPIXELSY));
    } else {
      iPointSize = 0;
      memset(&lf, 0, sizeof(lf));
    }
  }

  HB_ARRAYSETSTR(aRet, 1, lf.lfFaceName);
  hb_arraySetNI(aRet, 2, iPointSize);
  hb_arraySetNL(aRet, 3, lf.lfWidth);
  hb_arraySetNL(aRet, 4, lf.lfWeight);
  hb_arraySetNI(aRet, 5, lf.lfQuality);
  hb_arraySetL(aRet, 6, lf.lfItalic);
  hb_arraySetL(aRet, 7, lf.lfUnderline);
  hb_arraySetL(aRet, 8, lf.lfStrikeOut);

  hb_itemReturnRelease(aRet);
}

/*
wvw_SetMousePos(nWinNum, nRow, nCol) nWinNum is 0 based
What's the difference with GT_FUNC(mouse_SetPos) ???
this func is able to position cursor on any window

NOTE: consider using 'standard' SetMouse() instead:
      SetMouse(.T., nRow, nCol)
      This will treat (nRow,nCol) according to current wvw->fMainCoordMode setting
*/
HB_FUNC(WVW_SETMOUSEPOS)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    POINT xy;

    auto iRow = hb_parni(2);
    auto iCol = hb_parni(3);

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iRow, &iCol, nullptr, nullptr);

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iCol, iRow);

    if (ClientToScreen(wvw_win->hWnd, &xy)) {
      hb_retl(SetCursorPos(xy.x, xy.y + (wvw_win->PTEXTSIZE.y / 2)));
      return;
    }
  }

  hb_retl(false);
}

/*
by bdj
none in GTWVT
   wvw_FillRectangle(nWinNum, nTop, nLeft, nBottom, nRight, nRGBcolor/hBrush,
                     lTight, lUseBrush, aOffSet)

  if lTight, rect is drawn inside the character region
  AND top and left lines are lower two pixel down to make room for above/left object
  WARNING: GUI object of this type subject to be overwritten by chars
  NOTE that these lines are to be overwritten by displayed char,
       we are depending on the fact that GUI object will be painted last

  if lUseBrush, nRGBcolor is treated as a BRUSH handle
*/
HB_FUNC(WVW_FILLRECTANGLE)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();
  auto wvw_zer = hb_gt_wvw_win(0);

  if (wvw && wvw_win) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    int iOffTop = hb_parvni(9, 1);
    int iOffLeft = hb_parvni(9, 2);
    int iOffBottom = hb_parvni(9, 3);
    int iOffRight = hb_parvni(9, 4);

    POINT xy;

    COLORREF crRGBcolor = hbwapi_par_COLORREF(6);
    bool fTight = hb_parl(7);
    bool fUseBrush = hb_parl(8);
    LOGBRUSH lb{};
    HBRUSH hBrush;
    RECT rcXY;

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
    iTop = (fTight ? xy.y + 2 : xy.y) + iOffTop;
    iLeft = (fTight ? xy.x + 2 : xy.x) + iOffLeft;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
    iBottom = xy.y - wvw_win->iLineSpacing - 1 + 1 + iOffBottom;
    iRight = xy.x - 1 + 1 + iOffRight;

    rcXY.left = iLeft;
    rcXY.top = iTop;
    rcXY.right = iRight;
    rcXY.bottom = iBottom;

    lb.lbStyle = BS_SOLID;
    lb.lbColor = crRGBcolor;
    lb.lbHatch = 0;

    hBrush = fUseBrush ? hbwapi_par_HBRUSH(6) : CreateBrushIndirect(&lb);

    FillRect(wvw_win->hdc, &rcXY, hBrush);

    if (!fUseBrush) {
      SelectObject(wvw_zer->hdc, wvw->a.OriginalBrush);
      DeleteObject(hBrush);
    }

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_LBADDSTRING)
{
  void *hText;
  SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), LB_ADDSTRING, 0,
              reinterpret_cast<LPARAM>(HB_PARSTRDEF(3, &hText, nullptr)));
  hb_strfree(hText);
}

HB_FUNC(WVW_LBSETCURSEL)
{
  SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), LB_SETCURSEL, hb_parni(3), 0);
}

/*
WARNING!!! this function is not member of WVW_CB* group of functions
*/
HB_FUNC(WVW_CBADDSTRING)
{
  void *hText;
  SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), CB_ADDSTRING, 0,
              reinterpret_cast<LPARAM>(HB_PARSTRDEF(3, &hText, nullptr)));
  hb_strfree(hText);
}

/*
WARNING!!! this function is not member of WVW_CB* group of functions
*/
HB_FUNC(WVW_CBSETCURSEL)
{
  SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), CB_SETCURSEL, hb_parni(3), 0);
}

HB_FUNC(WVW_DLGSETICON)
{
  HICON hIcon = nullptr;

  if (HB_ISNUM(2)) {
    hIcon = LoadIcon(GetModuleHandle(nullptr), MAKEINTRESOURCE(hb_parni(2)));
  } else {
    void *hName;
    hIcon = static_cast<HICON>(LoadImage(nullptr, HB_PARSTRDEF(2, &hName, nullptr), IMAGE_ICON, 0, 0, LR_LOADFROMFILE));
    hb_strfree(hName);
  }

  if (hIcon) {
    SendMessage(hbwapi_par_raw_HWND(1), WM_SETICON, ICON_SMALL,
                reinterpret_cast<LPARAM>(hIcon));                                               /* Set Title Bar ICON */
    SendMessage(hbwapi_par_raw_HWND(1), WM_SETICON, ICON_BIG, reinterpret_cast<LPARAM>(hIcon)); /* Set Task List Icon */
  }

  hbwapi_ret_raw_HANDLE(hIcon);
}

/* GUI Drawing Functions */
/* Pritpal Bedi <pritpal@vouchcac.com> */

/*
Dialogs
original work by Pritpal Bedi in wvtutils.c
*/
HB_FUNC(WVW_CREATEDIALOGDYNAMIC)
{
  auto wvw = hb_gt_wvw();
  auto wvw_zer = hb_gt_wvw_win(0);

  if (wvw && wvw_zer) {
    int iIndex;

    /* check if we still have room for a new dialog */
    for (iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hDlgModeless)); iIndex++) {
      if (wvw->a.hDlgModeless[iIndex] == nullptr) {
        break;
      }
    }

    if (iIndex < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hDlgModeless))) {
      auto pFirst = hb_param(3, Harbour::Item::ANY);
      PHB_ITEM pFunc = nullptr;
      HWND hDlg = nullptr;
      auto iType = 0;
      auto iResource = hb_parni(4);

      if (pFirst->isEvalItem()) {
        /* pFunc is pointing to stored code block (later) */
        pFunc = hb_itemNew(pFirst);
        iType = 2;
      } else if (pFirst->isString()) {
        auto pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
        if (pExecSym) {
          pFunc = reinterpret_cast<PHB_ITEM>(pExecSym);
        }
        iType = 1;
      }

      if (HB_ISNUM(3)) {
        hDlg = CreateDialogIndirect(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)),
                                    hb_parl(2) ? wvw_zer->hWnd : nullptr, hbwapi_par_raw_DLGPROC(3));
      } else {
        switch (iResource) {
        case 0: {
          void *hText;
          hDlg = CreateDialog(GetModuleHandle(nullptr), HB_PARSTRDEF(1, &hText, nullptr),
                              hb_parl(2) ? wvw_zer->hWnd : nullptr, reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcMLess));
          hb_strfree(hText);
          break;
        }
        case 1:
          hDlg = CreateDialog(GetModuleHandle(nullptr), MAKEINTRESOURCE(hb_parni(1)),
                              hb_parl(2) ? wvw_zer->hWnd : nullptr, reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcMLess));
          break;

        case 2:
          hDlg = CreateDialogIndirect(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)),
                                      hb_parl(2) ? wvw_zer->hWnd : nullptr,
                                      reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcMLess));
          break;
        }
      }

      if (hDlg) {
        wvw->a.hDlgModeless[iIndex] = hDlg;
        if (pFunc) {
          wvw->a.pFunc[iIndex] = pFunc;
          wvw->a.iType[iIndex] = iType;
        } else {
          wvw->a.pFunc[iIndex] = nullptr;
          wvw->a.iType[iIndex] = 0;
        }
        SendMessage(hDlg, WM_INITDIALOG, 0, 0);
      } else {
        if (iType == 2 && pFunc) {
          hb_itemRelease(pFunc);
        }

        wvw->a.hDlgModeless[iIndex] = nullptr;
      }

      hbwapi_ret_raw_HANDLE(hDlg);
      return;
    }
  }

  hbwapi_ret_raw_HANDLE(nullptr);
}

HB_FUNC(WVW_CREATEDIALOGMODAL)
{
  auto wvw = hb_gt_wvw();
  auto wvw_zer = hb_gt_wvw_win(0);

  if (wvw && wvw_zer) {
    int iIndex;

    /* check if we still have room for a new dialog */
    for (iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hDlgModal)); iIndex++) {
      if (wvw->a.hDlgModal[iIndex] == nullptr) {
        break;
      }
    }

    if (iIndex < static_cast<int>(HB_SIZEOFARRAY(wvw->a.hDlgModal))) {
      auto pFirst = hb_param(3, Harbour::Item::ANY);
      auto iResource = hb_parni(4);
      INT_PTR iResult = 0;
      HWND hParent = hbwapi_is_HANDLE(5) ? hbwapi_par_raw_HWND(5) : wvw_zer->hWnd;

      if (pFirst->isEvalItem()) {
        wvw->a.pFuncModal[iIndex] = hb_itemNew(pFirst);
        wvw->a.iTypeModal[iIndex] = 2;
      } else if (pFirst->isString()) {
        auto pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
        wvw->a.pFuncModal[iIndex] = pExecSym ? reinterpret_cast<PHB_ITEM>(pExecSym) : nullptr;
        wvw->a.iTypeModal[iIndex] = 1;
      }

      switch (iResource) {
      case 0: {
        void *hText;
        iResult = DialogBoxParam(GetModuleHandle(nullptr), HB_PARSTRDEF(1, &hText, nullptr), hParent,
                                 reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcModal),
                                 static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
        hb_strfree(hText);
        break;
      }
      case 1:
        iResult = DialogBoxParam(GetModuleHandle(nullptr), MAKEINTRESOURCE(hb_parni(1)), hParent,
                                 reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcModal),
                                 static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
        break;

      case 2:
        iResult = DialogBoxIndirectParam(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)),
                                         hParent, reinterpret_cast<DLGPROC>(hb_gt_wvw_DlgProcModal),
                                         static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
        break;
      }

      hb_retnint(iResult);
      return;
    }
  }

  hb_retnint(0);
}

/*
aScr := wvw_SaveScreen(nWinNum, nTop, nLeft, nBottom, nRight)
*/

/*
TODO: reconsider, is it really needed? is it better to be handled by application?
     besides, with Windowing feature, it seems not needed anymore
*/
HB_FUNC(WVW_SAVESCREEN)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    HBITMAP hBmp;
    POINT xy;
    int iWidth, iHeight;
    auto info = hb_itemArrayNew(3);

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
    iTop = xy.y;
    iLeft = xy.x;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
    iBottom = xy.y - 1;
    iRight = xy.x - 1;

    iWidth = iRight - iLeft + 1;
    iHeight = iBottom - iTop + 1;

    hBmp = CreateCompatibleBitmap(wvw_win->hdc, iWidth, iHeight);

    auto oldBmp = static_cast<HBITMAP>(SelectObject(wvw_win->hCompDC, hBmp));
    BitBlt(wvw_win->hCompDC, 0, 0, iWidth, iHeight, wvw_win->hdc, iLeft, iTop, SRCCOPY);
    SelectObject(wvw_win->hCompDC, oldBmp);

    hb_arraySetNI(info, 1, iWidth);
    hb_arraySetNI(info, 2, iHeight);
    hbwapi_arraySet_HANDLE(info, 3, hBmp);

    hb_itemReturnRelease(info);
  }
}

/*
wvw_RestScreen(nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP)

TODO: reconsider, is it really needed? is it better to be handled by application?
      besides, with Windowing feature, it seems not needed anymore
*/
HB_FUNC(WVW_RESTSCREEN)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    POINT xy;
    int iWidth, iHeight;

    auto fResult = false;

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
    iTop = xy.y;
    iLeft = xy.x;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
    iBottom = xy.y - 1;
    iRight = xy.x - 1;

    iWidth = iRight - iLeft + 1;
    iHeight = iBottom - iTop + 1;

    auto hBmp =
        static_cast<HBITMAP>(SelectObject(wvw_win->hCompDC, static_cast<HBITMAP>(hbwapi_parv_raw_HANDLE(6, 3))));
    if (hBmp) {
      if (iWidth == hb_parvni(6, 1) && iHeight == hb_parvni(6, 2)) {
        if (BitBlt(wvw_win->hdc, iLeft, iTop, iWidth, iHeight, wvw_win->hCompDC, 0, 0, SRCCOPY)) {
          fResult = true;
        }
      } else if (StretchBlt(wvw_win->hdc, iLeft, iTop, iWidth, iHeight, wvw_win->hCompDC, 0, 0, hb_parvni(6, 1),
                            hb_parvni(6, 2), SRCCOPY)) {
        fResult = true;
      }

      SelectObject(wvw_win->hCompDC, hBmp);

      if (!hb_parl(7) /* fDoNotDestroyBMP */) {
        DeleteObject(static_cast<HBITMAP>(hbwapi_parv_raw_HANDLE(6, 3)));
      }
    }

    hb_retl(fResult);
  } else {
    hb_retl(false);
  }
}

/* Pritpal Bedi <pritpal@vouchcac.com> */

HB_FUNC(WVW_SETFONT)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto fResult = false;

  if (wvw_win) {
    void *hFontFace = nullptr;
    LPCTSTR fontFace = HB_ISCHAR(2) ? HB_PARSTR(2, &hFontFace, nullptr) : wvw_win->fontFace;
    auto height = hb_parnidef(3, wvw_win->fontHeight);
    auto width = hb_parnidef(4, wvw_win->fontWidth);
    auto Bold = hb_parnidef(5, wvw_win->fontWeight);
    auto Quality = hb_parnidef(6, wvw_win->fontQuality);

    auto hFont = hb_gt_wvw_GetFont(fontFace, height, width, Bold, Quality, wvw_win->CodePage);

    /* make sure the font could actually be created */
    if (hFont) {
      /* make sure that the font  will fit inside the
       * window with the current wvw_win->ROWS and wvw_win->COLS setting
       *
       * JC1: There's definitely something WRONG with this way of thinking.
       * This makes effectively impossible to enlarge the window from it's
       * initial size.
       *
       * x with the above remark, GTWVT comments out the following condition:
       * x TODO: I THINK I am I to keep it, am I?
       */

      if (hb_gt_wvw_ValidWindowSize(wvw_win, wvw_win->ROWS, wvw_win->COLS, hFont, width, nullptr, nullptr)) {
        size_t size;

        wvw_win->fontHeight = height;
        wvw_win->fontWidth = width;
        wvw_win->fontWeight = Bold;
        wvw_win->fontQuality = Quality;

        size = HB_STRLEN(fontFace);
        if (size > 0 && (size < HB_SIZEOFARRAY(wvw_win->fontFace) - 1)) {
          HB_STRNCPY(wvw_win->fontFace, fontFace, HB_SIZEOFARRAY(wvw_win->fontFace) - 1);
        }

        if (wvw_win->hWnd) {
          /* resize the window based on new fonts */
          hb_gt_wvw_ResetWindowSize(wvw_win, wvw_win->hWnd);

          /* force resize of caret */
          hb_gt_wvw_KillCaret(wvw_win);
          hb_gt_wvw_CreateCaret(wvw_win);
        }
        fResult = true;
      }
      DeleteObject(hFont);
    }

    hb_strfree(hFontFace);
  }

  hb_retl(fResult);
}

HB_FUNC(WVW_SETICON)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    void *hName;

    if (HB_ISNUM(2) && HB_ISCHAR(3)) {
      hbwapi_ret_raw_HANDLE(hb_gt_wvw_SetWindowIcon(wvw_win, hb_parni(2), HB_PARSTRDEF(3, &hName, nullptr)));
    } else {
      hbwapi_ret_raw_HANDLE(hb_gt_wvw_SetWindowIconFromFile(wvw_win, HB_PARSTRDEF(2, &hName, nullptr)));
    }

    hb_strfree(hName);
  } else {
    hbwapi_ret_raw_HANDLE(nullptr);
  }
}

HB_FUNC(WVW_GETWINDOWHANDLE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  hbwapi_ret_raw_HANDLE(wvw_win ? wvw_win->hWnd : nullptr);
}

HB_FUNC(WVW_GETCTRLHANDLE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto iStyle = 0;

  hbwapi_ret_raw_HWND(hb_gt_wvw_FindControlHandle(wvw_win, hb_parni(2), hb_parni(3), &iStyle));

  hb_storni(iStyle, 4);
}

HB_FUNC(WVW_SETCODEPAGE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retni(hb_gt_wvw_SetCodePage(wvw_win, hb_parni(2)));
  } else {
    hb_retni(0);
  }
}

/*
wvw_CenterWindow(nWinNum, lCenter, lPaint)  (nWinNum==0==MAIN)
*/
HB_FUNC(WVW_CENTERWINDOW)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retl(wvw_win->CentreWindow);

    wvw_win->CentreWindow = hb_parldef(2, true);

    if (hb_parl(3) /* fPaint */) {
      ShowWindow(wvw_win->hWnd, IsZoomed(wvw_win->hWnd) ? SW_MAXIMIZE : SW_RESTORE);

      hb_gt_wvw_ResetWindowSize(wvw_win, wvw_win->hWnd);
    }
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_SETMOUSEMOVE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retl(wvw_win->MouseMove);

    if (HB_ISLOG(2)) {
      wvw_win->MouseMove = hb_parl(2);
    }
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_GETXYFROMROWCOL)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto aRet = hb_itemArrayNew(2);
  POINT xy{};

  if (wvw_win) {
    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, hb_parni(3), hb_parni(2));
  }

  hb_arraySetNL(aRet, 1, xy.x);
  hb_arraySetNL(aRet, 2, xy.y);

  hb_itemReturnRelease(aRet);
}

/*
wvw_GetRowColFromXY([nWinNum], nX, nY)
return an array {nRow, nCol}
*/
HB_FUNC(WVW_GETROWCOLFROMXY)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto aRet = hb_itemArrayNew(2);
  POINT xy{};

  if (wvw_win) {
    xy = hb_gt_wvw_GetColRowFromXY(wvw_win, hb_parni(2), hb_parni(3));
  }

  hb_arraySetNL(aRet, 1, xy.y);
  hb_arraySetNL(aRet, 2, xy.x);

  hb_itemReturnRelease(aRet);
}

HB_FUNC(WVW_GETFONTINFO)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto aRet = hb_itemArrayNew(7);

  if (wvw_win) {
    HB_ARRAYSETSTR(aRet, 1, wvw_win->fontFace);
    hb_arraySetNL(aRet, 2, wvw_win->fontHeight);
    hb_arraySetNL(aRet, 3, wvw_win->fontWidth);
    hb_arraySetNL(aRet, 4, wvw_win->fontWeight);
    hb_arraySetNI(aRet, 5, wvw_win->fontQuality);
    hb_arraySetNL(aRet, 6, wvw_win->PTEXTSIZE.y);
    hb_arraySetNL(aRet, 7, wvw_win->PTEXTSIZE.x);
  } else {
    hb_arraySetC(aRet, 1, nullptr);
    hb_arraySetNL(aRet, 2, 0);
    hb_arraySetNL(aRet, 3, 0);
    hb_arraySetNL(aRet, 4, 0);
    hb_arraySetNI(aRet, 5, 0);
    hb_arraySetNL(aRet, 6, 0);
    hb_arraySetNL(aRet, 7, 0);
  }

  hb_itemReturnRelease(aRet);
}

/*
wvw_Maximize([nWinNum])
maximizes the window, if callback function WVW_SIZE() exists

note: in GTWVT wvt_Maximize() restores the window, not maximizes it
see also: wvw_Restore(), wvw_MaxMaxRow(), wvw_MaxMaxCol()
*/
HB_FUNC(WVW_MAXIMIZE)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win) {
    if (wvw->a.pSymWVW_SIZE) {
      ShowWindow(wvw_win->hWnd, SW_MAXIMIZE); /* app seems to be ready to handle the maximized window */
    } else {
      ShowWindow(wvw_win->hWnd, SW_RESTORE); /* the old, default behaviour as in GTWVT */
    }
  }
}

/*
wvw_Restore([nWinNum])
restores the window (similar with GTWVT's wvt_Maximize())

WARNING: restoring window from its maximized state might need handling
         in callback function WVW_SIZE(),
         because this function assumes no change in MaxRow()/MaxCol()
see also: wvw_Maximize(), wvw_MaxMaxRow(), wvw_MaxMaxCol()
*/
HB_FUNC(WVW_RESTORE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    ShowWindow(wvw_win->hWnd, SW_RESTORE);
  }
}
