//
// Video subsystem for Windows using GUI windows instead of Console
//
//    Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
// based on:
//
//    Copyright 2003 Peter Rees <peter@rees.co.nz>
//                    Rees Software & Systems Ltd
// based on
//   Bcc ConIO Video subsystem by
//     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
//     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
//   Video subsystem for Windows compilers
//     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
//     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
//

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

// Direct WinApi Functions - Prefixed wvg_*()

#if defined(__BORLANDC__)
#if !defined(NONAMELESSUNION)
#define NONAMELESSUNION
#endif
#if defined(DUMMYUNIONNAME)
#undef DUMMYUNIONNAME
#endif
#if defined(DUMMYUNIONNAME2)
#undef DUMMYUNIONNAME2
#endif
#if defined(DUMMYUNIONNAME3)
#undef DUMMYUNIONNAME3
#endif
#if defined(DUMMYUNIONNAME4)
#undef DUMMYUNIONNAME4
#endif
#if defined(DUMMYUNIONNAME5)
#undef DUMMYUNIONNAME5
#endif
#endif

#include "gtwvg.hpp"
#include "hbwapi.hpp"

#include <windowsx.h>

#if defined(__BORLANDC__) && __BORLANDC__ >= 0x0610
#undef NONAMELESSUNION
#endif

#if defined(NONAMELESSUNION)
#define HB_WIN_V_UNION(x, z) ((x).DUMMYUNIONNAME.z)
#else
#define HB_WIN_V_UNION(x, z) ((x).z)
#endif

#if !defined(GCLP_HBRBACKGROUND)
#define GCLP_HBRBACKGROUND -10
#endif

#define WIN_STATUSBAR_MAX_PARTS 256

#define wvg_parwparam(n) ((WPARAM)(HB_PTRUINT)hb_parnint(n))
#define wvg_parlparam(n) ((LPARAM)(HB_PTRUINT)hb_parnint(n))
#define wvg_parhandle(n) ((HANDLE)(HB_PTRUINT)hb_parnint(n))
#define wvg_parhwnd(n) ((HWND)(HB_PTRUINT)hb_parnint(n))
#define wvg_parwndproc(n) ((WNDPROC)(HB_PTRUINT)hb_parnint(n))
#define wvg_parhbrush(n) ((HBRUSH)(HB_PTRUINT)hb_parnint(n))
#define wvg_parhdc(n) ((HDC)(HB_PTRUINT)hb_parnint(n))
#define wvg_parcolor(n) ((COLORREF)(HB_PTRUINT)hb_parnint(n))

#define wvg_rethandle(n) (hb_retnint((HB_PTRUINT)n))

#if defined(__BORLANDC__) && !defined(HB_ARCH_64BIT)
#undef MAKELONG
#define MAKELONG(a, b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

static HINSTANCE wvg_hInstance(void)
{
  HANDLE hInstance;

  hb_winmainArgGet(&hInstance, nullptr, nullptr);

  return (HINSTANCE)hInstance;
}

HB_FUNC(WVG_HINSTANCE)
{
  hbwapi_ret_raw_HANDLE(wvg_hInstance());
}

//
//              Bitmap Management Function . Coutesy GTWVW
//

static BITMAPINFO *PackedDibLoad(LPCTSTR szFileName)
{
  BITMAPFILEHEADER bmfh;
  BITMAPINFO *pbmi;
  HB_BOOL bSuccess;
  DWORD dwPackedDibSize, dwBytesRead;
  HANDLE hFile;

  hFile = CreateFile(szFileName, GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, nullptr);

  if (hFile == INVALID_HANDLE_VALUE)
  {
    return nullptr;
  }
  
  bSuccess = ReadFile(hFile, &bmfh, sizeof(BITMAPFILEHEADER), &dwBytesRead, nullptr);

  if (!bSuccess || (dwBytesRead != sizeof(BITMAPFILEHEADER)) || (bmfh.bfType != *(WORD *)"BM"))
  {
    CloseHandle(hFile);
    return nullptr;
  }

  dwPackedDibSize = bmfh.bfSize - sizeof(BITMAPFILEHEADER);

  pbmi = (BITMAPINFO *)hb_xgrab(dwPackedDibSize);

  bSuccess = ReadFile(hFile, pbmi, dwPackedDibSize, &dwBytesRead, nullptr);
  CloseHandle(hFile);

  if (!bSuccess || (dwBytesRead != dwPackedDibSize))
  {
    hb_xfree(pbmi);
    return nullptr;
  }

  return pbmi;
}

static int PackedDibGetWidth(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcWidth;
  }
  else
  {
    return pPackedDib->bmiHeader.biWidth;
  }  
}

static int PackedDibGetHeight(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcHeight;
  }
  else
  {
    return abs(pPackedDib->bmiHeader.biHeight);
  }  
}

static int PackedDibGetBitCount(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcBitCount;
  }
  else
  {
    return pPackedDib->bmiHeader.biBitCount;
  }  
}

static int PackedDibGetInfoHeaderSize(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcSize;
  }
  else if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPINFOHEADER))
  {
    return pPackedDib->bmiHeader.biSize + (pPackedDib->bmiHeader.biCompression == BI_BITFIELDS ? 12 : 0);
  }
  else
  {
    return pPackedDib->bmiHeader.biSize;
  }  
}

static int PackedDibGetColorsUsed(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return 0;
  }
  else
  {
    return pPackedDib->bmiHeader.biClrUsed;
  }  
}

static int PackedDibGetNumColors(BITMAPINFO *pPackedDib)
{
  int iNumColors;

  iNumColors = PackedDibGetColorsUsed(pPackedDib);

  if (iNumColors == 0 && PackedDibGetBitCount(pPackedDib) < 16)
  {
    iNumColors = 1 << PackedDibGetBitCount(pPackedDib);
  }
  
  return iNumColors;
}

static int PackedDibGetColorTableSize(BITMAPINFO *pPackedDib)
{
  if (pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER))
  {
    return PackedDibGetNumColors(pPackedDib) * sizeof(RGBTRIPLE);
  }
  else
  {
    return PackedDibGetNumColors(pPackedDib) * sizeof(RGBQUAD);
  }  
}

static BYTE *PackedDibGetBitsPtr(BITMAPINFO *pPackedDib)
{
  return ((BYTE *)pPackedDib) + PackedDibGetInfoHeaderSize(pPackedDib) + PackedDibGetColorTableSize(pPackedDib);
}

static HBITMAP hPrepareBitmap(LPCTSTR szBitmap, UINT uiBitmap, int iExpWidth, int iExpHeight, HB_BOOL bMap3Dcolors,
                              HWND hCtrl, int iMode)
{
  HBITMAP hBitmap = nullptr;

  switch (iMode)
  {
  case 0:

    if (szBitmap)
    {
      int iWidth, iHeight;
      {
        BITMAPINFO *pPackedDib = nullptr;
        HDC hdc;

        if (!bMap3Dcolors)
        {
          pPackedDib = PackedDibLoad(szBitmap);
        }
        
        if (pPackedDib || bMap3Dcolors)
        {
          hdc = GetDC(hCtrl);

          if (!bMap3Dcolors)
          {
            hBitmap = CreateDIBitmap(hdc, (PBITMAPINFOHEADER)pPackedDib, CBM_INIT, PackedDibGetBitsPtr(pPackedDib),
                                     pPackedDib, DIB_RGB_COLORS);
            if (hBitmap == nullptr)
            {
              return nullptr;
            }
            
            iWidth = PackedDibGetWidth(pPackedDib);
            iHeight = PackedDibGetHeight(pPackedDib);
          }
          else
          {
            hBitmap = (HBITMAP)LoadImage((HINSTANCE)nullptr, szBitmap, IMAGE_BITMAP, iExpWidth, iExpHeight,
                                         LR_LOADFROMFILE | LR_LOADMAP3DCOLORS);
            if (hBitmap == nullptr)
            {
              return nullptr;
            }
            
            iWidth = iExpWidth;
            iHeight = iExpHeight;
          }

          if (iExpWidth == 0 && iExpHeight == 0)
          {
            iWidth = iExpWidth;
            iHeight = iExpHeight;
          }

          if (iExpWidth != iWidth || iExpHeight != iHeight)
          {
            HDC hdcSource, hdcTarget;
            HBITMAP hBitmap2;
            HB_BOOL bResult;

            hdcSource = CreateCompatibleDC(hdc);
            SelectObject(hdcSource, hBitmap);

            hdcTarget = CreateCompatibleDC(hdc);
            hBitmap2 = CreateCompatibleBitmap(hdcSource, iExpWidth, iExpHeight);
            SelectObject(hdcTarget, hBitmap2);

            bResult = StretchBlt(hdcTarget,  // handle to destination DC
                                 0,          // x-coord of destination upper-left corner
                                 0,          // y-coord of destination upper-left corner
                                 iExpWidth,  // width of destination rectangle
                                 iExpHeight, // height of destination rectangle
                                 hdcSource,  // handle to source DC
                                 0,          // x-coord of source upper-left corner
                                 0,          // y-coord of source upper-left corner
                                 iWidth,     // width of source rectangle
                                 iHeight,    // height of source rectangle
                                 SRCCOPY     // raster operation code
            );

            if (!bResult)
            {
              DeleteObject(hBitmap2);
            }
            else
            {
              DeleteObject(hBitmap);
              hBitmap = hBitmap2;
            }

            DeleteDC(hdcSource);
            DeleteDC(hdcTarget);
          }

          ReleaseDC(hCtrl, hdc);
          if (pPackedDib)
          {
            hb_xfree(pPackedDib);
          }  
        }
      }
    }
    break;
  case 1:
  {
    UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;

    hBitmap = (HBITMAP)LoadImage((HINSTANCE)wvg_hInstance(), szBitmap, IMAGE_BITMAP, iExpWidth, iExpHeight, uiOptions);

    if (hBitmap == nullptr)
    {
      return nullptr;
    }  
  }
  break;
  case 2: // loading from resourceid
  {
    UINT uiOptions = bMap3Dcolors ? LR_LOADMAP3DCOLORS : LR_DEFAULTCOLOR;
    char szResname[MAX_PATH + 1];

    hb_snprintf(szResname, sizeof(szResname), "?%u", uiBitmap);

    hBitmap = (HBITMAP)LoadImage((HINSTANCE)wvg_hInstance(), (LPCTSTR)MAKEINTRESOURCE((WORD)uiBitmap), IMAGE_BITMAP,
                                 iExpWidth, iExpHeight, uiOptions);
    if (hBitmap == nullptr)
    {
      return nullptr;
    }  

  } // loading from resources
  break;
  }

  return hBitmap;
}

HB_FUNC(WVG_PREPAREBITMAPFROMFILE)
{
  HBITMAP hBitmap;
  void *hText;

  hBitmap = hPrepareBitmap(HB_PARSTR(1, &hText, nullptr), 0, hb_parni(2), hb_parni(3), hb_parl(4),
                           (HWND)(HB_PTRUINT)hb_parnint(5), 0);
  hb_strfree(hText);
  hb_retptr((void *)hBitmap);
}

HB_FUNC(WVG_PREPAREBITMAPFROMRESOURCEID)
{
  HBITMAP hBitmap;

  hBitmap = hPrepareBitmap(nullptr, hb_parni(1), hb_parni(2), hb_parni(3), hb_parl(4), (HWND)(HB_PTRUINT)hb_parnint(5), 2);

  hb_retptr((void *)hBitmap);
}

HB_FUNC(WVG_PREPAREBITMAPFROMRESOURCENAME)
{
  HBITMAP hBitmap;
  void *hText;

  hBitmap = hPrepareBitmap(HB_PARSTR(1, &hText, nullptr), 0, hb_parni(2), hb_parni(3), hb_parl(4),
                           (HWND)(HB_PTRUINT)hb_parnint(5), 1);
  hb_strfree(hText);
  hb_retptr((void *)hBitmap);
}

HB_FUNC(WVG_STATUSBARCREATEPANEL)
{
  HWND hWndSB = (HWND)(HB_PTRUINT)hb_parnint(1);
  int iMode = hb_parni(2);

  if (hWndSB == nullptr || !IsWindow(hWndSB))
  {
    hb_retl(false);
    return;
  }

  switch (iMode)
  {
  case 0:
  {
    int ptArray[WIN_STATUSBAR_MAX_PARTS];
    int iParts;
    RECT rc = {0, 0, 0, 0};
    int n;
    int width;

    iParts = (int)SendMessage(hWndSB, SB_GETPARTS, (WPARAM)WIN_STATUSBAR_MAX_PARTS, (LPARAM)(LPINT)ptArray);

    GetClientRect(hWndSB, &rc);
    width = (int)(rc.right / (iParts + 1));
    for (n = 0; n < iParts; n++)
    {
      ptArray[n] = (width * (n + 1));
    }

    ptArray[iParts] = -1;

    if (SendMessage(hWndSB, SB_SETPARTS, (WPARAM)iParts + 1, (LPARAM)(LPINT)ptArray))
    {
      hb_retl(true);
      return;
    }
    break;
  }
  case -1:
  {
    RECT rc = {0, 0, 0, 0};
    int ptArray[WIN_STATUSBAR_MAX_PARTS];

    if (GetClientRect(hWndSB, &rc))
    {
      ptArray[0] = rc.right;

      SendMessage(hWndSB, SB_SETPARTS, (WPARAM)1, (LPARAM)(LPINT)ptArray);

      hb_retl(true);
      return;
    }
  }
  }

  hb_retl(false);
}

HB_FUNC(WVG_STATUSBARSETTEXT)
{
  HWND hWndSB = (HWND)(HB_PTRUINT)hb_parnint(1);

  if (hWndSB && IsWindow(hWndSB))
  {
    int iPart = hb_parnidef(2, 1);
    TCHAR szText[1024];
    int iFlags;
    void *hCaption;

    iPart -= 1; // Zero based

    iFlags = (int)HIWORD(SendMessage(hWndSB, SB_GETTEXT, (WPARAM)iPart, (LPARAM)szText));

    SendMessage(hWndSB, SB_SETTEXT, (WPARAM)iPart | iFlags, (LPARAM)HB_PARSTR(3, &hCaption, nullptr));

    hb_strfree(hCaption);
  }
}

HB_FUNC(WVG_STATUSBARREFRESH)
{
#if 0
   HWND hWndSB = (HWND)(HB_PTRUINT)hb_parnint(1);

   if (hWndSB && IsWindow(hWndSB))
   {
      int ptArray[WIN_STATUSBAR_MAX_PARTS];
      int iParts, i;

      iParts = SendMessage(hWndSB, SB_GETPARTS, WIN_STATUSBAR_MAX_PARTS, (LPARAM)(LPINT)ptArray);

      ptArray[iParts - 1] = -1;

      if (SendMessage(hWndSB, SB_SETPARTS, iParts, (LPARAM)(LPINT)ptArray))
      {
         hb_retl(true);
         return;
      }
   }
   hb_retl(false);
#endif
}

//
// Wvg_GetNMHInfo(nlParam)
//
HB_FUNC(WVG_GETNMHDRINFO)
{
  LPNMHDR lpnmh = (LPNMHDR)wvg_parlparam(1);
  auto pEvParams = hb_itemNew(nullptr);

  hb_arrayNew(pEvParams, 3);

  hb_arraySetNI(pEvParams, 1, lpnmh->code);
  hb_arraySetNInt(pEvParams, 2, (HB_PTRUINT)lpnmh->idFrom);
  hb_arraySetNInt(pEvParams, 3, (HB_PTRUINT)lpnmh->hwndFrom);

  hb_itemReturnRelease(pEvParams);
}

//
// Wvg_GetNMMouseInfo(nlParam)
//
HB_FUNC(WVG_GETNMMOUSEINFO)
{
  LPNMMOUSE nmm = (LPNMMOUSE)wvg_parlparam(1);
  NMHDR nmh = nmm->hdr;
  auto pEvParams = hb_itemNew(nullptr);

  hb_arrayNew(pEvParams, 4);

  hb_arraySetNI(pEvParams, 1, nmh.code);
  hb_arraySetNInt(pEvParams, 2, (HB_PTRUINT)nmh.idFrom);
  hb_arraySetNInt(pEvParams, 3, (HB_PTRUINT)nmh.hwndFrom);
  hb_arraySetNInt(pEvParams, 4, (HB_PTRUINT)nmm->dwItemSpec);

  hb_itemReturnRelease(pEvParams);
}

//
//  Wvg_GetNMTreeViewInfo(nlParam)
//
HB_FUNC(WVG_GETNMTREEVIEWINFO)
{
  LPNMTREEVIEW pnmtv = (LPNMTREEVIEW)wvg_parlparam(1);
  NMHDR nmh = pnmtv->hdr;

  auto pEvParams = hb_itemNew(nullptr);

  hb_arrayNew(pEvParams, 4);

  hb_arraySetNI(pEvParams, 1, nmh.code);
  hb_arraySetNInt(pEvParams, 2, (HB_PTRUINT)nmh.idFrom);
  hb_arraySetNInt(pEvParams, 3, (HB_PTRUINT)nmh.hwndFrom);
  hb_arraySetNI(pEvParams, 4, pnmtv->action);

  hb_itemReturnRelease(pEvParams);
}

//
//  Wvg_TreeView_GetSelectionInfo(::hWnd, nlParam, @cParent, @cText, @hParentOfSelected, @hItemSelected)
//
HB_FUNC(WVG_TREEVIEW_GETSELECTIONINFO)
{
  LPNMTREEVIEW pnmtv = (LPNMTREEVIEW)wvg_parlparam(2);
  HTREEITEM hSelected = pnmtv->itemNew.hItem;

  if (hSelected != nullptr)
  {
    TCHAR text[MAX_PATH + 1];
    TCHAR Parent[MAX_PATH + 1];
    TV_ITEM item;
    HTREEITEM hParent;

    hb_stornint((HB_PTRUINT)hSelected, 6);

    item.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_IMAGE;
    item.hItem = hSelected;
    item.pszText = text;
    item.cchTextMax = MAX_PATH;

    if (TreeView_GetItem(wvg_parhwnd(1), &item))
    {
      HB_STORSTR(text, 4);
    }

    hParent = TreeView_GetParent(wvg_parhwnd(1), hSelected);
    hb_stornint((HB_PTRUINT)hParent, 5);

    item.mask = TVIF_HANDLE | TVIF_TEXT;
    item.hItem = hParent;
    item.pszText = Parent;
    item.cchTextMax = MAX_PATH;

    if (TreeView_GetItem(wvg_parhwnd(1), &item))
    {
      HB_STORSTR(Parent, 3);
    }  
  }
}

//
//   hItem := Wvg_TreeView_AddItem(oItem:hTree, hParent, oItem:Caption)
//
HB_FUNC(WVG_TREEVIEW_ADDITEM)
{
  TVINSERTSTRUCT tvis;
  void *hText;

  tvis.hInsertAfter = TVI_LAST;
  HB_WIN_V_UNION(tvis, item.mask) = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_STATE;
  HB_WIN_V_UNION(tvis, item.cchTextMax) = MAX_PATH + 1;
  HB_WIN_V_UNION(tvis, item.stateMask) = TVIS_BOLD | TVIS_CUT | TVIS_DROPHILITED | TVIS_EXPANDEDONCE | TVIS_SELECTED |
                                         TVIS_EXPANDPARTIAL | TVIS_OVERLAYMASK | TVIS_STATEIMAGEMASK | TVIS_USERMASK;

  HB_WIN_V_UNION(tvis, item.state) = 0; // TVI_BOLD
  tvis.hParent = HB_ISNUM(2) ? (HTREEITEM)wvg_parhandle(2) : nullptr;
  HB_WIN_V_UNION(tvis, item.pszText) = (LPTSTR)HB_PARSTRDEF(3, &hText, nullptr);

  hb_retnint((HB_PTRUINT)TreeView_InsertItem(wvg_parhwnd(1), &tvis));

  hb_strfree(hText);
}

HB_FUNC(WVG_TREEVIEW_SHOWEXPANDED)
{
  HWND hwnd = wvg_parhwnd(1);
  HTREEITEM hroot, hitem, hitem1, hitem2, hitem3;
  int iExpand = (hb_parl(2) ? TVE_EXPAND : TVE_COLLAPSE);
  int iLevels = hb_parni(3) <= 0 ? 5 : hb_parni(3);

  hroot = TreeView_GetRoot(hwnd);
  if (hroot)
  {
    (void)TreeView_Expand(hwnd, hroot, iExpand);
    if (iLevels >= 2)
    {
      hitem = TreeView_GetNextItem(hwnd, hroot, TVGN_CHILD);
      while (hitem)
      {
        (void)TreeView_Expand(hwnd, hitem, iExpand);
        if (iLevels >= 3)
        {
          hitem1 = TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD);
          while (hitem1)
          {
            (void)TreeView_Expand(hwnd, hitem1, iExpand);
            if (iLevels >= 4)
            {
              hitem2 = TreeView_GetNextItem(hwnd, hitem1, TVGN_CHILD);
              while (hitem2)
              {
                (void)TreeView_Expand(hwnd, hitem2, iExpand);
                if (iLevels >= 5)
                {
                  hitem3 = TreeView_GetNextItem(hwnd, hitem2, TVGN_CHILD);
                  while (hitem3)
                  {
                    (void)TreeView_Expand(hwnd, hitem3, iExpand);
                    hitem3 = TreeView_GetNextItem(hwnd, hitem3, TVGN_NEXT);
                  }
                }
                hitem2 = TreeView_GetNextItem(hwnd, hitem2, TVGN_NEXT);
              }
            }
            hitem1 = TreeView_GetNextItem(hwnd, hitem1, TVGN_NEXT);
          }
        }
        hitem = TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT);
      }
    }
  }
}

//                            WvgFontDialog()

PHB_ITEM wvg_logfontTOarray(LPLOGFONT lf, HB_BOOL bEmpty)
{
  auto aFont = hb_itemNew(nullptr);

  hb_arrayNew(aFont, 15);

  if (bEmpty)
  {
    hb_arraySetC(aFont, 1, nullptr);
    hb_arraySetNL(aFont, 2, 0);
    hb_arraySetNL(aFont, 3, 0);
    hb_arraySetNL(aFont, 4, 0);
    hb_arraySetL(aFont, 5, 0);
    hb_arraySetL(aFont, 6, 0);
    hb_arraySetL(aFont, 7, 0);
    hb_arraySetNI(aFont, 8, 0);
    hb_arraySetNI(aFont, 9, 0);
    hb_arraySetNI(aFont, 10, 0);
    hb_arraySetNI(aFont, 11, 0);
    hb_arraySetNI(aFont, 12, 0);
    hb_arraySetNI(aFont, 13, 0);
    hb_arraySetNI(aFont, 14, 0);
    hb_arraySetNInt(aFont, 15, 0);
  }
  else
  {
    HB_ARRAYSETSTR(aFont, 1, lf->lfFaceName);
    hb_arraySetNL(aFont, 2, lf->lfHeight);
    hb_arraySetNL(aFont, 3, lf->lfWidth);
    hb_arraySetNL(aFont, 4, lf->lfWeight);
    hb_arraySetL(aFont, 5, lf->lfItalic);
    hb_arraySetL(aFont, 6, lf->lfUnderline);
    hb_arraySetL(aFont, 7, lf->lfStrikeOut);
    hb_arraySetNI(aFont, 8, lf->lfCharSet);
    hb_arraySetNI(aFont, 9, lf->lfEscapement);
    hb_arraySetNI(aFont, 10, lf->lfOrientation);
    hb_arraySetNI(aFont, 11, lf->lfOutPrecision);
    hb_arraySetNI(aFont, 12, lf->lfClipPrecision);
    hb_arraySetNI(aFont, 13, lf->lfQuality);
    hb_arraySetNI(aFont, 14, lf->lfPitchAndFamily);
  }

  return aFont;
}

//                   An Alternative to WndProc Callbacks

UINT_PTR CALLBACK WvgDialogProcChooseFont(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  HB_BOOL bret = false;
  HB_BOOL binit = false;
  PHB_ITEM block;

  if (msg == WM_INITDIALOG)
  {
    CHOOSEFONT *cf = (CHOOSEFONT *)lParam;
    auto pBlock = static_cast<PHB_ITEM>(hb_itemNew(reinterpret_cast<PHB_ITEM>(cf->lCustData)));
    SetProp(hwnd, TEXT("DIALOGPROC"), pBlock);
    binit = true;
  }

  block = static_cast<PHB_ITEM>(GetProp(hwnd, TEXT("DIALOGPROC")));

  if (block)
  {
    hb_vmPushEvalSym();
    hb_vmPush(block);
    hb_vmPushNumInt((HB_PTRUINT)hwnd);
    hb_vmPushInteger(msg);
    hb_vmPushNumInt((HB_PTRUINT)wParam);
    hb_vmPushNumInt((HB_PTRUINT)lParam);
    hb_vmDo(4);
    bret = hb_parnl(-1);

    if (msg == WM_NCDESTROY)
    {
      RemoveProp(hwnd, TEXT("DIALOGPROC"));
      hb_itemRelease(block);
    }
  }
  if (binit)
  {
    return true;
  }  

  return bret;
}

//
// Wvg_ChooseFont(hWnd, nWndProc, familyName, nominalPointSize,;
//                viewScreenFonts, viewPrinterFonts)
HB_FUNC(WVG_CHOOSEFONT)
{
  CHOOSEFONT cf; // = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  LOGFONT lf;    // = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  DWORD Flags;
  LONG PointSize = 0;
  HWND hWnd = wvg_parhwnd(1);
  TCHAR szStyle[MAX_PATH + 1];

  if (HB_ISCHAR(3))
  {
    void *hText;
    HB_STRNCPY(lf.lfFaceName, HB_PARSTR(3, &hText, nullptr), HB_SIZEOFARRAY(lf.lfFaceName) - 1);
    hb_strfree(hText);
  }
  if (HB_ISNUM(4) && hb_parnl(4))
  {
    HDC hdc = GetDC(hWnd);
    PointSize = -MulDiv((LONG)hb_parnl(4), GetDeviceCaps(hdc, LOGPIXELSY), 72);
    ReleaseDC(hWnd, hdc);
  }
  lf.lfHeight = PointSize;
  lf.lfWidth = 0;
  lf.lfWeight = 0;
  lf.lfItalic = 0;
  lf.lfUnderline = 0;
  lf.lfStrikeOut = 0;
  lf.lfCharSet = DEFAULT_CHARSET;
  lf.lfQuality = DEFAULT_QUALITY;
  lf.lfPitchAndFamily = FF_DONTCARE;

  Flags = CF_EFFECTS | CF_SHOWHELP | CF_APPLY | CF_INITTOLOGFONTSTRUCT | CF_ENABLEHOOK;

#if 0
   Flags |= CF_TTONLY;
   Flags |= CF_FIXEDPITCHONLY;
   Flags |= CF_SCALABLEONLY;
   Flags |= CF_NOVECTORFONTS;
   Flags |= CF_NOSCRIPTSEL;
   Flags |= CF_NOSIMULATIONS;              // ::synthesizeFonts  == .f. 
#endif

  if (hb_parl(5))
  {
    Flags = Flags | CF_SCREENFONTS;
  }
  if (hb_parl(6))
  {
    Flags = Flags | CF_PRINTERFONTS;
  }
  
  cf.lStructSize = sizeof(CHOOSEFONT);
  cf.hwndOwner = hWnd;
  cf.hDC = (HDC)nullptr; // only when ::oPrinterPS is defined
  cf.lpLogFont = &lf;
  cf.iPointSize = PointSize;
  cf.Flags = Flags;
  cf.rgbColors = RGB(0, 0, 0);

  cf.lCustData = (HB_PTRUINT)hb_param(2, Harbour::Item::BLOCK);
  cf.lpfnHook = WvgDialogProcChooseFont;

  cf.lpTemplateName = (LPTSTR)nullptr;
  cf.hInstance = (HINSTANCE)nullptr;
  cf.lpszStyle = (LPTSTR)szStyle;
  cf.nFontType = SCREEN_FONTTYPE; // ??
  cf.nSizeMin = 0;
  cf.nSizeMax = 0;

  if (ChooseFont(&cf))
  {
    PHB_ITEM aFont = wvg_logfontTOarray(&lf, false);
    auto aInfo = hb_itemNew(nullptr);

    hb_arrayNew(aInfo, 4);
    hb_arraySetNI(aInfo, 1, cf.iPointSize);
    hb_arraySetNInt(aInfo, 2, cf.rgbColors);
    hb_arraySetNI(aInfo, 3, cf.nFontType);
    HB_ARRAYSETSTR(aInfo, 4, cf.lpszStyle);
    hb_arraySet(aFont, 15, aInfo);

    hb_itemReturnRelease(aFont);
    hb_itemRelease(aInfo);
  }
}

HB_FUNC(WVG_CHOOSEFONT_GETLOGFONT)
{
  PHB_ITEM aFont;

  LOGFONT lf{};

  SendMessage(wvg_parhwnd(1), WM_CHOOSEFONT_GETLOGFONT, (WPARAM)0, (LPARAM)&lf);

  aFont = wvg_logfontTOarray(&lf, false);

  hb_itemReturnRelease(aFont);
}

HB_FUNC(WVG_FONTCREATE)
{
  HFONT hFont;
  PHB_ITEM aFont;

  LOGFONT lf{};

  aFont = hb_param(1, Harbour::Item::ARRAY);
  if (aFont)
  {
    HB_ITEMCOPYSTR(hb_arrayGetItemPtr(aFont, 1), lf.lfFaceName, HB_SIZEOFARRAY(lf.lfFaceName) - 1);

    lf.lfHeight = (LONG)hb_arrayGetNL(aFont, 2);
    lf.lfWidth = (LONG)hb_arrayGetNL(aFont, 3);
    lf.lfWeight = (LONG)hb_arrayGetNL(aFont, 4);
    lf.lfItalic = (BYTE)hb_arrayGetL(aFont, 5);
    lf.lfUnderline = (BYTE)hb_arrayGetL(aFont, 6);
    lf.lfStrikeOut = (BYTE)hb_arrayGetL(aFont, 7);
    lf.lfCharSet = (BYTE)hb_arrayGetNI(aFont, 8);
    lf.lfEscapement = (BYTE)hb_arrayGetNI(aFont, 9);
    lf.lfOrientation = (BYTE)hb_arrayGetNI(aFont, 10);
    lf.lfOutPrecision = (BYTE)hb_arrayGetNI(aFont, 11);
    lf.lfClipPrecision = (BYTE)hb_arrayGetNI(aFont, 12);
    lf.lfQuality = (BYTE)hb_arrayGetNI(aFont, 13);
    lf.lfPitchAndFamily = (BYTE)hb_arrayGetNI(aFont, 14);
  }

  hFont = CreateFontIndirect(&lf);

  if (hFont)
  {
    aFont = wvg_logfontTOarray(&lf, false);
    hb_arraySetNInt(aFont, 15, (HB_PTRUINT)hFont);
  }
  else
  {
    aFont = wvg_logfontTOarray(&lf, true);
  }
  
  hb_itemReturnRelease(aFont);
}

//
// Wvg_PointSizeToHeight(hdc, nPointSize)
//
HB_FUNC(WVG_POINTSIZETOHEIGHT)
{
  HDC hdc = HB_ISNUM(1) ? wvg_parhdc(1) : GetDC(GetDesktopWindow());

  hb_retnl((long)-MulDiv((LONG)hb_parnl(2), GetDeviceCaps(hdc, LOGPIXELSY), 72));

  if (!HB_ISNUM(1))
  {
    ReleaseDC(GetDesktopWindow(), hdc);
  }  
}

//
// Wvg_HeightToPointSize(hdc, nHeight)
//
HB_FUNC(WVG_HEIGHTTOPOINTSIZE)
{
  HDC hdc = HB_ISNUM(1) ? wvg_parhdc(1) : GetDC(GetDesktopWindow());

  hb_retnl((long)-MulDiv((LONG)hb_parnl(2), 72, GetDeviceCaps(hdc, LOGPIXELSY)));

  if (!HB_ISNUM(1))
  {
    ReleaseDC(GetDesktopWindow(), hdc);
  }  
}

HB_FUNC(WVG_SETCURRENTBRUSH)
{
#if (defined(_MSC_VER) && (_MSC_VER <= 1200)) && !defined(HB_ARCH_64BIT)
  SetClassLong(wvg_parhwnd(1), GCL_HBRBACKGROUND, (DWORD)hb_parnint(2));
#else
  SetClassLongPtr(wvg_parhwnd(1), GCLP_HBRBACKGROUND, (LONG_PTR)hb_parnint(2));
#endif
}

//                                IL  | DL
//  Wvg_AddToolBarButton(hWndTB, nBtn|hBitmap, cCaption, nButtonID, nMode, lIsTooltip)
//
HB_FUNC(WVG_ADDTOOLBARBUTTON)
{
  TBBUTTON tbb;
  HB_BOOL bSuccess;
  HWND hWndTB = hbwapi_par_raw_HWND(1);
  int iCommand = hb_parni(4);

  switch (hb_parni(5))
  {
  case 1: // button from image
  {
    int iNewString;

    // set string
    void *hCaption;
    iNewString = (int)SendMessage(hWndTB, TB_ADDSTRING, (WPARAM)0, (LPARAM)HB_PARSTR(3, &hCaption, nullptr));
    hb_strfree(hCaption);

    if (hb_parl(6))
    {
      SendMessage(hWndTB, TB_SETMAXTEXTROWS, (WPARAM)0, (LPARAM)0);
    }
    
    // add button
    tbb.iBitmap = hb_parni(2);
    tbb.idCommand = iCommand;
    tbb.fsState = TBSTATE_ENABLED;
    tbb.fsStyle = TBSTYLE_BUTTON | TBSTYLE_AUTOSIZE;
    tbb.dwData = 0;
    tbb.iString = iNewString;

    // FIXME: Convertion of LRESULT to HB_BOOL
    bSuccess = (HB_BOOL)SendMessage(hWndTB, TB_ADDBUTTONS, (WPARAM)1, (LPARAM)(LPTBBUTTON)&tbb);
    SendMessage(hWndTB, TB_SETPADDING, (WPARAM)0, (LPARAM)MAKELPARAM(10, 10));
    hb_retl(bSuccess);
    return;
  }

  case 2: // system bitmap

  case 3:            // separator
    tbb.iBitmap = 0; // Can be width of the separator
    tbb.idCommand = 0;
    tbb.fsState = TBSTATE_ENABLED;
    tbb.fsStyle = TBSTYLE_SEP;
    tbb.dwData = 0;
    tbb.iString = 0;

    // FIXME: Convertion of LRESULT to HB_BOOL
    bSuccess = (HB_BOOL)SendMessage(hWndTB, TB_ADDBUTTONS, (WPARAM)1, (LPARAM)(LPTBBUTTON)&tbb);
    hb_retl(bSuccess);
    return;
  }
}

//
// Wvg_RegisterClass(cClassName,
//
HB_FUNC(WVG_REGISTERCLASS_BYNAME)
{
  void *hClass;

  WNDCLASS wndclass{};
  wndclass.style = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
  wndclass.lpfnWndProc = DefWindowProc;
  wndclass.hInstance = (HINSTANCE)wvg_hInstance();
  wndclass.hIcon = nullptr;
  wndclass.hCursor = LoadCursor(nullptr, IDC_ARROW);
  wndclass.hbrBackground = nullptr;
  wndclass.lpszMenuName = nullptr;
  wndclass.lpszClassName = HB_PARSTR(1, &hClass, nullptr);

  if (!RegisterClass(&wndclass))
  {
    if (GetLastError() != 1410)
    {
      hb_errInternal(10001, "Failed to register DA window class", nullptr, nullptr);
    }
  }

  hb_strfree(hClass);
}

//
//  Function with win_FillRect() exists in hbwin:win_parn1.c with different approach.
//
HB_FUNC(WVG_FILLRECT)
{
  RECT rc;

  rc.left = hb_parvni(2, 1);
  rc.top = hb_parvni(2, 2);
  rc.right = hb_parvni(2, 3);
  rc.bottom = hb_parvni(2, 4);

  FillRect(wvg_parhdc(1), &rc, wvg_parhbrush(3));
}

HB_FUNC(WVG_BEGINMOUSETRACKING)
{
  TRACKMOUSEEVENT tmi;

  tmi.cbSize = sizeof(TRACKMOUSEEVENT);
  tmi.dwFlags = TME_LEAVE | TME_HOVER;
  tmi.hwndTrack = hbwapi_par_raw_HWND(1);
  tmi.dwHoverTime = 1;
  hbwapi_ret_L(_TrackMouseEvent(&tmi));
}

LRESULT CALLBACK ControlWindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  PHB_ITEM pBlock = static_cast<PHB_ITEM>(GetProp(hwnd, TEXT("BLOCKCALLBACK")));
  long lRet;

  if (pBlock)
  {
    if (hb_itemType(pBlock) == Harbour::Item::POINTER)
    {
      hb_vmPushSymbol(hb_dynsymSymbol(((PHB_SYMB)pBlock)->pDynSym));
      hb_vmPushNil();
    }
    else
    {
      hb_vmPushEvalSym();
      hb_vmPush(pBlock);
    }
    hb_vmPushNumInt((HB_PTRUINT)hwnd);
    hb_vmPushInteger(msg);
    hb_vmPushNumInt((HB_PTRUINT)wParam);
    hb_vmPushNumInt((HB_PTRUINT)lParam);
    hb_vmDo(4);
    lRet = (long)hb_parnint(-1);
    return lRet;
  }
  return DefWindowProc(hwnd, msg, wParam, lParam);
}

HB_FUNC(WVG_SETWINDOWPROCBLOCK)
{
  WNDPROC oldProc;
  HWND hWnd = hbwapi_par_raw_HWND(1);
  auto pBlock = hb_itemNew(hb_param(2, Harbour::Item::BLOCK));

  SetProp(hWnd, TEXT("BLOCKCALLBACK"), pBlock);

#if (defined(_MSC_VER) && (_MSC_VER <= 1200)) && !defined(HB_ARCH_64BIT)
  oldProc = (WNDPROC)SetWindowLong(hWnd, GWL_WNDPROC, (long)ControlWindowProcedure);
#else
  oldProc = (WNDPROC)SetWindowLongPtr(hWnd, GWLP_WNDPROC, (HB_PTRUINT)ControlWindowProcedure);
#endif

  hb_retnint((HB_PTRUINT)oldProc);
}

HB_FUNC(WVG_RELEASEWINDOWPROCBLOCK)
{
  HWND hWnd = hbwapi_par_raw_HWND(1);
  PHB_ITEM pBlock = static_cast<PHB_ITEM>(RemoveProp(hWnd, TEXT("BLOCKCALLBACK")));

  if (pBlock)
  {
    hb_itemRelease(pBlock);
  }  
}

//
// Wvg_CreateToolTipWindow(hControl) -> hWndTT
//
HB_FUNC(WVG_CREATETOOLTIPWINDOW)
{
  HWND hwndTip;

  hwndTip = CreateWindowEx(0, TOOLTIPS_CLASS, 0, WS_POPUP | TTS_ALWAYSTIP, /* | TTS_BALLOON, */
                           CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, wvg_parhwnd(1), nullptr,
                           wvg_hInstance(), nullptr);
  if (!hwndTip)
  {
    return;
  }

  TOOLINFO toolInfo{};
  toolInfo.cbSize = sizeof(toolInfo);
  toolInfo.hwnd = (HWND)wvg_parhwnd(1);
  toolInfo.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
  toolInfo.uId = (UINT_PTR)(HWND)wvg_parhwnd(1);
  toolInfo.lpszText = (LPTSTR)TEXT("");

  if (SendMessage(hwndTip, TTM_ADDTOOL, 0, (LPARAM)&toolInfo))
  {
    wvg_rethandle(hwndTip);
  }
  else
  {
    wvg_rethandle(nullptr);
  }
}

HB_FUNC(WVG_SETTOOLTIPTEXT)
{
  void *hText;

  TOOLINFO toolInfo{};
  toolInfo.cbSize = sizeof(toolInfo);
  toolInfo.hwnd = (HWND)wvg_parhwnd(1);
  toolInfo.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
  toolInfo.uId = (UINT_PTR)(HWND)wvg_parhwnd(1);
  toolInfo.lpszText = (LPTSTR)HB_PARSTRDEF(3, &hText, nullptr);

  SendMessage(wvg_parhwnd(2), TTM_SETTOOLINFO, (WPARAM)0, (LPARAM)&toolInfo);

  hb_strfree(hText);
}
