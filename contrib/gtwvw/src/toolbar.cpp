/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW toolbar functions
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

#include "hbgtwvw.hpp"

/*wvw_tbCreate([nWinNum], lDisplayText, nStyle, nSystemBitmap, nImageWidth, nImageHeight)
 * creates a toolbar at the top (no button initially)
 * lDisplayText==.f. button's string is used as tooltips (default)
 * nStyle: toolbar style, defaults to TBSTYLE_FLAT | TBSTYLE_TOOLTIPS
 * nSystemBitmap: 0:none, 1:small, 2:large (defaults: 1)
 *               small=16x16 large=24x24
 * nImageWidth/Height are in effect only if nSystemBitmap==0
 */
HB_FUNC(WVW_TBCREATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndParent = pWindowData->hWnd;
  HWND hWndTB;
  auto iMaxTextRows = static_cast<int>(HB_ISNIL(2) ? 0 : (hb_parl(2) ? 1 : 0));
  /*   DWORD dwStyle = static_cast<DWORD>(HB_ISNIL(3) ? TBSTYLE_FLAT | TBSTYLE_TOOLTIPS : hb_parni(3)); */
  auto dwStyle = static_cast<DWORD>(HB_ISNIL(3) ? TBSTYLE_ALTDRAG | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS |
                                                      TBSTYLE_TRANSPARENT | TBSTYLE_WRAPABLE
                                                : hb_parnl(3));

  auto iSystemBitmap = static_cast<int>(HB_ISNIL(4) ? 1 : hb_parni(4));
  auto iImageWidth = static_cast<int>(iSystemBitmap == 0 && HB_ISNUM(5) ? hb_parni(5) : -1);
  auto iImageHeight = static_cast<int>(iSystemBitmap == 0 && HB_ISNUM(6) ? hb_parni(6) : -1);
  TBADDBITMAP tbab{};

  InitCommonControls();

  if (pWindowData->hToolBar != nullptr)
  {
    hb_retnl(0);
    return;
  }

  if (iImageWidth < 0)
  {
    switch (iSystemBitmap)
    {
    case 1:
      iImageWidth = 16;
      break;
    case 2:
      iImageWidth = 24;
      break;
    default:
      iImageWidth = 16;
      break;
    }
  }
  if (iImageHeight < 0)
  {
    switch (iSystemBitmap)
    {
    case 1:
      iImageHeight = 16;
      break;
    case 2:
      iImageHeight = 24;
      break;
    default:
      iImageHeight = iImageWidth;
      break;
    }
  }

  hWndTB =
      CreateToolbarEx(hWndParent, WS_CHILD | WS_VISIBLE | dwStyle, WVW_ID_BASE_TOOLBAR + usWinNum, 0,
                      hb_getWvwData()->hInstance, 0, nullptr, 0, 0, 0, iImageWidth, iImageHeight, sizeof(TBBUTTON));

  if (hWndTB == nullptr)
  {
    MessageBox(nullptr, TEXT("Failed CreateToolbarEx..."), hb_gt_wvw_GetAppName(), MB_ICONERROR);
    hb_retnl(0);
  }

  pWindowData->tbOldProc =
      reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWndTB, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(hb_gt_wvwTBProc)));

  if (iSystemBitmap > 0)
  {
    tbab.hInst = HINST_COMMCTRL;

    tbab.nID = iSystemBitmap == 1 ? IDB_STD_SMALL_COLOR : IDB_STD_LARGE_COLOR;
    pWindowData->iStartStdBitmap = SendMessage(hWndTB, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab));

    tbab.nID = iSystemBitmap == 1 ? IDB_VIEW_SMALL_COLOR : IDB_VIEW_LARGE_COLOR;
    pWindowData->iStartViewBitmap = SendMessage(hWndTB, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab));

    tbab.nID = iSystemBitmap == 1 ? IDB_HIST_SMALL_COLOR : IDB_HIST_LARGE_COLOR;
    pWindowData->iStartHistBitmap = SendMessage(hWndTB, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab));
  }
  else
  {
    pWindowData->iStartStdBitmap = 0;
    pWindowData->iStartViewBitmap = 0;
    pWindowData->iStartHistBitmap = 0;
  }

  pWindowData->iTBImgWidth = iImageWidth;
  pWindowData->iTBImgHeight = iImageHeight;

  SendMessage(hWndTB, TB_SETMAXTEXTROWS, static_cast<WPARAM>(iMaxTextRows), 0);

  if (hWndTB)
  {

    hb_stornl(pWindowData->iStartStdBitmap, 7);
    hb_stornl(pWindowData->iStartViewBitmap, 8);
    hb_stornl(pWindowData->iStartHistBitmap, 9);

    hb_gt_wvwTBinitSize(pWindowData, hWndTB);

    pWindowData->hToolBar = hWndTB;

    hb_gt_wvwResetWindow(usWinNum);
  }

  hb_retnl(reinterpret_cast<LONG>(hWndTB));
}

/*wvw_tbAddButton([nWinNum], nCommand, xBitmap, cLabel, nBitmapType,;
 *                           lMap3Dcolors, lDropdown)
 * adds one button on the right of existing buttons
 * xBitmap:
 * nBitmap is resource id. or use cBitmap as bitmap file name.
 * (bitmap from resources cannot have > 256 colors)
 *
 * cLabel: if lDisplayText, it will be displayed below the bitmap
 *      otherwise it will be used as tooltip
 * nBitmapType: 0:custom, 1:system std bitmap, 2:system view bitmap, 3:system hist bitmap
 * lMap3Dcolors: defaults to .f.
 *         (meaningfull for custom bitmap only)
 *         if .t. the following color mapping will be performed:
 *            RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *            RGB(128,128,128) --> COLOR_3DSHADOW
 *            RGB(223,223,223) --> COLOR_3DLIGHT
 *         This might be desirable to have transparent effect.
 *         LIMITATION: this will work on 256 colored bitmaps only
 */

HB_FUNC(WVW_TBADDBUTTON)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iCommand = HB_ISNIL(2) ? 0 : hb_parni(2);

  char *szBitmap = HB_ISCHAR(3) ? const_cast<char *>(hb_parcx(3)) : nullptr;
  UINT uiBitmap = HB_ISNUM(3) ? static_cast<UINT>(hb_parni(3)) : 0;

  char *szLabel = HB_ISNIL(4) ? nullptr : const_cast<char *>(hb_parcx(4));
  int iBitmapType = HB_ISNIL(5) ? 0 : static_cast<int>(hb_parni(5));
  BOOL bMap3Dcolors = HB_ISLOG(6) ? hb_parl(6) : FALSE;
  BOOL bDropdown = HB_ISLOG(7) ? hb_parl(7) : FALSE;
  HWND hWndTB;
  USHORT usOldHeight;

  hWndTB = pWindowData->hToolBar;
  if (hWndTB == nullptr)
  {
    hb_retl(false);
    return;
  }

  if (iCommand >= WVW_ID_BASE_PUSHBUTTON)
  {
    MessageBox(nullptr, TEXT("Toolbar button Command Id too high. Potential conflict with pushbutton"),
               hb_gt_wvw_GetAppName(), MB_ICONERROR);
    hb_retl(false);
    return;
  }

  if (strlen(szLabel) > WVW_TB_LABELMAXLENGTH)
  {
    MessageBox(nullptr, TEXT("Cannot addbutton, Label too long..."), hb_gt_wvw_GetAppName(), MB_ICONERROR);
    hb_retl(false);
    return;
  }

  usOldHeight = pWindowData->usTBHeight;

  if (!AddTBButton(hWndTB, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, bMap3Dcolors, pWindowData, bDropdown))
  {
    if (iBitmapType == 0)
    {
      if (!AddTBButton(hWndTB, szBitmap, uiBitmap, szLabel, iCommand, 1, bMap3Dcolors, pWindowData, bDropdown))
      {
        MessageBox(nullptr, TEXT("Failed addbutton..."), hb_gt_wvw_GetAppName(), MB_ICONERROR);
        hb_retl(false);
        return;
      }
    }
    else
    {
      MessageBox(nullptr, TEXT("Failed addbutton..."), hb_gt_wvw_GetAppName(), MB_ICONERROR);
      hb_retl(false);
      return;
    }
  }

  hb_gt_wvwTBinitSize(pWindowData, hWndTB);

  if (pWindowData->usTBHeight != usOldHeight)
  {
    hb_gt_wvwResetWindow(usWinNum);
  }

  hb_retl(true);
}

/*wvw_tbButtonCount([nWinNum])
 * returns number of buttons in toolbar on window nWinNum
 */
HB_FUNC(WVW_TBBUTTONCOUNT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndTB;

  hWndTB = pWindowData->hToolBar;
  if (hWndTB == nullptr)
  {
    hb_retni(0);
    return;
  }

  hb_retni(SendMessage(hWndTB, TB_BUTTONCOUNT, 0, 0));
}

/*wvw_tbDelButton([nWinNum], nButton)
 * nButton is zero based index of button to delete
 * index=0 is the leftmost button
 * NOTE: button separator is indexed and deleteable too
 */
HB_FUNC(WVW_TBDELBUTTON)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iButton = HB_ISNUM(2) ? hb_parni(2) : -1;
  HWND hWndTB;
  USHORT usOldHeight;

  hWndTB = pWindowData->hToolBar;
  if (hWndTB == nullptr || iButton < 0)
  {
    hb_retl(false);
    return;
  }

  usOldHeight = pWindowData->usTBHeight;

  if (!SendMessage(hWndTB, TB_DELETEBUTTON, static_cast<WPARAM>(iButton), 0))
  {
    hb_retl(false);
    return;
  }

  hb_gt_wvwTBinitSize(pWindowData, hWndTB);

  if (pWindowData->usTBHeight != usOldHeight)
  {
    hb_gt_wvwResetWindow(usWinNum);
  }

  hb_retl(true);
}

/* wvw_tbGetButtonRect([nWinNum], nButton)
 * return an array {nRowStart, nColStart, nRowStop, nColStop}
 */
HB_FUNC(WVW_TBGETBUTTONRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iButton = HB_ISNUM(2) ? hb_parni(2) : -1;
  HWND hWndTB;
  RECT rc;
  RECT rcRect{};

  auto aXY = hb_itemNew(nullptr);

  hWndTB = pWindowData->hToolBar;
  if (hWndTB == nullptr || iButton < 0 ||
      !SendMessage(hWndTB, TB_GETRECT, static_cast<WPARAM>(iButton), reinterpret_cast<LPARAM>(&rc)))
  {
    hb_itemReturnRelease(aXY);
    return;
  }

  auto temp = hb_itemNew(nullptr);

  hb_arrayNew(aXY, 4);

  rcRect = hb_gt_wvwGetColRowFromXYRect(pWindowData, rc);
  hb_arraySetForward(aXY, 1, hb_itemPutNL(temp, HB_MAX(0, rcRect.top)));
  hb_arraySetForward(aXY, 2, hb_itemPutNL(temp, rcRect.left));

  hb_arraySetForward(aXY, 3, hb_itemPutNL(temp, HB_MIN(pWindowData->ROWS - 1, rcRect.bottom)));
  hb_arraySetForward(aXY, 4, hb_itemPutNL(temp, rcRect.right));
  hb_itemRelease(temp);

  hb_itemReturnRelease(aXY);
}

/*wvw_tbEnableButton([nWinNum], nButton, [lToggle])
 * nButton is zero based index of button to enable/disable
 * index=0 is the leftmost button
 * NOTE: button separator is indexed too
 * returns .t. if successful
 */
HB_FUNC(WVW_TBENABLEBUTTON)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iButton = HB_ISNUM(2) ? hb_parni(2) : -1;
  BOOL bEnable = HB_ISLOG(3) ? hb_parl(3) : TRUE;
  int iCommand;
  HWND hWndTB;
  USHORT usOldHeight;

  hWndTB = pWindowData->hToolBar;
  if (hWndTB == nullptr || iButton < 0)
  {
    hb_retl(false);
    return;
  }

  iCommand = IndexToCommand(hWndTB, iButton);
  if (iCommand < 0)
  {
    hb_retl(false);
    return;
  }

  usOldHeight = pWindowData->usTBHeight;

  if (!SendMessage(hWndTB, TB_ENABLEBUTTON, static_cast<WPARAM>(iCommand), static_cast<LPARAM>(MAKELONG(bEnable, 0))))
  {
    hb_retl(false);
    return;
  }

  hb_gt_wvwTBinitSize(pWindowData, hWndTB);

  if (pWindowData->usTBHeight != usOldHeight)
  {
    hb_gt_wvwResetWindow(usWinNum);
  }

  hb_retl(true);
}

/*wvw_tbDestroy( [nWinNum] )
 * destroy toolbar for window nWinNum
 */
HB_FUNC(WVW_TBDESTROY)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (!(pWindowData->hToolBar == nullptr))
  {
    DestroyWindow(pWindowData->hToolBar);
    pWindowData->hToolBar = nullptr;
    pWindowData->usTBHeight = 0;

    hb_gt_wvwResetWindow(usWinNum);
  }
}

/*wvw_tbIndex2Cmd([nWinNum], nIndex)
 * returns Command Id of button nIndex (0 based)
 * returns -1 if the button does not exist
 */
HB_FUNC(WVW_TBINDEX2CMD)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndTB = pWindowData->hToolBar;
  auto iIndex = hb_parni(2);
  int iCmd = IndexToCommand(hWndTB, iIndex);

  hb_retni(static_cast<int>(iCmd > 0 ? iCmd : -1));
}

/*wvw_tbCmd2Index([nWinNum], nCmd)
 * returns Index (0 based) of button whose command id is nCmd
 * returns -1 if the button does not exist
 */
HB_FUNC(WVW_TBCMD2INDEX)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndTB = pWindowData->hToolBar;
  auto iCmd = hb_parni(2);

  hb_retni(CommandToIndex(hWndTB, iCmd));
}

/* TOOLBAR ends                                                      */

#if _WIN32_IE > 0x400

/*                                                                   */
/*                              Tooltips                             */
/*                                                                   */

/*WVW_SetToolTopActive([nWinNum], [lToggle]) */
HB_FUNC(WVW_SETTOOLTIPACTIVE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  BOOL bActive = pWindowData->bToolTipActive;

  if (!HB_ISNIL(2))
  {

    if (hb_parl(2) && (pWindowData->hWndTT == nullptr))
    {
      hb_gt_wvwCreateToolTipWindow(pWindowData);
    }

    pWindowData->bToolTipActive = hb_parl(2);
  }

  hb_retl(bActive);
}

/*                                                                        */
/*   Wvw_SetToolTip( [nWinNum], nTop, nLeft, nBottom, nRight, cToolText ) */
/*                                                                        */
HB_FUNC(WVW_SETTOOLTIP)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  TOOLINFO ti{};
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;

  USHORT usTop = hb_parni(2), usLeft = hb_parni(3), usBottom = hb_parni(4), usRight = hb_parni(5);

  if (!pWindowData->bToolTipActive)
  {
    return;
  }

  if (hb_getWvwData()->s_bMainCoordMode)
  {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  ti.cbSize = sizeof(TOOLINFO);
  ti.hwnd = pWindowData->hWnd;
  ti.uId = WVW_ID_BASE_TOOLTIP + usWinNum;

  if (SendMessage(pWindowData->hWndTT, TTM_GETTOOLINFO, 0, static_cast<LPARAM>(&ti)))
  {
    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
    iTop = xy.y;
    iLeft = xy.x;

    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
    iBottom = xy.y - 1;
    iRight = xy.x - 1;

    ti.lpszText = hb_parc(6);
    ti.rect.left = iLeft;
    ti.rect.top = iTop;
    ti.rect.right = iRight;
    ti.rect.bottom = iBottom;

    SendMessage(pWindowData->hWndTT, TTM_SETTOOLINFO, 0, static_cast<LPARAM>(&ti));
  }
}

HB_FUNC(WVW_SETTOOLTIPTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  TOOLINFO ti;

  ti.cbSize = sizeof(TOOLINFO);
  ti.hwnd = pWindowData->hWnd;
  ti.uId = 100000;

  if (SendMessage(pWindowData->hWndTT, TTM_GETTOOLINFO, 0, static_cast<LPARAM>(&ti)))
  {
    ti.lpszText = hb_parcx(2);
    SendMessage(pWindowData->hWndTT, TTM_UPDATETIPTEXT, 0, static_cast<LPARAM>(&ti));
  }
}

HB_FUNC(WVW_SETTOOLTIPMARGIN)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  RECT rc{};

  rc.left = hb_parni(3);
  rc.top = hb_parni(2);
  rc.right = hb_parni(5);
  rc.bottom = hb_parni(4);

  SendMessage(pWindowData->hWndTT, TTM_SETMARGIN, 0, static_cast<LPARAM>(&rc));
}

HB_FUNC(WVW_SETTOOLTIPWIDTH)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  int iTipWidth = SendMessage(pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0);

  if (HB_ISNUM(2))
  {
    SendMessage(pWindowData->hWndTT, TTM_SETMAXTIPWIDTH, 0, static_cast<LPARAM>(static_cast<int>(hb_parni(2))));
  }

  hb_retni(iTipWidth);
}

HB_FUNC(WVW_SETTOOLTIPBKCOLOR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  COLORREF cr = SendMessage(pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0);

  if (HB_ISNUM(2))
  {
    SendMessage(pWindowData->hWndTT, TTM_SETTIPBKCOLOR, static_cast<WPARAM>(static_cast<COLORREF>(hb_parnl(2))), 0);
  }
  hb_retnl(static_cast<COLORREF>(cr));
}

HB_FUNC(WVW_SETTOOLTIPTEXTCOLOR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  COLORREF cr = SendMessage(pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0);

  if (HB_ISNUM(2))
  {
    SendMessage(pWindowData->hWndTT, TTM_SETTIPTEXTCOLOR, static_cast<WPARAM>(static_cast<COLORREF>(hb_parnl(2))), 0);
  }
  hb_retnl(static_cast<COLORREF>(cr));
}

HB_FUNC(WVW_SETTOOLTIPTITLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iIcon;

  if (!HB_ISNIL(3))
  {
    iIcon = HB_ISNIL(2) ? 0 : hb_parni(2);
    if (iIcon > 3)
    {
      iIcon = 0;
    }
    SendMessage(pWindowData->hWndTT, TTM_SETTITLE, static_cast<WPARAM>(iIcon), static_cast<LPARAM>(hb_parcx(3)));
  }
}

HB_FUNC(WVW_GETTOOLTIPWIDTH)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  hb_retni(SendMessage(pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0));
}

HB_FUNC(WVW_GETTOOLTIPBKCOLOR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  hb_retnl(static_cast<COLORREF>(SendMessage(pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0)));
}

HB_FUNC(WVW_GETTOOLTIPTEXTCOLOR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  hb_retnl(static_cast<COLORREF>(SendMessage(pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0)));
}

#endif
