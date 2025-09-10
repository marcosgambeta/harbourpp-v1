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

HB_FUNC(WVW_YESCLOSE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HMENU hMenu = GetSystemMenu(pWindowData->hWnd, FALSE);

  if (hMenu) {
    AppendMenu(hMenu, SC_CLOSE, MF_BYCOMMAND, "");
    DrawMenuBar(pWindowData->hWnd);
  }
}

HB_FUNC(WIN_SENDMESSAGE)
{
  char *cText = nullptr;

  if (HB_ISBYREF(4)) {
    cText = static_cast<char *>(hb_xgrab(hb_parcsiz(4)));
    hb_xmemcpy(cText, hb_parcx(4), hb_parcsiz(4));
  }

  hb_retnl(static_cast<ULONG>(SendMessage(
      reinterpret_cast<HWND>(HB_PARHANDLE(1)), static_cast<UINT>(hb_parni(2)),
      (HB_ISNIL(3) ? 0 : static_cast<WPARAM>(hb_parnl(3))),
      (HB_ISNIL(4) ? 0
                   : (HB_ISBYREF(4) ? reinterpret_cast<LPARAM>(static_cast<LPSTR>(cText))
                                    : (HB_ISCHAR(4) ? reinterpret_cast<LPARAM>(const_cast<LPSTR>(hb_parcx(4)))
                                                    : static_cast<LPARAM>(hb_parnl(4))))))));

  if (HB_ISBYREF(4)) {
    hb_storclen(cText, hb_parcsiz(4), 4);
    hb_xfree(cText);
  }
}

HB_FUNC(WIN_SENDDLGITEMMESSAGE)
{
  char *cText;
  auto pText = hb_param(5, Harbour::Item::STRING);

  if (pText) {
    cText = static_cast<char *>(hb_xgrab(hb_itemGetCLen(pText) + 1));
    hb_xmemcpy(cText, hb_itemGetCPtr(pText), hb_itemGetCLen(pText) + 1);
  } else {
    cText = nullptr;
  }

  hb_retnl(static_cast<LONG>(
      SendDlgItemMessage(reinterpret_cast<HWND>(HB_PARHANDLE(1)), static_cast<int>(hb_parni(2)),
                         static_cast<UINT>(hb_parni(3)), (HB_ISNIL(4) ? 0 : static_cast<WPARAM>(hb_parnl(4))),
                         (cText ? reinterpret_cast<LPARAM>(cText) : static_cast<LPARAM>(hb_parnl(5))))));

  if (pText) {
    hb_storclen(cText, hb_itemGetCLen(pText), 5);
  }

  if (cText) {
    hb_xfree(cText);
  }
}

/*
 *
 *  win_SetTimer( hWnd, nIdentifier, nTimeOut )
 */

HB_FUNC(WIN_SETTIMER)
{
  hb_retl(SetTimer(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2), hb_parni(3), nullptr));
}

HB_FUNC(WIN_SETFOCUS)
{
  SetFocus(reinterpret_cast<HWND>(HB_PARHANDLE(1)));
}

HB_FUNC(WIN_SETTEXTCOLOR)
{
  hb_retnl(
      static_cast<ULONG>(SetTextColor(reinterpret_cast<HDC>(HB_PARHANDLE(1)), static_cast<COLORREF>(hb_parnl(2)))));
}

HB_FUNC(WIN_SETBKCOLOR)
{
  hb_retnl(static_cast<ULONG>(SetBkColor(reinterpret_cast<HDC>(HB_PARHANDLE(1)), static_cast<COLORREF>(hb_parnl(2)))));
}

HB_FUNC(WVW_SETBKMODE)
{
  hb_retni(static_cast<int>(SetBkMode(reinterpret_cast<HDC>(HB_PARHANDLE(1)), hb_parni(2))));
}

HB_FUNC(WIN_GETSTOCKOBJECT)
{
  hb_retnl(reinterpret_cast<ULONG>(GetStockObject(hb_parnl(1))));
}

HB_FUNC(WIN_DELETEOBJECT)
{
  hb_retl(DeleteObject(reinterpret_cast<HGDIOBJ>(HB_PARHANDLE(1))));
}

HB_FUNC(WIN_SELECTOBJECT)
{
  hb_retnl(reinterpret_cast<ULONG>(
      SelectObject(reinterpret_cast<HDC>(HB_PARHANDLE(1)), reinterpret_cast<HGDIOBJ>(HB_PARHANDLE(2)))));
}

HB_FUNC(WIN_MULDIV)
{
  hb_retni(MulDiv(hb_parni(1), hb_parni(2), hb_parni(3)));
}

HB_FUNC(WIN_GETDIALOGBASEUNITS)
{
  hb_retnl(static_cast<LONG>(GetDialogBaseUnits()));
}

HB_FUNC(WIN_SETDLGITEMTEXT)
{
  SetDlgItemText(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2), hb_parc(3));
}

HB_FUNC(WIN_GETDLGITEMTEXT)
{
  USHORT iLen = static_cast<USHORT>(
                    SendMessage(static_cast<HWND>(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2))),
                                static_cast<UINT>(WM_GETTEXTLENGTH), 0, 0)) +
                1;
  auto cText = static_cast<char *>(hb_xgrab(iLen + 1));

  GetDlgItemText(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2), static_cast<LPTSTR>(cText), iLen);

  hb_retc(cText);
  hb_xfree(cText);
}

HB_FUNC(WIN_CHECKDLGBUTTON)
{
  hb_retl(CheckDlgButton(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2),
                         HB_ISNUM(3) ? hb_parni(3) : static_cast<UINT>(hb_parl(3))));
}

HB_FUNC(WIN_ISDLGBUTTONCHECKED)
{
  hb_retni(IsDlgButtonChecked(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2)));
}

HB_FUNC(WIN_CHECKRADIOBUTTON)
{
  hb_retl(CheckRadioButton(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2), hb_parni(3), hb_parni(4)));
}

HB_FUNC(WIN_GETDLGITEM)
{
  hb_retnl(reinterpret_cast<ULONG>(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2))));
}

HB_FUNC(WIN_MESSAGEBOX)
{
  hb_retni(
      MessageBox(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parcx(2), hb_parcx(3), HB_ISNIL(4) ? MB_OK : hb_parni(4)));
}

HB_FUNC(WIN_INVALIDATERECT)
{
  InvalidateRect(reinterpret_cast<HWND>(HB_PARHANDLE(1)), nullptr, TRUE);
}

/*
 *
 *  win_LoadIcon( ncIcon )
 */

HB_FUNC(WIN_LOADICON)
{
  HICON hIcon;

  if (HB_ISNUM(1)) {
    hIcon = LoadIcon(hb_getWvwData()->hInstance, MAKEINTRESOURCE(hb_parni(1)));
  } else {
    hIcon =
        static_cast<HICON>(LoadImage(static_cast<HINSTANCE>(nullptr), hb_parc(1), IMAGE_ICON, 0, 0, LR_LOADFROMFILE));
  }

  hb_retnl(reinterpret_cast<ULONG>(hIcon));
}

/*
 *
 *  win_LoadImage( ncImage, nSource ) -> hImage
 *    nSource == 0 ResourceIdByNumber
 *    nSource == 0 ResourceIdByName
 *    nSource == 0 ImageFromDiskFile
 */

HB_FUNC(WIN_LOADIMAGE)
{
  HBITMAP hImage = nullptr;
  auto iSource = hb_parni(2);

  switch (iSource) {
  case 0:
    hImage = LoadBitmap(hb_getWvwData()->hInstance, MAKEINTRESOURCE(hb_parni(1)));
    break;

  case 1:
    hImage = LoadBitmap(hb_getWvwData()->hInstance, hb_parc(1));
    break;

  case 2:
    hImage = static_cast<HBITMAP>(
        LoadImage(static_cast<HINSTANCE>(nullptr), hb_parc(1), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE));
    break;
  }

  hb_retnl(reinterpret_cast<ULONG>(hImage));
}

HB_FUNC(WIN_GETCLIENTRECT)
{
  RECT rc{};
  auto info = hb_itemArrayNew(4);

  GetClientRect(reinterpret_cast<HWND>(HB_PARHANDLE(1)), &rc);

  hb_arraySetNI(info, 1, rc.left);
  hb_arraySetNI(info, 2, rc.top);
  hb_arraySetNI(info, 3, rc.right);
  hb_arraySetNI(info, 4, rc.bottom);

  hb_itemReturnRelease(info);
}

/*
 *
 *    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels
 */

/* sorry, not supported in GTWVW
   HB_FUNC(WIN_DRAWIMAGE)
   {
   hb_retl(hb_wvt_DrawImage( ( HDC ) hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parc(6) ));
   }
 */

HB_FUNC(WIN_GETDC)
{
  HB_RETHANDLE(GetDC(reinterpret_cast<HWND>(HB_PARHANDLE(1))));
}

HB_FUNC(WIN_RELEASEDC)
{
  hb_retl(ReleaseDC(reinterpret_cast<HWND>(HB_PARHANDLE(1)), reinterpret_cast<HDC>(HB_PARHANDLE(2))));
}

HB_FUNC(WVW_RECTANGLE)
{
  Rectangle(reinterpret_cast<HDC>(HB_PARHANDLE(1)), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5));
}

HB_FUNC(WIN_CREATEBRUSH)
{
  LOGBRUSH lb{};

  lb.lbStyle = hb_parni(1);
  lb.lbColor = HB_ISNIL(2) ? RGB(0, 0, 0) : static_cast<COLORREF>(hb_parnl(2));
  lb.lbHatch = HB_ISNIL(3) ? 0 : hb_parni(3);

  hb_retnl(reinterpret_cast<ULONG>(CreateBrushIndirect(&lb)));
}

/*
 *
 *   win_DrawText( hDC, cText, aRect, nFormat )
 */

HB_FUNC(WIN_DRAWTEXT)
{
  RECT rc{};

  rc.left = hb_parvni(3, 1);
  rc.top = hb_parvni(3, 2);
  rc.right = hb_parvni(3, 3);
  rc.bottom = hb_parvni(3, 4);

  hb_retl(DrawText(reinterpret_cast<HDC>(HB_PARHANDLE(1)), hb_parc(2), strlen(hb_parc(2)), &rc, hb_parni(4)));
}

/* Adiciones a GtWVW desarrolladas por SOLUCIONES PERCEPTIVAS... */

HB_FUNC(WVW_GBCREATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  /* int   iStyle; */
  UINT uiPBid;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  LPCTSTR lpszCaption = HB_ISCHAR(6) ? hb_parcx(6) : nullptr;
  char *szBitmap = HB_ISCHAR(7) ? const_cast<char *>(hb_parcx(7)) : nullptr;
  UINT uiBitmap = HB_ISNUM(7) ? static_cast<UINT>(hb_parni(7)) : 0;
  double dStretch = !HB_ISNIL(10) ? hb_parnd(10) : 1;
  BOOL bMap3Dcolors = HB_ISLOG(11) ? static_cast<BOOL>(hb_parl(11)) : FALSE;

  iOffTop = !HB_ISNIL(9) ? hb_parvni(9, 1) : -1;
  iOffLeft = !HB_ISNIL(9) ? hb_parvni(9, 2) : -1;
  iOffBottom = !HB_ISNIL(9) ? hb_parvni(9, 3) : +1;
  iOffRight = !HB_ISNIL(9) ? hb_parvni(9, 4) : +1;

  uiPBid = ButtonCreate(usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption, szBitmap, uiBitmap,
                        hb_param(8, Harbour::Item::BLOCK), iOffTop, iOffLeft, iOffBottom, iOffRight, dStretch,
                        bMap3Dcolors, BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE);
  hb_retnl(static_cast<LONG>(uiPBid));
}

/* BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE
   BS_GROUPBOX | WS_GROUP | BS_TEXT | WS_OVERLAPPED */

HB_FUNC(WVW_RBCREATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  /* int   iStyle; */
  UINT uiPBid;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  LPCTSTR lpszCaption = HB_ISCHAR(6) ? hb_parcx(6) : nullptr;
  char *szBitmap = HB_ISCHAR(7) ? const_cast<char *>(hb_parcx(7)) : nullptr;
  UINT uiBitmap = HB_ISNUM(7) ? static_cast<UINT>(hb_parni(7)) : 0;
  double dStretch = !HB_ISNIL(10) ? hb_parnd(10) : 1;
  BOOL bMap3Dcolors = HB_ISLOG(11) ? static_cast<BOOL>(hb_parl(11)) : FALSE;

  if (!HB_ISBLOCK(8)) {
    hb_retnl(0);
    return;
  }

  iOffTop = !HB_ISNIL(9) ? hb_parvni(9, 1) : -2;
  iOffLeft = !HB_ISNIL(9) ? hb_parvni(9, 2) : -2;
  iOffBottom = !HB_ISNIL(9) ? hb_parvni(9, 3) : +2;
  iOffRight = !HB_ISNIL(9) ? hb_parvni(9, 4) : +2;

  uiPBid = ButtonCreate(usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption, szBitmap, uiBitmap,
                        hb_param(8, Harbour::Item::BLOCK), iOffTop, iOffLeft, iOffBottom, iOffRight, dStretch,
                        bMap3Dcolors, BS_AUTORADIOBUTTON /*| WS_GROUP*/);
  hb_retnl(static_cast<LONG>(uiPBid));
}

HB_FUNC(WVW_SETCONTROLTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte bStyle;
  auto hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (uiCtrlId == 0 || hWndPB == nullptr) {
    return;
  }
  SetWindowText(hWndPB, hb_parcx(3));
  hb_retl(true);
}

HB_FUNC(WVW_PBVISIBLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL bEnable = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte bStyle;
  auto hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int iCmdShow;

  if (uiCtrlId == 0 || hWndPB == nullptr) {
    hb_retl(false);
    return;
  }

  if (bEnable) {
    iCmdShow = SW_SHOW;
  } else {
    iCmdShow = SW_HIDE;
  }
  hb_retl(ShowWindow(hWndPB, iCmdShow) == 0);
}

HB_FUNC(WVW_CBVISIBLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL bEnable = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte bStyle;
  auto hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);
  int iCmdShow;

  if (hWndCB) {
    if (bEnable) {
      iCmdShow = SW_SHOW;
    } else {
      iCmdShow = SW_HIDE;
    }
    hb_retl(ShowWindow(hWndCB, iCmdShow) == 0);
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_CXVISIBLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL bEnable = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte bStyle;
  auto hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int iCmdShow;

  if (uiCtrlId == 0 || hWndPB == nullptr) {
    hb_retl(false);
    return;
  }

  if (bEnable) {
    iCmdShow = SW_SHOW;
  } else {
    iCmdShow = SW_HIDE;
  }
  hb_retl(ShowWindow(hWndPB, iCmdShow) == 0);
}

/* wvw_xbVisible( [nWinNum], nXBid, lShow )
 *  show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *  nWinNum better be NIL
 *  nXBid is the handle of the scrolbar
 *  lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 * returns .t. if successful
 */
HB_FUNC(WVW_XBVISIBLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto uiXBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  auto bShow = static_cast<BOOL>(HB_ISLOG(3) ? hb_parl(3) : TRUE);
  byte bStyle;
  auto hWndXB =
      static_cast<HWND>(uiXBid == 0 ? nullptr : FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle));

  if (uiXBid == 0 || hWndXB == nullptr) {
    hb_retl(false);
    return;
  }

  hb_retl(ShowScrollBar(hWndXB, SB_CTL, bShow));
}

HB_FUNC(WVW_MOUSE_COL)
{
  auto pData = hb_getWvwData();

  if (pData) {
    if (hb_gt_wvw_GetMainCoordMode()) {
      hb_retni(hb_gt_wvwGetMouseX(pData->s_pWindows[pData->s_usNumWindows - 1]) +
               hb_gt_wvwColOfs(pData->s_usNumWindows - 1));
    } else {
      hb_retni(hb_gt_wvwGetMouseX(pData->s_pWindows[pData->s_usCurWindow]));
    }
  } else {
    hb_retni(0);
  }
}

HB_FUNC(WVW_MOUSE_ROW)
{
  auto pData = hb_getWvwData();

  if (pData) {
    if (hb_gt_wvw_GetMainCoordMode()) {
      hb_retni(hb_gt_wvwGetMouseY(pData->s_pWindows[pData->s_usNumWindows - 1]) +
               hb_gt_wvwRowOfs(pData->s_usNumWindows - 1));
    } else {
      hb_retni(hb_gt_wvwGetMouseY(pData->s_pWindows[pData->s_usCurWindow]));
    }
  } else {
    hb_retni(0);
  }
}

HB_FUNC(SENDMESSAGE)
{

  hb_retnl(
      static_cast<LONG>(SendMessage(reinterpret_cast<HWND>(HB_PARHANDLE(1)), /* handle of destination window */
                                    static_cast<UINT>(hb_parni(2)),          /* message to send */
                                    static_cast<WPARAM>(hb_parnl(3)),        /* first message parameter */
                                    (HB_ISCHAR(4)) ? reinterpret_cast<LPARAM>(hb_parc(4))
                                                   : static_cast<LPARAM>(hb_parnl(4)) /* second message parameter */
                                    )));
}

HB_FUNC(SETPARENT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT usWinNum1 = HB_ISNIL(2)
                       ? (hb_gt_wvw_GetMainCoordMode() ? ((hb_gt_wvw_GetNumWindows()) - 1) : hb_gt_wvw_GetCurWindow())
                       : (static_cast<USHORT>(hb_parni(2)));
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pWindowData1 = hb_gt_wvw_GetWindowsData(usWinNum1);
  HWND hWndParent = pWindowData->hWnd;
  HWND hWndParent1 = pWindowData1->hWnd;

  if (usWinNum1 != 0) {
    SetParent(hWndParent, hWndParent1);
  }
}

HB_FUNC(BRINGTOTOP1)
{
  auto hWnd = reinterpret_cast<HWND>(HB_PARHANDLE(1));

#if 0
   DWORD ForegroundThreadID;
   DWORD ThisThreadID;
   DWORD timeout;
#endif

  if (IsIconic(hWnd)) {
    ShowWindow(hWnd, SW_RESTORE);
    hb_retl(true);
    return;
  }
  BringWindowToTop(hWnd); /* IE 5.5 related hack */
  SetForegroundWindow(hWnd);
}

HB_FUNC(ISWINDOW)
{
  hb_retl(IsWindow(reinterpret_cast<HWND>(HB_PARHANDLE(1))));
}

HB_FUNC(ADDTOOLTIPEX) /* changed by MAG */
{
  /* HWND hWnd = ( HWND ) hb_parnl(1); */
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pData = hb_getWvwData();

  int iStyle = TTS_ALWAYSTIP;
  INITCOMMONCONTROLSEX icex{};
  TOOLINFO ti{};

  /* Load the tooltip class from the DLL.
   */
  icex.dwSize = sizeof(icex);
  icex.dwICC = ICC_BAR_CLASSES;

  if (!InitCommonControlsEx(&icex)) {
  }

#if 0
   if( lToolTipBalloon ) {
      iStyle = iStyle | TTS_BALLOON;
   }
#endif

  if (!pData->hWndTT) {
    pData->hWndTT =
        CreateWindow(TOOLTIPS_CLASS, static_cast<LPSTR>(nullptr), iStyle, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                     CW_USEDEFAULT, nullptr, static_cast<HMENU>(nullptr), GetModuleHandle(nullptr), nullptr);
  }
  if (!pData->hWndTT) {
    hb_retnl(0);
    return;
  }
  ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
  ti.hwnd = pWindowData->hWnd;
  ti.uId = static_cast<UINT>(hb_parnl(2));
  /* ti.uId    = ( UINT ) GetDlgItem(hWnd, hb_parni(2)); */
  ti.hinst = GetModuleHandle(nullptr);
  ti.lpszText = const_cast<LPSTR>(hb_parc(3));

  hb_retl(SendMessage(pData->hWndTT, TTM_ADDTOOL, 0, reinterpret_cast<LPARAM>(static_cast<LPTOOLINFO>(&ti))));
}

/*
 * CreateImageList( array, cx, cy, nGrow, flags )
 */
HB_FUNC(CREATEIMAGELIST)
{
  auto pArray = hb_param(1, Harbour::Item::ARRAY);
  UINT flags = HB_ISNIL(5) ? ILC_COLOR : hb_parni(5);
  HIMAGELIST himl;
  ULONG ulLen = hb_arrayLen(pArray);
  HBITMAP hbmp;

  himl = ImageList_Create(hb_parni(2), hb_parni(3), flags, ulLen, hb_parni(4));

  for (ULONG ul = 1; ul <= ulLen; ul++) {
    hbmp = reinterpret_cast<HBITMAP>(hb_arrayGetNL(pArray, ul));
    ImageList_Add(himl, hbmp, static_cast<HBITMAP>(nullptr));
    DeleteObject(hbmp);
  }

  HB_RETHANDLE(himl);
}

HB_FUNC(IMAGELIST_ADD)
{
  hb_retnl(ImageList_Add(reinterpret_cast<HIMAGELIST>(HB_PARHANDLE(1)), reinterpret_cast<HBITMAP>(HB_PARHANDLE(2)),
                         static_cast<HBITMAP>(nullptr)));
}

HB_FUNC(IMAGELIST_ADDMASKED)
{
  hb_retnl(ImageList_AddMasked(reinterpret_cast<HIMAGELIST>(HB_PARHANDLE(1)),
                               reinterpret_cast<HBITMAP>(HB_PARHANDLE(2)), static_cast<COLORREF>(hb_parnl(3))));
}

HB_FUNC(GETBITMAPSIZE)
{
  BITMAP bitmap;
  auto aMetr = hb_itemArrayNew(3);

  GetObject(reinterpret_cast<HBITMAP>(HB_PARHANDLE(1)), sizeof(BITMAP), static_cast<LPVOID>(&bitmap));

  hb_arraySetNL(aMetr, 1, bitmap.bmWidth);
  hb_arraySetNL(aMetr, 2, bitmap.bmHeight);
  hb_arraySetNL(aMetr, 3, bitmap.bmBitsPixel);

  hb_itemReturnRelease(aMetr);
}

HB_FUNC(GETICONSIZE)
{
  ICONINFO iinfo;
  auto aMetr = hb_itemArrayNew(2);

  GetIconInfo(reinterpret_cast<HICON>(HB_PARHANDLE(1)), &iinfo);

  hb_arraySetNL(aMetr, 1, iinfo.xHotspot * 2);
  hb_arraySetNL(aMetr, 2, iinfo.yHotspot * 2);

  hb_itemReturnRelease(aMetr);
}

HB_FUNC(LOADIMAGE)
{
  if (HB_ISNUM(2)) {
    hb_retnl(reinterpret_cast<LONG>(
        LoadImage(hb_getWvwData()->hInstance, /* HB_ISNIL(1) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1),   handle
                                                 of the instance that contains the image */
                  static_cast<LPCTSTR>(MAKEINTRESOURCE(hb_parnl(2))), /* name or identifier of image */
                  static_cast<UINT>(hb_parni(3)),                     /* type of image */
                  hb_parni(4),                                        /* desired width */
                  hb_parni(5),                                        /* desired height */
                  static_cast<UINT>(hb_parni(6))                      /* load flags */
                  )));
  } else {
    HB_RETHANDLE(
        LoadImage(reinterpret_cast<HINSTANCE>(hb_parnl(1)), /* handle of the instance that contains the image */
                  static_cast<LPCTSTR>(hb_parc(2)),         /* name or identifier of image */
                  static_cast<UINT>(hb_parni(3)),           /* type of image */
                  hb_parni(4),                              /* desired width */
                  hb_parni(5),                              /* desired height */
                  static_cast<UINT>(hb_parni(6))            /* load flags */
                  ));
  }
}

HB_FUNC(LOADBITMAP)
{
  if (HB_ISNUM(1)) {
    if (!HB_ISNIL(2) && hb_parl(2)) {
      /*               hb_retnl((LONG) LoadBitmap( GetModuleHandle( NULL ),  MAKEINTRESOURCE(hb_parnl(1) ))); */
      HB_RETHANDLE(LoadBitmap(nullptr, reinterpret_cast<LPCTSTR>(hb_parnl(1))));
    } else {
      HB_RETHANDLE(LoadBitmap(GetModuleHandle(nullptr), reinterpret_cast<LPCTSTR>(hb_parnl(1))));
    }
  } else {
    HB_RETHANDLE(LoadBitmap(GetModuleHandle(nullptr), static_cast<LPCTSTR>(hb_parc(1))));
  }
}

HB_FUNC(LOADBITMAPEX)
{
  HINSTANCE h = HB_ISNUM(1) ? reinterpret_cast<HINSTANCE>(hb_parnl(1)) : GetModuleHandle(nullptr);

  if (HB_ISNUM(1) && HB_ISNUM(2)) {
    if (!HB_ISNIL(3) && hb_parl(3)) {
      /*               hb_retnl((LONG) LoadBitmap( h,  MAKEINTRESOURCE(hb_parnl(2) ))); */
      HB_RETHANDLE(LoadBitmap(h, reinterpret_cast<LPCTSTR>(hb_parnl(3))));
    } else {
      HB_RETHANDLE(LoadBitmap(static_cast<HINSTANCE>(h), reinterpret_cast<LPCTSTR>(hb_parnl(2))));
    }
  } else {
    HB_RETHANDLE(LoadBitmap(h, static_cast<LPCTSTR>(hb_parc(2))));
  }
}

HB_FUNC(OPENIMAGE)
{
  auto cFileName = hb_parc(1);
  BOOL lString = HB_ISNIL(2) ? 0 : hb_parl(2);
  int iFileSize;
  FILE *fp;
  /* IPicture * pPic; */
  LPPICTURE pPic = nullptr;
  IStream *pStream;
  HGLOBAL hG;
  HBITMAP hBitmap = 0;

  if (lString) {
    iFileSize = hb_parclen(1);
    hG = GlobalAlloc(GPTR, iFileSize);
    if (!hG) {
      hb_retnl(0);
      return;
    }
    memcpy(static_cast<void *>(hG), static_cast<void *>(const_cast<char *>(cFileName)), iFileSize);
  } else {
    fp = fopen(cFileName, "rb");
    if (!fp) {
      hb_retnl(0);
      return;
    }

    fseek(fp, 0, SEEK_END);
    iFileSize = ftell(fp);
    hG = GlobalAlloc(GPTR, iFileSize);
    if (!hG) {
      fclose(fp);
      hb_retnl(0);
      return;
    }
    fseek(fp, 0, SEEK_SET);
    fread(static_cast<void *>(hG), 1, iFileSize, fp);
    fclose(fp);
  }

  CreateStreamOnHGlobal(hG, 0, &pStream);

  if (!pStream) {
    GlobalFree(hG);
    hb_retnl(0);
    return;
  }

#if 0
#if defined(__cplusplus)
   OleLoadPicture(pStream, 0, 0, &IID_IPicture, static_cast<void**>(&pPic));
   pStream->Release();
#else
   OleLoadPicture(pStream, 0, 0, &IID_IPicture, static_cast<void**>(&pPic));
   pStream->lpVtbl->Release(pStream);
#endif
#endif

  GlobalFree(hG);

  if (!pPic) {
    hb_retnl(0);
    return;
  }

#if 0
#if defined(__cplusplus)
   pPic->get_Handle(static_cast<OLE_HANDLE*>(&hBitmap));
#else
   pPic->lpVtbl->get_Handle(pPic, static_cast<OLE_HANDLE*>(&hBitmap));
#endif
#endif

  hb_retnl(reinterpret_cast<LONG>(CopyImage(hBitmap, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG)));

#if 0
#if defined(__cplusplus)
   pPic->Release();
#else
   pPic->lpVtbl->Release(pPic);
#endif
#endif
}

HB_FUNC(OPENBITMAP)
{
  BITMAPFILEHEADER bmfh;
  BITMAPINFOHEADER bmih;
  DWORD dwRead;
  LPVOID lpvBits;
  HGLOBAL hmem1, hmem2;
  HBITMAP hbm;
  HDC hDC = (hb_pcount() > 1 && !HB_ISNIL(2)) ? reinterpret_cast<HDC>(HB_PARHANDLE(2)) : nullptr;
  HANDLE hfbm = CreateFile(hb_parc(1), GENERIC_READ, FILE_SHARE_READ, static_cast<LPSECURITY_ATTRIBUTES>(nullptr),
                           OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, static_cast<HANDLE>(nullptr));

  if ((reinterpret_cast<long int>(hfbm)) <= 0) {
    HB_RETHANDLE(nullptr);
    return;
  }
  /* Retrieve the BITMAPFILEHEADER structure. */
  ReadFile(hfbm, &bmfh, sizeof(BITMAPFILEHEADER), &dwRead, nullptr);

  /* Retrieve the BITMAPFILEHEADER structure. */
  ReadFile(hfbm, &bmih, sizeof(BITMAPINFOHEADER), &dwRead, nullptr);

  /* Allocate memory for the BITMAPINFO structure. */

  hmem1 = GlobalAlloc(GHND, sizeof(BITMAPINFOHEADER) + ((1 << bmih.biBitCount) * sizeof(RGBQUAD)));
  auto lpbmi = static_cast<LPBITMAPINFO>(GlobalLock(hmem1));

  /*  Load BITMAPINFOHEADER into the BITMAPINFO  structure. */
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

  /*  Retrieve the color table.
   * 1 << bmih.biBitCount == 2 ^ bmih.biBitCount
   */
  switch (bmih.biBitCount) {
  case 1:
  case 4:
  case 8:
    ReadFile(hfbm, lpbmi->bmiColors, ((1 << bmih.biBitCount) * sizeof(RGBQUAD)), &dwRead,
             static_cast<LPOVERLAPPED>(nullptr));
    break;

  case 16:
  case 32:
    if (bmih.biCompression == BI_BITFIELDS) {
      ReadFile(hfbm, lpbmi->bmiColors, (3 * sizeof(RGBQUAD)), &dwRead, static_cast<LPOVERLAPPED>(nullptr));
    }
    break;

  case 24:
    break;
  }

  /* Allocate memory for the required number of  bytes. */
  hmem2 = GlobalAlloc(GHND, (bmfh.bfSize - bmfh.bfOffBits));
  lpvBits = GlobalLock(hmem2);

  /* Retrieve the bitmap data. */

  ReadFile(hfbm, lpvBits, (bmfh.bfSize - bmfh.bfOffBits), &dwRead, nullptr);

  if (!hDC) {
    hDC = GetDC(0);
  }

  /* Create a bitmap from the data stored in the .BMP file.  */
  hbm = CreateDIBitmap(hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS);

  if (hb_pcount() < 2 || HB_ISNIL(2)) {
    ReleaseDC(0, hDC);
  }

  /* Unlock the global memory objects and close the .BMP file. */
  GlobalUnlock(hmem1);
  GlobalUnlock(hmem2);
  GlobalFree(hmem1);
  GlobalFree(hmem2);
  CloseHandle(hfbm);

  HB_RETHANDLE(hbm);
}

HB_FUNC(SETTEXTCOLOR)
{
  COLORREF crColor = SetTextColor(reinterpret_cast<HDC>(HB_PARHANDLE(1)), /* handle of device context */
                                  static_cast<COLORREF>(hb_parnl(2)));    /* text color */

  hb_retnl(static_cast<LONG>(crColor));
}

HB_FUNC(SETBKCOLOR)
{
  COLORREF crColor = SetBkColor(reinterpret_cast<HDC>(HB_PARHANDLE(1)), /* handle of device context */
                                static_cast<COLORREF>(hb_parnl(2)));    /* text color */

  hb_retnl(static_cast<LONG>(crColor));
}

HB_FUNC(CREATESOLIDBRUSH)
{
  HB_RETHANDLE(CreateSolidBrush(static_cast<COLORREF>(hb_parnl(1)) /* brush color */));
}

HB_FUNC(CREATEHATCHBRUSH)
{
  HB_RETHANDLE(CreateHatchBrush(hb_parni(1), static_cast<COLORREF>(hb_parnl(2))));
}

HB_FUNC(RGB)
{
  hb_retnl(RGB(hb_parni(1), hb_parni(2), hb_parni(3)));
}

#if 0
HB_FUNC(GETSYSCOLOR) // TODO: deprecated (using waGetSysColor from WinApi library)
{
   hb_retnl(static_cast<LONG>(GetSysColor(hb_parni(1))));
}
#endif

HB_FUNC_TRANSLATE(GETSYSCOLOR, WAGETSYSCOLOR)

HB_FUNC(REDRAWWINDOW)
{
  RedrawWindow(reinterpret_cast<HWND>(HB_PARHANDLE(1)), /* handle of window */
               nullptr,                                 /* address of structure with update rectangle */
               nullptr,                                 /* handle of update region */
               static_cast<UINT>(hb_parni(2)));         /* array of redraw flags */
}

/* CreateFont( fontName, nWidth, hHeight [,fnWeight] [,fdwCharSet],
               [,fdwItalic] [,fdwUnderline] [,fdwStrikeOut]  )
 */
HB_FUNC(CREATEFONT)
{
  int fnWeight = HB_ISNIL(4) ? 0 : hb_parni(4);
  DWORD fdwCharSet = HB_ISNIL(5) ? 0 : hb_parnl(5);
  DWORD fdwItalic = HB_ISNIL(6) ? 0 : hb_parnl(6);
  DWORD fdwUnderline = HB_ISNIL(7) ? 0 : hb_parnl(7);
  DWORD fdwStrikeOut = HB_ISNIL(8) ? 0 : hb_parnl(8);

  auto hFont = CreateFont(hb_parni(3),                       /* logical height of font */
                          hb_parni(2),                       /* logical average character width */
                          0,                                 /* angle of escapement */
                          0,                                 /* base-line orientation angle */
                          fnWeight,                          /* font weight */
                          fdwItalic,                         /* italic attribute flag */
                          fdwUnderline,                      /* underline attribute flag */
                          fdwStrikeOut,                      /* strikeout attribute flag */
                          fdwCharSet,                        /* character set identifier */
                          0,                                 /* output precision */
                          0,                                 /* clipping precision */
                          0,                                 /* output quality */
                          0,                                 /* pitch and family */
                          static_cast<LPCTSTR>(hb_parc(1))); /* pointer to typeface name string */
  HB_RETHANDLE(hFont);
}

HB_FUNC(SELECTFONT)
{
  LOGFONT lf;
  PHB_ITEM pObj = HB_ISNIL(1) ? nullptr : hb_param(1, Harbour::Item::OBJECT);
  /* PHB_ITEM temp1; */

  CHOOSEFONT cf;
  cf.lStructSize = sizeof(CHOOSEFONT);
  cf.hwndOwner = static_cast<HWND>(nullptr);
  cf.hDC = static_cast<HDC>(nullptr);
  cf.lpLogFont = &lf;
  cf.iPointSize = 0;
  cf.Flags = CF_SCREENFONTS | ((pObj) ? CF_INITTOLOGFONTSTRUCT : 0);
  cf.rgbColors = RGB(0, 0, 0);
  cf.lCustData = 0L;
  cf.lpfnHook = static_cast<LPCFHOOKPROC>(nullptr);
  cf.lpTemplateName = static_cast<LPSTR>(nullptr);
  cf.hInstance = static_cast<HINSTANCE>(nullptr);
  cf.lpszStyle = static_cast<LPSTR>(nullptr);
  cf.nFontType = SCREEN_FONTTYPE;
  cf.nSizeMin = 0;
  cf.nSizeMax = 0;

  /* Display the CHOOSEFONT common-dialog box. */

  if (!ChooseFont(&cf)) {
    // hb_itemRelease(aMetr);
    hb_ret();
    return;
  }

  /* Create a logical font based on the user's   */
  /* selection and return a handle identifying   */
  /* that font.                                  */

  auto hfont = CreateFontIndirect(cf.lpLogFont);

  auto aMetr = hb_itemArrayNew(9);
  hb_arraySetNInt(aMetr, 1, reinterpret_cast<HB_PTRUINT>(hfont));
  hb_arraySetC(aMetr, 2, lf.lfFaceName);
  hb_arraySetNL(aMetr, 3, lf.lfWidth);
  hb_arraySetNL(aMetr, 4, lf.lfHeight);
  hb_arraySetNL(aMetr, 5, lf.lfWeight);
  hb_arraySetNI(aMetr, 6, lf.lfCharSet);
  hb_arraySetNI(aMetr, 7, lf.lfItalic);
  hb_arraySetNI(aMetr, 8, lf.lfUnderline);
  hb_arraySetNI(aMetr, 9, lf.lfStrikeOut);
  hb_itemReturnRelease(aMetr);
}

HB_FUNC(INVALIDATERECT)
{
  RECT rc;

  if (hb_pcount() > 2) {
    rc.left = hb_parni(3);
    rc.top = hb_parni(4);
    rc.right = hb_parni(5);
    rc.bottom = hb_parni(6);
  }

  InvalidateRect(reinterpret_cast<HWND>(HB_PARHANDLE(1)), /* handle of window with changed update region */
                 (hb_pcount() > 2) ? &rc : nullptr,       /* address of rectangle coordinates */
                 hb_parni(2));                            /* erase-background flag */
}

HB_FUNC(TOOLBARADDBUTTONS)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  auto hWndCtrl = reinterpret_cast<HWND>(HB_PARHANDLE(2));
  /* HWND hToolTip = ( HWND ) hb_parnl(5) ; */
  auto pArray = hb_param(3, Harbour::Item::ARRAY);
  auto iButtons = hb_parni(4);
  auto tb = static_cast<struct _TBBUTTON *>(hb_xgrab(iButtons * sizeof(TBBUTTON)));
  PHB_ITEM pTemp;
  /* BOOL bSystem; */

#if 0
   ULONG  ulID;
#endif
  DWORD style = GetWindowLong(hWndCtrl, GWL_STYLE);
  USHORT usOldHeight;

  SetWindowLong(hWndCtrl, GWL_STYLE, style | TBSTYLE_TOOLTIPS | TBSTYLE_FLAT);

  SendMessage(hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0L);
  usOldHeight = pWindowData->usTBHeight;
  for (ULONG ulCount = 0; (ulCount < hb_arrayLen(pArray)); ulCount++) {
    pTemp = hb_arrayGetItemPtr(pArray, ulCount + 1);
#if 0
      ulID  = hb_arrayGetNI(pTemp, 1);
#endif
    /* bSystem = hb_arrayGetL(pTemp, 9); */

#if 0
      if( bSystem ) {
         if( ulID > 0 && ulID < 31 ) {
            tb[ulCount].iBitmap = ulID > 0 ? static_cast<int>(ulID) : -1;
         } else {
            tb[ulCount].iBitmap = ulID > 0 ? static_cast<int>(ulCount) : -1;
         }
      }
#endif
    tb[ulCount].idCommand = hb_arrayGetNI(pTemp, 2);
    tb[ulCount].fsState = static_cast<BYTE>(hb_arrayGetNI(pTemp, 3));
    tb[ulCount].fsStyle = static_cast<BYTE>(hb_arrayGetNI(pTemp, 4));
    tb[ulCount].dwData = hb_arrayGetNI(pTemp, 5);
    tb[ulCount].iString = hb_arrayGetCLen(pTemp, 6) > 0 ? reinterpret_cast<int>(hb_arrayGetCPtr(pTemp, 6)) : 0;
  }

  SendMessage(hWndCtrl, TB_ADDBUTTONS, static_cast<WPARAM>(iButtons),
              reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(tb)));
  SendMessage(hWndCtrl, TB_AUTOSIZE, 0, 0);
  hb_gt_wvwTBinitSize(pWindowData, hWndCtrl);

  if (pWindowData->usTBHeight != usOldHeight) {
    hb_gt_wvwResetWindow(usWinNum);
  }

  hb_xfree(tb);
}

HB_FUNC(SETBITMAPRESOURCEID)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  TBADDBITMAP tbab;
  auto hBitmap = reinterpret_cast<HBITMAP>(HB_PARHANDLE(3));
  auto uiBitmap = static_cast<UINT>(hb_parni(4));
  HWND hWndToolbar = pWindowData->hToolBar;
  int iNewBitmap;
  auto iBitmapType = hb_parni(2);
  int iOffset;

  switch (iBitmapType) {
  case 0:
    iOffset = 0;
    break;
  case 1:
    iOffset = pWindowData->iStartStdBitmap;
    break;
  case 2:
    iOffset = pWindowData->iStartViewBitmap;
    break;
  case 3:
    iOffset = pWindowData->iStartHistBitmap;
    break;
  default:
    iOffset = 0;
    break;
  }

  if (iBitmapType == 0) {
    tbab.hInst = nullptr;
    tbab.nID = reinterpret_cast<UINT>(hBitmap);
    iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, static_cast<WPARAM>(1), reinterpret_cast<WPARAM>(&tbab));
  } else { /* system bitmap */
    iNewBitmap = static_cast<int>(uiBitmap) + iOffset;
  }
  hb_retni(iNewBitmap);
}

HB_FUNC(DRAWICON)
{
  DrawIcon(reinterpret_cast<HDC>(HB_PARHANDLE(1)), hb_parni(3), hb_parni(4), reinterpret_cast<HICON>(HB_PARHANDLE(2)));
}

HB_FUNC(LOADICON)
{
  if (HB_ISNUM(1)) {
    HB_RETHANDLE(LoadIcon(nullptr, reinterpret_cast<LPCTSTR>(hb_parnl(1))));
  } else {
    HB_RETHANDLE(LoadIcon(GetModuleHandle(nullptr), static_cast<LPCTSTR>(hb_parc(1))));
  }
}

HB_FUNC(DRAWBITMAP)
{
  auto hDC = reinterpret_cast<HDC>(HB_PARHANDLE(1));
  auto hDCmem = CreateCompatibleDC(hDC);
  DWORD dwraster = HB_ISNIL(3) ? SRCCOPY : hb_parnl(3);
  auto hBitmap = reinterpret_cast<HBITMAP>(HB_PARHANDLE(2));
  BITMAP bitmap;
  int nWidthDest = (hb_pcount() >= 5 && !HB_ISNIL(6)) ? hb_parni(6) : 0;
  int nHeightDest = (hb_pcount() >= 6 && !HB_ISNIL(7)) ? hb_parni(7) : 0;

  SelectObject(hDCmem, hBitmap);
  GetObject(hBitmap, sizeof(BITMAP), static_cast<LPVOID>(&bitmap));
  if (nWidthDest && (nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight)) {
    StretchBlt(hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem, 0, 0, bitmap.bmWidth, bitmap.bmHeight,
               dwraster);
  } else {
    BitBlt(hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster);
  }

  DeleteDC(hDCmem);
}

HB_FUNC(WINDOW2BITMAP)
{
  auto hWnd = reinterpret_cast<HWND>(HB_PARHANDLE(1));
  BOOL lFull = HB_ISNIL(2) ? 0 : static_cast<BOOL>(hb_parl(2));
  HDC hDC = lFull ? GetWindowDC(hWnd) : GetDC(hWnd);
  auto hDCmem = CreateCompatibleDC(hDC);
  RECT rc;

  if (lFull) {
    GetWindowRect(hWnd, &rc);
  } else {
    GetClientRect(hWnd, &rc);
  }

  auto hBitmap = CreateCompatibleBitmap(hDC, rc.right - rc.left, rc.bottom - rc.top);
  SelectObject(hDCmem, hBitmap);

  BitBlt(hDCmem, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, 0, 0, SRCCOPY);

  DeleteDC(hDCmem);
  DeleteDC(hDC);
  HB_RETHANDLE(hBitmap);
}

/* wvw_SetMaxBMCache([nMax])
   Get/Set maximum user-bitmap cache (default is 20, minimum is 1).
   Returns old setting of maximum user-bitmap cache.

   Description:
   To minimize bitmap loading operation, wvw_drawimage caches bitmap once
   it reads from disk.
   Ie., subsequent wvw_drawimage will use the bitmap from the memory.
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
  auto p = hb_getWvwData();
  UINT uiOldMaxBMcache = 0;

  if (p) {
    uiOldMaxBMcache = p->s_sApp->uiMaxBMcache;

    if (!HB_ISNIL(1)) {
      p->s_sApp->uiMaxBMcache = static_cast<UINT>(hb_parni(1));
    }
  }

  hb_retni(uiOldMaxBMcache);
}

/* wvw_NumBMCache()
   Returns current number of user-bitmap cache.
 */
HB_FUNC(WVW_NUMBMCACHE)
{
  auto p = hb_getWvwData();

  hb_retni(p ? p->s_sApp->uiBMcache : 0);
}

/*                                                                   */
/*               Miscellaneous xHarbour callable functions           */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */
/* TIMER                                                             */

/*wvw_SetTimer([nWinNum], nInterval)
 * set timer event for every nInterval millisec
 *(effective only if WVW_TIMER() function exists)
 * eg. it can be usefull to update clock on status bar
 * returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different*/
HB_FUNC(WVW_SETTIMER)
{
  auto p = hb_getWvwData();

  if (p && p->s_sApp->pSymWVW_TIMER) {
    auto usWinNum = WVW_WHICH_WINDOW;
    auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

    SetTimer(pWindowData->hWnd, WVW_ID_BASE_TIMER + usWinNum, static_cast<UINT>(hb_parni(2)), nullptr);

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*wvw_KillTimer([nWinNum])
 * kill the timer event handler for window nWinNum
 * returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different */
HB_FUNC(WVW_KILLTIMER)
{
  auto p = hb_getWvwData();

  if (p && p->s_sApp->pSymWVW_TIMER) {
    auto usWinNum = WVW_WHICH_WINDOW;
    auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

    KillTimer(pWindowData->hWnd, WVW_ID_BASE_TIMER + usWinNum);
    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*wvw_GetPaintRect( nWinNum )   nWinNum is 0 based               */
/*returns array of paint pending rect {top, left, bottom, right} */
/*WARNING:                                                       */
/*unlike WVT, top maybe > bottom                                 */
/*            left maybe > right                                 */
/*in these cases, no paint request is pending                    */
/*(in WVT these is reflected in {0,0,0,0})                       */
HB_FUNC(WVW_GETPAINTRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  RECT rPaintRect = pWindowData->rPaintPending;
  auto info = hb_itemArrayNew(4);

  hb_arraySetNI(info, 1, rPaintRect.top);
  hb_arraySetNI(info, 2, rPaintRect.left);
  hb_arraySetNI(info, 3, rPaintRect.bottom);
  hb_arraySetNI(info, 4, rPaintRect.right);

  hb_itemReturnRelease(info);
}

HB_FUNC(WVW_SETPOINTER)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto iCursor = hb_parni(2);
  HCURSOR hCursor;

  switch (iCursor) {
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
    break;
  }

  SetClassLongPtr(pWindowData->hWnd, GCLP_HCURSOR, reinterpret_cast<LONG_PTR>(hCursor));
}

/*                                                                   */
/*   wvw_LoadPicture( nSlot, cFilePic )                              */
/*                                                                   */
HB_FUNC(WVW_LOADPICTURE)
{
  BOOL bResult = FALSE;

  auto p = hb_getWvwData();
  IPicture *iPicture = hb_gt_wvwLoadPicture(hb_parcx(2));

  if (p && iPicture) {
    int iSlot = hb_parni(1) - 1;

    if (p->s_sApp->iPicture[iSlot]) {
      hb_gt_wvwDestroyPicture(p->s_sApp->iPicture[iSlot]);
    }

    p->s_sApp->iPicture[iSlot] = iPicture;

    bResult = TRUE;
  }

  hb_retl(bResult);
}

/*                                                                                                */
/* wvw_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout, */
/*               nCharSet, nQuality, nEscapement )                                                */
/*                                                                                                */
HB_FUNC(WVW_LOADFONT)
{
  auto p = hb_getWvwData();

  if (p) {
    UINT usWinNum = p->s_usNumWindows - 1;
    auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
    LOGFONT logfont;
    int iSlot = hb_parni(1) - 1;

    logfont.lfEscapement = HB_ISNIL(11) ? 0 : (hb_parni(11) * 10);
    logfont.lfOrientation = 0;
    logfont.lfWeight = HB_ISNIL(5) ? 0 : hb_parni(5);
    logfont.lfItalic = HB_ISNIL(6) ? 0 : static_cast<BYTE>(hb_parl(6));
    logfont.lfUnderline = HB_ISNIL(7) ? 0 : static_cast<BYTE>(hb_parl(7));
    logfont.lfStrikeOut = HB_ISNIL(8) ? 0 : static_cast<BYTE>(hb_parl(8));
    logfont.lfCharSet = HB_ISNIL(9) ? static_cast<BYTE>(pWindowData->CodePage) : static_cast<BYTE>(hb_parni(9));
    logfont.lfOutPrecision = 0;
    logfont.lfClipPrecision = 0;
    logfont.lfQuality = HB_ISNIL(10) ? static_cast<BYTE>(DEFAULT_QUALITY) : static_cast<BYTE>(hb_parni(10));
    logfont.lfPitchAndFamily = FF_DONTCARE;
    logfont.lfHeight = HB_ISNIL(3) ? pWindowData->fontHeight : hb_parni(3);
    logfont.lfWidth =
        HB_ISNIL(4) ? (pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth) : hb_parni(4);

    strcpy(logfont.lfFaceName, HB_ISNIL(2) ? pWindowData->fontFace : hb_parcx(2));

    auto hFont = CreateFontIndirect(&logfont);
    if (hFont) {
      if (p->s_sApp->hUserFonts[iSlot]) {
        DeleteObject(static_cast<HFONT>(p->s_sApp->hUserFonts[iSlot]));
      }
      p->s_sApp->hUserFonts[iSlot] = hFont;
    }
  }
}

/*                                                                   */
/*  wvw_LoadPen( nSlot, nStyle, nWidth, nRGBColor )                  */
/*                                                                   */
HB_FUNC(WVW_LOADPEN)
{
  auto p = hb_getWvwData();
  int iSlot = hb_parni(1) - 1;

  int iPenStyle = HB_ISNIL(2) ? 0 : hb_parni(2);
  int iPenWidth = HB_ISNIL(3) ? 0 : hb_parni(3);
  auto crColor = static_cast<COLORREF>(HB_ISNIL(4) ? RGB(0, 0, 0) : hb_parnl(4));

  auto hPen = CreatePen(iPenStyle, iPenWidth, crColor);

  if (hPen) {
    if (p->s_sApp->hUserPens[iSlot]) {
      DeleteObject(static_cast<HPEN>(p->s_sApp->hUserPens[iSlot]));
    }
    p->s_sApp->hUserPens[iSlot] = hPen;

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW_MESSAGEBOX)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  hb_retni(MessageBox(pWindowData->hWnd, hb_parcx(2), hb_parcx(3), HB_ISNIL(4) ? MB_OK : hb_parni(4)));
}

/*                    End of Drawing Primitives                      */

/*                                                                   */
/*              Utility Functions . A Natural Extension              */
/*                copied and modified from gtwvt                     */

/*                                                                      */
/*     wvw_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ; */
/*                                    lItalic, lUnderline, lStrikeout ) */
/*                                                                      */

HB_FUNC(WVW_CHOOSEFONT)
{
  LONG PointSize = 0;
  auto p = hb_getWvwData();

  if (!HB_ISNIL(2)) {
    PointSize = -MulDiv(static_cast<LONG>(hb_parnl(2)),
                        GetDeviceCaps(p->s_pWindows[p->s_usNumWindows - 1]->hdc, LOGPIXELSY), 72);
  }

  LOGFONT lf{};
  lf.lfHeight = PointSize;
  lf.lfWidth = HB_ISNIL(3) ? 0 : hb_parni(3);
  lf.lfWeight = HB_ISNIL(4) ? 0 : hb_parni(4);
  lf.lfItalic = HB_ISNIL(6) ? 0 : static_cast<BYTE>(hb_parl(6));
  lf.lfUnderline = HB_ISNIL(7) ? 0 : static_cast<BYTE>(hb_parl(7));
  lf.lfStrikeOut = HB_ISNIL(8) ? 0 : static_cast<BYTE>(hb_parl(8));
  lf.lfCharSet = DEFAULT_CHARSET;
  lf.lfQuality = HB_ISNIL(5) ? DEFAULT_QUALITY : static_cast<BYTE>(hb_parni(5));
  lf.lfPitchAndFamily = FF_DONTCARE;
  if (HB_ISCHAR(1)) {
    strcpy(lf.lfFaceName, hb_parcx(1));
  }

  CHOOSEFONT cf{};
  cf.lStructSize = sizeof(CHOOSEFONT);
  cf.hwndOwner = p->s_pWindows[p->s_usNumWindows - 1]->hWnd;
  cf.hDC = static_cast<HDC>(nullptr);
  cf.lpLogFont = &lf;
  cf.iPointSize = 0;
  cf.Flags = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
  cf.rgbColors = RGB(0, 0, 0);
  cf.lCustData = 0L;
  cf.lpfnHook = static_cast<LPCFHOOKPROC>(nullptr);
  cf.lpTemplateName = static_cast<LPSTR>(nullptr);
  cf.hInstance = static_cast<HINSTANCE>(nullptr);
  cf.lpszStyle = static_cast<LPSTR>(nullptr);
  cf.nFontType = SCREEN_FONTTYPE;
  cf.nSizeMin = 0;
  cf.nSizeMax = 0;

  if (ChooseFont(&cf)) {
    PointSize = -MulDiv(lf.lfHeight, 72, GetDeviceCaps(p->s_pWindows[p->s_usNumWindows - 1]->hdc, LOGPIXELSY));

    hb_reta(8);

    hb_storvc(lf.lfFaceName, -1, 1);
    hb_storvnl(static_cast<LONG>(PointSize), -1, 2);
    hb_storvni(lf.lfWidth, -1, 3);
    hb_storvni(lf.lfWeight, -1, 4);
    hb_storvni(lf.lfQuality, -1, 5);
    hb_storvl(lf.lfItalic, -1, 6);
    hb_storvl(lf.lfUnderline, -1, 7);
    hb_storvl(lf.lfStrikeOut, -1, 8);
  } else {
    hb_reta(8);

    hb_storvc("", -1, 1);
    hb_storvnl(0, -1, 2);
    hb_storvni(0, -1, 3);
    hb_storvni(0, -1, 4);
    hb_storvni(0, -1, 5);
    hb_storvl(0, -1, 6);
    hb_storvl(0, -1, 7);
    hb_storvl(0, -1, 8);
  }

  return;
}

/*                                                                   */
/*    wvw_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected    */
/*                                                                   */

HB_FUNC(WVW_CHOOSECOLOR)
{
  auto p = hb_getWvwData();

  COLORREF crCustClr[16];
  for (auto i = 0; i < 16; i++) {
    crCustClr[i] = HB_ISARRAY(2) ? static_cast<COLORREF>(hb_parvnl(2, i + 1)) : GetSysColor(COLOR_BTNFACE);
  }

  CHOOSECOLOR cc;
  cc.lStructSize = sizeof(CHOOSECOLOR);
  cc.hwndOwner = p->s_pWindows[p->s_usNumWindows - 1]->hWnd;
  cc.rgbResult = HB_ISNIL(1) ? 0 : static_cast<COLORREF>(hb_parnl(1));
  cc.lpCustColors = crCustClr;
  cc.Flags = static_cast<WORD>(HB_ISNIL(3) ? CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN : hb_parnl(3));

  if (ChooseColor(&cc)) {
    hb_retnl(cc.rgbResult);
  } else {
    hb_retnl(-1);
  }
}

/*wvw_SetMousePos( nWinNum, nRow, nCol ) nWinNum is 0 based        */
/*WHAT'S the difference with GT_FUNC(mouse_SetPos) ???           */
/*this func is able to position cursor on any window               */

/*NOTE: consider using 'standard' SetMouse() instead:     */
/*      SetMouse(.t., nRow, nCol)                                  */
/*      This will treat (nRow,nCol) according to current s_pWvwData->s_bMainCoordMode setting */

HB_FUNC(WVW_SETMOUSEPOS)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto usRow = static_cast<USHORT>(hb_parni(2));
  auto usCol = static_cast<USHORT>(hb_parni(3));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, nullptr, nullptr);
  }

  POINT xy = hb_gt_wvwGetXYFromColRow(pWindowData, usCol, usRow);

  if (ClientToScreen(pWindowData->hWnd, &xy)) {
    hb_retl(SetCursorPos(xy.x, xy.y + (pWindowData->PTEXTSIZE.y / 2)));
  } else {
    hb_retl(false);
  }
}

/*by bdj                                                                                */
/*none in gtwvt                                                                         */
/*    wvw_FillRectangle( nWinNum, nTop, nLeft, nBottom, nRight, nRGBcolor/hBrush,       */
/*                       lTight, lUseBrush, aOffSet )                                   */
/*                                                                                      */
/*   if lTight, rect is drawn inside the character region                               */
/*   AND top and left lines are lower two pixel down to make room for above/left object */
/*   WARNING: gui object of this type subject to be overwritten by chars                */
/*   NOTE that these lines are to be overwritten by displayed char,                     */
/*        we are depending on the fact that gui object will be painted last             */
/*                                                                                      */
/*   if lUseBrush, nRGBcolor is treated as a BRUSH handle                               */
/*                                                                                      */

HB_FUNC(WVW_FILLRECTANGLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto p = hb_getWvwData();
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iTop, iLeft, iBottom, iRight;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  COLORREF crRGBcolor = HB_ISNIL(6) ? 0 : hb_parnl(6);
  BOOL bTight = HB_ISNIL(7) ? FALSE : hb_parl(7);
  BOOL bUseBrush = HB_ISNIL(8) ? FALSE : hb_parl(8);
  LOGBRUSH lb{};
  RECT xyRect{};

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  int iOffTop = !HB_ISNIL(9) ? hb_parvni(9, 1) : 0;
  int iOffLeft = !HB_ISNIL(9) ? hb_parvni(9, 2) : 0;
  int iOffBottom = !HB_ISNIL(9) ? hb_parvni(9, 3) : 0;
  int iOffRight = !HB_ISNIL(9) ? hb_parvni(9, 4) : 0;

  POINT xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = bTight ? xy.y + 2 : xy.y;
  iLeft = bTight ? xy.x + 2 : xy.x;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1;
  iRight = xy.x - 1;

  /* Aplica OffSet */
  iTop += iOffTop;
  iLeft += iOffLeft;
  iBottom += iOffBottom;
  iRight += iOffRight;

  xyRect.left = iLeft;
  xyRect.top = iTop;
  xyRect.right = iRight + 1;
  xyRect.bottom = iBottom + 1;

  lb.lbStyle = BS_SOLID;
  lb.lbColor = crRGBcolor;
  lb.lbHatch = 0;

  HBRUSH hBrush = !bUseBrush ? CreateBrushIndirect(&lb) : reinterpret_cast<HBRUSH>(HB_PARHANDLE(6));

  FillRect(pWindowData->hdc, &xyRect, hBrush);

  if (!bUseBrush) {
    SelectObject(p->s_pWindows[0]->hdc, static_cast<HBRUSH>(p->s_sApp->OriginalBrush));
    DeleteObject(hBrush);
  }

  hb_retl(true);
}

HB_FUNC(WVW_LBADDSTRING)
{
  SendMessage(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2)), LB_ADDSTRING, 0,
              reinterpret_cast<LPARAM>(const_cast<LPSTR>(hb_parcx(3))));
}

HB_FUNC(WVW_LBSETCURSEL)
{
  SendMessage(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2)), LB_SETCURSEL, hb_parni(3), 0);
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC(WVW_CBADDSTRING)
{
  SendMessage(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2)), CB_ADDSTRING, 0,
              reinterpret_cast<LPARAM>(const_cast<LPSTR>(hb_parcx(3))));
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC(WVW_CBSETCURSEL)
{
  SendMessage(GetDlgItem(reinterpret_cast<HWND>(HB_PARHANDLE(1)), hb_parni(2)), CB_SETCURSEL, hb_parni(3), 0);
}

HB_FUNC(WVW_DLGSETICON)
{
  HICON hIcon;

  if (HB_ISNUM(2)) {
    hIcon = LoadIcon(hb_getWvwData()->hInstance, MAKEINTRESOURCE(hb_parni(2)));
  } else {
    hIcon =
        static_cast<HICON>(LoadImage(static_cast<HINSTANCE>(nullptr), hb_parc(2), IMAGE_ICON, 0, 0, LR_LOADFROMFILE));
  }

  if (hIcon) {
    SendMessage(reinterpret_cast<HWND>(HB_PARHANDLE(1)), WM_SETICON, ICON_SMALL,
                reinterpret_cast<LPARAM>(hIcon)); /* Set Title Bar ICON */
    SendMessage(reinterpret_cast<HWND>(HB_PARHANDLE(1)), WM_SETICON, ICON_BIG,
                reinterpret_cast<LPARAM>(hIcon)); /* Set Task List Icon */
  }

  if (hIcon) {
    hb_retnl(reinterpret_cast<ULONG>(hIcon));
  }
}

/*                                                                   */
/*                      GUI Drawing Functions                        */
/*               Pritpal Bedi <pritpal@vouchcac.com>                 */
/*                                                                   */

/*                                                                   */
/*   wvw_SetPen( nPenStyle, nWidth, nColor )                         */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide.
 */

HB_FUNC(WVW_SETPEN)
{
  auto p = hb_getWvwData();

  if (HB_ISNIL(1)) {
    hb_retl(false);
  }

  auto iPenStyle = hb_parni(1);
  int iPenWidth = HB_ISNIL(2) ? 0 : hb_parni(2);
  COLORREF crColor = HB_ISNIL(3) ? RGB(0, 0, 0) : static_cast<COLORREF>(hb_parnl(3));

  auto hPen = CreatePen(iPenStyle, iPenWidth, crColor);

  if (hPen) {
    /* 20040923, was:
       if( s_pWvwData->s_pWindows[usWinNum]->currentPen ) {
          DeleteObject(static_cast<HPEN>(s_pWvwData->s_pWindows[usWinNum]->currentPen));
       }
       s_pWvwData->s_pWindows[usWinNum]->currentPen = hPen;
     */

    if (p->s_sApp->currentPen) {
      DeleteObject(static_cast<HPEN>(p->s_sApp->currentPen));
    }

    p->s_sApp->currentPen = hPen;

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*                                                                   */
/*   wvw_SetBrush( nStyle, nColor, [nHatch] )                        */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide.
 */

HB_FUNC(WVW_SETBRUSH)
{
  auto p = hb_getWvwData();

  if (HB_ISNIL(1)) {
    hb_retl(false);
  }

  LOGBRUSH lb{};
  lb.lbStyle = hb_parnl(1);
  lb.lbColor = HB_ISNIL(2) ? RGB(0, 0, 0) : static_cast<COLORREF>(hb_parnl(2));
  lb.lbHatch = HB_ISNIL(3) ? 0 : hb_parnl(3);

  auto hBrush = CreateBrushIndirect(&lb);

  if (hBrush) {
    /* 20040923,was:
       if( s_pWvwData->s_pWindows[usWinNum]->currentBrush ) {
          DeleteObject(static_cast<HBRUSH>(s_pWvwData->s_pWindows[usWinNum]->currentBrush));
       }
       s_pWvwData->s_pWindows[usWinNum]->currentBrush = hBrush;
     */

    if (p->s_sApp->currentBrush) {
      SelectObject(p->s_pWindows[0]->hdc, static_cast<HBRUSH>(p->s_sApp->OriginalBrush));
      DeleteObject(static_cast<HBRUSH>(p->s_sApp->currentBrush));
    }
    p->s_sApp->currentBrush = hBrush;

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

HB_FUNC(WVW__MAKEDLGTEMPLATE)
{
  WORD *p, *pdlgtemplate;
  auto nItems = static_cast<WORD>(hb_parvni(1, 4));
  int nchar;

  pdlgtemplate = p = static_cast<PWORD>(LocalAlloc(LPTR, 65534));

  DWORD lStyle = hb_parvnl(1, 3);

  *p++ = 1;
  *p++ = 0xFFFF;
  *p++ = LOWORD(hb_parvnl(1, 1));
  *p++ = HIWORD(hb_parvnl(1, 1));

  *p++ = LOWORD(hb_parvnl(1, 2));
  *p++ = HIWORD(hb_parvnl(1, 2));

  *p++ = LOWORD(lStyle);
  *p++ = HIWORD(lStyle);

  *p++ = static_cast<WORD>(nItems);
  *p++ = static_cast<short>(hb_parvni(1, 5));
  *p++ = static_cast<short>(hb_parvni(1, 6));
  *p++ = static_cast<short>(hb_parvni(1, 7));
  *p++ = static_cast<short>(hb_parvni(1, 8));
  *p++ = static_cast<short>(0);
  *p++ = static_cast<short>(0x00);

  if (hb_parinfa(1, 11) == Harbour::Item::STRING) {
    nchar = nCopyAnsiToWideChar(p, TEXT(const_cast<char *>(hb_parvcx(1, 11))));
    p += nchar;
  } else {
    *p++ = 0;
  }

  if ((lStyle & DS_SETFONT)) {
    *p++ = static_cast<short>(hb_parvni(1, 12));
    *p++ = static_cast<short>(hb_parvni(1, 13));
    *p++ = static_cast<short>(hb_parvni(1, 14));

    nchar = nCopyAnsiToWideChar(p, TEXT(const_cast<char *>(hb_parvcx(1, 15))));
    p += nchar;
  }

  for (auto i = 1; i <= nItems; i++) {

    p = lpwAlign(p);

    *p++ = LOWORD(hb_parvnl(2, i));
    *p++ = HIWORD(hb_parvnl(2, i));

    *p++ = LOWORD(hb_parvnl(3, i));
    *p++ = HIWORD(hb_parvnl(3, i));

    *p++ = LOWORD(hb_parvnl(4, i));
    *p++ = HIWORD(hb_parvnl(4, i));

    *p++ = static_cast<short>(hb_parvni(5, i));
    *p++ = static_cast<short>(hb_parvni(6, i));
    *p++ = static_cast<short>(hb_parvni(7, i));
    *p++ = static_cast<short>(hb_parvni(8, i));

    *p++ = LOWORD(hb_parvnl(9, i));
    *p++ = HIWORD(hb_parvnl(9, i));

    if (hb_parinfa(10, i) == Harbour::Item::STRING) {
      nchar = nCopyAnsiToWideChar(p, TEXT(const_cast<char *>(hb_parvcx(10, i))));
      p += nchar;
    } else {
      *p++ = 0xFFFF;
      *p++ = static_cast<WORD>(hb_parvni(10, i));
    }

    if (hb_parinfa(11, i) == Harbour::Item::STRING) {
      nchar = nCopyAnsiToWideChar(p, const_cast<LPSTR>(hb_parvcx(11, i)));
      p += nchar;
    } else {
      *p++ = 0xFFFF;
      *p++ = static_cast<WORD>(hb_parvni(11, i));
    }

    *p++ = 0x00;
  }

  p = lpwAlign(p);

  hb_retclen(reinterpret_cast<LPSTR>(pdlgtemplate),
             (reinterpret_cast<ULONG>(p) - reinterpret_cast<ULONG>(pdlgtemplate)));

  LocalFree(LocalHandle(pdlgtemplate));
}

HB_FUNC(WVW_GETCURSORPOS)
{
  POINT xy{};
  GetCursorPos(&xy);
  auto info = hb_itemArrayNew(2);
  hb_arraySetNI(info, 1, xy.x);
  hb_arraySetNI(info, 2, xy.y);
  hb_itemReturnRelease(info);
}

/* wvw_ShowWindow( [nWinNum], nCmdShow ) */
HB_FUNC(WVW_SHOWWINDOW)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iCmdShow = HB_ISNUM(2) ? hb_parni(2) : SW_SHOWNORMAL;

  ShowWindow(pWindowData->hWnd, iCmdShow);
}

/* wvw_UpdateWindow( [nWinNum] ) */
HB_FUNC(WVW_UPDATEWINDOW)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  UpdateWindow(pWindowData->hWnd);
}

/*
 *
 *                             Dialogs
 *          original work by Pritpal Bedi in WVTUTILS.C
 */

HB_FUNC(WVW_CREATEDIALOGDYNAMIC)
{
  auto pFirst = hb_param(3, Harbour::Item::ANY);
  PHB_ITEM pFunc = nullptr;
  PHB_DYNS pExecSym;
  auto p = hb_getWvwData();
  HWND hDlg = nullptr;
  int iIndex;
  int iType = 0;
  auto iResource = hb_parni(4);

  /* check if we still have room for a new dialog */

  for (iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++) {
    if (p->s_sApp->hDlgModeless[iIndex] == nullptr) {
      break;
    }
  }

  if (iIndex >= WVW_DLGML_MAX) {
    /* no more room */
    hb_retnl(static_cast<ULONG>(NULL));
    return;
  }

  if (pFirst->isBlock()) {
    /* pFunc is pointing to stored code block (later) */
    pFunc = hb_itemNew(pFirst);
    iType = 2;
  }
  // else if( HB_IS_STRING(pFirst) == Harbour::Item::STRING )
  else if (pFirst->isString()) {
    pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
    if (pExecSym) {
      pFunc = reinterpret_cast<PHB_ITEM>(pExecSym);
    }
    iType = 1;
  }

  {
    if (HB_ISNUM(3)) {
      hDlg = CreateDialogIndirect(
          hb_getWvwData()->hInstance, reinterpret_cast<LPDLGTEMPLATE>(const_cast<char *>(hb_parc(1))),
          hb_parl(2) ? p->s_pWindows[0]->hWnd : nullptr, reinterpret_cast<DLGPROC>(hb_parnl(3)));
    } else {
      switch (iResource) {
      case 0:
        hDlg = CreateDialog(hb_getWvwData()->hInstance, hb_parc(1), hb_parl(2) ? p->s_pWindows[0]->hWnd : nullptr,
                            static_cast<DLGPROC>(hb_gt_wvwDlgProcMLess));
        break;

      case 1:
        hDlg = CreateDialog(hb_getWvwData()->hInstance, MAKEINTRESOURCE(static_cast<WORD>(hb_parni(1))),
                            hb_parl(2) ? p->s_pWindows[0]->hWnd : nullptr, static_cast<DLGPROC>(hb_gt_wvwDlgProcMLess));
        break;

      case 2:
        hDlg = CreateDialogIndirect(
            hb_getWvwData()->hInstance, reinterpret_cast<LPDLGTEMPLATE>(const_cast<char *>(hb_parc(1))),
            hb_parl(2) ? p->s_pWindows[0]->hWnd : nullptr, static_cast<DLGPROC>(hb_gt_wvwDlgProcMLess));
        break;
      }
    }

    if (hDlg) {
      p->s_sApp->hDlgModeless[iIndex] = hDlg;
      if (pFunc) {
        /* if codeblock, store the codeblock and lock it there */
        if (pFirst->isBlock()) {
          p->s_sApp->pcbFunc[iIndex] = pFunc;
        }

        p->s_sApp->pFunc[iIndex] = pFunc;
        p->s_sApp->iType[iIndex] = iType;
      } else {
        p->s_sApp->pFunc[iIndex] = nullptr;
        p->s_sApp->iType[iIndex] = 0;
      }
      SendMessage(hDlg, WM_INITDIALOG, 0, 0);
    } else {
      if (iType == 2 && pFunc) {
        hb_itemRelease(pFunc);
      }

      p->s_sApp->hDlgModeless[iIndex] = nullptr;
    }
  }

  hb_retnl(reinterpret_cast<ULONG>(hDlg));
}

HB_FUNC(WVW_CREATEDIALOGMODAL)
{
  auto pFirst = hb_param(3, Harbour::Item::ANY);
  PHB_ITEM pFunc = nullptr;
  PHB_DYNS pExecSym;
  auto p = hb_getWvwData();
  int iIndex;
  auto iResource = hb_parni(4);
  int iResult = 0;
  HWND hParent = HB_ISNIL(5) ? p->s_pWindows[0]->hWnd : reinterpret_cast<HWND>(HB_PARHANDLE(5));

  /* check if we still have room for a new dialog */
  for (iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++) {
    if (p->s_sApp->hDlgModal[iIndex] == nullptr) {
      break;
    }
  }

  if (iIndex >= WVW_DLGMD_MAX) {
    /* no more room */
    hb_retni(0);
    return;
  }

  if (pFirst->isBlock()) {
    /* pFunc is pointing to stored code block (later) */

    p->s_sApp->pcbFuncModal[iIndex] = hb_itemNew(pFirst);

    pFunc = p->s_sApp->pcbFuncModal[iIndex];
    p->s_sApp->pFuncModal[iIndex] = pFunc;
    p->s_sApp->iTypeModal[iIndex] = 2;
  }
  // else if( HB_IS_STRING(pFirst) == Harbour::Item::STRING )
  else if (pFirst->isString()) {
    pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
    if (pExecSym) {
      pFunc = reinterpret_cast<PHB_ITEM>(pExecSym);
    }
    p->s_sApp->pFuncModal[iIndex] = pFunc;
    p->s_sApp->iTypeModal[iIndex] = 1;
  }

  switch (iResource) {
  case 0:
    iResult =
        DialogBoxParam(hb_getWvwData()->hInstance, hb_parc(1), hParent, static_cast<DLGPROC>(hb_gt_wvwDlgProcModal),
                       static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
    break;

  case 1:
    iResult = DialogBoxParam(hb_getWvwData()->hInstance, MAKEINTRESOURCE(static_cast<WORD>(hb_parni(1))), hParent,
                             static_cast<DLGPROC>(hb_gt_wvwDlgProcModal),
                             static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
    break;

  case 2:
    iResult = DialogBoxIndirectParam(
        hb_getWvwData()->hInstance, reinterpret_cast<LPDLGTEMPLATE>(const_cast<char *>(hb_parc(1))), hParent,
        static_cast<DLGPROC>(hb_gt_wvwDlgProcModal), static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
    break;
  }

  hb_retni(iResult);
}

/* removed from GTWVT, so we remove it from here also. I really don;t like doing it... */
HB_FUNC(WVW_DELETEOBJECT)
{
  hb_retl(DeleteObject(reinterpret_cast<HGDIOBJ>(HB_PARHANDLE(1))));
}

HB_FUNC(WVW_SETONTOP)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  RECT rect{};
  GetWindowRect(pWindowData->hWnd, &rect);
  hb_retl(SetWindowPos(pWindowData->hWnd, HWND_TOPMOST, rect.left, rect.top, 0, 0,
                       SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE));
}

HB_FUNC(WVW_SETASNORMAL)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  RECT rect{};
  GetWindowRect(pWindowData->hWnd, &rect);
  hb_retl(SetWindowPos(pWindowData->hWnd, HWND_NOTOPMOST, rect.left, rect.top, 0, 0,
                       SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE));
}

/*                                                                   */
/*   aScr := wvw_SaveScreen( nWinNum, nTop, nLeft, nBottom, nRight ) */
/*                                                                   */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC(WVW_SAVESCREEN)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  POINT xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  int iTop = xy.y;
  int iLeft = xy.x;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
  int iBottom = xy.y - 1;
  int iRight = xy.x - 1;

  int iWidth = iRight - iLeft + 1;
  int iHeight = iBottom - iTop + 1;

  auto hBmp = CreateCompatibleBitmap(pWindowData->hdc, iWidth, iHeight);

  auto oldBmp = static_cast<HBITMAP>(SelectObject(pWindowData->hCompDC, hBmp));
  BitBlt(pWindowData->hCompDC, 0, 0, iWidth, iHeight, pWindowData->hdc, iLeft, iTop, SRCCOPY);
  SelectObject(pWindowData->hCompDC, oldBmp);

  auto info = hb_itemArrayNew(3);
  hb_arraySetNI(info, 1, iWidth);
  hb_arraySetNI(info, 2, iHeight);
  hb_arraySetNInt(info, 3, reinterpret_cast<HB_PTRUINT>(hBmp));
  hb_itemReturnRelease(info);
}

/*                                                                     */
/*   wvw_RestScreen( nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP )*/
/*                                                                     */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC(WVW_RESTSCREEN)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  BOOL bResult = FALSE;
  BOOL bDoNotDestroyBMP = HB_ISNIL(7) ? FALSE : hb_parl(7);
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  POINT xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  int iTop = xy.y;
  int iLeft = xy.x;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
  int iBottom = xy.y - 1;
  int iRight = xy.x - 1;

  int iWidth = iRight - iLeft + 1;
  int iHeight = iBottom - iTop + 1;

  auto hBmp = static_cast<HBITMAP>(SelectObject(pWindowData->hCompDC, reinterpret_cast<HBITMAP>(hb_parvnl(6, 3))));
  if (hBmp) {
    if ((iWidth == hb_parvni(6, 1)) && (iHeight == hb_parvni(6, 2))) {
      if (BitBlt(pWindowData->hdc, iLeft, iTop, iWidth, iHeight, pWindowData->hCompDC, 0, 0, SRCCOPY)) {
        bResult = TRUE;
      }
    } else if (StretchBlt(pWindowData->hdc, iLeft, iTop, iWidth, iHeight, pWindowData->hCompDC, 0, 0, hb_parvni(6, 1),
                          hb_parvni(6, 2), SRCCOPY)) {
      bResult = TRUE;
    }
  }

  SelectObject(pWindowData->hCompDC, hBmp);

  if (!bDoNotDestroyBMP) {
    DeleteObject(reinterpret_cast<HBITMAP>(hb_parvnl(6, 3)));
  }

  hb_retl(bResult);
}

/*                                                                     */
/* wvw_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,*/
/*                 lStrikeout, nCharSet, nQuality, nEscapement )            */
/*                                                                          */
HB_FUNC(WVW_CREATEFONT)
{
  auto p = hb_getWvwData();
  UINT usWinNum = p->s_usNumWindows - 1;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  LOGFONT logfont;
  logfont.lfEscapement = HB_ISNIL(10) ? 0 : (hb_parni(10) * 10);
  logfont.lfOrientation = 0;
  logfont.lfWeight = HB_ISNIL(4) ? 0 : hb_parni(4);
  logfont.lfItalic = HB_ISNIL(5) ? 0 : static_cast<BYTE>(hb_parl(5));
  logfont.lfUnderline = HB_ISNIL(6) ? 0 : static_cast<BYTE>(hb_parl(6));
  logfont.lfStrikeOut = HB_ISNIL(7) ? 0 : static_cast<BYTE>(hb_parl(7));
  logfont.lfCharSet = HB_ISNIL(8) ? static_cast<BYTE>(pWindowData->CodePage) : static_cast<BYTE>(hb_parni(8));
  logfont.lfOutPrecision = 0;
  logfont.lfClipPrecision = 0;
  logfont.lfQuality = HB_ISNIL(9) ? static_cast<BYTE>(DEFAULT_QUALITY) : static_cast<BYTE>(hb_parni(9));
  logfont.lfPitchAndFamily = FF_DONTCARE;
  logfont.lfHeight = HB_ISNIL(2) ? pWindowData->fontHeight : hb_parni(2);
  logfont.lfWidth =
      HB_ISNIL(3) ? (pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth) : hb_parni(3);

  strcpy(logfont.lfFaceName, HB_ISNIL(1) ? pWindowData->fontFace : hb_parcx(1));

  auto hFont = CreateFontIndirect(&logfont);
  if (hFont) {
    hb_retnl(reinterpret_cast<ULONG>(hFont));
  } else {
    hb_retnl(0);
  }
}

#if 0
HB_FUNC(WVW_GETKEYSTATE) // TODO: deprecated (using waGetKeyState from WinApi library)
{
   hb_retni(GetKeyState(hb_parni(1)));
}
#endif

HB_FUNC_TRANSLATE(WVW_GETKEYSTATE, WAGETKEYSTATE)

HB_FUNC(WVW_LOWORD)
{
  hb_retni(static_cast<int>(hb_parnl(1) & 0xFFFF));
}

HB_FUNC(WVW_HIWORD)
{
  hb_retni(static_cast<int>((hb_parnl(1) >> 16) & 0xFFFF));
}
