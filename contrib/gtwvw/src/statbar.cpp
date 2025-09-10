/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW statusbar functions
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

/* wvw_sbCreate( [nWinNum] )
 * create status bar for window nWinNum, with one part.
 * returns handle to status bar of windows nWinNum
 * returns 0 if failed, eg. if there is already a status bar for this window
 */
HB_FUNC(WVW_SBCREATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndParent;
  HWND hWndSB;
  auto pData = hb_getWvwData();
  int ptArray[WVW_MAX_STATUS_PARTS];

  if (!(pWindowData->hStatusBar == nullptr)) {
    hb_retnl(0);
    return;
  }

  hWndParent = pWindowData->hWnd;
  hWndSB = CreateStatusWindow(WS_CHILD | WS_VISIBLE | WS_BORDER | SBT_TOOLTIPS, nullptr, hWndParent,
                              WVW_ID_BASE_STATUSBAR + usWinNum);
  if (hWndSB) {

    RECT rSB{};
    if (pWindowData->hSBfont == nullptr) {
      pWindowData->hSBfont = CreateFontIndirect(&pData->s_lfSB);
    }
    if (GetClientRect(hWndSB, &rSB)) {
      pWindowData->usSBHeight = static_cast<USHORT>(rSB.bottom);
    }
    pWindowData->hStatusBar = hWndSB;

    hb_gt_wvwResetWindow(usWinNum);

    ptArray[0] = rSB.right;
    SendMessage(hWndSB, WM_SETFONT, reinterpret_cast<WPARAM>(pWindowData->hSBfont), static_cast<LPARAM>(TRUE));

    SendMessage(hWndSB, SB_SETPARTS, 1, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)));
  }

  hb_retnl(reinterpret_cast<LONG>(hWndSB));
}

/*wvw_sbDestroy( [nWinNum] )
 * destroy status bar for window nWinNum
 */
HB_FUNC(WVW_SBDESTROY)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (!(pWindowData->hStatusBar == nullptr)) {
    if (pWindowData->hSBfont) {
      DeleteObject(static_cast<HFONT>(pWindowData->hSBfont));
      pWindowData->hSBfont = nullptr;
    }
    DestroyWindow(pWindowData->hStatusBar);
    pWindowData->hStatusBar = nullptr;
    pWindowData->bSBPaint = FALSE;
    pWindowData->usSBHeight = 0;

    hb_gt_wvwResetWindow(usWinNum);
  }
}

/* wvw_sbAddPart(nWinNum, cMaxText, nWidth, nStyle, lResetParts, [cIcon , cToolTip])
 *  ps.
 *  lResetParts==.t. :: remove all previously created parts
 *  nStyle: 0 (default), 0x0200 (SBT_POPOUT), 0x0100 (SBT_NOBORDERS)
 *  nWidth: expected width in pixels
 *  NOTE: if cMaxText is passed, nWidth is ignored. width of cMaxText will be used instead
 *  NOTE: the leftmost part will eventually have width of remaining spaces
 *  NOTE: cIcon and cToolTip does not work currently
 *
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC(WVW_SBADDPART)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndSB;
  int ptArray[WVW_MAX_STATUS_PARTS];
  int numOfParts;
  RECT rSB{};
  WORD displayFlags;
  HICON hIcon;
  BOOL lResetParts;
  USHORT usWidth;

  hWndSB = pWindowData->hStatusBar;
  if (hWndSB == nullptr) {
    hb_retnl(0);
    return;
  }

  displayFlags = HB_ISNIL(4) ? 0 : static_cast<WORD>(hb_parnl(4));
  lResetParts = !HB_ISNIL(5) && hb_parl(5);
  usWidth = HB_ISNIL(3) || hb_parni(3) <= 0 ? 5 * WVW_SPACE_BETWEEN_PARTS : static_cast<USHORT>(hb_parni(3));

  if (HB_ISCHAR(2)) {
    auto hDCSB = GetDC(hWndSB);
    SIZE size{};

    auto hFont = reinterpret_cast<HFONT>(SendMessage(hWndSB, WM_GETFONT, 0, 0));
    auto hOldFont = static_cast<HFONT>(SelectObject(hDCSB, hFont));

    if (GetTextExtentPoint32(hDCSB, hb_parcx(2), hb_parclen(2) + 1, &size)) {
      usWidth = static_cast<USHORT>(size.cx);
    }

    SelectObject(hDCSB, hOldFont);

    ReleaseDC(hWndSB, hDCSB);
  }

  if (!lResetParts) {
    numOfParts =
        SendMessage(hWndSB, SB_GETPARTS, WVW_MAX_STATUS_PARTS, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)));
  } else {
    numOfParts = 0;
  }
  numOfParts++;

  GetClientRect(hWndSB, &rSB);

  ptArray[numOfParts - 1] = rSB.right;
  if (!lResetParts) {
    for (auto n = 0; n < numOfParts - 1; n++) {
      ptArray[n] -= (usWidth + WVW_SPACE_BETWEEN_PARTS);
    }
  }

  SendMessage(hWndSB, SB_SETPARTS, numOfParts, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)));

  if (!HB_ISNIL(6)) {
    int cy = rSB.bottom - rSB.top - 4;
    int cx = cy;

    hIcon = static_cast<HICON>(LoadImage(0, hb_parcx(6), IMAGE_ICON, cx, cy,
                                         LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT | LR_DEFAULTSIZE));

    if (hIcon == nullptr) {
      hIcon = static_cast<HICON>(
          LoadImage(GetModuleHandle(nullptr), hb_parcx(6), IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR | LR_DEFAULTSIZE));
    }

    if (!(hIcon == nullptr)) {
      SendMessage(hWndSB, SB_SETICON, static_cast<WPARAM>(numOfParts) - 1, reinterpret_cast<LPARAM>(hIcon));
    }
  }

  SendMessage(hWndSB, SB_SETTEXT, (numOfParts - 1) | displayFlags, static_cast<LPARAM>(NULL));
  if (!HB_ISNIL(7)) {
    SendMessage(hWndSB, SB_SETTIPTEXT, static_cast<WPARAM>(numOfParts - 1), reinterpret_cast<LPARAM>(hb_parcx(7)));
  }

  hb_retni(numOfParts);
}

/*wvw_sbRefresh(nWinNum)
 * reinitialize StatusBar's parts, eg. after window resize
 * TODO: do it automatically, after hb_gt_wvwResetWindowSize()
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC(WVW_SBREFRESH)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndSB;
  int ptArray[WVW_MAX_STATUS_PARTS];
  int numOfParts;
  int iDiff;
  RECT rSB{};

  hWndSB = pWindowData->hStatusBar;
  if (hWndSB == nullptr) {
    hb_retnl(0);
    return;
  }

  numOfParts =
      SendMessage(hWndSB, SB_GETPARTS, WVW_MAX_STATUS_PARTS, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)));
  if (numOfParts == 0) {
    hb_retnl(0);
    return;
  }

  GetClientRect(hWndSB, &rSB);
  iDiff = rSB.right - ptArray[numOfParts - 1];

  for (auto n = 0; n <= numOfParts - 1; n++) {
    ptArray[n] += iDiff;
  }

  SendMessage(hWndSB, SB_SETPARTS, numOfParts, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)));

  hb_retni(numOfParts);
}

/*wvw_sbSetText([nWinNum], [nPart], cText)
 * Set Text of status bar's part #npart
 */
HB_FUNC(WVW_SBSETTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iPart = HB_ISNIL(2) ? 1 : hb_parni(2);

  if (!HB_ISNIL(4)) {
    if (HB_ISCHAR(4)) {
      pWindowData->cSBColorForeground = strtol(hb_parc(4), nullptr, 10);
    } else {
      pWindowData->cSBColorForeground = hb_parnl(4);
    }
  }

  if (!HB_ISNIL(5)) {
    if (HB_ISCHAR(5)) {
      pWindowData->cSBColorBackground = strtol(hb_parc(5), nullptr, 10);
    } else {
      pWindowData->cSBColorBackground = hb_parnl(5);
    }
  }

  if ((iPart == 0) && ((pWindowData->cSBColorForeground) || (pWindowData->cSBColorBackground))) {
    pWindowData->bSBPaint = TRUE;
    SendMessage(pWindowData->hStatusBar, SB_SETTEXT, SBT_OWNERDRAW, reinterpret_cast<LPARAM>(hb_parcx(3)));
    hb_gt_wvwProcessMessages(pWindowData);
  } else {
    SendMessage(pWindowData->hStatusBar, SB_SETTEXT, iPart, reinterpret_cast<LPARAM>(hb_parcx(3)));
  }
}

/*wvw_sbGetText([nWinNum], [nPart])
 * Get Text of status bar's part #npart
 */
HB_FUNC(WVW_SBGETTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  int iPart = HB_ISNIL(2) ? 1 : hb_parni(2);
  char cString[1024] = "";

  SendMessage(pWindowData->hStatusBar, SB_GETTEXT, static_cast<WPARAM>(iPart), reinterpret_cast<LPARAM>(cString));
  hb_retc(cString);
}

/*wvw_sbGetParts([nWinNum])
 * Get number of parts in statusbar of window nWinNum
 */
HB_FUNC(WVW_SBGETPARTS)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto numOfParts = static_cast<int>(SendMessage(pWindowData->hStatusBar, SB_GETPARTS, WVW_MAX_STATUS_PARTS, 0));

  hb_retni(numOfParts);
}

/*wvw_sbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, lItalic, lUnderline, lStrikeout
 *
 */
HB_FUNC(WVW_SBSETFONT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pData = hb_getWvwData();

  BOOL retval = TRUE;

  pData->s_lfSB.lfHeight = HB_ISNIL(3) ? pWindowData->fontHeight - 2 : hb_parnl(3);
  pData->s_lfSB.lfWidth = HB_ISNIL(4) ? pData->s_lfSB.lfWidth : hb_parni(4);
  pData->s_lfSB.lfEscapement = 0;
  pData->s_lfSB.lfOrientation = 0;
  pData->s_lfSB.lfWeight = HB_ISNIL(5) ? pData->s_lfSB.lfWeight : hb_parni(5);
  pData->s_lfSB.lfItalic = HB_ISNIL(7) ? pData->s_lfSB.lfItalic : static_cast<BYTE>(hb_parl(7));
  pData->s_lfSB.lfUnderline = HB_ISNIL(8) ? pData->s_lfSB.lfUnderline : static_cast<BYTE>(hb_parl(8));
  pData->s_lfSB.lfStrikeOut = HB_ISNIL(9) ? pData->s_lfSB.lfStrikeOut : static_cast<BYTE>(hb_parl(9));
  pData->s_lfSB.lfCharSet = DEFAULT_CHARSET;

  pData->s_lfSB.lfQuality = HB_ISNIL(6) ? pData->s_lfSB.lfQuality : static_cast<BYTE>(hb_parni(6));
  pData->s_lfSB.lfPitchAndFamily = FF_DONTCARE;
  if (HB_ISCHAR(2)) {
    strcpy(pData->s_lfSB.lfFaceName, hb_parcx(2));
  }

  if (pWindowData->hSBfont) {
    HFONT hOldFont = pWindowData->hSBfont;
    auto hFont = CreateFontIndirect(&pData->s_lfSB);
    if (hFont) {
      pWindowData->hSBfont = hFont;
      DeleteObject(static_cast<HFONT>(hOldFont));
    } else {
      retval = FALSE;
    }
  }

  hb_retl(retval);
}

/*wvw_xbCreate( [nWinNum], nStyle, nTop, nLeft, nLength, bBlock, aOffset)
 * create scroll bar for window nWinNum
 * nStyle: SBS_HORZ (0)=horizontal, SBS_VERT (1)=vertical
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nLength: length of scrollbar (in character unit)
 * NOTES: width of scrollbar (in character unit)
 *            horiz: defaults to one character height
 *            verti: defaults to one character _height_ too (!)
 *       use aOffset to adjust the dimension
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of scroll bar.
 *         defaults for vertical scroll bar: {0,+3,0,0}
 *         defaults for horiz scroll bar: {+3-linespacing,0,0,0}
 *         NOTES: these defaults are meant to make room for other common
 *                GUI elements like raised/recessed lines.
 *
 * bBlock:  codeblock to execute on every WM_VSCROLL/WM_HSCROLL event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nXBid  : scrollbar id
 *         nXBmsg : scrollbar message, ie. one of these:
 *         nXBpos : scrollthumb position (only if message==SB_THUMB...)
 *         the "must be handled" messages:
 *             SB_LINEUP/SB_LINELEFT     0: up/left button clicked
 *             SB_LINEDOWN/SB_LINERIGHT  1: down/right button clicked
 *             SB_PAGEUP/SB_PAGELEFT     2: upper/left shaft clicked
 *             SB_PAGEDOWN/SB_PAGERIGHT  3: lower/right shaft clicked
 *         the "may not be handled" messages:
 *             SB_THUMBPOSITION          4: scroll thumb is released at position nXBpos
 *             SB_THUMBTRACK             5: scroll thumb is being dragged at position nXBpos
 *             SB_ENDSCROLL              8
 *
 * returns control id of newly created scroll bar of windows nWinNum
 * returns 0 if failed
 *
 * example:
 * wvw_xbCreate( , 1, 10, 70, 12)
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     dimensions using default ones.
 *     buttons/parts behaviour using default ones.
 *
 * wvw_xbCreate( , 1, 10, 70, 12, {0, +5, 0, +5} )
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     left and right coordinate is shifted 5 pixels to the right.
 *     buttons/parts behaviour using default ones.
 *
 * NOTES:
 * ScrollRange is always 0 - 100.
 * Initial ScrollPos is 0
 */

HB_FUNC(WVW_XBCREATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndParent = pWindowData->hWnd;
  HWND hWndXB;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  auto iStyle = static_cast<int>(!HB_ISNUM(2) ? -1 : hb_parni(2));
  UINT uiXBid;
  auto usTop = static_cast<USHORT>(hb_parni(3));
  auto usLeft = static_cast<USHORT>(hb_parni(4));
  USHORT usBottom;
  USHORT usRight;

  if (iStyle < SBS_HORZ || iStyle > SBS_VERT || !HB_ISBLOCK(6)) {
    hb_retnl(0);
    return;
  }

  if (iStyle == SBS_VERT) {
    usBottom = usTop + static_cast<USHORT>(hb_parni(5)) - 1;
    usRight = usLeft;

    iOffTop = !HB_ISNIL(7) ? hb_parvni(7, 1) : 0;
    iOffLeft = !HB_ISNIL(7) ? hb_parvni(7, 2) : +3;
    iOffBottom = !HB_ISNIL(7) ? hb_parvni(7, 3) : 0;
    iOffRight = !HB_ISNIL(7) ? hb_parvni(7, 4) : 0;
  } else {
    usRight = usLeft + static_cast<USHORT>(hb_parni(5)) - 1;
    usBottom = usTop;

    iOffTop = !HB_ISNIL(7) ? hb_parvni(7, 1) : +3 - pWindowData->byLineSpacing;
    iOffLeft = !HB_ISNIL(7) ? hb_parvni(7, 2) : 0;
    iOffBottom = !HB_ISNIL(7) ? hb_parvni(7, 3) : 0;
    iOffRight = !HB_ISNIL(7) ? hb_parvni(7, 4) : 0;
  }

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  if (iStyle == SBS_VERT) {
    iBottom = xy.y - 1 + iOffBottom;
    iRight = iLeft + pWindowData->PTEXTSIZE.y - 1 + iOffRight;
  } else {
    iRight = xy.x - 1 + iOffRight;
    iBottom = iTop + pWindowData->PTEXTSIZE.y - 1 + iOffBottom;
  }

  uiXBid = LastControlId(usWinNum, WVW_CONTROL_SCROLLBAR);
  if (uiXBid == 0) {
    uiXBid = WVW_ID_BASE_SCROLLBAR;
  } else {
    uiXBid++;
  }

  hWndXB = CreateWindowEx(0L,                                                 /* no extended styles */
                          "SCROLLBAR",                                        /* scroll bar control class */
                          static_cast<LPSTR>(nullptr),                        /* text for window title bar */
                          WS_CHILD | WS_VISIBLE | static_cast<DWORD>(iStyle), /* scroll bar styles */
                          iLeft,                                              /* horizontal position */
                          iTop,                                               /* vertical position */
                          iRight - iLeft + 1,                                 /* width of the scroll bar */
                          iBottom - iTop + 1,                                 /* height */
                          hWndParent,                                         /* handle to main window */
                          reinterpret_cast<HMENU>(uiXBid),                    /* id for this scroll bar control */
                          hb_getWvwData()->hInstance,                         /* instance owning this window */
                          static_cast<LPVOID>(nullptr));                      /* pointer not needed */

  if (hWndXB) {

    RECT rXB{};
    RECT rOffXB{};

    rXB.top = usTop;
    rXB.left = usLeft;
    rXB.bottom = usBottom;
    rXB.right = usRight;
    rOffXB.top = iOffTop;
    rOffXB.left = iOffLeft;
    rOffXB.bottom = iOffBottom;
    rOffXB.right = iOffRight;

    SetScrollRange(hWndXB, SB_CTL, 0, 99, FALSE);
    SetScrollPos(hWndXB, SB_CTL, 0, TRUE);

    AddControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, uiXBid,
                     static_cast<PHB_ITEM>(hb_param(6, Harbour::Item::BLOCK)), rXB, rOffXB, static_cast<byte>(iStyle));

    auto OldProc =
        reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWndXB, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(hb_gt_wvwXBProc)));

    StoreControlProc(usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, OldProc);

    hb_retnl(static_cast<LONG>(uiXBid));
  } else {
    hb_retnl(0);
  }
}

/*wvw_xbDestroy( [nWinNum], nXBid )
 * destroy scrollbar nXBid for window nWinNum
 */
HB_FUNC(WVW_XBDESTROY)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto uiXBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  CONTROL_DATA *pcd = pWindowData->pcdCtrlList;
  CONTROL_DATA *pcdPrev = nullptr;

  while (pcd) {
    if (pcd->byCtrlClass == WVW_CONTROL_SCROLLBAR && pcd->uiCtrlid == uiXBid) {
      break;
    }

    pcdPrev = pcd;
    pcd = pcd->pNext;
  }
  if (pcd == nullptr) {
    return;
  }

  DestroyWindow(pcd->hWndCtrl);

  if (pcdPrev == nullptr) {
    pWindowData->pcdCtrlList = pcd->pNext;
  } else {
    pcdPrev->pNext = pcd->pNext;
  }

  if (pcd->phiCodeBlock) {
    hb_itemRelease(pcd->phiCodeBlock);
  }

  hb_xfree(pcd);
}

/*wvw_xbUpdate(nWinNum, XBid, [nPos], [nPageSize], [nMin], [nMax])
 * update scrollbar data and its display
 * nPos, nPageSize, nMin, nMax are optional.
 * however, both nMin & nMax must be supplied, or not at all.
 * returns current position of scroll thumb.
 * returns -1 if update failed.
 */
HB_FUNC(WVW_XBUPDATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto uiXBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  byte bStyle;
  auto hWndXB = FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle);
  auto iPos = static_cast<int>(HB_ISNIL(3) ? 0 : hb_parni(3));  // TODO: use hb_parnidef
  auto iPage = static_cast<int>(HB_ISNIL(4) ? 0 : hb_parni(4)); // TODO: use hb_parnidef
  auto iMin = static_cast<int>(HB_ISNIL(5) ? 0 : hb_parni(5));  // TODO: use hb_parnidef
  auto iMax = static_cast<int>(HB_ISNIL(6) ? 0 : hb_parni(6));  // TODO: use hb_parnidef
  SCROLLINFO si;
  int iRetval;
  UINT fMask = SIF_DISABLENOSCROLL;

  if (uiXBid == 0 || hWndXB == nullptr || iPage < 0) {
    hb_retni(-1);
    return;
  }

  if (!HB_ISNIL(3)) {
    fMask = fMask | SIF_POS;
  }
  if (!HB_ISNIL(4)) {
    fMask = fMask | SIF_PAGE;
  }
  if (!HB_ISNIL(5) && !HB_ISNIL(6)) {
    fMask = fMask | SIF_RANGE;
  }

  si.cbSize = sizeof(si);
  si.fMask = fMask;
  si.nMin = iMin;
  si.nMax = iMax;
  si.nPage = static_cast<UINT>(iPage);
  si.nPos = iPos;
  iRetval = SetScrollInfo(hWndXB, SB_CTL, static_cast<LPCSCROLLINFO>(&si), TRUE);

  hb_retni(iRetval);
}

/* wvw_xbInfo( [nWinNum], XBid )
 * return an array {nMin, nMax, nPageSize, nPos, nTrackPos }
 * return an empty array {} if invalid parameter passed.
 */
HB_FUNC(WVW_XBINFO)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  PHB_ITEM aInfo;
  SCROLLINFO si;

  auto uiXBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  byte bStyle;
  auto hWndXB = FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle);

  if (uiXBid == 0 || hWndXB == nullptr) {
    aInfo = hb_itemArrayNew(0);
    hb_itemReturn(aInfo);
    hb_itemRelease(aInfo);
    return;
  }

  si.cbSize = sizeof(si);
  si.fMask = SIF_ALL;

  if (!GetScrollInfo(hWndXB, SB_CTL, &si)) {
    aInfo = hb_itemArrayNew(0);
    hb_itemReturn(aInfo);
    hb_itemRelease(aInfo);
    return;
  }

  aInfo = hb_itemArrayNew(5);
  hb_arraySetNL(aInfo, 1, si.nMin);
  hb_arraySetNL(aInfo, 2, si.nMax);
  hb_arraySetNL(aInfo, 3, si.nPage);
  hb_arraySetNL(aInfo, 4, si.nPos);
  hb_arraySetNL(aInfo, 5, si.nTrackPos);

  hb_itemReturnRelease(aInfo);
}

/* wvw_xbEnable( [nWinNum], nXBid, nFlags )
 *  enable/disable scrollbar nXBid in window nWinNum (default to topmost window)
 *  nFlags: ESB_ENABLE_BOTH                    0: enable both arrows
 *        ESB_DISABLE_LEFT/ESB_DISABLE_UP    1: disable left/up arrow
 *        ESB_DISABLE_RIGHT/ESB_DISABLE_DOWN 2: disable right/down arrow
 *        ESB_DISABLE_BOTH                   3: disable both arrow
 * returns .t. if successful
 */
HB_FUNC(WVW_XBENABLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto uiXBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  auto uiFlags = static_cast<UINT>(HB_ISNIL(3) ? 0 : hb_parni(3));
  byte bStyle;
  auto hWndXB =
      static_cast<HWND>(uiXBid == 0 ? nullptr : FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle));

  if (uiXBid == 0 || hWndXB == nullptr || uiFlags > ESB_DISABLE_BOTH) {
    hb_retl(false);
    return;
  }

  hb_retl(EnableScrollBar(hWndXB, SB_CTL, uiFlags));
}

/* wvw_xbShow( [nWinNum], nXBid, lShow )
 *  show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *  nXBid is the handle of the scrolbar
 *  lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 * returns .t. if successful
 */
HB_FUNC(WVW_XBSHOW)
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

/* SCROLLBAR ends                                                    */
