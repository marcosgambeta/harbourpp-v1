/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW edit functions
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

/* wvw_ebCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, bBlock, ;
 *                         lMultiline, nMoreStyle, nMaxChar, nReserved, aOffset)
 * create editbox for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit)
 * nRight: col of bottom/right corner (in character unit)
 * cText: initial text to display, default = ""
 *      WARNING!! must be of "C" typed!
 * bBlock: codeblock to execute on these events:
 *         event=EN_SETFOCUS(...): editbox got focus
 *         event=EN_KILLFOCUS(...): editbox lose focus
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nEBid  : editbox id
 *         nType  : event type (EN_SETFOCUS/EN_KILLFOCUS supported)
 *
 *
 * lMultiline: .f. :: single line editbox (default)
 *            .t. :: multi line editbox
 * mapped internally into two types of editbox:
 *         WVW_EB_SINGLELINE (1): single line editbox
 *         WVW_EB_MULTILINE (2): multi line editbox
 *         default is WVW_EB_SINGLELINE (1)
 *
 * nMoreStyle: more style that will be added to the predefined style
 *            some examples: ES_PASSWORD, ES_READONLY
 *
 * nMaxChar: (FUTURE FEATURE) maximum number of chars allowed
 *
 * nReserved: reserved for future use
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of editbox.
 *         defaults: {-2,-2,+2,+2}
 *
 * returns control id of newly created editbox of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC(WVW_EBCREATE)
{
  HANDLE hInstance = nullptr;
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  HWND hWndParent = pWindowData->hWnd;
  HWND hWndEB;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  UINT uiEBid;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  LPTSTR lpszText = const_cast<LPTSTR>(hb_parcx(6));

  BOOL bMultiline = HB_ISLOG(8) ? hb_parl(8) : FALSE;
  auto bEBType = static_cast<BYTE>(bMultiline ? WVW_EB_MULTILINE : WVW_EB_SINGLELINE);

  auto dwMoreStyle = static_cast<DWORD>(HB_ISNUM(9) ? hb_parnl(9) : 0);

  auto usMaxChar = static_cast<USHORT>(HB_ISNUM(10) && hb_parni(10) > 0 ? hb_parni(10) : 0);

  DWORD dwStyle;
  auto pData = hb_getWvwData();

  if (pWindowData->hEBfont == nullptr)
  {
    pWindowData->hEBfont = CreateFontIndirect(&pData->s_lfEB);
    if (pWindowData->hEBfont == nullptr)
    {
      hb_retnl(0);
      return;
    }
  }

  iOffTop = !HB_ISNIL(12) ? hb_parvni(12, 1) : 0;
  iOffLeft = !HB_ISNIL(12) ? hb_parvni(12, 2) : 0;
  iOffBottom = !HB_ISNIL(12) ? hb_parvni(12, 3) : 0;
  iOffRight = !HB_ISNIL(12) ? hb_parvni(12, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode())
  {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  uiEBid = LastControlId(usWinNum, WVW_CONTROL_EDITBOX);
  if (uiEBid == 0)
  {
    uiEBid = WVW_ID_BASE_EDITBOX;
  }
  else
  {
    uiEBid++;
  }

  dwStyle = WS_BORDER | WS_GROUP | WS_TABSTOP | dwMoreStyle;

  if ((bEBType & WVW_EB_MULTILINE) == WVW_EB_MULTILINE)
  {
    dwStyle |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN | WS_BORDER | WS_VSCROLL;
  }
  else
  {
    dwStyle |= ES_AUTOHSCROLL;
  }

  if (pWindowData->CodePage == OEM_CHARSET)
  {
    dwStyle |= ES_OEMCONVERT;
  }

  hb_winmainArgGet(&hInstance, nullptr, nullptr);

  hWndEB =
      CreateWindowEx(0L, "EDIT", static_cast<LPSTR>(nullptr), WS_CHILD | WS_VISIBLE | static_cast<DWORD>(dwStyle),
                     iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, hWndParent, reinterpret_cast<HMENU>(uiEBid),
                     static_cast<HINSTANCE>(hInstance), static_cast<LPVOID>(nullptr));

  if (hWndEB)
  {
    RECT rXB{};
    RECT rOffXB{};
    /* USHORT i; */
    BOOL bFromOEM = (pWindowData->CodePage == OEM_CHARSET);

    if (bFromOEM)
    {
      auto ulLen = static_cast<ULONG>(strlen(lpszText));
      auto lpszTextANSI = static_cast<LPTSTR>(hb_xgrab(ulLen + 1));
      OemToChar(lpszText, lpszTextANSI);
      lpszText = lpszTextANSI;
    }

    SendMessage(static_cast<HWND>(hWndEB), WM_SETTEXT, 0, reinterpret_cast<LPARAM>(lpszText));

    if (bFromOEM)
    {
      hb_xfree(lpszText);
    }

    if (usMaxChar > 0)
    {
      SendMessage(static_cast<HWND>(hWndEB), EM_LIMITTEXT, static_cast<WPARAM>(usMaxChar), 0);
    }

    rXB.top = usTop;
    rXB.left = usLeft;
    rXB.bottom = usBottom;
    rXB.right = usRight;
    rOffXB.top = iOffTop;
    rOffXB.left = iOffLeft;
    rOffXB.bottom = iOffBottom;
    rOffXB.right = iOffRight;

    AddControlHandle(usWinNum, WVW_CONTROL_EDITBOX, hWndEB, uiEBid,
                     static_cast<PHB_ITEM>(hb_param(7, Harbour::Item::BLOCK)), rXB, rOffXB, static_cast<byte>(bEBType));

    auto OldProc =
        reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWndEB, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(hb_gt_wvwEBProc)));

    StoreControlProc(usWinNum, WVW_CONTROL_EDITBOX, hWndEB, OldProc);

    SendMessage(hWndEB, WM_SETFONT, reinterpret_cast<WPARAM>(pWindowData->hEBfont), static_cast<LPARAM>(TRUE));

    hb_retnl(static_cast<LONG>(uiEBid));
  }
  else
  {
    hb_retnl(0);
  }
}

/*wvw_ebDestroy( [nWinNum], nEBid )
 * destroy editbox nEBid for window nWinNum
 */
HB_FUNC(WVW_EBDESTROY)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto uiEBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  CONTROL_DATA *pcd = pWindowData->pcdCtrlList;
  auto pcdPrev = static_cast<CONTROL_DATA *>(nullptr);

  while (pcd)
  {
    if (pcd->byCtrlClass == WVW_CONTROL_EDITBOX && pcd->uiCtrlid == uiEBid)
    {
      break;
    }

    pcdPrev = pcd;
    pcd = pcd->pNext;
  }
  if (pcd == nullptr)
  {
    return;
  }

  DestroyWindow(pcd->hWndCtrl);

  if (pcdPrev == nullptr)
  {
    pWindowData->pcdCtrlList = pcd->pNext;
  }
  else
  {
    pcdPrev->pNext = pcd->pNext;
  }

  if (pcd->phiCodeBlock)
  {
    hb_itemRelease(pcd->phiCodeBlock);
  }

  hb_xfree(pcd);
}

/*wvw_ebSetFocus( [nWinNum], nEditId )
 * set the focus to editbox nEditId in window nWinNum
 */
HB_FUNC(WVW_EBSETFOCUS)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte bStyle;
  auto hWndEB = FindControlHandle(usWinNum, WVW_CONTROL_EDITBOX, uiCtrlId, &bStyle);

  if (hWndEB)
  {
    hb_retl(SetFocus(hWndEB) != nullptr);
  }
  else
  {
    hb_retl(false);
  }
}

/*wvw_ebIsFocused([nWinNum], nEditId)
 * returns .t. if the focus is on editbox nEditId in window nWinNum
 */
HB_FUNC(WVW_EBISFOCUSED)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte bStyle;
  auto hWndEB = FindControlHandle(usWinNum, WVW_CONTROL_EDITBOX, uiCtrlId, &bStyle);

  hb_retl(static_cast<HWND>(GetFocus()) == hWndEB);
}

/*wvw_ebEnable( [nWinNum], nEditId, [lEnable] )
 * enable/disable editbox nEditId on window nWinNum
 *(lEnable defaults to .t., ie. enabling the editbox)
 * return previous state of the editbox (TRUE:enabled FALSE:disabled)
 *(if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC(WVW_EBENABLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL bEnable = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte bStyle;
  auto hWndEB = FindControlHandle(usWinNum, WVW_CONTROL_EDITBOX, uiCtrlId, &bStyle);
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hWndEB)
  {
    hb_retl(EnableWindow(hWndEB, bEnable) == 0);

    if (!bEnable)
    {
      SetFocus(pWindowData->hWnd);
    }
  }
  else
  {
    hb_retl(false);
  }
}

/*wvw_ebEditable( [nWinNum], nEditId, [lEditable] )
 * get/set editability attribute from editbox nEditId on window nWinNum
 *(if lEditable is not specified, no change to editability)
 * return previous state of the editbox (TRUE:editable FALSE:not editable)
 *(if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC(WVW_EBEDITABLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL bEditable = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte bStyle;
  auto hWndEB = FindControlHandle(usWinNum, WVW_CONTROL_EDITBOX, uiCtrlId, &bStyle);

  if (hWndEB)
  {
    auto dwStyle = static_cast<DWORD>(GetWindowLong(hWndEB, GWL_STYLE));

    hb_retl(!((dwStyle & ES_READONLY) == ES_READONLY));

    if (!HB_ISNIL(3))
    {
      SendMessage(static_cast<HWND>(hWndEB), EM_SETREADONLY, static_cast<WPARAM>(!bEditable), 0);
    }
  }
  else
  {
    hb_retl(false);
  }
}

/*wvw_ebSetCodeblock( [nWinNum], nEBid, bBlock )
 * assign (new) codeblock bBlock to editbox nEBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC(WVW_EBSETCODEBLOCK)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto uiEBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  auto pData = hb_getWvwData();
  auto phiCodeBlock = hb_param(3, Harbour::Item::BLOCK);
  BOOL bOldSetting = pData->s_bRecurseCBlock;

  if (!phiCodeBlock || pcd == nullptr || pcd->bBusy)
  {
    hb_retl(false);
    return;
  }

  pData->s_bRecurseCBlock = FALSE;
  pcd->bBusy = TRUE;

  if (pcd->phiCodeBlock)
  {
    hb_itemRelease(pcd->phiCodeBlock);
  }

  pcd->phiCodeBlock = hb_itemNew(phiCodeBlock);

  pcd->bBusy = FALSE;
  pData->s_bRecurseCBlock = bOldSetting;

  hb_retl(true);
}

/* wvw_ebSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL editboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC(WVW_EBSETFONT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  BOOL retval = TRUE;
  auto pData = hb_getWvwData();

  pData->s_lfEB.lfHeight = HB_ISNIL(3) ? pWindowData->fontHeight - 2 : hb_parnl(3);
  pData->s_lfEB.lfWidth = HB_ISNIL(4) ? pData->s_lfEB.lfWidth : hb_parni(4);
  pData->s_lfEB.lfEscapement = 0;
  pData->s_lfEB.lfOrientation = 0;
  pData->s_lfEB.lfWeight = HB_ISNIL(5) ? pData->s_lfEB.lfWeight : hb_parni(5);
  pData->s_lfEB.lfItalic = HB_ISNIL(7) ? pData->s_lfEB.lfItalic : static_cast<BYTE>(hb_parl(7));
  pData->s_lfEB.lfUnderline = HB_ISNIL(8) ? pData->s_lfEB.lfUnderline : static_cast<BYTE>(hb_parl(8));
  pData->s_lfEB.lfStrikeOut = HB_ISNIL(9) ? pData->s_lfEB.lfStrikeOut : static_cast<BYTE>(hb_parl(9));
  pData->s_lfEB.lfCharSet = DEFAULT_CHARSET;

  pData->s_lfEB.lfQuality = HB_ISNIL(6) ? pData->s_lfEB.lfQuality : static_cast<BYTE>(hb_parni(6));
  pData->s_lfEB.lfPitchAndFamily = FF_DONTCARE;
  if (HB_ISCHAR(2))
  {
    strcpy(pData->s_lfEB.lfFaceName, hb_parcx(2));
  }

  if (pWindowData->hEBfont)
  {
    HFONT hOldFont = pWindowData->hEBfont;
    auto hFont = CreateFontIndirect(&pData->s_lfEB);
    if (hFont)
    {
      CONTROL_DATA *pcd = pWindowData->pcdCtrlList;

      while (pcd)
      {
        if ((pcd->byCtrlClass == WVW_CONTROL_EDITBOX) &&
            (reinterpret_cast<HFONT>(SendMessage(pcd->hWndCtrl, WM_GETFONT, 0, 0)) == hOldFont))
        {
          SendMessage(pcd->hWndCtrl, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
        }

        pcd = pcd->pNext;
      }

      pWindowData->hEBfont = hFont;
      DeleteObject(static_cast<HFONT>(hOldFont));
    }
    else
    {
      retval = FALSE;
    }
  }

  hb_retl(retval);
}

/*wvw_ebIsMultiline( [nWinNum], nEBid )
 * returns .t. if editbox nEBid in window nWinNum is multiline
 * otherwise .f.
 * Also returns .f. if nEBid not valid
 */
HB_FUNC(WVW_EBISMULTILINE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiEBid = hb_parni(2);
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  BOOL bMultiline;

  if (pcd == nullptr)
  {
    hb_retl(false);
    return;
  }

  bMultiline = ((pcd->bStyle & WVW_EB_MULTILINE) == WVW_EB_MULTILINE);
  hb_retl(bMultiline);
}

/* wvw_ebGetText( [nWinNum], nEBid,;
 *                          lSoftBreak )
 * returns current text from editbox nEBid in window nWinNum
 * lSoftBreak: Default is FALSE.
 *             insert soft line break character (CR+CR+LF) at wordwrap positions
 *             can be usefull to convert the text to MEMO format
 *             eg. converting editbox's softbreaks into memoline softbreak:
 *                cStr := wvw_ebGetText(NIL, nEBid, .T.)
 *                cStr := StrTran(cStr, CR + CR + LF, Chr(141) + LF)
 *
 * returns "" in case of error (eg. nEBid not valid)
 */
HB_FUNC(WVW_EBGETTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiEBid = hb_parni(2);
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  BOOL bSoftBreak = HB_ISLOG(3) ? hb_parl(3) : FALSE;
  USHORT usLen;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  BOOL bToOEM = (pWindowData->CodePage == OEM_CHARSET);

  if (pcd == nullptr)
  {
    hb_retl(false);
    return;
  }

  if (bSoftBreak)
  {
    SendMessage(static_cast<HWND>(pcd->hWndCtrl), EM_FMTLINES, static_cast<WPARAM>(TRUE), 0);
  }

  usLen = static_cast<USHORT>(SendMessage(static_cast<HWND>(pcd->hWndCtrl), WM_GETTEXTLENGTH, 0, 0)) + 1;

  auto lpszTextANSI = static_cast<LPTSTR>(hb_xgrab(usLen));

  SendMessage(static_cast<HWND>(pcd->hWndCtrl), WM_GETTEXT, usLen, reinterpret_cast<LPARAM>(lpszTextANSI));

  if (bToOEM)
  {
    auto ulLen = static_cast<ULONG>(strlen(lpszTextANSI));
    auto lpszText = static_cast<LPTSTR>(hb_xgrab(ulLen + 1));
    CharToOem(lpszTextANSI, lpszText);
    hb_retc(lpszText);
    hb_xfree(lpszText);
  }
  else
  {
    hb_retc(lpszTextANSI);
  }

  hb_xfree(lpszTextANSI);
}

/*wvw_ebSetText( [nWinNum], nEBid, cText )
 * set current text of editbox nEBid in window nWinNum
 * returns .t. if successful, .f. in case of error (eg. nEBid not valid)
 */
HB_FUNC(WVW_EBSETTEXT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiEBid = hb_parni(2);
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  BOOL bRetval;
  LPTSTR lpszText = const_cast<LPTSTR>(hb_parcx(3));
  BOOL bFromOEM = (pWindowData->CodePage == OEM_CHARSET);

  if (pcd == nullptr)
  {
    hb_retl(false);
    return;
  }

  if (bFromOEM)
  {
    auto ulLen = static_cast<ULONG>(strlen(lpszText));
    auto lpszTextANSI = static_cast<LPTSTR>(hb_xgrab(ulLen + 1));
    OemToChar(lpszText, lpszTextANSI);
    lpszText = lpszTextANSI;
  }

  bRetval = SendMessage(static_cast<HWND>(pcd->hWndCtrl), WM_SETTEXT, 0, reinterpret_cast<LPARAM>(lpszText));

  if (bFromOEM)
  {
    hb_xfree(lpszText);
  }

  hb_retl(bRetval);
}

/*wvw_ebGetSel( [nWinNum], nEBid, @nstart, @nend )
 * get selected text editbox nEBid in window nWinNum
 * the start selected text (0-based) is in nstart
 * the end selected text (0-based) is in nend
 * returns .t. if operation successful
 * returns .f. if not (eg. nEBid not valid)
 */
HB_FUNC(WVW_EBGETSEL)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiEBid = hb_parni(2);
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  DWORD dwStart, dwEnd;

  if (pcd == nullptr)
  {
    hb_retl(false);
    return;
  }

  SendMessage(static_cast<HWND>(pcd->hWndCtrl), EM_GETSEL, reinterpret_cast<WPARAM>(&dwStart),
              reinterpret_cast<LPARAM>(&dwEnd));

  if (HB_ISBYREF(3))
  {
    hb_stornl(dwStart, 3);
  }
  if (HB_ISBYREF(4))
  {
    hb_stornl(dwEnd, 4);
  }
  hb_retl(true);
}

/*wvw_ebSetSel( [nWinNum], nEBid, nstart, nend )
 * set selected text editbox nEBid in window nWinNum
 * the start selected text (0-based) is in nstart
 * the end selected text (0-based) is in nend
 * notes: nstart may be > nend (flipped selection)
 * notes: to selet all text: wvw_ebSetSel(nwinnum, nebid, 0, -1)
 * returns .t. if operation successful
 * returns .f. if not (eg. nEBid not valid)
 */
HB_FUNC(WVW_EBSETSEL)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  UINT uiEBid = hb_parni(2);
  auto pcd = GetControlData(usWinNum, WVW_CONTROL_EDITBOX, nullptr, uiEBid);
  auto dwStart = static_cast<DWORD>(HB_ISNUM(3) ? hb_parnl(3) : 0);
  auto dwEnd = static_cast<DWORD>(HB_ISNUM(4) ? hb_parnl(4) : 0);

  if (pcd == nullptr)
  {
    hb_retl(false);
    return;
  }

  SendMessage(static_cast<HWND>(pcd->hWndCtrl), EM_SETSEL, static_cast<WPARAM>(dwStart), static_cast<LPARAM>(dwEnd));
  hb_retl(true);
}

/* Static controls */

HB_FUNC(WVW_STCREATE)
{
  HANDLE hInstance = nullptr;
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pData = hb_getWvwData();
  HWND hWndParent = pWindowData->hWnd;
  HWND hWndCB;

#if 0
   RECT r;
   HDC  hDc;
#endif

  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  UINT uiCBid;
  BOOL bBorder = hb_parnl(7);
  ULONG ulExStyle = 0 | (bBorder ? WS_EX_CLIENTEDGE : 0);

  auto usWidth = static_cast<USHORT>(hb_parni(4));
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  USHORT usBottom = HB_ISNUM(11) ? static_cast<USHORT>(hb_parni(11)) : usTop;
  USHORT usRight = HB_ISNUM(12) ? static_cast<USHORT>(hb_parni(12)) : usLeft + usWidth - 1;
  /* char * sText = hb_parc(5); */

  int iStyle = (bBorder ? WS_BORDER : 0);
  int iBox = HB_ISNUM(10) ? hb_parni(10) : 0;
  HFONT hFont = nullptr;

  if (iBox > 0)
  {
    iStyle |= iBox;
  }

  if (HB_ISNUM(8))
  {
    hFont = reinterpret_cast<HFONT>(HB_PARHANDLE(8));
  }
  else if (pWindowData->hSTfont == nullptr)
  {
    pWindowData->hSTfont = CreateFontIndirect(&pData->s_lfST);
    if (pWindowData->hSTfont == nullptr)
    {
      hb_retnl(0);
      return;
    }
  }

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;

  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode())
  {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  uiCBid = LastControlId(usWinNum, WVW_CONTROL_STATIC);
  if (uiCBid == 0)
  {
    uiCBid = WVW_ID_BASE_STATIC;
  }
  else
  {
    uiCBid++;
  }

  hb_winmainArgGet(&hInstance, nullptr, nullptr);

  hWndCB = CreateWindowEx(ulExStyle, "STATIC", static_cast<LPSTR>(nullptr),
                          WS_CHILD | WS_VISIBLE | static_cast<DWORD>(iStyle), iLeft, iTop, iRight - iLeft + 1,
                          iBottom - iTop + 1, hWndParent, reinterpret_cast<HMENU>(uiCBid),
                          static_cast<HINSTANCE>(hInstance), static_cast<LPVOID>(nullptr));

  if (hWndCB)
  {
    if (HB_ISCHAR(5))
    {
      SendMessage(hWndCB, WM_SETTEXT, 0, reinterpret_cast<LPARAM>(hb_parc(5)));
    }
    if (hFont)
    {
      SendMessage(hWndCB, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
    }
    else
    {
      SendMessage(hWndCB, WM_SETFONT, reinterpret_cast<WPARAM>(pWindowData->hSTfont), static_cast<LPARAM>(TRUE));
    }
    hb_retnl(static_cast<LONG>(uiCBid));
    HB_STOREHANDLE(hWndCB, 9);
  }
  else
  {
    hb_retnl(0);
  }
}

HB_FUNC(WVW_STSETTEXT)
{
  auto hWndCB = reinterpret_cast<HWND>(HB_PARHANDLE(2));

  if (hWndCB)
  {

    SetWindowText(static_cast<HWND>(hWndCB), static_cast<LPCTSTR>(hb_parc(3)));
    hb_retl(true);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(WVW_STSETFONT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pData = hb_getWvwData();
  BOOL retval = TRUE;

  pData->s_lfST.lfHeight = HB_ISNIL(3) ? pWindowData->fontHeight - 2 : hb_parnl(3);
  pData->s_lfST.lfWidth = HB_ISNIL(4) ? pData->s_lfST.lfWidth : hb_parni(4);
  pData->s_lfST.lfEscapement = 0;
  pData->s_lfST.lfOrientation = 0;
  pData->s_lfST.lfWeight = HB_ISNIL(5) ? pData->s_lfST.lfWeight : hb_parni(5);
  pData->s_lfST.lfItalic = HB_ISNIL(7) ? pData->s_lfST.lfItalic : static_cast<BYTE>(hb_parl(7));
  pData->s_lfST.lfUnderline = HB_ISNIL(8) ? pData->s_lfST.lfUnderline : static_cast<BYTE>(hb_parl(8));
  pData->s_lfST.lfStrikeOut = HB_ISNIL(9) ? pData->s_lfST.lfStrikeOut : static_cast<BYTE>(hb_parl(9));
  pData->s_lfST.lfCharSet = DEFAULT_CHARSET;

  pData->s_lfST.lfQuality = HB_ISNIL(6) ? pData->s_lfST.lfQuality : static_cast<BYTE>(hb_parni(6));
  pData->s_lfST.lfPitchAndFamily = FF_DONTCARE;
  if (HB_ISCHAR(2))
  {
    strcpy(pData->s_lfST.lfFaceName, hb_parcx(2));
  }

  if (pWindowData->hSTfont)
  {
    HFONT hOldFont = pWindowData->hSTfont;
    auto hFont = CreateFontIndirect(&pData->s_lfST);
    if (hFont)
    {
      CONTROL_DATA *pcd = pWindowData->pcdCtrlList;

      while (pcd)
      {
        if ((pcd->byCtrlClass == WVW_CONTROL_STATIC) &&
            (reinterpret_cast<HFONT>(SendMessage(pcd->hWndCtrl, WM_GETFONT, 0, 0)) == hOldFont))
        {
          SendMessage(pcd->hWndCtrl, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
        }

        pcd = pcd->pNext;
      }

      pWindowData->hSTfont = hFont;
      DeleteObject(static_cast<HFONT>(hOldFont));
    }
    else
    {
      retval = FALSE;
    }
  }

  hb_retl(retval);
}
