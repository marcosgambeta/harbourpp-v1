/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW combobox functions
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

static LRESULT CALLBACK hb_gt_wvw_CBProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  HWND hWndParent = GetParent(hWnd);
  int nWin;
  int nKbdType;

  auto wvw = hb_gt_wvw();

  if (wvw == nullptr || hWndParent == nullptr) {
    return DefWindowProc(hWnd, message, wParam, lParam);
  }

  for (nWin = 0; nWin < wvw->iNumWindows; nWin++) {
    if (wvw->pWin[nWin]->hWnd == hWndParent) {
      break;
    }
  }

  if (nWin >= wvw->iNumWindows) {
    return DefWindowProc(hWnd, message, wParam, lParam);
  }

  PWVW_WIN wvw_win = wvw->pWin[nWin];

  int nCtrlId = hb_gt_wvw_FindControlId(wvw_win, WVW_CONTROL_COMBOBOX, hWnd, &nKbdType);
  if (nCtrlId == 0) {
    hb_errInternal(10010, "ComboBox: Control ID not found with hb_gt_wvw_FindControlId()", nullptr, nullptr);
    return DefWindowProc(hWnd, message, wParam, lParam);
  }

  WNDPROC OldProc = hb_gt_wvw_GetControlProc(wvw_win, WVW_CONTROL_COMBOBOX, hWnd);
  if (OldProc == nullptr) {
    hb_errInternal(10011, "ComboBox: Failed hb_gt_wvw_GetControlProc()", nullptr, nullptr);
    return DefWindowProc(hWnd, message, wParam, lParam);
  }

  switch (message) {
  case WM_KEYDOWN:
  case WM_SYSKEYDOWN: {
    bool bAlt = GetKeyState(VK_MENU) & 0x8000;
    bool bCtrl = GetKeyState(VK_CONTROL) & 0x8000;
    bool bShift = GetKeyState(VK_SHIFT) & 0x8000;

    if (!hb_gt_wvw_BufferedKey(static_cast<int>(wParam))) {
      break;
    }

    bool fDropped = SendMessage(hWnd, CB_GETDROPPEDSTATE, 0, 0);

    if (nKbdType == WVW_CB_KBD_STANDARD) {
      switch (wParam) {
      case VK_F4:
        if (bAlt) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;

      case VK_ESCAPE:
        if (!bCtrl && !bAlt && !bShift && !fDropped) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;

      case VK_TAB:
        if (!bCtrl && !bAlt) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;

      case VK_NEXT:

        if (fDropped || bAlt || bShift || bCtrl) {
          break;
        } else {
          SendMessage(hWnd, CB_SHOWDROPDOWN, static_cast<WPARAM>(TRUE), 0);
          return 0;
        }

      case VK_RETURN:
        if (!bCtrl && !bAlt && !bShift && !fDropped) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;
      }
      break;
    } else { /* WVW_CB_KBD_STANDARD */
      /* assume WVW_CB_KBD_CLIPPER */
      switch (wParam) {
      case VK_F4:
        if (bAlt) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;

      case VK_RETURN:

        if (fDropped || bAlt || bShift || bCtrl) {
          break;
        } else {
          SendMessage(hWnd, CB_SHOWDROPDOWN, static_cast<WPARAM>(TRUE), 0);
          return 0;
        }

      case VK_ESCAPE:
        if (fDropped || bAlt || bShift || bCtrl) {
          break;
        } else {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }

      case VK_UP:
      case VK_DOWN:
      case VK_RIGHT:
      case VK_LEFT:
      case VK_HOME:
      case VK_END:
      case VK_PRIOR:
      case VK_NEXT:
        if (fDropped) {
          break;
        } else {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }

      case VK_TAB:
        if (!bCtrl && !bAlt) {
          SetFocus(hWndParent);
          PostMessage(hWndParent, message, wParam, lParam);
          return 0;
        }
        break;
      }
      break;
    }
  }
  }

  return CallWindowProc(OldProc, hWnd, message, wParam, lParam);
}

static int hb_gt_wvw_GetFontDialogUnits(HWND hWnd, HFONT hFont)
{
  const TCHAR tmp[] = TEXT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");

  SIZE sz;

  auto hDC = GetDC(hWnd); /* get the DC to the window */

  SelectObject(hDC, hFont);                                 /* with the current font attributes, select the font */
  GetTextExtentPoint32(hDC, tmp, HB_SIZEOFARRAY(tmp), &sz); /* get its length */

  ReleaseDC(hWnd, hDC);

  return sz.cx / HB_SIZEOFARRAY(tmp); /* calculate the average character width */
}

/*
wvw_cbCreate([nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
                        nReserved, nKbdType, aOffset, @hControl, nStyle)
create combobox (drop-down list, no editbox) for window nWinNum
nTop: row of top/left corner (in character unit)
nLeft: col of top/left corner (in character unit)
nWidth: width of combobox (in character unit)
aText: array of drop-down list members, default = {"empty"}
     eg. {"yes","no"}
bBlock: codeblock to execute on these events:
       event=CBN_SELCHANGE(1): user changes selection
                     (not executed if selection
                     is changed programmatically)
        event=CBN_SETFOCUS
        event=CBN_KILLFOCUS
        This codeblock will be evaluated with these parameters:
        nWinNum: window number
        nCBid  : combobox id
        nType  : event type (CBN_SELCHANGE/CBN_SETFOCUS/CBN_KILLFOCUS supported)
        nIndex : index of the selected list item (0 based)
nListLines: number of lines for list items, default = 3
           (will be automatically truncated if it's > Len(aText))
nReserved: reserved for future (this parameter is now ignored)

nKbdType: WVW_CB_KBD_STANDARD (0): similar to standard windows convention
           ENTER/ESC: will kill focus from combobox
          WVW_CB_KBD_CLIPPER (1):
           ENTER: drop (show) the list box
           UP/DOWN/TAB/SHIFTTAB/ESC: kill focus
default is WVW_CB_KBD_STANDARD (0)

aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
        dimension of combobox.
        defaults: {-2,-2,+2,+2}
        NOTES: the third element (y2) is actually ignored.
               height of combobox is always 1 char height
               (see also wvw_cbSetFont())

returns control id of newly created combobox of windows nWinNum
returns 0 if failed
*/
HB_FUNC(WVW_CBCREATE)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win) {
    auto iWidth = hb_parni(4);
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    int iBottom = iTop;
    int iRight = iLeft + iWidth - 1;

    int iNumElement = HB_ISARRAY(5) ? static_cast<int>(hb_arrayLen(hb_param(5, Harbour::Item::ARRAY))) : 0;

    if (wvw_win->hCBfont == nullptr) {
      wvw_win->hCBfont = CreateFontIndirect(&wvw->lfCB);
      if (wvw_win->hCBfont == nullptr) {
        hbwapi_stor_HANDLE(nullptr, 11);
        hb_retni(0);
        return;
      }
    }

    int iOffTop = hb_parvni(10, 1);
    int iOffLeft = hb_parvni(10, 2);
    auto iOffBottom = hb_parnidef(7, 3); /* nListLines */
    int iOffRight = hb_parvni(10, 4);

    RECT rXB;
    rXB.top = iTop;
    rXB.left = iLeft;
    rXB.bottom = iBottom;
    rXB.right = iRight;

    RECT rOffXB;
    rOffXB.top = iOffTop;
    rOffXB.left = iOffLeft;
    rOffXB.bottom = iOffBottom;
    rOffXB.right = iOffRight;

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    POINT xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
    iTop = xy.y + iOffTop;
    iLeft = xy.x + iOffLeft;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
    iBottom = xy.y - wvw_win->iLineSpacing - 1 + (iOffBottom * hb_gt_wvw_LineHeight(wvw_win));
    iRight = xy.x - 1 + iOffRight;

    int nCtrlId = hb_gt_wvw_LastControlId(wvw_win, WVW_CONTROL_COMBOBOX);
    if (nCtrlId == 0) {
      nCtrlId = WVW_ID_BASE_COMBOBOX;
    } else {
      nCtrlId++;
    }

    InitCommonControls();

    HWND hWnd = CreateWindowEx(
        0, TEXT("COMBOBOX"), nullptr, WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | hb_parni(12) /* nStyle */,
        iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, wvw_win->hWnd,
        reinterpret_cast<HMENU>(static_cast<HB_PTRUINT>(nCtrlId)), GetModuleHandle(nullptr), nullptr);

    if (hWnd) {
      HB_SIZE nMaxWidth = 0;

      SendMessage(hWnd, WM_SETREDRAW, static_cast<WPARAM>(TRUE), 0);

      if (iNumElement == 0) {
        if (SendMessage(hWnd, CB_ADDSTRING, 0, reinterpret_cast<LPARAM>(TEXT("empty"))) < 0) {
          /* ignore failure */
        }
      } else {
        for (auto i = 1; i <= iNumElement; i++) {
          void *hText;

          if (SendMessage(hWnd, CB_ADDSTRING, 0, reinterpret_cast<LPARAM>(HB_PARASTR(5, i, &hText, nullptr))) < 0) {
            /* ignore failure */
          } else {
            auto nLen = static_cast<HB_SIZE>(SendMessage(hWnd, CB_GETLBTEXTLEN, i - 1, 0));
            if (nLen != static_cast<HB_SIZE>(CB_ERR) && nLen > nMaxWidth) {
              nMaxWidth = nLen;
            }
          }

          hb_strfree(hText);
        }
      }

      SendMessage(hWnd, CB_SETCURSEL, 0, 0);
      SendMessage(hWnd, CB_SETEXTENDEDUI, static_cast<WPARAM>(TRUE), 0);

      if (nMaxWidth > 2) {
        auto hFont = hb_gt_wvw_GetFont(wvw_win->fontFace, 10, wvw_win->fontWidth, wvw_win->fontWeight,
                                       wvw_win->fontQuality, wvw_win->CodePage);
        nMaxWidth = (nMaxWidth - 2) * hb_gt_wvw_GetFontDialogUnits(wvw_win->hWnd, hFont);
        DeleteObject(hFont);
      }
      SendMessage(hWnd, CB_SETDROPPEDWIDTH, static_cast<WPARAM>(nMaxWidth) + 100, 0);

      hb_gt_wvw_AddControlHandle(wvw_win, WVW_CONTROL_COMBOBOX, hWnd, nCtrlId, hb_param(6, Harbour::Item::EVALITEM),
                                 rXB, rOffXB, hb_parnidef(9, WVW_CB_KBD_STANDARD));
      hb_gt_wvw_StoreControlProc(wvw_win, WVW_CONTROL_COMBOBOX, hWnd,
                                 reinterpret_cast<WNDPROC>(SetWindowLongPtr(
                                     hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(hb_gt_wvw_CBProc))));

      SendMessage(hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(wvw_win->hCBfont), static_cast<LPARAM>(TRUE));

      hbwapi_stor_HANDLE(hWnd, 11);
      hb_retni(nCtrlId);
      return;
    }
  }

  hbwapi_stor_HANDLE(nullptr, 11);
  hb_retni(0);
}

/*
wvw_cbDestroy([nWinNum], nCBid)
destroy combobox nCBid for window nWinNum
*/
HB_FUNC(WVW_CBDESTROY)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto nCtrlId = hb_parni(2);
    auto wvw_ctl = wvw_win->ctlList;
    PWVW_CTL wvw_ctlPrev = nullptr;

    while (wvw_ctl) {
      if (wvw_ctl->nClass == WVW_CONTROL_COMBOBOX && wvw_ctl->nId == nCtrlId) {
        break;
      }

      wvw_ctlPrev = wvw_ctl;
      wvw_ctl = wvw_ctl->pNext;
    }

    if (wvw_ctl) {
      DestroyWindow(wvw_ctl->hWnd);

      if (wvw_ctlPrev) {
        wvw_ctlPrev->pNext = wvw_ctl->pNext;
      } else {
        wvw_win->ctlList = wvw_ctl->pNext;
      }

      if (wvw_ctl->pBlock) {
        hb_itemRelease(wvw_ctl->pBlock);
      }

      hb_xfree(wvw_ctl);
    }
  }
}

/*
wvw_cbSetFocus([nWinNum], nComboId)
set the focus to combobox nComboId in window nWinNum
*/
HB_FUNC(WVW_CBSETFOCUS)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, hb_parni(2), nullptr);
  hb_retl(hWnd && SetFocus(hWnd) != nullptr);
}

/*
wvw_cbIsFocused([nWinNum], nComboId)
returns .T. if the focus is on combobox nComboId in window nWinNum
*/
HB_FUNC(WVW_CBISFOCUSED)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, hb_parni(2), nullptr);
  hb_retl(hWnd && GetFocus() == hWnd);
}

/*
wvw_cbEnable([nWinNum], nComboId, [lEnable])
enable/disable button nComboId on window nWinNum
lEnable defaults to .T., ie. enabling the combobox)
return previous state of the combobox (.T.: enabled .F.: disabled)
(if nComboId is invalid, this function returns .F. too)
*/
HB_FUNC(WVW_CBENABLE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto hWnd = hb_gt_wvw_FindControlHandle(wvw_win, WVW_CONTROL_COMBOBOX, hb_parni(2), nullptr);

  if (hWnd) {
    bool fEnable = hb_parldef(3, true);

    hb_retl(EnableWindow(hWnd, fEnable) == 0);

    if (!fEnable) {
      SetFocus(wvw_win->hWnd);
    }
  } else {
    hb_retl(false);
  }
}

/*
wvw_cbSetCodeblock([nWinNum], nCBid, bBlock)
assign (new) codeblock bBlock to combobox nCBid for window nWinNum
return .T. if successful
*/
HB_FUNC(WVW_CBSETCODEBLOCK)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));
  auto pBlock = hb_param(3, Harbour::Item::EVALITEM);

  if (pBlock && wvw_ctl && !wvw_ctl->fBusy) {
    auto wvw = hb_gt_wvw();
    bool fOldSetting = wvw->fRecurseCBlock;
    wvw->fRecurseCBlock = false;
    wvw_ctl->fBusy = true;

    if (wvw_ctl->pBlock) {
      hb_itemRelease(wvw_ctl->pBlock);
    }

    wvw_ctl->pBlock = hb_itemNew(pBlock);
    wvw_ctl->fBusy = false;
    wvw->fRecurseCBlock = fOldSetting;

    hb_retl(true);
  } else {
    hb_retl(false);
  }
}

/*
wvw_cbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
                            lItalic, lUnderline, lStrikeout

this will initialize font for ALL comboboxes in window nWinNum
(including ones created later on)

TODO: ? should nHeight be ignored, and always forced to use standard char height?
*/
HB_FUNC(WVW_CBSETFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win) {
    auto fResult = true;

    wvw->lfCB.lfHeight = hb_parnldef(3, wvw_win->fontHeight - 2);
    wvw->lfCB.lfWidth = hb_parnldef(4, wvw->lfCB.lfWidth);
    wvw->lfCB.lfEscapement = 0;
    wvw->lfCB.lfOrientation = 0;
    wvw->lfCB.lfWeight = hb_parnldef(5, wvw->lfCB.lfWeight);
    wvw->lfCB.lfQuality = static_cast<BYTE>(hb_parnidef(6, wvw->lfCB.lfQuality));
    wvw->lfCB.lfItalic = static_cast<BYTE>(hb_parldef(7, wvw->lfCB.lfItalic));
    wvw->lfCB.lfUnderline = static_cast<BYTE>(hb_parldef(8, wvw->lfCB.lfUnderline));
    wvw->lfCB.lfStrikeOut = static_cast<BYTE>(hb_parldef(9, wvw->lfCB.lfStrikeOut));
    wvw->lfCB.lfCharSet = DEFAULT_CHARSET;
    wvw->lfCB.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(2)) {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), wvw->lfCB.lfFaceName, HB_SIZEOFARRAY(wvw->lfCB.lfFaceName));
      wvw_win->fontFace[HB_SIZEOFARRAY(wvw->lfCB.lfFaceName) - 1] = TEXT('\0');
    }

    if (wvw_win->hCBfont) {
      HFONT hOldFont = wvw_win->hCBfont;
      auto hFont = CreateFontIndirect(&wvw->lfCB);
      if (hFont) {
        auto wvw_ctl = wvw_win->ctlList;

        while (wvw_ctl) {
          if (wvw_ctl->nClass == WVW_CONTROL_COMBOBOX &&
              reinterpret_cast<HFONT>(SendMessage(wvw_ctl->hWnd, WM_GETFONT, 0, 0)) == hOldFont) {
            SendMessage(wvw_ctl->hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
          }

          wvw_ctl = wvw_ctl->pNext;
        }

        wvw_win->hCBfont = hFont;
        DeleteObject(hOldFont);
      } else {
        fResult = false;
      }
    }

    hb_retl(fResult);
  } else {
    hb_retl(false);
  }
}

/*
wvw_cbSetIndex([nWinNum], nCBid, nIndex)
set current selection of combobox nCBid in window nWinNum to nIndex
(nIndex is 0 based)
returns .T. if successful.

NOTE: the better name to this function should be wvw_cbSetCurSel()
      but that name is already used.
      (wvw_cbSetCurSel() and wvw_cbAddString() is NOT related to other
      WVW_CB* functions)
*/
HB_FUNC(WVW_CBSETINDEX)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));
  auto iIndex = hb_parni(3);
  hb_retl((wvw_ctl && iIndex >= 0) ? SendMessage(wvw_ctl->hWnd, CB_SETCURSEL, static_cast<WPARAM>(iIndex), 0) == iIndex
                                   : false);
}

/*
wvw_cbGetIndex([nWinNum], nCBid)
get current selection of combobox nCBid in window nWinNum
return nIndex (0 based)
returns CB_ERR (-1) if none selected

NOTE: the better name to this function should be WVW_CBgetCurSel()
      but that name is potentially misleading to wvw_cbSetCurSel
      which is not our family of wvw_cb*() functions
      (wvw_cbSetCurSel() and wvw_cbAddString() is NOT related to other
      WVW_CB* functions)
*/
HB_FUNC(WVW_CBGETINDEX)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));
  hb_retni(wvw_ctl ? static_cast<int>(SendMessage(wvw_ctl->hWnd, CB_GETCURSEL, 0, 0)) : CB_ERR);
}

/*
wvw_cbFindString([nWinNum], nCBid, cString)
find index of cString in combobox nCBid in window nWinNum
returns index of cString (0 based)
returns CB_ERR (-1) if string not found
NOTE:case insensitive
*/
HB_FUNC(WVW_CBFINDSTRING)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));

  if (wvw_ctl) {
    void *hStr;
    hb_retni(static_cast<int>(SendMessage(wvw_ctl->hWnd, CB_FINDSTRING, static_cast<WPARAM>(-1),
                                          reinterpret_cast<LPARAM>(HB_PARSTRDEF(3, &hStr, nullptr)))));
    hb_strfree(hStr);
  } else {
    hb_retni(CB_ERR);
  }
}

/*
wvw_cbGetCurText([nWinNum], nCBid)
get current selected cString in combobox nCBid in window nWinNum
returns "" if none selected
*/
HB_FUNC(WVW_CBGETCURTEXT)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));

  if (wvw_ctl) {
    auto iCurSel = static_cast<int>(SendMessage(wvw_ctl->hWnd, CB_GETCURSEL, 0, 0));
    auto nTextLen = static_cast<HB_SIZE>(SendMessage(wvw_ctl->hWnd, CB_GETLBTEXTLEN, static_cast<WPARAM>(iCurSel), 0));
    if (nTextLen == static_cast<HB_SIZE>(CB_ERR)) {
      hb_retc_null();
    } else {
      TCHAR *str = new TCHAR[nTextLen + 1];

      if (SendMessage(wvw_ctl->hWnd, CB_GETLBTEXT, static_cast<WPARAM>(iCurSel), reinterpret_cast<LPARAM>(str)) ==
          CB_ERR) {
        hb_retc_null();
      } else {
        HB_RETSTRLEN(str, nTextLen);
      }

      delete[] str;
    }
  } else {
    hb_retc_null();
  }
}

/*
wvw_cbIsDropped([nWinNum], nCBid)
get current dropped state of combobox nCBid in window nWinNum
returns .T. if listbox is being shown, otherwise .F.
Also returns .F. if nCBid not valid
*/
HB_FUNC(WVW_CBISDROPPED)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, nullptr, hb_parni(2));
  hb_retl(wvw_ctl ? static_cast<bool>(SendMessage(wvw_ctl->hWnd, CB_GETDROPPEDSTATE, 0, 0)) : false);
}

HB_FUNC(WVW_CBVISIBLE)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_COMBOBOX, hb_parni(2), nullptr);
  hb_retl(hWnd && ShowWindow(hWnd, hb_parldef(3, true) ? SW_SHOW : SW_HIDE) == 0);
}
