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

#define WVW_MAX_STATUS_PARTS 40   /* max # of parts in Status Bar */
#define WVW_SPACE_BETWEEN_PARTS 2 /* pixel space between Status Bar's parts */

/*
wvw_sbCreate([nWinNum])
create status bar for window nWinNum, with one part.
returns handle to status bar of windows nWinNum
returns 0 if failed, eg. if there is already a status bar for this window
*/
HB_FUNC(WVW_SBCREATE)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win && wvw_win->hStatusBar == nullptr) {
    HWND hWnd = CreateStatusWindow(WS_CHILD | WS_VISIBLE | WS_BORDER | SBT_TOOLTIPS, nullptr, wvw_win->hWnd,
                                   WVW_ID_BASE_STATUSBAR + wvw_win->nWinId);
    if (hWnd) {
      if (wvw_win->hSBfont == nullptr) {
        wvw_win->hSBfont = CreateFontIndirect(&wvw->lfSB);
      }

      RECT rSB{};

      if (GetClientRect(hWnd, &rSB)) {
        wvw_win->iSBHeight = rSB.bottom;
      }

      wvw_win->hStatusBar = hWnd;

      hb_gt_wvw_ResetWindow(wvw_win);

      int piArray = rSB.right;
      SendMessage(hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(wvw_win->hSBfont), static_cast<LPARAM>(TRUE));

      SendMessage(hWnd, SB_SETPARTS, 1, reinterpret_cast<LPARAM>(&piArray));
    }

    hbwapi_ret_raw_HANDLE(hWnd);
  } else {
    hbwapi_ret_raw_HANDLE(nullptr);
  }
}

/*
wvw_sbDestroy([nWinNum])
destroy status bar for window nWinNum
*/
HB_FUNC(WVW_SBDESTROY)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win && wvw_win->hStatusBar != nullptr) {
    if (wvw_win->hSBfont) {
      DeleteObject(wvw_win->hSBfont);
      wvw_win->hSBfont = nullptr;
    }
    DestroyWindow(wvw_win->hStatusBar);
    wvw_win->hStatusBar = nullptr;
    wvw_win->fSBPaint = false;
    wvw_win->iSBHeight = 0;
    hb_gt_wvw_ResetWindow(wvw_win);
  }
}

/*
wvw_sbAddPart(nWinNum, cMaxText, nWidth, nStyle, lResetParts, [cIcon], [cToolTip])
ps.
lResetParts==.T. :: remove all previously created parts
nStyle: 0 (default), 0x0200 (SBT_POPOUT), 0x0100 (SBT_NOBORDERS)
nWidth: expected width in pixels
NOTE: if cMaxText is passed, nWidth is ignored. width of cMaxText will be used instead
NOTE: the leftmost part will eventually have width of remaining spaces
NOTE: cIcon and cToolTip does not work currently

returns number of parts
returns 0 if failed
*/
HB_FUNC(WVW_SBADDPART)
{
  auto wvw_win = hb_gt_wvw_win_par();
  HWND hWnd;

  if (wvw_win && (hWnd = wvw_win->hStatusBar) != nullptr) {
    int piArray[WVW_MAX_STATUS_PARTS];
    int iNumOfParts;
    auto displayFlags = static_cast<WORD>(hb_parnl(4));
    bool fResetParts = hb_parl(5);
    int iWidth = hb_parni(3) <= 0 ? 5 * WVW_SPACE_BETWEEN_PARTS : hb_parni(3);

    if (HB_ISCHAR(2)) {
      auto hDCSB = GetDC(hWnd);

      HB_SIZE nLen;
      void *hText;
      LPCTSTR szText = HB_PARSTR(2, &hText, &nLen);

      SIZE size{};

      SelectObject(hDCSB, reinterpret_cast<HFONT>(SendMessage(hWnd, WM_GETFONT, 0, 0)));

      if (GetTextExtentPoint32(hDCSB, szText, static_cast<int>(nLen + 1), &size)) {
        iWidth = size.cx;
      }

      hb_strfree(hText);

      ReleaseDC(hWnd, hDCSB);
    }

    if (!fResetParts) {
      iNumOfParts = static_cast<int>(SendMessage(hWnd, SB_GETPARTS, HB_SIZEOFARRAY(piArray) - 1,
                                                 reinterpret_cast<LPARAM>(static_cast<LPINT>(piArray))));
    } else {
      iNumOfParts = 0;
    }
    iNumOfParts++;

    RECT rSB{};
    GetClientRect(hWnd, &rSB);

    piArray[iNumOfParts - 1] = rSB.right;
    if (!fResetParts) {
      for (auto n = 0; n < iNumOfParts - 1; n++) {
        piArray[n] -= iWidth + WVW_SPACE_BETWEEN_PARTS;
      }
    }

    SendMessage(hWnd, SB_SETPARTS, iNumOfParts, reinterpret_cast<LPARAM>(piArray));

    if (HB_ISCHAR(6)) {
      int cy = rSB.bottom - rSB.top - 4;
      int cx = cy;

      void *hName;
      LPCTSTR szName = HB_PARSTR(6, &hName, nullptr);

      auto hIcon = static_cast<HICON>(LoadImage(
          0, szName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT | LR_DEFAULTSIZE));
      if (hIcon == nullptr) {
        hIcon = static_cast<HICON>(
            LoadImage(GetModuleHandle(nullptr), szName, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR | LR_DEFAULTSIZE));
      }

      hb_strfree(hName);

      if (hIcon != nullptr) {
        SendMessage(hWnd, SB_SETICON, static_cast<WPARAM>(iNumOfParts) - 1, reinterpret_cast<LPARAM>(hIcon));
      }
    }

    SendMessage(hWnd, SB_SETTEXT, (iNumOfParts - 1) | displayFlags, 0);
    if (HB_ISCHAR(7)) {
      void *hText;
      SendMessage(hWnd, SB_SETTIPTEXT, static_cast<WPARAM>(iNumOfParts - 1),
                  reinterpret_cast<LPARAM>(HB_PARSTR(7, &hText, nullptr)));
      hb_strfree(hText);
    }

    hb_retni(iNumOfParts);
  } else {
    hb_retni(0);
  }
}

/*
wvw_sbRefresh(nWinNum)
reinitialize StatusBar's parts, eg. after window resize
TODO: do it automatically, after hb_gt_wvw_ResetWindowSize()
returns number of parts
returns 0 if failed
*/
HB_FUNC(WVW_SBREFRESH)
{
  auto wvw_win = hb_gt_wvw_win_par();
  HWND hWnd;

  if (wvw_win && (hWnd = wvw_win->hStatusBar) != nullptr) {
    int piArray[WVW_MAX_STATUS_PARTS];
    auto iNumOfParts = static_cast<int>(
        SendMessage(hWnd, SB_GETPARTS, HB_SIZEOFARRAY(piArray), reinterpret_cast<LPARAM>(static_cast<LPINT>(piArray))));
    if (iNumOfParts > 0) {
      RECT rSB{};
      GetClientRect(hWnd, &rSB);
      int iDiff = rSB.right - piArray[iNumOfParts - 1];

      for (auto n = 0; n <= iNumOfParts - 1; n++) {
        piArray[n] += iDiff;
      }

      SendMessage(hWnd, SB_SETPARTS, iNumOfParts, reinterpret_cast<LPARAM>(static_cast<LPINT>(piArray)));
      hb_retni(iNumOfParts);
      return;
    }
  }

  hb_retni(0);
}

/*
wvw_sbSetText([nWinNum], [nPart], cText)
Set Text of status bar's part #npart
*/
HB_FUNC(WVW_SBSETTEXT)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto iPart = hb_parnidef(2, 1);

    void *hText;

    if (HB_ISCHAR(4)) {
      wvw_win->cSBColorForeground = strtol(hb_parc(4), nullptr, 10);
    } else if (HB_ISNUM(4)) {
      wvw_win->cSBColorForeground = hbwapi_par_COLORREF(4);
    }

    if (HB_ISCHAR(5)) {
      wvw_win->cSBColorBackground = strtol(hb_parc(5), nullptr, 10);
    } else if (HB_ISNUM(5)) {
      wvw_win->cSBColorBackground = hbwapi_par_COLORREF(5);
    }

    if (iPart == 0 && (wvw_win->cSBColorForeground || wvw_win->cSBColorBackground)) {
      wvw_win->fSBPaint = true;
      SendMessage(wvw_win->hStatusBar, SB_SETTEXT, SBT_OWNERDRAW,
                  reinterpret_cast<LPARAM>(HB_PARSTRDEF(3, &hText, nullptr)));
      hb_gt_wvw_ProcessMessages(wvw_win);
    } else {
      SendMessage(wvw_win->hStatusBar, SB_SETTEXT, iPart, reinterpret_cast<LPARAM>(HB_PARSTRDEF(3, &hText, nullptr)));
    }

    hb_strfree(hText);
  }
}

/*
wvw_sbGetText([nWinNum], [nPart])
Get Text of status bar's part #npart
*/
HB_FUNC(WVW_SBGETTEXT)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    auto iPart = hb_parnidef(2, 1);
    WORD nLen = LOWORD(SendMessage(wvw_win->hStatusBar, SB_GETTEXTLENGTH, static_cast<WPARAM>(iPart), 0));
    TCHAR *szText = new TCHAR[nLen + 1];
    SendMessage(wvw_win->hStatusBar, SB_GETTEXT, static_cast<WPARAM>(iPart), reinterpret_cast<LPARAM>(szText));
    HB_RETSTRLEN(szText, nLen);
    delete[] szText;
  }
}

/*
wvw_sbGetParts([nWinNum])
Get number of parts in statusbar of window nWinNum
*/
HB_FUNC(WVW_SBGETPARTS)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retni(static_cast<int>(SendMessage(wvw_win->hStatusBar, SB_GETPARTS, WVW_MAX_STATUS_PARTS, 0)));
  }
}

/*
wvw_sbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
              lItalic, lUnderline, lStrikeout)
*/
HB_FUNC(WVW_SBSETFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win) {
    auto fResult = true;

    wvw->lfSB.lfHeight = hb_parnldef(3, wvw_win->fontHeight - 2);
    wvw->lfSB.lfWidth = hb_parnldef(4, wvw->lfSB.lfWidth);
    wvw->lfSB.lfEscapement = 0;
    wvw->lfSB.lfOrientation = 0;
    wvw->lfSB.lfWeight = hb_parnldef(5, wvw->lfSB.lfWeight);
    wvw->lfSB.lfQuality = static_cast<BYTE>(hb_parnidef(6, wvw->lfSB.lfQuality));
    wvw->lfSB.lfItalic = static_cast<BYTE>(hb_parldef(7, wvw->lfSB.lfItalic));
    wvw->lfSB.lfUnderline = static_cast<BYTE>(hb_parldef(8, wvw->lfSB.lfUnderline));
    wvw->lfSB.lfStrikeOut = static_cast<BYTE>(hb_parldef(9, wvw->lfSB.lfStrikeOut));
    wvw->lfSB.lfCharSet = DEFAULT_CHARSET;
    wvw->lfSB.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(2)) {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), wvw->lfSB.lfFaceName, HB_SIZEOFARRAY(wvw->lfSB.lfFaceName));
      wvw_win->fontFace[HB_SIZEOFARRAY(wvw->lfSB.lfFaceName) - 1] = TEXT('\0');
    }

    if (wvw_win->hSBfont) {
      HFONT hOldFont = wvw_win->hSBfont;
      auto hFont = CreateFontIndirect(&wvw->lfSB);
      if (hFont) {
        wvw_win->hSBfont = hFont;
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
