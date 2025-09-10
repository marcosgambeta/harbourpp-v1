/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW tooltip functions
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

/* workaround for missing declaration in MinGW */
#if !defined(TTM_SETTITLE) && defined(TTM_SETTITLEA)
#define TTM_SETTITLE TTM_SETTITLEA
#endif

/*
wvw_SetToolTopActive([nWinNum], [lToggle])
*/
HB_FUNC(WVW_SETTOOLTIPACTIVE)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retl(wvw_win->fToolTipActive);

    if (HB_ISLOG(2)) {
      wvw_win->fToolTipActive = hb_parl(2);

      if (wvw_win->fToolTipActive && wvw_win->hWndTT == nullptr) {
        hb_gt_wvw_CreateToolTipWindow(wvw_win);
      }
    }

    return;
  }
#endif
  hb_retl(false);
}

/*
wvw_SetToolTip([nWinNum], nTop, nLeft, nBottom, nRight, cToolText)
*/
HB_FUNC(WVW_SETTOOLTIP)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win && wvw_win->fToolTipActive) {
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parni(4);
    auto iRight = hb_parni(5);

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    TOOLINFO ti{};
    ti.cbSize = sizeof(ti);
    ti.hwnd = wvw_win->hWnd;
    ti.uId = WVW_ID_BASE_TOOLTIP + wvw_win->nWinId;

    if (SendMessage(wvw_win->hWndTT, TTM_GETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti))) {
      void *hText;

      POINT xy;

      xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
      iTop = xy.y;
      iLeft = xy.x;

      xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
      iBottom = xy.y - 1;
      iRight = xy.x - 1;

      ti.lpszText = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTRDEF(6, &hText, nullptr)));
      ti.rect.left = iLeft;
      ti.rect.top = iTop;
      ti.rect.right = iRight;
      ti.rect.bottom = iBottom;

      SendMessage(wvw_win->hWndTT, TTM_SETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti));

      hb_strfree(hText);
    }
  }
#endif
}

HB_FUNC(WVW_SETTOOLTIPTEXT)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    TOOLINFO ti;
    ti.cbSize = sizeof(ti);
    ti.hwnd = wvw_win->hWnd;
    ti.uId = 100000;

    if (SendMessage(wvw_win->hWndTT, TTM_GETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti))) {
      void *hText;
      ti.lpszText = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTRDEF(2, &hText, nullptr)));
      SendMessage(wvw_win->hWndTT, TTM_UPDATETIPTEXT, 0, reinterpret_cast<LPARAM>(&ti));
      hb_strfree(hText);
    }
  }
#endif
}

HB_FUNC(WVW_SETTOOLTIPMARGIN)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    RECT rc;
    rc.left = hb_parni(3);
    rc.top = hb_parni(2);
    rc.right = hb_parni(5);
    rc.bottom = hb_parni(4);

    SendMessage(wvw_win->hWndTT, TTM_SETMARGIN, 0, reinterpret_cast<LPARAM>(&rc));
  }
#endif
}

HB_FUNC(WVW_SETTOOLTIPWIDTH)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retni(static_cast<int>(SendMessage(wvw_win->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0)));

    if (HB_ISNUM(2)) {
      SendMessage(wvw_win->hWndTT, TTM_SETMAXTIPWIDTH, 0, static_cast<LPARAM>(hb_parni(2)));
    }

    return;
  }
#endif
  hb_retni(0);
}

HB_FUNC(WVW_SETTOOLTIPBKCOLOR)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hbwapi_ret_COLORREF(SendMessage(wvw_win->hWndTT, TTM_GETTIPBKCOLOR, 0, 0));

    if (HB_ISNUM(2)) {
      SendMessage(wvw_win->hWndTT, TTM_SETTIPBKCOLOR, static_cast<WPARAM>(hbwapi_par_COLORREF(2)), 0);
    }

    return;
  }
#endif
  hbwapi_ret_COLORREF(0);
}

HB_FUNC(WVW_SETTOOLTIPTEXTCOLOR)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hbwapi_ret_COLORREF(SendMessage(wvw_win->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0));

    if (HB_ISNUM(2)) {
      SendMessage(wvw_win->hWndTT, TTM_SETTIPTEXTCOLOR, static_cast<WPARAM>(hbwapi_par_COLORREF(2)), 0);
    }

    return;
  }
#endif
  hbwapi_ret_COLORREF(0);
}

HB_FUNC(WVW_SETTOOLTIPTITLE)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win && HB_ISCHAR(3)) {
    void *hText;

    auto iIcon = hb_parni(2);
    if (iIcon > 3) {
      iIcon = 0;
    }

    SendMessage(wvw_win->hWndTT, TTM_SETTITLE, static_cast<WPARAM>(iIcon),
                reinterpret_cast<LPARAM>(HB_PARSTR(3, &hText, nullptr)));
    hb_strfree(hText);
  }
#endif
}

HB_FUNC(WVW_GETTOOLTIPWIDTH)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hb_retni(static_cast<int>(SendMessage(wvw_win->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0)));
    return;
  }
#endif
  hb_retni(0);
}

HB_FUNC(WVW_GETTOOLTIPBKCOLOR)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hbwapi_ret_COLORREF(SendMessage(wvw_win->hWndTT, TTM_GETTIPBKCOLOR, 0, 0));
    return;
  }
#endif
  hbwapi_ret_COLORREF(0);
}

HB_FUNC(WVW_GETTOOLTIPTEXTCOLOR)
{
#if _WIN32_IE > 0x400
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win) {
    hbwapi_ret_COLORREF(SendMessage(wvw_win->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0));
    return;
  }
#endif
  hbwapi_ret_COLORREF(0);
}
