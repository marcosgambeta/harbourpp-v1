/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW static control functions
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

HB_FUNC(WVW_STCREATE)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win)
  {
    bool fBorder = hb_parl(7);
    auto iWidth = hb_parni(4);
    auto iTop = hb_parni(2);
    auto iLeft = hb_parni(3);
    auto iBottom = hb_parnidef(11, iTop);
    auto iRight = hb_parnidef(12, iLeft + iWidth - 1);
    auto iBox = hb_parni(10);
    HFONT hFont = nullptr;

    DWORD iStyle = fBorder ? WS_BORDER : 0;

    if (iBox > 0)
    {
      iStyle |= iBox;
    }

    if (HB_ISNUM(8))
    {
      hFont = hbwapi_par_raw_HFONT(8);
    }
    else if (wvw_win->hSTfont == nullptr)
    {
      wvw_win->hSTfont = CreateFontIndirect(&wvw->lfST);
      if (wvw_win->hSTfont == nullptr)
      {
        hbwapi_stor_HANDLE(nullptr, 9);
        hb_retni(0);
        return;
      }
    }

    int iOffTop = hb_parvni(6, 1);
    int iOffLeft = hb_parvni(6, 2);
    int iOffBottom = hb_parvni(6, 3);
    int iOffRight = hb_parvni(6, 4);

    hb_gt_wvw_HBFUNCPrologue(wvw_win, &iTop, &iLeft, &iBottom, &iRight);

    POINT xy;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iLeft, iTop);
    iTop = xy.y + iOffTop;
    iLeft = xy.x + iOffLeft;

    xy = hb_gt_wvw_GetXYFromColRow(wvw_win, iRight + 1, iBottom + 1);
    iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
    iRight = xy.x - 1 + iOffRight;

    int nCtrlId = hb_gt_wvw_LastControlId(wvw_win, WVW_CONTROL_STATIC);
    if (nCtrlId == 0)
    {
      nCtrlId = WVW_ID_BASE_STATIC;
    }
    else
    {
      nCtrlId++;
    }

    HWND hWnd =
        CreateWindowEx(fBorder ? WS_EX_CLIENTEDGE : 0, TEXT("STATIC"), nullptr, WS_CHILD | WS_VISIBLE | iStyle, iLeft,
                       iTop, iRight - iLeft + 1, iBottom - iTop + 1, wvw_win->hWnd,
                       reinterpret_cast<HMENU>(static_cast<HB_PTRUINT>(nCtrlId)), GetModuleHandle(nullptr), nullptr);

    if (hWnd)
    {
      if (HB_ISCHAR(5))
      {
        void *hText;
        SendMessage(hWnd, WM_SETTEXT, 0, reinterpret_cast<LPARAM>(HB_PARSTR(5, &hText, nullptr)));
        hb_strfree(hText);
      }

      if (hFont)
      {
        SendMessage(hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
      }
      else
      {
        SendMessage(hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(wvw_win->hSTfont), static_cast<LPARAM>(TRUE));
      }

      hbwapi_stor_HANDLE(hWnd, 9);
      hb_retni(nCtrlId);
      return;
    }
  }

  hbwapi_stor_HANDLE(nullptr, 9);
  hb_retni(0);
}

HB_FUNC(WVW_STSETFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win)
  {
    auto fResult = true;

    wvw->lfST.lfHeight = hb_parnldef(3, wvw_win->fontHeight - 2);
    wvw->lfST.lfWidth = hb_parnldef(4, wvw->lfST.lfWidth);
    wvw->lfST.lfEscapement = 0;
    wvw->lfST.lfOrientation = 0;
    wvw->lfST.lfWeight = hb_parnldef(5, wvw->lfST.lfWeight);
    wvw->lfST.lfQuality = static_cast<BYTE>(hb_parnidef(6, wvw->lfST.lfQuality));
    wvw->lfST.lfItalic = static_cast<BYTE>(hb_parldef(7, wvw->lfST.lfItalic));
    wvw->lfST.lfUnderline = static_cast<BYTE>(hb_parldef(8, wvw->lfST.lfUnderline));
    wvw->lfST.lfStrikeOut = static_cast<BYTE>(hb_parldef(9, wvw->lfST.lfStrikeOut));
    wvw->lfST.lfCharSet = DEFAULT_CHARSET;
    wvw->lfST.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(2))
    {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), wvw->lfST.lfFaceName, HB_SIZEOFARRAY(wvw->lfST.lfFaceName));
      wvw_win->fontFace[HB_SIZEOFARRAY(wvw->lfST.lfFaceName) - 1] = TEXT('\0');
    }

    if (wvw_win->hSTfont)
    {
      HFONT hOldFont = wvw_win->hSTfont;
      auto hFont = CreateFontIndirect(&wvw->lfST);
      if (hFont)
      {
        auto wvw_ctl = wvw_win->ctlList;

        while (wvw_ctl)
        {
          if (wvw_ctl->nClass == WVW_CONTROL_STATIC &&
              reinterpret_cast<HFONT>(SendMessage(wvw_ctl->hWnd, WM_GETFONT, 0, 0)) == hOldFont)
          {
            SendMessage(wvw_ctl->hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
          }

          wvw_ctl = wvw_ctl->pNext;
        }

        wvw_win->hSTfont = hFont;
        DeleteObject(hOldFont);
      }
      else
      {
        fResult = false;
      }
    }

    hb_retl(fResult);
  }
  else
  {
    hb_retl(false);
  }
}
