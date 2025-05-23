/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW pushbutton functions
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

/*
wvw_pbCreate([nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset, ;
             nStretchBitmap, lMap3Dcolors, @hControl, nStyle)
create pushbutton for window nWinNum
nTop: row of top/left corner (in character unit)
nLeft: col of top/left corner (in character unit)
nBottom: row of bottom/right corner (in character unit) defaults==nTop
nRight: col of bottom/right corner (in character unit) defaults==??
cText: caption, default == ""

cImage: bitmap file name, can be supplied as nImage: bitmap resource id

nStretchBitmap: a number between 0 and 1 (inclusive) as a factor to
              stretch the bitmap.
              1.0: bitmap covers the whole button
              0.5: bitmap covers 50% of button
              0: bitmap is not stretch
             (default is 1)

lMap3Dcolors: defaults to .F.
         if .T. the following color mapping will be performed:
            RGB(192, 192, 192) --> COLOR_3DFACE   ("transparent")
            RGB(128, 128, 128) --> COLOR_3DSHADOW
            RGB(223, 223, 223) --> COLOR_3DLIGHT
         This might be desirable to have transparent effect.
         LIMITATION: this will work on 256 colored bitmaps only

aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
       dimension of pushbutton.
       defaults for pushbutton: {-2,-2,+2,+2}

bBlock:  codeblock to execute on every BN_CLICK event.
       This codeblock will be evaluated with these parameters:
       nWinNum: window number
       nPBid  : pushbutton id

returns control id of newly created pushbutton of windows nWinNum
returns 0 if failed
*/
HB_FUNC(WVW_PBCREATE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  HWND hWnd = nullptr;

  if (wvw_win && HB_ISEVALITEM(8))
  {
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
                                    hb_parl(11) /* bMap3Dcolors */, BS_PUSHBUTTON | hb_parni(13) /* nStyle */, &hWnd));

    hb_strfree(hCaption);
  }
  else
  {
    hb_retni(0);
  }

  hbwapi_stor_HANDLE(hWnd, 12);
}

/*
wvw_pbDestroy([nWinNum], nPBid)
destroy button nPBid for window nWinNum
*/
HB_FUNC(WVW_PBDESTROY)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win)
  {
    auto nCtrlId = hb_parni(2);
    auto wvw_ctl = wvw_win->ctlList;
    PWVW_CTL wvw_ctlPrev = nullptr;

    while (wvw_ctl)
    {
      if (wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON && wvw_ctl->nId == nCtrlId)
      {
        break;
      }
      wvw_ctlPrev = wvw_ctl;
      wvw_ctl = wvw_ctl->pNext;
    }

    if (wvw_ctl)
    {
      DestroyWindow(wvw_ctl->hWnd);

      if (wvw_ctlPrev)
      {
        wvw_ctlPrev->pNext = wvw_ctl->pNext;
      }
      else
      {
        wvw_win->ctlList = wvw_ctl->pNext;
      }

      if (wvw_ctl->pBlock)
      {
        hb_itemRelease(wvw_ctl->pBlock);
      }

      hb_xfree(wvw_ctl);
    }
  }
}

/*
wvw_pbSetFocus([nWinNum], nButtonId)
set the focus to button nButtonId in window nWinNum
*/
HB_FUNC(WVW_PBSETFOCUS)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);
  hb_retl(hWnd && SetFocus(hWnd) != nullptr);
}

/*
wvw_pbIsFocused([nWinNum], nPBid)
returns .T. if the focus is on button nPBid in window nWinNum
*/
HB_FUNC(WVW_PBISFOCUSED)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);
  hb_retl(hWnd && GetFocus() == hWnd);
}

/*
wvw_pbEnable([nWinNum], nButtonId, [lToggle])
enable/disable button nButtonId on window nWinNum
(lToggle defaults to .T., ie. enabling the button)
return previous state of the button (.T.: enabled .F.: disabled)
(if nButtonId is invalid, this function returns .F. too)
*/
HB_FUNC(WVW_PBENABLE)
{
  auto wvw_win = hb_gt_wvw_win_par();

  auto hWnd = hb_gt_wvw_FindControlHandle(wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);

  if (hWnd)
  {
    bool fEnable = hb_parldef(3, true);

    hb_retl(EnableWindow(hWnd, fEnable) == 0);

    if (!fEnable)
    {
      SetFocus(wvw_win->hWnd);
    }
  }
  else
  {
    hb_retl(false);
  }
}

/*
wvw_pbSetCodeblock([nWinNum], nPBid, bBlock)
assign (new) codeblock bBlock to button nPBid for window nWinNum
return .T. if successful
*/
HB_FUNC(WVW_PBSETCODEBLOCK)
{
  auto wvw = hb_gt_wvw();
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, nullptr, hb_parni(2));
  auto pBlock = hb_param(3, Harbour::Item::EVALITEM);

  if (pBlock && wvw_ctl && !wvw_ctl->fBusy)
  {
    bool fOldSetting = wvw->fRecurseCBlock;

    wvw->fRecurseCBlock = false;
    wvw_ctl->fBusy = true;

    if (wvw_ctl->pBlock)
    {
      hb_itemRelease(wvw_ctl->pBlock);
    }

    wvw_ctl->pBlock = hb_itemNew(pBlock);

    wvw_ctl->fBusy = false;
    wvw->fRecurseCBlock = fOldSetting;

    hb_retl(true);
  }
  else
  {
    hb_retl(false);
  }
}

/*
wvw_pbSetStyle([nWinNum], nPBid, nStyle)
assign new style nStyle to button nPBid for window nWinNum
typical usage: nStyle==BS_DEFPUSHBUTTON (==01) to turn the button
                                               into default push button
                                               (thick border)
                       BS_PUSHBUTTON    (==00) to turn the button
                                               into regular push button

using other styles like BS_MULTILINE may also be useful,
but I haven't tried that

this function always return .T.
*/
HB_FUNC(WVW_PBSETSTYLE)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, nullptr, hb_parni(2));

  if (wvw_ctl && wvw_ctl->hWnd)
  {
    SendMessage(wvw_ctl->hWnd, BM_SETSTYLE, static_cast<WPARAM>(hb_parni(3)), static_cast<LPARAM>(TRUE));
  }

  hb_retl(true);
}

/*
wvw_pbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
              lItalic, lUnderline, lStrikeout)
this will initialize font for ALL pushbuttons in window nWinNum
(including ones created later on)
*/
HB_FUNC(WVW_PBSETFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win)
  {
    auto fResult = true;

    wvw->lfPB.lfHeight = hb_parnldef(3, wvw_win->fontHeight - 2);
    wvw->lfPB.lfWidth = hb_parnldef(4, wvw->lfPB.lfWidth);
    wvw->lfPB.lfEscapement = 0;
    wvw->lfPB.lfOrientation = 0;
    wvw->lfPB.lfWeight = hb_parnldef(5, wvw->lfPB.lfWeight);
    wvw->lfPB.lfQuality = static_cast<BYTE>(hb_parnidef(6, wvw->lfPB.lfQuality));
    wvw->lfPB.lfItalic = static_cast<BYTE>(hb_parldef(7, wvw->lfPB.lfItalic));
    wvw->lfPB.lfUnderline = static_cast<BYTE>(hb_parldef(8, wvw->lfPB.lfUnderline));
    wvw->lfPB.lfStrikeOut = static_cast<BYTE>(hb_parldef(9, wvw->lfPB.lfStrikeOut));
    wvw->lfPB.lfCharSet = DEFAULT_CHARSET;
    wvw->lfPB.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(2))
    {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), wvw->lfPB.lfFaceName, HB_SIZEOFARRAY(wvw->lfPB.lfFaceName));
      wvw_win->fontFace[HB_SIZEOFARRAY(wvw->lfPB.lfFaceName) - 1] = TEXT('\0');
    }

    if (wvw_win->hPBfont)
    {
      HFONT hOldFont = wvw_win->hPBfont;
      auto hFont = CreateFontIndirect(&wvw->lfPB);
      if (hFont)
      {
        auto wvw_ctl = wvw_win->ctlList;

        while (wvw_ctl)
        {
          if (wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON &&
              reinterpret_cast<HFONT>(SendMessage(wvw_ctl->hWnd, WM_GETFONT, 0, 0)) == hOldFont)
          {
            SendMessage(wvw_ctl->hWnd, WM_SETFONT, reinterpret_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
          }

          wvw_ctl = wvw_ctl->pNext;
        }

        wvw_win->hPBfont = hFont;
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

HB_FUNC(WVW_PBVISIBLE)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);
  hb_retl(hWnd && ShowWindow(hWnd, hb_parldef(3, true) ? SW_SHOW : SW_HIDE) == 0);
}
