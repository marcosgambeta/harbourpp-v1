/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW checkbox functions
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
wvw_cxCreate([nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset, ;
             nStretchBitmap, lMap3Dcolors, @hControl, nStyle)
create CHECKBOX for window nWinNum
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
         dimension of CHECKBOX.
         defaults for CHECKBOX: {-2,-2,+2,+2}

bBlock:  codeblock to execute on every BN_CLICK event.
         This codeblock will be evaluated with these parameters:
         nWinNum: window number
         nCXid  : CHECKBOX id

returns control id of newly created CHECKBOX of windows nWinNum
returns 0 if failed
*/
HB_FUNC(WVW_CXCREATE)
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
                                    hb_parl(11) /* bMap3Dcolors */, BS_AUTOCHECKBOX | hb_parni(13) /* nStyle */,
                                    &hWnd));

    hb_strfree(hCaption);
  }
  else
  {
    hb_retni(0);
  }

  hbwapi_stor_HANDLE(hWnd, 12);
}

/*
wvw_cxDestroy([nWinNum], nCXid)
destroy checkbox nCXid for window nWinNum
*/
HB_FUNC(WVW_CXDESTROY)
{
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw_win)
  {
    auto nCtrlId = hb_parni(2);
    auto wvw_ctl = wvw_win->ctlList;
    PWVW_CTL wvw_ctlPrev = nullptr;

    while (wvw_ctl)
    {
      if (wvw_ctl->nClass == WVW_CONTROL_CHECKBOX && wvw_ctl->nId == nCtrlId)
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
wvw_cxSetFocus([nWinNum], nButtonId)
set the focus to checkbox nButtonId in window nWinNum
*/
HB_FUNC(WVW_CXSETFOCUS)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_CHECKBOX, hb_parni(2), nullptr);
  hb_retl(hWnd && SetFocus(hWnd) != nullptr);
}

/*
wvw_cxEnable([nWinNum], nButtonId, [lToggle])
enable/disable checkbox nButtonId on window nWinNum
(lToggle defaults to .T., ie. enabling the checkbox)
return previous state of the checkbox (.T.: enabled .F.: disabled)
(if nButtonId is invalid, this function returns .F. too)
*/
HB_FUNC(WVW_CXENABLE)
{
  auto wvw_win = hb_gt_wvw_win_par();
  auto hWnd = hb_gt_wvw_FindControlHandle(wvw_win, WVW_CONTROL_CHECKBOX, hb_parni(2), nullptr);

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
wvw_cxSetCodeblock([nWinNum], nCXid, bBlock)
assign (new) codeblock bBlock to button nCXid for window nWinNum
return .T. if successful
*/
HB_FUNC(WVW_CXSETCODEBLOCK)
{
  auto wvw = hb_gt_wvw();

  if (wvw)
  {
    auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_CHECKBOX, nullptr, hb_parni(2));
    auto pBlock = hb_param(3, Harbour::Item::EVALITEM);
    bool fOldSetting = wvw->fRecurseCBlock;

    if (pBlock && wvw_ctl && !wvw_ctl->fBusy)
    {
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
      return;
    }
  }

  hb_retl(false);
}

/*
wvw_cxSetCheck([nWinNum], nCXid, nCheckState)
assigns check-state of checkbox nCXid
        0==unchecked     BST_UNCHECKED
        1==checked       BST_CHECKED
        2==indeterminate BST_INDETERMINATE
this function always returns .T.
*/
HB_FUNC(WVW_CXSETCHECK)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_CHECKBOX, nullptr, hb_parni(2));

  if (wvw_ctl && wvw_ctl->hWnd)
  {
    SendMessage(wvw_ctl->hWnd, BM_SETCHECK, static_cast<WPARAM>(hb_parnidef(3, BST_CHECKED)), 0);
  }

  hb_retl(true);
}

/*
wvw_cxGetCheck([nWinNum], nCXid)
returns check-state of checkbox nCXid
          0==unchecked     BST_UNCHECKED
          1==checked       BST_CHECKED
          2==indeterminate BST_INDETERMINATE
*/
HB_FUNC(WVW_CXGETCHECK)
{
  auto wvw_ctl = hb_gt_wvw_ctl(hb_gt_wvw_win_par(), WVW_CONTROL_CHECKBOX, nullptr, hb_parni(2));

  if (wvw_ctl && wvw_ctl->hWnd)
  {
    hb_retni(static_cast<int>(SendMessage(wvw_ctl->hWnd, BM_GETCHECK, 0, 0)));
  }
  else
  {
    hb_retni(0);
  }
}

/*
wvw_cxSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, lItalic, lUnderline, lStrikeout)
*/
HB_FUNC(WVW_CXSETFONT)
{
  auto wvw = hb_gt_wvw();
  auto wvw_win = hb_gt_wvw_win_par();

  if (wvw && wvw_win)
  {
    wvw->lfCX.lfHeight = hb_parnldef(3, wvw_win->fontHeight - 2);
    wvw->lfCX.lfWidth = hb_parnldef(4, wvw->lfCX.lfWidth);
    wvw->lfCX.lfEscapement = 0;
    wvw->lfCX.lfOrientation = 0;
    wvw->lfCX.lfWeight = hb_parnldef(5, wvw->lfCX.lfWeight);
    wvw->lfCX.lfQuality = static_cast<BYTE>(hb_parnidef(6, wvw->lfCX.lfQuality));
    wvw->lfCX.lfItalic = static_cast<BYTE>(hb_parldef(7, wvw->lfCX.lfItalic));
    wvw->lfCX.lfUnderline = static_cast<BYTE>(hb_parldef(8, wvw->lfCX.lfUnderline));
    wvw->lfCX.lfStrikeOut = static_cast<BYTE>(hb_parldef(9, wvw->lfCX.lfStrikeOut));
    wvw->lfCX.lfCharSet = DEFAULT_CHARSET;
    wvw->lfCX.lfPitchAndFamily = FF_DONTCARE;

    if (HB_ISCHAR(2))
    {
      HB_ITEMCOPYSTR(hb_param(2, Harbour::Item::STRING), wvw->lfCX.lfFaceName, HB_SIZEOFARRAY(wvw->lfCX.lfFaceName));
      wvw_win->fontFace[HB_SIZEOFARRAY(wvw->lfCX.lfFaceName) - 1] = TEXT('\0');
    }

    if (wvw_win->hCXfont)
    {
      HFONT hOldFont = wvw_win->hCXfont;
      auto hFont = CreateFontIndirect(&wvw->lfCX);
      if (hFont)
      {
#if 0
            auto wvw_ctl = wvw_win->ctlList;

            while( wvw_ctl ) {
               if( wvw_ctl->nClass == WVW_CONTROL_PUSHBUTTON && static_cast<HFONT>(SendMessage(wvw_ctl->hWnd, WM_GETFONT, 0, 0)) == hOldFont ) {
                  SendMessage(wvw_ctl->hWnd, WM_SETFONT, static_cast<WPARAM>(hFont), static_cast<LPARAM>(TRUE));
               }

               wvw_ctl = wvw_ctl->pNext;
            }
#endif
        wvw_win->hCXfont = hFont;
        DeleteObject(hOldFont);

        hb_retl(true);
        return;
      }
    }
  }

  hb_retl(false);
}

HB_FUNC(WVW_CXSTATUSFONT)
{
  auto wvw_win = hb_gt_wvw_win_par();
  auto wvw_ctl = hb_gt_wvw_ctl(wvw_win, WVW_CONTROL_PUSHBUTTON, nullptr, hb_parni(2));

  if (wvw_ctl && wvw_ctl->hWnd)
  {
    SendMessage(wvw_ctl->hWnd, WM_SETFONT,
                reinterpret_cast<WPARAM>((hb_parldef(3, true) /* fFocus */ ? wvw_win->hCXfont : wvw_win->hPBfont)),
                static_cast<LPARAM>(TRUE));
  }

  hb_retl(true);
}

HB_FUNC(WVW_CXVISIBLE)
{
  auto hWnd = hb_gt_wvw_FindControlHandle(hb_gt_wvw_win_par(), WVW_CONTROL_PUSHBUTTON, hb_parni(2), nullptr);
  hb_retl(hWnd && ShowWindow(hWnd, hb_parldef(3, true) ? SW_SHOW : SW_HIDE) == 0);
}
