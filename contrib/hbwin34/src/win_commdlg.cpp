//
// Common dialogs
//
// Copyright 2014 Viktor Szakats
// based on: Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
//

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

#include "hbwapi.hpp"
#include <hbapiitm.hpp>
#include <hbvm.hpp>

#include <commdlg.h>

#if defined(__MINGW32CE__)
// ChooseColorW() problem is fixed in current devel MINGW32CE version but
// people who use recent official release (0.50) needs it
#undef ChooseColor
BOOL WINAPI ChooseColor(LPCHOOSECOLORW);
#endif

#define _HB_CHOOSECOLOR_CB_PROP_ TEXT("__hbwin_win_ChooseColor_CB")

static UINT_PTR CALLBACK CCHookProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  UINT_PTR res;
  bool fInit = false;
  PHB_ITEM pBlock;

  if (msg == WM_INITDIALOG) {
    auto cc = reinterpret_cast<CHOOSECOLOR *>(lParam);
    SetProp(hWnd, _HB_CHOOSECOLOR_CB_PROP_, hb_itemNew(reinterpret_cast<PHB_ITEM>(cc->lCustData)));
    fInit = true;
  }

  if ((pBlock = static_cast<PHB_ITEM>(GetProp(hWnd, _HB_CHOOSECOLOR_CB_PROP_))) != nullptr && hb_vmRequestReenter()) {
    PHB_ITEM pWnd = hbwapi_itemPut_HANDLE(nullptr, hWnd);
    auto pMsg = hb_itemPutNInt(nullptr, msg);
    auto pLPa = hb_itemPutNInt(nullptr, wParam);
    auto pWPa = hb_itemPutNInt(nullptr, lParam);

    hb_evalBlock(pBlock, pWnd, pMsg, pLPa, pWPa);

    res = static_cast<UINT_PTR>(hbwapi_par_RESULT(-1));

    hb_itemRelease(pWnd);
    hb_itemRelease(pMsg);
    hb_itemRelease(pLPa);
    hb_itemRelease(pWPa);

    if (msg == WM_NCDESTROY) {
      RemoveProp(hWnd, _HB_CHOOSECOLOR_CB_PROP_);
      hb_itemRelease(pBlock);
    }

    hb_vmRequestRestore();
  } else {
    res = 0;
  }

  return fInit ? 1 : res;
}

HB_FUNC(WIN_CHOOSECOLOR)
{
  COLORREF crCustClr[16];

  void *hTpl;

  for (auto i = 0; i < static_cast<int>(HB_SIZEOFARRAY(crCustClr)); ++i) {
    crCustClr[i] = HB_ISARRAY(4) ? hbwapi_parv_COLORREF(4, i + 1) : RGB(0, 0, 0);
  }

  CHOOSECOLOR cc{};
  cc.lStructSize = sizeof(cc);
  cc.hwndOwner = hbwapi_par_raw_HWND(1);
  cc.hInstance = hbwapi_par_raw_HWND(2);
  cc.rgbResult = hbwapi_par_COLORREF(3);
  cc.lpCustColors = crCustClr;
  cc.Flags = hbwapi_par_WORD(5);
  cc.lCustData = static_cast<LPARAM>(reinterpret_cast<HB_PTRUINT>(hb_param(6, Harbour::Item::EVALITEM)));
  cc.lpfnHook = cc.lCustData ? CCHookProc : nullptr;
  cc.lpTemplateName = HB_PARSTR(7, &hTpl, nullptr);

  if (ChooseColor(&cc)) {
    hbwapi_ret_COLORREF(cc.rgbResult);
  } else {
    hbwapi_ret_COLORREF(-1);
  }

  hb_strfree(hTpl);
}
