//
// DispBox() function
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.hpp"
#include "hbapigt.hpp"
#include "hbapiitm.hpp"

HB_FUNC(DISPBOX)
{
  auto pTop = hb_param(1, Harbour::Item::NUMERIC);
  auto pLeft = hb_param(2, Harbour::Item::NUMERIC);
  auto pBottom = hb_param(3, Harbour::Item::NUMERIC);
  auto pRight = hb_param(4, Harbour::Item::NUMERIC);

  if (pTop && pLeft && pBottom && pRight) {
    auto pszBox = hb_parc(5);
    auto pszColor = hb_parc(6);

    if (pszBox) {
      int iColor;

      if (pszColor) {
        iColor = hb_gtColorToN(pszColor);
      } else if (HB_ISNUM(6)) {
        iColor = hb_parni(6);
      } else {
        iColor = -1;
      }
      hb_gtBoxEx(pTop->getNI(), pLeft->getNI(), pBottom->getNI(), pRight->getNI(), pszBox, iColor);
    } else {
      char szOldColor[HB_CLRSTR_LEN];

      if (pszColor) {
        hb_gtGetColorStr(szOldColor);
        hb_gtSetColorStr(pszColor);
      }

      if (hb_parni(5) == 2) {
        hb_gtBoxD(pTop->getNI(), pLeft->getNI(), pBottom->getNI(), pRight->getNI());
      } else {
        hb_gtBoxS(pTop->getNI(), pLeft->getNI(), pBottom->getNI(), pRight->getNI());
      }

      if (pszColor) {
        hb_gtSetColorStr(szOldColor);
      }
    }
  }
}

HB_FUNC(HB_DISPBOX)
{
  auto pTop = hb_param(1, Harbour::Item::NUMERIC);
  auto pLeft = hb_param(2, Harbour::Item::NUMERIC);
  auto pBottom = hb_param(3, Harbour::Item::NUMERIC);
  auto pRight = hb_param(4, Harbour::Item::NUMERIC);

  if (pTop && pLeft && pBottom && pRight) {
    auto pszBox = hb_parc(5);
    auto pszColor = hb_parc(6);
    int iColor = pszColor ? hb_gtColorToN(pszColor) : hb_parnidef(6, -1);

    hb_gtDrawBox(pTop->getNI(), pLeft->getNI(), pBottom->getNI(), pRight->getNI(), pszBox, iColor);
  }
}
