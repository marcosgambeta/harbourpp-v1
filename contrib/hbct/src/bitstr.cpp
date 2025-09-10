//
// CT3 Number and bit manipulation functions:
//       CToBit(), BitToC()
//
// Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include <hbapi.hpp>

HB_FUNC(CTOBIT)
{
  auto nString = hb_parclen(1);
  int iResult = 0;

  if (nString > 0) {
    auto nPattern = hb_parclen(2);

    if (nPattern >= 1 && nPattern <= 16) {
      auto pszString = hb_parc(1);
      auto pszPattern = hb_parc(2);

      for (HB_SIZE n = 0; n < nString; ++n) {
        char c = pszString[n];
        int i = 0;

        do {
          if (pszPattern[i] == c) {
            iResult |= 1 << (static_cast<int>(nPattern) - i - 1);
            break;
          }
        } while (++i < static_cast<int>(nPattern));
      }
    } else {
      iResult = -1;
    }
  }
  hb_retni(iResult);
}

HB_FUNC(BITTOC)
{
  auto nPattern = hb_parclen(2);

  if (nPattern >= 1 && nPattern <= 16) {
    auto pszPattern = hb_parc(2);
    char szBuffer[16];
    char *pszResult = &szBuffer[sizeof(szBuffer)];
    auto iLen = 0;

    auto iValue = hb_parnidef(1, -1);
    if (iValue > 0xFFFF || iValue < 0) {
      iValue = 0;
    }

    if (hb_parl(3)) {
      while (nPattern-- > 0) {
        *--pszResult = (iValue & 1) ? pszPattern[nPattern] : ' ';
        ++iLen;
        iValue >>= 1;
      }
    } else {
      while (iValue != 0 && nPattern-- > 0) {
        if (iValue & 1) {
          *--pszResult = pszPattern[nPattern];
          ++iLen;
        }
        iValue >>= 1;
      }
    }
    hb_retclen(pszResult, iLen);
  } else {
    hb_retc_null();
  }
}
