//
// Val() function
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
#include "hbapiitm.hpp"
#include "hbapierr.hpp"

// returns the numeric value of a character string representation of a number
static void hb_val(HB_BOOL fExt)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText)
  {
    auto szText = pText->getCPtr();
    auto iLen = static_cast<int>(pText->getCLen());
    HB_MAXINT lValue;
    double dValue;
    int iDec;
    int iWidth;
    bool fDbl = hb_valStrnToNum(szText, iLen, &lValue, &dValue, &iDec, &iWidth);

    if (fExt)
    {
      iLen = hb_parnidef(2, iLen);

      if (fDbl && iDec > 0)
      {
        iLen -= iDec + 1;
      }

      if (iLen > iWidth)
      {
        iWidth = iLen;
      }
      else if (iLen > 0)
      {
        while (iWidth > iLen && *szText == ' ')
        {
          iWidth--;
          szText++;
        }
      }
    }

    if (fDbl)
    {
      hb_retndlen(dValue, iWidth, iDec);
    }
    else
    {
      hb_retnintlen(lValue, iWidth);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 1098, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(VAL)
{
  hb_val(false);
}

HB_FUNC(HB_VAL)
{
  hb_val(true);
}
