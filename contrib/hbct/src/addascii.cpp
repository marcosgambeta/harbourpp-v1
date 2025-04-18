//
// AddAscii() CT3 string function
//
// Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
//        Author: Martin Vogel <vogel@inttec.de>
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

#include "ct.h"

HB_FUNC(ADDASCII)
{
  // suppressing return value ?
  int iNoRet = ct_getref() && HB_ISBYREF(1);

  if (HB_ISCHAR(1))
  {
    auto pcSource = hb_parc(1);
    auto sLen = hb_parclen(1);
    HB_SIZE sPos = hb_parnsdef(3, sLen);
    HB_LONG lValue;
    int iCarryOver;

    if (sPos > sLen || !HB_ISNUM(2) || sLen == 0)
    {
      int iArgErrorMode = ct_getargerrormode();

      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error(static_cast<HB_USHORT>(iArgErrorMode), EG_ARG, CT_ERROR_ADDASCII, nullptr, HB_ERR_FUNCNAME, 0,
                 EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS);
      }

      // return string unchanged
      if (iNoRet)
      {
        hb_retl(false);
      }
      else
      {
        hb_retclen(pcSource, sLen);
      }

      return;
    }

    auto pcResult = static_cast<char *>(hb_xgrab(sLen + 1));
    hb_xmemcpy(pcResult, pcSource, sLen);

    lValue = hb_parnl(2);
    iCarryOver = hb_parldef(4, 0);

    if (iCarryOver)
    {
      for (HB_SIZE sCurrent = sPos; sCurrent > 0 && lValue != 0; sCurrent--)
      {
        HB_LONG lResult = static_cast<HB_LONG>(pcSource[sCurrent - 1]) + (lValue % 256);

        lValue /= 256;
        if (lResult > 255)
        {
          lValue++;
        }
        else if (lResult < 0)
        {
          lValue--;
        }

        pcResult[sCurrent - 1] = static_cast<char>(lResult % 256);
      }
    }
    else
    {
      pcResult[sPos - 1] = static_cast<char>((static_cast<HB_LONG>(pcResult[sPos - 1]) + lValue) % 256);
    }

    hb_storclen(pcResult, sLen, 1);

    if (iNoRet)
    {
      hb_retl(false);
      hb_xfree(pcResult);
    }
    else
    {
      hb_retclen_buffer(pcResult, sLen);
    }
  }
  else
  {
    PHB_ITEM pSubst = nullptr;
    int iArgErrorMode = ct_getargerrormode();

    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst(static_cast<HB_USHORT>(iArgErrorMode), EG_ARG, CT_ERROR_ADDASCII, nullptr,
                              HB_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS);
    }

    if (pSubst != nullptr)
    {
      hb_itemReturnRelease(pSubst);
    }
    else if (iNoRet)
    {
      hb_retl(false);
    }
    else
    {
      hb_retc_null();
    }
  }
}
