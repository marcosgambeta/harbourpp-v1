//
// CharMix() CT3 function
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) (Initial code)
// Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
//        Author: Martin Vogel <vogel@inttec.de> (CT3 conformity)
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

HB_FUNC(CHARMIX)
{
  if (HB_ISCHAR(1))
  {
    auto pcString1 = hb_parc(1);
    const char *pcString2;
    auto sLen1 = hb_parclen(1);
    HB_SIZE sLen2, sPos2, sResultPos;

    if (sLen1 == 0)
    {
      int iArgErrorMode = ct_getargerrormode();

      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error(static_cast<HB_USHORT>(iArgErrorMode), EG_ARG, CT_ERROR_CHARMIX, nullptr, HB_ERR_FUNCNAME, 0,
                 EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS);
      }

      hb_retc_null();
      return;
    }

    if (HB_ISCHAR(2))
    {
      pcString2 = hb_parc(2);
      sLen2 = hb_parclen(2);
      if (sLen2 == 0)
      {
        int iArgErrorMode = ct_getargerrormode();

        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error(static_cast<HB_USHORT>(iArgErrorMode), EG_ARG, CT_ERROR_CHARMIX, nullptr, HB_ERR_FUNCNAME, 0,
                   EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS);
        }

        hb_retclen(pcString1, sLen1);
        return;
      }
    }
    else
    {
      pcString2 = " "; // NOTE: The original CT3 uses " " as 2nd string
                       // if the 2nd param is not a string !
      sLen2 = 1;
    }

    auto pcResult = static_cast<char *>(hb_xgrab(sLen1 * 2 + 1));
    sPos2 = sResultPos = 0;
    for (HB_SIZE sPos1 = 0; sPos1 < sLen1;)
    {
      pcResult[sResultPos++] = pcString1[sPos1++];
      pcResult[sResultPos++] = pcString2[sPos2++];
      sPos2 %= sLen2;
    }

    hb_retclen_buffer(pcResult, sLen1 * 2);
  }
  else
  {
    PHB_ITEM pSubst = nullptr;
    int iArgErrorMode = ct_getargerrormode();

    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst(static_cast<HB_USHORT>(iArgErrorMode), EG_ARG, CT_ERROR_CHARMIX, nullptr, HB_ERR_FUNCNAME,
                              0, EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS);
    }

    if (pSubst != nullptr)
    {
      hb_itemReturnRelease(pSubst);
    }
    else
    {
      hb_retc_null();
    }
  }
}
