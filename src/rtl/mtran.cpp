//
// MemoTran() function
//
// Copyright 1999 Jose Lalin <dezac@corevia.com>
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

// NOTE: pszResult must have an allocated buffer of at least nStringLen

static HB_SIZE hb_strMemotran(char *pszResult, const char *pszString, HB_SIZE nStringLen, char cHardCR, char cSoftCR)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strMemotran(%p, %s, %" HB_PFS "u, %x, %x)", static_cast<void*>(pszResult), pszString, nStringLen, cHardCR, cSoftCR));
#endif

  HB_SIZE nStringPos = 0;
  HB_SIZE nResultPos = 0;

  while (nStringPos < nStringLen)
  {
    if (pszString[nStringPos] == HB_CHAR_HARD1 && pszString[nStringPos + 1] == HB_CHAR_HARD2)
    {
      pszResult[nResultPos++] = cHardCR;
      nStringPos += 2;
    }
    else if (pszString[nStringPos] == HB_CHAR_SOFT1 && pszString[nStringPos + 1] == HB_CHAR_SOFT2)
    {
      pszResult[nResultPos++] = cSoftCR;
      nStringPos += 2;
    }
    else
    {
      pszResult[nResultPos++] = pszString[nStringPos++];
    }
  }

  pszResult[nResultPos] = '\0';

  return nResultPos;
}

HB_FUNC(MEMOTRAN)
{
  auto pString = hb_param(1, Harbour::Item::STRING);

  if (pString)
  {
    auto nLen = pString->getCLen();
    auto pszResult = static_cast<char *>(hb_xgrab(nLen + 1));
    const char *pszRepl;
    char cHardCR = ';';
    char cSoftCR = ' ';

    pszRepl = hb_parc(2);
    if (pszRepl)
    {
      cHardCR = *pszRepl;
    }

    // CA-Cl*pper checks 3rd cSoftCR parameter only
    // if 2nd one cHardCR is specified [druzus]

#ifdef HB_CLP_STRICT
    if (pszRepl)
#endif
      pszRepl = hb_parc(3);
    if (pszRepl)
    {
      cSoftCR = *pszRepl;
    }

    nLen = hb_strMemotran(pszResult, pString->getCPtr(), nLen, cHardCR, cSoftCR);
    hb_retclen_buffer(pszResult, nLen);
  }
  else
  {
    hb_retc_null();
  }
}
