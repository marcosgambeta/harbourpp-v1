//
// PRG functions for MD5 encryption/decryption using
// CFB (cipher feedback) mode
//
// Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbchksum.h"

static void hb_md5_init_seed(char *vect, const char *pszKey, int iLen)
{
  hb_md5(pszKey, iLen, vect);
}

static void hb_md5_next_seed(char *vect, const char *pszKey, int iLen)
{
  for (auto i = 0; i < 16; ++i)
  {
    vect[i] ^= pszKey[i % iLen];
  }
  hb_md5(vect, 16, vect);
}

// hb_MD5Encrypt(<cText>, <cPasswd>) --> <cCipher>
HB_FUNC(HB_MD5ENCRYPT)
{
  auto pData = hb_param(1, Harbour::Item::STRING);

  if (pData && hb_parclen(2) > 0)
  {
    auto nLen = pData->getCLen();

    if (nLen)
    {
      auto pszSource = pData->getCPtr();
      auto pszData = static_cast<char *>(hb_xgrab(nLen + 1));
      auto pszKey = hb_parc(2);
      auto iLen = static_cast<int>(hb_parclen(2));
      char vect[16];
      HB_SIZE n;

      hb_md5_init_seed(vect, pszKey, iLen);

      for (n = 0; n < nLen; ++n)
      {
        auto i = static_cast<int>(n & 0x0F);
        if (i == 0)
        {
          hb_md5_next_seed(vect, pszKey, iLen);
        }
        pszData[n] = (vect[i] ^= pszSource[n]);
      }
      hb_retclen_buffer(pszData, nLen);
    }
    else
    {
      hb_retc_null();
    }
  }
}

// hb_MD5Decrypt(<cCipher>, <cPasswd> ]) --> <cText>
HB_FUNC(HB_MD5DECRYPT)
{
  auto pData = hb_param(1, Harbour::Item::STRING);

  if (pData && hb_parclen(2) > 0)
  {
    auto nLen = pData->getCLen();

    if (nLen)
    {
      auto pszSource = pData->getCPtr();
      auto pszData = static_cast<char *>(hb_xgrab(nLen + 1));
      auto pszKey = hb_parc(2);
      auto iLen = static_cast<int>(hb_parclen(2));
      char vect[16];
      HB_SIZE n;

      hb_md5_init_seed(vect, pszKey, iLen);

      for (n = 0; n < nLen; ++n)
      {
        auto i = static_cast<int>(n & 0x0F);
        if (i == 0)
        {
          hb_md5_next_seed(vect, pszKey, iLen);
        }
        pszData[n] = (vect[i] ^ pszSource[n]);
        vect[i] = pszSource[n];
      }
      hb_retclen_buffer(pszData, nLen);
    }
    else
    {
      hb_retc_null();
    }
  }
}
