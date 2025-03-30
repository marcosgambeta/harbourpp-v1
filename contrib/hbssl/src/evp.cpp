//
// OpenSSL API (EVP) - Harbour interface.
//
// Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbssl.h"
#include <openssl/evp.h>

char *hb_openssl_strdup(const char *pszText)
{
  size_t len = strlen(pszText) + 1;
  auto pszDup = static_cast<char *>(OPENSSL_malloc(len));
  memcpy(pszDup, pszText, len);
  return pszDup;
}

HB_FUNC(OPENSSL_ADD_ALL_ALGORITHMS)
{
  OpenSSL_add_all_algorithms();
}

HB_FUNC(EVP_CLEANUP)
{
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  EVP_cleanup();
#endif
}

HB_FUNC(ERR_LOAD_EVP_STRINGS)
{
#if OPENSSL_VERSION_NUMBER < 0x30000000L
  ERR_load_EVP_strings();
#endif
}

HB_FUNC(EVP_PKEY_FREE)
{
  auto pKey = hb_param(1, Harbour::Item::POINTER);

  if (pKey != nullptr)
  {
    hb_EVP_PKEY_free(pKey);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_BYTESTOKEY)
{
  auto cipher = hb_EVP_CIPHER_par(1);
  auto md = hb_EVP_MD_par(2);

  if (cipher != nullptr && md != nullptr && (!HB_ISCHAR(3) || hb_parclen(3) == 8))
  {
    unsigned char key[EVP_MAX_KEY_LENGTH];
    unsigned char iv[EVP_MAX_IV_LENGTH];

    memset(key, 0, sizeof(key));
    memset(iv, 0, sizeof(iv));

    hb_retni(EVP_BytesToKey(cipher, static_cast<HB_SSL_CONST EVP_MD *>(md),
                            reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parc(3)) /* salt */,
                            reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parcx(4)) /* data */,
                            static_cast<int>(hb_parclen(4)), hb_parni(5) /* count */, key, iv));

    hb_storclen(reinterpret_cast<char *>(key), EVP_CIPHER_key_length(cipher), 6);
    hb_storclen(reinterpret_cast<char *>(iv), EVP_CIPHER_iv_length(cipher), 7);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
