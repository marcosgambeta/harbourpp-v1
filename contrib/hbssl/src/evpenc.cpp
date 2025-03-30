/*
 * OpenSSL API (EVP ENCODE) - Harbour interface.
 *
 * Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour)
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

#include "hbssl.h"
#include <hbapiitm.hpp>
#include <openssl/evp.h>

static HB_GARBAGE_FUNC(EVP_ENCODE_CTX_release)
{
  auto ph = static_cast<void **>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (ph && *ph)
  {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L &&                                                                           \
    (!defined(LIBRESSL_VERSION_NUMBER) || LIBRESSL_VERSION_NUMBER >= 0x20900000L)
    EVP_ENCODE_CTX_free(static_cast<EVP_ENCODE_CTX *>(*ph));
#else
    /* Destroy the object */
    hb_xfree(*ph);
#endif

    /* set pointer to nullptr just in case */
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcEVP_ENCODE_CTX_funcs = {EVP_ENCODE_CTX_release, hb_gcDummyMark};

static bool hb_EVP_ENCODE_CTX_is(int iParam)
{
  return hb_parptrGC(&s_gcEVP_ENCODE_CTX_funcs, iParam) != nullptr;
}

static EVP_ENCODE_CTX *hb_EVP_ENCODE_CTX_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcEVP_ENCODE_CTX_funcs, iParam));
  return ph ? static_cast<EVP_ENCODE_CTX *>(*ph) : nullptr;
}

HB_FUNC(EVP_ENCODE_CTX_NEW)
{
  auto ph = static_cast<void **>(hb_gcAllocate(sizeof(EVP_ENCODE_CTX *), &s_gcEVP_ENCODE_CTX_funcs));

#if OPENSSL_VERSION_NUMBER >= 0x10100000L &&                                                                           \
    (!defined(LIBRESSL_VERSION_NUMBER) || LIBRESSL_VERSION_NUMBER >= 0x20900000L)
  auto ctx = EVP_ENCODE_CTX_new();
#else
  auto ctx = static_cast<EVP_ENCODE_CTX *>(hb_xgrabz(sizeof(EVP_ENCODE_CTX)));
#endif

  *ph = ctx;

  hb_retptrGC(ph);
}

HB_FUNC_TRANSLATE(HB_EVP_ENCODE_CTX_CREATE, EVP_ENCODE_CTX_NEW)

HB_FUNC(EVP_ENCODEINIT)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      EVP_EncodeInit(ctx);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_ENCODEUPDATE)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      int size = 512;
      auto buffer = static_cast<unsigned char *>(hb_xgrab(size + 1));
      int result;

#if OPENSSL_VERSION_NUMBER >= 0x10100000L &&                                                                           \
    (!defined(LIBRESSL_VERSION_NUMBER) || LIBRESSL_VERSION_NUMBER >= 0x20900000L)
      result = EVP_EncodeUpdate(ctx, buffer, &size, reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parcx(3)),
                                static_cast<int>(hb_parclen(3)));
#else
      EVP_EncodeUpdate(ctx, buffer, &size, reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parcx(3)),
                       static_cast<int>(hb_parclen(3)));
      result = 1; /* Success */
#endif
      hb_retni(result);

      if (size > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), size, 2))
        {
          hb_xfree(buffer);
        }
      }
      else
      {
        hb_xfree(buffer);
        hb_storc(nullptr, 2);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_ENCODEFINAL)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      int size = 512;
      auto buffer = static_cast<unsigned char *>(hb_xgrab(size + 1));

      EVP_EncodeFinal(ctx, buffer, &size);

      if (size > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), size, 2))
        {
          hb_xfree(buffer);
        }
      }
      else
      {
        hb_xfree(buffer);
        hb_storc(nullptr, 2);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_DECODEINIT)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      EVP_DecodeInit(ctx);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_DECODEUPDATE)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      int size = 512;
      auto buffer = static_cast<unsigned char *>(hb_xgrab(size + 1));

      EVP_DecodeUpdate(ctx, buffer, &size, reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parcx(3)),
                       static_cast<int>(hb_parclen(3)));

      if (size > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), size, 2))
        {
          hb_xfree(buffer);
        }
      }
      else
      {
        hb_xfree(buffer);
        hb_storc(nullptr, 2);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_DECODEFINAL)
{
  if (hb_EVP_ENCODE_CTX_is(1))
  {
    auto ctx = hb_EVP_ENCODE_CTX_par(1);

    if (ctx != nullptr)
    {
      int size = 512;
      auto buffer = static_cast<unsigned char *>(hb_xgrab(size + 1));

      EVP_DecodeFinal(ctx, buffer, &size);

      if (size > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), size, 2))
        {
          hb_xfree(buffer);
        }
      }
      else
      {
        hb_xfree(buffer);
        hb_storc(nullptr, 2);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
