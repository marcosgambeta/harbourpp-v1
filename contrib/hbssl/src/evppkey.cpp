//
// OpenSSL API (EVP PKEY) - Harbour interface.
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
#include <hbapiitm.hpp>
#include <openssl/evp.h>
#include <openssl/rsa.h>

static HB_GARBAGE_FUNC(EVP_PKEY_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph)
  {
    // Destroy the object
    EVP_PKEY_free(static_cast<EVP_PKEY *>(*ph));

    // set pointer to nullptr just in case
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcEVP_PKEY_funcs = {EVP_PKEY_release, hb_gcDummyMark};

HB_BOOL hb_EVP_PKEY_is(int iParam)
{
  return hb_parptrGC(&s_gcEVP_PKEY_funcs, iParam) != nullptr;
}

EVP_PKEY *hb_EVP_PKEY_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcEVP_PKEY_funcs, iParam));
  return ph ? static_cast<EVP_PKEY *>(*ph) : nullptr;
}

EVP_PKEY *hb_EVP_PKEY_get(PHB_ITEM pItem)
{
  auto ph = static_cast<void **>(hb_itemGetPtrGC(pItem, &s_gcEVP_PKEY_funcs));
  return ph ? static_cast<EVP_PKEY *>(*ph) : nullptr;
}

void hb_EVP_PKEY_free(PHB_ITEM pItem)
{
  auto ph = static_cast<void **>(hb_itemGetPtrGC(pItem, &s_gcEVP_PKEY_funcs));

  if (ph && *ph)
  {
    EVP_PKEY_free(static_cast<EVP_PKEY *>(*ph));
    *ph = nullptr;
  }
}

void hb_EVP_PKEY_ret(EVP_PKEY *pkey)
{
  auto ph = static_cast<void **>(hb_gcAllocate(sizeof(EVP_PKEY *), &s_gcEVP_PKEY_funcs));
  *ph = pkey;
  hb_retptrGC(ph);
}

static HB_GARBAGE_FUNC(EVP_PKEY_CTX_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph)
  {
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
    EVP_PKEY_CTX_free(static_cast<EVP_PKEY_CTX *>(*ph));
#endif
    // set pointer to nullptr just in case
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcEVP_PKEY_CTX_funcs = {EVP_PKEY_CTX_release, hb_gcDummyMark};

#if 0
static bool hb_EVP_PKEY_CTX_is(int iParam)
{
  return hb_parptrGC(&s_gcEVP_PKEY_CTX_funcs, iParam) != nullptr;
}
#endif

#if OPENSSL_VERSION_NUMBER >= 0x10000000L
static EVP_PKEY_CTX *hb_EVP_PKEY_CTX_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcEVP_PKEY_CTX_funcs, iParam));
  return ph ? static_cast<EVP_PKEY_CTX *>(*ph) : nullptr;
}

static void hb_EVP_PKEY_CTX_ret(EVP_PKEY_CTX *pkey)
{
  auto ph = static_cast<void **>(hb_gcAllocate(sizeof(EVP_PKEY_CTX *), &s_gcEVP_PKEY_CTX_funcs));
  *ph = pkey;
  hb_retptrGC(ph);
}
#endif

HB_FUNC(EVP_PKEY_NEW)
{
  hb_EVP_PKEY_ret(EVP_PKEY_new());
}

HB_FUNC(EVP_PKEY_TYPE)
{
  hb_retni(EVP_PKEY_type(hb_parni(1)));
}

HB_FUNC(EVP_PKEY_BASE_ID)
{
  if (hb_EVP_PKEY_is(1))
  {
    auto pkey = hb_EVP_PKEY_par(1);

    if (pkey != nullptr)
    {
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
      hb_retni(EVP_PKEY_base_id(pkey));
#else
      hb_retni(EVP_PKEY_type(hb_parni(1)));
#endif
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_SIZE)
{
  if (hb_EVP_PKEY_is(1))
  {
    auto pkey = hb_EVP_PKEY_par(1);

    if (pkey != nullptr)
    {
      hb_retni(EVP_PKEY_size(pkey));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_BITS)
{
  if (hb_EVP_PKEY_is(1))
  {
    auto pkey = hb_EVP_PKEY_par(1);

    if (pkey != nullptr)
    {
      hb_retni(EVP_PKEY_bits(pkey));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_ASSIGN)
{
  if (hb_EVP_PKEY_is(1))
  {
    auto pkey = hb_EVP_PKEY_par(1);

    if (pkey != nullptr)
    {
      // QUESTION: Is hb_openssl_strdup() okay here? [vszakats]
      hb_retni(EVP_PKEY_assign(pkey, hb_parni(2), hb_openssl_strdup(hb_parcx(3))));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_ASSIGN_RSA)
{
#ifndef OPENSSL_NO_RSA
  if (hb_EVP_PKEY_is(1) && hb_RSA_is(2))
  {
    auto pkey = hb_EVP_PKEY_par(1);
    RSA *key = hb_RSA_par(2);
    int res = 0;

    if (pkey != nullptr && key != nullptr)
    {
      res = EVP_PKEY_assign_RSA(pkey, key);

      if (res != 0)
      {
#if OPENSSL_VERSION_NUMBER >= 0x0090700fL
        RSA_up_ref(key);
#else
        hb_RSA_par_remove(2);
#endif
      }
    }
    hb_retni(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

HB_FUNC(EVP_PKEY_ASSIGN_DSA)
{
#ifndef OPENSSL_NO_DSA
  if (hb_EVP_PKEY_is(1) && HB_ISPOINTER(2))
  {
    auto pkey = hb_EVP_PKEY_par(1);
    auto key = static_cast<DSA *>(hb_parptr(2));

    if (pkey != nullptr && key != nullptr)
    {
      hb_retni(EVP_PKEY_assign_DSA(pkey, key));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

HB_FUNC(EVP_PKEY_ASSIGN_DH)
{
#ifndef OPENSSL_NO_RSA
  if (hb_EVP_PKEY_is(1) && HB_ISPOINTER(2))
  {
    auto pkey = hb_EVP_PKEY_par(1);
    auto key = static_cast<DH *>(hb_parptr(2));

    if (pkey != nullptr && key != nullptr)
    {
      hb_retni(EVP_PKEY_assign_DH(pkey, key));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

HB_FUNC(EVP_PKEY_CTX_NEW)
{
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
  auto pkey = hb_EVP_PKEY_par(1);

  if (pkey != nullptr)
  {
    hb_EVP_PKEY_CTX_ret(EVP_PKEY_CTX_new(pkey, static_cast<ENGINE *>(hb_parptr(2))));
  }
#else
  if (hb_RSA_is(1))
  {
    hb_itemReturn(hb_param(1, Harbour::Item::POINTER));
  }
#endif
  else
  {
    hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_ENCRYPT_INIT)
{
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
  auto ctx = hb_EVP_PKEY_CTX_par(1);

  if (ctx != nullptr)
  {
    hb_retni(EVP_PKEY_encrypt_init(ctx));
  }
#else
  if (hb_RSA_is(1))
  {
    hb_retni(1);
  }
#endif
  else
  {
    hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#if OPENSSL_VERSION_NUMBER >= 0x10100000L
#define HB_RSA_KEY_ISPRIVATE(rsa) (RSA_get0_d(rsa) != nullptr)
#else
#define HB_RSA_KEY_ISPRIVATE(rsa) ((rsa)->d != nullptr)
#endif

HB_FUNC(EVP_PKEY_ENCRYPT)
{
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
  auto ctx = hb_EVP_PKEY_CTX_par(1);

  if (ctx != nullptr)
  {
    auto in = reinterpret_cast<const unsigned char *>(hb_parcx(3));
    size_t inlen = static_cast<size_t>(hb_parclen(3)), outlen = 0;
    unsigned char *buffer = nullptr;

    int ret = EVP_PKEY_encrypt(ctx, nullptr, &outlen, in, inlen);
    if (ret > 0)
    {
      buffer = static_cast<unsigned char *>(hb_xgrab(outlen + 1));

      ret = EVP_PKEY_encrypt(ctx, buffer, &outlen, in, inlen);
      if (ret > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), outlen, 2))
        {
          ret = 0;
        }
      }
    }
    if (ret <= 0)
    {
      if (buffer)
      {
        hb_xfree(buffer);
      }
      hb_storc(nullptr, 2);
    }
    hb_retni(ret);
  }
#else
  if (hb_RSA_is(1))
  {
    auto rsa = hb_RSA_par(1);
    auto from = static_cast<const unsigned char *>(hb_parcx(3));
    auto flen = static_cast<int>(hb_parclen(3));
    int ret;

    auto buffer = static_cast<unsigned char *>(hb_xgrab(RSA_size(rsa) + 1));

    if (HB_RSA_KEY_ISPRIVATE(rsa))
      // private key
      ret = RSA_private_encrypt(flen, const_cast<unsigned char *>(from), buffer, rsa, RSA_PKCS1_PADDING);
    else
    {
      // public key
      ret = RSA_public_encrypt(flen, const_cast<unsigned char *>(from), buffer, rsa, RSA_PKCS1_PADDING);
    }

    if (ret > 0)
    {
      if (!hb_storclen_buffer(static_cast<char *>(buffer), ret, 2))
      {
        ret = 0;
      }
    }
    if (ret <= 0)
    {
      if (buffer)
      {
        hb_xfree(buffer);
      }
      hb_storc(nullptr, 2);
    }
    hb_retni(ret);
  }
#endif
  else
  {
    hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_DECRYPT_INIT)
{
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
  auto ctx = hb_EVP_PKEY_CTX_par(1);

  if (ctx != nullptr)
  {
    hb_retni(EVP_PKEY_decrypt_init(ctx));
  }
#else
  if (hb_RSA_is(1))
  {
    hb_retni(1);
  }
#endif
  else
  {
    hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(EVP_PKEY_DECRYPT)
{
#if OPENSSL_VERSION_NUMBER >= 0x10000000L
  auto ctx = hb_EVP_PKEY_CTX_par(1);

  if (ctx != nullptr)
  {
    auto in = reinterpret_cast<const unsigned char *>(hb_parcx(3));
    size_t inlen = static_cast<size_t>(hb_parclen(3)), outlen = 0;
    unsigned char *buffer = nullptr;

    int ret = EVP_PKEY_decrypt(ctx, nullptr, &outlen, in, inlen);
    if (ret > 0)
    {
      buffer = static_cast<unsigned char *>(hb_xgrab(outlen + 1));

      ret = EVP_PKEY_decrypt(ctx, buffer, &outlen, in, inlen);
      if (ret > 0)
      {
        if (!hb_storclen_buffer(reinterpret_cast<char *>(buffer), outlen, 2))
        {
          ret = 0;
        }
      }
    }
    if (ret <= 0)
    {
      if (buffer)
      {
        hb_xfree(buffer);
      }
      hb_storc(nullptr, 2);
    }
    hb_retni(ret);
  }
#else
  if (hb_RSA_is(1))
  {
    auto rsa = hb_RSA_par(1);
    auto from = static_cast<const unsigned char *>(hb_parcx(3));
    auto flen = static_cast<int>(hb_parclen(3));
    int ret;

    auto buffer = static_cast<unsigned char *>(hb_xgrab(RSA_size(rsa) + 1));

    if (HB_RSA_KEY_ISPRIVATE(rsa))
    {
      // private key
      ret = RSA_private_decrypt(flen, const_cast<unsigned char *>(from), buffer, rsa, RSA_PKCS1_PADDING);
    }
    else
    {
      // public key
      ret = RSA_public_decrypt(flen, const_cast<unsigned char *>(from), buffer, rsa, RSA_PKCS1_PADDING);
    }

    if (ret > 0)
    {
      buffer = static_cast<unsigned char *>(hb_xrealloc(buffer, ret + 1));
      if (!hb_storclen_buffer(static_cast<char *>(buffer), ret, 2))
      {
        ret = 0;
      }
    }
    if (ret <= 0)
    {
      if (buffer)
      {
        hb_xfree(buffer);
      }
      hb_storc(nullptr, 2);
    }
    hb_retni(ret);
  }
#endif
  else
  {
    hb_errRT_BASE(EG_NOFUNC, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#if 0

int EVP_PKEY_set1_RSA(EVP_PKEY * pkey, RSA * key);
int EVP_PKEY_set1_DSA(EVP_PKEY * pkey, DSA * key);
int EVP_PKEY_set1_DH(EVP_PKEY * pkey, DH * key);
int EVP_PKEY_set1_EC_KEY(EVP_PKEY * pkey, EC_KEY * key);

RSA * EVP_PKEY_get1_RSA(EVP_PKEY * pkey);
DSA * EVP_PKEY_get1_DSA(EVP_PKEY * pkey);
DH * EVP_PKEY_get1_DH(EVP_PKEY * pkey);
EC_KEY * EVP_PKEY_get1_EC_KEY(EVP_PKEY * pkey);

// These changed in 0.9.9 to something different, they weren't probably documented before.
int EVP_PKEY_decrypt(unsigned char * dec_key, const unsigned char * enc_key, int enc_key_len, EVP_PKEY * private_key);
int EVP_PKEY_encrypt(unsigned char * enc_key, const unsigned char * key, int key_len, EVP_PKEY * pub_key);

// 1.0.0
int EVP_PKEY_sign_init(EVP_PKEY_CTX * ctx);
int EVP_PKEY_sign(EVP_PKEY_CTX * ctx, unsigned char * sig, size_t * siglen, const unsigned char * tbs, size_t tbslen);

int EVP_PKEY_verify_init(EVP_PKEY_CTX * ctx);
int EVP_PKEY_verify(EVP_PKEY_CTX * ctx, const unsigned char * sig, size_t siglen, const unsigned char * tbs, size_t tbslen);

int EVP_PKEY_verify_recover_init(EVP_PKEY_CTX * ctx);
int EVP_PKEY_verify_recover(EVP_PKEY_CTX * ctx, unsigned char * rout, size_t * routlen, const unsigned char * sig, size_t siglen);

#endif
