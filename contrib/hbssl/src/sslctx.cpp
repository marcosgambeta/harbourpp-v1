//
// OpenSSL API (SSL_CTX) - Harbour interface.
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

// This must come before #include "hbssl.h".
// OpenSSL 1.1.x and upper don't require Windows headers anymore,
// but if #included, it still must come before its own headers.
// The Harbour wrapper code doesn't need the Windows headers, so
// they will be dropped once 1.0.2 is EOLed in 2019-12-31.
#include <hbdefs.hpp>
#if defined(HB_OS_WIN)
#include <windows.h>
#include <wincrypt.h>
#endif

#include "hbssl.h"
#include <hbapiitm.hpp>
#include <openssl/rsa.h>

static HB_GARBAGE_FUNC(SSL_CTX_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph)
  {
    // Destroy the object
    SSL_CTX_free(static_cast<SSL_CTX *>(*ph));

    // set pointer to nullptr just in case
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcSSL_CTX_funcs = {SSL_CTX_release, hb_gcDummyMark};

HB_BOOL hb_SSL_CTX_is(int iParam)
{
  return hb_parptrGC(&s_gcSSL_CTX_funcs, iParam) != nullptr;
}

SSL_CTX *hb_SSL_CTX_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcSSL_CTX_funcs, iParam));
  return ph ? static_cast<SSL_CTX *>(*ph) : nullptr;
}

SSL_CTX *hb_SSL_CTX_itemGet(PHB_ITEM pItem)
{
  auto ph = static_cast<void **>(hb_itemGetPtrGC(pItem, &s_gcSSL_CTX_funcs));
  return ph ? static_cast<SSL_CTX *>(*ph) : nullptr;
}

const SSL_METHOD *hb_ssl_method_id_to_ptr(int n)
{
  const SSL_METHOD *p;

  switch (n)
  {
#if OPENSSL_VERSION_NUMBER >= 0x10100000L
  case HB_SSL_CTX_NEW_METHOD_TLS:
    p = TLS_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_TLS_SERVER:
    p = TLS_server_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_TLS_CLIENT:
    p = TLS_client_method();
    break;
#else
  case HB_SSL_CTX_NEW_METHOD_SSLV23:
    p = SSLv23_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER:
    p = SSLv23_server_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV23_CLIENT:
    p = SSLv23_client_method();
    break;
#if OPENSSL_VERSION_NUMBER < 0x10000000L
  case HB_SSL_CTX_NEW_METHOD_SSLV2:
    p = SSLv2_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV2_SERVER:
    p = SSLv2_server_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV2_CLIENT:
    p = SSLv2_client_method();
    break;
#endif
#ifndef OPENSSL_NO_SSL3_METHOD
  case HB_SSL_CTX_NEW_METHOD_SSLV3:
    p = SSLv3_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV3_SERVER:
    p = SSLv3_server_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_SSLV3_CLIENT:
    p = SSLv3_client_method();
    break;
#endif
#ifndef OPENSSL_NO_TLS1_METHOD
  case HB_SSL_CTX_NEW_METHOD_TLSV1:
    p = TLSv1_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_TLSV1_SERVER:
    p = TLSv1_server_method();
    break;
  case HB_SSL_CTX_NEW_METHOD_TLSV1_CLIENT:
    p = TLSv1_client_method();
    break;
#endif
#endif
  default:
    p = SSLv23_method();
  }

  return p;
}

HB_FUNC(SSL_CTX_NEW)
{
  auto ph = static_cast<void **>(hb_gcAllocate(sizeof(SSL_CTX *), &s_gcSSL_CTX_funcs));

#if OPENSSL_VERSION_NUMBER < 0x10000000L
  SSL_CTX *ctx =
      SSL_CTX_new(static_cast<SSL_METHOD *>(hb_ssl_method_id_to_ptr(hb_parnidef(1, HB_SSL_CTX_NEW_METHOD_DEFAULT))));
#else
  SSL_CTX *ctx = SSL_CTX_new(hb_ssl_method_id_to_ptr(hb_parnidef(1, HB_SSL_CTX_NEW_METHOD_DEFAULT)));
#endif

  *ph = static_cast<void *>(ctx);

  hb_retptrGC(ph);
}

HB_FUNC(SSL_CTX_SET_SSL_VERSION)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
      hb_retni(SSL_CTX_set_ssl_version(ctx, static_cast<SSL_METHOD *>(hb_ssl_method_id_to_ptr(hb_parni(2)))));
#else
      hb_retni(SSL_CTX_set_ssl_version(ctx, hb_ssl_method_id_to_ptr(hb_parni(2))));
#endif
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_TIMEOUT)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retnl(SSL_CTX_get_timeout(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_TIMEOUT)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_set_timeout(ctx, hb_parnl(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_CIPHER_LIST)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr && hb_parclen(2) <= 255)
    {
      SSL_CTX_set_cipher_list(ctx, hb_parcx(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_ADD_SESSION)
{
  if (hb_SSL_CTX_is(1) && hb_SSL_SESSION_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto session = hb_SSL_SESSION_par(2);

    if (ctx != nullptr && session != nullptr)
    {
      hb_retni(SSL_CTX_add_session(ctx, session));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_REMOVE_SESSION)
{
  if (hb_SSL_CTX_is(1) && hb_SSL_SESSION_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto session = hb_SSL_SESSION_par(2);

    if (ctx != nullptr && session != nullptr)
    {
      hb_retni(SSL_CTX_remove_session(ctx, session));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_FLUSH_SESSIONS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_flush_sessions(ctx, hb_parnl(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_SESSION_CACHE_MODE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_get_session_cache_mode(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_SESSION_CACHE_MODE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_set_session_cache_mode(ctx, hb_parni(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_CHECK_PRIVATE_KEY)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_check_private_key(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_QUIET_SHUTDOWN)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_get_quiet_shutdown(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_VERIFY_MODE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_get_verify_mode(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_ACCEPT)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_accept(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_ACCEPT_GOOD)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_accept_good(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_ACCEPT_RENEGOTIATE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_accept_renegotiate(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_CACHE_FULL)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_cache_full(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_CB_HITS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_cb_hits(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_CONNECT)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_connect(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_CONNECT_GOOD)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_connect_good(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_CONNECT_RENEGOTIATE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_connect_renegotiate(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_GET_CACHE_SIZE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_get_cache_size(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_HITS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_hits(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_MISSES)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_misses(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_NUMBER)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_number(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_TIMEOUTS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_sess_timeouts(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_NEED_TMP_RSA)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retnl(SSL_CTX_need_tmp_RSA(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SESS_SET_CACHE_SIZE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_sess_set_cache_size(ctx, hb_parni(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_DEFAULT_READ_AHEAD)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_set_default_read_ahead(ctx, hb_parni(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_OPTIONS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retnint(SSL_CTX_get_options(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_OPTIONS)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
      SSL_CTX_set_options(ctx, static_cast<uint64_t>(hb_parnint(2)));
#else
      SSL_CTX_set_options(ctx, static_cast<unsigned long>(hb_parnl(2)));
#endif
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_QUIET_SHUTDOWN)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_set_quiet_shutdown(ctx, hb_parni(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_SET_MODE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      SSL_CTX_set_mode(ctx, hb_parnl(2));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_MODE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_parnl(SSL_CTX_get_mode(ctx));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_CERTIFICATE)
{
  if (hb_SSL_CTX_is(1) && hb_X509_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto x509 = hb_X509_par(2);

    if (ctx != nullptr && x509 != nullptr)
    {
      hb_retni(SSL_CTX_use_certificate(ctx, x509));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_ADD_CLIENT_CA)
{
  if (hb_SSL_CTX_is(1) && hb_X509_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto x509 = hb_X509_par(2);

    if (ctx != nullptr && x509 != nullptr)
    {
      hb_retni(SSL_CTX_add_client_CA(ctx, x509));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_GET_CLIENT_CA_LIST)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
#if OPENSSL_VERSION_NUMBER < 0x10000000L ||                                                                            \
    OPENSSL_VERSION_NUMBER >= 0x1000000FL // NOTE: Compilation error when tried with 1.0.0beta5
      STACK_OF(X509_NAME) *stack = SSL_CTX_get_client_CA_list(ctx);
      int len = sk_X509_NAME_num(stack);

      if (len > 0)
      {
        auto pArray = hb_itemArrayNew(sk_X509_NAME_num(stack));

        for (auto tmp = 0; tmp < len; tmp++)
        {
          hb_arraySetPtr(pArray, tmp + 1, sk_X509_NAME_value(stack, tmp));
        }

        hb_itemReturnRelease(pArray);
      }
      else
#endif
        hb_reta(0);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_ADD_EXTRA_CHAIN_CERT)
{
  if (hb_SSL_CTX_is(1) && hb_X509_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto x509 = hb_X509_par(2);

    if (ctx != nullptr && x509 != nullptr)
    {
      hb_retnl(SSL_CTX_add_extra_chain_cert(ctx, x509));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_CERTIFICATE_FILE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_certificate_file(ctx, hb_parc(2), hb_parni(3)));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_CERTIFICATE_CHAIN_FILE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_certificate_chain_file(ctx, hb_parc(2)));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_PRIVATEKEY_FILE)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_PrivateKey_file(ctx, hb_parc(2), hb_parni(3)));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_RSAPRIVATEKEY)
{
#ifndef OPENSSL_NO_RSA
  if (hb_SSL_CTX_is(1) && hb_RSA_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);
    auto rsa = hb_RSA_par(2);

    if (ctx != nullptr && rsa != nullptr)
    {
      hb_retni(SSL_CTX_use_RSAPrivateKey(ctx, rsa));
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

HB_FUNC(SSL_CTX_USE_RSAPRIVATEKEY_FILE)
{
#ifndef OPENSSL_NO_RSA
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_RSAPrivateKey_file(ctx, hb_parc(2), hb_parni(3)));
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

HB_FUNC(SSL_CTX_USE_RSAPRIVATEKEY_ASN1)
{
#ifndef OPENSSL_NO_RSA
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_RSAPrivateKey_ASN1(ctx, reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parc(2)),
                                              static_cast<int>(hb_parclen(2))));
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

HB_FUNC(SSL_CTX_USE_PRIVATEKEY_ASN1)
{
  if (hb_SSL_CTX_is(2))
  {
    auto ctx = hb_SSL_CTX_par(2);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_PrivateKey_ASN1(hb_parni(1), ctx, reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parc(3)),
                                           static_cast<int>(hb_parclen(3))));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_CERTIFICATE_ASN1)
{
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_use_certificate_ASN1(ctx, static_cast<int>(hb_parclen(2)),
                                            reinterpret_cast<HB_SSL_CONST unsigned char *>(hb_parc(2))));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_USE_PRIVATEKEY)
{
  if (hb_SSL_CTX_is(1) && hb_EVP_PKEY_is(2))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      // QUESTION: It's unclear whether we should pass a copy here,
      //           and who should free such passed EV_PKEY object.
      //           [vszakats]
      hb_retni(SSL_CTX_use_PrivateKey(ctx, hb_EVP_PKEY_par(2)));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SSL_CTX_LOAD_VERIFY_LOCATIONS)
{
#ifndef OPENSSL_NO_STDIO
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_load_verify_locations(ctx, hb_parc(2) /* CAfile */, hb_parc(3) /* CApath */));
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

HB_FUNC(SSL_CTX_SET_DEFAULT_VERIFY_PATHS)
{
#ifndef OPENSSL_NO_STDIO
  if (hb_SSL_CTX_is(1))
  {
    auto ctx = hb_SSL_CTX_par(1);

    if (ctx != nullptr)
    {
      hb_retni(SSL_CTX_set_default_verify_paths(ctx));
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

#if 0

#define sk_X509_NAME_new_null() SKM_sk_new_null(X509_NAME)
#define sk_X509_NAME_push(st, val) SKM_sk_push(X509_NAME, (st), (val))
#define sk_X509_NAME_free(st) SKM_sk_free(X509_NAME, (st))

X509_STORE * SSL_CTX_get_cert_store(const SSL_CTX *);
void SSL_CTX_set_cert_store(SSL_CTX *, X509_STORE *);
void SSL_CTX_set_cert_store(SSL_CTX * ctx, X509_STORE * cs);
long SSL_CTX_ctrl(SSL_CTX * ctx, int cmd, long larg, char * parg);

void SSL_CTX_set_app_data(SSL_CTX * ctx, void * arg);
int SSL_CTX_set_ex_data(SSL_CTX * s, int idx, char * arg);
char * SSL_CTX_get_app_data(ctx);
char * SSL_CTX_get_ex_data(ctx, int);

int(*SSL_CTX_get_client_cert_cb(SSL_CTX * ctx))(SSL * ssl, X509 * *x509, EVP_PKEY * *pkey);
int SSL_CTX_get_ex_new_index(long argl, char * argp, int (*new_func); (void), int (* dup_func)(void), void (* free_func)(void))
void(*SSL_CTX_get_info_callback(SSL_CTX * ctx))(SSL * ssl, int cb, int ret);
int(*SSL_CTX_get_verify_callback(const SSL_CTX * ctx))(int ok, X509_STORE_CTX * ctx);
SSL_SESSION *(*SSL_CTX_sess_get_get_cb(SSL_CTX * ctx))(SSL * ssl, unsigned char * data, int len, int * copy);
int (*SSL_CTX_sess_get_new_cb(SSL_CTX * ctx)(SSL * ssl, SSL_SESSION * sess);
void (*SSL_CTX_sess_get_remove_cb(SSL_CTX * ctx)(SSL_CTX * ctx, SSL_SESSION * sess);
void SSL_CTX_sess_set_get_cb(SSL_CTX * ctx, SSL_SESSION * (*cb)(SSL * ssl, unsigned char * data, int len, int * copy));
void SSL_CTX_sess_set_new_cb(SSL_CTX * ctx, int (* cb)(SSL * ssl, SSL_SESSION * sess));
void SSL_CTX_sess_set_remove_cb(SSL_CTX * ctx, void (* cb)(SSL_CTX * ctx, SSL_SESSION * sess));
LHASH * SSL_CTX_sessions(SSL_CTX * ctx);
void SSL_CTX_set_cert_verify_cb(SSL_CTX * ctx, int (* cb)(), char * arg)
void SSL_CTX_set_client_CA_list(SSL_CTX * ctx, STACK * list);
void SSL_CTX_set_client_cert_cb(SSL_CTX * ctx, int (* cb)(SSL * ssl, X509 ** x509, EVP_PKEY ** pkey));
void SSL_CTX_set_default_passwd_cb(SSL_CTX * ctx, int (*cb); (void))
void SSL_CTX_set_info_callback(SSL_CTX * ctx, void (* cb)(SSL * ssl, int cb, int ret));
void SSL_CTX_set_msg_callback(SSL_CTX * ctx, void (* cb)(int write_p, int version, int content_type, const void * buf, size_t len, SSL * ssl, void * arg));
void SSL_CTX_set_msg_callback_arg(SSL_CTX * ctx, void * arg);
long SSL_CTX_set_tmp_dh(SSL_CTX * ctx, DH * dh);
long SSL_CTX_set_tmp_dh_callback(SSL_CTX * ctx, DH * (*cb)(void));
long SSL_CTX_set_tmp_rsa(SSL_CTX * ctx, RSA * rsa);
// SSL_CTX_set_tmp_rsa_callback
long SSL_CTX_set_tmp_rsa_callback(SSL_CTX * ctx, RSA * (*cb)(SSL * ssl, int export, int keylength));
long SSL_set_tmp_rsa_callback(SSL * ssl, RSA * (*cb)(SSL * ssl, int export, int keylength));
The same as SSL_CTX_set_tmp_rsa_callback, except it operates on an SSL session instead of a context.
void SSL_CTX_set_verify(SSL_CTX * ctx, int mode, int (*cb); (void))
void SSL_CTX_set_psk_client_callback(SSL_CTX * ctx, unsigned int (* callback)(SSL * ssl, const char * hint, char * identity, unsigned int max_identity_len, unsigned char * psk, unsigned int max_psk_len));
void SSL_CTX_set_psk_server_callback(SSL_CTX * ctx, unsigned int (* callback)(SSL * ssl, const char * identity, unsigned char * psk, int max_psk_len));

#endif
