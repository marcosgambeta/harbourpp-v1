//
// OpenSSL API (ERR) - Harbour interface.
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
#include <openssl/err.h>

HB_FUNC(ERR_LOAD_CRYPTO_STRINGS)
{
  ERR_load_crypto_strings();
}

HB_FUNC(ERR_PRINT_ERRORS)
{
  auto bio = hb_BIO_par(1);

  if (bio != nullptr)
  {
    ERR_print_errors(bio);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(ERR_GET_ERROR)
{
  hb_retnint(ERR_get_error());
}

HB_FUNC(ERR_PEEK_ERROR)
{
  hb_retnint(ERR_peek_error());
}

HB_FUNC(ERR_PEEK_LAST_ERROR)
{
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
  hb_retnint(ERR_peek_last_error());
#else
  hb_retnint(-1);
#endif
}

HB_FUNC(ERR_ERROR_STRING)
{
  char buffer[120 + 1];

  buffer[0] = '\0';

  ERR_error_string_n(static_cast<unsigned long>(hb_parnint(1)), buffer, sizeof(buffer));

  hb_retc(buffer);
}

HB_FUNC(ERR_LIB_ERROR_STRING)
{
  hb_retc(ERR_lib_error_string(static_cast<unsigned long>(hb_parnint(1))));
}

HB_FUNC(ERR_FUNC_ERROR_STRING)
{
  hb_retc(ERR_lib_error_string(static_cast<unsigned long>(hb_parnint(1))));
}

HB_FUNC(ERR_REASON_ERROR_STRING)
{
  hb_retc(ERR_lib_error_string(static_cast<unsigned long>(hb_parnint(1))));
}

HB_FUNC(ERR_GET_ERROR_LINE)
{
  const char *file = nullptr;
  auto line = 0;

#if OPENSSL_VERSION_NUMBER < 0x30000000L
  hb_retnint(ERR_get_error_line(&file, &line));
#else
  hb_retni(0);
#endif

  hb_storc(file, 1);
  hb_storni(line, 2);
}

HB_FUNC(ERR_PEEK_ERROR_LINE)
{
  const char *file = nullptr;
  auto line = 0;

  hb_retnint(ERR_peek_error_line(&file, &line));

  hb_storc(file, 1);
  hb_storni(line, 2);
}

HB_FUNC(ERR_PEEK_LAST_ERROR_LINE)
{
  const char *file = nullptr;
  auto line = 0;

#if OPENSSL_VERSION_NUMBER >= 0x00907000L
  hb_retnint(ERR_peek_last_error_line(&file, &line));
#else
  hb_retni(-1);
#endif

  hb_storc(file, 1);
  hb_storni(line, 2);
}

HB_FUNC(ERR_GET_ERROR_LINE_DATA)
{
  const char *file = nullptr;
  auto line = 0;
  const char *data = nullptr;
  auto flags = 0;

#if OPENSSL_VERSION_NUMBER < 0x30000000L
  hb_retnint(ERR_get_error_line_data(&file, &line, &data, &flags));
#else
  hb_retni(0);
#endif

  hb_storc(file, 1);
  hb_storni(line, 2);
  hb_storc(data, 3);
  hb_storni(flags, 4);
}

HB_FUNC(ERR_PEEK_ERROR_LINE_DATA)
{
  const char *file = nullptr;
  auto line = 0;
  const char *data = nullptr;
  auto flags = 0;

#if OPENSSL_VERSION_NUMBER < 0x30000000L
  hb_retnint(ERR_peek_error_line_data(&file, &line, &data, &flags));
#else
  hb_retni(0);
#endif

  hb_storc(file, 1);
  hb_storni(line, 2);
  hb_storc(data, 3);
  hb_storni(flags, 4);
}

HB_FUNC(ERR_PEEK_LAST_ERROR_LINE_DATA)
{
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
  const char *file = nullptr;
  auto line = 0;
  const char *data = nullptr;
  auto flags = 0;

#if OPENSSL_VERSION_NUMBER < 0x30000000L
  hb_retnint(ERR_peek_last_error_line_data(&file, &line, &data, &flags));
#else
  hb_retni(0);
#endif

  hb_storc(file, 1);
  hb_storni(line, 2);
  hb_storc(data, 3);
  hb_storni(flags, 4);
#else
  hb_retnint(-1);

  hb_storc(nullptr, 1);
  hb_storni(0, 2);
  hb_storc(nullptr, 3);
  hb_storni(0, 4);
#endif
}

HB_FUNC(ERR_FREE_STRINGS)
{
  ERR_free_strings();
}
