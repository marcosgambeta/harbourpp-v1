/*
 * PostgreSQL RDBMS low-level (client API) interface code.
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 * Copyright 2010-2016 Viktor Szakats (vsz.me/hb) (GC support, etc)
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbpgsql.hpp"
#include "hbapierr.hpp"
#include "hbapiitm.hpp"

static HB_GARBAGE_FUNC(PGconn_release)
{
  auto ph = static_cast<void **>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (ph && *ph)
  {
    /* Destroy the object */
    PQfinish(static_cast<PGconn *>(*ph));

    /* set pointer to nullptr to avoid multiple freeing */
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcPGconnFuncs = {PGconn_release, hb_gcDummyMark};

void hb_PGconn_ret(PGconn *p)
{
  if (p)
  {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(PGconn *), &s_gcPGconnFuncs));
    *ph = p;
    hb_retptrGC(ph);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

PGconn *hb_PGconn_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcPGconnFuncs, iParam));
  return ph ? static_cast<PGconn *>(*ph) : nullptr;
}

static HB_GARBAGE_FUNC(PGresult_release)
{
  auto ph = static_cast<void **>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (ph && *ph)
  {
    /* Destroy the object */
    PQclear(static_cast<PGresult *>(*ph));

    /* set pointer to nullptr to avoid multiple freeing */
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcPGresultFuncs = {PGresult_release, hb_gcDummyMark};

void hb_PGresult_ret(PGresult *p)
{
  if (p)
  {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(PGresult *), &s_gcPGresultFuncs));
    *ph = p;
    hb_retptrGC(ph);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

PGresult *hb_PGresult_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcPGresultFuncs, iParam));
  return ph ? static_cast<PGresult *>(*ph) : nullptr;
}

#if PG_VERSION_NUM >= 80000

static HB_GARBAGE_FUNC(PGcancel_release)
{
  auto ph = static_cast<void **>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (ph && *ph)
  {
    /* Destroy the object */
    PQfreeCancel(static_cast<PGcancel *>(*ph));

    /* set pointer to nullptr to avoid multiple freeing */
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcPGcancelFuncs = {PGcancel_release, hb_gcDummyMark};

static void hb_PGcancel_ret(PGcancel *p)
{
  if (p)
  {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(PGcancel *), &s_gcPGcancelFuncs));
    *ph = p;
    hb_retptrGC(ph);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

static PGcancel *hb_PGcancel_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcPGcancelFuncs, iParam));
  return ph ? static_cast<PGcancel *>(*ph) : nullptr;
}

#endif

#ifdef NODLL

static HB_GARBAGE_FUNC(FILE_release)
{
  auto ph = static_cast<void **>(Cargo);

  /* Check if pointer is not nullptr to avoid multiple freeing */
  if (ph && *ph)
  {
    /* Destroy the object */
    fclose(static_cast<FILE *>(*ph));

    /* set pointer to nullptr to avoid multiple freeing */
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gcFILEFuncs = {FILE_release, hb_gcDummyMark};

static void hb_FILE_ret(FILE *p)
{
  if (p)
  {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(FILE *), &s_gcFILEFuncs));
    *ph = p;
    hb_retptrGC(ph);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

static FILE *hb_FILE_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcFILEFuncs, iParam));
  return ph ? static_cast<FILE *>(*ph) : nullptr;
}

#endif

/* Get the version of the libpq library in use */

/*
PQLIBVERSION() -->
*/
HB_FUNC(PQLIBVERSION)
{
#if PG_VERSION_NUM >= 90100
  hb_retni(PQlibVersion());
#else
  hb_retni(0);
#endif
}

/* Connection handling functions */

/* 31.1. Database Connection Control Functions
   The following functions deal with making a connection to a PostgreSQL backend server. */

/*
PQCONNECTDBPARAMS() -->
*/
HB_FUNC(PQCONNECTDBPARAMS)
{
  auto pParam = hb_param(1, Harbour::Item::HASH);
  int len;

  if (pParam && (len = static_cast<int>(hb_hashLen(pParam))) > 0)
  {
#if PG_VERSION_NUM >= 90000
    auto paramKeyValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));
    auto paramValValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));

    for (auto pos = 0; pos < len; ++pos)
    {
      paramKeyValues[pos] = hb_itemGetCPtr(hb_hashGetKeyAt(pParam, pos + 1));
      paramValValues[pos] = hb_itemGetCPtr(hb_hashGetValueAt(pParam, pos + 1));
    }

    hb_PGconn_ret(PQconnectdbParams(paramKeyValues, paramValValues, hb_parl(2)));

    hb_xfree(static_cast<void *>(paramKeyValues));
    hb_xfree(static_cast<void *>(paramValValues));
#else
    hb_retptr(nullptr);
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONNECTDB() -->
*/
HB_FUNC(PQCONNECTDB)
{
  if (HB_ISCHAR(1))
  {
    hb_PGconn_ret(PQconnectdb(hb_parc(1)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* NOTE: Deprecated */

/*
PQSETDBLOGIN() -->
*/
HB_FUNC(PQSETDBLOGIN)
{
  hb_PGconn_ret(PQsetdbLogin(hb_parcx(1) /* pghost */, hb_parcx(2) /* pgport */, hb_parcx(3) /* pgoptions */,
                             hb_parcx(4) /* pgtty */, hb_parcx(5) /* dbName */, hb_parcx(6) /* login */,
                             hb_parcx(7) /* pwd */));
}

/*
PQCONNECTSTARTPARAMS() -->
*/
HB_FUNC(PQCONNECTSTARTPARAMS)
{
  auto pParam = hb_param(1, Harbour::Item::HASH);
  int len;

  if (pParam && (len = static_cast<int>(hb_hashLen(pParam))) > 0)
  {
#if PG_VERSION_NUM >= 90000
    auto paramKeyValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));
    auto paramValValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));

    for (auto pos = 0; pos < len; ++pos)
    {
      paramKeyValues[pos] = hb_itemGetCPtr(hb_hashGetKeyAt(pParam, pos + 1));
      paramValValues[pos] = hb_itemGetCPtr(hb_hashGetValueAt(pParam, pos + 1));
    }

    hb_PGconn_ret(PQconnectStartParams(paramKeyValues, paramValValues, hb_parl(2)));

    hb_xfree(static_cast<void *>(paramKeyValues));
    hb_xfree(static_cast<void *>(paramValValues));
#else
    hb_retptr(nullptr);
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONNECTSTART() -->
*/
HB_FUNC(PQCONNECTSTART)
{
  if (HB_ISCHAR(1))
  {
    hb_PGconn_ret(PQconnectStart(hb_parc(1)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONNECTPOLL() -->
*/
HB_FUNC(PQCONNECTPOLL)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQconnectPoll(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESET() -->
*/
HB_FUNC(PQRESET)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    PQreset(conn);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESETSTART() -->
*/
HB_FUNC(PQRESETSTART)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    PQresetStart(conn);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESETPOLL() -->
*/
HB_FUNC(PQRESETPOLL)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQresetPoll(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPINGPARAMS() -->
*/
HB_FUNC(PQPINGPARAMS)
{
  auto pParam = hb_param(1, Harbour::Item::HASH);
  int len;

  if (pParam && (len = static_cast<int>(hb_hashLen(pParam))) > 0)
  {
#if PG_VERSION_NUM >= 90100
    auto paramKeyValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));
    auto paramValValues = static_cast<const char **>(hb_xgrab(sizeof(char *) * len));

    for (auto pos = 0; pos < len; ++pos)
    {
      paramKeyValues[pos] = hb_itemGetCPtr(hb_hashGetKeyAt(pParam, pos + 1));
      paramValValues[pos] = hb_itemGetCPtr(hb_hashGetValueAt(pParam, pos + 1));
    }

    hb_retni(PQpingParams(paramKeyValues, paramValValues, hb_parl(3)));

    hb_xfree(static_cast<void *>(paramKeyValues));
    hb_xfree(static_cast<void *>(paramValValues));
#else
    hb_retptr(nullptr);
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPING() -->
*/
HB_FUNC(PQPING)
{
  if (HB_ISCHAR(1))
  {
#if PG_VERSION_NUM >= 90100
    hb_retni(PQping(hb_parc(1)));
#else
    hb_ret();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* 31.2. Connection Status Functions.
   These functions can be used to interrogate the status of an existing database connection object. */

/*
PQCLIENTENCODING() -->
*/
HB_FUNC(PQCLIENTENCODING)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQclientEncoding(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQSETCLIENTENCODING() -->
*/
HB_FUNC(PQSETCLIENTENCODING)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQsetClientEncoding(conn, hb_parcx(2)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQDB() -->
*/
HB_FUNC(PQDB)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQdb(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQUSER() -->
*/
HB_FUNC(PQUSER)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQuser(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPASS() -->
*/
HB_FUNC(PQPASS)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQpass(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQHOST() -->
*/
HB_FUNC(PQHOST)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQhost(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPORT() -->
*/
HB_FUNC(PQPORT)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQport(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQTTY() -->
*/
HB_FUNC(PQTTY)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQtty(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQOPTIONS() -->
*/
HB_FUNC(PQOPTIONS)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQoptions(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESULTERRORFIELD() -->
*/
HB_FUNC(PQRESULTERRORFIELD)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
#if PG_VERSION_NUM >= 70400
    hb_retc(PQresultErrorField(res, hb_parni(2)));
#else
    hb_retc_null();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESSTATUS() -->
*/
HB_FUNC(PQRESSTATUS)
{
  hb_retc(PQresStatus(static_cast<ExecStatusType>(hb_parnl(1))));
}

/* 31.2. Connection Status Functions.
   These functions can be used to interrogate the status of an existing database connection object. */

/*
PQSTATUS() -->
*/
HB_FUNC(PQSTATUS)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQstatus(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQTRANSACTIONSTATUS() -->
*/
HB_FUNC(PQTRANSACTIONSTATUS)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQtransactionStatus(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPARAMETERSTATUS() -->
*/
HB_FUNC(PQPARAMETERSTATUS)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQparameterStatus(conn, hb_parcx(2)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPROTOCOLVERSION() -->
*/
HB_FUNC(PQPROTOCOLVERSION)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQprotocolVersion(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQSERVERVERSION() -->
*/
HB_FUNC(PQSERVERVERSION)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
#if PG_VERSION_NUM >= 80000
    hb_retni(PQserverVersion(conn));
#else
    hb_retni(0);
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQERRORMESSAGE() -->
*/
HB_FUNC(PQERRORMESSAGE)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retc(PQerrorMessage(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQSOCKET() -->
*/
HB_FUNC(PQSOCKET)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQsocket(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQBACKENDPID() -->
*/
HB_FUNC(PQBACKENDPID)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQbackendPID(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONNECTIONNEEDSPASSWORD() -->
*/
HB_FUNC(PQCONNECTIONNEEDSPASSWORD)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
#if PG_VERSION_NUM >= 80300
    hb_retl(PQconnectionNeedsPassword(conn) ? true : false);
#else
    hb_ret();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONNECTIONUSEDPASSWORD() -->
*/
HB_FUNC(PQCONNECTIONUSEDPASSWORD)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
#if PG_VERSION_NUM >= 80300
    hb_retl(PQconnectionUsedPassword(conn) ? true : false);
#else
    hb_ret();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQSSLINUSE() -->
*/
HB_FUNC(PQSSLINUSE)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
#if PG_VERSION_NUM >= 90500
    hb_retl(PQsslInUse(conn) ? true : false);
#else
    hb_ret();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQSSLATTRIBUTE() -->
*/
HB_FUNC(PQSSLATTRIBUTE)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
#if PG_VERSION_NUM >= 90500
    hb_retc(PQsslAttribute(conn, hb_parcx(2)));
#else
    hb_retc_null();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Query handling functions */

/*
PQEXEC() -->
*/
HB_FUNC(PQEXEC)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_PGresult_ret(PQexec(conn, hb_parcx(2)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQEXECPARAMS() -->
*/
HB_FUNC(PQEXECPARAMS)
{
  auto conn = hb_PGconn_par(1);
  auto aParam = hb_param(3, Harbour::Item::ARRAY);

  if (conn && aParam)
  {
    auto n = static_cast<int>(hb_arrayLen(aParam));

    if (!n)
    {
      hb_PGresult_ret(PQexec(conn, hb_parcx(2)));
    }
    else
    {
      auto paramvalues = static_cast<const char **>(hb_xgrab(sizeof(char *) * n));

      for (auto i = 0; i < n; ++i)
      {
        paramvalues[i] = hb_arrayGetCPtr(aParam, i + 1);
      }

      hb_PGresult_ret(PQexecParams(conn, hb_parcx(2), n, nullptr, paramvalues, nullptr, nullptr, hb_parnidef(4, 1)));

      hb_xfree(static_cast<void *>(paramvalues));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFCOUNT() -->
*/
HB_FUNC(PQFCOUNT) /* not a direct wrapper */
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    auto nFields = 0;

    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      nFields = PQnfields(res);
    }

    hb_retni(nFields);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQLASTREC() -->
*/
HB_FUNC(PQLASTREC) /* not a direct wrapper */
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    auto nRows = 0;

    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      nRows = PQntuples(res);
    }

    hb_retni(nRows);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQGETVALUE() -->
*/
HB_FUNC(PQGETVALUE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      int nRow = hb_parni(2) - 1;
      int nCol = hb_parni(3) - 1;

      if (!PQgetisnull(res, nRow, nCol))
      {
        hb_retc(PQgetvalue(res, nRow, nCol));
      }
      else
      {
        hb_ret();
      }
    }
    else
    {
      hb_ret();
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQGETLENGTH() -->
*/
HB_FUNC(PQGETLENGTH)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    auto result = 0;

    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      int nRow = hb_parni(2) - 1;
      int nCol = hb_parni(3) - 1;

      result = PQgetlength(res, nRow, nCol);
    }

    hb_retni(result);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* PQmetadata() positions for array returned */
#define HBPG_META_FIELDNAME 1
#define HBPG_META_FIELDTYPE 2
#define HBPG_META_FIELDLEN 3
#define HBPG_META_FIELDDEC 4
#define HBPG_META_TABLE 5
#define HBPG_META_TABLECOL 6
#define HBPG_META_LEN_ 6

/*
PQMETADATA() -->
*/
HB_FUNC(PQMETADATA) /* not a direct wrapper */
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      int nFields = PQnfields(res), i;
      auto pResult = hb_itemArrayNew(nFields);

      for (i = 0; i < nFields; i++)
      {
        char buf[256];
        int typemod = PQfmod(res, i);
        auto length = 0;
        auto decimal = 0;

        switch (PQftype(res, i))
        {
        case BITOID:
          if (typemod >= 0)
          {
            length = static_cast<int>(typemod);
          }
          hb_strncpy(buf, "bit", sizeof(buf) - 1);
          break;

        case BOOLOID:
          length = 1;
          hb_strncpy(buf, "boolean", sizeof(buf) - 1);
          break;

        case BPCHAROID:
          if (typemod >= 0)
          {
            length = static_cast<int>(typemod - VARHDRSZ);
          }
          hb_strncpy(buf, "character", sizeof(buf) - 1);
          break;

        case FLOAT4OID:
          hb_strncpy(buf, "real", sizeof(buf) - 1);
          break;

        case FLOAT8OID:
          hb_strncpy(buf, "double precision", sizeof(buf) - 1);
          break;

        case INT2OID:
          hb_strncpy(buf, "smallint", sizeof(buf) - 1);
          break;

        case INT4OID:
          hb_strncpy(buf, "integer", sizeof(buf) - 1);
          break;

        case INT8OID:
        case OIDOID:
          hb_strncpy(buf, "bigint", sizeof(buf) - 1);
          break;

        case NUMERICOID:
          length = ((typemod - VARHDRSZ) >> 16) & 0xffff;
          decimal = (typemod - VARHDRSZ) & 0xffff;
          hb_strncpy(buf, "numeric", sizeof(buf) - 1);
          break;

        case DATEOID:
          hb_strncpy(buf, "date", sizeof(buf) - 1);
          break;

        case TIMEOID:
        case TIMETZOID:
          hb_strncpy(buf, "timezone", sizeof(buf) - 1);
          break;

        case TIMESTAMPOID:
        case TIMESTAMPTZOID:
          hb_strncpy(buf, "timestamp", sizeof(buf) - 1);
          break;

        case VARBITOID:
          if (typemod >= 0)
          {
            length = static_cast<int>(typemod);
          }
          hb_strncpy(buf, "bit varying", sizeof(buf) - 1);
          break;

        case VARCHAROID:
          if (typemod >= 0)
          {
            length = static_cast<int>(typemod - VARHDRSZ);
          }
          hb_strncpy(buf, "character varying", sizeof(buf) - 1);
          break;

        case TEXTOID:
          hb_strncpy(buf, "text", sizeof(buf) - 1);
          break;

        case CASHOID:
          hb_strncpy(buf, "money", sizeof(buf) - 1);
          break;

        case NAMEOID:
          hb_strncpy(buf, "name", sizeof(buf) - 1);
          break;

        default:
          hb_strncpy(buf, "not supported", sizeof(buf) - 1);
          break;
        }

        auto pField = hb_arrayGetItemPtr(pResult, i + 1);
        hb_arrayNew(pField, HBPG_META_LEN_);
        hb_arraySetC(pField, HBPG_META_FIELDNAME, PQfname(res, i));
        hb_arraySetC(pField, HBPG_META_FIELDTYPE, buf);
        hb_arraySetNI(pField, HBPG_META_FIELDLEN, length);
        hb_arraySetNI(pField, HBPG_META_FIELDDEC, decimal);
        hb_arraySetNL(pField, HBPG_META_TABLE, PQftable(res, i));
        hb_arraySetNI(pField, HBPG_META_TABLECOL, PQftablecol(res, i));
      }

      hb_itemReturnRelease(pResult);
    }
    else
    {
      hb_reta(0);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESULT2ARRAY() -->
*/
HB_FUNC(PQRESULT2ARRAY) /* not a direct wrapper */
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    if (PQresultStatus(res) == PGRES_TUPLES_OK)
    {
      int nRows = PQntuples(res);
      int nCols = PQnfields(res);

      auto pResult = hb_itemArrayNew(nRows);

      for (auto nRow = 0; nRow < nRows; nRow++)
      {
        auto pRow = hb_arrayGetItemPtr(pResult, nRow + 1);
        hb_arrayNew(pRow, nCols);
        for (auto nCol = 0; nCol < nCols; nCol++)
        {
          hb_arraySetC(pRow, nCol + 1, PQgetvalue(res, nRow, nCol));
        }
      }

      hb_itemReturnRelease(pResult);
    }
    else
    {
      hb_reta(0);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESULTERRORMESSAGE() -->
*/
HB_FUNC(PQRESULTERRORMESSAGE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retc(PQresultErrorMessage(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQRESULTSTATUS() -->
*/
HB_FUNC(PQRESULTSTATUS)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retni(PQresultStatus(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCMDSTATUS() -->
*/
HB_FUNC(PQCMDSTATUS)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retc(PQcmdStatus(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCMDTUPLES() -->
*/
HB_FUNC(PQCMDTUPLES)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retc(PQcmdTuples(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQESCAPESTRING() -->
*/
HB_FUNC(PQESCAPESTRING)
{
  auto source = hb_parcx(1);
  HB_SIZE size = strlen(source);
  auto dest = static_cast<char *>(hb_xgrab(size * 2 + 1));
  PQescapeString(dest, source, static_cast<size_t>(size));
  hb_retc_buffer(dest);
}

/*
PQESCAPEBYTEA() -->
*/
HB_FUNC(PQESCAPEBYTEA) /* deprecated */
{
  if (HB_ISCHAR(1))
  {
    auto from_length = static_cast<size_t>(hb_parclen(1));
    size_t to_length = from_length * 5 + 1;
    unsigned char *to = PQescapeBytea(reinterpret_cast<const unsigned char *>(hb_parc(1)), from_length, &to_length);
    hb_retclen(reinterpret_cast<char *>(to), static_cast<HB_SIZE>(to_length));
    PQfreemem(to);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQUNESCAPEBYTEA() -->
*/
HB_FUNC(PQUNESCAPEBYTEA)
{
  if (HB_ISCHAR(1))
  {
    size_t to_length;
    unsigned char *from = PQunescapeBytea(reinterpret_cast<const unsigned char *>(hb_parc(1)), &to_length);
    hb_retclen(reinterpret_cast<char *>(from), static_cast<HB_SIZE>(to_length));
    PQfreemem(from);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQOIDVALUE() -->
*/
HB_FUNC(PQOIDVALUE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retnl(static_cast<Oid>(PQoidValue(res)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQOIDSTATUS() -->
*/
HB_FUNC(PQOIDSTATUS)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retc(PQoidStatus(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQBINARYTUPLES() -->
*/
HB_FUNC(PQBINARYTUPLES)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retl(PQbinaryTuples(res) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFTABLE() -->
*/
HB_FUNC(PQFTABLE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retnl(static_cast<Oid>(PQftable(res, hb_parni(2) - 1)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFTYPE() -->
*/
HB_FUNC(PQFTYPE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retnl(static_cast<Oid>(PQftype(res, hb_parni(2) - 1)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFNAME() -->
*/
HB_FUNC(PQFNAME)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retc(PQfname(res, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFMOD() -->
*/
HB_FUNC(PQFMOD)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retni(PQfmod(res, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFSIZE() -->
*/
HB_FUNC(PQFSIZE)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retni(PQfsize(res, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQGETISNULL() -->
*/
HB_FUNC(PQGETISNULL)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retl(PQgetisnull(res, hb_parni(2) - 1, hb_parni(3) - 1) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFNUMBER() -->
*/
HB_FUNC(PQFNUMBER)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retni(PQfnumber(res, hb_parcx(2)) + 1);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQNTUPLES() -->
*/
HB_FUNC(PQNTUPLES)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retnl(PQntuples(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQNFIELDS() -->
*/
HB_FUNC(PQNFIELDS)
{
  auto res = hb_PGresult_par(1);

  if (res != nullptr)
  {
    hb_retnl(PQnfields(res));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Asynchronous functions */

/*
PQSENDQUERY() -->
*/
HB_FUNC(PQSENDQUERY)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQsendQuery(conn, hb_parcx(2)) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQGETRESULT() -->
*/
HB_FUNC(PQGETRESULT)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_PGresult_ret(PQgetResult(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQCONSUMEINPUT() -->
*/
HB_FUNC(PQCONSUMEINPUT)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQconsumeInput(conn) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQISBUSY() -->
*/
HB_FUNC(PQISBUSY)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQisBusy(conn) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQREQUESTCANCEL() -->
*/
HB_FUNC(PQREQUESTCANCEL) /* deprecated */
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQrequestCancel(conn) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQFLUSH() -->
*/
HB_FUNC(PQFLUSH)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQflush(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Set blocking/nonblocking connection to the backend */

/*
PQSETNONBLOCKING() -->
*/
HB_FUNC(PQSETNONBLOCKING)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQsetnonblocking(conn, hb_parl(2)) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQISNONBLOCKING() -->
*/
HB_FUNC(PQISNONBLOCKING)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(PQisnonblocking(conn) ? true : false);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Trace Connection handling functions */

/*
PQTRACECREATE() -->
*/
HB_FUNC(PQTRACECREATE) /* not a direct wrapper */
{
#ifdef NODLL
  hb_FILE_ret(hb_fopen(hb_parcx(1), "w+b"));
#else
  hb_retptr(nullptr);
#endif
}

/*
PQTRACE() -->
*/
HB_FUNC(PQTRACE)
{
#ifdef NODLL
  auto conn = hb_PGconn_par(1);
  auto trfile = hb_FILE_par(2);

  if (conn && trfile)
  {
    PQtrace(conn, trfile);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}

/*
PQUNTRACE() -->
*/
HB_FUNC(PQUNTRACE)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    PQuntrace(conn);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* PQERRORS_TERSE   0
   PQERRORS_DEFAULT 1
   PQERRORS_VERBOSE 2
 */

/*
PQSETERRORVERBOSITY() -->
*/
HB_FUNC(PQSETERRORVERBOSITY)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(static_cast<PGVerbosity>(PQsetErrorVerbosity(conn, static_cast<PGVerbosity>(hb_parni(2)))));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* Large Object functions */

/*
LO_IMPORT() -->
*/
HB_FUNC(LO_IMPORT)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(lo_import(conn, hb_parcx(2)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
LO_EXPORT() -->
*/
HB_FUNC(LO_EXPORT)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(lo_export(conn, static_cast<Oid>(hb_parnl(2)), hb_parcx(3)) == 1);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
LO_UNLINK() -->
*/
HB_FUNC(LO_UNLINK)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retl(lo_unlink(conn, static_cast<Oid>(hb_parnl(2))) == 1);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQGETCANCEL() -->
*/
HB_FUNC(PQGETCANCEL)
{
#if PG_VERSION_NUM >= 80000
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_PGcancel_ret(PQgetCancel(conn));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retptr(nullptr);
#endif
}

/*
PQCANCEL() -->
*/
HB_FUNC(PQCANCEL)
{
#if PG_VERSION_NUM >= 80000
  auto cancel = hb_PGcancel_par(1);

  if (cancel != nullptr)
  {
    char errbuf[256];
    errbuf[0] = '\0';
    hb_retl(PQcancel(cancel, errbuf, sizeof(errbuf) - 1) ? true : false);
    hb_storc(errbuf, 2);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retl(false);
  hb_storc(nullptr, 2);
#endif
}

/*
PQESCAPEBYTEACONN() -->
*/
HB_FUNC(PQESCAPEBYTEACONN)
{
#if PG_VERSION_NUM >= 80000
  auto conn = hb_PGconn_par(1);

  if (conn && HB_ISCHAR(2))
  {
    size_t from_length = hb_parclen(2);
    size_t to_length = from_length * 5 + 1;
    unsigned char *to =
        PQescapeByteaConn(conn, reinterpret_cast<unsigned const char *>(hb_parc(2)), from_length, &to_length);
    hb_retclen(reinterpret_cast<char *>(to), static_cast<HB_SIZE>(to_length));
    PQfreemem(to);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif
}

/*
PQPREPARE() -->
*/
HB_FUNC(PQPREPARE)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_PGresult_ret(PQprepare(conn, hb_parcx(2), hb_parcx(3), hb_parni(4), nullptr));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQEXECPREPARED() -->
*/
HB_FUNC(PQEXECPREPARED)
{
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    auto aParam = hb_param(3, Harbour::Item::ARRAY);
    HB_SIZE n = hb_arrayLen(aParam);

    auto paramvalues = static_cast<const char **>(hb_xgrab(sizeof(char *) * n));

    for (HB_SIZE i = 0; i < n; ++i)
    {
      paramvalues[i] = hb_arrayGetCPtr(aParam, i + 1);
    }

    hb_PGresult_ret(PQexecPrepared(conn, hb_parcx(2), static_cast<int>(n),
                                   static_cast<const char *const *>(paramvalues), nullptr, nullptr, 1));

    hb_xfree(static_cast<void *>(paramvalues));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
PQPUTCOPYDATA() -->
*/
HB_FUNC(PQPUTCOPYDATA)
{
#if PG_VERSION_NUM >= 80000
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQputCopyData(conn, hb_parcx(2), static_cast<int>(hb_parclen(2))));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif
}

/*
PQPUTCOPYEND() -->
*/
HB_FUNC(PQPUTCOPYEND)
{
#if PG_VERSION_NUM >= 80000
  auto conn = hb_PGconn_par(1);

  if (conn != nullptr)
  {
    hb_retni(PQputCopyEnd(conn, nullptr));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif
}

/*
PG_ENCODING_TO_CHAR() -->
*/
HB_FUNC(PG_ENCODING_TO_CHAR)
{
  hb_retc(pg_encoding_to_char(hb_parni(1)));
}

/* 31.19 Behavior in Threaded Programs */

/*
PQISTHREADSAFE() -->
*/
HB_FUNC(PQISTHREADSAFE)
{
#if PG_VERSION_NUM >= 80200
  hb_retl(PQisthreadsafe() ? true : false);
#else
  hb_retl(false);
#endif
}

#if 0

 TODO: Implement Full Large Objects Support
 TODO: Implement Prepared Query handling

extern int lo_open(PGconn * conn, Oid lobjId, int mode);
extern int lo_close(PGconn * conn, int fd);
extern int lo_read(PGconn * conn, int fd, char * buf, size_t len);
extern int lo_write(PGconn * conn, int fd, char * buf, size_t len);
extern int lo_lseek(PGconn * conn, int fd, int offset, int whence);
extern Oid lo_creat(PGconn * conn, int mode);
extern int lo_tell(PGconn * conn, int fd);

int PQsendQueryParams(PGconn * conn,
                      const char * command,
                      int nParams,
                      const Oid * paramTypes,
                      const char * const * paramValues,
                      const int * paramLengths,
                      const int * paramFormats,
                      int resultFormat);

int PQsendPrepare(PGconn * conn,
                  const char * stmtName,
                  const char * query,
                  int nParams,
                  const Oid * paramTypes);

int PQsendQueryPrepared(PGconn * conn,
                        const char * stmtName,
                        int nParams,
                        const char * const * paramValues,
                        const int * paramLengths,
                        const int * paramFormats,
                        int resultFormat);

#endif
