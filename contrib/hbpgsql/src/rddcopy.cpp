/*
 * PostgreSQL RDBMS low-level (client API) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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

#include "hbpgsql.hpp"

#include <hbapierr.hpp>
#include <hbapiitm.hpp>
#include <hbapirdd.hpp>
#include <hbvm.hpp>
#include <hbdate.hpp>

struct pgCopyContext
{
  char *buffer;
  int position;
  int length;
  bool str_trim;
  PGconn *connection;
};

#define HB_VM_UNLOCK()                                                                                                 \
  do                                                                                                                   \
  {                                                                                                                    \
  hb_vmUnlock()
#define HB_VM_LOCK()                                                                                                   \
  hb_vmLock();                                                                                                         \
  }                                                                                                                    \
  while (0)

#if PG_VERSION_NUM >= 80000
static bool addToContext(pgCopyContext *context, const char c)
{
  if (context->position == context->length)
  {
    bool fOK;
    HB_VM_UNLOCK();
    fOK = PQputCopyData(context->connection, context->buffer, context->position) != -1;
    HB_VM_LOCK();
    if (!fOK)
    {
      return false;
    }

    context->position = 0;
  }
  context->buffer[context->position++] = static_cast<HB_BYTE>(c);

  return true;
}

static bool addStrToContext(pgCopyContext *context, const char *str)
{
  while (*str)
  {
    if (context->position == context->length)
    {
      bool fOK;
      HB_VM_UNLOCK();
      fOK = PQputCopyData(context->connection, context->buffer, context->position) != -1;
      HB_VM_LOCK();
      if (!fOK)
      {
        return false;
      }

      context->position = 0;
    }
    context->buffer[context->position++] = static_cast<HB_BYTE>(*str++);
  }

  return true;
}
static bool addStrnToContext(pgCopyContext *context, const char *str, HB_SIZE size)
{
  HB_SIZE nSize = 0;

  while (nSize < size)
  {
    if (context->position == context->length)
    {
      bool fOK;
      HB_VM_UNLOCK();
      fOK = PQputCopyData(context->connection, context->buffer, context->position) != -1;
      HB_VM_LOCK();
      if (!fOK)
      {
        return false;
      }

      context->position = 0;
    }
    context->buffer[context->position++] = static_cast<HB_BYTE>(str[nSize++]);
  }

  return true;
}

/* Export field value into the buffer in PG accepted CSV format */
static bool exportBufSqlVar(pgCopyContext *context, PHB_ITEM pValue, const char *szQuote, const char *szEsc)
{
  switch (hb_itemType(pValue))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
  {
    auto nLen = hb_itemGetCLen(pValue);
    HB_SIZE nCnt = 0;
    auto szVal = hb_itemGetCPtr(pValue);

    if (!addStrToContext(context, szQuote))
    {
      return false;
    }

    if (context->str_trim)
    {
      while (nLen && HB_ISSPACE(szVal[nLen - 1]))
      {
        nLen--;
      }
    }

    while (*szVal && nCnt++ < nLen)
    {
      if (static_cast<HB_UCHAR>(*szVal) >= 32)
      {
        /* if( *szVal == *szDelim || *szVal == *szEsc || *szVal == *szQuote )
           we don't need to escape delim in CSV mode,
           only the quote and the escape itself */

        if (*szVal == *szQuote || *szVal == *szEsc)
        {
          if (!addToContext(context, *szEsc))
          {
            return false;
          }
        }
        if (!addToContext(context, *szVal))
        {
          return false;
        }
      }
      szVal++;
    }
    if (!addStrToContext(context, szQuote))
    {
      return false;
    }
    break;
  }

  case Harbour::Item::DATE:
  {
    char szDate[9];

    if (!addStrToContext(context, szQuote))
    {
      return false;
    }
    hb_itemGetDS(pValue, szDate);
    if (szDate[0] == ' ')
    {
      if (!addStrToContext(context, "0100-01-01"))
      {
        return false;
      }
    }
    else
    {
      if (!addStrnToContext(context, &szDate[0], 4) || !addToContext(context, '-') ||
          !addStrnToContext(context, &szDate[4], 2) || !addToContext(context, '-') ||
          !addStrnToContext(context, &szDate[6], 2))
      {
        return false;
      }
    }
    if (!addStrToContext(context, szQuote))
    {
      return false;
    }
    break;
  }

  case Harbour::Item::TIMESTAMP:
  {
    long lDate, lTime;
    char szDateTime[24];

    hb_itemGetTDT(pValue, &lDate, &lTime);
    hb_timeStampStr(szDateTime, lDate, lTime);
    if (!addStrToContext(context, szQuote) || !addStrToContext(context, szDateTime) ||
        !addStrToContext(context, szQuote))
    {
      return false;
    }
    break;
  }

  case Harbour::Item::LOGICAL:
#if 0
         if( !addStrToContext(context, szQuote) ||
             !addToContext(context, hb_itemGetL(pValue) ? 'Y' : 'N') ||
             !addStrToContext(context, szQuote) )
#else
    if (!addToContext(context, hb_itemGetL(pValue) ? 'Y' : 'N'))
#endif
      return false;
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
  {
    char szResult[HB_MAX_DOUBLE_LENGTH];
    int iSize, iWidth, iDec;

    hb_itemGetNLen(pValue, &iWidth, &iDec);
    iSize = (iDec > 0 ? iWidth + 1 + iDec : iWidth);
    if (hb_itemStrBuf(szResult, pValue, iSize, iDec))
    {
      auto iPos = 0;
      while (iSize && HB_ISSPACE(szResult[iPos]))
      {
        iPos++;
        iSize--;
      }
      if (!addStrnToContext(context, &szResult[iPos], iSize))
      {
        return false;
      }
    }
    else if (!addToContext(context, '0'))
    {
      return false;
    }
    break;
  }
  /* an "M" field or the other, might be a "V" in SixDriver */
  default:
    return false;
  }

  return true;
}
#endif

/*
HB_PQCOPYFROMWA() -->
*/
HB_FUNC(HB_PQCOPYFROMWA)
{
#if PG_VERSION_NUM >= 80000
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto pConn = hb_PGconn_par(1);

  if (pConn == nullptr)
  {
    hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
  else if (pArea == nullptr)
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
  else
  {
    static const char *sc_szQuote = "\"";
    static const char *sc_szEsc = "\"";
    static const char *sc_szDelim = ",";

    auto szTable = hb_parcx(2);
    auto pWhile = hb_param(3, Harbour::Item::EVALITEM);
    auto pFor = hb_param(4, Harbour::Item::EVALITEM);
    auto pFields = hb_param(5, Harbour::Item::ARRAY);
    HB_ULONG nCount = hb_parnldef(6, 0);
    bool str_rtrim = hb_parldef(7, true);
    HB_ULONG nBufLen = hb_parnldef(8, 1);
    HB_USHORT uiFields;
    HB_ULONG uiRecCount = 0;
    bool bNoFieldPassed = (pFields == nullptr || hb_arrayLen(pFields) == 0);
    HB_BOOL bEof = false;
    HB_USHORT uiFieldCopy = 0;
    HB_USHORT uiIter;
    char *szInit;
    char *szFields = nullptr;
    char *szTmp;
    PGresult *pgResult;
    bool bFail = false;

    auto pItem = hb_itemNew(nullptr);

    auto context = static_cast<pgCopyContext *>(hb_xgrabz(sizeof(pgCopyContext)));

    context->buffer = static_cast<char *>(hb_xgrab(sizeof(char) * nBufLen * 1400));
    context->position = 0;
    context->length = sizeof(char) * nBufLen * 1400;
    context->str_trim = str_rtrim;
    context->connection = pConn;

    SELF_FIELDCOUNT(pArea, &uiFields);

    if (!bNoFieldPassed)
    {
      szFields = static_cast<char *>(hb_xgrab(sizeof(char) * 2));
      szFields[0] = '(';
      szFields[1] = '\0';
      uiFieldCopy = static_cast<HB_USHORT>(hb_arrayLen(pFields));

      for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
      {
        auto szFieldName = hb_arrayGetCPtr(pFields, uiIter);
        if (szFieldName)
        {
          int iPos = hb_rddFieldIndex(pArea, szFieldName);

          szTmp = hb_xstrcpy(nullptr, szFields, szFieldName, nullptr);
          hb_xfree(szFields);
          szFields = szTmp;
          if (uiIter != uiFieldCopy)
          {
            szTmp = hb_xstrcpy(nullptr, szFields, sc_szDelim, nullptr);
            hb_xfree(szFields);
            szFields = szTmp;
          }

          if (iPos)
          {
            hb_arraySetNI(pFields, uiIter, iPos);
            continue;
          }
        }

        if (hb_arrayDel(pFields, uiIter))
        {
          hb_arraySize(pFields, hb_arrayLen(pFields) - 1);
          uiIter--;
          uiFieldCopy--;
        }
      }
      szTmp = hb_xstrcpy(nullptr, szFields, ")", nullptr);
      hb_xfree(szFields);
      szFields = szTmp;
    }

    if (szFields)
    {
      szInit = hb_xstrcpy(nullptr, "COPY ", szTable, " ", szFields, " FROM STDIN WITH DELIMITER '", sc_szDelim,
                          "' CSV  QUOTE AS '", sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", nullptr);
      hb_xfree(szFields);
    }
    else
    {
      szInit = hb_xstrcpy(nullptr, "COPY ", szTable, " FROM STDIN WITH DELIMITER '", sc_szDelim, "' CSV  QUOTE AS '",
                          sc_szQuote, "' ESCAPE AS '", sc_szEsc, "'", nullptr);
    }

    HB_VM_UNLOCK();
    pgResult = PQexec(context->connection, szInit);
    if (PQresultStatus(pgResult) != PGRES_COPY_IN)
    {
      bFail = true;
    }
    PQclear(pgResult);
    hb_xfree(szInit);
    HB_VM_LOCK();

    while (!bFail && (nCount == 0 || uiRecCount < nCount) && (!pWhile || hb_itemGetL(hb_vmEvalBlock(pWhile))))
    {

      if (SELF_EOF(pArea, &bEof) != Harbour::SUCCESS)
      {
        break;
      }

      if (bEof)
      {
        break;
      }

      if (!pFor || hb_itemGetL(hb_vmEvalBlock(pFor)))
      {
        if (bNoFieldPassed)
        {
          for (uiIter = 1; uiIter <= uiFields; uiIter++)
          {
            if (SELF_GETVALUE(pArea, uiIter, pItem) != Harbour::SUCCESS ||
                !exportBufSqlVar(context, pItem, sc_szQuote, sc_szEsc) ||
                !addStrToContext(context, uiIter == uiFields ? "\n" : sc_szDelim))
            {
              bFail = true;
              break;
            }
          }
        }
        else
        {
          for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
          {
            if (SELF_GETVALUE(pArea, static_cast<HB_USHORT>(hb_arrayGetNI(pFields, uiIter)), pItem) !=
                    Harbour::SUCCESS ||
                !exportBufSqlVar(context, pItem, sc_szQuote, sc_szEsc) ||
                !addStrToContext(context, uiIter == uiFieldCopy ? "\n" : sc_szDelim))
            {
              bFail = true;
              break;
            }
          }
        }

        if (bFail)
        {
          break;
        }

        uiRecCount++;
      }

      if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
      {
        break;
      }
    }

    if (!bFail && !addStrnToContext(context, "\\.\n", 3)) /* end CSV transfer */
    {
      bFail = true;
    }

    HB_VM_UNLOCK();
    if (bFail)
    {
      PQputCopyEnd(context->connection, "export buffer problems");
    }
    else if (PQputCopyData(context->connection, context->buffer, context->position) == -1 ||
             PQputCopyEnd(context->connection, nullptr) == -1)
    {
      bFail = true;
    }
    else
    {
      while ((pgResult = PQgetResult(context->connection)))
      {
        if (PQresultStatus(pgResult) != PGRES_COMMAND_OK)
        {
          bFail = true;
        }
        PQclear(pgResult);
      }
    }
    HB_VM_LOCK();

    hb_itemRelease(pItem);
    hb_xfree(context->buffer);
    hb_xfree(context);

    hb_retl(!bFail);
  }
#else
  hb_retl(false);
#endif
}
