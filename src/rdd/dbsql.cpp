//
// __dbSQL()
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// Copyright 2007 Lorenzo Fiorini <lorenzo.fiorini / at / gmail.com>
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
#include "hbapifs.hpp"
#include "hbapigt.hpp"
#include "hbapiitm.hpp"
#include "hbapirdd.hpp"
#include "hbapilng.hpp"
#include "hbapierr.hpp"
#include "hbdbferr.hpp"
#include "hbvm.hpp"
#include "hbdate.hpp"

#define HB_FILE_BUF_SIZE 0x10000
struct _HB_FILEBUF
{
  PHB_FILE pFile;
  HB_BYTE *pBuf;
  HB_SIZE nSize;
  HB_SIZE nPos;
};

using HB_FILEBUF = _HB_FILEBUF;
using PHB_FILEBUF = HB_FILEBUF *;

static void hb_flushFBuffer(PHB_FILEBUF pFileBuf)
{
  if (pFileBuf->nPos > 0)
  {
    hb_fileWrite(pFileBuf->pFile, pFileBuf->pBuf, pFileBuf->nPos, -1);
    pFileBuf->nPos = 0;
  }
}

static void hb_addToFBuffer(PHB_FILEBUF pFileBuf, char ch)
{
  if (pFileBuf->nPos == pFileBuf->nSize)
  {
    hb_flushFBuffer(pFileBuf);
  }
  pFileBuf->pBuf[pFileBuf->nPos++] = static_cast<HB_BYTE>(ch);
}

static void hb_addStrnToFBuffer(PHB_FILEBUF pFileBuf, const char *str, HB_SIZE nSize)
{
  HB_SIZE nPos = 0;

  while (nPos < nSize)
  {
    if (pFileBuf->nPos == pFileBuf->nSize)
    {
      hb_flushFBuffer(pFileBuf);
    }
    pFileBuf->pBuf[pFileBuf->nPos++] = static_cast<HB_BYTE>(str[nPos++]);
  }
}

static void hb_addStrToFBuffer(PHB_FILEBUF pFileBuf, const char *szStr)
{
  while (*szStr)
  {
    if (pFileBuf->nPos == pFileBuf->nSize)
    {
      hb_flushFBuffer(pFileBuf);
    }
    pFileBuf->pBuf[pFileBuf->nPos++] = static_cast<HB_BYTE>(*szStr++);
  }
}

static void hb_destroyFBuffer(PHB_FILEBUF pFileBuf)
{
  hb_flushFBuffer(pFileBuf);
  if (pFileBuf->pBuf)
  {
    hb_xfree(pFileBuf->pBuf);
  }
  hb_xfree(pFileBuf);
}

static PHB_FILEBUF hb_createFBuffer(PHB_FILE pFile, HB_SIZE nSize)
{
  auto pFileBuf = static_cast<PHB_FILEBUF>(hb_xgrab(sizeof(HB_FILEBUF)));

  pFileBuf->pFile = pFile;
  pFileBuf->pBuf = static_cast<HB_BYTE *>(hb_xgrab(nSize));
  pFileBuf->nSize = nSize;
  pFileBuf->nPos = 0;
  return pFileBuf;
}

// Export field value into the buffer in SQL format
static bool hb_exportBufSqlVar(PHB_FILEBUF pFileBuf, PHB_ITEM pValue, const char *szDelim, const char *szEsc)
{
  switch (hb_itemType(pValue))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
  {
    auto nLen = pValue->getCLen();
    HB_SIZE nCnt = 0;
    auto szVal = pValue->getCPtr();

    hb_addStrToFBuffer(pFileBuf, szDelim);
    while (nLen && HB_ISSPACE(szVal[nLen - 1]))
    {
      nLen--;
    }

    while (*szVal && nCnt++ < nLen)
    {
      if (*szVal == *szDelim || *szVal == *szEsc)
      {
        hb_addToFBuffer(pFileBuf, *szEsc);
      }
      if (static_cast<HB_UCHAR>(*szVal) >= 32)
      {
        hb_addToFBuffer(pFileBuf, *szVal);
      }
      else
      {
#if 0
               printf("%d %c", *szVal, *szVal);
#endif
      }
      szVal++;
    }
    hb_addStrToFBuffer(pFileBuf, szDelim);
    break;
  }

  case Harbour::Item::DATE:
  {
    char szDate[9];

    hb_addStrToFBuffer(pFileBuf, szDelim);
    pValue->getDS(szDate);
    if (szDate[0] == ' ')
    {
      hb_addStrToFBuffer(pFileBuf, "0100-01-01");
    }
    else
    {
      hb_addStrnToFBuffer(pFileBuf, &szDate[0], 4);
      hb_addToFBuffer(pFileBuf, '-');
      hb_addStrnToFBuffer(pFileBuf, &szDate[4], 2);
      hb_addToFBuffer(pFileBuf, '-');
      hb_addStrnToFBuffer(pFileBuf, &szDate[6], 2);
    }
    hb_addStrToFBuffer(pFileBuf, szDelim);
    break;
  }

  case Harbour::Item::TIMESTAMP:
  {
    long lDate, lTime;
    char szDateTime[24];

    pValue->getTDT(&lDate, &lTime);
    hb_timeStampStr(szDateTime, lDate, lTime);
    hb_addStrToFBuffer(pFileBuf, szDelim);
    hb_addStrToFBuffer(pFileBuf, szDateTime);
    hb_addStrToFBuffer(pFileBuf, szDelim);
    break;
  }

  case Harbour::Item::LOGICAL:
    hb_addStrToFBuffer(pFileBuf, szDelim);
    hb_addToFBuffer(pFileBuf, pValue->getL() ? 'Y' : 'N');
    hb_addStrToFBuffer(pFileBuf, szDelim);
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
      int iPos = 0;
      while (iSize && HB_ISSPACE(szResult[iPos]))
      {
        iPos++;
        iSize--;
      }
      hb_addStrnToFBuffer(pFileBuf, &szResult[iPos], iSize);
    }
    else
    {
      hb_addToFBuffer(pFileBuf, '0');
    }
    break;
  }
  // an "M" field or the other, might be a "V" in SixDriver
  default:
    // We do not want MEMO contents
    return false;
  }
  return true;
}

// Export DBF content to a SQL script file
static HB_ULONG hb_db2Sql(AREAP pArea, PHB_ITEM pFields, HB_MAXINT llNext, PHB_ITEM pWhile, PHB_ITEM pFor,
                          const char *szDelim, const char *szSep, const char *szEsc, const char *szTable,
                          PHB_FILE pFile, HB_BOOL fInsert, HB_BOOL fRecno)
{
  PHB_FILEBUF pFileBuf;
  HB_ULONG ulRecords = 0;
  HB_USHORT uiFields = 0, ui;
  PHB_ITEM pTmp;
  HB_BOOL fWriteSep = false;
  const char *szNewLine = hb_conNewLine();
  char *szInsert = nullptr;
  HB_BOOL fEof = true;
  HB_BOOL fNoFieldPassed = (pFields == nullptr || hb_arrayLen(pFields) == 0);

  if (SELF_FIELDCOUNT(pArea, &uiFields) != Harbour::SUCCESS)
  {
    return 0;
  }

  if (fInsert && szTable)
  {
    szInsert = hb_xstrcpy(nullptr, "INSERT INTO ", szTable, " VALUES ( ", nullptr);
  }

  pFileBuf = hb_createFBuffer(pFile, HB_FILE_BUF_SIZE);
  pTmp = hb_itemNew(nullptr);

  while (llNext-- > 0)
  {
    if (pWhile)
    {
      if (SELF_EVALBLOCK(pArea, pWhile) != Harbour::SUCCESS || !hb_itemGetL(pArea->valResult))
      {
        break;
      }
    }

    if (SELF_EOF(pArea, &fEof) != Harbour::SUCCESS || fEof)
    {
      break;
    }

    if (pFor)
    {
      if (SELF_EVALBLOCK(pArea, pFor) != Harbour::SUCCESS)
      {
        break;
      }
    }
    if (!pFor || hb_itemGetL(pArea->valResult))
    {
      ++ulRecords;

      if (szInsert != nullptr)
      {
        hb_addStrToFBuffer(pFileBuf, szInsert);
      }

      if (fRecno)
      {
        HB_ULONG ulRec = ulRecords;
        char szRecno[13], *szVal;

        szVal = szRecno + sizeof(szRecno);
        *--szVal = 0;
        do
        {
          *--szVal = static_cast<char>(ulRec % 10) + '0';
          ulRec /= 10;
        } while (ulRec);
        hb_addStrToFBuffer(pFileBuf, szVal);
        hb_addStrToFBuffer(pFileBuf, szSep);
      }

      if (fNoFieldPassed)
      {
        for (ui = 1; ui <= uiFields; ui++)
        {
          if (SELF_GETVALUE(pArea, ui, pTmp) != Harbour::SUCCESS)
          {
            break;
          }
          if (fWriteSep)
          {
            hb_addStrToFBuffer(pFileBuf, szSep);
          }
          fWriteSep = hb_exportBufSqlVar(pFileBuf, pTmp, szDelim, szEsc);
        }
        if (ui <= uiFields)
        {
          break;
        }
      }
      else
      {
        // TODO: exporting only some fields
      }

      if (szInsert != nullptr)
      {
        hb_addStrToFBuffer(pFileBuf, " );");
      }
      hb_addStrToFBuffer(pFileBuf, szNewLine);
      fWriteSep = false;
    }

    if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
    {
      break;
    }

    if ((llNext % 10000) == 0)
    {
      hb_inkeyPoll();
    }
  }

  if (szInsert != nullptr)
  {
    hb_xfree(szInsert);
  }
  hb_destroyFBuffer(pFileBuf);
  hb_itemRelease(pTmp);

#if 0
   // Writing EOF
   hb_fileWrite(pFile, "\x1A", 1, -1);
#endif

  return ulRecords;
}

// __dbSQL(.T., <cFileName>, <cTable>, [<bFor>], [<bWhile>], ;
//         [<nNext>], [<nRec>], [<lRest>], [<lAppend>], [<lInsert>], ;
//         [<lRecNo>], [<cSep>], [<cDelim>], [<cEsc>]) -> <nRecords>
HB_FUNC(__DBSQL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    HB_BOOL fExport = hb_parl(1);
    auto szFileName = hb_parc(2);
    auto szTable = hb_parc(3);
    auto pFields = hb_param(4, Harbour::Item::ARRAY);
    auto pFor = hb_param(5, Harbour::Item::BLOCK);
    auto pWhile = hb_param(6, Harbour::Item::BLOCK);
    auto pNext = hb_param(7, Harbour::Item::NUMERIC);
    PHB_ITEM pRecord = HB_ISNIL(8) ? nullptr : hb_param(8, Harbour::Item::ANY);
    HB_BOOL fRest = pWhile != nullptr || hb_parl(9);
    HB_BOOL fAppend = hb_parl(10);
    HB_BOOL fInsert = hb_parl(11);
    HB_BOOL fRecno = hb_parl(12);
    auto szSep = hb_parcx(13);
    auto szDelim = hb_parcx(14);
    auto szEsc = hb_parcx(15);
    HB_MAXINT llNext = HB_VMLONG_MAX;
    HB_ERRCODE errCode;
    PHB_FILE pFile;

    if (!szFileName)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
    else if (fExport)
    { // COPY TO SQL
      PHB_ITEM pError = nullptr;
      HB_BOOL fRetry;

      // Try to create Dat file
      do
      {
        pFile = hb_fileExtOpen(
            szFileName, nullptr,
            (fAppend ? 0 : FXO_TRUNCATE) | FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, pError);
        if (pFile == nullptr)
        {
          if (!pError)
          {
            pError = hb_errNew();
            hb_errPutSeverity(pError, ES_ERROR);
            if (fAppend)
            {
              hb_errPutGenCode(pError, EG_OPEN);
              hb_errPutSubCode(pError, EDBF_OPEN_DBF);
              hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_OPEN));
            }
            else
            {
              hb_errPutGenCode(pError, EG_CREATE);
              hb_errPutSubCode(pError, EDBF_CREATE_DBF);
              hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_CREATE));
            }
            hb_errPutFileName(pError, szFileName);
            hb_errPutFlags(pError, EF_CANRETRY | EF_CANDEFAULT);
            hb_errPutSubSystem(pError, "DBF2SQL");
            hb_errPutOsCode(pError, hb_fsError());
          }
          fRetry = hb_errLaunch(pError) == E_RETRY;
        }
        else
        {
          fRetry = false;
        }
      } while (fRetry);

      if (pError)
      {
        hb_itemRelease(pError);
      }

      if (pFile != nullptr)
      {
        if (fAppend)
        {
          hb_fileSeek(pFile, 0, FS_END);
        }

        errCode = Harbour::SUCCESS;
        if (pRecord)
        {
          errCode = SELF_GOTOID(pArea, pRecord);
        }
        else if (pNext)
        {
          llNext = pNext->getNInt();
        }
        else if (!fRest)
        {
          errCode = SELF_GOTOP(pArea);
        }

        if (errCode == Harbour::SUCCESS)
        {
          hb_retnint(
              hb_db2Sql(pArea, pFields, llNext, pWhile, pFor, szDelim, szSep, szEsc, szTable, pFile, fInsert, fRecno));
        }
        hb_fileClose(pFile);
      }
    }
    else
    {
      // TODO: import code
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}
