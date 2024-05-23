/*
 * SDF RDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.hpp"
#include "hbinit.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"
#include "hbdate.hpp"
#include "hbapirdd.hpp"
#include "hbapiitm.hpp"
#include "hbapilng.hpp"
#include "hbapierr.hpp"
#include "hbdbferr.hpp"
#include "hbrddsdf.hpp"
#include "rddsys.ch"

#define SUPERTABLE (&sdfSuper)

static RDDFUNCS sdfSuper;
static const HB_USHORT s_uiNumLength[9] = {0, 4, 6, 8, 11, 13, 16, 18, 20};

static void hb_sdfInitArea(SDFAREAP pArea, char *szFileName)
{
  const char *szEol;

  /* Allocate only after successfully open file */
  pArea->szFileName = hb_strdup(szFileName);

  /* set line separator: EOL */
  szEol = hb_setGetEOL();
  if (!szEol || !szEol[0])
  {
    szEol = hb_conNewLine();
  }
  pArea->szEol = hb_strdup(szEol);
  pArea->uiEolLen = static_cast<HB_USHORT>(strlen(szEol));
  pArea->fAnyEol = (szEol[0] == '\n' || szEol[0] == '\r') &&
                   (pArea->uiEolLen == 1 ||
                    (pArea->uiEolLen == 2 && szEol[0] != szEol[1] && (szEol[1] == '\n' || szEol[1] == '\r')));

  /* allocate record buffer, one additional byte is for deleted flag */
  pArea->pRecord = static_cast<HB_BYTE *>(hb_xgrab(pArea->uiRecordLen + pArea->uiEolLen + 1));
  /* pseudo deleted flag */
  *pArea->pRecord++ = ' ';
  memcpy(pArea->pRecord + pArea->uiRecordLen, pArea->szEol, pArea->uiEolLen);

  if (pArea->fReadonly)
  {
    /* allocate IO buffer */
    pArea->nBufferSize += pArea->fAnyEol ? 2 : pArea->uiEolLen;
    if (pArea->nBufferSize < 8192)
    {
      pArea->nBufferSize = 8192;
    }
    pArea->pBuffer = static_cast<HB_BYTE *>(hb_xgrab(pArea->nBufferSize));
  }
  pArea->ulRecCount = 0;
  pArea->nBufferIndex = pArea->nBufferRead = pArea->nBufferSize;
}

static void hb_sdfClearRecordBuffer(SDFAREAP pArea)
{
  memset(pArea->pRecord, ' ', pArea->uiRecordLen);
}

static HB_ERRCODE hb_sdfReadRecord(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfReadRecord(%p)", static_cast<void*>(pArea)));
#endif

  HB_SIZE nRead;

  pArea->area.fEof = true;

  nRead = 0;
  for (;;)
  {
    char ch;

    if (pArea->nBufferRead - pArea->nBufferIndex < static_cast<HB_SIZE>(pArea->uiEolLen) + 1 &&
        pArea->nBufferRead == pArea->nBufferSize)
    {
      HB_SIZE nLeft = pArea->nBufferRead - pArea->nBufferIndex;

      if (nLeft)
      {
        memmove(pArea->pBuffer, pArea->pBuffer + pArea->nBufferIndex, nLeft);
      }
      pArea->nBufferIndex = 0;
      pArea->nBufferRead = hb_fileRead(pArea->pFile, pArea->pBuffer + nLeft, pArea->nBufferSize - nLeft, -1);
      if (pArea->nBufferRead == static_cast<HB_SIZE>(FS_ERROR))
      {
        pArea->nBufferRead = 0;
      }
      pArea->nBufferRead += nLeft;
    }

    if (pArea->nBufferIndex >= pArea->nBufferRead)
    {
      break;
    }

    ch = pArea->pBuffer[pArea->nBufferIndex++];

    if (pArea->fAnyEol)
    {
      if (ch == '\r' || ch == '\n')
      {
        if (pArea->nBufferIndex < pArea->nBufferRead && pArea->pBuffer[pArea->nBufferIndex] != ch &&
            (pArea->pBuffer[pArea->nBufferIndex] == '\r' || pArea->pBuffer[pArea->nBufferIndex] == '\n'))
        {
          pArea->nBufferIndex++;
        }
        pArea->area.fEof = false;
        break;
      }
    }
    else if (ch == pArea->szEol[0])
    {
      if (pArea->uiEolLen == 1 ||
          (pArea->nBufferRead - pArea->nBufferIndex >= static_cast<HB_SIZE>(pArea->uiEolLen) - 1 &&
           memcmp(pArea->pBuffer + pArea->nBufferIndex, pArea->szEol + 1, pArea->uiEolLen - 1) == 0))
      {
        pArea->nBufferIndex += pArea->uiEolLen - 1;
        pArea->area.fEof = false;
        break;
      }
    }
    if (nRead < static_cast<HB_SIZE>(pArea->uiRecordLen) && ch != '\032')
    {
      pArea->pRecord[nRead++] = ch;
    }
  }

  if (nRead < static_cast<HB_SIZE>(pArea->uiRecordLen))
  {
    memset(pArea->pRecord + nRead, ' ', pArea->uiRecordLen - nRead);
  }
  if (nRead > 0)
  {
    pArea->area.fEof = false;
  }

  pArea->fPositioned = !pArea->area.fEof;

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_sdfNextRecord(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNextRecord(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fPositioned)
  {
    pArea->ulRecNo++;
    return hb_sdfReadRecord(pArea);
  }
  return Harbour::SUCCESS;
}

/*
 * -- SDF METHODS --
 */

/*
 * Position cursor at a specific physical record.
 */
static HB_ERRCODE hb_sdfGoTo(SDFAREAP pArea, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoTo(%p, %lu)", static_cast<void*>(pArea), ulRecNo));
#endif

#ifndef HB_CLP_STRICT
  if (pArea->fReadonly && ulRecNo >= pArea->ulRecNo)
  {
    while (pArea->ulRecNo < ulRecNo && pArea->fPositioned)
    {
      if (hb_sdfNextRecord(pArea) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }
    return Harbour::SUCCESS;
  }
#endif
  /* generate RTE */
  return SUPER_GOTO(&pArea->area, ulRecNo);
}

/*
 * Position the cursor to a specific, physical identity.
 */
static HB_ERRCODE hb_sdfGoToId(SDFAREAP pArea, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoToId(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pItem)));
#endif

#ifndef HB_CLP_STRICT
  if (HB_IS_NUMERIC(pItem))
  {
    return SELF_GOTO(&pArea->area, hb_itemGetNL(pItem));
  }
#endif
  /* generate RTE */
  return SUPER_GOTOID(&pArea->area, pItem);
}

/*
 * Position cursor at the first record.
 */
static HB_ERRCODE hb_sdfGoTop(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoTop(%p)", static_cast<void*>(pArea)));
#endif

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->area.fTop = true;
  pArea->area.fBottom = false;

  if (pArea->ulRecNo != 1)
  {
    if (pArea->ulRecNo != 0 || !pArea->fReadonly)
    {
      /* generate RTE */
      return SUPER_GOTOP(&pArea->area);
    }

    pArea->ulRecNo = 1;
    if (hb_sdfReadRecord(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return SELF_SKIPFILTER(&pArea->area, 1);
}

/*
 * Reposition cursor, regardless of filter.
 */
static HB_ERRCODE hb_sdfSkipRaw(SDFAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSkipRaw(%p,%ld)", static_cast<void*>(pArea), lToSkip));
#endif

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (lToSkip != 1 || !pArea->fReadonly)
  {
    /* generate RTE */
    return SUPER_SKIPRAW(&pArea->area, lToSkip);
  }
  else
  {
    return hb_sdfNextRecord(pArea);
  }
}

/*
 * Determine deleted status for a record.
 */
static HB_ERRCODE hb_sdfDeleted(SDFAREAP pArea, HB_BOOL *pDeleted)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleted(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pDeleted)));
#endif

  HB_SYMBOL_UNUSED(pArea);

  *pDeleted = HB_FALSE;

  return Harbour::SUCCESS;
}

/*
 * Obtain number of records in WorkArea.
 */
static HB_ERRCODE hb_sdfRecCount(SDFAREAP pArea, HB_ULONG *pRecCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecCount(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pRecCount)));
#endif

  *pRecCount = pArea->ulRecCount;

  return Harbour::SUCCESS;
}

/*
 * Obtain physical row number at current WorkArea cursor position.
 */
static HB_ERRCODE hb_sdfRecNo(SDFAREAP pArea, HB_ULONG *pulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecNo(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pulRecNo)));
#endif

  *pulRecNo = pArea->ulRecNo;

  return Harbour::SUCCESS;
}

/*
 * Obtain physical row ID at current WorkArea cursor position.
 */
static HB_ERRCODE hb_sdfRecId(SDFAREAP pArea, PHB_ITEM pRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecId(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pRecNo)));
#endif

  HB_ULONG ulRecNo;

  HB_ERRCODE errCode = SELF_RECNO(&pArea->area, &ulRecNo);

#ifdef HB_CLP_STRICT
  /* this is for strict Clipper compatibility but IMHO Clipper should not
     do that and always set fixed size independent to the record number */
  if (ulRecNo < 10000000)
  {
    hb_itemPutNLLen(pRecNo, ulRecNo, 7);
  }
  else
  {
    hb_itemPutNLLen(pRecNo, ulRecNo, 10);
  }
#else
  hb_itemPutNInt(pRecNo, ulRecNo);
#endif
  return errCode;
}

/*
 * Append a record to the WorkArea.
 */
static HB_ERRCODE hb_sdfAppend(SDFAREAP pArea, HB_BOOL fUnLockAll)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAppend(%p,%d)", static_cast<void*>(pArea), static_cast<int>(fUnLockAll)));
#endif

  HB_SYMBOL_UNUSED(fUnLockAll);

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (SELF_GOHOT(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->ulRecNo = ++pArea->ulRecCount;
  pArea->area.fEof = false;
  pArea->fPositioned = true;
  hb_sdfClearRecordBuffer(pArea);

  return Harbour::SUCCESS;
}

/*
 * Delete a record.
 */
static HB_ERRCODE hb_sdfDeleteRec(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfDeleteRec(%p)", static_cast<void*>(pArea)));
#endif

  HB_SYMBOL_UNUSED(pArea);

  /* It's not Cl*pper compatible so I had to disable it [druzus] */
#if 0
   if( pArea->fRecordChanged ) {
      pArea->ulRecCount--;
      pArea->area.fEof = true;
      pArea->fPositioned = pArea->fRecordChanged = false;
      hb_sdfClearRecordBuffer(pArea);
   }
#endif

  return Harbour::SUCCESS;
}

/*
 * Undelete the current record.
 */
static HB_ERRCODE hb_sdfRecall(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRecall(%p)", static_cast<void*>(pArea)));
#endif

  HB_SYMBOL_UNUSED(pArea);

  return Harbour::SUCCESS;
}

/*
 * Obtain the current value of a field.
 */
static HB_ERRCODE hb_sdfGetValue(SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetValue(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->area.lpFields + uiIndex;
  switch (pField->uiType)
  {
  case Harbour::DB::Field::STRING:
    if ((pField->uiFlags & HB_FF_BINARY) == 0)
    {
      HB_SIZE nLen = pField->uiLen;
      char *pszVal = hb_cdpnDup(reinterpret_cast<const char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], &nLen,
                                pArea->area.cdPage, hb_vmCDP());
      hb_itemPutCLPtr(pItem, pszVal, nLen);
    }
    else
    {
      hb_itemPutCL(pItem, reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], pField->uiLen);
    }
    break;

  case Harbour::DB::Field::LOGICAL:
    switch (pArea->pRecord[pArea->pFieldOffset[uiIndex]])
    {
    case 'T':
    case 't':
    case 'Y':
    case 'y':
      hb_itemPutL(pItem, true);
      break;
    default:
      hb_itemPutL(pItem, false);
      break;
    }
    break;

  case Harbour::DB::Field::DATE:
    hb_itemPutDS(pItem, reinterpret_cast<const char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex]);
    break;

  case Harbour::DB::Field::TIMESTAMP: {
    long lJulian, lMilliSec;
    HB_BYTE *pFieldPtr = pArea->pRecord + pArea->pFieldOffset[uiIndex], bChar;

    bChar = pFieldPtr[pField->uiLen];
    pFieldPtr[pField->uiLen] = 0;
    hb_timeStampStrGetDT(reinterpret_cast<const char *>(pFieldPtr), &lJulian, &lMilliSec);
    pFieldPtr[pField->uiLen] = bChar;
    hb_itemPutTDT(pItem, lJulian, lMilliSec);
    break;
  }

  case Harbour::DB::Field::LONG: {
    HB_MAXINT lVal;
    double dVal;
    auto fDbl = false;

    fDbl = hb_strnToNum(reinterpret_cast<const char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], pField->uiLen,
                        &lVal, &dVal);

    if (pField->uiDec)
    {
      hb_itemPutNDLen(pItem, fDbl ? dVal : static_cast<double>(lVal),
                      static_cast<int>(pField->uiLen - pField->uiDec - 1), static_cast<int>(pField->uiDec));
    }
    else if (fDbl)
    {
      hb_itemPutNDLen(pItem, dVal, static_cast<int>(pField->uiLen), 0);
    }
    else
    {
      hb_itemPutNIntLen(pItem, lVal, static_cast<int>(pField->uiLen));
    }
    break;
  }

  case Harbour::DB::Field::MEMO:
    hb_itemPutC(pItem, nullptr);
    break;

  case Harbour::DB::Field::NONE:
    hb_itemClear(pItem);
    break;

  default: {
    PHB_ITEM pError = hb_errNew();
    hb_errPutGenCode(pError, EG_DATATYPE);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_DATATYPE));
    hb_errPutOperation(pError, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
    hb_errPutSubCode(pError, EDBF_DATATYPE);
    SELF_ERROR(&pArea->area, pError);
    hb_itemRelease(pError);
    return Harbour::FAILURE;
  }
  }

  return Harbour::SUCCESS;
}

/*
 * Assign a value to a field.
 */
static HB_ERRCODE hb_sdfPutValue(SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutValue(%p,%hu,%p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  HB_SIZE nSize;

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (!pArea->fRecordChanged)
  {
    return Harbour::FAILURE;
  }

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = Harbour::SUCCESS;
  pField = pArea->area.lpFields + uiIndex;
  if (pField->uiType != Harbour::DB::Field::MEMO && pField->uiType != Harbour::DB::Field::NONE)
  {
    char szBuffer[256];

    if (HB_IS_MEMO(pItem) || HB_IS_STRING(pItem))
    {
      if (pField->uiType == Harbour::DB::Field::STRING)
      {
        if ((pField->uiFlags & HB_FF_BINARY) == 0)
        {
          nSize = pField->uiLen;
          hb_cdpnDup2(hb_itemGetCPtr(pItem), hb_itemGetCLen(pItem),
                      reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], &nSize, hb_vmCDP(),
                      pArea->area.cdPage);
        }
        else
        {
          nSize = hb_itemGetCLen(pItem);
          if (nSize > static_cast<HB_SIZE>(pField->uiLen))
          {
            nSize = pField->uiLen;
          }
          memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], hb_itemGetCPtr(pItem), nSize);
        }
        if (nSize < static_cast<HB_SIZE>(pField->uiLen))
        {
          memset(pArea->pRecord + pArea->pFieldOffset[uiIndex] + nSize, ' ', pField->uiLen - nSize);
        }
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (HB_IS_DATETIME(pItem))
    {
      if (pField->uiType == Harbour::DB::Field::DATE)
      {
        hb_itemGetDS(pItem, szBuffer);
        memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], szBuffer, 8);
      }
      else if (pField->uiType == Harbour::DB::Field::TIMESTAMP && (pField->uiLen == 12 || pField->uiLen == 23))
      {
        long lDate, lTime;
        hb_itemGetTDT(pItem, &lDate, &lTime);
        if (pField->uiLen == 12)
        {
          hb_timeStr(szBuffer, lTime);
        }
        else
        {
          hb_timeStampStr(szBuffer, lDate, lTime);
        }
        memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], szBuffer, pField->uiLen);
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (HB_IS_NUMBER(pItem))
    {
      if (pField->uiType == Harbour::DB::Field::LONG)
      {
        if (hb_itemStrBuf(szBuffer, pItem, pField->uiLen, pField->uiDec))
        {
          memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], szBuffer, pField->uiLen);
        }
        else
        {
          errCode = EDBF_DATAWIDTH;
          memset(pArea->pRecord + pArea->pFieldOffset[uiIndex], '*', pField->uiLen);
        }
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (HB_IS_LOGICAL(pItem))
    {
      if (pField->uiType == Harbour::DB::Field::LOGICAL)
      {
        pArea->pRecord[pArea->pFieldOffset[uiIndex]] = hb_itemGetL(pItem) ? 'T' : 'F';
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else
    {
      errCode = EDBF_DATATYPE;
    }
  }

  if (errCode != Harbour::SUCCESS)
  {
    PHB_ITEM pError = hb_errNew();
    HB_ERRCODE errGenCode = errCode == EDBF_DATAWIDTH ? EG_DATAWIDTH : EDBF_DATATYPE;

    hb_errPutGenCode(pError, errGenCode);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(errGenCode));
    hb_errPutOperation(pError, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
    hb_errPutSubCode(pError, errCode);
    hb_errPutFlags(pError, EF_CANDEFAULT);
    errCode = SELF_ERROR(&pArea->area, pError);
    hb_itemRelease(pError);
    return errCode == E_DEFAULT ? Harbour::SUCCESS : Harbour::FAILURE;
  }

  return Harbour::SUCCESS;
}

/*
 * Replace the current record.
 */
static HB_ERRCODE hb_sdfPutRec(SDFAREAP pArea, HB_BYTE *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfPutRec(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pBuffer)));
#endif

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (!pArea->fRecordChanged)
  {
    return Harbour::FAILURE;
  }

  /* Copy data to buffer */
  memcpy(pArea->pRecord, pBuffer + 1, pArea->uiRecordLen);

  return Harbour::SUCCESS;
}

/*
 * Retrieve current record buffer
 */
static HB_ERRCODE hb_sdfGetRec(SDFAREAP pArea, HB_BYTE **pBufferPtr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGetRec(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pBufferPtr)));
#endif

  *pBufferPtr = pArea->pRecord - 1;

  return Harbour::SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_sdfTrans(SDFAREAP pArea, LPDBTRANSINFO pTransInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfTrans(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pTransInfo)));
#endif

  if (pTransInfo->uiFlags & DBTF_MATCH)
  {
    if (!pArea->fTransRec || pArea->area.cdPage != pTransInfo->lpaDest->cdPage)
    {
      pTransInfo->uiFlags &= ~DBTF_PUTREC;
    }
    else if (pArea->area.rddID == pTransInfo->lpaDest->rddID)
    {
      pTransInfo->uiFlags |= DBTF_PUTREC;
    }
    else
    {
      auto pPutRec = hb_itemPutL(nullptr, false);
      if (SELF_INFO(pTransInfo->lpaDest, DBI_CANPUTREC, pPutRec) != Harbour::SUCCESS)
      {
        hb_itemRelease(pPutRec);
        return Harbour::FAILURE;
      }
      if (hb_itemGetL(pPutRec))
      {
        pTransInfo->uiFlags |= DBTF_PUTREC;
      }
      else
      {
        pTransInfo->uiFlags &= ~DBTF_PUTREC;
      }
      hb_itemRelease(pPutRec);
    }
  }
  return SUPER_TRANS(&pArea->area, pTransInfo);
}

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_sdfGoCold(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoCold(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fRecordChanged)
  {
    HB_SIZE nSize = pArea->uiRecordLen + pArea->uiEolLen;

    if (hb_fileWrite(pArea->pFile, pArea->pRecord, nSize, -1) != nSize)
    {
      PHB_ITEM pError = hb_errNew();

      hb_errPutGenCode(pError, EG_WRITE);
      hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_WRITE));
      hb_errPutSubCode(pError, EDBF_WRITE);
      hb_errPutOsCode(pError, hb_fsError());
      hb_errPutFileName(pError, pArea->szFileName);
      SELF_ERROR(&pArea->area, pError);
      hb_itemRelease(pError);
      return Harbour::FAILURE;
    }
    pArea->fRecordChanged = false;
    pArea->fFlush = true;
  }
  return Harbour::SUCCESS;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_sdfGoHot(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfGoHot(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fReadonly)
  {
    PHB_ITEM pError = hb_errNew();
    hb_errPutGenCode(pError, EG_READONLY);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_READONLY));
    hb_errPutSubCode(pError, EDBF_READONLY);
    SELF_ERROR(&pArea->area, pError);
    hb_itemRelease(pError);
    return Harbour::FAILURE;
  }
  pArea->fRecordChanged = true;
  return Harbour::SUCCESS;
}

/*
 * Write data buffer to the data store.
 */
static HB_ERRCODE hb_sdfFlush(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfFlush(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->area);

  if (pArea->fFlush && hb_setGetHardCommit())
  {
    hb_fileCommit(pArea->pFile);
    pArea->fFlush = false;
  }

  return errCode;
}

/*
 * Retrieve information about the current table/driver.
 */
static HB_ERRCODE hb_sdfInfo(SDFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfInfo(%p,%hu,%p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  switch (uiIndex)
  {
  case DBI_CANPUTREC:
    hb_itemPutL(pItem, pArea->fTransRec);
    break;

  case DBI_GETRECSIZE:
    hb_itemPutNL(pItem, pArea->uiRecordLen);
    break;

  case DBI_FULLPATH:
    hb_itemPutC(pItem, pArea->szFileName);
    break;

  case DBI_FILEHANDLE:
    hb_itemPutNInt(pItem, static_cast<HB_NHANDLE>(hb_fileHandle(pArea->pFile)));
    break;

  case DBI_SHARED:
    hb_itemPutL(pItem, pArea->fShared);
    break;

  case DBI_ISREADONLY:
    hb_itemPutL(pItem, pArea->fReadonly);
    break;

  case DBI_POSITIONED:
    hb_itemPutL(pItem, pArea->fPositioned);
    break;

  case DBI_DB_VERSION:
  case DBI_RDD_VERSION: {
    char szBuf[64];
    int iSub = hb_itemGetNI(pItem);

    if (iSub == 1)
    {
      hb_snprintf(szBuf, sizeof(szBuf), "%d.%d (%s)", 0, 1, "SDF");
    }
    else if (iSub == 2)
    {
      hb_snprintf(szBuf, sizeof(szBuf), "%d.%d (%s:%d)", 0, 1, "SDF", pArea->area.rddID);
    }
    else
    {
      hb_snprintf(szBuf, sizeof(szBuf), "%d.%d", 0, 1);
    }
    hb_itemPutC(pItem, szBuf);
    break;
  }

  default:
    return SUPER_INFO(&pArea->area, uiIndex, pItem);
  }

  return Harbour::SUCCESS;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_sdfAddField(SDFAREAP pArea, LPDBFIELDINFO pFieldInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfAddField(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFieldInfo)));
#endif

  switch (pFieldInfo->uiType)
  {
  case Harbour::DB::Field::MEMO:
  case Harbour::DB::Field::IMAGE:
  case Harbour::DB::Field::BLOB:
  case Harbour::DB::Field::OLE:
    pFieldInfo->uiType = Harbour::DB::Field::MEMO;
    pFieldInfo->uiLen = 0;
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::ANY:
    if (pFieldInfo->uiLen == 3)
    {
      pFieldInfo->uiType = Harbour::DB::Field::DATE;
      pFieldInfo->uiLen = 8;
    }
    else if (pFieldInfo->uiLen < 6)
    {
      pFieldInfo->uiType = Harbour::DB::Field::LONG;
      pFieldInfo->uiLen = s_uiNumLength[pFieldInfo->uiLen];
    }
    else
    {
      pFieldInfo->uiType = Harbour::DB::Field::MEMO;
      pFieldInfo->uiLen = 0;
    }
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::DATE:
    if (pFieldInfo->uiLen != 8)
    {
      pFieldInfo->uiLen = 8;
      pArea->fTransRec = false;
    }
    break;

  case Harbour::DB::Field::STRING:
  case Harbour::DB::Field::LONG:
    break;

  case Harbour::DB::Field::FLOAT:
    pFieldInfo->uiType = Harbour::DB::Field::LONG;
    break;

  case Harbour::DB::Field::INTEGER:
  case Harbour::DB::Field::CURRENCY:
  case Harbour::DB::Field::ROWVER:
  case Harbour::DB::Field::AUTOINC:
    pFieldInfo->uiType = Harbour::DB::Field::LONG;
    pFieldInfo->uiLen = s_uiNumLength[pFieldInfo->uiLen];
    if (pFieldInfo->uiDec)
    {
      pFieldInfo->uiLen++;
    }
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::DOUBLE:
  case Harbour::DB::Field::CURDOUBLE:
    pFieldInfo->uiType = Harbour::DB::Field::LONG;
    pFieldInfo->uiLen = 20;
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::VARLENGTH:
    pFieldInfo->uiType = Harbour::DB::Field::STRING;
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::LOGICAL:
    if (pFieldInfo->uiLen != 1)
    {
      pFieldInfo->uiLen = 1;
      pArea->fTransRec = false;
    }
    break;

  case Harbour::DB::Field::TIME:
    pFieldInfo->uiType = Harbour::DB::Field::TIMESTAMP;
    pFieldInfo->uiLen = 12;
    pArea->fTransRec = false;
    break;

  case Harbour::DB::Field::TIMESTAMP:
  case Harbour::DB::Field::MODTIME:
    pFieldInfo->uiType = Harbour::DB::Field::TIMESTAMP;
    pFieldInfo->uiLen = 23;
    pArea->fTransRec = false;
    break;

  default:
    pFieldInfo->uiType = Harbour::DB::Field::NONE;
    pFieldInfo->uiLen = 0;
    pArea->fTransRec = false;
    break;
  }

  pFieldInfo->uiFlags &= ~HB_FF_AUTOINC;

  /* Update field offset */
  pArea->pFieldOffset[pArea->area.uiFieldCount] = pArea->uiRecordLen;
  pArea->uiRecordLen += pFieldInfo->uiLen;

  return SUPER_ADDFIELD(&pArea->area, pFieldInfo);
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_sdfSetFieldExtent(SDFAREAP pArea, HB_USHORT uiFieldExtent)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfSetFieldExtent(%p,%hu)", static_cast<void*>(pArea), uiFieldExtent));
#endif

  if (SUPER_SETFIELDEXTENT(&pArea->area, uiFieldExtent) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  /* Alloc field offsets array */
  if (uiFieldExtent)
  {
    pArea->pFieldOffset = static_cast<HB_USHORT *>(hb_xgrabz(uiFieldExtent * sizeof(HB_USHORT)));
  }

  return Harbour::SUCCESS;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_sdfNewArea(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfNewArea(%p)", static_cast<void*>(pArea)));
#endif

  if (SUPER_NEW(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pArea->pFile = nullptr;
  pArea->fTransRec = true;
  pArea->uiRecordLen = 0;
  pArea->nBufferSize = 0;

  return Harbour::SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_sdfStructSize(SDFAREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfStrucSize(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(uiSize)));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(SDFAREA);
  return Harbour::SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_sdfClose(SDFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfClose(%p)", static_cast<void*>(pArea)));
#endif

  /* Update record and unlock records */
  if (pArea->pFile)
  {
    SELF_GOCOLD(&pArea->area);

    if (!pArea->fReadonly && hb_setGetEOF())
    {
      hb_fileWrite(pArea->pFile, "\032", 1, -1);
      pArea->fFlush = true;
    }
    SELF_FLUSH(&pArea->area);
    hb_fileClose(pArea->pFile);
    pArea->pFile = nullptr;
  }

  SUPER_CLOSE(&pArea->area);

  if (pArea->pFieldOffset)
  {
    hb_xfree(pArea->pFieldOffset);
    pArea->pFieldOffset = nullptr;
  }
  if (pArea->pRecord)
  {
    hb_xfree(pArea->pRecord - 1);
    pArea->pRecord = nullptr;
  }
  if (pArea->pBuffer)
  {
    hb_xfree(pArea->pBuffer);
    pArea->pBuffer = nullptr;
  }
  if (pArea->szEol)
  {
    hb_xfree(pArea->szEol);
    pArea->szEol = nullptr;
  }
  if (pArea->szFileName)
  {
    hb_xfree(pArea->szFileName);
    pArea->szFileName = nullptr;
  }

  return Harbour::SUCCESS;
}

/*
 * Create a data store in the specified WorkArea.
 */
static HB_ERRCODE hb_sdfCreate(SDFAREAP pArea, LPDBOPENINFO pCreateInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfCreate(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pCreateInfo)));
#endif

  PHB_ITEM pError = nullptr;
  auto fRetry = false;
  PHB_FNAME pFileName;
  char szFileName[HB_PATH_MAX];

  pArea->fShared = false;   /* pCreateInfo->fShared; */
  pArea->fReadonly = false; /* pCreateInfo->fReadonly */

  if (pCreateInfo->cdpId)
  {
    pArea->area.cdPage = hb_cdpFindExt(pCreateInfo->cdpId);
    if (!pArea->area.cdPage)
    {
      pArea->area.cdPage = hb_vmCDP();
    }
  }
  else
  {
    pArea->area.cdPage = hb_vmCDP();
  }

  pFileName = hb_fsFNameSplit(pCreateInfo->abName);
  if (hb_setGetDefExtension() && !pFileName->szExtension)
  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_INFO(&pArea->area, DBI_TABLEEXT, pItem) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pItem);
      hb_fsFNameMerge(szFileName, pFileName);
    }
    hb_itemRelease(pItem);
  }
  else
  {
    hb_strncpy(szFileName, pCreateInfo->abName, sizeof(szFileName) - 1);
  }
  hb_xfree(pFileName);

  /* Try create */
  do
  {
    pArea->pFile = hb_fileExtOpen(
        szFileName, nullptr, FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE | FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
        nullptr, pError);
    if (!pArea->pFile)
    {
      if (!pError)
      {
        pError = hb_errNew();
        hb_errPutGenCode(pError, EG_CREATE);
        hb_errPutSubCode(pError, EDBF_CREATE_DBF);
        hb_errPutOsCode(pError, hb_fsError());
        hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_CREATE));
        hb_errPutFileName(pError, szFileName);
        hb_errPutFlags(pError, EF_CANRETRY | EF_CANDEFAULT);
      }
      fRetry = (SELF_ERROR(&pArea->area, pError) == E_RETRY);
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

  if (!pArea->pFile)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = SUPER_CREATE(&pArea->area, pCreateInfo);
  if (errCode != Harbour::SUCCESS)
  {
    SELF_CLOSE(&pArea->area);
    return errCode;
  }

  hb_sdfInitArea(pArea, szFileName);
  pArea->ulRecNo = 1;
  pArea->area.fEof = true;
  pArea->fPositioned = false;
  hb_sdfClearRecordBuffer(pArea);

  return Harbour::SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_sdfOpen(SDFAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfOpen(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pOpenInfo)));
#endif

  PHB_ITEM pError = nullptr;
  PHB_FNAME pFileName;
  HB_USHORT uiFlags;
  auto fRetry = false;
  char szFileName[HB_PATH_MAX];
  char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];

  pArea->fShared = true;   /* pOpenInfo->fShared; */
  pArea->fReadonly = true; /* pOpenInfo->fReadonly; */

  if (pOpenInfo->cdpId)
  {
    pArea->area.cdPage = hb_cdpFindExt(pOpenInfo->cdpId);
    if (!pArea->area.cdPage)
    {
      pArea->area.cdPage = hb_vmCDP();
    }
  }
  else
  {
    pArea->area.cdPage = hb_vmCDP();
  }

  uiFlags = (pArea->fReadonly ? FO_READ : FO_READWRITE) | (pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE);

  pFileName = hb_fsFNameSplit(pOpenInfo->abName);
  /* Add default file name extension if necessary */
  if (hb_setGetDefExtension() && !pFileName->szExtension)
  {
    auto pFileExt = hb_itemNew(nullptr);
    if (SELF_INFO(&pArea->area, DBI_TABLEEXT, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
      hb_fsFNameMerge(szFileName, pFileName);
    }
    hb_itemRelease(pFileExt);
  }
  else
  {
    hb_strncpy(szFileName, pOpenInfo->abName, sizeof(szFileName) - 1);
  }

  /* Create default alias if necessary */
  if (!pOpenInfo->atomAlias && pFileName->szName)
  {
    const char *szName = strrchr(pFileName->szName, ':');
    if (szName == nullptr)
    {
      szName = pFileName->szName;
    }
    else
    {
      ++szName;
    }
    hb_strncpyUpperTrim(szAlias, szName, sizeof(szAlias) - 1);
    pOpenInfo->atomAlias = szAlias;
  }
  hb_xfree(pFileName);

  /* Try open */
  do
  {
    pArea->pFile =
        hb_fileExtOpen(szFileName, nullptr, uiFlags | FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME, nullptr, pError);
    if (!pArea->pFile)
    {
      if (!pError)
      {
        pError = hb_errNew();
        hb_errPutGenCode(pError, EG_OPEN);
        hb_errPutSubCode(pError, EDBF_OPEN_DBF);
        hb_errPutOsCode(pError, hb_fsError());
        hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_OPEN));
        hb_errPutFileName(pError, szFileName);
        hb_errPutFlags(pError, EF_CANRETRY | EF_CANDEFAULT);
      }
      fRetry = (SELF_ERROR(&pArea->area, pError) == E_RETRY);
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

  if (!pArea->pFile)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = SUPER_OPEN(&pArea->area, pOpenInfo);
  if (errCode != Harbour::SUCCESS)
  {
    SELF_CLOSE(&pArea->area);
    return Harbour::FAILURE;
  }

  hb_sdfInitArea(pArea, szFileName);

  /* Position cursor at the first record */
  return SELF_GOTOP(&pArea->area);
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_sdfRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_sdfRddInfo(%p,%hu,%lu,%p)", static_cast<void*>(pRDD), uiIndex, ulConnect, static_cast<void*>(pItem)));
#endif

  switch (uiIndex)
  {
  case RDDI_CANPUTREC:
  case RDDI_LOCAL:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_TABLEEXT:
    hb_itemPutC(pItem, SDF_TABLEEXT);
    break;

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

static const RDDFUNCS sdfTable = {
    nullptr /* hb_sdfBof */,
    nullptr /* hb_sdfEof */,
    nullptr /* hb_sdfFound */,
    nullptr /* hb_sdfGoBottom */,
    (DBENTRYP_UL)hb_sdfGoTo,
    (DBENTRYP_I)hb_sdfGoToId,
    (DBENTRYP_V)hb_sdfGoTop,
    nullptr /* hb_sdfSeek */,
    nullptr /* hb_sdfSkip */,
    nullptr /* hb_sdfSkipFilter */,
    (DBENTRYP_L)hb_sdfSkipRaw,
    (DBENTRYP_VF)hb_sdfAddField,
    (DBENTRYP_B)hb_sdfAppend,
    nullptr /* hb_sdfCreateFields */,
    (DBENTRYP_V)hb_sdfDeleteRec,
    (DBENTRYP_BP)hb_sdfDeleted,
    nullptr /* hb_sdfFieldCount */,
    nullptr /* hb_sdfFieldDisplay */,
    nullptr /* hb_sdfFieldInfo */,
    nullptr /* hb_sdfFieldName */,
    (DBENTRYP_V)hb_sdfFlush,
    (DBENTRYP_PP)hb_sdfGetRec,
    (DBENTRYP_SI)hb_sdfGetValue,
    nullptr /* hb_sdfGetVarLen */,
    (DBENTRYP_V)hb_sdfGoCold,
    (DBENTRYP_V)hb_sdfGoHot,
    (DBENTRYP_P)hb_sdfPutRec,
    (DBENTRYP_SI)hb_sdfPutValue,
    (DBENTRYP_V)hb_sdfRecall,
    (DBENTRYP_ULP)hb_sdfRecCount,
    nullptr /* hb_sdfRecInfo */,
    (DBENTRYP_ULP)hb_sdfRecNo,
    (DBENTRYP_I)hb_sdfRecId,
    (DBENTRYP_S)hb_sdfSetFieldExtent,
    nullptr /* hb_sdfAlias */,
    (DBENTRYP_V)hb_sdfClose,
    (DBENTRYP_VO)hb_sdfCreate,
    (DBENTRYP_SI)hb_sdfInfo,
    (DBENTRYP_V)hb_sdfNewArea,
    (DBENTRYP_VO)hb_sdfOpen,
    nullptr /* hb_sdfRelease */,
    (DBENTRYP_SP)hb_sdfStructSize,
    nullptr /* hb_sdfSysName */,
    nullptr /* hb_sdfEval */,
    nullptr /* hb_sdfPack */,
    nullptr /* hb_sdfPackRec */,
    nullptr /* hb_sdfSort */,
    (DBENTRYP_VT)hb_sdfTrans,
    nullptr /* hb_sdfTransRec */,
    nullptr /* hb_sdfZap */,
    nullptr /* hb_sdfChildEnd */,
    nullptr /* hb_sdfChildStart */,
    nullptr /* hb_sdfChildSync */,
    nullptr /* hb_sdfSyncChildren */,
    nullptr /* hb_sdfClearRel */,
    nullptr /* hb_sdfForceRel */,
    nullptr /* hb_sdfRelArea */,
    nullptr /* hb_sdfRelEval */,
    nullptr /* hb_sdfRelText */,
    nullptr /* hb_sdfSetRel */,
    nullptr /* hb_sdfOrderListAdd */,
    nullptr /* hb_sdfOrderListClear */,
    nullptr /* hb_sdfOrderListDelete */,
    nullptr /* hb_sdfOrderListFocus */,
    nullptr /* hb_sdfOrderListRebuild */,
    nullptr /* hb_sdfOrderCondition */,
    nullptr /* hb_sdfOrderCreate */,
    nullptr /* hb_sdfOrderDestroy */,
    nullptr /* hb_sdfOrderInfo */,
    nullptr /* hb_sdfClearFilter */,
    nullptr /* hb_sdfClearLocate */,
    nullptr /* hb_sdfClearScope */,
    nullptr /* hb_sdfCountScope */,
    nullptr /* hb_sdfFilterText */,
    nullptr /* hb_sdfScopeInfo */,
    nullptr /* hb_sdfSetFilter */,
    nullptr /* hb_sdfSetLocate */,
    nullptr /* hb_sdfSetScope */,
    nullptr /* hb_sdfSkipScope */,
    nullptr /* hb_sdfLocate */,
    nullptr /* hb_sdfCompile */,
    nullptr /* hb_sdfError */,
    nullptr /* hb_sdfEvalBlock */,
    nullptr /* hb_sdfRawLock */,
    nullptr /* hb_sdfLock */,
    nullptr /* hb_sdfUnLock */,
    nullptr /* hb_sdfCloseMemFile */,
    nullptr /* hb_sdfCreateMemFile */,
    nullptr /* hb_sdfGetValueFile */,
    nullptr /* hb_sdfOpenMemFile */,
    nullptr /* hb_sdfPutValueFile */,
    nullptr /* hb_sdfReadDBHeader */,
    nullptr /* hb_sdfWriteDBHeader */,
    nullptr /* hb_sdfInit */,
    nullptr /* hb_sdfExit */,
    nullptr /* hb_sdfDrop */,
    nullptr /* hb_sdfExists */,
    nullptr /* hb_sdfRename */,
    (DBENTRYP_RSLV)hb_sdfRddInfo,
    nullptr /* hb_sdfWhoCares */
};

HB_FUNC(SDF)
{
  ;
}

HB_FUNC_STATIC(SDF_GETFUNCTABLE)
{
  auto puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  auto pTable = static_cast<RDDFUNCS *>(hb_parptr(2));

#if 0
   HB_TRACE(HB_TR_DEBUG, ("SDF_GETFUNCTABLE(%p, %p)", static_cast<void*>(puiCount), static_cast<void*>(pTable)));
#endif

  if (pTable)
  {
    if (puiCount)
    {
      *puiCount = RDDFUNCSCOUNT;
    }
    hb_retni(hb_rddInheritEx(pTable, &sdfTable, &sdfSuper, nullptr, nullptr));
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

static void hb_sdfRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("SDF", RDT_TRANSFER) > 1)
  {
    hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
  }
}

HB_INIT_SYMBOLS_BEGIN(sdf1__InitSymbols){"SDF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(SDF)}, nullptr},
    {"SDF_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(SDF_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(sdf1__InitSymbols)

        HB_CALL_ON_STARTUP_BEGIN(_hb_sdf_rdd_init_) hb_vmAtInit(hb_sdfRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_sdf_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup sdf1__InitSymbols
#pragma startup _hb_sdf_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(sdf1__InitSymbols)                                                                                   \
  HB_DATASEG_FUNC(_hb_sdf_rdd_init_)
#include "hbiniseg.hpp"
#endif
