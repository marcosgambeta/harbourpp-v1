//
// DBF RDD module
//
// Copyright 2003-2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define HB_TRIGVAR_BYREF

#include "hbrdddbf.hpp"
#include "hbapiitm.hpp"
#include "hbapistr.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbset.hpp"
#include "hbdate.hpp"
#include "hbsxfunc.hpp"
#include "hbstack.hpp"
#include "hbvm.hpp"
#include "error.ch"
#include "rddsys.ch"
#include "hbsxdef.ch"

#include "hbapicdp.hpp"

static HB_USHORT s_uiRddId = static_cast<HB_USHORT>(-1);
static RDDFUNCS dbfSuper;

// Common functions.

#define HB_BLANK_APPEND 1
#define HB_BLANK_EOF 2
#define HB_BLANK_ROLLBACK 3

#define HB_BLANK_SKIP 100
#define HB_BLANK_AUTOINC 101
#define HB_BLANK_UNISPACE 102

#define HB_AUTOINC_NONE 0
#define HB_AUTOINC_STD 1
#define HB_AUTOINC_LONG 2

// generate Run-Time error
static HB_ERRCODE hb_dbfErrorRT(DBFAREAP pArea, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *szFileName,
                                HB_ERRCODE errOsCode, HB_USHORT uiFlags, PHB_ITEM *pErrorPtr)
{
  HB_ERRCODE errCode = Harbour::FAILURE;

  if (hb_vmRequestQuery() == 0)
  {
    PHB_ITEM pError;

    if (pErrorPtr)
    {
      if (!*pErrorPtr)
      {
        *pErrorPtr = hb_errNew();
      }
      pError = *pErrorPtr;
    }
    else
    {
      pError = hb_errNew();
    }
    hb_errPutGenCode(pError, errGenCode);
    hb_errPutSubCode(pError, errSubCode);
    hb_errPutOsCode(pError, errOsCode);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(errGenCode));
    if (szFileName != nullptr)
    {
      hb_errPutFileName(pError, szFileName);
    }
    if (uiFlags)
    {
      hb_errPutFlags(pError, uiFlags);
    }
    errCode = SELF_ERROR(&pArea->area, pError);
    if (!pErrorPtr)
    {
      hb_errRelease(pError);
    }
  }
  return errCode;
}

static HB_MAXINT hb_dbfRowVerGet(DBFAREAP pArea, HB_USHORT uiField, HB_MAXINT *pValue)
{
  DBFFIELD dbField;
  auto fLck = false;

  *pValue = 0;
  if (pArea->fShared && !pArea->fFLocked && !pArea->fHeaderLocked)
  {
    if (SELF_RAWLOCK(&pArea->area, HEADER_LOCK, 0) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    fLck = true;
  }

  if (hb_fileReadAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD)) ==
      sizeof(dbField))
  {
    *pValue = HB_GET_LE_UINT64(dbField.bReserved2) + 1;
    HB_PUT_LE_UINT64(dbField.bReserved2, *pValue);
    hb_fileWriteAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD));
  }

  if (fLck)
  {
    if (SELF_RAWLOCK(&pArea->area, HEADER_UNLOCK, 0) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

static void hb_dbfRowVerSet(DBFAREAP pArea, HB_USHORT uiField, HB_MAXINT nValue)
{
  DBFFIELD dbField;

  if (hb_fileReadAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD)) ==
      sizeof(dbField))
  {
    HB_PUT_LE_UINT64(dbField.bReserved2, nValue);
    hb_fileWriteAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD));
  }
}

static HB_BOOL hb_dbfIsAutoIncField(LPFIELD pField)
{
  if (pField->uiType == Harbour::DB::Field::AUTOINC)
  {
    return pField->uiLen - pField->uiDec > 4 ? HB_AUTOINC_LONG : HB_AUTOINC_STD;
  }
  else if (pField->uiType == Harbour::DB::Field::ROWVER)
  {
    return HB_AUTOINC_LONG;
  }
  else if ((pField->uiFlags & HB_FF_AUTOINC) != 0)
  {
    switch (pField->uiType)
    {
    case Harbour::DB::Field::DOUBLE:
      return HB_AUTOINC_LONG;
    case Harbour::DB::Field::LONG:
    case Harbour::DB::Field::FLOAT:
      return pField->uiLen - (pField->uiDec ? pField->uiDec + 1 : 0) > 9 ? HB_AUTOINC_LONG : HB_AUTOINC_STD;
    case Harbour::DB::Field::INTEGER:
      return pField->uiLen - pField->uiDec > 4 ? HB_AUTOINC_LONG : HB_AUTOINC_STD;
    }
  }
  return HB_AUTOINC_NONE;
}

static void hb_dbfNextValueInit(LPDBFFIELD pDbField, LPFIELD pField)
{
  if (hb_dbfIsAutoIncField(pField) == HB_AUTOINC_LONG)
  {
    HB_PUT_LE_UINT64(pDbField->bReserved2, 1);
  }
  else
  {
    HB_PUT_LE_UINT32(pDbField->bCounter, 1);
  }
  pDbField->bStep = 1;
}

static HB_MAXINT hb_dbfNextValueGet(DBFAREAP pArea, HB_USHORT uiField, bool fUpdate)
{
  HB_MAXINT nValue = 0;
  DBFFIELD dbField;

  if (hb_fileReadAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD)) ==
      sizeof(dbField))
  {
    int iType = hb_dbfIsAutoIncField(pArea->area.lpFields + uiField);

    if (iType == HB_AUTOINC_LONG)
    {
      nValue = HB_GET_LE_UINT64(dbField.bReserved2);
    }
    else
    {
      nValue = HB_GET_LE_UINT32(dbField.bCounter);
    }
    if (fUpdate)
    {
      if (iType == HB_AUTOINC_LONG)
      {
        HB_PUT_LE_UINT64(dbField.bReserved2, nValue + dbField.bStep);
      }
      else
      {
        HB_PUT_LE_UINT32(dbField.bCounter, nValue + dbField.bStep);
      }
      hb_fileWriteAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD));
    }
  }

  return nValue;
}

static HB_MAXINT hb_dbfNextValueSet(DBFAREAP pArea, HB_USHORT uiField, HB_MAXINT nValue)
{
  DBFFIELD dbField;
  HB_MAXINT nPrevValue = 0;

  if (hb_fileReadAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD)) ==
      sizeof(dbField))
  {
    if (hb_dbfIsAutoIncField(pArea->area.lpFields + uiField) == HB_AUTOINC_LONG)
    {
      nPrevValue = HB_GET_LE_UINT64(dbField.bReserved2);
      HB_PUT_LE_UINT64(dbField.bReserved2, nValue);
    }
    else
    {
      nPrevValue = HB_GET_LE_UINT32(dbField.bCounter);
      HB_PUT_LE_UINT32(dbField.bCounter, nValue);
    }
    hb_fileWriteAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD));
  }
  return nPrevValue;
}

static int hb_dbfNextValueStep(DBFAREAP pArea, HB_USHORT uiField, int iStep)
{
  DBFFIELD dbField;
  int iPrevStep = 0;

  if (hb_fileReadAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD)) ==
      sizeof(dbField))
  {
    iPrevStep = dbField.bStep;
    if (iStep != 0)
    {
      dbField.bStep = static_cast<HB_BYTE>(iStep);
      hb_fileWriteAt(pArea->pDataFile, &dbField, sizeof(dbField), sizeof(DBFHEADER) + uiField * sizeof(DBFFIELD));
    }
  }

  return iPrevStep;
}

static void hb_dbfTransCheckCounters(LPDBTRANSINFO lpdbTransInfo)
{
  auto fCopyCtr = true;
  HB_USHORT uiCount, uiDest;
  DBFAREAP pArea = reinterpret_cast<DBFAREAP>(lpdbTransInfo->lpaDest);

  if (pArea->ulRecCount > 0 || (pArea->fShared && !pArea->fFLocked))
  {
    fCopyCtr = false;
  }
  else
  {
    PHB_ITEM pItem = nullptr;

    // check if counters can be copied for all fields
    for (uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount)
    {
      HB_USHORT uiField = lpdbTransInfo->lpTransItems[uiCount].uiDest;
      LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

      if (hb_dbfIsAutoIncField(pField) != HB_AUTOINC_NONE)
      {
        if (pItem == nullptr)
        {
          pItem = hb_itemNew(nullptr);
        }
        if (SELF_FIELDINFO(lpdbTransInfo->lpaSource, lpdbTransInfo->lpTransItems[uiCount].uiSource, DBS_COUNTER,
                           pItem) != Harbour::SUCCESS)
        {
          fCopyCtr = false;
          break;
        }
      }
    }
    if (pItem != nullptr)
    {
      hb_itemRelease(pItem);
    }
  }

  if (fCopyCtr)
  {
    lpdbTransInfo->uiFlags |= DBTF_CPYCTR;
  }
  else
  {
    for (uiCount = uiDest = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount)
    {
      HB_USHORT uiField = lpdbTransInfo->lpTransItems[uiCount].uiDest;
      LPFIELD pField = lpdbTransInfo->lpaDest->lpFields + uiField - 1;

      if (hb_dbfIsAutoIncField(pField) == HB_AUTOINC_NONE && pField->uiType != Harbour::DB::Field::MODTIME)
      {
        if (uiDest != uiCount)
        {
          lpdbTransInfo->lpTransItems[uiDest].uiSource = lpdbTransInfo->lpTransItems[uiCount].uiSource;
          lpdbTransInfo->lpTransItems[uiDest].uiDest = lpdbTransInfo->lpTransItems[uiCount].uiDest;
        }
        ++uiDest;
      }
    }
    if (uiDest < uiCount)
    {
      lpdbTransInfo->uiItemCount = uiDest;
      lpdbTransInfo->uiFlags &= ~(DBTF_MATCH | DBTF_PUTREC);
    }
  }
}

static void hb_dbfUpdateStampFields(DBFAREAP pArea)
{
  long lJulian = 0, lMilliSec = 0;
  HB_MAXINT nRowVer = 0;
  LPFIELD pField;
  HB_USHORT uiCount;

  for (uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++)
  {
    switch (pField->uiType)
    {
    case Harbour::DB::Field::MODTIME:
    {
      HB_BYTE *pPtr = pArea->pRecord + pArea->pFieldOffset[uiCount];
      if (!pArea->fTransRec || HB_GET_LE_UINT64(pPtr) == 0)
      {
        if (lJulian == 0)
        {
          hb_timeStampGet(&lJulian, &lMilliSec);
        }
        HB_PUT_LE_UINT32(pPtr, lJulian);
        pPtr += 4;
        HB_PUT_LE_UINT32(pPtr, lMilliSec);
      }
      break;
    }
    case Harbour::DB::Field::ROWVER:
    {
      HB_BYTE *pPtr = pArea->pRecord + pArea->pFieldOffset[uiCount];
      if (!pArea->fTransRec || HB_GET_LE_UINT64(pPtr) == 0)
      {
        if (nRowVer == 0)
        {
          hb_dbfRowVerGet(pArea, uiCount, &nRowVer);
        }
        HB_PUT_LE_UINT64(pPtr, nRowVer);
      }
      break;
    }
    }
  }
}

static void hb_dbfSetBlankRecord(DBFAREAP pArea, int iType)
{
  HB_BYTE *pPtr = pArea->pRecord, bFill = ' ', bNext;
  HB_SIZE nSize = 1; // 1 byte ' ' for DELETE flag
  HB_USHORT uiCount;
  LPFIELD pField;

  for (uiCount = 0, pField = pArea->area.lpFields; uiCount < pArea->area.uiFieldCount; uiCount++, pField++)
  {
    HB_USHORT uiLen = pField->uiLen;

    switch (pField->uiType)
    {
    case Harbour::DB::Field::MEMO:
    case Harbour::DB::Field::IMAGE:
    case Harbour::DB::Field::BLOB:
    case Harbour::DB::Field::OLE:
      bNext = uiLen == 10 ? ' ' : '\0';
      break;

    case Harbour::DB::Field::DATE:
      bNext = uiLen == 8 ? ' ' : '\0';
      break;

    case Harbour::DB::Field::LOGICAL:
      bNext = ' ';
      break;

    case Harbour::DB::Field::STRING:
      bNext = (pField->uiFlags & HB_FF_UNICODE) != 0 ? HB_BLANK_UNISPACE : ' ';
      break;

    case Harbour::DB::Field::LONG:
    case Harbour::DB::Field::FLOAT:
      if (pField->uiFlags & HB_FF_AUTOINC)
      {
        if (iType == HB_BLANK_APPEND)
        {
          bNext = HB_BLANK_AUTOINC;
          break;
        }
        else if (iType == HB_BLANK_ROLLBACK)
        {
          bNext = HB_BLANK_SKIP;
          break;
        }
      }
      bNext = ' ';
      break;

    case Harbour::DB::Field::AUTOINC:
      if (iType == HB_BLANK_APPEND)
      {
        bNext = HB_BLANK_AUTOINC;
      }
      else if (iType == HB_BLANK_ROLLBACK)
      {
        bNext = HB_BLANK_SKIP;
      }
      else
      {
        bNext = '\0';
      }
      break;

    case Harbour::DB::Field::INTEGER:
    case Harbour::DB::Field::DOUBLE:
      if (pField->uiFlags & HB_FF_AUTOINC)
      {
        if (iType == HB_BLANK_APPEND)
        {
          bNext = HB_BLANK_AUTOINC;
          break;
        }
        else if (iType == HB_BLANK_ROLLBACK)
        {
          bNext = HB_BLANK_SKIP;
          break;
        }
      }
      bNext = '\0';
      break;

    case Harbour::DB::Field::VARLENGTH:
      if (pField->uiFlags & HB_FF_UNICODE)
      {
        uiLen = (uiLen + 1) << 1;
      }
      // fallthrough

    default:
      bNext = '\0';
      break;
    }

    if (bNext == bFill)
    {
      nSize += uiLen;
    }
    else
    {
      if (nSize)
      {
        memset(pPtr, bFill, nSize);
        pPtr += nSize;
        nSize = 0;
      }
      if (bNext == HB_BLANK_SKIP)
      {
        pPtr += uiLen;
      }
      else if (bNext == HB_BLANK_UNISPACE)
      {
        while (uiLen--)
        {
          HB_PUT_LE_UINT16(pPtr, 0x0020);
          pPtr += 2;
        }
      }
      else if (bNext == HB_BLANK_AUTOINC)
      {
        HB_MAXINT nValue = hb_dbfNextValueGet(pArea, uiCount, true);
        if (pField->uiType == Harbour::DB::Field::INTEGER || pField->uiType == Harbour::DB::Field::AUTOINC)
        {
          if (pField->uiDec)
          {
            nValue =
                static_cast<HB_MAXINT>(hb_numDecConv(static_cast<double>(nValue), -static_cast<int>(pField->uiDec)));
          }
          if (uiLen == 1)
          { // TODO: switch ?
            *pPtr = static_cast<signed char>(nValue);
          }
          else if (uiLen == 2)
          {
            HB_PUT_LE_UINT16(pPtr, nValue);
          }
          else if (uiLen == 3)
          {
            HB_PUT_LE_UINT24(pPtr, nValue);
          }
          else if (uiLen == 4)
          {
            HB_PUT_LE_UINT32(pPtr, nValue);
          }
          else if (uiLen == 8)
          {
            HB_PUT_LE_UINT64(pPtr, nValue);
          }
        }
        else if (pField->uiType == Harbour::DB::Field::DOUBLE)
        {
          HB_PUT_LE_DOUBLE(pPtr, nValue);
        }
        else
        {
          HB_USHORT ui = uiLen;
          do
          {
            pPtr[--ui] = static_cast<HB_BYTE>(nValue) % 10 + '0';
            nValue /= 10;
          } while (ui && nValue > 0);
          while (ui)
          {
            pPtr[--ui] = ' ';
          }
        }
        pPtr += uiLen;
      }
      else
      {
        nSize = uiLen;
        bFill = bNext;
      }
    }
  }
  memset(pPtr, bFill, nSize);

  nSize += pPtr - pArea->pRecord;
  if (nSize < static_cast<HB_SIZE>(pArea->uiRecordLen))
  {
    memset(pArea->pRecord + nSize, '\0', static_cast<HB_SIZE>(pArea->uiRecordLen) - nSize);
  }

  // set varlength and nullable bits in _NullFlags
  if (pArea->uiNullCount)
  {
    memset(pArea->pRecord + pArea->uiNullOffset, 0xff, pArea->uiNullCount >> 3);
    uiCount = pArea->uiNullCount & 0x07;
    if (uiCount)
    {
      pArea->pRecord[pArea->uiNullOffset + (pArea->uiNullCount >> 3)] = (1 << uiCount) - 1;
    }
  }
}

static void hb_dbfAllocNullFlag(DBFAREAP pArea, HB_USHORT uiField, bool fLength)
{
  if (!pArea->pFieldBits)
  {
    HB_SIZE nSize = sizeof(HB_DBFFIELDBITS) * pArea->area.uiFieldExtent;
    pArea->pFieldBits = static_cast<PHB_DBFFIELDBITS>(hb_xgrabz(nSize));
  }
  if (fLength)
  {
    pArea->pFieldBits[uiField].uiLengthBit = pArea->uiNullCount++;
  }
  else
  {
    pArea->pFieldBits[uiField].uiNullBit = pArea->uiNullCount++;
  }
}

static bool hb_dbfGetNullFlag(DBFAREAP pArea, HB_USHORT uiBit)
{
  return (pArea->pRecord[pArea->uiNullOffset + (uiBit >> 3)] & (1 << (uiBit & 0x07))) != 0;
}

static void hb_dbfSetNullFlag(HB_BYTE *pRecord, HB_USHORT uiNullOffset, HB_USHORT uiBit)
{
  pRecord[uiNullOffset + (uiBit >> 3)] |= 1 << (uiBit & 0x07);
}

static void hb_dbfClearNullFlag(HB_BYTE *pRecord, HB_USHORT uiNullOffset, HB_USHORT uiBit)
{
  pRecord[uiNullOffset + (uiBit >> 3)] &= ~(1 << (uiBit & 0x07));
}

// Executes user trigger function
static bool hb_dbfTriggerDo(DBFAREAP pArea, int iEvent, int iField, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfTriggerDo(%p,%d,%d,%p)", static_cast<void*>(pArea), iEvent, iField, pItem));
#endif

  auto fResult = true;

  if (hb_vmRequestQuery() == 0)
  {
    if (hb_vmRequestReenter())
    {
      hb_vmPushDynSym(pArea->pTriggerSym);
      hb_vmPushNil();
      // nEvent
      hb_vmPushInteger(iEvent);
      // nArea
      hb_vmPushInteger(pArea->area.uiArea);
      // nFieldPos (GET/PUT)
      hb_vmPushInteger(iField);
      // xTrigVal (PREUSE/GET/PUT)
      if (pItem != nullptr)
      {
#ifdef HB_TRIGVAR_BYREF
        hb_vmPushItemRef(pItem);
#else
        hb_vmPush(pItem);
#endif
        hb_vmProc(4);
      }
      else
      {
#if 0
            hb_vmPushInteger(0); // SIx3 makes this
#endif
        hb_vmProc(3);
      }
      fResult = hb_parl(-1);
      hb_vmRequestRestore();
    }
  }

  return fResult;
}

// Set user trigger function
static void hb_dbfTriggerSet(DBFAREAP pArea, PHB_ITEM pTrigger)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfTriggerSet(%p,%p)", static_cast<void*>(pArea), static_cast<void*>(pTrigger)));
#endif

  auto szName = hb_itemGetCPtr(pTrigger);
  pArea->pTriggerSym = *szName ? hb_dynsymFindName(szName) : nullptr;
  if (pArea->pTriggerSym && !hb_dynsymIsFunction(pArea->pTriggerSym))
  {
    pArea->pTriggerSym = nullptr;
  }
  pArea->fTrigger = pArea->pTriggerSym != nullptr;
}

// Return the total number of records.
static HB_ULONG hb_dbfCalcRecCount(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfCalcRecCount(%p)", static_cast<void*>(pArea)));
#endif

  if (!pArea->pDataFile)
  {
    return 0;
  }
  else
  {
    return static_cast<HB_ULONG>((hb_fileSize(pArea->pDataFile) - pArea->uiHeaderLen) / pArea->uiRecordLen);
  }
}

// Read current record from file.
static bool hb_dbfReadRecord(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfReadRecord(%p)", static_cast<void*>(pArea)));
#endif

  if (!pArea->pRecord)
  {
    return false;
  }

  if (!pArea->fPositioned)
  {
    pArea->fValidBuffer = true;
    return true;
  }

  if (pArea->ulRecNo > pArea->ulRecCount)
  {
    // Update record count
    if (pArea->fShared)
    {
      pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
    }

    if (pArea->ulRecNo > pArea->ulRecCount)
    {
      pArea->area.fEof = pArea->fValidBuffer = true;
      return true;
    }
  }

  // Read data from file
  if (hb_fileReadAt(pArea->pDataFile, pArea->pRecord, pArea->uiRecordLen,
                    static_cast<HB_FOFFSET>(pArea->uiHeaderLen) +
                        static_cast<HB_FOFFSET>(pArea->ulRecNo - 1) * static_cast<HB_FOFFSET>(pArea->uiRecordLen)) !=
      static_cast<HB_SIZE>(pArea->uiRecordLen))
  {
    hb_dbfErrorRT(pArea, EG_READ, EDBF_READ, pArea->szDataFileName, hb_fsError(), 0, nullptr);
    return false;
  }

  if (SELF_GETREC(&pArea->area, nullptr) == Harbour::FAILURE)
  {
    return false;
  }

  // Set flags
  pArea->fValidBuffer = pArea->fPositioned = true;
  pArea->fDeleted = pArea->pRecord[0] == '*';
  return true;
}

// Write current record to file.
static bool hb_dbfWriteRecord(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfWriteRecord(%p)", static_cast<void*>(pArea)));
#endif

  if (SELF_PUTREC(&pArea->area, nullptr) == Harbour::FAILURE)
  {
    return false;
  }

  pArea->fRecordChanged = false;
  pArea->fDataFlush = true;
  return true;
}

// Set encryption password
static bool hb_dbfPasswordSet(DBFAREAP pArea, PHB_ITEM pPasswd, bool fRaw)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPasswordSet(%p,%p,%d)", static_cast<void*>(pArea), static_cast<void*>(pPasswd), fRaw));
#endif

  char pKeyBuffer[8];
  auto fKeySet = false;
  auto fSet = false;

  auto nLen = hb_itemGetCLen(pPasswd);

  fSet = !pArea->fHasMemo && pPasswd->isString() && (!fRaw || nLen == 8);
  if (fSet)
  {
    if (nLen > 0)
    {
      if (nLen < 8)
      {
        memcpy(pKeyBuffer, pPasswd->getCPtr(), nLen);
        memset(pKeyBuffer + nLen, '\0', 8 - nLen);
      }
      else
      {
        memcpy(pKeyBuffer, pPasswd->getCPtr(), 8);
      }
    }
  }

  if (pArea->pCryptKey)
  {
    hb_itemPutCL(pPasswd, pArea->pCryptKey, 8);
  }
  else
  {
    hb_itemClear(pPasswd);
  }

  if (fSet)
  {
    if (pArea->pRecord && pArea->fPositioned)
    {
      SELF_GOCOLD(&pArea->area);
      pArea->fValidBuffer = false;
    }
    if (pArea->pCryptKey)
    {
      // clean the memory with password key - though it's not
      // a serious actions in such case ;-)
      memset(pArea->pCryptKey, '\0', 8);
      hb_xfree(pArea->pCryptKey);
      pArea->pCryptKey = nullptr;
    }
    if (nLen > 0)
    {
      // at this moment only one encryption method is used,
      // I'll add other later, [druzus]
      pArea->bCryptType = DB_CRYPT_SIX;
      pArea->pCryptKey = static_cast<char *>(hb_xgrab(8));

      // SIX encode the key with its own value before use
      if (!fRaw)
      {
        hb_sxEnCrypt(pKeyBuffer, pArea->pCryptKey, pKeyBuffer, 8);
      }
      else
      {
        memcpy(pArea->pCryptKey, pKeyBuffer, 8);
      }
      fKeySet = true;
    }
  }

  return fKeySet;
}

// Encrypt/Decrypt table
static void hb_dbfTableCrypt(DBFAREAP pArea, PHB_ITEM pPasswd, bool fEncrypt)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfTableCrypt(%p,%p,%d)", static_cast<void*>(pArea), static_cast<void*>(pPasswd), fEncrypt));
#endif

  if (!pArea->fReadonly && !pArea->fShared && fEncrypt ? !pArea->fTableEncrypted && !pArea->fHasMemo
                                                       : pArea->fTableEncrypted)
  {
    HB_ULONG ulRecords;

    if (SELF_RECCOUNT(&pArea->area, &ulRecords) == Harbour::SUCCESS)
    {
      HB_ERRCODE errCode = Harbour::SUCCESS;
      char *pOldCryptKey, *pNewCryptKey;

      pOldCryptKey = pArea->pCryptKey;
      pArea->pCryptKey = nullptr;
      hb_dbfPasswordSet(pArea, pPasswd, false);
      pNewCryptKey = pArea->pCryptKey;
      if (!fEncrypt)
      {
        if (pNewCryptKey)
        {
          if (pOldCryptKey)
          {
            hb_xfree(pNewCryptKey);
          }
          else
          {
            pOldCryptKey = pNewCryptKey;
          }
          pNewCryptKey = nullptr;
        }
      }
      else if (!pNewCryptKey)
      {
        pNewCryptKey = pOldCryptKey;
      }

      for (HB_ULONG ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo)
      {
        pArea->pCryptKey = pOldCryptKey;
        errCode = SELF_GOTO(&pArea->area, ulRecNo);
        if (errCode != Harbour::SUCCESS)
        {
          break;
        }
        if (!hb_dbfReadRecord(pArea))
        {
          errCode = Harbour::FAILURE;
          break;
        }
        pArea->pCryptKey = pNewCryptKey;
        // Buffer is hot?
        if (!pArea->fRecordChanged)
        {
          errCode = SELF_GOHOT(&pArea->area);
          if (errCode != Harbour::SUCCESS)
          {
            break;
          }
        }
        // Force record encryption/decryption
        pArea->fEncrypted = fEncrypt;
        // Save encrypted record
        errCode = SELF_GOCOLD(&pArea->area);
        if (errCode != Harbour::SUCCESS)
        {
          break;
        }
      }
      pArea->pCryptKey = pNewCryptKey;
      if (pOldCryptKey && pOldCryptKey != pNewCryptKey)
      {
        hb_xfree(pOldCryptKey);
      }
      if (errCode == Harbour::SUCCESS)
      {
        pArea->fTableEncrypted = fEncrypt;
        SELF_WRITEDBHEADER(&pArea->area);
      }
    }
  }
}

// Unlock all records.
static HB_ERRCODE hb_dbfUnlockAllRecords(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnlockAllRecords(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->pLocksPos)
  {
    errCode = SELF_GOCOLD(&pArea->area);
    for (HB_ULONG ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++)
    {
      SELF_RAWLOCK(&pArea->area, REC_UNLOCK, pArea->pLocksPos[ulCount]);
    }
    hb_xfree(pArea->pLocksPos);
    pArea->pLocksPos = nullptr;
  }
  pArea->ulNumLocksPos = 0;
  return errCode;
}

// Unlock a records.
static HB_ERRCODE hb_dbfUnlockRecord(DBFAREAP pArea, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnlockRecord(%p, %lu)", static_cast<void*>(pArea), ulRecNo));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;
  HB_ULONG ulCount;

  // Search the locked record
  for (ulCount = 0; ulCount < pArea->ulNumLocksPos && pArea->pLocksPos[ulCount] != ulRecNo; ulCount++)
  {
  }

  if (ulCount < pArea->ulNumLocksPos)
  {
    errCode = SELF_GOCOLD(&pArea->area);
    SELF_RAWLOCK(&pArea->area, REC_UNLOCK, ulRecNo);
    if (pArea->ulNumLocksPos == 1)
    { // Delete the list
      hb_xfree(pArea->pLocksPos);
      pArea->pLocksPos = nullptr;
      pArea->ulNumLocksPos = 0;
    }
    else
    { // Resize the list
      HB_ULONG *pList = pArea->pLocksPos + ulCount;
      memmove(pList, pList + 1, (pArea->ulNumLocksPos - ulCount - 1) * sizeof(HB_ULONG));
      pArea->pLocksPos =
          static_cast<HB_ULONG *>(hb_xrealloc(pArea->pLocksPos, (pArea->ulNumLocksPos - 1) * sizeof(HB_ULONG)));
      pArea->ulNumLocksPos--;
    }
  }
  return errCode;
}

// Lock a record.
static HB_ERRCODE hb_dbfLockRecord(DBFAREAP pArea, HB_ULONG ulRecNo, HB_USHORT *pResult, bool bExclusive)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLockRecord(%p, %lu, %p, %i)", static_cast<void*>(pArea), ulRecNo, static_cast<void*>(pResult), static_cast<int>(bExclusive)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (pArea->fFLocked)
  {
    *pResult = HB_TRUE;
    return Harbour::SUCCESS;
  }

  if (ulRecNo == 0)
  {
    ulRecNo = pArea->ulRecNo;
  }

  if (bExclusive)
  {
    hb_dbfUnlockAllRecords(pArea);
  }
  else if (pArea->ulNumLocksPos > 0)
  {
    for (HB_ULONG ul = 0; ul < pArea->ulNumLocksPos; ul++)
    {
      if (pArea->pLocksPos[ul] == ulRecNo)
      {
        *pResult = HB_TRUE;
        return Harbour::SUCCESS;
      }
    }
  }

  if (SELF_RAWLOCK(&pArea->area, REC_LOCK, ulRecNo) == Harbour::SUCCESS)
  {
    if (pArea->ulNumLocksPos == 0)
    { // Create the list
      pArea->pLocksPos = static_cast<HB_ULONG *>(hb_xgrab(sizeof(HB_ULONG)));
    }
    else
    { // Resize the list
      pArea->pLocksPos =
          static_cast<HB_ULONG *>(hb_xrealloc(pArea->pLocksPos, (pArea->ulNumLocksPos + 1) * sizeof(HB_ULONG)));
    }
    pArea->pLocksPos[pArea->ulNumLocksPos++] = ulRecNo;
    *pResult = HB_TRUE;
    if (ulRecNo == pArea->ulRecNo)
    {
      if (!pArea->fPositioned)
      {
        if (SELF_GOTO(&pArea->area, pArea->ulRecNo) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }
      }
      else if (!pArea->fRecordChanged)
      {
        if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }
        pArea->fValidBuffer = false;
      }
    }
  }
  else
  {
    *pResult = HB_FALSE;
  }
  return Harbour::SUCCESS;
}

// Lock a file.
static HB_ERRCODE hb_dbfLockFile(DBFAREAP pArea, HB_USHORT *pResult)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLockFile(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pResult)));
#endif

  if (!pArea->fFLocked)
  {
    if (pArea->lpdbPendingRel)
    {
      if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }

    hb_dbfUnlockAllRecords(pArea);

    SELF_RAWLOCK(&pArea->area, FILE_LOCK, 0);
    *pResult = static_cast<HB_USHORT>(pArea->fFLocked);

    if (!pArea->fPositioned)
    {
      SELF_GOTO(&pArea->area, pArea->ulRecNo);
    }
    else if (!pArea->fRecordChanged)
    {
      SELF_GOCOLD(&pArea->area);
      pArea->fValidBuffer = false;
    }
  }
  else
  {
    *pResult = HB_TRUE;
  }

  return Harbour::SUCCESS;
}

// Unlock a file.
static HB_ERRCODE hb_dbfUnlockFile(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfUnlockFile(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->fFLocked)
  {
    errCode = SELF_GOCOLD(&pArea->area);
    SELF_RAWLOCK(&pArea->area, FILE_UNLOCK, 0);
  }
  return errCode;
}

// Test if a record is locked.
static bool hb_dbfIsLocked(DBFAREAP pArea, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfIsLocked(%p)", static_cast<void*>(pArea)));
#endif

  HB_ULONG ulCount;

  ulCount = pArea->ulNumLocksPos;
  while (ulCount > 0)
  {
    if (pArea->pLocksPos[ulCount - 1] == ulRecNo)
    {
      return true;
    }
    ulCount--;
  }

  return false;
}

// Return an array filled all locked records.
static void hb_dbfGetLockArray(DBFAREAP pArea, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetLockArray(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pItem)));
#endif

  hb_arrayNew(pItem, pArea->ulNumLocksPos);
  for (HB_ULONG ulCount = 0; ulCount < pArea->ulNumLocksPos; ulCount++)
  {
    hb_arraySetNInt(pItem, ulCount + 1, pArea->pLocksPos[ulCount]);
  }
}

// Converts EDBF_* error code into EG_* one.
// This function is common for different DBF based RDD implementation
// so I don't make it static
HB_ERRCODE hb_dbfGetEGcode(HB_ERRCODE errCode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetEGcode(%u)", errCode));
#endif

  HB_ERRCODE errEGcode;

  switch (errCode)
  {
  case EDBF_OPEN_DBF:
    errEGcode = EG_OPEN;
    break;
  case EDBF_CREATE_DBF:
    errEGcode = EG_CREATE;
    break;
  case EDBF_READ:
    errEGcode = EG_READ;
    break;
  case EDBF_WRITE:
    errEGcode = EG_WRITE;
    break;
  case EDBF_CORRUPT:
    errEGcode = EG_CORRUPTION;
    break;
  case EDBF_DATATYPE:
    errEGcode = EG_DATATYPE;
    break;
  case EDBF_DATAWIDTH:
    errEGcode = EG_DATAWIDTH;
    break;
  case EDBF_UNLOCKED:
    errEGcode = EG_UNLOCKED;
    break;
  case EDBF_SHARED:
    errEGcode = EG_SHARED;
    break;
  case EDBF_APPENDLOCK:
    errEGcode = EG_APPENDLOCK;
    break;
  case EDBF_READONLY:
    errEGcode = EG_READONLY;
    break;
  case EDBF_LOCK:
    errEGcode = EG_LOCK;
    break;
  case EDBF_INVALIDKEY:
  default:
    errEGcode = EG_UNSUPPORTED;
    break;
  }

  return errEGcode;
}

// Converts memo block offset into ASCII.
// This function is common for different MEMO implementation
// so I left it in DBF.
HB_ULONG hb_dbfGetMemoBlock(DBFAREAP pArea, HB_USHORT uiIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetMemoBlock(%p, %hu)", static_cast<void*>(pArea), uiIndex));
#endif

  HB_ULONG ulBlock = 0;

  if (pArea->area.lpFields[uiIndex].uiLen == 4)
  {
    ulBlock = HB_GET_LE_UINT32(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);
  }
  else
  {
    for (HB_USHORT uiCount = 0; uiCount < 10; uiCount++)
    {
      HB_BYTE bByte = pArea->pRecord[pArea->pFieldOffset[uiIndex] + uiCount];
      if (bByte >= '0' && bByte <= '9')
      {
        ulBlock = ulBlock * 10 + (bByte - '0');
      }
    }
  }

  return ulBlock;
}

// Converts ASCII data into memo block offset.
// This function is common for different MEMO implementation
// so I left it in DBF.
void hb_dbfPutMemoBlock(DBFAREAP pArea, HB_USHORT uiIndex, HB_ULONG ulBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPutMemoBlock(%p, %hu, %lu)", static_cast<void*>(pArea), uiIndex, ulBlock));
#endif

  if (pArea->area.lpFields[uiIndex].uiLen == 4)
  {
    HB_PUT_LE_UINT32(&pArea->pRecord[pArea->pFieldOffset[uiIndex]], ulBlock);
  }
  else
  {
    for (HB_SHORT iCount = 9; iCount >= 0; iCount--)
    {
      if (ulBlock > 0)
      {
        pArea->pRecord[pArea->pFieldOffset[uiIndex] + iCount] = static_cast<HB_BYTE>(ulBlock % 10) + '0';
        ulBlock /= 10;
      }
      else
      {
        pArea->pRecord[pArea->pFieldOffset[uiIndex] + iCount] = ' ';
      }
    }
  }
}

// Retrive memo field information stored in DBF file
// This function is common for different MEMO implementation
// so I left it in DBF.
HB_ERRCODE hb_dbfGetMemoData(DBFAREAP pArea, HB_USHORT uiIndex, HB_ULONG *pulBlock, HB_ULONG *pulSize,
                             HB_ULONG *pulType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetMemoData(%p, %hu, %p, %p, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pulBlock), static_cast<void*>(pulSize), static_cast<void*>(pulType)));
#endif

  *pulBlock = *pulSize = *pulType = 0;

  if (uiIndex >= pArea->area.uiFieldCount || (pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::MEMO &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::IMAGE &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::BLOB &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::OLE))
  {
    return Harbour::FAILURE;
  }

  if (pArea->area.lpFields[uiIndex].uiLen == 4)
  {
    *pulBlock = HB_GET_LE_UINT32(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);
    return Harbour::SUCCESS;
  }
  else if (pArea->area.lpFields[uiIndex].uiLen == 10)
  {
    HB_ULONG ulValue;

    if (pArea->bMemoType == DB_MEMO_SMT)
    {
      LPSMTFIELD pSMTFiled = reinterpret_cast<LPSMTFIELD>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);

      ulValue = HB_GET_LE_UINT16(pSMTFiled->type);
      if (ulValue != 0x2020)
      {
        *pulType = ulValue;
        *pulSize = HB_GET_LE_UINT32(pSMTFiled->length);
        *pulBlock = HB_GET_LE_UINT32(pSMTFiled->block);
      }
    }
    else if (pArea->pRecord[pArea->pFieldOffset[uiIndex]] != 0)
    {
      // check for NULL fields created by Access, they have Chr(0) set
      // in the whole memo block address, [druzus]
      ulValue = 0;
      for (HB_USHORT uiCount = 0; uiCount < 10; uiCount++)
      {
        HB_BYTE bByte = pArea->pRecord[pArea->pFieldOffset[uiIndex] + uiCount];
        if (bByte >= '0' && bByte <= '9')
        {
          ulValue = ulValue * 10 + (bByte - '0');
        }
        else if (bByte != ' ' || ulValue)
        {
          return hb_dbfErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, pArea->szDataFileName, 0, EF_CANDEFAULT, nullptr) ==
                         E_DEFAULT
                     ? Harbour::SUCCESS
                     : Harbour::FAILURE;
        }
      }
      *pulBlock = ulValue;
    }
    return Harbour::SUCCESS;
  }

  return Harbour::FAILURE;
}

// Write memo data information into  memo field in DBF file
// This function is common for different MEMO implementation
// so I left it in DBF.
HB_ERRCODE hb_dbfSetMemoData(DBFAREAP pArea, HB_USHORT uiIndex, HB_ULONG ulBlock, HB_ULONG ulSize, HB_ULONG ulType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSetMemoData(%p, %hu, %lu, %lu, %lu)", static_cast<void*>(pArea), uiIndex, ulBlock, ulSize, ulType));
#endif

  if (uiIndex >= pArea->area.uiFieldCount || (pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::MEMO &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::IMAGE &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::BLOB &&
                                              pArea->area.lpFields[uiIndex].uiType != Harbour::DB::Field::OLE))
  {
    return Harbour::FAILURE;
  }

  if (pArea->area.lpFields[uiIndex].uiLen == 4)
  {
    HB_PUT_LE_UINT32(&pArea->pRecord[pArea->pFieldOffset[uiIndex]], ulBlock);
    return Harbour::SUCCESS;
  }
  else if (pArea->area.lpFields[uiIndex].uiLen == 10)
  {
    if (pArea->bMemoType == DB_MEMO_SMT)
    {
      LPSMTFIELD pSMTFiled = reinterpret_cast<LPSMTFIELD>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);

      HB_PUT_LE_UINT16(pSMTFiled->type, ulType);
      HB_PUT_LE_UINT32(pSMTFiled->length, ulSize);
      HB_PUT_LE_UINT32(pSMTFiled->block, ulBlock);
    }
    else
    {
      for (HB_SHORT iCount = 9; iCount >= 0; iCount--)
      {
        if (ulBlock > 0)
        {
          pArea->pRecord[pArea->pFieldOffset[uiIndex] + iCount] = static_cast<HB_BYTE>(ulBlock % 10) + '0';
          ulBlock /= 10;
        }
        else
        {
          pArea->pRecord[pArea->pFieldOffset[uiIndex] + iCount] = ' ';
        }
      }
    }
    return Harbour::SUCCESS;
  }

  return Harbour::FAILURE;
}

// Get information about locking schemes for additional files (MEMO, INDEX)
// This function is common for different MEMO implementation
// so I left it in DBF.
HB_BOOL hb_dbfLockIdxGetData(HB_BYTE bScheme, PHB_DBFLOCKDATA pLockData)
{
  pLockData->next = pLockData->tolock = 0;
  pLockData->type = 0;

  switch (bScheme)
  {
  case DB_DBFLOCK_CLIPPER:
    pLockData->offset = IDX_LOCKPOS_CLIPPER;
    pLockData->size = IDX_LOCKPOOL_CLIPPER;
    break;

  case DB_DBFLOCK_CLIPPER2:
    pLockData->offset = IDX_LOCKPOS_CLIPPER2;
    pLockData->size = IDX_LOCKPOOL_CLIPPER2;
    break;

  case DB_DBFLOCK_COMIX:
    pLockData->offset = IDX_LOCKPOS_COMIX;
    pLockData->size = IDX_LOCKPOOL_COMIX;
    break;

  case DB_DBFLOCK_VFP:
    pLockData->offset = IDX_LOCKPOS_VFP;
    pLockData->size = IDX_LOCKPOOL_VFP;
    break;

  case DB_DBFLOCK_HB32:
    pLockData->offset = IDX_LOCKPOS_HB32;
    pLockData->size = IDX_LOCKPOOL_HB32;
    break;

#ifndef HB_LONG_LONG_OFF
  case DB_DBFLOCK_HB64:
    pLockData->offset = IDX_LOCKPOS_HB64;
    pLockData->size = IDX_LOCKPOOL_HB64;
    break;
#endif

  default:
    pLockData->offset = pLockData->size = 0;
    return false;
  }
  return true;
}

static bool hb_dbfLockIdxRepeatFail(DBFAREAP pArea, PHB_DBFLOCKDATA pLockData)
{
  HB_SYMBOL_UNUSED(pArea);
  HB_SYMBOL_UNUSED(pLockData);

  // TODO: call special error handler (LOCKHANDLER) here

  return true;
}

// Set lock using current locking schemes in additional files (MEMO, INDEX)
// This function is common for different MEMO implementation
// so I left it in DBF.
HB_BOOL hb_dbfLockIdxFile(DBFAREAP pArea, PHB_FILE pFile, int iType, HB_BOOL fLateWrlck, PHB_DBFLOCKDATA pLockData)
{
  HB_FOFFSET tolock;
  auto fOK = false;

  switch (iType & FL_MASK)
  {
  case FL_LOCK:
    if (!hb_dbfLockIdxGetData(pArea->bLockType, pLockData))
    {
      return false;
    }

    if (pLockData->size && (iType & FLX_SHARED) != 0)
    {
      if (++pLockData->count >= 16)
      {
        pLockData->size = 0;
        pLockData->count = 0;
        iType &= ~FLX_SHARED;
      }
    }
    else
    {
      pLockData->count = 0;
    }

    tolock = 0;
    for (;;)
    {
      HB_FOFFSET size = 1, offset = pLockData->offset;
      if (pLockData->count != 0)
      {
        offset += static_cast<HB_FOFFSET>(hb_random_num() * pLockData->size) + 1;
      }
      else if (pLockData->size != 0)
      {
        size = pLockData->size + 1;
      }
      if (hb_fileLock(pFile, offset, size, size > 1 ? iType & ~FLX_WAIT : iType))
      {
        pLockData->offset = offset;
        pLockData->size = size;
        pLockData->tolock = tolock;
        pLockData->type = iType;
        if (!fLateWrlck && tolock != 0)
        {
          if (!hb_dbfLockIdxWrite(pArea, pFile, pLockData))
          {
            hb_fileLock(pFile, offset, size, FL_UNLOCK);
            break;
          }
        }
        return true;
      }
      if ((iType & FLX_WAIT) == 0)
      {
        break;
      }
      else if (size > 1)
      {
        tolock = size - 1;
        pLockData->size = 0;
      }
      else if (!hb_dbfLockIdxRepeatFail(pArea, pLockData))
      {
        break;
      }
      else
      {
        hb_releaseCPU();
      }
    }
    pLockData->offset = pLockData->size = pLockData->next = pLockData->tolock = 0;
    pLockData->type = 0;
    break;

  case FL_UNLOCK:
    fOK = hb_fileLock(pFile, pLockData->offset, pLockData->size, iType);
    if (pLockData->next)
    {
      if (!hb_fileLock(pFile, pLockData->offset + pLockData->size, pLockData->next, iType))
      {
        fOK = false;
      }
    }
    if (fOK)
    {
      pLockData->offset = pLockData->size = pLockData->next = pLockData->tolock = 0;
      pLockData->type = 0;
      return true;
    }
  }
  return false;
}

HB_BOOL hb_dbfLockIdxWrite(DBFAREAP pArea, PHB_FILE pFile, PHB_DBFLOCKDATA pLockData)
{
  if (pLockData->tolock)
  {
    // FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT
    while (!hb_fileLock(pFile, pLockData->offset + pLockData->size, pLockData->tolock, pLockData->type))
    {
      if (!hb_dbfLockIdxRepeatFail(pArea, pLockData))
      {
        return false;
      }
      hb_releaseCPU();
    }
    pLockData->next = pLockData->tolock;
    pLockData->tolock = 0;
  }
  return true;
}

// Get DBF locking parameters
static HB_ERRCODE hb_dbfLockData(DBFAREAP pArea, HB_FOFFSET *pnPos, HB_FOFFSET *pnFlSize, HB_FOFFSET *pnRlSize,
                                 int *iDir)
{
  switch (pArea->bLockType)
  {
  case DB_DBFLOCK_CLIPPER:
    *pnPos = DBF_LOCKPOS_CLIPPER;
    *iDir = DBF_LOCKDIR_CLIPPER;
    *pnFlSize = DBF_FLCKSIZE_CLIPPER;
    *pnRlSize = DBF_RLCKSIZE_CLIPPER;
    break;

  case DB_DBFLOCK_CLIPPER2:
    *pnPos = DBF_LOCKPOS_CLIPPER2;
    *iDir = DBF_LOCKDIR_CLIPPER2;
    *pnFlSize = DBF_FLCKSIZE_CLIPPER2;
    *pnRlSize = DBF_RLCKSIZE_CLIPPER2;
    break;

  case DB_DBFLOCK_COMIX:
    *pnPos = DBF_LOCKPOS_COMIX;
    *iDir = DBF_LOCKDIR_COMIX;
    *pnFlSize = DBF_FLCKSIZE_COMIX;
    *pnRlSize = DBF_RLCKSIZE_COMIX;
    break;

  case DB_DBFLOCK_VFP:
    if (pArea->fHasTags)
    {
      *pnPos = DBF_LOCKPOS_VFPX;
      *iDir = DBF_LOCKDIR_VFPX;
      *pnFlSize = DBF_FLCKSIZE_VFPX;
      *pnRlSize = DBF_RLCKSIZE_VFPX;
    }
    else
    {
      *pnPos = DBF_LOCKPOS_VFP;
      *iDir = DBF_LOCKDIR_VFP;
      *pnFlSize = DBF_FLCKSIZE_VFP;
      *pnRlSize = DBF_RLCKSIZE_VFP;
    }
    break;

  case DB_DBFLOCK_HB32:
    *pnPos = DBF_LOCKPOS_HB32;
    *iDir = DBF_LOCKDIR_HB32;
    *pnFlSize = DBF_FLCKSIZE_HB32;
    *pnRlSize = DBF_RLCKSIZE_HB32;
    break;

#ifndef HB_LONG_LONG_OFF
  case DB_DBFLOCK_HB64:
    *pnPos = DBF_LOCKPOS_HB64;
    *iDir = DBF_LOCKDIR_HB64;
    *pnFlSize = DBF_FLCKSIZE_HB64;
    *pnRlSize = DBF_RLCKSIZE_HB64;
    break;
#endif
  default:
    *pnPos = *pnFlSize = *pnRlSize = 0;
    *iDir = 0;
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

static int hb_dbfLockTest(DBFAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLockTest(%p, %hu, %lu)", static_cast<void*>(pArea), uiAction, ulRecNo));
#endif

  int iResult = -1;

  if (!pArea->fShared || pArea->fFLocked || (uiAction == REC_LOCK && hb_dbfIsLocked(pArea, ulRecNo)))
  {
    iResult = 0;
  }
  else
  {
    HB_FOFFSET nPos, nFlSize, nRlSize;
    int iDir;

    if (hb_dbfLockData(pArea, &nPos, &nFlSize, &nRlSize, &iDir) == Harbour::SUCCESS)
    {
      switch (uiAction)
      {
      case FILE_LOCK:
        if (iDir < 0)
        {
          nPos -= nFlSize;
        }
        else
        {
          nPos++;
        }
        iResult = hb_fileLockTest(pArea->pDataFile, nPos, nFlSize, FL_LOCK);
        break;

      case REC_LOCK:
        if (iDir < 0)
        {
          nPos -= ulRecNo;
        }
        else if (iDir == 2)
        {
          nPos += (ulRecNo - 1) * pArea->uiRecordLen + pArea->uiHeaderLen;
        }
        else
        {
          nPos += ulRecNo;
        }

        iResult = hb_fileLockTest(pArea->pDataFile, nPos, nRlSize, FL_LOCK);
        break;
      }
    }
  }

  return iResult;
}

// -- DBF METHODS --

// Determine logical beginning of file.
static HB_ERRCODE hb_dbfBof(DBFAREAP pArea, HB_BOOL *pBof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfBof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBof)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  *pBof = pArea->area.fBof;
  return Harbour::SUCCESS;
}

// Determine logical end of file.
static HB_ERRCODE hb_dbfEof(DBFAREAP pArea, HB_BOOL *pEof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfEof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pEof)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  *pEof = pArea->area.fEof;
  return Harbour::SUCCESS;
}

// Determine outcome of the last search operation.
static HB_ERRCODE hb_dbfFound(DBFAREAP pArea, HB_BOOL *pFound)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfFound(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFound)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  *pFound = pArea->area.fFound;
  return Harbour::SUCCESS;
}

// Position cursor at the last record.
static HB_ERRCODE hb_dbfGoBottom(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoBottom(%p)", static_cast<void*>(pArea)));
#endif

  if (SELF_GOCOLD(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  // Update record count
  if (pArea->fShared)
  {
    pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
  }

  pArea->area.fTop = false;
  pArea->area.fBottom = true;
  if (SELF_GOTO(&pArea->area, pArea->ulRecCount) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  return SELF_SKIPFILTER(&pArea->area, -1);
}

// Position cursor at a specific physical record.
static HB_ERRCODE hb_dbfGoTo(DBFAREAP pArea, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoTo(%p, %lu)", static_cast<void*>(pArea), ulRecNo));
#endif

  if (SELF_GOCOLD(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pArea->lpdbPendingRel)
  {
    if (pArea->lpdbPendingRel->isScoped)
    {
      if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }
    else
    { // Reset parent rel struct
      pArea->lpdbPendingRel = nullptr;
    }
  }

  // Update record count
  if (ulRecNo > pArea->ulRecCount && pArea->fShared)
  {
    pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
  }

  if (ulRecNo <= pArea->ulRecCount && ulRecNo >= 1)
  {
    pArea->ulRecNo = ulRecNo;
    pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = false;
    pArea->fPositioned = true;
  }
  else
  { // Out of space
    pArea->ulRecNo = pArea->ulRecCount + 1;
    pArea->area.fBof = pArea->area.fEof = pArea->fValidBuffer = true;
    pArea->fPositioned = pArea->fDeleted = pArea->fEncrypted = false;

    // Clear record buffer
    hb_dbfSetBlankRecord(pArea, HB_BLANK_EOF);
  }
  pArea->area.fFound = false;

  // Force relational movement in child WorkAreas
  if (pArea->area.lpdbRelations)
  {
    return SELF_SYNCCHILDREN(&pArea->area);
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

// Position the cursor to a specific, physical identity.
static HB_ERRCODE hb_dbfGoToId(DBFAREAP pArea, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoToId(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pItem)));
#endif

  if (pItem->isNumeric())
  {
    return SELF_GOTO(&pArea->area, pItem->getNL());
  }
  else
  {
    hb_dbfErrorRT(pArea, EG_DATATYPE, EDBF_DATATYPE, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
}

// Position cursor at the first record.
static HB_ERRCODE hb_dbfGoTop(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoTop(%p)", static_cast<void*>(pArea)));
#endif

  pArea->area.fTop = true;
  pArea->area.fBottom = false;

  if (SELF_GOTO(&pArea->area, 1) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  return SELF_SKIPFILTER(&pArea->area, 1);
}

#define hb_dbfSeek nullptr

// Reposition cursor relative to current position.
static HB_ERRCODE hb_dbfSkip(DBFAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSkip(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  pArea->area.fTop = pArea->area.fBottom = false;

  if (lToSkip == 0 || pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter || hb_setGetDeleted())
  {
    return SUPER_SKIP(&pArea->area, lToSkip);
  }

  HB_ERRCODE errCode = SELF_SKIPRAW(&pArea->area, lToSkip);

  // TODO: remove this hack - it's not necessary if SKIPRAW works
  // as it should, Druzus

  // Move first record and set Bof flag
  if (errCode == Harbour::SUCCESS && pArea->area.fBof && lToSkip < 0)
  {
    errCode = SELF_GOTOP(&pArea->area);
    pArea->area.fBof = true;
  }

  // Update Bof and Eof flags
  if (lToSkip < 0)
  {
    pArea->area.fEof = false;
  }
  else // if( lToSkip > 0 )
  {
    pArea->area.fBof = false;
  }

  return errCode;
}

#define hb_dbfSkipFilter nullptr

// Reposition cursor, regardless of filter.
static HB_ERRCODE hb_dbfSkipRaw(DBFAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSkipRaw(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  HB_ERRCODE errCode;

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (lToSkip == 0)
  {
    auto bBof = false;
    auto bEof = false;

    // Save flags
    bBof = pArea->area.fBof;
    bEof = pArea->area.fEof;

    errCode = SELF_GOTO(&pArea->area, pArea->ulRecNo);

    // Restore flags
    pArea->area.fBof = bBof;
    pArea->area.fEof = bEof;
  }
  else if (lToSkip < 0 && static_cast<HB_ULONG>(-lToSkip) >= pArea->ulRecNo)
  {
    errCode = SELF_GOTO(&pArea->area, 1);
    pArea->area.fBof = true;
  }
  else
  {
    errCode = SELF_GOTO(&pArea->area, pArea->ulRecNo + lToSkip);
  }

  return errCode;
}

// Add a field to the WorkArea.
static HB_ERRCODE hb_dbfAddField(DBFAREAP pArea, LPDBFIELDINFO pFieldInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfAddField(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFieldInfo)));
#endif

  switch (pFieldInfo->uiType)
  {
  case Harbour::DB::Field::IMAGE:
  case Harbour::DB::Field::BLOB:
  case Harbour::DB::Field::OLE:
    pFieldInfo->uiFlags |= HB_FF_BINARY;
    // fallthrough
  case Harbour::DB::Field::MEMO:
    if (pArea->bMemoType == DB_MEMO_SMT)
    {
      pFieldInfo->uiLen = 10;
    }
    break;
  }

  // Update field offset
  pArea->pFieldOffset[pArea->area.uiFieldCount] = pArea->uiRecordLen;
  pArea->uiRecordLen += pFieldInfo->uiLen;
  if ((pFieldInfo->uiFlags & HB_FF_UNICODE) != 0)
  {
    if (pFieldInfo->uiType == Harbour::DB::Field::STRING)
    {
      pArea->uiRecordLen += pFieldInfo->uiLen;
    }
    else if (pFieldInfo->uiType == Harbour::DB::Field::VARLENGTH)
    {
      pArea->uiRecordLen += pFieldInfo->uiLen + 2;
    }
  }
  if (pArea->pFieldOffset[pArea->area.uiFieldCount] > pArea->uiRecordLen)
  {
    return Harbour::FAILURE;
  }
  else
  {
    return SUPER_ADDFIELD(&pArea->area, pFieldInfo);
  }
}

// Append a record to the WorkArea.
static HB_ERRCODE hb_dbfAppend(DBFAREAP pArea, HB_BOOL bUnLockAll)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfAppend(%p, %d)", static_cast<void*>(pArea), static_cast<int>(bUnLockAll)));
#endif

  HB_USHORT fLocked;

  if (SELF_GOCOLD(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_APPEND, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  if (pArea->fReadonly)
  {
    hb_dbfErrorRT(pArea, EG_READONLY, EDBF_READONLY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (pArea->lpdbPendingRel)
  {
    if (pArea->lpdbPendingRel->isScoped)
    {
      if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }
    else
    { // Reset parent rel struct
      pArea->lpdbPendingRel = nullptr;
    }
  }

  if (pArea->fShared)
  {
    fLocked = false;
    if (SELF_RAWLOCK(&pArea->area, APPEND_LOCK, 0) == Harbour::SUCCESS)
    {
      HB_ULONG ulNewRecord;
      // Update RecCount
      pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
      ulNewRecord = pArea->ulRecCount + 1;
      if (pArea->fFLocked || hb_dbfIsLocked(pArea, ulNewRecord))
      {
        fLocked = true;
      }
      else if (hb_dbfLockRecord(pArea, ulNewRecord, &fLocked, bUnLockAll) != Harbour::SUCCESS)
      {
        if (fLocked)
        {
          hb_dbfUnlockRecord(pArea, ulNewRecord);
        }
        SELF_RAWLOCK(&pArea->area, APPEND_UNLOCK, 0);
        return Harbour::FAILURE;
      }
    }
    if (!fLocked)
    {
      SELF_RAWLOCK(&pArea->area, APPEND_UNLOCK, 0);
      hb_dbfErrorRT(pArea, EG_APPENDLOCK, EDBF_APPENDLOCK, nullptr, 0, EF_CANDEFAULT, nullptr);
      return Harbour::FAILURE;
    }
  }

  // Clear record buffer and update pArea
  hb_dbfSetBlankRecord(pArea, HB_BLANK_APPEND);

  pArea->fValidBuffer = pArea->fUpdateHeader = pArea->fRecordChanged = pArea->fAppend = pArea->fPositioned = true;
  pArea->ulRecCount++;
  pArea->ulRecNo = pArea->ulRecCount;
  pArea->fDeleted = pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = false;
  pArea->fEncrypted = pArea->pCryptKey != nullptr && !pArea->fHasMemo;

  if (pArea->fShared)
  {
    HB_ERRCODE errCode = SELF_GOCOLD(&pArea->area);
    SELF_RAWLOCK(&pArea->area, APPEND_UNLOCK, 0);
    return errCode;
  }
  return Harbour::SUCCESS;
}

#define hb_dbfCreateFields nullptr

// Delete a record.
static HB_ERRCODE hb_dbfDeleteRec(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfDeleteRec(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_DELETE, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  // Buffer is hot?
  if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pArea->pRecord[0] = '*';
  pArea->fDeleted = true;
  return Harbour::SUCCESS;
}

// Determine deleted status for a record.
static HB_ERRCODE hb_dbfDeleted(DBFAREAP pArea, HB_BOOL *pDeleted)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfDeleted(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pDeleted)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  *pDeleted = pArea->fDeleted;
  return Harbour::SUCCESS;
}

#define hb_dbfFieldCount nullptr
#define hb_dbfFieldDisplay nullptr
#define hb_dbfFieldName nullptr

// Write data buffer to the data store.
static HB_ERRCODE hb_dbfFlush(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfFlush(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->area);
  if (errCode == Harbour::SUCCESS)
  {
    if (pArea->fUpdateHeader && (pArea->uiSetHeader & DB_SETHEADER_COMMIT) != 0)
    {
      errCode = SELF_WRITEDBHEADER(&pArea->area);
    }
  }

  if (errCode == Harbour::SUCCESS && hb_setGetHardCommit())
  {
    if (pArea->fDataFlush)
    {
      hb_fileCommit(pArea->pDataFile);
      pArea->fDataFlush = false;
    }
    if (pArea->fHasMemo && pArea->pMemoFile && pArea->fMemoFlush)
    {
      hb_fileCommit(pArea->pMemoFile);
      pArea->fMemoFlush = false;
    }
  }

  return errCode;
}

// Retrieve current record buffer
static HB_ERRCODE hb_dbfGetRec(DBFAREAP pArea, HB_BYTE **pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetRec(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBuffer)));
#endif

  if (pBuffer != nullptr)
  {
    // Read record
    if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
    {
      return Harbour::FAILURE;
    }

    *pBuffer = pArea->pRecord;
  }
  else
  {
    if (pArea->pRecord[0] == 'D' || pArea->pRecord[0] == 'E')
    {
      pArea->fEncrypted = true;
      pArea->pRecord[0] = pArea->pRecord[0] == 'D' ? '*' : ' ';
      if (pArea->pCryptKey && pArea->bCryptType == DB_CRYPT_SIX)
      {
        hb_sxDeCrypt(reinterpret_cast<const char *>(pArea->pRecord) + 1, reinterpret_cast<char *>(pArea->pRecord) + 1,
                     pArea->pCryptKey, pArea->uiRecordLen - 1);
      }
    }
    else
    {
      pArea->fEncrypted = false;
    }
  }
  return Harbour::SUCCESS;
}

// Obtain the current value of a field.
static HB_ERRCODE hb_dbfGetValue(DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetValue(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  auto fError = false;
  char *pszVal;
  double dVal;
  HB_SIZE nLen;

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  fError = false;
  pField = pArea->area.lpFields + uiIndex;
  switch (pField->uiType)
  {
  case Harbour::DB::Field::STRING:
    nLen = pField->uiLen;
    if ((pField->uiFlags & HB_FF_UNICODE) != 0)
    {
      hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE,
                          reinterpret_cast<const HB_WCHAR *>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]), nLen);
    }
    else if ((pField->uiFlags & HB_FF_BINARY) == 0)
    {
      pszVal = hb_cdpnDup(reinterpret_cast<const char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], &nLen,
                          pArea->area.cdPage, hb_vmCDP());
      hb_itemPutCLPtr(pItem, pszVal, nLen);
    }
    else
    {
      pszVal = reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex];
      hb_itemPutCL(pItem, pszVal, nLen);
    }
    break;

  case Harbour::DB::Field::VARLENGTH:
    nLen = pField->uiLen;
    if ((pField->uiFlags & HB_FF_UNICODE) != 0)
    {
      nLen = HB_GET_LE_UINT16(&pArea->pRecord[pArea->pFieldOffset[uiIndex] + (nLen << 1)]);
      if (nLen == 0xFFFF || nLen > static_cast<HB_SIZE>(pField->uiLen))
      { // protection against corrupted files
        nLen = 0;
      }
      hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE,
                          reinterpret_cast<const HB_WCHAR *>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]), nLen);
    }
    else
    {
      if (hb_dbfGetNullFlag(pArea, pArea->pFieldBits[uiIndex].uiLengthBit))
      {
        nLen = static_cast<HB_UCHAR>(pArea->pRecord[pArea->pFieldOffset[uiIndex] + nLen - 1]);
        // protection against corrupted files
        if (nLen > static_cast<HB_SIZE>(pField->uiLen))
        {
          nLen = pField->uiLen;
        }
      }
      if ((pField->uiFlags & HB_FF_BINARY) == 0)
      {
        pszVal = hb_cdpnDup(reinterpret_cast<const char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], &nLen,
                            pArea->area.cdPage, hb_vmCDP());
      }
      else
      {
        pszVal = reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex];
      }

      hb_itemPutCLPtr(pItem, pszVal, nLen);
    }
    break;

  case Harbour::DB::Field::LOGICAL:
    hb_itemPutL(pItem, pArea->pRecord[pArea->pFieldOffset[uiIndex]] == 'T' ||
                           pArea->pRecord[pArea->pFieldOffset[uiIndex]] == 't' ||
                           pArea->pRecord[pArea->pFieldOffset[uiIndex]] == 'Y' ||
                           pArea->pRecord[pArea->pFieldOffset[uiIndex]] == 'y');
    break;

  case Harbour::DB::Field::DATE:
    if (pField->uiLen == 3)
    {
      hb_itemPutDL(pItem, HB_GET_LE_UINT24(pArea->pRecord + pArea->pFieldOffset[uiIndex]));
    }
    else if (pField->uiLen == 4)
    {
      hb_itemPutDL(pItem, HB_GET_LE_UINT32(pArea->pRecord + pArea->pFieldOffset[uiIndex]));
    }
    else
    {
      hb_itemPutDS(pItem, reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex]);
    }
    break;

  case Harbour::DB::Field::TIME:
    if (pField->uiLen == 4)
    {
      hb_itemPutTDT(pItem, 0, HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex]));
      break;
    }
    // fallthrough

  case Harbour::DB::Field::MODTIME:
  case Harbour::DB::Field::TIMESTAMP:
    hb_itemPutTDT(pItem, HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex]),
                  HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex] + 4));
    break;

  case Harbour::DB::Field::INTEGER:
  case Harbour::DB::Field::CURRENCY:
  case Harbour::DB::Field::AUTOINC:
  case Harbour::DB::Field::ROWVER:
    if (pField->uiDec)
    {
      int iLen;

      switch (pField->uiLen)
      {
      case 1:
        dVal = static_cast<HB_SCHAR>(pArea->pRecord[pArea->pFieldOffset[uiIndex]]);
        iLen = 4;
        break;
      case 2:
        dVal = HB_GET_LE_INT16(pArea->pRecord + pArea->pFieldOffset[uiIndex]);
        iLen = 6;
        break;
      case 3:
        dVal = HB_GET_LE_INT24(pArea->pRecord + pArea->pFieldOffset[uiIndex]);
        iLen = 10;
        break;
      case 4:
        dVal = HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex]);
        iLen = 10;
        break;
      case 8:
        dVal = static_cast<double>(HB_GET_LE_INT64(pArea->pRecord + pArea->pFieldOffset[uiIndex]));
        iLen = 20;
        break;
      default:
        dVal = 0;
        iLen = 0;
        fError = true;
        break;
      }
      hb_itemPutNDLen(pItem, hb_numDecConv(dVal, static_cast<int>(pField->uiDec)), iLen,
                      static_cast<int>(pField->uiDec));
    }
    else
    {
      switch (pField->uiLen)
      {
      case 1:
        hb_itemPutNILen(pItem, static_cast<HB_SCHAR>(pArea->pRecord[pArea->pFieldOffset[uiIndex]]), 4);
        break;
      case 2:
        hb_itemPutNILen(pItem, static_cast<int>(HB_GET_LE_INT16(pArea->pRecord + pArea->pFieldOffset[uiIndex])), 6);
        break;
      case 3:
        hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(HB_GET_LE_INT24(pArea->pRecord + pArea->pFieldOffset[uiIndex])),
                          10);
        break;
      case 4:
        hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex])),
                          10);
        break;
      case 8:
#ifndef HB_LONG_LONG_OFF
        hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(HB_GET_LE_INT64(pArea->pRecord + pArea->pFieldOffset[uiIndex])),
                          20);
#else
        hb_itemPutNLen(pItem, static_cast<double>(HB_GET_LE_INT64(pArea->pRecord + pArea->pFieldOffset[uiIndex])), 20,
                       0);
#endif
        break;
      default:
        fError = true;
        break;
      }
    }
    break;

  case Harbour::DB::Field::DOUBLE:
  case Harbour::DB::Field::CURDOUBLE:
    hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(pArea->pRecord + pArea->pFieldOffset[uiIndex]),
                    20 - (pField->uiDec > 0 ? (pField->uiDec + 1) : 0), static_cast<int>(pField->uiDec));
    break;

  case Harbour::DB::Field::LONG:
  {
    HB_MAXINT lVal;
    HB_BOOL fDbl;

    // dBase documentation defines maximum numeric field size as 20
    // but Clipper allows to create longer fields so I remove this
    // limit, Druzus
#if 0
         if( pField->uiLen > 20 ) {
            fError = true;
            break;
         }
#endif
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
  case Harbour::DB::Field::FLOAT:
    pszVal = reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex];
    dVal = hb_strVal(pszVal, pField->uiLen);
    nLen = pField->uiLen;
    while (--nLen && HB_ISDIGIT(pszVal[nLen]))
    {
      ;
    }
    if (nLen && (pszVal[nLen] == '+' || pszVal[nLen] == '-') && (pszVal[nLen - 1] == 'e' || pszVal[nLen - 1] == 'E'))
    {
      HB_USHORT uiLen = static_cast<HB_USHORT>(nLen);
      int iExp = 0;

      while (++uiLen < pField->uiLen)
      {
        iExp = iExp * 10 + (pszVal[uiLen] - '0');
      }
      if (pszVal[nLen] == '-')
      {
        iExp = -iExp;
      }
      dVal = hb_numExpConv(dVal, -iExp);
    }
    hb_itemPutNDLen(pItem, dVal, static_cast<int>(pField->uiLen - pField->uiDec - 1), static_cast<int>(pField->uiDec));
    break;

  case Harbour::DB::Field::ANY:
    if (pField->uiLen == 3)
    {
      hb_itemPutDL(pItem, hb_sxPtoD(reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex]));
    }
    else if (pField->uiLen == 4)
    {
      hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(HB_GET_LE_INT32(pArea->pRecord + pArea->pFieldOffset[uiIndex])),
                        10);
    }
    else
    {
      fError = true;
    }
    break;

  case Harbour::DB::Field::MEMO:
  default:
    fError = true;
    break;
  }

  // Any error?
  if (fError)
  {
    auto pError = hb_errNew();
    hb_errPutGenCode(pError, EG_DATATYPE);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_DATATYPE));
    hb_errPutOperation(pError, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
    hb_errPutSubCode(pError, EDBF_DATATYPE);
    SELF_ERROR(&pArea->area, pError);
    hb_itemRelease(pError);
    return Harbour::FAILURE;
  }

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_GET, uiIndex + 1, pItem))
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

// Obtain the length of a field value.
static HB_ERRCODE hb_dbfGetVarLen(DBFAREAP pArea, HB_USHORT uiIndex, HB_SIZE *pLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetVarLen(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pLength)));
#endif

  *pLength = pArea->area.lpFields[uiIndex - 1].uiLen;

  return Harbour::SUCCESS;
}

// Perform a write of WorkArea memory to the data store.
static HB_ERRCODE hb_dbfGoCold(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoCold(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->fRecordChanged)
  {
    if (pArea->fTrigger)
    {
      // The pending relation may move the record pointer so we should
      // disable them for trigger evaluation
      LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = nullptr;

      hb_dbfTriggerDo(pArea, EVENT_UPDATE, 0, nullptr);

      // Restore disabled pending relation
      pArea->lpdbPendingRel = lpdbPendingRel;
    }

    if (pArea->fModStamp)
    {
      hb_dbfUpdateStampFields(pArea);
    }

    // Write current record
    if (!hb_dbfWriteRecord(pArea))
    {
      errCode = Harbour::FAILURE;
    }
    else
    {
      if (pArea->uiSetHeader & DB_SETHEADER_REPLACE)
      {
        pArea->fUpdateHeader = true;
      }
      pArea->fAppend = false;
      if (pArea->fShared && pArea->fUpdateHeader && (pArea->uiSetHeader & DB_SETHEADER_WRITE) != 0)
      {
        errCode = SELF_WRITEDBHEADER(&pArea->area);
      }
    }
  }
  return errCode;
}

// Mark the WorkArea data buffer as hot.
static HB_ERRCODE hb_dbfGoHot(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGoHot(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fReadonly)
  {
    hb_dbfErrorRT(pArea, EG_READONLY, EDBF_READONLY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  else if (pArea->fShared && !pArea->fFLocked && !hb_dbfIsLocked(pArea, pArea->ulRecNo))
  {
    hb_dbfErrorRT(pArea, EG_UNLOCKED, EDBF_UNLOCKED, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  pArea->fRecordChanged = true;

  return Harbour::SUCCESS;
}

// Replace the current record.
static HB_ERRCODE hb_dbfPutRec(DBFAREAP pArea, const HB_BYTE *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPutRec(%p, %p)", static_cast<void*>(pArea), static_cast<const void*>(pBuffer)));
#endif

  if (pBuffer != nullptr)
  {
    if (pArea->lpdbPendingRel)
    {
      if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }

    if (!pArea->fPositioned)
    {
      return Harbour::SUCCESS;
    }

    if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }

    // Copy data to buffer
    memcpy(pArea->pRecord, pBuffer, pArea->uiRecordLen);

    // TODO: such operation should be forbidden
    // maybe it will be good to return Harbour::FAILURE when
    //    pArea->pRecord[0] != '*' && pArea->pRecord[0] != ' '
    if (pArea->pRecord[0] == 'D' || pArea->pRecord[0] == 'E')
    {
      if (!pArea->fHasMemo)
      {
        pArea->fEncrypted = true;
      }
      pArea->pRecord[0] = pArea->pRecord[0] == 'D' ? '*' : ' ';
    }

    pArea->fDeleted = pArea->pRecord[0] == '*';
  }
  else
  { // if( pArea->fRecordChanged )
    HB_BYTE *pRecord = pArea->pRecord;
    HB_SIZE nWritten;

    if (pArea->pCryptKey)
    {
      // This enables record encryption in update operation
      if (pArea->bCryptType == DB_CRYPT_SIX && !pArea->fHasMemo)
      {
        pArea->fEncrypted = true;
      }

      if (pArea->bCryptType == DB_CRYPT_SIX && pArea->fEncrypted)
      {
        pRecord = static_cast<HB_BYTE *>(hb_xgrab(pArea->uiRecordLen));
        pRecord[0] = pArea->fDeleted ? 'D' : 'E';
        hb_sxEnCrypt(reinterpret_cast<const char *>(pArea->pRecord) + 1, reinterpret_cast<char *>(pRecord) + 1,
                     pArea->pCryptKey, pArea->uiRecordLen - 1);
      }
    }

    // Write data to file
    nWritten =
        hb_fileWriteAt(pArea->pDataFile, pRecord, pArea->uiRecordLen,
                       static_cast<HB_FOFFSET>(pArea->uiHeaderLen) +
                           static_cast<HB_FOFFSET>(pArea->ulRecNo - 1) * static_cast<HB_FOFFSET>(pArea->uiRecordLen));
    if (pRecord != pArea->pRecord)
    {
      hb_xfree(pRecord);
    }

    if (nWritten != static_cast<HB_SIZE>(pArea->uiRecordLen))
    {
      hb_dbfErrorRT(pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName, hb_fsError(), 0, nullptr);
      return Harbour::FAILURE;
    }
  }
  return Harbour::SUCCESS;
}

// Assign a value to a field.
static HB_ERRCODE hb_dbfPutValue(DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPutValue(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  // this buffer is for varlength, date and number conversions,
  // dBase documentation defines maximum numeric field size as 20
  // but Clipper allows to create longer fields so I removed this
  // limit [druzus]
  char szBuffer[256];
  const char *pszPtr;
  HB_SIZE nSize, nLen;
  HB_BYTE *ptr;

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_PUT, uiIndex, pItem))
    {
      return Harbour::FAILURE;
    }
  }

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  // Buffer is hot?
  if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = Harbour::SUCCESS;
  pField = pArea->area.lpFields + uiIndex;
  if (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
      pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE)
  {
    errCode = EDBF_DATATYPE;
  }
  else
  {
    if (pItem->isMemo() || pItem->isString())
    {
      nLen = pField->uiLen;
      if (pField->uiType == Harbour::DB::Field::STRING)
      {
        if ((pField->uiFlags & HB_FF_UNICODE) != 0)
        {
          HB_WCHAR *pwBuffer = reinterpret_cast<HB_WCHAR *>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);
          nLen = hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, pwBuffer, nLen);
          while (nLen < static_cast<HB_SIZE>(pField->uiLen))
          {
            HB_PUT_LE_UINT16(&pwBuffer[nLen], ' ');
            ++nLen;
          }
        }
        else
        {
          pszPtr = pItem->getCPtr();
          nSize = pItem->getCLen();
          if ((pField->uiFlags & HB_FF_BINARY) == 0)
          {
            hb_cdpnDup2(pszPtr, nSize, reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], &nLen,
                        hb_vmCDP(), pArea->area.cdPage);
          }
          else
          {
            if (nLen > nSize)
            {
              nLen = nSize;
            }
            memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], pItem->getCPtr(), nLen);
          }
          if (nLen < static_cast<HB_SIZE>(pField->uiLen))
          {
            memset(pArea->pRecord + pArea->pFieldOffset[uiIndex] + nLen, ' ', pField->uiLen - nLen);
          }
        }
      }
      else if (pField->uiType == Harbour::DB::Field::VARLENGTH)
      {
        if ((pField->uiFlags & HB_FF_UNICODE) != 0)
        {
          HB_WCHAR *pwBuffer = reinterpret_cast<HB_WCHAR *>(&pArea->pRecord[pArea->pFieldOffset[uiIndex]]);
          nLen = hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, pwBuffer, nLen);
          HB_PUT_LE_UINT16(&pwBuffer[pField->uiLen], nLen);
        }
        else
        {
          pszPtr = pItem->getCPtr();
          nSize = pItem->getCLen();
          if ((pField->uiFlags & HB_FF_BINARY) == 0)
          {
            if (nLen > static_cast<HB_SIZE>(sizeof(szBuffer)))
            {
              nLen = sizeof(szBuffer);
            }
            pszPtr = hb_cdpnDup2(pszPtr, nSize, szBuffer, &nLen, hb_vmCDP(), pArea->area.cdPage);
          }
          else
          {
            if (nLen > nSize)
            {
              nLen = nSize;
            }
          }
          memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], pszPtr, nLen);

          if (nLen < static_cast<HB_SIZE>(pField->uiLen))
          {
            pArea->pRecord[pArea->pFieldOffset[uiIndex] + pField->uiLen - 1] = static_cast<HB_BYTE>(nLen);
            hb_dbfSetNullFlag(pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[uiIndex].uiLengthBit);
          }
          else
          {
            hb_dbfClearNullFlag(pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[uiIndex].uiLengthBit);
          }
        }
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (pItem->isDateTime())
    {
      if (pField->uiType == Harbour::DB::Field::DATE)
      {
        if (pField->uiLen == 3)
        {
          HB_PUT_LE_UINT24(pArea->pRecord + pArea->pFieldOffset[uiIndex], pItem->getDL());
        }
        else if (pField->uiLen == 4)
        {
          HB_PUT_LE_UINT32(pArea->pRecord + pArea->pFieldOffset[uiIndex], pItem->getDL());
        }
        else
        {
          pItem->getDS(szBuffer);
          memcpy(pArea->pRecord + pArea->pFieldOffset[uiIndex], szBuffer, 8);
        }
      }
      else if (pField->uiType == Harbour::DB::Field::TIMESTAMP || pField->uiType == Harbour::DB::Field::TIME ||
               (pField->uiType == Harbour::DB::Field::MODTIME && pArea->fTransRec))
      {
        long lDate, lTime;

        pItem->getTDT(&lDate, &lTime);
        ptr = pArea->pRecord + pArea->pFieldOffset[uiIndex];
        if (pField->uiType != Harbour::DB::Field::TIME)
        {
          HB_PUT_LE_UINT32(ptr, lDate);
          ptr += 4;
        }
        HB_PUT_LE_UINT32(ptr, lTime);
      }
      else if (pField->uiType == Harbour::DB::Field::ANY && pField->uiLen == 3)
      {
        hb_sxDtoP(reinterpret_cast<char *>(pArea->pRecord) + pArea->pFieldOffset[uiIndex], pItem->getDL());
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (pItem->isNumber())
    {
      if (pField->uiType == Harbour::DB::Field::LONG || pField->uiType == Harbour::DB::Field::FLOAT)
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
      else if (pField->uiType == Harbour::DB::Field::INTEGER ||
               (pArea->fTransRec &&
                (pField->uiType == Harbour::DB::Field::AUTOINC || pField->uiType == Harbour::DB::Field::ROWVER)))
      {
        HB_MAXINT lVal;
        int iSize;

        if (pField->uiDec || pItem->isDouble())
        {
          double dVal;
#if 0 // this version rounds double values to nearest integer
          dVal = hb_numDecConv(pItem->getND(), -static_cast<int>(pField->uiDec));
#else // this one truncates double value to integer dropping fractional part
          dVal = pItem->getND();
          if (pField->uiDec)
          {
            dVal = hb_numDecConv(dVal, -static_cast<int>(pField->uiDec));
          }
#endif
          lVal = static_cast<HB_MAXINT>(dVal);
          if (!HB_DBL_LIM_INT64(dVal))
          {
            iSize = pField->uiLen + 1;
          }
          else
#ifndef HB_LONG_LONG_OFF
            iSize = HB_LIM_INT8(lVal)
                        ? 1
                        : (HB_LIM_INT16(lVal) ? 2 : (HB_LIM_INT24(lVal) ? 3 : (HB_LIM_INT32(lVal) ? 4 : 8)));
#else
            iSize =
                HB_DBL_LIM_INT8(dVal)
                    ? 1
                    : (HB_DBL_LIM_INT16(dVal) ? 2 : (HB_DBL_LIM_INT24(dVal) ? 3 : (HB_DBL_LIM_INT32(dVal) ? 4 : 8)));
#endif
        }
        else
        {
          lVal = static_cast<HB_MAXINT>(pItem->getNInt());
#ifdef HB_LONG_LONG_OFF
          dVal = static_cast<double>(lVal);
#endif
          iSize = HB_LIM_INT8(lVal)
                      ? 1
                      : (HB_LIM_INT16(lVal) ? 2 : (HB_LIM_INT24(lVal) ? 3 : (HB_LIM_INT32(lVal) ? 4 : 8)));
        }

        if (iSize > pField->uiLen)
        {
          errCode = EDBF_DATAWIDTH;
        }
        else
        {
          switch (pField->uiLen)
          {
          case 1:
            pArea->pRecord[pArea->pFieldOffset[uiIndex]] = static_cast<signed char>(lVal);
            break;
          case 2:
            HB_PUT_LE_UINT16(pArea->pRecord + pArea->pFieldOffset[uiIndex], static_cast<HB_U16>(lVal));
            break;
          case 3:
            HB_PUT_LE_UINT24(pArea->pRecord + pArea->pFieldOffset[uiIndex], static_cast<HB_U32>(lVal));
            break;
          case 4:
            HB_PUT_LE_UINT32(pArea->pRecord + pArea->pFieldOffset[uiIndex], static_cast<HB_U32>(lVal));
            break;
          case 8:
#ifndef HB_LONG_LONG_OFF
            HB_PUT_LE_UINT64(pArea->pRecord + pArea->pFieldOffset[uiIndex], static_cast<HB_U64>(lVal));
#else
            HB_PUT_LE_UINT64(pArea->pRecord + pArea->pFieldOffset[uiIndex], dVal);
#endif
            break;
          default:
            errCode = EDBF_DATATYPE;
            break;
          }
        }
      }
      else if (pField->uiType == Harbour::DB::Field::DOUBLE)
      {
        HB_PUT_LE_DOUBLE(pArea->pRecord + pArea->pFieldOffset[uiIndex], pItem->getND());
      }
      else if (pField->uiType == Harbour::DB::Field::ANY && pField->uiLen == 4)
      {
        HB_MAXINT lVal = pItem->getNInt();
        if (pItem->isDouble() ? HB_DBL_LIM_INT32(pItem->getND()) : HB_LIM_INT32(lVal))
        {
          HB_PUT_LE_UINT32(pArea->pRecord + pArea->pFieldOffset[uiIndex], static_cast<HB_U32>(lVal));
        }
        else
        {
          errCode = EDBF_DATAWIDTH;
        }
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
    else if (pItem->isLogical())
    {
      if (pField->uiType == Harbour::DB::Field::LOGICAL)
      {
        pArea->pRecord[pArea->pFieldOffset[uiIndex]] = pItem->getL() ? 'T' : 'F';
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

  // Exit if any error
  if (errCode != Harbour::SUCCESS)
  {
    auto pError = hb_errNew();
    hb_errPutGenCode(pError, hb_dbfGetEGcode(errCode));
    hb_errPutDescription(pError, hb_langDGetErrorDesc(hb_dbfGetEGcode(errCode)));
    hb_errPutOperation(pError, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
    hb_errPutSubCode(pError, errCode);
    hb_errPutFlags(pError, EF_CANDEFAULT);
    hb_errPutArgs(pError, 1, pItem);
    errCode = SELF_ERROR(&pArea->area, pError);
    hb_itemRelease(pError);
    return errCode == E_DEFAULT ? Harbour::SUCCESS : Harbour::FAILURE;
  }
  else if ((pField->uiFlags & HB_FF_NULLABLE) != 0)
  {
    hb_dbfClearNullFlag(pArea->pRecord, pArea->uiNullOffset, pArea->pFieldBits[uiIndex].uiNullBit);
  }

  return Harbour::SUCCESS;
}

// Undelete the current record.
static HB_ERRCODE hb_dbfRecall(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRecall(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_RECALL, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  // Buffer is hot?
  if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->pRecord[0] = ' ';
  pArea->fDeleted = false;
  return Harbour::SUCCESS;
}

// Obtain number of records in WorkArea.
static HB_ERRCODE hb_dbfRecCount(DBFAREAP pArea, HB_ULONG *pRecCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRecCount(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecCount)));
#endif

  // Update record count
  if (pArea->fShared)
  {
    pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
  }

  *pRecCount = pArea->ulRecCount;
  return Harbour::SUCCESS;
}

// Obtain physical row number at current WorkArea cursor position.
static HB_ERRCODE hb_dbfRecNo(DBFAREAP pArea, HB_ULONG *pulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRecNo(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pulRecNo)));
#endif

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  *pulRecNo = pArea->ulRecNo;
  return Harbour::SUCCESS;
}

// Obtain physical row ID at current WorkArea cursor position.
static HB_ERRCODE hb_dbfRecId(DBFAREAP pArea, PHB_ITEM pRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRecId(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecNo)));
#endif

  HB_ULONG ulRecNo = 0;

  HB_ERRCODE errCode = SELF_RECNO(&pArea->area, &ulRecNo);

#ifdef HB_CLP_STRICT
  // this is for strict Clipper compatibility but IMHO Clipper should not
  // do that and always set fixed size independent to the record number
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

// Establish the extent of the array of fields for a WorkArea.
static HB_ERRCODE hb_dbfSetFieldExtent(DBFAREAP pArea, HB_USHORT uiFieldExtent)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSetFieldExtent(%p, %hu)", static_cast<void*>(pArea), uiFieldExtent));
#endif

  if (SUPER_SETFIELDEXTENT(&pArea->area, uiFieldExtent) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  // Alloc field offsets array
  if (uiFieldExtent)
  {
    pArea->pFieldOffset = static_cast<HB_USHORT *>(hb_xgrabz(uiFieldExtent * sizeof(HB_USHORT)));
  }

  return Harbour::SUCCESS;
}

#define hb_dbfAlias nullptr

// Close the table in the WorkArea.
static HB_ERRCODE hb_dbfClose(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfClose(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_PRECLOSE, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  // Reset parent rel struct
  pArea->lpdbPendingRel = nullptr;

  // Update record and unlock records
  if (pArea->pDataFile)
  {
    // update buffers
    SELF_GOCOLD(&pArea->area);

    // Unlock all records
    SELF_UNLOCK(&pArea->area, nullptr);

    // Update header
    if (pArea->fUpdateHeader)
    {
      pArea->uiSetHeader |= DB_SETHEADER_EOL;
      SELF_WRITEDBHEADER(&pArea->area);
    }

    // It's not Clipper compatible but it reduces the problem with
    // buggy Windows network setting
    if (hb_setGetHardCommit())
    {
      SELF_FLUSH(&pArea->area);
    }
  }

  SUPER_CLOSE(&pArea->area);

  if (pArea->pDataFile)
  {
    hb_fileClose(pArea->pDataFile);
    pArea->pDataFile = nullptr;

    if (pArea->fTemporary)
    {
      hb_fileDelete(pArea->szDataFileName);
    }
  }

  // Close the memo file
  if (pArea->fHasMemo && pArea->pMemoFile)
  {
    hb_fileClose(pArea->pMemoFile);
    pArea->pMemoFile = nullptr;

    if (pArea->fTemporary)
    {
      hb_fileDelete(pArea->szMemoFileName);
    }
  }

  pArea->fTemporary = false;

  // Free field offset array
  if (pArea->pFieldOffset)
  {
    hb_xfree(pArea->pFieldOffset);
    pArea->pFieldOffset = nullptr;
  }

  // Free field bits array
  if (pArea->pFieldBits)
  {
    hb_xfree(pArea->pFieldBits);
    pArea->pFieldBits = nullptr;
  }

  // Free buffer
  if (pArea->pRecord)
  {
    hb_xfree(pArea->pRecord);
    pArea->pRecord = nullptr;
  }

  // Free encryption password key
  if (pArea->pCryptKey)
  {
    memset(pArea->pCryptKey, '\0', 8);
    hb_xfree(pArea->pCryptKey);
    pArea->pCryptKey = nullptr;
  }

  // Free all filenames
  if (pArea->szDataFileName)
  {
    hb_xfree(pArea->szDataFileName);
    pArea->szDataFileName = nullptr;
  }
  if (pArea->szMemoFileName)
  {
    hb_xfree(pArea->szMemoFileName);
    pArea->szMemoFileName = nullptr;
  }

  if (pArea->fTrigger)
  {
    hb_dbfTriggerDo(pArea, EVENT_POSTCLOSE, 0, nullptr);
    pArea->fTrigger = false;
  }

  return Harbour::SUCCESS;
}

// Create a data store in the specified WorkArea.
static HB_ERRCODE hb_dbfCreate(DBFAREAP pArea, LPDBOPENINFO pCreateInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfCreate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pCreateInfo)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS, errSubCode = 0;
  HB_SIZE nSize;
  HB_USHORT uiCount, uiLen;
  auto fRawBlob = false;
  DBFFIELD *pThisField;
  PHB_FNAME pFileName;
  PHB_ITEM pItem = nullptr, pError;
  char szFileName[HB_PATH_MAX];

  pArea->lpdbOpenInfo = pCreateInfo;

  if (!pArea->fTemporary)
  {
    pFileName = hb_fsFNameSplit(pCreateInfo->abName);

    if (!pFileName->szExtension && hb_setGetDefExtension())
    {
      pItem = hb_itemPutNil(pItem);
      if (SELF_INFO(&pArea->area, DBI_TABLEEXT, pItem) != Harbour::SUCCESS)
      {
        hb_itemRelease(pItem);
        hb_xfree(pFileName);
        pArea->lpdbOpenInfo = nullptr;
        return Harbour::FAILURE;
      }
      pFileName->szExtension = hb_itemGetCPtr(pItem);
      hb_fsFNameMerge(szFileName, pFileName);
    }
    else
    {
      hb_strncpy(szFileName, pCreateInfo->abName, sizeof(szFileName) - 1);
    }
    hb_xfree(pFileName);
  }

  pItem = hb_itemPutL(pItem, false);
  fRawBlob = SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_BLOB_SUPPORT, pCreateInfo->ulConnection, pItem) ==
                 Harbour::SUCCESS &&
             hb_itemGetL(pItem);

  if (pArea->bLockType == 0)
  {
    pItem = hb_itemPutNil(pItem);
    if (SELF_INFO(&pArea->area, DBI_LOCKSCHEME, pItem) != Harbour::SUCCESS)
    {
      hb_itemRelease(pItem);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    pArea->bLockType = static_cast<HB_BYTE>(hb_itemGetNI(pItem));
    if (pArea->bLockType == 0)
    {
      pArea->bLockType = DB_DBFLOCK_CLIPPER;
    }
  }

  if (pArea->bTableType == DB_DBF_VFP && !fRawBlob)
  {
    pArea->bMemoType = DB_MEMO_FPT;
  }
  else if (pArea->bMemoType == 0)
  {
    // get memo type
    pItem = hb_itemPutNil(pItem);
    if (SELF_INFO(&pArea->area, DBI_MEMOTYPE, pItem) != Harbour::SUCCESS)
    {
      hb_itemRelease(pItem);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    pArea->bMemoType = static_cast<HB_BYTE>(hb_itemGetNI(pItem));
  }

  pArea->bCryptType = DB_CRYPT_NONE;

  if (pItem != nullptr)
  {
    hb_itemRelease(pItem);
  }

  nSize =
      static_cast<HB_SIZE>(pArea->area.uiFieldCount) * sizeof(DBFFIELD) + (pArea->bTableType == DB_DBF_VFP ? 264 : 2);
  if (nSize + sizeof(DBFHEADER) > UINT16_MAX)
  {
    hb_dbfErrorRT(pArea, EG_CREATE, EDBF_DATAWIDTH, pCreateInfo->abName, 0, 0, nullptr);
    pArea->lpdbOpenInfo = nullptr;
    return Harbour::FAILURE;
  }

  if (!fRawBlob)
  {
    pError = nullptr;
    // Try create
    do
    {
      if (pArea->fTemporary)
      {
        pArea->pDataFile = hb_fileCreateTempEx(szFileName, nullptr, nullptr, nullptr, FC_NORMAL);
      }
      else
      {
        pArea->pDataFile = hb_fileExtOpen(szFileName, nullptr,
                                          FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE | FXO_DEFAULTS | FXO_SHARELOCK |
                                              FXO_COPYNAME | FXO_NOSEEKPOS,
                                          nullptr, pError);
      }
      if (pArea->pDataFile)
      {
        break;
      }
    } while (hb_dbfErrorRT(pArea, EG_CREATE, EDBF_CREATE_DBF, szFileName, hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                           &pError) == E_RETRY);
    if (pError)
    {
      hb_itemRelease(pError);
    }

    if (!pArea->pDataFile)
    {
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
  }

  pArea->szDataFileName = hb_strdup(szFileName);

  auto pBuffer = static_cast<HB_BYTE *>(hb_xgrabz(nSize + sizeof(DBFFIELD) + 1));
  pThisField = reinterpret_cast<DBFFIELD *>(pBuffer);

  pArea->fHasMemo = false;

  // Size for deleted flag
  pArea->uiRecordLen = 1;
  pArea->uiNullCount = 0;
  for (uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++)
  {
    LPFIELD pField = pArea->area.lpFields + uiCount;
    hb_strncpy(reinterpret_cast<char *>(pThisField->bName), hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)),
               sizeof(pThisField->bName) - 1);
    pArea->pFieldOffset[uiCount] = pArea->uiRecordLen;
    // field offset
    if (pArea->bTableType == DB_DBF_VFP)
    {
      HB_PUT_LE_UINT16(pThisField->bReserved1, pArea->uiRecordLen);
    }
    pThisField->bFieldFlags =
        static_cast<HB_BYTE>(pField->uiFlags) & (HB_FF_HIDDEN | HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_AUTOINC);
    switch (pField->uiType)
    {
    case Harbour::DB::Field::STRING:
      if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        pThisField->bType = '\x1A';
        if (pField->uiLen > 32767)
        {
          pField->uiLen = 32767;
        }
        uiLen = (pField->uiLen << 1);
      }
      else
      {
        pThisField->bType = 'C';
        uiLen = pField->uiLen;
      }
      pThisField->bLen = static_cast<HB_BYTE>(uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(uiLen >> 8);
      pArea->uiRecordLen += uiLen;
      break;

    case Harbour::DB::Field::LOGICAL:
      pThisField->bType = 'L';
      pThisField->bLen = 1;
      pArea->uiRecordLen++;
      break;

    case Harbour::DB::Field::MEMO:
      pThisField->bType = (pField->uiFlags & HB_FF_UNICODE) ? '\x1C' : 'M';
      if (pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT)
      {
        pField->uiLen = 10;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pArea->uiRecordLen += pField->uiLen;
      pArea->fHasMemo = true;
      break;

    case Harbour::DB::Field::BLOB:
      pThisField->bType = 'W';
      if (pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT)
      {
        pField->uiLen = 10;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      pArea->fHasMemo = true;
      break;

    case Harbour::DB::Field::IMAGE:
      pThisField->bType = 'P';
      if (pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT)
      {
        pField->uiLen = 10;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      pArea->fHasMemo = true;
      break;

    case Harbour::DB::Field::OLE:
      pThisField->bType = 'G';
      if (pField->uiLen != 4 || pArea->bMemoType == DB_MEMO_SMT)
      {
        pField->uiLen = 10;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      pArea->fHasMemo = true;
      break;

    case Harbour::DB::Field::ANY:
      if (pArea->bTableType == DB_DBF_VFP)
      {
        errSubCode = EDBF_DATATYPE;
      }
      else
      {
        pThisField->bType = 'V';
        if (pField->uiLen < 3 || pField->uiLen == 5)
        {
          pField->uiLen = 6;
        }
        pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
        pThisField->bDec = static_cast<HB_BYTE>(pField->uiLen >> 8);
        pArea->uiRecordLen += pField->uiLen;
        if (pThisField->bLen >= 6)
        {
          pArea->uiMemoVersion = DB_MEMOVER_SIX;
          pArea->fHasMemo = true;
        }
      }
      break;

    case Harbour::DB::Field::DATE:
      pThisField->bType = 'D';
      if (pField->uiLen == 3 || pField->uiLen == 4)
      {
        pThisField->bFieldFlags |= HB_FF_BINARY;
      }
      else
      {
        pField->uiLen = pThisField->bLen = 8;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::LONG:
      pThisField->bType = 'N';
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(pField->uiDec);
      if ((pField->uiFlags & HB_FF_AUTOINC) != 0)
      {
        hb_dbfNextValueInit(pThisField, pField);
      }
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::FLOAT:
      pThisField->bType = 'F';
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(pField->uiDec);
      if ((pField->uiFlags & HB_FF_AUTOINC) != 0)
      {
        hb_dbfNextValueInit(pThisField, pField);
      }
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::DOUBLE:
    case Harbour::DB::Field::CURDOUBLE:
      pThisField->bType = 'B';
      pField->uiLen = 8;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(pField->uiDec);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      if ((pField->uiFlags & HB_FF_AUTOINC) != 0)
      {
        hb_dbfNextValueInit(pThisField, pField);
      }
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::INTEGER:
    case Harbour::DB::Field::CURRENCY:
      pThisField->bType = (pArea->bTableType == DB_DBF_VFP && pField->uiLen == 8 && pField->uiDec == 4) ? 'Y' : 'I';
      if ((pField->uiLen > 4 && pField->uiLen != 8) || pField->uiLen == 0)
      {
        pField->uiLen = 4;
      }
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(pField->uiDec);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      if ((pField->uiFlags & HB_FF_AUTOINC) != 0)
      {
        hb_dbfNextValueInit(pThisField, pField);
      }
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::VARLENGTH:
      if (pField->uiLen == 0)
      {
        pField->uiLen = 1;
      }
      if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        if (pField->uiLen > 32766)
        {
          pField->uiLen = 32766;
        }
        pThisField->bType = '\x1B';
        uiLen = (pField->uiLen + 1) << 1;
      }
      else
      {
        if (pField->uiLen > 255)
        {
          pField->uiLen = 255;
        }
        if (pArea->bTableType == DB_DBF_VFP && (pField->uiFlags & HB_FF_BINARY) == 0)
        {
          pThisField->bType = 'V';
        }
        else
        {
          pThisField->bType = 'Q';
        }
        uiLen = pField->uiLen;
      }
      pThisField->bLen = static_cast<HB_BYTE>(uiLen);
      pThisField->bDec = static_cast<HB_BYTE>(uiLen >> 8);
      pArea->uiRecordLen += uiLen;
      hb_dbfAllocNullFlag(pArea, uiCount, true);
      break;

    case Harbour::DB::Field::TIME:
      pThisField->bType = 'T';
      pField->uiLen = 4;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::TIMESTAMP:
      pThisField->bType = pArea->bTableType == DB_DBF_VFP ? 'T' : '@';
      pField->uiLen = 8;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      break;

    case Harbour::DB::Field::MODTIME:
      pThisField->bType = '=';
      pField->uiLen = 8;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      pArea->uiRecordLen += pField->uiLen;
      pArea->fModStamp = true;
      break;

    case Harbour::DB::Field::ROWVER:
      pThisField->bType = '^';
      pField->uiLen = 8;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
#if 0
            HB_PUT_LE_UINT64(pThisField->bReserved2, 0);
#endif
      pArea->uiRecordLen += pField->uiLen;
      pArea->fModStamp = true;
      break;

    case Harbour::DB::Field::AUTOINC:
      pThisField->bType = '+';
      pField->uiLen = 4;
      pThisField->bLen = static_cast<HB_BYTE>(pField->uiLen);
      pThisField->bFieldFlags |= HB_FF_BINARY;
      hb_dbfNextValueInit(pThisField, pField);
      pArea->uiRecordLen += pField->uiLen;
      pArea->fAutoInc = true;
      break;

    default:
      errSubCode = EDBF_DATATYPE;
    }

    if (pArea->pFieldOffset[uiCount] > pArea->uiRecordLen)
    {
      errSubCode = EDBF_DATAWIDTH;
    }
    if (errSubCode != 0)
    {
      break;
    }

    if ((pField->uiFlags & HB_FF_NULLABLE) != 0)
    {
      hb_dbfAllocNullFlag(pArea, uiCount, false);
    }

    pThisField++;
  }

  if (errSubCode == 0 && pArea->uiNullCount)
  {
    hb_strncpy(reinterpret_cast<char *>(pThisField->bName), "_NullFlags", sizeof(pThisField->bName) - 1);
    HB_PUT_LE_UINT16(pThisField->bReserved1, pArea->uiRecordLen);
    pThisField->bType = '0';
    pThisField->bFieldFlags = HB_FF_HIDDEN;
    uiCount = (pArea->uiNullCount + 7) >> 3;
    pThisField->bLen = static_cast<HB_BYTE>(uiCount);
    pThisField->bDec = static_cast<HB_BYTE>(uiCount >> 8);
    pArea->uiNullOffset = pArea->uiRecordLen;
    pArea->uiRecordLen += uiCount;
    nSize += sizeof(DBFFIELD);
    pThisField++;
    if (nSize + sizeof(DBFHEADER) > UINT16_MAX || pArea->uiNullOffset > pArea->uiRecordLen)
    {
      errSubCode = EDBF_DATAWIDTH;
    }
  }

  if (errSubCode != 0)
  {
    hb_xfree(pBuffer);
    SELF_CLOSE(&pArea->area);
    hb_dbfErrorRT(pArea, EG_CREATE, errSubCode, pCreateInfo->abName, 0, 0, nullptr);
    pArea->lpdbOpenInfo = nullptr;
    return Harbour::FAILURE;
  }

  // set end of fields marker
  pThisField->bName[0] = '\r';

  pArea->fShared = false;   // pCreateInfo->fShared
  pArea->fReadonly = false; // pCreateInfo->fReadonly
  pArea->ulRecCount = 0;
  pArea->uiHeaderLen = static_cast<HB_USHORT>(sizeof(DBFHEADER) + nSize);
  if (fRawBlob)
  {
    pArea->fHasMemo = true;
  }
  if (!pArea->fHasMemo)
  {
    pArea->bMemoType = DB_MEMO_NONE;
  }
  pArea->ulMemoBlockSize = 0;

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

  pItem = hb_itemNew(nullptr);
  if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_PENDINGPASSWORD, pCreateInfo->ulConnection, pItem) ==
      Harbour::SUCCESS)
  {
    if (hb_dbfPasswordSet(pArea, pItem, false))
    {
      pArea->fTableEncrypted = true;
    }
  }
  else
  {
    hb_itemClear(pItem);
    if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_PASSWORD, pCreateInfo->ulConnection, pItem) == Harbour::SUCCESS)
    {
      if (hb_dbfPasswordSet(pArea, pItem, false))
      {
        pArea->fTableEncrypted = true;
      }
    }
  }
  hb_itemRelease(pItem);

  if (!fRawBlob)
  {
    // Write header
    errCode = SELF_WRITEDBHEADER(&pArea->area);
    if (errCode != Harbour::SUCCESS)
    {
      hb_xfree(pBuffer);
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return errCode;
    }

    // Write fields and eof mark
    pBuffer[nSize] = '\032';
    if (hb_fileWriteAt(pArea->pDataFile, pBuffer, nSize + 1, sizeof(DBFHEADER)) != nSize + 1)
    {
      hb_xfree(pBuffer);
      hb_dbfErrorRT(pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName, hb_fsError(), 0, nullptr);
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    pArea->fDataFlush = true;
  }
  hb_xfree(pBuffer);

  // Create memo file
  if (pArea->fHasMemo)
  {
    pFileName = hb_fsFNameSplit(szFileName);
    pFileName->szExtension = nullptr;
    hb_fsFNameMerge(szFileName, pFileName);
    hb_xfree(pFileName);
    pCreateInfo->abName = szFileName;
    errCode = SELF_CREATEMEMFILE(&pArea->area, pCreateInfo);
  }
  // If successful call SUPER_CREATE to finish system jobs
  if (errCode == Harbour::SUCCESS)
  {
    errCode = SUPER_CREATE(&pArea->area, pCreateInfo);
  }

  if (errCode != Harbour::SUCCESS)
  {
    SELF_CLOSE(&pArea->area);
    pArea->lpdbOpenInfo = nullptr;
    return errCode;
  }

  // Alloc buffer
  pArea->pRecord = static_cast<HB_BYTE *>(hb_xgrab(pArea->uiRecordLen));
  pArea->fValidBuffer = false;

  // Update the number of record for corrupted headers
  pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
  pArea->lpdbOpenInfo = nullptr;

  // Position cursor at the first record
  return SELF_GOTOP(&pArea->area);
}

// Retrieve information about the current driver.
static HB_ERRCODE hb_dbfInfo(DBFAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  switch (uiIndex)
  {
  case DBI_ISDBF:
  case DBI_CANPUTREC:
    hb_itemPutL(pItem, true);
    break;

  case DBI_GETHEADERSIZE:
    hb_itemPutNL(pItem, pArea->uiHeaderLen);
    break;

  case DBI_LASTUPDATE:
    hb_itemPutD(pItem,
                pArea->dbfHeader.bYear > 99 ? 1900 + pArea->dbfHeader.bYear : hb_setUpdateEpoch(pArea->dbfHeader.bYear),
                pArea->dbfHeader.bMonth, pArea->dbfHeader.bDay);
    break;

  case DBI_GETRECSIZE:
    hb_itemPutNL(pItem, pArea->uiRecordLen);
    break;

  case DBI_GETLOCKARRAY:
    hb_dbfGetLockArray(pArea, pItem);
    break;

  case DBI_TABLEEXT:
    hb_itemClear(pItem);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_TABLEEXT, 0, pItem);

  case DBI_FULLPATH:
    hb_itemPutC(pItem, pArea->szDataFileName);
    break;

  case DBI_MEMOTYPE:
    hb_itemPutNI(pItem, DB_MEMO_NONE);
    break;

  case DBI_TABLETYPE:
    if (!pArea->pDataFile)
    {
      hb_itemClear(pItem);
      return SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_TABLETYPE, 0, pItem);
    }
    hb_itemPutNI(pItem, pArea->bTableType);
    break;

  case DBI_FILEHANDLE:
    hb_itemPutNInt(pItem, !pArea->pDataFile
                              ? FS_ERROR
                              : static_cast<HB_MAXINT>(static_cast<HB_NHANDLE>(hb_fileHandle(pArea->pDataFile))));
    break;

  case DBI_MEMOHANDLE:
    hb_itemPutNInt(pItem, !pArea->pMemoFile
                              ? FS_ERROR
                              : static_cast<HB_MAXINT>(static_cast<HB_NHANDLE>(hb_fileHandle(pArea->pMemoFile))));
    break;

  case DBI_TRANSREC:
  {
    bool fTransRec = pArea->fTransRec;

    if (pItem->isLogical())
    {
      pArea->fTransRec = pItem->getL();
    }
    else if (pItem->isPointer())
    {
      LPDBTRANSINFO lpdbTransInfo = hb_dbTransInfoGet(pItem);

      if (lpdbTransInfo)
      {
        if (pArea->fShared && pArea->fFLocked)
        {
          pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
        }

        hb_dbfTransCheckCounters(lpdbTransInfo);
        pArea->fTransRec = true;
      }
    }
    hb_itemPutL(pItem, fTransRec);
    break;
  }
  case DBI_SHARED:
  {
    bool fShared = pArea->fShared;

    if (pItem->isLogical())
    {
      pArea->fShared = pItem->getL();
    }
    hb_itemPutL(pItem, fShared);
    break;
  }
  case DBI_ISFLOCK:
    hb_itemPutL(pItem, pArea->fFLocked);
    break;

  case DBI_ISREADONLY:
    hb_itemPutL(pItem, pArea->fReadonly);
    break;

  case DBI_ISTEMPORARY:
    if (!pArea->pDataFile && !pArea->pMemoFile && pItem->isLogical())
    {
      pArea->fTemporary = pItem->getL();
    }
    else
    {
      hb_itemPutL(pItem, pArea->fTemporary);
    }
    break;

  case DBI_VALIDBUFFER:
    hb_itemPutL(pItem, pArea->fValidBuffer);
    break;

  case DBI_POSITIONED:
    hb_itemPutL(pItem, pArea->fPositioned);
    break;

  case DBI_ISENCRYPTED:
    hb_itemPutL(pItem, pArea->fTableEncrypted);
    break;

  case DBI_DECRYPT:
    hb_dbfTableCrypt(pArea, pItem, false);
    hb_itemPutL(pItem, !pArea->fTableEncrypted);
    break;

  case DBI_ENCRYPT:
    hb_dbfTableCrypt(pArea, pItem, true);
    hb_itemPutL(pItem, pArea->fTableEncrypted);
    break;

  case DBI_LOCKCOUNT:
    hb_itemPutNL(pItem, pArea->ulNumLocksPos);
    break;

  case DBI_LOCKOFFSET:
  {
    HB_FOFFSET nPos, nFlSize, nRlSize;
    int iDir;

    hb_dbfLockData(pArea, &nPos, &nFlSize, &nRlSize, &iDir);
    hb_itemPutNInt(pItem, nPos);
    break;
  }

  case DBI_LOCKTEST:
    if (pItem->isNumeric())
    {
      hb_itemPutNI(pItem, hb_dbfLockTest(pArea, REC_LOCK, pItem->getNL()));
    }
    else
    {
      hb_itemPutNI(pItem, hb_dbfLockTest(pArea, FILE_LOCK, 0));
    }
    break;

  case DBI_LOCKSCHEME:
  {
    int iScheme = hb_itemGetNI(pItem);
    if (pArea->bLockType)
    {
      hb_itemPutNI(pItem, pArea->bLockType);
    }
    else
    {
      hb_itemClear(pItem);
      errCode = SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_LOCKSCHEME, 0, pItem);
    }
    switch (iScheme)
    {
    case DB_DBFLOCK_CLIPPER:
    case DB_DBFLOCK_CLIPPER2:
    case DB_DBFLOCK_COMIX:
    case DB_DBFLOCK_VFP:
    case DB_DBFLOCK_HB32:
#ifndef HB_LONG_LONG_OFF
    case DB_DBFLOCK_HB64:
#endif
      pArea->bLockType = static_cast<HB_BYTE>(iScheme);
    }
    break;
  }
  case DBI_SETHEADER:
  {
    HB_UINT uiSetHeader = pArea->uiSetHeader;

    if (pItem->isNumeric())
    {
      int iMode = pItem->getNI();
      if ((iMode & ~DB_SETHEADER_MASK) == 0)
      {
        pArea->uiSetHeader = iMode;
      }
    }
    hb_itemPutNI(pItem, uiSetHeader);
    break;
  }
  case DBI_ROLLBACK:
    if (pArea->fRecordChanged)
    {
      if (pArea->fAppend)
      {
        hb_dbfSetBlankRecord(pArea, HB_BLANK_ROLLBACK);
        pArea->fDeleted = false;
      }
      else
      {
        pArea->fRecordChanged = pArea->fValidBuffer = false;
      }
    }
    break;

  case DBI_PASSWORD:
    hb_dbfPasswordSet(pArea, pItem, false);
    break;

  case DBI_TRIGGER:
    if (pItem->isLogical())
    {
      pArea->fTrigger = pArea->pTriggerSym && pItem->getL();
    }
    else
    {
      PHB_DYNS pTriggerSym = pArea->pTriggerSym;
      if (pItem->isString())
      {
        hb_dbfTriggerSet(pArea, pItem);
      }
      hb_itemPutC(pItem, pTriggerSym ? hb_dynsymName(pTriggerSym) : nullptr);
    }
    break;

  case DBI_OPENINFO:
    hb_itemPutPtr(pItem, pArea->lpdbOpenInfo);
    break;

  case DBI_DIRTYREAD:
  {
    bool fDirty = HB_DIRTYREAD(pArea);

    if (pItem->isLogical())
    {
      pArea->uiDirtyRead = pItem->getL() ? HB_IDXREAD_DIRTY : HB_IDXREAD_CLEAN;
    }
    else if (!pItem->isNil())
    {
      pArea->uiDirtyRead = HB_IDXREAD_DEFAULT;
    }

    hb_itemPutL(pItem, fDirty);
    break;
  }
  case DBI_DB_VERSION:
  case DBI_RDD_VERSION:
  {
    char szBuf[64];
    int iSub = hb_itemGetNI(pItem);

    if (iSub == 1)
    {
      hb_snprintf(szBuf, sizeof(szBuf), "%d.%d (%s)", 0, 1, "DBF");
    }
    else if (iSub == 2)
    {
      hb_snprintf(szBuf, sizeof(szBuf), "%d.%d (%s:%d)", 0, 1, "DBF", pArea->area.rddID);
    }
    // hb_snprintf(szBuf, sizeof(szBuf), "%d.%d (%s:%d)", 0, 1, pArea->pRddNode->szName, pArea->area.rddID);
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

  return errCode;
}

static HB_ERRCODE hb_dbfFieldInfo(DBFAREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfFieldInfo(%p, %hu, %hu, %p)", static_cast<void*>(pArea), uiIndex, uiType, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  auto fLck = false;

  if (uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  switch (uiType)
  {
  case DBS_ISNULL:
    pField = pArea->area.lpFields + uiIndex - 1;
    hb_itemPutL(pItem, (pField->uiFlags & HB_FF_NULLABLE) != 0 &&
                           hb_dbfGetNullFlag(pArea, pArea->pFieldBits[uiIndex - 1].uiNullBit));
    return Harbour::SUCCESS;
  case DBS_COUNTER:
    if (hb_dbfIsAutoIncField(pArea->area.lpFields + uiIndex - 1) != HB_AUTOINC_NONE)
    {
      HB_MAXINT nValue;
      fLck = false;
      if (pArea->fShared && !pArea->fFLocked && !pArea->fHeaderLocked)
      {
        if (SELF_RAWLOCK(&pArea->area, HEADER_LOCK, 0) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }
        fLck = true;
      }
      if (pItem->isNumeric())
      {
        nValue = hb_dbfNextValueSet(pArea, uiIndex - 1, pItem->getNInt());
      }
      else
      {
        nValue = hb_dbfNextValueGet(pArea, uiIndex - 1, false);
      }

      if (fLck)
      {
        SELF_RAWLOCK(&pArea->area, HEADER_UNLOCK, 0);
      }
      hb_itemPutNInt(pItem, nValue);
      return Harbour::SUCCESS;
    }
    hb_itemClear(pItem);
    return Harbour::FAILURE;
  case DBS_STEP:
    if (hb_dbfIsAutoIncField(pArea->area.lpFields + uiIndex - 1) != HB_AUTOINC_NONE)
    {
      int iValue;
      if (pItem->isNumeric())
      {
        fLck = false;
        if (pArea->fShared && !pArea->fFLocked && !pArea->fHeaderLocked)
        {
          if (SELF_RAWLOCK(&pArea->area, HEADER_LOCK, 0) != Harbour::SUCCESS)
          {
            return Harbour::FAILURE;
          }
          fLck = true;
        }
        iValue = hb_dbfNextValueStep(pArea, uiIndex - 1, pItem->getNI());
        if (fLck)
        {
          SELF_RAWLOCK(&pArea->area, HEADER_UNLOCK, 0);
        }
      }
      else
      {
        iValue = hb_dbfNextValueStep(pArea, uiIndex - 1, 0);
      }
      hb_itemPutNI(pItem, iValue);
      return Harbour::SUCCESS;
    }
    hb_itemClear(pItem);
    return Harbour::FAILURE;
  default:
    return SUPER_FIELDINFO(&pArea->area, uiIndex, uiType, pItem);
  }
}

// Retrieve information about a raw
static HB_ERRCODE hb_dbfRecInfo(DBFAREAP pArea, PHB_ITEM pRecID, HB_USHORT uiInfoType, PHB_ITEM pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRecInfo(%p, %p, %hu, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecID), uiInfoType, static_cast<void*>(pInfo)));
#endif

  HB_ULONG ulRecNo = hb_itemGetNL(pRecID), ulPrevRec = 0;
  HB_ERRCODE errResult = Harbour::SUCCESS;
  HB_BOOL bDeleted;

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (ulRecNo == 0)
  {
    ulRecNo = pArea->ulRecNo;
  }
  else if (ulRecNo != pArea->ulRecNo)
  {
    switch (uiInfoType)
    {
    case DBRI_DELETED:
    case DBRI_ENCRYPTED:
    case DBRI_RAWRECORD:
    case DBRI_RAWMEMOS:
    case DBRI_RAWDATA:
      ulPrevRec = pArea->ulRecNo;
      errResult = SELF_GOTO(&pArea->area, ulRecNo);
      if (errResult != Harbour::SUCCESS)
      {
        return errResult;
      }
      break;
    }
  }

  switch (uiInfoType)
  {
  case DBRI_DELETED:
    errResult = SELF_DELETED(&pArea->area, &bDeleted);
    if (errResult == Harbour::SUCCESS)
    {
      hb_itemPutL(pInfo, bDeleted);
    }
    break;

  case DBRI_LOCKED:
    // Clipper also checks only fShared and RLOCK and ignore FLOCK
    hb_itemPutL(pInfo, !pArea->fShared || /* pArea->fFLocked || */ hb_dbfIsLocked(pArea, ulRecNo));
    break;

  case DBRI_RECSIZE:
    hb_itemPutNL(pInfo, pArea->uiRecordLen);
    break;

  case DBRI_RECNO:
    hb_itemPutNInt(pInfo, ulRecNo);
    break;

  case DBRI_UPDATED:
    hb_itemPutL(pInfo, ulRecNo == pArea->ulRecNo && pArea->fRecordChanged);
    break;

  case DBRI_ENCRYPTED:
    if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
    {
      errResult = Harbour::FAILURE;
    }
    else
    {
      hb_itemPutL(pInfo, pArea->fEncrypted);
    }
    break;

  case DBRI_RAWRECORD:
    if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
    {
      errResult = Harbour::FAILURE;
    }
    else
    {
      hb_itemPutCL(pInfo, reinterpret_cast<char *>(pArea->pRecord), pArea->uiRecordLen);
    }
    break;

  case DBRI_RAWMEMOS:
  case DBRI_RAWDATA:
  {
    HB_SIZE nLength;

    if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
    {
      errResult = Harbour::FAILURE;
      break;
    }
    nLength = uiInfoType == DBRI_RAWDATA ? pArea->uiRecordLen : 0;
    auto pResult = static_cast<HB_BYTE *>(hb_xgrab(nLength + 1));
    if (nLength)
    {
      memcpy(pResult, pArea->pRecord, nLength);
    }

    if (pArea->fHasMemo)
    {
      for (HB_USHORT uiFields = 0; uiFields < pArea->area.uiFieldCount; uiFields++)
      {
        if (pArea->area.lpFields[uiFields].uiType == Harbour::DB::Field::MEMO ||
            pArea->area.lpFields[uiFields].uiType == Harbour::DB::Field::IMAGE ||
            pArea->area.lpFields[uiFields].uiType == Harbour::DB::Field::BLOB ||
            pArea->area.lpFields[uiFields].uiType == Harbour::DB::Field::OLE)
        {
          errResult = SELF_GETVALUE(&pArea->area, uiFields + 1, pInfo);
          if (errResult != Harbour::SUCCESS)
          {
            break;
          }
          auto nLen = hb_itemGetCLen(pInfo);
          if (nLen > 0)
          {
            pResult = static_cast<HB_BYTE *>(hb_xrealloc(pResult, nLength + nLen + 1));
            memcpy(pResult + nLength, hb_itemGetCPtr(pInfo), nLen);
            nLength += nLen;
          }
        }
      }
    }
    hb_itemPutCLPtr(pInfo, reinterpret_cast<char *>(pResult), nLength);
    break;
  }

  default:
    errResult = SUPER_RECINFO(&pArea->area, pRecID, uiInfoType, pInfo);
  }
  if (ulPrevRec != 0)
  {
    if (SELF_GOTO(&pArea->area, ulPrevRec) != Harbour::SUCCESS && errResult == Harbour::SUCCESS)
    {
      errResult = Harbour::FAILURE;
    }
  }
  return errResult;
}

// Clear the WorkArea for use.
static HB_ERRCODE hb_dbfNewArea(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfNewArea(%p)", static_cast<void*>(pArea)));
#endif

  if (SUPER_NEW(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  // set maximum field name length to 10 characters
  pArea->area.uiMaxFieldNameLength = 10;

  pArea->pDataFile = pArea->pMemoFile = pArea->pMemoTmpFile = nullptr;
  pArea->fDataFlush = pArea->fMemoFlush = false;
  // Index dirty read flag initialized to global RDD setting
  pArea->uiDirtyRead = HB_IDXREAD_DEFAULT;
  // Size for deleted records flag
  pArea->uiRecordLen = 1;
  // DBF header update mode
  pArea->uiSetHeader = DB_SETHEADER_APPENDSYNC;

  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_TABLETYPE, 0, pItem) == Harbour::SUCCESS)
    {
      pArea->bTableType = static_cast<HB_BYTE>(pItem->getNI());
    }
    hb_itemClear(pItem);
    if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_SETHEADER, 0, pItem) == Harbour::SUCCESS)
    {
      pArea->uiSetHeader = static_cast<HB_UINT>(pItem->getNI());
    }
    hb_itemRelease(pItem);
  }

  return Harbour::SUCCESS;
}

// Open a data store in the WorkArea.
static HB_ERRCODE hb_dbfOpen(DBFAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfOpen(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOpenInfo)));
#endif

  HB_ERRCODE errCode;
  HB_USHORT uiFields, uiCount, uiSkip, uiDecimals, uiLen, uiFlags, uiFlagsMask;
  auto fRawBlob = false;
  PHB_ITEM pError, pItem;
  PHB_FNAME pFileName;
  HB_BYTE *pBuffer;
  LPDBFFIELD pField;
  DBFIELDINFO dbFieldInfo;
  char szFileName[HB_PATH_MAX];
  char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];

  pArea->lpdbOpenInfo = pOpenInfo;

  pItem = hb_itemNew(nullptr);

  if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_PENDINGTRIGGER, pOpenInfo->ulConnection, pItem) == Harbour::SUCCESS)
  {
    if (pItem->isString())
    {
      hb_dbfTriggerSet(pArea, pItem);
    }
  }

  if (!pArea->fTrigger)
  {
    hb_itemClear(pItem);
    if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_TRIGGER, pOpenInfo->ulConnection, pItem) == Harbour::SUCCESS)
    {
      if (pItem->isString())
      {
        hb_dbfTriggerSet(pArea, pItem);
      }
    }
  }

  if (pArea->fTrigger)
  {
    hb_itemPutC(pItem, pOpenInfo->abName);
    if (!hb_dbfTriggerDo(pArea, EVENT_PREUSE, 0, pItem))
    {
      hb_itemRelease(pItem);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    hb_strncpy(szFileName, hb_itemGetCPtr(pItem), sizeof(szFileName) - 1);
  }
  else
  {
    hb_strncpy(szFileName, pOpenInfo->abName, sizeof(szFileName) - 1);
  }

  if (!pArea->bLockType)
  {
    hb_itemClear(pItem);
    if (SELF_INFO(&pArea->area, DBI_LOCKSCHEME, pItem) != Harbour::SUCCESS)
    {
      hb_itemRelease(pItem);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    pArea->bLockType = static_cast<HB_BYTE>(hb_itemGetNI(pItem));
    if (!pArea->bLockType)
    {
      pArea->bLockType = DB_DBFLOCK_CLIPPER;
    }
  }

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

  pArea->fShared = pOpenInfo->fShared;
  pArea->fReadonly = pOpenInfo->fReadonly;
  // Force exclusive mode
  //   0: AUTOSHARE disabled.
  //   1: AUTOSHARE enabled.
  //   2: force exclusive mode.
  if (hb_setGetAutoShare() == 2)
  {
    pArea->fShared = false;
  }
  uiFlags = (pArea->fReadonly ? FO_READ : FO_READWRITE) | (pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE);
  pError = nullptr;

  pFileName = hb_fsFNameSplit(szFileName);
  // Add default file name extension if necessary
  if (!pFileName->szExtension && hb_setGetDefExtension())
  {
    hb_itemClear(pItem);
    if (SELF_INFO(&pArea->area, DBI_TABLEEXT, pItem) != Harbour::SUCCESS)
    {
      hb_xfree(pFileName);
      hb_itemRelease(pItem);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }
    pFileName->szExtension = hb_itemGetCPtr(pItem);
    hb_fsFNameMerge(szFileName, pFileName);
  }

  // Create default alias if necessary
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

  hb_itemClear(pItem);
  fRawBlob =
      SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_BLOB_SUPPORT, pOpenInfo->ulConnection, pItem) == Harbour::SUCCESS &&
      hb_itemGetL(pItem);
  hb_itemClear(pItem);
  uiDecimals = static_cast<HB_USHORT>(
      SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_DECIMALS, pOpenInfo->ulConnection, pItem) == Harbour::SUCCESS
          ? hb_itemGetNI(pItem)
          : 0);
  hb_itemRelease(pItem);
  uiFlagsMask = 0;

  if (fRawBlob)
  {
    uiFields = uiSkip = 0;
    pBuffer = nullptr;
    pArea->fHasMemo = true;
  }
  else
  {
    HB_ERRCODE errOsCode;
    HB_SIZE nSize;

    // Try open
    do
    {
      pArea->pDataFile = hb_fileExtOpen(
          szFileName, nullptr, uiFlags | FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME | FXO_NOSEEKPOS, nullptr, pError);
      if (pArea->pDataFile)
      {
        break;
      }
    } while (hb_dbfErrorRT(pArea, EG_OPEN, EDBF_OPEN_DBF, szFileName, hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                           &pError) == E_RETRY);

    if (pError)
    {
      hb_itemRelease(pError);
      pError = nullptr;
    }

    // Exit if error
    if (!pArea->pDataFile)
    {
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return Harbour::FAILURE;
    }

    // Allocate only after successfully open file
    pArea->szDataFileName = hb_strdup(szFileName);

    // Read file header and exit if error
    errCode = SELF_READDBHEADER(&pArea->area);
    if (errCode != Harbour::SUCCESS)
    {
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return errCode;
    }

    // Add fields
    uiSkip = 0;
    uiFields = (pArea->uiHeaderLen - sizeof(DBFHEADER)) / sizeof(DBFFIELD);
    nSize = static_cast<HB_SIZE>(uiFields) * sizeof(DBFFIELD);
    pBuffer = uiFields ? static_cast<HB_BYTE *>(hb_xgrab(nSize)) : nullptr;

    // Read fields and exit if error
    do
    {
      if (hb_fileReadAt(pArea->pDataFile, pBuffer, nSize, sizeof(DBFHEADER)) == nSize)
      {
        errCode = Harbour::SUCCESS;
        break;
      }
      errOsCode = hb_fsError();
      errCode = Harbour::FAILURE;
    } while (hb_dbfErrorRT(pArea, errOsCode == 0 ? EG_CORRUPTION : EG_READ, errOsCode == 0 ? EDBF_CORRUPT : EDBF_READ,
                           pArea->szDataFileName, errOsCode, EF_CANRETRY | EF_CANDEFAULT, &pError) == E_RETRY);
    if (pError)
    {
      hb_itemRelease(pError);
    }

    // Exit if error
    if (errCode != Harbour::SUCCESS)
    {
      if (pBuffer)
      {
        hb_xfree(pBuffer);
      }
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return errCode;
    }

    // We cannot accept bFieldFlags as is because Clipper
    // creates tables where this field is random so we have to
    // try to guess if we can use it. If we know that table
    // was created by VFP which uses field flags then we can
    // retrieve information from bFieldFlags without any problem.
    // Otherwise we check if extended field types are used or if
    // unused bytes in field area are cleared. It's not perfect
    // but works in most of cases, Druzus.
    uiFlags = HB_FF_HIDDEN | HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_AUTOINC;
    if (pArea->bTableType == DB_DBF_VFP)
    {
      uiFlagsMask = uiFlags;
    }

    // some RDDs use the additional space in the header after field array
    // for private data we should check for 0x0D marker to not use this
    // data as fields description.
    for (uiCount = 0; uiCount < uiFields; uiCount++)
    {
      pField = reinterpret_cast<LPDBFFIELD>(pBuffer + uiCount * sizeof(DBFFIELD));

      if (uiFlagsMask == 0)
      {
        switch (pField->bType)
        {
        case 'L':
        case 'D':
          if (pField->bFieldFlags & ~HB_FF_NULLABLE)
          {
            uiFlags = 0;
            break;
          }
          // fallthrough
        case 'N':
          if (pField->bFieldFlags & ~(HB_FF_NULLABLE | HB_FF_AUTOINC))
          {
            uiFlags = 0;
            break;
          }
          else if ((pField->bFieldFlags & HB_FF_AUTOINC) != 0)
          {
            if (HB_GET_LE_UINT32(pField->bReserved1) != 0 ||
                (pField->bLen - (pField->bDec ? pField->bDec + 1 : 0) > 9
                     ? HB_GET_LE_UINT32(pField->bCounter) != 0
                     : ((uiCount > 0 && HB_GET_LE_UINT32(pField->bReserved2) != 0) ||
                        HB_GET_LE_UINT32(&pField->bReserved2[4]) != 0)))
            {
              uiFlags = 0;
            }
            break;
          }
          // fallthrough
        case 'C':
        case 'M':
        case 'V':
          if (HB_GET_LE_UINT32(pField->bReserved1) != 0 || (uiCount > 0 && HB_GET_LE_UINT32(pField->bReserved2) != 0) ||
              HB_GET_LE_UINT32(&pField->bReserved2[4]) != 0 || HB_GET_LE_UINT32(pField->bCounter) != 0 ||
              pField->bStep != 0 || (pField->bFieldFlags & ~(HB_FF_NULLABLE | HB_FF_BINARY)) != 0)
          {
            uiFlags = 0;
          }
          break;
        default:
          uiFlagsMask = HB_FF_HIDDEN | HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_AUTOINC;
        }
      }

      if (pField->bName[0] == 0x0d)
      {
        uiFields = uiCount;
        break;
      }
      else if ((pField->bFieldFlags & 0x01) != 0 && (pField->bType == '0' || pArea->bTableType == DB_DBF_VFP))
      {
        uiSkip++;
      }
    }
    uiFlagsMask |= uiFlags;
    uiFields -= uiSkip;
  }

  // CL5.3 allow to create and open DBFs without fields
#ifdef HB_CLP_STRICT
  if (uiFields == 0)
  {
    errCode = Harbour::FAILURE;
  }
  else
#endif
  {
    errCode = SELF_SETFIELDEXTENT(&pArea->area, uiFields);
    if (errCode != Harbour::SUCCESS)
    {
      SELF_CLOSE(&pArea->area);
      pArea->lpdbOpenInfo = nullptr;
      return errCode;
    }
  }

  // Clear dbFieldInfo structure
  memset(&dbFieldInfo, 0, sizeof(dbFieldInfo));

  // Size for deleted flag
  pArea->uiRecordLen = 1;
  pArea->uiNullCount = 0;
  for (uiCount = 0; uiCount < uiFields + uiSkip; uiCount++)
  {
    pField = reinterpret_cast<LPDBFFIELD>(pBuffer + uiCount * sizeof(DBFFIELD));
    pField->bName[10] = '\0';
#if 0
      hb_strupp(static_cast<char*>(pField->bName));
#endif
    dbFieldInfo.atomName = reinterpret_cast<const char *>(pField->bName);
    dbFieldInfo.uiLen = pField->bLen;
    dbFieldInfo.uiDec = 0;
    dbFieldInfo.uiTypeExtended = 0;
    dbFieldInfo.uiFlags = pField->bFieldFlags & uiFlagsMask;

    switch (pField->bType)
    {
    case 'C':
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      dbFieldInfo.uiLen = pField->bLen + pField->bDec * 256;
      break;

    case 'L':
      dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
      dbFieldInfo.uiLen = 1;
      break;

    case 'D':
      dbFieldInfo.uiType = Harbour::DB::Field::DATE;
      if (dbFieldInfo.uiLen != 3 && dbFieldInfo.uiLen != 4)
      {
        dbFieldInfo.uiLen = 8;
      }
      break;

    case 'I':
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
      if ((dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8) || dbFieldInfo.uiLen == 0)
      {
        dbFieldInfo.uiLen = 4;
      }
      dbFieldInfo.uiDec = pField->bDec;
      break;

    case 'Y':
      dbFieldInfo.uiType = Harbour::DB::Field::CURRENCY;
      if ((dbFieldInfo.uiLen > 4 && dbFieldInfo.uiLen != 8) || dbFieldInfo.uiLen == 0)
      {
        dbFieldInfo.uiLen = 8;
      }
      dbFieldInfo.uiDec = pField->bDec;
      break;

    case '2':
    case '4':
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
      dbFieldInfo.uiLen = pField->bType - '0';
      break;

    case 'N':
      dbFieldInfo.uiType = Harbour::DB::Field::LONG;
      dbFieldInfo.uiDec = pField->bDec;
      // dBase documentation defines maximum numeric field size as 20
      // but Clipper allows to create longer fields so I removed this
      // limit, Druzus
#if 0
      if (pField->bLen > 20)
      {
        errCode = Harbour::FAILURE;
      }
#endif
      break;

    case 'F':
      dbFieldInfo.uiType = Harbour::DB::Field::FLOAT;
      dbFieldInfo.uiDec = pField->bDec;
      // See note above
      break;

    case '8':
    case 'B':
      dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
      dbFieldInfo.uiDec = pField->bDec;
      if (dbFieldInfo.uiLen != 8)
      {
        errCode = Harbour::FAILURE;
      }
      else if (dbFieldInfo.uiDec == 0)
      {
        dbFieldInfo.uiDec = uiDecimals;
      }
      break;

    case 'T':
      if (dbFieldInfo.uiLen == 8)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
      }
      else if (dbFieldInfo.uiLen == 4)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIME;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;

    // types which are not supported by VM - mapped to different ones
    case '@':
      dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
      if (dbFieldInfo.uiLen != 8)
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case '=':
      dbFieldInfo.uiType = Harbour::DB::Field::MODTIME;
      if (dbFieldInfo.uiLen != 8)
      {
        errCode = Harbour::FAILURE;
      }
      pArea->fModStamp = true;
      break;

    case '^':
      dbFieldInfo.uiType = Harbour::DB::Field::ROWVER;
      if (dbFieldInfo.uiLen != 8)
      {
        errCode = Harbour::FAILURE;
      }
      pArea->fModStamp = true;
      break;

    case '+':
      dbFieldInfo.uiType = Harbour::DB::Field::AUTOINC;
      if (dbFieldInfo.uiLen != 4)
      {
        errCode = Harbour::FAILURE;
      }
      pArea->fAutoInc = true;
      break;

    case 'Q':
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      if (pArea->bTableType == DB_DBF_VFP)
      {
        dbFieldInfo.uiFlags |= HB_FF_BINARY;
      }
      else
      {
        dbFieldInfo.uiFlags |= HB_FF_BINARY & pField->bFieldFlags;
      }
      hb_dbfAllocNullFlag(pArea, uiCount, true);
      break;

    case 'V':
      if (pArea->bTableType == DB_DBF_VFP)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
#if 0
               dbFieldInfo.uiFlags &= ~HB_FF_BINARY;
#endif
        hb_dbfAllocNullFlag(pArea, uiCount, true);
      }
      else
      {
        dbFieldInfo.uiType = Harbour::DB::Field::ANY;
        if (dbFieldInfo.uiLen >= 6)
        {
          pArea->uiMemoVersion = DB_MEMOVER_SIX;
          pArea->fHasMemo = true;
        }
      }
      break;

    case 'M':
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
      pArea->fHasMemo = true;
      break;

    case 'P':
      dbFieldInfo.uiType = Harbour::DB::Field::IMAGE;
      dbFieldInfo.uiFlags |= HB_FF_BINARY;
      pArea->fHasMemo = true;
      break;

    case 'W':
      dbFieldInfo.uiType = Harbour::DB::Field::BLOB;
      dbFieldInfo.uiFlags |= HB_FF_BINARY;
      pArea->fHasMemo = true;
      break;

    case 'G':
      dbFieldInfo.uiType = Harbour::DB::Field::OLE;
      dbFieldInfo.uiFlags |= HB_FF_BINARY;
      pArea->fHasMemo = true;
      break;

    case '\x1A':
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      uiLen = pField->bLen + pField->bDec * 256;
      if (uiLen & 1)
      {
        errCode = Harbour::FAILURE;
      }
      dbFieldInfo.uiLen = uiLen >> 1;
      break;

    case '\x1B':
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      uiLen = pField->bLen + pField->bDec * 256;
      if (uiLen & 1 || uiLen < 2)
      {
        errCode = Harbour::FAILURE;
      }
      dbFieldInfo.uiLen = (uiLen >> 1) - 1;
      break;

    case '\x1C':
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
      dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      pArea->fHasMemo = true;
      break;

    case '0':
      if ((pField->bFieldFlags & HB_FF_HIDDEN) != 0)
      {
        if (memcmp(dbFieldInfo.atomName, "_NullFlags", 10) == 0)
        {
          pArea->uiNullOffset = pArea->uiRecordLen;
        }
        pArea->uiRecordLen += dbFieldInfo.uiLen;
        if (pArea->uiRecordLen >= dbFieldInfo.uiLen)
        {
          continue;
        }
      }
      // fallthrough

    default:
      errCode = Harbour::FAILURE;
      break;
    }

    if (errCode == Harbour::SUCCESS)
    {
      if ((dbFieldInfo.uiFlags & HB_FF_NULLABLE) != 0)
      {
        hb_dbfAllocNullFlag(pArea, uiCount, false);
      }
      // Add field
      errCode = SELF_ADDFIELD(&pArea->area, &dbFieldInfo);
    }

    // Exit if error
    if (errCode != Harbour::SUCCESS)
    {
      break;
    }
  }
  if (pBuffer)
  {
    hb_xfree(pBuffer);
  }

  if (pArea->uiNullCount > 0 && pArea->uiNullOffset == 0)
  {
    errCode = Harbour::FAILURE;
  }

  // Exit if error
  if (errCode != Harbour::SUCCESS)
  {
    hb_dbfErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, pArea->szDataFileName, 0, EF_CANDEFAULT, nullptr);
    SELF_CLOSE(&pArea->area);
    pArea->lpdbOpenInfo = nullptr;
    return errCode;
  }

  pItem = hb_itemNew(nullptr);
  if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_PENDINGPASSWORD, pOpenInfo->ulConnection, pItem) ==
      Harbour::SUCCESS)
  {
    hb_dbfPasswordSet(pArea, pItem, false);
  }
  else
  {
    hb_itemClear(pItem);
    if (SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_PASSWORD, pOpenInfo->ulConnection, pItem) == Harbour::SUCCESS)
    {
      hb_dbfPasswordSet(pArea, pItem, false);
    }
  }
  hb_itemRelease(pItem);

  // Open memo file if exists
  if (pArea->fHasMemo)
  {
    pFileName = hb_fsFNameSplit(szFileName);
    pFileName->szExtension = nullptr;
    hb_fsFNameMerge(szFileName, pFileName);
    hb_xfree(pFileName);
    pOpenInfo->abName = szFileName;
    errCode = SELF_OPENMEMFILE(&pArea->area, pOpenInfo);
  }

  if (errCode == Harbour::SUCCESS)
  {
    // If successful call SUPER_OPEN to finish system jobs
    errCode = SUPER_OPEN(&pArea->area, pOpenInfo);
  }

  if (errCode != Harbour::SUCCESS)
  {
    SELF_CLOSE(&pArea->area);
    pArea->lpdbOpenInfo = nullptr;
    return Harbour::FAILURE;
  }

  // Alloc buffer
  pArea->pRecord = static_cast<HB_BYTE *>(hb_xgrab(pArea->uiRecordLen));
  pArea->fValidBuffer = false;

  // Update the number of record for corrupted headers
  pArea->ulRecCount = hb_dbfCalcRecCount(pArea);

  // Position cursor at the first record
  errCode = SELF_GOTOP(&pArea->area);

  if (pArea->fTrigger)
  {
    hb_dbfTriggerDo(pArea, EVENT_POSTUSE, 0, nullptr);
  }

  pArea->lpdbOpenInfo = nullptr;

  return errCode;
}

#define hb_dbfRelease nullptr

// Retrieve the size of the WorkArea structure.
static HB_ERRCODE hb_dbfStructSize(DBFAREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfStrucSize(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiSize)));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(DBFAREA);
  return Harbour::SUCCESS;
}

#define hb_dbfSysName nullptr
#define hb_dbfEval nullptr

// Pack helper function called for each packed record
static HB_ERRCODE hb_dbfPackRec(DBFAREAP pArea, HB_ULONG ulRecNo, HB_BOOL *fWritten)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPackRec(%p, %lu, %p)", static_cast<void*>(pArea), ulRecNo, static_cast<void*>(fWritten)));
#endif

  HB_SYMBOL_UNUSED(ulRecNo);

  *fWritten = !pArea->fDeleted;

  return Harbour::SUCCESS;
}

// Remove records marked for deletion from a database.
static HB_ERRCODE hb_dbfPack(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPack(%p)", static_cast<void*>(pArea)));
#endif

  HB_ULONG ulRecIn, ulRecOut, ulEvery, ulUserEvery;
  PHB_ITEM pBlock;
  HB_BOOL fWritten;

  if (pArea->fReadonly)
  {
    hb_dbfErrorRT(pArea, EG_READONLY, EDBF_READONLY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->fShared)
  {
    hb_dbfErrorRT(pArea, EG_SHARED, EDBF_SHARED, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_PACK, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  // This is bad hack but looks that people begins to use it :-(
  // so I'll add workaround to make it more safe
  if (pArea->area.valResult && pArea->area.valResult->isArray() && hb_arrayLen(pArea->area.valResult) == 2 &&
      (hb_arrayGetType(pArea->area.valResult, 1) & Harbour::Item::BLOCK) != 0 &&
      (hb_arrayGetType(pArea->area.valResult, 2) & Harbour::Item::NUMERIC) != 0)
  {
    pBlock = hb_itemNew(nullptr);
    hb_arrayGet(pArea->area.valResult, 1, pBlock);
    if (hb_arrayGetND(pArea->area.valResult, 2) >= 1)
    {
      ulUserEvery = hb_arrayGetNL(pArea->area.valResult, 2);
    }
    else
    {
      ulUserEvery = 1;
    }
  }
  else
  {
    pBlock = nullptr;
    ulUserEvery = 0;
  }

  ulRecOut = ulEvery = 0;
  ulRecIn = 1;
  while (ulRecIn <= pArea->ulRecCount)
  {
    if (SELF_GOTO(&pArea->area, ulRecIn) != Harbour::SUCCESS)
    {
      if (pBlock)
      {
        hb_itemRelease(pBlock);
      }
      return Harbour::FAILURE;
    }
    if (!hb_dbfReadRecord(pArea))
    {
      if (pBlock)
      {
        hb_itemRelease(pBlock);
      }
      return Harbour::FAILURE;
    }

    // Execute the Code Block
    if (pBlock)
    {
      if (++ulEvery >= ulUserEvery)
      {
        ulEvery = 0;
        if (SELF_EVALBLOCK(&pArea->area, pBlock) != Harbour::SUCCESS)
        {
          hb_itemRelease(pBlock);
          return Harbour::FAILURE;
        }
      }
    }

    if (SELF_PACKREC(&pArea->area, ulRecOut + 1, &fWritten) != Harbour::SUCCESS)
    {
      if (pBlock)
      {
        hb_itemRelease(pBlock);
      }
      return Harbour::FAILURE;
    }

    if (fWritten)
    {
      ulRecOut++;
      if (pArea->ulRecNo != ulRecOut || pArea->fRecordChanged)
      {
        pArea->ulRecNo = ulRecOut;
        pArea->fRecordChanged = true;
        if (!hb_dbfWriteRecord(pArea))
        {
          if (pBlock)
          {
            hb_itemRelease(pBlock);
          }
          return Harbour::FAILURE;
        }
      }
    }
    ulRecIn++;
  }

  // Execute the Code Block for pending record
  if (pBlock)
  {
    if (ulEvery > 0)
    {
      if (SELF_EVALBLOCK(&pArea->area, pBlock) != Harbour::SUCCESS)
      {
        hb_itemRelease(pBlock);
        return Harbour::FAILURE;
      }
    }
    hb_itemRelease(pBlock);
  }

  if (pArea->ulRecCount != ulRecOut)
  {
    pArea->ulRecCount = ulRecOut;
    if (SELF_WRITEDBHEADER(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  return SELF_GOTO(&pArea->area, 1);
}

static HB_ERRCODE hb_dbfTransCond(DBFAREAP pArea, LPDBTRANSINFO pTransInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfTransCond(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pTransInfo)));
#endif

  if (pTransInfo->uiFlags & DBTF_MATCH)
  {
    if (pArea->fHasMemo || pArea->area.cdPage != pTransInfo->lpaDest->cdPage)
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

  return Harbour::SUCCESS;
}

// NOTE: For large tables the sorting algorithm may access source records
//       more then once. It means that results may be wrongly sorted when
//       table is changed online by other station during exporting. This
//       can be easy eliminated by copping source records to temporary
//       file anyhow it will introduce additional overhead in all cases
//       and user can easy eliminate the problem by simple FLOCK before
//       sort or making export to temporary file and then sorting this
//       file so I decided to not implement it.
//       I haven't tested what Cl*pper exactly does in such case so
//       I cannot say if current behavior is or isn't Cl*pper compatible.
//       [druzus]
#define HB_SORTREC_ARRAYSIZE 0x10000
#define HB_SORTREC_FIRSTALLOC 0x100
#define HB_SORTREC_MINRECBUF 0x10

#if HB_SORTREC_ARRAYSIZE <= 0x10000
using HB_SORTIDX = HB_U16;
#else
using HB_SORTIDX = HB_U32;
#endif
using HB_DBRECNO = HB_U32;

struct HB_DBSORTPAGE
{
  HB_FOFFSET nOffset;
  HB_DBRECNO nCount;
  HB_DBRECNO nInBuf;
  HB_DBRECNO nCurrent;
  HB_DBRECNO *pnRecords;
};

using PHB_DBSORTPAGE = HB_DBSORTPAGE *;

struct DBSORTREC
{
  LPDBSORTINFO pSortInfo;

  PHB_FILE pTempFile;
  char *szTempFileName;

  HB_SORTIDX nPages;
  HB_SORTIDX nMaxPage;
  PHB_DBSORTPAGE pSwapPages;

  HB_DBRECNO nCount;
  HB_DBRECNO nMaxRec;
  HB_SORTIDX *pnIndex;
  HB_DBRECNO *pnRecords;
  HB_DBRECNO *pnOrder;
  PHB_ITEM pSortArray;
};

using LPDBSORTREC = DBSORTREC *;

static HB_ERRCODE hb_dbfSortInit(LPDBSORTREC pSortRec, LPDBSORTINFO pSortInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSortInit(%p, %p)", static_cast<void*>(pSortInfo), static_cast<void*>(pSortRec)));
#endif

  HB_USHORT uiCount, uiDest;

  memset(pSortRec, 0, sizeof(*pSortRec));
  pSortRec->pSortInfo = pSortInfo;

  for (uiCount = uiDest = 0; uiCount < pSortInfo->uiItemCount; ++uiCount)
  {
    LPFIELD pField = pSortInfo->dbtri.lpaSource->lpFields + pSortInfo->lpdbsItem[uiCount].uiField - 1;

    switch (pField->uiType)
    {
    case Harbour::DB::Field::ANY:
      if (pField->uiLen == 4)
      {
        pSortInfo->lpdbsItem[uiCount].uiFlags |= SF_LONG;
        break;
      }
      if (pField->uiLen == 3)
      {
        break;
      }
      // fallthrough
    case Harbour::DB::Field::MEMO:
    case Harbour::DB::Field::IMAGE:
    case Harbour::DB::Field::BLOB:
    case Harbour::DB::Field::OLE:
      pSortInfo->lpdbsItem[uiCount].uiField = 0;
      break;

    case Harbour::DB::Field::INTEGER:
    case Harbour::DB::Field::CURRENCY:
    case Harbour::DB::Field::AUTOINC:
    case Harbour::DB::Field::ROWVER:
      pSortInfo->lpdbsItem[uiCount].uiFlags |= pField->uiDec == 0 ? SF_LONG : SF_DOUBLE;
      break;
    case Harbour::DB::Field::LONG:
      if (pField->uiDec == 0 && pField->uiLen < 19)
      {
        pSortInfo->lpdbsItem[uiCount].uiFlags |= SF_LONG;
        break;
      }
      // fallthrough
    case Harbour::DB::Field::FLOAT:
    case Harbour::DB::Field::DOUBLE:
    case Harbour::DB::Field::CURDOUBLE:
      pSortInfo->lpdbsItem[uiCount].uiFlags |= SF_DOUBLE;
      break;

    case Harbour::DB::Field::STRING:
    case Harbour::DB::Field::VARLENGTH:
      break;

    case Harbour::DB::Field::DATE:
    case Harbour::DB::Field::TIME:
    case Harbour::DB::Field::MODTIME:
    case Harbour::DB::Field::TIMESTAMP:
    case Harbour::DB::Field::LOGICAL:
      break;

    default:
      pSortInfo->lpdbsItem[uiCount].uiField = 0;
      break;
    }
    switch (pField->uiType)
    {
    case Harbour::DB::Field::STRING:
    case Harbour::DB::Field::VARLENGTH:
      break;
    default:
      pSortInfo->lpdbsItem[uiCount].uiFlags &= ~SF_CASE;
    }
    if (pSortInfo->lpdbsItem[uiCount].uiField != 0)
    {
      if (uiCount != uiDest)
      {
        pSortInfo->lpdbsItem[uiDest].uiField = pSortInfo->lpdbsItem[uiCount].uiField;
        pSortInfo->lpdbsItem[uiDest].uiFlags = pSortInfo->lpdbsItem[uiCount].uiFlags;
      }
      ++uiDest;
    }
  }
  pSortInfo->uiItemCount = uiDest;

  return Harbour::SUCCESS;
}

static void hb_dbfSortFree(LPDBSORTREC pSortRec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSortFree(%p)", static_cast<void*>(pSortRec)));
#endif

  if (pSortRec->pTempFile != nullptr)
  {
    hb_fileClose(pSortRec->pTempFile);
  }
  if (pSortRec->szTempFileName)
  {
    hb_fileDelete(pSortRec->szTempFileName);
    hb_xfree(pSortRec->szTempFileName);
  }

  if (pSortRec->pSortArray)
  {
    hb_itemRelease(pSortRec->pSortArray);
  }
  if (pSortRec->pnIndex)
  {
    hb_xfree(pSortRec->pnIndex);
  }
  if (pSortRec->pnRecords)
  {
    hb_xfree(pSortRec->pnRecords);
  }
  if (pSortRec->pnOrder)
  {
    hb_xfree(pSortRec->pnOrder);
  }
  if (pSortRec->pSwapPages)
  {
    hb_xfree(pSortRec->pSwapPages);
  }
}

static int hb_dbfSortCmp(LPDBSORTREC pSortRec, PHB_ITEM pValue1, PHB_ITEM pValue2)
{
  for (HB_USHORT uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; ++uiCount)
  {
    HB_USHORT uiFlags = pSortRec->pSortInfo->lpdbsItem[uiCount].uiFlags;
    auto pItem1 = hb_arrayGetItemPtr(pValue1, uiCount + 1);
    auto pItem2 = hb_arrayGetItemPtr(pValue2, uiCount + 1);
    int i = 0;

    if (uiFlags & SF_DOUBLE)
    {
      auto dValue1 = hb_itemGetND(pItem1);
      auto dValue2 = hb_itemGetND(pItem2);
      i = dValue1 < dValue2 ? -1 : (dValue1 == dValue2 ? 0 : 1);
    }
    else if (uiFlags & SF_LONG)
    {
      HB_MAXINT nValue1 = hb_itemGetNInt(pItem1), nValue2 = hb_itemGetNInt(pItem2);
      i = nValue1 < nValue2 ? -1 : (nValue1 == nValue2 ? 0 : 1);
    }
    else if (pItem1->isString())
    {
      if (!pItem2->isString())
      {
        i = 1;
      }
      else if (uiFlags & SF_CASE)
      {
        i = hb_itemStrICmp(pItem1, pItem2, true);
      }
      else
      {
        i = hb_itemStrCmp(pItem1, pItem2, true);
      }
    }
    else if (pItem1->isDateTime())
    {
      double dValue1 = pItem1->getTD(), dValue2 = hb_itemGetTD(pItem2);
      i = dValue1 < dValue2 ? -1 : (dValue1 == dValue2 ? 0 : 1);
    }
    else if (pItem1->isLogical())
    {
      i = pItem1->getL() ? (hb_itemGetL(pItem2) ? 0 : 1) : (hb_itemGetL(pItem2) ? -1 : 0);
    }
    if (i != 0)
    {
      return (uiFlags & SF_DESCEND) ? -i : i;
    }
  }

  return 0;
}

static int hb_dbfSortCompare(LPDBSORTREC pSortRec, HB_SORTIDX nIndex1, HB_SORTIDX nIndex2)
{
  int i = hb_dbfSortCmp(pSortRec, hb_arrayGetItemPtr(pSortRec->pSortArray, nIndex1 + 1),
                        hb_arrayGetItemPtr(pSortRec->pSortArray, nIndex2 + 1));
  return i == 0 ? (nIndex1 < nIndex2 ? -1 : 1) : i;
}

static HB_BOOL hb_dbfSortQSort(LPDBSORTREC pSortRec, HB_SORTIDX *pSrc, HB_SORTIDX *pBuf, HB_DBRECNO nKeys)
{
  if (nKeys > 1)
  {
    HB_DBRECNO n1, n2;
    HB_SORTIDX *pPtr1, *pPtr2, *pDst;
    HB_BOOL f1, f2;

    n1 = nKeys >> 1;
    n2 = nKeys - n1;
    pPtr1 = &pSrc[0];
    pPtr2 = &pSrc[n1];

    f1 = hb_dbfSortQSort(pSortRec, pPtr1, &pBuf[0], n1);
    f2 = hb_dbfSortQSort(pSortRec, pPtr2, &pBuf[n1], n2);
    if (f1)
    {
      pDst = pBuf;
    }
    else
    {
      pDst = pSrc;
      pPtr1 = &pBuf[0];
    }
    if (!f2)
    {
      pPtr2 = &pBuf[n1];
    }
    while (n1 > 0 && n2 > 0)
    {
      if (hb_dbfSortCompare(pSortRec, *pPtr1, *pPtr2) <= 0)
      {
        *pDst++ = *pPtr1++;
        n1--;
      }
      else
      {
        *pDst++ = *pPtr2++;
        n2--;
      }
    }
    if (n1 > 0)
    {
      memcpy(pDst, pPtr1, n1 * sizeof(HB_SORTIDX));
    }
    else if (n2 > 0 && f1 == f2)
    {
      memcpy(pDst, pPtr2, n2 * sizeof(HB_SORTIDX));
    }
    return !f1;
  }
  return true;
}

static HB_DBRECNO *hb_dbfSortSort(LPDBSORTREC pSortRec)
{
  HB_SORTIDX *pOrder;
  HB_DBRECNO nCount;

  if (pSortRec->pnIndex == nullptr)
  {
    pSortRec->pnIndex =
        static_cast<HB_SORTIDX *>(hb_xgrab((static_cast<HB_SIZE>(pSortRec->nCount) << 1) * sizeof(HB_SORTIDX)));
  }
  for (nCount = 0; nCount < pSortRec->nCount; ++nCount)
  {
    pSortRec->pnIndex[nCount] = static_cast<HB_SORTIDX>(nCount);
  }

  pOrder = pSortRec->pnIndex;
  if (!hb_dbfSortQSort(pSortRec, pOrder, &pSortRec->pnIndex[pSortRec->nCount], pSortRec->nCount))
  {
    pOrder += pSortRec->nCount;
  }

  if (pSortRec->pnOrder == nullptr)
  {
    pSortRec->pnOrder =
        static_cast<HB_DBRECNO *>(hb_xgrab(static_cast<HB_SIZE>(pSortRec->nCount) * sizeof(HB_DBRECNO)));
  }
  for (nCount = 0; nCount < pSortRec->nCount; ++nCount)
  {
    pSortRec->pnOrder[nCount] = pSortRec->pnRecords[pOrder[nCount]];
  }

  return pSortRec->pnOrder;
}

static void hb_dbfSortInsPage(LPDBSORTREC pSortRec, HB_SORTIDX *pIndex, HB_SORTIDX nFirst, HB_SORTIDX nLast,
                              HB_SORTIDX nAt)
{
  while (nFirst < nLast)
  {
    HB_SORTIDX nMiddle = (nFirst + nLast) >> 1;
    int i = hb_dbfSortCompare(pSortRec, pIndex[nAt], pIndex[nMiddle]);

    if (i < 0)
    {
      nLast = nMiddle;
    }
    else
    {
      nFirst = nMiddle + 1;
    }
  }
  if (nAt == 0)
  {
    if (nFirst > 1)
    {
      nLast = pIndex[0];
      memmove(pIndex, &pIndex[1], (nFirst - 1) * sizeof(HB_SORTIDX));
      pIndex[nFirst - 1] = nLast;
    }
  }
  else if (nFirst != nAt)
  {
    nLast = pIndex[nAt];
    memmove(&pIndex[nFirst + 1], &pIndex[nFirst], (nAt - nFirst) * sizeof(HB_SORTIDX));
    pIndex[nFirst] = nLast;
  }
}

static HB_ERRCODE hb_dbfSortWritePage(LPDBSORTREC pSortRec)
{
  HB_DBRECNO *pData = hb_dbfSortSort(pSortRec);
  HB_SIZE nSize = static_cast<HB_SIZE>(pSortRec->nCount) * sizeof(HB_DBRECNO);
  AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

  if (pSortRec->pTempFile == nullptr)
  {
    char szName[HB_PATH_MAX];
    pSortRec->pTempFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szName);
    if (pSortRec->pTempFile == nullptr)
    {
      hb_dbfErrorRT(reinterpret_cast<DBFAREAP>(pArea), EG_CREATE, EDBF_CREATE_TEMP, szName, hb_fsError(), 0, nullptr);
      return Harbour::FAILURE;
    }
    pSortRec->szTempFileName = hb_strdup(szName);
  }

  if (pSortRec->nPages == pSortRec->nMaxPage)
  {
    pSortRec->nMaxPage += 8;
    pSortRec->pSwapPages =
        static_cast<PHB_DBSORTPAGE>(hb_xrealloc(pSortRec->pSwapPages, pSortRec->nMaxPage * sizeof(HB_DBSORTPAGE)));
  }
  memset(&pSortRec->pSwapPages[pSortRec->nPages], 0, sizeof(HB_DBSORTPAGE));
  pSortRec->pSwapPages[pSortRec->nPages].nCount = pSortRec->nCount;
  pSortRec->pSwapPages[pSortRec->nPages].nOffset = hb_fileSize(pSortRec->pTempFile);

  if (hb_fileWriteAt(pSortRec->pTempFile, pData, nSize, pSortRec->pSwapPages[pSortRec->nPages].nOffset) != nSize)
  {
    hb_dbfErrorRT(reinterpret_cast<DBFAREAP>(pArea), EG_WRITE, EDBF_WRITE_TEMP, pSortRec->szTempFileName, hb_fsError(),
                  0, nullptr);
    return Harbour::FAILURE;
  }
  pSortRec->nPages++;
  pSortRec->nCount = 0;

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_dbfSortReadRec(LPDBSORTREC pSortRec, PHB_ITEM pValue)
{
  AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

  if (pValue->isNil())
  {
    hb_arrayNew(pValue, pSortRec->pSortInfo->uiItemCount);
  }
  else
  {
    hb_arraySize(pValue, pSortRec->pSortInfo->uiItemCount);
  }

  for (HB_SHORT uiCount = 0; uiCount < pSortRec->pSortInfo->uiItemCount; uiCount++)
  {
    auto pItem = hb_arrayGetItemPtr(pValue, uiCount + 1);
    HB_USHORT uiField = pSortRec->pSortInfo->lpdbsItem[uiCount].uiField;
    if (SELF_GETVALUE(pArea, uiField, pItem) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_dbfSortReadPage(LPDBSORTREC pSortRec, PHB_DBSORTPAGE pPage)
{
  AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
  HB_DBRECNO nCount = HB_MIN(pSortRec->nMaxRec, pPage->nCount);
  HB_SIZE nSize = static_cast<HB_SIZE>(nCount) * sizeof(HB_DBRECNO);

  if (hb_fileReadAt(pSortRec->pTempFile, pPage->pnRecords, nSize, pPage->nOffset) != nSize)
  {
    hb_dbfErrorRT(reinterpret_cast<DBFAREAP>(pArea), EG_READ, EDBF_READ_TEMP, pSortRec->szTempFileName, hb_fsError(), 0,
                  nullptr);
    return Harbour::FAILURE;
  }
  pPage->nOffset += nSize;
  pPage->nInBuf = nCount;
  pPage->nCurrent = 0;

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_dbfSortGetRec(LPDBSORTREC pSortRec, HB_DBRECNO *pnRecNo)
{
  HB_SORTIDX nPage = pSortRec->pnIndex[0];
  PHB_DBSORTPAGE pPage = &pSortRec->pSwapPages[nPage];

  *pnRecNo = pPage->pnRecords[pPage->nCurrent++];
  if (--pPage->nCount == 0)
  {
    if (--pSortRec->nPages > 0)
    {
      memmove(pSortRec->pnIndex, &pSortRec->pnIndex[1], pSortRec->nPages * sizeof(HB_SORTIDX));
    }
  }
  else
  {
    if (pPage->nCurrent == pPage->nInBuf)
    {
      if (hb_dbfSortReadPage(pSortRec, pPage) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }
    if (pSortRec->nPages > 1)
    {
      AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;
      if (SELF_GOTO(pArea, pPage->pnRecords[pPage->nCurrent]) != Harbour::SUCCESS ||
          hb_dbfSortReadRec(pSortRec, hb_arrayGetItemPtr(pSortRec->pSortArray, nPage + 1)) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      hb_dbfSortInsPage(pSortRec, pSortRec->pnIndex, 1, pSortRec->nPages, 0);
    }
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_dbfSortFinish(LPDBSORTREC pSortRec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSortFinish(%p)", static_cast<void*>(pSortRec)));
#endif

  AREAP pArea = pSortRec->pSortInfo->dbtri.lpaSource;

  if (pSortRec->nCount > 0)
  {
    if (pSortRec->nPages > 0)
    {
      HB_DBRECNO nCount, *pnOrder;

      if (hb_dbfSortWritePage(pSortRec) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }

      if (pSortRec->pSortArray)
      {
        hb_itemRelease(pSortRec->pSortArray);
      }
      if (pSortRec->pnRecords)
      {
        hb_xfree(pSortRec->pnRecords);
      }
      if (pSortRec->pnIndex)
      {
        hb_xfree(pSortRec->pnIndex);
      }
      if (pSortRec->pnOrder)
      {
        hb_xfree(pSortRec->pnOrder);
      }

      if (pSortRec->nPages > HB_SORTREC_MINRECBUF)
      {
        pSortRec->nMaxRec = pSortRec->nMaxRec * HB_SORTREC_MINRECBUF / pSortRec->nPages;
      }
      nCount = pSortRec->pSwapPages[pSortRec->nPages - 1].nCount;
      if (nCount < pSortRec->nMaxRec)
      {
        nCount = pSortRec->nMaxRec - nCount;
      }
      else
      {
        nCount = 0;
      }
      nCount = pSortRec->nMaxRec * pSortRec->nPages - nCount;

      pSortRec->pSortArray = hb_itemArrayNew(pSortRec->nPages);
      pSortRec->pnRecords = static_cast<HB_DBRECNO *>(hb_xgrab(pSortRec->nPages * sizeof(HB_DBRECNO)));
      pSortRec->pnIndex = static_cast<HB_SORTIDX *>(hb_xgrab(pSortRec->nPages * sizeof(HB_SORTIDX)));
      pSortRec->pnOrder = pnOrder = static_cast<HB_DBRECNO *>(hb_xgrab(nCount * sizeof(HB_DBRECNO)));
      for (HB_SORTIDX nPage = 0; nPage < pSortRec->nPages; ++nPage, pnOrder += pSortRec->nMaxRec)
      {
        pSortRec->pSwapPages[nPage].pnRecords = pnOrder;
        if (hb_dbfSortReadPage(pSortRec, &pSortRec->pSwapPages[nPage]) != Harbour::SUCCESS ||
            SELF_GOTO(pArea, pnOrder[0]) != Harbour::SUCCESS ||
            hb_dbfSortReadRec(pSortRec, hb_arrayGetItemPtr(pSortRec->pSortArray, nPage + 1)) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }

        pSortRec->pnIndex[nPage] = nPage;
        if (nPage > 0)
        {
          hb_dbfSortInsPage(pSortRec, pSortRec->pnIndex, 0, nPage, nPage);
        }
      }
    }
    else
    {
      pSortRec->pSwapPages = static_cast<PHB_DBSORTPAGE>(hb_xgrabz(sizeof(HB_DBSORTPAGE)));
      pSortRec->pSwapPages[0].nCount = pSortRec->pSwapPages[0].nInBuf = pSortRec->nCount;
      pSortRec->pSwapPages[0].pnRecords = hb_dbfSortSort(pSortRec);
      pSortRec->nPages = 1;
      pSortRec->pnIndex = static_cast<HB_SORTIDX *>(hb_xrealloc(pSortRec->pnIndex, sizeof(HB_SORTIDX)));
      pSortRec->pnIndex[0] = 0;
      if (pSortRec->pSortArray)
      {
        hb_itemRelease(pSortRec->pSortArray);
        pSortRec->pSortArray = nullptr;
      }
      if (pSortRec->pnRecords)
      {
        hb_xfree(pSortRec->pnRecords);
        pSortRec->pnRecords = nullptr;
      }
    }
  }

  while (pSortRec->nPages > 0)
  {
    HB_DBRECNO nRecNo;

    if (hb_dbfSortGetRec(pSortRec, &nRecNo) != Harbour::SUCCESS || SELF_GOTO(pArea, nRecNo) != Harbour::SUCCESS ||
        SELF_TRANSREC(pArea, &pSortRec->pSortInfo->dbtri) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_dbfSortAdd(LPDBSORTREC pSortRec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSortAdd(%p)", static_cast<void*>(pSortRec)));
#endif

  AREAP pArea;
  HB_ULONG ulRecNo;

  pArea = pSortRec->pSortInfo->dbtri.lpaSource;

  if (pSortRec->nCount == pSortRec->nMaxRec)
  {
    if (pSortRec->nMaxRec < HB_SORTREC_ARRAYSIZE)
    {
      if (pSortRec->nMaxRec == 0)
      {
        pSortRec->nMaxRec = HB_SORTREC_FIRSTALLOC;
      }
      else
      {
        pSortRec->nMaxRec <<= 1;
      }
      if (pSortRec->nMaxRec > HB_SORTREC_ARRAYSIZE)
      {
        pSortRec->nMaxRec = HB_SORTREC_ARRAYSIZE;
      }

      pSortRec->pnRecords = static_cast<HB_DBRECNO *>(
          hb_xrealloc(pSortRec->pnRecords, static_cast<HB_SIZE>(pSortRec->nMaxRec) * sizeof(HB_DBRECNO)));
      if (pSortRec->pSortArray)
      {
        hb_arraySize(pSortRec->pSortArray, pSortRec->nMaxRec);
      }
      else
      {
        pSortRec->pSortArray = hb_itemArrayNew(pSortRec->nMaxRec);
      }
    }
    if (pSortRec->nCount == pSortRec->nMaxRec)
    {
      if (hb_dbfSortWritePage(pSortRec) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }
  }

  if (SELF_RECNO(pArea, &ulRecNo) != Harbour::SUCCESS ||
      hb_dbfSortReadRec(pSortRec, hb_arrayGetItemPtr(pSortRec->pSortArray, pSortRec->nCount + 1)) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }
  pSortRec->pnRecords[pSortRec->nCount++] = static_cast<HB_DBRECNO>(ulRecNo);

  return Harbour::SUCCESS;
}

// Export sorted records
static HB_ERRCODE hb_dbfSort(DBFAREAP pArea, LPDBSORTINFO pSortInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSort(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pSortInfo)));
#endif

  DBSORTREC dbSortRec;
  HB_BOOL fEof;
  auto fFor = false;
  HB_LONG lNext = 1;

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (hb_dbfSortInit(&dbSortRec, pSortInfo) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (pSortInfo->uiItemCount == 0)
  {
    return SELF_TRANS(&pArea->area, &pSortInfo->dbtri);
  }

  HB_ERRCODE errCode = hb_dbfTransCond(pArea, &pSortInfo->dbtri);
  if (errCode == Harbour::SUCCESS)
  {
    if (pSortInfo->dbtri.dbsci.itmRecID)
    {
      errCode = SELF_GOTOID(&pArea->area, pSortInfo->dbtri.dbsci.itmRecID);
    }
    else if (pSortInfo->dbtri.dbsci.lNext)
    {
      lNext = hb_itemGetNL(pSortInfo->dbtri.dbsci.lNext);
    }
    else if (!pSortInfo->dbtri.dbsci.itmCobWhile && !hb_itemGetLX(pSortInfo->dbtri.dbsci.fRest))
    {
      errCode = SELF_GOTOP(&pArea->area);
    }
  }

  // TODO: use SKIPSCOPE() method and fRest parameter

  while (errCode == Harbour::SUCCESS && lNext > 0)
  {
    errCode = SELF_EOF(&pArea->area, &fEof);
    if (errCode != Harbour::SUCCESS || fEof)
    {
      break;
    }

    if (pSortInfo->dbtri.dbsci.itmCobWhile)
    {
      errCode = SELF_EVALBLOCK(&pArea->area, pSortInfo->dbtri.dbsci.itmCobWhile);
      if (errCode != Harbour::SUCCESS || !hb_itemGetLX(pArea->area.valResult))
      {
        break;
      }
    }

    if (pSortInfo->dbtri.dbsci.itmCobFor)
    {
      errCode = SELF_EVALBLOCK(&pArea->area, pSortInfo->dbtri.dbsci.itmCobFor);
      if (errCode != Harbour::SUCCESS)
      {
        break;
      }
      fFor = hb_itemGetLX(pArea->area.valResult);
    }
    else
    {
      fFor = true;
    }

    if (fFor)
    {
      errCode = hb_dbfSortAdd(&dbSortRec);
    }

    if (errCode != Harbour::SUCCESS || pSortInfo->dbtri.dbsci.itmRecID || (pSortInfo->dbtri.dbsci.lNext && --lNext < 1))
    {
      break;
    }

    errCode = SELF_SKIP(&pArea->area, 1);
  }

  if (errCode == Harbour::SUCCESS)
  {
    errCode = hb_dbfSortFinish(&dbSortRec);
  }

  hb_dbfSortFree(&dbSortRec);

  return errCode;
}

// Copy one or more records from one WorkArea to another.
static HB_ERRCODE hb_dbfTrans(DBFAREAP pArea, LPDBTRANSINFO pTransInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfTrans(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pTransInfo)));
#endif

  if (hb_dbfTransCond(pArea, pTransInfo) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }
  else
  {
    return SUPER_TRANS(&pArea->area, pTransInfo);
  }
}

#define hb_dbfTransRec nullptr

// Physically remove all records from data store.
static HB_ERRCODE hb_dbfZap(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfZap(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fReadonly)
  {
    hb_dbfErrorRT(pArea, EG_READONLY, EDBF_READONLY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->fShared)
  {
    hb_dbfErrorRT(pArea, EG_SHARED, EDBF_SHARED, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (pArea->fTrigger)
  {
    if (!hb_dbfTriggerDo(pArea, EVENT_ZAP, 0, nullptr))
    {
      return Harbour::FAILURE;
    }
  }

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->ulRecCount = 0;

  if (SELF_WRITEDBHEADER(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (SELF_GOTO(&pArea->area, 0) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  // reset auto-increment and row version fields
  for (HB_USHORT uiField = 0; uiField < pArea->area.uiFieldCount; uiField++)
  {
    if (pArea->area.lpFields[uiField].uiType == Harbour::DB::Field::ROWVER)
    {
      hb_dbfRowVerSet(pArea, uiField, 0);
    }
    else if (hb_dbfIsAutoIncField(&pArea->area.lpFields[uiField]) != HB_AUTOINC_NONE)
    {
      hb_dbfNextValueSet(pArea, uiField, 1);
    }
  }

  // Zap memo file
  if (pArea->fHasMemo)
  {
    if (SELF_CREATEMEMFILE(&pArea->area, nullptr) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  return Harbour::SUCCESS;
}

// Report end of relation.
static HB_ERRCODE hb_dbfChildEnd(DBFAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfChildEnd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  HB_ERRCODE errCode;

  if (pArea->lpdbPendingRel == pRelInfo)
  {
    errCode = SELF_FORCEREL(&pArea->area);
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }
  SUPER_CHILDEND(&pArea->area, pRelInfo);
  return errCode;
}

// Report initialization of a relation.
static HB_ERRCODE hb_dbfChildStart(DBFAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfChildStart(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  if (SELF_CHILDSYNC(&pArea->area, pRelInfo) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }
  return SUPER_CHILDSTART(&pArea->area, pRelInfo);
}

// Post a pending relational movement.
static HB_ERRCODE hb_dbfChildSync(DBFAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfChildSync(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  // !!! The side effect of calling GOCOLD() inside CHILDSYNC() is
  // evaluation of index expressions (index KEY and FOR condition)
  // when the pArea is not the current one - it means that the
  // used RDD has to set proper work area before eval.
  // IMHO GOCOLD() could be safely removed from this place but I'm not
  // sure it's Clipper compatible - I will have to check it, Druzus.

  // I've checked in CL5.3 Technical Reference Guide that only
  // FORCEREL() should ensure that the work area buffer is not HOT
  // and then call RELEVAL() - I hope it describes the CL5.3 DBF* RDDs
  // behavior so I replicate it - the GOCOLD() is moved from CHILDSYNC()
  // to FORCEREL(), Druzus.

  // After some cleanups, the core DBF* code can work with GOCOLD() here
  // and in FORCEREL() without any problems. Because calling GOCOLD() in
  // FORCEREL() may interacts with badly written users RDD which inherits
  // from DBF* RDDs and/or user triggers then I decided to keep it here,
  // Druzus.

  if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->lpdbPendingRel = pRelInfo;

  if (pArea->area.lpdbRelations)
  {
    return SELF_SYNCCHILDREN(&pArea->area);
  }

  return Harbour::SUCCESS;
}

#define hb_dbfSyncChildren nullptr
#define hb_dbfClearRel nullptr

// Force relational seeks in the specified WorkArea.
static HB_ERRCODE hb_dbfForceRel(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfForceRel(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->lpdbPendingRel)
  {
    LPDBRELINFO lpdbPendingRel;

    lpdbPendingRel = pArea->lpdbPendingRel;
    pArea->lpdbPendingRel = nullptr;

// update buffers
// commented out - see comment above in CHILDSYNC() method, Druzus
#if 0
      SELF_GOCOLD(&pArea->area);
#endif

    return SELF_RELEVAL(&pArea->area, lpdbPendingRel);
  }
  return Harbour::SUCCESS;
}

#define hb_dbfRelArea nullptr
#define hb_dbfRelEval nullptr
#define hb_dbfRelText nullptr
#define hb_dbfSetRel nullptr

#define hb_dbfOrderListAdd nullptr
#define hb_dbfOrderListClear nullptr
#define hb_dbfOrderListDelete nullptr
#define hb_dbfOrderListFocus nullptr
#define hb_dbfOrderListRebuild nullptr
#define hb_dbfOrderCondition nullptr
#define hb_dbfOrderCreate nullptr
#define hb_dbfOrderDestroy nullptr
#define hb_dbfOrderInfo nullptr

// Clear the filter condition for the specified WorkArea.
static HB_ERRCODE hb_dbfClearFilter(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfClearFilter(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  return SUPER_CLEARFILTER(&pArea->area);
}

#define hb_dbfClearLocate nullptr
#define hb_dbfClearScope nullptr
#define hb_dbfCountScope nullptr
#define hb_dbfFilterText nullptr
#define hb_dbfScopeInfo nullptr

// Set the filter condition for the specified WorkArea.
static HB_ERRCODE hb_dbfSetFilter(DBFAREAP pArea, LPDBFILTERINFO pFilterInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfSetFilter(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFilterInfo)));
#endif

  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  return SUPER_SETFILTER(&pArea->area, pFilterInfo);
}

#define hb_dbfSetLocate nullptr
#define hb_dbfSetScope nullptr
#define hb_dbfSkipScope nullptr
#define hb_dbfLocate nullptr

#define hb_dbfCompile nullptr
#define hb_dbfError nullptr
#define hb_dbfEvalBlock nullptr

// Perform a network low-level lock in the specified WorkArea.
static HB_ERRCODE hb_dbfRawLock(DBFAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRawLock(%p, %hu, %lu)", static_cast<void*>(pArea), uiAction, ulRecNo));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->fShared)
  {
    HB_FOFFSET nPos, nFlSize, nRlSize;
    int iDir;
    auto fLck = false;

    if (hb_dbfLockData(pArea, &nPos, &nFlSize, &nRlSize, &iDir) == Harbour::FAILURE)
    {
      return Harbour::FAILURE;
    }

    switch (uiAction)
    {
    case FILE_LOCK:
      if (!pArea->fFLocked)
      {
        if (iDir < 0)
        {
          nPos -= nFlSize;
        }
        else
        {
          nPos++;
        }

        fLck = hb_fileLock(pArea->pDataFile, nPos, nFlSize, FL_LOCK);
        if (!fLck)
        {
          errCode = Harbour::FAILURE;
        }
        else
        {
          pArea->fFLocked = true;
        }
      }
      break;

    case FILE_UNLOCK:
      if (pArea->fFLocked)
      {
        if (iDir < 0)
        {
          nPos -= nFlSize;
        }
        else
        {
          nPos++;
        }

        fLck = hb_fileLock(pArea->pDataFile, nPos, nFlSize, FL_UNLOCK);
        if (!fLck)
        {
          errCode = Harbour::FAILURE;
        }
        pArea->fFLocked = false;
      }
      break;

    case REC_LOCK:
      if (!pArea->fFLocked)
      {
        if (iDir < 0)
        {
          nPos -= ulRecNo;
        }
        else if (iDir == 2)
        {
          nPos += (ulRecNo - 1) * pArea->uiRecordLen + pArea->uiHeaderLen;
        }
        else
        {
          nPos += ulRecNo;
        }

        fLck = hb_fileLock(pArea->pDataFile, nPos, nRlSize, FL_LOCK);
        if (!fLck)
        {
          errCode = Harbour::FAILURE;
        }
      }
      break;

    case REC_UNLOCK:
      if (!pArea->fFLocked)
      {
        if (iDir < 0)
        {
          nPos -= ulRecNo;
        }
        else if (iDir == 2)
        {
          nPos += (ulRecNo - 1) * pArea->uiRecordLen + pArea->uiHeaderLen;
        }
        else
        {
          nPos += ulRecNo;
        }

        fLck = hb_fileLock(pArea->pDataFile, nPos, nRlSize, FL_UNLOCK);
        if (!fLck)
        {
          errCode = Harbour::FAILURE;
        }
      }
      break;

    case APPEND_LOCK:
    case HEADER_LOCK:
      if (!pArea->fHeaderLocked)
      {
        for (;;)
        {
          fLck = hb_fileLock(pArea->pDataFile, nPos, 1, FL_LOCK | FLX_WAIT);
          // TODO: call special error handler (LOCKHANDLER) if ! fLck
          if (fLck)
          {
            break;
          }
          hb_releaseCPU();
        }
        if (!fLck)
        {
          errCode = Harbour::FAILURE;
        }
        else
        {
          pArea->fHeaderLocked = true;
        }
      }
      break;

    case APPEND_UNLOCK:
    case HEADER_UNLOCK:
      if (pArea->fHeaderLocked)
      {
        if (!hb_fileLock(pArea->pDataFile, nPos, 1, FL_UNLOCK))
        {
          errCode = Harbour::FAILURE;
        }
        pArea->fHeaderLocked = false;
      }
      break;
    }
  }
  return errCode;
}

// Perform a network lock in the specified WorkArea.
static HB_ERRCODE hb_dbfLock(DBFAREAP pArea, LPDBLOCKINFO pLockInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfLock(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pLockInfo)));
#endif

  if (pArea->fShared)
  {
    switch (pLockInfo->uiMethod)
    {
    case DBLM_EXCLUSIVE:
      return hb_dbfLockRecord(pArea, 0, &pLockInfo->fResult, true);

    case DBLM_MULTIPLE:
      return hb_dbfLockRecord(pArea, hb_itemGetNL(pLockInfo->itmRecID), &pLockInfo->fResult, false);

    case DBLM_FILE:
      return hb_dbfLockFile(pArea, &pLockInfo->fResult);

    default:
      pLockInfo->fResult = false;
    }
  }
  else
  {
    pLockInfo->fResult = true;
  }

  return Harbour::SUCCESS;
}

// Release network locks in the specified WorkArea.
static HB_ERRCODE hb_dbfUnLock(DBFAREAP pArea, PHB_ITEM pRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("dbfUnLock(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecNo)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->fShared)
  {
    if (pArea->ulNumLocksPos > 0)
    {
      HB_ULONG ulRecNo = hb_itemGetNL(pRecNo);
      // Unlock all records?
      if (ulRecNo == 0)
      {
        errCode = hb_dbfUnlockAllRecords(pArea);
      }
      else if (hb_dbfIsLocked(pArea, ulRecNo))
      {
        errCode = hb_dbfUnlockRecord(pArea, ulRecNo);
      }
    }
    if (pArea->fFLocked)
    {
      errCode = hb_dbfUnlockFile(pArea);
    }
  }
  return errCode;
}

#define hb_dbfCloseMemFile nullptr

// Create a memo file in the WorkArea.
static HB_ERRCODE hb_dbfCreateMemFile(DBFAREAP pArea, LPDBOPENINFO pCreateInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfCreateMemFile(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pCreateInfo)));
#endif

  if (pCreateInfo)
  {
    hb_dbfErrorRT(pArea, EG_CREATE, EDBF_DATATYPE, pCreateInfo->abName, 0, 0, nullptr);
  }

  pArea->fHasMemo = false;

  return Harbour::FAILURE;
}

// BLOB2FILE - retrieve memo contents into file
static HB_ERRCODE hb_dbfGetValueFile(DBFAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfGetValueFile(%p, %hu, %s, %hu)", static_cast<void*>(pArea), uiIndex, szFile, uiMode));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;
  LPFIELD pField;

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->area.lpFields + uiIndex;
  if (pField->uiType == Harbour::DB::Field::STRING)
  {
    PHB_FILE pFile;

    pFile = hb_fileExtOpen(szFile, nullptr,
                           FO_WRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS |
                               (uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE),
                           nullptr, nullptr);
    if (!pFile)
    {
      errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
    }
    else
    {
      if (hb_fileWriteAt(pFile, pArea->pRecord + pArea->pFieldOffset[uiIndex], pField->uiLen, hb_fileSize(pFile)) !=
          static_cast<HB_SIZE>(pField->uiLen))
      {
        errCode = EDBF_WRITE;
      }
      hb_fileClose(pFile);
    }
  }
  else
  {
    errCode = EDBF_DATATYPE;
  }

  // Exit if any error
  if (errCode != Harbour::SUCCESS)
  {
    hb_dbfErrorRT(pArea, hb_dbfGetEGcode(errCode), errCode, errCode != EDBF_DATATYPE ? szFile : nullptr,
                  errCode != EDBF_DATATYPE ? hb_fsError() : 0, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

// Open a memo file in the specified WorkArea.
static HB_ERRCODE hb_dbfOpenMemFile(DBFAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfOpenMemFile(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOpenInfo)));
#endif

  hb_dbfErrorRT(pArea, EG_OPEN, EDBF_OPEN_DBF, pOpenInfo->abName, 0, 0, nullptr);

  return Harbour::FAILURE;
}

// FILE2BLOB - store file contents in MEMO
static HB_ERRCODE hb_dbfPutValueFile(DBFAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfPutValueFile(%p, %hu, %s, %hu)", static_cast<void*>(pArea), uiIndex, szFile, uiMode));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;
  LPFIELD pField;

  HB_SYMBOL_UNUSED(uiMode);

  if (pArea->lpdbPendingRel)
  {
    if (SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // Read record
  if (!pArea->fValidBuffer && !hb_dbfReadRecord(pArea))
  {
    return Harbour::FAILURE;
  }

  if (--uiIndex >= pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  if (!pArea->fPositioned)
  {
    return Harbour::FAILURE;
  }

  // Buffer is hot?
  if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->area.lpFields + uiIndex;
  if (pField->uiType == Harbour::DB::Field::STRING)
  {
    PHB_FILE pFile;

    pFile = hb_fileExtOpen(szFile, nullptr, FO_READ | FO_DENYNONE | FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS,
                           nullptr, nullptr);
    if (!pFile)
    {
      errCode = EDBF_OPEN_DBF;
    }
    else
    {
      HB_SIZE nRead = hb_fileReadAt(pFile, pArea->pRecord + pArea->pFieldOffset[uiIndex], pField->uiLen, 0);
      if (nRead != static_cast<HB_SIZE>(FS_ERROR) && nRead < static_cast<HB_SIZE>(pField->uiLen))
      {
        memset(pArea->pRecord + pArea->pFieldOffset[uiIndex] + nRead, ' ', pField->uiLen - nRead);
      }
      hb_fileClose(pFile);
    }
  }
  else
  {
    errCode = EDBF_DATATYPE;
  }

  // Exit if any error
  if (errCode != Harbour::SUCCESS)
  {
    hb_dbfErrorRT(pArea, hb_dbfGetEGcode(errCode), errCode, errCode != EDBF_DATATYPE ? szFile : nullptr,
                  errCode != EDBF_DATATYPE ? hb_fsError() : 0, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

// Read the database file header record in the WorkArea.
static HB_ERRCODE hb_dbfReadDBHeader(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfReadDBHeader(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode;
  PHB_ITEM pError;

  pError = nullptr;
  do
  {
    errCode = Harbour::SUCCESS;

    if (hb_fileReadAt(pArea->pDataFile, &pArea->dbfHeader, sizeof(DBFHEADER), 0) != sizeof(DBFHEADER))
    {
      errCode = EDBF_READ;
    }
    else
    {
      pArea->fAutoInc = pArea->fModStamp = pArea->fTableEncrypted = pArea->fHasMemo = false;
      pArea->bMemoType = DB_MEMO_NONE;
      pArea->bCryptType = DB_CRYPT_NONE;
      if (pArea->bTableType == DB_DBF_VFP)
      {
        pArea->bTableType = DB_DBF_STD;
      }

      pArea->fHasTags = (pArea->dbfHeader.bHasTags & 0x01) != 0;

      switch (pArea->dbfHeader.bVersion)
      {
      case 0x31:
        pArea->fAutoInc = true;
        // fallthrough
      case 0x30:
      case 0x32:
        if (pArea->dbfHeader.bHasTags & 0x02)
        {
          pArea->bMemoType = DB_MEMO_FPT;
          pArea->fHasMemo = true;
        }
        pArea->bTableType = DB_DBF_VFP;
        break;

      case 0x03:
      case 0x07: // CA-VO DBFNTX and ANSI CP
        break;

      case 0x83:
      case 0x87: // CA-VO DBFNTX+MEMO and ANSI CP
        pArea->fHasMemo = true;
        pArea->bMemoType = DB_MEMO_DBT;
        break;

      case 0xE5:
        pArea->fHasMemo = true;
        pArea->bMemoType = DB_MEMO_SMT;
        break;

      case 0xF5:
        pArea->fHasMemo = true;
        pArea->bMemoType = DB_MEMO_FPT;
        break;

      case 0x06:
        pArea->fTableEncrypted = true;
        pArea->bCryptType = DB_CRYPT_SIX;
        break;

      case 0x86:
        pArea->fTableEncrypted = true;
        pArea->fHasMemo = true;
        pArea->bCryptType = DB_CRYPT_SIX;
        pArea->bMemoType = DB_MEMO_DBT;
        break;

      case 0xE6:
        pArea->fHasMemo = true;
        pArea->fTableEncrypted = true;
        pArea->bCryptType = DB_CRYPT_SIX;
        pArea->bMemoType = DB_MEMO_SMT;
        break;

      case 0xF6:
        pArea->fHasMemo = true;
        pArea->fTableEncrypted = true;
        pArea->bCryptType = DB_CRYPT_SIX;
        pArea->bMemoType = DB_MEMO_FPT;
        break;

      default:
        errCode = EDBF_CORRUPT;
      }
      if (errCode == Harbour::SUCCESS)
      {
        break;
      }
    }
  } while (hb_dbfErrorRT(pArea, hb_dbfGetEGcode(errCode), errCode, pArea->szDataFileName, hb_fsError(),
                         EF_CANRETRY | EF_CANDEFAULT, &pError) == E_RETRY);
  if (pError)
  {
    hb_itemRelease(pError);
  }

  if (errCode != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  pArea->uiHeaderLen = HB_GET_LE_UINT16(pArea->dbfHeader.uiHeaderLen);
  pArea->ulRecCount = HB_GET_LE_UINT32(pArea->dbfHeader.ulRecCount);

  return Harbour::SUCCESS;
}

// Write the database file header record in the WorkArea.
static HB_ERRCODE hb_dbfWriteDBHeader(DBFAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfWriteDBHeader(%p)", static_cast<void*>(pArea)));
#endif

  int iYear, iMonth, iDay;
  auto fLck = false;
  HB_ERRCODE errCode;

  if (pArea->fReadonly)
  {
    hb_dbfErrorRT(pArea, EG_READONLY, EDBF_READONLY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  pArea->dbfHeader.bHasTags = pArea->fHasTags ? 0x01 : 0x00;
  if (pArea->bTableType == DB_DBF_VFP)
  {
    pArea->dbfHeader.bVersion = (pArea->fAutoInc ? 0x31 : 0x30);
    if (pArea->fHasMemo && pArea->bMemoType == DB_MEMO_FPT)
    {
      pArea->dbfHeader.bHasTags |= 0x02;
    }
  }
  else
  {
    pArea->dbfHeader.bVersion = 0x03;
    if (pArea->fHasMemo)
    {
      if (pArea->bMemoType == DB_MEMO_DBT)
      {
        pArea->dbfHeader.bVersion = 0x83;
      }
      else if (pArea->bMemoType == DB_MEMO_FPT)
      {
        pArea->dbfHeader.bVersion = 0xF5;
      }
      else if (pArea->bMemoType == DB_MEMO_SMT)
      {
        pArea->dbfHeader.bVersion = 0xE5;
      }
    }
    if (pArea->fTableEncrypted && pArea->bCryptType == DB_CRYPT_SIX)
    {
      pArea->dbfHeader.bVersion = (pArea->dbfHeader.bVersion & 0xf0) | 0x06;
    }
  }

  hb_dateToday(&iYear, &iMonth, &iDay);
  pArea->dbfHeader.bYear = static_cast<HB_BYTE>(
      pArea->bTableType == DB_DBF_STD && (pArea->uiSetHeader & DB_SETHEADER_YYEAR) == 0 ? iYear - 1900 : iYear % 100);
  pArea->dbfHeader.bMonth = static_cast<HB_BYTE>(iMonth);
  pArea->dbfHeader.bDay = static_cast<HB_BYTE>(iDay);

  // Update record count
  if (pArea->fShared)
  {
    if (!pArea->fHeaderLocked)
    {
      if (SELF_RAWLOCK(&pArea->area, HEADER_LOCK, 0) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      fLck = true;
    }
    pArea->ulRecCount = hb_dbfCalcRecCount(pArea);
  }

  HB_PUT_LE_UINT32(pArea->dbfHeader.ulRecCount, pArea->ulRecCount);
  HB_PUT_LE_UINT16(pArea->dbfHeader.uiHeaderLen, pArea->uiHeaderLen);
  HB_PUT_LE_UINT16(pArea->dbfHeader.uiRecordLen, pArea->uiRecordLen);
  if (hb_fileWriteAt(pArea->pDataFile, &pArea->dbfHeader, sizeof(DBFHEADER), 0) == sizeof(DBFHEADER))
  {
    errCode = Harbour::SUCCESS;
    if (!pArea->fShared || (pArea->uiSetHeader & DB_SETHEADER_EOL) != 0)
    {
      // write eof mark
      HB_FOFFSET nOffset = static_cast<HB_FOFFSET>(pArea->uiHeaderLen) +
                           static_cast<HB_FOFFSET>(pArea->uiRecordLen) * static_cast<HB_FOFFSET>(pArea->ulRecCount);
      if (hb_fileWriteAt(pArea->pDataFile, "\032", 1, nOffset) == 1)
      {
        hb_fileTruncAt(pArea->pDataFile, nOffset + 1);
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
    }
  }
  else
  {
    errCode = Harbour::FAILURE;
  }

  pArea->fDataFlush = true;
  pArea->fUpdateHeader = false;
  if (fLck)
  {
    if (SELF_RAWLOCK(&pArea->area, HEADER_UNLOCK, 0) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (errCode != Harbour::SUCCESS)
  {
    hb_dbfErrorRT(pArea, EG_WRITE, EDBF_WRITE, pArea->szDataFileName, hb_fsError(), 0, nullptr);
  }

  return errCode;
}

static HB_ERRCODE hb_dbfDrop(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfDrop(%p,%p,%p,%lu)", static_cast<void*>(pRDD), static_cast<void*>(pItemTable), static_cast<void*>(pItemIndex), ulConnect));
#endif

  char szFileName[HB_PATH_MAX];
  const char *szExt;
  PHB_ITEM pFileExt = nullptr;
  PHB_FNAME pFileName;
  auto fTable = false;
  auto fResult = false;

  auto szFile = hb_itemGetCPtr(pItemIndex);
  if (!szFile[0])
  {
    // Try to delete index file
    szFile = hb_itemGetCPtr(pItemTable);
    if (!szFile[0])
    {
      return Harbour::FAILURE;
    }
    fTable = true;
  }

  pFileName = hb_fsFNameSplit(szFile);

  if (!pFileName->szExtension && (!fTable || hb_setGetDefExtension()))
  {
    // Add default extension if missing
    pFileExt = hb_itemPutNil(pFileExt);
    if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  // Use hb_fileExists() first to locate table which can be in different path
  if (hb_fileExists(szFileName, szFileName))
  {
    fResult = hb_fileDelete(szFileName);
    if (fResult && fTable)
    {
      // Database table file has been deleted, now check if memo is
      // supported and if yes then try to delete memo file if it exists
      // in the same directory as table file
      // hb_fsFNameSplit() repeated intentionally to respect
      // the path set by hb_FileExists()
      pFileName = hb_fsFNameSplit(szFileName);
      pFileExt = hb_itemPutNil(pFileExt);
      if (SELF_RDDINFO(pRDD, RDDI_MEMOEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fileDelete(szFileName);
        }
      }
      // and try to delete production index also if it exists
      // in the same directory as table file
      hb_itemClear(pFileExt);
      if (SELF_RDDINFO(pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fileDelete(szFileName);
        }
      }
      hb_xfree(pFileName);
    }
  }

  if (pFileExt)
  {
    hb_itemRelease(pFileExt);
  }

  return fResult ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE hb_dbfExists(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfExists(%p,%p,%p,%lu)", static_cast<void*>(pRDD), static_cast<void*>(pItemTable), static_cast<void*>(pItemIndex), ulConnect));
#endif

  char szFileName[HB_PATH_MAX];
  PHB_ITEM pFileExt = nullptr;
  PHB_FNAME pFileName;
  auto fTable = false;

  auto szFile = hb_itemGetCPtr(pItemIndex);
  if (!szFile[0])
  {
    szFile = hb_itemGetCPtr(pItemTable);
    if (!szFile[0])
    {
      return Harbour::FAILURE;
    }
    fTable = true;
  }

  pFileName = hb_fsFNameSplit(szFile);

  if (!pFileName->szExtension && (!fTable || hb_setGetDefExtension()))
  {
    pFileExt = hb_itemPutNil(pFileExt);
    if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  if (pFileExt)
  {
    hb_itemRelease(pFileExt);
  }

  return hb_fileExists(szFileName, szFileName) ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE hb_dbfRename(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, PHB_ITEM pItemNew,
                               HB_ULONG ulConnect)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRename(%p,%p,%p,%p,%lu)", static_cast<void*>(pRDD), static_cast<void*>(pItemTable), static_cast<void*>(pItemIndex), static_cast<void*>(pItemNew), ulConnect));
#endif

  char szFileName[HB_PATH_MAX];
  const char *szExt;
  PHB_ITEM pFileExt = nullptr;
  PHB_FNAME pFileName;
  auto fTable = false;
  auto fResult = false;

  auto szFile = hb_itemGetCPtr(pItemIndex);
  if (!szFile[0])
  {
    // Try to delete index file
    szFile = hb_itemGetCPtr(pItemTable);
    if (!szFile[0])
    {
      return Harbour::FAILURE;
    }
    fTable = true;
  }

  pFileName = hb_fsFNameSplit(szFile);

  if (!pFileName->szExtension && (!fTable || hb_setGetDefExtension()))
  {
    // Add default extension if missing
    pFileExt = hb_itemPutNil(pFileExt);
    if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  szFile = hb_itemGetCPtr(pItemNew);
  // Use hb_fileExists() first to locate table which can be in different path
  if (szFile[0] && hb_fileExists(szFileName, szFileName))
  {
    char szFileNew[HB_PATH_MAX];
    PHB_FNAME pFileNameNew;

    // hb_fsFNameSplit() repeated intentionally to respect
    // the path set by hb_FileExists()
    pFileName = hb_fsFNameSplit(szFileName);

    pFileNameNew = hb_fsFNameSplit(szFile);
    if (!pFileNameNew->szExtension && (!fTable || hb_setGetDefExtension()))
    {
      // Add default extension if missing
      pFileExt = hb_itemPutNil(pFileExt);
      if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        pFileNameNew->szExtension = hb_itemGetCPtr(pFileExt);
      }
    }
    if (!pFileNameNew->szPath)
    {
      pFileNameNew->szPath = pFileName->szPath;
    }
    hb_fsFNameMerge(szFileNew, pFileNameNew);

    fResult = hb_fileRename(szFileName, szFileNew);
    if (fResult && fTable)
    {
      // Database table file has been renamed, now check if memo is
      // supported and if yes then try to rename memo file if it exists
      // in the same directory as table file
      pFileExt = hb_itemPutNil(pFileExt);
      if (SELF_RDDINFO(pRDD, RDDI_MEMOEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          pFileNameNew->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fsFNameMerge(szFileNew, pFileNameNew);
          hb_fileRename(szFileName, szFileNew);
        }
      }
      // and try to rename production index also if it exists
      // in the same directory as table file
      hb_itemClear(pFileExt);
      if (SELF_RDDINFO(pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          pFileNameNew->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fsFNameMerge(szFileNew, pFileNameNew);
          hb_fileRename(szFileName, szFileNew);
        }
      }
    }
    hb_xfree(pFileName);
    hb_xfree(pFileNameNew);
  }

  if (pFileExt)
  {
    hb_itemRelease(pFileExt);
  }

  return fResult ? Harbour::SUCCESS : Harbour::FAILURE;
}

static void hb_dbfInitTSD(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfInitTSD(%p)", Cargo));
#endif

  (static_cast<LPDBFDATA>(Cargo))->bTableType = DB_DBF_STD;
  (static_cast<LPDBFDATA>(Cargo))->bCryptType = DB_CRYPT_NONE;
  (static_cast<LPDBFDATA>(Cargo))->uiDirtyRead = HB_IDXREAD_CLEANMASK;
  (static_cast<LPDBFDATA>(Cargo))->uiSetHeader = DB_SETHEADER_APPENDSYNC;
}

static void hb_dbfDestroyTSD(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfDestroyTSD(%p)", Cargo));
#endif

  LPDBFDATA pData;

  pData = static_cast<LPDBFDATA>(Cargo);

  if (pData->szTrigger)
  {
    hb_xfree(pData->szTrigger);
  }
  if (pData->szPendingTrigger)
  {
    hb_xfree(pData->szPendingTrigger);
  }
  if (pData->szPasswd)
  {
    hb_xfree(pData->szPasswd);
  }
  if (pData->szPendingPasswd)
  {
    hb_xfree(pData->szPendingPasswd);
  }
}

static HB_ERRCODE hb_dbfInit(LPRDDNODE pRDD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfInit(%p)", static_cast<void*>(pRDD)));
#endif

  auto pTSD = static_cast<PHB_TSD>(hb_xgrab(sizeof(HB_TSD)));
  HB_TSD_INIT(pTSD, sizeof(DBFDATA), hb_dbfInitTSD, hb_dbfDestroyTSD);
  pRDD->lpvCargo = static_cast<void *>(pTSD);

  if (ISSUPER_INIT(pRDD))
  {
    return SUPER_INIT(pRDD);
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

static HB_ERRCODE hb_dbfExit(LPRDDNODE pRDD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfExit(%p)", static_cast<void*>(pRDD)));
#endif

  if (pRDD->lpvCargo)
  {
    hb_stackReleaseTSD(static_cast<PHB_TSD>(pRDD->lpvCargo));
    hb_xfree(pRDD->lpvCargo);
    pRDD->lpvCargo = nullptr;
  }
  s_uiRddId = static_cast<HB_USHORT>(-1);

  if (ISSUPER_EXIT(pRDD))
  {
    return SUPER_EXIT(pRDD);
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

static HB_ERRCODE hb_dbfRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbfRddInfo(%p, %hu, %lu, %p)", static_cast<void*>(pRDD), uiIndex, ulConnect, pItem));
#endif

  LPDBFDATA pData;

  pData = DBFNODE_DATA(pRDD);

  switch (uiIndex)
  {
  case RDDI_ISDBF:
  case RDDI_CANPUTREC:
  case RDDI_LOCAL:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_TABLEEXT:
  {
    auto szNew = hb_itemGetCPtr(pItem);
    char *szNewVal;

    szNewVal = szNew[0] == '.' && szNew[1] ? hb_strdup(szNew) : nullptr;
    hb_itemPutC(pItem, pData->szTableExt[0] ? pData->szTableExt : DBF_TABLEEXT);
    if (szNewVal != nullptr)
    {
      hb_strncpy(pData->szTableExt, szNewVal, sizeof(pData->szTableExt) - 1);
      hb_xfree(szNewVal);
    }
    break;
  }
  case RDDI_TABLETYPE:
  {
    int iType = hb_itemGetNI(pItem);
    hb_itemPutNI(pItem, pData->bTableType ? pData->bTableType : DB_DBF_STD);
    switch (iType)
    {
    case DB_DBF_STD: // standard dBase/Clipper DBF file
    case DB_DBF_VFP: // VFP DBF file
      pData->bTableType = static_cast<HB_BYTE>(iType);
    }
    break;
  }
  case RDDI_LOCKSCHEME:
  {
    int iScheme = hb_itemGetNI(pItem);

    hb_itemPutNI(pItem, pData->bLockType ? pData->bLockType : hb_setGetDBFLockScheme());
    switch (iScheme)
    {
    case DB_DBFLOCK_CLIPPER:
    case DB_DBFLOCK_CLIPPER2:
    case DB_DBFLOCK_COMIX:
    case DB_DBFLOCK_VFP:
    case DB_DBFLOCK_HB32:
#ifndef HB_LONG_LONG_OFF
    case DB_DBFLOCK_HB64:
#endif
      pData->bLockType = static_cast<HB_BYTE>(iScheme);
    }
    break;
  }
  case RDDI_SETHEADER:
  {
    HB_USHORT uiSetHeader = pData->uiSetHeader;

    if (pItem->isNumeric())
    {
      int iMode = pItem->getNI();
      if ((iMode & ~DB_SETHEADER_MASK) == 0)
      {
        pData->uiSetHeader = static_cast<HB_USHORT>(iMode);
      }
    }
    hb_itemPutNI(pItem, uiSetHeader);
    break;
  }
  case RDDI_DIRTYREAD:
  {
    bool fDirty = (pData->uiDirtyRead == HB_IDXREAD_DIRTYMASK);
    if (pItem->isLogical())
    {
      pData->uiDirtyRead = pItem->getL() ? HB_IDXREAD_DIRTYMASK : HB_IDXREAD_CLEANMASK;
    }
    hb_itemPutL(pItem, fDirty);
    break;
  }
  case RDDI_INDEXPAGESIZE:
  {
    int iPageSize = hb_itemGetNI(pItem);

    hb_itemPutNI(pItem, pData->uiIndexPageSize);
    if (iPageSize >= 0x200 && iPageSize <= 0x2000 && ((iPageSize - 1) & iPageSize) == 0)
    {
      pData->uiIndexPageSize = static_cast<HB_USHORT>(iPageSize);
    }
    break;
  }
  case RDDI_DECIMALS:
  {
    int iDecimals = pItem->isNumeric() ? pItem->getNI() : -1;

    hb_itemPutNI(pItem, pData->bDecimals);
    if (iDecimals >= 0 && iDecimals <= 20)
    {
      pData->bDecimals = static_cast<HB_BYTE>(iDecimals);
    }
    break;
  }
  case RDDI_TRIGGER:
  {
    char *szTrigger = pData->szTrigger;
    auto fFree = false;

    if (pItem->isString())
    {
      fFree = true;
      pData->szTrigger = pItem->getCLen() > 0 ? pItem->getC() : nullptr;
    }

    if (fFree && szTrigger)
    {
      hb_itemPutCPtr(pItem, szTrigger);
    }
    else
    {
      hb_itemPutC(pItem, szTrigger);
    }

    if (!szTrigger && !fFree)
    {
      return Harbour::FAILURE;
    }

    break;
  }
  case RDDI_PENDINGTRIGGER:
    if (pItem->isString())
    {
      if (pData->szPendingTrigger)
      {
        hb_xfree(pData->szPendingTrigger);
        pData->szPendingTrigger = nullptr;
      }
      if (pItem->getCLen() > 0)
      {
        pData->szPendingTrigger = pItem->getC();
      }
    }
    else if (pData->szPendingTrigger)
    {
      hb_itemPutCPtr(pItem, pData->szPendingTrigger);
      pData->szPendingTrigger = nullptr;
    }
    else
    {
      return Harbour::FAILURE;
    }
    break;

  case RDDI_PASSWORD:
  {
    char *szPasswd = pData->szPasswd;
    auto fFree = false;

    if (pItem->isString())
    {
      fFree = true;
      pData->szPasswd = pItem->getCLen() > 0 ? pItem->getC() : nullptr;
    }

    if (fFree && szPasswd)
    {
      hb_itemPutCPtr(pItem, szPasswd);
    }
    else
    {
      hb_itemPutC(pItem, szPasswd);
    }

    if (!szPasswd && !fFree)
    {
      return Harbour::FAILURE;
    }

    break;
  }
  case RDDI_PENDINGPASSWORD:
    if (pItem->isString())
    {
      if (pData->szPendingPasswd)
      {
        hb_xfree(pData->szPendingPasswd);
        pData->szPendingPasswd = nullptr;
      }
      if (pItem->getCLen() > 0)
      {
        pData->szPendingPasswd = pItem->getC();
      }
    }
    else if (pData->szPendingPasswd)
    {
      hb_itemPutCPtr(pItem, pData->szPendingPasswd);
      pData->szPendingPasswd = nullptr;
    }
    else
    {
      return Harbour::FAILURE;
    }
    break;

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

#define hb_dbfWhoCares nullptr

static const RDDFUNCS dbfTable = {(DBENTRYP_BP)hb_dbfBof,
                                  (DBENTRYP_BP)hb_dbfEof,
                                  (DBENTRYP_BP)hb_dbfFound,
                                  (DBENTRYP_V)hb_dbfGoBottom,
                                  (DBENTRYP_UL)hb_dbfGoTo,
                                  (DBENTRYP_I)hb_dbfGoToId,
                                  (DBENTRYP_V)hb_dbfGoTop,
                                  (DBENTRYP_BIB)hb_dbfSeek,
                                  (DBENTRYP_L)hb_dbfSkip,
                                  (DBENTRYP_L)hb_dbfSkipFilter,
                                  (DBENTRYP_L)hb_dbfSkipRaw,
                                  (DBENTRYP_VF)hb_dbfAddField,
                                  (DBENTRYP_B)hb_dbfAppend,
                                  (DBENTRYP_I)hb_dbfCreateFields,
                                  (DBENTRYP_V)hb_dbfDeleteRec,
                                  (DBENTRYP_BP)hb_dbfDeleted,
                                  (DBENTRYP_SP)hb_dbfFieldCount,
                                  (DBENTRYP_VF)hb_dbfFieldDisplay,
                                  (DBENTRYP_SSI)hb_dbfFieldInfo,
                                  (DBENTRYP_SCP)hb_dbfFieldName,
                                  (DBENTRYP_V)hb_dbfFlush,
                                  (DBENTRYP_PP)hb_dbfGetRec,
                                  (DBENTRYP_SI)hb_dbfGetValue,
                                  (DBENTRYP_SVL)hb_dbfGetVarLen,
                                  (DBENTRYP_V)hb_dbfGoCold,
                                  (DBENTRYP_V)hb_dbfGoHot,
                                  (DBENTRYP_P)hb_dbfPutRec,
                                  (DBENTRYP_SI)hb_dbfPutValue,
                                  (DBENTRYP_V)hb_dbfRecall,
                                  (DBENTRYP_ULP)hb_dbfRecCount,
                                  (DBENTRYP_ISI)hb_dbfRecInfo,
                                  (DBENTRYP_ULP)hb_dbfRecNo,
                                  (DBENTRYP_I)hb_dbfRecId,
                                  (DBENTRYP_S)hb_dbfSetFieldExtent,
                                  (DBENTRYP_CP)hb_dbfAlias,
                                  (DBENTRYP_V)hb_dbfClose,
                                  (DBENTRYP_VO)hb_dbfCreate,
                                  (DBENTRYP_SI)hb_dbfInfo,
                                  (DBENTRYP_V)hb_dbfNewArea,
                                  (DBENTRYP_VO)hb_dbfOpen,
                                  (DBENTRYP_V)hb_dbfRelease,
                                  (DBENTRYP_SP)hb_dbfStructSize,
                                  (DBENTRYP_CP)hb_dbfSysName,
                                  (DBENTRYP_VEI)hb_dbfEval,
                                  (DBENTRYP_V)hb_dbfPack,
                                  (DBENTRYP_LSP)hb_dbfPackRec,
                                  (DBENTRYP_VS)hb_dbfSort,
                                  (DBENTRYP_VT)hb_dbfTrans,
                                  (DBENTRYP_VT)hb_dbfTransRec,
                                  (DBENTRYP_V)hb_dbfZap,
                                  (DBENTRYP_VR)hb_dbfChildEnd,
                                  (DBENTRYP_VR)hb_dbfChildStart,
                                  (DBENTRYP_VR)hb_dbfChildSync,
                                  (DBENTRYP_V)hb_dbfSyncChildren,
                                  (DBENTRYP_V)hb_dbfClearRel,
                                  (DBENTRYP_V)hb_dbfForceRel,
                                  (DBENTRYP_SSP)hb_dbfRelArea,
                                  (DBENTRYP_VR)hb_dbfRelEval,
                                  (DBENTRYP_SI)hb_dbfRelText,
                                  (DBENTRYP_VR)hb_dbfSetRel,
                                  (DBENTRYP_VOI)hb_dbfOrderListAdd,
                                  (DBENTRYP_V)hb_dbfOrderListClear,
                                  (DBENTRYP_VOI)hb_dbfOrderListDelete,
                                  (DBENTRYP_VOI)hb_dbfOrderListFocus,
                                  (DBENTRYP_V)hb_dbfOrderListRebuild,
                                  (DBENTRYP_VOO)hb_dbfOrderCondition,
                                  (DBENTRYP_VOC)hb_dbfOrderCreate,
                                  (DBENTRYP_VOI)hb_dbfOrderDestroy,
                                  (DBENTRYP_SVOI)hb_dbfOrderInfo,
                                  (DBENTRYP_V)hb_dbfClearFilter,
                                  (DBENTRYP_V)hb_dbfClearLocate,
                                  (DBENTRYP_V)hb_dbfClearScope,
                                  (DBENTRYP_VPLP)hb_dbfCountScope,
                                  (DBENTRYP_I)hb_dbfFilterText,
                                  (DBENTRYP_SI)hb_dbfScopeInfo,
                                  (DBENTRYP_VFI)hb_dbfSetFilter,
                                  (DBENTRYP_VLO)hb_dbfSetLocate,
                                  (DBENTRYP_VOS)hb_dbfSetScope,
                                  (DBENTRYP_VPL)hb_dbfSkipScope,
                                  (DBENTRYP_B)hb_dbfLocate,
                                  (DBENTRYP_CC)hb_dbfCompile,
                                  (DBENTRYP_I)hb_dbfError,
                                  (DBENTRYP_I)hb_dbfEvalBlock,
                                  (DBENTRYP_VSP)hb_dbfRawLock,
                                  (DBENTRYP_VL)hb_dbfLock,
                                  (DBENTRYP_I)hb_dbfUnLock,
                                  (DBENTRYP_V)hb_dbfCloseMemFile,
                                  (DBENTRYP_VO)hb_dbfCreateMemFile,
                                  (DBENTRYP_SCCS)hb_dbfGetValueFile,
                                  (DBENTRYP_VO)hb_dbfOpenMemFile,
                                  (DBENTRYP_SCCS)hb_dbfPutValueFile,
                                  (DBENTRYP_V)hb_dbfReadDBHeader,
                                  (DBENTRYP_V)hb_dbfWriteDBHeader,
                                  (DBENTRYP_R)hb_dbfInit,
                                  (DBENTRYP_R)hb_dbfExit,
                                  (DBENTRYP_RVVL)hb_dbfDrop,
                                  (DBENTRYP_RVVL)hb_dbfExists,
                                  (DBENTRYP_RVVVL)hb_dbfRename,
                                  (DBENTRYP_RSLV)hb_dbfRddInfo,
                                  (DBENTRYP_SVP)hb_dbfWhoCares};

HB_FUNC(_DBF)
{
  ;
}

HB_FUNC_STATIC(DBF_GETFUNCTABLE)
{
  auto puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  auto pTable = static_cast<RDDFUNCS *>(hb_parptr(2));
  auto uiRddId = static_cast<HB_USHORT>(hb_parni(4));

#if 0
   HB_TRACE(HB_TR_DEBUG, ("DBF_GETFUNCTABLE(%p, %p)", static_cast<void*>(puiCount), static_cast<void*>(pTable)));
#endif

  if (pTable)
  {
    if (puiCount)
    {
      *puiCount = RDDFUNCSCOUNT;
    }
    HB_ERRCODE errCode = hb_rddInheritEx(pTable, &dbfTable, &dbfSuper, nullptr, nullptr);
    hb_retni(errCode);
    if (errCode == Harbour::SUCCESS)
    {
      // we successfully register our RDD so now we can initialize it
      // You may think that this place is RDD init statement, Druzus
      s_uiRddId = uiRddId;
    }
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

static void hb_dbfRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("DBF", RDT_FULL) > 1)
  {
    hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
  }
}

HB_INIT_SYMBOLS_BEGIN(dbf1__InitSymbols){"_DBF", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(_DBF)}, nullptr},
    {"DBF_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(DBF_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(dbf1__InitSymbols)

        HB_CALL_ON_STARTUP_BEGIN(_hb_dbf_rdd_init_) hb_vmAtInit(hb_dbfRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_dbf_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup dbf1__InitSymbols
#pragma startup _hb_dbf_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(dbf1__InitSymbols)                                                                                   \
  HB_DATASEG_FUNC(_hb_dbf_rdd_init_)
#include "hbiniseg.hpp"
#endif
