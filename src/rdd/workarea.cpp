//
// Default RDD module
//
// Copyright 1999 Bruno Cantero <bruno@issnet.net>
// Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// Copyright 2002 Horacio Roldan <harbour_ar@yahoo.com.ar> (hb_waCloseAux())
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
#include "hbapirdd.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbthread.hpp"
#include "hbset.hpp"

// Note for Harbour++ v2: use only std::mutex
#if defined(HB_USE_CPP_MUTEX)
#include <iostream>
#include <thread>
#include <mutex>
#endif

// -- BASIC RDD METHODS --

// Determine logical beginning of file.
static HB_ERRCODE hb_waBof(AREAP pArea, HB_BOOL *pBof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waBof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBof)));
#endif

  *pBof = pArea->fBof;
  return Harbour::SUCCESS;
}

// Determine logical end of file.
static HB_ERRCODE hb_waEof(AREAP pArea, HB_BOOL *pEof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waEof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pEof)));
#endif

  *pEof = pArea->fEof;
  return Harbour::SUCCESS;
}

// Determine outcome of the last search operation.
static HB_ERRCODE hb_waFound(AREAP pArea, HB_BOOL *pFound)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waFound(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFound)));
#endif

  *pFound = pArea->fFound;
  return Harbour::SUCCESS;
}

// Reposition cursor relative to current position.
static HB_ERRCODE hb_waSkip(AREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSkip(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  HB_LONG lSkip;

  // Flush record and exit
  if (lToSkip == 0)
  {
    return SELF_SKIPRAW(pArea, 0);
  }

  pArea->fTop = pArea->fBottom = false;

  if (lToSkip > 0)
  {
    lSkip = 1;
  }
  else
  {
    lSkip = -1;
    lToSkip *= -1;
  }
  while (--lToSkip >= 0)
  {
    if (SELF_SKIPRAW(pArea, lSkip) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    if (SELF_SKIPFILTER(pArea, lSkip) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    if (pArea->fBof || pArea->fEof)
    {
      break;
    }
  }

  // Update Bof and Eof flags
  if (lSkip < 0)
  {
    pArea->fEof = false;
  }
  else // ( lSkip > 0 )
  {
    pArea->fBof = false;
  }

  return Harbour::SUCCESS;
}

// Reposition cursor respecting any filter setting.
static HB_ERRCODE hb_waSkipFilter(AREAP pArea, HB_LONG lUpDown)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSkipFilter(%p, %ld)", static_cast<void*>(pArea), lUpDown));
#endif

  HB_BOOL fBottom, fDeleted;
  HB_ERRCODE errCode;

  if (pArea->dbfi.itmCobExpr == nullptr && !hb_setGetDeleted())
  {
    return Harbour::SUCCESS;
  }

  // Since lToSkip is passed to SkipRaw, it should never request more than
  // a single skip.
  // The implied purpose of hb_waSkipFilter() is to get off of a "bad" record
  // after a skip was performed, NOT to skip lToSkip filtered records.
  lUpDown = (lUpDown < 0 ? -1 : 1);

  // remember if we are here after SLEF_GOTOP()
  fBottom = pArea->fBottom;

  while (!pArea->fBof && !pArea->fEof)
  {
    // SET DELETED
    if (hb_setGetDeleted())
    {
      if (SELF_DELETED(pArea, &fDeleted) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      if (fDeleted)
      {
        if (SELF_SKIPRAW(pArea, lUpDown) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }
        continue;
      }
    }

    // SET FILTER TO
    if (pArea->dbfi.itmCobExpr)
    {
      if (SELF_EVALBLOCK(pArea, pArea->dbfi.itmCobExpr) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }

      if (pArea->valResult->isLogical() && !pArea->valResult->getL())
      {
        if (SELF_SKIPRAW(pArea, lUpDown) != Harbour::SUCCESS)
        {
          return Harbour::FAILURE;
        }
        continue;
      }
    }

    break;
  }

  // The only one situation when we should repos is backward skipping
  // if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
  // then GOEOF() if not then GOTOP()
  if (pArea->fBof && lUpDown < 0)
  {
    if (fBottom)
    {
      // GOTO EOF (phantom) record -
      // this is the only one place where GOTO is used by Harbour
      // directly and RDD which does not operate on numbers should
      // serve this method only as SELF_GOEOF() synonym. If it's a
      // problem then we can remove this if and always use SELF_GOTOP()
      // but it also means second table scan if all records filtered
      // are out of filter so I do not want to do that. I will prefer
      // explicit add SELF_GOEOF() method
      errCode = SELF_GOTO(pArea, 0);
    }
    else
    {
      errCode = SELF_GOTOP(pArea);
      pArea->fBof = true;
    }
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }

  return errCode;
}

// Add a field to the WorkArea.
static HB_ERRCODE hb_waAddField(AREAP pArea, LPDBFIELDINFO pFieldInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waAddField(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFieldInfo)));
#endif

  LPFIELD pField;
  char szFieldName[HB_SYMBOL_NAME_LEN + 1];
  const char *szPtr;

  // Validate the name of field
  szPtr = pFieldInfo->atomName;
  while (HB_ISSPACE(*szPtr))
  {
    ++szPtr;
  }
  hb_strncpyUpperTrim(szFieldName, szPtr, HB_MIN(HB_SYMBOL_NAME_LEN, pArea->uiMaxFieldNameLength));
  if (szFieldName[0] == 0)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->lpFields + pArea->uiFieldCount;
  if (pArea->uiFieldCount > 0)
  {
    (static_cast<LPFIELD>(pField - 1))->lpfNext = pField;
  }
  pField->sym = static_cast<void *>(hb_dynsymGetCase(szFieldName));
  pField->uiType = pFieldInfo->uiType;
  pField->uiTypeExtended = pFieldInfo->uiTypeExtended;
  pField->uiLen = pFieldInfo->uiLen;
  pField->uiDec = pFieldInfo->uiDec;
  pField->uiFlags = pFieldInfo->uiFlags;
  pField->uiArea = pArea->uiArea;
  pArea->uiFieldCount++;

  return Harbour::SUCCESS;
}

// Add all fields defined in an array to the WorkArea.
static HB_ERRCODE hb_waCreateFields(AREAP pArea, PHB_ITEM pStruct)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waCreateFields(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pStruct)));
#endif

  HB_USHORT uiItems;
  HB_ERRCODE errCode = Harbour::SUCCESS;
  DBFIELDINFO dbFieldInfo;

  uiItems = static_cast<HB_USHORT>(hb_arrayLen(pStruct));
  if (SELF_SETFIELDEXTENT(pArea, uiItems) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  for (HB_USHORT uiCount = 0; uiCount < uiItems; uiCount++)
  {
    HB_USHORT uiLen, uiDec;
    int iData;

    dbFieldInfo.uiTypeExtended = 0;
    auto pFieldDesc = hb_arrayGetItemPtr(pStruct, uiCount + 1);
    dbFieldInfo.atomName = hb_arrayGetCPtr(pFieldDesc, DBS_NAME);
    iData = hb_arrayGetNI(pFieldDesc, DBS_LEN);
    if (iData < 0)
    {
      iData = 0;
    }
    uiLen = dbFieldInfo.uiLen = static_cast<HB_USHORT>(iData);
    iData = hb_arrayGetNI(pFieldDesc, DBS_DEC);
    if (iData < 0)
    {
      iData = 0;
    }
    uiDec = static_cast<HB_USHORT>(iData);
    dbFieldInfo.uiDec = 0;
    auto szType = hb_arrayGetCPtr(pFieldDesc, DBS_TYPE);
    iData = HB_TOUPPER(*szType);
#ifdef DBS_FLAG
    dbFieldInfo.uiFlags = hb_arrayGetNI(pFieldDesc, DBS_FLAG);
#else
    dbFieldInfo.uiFlags = 0;
    while (*++szType)
    {
      if (*szType == ':')
      {
        while (*++szType)
        {
          switch (HB_TOUPPER(*szType))
          {
          case 'N':
            dbFieldInfo.uiFlags |= HB_FF_NULLABLE;
            break;
          case 'B':
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            break;
          case '+':
            dbFieldInfo.uiFlags |= HB_FF_AUTOINC;
            break;
          case 'Z':
            dbFieldInfo.uiFlags |= HB_FF_COMPRESSED;
            break;
          case 'E':
            dbFieldInfo.uiFlags |= HB_FF_ENCRYPTED;
            break;
          case 'U':
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            break;
          }
        }
        break;
      }
    }
#endif
    switch (iData)
    {
    case 'C':
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      dbFieldInfo.uiLen = uiLen;
      // Too many people reported the behavior with code below as a
      // Clipper compatibility bug so I commented this code, Druzus.
#if 0
#ifdef HB_CLP_STRICT
                  dbFieldInfo.uiLen = uiLen;
#else
                  dbFieldInfo.uiLen = uiLen + uiDec * 256;
#endif
#endif
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'L':
      dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
      dbFieldInfo.uiLen = 1;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case 'D':
      dbFieldInfo.uiType = Harbour::DB::Field::DATE;
      dbFieldInfo.uiLen = (uiLen == 3 || uiLen == 4) ? uiLen : 8;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case 'I':
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
      dbFieldInfo.uiLen = ((uiLen > 0 && uiLen <= 4) || uiLen == 8) ? uiLen : 4;
      dbFieldInfo.uiDec = uiDec;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case 'Y':
      dbFieldInfo.uiType = Harbour::DB::Field::CURRENCY;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiDec = 4;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case 'Z':
      dbFieldInfo.uiType = Harbour::DB::Field::CURDOUBLE;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiDec = uiDec;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case '2':
    case '4':
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
      dbFieldInfo.uiLen = static_cast<HB_USHORT>(iData - '0');
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case 'B':
    case '8':
      dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiDec = uiDec;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case 'N':
      dbFieldInfo.uiType = Harbour::DB::Field::LONG;
      dbFieldInfo.uiDec = uiDec;
      // dBase documentation defines maximum numeric field size as 20
      // but Clipper allows to create longer fields so I remove this
      // limit, Druzus
#if 0
      if (uiLen > 20)
#endif
      if (uiLen > 255)
      {
        errCode = Harbour::FAILURE;
      }
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case 'F':
      dbFieldInfo.uiType = Harbour::DB::Field::FLOAT;
      dbFieldInfo.uiDec = uiDec;
      // see note above
      if (uiLen > 255)
      {
        errCode = Harbour::FAILURE;
      }
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case 'T':
      if (uiLen == 8)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
        dbFieldInfo.uiLen = 8;
      }
      else
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIME;
        dbFieldInfo.uiLen = 4;
      }
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case '@':
      dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case '=':
      dbFieldInfo.uiType = Harbour::DB::Field::MODTIME;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiFlags = 0;
      break;

    case '^':
      dbFieldInfo.uiType = Harbour::DB::Field::ROWVER;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiFlags = 0;
      break;

    case '+':
      dbFieldInfo.uiType = Harbour::DB::Field::AUTOINC;
      dbFieldInfo.uiLen = 4;
      dbFieldInfo.uiFlags = 0;
      break;

    case 'Q':
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      dbFieldInfo.uiLen = uiLen > 255 ? 255 : (uiLen == 0 ? 1 : uiLen);
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'V':
      dbFieldInfo.uiType = Harbour::DB::Field::ANY;
      dbFieldInfo.uiLen = (uiLen < 3 || uiLen == 5) ? 6 : uiLen;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'M':
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
      dbFieldInfo.uiLen = (uiLen == 4) ? 4 : 10;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'P':
      dbFieldInfo.uiType = Harbour::DB::Field::IMAGE;
      dbFieldInfo.uiLen = (uiLen == 4) ? 4 : 10;
      dbFieldInfo.uiFlags &= HB_FF_BINARY;
      break;

    case 'W':
      dbFieldInfo.uiType = Harbour::DB::Field::BLOB;
      dbFieldInfo.uiLen = (uiLen == 4) ? 4 : 10;
      dbFieldInfo.uiFlags &= HB_FF_BINARY;
      break;

    case 'G':
      dbFieldInfo.uiType = Harbour::DB::Field::OLE;
      dbFieldInfo.uiLen = (uiLen == 4) ? 4 : 10;
      dbFieldInfo.uiFlags &= HB_FF_BINARY;
      break;

    default:
      errCode = Harbour::FAILURE;
      break;
    }

    if (errCode == Harbour::SUCCESS)
    {
      errCode = SELF_ADDFIELD(pArea, &dbFieldInfo); // Add field
    }

    if (errCode != Harbour::SUCCESS)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return errCode;
    }
  }
  return Harbour::SUCCESS;
}

// Determine the number of fields in the WorkArea.
static HB_ERRCODE hb_waFieldCount(AREAP pArea, HB_USHORT *uiFields)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldCount(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiFields)));
#endif

  *uiFields = pArea->uiFieldCount;
  return Harbour::SUCCESS;
}

// Retrieve information about a field.
static HB_ERRCODE hb_waFieldInfo(AREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldInfo(%p, %hu, %hu, %p)", static_cast<void*>(pArea), uiIndex, uiType, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;

  if (uiIndex > pArea->uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->lpFields + uiIndex - 1;
  switch (uiType)
  {
  case DBS_NAME:
    hb_itemPutC(pItem, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
    break;

  case DBS_TYPE:
  {
    HB_USHORT uiFlags = 0;
    char szType[8];
    char cType;
    int iLen = 0;

    switch (pField->uiType)
    {
    case Harbour::DB::Field::STRING:
      cType = 'C';
      uiFlags = HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_UNICODE | HB_FF_ENCRYPTED | HB_FF_COMPRESSED;
      break;

    case Harbour::DB::Field::LOGICAL:
      cType = 'L';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::DATE:
      cType = 'D';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::LONG:
      cType = 'N';
      uiFlags = HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case Harbour::DB::Field::INTEGER:
      cType = 'I';
      uiFlags = HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case Harbour::DB::Field::DOUBLE:
      cType = 'B';
      uiFlags = HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case Harbour::DB::Field::FLOAT:
      cType = 'F';
      uiFlags = HB_FF_NULLABLE | HB_FF_AUTOINC;
      break;

    case Harbour::DB::Field::TIME:
      cType = 'T';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::TIMESTAMP:
      cType = '@';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::MODTIME:
      cType = '=';
      break;

    case Harbour::DB::Field::ROWVER:
      cType = '^';
      break;

    case Harbour::DB::Field::AUTOINC:
      cType = '+';
      uiFlags = HB_FF_AUTOINC;
      break;

    case Harbour::DB::Field::CURRENCY:
      cType = 'Y';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::CURDOUBLE:
      cType = 'Z';
      uiFlags = HB_FF_NULLABLE;
      break;

    case Harbour::DB::Field::VARLENGTH:
      cType = 'Q';
      uiFlags = HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_UNICODE | HB_FF_ENCRYPTED | HB_FF_COMPRESSED;
      break;

    case Harbour::DB::Field::ANY:
      cType = 'V';
      uiFlags = HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_UNICODE | HB_FF_ENCRYPTED | HB_FF_COMPRESSED;
      break;

    case Harbour::DB::Field::MEMO:
      cType = 'M';
      uiFlags = HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_UNICODE | HB_FF_ENCRYPTED | HB_FF_COMPRESSED;
      break;

    case Harbour::DB::Field::IMAGE:
      cType = 'P';
      break;

    case Harbour::DB::Field::BLOB:
      cType = 'W';
      break;

    case Harbour::DB::Field::OLE:
      cType = 'G';
      break;

    default:
      cType = 'U';
      break;
    }
    szType[iLen++] = cType;
    uiFlags &= pField->uiFlags;
    if (uiFlags != 0)
    {
#ifndef DBS_FLAG
      szType[iLen++] = ':';
      if (uiFlags & HB_FF_NULLABLE)
      {
        szType[iLen++] = 'N';
      }
      if (uiFlags & HB_FF_BINARY)
      {
        szType[iLen++] = 'B';
      }
      if (uiFlags & HB_FF_AUTOINC)
      {
        szType[iLen++] = '+';
      }
      if (uiFlags & HB_FF_COMPRESSED)
      {
        szType[iLen++] = 'Z';
      }
      if (uiFlags & HB_FF_ENCRYPTED)
      {
        szType[iLen++] = 'E';
      }
      if (uiFlags & HB_FF_UNICODE)
      {
        szType[iLen++] = 'U';
      }
#endif
    }
    hb_itemPutCL(pItem, szType, iLen);
    break;
  }
  case DBS_LEN:
    hb_itemPutNL(pItem, pField->uiLen);
    break;

  case DBS_DEC:
    hb_itemPutNL(pItem, pField->uiDec);
    break;

#ifdef DBS_FLAG
  case DBS_FLAG:
    hb_itemPutNL(pItem, pField->uiFlags);
    break;
#endif

  default:
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

// Determine the name associated with a field number.
static HB_ERRCODE hb_waFieldName(AREAP pArea, HB_USHORT uiIndex, char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldName(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(szName)));
#endif

  LPFIELD pField;

  if (uiIndex > pArea->uiFieldExtent)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->lpFields + uiIndex - 1;
  hb_strncpy(szName, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)), pArea->uiMaxFieldNameLength);
  return Harbour::SUCCESS;
}

// Establish the extent of the array of fields for a WorkArea.
static HB_ERRCODE hb_waSetFieldExtent(AREAP pArea, HB_USHORT uiFieldExtent)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetFieldExtent(%p, %hu)", static_cast<void*>(pArea), uiFieldExtent));
#endif

  pArea->uiFieldExtent = uiFieldExtent;

  // Alloc field array
  if (uiFieldExtent)
  {
    pArea->lpFields = static_cast<LPFIELD>(hb_xgrabz(uiFieldExtent * sizeof(FIELD)));
  }

  return Harbour::SUCCESS;
}

// Obtain the alias of the WorkArea.
static HB_ERRCODE hb_waAlias(AREAP pArea, char *szAlias)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waAlias(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(szAlias)));
#endif

  hb_strncpy(szAlias,
             pArea->atomAlias && hb_dynsymAreaHandle(static_cast<PHB_DYNS>(pArea->atomAlias))
                 ? hb_dynsymName(static_cast<PHB_DYNS>(pArea->atomAlias))
                 : "",
             HB_RDD_MAX_ALIAS_LEN);

  return Harbour::SUCCESS;
}

// Close the table in the WorkArea.
static HB_ERRCODE hb_waClose(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waClose(%p)", static_cast<void*>(pArea)));
#endif

  // Clear items
  SELF_CLEARFILTER(pArea);
  SELF_CLEARREL(pArea);
  SELF_CLEARLOCATE(pArea);

  // Clear relations that has this area as a child
  hb_rddCloseAllParentRelations(pArea);

  if (pArea->atomAlias)
  {
    hb_dynsymSetAreaHandle(static_cast<PHB_DYNS>(pArea->atomAlias), 0);
  }

  return Harbour::SUCCESS;
}

// Retrieve information about the current driver.
static HB_ERRCODE hb_waInfo(AREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  switch (uiIndex)
  {
  case DBI_ISDBF:
  case DBI_CANPUTREC:
  case DBI_ISFLOCK:
  case DBI_SHARED:
  case DBI_TRANSREC:
    hb_itemPutL(pItem, false);
    break;

  // IMHO better to return Harbour::FAILURE to notice that it's not supported
  case DBI_GETDELIMITER:
  case DBI_SETDELIMITER:
  case DBI_SEPARATOR:
    hb_itemPutC(pItem, nullptr);
    return Harbour::FAILURE;

  case DBI_GETHEADERSIZE:
  case DBI_GETRECSIZE:
  case DBI_LOCKCOUNT:
    hb_itemPutNI(pItem, 0);
    break;

  case DBI_LASTUPDATE:
    hb_itemPutDL(pItem, 0);
    break;

  case DBI_GETLOCKARRAY:
    hb_arrayNew(pItem, 0);
    break;

  case DBI_CHILDCOUNT:
  {
    LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
    HB_USHORT uiCount = 0;
    while (lpdbRelations)
    {
      uiCount++;
      lpdbRelations = lpdbRelations->lpdbriNext;
    }
    hb_itemPutNI(pItem, uiCount);
    break;
  }

  case DBI_BOF:
    hb_itemPutL(pItem, pArea->fBof);
    break;

  case DBI_EOF:
    hb_itemPutL(pItem, pArea->fEof);
    break;

  case DBI_DBFILTER:
    if (pArea->dbfi.abFilterText)
    {
      hb_itemCopy(pItem, pArea->dbfi.abFilterText);
    }
    else
    {
      hb_itemPutC(pItem, nullptr);
    }
    break;

  case DBI_FOUND:
    hb_itemPutL(pItem, pArea->fFound);
    break;

  case DBI_FCOUNT:
    hb_itemPutNI(pItem, pArea->uiFieldCount);
    break;

  case DBI_ALIAS:
  {
    char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];
    if (SELF_ALIAS(pArea, szAlias) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    hb_itemPutC(pItem, szAlias);
    break;
  }

  case DBI_TABLEEXT:
  {
    LPRDDNODE pNode = SELF_RDDNODE(pArea);
    hb_itemClear(pItem);
    return pNode ? SELF_RDDINFO(pNode, RDDI_TABLEEXT, 0, pItem) : Harbour::FAILURE;
  }
  case DBI_SCOPEDRELATION:
  {
    int iRelNo = hb_itemGetNI(pItem);
    auto fScoped = false;

    if (iRelNo > 0)
    {
      LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
      while (lpdbRelations)
      {
        if (--iRelNo == 0)
        {
          fScoped = lpdbRelations->isScoped;
          break;
        }
        lpdbRelations = lpdbRelations->lpdbriNext;
      }
    }
    hb_itemPutL(pItem, fScoped);
    break;
  }
  case DBI_POSITIONED:
  {
    HB_ULONG ulRecCount, ulRecNo;
    if (SELF_RECNO(pArea, &ulRecNo) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    if (ulRecNo == 0)
    {
      hb_itemPutL(pItem, false);
    }
    else if (SELF_RECCOUNT(pArea, &ulRecCount) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    else
    {
      hb_itemPutL(pItem, ulRecNo != ulRecCount + 1);
    }
    break;
  }
  case DBI_CODEPAGE:
    hb_itemPutC(pItem, pArea->cdPage ? pArea->cdPage->id : nullptr);
    break;

  case DBI_RM_SUPPORTED:
    hb_itemPutL(pItem, false);
    break;

  case DBI_DB_VERSION:
    hb_itemPutC(pItem, nullptr);
    break;

  case DBI_RDD_VERSION:
    hb_itemPutC(pItem, nullptr);
    break;

  default:
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

// Retrieve information about the current order that SELF could not.
// Called by SELF_ORDINFO() if uiIndex is not supported.
static HB_ERRCODE hb_waOrderInfo(AREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waOrderInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pInfo)));
#endif

  HB_SYMBOL_UNUSED(pArea);
  HB_SYMBOL_UNUSED(uiIndex);

  if (pInfo->itmResult)
  {
    hb_itemClear(pInfo->itmResult);
  }

// CA-Cl*pper does not generate RT error when default ORDERINFO() method
// is called
#if 0
   hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
#endif

  return Harbour::FAILURE;
}

// Clear the WorkArea for use.
static HB_ERRCODE hb_waNewArea(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waNewArea(%p)", static_cast<void*>(pArea)));
#endif

  pArea->valResult = hb_itemNew(nullptr);
  pArea->lpdbRelations = nullptr;
  pArea->uiParents = 0;
  pArea->uiMaxFieldNameLength = HB_SYMBOL_NAME_LEN;

  return Harbour::SUCCESS;
}

// Open a data store in the WorkArea.
// Like in Clipper it's also mapped as Create() method at WA level
static HB_ERRCODE hb_waOpen(AREAP pArea, LPDBOPENINFO pInfo)
{
  if (!pArea->atomAlias && pInfo->atomAlias && pInfo->atomAlias[0])
  {
    pArea->atomAlias = hb_rddAllocWorkAreaAlias(pInfo->atomAlias, static_cast<int>(pInfo->uiArea));
    if (!pArea->atomAlias)
    {
      SELF_CLOSE(pArea);
      return Harbour::FAILURE;
    }
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_waOrderCondition(AREAP pArea, LPDBORDERCONDINFO param)
{
  if (pArea->lpdbOrdCondInfo)
  {
    if (pArea->lpdbOrdCondInfo->abFor)
    {
      hb_xfree(pArea->lpdbOrdCondInfo->abFor);
    }
    if (pArea->lpdbOrdCondInfo->abWhile)
    {
      hb_xfree(pArea->lpdbOrdCondInfo->abWhile);
    }
    if (pArea->lpdbOrdCondInfo->itmCobFor)
    {
      hb_itemRelease(pArea->lpdbOrdCondInfo->itmCobFor);
    }
    if (pArea->lpdbOrdCondInfo->itmCobWhile)
    {
      hb_itemRelease(pArea->lpdbOrdCondInfo->itmCobWhile);
    }
    if (pArea->lpdbOrdCondInfo->itmCobEval)
    {
      hb_itemRelease(pArea->lpdbOrdCondInfo->itmCobEval);
    }
    if (pArea->lpdbOrdCondInfo->itmStartRecID)
    {
      hb_itemRelease(pArea->lpdbOrdCondInfo->itmStartRecID);
    }
    if (pArea->lpdbOrdCondInfo->itmRecID)
    {
      hb_itemRelease(pArea->lpdbOrdCondInfo->itmRecID);
    }
    hb_xfree(pArea->lpdbOrdCondInfo);
  }
  pArea->lpdbOrdCondInfo = param;

  return Harbour::SUCCESS;
}

// Release all references to a WorkArea.
static HB_ERRCODE hb_waRelease(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waRelease(%p)", static_cast<void*>(pArea)));
#endif

  // Free all allocated pointers
  if (pArea->lpFields)
  {
    hb_xfree(pArea->lpFields);
  }
  if (pArea->valResult)
  {
    hb_itemRelease(pArea->valResult);
  }
  if (pArea->lpdbOrdCondInfo)
  {
    // intentionally direct call not a method
    hb_waOrderCondition(pArea, nullptr);
  }
  hb_xfree(pArea);
  return Harbour::SUCCESS;
}

// Retrieve the size of the WorkArea structure.
static HB_ERRCODE hb_waStructSize(AREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waStrucSize(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiSize)));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(AREA);
  return Harbour::SUCCESS;
}

// Obtain the name of replaceable database driver (RDD) subsystem.
static HB_ERRCODE hb_waSysName(AREAP pArea, char *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSysName(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBuffer)));
#endif

  hb_strncpy(pBuffer, SELF_RDDNODE(pArea)->szName, HB_RDD_MAX_DRIVERNAME_LEN);

  return Harbour::SUCCESS;
}

// Evaluate code block for each record in WorkArea.
static HB_ERRCODE hb_waEval(AREAP pArea, LPDBEVALINFO pEvalInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waEval(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pEvalInfo)));
#endif

  HB_LONG lNext = 1;
  HB_BOOL fEof;
  auto fFor = false;

  if (pEvalInfo->dbsci.itmRecID)
  {
    if (SELF_GOTOID(pArea, pEvalInfo->dbsci.itmRecID) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  else if (pEvalInfo->dbsci.lNext)
  {
    lNext = hb_itemGetNL(pEvalInfo->dbsci.lNext);
    if (lNext <= 0)
    {
      return Harbour::SUCCESS;
    }
  }
  else if (!pEvalInfo->dbsci.itmCobWhile && !hb_itemGetLX(pEvalInfo->dbsci.fRest))
  {
    if (SELF_GOTOP(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // TODO: use SKIPSCOPE() method and fRest parameter

  for (;;)
  {
    if (SELF_EOF(pArea, &fEof) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }

    if (fEof)
    {
      break;
    }

    if (pEvalInfo->dbsci.itmCobWhile)
    {
      if (SELF_EVALBLOCK(pArea, pEvalInfo->dbsci.itmCobWhile) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      if (!hb_itemGetLX(pArea->valResult))
      {
        break;
      }
    }

    if (pEvalInfo->dbsci.itmCobFor)
    {
      if (SELF_EVALBLOCK(pArea, pEvalInfo->dbsci.itmCobFor) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      fFor = hb_itemGetLX(pArea->valResult);
    }
    else
    {
      fFor = true;
    }

    if (fFor)
    {
      if (SELF_EVALBLOCK(pArea, pEvalInfo->itmBlock) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }

    if (pEvalInfo->dbsci.itmRecID || (pEvalInfo->dbsci.lNext && --lNext < 1))
    {
      break;
    }

    if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

// Locate a record which pass given condition
static HB_ERRCODE hb_waLocate(AREAP pArea, HB_BOOL fContinue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waLocate(%p, %d)", static_cast<void*>(pArea), fContinue));
#endif

  HB_LONG lNext = 1;
  HB_BOOL fEof;

  if (fContinue)
  {
    if (!pArea->dbsi.itmCobFor)
    {
      return Harbour::SUCCESS;
    }

    if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  else if (pArea->dbsi.itmRecID)
  {
    if (SELF_GOTOID(pArea, pArea->dbsi.itmRecID) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  else if (pArea->dbsi.lNext)
  {
    lNext = hb_itemGetNL(pArea->dbsi.lNext);
    if (lNext <= 0)
    {
      return Harbour::SUCCESS;
    }
  }
  else if (!pArea->dbsi.itmCobWhile && !hb_itemGetLX(pArea->dbsi.fRest))
  {
    if (SELF_GOTOP(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  pArea->fFound = false;

  // TODO: use SKIPSCOPE() method and fRest parameter

  for (;;)
  {
    if (SELF_EOF(pArea, &fEof) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }

    if (fEof)
    {
      break;
    }

    if (!fContinue && pArea->dbsi.itmCobWhile)
    {
      if (SELF_EVALBLOCK(pArea, pArea->dbsi.itmCobWhile) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      if (!hb_itemGetLX(pArea->valResult))
      {
        break;
      }
    }

    if (!pArea->dbsi.itmCobFor)
    {
      pArea->fFound = true;
      break;
    }
    else
    {
      if (SELF_EVALBLOCK(pArea, pArea->dbsi.itmCobFor) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }

      if (hb_itemGetLX(pArea->valResult))
      {
        pArea->fFound = true;
        break;
      }
    }

    if (!fContinue && (pArea->dbsi.itmRecID || (pArea->dbsi.lNext && --lNext < 1)))
    {
      break;
    }

    if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

// Copy one or more records from one WorkArea to another.
static HB_ERRCODE hb_waTrans(AREAP pArea, LPDBTRANSINFO pTransInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waTrans(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pTransInfo)));
#endif

  HB_LONG lNext = 1;
  HB_BOOL fEof;
  auto fFor = false;

  if (pTransInfo->dbsci.itmRecID)
  {
    if (SELF_GOTOID(pArea, pTransInfo->dbsci.itmRecID) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }
  else if (pTransInfo->dbsci.lNext)
  {
    lNext = hb_itemGetNL(pTransInfo->dbsci.lNext);
    if (lNext <= 0)
    {
      return Harbour::SUCCESS;
    }
  }
  else if (!pTransInfo->dbsci.itmCobWhile && !hb_itemGetLX(pTransInfo->dbsci.fRest))
  {
    if (SELF_GOTOP(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  // TODO: use SKIPSCOPE() method and fRest parameter

  for (;;)
  {
    if (SELF_EOF(pArea, &fEof) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }

    if (fEof)
    {
      break;
    }

    if (pTransInfo->dbsci.itmCobWhile)
    {
      if (SELF_EVALBLOCK(pArea, pTransInfo->dbsci.itmCobWhile) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      if (!hb_itemGetLX(pArea->valResult))
      {
        break;
      }
    }

    if (pTransInfo->dbsci.itmCobFor)
    {
      if (SELF_EVALBLOCK(pArea, pTransInfo->dbsci.itmCobFor) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
      fFor = hb_itemGetLX(pArea->valResult);
    }
    else
    {
      fFor = true;
    }

    if (fFor)
    {
      if (SELF_TRANSREC(pArea, pTransInfo) != Harbour::SUCCESS)
      {
        return Harbour::FAILURE;
      }
    }

    if (pTransInfo->dbsci.itmRecID || (pTransInfo->dbsci.lNext && --lNext < 1))
    {
      break;
    }

    if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

// Copy a record to another WorkArea.
static HB_ERRCODE hb_waTransRec(AREAP pArea, LPDBTRANSINFO pTransInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waTransRec(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pTransInfo)));
#endif

  HB_BOOL bDeleted;
  HB_BYTE *pRecord;
  HB_ERRCODE errCode;

  if (pTransInfo->uiFlags & DBTF_MATCH && pTransInfo->uiFlags & DBTF_PUTREC)
  {
    // Record deleted?
    errCode = SELF_DELETED(pArea, &bDeleted);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    errCode = SELF_GETREC(pArea, &pRecord);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    // Append a new record
    errCode = SELF_APPEND(pTransInfo->lpaDest, true);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    // Copy record
    errCode = SELF_PUTREC(pTransInfo->lpaDest, pRecord);
  }
  else
  {
    LPDBTRANSITEM pTransItem;
    PHB_ITEM pItem;

    if (pTransInfo->uiFlags & DBTF_RECALL)
    {
      bDeleted = false;
    }
    else
    {
      // Record deleted?
      errCode = SELF_DELETED(pArea, &bDeleted);
      if (errCode != Harbour::SUCCESS)
      {
        return errCode;
      }
    }

    // Append a new record
    errCode = SELF_APPEND(pTransInfo->lpaDest, true);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    pItem = hb_itemNew(nullptr);
    pTransItem = pTransInfo->lpTransItems;
    for (HB_USHORT uiCount = pTransInfo->uiItemCount; uiCount; --uiCount)
    {
      errCode = SELF_GETVALUE(pArea, pTransItem->uiSource, pItem);
      if (errCode != Harbour::SUCCESS)
      {
        break;
      }
      errCode = SELF_PUTVALUE(pTransInfo->lpaDest, pTransItem->uiDest, pItem);
      if (errCode != Harbour::SUCCESS)
      {
        break;
      }
      ++pTransItem;
    }
    hb_itemRelease(pItem);
  }

  // Delete the new record if copy fail
  if (errCode != Harbour::SUCCESS)
  {
    SELF_DELETE(pTransInfo->lpaDest);
  }
  else if (bDeleted)
  {
    // Record with deleted flag
    if (pTransInfo->uiFlags & DBTF_RECALL)
    {
      errCode = SELF_RECALL(pTransInfo->lpaDest);
    }
    else
    {
      errCode = SELF_DELETE(pTransInfo->lpaDest);
    }
  }

  return errCode;
}

// Report end of relation.
static HB_ERRCODE hb_waChildEnd(AREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waChildEnd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  if (pRelInfo->isScoped)
  {
    DBORDERINFO pInfo;
    pInfo.itmOrder = nullptr;
    pInfo.atomBagName = nullptr;
    pInfo.itmResult = hb_itemNew(nullptr);
    pInfo.itmNewVal = nullptr;
    SELF_ORDINFO(pArea, DBOI_SCOPETOPCLEAR, &pInfo);
    SELF_ORDINFO(pArea, DBOI_SCOPEBOTTOMCLEAR, &pInfo);
    hb_itemRelease(pInfo.itmResult);
  }

  pArea->uiParents--;
  return Harbour::SUCCESS;
}

// Report initialization of a relation.
static HB_ERRCODE hb_waChildStart(AREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waChildStart(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif
  HB_SYMBOL_UNUSED(pRelInfo);

  pArea->uiParents++;
  return Harbour::SUCCESS;
}

// Force relational movement in child WorkAreas.
static HB_ERRCODE hb_waSyncChildren(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSyncChildren(%p)", static_cast<void*>(pArea)));
#endif

  LPDBRELINFO lpdbRelation;

  lpdbRelation = pArea->lpdbRelations;
  while (lpdbRelation)
  {
    if (SELF_CHILDSYNC(lpdbRelation->lpaChild, lpdbRelation) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
    lpdbRelation = lpdbRelation->lpdbriNext;
  }

  return Harbour::SUCCESS;
}

// Clear all relations in the specified WorkArea.
static HB_ERRCODE hb_waClearRel(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearRel(%p)", static_cast<void*>(pArea)));
#endif

  // Free all relations
  if (pArea->lpdbRelations)
  {
    auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();

    do
    {
      LPDBRELINFO lpdbRelation = pArea->lpdbRelations;

      hb_rddSelectWorkAreaNumber(lpdbRelation->lpaChild->uiArea);
      SELF_CHILDEND(lpdbRelation->lpaChild, lpdbRelation);
      pArea->lpdbRelations = lpdbRelation->lpdbriNext;

      if (lpdbRelation->itmCobExpr)
      {
        hb_itemRelease(lpdbRelation->itmCobExpr);
      }
      if (lpdbRelation->abKey)
      {
        hb_itemRelease(lpdbRelation->abKey);
      }
      hb_xfree(lpdbRelation);
    } while (pArea->lpdbRelations);

    hb_rddSelectWorkAreaNumber(iCurrArea);
  }

  return Harbour::SUCCESS;
}

// Obtain the workarea number of the specified relation.
static HB_ERRCODE hb_waRelArea(AREAP pArea, HB_USHORT uiRelNo, HB_USHORT *pRelArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waRelArea(%p, %hu, %p)", static_cast<void*>(pArea), uiRelNo, static_cast<void*>(pRelArea)));
#endif

  LPDBRELINFO lpdbRelations;
  HB_USHORT uiIndex = 1;

  *pRelArea = 0;
  lpdbRelations = pArea->lpdbRelations;
  while (lpdbRelations)
  {
    if (uiIndex++ == uiRelNo)
    {
      *pRelArea = lpdbRelations->lpaChild->uiArea;
      break;
    }
    lpdbRelations = lpdbRelations->lpdbriNext;
  }
  return *pRelArea ? Harbour::SUCCESS : Harbour::FAILURE;
}

// Evaluate a block against the relation in specified WorkArea.
static HB_ERRCODE hb_waRelEval(AREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waRelEval(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  HB_BOOL fEof;

  HB_ERRCODE errCode = SELF_EOF(pRelInfo->lpaParent, &fEof);
  if (errCode == Harbour::SUCCESS)
  {
    if (fEof)
    {
      errCode = SELF_GOTO(pArea, 0);
    }
    else
    {
      errCode = SELF_EVALBLOCK(pRelInfo->lpaParent, pRelInfo->itmCobExpr);
      if (errCode == Harbour::SUCCESS)
      {
        PHB_ITEM pResult;
        DBORDERINFO pInfo{};

        // Check the current order
        pResult = pRelInfo->lpaParent->valResult;
        pRelInfo->lpaParent->valResult = nullptr;
        pInfo.itmResult = hb_itemPutNI(nullptr, 0);
        errCode = SELF_ORDINFO(pArea, DBOI_NUMBER, &pInfo);

        if (errCode == Harbour::SUCCESS)
        {
          int iOrder = hb_itemGetNI(pInfo.itmResult);
          if (iOrder != 0)
          {
            if (pRelInfo->isScoped)
            {
              pInfo.itmNewVal = pResult;
              errCode = SELF_ORDINFO(pArea, DBOI_SCOPETOP, &pInfo);
              if (errCode == Harbour::SUCCESS)
              {
                errCode = SELF_ORDINFO(pArea, DBOI_SCOPEBOTTOM, &pInfo);
              }
            }
            if (errCode == Harbour::SUCCESS)
            {
              errCode = SELF_SEEK(pArea, false, pResult, false);
            }
          }
          else
          {
// If current order equals to zero, use GOTOID instead of SEEK
// Unfortunately it interacts with buggy .prg code which returns
// non numerical values from relation expression and RDD accepts
// only numerical record ID. In such case SELF_GOTO() works like
// SELF_GOEOF() but SELF_GOTOID() reports error. So for Clipper
// compatibility SELF_GOTO() is used here but if RDD can use
// non numerical record IDs then this method should be overloaded
// to use SELF_GOTOID(), [druzus]
#if 0
                  errCode = SELF_GOTOID(pArea, pResult);
#endif
            errCode = SELF_GOTO(pArea, hb_itemGetNL(pResult));
            if (errCode == Harbour::SUCCESS)
            {
              errCode = SELF_EOF(pArea, &fEof);
              if (errCode == Harbour::SUCCESS)
              {
                pArea->fFound = !fEof;
              }
            }
          }
        }
        hb_itemRelease(pInfo.itmResult);
        hb_itemRelease(pResult);
      }
    }
  }
  return errCode;
}

// Obtain the character expression of the specified relation.
static HB_ERRCODE hb_waRelText(AREAP pArea, HB_USHORT uiRelNo, PHB_ITEM pExpr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waRelText(%p, %hu, %p)", static_cast<void*>(pArea), uiRelNo, static_cast<void*>(pExpr)));
#endif

  LPDBRELINFO lpdbRelations;
  HB_USHORT uiIndex = 1;

  lpdbRelations = pArea->lpdbRelations;

  while (lpdbRelations)
  {
    if (uiIndex++ == uiRelNo)
    {
      hb_itemCopy(pExpr, lpdbRelations->abKey);
      return Harbour::SUCCESS;
    }
    lpdbRelations = lpdbRelations->lpdbriNext;
  }

  return Harbour::FAILURE;
}

// Set a relation in the parent file.
static HB_ERRCODE hb_waSetRel(AREAP pArea, LPDBRELINFO lpdbRelInf)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetRel(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(lpdbRelInf)));
#endif

  LPDBRELINFO lpdbRelations;

  lpdbRelations = pArea->lpdbRelations;
  if (!lpdbRelations)
  {
    pArea->lpdbRelations = static_cast<LPDBRELINFO>(hb_xgrab(sizeof(DBRELINFO)));
    lpdbRelations = pArea->lpdbRelations;
  }
  else
  {
    while (lpdbRelations->lpdbriNext)
    {
      lpdbRelations = lpdbRelations->lpdbriNext;
    }
    lpdbRelations->lpdbriNext = static_cast<LPDBRELINFO>(hb_xgrab(sizeof(DBRELINFO)));
    lpdbRelations = lpdbRelations->lpdbriNext;
  }
  lpdbRelations->lpaParent = pArea;
  lpdbRelations->lpaChild = lpdbRelInf->lpaChild;
  lpdbRelations->itmCobExpr = lpdbRelInf->itmCobExpr;
  lpdbRelations->isScoped = lpdbRelInf->isScoped;
  lpdbRelations->isOptimized = lpdbRelInf->isOptimized;
  lpdbRelations->abKey = lpdbRelInf->abKey;
  lpdbRelations->lpdbriNext = lpdbRelInf->lpdbriNext;

  return SELF_CHILDSTART(lpdbRelInf->lpaChild, lpdbRelations);
}

// Clear the active filter expression.
static HB_ERRCODE hb_waClearFilter(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearFilter(%p)", static_cast<void*>(pArea)));
#endif

  // Free all items
  if (pArea->dbfi.itmCobExpr)
  {
    hb_itemRelease(pArea->dbfi.itmCobExpr);
    pArea->dbfi.itmCobExpr = nullptr;
  }
  if (pArea->dbfi.abFilterText)
  {
    hb_itemRelease(pArea->dbfi.abFilterText);
    pArea->dbfi.abFilterText = nullptr;
  }
  pArea->dbfi.fOptimized = false;
  pArea->dbfi.fFilter = false;

  return Harbour::SUCCESS;
}

// Clear the active locate expression.
static HB_ERRCODE hb_waClearLocate(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearLocate(%p)", static_cast<void*>(pArea)));
#endif

  // Free all items
  if (pArea->dbsi.itmCobFor)
  {
    hb_itemRelease(pArea->dbsi.itmCobFor);
    pArea->dbsi.itmCobFor = nullptr;
  }
  if (pArea->dbsi.lpstrFor)
  {
    hb_itemRelease(pArea->dbsi.lpstrFor);
    pArea->dbsi.lpstrFor = nullptr;
  }
  if (pArea->dbsi.itmCobWhile)
  {
    hb_itemRelease(pArea->dbsi.itmCobWhile);
    pArea->dbsi.itmCobWhile = nullptr;
  }
  if (pArea->dbsi.lpstrWhile)
  {
    hb_itemRelease(pArea->dbsi.lpstrWhile);
    pArea->dbsi.lpstrWhile = nullptr;
  }
  if (pArea->dbsi.lNext)
  {
    hb_itemRelease(pArea->dbsi.lNext);
    pArea->dbsi.lNext = nullptr;
  }
  if (pArea->dbsi.itmRecID)
  {
    hb_itemRelease(pArea->dbsi.itmRecID);
    pArea->dbsi.itmRecID = nullptr;
  }
  if (pArea->dbsi.fRest)
  {
    hb_itemRelease(pArea->dbsi.fRest);
    pArea->dbsi.fRest = nullptr;
  }

  return Harbour::SUCCESS;
}

// Return filter condition of the specified WorkArea.
static HB_ERRCODE hb_waFilterText(AREAP pArea, PHB_ITEM pFilter)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waFilterText(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFilter)));
#endif

  if (pArea->dbfi.abFilterText)
  {
    hb_itemCopy(pFilter, pArea->dbfi.abFilterText);
  }

  return Harbour::SUCCESS;
}

// Set the filter condition for the specified WorkArea.
static HB_ERRCODE hb_waSetFilter(AREAP pArea, LPDBFILTERINFO pFilterInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetFilter(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFilterInfo)));
#endif

  // Clear the active filter expression
  if (SELF_CLEARFILTER(pArea) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (pFilterInfo->itmCobExpr)
  {
    pArea->dbfi.itmCobExpr = hb_itemNew(pFilterInfo->itmCobExpr);
  }
  if (pFilterInfo->abFilterText)
  {
    pArea->dbfi.abFilterText = hb_itemNew(pFilterInfo->abFilterText);
  }
  pArea->dbfi.fOptimized = pFilterInfo->fOptimized;
  pArea->dbfi.fFilter = true;

  return Harbour::SUCCESS;
}

// Set the locate scope for the specified WorkArea.
static HB_ERRCODE hb_waSetLocate(AREAP pArea, LPDBSCOPEINFO pScopeInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetLocate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pScopeInfo)));
#endif

  // Clear the active locate expression
  if (SELF_CLEARLOCATE(pArea) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }

  if (pScopeInfo->itmCobFor)
  {
    pArea->dbsi.itmCobFor = hb_itemNew(pScopeInfo->itmCobFor);
  }

  if (pScopeInfo->lpstrFor)
  {
    pArea->dbsi.lpstrFor = hb_itemNew(pScopeInfo->lpstrFor);
  }

  if (pScopeInfo->itmCobWhile)
  {
    pArea->dbsi.itmCobWhile = hb_itemNew(pScopeInfo->itmCobWhile);
  }

  if (pScopeInfo->lpstrWhile)
  {
    pArea->dbsi.lpstrWhile = hb_itemNew(pScopeInfo->lpstrWhile);
  }

  if (pScopeInfo->lNext)
  {
    pArea->dbsi.lNext = hb_itemNew(pScopeInfo->lNext);
  }

  if (pScopeInfo->itmRecID)
  {
    pArea->dbsi.itmRecID = hb_itemNew(pScopeInfo->itmRecID);
  }

  if (pScopeInfo->fRest)
  {
    pArea->dbsi.fRest = hb_itemNew(pScopeInfo->fRest);
  }

  pArea->dbsi.fIgnoreFilter = pScopeInfo->fIgnoreFilter;
  pArea->dbsi.fIncludeDeleted = pScopeInfo->fIncludeDeleted;
  pArea->dbsi.fLast = pScopeInfo->fLast;
  pArea->dbsi.fIgnoreDuplicates = pScopeInfo->fIgnoreDuplicates;
  pArea->dbsi.fBackward = pScopeInfo->fBackward;
  pArea->dbsi.fOptimized = pScopeInfo->fOptimized;

  return Harbour::SUCCESS;
}

// Compile a character expression.
static HB_ERRCODE hb_waCompile(AREAP pArea, const char *szExpr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waCompile(%p, %p)", static_cast<void*>(pArea), static_cast<const void *>(szExpr)));
#endif

  if (!pArea->valResult)
  {
    pArea->valResult = hb_itemNew(nullptr);
  }
  return hb_vmCompileMacro(szExpr, pArea->valResult) ? Harbour::SUCCESS : Harbour::FAILURE;
}

// Raise a runtime error.
static HB_ERRCODE hb_waError(AREAP pArea, PHB_ITEM pError)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waError(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pError)));
#endif

  char szRddName[HB_RDD_MAX_DRIVERNAME_LEN + 1];

  if (pArea && pArea->lprfsHost->sysName)
  {
    SELF_SYSNAME(pArea, szRddName);
  }
  else
  {
    hb_strncpy(szRddName, "???DRIVER", HB_RDD_MAX_DRIVERNAME_LEN);
  }
  hb_errPutSeverity(pError, ES_ERROR);
  hb_errPutSubSystem(pError, szRddName);
  return hb_errLaunch(pError);
}

// Evaluate a code block.
static HB_ERRCODE hb_waEvalBlock(AREAP pArea, PHB_ITEM pBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waEvalBlock(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBlock)));
#endif

  int iUsedArea;

  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  iUsedArea = pArea->uiArea;
  if (iCurrArea != iUsedArea)
  {
    hb_rddSelectWorkAreaNumber(iUsedArea);
  }

  auto pItem = hb_vmEvalBlockOrMacro(pBlock);

  if (static_cast<AREAP>(hb_rddGetWorkAreaPointer(iUsedArea)) != pArea)
  {
    return Harbour::FAILURE;
  }

  if (!pArea->valResult)
  {
    pArea->valResult = hb_itemNew(nullptr);
  }
  hb_itemMove(pArea->valResult, pItem);

  hb_rddSelectWorkAreaNumber(iCurrArea);

  return hb_vmRequestQuery() ? Harbour::FAILURE : Harbour::SUCCESS;
}

// RDD info
static HB_ERRCODE hb_waRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnection, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddInfo(%p, %hu, %lu, %p)", static_cast<void*>(pRDD), uiIndex, ulConnection, static_cast<void*>(pItem)));
#endif

  auto fResult = false;
  int iResult;

  HB_SYMBOL_UNUSED(pRDD);
  HB_SYMBOL_UNUSED(ulConnection);

  switch (uiIndex)
  {
  case RDDI_ISDBF:
  case RDDI_CANPUTREC:
  case RDDI_LOCAL:
  case RDDI_REMOTE:
  case RDDI_RECORDMAP:
  case RDDI_ENCRYPTION:
  case RDDI_AUTOLOCK:
  case RDDI_STRUCTORD:
  case RDDI_LARGEFILE:
  case RDDI_MULTITAG:
  case RDDI_SORTRECNO:
  case RDDI_MULTIKEY:
  case RDDI_BLOB_SUPPORT:
    hb_itemPutL(pItem, false);
    break;

  case RDDI_CONNECTION:
  case RDDI_TABLETYPE:
  case RDDI_MEMOTYPE:
  case RDDI_MEMOVERSION:
    hb_itemPutNI(pItem, 0);
    break;

  case RDDI_STRICTREAD:
    fResult = hb_setGetStrictRead();
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      hb_setSetItem(HB_SET_STRICTREAD, pItem);
    }
    hb_itemPutL(pItem, fResult);
    break;
  case RDDI_OPTIMIZE:
    fResult = hb_setGetOptimize();
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      hb_setSetItem(HB_SET_OPTIMIZE, pItem);
    }
    hb_itemPutL(pItem, fResult);
    break;
  case RDDI_FORCEOPT:
    fResult = hb_setGetForceOpt();
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      hb_setSetItem(HB_SET_FORCEOPT, pItem);
    }
    hb_itemPutL(pItem, fResult);
    break;
  case RDDI_AUTOOPEN:
    fResult = hb_setGetAutOpen();
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      hb_setSetItem(HB_SET_AUTOPEN, pItem);
    }
    hb_itemPutL(pItem, fResult);
    break;
  case RDDI_AUTOORDER:
    iResult = hb_setGetAutOrder();
    if (hb_itemType(pItem) & Harbour::Item::NUMERIC)
    {
      hb_setSetItem(HB_SET_AUTORDER, pItem);
    }
    hb_itemPutNI(pItem, iResult);
    break;
  case RDDI_AUTOSHARE:
    fResult = hb_setGetAutoShare();
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      hb_setSetItem(HB_SET_AUTOSHARE, pItem);
    }
    hb_itemPutL(pItem, fResult);
    break;
  case RDDI_LOCKSCHEME:
    iResult = hb_setGetDBFLockScheme();
    if (hb_itemType(pItem) & Harbour::Item::NUMERIC)
    {
      hb_setSetItem(HB_SET_DBFLOCKSCHEME, pItem);
    }
    hb_itemPutNI(pItem, iResult);
    break;
  case RDDI_MEMOBLOCKSIZE:
    iResult = hb_setGetMBlockSize();
    if (hb_itemType(pItem) & Harbour::Item::NUMERIC)
    {
      hb_setSetItem(HB_SET_MBLOCKSIZE, pItem);
    }
    hb_itemPutNI(pItem, iResult);
    break;
  case RDDI_MEMOEXT:
  {
    const char *szExt = hb_setGetMFileExt();
    char *szResult = szExt ? hb_strdup(szExt) : nullptr;
    if (hb_itemType(pItem) & Harbour::Item::STRING)
    {
      hb_setSetItem(HB_SET_MFILEEXT, pItem);
    }
    hb_itemPutCPtr(pItem, szResult);
    break;
  }
  case RDDI_TABLEEXT:
  case RDDI_ORDBAGEXT:
  case RDDI_ORDEREXT:
  case RDDI_ORDSTRUCTEXT:
  case RDDI_DELIMITER:
  case RDDI_SEPARATOR:
  case RDDI_TRIGGER:
  case RDDI_PENDINGTRIGGER:
    hb_itemPutC(pItem, nullptr);
    // fallthrough - return Harbour::FAILURE

  default:
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

// Raise a runtime error if an method is not defined.
static HB_ERRCODE hb_waUnsupported(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waUnsupported(%p)", static_cast<void*>(pArea)));
#endif

  auto pError = hb_errNew();
  hb_errPutGenCode(pError, EG_UNSUPPORTED);
  hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_UNSUPPORTED));
  SELF_ERROR(pArea, pError);
  hb_itemRelease(pError);

  return Harbour::FAILURE;
}

static HB_ERRCODE hb_waUnsupported_B(AREAP pArea, HB_BOOL p1)
{
  HB_SYMBOL_UNUSED(p1);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_V(AREAP pArea, void *p1)
{
  HB_SYMBOL_UNUSED(p1);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_L(AREAP pArea, HB_LONG p1)
{
  HB_SYMBOL_UNUSED(p1);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_SL(AREAP pArea, HB_USHORT p1, HB_LONG p2)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_SV(AREAP pArea, HB_USHORT p1, void *p2)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_LV(AREAP pArea, HB_LONG p1, void *p2)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_VL(AREAP pArea, void *p1, HB_LONG p2)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_VV(AREAP pArea, void *p1, void *p2)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_BVB(AREAP pArea, HB_BOOL p1, void *p2, HB_BOOL p3)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);
  HB_SYMBOL_UNUSED(p3);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_SVS(AREAP pArea, HB_USHORT p1, void *p2, HB_USHORT p3)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);
  HB_SYMBOL_UNUSED(p3);

  return hb_waUnsupported(pArea);
}

static HB_ERRCODE hb_waUnsupported_VSV(AREAP pArea, void *p1, HB_USHORT p2, void *p3)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);
  HB_SYMBOL_UNUSED(p3);

  return hb_waUnsupported(pArea);
}

// Raise a runtime error if an method is not defined.
static HB_ERRCODE hb_waRddUnsupported(LPRDDNODE pRDD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waRDDUnsupported(%p)", static_cast<void*>(pRDD)));
#endif

  auto pError = hb_errNew();
  hb_errPutGenCode(pError, EG_UNSUPPORTED);
  hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_UNSUPPORTED));

  hb_errPutSeverity(pError, ES_ERROR);
  hb_errPutSubSystem(pError, pRDD->szName);
  hb_errLaunch(pError);
  hb_itemRelease(pError);

  return Harbour::FAILURE;
}

static HB_ERRCODE hb_waRddUnsupported_VVL(LPRDDNODE pRDD, void *p1, void *p2, HB_LONG p3)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);
  HB_SYMBOL_UNUSED(p3);

  return hb_waRddUnsupported(pRDD);
}

static HB_ERRCODE hb_waRddUnsupported_VVVL(LPRDDNODE pRDD, void *p1, void *p2, void *p3, HB_LONG p4)
{
  HB_SYMBOL_UNUSED(p1);
  HB_SYMBOL_UNUSED(p2);
  HB_SYMBOL_UNUSED(p3);
  HB_SYMBOL_UNUSED(p4);

  return hb_waRddUnsupported(pRDD);
}

#if 0
// Empty method.
static HB_ERRCODE hb_waNull(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_waNull(%p)", static_cast<void*>(pArea)));
#endif

   HB_SYMBOL_UNUSED(pArea);

   return Harbour::SUCCESS;
}
#endif

// The default virtual method table for all WorkAreas.
static const RDDFUNCS waTable = {
    // Movement and positioning methods
    /* ( DBENTRYP_BP )   */ hb_waBof,        // Bof
    /* ( DBENTRYP_BP )   */ hb_waEof,        // Eof
    /* ( DBENTRYP_BP )   */ hb_waFound,      // Found
    (DBENTRYP_V)hb_waUnsupported,            // GoBottom
    (DBENTRYP_UL)hb_waUnsupported_L,         // GoTo
    (DBENTRYP_I)hb_waUnsupported_V,          // GoToId
    (DBENTRYP_V)hb_waUnsupported,            // GoTop
    (DBENTRYP_BIB)hb_waUnsupported_BVB,      // Seek
    /* ( DBENTRYP_L )    */ hb_waSkip,       // Skip
    /* ( DBENTRYP_L )    */ hb_waSkipFilter, // SkipFilter
    (DBENTRYP_L)hb_waUnsupported_L,          // SkipRaw

    // Data management
    /* ( DBENTRYP_VF )   */ hb_waAddField,       // AddField
    (DBENTRYP_B)hb_waUnsupported_B,              // Append
    /* ( DBENTRYP_I )    */ hb_waCreateFields,   // CreateFields
    (DBENTRYP_V)hb_waUnsupported,                // DeleteRec
    (DBENTRYP_BP)hb_waUnsupported_V,             // Deleted
    /* ( DBENTRYP_SP )   */ hb_waFieldCount,     // FieldCount
    (DBENTRYP_VF)hb_waUnsupported_V,             // FieldDisplay
    /* ( DBENTRYP_SSI )  */ hb_waFieldInfo,      // FieldInfo
    /* ( DBENTRYP_SCP )  */ hb_waFieldName,      // FieldName
    (DBENTRYP_V)hb_waUnsupported,                // Flush
    (DBENTRYP_PP)hb_waUnsupported_V,             // GetRec
    (DBENTRYP_SI)hb_waUnsupported_SV,            // GetValue
    (DBENTRYP_SVL)hb_waUnsupported_SV,           // GetVarLen
    (DBENTRYP_V)hb_waUnsupported,                // GoCold
    (DBENTRYP_V)hb_waUnsupported,                // GoHot
    (DBENTRYP_P)hb_waUnsupported_V,              // PutRec
    (DBENTRYP_SI)hb_waUnsupported_SV,            // PutValue
    (DBENTRYP_V)hb_waUnsupported,                // Recall
    (DBENTRYP_ULP)hb_waUnsupported_V,            // RecCount
    (DBENTRYP_ISI)hb_waUnsupported_VSV,          // RecInfo
    (DBENTRYP_ULP)hb_waUnsupported_V,            // RecNo
    (DBENTRYP_I)hb_waUnsupported_V,              // RecId
    /* ( DBENTRYP_S )    */ hb_waSetFieldExtent, // SetFieldExtent

    // WorkArea/Database management
    /* ( DBENTRYP_CP )   */ hb_waAlias,      // Alias
    /* ( DBENTRYP_V )    */ hb_waClose,      // Close
                                             // Like in Clipper map CREATE() method at work area level to OPEN()
    /* ( DBENTRYP_VO )   */ hb_waOpen,       // Create
    /* ( DBENTRYP_SI )   */ hb_waInfo,       // Info
    /* ( DBENTRYP_V )    */ hb_waNewArea,    // NewArea
    /* ( DBENTRYP_VO )   */ hb_waOpen,       // Open
    /* ( DBENTRYP_V )    */ hb_waRelease,    // Release
    /* ( DBENTRYP_SP )   */ hb_waStructSize, // StructSize
    /* ( DBENTRYP_CP )   */ hb_waSysName,    // SysName
    /* ( DBENTRYP_VEI )  */ hb_waEval,       // Eval
    (DBENTRYP_V)hb_waUnsupported,            // Pack
    (DBENTRYP_LSP)hb_waUnsupported_LV,       // PackRec
    (DBENTRYP_VS)hb_waUnsupported_V,         // Sort
    /* ( DBENTRYP_VT )   */ hb_waTrans,      // Trans
    /* ( DBENTRYP_VT )   */ hb_waTransRec,   // TransRec
    (DBENTRYP_V)hb_waUnsupported,            // Zap

    // Relational Methods
    /* ( DBENTRYP_VR )   */ hb_waChildEnd,     // ChildEnd
    /* ( DBENTRYP_VR )   */ hb_waChildStart,   // ChildStart
    (DBENTRYP_VR)hb_waUnsupported_V,           // ChildSync
    /* ( DBENTRYP_V )    */ hb_waSyncChildren, // SyncChildren
    /* ( DBENTRYP_V )    */ hb_waClearRel,     // ClearRel
    (DBENTRYP_V)hb_waUnsupported,              // ForceRel
    /* ( DBENTRYP_SSP )  */ hb_waRelArea,      // RelArea
    /* ( DBENTRYP_VR )   */ hb_waRelEval,      // RelEval
    /* ( DBENTRYP_SI )   */ hb_waRelText,      // RelText
    /* ( DBENTRYP_VR )   */ hb_waSetRel,       // SetRel

    // Order Management
    (DBENTRYP_VOI)hb_waUnsupported_V,            // OrderListAdd
    (DBENTRYP_V)hb_waUnsupported,                // OrderListClear
    (DBENTRYP_VOI)hb_waUnsupported_V,            // OrderListDelete
    (DBENTRYP_VOI)hb_waUnsupported_V,            // OrderListFocus
    (DBENTRYP_V)hb_waUnsupported,                // OrderListRebuild
    /* ( DBENTRYP_VOO )  */ hb_waOrderCondition, // OrderCondition
    (DBENTRYP_VOC)hb_waUnsupported_V,            // OrderCreate
    (DBENTRYP_VOI)hb_waUnsupported_V,            // OrderDestroy
    /* ( DBENTRYP_SVOI ) */ hb_waOrderInfo,      // OrderInfo

    // Filters and Scope Settings
    /* ( DBENTRYP_V )    */ hb_waClearFilter, // ClearFilter
    /* ( DBENTRYP_V )    */ hb_waClearLocate, // ClearLocate
    (DBENTRYP_V)hb_waUnsupported,             // ClearScope
    (DBENTRYP_VPLP)hb_waUnsupported_VV,       // CountScope
    /* ( DBENTRYP_I )    */ hb_waFilterText,  // FilterText
    (DBENTRYP_SI)hb_waUnsupported_SV,         // ScopeInfo
    /* ( DBENTRYP_VFI )  */ hb_waSetFilter,   // SetFilter
    /* ( DBENTRYP_VLO )  */ hb_waSetLocate,   // SetLocate
    (DBENTRYP_VOS)hb_waUnsupported_V,         // SetScope
    (DBENTRYP_VPL)hb_waUnsupported_VL,        // SkipScope
    /* ( DBENTRYP_B )    */ hb_waLocate,      // Locate

    // Miscellaneous
    /* ( DBENTRYP_CC )   */ hb_waCompile,   // Compile
    /* ( DBENTRYP_I )    */ hb_waError,     // Error
    /* ( DBENTRYP_I )    */ hb_waEvalBlock, // EvalBlock

    // Network operations
    (DBENTRYP_VSP)hb_waUnsupported_SL, // RawLock
    (DBENTRYP_VL)hb_waUnsupported_V,   // Lock
    (DBENTRYP_I)hb_waUnsupported_V,    // UnLock

    // Memofile functions
    (DBENTRYP_V)hb_waUnsupported,        // CloseMemFile
    (DBENTRYP_VO)hb_waUnsupported_V,     // CreateMemFile
    (DBENTRYP_SCCS)hb_waUnsupported_SVS, // GetValueFile
    (DBENTRYP_VO)hb_waUnsupported_V,     // OpenMemFile
    (DBENTRYP_SCCS)hb_waUnsupported_SVS, // PutValueFile

    // Database file header handling
    (DBENTRYP_V)hb_waUnsupported, // ReadDBHeader
    (DBENTRYP_V)hb_waUnsupported, // WriteDBHeader

    // non WorkArea functions
    (DBENTRYP_R) nullptr,                     // Init
    (DBENTRYP_R) nullptr,                     // Exit
    (DBENTRYP_RVVL)hb_waRddUnsupported_VVL,   // Drop
    (DBENTRYP_RVVL)hb_waRddUnsupported_VVL,   // Exists
    (DBENTRYP_RVVVL)hb_waRddUnsupported_VVVL, // Rename
    /* ( DBENTRYP_RSLV ) */ hb_waRddInfo,     // RddInfo

    // Special and reserved methods
    (DBENTRYP_SVP)hb_waUnsupported_SV // WhoCares
};

#define HB_RDD_POOL_ALLOCSIZE 128
// common for all threads list of registered RDDs
#if defined(HB_USE_CPP_MUTEX)
std::mutex rddMtx;
#else
static HB_CRITICAL_NEW(s_rddMtx);
#endif
static LPRDDNODE *s_RddList = nullptr; // Registered RDDs pool
static HB_USHORT s_uiRddMax = 0;       // Size of RDD pool
static HB_USHORT s_uiRddCount = 0;     // Number of registered RDD

static HB_RDDACCEPT *s_rddRedirAccept = nullptr;
static HB_USHORT s_uiRddRedirMax = 0;
static HB_USHORT s_uiRddRedirCount = 0;

// Get RDD node pointer
LPRDDNODE hb_rddGetNode(HB_USHORT uiNode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetNode(%hu)", uiNode));
#endif

  return uiNode < s_uiRddCount ? s_RddList[uiNode] : nullptr;
}

PHB_ITEM hb_rddList(HB_USHORT uiType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddList(%hu)", uiType));
#endif

  HB_USHORT uiCount, uiIndex, uiRdds;

  for (uiCount = uiRdds = 0; uiCount < s_uiRddCount; ++uiCount)
  {
    if (uiType == 0 || s_RddList[uiCount]->uiType == uiType)
    {
      ++uiRdds;
    }
  }
  auto pRddArray = hb_itemArrayNew(uiRdds);
  for (uiCount = uiIndex = 0; uiCount < s_uiRddCount && uiIndex < uiRdds; ++uiCount)
  {
    LPRDDNODE pNode = s_RddList[uiCount];
    if (uiType == 0 || pNode->uiType == uiType)
    {
      hb_arraySetC(pRddArray, ++uiIndex, pNode->szName);
    }
  }
  return pRddArray;
}

// Find a RDD node.
LPRDDNODE hb_rddFindNode(const char *szDriver, HB_USHORT *uiIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, static_cast<void*>(uiIndex)));
#endif

  for (HB_USHORT uiCount = 0; uiCount < s_uiRddCount; uiCount++)
  {
    LPRDDNODE pNode = s_RddList[uiCount];
    if (strcmp(pNode->szName, szDriver) == 0)
    { // Matched RDD
      if (uiIndex)
      {
        *uiIndex = uiCount;
      }
      return pNode;
    }
  }
  if (uiIndex)
  {
    *uiIndex = 0;
  }
  return nullptr;
}

// Find a RDD node respecting file/table name
LPRDDNODE hb_rddFindFileNode(LPRDDNODE pRddNode, const char *szFileName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindFileNode(%p, %s)", static_cast<void*>(pRddNode), szFileName));
#endif

  if (szFileName != nullptr && szFileName[0] && s_uiRddRedirCount)
  {
    for (HB_USHORT uiCount = 0; uiCount < s_uiRddRedirCount; uiCount++)
    {
      LPRDDNODE pNode = s_rddRedirAccept[uiCount](pRddNode, szFileName);
      if (pNode)
      {
        return pNode;
      }
    }
  }

  return pRddNode;
}

// dummy RDD file/table name redirector
static LPRDDNODE hb_rddDummyFileAccept(LPRDDNODE pRddNode, const char *szFileName)
{
  HB_SYMBOL_UNUSED(pRddNode);
  HB_SYMBOL_UNUSED(szFileName);

  return nullptr;
}

// Add new RDD file/table name redirector
void hb_rddSetFileRedirector(HB_RDDACCEPT funcAccept, HB_BOOL fEnable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddSetFileRedirector(%p, %d)", static_cast<void*>(funcAccept), fEnable));
#endif

  HB_USHORT uiFree;

#if defined(HB_USE_CPP_MUTEX)
  rddMtx.lock();
#else
  hb_threadEnterCriticalSection(&s_rddMtx);
#endif
  uiFree = s_uiRddRedirCount + 1;
  for (HB_USHORT uiCount = 0; uiCount < s_uiRddRedirCount; uiCount++)
  {
    if (s_rddRedirAccept[uiCount] == funcAccept)
    {
      if (!fEnable)
      {
        s_rddRedirAccept[uiCount] = hb_rddDummyFileAccept;
      }
      return;
    }
    else if (s_rddRedirAccept[uiCount] == hb_rddDummyFileAccept)
    {
      uiFree = uiCount;
    }
  }
  if (uiFree < s_uiRddRedirCount)
  {
    s_rddRedirAccept[uiFree] = funcAccept;
  }
  else
  {
    if (s_uiRddRedirCount == s_uiRddRedirMax)
    {
      s_uiRddRedirMax += HB_RDD_POOL_ALLOCSIZE;
      s_rddRedirAccept =
          static_cast<HB_RDDACCEPT *>(hb_xrealloc(s_rddRedirAccept, sizeof(HB_RDDACCEPT) * s_uiRddRedirMax));
    }
    s_rddRedirAccept[s_uiRddRedirCount] = funcAccept;
    s_uiRddRedirCount++;
  }
#if defined(HB_USE_CPP_MUTEX)
  rddMtx.unlock();
#else
  hb_threadLeaveCriticalSection(&s_rddMtx);
#endif
}

// Shutdown the RDD system.
void hb_rddShutDown(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));
#endif

  hb_rddCloseDetachedAreas();

  if (s_uiRddCount > 0)
  {
    for (HB_USHORT uiCount = 0; uiCount < s_uiRddCount; uiCount++)
    {
      if (s_RddList[uiCount]->pTable.exit != nullptr)
      {
        SELF_EXIT(s_RddList[uiCount]);
      }
      hb_xfree(s_RddList[uiCount]);
    }
    hb_xfree(s_RddList);
    s_RddList = nullptr;
    s_uiRddMax = s_uiRddCount = 0;
  }
  if (s_uiRddRedirCount)
  {
    hb_xfree(s_rddRedirAccept);
    s_rddRedirAccept = nullptr;
    s_uiRddRedirMax = s_uiRddRedirCount = 0;
  }
}

// Register a RDD driver.
int hb_rddRegister(const char *szDriver, HB_USHORT uiType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddRegister(%s, %hu)", szDriver, uiType));
#endif

  char szGetFuncTable[HB_RDD_MAX_DRIVERNAME_LEN + 14];
  HB_USHORT uiFunctions = 0;
  int iResult;

  if (hb_rddFindNode(szDriver, nullptr))
  { // Duplicated RDD
    return 1;
  }

  hb_snprintf(szGetFuncTable, sizeof(szGetFuncTable), "%s_GETFUNCTABLE", szDriver);
  auto pGetFuncTable = hb_dynsymFindName(szGetFuncTable);
  if (!pGetFuncTable)
  {
    return 2; // Not valid RDD
  }

  // Create a new RDD node
  auto pRddNewNode = static_cast<LPRDDNODE>(hb_xgrabz(sizeof(RDDNODE)));

  // Fill the new RDD node
  hb_strncpy(pRddNewNode->szName, szDriver, sizeof(pRddNewNode->szName) - 1);
  pRddNewNode->uiType = uiType;
  pRddNewNode->rddID = s_uiRddCount;
  pRddNewNode->rddSuperID = static_cast<HB_USHORT>(-1);

  // Call <szDriver>_GETFUNCTABLE()
  hb_vmPushDynSym(pGetFuncTable);
  hb_vmPushNil();
  hb_vmPushPointer(static_cast<void *>(&uiFunctions));
  hb_vmPushPointer(static_cast<void *>(&pRddNewNode->pTable));
  hb_vmPushPointer(static_cast<void *>(&pRddNewNode->pSuperTable));
  hb_vmPushInteger(s_uiRddCount);
  hb_vmPushPointer(static_cast<void *>(&pRddNewNode->rddSuperID));
  hb_vmProc(5);
  if (hb_parnidef(-1, Harbour::FAILURE) != Harbour::SUCCESS)
  {
    iResult = 3; // Invalid FUNCTABLE
  }
  else
  {
#if defined(HB_USE_CPP_MUTEX)
    rddMtx.lock();
#else
    hb_threadEnterCriticalSection(&s_rddMtx);
#endif
    // repeat the test to protect against possible registering RDD by
    //  <szDriver>_GETFUNCTABLE()
    if (!hb_rddFindNode(szDriver, nullptr))
    { // Duplicated RDD
      if (s_uiRddCount == s_uiRddMax)
      {
        s_uiRddMax += HB_RDD_POOL_ALLOCSIZE;
        s_RddList = static_cast<LPRDDNODE *>(hb_xrealloc(s_RddList, sizeof(LPRDDNODE) * s_uiRddMax));
      }
      s_RddList[s_uiRddCount] = pRddNewNode; // Add the new RDD node
      s_uiRddCount++;
      iResult = 0;
    }
    else
    {
      iResult = 1;
    }
#if defined(HB_USE_CPP_MUTEX)
    rddMtx.unlock();
#else
    hb_threadLeaveCriticalSection(&s_rddMtx);
#endif
  }

  if (iResult != 0)
  {
    hb_xfree(pRddNewNode);
  }
  else if (pRddNewNode->pTable.init != nullptr)
  {
    SELF_INIT(pRddNewNode);
  }

  return iResult;
}

// pTable - a table in new RDDNODE that will be filled
// pSubTable - a table with a list of supported functions
// pSuperTable - a current table in a RDDNODE
// szDrvName - a driver name that will be inherited
HB_ERRCODE hb_rddInheritEx(RDDFUNCS *pTable, const RDDFUNCS *pSubTable, RDDFUNCS *pSuperTable, const char *szDrvName,
                           HB_USHORT *puiSuperRddId)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddInheritEx(%p, %p, %p, %s, %p)", static_cast<void*>(pTable), static_cast<const void*>(pSubTable), static_cast<void*>(pSuperTable), szDrvName, static_cast<void*>(puiSuperRddId)));
#endif

  LPRDDNODE pRddNode;
  DBENTRYP_V *pFunction;
  const DBENTRYP_V *pSubFunction;

  if (!pTable)
  {
    return Harbour::FAILURE;
  }

  // Copy the pSuperTable into pTable
  if (!szDrvName || !*szDrvName)
  {
    // no name for inherited driver - use the default one
    memcpy(pTable, &waTable, sizeof(RDDFUNCS));
    memcpy(pSuperTable, &waTable, sizeof(RDDFUNCS));
    if (puiSuperRddId)
    {
      *puiSuperRddId = static_cast<HB_USHORT>(-1);
    }
  }
  else
  {
    char szSuperName[HB_RDD_MAX_DRIVERNAME_LEN + 1];
    hb_strncpyUpper(szSuperName, szDrvName, sizeof(szSuperName) - 1);
    pRddNode = hb_rddFindNode(szSuperName, nullptr);

    if (!pRddNode)
    {
      return Harbour::FAILURE;
    }

    memcpy(pTable, &pRddNode->pTable, sizeof(RDDFUNCS));
    memcpy(pSuperTable, &pRddNode->pTable, sizeof(RDDFUNCS));
    if (puiSuperRddId)
    {
      *puiSuperRddId = pRddNode->rddID;
    }
  }

  // Copy the non nullptr entries from pSubTable into pTable
  pFunction = reinterpret_cast<DBENTRYP_V *>(pTable);
  pSubFunction = reinterpret_cast<const DBENTRYP_V *>(pSubTable);
  for (HB_USHORT uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++)
  {
    if (*pSubFunction)
    {
      *pFunction = *pSubFunction;
    }
    pFunction++;
    pSubFunction++;
  }
  return Harbour::SUCCESS;
}

HB_ERRCODE hb_rddInherit(RDDFUNCS *pTable, const RDDFUNCS *pSubTable, RDDFUNCS *pSuperTable, const char *szDrvName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", static_cast<void*>(pTable), static_cast<const void*>(pSubTable), static_cast<void*>(pSuperTable), szDrvName));
#endif

  return hb_rddInheritEx(pTable, pSubTable, pSuperTable, szDrvName, nullptr);
}

HB_BOOL hb_rddIsDerivedFrom(HB_USHORT uiRddID, HB_USHORT uiSuperRddID)
{
  if (uiRddID == uiSuperRddID)
  {
    return true;
  }

  while (uiRddID < s_uiRddCount)
  {
    uiRddID = s_RddList[uiRddID]->rddSuperID;
    if (uiRddID == uiSuperRddID)
    {
      return true;
    }
  }
  return false;
}

// extend the size of RDD nodes buffer to given value to avoid later
// RT reallocations. It may be useful in some very seldom cases
// for MT programs which will register dynamically at runtime
// more then 128 RDDs.
HB_FUNC(__RDDPREALLOCATE)
{
  HB_LONG lNewSize = hb_parnl(1);

  if (lNewSize > static_cast<HB_LONG>(USHRT_MAX))
  {
    lNewSize = USHRT_MAX;
  }
  if (lNewSize > static_cast<HB_LONG>(s_uiRddMax))
  {
    s_uiRddMax += HB_RDD_POOL_ALLOCSIZE;
    s_RddList = static_cast<LPRDDNODE *>(hb_xrealloc(s_RddList, sizeof(LPRDDNODE) * s_uiRddMax));
  }

  hb_retnl(s_uiRddMax);
}

HB_FUNC_EXTERN(RDDSYS);
extern void _hb_rddWorkAreaForceLink(void);
void _hb_rddWorkAreaForceLink(void)
{
  HB_FUNC_EXEC(RDDSYS);
}
