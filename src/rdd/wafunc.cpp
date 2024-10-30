//
// Default RDD module
//
// Copyright 1999 Bruno Cantero <bruno@issnet.net>
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//

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

#include "hbapi.hpp"
#include "hbapirdd.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbvm.hpp"
#include "rddsys.ch"
#include "hbset.hpp"

// check if a given name can be used as alias expression
HB_ERRCODE hb_rddVerifyAliasName(const char *szAlias)
{
  if (szAlias != nullptr)
  {
    char c;

    // Clipper ignores only trailing spaces
#if 0
      while( *szAlias == ' ' ) {
         szAlias++;
      }
#endif

    c = *szAlias;
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
    {
      c = *(++szAlias);
      while (c != 0)
      {
        if (c != '_' && !(c >= '0' && c <= '9') && !(c >= 'A' && c <= 'Z') && !(c >= 'a' && c <= 'z'))
        {
          if (c == ' ')
          {
            while (*(++szAlias) == ' ')
            {
              ;
            }
            if (!*szAlias)
            {
              break;
            }
          }
          return Harbour::FAILURE;
        }
        c = *(++szAlias);
      }
      return Harbour::SUCCESS;
    }
  }
  return Harbour::FAILURE;
}

// Prepares a new WorkArea node.
void *hb_rddNewAreaNode(LPRDDNODE pRddNode, HB_USHORT uiRddID)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p,%hu)", static_cast<void*>(pRddNode), uiRddID));
#endif

  AREAP pArea;

  if (pRddNode->uiAreaSize == 0)
  { // Calculate the size of WorkArea
    HB_USHORT uiSize;

    pArea = static_cast<AREAP>(hb_xgrabz(sizeof(AREA)));
    pArea->lprfsHost = &pRddNode->pTable;
    pArea->rddID = uiRddID;

    if (SELF_STRUCTSIZE(pArea, &uiSize) != Harbour::SUCCESS)
    {
      return nullptr;
    }

    // Need more space?
    if (uiSize > sizeof(AREA))
    { // Size of Area changed
      pArea = static_cast<AREAP>(hb_xrealloc(pArea, uiSize));
      memset(pArea, 0, uiSize);
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;
    }

    pRddNode->uiAreaSize = uiSize; // Update the size of WorkArea
  }
  else
  {
    pArea = static_cast<AREAP>(hb_xgrabz(pRddNode->uiAreaSize));
    pArea->lprfsHost = &pRddNode->pTable;
    pArea->rddID = uiRddID;
  }

  if (SELF_NEW(pArea) != Harbour::SUCCESS)
  {
    SELF_RELEASE(pArea);
    return nullptr;
  }

  return static_cast<void *>(pArea);
}

HB_ERRCODE hb_rddGetTempAlias(char *szAliasTmp)
{
  int iArea;

  for (auto i = 1; i < 1000; i++)
  {
    hb_snprintf(szAliasTmp, 11, "__HBTMP%03i", i);
    if (hb_rddGetAliasNumber(szAliasTmp, &iArea) != Harbour::SUCCESS)
    {
      return Harbour::SUCCESS;
    }
  }
  szAliasTmp[0] = '\0';
  return Harbour::FAILURE;
}

// allocate and return atomAlias for new workarea or nullptr if alias already exist
void *hb_rddAllocWorkAreaAlias(const char *szAlias, int iArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddAllocWorkAreaAlias(%s, %d)", szAlias, iArea));
#endif

  int iDummyArea;

  // Verify if the alias name is valid symbol
  if (hb_rddVerifyAliasName(szAlias) != Harbour::SUCCESS)
  {
    hb_errRT_DBCMD_Ext(EG_BADALIAS, EDBCMD_BADALIAS, nullptr, szAlias, EF_CANDEFAULT);
    // Verify if the alias is already in use
  }
  else if (hb_rddGetAliasNumber(szAlias, &iDummyArea) == Harbour::SUCCESS)
  {
    hb_errRT_DBCMD_Ext(EG_DUPALIAS, EDBCMD_DUPALIAS, nullptr, szAlias, EF_CANDEFAULT);
  }
  else
  {
    auto pSymAlias = hb_dynsymGet(szAlias);

    if (hb_dynsymAreaHandle(pSymAlias) == 0)
    {
      hb_dynsymSetAreaHandle(pSymAlias, iArea);
      return pSymAlias;
    }
    hb_errRT_DBCMD_Ext(EG_DUPALIAS, EDBCMD_DUPALIAS, nullptr, szAlias, EF_CANDEFAULT);
  }

  return nullptr;
}

// Find a field index by name
HB_USHORT hb_rddFieldIndex(AREAP pArea, const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldIndex(%p, %s)", static_cast<void*>(pArea), szName));
#endif

  while (HB_ISSPACE(*szName))
  {
    ++szName;
  }

  if (*szName)
  {
    HB_SIZE nLen = strlen(szName);

    while (HB_ISSPACE(szName[nLen - 1]))
    {
      --nLen;
    }

    if (nLen <= HB_SYMBOL_NAME_LEN)
    {
      char szFieldName[HB_SYMBOL_NAME_LEN + 1];

      szFieldName[nLen] = '\0';
      while (nLen--)
      {
        szFieldName[nLen] = HB_TOUPPER(szName[nLen]);
      }

      auto pDynSym = hb_dynsymFind(szFieldName);
      if (pDynSym)
      {
        LPFIELD pField = pArea->lpFields;
        HB_USHORT uiCount = 0;

        while (pField)
        {
          ++uiCount;
          if (pDynSym == static_cast<PHB_DYNS>(pField->sym))
          {
            return uiCount;
          }
          pField = pField->lpfNext;
        }
      }
    }
  }
  return 0;
}

// find a field expression index, this function strips _FIELD->, FIELD->,
// alias-> prefixes
HB_USHORT hb_rddFieldExpIndex(AREAP pArea, const char *szField)
{
  while (HB_ISSPACE(*szField))
  {
    ++szField;
  }

  if (strchr(szField, '>') != nullptr)
  {
    char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];
    int j, l, n;

    n = 0;
    if (SELF_ALIAS(pArea, szAlias) == Harbour::SUCCESS)
    {
      l = static_cast<int>(strlen(szAlias));
    }
    else
    {
      l = 0;
    }

    // strip the _FIELD-> and FIELD-> prefix, it could be nested
    // so repeat this process until all prefixes will be removed
    do
    {
      int i;
      j = n;
      i = 0;
      if (HB_ISFIRSTIDCHAR(szField[n]))
      {
        ++i;
        while (HB_ISNEXTIDCHAR(szField[n + i]))
        {
          ++i;
        }

        if (!((i == l && hb_strnicmp(&szField[n], szAlias, l) == 0)) &&
            !(i >= 4 && i <= 5 && hb_strnicmp(&szField[n], "FIELD", i) == 0) &&
            !(i >= 4 && i <= 6 && hb_strnicmp(&szField[n], "_FIELD", i) == 0))
        {
          i = 0;
        }
      }

      if (i > 0)
      {
        i += n;
        while (HB_ISSPACE(szField[i]))
        {
          i++;
        }
        if (szField[i] == '-' && szField[i + 1] == '>')
        {
          n = i + 2;
          while (szField[n] == ' ')
          {
            n++;
          }
        }
      }
    } while (n != j);
    szField = &szField[n];
  }
  return hb_rddFieldIndex(pArea, szField);
}

// Find a WorkArea by the alias, return Harbour::FAILURE if not found
HB_ERRCODE hb_rddGetAliasNumber(const char *szAlias, int *iArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetAliasNumber(%s, %p)", szAlias, static_cast<void*>(iArea)));
#endif

  auto fOneLetter = false;
  char c;

  while (*szAlias == ' ')
  {
    szAlias++;
  }

  c = szAlias[0];
  if (c >= 'a' && c <= 'z')
  {
    c -= 'a' - 'A';
  }

  fOneLetter = c && (szAlias[1] == 0 || szAlias[1] == ' ');

  if (c >= '0' && c <= '9')
  {
    *iArea = atoi(szAlias);
  }
  else if (fOneLetter && c >= 'A' && c <= 'K')
  {
    *iArea = c - 'A' + 1;
  }
  else if (fOneLetter && c == 'M')
  {
    *iArea = HB_RDD_MAX_AREA_NUM;
  }
  else
  {
    auto pSymAlias = hb_dynsymFindName(szAlias);

    *iArea = pSymAlias ? static_cast<int>(hb_dynsymAreaHandle(pSymAlias)) : 0;
    if (*iArea == 0)
    {
      return Harbour::FAILURE;
    }
  }

  return Harbour::SUCCESS;
}

// Select a WorkArea by the symbol name.
HB_ERRCODE hb_rddSelectWorkAreaSymbol(PHB_SYMB pSymAlias)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", static_cast<void*>(pSymAlias)));
#endif

  const char *szName;

  auto iArea = static_cast<int>(hb_dynsymAreaHandle(pSymAlias->pDynSym));
  if (iArea)
  {
    hb_rddSelectWorkAreaNumber(iArea);
    return Harbour::SUCCESS;
  }

  szName = hb_dynsymName(pSymAlias->pDynSym);

  if (szName[0] && !szName[1])
  {
    if (szName[0] >= 'A' && szName[0] <= 'K')
    {
      hb_rddSelectWorkAreaNumber(szName[0] - 'A' + 1);
      return Harbour::SUCCESS;
    }
    else if (szName[0] >= 'a' && szName[0] <= 'k')
    {
      hb_rddSelectWorkAreaNumber(szName[0] - 'a' + 1);
      return Harbour::SUCCESS;
    }
    else if (szName[0] == 'M' || szName[0] == 'm')
    {
      hb_rddSelectWorkAreaNumber(HB_RDD_MAX_AREA_NUM);
      return Harbour::SUCCESS;
    }
  }

  // generate an error with retry possibility
  // (user created error handler can open a missing database)

  auto pError = hb_errRT_New(ES_ERROR, nullptr, EG_NOALIAS, EDBCMD_NOALIAS, nullptr, pSymAlias->szName, 0, EF_CANRETRY);
  HB_ERRCODE errCode = Harbour::FAILURE;

  do
  {
    if (hb_errLaunch(pError) != E_RETRY)
    {
      break;
    }
    iArea = static_cast<int>(hb_dynsymAreaHandle(pSymAlias->pDynSym));
    if (iArea)
    {
      hb_rddSelectWorkAreaNumber(iArea);
      errCode = Harbour::SUCCESS;
    }
  } while (errCode == Harbour::FAILURE);

  hb_itemRelease(pError);

  return errCode;
}

// Select a WorkArea by the name.
HB_ERRCODE hb_rddSelectWorkAreaAlias(const char *szAlias)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szAlias));
#endif

  int iArea;

  HB_ERRCODE errCode = hb_rddGetAliasNumber(szAlias, &iArea);

  if (errCode == Harbour::FAILURE)
  {
    // generate an error with retry possibility
    // (user created error handler can open a missing database)
    auto pError = hb_errRT_New(ES_ERROR, nullptr, EG_NOALIAS, EDBCMD_NOALIAS, nullptr, szAlias, 0, EF_CANRETRY);

    do
    {
      if (hb_errLaunch(pError) != E_RETRY)
      {
        break;
      }
      errCode = hb_rddGetAliasNumber(szAlias, &iArea);
    } while (errCode == Harbour::FAILURE);

    hb_itemRelease(pError);
  }

  if (errCode == Harbour::SUCCESS)
  {
    if (iArea < 1 || iArea > HB_RDD_MAX_AREA_NUM)
    {
      errCode = hb_rddSelectFirstAvailable();
    }
    else
    {
      errCode = hb_rddSelectWorkAreaNumber(iArea);
    }
  }

  return errCode;
}

// Obtain the current value of a field.
HB_ERRCODE hb_rddFieldGet(PHB_ITEM pItem, PHB_SYMB pFieldSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pFieldSymbol)));
#endif

  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  if (pArea != nullptr)
  {
    HB_USHORT uiField = 1;
    LPFIELD pField = pArea->lpFields;
    PHB_DYNS pDynSym = pFieldSymbol->pDynSym;

    while (pField)
    {
      if (static_cast<PHB_DYNS>(pField->sym) == pDynSym)
      {
        return SELF_GETVALUE(pArea, uiField, pItem);
      }
      ++uiField;
      pField = pField->lpfNext;
    }
  }
  return Harbour::FAILURE;
}

// Assign a value to a field.
HB_ERRCODE hb_rddFieldPut(PHB_ITEM pItem, PHB_SYMB pFieldSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pFieldSymbol)));
#endif

  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  if (pArea != nullptr)
  {
    HB_USHORT uiField = 1;
    LPFIELD pField = pArea->lpFields;
    PHB_DYNS pDynSym = pFieldSymbol->pDynSym;

    while (pField)
    {
      if (static_cast<PHB_DYNS>(pField->sym) == pDynSym)
      {
        return SELF_PUTVALUE(pArea, uiField, pItem);
      }
      ++uiField;
      pField = pField->lpfNext;
    }
  }
  return Harbour::FAILURE;
}

// Obtain the current value of a field.
HB_ERRCODE hb_rddGetFieldValue(PHB_ITEM pItem, PHB_SYMB pFieldSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pFieldSymbol)));
#endif

  HB_ERRCODE errCode = hb_rddFieldGet(pItem, pFieldSymbol);

  if (errCode == Harbour::FAILURE && hb_vmRequestQuery() == 0)
  {
    // generate an error with retry possibility
    // (user created error handler can make this field accessible)
    auto pError =
        hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, EDBCMD_NOVAR, nullptr, pFieldSymbol->szName, 0, EF_CANRETRY);
    hb_itemClear(pItem);

    while (hb_errLaunch(pError) == E_RETRY)
    {
      errCode = hb_rddFieldGet(pItem, pFieldSymbol);

      if (errCode == Harbour::SUCCESS || hb_vmRequestQuery() != 0)
      {
        break;
      }
    }
    hb_itemRelease(pError);
  }

  return errCode;
}

// Assign a value to a field.
HB_ERRCODE hb_rddPutFieldValue(PHB_ITEM pItem, PHB_SYMB pFieldSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pFieldSymbol)));
#endif

  HB_ERRCODE errCode = hb_rddFieldPut(pItem, pFieldSymbol);

  if (errCode == Harbour::FAILURE && hb_vmRequestQuery() == 0)
  {
    // generate an error with retry possibility
    // (user created error handler can make this field accessible)
    auto pError =
        hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, EDBCMD_NOVAR, nullptr, pFieldSymbol->szName, 0, EF_CANRETRY);

    while (hb_errLaunch(pError) == E_RETRY)
    {
      errCode = hb_rddFieldPut(pItem, pFieldSymbol);

      if (errCode == Harbour::SUCCESS || hb_vmRequestQuery() != 0)
      {
        break;
      }
    }
    hb_itemRelease(pError);
  }

  return errCode;
}

HB_ERRCODE hb_rddOpenTable(const char *szFileName, const char *szDriver, HB_USHORT uiArea, const char *szAlias,
                           HB_BOOL fShared, HB_BOOL fReadonly, const char *szCpId, HB_ULONG ulConnection,
                           PHB_ITEM pStruct, PHB_ITEM pDelim)
{
  DBOPENINFO pInfo;

  // uiArea = 0 in hb_rddInsertAreaNode() means chose first
  // available free area, otherwise we should close table in
  // current WA and it should be done before parameter validation
  // RT errors below. This breaks xHarbour like MT code which
  // shares WA between threads so dbUseArea() should be covered
  // by external mutex to make lNewArea MT safe, [druzus]
  if (uiArea && uiArea < HB_RDD_MAX_AREA_NUM)
  {
    hb_rddSelectWorkAreaNumber(uiArea);
    hb_rddReleaseCurrentArea();
  }
  else if (hb_rddSelectFirstAvailable() != Harbour::SUCCESS)
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }

  // Clipper clears NETERR flag before parameter validation, [druzus]
  hb_rddSetNetErr(false);

  // Now check parameters, first RDD name.
  // Clipper seems to make something like:
  //    if( szDriver && strlen(szDriver) > 1 )
  // but I do not think we should replicate it, [druzus]
  szDriver = hb_rddFindDrv(szDriver, szFileName);

  // First try to create new area node and validate RDD name
  if (!szDriver || !hb_rddInsertAreaNode(szDriver))
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }

  // Then check if valid file name was given - Clipper allows to use empty
  // ("") file name
  if (!szFileName)
  {
    hb_rddReleaseCurrentArea();
    hb_errRT_DBCMD(EG_ARG, EDBCMD_USE_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }

  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  // Fill pInfo structure
  pInfo.uiArea = pArea->uiArea;
  pInfo.abName = szFileName;
  pInfo.atomAlias = szAlias;
  pInfo.fShared = fShared;
  pInfo.fReadonly = fReadonly;
  pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
  pInfo.ulConnection = ulConnection;
  pInfo.lpdbHeader = nullptr;

  HB_ERRCODE errCode = pStruct ? SELF_CREATEFIELDS(pArea, pStruct) : Harbour::SUCCESS;
  if (errCode == Harbour::SUCCESS)
  {
    if (pDelim && !pDelim->isNil())
    {
      errCode = SELF_INFO(pArea, DBI_SETDELIMITER, pDelim);
    }
    if (errCode == Harbour::SUCCESS)
    {
      // Open file
      errCode = SELF_OPEN(pArea, &pInfo);
    }
  }

  if (errCode != Harbour::SUCCESS)
  {
    hb_rddReleaseCurrentArea();
  }

  return errCode;
}

HB_ERRCODE hb_rddCreateTable(const char *szFileName, const char *szDriver, HB_USHORT uiArea, const char *szAlias,
                             HB_BOOL fKeepOpen, const char *szCpId, HB_ULONG ulConnection, PHB_ITEM pStruct,
                             PHB_ITEM pDelim)
{
  DBOPENINFO pInfo;
  HB_ERRCODE errCode;
  HB_USHORT uiPrevArea;

  if (!szFileName)
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }

  uiPrevArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());

  // 0 means chose first available in hb_rddInsertAreaNode()
  hb_rddSelectWorkAreaNumber(uiArea);
  if (uiArea)
  {
    hb_rddReleaseCurrentArea();
  }

  szDriver = hb_rddFindDrv(szDriver, szFileName);

  // Create a new WorkArea node
  if (!szDriver || !hb_rddInsertAreaNode(szDriver))
  {
    hb_rddSelectWorkAreaNumber(uiPrevArea);
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  // Fill pInfo structure
  pInfo.uiArea = pArea->uiArea;
  pInfo.abName = szFileName;
  pInfo.atomAlias = szAlias;
  pInfo.fShared = false;
  pInfo.fReadonly = false;
  pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
  pInfo.ulConnection = ulConnection;
  pInfo.lpdbHeader = nullptr;

  if (pDelim && !pDelim->isNil())
  {
    errCode = SELF_INFO(pArea, DBI_SETDELIMITER, pDelim);
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }

  if (errCode == Harbour::SUCCESS)
  {
    errCode = SELF_CREATEFIELDS(pArea, pStruct);
    if (errCode == Harbour::SUCCESS)
    {
      errCode = SELF_CREATE(pArea, &pInfo);
    }
  }

  if (!fKeepOpen || errCode != Harbour::SUCCESS)
  {
    hb_rddReleaseCurrentArea();
    hb_rddSelectWorkAreaNumber(uiPrevArea);
  }

  return errCode;
}

HB_ERRCODE hb_rddCreateTableTemp(const char *szDriver, const char *szAlias, const char *szCpId, HB_ULONG ulConnection,
                                 PHB_ITEM pStruct)
{
  char szDriverBuffer[HB_RDD_MAX_DRIVERNAME_LEN + 1];
  DBOPENINFO pInfo;
  HB_USHORT uiPrevArea;

  uiPrevArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());

  // 0 means chose first available in hb_rddInsertAreaNode()
  hb_rddSelectWorkAreaNumber(0);

  if (szDriver != nullptr && szDriver[0])
  {
    hb_strncpyUpper(szDriverBuffer, szDriver, sizeof(szDriverBuffer) - 1);
    szDriver = szDriverBuffer;
  }
  else
  {
    szDriver = hb_rddDefaultDrv(nullptr);
  }

  // Create a new WorkArea node
  if (!hb_rddInsertAreaNode(szDriver))
  {
    hb_rddSelectWorkAreaNumber(uiPrevArea);
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return Harbour::FAILURE;
  }
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  // Fill pInfo structure
  pInfo.uiArea = pArea->uiArea;
  pInfo.abName = nullptr;
  pInfo.atomAlias = szAlias;
  pInfo.fShared = false;
  pInfo.fReadonly = false;
  pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
  pInfo.ulConnection = ulConnection;
  pInfo.lpdbHeader = nullptr;

  auto pItem = hb_itemPutL(nullptr, true);
  HB_ERRCODE errCode = SELF_INFO(pArea, DBI_ISTEMPORARY, pItem);
  hb_itemRelease(pItem);

  if (errCode == Harbour::SUCCESS)
  {
    errCode = SELF_CREATEFIELDS(pArea, pStruct);
    if (errCode == Harbour::SUCCESS)
    {
      errCode = SELF_CREATE(pArea, &pInfo);
    }
  }

  if (errCode != Harbour::SUCCESS)
  {
    hb_rddReleaseCurrentArea();
    hb_rddSelectWorkAreaNumber(uiPrevArea);
  }

  return errCode;
}

static void hb_fldStructure(AREAP pArea, HB_USHORT uiField, HB_USHORT uiSize, PHB_ITEM pField)
{
#ifdef DBS_FLAG
  static const HB_USHORT s_uiActions[] = {DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC, DBS_FLAG};
#else
  static const HB_USHORT s_uiActions[] = {DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC};
#endif

  if (uiSize == 0 || uiSize > HB_SIZEOFARRAY(s_uiActions))
  {
    uiSize = HB_SIZEOFARRAY(s_uiActions);
  }

  hb_arrayNew(pField, uiSize);
  for (HB_USHORT uiCount = 0; uiCount < uiSize; ++uiCount)
  {
    SELF_FIELDINFO(pArea, uiField, s_uiActions[uiCount], hb_arrayGetItemPtr(pField, uiCount + 1));
  }
}

void hb_tblStructure(AREAP pArea, PHB_ITEM pStruct, HB_USHORT uiSize)
{
  HB_USHORT uiFields;

  if (SELF_FIELDCOUNT(pArea, &uiFields) == Harbour::SUCCESS)
  {
    if (hb_arraySize(pStruct, uiFields))
    {
      for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
      {
        hb_fldStructure(pArea, uiCount, uiSize, hb_arrayGetItemPtr(pStruct, uiCount));
      }
    }
  }
}

static const char *hb_dbTransFieldPos(PHB_ITEM pFields, HB_USHORT uiField)
{
  const char *szField = nullptr;

  auto pItem = hb_arrayGetItemPtr(pFields, uiField);
  if (pItem != nullptr)
  {
    if (pItem->isArray())
    {
      szField = hb_arrayGetCPtr(pItem, DBS_NAME);
    }
    else
    {
      szField = pItem->getCPtr();
    }

    if (*szField == '\0')
    {
      szField = nullptr;
    }
  }

  return szField;
}

static const HB_GC_FUNCS s_gcTransInfo = {hb_gcDummyClear, hb_gcDummyMark};

PHB_ITEM hb_dbTransInfoPut(PHB_ITEM pItem, LPDBTRANSINFO lpdbTransInfo)
{
  LPDBTRANSINFO *pHolder;

  pHolder = static_cast<LPDBTRANSINFO *>(hb_gcAllocate(sizeof(LPDBTRANSINFO), &s_gcTransInfo));
  *pHolder = lpdbTransInfo;

  return hb_itemPutPtrGC(pItem, pHolder);
}

LPDBTRANSINFO hb_dbTransInfoGet(PHB_ITEM pItem)
{
  LPDBTRANSINFO *pHolder = static_cast<LPDBTRANSINFO *>(hb_itemGetPtrGC(pItem, &s_gcTransInfo));

  return pHolder ? *pHolder : nullptr;
}

// update counters for autoinc and rowver fields
HB_ERRCODE hb_dbTransCounters(LPDBTRANSINFO lpdbTransInfo)
{
  auto pItem = hb_itemNew(nullptr);

  for (HB_USHORT uiCount = 0; uiCount < lpdbTransInfo->uiItemCount; ++uiCount)
  {
    LPDBTRANSITEM lpdbTransItem = &lpdbTransInfo->lpTransItems[uiCount];

    if (SELF_FIELDINFO(lpdbTransInfo->lpaSource, lpdbTransItem->uiSource, DBS_COUNTER, pItem) == Harbour::SUCCESS &&
        SELF_FIELDINFO(lpdbTransInfo->lpaDest, lpdbTransItem->uiDest, DBS_COUNTER, pItem) == Harbour::SUCCESS)
    {
      hb_itemClear(pItem);
      if (SELF_FIELDINFO(lpdbTransInfo->lpaSource, lpdbTransItem->uiSource, DBS_STEP, pItem) == Harbour::SUCCESS)
      {
        SELF_FIELDINFO(lpdbTransInfo->lpaDest, lpdbTransItem->uiDest, DBS_STEP, pItem);
      }
    }
    hb_itemClear(pItem);
  }
  hb_itemRelease(pItem);

  return Harbour::SUCCESS;
}

HB_ERRCODE hb_dbTransStruct(AREAP lpaSource, AREAP lpaDest, LPDBTRANSINFO lpdbTransInfo, PHB_ITEM *pStruct,
                            PHB_ITEM pFields)
{
  HB_USHORT uiFields, uiSize, uiCount, uiPosSrc, uiPosDst, uiSizeSrc, uiSizeDst;
  const char *szField;
  auto fAll = false;

  HB_ERRCODE errCode = SELF_FIELDCOUNT(lpaSource, &uiSizeSrc);
  if (errCode != Harbour::SUCCESS)
  {
    return errCode;
  }

  if (lpaDest)
  {
    errCode = SELF_FIELDCOUNT(lpaDest, &uiSizeDst);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }
    uiSize = HB_MIN(uiSizeDst, uiSizeSrc);
  }
  else
  {
    uiSize = uiSizeDst = uiSizeSrc;
  }

  if (!uiSize)
  {
    return Harbour::FAILURE;
  }
  if (hb_itemType(pFields) & Harbour::Item::ARRAY)
  {
    uiFields = static_cast<HB_USHORT>(hb_arrayLen(pFields));
    if (uiFields)
    {
      uiSize = uiFields;
    }
  }
  else
  {
    uiFields = 0;
  }

  fAll = (uiSizeDst == uiSizeSrc);

  lpdbTransInfo->lpaSource = lpaSource;
  lpdbTransInfo->lpaDest = lpaDest;
  lpdbTransInfo->lpTransItems = static_cast<LPDBTRANSITEM>(hb_xgrab(uiSize * sizeof(DBTRANSITEM)));

  if (!lpaDest)
  {
    *pStruct = hb_itemNew(nullptr);
    hb_arrayNew(*pStruct, 0);
  }

  if (uiFields == 0)
  {
    if (lpaDest)
    {
      auto pItem = hb_itemNew(nullptr);
      uiSize = 0;
      for (uiCount = 1; uiCount <= uiSizeSrc; ++uiCount)
      {
        if (SELF_FIELDINFO(lpaSource, uiCount, DBS_NAME, pItem) != Harbour::SUCCESS)
        {
          uiSize = 0;
          break;
        }
        szField = hb_itemGetCPtr(pItem);
        uiPosDst = hb_rddFieldExpIndex(lpaDest, szField);
        if (uiPosDst != uiCount)
        {
          fAll = false;
        }
        if (uiPosDst)
        {
          HB_USHORT ui;

          // check for replicated field names in source area
          for (ui = 0; ui < uiSize; ++ui)
          {
            if (lpdbTransInfo->lpTransItems[ui].uiDest == uiPosDst)
            {
              break;
            }
          }
          if (ui == uiSize)
          {
            lpdbTransInfo->lpTransItems[uiSize].uiSource = uiCount;
            lpdbTransInfo->lpTransItems[uiSize++].uiDest = uiPosDst;
          }
        }
      }
      hb_itemRelease(pItem);
    }
    else
    {
      hb_tblStructure(lpaSource, *pStruct, 0);
      uiSize = static_cast<HB_USHORT>(hb_arrayLen(*pStruct));
      for (uiCount = 0; uiCount < uiSize; ++uiCount)
      {
        lpdbTransInfo->lpTransItems[uiCount].uiSource = lpdbTransInfo->lpTransItems[uiCount].uiDest = uiCount + 1;
      }
    }
  }
  else
  {
    uiSize = 0;
    for (uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      szField = hb_dbTransFieldPos(pFields, uiCount);
      if (szField != nullptr)
      {
        uiPosSrc = hb_rddFieldExpIndex(lpaSource, szField);
        if (!uiPosSrc)
        {
          continue;
        }
        if (lpaDest)
        {
          uiPosDst = hb_rddFieldExpIndex(lpaDest, szField);
        }
        else
        {
          uiPosDst = uiSize + 1;
        }
        if (uiPosDst)
        {
          if (uiPosSrc != uiPosDst)
          {
            fAll = false;
          }
          lpdbTransInfo->lpTransItems[uiSize].uiSource = uiPosSrc;
          lpdbTransInfo->lpTransItems[uiSize++].uiDest = uiPosDst;
          if (!lpaDest)
          {
            hb_arraySize(*pStruct, uiSize);
            hb_fldStructure(lpaSource, uiPosSrc, 0, hb_arrayGetItemPtr(*pStruct, uiSize));
          }
        }
      }
    }
  }

  if (uiSize != uiSizeSrc)
  {
    fAll = false;
  }

  if (fAll && lpaDest)
  {
    auto pSrcItm = hb_itemNew(nullptr);
    auto pDstItm = hb_itemNew(nullptr);
    // if fAll is HB_TRUE here then it means that all fields are included
    // and they are on the same positions in both tables, so now check
    // if their types and sizes are also equal
    for (uiCount = 1; uiCount <= uiSize; ++uiCount)
    {
      if (SELF_FIELDINFO(lpaSource, uiCount, DBS_TYPE, pSrcItm) != Harbour::SUCCESS ||
          SELF_FIELDINFO(lpaDest, uiCount, DBS_TYPE, pDstItm) != Harbour::SUCCESS)
      {
        uiSize = 0;
        break;
      }
      if (hb_stricmp(hb_itemGetCPtr(pSrcItm), hb_itemGetCPtr(pDstItm)) != 0)
      {
        fAll = false;
        break;
      }
      if (SELF_FIELDINFO(lpaSource, uiCount, DBS_LEN, pSrcItm) != Harbour::SUCCESS ||
          SELF_FIELDINFO(lpaDest, uiCount, DBS_LEN, pDstItm) != Harbour::SUCCESS)
      {
        uiSize = 0;
        break;
      }
      if (hb_itemGetNL(pSrcItm) != hb_itemGetNL(pDstItm))
      {
        fAll = false;
        break;
      }
      if (SELF_FIELDINFO(lpaSource, uiCount, DBS_DEC, pSrcItm) != Harbour::SUCCESS ||
          SELF_FIELDINFO(lpaDest, uiCount, DBS_DEC, pDstItm) != Harbour::SUCCESS)
      {
        uiSize = 0;
        break;
      }
      if (hb_itemGetNL(pSrcItm) != hb_itemGetNL(pDstItm))
      {
        fAll = false;
        break;
      }
#ifdef DBS_FLAG
      if (SELF_FIELDINFO(lpaSource, uiCount, DBS_FLAG, pSrcItm) != Harbour::SUCCESS ||
          SELF_FIELDINFO(lpaDest, uiCount, DBS_FLAG, pDstItm) != Harbour::SUCCESS)
      {
        uiSize = 0;
        break;
      }
      if (hb_itemGetNL(pSrcItm) != hb_itemGetNL(pDstItm))
      {
        fAll = false;
        break;
      }
#endif
    }
    hb_itemRelease(pSrcItm);
    hb_itemRelease(pDstItm);
  }

  lpdbTransInfo->uiFlags = fAll ? DBTF_MATCH : 0;
  lpdbTransInfo->uiItemCount = uiSize;

  return uiSize ? Harbour::SUCCESS : Harbour::FAILURE;
}

HB_ERRCODE hb_rddTransRecords(AREAP pArea, const char *szFileName, const char *szDriver, HB_ULONG ulConnection,
                              PHB_ITEM pFields, HB_BOOL fExport, PHB_ITEM pCobFor, PHB_ITEM pStrFor, PHB_ITEM pCobWhile,
                              PHB_ITEM pStrWhile, PHB_ITEM pNext, PHB_ITEM pRecID, PHB_ITEM pRest, const char *szCpId,
                              PHB_ITEM pDelim)
{
  AREAP lpaClose = nullptr;
  PHB_ITEM pStruct = nullptr;
  DBTRANSINFO dbTransInfo{};
  HB_USHORT uiPrevArea;
  HB_ERRCODE errCode;

  uiPrevArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());

  szDriver = hb_rddFindDrv(szDriver, szFileName);

  if (fExport)
  {
    errCode = hb_dbTransStruct(pArea, nullptr, &dbTransInfo, &pStruct, pFields);
    if (errCode == Harbour::SUCCESS)
    {
      errCode = hb_rddCreateTable(szFileName, szDriver, 0, "", true, szCpId, ulConnection, pStruct, pDelim);
      if (errCode == Harbour::SUCCESS)
      {
        dbTransInfo.lpaDest = lpaClose = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
      }
    }
  }
  else
  {
    LPRDDNODE pRddNode = hb_rddFindNode(szDriver, nullptr);

    if (!pRddNode)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_USE_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return Harbour::FAILURE;
    }

    if (pRddNode->uiType == RDT_TRANSFER)
    {
      errCode = hb_dbTransStruct(pArea, nullptr, &dbTransInfo, &pStruct, pFields);

      // revert area and items
      dbTransInfo.lpaDest = dbTransInfo.lpaSource;
      for (HB_USHORT uiCount = 0; uiCount < dbTransInfo.uiItemCount; ++uiCount)
      {
        HB_USHORT uiSwap = dbTransInfo.lpTransItems[uiCount].uiSource;
        dbTransInfo.lpTransItems[uiCount].uiSource = dbTransInfo.lpTransItems[uiCount].uiDest;
        dbTransInfo.lpTransItems[uiCount].uiDest = uiSwap;
      }

      if (errCode == Harbour::SUCCESS)
      {
        errCode = hb_rddOpenTable(szFileName, szDriver, 0, "", true, true, szCpId, ulConnection, pStruct, pDelim);
        if (errCode == Harbour::SUCCESS)
        {
          lpaClose = dbTransInfo.lpaSource = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
        }
      }
    }
    else
    {
      errCode = hb_rddOpenTable(szFileName, szDriver, 0, "", true, true, szCpId, ulConnection, nullptr, pDelim);
      if (errCode == Harbour::SUCCESS)
      {
        lpaClose = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
        errCode = hb_dbTransStruct(lpaClose, pArea, &dbTransInfo, nullptr, pFields);
      }
    }
  }

  if (pStruct)
  {
    hb_itemRelease(pStruct);
  }

  if (errCode == Harbour::SUCCESS)
  {
    PHB_ITEM pTransItm;

    hb_rddSelectWorkAreaNumber(dbTransInfo.lpaSource->uiArea);

    dbTransInfo.dbsci.itmCobFor = pCobFor;
    dbTransInfo.dbsci.lpstrFor = pStrFor;
    dbTransInfo.dbsci.itmCobWhile = pCobWhile;
    dbTransInfo.dbsci.lpstrWhile = pStrWhile;
    dbTransInfo.dbsci.lNext = pNext;
    dbTransInfo.dbsci.itmRecID = pRecID;
    dbTransInfo.dbsci.fRest = pRest;

    dbTransInfo.dbsci.fIgnoreFilter = true;
    dbTransInfo.dbsci.fIncludeDeleted = true;
    dbTransInfo.dbsci.fLast = false;
    dbTransInfo.dbsci.fIgnoreDuplicates = false;
    dbTransInfo.dbsci.fBackward = false;

    pTransItm = hb_dbTransInfoPut(nullptr, &dbTransInfo);
    errCode = SELF_INFO(dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm);
    if (errCode == Harbour::SUCCESS)
    {
      errCode = dbTransInfo.uiItemCount == 0 ? Harbour::FAILURE : SELF_TRANS(dbTransInfo.lpaSource, &dbTransInfo);
      // we always call DBI_TRANSREC second time after TRANS() method
      // even if TRANS() failed - it's for RDDs which may need to store
      // pointer to dbTransInfo in first call and then release it and/or
      // clean some structures allocated for transfer operation [druzus]
      SELF_INFO(dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm);
      if (errCode == Harbour::SUCCESS && (dbTransInfo.uiFlags & DBTF_CPYCTR))
      {
        errCode = hb_dbTransCounters(&dbTransInfo);
      }
    }
    hb_itemRelease(pTransItm);
  }

  if (dbTransInfo.lpTransItems)
  {
    hb_xfree(dbTransInfo.lpTransItems);
  }
  if (lpaClose)
  {
    hb_rddSelectWorkAreaNumber(lpaClose->uiArea);
    hb_rddReleaseCurrentArea();
  }
  hb_rddSelectWorkAreaNumber(uiPrevArea);

  return errCode;
}

static HB_ERRCODE hb_rddCloseParentRel(AREAP pArea, void *pChildArea)
{
  if (pArea->lpdbRelations)
  {
    LPDBRELINFO *lpdbRelationPtr = &pArea->lpdbRelations;
    HB_USHORT uiArea = (static_cast<AREAP>(pChildArea))->uiArea;

    do
    {
      LPDBRELINFO lpdbRelation = *lpdbRelationPtr;

      if (lpdbRelation->lpaChild->uiArea == uiArea)
      {
        // Clear this relation
        hb_rddSelectWorkAreaNumber(lpdbRelation->lpaChild->uiArea);
        SELF_CHILDEND(lpdbRelation->lpaChild, lpdbRelation);
        if (lpdbRelation->itmCobExpr)
        {
          hb_itemRelease(lpdbRelation->itmCobExpr);
        }
        if (lpdbRelation->abKey)
        {
          hb_itemRelease(lpdbRelation->abKey);
        }

        *lpdbRelationPtr = lpdbRelation->lpdbriNext;
        hb_xfree(lpdbRelation);
      }
      else
      {
        lpdbRelationPtr = &lpdbRelation->lpdbriNext;
      }
    } while (*lpdbRelationPtr);
  }
  return Harbour::SUCCESS;
}

// close all parent relations
HB_ERRCODE hb_rddCloseAllParentRelations(AREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAllParentRelations(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->uiParents > 0)
  {
    HB_USHORT uiArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());
    errCode = hb_rddIterateWorkAreas(hb_rddCloseParentRel, pArea);
    hb_rddSelectWorkAreaNumber(uiArea);
  }

  return errCode;
}

static HB_ERRCODE hb_rddEvalWABlock(AREAP pArea, void *pBlock)
{
  hb_rddSelectWorkAreaNumber(pArea->uiArea);
  auto pItem = hb_vmEvalBlockOrMacro(static_cast<PHB_ITEM>(pBlock));

  if (hb_vmRequestQuery() != 0 || (pItem->isLogical() && !pItem->getL()))
  {
    return Harbour::FAILURE;
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

HB_ERRCODE hb_rddEvalWA(PHB_ITEM pBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rddEvalWA(%p)", static_cast<void*>(pBlock)));
#endif

  HB_USHORT uiArea;

  uiArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());
  HB_ERRCODE errCode = hb_rddIterateWorkAreas(hb_rddEvalWABlock, pBlock);
  hb_rddSelectWorkAreaNumber(uiArea);

  return errCode;
}
