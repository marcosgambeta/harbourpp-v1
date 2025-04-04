//
// SIX compatible functions:
//       sx_MakeSem()
//       sx_KillSem()
//       sx_IsSem()
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapifs.hpp"
#include "hbapirdd.hpp"

static bool hb_sxSemName(char *szFileName)
{
  auto szName = hb_parc(1);
  auto fResult = false;

  if (szName != nullptr && szName[0])
  {
    hb_cdpnDup2Lower(hb_vmCDP(), szName, strlen(szName), szFileName, HB_PATH_MAX);
    szFileName[HB_PATH_MAX - 1] = '\0';
    fResult = true;
  }
  else
  {
    auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

    if (pArea != nullptr)
    {
      DBORDERINFO pOrderInfo{};

      pOrderInfo.itmOrder = hb_param(1, Harbour::Item::NUMERIC);
      if (pOrderInfo.itmOrder && pOrderInfo.itmOrder->getNI() == 0)
      {
        pOrderInfo.itmOrder = nullptr;
      }
      pOrderInfo.itmResult = hb_itemPutC(nullptr, nullptr);
      SELF_ORDINFO(pArea, DBOI_NAME, &pOrderInfo);
      szName = hb_itemGetCPtr(pOrderInfo.itmResult);
      if (szName != nullptr && szName[0])
      {
        hb_cdpnDup2Lower(hb_vmCDP(), szName, strlen(szName), szFileName, HB_PATH_MAX);
        szFileName[HB_PATH_MAX - 1] = '\0';
        fResult = true;
      }
      hb_itemRelease(pOrderInfo.itmResult);
    }
  }

  return fResult;
}

static PHB_FILE hb_sxSemOpen(char *szFileName, HB_BOOL *pfNewFile)
{
  PHB_FILE pFile;
  int i = 0;

  do
  {
    pFile = hb_fileExtOpen(szFileName, ".sem",
                           FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME, nullptr, nullptr);
    if (pFile != nullptr)
    {
      break;
    }

    if (pfNewFile)
    {
      pFile = hb_fileExtOpen(szFileName, ".sem",
                             FXO_UNIQUE | FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME,
                             nullptr, nullptr);
      if (pFile != nullptr)
      {
        *pfNewFile = HB_TRUE;
        break;
      }
    }
    else
    {
      HB_ERRCODE errCode = hb_fsError();
      if (errCode != 5 && errCode != 32 && errCode != 33)
      {
        break;
      }
    }

    hb_idleSleep(0.01);
  } while (++i < 25);

  return pFile;
}

HB_FUNC(SX_MAKESEM)
{
  char szFileName[HB_PATH_MAX];
  int iUsers = -1;
  HB_BOOL fError = false, fNewFile = false;

  if (hb_sxSemName(szFileName))
  {
    PHB_FILE pFile = hb_sxSemOpen(szFileName, &fNewFile);

    if (pFile != nullptr)
    {
      HB_BYTE buffer[2];

      if (fNewFile)
      {
        iUsers = 1;
      }
      else
      {
        if (hb_fileReadAt(pFile, buffer, 2, 0) != 2)
        {
          fError = true;
        }
        else
        {
          iUsers = HB_GET_LE_INT16(buffer) + 1;
        }
      }
      if (!fError)
      {
        HB_PUT_LE_UINT16(buffer, iUsers);
        if (hb_fileWriteAt(pFile, buffer, 2, 0) != 2)
        {
          fError = true;
        }
      }
      hb_fileClose(pFile);
    }
  }
  if (fError)
  {
    iUsers = -1;
  }
  hb_retni(iUsers);
}

HB_FUNC(SX_KILLSEM)
{
  char szFileName[HB_PATH_MAX];
  int iUsers = -1;

  if (hb_sxSemName(szFileName))
  {
    PHB_FILE pFile = hb_sxSemOpen(szFileName, nullptr);

    if (pFile != nullptr)
    {
      HB_BYTE buffer[2];
      if (hb_fileReadAt(pFile, buffer, 2, 0) == 2)
      {
        iUsers = HB_GET_LE_INT16(buffer) - 1;
        HB_PUT_LE_UINT16(buffer, iUsers);
        hb_fileWriteAt(pFile, buffer, 2, 0);
      }
      hb_fileClose(pFile);
      if (iUsers == 0)
      {
        hb_fileDelete(szFileName);
      }
    }
  }
  hb_retni(iUsers);
}

HB_FUNC(SX_ISSEM)
{
  char szFileName[HB_PATH_MAX];
  PHB_FILE pFile = nullptr;

  if (hb_sxSemName(szFileName))
  {
    pFile = hb_sxSemOpen(szFileName, nullptr);
    if (pFile != nullptr)
    {
      hb_fileClose(pFile);
    }
  }

  hb_retl(pFile != nullptr);
}
