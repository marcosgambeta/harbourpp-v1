//
// SIX compatible functions:
//       hb_sxEnCrypt()
//       hb_sxDeCrypt()
//
//       sx_Encrypt()
//       sx_Decrypt()
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

#include "hbsxfunc.hpp"

#define rnd_mul1 0x0de6d
#define rnd_mul2 0x0278d

static HB_U32 hb_sxInitSeed(const char *pKeyVal, HB_U16 *puiKey)
{
  HB_U32 ulSeed = 0;

  for (auto i = 0; i < 7; i++)
  {
    ulSeed = (((ulSeed >> 16) + (ulSeed << 16)) * 17) + HB_GET_LE_UINT16(&pKeyVal[i]);
  }
  ulSeed |= 1;
  *puiKey = static_cast<HB_U16>(ulSeed);
  return (ulSeed << 16) + (ulSeed >> 16);
}

static HB_U32 hb_sxNextSeed(HB_U32 ulSeed, const char *pKeyVal, HB_U16 *puiKey)
{
  HB_U32 ulTemp1, ulTemp2;
  HB_U16 uiSeedLo, uiSeedHi;

  uiSeedLo = static_cast<HB_U16>(ulSeed);
  ulTemp1 = static_cast<HB_U32>(rnd_mul1) * static_cast<HB_U32>(uiSeedLo);
  ulTemp2 = static_cast<HB_U32>(rnd_mul2) * static_cast<HB_U32>(uiSeedLo) + (ulTemp1 >> 16);
  uiSeedLo = static_cast<HB_U16>(ulTemp1);
  ulTemp1 = static_cast<HB_U32>(rnd_mul1) * (ulSeed >> 16);
  uiSeedHi = static_cast<HB_U16>(ulTemp1 + ulTemp2);
  ulSeed = (static_cast<HB_U32>(uiSeedHi) << 16) + static_cast<HB_U32>(uiSeedLo);
  uiSeedHi |= 1;
  *puiKey = uiSeedHi + HB_GET_LE_UINT16(pKeyVal);
  return ulSeed;
}

void hb_sxEnCrypt(const char *pSrc, char *pDst, const char *pKeyVal, HB_SIZE nLen)
{
  HB_U32 ulSeed;
  HB_U16 uiKey;
  HB_SIZE nPos;
  int i;

  ulSeed = hb_sxInitSeed(pKeyVal, &uiKey);
  for (nPos = 0, i = 0; nPos < nLen; nPos++)
  {
    HB_UCHAR ucChar, ucShft;

    ucChar = static_cast<HB_UCHAR>(pSrc[nPos]);
    ucShft = static_cast<HB_UCHAR>(uiKey & 0x07);
    pDst[nPos] = ((ucChar >> ucShft) + (ucChar << (8 - ucShft)) + (uiKey & 0xFF));
    ulSeed = hb_sxNextSeed(ulSeed, &pKeyVal[i], &uiKey);
    if (++i == 7)
    {
      i = 0;
    }
  }
}

void hb_sxDeCrypt(const char *pSrc, char *pDst, const char *pKeyVal, HB_SIZE nLen)
{
  HB_U32 ulSeed;
  HB_U16 uiKey;
  HB_SIZE nPos;
  int i;

  ulSeed = hb_sxInitSeed(pKeyVal, &uiKey);
  for (nPos = 0, i = 0; nPos < nLen; nPos++)
  {
    HB_UCHAR ucChar, ucShft;

    ucChar = static_cast<HB_UCHAR>(pSrc[nPos]) - (uiKey & 0xFF);
    ucShft = static_cast<HB_UCHAR>(uiKey & 0x07);
    pDst[nPos] = ((ucChar << ucShft) + (ucChar >> (8 - ucShft)));
    ulSeed = hb_sxNextSeed(ulSeed, &pKeyVal[i], &uiKey);
    if (++i == 7)
    {
      i = 0;
    }
  }
}

static bool _hb_sxGetKey(PHB_ITEM pKeyItem, char *pKeyVal)
{
  auto fResult = false;
  PHB_ITEM pItem = nullptr;

  if (!(hb_itemType(pKeyItem) & Harbour::Item::STRING))
  {
    auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

    if (pArea != nullptr)
    {
      pItem = hb_itemNew(nullptr);
      if (SELF_INFO(pArea, DBI_PASSWORD, pItem) == Harbour::SUCCESS)
      {
        pKeyItem = pItem;
      }
    }
  }
  if (hb_itemType(pKeyItem) & Harbour::Item::STRING)
  {
    auto nKey = pKeyItem->getCLen();
    if (nKey)
    {
      memcpy(pKeyVal, pKeyItem->getCPtr(), HB_MIN(nKey, 8));
    }
    if (nKey < 8)
    {
      memset(pKeyVal + nKey, 0, 8 - nKey);
    }
    fResult = true;
  }
  if (pItem != nullptr)
  {
    hb_itemRelease(pItem);
  }
  return fResult;
}

HB_FUNC(SX_ENCRYPT)
{
  if (hb_pcount() > 0)
  {
    char keyBuf[8];
    auto nLen = hb_parclen(1);

    if (nLen > 0 && _hb_sxGetKey(hb_param(2, Harbour::Item::ANY), keyBuf))
    {
      auto pDst = static_cast<char *>(hb_xgrab(nLen + 1));
      hb_sxEnCrypt(hb_parc(1), pDst, keyBuf, nLen);
      pDst[nLen] = 0;
      hb_retclen_buffer(pDst, nLen);
    }
    else
    {
      hb_itemReturn(hb_param(1, Harbour::Item::ANY));
    }
  }
}

HB_FUNC(SX_DECRYPT)
{
  if (hb_pcount() > 0)
  {
    char keyBuf[8];
    auto nLen = hb_parclen(1);

    if (nLen > 0 && _hb_sxGetKey(hb_param(2, Harbour::Item::ANY), keyBuf))
    {
      auto pDst = static_cast<char *>(hb_xgrab(nLen + 1));
      hb_sxDeCrypt(hb_parc(1), pDst, keyBuf, nLen);
      pDst[nLen] = 0;
      hb_retclen_buffer(pDst, nLen);
    }
    else
    {
      hb_itemReturn(hb_param(1, Harbour::Item::ANY));
    }
  }
}
