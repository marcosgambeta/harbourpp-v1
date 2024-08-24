/*
 * The Hash tables API (PRG level)
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

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

#include "hbvmint.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbstack.hpp"

HB_FUNC(HB_HASH)
{
  auto iPCount = hb_pcount();

  if (iPCount & 1)
  {
    hb_errRT_BASE(EG_BOUND, 1131, nullptr, hb_langDGetErrorDesc(EG_ARRDIMENSION), HB_ERR_ARGS_BASEPARAMS);
  }
  else
  {
    PHB_ITEM pHash = hb_hashNew(nullptr);
    for (auto iParam = 1; iParam <= iPCount; iParam += 2)
    {
      auto pKey = hb_param(iParam, Harbour::Item::HASHKEY);
      auto pValue = hb_param(iParam + 1, Harbour::Item::ANY);
      if (pKey)
      {
        hb_hashAdd(pHash, pKey, pValue);
      }
      else
      {
        hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 3, pHash,
                      hb_param(iParam, Harbour::Item::ANY), pValue);
        break;
      }
    }
    hb_itemReturnRelease(pHash);
  }
}

HB_FUNC(HB_HHASKEY)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    HB_SIZE nPos;
    hb_retl(hb_hashScanSoft(pHash, pKey, &nPos));
    hb_storns(nPos, 3);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HPOS)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    HB_SIZE nPos;
    hb_hashScan(pHash, pKey, &nPos);
    hb_retns(nPos);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HGET)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    auto pDest = hb_hashGetItemPtr(pHash, pKey, HB_HASH_AUTOADD_ACCESS);
    if (pDest)
    {
      hb_itemReturn(pDest);
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pHash, pKey);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HGETDEF)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    auto pDest = hb_hashGetItemPtr(pHash, pKey, HB_HASH_AUTOADD_ACCESS);
    if (pDest)
    {
      hb_itemReturn(pDest);
    }
    else
    {
      auto pDefault = hb_param(3, Harbour::Item::ANY);
      if (pDefault)
      {
        hb_itemReturn(pDefault);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HSETDEF)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    auto pDefault = hb_param(3, Harbour::Item::ANY);
    int iFlags = hb_hashGetFlags(pHash);

    if ((iFlags & HB_HASH_AUTOADD_ACCESS) == 0)
    {
      hb_hashSetFlags(pHash, HB_HASH_AUTOADD_ACCESS);
    }

    auto pDest = hb_hashGetItemPtr(pHash, pKey, HB_HASH_AUTOADD_ACCESS);

    if ((iFlags & HB_HASH_AUTOADD_ACCESS) == 0)
    {
      hb_hashClearFlags(pHash, HB_HASH_AUTOADD_ACCESS);
    }

    if (pDest)
    {
      if (pDefault && !hb_itemTypeCmp(pDest, pDefault))
      {
        hb_itemCopy(pDest, pDefault);
      }
      hb_itemReturn(pDest);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HGETREF)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    auto pDest = hb_hashGetItemPtr(pHash, pKey, HB_HASH_AUTOADD_ACCESS);
    hb_itemParamStore(3, pDest);
    hb_retl(pDest != nullptr);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(HB_HSET)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);
  auto pValue = hb_param(3, Harbour::Item::ANY);

  if (pHash && pKey && pValue)
  {
    hb_hashAdd(pHash, pKey, pValue);
    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HDEL)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pKey = hb_param(2, Harbour::Item::HASHKEY);

  if (pHash && pKey)
  {
    hb_hashDel(pHash, pKey);
    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HKEYAT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pPos = hb_param(2, Harbour::Item::NUMERIC);

  if (pHash && pPos)
  {
    auto pKey = hb_hashGetKeyAt(pHash, hb_itemGetNS(pPos));
    if (pKey)
    {
      hb_itemReturn(pKey);
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1187, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HVALUEAT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pPos = hb_param(2, Harbour::Item::NUMERIC);
  auto pValue = hb_param(3, Harbour::Item::ANY);

  if (pHash && pPos)
  {
    auto pItem = hb_hashGetValueAt(pHash, hb_itemGetNS(pPos));
    if (pItem != nullptr)
    {
      if (pValue)
      {
        hb_itemCopy(pItem, pValue);
      }
      else
      {
        pValue = pItem;
      }
      hb_itemReturn(pValue);
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1187, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HPAIRAT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pPos = hb_param(2, Harbour::Item::NUMERIC);

  if (pHash && pPos)
  {
    auto pKey = hb_hashGetKeyAt(pHash, hb_itemGetNS(pPos));
    auto pValue = hb_hashGetValueAt(pHash, hb_itemGetNS(pPos));
    if (pKey && pValue)
    {
      auto pDstKey = hb_param(3, Harbour::Item::BYREF);
      auto pDstVal = hb_param(4, Harbour::Item::BYREF);
      if (pDstKey && pDstVal)
      {
        hb_itemCopy(pDstKey, pKey);
        hb_itemCopy(pDstVal, pValue);
      }
      else
      {
        auto pResult = hb_itemArrayNew(2);
        hb_arraySet(pResult, 1, pKey);
        hb_arraySet(pResult, 2, pValue);
        hb_itemReturnRelease(pResult);
      }
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1187, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HDELAT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pPos = hb_param(2, Harbour::Item::NUMERIC);

  if (pHash && pPos)
  {
    if (hb_hashDelAt(pHash, hb_itemGetNS(pPos)))
    {
      hb_itemReturn(pHash);
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 2, pHash, pPos);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HKEYS)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_itemReturnRelease(hb_hashGetKeys(pHash));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HVALUES)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_itemReturnRelease(hb_hashGetValues(pHash));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HCLEAR)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_hashClear(pHash);
    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HFILL)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pValue = hb_param(2, Harbour::Item::ANY);

  if (pHash && pValue)
  {
    PHB_ITEM pDest;
    HB_SIZE nPos = 0;

    while ((pDest = hb_hashGetValueAt(pHash, ++nPos)) != nullptr)
    {
      hb_itemCopy(pDest, pValue);
    }

    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HCLONE)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_hashCloneTo(hb_stackReturnItem(), pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HCOPY)
{
  auto pSource = hb_param(1, Harbour::Item::HASH);
  auto pDest = hb_param(2, Harbour::Item::HASH);

  if (pSource && pDest)
  {
    if (pSource != pDest)
    {
      HB_SIZE nLen = hb_hashLen(pSource);

      HB_SIZE nStart = hb_parns(3);
      if (!nStart)
      {
        ++nStart;
      }
      HB_SIZE nCount = HB_ISNUM(4) ? static_cast<HB_SIZE>(hb_parns(4)) : nLen - nStart + 1;

      while (nCount--)
      {
        auto pKey = hb_hashGetKeyAt(pSource, nStart);
        auto pValue = hb_hashGetValueAt(pSource, nStart);
        if (pKey && pValue)
        {
          hb_hashAdd(pDest, pKey, pValue);
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    hb_itemReturn(pDest);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HMERGE)
{
  auto pDest = hb_param(1, Harbour::Item::HASH);
  auto pSource = hb_param(2, Harbour::Item::HASH);

  if (pDest && pSource)
  {
    if (pSource != pDest)
    {
      auto pAction = hb_param(3, Harbour::Item::EVALITEM | Harbour::Item::NUMERIC);

      if (pAction && HB_IS_EVALITEM(pAction))
      {
        HB_SIZE nLen = hb_hashLen(pSource), nPos = 0;
        while (++nPos <= nLen)
        {
          auto pKey = hb_hashGetKeyAt(pSource, nPos);
          auto pValue = hb_hashGetValueAt(pSource, nPos);
          if (pKey && pValue)
          {
            hb_vmPushEvalSym();
            hb_vmPush(pAction);
            hb_vmPush(pKey);
            hb_vmPush(pValue);
            hb_vmPushSize(nPos);
            hb_vmSend(3);
            {
              auto pReturn = hb_stackReturnItem();
              if (HB_IS_LOGICAL(pReturn) && hb_itemGetL(pReturn))
              {
                hb_hashAdd(pDest, pKey, pValue);
              }
            }
          }
          else
          {
            break;
          }
        }
      }
      else
      {
        hb_hashJoin(pDest, pSource, pAction ? hb_itemGetNI(pAction) : HB_HASH_UNION);
      }
    }
    hb_itemReturn(pDest);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HEVAL)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pBlock = hb_param(2, Harbour::Item::EVALITEM);

  if (pHash && pBlock)
  {
    HB_SIZE nLen = hb_hashLen(pHash);

    HB_SIZE nStart = hb_parns(3);
    if (!nStart)
    {
      ++nStart;
    }
    HB_SIZE nCount = HB_ISNUM(4) ? static_cast<HB_SIZE>(hb_parns(4)) : nLen - nStart + 1;

    while (nCount--)
    {
      auto pKey = hb_hashGetKeyAt(pHash, nStart);
      auto pValue = hb_hashGetValueAt(pHash, nStart);
      if (pKey && pValue)
      {
        hb_vmPushEvalSym();
        hb_vmPush(pBlock);
        hb_vmPush(pKey);
        hb_vmPush(pValue);
        hb_vmPushSize(nStart);
        hb_vmSend(3);
      }
      else
      {
        break;
      }
      ++nStart;
    }

    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HSCAN)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pValue = hb_param(2, Harbour::Item::ANY);

  if (pHash && pValue)
  {
    bool fExact = hb_parl(5);
    auto fFound = false;
    HB_SIZE nLen = hb_hashLen(pHash);

    HB_SIZE nStart = hb_parns(3);
    if (!nStart)
    {
      ++nStart;
    }
    HB_SIZE nCount = HB_ISNUM(4) ? static_cast<HB_SIZE>(hb_parns(4)) : nLen - nStart + 1;

    if (HB_IS_EVALITEM(pValue))
    {
      while (nCount--)
      {
        auto pKey = hb_hashGetKeyAt(pHash, nStart);
        auto pVal = hb_hashGetValueAt(pHash, nStart);
        if (pKey && pValue)
        {
          hb_vmPushEvalSym();
          hb_vmPush(pValue);
          hb_vmPush(pKey);
          hb_vmPush(pVal);
          hb_vmPushSize(nStart);
          hb_vmSend(3);
          {
            auto pReturn = hb_stackReturnItem();
            if (HB_IS_LOGICAL(pReturn) && hb_itemGetL(pReturn))
            {
              fFound = true;
              break;
            }
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_STRING(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_STRING(pItem) && hb_itemStrCmp(pItem, pValue, fExact) == 0)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_NUMINT(pValue))
    {
      HB_MAXINT nValue = hb_itemGetNInt(pValue);
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_NUMERIC(pItem) && hb_itemGetNInt(pItem) == nValue &&
              hb_itemGetND(pItem) == static_cast<double>(nValue))
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_NUMERIC(pValue))
    {
      double dValue = hb_itemGetND(pValue);
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_NUMERIC(pItem) && hb_itemGetND(pItem) == dValue)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_DATETIME(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_DATETIME(pItem) && pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
              (!fExact || pItem->item.asDateTime.time == pValue->item.asDateTime.time))
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_LOGICAL(pValue))
    {
      HB_BOOL fValue = hb_itemGetDL(pValue);
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_LOGICAL(pItem) && hb_itemGetL(pItem) == fValue)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_NIL(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_NIL(pItem))
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (HB_IS_POINTER(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_POINTER(pItem) && pItem->item.asPointer.value == pValue->item.asPointer.value)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (fExact && HB_IS_ARRAY(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_ARRAY(pItem) && pItem->item.asArray.value == pValue->item.asArray.value)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }
    else if (fExact && HB_IS_HASH(pValue))
    {
      while (nCount--)
      {
        auto pItem = hb_hashGetValueAt(pHash, nStart);
        if (pItem != nullptr)
        {
          if (HB_IS_HASH(pItem) && pItem->item.asHash.value == pValue->item.asHash.value)
          {
            fFound = true;
            break;
          }
        }
        else
        {
          break;
        }
        ++nStart;
      }
    }

    hb_retns(fFound ? nStart : 0);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1123, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HSORT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_hashSort(pHash);
    hb_itemReturn(pHash);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HCASEMATCH)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    auto pValue = hb_param(2, Harbour::Item::LOGICAL);
    int iFlags = hb_hashGetFlags(pHash);

    hb_retl((iFlags & HB_HASH_IGNORECASE) == 0);

    if (pValue)
    {
      if (hb_itemGetL(pValue))
      {
        if ((iFlags & HB_HASH_IGNORECASE) != 0)
        {
          hb_hashClearFlags(pHash, HB_HASH_IGNORECASE);
          hb_hashSetFlags(pHash, HB_HASH_RESORT);
        }
      }
      else if ((iFlags & HB_HASH_IGNORECASE) == 0)
      {
        hb_hashClearFlags(pHash, HB_HASH_BINARY);
        hb_hashSetFlags(pHash, HB_HASH_IGNORECASE | HB_HASH_RESORT);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HBINARY)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    auto pValue = hb_param(2, Harbour::Item::LOGICAL);
    int iFlags = hb_hashGetFlags(pHash);

    hb_retl((iFlags & HB_HASH_BINARY) != 0);

    if (pValue)
    {
      if (hb_itemGetL(pValue))
      {
        if ((iFlags & HB_HASH_BINARY) == 0)
        {
          hb_hashClearFlags(pHash, HB_HASH_IGNORECASE);
          hb_hashSetFlags(pHash, HB_HASH_BINARY | HB_HASH_RESORT);
        }
      }
      else if ((iFlags & HB_HASH_BINARY) != 0)
      {
        hb_hashClearFlags(pHash, HB_HASH_BINARY);
        hb_hashSetFlags(pHash, HB_HASH_RESORT);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HAUTOADD)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    auto pValue = hb_param(2, Harbour::Item::LOGICAL | Harbour::Item::NUMERIC);
    int iOldFlags = hb_hashGetFlags(pHash) & HB_HASH_AUTOADD_MASK;

    hb_retni(iOldFlags);

    if (hb_pcount() >= 3)
    {
      hb_hashSetDefault(pHash, hb_param(3, Harbour::Item::ANY));
    }

    if (pValue)
    {
      if (HB_IS_LOGICAL(pValue))
      {
        if (hb_itemGetL(pValue))
        {
          hb_hashSetFlags(pHash, hb_hashGetDefault(pHash) ? HB_HASH_AUTOADD_ALWAYS : HB_HASH_AUTOADD_ASSIGN);
        }
        else if (iOldFlags)
        {
          hb_hashClearFlags(pHash, iOldFlags);
        }
      }
      else
      {
        int iNewFlags = hb_itemGetNI(pValue);
        if ((iNewFlags | iOldFlags) != iNewFlags)
        {
          hb_hashClearFlags(pHash, iOldFlags);
        }
        if (iNewFlags)
        {
          hb_hashSetFlags(pHash, iNewFlags);
        }
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HKEEPORDER)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    auto pValue = hb_param(2, Harbour::Item::LOGICAL);
    int iFlags = hb_hashGetFlags(pHash);

    hb_retl((iFlags & HB_HASH_KEEPORDER) != 0);

    if (pValue)
    {
      if (hb_itemGetL(pValue))
      {
        if ((iFlags & HB_HASH_KEEPORDER) == 0)
        {
          hb_hashSetFlags(pHash, HB_HASH_KEEPORDER);
        }
      }
      else
      {
        if ((iFlags & HB_HASH_KEEPORDER) != 0)
        {
          hb_hashClearFlags(pHash, HB_HASH_KEEPORDER);
        }
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HALLOCATE)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);
  auto pValue = hb_param(2, Harbour::Item::NUMERIC);

  if (pHash && pValue)
  {
    HB_ISIZ nMem = hb_itemGetNS(pValue);
    if (nMem >= 0)
    {
      hb_hashPreallocate(pHash, nMem);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_HDEFAULT)
{
  auto pHash = hb_param(1, Harbour::Item::HASH);

  if (pHash)
  {
    hb_itemReturn(hb_hashGetDefault(pHash));
    if (hb_pcount() > 1)
    {
      hb_hashSetDefault(pHash, hb_param(2, Harbour::Item::ANY));
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2017, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#if defined(HB_LEGACY_LEVEL5)
HB_FUNC(HB_HSETAUTOADD)
{
  HB_FUNC_EXEC(HB_HAUTOADD);
  hb_itemReturn(hb_param(1, Harbour::Item::HASH));
}
HB_FUNC(HB_HSETCASEMATCH)
{
  HB_FUNC_EXEC(HB_HCASEMATCH);
  hb_itemReturn(hb_param(1, Harbour::Item::HASH));
}
HB_FUNC(HB_HSETBINARY)
{
  HB_FUNC_EXEC(HB_HBINARY);
  hb_itemReturn(hb_param(1, Harbour::Item::HASH));
}
HB_FUNC(HB_HSETORDER)
{
  HB_FUNC_EXEC(HB_HKEEPORDER);
  hb_itemReturn(hb_param(1, Harbour::Item::HASH));
}
#endif
