//
// SetKey() and related functions
//
// Copyright 1999 April White <bright.tigra gmail.com>
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

// Either way you have to clean up the memory on exit. The best way to
// do this is to add a hb_setkeyInit() and hb_setkeyExit() function
// and call them from console.c Init/Exit functions.

#include "hbapi.hpp"
#include "hbvm.hpp"
#include "hbapiitm.hpp"
#include "hbapigt.hpp"
#include "hbstack.hpp"

struct HB_SETKEY_
{
  int iKeyCode;
  PHB_ITEM pAction;
  PHB_ITEM pIsActive;
  struct HB_SETKEY_ *next;
};

using HB_SETKEY = HB_SETKEY_;
using PHB_SETKEY = HB_SETKEY_ *;

struct HB_SK_DATA
{
  PHB_SETKEY sk_list;
};

using PHB_SK_DATA = HB_SK_DATA *;

static void hb_setkeyRelease(void *cargo)
{
  PHB_SETKEY sk_list = (static_cast<PHB_SK_DATA>(cargo))->sk_list;

  while (sk_list)
  {
    PHB_SETKEY sk_list_tmp;
    hb_itemRelease(sk_list->pAction);
    if (sk_list->pIsActive)
    {
      hb_itemRelease(sk_list->pIsActive);
    }
    sk_list_tmp = sk_list;
    sk_list = sk_list->next;
    hb_xfree(sk_list_tmp);
  }

  (static_cast<PHB_SK_DATA>(cargo))->sk_list = nullptr;
}

static HB_TSD_NEW(s_skData, sizeof(HB_SK_DATA), nullptr, hb_setkeyRelease);

static bool sk_testActive(PHB_ITEM pIsActive, int iKeyCode)
{
  if (pIsActive)
  {
    hb_vmPushEvalSym();
    hb_vmPush(pIsActive);
    hb_vmPushInteger(iKeyCode);
    hb_vmSend(1);
    return hb_parldef(-1, true);
  }
  return true;
}

static PHB_SETKEY sk_findkey(int iKeyCode, PHB_SETKEY sk_list, PHB_SETKEY *sk_list_end)
{
  PHB_SETKEY sk_list_tmp;

  *sk_list_end = nullptr;
  for (sk_list_tmp = sk_list; sk_list_tmp && sk_list_tmp->iKeyCode != iKeyCode; sk_list_tmp = sk_list_tmp->next)
  {
    *sk_list_end = sk_list_tmp;
  }

  return sk_list_tmp;
}

static void sk_add(PHB_SETKEY *sk_list_ptr, HB_BOOL bReturn, int iKeyCode, PHB_ITEM pAction, PHB_ITEM pIsActive)
{
  if (iKeyCode)
  {
    PHB_SETKEY sk_list_tmp, sk_list_end;

    if (pIsActive && !pIsActive->isEvalItem())
    {
      pIsActive = nullptr;
    }
    if (pAction && !pAction->isEvalItem())
    {
      pAction = nullptr;
    }

    sk_list_tmp = sk_findkey(iKeyCode, *sk_list_ptr, &sk_list_end);
    if (sk_list_tmp == nullptr)
    {
      if (pAction)
      {
        sk_list_tmp = static_cast<PHB_SETKEY>(hb_xgrab(sizeof(HB_SETKEY)));
        sk_list_tmp->next = nullptr;
        sk_list_tmp->iKeyCode = iKeyCode;
        sk_list_tmp->pAction = hb_itemNew(pAction);
        sk_list_tmp->pIsActive = pIsActive ? hb_itemNew(pIsActive) : nullptr;

        if (sk_list_end == nullptr)
        {
          *sk_list_ptr = sk_list_tmp;
        }
        else
        {
          sk_list_end->next = sk_list_tmp;
        }
      }
    }
    else
    {
      // Return the previous value

      if (bReturn)
      {
        hb_itemReturn(sk_list_tmp->pAction);
      }

      // Free the previous values

      hb_itemRelease(sk_list_tmp->pAction);
      if (sk_list_tmp->pIsActive)
      {
        hb_itemRelease(sk_list_tmp->pIsActive);
      }

      // Set the new values or free the entry

      if (pAction)
      {
        sk_list_tmp->pAction = hb_itemNew(pAction);
        sk_list_tmp->pIsActive = pIsActive ? hb_itemNew(pIsActive) : nullptr;
      }
      else
      {
        // if this is true, then the key found is the first key in the list
        if (sk_list_end == nullptr)
        {
          sk_list_tmp = *sk_list_ptr;
          *sk_list_ptr = sk_list_tmp->next;
          hb_xfree(sk_list_tmp);
        }
        else
        {
          sk_list_end->next = sk_list_tmp->next;
          hb_xfree(sk_list_tmp);
        }
      }
    }
  }
}

HB_FUNC(SETKEY)
{
  auto iKeyCode = hb_parni(1);

  if (iKeyCode != 0)
  {
    auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));

    if (hb_pcount() == 1)
    {
      // Get a SETKEY value
      PHB_SETKEY sk_list_tmp, sk_list_end;

      // sk_list_end is not used in this context
      sk_list_tmp = sk_findkey(iKeyCode, sk_data->sk_list, &sk_list_end);

      if (sk_list_tmp)
      {
        hb_itemReturn(sk_list_tmp->pAction);
      }
    }
    else
    {
      // Set a SETKEY value
      sk_add(&sk_data->sk_list, true, iKeyCode, hb_param(2, Harbour::Item::EVALITEM), nullptr);
    }
  }
}

HB_FUNC(HB_SETKEY)
{
  auto iKeyCode = hb_parni(1);

  if (iKeyCode != 0)
  {
    auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));

    if (hb_pcount() == 1)
    {
      // Get a SETKEY value
      PHB_SETKEY sk_list_tmp, sk_list_end;

      // sk_list_end is not used in this context
      sk_list_tmp = sk_findkey(iKeyCode, sk_data->sk_list, &sk_list_end);
      if (sk_list_tmp == nullptr)
      {
        int iKeyStd = hb_inkeyKeyStd(iKeyCode);

        if (iKeyStd != iKeyCode)
        {
          sk_list_tmp = sk_findkey(iKeyStd, sk_data->sk_list, &sk_list_end);
          iKeyCode = iKeyStd;
        }
      }

      if (sk_list_tmp)
      {
        if (sk_testActive(sk_list_tmp->pIsActive, iKeyCode))
        {
          hb_itemReturn(sk_list_tmp->pAction);
        }
      }
    }
    else
    {
      // Set a SETKEY value
      sk_add(&sk_data->sk_list, true, iKeyCode, hb_param(2, Harbour::Item::EVALITEM),
             hb_param(3, Harbour::Item::EVALITEM));
    }
  }
}

// Sets the same block for an array of keycodes

HB_FUNC(HB_SETKEYARRAY)
{
  auto pKeyCodeArray = hb_param(1, Harbour::Item::ARRAY);
  auto pAction = hb_param(2, Harbour::Item::EVALITEM);

  if (pKeyCodeArray && pAction)
  {
    auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));
    auto pIsActive = hb_param(3, Harbour::Item::EVALITEM);
    HB_SIZE nLen = hb_arrayLen(pKeyCodeArray);
    HB_SIZE nPos;

    for (nPos = 1; nPos <= nLen; nPos++)
    {
      sk_add(&sk_data->sk_list, false, hb_arrayGetNI(pKeyCodeArray, nPos), pAction, pIsActive);
    }
  }
}

HB_FUNC(HB_SETKEYGET)
{
  auto pKeyCode = hb_param(1, Harbour::Item::NUMERIC);

  if (pKeyCode)
  {
    auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));
    PHB_SETKEY sk_list_tmp, sk_list_end;

    // sk_list_end is not used in this context
    sk_list_tmp = sk_findkey(pKeyCode->getNI(), sk_data->sk_list, &sk_list_end);

    if (sk_list_tmp)
    {
      hb_itemReturn(sk_list_tmp->pAction);

      if (sk_list_tmp->pIsActive)
      {
        hb_itemParamStore(2, sk_list_tmp->pIsActive);
      }
    }
  }
}

HB_FUNC(HB_SETKEYSAVE)
{
  auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));
  PHB_SETKEY sk_list_tmp;
  HB_SIZE nItemCount, nItem;

  // build an multi-dimensional array from existing hot-keys, and return it

  // count the number of items in the list
  for (nItemCount = 0, sk_list_tmp = sk_data->sk_list; sk_list_tmp; nItemCount++, sk_list_tmp = sk_list_tmp->next)
  {
    ;
  }

  auto pKeys = hb_itemArrayNew(nItemCount);
  auto pKeyElements = hb_itemNew(nullptr);

  for (nItem = 1, sk_list_tmp = sk_data->sk_list; nItem <= nItemCount; nItem++, sk_list_tmp = sk_list_tmp->next)
  {
    hb_arrayNew(pKeyElements, 3);
    hb_arraySetNI(pKeyElements, 1, sk_list_tmp->iKeyCode);
    hb_arraySet(pKeyElements, 2, sk_list_tmp->pAction);
    if (sk_list_tmp->pIsActive)
    {
      hb_arraySet(pKeyElements, 3, sk_list_tmp->pIsActive);
    }
    hb_arraySetForward(pKeys, nItem, pKeyElements);
  }
  hb_itemRelease(pKeyElements);
  hb_itemReturnRelease(pKeys);

  auto pParam = hb_param(1, Harbour::Item::ANY);
  if (pParam)
  {
    hb_setkeyRelease(sk_data); // destroy the internal list

    if (pParam->isArray())
    {
      nItemCount = hb_arrayLen(pParam);

      for (nItem = 1; nItem <= nItemCount; nItem++)
      {
        auto itmKeyElements = hb_arrayGetItemPtr(pParam, nItem);

        sk_add(&sk_data->sk_list, false, hb_arrayGetNI(itmKeyElements, 1), hb_arrayGetItemPtr(itmKeyElements, 2),
               hb_arrayGetItemPtr(itmKeyElements, 3));
      }
    }
  }
}

HB_FUNC(HB_SETKEYCHECK)
{
  HB_BOOL bIsKeySet = false;
  auto iKeyCode = hb_parni(1);

  if (iKeyCode != 0)
  {
    auto sk_data = static_cast<PHB_SK_DATA>(hb_stackGetTSD(&s_skData));
    PHB_SETKEY sk_list_tmp, sk_list_end;

    // sk_list_end is not used in this context
    sk_list_tmp = sk_findkey(iKeyCode, sk_data->sk_list, &sk_list_end);
    if (sk_list_tmp == nullptr)
    {
      int iKeyStd = hb_inkeyKeyStd(iKeyCode);

      if (iKeyStd != iKeyCode)
      {
        sk_list_tmp = sk_findkey(iKeyStd, sk_data->sk_list, &sk_list_end);
        iKeyCode = iKeyStd;
      }
    }

    if (sk_list_tmp)
    {
      if (sk_testActive(sk_list_tmp->pIsActive, iKeyCode))
      {
        auto uiPCount = static_cast<HB_USHORT>(hb_pcount());
        HB_USHORT uiParam;

        hb_vmPushEvalSym();
        hb_vmPush(sk_list_tmp->pAction);
        for (uiParam = 2; uiParam <= uiPCount; ++uiParam)
        {
          hb_vmPush(hb_stackItemFromBase(uiParam));
        }
        hb_vmPushInteger(iKeyCode);
        hb_vmSend(uiPCount);

        bIsKeySet = true;
      }
    }
  }

  hb_retl(bIsKeySet);
}
