//
// The Eval API
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) (hb_itemDo()/hb_itemDoC() (based on HB_DO() by Ryszard
// Glab))
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

#include "hbvmint.hpp"
#include "hbapi.hpp"
#include "hbstack.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbvm.hpp"

HB_BOOL hb_evalNew(PHB_EVALINFO pEvalInfo, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_evalNew(%p, %p)", static_cast<void*>(pEvalInfo), static_cast<void*>(pItem)));
#endif

  if (pEvalInfo) {
    memset(pEvalInfo, 0, sizeof(HB_EVALINFO));
    pEvalInfo->pItems[0] = pItem;
    pEvalInfo->paramCount = 0;

    return true;
  } else {
    return false;
  }
}

// NOTE: CA-Cl*pper is buggy and will not check if more parameters are
//       added than the maximum (9). [vszakats]

// NOTE: CA-Cl*pper NG suggests that the Items passed as parameters should/may
//       be released by the programmer explicitly. But in fact hb_evalRelease()
//       will automatically release them all. The sample programs in the
//       NG are doing it that way. Releasing the parameters explicitly in
//       Harbour will cause an internal error, while it will be silently
//       ignored (?) in CA-Cl*pper. This is due to the different internal
//       handling of the Items, but IIRC it causes leak in CA-Cl*pper. All in
//       all, don't release the eval parameter Items explicitly to make both
//       Harbour and CA-Cl*pper happy. [vszakats]

HB_BOOL hb_evalPutParam(PHB_EVALINFO pEvalInfo, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_evalPutParam(%p, %p)", static_cast<void*>(pEvalInfo), static_cast<void*>(pItem)));
#endif

  if (pEvalInfo && pItem && pEvalInfo->paramCount < HB_EVAL_PARAM_MAX_) {
    pEvalInfo->pItems[++pEvalInfo->paramCount] = pItem;
    return true;
  } else {
    return false;
  }
}

PHB_ITEM hb_evalLaunch(PHB_EVALINFO pEvalInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_evalLaunch(%p)", static_cast<void*>(pEvalInfo)));
#endif

  PHB_ITEM pResult = nullptr;

  if (pEvalInfo) {
    PHB_ITEM pItem = pEvalInfo->pItems[0];
    PHB_SYMB pSymbol = nullptr;

    if (pItem->isString()) {
      auto pDynSym = hb_dynsymFindName(pItem->stringValue());

      if (pDynSym) {
        pSymbol = pDynSym->pSymbol;
        pItem = nullptr;
      }
    } else if (pItem->isSymbol()) {
      pSymbol = pItem->symbolValue();
      pItem = nullptr;
    } else if (pItem->isBlock()) {
      pSymbol = &hb_symEval;
    }

    if (pSymbol) {
      HB_USHORT uiParam = 0;

      hb_vmPushSymbol(pSymbol);
      if (pItem != nullptr) {
        hb_vmPush(pItem);
      } else {
        hb_vmPushNil();
      }
      while (uiParam < pEvalInfo->paramCount) {
        hb_vmPush(pEvalInfo->pItems[++uiParam]);
      }
      if (pItem != nullptr) {
        hb_vmSend(uiParam);
      } else {
        hb_vmProc(uiParam);
      }
      pResult = hb_itemNew(hb_stackReturnItem());
    }
  }

  return pResult;
}

// NOTE: CA-Cl*pper NG states that hb_evalLaunch() must be called at least
//       once and only once before calling hb_evalRelease(). Harbour doesn't
//       have these requirements. [vszakats]

HB_BOOL hb_evalRelease(PHB_EVALINFO pEvalInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_evalRelease(%p)", static_cast<void*>(pEvalInfo)));
#endif

  if (pEvalInfo) {
    for (HB_USHORT uiParam = 0; uiParam <= pEvalInfo->paramCount; uiParam++) {
      hb_itemRelease(pEvalInfo->pItems[uiParam]);
      pEvalInfo->pItems[uiParam] = nullptr;
    }
    pEvalInfo->paramCount = 0;
    return true;
  } else {
    return false;
  }
}

// NOTE: Same purpose as hb_evalLaunch(), but simpler, faster and more flexible.
//       It can be used to call symbols, functions names, or blocks, the items
//       don't need to be duplicated when passed as argument, one line is
//       enough to initiate a call, the number of parameters is not limited.
//       [vszakats]

// NOTE: When calling hb_itemDo() with no arguments for the Harbour item being
//       evaluated, you must use '(PHB_ITEM *) 0' as the third parameter.

PHB_ITEM hb_itemDo(PHB_ITEM pItem, HB_ULONG ulPCount, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemDo(%p, %lu, ...)", static_cast<void*>(pItem), ulPCount));
#endif

  PHB_ITEM pResult = nullptr;

  if (pItem != nullptr) {
    PHB_SYMB pSymbol = nullptr;

    if (pItem->isString()) {
      auto pDynSym = hb_dynsymFindName(pItem->stringValue());

      if (pDynSym) {
        pSymbol = pDynSym->pSymbol;
        pItem = nullptr;
      }
    } else if (pItem->isSymbol()) {
      pSymbol = pItem->symbolValue();
      pItem = nullptr;
    } else if (pItem->isBlock()) {
      pSymbol = &hb_symEval;
    }

    if (pSymbol) {
      if (hb_vmRequestReenter()) {
        hb_vmPushSymbol(pSymbol);
        if (pItem != nullptr) {
          hb_vmPush(pItem);
        } else {
          hb_vmPushNil();
        }

        if (ulPCount) {
          va_list va;
          va_start(va, ulPCount);
          for (HB_ULONG ulParam = 1; ulParam <= ulPCount; ulParam++) {
            hb_vmPush(va_arg(va, PHB_ITEM));
          }
          va_end(va);
        }
        if (pItem != nullptr) {
          hb_vmSend(static_cast<HB_USHORT>(ulPCount));
        } else {
          hb_vmProc(static_cast<HB_USHORT>(ulPCount));
        }

        pResult = hb_itemNew(hb_stackReturnItem());
        hb_vmRequestRestore();
      }
    }
  }

  return pResult;
}

// NOTE: Same as hb_itemDo(), but even simpler, since the function name can be
//       directly passed as a zero terminated string. [vszakats]

// NOTE: When calling hb_itemDoC() with no arguments for the Harbour function
//       being called, you must use '(PHB_ITEM *) 0' as the third parameter.

PHB_ITEM hb_itemDoC(const char *szFunc, HB_ULONG ulPCount, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemDoC(%s, %lu, ...)", szFunc, ulPCount));
#endif

  PHB_ITEM pResult = nullptr;

  if (szFunc != nullptr) {
    auto pDynSym = hb_dynsymFindName(szFunc);

    if (pDynSym) {
      if (hb_vmRequestReenter()) {
        hb_vmPushSymbol(pDynSym->pSymbol);
        hb_vmPushNil();
        if (ulPCount) {
          va_list va;
          va_start(va, ulPCount);
          for (HB_ULONG ulParam = 1; ulParam <= ulPCount; ulParam++) {
            hb_vmPush(va_arg(va, PHB_ITEM));
          }
          va_end(va);
        }
        hb_vmProc(static_cast<HB_USHORT>(ulPCount));
        pResult = hb_itemNew(hb_stackReturnItem());
        hb_vmRequestRestore();
      }
    }
  }

  return pResult;
}

// Notice that these two functions place the result at hb_stackReturnItem(),
// that you may access its value using a hb_par...(-1).

// undocumented Clipper _cEval0()
void hb_evalBlock0(PHB_ITEM pCodeBlock)
{
  hb_vmPushEvalSym();
  hb_vmPush(pCodeBlock);
  hb_vmSend(0);
}

// undocumented Clipper _cEval1()
void hb_evalBlock1(PHB_ITEM pCodeBlock, PHB_ITEM pParam)
{
  hb_vmPushEvalSym();
  hb_vmPush(pCodeBlock);
  hb_vmPush(pParam);
  hb_vmSend(1);
}

// same functionality but with a nullptr terminated list of parameters
void hb_evalBlock(PHB_ITEM pCodeBlock, ...)
{
  va_list args;
  HB_USHORT uiParams = 0;
  PHB_ITEM pParam;

  hb_vmPushEvalSym();
  hb_vmPush(pCodeBlock);

  va_start(args, pCodeBlock);
  while ((pParam = va_arg(args, PHB_ITEM)) != nullptr) {
    hb_vmPush(pParam);
    uiParams++;
  }
  va_end(args);

  hb_vmSend(uiParams);
}

HB_FUNC(HB_FORNEXT) // nStart, nEnd | bEnd, bCode, nStep
{
  auto pCodeBlock = hb_param(3, Harbour::Item::BLOCK);

  if (pCodeBlock) {
    HB_MAXINT nStart = hb_parnint(1), nEnd;
    HB_MAXINT nStep = (hb_pcount() > 3) ? hb_parnint(4) : 1;

    auto pEndBlock = hb_param(2, Harbour::Item::BLOCK);

    if (pEndBlock) {
      hb_evalBlock0(pEndBlock);
      nEnd = hb_parnint(-1);

      while (nStart <= nEnd) {
        hb_vmPushEvalSym();
        hb_vmPush(pCodeBlock);
        hb_vmPushNumInt(nStart);
        hb_vmSend(1);

        nStart += nStep;

        hb_evalBlock0(pEndBlock);
        nEnd = hb_parnint(-1);
      }
    } else {
      nEnd = hb_parnint(2);
      while (nStart <= nEnd) {
        hb_vmPushEvalSym();
        hb_vmPush(pCodeBlock);
        hb_vmPushNumInt(nStart);
        hb_vmSend(1);

        nStart += nStep;
      }
    }
  }
}

// based on xHarbour's hb_ExecFromArray() by Giancarlo Niccolai
// This version supports the same syntax though it's independent
// implementation [druzus]
//
// The following syntax is supported:
//    hb_ExecFromArray(<cFuncName> [, <aParams> ])
//    hb_ExecFromArray(@<funcName>() [, <aParams> ])
//    hb_ExecFromArray(<bCodeBlock> [, <aParams> ])
//    hb_ExecFromArray(<oObject>, <cMethodName> [, <aParams> ])
//    hb_ExecFromArray(<oObject>, @<msgName>() [, <aParams> ])
// or:
//    hb_ExecFromArray(<aExecArray>)
// where <aExecArray> is in one of the following format:
//    { <cFuncName> [, <params,...>] }
//    { @<funcName>() [, <params,...>] }
//    { <bCodeBlock> [, <params,...>] }
//    { <oObject>, <cMethodName> [, <params,...>] }
//    { <oObject>, @<msgName>() [, <params,...>] }

HB_FUNC(HB_EXECFROMARRAY)
{
  PHB_SYMB pExecSym = nullptr;
  PHB_ITEM pFunc = nullptr;
  PHB_ITEM pSelf = nullptr;
  PHB_ITEM pArray = nullptr;
  PHB_ITEM pItem;
  HB_ULONG ulParamOffset = 0;
  auto iPCount = hb_pcount();

  // decode parameters
  if (iPCount) {
    auto pParam = hb_param(1, Harbour::Item::ANY);

    if (iPCount == 1) {
      if (pParam->isArray() && !pParam->isObject()) {
        pArray = pParam;
        pItem = hb_arrayGetItemPtr(pArray, 1);
        if (pItem->isObject()) {
          pSelf = pItem;
          pFunc = hb_arrayGetItemPtr(pArray, 2);
          ulParamOffset = 2;
        } else {
          pFunc = pItem;
          ulParamOffset = 1;
        }
      } else {
        pFunc = pParam;
      }
    } else if (pParam->isObject() && iPCount <= 3) {
      pSelf = pParam;
      pFunc = hb_param(2, Harbour::Item::ANY);
      pArray = hb_param(3, Harbour::Item::ANY);
    } else if (iPCount == 2) {
      pFunc = pParam;
      pArray = hb_param(2, Harbour::Item::ANY);
    }
  }

  if (pFunc && (!pArray || pArray->isArray())) {
    if (pFunc->isSymbol()) {
      pExecSym = pFunc->getSymbol();
    } else if (pFunc->isString()) {
      pExecSym = hb_dynsymGet(pFunc->getCPtr())->pSymbol;
    } else if (pFunc->isBlock() && !pSelf) {
      pSelf = pFunc;
      pExecSym = &hb_symEval;
    }
  }

  if (pExecSym) {
    pFunc = hb_stackBaseItem();
    pItem = hb_stackItem(pFunc->symbolStackState()->nBaseItem);
    pFunc->symbolStackState()->uiClass = pItem->symbolStackState()->uiClass;
    pFunc->symbolStackState()->uiMethod = pItem->symbolStackState()->uiMethod;

    iPCount = 0;
    hb_vmPushSymbol(pExecSym);
    if (pSelf) {
      hb_vmPush(pSelf);
    } else {
      hb_vmPushNil();
    }

    if (pArray) {
      pItem = hb_arrayGetItemPtr(pArray, ++ulParamOffset);
      while (pItem && iPCount < 255) {
        hb_vmPush(pItem);
        ++iPCount;
        pItem = hb_arrayGetItemPtr(pArray, ++ulParamOffset);
      }
    }

    if (pSelf) {
      hb_vmSend(static_cast<HB_USHORT>(iPCount));
    } else {
      hb_vmProc(static_cast<HB_USHORT>(iPCount));
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_BOOL hb_execFromArray(PHB_ITEM pParam)
{
  PHB_ITEM pArray = nullptr;
  PHB_ITEM pSelf = nullptr;
  HB_ULONG ulParamOffset = 0;

  if (pParam && pParam->isArray() && !pParam->isObject()) {
    pArray = pParam;
    pParam = hb_arrayGetItemPtr(pArray, 1);
    if (pParam->isObject()) {
      pSelf = pParam;
      pParam = hb_arrayGetItemPtr(pArray, 2);
      ulParamOffset = 2;
    } else {
      ulParamOffset = 1;
    }
  }

  if (pParam) {
    PHB_SYMB pExecSym = nullptr;

    if (pParam->isSymbol()) {
      pExecSym = pParam->getSymbol();
    } else if (pParam->isString()) {
      pExecSym = hb_dynsymGet(pParam->getCPtr())->pSymbol;
    } else if (pParam->isBlock() && !pSelf) {
      pSelf = pParam;
      pExecSym = &hb_symEval;
    }

    if (pExecSym) {
      int iPCount = 0;

      hb_vmPushSymbol(pExecSym);
      if (pSelf) {
        hb_vmPush(pSelf);
      } else {
        hb_vmPushNil();
      }

      if (pArray) {
        pParam = hb_arrayGetItemPtr(pArray, ++ulParamOffset);
        while (pParam && iPCount < 255) {
          hb_vmPush(pParam);
          ++iPCount;
          pParam = hb_arrayGetItemPtr(pArray, ++ulParamOffset);
        }
      }

      if (pSelf) {
        hb_vmSend(static_cast<HB_USHORT>(iPCount));
      } else {
        hb_vmProc(static_cast<HB_USHORT>(iPCount));
      }

      return true;
    }
  }

  hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);

  return false;
}

// hb_ExecMsg(<sFuncSym>, <object>, [<params,...>]) --> <xResult>
// Execute <sFuncSym> with <object> set as QSELF() value
HB_FUNC(HB_EXECMSG)
{
  auto iParams = hb_pcount();

  if (iParams >= 2 && HB_ISSYMBOL(1)) {
    auto pBase = hb_stackBaseItem();
    pBase->setSymbolParamCnt(0);
    pBase->setSymbolParamDeclCnt(0);
    hb_vmProc(static_cast<HB_USHORT>(iParams - 2));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
