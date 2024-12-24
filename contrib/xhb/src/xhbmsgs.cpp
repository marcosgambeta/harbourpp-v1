//
// xHarbour compatible messages used in overloaded scalar classes
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

#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapilng.hpp>
#include <hbstack.hpp>
#include <hbmath.hpp>

HB_FUNC(XHB_HASHERROR)
{
  const char *szMessage = hb_itemGetSymbol(hb_stackBaseItem())->szName;
  auto iPCount = hb_pcount();

  if (iPCount == 1)
  {
    if (szMessage[0] == '_')
    { /* ASSIGN */
      auto pIndex = hb_itemPutCConst(hb_stackAllocItem(), szMessage + 1);
      PHB_ITEM pDest = hb_hashGetItemPtr(hb_stackSelfItem(), pIndex, HB_HASH_AUTOADD_ASSIGN);
      hb_stackPop();
      if (pDest)
      {
        auto pValue = hb_param(1, Harbour::Item::ANY);
        hb_itemCopyFromRef(pDest, pValue);
        hb_itemReturn(pValue);
        return;
      }
    }
  }
  else if (iPCount == 0)
  { /* ACCESS */
    auto pIndex = hb_itemPutCConst(hb_stackAllocItem(), szMessage);
    PHB_ITEM pValue = hb_hashGetItemPtr(hb_stackSelfItem(), pIndex, HB_HASH_AUTOADD_ACCESS);
    hb_stackPop();
    if (pValue)
    {
      hb_itemReturn(pValue);
      return;
    }
  }

  if (szMessage[0] == '_')
  {
    hb_errRT_BASE_SubstR(EG_NOVARMETHOD, 1005, nullptr, szMessage + 1, HB_ERR_ARGS_SELFPARAMS);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_NOMETHOD, 1004, nullptr, szMessage, HB_ERR_ARGS_SELFPARAMS);
  }
}

HB_FUNC(XHB_INCLUDE)
{
  auto pSelf = hb_stackSelfItem();
  auto pKey = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isArray())
  {
    hb_retl(hb_arrayScan(pSelf, pKey, nullptr, nullptr, true) != 0);
  }
  else if (pSelf->isHash() && (pKey->isHashKey() || hb_hashLen(pKey) == 1))
  {
    hb_retl(hb_hashScan(pSelf, pKey, nullptr));
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1109, nullptr, "$", 2, pKey, pSelf);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_EEQUAL)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue == static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) == dValue);
  }
  else if (pSelf->isBlock() && pValue->isBlock())
  {
    hb_retl(hb_codeblockId(pSelf) == hb_codeblockId(pValue));
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1070, nullptr, "==", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_EQUAL)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue == static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) == dValue);
  }
  else if (pSelf->isHash() && pValue->isHash())
  {
    hb_retl(hb_hashId(pSelf) == hb_hashId(pValue));
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1071, nullptr, "=", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_NOTEQUAL)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue != static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) != dValue);
  }
  else if (pSelf->isHash() && pValue->isHash())
  {
    hb_retl(hb_hashId(pSelf) != hb_hashId(pValue));
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1072, nullptr, "<>", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_LESS)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue < static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) < dValue);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1073, nullptr, "<", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_LESSEQ)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue <= static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) <= dValue);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1074, nullptr, "<=", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_GREATER)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue > static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) > dValue);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1075, nullptr, ">", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_GREATEREQ)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    auto dValue = hb_itemGetND(pSelf);
    hb_retl(dValue >= static_cast<double>(uc));
  }
  else if (hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto dValue = hb_itemGetND(pValue);
    hb_retl(static_cast<double>(uc) >= dValue);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1076, nullptr, ">=", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

/*
 * check if array/string index is in valid range, update it if necessary
 * in xHarbour compatibility mode where negative indexes are used to access
 * data from tail
 */
#define XHB_IS_VALID_INDEX(idx, max)                                                                                   \
  ((static_cast<HB_ISIZ>(idx) < 0 ? (idx) += (max) + 1 : (idx)) > 0 && static_cast<HB_SIZE>(idx) <= (max))

HB_FUNC(XHB_INDEX)
{
  auto pSelf = hb_stackSelfItem();
  auto pIndex = hb_param(1, Harbour::Item::ANY);

  if (hb_pcount() == 2)
  { /* ASSIGN */
    auto pValue = hb_param(2, Harbour::Item::ANY);
    if (pIndex->isNumeric())
    {
      HB_SIZE nIndex = hb_itemGetNS(pIndex);
      if (pSelf->isArray())
      {
        HB_SIZE nLen = hb_arrayLen(pSelf);
        if (XHB_IS_VALID_INDEX(nIndex, nLen))
        {
          hb_itemMoveRef(hb_arrayGetItemPtr(pSelf, nIndex), pValue);
        }
        else
        {
          hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
        }
      }
      else if (pSelf->isString())
      {
        HB_SIZE nLen = hb_itemGetCLen(pSelf);
        if (XHB_IS_VALID_INDEX(nIndex, nLen))
        {
          char cValue = pValue->isString() ? hb_itemGetCPtr(pValue)[0] : static_cast<char>(hb_itemGetNI(pValue));
          if (nLen == 1)
          {
            hb_itemPutCL(pSelf, &cValue, 1);
          }
          else
          {
            char *pszText;
            if (hb_itemGetWriteCL(pSelf, &pszText, &nLen) && nIndex > 0 && nIndex <= nLen)
            {
              pszText[nIndex - 1] = cValue;
            }
          }
        }
        else
        {
          hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 1069, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
      }
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1069, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
    }

    hb_itemReturn(pSelf);
  }
  else
  { /* ACCESS */
    if (pIndex->isNumeric())
    {
      HB_SIZE nIndex = hb_itemGetNS(pIndex);
      if (pSelf->isArray())
      {
        HB_SIZE nLen = hb_arrayLen(pSelf);
        if (XHB_IS_VALID_INDEX(nIndex, nLen))
        {
          hb_itemReturn(hb_arrayGetItemPtr(pSelf, nIndex));
        }
        else
        {
          hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pSelf, pIndex);
        }
      }
      else if (pSelf->isString())
      {
        HB_SIZE nLen = hb_itemGetCLen(pSelf);
        if (XHB_IS_VALID_INDEX(nIndex, nLen))
        {
          hb_retclen(hb_itemGetCPtr(pSelf) + nIndex - 1, 1);
        }
        else
        {
          hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pSelf, pIndex);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pSelf, pIndex);
      }
    }
    else
    {
      PHB_ITEM pResult =
          hb_errRT_BASE_Subst(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pSelf, pIndex);
      if (pResult)
      {
        hb_itemReturnRelease(pResult);
      }
    }
  }
}

HB_FUNC(XHB_PLUS)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    int iDec;
    double dValue = hb_itemGetNDDec(pSelf, &iDec);
    hb_retnlen(dValue + uc, 0, iDec);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    uc += static_cast<HB_UCHAR>(hb_itemGetNI(pValue));
    hb_retclen(reinterpret_cast<char *>(&uc), 1);
  }
  else if (pSelf->isHash() && pValue->isHash())
  {
    PHB_ITEM pHash = hb_hashClone(pSelf);
    hb_hashJoin(pHash, pValue, HB_HASH_UNION);
    hb_itemReturnRelease(pHash);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1081, nullptr, "+", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_MINUS)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    int iDec;
    double dValue = hb_itemGetNDDec(pSelf, &iDec);
    hb_retnlen(dValue - uc, 0, iDec);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    uc -= static_cast<HB_UCHAR>(hb_itemGetNI(pValue));
    hb_retclen(reinterpret_cast<char *>(&uc), 1);
  }
  else if (pSelf->isHash() && pValue->isHash())
  {
    PHB_ITEM pHash = hb_hashClone(pSelf);
    hb_hashRemove(pHash, pValue);
    hb_itemReturnRelease(pHash);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1082, nullptr, "-", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_INC)
{
  auto pSelf = hb_stackSelfItem();

  if (pSelf->isNumeric())
  {
    hb_retnd(hb_itemGetND(pSelf) + 1);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1)
  {
    HB_UCHAR uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]) + 1;
    hb_retclen(reinterpret_cast<char *>(&uc), 1);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1086, nullptr, "++", 1, pSelf);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_DEC)
{
  auto pSelf = hb_stackSelfItem();

  if (pSelf->isNumeric())
  {
    hb_retnd(hb_itemGetND(pSelf) - 1);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1)
  {
    HB_UCHAR uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]) - 1;
    hb_retclen(reinterpret_cast<char *>(&uc), 1);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1087, nullptr, "--", 1, pSelf);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_MULT)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    int iDec;
    double dValue = hb_itemGetNDDec(pSelf, &iDec);
    hb_retndlen(dValue * uc, 0, iDec);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    int iDec;
    double dValue = hb_itemGetNDDec(pValue, &iDec);
    hb_retndlen(static_cast<double>(uc) * dValue, 0, iDec);
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && hb_itemGetCLen(pValue) == 1)
  {
    auto uc1 = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto uc2 = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    hb_retnint(uc1 * uc2);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1083, nullptr, "*", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_DIV)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    if (uc == 0)
    {
      PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ZERODIV, 1340, nullptr, "/", 2, pSelf, pValue);
      if (pResult)
      {
        hb_itemReturnRelease(pResult);
      }
    }
    else
    {
      hb_retnd(hb_itemGetND(pSelf) / uc);
    }
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue &&
           (pValue->isNumeric() || hb_itemGetCLen(pValue) == 1))
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    double dDivisor = pValue->isNumeric() ? hb_itemGetND(pValue)
                                            : static_cast<double>(static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]));

    if (dDivisor == 0)
    {
      PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ZERODIV, 1340, nullptr, "/", 2, pSelf, pValue);
      if (pResult)
      {
        hb_itemReturnRelease(pResult);
      }
    }
    else
    {
      hb_retnd(static_cast<double>(uc) / dDivisor);
    }
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1084, nullptr, "/", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_MOD)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    if (uc == 0)
    {
      PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ZERODIV, 1341, nullptr, "%", 2, pSelf, pValue);
      if (pResult)
      {
        hb_itemReturnRelease(pResult);
      }
    }
    else
    {
      hb_retnd(fmod(hb_itemGetND(pSelf), static_cast<double>(uc)));
    }
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue &&
           (pValue->isNumeric() || hb_itemGetCLen(pValue) == 1))
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    double dDivisor = pValue->isNumeric() ? hb_itemGetND(pValue)
                                            : static_cast<double>(static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]));

    if (dDivisor == 0)
    {
      PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ZERODIV, 1341, nullptr, "%", 2, pSelf, pValue);
      if (pResult)
      {
        hb_itemReturnRelease(pResult);
      }
    }
    else
    {
      hb_retnd(fmod(static_cast<double>(uc), dDivisor));
    }
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1085, nullptr, "%", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}

HB_FUNC(XHB_POW)
{
  auto pSelf = hb_stackSelfItem();
  auto pValue = hb_param(1, Harbour::Item::ANY);

  if (pSelf->isNumeric() && hb_itemGetCLen(pValue) == 1)
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    hb_retnd(pow(hb_itemGetND(pSelf), static_cast<double>(uc)));
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && pValue && pValue->isNumeric())
  {
    auto uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    hb_retnd(pow(static_cast<double>(uc), hb_itemGetND(pValue)));
  }
  else if (pSelf->isString() && hb_itemGetCLen(pSelf) == 1 && hb_itemGetCLen(pValue) == 1)
  {
    auto uc1 = static_cast<HB_UCHAR>(hb_itemGetCPtr(pSelf)[0]);
    auto uc2 = static_cast<HB_UCHAR>(hb_itemGetCPtr(pValue)[0]);
    hb_retnd(pow(static_cast<double>(uc1), static_cast<double>(uc2)));
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1088, nullptr, "^", 2, pSelf, pValue);
    if (pResult)
    {
      hb_itemReturnRelease(pResult);
    }
  }
}
