//
// Compiler Expression Optimizer - reducing expressions
//
// Copyright 1999 Ryszard Glab
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

// NOTE: This must be the first definition
//    This is a common code shared by macro and standalone compiler
#define HB_COMMON_SUPPORT

#include "hbmacro.hpp"
#include "hbcomp.hpp"
#include "hbdate.hpp"
#include "hbmath.hpp"

static bool hb_compExprHasMacro(const char *szText, HB_SIZE nLen, HB_COMP_DECL)
{
  while (nLen--)
  {
    if (*szText++ == '&')
    {
      if (!HB_SUPPORT_HARBOUR ||
          (nLen && (*szText == '_' || (*szText >= 'A' && *szText <= 'Z') || (*szText >= 'a' && *szText <= 'z'))))
      {
        return true;
      }
    }
  }
  return false;
}

static PHB_EXPR hb_compExprReducePlusStrings(PHB_EXPR pLeft, PHB_EXPR pRight, HB_COMP_DECL)
{
  if (pLeft->value.asString.dealloc)
  {
    pLeft->value.asString.string =
        static_cast<char *>(hb_xrealloc(pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1));
    memcpy(pLeft->value.asString.string + pLeft->nLength, pRight->value.asString.string, pRight->nLength);
    pLeft->nLength += pRight->nLength;
    pLeft->value.asString.string[pLeft->nLength] = '\0';
  }
  else
  {
    auto szString = static_cast<char *>(hb_xgrab(pLeft->nLength + pRight->nLength + 1));
    memcpy(szString, pLeft->value.asString.string, pLeft->nLength);
    memcpy(szString + pLeft->nLength, pRight->value.asString.string, pRight->nLength);
    pLeft->nLength += pRight->nLength;
    szString[pLeft->nLength] = '\0';
    pLeft->value.asString.string = szString;
    pLeft->value.asString.dealloc = true;
  }
  HB_COMP_EXPR_FREE(pRight);
  return pLeft;
}

static PHB_EXPR hb_compExprReduceMinusStrings(PHB_EXPR pLeft, PHB_EXPR pRight, HB_COMP_DECL)
{
  char *szText = pLeft->value.asString.string;
  HB_SIZE nLen = pLeft->nLength;

  while (nLen && szText[nLen - 1] == ' ')
  {
    --nLen;
  }

  if (pLeft->value.asString.dealloc)
  {
    pLeft->value.asString.string =
        static_cast<char *>(hb_xrealloc(pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1));
    memcpy(pLeft->value.asString.string + nLen, pRight->value.asString.string, pRight->nLength);
    memset(pLeft->value.asString.string + nLen + pRight->nLength, ' ', pLeft->nLength - nLen);
    pLeft->nLength += pRight->nLength;
    pLeft->value.asString.string[pLeft->nLength] = '\0';
  }
  else
  {
    auto szString = static_cast<char *>(hb_xgrab(pLeft->nLength + pRight->nLength + 1));
    memcpy(szString, pLeft->value.asString.string, nLen);
    memcpy(szString + nLen, pRight->value.asString.string, pRight->nLength);
    memset(szString + nLen + pRight->nLength, ' ', pLeft->nLength - nLen);
    pLeft->nLength += pRight->nLength;
    szString[pLeft->nLength] = '\0';
    pLeft->value.asString.string = szString;
    pLeft->value.asString.dealloc = true;
  }
  HB_COMP_EXPR_FREE(pRight);
  return pLeft;
}

PHB_EXPR hb_compExprReduceMod(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC)
  {
    switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
    {
    case HB_ET_LONG:
      if (pRight->value.asNum.val.l)
      {
        pSelf->value.asNum.val.l = pLeft->value.asNum.val.l % pRight->value.asNum.val.l;
        pSelf->value.asNum.bDec = 0;
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        pSelf->value.asNum.NumType = HB_ET_LONG;
        pSelf->ExprType = HB_ET_NUMERIC;
        pSelf->ValType = HB_EV_NUMERIC;
        HB_COMP_EXPR_FREE(pLeft);
        HB_COMP_EXPR_FREE(pRight);
      }
      break;

    default:
      if (HB_SUPPORT_HARBOUR)
      {
        double dDivisor = pRight->value.asNum.NumType == HB_ET_LONG ? static_cast<double>(pRight->value.asNum.val.l)
                                                                    : pRight->value.asNum.val.d;
        if (dDivisor)
        {
          double dValue = pLeft->value.asNum.NumType == HB_ET_LONG ? static_cast<double>(pLeft->value.asNum.val.l)
                                                                   : pLeft->value.asNum.val.d;
          pSelf->value.asNum.val.d = fmod(dValue, dDivisor);
          pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
          pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
          pSelf->value.asNum.NumType = HB_ET_DOUBLE;
          pSelf->ExprType = HB_ET_NUMERIC;
          pSelf->ValType = HB_EV_NUMERIC;
          HB_COMP_EXPR_FREE(pLeft);
          HB_COMP_EXPR_FREE(pRight);
        }
      }
    }
  }
  else
  {
    // TODO: Check for incompatible types e.g.  3 % "txt"
  }
  return pSelf;
}

PHB_EXPR hb_compExprReduceDiv(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC)
  {
    HB_BYTE bType = (pLeft->value.asNum.NumType & pRight->value.asNum.NumType);

    switch (bType)
    {
    case HB_ET_LONG:

      if (pRight->value.asNum.val.l)
      {
        if (pLeft->value.asNum.val.l % pRight->value.asNum.val.l == 0)
        {
          // Return integer results as long
          pSelf->value.asNum.val.l = pLeft->value.asNum.val.l / pRight->value.asNum.val.l;
          pSelf->value.asNum.bDec = 0;
          pSelf->value.asNum.NumType = HB_ET_LONG;
        }
        else
        {
          // Return non-integer results as double
          pSelf->value.asNum.val.d =
              static_cast<double>(pLeft->value.asNum.val.l) / static_cast<double>(pRight->value.asNum.val.l);
          pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
          pSelf->value.asNum.NumType = HB_ET_DOUBLE;
        }
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        pSelf->ExprType = HB_ET_NUMERIC;
      }
      break;

    case HB_ET_DOUBLE:

      if (pRight->value.asNum.val.d != 0.0)
      {
        pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / pRight->value.asNum.val.d;
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
        pSelf->ExprType = HB_ET_NUMERIC;
      }
      break;

    default:

      if (pLeft->value.asNum.NumType == HB_ET_DOUBLE)
      {
        if (pRight->value.asNum.val.l)
        {
          pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / static_cast<double>(pRight->value.asNum.val.l);
          pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
          pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
          pSelf->value.asNum.NumType = HB_ET_DOUBLE;
          pSelf->ExprType = HB_ET_NUMERIC;
        }
      }
      else
      {
        if (pRight->value.asNum.val.d != 0.0)
        {
          pSelf->value.asNum.val.d = static_cast<double>(pLeft->value.asNum.val.l) / pRight->value.asNum.val.d;
          pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
          pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
          pSelf->value.asNum.NumType = HB_ET_DOUBLE;
          pSelf->ExprType = HB_ET_NUMERIC;
        }
      }

    } // switch bType

    if (pSelf->ExprType == HB_ET_NUMERIC)
    {
      // The expression was reduced - delete old components
      pSelf->ValType = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
  }
  else
  {
    // TODO: Check for incompatible types e.g.  3 / "txt"
  }
  return pSelf;
}

PHB_EXPR hb_compExprReduceMult(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC)
  {
    HB_BYTE bType = (pLeft->value.asNum.NumType & pRight->value.asNum.NumType);

    switch (bType)
    {
    case HB_ET_LONG:
    {
      HB_MAXDBL dVal =
          static_cast<HB_MAXDBL>(pLeft->value.asNum.val.l) * static_cast<HB_MAXDBL>(pRight->value.asNum.val.l);

      if (HB_DBL_LIM_LONG(dVal))
      {
        pSelf->value.asNum.val.l = pLeft->value.asNum.val.l * pRight->value.asNum.val.l;
        pSelf->value.asNum.NumType = HB_ET_LONG;
      }
      else
      {
        pSelf->value.asNum.val.d = static_cast<double>(dVal);
        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.bDec = 0;
      break;
    }

    case HB_ET_DOUBLE:

      pSelf->value.asNum.val.d = pLeft->value.asNum.val.d * pRight->value.asNum.val.d;
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.bDec =
          static_cast<HB_UCHAR>(HB_MIN(pLeft->value.asNum.bDec + pRight->value.asNum.bDec, HB_DEFAULT_DECIMALS));
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      break;

    default:

      if (pLeft->value.asNum.NumType == HB_ET_DOUBLE)
      {
        pSelf->value.asNum.val.d = pLeft->value.asNum.val.d * static_cast<double>(pRight->value.asNum.val.l);
        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
      }
      else
      {
        pSelf->value.asNum.val.d = static_cast<double>(pLeft->value.asNum.val.l) * pRight->value.asNum.val.d;
        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;
    }
    pSelf->ExprType = HB_ET_NUMERIC;
    pSelf->ValType = HB_EV_NUMERIC;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else
  {
    // TODO: Check for incompatible types e.g. 3 * "txt"
  }
  return pSelf;
}

PHB_EXPR hb_compExprReducePower(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC)
  {
    HB_BYTE bType = (pLeft->value.asNum.NumType & pRight->value.asNum.NumType);

    switch (bType)
    {
    case HB_ET_LONG:
      pSelf->value.asNum.val.d =
          pow(static_cast<double>(pLeft->value.asNum.val.l), static_cast<double>(pRight->value.asNum.val.l));
      break;

    case HB_ET_DOUBLE:
      pSelf->value.asNum.val.d = pow(pLeft->value.asNum.val.d, pRight->value.asNum.val.d);
      break;

    default:
      if (pLeft->value.asNum.NumType == HB_ET_DOUBLE)
      {
        pSelf->value.asNum.val.d = pow(pLeft->value.asNum.val.d, static_cast<double>(pRight->value.asNum.val.l));
      }
      else
      {
        pSelf->value.asNum.val.d = pow(static_cast<double>(pLeft->value.asNum.val.l), pRight->value.asNum.val.d);
      }
      break;
    }
    pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
    pSelf->value.asNum.NumType = HB_ET_DOUBLE;
    pSelf->ExprType = HB_ET_NUMERIC;
    pSelf->ValType = HB_EV_NUMERIC;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else
  {
    // TODO: Check for incompatible types e.g. 3 * "txt"
  }
  return pSelf;
}

static void hb_compExprReduceTimeStampPut(PHB_EXPR pExpr, long lJulian, long lMilliSec)
{
  // timestamp normalization
  if (lJulian < 0)
  {
    if (lMilliSec <= -HB_MILLISECS_PER_DAY)
    {
      lMilliSec += HB_MILLISECS_PER_DAY;
      --lJulian;
    }
    else if (lMilliSec > 0)
    {
      lMilliSec -= HB_MILLISECS_PER_DAY;
      ++lJulian;
      if (lMilliSec > 0)
      {
        lMilliSec -= HB_MILLISECS_PER_DAY;
        ++lJulian;
      }
    }
  }
  else
  {
    if (lMilliSec >= HB_MILLISECS_PER_DAY)
    {
      lMilliSec -= HB_MILLISECS_PER_DAY;
      ++lJulian;
    }
    else if (lMilliSec < 0)
    {
      lMilliSec += HB_MILLISECS_PER_DAY;
      --lJulian;
      if (lMilliSec < 0)
      {
        lMilliSec += HB_MILLISECS_PER_DAY;
        --lJulian;
      }
    }
  }

  pExpr->value.asDate.lDate = lJulian;
  pExpr->value.asDate.lTime = lMilliSec;
  pExpr->ExprType = HB_ET_TIMESTAMP;
  pExpr->ValType = HB_EV_TIMESTAMP;
}

static void hb_compExprReduceTimeStampAdd(PHB_EXPR pExpr, PHB_EXPR pTimeStamp, double dValue)
{
  long lJulian, lMilliSec;

  hb_timeStampUnpackDT(dValue, &lJulian, &lMilliSec);

  lJulian += pTimeStamp->value.asDate.lDate;
  lMilliSec += pTimeStamp->value.asDate.lTime;

  hb_compExprReduceTimeStampPut(pExpr, lJulian, lMilliSec);
}

PHB_EXPR hb_compExprReduceMinus(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC)
  {
    HB_BYTE bType = (pLeft->value.asNum.NumType & pRight->value.asNum.NumType);

    switch (bType)
    {
    case HB_ET_LONG:
    {
      HB_MAXDBL dVal =
          static_cast<HB_MAXDBL>(pLeft->value.asNum.val.l) - static_cast<HB_MAXDBL>(pRight->value.asNum.val.l);

      if (HB_DBL_LIM_LONG(dVal))
      {
        pSelf->value.asNum.val.l = pLeft->value.asNum.val.l - pRight->value.asNum.val.l;
        pSelf->value.asNum.NumType = HB_ET_LONG;
      }
      else
      {
        pSelf->value.asNum.val.d = static_cast<double>(dVal);
        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.bDec = 0;

      break;
    }

    case HB_ET_DOUBLE:

      pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - pRight->value.asNum.val.d;
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      if (pLeft->value.asNum.bDec < pRight->value.asNum.bDec)
      {
        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
      }
      else
      {
        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
      }
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;

      break;

    default:

      if (pLeft->value.asNum.NumType == HB_ET_DOUBLE)
      {
        pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - static_cast<double>(pRight->value.asNum.val.l);
        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
      }
      else
      {
        pSelf->value.asNum.val.d = static_cast<double>(pLeft->value.asNum.val.l) - pRight->value.asNum.val.d;
        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;
    }
    pSelf->ExprType = HB_ET_NUMERIC;
    pSelf->ValType = HB_EV_NUMERIC;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if ((pLeft->ExprType == HB_ET_DATE || pLeft->ExprType == HB_ET_TIMESTAMP) &&
           (pRight->ExprType == HB_ET_DATE || pRight->ExprType == HB_ET_TIMESTAMP))
  {
    long lTime = pLeft->value.asDate.lTime - pRight->value.asDate.lTime,
         lDate = pLeft->value.asDate.lDate - pRight->value.asDate.lDate;
    if (lTime == 0)
    {
      pSelf->value.asNum.val.l = lDate;
      pSelf->value.asNum.bDec = 0;
      pSelf->value.asNum.NumType = HB_ET_LONG;
    }
    else
    {
      pSelf->value.asNum.val.d = hb_timeStampPackDT(lDate, lTime);
      pSelf->value.asNum.bDec = HB_TIMEDIFF_DEC;
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;
    }
    pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    pSelf->ExprType = HB_ET_NUMERIC;
    pSelf->ValType = HB_EV_NUMERIC;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_NUMERIC)
  {
    if (pRight->value.asNum.NumType == HB_ET_LONG)
    {
      pSelf->value.asDate.lDate = pLeft->value.asDate.lDate - static_cast<long>(pRight->value.asNum.val.l);
    }
    else
    {
      pSelf->value.asDate.lDate = pLeft->value.asDate.lDate - HB_CAST_LONG(pRight->value.asNum.val.d);
    }
    pSelf->value.asDate.lTime = 0;
    pSelf->ExprType = HB_ET_DATE;
    pSelf->ValType = HB_EV_DATE;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_NUMERIC)
  {
    if (pRight->value.asNum.NumType == HB_ET_LONG)
    {
      hb_compExprReduceTimeStampPut(pSelf, pLeft->value.asDate.lDate - static_cast<long>(pRight->value.asNum.val.l),
                                    pLeft->value.asDate.lTime);
    }
    else
    {
      hb_compExprReduceTimeStampAdd(pSelf, pLeft, -pRight->value.asNum.val.d);
    }
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING)
  {
    if (pRight->nLength == 0)
    {
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (pLeft->nLength == 0)
    {
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pRight;
      HB_COMP_EXPR_FREE(pLeft);
    }
    else
    {
      auto fReduce = true;

      // Do not reduce strings with the macro operator '&'
      if (HB_SUPPORT_MACROTEXT)
      {
        char *szText = pLeft->value.asString.string;
        HB_SIZE nLen = pLeft->nLength;
        while (nLen && szText[nLen - 1] == ' ')
        {
          --nLen;
        }
        while (nLen--)
        {
          if (*szText++ == '&')
          {
            char ch = nLen ? *szText : *pRight->value.asString.string;
            if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || !HB_SUPPORT_HARBOUR)
            {
              fReduce = false;
              break;
            }
          }
        }
      }

      if (fReduce)
      {
        pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
        HB_COMP_EXPR_FREE(pSelf);
        pSelf = hb_compExprReduceMinusStrings(pLeft, pRight, HB_COMP_PARAM);
      }
    }
  }
  else
  {
    // TODO: Check for incompatible types e.g. "txt" - 3
  }
  return pSelf;
}

static bool hb_compExprReducePlusNums(PHB_EXPR pSelf, PHB_EXPR pAdd)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;
  PHB_EXPR pNum;

  if (pLeft->ExprType == HB_ET_NUMERIC)
  {
    pNum = pLeft;
  }
  else if (pRight->ExprType == HB_ET_NUMERIC)
  {
    pNum = pRight;
  }
  else if (pLeft->ExprType == HB_EO_PLUS)
  {
    return hb_compExprReducePlusNums(pLeft, pAdd);
  }
  else if (pRight->ExprType == HB_EO_PLUS)
  {
    return hb_compExprReducePlusNums(pRight, pAdd);
  }
  else
  {
    return false;
  }

  switch (pNum->value.asNum.NumType & pAdd->value.asNum.NumType)
  {
  case HB_ET_LONG:
  {
    HB_MAXDBL dVal = static_cast<HB_MAXDBL>(pNum->value.asNum.val.l) + static_cast<HB_MAXDBL>(pAdd->value.asNum.val.l);
    if (HB_DBL_LIM_LONG(dVal))
    {
      pNum->value.asNum.val.l += pAdd->value.asNum.val.l;
    }
    else
    {
      pNum->value.asNum.val.d = static_cast<double>(dVal);
      pNum->value.asNum.NumType = HB_ET_DOUBLE;
    }
    pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    pNum->value.asNum.bDec = 0;
    break;
  }

  case HB_ET_DOUBLE:
    pNum->value.asNum.val.d += pAdd->value.asNum.val.d;
    pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    if (pNum->value.asNum.bDec < pAdd->value.asNum.bDec)
    {
      pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
    }
    break;

  default:
    if (pNum->value.asNum.NumType == HB_ET_DOUBLE)
    {
      pNum->value.asNum.val.d += static_cast<double>(pAdd->value.asNum.val.l);
    }
    else
    {
      pNum->value.asNum.val.d = static_cast<double>(pNum->value.asNum.val.l) + pAdd->value.asNum.val.d;
      pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
      pNum->value.asNum.NumType = HB_ET_DOUBLE;
    }
    pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    break;
  }

  return true;
}

PHB_EXPR hb_compExprReducePlus(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_NUMERIC)
  {
    if (pRight->ExprType == HB_ET_NUMERIC)
    {
      HB_BYTE bType = (pLeft->value.asNum.NumType & pRight->value.asNum.NumType);

      switch (bType)
      {
      case HB_ET_LONG:
      {
        HB_MAXDBL dVal =
            static_cast<HB_MAXDBL>(pLeft->value.asNum.val.l) + static_cast<HB_MAXDBL>(pRight->value.asNum.val.l);

        if (HB_DBL_LIM_LONG(dVal))
        {
          pSelf->value.asNum.val.l = pLeft->value.asNum.val.l + pRight->value.asNum.val.l;
          pSelf->value.asNum.NumType = HB_ET_LONG;
        }
        else
        {
          pSelf->value.asNum.val.d = static_cast<double>(dVal);
          pSelf->value.asNum.NumType = HB_ET_DOUBLE;
        }
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        pSelf->value.asNum.bDec = 0;
        break;
      }

      case HB_ET_DOUBLE:
        pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + pRight->value.asNum.val.d;
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        if (pLeft->value.asNum.bDec < pRight->value.asNum.bDec)
        {
          pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
        }
        else
        {
          pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
        }
        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
        break;

      default:
        if (pLeft->value.asNum.NumType == HB_ET_DOUBLE)
        {
          pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + static_cast<double>(pRight->value.asNum.val.l);
          pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
        }
        else
        {
          pSelf->value.asNum.val.d = static_cast<double>(pLeft->value.asNum.val.l) + pRight->value.asNum.val.d;
          pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
        }
        pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      }
      pSelf->ExprType = HB_ET_NUMERIC;
      pSelf->ValType = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (pRight->ExprType == HB_ET_DATE)
    {
      if (pLeft->value.asNum.NumType == HB_ET_LONG)
      {
        pSelf->value.asDate.lDate = pRight->value.asDate.lDate + static_cast<long>(pLeft->value.asNum.val.l);
      }
      else
      {
        pSelf->value.asDate.lDate = pRight->value.asDate.lDate + HB_CAST_LONG(pLeft->value.asNum.val.d);
      }
      pSelf->value.asDate.lTime = 0;
      pSelf->ExprType = HB_ET_DATE;
      pSelf->ValType = HB_EV_DATE;
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (pRight->ExprType == HB_ET_TIMESTAMP)
    {
      if (pLeft->value.asNum.NumType == HB_ET_LONG)
      {
        hb_compExprReduceTimeStampPut(pSelf, pRight->value.asDate.lDate + static_cast<long>(pLeft->value.asNum.val.l),
                                      pRight->value.asDate.lTime);
      }
      else
      {
        hb_compExprReduceTimeStampAdd(pSelf, pRight, pLeft->value.asNum.val.d);
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (HB_SUPPORT_EXTOPT &&
             (pLeft->value.asNum.NumType == HB_ET_LONG ? pLeft->value.asNum.val.l == 0 : pLeft->value.asNum.val.d == 0))
    {
      // NOTE: This will not generate a runtime error if incompatible
      // data type is used
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pRight;
      HB_COMP_EXPR_FREE(pLeft);
    }
    else if (HB_SUPPORT_EXTOPT && pRight->ExprType == HB_EO_PLUS)
    {
      if (hb_compExprReducePlusNums(pRight, pLeft))
      {
        pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
        HB_COMP_EXPR_FREE(pSelf);
        pSelf = pRight;
        HB_COMP_EXPR_FREE(pLeft);
      }
    }
    else
    {
      // TODO: Check for incompatible types e.g. "txt" + 3
    }
  }
  else if (pRight->ExprType == HB_ET_NUMERIC)
  {
    if (pLeft->ExprType == HB_ET_DATE)
    {
      if (pRight->value.asNum.NumType == HB_ET_LONG)
      {
        pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + static_cast<long>(pRight->value.asNum.val.l);
      }
      else
      {
        pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + HB_CAST_LONG(pRight->value.asNum.val.d);
      }
      pSelf->value.asDate.lTime = 0;
      pSelf->ExprType = HB_ET_DATE;
      pSelf->ValType = HB_EV_DATE;
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (pLeft->ExprType == HB_ET_TIMESTAMP)
    {
      if (pRight->value.asNum.NumType == HB_ET_LONG)
      {
        hb_compExprReduceTimeStampPut(pSelf, pLeft->value.asDate.lDate + static_cast<long>(pRight->value.asNum.val.l),
                                      pLeft->value.asDate.lTime);
      }
      else
      {
        hb_compExprReduceTimeStampAdd(pSelf, pLeft, pRight->value.asNum.val.d);
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (HB_SUPPORT_EXTOPT && (pRight->value.asNum.NumType == HB_ET_LONG ? pRight->value.asNum.val.l == 0
                                                                             : pRight->value.asNum.val.d == 0))
    {
      // NOTE: This will not generate a runtime error if incompatible
      // data type is used
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (HB_SUPPORT_EXTOPT && pLeft->ExprType == HB_EO_PLUS)
    {
      if (hb_compExprReducePlusNums(pLeft, pRight))
      {
        pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
        HB_COMP_EXPR_FREE(pSelf);
        pSelf = pLeft;
        HB_COMP_EXPR_FREE(pRight);
      }
    }
    else
    {
      // TODO: Check for incompatible types e.g. "txt" + 3
    }
  }
  else if ((pLeft->ExprType == HB_ET_DATE || pLeft->ExprType == HB_ET_TIMESTAMP) &&
           (pRight->ExprType == HB_ET_DATE || pRight->ExprType == HB_ET_TIMESTAMP))
  {
    if (pLeft->ExprType == HB_ET_TIMESTAMP || pRight->ExprType == HB_ET_TIMESTAMP)
    {
      hb_compExprReduceTimeStampPut(pSelf, pLeft->value.asDate.lDate + pRight->value.asDate.lDate,
                                    pLeft->value.asDate.lTime + pRight->value.asDate.lTime);
    }
    else
    {
      // NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs.
      pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + pRight->value.asDate.lDate;
      pSelf->value.asDate.lTime = 0;
      pSelf->ExprType = HB_ET_DATE;
      pSelf->ValType = HB_EV_DATE;
    }
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING)
  {
    if (pRight->nLength == 0)
    {
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
      HB_COMP_EXPR_FREE(pRight);
    }
    else if (pLeft->nLength == 0)
    {
      pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pRight;
      HB_COMP_EXPR_FREE(pLeft);
    }
    else
    {
      auto fReduce = true;

      // Do not reduce strings with the macro operator '&'
      if (HB_SUPPORT_MACROTEXT)
      {
        char *szText = pLeft->value.asString.string;
        HB_SIZE nLen = pLeft->nLength;

        while (nLen--)
        {
          if (*szText++ == '&')
          {
            char ch = nLen ? *szText : *pRight->value.asString.string;
            if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_' || !HB_SUPPORT_HARBOUR)
            {
              fReduce = false;
              break;
            }
          }
        }
      }
      if (fReduce)
      {
        pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
        HB_COMP_EXPR_FREE(pSelf);
        pSelf = hb_compExprReducePlusStrings(pLeft, pRight, HB_COMP_PARAM);
      }
    }
  }
  else
  {
    // TODO: Check for incompatible types e.g. "txt" + 3
  }
  return pSelf;
}

PHB_EXPR hb_compExprReduceNegate(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pExpr = pSelf->value.asOperator.pLeft;

  if (pExpr->ExprType == HB_ET_NUMERIC)
  {
    if (pExpr->value.asNum.NumType == HB_ET_DOUBLE)
    {
      pExpr->value.asNum.val.d = -pExpr->value.asNum.val.d;
      pExpr->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    }
    else
    {
#if - HB_VMLONG_MAX > HB_VMLONG_MIN
      if (pExpr->value.asNum.val.l < -HB_VMLONG_MAX)
      {
        pExpr->value.asNum.NumType = HB_ET_DOUBLE;
        pExpr->value.asNum.val.d = -static_cast<double>(pExpr->value.asNum.val.l);
        pExpr->value.asNum.bDec = 0;
      }
      else
#endif
      {
        pExpr->value.asNum.val.l = -pExpr->value.asNum.val.l;
      }
      pExpr->value.asNum.bWidth = HB_DEFAULT_WIDTH;
    }
    pSelf->ExprType = HB_ET_NONE; // suppress deletion of operator components
    HB_COMP_EXPR_FREE(pSelf);
    pSelf = pExpr;
  }
  else if (pExpr->ExprType == HB_EO_NEGATE && HB_SUPPORT_EXTOPT)
  {
    // NOTE: This will not generate a runtime error if incompatible
    // data type is used
    pExpr->ExprType = HB_ET_NONE; // suppress deletion of operator components
    pExpr = pExpr->value.asOperator.pLeft;
    HB_COMP_EXPR_FREE(pSelf);
    pSelf = pExpr;
  }
  // TODO: add checking of incompatible types
  //   else
  //   {
  //   }

  return pSelf;
}

PHB_EXPR hb_compExprReduceIN(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType && pLeft->ExprType == HB_ET_STRING)
  {
    // Both arguments are literal strings

    // NOTE: If macro substitution is not disabled (-kM compiler
    //       switch) then we cannot reduce also strings which
    //       have macro operator '&'
    if (!HB_SUPPORT_MACROTEXT || (!hb_compExprHasMacro(pLeft->value.asString.string, pLeft->nLength, HB_COMP_PARAM) &&
                                  !hb_compExprHasMacro(pRight->value.asString.string, pRight->nLength, HB_COMP_PARAM)))
    {
      auto bResult = false;

      // NOTE: CA-Cl*pper has a bug where the $ operator returns .T.
      //       when an empty string is searched [vszakats]
      //
      //       But this bug exist only in compiler and CA-Cl*pper macro
      //       compiler does not have optimizer. This bug is replicated
      //       by us only when Harbour extensions in compiler (-kh) are
      //       not enabled f.e. in strict Clipper compatible mode (-kc)
      //       [druzus]
      if (pLeft->nLength == 0)
      {
        bResult = HB_COMP_PARAM->mode == HB_MODE_COMPILER && !HB_SUPPORT_HARBOUR;
      }
      else
      {
        bResult = (hb_strAt(pLeft->value.asString.string, pLeft->nLength, pRight->value.asString.string,
                            pRight->nLength) != 0);
      }

      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
  }
  // TODO: add checking for incompatible types
  return pSelf;
}

PHB_EXPR hb_compExprReduceNE(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      // .F. != .T.  = .T.
      // .T. != .T.  = .F.
      // .F. != .F.  = .F.
      // .T. != .F.  = .T.
      bool bResult = (pLeft->value.asLogical != pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_STRING:
      // NOTE: the result depends on SET EXACT setting then it
      // cannot be optimized except the case when null strings are
      // compared - "" != "" is always HB_FALSE regardless of EXACT
      // setting
      if ((pLeft->nLength | pRight->nLength) == 0)
      {
        HB_COMP_EXPR_FREE(pLeft);
        HB_COMP_EXPR_FREE(pRight);
        pSelf->ExprType = HB_ET_LOGICAL;
        pSelf->ValType = HB_EV_LOGICAL;
        pSelf->value.asLogical = false;

        // NOTE: COMPATIBILITY: Clipper doesn't optimize this
      }
      break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l != pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d != pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l != pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d != pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = pLeft->value.asDate.lDate != pRight->value.asDate.lDate ||
                     pLeft->value.asDate.lTime != pRight->value.asDate.lTime;
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_NIL:
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = false;
      break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate != pRight->value.asDate.lDate;
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (HB_SUPPORT_EXTOPT && (pLeft->ExprType == HB_ET_LOGICAL || pRight->ExprType == HB_ET_LOGICAL))
  {
    // NOTE: This will not generate a runtime error if incompatible
    // data type is used

    if (pLeft->ExprType == HB_ET_LOGICAL)
    {
      pSelf->value.asOperator.pLeft = pRight;
      pRight = pLeft;
      pLeft = pSelf->value.asOperator.pRight;
    }

    if (!pRight->value.asLogical)
    {
      pSelf->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
    }
    else if (pLeft->ExprType == HB_EO_NOT)
    {
      pSelf->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft->value.asOperator.pLeft;
      pLeft->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pLeft);
    }
    else
    {
      pSelf->ExprType = HB_EO_NOT;
      pSelf->value.asOperator.pRight = nullptr;
    }
    HB_COMP_EXPR_FREE(pRight);
  }
  else if ((pLeft->ExprType == HB_ET_NIL &&
            (pRight->ExprType == HB_ET_NUMERIC || pRight->ExprType == HB_ET_LOGICAL || pRight->ExprType == HB_ET_DATE ||
             pRight->ExprType == HB_ET_TIMESTAMP || pRight->ExprType == HB_ET_STRING ||
             pRight->ExprType == HB_ET_CODEBLOCK || pRight->ExprType == HB_ET_ARRAY || pRight->ExprType == HB_ET_HASH ||
             pRight->ExprType == HB_ET_FUNREF)) ||
           (pRight->ExprType == HB_ET_NIL &&
            (pLeft->ExprType == HB_ET_NUMERIC || pLeft->ExprType == HB_ET_LOGICAL || pLeft->ExprType == HB_ET_DATE ||
             pLeft->ExprType == HB_ET_TIMESTAMP || pLeft->ExprType == HB_ET_STRING ||
             pLeft->ExprType == HB_ET_CODEBLOCK || pLeft->ExprType == HB_ET_ARRAY || pLeft->ExprType == HB_ET_HASH ||
             pLeft->ExprType == HB_ET_FUNREF)))
  {
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    pSelf->value.asLogical = true;
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceGE(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      // .T. >= .F.  = .T.
      // .T. >= .T.  = .T.
      // .F. >= .F.  = .T.
      // .F. >= .T.  = .f.
      bool bResult = !(!pLeft->value.asLogical && pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l >= pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d >= pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l >= pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d >= pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = (pLeft->value.asDate.lDate > pRight->value.asDate.lDate) ||
                     (pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                      pLeft->value.asDate.lTime >= pRight->value.asDate.lTime);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate >= pRight->value.asDate.lDate;
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceLE(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      // .T. <= .F.  = .F.
      // .T. <= .T.  = .T.
      // .F. <= .F.  = .T.
      // .F. <= .T.  = .T.
      bool bResult = !(pLeft->value.asLogical && !pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l <= pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d <= pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l <= pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d <= pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = (pLeft->value.asDate.lDate < pRight->value.asDate.lDate) ||
                     (pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                      pLeft->value.asDate.lTime <= pRight->value.asDate.lTime);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate <= pRight->value.asDate.lDate;
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceGT(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      // .T. > .F.  = .T.
      // .T. > .T.  = .F.
      // .F. > .F.  = .F.
      // .F. > .T.  = .F.
      bool bResult = (pLeft->value.asLogical && !pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l > pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d > pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l > pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d > pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = (pLeft->value.asDate.lDate > pRight->value.asDate.lDate) ||
                     (pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                      pLeft->value.asDate.lTime > pRight->value.asDate.lTime);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate > pRight->value.asDate.lDate;
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceLT(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      // .F. < .T.  = .T.
      // .T. < .T.  = .F.
      // .F. < .F.  = .F.
      // .T. < .F.  = .F.
      bool bResult = (!pLeft->value.asLogical && pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l < pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d < pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l < pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d < pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = (pLeft->value.asDate.lDate < pRight->value.asDate.lDate) ||
                     (pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                      pLeft->value.asDate.lTime < pRight->value.asDate.lTime);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
    }
    break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate < pRight->value.asDate.lDate;
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceEQ(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == pRight->ExprType)
  {
    switch (pLeft->ExprType)
    {
    case HB_ET_LOGICAL:
    {
      bool bResult = (pLeft->value.asLogical == pRight->value.asLogical);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
      break;
    }

    case HB_ET_STRING:
      // NOTE: when not exact comparison (==) is used
      // the result depends on SET EXACT setting then it
      // cannot be optimized except the case when null strings are
      // compared - "" = "" is always TRUE regardless of EXACT
      // setting.
      // If macro substitution is not disabled (-kM compiler
      // switch) then we cannot reduce also strings which
      // have macro operator '&'
      if ((pLeft->nLength | pRight->nLength) == 0 ||
          (pSelf->ExprType == HB_EO_EQ &&
           (!HB_SUPPORT_MACROTEXT ||
            (!hb_compExprHasMacro(pLeft->value.asString.string, pLeft->nLength, HB_COMP_PARAM) &&
             !hb_compExprHasMacro(pRight->value.asString.string, pRight->nLength, HB_COMP_PARAM)))))
      {
        bool bResult = pLeft->nLength == pRight->nLength &&
                       memcmp(pLeft->value.asString.string, pRight->value.asString.string, pLeft->nLength) == 0;
        HB_COMP_EXPR_FREE(pLeft);
        HB_COMP_EXPR_FREE(pRight);
        pSelf->ExprType = HB_ET_LOGICAL;
        pSelf->ValType = HB_EV_LOGICAL;
        pSelf->value.asLogical = bResult;
      }
      break;

    case HB_ET_NUMERIC:
    {
      auto bResult = false;

      switch (pLeft->value.asNum.NumType & pRight->value.asNum.NumType)
      {
      case HB_ET_LONG:
        bResult = (pLeft->value.asNum.val.l == pRight->value.asNum.val.l);
        break;
      case HB_ET_DOUBLE:
        bResult = (pLeft->value.asNum.val.d == pRight->value.asNum.val.d);
        break;
      default:
        if (pLeft->value.asNum.NumType == HB_ET_LONG)
        {
          bResult = (pLeft->value.asNum.val.l == pRight->value.asNum.val.d);
        }
        else
        {
          bResult = (pLeft->value.asNum.val.d == pRight->value.asNum.val.l);
        }
        break;
      }
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
      break;
    }

    case HB_ET_DATE:
    case HB_ET_TIMESTAMP:
    {
      bool bResult = (pLeft->value.asDate.lDate == pRight->value.asDate.lDate) &&
                     (pLeft->value.asDate.lTime == pRight->value.asDate.lTime);
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
      break;
    }

    case HB_ET_NIL:
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = true;
      break;
    }
  }
  else if ((pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_DATE) ||
           (pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_TIMESTAMP))
  {
    pSelf->value.asLogical = pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                             (pLeft->value.asDate.lTime == pRight->value.asDate.lTime || pSelf->ExprType != HB_EO_EQ);
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
  }
  else if (HB_SUPPORT_EXTOPT && (pLeft->ExprType == HB_ET_LOGICAL || pRight->ExprType == HB_ET_LOGICAL))
  {
    // NOTE: This will not generate a runtime error if incompatible
    // data type is used

    if (pLeft->ExprType == HB_ET_LOGICAL)
    {
      pSelf->value.asOperator.pLeft = pRight;
      pRight = pLeft;
      pLeft = pSelf->value.asOperator.pRight;
    }

    if (pRight->value.asLogical)
    {
      pSelf->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
    }
    else if (pLeft->ExprType == HB_EO_NOT)
    {
      pSelf->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft->value.asOperator.pLeft;
      pLeft->ExprType = HB_ET_NONE;
      HB_COMP_EXPR_FREE(pLeft);
    }
    else
    {
      pSelf->ExprType = HB_EO_NOT;
      pSelf->value.asOperator.pRight = nullptr;
    }
    HB_COMP_EXPR_FREE(pRight);
  }
  else if ((pLeft->ExprType == HB_ET_NIL &&
            (pRight->ExprType == HB_ET_NUMERIC || pRight->ExprType == HB_ET_LOGICAL || pRight->ExprType == HB_ET_DATE ||
             pRight->ExprType == HB_ET_TIMESTAMP || pRight->ExprType == HB_ET_STRING ||
             pRight->ExprType == HB_ET_CODEBLOCK || pRight->ExprType == HB_ET_ARRAY || pRight->ExprType == HB_ET_HASH ||
             pRight->ExprType == HB_ET_FUNREF)) ||
           (pRight->ExprType == HB_ET_NIL &&
            (pLeft->ExprType == HB_ET_NUMERIC || pLeft->ExprType == HB_ET_LOGICAL || pLeft->ExprType == HB_ET_DATE ||
             pLeft->ExprType == HB_ET_TIMESTAMP || pLeft->ExprType == HB_ET_STRING ||
             pLeft->ExprType == HB_ET_CODEBLOCK || pLeft->ExprType == HB_ET_ARRAY || pLeft->ExprType == HB_ET_HASH ||
             pLeft->ExprType == HB_ET_FUNREF)))
  {
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    pSelf->value.asLogical = false;
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceAnd(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL)
  {
    bool bResult = pLeft->value.asLogical && pRight->value.asLogical;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    pSelf->value.asLogical = bResult;
  }
  else if (pLeft->ExprType == HB_ET_LOGICAL && HB_COMP_ISSUPPORTED(HB_COMPFLAG_SHORTCUTS))
  {
    if (pLeft->value.asLogical)
    {
      // .T. .AND. expr => expr
      HB_COMP_EXPR_FREE(pLeft);
      pSelf->ExprType = HB_ET_NONE; // don't delete expression components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pRight;
    }
    else
    {
      // .F. .AND. expr => .F.
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight); // discard expression
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = false;
    }
  }
  else if (pRight->ExprType == HB_ET_LOGICAL && HB_COMP_ISSUPPORTED(HB_COMPFLAG_SHORTCUTS) &&
           (HB_COMP_PARAM->mode == HB_MODE_COMPILER || HB_SUPPORT_HARBOUR))
  {
    if (pRight->value.asLogical)
    {
      // expr .AND. .T. => expr
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_NONE; // don't delete expression components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
    }
    else
    {
      // expr .AND. .F. => .F.
      HB_COMP_EXPR_FREE(pLeft); // discard expression
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = false;
    }
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceOr(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pLeft = pSelf->value.asOperator.pLeft;
  PHB_EXPR pRight = pSelf->value.asOperator.pRight;

  if (pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL)
  {
    bool bResult = pLeft->value.asLogical || pRight->value.asLogical;
    HB_COMP_EXPR_FREE(pLeft);
    HB_COMP_EXPR_FREE(pRight);
    pSelf->ExprType = HB_ET_LOGICAL;
    pSelf->ValType = HB_EV_LOGICAL;
    pSelf->value.asLogical = bResult;
  }
  else if (pLeft->ExprType == HB_ET_LOGICAL && HB_COMP_ISSUPPORTED(HB_COMPFLAG_SHORTCUTS))
  {
    if (pLeft->value.asLogical)
    {
      // .T. .OR. expr => .T.
      HB_COMP_EXPR_FREE(pLeft);
      HB_COMP_EXPR_FREE(pRight); // discard expression
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = true;
    }
    else
    {
      // .F. .OR. expr => expr
      HB_COMP_EXPR_FREE(pLeft);
      pSelf->ExprType = HB_ET_NONE; // don't delete expression components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pRight;
    }
  }
  else if (pRight->ExprType == HB_ET_LOGICAL && HB_COMP_ISSUPPORTED(HB_COMPFLAG_SHORTCUTS) &&
           (HB_COMP_PARAM->mode == HB_MODE_COMPILER || HB_SUPPORT_HARBOUR))
  {
    if (pRight->value.asLogical)
    {
      // expr .OR. .T. => .T.
      HB_COMP_EXPR_FREE(pLeft); // discard expression
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType = HB_EV_LOGICAL;
      pSelf->value.asLogical = true;
    }
    else
    {
      // expr .OR. .F. => expr
      HB_COMP_EXPR_FREE(pRight);
      pSelf->ExprType = HB_ET_NONE; // don't delete expression components
      HB_COMP_EXPR_FREE(pSelf);
      pSelf = pLeft;
    }
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }
  return pSelf;
}

PHB_EXPR hb_compExprReduceNot(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pExpr = pSelf->value.asOperator.pLeft;

  if (pExpr->ExprType == HB_ET_LOGICAL)
  {
    pExpr->value.asLogical = !pExpr->value.asLogical;
    HB_COMP_EXPR_CLEAR(pSelf); // suppress deletion of operator components
    pSelf = pExpr;
  }
  else if (pExpr->ExprType == HB_EO_NOT && HB_SUPPORT_EXTOPT)
  {
    // NOTE: This will not generate a runtime error if incompatible
    // data type is used
    pExpr->ExprType = HB_ET_NONE; // suppress deletion of operator components
    pExpr = pExpr->value.asOperator.pLeft;
    HB_COMP_EXPR_FREE(pSelf);
    pSelf = pExpr;
  }
  // TODO: add checking of incompatible types
  // else
  // {
  // }

  return pSelf;
}

PHB_EXPR hb_compExprReduceIIF(PHB_EXPR pSelf, HB_COMP_DECL)
{
  // get conditional expression
  PHB_EXPR pExpr = pSelf->value.asList.pExprList;

  if (pExpr->ExprType == HB_ET_LOGICAL)
  {
    // the condition was reduced to a logical value: .T. or .F.
    if (pExpr->value.asLogical)
    {
      // .T. was specified
      pExpr = pExpr->pNext; // skip to TRUE expression
      // delete condition  - it is no longer needed
      HB_COMP_EXPR_FREE(pSelf->value.asList.pExprList);
      // assign nullptr to a start of expressions list to suppress
      // deletion of expression's components - we are deleting them
      // here
      pSelf->value.asList.pExprList = nullptr;
      HB_COMP_EXPR_FREE(pSelf);
      // store the TRUE expression as a result of reduction
      pSelf = pExpr;
      pExpr = pExpr->pNext;     // skip to FALSE expression
      HB_COMP_EXPR_FREE(pExpr); // delete FALSE expression
      pSelf->pNext = nullptr;
    }
    else
    {
      // .F. was specified
      pExpr = pExpr->pNext; // skip to TRUE expression
      // delete condition  - it is no longer needed
      HB_COMP_EXPR_FREE(pSelf->value.asList.pExprList);
      // assign nullptr to a start of expressions list to suppress
      // deletion of expression's components - we are deleting them
      // here
      pSelf->value.asList.pExprList = nullptr;
      HB_COMP_EXPR_FREE(pSelf);
      // store the FALSE expression as a result of reduction
      pSelf = pExpr->pNext;
      HB_COMP_EXPR_FREE(pExpr); // delete TRUE expression
      pSelf->pNext = nullptr;
    }

    // this will cause warning when IIF is used as statement
#if 0
    if (pSelf->ExprType == HB_ET_NONE)
    {
      pSelf->ExprType = HB_ET_NIL;
      pSelf->ValType = HB_EV_NIL;
    }
#endif
  }
  // check if valid expression is passed
  else if (pExpr->ExprType == HB_ET_NIL || pExpr->ExprType == HB_ET_NUMERIC || pExpr->ExprType == HB_ET_DATE ||
           pExpr->ExprType == HB_ET_TIMESTAMP || pExpr->ExprType == HB_ET_STRING ||
           pExpr->ExprType == HB_ET_CODEBLOCK || pExpr->ExprType == HB_ET_ARRAY || pExpr->ExprType == HB_ET_HASH ||
           pExpr->ExprType == HB_ET_VARREF || pExpr->ExprType == HB_ET_REFERENCE || pExpr->ExprType == HB_ET_FUNREF)
  {
    HB_COMP_ERROR_TYPE(pExpr);
  }
  return pSelf;
}

// replace the list containing a single expression with a simple expression
// - strips parenthesis
//  ( EXPR ) -> EXPR
PHB_EXPR hb_compExprListStrip(PHB_EXPR pSelf, HB_COMP_DECL)
{
  while (pSelf->ExprType == HB_ET_LIST && hb_compExprListLen(pSelf) == 1 &&
         pSelf->value.asList.pExprList->ExprType <= HB_ET_VARIABLE &&
         !hb_compExprIsArrayToParams(pSelf->value.asList.pExprList))
  {
    // replace the list with a simple expression
    //  ( EXPR ) -> EXPR
    PHB_EXPR pExpr = pSelf;
    pSelf = pSelf->value.asList.pExprList;
    pExpr->value.asList.pExprList = nullptr;
    HB_COMP_EXPR_FREE(pExpr);
  }

  return pSelf;
}

HB_BOOL hb_compExprReduceAT(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pSub = pParms->value.asList.pExprList;
  PHB_EXPR pText = pSub->pNext;

  if (pSub->ExprType == HB_ET_STRING && pText->ExprType == HB_ET_STRING && !HB_SUPPORT_USERCP)
  {
    PHB_EXPR pReduced;

    // NOTE: CA-Cl*pper has a bug in At("", cText) compile time
    //       optimization and always set 1 as result in such cases.
    //       This bug exist only in compiler and CA-Cl*pper macro
    //       compiler does not have optimizer. This bug is replicated
    //       by us only when Harbour extensions in compiler (-kh) are
    //       not enabled f.e. in strict Clipper compatible mode (-kc)
    //       [druzus]
    if (pSub->nLength == 0)
    {
      pReduced =
          hb_compExprNewLong((HB_COMP_PARAM->mode == HB_MODE_COMPILER && !HB_SUPPORT_HARBOUR) ? 1 : 0, HB_COMP_PARAM);
    }
    else
    {
      pReduced = hb_compExprNewLong(
          hb_strAt(pSub->value.asString.string, pSub->nLength, pText->value.asString.string, pText->nLength),
          HB_COMP_PARAM);
    }

    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pParms);

    memcpy(pSelf, pReduced, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pReduced);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_compExprReduceCHR(PHB_EXPR pSelf, HB_COMP_DECL)
{
  auto fDoOpt = false;
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_NUMERIC)
  {
    if (HB_SUPPORT_USERCP)
    {
      int iVal = pArg->value.asNum.NumType == HB_ET_LONG ? static_cast<int>(pArg->value.asNum.val.l)
                                                         : static_cast<int>(pArg->value.asNum.val.d);
      fDoOpt = iVal >= 0 && iVal <= 127;
    }
    else
    {
      fDoOpt = true;
    }
  }

  // try to change it into a string
  if (fDoOpt)
  {
    // NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
    //       Chr() cases where the passed parameter is a constant which
    //       can be divided by 256 but it's not zero, in this case it
    //       will return an empty string instead of a Chr(0). [vszakats]
    //
    //       But this bug exist only in compiler and CA-Cl*pper macro
    //       compiler does not have optimizer. This bug is replicated
    //       by us only when Harbour extensions in compiler (-kh) are
    //       not enabled f.e. in strict Clipper compatible mode (-kc)
    //       [druzus]

    PHB_EXPR pExpr = HB_COMP_EXPR_NEW(HB_ET_STRING);

    pExpr->ValType = HB_EV_STRING;
    if (pArg->value.asNum.NumType == HB_ET_LONG)
    {
      if (HB_COMP_PARAM->mode == HB_MODE_COMPILER && !HB_SUPPORT_HARBOUR && (pArg->value.asNum.val.l & 0xff) == 0 &&
          pArg->value.asNum.val.l != 0)
      {
        pExpr->value.asString.string = const_cast<char *>("");
        pExpr->value.asString.dealloc = false;
        pExpr->nLength = 0;
      }
      else
      {
        pExpr->value.asString.string = const_cast<char *>(hb_szAscii[static_cast<int>(pArg->value.asNum.val.l) & 0xff]);
        pExpr->value.asString.dealloc = false;
        pExpr->nLength = 1;
      }
    }
    else
    {
      pExpr->value.asString.string = const_cast<char *>(hb_szAscii[HB_CAST_INT(pArg->value.asNum.val.d) & 0xff]);
      pExpr->value.asString.dealloc = false;
      pExpr->nLength = 1;
    }

    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceBCHAR(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_NUMERIC)
  {
    PHB_EXPR pExpr = HB_COMP_EXPR_NEW(HB_ET_STRING);

    pExpr->ValType = HB_EV_STRING;
    pExpr->value.asString.string = const_cast<char *>(
        hb_szAscii[(pArg->value.asNum.NumType == HB_ET_LONG ? static_cast<int>(pArg->value.asNum.val.l)
                                                            : HB_CAST_INT(pArg->value.asNum.val.d)) &
                   0xff]);
    pExpr->value.asString.dealloc = false;
    pExpr->nLength = 1;

    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceLEN(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  // FIXME: do not optimize when array/hash args have user expressions
  if ((pArg->ExprType == HB_ET_STRING && !HB_SUPPORT_USERCP) || pArg->ExprType == HB_ET_ARRAY ||
      pArg->ExprType == HB_ET_HASH)
  {
    PHB_EXPR pExpr =
        hb_compExprNewLong(pArg->ExprType == HB_ET_HASH ? pArg->nLength >> 1 : pArg->nLength, HB_COMP_PARAM);

    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }
  return false;
}

HB_BOOL hb_compExprReduceEMPTY(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;
  auto fReduced = true;
  auto fResult = false;

  switch (pArg->ExprType)
  {
  case HB_ET_STRING:
    fResult = hb_strEmpty(pArg->value.asString.string, pArg->nLength);
    break;

  case HB_ET_ARRAY:
  case HB_ET_HASH:
    // FIXME: do not optimize when array/hash args have user expressions
    fResult = pArg->nLength == 0;
    break;

  case HB_ET_NUMERIC:
    if (pArg->value.asNum.NumType == HB_ET_DOUBLE)
    {
      fResult = pArg->value.asNum.val.d == 0.0;
    }
    else
    {
      fResult = pArg->value.asNum.val.l == 0;
    }
    break;

  case HB_ET_LOGICAL:
    fResult = !pArg->value.asLogical;
    break;

  case HB_ET_NIL:
    fResult = true;
    break;

  case HB_ET_DATE:
    fResult = pArg->value.asDate.lDate == 0;
    break;

  case HB_ET_TIMESTAMP:
    fResult = pArg->value.asDate.lDate == 0 && pArg->value.asDate.lTime == 0;
    break;

  case HB_ET_CODEBLOCK:
    break;

  // case HB_ET_FUNREF:
  default:
    fReduced = false;
  }

  if (fReduced)
  {
    PHB_EXPR pExpr = hb_compExprNewLogical(fResult, HB_COMP_PARAM);
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }
  return false;
}

HB_BOOL hb_compExprReduceASC(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_STRING &&
      (!HB_SUPPORT_USERCP || static_cast<HB_UCHAR>(pArg->value.asString.string[0]) <= 127))
  {
    PHB_EXPR pExpr = hb_compExprNewLong(static_cast<HB_UCHAR>(pArg->value.asString.string[0]), HB_COMP_PARAM);
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }
  return false;
}

HB_BOOL hb_compExprReduceBCODE(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_STRING)
  {
    PHB_EXPR pExpr = hb_compExprNewLong(static_cast<HB_UCHAR>(pArg->value.asString.string[0]), HB_COMP_PARAM);
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }
  return false;
}

HB_BOOL hb_compExprReduceINT(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_NUMERIC)
  {
    PHB_EXPR pExpr;

    if (pArg->value.asNum.NumType == HB_ET_LONG)
    {
      pExpr = hb_compExprNewLong(pArg->value.asNum.val.l, HB_COMP_PARAM);
    }
    else
    {
      HB_MAXDBL dVal = static_cast<HB_MAXDBL>(pArg->value.asNum.val.d);
      if (HB_DBL_LIM_LONG(dVal))
      {
        pExpr = hb_compExprNewLong(static_cast<HB_MAXINT>(pArg->value.asNum.val.d), HB_COMP_PARAM);
      }
      else
      {
        pExpr = hb_compExprNewDouble(pArg->value.asNum.val.d, pArg->value.asNum.bWidth, 0, HB_COMP_PARAM);
      }
    }
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }
  return false;
}

HB_BOOL hb_compExprReduceSTOT(PHB_EXPR pSelf, HB_USHORT usCount, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms ? pParms->value.asList.pExprList : nullptr;
  PHB_EXPR pExpr = nullptr;

  if (usCount == 0)
  {
    pExpr = hb_compExprNewTimeStamp(0, 0, HB_COMP_PARAM);
  }
  else if (pArg && pArg->ExprType == HB_ET_STRING)
  {
    long lDate, lTime;

    hb_timeStampStrRawGet(pArg->value.asString.string, &lDate, &lTime);
    pExpr = hb_compExprNewTimeStamp(lDate, lTime, HB_COMP_PARAM);
  }

  if (pExpr)
  {
    if (pSelf->value.asFunCall.pParms)
    {
      HB_COMP_EXPR_FREE(pParms);
    }
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceSTOD(PHB_EXPR pSelf, HB_USHORT usCount, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms ? pParms->value.asList.pExprList : nullptr;
  PHB_EXPR pExpr = nullptr;

  if (usCount == 0)
  {
    pExpr = hb_compExprNewDate(0, HB_COMP_PARAM);
  }
  else if (pArg && pArg->ExprType == HB_ET_STRING && (pArg->nLength >= 7 || pArg->nLength == 0))
  {
    pExpr = hb_compExprNewDate(pArg->nLength == 0 ? 0 : hb_dateEncStr(pArg->value.asString.string), HB_COMP_PARAM);
  }

  if (pExpr)
  {
    if (pSelf->value.asFunCall.pParms)
    {
      HB_COMP_EXPR_FREE(pParms);
    }
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceDTOS(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_DATE || pArg->ExprType == HB_ET_TIMESTAMP)
  {
    char szBuffer[9];
    char *szDate = static_cast<char *>(
        memcpy(hb_xgrab(9), hb_dateDecStr(szBuffer, static_cast<long>(pArg->value.asDate.lDate)), 9));
    PHB_EXPR pExpr = hb_compExprNewString(szDate, 8, true, HB_COMP_PARAM);
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceCTOD(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_STRING && pArg->nLength == 0)
  {
    PHB_EXPR pExpr = hb_compExprNewDate(0, HB_COMP_PARAM);
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceUPPER(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pArg = pParms->value.asList.pExprList;

  if (pArg->ExprType == HB_ET_STRING)
  {
    HB_SIZE nLen = pArg->nLength;
    auto fLower = false;

    if (nLen)
    {
      const char *szValue = pArg->value.asString.string;
      do
      {
        char c = *szValue++;
        if (c >= 'a' && c <= 'z')
        {
          fLower = true;
        }
        else if (!((c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == ' '))
        {
          break;
        }
      } while (--nLen);
    }

    if (nLen == 0)
    {
      PHB_EXPR pExpr;
      char *szValue;
      auto fDealloc = false;

      if (fLower)
      {
        if (pArg->nLength == 1)
        {
          szValue =
              const_cast<char *>(hb_szAscii[HB_TOUPPER(static_cast<unsigned char>(pArg->value.asString.string[0]))]);
          fDealloc = false;
        }
        else
        {
          if (pArg->value.asString.dealloc)
          {
            szValue = pArg->value.asString.string;
            pArg->value.asString.dealloc = false;
            fDealloc = true;
          }
          else
          {
            szValue = static_cast<char *>(hb_xgrab(pArg->nLength + 1));
            memcpy(szValue, pArg->value.asString.string, pArg->nLength + 1);
            fDealloc = true;
          }
          do
          {
            szValue[nLen] = static_cast<char>(HB_TOUPPER(static_cast<unsigned char>(szValue[nLen])));
          } while (++nLen < pArg->nLength);
        }
      }
      else
      {
        szValue = pArg->value.asString.string;
        fDealloc = pArg->value.asString.dealloc;
        pArg->value.asString.dealloc = false;
      }

      pExpr = HB_COMP_EXPR_NEW(HB_ET_STRING);
      pExpr->ValType = HB_EV_STRING;
      pExpr->value.asString.string = szValue;
      pExpr->value.asString.dealloc = fDealloc;
      pExpr->nLength = pArg->nLength;

      HB_COMP_EXPR_FREE(pParms);
      HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
      memcpy(pSelf, pExpr, sizeof(HB_EXPR));
      HB_COMP_EXPR_CLEAR(pExpr);

      return true;
    }
  }

  return false;
}

HB_BOOL hb_compExprReduceMIN(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pFirst = pParms->value.asList.pExprList;
  PHB_EXPR pNext = pFirst->pNext;
  PHB_EXPR pExpr = nullptr;

  if (pFirst->ExprType == pNext->ExprType)
  {

    if (pFirst->ExprType == HB_ET_NUMERIC)
    {
      HB_BYTE bType = (pFirst->value.asNum.NumType & pNext->value.asNum.NumType);

      switch (bType)
      {
      case HB_ET_LONG:
        pExpr = pFirst->value.asNum.val.l <= pNext->value.asNum.val.l ? pFirst : pNext;
        break;

      case HB_ET_DOUBLE:
        pExpr = pFirst->value.asNum.val.d <= pNext->value.asNum.val.d ? pFirst : pNext;
        break;

      default:
        if (pFirst->value.asNum.NumType == HB_ET_DOUBLE)
        {
          pExpr = (pFirst->value.asNum.val.d <= static_cast<double>(pNext->value.asNum.val.l)) ? pFirst : pNext;
        }
        else
        {
          pExpr = (static_cast<double>(pFirst->value.asNum.val.l) <= pNext->value.asNum.val.d) ? pFirst : pNext;
        }
      }
    }
    else if (pFirst->ExprType == HB_ET_DATE)
    {
      pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ? pFirst : pNext;
    }
    else if (pFirst->ExprType == HB_ET_TIMESTAMP)
    {
      pExpr = (pFirst->value.asDate.lDate < pNext->value.asDate.lDate ||
               (pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                pFirst->value.asDate.lTime <= pNext->value.asDate.lTime))
                  ? pFirst
                  : pNext;
    }
    else if (pFirst->ExprType == HB_ET_LOGICAL)
    {
      pExpr = !pFirst->value.asLogical ? pFirst : pNext;
    }
  }
  else if (pFirst->ExprType == HB_ET_DATE && pNext->ExprType == HB_ET_TIMESTAMP)
  {
    pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ? pFirst : pNext;
  }
  else if (pFirst->ExprType == HB_ET_TIMESTAMP && pNext->ExprType == HB_ET_DATE)
  {
    pExpr = pFirst->value.asDate.lDate < pNext->value.asDate.lDate ? pFirst : pNext;
  }

  if (pExpr)
  {
    PHB_EXPR *pExprPtr = &pParms->value.asList.pExprList;

    while (*pExprPtr)
    {
      if (*pExprPtr == pExpr)
      {
        *pExprPtr = pExpr->pNext;
        break;
      }
      pExprPtr = &(*pExprPtr)->pNext;
    }
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceMAX(PHB_EXPR pSelf, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pFirst = pParms->value.asList.pExprList;
  PHB_EXPR pNext = pFirst->pNext;
  PHB_EXPR pExpr = nullptr;

  if (pFirst->ExprType == pNext->ExprType)
  {

    if (pFirst->ExprType == HB_ET_NUMERIC)
    {
      HB_BYTE bType = (pFirst->value.asNum.NumType & pNext->value.asNum.NumType);

      switch (bType)
      {
      case HB_ET_LONG:
        pExpr = pFirst->value.asNum.val.l >= pNext->value.asNum.val.l ? pFirst : pNext;
        break;

      case HB_ET_DOUBLE:
        pExpr = pFirst->value.asNum.val.d >= pNext->value.asNum.val.d ? pFirst : pNext;
        break;

      default:
        if (pFirst->value.asNum.NumType == HB_ET_DOUBLE)
        {
          pExpr = (pFirst->value.asNum.val.d >= static_cast<double>(pNext->value.asNum.val.l)) ? pFirst : pNext;
        }
        else
        {
          pExpr = (static_cast<double>(pFirst->value.asNum.val.l) >= pNext->value.asNum.val.d) ? pFirst : pNext;
        }
      }
    }
    else if (pFirst->ExprType == HB_ET_DATE)
    {
      pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ? pFirst : pNext;
    }
    else if (pFirst->ExprType == HB_ET_TIMESTAMP)
    {
      pExpr = (pFirst->value.asDate.lDate > pNext->value.asDate.lDate ||
               (pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                pFirst->value.asDate.lTime >= pNext->value.asDate.lTime))
                  ? pFirst
                  : pNext;
    }
    else if (pFirst->ExprType == HB_ET_LOGICAL)
    {
      pExpr = pFirst->value.asLogical ? pFirst : pNext;
    }
  }
  else if (pFirst->ExprType == HB_ET_DATE && pNext->ExprType == HB_ET_TIMESTAMP)
  {
    pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ? pFirst : pNext;
  }
  else if (pFirst->ExprType == HB_ET_TIMESTAMP && pNext->ExprType == HB_ET_DATE)
  {
    pExpr = pFirst->value.asDate.lDate > pNext->value.asDate.lDate ? pFirst : pNext;
  }

  if (pExpr)
  {
    PHB_EXPR *pExprPtr = &pParms->value.asList.pExprList;

    while (*pExprPtr)
    {
      if (*pExprPtr == pExpr)
      {
        *pExprPtr = pExpr->pNext;
        break;
      }
      pExprPtr = &(*pExprPtr)->pNext;
    }
    HB_COMP_EXPR_FREE(pParms);
    HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
    memcpy(pSelf, pExpr, sizeof(HB_EXPR));
    HB_COMP_EXPR_CLEAR(pExpr);
    return true;
  }

  return false;
}

HB_BOOL hb_compExprReduceBitFunc(PHB_EXPR pSelf, HB_MAXINT nResult, HB_BOOL fBool, HB_COMP_DECL)
{
  PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
  PHB_EXPR pExpr =
      fBool ? hb_compExprNewLogical(nResult != 0, HB_COMP_PARAM) : hb_compExprNewLong(nResult, HB_COMP_PARAM);

  HB_COMP_EXPR_FREE(pParms);
  HB_COMP_EXPR_FREE(pSelf->value.asFunCall.pFunName);
  memcpy(pSelf, pExpr, sizeof(HB_EXPR));
  HB_COMP_EXPR_CLEAR(pExpr);
  return true;
}
