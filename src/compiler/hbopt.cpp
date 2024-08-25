/*
 * Compiler PCODE optimizer
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#include "hbcomp.hpp"
#include "hbassert.hpp"

#define HB_OPT_FUNC(func) HB_PCODE_FUNC(func, void *)
typedef HB_OPT_FUNC(HB_OPT_FUNC_);
using PHB_OPT_FUNC = HB_OPT_FUNC_ *;

static HB_OPT_FUNC(hb_p_poplocal)
{
  HB_BYTE *pVar = &pFunc->pCode[nPCodePos + 1];
  HB_SHORT iVar = HB_PCODE_MKSHORT(pVar);

  HB_SYMBOL_UNUSED(cargo);

  if (HB_LIM_INT8(iVar))
  {
    pFunc->pCode[nPCodePos] = HB_P_POPLOCALNEAR;
    hb_compNOOPfill(pFunc, nPCodePos + 2, 1, false, false);
  }

  return 3;
}

static HB_OPT_FUNC(hb_p_pushlocal)
{
  HB_BYTE *pVar = &pFunc->pCode[nPCodePos + 1];
  HB_SHORT iVar = HB_PCODE_MKSHORT(pVar);

  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 3] == HB_P_POPLOCAL && HB_PCODE_MKSHORT(&pFunc->pCode[nPCodePos + 4]) == iVar &&
      !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 6, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 3] == HB_P_POPLOCALNEAR &&
           static_cast<HB_SCHAR>(pFunc->pCode[nPCodePos + 4]) == iVar && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 5, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 3] == HB_P_POP && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
  }
  else if (HB_LIM_INT8(iVar))
  {
    pFunc->pCode[nPCodePos] = HB_P_PUSHLOCALNEAR;
    hb_compNOOPfill(pFunc, nPCodePos + 2, 1, false, false);
  }

  return 3;
}

static HB_OPT_FUNC(hb_p_pushlocalnear)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 2] == HB_P_POPLOCAL &&
      static_cast<HB_SCHAR>(pFunc->pCode[nPCodePos + 1]) == HB_PCODE_MKSHORT(&pFunc->pCode[nPCodePos + 3]) &&
      !hb_compHasJump(pFunc, nPCodePos + 2))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 5, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 2] == HB_P_POPLOCALNEAR &&
           pFunc->pCode[nPCodePos + 1] == pFunc->pCode[nPCodePos + 3] && !hb_compHasJump(pFunc, nPCodePos + 2))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 2] == HB_P_POP && !hb_compHasJump(pFunc, nPCodePos + 2))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 3, false, false);
  }

  return 2;
}

static HB_OPT_FUNC(hb_p_localaddint)
{
  HB_BYTE *pVar = &pFunc->pCode[nPCodePos + 1];
  HB_SHORT iVar = HB_PCODE_MKSHORT(pVar);

  HB_SYMBOL_UNUSED(cargo);

  if (HB_LIM_INT8(iVar))
  {
    pVar[0] = HB_P_LOCALNEARADDINT;
    pVar[1] = HB_LOBYTE(iVar);
    hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
  }

  return 5;
}

static HB_OPT_FUNC(hb_p_pushstatic)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 3] == HB_P_POPSTATIC &&
      HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 1]) == HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 4]) &&
      !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 6, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 3] == HB_P_POP && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
  }

  return 3;
}

static HB_OPT_FUNC(hb_p_pushmemvar)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 3] == HB_P_POPMEMVAR &&
      HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 1]) == HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 4]) &&
      !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 6, false, false);
  }
  else if (pFunc->pCode[nPCodePos + 3] == HB_P_POP && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
  }

  return 3;
}

static HB_OPT_FUNC(hb_p_pushnil)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 1] == HB_P_POP && !hb_compHasJump(pFunc, nPCodePos + 1))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 2, false, false);
  }

  return 1;
}

static HB_OPT_FUNC(hb_p_false)
{
  HB_SYMBOL_UNUSED(cargo);

  switch (pFunc->pCode[nPCodePos + 1])
  {
  case HB_P_POP:
  case HB_P_NOT:
  case HB_P_JUMPFALSENEAR:
  case HB_P_JUMPFALSE:
  case HB_P_JUMPFALSEFAR:
  case HB_P_JUMPTRUENEAR:
  case HB_P_JUMPTRUE:
  case HB_P_JUMPTRUEFAR:
    if (!hb_compHasJump(pFunc, nPCodePos + 1))
    {
      int iCount = 1;

      switch (pFunc->pCode[nPCodePos + 1])
      {
      case HB_P_JUMPFALSENEAR:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMPNEAR;
        break;
      case HB_P_JUMPFALSE:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMP;
        break;
      case HB_P_JUMPFALSEFAR:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMPFAR;
        break;
      case HB_P_NOT:
        pFunc->pCode[nPCodePos + 1] = HB_P_TRUE;
        break;
      case HB_P_POP:
        iCount = 2;
        break;
      case HB_P_JUMPTRUENEAR:
        iCount = 3;
        break;
      case HB_P_JUMPTRUE:
        iCount = 4;
        break;
      case HB_P_JUMPTRUEFAR:
        iCount = 5;
        break;
      }
      hb_compNOOPfill(pFunc, nPCodePos, iCount, false, false);
    }
    break;
  }
  return 1;
}

static HB_OPT_FUNC(hb_p_true)
{
  HB_SYMBOL_UNUSED(cargo);

  switch (pFunc->pCode[nPCodePos + 1])
  {
  case HB_P_POP:
  case HB_P_NOT:
  case HB_P_JUMPTRUENEAR:
  case HB_P_JUMPTRUE:
  case HB_P_JUMPTRUEFAR:
  case HB_P_JUMPFALSENEAR:
  case HB_P_JUMPFALSE:
  case HB_P_JUMPFALSEFAR:
    if (!hb_compHasJump(pFunc, nPCodePos + 1))
    {
      int iCount = 1;

      switch (pFunc->pCode[nPCodePos + 1])
      {
      case HB_P_JUMPTRUENEAR:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMPNEAR;
        break;
      case HB_P_JUMPTRUE:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMP;
        break;
      case HB_P_JUMPTRUEFAR:
        pFunc->pCode[nPCodePos + 1] = HB_P_JUMPFAR;
        break;
      case HB_P_NOT:
        pFunc->pCode[nPCodePos + 1] = HB_P_FALSE;
        break;
      case HB_P_POP:
        iCount = 2;
        break;
      case HB_P_JUMPFALSENEAR:
        iCount = 3;
        break;
      case HB_P_JUMPFALSE:
        iCount = 4;
        break;
      case HB_P_JUMPFALSEFAR:
        iCount = 5;
        break;
      }
      hb_compNOOPfill(pFunc, nPCodePos, iCount, false, false);
    }
    break;
  }
  return 1;
}

static HB_OPT_FUNC(hb_p_duplicate)
{
  HB_SYMBOL_UNUSED(cargo);

  switch (pFunc->pCode[nPCodePos + 1])
  {
  case HB_P_JUMPTRUEFAR:
  case HB_P_JUMPFALSEFAR:
    if (pFunc->pCode[nPCodePos + 5] == HB_P_POP)
    {
      HB_BYTE *pAddr = &pFunc->pCode[nPCodePos + 2];
      HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr), nLastOffset = 0;
      HB_SIZE nNewPos = nPCodePos + 1 + nOffset;
      auto fNot = false;
      auto fOK = true;
      auto fRepeat = true;

      do
      {
        if (pFunc->pCode[nNewPos] == HB_P_DUPLICATE)
        {
          if (nOffset > 0)
          {
            hb_p_duplicate(pFunc, nNewPos, nullptr);
          }
        }

        if (pFunc->pCode[nNewPos] == HB_P_NOOP)
        {
          nNewPos++;
          nOffset++;
        }
        else if (pFunc->pCode[nNewPos] == HB_P_NOT)
        {
          nNewPos++;
          nOffset++;
          fNot = !fNot;
          fOK = !fOK;
        }
        else if (pFunc->pCode[nNewPos] == HB_P_DUPLICATE &&
                 (pFunc->pCode[nNewPos + 1] == HB_P_JUMPTRUEFAR || pFunc->pCode[nNewPos + 1] == HB_P_JUMPFALSEFAR))
        {
          HB_ISIZ nJump;
          if (pFunc->pCode[nNewPos + 1] != pFunc->pCode[nPCodePos + 1])
          {
            fNot = !fNot;
          }
          nJump = fNot ? 4 : HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 2]);
          nOffset += nJump + 1;
          nNewPos = nPCodePos + 1 + nOffset;
          fRepeat = nJump > 0;
        }
        else
        {
          fRepeat = false;
          if (pFunc->pCode[nNewPos] == HB_P_POP)
          {
            fOK = true;
          }
        }

        if (fOK)
        {
          nLastOffset = nOffset;
        }
      } while (fRepeat);

      if ((pFunc->pCode[nNewPos] == HB_P_JUMPTRUEFAR || pFunc->pCode[nNewPos] == HB_P_JUMPFALSEFAR) &&
          !hb_compHasJump(pFunc, nPCodePos + 1) && !hb_compHasJump(pFunc, nPCodePos + 5))
      {
        if (pFunc->pCode[nNewPos] != pFunc->pCode[nPCodePos + 1])
        {
          fNot = !fNot;
        }
        if (fNot)
        {
          nOffset += 4;
        }
        else
        {
          nOffset += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
        }

        HB_PUT_LE_UINT24(pAddr, nOffset);
        hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
        hb_compNOOPfill(pFunc, nPCodePos + 5, 1, false, false);
      }
      else if (nLastOffset)
      {
        HB_PUT_LE_UINT24(pAddr, nLastOffset);
      }
    }
    break;
  }
  return 1;
}

static HB_OPT_FUNC(hb_p_not)
{
  HB_BYTE opcode;

  HB_SYMBOL_UNUSED(cargo);

  switch (pFunc->pCode[nPCodePos + 1])
  {
  case HB_P_NOT:
    opcode = HB_P_NOOP;
    break;
  case HB_P_JUMPTRUENEAR:
    opcode = HB_P_JUMPFALSENEAR;
    break;
  case HB_P_JUMPTRUE:
    opcode = HB_P_JUMPFALSE;
    break;
  case HB_P_JUMPTRUEFAR:
    opcode = HB_P_JUMPFALSEFAR;
    break;
  case HB_P_JUMPFALSENEAR:
    opcode = HB_P_JUMPTRUENEAR;
    break;
  case HB_P_JUMPFALSE:
    opcode = HB_P_JUMPTRUE;
    break;
  case HB_P_JUMPFALSEFAR:
    opcode = HB_P_JUMPTRUEFAR;
    break;
/* This optimization will be enabled in the future in a little bit differ form */
#if 0
      case HB_P_DUPLICATE:
         if( (pFunc->pCode[nPCodePos + 2] == HB_P_JUMPTRUEFAR || pFunc->pCode[nPCodePos + 2] == HB_P_JUMPFALSEFAR ) && pFunc->pCode[nPCodePos + 6] == HB_P_POP ) {
            HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 3];
            HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr);

            if( nOffset > 0 ) {
               hb_p_duplicate(pFunc, nPCodePos + 1, nullptr);
               nOffset = HB_PCODE_MKINT24(pAddr);
            }

            if( (pFunc->pCode[nPCodePos + 1] == HB_P_NOT ||
                 (pFunc->pCode[nPCodePos + 1] == HB_P_DUPLICATE && pFunc->pCode[nPCodePos + nOffset + 2] == HB_P_NOT)) &&
                !hb_compHasJump(pFunc, nPCodePos + 1) ) {
               hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
               if( pFunc->pCode[nPCodePos + 2] == HB_P_JUMPTRUEFAR ) {
                  pFunc->pCode[nPCodePos + 2] = HB_P_JUMPFALSEFAR;
               } else {
                  pFunc->pCode[nPCodePos + 2] = HB_P_JUMPTRUEFAR;
               }
               if( pFunc->pCode[nPCodePos + 1] == HB_P_DUPLICATE ) {
                  ++nOffset;
                  HB_PUT_LE_UINT24(pAddr, nOffset);
               }
            }
         }
         /* fallthrough */
#endif
  default:
    opcode = HB_P_LAST_PCODE;
    break;
  }

  if (opcode < HB_P_LAST_PCODE && !hb_compHasJump(pFunc, nPCodePos + 1))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
    if (opcode == HB_P_NOOP)
    {
      hb_compNOOPfill(pFunc, nPCodePos + 1, 1, false, false);
    }
    else
    {
      pFunc->pCode[nPCodePos + 1] = opcode;
    }
  }
  return 1;
}

static HB_OPT_FUNC(hb_p_jumpfar)
{
  HB_BYTE *pAddr = &pFunc->pCode[nPCodePos + 1];
  HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr);
  HB_SIZE nNewPos = nPCodePos + nOffset;

  HB_SYMBOL_UNUSED(cargo);

  if (nOffset == 4)
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
  }
  else
  {
    auto fLine = false;

    if (pFunc->pCode[nNewPos] == HB_P_LINE)
    {
      fLine = true;
      nNewPos += 3;
      nOffset += 3;
    }

    switch (pFunc->pCode[nNewPos])
    {
    case HB_P_JUMPFAR:
      nOffset += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
      if (!fLine || pFunc->pCode[nPCodePos + nOffset] == HB_P_LINE)
      {
        HB_PUT_LE_UINT24(pAddr, nOffset);
      }
      break;

    case HB_P_JUMPFALSEFAR:
      nNewPos += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
      if (nNewPos == nPCodePos + 4 &&
          (!fLine || (pFunc->pCode[nNewPos] == HB_P_LINE && pFunc->pCode[nPCodePos + nOffset + 4] == HB_P_LINE)))
      {
        pFunc->pCode[nPCodePos] = HB_P_JUMPTRUEFAR;
        HB_PUT_LE_UINT24(pAddr, nOffset + 4);
      }
      break;

    case HB_P_JUMPTRUEFAR:
      nNewPos += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
      if (nNewPos == nPCodePos + 4 &&
          (!fLine || (pFunc->pCode[nNewPos] == HB_P_LINE && pFunc->pCode[nPCodePos + nOffset + 4] == HB_P_LINE)))
      {
        pFunc->pCode[nPCodePos] = HB_P_JUMPFALSEFAR;
        HB_PUT_LE_UINT24(pAddr, nOffset + 4);
      }
      break;
    }
  }
  return 4;
}

static HB_OPT_FUNC(hb_p_jumpfalsefar)
{
  HB_BYTE *pAddr = &pFunc->pCode[nPCodePos + 1];
  HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr);
  HB_SIZE nNewPos = nPCodePos + nOffset;
  auto fLine = false;

  HB_SYMBOL_UNUSED(cargo);

  if (nOffset == 8 && pFunc->pCode[nPCodePos + 4] == HB_P_JUMPFAR && !hb_compHasJump(pFunc, nPCodePos + 4))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
    pFunc->pCode[nPCodePos + 4] = HB_P_JUMPTRUEFAR;
  }
  else if (nOffset == 11 && pFunc->pCode[nPCodePos + 4] == HB_P_LINE && pFunc->pCode[nPCodePos + 11] == HB_P_LINE &&
           pFunc->pCode[nPCodePos + 7] == HB_P_JUMPFAR &&
           pFunc->pCode[nPCodePos + 7 + HB_PCODE_MKINT24(&pFunc->pCode[nPCodePos + 8])] == HB_P_LINE &&
           !hb_compHasJump(pFunc, nPCodePos + 4) && !hb_compHasJump(pFunc, nPCodePos + 7))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 7, false, false);
    pFunc->pCode[nPCodePos + 7] = HB_P_JUMPTRUEFAR;
  }
  else
  {
    if (pFunc->pCode[nNewPos] == HB_P_LINE)
    {
      fLine = true;
      nNewPos += 3;
      nOffset += 3;
    }
    if (pFunc->pCode[nNewPos] == HB_P_JUMPFAR)
    {
      nOffset += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
      if (!fLine || pFunc->pCode[nPCodePos + nOffset] == HB_P_LINE)
      {
        HB_PUT_LE_UINT24(pAddr, nOffset);
      }
    }
  }
  return 4;
}

static HB_OPT_FUNC(hb_p_jumptruefar)
{
  HB_BYTE *pAddr = &pFunc->pCode[nPCodePos + 1];
  HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr);
  HB_SIZE nNewPos = nPCodePos + nOffset;
  auto fLine = false;

  HB_SYMBOL_UNUSED(cargo);

  if (nOffset == 8 && pFunc->pCode[nPCodePos + 4] == HB_P_JUMPFAR && !hb_compHasJump(pFunc, nPCodePos + 4))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 4, false, false);
    pFunc->pCode[nPCodePos + 4] = HB_P_JUMPFALSEFAR;
  }
  else if (nOffset == 11 && pFunc->pCode[nPCodePos + 4] == HB_P_LINE && pFunc->pCode[nPCodePos + 11] == HB_P_LINE &&
           pFunc->pCode[nPCodePos + 7] == HB_P_JUMPFAR &&
           pFunc->pCode[nPCodePos + 7 + HB_PCODE_MKINT24(&pFunc->pCode[nPCodePos + 8])] == HB_P_LINE &&
           !hb_compHasJump(pFunc, nPCodePos + 4) && !hb_compHasJump(pFunc, nPCodePos + 7))
  {
    hb_compNOOPfill(pFunc, nPCodePos, 7, false, false);
    pFunc->pCode[nPCodePos + 7] = HB_P_JUMPFALSEFAR;
  }
  else
  {
    if (pFunc->pCode[nNewPos] == HB_P_LINE)
    {
      fLine = true;
      nNewPos += 3;
      nOffset += 3;
    }
    if (pFunc->pCode[nNewPos] == HB_P_JUMPFAR)
    {
      nOffset += HB_PCODE_MKINT24(&pFunc->pCode[nNewPos + 1]);
      if (!fLine || pFunc->pCode[nPCodePos + nOffset] == HB_P_LINE)
      {
        HB_PUT_LE_UINT24(pAddr, nOffset);
      }
    }
  }
  return 4;
}

static HB_OPT_FUNC(hb_p_switch)
{
  HB_USHORT usCases = HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 1]);
  HB_SIZE nStart = nPCodePos;

  HB_SYMBOL_UNUSED(cargo);

  nPCodePos += 3;
  for (HB_USHORT us = 0; us < usCases; ++us)
  {
    switch (pFunc->pCode[nPCodePos])
    {
    case HB_P_PUSHBYTE:
      nPCodePos += 2;
      break;
    case HB_P_PUSHINT:
      nPCodePos += 3;
      break;
    case HB_P_PUSHLONG:
    case HB_P_PUSHDATE:
      nPCodePos += 5;
      break;
    case HB_P_PUSHLONGLONG:
      nPCodePos += 9;
      break;
    case HB_P_PUSHSTRSHORT:
      nPCodePos += 2 + pFunc->pCode[nPCodePos + 1];
      break;
    case HB_P_PUSHSTR:
      nPCodePos += 3 + HB_PCODE_MKUSHORT(&pFunc->pCode[nPCodePos + 1]);
      break;
    case HB_P_PUSHSTRLARGE:
      nPCodePos += 4 + HB_PCODE_MKUINT24(&pFunc->pCode[nPCodePos + 1]);
      break;
    case HB_P_PUSHNIL:
      /* default clause */
      us = usCases;
      nPCodePos++;
      break;
    }
    switch (pFunc->pCode[nPCodePos])
    {
    case HB_P_JUMPNEAR:
      nPCodePos += 2;
      break;
    case HB_P_JUMP:
      nPCodePos += 3;
      break;
    /*case HB_P_JUMPFAR:*/
    default:
      nPCodePos += 4;
      break;
    }
  }

  return nPCodePos - nStart;
}

static HB_OPT_FUNC(hb_p_function)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 3] == HB_P_RETVALUE && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    pFunc->pCode[nPCodePos] = HB_P_DO;
    hb_compNOOPfill(pFunc, nPCodePos + 3, 1, false, false);
  }
  return 3;
}

static HB_OPT_FUNC(hb_p_functionshort)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 2] == HB_P_RETVALUE && !hb_compHasJump(pFunc, nPCodePos + 2))
  {
    pFunc->pCode[nPCodePos] = HB_P_DOSHORT;
    hb_compNOOPfill(pFunc, nPCodePos + 2, 1, false, false);
  }
  return 2;
}

static HB_OPT_FUNC(hb_p_macrofunc)
{
  HB_SYMBOL_UNUSED(cargo);

  if (pFunc->pCode[nPCodePos + 3] == HB_P_RETVALUE && !hb_compHasJump(pFunc, nPCodePos + 3))
  {
    pFunc->pCode[nPCodePos] = HB_P_MACRODO;
    hb_compNOOPfill(pFunc, nPCodePos + 3, 1, false, false);
  }
  return 3;
}

static HB_OPT_FUNC(hb_p_endblock)
{
  HB_SYMBOL_UNUSED(cargo);

  if (nPCodePos + 1 < pFunc->nPCodePos && pFunc->pCode[nPCodePos + 1] == HB_P_ENDBLOCK)
  {
    hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
  }
  return 1;
}

static HB_OPT_FUNC(hb_p_endproc)
{
  HB_SYMBOL_UNUSED(cargo);

  if (nPCodePos + 1 < pFunc->nPCodePos && pFunc->pCode[nPCodePos + 1] == HB_P_ENDPROC)
  {
    hb_compNOOPfill(pFunc, nPCodePos, 1, false, false);
  }
  return 1;
}

/* NOTE: The  order of functions have to match the order of opcodes mnemonics
 */
// clang-format off
static const PHB_OPT_FUNC s_opt_table[] =
{
   nullptr,                    /* HB_P_AND                   */
   nullptr,                    /* HB_P_ARRAYPUSH             */
   nullptr,                    /* HB_P_ARRAYPOP              */
   nullptr,                    /* HB_P_ARRAYDIM              */
   nullptr,                    /* HB_P_ARRAYGEN              */
   nullptr,                    /* HB_P_EQUAL                 */
   hb_p_endblock,              /* HB_P_ENDBLOCK              */
   hb_p_endproc,               /* HB_P_ENDPROC               */
   nullptr,                    /* HB_P_EXACTLYEQUAL          */
   hb_p_false,                 /* HB_P_FALSE                 */
   nullptr,                    /* HB_P_FORTEST               */
   hb_p_function,              /* HB_P_FUNCTION              */
   hb_p_functionshort,         /* HB_P_FUNCTIONSHORT         */
   nullptr,                    /* HB_P_FRAME                 */
   nullptr,                    /* HB_P_FUNCPTR               */
   nullptr,                    /* HB_P_GREATER               */
   nullptr,                    /* HB_P_GREATEREQUAL          */
   nullptr,                    /* HB_P_DEC                   */
   nullptr,                    /* HB_P_DIVIDE                */
   nullptr,                    /* HB_P_DO                    */
   nullptr,                    /* HB_P_DOSHORT               */
   hb_p_duplicate,             /* HB_P_DUPLICATE             */
   nullptr,                    /* HB_P_PUSHTIMESTAMP         */
   nullptr,                    /* HB_P_INC                   */
   nullptr,                    /* HB_P_INSTRING              */
   nullptr,                    /* HB_P_JUMPNEAR              */
   nullptr,                    /* HB_P_JUMP                  */
   hb_p_jumpfar,               /* HB_P_JUMPFAR               */
   nullptr,                    /* HB_P_JUMPFALSENEAR         */
   nullptr,                    /* HB_P_JUMPFALSE             */
   hb_p_jumpfalsefar,          /* HB_P_JUMPFALSEFAR          */
   nullptr,                    /* HB_P_JUMPTRUENEAR          */
   nullptr,                    /* HB_P_JUMPTRUE              */
   hb_p_jumptruefar,           /* HB_P_JUMPTRUEFAR           */
   nullptr,                    /* HB_P_LESSEQUAL             */
   nullptr,                    /* HB_P_LESS                  */
   nullptr,                    /* HB_P_LINE                  */
   nullptr,                    /* HB_P_LOCALNAME             */
   nullptr,                    /* HB_P_MACROPOP              */
   nullptr,                    /* HB_P_MACROPOPALIASED       */
   nullptr,                    /* HB_P_MACROPUSH             */
   nullptr,                    /* HB_P_MACROARRAYGEN         */
   nullptr,                    /* HB_P_MACROPUSHLIST         */
   nullptr,                    /* HB_P_MACROPUSHINDEX        */
   nullptr,                    /* HB_P_MACROPUSHPARE         */
   nullptr,                    /* HB_P_MACROPUSHALIASED      */
   nullptr,                    /* HB_P_MACROSYMBOL           */
   nullptr,                    /* HB_P_MACROTEXT             */
   nullptr,                    /* HB_P_MESSAGE               */
   nullptr,                    /* HB_P_MINUS                 */
   nullptr,                    /* HB_P_MODULUS               */
   nullptr,                    /* HB_P_MODULENAME            */
                               /* start: pcodes generated by macro compiler */
   nullptr,                    /* HB_P_MMESSAGE              */
   nullptr,                    /* HB_P_MPOPALIASEDFIELD      */
   nullptr,                    /* HB_P_MPOPALIASEDVAR        */
   nullptr,                    /* HB_P_MPOPFIELD             */
   nullptr,                    /* HB_P_MPOPMEMVAR            */
   nullptr,                    /* HB_P_MPUSHALIASEDFIELD     */
   nullptr,                    /* HB_P_MPUSHALIASEDVAR       */
   nullptr,                    /* HB_P_MPUSHBLOCK            */
   nullptr,                    /* HB_P_MPUSHFIELD            */
   nullptr,                    /* HB_P_MPUSHMEMVAR           */
   nullptr,                    /* HB_P_MPUSHMEMVARREF        */
   nullptr,                    /* HB_P_MPUSHSYM              */
   nullptr,                    /* HB_P_MPUSHVARIABLE         */
                               /* end: */
   nullptr,                    /* HB_P_MULT                  */
   nullptr,                    /* HB_P_NEGATE                */
   nullptr,                    /* HB_P_NOOP                  */
   hb_p_not,                   /* HB_P_NOT                   */
   nullptr,                    /* HB_P_NOTEQUAL              */
   nullptr,                    /* HB_P_OR                    */
   nullptr,                    /* HB_P_PARAMETER             */
   nullptr,                    /* HB_P_PLUS                  */
   nullptr,                    /* HB_P_POP                   */
   nullptr,                    /* HB_P_POPALIAS              */
   nullptr,                    /* HB_P_POPALIASEDFIELD       */
   nullptr,                    /* HB_P_POPALIASEDFIELDNEAR   */
   nullptr,                    /* HB_P_POPALIASEDVAR         */
   nullptr,                    /* HB_P_POPFIELD              */
   hb_p_poplocal,              /* HB_P_POPLOCAL              */
   nullptr,                    /* HB_P_POPLOCALNEAR          */
   nullptr,                    /* HB_P_POPMEMVAR             */
   nullptr,                    /* HB_P_POPSTATIC             */
   nullptr,                    /* HB_P_POPVARIABLE           */
   nullptr,                    /* HB_P_POWER                 */
   nullptr,                    /* HB_P_PUSHALIAS             */
   nullptr,                    /* HB_P_PUSHALIASEDFIELD      */
   nullptr,                    /* HB_P_PUSHALIASEDFIELDNEAR  */
   nullptr,                    /* HB_P_PUSHALIASEDVAR        */
   nullptr,                    /* HB_P_PUSHBLOCK             */
   nullptr,                    /* HB_P_PUSHBLOCKSHORT        */
   nullptr,                    /* HB_P_PUSHFIELD             */
   nullptr,                    /* HB_P_PUSHBYTE              */
   nullptr,                    /* HB_P_PUSHINT               */
   hb_p_pushlocal,             /* HB_P_PUSHLOCAL             */
   hb_p_pushlocalnear,         /* HB_P_PUSHLOCALNEAR         */
   nullptr,                    /* HB_P_PUSHLOCALREF          */
   nullptr,                    /* HB_P_PUSHLONG              */
   hb_p_pushmemvar,            /* HB_P_PUSHMEMVAR            */
   nullptr,                    /* HB_P_PUSHMEMVARREF         */
   hb_p_pushnil,               /* HB_P_PUSHNIL               */
   nullptr,                    /* HB_P_PUSHDOUBLE            */
   nullptr,                    /* HB_P_PUSHSELF              */
   hb_p_pushstatic,            /* HB_P_PUSHSTATIC            */
   nullptr,                    /* HB_P_PUSHSTATICREF         */
   nullptr,                    /* HB_P_PUSHSTR               */
   nullptr,                    /* HB_P_PUSHSTRSHORT          */
   nullptr,                    /* HB_P_PUSHSYM               */
   nullptr,                    /* HB_P_PUSHSYMNEAR           */
   nullptr,                    /* HB_P_PUSHVARIABLE          */
   nullptr,                    /* HB_P_RETVALUE              */
   nullptr,                    /* HB_P_SEND                  */
   nullptr,                    /* HB_P_SENDSHORT             */
   nullptr,                    /* HB_P_SEQBEGIN              */
   nullptr,                    /* HB_P_SEQEND                */
   nullptr,                    /* HB_P_SEQRECOVER            */
   nullptr,                    /* HB_P_SFRAME                */
   nullptr,                    /* HB_P_STATICS               */
   nullptr,                    /* HB_P_STATICNAME            */
   nullptr,                    /* HB_P_SWAPALIAS             */
   hb_p_true,                  /* HB_P_TRUE                  */
   nullptr,                    /* HB_P_ZERO                  */
   nullptr,                    /* HB_P_ONE                   */
   hb_p_macrofunc,             /* HB_P_MACROFUNC             */
   nullptr,                    /* HB_P_MACRODO               */
   nullptr,                    /* HB_P_MPUSHSTR              */
   nullptr,                    /* HB_P_LOCALNEARADDINT       */
   nullptr,                    /* HB_P_MACROPUSHREF          */
   nullptr,                    /* HB_P_PUSHLONGLONG          */
   nullptr,                    /* HB_P_ENUMSTART             */
   nullptr,                    /* HB_P_ENUMNEXT              */
   nullptr,                    /* HB_P_ENUMPREV              */
   nullptr,                    /* HB_P_ENUMEND               */
   hb_p_switch,                /* HB_P_SWITCH                */
   nullptr,                    /* HB_P_PUSHDATE              */
   nullptr,                    /* HB_P_PLUSEQPOP             */
   nullptr,                    /* HB_P_MINUSEQPOP            */
   nullptr,                    /* HB_P_MULTEQPOP             */
   nullptr,                    /* HB_P_DIVEQPOP              */
   nullptr,                    /* HB_P_PLUSEQ                */
   nullptr,                    /* HB_P_MINUSEQ               */
   nullptr,                    /* HB_P_MULTEQ                */
   nullptr,                    /* HB_P_DIVEQ                 */
   nullptr,                    /* HB_P_WITHOBJECTSTART       */
   nullptr,                    /* HB_P_WITHOBJECTMESSAGE     */
   nullptr,                    /* HB_P_WITHOBJECTEND         */
   nullptr,                    /* HB_P_MACROSEND             */
   nullptr,                    /* HB_P_PUSHOVARREF           */
   nullptr,                    /* HB_P_ARRAYPUSHREF          */
   nullptr,                    /* HB_P_VFRAME                */
   nullptr,                    /* HB_P_LARGEFRAME            */
   nullptr,                    /* HB_P_LARGEVFRAME           */
   nullptr,                    /* HB_P_PUSHSTRHIDDEN         */
   hb_p_localaddint,           /* HB_P_LOCALADDINT           */
   nullptr,                    /* HB_P_MODEQPOP              */
   nullptr,                    /* HB_P_EXPEQPOP              */
   nullptr,                    /* HB_P_MODEQ                 */
   nullptr,                    /* HB_P_EXPEQ                 */
   nullptr,                    /* HB_P_DUPLUNREF             */
   nullptr,                    /* HB_P_MPUSHBLOCKLARGE       */
   nullptr,                    /* HB_P_MPUSHSTRLARGE         */
   nullptr,                    /* HB_P_PUSHBLOCKLARGE        */
   nullptr,                    /* HB_P_PUSHSTRLARGE          */
   nullptr,                    /* HB_P_SWAP                  */
   nullptr,                    /* HB_P_PUSHVPARAMS           */
   nullptr,                    /* HB_P_PUSHUNREF             */
   nullptr,                    /* HB_P_SEQALWAYS             */
   nullptr,                    /* HB_P_ALWAYSBEGIN           */
   nullptr,                    /* HB_P_ALWAYSEND             */
   nullptr,                    /* HB_P_DECEQPOP              */
   nullptr,                    /* HB_P_INCEQPOP              */
   nullptr,                    /* HB_P_DECEQ                 */
   nullptr,                    /* HB_P_INCEQ                 */
   nullptr,                    /* HB_P_LOCALDEC              */
   nullptr,                    /* HB_P_LOCALINC              */
   nullptr,                    /* HB_P_LOCALINCPUSH          */
   nullptr,                    /* HB_P_PUSHFUNCSYM           */
   nullptr,                    /* HB_P_HASHGEN               */
   nullptr,                    /* HB_P_SEQBLOCK              */
   nullptr,                    /* HB_P_THREADSTATICS         */
   nullptr                     /* HB_P_PUSHAPARAMS           */
};
// clang-format on

void hb_compOptimizePCode(HB_COMP_DECL, PHB_HFUNC pFunc)
{
  const PHB_OPT_FUNC *pFuncTable = s_opt_table;

  HB_SYMBOL_UNUSED(HB_COMP_PARAM);

  assert(HB_P_LAST_PCODE == sizeof(s_opt_table) / sizeof(PHB_OPT_FUNC));

  hb_compPCodeEval(pFunc, static_cast<const PHB_PCODE_FUNC *>(pFuncTable), nullptr);
}

/*
 *   PCode trace optimizer
 */

#define OPT_LOCAL_FLAG_BLOCK 1
#define OPT_LOCAL_FLAG_PUSH 2
#define OPT_LOCAL_FLAG_POP 4
#define OPT_LOCAL_FLAG_PUSHREF 8
#define OPT_LOCAL_FLAG_POPSELF 16
#define OPT_LOCAL_FLAG_CHANGE 32

struct HB_OPT_LOCAL
{
  HB_SHORT isNumber;
  HB_BYTE bFlags;
};

using PHB_OPT_LOCAL = HB_OPT_LOCAL *;

static bool hb_compIsJump(HB_BYTE bPCode)
{
  return (bPCode >= HB_P_JUMPNEAR && bPCode <= HB_P_JUMPTRUEFAR) || /* All jumps */
         bPCode == HB_P_SEQBEGIN || bPCode == HB_P_SEQEND || bPCode == HB_P_SEQALWAYS || bPCode == HB_P_ALWAYSBEGIN;
}

static bool hb_compIsUncondJump(HB_BYTE bPCode)
{
  return bPCode == HB_P_JUMPNEAR || bPCode == HB_P_JUMP || bPCode == HB_P_JUMPFAR;
  /*     || bPCode == HB_P_SEQEND;
     BEGIN SEQUENCE/END SEQUENCE logic could not be processed using conditional/unconditional
     jumps. I set HB_P_SEQEND as conditional jump though this PCode instruction is processed
     as unconditional jump by Harbour VM. This hack solves 'Variable is assigned but not used'
     warning false positive in code:
       BEGIN SEQUENCE
          nI := 1
          Break(NIL)
       RECOVER
          ? nI
       END SEQUENCE
     [Mindaugas]
   */
}

#if 0
static HB_SHORT hb_compIsLocalOp(HB_BYTE bCode)
{
   return bCode == HB_P_POPLOCAL ||
          bCode == HB_P_POPLOCALNEAR ||
          bCode == HB_P_PUSHLOCAL ||
          bCode == HB_P_PUSHLOCALNEAR ||
          bCode == HB_P_PUSHLOCALREF ||
          bCode == HB_P_LOCALNEARADDINT ||
          bCode == HB_P_LOCALADDINT ||
          bCode == HB_P_LOCALDEC ||
          bCode == HB_P_LOCALINC ||
          bCode == HB_P_LOCALINCPUSH;
}
#endif

static HB_SHORT hb_compLocalGetNumber(HB_BYTE *pCode)
{
  switch (*pCode)
  {
  case HB_P_POPLOCALNEAR:
  case HB_P_PUSHLOCALNEAR:
  case HB_P_LOCALNEARADDINT:
    return *(reinterpret_cast<signed char *>(pCode) + 1);

  case HB_P_POPLOCAL:
  case HB_P_PUSHLOCAL:
  case HB_P_PUSHLOCALREF:
  case HB_P_LOCALDEC:
  case HB_P_LOCALINC:
  case HB_P_LOCALINCPUSH:
  case HB_P_LOCALADDINT:
    return HB_PCODE_MKSHORT(pCode + 1);
  }
  assert(0);
  return 0;
}

static HB_ISIZ hb_compJumpGetOffset(HB_BYTE *pCode)
{
  switch (*pCode)
  {
  case HB_P_JUMPNEAR:
  case HB_P_JUMPFALSENEAR:
  case HB_P_JUMPTRUENEAR:
    return *(reinterpret_cast<signed char *>(pCode) + 1);

  case HB_P_JUMP:
  case HB_P_JUMPFALSE:
  case HB_P_JUMPTRUE:
    return HB_PCODE_MKSHORT(pCode + 1);

  case HB_P_JUMPFAR:
  case HB_P_JUMPFALSEFAR:
  case HB_P_JUMPTRUEFAR:
  case HB_P_SEQBEGIN:
  case HB_P_SEQEND:
  case HB_P_SEQALWAYS:
  case HB_P_ALWAYSBEGIN:
    return HB_PCODE_MKINT24(pCode + 1);
  }
  assert(0);
  return 0;
}

static void hb_compPCodeEnumScanLocals(PHB_HFUNC pFunc, PHB_OPT_LOCAL pLocals)
{
  HB_SIZE nPos = 0, nLastPos = 0;
  HB_SHORT isVar = 0;
  auto fWasJump = false;

  while (nPos < pFunc->nPCodePos)
  {
    if (hb_compIsJump(pFunc->pCode[nPos]))
    {
      fWasJump = true;
    }

    switch (pFunc->pCode[nPos])
    {
    case HB_P_POPLOCALNEAR:
    case HB_P_PUSHLOCALNEAR:
    case HB_P_LOCALNEARADDINT:
      isVar = static_cast<signed char>(pFunc->pCode[nPos + 1]);
      break;

    case HB_P_LOCALNAME:
    case HB_P_POPLOCAL:
    case HB_P_PUSHLOCAL:
    case HB_P_PUSHLOCALREF:
    case HB_P_LOCALADDINT:
    case HB_P_LOCALDEC:
    case HB_P_LOCALINC:
    case HB_P_LOCALINCPUSH:
      isVar = HB_PCODE_MKSHORT(&pFunc->pCode[nPos + 1]);
      break;
    }

    switch (pFunc->pCode[nPos])
    {
    case HB_P_POPLOCALNEAR:
    case HB_P_POPLOCAL:
      if (isVar > 0)
      {
        if (nPos > 0 && pFunc->pCode[nLastPos] == HB_P_PUSHSELF && !hb_compHasJump(pFunc, nPos) && !fWasJump)
        {
          /* For real POPSELF support we need to do backward tree
             tracing. This is not implemented, but using fWasJump
             we can easy optimize Self := QSelf() at the beginning
             of functions. [Mindaugas]
           */
          pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_POPSELF;
        }
        else
        {
          pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_POP;
        }
      }
      break;

    case HB_P_PUSHLOCALNEAR:
    case HB_P_PUSHLOCAL:
      if (isVar > 0)
      {
        pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_PUSH;
      }
      break;

    case HB_P_PUSHLOCALREF:
      if (isVar > 0)
      {
        HB_SIZE nPosNext = nPos + hb_compPCodeSize(pFunc, nPos);
        HB_BYTE bCodeNext = pFunc->pCode[nPosNext];
        HB_BYTE bCodeNext2 = pFunc->pCode[nPosNext + hb_compPCodeSize(pFunc, nPosNext)];

        if ((bCodeNext == HB_P_PUSHTIMESTAMP || bCodeNext == HB_P_PUSHBLOCK || bCodeNext == HB_P_PUSHBLOCKSHORT ||
             bCodeNext == HB_P_PUSHFIELD || bCodeNext == HB_P_PUSHBYTE || bCodeNext == HB_P_PUSHINT ||
             bCodeNext == HB_P_PUSHLOCAL || bCodeNext == HB_P_PUSHLOCALNEAR || bCodeNext == HB_P_PUSHLONG ||
             bCodeNext == HB_P_PUSHMEMVAR || bCodeNext == HB_P_PUSHNIL || bCodeNext == HB_P_PUSHDOUBLE ||
             bCodeNext == HB_P_PUSHSELF || bCodeNext == HB_P_PUSHSTATIC || bCodeNext == HB_P_PUSHSTR ||
             bCodeNext == HB_P_PUSHSTRSHORT || bCodeNext == HB_P_PUSHVARIABLE || bCodeNext == HB_P_ONE ||
             bCodeNext == HB_P_ZERO || bCodeNext == HB_P_PUSHLONGLONG || bCodeNext == HB_P_PUSHDATE ||
             bCodeNext == HB_P_PUSHSTRHIDDEN || bCodeNext == HB_P_PUSHBLOCKLARGE || bCodeNext == HB_P_PUSHSTRLARGE ||
             bCodeNext == HB_P_LOCALINCPUSH) &&
            (bCodeNext2 == HB_P_PLUSEQPOP || bCodeNext2 == HB_P_MINUSEQPOP || bCodeNext2 == HB_P_MULTEQPOP ||
             bCodeNext2 == HB_P_DIVEQPOP || bCodeNext2 == HB_P_MODEQPOP || bCodeNext2 == HB_P_EXPEQPOP ||
             bCodeNext2 == HB_P_DECEQPOP || bCodeNext2 == HB_P_INCEQPOP || bCodeNext2 == HB_P_PLUSEQ ||
             bCodeNext2 == HB_P_MINUSEQ || bCodeNext2 == HB_P_MULTEQ || bCodeNext2 == HB_P_DIVEQ ||
             bCodeNext2 == HB_P_MODEQ || bCodeNext2 == HB_P_EXPEQ || bCodeNext2 == HB_P_DECEQ ||
             bCodeNext2 == HB_P_INCEQ))
        {
          pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_CHANGE;
        }
        else
        {
          pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_PUSHREF;
        }
      }
      break;

    case HB_P_LOCALADDINT:
    case HB_P_LOCALNEARADDINT:
    case HB_P_LOCALDEC:
    case HB_P_LOCALINC:
      if (isVar > 0)
      {
        pLocals[isVar - 1].bFlags |= OPT_LOCAL_FLAG_CHANGE;
      }
      break;

    case HB_P_LOCALINCPUSH:
      if (isVar > 0)
      {
        pLocals[isVar - 1].bFlags |= (OPT_LOCAL_FLAG_CHANGE | OPT_LOCAL_FLAG_PUSH);
      }
      break;

    case HB_P_PUSHBLOCK:
    case HB_P_PUSHBLOCKLARGE: {
      HB_BYTE *pCode = &pFunc->pCode[nPos + 5];
      HB_USHORT usVarCount;

      if (pFunc->pCode[nPos] == HB_P_PUSHBLOCKLARGE)
      {
        pCode++;
      }

      usVarCount = HB_PCODE_MKUSHORT(pCode);
      while (usVarCount--)
      {
        HB_USHORT usVar;
        pCode += 2;
        usVar = HB_PCODE_MKUSHORT(pCode);
        if (usVar > 0)
        {
          pLocals[usVar - 1].bFlags |= OPT_LOCAL_FLAG_BLOCK;
        }
      }
      break;
    }

      /* local name is not a big usage...
      case HB_P_LOCALNAME:
      */
    }
    nLastPos = nPos;
    nPos += hb_compPCodeSize(pFunc, nPos);
  }
}

static void hb_compPCodeEnumSelfifyLocal(PHB_HFUNC pFunc, HB_SHORT isLocal)
{
  HB_SIZE nPos = 0, nLastPos = 0;

  while (nPos < pFunc->nPCodePos)
  {
    switch (pFunc->pCode[nPos])
    {
    case HB_P_PUSHLOCALNEAR:
      if (isLocal == static_cast<signed char>(pFunc->pCode[nPos + 1]))
      {
        pFunc->pCode[nPos] = HB_P_PUSHSELF;
        hb_compNOOPfill(pFunc, nPos + 1, 1, false, false);
      }
      break;

    case HB_P_PUSHLOCAL:
      if (isLocal == HB_PCODE_MKSHORT(&pFunc->pCode[nPos + 1]))
      {
        pFunc->pCode[nPos] = HB_P_PUSHSELF;
        hb_compNOOPfill(pFunc, nPos + 1, 2, false, false);
      }
      break;

    case HB_P_POPLOCALNEAR:
      if (isLocal == static_cast<signed char>(pFunc->pCode[nPos + 1]))
      {
        assert(nPos > 0 && pFunc->pCode[nLastPos] == HB_P_PUSHSELF && !hb_compHasJump(pFunc, nPos));

        hb_compNOOPfill(pFunc, nLastPos, 1, false, false);
        hb_compNOOPfill(pFunc, nPos, 2, false, false);
      }
      break;

    case HB_P_POPLOCAL:
      if (isLocal == HB_PCODE_MKSHORT(&pFunc->pCode[nPos + 1]))
      {
        assert(nPos > 0 && pFunc->pCode[nLastPos] == HB_P_PUSHSELF && !hb_compHasJump(pFunc, nPos));

        hb_compNOOPfill(pFunc, nLastPos, 1, false, false);
        hb_compNOOPfill(pFunc, nPos, 3, false, false);
      }
      break;
    }
    nLastPos = nPos;
    nPos += hb_compPCodeSize(pFunc, nPos);
  }
}

static int hb_compPCodeTraceAssignedUnused(PHB_HFUNC pFunc, HB_SIZE nPos, HB_BYTE *pMap, HB_SHORT isLocal,
                                           bool fCanBreak)
{
  for (;;)
  {
    if (pMap[nPos])
    {
      return 0;
    }

    pMap[nPos] = 1;

    if (pFunc->pCode[nPos] == HB_P_FUNCTION || pFunc->pCode[nPos] == HB_P_FUNCTIONSHORT ||
        pFunc->pCode[nPos] == HB_P_DO || pFunc->pCode[nPos] == HB_P_DOSHORT || pFunc->pCode[nPos] == HB_P_SEND ||
        pFunc->pCode[nPos] == HB_P_SENDSHORT || pFunc->pCode[nPos] == HB_P_MACRODO ||
        pFunc->pCode[nPos] == HB_P_MACROFUNC || pFunc->pCode[nPos] == HB_P_MACROSEND)
    {
      fCanBreak = true;
    }
    else if (pFunc->pCode[nPos] == HB_P_POPLOCAL || pFunc->pCode[nPos] == HB_P_POPLOCALNEAR ||
             pFunc->pCode[nPos] == HB_P_PUSHLOCAL || pFunc->pCode[nPos] == HB_P_PUSHLOCALNEAR ||
             pFunc->pCode[nPos] == HB_P_PUSHLOCALREF || pFunc->pCode[nPos] == HB_P_LOCALINCPUSH ||
             pFunc->pCode[nPos] == HB_P_LOCALDEC || pFunc->pCode[nPos] == HB_P_LOCALINC ||
             pFunc->pCode[nPos] == HB_P_LOCALADDINT || pFunc->pCode[nPos] == HB_P_LOCALNEARADDINT)
    {
      if (hb_compLocalGetNumber(pFunc->pCode + nPos) == isLocal)
      {
        if (pFunc->pCode[nPos] == HB_P_POPLOCAL || pFunc->pCode[nPos] == HB_P_POPLOCALNEAR)
        {
          if (fCanBreak)
          {
            nPos += hb_compPCodeSize(pFunc, nPos);
            while (pFunc->pCode[nPos] != HB_P_ENDPROC && pFunc->pCode[nPos] != HB_P_ENDBLOCK &&
                   pFunc->pCode[nPos] != HB_P_SEQBEGIN && pFunc->pCode[nPos] != HB_P_SEQEND)
            {
              nPos += hb_compPCodeSize(pFunc, nPos);
            }
            if (pFunc->pCode[nPos] == HB_P_SEQEND)
            {
              /* Process RECOVER */
              fCanBreak = false;
              nPos += hb_compPCodeSize(pFunc, nPos);
              continue;
            }
            return 0;
          }
          else
          {
            return 0;
          }
        }
        else
        {
          return 1;
        }
      }
    }

    /* The following part of the function is standard for all recursive pcode
       tracing, except recursive function calls are hardcoded. We can implement
       universal recursive tracer by putting all parameters into "cargo"
       structure. [Mindaugas] */

    if (hb_compIsJump(pFunc->pCode[nPos]))
    {
      HB_SIZE nPos2 = nPos + hb_compJumpGetOffset(&pFunc->pCode[nPos]);

      if (hb_compIsUncondJump(pFunc->pCode[nPos]))
      {
        nPos = nPos2;
        continue;
      }

      if (hb_compPCodeTraceAssignedUnused(pFunc, nPos2, pMap, isLocal, fCanBreak))
      {
        return 1;
      }
    }
    else if (pFunc->pCode[nPos] == HB_P_SWITCH)
    { /* Switch is multi-place jump */
      HB_USHORT usCount = HB_PCODE_MKUSHORT(pFunc->pCode + nPos + 1);

      nPos += 3;
      for (HB_USHORT us = 0; us < usCount; us++)
      {
        if (hb_compPCodeTraceAssignedUnused(pFunc, nPos, pMap, isLocal, fCanBreak))
        {
          return 1;
        }

        nPos += hb_compPCodeSize(pFunc, nPos);
        nPos += hb_compPCodeSize(pFunc, nPos);
      }
      continue;
    }

    if (pFunc->pCode[nPos] == HB_P_ENDPROC || pFunc->pCode[nPos] == HB_P_ENDBLOCK)
    {
      break;
    }

    nPos += hb_compPCodeSize(pFunc, nPos);
  }
  return 0;
}

static void hb_compPCodeEnumAssignedUnused(HB_COMP_DECL, PHB_HFUNC pFunc, PHB_OPT_LOCAL pLocals)
{
  HB_USHORT usLine = 0;

  auto pMap = static_cast<HB_BYTE *>(hb_xgrab(pFunc->nPCodePos));

  HB_SIZE nPos = 0, nLastPos = 0;

  while (nPos < pFunc->nPCodePos)
  {
    HB_SHORT isLocal;
    int iCheck = 0;

    if (pFunc->pCode[nPos] == HB_P_POPLOCAL || pFunc->pCode[nPos] == HB_P_POPLOCALNEAR)
    {
      /* skip pop NIL (var := NIL), to allow force garbage collection */
      if (nPos == 0 || pFunc->pCode[nLastPos] != HB_P_PUSHNIL)
      {
        iCheck = 1;
      }
    }
    else if (pFunc->pCode[nPos] == HB_P_LOCALDEC || pFunc->pCode[nPos] == HB_P_LOCALINC ||
             pFunc->pCode[nPos] == HB_P_LOCALADDINT || pFunc->pCode[nPos] == HB_P_LOCALNEARADDINT)
    {
      iCheck = 1;
    }
    else if (pFunc->pCode[nPos] == HB_P_PUSHLOCALREF)
    {
      HB_SIZE nPosNext = nPos + hb_compPCodeSize(pFunc, nPos);

      switch (pFunc->pCode[nPosNext])
      {
      case HB_P_PUSHTIMESTAMP:
      case HB_P_PUSHBLOCK:
      case HB_P_PUSHBLOCKSHORT:
      case HB_P_PUSHFIELD:
      case HB_P_PUSHBYTE:
      case HB_P_PUSHINT:
      case HB_P_PUSHLOCAL:
      case HB_P_PUSHLOCALNEAR:
      case HB_P_PUSHLONG:
      case HB_P_PUSHMEMVAR:
      case HB_P_PUSHNIL:
      case HB_P_PUSHDOUBLE:
      case HB_P_PUSHSELF:
      case HB_P_PUSHSTATIC:
      case HB_P_PUSHSTR:
      case HB_P_PUSHSTRSHORT:
      case HB_P_PUSHVARIABLE:
      case HB_P_ONE:
      case HB_P_ZERO:
      case HB_P_PUSHLONGLONG:
      case HB_P_PUSHDATE:
      case HB_P_PUSHSTRHIDDEN:
      case HB_P_PUSHBLOCKLARGE:
      case HB_P_PUSHSTRLARGE:
      case HB_P_LOCALINCPUSH:
        switch (pFunc->pCode[nPosNext + hb_compPCodeSize(pFunc, nPosNext)])
        {
        case HB_P_PLUSEQPOP:
        case HB_P_MINUSEQPOP:
        case HB_P_MULTEQPOP:
        case HB_P_DIVEQPOP:
        case HB_P_MODEQPOP:
        case HB_P_EXPEQPOP:
        case HB_P_DECEQPOP:
        case HB_P_INCEQPOP:
          iCheck = 1;
          break;
        case HB_P_PLUSEQ:
        case HB_P_MINUSEQ:
        case HB_P_MULTEQ:
        case HB_P_DIVEQ:
        case HB_P_MODEQ:
        case HB_P_EXPEQ:
        case HB_P_DECEQ:
        case HB_P_INCEQ:
          iCheck = 2;
          break;
        }
      }
    }

    if (iCheck != 0 &&
        (isLocal = hb_compLocalGetNumber(&pFunc->pCode[nPos])) > static_cast<HB_SHORT>(pFunc->wParamCount))
    {
      PHB_HVAR pVar = pFunc->pLocals;

      for (HB_SHORT is = 1; is < isLocal; is++)
      {
        pVar = pVar->pNext;
      }

      assert(pLocals[isLocal - 1].bFlags != 0);

      /* Skip detachables, referenced, optimizable self */
      if ((pLocals[isLocal - 1].bFlags & (OPT_LOCAL_FLAG_BLOCK | OPT_LOCAL_FLAG_PUSHREF)) == 0 &&
          pLocals[isLocal - 1].bFlags != OPT_LOCAL_FLAG_POPSELF &&
          pLocals[isLocal - 1].bFlags != (OPT_LOCAL_FLAG_PUSH | OPT_LOCAL_FLAG_POPSELF))
      {
        memset(pMap, 0, pFunc->nPCodePos);
        if (iCheck == 1)
        {
          pMap[nPos] = 1;
        }

        if (!hb_compPCodeTraceAssignedUnused(pFunc, nPos + hb_compPCodeSize(pFunc, nPos), pMap, isLocal, false))
        {
          char szFun[256];

          /* FIXME: We calculate line number by simple tracking last HB_P_LINE,
             but it can work bad, if line number optimizer is clever enough.
             To obtain real line number we need one more tree scan or other
             algorithm. [Mindaugas] */

          if (HB_COMP_PARAM->iErrorFmt == HB_ERRORFMT_CLIPPER)
          {
            hb_snprintf(szFun, sizeof(szFun), "%s(%i)", pFunc->szName, usLine);
          }
          else
          {
            hb_snprintf(szFun, sizeof(szFun), "%i:%s", usLine, pFunc->szName);
          }
          hb_compGenWarning(HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_ASSIGNED_UNUSED, pVar->szName, szFun);
        }
      }
    }
    else if (pFunc->pCode[nPos] == HB_P_LINE)
    {
      usLine = HB_PCODE_MKUSHORT(pFunc->pCode + nPos + 1);
    }
    nLastPos = nPos;
    nPos += hb_compPCodeSize(pFunc, nPos);
  }
  hb_xfree(pMap);
}

static void hb_compPCodeEnumRenumberLocals(PHB_HFUNC pFunc, PHB_OPT_LOCAL pLocals)
{
  HB_SIZE nPos = 0;

  while (nPos < pFunc->nPCodePos)
  {
    switch (pFunc->pCode[nPos])
    {
    case HB_P_POPLOCALNEAR:
    case HB_P_PUSHLOCALNEAR:
    case HB_P_LOCALNEARADDINT: {
      HB_BYTE *pVar = &pFunc->pCode[nPos + 1];
      HB_SHORT isVar = static_cast<signed char>(pVar[0]);

      if (isVar > 0 && pLocals[isVar - 1].isNumber != isVar)
      {
        isVar = pLocals[isVar - 1].isNumber;
        if (isVar > 0)
        {
          pVar[0] = HB_LOBYTE(isVar);
          pVar[1] = HB_HIBYTE(isVar);
        }
        else
        {
          hb_compNOOPfill(pFunc, nPos, hb_compPCodeSize(pFunc, nPos), false, false);
        }
      }
      break;
    }

    case HB_P_LOCALNAME:
    case HB_P_POPLOCAL:
    case HB_P_PUSHLOCAL:
    case HB_P_PUSHLOCALREF:
    case HB_P_LOCALADDINT:
    case HB_P_LOCALDEC:
    case HB_P_LOCALINC:
    case HB_P_LOCALINCPUSH: {
      HB_BYTE *pVar = &pFunc->pCode[nPos + 1];
      HB_SHORT isVar = HB_PCODE_MKSHORT(pVar);

      if (isVar > 0 && pLocals[isVar - 1].isNumber != isVar)
      {
        isVar = pLocals[isVar - 1].isNumber;
        if (isVar > 0)
        {
          pVar[0] = HB_LOBYTE(isVar);
          pVar[1] = HB_HIBYTE(isVar);
        }
        else
        {
          hb_compNOOPfill(pFunc, nPos, hb_compPCodeSize(pFunc, nPos), false, false);
        }
      }
      break;
    }

    case HB_P_PUSHBLOCK:
    case HB_P_PUSHBLOCKLARGE: {
      HB_BYTE *pVar = &pFunc->pCode[nPos + 5];
      HB_USHORT usVarCount;

      if (pFunc->pCode[nPos] == HB_P_PUSHBLOCKLARGE)
      {
        pVar++;
      }

      usVarCount = HB_PCODE_MKUSHORT(pVar);
      while (usVarCount--)
      {
        HB_SHORT isVar;

        pVar += 2;
        isVar = HB_PCODE_MKSHORT(pVar);

        if (isVar > 0 && pLocals[isVar - 1].isNumber != isVar)
        {
          isVar = pLocals[isVar - 1].isNumber;

          assert(isVar > 0); /* We do not allow removal of detached locals */

          pVar[0] = HB_LOBYTE(isVar);
          pVar[1] = HB_HIBYTE(isVar);
        }
      }
      break;
    }
    }
    nPos += hb_compPCodeSize(pFunc, nPos);
  }
}

void hb_compPCodeTraceOptimizer(HB_COMP_DECL)
{
  PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;

  /* Many (perhaps ALL) functions of pcode trace optimization depends on pcodes.
     Please, check these functions if new pcode is added, or existing changed.
     Special attention should be paid, if new pcode introduces branching, codeblocks,
     or are related to parameters, local variables. [Mindaugas] */

  assert(HB_P_LAST_PCODE == 181);

  HB_USHORT usLocalCount = 0;
  PHB_HVAR pVar = pFunc->pLocals;
  while (pVar)
  {
    pVar = pVar->pNext;
    usLocalCount++;
  }

  if (!usLocalCount)
  {
    return;
  }

  /* FIXME: Support for PARAMETER sentence is not implemented.
            The temporary solution is to disable optimization at all if PARAMETER is used.  */
  {
    HB_SIZE nPos = 0;

    while (nPos < pFunc->nPCodePos)
    {
      if (pFunc->pCode[nPos] == HB_P_PARAMETER)
      {
        return;
      }
      nPos += hb_compPCodeSize(pFunc, nPos);
    }
  }

  /* Initial scan */
  auto pLocals = static_cast<PHB_OPT_LOCAL>(hb_xgrabz(sizeof(HB_OPT_LOCAL) * usLocalCount));
  hb_compPCodeEnumScanLocals(pFunc, pLocals);

  /* Check */
  HB_USHORT usIndex = 0;
  pVar = pFunc->pLocals;
  while (pVar)
  {
    /* Compiler and optimizer should have the same opinion about variable usage */
    /* I hope that it will in the future but now compiler does not detect some
     * dead code before marking variables as used so this conditions fails in
     * code like:
     *    proc main()
     *       local x
     *       if .F.
     *          x := 1
     *       endif
     *       return
     * [druzus]
     */
#if 0
      assert((!(pVar->iUsed & HB_VU_USED) && pLocals[usIndex].bFlags == 0) || ((pVar->iUsed & HB_VU_USED) && pLocals[usIndex].bFlags != 0));
#endif

    if (usIndex >= pFunc->wParamCount && pLocals[usIndex].bFlags == OPT_LOCAL_FLAG_PUSH)
    {
      char szFun[256];

      if (HB_COMP_PARAM->iErrorFmt == HB_ERRORFMT_CLIPPER)
      {
        hb_snprintf(szFun, sizeof(szFun), "%s(%i)", pFunc->szName, pVar->iDeclLine);
      }
      else
      {
        hb_snprintf(szFun, sizeof(szFun), "%i:%s", pVar->iDeclLine, pFunc->szName);
      }
      hb_compGenWarning(HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_NEVER_ASSIGNED, pVar->szName, szFun);
    }

    pVar = pVar->pNext;
    usIndex++;
  }

  /* Selfifying */
  if (HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) && !HB_COMP_PARAM->fDebugInfo)
  {
    pVar = pFunc->pLocals;
    for (usIndex = 0; usIndex < pFunc->wParamCount; usIndex++)
    {
      pVar = pVar->pNext;
    }

    for (usIndex = pFunc->wParamCount; usIndex < usLocalCount; usIndex++)
    {
      if (pLocals[usIndex].bFlags == (OPT_LOCAL_FLAG_PUSH | OPT_LOCAL_FLAG_POPSELF) ||
          pLocals[usIndex].bFlags == OPT_LOCAL_FLAG_POPSELF)
      {
#if 0
            printf("Info: %s(%d) selfifying variable '%s'\n", pFunc->szName, pVar->iDeclLine, pVar->szName);
#endif
        hb_compPCodeEnumSelfifyLocal(pFunc, usIndex + 1);
        pLocals[usIndex].bFlags = 0;
      }
      pVar = pVar->pNext;
    }
  }

  /* Scan assigned, but not used */
  hb_compPCodeEnumAssignedUnused(HB_COMP_PARAM, pFunc, pLocals);

  /* Delete unused */
  if (HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) && !HB_COMP_PARAM->fDebugInfo)
  {
    auto fBool = false;

    for (usIndex = pFunc->wParamCount; usIndex < usLocalCount; usIndex++)
    {
      if (pLocals[usIndex].bFlags == 0)
      {
        fBool = true;
        break;
      }
    }

    if (fBool)
    {
      usIndex = usLocalCount = 0;
      PHB_HVAR *ppVar = &pFunc->pLocals;
      pVar = pFunc->pLocals;
      while (pVar)
      {
        if (usLocalCount < pFunc->wParamCount || pLocals[usLocalCount].bFlags != 0)
        {
          pLocals[usLocalCount].isNumber = ++usIndex;
          ppVar = &pVar->pNext;
          pVar = pVar->pNext;
        }
        else
        {
#if 0
               printf("Info: %s(%d) removing unused variable '%s'\n", pFunc->szName, pVar->iDeclLine, pVar->szName);
#endif

          /* Delete pVar from the linked list */
          *ppVar = pVar->pNext;
          hb_xfree(pVar);
          pVar = *ppVar;
        }
        usLocalCount++;
      }
      hb_compPCodeEnumRenumberLocals(pFunc, pLocals);
    }
  }
  hb_xfree(pLocals);
}
