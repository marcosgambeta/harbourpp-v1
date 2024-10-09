//
// ProcName(), ProcLine() and ProcFile() functions
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) (ProcFile())
// Copyright 2001 JFL (Mafact) <jfl@mafact.com>
//    Adding the MethodName() just calling ProcName()
//    Special treatment in case of Object and Eval (only for method mame)
//    skipping block and adding (b) before the method name
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

#include "hbvmint.hpp"
#include "hbapi.hpp"
#include "hbapicls.hpp"
#include "hbapiitm.hpp"
#include "hbstack.hpp"
#include "hbvm.hpp"

HB_FUNC(HB_METHODNAME)
{
  char szName[HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5];
  hb_retc(hb_procname(hb_parni(1) + 1, szName, true));
}

HB_FUNC(PROCNAME)
{
  char szName[HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5];
  hb_retc(hb_procname(hb_parni(1) + 1, szName, false));
}

HB_FUNC(PROCLINE)
{
  HB_ISIZ nOffset = hb_stackBaseProcOffset(hb_parni(1) + 1);

  if (nOffset > 0)
  {
    hb_retni(hb_stackItem(nOffset)->symbolStackState()->uiLineNo);
  }
  else
  {
    hb_retni(0);
  }
}

#ifdef HB_CLP_UNDOC

// NOTE: Clipper undocumented function, which always returns an empty
//       string. [vszakats]

HB_FUNC(PROCFILE)
{
#ifndef HB_CLP_STRICT
  PHB_SYMB pSym = nullptr;

  if (HB_ISSYMBOL(1))
  {
    pSym = hb_param(1, Harbour::Item::SYMBOL)->getSymbol();
  }
  else if (HB_ISCHAR(1))
  {
    auto pDynSym = hb_dynsymFindName(hb_parc(1));

    if (pDynSym)
    {
      pSym = pDynSym->pSymbol;
    }
  }
  else
  {
    HB_ISIZ nOffset = hb_stackBaseProcOffset(hb_parni(1) + 1);

    if (nOffset > 0)
    {
      auto pBase = hb_stackItem(nOffset);

      pSym = pBase->symbolValue();
      if (pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym)
      {
        auto pSelf = hb_stackItem(nOffset + 1);

        if (pSelf->isBlock())
        {
          pSym = pSelf->blockValue()->pDefSymb;
        }
        else if (pBase->symbolStackState()->uiClass)
        {
          pSym = hb_clsMethodSym(pBase);
        }
      }
      else if (pBase->symbolStackState()->uiClass)
      {
        pSym = hb_clsMethodSym(pBase);
      }
    }
  }
  hb_retc(hb_vmFindModuleSymbolName(hb_vmGetRealFuncSym(pSym)));
#else
  hb_retc_null();
#endif
}

#endif

// NOTE: szName size must be an at least:
//       HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 [vszakats]
#define HB_PROCBUF_LEN (HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 4)
char *hb_procname(int iLevel, char *szName, HB_BOOL fMethodName)
{
  HB_ISIZ nOffset = hb_stackBaseProcOffset(iLevel);

  szName[0] = '\0';
  if (nOffset > 0)
  {
    auto pBase = hb_stackItem(nOffset);
    auto pSelf = hb_stackItem(nOffset + 1);

    if (fMethodName && nOffset > 0 && pBase->symbolValue() == &hb_symEval &&
        pBase->symbolStackState()->uiClass)
    {
      HB_ISIZ nPrevOffset = hb_stackItem(nOffset)->symbolStackState()->nBaseItem;

      if (hb_stackItem(nPrevOffset)->symbolStackState()->uiClass == pBase->symbolStackState()->uiClass &&
          hb_stackItem(nPrevOffset)->symbolStackState()->uiMethod == pBase->symbolStackState()->uiMethod)
      {
        pBase = hb_stackItem(nPrevOffset);
        pSelf = hb_stackItem(nPrevOffset + 1);
      }
    }

    if (pBase->symbolValue() == &hb_symEval || pBase->symbolValue()->pDynSym == hb_symEval.pDynSym)
    {
      hb_strncat(szName, "(b)", HB_PROCBUF_LEN);
      // it is a method name?
      if (fMethodName && pBase->symbolStackState()->uiClass)
      {
        hb_strncat(szName, hb_clsName(pBase->symbolStackState()->uiClass), HB_PROCBUF_LEN);
        hb_strncat(szName, ":", HB_PROCBUF_LEN);
        hb_strncat(
            szName,
            hb_clsMethodName(pBase->symbolStackState()->uiClass, pBase->symbolStackState()->uiMethod),
            HB_PROCBUF_LEN);
      }
      else if (pSelf->isBlock())
      {
        hb_strncat(szName, pSelf->blockValue()->pDefSymb->szName, HB_PROCBUF_LEN);
      }
      else if (pSelf->isSymbol())
      {
        hb_strncpy(szName, pSelf->symbolValue()->szName, HB_PROCBUF_LEN);
      }
      else
      {
        hb_strncat(szName, pBase->symbolValue()->szName, HB_PROCBUF_LEN);
      }
    }
    else
    {
      // it is a method name?
      if (pBase->symbolStackState()->uiClass)
      {
        hb_strncat(szName, hb_clsName(pBase->symbolStackState()->uiClass), HB_PROCBUF_LEN);
        hb_strncat(szName, ":", HB_PROCBUF_LEN);
      }
      hb_strncat(szName, pBase->symbolValue()->szName, HB_PROCBUF_LEN);
    }
  }

  return szName;
}

// NOTE: szName size must be an at least:
//          HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5
//       szFile size must be an at least:
//          HB_PATH_MAX
HB_BOOL hb_procinfo(int iLevel, char *szName, HB_USHORT *puiLine, char *szFile)
{
  HB_ISIZ nOffset = hb_stackBaseProcOffset(iLevel);

  if (nOffset > 0)
  {
    auto pBase = hb_stackItem(nOffset);
    auto pSelf = hb_stackItem(nOffset + 1);

    PHB_SYMB pSym = pBase->symbolValue();

    if (szName != nullptr)
    {
      szName[0] = '\0';
      if (pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym)
      {
        hb_strncat(szName, "(b)", HB_PROCBUF_LEN);

        if (pSelf->isBlock())
        {
          hb_strncat(szName, pSelf->blockValue()->pDefSymb->szName, HB_PROCBUF_LEN);
        }
        else
        {
          hb_strncat(szName, pSym->szName, HB_PROCBUF_LEN);
        }
      }
      else
      {
        if (pBase->symbolStackState()->uiClass)
        { // it is a method name
          hb_strncat(szName, hb_clsName(pBase->symbolStackState()->uiClass), HB_PROCBUF_LEN);
          hb_strncat(szName, ":", HB_PROCBUF_LEN);
        }
        hb_strncat(szName, pSym->szName, HB_PROCBUF_LEN);
      }
    }

    if (puiLine)
    {
      *puiLine = pBase->symbolStackState()->uiLineNo;
    }

    if (szFile != nullptr)
    {
      if (pSelf->isBlock() && (pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym))
      {
        pSym = pSelf->blockValue()->pDefSymb;
      }
      else if (pBase->symbolStackState()->uiClass)
      {
        pSym = hb_clsMethodSym(pBase);
      }

      const char *szModule = hb_vmFindModuleSymbolName(hb_vmGetRealFuncSym(pSym));

      if (szModule != nullptr)
      {
        hb_strncpy(szFile, szModule, HB_PATH_MAX - 1);
      }
      else
      {
        szFile[0] = '\0';
      }
    }

    return true;
  }

  if (szName != nullptr)
  {
    szName[0] = '\0';
  }
  if (puiLine)
  {
    *puiLine = 0;
  }
  if (szFile != nullptr)
  {
    szFile[0] = '\0';
  }

  return false;
}
