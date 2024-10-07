//
// Memvar (PRIVATE/PUBLIC) runtime support
//
// Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
//   __mvSave(), __mvRestore() (Thanks to Dave Pearson and Jo French for
//   the original Clipper function FReadMem() to read .mem files)
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

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapifs.hpp" // for __mvSave()/__mvRestore()
#include "hbdate.hpp"  // for __mvSave()/__mvRestore()
#include "hbcomp.hpp"  // for HB_VSCOMP_* macros
#include "error.ch"
#include "hbmemvar.ch"
#include "hbset.hpp"
#include "hbstack.hpp"

#include <cstddef>

#if !defined(HB_MT_VM)
#define hb_dynsymGetMemvar(p) (static_cast<PHB_ITEM>((p)->pMemvar))
#define hb_dynsymSetMemvar(p, h)                                                                                       \
  do                                                                                                                   \
  {                                                                                                                    \
    (p)->pMemvar = (h);                                                                                                \
  } while (false)
#endif

#define TABLE_INITHB_VALUE 100
#define TABLE_EXPANDHB_VALUE 50

struct mv_PUBLIC_var_info
{
  int iPos;
  HB_BOOL bFound; // TODO: bool
  PHB_DYNS pDynSym;
};

struct mv_memvarArray_info
{
  PHB_ITEM pArray;
  PHB_DYNS *pDyns;
  HB_SIZE nCount;
  int iScope;
};

static void hb_memvarCreateFromDynSymbol(PHB_DYNS pDynVar, int iScope, PHB_ITEM pValue);

static PHB_ITEM hb_memvarValueNew(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueNew()"));
#endif

  auto pMemvar = static_cast<PHB_ITEM>(hb_xgrab(sizeof(HB_ITEM)));
  pMemvar->setType(Harbour::Item::NIL);
  return pMemvar;
}

// This function increases the number of references to passed global value
#undef hb_memvarValueIncRef
void hb_memvarValueIncRef(PHB_ITEM pMemvar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueIncRef(%p)", static_cast<void*>(pMemvar)));
#endif

  hb_xRefInc(pMemvar);
}

// This function decreases the number of references to passed global value.
// If it is the last reference then this value is deleted.
void hb_memvarValueDecRef(PHB_ITEM pMemvar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%p)", static_cast<void*>(pMemvar)));
#endif

  if (hb_xRefDec(pMemvar))
  {
    if (pMemvar->isComplex())
    {
      pMemvar->clear();
    }
    hb_xfree(pMemvar);
  }
}

// Detach public or private variable (swap current value with a memvar handle)
static void hb_memvarDetachDynSym(PHB_DYNS pDynSym, PHB_ITEM pPrevMemvar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachDynSym(%p,%p)", static_cast<void*>(pDynSym), static_cast<void*>(pPrevMemvar)));
#endif

  PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynSym);
  hb_dynsymSetMemvar(pDynSym, pPrevMemvar);
  hb_memvarValueDecRef(pMemvar);
}

// Detach local variable (swap current value with a memvar handle)
PHB_ITEM hb_memvarDetachLocal(PHB_ITEM pLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachLocal(%p)", static_cast<void*>(pLocal)));
#endif

  if (pLocal->isByRef())
  {
    do
    {
      if (pLocal->isMemVar() || pLocal->isExtRef())
      {
        break;
      }
      else if (pLocal->isEnum())
      {
        if (!pLocal->item.asEnum.valuePtr)
        {
          PHB_ITEM pBase = pLocal->item.asEnum.basePtr->isByRef() ? hb_itemUnRef(pLocal->item.asEnum.basePtr)
                                                                  : pLocal->item.asEnum.basePtr;
          if (pBase->isArray())
          {
            auto pItem = hb_itemNew(nullptr);
            hb_arrayGetItemRef(pBase, pLocal->item.asEnum.offset, pItem);
            pLocal->item.asEnum.valuePtr = pItem;
            pLocal = pItem;
            break;
          }
        }
      }
      else if (pLocal->item.asRefer.value >= 0 && pLocal->item.asRefer.offset == 0)
      {
        break;
      }
      pLocal = hb_itemUnRefOnce(pLocal);
    } while (pLocal->isByRef());
  }

  // Change the value only if this variable is not referenced
  // by another codeblock yet.
  // In this case we have to copy the current value to a global memory
  // pool so it can be shared by codeblocks
  if (!pLocal->isMemVar())
  {
    auto pMemvar = hb_memvarValueNew();
    hb_itemRawCpy(pMemvar, pLocal);
    pMemvar->type &= ~Harbour::Item::DEFAULT;
    pLocal->setType(Harbour::Item::BYREF | Harbour::Item::MEMVAR);
    pLocal->item.asMemvar.value = pMemvar;
  }

  return pLocal;
}

// This function pushes passed dynamic symbol that belongs to PRIVATE variable
// into the stack. The value will be popped from it if the variable falls
// outside the scope (either by using RELEASE, CLEAR ALL, CLEAR MEMORY or by
// an exit from the function/procedure)
static void hb_memvarAddPrivate(PHB_DYNS pDynSym, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarAddPrivate(%p,%p)", static_cast<void*>(pDynSym), static_cast<void*>(pValue)));
#endif

  HB_STACK_TLS_PRELOAD

  PHB_PRIVATE_STACK pPrivateStack = hb_stackGetPrivateStack();

  PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynSym);
  // If the variable with the same name exists already
  // and it's PRIVATE variable declared in this function then
  // do not push new memvar on PRIVATEs stack
  if (pMemvar != nullptr)
  {
    HB_SIZE nCount = pPrivateStack->count;
    while (nCount > pPrivateStack->base)
    {
      if (pDynSym == pPrivateStack->stack[nCount - 1].pDynSym)
      {
        break;
      }
      --nCount;
    }
    if (nCount <= pPrivateStack->base)
    {
      pMemvar = nullptr;
    }
  }

  if (!pMemvar)
  {
    // Allocate the value from the end of table
    if (pPrivateStack->count == pPrivateStack->size)
    {
      // No more free values in the table - expand the table
      if (pPrivateStack->size == 0)
      {
        pPrivateStack->stack = static_cast<PHB_PRIVATE_ITEM>(hb_xgrab(sizeof(HB_PRIVATE_ITEM) * TABLE_INITHB_VALUE));
        pPrivateStack->size = TABLE_INITHB_VALUE;
        pPrivateStack->count = pPrivateStack->base = 0;
      }
      else
      {
        pPrivateStack->size += TABLE_EXPANDHB_VALUE;
        pPrivateStack->stack = static_cast<PHB_PRIVATE_ITEM>(
            hb_xrealloc(pPrivateStack->stack, sizeof(HB_PRIVATE_ITEM) * pPrivateStack->size));
      }
    }

    pPrivateStack->stack[pPrivateStack->count].pDynSym = pDynSym;
    pPrivateStack->stack[pPrivateStack->count++].pPrevMemvar = hb_dynsymGetMemvar(pDynSym);

    if (pValue && pValue->isMemVar())
    {
      pMemvar = pValue->item.asMemvar.value;
      hb_xRefInc(pMemvar);
      pValue = nullptr;
    }
    else
    {
      pMemvar = hb_memvarValueNew();
    }
    hb_dynsymSetMemvar(pDynSym, pMemvar);
  }

  if (pValue != nullptr)
  {
    hb_itemCopy(pMemvar, pValue);
    // Remove MEMOFLAG if exists (assignment from field).
    pMemvar->type &= ~Harbour::Item::MEMOFLAG;
  }
}

// This function returns current PRIVATE variables stack base
HB_SIZE hb_memvarGetPrivatesBase(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetPrivatesBase()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_SIZE nBase = hb_stackGetPrivateStack()->base;
  hb_stackGetPrivateStack()->base = hb_stackGetPrivateStack()->count;
  return nBase;
}

// This function releases PRIVATE variables created after passed base
void hb_memvarSetPrivatesBase(HB_SIZE nBase)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetPrivatesBase(%" HB_PFS "u)", nBase));
#endif

  HB_STACK_TLS_PRELOAD

  PHB_PRIVATE_STACK pPrivateStack = hb_stackGetPrivateStack();

  while (pPrivateStack->count > pPrivateStack->base)
  {
    PHB_DYNS pDynSym = pPrivateStack->stack[--pPrivateStack->count].pDynSym;

    if (hb_dynsymGetMemvar(pDynSym))
    {
      // Restore previous value for variables that were overridden
      hb_memvarDetachDynSym(pDynSym, pPrivateStack->stack[pPrivateStack->count].pPrevMemvar);
    }
  }
  pPrivateStack->base = nBase;
}

// Update PRIVATE base offset so they will not be removed
// when function return
void hb_memvarUpdatePrivatesBase(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarUpdatePrivatesBase()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackGetPrivateStack()->base = hb_stackGetPrivateStack()->count;
}

// Reset PRIVATE base offset to the level of previous function
static void hb_memvarResetPrivatesBase(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarResetPrivatesBase()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackGetPrivateStack()->base = hb_stackBaseItem()->symbolStackState()->nPrivateBase;
}

// This functions copies passed item value into the memvar pointed
// by symbol
//
// pMemvar - symbol associated with a variable
// pItem   - value to store in memvar
void hb_memvarSetValue(PHB_SYMB pMemvarSymb, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetValue(%p, %p)", static_cast<void*>(pMemvarSymb), static_cast<void*>(pItem)));
#endif

  PHB_DYNS pDyn = pMemvarSymb->pDynSym;
  if (pDyn != nullptr)
  {
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDyn);

#if 0
      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) assigned", static_cast<void*>(pMemvar), pMemvarSymb->szName));
#endif

    if (pMemvar != nullptr)
    {
      // value is already created
      hb_itemCopyToRef(pMemvar, pItem);
      // Remove MEMOFLAG if exists (assignment from field).
      pMemvar->type &= ~Harbour::Item::MEMOFLAG;
    }
    else
    {
      // assignment to undeclared memvar - PRIVATE is assumed
      hb_memvarCreateFromDynSymbol(pDyn, HB_VSCOMP_PRIVATE, pItem);
    }
  }
  else
  {
    hb_errInternal(HB_EI_MVBADSYMBOL, nullptr, pMemvarSymb->szName, nullptr);
  }
}

HB_ERRCODE hb_memvarGet(PHB_ITEM pItem, PHB_SYMB pMemvarSymb)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGet(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pMemvarSymb)));
#endif

  HB_ERRCODE errCode = Harbour::FAILURE;

  PHB_DYNS pDyn = pMemvarSymb->pDynSym;
  if (pDyn != nullptr)
  {
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDyn);

#if 0
      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) queried", static_cast<void*>(pMemvar), pMemvarSymb->szName));
#endif

    if (pMemvar != nullptr)
    {
      // value is already created
      if (pMemvar->isByRef())
      {
        hb_itemCopy(pItem, hb_itemUnRef(pMemvar));
      }
      else
      {
        hb_itemCopy(pItem, pMemvar);
      }
      errCode = Harbour::SUCCESS;
    }
  }
  else
  {
    hb_errInternal(HB_EI_MVBADSYMBOL, nullptr, pMemvarSymb->szName, nullptr);
  }

  return errCode;
}

void hb_memvarGetValue(PHB_ITEM pItem, PHB_SYMB pMemvarSymb)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetValue(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pMemvarSymb)));
#endif

  if (hb_memvarGet(pItem, pMemvarSymb) == Harbour::FAILURE)
  {
    // Generate an error with retry possibility
    // (user created error handler can create this variable)
    auto pError = hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, 1003, nullptr, pMemvarSymb->szName, 0, EF_CANRETRY);
    pItem->clear();

    while (hb_errLaunch(pError) == E_RETRY)
    {
      if (hb_memvarGet(pItem, pMemvarSymb) == Harbour::SUCCESS)
      {
        break;
      }
    }

    hb_errRelease(pError);
  }
}

void hb_memvarGetRefer(PHB_ITEM pItem, PHB_SYMB pMemvarSymb)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetRefer(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(pMemvarSymb)));
#endif

  auto pDyn = static_cast<PHB_DYNS>(pMemvarSymb->pDynSym);
  if (pDyn != nullptr)
  {
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDyn);

#if 0
      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) referenced", static_cast<void*>(pMemvar), pMemvarSymb->szName));
#endif

    if (pMemvar != nullptr)
    {
      if (pMemvar->isByRef() && !pMemvar->isEnum())
      {
        hb_itemCopy(pItem, pMemvar);
      }
      else
      {
        // value is already created
        pItem->setType(Harbour::Item::BYREF | Harbour::Item::MEMVAR);
        pItem->item.asMemvar.value = pMemvar;
        hb_xRefInc(pMemvar);
      }
    }
    else
    {
      // Generate an error with retry possibility
      // (user created error handler can make this variable accessible)
      auto pError = hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, 1003, nullptr, pMemvarSymb->szName, 0, EF_CANRETRY);
      pItem->clear();

      while (hb_errLaunch(pError) == E_RETRY)
      {
        pMemvar = hb_dynsymGetMemvar(pDyn);
        if (pMemvar != nullptr)
        {
          if (pMemvar->isByRef() && !pMemvar->isEnum())
          {
            hb_itemCopy(pItem, pMemvar);
          }
          else
          {
            // value is already created
            pItem->setType(Harbour::Item::BYREF | Harbour::Item::MEMVAR);
            pItem->item.asMemvar.value = pMemvar;
            hb_xRefInc(pMemvar);
          }
          break;
        }
      }
      hb_errRelease(pError);
    }
  }
  else
  {
    hb_errInternal(HB_EI_MVBADSYMBOL, nullptr, pMemvarSymb->szName, nullptr);
  }
}

PHB_ITEM hb_memvarGetItem(PHB_SYMB pMemvarSymb)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetItem(%p)", static_cast<void*>(pMemvarSymb)));
#endif

  if (pMemvarSymb->pDynSym)
  {
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pMemvarSymb->pDynSym);

    if (pMemvar != nullptr)
    {
      if (pMemvar->isByRef())
      {
        return hb_itemUnRef(pMemvar);
      }
      else
      {
        return pMemvar;
      }
    }
  }
  return nullptr;
}

void hb_memvarNewParameter(PHB_SYMB pSymbol, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarNewParameter(%p, %p)", static_cast<void*>(pSymbol), static_cast<void*>(pValue)));
#endif

  hb_memvarCreateFromDynSymbol(pSymbol->pDynSym, HB_VSCOMP_PRIVATE, pValue);
}

static PHB_DYNS hb_memvarFindSymbol(const char *szArg, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarFindSymbol(%p,%" HB_PFS "u)", static_cast<const void*>(szArg), nLen));
#endif

  PHB_DYNS pDynSym = nullptr;

  if (nLen && szArg && *szArg)
  {
    char szUprName[HB_SYMBOL_NAME_LEN + 1];
    int iSize = 0;

    do
    {
      char cChar = *szArg++;

      if (cChar >= 'a' && cChar <= 'z')
      {
        szUprName[iSize++] = cChar - ('a' - 'A');
      }
      else if (cChar == ' ' || cChar == '\t' || cChar == '\n')
      {
        if (iSize)
        {
          break;
        }
      }
      else if (!cChar)
      {
        break;
      }
      else
      {
        szUprName[iSize++] = cChar;
      }
    } while (--nLen && iSize < HB_SYMBOL_NAME_LEN);

    if (iSize)
    {
      szUprName[iSize] = '\0';
      pDynSym = hb_dynsymFind(szUprName);
    }
  }
  return pDynSym;
}

static PHB_DYNS hb_memvarGetSymbol(PHB_ITEM pItem)
{
#if 0
  HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetSymbol(%p)", pItem));
#endif

  PHB_DYNS pDynSym = nullptr;

  if (pItem != nullptr)
  {
    if (pItem->isString())
    {
      pDynSym = hb_memvarFindSymbol(pItem->stringValue(), pItem->stringLength());
    }
    else if (pItem->isSymbol())
    {
      pDynSym = pItem->symbolValue()->pDynSym;
      if (pDynSym == nullptr)
      {
        pDynSym = hb_dynsymFind(pItem->symbolValue()->szName);
      }
    }
  }
  return pDynSym;
}

char *hb_memvarGetStrValuePtr(char *szVarName, HB_SIZE *pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetStrValuePtr(%s, %p)", szVarName, static_cast<void*>(pnLen)));
#endif

  char *szValue = nullptr;

  auto pDynVar = hb_memvarFindSymbol(szVarName, *pnLen);

  if (pDynVar != nullptr)
  {
    // there is dynamic symbol with the requested name - check if it is
    // a memvar variable
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynVar);

    if (pMemvar != nullptr)
    {
      // variable contains some data
      if (pMemvar->isByRef())
      {
        pMemvar = hb_itemUnRef(pMemvar);
      }

      if (pMemvar->isString())
      {
        szValue = pMemvar->stringValue();
        *pnLen = pMemvar->stringLength();
      }
    }
  }

  return szValue;
}

// This function creates a value for memvar variable
//
// pMemvar - an item that stores the name of variable - it can be either
//          the Harbour::Item::SYMBOL (if created by PUBLIC statement) or Harbour::Item::STRING
//          (if created by direct call to __mvPublic() function)
// iScope - the scope of created variable - if a variable with the same name
//          exists already then it's value is hidden by new variable with
//          passed scope
// pValue - optional item used to initialize the value of created variable
//          or nullptr
void hb_memvarCreateFromItem(PHB_ITEM pMemvar, int iScope, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromItem(%p, %d, %p)", static_cast<void*>(pMemvar), iScope, static_cast<void*>(pValue)));
#endif

  PHB_DYNS pDynVar = nullptr;

  // find dynamic symbol or create one
  if (pMemvar->isSymbol())
  {
    pDynVar = pMemvar->symbolValue()->pDynSym;
    if (pDynVar == nullptr)
    {
      pDynVar = hb_dynsymGet(pMemvar->symbolValue()->szName);
    }
  }
  else if (pMemvar->isString())
  {
    pDynVar = hb_dynsymGet(pMemvar->stringValue());
  }

  if (pDynVar != nullptr)
  {
    hb_memvarCreateFromDynSymbol(pDynVar, iScope, pValue);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3008, nullptr, "&", HB_ERR_ARGS_BASEPARAMS);
  }
}

static void hb_memvarCreateFromDynSymbol(PHB_DYNS pDynVar, int iScope, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromDynSymbol(%p, %d, %p)", static_cast<void*>(pDynVar), iScope, static_cast<void*>(pValue)));
#endif

  if (iScope & HB_VSCOMP_PUBLIC)
  {
    // If the variable with the same name exists already
    // then the current value have to be unchanged
    if (!hb_dynsymGetMemvar(pDynVar))
    {
      auto pMemvar = hb_memvarValueNew();

      hb_dynsymSetMemvar(pDynVar, pMemvar);

      if (pValue != nullptr)
      {
        hb_itemCopy(pMemvar, pValue);
        // Remove MEMOFLAG if exists (assignment from field).
        pMemvar->type &= ~Harbour::Item::MEMOFLAG;
      }
      else
      {
        // new PUBLIC variable - initialize it to .F.
        pMemvar->setType(Harbour::Item::LOGICAL);

        // NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized
        //       to .T., this is normal Clipper behaviour. [vszakats]

        pMemvar->setLogicalValue(
            (strcmp(pDynVar->pSymbol->szName, "HARBOUR") == 0 || strcmp(pDynVar->pSymbol->szName, "CLIPPER") == 0));
      }
    }
  }
  else
  {
    // Create new PRIVATE var and add it to the PRIVATE variables stack
    hb_memvarAddPrivate(pDynVar, pValue);
  }
}

// This function releases all memory occupied by a memvar variable
// It also restores the value that was hidden if there is another
// PRIVATE variable with the same name.
static void hb_memvarRelease(PHB_ITEM pMemvar)
{
#if 0
  HB_TRACE(HB_TR_DEBUG, ("hb_memvarRelease(%p)", static_cast<void *>(pMemvar)));
#endif

  auto pDynSymbol = hb_memvarGetSymbol(pMemvar);

  if (pDynSymbol && hb_dynsymGetMemvar(pDynSymbol))
  {
    HB_STACK_TLS_PRELOAD
    HB_SIZE nBase = hb_stackGetPrivateStack()->count;

    // Find the variable with a requested name that is currently visible
    // Start from the top of the stack.
    while (nBase > 0)
    {
      if (pDynSymbol == hb_stackGetPrivateStack()->stack[--nBase].pDynSym)
      {
        // reset current value to NIL - the overridden variables will be
        // visible after exit from current procedure
        pMemvar = hb_dynsymGetMemvar(pDynSymbol);
        if (pMemvar != nullptr)
        {
          pMemvar->clear();
        }
        return;
      }
    }

    // No match found for PRIVATEs - it's PUBLIC so let's remove it.
    hb_memvarDetachDynSym(pDynSymbol, nullptr);
  }
  else if ((HB_ITEM_TYPERAW(pMemvar) & (Harbour::Item::STRING | Harbour::Item::SYMBOL)) == 0)
  {
    hb_errRT_BASE(EG_ARG, 3008, nullptr, "RELEASE", 1, pMemvar);
  }
}

// This function releases all memory occupied by a memvar variable and
// assigns NIL value - it releases variables created in current
// procedure only.
// The scope of released variables are specified using passed name's mask
static void hb_memvarReleaseWithMask(const char *szMask, bool bInclude)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarReleaseWithMask(%s, %d)", szMask, static_cast<int>(bInclude)));
#endif

  HB_STACK_TLS_PRELOAD

  HB_SIZE nCount = hb_stackGetPrivateStack()->count;
  HB_SIZE nBase = hb_stackBaseItem()->symbolStackState()->nPrivateBase;
  while (nCount-- > nBase)
  {
    PHB_DYNS pDynVar = hb_stackGetPrivateStack()->stack[nCount].pDynSym;
    // reset current value to NIL - the overridden variables will be
    // visible after exit from current procedure
    PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynVar);
    if (pMemvar != nullptr)
    {
      bool fMatch = hb_strMatchCaseWildExact(pDynVar->pSymbol->szName, szMask);
      if (bInclude ? fMatch : !fMatch)
      {
        pMemvar->clear();
      }
    }
  }
}

// Checks if passed dynamic symbol is a variable and returns its scope
static int hb_memvarScopeGet(PHB_DYNS pDynVar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScopeGet(%p)", static_cast<void*>(pDynVar)));
#endif

  if (hb_dynsymGetMemvar(pDynVar) == 0)
  {
    return HB_MV_UNKNOWN;
  }
  else
  {
    HB_STACK_TLS_PRELOAD
    HB_SIZE nBase = hb_stackGetPrivateStack()->count; // start from the top of the stack

    while (nBase)
    {
      if (pDynVar == hb_stackGetPrivateStack()->stack[--nBase].pDynSym)
      {
        if (nBase >= hb_stackGetPrivateStack()->base)
        {
          return HB_MV_PRIVATE_LOCAL;
        }
        else
        {
          return HB_MV_PRIVATE_GLOBAL;
        }
      }
    }
    return HB_MV_PUBLIC;
  }
}

// This function checks the scope of passed variable name
int hb_memvarScope(const char *szVarName, HB_SIZE nLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScope(%s, %" HB_PFS "u)", szVarName, nLength));
#endif

  auto pDynVar = hb_memvarFindSymbol(szVarName, nLength);

  if (pDynVar != nullptr)
  {
    return hb_memvarScopeGet(pDynVar);
  }
  else
  {
    return HB_MV_NOT_FOUND;
  }
}

#if !defined(HB_MT_VM)
// Releases memory occupied by a variable
static HB_DYNS_FUNC(hb_memvarClear)
{
  if (pDynSymbol != static_cast<PHB_DYNS>(Cargo) && hb_dynsymGetMemvar(pDynSymbol))
  {
    hb_memvarDetachDynSym(pDynSymbol, nullptr);
  }

  return true;
}
#endif

// Clear all memvar variables optionally without GetList PUBLIC variable
void hb_memvarsClear(HB_BOOL fAll)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsClear(%d)", static_cast<int>(fAll)));
#endif

  HB_STACK_TLS_PRELOAD

  PHB_DYNS pGetList = fAll ? nullptr : hb_dynsymFind("GETLIST");

  hb_stackClearMemvarsBase();
  hb_stackGetPrivateStack()->base = 0;
  hb_memvarSetPrivatesBase(0);
#if !defined(HB_MT_VM)
  hb_dynsymEval(hb_memvarClear, static_cast<void *>(pGetList));
#else
  // this is a little bit hacked but many times faster version
  // of memvars clearing because it scans only given thread stack
  // not global dynamic symbol table. It noticeable reduces the cost
  // of HVM thread releasing [druzus].
  hb_stackClearMemvars(pGetList ? pGetList->uiSymNum : 0);
#endif
}

// Checks passed dynamic symbol if it is a PUBLIC variable and
// increments the counter eventually
static HB_DYNS_FUNC(hb_memvarCountPublics)
{
  if (hb_memvarScopeGet(pDynSymbol) == HB_MV_PUBLIC)
  {
    (*(static_cast<int *>(Cargo)))++;
  }

  return true;
}

static HB_SIZE hb_memvarGetBaseOffset(int iProcLevel)
{
  HB_STACK_TLS_PRELOAD

  if (iProcLevel > 0)
  {
    int iLevel = hb_stackCallDepth();
    if (iProcLevel < iLevel)
    {
      HB_ISIZ nOffset = hb_stackBaseProcOffset(iLevel - iProcLevel - 1);
      if (nOffset > 0)
      {
        return hb_stackItem(nOffset)->symbolStackState()->nPrivateBase;
      }
    }
  }

  return hb_stackBaseItem()->symbolStackState()->nPrivateBase;
}

// Count the number of variables with given scope
static HB_ISIZ hb_memvarCount(int iScope, int iLevel)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCount(%d,%d)", iScope, iLevel));
#endif

  if (iScope == HB_MV_PUBLIC)
  {
    int iPublicCnt = 0;

    hb_dynsymProtectEval(hb_memvarCountPublics, static_cast<void *>(&iPublicCnt));
    return iPublicCnt;
  }
  else
  { // number of PRIVATE variables
    HB_STACK_TLS_PRELOAD

    if (iScope == HB_MV_PRIVATE_LOCAL)
    {
      return hb_stackGetPrivateStack()->count - hb_memvarGetBaseOffset(iLevel);
    }
    else if (iScope == HB_MV_PRIVATE_GLOBAL)
    {
      return hb_memvarGetBaseOffset(iLevel);
    }
    else
    {
      return hb_stackGetPrivateStack()->count;
    }
  }
}

// Checks passed dynamic symbol if it is a PUBLIC variable and returns
// a pointer to its dynamic symbol
static HB_DYNS_FUNC(hb_memvarFindPublicByPos)
{
  auto bCont = true;

  if (hb_memvarScopeGet(pDynSymbol) == HB_MV_PUBLIC)
  {
    auto pStruPub = static_cast<struct mv_PUBLIC_var_info *>(Cargo);
    if (pStruPub->iPos-- == 0)
    {
      pStruPub->bFound = true;
      pStruPub->pDynSym = pDynSymbol;
      bCont = false;
    }
  }

  return bCont;
}

// Returns the pointer to item that holds a value of variable (or nullptr if
// not found). It fills also the pointer to the variable name
// Both pointers points to existing and used data - they shouldn't be
// deallocated.
static PHB_ITEM hb_memvarDebugVariable(int iScope, int iPos, const char **pszName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDebugVariable(%d, %d, %p)", iScope, iPos, static_cast<const void*>(pszName)));
#endif

  PHB_ITEM pValue = nullptr;

  *pszName = nullptr;

  if (iPos > 0)
  {
    --iPos;
    if (iScope == HB_MV_PUBLIC)
    {
      struct mv_PUBLIC_var_info struPub;
      struPub.iPos = iPos;
      struPub.bFound = false;
      // enumerate existing dynamic symbols and fill this structure
      // with info for requested PUBLIC variable
      hb_dynsymProtectEval(hb_memvarFindPublicByPos, static_cast<void *>(&struPub));
      if (struPub.bFound)
      {
        pValue = hb_dynsymGetMemvar(struPub.pDynSym);
        *pszName = struPub.pDynSym->pSymbol->szName;
      }
    }
    else
    {
      HB_STACK_TLS_PRELOAD
      if (static_cast<HB_SIZE>(iPos) < hb_stackGetPrivateStack()->count)
      {
        PHB_DYNS pDynSym = hb_stackGetPrivateStack()->stack[iPos].pDynSym;

        pValue = hb_dynsymGetMemvar(pDynSym);
        *pszName = pDynSym->pSymbol->szName;
      }
    }
  }

  return pValue;
}

static HB_DYNS_FUNC(hb_memvarCountVisible)
{
  PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynSymbol);

  if (pMemvar != nullptr)
  {
    auto pMVInfo = static_cast<struct mv_memvarArray_info *>(Cargo);
    if (!pMVInfo->iScope || (hb_memvarScopeGet(pDynSymbol) & pMVInfo->iScope) != 0)
    {
      pMVInfo->pDyns[pMVInfo->nCount++] = pDynSymbol;
    }
  }
  return true;
}

PHB_ITEM hb_memvarSaveInArray(int iScope, HB_BOOL fCopy)
{
  HB_STACK_TLS_PRELOAD

  iScope &= HB_MV_PUBLIC | HB_MV_PRIVATE;
  if (iScope == (HB_MV_PUBLIC | HB_MV_PRIVATE))
  {
    iScope = 0;
  }

  struct mv_memvarArray_info MVInfo;
#if !defined(HB_MT_VM)
  MVInfo.pDyns = static_cast<PHB_DYNS *>(hb_xgrab(hb_dynsymCount() * sizeof(PHB_DYNS)));
#else
  MVInfo.pDyns = static_cast<PHB_DYNS *>(hb_xgrab(hb_stackDynHandlesCount() * sizeof(PHB_DYNS)));
#endif
  MVInfo.nCount = 0;
  MVInfo.iScope = iScope;

  hb_dynsymProtectEval(hb_memvarCountVisible, static_cast<void *>(&MVInfo));
  PHB_ITEM pArray = nullptr;
  if (MVInfo.nCount > 0)
  {
    pArray = hb_itemArrayNew(MVInfo.nCount);
    do
    {
      auto pItem = hb_arrayGetItemPtr(pArray, MVInfo.nCount);
      if (pItem != nullptr)
      {
        PHB_DYNS pDynSymbol = MVInfo.pDyns[--MVInfo.nCount];
        PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynSymbol);

        hb_arrayNew(pItem, 2);
        hb_arraySetSymbol(pItem, 1, pDynSymbol->pSymbol);
        pItem = hb_arrayGetItemPtr(pItem, 2);
        if (fCopy)
        {
          hb_itemCopy(pItem, pMemvar);
          hb_memvarDetachLocal(pItem);
        }
        else
        {
          pItem->setType(Harbour::Item::BYREF | Harbour::Item::MEMVAR);
          pItem->item.asMemvar.value = pMemvar;
          hb_xRefInc(pMemvar);
        }
      }
    } while (MVInfo.nCount);
  }
  hb_xfree(MVInfo.pDyns);

  return pArray;
}

void hb_memvarRestoreFromArray(PHB_ITEM pArray)
{
  const std::size_t nCount = hb_arrayLen(pArray);
  for (std::size_t nPos = 1; nPos <= nCount; ++nPos)
  {
    auto pItem = hb_arrayGetItemPtr(pArray, nPos);
    PHB_DYNS pDynSym = hb_arrayGetSymbol(pItem, 1)->pDynSym;
    PHB_ITEM pMemvar = hb_arrayGetItemPtr(pItem, 2)->item.asMemvar.value;
    hb_memvarValueIncRef(pMemvar);
    if (hb_dynsymGetMemvar(pDynSym))
    {
      hb_memvarDetachDynSym(pDynSym, pMemvar);
    }
    else
    {
      hb_dynsymSetMemvar(pDynSym, pMemvar);
    }
  }
}

// -

static const char *hb_memvarGetMask(int iParam)
{
  auto pszMask = hb_parc(iParam);

  if (!pszMask || pszMask[0] == '*')
  {
    pszMask = "*";
  }
  return pszMask;
}

HB_FUNC(__MVPUBLIC)
{
  HB_STACK_TLS_PRELOAD
  auto iCount = hb_pcount();

  if (iCount)
  {
    for (auto i = 1; i <= iCount; i++)
    {
      auto pMemvar = hb_param(i, Harbour::Item::ANY);

      if (pMemvar != nullptr)
      {
        if (pMemvar->isArray())
        {
          // we are accepting an one-dimensional array of strings only
          const std::size_t nLen = hb_arrayLen(pMemvar);

          for (std::size_t n = 1; n <= nLen; n++)
          {
            hb_memvarCreateFromItem(hb_arrayGetItemPtr(pMemvar, n), HB_VSCOMP_PUBLIC, nullptr);
          }
        }
        else
        {
          hb_memvarCreateFromItem(pMemvar, HB_VSCOMP_PUBLIC, nullptr);
        }
      }
    }
  }
}

HB_FUNC(__MVPRIVATE)
{
  HB_STACK_TLS_PRELOAD
  auto iCount = hb_pcount();

  if (iCount)
  {
    hb_memvarResetPrivatesBase();
    for (auto i = 1; i <= iCount; i++)
    {
      auto pMemvar = hb_param(i, Harbour::Item::ANY);

      if (pMemvar != nullptr)
      {
        if (pMemvar->isArray())
        {
          // we are accepting an one-dimensional array of strings only
          const std::size_t nLen = hb_arrayLen(pMemvar);

          for (std::size_t n = 1; n <= nLen; n++)
          {
            hb_memvarCreateFromItem(hb_arrayGetItemPtr(pMemvar, n), HB_VSCOMP_PRIVATE, nullptr);
          }
        }
        else
        {
          hb_memvarCreateFromItem(pMemvar, HB_VSCOMP_PRIVATE, nullptr);
        }
      }
    }
    hb_memvarUpdatePrivatesBase();
  }
}

HB_FUNC(__MVXRELEASE)
{
  HB_STACK_TLS_PRELOAD
  auto iCount = hb_pcount();

  if (iCount)
  {
    for (auto i = 1; i <= iCount; i++)
    {
      auto pMemvar = hb_param(i, Harbour::Item::ANY);

      if (pMemvar != nullptr)
      {
        if (pMemvar->isArray())
        {
          // we are accepting an one-dimensional array of strings only
          const std::size_t nLen = hb_arrayLen(pMemvar);

          for (std::size_t n = 1; n <= nLen; n++)
          {
            hb_memvarRelease(hb_arrayGetItemPtr(pMemvar, n));
          }
        }
        else
        {
          hb_memvarRelease(pMemvar);
        }
      }
    }
  }
}

HB_FUNC(__MVRELEASE)
{
  HB_STACK_TLS_PRELOAD
  auto iCount = hb_pcount();

  if (iCount && HB_ISCHAR(1))
  {
    const char *pszMask = hb_memvarGetMask(1);
    bool bIncludeVar = (pszMask[0] == '*' && !pszMask[1]) || iCount < 2 || hb_parl(2);
    hb_memvarReleaseWithMask(pszMask, bIncludeVar);
  }
}

HB_FUNC(__MVSCOPE)
{
  HB_STACK_TLS_PRELOAD
  int iMemvar = HB_MV_ERROR;

  if (hb_pcount())
  {
    auto pVarName = hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL);

    if (pVarName != nullptr)
    {
      auto pDynVar = hb_memvarGetSymbol(pVarName);

      if (pDynVar != nullptr)
      {
        iMemvar = hb_memvarScopeGet(pDynVar);
      }
      else
      {
        iMemvar = HB_MV_NOT_FOUND;
      }
    }
  }

  hb_retni(iMemvar);
}

HB_FUNC(__MVCLEAR)
{
  hb_memvarsClear(false);
}

HB_FUNC(__MVDBGINFO)
{
  HB_STACK_TLS_PRELOAD
  auto iCount = hb_pcount();

  if (iCount == 1 || iCount == 2)
  { // request for a number of variables
    hb_retns(hb_memvarCount(hb_parni(1), hb_parni(2)));
  }
  else if (iCount > 2)
  { // request for a value of variable
    const char *szName;

    PHB_ITEM pValue = hb_memvarDebugVariable(hb_parni(1), hb_parni(2), &szName);

    if (pValue != nullptr)
    { // the requested variable was found
      hb_storc(szName, 3);
      hb_itemCopyFromRef(hb_stackReturnItem(), pValue);
    }
    else
    {
      hb_ret(); // return NIL value
      hb_storc("?", 3);
    }
  }
}

HB_FUNC(__MVEXIST)
{
  HB_STACK_TLS_PRELOAD
  auto pDyn = hb_memvarGetSymbol(hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL));
  hb_retl(pDyn && hb_dynsymGetMemvar(pDyn));
}

HB_FUNC(__MVGET)
{
  auto pName = hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL);

  if (pName != nullptr)
  {
    HB_STACK_TLS_PRELOAD
    auto pDynVar = hb_memvarGetSymbol(pName);

    if (pDynVar != nullptr)
    {
      auto pValue = hb_stackAllocItem();
      hb_memvarGetValue(pValue, pDynVar->pSymbol);
      hb_itemReturnForward(pValue);
      hb_stackDec();
    }
    else
    {
      // Generate an error with retry possibility
      // (user created error handler can create this variable)
      auto pError =
          hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, 1003, nullptr,
                       pName->isString() ? pName->stringValue() : pName->symbolValue()->szName, 0, EF_CANRETRY);

      while (hb_errLaunch(pError) == E_RETRY)
      {
        pDynVar = hb_memvarGetSymbol(pName);
        if (pDynVar != nullptr)
        {
          auto pValue = hb_stackAllocItem();
          hb_memvarGetValue(pValue, pDynVar->pSymbol);
          hb_itemReturnForward(pValue);
          hb_stackDec();
          break;
        }
      }
      hb_errRelease(pError);
    }
  }
  else
  {
    // either the first parameter is not specified or it has a wrong type
    // (it must be a string)
    // This is not a critical error - we can continue normal processing
    hb_errRT_BASE_SubstR(EG_ARG, 3009, nullptr, nullptr, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(__MVGETDEF)
{
  auto pName = hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL);

  if (pName != nullptr)
  {
    HB_STACK_TLS_PRELOAD
    PHB_ITEM pMemvar;
    auto pDynVar = hb_memvarGetSymbol(pName);

    if (pDynVar && (pMemvar = hb_dynsymGetMemvar(pDynVar)) != nullptr)
    {
      hb_itemReturn(pMemvar->isByRef() ? hb_itemUnRef(pMemvar) : pMemvar);
    }
    else if (hb_pcount() >= 2)
    {
      hb_itemReturn(hb_param(2, Harbour::Item::ANY));
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3009, nullptr, nullptr, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(__MVPUT)
{
  auto pName = hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL);
  auto pValue = hb_paramError(2);

  if (pName != nullptr)
  {
    // the first parameter is a string with not empty variable name
    auto pDynVar = hb_memvarGetSymbol(pName);
    if (pDynVar != nullptr)
    {
      // variable was declared somewhere - assign a new value
      hb_memvarSetValue(pDynVar->pSymbol, pValue);
    }
    else
    {
      // attempt to assign a value to undeclared variable
      // create the PRIVATE one
      hb_memvarCreateFromDynSymbol(
          hb_dynsymGet(pName->isString() ? pName->stringValue() : pName->symbolValue()->szName),
          HB_VSCOMP_PRIVATE, pValue);
    }
    hb_memvarUpdatePrivatesBase();
    hb_itemReturn(pValue);
  }
  else
  {
    // either the first parameter is not specified or it has a wrong type
    // (it must be a string or symbol)
    // This is not a critical error - we can continue normal processing
    auto pRetValue = hb_errRT_BASE_Subst(EG_ARG, 3010, nullptr, nullptr, HB_ERR_ARGS_BASEPARAMS);
    if (pRetValue != nullptr)
    {
      hb_itemRelease(pRetValue);
    }
    hb_itemReturn(pValue);
  }
}

#define HB_MEM_REC_LEN 32
#define HB_MEM_NUM_LEN 8

struct MEMVARSAVE_CARGO
{
  const char *pszMask;
  HB_BOOL bIncludeMask; // TODO: bool
  HB_BYTE *buffer;
  PHB_FILE fhnd;
};

// saves a variable to a mem file already open

static HB_DYNS_FUNC(hb_memvarSave)
{
  const char *pszMask = (static_cast<MEMVARSAVE_CARGO *>(Cargo))->pszMask;
  bool bIncludeMask = (static_cast<MEMVARSAVE_CARGO *>(Cargo))->bIncludeMask;
  HB_BYTE *buffer = (static_cast<MEMVARSAVE_CARGO *>(Cargo))->buffer;
  PHB_FILE fhnd = (static_cast<MEMVARSAVE_CARGO *>(Cargo))->fhnd;

  // NOTE: Harbour name lengths are not limited, but the .mem file
  //       structure is not flexible enough to allow for it.
  //       [vszakats]

  PHB_ITEM pMemvar = hb_dynsymGetMemvar(pDynSymbol);
  if (pMemvar != nullptr)
  {
    bool bMatch = hb_strMatchCaseWildExact(pDynSymbol->pSymbol->szName, pszMask);

    // Process it if it matches the passed mask
    if (bIncludeMask ? bMatch : !bMatch)
    {
      // NOTE: Clipper will not initialize the record buffer with
      //       zeros, so they will look trashed. [vszakats]
      memset(buffer, 0, HB_MEM_REC_LEN);

      // NOTE: Save only the first 10 characters of the name
      hb_strncpy(reinterpret_cast<char *>(buffer), pDynSymbol->pSymbol->szName, 10);

      if (pMemvar->isString())
      {
        // Store the closing zero byte, too
        HB_SIZE nLen = pMemvar->getCLen() + 1;
        int iOverFlow = 0;

        // Clipper supports only 64 KiB strings
        if (nLen > USHRT_MAX)
        {
          nLen = USHRT_MAX;
          iOverFlow = 1;
        }
        buffer[11] = 'C' + 128;
        HB_PUT_LE_UINT16(&buffer[16], nLen);
        hb_fileWrite(fhnd, buffer, HB_MEM_REC_LEN, -1);
        hb_fileWrite(fhnd, pMemvar->getCPtr(), nLen - iOverFlow, -1);
        if (iOverFlow)
        {
          hb_fileWrite(fhnd, "\0", 1, -1);
        }
      }
      else if (pMemvar->isNumeric())
      {
        auto dNumber = pMemvar->getND();
        int iWidth;
        int iDec;
        hb_itemGetNLen(pMemvar, &iWidth, &iDec);
        buffer[11] = 'N' + 128;
#ifdef HB_CLP_STRICT
        // NOTE: This is the buggy, but fully CA-Cl*pper compatible method. [vszakats]
        buffer[16] = static_cast<HB_BYTE>(iWidth) + (pMemvar->isDouble() ? static_cast<HB_BYTE>(iDec + 1) : 0);
#else
        // NOTE: This would be the correct method, but Clipper is buggy here. [vszakats]
        buffer[16] = static_cast<HB_BYTE>(iWidth) + (iDec == 0 ? 0 : static_cast<HB_BYTE>(iDec + 1));
#endif
        buffer[17] = static_cast<HB_BYTE>(iDec);
        HB_PUT_LE_DOUBLE(&buffer[HB_MEM_REC_LEN], dNumber);
        hb_fileWrite(fhnd, buffer, HB_MEM_REC_LEN + HB_MEM_NUM_LEN, -1);
      }
      else if (pMemvar->isDate())
      {
        auto dNumber = static_cast<double>(pMemvar->getDL());
        buffer[11] = 'D' + 128;
        buffer[16] = 1;
        buffer[17] = 0;
        HB_PUT_LE_DOUBLE(&buffer[HB_MEM_REC_LEN], dNumber);
        hb_fileWrite(fhnd, buffer, HB_MEM_REC_LEN + HB_MEM_NUM_LEN, -1);
      }
      else if (pMemvar->isTimeStamp())
      {
        double dNumber = pMemvar->getTD();
        buffer[11] = 'T' + 128;
        buffer[16] = 1;
        buffer[17] = 0;
        HB_PUT_LE_DOUBLE(&buffer[HB_MEM_REC_LEN], dNumber);
        hb_fileWrite(fhnd, buffer, HB_MEM_REC_LEN + HB_MEM_NUM_LEN, -1);
      }
      else if (pMemvar->isLogical())
      {
        buffer[11] = 'L' + 128;
        buffer[16] = 1;
        buffer[17] = 0;
        buffer[HB_MEM_REC_LEN] = pMemvar->getL() ? 1 : 0;
        hb_fileWrite(fhnd, buffer, HB_MEM_REC_LEN + 1, -1);
      }
    }
  }
  return true;
}

HB_FUNC(__MVSAVE)
{
  HB_STACK_TLS_PRELOAD

  // Clipper also checks for the number of arguments here
  if (hb_pcount() == 3 && HB_ISCHAR(1) && HB_ISCHAR(2) && HB_ISLOG(3))
  {
    PHB_FILE fhnd;
    auto pszFileName = hb_parc(1);
    PHB_ITEM pError = nullptr;

    // Create .mem file
    do
    {
      fhnd = hb_fileExtOpen(pszFileName, hb_stackSetStruct()->HB_SET_DEFEXTENSIONS ? ".mem" : nullptr,
                            FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, pError);
      if (fhnd == nullptr)
      {
        pError = hb_errRT_FileError(pError, nullptr, EG_CREATE, 2006, pszFileName);
        if (hb_errLaunch(pError) != E_RETRY)
        {
          break;
        }
      }
    } while (fhnd == nullptr);

    if (fhnd != nullptr)
    {
      HB_BYTE buffer[HB_MEM_REC_LEN + HB_MEM_NUM_LEN];

      MEMVARSAVE_CARGO msc;
      msc.pszMask = hb_memvarGetMask(2);
      msc.bIncludeMask = hb_parl(3);
      msc.buffer = buffer;
      msc.fhnd = fhnd;

      // Walk through all visible memory variables and save each one

      hb_dynsymEval(hb_memvarSave, static_cast<void *>(&msc));

      buffer[0] = '\x1A';
      hb_fileWrite(fhnd, buffer, 1, -1);

      // NOTE: Here, we're not CA-Cl*pper compatible by default settings. [vszakats]
#ifndef HB_CLP_STRICT
      if (hb_setGetHardCommit())
      {
        hb_fileCommit(fhnd);
      }
#endif

      hb_fileClose(fhnd);
    }

    if (pError != nullptr)
    {
      hb_itemRelease(pError);
    }
  }
  else
  {
    // NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3b. [ckedem]
    hb_errRT_BASE(EG_ARG, 2008, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// NOTE: There's an extension in Harbour, which makes it possible to only
//       load (or not load) variable names with a specific name mask.
//       [vszakats]

HB_FUNC(__MVRESTORE)
{
  // Clipper checks for the number of arguments here here, but we cannot
  // in Harbour since we have two optional parameters as an extension.
#ifdef HB_CLP_STRICT
  if (hb_pcount() == 2 && HB_ISCHAR(1) && HB_ISLOG(2))
  {
#else
  if (HB_ISCHAR(1) && HB_ISLOG(2))
  {
#endif
    HB_STACK_TLS_PRELOAD

    bool bAdditive = hb_parl(2);

    // Clear all memory variables if not ADDITIVE

    if (!bAdditive)
    {
      hb_memvarsClear(false);
    }

    PHB_FILE fhnd;
    auto pszFileName = hb_parc(1);
    PHB_ITEM pError = nullptr;

    // Open .mem file
    do
    {
      fhnd = hb_fileExtOpen(pszFileName, hb_stackSetStruct()->HB_SET_DEFEXTENSIONS ? ".mem" : nullptr,
                            FO_READ | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, pError);
      if (fhnd == nullptr)
      {
        pError = hb_errRT_FileError(pError, nullptr, EG_OPEN, 2005, pszFileName);
        if (hb_errLaunch(pError) != E_RETRY)
        {
          break;
        }
      }
    } while (fhnd == nullptr);

    if (fhnd != nullptr)
    {
#ifdef HB_CLP_STRICT
      const char *pszMask = "*";
      auto bIncludeMask = true;
#else
      const char *pszMask = hb_memvarGetMask(3);
      bool bIncludeMask = hb_parldef(4, true);
#endif

      HB_BYTE buffer[HB_MEM_REC_LEN];
      PHB_ITEM pItem = nullptr;

      while (hb_fileRead(fhnd, buffer, HB_MEM_REC_LEN, -1) == HB_MEM_REC_LEN)
      {
        // FoxPro does not add 128 to item type: 'N', 'C', 'D', 'L'
        // CA-Cl*pper respects it and read such files so we also should.
        auto uiType = static_cast<HB_USHORT>(buffer[11] & 0x7f);
        auto uiWidth = static_cast<HB_USHORT>(buffer[16]);
        auto uiDec = static_cast<HB_USHORT>(buffer[17]);

        // protect against corrupted files
        buffer[10] = '\0';
        auto pszName = reinterpret_cast<char *>(buffer);

        switch (uiType)
        {
        case 'C':
        {
          uiWidth += uiDec * 256;
          auto pbyString = static_cast<HB_BYTE *>(hb_xgrab(uiWidth));

          if (hb_fileRead(fhnd, pbyString, uiWidth, -1) == static_cast<HB_SIZE>(uiWidth))
          {
            pItem = hb_itemPutCLPtr(pItem, reinterpret_cast<char *>(pbyString), uiWidth - 1);
          }
          else
          {
            hb_xfree(pbyString);
            pszName = nullptr;
          }

          break;
        }

        case 'N':
        {
          HB_BYTE pbyNumber[HB_MEM_NUM_LEN];

          if (hb_fileRead(fhnd, pbyNumber, HB_MEM_NUM_LEN, -1) == HB_MEM_NUM_LEN)
          {
            pItem = hb_itemPutNLen(pItem, HB_GET_LE_DOUBLE(pbyNumber), uiWidth - (uiDec ? (uiDec + 1) : 0), uiDec);
          }
          else
          {
            pszName = nullptr;
          }

          break;
        }

        case 'D':
        {
          HB_BYTE pbyNumber[HB_MEM_NUM_LEN];

          if (hb_fileRead(fhnd, pbyNumber, HB_MEM_NUM_LEN, -1) == HB_MEM_NUM_LEN)
          {
            pItem = hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_DOUBLE(pbyNumber)));
          }
          else
          {
            pszName = nullptr;
          }

          break;
        }

        case 'T':
        {
          HB_BYTE pbyNumber[HB_MEM_NUM_LEN];

          if (hb_fileRead(fhnd, pbyNumber, HB_MEM_NUM_LEN, -1) == HB_MEM_NUM_LEN)
          {
            pItem = hb_itemPutTD(pItem, HB_GET_LE_DOUBLE(pbyNumber));
          }
          else
          {
            pszName = nullptr;
          }

          break;
        }

        case 'L':
        {
          HB_BYTE pbyLogical[1];

          if (hb_fileRead(fhnd, pbyLogical, 1, -1) == 1)
          {
            pItem = hb_itemPutL(pItem, pbyLogical[0] != 0);
          }
          else
          {
            pszName = nullptr;
          }

          break;
        }

        default:
          pszName = nullptr;
        }

        if (pszName != nullptr)
        {
          bool bMatch = hb_strMatchCaseWildExact(pszName, pszMask);

          // Process it if it matches the passed mask
          if (bIncludeMask ? bMatch : !bMatch)
          {
            // the first parameter is a string with not empty variable name
            auto pDynVar = hb_memvarFindSymbol(pszName, strlen(pszName));

            if (pDynVar != nullptr)
            {
              // variable was declared somewhere - assign a new value
              hb_memvarSetValue(pDynVar->pSymbol, pItem);
            }
            else
            {
              // attempt to assign a value to undeclared variable create the PRIVATE one
              hb_memvarCreateFromDynSymbol(hb_dynsymGet(pszName), HB_VSCOMP_PRIVATE, pItem);
            }
          }
        }
      }

      hb_fileClose(fhnd);
      hb_memvarUpdatePrivatesBase();
      hb_itemReturnRelease(pItem);
    }
    else
    {
      hb_retl(false);
    }

    if (pError != nullptr)
    {
      hb_itemRelease(pError);
    }
  }
  else
  {
    // NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3b. [ckedem]
    hb_errRT_BASE(EG_ARG, 2007, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// This is a hacking function which changes base private offset so
// PRIVATE variables created in function which calls __mvSetBase()
// will not be released when the function exit but will be inherited
// by its caller. [druzus]
HB_FUNC(__MVSETBASE)
{
  HB_STACK_TLS_PRELOAD
  HB_ISIZ nOffset = hb_stackBaseProcOffset(0);

  if (nOffset > 0)
  {
    hb_stackItem(nOffset)->symbolStackState()->nPrivateBase = hb_memvarGetPrivatesBase();
  }
}

// debugger function
PHB_ITEM hb_memvarGetValueBySym(PHB_DYNS pDynSym)
{
  return hb_dynsymGetMemvar(pDynSym);
}
