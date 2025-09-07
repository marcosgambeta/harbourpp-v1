//
// Dynamic symbol table management
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbstack.hpp"

// Note for Harbour++ v2: use only std::mutex
#if defined(HB_USE_CPP_MUTEX)
#include <iostream>
#include <thread>
#include <mutex>
#endif

struct DYNHB_ITEM
{
  PHB_DYNS pDynSym; // Pointer to dynamic symbol
};

struct HB_SYM_HOLDER
{
  HB_SYMB symbol;
  struct HB_SYM_HOLDER *pNext;
  char szName[1];
};

#if defined(HB_USE_CPP_MUTEX)

std::mutex dynsMtx;

#define HB_DYNSYM_LOCK() dynsMtx.lock()
#define HB_DYNSYM_UNLOCK() dynsMtx.unlock()

#if defined(HB_MT_VM)
#define hb_dynsymHandles(p) hb_stackGetDynHandle(p)
#else
#define hb_dynsymHandles(p) (p)
#endif

#else

#if defined(HB_MT_VM)

#include "hbthread.hpp"

static HB_CRITICAL_NEW(s_dynsMtx);
#define HB_DYNSYM_LOCK() hb_threadEnterCriticalSection(&s_dynsMtx)
#define HB_DYNSYM_UNLOCK() hb_threadLeaveCriticalSection(&s_dynsMtx)

#define hb_dynsymHandles(p) hb_stackGetDynHandle(p)

#else

#define HB_DYNSYM_LOCK()                                                                                               \
  do {                                                                                                                 \
  } while (false)
#define HB_DYNSYM_UNLOCK()                                                                                             \
  do {                                                                                                                 \
  } while (false)

#define hb_dynsymHandles(p) (p)

#endif // HB_MT_VM

#endif // HB_USE_CPP_MUTEX

static DYNHB_ITEM *s_pDynItems = nullptr; // Pointer to dynamic items
static HB_SYMCNT s_uiDynSymbols = 0;      // Number of symbols present

static HB_SYM_HOLDER *s_pAllocSyms = nullptr; // symbols allocated dynamically

// table index for dynamic symbol to number conversions
static DYNHB_ITEM *s_pDynIndex = nullptr;
static HB_SYMCNT s_uiDynIdxSize = 0;

// Insert new symbol into dynamic symbol table.
// In MT mode caller should protected it by HB_DYNSYM_LOCK()
static PHB_DYNS hb_dynsymInsert(PHB_SYMB pSymbol, HB_SYMCNT uiPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymInsert(%p, %u)", static_cast<void*>(pSymbol), uiPos));
#endif

  if (++s_uiDynSymbols == 0) {
    --s_uiDynSymbols;
    hb_errInternal(6004, "Internal error: size of dynamic symbol table exceed", nullptr, nullptr);
  } else if (s_uiDynSymbols == 1) {
    s_pDynItems = static_cast<DYNHB_ITEM *>(hb_xgrab(sizeof(DYNHB_ITEM)));
  } else {
    s_pDynItems = static_cast<DYNHB_ITEM *>(hb_xrealloc(s_pDynItems, s_uiDynSymbols * sizeof(DYNHB_ITEM)));
    memmove(&s_pDynItems[uiPos + 1], &s_pDynItems[uiPos], sizeof(DYNHB_ITEM) * (s_uiDynSymbols - uiPos - 1));
  }

  auto pDynSym = static_cast<PHB_DYNS>(hb_xgrabz(sizeof(HB_DYNS)));
  pDynSym->pSymbol = pSymbol;
  pDynSym->uiSymNum = s_uiDynSymbols;

  pSymbol->pDynSym = s_pDynItems[uiPos].pDynSym = pDynSym;

  return pDynSym;
}

// Find symbol in dynamic symbol table and set it's position.
// If not found set position for insert operation.
// In MT mode caller should protected it by HB_DYNSYM_LOCK()
static PHB_DYNS hb_dynsymPos(const char *szName, HB_SYMCNT *puiPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymPos(%s, %p)", szName, static_cast<void*>(puiPos)));
#endif

  HB_SYMCNT uiFirst = 0;
  HB_SYMCNT uiLast = s_uiDynSymbols;
  HB_SYMCNT uiMiddle = uiLast >> 1;

  while (uiFirst < uiLast) {
    int iCmp = strcmp(s_pDynItems[uiMiddle].pDynSym->pSymbol->szName, szName);

    if (iCmp == 0) {
      *puiPos = uiMiddle;
      return s_pDynItems[uiMiddle].pDynSym;
    } else if (iCmp < 0) {
      uiLast = uiMiddle;
    } else { // if( iCmp > 0 )
      uiFirst = uiMiddle + 1;
    }
    uiMiddle = (uiFirst + uiLast) >> 1;
  }

  *puiPos = uiMiddle;

  return nullptr;
}

// Create new symbol.
// In MT mode caller should protected it by HB_DYNSYM_LOCK()
static PHB_SYMB hb_symbolAlloc(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_symbolAlloc(%s)", szName));
#endif

  auto iLen = static_cast<int>(strlen(szName));
  auto pHolder = static_cast<HB_SYM_HOLDER *>(hb_xgrab(sizeof(HB_SYM_HOLDER) + iLen));
  memcpy(pHolder->szName, szName, iLen + 1);
  pHolder->pNext = s_pAllocSyms;
  s_pAllocSyms = pHolder;

  pHolder->symbol.szName = pHolder->szName;
  pHolder->symbol.scope.value = 0;
  pHolder->symbol.value.pFunPtr = nullptr;
  pHolder->symbol.pDynSym = nullptr;

  return &pHolder->symbol;
}

// Find symbol in dynamic symbol table
PHB_DYNS hb_dynsymFind(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFind(%s)", szName));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiFirst = 0;
  HB_SYMCNT uiLast = s_uiDynSymbols;

  while (uiFirst < uiLast) {
    HB_SYMCNT uiMiddle = (uiFirst + uiLast) >> 1;
    int iCmp = strcmp(s_pDynItems[uiMiddle].pDynSym->pSymbol->szName, szName);

    if (iCmp == 0) {
      HB_DYNSYM_UNLOCK();
      return s_pDynItems[uiMiddle].pDynSym;
    } else if (iCmp < 0) {
      uiLast = uiMiddle;
    } else { // if( iCmp > 0 )
      uiFirst = uiMiddle + 1;
    }
  }

  HB_DYNSYM_UNLOCK();

  return nullptr;
}

// Create new symbol
PHB_SYMB hb_symbolNew(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_symbolNew(%s)", szName));
#endif

  PHB_SYMB pSymbol;

  HB_DYNSYM_LOCK();

  pSymbol = hb_symbolAlloc(szName);

  HB_DYNSYM_UNLOCK();

  return pSymbol;
}

// creates a new dynamic symbol
PHB_DYNS hb_dynsymNew(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymNew(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiPos;

  PHB_DYNS pDynSym = hb_dynsymPos(pSymbol->szName, &uiPos); // Find position
  if (!pDynSym) {
    pDynSym = hb_dynsymInsert(pSymbol, uiPos);
  } else {
    pSymbol->pDynSym = pDynSym;

    if ((pDynSym->pSymbol->scope.value & pSymbol->scope.value & HB_FS_LOCAL) != 0 && pDynSym->pSymbol != pSymbol) {
      // Someone is using linker which allows to create binaries
      // with multiple function definitions. It's a big chance that
      // wrong binaries are created in such case, f.e both functions
      // linked and not all references updated. Anyhow now we will
      // have to guess which symbol is the real local one [druzus]

      // Let's check if linker updated function address so both symbols
      // refer to the same function
      if (pDynSym->pSymbol->value.pFunPtr == pSymbol->value.pFunPtr) {
        // The addresses have been updated, f.e. in such way works GCC
        // in Linux (but not MinGW and DJGPP) if user allows to create
        // binaries with multiple symbols by
        //    -Wl,--allow-multiple-definition
        // when whole module cannot be cleanly replaced.
        // OpenWatcom for Linux, MS-DOS and Windows (I haven't tested OS2
        // version), POCC and XCC (with /FORCE:MULTIPLE) also update
        // addresses in such case.
        //
        // We are guessing that symbols are registered in reverted order
        // so we remove the HB_FS_LOCAL flag from previously registered
        // symbol but some linkers may use different order so it does
        // not have to be true in all cases
        pDynSym->pSymbol->scope.value &= ~HB_FS_LOCAL;
      } else {
        // We have multiple symbol with the same name which refer
        // to different public functions inside this single binary
        // Let's check if this symbol is loaded from dynamic library
        // (.so, .dll, .dyn, ...) or .hrb file
        if (pSymbol->scope.value & HB_FS_PCODEFUNC) {
          // It's dynamic module so we are guessing that HVM
          // intentionally not updated function address allowing
          // multiple functions, f.e. programmer asked about keeping
          // local references using hb_libLoad()/hb_hrbLoad() parameter.
          // In such case update pDynSym address in the new symbol but
          // do not register it as the main one
          HB_DYNSYM_UNLOCK();
          return pDynSym; // Return pointer to DynSym
        }
        // The multiple symbols comes from single binaries - we have to
        // decide what to do with them. We can leave it as is or we can
        // try to overload one symbol so both will point to the same
        // function. For .prg code such overloading will work but not
        // for C code which makes something like: HB_FUNC_EXEC( funcname );
        // In such case we cannot do anything - we cannot even detect
        // such situation. In some cases even linker cannot detect it
        // because C compiler can make auto-inlining or some bindings
        // which are not visible for linker

        // Let's try to overload one of the functions. Simple:
        //    pDynSym->pSymbol->value.pFunPtr = pSymbol->value.pFunPtr;
        // is not good idea because it's possible that this symbol will
        // be overloaded yet another time after preprocessing rest of
        // symbols so we will use HB_FS_DEFERRED flag which is updated
        // dynamically in hb_vmSend()/hb_vmDo() functions
#define HB_OVERLOAD_MULTIPLE_FUNC

#if defined(HB_OVERLOAD_MULTIPLE_FUNC)
        // In such way works MinGW, DJGPP, BCC
#if defined(__GNUC__)
        // MinGW (like most of other GCC ports) uses reverted order for
        // initialization functions
        pDynSym->pSymbol->scope.value &= ~HB_FS_LOCAL;
        pDynSym->pSymbol->scope.value |= HB_FS_DEFERRED;
#else
        // BCC, DJGPP, ...
        pSymbol->scope.value &= ~HB_FS_LOCAL;
        pSymbol->scope.value |= HB_FS_DEFERRED;
#endif
#endif
      }
    }

    if ((!pDynSym->pSymbol->value.pFunPtr && pSymbol->value.pFunPtr) || (pSymbol->scope.value & HB_FS_LOCAL) != 0) {
      pDynSym->pSymbol = pSymbol;
#ifndef HB_NO_PROFILER
      pDynSym->ulCalls = 0;
      pDynSym->ulTime = 0;
      pDynSym->ulRecurse = 0;
#endif
    }
  }

  HB_DYNSYM_UNLOCK();

  return pDynSym;
}

// finds and creates a symbol if not found
PHB_DYNS hb_dynsymGetCase(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGetCase(%s)", szName));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiPos;

  PHB_DYNS pDynSym = hb_dynsymPos(szName, &uiPos);
  if (!pDynSym) {
    pDynSym = hb_dynsymInsert(hb_symbolAlloc(szName), uiPos);
  }

  HB_DYNSYM_UNLOCK();

  return pDynSym;
}

PHB_DYNS hb_dynsymGet(const char *szName) // finds and creates a symbol if not found
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGet(%s)", szName));
#endif

  char szUprName[HB_SYMBOL_NAME_LEN + 1];

  // make a copy as we may get a const string, then turn it to uppercase
  // NOTE: This block is optimized for speed [vszakats]
  {
    int iLen = HB_SYMBOL_NAME_LEN;
    char *pDest = szUprName;

    do {
      char cChar = *szName++;
      if (cChar == 0 || cChar == ' ' || cChar == '\t') {
        break;
      } else if (cChar >= 'a' && cChar <= 'z') {
        *pDest++ = cChar - ('a' - 'A');
      } else {
        *pDest++ = cChar;
      }
    } while (--iLen);
    *pDest = '\0';
  }

  return hb_dynsymGetCase(szUprName);
}

PHB_DYNS hb_dynsymFindName(const char *szName) // finds a symbol
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFindName(%s)", szName));
#endif

  char szUprName[HB_SYMBOL_NAME_LEN + 1];

  // make a copy as we may get a const string, then turn it to uppercase
  // NOTE: This block is optimized for speed [vszakats]
  {
    int iLen = HB_SYMBOL_NAME_LEN;
    char *pDest = szUprName;

    do {
      char cChar = *szName++;
      if (cChar == 0 || cChar == ' ' || cChar == '\t') {
        break;
      } else if (cChar >= 'a' && cChar <= 'z') {
        *pDest++ = cChar - ('a' - 'A');
      } else {
        *pDest++ = cChar;
      }
    } while (--iLen);
    *pDest = '\0';
  }

  return hb_dynsymFind(szUprName);
}

PHB_SYMB hb_dynsymGetSymbol(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGetSymbol(%s)", szName));
#endif

  return hb_dynsymGet(szName)->pSymbol;
}

PHB_SYMB hb_dynsymFindSymbol(const char *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFindSymbol(%s)", szName));
#endif

  auto pDynSym = hb_dynsymFind(szName);
  return pDynSym ? pDynSym->pSymbol : nullptr;
}

PHB_SYMB hb_dynsymSymbol(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymSymbol(%p)", static_cast<void*>(pDynSym)));
#endif

  return pDynSym->pSymbol;
}

const char *hb_dynsymName(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymName(%p)", static_cast<void*>(pDynSym)));
#endif

  return pDynSym->pSymbol->szName;
}

HB_BOOL hb_dynsymIsFunction(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymIsFunction(%p)", static_cast<void*>(pDynSym)));
#endif

  return pDynSym->pSymbol->value.pFunPtr != nullptr;
}

HB_BOOL hb_dynsymIsMemvar(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymIsMemvar(%p)", static_cast<void*>(pDynSym)));
#endif

  return hb_dynsymHandles(pDynSym)->pMemvar != nullptr;
}

PHB_ITEM hb_dynsymGetMemvar(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymGetMemvar(%p)", static_cast<void*>(pDynSym)));
#endif

  return static_cast<PHB_ITEM>(hb_dynsymHandles(pDynSym)->pMemvar);
}

void hb_dynsymSetMemvar(PHB_DYNS pDynSym, PHB_ITEM pMemvar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymSetMemvar(%p, %p)", static_cast<void*>(pDynSym), static_cast<void*>(pMemvar)));
#endif

  hb_dynsymHandles(pDynSym)->pMemvar = static_cast<void *>(pMemvar);
}

int hb_dynsymAreaHandle(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymAreaHandle(%p)", static_cast<void*>(pDynSym)));
#endif

  return hb_dynsymHandles(pDynSym)->uiArea;
}

void hb_dynsymSetAreaHandle(PHB_DYNS pDynSym, int iArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymSetAreaHandle(%p, %d)", static_cast<void*>(pDynSym), iArea));
#endif

  hb_dynsymHandles(pDynSym)->uiArea = static_cast<HB_USHORT>(iArea);
}

static PHB_DYNS hb_dynsymGetByIndex(HB_LONG lIndex)
{
  HB_DYNSYM_LOCK();

  PHB_DYNS pDynSym = nullptr;

  if (lIndex >= 1 && static_cast<HB_ULONG>(lIndex) <= s_uiDynSymbols) {
    pDynSym = s_pDynItems[lIndex - 1].pDynSym;
  }

  HB_DYNSYM_UNLOCK();

  return pDynSym;
}

static PHB_DYNS hb_dynsymByItem(PHB_ITEM pItem)
{
  PHB_DYNS pDynSym = nullptr;

  if (pItem) {
    if (pItem->isString()) {
      pDynSym = hb_dynsymFindName(pItem->stringValue());
    } else if (pItem->isSymbol()) {
      pDynSym = pItem->symbolValue()->pDynSym;
      if (pDynSym == nullptr) {
        pDynSym = hb_dynsymFind(pItem->symbolValue()->szName);
      }
    } else if (pItem->isNumeric()) {
      pDynSym = hb_dynsymGetByIndex(pItem->getNL());
    }
  }
  return pDynSym;
}

HB_LONG hb_dynsymCount(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymCount()"));
#endif

  return s_uiDynSymbols;
}

HB_SYMCNT hb_dynsymToNum(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymToNum(%p)", static_cast<void*>(pDynSym)));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiSymNum = pDynSym->uiSymNum;

  if (uiSymNum > s_uiDynIdxSize) {
    s_pDynIndex = static_cast<DYNHB_ITEM *>(hb_xrealloc(s_pDynIndex, uiSymNum * sizeof(DYNHB_ITEM)));
    memset(&s_pDynIndex[s_uiDynIdxSize], 0, (uiSymNum - s_uiDynIdxSize) * sizeof(DYNHB_ITEM));
    s_uiDynIdxSize = uiSymNum;
  }

  if (s_pDynIndex[uiSymNum - 1].pDynSym == nullptr) {
    s_pDynIndex[uiSymNum - 1].pDynSym = pDynSym;
  }

  HB_DYNSYM_UNLOCK();

  return uiSymNum;
}

PHB_DYNS hb_dynsymFromNum(HB_SYMCNT uiSymNum)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymFromNum(%d)", uiSymNum));
#endif

  HB_DYNSYM_LOCK();

  PHB_DYNS pDynSym = uiSymNum > 0 && uiSymNum <= s_uiDynIdxSize ? s_pDynIndex[uiSymNum - 1].pDynSym : nullptr;

  HB_DYNSYM_UNLOCK();

  return pDynSym;
}

void hb_dynsymEval(PHB_DYNS_FUNC pFunction, void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymEval(%p, %p)", reinterpret_cast<void*>(pFunction), Cargo));
#endif

  PHB_DYNS pDynSym = nullptr;
  HB_SYMCNT uiPos = 0;

  for (;;) {

    HB_DYNSYM_LOCK();

    if (pDynSym) {
      // protection against resizing dynamic symbol by
      // user function or other thread in MT mode
      while (s_pDynItems[uiPos].pDynSym != pDynSym) {
        if (++uiPos >= s_uiDynSymbols) {
          break;
        }
      }
    }
    if (++uiPos < s_uiDynSymbols) {
      pDynSym = s_pDynItems[uiPos].pDynSym;
    } else {
      pDynSym = nullptr;
    }

    HB_DYNSYM_UNLOCK();

    if (!pDynSym || !(pFunction)(pDynSym, Cargo)) {
      break;
    }
  }
}

void hb_dynsymProtectEval(PHB_DYNS_FUNC pFunction, void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymProtectEval(%p, %p)", reinterpret_cast<void*>(pFunction), Cargo));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiPos = 0;

  while (uiPos < s_uiDynSymbols) {
    if (!(pFunction)(s_pDynItems[uiPos++].pDynSym, Cargo)) {
      break;
    }
  }

  HB_DYNSYM_UNLOCK();
}

void hb_dynsymRelease(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymRelease()"));
#endif

  HB_DYNSYM_LOCK();

  if (s_uiDynIdxSize) {
    hb_xfree(s_pDynIndex);
    s_pDynIndex = nullptr;
    s_uiDynIdxSize = 0;
  }

  if (s_uiDynSymbols) {
    do {
      hb_xfree((s_pDynItems + --s_uiDynSymbols)->pDynSym);
    } while (s_uiDynSymbols);
    hb_xfree(s_pDynItems);
    s_pDynItems = nullptr;
  }

  while (s_pAllocSyms) {
    HB_SYM_HOLDER *pHolder = s_pAllocSyms;
    s_pAllocSyms = s_pAllocSyms->pNext;
    hb_xfree(pHolder);
  }

  HB_DYNSYM_UNLOCK();
}

HB_FUNC(__DYNSCOUNT) // How much symbols do we have: dsCount = __dynsymCount()
{
  HB_STACK_TLS_PRELOAD
  hb_retnint(s_uiDynSymbols);
}

HB_FUNC(__DYNSGETNAME) // Get name of symbol: cSymbol = __dynsymGetName(dsIndex)
{
  HB_STACK_TLS_PRELOAD
  auto pDynSym = hb_dynsymGetByIndex(hb_parnl(1));
  hb_retc(pDynSym ? pDynSym->pSymbol->szName : nullptr);
}

HB_FUNC(__DYNSGETINDEX) // Gimme index number of symbol: dsIndex = __dynsymGetIndex( cSymbol | sSymbol )
{
  HB_STACK_TLS_PRELOAD
  HB_SYMCNT uiPos = 0;
  PHB_DYNS pDynSym = hb_dynsymByItem(hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL));

  if (pDynSym) {
    HB_DYNSYM_LOCK();
    if (hb_dynsymPos(pDynSym->pSymbol->szName, &uiPos)) {
      ++uiPos;
    } else {
      uiPos = 0;
    }
    HB_DYNSYM_UNLOCK();
  }
  hb_retnint(uiPos);
}

HB_FUNC(HB_ISFUNCTION) // returns .T. if a symbol has a function/procedure pointer, given its symbol or name
{
  HB_STACK_TLS_PRELOAD
  PHB_DYNS pDynSym = hb_dynsymByItem(hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL));

  hb_retl(pDynSym && hb_dynsymIsFunction(pDynSym));
}

HB_FUNC(__DYNSISFUN) // returns .T. if a symbol has a function/procedure pointer, given its symbol index or name
{
  HB_STACK_TLS_PRELOAD
  PHB_DYNS pDynSym =
      hb_dynsymByItem(hb_param(1, Harbour::Item::STRING | Harbour::Item::SYMBOL | Harbour::Item::NUMERIC));

  hb_retl(pDynSym && hb_dynsymIsFunction(pDynSym));
}

HB_FUNC(__DYNSGETPRF) // profiler: It returns an array with a function or procedure called and consumed times { nTimes,
                      // nTime }, given the dynamic symbol index
{
  HB_STACK_TLS_PRELOAD
#ifndef HB_NO_PROFILER
  auto pDynSym = hb_dynsymGetByIndex(hb_parnl(1));
#endif

  hb_reta(2);
  hb_storvnl(0, -1, 1);
  hb_storvnl(0, -1, 2);

#ifndef HB_NO_PROFILER
  if (pDynSym) {
    if (hb_dynsymIsFunction(pDynSym)) { // it is a function or procedure
      hb_storvnl(pDynSym->ulCalls, -1, 1);
      hb_storvnl(pDynSym->ulTime, -1, 2);
    }
  }
#endif
}

HB_FUNC(__DYNSN2SYM)
{
  HB_STACK_TLS_PRELOAD
  auto szName = hb_parc(1);

  if (szName != nullptr) {
    hb_itemPutSymbol(hb_stackReturnItem(), hb_dynsymGet(szName)->pSymbol);
  }
}

HB_FUNC(__DYNSN2PTR)
{
  HB_STACK_TLS_PRELOAD
  auto szName = hb_parc(1);
  hb_retptr(szName ? hb_dynsymGet(szName) : nullptr);
}

HB_FUNC(__DYNSP2NAME)
{
  HB_STACK_TLS_PRELOAD
  auto pDynSym = static_cast<PHB_DYNS>(hb_parptr(1));
  hb_retc(pDynSym != nullptr ? pDynSym->pSymbol->szName : nullptr);
}

// internal function used to debug dynamic symbol integrity
static int hb_dynsymVerify(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dynsymVerify()"));
#endif

  HB_DYNSYM_LOCK();

  HB_SYMCNT uiPos = 0;
  int iResult = 0;

  while (iResult == 0 && uiPos < s_uiDynSymbols) {
    PHB_DYNS pDynSym = s_pDynItems[uiPos].pDynSym;
    HB_SYMCNT uiAt;
    int iCmp;

    if (uiPos > 0 && (iCmp = strcmp(s_pDynItems[uiPos - 1].pDynSym->pSymbol->szName, pDynSym->pSymbol->szName)) <= 0) {
      iResult = iCmp == 0 ? -1 : -2;
    } else if (hb_dynsymPos(pDynSym->pSymbol->szName, &uiAt) != pDynSym) {
      iResult = -3;
    } else if (uiAt != uiPos) {
      iResult = -4;
    } else {
      ++uiPos;
    }
  }

  HB_DYNSYM_UNLOCK();

  return iResult;
}

HB_FUNC(__DYNSVERIFY)
{
  HB_STACK_TLS_PRELOAD
  hb_retni(hb_dynsymVerify());
}
