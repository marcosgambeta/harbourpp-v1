//
// The eval stack management functions
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

#define HB_STACK_PRELOAD

// NOTE: Need to have these before Harbour headers,
//       because in MT mode, they will automatically #include <os2.h>.
#define INCL_DOSPROCESS

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapicls.hpp"
#include "hbstack.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapirdd.hpp"
#include "hbdate.hpp"

#if defined(HB_USE_CPP_MUTEX)
#include <iostream>
#include <thread>
#include <mutex>
#endif

// ---

#if !defined(STACK_INITHB_ITEMS)
#define STACK_INITHB_ITEMS 200
#endif
#if !defined(STACK_EXPANDHB_ITEMS)
#define STACK_EXPANDHB_ITEMS 20
#endif

// ---

#if defined(HB_MT_VM)

#include "hbthread.hpp"

#if defined(HB_USE_CPP_MUTEX)
std::mutex TSD_counter;
#else
static HB_CRITICAL_NEW(TSD_counter);
#endif
static int s_iTSDCounter = 0;

#ifdef HB_USE_TLS

// compiler has native support for TLS
#if !defined(_HB_STACK_MACROS_)
#if defined(__BORLANDC__)
static PHB_STACK HB_TLS_ATTR hb_stack_ptr;
#else
static HB_TLS_ATTR PHB_STACK hb_stack_ptr;
#endif
#elif !defined(_HB_STACK_LOCAL_MACROS_)
#if defined(__BORLANDC__)
PHB_STACK HB_TLS_ATTR hb_stack_ptr = nullptr;
#else
HB_TLS_ATTR PHB_STACK hb_stack_ptr = nullptr;
#endif
#endif

#define hb_stack_alloc()                                                                                               \
  do {                                                                                                                 \
    hb_stack_ptr = static_cast<PHB_STACK>(hb_xgrab(sizeof(HB_STACK)));                                                 \
  } while (false)
#define hb_stack_dealloc()                                                                                             \
  do {                                                                                                                 \
    hb_xfree(hb_stack_ptr);                                                                                            \
    hb_stack_ptr = nullptr;                                                                                            \
  } while (false)
#define hb_stack_ready() (hb_stack_ptr != nullptr)

#else

// compiler has no native TLS support, we have to implement it ourselves
#if !defined(_HB_STACK_MACROS_)
static HB_TLS_KEY hb_stack_key;
#define hb_stack_ptr (static_cast<PHB_STACK>(hb_tls_get(hb_stack_key)))
#elif !defined(_HB_STACK_LOCAL_MACROS_)
HB_TLS_KEY hb_stack_key;
#endif
static volatile auto s_fInited = false;
#define hb_stack_alloc()                                                                                               \
  do {                                                                                                                 \
    if (!s_fInited) {                                                                                                  \
      hb_tls_init(hb_stack_key);                                                                                       \
      s_fInited = true;                                                                                                \
    }                                                                                                                  \
    hb_tls_set(hb_stack_key, hb_xgrab(sizeof(HB_STACK)));                                                              \
  } while (false)
#define hb_stack_dealloc()                                                                                             \
  do {                                                                                                                 \
    hb_xfree(static_cast<void *>(hb_tls_get(hb_stack_key)));                                                           \
    hb_tls_set(hb_stack_key, nullptr);                                                                                 \
  } while (false)
#define hb_stack_ready() (s_fInited && hb_tls_get(hb_stack_key))

#endif // HB_USE_TLS

#if !defined(HB_STACK_PRELOAD)
#undef hb_stack
#define hb_stack (*hb_stack_ptr)
#endif

#else

// no MT mode
#if !defined(_HB_STACK_MACROS_)
static HB_STACK hb_stack;
#elif !defined(_HB_STACK_LOCAL_MACROS_)
HB_STACK hb_stack;
#endif

#define hb_stack_alloc()
#define hb_stack_dealloc()
#define hb_stack_ready() (true)

#endif // HB_MT_VM

// ---

static char s_szDirBuffer[HB_PATH_MAX];
static HB_IOERRORS s_IOErrors;
static HB_TRACEINFO s_traceInfo;

// ---

static HB_SYMB s_initSymbol = {"hb_stackInit", {HB_FS_STATIC}, {nullptr}, nullptr};

// ---

static void hb_stack_init(PHB_STACK pStack)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stack_init(%p)", static_cast<void*>(pStack)));
#endif

  memset(pStack, 0, sizeof(HB_STACK));

  pStack->pItems = static_cast<PHB_ITEM *>(hb_xgrab(sizeof(PHB_ITEM) * STACK_INITHB_ITEMS));
  pStack->pBase = pStack->pItems;
  pStack->pPos = pStack->pItems; // points to the first stack item
  pStack->nItems = STACK_INITHB_ITEMS;
  pStack->pEnd = pStack->pItems + pStack->nItems;

  for (HB_ISIZ n = 0; n < pStack->nItems; ++n) {
    pStack->pItems[n] = static_cast<PHB_ITEM>(hb_xgrab(sizeof(HB_ITEM)));
    pStack->pItems[n]->setType(Harbour::Item::NIL);
  }

  pStack->pPos++;
  hb_itemPutSymbol(*pStack->pItems, &s_initSymbol);
  (*pStack->pItems)->setSymbolStackState(&pStack->state);

  pStack->rdd.uiCurrArea = 1;
  pStack->iKeyPoll = 1;
}

static void hb_stack_destroy_TSD(PHB_STACK pStack)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stack_destroy_TSD(%p)", static_cast<void*>(pStack)));
#endif

  while (pStack->iTSD) {
    if (pStack->pTSD[pStack->iTSD].pTSD) {
      if (pStack->pTSD[pStack->iTSD].pTSD->pCleanFunc) {
        pStack->pTSD[pStack->iTSD].pTSD->pCleanFunc(pStack->pTSD[pStack->iTSD].value);
      }
      hb_xfree(pStack->pTSD[pStack->iTSD].value);
#if !defined(HB_MT_VM)
      pStack->pTSD[pStack->iTSD].pTSD->iHandle = 0;
#endif
    }
    if (--pStack->iTSD == 0) {
      hb_xfree(pStack->pTSD);
      pStack->pTSD = nullptr;
    }
  }
}

static void hb_stack_free(PHB_STACK pStack)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stack_free(%p)", static_cast<void*>(pStack)));
#endif

  hb_stack_destroy_TSD(pStack);

  if (pStack->privates.stack) {
    hb_xfree(pStack->privates.stack);
    pStack->privates.stack = nullptr;
    pStack->privates.size = pStack->privates.count = pStack->privates.base = 0;
  }
  HB_ISIZ n = pStack->nItems - 1;
  while (n >= 0) {
    hb_xfree(pStack->pItems[n--]);
  }
  hb_xfree(pStack->pItems);
  pStack->pItems = pStack->pPos = pStack->pBase = nullptr;
  pStack->nItems = 0;
#if defined(HB_MT_VM)
  if (pStack->pDirBuffer) {
    hb_xfree(pStack->pDirBuffer);
    pStack->pDirBuffer = nullptr;
  }
  if (pStack->uiDynH) {
    hb_xfree(pStack->pDynH);
    pStack->pDynH = nullptr;
    pStack->uiDynH = 0;
  }
#endif
}

void hb_stackDestroyTSD(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDestroyTSD()"));
#endif

  HB_STACK_TLS_PRELOAD

  hb_stack_destroy_TSD(&hb_stack);
}

void *hb_stackGetTSD(PHB_TSD pTSD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackGetTSD(%p)", static_cast<void*>(pTSD)));
#endif

  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  if (pTSD->iHandle == 0 || pTSD->iHandle > hb_stack.iTSD || hb_stack.pTSD[pTSD->iHandle].pTSD == nullptr) {
    if (pTSD->iHandle == 0) {
#if defined(HB_USE_CPP_MUTEX)
      TSD_counter.lock();
#else
      hb_threadEnterCriticalSection(&TSD_counter);
#endif
      // repeated test protected by mutex to avoid race condition
      if (pTSD->iHandle == 0) {
        pTSD->iHandle = ++s_iTSDCounter;
      }
#if defined(HB_USE_CPP_MUTEX)
      TSD_counter.unlock();
#else
      hb_threadLeaveCriticalSection(&TSD_counter);
#endif
    }

    if (pTSD->iHandle > hb_stack.iTSD) {
      hb_stack.pTSD =
          static_cast<PHB_TSD_HOLDER>(hb_xrealloc(hb_stack.pTSD, (pTSD->iHandle + 1) * sizeof(HB_TSD_HOLDER)));
      memset(&hb_stack.pTSD[hb_stack.iTSD + 1], 0, (pTSD->iHandle - hb_stack.iTSD) * sizeof(HB_TSD_HOLDER));
      hb_stack.iTSD = pTSD->iHandle;
    }
#else
  if (pTSD->iHandle == 0) {
    HB_SIZE nSize = (hb_stack.iTSD + 2) * sizeof(HB_TSD_HOLDER);
    if (hb_stack.iTSD == 0) {
      hb_stack.pTSD = static_cast<PHB_TSD_HOLDER>(hb_xgrabz(nSize));
    } else {
      hb_stack.pTSD = static_cast<PHB_TSD_HOLDER>(hb_xrealloc(hb_stack.pTSD, nSize));
    }
    pTSD->iHandle = ++hb_stack.iTSD;
#endif

    hb_stack.pTSD[pTSD->iHandle].pTSD = pTSD;
    hb_stack.pTSD[pTSD->iHandle].value = hb_xgrabz(pTSD->iSize);
    if (pTSD->pInitFunc) {
      pTSD->pInitFunc(hb_stack.pTSD[pTSD->iHandle].value);
    }
  }
  return hb_stack.pTSD[pTSD->iHandle].value;
}

void *hb_stackTestTSD(PHB_TSD pTSD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackTestTSD(%p)", static_cast<void*>(pTSD)));
#endif

  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  return (pTSD->iHandle && pTSD->iHandle <= hb_stack.iTSD) ? hb_stack.pTSD[pTSD->iHandle].value : nullptr;
#else
  return pTSD->iHandle ? hb_stack.pTSD[pTSD->iHandle].value : nullptr;
#endif
}

void hb_stackReleaseTSD(PHB_TSD pTSD)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackReleaseTSD(%p)", static_cast<void*>(pTSD)));
#endif

  HB_STACK_TLS_PRELOAD

  if (pTSD->iHandle && pTSD->iHandle <= hb_stack.iTSD && hb_stack.pTSD[pTSD->iHandle].value) {
    if (pTSD->pCleanFunc) {
      pTSD->pCleanFunc(hb_stack.pTSD[pTSD->iHandle].value);
    }
    hb_xfree(hb_stack.pTSD[pTSD->iHandle].value);
    hb_stack.pTSD[pTSD->iHandle].value = nullptr;
    hb_stack.pTSD[pTSD->iHandle].pTSD = nullptr;
    pTSD->iHandle = 0;
    // TODO: add recovery system to not lose TSD handles and
    //       make this functionality more general and public
    //       for 3rd party developers
  }
}

void hb_stackInit(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackInit()"));
#endif

#if defined(HB_MT_VM)
  hb_stack_alloc();
#endif
  {
    HB_STACK_TLS_PRELOAD
    hb_stack_init(&hb_stack);
    hb_xinit_thread();
  }
}

void hb_stackFree(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackFree()"));
#endif

  HB_STACK_TLS_PRELOAD

  hb_stack_free(&hb_stack);
  hb_xexit_thread();
#if defined(HB_MT_VM)
  hb_stack_dealloc();
#endif
}

#if defined(HB_MT_VM)

#undef hb_stackList
void *hb_stackList(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackList()"));
#endif

  HB_STACK_TLS_PRELOAD
  return hb_stack.pStackLst;
}

#undef hb_stackListSet
void hb_stackListSet(void *pStackLst)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackListSet(%p)", pStackLst));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stack.pStackLst = pStackLst;
}

#undef hb_stackIdSetActionRequest
void hb_stackIdSetActionRequest(void *pStackId, HB_USHORT uiAction)
{
  (static_cast<PHB_STACK>(pStackId))->uiActionRequest = uiAction;
}

#undef hb_stackDynHandlesCount
HB_SYMCNT hb_stackDynHandlesCount(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDynHandlesCount()"));
#endif

  HB_STACK_TLS_PRELOAD
  return hb_stack.uiDynH;
}

PHB_DYN_HANDLES hb_stackGetDynHandle(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackGetDynHandle()"));
#endif

  HB_STACK_TLS_PRELOAD

  HB_SYMCNT uiDynSym = pDynSym->uiSymNum;
  if (uiDynSym > hb_stack.uiDynH) {
    hb_stack.pDynH = static_cast<PHB_DYN_HANDLES>(hb_xrealloc(hb_stack.pDynH, uiDynSym * sizeof(HB_DYN_HANDLES)));
    memset(&hb_stack.pDynH[hb_stack.uiDynH], 0, (uiDynSym - hb_stack.uiDynH) * sizeof(HB_DYN_HANDLES));
    hb_stack.uiDynH = uiDynSym;
  }

  return &hb_stack.pDynH[uiDynSym - 1];
}

void hb_stackClearMemvars(HB_SYMCNT uiExcept)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackClearMemvars(%d)", uiExcept));
#endif

  HB_STACK_TLS_PRELOAD

  HB_SYMCNT uiDynSym = hb_stack.uiDynH;
  while (uiDynSym > 0) {
    if (uiDynSym-- != uiExcept) {
      if (hb_stack.pDynH[uiDynSym].pMemvar) {
        auto pMemvar = static_cast<PHB_ITEM>(hb_stack.pDynH[uiDynSym].pMemvar);
        hb_stack.pDynH[uiDynSym].pMemvar = nullptr;
        hb_memvarValueDecRef(pMemvar);
      }
    }
  }
}

#undef hb_stackQuitState
HB_BOOL hb_stackQuitState(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.uiQuitState != 0;
}

#undef hb_stackSetQuitState
void hb_stackSetQuitState(HB_USHORT uiState)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.uiQuitState = uiState;
}

#undef hb_stackUnlock
int hb_stackUnlock(void)
{
  HB_STACK_TLS_PRELOAD
  return ++hb_stack.iUnlocked;
}

#undef hb_stackLock
int hb_stackLock(void)
{
  HB_STACK_TLS_PRELOAD
  return --hb_stack.iUnlocked;
}

#undef hb_stackLockCount
int hb_stackLockCount(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.iUnlocked;
}

#endif // HB_MT_VM

#undef hb_stackKeyPolls
int *hb_stackKeyPolls(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackKeyPolls()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.iKeyPoll;
}

#undef hb_stackDebugRequest
HB_BOOL *hb_stackDebugRequest(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDebugRequest()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.fDebugRequest;
}

#undef hb_stackDebugInfo
void **hb_stackDebugInfo(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDebugInfo()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.pDebugInfo;
}

#undef hb_stackGetPrivateStack
PHB_PRIVATE_STACK hb_stackGetPrivateStack(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackGetPrivateStack()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.privates;
}

#undef hb_stackSetStruct
PHB_SET_STRUCT hb_stackSetStruct(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackSetStruct()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.set;
}

#undef hb_stackId
void *hb_stackId(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackId()"));
#endif

#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return static_cast<void *>(&hb_stack);
  } else {
    return nullptr;
  }
#else
  {
    HB_STACK_TLS_PRELOAD
    return static_cast<void *>(&hb_stack);
  }
#endif
}

#undef hb_stackPop
void hb_stackPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPop()"));
#endif

  HB_STACK_TLS_PRELOAD

  if (--hb_stack.pPos <= hb_stack.pBase) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  if ((*hb_stack.pPos)->isComplex()) {
    (*hb_stack.pPos)->clear();
  }
}

#undef hb_stackPopReturn
void hb_stackPopReturn(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPopReturn()"));
#endif

  HB_STACK_TLS_PRELOAD

  if ((&hb_stack.Return)->isComplex()) {
    (&hb_stack.Return)->clear();
  }

  if (--hb_stack.pPos <= hb_stack.pBase) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  hb_itemRawMove(&hb_stack.Return, *hb_stack.pPos);
}

#undef hb_stackDec
void hb_stackDec(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDec()"));
#endif

  HB_STACK_TLS_PRELOAD

  if (--hb_stack.pPos <= hb_stack.pBase) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }
}

#undef hb_stackDecrease
void hb_stackDecrease(HB_SIZE nItems)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDecrease()"));
#endif

  HB_STACK_TLS_PRELOAD

  if ((hb_stack.pPos -= nItems) <= hb_stack.pBase) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }
}

#undef hb_stackPush
void hb_stackPush(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPush()"));
#endif

  HB_STACK_TLS_PRELOAD

  // enough room for another item ?
  if (++hb_stack.pPos == hb_stack.pEnd) {
    hb_stackIncrease();
  }
}

#undef hb_stackAllocItem
PHB_ITEM hb_stackAllocItem(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackAllocItem()"));
#endif

  HB_STACK_TLS_PRELOAD

  if (++hb_stack.pPos == hb_stack.pEnd) {
    hb_stackIncrease();
  }

  (*(hb_stack.pPos - 1))->setType(Harbour::Item::NIL);

  return *(hb_stack.pPos - 1);
}

#undef hb_stackPushReturn
void hb_stackPushReturn(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPushReturn()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemRawMove(*hb_stack.pPos, &hb_stack.Return);

  // enough room for another item ?
  if (++hb_stack.pPos == hb_stack.pEnd) {
    hb_stackIncrease();
  }
}

void hb_stackIncrease(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackIncrease()"));
#endif

  HB_STACK_TLS_PRELOAD

  HB_ISIZ nBaseIndex = hb_stack.pBase - hb_stack.pItems; // index of stack base
  HB_ISIZ nCurrIndex = hb_stack.pPos - hb_stack.pItems;  // index of current top item
  HB_ISIZ nEndIndex = hb_stack.pEnd - hb_stack.pItems;   // index of current top item

  // no, make more headroom:
  hb_stack.pItems = static_cast<PHB_ITEM *>(
      hb_xrealloc(static_cast<void *>(hb_stack.pItems), sizeof(PHB_ITEM) * (hb_stack.nItems + STACK_EXPANDHB_ITEMS)));

  // fix possibly modified by realloc pointers:
  hb_stack.pPos = hb_stack.pItems + nCurrIndex;
  hb_stack.pBase = hb_stack.pItems + nBaseIndex;
  hb_stack.nItems += STACK_EXPANDHB_ITEMS;
  hb_stack.pEnd = hb_stack.pItems + hb_stack.nItems;

  do {
    hb_stack.pItems[nEndIndex] = static_cast<PHB_ITEM>(hb_xgrab(sizeof(HB_ITEM)));
    hb_stack.pItems[nEndIndex]->setType(Harbour::Item::NIL);
  } while (++nEndIndex < hb_stack.nItems);
}

void hb_stackRemove(HB_ISIZ nUntilPos)
{
  HB_STACK_TLS_PRELOAD
  PHB_ITEM *pEnd = hb_stack.pItems + nUntilPos;

  while (hb_stack.pPos > pEnd) {
    --hb_stack.pPos;
    if ((*hb_stack.pPos)->isComplex()) {
      (*hb_stack.pPos)->clear();
    }
  }
}

#if defined(HB_VM_DEBUG)

static void hb_stackDispLocal(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispLocal()"));
#endif

  char buffer[1024];
  HB_STACK_TLS_PRELOAD

  hb_conOutErr(hb_conNewLine(), 0);
  hb_snprintf(buffer, sizeof(buffer), HB_I_("Virtual Machine Stack Dump at %s(%i):"),
              (*hb_stack.pBase)->symbolValue()->szName, (*hb_stack.pBase)->symbolStackState()->uiLineNo);
  hb_conOutErr(buffer, 0);
  hb_conOutErr(hb_conNewLine(), 0);
  hb_conOutErr("--------------------------", 0);

  for (PHB_ITEM *pBase = hb_stack.pBase; pBase <= hb_stack.pPos; pBase++) {
    hb_conOutErr(hb_conNewLine(), 0);

    switch (hb_itemType(*pBase)) {
    case Harbour::Item::NIL:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("NIL "));
      break;

    case Harbour::Item::ARRAY:
      if (hb_arrayIsObject(*pBase)) {
        hb_snprintf(buffer, sizeof(buffer), HB_I_("OBJECT = %s "), hb_objGetClsName(*pBase));
      } else {
        hb_snprintf(buffer, sizeof(buffer), HB_I_("ARRAY "));
      }
      break;

    case Harbour::Item::BLOCK:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("BLOCK "));
      break;

    case Harbour::Item::DATE: {
      char szDate[9];
      hb_snprintf(buffer, sizeof(buffer), HB_I_("DATE = \"%s\" "), (*pBase)->getDS(szDate));
    } break;

    case Harbour::Item::TIMESTAMP: {
      char szDateTime[24];
      hb_snprintf(buffer, sizeof(buffer), HB_I_("TIMESTAMP = \"%s\" "),
                  hb_timeStampStr(szDateTime, (*pBase)->dateTimeJulian(), (*pBase)->dateTimeTime()));
    } break;

    case Harbour::Item::DOUBLE:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("DOUBLE = %f "), (*pBase)->getND());
      break;

    case Harbour::Item::LOGICAL:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("LOGICAL = %s "), (*pBase)->getL() ? ".T." : ".F.");
      break;

    case Harbour::Item::LONG: {
      char szBuf[24];
      hb_snprintf(buffer, sizeof(buffer), HB_I_("LONG = %s "), hb_numToStr(szBuf, sizeof(szBuf), (*pBase)->getNInt()));
      break;
    }

    case Harbour::Item::INTEGER:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("INTEGER = %i "), (*pBase)->getNI());
      break;

    case Harbour::Item::STRING:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("STRING = \"%s\" "), (*pBase)->getCPtr());
      break;

    case Harbour::Item::SYMBOL:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("SYMBOL = %s "), (*pBase)->symbolValue()->szName);
      break;

    case Harbour::Item::POINTER:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("POINTER = %p "), (*pBase)->pointerValue());
      break;

    default:
      hb_snprintf(buffer, sizeof(buffer), HB_I_("UNKNOWN = TYPE %i "), hb_itemType(*pBase));
      break;
    }

    hb_conOutErr(buffer, 0);
  }
}

#endif

PHB_ITEM hb_stackNewFrame(PHB_STACK_STATE pFrame, HB_USHORT uiParams)
{
  HB_STACK_TLS_PRELOAD

  PHB_ITEM *pBase = hb_stack.pPos - uiParams - 2;
  PHB_ITEM pItem = *pBase; // procedure symbol

  if (!pItem->isSymbol()) {
#if defined(HB_VM_DEBUG)
    hb_stackDispLocal();
#endif
    hb_errInternal(HB_EI_VMNOTSYMBOL, nullptr, "hb_vmDo()", nullptr);
  }

  pFrame->nBaseItem = hb_stack.pBase - hb_stack.pItems;
  pFrame->pStatics = hb_stack.pStatics;
  // as some type of protection we can set hb_stack.pStatics to nullptr here
  pFrame->nPrivateBase = hb_memvarGetPrivatesBase();
  pFrame->uiClass = pFrame->uiMethod = pFrame->uiLineNo = 0;
  pFrame->fDebugging = false;

  pItem->setSymbolStackState(pFrame);
  pItem->setSymbolParamCnt(uiParams);
  // set default value of 'paramdeclcnt' - it will be updated
  // in hb_vm[V]Frame only
  pItem->setSymbolParamDeclCnt(uiParams);
  hb_stack.pBase = pBase;

  return pItem;
}

void hb_stackOldFrame(PHB_STACK_STATE pFrame)
{
  HB_STACK_TLS_PRELOAD
  if (hb_stack.pPos <= hb_stack.pBase) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  do {
    --hb_stack.pPos;
    if ((*hb_stack.pPos)->isComplex()) {
      (*hb_stack.pPos)->clear();
    }
  } while (hb_stack.pPos > hb_stack.pBase);

  hb_stack.pBase = hb_stack.pItems + pFrame->nBaseItem;
  hb_stack.pStatics = pFrame->pStatics;
  hb_memvarSetPrivatesBase(pFrame->nPrivateBase);
}

#undef hb_stackItem
PHB_ITEM hb_stackItem(HB_ISIZ nItemPos)
{
  HB_STACK_TLS_PRELOAD
  if (nItemPos < 0) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  return *(hb_stack.pItems + nItemPos);
}

#undef hb_stackItemFromTop
PHB_ITEM hb_stackItemFromTop(int iFromTop)
{
  HB_STACK_TLS_PRELOAD
  if (iFromTop >= 0) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  return *(hb_stack.pPos + iFromTop);
}

#undef hb_stackItemFromBase
PHB_ITEM hb_stackItemFromBase(int iFromBase)
{
  HB_STACK_TLS_PRELOAD
  if (iFromBase < 0) {
    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  }

  return *(hb_stack.pBase + iFromBase + 1);
}

#undef hb_stackLocalVariable
PHB_ITEM hb_stackLocalVariable(int iLocal)
{
  HB_STACK_TLS_PRELOAD
  PHB_ITEM pBase = *hb_stack.pBase;

  // if( iLocal <= 0 )
  //    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  if (pBase->symbolParamCnt() > pBase->symbolParamDeclCnt()) {
    // function with variable number of parameters:
    // FUNCTION foo(a,b,c,...)
    // LOCAL x,y,z
    // number of passed parameters is bigger then number of declared
    // parameters - skip additional parameters only for local variables
    if (iLocal > pBase->symbolParamDeclCnt()) {
      iLocal += pBase->symbolParamCnt() - pBase->symbolParamDeclCnt();
    }
  }
  return *(hb_stack.pBase + iLocal + 1);
}

#undef hb_stackLocalVariableAt
PHB_ITEM hb_stackLocalVariableAt(int *piFromBase)
{
  HB_STACK_TLS_PRELOAD
  PHB_ITEM pBase = *hb_stack.pBase;

  // if( *piFromBase <= 0 )
  //    hb_errInternal(HB_EI_STACKUFLOW, nullptr, nullptr, nullptr);
  if (pBase->symbolParamCnt() > pBase->symbolParamDeclCnt()) {
    // function with variable number of parameters:
    // FUNCTION foo(a,b,c,...)
    // LOCAL x,y,z
    // number of passed parameters is bigger then number of declared
    // parameters - skip additional parameters only for local variables
    if (*piFromBase > pBase->symbolParamDeclCnt()) {
      *piFromBase += pBase->symbolParamCnt() - pBase->symbolParamDeclCnt();
    }
  }
  return *(hb_stack.pBase + *piFromBase + 1);
}

#undef hb_stackBaseItem
PHB_ITEM hb_stackBaseItem(void)
{
  HB_STACK_TLS_PRELOAD
  return *hb_stack.pBase;
}

// Returns SELF object, an evaluated codeblock or NIL for normal func/proc
#undef hb_stackSelfItem
PHB_ITEM hb_stackSelfItem(void)
{
  HB_STACK_TLS_PRELOAD
  return *(hb_stack.pBase + 1);
}

#undef hb_stackReturnItem
PHB_ITEM hb_stackReturnItem(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackReturnItem()"));
#endif

  HB_STACK_TLS_PRELOAD
  return &hb_stack.Return;
}

#undef hb_stackTopOffset
HB_ISIZ hb_stackTopOffset(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pPos - hb_stack.pItems;
}

#undef hb_stackBaseOffset
HB_ISIZ hb_stackBaseOffset(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pBase - hb_stack.pItems + 1;
}

#undef hb_stackTotalItems
HB_ISIZ hb_stackTotalItems(void)
{
#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return hb_stack.nItems;
  }
  return 0;
#else
  HB_STACK_TLS_PRELOAD
  return hb_stack.nItems;
#endif
}

#if defined(HB_MT_VM)
void *hb_stackAllocator(void)
{
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return hb_stack.allocator;
  }
  return nullptr;
}
#endif

#undef hb_stackDateBuffer
char *hb_stackDateBuffer(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.szDate;
}

char *hb_stackDirBuffer(void)
{
#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    if (!hb_stack.pDirBuffer) {
      hb_stack.pDirBuffer = static_cast<char *>(hb_xgrab(HB_PATH_MAX));
    }
    return hb_stack.pDirBuffer;
  }
#endif
  return s_szDirBuffer;
}

PHB_IOERRORS hb_stackIOErrors(void)
{
#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return &hb_stack.IOErrors;
  }
#endif
  return &s_IOErrors;
}

void *hb_stackGetGT(void)
{
#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return hb_stack.hGT;
  } else {
    return nullptr;
  }
#else
  HB_STACK_TLS_PRELOAD
  return hb_stack.hGT;
#endif
}

void hb_stackSetGT(void *hGT)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.hGT = hGT;
}

PHB_STACKRDD hb_stackRDD(void)
{
  HB_STACK_TLS_PRELOAD
  return &hb_stack.rdd;
}

#undef hb_stackGetStaticsBase
void *hb_stackGetStaticsBase(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pStatics;
}

#undef hb_stackSetStaticsBase
void hb_stackSetStaticsBase(void *pBase)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.pStatics = pBase;
}

#undef hb_stackGetRecoverBase
HB_ISIZ hb_stackGetRecoverBase(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.nRecoverBase;
}

#undef hb_stackSetRecoverBase
void hb_stackSetRecoverBase(HB_ISIZ nBase)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.nRecoverBase = nBase;
}

#undef hb_stackGetActionRequest
HB_USHORT hb_stackGetActionRequest(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.uiActionRequest;
}

#undef hb_stackSetActionRequest
void hb_stackSetActionRequest(HB_USHORT uiAction)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.uiActionRequest = uiAction;
}

#undef hb_stackWithObjectItem
PHB_ITEM hb_stackWithObjectItem(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.nWithObject ? *(hb_stack.pItems + hb_stack.nWithObject) : nullptr;
}

#undef hb_stackWithObjectOffset
HB_ISIZ hb_stackWithObjectOffset(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.nWithObject;
}

#undef hb_stackWithObjectSetOffset
void hb_stackWithObjectSetOffset(HB_ISIZ nOffset)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.nWithObject = nOffset;
}

#undef hb_stackGetCDP
void *hb_stackGetCDP(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pCDP;
}

#undef hb_stackSetCDP
void hb_stackSetCDP(void *pCDP)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.pCDP = pCDP;
}

#undef hb_stackGetLang
void *hb_stackGetLang(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pLang;
}

#undef hb_stackSetLang
void hb_stackSetLang(void *pLang)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.pLang = pLang;
}

#undef hb_stackGetI18N
void *hb_stackGetI18N(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stack.pI18N;
}

#undef hb_stackSetI18N
void hb_stackSetI18N(void *pI18N)
{
  HB_STACK_TLS_PRELOAD
  hb_stack.pI18N = pI18N;
}

#undef hb_stackItemBasePtr
PHB_ITEM **hb_stackItemBasePtr(void)
{
  HB_STACK_TLS_PRELOAD
  return &hb_stack.pItems;
}

void hb_stackClearMemvarsBase(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackClearMemvarsBase()"));
#endif

  HB_STACK_TLS_PRELOAD

  PHB_ITEM pBase = *hb_stack.pBase;

  while (pBase->symbolStackState()->nPrivateBase != 0) {
    pBase->symbolStackState()->nPrivateBase = 0;
    pBase = *(hb_stack.pItems + pBase->symbolStackState()->nBaseItem);
  }
}

int hb_stackCallDepth(void)
{
  HB_STACK_TLS_PRELOAD
  HB_ISIZ nOffset = hb_stack.pBase - hb_stack.pItems;
  int iLevel = 0;

  while (nOffset > 0) {
    nOffset = (*(hb_stack.pItems + nOffset))->symbolStackState()->nBaseItem;
    ++iLevel;
  }

  return iLevel;
}

HB_ISIZ hb_stackBaseProcOffset(int iLevel)
{
  HB_STACK_TLS_PRELOAD
  HB_ISIZ nOffset = hb_stack.pBase - hb_stack.pItems;

  while (iLevel-- > 0 && nOffset > 0) {
    nOffset = (*(hb_stack.pItems + nOffset))->symbolStackState()->nBaseItem;
  }

  if (iLevel < 0 && (nOffset > 0 || (*hb_stack.pItems)->isSymbol())) {
    return nOffset;
  } else {
    return -1;
  }
}

HB_ISIZ hb_stackBaseSymbolOffset(PHB_SYMB pSymbol)
{
  HB_STACK_TLS_PRELOAD
  HB_ISIZ nOffset = hb_stack.pBase - hb_stack.pItems;

  while (nOffset > 0) {
    PHB_ITEM pItem = hb_stack.pItems[nOffset];
    if (pItem->symbolValue() == pSymbol ||
        (pSymbol->pDynSym != nullptr && pItem->symbolValue()->pDynSym == pSymbol->pDynSym)) {
      return nOffset;
    }
    nOffset = pItem->symbolStackState()->nBaseItem;
  }
  return -1;
}

void hb_stackBaseProcInfo(char *szProcName, HB_USHORT *puiProcLine)
{
  // This function is called by FM module and has to be ready for execution
  // before hb_stack initialization, [druzus]
  // szProcName should be at least HB_SYMBOL_NAME_LEN + 1 bytes buffer

#if defined(HB_MT_VM)
  if (!hb_stack_ready()) {
    szProcName[0] = '\0';
    *puiProcLine = 0;
  } else
#endif
  {
    HB_STACK_TLS_PRELOAD
    if (hb_stack.pPos > hb_stack.pBase) {
      hb_strncpy(szProcName, (*hb_stack.pBase)->symbolValue()->szName, HB_SYMBOL_NAME_LEN);
      *puiProcLine = (*hb_stack.pBase)->symbolStackState()->uiLineNo;
    } else {
      szProcName[0] = '\0';
      *puiProcLine = 0;
    }
  }
}

void hb_stackDispCall(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispCall()"));
#endif

  int iLevel = 0;
  char buffer[HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 + 10]; // additional 10 bytes for line info (%hu) overhead
  HB_USHORT uiLine;
  char file[HB_PATH_MAX];

  while (hb_procinfo(iLevel++, buffer, &uiLine, file)) {
    auto l = static_cast<int>(strlen(buffer));
    hb_snprintf(buffer + l, sizeof(buffer) - l, "(%hu)%s%s", uiLine, *file ? HB_I_(" in ") : "", file);
    hb_conOutErr("Called from ", 0);
    hb_conOutErr(buffer, 0);
    hb_conOutErr(hb_conNewLine(), 0);
  }
}

// ------------------------------------------------------------------------
// The garbage collector interface
// ------------------------------------------------------------------------

#if !defined(HB_MT_VM)
// helper function to scan all visible memvar variables
static HB_DYNS_FUNC(hb_stackMemvarScan)
{
  PHB_ITEM pMemvar;

  HB_SYMBOL_UNUSED(Cargo);

  pMemvar = hb_dynsymGetMemvar(pDynSymbol);
  if (pMemvar && pMemvar->isGCItem()) {
    hb_gcItemRef(pMemvar);
  }

  return true;
}
#endif

// Mark all memvars (PRIVATEs and PUBLICs)
static void hb_stackIsMemvarRef(PHB_STACK pStack)
{
  // 1. Mark all hidden memvars (PRIVATEs and PUBLICs)
  PHB_PRIVATE_STACK pPrivateStack = &pStack->privates;
  HB_SIZE nCount = pPrivateStack->count;

  while (nCount) {
    PHB_ITEM pMemvar = pPrivateStack->stack[--nCount].pPrevMemvar;
    if (pMemvar && pMemvar->isGCItem()) {
      hb_gcItemRef(pMemvar);
    }
  }
  // 2. Mark all visible memvars (PRIVATEs and PUBLICs)
#if defined(HB_MT_VM)
  {
    HB_SYMCNT uiDynSym = pStack->uiDynH;

    while (uiDynSym > 0) {
      auto pMemvar = static_cast<PHB_ITEM>(pStack->pDynH[--uiDynSym].pMemvar);
      if (pMemvar && pMemvar->isGCItem()) {
        hb_gcItemRef(pMemvar);
      }
    }
  }
#else
  hb_dynsymEval(hb_stackMemvarScan, nullptr);
#endif
}

// Mark all thread static variables
static void hb_stackIsTsdRef(PHB_STACK pStack, PHB_TSD_FUNC pCleanFunc)
{
  int iTSD = pStack->iTSD;

  while (iTSD) {
    if (pStack->pTSD[iTSD].pTSD && pStack->pTSD[iTSD].pTSD->pCleanFunc == pCleanFunc) {
      auto pItem = static_cast<PHB_ITEM>(pStack->pTSD[iTSD].value);
      if (pItem->isGCItem()) {
        hb_gcItemRef(pItem);
      }
    }
    --iTSD;
  }
}

// Mark all locals as used so they will not be released by the
// garbage collector
void hb_stackIsStackRef(void *pStackId, PHB_TSD_FUNC pCleanFunc)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackIsStackRef()"));
#endif

  auto pStack = static_cast<PHB_STACK>(pStackId);
  HB_ISIZ nCount = pStack->pPos - pStack->pItems;
  while (nCount > 0) {
    PHB_ITEM pItem = pStack->pItems[--nCount];
    if (pItem->isGCItem()) {
      hb_gcItemRef(pItem);
    }
  }

  hb_gcItemRef(&pStack->Return);

  hb_stackIsMemvarRef(pStack);

  if (pCleanFunc) {
    hb_stackIsTsdRef(pStack, pCleanFunc);
  }

  hb_gtIsGtRef(pStack->hGT);
}

void hb_stackUpdateAllocator(void *pStackId, PHB_ALLOCUPDT_FUNC pFunc, int iCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stackUpdateAllocator(%p, %p, %d)", pStackId, reinterpret_cast<void*>(pFunc), iCount));
#endif

#if defined(HB_MT_VM)
  {
    auto pStack = static_cast<PHB_STACK>(pStackId);

    if (pStack->allocator) {
      pStack->allocator = pFunc(pStack->allocator, iCount);
    }
  }
#else
  HB_SYMBOL_UNUSED(pStackId);
  HB_SYMBOL_UNUSED(pFunc);
  HB_SYMBOL_UNUSED(iCount);
#endif
}

PHB_TRACEINFO hb_traceinfo(void)
{
#if defined(HB_MT_VM)
  if (hb_stack_ready()) {
    HB_STACK_TLS_PRELOAD
    return &hb_stack.traceInfo;
  }
#endif
  return &s_traceInfo;
}

void hb_traceset(int level, const char *file, int line, const char *proc)
{
  PHB_TRACEINFO pTrace = hb_traceinfo();

  pTrace->level = level;
  pTrace->file = file;
  pTrace->line = line;
  pTrace->proc = proc;
}
