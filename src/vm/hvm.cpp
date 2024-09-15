//
// The Virtual Machine
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) (hb_vmPushLongConst(), hb_vmPushDoubleConst())
// Copyright 1999 Eddie Runia <eddie@runia.com> (__dbgVMVarSGet(), __dbgVMVarSList())
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

#define HB_STACK_PRELOAD

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbstack.hpp"
#include "hbapierr.hpp"
#include "hbapicls.hpp"
#include "hbapidbg.hpp"
#include "hbapiitm.hpp"
#include "hbapilng.hpp"
#include "hbapirdd.hpp"
#include "hbapigt.hpp"
#include "hbapicdp.hpp"
#include "hbvm.hpp"
#include "hbxvm.hpp"
#include "hbpcode.hpp"
#include "hbset.hpp"
#include "hbdate.hpp"
#include "hbmath.hpp"
#include "hbdebug.ch"
#if defined(HB_MT_VM)
#include "hbthread.hpp"
#endif // HB_MT_VM
#include "hbmemory.ch"

#ifndef HB_NO_PROFILER
#include <time.h>
#endif

HB_FUNC_EXTERN(SYSINIT);
HB_FUNC_EXTERN(BREAK);

// PCode functions

// Operators (mathematical / character / misc)

// negates (-) the latest value on the stack
static void hb_vmNegate();
// increment the latest numeric value on the stack
static void hb_vmInc(PHB_ITEM pItem);
// decrements the latest numeric value on the stack
static void hb_vmDec(PHB_ITEM pItem);
// pushes a function address pointer. Removes the symbol from the stack
static void hb_vmFuncPtr();
// add integer to given item
static void hb_vmAddInt(PHB_ITEM pResult, HB_LONG lAdd);
// sums given values
static void hb_vmPlus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);
// subtracts given values
static void hb_vmMinus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);
// multiplies given values
static void hb_vmMult(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);
// divides the given values
static void hb_vmDivide(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);
// calculates modulus given values
static void hb_vmModulus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);
// power given values
static void hb_vmPower(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2);

// Operators (relational)

// checks if the two latest values on the stack are equal, removes both and leaves result
static void hb_vmEqual();
// checks if the two latest values on the stack are exactly equal, removes both and leaves result
static void hb_vmExactlyEqual();
// checks if the two latest values on the stack are not equal, removes both and leaves result
static void hb_vmNotEqual();
// checks if the latest - 1 value is less than the latest, removes both and leaves result
static void hb_vmLess();
// checks if the latest - 1 value is less than or equal the latest, removes both and leaves result
static void hb_vmLessEqual();
// checks if the latest - 1 value is greater than the latest, removes both and leaves result
static void hb_vmGreater();
// checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result
static void hb_vmGreaterEqual();
// check whether string 1 is contained in string 2
static void hb_vmInstring();
// test for end condition of for
static void hb_vmForTest();
// set begin sequence WITH codeblock
static void hb_vmSeqBlock();
// prepare WITH OBJECT block
static void hb_vmWithObjectStart();
// prepare FOR EACH loop
static void hb_vmEnumStart(int nVars, int nDescend);
// increment FOR EACH loop counter
static void hb_vmEnumNext();
// decrement FOR EACH loop counter
static void hb_vmEnumPrev();
// rewind the stack after FOR EACH loop counter
static void hb_vmEnumEnd();
// make a SWITCH statement
static const HB_BYTE *hb_vmSwitch(const HB_BYTE *pCode, HB_USHORT);

// Operators (logical)

// changes the latest logical value on the stack
static void hb_vmNot();
// performs the logical AND on the latest two values, removes them and leaves result on the stack
static void hb_vmAnd();
// performs the logical OR on the latest two values, removes them and leaves result on the stack
static void hb_vmOr();

// Array

// pushes an array element to the stack, removing the array and the index from the stack
static void hb_vmArrayPush();
// pushes a reference to an array element to the stack, removing the array and the index from the stack
static void hb_vmArrayPushRef();
// pops a value from the stack
static void hb_vmArrayPop();
// generates an uiDimensions Array and initialize those dimensions from the stack values
static void hb_vmArrayDim(HB_USHORT uiDimensions);
// generates an nElements Array and fills it from the stack values
static void hb_vmArrayGen(HB_SIZE nElements);
// generates an nElements Hash and fills it from the stack values
static void hb_vmHashGen(HB_SIZE nElements);

// macros

// execute function passing arguments set on HVM stack func(&var)
static void hb_vmMacroDo(HB_USHORT uiArgSets);
// execute procedure passing arguments set on HVM stack func(&var)
static void hb_vmMacroFunc(HB_USHORT uiArgSets);
// execute procedure passing arguments set on HVM stack func(&var)
static void hb_vmMacroSend(HB_USHORT uiArgSets);
// generate array from arguments set on HVM stack { &var }
static void hb_vmMacroArrayGen(HB_USHORT uiArgSets);
// push macro array index {...}[ &var ]
static void hb_vmMacroPushIndex();

// Database

// select the workarea using a given item or a substituted value
static HB_ERRCODE hb_vmSelectWorkarea(PHB_ITEM, PHB_SYMB);
// swaps items on the eval stack and pops the workarea number
static void hb_vmSwapAlias();

// Execution

// executes a codeblock
static HARBOUR hb_vmDoBlock();
// increases the stack pointer for the amount of locals and params supplied
static void hb_vmFrame(HB_USHORT usLocals, unsigned char ucParams);
// increases the stack pointer for the amount of locals and variable number of params supplied
static void hb_vmVFrame(HB_USHORT usLocals, unsigned char ucParams);
// sets the statics frame for a function
static void hb_vmSFrame(PHB_SYMB pSym);
// increases the global statics array to hold a PRG statics
static void hb_vmStatics(PHB_SYMB pSym, HB_USHORT uiStatics);
// mark thread static variables
static void hb_vmInitThreadStatics(HB_USHORT uiCount, const HB_BYTE *pCode);
// clear complex static variables
static void hb_vmStaticsClear();
// release arrays with static variables
static void hb_vmStaticsRelease();

// Push

// pushes the current workarea number
static void hb_vmPushAlias();
// pushes an aliased field on the eval stack
static void hb_vmPushAliasedField(PHB_SYMB);
// pushes an aliased variable on the eval stack
static void hb_vmPushAliasedVar(PHB_SYMB);
// creates a codeblock
static void hb_vmPushBlock(const HB_BYTE *pCode, PHB_SYMB pSymbols, HB_SIZE nLen);
// creates a codeblock
static void hb_vmPushBlockShort(const HB_BYTE *pCode, PHB_SYMB pSymbols, HB_SIZE nLen);
// creates a macro-compiled codeblock
static void hb_vmPushMacroBlock(const HB_BYTE *pCode, HB_SIZE nSize, HB_USHORT usParams);
// Pushes a double constant (pcode)
static void hb_vmPushDoubleConst(double dNumber, int iWidth, int iDec);
// pushes the content of a local onto the stack
static void hb_vmPushLocal(int iLocal);
// pushes a local by reference onto the stack
static void hb_vmPushLocalByRef(int iLocal);
// pushes a HB_MAXINT number onto the stack
static void hb_vmPushHBLong(HB_MAXINT nNumber);
#if !defined(HB_LONG_LONG_OFF)
// Pushes a long long constant (pcode)
static void hb_vmPushLongLongConst(HB_LONGLONG lNumber);
#endif
#if HB_VMINT_MAX >= INT32_MAX
// Pushes a int constant (pcode)
static void hb_vmPushIntegerConst(int iNumber);
#else
// Pushes a long constant (pcode)
static void hb_vmPushLongConst(long lNumber);
#endif
// pushes the content of a static onto the stack
static void hb_vmPushStatic(HB_USHORT uiStatic);
// pushes a static by reference onto the stack
static void hb_vmPushStaticByRef(HB_USHORT uiStatic);
// pushes undeclared variable
static void hb_vmPushVariable(PHB_SYMB pVarSymb);
// pushes reference to object variable
static void hb_vmPushObjectVarRef();
// pushes variable parameters
static void hb_vmPushVParams();
// pushes array items
static void hb_vmPushAParams();
// push the unreferenced latest value on the stack
static void hb_vmPushUnRef();
// duplicates the latest value on the stack
static void hb_vmDuplicate();
// duplicates the latest value on the stack and unref the source one
static void hb_vmDuplUnRef();
// swap bCount+1 time two items on HVM stack starting from the most top one
static void hb_vmSwap(int iCount);

// Pop

// pops the stack latest value and returns its logical value
static HB_BOOL hb_vmPopLogical();
// pops the workarea number form the eval stack
static void hb_vmPopAlias();
// pops an aliased field from the eval stack
static void hb_vmPopAliasedField(PHB_SYMB);
// pops an aliased variable from the eval stack
static void hb_vmPopAliasedVar(PHB_SYMB);
// pops the stack latest value onto a local
static void hb_vmPopLocal(int iLocal);
// pops the stack latest value onto a static
static void hb_vmPopStatic(HB_USHORT uiStatic);

// misc

// executes all _INITSTATICS functions
static void hb_vmDoInitStatics();
// executes all defined PRGs INIT functions
static void hb_vmDoInitFunctions(HB_BOOL);
// executes all defined PRGs EXIT functions
static void hb_vmDoExitFunctions();
// releases the memory of the local symbols linked list
static void hb_vmReleaseLocalSymbols();

// create object index reference
static void hb_vmMsgIndexReference(PHB_ITEM pRefer, PHB_ITEM pObject, PHB_ITEM pIndex);

#ifndef HB_NO_DEBUG
// locals and parameters index and name information for the debugger
static void hb_vmLocalName(HB_USHORT uiLocal, const char *szLocalName);
// statics vars information for the debugger
static void hb_vmStaticName(HB_BYTE bIsGlobal, HB_USHORT uiStatic, const char *szStaticName);
// PRG and function name information for the debugger
static void hb_vmModuleName(const char *szModuleName);

static void hb_vmDebugEntry(int nMode, int nLine, const char *szName, int nIndex, PHB_ITEM pFrame);
// shuts down the debugger
static void hb_vmDebuggerExit(HB_BOOL fRemove);
// makes the debugger shows a specific source code line
static void hb_vmDebuggerShowLine(HB_USHORT uiLine);
// notifies the debugger for an endproc
static void hb_vmDebuggerEndProc();

static PHB_DYNS s_pDynsDbgEntry = nullptr; // Cached __DBGENTRY symbol
static HB_DBGENTRY_FUNC s_pFunDbgEntry;    // C level debugger entry
#endif

static auto s_fInternalsEnabled = true;

#if defined(HB_MT_VM)
static int volatile hb_vmThreadRequest = 0;
static void hb_vmRequestTest();

static PHB_ITEM s_pSymbolsMtx = nullptr;

static HB_CRITICAL_NEW(s_atInitMtx);
#define HB_ATINIT_LOCK() hb_threadEnterCriticalSection(&s_atInitMtx)
#define HB_ATINIT_UNLOCK() hb_threadLeaveCriticalSection(&s_atInitMtx)
#define HB_TASK_SHEDULER() HB_THREAD_SHEDULER()
#else
#define HB_ATINIT_LOCK()
#define HB_ATINIT_UNLOCK()
#define HB_TASK_SHEDULER()
#endif // HB_MT_VM

#ifndef HB_NO_PROFILER
static HB_ULONG hb_ulOpcodesCalls[HB_P_LAST_PCODE]; // array to profile opcodes calls
static HB_ULONG hb_ulOpcodesTime[HB_P_LAST_PCODE];  // array to profile opcodes consumed time
static auto hb_bProfiler = false;                   // profiler status is off
#endif

#if defined(HB_PRG_TRACE)
static auto hb_bTracePrgCalls = false; // prg tracing is off
#define HB_TRACE_PRG(_TRMSG_)                                                                                          \
  if (hb_bTracePrgCalls)                                                                                               \
  HB_TRACE(HB_TR_ALWAYS, _TRMSG_)
#else
#define HB_TRACE_PRG(_TRMSG_)
#endif

static const char *s_vm_pszLinkedMain = nullptr; // name of startup function set by linker

// virtual machine state

HB_SYMB hb_symEval = {"EVAL", {HB_FS_PUBLIC}, {hb_vmDoBlock}, nullptr}; // symbol to evaluate codeblocks
static HB_SYMB s_symBreak = {"BREAK", {HB_FS_PUBLIC}, {HB_FUNCNAME(BREAK)}, nullptr}; // symbol to generate break
static PHB_ITEM s_breakBlock = nullptr;

static auto s_fHVMActive = false;      // is HVM ready for PCODE executing
static auto s_fDoExitProc = true;      // execute EXIT procedures
static int s_nErrorLevel = 0;          // application exit status
static PHB_SYMB s_pSymStart = nullptr; // start symbol of the application. MAIN() is not required

static PHB_SYMBOLS s_pSymbols = nullptr; // to hold a linked list of all different modules symbol tables
static HB_ULONG s_ulFreeSymbols = 0;     // number of free module symbols
static void *s_hDynLibID = nullptr;      // unique identifier to mark symbol tables loaded from dynamic libraries
static auto s_fCloneSym = false;         // clone registered symbol tables

#ifndef HB_GUI
auto s_fKeyPool = true;
#endif

// main VM thread stack ID
static void *s_main_thread = nullptr;

// SEQUENCE envelope items position from stack top active
#define HB_RECOVER_STATE -1
#define HB_RECOVER_VALUE -2

#define HB_SEQ_CANRECOVER 64
#define HB_SEQ_DOALWAYS 128

static PHB_FUNC_LIST s_InitFunctions = nullptr;
static PHB_FUNC_LIST s_ExitFunctions = nullptr;
static PHB_FUNC_LIST s_QuitFunctions = nullptr;

static PHB_ITEM hb_breakBlock()
{
  if (s_breakBlock == nullptr)
  {
    static const HB_BYTE s_pCode[8] = {HB_P_PUSHFUNCSYM,   0, 0, /* BREAK */
                                       HB_P_PUSHLOCALNEAR, 1,    /* oErr */
                                       HB_P_FUNCTIONSHORT, 1, HB_P_ENDBLOCK};

    s_breakBlock = hb_itemNew(nullptr);
    s_breakBlock->item.asBlock.value = hb_codeblockNew(s_pCode, /* pcode buffer         */
                                                       0,       /* number of referenced local variables */
                                                       nullptr, /* table with referenced local variables */
                                                       &s_symBreak, sizeof(s_pCode));
    s_breakBlock->type = Harbour::Item::BLOCK;
    s_breakBlock->item.asBlock.paramcnt = 1;
    s_breakBlock->item.asBlock.lineno = 0;
    s_breakBlock->item.asBlock.hclass = 0;
    s_breakBlock->item.asBlock.method = 0;
  }
  return s_breakBlock;
}

static void hb_breakBlockRelease()
{
  if (s_breakBlock != nullptr)
  {
    hb_itemRelease(s_breakBlock);
    s_breakBlock = nullptr;
  }
}

static void hb_vmAddModuleFunction(PHB_FUNC_LIST *pLstPtr, HB_INIT_FUNC pFunc, void *cargo)
{
  auto pLst = static_cast<PHB_FUNC_LIST>(hb_xgrab(sizeof(HB_FUNC_LIST)));

  pLst->pFunc = pFunc;
  pLst->cargo = cargo;
  pLst->hDynLib = s_hDynLibID;
  HB_ATINIT_LOCK();
  pLst->pNext = *pLstPtr;
  *pLstPtr = pLst;
  HB_ATINIT_UNLOCK();
}

static void hb_vmDoModuleFunctions(PHB_FUNC_LIST *pLstPtr)
{
  while (*pLstPtr)
  {
    PHB_FUNC_LIST pLst = *pLstPtr;
    *pLstPtr = pLst->pNext;
    pLst->pFunc(pLst->cargo);
    hb_xfree(pLst);
  }
}

static void hb_vmDoModuleLibFunctions(PHB_FUNC_LIST *pLstPtr, void *hDynLib)
{
  while (*pLstPtr)
  {
    PHB_FUNC_LIST pLst = *pLstPtr;
    if (pLst->hDynLib == hDynLib)
    {
      *pLstPtr = pLst->pNext;
      pLst->pFunc(pLst->cargo);
      hb_xfree(pLst);
    }
    else
    {
      pLstPtr = &pLst->pNext;
    }
  }
}

static void hb_vmDoModuleSetLibID(PHB_FUNC_LIST pLst, void *hDynLib, void *hNewDynLib)
{
  while (pLst)
  {
    if (pLst->hDynLib == hDynLib)
    {
      pLst->hDynLib = hNewDynLib;
    }
    pLst = pLst->pNext;
  }
}

static void hb_vmCleanModuleFunctions()
{
  PHB_FUNC_LIST pLst;

  while (s_InitFunctions)
  {
    pLst = s_InitFunctions;
    s_InitFunctions = pLst->pNext;
    hb_xfree(pLst);
  }
  while (s_ExitFunctions)
  {
    pLst = s_ExitFunctions;
    s_ExitFunctions = pLst->pNext;
    hb_xfree(pLst);
  }
  while (s_QuitFunctions)
  {
    pLst = s_QuitFunctions;
    s_QuitFunctions = pLst->pNext;
    hb_xfree(pLst);
  }
}

void hb_vmAtInit(HB_INIT_FUNC pFunc, void *cargo)
{
  hb_vmAddModuleFunction(&s_InitFunctions, pFunc, cargo);
}

void hb_vmAtExit(HB_INIT_FUNC pFunc, void *cargo)
{
  hb_vmAddModuleFunction(&s_ExitFunctions, pFunc, cargo);
}

void hb_vmAtQuit(HB_INIT_FUNC pFunc, void *cargo)
{
  hb_vmAddModuleFunction(&s_QuitFunctions, pFunc, cargo);
}

static void hb_vmDoModuleInitFunctions()
{
  hb_vmDoModuleFunctions(&s_InitFunctions);
}

static void hb_vmDoModuleExitFunctions()
{
  hb_vmDoModuleFunctions(&s_ExitFunctions);
}

static void hb_vmDoModuleQuitFunctions()
{
  hb_vmDoModuleFunctions(&s_QuitFunctions);
}

/* call __HBVMINIT() function to initialize GetList public variable
 * and set ErrorBlock() by ErrorSys() function
 */
static void hb_vmDoInitHVM()
{
  auto pDynSym = hb_dynsymFind("__HBVMINIT");

  if (pDynSym && pDynSym->pSymbol->value.pFunPtr)
  {
    hb_vmPushSymbol(pDynSym->pSymbol);
    hb_vmPushNil();
    hb_vmProc(0);
  }
}

/* call __SetHelpK() if HELP() function is linked */
static void hb_vmDoInitHelp()
{
  auto pDynSym = hb_dynsymFind("HELP");

  if (pDynSym && pDynSym->pSymbol->value.pFunPtr)
  {
    pDynSym = hb_dynsymFind("__SETHELPK");
    if (pDynSym && pDynSym->pSymbol->value.pFunPtr)
    {
      hb_vmPushSymbol(pDynSym->pSymbol);
      hb_vmPushNil();
      hb_vmProc(0);
    }
  }
}

#if !defined(HB_MT_VM)

HB_BOOL hb_vmIsMt(void)
{
  return false;
}

HB_BOOL hb_vmThreadIsMain(void *Cargo)
{
  HB_SYMBOL_UNUSED(Cargo);
  return s_fHVMActive;
}

void hb_vmLock(void)
{
}

void hb_vmLockForce(void)
{
}

void hb_vmUnlock(void)
{
}

HB_BOOL hb_vmSuspendThreads(HB_BOOL fWait)
{
  HB_SYMBOL_UNUSED(fWait);
  return true;
}

void hb_vmResumeThreads(void)
{
}

void hb_vmTerminateThreads(void)
{
}

void hb_vmWaitForThreads(void)
{
}

HB_BOOL hb_vmThreadRegister(void *Cargo)
{
  HB_SYMBOL_UNUSED(Cargo);
  return false;
}

void hb_vmThreadRelease(void *Cargo)
{
#if 0
   auto pState = static_cast<PHB_THREADSTATE>(Cargo);
   PHB_ITEM pThItm = pState->pThItm;
   pState->pThItm = nullptr;
   if( pThItm ) {
      hb_itemRelease(pThItm);
   }
#endif

  HB_SYMBOL_UNUSED(Cargo);
}

void *hb_vmThreadState(void)
{
  return nullptr;
}

void hb_vmThreadInit(void *Cargo)
{
  HB_SYMBOL_UNUSED(Cargo);
}

void hb_vmThreadQuit(void)
{
}

void hb_vmThreadQuitRequest(void *Cargo)
{
  HB_SYMBOL_UNUSED(Cargo);
}

#else

static HB_CRITICAL_NEW(s_vmMtx);
static HB_COND_NEW(s_vmCond);

/* number of allocated HVM stacks */
static int volatile s_iStackCount = 0;
/* number of running HVM threads */
static int volatile s_iRunningCount = 0;
/* active HVM stacks list */
static PHB_THREADSTATE s_vmStackLst = nullptr;
/* thread number */
static HB_THREAD_NO s_threadNo = 0;

#define HB_THREQUEST_STOP 1
#define HB_THREQUEST_QUIT 2

#define HB_VM_LOCK() hb_threadEnterCriticalSection(&s_vmMtx)
#define HB_VM_UNLOCK() hb_threadLeaveCriticalSection(&s_vmMtx)

HB_BOOL hb_vmIsMt(void)
{
  return true;
}

static void hb_vmRequestTest()
{
  HB_VM_LOCK();

  s_iRunningCount--;
  for (;;)
  {
    if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
    {
      HB_STACK_TLS_PRELOAD
      if (!hb_stackQuitState())
      {
        hb_stackSetQuitState(true);
        hb_stackSetActionRequest(HB_QUIT_REQUESTED);
      }
    }
    if (hb_vmThreadRequest & HB_THREQUEST_STOP)
    {
      hb_threadCondBroadcast(&s_vmCond);
      hb_threadCondWait(&s_vmCond, &s_vmMtx);
    }
    else
    {
      break;
    }
  }
  s_iRunningCount++;

  HB_VM_UNLOCK();
}

/* unlock VM, allow GC and other exclusive single task code execution */
void hb_vmUnlock(void)
{
  if (s_fHVMActive)
  {
    HB_STACK_TLS_PRELOAD

    if (hb_stackId())
    { /* check if thread has associated HVM stack */
      if (hb_stackUnlock() == 1)
      {
        HB_VM_LOCK();
        s_iRunningCount--;
        if (hb_vmThreadRequest)
        {
          if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
          {
            if (!hb_stackQuitState())
            {
              hb_stackSetQuitState(true);
              hb_stackSetActionRequest(HB_QUIT_REQUESTED);
            }
          }
          hb_threadCondBroadcast(&s_vmCond);
        }
        HB_VM_UNLOCK();
      }
    }

    HB_TASK_SHEDULER();
  }
}

/* lock VM blocking GC and other exclusive single task code execution */
void hb_vmLock(void)
{
  if (s_fHVMActive)
  {
    HB_STACK_TLS_PRELOAD

    if (hb_stackId())
    { /* check if thread has associated HVM stack */
      if (hb_stackLock() == 0)
      {
        HB_VM_LOCK();
        for (;;)
        {
          if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
          {
            if (!hb_stackQuitState())
            {
              hb_stackSetQuitState(true);
              hb_stackSetActionRequest(HB_QUIT_REQUESTED);
            }
          }
          if (hb_vmThreadRequest & HB_THREQUEST_STOP)
          {
            hb_threadCondWait(&s_vmCond, &s_vmMtx);
          }
          else
          {
            break;
          }
        }
        s_iRunningCount++;
        HB_VM_UNLOCK();
      }
    }
  }
}

void hb_vmLockForce(void)
{
  if (s_fHVMActive)
  {
    HB_STACK_TLS_PRELOAD

    if (hb_stackId())
    { /* check if thread has associated HVM stack */
      if (hb_stackLock() == 0)
      {
        HB_VM_LOCK();
        if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
        {
          if (!hb_stackQuitState())
          {
            hb_stackSetQuitState(true);
            hb_stackSetActionRequest(HB_QUIT_REQUESTED);
          }
        }
        s_iRunningCount++;
        HB_VM_UNLOCK();
      }
    }
  }
}

/* (try to) stop all threads except current one */
HB_BOOL hb_vmSuspendThreads(HB_BOOL fWait)
{
  HB_VM_LOCK();

  if ((hb_vmThreadRequest & (HB_THREQUEST_STOP | HB_THREQUEST_QUIT)) == 0)
  {
    hb_vmThreadRequest |= HB_THREQUEST_STOP;
    --s_iRunningCount;
    for (;;)
    {
      if (s_iRunningCount <= 0)
      {
        ++s_iRunningCount;
        return true;
      }
      if (!fWait)
      {
        break;
      }
      hb_threadCondWait(&s_vmCond, &s_vmMtx);
      if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
      {
        break;
      }
    }
    ++s_iRunningCount;
    hb_vmThreadRequest &= ~HB_THREQUEST_STOP;
    hb_threadCondBroadcast(&s_vmCond);
  }

  HB_VM_UNLOCK();

  return false;
}

/* unblock execution of threads stopped by hb_vmSuspendThreads() */
void hb_vmResumeThreads(void)
{
  hb_vmThreadRequest &= ~HB_THREQUEST_STOP;
  hb_threadCondBroadcast(&s_vmCond);
  HB_VM_UNLOCK();
}

/* send QUIT request to all threads except current one
 * and wait for their termination,
 * should be called only by main HVM thread
 */
void hb_vmTerminateThreads(void)
{
  HB_STACK_TLS_PRELOAD

  if (s_main_thread == hb_stackId())
  {
    HB_VM_LOCK();

    hb_vmThreadRequest |= HB_THREQUEST_QUIT;
    --s_iRunningCount;

    hb_threadMutexUnlockAll();
    hb_threadMutexUnsubscribeAll();

    hb_threadCondBroadcast(&s_vmCond);

    while (s_iStackCount > 1)
    {
      hb_threadCondWait(&s_vmCond, &s_vmMtx);
    }

    ++s_iRunningCount;
#if 0
      hb_vmThreadRequest &= ~HB_THREQUEST_QUIT;
#endif
    hb_vmThreadRequest = 0;

    HB_VM_UNLOCK();
  }
}

/* wait for all threads to terminate
 * should be called only by main HVM thread
 */
void hb_vmWaitForThreads(void)
{
  HB_STACK_TLS_PRELOAD

  if (s_main_thread == hb_stackId())
  {
    HB_VM_LOCK();

    --s_iRunningCount;
    if (hb_vmThreadRequest)
    {
      hb_threadCondBroadcast(&s_vmCond);
    }

    while (s_iStackCount > 1)
    {
      hb_threadCondWait(&s_vmCond, &s_vmMtx);
    }

    ++s_iRunningCount;

    HB_VM_UNLOCK();
  }
}

void *hb_vmThreadState(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadState()"));
#endif

  HB_STACK_TLS_PRELOAD

  return hb_stackId() ? hb_stackList() : nullptr;
}

HB_BOOL hb_vmThreadIsMain(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadIsMain(%p)", Cargo));
#endif

  if (!s_fHVMActive || s_main_thread == nullptr)
  {
    return false;
  }
  else if (Cargo)
  {
    return s_main_thread == (static_cast<PHB_THREADSTATE>(Cargo))->pStackId;
  }
  else
  {
    HB_STACK_TLS_PRELOAD
    return s_main_thread == hb_stackId();
  }
}

static void hb_vmStackAdd(PHB_THREADSTATE pState)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStackAdd(%p)", static_cast<void*>(pState)));
#endif

  if (!pState->pPrev)
  {
    if (s_vmStackLst)
    {
      pState->pNext = s_vmStackLst;
      pState->pPrev = s_vmStackLst->pPrev;
      pState->pPrev->pNext = pState;
      s_vmStackLst->pPrev = pState;
    }
    else
    {
      s_vmStackLst = pState->pNext = pState->pPrev = pState;
    }
    s_iStackCount++;
  }
  if (pState->th_no == 0)
  {
    pState->th_no = ++s_threadNo;
  }
}

static PHB_ITEM hb_vmStackDel(PHB_THREADSTATE pState, HB_BOOL fCounter)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStackDel(%p,%d)", static_cast<void*>(pState), static_cast<int>(fCounter)));
#endif

  pState->fActive = false;
  pState->pStackId = nullptr;
  pState->fFinished = true;

  if (pState->pPrev)
  {
    pState->pPrev->pNext = pState->pNext;
    pState->pNext->pPrev = pState->pPrev;
    if (s_vmStackLst == pState)
    {
      s_vmStackLst = pState->pNext;
      if (s_vmStackLst == pState)
      {
        s_vmStackLst = nullptr;
      }
    }
    pState->pPrev = pState->pNext = nullptr;
    if (fCounter)
    {
      s_iStackCount--;
    }
  }

  /* NOTE: releasing pThItm may force pState freeing if parent
   *       thread does not keep thread pointer item. So it's
   *       important to not access it later. [druzus]
   */
  PHB_ITEM pThItm = pState->pThItm;
  pState->pThItm = nullptr;

  return pThItm;
}

static void hb_vmStackInit(PHB_THREADSTATE pState)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStackInit(%p)", static_cast<void*>(pState)));
#endif

  hb_stackInit(); /* initialize HVM thread stack */

  HB_VM_LOCK();
  {
    HB_STACK_TLS_PRELOAD
    hb_stackUnlock();
    pState->pStackId = hb_stackId();
    hb_stackListSet(static_cast<void *>(pState));
    pState->fActive = true;
    hb_vmStackAdd(pState);
  }
  HB_VM_UNLOCK();
}

static void hb_vmStackRelease()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStackRelease()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_VM_LOCK();
  bool fLocked = hb_stackUnlock() == 1;
  PHB_ITEM pThItm = hb_vmStackDel(static_cast<PHB_THREADSTATE>(hb_stackList()), false);
  HB_VM_UNLOCK();

  /* NOTE: releasing pThItm may force pState freeing if parent
   *       thread does not keep thread pointer item. So it's
   *       important to not access it later. [druzus]
   */
  if (pThItm)
  {
    hb_itemRelease(pThItm);
  }

  hb_setRelease(hb_stackSetStruct());
  hb_stackFree();
  hb_threadMutexUnlockAll();
  HB_VM_LOCK();

  if (fLocked)
  {
    s_iRunningCount--;
  }

  s_iStackCount--;
  hb_threadCondBroadcast(&s_vmCond);
  HB_VM_UNLOCK();
}

HB_BOOL hb_vmThreadRegister(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadRegister(%p)", Cargo));
#endif

  HB_VM_LOCK();
  hb_vmStackAdd(static_cast<PHB_THREADSTATE>(Cargo));
  HB_VM_UNLOCK();
  return true;
}

void hb_vmThreadRelease(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadRelease(%p)", Cargo));
#endif

  HB_VM_LOCK();
  PHB_ITEM pThItm = hb_vmStackDel(static_cast<PHB_THREADSTATE>(Cargo), true);
  hb_threadCondBroadcast(&s_vmCond);
  HB_VM_UNLOCK();

  if (pThItm)
  {
    hb_itemRelease(pThItm);
  }
}

/* thread entry point */
void hb_vmThreadInit(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadInit(%p)", Cargo));
#endif

  auto pState = static_cast<PHB_THREADSTATE>(Cargo);
  if (!pState)
  {
    pState = hb_threadStateNew();
  }

  hb_vmStackInit(pState); /* initialize HVM thread stack */
  hb_vmLock();
  {
    HB_STACK_TLS_PRELOAD

    hb_cdpSelectID(pState->pszCDP);
    hb_langSelectID(pState->pszLang);

    hb_vmSetI18N(pState->pI18N);
    pState->pI18N = nullptr;

    if (pState->pSet)
    {
      memcpy(hb_stackSetStruct(), pState->pSet, sizeof(HB_SET_STRUCT));
      hb_xfree(pState->pSet);
      pState->pSet = nullptr;
    }
    else
    {
      hb_setInitialize(hb_stackSetStruct());
    }

    hb_gtAttach(pState->hGT);
    pState->hGT = nullptr;

    if (pState->pszDefRDD)
    {
      hb_stackRDD()->szDefaultRDD = pState->pszDefRDD;
    }

    if (s_fHVMActive)
    {
      /* call __HBVMINIT() function to initialize GetList public variable
       * and set ErrorBlock() by ErrorSys() function
       */
      hb_vmDoInitHVM();
    }

    if (pState->pMemvars)
    {
      hb_memvarRestoreFromArray(pState->pMemvars);
      hb_itemRelease(pState->pMemvars);
      pState->pMemvars = nullptr;
    }
  }
}

/* thread leave point */
void hb_vmThreadQuit(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadQuit()"));
#endif

  HB_STACK_TLS_PRELOAD

  hb_stackSetQuitState(true);
  hb_stackSetActionRequest(0);

  auto pState = static_cast<PHB_THREADSTATE>(hb_stackList());
  {
    auto pReturn = hb_stackReturnItem();

    if (HB_IS_BYREF(pReturn))
    {
      pReturn = hb_itemUnRef(pReturn);
    }

    if (!pState->pResult)
    {
      pState->pResult = hb_itemNew(pReturn);
      hb_gcUnlock(pState->pResult);
    }
    else
    {
      hb_itemCopy(pState->pResult, pReturn);
    }
  }
  hb_itemClear(hb_stackReturnItem());

  hb_stackSetActionRequest(0);
  hb_rddCloseAll();      /* close all workareas */
  hb_stackRemove(1);     /* clear stack items, leave only initial symbol item */
  hb_memvarsClear(true); /* clear all PUBLIC (and PRIVATE if any) variables */
  hb_vmSetI18N(nullptr); /* remove i18n translation table */
#ifndef HB_NO_DEBUG
  hb_vmDebuggerExit(false); /* deactivate debugger */
#endif
  hb_gtRelease(nullptr);
  hb_vmStackRelease(); /* release HVM stack and remove it from linked HVM stacks list */
}

/* send QUIT request to given thread */
void hb_vmThreadQuitRequest(void *Cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadQuitRequest(%p)", Cargo));
#endif

  auto pState = static_cast<PHB_THREADSTATE>(Cargo);

  HB_VM_LOCK();

  if (pState->pStackId && pState->fActive)
  {
    hb_stackIdSetActionRequest(pState->pStackId, HB_QUIT_REQUESTED);
  }

  HB_VM_UNLOCK();
}

#endif /* HB_MT_VM */

PHB_ITEM hb_vmThreadStart(HB_ULONG ulAttr, PHB_CARGO_FUNC pFunc, void *cargo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmThreadStart(%lu,%p,%p)", ulAttr, reinterpret_cast<void*>(pFunc), cargo));
#endif

#if defined(HB_MT_VM)
  return hb_threadStart(ulAttr, pFunc, cargo);
#else
  HB_SYMBOL_UNUSED(ulAttr);
  HB_SYMBOL_UNUSED(pFunc);
  HB_SYMBOL_UNUSED(cargo);
  return nullptr;
#endif /* HB_MT_VM */
}

void hb_vmSetFunction(PHB_SYMB pOldSym, PHB_SYMB pNewSym)
{
  PHB_SYMBOLS pLastSymbols = s_pSymbols;
  HB_SYMB SymOldBuf, SymNewBuf;

  /* make copy of symbols to eliminate possible problem with
   * dynamic modification of passed parameters inside the loop
   */
  memcpy(&SymOldBuf, pOldSym, sizeof(SymOldBuf));
  pOldSym = &SymOldBuf;
  memcpy(&SymNewBuf, pNewSym, sizeof(SymNewBuf));
  pNewSym = &SymNewBuf;

  while (pLastSymbols)
  {
    HB_USHORT uiSymbols = pLastSymbols->uiModuleSymbols;

    for (HB_USHORT ui = 0; ui < uiSymbols; ++ui)
    {
      PHB_SYMB pSym = pLastSymbols->pModuleSymbols + ui;

      if (pSym->value.pFunPtr == pOldSym->value.pFunPtr &&
          (pSym->value.pFunPtr || strcmp(pSym->szName, pOldSym->szName) == 0))
      {
        pSym->value.pFunPtr = pNewSym->value.pFunPtr;
        pSym->scope.value = pNewSym->scope.value;
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

void hb_vmSetDynFunc(PHB_DYNS pDynSym)
{
  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  while (pLastSymbols)
  {
    HB_USHORT uiSymbols = pLastSymbols->uiModuleSymbols;

    for (HB_USHORT ui = 0; ui < uiSymbols; ++ui)
    {
      PHB_SYMB pSym = pLastSymbols->pModuleSymbols + ui;

      if (pSym->pDynSym == pDynSym && pDynSym->pSymbol != pSym)
      {
        pSym->scope.value |= HB_FS_DEFERRED;
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

/* application entry point */

void hb_vmInit(HB_BOOL bStartMainProc)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInit()"));
#endif

#if defined(HB_OS_WIN)
  hb_winmainArgVBuild();
#endif

  hb_xinit();
  hb_vmSetExceptionHandler();
  hb_vmSymbolInit_RT(); /* initialize symbol table with runtime support functions */

#if defined(HB_MT_VM)
  hb_threadInit();
  hb_vmStackInit(hb_threadStateNew()); /* initialize HVM thread stack */
  s_pSymbolsMtx = hb_threadMutexCreate();
#else
  hb_stackInit(); /* initialize HVM stack */
#endif /* HB_MT_VM */
  /* Set the language and codepage to the default */
  /* This trick is needed to stringify the macro value */
  hb_langSelectID(HB_MACRO2STRING(HB_LANG_DEFAULT));
  hb_cdpSelectID(HB_MACRO2STRING(HB_CODEPAGE_DEFAULT));
  {
    HB_STACK_TLS_PRELOAD
    s_main_thread = hb_stackId();
    /* _SET_* initialization */
    hb_setInitialize(hb_stackSetStruct());
  }

  hb_cmdargUpdate();

  hb_clsInit(); /* initialize Classy/OO system */

  hb_errInit();
  hb_breakBlock();

  /* initialize dynamic symbol for evaluating codeblocks and break function */
  hb_symEval.pDynSym = hb_dynsymGetCase(hb_symEval.szName);
  s_symBreak.pDynSym = hb_dynsymGetCase(s_symBreak.szName);

  hb_conInit();

  /* Check for some internal switches */
  hb_cmdargProcess();

  hb_i18n_init(); /* initialize i18n module */

#ifndef HB_NO_PROFILER
  /* Initialize opcodes profiler support arrays */
  {
    for (HB_ULONG ul = 0; ul < HB_P_LAST_PCODE; ul++)
    {
      hb_ulOpcodesCalls[ul] = 0;
      hb_ulOpcodesTime[ul] = 0;
    }
  }
#endif

  /* enable executing PCODE (HVM reenter request) */
  s_fHVMActive = true;

  /* lock main HVM thread */
  hb_vmLock();

#ifndef HB_NO_DEBUG
  s_pDynsDbgEntry = hb_dynsymFind("__DBGENTRY");
  if (s_pDynsDbgEntry)
  {
    /* Try to get C dbgEntry() function pointer */
    if (!s_pFunDbgEntry)
    {
      hb_vmDebugEntry(HB_DBG_GETENTRY, 0, nullptr, 0, nullptr);
    }
    if (!s_pFunDbgEntry)
    {
      s_pFunDbgEntry = hb_vmDebugEntry;
    }
  }
#endif

  /* Call functions that initializes static variables
   * Static variables have to be initialized before any INIT functions
   * because INIT function can use static variables
   */
  hb_vmDoInitStatics();

  /* call __HBVMINIT() function to initialize GetList public variable
   * and set ErrorBlock() by ErrorSys() function.
   */
  hb_vmDoInitHVM();

  hb_clsDoInit(); /* initialize Class(y) .prg functions */

  hb_vmDoModuleInitFunctions(); /* process AtInit registered functions */
  hb_vmDoInitFunctions(true);   /* process registered CLIPINIT INIT procedures */
  hb_vmDoInitFunctions(false);  /* process registered other INIT procedures */

  /* Call __SetHelpK() function to redirect K_F1 to HELP() function
   * if it is linked. CA-Cl*pper calls it after INIT PROCEDUREs and
   * before executing the application entry function.
   */
  hb_vmDoInitHelp();

  /* This is undocumented CA-Cl*pper, if there's a function called _APPMAIN()
     it will be executed first. [vszakats] */
  {
    auto pDynSym = hb_dynsymFind("_APPMAIN");

    if (pDynSym && pDynSym->pSymbol->value.pFunPtr)
    {
      s_pSymStart = pDynSym->pSymbol;
    }
    else
    {
      /* if first char is '@' then start procedure were set by
       * programmer explicitly and should have the highest priority
       * otherwise it's the name of first public function in
       * first linked module which is used if there is no
       * HB_START_PROCEDURE in code
       */
      const char *pszMain;

      if (s_vm_pszLinkedMain && *s_vm_pszLinkedMain == '@')
      {
        pszMain = s_vm_pszLinkedMain + 1;
        pDynSym = hb_dynsymFind(pszMain);
      }
      else
      {
#ifndef HB_START_PROCEDURE
        pszMain = nullptr;
#else
        pszMain = HB_START_PROCEDURE;
        pDynSym = hb_dynsymFind(pszMain);
        if (!(pDynSym && pDynSym->pSymbol->value.pFunPtr))
#endif
        {
          if (s_vm_pszLinkedMain)
          {
            pszMain = s_vm_pszLinkedMain;
            pDynSym = hb_dynsymFind(pszMain);
          }
        }
      }

      if (pDynSym && pDynSym->pSymbol->value.pFunPtr)
      {
        s_pSymStart = pDynSym->pSymbol;
      }
#ifdef HB_START_PROC_STRICT
      else
      {
        /* clear startup symbol set by initialization code */
        s_pSymStart = nullptr;
      }
#endif

#ifndef HB_CLP_STRICT
      if (bStartMainProc && !s_pSymStart)
      {
        if (pszMain)
        {
          hb_errInternal(HB_EI_VMBADSTARTUP, nullptr, pszMain, nullptr);
        }
        else
        {
          hb_errInternal(HB_EI_VMNOSTARTUP, nullptr, nullptr, nullptr);
        }
      }
#endif
    }
  }

  if (bStartMainProc && s_pSymStart)
  {
    hb_vmPushSymbol(s_pSymStart);                           /* pushes first HB_FS_PUBLIC defined symbol to the stack */
    hb_vmPushNil();                                         /* places NIL at self */
    hb_vmProc(static_cast<HB_USHORT>(hb_cmdargPushArgs())); /* invoke it with number of supplied parameters */
  }
}

int hb_vmQuit(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmQuit()"));
#endif

  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  hb_vmTerminateThreads();
#endif

  hb_vmDoExitFunctions();       /* process defined EXIT functions */
  hb_vmDoModuleExitFunctions(); /* process AtExit registered functions */

  /* release all known items stored in subsystems */
  hb_itemClear(hb_stackReturnItem());
  hb_stackRemove(1); /* clear stack items, leave only initial symbol item */

  /* intentionally here to allow executing object destructors for all
   * cross referenced items before we release classy subsystem
   */
  hb_gcCollectAll(true);

  /* Clear any pending actions so RDD shutdown process
   * can be cleanly executed
   */
  hb_stackSetActionRequest(0);
  hb_rddCloseAll();      /* close all workareas */
  hb_rddShutDown();      /* remove all registered RDD drivers */
  hb_memvarsClear(true); /* clear all PUBLIC (and PRIVATE if any) variables */
  hb_vmSetI18N(nullptr); /* remove i18n translation table */
  hb_i18n_exit();        /* unregister i18n module */

  hb_itemClear(hb_stackReturnItem());
  hb_gcCollectAll(true);
#ifndef HB_NO_DEBUG
  /* deactivate debugger */
  hb_vmDebuggerExit(true);
#endif

  /* stop executing PCODE (HVM reenter request) */
  s_fHVMActive = false;

  hb_vmStaticsClear();

  /* release thread specific data */
  hb_stackDestroyTSD();

  hb_breakBlockRelease();
  hb_errExit();
  hb_clsReleaseAll();

  hb_vmStaticsRelease();

  /* release all remaining items */

  hb_conRelease();            /* releases Console */
  hb_vmReleaseLocalSymbols(); /* releases the local modules linked list */
  hb_dynsymRelease();         /* releases the dynamic symbol table */
  hb_itemClear(hb_stackReturnItem());
  hb_gcCollectAll(true);

  hb_vmDoModuleQuitFunctions(); /* process AtQuit registered functions */
  hb_vmCleanModuleFunctions();

#if defined(HB_MT_VM)
  hb_vmStackRelease(); /* release HVM stack and remove it from linked HVM stacks list */
  if (s_pSymbolsMtx)
  {
    hb_itemRelease(s_pSymbolsMtx);
    s_pSymbolsMtx = nullptr;
  }
  hb_threadExit();
#else
  hb_setRelease(hb_stackSetStruct()); /* releases Sets */
  hb_stackFree();
#endif /* HB_MT_VM */

  hb_langReleaseAll(); /* release lang modules */
  hb_cdpReleaseAll();  /* releases codepages */

  /* release all known garbage */
  if (hb_xquery(HB_MEM_STATISTICS) == 0)
  { /* check if fmstat is ON */
    hb_gcReleaseAll();
  }

  hb_vmUnsetExceptionHandler();

  hb_xexit();

#if defined(HB_OS_WIN)
  hb_winmainArgVFree();
#endif

  return s_nErrorLevel;
}

void hb_vmExecute(const HB_BYTE *pCode, PHB_SYMB pSymbols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmExecute(%p, %p)", static_cast<const void*>(pCode), static_cast<void*>(pSymbols)));
#endif

  HB_STACK_TLS_PRELOAD
  auto bCanRecover = false;
  bool bDynCode = pSymbols == nullptr || (pSymbols->scope.value & HB_FS_DYNCODE) != 0;

#ifndef HB_NO_PROFILER
  HB_ULONG ulLastOpcode = 0; /* opcodes profiler support */
  HB_ULONG ulPastClock = 0;  /* opcodes profiler support */
#endif
#ifndef HB_GUI
  int *piKeyPolls = hb_stackKeyPolls();
#endif

#ifndef HB_NO_PROFILER
  if (hb_bProfiler)
  {
    ulPastClock = static_cast<HB_ULONG>(clock());
  }
#endif

  for (;;)
  {
#ifndef HB_NO_PROFILER
    if (hb_bProfiler)
    {
      auto ulActualClock = static_cast<HB_ULONG>(clock());

      hb_ulOpcodesTime[ulLastOpcode] += (ulActualClock - ulPastClock);
      ulPastClock = ulActualClock;
      ulLastOpcode = pCode[0];
      hb_ulOpcodesCalls[ulLastOpcode]++;
    }
#endif

#ifndef HB_GUI
    if (!--(*piKeyPolls))
    {
      if (s_fKeyPool)
      {
        hb_inkeyPoll();
      }
      *piKeyPolls = 65536;

      /* IMHO we should have a _SET_ controlled by user
       * something like:

      if( hb_stackSetStruct()->HB_SET_KEYPOLL ) {
         hb_inkeyPoll();
         *piKeyPolls = hb_stackSetStruct()->HB_SET_KEYPOLL;
      }

      for some GTs which can work in asynchronous mode user may
      set it to 0 (or if he doesn't need any inkey poll) and
      when ALT+C/ALT+D is pressed (or any other platform dependent
      key combination) they should set proper flags in
      ActionRequest so we can serve it in main VM loop without
      performance decrease or ignore depending on
      hb_stackSetStruct()->HB_SET_CANCEL,
      hb_stackSetStruct()->HB_SET_DEBUG flags
      */
    }
#endif
#if defined(HB_MT_VM)
    if (hb_vmThreadRequest)
    {
      hb_vmRequestTest();
    }
#endif

    switch (pCode[0])
    {
      /* Operators (mathematical / character / misc) */

    case HB_P_NEGATE:
      hb_vmNegate();
      pCode++;
      break;

    case HB_P_PLUS:
      hb_vmPlus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_PLUSEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmPlus(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_PLUSEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmPlus(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_MINUS:
      hb_vmMinus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_MINUSEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmMinus(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_MINUSEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmMinus(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_MULT:
      hb_vmMult(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_MULTEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmMult(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_MULTEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmMult(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_DIVIDE:
      hb_vmDivide(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_DIVEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmDivide(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_DIVEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmDivide(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_MODULUS:
      hb_vmModulus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_MODEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmModulus(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_MODEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmModulus(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_POWER:
      hb_vmPower(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_EXPEQ: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      auto pValue = hb_stackItemFromTop(-1);
      hb_vmPower(pResult, pResult, pValue);
      hb_itemCopy(pValue, pResult);
      hb_itemMove(hb_stackItemFromTop(-2), pValue);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_EXPEQPOP: {
      auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
      hb_vmPower(pResult, pResult, hb_stackItemFromTop(-1));
      hb_stackPop();
      hb_stackPop();
      pCode++;
      break;
    }

    case HB_P_INC:
      hb_vmInc(hb_stackItemFromTop(-1));
      pCode++;
      break;

    case HB_P_INCEQ: {
      auto pResult = hb_stackItemFromTop(-1);
      auto pValue = hb_itemUnRef(pResult);
      hb_vmInc(pValue);
      auto pTemp = hb_stackAllocItem();
      hb_itemCopy(pTemp, pValue);
      hb_itemMove(pResult, pTemp);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_INCEQPOP:
      hb_vmInc(hb_itemUnRef(hb_stackItemFromTop(-1)));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_DEC:
      hb_vmDec(hb_stackItemFromTop(-1));
      pCode++;
      break;

    case HB_P_DECEQ: {
      auto pResult = hb_stackItemFromTop(-1);
      auto pValue = hb_itemUnRef(pResult);
      hb_vmDec(pValue);
      auto pTemp = hb_stackAllocItem();
      hb_itemCopy(pTemp, pValue);
      hb_itemMove(pResult, pTemp);
      hb_stackDec();
      pCode++;
      break;
    }

    case HB_P_DECEQPOP:
      hb_vmDec(hb_itemUnRef(hb_stackItemFromTop(-1)));
      hb_stackPop();
      pCode++;
      break;

    case HB_P_FUNCPTR:
      hb_vmFuncPtr();
      pCode++;
      break;

      /* Operators (relational) */

    case HB_P_EQUAL:
      hb_vmEqual();
      pCode++;
      break;

    case HB_P_EXACTLYEQUAL:
      hb_vmExactlyEqual();
      pCode++;
      break;

    case HB_P_NOTEQUAL:
      hb_vmNotEqual();
      pCode++;
      break;

    case HB_P_LESS:
      hb_vmLess();
      pCode++;
      break;

    case HB_P_LESSEQUAL:
      hb_vmLessEqual();
      pCode++;
      break;

    case HB_P_GREATER:
      hb_vmGreater();
      pCode++;
      break;

    case HB_P_GREATEREQUAL:
      hb_vmGreaterEqual();
      pCode++;
      break;

    case HB_P_INSTRING:
      hb_vmInstring();
      pCode++;
      break;

    case HB_P_FORTEST:
      hb_vmForTest();
      pCode++;
      break;

    case HB_P_ENUMSTART:
      hb_vmEnumStart(static_cast<unsigned char>(pCode[1]), static_cast<unsigned char>(pCode[2]));
      pCode += 3;
      break;

    case HB_P_ENUMNEXT:
      hb_vmEnumNext();
      pCode++;
      break;

    case HB_P_ENUMPREV:
      hb_vmEnumPrev();
      pCode++;
      break;

    case HB_P_ENUMEND:
      hb_vmEnumEnd();
      pCode++;
      break;

    case HB_P_SWITCH:
      pCode = hb_vmSwitch(pCode + 3, HB_PCODE_MKUSHORT(&pCode[1]));
      break;

      /* Operators (logical) */

    case HB_P_NOT:
      hb_vmNot();
      pCode++;
      break;

    case HB_P_AND:
      hb_vmAnd();
      pCode++;
      break;

    case HB_P_OR:
      hb_vmOr();
      pCode++;
      break;

      /* Array */

    case HB_P_ARRAYPUSH:
      hb_vmArrayPush();
      pCode++;
      break;

    case HB_P_ARRAYPUSHREF:
      hb_vmArrayPushRef();
      pCode++;
      break;

    case HB_P_ARRAYPOP:
      hb_vmArrayPop();
      pCode++;
      break;

    case HB_P_ARRAYDIM:
      hb_vmArrayDim(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_ARRAYGEN:
      hb_vmArrayGen(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_HASHGEN:
      hb_vmHashGen(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

      /* Object */

    case HB_P_MESSAGE:
      hb_vmPushSymbol(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

      /* Database */

    case HB_P_SWAPALIAS:
      hb_vmSwapAlias();
      pCode++;
      break;

      /* Execution */

    case HB_P_DO:
      hb_vmProc(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_DOSHORT:
      hb_vmProc(pCode[1]);
      pCode += 2;
      break;

    case HB_P_FUNCTION:
      hb_itemSetNil(hb_stackReturnItem());
      hb_vmProc(HB_PCODE_MKUSHORT(&pCode[1]));
      hb_stackPushReturn();
      pCode += 3;
      break;

    case HB_P_FUNCTIONSHORT:
      hb_itemSetNil(hb_stackReturnItem());
      hb_vmProc(pCode[1]);
      hb_stackPushReturn();
      pCode += 2;
      break;

    case HB_P_SEND:
      hb_itemSetNil(hb_stackReturnItem());
      hb_vmSend(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      /* Small opt */
      if (pCode[0] == HB_P_POP)
      {
        pCode++;
      }
      else
      {
        hb_stackPushReturn();
      }
      break;

    case HB_P_SENDSHORT:
      hb_itemSetNil(hb_stackReturnItem());
      hb_vmSend(pCode[1]);
      pCode += 2;
      /* Small opt */
      if (pCode[0] == HB_P_POP)
      {
        pCode++;
      }
      else
      {
        hb_stackPushReturn();
      }
      break;

    case HB_P_PUSHOVARREF:
      hb_vmPushObjectVarRef();
      pCode++;
      break;

    case HB_P_LINE:
#if 0
            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINE: %s (%i)", hb_stackBaseItem()->item.asSymbol.value->szName, hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo));
#endif

      hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = HB_PCODE_MKUSHORT(&pCode[1]);
#ifndef HB_NO_DEBUG
      if (hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging)
      {
        hb_vmDebuggerShowLine(hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo);
      }
#endif
      pCode += 3;
      break;

    case HB_P_PARAMETER:
      hb_memvarNewParameter(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]), hb_stackItemFromBase(pCode[3]));
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopParameter)"));
#endif
      pCode += 4;
      break;

    case HB_P_FRAME:
      hb_vmFrame(static_cast<unsigned char>(pCode[1]), static_cast<unsigned char>(pCode[2]));
      pCode += 3;
      break;

    case HB_P_VFRAME:
      hb_vmVFrame(static_cast<unsigned char>(pCode[1]), static_cast<unsigned char>(pCode[2]));
      pCode += 3;
      break;

    case HB_P_LARGEFRAME:
      hb_vmFrame(HB_PCODE_MKUSHORT(&pCode[1]), static_cast<unsigned char>(pCode[3]));
      pCode += 4;
      break;

    case HB_P_LARGEVFRAME:
      hb_vmVFrame(HB_PCODE_MKUSHORT(&pCode[1]), static_cast<unsigned char>(pCode[3]));
      pCode += 4;
      break;

    case HB_P_SFRAME:
      hb_vmSFrame(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_STATICS:
      hb_vmStatics(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]), HB_PCODE_MKUSHORT(&pCode[3]));
      pCode += 5;
      break;

    case HB_P_THREADSTATICS: {
      HB_USHORT uiCount = HB_PCODE_MKUSHORT(&pCode[1]);
      hb_vmInitThreadStatics(uiCount, &pCode[3]);
      pCode += 3 + (static_cast<HB_ULONG>(uiCount) << 1);
      break;
    }

    case HB_P_LOCALNAME:
#ifndef HB_NO_DEBUG
      hb_vmLocalName(HB_PCODE_MKUSHORT(&pCode[1]), reinterpret_cast<const char *>(pCode) + 3);
#endif
      pCode += 3;
      while (*pCode++)
      {
        ;
      }
      break;

    case HB_P_STATICNAME:
#ifndef HB_NO_DEBUG
      hb_vmStaticName(pCode[1], HB_PCODE_MKUSHORT(&pCode[2]), reinterpret_cast<const char *>(pCode) + 4);
#endif
      pCode += 4;
      while (*pCode++)
      {
        ;
      }
      break;

    case HB_P_MODULENAME:
#ifndef HB_NO_DEBUG
      hb_vmModuleName(reinterpret_cast<const char *>(pCode) + 1);
#endif
      pCode++;
      while (*pCode++)
      {
        ;
      }
      break;

    case HB_P_RETVALUE:
      hb_stackPopReturn();
      hb_stackReturnItem()->type &= ~Harbour::Item::MEMOFLAG;
      pCode++;
      break;

    case HB_P_ENDBLOCK:
#if 0
            HB_TRACE(HB_TR_INFO, ("HB_P_ENDBLOCK"));
#endif
      hb_stackPopReturn();
      /* manually inlined hb_vmRequestEndProc() for some C compilers
       * which does not make such optimization
       */
      hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
      break;

    case HB_P_ENDPROC:
#if 0
            HB_TRACE(HB_TR_INFO, ("HB_P_ENDPROC"));
#endif
      /* manually inlined hb_vmRequestEndProc() for some C compilers
       * which does not make such optimization
       */
      hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
      break;

      /* BEGIN SEQUENCE/RECOVER/ALWAYS/END SEQUENCE */

    case HB_P_SEQBLOCK:
      hb_vmSeqBlock();
      pCode++;
      break;

    case HB_P_SEQALWAYS: {
      /*
       * Create the SEQUENCE envelope
       * [ break return value ]  -2
       * [ recover envelope   ]  -1
       * [                    ] <- new recover base
       */

      /*
       * 1) clear the storage for value returned by BREAK statement
       */
      hb_stackAllocItem()->type = Harbour::Item::NIL;

      /*
       * 2) recover data
       */
      auto pItem = hb_stackAllocItem();
      /* mark type as NIL - it's not real item */
      pItem->type = Harbour::Item::RECOVER;
      /* store the address of RECOVER or END opcode */
      pItem->item.asRecover.recover = pCode + HB_PCODE_MKINT24(&pCode[1]);
      /* store current RECOVER base */
      pItem->item.asRecover.base = hb_stackGetRecoverBase();
      /* store current bCanRecover flag - in a case of nested sequences */
      pItem->item.asRecover.flags = HB_SEQ_DOALWAYS | (bCanRecover ? HB_SEQ_CANRECOVER : 0);
      /* clear new recovery state */
      pItem->item.asRecover.request = 0;

      /*
       * set new recover base
       */
      hb_stackSetRecoverBase(hb_stackTopOffset());
      /*
       * we are now inside a valid SEQUENCE envelope
       */
      bCanRecover = true;
      pCode += 4;
      break;
    }

    case HB_P_ALWAYSBEGIN:
#if defined(_HB_RECOVER_DEBUG)
      if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
      {
        hb_errInternal(HB_EI_ERRUNRECOV, "HB_P_ALWAYSBEGIN", nullptr, nullptr);
      }
#endif
      /* change the recover address to ALWAYSEND opcode */
      hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.recover = pCode + HB_PCODE_MKINT24(&pCode[1]);
      /* store and reset action */
      hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags |=
          hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request;
      hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request = 0;
      /* store RETURN value */
      if (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_ENDPROC_REQUESTED)
      {
        hb_itemMove(hb_stackItemFromTop(HB_RECOVER_VALUE), hb_stackReturnItem());
      }
      pCode += 4;
      break;

    case HB_P_ALWAYSEND: {
#if defined(_HB_RECOVER_DEBUG)
      if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
      {
        hb_errInternal(HB_EI_ERRUNRECOV, "HB_P_ALWAYSEND", nullptr, nullptr);
      }
#endif
      HB_USHORT uiPrevAction = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags;
      HB_USHORT uiCurrAction = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request;

      /* restore previous recovery base */
      bCanRecover = (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_CANRECOVER) != 0;
      hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);

      /* restore requested action */
      if ((uiCurrAction | uiPrevAction) & HB_QUIT_REQUESTED)
      {
        hb_stackSetActionRequest(HB_QUIT_REQUESTED);
      }
      else if ((uiCurrAction | uiPrevAction) & HB_BREAK_REQUESTED)
      {
        hb_stackSetActionRequest(HB_BREAK_REQUESTED);
      }
      else if ((uiCurrAction | uiPrevAction) & HB_ENDPROC_REQUESTED)
      {
        hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
      }
      else
      {
        hb_stackSetActionRequest(0);
      }

      /* Remove the ALWAYS envelope */
      hb_stackDec();

      /* restore RETURN value if not overloaded inside ALWAYS code */
      if (!(uiCurrAction & HB_ENDPROC_REQUESTED) && (uiPrevAction & HB_ENDPROC_REQUESTED))
      {
        hb_stackPopReturn();
      }
      else
      {
        hb_stackPop();
      }
      pCode++;
      break;
    }

    case HB_P_SEQBEGIN: {
      /*
       * Create the SEQUENCE envelope
       * [ break return value ]  -2
       * [ recover envelope   ]  -1
       * [                    ] <- new recover base
       */

      /*
       * 1) clear the storage for value returned by BREAK statement
       */
      hb_stackAllocItem()->type = Harbour::Item::NIL;

      /*
       * 2) recover data
       */
      auto pItem = hb_stackAllocItem();
      /* mark type as NIL - it's not real item */
      pItem->type = Harbour::Item::RECOVER;
      /* store the address of RECOVER or END opcode */
      pItem->item.asRecover.recover = pCode + HB_PCODE_MKINT24(&pCode[1]);
      /* store current RECOVER base */
      pItem->item.asRecover.base = hb_stackGetRecoverBase();
      /* store current bCanRecover flag - in a case of nested sequences */
      pItem->item.asRecover.flags = bCanRecover ? HB_SEQ_CANRECOVER : 0;
      /* clear new recovery state */
      pItem->item.asRecover.request = 0;

      /*
       * set new recover base
       */
      hb_stackSetRecoverBase(hb_stackTopOffset());
      /*
       * we are now inside a valid SEQUENCE envelope
       */
      bCanRecover = true;
      pCode += 4;
      break;
    }

    case HB_P_SEQEND:
      /*
       * Remove the SEQUENCE envelope
       * This is executed either at the end of sequence or as the
       * response to the break statement if there is no RECOVER clause
       */
#if defined(_HB_RECOVER_DEBUG)
      if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
      {
        hb_errInternal(HB_EI_ERRUNRECOV, "HB_P_SEQEND", nullptr, nullptr);
      }
#endif
      /*
       * 2) Restore previous recovery state
       */
      bCanRecover = (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_CANRECOVER) != 0;
      hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
      hb_stackDec();

      /*
       * 1) Discard the value returned by BREAK statement - there
       * was no RECOVER clause or there was no BREAK statement
       */
      hb_stackPop();
      /*
       * skip outside of SEQUENCE structure
       */
      pCode += HB_PCODE_MKINT24(&pCode[1]);
      break;

    case HB_P_SEQRECOVER:
      /*
       * Execute the RECOVER code
       */
#if defined(_HB_RECOVER_DEBUG)
      if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
      {
        hb_errInternal(HB_EI_ERRUNRECOV, "HB_P_SEQRECOVER", nullptr, nullptr);
      }
#endif
      /*
       * 2) Restore previous recovery state
       */
      bCanRecover = (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_CANRECOVER) != 0;
      hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
      hb_stackDec();
      /*
       * 1) Leave the value returned from BREAK  - it will be popped
       * in next executed opcode
       */
      pCode++;
      break;

      /* Jumps */

    case HB_P_JUMPNEAR:
      pCode += static_cast<signed char>(pCode[1]);
      break;

    case HB_P_JUMP:
      pCode += HB_PCODE_MKSHORT(&pCode[1]);
      break;

    case HB_P_JUMPFAR:
      pCode += HB_PCODE_MKINT24(&pCode[1]);
      break;

    case HB_P_JUMPFALSENEAR:
      if (!hb_vmPopLogical())
      {
        pCode += static_cast<signed char>(pCode[1]);
      }
      else
      {
        pCode += 2;
      }
      break;

    case HB_P_JUMPFALSE:
      if (!hb_vmPopLogical())
      {
        pCode += HB_PCODE_MKSHORT(&pCode[1]);
      }
      else
      {
        pCode += 3;
      }
      break;

    case HB_P_JUMPFALSEFAR:
      if (!hb_vmPopLogical())
      {
        pCode += HB_PCODE_MKINT24(&pCode[1]);
      }
      else
      {
        pCode += 4;
      }
      break;

    case HB_P_JUMPTRUENEAR:
      if (hb_vmPopLogical())
      {
        pCode += static_cast<signed char>(pCode[1]);
      }
      else
      {
        pCode += 2;
      }
      break;

    case HB_P_JUMPTRUE:
      if (hb_vmPopLogical())
      {
        pCode += HB_PCODE_MKSHORT(&pCode[1]);
      }
      else
      {
        pCode += 3;
      }
      break;

    case HB_P_JUMPTRUEFAR:
      if (hb_vmPopLogical())
      {
        pCode += HB_PCODE_MKINT24(&pCode[1]);
      }
      else
      {
        pCode += 4;
      }
      break;

      /* Push */

    case HB_P_TRUE: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::LOGICAL;
      pItem->item.asLogical.value = true;
      pCode++;
      break;
    }

    case HB_P_FALSE: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::LOGICAL;
      pItem->item.asLogical.value = false;
      pCode++;
      break;
    }

    case HB_P_ONE: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::INTEGER;
      pItem->item.asInteger.value = 1;
      pItem->item.asInteger.length = 10;
#if 0
            HB_TRACE(HB_TR_INFO, ("(HB_P_ONE)"));
#endif
      pCode++;
      break;
    }

    case HB_P_ZERO: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::INTEGER;
      pItem->item.asInteger.value = 0;
      pItem->item.asInteger.length = 10;
#if 0
            HB_TRACE(HB_TR_INFO, ("(HB_P_ZERO)"));
#endif
      pCode++;
      break;
    }

    case HB_P_PUSHNIL:
      hb_stackAllocItem()->type = Harbour::Item::NIL;
#if 0
            HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHNIL)"));
#endif
      pCode++;
      break;

    case HB_P_PUSHBYTE: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::INTEGER;
      pItem->item.asInteger.value = static_cast<signed char>(pCode[1]);
      pItem->item.asInteger.length = 10;
#if 0
            HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHBYTE)"));
#endif
      pCode += 2;
      break;
    }

    case HB_P_PUSHINT: {
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::INTEGER;
      pItem->item.asInteger.value = HB_PCODE_MKSHORT(&pCode[1]);
      pItem->item.asInteger.length = 10;
#if 0
            HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHINT)"));
#endif
      pCode += 3;
      break;
    }

    case HB_P_PUSHLONG:
#if 0
            HB_TRACE(HB_TR_DEBUG, ("(HB_P_PUSHLONG)"));
#endif
#if HB_VMINT_MAX >= INT32_MAX
      hb_vmPushIntegerConst(static_cast<int>(HB_PCODE_MKLONG(&pCode[1])));
#else
      hb_vmPushLongConst(static_cast<long>(HB_PCODE_MKLONG(&pCode[1])));
#endif
      pCode += 5;
      break;

    case HB_P_PUSHLONGLONG:
#if 0
            HB_TRACE(HB_TR_DEBUG, ("(HB_P_PUSHLONGLONG)"));
#endif
#if !defined(HB_LONG_LONG_OFF)
      hb_vmPushLongLongConst(HB_PCODE_MKLONGLONG(&pCode[1]));
#else
      hb_vmPushDoubleConst(HB_PCODE_MKLONGLONG(&pCode[1]), HB_DEFAULT_WIDTH, HB_DEFAULT_DECIMALS);
#endif
      pCode += 9;
      break;

    case HB_P_PUSHDOUBLE:
      hb_vmPushDoubleConst(HB_PCODE_MKDOUBLE(&pCode[1]),
                           static_cast<int>(*static_cast<const unsigned char *>(&pCode[1 + sizeof(double)])),
                           static_cast<int>(*static_cast<const unsigned char *>(&pCode[2 + sizeof(double)])));
      pCode += 3 + sizeof(double);
      break;

    case HB_P_PUSHSTRSHORT:
      if (bDynCode)
      {
        hb_vmPushString(reinterpret_cast<const char *>(pCode) + 2, static_cast<HB_SIZE>(pCode[1]) - 1);
      }
      else
      {
        hb_vmPushStringPcode(reinterpret_cast<const char *>(pCode) + 2, static_cast<HB_SIZE>(pCode[1]) - 1);
      }
      pCode += 2 + pCode[1];
      break;

    case HB_P_PUSHSTR: {
      HB_USHORT uiSize = HB_PCODE_MKUSHORT(&pCode[1]);
      if (bDynCode)
      {
        hb_vmPushString(reinterpret_cast<const char *>(pCode) + 3, uiSize - 1);
      }
      else
      {
        hb_vmPushStringPcode(reinterpret_cast<const char *>(pCode) + 3, uiSize - 1);
      }
      pCode += 3 + uiSize;
      break;
    }

    case HB_P_PUSHSTRLARGE: {
      HB_SIZE nSize = HB_PCODE_MKUINT24(&pCode[1]);
      if (bDynCode)
      {
        hb_vmPushString(reinterpret_cast<const char *>(pCode) + 4, nSize - 1);
      }
      else
      {
        hb_vmPushStringPcode(reinterpret_cast<const char *>(pCode) + 4, nSize - 1);
      }
      pCode += 4 + nSize;
      break;
    }

    case HB_P_PUSHSTRHIDDEN: {
      auto nSize = static_cast<HB_SIZE>(HB_PCODE_MKUSHORT(&pCode[2]));
      char *szText = hb_compDecodeString(pCode[1], reinterpret_cast<const char *>(pCode) + 4, &nSize);
      hb_itemPutCLPtr(hb_stackAllocItem(), szText, nSize);
      pCode += (4 + nSize);
      break;
    }

    case HB_P_PUSHDATE: {
#if 0
            HB_TRACE(HB_TR_DEBUG, ("(HB_P_PUSHDATE)"));
#endif
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::DATE;
      pItem->item.asDateTime.julian = static_cast<long>(HB_PCODE_MKLONG(&pCode[1]));
      pItem->item.asDateTime.time = 0;
      pCode += 5;
      break;
    }

    case HB_P_PUSHTIMESTAMP: {
#if 0
            HB_TRACE(HB_TR_DEBUG, ("(HB_P_PUSHTIMESTAMP)"));
#endif
      auto pItem = hb_stackAllocItem();
      pItem->type = Harbour::Item::TIMESTAMP;
      pItem->item.asDateTime.julian = static_cast<long>(HB_PCODE_MKLONG(&pCode[1]));
      pItem->item.asDateTime.time = static_cast<long>(HB_PCODE_MKLONG(&pCode[5]));
      pCode += 9;
      break;
    }

    case HB_P_PUSHBLOCK: {
      /* +0    -> _pushblock
       * +1 +2 -> size of codeblock
       * +3 +4 -> number of expected parameters
       * +5 +6 -> number of referenced local variables
       * +7    -> start of table with referenced local variables
       */
      HB_SIZE nSize = HB_PCODE_MKUSHORT(&pCode[1]);
      hb_vmPushBlock(pCode + 3, pSymbols, bDynCode ? nSize - 7 : 0);
      pCode += nSize;
      break;
    }
    case HB_P_PUSHBLOCKLARGE: {
      /* +0       -> _pushblock
       * +1 +2 +3 -> size of codeblock
       * +4 +5    -> number of expected parameters
       * +6 +7    -> number of referenced local variables
       * +8       -> start of table with referenced local variables
       */
      HB_SIZE nSize = HB_PCODE_MKUINT24(&pCode[1]);
      hb_vmPushBlock(pCode + 4, pSymbols, bDynCode ? nSize - 8 : 0);
      pCode += nSize;
      break;
    }
    case HB_P_PUSHBLOCKSHORT: {
      /* +0    -> _pushblock
       * +1    -> size of codeblock
       */
      HB_SIZE nSize = pCode[1];
      hb_vmPushBlockShort(pCode + 2, pSymbols, bDynCode ? nSize - 2 : 0);
      pCode += nSize;
      break;
    }

    case HB_P_PUSHSELF:
      hb_vmPush(hb_stackSelfItem());
      pCode++;
      break;

    case HB_P_PUSHSYM:
      hb_vmPushSymbol(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHSYMNEAR:
      hb_vmPushSymbol(pSymbols + pCode[1]);
      pCode += 2;
      break;

    case HB_P_PUSHFUNCSYM:
      hb_vmPushSymbol(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      hb_stackAllocItem()->type = Harbour::Item::NIL;
      pCode += 3;
      break;

    case HB_P_PUSHALIAS:
      hb_vmPushAlias();
      pCode++;
      break;

    case HB_P_PUSHALIASEDFIELD:
      hb_vmPushAliasedField(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHALIASEDFIELDNEAR:
      hb_vmPushAliasedField(pSymbols + pCode[1]);
      pCode += 2;
      break;

    case HB_P_PUSHALIASEDVAR:
      hb_vmPushAliasedVar(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHFIELD:
      /* It pushes the current value of the given field onto the eval stack
       */
      hb_rddGetFieldValue(hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushField)"));
#endif
      pCode += 3;
      break;

    case HB_P_PUSHLOCAL:
      hb_vmPushLocal(HB_PCODE_MKSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHLOCALNEAR:
      hb_vmPushLocal(static_cast<signed char>(pCode[1]));
      pCode += 2; /* only first two bytes are used */
      break;

    case HB_P_PUSHLOCALREF:
      hb_vmPushLocalByRef(HB_PCODE_MKSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHSTATIC:
      hb_vmPushStatic(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHSTATICREF:
      hb_vmPushStaticByRef(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_PUSHMEMVAR:
      hb_memvarGetValue(hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvar)"));
#endif
      pCode += 3;
      break;

    case HB_P_PUSHMEMVARREF:
      hb_memvarGetRefer(hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvarRef)"));
#endif
      pCode += 3;
      break;

    case HB_P_PUSHVARIABLE:
      /* Push a value of variable of unknown type onto the eval stack
       */
      hb_vmPushVariable(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_DUPLICATE:
      hb_vmDuplicate();
      pCode++;
      break;

    case HB_P_DUPLUNREF:
      hb_vmDuplUnRef();
      pCode++;
      break;

    case HB_P_PUSHUNREF:
      hb_vmPushUnRef();
      pCode++;
      break;

    case HB_P_PUSHVPARAMS:
      hb_vmPushVParams();
      pCode++;
      break;

    case HB_P_PUSHAPARAMS:
      hb_vmPushAParams();
      pCode++;
      break;

    case HB_P_SWAP:
      hb_vmSwap(static_cast<unsigned char>(pCode[1]));
      pCode += 2;
      break;

      /* Pop */

    case HB_P_POP:
      hb_stackPop();
      pCode++;
      break;

    case HB_P_POPALIAS:
      hb_vmPopAlias();
      pCode++;
      break;

    case HB_P_POPALIASEDFIELD:
      hb_vmPopAliasedField(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_POPALIASEDFIELDNEAR:
      hb_vmPopAliasedField(pSymbols + pCode[1]);
      pCode += 2;
      break;

    case HB_P_POPALIASEDVAR:
      hb_vmPopAliasedVar(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_POPFIELD:
      /* Pops a value from the eval stack and uses it to set
       * a new value of the given field
       */
      hb_rddPutFieldValue(hb_stackItemFromTop(-1), pSymbols + HB_PCODE_MKUSHORT(&pCode[1]));
      hb_stackPop();
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopField)"));
#endif
      pCode += 3;
      break;

    case HB_P_POPLOCAL:
      hb_vmPopLocal(HB_PCODE_MKSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_POPLOCALNEAR:
      hb_vmPopLocal(static_cast<signed char>(pCode[1]));
      pCode += 2; /* only first two bytes are used */
      break;

    case HB_P_POPSTATIC:
      hb_vmPopStatic(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_POPMEMVAR:
      hb_memvarSetValue(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]), hb_stackItemFromTop(-1));
      hb_stackPop();
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopMemvar)"));
#endif
      pCode += 3;
      break;

    case HB_P_POPVARIABLE: {
      /*
         2004-03-19 Ron Pinkas
         Test with Clipper shows that for assignment, MEMVAR context
         is always used even if MEMVAR does NOT exists, and a FIELD
         with this name exists!!!
         Here is the Test Used - Clipper produced NO runtime error -
         indicating MEMVAR was created.
           PROCEDURE Main()
              USE test.dbf
              First := First
              dbCloseArea()
              ? First
              RETURN
       */
#if 0
            /* Pops a value from the eval stack and uses it to set
             * a new value of a variable of unknown type.
             */
            PHB_SYMB pSymbol = pSymbols + HB_PCODE_MKUSHORT(&pCode[1]);

            if( pSymbol->pDynSym && hb_dynsymGetMemvar(pSymbol->pDynSym) ) {
               /* If exist a memory symbol with this name use it */
               hb_memvarSetValue(pSymbol, hb_stackItemFromTop(-1));
            } else if( hb_rddFieldPut(hb_stackItemFromTop(-1), pSymbol) == Harbour::FAILURE ) {
               /* Try with a field and after create a memvar */
               hb_memvarSetValue(pSymbol, hb_stackItemFromTop(-1));
            }
#else
      hb_memvarSetValue(pSymbols + HB_PCODE_MKUSHORT(&pCode[1]), hb_stackItemFromTop(-1));
#endif
      hb_stackPop();
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopVariable)"));
#endif
      pCode += 3;
      break;
    }

      /* macro creation */

    case HB_P_MACROPOP:
      /* compile and run - pop a value from the stack */
      hb_macroSetValue(hb_stackItemFromTop(-1), pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPOPALIASED:
      /* compile and run - pop an aliased variable from the stack */
      hb_macroPopAliasedValue(hb_stackItemFromTop(-2), hb_stackItemFromTop(-1), pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPUSH:
      /* compile and run - leave the result on the stack */
      /* the topmost element on the stack contains a macro
       * string for compilation
       */
      hb_macroGetValue(hb_stackItemFromTop(-1), 0, pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPUSHLIST:
      /* compile and run - leave the result on the stack */
      /* the topmost element on the stack contains a macro
       * string for compilation
       */
      hb_macroGetValue(hb_stackItemFromTop(-1), HB_P_MACROPUSHLIST, pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPUSHINDEX:
      hb_vmMacroPushIndex();
      pCode++;
      break;

    case HB_P_MACROARRAYGEN:
      hb_vmMacroArrayGen(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_MACRODO:
      hb_vmMacroDo(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_MACROFUNC:
      hb_vmMacroFunc(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_MACROSEND:
      hb_vmMacroSend(HB_PCODE_MKUSHORT(&pCode[1]));
      pCode += 3;
      break;

    case HB_P_MACROPUSHPARE:
      /* compile and run - leave the result on the stack */
      /* the topmost element on the stack contains a macro
       * string for compilation
       */
      hb_macroGetValue(hb_stackItemFromTop(-1), HB_P_MACROPUSHPARE, pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPUSHALIASED:
      /* compile and run - leave an aliased variable on the stack */
      hb_macroPushAliasedValue(hb_stackItemFromTop(-2), hb_stackItemFromTop(-1), pCode[1]);
      pCode += 2;
      break;

    case HB_P_MACROPUSHREF: {
      auto pMacro = hb_stackItemFromTop(-1);
      hb_macroPushReference(pMacro);
      pCode++;
      break;
    }

    case HB_P_MACROSYMBOL:
      /* compile into a symbol name (used in function calls) */
      hb_macroPushSymbol(hb_stackItemFromTop(-1));
      pCode++;
      break;

    case HB_P_MACROTEXT:
      /* macro text substitution
       * "text &macro.other text"
       */
      hb_macroTextValue(hb_stackItemFromTop(-1));
      pCode++;
      break;

      /* macro compiled opcodes - we are using symbol address here */

    case HB_P_MMESSAGE: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPushSymbol(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPOPALIASEDFIELD: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPopAliasedField(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPOPALIASEDVAR: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPopAliasedVar(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPOPFIELD: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      /* Pops a value from the eval stack and uses it to set
       * a new value of the given field
       */
      hb_rddPutFieldValue((hb_stackItemFromTop(-1)), pDynSym->pSymbol);
      hb_stackPop();
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPopField)"));
#endif
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPOPMEMVAR: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_memvarSetValue(pDynSym->pSymbol, hb_stackItemFromTop(-1));
      hb_stackPop();
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPopMemvar)"));
#endif
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHALIASEDFIELD: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPushAliasedField(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHALIASEDVAR: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPushAliasedVar(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHBLOCK: {
      /*NOTE: the pcode is stored in dynamically allocated memory
       * We need to handle it with more care than compile-time
       * codeblocks
       */
      /* +0    -> _pushblock
       * +1 +2 -> size of codeblock
       * +3 +4 -> number of expected parameters
       * +5    -> pcode bytes
       */
      HB_SIZE nSize = HB_PCODE_MKUSHORT(&pCode[1]);
      hb_vmPushMacroBlock(pCode + 5, nSize - 5, HB_PCODE_MKUSHORT(&pCode[3]));
      pCode += nSize;
      break;
    }

    case HB_P_MPUSHBLOCKLARGE: {
      /*NOTE: the pcode is stored in dynamically allocated memory
       * We need to handle it with more care than compile-time
       * codeblocks
       */
      /* +0       -> _pushblock
       * +1 +2 +3 -> size of codeblock
       * +4 +5    -> number of expected parameters
       * +6       -> pcode bytes
       */
      HB_SIZE nSize = HB_PCODE_MKUINT24(&pCode[1]);
      hb_vmPushMacroBlock(pCode + 6, nSize - 6, HB_PCODE_MKUSHORT(&pCode[4]));
      pCode += nSize;
      break;
    }

    case HB_P_MPUSHFIELD: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      /* It pushes the current value of the given field onto the eval stack
       */
      hb_rddGetFieldValue(hb_stackAllocItem(), pDynSym->pSymbol);
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushField)"));
#endif
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHMEMVAR: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_memvarGetValue(hb_stackAllocItem(), pDynSym->pSymbol);
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvar)"));
#endif
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHMEMVARREF: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_memvarGetRefer(hb_stackAllocItem(), pDynSym->pSymbol);
#if 0
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvarRef)"));
#endif
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHSYM: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPushSymbol(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHVARIABLE: {
      auto pDynSym = static_cast<PHB_DYNS>(HB_GET_PTR(pCode + 1));
      hb_vmPushVariable(pDynSym->pSymbol);
      pCode += sizeof(PHB_DYNS) + 1;
      break;
    }

    case HB_P_MPUSHSTR: {
      HB_USHORT uiSize = HB_PCODE_MKUSHORT(&pCode[1]);
      hb_vmPushString(reinterpret_cast<const char *>(pCode + 3), uiSize - 1);
      pCode += 3 + uiSize;
      break;
    }

    case HB_P_MPUSHSTRLARGE: {
      HB_SIZE nSize = HB_PCODE_MKUINT24(&pCode[1]);
      hb_vmPushString(reinterpret_cast<const char *>(pCode + 3), nSize - 1);
      pCode += 4 + nSize;
      break;
    }

    case HB_P_LOCALNEARADDINT: {
      int iLocal = pCode[1];
#if 0
            HB_TRACE(HB_TR_DEBUG, ("HB_P_LOCALNEARADDINT"));
#endif

      hb_vmAddInt(hb_stackLocalVariable(iLocal), HB_PCODE_MKSHORT(&pCode[2]));
      pCode += 4;
      break;
    }

    case HB_P_LOCALADDINT: {
      int iLocal = HB_PCODE_MKUSHORT(&pCode[1]);
#if 0
            HB_TRACE(HB_TR_DEBUG, ("HB_P_LOCALADDINT"));
#endif

      hb_vmAddInt(hb_stackLocalVariable(iLocal), HB_PCODE_MKSHORT(&pCode[3]));
      pCode += 5;
      break;
    }

    case HB_P_LOCALINC: {
      int iLocal = HB_PCODE_MKUSHORT(&pCode[1]);
      PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
      hb_vmInc(HB_IS_BYREF(pLocal) ? hb_itemUnRef(pLocal) : pLocal);
      pCode += 3;
      break;
    }

    case HB_P_LOCALDEC: {
      int iLocal = HB_PCODE_MKUSHORT(&pCode[1]);
      PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
      hb_vmDec(HB_IS_BYREF(pLocal) ? hb_itemUnRef(pLocal) : pLocal);
      pCode += 3;
      break;
    }

    case HB_P_LOCALINCPUSH: {
      int iLocal = HB_PCODE_MKUSHORT(&pCode[1]);
      PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
      if (HB_IS_BYREF(pLocal))
      {
        pLocal = hb_itemUnRef(pLocal);
      }
      hb_vmInc(pLocal);
      hb_itemCopy(hb_stackAllocItem(), pLocal);
      pCode += 3;
      break;
    }

      /* WITH OBJECT */

    case HB_P_WITHOBJECTMESSAGE: {
      HB_USHORT wSymPos = HB_PCODE_MKUSHORT(&pCode[1]);
      if (wSymPos != 0xFFFF)
      {
        /* NOTE: 0xFFFF is passed when ':&varmacro' syntax is used.
         * In this case symbol is already pushed on the stack
         * using HB_P_MACROSYMBOL.
         */
        hb_vmPushSymbol(pSymbols + wSymPos);
      }
      PHB_ITEM pWith = hb_stackWithObjectItem();
      if (pWith)
      {
        hb_vmPush(pWith);
      }
      else
      {
        hb_stackAllocItem()->type = Harbour::Item::NIL;
      }
      pCode += 3;
      break;
    }

    case HB_P_WITHOBJECTSTART:
      hb_vmWithObjectStart();
      pCode++;
      break;

    case HB_P_WITHOBJECTEND:
      hb_stackPop(); /* remove with object envelope */
      hb_stackPop(); /* remove implicit object */
      pCode++;
      break;

      /* misc */

    case HB_P_NOOP:
      /* Intentionally do nothing */
      pCode++;
      break;

    default:
      /* TODO: Include to failing pcode in the error message */
      hb_errInternal(HB_EI_VMBADOPCODE, nullptr, nullptr, nullptr);
      break;
    }

    if (hb_stackGetActionRequest())
    {
      if (hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED)
      {
        /* request to stop current procedure was issued
         * (from macro evaluation)
         */

        /* This code allow to use RETURN inside BEGIN/END sequence
         * or in RECOVER code when ALWAYS clause is used
         */
        if (bCanRecover)
        {
          do
          {
            hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
            {
              hb_errInternal(HB_EI_ERRUNRECOV, "ENDPROC", nullptr, nullptr);
            }
#endif
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_DOALWAYS)
            {
              break;
            }
            /* Restore previous recovery state */
            bCanRecover = (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_CANRECOVER) != 0;
            hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
          } while (bCanRecover);

          /* ALWAYS found? */
          if (bCanRecover)
          {
#if defined(_HB_RECOVER_DEBUG)
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
            {
              hb_errInternal(HB_EI_ERRUNRECOV, "ENDPROC ALWAYS", nullptr, nullptr);
            }
#endif
            /* reload the address of ALWAYS code */
            pCode = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.recover;
            /* store and reset action */
            hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request = hb_stackGetActionRequest();
            hb_stackSetActionRequest(0);
            continue;
          }
        }
        hb_stackSetActionRequest(0);
        break;
      }
      else if (hb_stackGetActionRequest() & HB_BREAK_REQUESTED)
      {
        if (bCanRecover)
        {
          /*
           * There is the BEGIN/END sequence defined in current
           * procedure/function - use it to continue opcodes execution
           */
          /*
           * remove all items placed on the stack after BEGIN code
           */
          hb_stackRemove(hb_stackGetRecoverBase());
          /*
           * reload the address of recovery code
           */
#if defined(_HB_RECOVER_DEBUG)
          if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
          {
            hb_errInternal(HB_EI_ERRUNRECOV, "BREAK", nullptr, nullptr);
          }
#endif
          pCode = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.recover;
          /*
           * leave the SEQUENCE envelope on the stack - it will
           * be popped either in RECOVER or END opcode
           */

          /* store and reset action */
          hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request = hb_stackGetActionRequest();
          hb_stackSetActionRequest(0);
        }
        else
        {
          break;
        }
      }
      else if (hb_stackGetActionRequest() & HB_QUIT_REQUESTED)
      {
        if (bCanRecover)
        {
          do
          {
            hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
            {
              hb_errInternal(HB_EI_ERRUNRECOV, "QUIT", nullptr, nullptr);
            }
#endif
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_DOALWAYS)
            {
              break;
            }
            /* Restore previous recovery state */
            bCanRecover = (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_CANRECOVER) != 0;
            hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
            /* skip other steps */
          } while (bCanRecover);

          /* ALWAYS found? */
          if (bCanRecover)
          {
#if defined(_HB_RECOVER_DEBUG)
            if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
            {
              hb_errInternal(HB_EI_ERRUNRECOV, "QUIT ALWAYS", nullptr, nullptr);
            }
#endif
            /* reload the address of ALWAYS code */
            pCode = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.recover;
            /* store and reset action */
            hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request = hb_stackGetActionRequest();
            hb_stackSetActionRequest(0);
            continue;
          }
        }
        break;
      }
    }
  }
}

/* Operators (mathematical / character / misc) */

static void hb_vmAddInt(PHB_ITEM pResult, HB_LONG lAdd)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmAddInt(%p,%ld)", static_cast<void*>(pResult), lAdd));
#endif

  if (HB_IS_BYREF(pResult))
  {
    pResult = hb_itemUnRef(pResult);
  }

  if (HB_IS_NUMINT(pResult))
  {
    HB_MAXINT nVal = HB_ITEM_GET_NUMINTRAW(pResult), nResult;

    nResult = nVal + lAdd;

    if (lAdd >= 0 ? nResult >= nVal : nResult < nVal)
    {
      HB_ITEM_PUT_NUMINTRAW(pResult, nResult);
    }
    else
    {
      pResult->type = Harbour::Item::DOUBLE;
      pResult->item.asDouble.value = static_cast<double>(nVal) + lAdd;
      pResult->item.asDouble.length = HB_DBL_LENGTH(pResult->item.asDouble.value);
      pResult->item.asDouble.decimal = 0;
    }
  }
  else if (pResult->isDouble())
  {
    pResult->item.asDouble.value += lAdd;
    pResult->item.asDouble.length = HB_DBL_LENGTH(pResult->item.asDouble.value);
  }
  else if (HB_IS_DATETIME(pResult))
  {
    pResult->type &= ~Harbour::Item::DEFAULT;
    pResult->item.asDateTime.julian += lAdd;
  }
  else if (hb_objHasOperator(pResult, HB_OO_OP_PLUS))
  {
    HB_STACK_TLS_PRELOAD
    hb_vmPushLong(lAdd);
    hb_objOperatorCall(HB_OO_OP_PLUS, pResult, pResult, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    HB_STACK_TLS_PRELOAD
    PHB_ITEM pSubst;

    hb_vmPushLong(lAdd);
    pSubst = hb_errRT_BASE_Subst(EG_ARG, 1081, nullptr, "+", 2, pResult, hb_stackItemFromTop(-1));
    if (pSubst)
    {
      hb_stackPop();
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmNegate()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);

  if (pItem->isInteger())
  {
#if - HB_VMINT_MAX > HB_VMINT_MIN
    if (pItem->item.asInteger.value < -HB_VMINT_MAX)
    {
#if HB_VMLONG_MAX > HB_VMINT_MAX
      auto nValue = static_cast<HB_MAXINT>(pItem->item.asInteger.value);
      pItem->type = Harbour::Item::LONG;
      pItem->item.asLong.value = -nValue;
      pItem->item.asLong.length = HB_LONG_EXPLENGTH(-nValue);
#else
      auto dValue = static_cast<double>(pItem->item.asInteger.value);
      pItem->type = Harbour::Item::DOUBLE;
      pItem->item.asDouble.value = -dValue;
      pItem->item.asDouble.length = HB_DBL_LENGTH(-dValue);
      pItem->item.asDouble.decimal = 0;
#endif
    }
    else
#endif
    {
      pItem->type = Harbour::Item::INTEGER;
      pItem->item.asInteger.value = -pItem->item.asInteger.value;
      pItem->item.asInteger.length = HB_INT_EXPLENGTH(pItem->item.asInteger.value);
    }
  }
  else if (HB_IS_LONG(pItem))
  {
#if - HB_VMLONG_MAX > HB_VMLONG_MIN
    if (pItem->item.asLong.value < -HB_VMLONG_MAX)
    {
      auto dValue = static_cast<double>(pItem->item.asLong.value);
      pItem->type = Harbour::Item::DOUBLE;
      pItem->item.asDouble.value = -dValue;
      pItem->item.asDouble.length = HB_DBL_LENGTH(-dValue);
      pItem->item.asDouble.decimal = 0;
    }
    else
#endif
    {
      pItem->type = Harbour::Item::LONG;
      pItem->item.asLong.value = -pItem->item.asLong.value;
      pItem->item.asLong.length = HB_LONG_EXPLENGTH(pItem->item.asLong.value);
    }
  }
  else if (pItem->isDouble())
  {
    pItem->type = Harbour::Item::DOUBLE;
    pItem->item.asDouble.value = -pItem->item.asDouble.value;
    pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1080, nullptr, "-", 1, pItem);

    if (pResult)
    {
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmTimeStampPut(PHB_ITEM pItem, long lJulian, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmTimeStampPut(%p,%ld,%ld)", static_cast<void*>(pItem), lJulian, lMilliSec));
#endif

  /* timestamp normalization */
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

  hb_itemPutTDT(pItem, lJulian, lMilliSec);
}

static void hb_vmTimeStampAdd(PHB_ITEM pResult, PHB_ITEM pItem, double dValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmTimeStampAdd(%p,%p,%lf)", static_cast<void*>(pResult), static_cast<void*>(pItem), dValue));
#endif

  long lJulian, lMilliSec;
  hb_timeStampUnpackDT(dValue, &lJulian, &lMilliSec);

  lJulian += pItem->item.asDateTime.julian;
  lMilliSec += pItem->item.asDateTime.time;

  hb_vmTimeStampPut(pResult, lJulian, lMilliSec);
}

static void hb_vmPlus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPlus(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW(pItem1);
    HB_MAXINT nNumber2 = HB_ITEM_GET_NUMINTRAW(pItem2);
    HB_MAXINT nResult = nNumber1 + nNumber2;

    if (HB_IS_COMPLEX(pResult))
    {
      hb_itemClear(pResult);
    }

    if (nNumber2 >= 0 ? nResult >= nNumber1 : nResult < nNumber1)
    {
      HB_ITEM_PUT_NUMINTRAW(pResult, nResult);
    }
    else
    {
      double dResult = static_cast<double>(nNumber1) + static_cast<double>(nNumber2);
      pResult->type = Harbour::Item::DOUBLE;
      pResult->item.asDouble.value = dResult;
      pResult->item.asDouble.length = HB_DBL_LENGTH(dResult);
      pResult->item.asDouble.decimal = 0;
    }
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    int iDec1, iDec2;
    double dNumber1 = hb_itemGetNDDec(pItem1, &iDec1);
    double dNumber2 = hb_itemGetNDDec(pItem2, &iDec2);

    hb_itemPutNDDec(pResult, dNumber1 + dNumber2, HB_MAX(iDec1, iDec2));
  }
  else if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    HB_SIZE nLen1 = pItem1->item.asString.length;
    HB_SIZE nLen2 = pItem2->item.asString.length;

    if (nLen2)
    {
      if (nLen1)
      {
        if (nLen1 < HB_SIZE_MAX - nLen2)
        {
          if (pResult != pItem1)
          {
            hb_itemMove(pResult, pItem1);
            pItem1 = pResult;
          }
          hb_itemReSizeString(pItem1, nLen1 + nLen2);
          hb_xmemcpy(pItem1->item.asString.value + nLen1, pItem2->item.asString.value, nLen2);
        }
        else
        {
          hb_errRT_BASE(EG_STROVERFLOW, 1209, nullptr, "+", 2, pItem1, pItem2);
        }
      }
      else
      {
        hb_itemCopy(pResult, pItem2);
      }
    }
    else if (pResult != pItem1)
    {
      hb_itemCopy(pResult, pItem1);
    }
    pResult->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() || pItem2->isTimeStamp())
    {
      hb_vmTimeStampPut(pResult, pItem1->item.asDateTime.julian + pItem2->item.asDateTime.julian,
                        pItem1->item.asDateTime.time + pItem2->item.asDateTime.time);
    }
    else
    {
      /* NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs. */
      hb_itemPutDL(pResult, pItem1->item.asDateTime.julian + pItem2->item.asDateTime.julian);
    }
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    if (pItem1->isTimeStamp())
    {
      if (HB_IS_NUMINT(pItem2))
      {
        hb_vmTimeStampPut(pResult, pItem1->item.asDateTime.julian + static_cast<long>(HB_ITEM_GET_NUMINTRAW(pItem2)),
                          pItem1->item.asDateTime.time);
      }
      else
      {
        hb_vmTimeStampAdd(pResult, pItem1, pItem2->item.asDouble.value);
      }
    }
    else
    {
      hb_itemPutDL(pResult, hb_itemGetDL(pItem1) + hb_itemGetNL(pItem2));
    }
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem2->isTimeStamp())
    {
      if (HB_IS_NUMINT(pItem1))
      {
        hb_vmTimeStampPut(pResult, static_cast<long>(HB_ITEM_GET_NUMINTRAW(pItem1)) + pItem2->item.asDateTime.julian,
                          pItem2->item.asDateTime.time);
      }
      else
      {
        hb_vmTimeStampAdd(pResult, pItem2, pItem1->item.asDouble.value);
      }
    }
    else
    {
      hb_itemPutDL(pResult, hb_itemGetNL(pItem1) + hb_itemGetDL(pItem2));
    }
  }
  else if (!hb_objOperatorCall(HB_OO_OP_PLUS, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1081, nullptr, "+", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmMinus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMinus(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW(pItem1);
    HB_MAXINT nNumber2 = HB_ITEM_GET_NUMINTRAW(pItem2);
    HB_MAXINT nResult = nNumber1 - nNumber2;

    if (HB_IS_COMPLEX(pResult))
    {
      hb_itemClear(pResult);
    }

    if (nNumber2 <= 0 ? nResult >= nNumber1 : nResult < nNumber1)
    {
      HB_ITEM_PUT_NUMINTRAW(pResult, nResult);
    }
    else
    {
      double dResult = static_cast<double>(nNumber1) - static_cast<double>(nNumber2);
      pResult->type = Harbour::Item::DOUBLE;
      pResult->item.asDouble.value = dResult;
      pResult->item.asDouble.length = HB_DBL_LENGTH(dResult);
      pResult->item.asDouble.decimal = 0;
    }
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    int iDec1, iDec2;
    double dNumber1 = hb_itemGetNDDec(pItem1, &iDec1);
    double dNumber2 = hb_itemGetNDDec(pItem2, &iDec2);

    hb_itemPutNDDec(pResult, dNumber1 - dNumber2, HB_MAX(iDec1, iDec2));
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    long lTime = pItem1->item.asDateTime.time - pItem2->item.asDateTime.time,
         lJulian = pItem1->item.asDateTime.julian - pItem2->item.asDateTime.julian;
    if (lTime != 0)
    {
      hb_itemPutNDDec(pResult, hb_timeStampPackDT(lJulian, lTime), HB_TIMEDIFF_DEC);
    }
    else
    {
      if (HB_IS_COMPLEX(pResult))
      {
        hb_itemClear(pResult);
      }
      HB_ITEM_PUT_LONGRAW(pResult, lJulian);
    }
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    if (pItem1->isTimeStamp())
    {
      if (HB_IS_NUMINT(pItem2))
      {
        hb_vmTimeStampPut(pResult, pItem1->item.asDateTime.julian - static_cast<long>(HB_ITEM_GET_NUMINTRAW(pItem2)),
                          pItem1->item.asDateTime.time);
      }
      else
      {
        hb_vmTimeStampAdd(pResult, pItem1, -pItem2->item.asDouble.value);
      }
    }
    else
    {
      hb_itemPutDL(pResult, hb_itemGetDL(pItem1) - hb_itemGetNL(pItem2));
    }
  }
  else if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    HB_SIZE nLen1 = pItem1->item.asString.length;
    HB_SIZE nLen2 = pItem2->item.asString.length;

    if (nLen1 == 0)
    {
      hb_itemCopy(pResult, pItem2);
      pResult->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
    }
    else if (nLen2 == 0)
    {
      if (pResult != pItem1)
      {
        hb_itemCopy(pResult, pItem1);
      }
      pResult->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
    }
    else if (nLen1 < HB_SIZE_MAX - nLen2)
    {
      if (pResult != pItem1)
      {
        hb_itemMove(pResult, pItem1);
        pItem1 = pResult;
      }
      hb_itemReSizeString(pItem1, nLen1 + nLen2);
      while (nLen1 && pItem1->item.asString.value[nLen1 - 1] == ' ')
      {
        nLen1--;
      }
      hb_xmemcpy(pItem1->item.asString.value + nLen1, pItem2->item.asString.value, nLen2);
      hb_xmemset(pItem1->item.asString.value + nLen1 + nLen2, ' ', pItem1->item.asString.length - nLen1 - nLen2);
    }
    else
    {
      hb_errRT_BASE(EG_STROVERFLOW, 1210, nullptr, "-", 2, pItem1, pItem2);
    }
  }
  else if (!hb_objOperatorCall(HB_OO_OP_MINUS, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1082, nullptr, "-", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmMult(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMult(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

#if - (HB_VMLONG_MAX / HB_VMINT_MIN) >= HB_VMINT_MAX && 1
  if (pItem1->isInteger() && pItem2->isInteger())
  {
    HB_MAXINT nResult =
        static_cast<HB_MAXINT>(pItem1->item.asInteger.value) * static_cast<HB_MAXINT>(pItem2->item.asInteger.value);
    if (HB_IS_COMPLEX(pResult))
    {
      hb_itemClear(pResult);
    }
    HB_ITEM_PUT_NUMINTRAW(pResult, nResult);
  }
  else
#endif
      if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    int iDec1, iDec2;
    double dNumber1 = hb_itemGetNDDec(pItem1, &iDec1);
    double dNumber2 = hb_itemGetNDDec(pItem2, &iDec2);

    hb_itemPutNumType(pResult, dNumber1 * dNumber2, iDec1 + iDec2, HB_ITEM_TYPERAW(pItem1), HB_ITEM_TYPERAW(pItem2));
  }
  else if (!hb_objOperatorCall(HB_OO_OP_MULT, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1083, nullptr, "*", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmDivide(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDivide(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    HB_MAXINT nDivisor = HB_ITEM_GET_NUMINTRAW(pItem2);

    if (nDivisor == 0)
    {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1340, nullptr, "/", 2, pItem1, pItem2);

      if (pSubst)
      {
        hb_itemMove(pResult, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else
    {
      HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW(pItem1);
      hb_itemPutND(pResult, static_cast<double>(nNumber1) / static_cast<double>(nDivisor));
    }
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    auto dDivisor = hb_itemGetND(pItem2);

    if (dDivisor == 0.0)
    {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1340, nullptr, "/", 2, pItem1, pItem2);

      if (pSubst)
      {
        hb_itemMove(pResult, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else
    {
      /* If all both operand was integer and the result is an integer, too,
         push the number without decimals. Clipper compatible. Actually,
         this is not Clipper compatible. The only time Clipper returns 0
         decimal places is for compiler optimized integer division with an
         integer result. Therefore this code is not needed and has been
         removed - David G. Holm <dholm@jsd-llc.com>
       */
      hb_itemPutND(pResult, hb_itemGetND(pItem1) / dDivisor);
    }
  }
  else if (!hb_objOperatorCall(HB_OO_OP_DIVIDE, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1084, nullptr, "/", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmModulus(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModulus(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    HB_MAXINT nDivisor = HB_ITEM_GET_NUMINTRAW(pItem2);

    if (nDivisor == 0)
    {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1341, nullptr, "%", 2, pItem1, pItem2);

      if (pSubst)
      {
        hb_itemMove(pResult, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else
    {
      /* NOTE: Clipper always returns the result of modulus with the SET number of decimal places. */
      hb_itemPutND(pResult, static_cast<double>(HB_ITEM_GET_NUMINTRAW(pItem1) % nDivisor));
    }
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    auto dDivisor = hb_itemGetND(pItem2);

    if (dDivisor == 0.0)
    {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1341, nullptr, "%", 2, pItem1, pItem2);

      if (pSubst)
      {
        hb_itemMove(pResult, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else
    {
      /* NOTE: Clipper always returns the result of modulus with the SET number of decimal places. */
      hb_itemPutND(pResult, fmod(hb_itemGetND(pItem1), dDivisor));
    }
  }
  else if (!hb_objOperatorCall(HB_OO_OP_MOD, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1085, nullptr, "%", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmPower(PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPower(%p,%p,%p)", static_cast<void*>(pResult), static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    /* NOTE: Clipper always returns the result of power with the SET number of decimal places. */
    hb_itemPutND(pResult, pow(hb_itemGetND(pItem1), hb_itemGetND(pItem2)));
  }
  else if (!hb_objOperatorCall(HB_OO_OP_POWER, pResult, pItem1, pItem2, nullptr))
  {
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1088, nullptr, "^", 2, pItem1, pItem2);

    if (pSubst)
    {
      hb_itemMove(pResult, pSubst);
      hb_itemRelease(pSubst);
    }
  }
}

static void hb_vmInc(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInc(%p)", static_cast<void*>(pItem)));
#endif

  if (HB_IS_NUMINT(pItem))
  {
    if (pItem->isInteger())
    {
      if (pItem->item.asInteger.value < HB_VMINT_MAX)
      {
        pItem->type = Harbour::Item::INTEGER;
        pItem->item.asInteger.value++;
        pItem->item.asInteger.length = HB_INT_EXPLENGTH(pItem->item.asInteger.value);
      }
      else
      {
#if HB_VMINT_MAX < HB_VMLONG_MAX
        pItem->type = Harbour::Item::LONG;
        pItem->item.asLong.value = static_cast<HB_MAXINT>(pItem->item.asInteger.value) + 1;
        pItem->item.asLong.length = HB_LONG_EXPLENGTH(pItem->item.asLong.value);
#else
        pItem->type = Harbour::Item::DOUBLE;
        pItem->item.asDouble.value = static_cast<double>(pItem->item.asInteger.value) + 1;
        pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
        pItem->item.asDouble.decimal = 0;
#endif
      }
    }
    else if (pItem->item.asLong.value < HB_VMLONG_MAX)
    {
      pItem->type = Harbour::Item::LONG;
      pItem->item.asLong.value++;
      pItem->item.asLong.length = HB_LONG_EXPLENGTH(pItem->item.asLong.value);
    }
    else
    {
      pItem->type = Harbour::Item::DOUBLE;
      pItem->item.asDouble.value = static_cast<double>(pItem->item.asLong.value) + 1;
      pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
      pItem->item.asDouble.decimal = 0;
    }
  }
  else if (pItem->isDouble())
  {
    pItem->type = Harbour::Item::DOUBLE;
    pItem->item.asDouble.value++;
    pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
  }
  else if (HB_IS_DATETIME(pItem))
  {
    pItem->type &= ~Harbour::Item::DEFAULT;
    pItem->item.asDateTime.julian++;
  }
  else if (!hb_objOperatorCall(HB_OO_OP_INC, pItem, pItem, nullptr, nullptr))
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1086, nullptr, "++", 1, pItem);

    if (pResult)
    {
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmDec(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDec(%p)", static_cast<void*>(pItem)));
#endif

  if (HB_IS_NUMINT(pItem))
  {
    if (pItem->isInteger())
    {
      if (pItem->item.asInteger.value > HB_VMINT_MIN)
      {
        pItem->type = Harbour::Item::INTEGER;
        pItem->item.asInteger.value--;
        pItem->item.asInteger.length = HB_INT_EXPLENGTH(pItem->item.asInteger.value);
      }
      else
      {
#if HB_VMINT_MIN > HB_VMLONG_MIN
        pItem->type = Harbour::Item::LONG;
        pItem->item.asLong.value = static_cast<HB_MAXINT>(pItem->item.asInteger.value) - 1;
        pItem->item.asLong.length = HB_LONG_EXPLENGTH(pItem->item.asLong.value);
#else
        pItem->type = Harbour::Item::DOUBLE;
        pItem->item.asDouble.value = static_cast<double>(pItem->item.asInteger.value) - 1;
        pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
        pItem->item.asDouble.decimal = 0;
#endif
      }
    }
    else if (pItem->item.asLong.value > HB_VMLONG_MIN)
    {
      pItem->type = Harbour::Item::LONG;
      pItem->item.asLong.value--;
      pItem->item.asLong.length = HB_LONG_EXPLENGTH(pItem->item.asLong.value);
    }
    else
    {
      pItem->type = Harbour::Item::DOUBLE;
      pItem->item.asDouble.value = static_cast<double>(pItem->item.asLong.value) - 1;
      pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
      pItem->item.asDouble.decimal = 0;
    }
  }
  else if (pItem->isDouble())
  {
    pItem->type = Harbour::Item::DOUBLE;
    pItem->item.asDouble.value--;
    pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
  }
  else if (HB_IS_DATETIME(pItem))
  {
    pItem->type &= ~Harbour::Item::DEFAULT;
    pItem->item.asDateTime.julian--;
  }
  else if (!hb_objOperatorCall(HB_OO_OP_DEC, pItem, pItem, nullptr, nullptr))
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1087, nullptr, "--", 1, pItem);

    if (pResult)
    {
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmFuncPtr() /* pushes a function address pointer. Removes the symbol from the stack */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFuncPtr()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);

  if (HB_IS_SYMBOL(pItem))
  {
    /* do nothing - now we are using Harbour::Item::SYMBOL */
#if 0
      hb_stackPop();
      hb_vmPushPointer(static_cast<void*>(pItem->item.asSymbol.value->value.pFunPtr));
#endif
  }
  else
  {
    hb_errInternal(HB_EI_VMNOTSYMBOL, nullptr, "hb_vmFuncPtr()", nullptr);
  }
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmExactlyEqual()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmExactlyEqual()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (pItem1->isNil())
  {
    /* pItem1 is NIL so this is safe */
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = pItem2->isNil();
    hb_stackPop(); /* clear the pItem2 */
  }
  else if (pItem2->isNil())
  {
    hb_stackDec(); /* pItem2 is already NIL */
    if (HB_IS_COMPLEX(pItem1))
    {
      hb_itemClear(pItem1);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = false;
  }
  else if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    bool fResult =
        pItem1->item.asString.length == pItem2->item.asString.length &&
        (pItem1->item.asString.value == pItem2->item.asString.value ||
         memcmp(pItem1->item.asString.value, pItem2->item.asString.value, pItem1->item.asString.length) == 0);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) == HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) == HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                                    pItem1->item.asDateTime.time == pItem2->item.asDateTime.time);
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value =
        pItem1->item.asLogical.value ? pItem2->item.asLogical.value : !pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (HB_IS_POINTER(pItem1) && HB_IS_POINTER(pItem2))
  {
    bool fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (HB_IS_HASH(pItem1) && HB_IS_HASH(pItem2))
  {
    bool fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
#ifndef HB_CLP_STRICT
  else if (pItem1->isBlock() && pItem2->isBlock())
  {
    bool fResult = pItem1->item.asBlock.value == pItem2->item.asBlock.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
#endif
  else if (HB_IS_SYMBOL(pItem1) && HB_IS_SYMBOL(pItem2))
  {
    pItem1->item.asLogical.value = pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
                                   (pItem1->item.asSymbol.value->pDynSym != nullptr &&
                                    pItem1->item.asSymbol.value->pDynSym == pItem2->item.asSymbol.value->pDynSym);
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isArray() && pItem2->isArray() && !hb_objHasOperator(pItem1, HB_OO_OP_EXACTEQUAL))
  {
    bool fResult = pItem1->item.asArray.value == pItem2->item.asArray.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (hb_objOperatorCall(HB_OO_OP_EXACTEQUAL, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1070, nullptr, "==", 2, pItem1, pItem2);
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmEqual()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEqual()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (pItem1->isNil())
  {
    /* pItem1 is NIL so this is safe */
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = pItem2->isNil();
    hb_stackPop(); /* clear the pItem2 */
  }
  else if (pItem2->isNil())
  {
    hb_stackDec(); /* pItem2 is already NIL */
    if (HB_IS_COMPLEX(pItem1))
    {
      hb_itemClear(pItem1);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = false;
  }
  else if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    bool fResult = hb_itemStrCmp(pItem1, pItem2, false) == 0;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) == HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) == HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian) &&
                                     (pItem1->item.asDateTime.time == pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value =
        pItem1->item.asLogical.value ? pItem2->item.asLogical.value : !pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (HB_IS_POINTER(pItem1) && HB_IS_POINTER(pItem2))
  {
    bool fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
#if 0
   else if( HB_IS_HASH(pItem1) && HB_IS_HASH(pItem2) ) {
      bool fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;
      hb_stackPop();
      hb_itemClear(pItem1);
      pItem1->type = Harbour::Item::LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#endif
  else if (hb_objOperatorCall(HB_OO_OP_EQUAL, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1071, nullptr, "=", 2, pItem1, pItem2);
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmNotEqual()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmNotEqual()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (pItem1->isNil())
  {
    /* pItem1 is NIL so this is safe */
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = !pItem2->isNil();
    hb_stackPop(); /* clear the pItem2 */
  }
  else if (pItem2->isNil())
  {
    hb_stackDec(); /* pItem2 is already NIL */
    if (HB_IS_COMPLEX(pItem1))
    {
      hb_itemClear(pItem1);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = true;
  }
  else if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    int i = hb_itemStrCmp(pItem1, pItem2, false);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = i != 0;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) != HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) != HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian != pItem2->item.asDateTime.julian) ||
                                     (pItem1->item.asDateTime.time != pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian != pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value =
        pItem1->item.asLogical.value ? !pItem2->item.asLogical.value : pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (HB_IS_POINTER(pItem1) && HB_IS_POINTER(pItem2))
  {
    bool fResult = pItem1->item.asPointer.value != pItem2->item.asPointer.value;
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
#if 0
   else if( HB_IS_HASH(pItem1) && HB_IS_HASH(pItem2) ) {
      bool fResult = pItem1->item.asHash.value != pItem2->item.asHash.value;
      hb_stackPop();
      hb_itemClear(pItem1);
      pItem1->type = Harbour::Item::LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
#endif
  else if (hb_objOperatorCall(HB_OO_OP_NOTEQUAL, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1072, nullptr, "<>", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmLess()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLess()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    int i = hb_itemStrCmp(pItem1, pItem2, false);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = i < 0;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) < HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) < HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian < pItem2->item.asDateTime.julian) ||
                                     (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                                      pItem1->item.asDateTime.time < pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian < pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value = !pItem1->item.asLogical.value && pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_LESS, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1073, nullptr, "<", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmLessEqual()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLessEqual()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    int i = hb_itemStrCmp(pItem1, pItem2, false);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = i <= 0;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) <= HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) <= HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian < pItem2->item.asDateTime.julian) ||
                                     (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                                      pItem1->item.asDateTime.time <= pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian <= pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value = !pItem1->item.asLogical.value || pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_LESSEQUAL, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1074, nullptr, "<=", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmGreater()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreater()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    int i = hb_itemStrCmp(pItem1, pItem2, false);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = i > 0;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) > HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) > HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian > pItem2->item.asDateTime.julian) ||
                                     (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                                      pItem1->item.asDateTime.time > pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian > pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value = pItem1->item.asLogical.value && !pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_GREATER, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1075, nullptr, ">", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmGreaterEqual()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreaterEqual()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    int i = hb_itemStrCmp(pItem1, pItem2, false);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = i >= 0;
  }
  else if (HB_IS_NUMINT(pItem1) && HB_IS_NUMINT(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMINTRAW(pItem1) >= HB_ITEM_GET_NUMINTRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_NUMERIC(pItem1) && HB_IS_NUMERIC(pItem2))
  {
    pItem1->item.asLogical.value = (HB_ITEM_GET_NUMDBLRAW(pItem1) >= HB_ITEM_GET_NUMDBLRAW(pItem2));
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (HB_IS_DATETIME(pItem1) && HB_IS_DATETIME(pItem2))
  {
    if (pItem1->isTimeStamp() && pItem2->isTimeStamp())
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian > pItem2->item.asDateTime.julian) ||
                                     (pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
                                      pItem1->item.asDateTime.time >= pItem2->item.asDateTime.time);
    }
    else
    {
      pItem1->item.asLogical.value = (pItem1->item.asDateTime.julian >= pItem2->item.asDateTime.julian);
    }
    pItem1->type = Harbour::Item::LOGICAL;
    hb_stackDec();
  }
  else if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->item.asLogical.value = pItem1->item.asLogical.value || !pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_GREATEREQUAL, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1076, nullptr, ">=", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmInstring()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInstring()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem1 = hb_stackItemFromTop(-2);
  auto pItem2 = hb_stackItemFromTop(-1);

  if (HB_IS_STRING(pItem1) && HB_IS_STRING(pItem2))
  {
    bool fResult = (hb_strAt(pItem1->item.asString.value, pItem1->item.asString.length, pItem2->item.asString.value,
                             pItem2->item.asString.length) != 0);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (HB_IS_HASH(pItem2) && (HB_IS_HASHKEY(pItem1) || hb_hashLen(pItem1) == 1))
  {
    bool fResult = hb_hashScan(pItem2, pItem1, nullptr);
    hb_stackPop();
    hb_itemClear(pItem1);
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = fResult;
  }
  else if (hb_objOperatorCall(HB_OO_OP_INCLUDE, pItem1, pItem2, pItem1, nullptr))
  {
    hb_stackPop();
  }
  else if (hb_objOperatorCall(HB_OO_OP_INSTRING, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1109, nullptr, "$", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

/* At this moment the eval stack should store:
 * -3 -> <current counter value>
 * -2 -> <end value>
 * -1 -> <step value>
 */
static void hb_vmForTest() /* Test to check the end point of the FOR */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmForTest()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto fBack = false;

  auto pStep = hb_stackItemFromTop(-1);
  if (HB_IS_NUMERIC(pStep))
  {
    fBack = HB_ITEM_GET_NUMDBLRAW(pStep) < 0.0;
    hb_stackDec();
  }
  else
  {
    hb_vmPushInteger(0);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1073, nullptr, "<", 2, pStep, hb_stackItemFromTop(-1));

    if (pResult)
    {
      if (pResult->isLogical())
      {
        fBack = pResult->item.asLogical.value;
        hb_itemRelease(pResult);
        hb_stackPop();
        hb_stackPop();
      }
      else
      {
        hb_itemMove(hb_stackItemFromTop(-1), pResult);
        hb_itemRelease(pResult);
        hb_errRT_BASE(EG_ARG, 1066, nullptr, hb_langDGetErrorDesc(EG_CONDITION), 1, hb_stackItemFromTop(-1));
        return;
      }
    }
    else
    {
      return;
    }
  }

  if (fBack)
  {
    hb_vmLess();
  }
  else
  {
    hb_vmGreater();
  }
}

/* Begin Sequence WITH block auto destructor */
static HB_GARBAGE_FUNC(hb_SeqBlockDestructor)
{
  hb_itemMove(hb_errorBlock(), static_cast<PHB_ITEM>(Cargo));
}

static const HB_GC_FUNCS s_gcSeqBlockFuncs = {hb_SeqBlockDestructor, hb_gcGripMark};

static void hb_vmSeqBlock()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSeqBlock()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isBlock())
  {
    PHB_ITEM pBlock = hb_errorBlock();
    auto pBlockCopy = static_cast<PHB_ITEM>(hb_gcAllocRaw(sizeof(HB_ITEM), &s_gcSeqBlockFuncs));
    hb_itemRawCpy(pBlockCopy, pBlock);
    hb_itemRawCpy(pBlock, pItem);
    pItem->type = Harbour::Item::POINTER;
    pItem->item.asPointer.value = pBlockCopy;
    pItem->item.asPointer.collect = pItem->item.asPointer.single = true;
  }
}

/* With object auto destructor */
static HB_GARBAGE_FUNC(hb_withObjectDestructor)
{
  HB_STACK_TLS_PRELOAD
  auto pnWithObjectBase = static_cast<HB_ISIZ *>(Cargo);
  hb_stackWithObjectSetOffset(*pnWithObjectBase);
}

static const HB_GC_FUNCS s_gcWithObjectFuncs = {hb_withObjectDestructor, hb_gcDummyMark};

static void hb_vmWithObjectStart()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmWithObjectStart()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackAllocItem();
  auto pnWithObjectBase = static_cast<HB_ISIZ *>(hb_gcAllocRaw(sizeof(HB_ISIZ), &s_gcWithObjectFuncs));
  *pnWithObjectBase = hb_stackWithObjectOffset();
  pItem->type = Harbour::Item::POINTER;
  pItem->item.asPointer.value = pnWithObjectBase;
  pItem->item.asPointer.collect = pItem->item.asPointer.single = true;
  /* The object is pushed directly before this pcode */
  /* store position of current WITH OBJECT frame */
  hb_stackWithObjectSetOffset(hb_stackTopOffset() - 2);
}

/*
 * Release enumerator items - called from hb_itemClear()
 */
void hb_vmEnumRelease(PHB_ITEM pBase, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEnumRelease(%p,%p)", static_cast<void*>(pBase), static_cast<void*>(pValue)));
#endif

  HB_STACK_TLS_PRELOAD

  if (pValue)
  {
    hb_itemRelease(pValue);
  }

  if (HB_IS_OBJECT(pBase) && hb_vmRequestQuery() == 0 && hb_objHasOperator(pBase, HB_OO_OP_ENUMSTOP))
  {
    hb_stackPushReturn();
    HB_VM_PUSHNIL();
    hb_objOperatorCall(HB_OO_OP_ENUMSTOP, hb_stackItemFromTop(-1), pBase, nullptr, nullptr);
    hb_stackPop();
    hb_stackPopReturn();
  }
}

/*
 * extended reference used as enumerator destructor
 */
struct HB_ENUMREF
{
  HB_ITEM basevalue;
  HB_ITEM oldvalue;
  HB_ITEM enumref;
};

using PHB_ENUMREF = HB_ENUMREF *;

static PHB_ITEM hb_vmEnumRefRead(PHB_ITEM pRefer)
{
  return &(static_cast<PHB_ENUMREF>(pRefer->item.asExtRef.value))->oldvalue;
}

static PHB_ITEM hb_vmEnumRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  HB_SYMBOL_UNUSED(pRefer);
  HB_SYMBOL_UNUSED(pSource);
  return nullptr;
}

static void hb_vmEnumRefCopy(PHB_ITEM pDest)
{
  pDest->type = Harbour::Item::NIL;
}

static void hb_vmEnumRefClear(void *value)
{
  hb_itemMove(hb_itemUnRefOnce(&(static_cast<PHB_ENUMREF>(value))->enumref),
              &(static_cast<PHB_ENUMREF>(value))->oldvalue);
  if (HB_IS_COMPLEX(&(static_cast<PHB_ENUMREF>(value))->basevalue))
  {
    hb_itemClear(&(static_cast<PHB_ENUMREF>(value))->basevalue);
  }
  if (HB_IS_COMPLEX(&(static_cast<PHB_ENUMREF>(value))->enumref))
  {
    hb_itemClear(&(static_cast<PHB_ENUMREF>(value))->enumref);
  }

  hb_xfree(value);
}

static void hb_vmEnumRefMark(void *value)
{
  if (HB_IS_GCITEM(&(static_cast<PHB_ENUMREF>(value))->basevalue))
  {
    hb_gcItemRef(&(static_cast<PHB_ENUMREF>(value))->basevalue);
  }
  if (HB_IS_GCITEM(&(static_cast<PHB_ENUMREF>(value))->oldvalue))
  {
    hb_gcItemRef(&(static_cast<PHB_ENUMREF>(value))->oldvalue);
  }
  if (HB_IS_GCITEM(&(static_cast<PHB_ENUMREF>(value))->enumref))
  {
    hb_gcItemRef(&(static_cast<PHB_ENUMREF>(value))->enumref);
  }
}

/*
 * create extended reference for enumerator destructor
 */
static void hb_vmEnumReference(PHB_ITEM pBase)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEnumReference(%p)", static_cast<void*>(pBase)));
#endif

  static const HB_EXTREF s_EnumExtRef = {hb_vmEnumRefRead, hb_vmEnumRefWrite, hb_vmEnumRefCopy, hb_vmEnumRefClear,
                                         hb_vmEnumRefMark};

  auto pEnumExtRef = static_cast<PHB_ENUMREF>(hb_xgrab(sizeof(HB_ENUMREF)));
  pEnumExtRef->oldvalue.type = Harbour::Item::NIL;
  pEnumExtRef->enumref.type = Harbour::Item::NIL;
  hb_itemRawCpy(&pEnumExtRef->basevalue, pBase);
  pBase->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pBase->item.asExtRef.value = static_cast<void *>(pEnumExtRef);
  pBase->item.asExtRef.func = &s_EnumExtRef;
}

/* At this moment the eval stack should store:
 * -2 -> <array for traverse>
 * -1 -> <the reference to enumerate variable>
 */
/* Test to check the start point of the FOR EACH loop */
static void hb_vmEnumStart(int nVars, int nDescend)
{
  HB_STACK_TLS_PRELOAD
  auto fStart = true;

#if 0
   pItem = hb_itemUnRef(hb_stackItemFromTop(-(static_cast<int>(nVars) << 1)));
   if( (pItem->type & (Harbour::Item::ARRAY | Harbour::Item::HASH | Harbour::Item::STRING)) == 0 ) {
      hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 1, pItem);
      return;
   }
#endif

  for (int i = static_cast<int>(nVars) << 1; i > 0 && fStart; i -= 2)
  {
    PHB_ITEM pBase;

    auto pValue = hb_stackItemFromTop(-i);
    /* create extended reference for enumerator destructor */
    hb_vmEnumReference(pValue);
    /* store the reference to control variable */
    auto pEnumRef = hb_stackItemFromTop(-i + 1);
    hb_itemCopy(&(static_cast<PHB_ENUMREF>(pValue->item.asExtRef.value))->enumref, pEnumRef);
    /* the control variable */
    auto pEnum = hb_itemUnRefOnce(pEnumRef);
    /* store the old value of control variable and clear it */
    hb_itemMove(&(static_cast<PHB_ENUMREF>(pValue->item.asExtRef.value))->oldvalue, pEnum);

    /* set the iterator value */
    pEnum->type = Harbour::Item::BYREF | Harbour::Item::ENUM;
    pEnum->item.asEnum.basePtr = pBase = &(static_cast<PHB_ENUMREF>(pValue->item.asExtRef.value))->basevalue;
    pEnum->item.asEnum.valuePtr = nullptr;

    if (HB_IS_BYREF(pBase))
    {
      pBase = hb_itemUnRef(pBase);
    }

    if (HB_IS_OBJECT(pBase) && hb_objHasOperator(pBase, HB_OO_OP_ENUMSTART))
    {
      pEnum->item.asEnum.offset = 0;
      pEnum->item.asEnum.valuePtr = hb_itemNew(nullptr);
      HB_VM_PUSHNIL();
      hb_vmPushLogical(nDescend == 0);
      hb_objOperatorCall(HB_OO_OP_ENUMSTART, hb_stackItemFromTop(-2), pBase, pEnumRef, hb_stackItemFromTop(-1));
      hb_stackPop();
      if (hb_vmRequestQuery() != 0 || !hb_vmPopLogical())
      {
        fStart = false;
        break;
      }
      else if (hb_objHasOperator(pBase, HB_OO_OP_ENUMSKIP))
      {
        continue;
      }
      hb_itemRelease(pEnum->item.asEnum.valuePtr);
      pEnum->item.asEnum.valuePtr = nullptr;
    }

    if (pBase->isArray())
    {
      /* the index into an array */
      pEnum->item.asEnum.offset = (nDescend > 0) ? 1 : pBase->item.asArray.value->nLen;
      if (pBase->item.asArray.value->nLen == 0)
      {
        fStart = false;
      }
    }
    else if (HB_IS_HASH(pBase))
    {
      HB_SIZE nLen = hb_hashLen(pBase);
      /* the index into a hash */
      pEnum->item.asEnum.offset = (nDescend > 0) ? 1 : nLen;
      if (nLen == 0)
      {
        fStart = false;
      }
    }
    else if (HB_IS_STRING(pBase))
    {
      /* storage item for single characters */
      pEnum->item.asEnum.offset = (nDescend > 0) ? 1 : pBase->item.asString.length;
      if (pBase->item.asString.length)
      {
        pEnum->item.asEnum.valuePtr =
            hb_itemPutCL(nullptr, pBase->item.asString.value + pEnum->item.asEnum.offset - 1, 1);
      }
      else
      {
        fStart = false;
      }
    }
    else if (hb_vmRequestQuery() == 0)
    {
      hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 1, pBase);
      return;
    }
  }

  hb_vmPushInteger(nVars); /* number of iterators */
  /* empty array/string - do not start enumerations loop */
  hb_vmPushLogical(fStart);
}

/* Enumeration in ascending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumNext()
{
  HB_STACK_TLS_PRELOAD
  int i;

  for (i = static_cast<int>(hb_stackItemFromTop(-1)->item.asInteger.value); i > 0; --i)
  {
    auto pEnumRef = hb_stackItemFromTop(-(i << 1));
    auto pEnum = hb_itemUnRefOnce(pEnumRef);
    PHB_ITEM pBase = pEnum->item.asEnum.basePtr;
    if (HB_IS_BYREF(pBase))
    {
      pBase = hb_itemUnRef(pBase);
    }
    if (pBase->isArray())
    {
      if (HB_IS_OBJECT(pBase) && hb_objHasOperator(pBase, HB_OO_OP_ENUMSKIP))
      {
        ++pEnum->item.asEnum.offset;
        HB_VM_PUSHNIL();
        hb_vmPushLogical(false);
        hb_objOperatorCall(HB_OO_OP_ENUMSKIP, hb_stackItemFromTop(-2), pBase, pEnumRef, hb_stackItemFromTop(-1));
        hb_stackPop();
        if (hb_vmRequestQuery() != 0 || !hb_vmPopLogical())
        {
          break;
        }
      }
      else
      {
        /* Clear the item value which can be set with RT error
           when enumerator was out of array size during unreferencing
         */
        if (pEnum->item.asEnum.valuePtr)
        {
          hb_itemRelease(pEnum->item.asEnum.valuePtr);
          pEnum->item.asEnum.valuePtr = nullptr;
        }
        if (static_cast<HB_SIZE>(++pEnum->item.asEnum.offset) > pBase->item.asArray.value->nLen)
        {
          break;
        }
      }
    }
    else if (HB_IS_HASH(pBase))
    {
      /* Clear the item value which can be set with RT error
         when enumerator was out of array size during unreferencing
       */
      if (pEnum->item.asEnum.valuePtr)
      {
        hb_itemRelease(pEnum->item.asEnum.valuePtr);
        pEnum->item.asEnum.valuePtr = nullptr;
      }
      if (static_cast<HB_SIZE>(++pEnum->item.asEnum.offset) > hb_hashLen(pBase))
      {
        break;
      }
    }
    else if (HB_IS_STRING(pBase))
    {
      if (static_cast<HB_SIZE>(++pEnum->item.asEnum.offset) > pBase->item.asString.length)
      {
        break;
      }
      pEnum->item.asEnum.valuePtr =
          hb_itemPutCL(pEnum->item.asEnum.valuePtr, pBase->item.asString.value + pEnum->item.asEnum.offset - 1, 1);
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 1, pBase);
      return;
    }
  }
  hb_vmPushLogical(i == 0);
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumPrev()
{
  HB_STACK_TLS_PRELOAD
  int i;

  for (i = hb_stackItemFromTop(-1)->item.asInteger.value; i > 0; --i)
  {
    auto pEnumRef = hb_stackItemFromTop(-(i << 1));
    auto pEnum = hb_itemUnRefOnce(pEnumRef);
    PHB_ITEM pBase = pEnum->item.asEnum.basePtr;
    if (HB_IS_BYREF(pBase))
    {
      pBase = hb_itemUnRef(pBase);
    }
    if (pBase->isArray())
    {
      if (HB_IS_OBJECT(pBase) && hb_objHasOperator(pBase, HB_OO_OP_ENUMSKIP))
      {
        --pEnum->item.asEnum.offset;
        HB_VM_PUSHNIL();
        hb_vmPushLogical(true);
        hb_objOperatorCall(HB_OO_OP_ENUMSKIP, hb_stackItemFromTop(-2), pBase, pEnumRef, hb_stackItemFromTop(-1));
        hb_stackPop();
        if (hb_vmRequestQuery() != 0 || !hb_vmPopLogical())
        {
          break;
        }
      }
      else
      {
        /* Clear the item value which can be set with RT error
           when enumerator was out of array size during unreferencing
         */
        if (pEnum->item.asEnum.valuePtr)
        {
          hb_itemRelease(pEnum->item.asEnum.valuePtr);
          pEnum->item.asEnum.valuePtr = nullptr;
        }
        if (--pEnum->item.asEnum.offset == 0)
        {
          break;
        }
      }
    }
    else if (HB_IS_HASH(pBase))
    {
      /* Clear the item value which can be set with RT error
         when enumerator was out of array size during unreferencing
       */
      if (pEnum->item.asEnum.valuePtr)
      {
        hb_itemRelease(pEnum->item.asEnum.valuePtr);
        pEnum->item.asEnum.valuePtr = nullptr;
      }
      if (--pEnum->item.asEnum.offset == 0)
      {
        break;
      }
    }
    else if (HB_IS_STRING(pBase))
    {
      if (--pEnum->item.asEnum.offset == 0)
      {
        break;
      }
      pEnum->item.asEnum.valuePtr =
          hb_itemPutCL(pEnum->item.asEnum.valuePtr, pBase->item.asString.value + pEnum->item.asEnum.offset - 1, 1);
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 1, pBase);
      return;
    }
  }
  hb_vmPushLogical(i == 0);
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumEnd()
{
  HB_STACK_TLS_PRELOAD

  /* remove number of iterators */
  int iVars = hb_stackItemFromTop(-1)->item.asInteger.value;
  hb_stackDec();

  while (--iVars >= 0)
  {
    hb_stackPop();
    hb_stackPop();
  }
}

static PHB_ITEM hb_vmSwitchGet()
{
  HB_STACK_TLS_PRELOAD
  auto pSwitch = hb_stackItemFromTop(-1);

  if (!(HB_IS_NUMINT(pSwitch) || HB_IS_STRING(pSwitch)))
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 3104, nullptr, "SWITCH", 1, pSwitch);

    if (!pResult)
    {
      return nullptr;
    }

    hb_itemMove(pSwitch, pResult);
    hb_itemRelease(pResult);
  }

  return pSwitch;
}

static const HB_BYTE *hb_vmSwitch(const HB_BYTE *pCode, HB_USHORT casesCnt)
{
  HB_STACK_TLS_PRELOAD
  PHB_ITEM pSwitch = hb_vmSwitchGet();

  if (pSwitch)
  {
    auto fFound = false;

    while (!fFound && casesCnt--)
    {
      switch (pCode[0])
      {
      case HB_P_PUSHLONG:
        if (HB_IS_NUMINT(pSwitch))
        {
          fFound = HB_ITEM_GET_NUMINTRAW(pSwitch) == HB_PCODE_MKLONG(&pCode[1]);
        }
        pCode += 5;
        break;

      case HB_P_PUSHSTRSHORT:
        if (HB_IS_STRING(pSwitch))
        {
#if 0
                  fFound = hb_itemStrCmp(pItem1, pItem2, bExact);
#endif
          fFound = static_cast<HB_SIZE>(pCode[1]) - 1 == pSwitch->item.asString.length &&
                   memcmp(pSwitch->item.asString.value, &pCode[2], pSwitch->item.asString.length) == 0;
        }
        pCode += 2 + pCode[1];
        break;

      case HB_P_PUSHNIL:
        /* default clause */
        fFound = true;
        pCode++;
        break;
      }

      switch (pCode[0])
      {
      case HB_P_JUMPNEAR:
        if (fFound)
        {
          pCode += static_cast<signed char>(pCode[1]);
        }
        else
        {
          pCode += 2;
        }
        break;
      case HB_P_JUMP:
        if (fFound)
        {
          pCode += HB_PCODE_MKSHORT(&pCode[1]);
        }
        else
        {
          pCode += 3;
        }
        break;
      case HB_P_JUMPFAR:
        if (fFound)
        {
          pCode += HB_PCODE_MKINT24(&pCode[1]);
        }
        else
        {
          pCode += 4;
        }
        break;
      }
    }
  }
  hb_stackPop();
  return pCode;
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmNot()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);

  if (pItem->isLogical())
  {
    pItem->type = Harbour::Item::LOGICAL;
    pItem->item.asLogical.value = !pItem->item.asLogical.value;
  }
  else if (!hb_objOperatorCall(HB_OO_OP_NOT, pItem, pItem, nullptr, nullptr))
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1077, nullptr, ".NOT.", 1, pItem);

    if (pResult)
    {
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmAnd()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmAnd()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_AND, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1078, nullptr, ".AND.", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

static void hb_vmOr()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmOr()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem2 = hb_stackItemFromTop(-1);
  auto pItem1 = hb_stackItemFromTop(-2);

  if (pItem1->isLogical() && pItem2->isLogical())
  {
    pItem1->type = Harbour::Item::LOGICAL;
    pItem1->item.asLogical.value = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
    hb_stackDec();
  }
  else if (hb_objOperatorCall(HB_OO_OP_OR, pItem1, pItem1, pItem2, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1079, nullptr, ".OR.", 2, pItem1, pItem2);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem1, pResult);
      hb_itemRelease(pResult);
    }
  }
}

/* ------------------------------- */
/* Array                           */
/* ------------------------------- */

static void hb_vmArrayPush()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPush()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_SIZE nIndex;

  auto pIndex = hb_stackItemFromTop(-1);
  auto pArray = hb_stackItemFromTop(-2);

  if (HB_IS_HASH(pArray) && HB_IS_HASHKEY(pIndex))
  {
    auto pValue = hb_hashGetItemPtr(pArray, pIndex, HB_HASH_AUTOADD_ACCESS);
    if (pValue)
    {
      hb_itemCopy(pIndex, pValue);
      hb_itemMove(pArray, pIndex);
      hb_stackDec();
    }
    else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
    {
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
    }
    return;
  }
  else if (pIndex->isInteger())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asInteger.value);
  }
  else if (HB_IS_LONG(pIndex))
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asLong.value);
  }
  else if (pIndex->isDouble())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asDouble.value);
  }
  else
  {
    if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
    {
      hb_stackPop();
    }
    else
    {
      PHB_ITEM pResult =
          hb_errRT_BASE_Subst(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
      if (pResult)
      {
        hb_stackPop();
        hb_itemMove(pArray, pResult);
        hb_itemRelease(pResult);
      }
    }
    return;
  }

  if (pArray->isArray())
  {
    if (HB_IS_OBJECT(pArray) && hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
    {
      hb_stackPop();
      return;
    }

    if (HB_IS_VALID_INDEX(nIndex, pArray->item.asArray.value->nLen))
    {
      hb_itemCopy(pIndex, pArray->item.asArray.value->pItems + nIndex - 1);
      hb_itemMove(pArray, pIndex);
      hb_stackDec();
    }
    else if (!HB_IS_OBJECT(pArray) && hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
    {
      hb_stackPop();
    }
    else
    {
#ifdef HB_CLP_STRICT
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 0);
#else
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
#endif
    }
  }
  else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
  {
    hb_stackPop();
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
  }
}

static void hb_vmArrayPushRef()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPushRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_SIZE nIndex;

  auto pIndex = hb_stackItemFromTop(-1);
  auto pRefer = hb_stackItemFromTop(-2);
  PHB_ITEM pArray = HB_IS_BYREF(pRefer) ? hb_itemUnRef(pRefer) : pRefer;

  if (HB_IS_HASH(pArray) && HB_IS_HASHKEY(pIndex))
  {
    auto pValue = hb_hashGetItemRefPtr(pArray, pIndex);
    if (pValue)
    {
      hb_itemCopy(pIndex, pValue);
      hb_itemMove(pRefer, pIndex);
      hb_stackDec();
    }
    else if (hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
    {
      /* create extended object index reference */
      hb_vmMsgIndexReference(pRefer, pArray, pIndex);
      hb_stackPop();
      return;
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
    }
    return;
  }
  else if (pIndex->isInteger())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asInteger.value);
  }
  else if (HB_IS_LONG(pIndex))
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asLong.value);
  }
  else if (pIndex->isDouble())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asDouble.value);
  }
  else if (hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
  {
    /* create extended object index reference */
    hb_vmMsgIndexReference(pRefer, pArray, pIndex);
    hb_stackPop();
    return;
  }
  else
  {
    PHB_ITEM pResult =
        hb_errRT_BASE_Subst(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);

    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pRefer, pResult);
      hb_itemRelease(pResult);
    }
    return;
  }

  if (pArray->isArray())
  {
    if (HB_IS_OBJECT(pArray) && hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
    {
      /* create extended object index reference */
      hb_vmMsgIndexReference(pRefer, pArray, pIndex);
      hb_stackPop();
      return;
    }
    else if (HB_IS_VALID_INDEX(nIndex, pArray->item.asArray.value->nLen))
    {
      /* This function is safe for overwriting passed array, [druzus] */
      hb_arrayGetItemRef(pArray, nIndex, pRefer);
      hb_stackDec();
    }
    else if (!HB_IS_OBJECT(pArray) && hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
    {
      /* create extended object index reference */
      hb_vmMsgIndexReference(pRefer, pArray, pIndex);
      hb_stackPop();
      return;
    }
    else
    {
#ifdef HB_CLP_STRICT
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 0);
#else
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
#endif
    }
  }
  else if (hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
  {
    /* create extended object index reference */
    hb_vmMsgIndexReference(pRefer, pArray, pIndex);
    hb_stackPop();
    return;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
  }
}

static void hb_vmArrayPop()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_SIZE nIndex;

  auto pValue = hb_stackItemFromTop(-3);
  auto pArray = hb_stackItemFromTop(-2);
  auto pIndex = hb_stackItemFromTop(-1);

  if (HB_IS_BYREF(pArray))
  {
    pArray = hb_itemUnRef(pArray);
  }

  if (HB_IS_HASH(pArray) && HB_IS_HASHKEY(pIndex))
  {
    auto pDest = hb_hashGetItemPtr(pArray, pIndex, HB_HASH_AUTOADD_ASSIGN);
    if (pDest)
    {
      pValue->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
      hb_itemMoveFromRef(pDest, pValue);
      hb_stackPop();
      hb_stackPop();
      hb_stackDec(); /* value was moved above hb_stackDec() is enough */
    }
    else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 3, pArray, pIndex, pValue);
    }
    return;
  }
  else if (pIndex->isInteger())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asInteger.value);
  }
  else if (HB_IS_LONG(pIndex))
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asLong.value);
  }
  else if (pIndex->isDouble())
  {
    nIndex = static_cast<HB_SIZE>(pIndex->item.asDouble.value);
  }
  else
  {
    if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1069, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
    }
    return;
  }

  if (pArray->isArray())
  {
    if (HB_IS_OBJECT(pArray) && hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
      return;
    }

    if (HB_IS_VALID_INDEX(nIndex, pArray->item.asArray.value->nLen))
    {
      pValue->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
      hb_itemMoveRef(pArray->item.asArray.value->pItems + nIndex - 1, pValue);
      hb_stackPop();
      hb_stackPop();
      hb_stackDec(); /* value was moved above hb_stackDec() is enough */
    }
    else if (!HB_IS_OBJECT(pArray) && hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
    }
    else
    {
#ifdef HB_CLP_STRICT
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 0);
#else
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
#endif
    }
  }
  else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue))
  {
    hb_stackPop();
    hb_stackPop();
    hb_stackPop();
    return;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1069, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, pIndex);
  }
}

static void hb_vmArrayGen(HB_SIZE nElements) /* generates an nElements Array and fills it from the stack values */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayGen(%" HB_PFS "u)", nElements));
#endif

  HB_STACK_TLS_PRELOAD

  /* create new array on HVM stack */
  auto pArray = hb_stackAllocItem();
  hb_arrayNew(pArray, nElements);

  if (nElements)
  {
    /* move items from HVM stack to created array */
    for (HB_SIZE nPos = 0; nPos < nElements; nPos++)
    {
      auto pValue = hb_stackItemFromTop(static_cast<int>(nPos - nElements - 1));
      pValue->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
      hb_itemMove(pArray->item.asArray.value->pItems + nPos, pValue);
    }
    /* move the new array to position of first parameter */
    hb_itemMove(hb_stackItemFromTop(-1 - static_cast<int>(nElements)), pArray);

    /* decrease the stack counter - all items are NIL */
    hb_stackDecrease(nElements);
  }
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static HB_BOOL hb_vmArrayNew(PHB_ITEM pArray, HB_USHORT uiDimension)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayNew(%p, %hu)", static_cast<void*>(pArray), uiDimension));
#endif

  HB_STACK_TLS_PRELOAD
  HB_ISIZ nElements;

  auto pDim = hb_stackItemFromTop(static_cast<int>(-1 - uiDimension));

  /* use the proper type of number of elements */
  if (pDim->isInteger())
  {
    nElements = static_cast<HB_ISIZ>(pDim->item.asInteger.value);
  }
  else if (HB_IS_LONG(pDim))
  {
    nElements = static_cast<HB_ISIZ>(pDim->item.asLong.value);
  }
  else if (pDim->isDouble())
  {
    nElements = static_cast<HB_ISIZ>(pDim->item.asDouble.value);
  }
  else
  {
    /* NOTE: Clipper creates empty array if non-numeric value is
     * specified as dimension and stops further processing.
     * There is no runtime error generated.
     */
    nElements = 0;
  }

  if (nElements >= 0)
  {
    /* create an array */
    hb_arrayNew(pArray, nElements);

    if (--uiDimension)
    {
      /* call self recursively to create next dimensions */
      while (nElements--)
      {
        if (!hb_vmArrayNew(pArray->item.asArray.value->pItems + nElements, uiDimension))
        {
          return false;
        }
      }
    }
    return true;
  }

  hb_errRT_BASE(EG_BOUND, 1131, nullptr, hb_langDGetErrorDesc(EG_ARRDIMENSION), 0);
  return false;
}

static void hb_vmArrayDim(
    HB_USHORT uiDimensions) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayDim(%hu)", uiDimensions));
#endif

  HB_STACK_TLS_PRELOAD

  if (uiDimensions)
  {
    hb_vmArrayNew(hb_stackAllocItem(), uiDimensions);

    hb_itemMove(hb_stackItemFromTop(static_cast<int>(-1 - uiDimensions)), hb_stackItemFromTop(-1));
    do
    {
      hb_stackPop();
    } while (--uiDimensions);
  }
  else
  {
    HB_VM_PUSHNIL();
  }
}

static void hb_vmHashGen(HB_SIZE nElements) /* generates an nElements Hash and fills it from the stack values */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmHashGen(%" HB_PFS "u)", nElements));
#endif

  HB_STACK_TLS_PRELOAD

  /* create new hash item */
  PHB_ITEM pHash = hb_hashNew(nullptr);
  hb_hashPreallocate(pHash, nElements);
  nElements <<= 1;
  int iPos = -static_cast<int>(nElements);
  while (iPos)
  {
    auto pKey = hb_stackItemFromTop(iPos++);
    auto pVal = hb_stackItemFromTop(iPos++);
    if (HB_IS_HASHKEY(pKey))
    {
      hb_hashAdd(pHash, pKey, pVal);
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 3, pHash, pKey, pVal);
      break;
    }
  }
  hb_stackRemove(hb_stackTopOffset() - nElements);
  hb_itemMove(hb_stackAllocItem(), pHash);
  hb_itemRelease(pHash);
}

/* ------------------------------- */
/* Macros                          */
/* ------------------------------- */

static void hb_vmMacroPushIndex()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMacroPushIndex()"));
#endif

  HB_STACK_TLS_PRELOAD

  /*
   * Now the top most element on the stack points to number of
   * additional indexes to generated array
   */
  HB_SIZE nIndexes = hb_itemGetNS(hb_stackItemFromTop(-1));
  hb_stackDec();

  if (nIndexes > 1)
  {
    HB_SIZE n = 1;

    hb_vmArrayGen(nIndexes - 1);
    auto pIndexArray = hb_itemNew(hb_stackItemFromTop(-1));
    hb_stackPop();

    /* First index is still on stack.*/
    do
    {
      auto pArray = hb_stackItemFromTop(-2);
      if (HB_IS_BYREF(pArray))
      {
        hb_vmArrayPushRef();
      }
      else
      {
        hb_vmArrayPush();
      }
      /* RT error? */
      if (hb_stackGetActionRequest() != 0)
      {
        break;
      }
      hb_vmPush(hb_arrayGetItemPtr(pIndexArray, n));
    } while (++n < nIndexes);

    hb_itemRelease(pIndexArray);
  }
  else if (nIndexes == 0)
  {
    HB_VM_PUSHNIL(); /* It will force RT error later in array push or pop */
  }
}

/*
 * On HVM stack we have sets with arguments
 *    offset   value
 *    (-9)     6
 *    (-8)     7
 *    (-7)     2 // number of arguments
 *    (-6)     1
 *    (-5)     2
 *    (-4)     2 // number of arguments
 *    (-3)     1
 *    (-2)     2
 *    (-1)     2 // number of arguments
 * we should join them into one continuous list
 */
static HB_LONG hb_vmArgsJoin(HB_LONG lLevel, HB_USHORT uiArgSets)
{
  HB_STACK_TLS_PRELOAD
  auto pArgs = hb_stackItemFromTop(lLevel);

  HB_LONG lArgs = hb_itemGetNL(pArgs);
  if (HB_IS_COMPLEX(pArgs))
  {
    hb_itemClear(pArgs);
  }

  if (--uiArgSets)
  {
    HB_LONG lRestArgs = lArgs;
    lArgs += hb_vmArgsJoin(lLevel - lArgs - 1, uiArgSets);
    HB_LONG lOffset = lLevel - lRestArgs - uiArgSets;
    while (lRestArgs--)
    {
      hb_itemMove(hb_stackItemFromTop(lOffset), hb_stackItemFromTop(lOffset + uiArgSets));
      ++lOffset;
    }
  }

  return lArgs;
}

static void hb_vmMacroDo(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMacroDo(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  HB_LONG lArgs = hb_vmArgsJoin(-1, uiArgSets);
  hb_stackDecrease(uiArgSets);
  hb_vmProc(static_cast<HB_USHORT>(lArgs));
}

static void hb_vmMacroFunc(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMacroFunc(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  HB_LONG lArgs = hb_vmArgsJoin(-1, uiArgSets);
  hb_stackDecrease(uiArgSets);
  hb_itemSetNil(hb_stackReturnItem());
  hb_vmProc(static_cast<HB_USHORT>(lArgs));
  hb_stackPushReturn();
}

static void hb_vmMacroSend(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMacroSend(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  HB_LONG lArgs = hb_vmArgsJoin(-1, uiArgSets);
  hb_stackDecrease(uiArgSets);
  hb_itemSetNil(hb_stackReturnItem());
  hb_vmSend(static_cast<HB_USHORT>(lArgs));
  hb_stackPushReturn();
}

static void hb_vmMacroArrayGen(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMacroArrayGen(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  HB_LONG lArgs = hb_vmArgsJoin(-1, uiArgSets);
  hb_stackDecrease(uiArgSets);
  hb_vmArrayGen(lArgs);
}

static void hb_vmPushVParams()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushVParams()"));
#endif

  HB_STACK_TLS_PRELOAD
  int i = 0;

  auto pBase = hb_stackBaseItem();
  int iFirst = pBase->item.asSymbol.paramdeclcnt;
  int iPCount = pBase->item.asSymbol.paramcnt;
  while (++iFirst <= iPCount)
  {
    hb_vmPush(hb_stackItemFromBase(iFirst));
    i++;
  }
  hb_vmPushInteger(i);
}

static void hb_vmPushAParams()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAParams()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pArray = hb_stackItemFromTop(-1);
  if (pArray->isArray())
  {
    HB_SIZE nLen = pArray->item.asArray.value->nLen;

    if (nLen)
    {
      for (HB_SIZE nPos = 1; nPos < nLen; ++nPos)
      {
        hb_vmPush(pArray->item.asArray.value->pItems + nPos);
      }
      auto pCount = hb_stackAllocItem();
      hb_itemCopy(pCount, pArray->item.asArray.value->pItems);
      hb_itemMove(pArray, pCount);
      hb_itemPutNS(pCount, nLen);
    }
    else
    {
      hb_itemPutNL(pArray, 0);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 1, pArray);
  }
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static HB_ERRCODE hb_vmSelectWorkarea(PHB_ITEM pAlias, PHB_SYMB pField)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSelectWorkArea(%p,%p)", static_cast<void*>(pAlias), static_cast<void*>(pField)));
#endif

  HB_STACK_TLS_PRELOAD
  HB_ERRCODE errCode;
  auto fRepeat = false;

  /* NOTE: Clipper doesn't generate an error if an workarea specified
   * as numeric value cannot be selected
   */
  do
  {
    fRepeat = false;
    errCode = Harbour::SUCCESS;

    switch (HB_ITEM_TYPE(pAlias))
    {
    case Harbour::Item::INTEGER:
      /* Alias was used as integer value, for example: 4->field
       * or it was saved on the stack using hb_vmPushAlias()
       * or was evaluated from an expression, (nWorkArea)->field
       */
      hb_rddSelectWorkAreaNumber(pAlias->item.asInteger.value);
      pAlias->type = Harbour::Item::NIL;
      break;

    case Harbour::Item::LONG:
      /* Alias was evaluated from an expression, (nWorkArea)->field
       */
      hb_rddSelectWorkAreaNumber(static_cast<int>(pAlias->item.asLong.value));
      pAlias->type = Harbour::Item::NIL;
      break;

    case Harbour::Item::DOUBLE:
      /* Alias was evaluated from an expression, (nWorkArea)->field
       */
      hb_rddSelectWorkAreaNumber(static_cast<int>(pAlias->item.asDouble.value));
      pAlias->type = Harbour::Item::NIL;
      break;

    case Harbour::Item::SYMBOL:
      /* Alias was specified using alias identifier, for example: al->field
       */
      errCode = hb_rddSelectWorkAreaSymbol(pAlias->item.asSymbol.value);
      pAlias->type = Harbour::Item::NIL;
      break;

    case Harbour::Item::STRING: {
      /* Alias was evaluated from an expression, for example: (cVar)->field
       */
      /* expand '&' operator if exists */
      HB_BOOL bNewString;

      char *szAlias = hb_macroExpandString(pAlias->item.asString.value, pAlias->item.asString.length, &bNewString);
      if (pField)
      {
        errCode = hb_rddSelectWorkAreaAlias(szAlias);
      }
      else
      {
        int iArea;
        hb_rddGetAliasNumber(szAlias, &iArea);
        hb_rddSelectWorkAreaNumber(iArea);
      }

      if (bNewString)
      {
        hb_xfree(szAlias);
      }
      hb_itemClear(pAlias);
      break;
    }

    default:
      if (pField)
      {
        hb_vmPushString(pField->szName, strlen(pField->szName));
        PHB_ITEM pSubstVal = hb_errRT_BASE_Subst(EG_ARG, 1065, nullptr, "&", 2, pAlias, hb_stackItemFromTop(-1));
        hb_stackPop();
        if (pSubstVal)
        {
          hb_itemMove(pAlias, pSubstVal);
          hb_itemRelease(pSubstVal);
          fRepeat = true;
        }
        else
        {
          hb_itemSetNil(pAlias);
          errCode = Harbour::FAILURE;
        }
      }
      else
      {
        hb_rddSelectWorkAreaNumber(-1);
        hb_itemSetNil(pAlias);
      }
      break;
    }
  } while (fRepeat);

  return errCode;
}

/* Swaps two last items on the eval stack - the last item after swapping
 * is popped as current workarea number
 */
static void hb_vmSwapAlias()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSwapAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackItemFromTop(-1);
  PHB_ITEM pWorkArea = hb_stackItemFromTop(-2);
  hb_vmSelectWorkarea(pWorkArea, nullptr);
  hb_itemMove(pWorkArea, pItem);
  hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void hb_vmProc(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProc(%hu)", uiParams));
#endif

  HB_STACK_STATE sStackState;

#ifndef HB_NO_PROFILER
  HB_ULONG ulClock = 0;
  bool bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

  HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
  if (bProfiler)
  {
    ulClock = static_cast<HB_ULONG>(clock());
  }
#endif

  /* Poll the console keyboard */
#if 0
#ifndef HB_GUI
   if( s_fKeyPool ) {
      hb_inkeyPoll();
   }
#endif
#endif

  PHB_SYMB pSym = hb_stackNewFrame(&sStackState, uiParams)->item.asSymbol.value;
  HB_VM_FUNCUNREF(pSym);
  if (HB_VM_ISFUNC(pSym))
  {
    HB_TRACE_PRG(("Calling: %s", pSym->szName));

#ifndef HB_NO_PROFILER
    if (bProfiler && pSym->pDynSym)
    {
      pSym->pDynSym->ulRecurse++;
    }
#endif

    HB_VM_EXECUTE(pSym);

#ifndef HB_NO_PROFILER
    if (bProfiler && pSym->pDynSym)
    {
      pSym->pDynSym->ulCalls++; /* profiler support */
      /* Time spent has to be added only inside topmost call of a recursive function */
      if (--pSym->pDynSym->ulRecurse == 0)
      {
        pSym->pDynSym->ulTime += clock() - ulClock; /* profiler support */
      }
    }
#endif
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_NOFUNC, 1001, nullptr, pSym->szName, HB_ERR_ARGS_BASEPARAMS);
  }

#ifndef HB_NO_DEBUG
  if (sStackState.fDebugging)
  {
    hb_vmDebuggerEndProc();
  }
#endif

  hb_stackOldFrame(&sStackState);
}

void hb_vmDo(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDo(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD

#ifndef HB_NO_PROFILER
  HB_ULONG ulClock = 0;
  bool bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

  HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
  if (bProfiler)
  {
    ulClock = static_cast<HB_ULONG>(clock());
  }
#endif

  /* Poll the console keyboard */
#if 0
#ifndef HB_GUI
   if( s_fKeyPool ) {
      hb_inkeyPoll();
   }
#endif
#endif

  HB_STACK_STATE sStackState;
  PHB_SYMB pSym = hb_stackNewFrame(&sStackState, uiParams)->item.asSymbol.value;
  auto pSelf = hb_stackSelfItem(); /* NIL, OBJECT or BLOCK */

  if (!pSelf->isNil())
  { /* are we sending a message ? */
    PHB_SYMB pExecSym = hb_objGetMethod(pSelf, pSym, &sStackState);
    if (pExecSym)
    {
      HB_VM_FUNCUNREF(pExecSym);
    }
    if (pExecSym && HB_VM_ISFUNC(pExecSym))
    {
      HB_TRACE_PRG(("Calling: %s:%s", hb_objGetClsName(pSelf), pSym->szName));

      HB_VM_EXECUTE(pExecSym);

#ifndef HB_NO_PROFILER
      if (bProfiler)
      {
        hb_mthAddTime(clock() - ulClock);
      }
#endif
    }
    else if (pSym->szName[0] == '_')
    {
      hb_errRT_BASE_SubstR(EG_NOVARMETHOD, 1005, nullptr, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS);
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_NOMETHOD, 1004, nullptr, pSym->szName, HB_ERR_ARGS_SELFPARAMS);
    }
  }
  else
  { /* it is a function */
    HB_VM_FUNCUNREF(pSym);
    if (HB_VM_ISFUNC(pSym))
    {
      HB_TRACE_PRG(("Calling: %s", pSym->szName));

#ifndef HB_NO_PROFILER
      if (bProfiler && pSym->pDynSym)
      {
        pSym->pDynSym->ulRecurse++;
      }
#endif

      HB_VM_EXECUTE(pSym);

#ifndef HB_NO_PROFILER
      if (bProfiler && pSym->pDynSym)
      {
        pSym->pDynSym->ulCalls++; /* profiler support */
        /* Time spent has to be added only inside topmost call of a recursive function */
        if (--pSym->pDynSym->ulRecurse == 0)
        {
          pSym->pDynSym->ulTime += clock() - ulClock; /* profiler support */
        }
      }
#endif
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_NOFUNC, 1001, nullptr, pSym->szName, HB_ERR_ARGS_BASEPARAMS);
    }
  }

#ifndef HB_NO_DEBUG
  if (sStackState.fDebugging)
  {
    hb_vmDebuggerEndProc();
  }
#endif

  hb_stackOldFrame(&sStackState);
}

void hb_vmSend(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSend(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD

#ifndef HB_NO_PROFILER
  HB_ULONG ulClock = 0;
  bool bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

  HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
  if (bProfiler)
  {
    ulClock = static_cast<HB_ULONG>(clock());
  }
#endif

  /* Poll the console keyboard */
#if 0
#if !defined(HB_GUI)
      hb_inkeyPoll();
#endif
#endif

  HB_STACK_STATE sStackState;
  PHB_SYMB pSym = hb_stackNewFrame(&sStackState, uiParams)->item.asSymbol.value;
  auto pSelf = hb_stackSelfItem(); /* NIL, OBJECT or BLOCK */

  PHB_SYMB pExecSym = hb_objGetMethod(pSelf, pSym, &sStackState);
  if (pExecSym)
  {
    HB_VM_FUNCUNREF(pExecSym);
  }
  if (pExecSym && HB_VM_ISFUNC(pExecSym))
  {
    HB_TRACE_PRG(("Calling: %s:%s", hb_objGetClsName(pSelf), pSym->szName));

    HB_VM_EXECUTE(pExecSym);

#ifndef HB_NO_PROFILER
    if (bProfiler)
    {
      hb_mthAddTime(clock() - ulClock);
    }
#endif
  }
  else if (pSym->szName[0] == '_')
  {
    hb_errRT_BASE_SubstR(EG_NOVARMETHOD, 1005, nullptr, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_NOMETHOD, 1004, nullptr, pSym->szName, HB_ERR_ARGS_SELFPARAMS);
  }

#ifndef HB_NO_DEBUG
  if (sStackState.fDebugging)
  {
    hb_vmDebuggerEndProc();
  }
#endif

  hb_stackOldFrame(&sStackState);
}

static void hb_vmPushObjectVarRef()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushObjectVarRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  HB_STACK_STATE sStackState;
  PHB_ITEM pItem = hb_stackNewFrame(&sStackState, 0); /* procedure name */
  PHB_SYMB pSym = pItem->item.asSymbol.value;
  if (!hb_objGetVarRef(hb_stackSelfItem(), pSym, &sStackState) && hb_vmRequestQuery() == 0)
  {
    hb_errRT_BASE_SubstR(EG_NOVARMETHOD, 1005, nullptr, pSym->szName + (pSym->szName[0] == '_' ? 1 : 0), 1,
                         hb_stackSelfItem());
  }
  hb_stackOldFrame(&sStackState);
  hb_stackPushReturn();
}

void hb_vmEval(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEval(%hu)", uiParams));
#endif

#ifndef HB_NO_PROFILER
  HB_ULONG ulClock = 0;
  bool bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

  HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
  if (bProfiler)
  {
    ulClock = static_cast<HB_ULONG>(clock());
  }
#endif

  HB_STACK_STATE sStackState;
  hb_stackNewFrame(&sStackState, uiParams);

  hb_vmDoBlock();

#ifndef HB_NO_PROFILER
  if (bProfiler)
  {
    hb_mthAddTime(clock() - ulClock);
  }
#endif

#ifndef HB_NO_DEBUG
  if (sStackState.fDebugging)
  {
    hb_vmDebuggerEndProc();
  }
#endif

  hb_stackOldFrame(&sStackState);
}

static HARBOUR hb_vmDoBlock()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoBlock()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pBlock = hb_stackSelfItem();
  if (!pBlock->isBlock())
  {
    hb_errInternal(HB_EI_VMNOTCBLOCK, nullptr, "hb_vmDoBlock()", nullptr);
  }

  auto pBase = hb_stackBaseItem();

  /* set number of declared parameters */
  pBase->item.asSymbol.paramdeclcnt = pBlock->item.asBlock.paramcnt;
  /* set the current line number to a line where the codeblock was defined */
  pBase->item.asSymbol.stackstate->uiLineNo = pBlock->item.asBlock.lineno;
  /* set execution context for OOP scope */
  pBase->item.asSymbol.stackstate->uiClass = pBlock->item.asBlock.hclass;
  pBase->item.asSymbol.stackstate->uiMethod = pBlock->item.asBlock.method;
  /* add missing parameters */
  int iParam = pBlock->item.asBlock.paramcnt - pBase->item.asSymbol.paramcnt;
  while (--iParam >= 0)
  {
    hb_stackAllocItem()->type = Harbour::Item::NIL;
  }
  /* set static base offset */
  hb_stackSetStaticsBase(pBlock->item.asBlock.value->pStatics);

  hb_vmExecute(pBlock->item.asBlock.value->pCode, pBlock->item.asBlock.value->pSymbols);
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
 */
PHB_ITEM hb_vmEvalBlock(PHB_ITEM pBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlock(%p)", static_cast<void*>(pBlock)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushEvalSym();
  hb_vmPush(pBlock);
  hb_vmSend(0);
  return hb_stackReturnItem();
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PHB_ITEM
 *
 * for example:
 *  retVal = hb_vmEvalBlockV(pBlock, 2, pParam1, pParam2);
 */
PHB_ITEM hb_vmEvalBlockV(PHB_ITEM pBlock, HB_ULONG ulArgCount, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlockV(%p, %lu, ...)", static_cast<void*>(pBlock), ulArgCount));
#endif

  HB_STACK_TLS_PRELOAD
  va_list va;

  hb_vmPushEvalSym();
  hb_vmPush(pBlock);

  va_start(va, ulArgCount);
  for (HB_ULONG i = 1; i <= ulArgCount; i++)
  {
    hb_vmPush(va_arg(va, PHB_ITEM));
  }
  va_end(va);

  /* take care here, possible loss of data long to short ... */
  /* added an explicit casting here for VC++ JFL */
  hb_vmSend(static_cast<HB_USHORT>(ulArgCount));

  return hb_stackReturnItem();
}

/* Evaluates a passed codeblock item or macro pointer item
 */
PHB_ITEM hb_vmEvalBlockOrMacro(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlockOrMacro(%p)", static_cast<void*>(pItem)));
#endif

  HB_STACK_TLS_PRELOAD

  if (pItem->isBlock())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pItem);
    hb_vmEval(0);
  }
  else
  {
    auto pMacro = static_cast<PHB_MACRO>(hb_itemGetPtr(pItem));
    if (pMacro)
    {
      hb_macroRun(pMacro);
      hb_stackPopReturn();
    }
    else
    {
      hb_itemSetNil(hb_stackReturnItem());
    }
  }
  return hb_stackReturnItem();
}

/*
 * destroy codeblock or macro in given item
 */
void hb_vmDestroyBlockOrMacro(PHB_ITEM pItem)
{
#if 0
  HB_TRACE(HB_TR_DEBUG, ("hb_vmDestroyBlockOrMacro(%p)", static_cast<void*>(pItem)));
#endif

  if (HB_IS_POINTER(pItem))
  {
    auto pMacro = static_cast<PHB_MACRO>(hb_itemGetPtr(pItem));
    if (pMacro)
    {
      hb_macroDelete(pMacro);
    }
  }
  hb_itemRelease(pItem);
}

/*
 * compile given expression and return macro pointer item or NULL
 */
PHB_ITEM hb_vmCompileMacro(const char *szExpr, PHB_ITEM pDest)
{
#if 0
  HB_TRACE(HB_TR_DEBUG, ("hb_vmCompileMacro(%s,%p)", szExpr, pDest));
#endif

  if (szExpr != nullptr)
  {
    PHB_MACRO pMacro = hb_macroCompile(szExpr);
    if (pMacro)
    {
      return hb_itemPutPtr(pDest, static_cast<void *>(pMacro));
    }
  }
  if (pDest)
  {
    hb_itemClear(pDest);
  }
  return nullptr;
}

void hb_vmFunction(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFunction(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemSetNil(hb_stackReturnItem());
  hb_vmDo(uiParams);
}

#ifndef HB_NO_DEBUG
static void hb_vmDebugEntry(int nMode, int nLine, const char *szName, int nIndex, PHB_ITEM pFrame)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebugEntry"));
#endif

  HB_STACK_TLS_PRELOAD

  switch (nMode)
  {
  case HB_DBG_MODULENAME:
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_MODULENAME);
    hb_vmPushString(szName, strlen(szName));
    hb_vmProc(2);
    break;

  case HB_DBG_LOCALNAME:
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_LOCALNAME);
    hb_vmPushInteger(nIndex);
    hb_vmPushString(szName, strlen(szName));
    hb_vmProc(3);
    break;

  case HB_DBG_STATICNAME:
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_STATICNAME);
    hb_vmPush(pFrame);        /* current static frame */
    hb_vmPushInteger(nIndex); /* variable index */
    hb_vmPushString(szName, strlen(szName));
    hb_vmProc(4);
    break;

  case HB_DBG_SHOWLINE:
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_SHOWLINE);
    hb_vmPushInteger(nLine);
    hb_vmProc(2);
    break;

  case HB_DBG_ENDPROC:
    hb_stackPushReturn(); /* saves the previous returned value */
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_ENDPROC);
    hb_vmProc(1);
    hb_stackPopReturn(); /* restores the previous returned value */
    break;

  case HB_DBG_GETENTRY:
    /* Try to get C dbgEntry() function pointer */
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_GETENTRY);
    hb_vmProc(1);
    break;

  case HB_DBG_VMQUIT:
    hb_vmPushDynSym(s_pDynsDbgEntry);
    HB_VM_PUSHNIL();
    hb_vmPushInteger(HB_DBG_VMQUIT);
    hb_vmPushInteger(nIndex);
    hb_vmProc(2);
    break;
  }
}

static void hb_vmDummyDebugEntry(int nMode, int nLine, const char *szName, int nIndex, PHB_ITEM pFrame)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDummyDebugEntry"));
#endif

  HB_SYMBOL_UNUSED(nMode);
  HB_SYMBOL_UNUSED(nLine);
  HB_SYMBOL_UNUSED(szName);
  HB_SYMBOL_UNUSED(nIndex);
  HB_SYMBOL_UNUSED(pFrame);
}

static void hb_vmDebuggerExit(HB_BOOL fRemove)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerExit(%d)", fRemove));
#endif

  /* is debugger linked ? */
  if (s_pFunDbgEntry)
  {
    /* inform debugger that we are quitting now */
    s_pFunDbgEntry(HB_DBG_VMQUIT, 0, nullptr, fRemove ? 1 : 0, nullptr);
    /* set dummy debugger function to avoid debugger activation in .prg
     *       destructors if any */
    if (fRemove)
    {
      s_pFunDbgEntry = hb_vmDummyDebugEntry;
    }
  }
}

static void hb_vmDebuggerEndProc()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerEndProc()"));
#endif

  s_pFunDbgEntry(HB_DBG_ENDPROC, 0, nullptr, 0, nullptr);
}

static void hb_vmDebuggerShowLine(HB_USHORT uiLine) /* makes the debugger shows a specific source code line */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerShowLine(%hu)", uiLine));
#endif

  s_pFunDbgEntry(HB_DBG_SHOWLINE, uiLine, nullptr, 0, nullptr);
}

static void hb_vmLocalName(
    HB_USHORT uiLocal, const char *szLocalName) /* locals and parameters index and name information for the debugger */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLocalName(%hu, %s)", uiLocal, szLocalName));
#endif

  HB_STACK_TLS_PRELOAD
  if (hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging)
  {
    s_pFunDbgEntry(HB_DBG_LOCALNAME, 0, szLocalName, uiLocal, nullptr);
  }
}

static void hb_vmStaticName(HB_BYTE bIsGlobal, HB_USHORT uiStatic,
                            const char *szStaticName) /* statics vars information for the debugger */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStaticName(%hu, %s)", uiStatic, szStaticName));
#endif

  HB_STACK_TLS_PRELOAD
  HB_SYMBOL_UNUSED(bIsGlobal);
  if (hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging)
  {
    s_pFunDbgEntry(HB_DBG_STATICNAME, 0, szStaticName, uiStatic, static_cast<PHB_ITEM>(hb_stackGetStaticsBase()));
  }
}

static void hb_vmModuleName(const char *szModuleName) /* PRG and function name information for the debugger */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModuleName(%s)", szModuleName));
#endif

  if (s_pFunDbgEntry)
  {
    HB_STACK_TLS_PRELOAD
    s_pFunDbgEntry(HB_DBG_MODULENAME, 0, szModuleName, 0, nullptr);
    hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging = true;
  }
}
#endif

static void hb_vmFrame(HB_USHORT usLocals, unsigned char ucParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFrame(%d, %d)", static_cast<int>(usLocals), static_cast<int>(ucParams)));
#endif

  HB_STACK_TLS_PRELOAD
  int iTotal;

  auto pBase = hb_stackBaseItem();

#if 0
   /* This old code which clears additional parameters to make space for
    * local variables without updating pBase->item.asSymbol.paramdeclcnt
    */
   iTotal = pBase->item.asSymbol.paramcnt - ucParams;
   if( iTotal > 0 ) {
      pBase->item.asSymbol.paramcnt = ucParams;
      do {
         hb_itemClear(hb_stackItemFromTop(-iTotal));
      } while( --iTotal > 0 );
   }

   iTotal = usLocals + ucParams;
   if( iTotal ) {
      iTotal -= pBase->item.asSymbol.paramcnt;
      while( --iTotal >= 0 ) {
         HB_VM_PUSHNIL();
      }
   }
#else
  pBase->item.asSymbol.paramdeclcnt = ucParams;

  iTotal = ucParams - pBase->item.asSymbol.paramcnt;
  if (iTotal < 0)
  {
    iTotal = 0;
  }
  iTotal += usLocals;

  while (--iTotal >= 0)
  {
    HB_VM_PUSHNIL();
  }
#endif
}

static void hb_vmVFrame(HB_USHORT usLocals, unsigned char ucParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmVFrame(%d, %d)", static_cast<int>(usLocals), static_cast<int>(ucParams)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pBase = hb_stackBaseItem();

  pBase->item.asSymbol.paramdeclcnt = ucParams;

  int iTotal = ucParams - pBase->item.asSymbol.paramcnt;
  if (iTotal < 0)
  {
    iTotal = 0;
  }
  iTotal += usLocals;

  while (--iTotal >= 0)
  {
    HB_VM_PUSHNIL();
  }
}

static void hb_vmSFrame(PHB_SYMB pSym) /* sets the statics frame for a function */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSFrame(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD

  /* _INITSTATICS is now the statics frame. Statics() changed it! */
  hb_stackSetStaticsBase(pSym->value.pStaticsBase);
}

static void hb_vmStatics(PHB_SYMB pSym,
                         HB_USHORT uiStatics) /* initializes the global aStatics array or redimensions it */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStatics(%p, %hu)", static_cast<void*>(pSym), uiStatics));
#endif

  /* statics frame for this PRG */
  pSym->value.pStaticsBase = static_cast<void *>(hb_itemArrayNew(uiStatics));
  pSym->scope.value |= HB_FS_FRAME;
}

#if defined(HB_MT_VM)
/*
 * extended thread static variable reference structure
 */
struct HB_TSVREF
{
  HB_ITEM source;
  HB_TSD threadData;
};

using PHB_TSVREF = HB_TSVREF *;

/*
 * extended thread static variable reference functions
 */
static PHB_ITEM hb_vmTSVRefRead(PHB_ITEM pRefer)
{
  auto pTSVRef = static_cast<PHB_TSVREF>(pRefer->item.asExtRef.value);
  auto pItem = static_cast<PHB_ITEM>(hb_stackTestTSD(&pTSVRef->threadData));

  if (!pItem)
  {
    pItem = static_cast<PHB_ITEM>(hb_stackGetTSD(&pTSVRef->threadData));
    hb_itemCloneTo(pItem, &pTSVRef->source);
  }
  return pItem;
}

static PHB_ITEM hb_vmTSVRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  auto pTSVRef = static_cast<PHB_TSVREF>(pRefer->item.asExtRef.value);
  HB_SYMBOL_UNUSED(pSource);
  return static_cast<PHB_ITEM>(hb_stackGetTSD(&pTSVRef->threadData));
}

static void hb_vmTSVRefCopy(PHB_ITEM pDest)
{
  hb_xRefInc(pDest->item.asExtRef.value);
}

static void hb_vmTSVRefClear(void *value)
{
  if (hb_xRefDec(value))
  {
    if (HB_IS_COMPLEX(&(static_cast<PHB_TSVREF>(value))->source))
    {
      hb_itemClear(&(static_cast<PHB_TSVREF>(value))->source);
    }

    hb_stackReleaseTSD(&(static_cast<PHB_TSVREF>(value))->threadData);
    hb_xfree(value);
  }
}

static void hb_vmTSVRefMark(void *value)
{
  if (HB_IS_GCITEM(&(static_cast<PHB_TSVREF>(value))->source))
  {
    hb_gcItemRef(&(static_cast<PHB_TSVREF>(value))->source);
  }

  auto pItem = static_cast<PHB_ITEM>(hb_stackTestTSD(&(static_cast<PHB_TSVREF>(value))->threadData));
  if (pItem && HB_IS_GCITEM(pItem))
  {
    hb_gcItemRef(pItem);
  }
}

/* destructor for terminated threads */
static void hb_vmTSVarClean(void *pThreadItem)
{
  if (HB_IS_COMPLEX(static_cast<PHB_ITEM>(pThreadItem)))
  {
    hb_itemClear(static_cast<PHB_ITEM>(pThreadItem));
  }
}

/*
 * create extended thread static variable reference
 */
static void hb_vmTSVReference(PHB_ITEM pStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmTSVReference(%p)", static_cast<void*>(pStatic)));
#endif

  static const HB_EXTREF s_TSVExtRef = {hb_vmTSVRefRead, hb_vmTSVRefWrite, hb_vmTSVRefCopy, hb_vmTSVRefClear,
                                        hb_vmTSVRefMark};

  HB_STACK_TLS_PRELOAD

  auto pTSVRef = static_cast<PHB_TSVREF>(hb_xgrab(sizeof(HB_TSVREF)));

  pTSVRef->source.type = Harbour::Item::NIL;
  HB_TSD_INIT(&pTSVRef->threadData, sizeof(HB_ITEM), nullptr, hb_vmTSVarClean);

  /* Use hb_stackReturnItem() as temporary item holder */
  auto pRefer = hb_stackReturnItem();
  if (HB_IS_COMPLEX(pRefer))
  {
    hb_itemClear(pRefer);
  }
  pRefer->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pRefer->item.asExtRef.value = static_cast<void *>(pTSVRef);
  pRefer->item.asExtRef.func = &s_TSVExtRef;

  hb_itemMove(&pTSVRef->source, pStatic);
  hb_itemMove(pStatic, pRefer);
}

static void hb_vmInitThreadStatics(HB_USHORT uiCount, const HB_BYTE *pCode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInitThreadStatics(%hu,%p)", uiCount, static_cast<const void*>(pCode)));
#endif

  HB_STACK_TLS_PRELOAD

  while (uiCount--)
  {
    HB_USHORT uiStatic = HB_PCODE_MKUSHORT(pCode);
    PHB_ITEM pStatic = (static_cast<PHB_ITEM>(hb_stackGetStaticsBase()))->item.asArray.value->pItems + uiStatic - 1;
    hb_vmTSVReference(pStatic);
    pCode += 2;
  }
}
#else
static void hb_vmInitThreadStatics(HB_USHORT uiCount, const HB_BYTE *pCode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInitThreadStatics(%hu,%p)", uiCount, static_cast<const void*>(pCode)));
#endif

  /* single thread VM - do nothing, use normal static variables */

  HB_SYMBOL_UNUSED(uiCount);
  HB_SYMBOL_UNUSED(pCode);
}
#endif /* HB_MT_VM */

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void hb_vmPush(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPush(%p)", static_cast<void*>(pItem)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemCopy(hb_stackAllocItem(), pItem);
}

void hb_vmPushNil(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNil()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackAllocItem()->type = Harbour::Item::NIL;
}

void hb_vmPushLogical(HB_BOOL bValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLogical(%d)", static_cast<int>(bValue)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::LOGICAL;
  pItem->item.asLogical.value = bValue;
}

/* not used by HVM code */
void hb_vmPushNumber(double dNumber, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNumber(%lf, %d)", dNumber, iDec));
#endif

  HB_STACK_TLS_PRELOAD

  if (iDec)
  {
    hb_vmPushDouble(dNumber, iDec);
  }
  else if (HB_DBL_LIM_INT(dNumber))
  {
    hb_vmPushInteger(static_cast<int>(dNumber));
  }
  else if (HB_DBL_LIM_LONG(dNumber))
  {
    hb_vmPushHBLong(static_cast<HB_MAXINT>(dNumber));
  }
  else
  {
    hb_vmPushDouble(dNumber, hb_stackSetStruct()->HB_SET_DECIMALS);
  }
}

static int hb_vmCalcIntWidth(HB_MAXINT nNumber)
{
  int iWidth;

  if (nNumber <= -1000000000L)
  {
    iWidth = 20;
  }
  else
  {
    iWidth = 10;
    while (nNumber >= 1000000000L)
    {
      iWidth++;
      nNumber /= 10;
    }
  }
  return iWidth;
}

void hb_vmPushInteger(int iNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushInteger(%d)", iNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::INTEGER;
  pItem->item.asInteger.value = iNumber;
  pItem->item.asInteger.length = HB_INT_LENGTH(iNumber);
}

#if HB_VMINT_MAX >= INT32_MAX
static void hb_vmPushIntegerConst(int iNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushIntegerConst(%d)", iNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::INTEGER;
  pItem->item.asInteger.value = iNumber;
  pItem->item.asInteger.length = static_cast<HB_USHORT>(hb_vmCalcIntWidth(iNumber));
}
#else
static void hb_vmPushLongConst(long lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongConst(%ld)", lNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::LONG;
  pItem->item.asLong.value = static_cast<HB_MAXINT>(lNumber);
  pItem->item.asLong.length = static_cast<HB_USHORT>(hb_vmCalcIntWidth(lNumber));
}
#endif

void hb_vmPushLong(long lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLong(%ld)", lNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  HB_ITEM_PUT_LONGRAW(pItem, lNumber);
}

void hb_vmPushSize(HB_ISIZ nNumber)
{
#if HB_SIZE_MAX <= HB_VMUINT_MAX
  hb_vmPushInteger(static_cast<int>(nNumber));
#else
  if (HB_LIM_INT(nNumber))
  {
    hb_vmPushInteger(static_cast<int>(nNumber));
  }
  else
  {
    hb_vmPushHBLong(nNumber);
  }
#endif
}

static void hb_vmPushHBLong(HB_MAXINT nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushHBLong(%" PFHL "d)", nNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::LONG;
  pItem->item.asLong.value = nNumber;
  pItem->item.asLong.length = HB_LONG_LENGTH(nNumber);
}

#if !defined(HB_LONG_LONG_OFF)
static void hb_vmPushLongLongConst(HB_LONGLONG llNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongLongConst(%" PFLL "d)", llNumber));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::LONG;
  pItem->item.asLong.value = static_cast<HB_MAXINT>(llNumber);
  pItem->item.asLong.length = static_cast<HB_USHORT>(hb_vmCalcIntWidth(llNumber));
}
#endif

void hb_vmPushNumInt(HB_MAXINT nNumber)
{
  if (HB_LIM_INT(nNumber))
  {
    hb_vmPushInteger(static_cast<int>(nNumber));
  }
  else
  {
    hb_vmPushHBLong(nNumber);
  }
}

void hb_vmPushDouble(double dNumber, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDouble(%lf, %d)", dNumber, iDec));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::DOUBLE;
  pItem->item.asDouble.value = dNumber;
  pItem->item.asDouble.length = HB_DBL_LENGTH(dNumber);
  if (iDec == HB_DEFAULT_DECIMALS)
  {
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(hb_stackSetStruct()->HB_SET_DECIMALS);
  }
  else
  {
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(iDec);
  }
}

static void hb_vmPushDoubleConst(double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::DOUBLE;
  pItem->item.asDouble.value = dNumber;

  if (iDec == HB_DEFAULT_DECIMALS)
  {
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(hb_stackSetStruct()->HB_SET_DECIMALS);
  }
  else
  {
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(iDec);
  }

  if (iWidth == HB_DEFAULT_WIDTH)
  {
    pItem->item.asDouble.length = HB_DBL_LENGTH(dNumber);
  }
  else
  {
    pItem->item.asDouble.length = static_cast<HB_USHORT>(iWidth);
  }
}

void hb_vmPushDate(long lDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDate(%ld)", lDate));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::DATE;
  pItem->item.asDateTime.julian = lDate;
  pItem->item.asDateTime.time = 0;
}

void hb_vmPushTimeStamp(long lJulian, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushTimeStamp(%ld, %ld)", lJulian, lMilliSec));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::TIMESTAMP;
  pItem->item.asDateTime.julian = lJulian;
  pItem->item.asDateTime.time = lMilliSec;
}

void hb_vmPushPointer(void *pPointer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushPointer(%p)", static_cast<void*>(pPointer)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::POINTER;
  pItem->item.asPointer.value = pPointer;
  pItem->item.asPointer.collect = pItem->item.asPointer.single = false;
}

void hb_vmPushPointerGC(void *pPointer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushPointerGC(%p)", static_cast<void*>(pPointer)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::POINTER;
  pItem->item.asPointer.value = pPointer;
  pItem->item.asPointer.collect = true;
  pItem->item.asPointer.single = false;
  hb_gcAttach(pPointer);
}

void hb_vmPushString(const char *szText, HB_SIZE nLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushString(%s, %" HB_PFS "u)", szText, nLength));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemPutCL(hb_stackAllocItem(), szText, nLength);
}

void hb_vmPushStringPcode(const char *szText, HB_SIZE nLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStringPcode(%s, %" HB_PFS "u)", szText, nLength));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::STRING;
  pItem->item.asString.allocated = 0;
  pItem->item.asString.length = nLength;
  pItem->item.asString.value =
      const_cast<char *>((nLength <= 1 ? hb_szAscii[static_cast<unsigned char>(szText[0])] : szText));
}

void hb_vmPushSymbol(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushSymbol(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::SYMBOL;
  pItem->item.asSymbol.value = pSym;
  pItem->item.asSymbol.stackstate = nullptr;
}

void hb_vmPushDynSym(PHB_DYNS pDynSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDynSym(%p)", static_cast<void*>(pDynSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::SYMBOL;
  pItem->item.asSymbol.value = pDynSym->pSymbol;
  pItem->item.asSymbol.stackstate = nullptr;
}

void hb_vmPushEvalSym(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushEvalSym()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::SYMBOL;
  pItem->item.asSymbol.value = &hb_symEval;
  pItem->item.asSymbol.stackstate = nullptr;
}

/* -3    -> HB_P_PUSHBLOCK
 * -2 -1 -> size of codeblock
 *  0 +1 -> number of expected parameters
 * +2 +3 -> number of referenced local variables
 * +4    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlock(const HB_BYTE *pCode, PHB_SYMB pSymbols, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlock(%p,%p,%" HB_PFS "u)", static_cast<const void*>(pCode), static_cast<void*>(pSymbols), nLen));
#endif

  HB_STACK_TLS_PRELOAD
  HB_USHORT uiLocals = HB_PCODE_MKUSHORT(&pCode[2]);

  if (nLen)
  {
    nLen -= uiLocals << 1;
  }

  auto pItem = hb_stackAllocItem();

  pItem->item.asBlock.value = hb_codeblockNew(pCode + 4 + (uiLocals << 1), /* pcode buffer         */
                                              uiLocals,                    /* number of referenced local variables */
                                              pCode + 4,                   /* table with referenced local variables */
                                              pSymbols, nLen);

  pItem->type = Harbour::Item::BLOCK;
  /* store the number of expected parameters
   */
  pItem->item.asBlock.paramcnt = HB_PCODE_MKUSHORT(pCode);
  /* store the line number where the codeblock was defined
   */
  pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
  pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
  pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -2    -> HB_P_PUSHBLOCKSHORT
 * -1    -> size of codeblock
 *  0    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlockShort(const HB_BYTE *pCode, PHB_SYMB pSymbols, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlockShort(%p,%p,%" HB_PFS "u)", static_cast<const void*>(pCode), static_cast<void*>(pSymbols), nLen));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();

  pItem->item.asBlock.value = hb_codeblockNew(pCode,   /* pcode buffer         */
                                              0,       /* number of referenced local variables */
                                              nullptr, /* table with referenced local variables */
                                              pSymbols, nLen);

  pItem->type = Harbour::Item::BLOCK;

  /* store the number of expected parameters
   */
  pItem->item.asBlock.paramcnt = 0;
  /* store the line number where the codeblock was defined
   */
  pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
  pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
  pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -(5|6)     -> HB_P_MPUSHBLOCK[LARGE]
 * [-5] -4 -3 -> size of codeblock
 * -2 -1      -> number of expected parameters
 * +0         -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void hb_vmPushMacroBlock(const HB_BYTE *pCode, HB_SIZE nSize, HB_USHORT usParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushMacroBlock(%p,%" HB_PFS "u,%hu)", static_cast<const void*>(pCode), nSize, usParams));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->item.asBlock.value = hb_codeblockMacroNew(pCode, nSize);
  pItem->type = Harbour::Item::BLOCK;
  /* store the number of expected parameters
   */
  pItem->item.asBlock.paramcnt = usParams;
  /* store the line number where the codeblock was defined
   */
  pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
  pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
  pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::INTEGER;
  pItem->item.asInteger.value = hb_rddGetCurrentWorkAreaNumber();
  pItem->item.asInteger.length = 10;
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void hb_vmPushAliasedField(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedField(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  auto pAlias = hb_stackItemFromTop(-1);

  /*
   * NOTE: hb_vmSelectWorkarea() clears passed item
   */
  if (hb_vmSelectWorkarea(pAlias, pSym) == Harbour::SUCCESS)
  {
    hb_rddGetFieldValue(pAlias, pSym);
  }

  hb_rddSelectWorkAreaNumber(iCurrArea);
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of either a field or a memvar based on alias value
 * (for performance reason it replaces alias value with field value)
 * This is used in the following context:
 * (any_alias)->variable
 */
static void hb_vmPushAliasedVar(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedVar(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pAlias = hb_stackItemFromTop(-1);

  if (HB_IS_STRING(pAlias))
  {
    const char *szAlias = pAlias->item.asString.value;

    if (szAlias[0] == 'M' || szAlias[0] == 'm')
    {
      if (pAlias->item.asString.length == 1 ||                                 /* M->variable */
          (pAlias->item.asString.length >= 4 && hb_strnicmp(szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                                            pAlias->item.asString.length) == 0))
      {
        hb_memvarGetValue(pAlias, pSym);
        return;
      }
    }
    else if (pAlias->item.asString.length >= 4 && (hb_strnicmp(szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                                               pAlias->item.asString.length) == 0 ||
                                                   hb_strnicmp(szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                                               pAlias->item.asString.length) == 0))
    {
      hb_rddGetFieldValue(pAlias, pSym);
      return;
    }
  }
  hb_vmPushAliasedField(pSym);
}

static void hb_vmPushLocal(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocal(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal;

  if (iLocal >= 0)
  {
    /* local variable or local parameter */
    pLocal = hb_stackLocalVariable(iLocal);
  }
  else
  {
    /* local variable referenced in a codeblock
     * hb_stackSelfItem() points to a codeblock that is currently evaluated
     */
    pLocal = hb_codeblockGetRef(hb_stackSelfItem()->item.asBlock.value, iLocal);
  }

  hb_itemCopy(hb_stackAllocItem(), HB_IS_BYREF(pLocal) ? hb_itemUnRef(pLocal) : pLocal);
}

static void hb_vmPushLocalByRef(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocalByRef(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD

  auto pTop = hb_stackAllocItem();
  /* we store its stack offset instead of a pointer to support a dynamic stack */
  if (iLocal >= 0)
  {
    PHB_ITEM pLocal = hb_stackLocalVariableAt(&iLocal);
    if (HB_IS_BYREF(pLocal) && !HB_IS_ENUM(pLocal))
    {
      hb_itemCopy(pTop, pLocal);
      return;
    }
    pTop->item.asRefer.BasePtr.itemsbasePtr = hb_stackItemBasePtr();
  }
  else
  {
    /* store direct codeblock address because an item where a codeblock
     * is stored can be no longer placed on the eval stack at the time
     * of a codeblock evaluation or variable access
     */
    pTop->item.asRefer.BasePtr.block = hb_stackSelfItem()->item.asBlock.value;
  }
  pTop->type = Harbour::Item::BYREF;
  pTop->item.asRefer.value = iLocal;
  pTop->item.asRefer.offset = hb_stackBaseOffset();
}

static void hb_vmPushStatic(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStatic(%hu)", uiStatic));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pStatic = (static_cast<PHB_ITEM>(hb_stackGetStaticsBase()))->item.asArray.value->pItems + uiStatic - 1;
  hb_itemCopy(hb_stackAllocItem(), HB_IS_BYREF(pStatic) ? hb_itemUnRef(pStatic) : pStatic);
}

static void hb_vmPushStaticByRef(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStaticByRef(%hu)", uiStatic));
#endif

  HB_STACK_TLS_PRELOAD

  auto pTop = hb_stackAllocItem();
  auto pBase = static_cast<PHB_ITEM>(hb_stackGetStaticsBase());

  if (HB_IS_BYREF(pBase->item.asArray.value->pItems + uiStatic - 1) &&
      !HB_IS_ENUM(pBase->item.asArray.value->pItems + uiStatic - 1))
  {
    hb_itemCopy(pTop, pBase->item.asArray.value->pItems + uiStatic - 1);
    return;
  }
  pTop->type = Harbour::Item::BYREF;
  /* we store the offset instead of a pointer to support a dynamic stack */
  pTop->item.asRefer.value = uiStatic - 1;
  pTop->item.asRefer.offset = 0; /* 0 for static variables */
  pTop->item.asRefer.BasePtr.array = pBase->item.asArray.value;
  hb_gcRefInc(pBase->item.asArray.value);
}

static void hb_vmPushVariable(PHB_SYMB pVarSymb)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("(hb_vmPushVariable)"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackAllocItem();

  /* First try if passed symbol is a name of field
   * in a current workarea - if it is not a field (Harbour::FAILURE)
   * then try the memvar variable
   */
  if (hb_rddFieldGet(pItem, pVarSymb) != Harbour::SUCCESS && hb_memvarGet(pItem, pVarSymb) != Harbour::SUCCESS)
  {

    auto pError = hb_errRT_New(ES_ERROR, nullptr, EG_NOVAR, 1003, nullptr, pVarSymb->szName, 0, EF_CANRETRY);
    hb_itemClear(pItem);

    while (hb_errLaunch(pError) == E_RETRY)
    {
      if (hb_rddFieldGet(pItem, pVarSymb) == Harbour::SUCCESS || hb_memvarGet(pItem, pVarSymb) == Harbour::SUCCESS)
      {
        break;
      }
    }

    hb_errRelease(pError);
  }
}

static void hb_vmDuplicate()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplicate()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackItemFromTop(-1);
  hb_itemCopy(hb_stackAllocItem(), pItem);
}

static void hb_vmDuplUnRef()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplUnRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackItemFromTop(-1);
  hb_itemCopy(hb_stackAllocItem(), pItem);
  if (HB_IS_BYREF(pItem))
  {
    hb_itemCopy(pItem, hb_itemUnRef(pItem));
  }
}

static void hb_vmPushUnRef()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushUnRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackItemFromTop(-1);
  hb_itemCopy(hb_stackAllocItem(), HB_IS_BYREF(pItem) ? hb_itemUnRef(pItem) : pItem);
}

static void hb_vmSwap(int iCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSwap(%d)", iCount));
#endif

  HB_STACK_TLS_PRELOAD
  int i = -1;

  do
  {
    hb_itemSwap(hb_stackItemFromTop(i), hb_stackItemFromTop(i - 1));
    --i;
  } while (iCount--);
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

static HB_BOOL hb_vmPopLogical()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLogical()"));
#endif

  HB_STACK_TLS_PRELOAD

  if (hb_stackItemFromTop(-1)->isLogical())
  {
    bool fValue = hb_stackItemFromTop(-1)->item.asLogical.value;
    hb_stackDec();
    return fValue;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 1066, nullptr, hb_langDGetErrorDesc(EG_CONDITION), 1, hb_stackItemFromTop(-1));
    return false;
  }
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmSelectWorkarea(hb_stackItemFromTop(-1), nullptr); /* it clears the passed item */
  hb_stackDec();
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into a given field
 */
static void hb_vmPopAliasedField(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedField(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD

  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  if (hb_vmSelectWorkarea(hb_stackItemFromTop(-1), pSym) == Harbour::SUCCESS)
  {
    hb_rddPutFieldValue(hb_stackItemFromTop(-2), pSym);
  }

  hb_rddSelectWorkAreaNumber(iCurrArea);
  hb_stackDec(); /* alias - it was cleared in hb_vmSelectWorkarea() */
  hb_stackPop(); /* field value */
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into either a field or a memvar based on the alias value
 * This is used in the following context:
 * (any_alias)->variable
 */
static void hb_vmPopAliasedVar(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedVar(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pAlias = hb_stackItemFromTop(-1);

  /*
   * "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
   */
  if (HB_IS_STRING(pAlias))
  {
    const char *szAlias = pAlias->item.asString.value;

    if (szAlias[0] == 'M' || szAlias[0] == 'm')
    {
      if (pAlias->item.asString.length == 1 ||                                 /* M->variable */
          (pAlias->item.asString.length >= 4 && hb_strnicmp(szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                                            pAlias->item.asString.length) == 0))
      {
        hb_memvarSetValue(pSym, hb_stackItemFromTop(-2));
        hb_stackPop(); /* alias */
        hb_stackPop(); /* value */
        return;
      }
    }
    else if (pAlias->item.asString.length >= 4 && (hb_strnicmp(szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                                               pAlias->item.asString.length) == 0 ||
                                                   hb_strnicmp(szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                                               pAlias->item.asString.length) == 0))
    {
      hb_rddPutFieldValue(hb_stackItemFromTop(-2), pSym);
      hb_stackPop(); /* alias */
      hb_stackPop(); /* value */
      return;
    }
  }
  hb_vmPopAliasedField(pSym);
}

static void hb_vmPopLocal(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLocal(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD

  auto pVal = hb_stackItemFromTop(-1);

  /* Remove MEMOFLAG if exists (assignment from field). */
  pVal->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);

  PHB_ITEM pLocal;

  if (iLocal >= 0)
  {
    /* local variable or local parameter */
    pLocal = hb_stackLocalVariable(iLocal);
  }
  else
  {
    /* local variable referenced in a codeblock
     * hb_stackSelfItem() points to a codeblock that is currently evaluated
     */
    pLocal = hb_codeblockGetRef(hb_stackSelfItem()->item.asBlock.value, iLocal);
  }

  hb_itemMoveToRef(pLocal, pVal);
  hb_stackDec();
}

static void hb_vmPopStatic(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopStatic(%hu)", uiStatic));
#endif

  HB_STACK_TLS_PRELOAD
  auto pVal = hb_stackItemFromTop(-1);
  /* Remove MEMOFLAG if exists (assignment from field). */
  pVal->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
  PHB_ITEM pStatic = (static_cast<PHB_ITEM>(hb_stackGetStaticsBase()))->item.asArray.value->pItems + uiStatic - 1;
  hb_itemMoveToRef(pStatic, pVal);
  hb_stackDec();
}

/* ------------------------------- */
/*
 * Functions to manage module symbols
 */

PHB_SYMB hb_vmGetRealFuncSym(PHB_SYMB pSym)
{
  if (pSym && !(pSym->scope.value & HB_FS_LOCAL))
  {
    pSym = pSym->pDynSym && ((pSym->pDynSym->pSymbol->scope.value & HB_FS_LOCAL) ||
                             pSym->pDynSym->pSymbol->value.pFunPtr == pSym->value.pFunPtr)
               ? pSym->pDynSym->pSymbol
               : nullptr;
  }

  return pSym;
}

HB_BOOL hb_vmLockModuleSymbols(void)
{
#if defined(HB_MT_VM)
  return !s_pSymbolsMtx || hb_threadMutexLock(s_pSymbolsMtx);
#else
  return true;
#endif /* HB_MT_VM */
}

void hb_vmUnlockModuleSymbols(void)
{
#if defined(HB_MT_VM)
  if (s_pSymbolsMtx)
  {
    hb_threadMutexUnlock(s_pSymbolsMtx);
  }
#endif /* HB_MT_VM */
}

const char *hb_vmFindModuleSymbolName(PHB_SYMB pSym)
{
  if (pSym)
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;

    while (pLastSymbols)
    {
      if (pSym >= pLastSymbols->pModuleSymbols && pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols)
      {
        return pLastSymbols->szModuleName;
      }
      pLastSymbols = pLastSymbols->pNext;
    }
  }
  return nullptr;
}

HB_BOOL hb_vmFindModuleSymbols(PHB_SYMB pSym, PHB_SYMB *pSymbols, HB_USHORT *puiSymbols)
{
  if (pSym)
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;

#if 0
      if( pSym->scope.value & HB_FS_PCODEFUNC ) {
         * pSymbols = pSym->value.pCodeFunc->pSymbols;
      }
#endif

    while (pLastSymbols)
    {
      if (pLastSymbols->fActive && pSym >= pLastSymbols->pModuleSymbols &&
          pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols)
      {
        *pSymbols = pLastSymbols->pModuleSymbols;
        *puiSymbols = pLastSymbols->uiModuleSymbols;
        return true;
      }
      pLastSymbols = pLastSymbols->pNext;
    }
  }

  *pSymbols = nullptr;
  *puiSymbols = 0;
  return false;
}

PHB_SYMB hb_vmFindFuncSym(const char *szFuncName, void *hDynLib)
{
  static PHB_SYMB pFuncSym = nullptr;

  if (szFuncName != nullptr)
  {
    PHB_SYMBOLS pSymbols = s_pSymbols;

    while (pSymbols)
    {
      if (pSymbols->fActive && pSymbols->hDynLib == hDynLib)
      {
        for (HB_USHORT ui = 0; ui < pSymbols->uiModuleSymbols; ++ui)
        {
          PHB_SYMB pSymbol = &pSymbols->pModuleSymbols[ui];

          if ((pSymbol->scope.value & HB_FS_LOCAL) != 0 && hb_stricmp(pSymbol->szName, szFuncName) == 0)
          {
            if ((pSymbol->scope.value & HB_FS_STATIC) == 0)
            {
              return pSymbol;
            }
            else if (!pFuncSym)
            {
              pFuncSym = pSymbol;
            }
          }
        }
      }
      pSymbols = pSymbols->pNext;
    }
  }

  return pFuncSym;
}

#define HB_SYM_STATICSBASE(p)                                                                                          \
  (static_cast<PHB_ITEM>(((p)->scope.value & HB_FS_FRAME) ? (p)->value.pStaticsBase : nullptr))

static void hb_vmStaticsClear()
{
  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  while (pLastSymbols)
  {
    if (pLastSymbols->uiStaticsOffset)
    {
      PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
      PHB_ITEM pStatics = HB_SYM_STATICSBASE(pSym);
      if (pStatics)
      {
        HB_SIZE nLen = hb_arrayLen(pStatics);

        for (HB_SIZE ul = 1; ul <= nLen; ++ul)
        {
          auto pItem = hb_arrayGetItemPtr(pStatics, ul);
          if (pItem && HB_IS_COMPLEX(pItem))
          {
            hb_itemClear(pItem);
          }
        }
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

static void hb_vmStaticsRelease()
{
  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  while (pLastSymbols)
  {
    if (pLastSymbols->uiStaticsOffset)
    {
      PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
      PHB_ITEM pStatics = HB_SYM_STATICSBASE(pSym);
      if (pStatics)
      {
        hb_itemRelease(pStatics);
        pSym->value.pStaticsBase = nullptr;
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

static HB_SIZE hb_vmStaticsCount()
{
  HB_SIZE nStatics = 0;

  if (hb_vmLockModuleSymbols())
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;
    while (pLastSymbols)
    {
      if (pLastSymbols->uiStaticsOffset)
      {
        PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
        PHB_ITEM pStatics = HB_SYM_STATICSBASE(pSym);
        if (pStatics)
        {
          nStatics += hb_arrayLen(pStatics);
        }
      }
      pLastSymbols = pLastSymbols->pNext;
    }
    hb_vmUnlockModuleSymbols();
  }

  return nStatics;
}

static PHB_ITEM hb_vmStaticsArray()
{
  PHB_ITEM pArray = nullptr;

  if (hb_vmLockModuleSymbols())
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;
    HB_SIZE nCount = hb_vmStaticsCount();
    pArray = hb_itemArrayNew(nCount);
    HB_SIZE nOffset = 0;
    while (pLastSymbols)
    {
      if (pLastSymbols->uiStaticsOffset)
      {
        PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
        PHB_ITEM pStatics = HB_SYM_STATICSBASE(pSym);
        if (pStatics)
        {
          HB_SIZE nLen = hb_arrayLen(pStatics);
          for (HB_SIZE n = 1; n <= nLen; ++n)
          {
            hb_arraySet(pArray, ++nOffset, hb_arrayGetItemPtr(pStatics, n));
          }
        }
      }
      pLastSymbols = pLastSymbols->pNext;
    }
    hb_vmUnlockModuleSymbols();
  }

  return pArray;
}

static PHB_SYMBOLS hb_vmFindFreeModule(PHB_SYMB pSymbols, HB_USHORT uiSymbols, const char *szModuleName, HB_ULONG ulID)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFindFreeModule(%p,%hu,%s,%lu)", static_cast<void*>(pSymbols), uiSymbols, szModuleName, ulID));
#endif

  if (s_ulFreeSymbols)
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;

    while (pLastSymbols)
    {
      if (!pLastSymbols->fActive && pLastSymbols->ulID == ulID && pLastSymbols->uiModuleSymbols == uiSymbols &&
          pLastSymbols->szModuleName != nullptr && strcmp(pLastSymbols->szModuleName, szModuleName) == 0)
      {
        PHB_SYMB pModuleSymbols = pLastSymbols->pModuleSymbols;
        HB_USHORT ui;

        for (ui = 0; ui < uiSymbols; ++ui)
        {
          if (((pSymbols[ui].scope.value & ~(HB_FS_PCODEFUNC | HB_FS_DYNCODE | HB_FS_DEFERRED)) !=
                   (pModuleSymbols[ui].scope.value & ~HB_FS_DEFERRED) &&
               !(ui != 0 && ui == pLastSymbols->uiStaticsOffset && HB_SYM_STATICSBASE(&pModuleSymbols[ui]))) ||
              strcmp(pSymbols[ui].szName, pModuleSymbols[ui].szName) != 0)
          {
            break;
          }
        }
        if (ui == uiSymbols)
        {
          --s_ulFreeSymbols;
          return pLastSymbols;
        }
      }
      pLastSymbols = pLastSymbols->pNext;
    }
  }

  return nullptr;
}

void hb_vmFreeSymbols(PHB_SYMBOLS pSymbols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFreeSymbols(%p)", static_cast<void*>(pSymbols)));
#endif

  if (pSymbols->fActive && hb_vmLockModuleSymbols())
  {
    if (pSymbols->fActive)
    {
      for (HB_USHORT ui = 0; ui < pSymbols->uiModuleSymbols; ++ui)
      {
        PHB_SYMB pSymbol = &pSymbols->pModuleSymbols[ui];

        /* do not overwrite already initialized statics' frame */
        if (ui == 0 || ui != pSymbols->uiStaticsOffset || !HB_SYM_STATICSBASE(pSymbol))
        {
          pSymbol->value.pFunPtr = nullptr;
          if (pSymbol->pDynSym && pSymbol->pDynSym->pSymbol != pSymbol && (pSymbol->scope.value & HB_FS_LOCAL) == 0)
          {
            pSymbol->scope.value |= HB_FS_DEFERRED;
          }
          pSymbol->scope.value &= ~(HB_FS_PCODEFUNC | HB_FS_DYNCODE);
        }
      }
      pSymbols->hDynLib = nullptr;
      pSymbols->fActive = false;
      ++s_ulFreeSymbols;
    }
    hb_vmUnlockModuleSymbols();
  }
}

void hb_vmBeginSymbolGroup(void *hDynLib, HB_BOOL fClone)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmBeginSymbolGroup(%p,%d)", hDynLib, static_cast<int>(fClone)));
#endif

  s_hDynLibID = hDynLib;
  s_fCloneSym = fClone;
}

void hb_vmInitSymbolGroup(void *hNewDynLib, int argc, const char *argv[])
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInitSymbolGroup(%p,%d,%p)", hNewDynLib, argc, static_cast<const void*>(argv)));
#endif

  s_fCloneSym = false;

  if (s_hDynLibID)
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;
    void *hDynLib = s_hDynLibID;
    auto fFound = false;
    HB_USHORT ui;

    s_hDynLibID = nullptr;

    while (pLastSymbols)
    {
      if (pLastSymbols->hDynLib == hDynLib)
      {
        fFound = true;

        if (pLastSymbols->fInitStatics && pLastSymbols->fActive)
        {
          for (ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++)
          {
            HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

            if (scope == HB_FS_INITEXIT && !(ui != 0 && ui == pLastSymbols->uiStaticsOffset &&
                                             HB_SYM_STATICSBASE(pLastSymbols->pModuleSymbols + ui)))
            {
              hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
              hb_vmPushNil();
              hb_vmProc(0);
            }
          }
          pLastSymbols->fInitStatics = false;
        }

        pLastSymbols->hDynLib = hNewDynLib;
      }
      pLastSymbols = pLastSymbols->pNext;
    }

    /* library symbols are modified beforeinit functions
       execution intentionally because init functions may
       load new modules [druzus] */
    hb_vmDoModuleSetLibID(s_InitFunctions, hDynLib, hNewDynLib);
    hb_vmDoModuleSetLibID(s_ExitFunctions, hDynLib, hNewDynLib);
    hb_vmDoModuleSetLibID(s_QuitFunctions, hDynLib, hNewDynLib);
    hb_vmDoModuleLibFunctions(&s_InitFunctions, hNewDynLib);

    if (fFound)
    {
      auto fClipInit = true;

      do
      {
        pLastSymbols = s_pSymbols;
        while (pLastSymbols && hb_vmRequestQuery() == 0)
        {
          if (pLastSymbols->hDynLib == hNewDynLib)
          {
            if (pLastSymbols->fActive && (pLastSymbols->hScope & HB_FS_INIT) != 0)
            {
              ui = pLastSymbols->uiModuleSymbols;
              while (ui--)
              {
                HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

                if (scope == HB_FS_INIT &&
                    (strcmp((pLastSymbols->pModuleSymbols + ui)->szName, "CLIPINIT$") == 0 ? fClipInit : !fClipInit))
                {
                  hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
                  hb_vmPushNil();
                  for (auto i = 0; i < argc; ++i)
                  {
                    hb_vmPushString(argv[i], strlen(argv[i]));
                  }
                  hb_vmProc(static_cast<HB_USHORT>(argc));
                  if (hb_vmRequestQuery() != 0)
                  {
                    break;
                  }
                }
              }
            }
          }
          pLastSymbols = pLastSymbols->pNext;
        }
        fClipInit = !fClipInit;
      } while (!fClipInit);
    }
  }
}

void hb_vmExitSymbolGroup(void *hDynLib)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmExitSymbolGroup(%p)", hDynLib));
#endif

  if (hDynLib)
  {
    PHB_SYMBOLS pLastSymbols = s_pSymbols;
    auto fFound = false;

    while (pLastSymbols)
    {
      if (pLastSymbols->hDynLib == hDynLib)
      {
        fFound = true;
        if (pLastSymbols->fActive && (pLastSymbols->hScope & HB_FS_EXIT) != 0)
        {
          for (HB_USHORT ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++)
          {
            HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

            if (scope == HB_FS_EXIT)
            {
              hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
              hb_vmPushNil();
              hb_vmProc(0);
            }
          }
        }
      }
      pLastSymbols = pLastSymbols->pNext;
    }

    hb_vmDoModuleLibFunctions(&s_ExitFunctions, hDynLib);
    hb_vmDoModuleLibFunctions(&s_QuitFunctions, hDynLib);

    if (fFound)
    {
      pLastSymbols = s_pSymbols;
      while (pLastSymbols)
      {
        if (pLastSymbols->hDynLib == hDynLib)
        {
          hb_vmFreeSymbols(pLastSymbols);
        }
        pLastSymbols = pLastSymbols->pNext;
      }
    }
  }
}

PHB_SYMBOLS hb_vmRegisterSymbols(PHB_SYMB pModuleSymbols, HB_USHORT uiSymbols, const char *szModuleName, HB_ULONG ulID,
                                 HB_BOOL fDynLib, HB_BOOL fClone, HB_BOOL fOverLoad)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRegisterSymbols(%p,%hu,%s,%lu,%d,%d,%d)", static_cast<void*>(pModuleSymbols), uiSymbols, szModuleName, ulID, static_cast<int>(fDynLib), static_cast<int>(fClone), static_cast<int>(fOverLoad)));
#endif

  auto fRecycled = false;
  auto fInitStatics = false;
  HB_USHORT ui;

  PHB_SYMBOLS pNewSymbols =
      s_ulFreeSymbols == 0 ? nullptr : hb_vmFindFreeModule(pModuleSymbols, uiSymbols, szModuleName, ulID);

  if (pNewSymbols)
  {
    pNewSymbols->fActive = fRecycled = true;
    pNewSymbols->hDynLib = s_hDynLibID;
    pNewSymbols->hScope = 0;
  }
  else
  {
    fRecycled = false;

    if (fClone)
    {
      HB_SIZE nSymSize = uiSymbols * sizeof(HB_SYMB);
      HB_SIZE nSize = nSymSize;
      for (ui = 0; ui < uiSymbols; ui++)
      {
        nSize += strlen(pModuleSymbols[ui].szName) + 1;
      }
      auto buffer = static_cast<char *>(memcpy(hb_xgrab(nSize), pModuleSymbols, nSymSize));
      pModuleSymbols = reinterpret_cast<PHB_SYMB>(buffer);
      for (ui = 0; ui < uiSymbols; ui++)
      {
        buffer += nSymSize;
        nSymSize = strlen(pModuleSymbols[ui].szName) + 1;
        memcpy(buffer, pModuleSymbols[ui].szName, nSymSize);
        pModuleSymbols[ui].szName = buffer;
      }
    }

    pNewSymbols = static_cast<PHB_SYMBOLS>(hb_xgrab(sizeof(HB_SYMBOLS)));
    pNewSymbols->pModuleSymbols = pModuleSymbols;
    pNewSymbols->uiModuleSymbols = uiSymbols;
    pNewSymbols->uiStaticsOffset = 0;
    pNewSymbols->szModuleName = hb_strdup(szModuleName);
    pNewSymbols->ulID = ulID;
    pNewSymbols->fAllocated = fClone;
    pNewSymbols->fActive = true;
    pNewSymbols->fInitStatics = false;
    pNewSymbols->hDynLib = s_hDynLibID;
    pNewSymbols->hScope = 0;
    pNewSymbols->pNext = nullptr;

    if (s_pSymbols == nullptr)
    {
      s_pSymbols = pNewSymbols;
    }
    else
    {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;
      while (pLastSymbols->pNext)
      { /* locates the latest processed group of symbols */
        pLastSymbols = pLastSymbols->pNext;
      }
      pLastSymbols->pNext = pNewSymbols;
    }
  }

  for (ui = 0; ui < uiSymbols; ui++)
  { /* register each public symbol on the dynamic symbol table */
    PHB_SYMB pSymbol = pNewSymbols->pModuleSymbols + ui;

    bool fStatics = (pSymbol->scope.value & HB_FS_INITEXIT) == HB_FS_INITEXIT ||
                    (fRecycled && ui != 0 && ui == pNewSymbols->uiStaticsOffset && HB_SYM_STATICSBASE(pSymbol));

    if (fRecycled && !fStatics)
    {
      pSymbol->value.pFunPtr = (pModuleSymbols + ui)->value.pFunPtr;
      pSymbol->scope.value = (pModuleSymbols + ui)->scope.value;
    }
    if (fDynLib)
    {
      pSymbol->scope.value |= HB_FS_DYNCODE;
    }

    HB_SYMBOLSCOPE hSymScope = pSymbol->scope.value;
    pNewSymbols->hScope |= hSymScope;
#if 0
      fPublic = (hSymScope & (HB_FS_PUBLIC | HB_FS_MESSAGE | HB_FS_MEMVAR)) != 0;
#endif
    bool fPublic = (hSymScope & (HB_FS_INITEXIT | HB_FS_STATIC | HB_FS_FRAME)) == 0;
    if (fStatics)
    {
      if (!fRecycled && strncmp(pSymbol->szName, "(_INITSTATICS", 13) == 0)
      {
        pNewSymbols->uiStaticsOffset = ui;
      }
      fInitStatics = true;
    }

    if ((hSymScope & (HB_FS_PCODEFUNC | HB_FS_LOCAL | HB_FS_FRAME)) == (HB_FS_PCODEFUNC | HB_FS_LOCAL) &&
        (fRecycled || fClone))
    {
      pSymbol->value.pCodeFunc->pSymbols = pNewSymbols->pModuleSymbols;
    }

    if (!s_pSymStart && !fDynLib && !fStatics && (hSymScope & HB_FS_FIRST) != 0 && (hSymScope & HB_FS_INITEXIT) == 0)
    {
      /* first public defined symbol to start execution */
      s_pSymStart = pSymbol;
    }

    /* Enable this code to see static functions which are registered in global dynsym table */
#if 0
      if( fPublic && (hSymScope & (HB_FS_INITEXIT | HB_FS_STATIC)) != 0 ) {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("Registering: %s:%s scope %04x", szModuleName, pSymbol->szName, hSymScope));
#endif
      }
#endif

    if (fPublic)
    {
      if (fDynLib && HB_VM_ISFUNC(pSymbol))
      {
        auto pDynSym = hb_dynsymFind(pSymbol->szName);

        if (pDynSym)
        {
          if (fOverLoad && (pSymbol->scope.value & HB_FS_LOCAL) != 0)
          {
            /* overload existing public function */
            pDynSym->pSymbol = pSymbol;
            hb_vmSetDynFunc(pDynSym);
            continue;
          }
          pSymbol->pDynSym = pDynSym;
          if (pDynSym->pSymbol != pSymbol && HB_VM_ISFUNC(pDynSym->pSymbol) &&
              (pDynSym->pSymbol->value.pFunPtr != pSymbol->value.pFunPtr ||
               (pDynSym->pSymbol->scope.value & HB_FS_LOCAL) != 0 ||
               ((pSymbol->scope.value & (HB_FS_LOCAL | HB_FS_DYNCODE)) != (HB_FS_LOCAL | HB_FS_DYNCODE))))
          {
            pSymbol->scope.value = (pSymbol->scope.value & ~(HB_FS_PCODEFUNC | HB_FS_LOCAL)) |
                                   (pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC);
            pSymbol->value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
          }
          else
          {
            pDynSym->pSymbol = pSymbol;
          }
          continue;
        }
      }

      hb_dynsymNew(pSymbol);
    }
  }

  if (!fRecycled)
  {
    pNewSymbols->fInitStatics = fInitStatics;
  }

  return pNewSymbols;
}

static void hb_vmVerifySymbols(PHB_ITEM pArray)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmVerifySymbols(%p)", static_cast<void*>(pArray)));
#endif

  PHB_SYMBOLS pLastSymbols = s_pSymbols;
  PHB_ITEM pItem = nullptr;

  hb_arrayNew(pArray, 0);

  while (pLastSymbols)
  {
    HB_USHORT uiSymbols = pLastSymbols->uiModuleSymbols;

    for (HB_USHORT ui = 0; ui < uiSymbols; ++ui)
    {
      PHB_SYMB pSym = pLastSymbols->pModuleSymbols + ui;

      if (pSym->pDynSym && hb_dynsymFind(pSym->szName) != pSym->pDynSym)
      {
        char szText[256];
        hb_snprintf(szText, sizeof(szText), "%s->%s", pLastSymbols->szModuleName, pSym->szName);
        pItem = hb_itemPutC(pItem, szText);
        hb_arrayAddForward(pArray, pItem);
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
  if (pItem != nullptr)
  {
    hb_itemRelease(pItem);
  }
}

static void hb_vmVerifyPCodeVersion(const char *szModuleName, HB_USHORT uiPCodeVer)
{
  if (uiPCodeVer != 0)
  {
    if (uiPCodeVer > HB_PCODE_VER || /* the module is compiled with newer compiler version then HVM */
        uiPCodeVer < HB_PCODE_VER_MIN)
    { /* the module is compiled with old not longer supported by HVM compiler version */
      char szPCode[10];
      hb_snprintf(szPCode, sizeof(szPCode), "%i.%i", uiPCodeVer >> 8, uiPCodeVer & 0xff);

      hb_errInternal(HB_EI_ERRUNRECOV,
                     "Module '%s'\n"
                     "was compiled with unsupported PCODE version %s.\n"
                     "Please recompile.",
                     szModuleName, szPCode);
    }
  }
}

/*
 * module symbols initialization with extended information
 */
PHB_SYMB hb_vmProcessSymbols(PHB_SYMB pSymbols, HB_USHORT uiModuleSymbols, const char *szModuleName, HB_ULONG ulID,
                             HB_USHORT uiPCodeVer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSymbols(%p,%hu,%s,%lu,%hu)", static_cast<void*>(pSymbols), uiModuleSymbols, szModuleName, ulID, uiPCodeVer));
#endif

  hb_vmVerifyPCodeVersion(szModuleName, uiPCodeVer);
  return hb_vmRegisterSymbols(pSymbols, uiModuleSymbols, szModuleName, ulID, s_fCloneSym, s_fCloneSym, false)
      ->pModuleSymbols;
}

PHB_SYMB hb_vmProcessDynLibSymbols(PHB_SYMB pSymbols, HB_USHORT uiModuleSymbols, const char *szModuleName,
                                   HB_ULONG ulID, HB_USHORT uiPCodeVer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessDynLibSymbols(%p,%hu,%s,%lu,%hu)", static_cast<void*>(pSymbols), uiModuleSymbols, szModuleName, ulID, uiPCodeVer));
#endif

  hb_vmVerifyPCodeVersion(szModuleName, uiPCodeVer);
  return hb_vmRegisterSymbols(pSymbols, uiModuleSymbols, szModuleName, ulID, true, true, false)->pModuleSymbols;
}

static void hb_vmReleaseLocalSymbols()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmReleaseLocalSymbols()"));
#endif

  while (s_pSymbols)
  {
    PHB_SYMBOLS pDestroy = s_pSymbols;
    s_pSymbols = s_pSymbols->pNext;
    if (pDestroy->szModuleName)
    {
      hb_xfree(pDestroy->szModuleName);
    }
    if (pDestroy->fAllocated)
    {
      hb_xfree(pDestroy->pModuleSymbols);
    }
    hb_xfree(pDestroy);
  }
}

/* This calls all _INITSTATICS functions defined in the application.
 * We are using a special symbol's scope HB_FS_INITEXIT to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void hb_vmDoInitStatics()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitStatics()"));
#endif

  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  while (pLastSymbols)
  {
    if (pLastSymbols->fInitStatics)
    {
      for (HB_USHORT ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++)
      {
        HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

        if (scope == HB_FS_INITEXIT)
        {
          hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
          hb_vmPushNil();
          hb_vmProc(0);
        }
      }
      pLastSymbols->fInitStatics = false;
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

static void hb_vmDoInitFunctions(HB_BOOL fClipInit)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitFunctions(%d)", fClipInit));
#endif

  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  while (pLastSymbols && hb_vmRequestQuery() == 0)
  {
    /* only if module contains some INIT functions */
    if (pLastSymbols->fActive && (pLastSymbols->hScope & HB_FS_INIT) != 0)
    {
      HB_USHORT ui = pLastSymbols->uiModuleSymbols;

      while (ui--)
      {
        HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

        if (scope == HB_FS_INIT &&
            (strcmp((pLastSymbols->pModuleSymbols + ui)->szName, "CLIPINIT$") == 0 ? fClipInit : !fClipInit))
        {
          hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
          hb_vmPushNil();
          hb_vmProc(static_cast<HB_USHORT>(hb_cmdargPushArgs()));
          if (hb_vmRequestQuery() != 0)
          {
            break;
          }
        }
      }
    }
    pLastSymbols = pLastSymbols->pNext;
  }
}

static void hb_vmDoExitFunctions()
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoExitFunctions()"));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_SYMBOLS pLastSymbols = s_pSymbols;

  /* EXIT procedures should be processed? */
  if (s_fDoExitProc)
  {
    s_fDoExitProc = false;
    hb_stackSetActionRequest(0);

    while (pLastSymbols)
    {
      /* only if module contains some EXIT functions */
      if (pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_EXIT)
      {
        for (HB_USHORT ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++)
        {
          HB_SYMBOLSCOPE scope = (pLastSymbols->pModuleSymbols + ui)->scope.value & HB_FS_INITEXIT;

          if (scope == HB_FS_EXIT)
          {
            hb_vmPushSymbol(pLastSymbols->pModuleSymbols + ui);
            hb_vmPushNil();
            hb_vmProc(0);
            if (hb_stackGetActionRequest())
            {
              /* QUIT or BREAK was issued - stop processing
               */
              return;
            }
          }
        }
      }
      pLastSymbols = pLastSymbols->pNext;
    }
  }
}

/* ------------------------------- */
/* Extended references             */
/* ------------------------------- */

/*
 * extended item reference functions
 */
static PHB_ITEM hb_vmItemRawRefRead(PHB_ITEM pRefer)
{
  return static_cast<PHB_ITEM>(pRefer->item.asExtRef.value);
}

static PHB_ITEM hb_vmItemRawRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  HB_SYMBOL_UNUSED(pSource);
  return static_cast<PHB_ITEM>(pRefer->item.asExtRef.value);
}

static void hb_vmItemRawRefCopy(PHB_ITEM pDest)
{
  pDest->type = Harbour::Item::NIL;
  hb_itemCopy(pDest, static_cast<PHB_ITEM>(pDest->item.asExtRef.value));
}

static void hb_vmItemRawRefDummy(void *value)
{
  HB_SYMBOL_UNUSED(value);
}

static const HB_EXTREF s_ItmExtRawRef = {hb_vmItemRawRefRead, hb_vmItemRawRefWrite, hb_vmItemRawRefCopy,
                                         hb_vmItemRawRefDummy, hb_vmItemRawRefDummy};

struct HB_ITMREF
{
  HB_ITEM memvar;
  PHB_ITEM value;
};

using PHB_ITMREF = HB_ITMREF *;

static PHB_ITEM hb_vmItemRefRead(PHB_ITEM pRefer)
{
  return &(static_cast<PHB_ITMREF>(pRefer->item.asExtRef.value))->memvar;
}

static PHB_ITEM hb_vmItemRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  return hb_itemUnRefWrite((static_cast<PHB_ITMREF>(pRefer->item.asExtRef.value))->value, pSource);
}

static void hb_vmItemRefCopy(PHB_ITEM pDest)
{
  pDest->type = Harbour::Item::NIL;
  hb_itemCopy(pDest, &(static_cast<PHB_ITMREF>(pDest->item.asExtRef.value))->memvar);
}

static void hb_vmItemRefClear(void *value)
{
  auto pItmRef = static_cast<PHB_ITMREF>(value);

#if 1
  if (!HB_IS_MEMVAR(&pItmRef->memvar) || pItmRef->memvar.item.asMemvar.value != pItmRef->value ||
      !HB_IS_EXTREF(pItmRef->value) || pItmRef->value->item.asExtRef.func != &s_ItmExtRawRef)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_vmItemRefClear()", nullptr, nullptr);
  }
#endif

  if (hb_xRefDec(pItmRef->value))
  {
    hb_xfree(pItmRef->value);
  }
  else
  {
    pItmRef->memvar.type = Harbour::Item::NIL;
    hb_itemCopyFromRef(&pItmRef->memvar, pItmRef->value);
    hb_itemMove(pItmRef->value, &pItmRef->memvar);
  }

  hb_xfree(value);
}

static void hb_vmItemRefMark(void *value)
{
  /* the original value should be accessible from initial item so it's
   * not necessary to mark if form this point.
   */
#if 1
  HB_SYMBOL_UNUSED(value);
#else
  hb_gcItemRef((static_cast<PHB_ITMREF>(value))->memvar);
  hb_gcItemRef((static_cast<PHB_ITMREF>(value))->value);
#endif
}

/*
 * push extended item reference
 */
void hb_vmPushItemRef(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushItemRef(%p)", static_cast<void*>(pItem)));
#endif

  static const HB_EXTREF s_ItmExtRef = {hb_vmItemRefRead, hb_vmItemRefWrite, hb_vmItemRefCopy, hb_vmItemRefClear,
                                        hb_vmItemRefMark};

  HB_STACK_TLS_PRELOAD

  auto pItmRef = static_cast<PHB_ITMREF>(hb_xgrab(sizeof(HB_ITMREF)));

  pItmRef->value = static_cast<PHB_ITEM>(hb_xgrab(sizeof(HB_ITEM)));
  pItmRef->value->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pItmRef->value->item.asExtRef.value = static_cast<void *>(pItem);
  pItmRef->value->item.asExtRef.func = &s_ItmExtRawRef;

  pItmRef->memvar.type = Harbour::Item::BYREF | Harbour::Item::MEMVAR;
  pItmRef->memvar.item.asMemvar.value = pItmRef->value;

  auto pRefer = hb_stackAllocItem();
  pRefer->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pRefer->item.asExtRef.value = static_cast<void *>(pItmRef);
  pRefer->item.asExtRef.func = &s_ItmExtRef;
}

/* ------------------------------- */

/*
 * extended message reference structure
 */
struct HB_MSGREF
{
  PHB_DYNS access;
  PHB_DYNS assign;
  HB_ITEM object;
  HB_ITEM value;
};

using PHB_MSGREF = HB_MSGREF *;

/*
 * extended message reference functions
 */
static PHB_ITEM hb_vmMsgRefRead(PHB_ITEM pRefer)
{
  auto pMsgRef = static_cast<PHB_MSGREF>(pRefer->item.asExtRef.value);

  if (hb_vmRequestQuery() == 0)
  {
    HB_STACK_TLS_PRELOAD

    hb_stackPushReturn();
    if ((pMsgRef->value.type & Harbour::Item::DEFAULT) == 0)
    {
      hb_vmPushDynSym(pMsgRef->assign);
      hb_vmPush(&pMsgRef->object);
      hb_vmPush(&pMsgRef->value);
      hb_vmSend(1);
    }
    else
    {
      if (!pMsgRef->access)
      {
        pMsgRef->access = hb_dynsymGetCase(pMsgRef->assign->pSymbol->szName + 1);
      }
      hb_vmPushDynSym(pMsgRef->access);
      hb_vmPush(&pMsgRef->object);
      hb_vmSend(0);
    }
    hb_itemMove(&pMsgRef->value, hb_stackReturnItem());
    pMsgRef->value.type |= Harbour::Item::DEFAULT;
    hb_stackPopReturn();
  }
  return &pMsgRef->value;
}

static PHB_ITEM hb_vmMsgRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  auto pMsgRef = static_cast<PHB_MSGREF>(pRefer->item.asExtRef.value);

  if (hb_vmRequestQuery() == 0)
  {
    HB_STACK_TLS_PRELOAD
    hb_stackPushReturn();
    hb_vmPushDynSym(pMsgRef->assign);
    hb_vmPush(&pMsgRef->object);
    hb_vmPush(pSource);
    hb_vmSend(1);
    hb_itemCopy(&pMsgRef->value, pSource);
    pMsgRef->value.type |= Harbour::Item::DEFAULT;
    hb_stackPopReturn();
  }
  return nullptr;
#if 0
   return &pMsgIdxRef->value;
#endif
}

static void hb_vmMsgRefCopy(PHB_ITEM pDest)
{
  auto pMsgRef = static_cast<PHB_MSGREF>(pDest->item.asExtRef.value);

  hb_xRefInc(pMsgRef);

  if ((pMsgRef->value.type & Harbour::Item::DEFAULT) == 0)
  {
    if (hb_vmRequestReenter())
    {
      hb_vmPushDynSym(pMsgRef->assign);
      hb_vmPush(&pMsgRef->object);
      hb_vmPush(&pMsgRef->value);
      hb_vmSend(1);
      hb_vmRequestRestore();
      pMsgRef->value.type |= Harbour::Item::DEFAULT;
    }
  }
}

static void hb_vmMsgRefClear(void *value)
{
  auto pMsgRef = static_cast<PHB_MSGREF>(value);

  /* value were change by C code without calling RefWrite(),
   *  f.e. hb_stor*() function
   */
  if ((pMsgRef->value.type & Harbour::Item::DEFAULT) == 0)
  {
    if (hb_vmRequestReenter())
    {
      hb_vmPushDynSym(pMsgRef->assign);
      hb_vmPush(&pMsgRef->object);
      hb_vmPush(&pMsgRef->value);
      hb_vmSend(1);
      hb_vmRequestRestore();
      pMsgRef->value.type |= Harbour::Item::DEFAULT;
    }
  }

  if (hb_xRefDec(value))
  {
    if (HB_IS_COMPLEX(&pMsgRef->value))
    {
      hb_itemClear(&pMsgRef->value);
    }
    if (HB_IS_COMPLEX(&pMsgRef->object))
    {
      hb_itemClear(&pMsgRef->object);
    }
    hb_xfree(value);
  }
}

static void hb_vmMsgRefMark(void *value)
{
  if (HB_IS_GCITEM(&(static_cast<PHB_MSGREF>(value))->object))
  {
    hb_gcItemRef(&(static_cast<PHB_MSGREF>(value))->object);
  }
  if (HB_IS_GCITEM(&(static_cast<PHB_MSGREF>(value))->value))
  {
    hb_gcItemRef(&(static_cast<PHB_MSGREF>(value))->value);
  }
}

/*
 * create extended message reference
 */
HB_BOOL hb_vmMsgReference(PHB_ITEM pObject, PHB_DYNS pMessage, PHB_DYNS pAccMsg)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMsgReference(%p,%p,%p)", static_cast<void*>(pObject), static_cast<void*>(pMessage), static_cast<void*>(pAccMsg)));
#endif

  static const HB_EXTREF s_MsgExtRef = {hb_vmMsgRefRead, hb_vmMsgRefWrite, hb_vmMsgRefCopy, hb_vmMsgRefClear,
                                        hb_vmMsgRefMark};

  HB_STACK_TLS_PRELOAD

  auto pMsgRef = static_cast<PHB_MSGREF>(hb_xgrab(sizeof(HB_MSGREF)));
  pMsgRef->access = pAccMsg;
  pMsgRef->assign = pMessage;
  pMsgRef->value.type = Harbour::Item::NIL | Harbour::Item::DEFAULT;
  pMsgRef->object.type = Harbour::Item::NIL;
  hb_itemMove(&pMsgRef->object, pObject);

  auto pRefer = hb_stackReturnItem();
  if (HB_IS_COMPLEX(pRefer))
  {
    hb_itemClear(pRefer);
  }
  pRefer->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pRefer->item.asExtRef.value = static_cast<void *>(pMsgRef);
  pRefer->item.asExtRef.func = &s_MsgExtRef;

  return true;
}

/* ------------------------------- */

/*
 * extended object index reference structure
 */
struct HB_MSGIDXREF
{
  HB_ITEM object;
  HB_ITEM value;
  HB_ITEM index;
};

using PHB_MSGIDXREF = HB_MSGIDXREF *;

/*
 * extended object index reference functions
 */
static PHB_ITEM hb_vmMsgIdxRefRead(PHB_ITEM pRefer)
{
  auto pMsgIdxRef = static_cast<PHB_MSGIDXREF>(pRefer->item.asExtRef.value);

  if (hb_vmRequestQuery() == 0)
  {
    HB_STACK_TLS_PRELOAD
    PHB_ITEM pObject = HB_IS_BYREF(&pMsgIdxRef->object) ? hb_itemUnRef(&pMsgIdxRef->object) : &pMsgIdxRef->object;

    hb_stackPushReturn();
    if ((pMsgIdxRef->value.type & Harbour::Item::DEFAULT) == 0)
    {
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pObject, pObject, &pMsgIdxRef->index, &pMsgIdxRef->value);
    }
    else
    {
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, &pMsgIdxRef->value, pObject, &pMsgIdxRef->index, nullptr);
    }
    hb_stackPopReturn();
    pMsgIdxRef->value.type |= Harbour::Item::DEFAULT;
  }
  return &pMsgIdxRef->value;
}

static PHB_ITEM hb_vmMsgIdxRefWrite(PHB_ITEM pRefer, PHB_ITEM pSource)
{
  auto pMsgIdxRef = static_cast<PHB_MSGIDXREF>(pRefer->item.asExtRef.value);

  if (hb_vmRequestQuery() == 0)
  {
    HB_STACK_TLS_PRELOAD
    PHB_ITEM pObject = HB_IS_BYREF(&pMsgIdxRef->object) ? hb_itemUnRef(&pMsgIdxRef->object) : &pMsgIdxRef->object;
    hb_stackPushReturn();
    hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pObject, pObject, &pMsgIdxRef->index, pSource);
    hb_stackPopReturn();
    pMsgIdxRef->value.type |= Harbour::Item::DEFAULT;
  }

  return nullptr;
#if 0
   return &pMsgIdxRef->value;
#endif
}

static void hb_vmMsgIdxRefCopy(PHB_ITEM pDest)
{
  auto pMsgIdxRef = static_cast<PHB_MSGIDXREF>(pDest->item.asExtRef.value);

  hb_xRefInc(pMsgIdxRef);

  /* value were change by C code without calling RefWrite(),
   *  f.e. hb_stor*() function
   */
  if ((pMsgIdxRef->value.type & Harbour::Item::DEFAULT) == 0)
  {
    if (hb_vmRequestReenter())
    {
      PHB_ITEM pObject = HB_IS_BYREF(&pMsgIdxRef->object) ? hb_itemUnRef(&pMsgIdxRef->object) : &pMsgIdxRef->object;
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pObject, pObject, &pMsgIdxRef->index, &pMsgIdxRef->value);
      hb_vmRequestRestore();
    }
  }
}

static void hb_vmMsgIdxRefClear(void *value)
{
  auto pMsgIdxRef = static_cast<PHB_MSGIDXREF>(value);

  /* value were change by C code without calling RefWrite(),
   *  f.e. hb_stor*() function
   */
  if ((pMsgIdxRef->value.type & Harbour::Item::DEFAULT) == 0)
  {
    if (hb_vmRequestReenter())
    {
      PHB_ITEM pObject = HB_IS_BYREF(&pMsgIdxRef->object) ? hb_itemUnRef(&pMsgIdxRef->object) : &pMsgIdxRef->object;
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pObject, pObject, &pMsgIdxRef->index, &pMsgIdxRef->value);
      hb_vmRequestRestore();
    }
  }

  if (hb_xRefDec(value))
  {
    if (HB_IS_COMPLEX(&pMsgIdxRef->value))
    {
      hb_itemClear(&pMsgIdxRef->value);
    }
    if (HB_IS_COMPLEX(&pMsgIdxRef->object))
    {
      hb_itemClear(&pMsgIdxRef->object);
    }
    if (HB_IS_COMPLEX(&pMsgIdxRef->index))
    {
      hb_itemClear(&pMsgIdxRef->index);
    }
    hb_xfree(value);
  }
}

static void hb_vmMsgIdxRefMark(void *value)
{
  if (HB_IS_GCITEM(&(static_cast<PHB_MSGIDXREF>(value))->object))
  {
    hb_gcItemRef(&(static_cast<PHB_MSGIDXREF>(value))->object);
  }
  if (HB_IS_GCITEM(&(static_cast<PHB_MSGIDXREF>(value))->index))
  {
    hb_gcItemRef(&(static_cast<PHB_MSGIDXREF>(value))->index);
  }
  if (HB_IS_GCITEM(&(static_cast<PHB_MSGIDXREF>(value))->value))
  {
    hb_gcItemRef(&(static_cast<PHB_MSGIDXREF>(value))->value);
  }
}

/*
 * create extended message reference
 */
static void hb_vmMsgIndexReference(PHB_ITEM pRefer, PHB_ITEM pObject, PHB_ITEM pIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMsgIndexReference(%p,%p,%p)", static_cast<void*>(pRefer), static_cast<void*>(pObject), static_cast<void*>(pIndex)));
#endif

  static const HB_EXTREF s_MsgIdxExtRef = {hb_vmMsgIdxRefRead, hb_vmMsgIdxRefWrite, hb_vmMsgIdxRefCopy,
                                           hb_vmMsgIdxRefClear, hb_vmMsgIdxRefMark};

  auto pMsgIdxRef = static_cast<PHB_MSGIDXREF>(hb_xgrab(sizeof(HB_MSGIDXREF)));
  pMsgIdxRef->value.type = Harbour::Item::NIL | Harbour::Item::DEFAULT;
  pMsgIdxRef->object.type = Harbour::Item::NIL;
  pMsgIdxRef->index.type = Harbour::Item::NIL;
  hb_itemCopy(&pMsgIdxRef->object, HB_IS_STRING(pObject) ? pRefer : pObject);
  hb_itemMove(&pMsgIdxRef->index, pIndex);

  pIndex->type = Harbour::Item::BYREF | Harbour::Item::EXTREF;
  pIndex->item.asExtRef.value = static_cast<void *>(pMsgIdxRef);
  pIndex->item.asExtRef.func = &s_MsgIdxExtRef;
  hb_itemMove(pRefer, pIndex);
}

/* ------------------------------- */
/* VM exceptions                   */
/* ------------------------------- */

void hb_vmRequestQuit(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestQuit()"));
#endif

  HB_STACK_TLS_PRELOAD

  /* In MT mode EXIT functions are executed only from hb_vmQuit()
   * when all other threads have terminated
   */
#if !defined(HB_MT_VM)
  hb_vmDoExitFunctions(); /* process defined EXIT functions */
#endif                    /* HB_MT_VM */
  hb_stackSetActionRequest(HB_QUIT_REQUESTED);
}

void hb_vmRequestEndProc(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestEndProc()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
}

void hb_vmRequestBreak(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestBreak(%p)", static_cast<void*>(pItem)));
#endif

  HB_STACK_TLS_PRELOAD

  HB_ISIZ nRecoverBase = hb_stackGetRecoverBase();
  while (nRecoverBase && (hb_stackItem(nRecoverBase + HB_RECOVER_STATE)->item.asRecover.flags & HB_SEQ_DOALWAYS))
  {
#if defined(_HB_RECOVER_DEBUG)
    if (hb_stackItem(nRecoverBase + HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
    {
      hb_errInternal(HB_EI_ERRUNRECOV, "hb_vmRequestBreak", nullptr, nullptr);
    }
#endif
    nRecoverBase = hb_stackItem(nRecoverBase + HB_RECOVER_STATE)->item.asRecover.base;
  }

  if (nRecoverBase)
  {
#if defined(_HB_RECOVER_DEBUG)
    if (hb_stackItem(nRecoverBase + HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
    {
      hb_errInternal(HB_EI_ERRUNRECOV, "hb_vmRequestBreak2", nullptr, nullptr);
    }
#endif
    if (pItem != nullptr)
    {
      hb_itemCopy(hb_stackItem(nRecoverBase + HB_RECOVER_VALUE), pItem);
    }

    hb_stackSetActionRequest(HB_BREAK_REQUESTED);
  }
  else
  {
#ifdef HB_CLP_STRICT
    /*
     * do not execute EXIT procedures to be as close as possible
     * buggy Clipper behavior. [druzus]
     */
    s_fDoExitProc = false;
    hb_stackSetActionRequest(HB_QUIT_REQUESTED);
#else
    /*
     * Clipper has a bug here. Tests shows that it set exception flag
     * and then tries to execute EXIT procedures so the first one is
     * immediately interrupted. Because Clipper does not check the
     * exception flag often enough then it's possible to execute one
     * function from first EXIT PROC. Using small trick with
     * QOut(Type(cPrivateVar)) in the EXIT procedure (Type() is
     * not normal function) we can also check that it tries to execute
     * EXIT procedures exactly here before leave current function.
     * So to be as close as possible the Clipper intentional behavior
     * we execute hb_vmRequestQuit() here. [druzus]
     */
    hb_vmRequestQuit();
#endif
  }
}

void hb_vmRequestCancel(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestCancel()"));
#endif

  HB_STACK_TLS_PRELOAD

  if (hb_stackSetStruct()->HB_SET_CANCEL)
  {
    char
        buffer[HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 + 10]; /* additional 10 bytes for line info (%hu) overhead */
    char file[HB_PATH_MAX];
    HB_USHORT uiLine;
    int iLevel = 0;

    hb_conOutErr(hb_conNewLine(), 0);
    hb_conOutErr("Cancelled at: ", 0);

    while (hb_procinfo(iLevel++, buffer, &uiLine, file))
    {
      auto l = static_cast<int>(strlen(buffer));
      hb_snprintf(buffer + l, sizeof(buffer) - l, " (%hu)%s%s", uiLine, *file ? HB_I_(" in ") : "", file);
      hb_conOutErr(buffer, 0);
      hb_conOutErr(hb_conNewLine(), 0);
    }

    /*
     * Clipper does not execute EXIT procedures when quitting using break key
     */
    s_fDoExitProc = false;
    hb_stackSetActionRequest(HB_QUIT_REQUESTED);
  }
}

HB_USHORT hb_vmRequestQuery(void)
{
  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  if (hb_vmThreadRequest & HB_THREQUEST_QUIT)
  {
    if (!hb_stackQuitState())
    {
      hb_stackSetQuitState(true);
      hb_stackSetActionRequest(HB_QUIT_REQUESTED);
    }
  }
#endif

  return hb_stackGetActionRequest();
}

HB_BOOL hb_vmRequestReenter(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestReenter()"));
#endif

  if (s_fHVMActive)
  {
    HB_STACK_TLS_PRELOAD
    int iLocks = 0;

#if defined(HB_MT_VM)
    if (hb_stackId() == nullptr)
    {
      return false;
    }
    else
    {
      while (hb_stackLockCount() > 0)
      {
        hb_vmLock();
        ++iLocks;
      }
    }
#endif

    hb_stackPushReturn();

    auto pItem = hb_stackAllocItem();
    pItem->type = Harbour::Item::RECOVER;
    pItem->item.asRecover.recover = nullptr;
    pItem->item.asRecover.base = iLocks;
    pItem->item.asRecover.flags = 0;
    pItem->item.asRecover.request = hb_stackGetActionRequest();

    hb_stackSetActionRequest(0);

    return true;
  }
  return false;
}

void hb_vmRequestRestore(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestRestore()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_vmRequestRestore", nullptr, nullptr);
  }
  HB_USHORT uiAction = pItem->item.asRecover.request | hb_stackGetActionRequest();

#if defined(HB_MT_VM)
  if (uiAction & HB_VMSTACK_REQUESTED)
  {
    hb_vmThreadQuit();
  }
  else
  {
    auto iCount = static_cast<int>(pItem->item.asRecover.base);
#else
  {
#endif
    if (uiAction & HB_QUIT_REQUESTED)
    {
      hb_stackSetActionRequest(HB_QUIT_REQUESTED);
    }
    else if (uiAction & HB_BREAK_REQUESTED)
    {
      hb_stackSetActionRequest(HB_BREAK_REQUESTED);
    }
    else if (uiAction & HB_ENDPROC_REQUESTED)
    {
      hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
    }
    else
    {
      hb_stackSetActionRequest(0);
    }

    hb_stackDec();
    hb_stackPopReturn();

#if defined(HB_MT_VM)
    while (iCount-- > 0)
    {
      hb_vmUnlock();
    }
#endif
  }
}

HB_BOOL hb_vmRequestReenterExt(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestReenterExt()"));
#endif

  if (s_fHVMActive)
  {
    HB_USHORT uiAction = 0;
    int iLocks = 0;

#if defined(HB_MT_VM)
    HB_STACK_TLS_PRELOAD

    if (hb_stackId() == nullptr)
    {
      uiAction = HB_VMSTACK_REQUESTED;

      /* protection against executing hb_threadStateNew() during GC pass */
      HB_VM_LOCK();
      for (;;)
      {
        if (hb_vmThreadRequest & HB_THREQUEST_STOP)
        {
          hb_threadCondWait(&s_vmCond, &s_vmMtx);
        }
        else
        {
          break;
        }
      }
      s_iRunningCount++;
      HB_VM_UNLOCK();

      hb_vmThreadInit(nullptr);
      HB_STACK_TLS_RELOAD

      HB_VM_LOCK();
      s_iRunningCount--;
      hb_threadCondBroadcast(&s_vmCond);
      HB_VM_UNLOCK();
    }
    else
    {
      while (hb_stackLockCount() > 0)
      {
        hb_vmLock();
        ++iLocks;
      }
      hb_stackPushReturn();
    }
#else
    hb_stackPushReturn();
#endif
    auto pItem = hb_stackAllocItem();
    pItem->type = Harbour::Item::RECOVER;
    pItem->item.asRecover.recover = nullptr;
    pItem->item.asRecover.base = iLocks;
    pItem->item.asRecover.flags = 0;
    pItem->item.asRecover.request = uiAction | hb_stackGetActionRequest();

    hb_stackSetActionRequest(0);

    return true;
  }

  return false;
}

HB_BOOL hb_vmTryEval(PHB_ITEM *pResult, PHB_ITEM pItem, HB_ULONG ulPCount, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmTryEval(%p, %p, %lu)", static_cast<void*>(pResult), static_cast<void*>(pItem), ulPCount));
#endif

  auto fResult = false;
  *pResult = nullptr;
  if (s_fHVMActive)
  {
    PHB_SYMB pSymbol = nullptr;

    if (HB_IS_STRING(pItem))
    {
      auto pDynSym = hb_dynsymFindName(pItem->item.asString.value);

      if (pDynSym)
      {
        pSymbol = pDynSym->pSymbol;
        pItem = nullptr;
      }
    }
    else if (HB_IS_SYMBOL(pItem))
    {
      pSymbol = pItem->item.asSymbol.value;
      pItem = nullptr;
    }
    else if (pItem->isBlock())
    {
      pSymbol = &hb_symEval;
    }

    if (pSymbol && hb_vmRequestReenter())
    {
      HB_STACK_TLS_PRELOAD

      hb_xvmSeqBegin();
      hb_vmPush(hb_breakBlock());
      hb_vmSeqBlock();

      hb_vmPushSymbol(pSymbol);
      if (pItem != nullptr)
      {
        hb_vmPush(pItem);
      }
      else
      {
        hb_vmPushNil();
      }

      if (ulPCount)
      {
        va_list va;
        va_start(va, ulPCount);
        for (HB_ULONG ulParam = 1; ulParam <= ulPCount; ulParam++)
        {
          hb_vmPush(va_arg(va, PHB_ITEM));
        }
        va_end(va);
      }
      if (pItem != nullptr)
      {
        hb_vmSend(static_cast<HB_USHORT>(ulPCount));
      }
      else
      {
        hb_vmProc(static_cast<HB_USHORT>(ulPCount));
      }

      hb_stackPop();
      if (hb_xvmSeqEndTest())
      {
        hb_xvmSeqRecover();
        *pResult = hb_itemNew(nullptr);
        hb_itemMove(*pResult, hb_stackItemFromTop(-1));
        hb_stackDec();
        hb_stackSetActionRequest(0);
      }
      else
      {
        *pResult = hb_itemNew(hb_stackReturnItem());
        fResult = true;
      }
      hb_vmRequestRestore();
    }
  }
  return fResult;
}

HB_BOOL hb_vmIsActive(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsActive()"));
#endif

  return s_fHVMActive;
}

HB_BOOL hb_vmIsReady(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsReady()"));
#endif

#if defined(HB_MT_VM)
  if (s_fHVMActive)
  {
    HB_STACK_TLS_PRELOAD
    return hb_stackId() != nullptr;
  }
  else
  {
    return false;
  }
#else
  return s_fHVMActive;
#endif
}

HB_BOOL hb_vmInternalsEnabled(void)
{
  return s_fInternalsEnabled;
}

PHB_CODEPAGE hb_vmCDP(void)
{
  HB_STACK_TLS_PRELOAD
  return static_cast<PHB_CODEPAGE>(hb_stackGetCDP());
}

void hb_vmSetCDP(PHB_CODEPAGE pCDP)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetCDP(static_cast<void *>(pCDP));
}

PHB_LANG hb_vmLang(void)
{
  HB_STACK_TLS_PRELOAD
  return static_cast<PHB_LANG>(hb_stackGetLang());
}

void hb_vmSetLang(PHB_LANG pLang)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetLang(HB_UNCONST(pLang));
}

void *hb_vmI18N(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackGetI18N();
}

void hb_vmSetI18N(void *pI18N)
{
  HB_STACK_TLS_PRELOAD
  hb_i18n_release(hb_stackGetI18N());
  hb_stackSetI18N(pI18N);
}

#if defined(HB_MT_VM)
#define HB_XVM_RETURN                                                                                                  \
  {                                                                                                                    \
    if (hb_vmThreadRequest)                                                                                            \
    {                                                                                                                  \
      hb_vmRequestTest();                                                                                              \
    }                                                                                                                  \
    return (hb_stackGetActionRequest() & (HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED)) != 0;        \
  }
#else
#define HB_XVM_RETURN                                                                                                  \
  {                                                                                                                    \
    return (hb_stackGetActionRequest() & (HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED)) != 0;        \
  }
#endif /* HB_MT_VM */

void hb_xvmExitProc(void)
{
  HB_STACK_TLS_PRELOAD
  if (hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED)
  {
    hb_stackSetActionRequest(0);
  }
}

void hb_xvmEndProc(void)
{
  HB_STACK_TLS_PRELOAD
  if (!(hb_stackGetActionRequest() & (HB_QUIT_REQUESTED | HB_BREAK_REQUESTED)))
  {
    hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
  }
}

void hb_xvmSeqBegin(void)
{
  HB_STACK_TLS_PRELOAD

  /*
   * Create the SEQUENCE envelope
   * To keep compatibility with pure PCODE evaluation we have
   * use exactly the same SEQUENCE envelope or hb_vmRequestBreak()
   * will not work as expected.
   *
   * [ break return value ]  -2
   * [ recover envelope   ]  -1
   * [                    ] <- new recover base
   */

  /* 1) clear the storage for value returned by BREAK statement */
  hb_stackAllocItem()->type = Harbour::Item::NIL;
  /* 2) recovery state */
  auto pItem = hb_stackAllocItem();
  /* mark type as NIL - it's not real item */
  pItem->type = Harbour::Item::RECOVER;
  /* address of RECOVER or END opcode - not used in C code */
  pItem->item.asRecover.recover = nullptr;
  /* store current RECOVER base */
  pItem->item.asRecover.base = hb_stackGetRecoverBase();
  /* store current bCanRecover flag - not used in C code */
  pItem->item.asRecover.flags = 0;
  /* clear new recovery state */
  pItem->item.asRecover.request = 0;

  /* set new recover base */
  hb_stackSetRecoverBase(hb_stackTopOffset());
}

HB_BOOL hb_xvmSeqEnd(void)
{
  HB_STACK_TLS_PRELOAD

  /*
   * remove all items placed on the stack after BEGIN code
   */
  hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_xvmSeqEnd", nullptr, nullptr);
  }
#endif
  /*
   * Remove the SEQUENCE envelope
   * This is executed either at the end of sequence or as the
   * response to the break statement if there is no RECOVER clause
   */

  /* 2) Restore previous recovery base address */
  hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
  hb_stackDec();
  /* 1) Discard the value returned by BREAK statement */
  hb_stackPop();

#if defined(HB_MT_VM)
  if (hb_vmThreadRequest)
  {
    hb_vmRequestTest();
  }
#endif /* HB_MT_VM */
  if (hb_stackGetActionRequest() & (HB_ENDPROC_REQUESTED | HB_QUIT_REQUESTED))
  {
    return true;
  }
  else if (hb_stackGetActionRequest() & HB_BREAK_REQUESTED)
  {
    hb_stackSetActionRequest(0);
  }
  return false;
}

HB_BOOL hb_xvmSeqEndTest(void)
{
  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  if (hb_vmThreadRequest)
  {
    hb_vmRequestTest();
  }
#endif /* HB_MT_VM */
  if ((hb_stackGetActionRequest() & (HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED)) != 0)
  {
    return true;
  }

  /*
   * remove all items placed on the stack after BEGIN code
   */
  hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_xvmSeqEndTest", nullptr, nullptr);
  }
#endif
  /*
   * Remove the SEQUENCE envelope
   * This is executed either at the end of sequence or as the
   * response to the break statement if there is no RECOVER clause
   */

  /* 2) Restore previous recovery base address */
  hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
  hb_stackDec();
  /* 1) Discard the value returned by BREAK statement */
  hb_stackPop();
  return false;
}

HB_BOOL hb_xvmSeqRecover(void)
{
  HB_STACK_TLS_PRELOAD

  /*
   * Execute the RECOVER code
   */

  /*
   * remove all items placed on the stack after BEGIN code
   */
  hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_xvmSeqRecover", nullptr, nullptr);
  }
#endif
  /* 2) Restore previous recovery base address */
  hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
  hb_stackDec();
  /* 1) Leave the value returned from BREAK */

#if defined(HB_MT_VM)
  if (hb_vmThreadRequest)
  {
    hb_vmRequestTest();
  }
#endif /* HB_MT_VM */
  if (hb_stackGetActionRequest() & (HB_ENDPROC_REQUESTED | HB_QUIT_REQUESTED))
  {
    return true;
  }
  else if (hb_stackGetActionRequest() & HB_BREAK_REQUESTED)
  {
    hb_stackSetActionRequest(0);
  }
  return false;
}

void hb_xvmSeqAlways(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSeqAlways()"));
#endif

  HB_STACK_TLS_PRELOAD

  /* Create the SEQUENCE ALWAYS envelope */
  /* 1) clear the storage for RETURN value */
  hb_stackAllocItem()->type = Harbour::Item::NIL;
  /* 2) recovery state */
  auto pItem = hb_stackAllocItem();
  /* mark type as NIL - it's not real item */
  pItem->type = Harbour::Item::RECOVER;
  /* address of RECOVER or END opcode - not used in C code */
  pItem->item.asRecover.recover = nullptr;
  /* store current RECOVER base */
  pItem->item.asRecover.base = hb_stackGetRecoverBase();
  /* store current bCanRecover flag - not used in C code */
  pItem->item.asRecover.flags = 0;
  /* clear new recovery state */
  pItem->item.asRecover.request = 0;
  /* set sequence type */
  pItem->item.asRecover.flags = HB_SEQ_DOALWAYS;
  /* set new recover base */
  hb_stackSetRecoverBase(hb_stackTopOffset());
}

HB_BOOL hb_xvmAlwaysBegin(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmAlwaysBegin()"));
#endif

  HB_STACK_TLS_PRELOAD

  /* remove all items placed on the stack after BEGIN code */
  hb_stackRemove(hb_stackGetRecoverBase());
#if defined(_HB_RECOVER_DEBUG)
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_xvmAlwaysBegin", nullptr, nullptr);
  }
#endif
  /* store and reset action */
  hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request = hb_stackGetActionRequest();
  hb_stackSetActionRequest(0);
  /* store RETURN value */
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request & HB_ENDPROC_REQUESTED)
  {
    hb_itemMove(hb_stackItemFromTop(HB_RECOVER_VALUE), hb_stackReturnItem());
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmAlwaysEnd(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmAlwaysEnd()"));
#endif

  HB_STACK_TLS_PRELOAD

  /* remove all items placed on the stack after ALWAYSBEGIN code */
  hb_stackRemove(hb_stackGetRecoverBase());

#if defined(_HB_RECOVER_DEBUG)
  if (hb_stackItemFromTop(HB_RECOVER_STATE)->type != Harbour::Item::RECOVER)
  {
    hb_errInternal(HB_EI_ERRUNRECOV, "hb_xvmAlwaysEnd", nullptr, nullptr);
  }
#endif
  /* restore previous recovery base address */
  hb_stackSetRecoverBase(hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.base);
  HB_USHORT uiCurrAction = hb_stackGetActionRequest();
  HB_USHORT uiPrevAction = hb_stackItemFromTop(HB_RECOVER_STATE)->item.asRecover.request;
  /* restore requested action */
  if ((uiCurrAction | uiPrevAction) & HB_QUIT_REQUESTED)
  {
    hb_stackSetActionRequest(HB_QUIT_REQUESTED);
  }
  else if ((uiCurrAction | uiPrevAction) & HB_BREAK_REQUESTED)
  {
    hb_stackSetActionRequest(HB_BREAK_REQUESTED);
  }
  else if ((uiCurrAction | uiPrevAction) & HB_ENDPROC_REQUESTED)
  {
    hb_stackSetActionRequest(HB_ENDPROC_REQUESTED);
  }
  else
  {
    hb_stackSetActionRequest(0);
  }
  /* remove the ALWAYS envelope */
  hb_stackDec();
  /* restore RETURN value if not overloaded inside ALWAYS code */
  if (!(uiCurrAction & HB_ENDPROC_REQUESTED) && (uiPrevAction & HB_ENDPROC_REQUESTED))
  {
    hb_stackPopReturn();
  }
  else
  {
    hb_stackPop();
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmSeqBlock(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSeqBlock()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmSeqBlock();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumStart(int nVars, int nDescend)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumStart(%d,%d)", nVars, nDescend));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmEnumStart(nVars, nDescend);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumNext(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumNext()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmEnumNext();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumPrev(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumPrev()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmEnumPrev();
  HB_XVM_RETURN
}

void hb_xvmEnumEnd(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEnumEnd()"));
#endif

  hb_vmEnumEnd();
}

HB_BOOL hb_xvmSwitchGet(PHB_ITEM *pSwitchPtr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSwitchGet(%p)", static_cast<void*>(pSwitchPtr)));
#endif

  HB_STACK_TLS_PRELOAD
  *pSwitchPtr = hb_vmSwitchGet();
  HB_XVM_RETURN
}

void hb_xvmSetLine(HB_USHORT uiLine)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSetLine(%hu)", uiLine));
#endif

  HB_STACK_TLS_PRELOAD

  hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = uiLine;
#ifndef HB_NO_DEBUG
  if (hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging)
  {
    hb_vmDebuggerShowLine(uiLine);
  }
#endif
}

void hb_xvmFrame(int iLocals, int iParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFrame(%d, %d)", iLocals, iParams));
#endif

  hb_vmFrame(static_cast<HB_USHORT>(iLocals), static_cast<unsigned char>(iParams));
}

void hb_xvmVFrame(int iLocals, int iParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmVFrame(%d, %d)", iLocals, iParams));
#endif

  hb_vmVFrame(static_cast<HB_USHORT>(iLocals), static_cast<unsigned char>(iParams));
}

void hb_xvmSFrame(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSFrame(%p)", static_cast<void*>(pSymbol)));
#endif

  hb_vmSFrame(pSymbol);
}

HB_BOOL hb_xvmDo(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDo(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmProc(uiParams);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmFunction(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFunction(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemSetNil(hb_stackReturnItem());
  hb_vmProc(uiParams);
  hb_stackPushReturn();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmSend(HB_USHORT uiParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSend(%hu)", uiParams));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemSetNil(hb_stackReturnItem());
  hb_vmSend(uiParams);
  hb_stackPushReturn();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushObjectVarRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushObjectVarRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushObjectVarRef();
  HB_XVM_RETURN
}

void hb_xvmRetValue(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmRetValue()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackPopReturn();
  hb_stackReturnItem()->type &= ~Harbour::Item::MEMOFLAG;
}

void hb_xvmRetNil(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmRetNil()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemSetNil(hb_stackReturnItem());
}

void hb_xvmRetInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmRetInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD
  hb_itemPutNL(hb_stackReturnItem(), lValue);
}

void hb_xvmStatics(PHB_SYMB pSymbol, HB_USHORT uiStatics)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmStatics(%p,%hu)", static_cast<void*>(pSymbol), uiStatics));
#endif

  hb_vmStatics(pSymbol, uiStatics);
}

void hb_xvmThreadStatics(HB_USHORT uiStatics, const HB_BYTE *statics)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmThreadStatics(%hu,%p)", uiStatics, static_cast<const void*>(statics)));
#endif

  hb_vmInitThreadStatics(uiStatics, statics);
}

void hb_xvmParameter(PHB_SYMB pSymbol, int iParams)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmParameter(%p,%d)", static_cast<void*>(pSymbol), iParams));
#endif

  HB_STACK_TLS_PRELOAD
  hb_memvarNewParameter(pSymbol, hb_stackItemFromBase(iParams));
}

void hb_xvmPushLocal(HB_SHORT iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLocal(%hd)", iLocal));
#endif

  hb_vmPushLocal(iLocal);
}

void hb_xvmPushLocalByRef(HB_SHORT iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLocalByRef(%hd)", iLocal));
#endif

  hb_vmPushLocalByRef(iLocal);
}

void hb_xvmPopLocal(HB_SHORT iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopLocal(%hd)", iLocal));
#endif

  hb_vmPopLocal(iLocal);
}

static PHB_ITEM hb_xvmLocalPtr(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalPtr(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD

  if (iLocal >= 0)
  {
    /* local variable or local parameter */
    return hb_stackLocalVariable(iLocal);
  }
  else
  {
    /* local variable referenced in a codeblock
     * hb_stackSelfItem() points to a codeblock that is currently evaluated
     */
    return hb_codeblockGetRef(hb_stackSelfItem()->item.asBlock.value, iLocal);
  }
}

void hb_xvmCopyLocals(int iDest, int iSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmCopyLocals(%d,%d)", iDest, iSource));
#endif

  PHB_ITEM pDest = hb_xvmLocalPtr(iDest);
  hb_itemCopyToRef(hb_xvmLocalPtr(iSource), HB_IS_BYREF(pDest) ? hb_itemUnRef(pDest) : pDest);
}

void hb_xvmPushStatic(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushStatic(%hu)", uiStatic));
#endif

  hb_vmPushStatic(uiStatic);
}

void hb_xvmPushStaticByRef(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushStaticByRef(%hu)", uiStatic));
#endif

  hb_vmPushStaticByRef(uiStatic);
}

void hb_xvmPopStatic(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopStatic(%hu)", uiStatic));
#endif

  hb_vmPopStatic(uiStatic);
}

HB_BOOL hb_xvmPushVariable(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushVariable(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushVariable(pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopVariable(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopVariable(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  /* See the note above in HB_P_POPVARIABLE */
#if 0
   if( pSymbol->pDynSym && hb_dynsymGetMemvar(pSymbol->pDynSym) ) {
      hb_memvarSetValue(pSymbol, hb_stackItemFromTop(-1));
   } else if( hb_rddFieldPut(hb_stackItemFromTop(-1), pSymbol) == Harbour::FAILURE )
#endif
  hb_memvarSetValue(pSymbol, hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

void hb_xvmPushBlockShort(const HB_BYTE *pCode, PHB_SYMB pSymbols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushBlockShort(%p, %p)", static_cast<const void*>(pCode), static_cast<void*>(pSymbols)));
#endif

  hb_vmPushBlockShort(pCode, pSymbols, false);
}

void hb_xvmPushBlock(const HB_BYTE *pCode, PHB_SYMB pSymbols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushBlock(%p, %p)", static_cast<const void*>(pCode), static_cast<void*>(pSymbols)));
#endif

  hb_vmPushBlock(pCode, pSymbols, false);
}

void hb_xvmPushSelf(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushSelf()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPush(hb_stackSelfItem());
}

void hb_xvmPushFuncSymbol(PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushFuncSymbol(%p)", static_cast<void*>(pSym)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->type = Harbour::Item::SYMBOL;
  pItem->item.asSymbol.value = pSym;
  pItem->item.asSymbol.stackstate = nullptr;
  hb_stackAllocItem()->type = Harbour::Item::NIL;
}

HB_BOOL hb_xvmPopLogical(HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopLogical(%p)", static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD
  *pfValue = hb_vmPopLogical();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAlias(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPopAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmSelectWorkarea(hb_stackItemFromTop(-1), nullptr); /* it clears the passed item */
  hb_stackDec();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmSwapAlias(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSwapAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmSwapAlias();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushField(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushField(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_rddGetFieldValue(hb_stackAllocItem(), pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAlias(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushAlias()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushAlias();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedField(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushAliasedField(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushAliasedField(pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedFieldExt(PHB_SYMB pAlias, PHB_SYMB pField)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushAliasedFieldExt(%p,%p)", static_cast<void*>(pAlias), static_cast<void*>(pField)));
#endif

  HB_STACK_TLS_PRELOAD
  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  if (hb_rddSelectWorkAreaSymbol(pAlias) == Harbour::SUCCESS)
  {
    hb_rddGetFieldValue(hb_stackAllocItem(), pField);
  }
  hb_rddSelectWorkAreaNumber(iCurrArea);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedVar(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushAliasedVar(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPushAliasedVar(pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopField(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopField(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_rddPutFieldValue(hb_stackItemFromTop(-1), pSymbol);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushMemvar(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushMemvar(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_memvarGetValue(hb_stackAllocItem(), pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPushMemvarByRef(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPushMemvarByRef(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_memvarGetRefer(hb_stackAllocItem(), pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopMemvar(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopMemvar(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_memvarSetValue(pSymbol, hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedField(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopAliasedField(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPopAliasedField(pSymbol);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedFieldExt(PHB_SYMB pAlias, PHB_SYMB pField)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopAliasedFieldExt(%p,%p)", static_cast<void*>(pAlias), static_cast<void*>(pField)));
#endif

  HB_STACK_TLS_PRELOAD
  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  if (hb_rddSelectWorkAreaSymbol(pAlias) == Harbour::SUCCESS)
  {
    hb_rddPutFieldValue(hb_stackItemFromTop(-1), pField);
    hb_stackPop();
  }
  hb_rddSelectWorkAreaNumber(iCurrArea);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedVar(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmPopAliasedVar(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPopAliasedVar(pSymbol);
  HB_XVM_RETURN
}

void hb_xvmLocalSetInt(int iLocal, HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalSetInt(%d, %ld)", iLocal, lValue));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal;

  if (iLocal >= 0)
  {
    /* local variable or local parameter */
    pLocal = hb_stackLocalVariable(iLocal);
    if (HB_IS_BYREF(pLocal))
    {
      pLocal = hb_itemUnRef(pLocal);
    }
  }
  else
  {
    /* local variable referenced in a codeblock
     * hb_stackSelfItem() points to a codeblock that is currently evaluated
     */
    pLocal = hb_codeblockGetVar(hb_stackSelfItem(), iLocal);
  }

  if (HB_IS_OBJECT(pLocal) && hb_objHasOperator(pLocal, HB_OO_OP_ASSIGN))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_ASSIGN, pLocal, pLocal, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_itemPutNL(pLocal, lValue);
  }
}

HB_BOOL hb_xvmLocalAddInt(int iLocal, HB_LONG lAdd)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalAddInt(%d,%ld)", iLocal, lAdd));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmAddInt(hb_stackLocalVariable(iLocal), lAdd);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalInc(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalInc(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
  hb_vmInc(HB_IS_BYREF(pLocal) ? hb_itemUnRef(pLocal) : pLocal);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalDec(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalDec(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
  hb_vmDec(HB_IS_BYREF(pLocal) ? hb_itemUnRef(pLocal) : pLocal);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalIncPush(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalInc(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
  if (HB_IS_BYREF(pLocal))
  {
    pLocal = hb_itemUnRef(pLocal);
  }
  hb_vmInc(pLocal);
  hb_itemCopy(hb_stackAllocItem(), pLocal);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalAdd(int iLocal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalAdd(%d)", iLocal));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pLocal = hb_stackLocalVariable(iLocal);
  if (HB_IS_BYREF(pLocal))
  {
    pLocal = hb_itemUnRef(pLocal);
  }
  hb_vmPlus(pLocal, hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmStaticAdd(HB_USHORT uiStatic)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmStaticAdd(%hu)", uiStatic));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pStatic = (static_cast<PHB_ITEM>(hb_stackGetStaticsBase()))->item.asArray.value->pItems + uiStatic - 1;
  if (HB_IS_BYREF(pStatic))
  {
    pStatic = hb_itemUnRef(pStatic);
  }
  hb_vmPlus(pStatic, hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMemvarAdd(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_INFO, ("hb_xvmMemvarAdd(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD
  auto pVal1 = hb_stackItemFromTop(-2);
  auto pVal2 = hb_stackItemFromTop(-1);
  if (HB_IS_STRING(pVal1) && HB_IS_STRING(pVal2))
  {
    PHB_ITEM pMemVar = hb_memvarGetItem(pSymbol);
    if (pMemVar)
    {
      hb_vmPlus(pMemVar, pVal1, pVal2);
      hb_stackPop();
      hb_stackPop();
      HB_XVM_RETURN
    }
  }
  hb_vmPlus(pVal1, pVal1, pVal2);
  hb_memvarSetValue(pSymbol, pVal1);
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmAnd(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmAnd()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmAnd();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmOr(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmOr()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmOr();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmNot(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNot()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmNot();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmNegate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNegate()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmNegate();
  HB_XVM_RETURN
}

void hb_xvmDuplicate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDuplicate()"));
#endif

  hb_vmDuplicate();
}

void hb_xvmDuplUnRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDuplUnRef()"));
#endif

  hb_vmDuplUnRef();
}

void hb_xvmPushUnRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushUnRef()"));
#endif

  hb_vmPushUnRef();
}

void hb_xvmSwap(int iCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmSwap(%d)", iCount));
#endif

  hb_vmSwap(iCount);
}

HB_BOOL hb_xvmForTest(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmForTest()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmForTest();
  HB_XVM_RETURN
}

void hb_xvmFuncPtr(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmFuncPtr()"));
#endif

  hb_vmFuncPtr();
}

HB_BOOL hb_xvmEqual(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEqual()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmEqual();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmExactlyEqual(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmExactlyEqual()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmExactlyEqual();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmEqualInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEqualInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) == lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value == static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value == static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value == static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isNil())
  {
    pItem->item.asLogical.value = false;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_EQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_EQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1071, nullptr, "=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmEqualIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmEqualIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) == lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value == static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value == static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value == static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (pItem->isNil())
  {
    *pfValue = false;
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_EQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_EQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1071, nullptr, "=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqual(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNotEqual()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmNotEqual();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqualInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNotEqualInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) != lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value != static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value != static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value != static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isNil())
  {
    pItem->item.asLogical.value = true;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_NOTEQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_NOTEQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1072, nullptr, "<>", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqualIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmNotEqualIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) != lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value != static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value != static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value != static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (pItem->isNil())
  {
    *pfValue = true;
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_NOTEQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_NOTEQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1072, nullptr, "<>", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmLess(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLess()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmLess();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLessThenInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessThenInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) < lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value < static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value < static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value < static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_LESS))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_LESS, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1073, nullptr, "<", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmLessThenIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessThenIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) < lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value < static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value < static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value < static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_LESS))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_LESS, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1073, nullptr, "<", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqual(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessEqual()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmLessEqual();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqualThenInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessEqualThenInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) <= lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value <= static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value <= static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value <= static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_LESSEQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_LESSEQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1074, nullptr, "<=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqualThenIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLessEqualThenIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) <= lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value <= static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value <= static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value <= static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_LESSEQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_LESSEQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1074, nullptr, "<=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreater(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreater()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmGreater();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterThenInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterThenInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) > lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value > static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value > static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value > static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_GREATER))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_GREATER, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1075, nullptr, ">", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterThenIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterThenIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) > lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value > static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value > static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value > static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_GREATER))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_GREATER, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1075, nullptr, ">", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqual(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterEqual()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmGreaterEqual();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqualThenInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterEqualThenInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    pItem->item.asLogical.value = static_cast<HB_LONG>(pItem->item.asInteger.value) >= lValue;
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value >= static_cast<HB_MAXINT>(lValue);
    pItem->item.asLogical.value = f;
#else
    pItem->item.asLogical.value = pItem->item.asLong.value >= static_cast<HB_MAXINT>(lValue);
#endif
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (pItem->isDouble())
  {
    pItem->item.asLogical.value = pItem->item.asDouble.value >= static_cast<double>(lValue);
    pItem->type = Harbour::Item::LOGICAL;
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_GREATEREQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_GREATEREQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1076, nullptr, ">=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqualThenIntIs(HB_LONG lValue, HB_BOOL *pfValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmGreaterEqualThenIntIs(%ld,%p)", lValue, static_cast<void*>(pfValue)));
#endif

  HB_STACK_TLS_PRELOAD

  auto pItem = hb_stackItemFromTop(-1);
  if (pItem->isInteger())
  {
    *pfValue = static_cast<HB_LONG>(pItem->item.asInteger.value) >= lValue;
    hb_stackDec();
  }
  else if (HB_IS_LONG(pItem))
  {
#if defined(__DCC__) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
    bool f = pItem->item.asLong.value >= static_cast<HB_MAXINT>(lValue);
    *pfValue = f;
#else
    *pfValue = pItem->item.asLong.value >= static_cast<HB_MAXINT>(lValue);
#endif
    hb_stackDec();
  }
  else if (pItem->isDouble())
  {
    *pfValue = pItem->item.asDouble.value >= static_cast<double>(lValue);
    hb_stackDec();
  }
  else if (hb_objHasOperator(pItem, HB_OO_OP_GREATEREQUAL))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_GREATEREQUAL, pItem, pItem, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
    return hb_xvmPopLogical(pfValue);
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pResult = hb_errRT_BASE_Subst(EG_ARG, 1074, nullptr, "<=", 2, pItem, hb_stackItemFromTop(-1));
    if (pResult)
    {
      hb_stackPop();
      hb_itemMove(pItem, pResult);
      hb_itemRelease(pResult);
      return hb_xvmPopLogical(pfValue);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmInstring(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmInstring()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmInstring();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmAddInt(HB_LONG lAdd)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmAddInt(%ld)", lAdd));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmAddInt(hb_stackItemFromTop(-1), lAdd);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPlus(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPlus()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPlus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPlusEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPlusEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmPlus(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPlusEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPlusEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmPlus(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMinus(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMinus()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMinus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMinusEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMinusEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmMinus(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMinusEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMinusEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmMinus(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMultByInt(HB_LONG lValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMultByInt(%ld)", lValue));
#endif

  HB_STACK_TLS_PRELOAD

  auto pValue = hb_stackItemFromTop(-1);
  if (HB_IS_NUMERIC(pValue))
  {
    int iDec;
    double dValue = hb_itemGetNDDec(pValue, &iDec);
    hb_itemPutNumType(pValue, dValue * lValue, iDec, HB_ITEM_TYPERAW(pValue), Harbour::Item::INTEGER);
  }
  else if (hb_objHasOperator(pValue, HB_OO_OP_MULT))
  {
    hb_vmPushLong(lValue);
    hb_objOperatorCall(HB_OO_OP_MULT, pValue, pValue, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lValue);
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1083, nullptr, "*", 2, pValue, hb_stackItemFromTop(-1));
    if (pSubst)
    {
      hb_stackPop();
      hb_itemMove(pValue, pSubst);
      hb_itemRelease(pSubst);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmMult(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMult()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMult(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMultEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMultEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmMult(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMultEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMultEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmMult(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDivideByInt(HB_LONG lDivisor)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivideByInt(%ld)", lDivisor));
#endif

  HB_STACK_TLS_PRELOAD

  auto pValue = hb_stackItemFromTop(-1);
  if (HB_IS_NUMERIC(pValue))
  {
    if (lDivisor == 0)
    {
      hb_vmPushLong(lDivisor);
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1340, nullptr, "/", 2, pValue, hb_stackItemFromTop(-1));
      if (pSubst)
      {
        hb_stackPop();
        hb_itemMove(pValue, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else
    {
      hb_itemPutND(pValue, hb_itemGetND(pValue) / lDivisor);
    }
  }
  else if (hb_objHasOperator(pValue, HB_OO_OP_DIVIDE))
  {
    hb_vmPushLong(lDivisor);
    hb_objOperatorCall(HB_OO_OP_DIVIDE, pValue, pValue, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lDivisor);
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1084, nullptr, "/", 2, pValue, hb_stackItemFromTop(-1));
    if (pSubst)
    {
      hb_stackPop();
      hb_itemMove(pValue, pSubst);
      hb_itemRelease(pSubst);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmModulusByInt(HB_LONG lDivisor)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModulusByInt(%ld)", lDivisor));
#endif

  HB_STACK_TLS_PRELOAD

  auto pValue = hb_stackItemFromTop(-1);
  if (HB_IS_NUMERIC(pValue))
  {
    if (lDivisor == 0)
    {
      hb_vmPushLong(lDivisor);
      PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ZERODIV, 1341, nullptr, "%", 2, pValue, hb_stackItemFromTop(-1));
      if (pSubst)
      {
        hb_stackPop();
        hb_itemMove(pValue, pSubst);
        hb_itemRelease(pSubst);
      }
    }
    else if (HB_IS_NUMINT(pValue))
    {
      hb_itemPutND(pValue, static_cast<double>(HB_ITEM_GET_NUMINTRAW(pValue) % lDivisor));
    }
    else
    {
      hb_itemPutND(pValue, fmod(hb_itemGetND(pValue), lDivisor));
    }
  }
  else if (hb_objHasOperator(pValue, HB_OO_OP_MOD))
  {
    hb_vmPushLong(lDivisor);
    hb_objOperatorCall(HB_OO_OP_MOD, pValue, pValue, hb_stackItemFromTop(-1), nullptr);
    hb_stackPop();
  }
  else
  {
    hb_vmPushLong(lDivisor);
    PHB_ITEM pSubst = hb_errRT_BASE_Subst(EG_ARG, 1085, nullptr, "%", 2, pValue, hb_stackItemFromTop(-1));
    if (pSubst)
    {
      hb_stackPop();
      hb_itemMove(pValue, pSubst);
      hb_itemRelease(pSubst);
    }
  }

  HB_XVM_RETURN
}

HB_BOOL hb_xvmDivide(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivide()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmDivide(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDivEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmDivide(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDivEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDivEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmDivide(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmModulus(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModulus()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmModulus(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmModEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmModulus(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmModEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmModulus(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmPower(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPower()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmPower(hb_stackItemFromTop(-2), hb_stackItemFromTop(-2), hb_stackItemFromTop(-1));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmExpEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmExpEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  auto pValue = hb_stackItemFromTop(-1);
  hb_vmPower(pResult, pResult, pValue);
  hb_itemCopy(pValue, pResult);
  hb_itemMove(hb_stackItemFromTop(-2), pValue);
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmExpEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmExpEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_itemUnRef(hb_stackItemFromTop(-2));
  hb_vmPower(pResult, pResult, hb_stackItemFromTop(-1));
  hb_stackPop();
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmInc(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmInc()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmInc(hb_stackItemFromTop(-1));
  HB_XVM_RETURN
}

HB_BOOL hb_xvmIncEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmIncEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_stackItemFromTop(-1);
  auto pValue = hb_itemUnRef(pResult);
  hb_vmInc(pValue);
  auto pTemp = hb_stackAllocItem();
  hb_itemCopy(pTemp, pValue);
  hb_itemMove(pResult, pTemp);
  hb_stackDec();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmIncEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmIncEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmInc(hb_itemUnRef(hb_stackItemFromTop(-1)));
  hb_stackPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDec(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDec()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmDec(hb_stackItemFromTop(-1));
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDecEq(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDecEq()"));
#endif

  HB_STACK_TLS_PRELOAD
  auto pResult = hb_stackItemFromTop(-1);
  auto pValue = hb_itemUnRef(pResult);
  hb_vmDec(pValue);
  auto pTemp = hb_stackAllocItem();
  hb_itemCopy(pTemp, pValue);
  hb_itemMove(pResult, pTemp);
  hb_stackDec();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmDecEqPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmDecEqPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmDec(hb_itemUnRef(hb_stackItemFromTop(-1)));
  hb_stackPop();
  HB_XVM_RETURN
}

void hb_xvmArrayDim(HB_USHORT uiDimensions)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayDim(%hu)", uiDimensions));
#endif

  hb_vmArrayDim(uiDimensions);
}

void hb_xvmArrayGen(HB_SIZE nElements)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayGen(%" HB_PFS "u)", nElements));
#endif

  hb_vmArrayGen(nElements);
}

void hb_xvmHashGen(HB_SIZE nElements)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmHashGen(%" HB_PFS "u)", nElements));
#endif

  hb_vmHashGen(nElements);
}

static void hb_vmArrayItemPush(HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayItemPush(%" HB_PFS "u)", nIndex));
#endif

  HB_STACK_TLS_PRELOAD

  auto pArray = hb_stackItemFromTop(-1);

  if (pArray->isArray())
  {
    if (HB_IS_OBJECT(pArray) && hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
    {
      hb_vmPushNumInt(nIndex);
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), nullptr);
      hb_stackPop();
      return;
    }

    if (HB_IS_VALID_INDEX(nIndex, pArray->item.asArray.value->nLen))
    {
      auto pItem = hb_stackAllocItem();
      hb_itemCopy(pItem, pArray->item.asArray.value->pItems + nIndex - 1);
      hb_itemMove(pArray, pItem);
      hb_stackDec();
    }
    else
    {
      hb_vmPushNumInt(nIndex);
      if (!HB_IS_OBJECT(pArray) &&
          hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), nullptr))
      {
        hb_stackPop();
      }
      else
      {
#ifdef HB_CLP_STRICT
        hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 0);
#else
        hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, hb_stackItemFromTop(-1));
#endif
      }
    }
  }
  else if (HB_IS_HASH(pArray))
  {
    hb_vmPushNumInt(nIndex);
    auto pIndex = hb_stackItemFromTop(-1);
    auto pValue = hb_hashGetItemPtr(pArray, pIndex, HB_HASH_AUTOADD_ACCESS);

    if (pValue)
    {
      hb_itemCopy(pIndex, pValue);
      hb_itemMove(pArray, pIndex);
      hb_stackDec();
    }
    else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, nullptr))
    {
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, pIndex);
    }
  }
  else
  {
    hb_vmPushNumInt(nIndex);
    if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), nullptr))
    {
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1068, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pArray, hb_stackItemFromTop(-1));
    }
  }
}

static void hb_vmArrayItemPop(HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayItemPop(%" HB_PFS "u)", nIndex));
#endif

  HB_STACK_TLS_PRELOAD

  auto pValue = hb_stackItemFromTop(-2);
  auto pArray = hb_stackItemFromTop(-1);

  if (HB_IS_BYREF(pArray))
  {
    pArray = hb_itemUnRef(pArray);
  }

  if (pArray->isArray())
  {
    if (HB_IS_OBJECT(pArray) && hb_objHasOperator(pArray, HB_OO_OP_ARRAYINDEX))
    {
      hb_vmPushNumInt(nIndex);
      hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), pValue);
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
      return;
    }

    if (HB_IS_VALID_INDEX(nIndex, pArray->item.asArray.value->nLen))
    {
      pValue->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
      hb_itemMoveRef(pArray->item.asArray.value->pItems + nIndex - 1, pValue);
      hb_stackPop();
      hb_stackDec(); /* value was moved above hb_stackDec() is enough */
    }
    else
    {
      hb_vmPushNumInt(nIndex);
      if (!HB_IS_OBJECT(pArray) &&
          hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), pValue))
      {
        hb_stackPop();
        hb_stackPop();
        hb_stackPop();
      }
      else
      {
#ifdef HB_CLP_STRICT
        hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 0);
#else
        hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, hb_stackItemFromTop(-1));
#endif
      }
    }
  }
  else if (HB_IS_HASH(pArray))
  {
    hb_vmPushNumInt(nIndex);
    auto pDest = hb_hashGetItemPtr(pArray, hb_stackItemFromTop(-1), HB_HASH_AUTOADD_ASSIGN);
    if (pDest)
    {
      pValue->type &= ~(Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT);
      hb_itemMoveRef(pDest, pValue);
      hb_stackPop();
      hb_stackPop();
      hb_stackDec(); /* value was moved above hb_stackDec() is enough */
    }
    else if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_BOUND, 1133, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 3, pArray, hb_stackItemFromTop(-1),
                    pValue);
    }
  }
  else
  {
    hb_vmPushNumInt(nIndex);
    if (hb_objOperatorCall(HB_OO_OP_ARRAYINDEX, pArray, pArray, hb_stackItemFromTop(-1), pValue))
    {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 1069, nullptr, hb_langDGetErrorDesc(EG_ARRASSIGN), 1, hb_stackItemFromTop(-1));
    }
  }
}

HB_BOOL hb_xvmArrayPush(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPush()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmArrayPush();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayPushRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPushRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmArrayPushRef();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayItemPush(HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayItemPush(%" HB_PFS "u)", nIndex));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmArrayItemPush(nIndex);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayPop(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayPop()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmArrayPop();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayItemPop(HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmArrayItemPop(%" HB_PFS "u)", nIndex));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmArrayItemPop(nIndex);
  HB_XVM_RETURN
}

void hb_xvmPushDouble(double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushDouble(%lf, %d, %d)", dNumber, iWidth, iDec));
#endif

  hb_vmPushDoubleConst(dNumber, iWidth, iDec);
}

#ifdef HB_LONG_LONG_OFF
void hb_xvmPushLongLong(double dNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLongLong(%l.0f)", dNumber));
#endif

  hb_vmPushDoubleConst(dNumber, HB_DEFAULT_WIDTH, 0);
}
#else
void hb_xvmPushLongLong(HB_LONGLONG llNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushLongLong(%" PFLL "i)", llNumber));
#endif

  hb_vmPushLongLongConst(llNumber);
}
#endif

void hb_xvmPushStringHidden(int iMethod, const char *szText, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushStringHidden(%d, %s, %" HB_PFS "u)", iMethod, szText, nSize));
#endif

  HB_STACK_TLS_PRELOAD
  char *szString = hb_compDecodeString(iMethod, szText, &nSize);
  hb_itemPutCLPtr(hb_stackAllocItem(), szString, nSize);
}

void hb_xvmLocalName(HB_USHORT uiLocal, const char *szLocalName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmLocalName(%hu, %s)", uiLocal, szLocalName));
#endif

#ifndef HB_NO_DEBUG
  hb_vmLocalName(uiLocal, szLocalName);
#else
  HB_SYMBOL_UNUSED(uiLocal);
  HB_SYMBOL_UNUSED(szLocalName);
#endif
}

void hb_xvmStaticName(HB_BYTE bIsGlobal, HB_USHORT uiStatic, const char *szStaticName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmStaticName(%d, %hu, %s)", static_cast<int>(bIsGlobal), uiStatic, szStaticName));
#endif

#ifndef HB_NO_DEBUG
  hb_vmStaticName(bIsGlobal, uiStatic, szStaticName);
#else
  HB_SYMBOL_UNUSED(bIsGlobal);
  HB_SYMBOL_UNUSED(uiStatic);
  HB_SYMBOL_UNUSED(szStaticName);
#endif
}

void hb_xvmModuleName(const char *szModuleName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmModuleName(%s)", szModuleName));
#endif

#ifndef HB_NO_DEBUG
  hb_vmModuleName(szModuleName);
#else
  HB_SYMBOL_UNUSED(szModuleName);
#endif
}

HB_BOOL hb_xvmMacroArrayGen(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroArrayGen(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMacroArrayGen(uiArgSets);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroDo(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroDo(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMacroDo(uiArgSets);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroFunc(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroFunc(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMacroFunc(uiArgSets);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroSend(HB_USHORT uiArgSets)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroSend(%hu)", uiArgSets));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMacroSend(uiArgSets);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPush(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPush(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroGetValue(hb_stackItemFromTop(-1), 0, iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushRef()"));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pMacro = hb_stackItemFromTop(-1);
  hb_macroPushReference(pMacro);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushIndex(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushIndex()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_vmMacroPushIndex();
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushList(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushList(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroGetValue(hb_stackItemFromTop(-1), HB_P_MACROPUSHLIST, iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushPare(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushPare(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroGetValue(hb_stackItemFromTop(-1), HB_P_MACROPUSHPARE, iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushAliased(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPushAliased(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroPushAliasedValue(hb_stackItemFromTop(-2), hb_stackItemFromTop(-1), iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPop(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPop(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroSetValue(hb_stackItemFromTop(-1), iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPopAliased(int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroPopAliased(%d)", iFlags));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroPopAliasedValue(hb_stackItemFromTop(-2), hb_stackItemFromTop(-1), iFlags);
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroSymbol(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroSymbol()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroPushSymbol(hb_stackItemFromTop(-1));
  HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroText(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmMacroText()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_macroTextValue(hb_stackItemFromTop(-1));
  HB_XVM_RETURN
}

void hb_xvmPushVParams(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushVParams()"));
#endif

  hb_vmPushVParams();
}

void hb_xvmPushAParams(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmPushAParams()"));
#endif

  hb_vmPushAParams();
}

void hb_xvmWithObjectStart(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmWithObjectStart()"));
#endif

  hb_vmWithObjectStart();
}

void hb_xvmWithObjectEnd(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmWithObjectEnd()"));
#endif

  HB_STACK_TLS_PRELOAD
  hb_stackPop(); /* remove with object envelope */
  hb_stackPop(); /* remove implicit object */
}

void hb_xvmWithObjectMessage(PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xvmWithObjectMessage(%p)", static_cast<void*>(pSymbol)));
#endif

  HB_STACK_TLS_PRELOAD

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
  }

  PHB_ITEM pWith = hb_stackWithObjectItem();
  if (pWith)
  {
    hb_vmPush(pWith);
  }
  else
  {
    hb_stackAllocItem()->type = Harbour::Item::NIL;
  }
}

/* ------------------------------------------------------------------------ */
/* The debugger support functions */
/* ------------------------------------------------------------------------ */

void hb_vmRequestDebug(void)
{
#ifndef HB_NO_DEBUG
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestDebug()"));
#endif

  HB_STACK_TLS_PRELOAD
  *(hb_stackDebugRequest()) = true;
#endif
}

HB_BOOL hb_dbg_InvokeDebug(HB_BOOL bInvoke)
{
#ifndef HB_NO_DEBUG
  HB_STACK_TLS_PRELOAD
  HB_BOOL *pfRequest = hb_stackDebugRequest();
  HB_BOOL bRequest = *pfRequest;
  *pfRequest = bInvoke;
  return bRequest;
#else
  HB_SYMBOL_UNUSED(bInvoke);
  return false;
#endif
}

HB_DBGENTRY_FUNC hb_dbg_SetEntry(HB_DBGENTRY_FUNC pFunDbgEntry)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dbg_SetEntry(%p)", reinterpret_cast<void*>(pFunDbgEntry)));
#endif

  HB_DBGENTRY_FUNC pPrevFunc;

#ifndef HB_NO_DEBUG
  pPrevFunc = s_pFunDbgEntry;
  s_pFunDbgEntry = pFunDbgEntry;
#else
  HB_SYMBOL_UNUSED(pFunDbgEntry);
  pPrevFunc = nullptr;
#endif

  return pPrevFunc;
}

PHB_ITEM hb_dbg_vmVarSGet(PHB_ITEM pStaticsBase, int nOffset)
{
  if (pStaticsBase)
  {
    return hb_arrayGetItemPtr(pStaticsBase, nOffset);
  }
  else
  {
    return nullptr;
  }
}

HB_ULONG hb_dbg_ProcLevel(void)
{
  return hb_stackCallDepth();
}

/*
 * check if the debugger activation was requested or request the debugger
 * activation if .T. is passed
 */
HB_FUNC(__DBGINVOKEDEBUG)
{
  HB_STACK_TLS_PRELOAD

  if (hb_vmInternalsEnabled())
  {
#ifndef HB_NO_DEBUG
    HB_BOOL *pfRequest = hb_stackDebugRequest();
    hb_retl(*pfRequest);
    *pfRequest = hb_parl(1);
#else
    hb_retl(false);
#endif
  }
  else
  {
    hb_retl(false);
  }
}

/* Return the statics array. Please AClone() before assignments
 * __dbgVMVarSList() --> <aStat>
 */
HB_FUNC(__DBGVMVARSLIST)
{
  if (hb_vmInternalsEnabled())
  {
    hb_itemReturnRelease(hb_vmStaticsArray());
  }
  else
  {
    HB_STACK_TLS_PRELOAD
    hb_reta(0);
  }
}

/* Return the statics array length.
 * __dbgVMVarSLen() --> <nStatics>
 */
HB_FUNC(__DBGVMVARSLEN)
{
  HB_STACK_TLS_PRELOAD

  if (hb_vmInternalsEnabled())
  {
    hb_retnint(hb_vmStaticsCount());
  }
  else
  {
    hb_retnint(0);
  }
}

/* Return a specified statics
 * __dbgVMVarSGet(<nStatic>) --> <xStat>
 */
HB_FUNC(__DBGVMVARSGET)
{
  if (hb_vmInternalsEnabled())
  {
    hb_itemReturn(hb_dbg_vmVarSGet(hb_param(1, Harbour::Item::ARRAY), hb_parni(2)));
  }
}

/*
 * Sets the value of a specified statics
 * __dbgVMVarSSet(<nStatic>, <uValue>) --> NIL
 */
HB_FUNC(__DBGVMVARSSET)
{
  if (hb_vmInternalsEnabled())
  {
    auto pStaticsBase = hb_param(1, Harbour::Item::ARRAY);
    auto pItem = hb_param(3, Harbour::Item::ANY);

    if (pStaticsBase && pItem)
    {
      hb_arraySet(pStaticsBase, hb_parni(2), pItem);
    }
  }
}

HB_FUNC(__DBGPROCLEVEL)
{
  if (hb_vmInternalsEnabled())
  {
    HB_STACK_TLS_PRELOAD
    hb_retnl(hb_dbg_ProcLevel() - 1); /* Don't count self */
  }
}

/*
 * These functions are for GLOBAL variables - now they are only for
 * compatibility with xHarbour debugger - Harbour does not support
 * GLOBALs
 */
HB_ULONG hb_dbg_vmVarGCount(void)
{
#if 0
   return hb_arrayLen(&s_aGlobals);
#else
  return 0;
#endif
}

PHB_ITEM hb_dbg_vmVarGGet(int nGlobal, int nOffset)
{
#if 0
   return hb_arrayGetItemPtr(&s_aGlobals, nGlobal + nOffset);
#else
  HB_SYMBOL_UNUSED(nGlobal);
  HB_SYMBOL_UNUSED(nOffset);
  return nullptr;
#endif
}

/*
 * Return a clone of the globals array.
 * __dbgVMVarGList() --> <aStat>
 */
HB_FUNC(__DBGVMVARGLIST)
{
  if (hb_vmInternalsEnabled())
  {
#if 0
      PHB_ITEM pGlobals = hb_itemClone(&s_aGlobals);
#else
    auto pGlobals = hb_itemArrayNew(0);
#endif
    hb_itemReturnRelease(pGlobals);
  }
  else
  {
    HB_STACK_TLS_PRELOAD
    hb_reta(0);
  }
}

HB_FUNC(__DBGVMVARGGET)
{
  if (hb_vmInternalsEnabled())
  {
    hb_itemReturn(hb_dbg_vmVarGGet(hb_parni(1), hb_parni(2)));
  }
}

HB_FUNC(__DBGVMVARGSET)
{
#if 0
   if( hb_vmInternalsEnabled() ) {
      auto pItem = hb_param(3, Harbour::Item::ANY);
      if( pItem != nullptr ) {
         hb_arraySet(&s_aGlobals, hb_parni(1) + hb_parni(2), pItem);
      }
   }
#endif
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsStaticRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsStaticRef()"));
#endif

  /* statics are stored as an item of arrays allocated by hb_itemNew() so
   * they do not need any special GC support
   */
}

void hb_vmIsStackRef(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsStackRef()"));
#endif

#if defined(HB_MT_VM)
  if (s_vmStackLst)
  {
    PHB_THREADSTATE pStack = s_vmStackLst;
    do
    {
      hb_gcMark(pStack);
      if (pStack->fActive && pStack->pStackId)
      {
        hb_stackIsStackRef(pStack->pStackId, hb_vmTSVarClean);
      }
      pStack = pStack->pNext;
    } while (pStack != s_vmStackLst);
  }
#else
  hb_stackIsStackRef(hb_stackId(), nullptr);
#endif /* HB_MT_VM */
}

void hb_vmUpdateAllocator(PHB_ALLOCUPDT_FUNC pFunc, int iCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmUpdateAllocator(%p, %d)", reinterpret_cast<void*>(pFunc), iCount));
#endif

#if defined(HB_MT_VM)
  if (s_vmStackLst)
  {
    PHB_THREADSTATE pStack = s_vmStackLst;
    do
    {
      if (pStack->pStackId)
      {
        hb_stackUpdateAllocator(pStack->pStackId, pFunc, iCount);
      }
      pStack = pStack->pNext;
    } while (pStack != s_vmStackLst);
  }
#else
  hb_stackUpdateAllocator(hb_stackId(), pFunc, iCount);
#endif /* HB_MT_VM */
}

/* ------------------------------------------------------------------------ */

/*
 * Turns on | off the profiler activity
 * __SetProfiler(<lOnOff>) --> <lOldValue>
 */
HB_FUNC(__SETPROFILER)
{
  HB_STACK_TLS_PRELOAD
#ifdef HB_NO_PROFILER
  hb_retl(false);
#else
  hb_retl(hb_bProfiler);
  if (HB_ISLOG(1))
  {
    hb_bProfiler = hb_parl(1);
  }
#endif
}

HB_FUNC(__OPCOUNT) /* it returns the total amount of opcodes */
{
  HB_STACK_TLS_PRELOAD
  hb_retnl(HB_P_LAST_PCODE - 1);
}

HB_FUNC(__OPGETPRF) /* profiler: It returns an array with an opcode called and
                       consumed times { nTimes, nTime },
                       given the opcode index */
{
  HB_STACK_TLS_PRELOAD
#ifndef HB_NO_PROFILER
  HB_ULONG ulOpcode = hb_parnl(1);
  hb_reta(2);
  if (ulOpcode < HB_P_LAST_PCODE)
  {
    hb_storvnl(hb_ulOpcodesCalls[ulOpcode], -1, 1);
    hb_storvnl(hb_ulOpcodesTime[ulOpcode], -1, 2);
  }
  else
#else
  hb_reta(2);
#endif
  {
    hb_storvnl(0, -1, 1);
    hb_storvnl(0, -1, 2);
  }
}

/*
 * Turns on | off tracing of PRG-level function and method calls
 * __TracePrgCalls(<lOnOff>) --> <lOldValue>
 */
HB_FUNC(__TRACEPRGCALLS)
{
  HB_STACK_TLS_PRELOAD
#if defined(HB_PRG_TRACE)
  hb_retl(hb_bTracePrgCalls);
  if (HB_ISLOG(1))
  {
    hb_bTracePrgCalls = hb_parl(1);
  }
#else
  hb_retl(false);
#endif
}

HB_FUNC(__QUITCANCEL)
{
  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  if (!hb_stackQuitState())
#endif
  {
    HB_ISIZ nRecoverBase = hb_stackGetRecoverBase();

    if (nRecoverBase)
    {
      auto pRecover = hb_stackItem(nRecoverBase + HB_RECOVER_STATE);

#if defined(_HB_RECOVER_DEBUG)
      if (pRecover->type != Harbour::Item::RECOVER)
      {
        hb_errInternal(HB_EI_ERRUNRECOV, "hb_vmRequestBreak", nullptr, nullptr);
      }
#endif
      if (pRecover->item.asRecover.flags & HB_SEQ_DOALWAYS)
      {
        pRecover->item.asRecover.flags &= ~HB_QUIT_REQUESTED;
        pRecover->item.asRecover.request &= ~HB_QUIT_REQUESTED;
      }
    }
  }
}

HB_BOOL hb_vmSetKeyPool(HB_BOOL fEnable)
{
#ifndef HB_GUI
  bool fPrev = s_fKeyPool;
  s_fKeyPool = fEnable;
  return fPrev;
#else
  HB_SYMBOL_UNUSED(fEnable);
  return false;
#endif
}

HB_FUNC(__VMKEYPOOL)
{
  HB_STACK_TLS_PRELOAD

#ifndef HB_GUI
  hb_retl(s_fKeyPool);
  if (HB_ISLOG(1))
  {
    s_fKeyPool = hb_parl(1);
  }
#else
  hb_retl(false);
#endif
}

HB_FUNC(__VMNOINTERNALS)
{
  s_fInternalsEnabled = false;
}

HB_FUNC(__VMITEMID)
{
  HB_STACK_TLS_PRELOAD

  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr)
  {
    if (pItem->isArray())
    {
      hb_retptr(hb_arrayId(pItem));
    }
    else if (HB_IS_HASH(pItem))
    {
      hb_retptr(hb_hashId(pItem));
    }
    else if (pItem->isBlock())
    {
      hb_retptr(hb_codeblockId(pItem));
    }
    else if (HB_IS_SYMBOL(pItem))
    {
      hb_retptr(pItem->item.asSymbol.value);
    }
  }
}

HB_FUNC(__VMITEMREFS)
{
  HB_STACK_TLS_PRELOAD

  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr)
  {
    if (pItem->isArray())
    {
      hb_retnint(hb_arrayRefs(pItem));
    }
    else if (HB_IS_HASH(pItem))
    {
      hb_retnint(hb_hashRefs(pItem));
    }
    else if (pItem->isBlock())
    {
      hb_retnint(hb_codeblockRefs(pItem));
    }
    else if (HB_IS_POINTER(pItem))
    {
      hb_retnint(hb_gcRefCount(pItem->item.asPointer.value));
    }
    else if (HB_IS_STRING(pItem))
    {
      hb_retnint(hb_xRefCount(pItem->item.asString.value));
    }
  }
}

HB_FUNC(__VMMODULESVERIFY)
{
  HB_STACK_TLS_PRELOAD
  hb_vmVerifySymbols(hb_stackReturnItem());
}

HB_FUNC(__VMCOUNTTHREADS)
{
  int iStacks, iThreads;

#if defined(HB_MT_VM)
  HB_STACK_TLS_PRELOAD

  HB_VM_LOCK();

  iStacks = s_iStackCount;
  iThreads = s_iRunningCount;

  HB_VM_UNLOCK();
#else
  iStacks = iThreads = 0;
#endif

  hb_storni(iStacks, 1);
  hb_storni(iThreads, 2);

  hb_retni(iThreads);
}

HB_FUNC(__BREAKBLOCK)
{
  hb_itemReturn(hb_breakBlock());
}

HB_FUNC(__RECOVERERRORBLOCK)
{
  HB_STACK_TLS_PRELOAD

  HB_ISIZ nRecoverBase = hb_stackGetRecoverBase();

  if (nRecoverBase > 0 && nRecoverBase < hb_stackTopOffset())
  {
    auto pItem = hb_stackItem(nRecoverBase);

    if (HB_IS_POINTER(pItem) && pItem->item.asPointer.collect && pItem->item.asPointer.single &&
        hb_gcFuncs(pItem->item.asPointer.value) == &s_gcSeqBlockFuncs)
    {
      hb_itemReturn(static_cast<PHB_ITEM>(pItem->item.asPointer.value));
    }
  }
}

HB_FUNC(HB_ARRAYTOPARAMS)
{
  HB_STACK_TLS_PRELOAD

  auto pArray = hb_param(1, Harbour::Item::ARRAY);

  if (pArray)
  {
    hb_arrayLast(pArray, hb_stackReturnItem());
  }
}

HB_FUNC(ERRORLEVEL)
{
  HB_STACK_TLS_PRELOAD

  hb_retni(s_nErrorLevel);

  /* NOTE: This should be HB_ISNUM(1), but it's sort of a Clipper bug that it
           accepts other types also and considers them zero. [vszakats] */

  if (hb_pcount() >= 1)
  {
    /* Only replace the error level if a parameter was passed */
    s_nErrorLevel = hb_parni(1);
  }
}

/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

extern void hb_vmForceLink(void);
void hb_vmForceLink(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmForceLink()"));
#endif

  HB_FUNC_EXEC(SYSINIT);
}

/* NOTE: Pass string literals only. */
void hb_vmSetLinkedMain(const char *szMain)
{
  s_vm_pszLinkedMain = szMain;
}

void hb_vmSetDefaultGT(const char *szGtName)
{
  hb_gtSetDefault(szGtName);
}

/* Force linking default language and codepage modules */
HB_CODEPAGE_REQUEST(HB_CODEPAGE_DEFAULT)
HB_LANG_REQUEST(HB_LANG_DEFAULT)

#undef HB_FORCE_LINK_MAIN

#if !defined(HB_DYNLIB) && defined(HB_OS_WIN) && (defined(__MINGW32__))

#define HB_FORCE_LINK_MAIN hb_forceLinkMainWin

#endif

#ifdef HB_FORCE_LINK_MAIN
HB_EXTERN_BEGIN
extern void HB_FORCE_LINK_MAIN(void);
HB_EXTERN_END
extern void _hb_forceLinkMain(void);
void _hb_forceLinkMain()
{
  HB_FORCE_LINK_MAIN();
}
#endif
