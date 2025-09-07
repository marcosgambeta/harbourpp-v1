//
// Exception handlers
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt) (hb_winExceptionHandler() Windows exception info dump
// code.) Copyright 2008-2010 Viktor Szakats (vszakats.net/harbour) (hb_winExceptionHandler() Module listing code,
// x86_64/WinCE/ARM support, OS/2, MIPS32, MIPS64, SH, IA64 CPU dumps.)
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

#include "hbapi.hpp"
#include "hbvm.hpp"
#include "hbapifs.hpp"
#include "hbdate.hpp"
#include "hbapierr.hpp"
#include "hbset.hpp"

#if defined(HB_OS_UNIX)
#include <unistd.h>
#include <signal.h>
#if defined(SIGSTKSZ) && ((defined(_BSD_SOURCE) && _BSD_SOURCE) || (defined(_XOPEN_SOURCE) && _XOPEN_SOURCE >= 500))
#define HB_SIGNAL_EXCEPTION_HANDLER
#endif
#elif defined(HB_OS_WIN)
#include <windows.h>
#include <tlhelp32.h>
#include "hbwinuni.hpp"
// BCC and MinGW doesn't seem to #define this
#ifndef TH32CS_SNAPMODULE32
#define TH32CS_SNAPMODULE32 0
#endif
#endif

#if defined(HB_SIGNAL_EXCEPTION_HANDLER)
#if defined(__GLIBC__) && defined(__GLIBC_MINOR__) && (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 34)
static HB_BYTE *s_signal_stack[32768];
#else
static HB_BYTE
    *s_signal_stack[SIGSTKSZ]; // TODO: error: size of array �s_signal_stack� is not an integral constant-expression
#endif
#endif

#if defined(HB_OS_WIN)

static LONG WINAPI hb_winExceptionHandler(struct _EXCEPTION_POINTERS *pExceptionInfo)
{
  char errmsg[8192];
  int errmsglen = sizeof(errmsg) - 1;
  errmsg[0] = '\0';

#if defined(HB_OS_WIN_64) && defined(HB_CPU_X86_64)
  {
    char buf[32];
    PCONTEXT pCtx = pExceptionInfo->ContextRecord;
    const char *szCode;

    // two most common codes
    switch (pExceptionInfo->ExceptionRecord->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION:
      szCode = " "
               "ACCESS_VIOLATION";
      break;
    case EXCEPTION_IN_PAGE_ERROR:
      szCode = " "
               "IN_PAGE_ERROR";
      break;
    default:
      szCode = "";
    }

    hb_snprintf(errmsg, errmsglen,
                "\n\n"
                "    Exception Code:%08X%s\n"
                "    Exception Address:%016" PFLL "X\n"
                "    RAX:%016" PFLL "X  RBX:%016" PFLL "X  RCX:%016" PFLL "X  RDX:%016" PFLL "X\n"
                "    RSI:%016" PFLL "X  RDI:%016" PFLL "X  RBP:%016" PFLL "X\n"
                "    R8 :%016" PFLL "X  R9 :%016" PFLL "X  R10:%016" PFLL "X  R11:%016" PFLL "X\n"
                "    R12:%016" PFLL "X  R13:%016" PFLL "X  R14:%016" PFLL "X  R15:%016" PFLL "X\n"
                "    CS:RIP:%04X:%016" PFLL "X  SS:RSP:%04X:%016" PFLL "X\n"
                "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
                "    Flags:%08X\n",
                static_cast<HB_U32>(pExceptionInfo->ExceptionRecord->ExceptionCode), szCode,
                reinterpret_cast<HB_PTRUINT>(pExceptionInfo->ExceptionRecord->ExceptionAddress), pCtx->Rax, pCtx->Rbx,
                pCtx->Rcx, pCtx->Rdx, pCtx->Rsi, pCtx->Rdi, pCtx->Rbp, pCtx->R8, pCtx->R9, pCtx->R10, pCtx->R11,
                pCtx->R12, pCtx->R13, pCtx->R14, pCtx->R15, static_cast<HB_U32>(pCtx->SegCs), pCtx->Rip,
                static_cast<HB_U32>(pCtx->SegSs), pCtx->Rsp, static_cast<HB_U32>(pCtx->SegDs),
                static_cast<HB_U32>(pCtx->SegEs), static_cast<HB_U32>(pCtx->SegFs), static_cast<HB_U32>(pCtx->SegGs),
                static_cast<HB_U32>(pCtx->EFlags));

    if (pExceptionInfo->ExceptionRecord->NumberParameters &&
        pExceptionInfo->ExceptionRecord->NumberParameters < static_cast<DWORD>(EXCEPTION_MAXIMUM_PARAMETERS)) {
      hb_strncat(errmsg, "    Exception Parameters:", errmsglen);
      for (DWORD arg = 0; arg < pExceptionInfo->ExceptionRecord->NumberParameters; ++arg) {
        hb_snprintf(buf, sizeof(buf), " %016" PFLL "X",
                    static_cast<HB_U64>(pExceptionInfo->ExceptionRecord->ExceptionInformation[arg]));
        hb_strncat(errmsg, buf, errmsglen);
      }
      hb_strncat(errmsg, "\n", errmsglen);
    }

    // TODO: 64-bit stack trace.
    //       See: - StackWalk64()
    //            - https://www.codeproject.com/KB/threads/StackWalker.aspx?fid=202364
  }
#elif defined(HB_OS_WIN_64) && defined(HB_CPU_IA_64)
  {
    PCONTEXT pCtx = pExceptionInfo->ContextRecord;

    hb_snprintf(errmsg, errmsglen,
                "\n\n"
                "    Exception Code:%08X\n"
                "    Exception Address:%016" PFLL "X\n"
                "    IS0 :%016" PFLL "X  IS1 :%016" PFLL "X  IS2 :%016" PFLL "X  IS3 :%016" PFLL "X\n"
                "    IT0 :%016" PFLL "X  IT1 :%016" PFLL "X  IT2 :%016" PFLL "X  IT3 :%016" PFLL "X\n"
                "    IT4 :%016" PFLL "X  IT5 :%016" PFLL "X  IT6 :%016" PFLL "X  IT7 :%016" PFLL "X\n"
                "    IT8 :%016" PFLL "X  IT9 :%016" PFLL "X  IT10:%016" PFLL "X  IT11:%016" PFLL "X\n"
                "    IT12:%016" PFLL "X  IT13:%016" PFLL "X  IT14:%016" PFLL "X  IT15:%016" PFLL "X\n"
                "    IT16:%016" PFLL "X  IT17:%016" PFLL "X  IT18:%016" PFLL "X  IT19:%016" PFLL "X\n"
                "    IT20:%016" PFLL "X  IT21:%016" PFLL "X  IT22:%016" PFLL "X\n"
                "    IGp :%016" PFLL "X  IV0 :%016" PFLL "X  ISp :%016" PFLL "X  ITeb:%016" PFLL "X\n"
                "    INat:%016" PFLL "X\n",
                static_cast<HB_U32>(pExceptionInfo->ExceptionRecord->ExceptionCode),
                pExceptionInfo->ExceptionRecord->ExceptionAddress, pCtx->IntS0, pCtx->IntS1, pCtx->IntS2, pCtx->IntS3,
                pCtx->IntT0, pCtx->IntT1, pCtx->IntT2, pCtx->IntT3, pCtx->IntT4, pCtx->IntT5, pCtx->IntT6, pCtx->IntT7,
                pCtx->IntT8, pCtx->IntT9, pCtx->IntT10, pCtx->IntT11, pCtx->IntT12, pCtx->IntT13, pCtx->IntT14,
                pCtx->IntT15, pCtx->IntT16, pCtx->IntT17, pCtx->IntT18, pCtx->IntT19, pCtx->IntT20, pCtx->IntT21,
                pCtx->IntT22, pCtx->IntGp, pCtx->IntV0, pCtx->IntSp, pCtx->IntTeb, pCtx->IntNats);
  }
#elif defined(HB_CPU_X86)
  {
    char buf[64 + MAX_PATH];
    PCONTEXT pCtx = pExceptionInfo->ContextRecord;
    const char *szCode;

    // two most common codes
    switch (pExceptionInfo->ExceptionRecord->ExceptionCode) {
    case EXCEPTION_ACCESS_VIOLATION:
      szCode = " "
               "ACCESS_VIOLATION";
      break;
    case EXCEPTION_IN_PAGE_ERROR:
      szCode = " "
               "IN_PAGE_ERROR";
      break;
    default:
      szCode = "";
    }

    hb_snprintf(errmsg, errmsglen,
                "\n\n"
                "    Exception Code:%08X%s\n"
                "    Exception Address:%08X\n"
                "    EAX:%08X  EBX:%08X  ECX:%08X  EDX:%08X\n"
                "    ESI:%08X  EDI:%08X  EBP:%08X\n"
                "    CS:EIP:%04X:%08X  SS:ESP:%04X:%08X\n"
                "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
                "    Flags:%08X\n",
                static_cast<HB_U32>(pExceptionInfo->ExceptionRecord->ExceptionCode), szCode,
                reinterpret_cast<HB_U32>(pExceptionInfo->ExceptionRecord->ExceptionAddress),
                static_cast<HB_U32>(pCtx->Eax), static_cast<HB_U32>(pCtx->Ebx), static_cast<HB_U32>(pCtx->Ecx),
                static_cast<HB_U32>(pCtx->Edx), static_cast<HB_U32>(pCtx->Esi), static_cast<HB_U32>(pCtx->Edi),
                static_cast<HB_U32>(pCtx->Ebp), static_cast<HB_U32>(pCtx->SegCs), static_cast<HB_U32>(pCtx->Eip),
                static_cast<HB_U32>(pCtx->SegSs), static_cast<HB_U32>(pCtx->Esp), static_cast<HB_U32>(pCtx->SegDs),
                static_cast<HB_U32>(pCtx->SegEs), static_cast<HB_U32>(pCtx->SegFs), static_cast<HB_U32>(pCtx->SegGs),
                static_cast<HB_U32>(pCtx->EFlags));

    if (pExceptionInfo->ExceptionRecord->NumberParameters &&
        pExceptionInfo->ExceptionRecord->NumberParameters < static_cast<DWORD>(EXCEPTION_MAXIMUM_PARAMETERS)) {
      hb_strncat(errmsg, "    Exception Parameters:", errmsglen);
      for (DWORD arg = 0; arg < pExceptionInfo->ExceptionRecord->NumberParameters; ++arg) {
        hb_snprintf(buf, sizeof(buf), " %08X",
                    static_cast<HB_U32>(pExceptionInfo->ExceptionRecord->ExceptionInformation[arg]));
        hb_strncat(errmsg, buf, errmsglen);
      }
      hb_strncat(errmsg, "\n", errmsglen);
    }

    {
      hb_strncat(errmsg, "    CS:EIP:", errmsglen);
      auto pc = reinterpret_cast<unsigned char *>(pCtx->Eip);
      for (auto i = 0; i < 16; i++) {
        // FIXME: Unsafe function.
        if (IsBadReadPtr(pc, 1)) {
          break;
        }
        hb_snprintf(buf, sizeof(buf), " %02X", static_cast<int>(pc[i]));
        hb_strncat(errmsg, buf, errmsglen);
      }
      hb_strncat(errmsg, "\n    SS:ESP:", errmsglen);
      auto sc = reinterpret_cast<unsigned int *>(pCtx->Esp);
      for (auto i = 0; i < 16; i++) {
        // FIXME: Unsafe function.
        if (IsBadReadPtr(sc, 4)) {
          break;
        }
        hb_snprintf(buf, sizeof(buf), " %08X", sc[i]);
        hb_strncat(errmsg, buf, errmsglen);
      }
      hb_strncat(errmsg, "\n\n", errmsglen);
      hb_strncat(errmsg, "    C stack:\n", errmsglen);
      hb_strncat(errmsg, "    EIP:     EBP:       Frame: OldEBP, RetAddr, Params...\n", errmsglen);
      unsigned int eip = pCtx->Eip;
      auto ebp = reinterpret_cast<unsigned int *>(pCtx->Ebp);
      // FIXME: Unsafe function.
      if (!IsBadWritePtr(ebp, 8)) {
        for (auto i = 0; i < 20; i++) {
          // FIXME: Unsafe function.
          if (reinterpret_cast<unsigned int>(ebp) % 4 != 0 || IsBadWritePtr(ebp, 40) ||
              reinterpret_cast<unsigned int>(ebp) >= ebp[0]) {
            break;
          }
          hb_snprintf(buf, sizeof(buf), "    %08X %08X  ", static_cast<int>(eip), reinterpret_cast<int>(ebp));
          hb_strncat(errmsg, buf, errmsglen);
          for (unsigned int j = 0; j < 10 && reinterpret_cast<unsigned int>(ebp + j) < ebp[0]; j++) {
            hb_snprintf(buf, sizeof(buf), " %08X", ebp[j]);
            hb_strncat(errmsg, buf, errmsglen);
          }
          hb_strncat(errmsg, "\n", errmsglen);
          eip = ebp[1];
          ebp = reinterpret_cast<unsigned int *>(ebp[0]);
        }
        hb_strncat(errmsg, "\n", errmsglen);
      }
    }
  }
#endif

  {
    // Take a snapshot of all modules in the specified process.
    HANDLE hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, GetCurrentProcessId());

    if (hModuleSnap != INVALID_HANDLE_VALUE) {
      MODULEENTRY32 me32;

      // Set the size of the structure before using it.
      me32.dwSize = sizeof(MODULEENTRY32);

      // Retrieve information about the first module, and exit if unsuccessful
      if (Module32First(hModuleSnap, &me32)) {
        hb_strncat(errmsg, "\nModules:\n", errmsglen);

        // Now walk the module list of the process, and display information about each module
        do {
          char buf[256];
#if defined(HB_OS_WIN_64)
          // FIXME: me32.szExePath seemed trashed in some (standalone) tests.
          hb_snprintf(buf, sizeof(buf), "%016" PFLL "X %016" PFLL "X %s\n",
                      reinterpret_cast<HB_PTRUINT>(me32.modBaseAddr), static_cast<HB_PTRUINT>(me32.modBaseSize),
                      me32.szExePath);
#else
          char szBuffer[MAX_PATH];
          hb_strncpy(szBuffer, reinterpret_cast<const char *>(me32.szExePath), HB_SIZEOFARRAY(szBuffer) - 1);
          hb_snprintf(buf, sizeof(buf), "%08lX %08lX %s\n", reinterpret_cast<HB_PTRUINT>(me32.modBaseAddr),
                      static_cast<HB_PTRUINT>(me32.modBaseSize), szBuffer);
#endif
          hb_strncat(errmsg, buf, errmsglen);
        } while (Module32Next(hModuleSnap, &me32));
      }

      // Do not forget to clean up the snapshot object.
      CloseHandle(hModuleSnap);
    }
  }

  hb_errInternalRaw(6005, "Exception error:%s", errmsg, nullptr);

  return hb_cmdargCheck("BATCH") ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH;
}

#elif defined(HB_SIGNAL_EXCEPTION_HANDLER)

static void hb_signalExceptionHandler(int sig, siginfo_t *si, void *ucp)
{
  char buffer[32];
  const char *signame;
  const char *sigaddr;

  HB_SYMBOL_UNUSED(ucp);

  switch (sig) {
  case SIGSEGV:
    signame = "SIGSEGV";
    hb_snprintf(buffer, sizeof(buffer), "%p", si->si_addr);
    sigaddr = buffer;
    break;
  case SIGILL:
    signame = "SIGILL";
    hb_snprintf(buffer, sizeof(buffer), "%p", si->si_addr);
    sigaddr = buffer;
    break;
  case SIGFPE:
    signame = "SIGFPE";
    hb_snprintf(buffer, sizeof(buffer), "%p", si->si_addr);
    sigaddr = buffer;
    break;
  case SIGBUS:
    signame = "SIGBUS";
    hb_snprintf(buffer, sizeof(buffer), "%p", si->si_addr);
    sigaddr = buffer;
    break;
  default:
    hb_snprintf(buffer, sizeof(buffer), "sig:%d", sig);
    signame = buffer;
    sigaddr = "UNKNOWN";
    break;
  }

  hb_errInternal(6005, "Exception %s at address %s", signame, sigaddr);
}

#endif

void hb_vmSetExceptionHandler(void)
{
#if defined(HB_OS_WIN)
  {
    LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter(hb_winExceptionHandler);
    HB_SYMBOL_UNUSED(ef);
  }
#elif defined(HB_SIGNAL_EXCEPTION_HANDLER)
  {
    stack_t ss;
    ss.ss_sp = static_cast<void *>(s_signal_stack);
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;
    // set alternative stack for SIGSEGV executed on stack overflow
    if (sigaltstack(&ss, nullptr) == 0) {
      struct sigaction act;
      int sigs[] = {SIGSEGV, SIGILL, SIGFPE, SIGBUS, 0};

      // Ignore SIGPIPEs so they don't kill us.
      signal(SIGPIPE, SIG_IGN);
      for (auto i = 0; sigs[i]; ++i) {
        sigaction(sigs[i], 0, &act);
        act.sa_sigaction = hb_signalExceptionHandler;
        act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESETHAND;
        sigaction(sigs[i], &act, 0);
      }
    }
  }
#endif
}

void hb_vmUnsetExceptionHandler(void)
{
#if defined(HB_SIGNAL_EXCEPTION_HANDLER)
  {
    // we are using static buffer for alternative stack so we do not
    // have to deallocate it to free the memory on application exit
#if 0
      stack_t ss, oss;
      ss.ss_sp = nullptr;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = SS_DISABLE;
      // set alternative stack for SIGSEGV executed on stack overflow
      if( sigaltstack(&ss, &oss) == 0 ) {
         if( oss.ss_sp && SS_DISABLE ) {
            free(oss.ss_sp);
         }
      }
#endif
  }
#endif
}
