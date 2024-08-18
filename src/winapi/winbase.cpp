//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <winbase.h>
#include <fileapi.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINBASEAPI HLOCAL WINAPI LocalAlloc (UINT uFlags, SIZE_T uBytes)
*/
HB_FUNC( WALOCALALLOC )
{
  wa_ret_HLOCAL(LocalAlloc(wa_par_UINT(1), wa_par_SIZE_T(2)));
}

/*
WINBASEAPI HLOCAL WINAPI LocalFree (HLOCAL hMem)
*/
HB_FUNC( WALOCALFREE )
{
  wa_ret_HLOCAL(LocalFree(wa_par_HLOCAL(1)));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalAlloc (UINT uFlags, SIZE_T dwBytes)
*/
HB_FUNC( WAGLOBALALLOC )
{
  wa_ret_HGLOBAL(GlobalAlloc(wa_par_UINT(1), wa_par_SIZE_T(2)));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalReAlloc (HGLOBAL hMem, SIZE_T dwBytes, UINT uFlags)
*/
HB_FUNC( WAGLOBALREALLOC )
{
  wa_ret_HGLOBAL(GlobalReAlloc(wa_par_HGLOBAL(1), wa_par_SIZE_T(2), wa_par_UINT(3)));
}

/*
WINBASEAPI SIZE_T WINAPI GlobalSize (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALSIZE )
{
  wa_ret_SIZE_T(GlobalSize(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI UINT WINAPI GlobalFlags (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALFLAGS )
{
  wa_ret_UINT(GlobalFlags(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI LPVOID WINAPI GlobalLock (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALLOCK )
{
  hb_retptr(GlobalLock(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalHandle (LPCVOID pMem)
*/
HB_FUNC( WAGLOBALHANDLE )
{
  wa_ret_HGLOBAL(GlobalHandle(static_cast<LPCVOID>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI GlobalUnlock (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALUNLOCK )
{
  wa_ret_BOOL(GlobalUnlock(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalFree (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALFREE )
{
  wa_ret_HGLOBAL(GlobalFree(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI SIZE_T WINAPI GlobalCompact (DWORD dwMinFree)
*/
HB_FUNC( WAGLOBALCOMPACT )
{
  wa_ret_SIZE_T(GlobalCompact(wa_par_DWORD(1)));
}

/*
WINBASEAPI VOID WINAPI GlobalFix (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALFIX )
{
  GlobalFix(wa_par_HGLOBAL(1));
}

/*
WINBASEAPI VOID WINAPI GlobalUnfix (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALUNFIX )
{
  GlobalUnfix(wa_par_HGLOBAL(1));
}

/*
WINBASEAPI LPVOID WINAPI GlobalWire (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALWIRE )
{
  hb_retptr(GlobalWire(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GlobalUnWire (HGLOBAL hMem)
*/
HB_FUNC( WAGLOBALUNWIRE )
{
  wa_ret_BOOL(GlobalUnWire(wa_par_HGLOBAL(1)));
}

/*
WINBASEAPI VOID WINAPI GlobalMemoryStatus (LPMEMORYSTATUS lpBuffer)
*/

/*
WINBASEAPI HLOCAL WINAPI LocalReAlloc (HLOCAL hMem, SIZE_T uBytes, UINT uFlags)
*/
HB_FUNC( WALOCALREALLOC )
{
  wa_ret_HLOCAL(LocalReAlloc(wa_par_HLOCAL(1), wa_par_SIZE_T(2), wa_par_UINT(3)));
}

/*
WINBASEAPI LPVOID WINAPI LocalLock (HLOCAL hMem)
*/
HB_FUNC( WALOCALLOCK )
{
  hb_retptr(LocalLock(wa_par_HLOCAL(1)));
}

/*
WINBASEAPI HLOCAL WINAPI LocalHandle (LPCVOID pMem)
*/
HB_FUNC( WALOCALHANDLE )
{
  wa_ret_HLOCAL(LocalHandle(static_cast<LPCVOID>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI LocalUnlock (HLOCAL hMem)
*/
HB_FUNC( WALOCALUNLOCK )
{
  wa_ret_BOOL(LocalUnlock(wa_par_HLOCAL(1)));
}

/*
WINBASEAPI SIZE_T WINAPI LocalSize (HLOCAL hMem)
*/
HB_FUNC( WALOCALSIZE )
{
  wa_ret_SIZE_T(LocalSize(wa_par_HLOCAL(1)));
}

/*
WINBASEAPI UINT WINAPI LocalFlags (HLOCAL hMem)
*/
HB_FUNC( WALOCALFLAGS )
{
  wa_ret_UINT(LocalFlags(wa_par_HLOCAL(1)));
}

/*
WINBASEAPI SIZE_T WINAPI LocalShrink (HLOCAL hMem, UINT cbNewSize)
*/
HB_FUNC( WALOCALSHRINK )
{
  wa_ret_SIZE_T(LocalShrink(wa_par_HLOCAL(1), wa_par_UINT(2)));
}

/*
WINBASEAPI SIZE_T WINAPI LocalCompact (UINT uMinFree)
*/
HB_FUNC( WALOCALCOMPACT )
{
  wa_ret_SIZE_T(LocalCompact(wa_par_UINT(1)));
}

/*
WINBASEAPI LPVOID WINAPI VirtualAllocExNuma (HANDLE hProcess, LPVOID lpAddress, SIZE_T dwSize, DWORD flAllocationType, DWORD flProtect, DWORD nndPreferred)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessorSystemCycleTime (USHORT Group, PSYSTEM_PROCESSOR_CYCLE_TIME_INFORMATION Buffer, PDWORD ReturnedLength)
*/

/*
WINBASEAPI WINBOOL WINAPI GetPhysicallyInstalledSystemMemory (PULONGLONG TotalMemoryInKilobytes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetBinaryTypeA (LPCSTR lpApplicationName, LPDWORD lpBinaryType)
*/
#if 0
HB_FUNC( WAGETBINARYTYPEA )
{
  DWORD BinaryType{};
  wa_ret_BOOL(GetBinaryTypeA(wa_par_LPCSTR(1), &BinaryType));
  wa_stor_DWORD(BinaryType, 2);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetBinaryTypeW (LPCWSTR lpApplicationName, LPDWORD lpBinaryType)
*/
#if 0
HB_FUNC( WAGETBINARYTYPEW )
{
  DWORD BinaryType{};
  wa_ret_BOOL(GetBinaryTypeW(wa_par_LPCWSTR(1), &BinaryType));
  wa_stor_DWORD(BinaryType, 2);
}
#endif

HB_FUNC( WAGETBINARYTYPE )
{
  void * str1{};
  DWORD BinaryType{};
  wa_ret_BOOL(GetBinaryType(HB_PARSTR(1, &str1, nullptr), &BinaryType));
  wa_stor_DWORD(BinaryType, 2);
  hb_strfree(str1);
}

/*
WINBASEAPI DWORD WINAPI GetShortPathNameA (LPCSTR lpszLongPath, LPSTR lpszShortPath, DWORD cchBuffer)
*/
HB_FUNC( WAGETSHORTPATHNAMEA )
{
  wa_ret_DWORD(GetShortPathNameA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI DWORD WINAPI GetLongPathNameTransactedA (LPCSTR lpszShortPath, LPSTR lpszLongPath, DWORD cchBuffer, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetLongPathNameTransactedW (LPCWSTR lpszShortPath, LPWSTR lpszLongPath, DWORD cchBuffer, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessAffinityMask (HANDLE hProcess, PDWORD_PTR lpProcessAffinityMask, PDWORD_PTR lpSystemAffinityMask)
*/

/*
WINBASEAPI WINBOOL WINAPI SetProcessAffinityMask (HANDLE hProcess, DWORD_PTR dwProcessAffinityMask)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessIoCounters (HANDLE hProcess, PIO_COUNTERS lpIoCounters)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessWorkingSetSize (HANDLE hProcess, PSIZE_T lpMinimumWorkingSetSize, PSIZE_T lpMaximumWorkingSetSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetProcessWorkingSetSize (HANDLE hProcess, SIZE_T dwMinimumWorkingSetSize, SIZE_T dwMaximumWorkingSetSize)
*/
HB_FUNC( WASETPROCESSWORKINGSETSIZE )
{
  wa_ret_BOOL(SetProcessWorkingSetSize(wa_par_HANDLE(1), wa_par_SIZE_T(2), wa_par_SIZE_T(3)));
}

/*
WINBASEAPI VOID WINAPI FatalExit (int ExitCode)
*/
HB_FUNC( WAFATALEXIT )
{
  FatalExit(wa_par_int(1));
}

/*
WINBASEAPI WINBOOL WINAPI SetEnvironmentStringsA (LPCH NewEnvironment)
*/

/*
WINBASEAPI VOID WINAPI RaiseFailFastException (PEXCEPTION_RECORD pExceptionRecord, PCONTEXT pContextRecord, DWORD dwFlags)
*/

/*
WINBASEAPI DWORD WINAPI SetThreadIdealProcessor (HANDLE hThread, DWORD dwIdealProcessor)
*/
HB_FUNC( WASETTHREADIDEALPROCESSOR )
{
  wa_ret_DWORD(SetThreadIdealProcessor(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINBASEAPI LPVOID WINAPI CreateFiber (SIZE_T dwStackSize, LPFIBER_START_ROUTINE lpStartAddress, LPVOID lpParameter)
*/

/*
WINBASEAPI LPVOID WINAPI CreateFiberEx (SIZE_T dwStackCommitSize, SIZE_T dwStackReserveSize, DWORD dwFlags, LPFIBER_START_ROUTINE lpStartAddress, LPVOID lpParameter)
*/

/*
WINBASEAPI VOID WINAPI DeleteFiber (LPVOID lpFiber)
*/
HB_FUNC( WADELETEFIBER )
{
  DeleteFiber(static_cast<LPVOID>(hb_parptr(1)));
}

/*
WINBASEAPI LPVOID WINAPI ConvertThreadToFiber (LPVOID lpParameter)
*/
HB_FUNC( WACONVERTTHREADTOFIBER )
{
  hb_retptr(ConvertThreadToFiber(static_cast<LPVOID>(hb_parptr(1))));
}

/*
WINBASEAPI LPVOID WINAPI ConvertThreadToFiberEx (LPVOID lpParameter, DWORD dwFlags)
*/
HB_FUNC( WACONVERTTHREADTOFIBEREX )
{
  hb_retptr(ConvertThreadToFiberEx(static_cast<LPVOID>(hb_parptr(1)), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI ConvertFiberToThread (VOID)
*/
HB_FUNC( WACONVERTFIBERTOTHREAD )
{
  wa_ret_BOOL(ConvertFiberToThread());
}

/*
WINBASEAPI VOID WINAPI SwitchToFiber (LPVOID lpFiber)
*/
HB_FUNC( WASWITCHTOFIBER )
{
  SwitchToFiber(static_cast<LPVOID>(hb_parptr(1)));
}

/*
WINBASEAPI DWORD_PTR WINAPI SetThreadAffinityMask (HANDLE hThread, DWORD_PTR dwThreadAffinityMask)
*/

/*
WINBASEAPI WINBOOL WINAPI GetThreadInformation (HANDLE hThread, THREAD_INFORMATION_CLASS ThreadInformationClass, LPVOID ThreadInformation, DWORD ThreadInformationSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetThreadInformation (HANDLE hThread, THREAD_INFORMATION_CLASS ThreadInformationClass, LPVOID ThreadInformation, DWORD ThreadInformationSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessInformation (HANDLE hProcess, PROCESS_INFORMATION_CLASS ProcessInformationClass, LPVOID ProcessInformation, DWORD ProcessInformationSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetProcessInformation (HANDLE hProcess, PROCESS_INFORMATION_CLASS ProcessInformationClass, LPVOID ProcessInformation, DWORD ProcessInformationSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetProcessDEPPolicy (DWORD dwFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessDEPPolicy (HANDLE hProcess, LPDWORD lpFlags, PBOOL lpPermanent)
*/
#if 0
HB_FUNC( WAGETPROCESSDEPPOLICY )
{
  DWORD Flags{};
  BOOL Permanent{};
  wa_ret_BOOL(GetProcessDEPPolicy(wa_par_HANDLE(1), &Flags, &Permanent));
  wa_stor_DWORD(Flags, 2);
  wa_stor_BOOL(Permanent, 3);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetProcessPriorityBoost (HANDLE hProcess, WINBOOL bDisablePriorityBoost)
*/
HB_FUNC( WASETPROCESSPRIORITYBOOST )
{
  wa_ret_BOOL(SetProcessPriorityBoost(wa_par_HANDLE(1), wa_par_BOOL(2)));
}

/*
WINBASEAPI WINBOOL WINAPI GetProcessPriorityBoost (HANDLE hProcess, PBOOL pDisablePriorityBoost)
*/

/*
WINBASEAPI WINBOOL WINAPI RequestWakeupLatency (LATENCY_TIME latency)
*/

/*
WINBASEAPI WINBOOL WINAPI IsSystemResumeAutomatic (VOID)
*/
HB_FUNC( WAISSYSTEMRESUMEAUTOMATIC )
{
  wa_ret_BOOL(IsSystemResumeAutomatic());
}

/*
WINBASEAPI WINBOOL WINAPI GetThreadIOPendingFlag (HANDLE hThread, PBOOL lpIOIsPending)
*/

/*
WINBASEAPI WINBOOL WINAPI GetThreadSelectorEntry (HANDLE hThread, DWORD dwSelector, LPLDT_ENTRY lpSelectorEntry)
*/

/*
WINBASEAPI EXECUTION_STATE WINAPI SetThreadExecutionState (EXECUTION_STATE esFlags)
*/

/*
WINBASEAPI HANDLE WINAPI PowerCreateRequest (PREASON_CONTEXT Context)
*/

/*
WINBASEAPI WINBOOL WINAPI PowerSetRequest (HANDLE PowerRequest, POWER_REQUEST_TYPE RequestType)
*/

/*
WINBASEAPI WINBOOL WINAPI PowerClearRequest (HANDLE PowerRequest, POWER_REQUEST_TYPE RequestType)
*/

/*
WINBASEAPI VOID WINAPI RestoreLastError (DWORD dwErrCode)
*/
#if 0
HB_FUNC( WARESTORELASTERROR )
{
  RestoreLastError(wa_par_DWORD(1));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFileCompletionNotificationModes (HANDLE FileHandle, UCHAR Flags)
*/
#if 0
HB_FUNC( WASETFILECOMPLETIONNOTIFICATIONMODES )
{
  wa_ret_BOOL(SetFileCompletionNotificationModes(wa_par_HANDLE(1), wa_par_UCHAR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFileIoOverlappedRange (HANDLE FileHandle, PUCHAR OverlappedRangeStart, ULONG Length)
*/
#if 0
HB_FUNC( WASetFileIoOverlappedRange )
{
  UCHAR OverlappedRangeStart{};
  wa_ret_BOOL(SetFileIoOverlappedRange(wa_par_HANDLE(1), &OverlappedRangeStart, wa_par_ULONG(3)));
  wa_stor_UCHAR(OverlappedRangeStart, 2);
}
#endif

/*
WINBASEAPI DWORD WINAPI GetThreadErrorMode (VOID)
*/
HB_FUNC( WAGETTHREADERRORMODE )
{
  wa_ret_DWORD(GetThreadErrorMode());
}

/*
WINBASEAPI WINBOOL WINAPI SetThreadErrorMode (DWORD dwNewMode, LPDWORD lpOldMode)
*/
HB_FUNC( WASETTHREADERRORMODE )
{
  DWORD OldMode{};
  wa_ret_BOOL(SetThreadErrorMode(wa_par_DWORD(1), &OldMode));
  wa_stor_DWORD(OldMode, 2);
}

/*
WINBASEAPI WINBOOL WINAPI Wow64GetThreadContext (HANDLE hThread, PWOW64_CONTEXT lpContext)
*/

/*
WINBASEAPI WINBOOL WINAPI Wow64SetThreadContext (HANDLE hThread, CONST WOW64_CONTEXT *lpContext)
*/

/*
WINBASEAPI WINBOOL WINAPI Wow64GetThreadSelectorEntry (HANDLE hThread, DWORD dwSelector, PWOW64_LDT_ENTRY lpSelectorEntry)
*/

/*
WINBASEAPI DWORD WINAPI Wow64SuspendThread (HANDLE hThread)
*/

/*
WINBASEAPI WINBOOL WINAPI DebugSetProcessKillOnExit (WINBOOL KillOnExit)
*/
HB_FUNC( WADEBUGSETPROCESSKILLONEXIT )
{
  wa_ret_BOOL(DebugSetProcessKillOnExit(wa_par_BOOL(1)));
}

/*
WINBASEAPI WINBOOL WINAPI DebugBreakProcess (HANDLE Process)
*/
HB_FUNC( WADEBUGBREAKPROCESS )
{
  wa_ret_BOOL(DebugBreakProcess(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI PulseEvent (HANDLE hEvent)
*/
HB_FUNC( WAPULSEEVENT )
{
  wa_ret_BOOL(PulseEvent(wa_par_HANDLE(1)));
}

/*
WINBASEAPI DWORD WINAPI WaitForMultipleObjects (DWORD nCount, CONST HANDLE *lpHandles, WINBOOL bWaitAll, DWORD dwMilliseconds)
*/

/*
WINBASEAPI ATOM WINAPI GlobalDeleteAtom (ATOM nAtom)
*/
HB_FUNC( WAGLOBALDELETEATOM )
{
  wa_ret_ATOM(GlobalDeleteAtom(wa_par_ATOM(1)));
}

/*
WINBASEAPI WINBOOL WINAPI InitAtomTable (DWORD nSize)
*/
HB_FUNC( WAINITATOMTABLE )
{
  wa_ret_BOOL(InitAtomTable(wa_par_DWORD(1)));
}

/*
WINBASEAPI ATOM WINAPI DeleteAtom (ATOM nAtom)
*/
HB_FUNC( WADELETEATOM )
{
  wa_ret_ATOM(DeleteAtom(wa_par_ATOM(1)));
}

/*
WINBASEAPI UINT WINAPI SetHandleCount (UINT uNumber)
*/
HB_FUNC( WASETHANDLECOUNT )
{
  wa_ret_UINT(SetHandleCount(wa_par_UINT(1)));
}

/*
WINBASEAPI WINBOOL WINAPI RequestDeviceWakeup (HANDLE hDevice)
*/
HB_FUNC( WAREQUESTDEVICEWAKEUP )
{
  wa_ret_BOOL(RequestDeviceWakeup(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI CancelDeviceWakeupRequest (HANDLE hDevice)
*/
HB_FUNC( WACANCELDEVICEWAKEUPREQUEST )
{
  wa_ret_BOOL(CancelDeviceWakeupRequest(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetDevicePowerState (HANDLE hDevice, WINBOOL *pfOn)
*/

/*
WINBASEAPI WINBOOL WINAPI SetMessageWaitingIndicator (HANDLE hMsgIndicator, ULONG ulMsgCount)
*/
HB_FUNC( WASETMESSAGEWAITINGINDICATOR )
{
  wa_ret_BOOL(SetMessageWaitingIndicator(wa_par_HANDLE(1), wa_par_ULONG(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetFileShortNameA (HANDLE hFile, LPCSTR lpShortName)
*/
#if 0
HB_FUNC( WASETFILESHORTNAMEA )
{
  wa_ret_BOOL(SetFileShortNameA(wa_par_HANDLE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFileShortNameW (HANDLE hFile, LPCWSTR lpShortName)
*/
#if 0
HB_FUNC( WASETFILESHORTNAMEW )
{
  wa_ret_BOOL(SetFileShortNameW(wa_par_HANDLE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WASETFILESHORTNAME )
{
  void * str2{};
  wa_ret_BOOL(SetFileShortName(wa_par_HANDLE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINBASEAPI DWORD WINAPI LoadModule (LPCSTR lpModuleName, LPVOID lpParameterBlock)
*/
HB_FUNC( WALOADMODULE )
{
  wa_ret_DWORD(LoadModule(wa_par_LPCSTR(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINBASEAPI UINT WINAPI WinExec (LPCSTR lpCmdLine, UINT uCmdShow)
*/
HB_FUNC( WAWINEXEC )
{
  wa_ret_UINT(WinExec(wa_par_LPCSTR(1), wa_par_UINT(2)));
}

/*
WINBASEAPI WINBOOL WINAPI ClearCommBreak (HANDLE hFile)
*/
HB_FUNC( WACLEARCOMMBREAK )
{
  wa_ret_BOOL(ClearCommBreak(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI ClearCommError (HANDLE hFile, LPDWORD lpErrors, LPCOMSTAT lpStat)
*/
HB_FUNC( WACLEARCOMMERROR )
{
  DWORD Errors{};
  wa_ret_BOOL(ClearCommError(wa_par_HANDLE(1), &Errors, wa_par_COMSTAT(3)));
  wa_stor_DWORD(Errors, 2);
}

/*
WINBASEAPI WINBOOL WINAPI SetupComm (HANDLE hFile, DWORD dwInQueue, DWORD dwOutQueue)
*/
HB_FUNC( WASETUPCOMM )
{
  wa_ret_BOOL(SetupComm(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI EscapeCommFunction (HANDLE hFile, DWORD dwFunc)
*/
HB_FUNC( WAESCAPECOMMFUNCTION )
{
  wa_ret_BOOL(EscapeCommFunction(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommConfig(HANDLE hCommDev, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
BOOL GetCommConfig([in] HANDLE hCommDev, [out] LPCOMMCONFIG lpCC, [in, out] LPDWORD lpdwSize)
*/
#if 0
HB_FUNC( WAGETCOMMCONFIG )
{
  DWORD dwSize = wa_par_DWORD(3);
  wa_ret_BOOL(GetCommConfig(wa_par_HANDLE(1), ###, &dwSize));
  wa_stor_DWORD(dwSize, 3);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetCommMask (HANDLE hFile, LPDWORD lpEvtMask)
*/
HB_FUNC( WAGETCOMMMASK )
{
  DWORD EvtMask{};
  wa_ret_BOOL(GetCommMask(wa_par_HANDLE(1), &EvtMask));
  wa_stor_DWORD(EvtMask, 2);
}

/*
WINBASEAPI WINBOOL WINAPI GetCommProperties (HANDLE hFile, LPCOMMPROP lpCommProp)
*/
#if 0
HB_FUNC( WAGETCOMMPROPERTIES )
{
  wa_ret_BOOL(GetCommProperties(wa_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetCommModemStatus (HANDLE hFile, LPDWORD lpModemStat)
*/
HB_FUNC( WAGETCOMMMODEMSTATUS )
{
  DWORD ModemStat{};
  wa_ret_BOOL(GetCommModemStatus(wa_par_HANDLE(1), &ModemStat));
  wa_stor_DWORD(ModemStat, 2);
}

/*
WINBASEAPI WINBOOL WINAPI GetCommState (HANDLE hFile, LPDCB lpDCB)
*/
HB_FUNC( WAGETCOMMSTATE )
{
  wa_ret_BOOL(GetCommState(wa_par_HANDLE(1), wa_par_DCB(2)));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommTimeouts (HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/
#if 0
HB_FUNC( WAGETCOMMTIMEOUTS )
{
  wa_ret_BOOL(GetCommTimeouts(wa_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI PurgeComm (HANDLE hFile, DWORD dwFlags)
*/
HB_FUNC( WAPURGECOMM )
{
  wa_ret_BOOL(PurgeComm(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommBreak (HANDLE hFile)
*/
HB_FUNC( WASETCOMMBREAK )
{
  wa_ret_BOOL(SetCommBreak(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommConfig (HANDLE hCommDev, LPCOMMCONFIG lpCC, DWORD dwSize)
*/
#if 0
HB_FUNC( WASETCOMMCONFIG )
{
  wa_ret_BOOL(SetCommConfig(wa_par_HANDLE(1), ###, wa_par_DWORD(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetCommMask (HANDLE hFile, DWORD dwEvtMask)
*/
HB_FUNC( WASETCOMMMASK )
{
  wa_ret_BOOL(SetCommMask(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommState (HANDLE hFile, LPDCB lpDCB)
*/
HB_FUNC( WASETCOMMSTATE )
{
  wa_ret_BOOL(SetCommState(wa_par_HANDLE(1), wa_par_DCB(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommTimeouts (HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/
#if 0
HB_FUNC( WASETCOMMTIMEOUTS )
{
  wa_ret_BOOL(SetCommTimeouts(wa_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI TransmitCommChar (HANDLE hFile, char cChar)
*/
HB_FUNC( WATRANSMITCOMMCHAR )
{
  wa_ret_BOOL(TransmitCommChar(wa_par_HANDLE(1), wa_par_char(2)));
}

/*
WINBASEAPI WINBOOL WINAPI WaitCommEvent (HANDLE hFile, LPDWORD lpEvtMask, LPOVERLAPPED lpOverlapped)
*/
#if 0
HB_FUNC( WAWAITCOMMEVENT )
{
  DWORD EvtMask{};
  wa_ret_BOOL(WaitCommEvent(wa_par_HANDLE(1), &EvtMask, ###));
  wa_stor_DWORD(EvtMask, 3);
}
#endif

/*
WINBASEAPI DWORD WINAPI SetTapePosition (HANDLE hDevice, DWORD dwPositionMethod, DWORD dwPartition, DWORD dwOffsetLow, DWORD dwOffsetHigh, WINBOOL bImmediate)
*/
HB_FUNC( WASETTAPEPOSITION )
{
  wa_ret_DWORD(SetTapePosition(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4), wa_par_DWORD(5), wa_par_BOOL(6)));
}

/*
WINBASEAPI DWORD WINAPI GetTapePosition (HANDLE hDevice, DWORD dwPositionType, LPDWORD lpdwPartition, LPDWORD lpdwOffsetLow, LPDWORD lpdwOffsetHigh)
*/
HB_FUNC( WAGETTAPEPOSITION )
{
  DWORD dwPartition{};
  DWORD dwOffsetLow{};
  DWORD dwOffsetHigh{};
  wa_ret_DWORD(GetTapePosition(wa_par_HANDLE(1), wa_par_DWORD(2), &dwPartition, &dwOffsetLow, &dwOffsetHigh));
  wa_stor_DWORD(dwPartition, 3);
  wa_stor_DWORD(dwOffsetLow, 4);
  wa_stor_DWORD(dwOffsetHigh, 5);
}

/*
WINBASEAPI DWORD WINAPI PrepareTape (HANDLE hDevice, DWORD dwOperation, WINBOOL bImmediate)
*/
HB_FUNC( WAPREPARETAPE )
{
  wa_ret_DWORD(PrepareTape(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}

/*
WINBASEAPI DWORD WINAPI EraseTape (HANDLE hDevice, DWORD dwEraseType, WINBOOL bImmediate)
*/
HB_FUNC( WAERASETAPE )
{
  wa_ret_DWORD(EraseTape(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}

/*
WINBASEAPI DWORD WINAPI CreateTapePartition (HANDLE hDevice, DWORD dwPartitionMethod, DWORD dwCount, DWORD dwSize)
*/
HB_FUNC( WACREATETAPEPARTITION )
{
  wa_ret_DWORD(CreateTapePartition(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4)));
}

/*
WINBASEAPI DWORD WINAPI WriteTapemark (HANDLE hDevice, DWORD dwTapemarkType, DWORD dwTapemarkCount, WINBOOL bImmediate)
*/
HB_FUNC( WAWRITETAPEMARK )
{
  wa_ret_DWORD(WriteTapemark(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_BOOL(4)));
}

/*
WINBASEAPI DWORD WINAPI GetTapeStatus (HANDLE hDevice)
*/
HB_FUNC( WAGETTAPESTATUS )
{
  wa_ret_DWORD(GetTapeStatus(wa_par_HANDLE(1)));
}

/*
WINBASEAPI DWORD WINAPI GetTapeParameters (HANDLE hDevice, DWORD dwOperation, LPDWORD lpdwSize, LPVOID lpTapeInformation)
*/
HB_FUNC( WAGETTAPEPARAMETERS )
{
  DWORD dwSize{};
  wa_ret_DWORD(GetTapeParameters(wa_par_HANDLE(1), wa_par_DWORD(2), &dwSize, static_cast<LPVOID>(hb_parptr(4))));
  wa_stor_DWORD(dwSize, 3);
}

/*
WINBASEAPI DWORD WINAPI SetTapeParameters (HANDLE hDevice, DWORD dwOperation, LPVOID lpTapeInformation)
*/
HB_FUNC( WASETTAPEPARAMETERS )
{
  wa_ret_DWORD(SetTapeParameters(wa_par_HANDLE(1), wa_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINBASEAPI DEP_SYSTEM_POLICY_TYPE WINAPI GetSystemDEPPolicy (VOID)
*/

/*
WINBASEAPI WINBOOL WINAPI GetSystemRegistryQuota (PDWORD pdwQuotaAllowed, PDWORD pdwQuotaUsed)
*/

/*
WINBOOL WINAPI GetSystemTimes (LPFILETIME lpIdleTime, LPFILETIME lpKernelTime, LPFILETIME lpUserTime)
*/
HB_FUNC( WAGETSYSTEMTIMES )
{
  wa_ret_BOOL(GetSystemTimes(wa_par_FILETIME(1), wa_par_FILETIME(2), wa_par_FILETIME(3)));
}

/*
WINBASEAPI WINBOOL WINAPI FileTimeToDosDateTime (CONST FILETIME *lpFileTime, LPWORD lpFatDate, LPWORD lpFatTime)
*/
HB_FUNC( WAFILETIMETODOSDATETIME )
{
  WORD FatDate{};
  WORD FatTime{};
  wa_ret_BOOL(FileTimeToDosDateTime(wa_par_FILETIME(1), &FatDate, &FatTime));
  wa_stor_WORD(FatDate, 2);
  wa_stor_WORD(FatTime, 3);
}

/*
WINBASEAPI WINBOOL WINAPI DosDateTimeToFileTime (WORD wFatDate, WORD wFatTime, LPFILETIME lpFileTime)
*/
HB_FUNC( WADOSDATETIMETOFILETIME )
{
  wa_ret_BOOL(DosDateTimeToFileTime(wa_par_WORD(1), wa_par_WORD(2), wa_par_FILETIME(3)));
}

/*
WINBASEAPI WINBOOL WINAPI SetSystemTimeAdjustment (DWORD dwTimeAdjustment, WINBOOL bTimeAdjustmentDisabled)
*/
HB_FUNC( WASETSYSTEMTIMEADJUSTMENT )
{
  wa_ret_BOOL(SetSystemTimeAdjustment(wa_par_DWORD(1), wa_par_BOOL(2)));
}

/*
WINBASEAPI int WINAPI MulDiv (int nNumber, int nNumerator, int nDenominator)
*/
HB_FUNC( WAMULDIV )
{
  wa_ret_int(MulDiv(wa_par_int(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINBASEAPI DWORD WINAPI FormatMessageA (DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, LPSTR lpBuffer, DWORD nSize, va_list *Arguments)
*/

/*
WINBASEAPI DWORD WINAPI FormatMessageW (DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, LPWSTR lpBuffer, DWORD nSize, va_list *Arguments)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeInfo (HANDLE hNamedPipe, LPDWORD lpFlags, LPDWORD lpOutBufferSize, LPDWORD lpInBufferSize, LPDWORD lpMaxInstances)
*/
HB_FUNC( WAGETNAMEDPIPEINFO )
{
  DWORD Flags{};
  DWORD OutBufferSize{};
  DWORD InBufferSize{};
  DWORD MaxInstances{};
  wa_ret_BOOL(GetNamedPipeInfo(wa_par_HANDLE(1), &Flags, &OutBufferSize, &InBufferSize, &MaxInstances));
  wa_stor_DWORD(Flags, 2);
  wa_stor_DWORD(OutBufferSize, 3);
  wa_stor_DWORD(InBufferSize, 4);
  wa_stor_DWORD(MaxInstances, 5);
}

/*
WINBASEAPI HANDLE WINAPI CreateMailslotA (LPCSTR lpName, DWORD nMaxMessageSize, DWORD lReadTimeout, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI HANDLE WINAPI CreateMailslotW (LPCWSTR lpName, DWORD nMaxMessageSize, DWORD lReadTimeout, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetMailslotInfo (HANDLE hMailslot, LPDWORD lpMaxMessageSize, LPDWORD lpNextSize, LPDWORD lpMessageCount, LPDWORD lpReadTimeout)
*/
HB_FUNC( WAGETMAILSLOTINFO )
{
  DWORD MaxMessageSize{};
  DWORD NextSize{};
  DWORD MessageCount{};
  DWORD ReadTimeout{};
  wa_ret_BOOL(GetMailslotInfo(wa_par_HANDLE(1), &MaxMessageSize, &NextSize, &MessageCount, &ReadTimeout));
  wa_stor_DWORD(MaxMessageSize, 2);
  wa_stor_DWORD(NextSize, 3);
  wa_stor_DWORD(MessageCount, 4);
  wa_stor_DWORD(ReadTimeout, 5);
}

/*
WINBASEAPI WINBOOL WINAPI SetMailslotInfo (HANDLE hMailslot, DWORD lReadTimeout)
*/
HB_FUNC( WASETMAILSLOTINFO )
{
  wa_ret_BOOL(SetMailslotInfo(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINADVAPI WINBOOL WINAPI EncryptFileA (LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WAENCRYPTFILEA )
{
  wa_ret_BOOL(EncryptFileA(wa_par_LPCSTR(1)));
}
#endif

/*
WINADVAPI WINBOOL WINAPI EncryptFileW (LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WAENCRYPTFILEW )
{
  wa_ret_BOOL(EncryptFileW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAENCRYPTFILE )
{
  void * str1{};
  wa_ret_BOOL(EncryptFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINADVAPI WINBOOL WINAPI DecryptFileA (LPCSTR lpFileName, DWORD dwReserved)
*/
#if 0
HB_FUNC( WADECRYPTFILEA )
{
  wa_ret_BOOL(DecryptFileA(wa_par_LPCSTR(1), wa_par_DWORD(2)));
}
#endif

/*
WINADVAPI WINBOOL WINAPI DecryptFileW (LPCWSTR lpFileName, DWORD dwReserved)
*/
#if 0
HB_FUNC( WADECRYPTFILEW )
{
  wa_ret_BOOL(DecryptFileW(wa_par_LPCWSTR(1), wa_par_DWORD(2)));
}
#endif

HB_FUNC( WADECRYPTFILE )
{
  void * str1{};
  wa_ret_BOOL(DecryptFile(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2)));
  hb_strfree(str1);
}

/*
WINADVAPI WINBOOL WINAPI FileEncryptionStatusA (LPCSTR lpFileName, LPDWORD lpStatus)
*/
#if 0
HB_FUNC( WAFILEENCRYPTIONSTATUSA )
{
  DWORD Status{};
  wa_ret_BOOL(FileEncryptionStatusA(wa_par_LPCSTR(1), &Status));
  wa_stor_DWORD(Status, 2);
}
#endif

/*
WINADVAPI WINBOOL WINAPI FileEncryptionStatusW (LPCWSTR lpFileName, LPDWORD lpStatus)
*/
#if 0
HB_FUNC( WAFILEENCRYPTIONSTATUSW )
{
  DWORD Status{};
  wa_ret_BOOL(FileEncryptionStatusW(wa_par_LPCWSTR(1), &Status));
  wa_stor_DWORD(Status, 2);
}
#endif

HB_FUNC( WAFILEENCRYPTIONSTATUS )
{
  void * str1{};
  DWORD Status{};
  wa_ret_BOOL(FileEncryptionStatus(HB_PARSTR(1, &str1, nullptr), &Status));
  wa_stor_DWORD(Status, 2);
  hb_strfree(str1);
}

/*
WINADVAPI DWORD WINAPI OpenEncryptedFileRawA (LPCSTR lpFileName, ULONG ulFlags, PVOID *pvContext)
*/

/*
WINADVAPI DWORD WINAPI OpenEncryptedFileRawW (LPCWSTR lpFileName, ULONG ulFlags, PVOID *pvContext)
*/

/*
WINADVAPI DWORD WINAPI ReadEncryptedFileRaw (PFE_EXPORT_FUNC pfExportCallback, PVOID pvCallbackContext, PVOID pvContext)
*/

/*
WINADVAPI DWORD WINAPI WriteEncryptedFileRaw (PFE_IMPORT_FUNC pfImportCallback, PVOID pvCallbackContext, PVOID pvContext)
*/

/*
WINADVAPI VOID WINAPI CloseEncryptedFileRaw (PVOID pvContext)
*/
HB_FUNC( WACLOSEENCRYPTEDFILERAW )
{
  CloseEncryptedFileRaw(static_cast<PVOID>(hb_parptr(1)));
}

/*
WINBASEAPI int WINAPI lstrcmpA (LPCSTR lpString1, LPCSTR lpString2)
*/
#if 0
HB_FUNC( WALSTRCMPA )
{
  wa_ret_int(lstrcmpA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI int WINAPI lstrcmpW (LPCWSTR lpString1, LPCWSTR lpString2)
*/
#if 0
HB_FUNC( WALSTRCMPW )
{
  wa_ret_int(lstrcmpW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALSTRCMP )
{
  void * str1{};
  void * str2{};
  wa_ret_int(lstrcmp(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI int WINAPI lstrcmpiA (LPCSTR lpString1, LPCSTR lpString2)
*/
#if 0
HB_FUNC( WALSTRCMPIA )
{
  wa_ret_int(lstrcmpiA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI int WINAPI lstrcmpiW (LPCWSTR lpString1, LPCWSTR lpString2)
*/
#if 0
HB_FUNC( WALSTRCMPIW )
{
  wa_ret_int(lstrcmpiW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALSTRCMPI )
{
  void * str1{};
  void * str2{};
  wa_ret_int(lstrcmpi(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI LPSTR WINAPI lstrcpynA (LPSTR lpString1, LPCSTR lpString2, int iMaxLength)
*/
HB_FUNC( WALSTRCPYNA )
{
  hb_retc(( LPSTR ) lstrcpynA(const_cast<LPSTR>(hb_parc(1)), wa_par_LPCSTR(2), wa_par_int(3)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcpynW (LPWSTR lpString1, LPCWSTR lpString2, int iMaxLength)
*/

/*
WINBASEAPI LPSTR WINAPI lstrcpyA (LPSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WALSTRCPYA )
{
  hb_retc(( LPSTR ) lstrcpyA(const_cast<LPSTR>(hb_parc(1)), wa_par_LPCSTR(2)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcpyW (LPWSTR lpString1, LPCWSTR lpString2)
*/

/*
WINBASEAPI LPSTR WINAPI lstrcatA (LPSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WALSTRCATA )
{
  hb_retc(( LPSTR ) lstrcatA(const_cast<LPSTR>(hb_parc(1)), wa_par_LPCSTR(2)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcatW (LPWSTR lpString1, LPCWSTR lpString2)
*/

/*
WINBASEAPI int WINAPI lstrlenA (LPCSTR lpString)
*/
#if 0
HB_FUNC( WALSTRLENA )
{
  wa_ret_int(lstrlenA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI int WINAPI lstrlenW (LPCWSTR lpString)
*/
#if 0
HB_FUNC( WALSTRLENW )
{
  wa_ret_int(lstrlenW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WALSTRLEN )
{
  void * str1{};
  wa_ret_int(lstrlen(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI HFILE WINAPI OpenFile (LPCSTR lpFileName, LPOFSTRUCT lpReOpenBuff, UINT uStyle)
*/

/*
WINBASEAPI HFILE WINAPI _lopen (LPCSTR lpPathName, int iReadWrite)
*/
HB_FUNC( WA_LOPEN )
{
  wa_ret_HFILE(_lopen(wa_par_LPCSTR(1), wa_par_int(2)));
}

/*
WINBASEAPI HFILE WINAPI _lcreat (LPCSTR lpPathName, int iAttribute)
*/
HB_FUNC( WA_LCREAT )
{
  wa_ret_HFILE(_lcreat(wa_par_LPCSTR(1), wa_par_int(2)));
}

/*
WINBASEAPI UINT WINAPI _lread (HFILE hFile, LPVOID lpBuffer, UINT uBytes)
*/
HB_FUNC( WA_LREAD )
{
  wa_ret_UINT(_lread(wa_par_HFILE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_UINT(3)));
}

/*
WINBASEAPI UINT WINAPI _lwrite (HFILE hFile, LPCCH lpBuffer, UINT uBytes)
*/

/*
WINBASEAPI __LONG32 WINAPI _hread (HFILE hFile, LPVOID lpBuffer, __LONG32 lBytes)
*/

/*
WINBASEAPI __LONG32 WINAPI _hwrite (HFILE hFile, LPCCH lpBuffer, __LONG32 lBytes)
*/

/*
WINBASEAPI HFILE WINAPI _lclose (HFILE hFile)
*/
HB_FUNC( WA_LCLOSE )
{
  wa_ret_HFILE(_lclose(wa_par_HFILE(1)));
}

/*
WINBASEAPI LONG WINAPI _llseek (HFILE hFile, LONG lOffset, int iOrigin)
*/
HB_FUNC( WA_LLSEEK )
{
  wa_ret_LONG(_llseek(wa_par_HFILE(1), wa_par_LONG(2), wa_par_int(3)));
}

/*
WINADVAPI WINBOOL WINAPI IsTextUnicode (CONST VOID *lpv, int iSize, LPINT lpiResult)
*/
HB_FUNC( WAISTEXTUNICODE )
{
  INT iResult{};
  wa_ret_BOOL(IsTextUnicode(static_cast<CONST VOID*>(hb_parptr(1)), wa_par_int(2), &iResult));
  wa_stor_INT(iResult, 3);
}

/*
WINBASEAPI DWORD WINAPI SignalObjectAndWait (HANDLE hObjectToSignal, HANDLE hObjectToWaitOn, DWORD dwMilliseconds, WINBOOL bAlertable)
*/
HB_FUNC( WASIGNALOBJECTANDWAIT )
{
  wa_ret_DWORD(SignalObjectAndWait(wa_par_HANDLE(1), wa_par_HANDLE(2), wa_par_DWORD(3), wa_par_BOOL(4)));
}

/*
WINBASEAPI WINBOOL WINAPI BackupRead (HANDLE hFile, LPBYTE lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, WINBOOL bAbort, WINBOOL bProcessSecurity, LPVOID *lpContext)
*/

/*
WINBASEAPI WINBOOL WINAPI BackupSeek (HANDLE hFile, DWORD dwLowBytesToSeek, DWORD dwHighBytesToSeek, LPDWORD lpdwLowByteSeeked, LPDWORD lpdwHighByteSeeked, LPVOID *lpContext)
*/

/*
WINBASEAPI WINBOOL WINAPI BackupWrite (HANDLE hFile, LPBYTE lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, WINBOOL bAbort, WINBOOL bProcessSecurity, LPVOID *lpContext)
*/

/*
WINBASEAPI HANDLE WINAPI CreateSemaphoreW (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, LPCWSTR lpName)
*/

/*
WINBASEAPI HMODULE WINAPI LoadLibraryA (LPCSTR lpLibFileName)
*/
#if 0
HB_FUNC( WALOADLIBRARYA )
{
  wa_ret_HMODULE(LoadLibraryA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI HMODULE WINAPI LoadLibraryW (LPCWSTR lpLibFileName)
*/
#if 0
HB_FUNC( WALOADLIBRARYW )
{
  wa_ret_HMODULE(LoadLibraryW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WALOADLIBRARY )
{
  void * str1{};
  wa_ret_HMODULE(LoadLibrary(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI HANDLE WINAPI OpenMutexA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
HB_FUNC( WAOPENMUTEXA )
{
  wa_ret_HANDLE(OpenMutexA(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI HANDLE WINAPI CreateSemaphoreA (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, LPCSTR lpName)
*/

/*
WINBASEAPI HANDLE WINAPI OpenSemaphoreA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
HB_FUNC( WAOPENSEMAPHOREA )
{
  wa_ret_HANDLE(OpenSemaphoreA(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI HANDLE WINAPI CreateWaitableTimerA (LPSECURITY_ATTRIBUTES lpTimerAttributes, WINBOOL bManualReset, LPCSTR lpTimerName)
*/

/*
WINBASEAPI HANDLE WINAPI CreateWaitableTimerW (LPSECURITY_ATTRIBUTES lpTimerAttributes, WINBOOL bManualReset, LPCWSTR lpTimerName)
*/

/*
WINBASEAPI HANDLE WINAPI OpenWaitableTimerA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpTimerName)
*/
HB_FUNC( WAOPENWAITABLETIMERA )
{
  wa_ret_HANDLE(OpenWaitableTimerA(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI HANDLE WINAPI CreateFileMappingA (HANDLE hFile, LPSECURITY_ATTRIBUTES lpFileMappingAttributes, DWORD flProtect, DWORD dwMaximumSizeHigh, DWORD dwMaximumSizeLow, LPCSTR lpName)
*/

/*
WINBASEAPI HANDLE WINAPI CreateSemaphoreExA (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, LPCSTR lpName, DWORD dwFlags, DWORD dwDesiredAccess)
*/

/*
WINBASEAPI HANDLE WINAPI CreateWaitableTimerExA (LPSECURITY_ATTRIBUTES lpTimerAttributes, LPCSTR lpTimerName, DWORD dwFlags, DWORD dwDesiredAccess)
*/

/*
WINBASEAPI HANDLE WINAPI CreateFileMappingNumaA (HANDLE hFile, LPSECURITY_ATTRIBUTES lpFileMappingAttributes, DWORD flProtect, DWORD dwMaximumSizeHigh, DWORD dwMaximumSizeLow, LPCSTR lpName, DWORD nndPreferred)
*/

/*
WINBASEAPI HANDLE WINAPI OpenFileMappingA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
HB_FUNC( WAOPENFILEMAPPINGA )
{
  wa_ret_HANDLE(OpenFileMappingA(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WAGETLOGICALDRIVESTRINGSA )
{
  wa_ret_DWORD(GetLogicalDriveStringsA(wa_par_DWORD(1), const_cast<LPSTR>(hb_parc(2))));
}


/*
WINBASEAPI HMODULE WINAPI LoadPackagedLibrary (LPCWSTR lpwLibFileName, DWORD Reserved)
*/
#if 0
HB_FUNC( WALOADPACKAGEDLIBRARY )
{
  wa_ret_HMODULE(LoadPackagedLibrary(wa_par_LPCWSTR(1), wa_par_DWORD(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI QueryFullProcessImageNameA (HANDLE hProcess, DWORD dwFlags, LPSTR lpExeName, PDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI QueryFullProcessImageNameW (HANDLE hProcess, DWORD dwFlags, LPWSTR lpExeName, PDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessShutdownParameters (LPDWORD lpdwLevel, LPDWORD lpdwFlags)
*/
HB_FUNC( WAGETPROCESSSHUTDOWNPARAMETERS )
{
  DWORD dwLevel{};
  DWORD dwFlags{};
  wa_ret_BOOL(GetProcessShutdownParameters(&dwLevel, &dwFlags));
  wa_stor_DWORD(dwLevel, 1);
  wa_stor_DWORD(dwFlags, 2);
}

/*
WINBASEAPI VOID WINAPI FatalAppExitA (UINT uAction, LPCSTR lpMessageText)
*/
#if 0
HB_FUNC( WAFATALAPPEXITA )
{
  FatalAppExitA(wa_par_UINT(1), wa_par_LPCSTR(2));
}
#endif

/*
WINBASEAPI VOID WINAPI FatalAppExitW (UINT uAction, LPCWSTR lpMessageText)
*/
#if 0
HB_FUNC( WAFATALAPPEXITW )
{
  FatalAppExitW(wa_par_UINT(1), wa_par_LPCWSTR(2));
}
#endif

HB_FUNC( WAFATALAPPEXIT )
{
  void * str2{};
  FatalAppExit(wa_par_UINT(1), HB_PARSTR(2, &str2, nullptr));
  hb_strfree(str2);
}

/*
WINBASEAPI VOID WINAPI GetStartupInfoA (LPSTARTUPINFOA lpStartupInfo)
*/

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableA (LPCSTR lpName, LPCSTR lpGuid, PVOID pBuffer, DWORD nSize)
*/
#if 0
HB_FUNC( WAGETFIRMWAREENVIRONMENTVARIABLEA )
{
  wa_ret_DWORD(GetFirmwareEnvironmentVariableA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}
#endif

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pBuffer, DWORD nSize)
*/
#if 0
HB_FUNC( WAGETFIRMWAREENVIRONMENTVARIABLEW )
{
  wa_ret_DWORD(GetFirmwareEnvironmentVariableW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}
#endif

HB_FUNC( WAGETFIRMWAREENVIRONMENTVARIABLE )
{
  void * str1{};
  void * str2{};
  wa_ret_DWORD(GetFirmwareEnvironmentVariable(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableA (LPCSTR lpName, LPCSTR lpGuid, PVOID pValue, DWORD nSize)
*/
#if 0
HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLEA )
{
  wa_ret_BOOL(SetFirmwareEnvironmentVariableA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pValue, DWORD nSize)
*/
#if 0
HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLEW )
{
  wa_ret_BOOL(SetFirmwareEnvironmentVariableW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}
#endif

HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLE )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(SetFirmwareEnvironmentVariable(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI HRSRC WINAPI FindResourceA (HMODULE hModule, LPCSTR lpName, LPCSTR lpType)
*/
#if 0
HB_FUNC( WAFINDRESOURCEA )
{
  wa_ret_HRSRC(FindResourceA(wa_par_HMODULE(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

/*
WINBASEAPI HRSRC WINAPI FindResourceW (HMODULE hModule, LPCWSTR lpName, LPCWSTR lpType)
*/
#if 0
HB_FUNC( WAFINDRESOURCEW )
{
  wa_ret_HRSRC(FindResourceW(wa_par_HMODULE(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WAFINDRESOURCE )
{
  void * str2{};
  void * str3{};
  wa_ret_HRSRC(FindResource(wa_par_HMODULE(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINBASEAPI HRSRC WINAPI FindResourceExA (HMODULE hModule, LPCSTR lpType, LPCSTR lpName, WORD wLanguage)
*/
HB_FUNC( WAFINDRESOURCEEXA )
{
  wa_ret_HRSRC(FindResourceExA(wa_par_HMODULE(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_WORD(4)));
}

/*
WINBASEAPI WINBOOL WINAPI EnumResourceTypesA (HMODULE hModule, ENUMRESTYPEPROCA lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI WINBOOL WINAPI EnumResourceTypesW (HMODULE hModule, ENUMRESTYPEPROCW lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI WINBOOL WINAPI EnumResourceNamesA (HMODULE hModule, LPCSTR lpType, ENUMRESNAMEPROCA lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI WINBOOL WINAPI EnumResourceNamesW (HMODULE hModule, LPCWSTR lpType, ENUMRESNAMEPROCW lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesA (HMODULE hModule, LPCSTR lpType, LPCSTR lpName, ENUMRESLANGPROCA lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesW (HMODULE hModule, LPCWSTR lpType, LPCWSTR lpName, ENUMRESLANGPROCW lpEnumFunc, LONG_PTR lParam)
*/

/*
WINBASEAPI HANDLE WINAPI BeginUpdateResourceA (LPCSTR pFileName, WINBOOL bDeleteExistingResources)
*/
#if 0
HB_FUNC( WABEGINUPDATERESOURCEA )
{
  wa_ret_HANDLE(BeginUpdateResourceA(wa_par_LPCSTR(1), wa_par_BOOL(2)));
}
#endif

/*
WINBASEAPI HANDLE WINAPI BeginUpdateResourceW (LPCWSTR pFileName, WINBOOL bDeleteExistingResources)
*/
#if 0
HB_FUNC( WABEGINUPDATERESOURCEW )
{
  wa_ret_HANDLE(BeginUpdateResourceW(wa_par_LPCWSTR(1), wa_par_BOOL(2)));
}
#endif

HB_FUNC( WABEGINUPDATERESOURCE )
{
  void * str1{};
  wa_ret_HANDLE(BeginUpdateResource(HB_PARSTR(1, &str1, nullptr), wa_par_BOOL(2)));
  hb_strfree(str1);
}

/*
WINBASEAPI WINBOOL WINAPI UpdateResourceA (HANDLE hUpdate, LPCSTR lpType, LPCSTR lpName, WORD wLanguage, LPVOID lpData, DWORD cb)
*/
#if 0
HB_FUNC( WAUPDATERESOURCEA )
{
  wa_ret_BOOL(UpdateResourceA(wa_par_HANDLE(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_WORD(4), static_cast<LPVOID>(hb_parptr(5)), wa_par_DWORD(6)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI UpdateResourceW (HANDLE hUpdate, LPCWSTR lpType, LPCWSTR lpName, WORD wLanguage, LPVOID lpData, DWORD cb)
*/
#if 0
HB_FUNC( WAUPDATERESOURCEW )
{
  wa_ret_BOOL(UpdateResourceW(wa_par_HANDLE(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_WORD(4), static_cast<LPVOID>(hb_parptr(5)), wa_par_DWORD(6)));
}
#endif

HB_FUNC( WAUPDATERESOURCE )
{
  void * str2{};
  void * str3{};
  wa_ret_BOOL(UpdateResource(wa_par_HANDLE(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_WORD(4), static_cast<LPVOID>(hb_parptr(5)), wa_par_DWORD(6)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINBASEAPI WINBOOL WINAPI EndUpdateResourceA (HANDLE hUpdate, WINBOOL fDiscard)
*/
#if 0
HB_FUNC( WAENDUPDATERESOURCEA )
{
  wa_ret_BOOL(EndUpdateResourceA(wa_par_HANDLE(1), wa_par_BOOL(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI EndUpdateResourceW (HANDLE hUpdate, WINBOOL fDiscard)
*/
#if 0
HB_FUNC( WAENDUPDATERESOURCEW )
{
  wa_ret_BOOL(EndUpdateResourceW(wa_par_HANDLE(1), wa_par_BOOL(2)));
}
#endif

HB_FUNC( WAENDUPDATERESOURCE )
{
  wa_ret_BOOL(EndUpdateResource(wa_par_HANDLE(1), wa_par_BOOL(2)));
}

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableExA (LPCSTR lpName, LPCSTR lpGuid, PVOID pBuffer, DWORD nSize, PDWORD pdwAttribubutes)
*/

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableExW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pBuffer, DWORD nSize, PDWORD pdwAttribubutes)
*/

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableExA (LPCSTR lpName, LPCSTR lpGuid, PVOID pValue, DWORD nSize, DWORD dwAttributes)
*/
#if 0
HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLEEXA )
{
  wa_ret_BOOL(SetFirmwareEnvironmentVariableExA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4), wa_par_DWORD(5)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableExW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pValue, DWORD nSize, DWORD dwAttributes)
*/
#if 0
HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLEEXW )
{
  wa_ret_BOOL(SetFirmwareEnvironmentVariableExW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4), wa_par_DWORD(5)));
}
#endif

#if 0
HB_FUNC( WASETFIRMWAREENVIRONMENTVARIABLEEX )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(SetFirmwareEnvironmentVariableEx(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4), wa_par_DWORD(5)));
  hb_strfree(str1);
  hb_strfree(str2);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetFirmwareType (PFIRMWARE_TYPE FirmwareType)
*/

/*
WINBASEAPI WINBOOL WINAPI IsNativeVhdBoot (PBOOL NativeVhdBoot)
*/
#if 0
HB_FUNC( WAISNATIVEVHDBOOT )
{
  BOOL NativeVhdBoot{};
  wa_ret_BOOL(IsNativeVhdBoot(&NativeVhdBoot));
  wa_stor_BOOL(NativeVhdBoot, 1);
}
#endif

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomA (LPCSTR lpString)
*/
#if 0
HB_FUNC( WAGLOBALADDATOMA )
{
  wa_ret_ATOM(GlobalAddAtomA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomW (LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAGLOBALADDATOMW )
{
  wa_ret_ATOM(GlobalAddAtomW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAGLOBALADDATOM )
{
  void * str1{};
  wa_ret_ATOM(GlobalAddAtom(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomExA (LPCSTR lpString, DWORD Flags)
*/
#if 0
HB_FUNC( WAGLOBALADDATOMEXA )
{
  wa_ret_ATOM(GlobalAddAtomExA(wa_par_LPCSTR(1), wa_par_DWORD(2)));
}
#endif

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomExW (LPCWSTR lpString, DWORD Flags)
*/
#if 0
HB_FUNC( WAGLOBALADDATOMEXW )
{
  wa_ret_ATOM(GlobalAddAtomExW(wa_par_LPCWSTR(1), wa_par_DWORD(2)));
}
#endif

#if 0
HB_FUNC( WAGLOBALADDATOMEX )
{
  void * str1{};
  wa_ret_ATOM(GlobalAddAtomEx(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2)));
  hb_strfree(str1);
}
#endif

/*
WINBASEAPI ATOM WINAPI GlobalFindAtomA (LPCSTR lpString)
*/
#if 0
HB_FUNC( WAGLOBALFINDATOMA )
{
  wa_ret_ATOM(GlobalFindAtomA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI ATOM WINAPI GlobalFindAtomW (LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAGLOBALFINDATOMW )
{
  wa_ret_ATOM(GlobalFindAtomW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAGLOBALFINDATOM )
{
  void * str1{};
  wa_ret_ATOM(GlobalFindAtom(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI UINT WINAPI GlobalGetAtomNameA (ATOM nAtom, LPSTR lpBuffer, int nSize)
*/
#if 0
HB_FUNC( WAGLOBALGETATOMNAMEA )
{
  wa_ret_UINT(GlobalGetAtomNameA(wa_par_ATOM(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}
#endif

/*
WINBASEAPI UINT WINAPI GlobalGetAtomNameW (ATOM nAtom, LPWSTR lpBuffer, int nSize)
*/
#if 0
HB_FUNC( WAGLOBALGETATOMNAMEW )
{
  wa_ret_UINT(GlobalGetAtomNameW(wa_par_ATOM(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}
#endif

/*
WINBASEAPI ATOM WINAPI AddAtomA (LPCSTR lpString)
*/
#if 0
HB_FUNC( WAADDATOMA )
{
  wa_ret_ATOM(AddAtomA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI ATOM WINAPI AddAtomW (LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAADDATOMW )
{
  wa_ret_ATOM(AddAtomW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAADDATOM )
{
  void * str1{};
  wa_ret_ATOM(AddAtom(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI ATOM WINAPI FindAtomA (LPCSTR lpString)
*/
#if 0
HB_FUNC( WAFINDATOMA )
{
  wa_ret_ATOM(FindAtomA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI ATOM WINAPI FindAtomW (LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAFINDATOMW )
{
  wa_ret_ATOM(FindAtomW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAFINDATOM )
{
  void * str1{};
  wa_ret_ATOM(FindAtom(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI UINT WINAPI GetAtomNameA (ATOM nAtom, LPSTR lpBuffer, int nSize)
*/
HB_FUNC( WAGETATOMNAMEA ) // TODO: fix
{
  wa_ret_UINT(GetAtomNameA(wa_par_ATOM(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}

/*
WINBASEAPI UINT WINAPI GetAtomNameW (ATOM nAtom, LPWSTR lpBuffer, int nSize)
*/
HB_FUNC( WAGETATOMNAMEW ) // TODO: fix
{
  wa_ret_UINT(GetAtomNameW(wa_par_ATOM(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}

/*
WINBASEAPI UINT WINAPI GetProfileIntA (LPCSTR lpAppName, LPCSTR lpKeyName, INT nDefault)
*/
#if 0
HB_FUNC( WAGETPROFILEINTA )
{
  wa_ret_UINT(GetProfileIntA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_INT(3)));
}
#endif

/*
WINBASEAPI UINT WINAPI GetProfileIntW (LPCWSTR lpAppName, LPCWSTR lpKeyName, INT nDefault)
*/
#if 0
HB_FUNC( WAGETPROFILEINTW )
{
  wa_ret_UINT(GetProfileIntW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_INT(3)));
}
#endif

HB_FUNC( WAGETPROFILEINT )
{
  void * str1{};
  void * str2{};
  wa_ret_UINT(GetProfileInt(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_INT(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI DWORD WINAPI GetProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpDefault, LPSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WAGETPROFILESTRINGA ) // TODO: fix
{
  wa_ret_DWORD(GetProfileStringA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), const_cast<LPSTR>(hb_parc(4)), wa_par_DWORD(5)));
}

/*
WINBASEAPI DWORD WINAPI GetProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpDefault, LPWSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WAGETPROFILESTRINGW ) // TODO: fix
{
  wa_ret_DWORD(GetProfileStringW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4))), wa_par_DWORD(5)));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpString)
*/
#if 0
HB_FUNC( WAWRITEPROFILESTRINGA )
{
  wa_ret_BOOL(WriteProfileStringA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI WriteProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAWRITEPROFILESTRINGW )
{
  wa_ret_BOOL(WriteProfileStringW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WAWRITEPROFILESTRING )
{
  void * str1{};
  void * str2{};
  void * str3{};
  wa_ret_BOOL(WriteProfileString(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINBASEAPI DWORD WINAPI GetProfileSectionA (LPCSTR lpAppName, LPSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WAGETPROFILESECTIONA ) // TODO: fix
{
  wa_ret_DWORD(GetProfileSectionA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI DWORD WINAPI GetProfileSectionW (LPCWSTR lpAppName, LPWSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WAGETPROFILESECTIONW ) // TODO: fix
{
  wa_ret_DWORD(GetProfileSectionW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileSectionA (LPCSTR lpAppName, LPCSTR lpString)
*/
#if 0
HB_FUNC( WAWRITEPROFILESECTIONA )
{
  wa_ret_BOOL(WriteProfileSectionA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI WriteProfileSectionW (LPCWSTR lpAppName, LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAWRITEPROFILESECTIONW )
{
  wa_ret_BOOL(WriteProfileSectionW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAWRITEPROFILESECTION )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(WriteProfileSection(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI UINT WINAPI GetPrivateProfileIntA (LPCSTR lpAppName, LPCSTR lpKeyName, INT nDefault, LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WAGETPRIVATEPROFILEINTA )
{
  wa_ret_UINT(GetPrivateProfileIntA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_INT(3), wa_par_LPCSTR(4)));
}
#endif

/*
WINBASEAPI UINT WINAPI GetPrivateProfileIntW (LPCWSTR lpAppName, LPCWSTR lpKeyName, INT nDefault, LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WAGETPRIVATEPROFILEINTW )
{
  wa_ret_UINT(GetPrivateProfileIntW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_INT(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC( WAGETPRIVATEPROFILEINT )
{
  void * str1{};
  void * str2{};
  void * str4{};
  wa_ret_UINT(GetPrivateProfileInt(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_INT(3), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str4);
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpDefault, LPSTR lpReturnedString, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESTRINGA )
{
  wa_ret_DWORD(GetPrivateProfileStringA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), const_cast<LPSTR>(hb_parc(4)), wa_par_DWORD(5), wa_par_LPCSTR(6)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpDefault, LPWSTR lpReturnedString, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESTRINGW )
{
  wa_ret_DWORD(GetPrivateProfileStringW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4))), wa_par_DWORD(5), wa_par_LPCWSTR(6)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpString, LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESTRINGA )
{
  wa_ret_BOOL(WritePrivateProfileStringA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_LPCSTR(4)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpString, LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESTRINGW )
{
  wa_ret_BOOL(WritePrivateProfileStringW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC( WAWRITEPRIVATEPROFILESTRING )
{
  void * str1{};
  void * str2{};
  void * str3{};
  void * str4{};
  wa_ret_BOOL(WritePrivateProfileString(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionA (LPCSTR lpAppName, LPSTR lpReturnedString, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESECTIONA )
{
  wa_ret_DWORD(GetPrivateProfileSectionA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3), wa_par_LPCSTR(4)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionW (LPCWSTR lpAppName, LPWSTR lpReturnedString, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESECTIONW )
{
  wa_ret_DWORD(GetPrivateProfileSectionW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3), wa_par_LPCWSTR(4)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionA (LPCSTR lpAppName, LPCSTR lpString, LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESECTIONA )
{
  wa_ret_BOOL(WritePrivateProfileSectionA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionW (LPCWSTR lpAppName, LPCWSTR lpString, LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESECTIONW )
{
  wa_ret_BOOL(WritePrivateProfileSectionW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WAWRITEPRIVATEPROFILESECTION )
{
  void * str1{};
  void * str2{};
  void * str3{};
  wa_ret_BOOL(WritePrivateProfileSection(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesA (LPSTR lpszReturnBuffer, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESECTIONNAMESA )
{
  wa_ret_DWORD(GetPrivateProfileSectionNamesA(const_cast<LPSTR>(hb_parc(1)), wa_par_DWORD(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesW (LPWSTR lpszReturnBuffer, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WAGETPRIVATEPROFILESECTIONNAMESW )
{
  wa_ret_DWORD(GetPrivateProfileSectionNamesW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_DWORD(2), wa_par_LPCWSTR(3)));
}

/*
WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructA (LPCSTR lpszSection, LPCSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCSTR szFile)
*/
HB_FUNC( WAGETPRIVATEPROFILESTRUCTA )
{
  wa_ret_BOOL(GetPrivateProfileStructA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_UINT(4), wa_par_LPCSTR(5)));
}

/*
WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructW (LPCWSTR lpszSection, LPCWSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCWSTR szFile)
*/
HB_FUNC( WAGETPRIVATEPROFILESTRUCTW )
{
  wa_ret_BOOL(GetPrivateProfileStructW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_UINT(4), wa_par_LPCWSTR(5)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructA (LPCSTR lpszSection, LPCSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCSTR szFile)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESTRUCTA )
{
  wa_ret_BOOL(WritePrivateProfileStructA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_UINT(4), wa_par_LPCSTR(5)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructW (LPCWSTR lpszSection, LPCWSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCWSTR szFile)
*/
#if 0
HB_FUNC( WAWRITEPRIVATEPROFILESTRUCTW )
{
  wa_ret_BOOL(WritePrivateProfileStructW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_UINT(4), wa_par_LPCWSTR(5)));
}
#endif

HB_FUNC( WAWRITEPRIVATEPROFILESTRUCT )
{
  void * str1{};
  void * str2{};
  void * str5{};
  wa_ret_BOOL(WritePrivateProfileStruct(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), static_cast<LPVOID>(hb_parptr(3)), wa_par_UINT(4), HB_PARSTR(5, &str5, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str5);
}

/*
WINBASEAPI DWORD WINAPI GetTempPathA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WAGETTEMPPATHA )
{
  wa_ret_DWORD(GetTempPathA(wa_par_DWORD(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINBASEAPI UINT WINAPI GetTempFileNameA (LPCSTR lpPathName, LPCSTR lpPrefixString, UINT uUnique, LPSTR lpTempFileName)
*/
HB_FUNC( WAGETTEMPFILENAMEA )
{
  wa_ret_UINT(GetTempFileNameA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_UINT(3), const_cast<LPSTR>(hb_parc(4))));
}

/*
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryA (LPSTR lpBuffer, UINT uSize)
*/
HB_FUNC( WAGETSYSTEMWOW64DIRECTORYA )
{
  wa_ret_UINT(GetSystemWow64DirectoryA(const_cast<LPSTR>(hb_parc(1)), wa_par_UINT(2)));
}

/*
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryW (LPWSTR lpBuffer, UINT uSize)
*/
HB_FUNC( WAGETSYSTEMWOW64DIRECTORYW )
{
  wa_ret_UINT(GetSystemWow64DirectoryW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_UINT(2)));
}

/*
WINBASEAPI BOOLEAN WINAPI Wow64EnableWow64FsRedirection (BOOLEAN Wow64FsEnableRedirection)
*/
HB_FUNC( WAWOW64ENABLEWOW64FSREDIRECTION )
{
  wa_ret_BOOLEAN(Wow64EnableWow64FsRedirection(wa_par_BOOLEAN(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetDllDirectoryA (LPCSTR lpPathName)
*/
#if 0
HB_FUNC( WASETDLLDIRECTORYA )
{
  wa_ret_BOOL(SetDllDirectoryA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetDllDirectoryW (LPCWSTR lpPathName)
*/
#if 0
HB_FUNC( WASETDLLDIRECTORYW )
{
  wa_ret_BOOL(SetDllDirectoryW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WASETDLLDIRECTORY )
{
  void * str1{};
  wa_ret_BOOL(SetDllDirectory(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI DWORD WINAPI GetDllDirectoryA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WAGETDLLDIRECTORYA )
{
  wa_ret_DWORD(GetDllDirectoryA(wa_par_DWORD(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINBASEAPI DWORD WINAPI GetDllDirectoryW (DWORD nBufferLength, LPWSTR lpBuffer)
*/
HB_FUNC( WAGETDLLDIRECTORYW )
{
  wa_ret_DWORD(GetDllDirectoryW(wa_par_DWORD(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2)))));
}

/*
WINBASEAPI WINBOOL WINAPI SetSearchPathMode (DWORD Flags)
*/
HB_FUNC( WASETSEARCHPATHMODE )
{
  wa_ret_BOOL(SetSearchPathMode(wa_par_DWORD(1)));
}

/*
WINBASEAPI WINBOOL WINAPI CreateDirectoryExA (LPCSTR lpTemplateDirectory, LPCSTR lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateDirectoryExW (LPCWSTR lpTemplateDirectory, LPCWSTR lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateDirectoryTransactedA (LPCSTR lpTemplateDirectory, LPCSTR lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateDirectoryTransactedW (LPCWSTR lpTemplateDirectory, LPCWSTR lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI RemoveDirectoryTransactedA (LPCSTR lpPathName, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WAREMOVEDIRECTORYTRANSACTEDA )
{
  wa_ret_BOOL(RemoveDirectoryTransactedA(wa_par_LPCSTR(1), wa_par_HANDLE(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI RemoveDirectoryTransactedW (LPCWSTR lpPathName, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WAREMOVEDIRECTORYTRANSACTEDW )
{
  wa_ret_BOOL(RemoveDirectoryTransactedW(wa_par_LPCWSTR(1), wa_par_HANDLE(2)));
}
#endif

#if 0
HB_FUNC( WAREMOVEDIRECTORYTRANSACTED )
{
  void * str1{};
  wa_ret_BOOL(RemoveDirectoryTransacted(HB_PARSTR(1, &str1, nullptr), wa_par_HANDLE(2)));
  hb_strfree(str1);
}
#endif

/*
WINBASEAPI DWORD WINAPI GetFullPathNameTransactedA (LPCSTR lpFileName, DWORD nBufferLength, LPSTR lpBuffer, LPSTR *lpFilePart, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetFullPathNameTransactedW (LPCWSTR lpFileName, DWORD nBufferLength, LPWSTR lpBuffer, LPWSTR *lpFilePart, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI DefineDosDeviceA (DWORD dwFlags, LPCSTR lpDeviceName, LPCSTR lpTargetPath)
*/
HB_FUNC( WADEFINEDOSDEVICEA )
{
  wa_ret_BOOL(DefineDosDeviceA(wa_par_DWORD(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}

/*
WINBASEAPI DWORD WINAPI QueryDosDeviceA (LPCSTR lpDeviceName, LPSTR lpTargetPath, DWORD ucchMax)
*/
HB_FUNC( WAQUERYDOSDEVICEA )
{
  wa_ret_DWORD(QueryDosDeviceA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI HANDLE WINAPI CreateFileTransactedA (LPCSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile, HANDLE hTransaction, PUSHORT pusMiniVersion, PVOID lpExtendedParameter)
*/

/*
WINBASEAPI HANDLE WINAPI CreateFileTransactedW (LPCWSTR lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile, HANDLE hTransaction, PUSHORT pusMiniVersion, PVOID lpExtendedParameter)
*/

/*
WINBASEAPI HANDLE WINAPI ReOpenFile (HANDLE hOriginalFile, DWORD dwDesiredAccess, DWORD dwShareMode, DWORD dwFlagsAndAttributes)
*/
HB_FUNC( WAREOPENFILE )
{
  wa_ret_HANDLE(ReOpenFile(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4)));
}

/*
WINBASEAPI WINBOOL WINAPI SetFileAttributesTransactedA (LPCSTR lpFileName, DWORD dwFileAttributes, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WASETFILEATTRIBUTESTRANSACTEDA )
{
  wa_ret_BOOL(SetFileAttributesTransactedA(wa_par_LPCSTR(1), wa_par_DWORD(2), wa_par_HANDLE(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetFileAttributesTransactedW (LPCWSTR lpFileName, DWORD dwFileAttributes, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WASETFILEATTRIBUTESTRANSACTEDW )
{
  wa_ret_BOOL(SetFileAttributesTransactedW(wa_par_LPCWSTR(1), wa_par_DWORD(2), wa_par_HANDLE(3)));
}
#endif

#if 0
HB_FUNC( WASETFILEATTRIBUTESTRANSACTED )
{
  void * str1{};
  wa_ret_BOOL(SetFileAttributesTransacted(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2), wa_par_HANDLE(3)));
  hb_strfree(str1);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetFileAttributesTransactedA (LPCSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFileAttributesTransactedW (LPCWSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeA (LPCSTR lpFileName, LPDWORD lpFileSizeHigh)
*/
#if 0
HB_FUNC( WAGETCOMPRESSEDFILESIZEA )
{
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSizeA(wa_par_LPCSTR(1), &FileSizeHigh));
  wa_stor_DWORD(FileSizeHigh, 2);
}
#endif

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeW (LPCWSTR lpFileName, LPDWORD lpFileSizeHigh)
*/
#if 0
HB_FUNC( WAGETCOMPRESSEDFILESIZEW )
{
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSizeW(wa_par_LPCWSTR(1), &FileSizeHigh));
  wa_stor_DWORD(FileSizeHigh, 2);
}
#endif

HB_FUNC( WAGETCOMPRESSEDFILESIZE )
{
  void * str1{};
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSize(HB_PARSTR(1, &str1, nullptr), &FileSizeHigh));
  wa_stor_DWORD(FileSizeHigh, 2);
  hb_strfree(str1);
}

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeTransactedA (LPCSTR lpFileName, LPDWORD lpFileSizeHigh, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WAGETCOMPRESSEDFILESIZETRANSACTEDA )
{
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSizeTransactedA(wa_par_LPCSTR(1), &FileSizeHigh, wa_par_HANDLE(3)));
  wa_stor_DWORD(FileSizeHigh, 2);
}
#endif

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeTransactedW (LPCWSTR lpFileName, LPDWORD lpFileSizeHigh, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WAGETCOMPRESSEDFILESIZETRANSACTEDW )
{
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSizeTransactedW(wa_par_LPCWSTR(1), &FileSizeHigh, wa_par_HANDLE(3)));
  wa_stor_DWORD(FileSizeHigh, 2);
}
#endif

#if 0
HB_FUNC( WAGETCOMPRESSEDFILESIZETRANSACTED )
{
  void * str1{};
  DWORD FileSizeHigh{};
  wa_ret_DWORD(GetCompressedFileSizeTransacted(HB_PARSTR(1, &str1, nullptr), &FileSizeHigh, wa_par_HANDLE(3)));
  wa_stor_DWORD(FileSizeHigh, 2);
  hb_strfree(str1);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI DeleteFileTransactedA (LPCSTR lpFileName, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WADELETEFILETRANSACTEDA )
{
  wa_ret_BOOL(DeleteFileTransactedA(wa_par_LPCSTR(1), wa_par_HANDLE(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI DeleteFileTransactedW (LPCWSTR lpFileName, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WADELETEFILETRANSACTEDW )
{
  wa_ret_BOOL(DeleteFileTransactedW(wa_par_LPCWSTR(1), wa_par_HANDLE(2)));
}
#endif

#if 0
HB_FUNC( WADELETEFILETRANSACTED )
{
  void * str1{};
  wa_ret_BOOL(DeleteFileTransacted(HB_PARSTR(1, &str1, nullptr), wa_par_HANDLE(2)));
  hb_strfree(str1);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3A (LPCSTR lpName, LPSTR lpOemName, DWORD OemNameSize, PBOOL pbNameContainsSpaces, PBOOL pbNameLegal)
*/

/*
WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3W (LPCWSTR lpName, LPSTR lpOemName, DWORD OemNameSize, PBOOL pbNameContainsSpaces, PBOOL pbNameLegal)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyFileA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, WINBOOL bFailIfExists)
*/
#if 0
HB_FUNC( WACOPYFILEA )
{
  wa_ret_BOOL(CopyFileA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_BOOL(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI CopyFileW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, WINBOOL bFailIfExists)
*/
#if 0
HB_FUNC( WACOPYFILEW )
{
  wa_ret_BOOL(CopyFileW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_BOOL(3)));
}
#endif

HB_FUNC( WACOPYFILE )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(CopyFileW(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_BOOL(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI WINBOOL WINAPI CopyFileExA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyFileExW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstFileTransactedA (LPCSTR lpFileName, FINDEX_INFO_LEVELS fInfoLevelId, LPVOID lpFindFileData, FINDEX_SEARCH_OPS fSearchOp, LPVOID lpSearchFilter, DWORD dwAdditionalFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstFileTransactedW (LPCWSTR lpFileName, FINDEX_INFO_LEVELS fInfoLevelId, LPVOID lpFindFileData, FINDEX_SEARCH_OPS fSearchOp, LPVOID lpSearchFilter, DWORD dwAdditionalFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyFileTransactedA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyFileTransactedW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI HRESULT WINAPI CopyFile2 (PCWSTR pwszExistingFileName, PCWSTR pwszNewFileName, COPYFILE2_EXTENDED_PARAMETERS *pExtendedParameters)
*/

/*
WINBASEAPI WINBOOL WINAPI MoveFileA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName)
*/
#if 0
HB_FUNC( WAMOVEFILEA )
{
  wa_ret_BOOL(MoveFileA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI MoveFileW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName)
*/
#if 0
HB_FUNC( WAMOVEFILEW )
{
  wa_ret_BOOL(MoveFileW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAMOVEFILE )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(MoveFile(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI WINBOOL WINAPI MoveFileExA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, DWORD dwFlags)
*/
#if 0
HB_FUNC( WAMOVEFILEEXA )
{
  wa_ret_BOOL(MoveFileExA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_DWORD(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI MoveFileExW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, DWORD dwFlags)
*/
#if 0
HB_FUNC( WAMOVEFILEEXW )
{
  wa_ret_BOOL(MoveFileExW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_DWORD(3)));
}
#endif

HB_FUNC( WAMOVEFILEEX )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(MoveFileEx(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI WINBOOL WINAPI MoveFileWithProgressA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, DWORD dwFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI MoveFileWithProgressW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, DWORD dwFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI MoveFileTransactedA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, DWORD dwFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI MoveFileTransactedW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, DWORD dwFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI ReplaceFileA (LPCSTR lpReplacedFileName, LPCSTR lpReplacementFileName, LPCSTR lpBackupFileName, DWORD dwReplaceFlags, LPVOID lpExclude, LPVOID lpReserved)
*/
#if 0
HB_FUNC( WAREPLACEFILEA )
{
  wa_ret_BOOL(ReplaceFileA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)), static_cast<LPVOID>(hb_parptr(6))));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI ReplaceFileW (LPCWSTR lpReplacedFileName, LPCWSTR lpReplacementFileName, LPCWSTR lpBackupFileName, DWORD dwReplaceFlags, LPVOID lpExclude, LPVOID lpReserved)
*/
#if 0
HB_FUNC( WAREPLACEFILEW )
{
  wa_ret_BOOL(ReplaceFileW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)), static_cast<LPVOID>(hb_parptr(6))));
}
#endif

HB_FUNC( WAREPLACEFILE )
{
  void * str1{};
  void * str2{};
  void * str3{};
  wa_ret_BOOL(ReplaceFile(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)), static_cast<LPVOID>(hb_parptr(6))));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINBASEAPI WINBOOL WINAPI CreateHardLinkA (LPCSTR lpFileName, LPCSTR lpExistingFileName, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateHardLinkW (LPCWSTR lpFileName, LPCWSTR lpExistingFileName, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateHardLinkTransactedA (LPCSTR lpFileName, LPCSTR lpExistingFileName, LPSECURITY_ATTRIBUTES lpSecurityAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateHardLinkTransactedW (LPCWSTR lpFileName, LPCWSTR lpExistingFileName, LPSECURITY_ATTRIBUTES lpSecurityAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstStreamW (LPCWSTR lpFileName, STREAM_INFO_LEVELS InfoLevel, LPVOID lpFindStreamData, DWORD dwFlags)
*/

/*
WINBASEAPI WINBOOL APIENTRY FindNextStreamW (HANDLE hFindStream, LPVOID lpFindStreamData)
*/
HB_FUNC( WAFINDNEXTSTREAMW )
{
  wa_ret_BOOL(FindNextStreamW(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINBASEAPI HANDLE WINAPI FindFirstStreamTransactedW (LPCWSTR lpFileName, STREAM_INFO_LEVELS InfoLevel, LPVOID lpFindStreamData, DWORD dwFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstFileNameW (LPCWSTR lpFileName, DWORD dwFlags, LPDWORD StringLength, PWSTR LinkName)
*/

/*
WINBASEAPI WINBOOL APIENTRY FindNextFileNameW (HANDLE hFindStream, LPDWORD StringLength, PWSTR LinkName)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstFileNameTransactedW (LPCWSTR lpFileName, DWORD dwFlags, LPDWORD StringLength, PWSTR LinkName, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeClientComputerNameA (HANDLE Pipe, LPSTR ClientComputerName, ULONG ClientComputerNameLength)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeClientProcessId (HANDLE Pipe, PULONG ClientProcessId)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeClientSessionId (HANDLE Pipe, PULONG ClientSessionId)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeServerProcessId (HANDLE Pipe, PULONG ServerProcessId)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeServerSessionId (HANDLE Pipe, PULONG ServerSessionId)
*/

/*
WINBASEAPI WINBOOL WINAPI SetFileBandwidthReservation (HANDLE hFile, DWORD nPeriodMilliseconds, DWORD nBytesPerPeriod, WINBOOL bDiscardable, LPDWORD lpTransferSize, LPDWORD lpNumOutstandingRequests)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFileBandwidthReservation (HANDLE hFile, LPDWORD lpPeriodMilliseconds, LPDWORD lpBytesPerPeriod, LPBOOL pDiscardable, LPDWORD lpTransferSize, LPDWORD lpNumOutstandingRequests)
*/

/*
WINBASEAPI HANDLE WINAPI CreateNamedPipeA (LPCSTR lpName, DWORD dwOpenMode, DWORD dwPipeMode, DWORD nMaxInstances, DWORD nOutBufferSize, DWORD nInBufferSize, DWORD nDefaultTimeOut, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeHandleStateA (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, LPSTR lpUserName, DWORD nMaxUserNameSize)
*/
HB_FUNC( WAGETNAMEDPIPEHANDLESTATEA )
{
  DWORD State{};
  DWORD CurInstances{};
  DWORD MaxCollectionCount{};
  DWORD CollectDataTimeout{};
  wa_ret_BOOL(GetNamedPipeHandleStateA(wa_par_HANDLE(1), &State, &CurInstances, &MaxCollectionCount, &CollectDataTimeout, const_cast<LPSTR>(hb_parc(6)), wa_par_DWORD(7)));
  wa_stor_DWORD(State, 2);
  wa_stor_DWORD(CurInstances, 3);
  wa_stor_DWORD(MaxCollectionCount, 4);
  wa_stor_DWORD(CollectDataTimeout, 5);
}

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeHandleStateW (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, LPWSTR lpUserName, DWORD nMaxUserNameSize)
*/
HB_FUNC( WAGETNAMEDPIPEHANDLESTATEW )
{
  DWORD State{};
  DWORD CurInstances{};
  DWORD MaxCollectionCount{};
  DWORD CollectDataTimeout{};
  wa_ret_BOOL(GetNamedPipeHandleStateW(wa_par_HANDLE(1), &State, &CurInstances, &MaxCollectionCount, &CollectDataTimeout, reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(6))), wa_par_DWORD(7)));
  wa_stor_DWORD(State, 2);
  wa_stor_DWORD(CurInstances, 3);
  wa_stor_DWORD(MaxCollectionCount, 4);
  wa_stor_DWORD(CollectDataTimeout, 5);
}

/*
WINBASEAPI WINBOOL WINAPI CallNamedPipeA (LPCSTR lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut)
*/
HB_FUNC( WACALLNAMEDPIPEA )
{
  DWORD BytesRead{};
  wa_ret_BOOL(CallNamedPipeA(wa_par_LPCSTR(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPVOID>(hb_parptr(4)), wa_par_DWORD(5), &BytesRead, wa_par_DWORD(7)));
  wa_stor_DWORD(BytesRead, 6);
}

/*
WINBASEAPI WINBOOL WINAPI CallNamedPipeW (LPCWSTR lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut)
*/
HB_FUNC( WACALLNAMEDPIPEW )
{
  DWORD BytesRead{};
  wa_ret_BOOL(CallNamedPipeW(wa_par_LPCWSTR(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPVOID>(hb_parptr(4)), wa_par_DWORD(5), &BytesRead, wa_par_DWORD(7)));
  wa_stor_DWORD(BytesRead, 6);
}

/*
WINBASEAPI WINBOOL WINAPI WaitNamedPipeA (LPCSTR lpNamedPipeName, DWORD nTimeOut)
*/
HB_FUNC( WAWAITNAMEDPIPEA )
{
  wa_ret_BOOL(WaitNamedPipeA(wa_par_LPCSTR(1), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeLabelA (LPCSTR lpRootPathName, LPCSTR lpVolumeName)
*/
#if 0
HB_FUNC( WASETVOLUMELABELA )
{
  wa_ret_BOOL(SetVolumeLabelA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetVolumeLabelW (LPCWSTR lpRootPathName, LPCWSTR lpVolumeName)
*/
#if 0
HB_FUNC( WASETVOLUMELABELW )
{
  wa_ret_BOOL(SetVolumeLabelW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WASETVOLUMELABEL )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(SetVolumeLabel(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI VOID WINAPI SetFileApisToOEM (VOID)
*/
HB_FUNC( WASETFILEAPISTOOEM )
{
  SetFileApisToOEM();
}

/*
WINBASEAPI VOID WINAPI SetFileApisToANSI (VOID)
*/
HB_FUNC( WASETFILEAPISTOANSI )
{
  SetFileApisToANSI();
}

/*
WINBASEAPI WINBOOL WINAPI AreFileApisANSI (VOID)
*/
HB_FUNC( WAAREFILEAPISANSI )
{
  wa_ret_BOOL(AreFileApisANSI());
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumeInformationA (LPCSTR lpRootPathName, LPSTR lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, LPSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize)
*/
HB_FUNC( WAGETVOLUMEINFORMATIONA )
{
  DWORD VolumeSerialNumber{};
  DWORD MaximumComponentLength{};
  DWORD FileSystemFlags{};
  wa_ret_BOOL(GetVolumeInformationA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3), &VolumeSerialNumber, &MaximumComponentLength, &FileSystemFlags, const_cast<LPSTR>(hb_parc(7)), wa_par_DWORD(8)));
  wa_stor_DWORD(VolumeSerialNumber, 4);
  wa_stor_DWORD(MaximumComponentLength, 5);
  wa_stor_DWORD(FileSystemFlags, 6);
}

/*
WINADVAPI WINBOOL WINAPI ClearEventLogA (HANDLE hEventLog, LPCSTR lpBackupFileName)
*/
#if 0
HB_FUNC( WACLEAREVENTLOGA )
{
  wa_ret_BOOL(ClearEventLogA(wa_par_HANDLE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI WINBOOL WINAPI ClearEventLogW (HANDLE hEventLog, LPCWSTR lpBackupFileName)
*/
#if 0
HB_FUNC( WACLEAREVENTLOGW )
{
  wa_ret_BOOL(ClearEventLogW(wa_par_HANDLE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WACLEAREVENTLOG )
{
  void * str1{};
  wa_ret_BOOL(ClearEventLog(wa_par_HANDLE(1), HB_PARSTR(2, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINADVAPI WINBOOL WINAPI BackupEventLogA (HANDLE hEventLog, LPCSTR lpBackupFileName)
*/
#if 0
HB_FUNC( WABACKUPEVENTLOGA )
{
  wa_ret_BOOL(BackupEventLogA(wa_par_HANDLE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI WINBOOL WINAPI BackupEventLogW (HANDLE hEventLog, LPCWSTR lpBackupFileName)
*/
#if 0
HB_FUNC( WABACKUPEVENTLOGW )
{
  wa_ret_BOOL(BackupEventLogW(wa_par_HANDLE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WABACKUPEVENTLOG )
{
  void * str1{};
  wa_ret_BOOL(BackupEventLog(wa_par_HANDLE(1), HB_PARSTR(2, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINADVAPI WINBOOL WINAPI CloseEventLog (HANDLE hEventLog)
*/
HB_FUNC( WACLOSEEVENTLOG )
{
  wa_ret_BOOL(CloseEventLog(wa_par_HANDLE(1)));
}

/*
WINADVAPI WINBOOL WINAPI DeregisterEventSource (HANDLE hEventLog)
*/
HB_FUNC( WADEREGISTEREVENTSOURCE )
{
  wa_ret_BOOL(DeregisterEventSource(wa_par_HANDLE(1)));
}

/*
WINADVAPI WINBOOL WINAPI NotifyChangeEventLog (HANDLE hEventLog, HANDLE hEvent)
*/
HB_FUNC( WANOTIFYCHANGEEVENTLOG )
{
  wa_ret_BOOL(NotifyChangeEventLog(wa_par_HANDLE(1), wa_par_HANDLE(2)));
}

/*
WINADVAPI WINBOOL WINAPI GetNumberOfEventLogRecords (HANDLE hEventLog, PDWORD NumberOfRecords)
*/
HB_FUNC( WAGETNUMBEROFEVENTLOGRECORDS )
{
  DWORD NumberOfRecords{};
  wa_ret_BOOL(GetNumberOfEventLogRecords(wa_par_HANDLE(1), &NumberOfRecords));
  wa_stor_DWORD(NumberOfRecords, 2);
}

/*
WINADVAPI WINBOOL WINAPI GetOldestEventLogRecord (HANDLE hEventLog, PDWORD OldestRecord)
*/
HB_FUNC( WAGETOLDESTEVENTLOGRECORD )
{
  DWORD OldestRecord{};
  wa_ret_BOOL(GetOldestEventLogRecord(wa_par_HANDLE(1), &OldestRecord));
  wa_stor_DWORD(OldestRecord, 2);
}

/*
WINADVAPI HANDLE WINAPI OpenEventLogA (LPCSTR lpUNCServerName, LPCSTR lpSourceName)
*/
#if 0
HB_FUNC( WAOPENEVENTLOGA )
{
  wa_ret_HANDLE(OpenEventLogA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI HANDLE WINAPI OpenEventLogW (LPCWSTR lpUNCServerName, LPCWSTR lpSourceName)
*/
#if 0
HB_FUNC( WAOPENEVENTLOGW )
{
  wa_ret_HANDLE(OpenEventLogW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAOPENEVENTLOG )
{
  void * str1{};
  void * str2{};
  wa_ret_HANDLE(OpenEventLog(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINADVAPI HANDLE WINAPI RegisterEventSourceA (LPCSTR lpUNCServerName, LPCSTR lpSourceName)
*/
#if 0
HB_FUNC( WAREGISTEREVENTSOURCEA )
{
  wa_ret_HANDLE(RegisterEventSourceA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI HANDLE WINAPI RegisterEventSourceW (LPCWSTR lpUNCServerName, LPCWSTR lpSourceName)
*/
#if 0
HB_FUNC( WAREGISTEREVENTSOURCEW )
{
  wa_ret_HANDLE(RegisterEventSourceW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAREGISTEREVENTSOURCE )
{
  void * str1{};
  void * str2{};
  wa_ret_HANDLE(RegisterEventSource(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINADVAPI HANDLE WINAPI OpenBackupEventLogA (LPCSTR lpUNCServerName, LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WAOPENBACKUPEVENTLOGA )
{
  wa_ret_HANDLE(OpenBackupEventLogA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI HANDLE WINAPI OpenBackupEventLogW (LPCWSTR lpUNCServerName, LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WAOPENBACKUPEVENTLOGW )
{
  wa_ret_HANDLE(OpenBackupEventLogW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAOPENBACKUPEVENTLOG )
{
  void * str1{};
  void * str2{};
  wa_ret_HANDLE(OpenBackupEventLog(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINADVAPI WINBOOL WINAPI ReadEventLogA (HANDLE hEventLog, DWORD dwReadFlags, DWORD dwRecordOffset, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, DWORD *pnBytesRead, DWORD *pnMinNumberOfBytesNeeded)
*/

/*
WINADVAPI WINBOOL WINAPI ReadEventLogW (HANDLE hEventLog, DWORD dwReadFlags, DWORD dwRecordOffset, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, DWORD *pnBytesRead, DWORD *pnMinNumberOfBytesNeeded)
*/

/*
WINADVAPI WINBOOL WINAPI ReportEventA (HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID, PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, LPCSTR *lpStrings, LPVOID lpRawData)
*/

/*
WINADVAPI WINBOOL WINAPI ReportEventW (HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID, PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, LPCWSTR *lpStrings, LPVOID lpRawData)
*/

/*
WINADVAPI WINBOOL WINAPI GetEventLogInformation (HANDLE hEventLog, DWORD dwInfoLevel, LPVOID lpBuffer, DWORD cbBufSize, LPDWORD pcbBytesNeeded)
*/
HB_FUNC( WAGETEVENTLOGINFORMATION )
{
  DWORD cbBytesNeeded{};
  wa_ret_BOOL(GetEventLogInformation(wa_par_HANDLE(1), wa_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_DWORD(4), &cbBytesNeeded));
  wa_stor_DWORD(cbBytesNeeded, 5);
}

/*
WINADVAPI WINBOOL WINAPI OperationStart (OPERATION_START_PARAMETERS *OperationStartParams)
*/

/*
WINADVAPI WINBOOL WINAPI OperationEnd (OPERATION_END_PARAMETERS *OperationEndParams)
*/

/*
WINADVAPI WINBOOL WINAPI AccessCheckAndAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, LPSTR ObjectTypeName, LPSTR ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, DWORD DesiredAccess, PGENERIC_MAPPING GenericMapping, WINBOOL ObjectCreation, LPDWORD GrantedAccess, LPBOOL AccessStatus, LPBOOL pfGenerateOnClose)
*/

/*
WINADVAPI WINBOOL WINAPI AccessCheckByTypeAndAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, LPCSTR ObjectTypeName, LPCSTR ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, PSID PrincipalSelfSid, DWORD DesiredAccess, AUDIT_EVENT_TYPE AuditType, DWORD Flags, POBJECT_TYPE_LIST ObjectTypeList, DWORD ObjectTypeListLength, PGENERIC_MAPPING GenericMapping, WINBOOL ObjectCreation, LPDWORD GrantedAccess, LPBOOL AccessStatus, LPBOOL pfGenerateOnClose)
*/

/*
WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, LPCSTR ObjectTypeName, LPCSTR ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, PSID PrincipalSelfSid, DWORD DesiredAccess, AUDIT_EVENT_TYPE AuditType, DWORD Flags, POBJECT_TYPE_LIST ObjectTypeList, DWORD ObjectTypeListLength, PGENERIC_MAPPING GenericMapping, WINBOOL ObjectCreation, LPDWORD GrantedAccess, LPDWORD AccessStatusList, LPBOOL pfGenerateOnClose)
*/

/*
WINADVAPI WINBOOL WINAPI AccessCheckByTypeResultListAndAuditAlarmByHandleA (LPCSTR SubsystemName, LPVOID HandleId, HANDLE ClientToken, LPCSTR ObjectTypeName, LPCSTR ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, PSID PrincipalSelfSid, DWORD DesiredAccess, AUDIT_EVENT_TYPE AuditType, DWORD Flags, POBJECT_TYPE_LIST ObjectTypeList, DWORD ObjectTypeListLength, PGENERIC_MAPPING GenericMapping, WINBOOL ObjectCreation, LPDWORD GrantedAccess, LPDWORD AccessStatusList, LPBOOL pfGenerateOnClose)
*/

/*
WINADVAPI WINBOOL WINAPI ObjectOpenAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, LPSTR ObjectTypeName, LPSTR ObjectName, PSECURITY_DESCRIPTOR pSecurityDescriptor, HANDLE ClientToken, DWORD DesiredAccess, DWORD GrantedAccess, PPRIVILEGE_SET Privileges, WINBOOL ObjectCreation, WINBOOL AccessGranted, LPBOOL GenerateOnClose)
*/

/*
WINADVAPI WINBOOL WINAPI ObjectPrivilegeAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, HANDLE ClientToken, DWORD DesiredAccess, PPRIVILEGE_SET Privileges, WINBOOL AccessGranted)
*/

/*
WINADVAPI WINBOOL WINAPI ObjectCloseAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, WINBOOL GenerateOnClose)
*/
HB_FUNC( WAOBJECTCLOSEAUDITALARMA )
{
  wa_ret_BOOL(ObjectCloseAuditAlarmA(wa_par_LPCSTR(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_BOOL(3)));
}

/*
WINADVAPI WINBOOL WINAPI ObjectDeleteAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, WINBOOL GenerateOnClose)
*/
HB_FUNC( WAOBJECTDELETEAUDITALARMA )
{
  wa_ret_BOOL(ObjectDeleteAuditAlarmA(wa_par_LPCSTR(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_BOOL(3)));
}

/*
WINADVAPI WINBOOL WINAPI PrivilegedServiceAuditAlarmA (LPCSTR SubsystemName, LPCSTR ServiceName, HANDLE ClientToken, PPRIVILEGE_SET Privileges, WINBOOL AccessGranted)
*/

/*
WINADVAPI WINBOOL WINAPI SetFileSecurityA (LPCSTR lpFileName, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor)
*/

/*
WINADVAPI WINBOOL WINAPI GetFileSecurityA (LPCSTR lpFileName, SECURITY_INFORMATION RequestedInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor, DWORD nLength, LPDWORD lpnLengthNeeded)
*/

/*
WINBASEAPI WINBOOL WINAPI ReadDirectoryChangesW (HANDLE hDirectory, LPVOID lpBuffer, DWORD nBufferLength, WINBOOL bWatchSubtree, DWORD dwNotifyFilter, LPDWORD lpBytesReturned, LPOVERLAPPED lpOverlapped, LPOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine)
*/

/*
WINBASEAPI WINBOOL WINAPI IsBadReadPtr (CONST VOID *lp, UINT_PTR ucb)
*/
HB_FUNC( WAISBADREADPTR )
{
  wa_ret_BOOL(IsBadReadPtr(static_cast<CONST VOID*>(hb_parptr(1)), wa_par_UINT_PTR(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadWritePtr (LPVOID lp, UINT_PTR ucb)
*/
HB_FUNC( WAISBADWRITEPTR )
{
  wa_ret_BOOL(IsBadWritePtr(static_cast<LPVOID>(hb_parptr(1)), wa_par_UINT_PTR(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadHugeReadPtr (CONST VOID *lp, UINT_PTR ucb)
*/
HB_FUNC( WAISBADHUGEREADPTR )
{
  wa_ret_BOOL(IsBadHugeReadPtr(static_cast<CONST VOID*>(hb_parptr(1)), wa_par_UINT_PTR(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadHugeWritePtr (LPVOID lp, UINT_PTR ucb)
*/
HB_FUNC( WAISBADHUGEWRITEPTR )
{
  wa_ret_BOOL(IsBadHugeWritePtr(static_cast<LPVOID>(hb_parptr(1)), wa_par_UINT_PTR(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadCodePtr (FARPROC lpfn)
*/

/*
WINBASEAPI WINBOOL WINAPI IsBadStringPtrA (LPCSTR lpsz, UINT_PTR ucchMax)
*/
#if 0
HB_FUNC( WAISBADSTRINGPTRA )
{
  wa_ret_BOOL(IsBadStringPtrA(wa_par_LPCSTR(1), wa_par_UINT_PTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI IsBadStringPtrW (LPCWSTR lpsz, UINT_PTR ucchMax)
*/
#if 0
HB_FUNC( WAISBADSTRINGPTRW )
{
  wa_ret_BOOL(IsBadStringPtrW(wa_par_LPCWSTR(1), wa_par_UINT_PTR(2)));
}
#endif

HB_FUNC( WAISBADSTRINGPTR )
{
  void * str1{};
  wa_ret_BOOL(IsBadStringPtr(HB_PARSTR(1, &str1, nullptr), wa_par_UINT_PTR(2)));
  hb_strfree(str1);
}

/*
WINBASEAPI LPVOID WINAPI MapViewOfFileExNuma (HANDLE hFileMappingObject, DWORD dwDesiredAccess, DWORD dwFileOffsetHigh, DWORD dwFileOffsetLow, SIZE_T dwNumberOfBytesToMap, LPVOID lpBaseAddress, DWORD nndPreferred)
*/

/*
WINADVAPI WINBOOL WINAPI AddConditionalAce (PACL pAcl, DWORD dwAceRevision, DWORD AceFlags, UCHAR AceType, DWORD AccessMask, PSID pSid, PWCHAR ConditionStr, DWORD *ReturnLength)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountSidA (LPCSTR lpSystemName, PSID Sid, LPSTR Name, LPDWORD cchName, LPSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountSidW (LPCWSTR lpSystemName, PSID Sid, LPWSTR Name, LPDWORD cchName, LPWSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountNameA (LPCSTR lpSystemName, LPCSTR lpAccountName, PSID Sid, LPDWORD cbSid, LPSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountNameW (LPCWSTR lpSystemName, LPCWSTR lpAccountName, PSID Sid, LPDWORD cbSid, LPWSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountNameLocalA (LPCSTR lpAccountName, PSID Sid, LPDWORD cbSid, LPSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountNameLocalW (LPCWSTR lpAccountName, PSID Sid, LPDWORD cbSid, LPWSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountSidLocalA (PSID Sid, LPSTR Name, LPDWORD cchName, LPSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupAccountSidLocalW (PSID Sid, LPWSTR Name, LPDWORD cchName, LPWSTR ReferencedDomainName, LPDWORD cchReferencedDomainName, PSID_NAME_USE peUse)
*/

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeValueA (LPCSTR lpSystemName, LPCSTR lpName, PLUID lpLuid)
*/

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeValueW (LPCWSTR lpSystemName, LPCWSTR lpName, PLUID lpLuid)
*/

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeNameA (LPCSTR lpSystemName, PLUID lpLuid, LPSTR lpName, LPDWORD cchName)
*/

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeNameW (LPCWSTR lpSystemName, PLUID lpLuid, LPWSTR lpName, LPDWORD cchName)
*/

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeDisplayNameA (LPCSTR lpSystemName, LPCSTR lpName, LPSTR lpDisplayName, LPDWORD cchDisplayName, LPDWORD lpLanguageId)
*/
HB_FUNC( WALOOKUPPRIVILEGEDISPLAYNAMEA ) // TODO: corrigir parametro 3
{
  DWORD cchDisplayName{};
  DWORD LanguageId{};
  wa_ret_BOOL(LookupPrivilegeDisplayNameA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), const_cast<LPSTR>(hb_parc(3)), &cchDisplayName, &LanguageId));
  wa_stor_DWORD(cchDisplayName, 4);
  wa_stor_DWORD(LanguageId, 5);
}

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeDisplayNameW (LPCWSTR lpSystemName, LPCWSTR lpName, LPWSTR lpDisplayName, LPDWORD cchDisplayName, LPDWORD lpLanguageId)
*/
HB_FUNC( WALOOKUPPRIVILEGEDISPLAYNAMEW ) // TODO: corrigir parametro 3
{
  DWORD cchDisplayName{};
  DWORD LanguageId{};
  wa_ret_BOOL(LookupPrivilegeDisplayNameW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), &cchDisplayName, &LanguageId));
  wa_stor_DWORD(cchDisplayName, 4);
  wa_stor_DWORD(LanguageId, 5);
}

#if 0
HB_FUNC( WALOOKUPPRIVILEGEDISPLAYNAME ) // TODO: corrigir parametro 3
{
  void * str1{};
  void * str2{};
  void * str3{};
  DWORD cchDisplayName{};
  DWORD LanguageId{};
  wa_ret_BOOL(LookupPrivilegeDisplayName(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), &cchDisplayName, &LanguageId));
  wa_stor_DWORD(cchDisplayName, 4);
  wa_stor_DWORD(LanguageId, 5);
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI BuildCommDCBA (LPCSTR lpDef, LPDCB lpDCB)
*/

/*
WINBASEAPI WINBOOL WINAPI BuildCommDCBW (LPCWSTR lpDef, LPDCB lpDCB)
*/

/*
WINBASEAPI WINBOOL WINAPI BuildCommDCBAndTimeoutsA (LPCSTR lpDef, LPDCB lpDCB, LPCOMMTIMEOUTS lpCommTimeouts)
*/

/*
WINBASEAPI WINBOOL WINAPI BuildCommDCBAndTimeoutsW (LPCWSTR lpDef, LPDCB lpDCB, LPCOMMTIMEOUTS lpCommTimeouts)
*/

/*
WINBASEAPI WINBOOL WINAPI CommConfigDialogA (LPCSTR lpszName, HWND hWnd, LPCOMMCONFIG lpCC)
*/

/*
WINBASEAPI WINBOOL WINAPI CommConfigDialogW (LPCWSTR lpszName, HWND hWnd, LPCOMMCONFIG lpCC)
*/

/*
WINBASEAPI WINBOOL WINAPI GetDefaultCommConfigA (LPCSTR lpszName, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetDefaultCommConfigW (LPCWSTR lpszName, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetDefaultCommConfigA (LPCSTR lpszName, LPCOMMCONFIG lpCC, DWORD dwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetDefaultCommConfigW (LPCWSTR lpszName, LPCOMMCONFIG lpCC, DWORD dwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetComputerNameA (LPSTR lpBuffer, LPDWORD nSize)
*/
#if 0
HB_FUNC( WAGETCOMPUTERNAMEA ) // TODO: corrigir parametro 1
{
  DWORD nSize{};
  wa_ret_BOOL(GetComputerNameA(const_cast<LPSTR>(hb_parc(1)), &nSize));
  wa_stor_DWORD(nSize, 2);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetComputerNameW (LPWSTR lpBuffer, LPDWORD nSize)
*/
#if 0
HB_FUNC( WAGETCOMPUTERNAMEW ) // TODO: corrigir parametro 1
{
  DWORD nSize{};
  wa_ret_BOOL(GetComputerNameW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), &nSize));
  wa_stor_DWORD(nSize, 2);
}
#endif

HB_FUNC( WAGETCOMPUTERNAME )
{
  DWORD nSize = wa_par_DWORD(2) + 1;
  TCHAR * buffer = new TCHAR[nSize];
  wa_ret_BOOL(GetComputerName(buffer, &nSize));
  HB_STORSTRLEN(buffer, nSize, 1);
  wa_stor_DWORD(nSize, 2);
  delete[] buffer;
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameA (LPCSTR lpComputerName)
*/
#if 0
HB_FUNC( WASETCOMPUTERNAMEA )
{
  wa_ret_BOOL(SetComputerNameA(wa_par_LPCSTR(1)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameW (LPCWSTR lpComputerName)
*/
#if 0
HB_FUNC( WASETCOMPUTERNAMEW )
{
  wa_ret_BOOL(SetComputerNameW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WASETCOMPUTERNAME )
{
  void * str1{};
  wa_ret_BOOL(SetComputerName(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameExA (COMPUTER_NAME_FORMAT NameType, LPCTSTR lpBuffer)
*/

/*
WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameA (LPCSTR Hostname, LPSTR ComputerName, LPDWORD nSize)
*/
HB_FUNC( WADNSHOSTNAMETOCOMPUTERNAMEA )
{
  DWORD nSize{};
  wa_ret_BOOL(DnsHostnameToComputerNameA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), &nSize));
  wa_stor_DWORD(nSize, 3);
}

/*
WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameW (LPCWSTR Hostname, LPWSTR ComputerName, LPDWORD nSize)
*/
HB_FUNC( WADNSHOSTNAMETOCOMPUTERNAMEW )
{
  DWORD nSize{};
  wa_ret_BOOL(DnsHostnameToComputerNameW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), &nSize));
  wa_stor_DWORD(nSize, 3);
}

/*
WINADVAPI WINBOOL WINAPI GetUserNameA (LPSTR lpBuffer, LPDWORD pcbBuffer)
*/
HB_FUNC( WAGETUSERNAMEA ) // TODO: corrigir parametro 1
{
  DWORD pcbBuffer{};
  wa_ret_BOOL(GetUserNameA(const_cast<LPSTR>(hb_parc(1)), &pcbBuffer));
  wa_stor_DWORD(pcbBuffer, 2);
}

/*
WINADVAPI WINBOOL WINAPI GetUserNameW (LPWSTR lpBuffer, LPDWORD pcbBuffer)
*/
HB_FUNC( WAGETUSERNAMEW ) // TODO: corrigir parametro 1
{
  DWORD pcbBuffer{};
  wa_ret_BOOL(GetUserNameW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), &pcbBuffer));
  wa_stor_DWORD(pcbBuffer, 2);
}

/*
WINADVAPI WINBOOL WINAPI LogonUserA (LPCSTR lpszUsername, LPCSTR lpszDomain, LPCSTR lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken)
*/

/*
WINADVAPI WINBOOL WINAPI LogonUserW (LPCWSTR lpszUsername, LPCWSTR lpszDomain, LPCWSTR lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken)
*/

/*
WINADVAPI WINBOOL WINAPI LogonUserExA (LPCSTR lpszUsername, LPCSTR lpszDomain, LPCSTR lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken, PSID *ppLogonSid, PVOID *ppProfileBuffer, LPDWORD pdwProfileLength, PQUOTA_LIMITS pQuotaLimits)
*/

/*
WINADVAPI WINBOOL WINAPI LogonUserExW (LPCWSTR lpszUsername, LPCWSTR lpszDomain, LPCWSTR lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken, PSID *ppLogonSid, PVOID *ppProfileBuffer, LPDWORD pdwProfileLength, PQUOTA_LIMITS pQuotaLimits)
*/

/*
WINADVAPI WINBOOL WINAPI CreateProcessAsUserA (HANDLE hToken, LPCSTR lpApplicationName, LPSTR lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, WINBOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, LPCSTR lpCurrentDirectory, LPSTARTUPINFOA lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation)
*/

/*
WINADVAPI WINBOOL WINAPI CreateProcessWithLogonW (LPCWSTR lpUsername, LPCWSTR lpDomain, LPCWSTR lpPassword, DWORD dwLogonFlags, LPCWSTR lpApplicationName, LPWSTR lpCommandLine, DWORD dwCreationFlags, LPVOID lpEnvironment, LPCWSTR lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation)
*/

/*
WINADVAPI WINBOOL WINAPI CreateProcessWithTokenW (HANDLE hToken, DWORD dwLogonFlags, LPCWSTR lpApplicationName, LPWSTR lpCommandLine, DWORD dwCreationFlags, LPVOID lpEnvironment, LPCWSTR lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation)
*/

/*
WINADVAPI WINBOOL WINAPI IsTokenUntrusted (HANDLE TokenHandle)
*/
HB_FUNC( WAISTOKENUNTRUSTED )
{
  wa_ret_BOOL(IsTokenUntrusted(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI RegisterWaitForSingleObject (PHANDLE phNewWaitObject, HANDLE hObject, WAITORTIMERCALLBACK Callback, PVOID Context, ULONG dwMilliseconds, ULONG dwFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI UnregisterWait (HANDLE WaitHandle)
*/
HB_FUNC( WAUNREGISTERWAIT )
{
  wa_ret_BOOL(UnregisterWait(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI BindIoCompletionCallback (HANDLE FileHandle, LPOVERLAPPED_COMPLETION_ROUTINE Function, ULONG Flags)
*/

/*
WINBASEAPI HANDLE WINAPI SetTimerQueueTimer (HANDLE TimerQueue, WAITORTIMERCALLBACK Callback, PVOID Parameter, DWORD DueTime, DWORD Period, WINBOOL PreferIo)
*/

/*
WINBASEAPI WINBOOL WINAPI CancelTimerQueueTimer (HANDLE TimerQueue, HANDLE Timer)
*/
HB_FUNC( WACANCELTIMERQUEUETIMER )
{
  wa_ret_BOOL(CancelTimerQueueTimer(wa_par_HANDLE(1), wa_par_HANDLE(2)));
}

/*
WINBASEAPI WINBOOL WINAPI DeleteTimerQueue (HANDLE TimerQueue)
*/
HB_FUNC( WADELETETIMERQUEUE )
{
  wa_ret_BOOL(DeleteTimerQueue(wa_par_HANDLE(1)));
}

/*
WINBASEAPI HANDLE WINAPI CreatePrivateNamespaceA (LPSECURITY_ATTRIBUTES lpPrivateNamespaceAttributes, LPVOID lpBoundaryDescriptor, LPCSTR lpAliasPrefix)
*/

/*
WINBASEAPI HANDLE WINAPI OpenPrivateNamespaceA (LPVOID lpBoundaryDescriptor, LPCSTR lpAliasPrefix)
*/
HB_FUNC( WAOPENPRIVATENAMESPACEA )
{
  wa_ret_HANDLE(OpenPrivateNamespaceA(static_cast<LPVOID>(hb_parptr(1)), wa_par_LPCSTR(2)));
}

/*
WINBASEAPI HANDLE APIENTRY CreateBoundaryDescriptorA (LPCSTR Name, ULONG Flags)
*/
HB_FUNC( WACREATEBOUNDARYDESCRIPTORA )
{
  wa_ret_HANDLE(CreateBoundaryDescriptorA(wa_par_LPCSTR(1), wa_par_ULONG(2)));
}

/*
WINBASEAPI WINBOOL WINAPI AddIntegrityLabelToBoundaryDescriptor (HANDLE *BoundaryDescriptor, PSID IntegrityLabel)
*/

/*
WINADVAPI WINBOOL WINAPI GetCurrentHwProfileA (LPHW_PROFILE_INFOA lpHwProfileInfo)
*/

/*
WINADVAPI WINBOOL WINAPI GetCurrentHwProfileW (LPHW_PROFILE_INFOW lpHwProfileInfo)
*/

/*
WINBASEAPI WINBOOL WINAPI VerifyVersionInfoA (LPOSVERSIONINFOEXA lpVersionInformation, DWORD dwTypeMask, DWORDLONG dwlConditionMask)
*/

/*
WINBASEAPI WINBOOL WINAPI VerifyVersionInfoW (LPOSVERSIONINFOEXW lpVersionInformation, DWORD dwTypeMask, DWORDLONG dwlConditionMask)
*/

/*
WINBASEAPI WINBOOL WINAPI GetSystemPowerStatus (LPSYSTEM_POWER_STATUS lpSystemPowerStatus)
*/

/*
WINBASEAPI WINBOOL WINAPI SetSystemPowerState (WINBOOL fSuspend, WINBOOL fForce)
*/
HB_FUNC( WASETSYSTEMPOWERSTATE )
{
  wa_ret_BOOL(SetSystemPowerState(wa_par_BOOL(1), wa_par_BOOL(2)));
}

/*
WINBASEAPI PVOID WINAPI RegisterBadMemoryNotification (PBAD_MEMORY_CALLBACK_ROUTINE Callback)
*/

/*
WINBASEAPI WINBOOL WINAPI UnregisterBadMemoryNotification (PVOID RegistrationHandle)
*/

/*
WINBASEAPI WINBOOL WINAPI GetMemoryErrorHandlingCapabilities (PULONG Capabilities)
*/
#if 0
HB_FUNC( WAGETMEMORYERRORHANDLINGCAPABILITIES )
{
  ULONG Capabilities{};
  wa_ret_BOOL(GetMemoryErrorHandlingCapabilities(&Capabilities));
  wa_stor_DWORD(Capabilities, 1);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI AllocateUserPhysicalPages (HANDLE hProcess, PULONG_PTR NumberOfPages, PULONG_PTR PageArray)
*/

/*
WINBASEAPI WINBOOL WINAPI FreeUserPhysicalPages (HANDLE hProcess, PULONG_PTR NumberOfPages, PULONG_PTR PageArray)
*/

/*
WINBASEAPI WINBOOL WINAPI MapUserPhysicalPages (PVOID VirtualAddress, ULONG_PTR NumberOfPages, PULONG_PTR PageArray)
*/

/*
WINBASEAPI WINBOOL WINAPI MapUserPhysicalPagesScatter (PVOID *VirtualAddresses, ULONG_PTR NumberOfPages, PULONG_PTR PageArray)
*/

/*
WINBASEAPI HANDLE WINAPI CreateJobObjectA (LPSECURITY_ATTRIBUTES lpJobAttributes, LPCSTR lpName)
*/

/*
WINBASEAPI HANDLE WINAPI CreateJobObjectW (LPSECURITY_ATTRIBUTES lpJobAttributes, LPCWSTR lpName)
*/

/*
WINBASEAPI HANDLE WINAPI OpenJobObjectA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
#if 0
HB_FUNC( WAOPENJOBOBJECTA )
{
  wa_ret_HANDLE(OpenJobObjectA(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCSTR(3)));
}
#endif

/*
WINBASEAPI HANDLE WINAPI OpenJobObjectW (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCWSTR lpName)
*/
#if 0
HB_FUNC( WAOPENJOBOBJECTW )
{
  wa_ret_HANDLE(OpenJobObjectW(wa_par_DWORD(1), wa_par_BOOL(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WAOPENJOBOBJECT )
{
  void * str3{};
  wa_ret_HANDLE(OpenJobObject(wa_par_DWORD(1), wa_par_BOOL(2), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str3);
}

/*
WINBASEAPI WINBOOL WINAPI AssignProcessToJobObject (HANDLE hJob, HANDLE hProcess)
*/
HB_FUNC( WAASSIGNPROCESSTOJOBOBJECT )
{
  wa_ret_BOOL(AssignProcessToJobObject(wa_par_HANDLE(1), wa_par_HANDLE(2)));
}

/*
WINBASEAPI WINBOOL WINAPI TerminateJobObject (HANDLE hJob, UINT uExitCode)
*/
HB_FUNC( WATERMINATEJOBOBJECT )
{
  wa_ret_BOOL(TerminateJobObject(wa_par_HANDLE(1), wa_par_UINT(2)));
}

/*
WINBASEAPI WINBOOL WINAPI QueryInformationJobObject (HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInformationClass, LPVOID lpJobObjectInformation, DWORD cbJobObjectInformationLength, LPDWORD lpReturnLength)
*/

/*
WINBASEAPI WINBOOL WINAPI SetInformationJobObject (HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInformationClass, LPVOID lpJobObjectInformation, DWORD cbJobObjectInformationLength)
*/

/*
WINBASEAPI WINBOOL WINAPI CreateJobSet (ULONG NumJob, PJOB_SET_ARRAY UserJobSet, ULONG Flags)
*/

/*
WINBASEAPI HANDLE WINAPI FindFirstVolumeA (LPSTR lpszVolumeName, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDFIRSTVOLUMEA )
{
  wa_ret_HANDLE(FindFirstVolumeA(const_cast<LPSTR>(hb_parc(1)), wa_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeA (HANDLE hFindVolume, LPSTR lpszVolumeName, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDNEXTVOLUMEA )
{
  wa_ret_BOOL(FindNextVolumeA(wa_par_HANDLE(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointA (LPCSTR lpszRootPathName, LPSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDFIRSTVOLUMEMOUNTPOINTA )
{
  wa_ret_HANDLE(FindFirstVolumeMountPointA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointW (LPCWSTR lpszRootPathName, LPWSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDFIRSTVOLUMEMOUNTPOINTW )
{
  wa_ret_HANDLE(FindFirstVolumeMountPointW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointA (HANDLE hFindVolumeMountPoint, LPSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDNEXTVOLUMEMOUNTPOINTA )
{
  wa_ret_BOOL(FindNextVolumeMountPointA(wa_par_HANDLE(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointW (HANDLE hFindVolumeMountPoint, LPWSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WAFINDNEXTVOLUMEMOUNTPOINTW )
{
  wa_ret_BOOL(FindNextVolumeMountPointW(wa_par_HANDLE(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI FindVolumeMountPointClose (HANDLE hFindVolumeMountPoint)
*/
HB_FUNC( WAFINDVOLUMEMOUNTPOINTCLOSE )
{
  wa_ret_BOOL(FindVolumeMountPointClose(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeMountPointA (LPCSTR lpszVolumeMountPoint, LPCSTR lpszVolumeName)
*/
#if 0
HB_FUNC( WASETVOLUMEMOUNTPOINTA )
{
  wa_ret_BOOL(SetVolumeMountPointA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetVolumeMountPointW (LPCWSTR lpszVolumeMountPoint, LPCWSTR lpszVolumeName)
*/
#if 0
HB_FUNC( WASETVOLUMEMOUNTPOINTW )
{
  wa_ret_BOOL(SetVolumeMountPointW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WASETVOLUMEMOUNTPOINT )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(SetVolumeMountPoint(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBASEAPI WINBOOL WINAPI DeleteVolumeMountPointA (LPCSTR lpszVolumeMountPoint)
*/
HB_FUNC( WADELETEVOLUMEMOUNTPOINTA )
{
  wa_ret_BOOL(DeleteVolumeMountPointA(wa_par_LPCSTR(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumeNameForVolumeMountPointA (LPCSTR lpszVolumeMountPoint, LPSTR lpszVolumeName, DWORD cchBufferLength)
*/
HB_FUNC( WAGETVOLUMENAMEFORVOLUMEMOUNTPOINTA )
{
  wa_ret_BOOL(GetVolumeNameForVolumeMountPointA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumePathNameA (LPCSTR lpszFileName, LPSTR lpszVolumePathName, DWORD cchBufferLength)
*/
HB_FUNC( WAGETVOLUMEPATHNAMEA )
{
  wa_ret_BOOL(GetVolumePathNameA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumePathNamesForVolumeNameA (LPCSTR lpszVolumeName, LPCH lpszVolumePathNames, DWORD cchBufferLength, PDWORD lpcchReturnLength)
*/

/*
WINBASEAPI WINBOOL WINAPI AllocateUserPhysicalPagesNuma (HANDLE hProcess, PULONG_PTR NumberOfPages, PULONG_PTR PageArray, DWORD nndPreferred)
*/

/*
WINBASEAPI HANDLE WINAPI CreateActCtxA (PCACTCTXA pActCtx)
*/

/*
WINBASEAPI HANDLE WINAPI CreateActCtxW (PCACTCTXW pActCtx)
*/

/*
WINBASEAPI VOID WINAPI AddRefActCtx (HANDLE hActCtx)
*/
HB_FUNC( WAADDREFACTCTX )
{
  AddRefActCtx(wa_par_HANDLE(1));
}

/*
WINBASEAPI VOID WINAPI ReleaseActCtx (HANDLE hActCtx)
*/
HB_FUNC( WARELEASEACTCTX )
{
  ReleaseActCtx(wa_par_HANDLE(1));
}

/*
WINBASEAPI WINBOOL WINAPI ZombifyActCtx (HANDLE hActCtx)
*/
HB_FUNC( WAZOMBIFYACTCTX )
{
  wa_ret_BOOL(ZombifyActCtx(wa_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI ActivateActCtx (HANDLE hActCtx, ULONG_PTR *lpCookie)
*/

/*
WINBASEAPI WINBOOL WINAPI DeactivateActCtx (DWORD dwFlags, ULONG_PTR ulCookie)
*/
HB_FUNC( WADEACTIVATEACTCTX )
{
  wa_ret_BOOL(DeactivateActCtx(wa_par_DWORD(1), wa_par_ULONG_PTR(2)));
}

/*
WINBASEAPI WINBOOL WINAPI GetCurrentActCtx (HANDLE *lphActCtx)
*/

/*
WINBASEAPI WINBOOL WINAPI FindActCtxSectionStringA (DWORD dwFlags, const GUID *lpExtensionGuid, ULONG ulSectionId, LPCSTR lpStringToFind, PACTCTX_SECTION_KEYED_DATA ReturnedData)
*/

/*
WINBASEAPI WINBOOL WINAPI FindActCtxSectionStringW (DWORD dwFlags, const GUID *lpExtensionGuid, ULONG ulSectionId, LPCWSTR lpStringToFind, PACTCTX_SECTION_KEYED_DATA ReturnedData)
*/

/*
WINBASEAPI WINBOOL WINAPI FindActCtxSectionGuid (DWORD dwFlags, const GUID *lpExtensionGuid, ULONG ulSectionId, const GUID *lpGuidToFind, PACTCTX_SECTION_KEYED_DATA ReturnedData)
*/

/*
WINBASEAPI WINBOOL WINAPI QueryActCtxW (DWORD dwFlags, HANDLE hActCtx, PVOID pvSubInstance, ULONG ulInfoClass, PVOID pvBuffer, SIZE_T cbBuffer, SIZE_T *pcbWrittenOrRequired)
*/

/*
WINBASEAPI DWORD WINAPI WTSGetActiveConsoleSessionId (VOID)
*/
HB_FUNC( WAWTSGETACTIVECONSOLESESSIONID )
{
  wa_ret_DWORD(WTSGetActiveConsoleSessionId());
}

/*
WINBASEAPI WINBOOL WINAPI GetNumaProcessorNode (UCHAR Processor, PUCHAR NodeNumber)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaNodeProcessorMask (UCHAR Node, PULONGLONG ProcessorMask)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaAvailableMemoryNode (UCHAR Node, PULONGLONG AvailableBytes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaProximityNode (ULONG ProximityId, PUCHAR NodeNumber)
*/

/*
WINBASEAPI WORD WINAPI GetActiveProcessorGroupCount (VOID)
*/
#if 0
HB_FUNC( WAGETACTIVEPROCESSORGROUPCOUNT )
{
  wa_ret_WORD(GetActiveProcessorGroupCount());
}
#endif

/*
WINBASEAPI WORD WINAPI GetMaximumProcessorGroupCount (VOID)
*/
#if 0
HB_FUNC( WAGETMAXIMUMPROCESSORGROUPCOUNT )
{
  wa_ret_WORD(GetMaximumProcessorGroupCount());
}
#endif

/*
WINBASEAPI DWORD WINAPI GetActiveProcessorCount (WORD GroupNumber)
*/
#if 0
HB_FUNC( WAGETACTIVEPROCESSORCOUNT )
{
  wa_ret_DWORD(GetActiveProcessorCount(wa_par_WORD(1)));
}
#endif

/*
WINBASEAPI DWORD WINAPI GetMaximumProcessorCount (WORD GroupNumber)
*/
#if 0
HB_FUNC( WAGETMAXIMUMPROCESSORCOUNT )
{
  wa_ret_DWORD(GetMaximumProcessorCount(wa_par_WORD(1)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetNumaNodeNumberFromHandle (HANDLE hFile, PUSHORT NodeNumber)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaProcessorNodeEx (PPROCESSOR_NUMBER Processor, PUSHORT NodeNumber)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaAvailableMemoryNodeEx (USHORT Node, PULONGLONG AvailableBytes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetNumaProximityNodeEx (ULONG ProximityId, PUSHORT NodeNumber)
*/

/*
WINBASEAPI HRESULT WINAPI RegisterApplicationRecoveryCallback (APPLICATION_RECOVERY_CALLBACK pRecoveyCallback, PVOID pvParameter, DWORD dwPingInterval, DWORD dwFlags)
*/

/*
WINBASEAPI HRESULT WINAPI UnregisterApplicationRecoveryCallback (void)
*/

/*
WINBASEAPI HRESULT WINAPI RegisterApplicationRestart (PCWSTR pwzCommandline, DWORD dwFlags)
*/
#if 0
HB_FUNC( WAREGISTERAPPLICATIONRESTART )
{
  wa_ret_HRESULT(RegisterApplicationRestart(static_cast<PCWSTR>(hb_parc(1)), wa_par_DWORD(2)));
}
#endif

/*
WINBASEAPI HRESULT WINAPI UnregisterApplicationRestart (void)
*/
#if 0
HB_FUNC( WAUNREGISTERAPPLICATIONRESTART )
{
  wa_ret_HRESULT(UnregisterApplicationRestart()));
}
#endif

/*
WINBASEAPI HRESULT WINAPI GetApplicationRecoveryCallback (HANDLE hProcess, APPLICATION_RECOVERY_CALLBACK *pRecoveryCallback, PVOID *ppvParameter, PDWORD pdwPingInterval, PDWORD pdwFlags)
*/

/*
WINBASEAPI HRESULT WINAPI GetApplicationRestartSettings (HANDLE hProcess, PWSTR pwzCommandline, PDWORD pcchSize, PDWORD pdwFlags)
*/

/*
WINBASEAPI HRESULT WINAPI ApplicationRecoveryInProgress (PBOOL pbCancelled)
*/
#if 0
HB_FUNC( WAAPPLICATIONRECOVERYINPROGRESS )
{
  BOOL Cancelled{};
  wa_ret_HRESULT(ApplicationRecoveryInProgress(&Cancelled));
  wa_stor_BOOL(Cancelled, 1);
}
#endif

/*
WINBASEAPI VOID WINAPI ApplicationRecoveryFinished (WINBOOL bSuccess)
*/
#if 0
HB_FUNC( WAAPPLICATIONRECOVERYFINISHED )
{
  ApplicationRecoveryFinished(wa_par_BOOL(1));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetFileInformationByHandleEx (HANDLE hFile, FILE_INFO_BY_HANDLE_CLASS FileInformationClass, LPVOID lpFileInformation, DWORD dwBufferSize)
*/

/*
WINBASEAPI HANDLE WINAPI OpenFileById (HANDLE hVolumeHint, LPFILE_ID_DESCRIPTOR lpFileId, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwFlagsAndAttributes)
*/

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkA (LPCSTR lpSymlinkFileName, LPCSTR lpTargetFileName, DWORD dwFlags)
*/
#if 0
HB_FUNC( WACREATESYMBOLICLINKA )
{
  wa_ret_BOOLEAN(CreateSymbolicLinkA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_DWORD(3)));
}
#endif

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkW (LPCWSTR lpSymlinkFileName, LPCWSTR lpTargetFileName, DWORD dwFlags)
*/
#if 0
HB_FUNC( WACREATESYMBOLICLINKW )
{
  wa_ret_BOOLEAN(CreateSymbolicLinkW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_DWORD(3)));
}
#endif

#if 0
HB_FUNC( WACREATESYMBOLICLINK )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOLEAN(CreateSymbolicLink(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}
#endif

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkTransactedA (LPCSTR lpSymlinkFileName, LPCSTR lpTargetFileName, DWORD dwFlags, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WACREATESYMBOLICLINKTRANSACTEDA )
{
  wa_ret_BOOLEAN(CreateSymbolicLinkTransactedA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_DWORD(3), wa_par_HANDLE(4)));
}
#endif

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkTransactedW (LPCWSTR lpSymlinkFileName, LPCWSTR lpTargetFileName, DWORD dwFlags, HANDLE hTransaction)
*/
#if 0
HB_FUNC( WACREATESYMBOLICLINKTRANSACTEDW )
{
  wa_ret_BOOLEAN(CreateSymbolicLinkTransactedW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_DWORD(3), wa_par_HANDLE(4)));
}
#endif

#if 0
HB_FUNC( WACREATESYMBOLICLINKTRANSACTED )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOLEAN(CreateSymbolicLinkTransacted(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3), wa_par_HANDLE(4)));
  hb_strfree(str1);
  hb_strfree(str2);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI QueryActCtxSettingsW (DWORD dwFlags, HANDLE hActCtx, PCWSTR settingsNameSpace, PCWSTR settingName, PWSTR pvBuffer, SIZE_T dwBuffer, SIZE_T *pdwWrittenOrRequired)
*/

/*
WINBASEAPI WINBOOL WINAPI ReplacePartitionUnit (PWSTR TargetPartition, PWSTR SparePartition, ULONG Flags)
*/

/*
WINBASEAPI WINBOOL WINAPI AddSecureMemoryCacheCallback (PSECURE_MEMORY_CACHE_CALLBACK pfnCallBack)
*/

/*
WINBASEAPI WINBOOL WINAPI RemoveSecureMemoryCacheCallback (PSECURE_MEMORY_CACHE_CALLBACK pfnCallBack)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyContext (PCONTEXT Destination, DWORD ContextFlags, PCONTEXT Source)
*/

/*
WINBASEAPI WINBOOL WINAPI InitializeContext (PVOID Buffer, DWORD ContextFlags, PCONTEXT *Context, PDWORD ContextLength)
*/

/*
WINBASEAPI DWORD64 WINAPI GetEnabledXStateFeatures (VOID)
*/

/*
WINBASEAPI WINBOOL WINAPI GetXStateFeaturesMask (PCONTEXT Context, PDWORD64 FeatureMask)
*/

/*
WINBASEAPI PVOID WINAPI LocateXStateFeature (PCONTEXT Context, DWORD FeatureId, PDWORD Length)
*/

/*
WINBASEAPI WINBOOL WINAPI SetXStateFeaturesMask (PCONTEXT Context, DWORD64 FeatureMask)
*/

/*
WINBASEAPI DWORD APIENTRY EnableThreadProfiling (HANDLE ThreadHandle, DWORD Flags, DWORD64 HardwareCounters, HANDLE *PerformanceDataHandle)
*/

/*
WINBASEAPI DWORD APIENTRY DisableThreadProfiling (HANDLE PerformanceDataHandle)
*/

/*
WINBASEAPI DWORD APIENTRY QueryThreadProfiling (HANDLE ThreadHandle, PBOOLEAN Enabled)
*/

/*
WINBASEAPI DWORD APIENTRY ReadThreadProfilingData (HANDLE PerformanceDataHandle, DWORD Flags, PPERFORMANCE_DATA PerformanceData)
*/
