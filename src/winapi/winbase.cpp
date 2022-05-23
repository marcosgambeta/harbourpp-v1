/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include <winbase.h>
#include <hbapi.h>

/*
WINBASEAPI HLOCAL WINAPI LocalAlloc (UINT uFlags, SIZE_T uBytes)
*/
HB_FUNC( WINAPI_LOCALALLOC )
{
  hb_retptr(LocalAlloc(static_cast<UINT>(hb_parni(1)), static_cast<SIZE_T>(hb_parnl(2))));
}

/*
WINBASEAPI HLOCAL WINAPI LocalFree (HLOCAL hMem)
*/
HB_FUNC( WINAPI_LOCALFREE )
{
  hb_retptr(LocalFree(static_cast<HLOCAL>(hb_parptr(1))));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalAlloc (UINT uFlags, SIZE_T dwBytes)
*/
HB_FUNC( WINAPI_GLOBALALLOC )
{
  hb_retptr(GlobalAlloc(static_cast<UINT>(hb_parni(1)), static_cast<SIZE_T>(hb_parnl(2))));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalReAlloc (HGLOBAL hMem, SIZE_T dwBytes, UINT uFlags)
*/
HB_FUNC( WINAPI_GLOBALREALLOC )
{
  hb_retptr(GlobalReAlloc(static_cast<HGLOBAL>(hb_parptr(1)), static_cast<SIZE_T>(hb_parnl(2)), static_cast<UINT>(hb_parni(3))));
}

/*
WINBASEAPI SIZE_T WINAPI GlobalSize (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALSIZE )
{
  hb_retnl(GlobalSize(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI UINT WINAPI GlobalFlags (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALFLAGS )
{
  hb_retni(GlobalFlags(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI LPVOID WINAPI GlobalLock (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALLOCK )
{
  hb_retptr(GlobalLock(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalHandle (LPCVOID pMem)
*/
HB_FUNC( WINAPI_GLOBALHANDLE )
{
  hb_retptr(GlobalHandle(static_cast<LPCVOID>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI GlobalUnlock (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALUNLOCK )
{
  hb_retl(GlobalUnlock(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI HGLOBAL WINAPI GlobalFree (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALFREE )
{
  hb_retptr(GlobalFree(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI SIZE_T WINAPI GlobalCompact (DWORD dwMinFree)
*/
HB_FUNC( WINAPI_GLOBALCOMPACT )
{
  hb_retnl(GlobalCompact(static_cast<DWORD>(hb_parnl(1))));
}

/*
WINBASEAPI VOID WINAPI GlobalFix (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALFIX )
{
  GlobalFix(static_cast<HGLOBAL>(hb_parptr(1)));
}

/*
WINBASEAPI VOID WINAPI GlobalUnfix (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALUNFIX )
{
  GlobalUnfix(static_cast<HGLOBAL>(hb_parptr(1)));
}

/*
WINBASEAPI LPVOID WINAPI GlobalWire (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALWIRE )
{
  hb_retptr(GlobalWire(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI GlobalUnWire (HGLOBAL hMem)
*/
HB_FUNC( WINAPI_GLOBALUNWIRE )
{
  hb_retl(GlobalUnWire(static_cast<HGLOBAL>(hb_parptr(1))));
}

/*
WINBASEAPI VOID WINAPI GlobalMemoryStatus (LPMEMORYSTATUS lpBuffer)
*/

/*
WINBASEAPI HLOCAL WINAPI LocalReAlloc (HLOCAL hMem, SIZE_T uBytes, UINT uFlags)
*/
HB_FUNC( WINAPI_LOCALREALLOC )
{
  hb_retptr(LocalReAlloc(static_cast<HLOCAL>(hb_parptr(1)), static_cast<SIZE_T>(hb_parnl(2)), static_cast<UINT>(hb_parni(3))));
}

/*
WINBASEAPI LPVOID WINAPI LocalLock (HLOCAL hMem)
*/
HB_FUNC( WINAPI_LOCALLOCK )
{
  hb_retptr(LocalLock(static_cast<HLOCAL>(hb_parptr(1))));
}

/*
WINBASEAPI HLOCAL WINAPI LocalHandle (LPCVOID pMem)
*/
HB_FUNC( WINAPI_LOCALHANDLE )
{
  hb_retptr(LocalHandle(static_cast<LPCVOID>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI LocalUnlock (HLOCAL hMem)
*/
HB_FUNC( WINAPI_LOCALUNLOCK )
{
  hb_retl(LocalUnlock(static_cast<HLOCAL>(hb_parptr(1))));
}

/*
WINBASEAPI SIZE_T WINAPI LocalSize (HLOCAL hMem)
*/
HB_FUNC( WINAPI_LOCALSIZE )
{
  hb_retnl(LocalSize(static_cast<HLOCAL>(hb_parptr(1))));
}

/*
WINBASEAPI UINT WINAPI LocalFlags (HLOCAL hMem)
*/
HB_FUNC( WINAPI_LOCALFLAGS )
{
  hb_retni(LocalFlags(static_cast<HLOCAL>(hb_parptr(1))));
}

/*
WINBASEAPI SIZE_T WINAPI LocalShrink (HLOCAL hMem, UINT cbNewSize)
*/
HB_FUNC( WINAPI_LOCALSHRINK )
{
  hb_retnl(LocalShrink(static_cast<HLOCAL>(hb_parptr(1)), static_cast<UINT>(hb_parni(2))));
}

/*
WINBASEAPI SIZE_T WINAPI LocalCompact (UINT uMinFree)
*/
HB_FUNC( WINAPI_LOCALCOMPACT )
{
  hb_retnl(LocalCompact(static_cast<UINT>(hb_parni(1))));
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
HB_FUNC( WINAPI_GETBINARYTYPEA )
{
  hb_retl(GetBinaryTypeA(( LPCSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI GetBinaryTypeW (LPCWSTR lpApplicationName, LPDWORD lpBinaryType)
*/
HB_FUNC( WINAPI_GETBINARYTYPEW )
{
  hb_retl(GetBinaryTypeW(( LPCWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI DWORD WINAPI GetShortPathNameA (LPCSTR lpszLongPath, LPSTR lpszShortPath, DWORD cchBuffer)
*/
HB_FUNC( WINAPI_GETSHORTPATHNAMEA )
{
  hb_retnl(GetShortPathNameA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
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
HB_FUNC( WINAPI_SETPROCESSWORKINGSETSIZE )
{
  hb_retl(SetProcessWorkingSetSize(static_cast<HANDLE>(hb_parptr(1)), static_cast<SIZE_T>(hb_parnl(2)), static_cast<SIZE_T>(hb_parnl(3))));
}

/*
WINBASEAPI VOID WINAPI FatalExit (int ExitCode)
*/
HB_FUNC( WINAPI_FATALEXIT )
{
  FatalExit(hb_parni(1));
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
HB_FUNC( WINAPI_SETTHREADIDEALPROCESSOR )
{
  hb_retnl(SetThreadIdealProcessor(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
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
HB_FUNC( WINAPI_DELETEFIBER )
{
  DeleteFiber(static_cast<LPVOID>(hb_parptr(1)));
}

/*
WINBASEAPI LPVOID WINAPI ConvertThreadToFiber (LPVOID lpParameter)
*/
HB_FUNC( WINAPI_CONVERTTHREADTOFIBER )
{
  hb_retptr(ConvertThreadToFiber(static_cast<LPVOID>(hb_parptr(1))));
}

/*
WINBASEAPI LPVOID WINAPI ConvertThreadToFiberEx (LPVOID lpParameter, DWORD dwFlags)
*/
HB_FUNC( WINAPI_CONVERTTHREADTOFIBEREX )
{
  hb_retptr(ConvertThreadToFiberEx(static_cast<LPVOID>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI ConvertFiberToThread (VOID)
*/
HB_FUNC( WINAPI_CONVERTFIBERTOTHREAD )
{
  hb_retl(ConvertFiberToThread());
}

/*
WINBASEAPI VOID WINAPI SwitchToFiber (LPVOID lpFiber)
*/
HB_FUNC( WINAPI_SWITCHTOFIBER )
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

/*
WINBASEAPI WINBOOL WINAPI SetProcessPriorityBoost (HANDLE hProcess, WINBOOL bDisablePriorityBoost)
*/
HB_FUNC( WINAPI_SETPROCESSPRIORITYBOOST )
{
  hb_retl(SetProcessPriorityBoost(static_cast<HANDLE>(hb_parptr(1)), hb_parl(2)));
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
HB_FUNC( WINAPI_ISSYSTEMRESUMEAUTOMATIC )
{
  hb_retl(IsSystemResumeAutomatic());
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

/*
WINBASEAPI WINBOOL WINAPI SetFileCompletionNotificationModes (HANDLE FileHandle, UCHAR Flags)
*/

/*
WINBASEAPI WINBOOL WINAPI SetFileIoOverlappedRange (HANDLE FileHandle, PUCHAR OverlappedRangeStart, ULONG Length)
*/

/*
WINBASEAPI DWORD WINAPI GetThreadErrorMode (VOID)
*/
HB_FUNC( WINAPI_GETTHREADERRORMODE )
{
  hb_retnl(GetThreadErrorMode());
}

/*
WINBASEAPI WINBOOL WINAPI SetThreadErrorMode (DWORD dwNewMode, LPDWORD lpOldMode)
*/
HB_FUNC( WINAPI_SETTHREADERRORMODE )
{
  hb_retl(SetThreadErrorMode(static_cast<DWORD>(hb_parnl(1)), static_cast<LPDWORD>(hb_parptr(2))));
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
HB_FUNC( WINAPI_DEBUGSETPROCESSKILLONEXIT )
{
  hb_retl(DebugSetProcessKillOnExit(hb_parl(1)));
}

/*
WINBASEAPI WINBOOL WINAPI DebugBreakProcess (HANDLE Process)
*/
HB_FUNC( WINAPI_DEBUGBREAKPROCESS )
{
  hb_retl(DebugBreakProcess(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI PulseEvent (HANDLE hEvent)
*/
HB_FUNC( WINAPI_PULSEEVENT )
{
  hb_retl(PulseEvent(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI DWORD WINAPI WaitForMultipleObjects (DWORD nCount, CONST HANDLE *lpHandles, WINBOOL bWaitAll, DWORD dwMilliseconds)
*/

/*
WINBASEAPI ATOM WINAPI GlobalDeleteAtom (ATOM nAtom)
*/
HB_FUNC( WINAPI_GLOBALDELETEATOM )
{
  hb_retni(GlobalDeleteAtom(( ATOM ) hb_parni(1)));
}

/*
WINBASEAPI WINBOOL WINAPI InitAtomTable (DWORD nSize)
*/
HB_FUNC( WINAPI_INITATOMTABLE )
{
  hb_retl(InitAtomTable(static_cast<DWORD>(hb_parnl(1))));
}

/*
WINBASEAPI ATOM WINAPI DeleteAtom (ATOM nAtom)
*/
HB_FUNC( WINAPI_DELETEATOM )
{
  hb_retni(DeleteAtom(( ATOM ) hb_parni(1)));
}

/*
WINBASEAPI UINT WINAPI SetHandleCount (UINT uNumber)
*/
HB_FUNC( WINAPI_SETHANDLECOUNT )
{
  hb_retni(SetHandleCount(static_cast<UINT>(hb_parni(1))));
}

/*
WINBASEAPI WINBOOL WINAPI RequestDeviceWakeup (HANDLE hDevice)
*/
HB_FUNC( WINAPI_REQUESTDEVICEWAKEUP )
{
  hb_retl(RequestDeviceWakeup(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI CancelDeviceWakeupRequest (HANDLE hDevice)
*/
HB_FUNC( WINAPI_CANCELDEVICEWAKEUPREQUEST )
{
  hb_retl(CancelDeviceWakeupRequest(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI GetDevicePowerState (HANDLE hDevice, WINBOOL *pfOn)
*/

/*
WINBASEAPI WINBOOL WINAPI SetMessageWaitingIndicator (HANDLE hMsgIndicator, ULONG ulMsgCount)
*/
HB_FUNC( WINAPI_SETMESSAGEWAITINGINDICATOR )
{
  hb_retl(SetMessageWaitingIndicator(static_cast<HANDLE>(hb_parptr(1)), ( ULONG ) hb_parnl(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetFileShortNameA (HANDLE hFile, LPCSTR lpShortName)
*/
HB_FUNC( WINAPI_SETFILESHORTNAMEA )
{
  hb_retl(SetFileShortNameA(static_cast<HANDLE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetFileShortNameW (HANDLE hFile, LPCWSTR lpShortName)
*/
HB_FUNC( WINAPI_SETFILESHORTNAMEW )
{
  hb_retl(SetFileShortNameW(static_cast<HANDLE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI DWORD WINAPI LoadModule (LPCSTR lpModuleName, LPVOID lpParameterBlock)
*/
HB_FUNC( WINAPI_LOADMODULE )
{
  hb_retnl(LoadModule(( LPCSTR ) hb_parc(1), static_cast<LPVOID>(hb_parptr(2))));
}

/*
WINBASEAPI UINT WINAPI WinExec (LPCSTR lpCmdLine, UINT uCmdShow)
*/
HB_FUNC( WINAPI_WINEXEC )
{
  hb_retni(WinExec(( LPCSTR ) hb_parc(1), static_cast<UINT>(hb_parni(2))));
}

/*
WINBASEAPI WINBOOL WINAPI ClearCommBreak (HANDLE hFile)
*/
HB_FUNC( WINAPI_CLEARCOMMBREAK )
{
  hb_retl(ClearCommBreak(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI ClearCommError (HANDLE hFile, LPDWORD lpErrors, LPCOMSTAT lpStat)
*/

/*
WINBASEAPI WINBOOL WINAPI SetupComm (HANDLE hFile, DWORD dwInQueue, DWORD dwOutQueue)
*/
HB_FUNC( WINAPI_SETUPCOMM )
{
  hb_retl(SetupComm(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI EscapeCommFunction (HANDLE hFile, DWORD dwFunc)
*/
HB_FUNC( WINAPI_ESCAPECOMMFUNCTION )
{
  hb_retl(EscapeCommFunction(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommConfig (HANDLE hCommDev, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetCommMask (HANDLE hFile, LPDWORD lpEvtMask)
*/
HB_FUNC( WINAPI_GETCOMMMASK )
{
  hb_retl(GetCommMask(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommProperties (HANDLE hFile, LPCOMMPROP lpCommProp)
*/

/*
WINBASEAPI WINBOOL WINAPI GetCommModemStatus (HANDLE hFile, LPDWORD lpModemStat)
*/
HB_FUNC( WINAPI_GETCOMMMODEMSTATUS )
{
  hb_retl(GetCommModemStatus(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommState (HANDLE hFile, LPDCB lpDCB)
*/

/*
WINBASEAPI WINBOOL WINAPI GetCommTimeouts (HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/

/*
WINBASEAPI WINBOOL WINAPI PurgeComm (HANDLE hFile, DWORD dwFlags)
*/
HB_FUNC( WINAPI_PURGECOMM )
{
  hb_retl(PurgeComm(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommBreak (HANDLE hFile)
*/
HB_FUNC( WINAPI_SETCOMMBREAK )
{
  hb_retl(SetCommBreak(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommConfig (HANDLE hCommDev, LPCOMMCONFIG lpCC, DWORD dwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI SetCommMask (HANDLE hFile, DWORD dwEvtMask)
*/
HB_FUNC( WINAPI_SETCOMMMASK )
{
  hb_retl(SetCommMask(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommState (HANDLE hFile, LPDCB lpDCB)
*/

/*
WINBASEAPI WINBOOL WINAPI SetCommTimeouts (HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/

/*
WINBASEAPI WINBOOL WINAPI TransmitCommChar (HANDLE hFile, char cChar)
*/

/*
WINBASEAPI WINBOOL WINAPI WaitCommEvent (HANDLE hFile, LPDWORD lpEvtMask, LPOVERLAPPED lpOverlapped)
*/

/*
WINBASEAPI DWORD WINAPI SetTapePosition (HANDLE hDevice, DWORD dwPositionMethod, DWORD dwPartition, DWORD dwOffsetLow, DWORD dwOffsetHigh, WINBOOL bImmediate)
*/
HB_FUNC( WINAPI_SETTAPEPOSITION )
{
  hb_retnl(SetTapePosition(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<DWORD>(hb_parnl(3)), static_cast<DWORD>(hb_parnl(4)), static_cast<DWORD>(hb_parnl(5)), hb_parl(6)));
}

/*
WINBASEAPI DWORD WINAPI GetTapePosition (HANDLE hDevice, DWORD dwPositionType, LPDWORD lpdwPartition, LPDWORD lpdwOffsetLow, LPDWORD lpdwOffsetHigh)
*/
HB_FUNC( WINAPI_GETTAPEPOSITION )
{
  hb_retnl(GetTapePosition(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
WINBASEAPI DWORD WINAPI PrepareTape (HANDLE hDevice, DWORD dwOperation, WINBOOL bImmediate)
*/
HB_FUNC( WINAPI_PREPARETAPE )
{
  hb_retnl(PrepareTape(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), hb_parl(3)));
}

/*
WINBASEAPI DWORD WINAPI EraseTape (HANDLE hDevice, DWORD dwEraseType, WINBOOL bImmediate)
*/
HB_FUNC( WINAPI_ERASETAPE )
{
  hb_retnl(EraseTape(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), hb_parl(3)));
}

/*
WINBASEAPI DWORD WINAPI CreateTapePartition (HANDLE hDevice, DWORD dwPartitionMethod, DWORD dwCount, DWORD dwSize)
*/
HB_FUNC( WINAPI_CREATETAPEPARTITION )
{
  hb_retnl(CreateTapePartition(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<DWORD>(hb_parnl(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI DWORD WINAPI WriteTapemark (HANDLE hDevice, DWORD dwTapemarkType, DWORD dwTapemarkCount, WINBOOL bImmediate)
*/
HB_FUNC( WINAPI_WRITETAPEMARK )
{
  hb_retnl(WriteTapemark(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<DWORD>(hb_parnl(3)), hb_parl(4)));
}

/*
WINBASEAPI DWORD WINAPI GetTapeStatus (HANDLE hDevice)
*/
HB_FUNC( WINAPI_GETTAPESTATUS )
{
  hb_retnl(GetTapeStatus(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI DWORD WINAPI GetTapeParameters (HANDLE hDevice, DWORD dwOperation, LPDWORD lpdwSize, LPVOID lpTapeInformation)
*/
HB_FUNC( WINAPI_GETTAPEPARAMETERS )
{
  hb_retnl(GetTapeParameters(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPVOID>(hb_parptr(4))));
}

/*
WINBASEAPI DWORD WINAPI SetTapeParameters (HANDLE hDevice, DWORD dwOperation, LPVOID lpTapeInformation)
*/
HB_FUNC( WINAPI_SETTAPEPARAMETERS )
{
  hb_retnl(SetTapeParameters(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<LPVOID>(hb_parptr(3))));
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

/*
WINBASEAPI WINBOOL WINAPI FileTimeToDosDateTime (CONST FILETIME *lpFileTime, LPWORD lpFatDate, LPWORD lpFatTime)
*/

/*
WINBASEAPI WINBOOL WINAPI DosDateTimeToFileTime (WORD wFatDate, WORD wFatTime, LPFILETIME lpFileTime)
*/

/*
WINBASEAPI WINBOOL WINAPI SetSystemTimeAdjustment (DWORD dwTimeAdjustment, WINBOOL bTimeAdjustmentDisabled)
*/
HB_FUNC( WINAPI_SETSYSTEMTIMEADJUSTMENT )
{
  hb_retl(SetSystemTimeAdjustment(static_cast<DWORD>(hb_parnl(1)), hb_parl(2)));
}

/*
WINBASEAPI int WINAPI MulDiv (int nNumber, int nNumerator, int nDenominator)
*/
HB_FUNC( WINAPI_MULDIV )
{
  hb_retni(MulDiv(hb_parni(1), hb_parni(2), hb_parni(3)));
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
HB_FUNC( WINAPI_GETNAMEDPIPEINFO )
{
  hb_retl(GetNamedPipeInfo(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5))));
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
HB_FUNC( WINAPI_GETMAILSLOTINFO )
{
  hb_retl(GetMailslotInfo(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
WINBASEAPI WINBOOL WINAPI SetMailslotInfo (HANDLE hMailslot, DWORD lReadTimeout)
*/
HB_FUNC( WINAPI_SETMAILSLOTINFO )
{
  hb_retl(SetMailslotInfo(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINADVAPI WINBOOL WINAPI EncryptFileA (LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_ENCRYPTFILEA )
{
  hb_retl(EncryptFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINADVAPI WINBOOL WINAPI EncryptFileW (LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_ENCRYPTFILEW )
{
  hb_retl(EncryptFileW(( LPCWSTR ) hb_parc(1)));
}

/*
WINADVAPI WINBOOL WINAPI DecryptFileA (LPCSTR lpFileName, DWORD dwReserved)
*/
HB_FUNC( WINAPI_DECRYPTFILEA )
{
  hb_retl(DecryptFileA(( LPCSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINADVAPI WINBOOL WINAPI DecryptFileW (LPCWSTR lpFileName, DWORD dwReserved)
*/
HB_FUNC( WINAPI_DECRYPTFILEW )
{
  hb_retl(DecryptFileW(( LPCWSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINADVAPI WINBOOL WINAPI FileEncryptionStatusA (LPCSTR lpFileName, LPDWORD lpStatus)
*/
HB_FUNC( WINAPI_FILEENCRYPTIONSTATUSA )
{
  hb_retl(FileEncryptionStatusA(( LPCSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINADVAPI WINBOOL WINAPI FileEncryptionStatusW (LPCWSTR lpFileName, LPDWORD lpStatus)
*/
HB_FUNC( WINAPI_FILEENCRYPTIONSTATUSW )
{
  hb_retl(FileEncryptionStatusW(( LPCWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
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
HB_FUNC( WINAPI_CLOSEENCRYPTEDFILERAW )
{
  CloseEncryptedFileRaw(static_cast<PVOID>(hb_parptr(1)));
}

/*
WINBASEAPI int WINAPI lstrcmpA (LPCSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCMPA )
{
  hb_retni(lstrcmpA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI int WINAPI lstrcmpW (LPCWSTR lpString1, LPCWSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCMPW )
{
  hb_retni(lstrcmpW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI int WINAPI lstrcmpiA (LPCSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCMPIA )
{
  hb_retni(lstrcmpiA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI int WINAPI lstrcmpiW (LPCWSTR lpString1, LPCWSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCMPIW )
{
  hb_retni(lstrcmpiW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI LPSTR WINAPI lstrcpynA (LPSTR lpString1, LPCSTR lpString2, int iMaxLength)
*/
HB_FUNC( WINAPI_LSTRCPYNA )
{
  hb_retc(( LPSTR ) lstrcpynA(( LPSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), hb_parni(3)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcpynW (LPWSTR lpString1, LPCWSTR lpString2, int iMaxLength)
*/

/*
WINBASEAPI LPSTR WINAPI lstrcpyA (LPSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCPYA )
{
  hb_retc(( LPSTR ) lstrcpyA(( LPSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcpyW (LPWSTR lpString1, LPCWSTR lpString2)
*/

/*
WINBASEAPI LPSTR WINAPI lstrcatA (LPSTR lpString1, LPCSTR lpString2)
*/
HB_FUNC( WINAPI_LSTRCATA )
{
  hb_retc(( LPSTR ) lstrcatA(( LPSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI LPWSTR WINAPI lstrcatW (LPWSTR lpString1, LPCWSTR lpString2)
*/

/*
WINBASEAPI int WINAPI lstrlenA (LPCSTR lpString)
*/
HB_FUNC( WINAPI_LSTRLENA )
{
  hb_retni(lstrlenA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI int WINAPI lstrlenW (LPCWSTR lpString)
*/
HB_FUNC( WINAPI_LSTRLENW )
{
  hb_retni(lstrlenW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI HFILE WINAPI OpenFile (LPCSTR lpFileName, LPOFSTRUCT lpReOpenBuff, UINT uStyle)
*/

/*
WINBASEAPI HFILE WINAPI _lopen (LPCSTR lpPathName, int iReadWrite)
*/
HB_FUNC( WINAPI__LOPEN )
{
  hb_retni(_lopen(( LPCSTR ) hb_parc(1), hb_parni(2)));
}

/*
WINBASEAPI HFILE WINAPI _lcreat (LPCSTR lpPathName, int iAttribute)
*/
HB_FUNC( WINAPI__LCREAT )
{
  hb_retni(_lcreat(( LPCSTR ) hb_parc(1), hb_parni(2)));
}

/*
WINBASEAPI UINT WINAPI _lread (HFILE hFile, LPVOID lpBuffer, UINT uBytes)
*/
HB_FUNC( WINAPI__LREAD )
{
  hb_retni(_lread(( HFILE ) hb_parni(1), static_cast<LPVOID>(hb_parptr(2)), static_cast<UINT>(hb_parni(3))));
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
HB_FUNC( WINAPI__LCLOSE )
{
  hb_retni(_lclose(( HFILE ) hb_parni(1)));
}

/*
WINBASEAPI LONG WINAPI _llseek (HFILE hFile, LONG lOffset, int iOrigin)
*/
HB_FUNC( WINAPI__LLSEEK )
{
  hb_retnl(_llseek(( HFILE ) hb_parni(1), ( LONG ) hb_parnl(2), hb_parni(3)));
}

/*
WINADVAPI WINBOOL WINAPI IsTextUnicode (CONST VOID *lpv, int iSize, LPINT lpiResult)
*/

/*
WINBASEAPI DWORD WINAPI SignalObjectAndWait (HANDLE hObjectToSignal, HANDLE hObjectToWaitOn, DWORD dwMilliseconds, WINBOOL bAlertable)
*/
HB_FUNC( WINAPI_SIGNALOBJECTANDWAIT )
{
  hb_retnl(SignalObjectAndWait(static_cast<HANDLE>(hb_parptr(1)), static_cast<HANDLE>(hb_parptr(2)), static_cast<DWORD>(hb_parnl(3)), hb_parl(4)));
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
WINBASEAPI HMODULE WINAPI LoadLibraryW (LPCWSTR lpLibFileName)
*/
HB_FUNC( WINAPI_LOADLIBRARYW )
{
  hb_retptr(LoadLibraryW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI HANDLE WINAPI OpenMutexA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
HB_FUNC( WINAPI_OPENMUTEXA )
{
  hb_retptr(OpenMutexA(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI HANDLE WINAPI CreateSemaphoreA (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, LPCSTR lpName)
*/

/*
WINBASEAPI HANDLE WINAPI OpenSemaphoreA (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCSTR lpName)
*/
HB_FUNC( WINAPI_OPENSEMAPHOREA )
{
  hb_retptr(OpenSemaphoreA(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_OPENWAITABLETIMERA )
{
  hb_retptr(OpenWaitableTimerA(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_OPENFILEMAPPINGA )
{
  hb_retptr(OpenFileMappingA(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WINAPI_GETLOGICALDRIVESTRINGSA )
{
  hb_retnl(GetLogicalDriveStringsA(static_cast<DWORD>(hb_parnl(1)), ( LPSTR ) hb_parc(2)));
}

/*
WINBASEAPI HMODULE WINAPI LoadLibraryA (LPCSTR lpLibFileName)
*/
HB_FUNC( WINAPI_LOADLIBRARYA )
{
  hb_retptr(LoadLibraryA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI HMODULE WINAPI LoadPackagedLibrary (LPCWSTR lpwLibFileName, DWORD Reserved)
*/

/*
WINBASEAPI WINBOOL WINAPI QueryFullProcessImageNameA (HANDLE hProcess, DWORD dwFlags, LPSTR lpExeName, PDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI QueryFullProcessImageNameW (HANDLE hProcess, DWORD dwFlags, LPWSTR lpExeName, PDWORD lpdwSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetProcessShutdownParameters (LPDWORD lpdwLevel, LPDWORD lpdwFlags)
*/
HB_FUNC( WINAPI_GETPROCESSSHUTDOWNPARAMETERS )
{
  hb_retl(GetProcessShutdownParameters(static_cast<LPDWORD>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI VOID WINAPI FatalAppExitA (UINT uAction, LPCSTR lpMessageText)
*/
HB_FUNC( WINAPI_FATALAPPEXITA )
{
  FatalAppExitA(static_cast<UINT>(hb_parni(1)), ( LPCSTR ) hb_parc(2));
}

/*
WINBASEAPI VOID WINAPI FatalAppExitW (UINT uAction, LPCWSTR lpMessageText)
*/
HB_FUNC( WINAPI_FATALAPPEXITW )
{
  FatalAppExitW(static_cast<UINT>(hb_parni(1)), ( LPCWSTR ) hb_parc(2));
}

/*
WINBASEAPI VOID WINAPI GetStartupInfoA (LPSTARTUPINFOA lpStartupInfo)
*/

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableA (LPCSTR lpName, LPCSTR lpGuid, PVOID pBuffer, DWORD nSize)
*/
HB_FUNC( WINAPI_GETFIRMWAREENVIRONMENTVARIABLEA )
{
  hb_retnl(GetFirmwareEnvironmentVariableA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<PVOID>(hb_parptr(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI DWORD WINAPI GetFirmwareEnvironmentVariableW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pBuffer, DWORD nSize)
*/
HB_FUNC( WINAPI_GETFIRMWAREENVIRONMENTVARIABLEW )
{
  hb_retnl(GetFirmwareEnvironmentVariableW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), static_cast<PVOID>(hb_parptr(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableA (LPCSTR lpName, LPCSTR lpGuid, PVOID pValue, DWORD nSize)
*/
HB_FUNC( WINAPI_SETFIRMWAREENVIRONMENTVARIABLEA )
{
  hb_retl(SetFirmwareEnvironmentVariableA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<PVOID>(hb_parptr(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pValue, DWORD nSize)
*/
HB_FUNC( WINAPI_SETFIRMWAREENVIRONMENTVARIABLEW )
{
  hb_retl(SetFirmwareEnvironmentVariableW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), static_cast<PVOID>(hb_parptr(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI HRSRC WINAPI FindResourceA (HMODULE hModule, LPCSTR lpName, LPCSTR lpType)
*/

/*
WINBASEAPI HRSRC WINAPI FindResourceW (HMODULE hModule, LPCWSTR lpName, LPCWSTR lpType)
*/

/*
WINBASEAPI HRSRC WINAPI FindResourceExA (HMODULE hModule, LPCSTR lpType, LPCSTR lpName, WORD wLanguage)
*/

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
HB_FUNC( WINAPI_BEGINUPDATERESOURCEA )
{
  hb_retptr(BeginUpdateResourceA(( LPCSTR ) hb_parc(1), hb_parl(2)));
}

/*
WINBASEAPI HANDLE WINAPI BeginUpdateResourceW (LPCWSTR pFileName, WINBOOL bDeleteExistingResources)
*/
HB_FUNC( WINAPI_BEGINUPDATERESOURCEW )
{
  hb_retptr(BeginUpdateResourceW(( LPCWSTR ) hb_parc(1), hb_parl(2)));
}

/*
WINBASEAPI WINBOOL WINAPI UpdateResourceA (HANDLE hUpdate, LPCSTR lpType, LPCSTR lpName, WORD wLanguage, LPVOID lpData, DWORD cb)
*/
HB_FUNC( WINAPI_UPDATERESOURCEA )
{
  hb_retl(UpdateResourceA(static_cast<HANDLE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), static_cast<WORD>(hb_parni(4)), static_cast<LPVOID>(hb_parptr(5)), static_cast<DWORD>(hb_parnl(6))));
}

/*
WINBASEAPI WINBOOL WINAPI UpdateResourceW (HANDLE hUpdate, LPCWSTR lpType, LPCWSTR lpName, WORD wLanguage, LPVOID lpData, DWORD cb)
*/
HB_FUNC( WINAPI_UPDATERESOURCEW )
{
  hb_retl(UpdateResourceW(static_cast<HANDLE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( WORD ) hb_parni(4), static_cast<LPVOID>(hb_parptr(5)), static_cast<DWORD>(hb_parnl(6))));
}

/*
WINBASEAPI WINBOOL WINAPI EndUpdateResourceA (HANDLE hUpdate, WINBOOL fDiscard)
*/
HB_FUNC( WINAPI_ENDUPDATERESOURCEA )
{
  hb_retl(EndUpdateResourceA(static_cast<HANDLE>(hb_parptr(1)), hb_parl(2)));
}

/*
WINBASEAPI WINBOOL WINAPI EndUpdateResourceW (HANDLE hUpdate, WINBOOL fDiscard)
*/
HB_FUNC( WINAPI_ENDUPDATERESOURCEW )
{
  hb_retl(EndUpdateResourceW(static_cast<HANDLE>(hb_parptr(1)), hb_parl(2)));
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

/*
WINBASEAPI WINBOOL WINAPI SetFirmwareEnvironmentVariableExW (LPCWSTR lpName, LPCWSTR lpGuid, PVOID pValue, DWORD nSize, DWORD dwAttributes)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFirmwareType (PFIRMWARE_TYPE FirmwareType)
*/

/*
WINBASEAPI WINBOOL WINAPI IsNativeVhdBoot (PBOOL NativeVhdBoot)
*/

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomA (LPCSTR lpString)
*/
HB_FUNC( WINAPI_GLOBALADDATOMA )
{
  hb_retni(GlobalAddAtomA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomW (LPCWSTR lpString)
*/
HB_FUNC( WINAPI_GLOBALADDATOMW )
{
  hb_retni(GlobalAddAtomW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomExA (LPCSTR lpString, DWORD Flags)
*/
HB_FUNC( WINAPI_GLOBALADDATOMEXA )
{
  hb_retni(GlobalAddAtomExA(( LPCSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI ATOM WINAPI GlobalAddAtomExW (LPCWSTR lpString, DWORD Flags)
*/
HB_FUNC( WINAPI_GLOBALADDATOMEXW )
{
  hb_retni(GlobalAddAtomExW(( LPCWSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI ATOM WINAPI GlobalFindAtomA (LPCSTR lpString)
*/
HB_FUNC( WINAPI_GLOBALFINDATOMA )
{
  hb_retni(GlobalFindAtomA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI GlobalFindAtomW (LPCWSTR lpString)
*/
HB_FUNC( WINAPI_GLOBALFINDATOMW )
{
  hb_retni(GlobalFindAtomW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI UINT WINAPI GlobalGetAtomNameA (ATOM nAtom, LPSTR lpBuffer, int nSize)
*/
HB_FUNC( WINAPI_GLOBALGETATOMNAMEA )
{
  hb_retni(GlobalGetAtomNameA(( ATOM ) hb_parni(1), ( LPSTR ) hb_parc(2), hb_parni(3)));
}

/*
WINBASEAPI UINT WINAPI GlobalGetAtomNameW (ATOM nAtom, LPWSTR lpBuffer, int nSize)
*/
HB_FUNC( WINAPI_GLOBALGETATOMNAMEW )
{
  hb_retni(GlobalGetAtomNameW(( ATOM ) hb_parni(1), ( LPWSTR ) hb_parc(2), hb_parni(3)));
}

/*
WINBASEAPI ATOM WINAPI AddAtomA (LPCSTR lpString)
*/
HB_FUNC( WINAPI_ADDATOMA )
{
  hb_retni(AddAtomA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI AddAtomW (LPCWSTR lpString)
*/
HB_FUNC( WINAPI_ADDATOMW )
{
  hb_retni(AddAtomW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI FindAtomA (LPCSTR lpString)
*/
HB_FUNC( WINAPI_FINDATOMA )
{
  hb_retni(FindAtomA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI ATOM WINAPI FindAtomW (LPCWSTR lpString)
*/
HB_FUNC( WINAPI_FINDATOMW )
{
  hb_retni(FindAtomW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI UINT WINAPI GetAtomNameA (ATOM nAtom, LPSTR lpBuffer, int nSize)
*/
HB_FUNC( WINAPI_GETATOMNAMEA )
{
  hb_retni(GetAtomNameA(( ATOM ) hb_parni(1), ( LPSTR ) hb_parc(2), hb_parni(3)));
}

/*
WINBASEAPI UINT WINAPI GetAtomNameW (ATOM nAtom, LPWSTR lpBuffer, int nSize)
*/
HB_FUNC( WINAPI_GETATOMNAMEW )
{
  hb_retni(GetAtomNameW(( ATOM ) hb_parni(1), ( LPWSTR ) hb_parc(2), hb_parni(3)));
}

/*
WINBASEAPI UINT WINAPI GetProfileIntA (LPCSTR lpAppName, LPCSTR lpKeyName, INT nDefault)
*/
HB_FUNC( WINAPI_GETPROFILEINTA )
{
  hb_retni(GetProfileIntA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( INT ) hb_parni(3)));
}

/*
WINBASEAPI UINT WINAPI GetProfileIntW (LPCWSTR lpAppName, LPCWSTR lpKeyName, INT nDefault)
*/
HB_FUNC( WINAPI_GETPROFILEINTW )
{
  hb_retni(GetProfileIntW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( INT ) hb_parni(3)));
}

/*
WINBASEAPI DWORD WINAPI GetProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpDefault, LPSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WINAPI_GETPROFILESTRINGA )
{
  hb_retnl(GetProfileStringA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
WINBASEAPI DWORD WINAPI GetProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpDefault, LPWSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WINAPI_GETPROFILESTRINGW )
{
  hb_retnl(GetProfileStringW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPWSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpString)
*/
HB_FUNC( WINAPI_WRITEPROFILESTRINGA )
{
  hb_retl(WriteProfileStringA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpString)
*/
HB_FUNC( WINAPI_WRITEPROFILESTRINGW )
{
  hb_retl(WriteProfileStringW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3)));
}

/*
WINBASEAPI DWORD WINAPI GetProfileSectionA (LPCSTR lpAppName, LPSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WINAPI_GETPROFILESECTIONA )
{
  hb_retnl(GetProfileSectionA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI DWORD WINAPI GetProfileSectionW (LPCWSTR lpAppName, LPWSTR lpReturnedString, DWORD nSize)
*/
HB_FUNC( WINAPI_GETPROFILESECTIONW )
{
  hb_retnl(GetProfileSectionW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileSectionA (LPCSTR lpAppName, LPCSTR lpString)
*/
HB_FUNC( WINAPI_WRITEPROFILESECTIONA )
{
  hb_retl(WriteProfileSectionA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI WriteProfileSectionW (LPCWSTR lpAppName, LPCWSTR lpString)
*/
HB_FUNC( WINAPI_WRITEPROFILESECTIONW )
{
  hb_retl(WriteProfileSectionW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI UINT WINAPI GetPrivateProfileIntA (LPCSTR lpAppName, LPCSTR lpKeyName, INT nDefault, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILEINTA )
{
  hb_retni(GetPrivateProfileIntA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( INT ) hb_parni(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINBASEAPI UINT WINAPI GetPrivateProfileIntW (LPCWSTR lpAppName, LPCWSTR lpKeyName, INT nDefault, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILEINTW )
{
  hb_retni(GetPrivateProfileIntW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( INT ) hb_parni(3), ( LPCWSTR ) hb_parc(4)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpDefault, LPSTR lpReturnedString, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESTRINGA )
{
  hb_retnl(GetPrivateProfileStringA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5)), ( LPCSTR ) hb_parc(6)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpDefault, LPWSTR lpReturnedString, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESTRINGW )
{
  hb_retnl(GetPrivateProfileStringW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPWSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5)), ( LPCWSTR ) hb_parc(6)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringA (LPCSTR lpAppName, LPCSTR lpKeyName, LPCSTR lpString, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESTRINGA )
{
  hb_retl(WritePrivateProfileStringA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStringW (LPCWSTR lpAppName, LPCWSTR lpKeyName, LPCWSTR lpString, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESTRINGW )
{
  hb_retl(WritePrivateProfileStringW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionA (LPCSTR lpAppName, LPSTR lpReturnedString, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESECTIONA )
{
  hb_retnl(GetPrivateProfileSectionA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3)), ( LPCSTR ) hb_parc(4)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionW (LPCWSTR lpAppName, LPWSTR lpReturnedString, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESECTIONW )
{
  hb_retnl(GetPrivateProfileSectionW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3)), ( LPCWSTR ) hb_parc(4)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionA (LPCSTR lpAppName, LPCSTR lpString, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESECTIONA )
{
  hb_retl(WritePrivateProfileSectionA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileSectionW (LPCWSTR lpAppName, LPCWSTR lpString, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESECTIONW )
{
  hb_retl(WritePrivateProfileSectionW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesA (LPSTR lpszReturnBuffer, DWORD nSize, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESECTIONNAMESA )
{
  hb_retnl(GetPrivateProfileSectionNamesA(( LPSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesW (LPWSTR lpszReturnBuffer, DWORD nSize, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESECTIONNAMESW )
{
  hb_retnl(GetPrivateProfileSectionNamesW(( LPWSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), ( LPCWSTR ) hb_parc(3)));
}

/*
WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructA (LPCSTR lpszSection, LPCSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCSTR szFile)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESTRUCTA )
{
  hb_retl(GetPrivateProfileStructA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<UINT>(hb_parni(4)), ( LPCSTR ) hb_parc(5)));
}

/*
WINBASEAPI WINBOOL WINAPI GetPrivateProfileStructW (LPCWSTR lpszSection, LPCWSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCWSTR szFile)
*/
HB_FUNC( WINAPI_GETPRIVATEPROFILESTRUCTW )
{
  hb_retl(GetPrivateProfileStructW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<UINT>(hb_parni(4)), ( LPCWSTR ) hb_parc(5)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructA (LPCSTR lpszSection, LPCSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCSTR szFile)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESTRUCTA )
{
  hb_retl(WritePrivateProfileStructA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<UINT>(hb_parni(4)), ( LPCSTR ) hb_parc(5)));
}

/*
WINBASEAPI WINBOOL WINAPI WritePrivateProfileStructW (LPCWSTR lpszSection, LPCWSTR lpszKey, LPVOID lpStruct, UINT uSizeStruct, LPCWSTR szFile)
*/
HB_FUNC( WINAPI_WRITEPRIVATEPROFILESTRUCTW )
{
  hb_retl(WritePrivateProfileStructW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<UINT>(hb_parni(4)), ( LPCWSTR ) hb_parc(5)));
}

/*
WINBASEAPI DWORD WINAPI GetTempPathA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WINAPI_GETTEMPPATHA )
{
  hb_retnl(GetTempPathA(static_cast<DWORD>(hb_parnl(1)), ( LPSTR ) hb_parc(2)));
}

/*
WINBASEAPI UINT WINAPI GetTempFileNameA (LPCSTR lpPathName, LPCSTR lpPrefixString, UINT uUnique, LPSTR lpTempFileName)
*/
HB_FUNC( WINAPI_GETTEMPFILENAMEA )
{
  hb_retni(GetTempFileNameA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<UINT>(hb_parni(3)), ( LPSTR ) hb_parc(4)));
}

/*
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryA (LPSTR lpBuffer, UINT uSize)
*/
HB_FUNC( WINAPI_GETSYSTEMWOW64DIRECTORYA )
{
  hb_retni(GetSystemWow64DirectoryA(( LPSTR ) hb_parc(1), ( UINT ) hb_parni(2)));
}

/*
WINBASEAPI UINT WINAPI GetSystemWow64DirectoryW (LPWSTR lpBuffer, UINT uSize)
*/
HB_FUNC( WINAPI_GETSYSTEMWOW64DIRECTORYW )
{
  hb_retni(GetSystemWow64DirectoryW(( LPWSTR ) hb_parc(1), static_cast<UINT>(hb_parni(2))));
}

/*
WINBASEAPI BOOLEAN WINAPI Wow64EnableWow64FsRedirection (BOOLEAN Wow64FsEnableRedirection)
*/
HB_FUNC( WINAPI_WOW64ENABLEWOW64FSREDIRECTION )
{
  hb_retl(Wow64EnableWow64FsRedirection(hb_parl(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetDllDirectoryA (LPCSTR lpPathName)
*/
HB_FUNC( WINAPI_SETDLLDIRECTORYA )
{
  hb_retl(SetDllDirectoryA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetDllDirectoryW (LPCWSTR lpPathName)
*/
HB_FUNC( WINAPI_SETDLLDIRECTORYW )
{
  hb_retl(SetDllDirectoryW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI DWORD WINAPI GetDllDirectoryA (DWORD nBufferLength, LPSTR lpBuffer)
*/
HB_FUNC( WINAPI_GETDLLDIRECTORYA )
{
  hb_retnl(GetDllDirectoryA(static_cast<DWORD>(hb_parnl(1)), ( LPSTR ) hb_parc(2)));
}

/*
WINBASEAPI DWORD WINAPI GetDllDirectoryW (DWORD nBufferLength, LPWSTR lpBuffer)
*/
HB_FUNC( WINAPI_GETDLLDIRECTORYW )
{
  hb_retnl(GetDllDirectoryW(static_cast<DWORD>(hb_parnl(1)), ( LPWSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetSearchPathMode (DWORD Flags)
*/
HB_FUNC( WINAPI_SETSEARCHPATHMODE )
{
  hb_retl(SetSearchPathMode(static_cast<DWORD>(hb_parnl(1))));
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

/*
WINBASEAPI WINBOOL WINAPI RemoveDirectoryTransactedW (LPCWSTR lpPathName, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetFullPathNameTransactedA (LPCSTR lpFileName, DWORD nBufferLength, LPSTR lpBuffer, LPSTR *lpFilePart, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetFullPathNameTransactedW (LPCWSTR lpFileName, DWORD nBufferLength, LPWSTR lpBuffer, LPWSTR *lpFilePart, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI DefineDosDeviceA (DWORD dwFlags, LPCSTR lpDeviceName, LPCSTR lpTargetPath)
*/
HB_FUNC( WINAPI_DEFINEDOSDEVICEA )
{
  hb_retl(DefineDosDeviceA(static_cast<DWORD>(hb_parnl(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI DWORD WINAPI QueryDosDeviceA (LPCSTR lpDeviceName, LPSTR lpTargetPath, DWORD ucchMax)
*/
HB_FUNC( WINAPI_QUERYDOSDEVICEA )
{
  hb_retnl(QueryDosDeviceA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
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
HB_FUNC( WINAPI_REOPENFILE )
{
  hb_retptr(ReOpenFile(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<DWORD>(hb_parnl(3)), static_cast<DWORD>(hb_parnl(4))));
}

/*
WINBASEAPI WINBOOL WINAPI SetFileAttributesTransactedA (LPCSTR lpFileName, DWORD dwFileAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI SetFileAttributesTransactedW (LPCWSTR lpFileName, DWORD dwFileAttributes, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFileAttributesTransactedA (LPCSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFileAttributesTransactedW (LPCWSTR lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeA (LPCSTR lpFileName, LPDWORD lpFileSizeHigh)
*/
HB_FUNC( WINAPI_GETCOMPRESSEDFILESIZEA )
{
  hb_retnl(GetCompressedFileSizeA(( LPCSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeW (LPCWSTR lpFileName, LPDWORD lpFileSizeHigh)
*/
HB_FUNC( WINAPI_GETCOMPRESSEDFILESIZEW )
{
  hb_retnl(GetCompressedFileSizeW(( LPCWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeTransactedA (LPCSTR lpFileName, LPDWORD lpFileSizeHigh, HANDLE hTransaction)
*/

/*
WINBASEAPI DWORD WINAPI GetCompressedFileSizeTransactedW (LPCWSTR lpFileName, LPDWORD lpFileSizeHigh, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI DeleteFileTransactedA (LPCSTR lpFileName, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI DeleteFileTransactedW (LPCWSTR lpFileName, HANDLE hTransaction)
*/

/*
WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3A (LPCSTR lpName, LPSTR lpOemName, DWORD OemNameSize, PBOOL pbNameContainsSpaces, PBOOL pbNameLegal)
*/

/*
WINBASEAPI WINBOOL WINAPI CheckNameLegalDOS8Dot3W (LPCWSTR lpName, LPSTR lpOemName, DWORD OemNameSize, PBOOL pbNameContainsSpaces, PBOOL pbNameLegal)
*/

/*
WINBASEAPI WINBOOL WINAPI CopyFileA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, WINBOOL bFailIfExists)
*/
HB_FUNC( WINAPI_COPYFILEA )
{
  hb_retl(CopyFileA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), hb_parl(3)));
}

/*
WINBASEAPI WINBOOL WINAPI CopyFileW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, WINBOOL bFailIfExists)
*/
HB_FUNC( WINAPI_COPYFILEW )
{
  hb_retl(CopyFileW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), hb_parl(3)));
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
HB_FUNC( WINAPI_MOVEFILEA )
{
  hb_retl(MoveFileA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI MoveFileW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName)
*/
HB_FUNC( WINAPI_MOVEFILEW )
{
  hb_retl(MoveFileW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI MoveFileExA (LPCSTR lpExistingFileName, LPCSTR lpNewFileName, DWORD dwFlags)
*/
HB_FUNC( WINAPI_MOVEFILEEXA )
{
  hb_retl(MoveFileExA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI MoveFileExW (LPCWSTR lpExistingFileName, LPCWSTR lpNewFileName, DWORD dwFlags)
*/
HB_FUNC( WINAPI_MOVEFILEEXW )
{
  hb_retl(MoveFileExW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
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
HB_FUNC( WINAPI_REPLACEFILEA )
{
  hb_retl(ReplaceFileA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), static_cast<DWORD>(hb_parnl(4)), static_cast<LPVOID>(hb_parptr(5)), static_cast<LPVOID>(hb_parptr(6))));
}

/*
WINBASEAPI WINBOOL WINAPI ReplaceFileW (LPCWSTR lpReplacedFileName, LPCWSTR lpReplacementFileName, LPCWSTR lpBackupFileName, DWORD dwReplaceFlags, LPVOID lpExclude, LPVOID lpReserved)
*/
HB_FUNC( WINAPI_REPLACEFILEW )
{
  hb_retl(ReplaceFileW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), static_cast<DWORD>(hb_parnl(4)), static_cast<LPVOID>(hb_parptr(5)), static_cast<LPVOID>(hb_parptr(6))));
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
HB_FUNC( WINAPI_FINDNEXTSTREAMW )
{
  hb_retl(FindNextStreamW(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2))));
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
HB_FUNC( WINAPI_GETNAMEDPIPEHANDLESTATEA )
{
  hb_retl(GetNamedPipeHandleStateA(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5)), ( LPSTR ) hb_parc(6), static_cast<DWORD>(hb_parnl(7))));
}

/*
WINBASEAPI WINBOOL WINAPI GetNamedPipeHandleStateW (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, LPWSTR lpUserName, DWORD nMaxUserNameSize)
*/
HB_FUNC( WINAPI_GETNAMEDPIPEHANDLESTATEW )
{
  hb_retl(GetNamedPipeHandleStateW(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5)), ( LPWSTR ) hb_parc(6), static_cast<DWORD>(hb_parnl(7))));
}

/*
WINBASEAPI WINBOOL WINAPI CallNamedPipeA (LPCSTR lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut)
*/
HB_FUNC( WINAPI_CALLNAMEDPIPEA )
{
  hb_retl(CallNamedPipeA(( LPCSTR ) hb_parc(1), static_cast<LPVOID>(hb_parptr(2)), static_cast<DWORD>(hb_parnl(3)), static_cast<LPVOID>(hb_parptr(4)), static_cast<DWORD>(hb_parnl(5)), static_cast<LPDWORD>(hb_parptr(6)), static_cast<DWORD>(hb_parnl(7))));
}

/*
WINBASEAPI WINBOOL WINAPI CallNamedPipeW (LPCWSTR lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut)
*/
HB_FUNC( WINAPI_CALLNAMEDPIPEW )
{
  hb_retl(CallNamedPipeW(( LPCWSTR ) hb_parc(1), static_cast<LPVOID>(hb_parptr(2)), static_cast<DWORD>(hb_parnl(3)), static_cast<LPVOID>(hb_parptr(4)), static_cast<DWORD>(hb_parnl(5)), static_cast<LPDWORD>(hb_parptr(6)), static_cast<DWORD>(hb_parnl(7))));
}

/*
WINBASEAPI WINBOOL WINAPI WaitNamedPipeA (LPCSTR lpNamedPipeName, DWORD nTimeOut)
*/
HB_FUNC( WINAPI_WAITNAMEDPIPEA )
{
  hb_retl(WaitNamedPipeA(( LPCSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeLabelA (LPCSTR lpRootPathName, LPCSTR lpVolumeName)
*/
HB_FUNC( WINAPI_SETVOLUMELABELA )
{
  hb_retl(SetVolumeLabelA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeLabelW (LPCWSTR lpRootPathName, LPCWSTR lpVolumeName)
*/
HB_FUNC( WINAPI_SETVOLUMELABELW )
{
  hb_retl(SetVolumeLabelW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI VOID WINAPI SetFileApisToOEM (VOID)
*/
HB_FUNC( WINAPI_SETFILEAPISTOOEM )
{
  SetFileApisToOEM();
}

/*
WINBASEAPI VOID WINAPI SetFileApisToANSI (VOID)
*/
HB_FUNC( WINAPI_SETFILEAPISTOANSI )
{
  SetFileApisToANSI();
}

/*
WINBASEAPI WINBOOL WINAPI AreFileApisANSI (VOID)
*/
HB_FUNC( WINAPI_AREFILEAPISANSI )
{
  hb_retl(AreFileApisANSI());
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumeInformationA (LPCSTR lpRootPathName, LPSTR lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, LPSTR lpFileSystemNameBuffer, DWORD nFileSystemNameSize)
*/
HB_FUNC( WINAPI_GETVOLUMEINFORMATIONA )
{
  hb_retl(GetVolumeInformationA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3)), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5)), static_cast<LPDWORD>(hb_parptr(6)), ( LPSTR ) hb_parc(7), static_cast<DWORD>(hb_parnl(8))));
}

/*
WINADVAPI WINBOOL WINAPI ClearEventLogA (HANDLE hEventLog, LPCSTR lpBackupFileName)
*/
HB_FUNC( WINAPI_CLEAREVENTLOGA )
{
  hb_retl(ClearEventLogA(static_cast<HANDLE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI WINBOOL WINAPI ClearEventLogW (HANDLE hEventLog, LPCWSTR lpBackupFileName)
*/
HB_FUNC( WINAPI_CLEAREVENTLOGW )
{
  hb_retl(ClearEventLogW(static_cast<HANDLE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI WINBOOL WINAPI BackupEventLogA (HANDLE hEventLog, LPCSTR lpBackupFileName)
*/
HB_FUNC( WINAPI_BACKUPEVENTLOGA )
{
  hb_retl(BackupEventLogA(static_cast<HANDLE>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI WINBOOL WINAPI BackupEventLogW (HANDLE hEventLog, LPCWSTR lpBackupFileName)
*/
HB_FUNC( WINAPI_BACKUPEVENTLOGW )
{
  hb_retl(BackupEventLogW(static_cast<HANDLE>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI WINBOOL WINAPI CloseEventLog (HANDLE hEventLog)
*/
HB_FUNC( WINAPI_CLOSEEVENTLOG )
{
  hb_retl(CloseEventLog(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINADVAPI WINBOOL WINAPI DeregisterEventSource (HANDLE hEventLog)
*/
HB_FUNC( WINAPI_DEREGISTEREVENTSOURCE )
{
  hb_retl(DeregisterEventSource(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINADVAPI WINBOOL WINAPI NotifyChangeEventLog (HANDLE hEventLog, HANDLE hEvent)
*/
HB_FUNC( WINAPI_NOTIFYCHANGEEVENTLOG )
{
  hb_retl(NotifyChangeEventLog(static_cast<HANDLE>(hb_parptr(1)), static_cast<HANDLE>(hb_parptr(2))));
}

/*
WINADVAPI WINBOOL WINAPI GetNumberOfEventLogRecords (HANDLE hEventLog, PDWORD NumberOfRecords)
*/

/*
WINADVAPI WINBOOL WINAPI GetOldestEventLogRecord (HANDLE hEventLog, PDWORD OldestRecord)
*/

/*
WINADVAPI HANDLE WINAPI OpenEventLogA (LPCSTR lpUNCServerName, LPCSTR lpSourceName)
*/
HB_FUNC( WINAPI_OPENEVENTLOGA )
{
  hb_retptr(OpenEventLogA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI HANDLE WINAPI OpenEventLogW (LPCWSTR lpUNCServerName, LPCWSTR lpSourceName)
*/
HB_FUNC( WINAPI_OPENEVENTLOGW )
{
  hb_retptr(OpenEventLogW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI HANDLE WINAPI RegisterEventSourceA (LPCSTR lpUNCServerName, LPCSTR lpSourceName)
*/
HB_FUNC( WINAPI_REGISTEREVENTSOURCEA )
{
  hb_retptr(RegisterEventSourceA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI HANDLE WINAPI RegisterEventSourceW (LPCWSTR lpUNCServerName, LPCWSTR lpSourceName)
*/
HB_FUNC( WINAPI_REGISTEREVENTSOURCEW )
{
  hb_retptr(RegisterEventSourceW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI HANDLE WINAPI OpenBackupEventLogA (LPCSTR lpUNCServerName, LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_OPENBACKUPEVENTLOGA )
{
  hb_retptr(OpenBackupEventLogA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI HANDLE WINAPI OpenBackupEventLogW (LPCWSTR lpUNCServerName, LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_OPENBACKUPEVENTLOGW )
{
  hb_retptr(OpenBackupEventLogW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
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
HB_FUNC( WINAPI_GETEVENTLOGINFORMATION )
{
  hb_retl(GetEventLogInformation(static_cast<HANDLE>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2)), static_cast<LPVOID>(hb_parptr(3)), static_cast<DWORD>(hb_parnl(4)), static_cast<LPDWORD>(hb_parptr(5))));
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
HB_FUNC( WINAPI_OBJECTCLOSEAUDITALARMA )
{
  hb_retl(ObjectCloseAuditAlarmA(( LPCSTR ) hb_parc(1), static_cast<LPVOID>(hb_parptr(2)), hb_parl(3)));
}

/*
WINADVAPI WINBOOL WINAPI ObjectDeleteAuditAlarmA (LPCSTR SubsystemName, LPVOID HandleId, WINBOOL GenerateOnClose)
*/
HB_FUNC( WINAPI_OBJECTDELETEAUDITALARMA )
{
  hb_retl(ObjectDeleteAuditAlarmA(( LPCSTR ) hb_parc(1), static_cast<LPVOID>(hb_parptr(2)), hb_parl(3)));
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

/*
WINBASEAPI WINBOOL WINAPI IsBadWritePtr (LPVOID lp, UINT_PTR ucb)
*/
HB_FUNC( WINAPI_ISBADWRITEPTR )
{
  hb_retl(IsBadWritePtr(static_cast<LPVOID>(hb_parptr(1)), ( UINT_PTR ) hb_parni(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadHugeReadPtr (CONST VOID *lp, UINT_PTR ucb)
*/

/*
WINBASEAPI WINBOOL WINAPI IsBadHugeWritePtr (LPVOID lp, UINT_PTR ucb)
*/
HB_FUNC( WINAPI_ISBADHUGEWRITEPTR )
{
  hb_retl(IsBadHugeWritePtr(static_cast<LPVOID>(hb_parptr(1)), ( UINT_PTR ) hb_parni(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadCodePtr (FARPROC lpfn)
*/

/*
WINBASEAPI WINBOOL WINAPI IsBadStringPtrA (LPCSTR lpsz, UINT_PTR ucchMax)
*/
HB_FUNC( WINAPI_ISBADSTRINGPTRA )
{
  hb_retl(IsBadStringPtrA(( LPCSTR ) hb_parc(1), ( UINT_PTR ) hb_parni(2)));
}

/*
WINBASEAPI WINBOOL WINAPI IsBadStringPtrW (LPCWSTR lpsz, UINT_PTR ucchMax)
*/
HB_FUNC( WINAPI_ISBADSTRINGPTRW )
{
  hb_retl(IsBadStringPtrW(( LPCWSTR ) hb_parc(1), ( UINT_PTR ) hb_parni(2)));
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
HB_FUNC( WINAPI_LOOKUPPRIVILEGEDISPLAYNAMEA )
{
  hb_retl(LookupPrivilegeDisplayNameA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
WINADVAPI WINBOOL WINAPI LookupPrivilegeDisplayNameW (LPCWSTR lpSystemName, LPCWSTR lpName, LPWSTR lpDisplayName, LPDWORD cchDisplayName, LPDWORD lpLanguageId)
*/
HB_FUNC( WINAPI_LOOKUPPRIVILEGEDISPLAYNAMEW )
{
  hb_retl(LookupPrivilegeDisplayNameW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), static_cast<LPDWORD>(hb_parptr(4)), static_cast<LPDWORD>(hb_parptr(5))));
}

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
HB_FUNC( WINAPI_GETCOMPUTERNAMEA )
{
  hb_retl(GetComputerNameA(( LPSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI GetComputerNameW (LPWSTR lpBuffer, LPDWORD nSize)
*/
HB_FUNC( WINAPI_GETCOMPUTERNAMEW )
{
  hb_retl(GetComputerNameW(( LPWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameA (LPCSTR lpComputerName)
*/
HB_FUNC( WINAPI_SETCOMPUTERNAMEA )
{
  hb_retl(SetComputerNameA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameW (LPCWSTR lpComputerName)
*/
HB_FUNC( WINAPI_SETCOMPUTERNAMEW )
{
  hb_retl(SetComputerNameW(( LPCWSTR ) hb_parc(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameExA (COMPUTER_NAME_FORMAT NameType, LPCTSTR lpBuffer)
*/

/*
WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameA (LPCSTR Hostname, LPSTR ComputerName, LPDWORD nSize)
*/
HB_FUNC( WINAPI_DNSHOSTNAMETOCOMPUTERNAMEA )
{
  hb_retl(DnsHostnameToComputerNameA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
WINBASEAPI WINBOOL WINAPI DnsHostnameToComputerNameW (LPCWSTR Hostname, LPWSTR ComputerName, LPDWORD nSize)
*/
HB_FUNC( WINAPI_DNSHOSTNAMETOCOMPUTERNAMEW )
{
  hb_retl(DnsHostnameToComputerNameW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
WINADVAPI WINBOOL WINAPI GetUserNameA (LPSTR lpBuffer, LPDWORD pcbBuffer)
*/
HB_FUNC( WINAPI_GETUSERNAMEA )
{
  hb_retl(GetUserNameA(( LPSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINADVAPI WINBOOL WINAPI GetUserNameW (LPWSTR lpBuffer, LPDWORD pcbBuffer)
*/
HB_FUNC( WINAPI_GETUSERNAMEW )
{
  hb_retl(GetUserNameW(( LPWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
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
HB_FUNC( WINAPI_ISTOKENUNTRUSTED )
{
  hb_retl(IsTokenUntrusted(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI RegisterWaitForSingleObject (PHANDLE phNewWaitObject, HANDLE hObject, WAITORTIMERCALLBACK Callback, PVOID Context, ULONG dwMilliseconds, ULONG dwFlags)
*/

/*
WINBASEAPI WINBOOL WINAPI UnregisterWait (HANDLE WaitHandle)
*/
HB_FUNC( WINAPI_UNREGISTERWAIT )
{
  hb_retl(UnregisterWait(static_cast<HANDLE>(hb_parptr(1))));
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
HB_FUNC( WINAPI_CANCELTIMERQUEUETIMER )
{
  hb_retl(CancelTimerQueueTimer(static_cast<HANDLE>(hb_parptr(1)), static_cast<HANDLE>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI DeleteTimerQueue (HANDLE TimerQueue)
*/
HB_FUNC( WINAPI_DELETETIMERQUEUE )
{
  hb_retl(DeleteTimerQueue(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI HANDLE WINAPI CreatePrivateNamespaceA (LPSECURITY_ATTRIBUTES lpPrivateNamespaceAttributes, LPVOID lpBoundaryDescriptor, LPCSTR lpAliasPrefix)
*/

/*
WINBASEAPI HANDLE WINAPI OpenPrivateNamespaceA (LPVOID lpBoundaryDescriptor, LPCSTR lpAliasPrefix)
*/
HB_FUNC( WINAPI_OPENPRIVATENAMESPACEA )
{
  hb_retptr(OpenPrivateNamespaceA(static_cast<LPVOID>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI HANDLE APIENTRY CreateBoundaryDescriptorA (LPCSTR Name, ULONG Flags)
*/
HB_FUNC( WINAPI_CREATEBOUNDARYDESCRIPTORA )
{
  hb_retptr(CreateBoundaryDescriptorA(( LPCSTR ) hb_parc(1), ( ULONG ) hb_parnl(2)));
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
HB_FUNC( WINAPI_SETSYSTEMPOWERSTATE )
{
  hb_retl(SetSystemPowerState(hb_parl(1), hb_parl(2)));
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
HB_FUNC( WINAPI_OPENJOBOBJECTA )
{
  hb_retptr(OpenJobObjectA(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINBASEAPI HANDLE WINAPI OpenJobObjectW (DWORD dwDesiredAccess, WINBOOL bInheritHandle, LPCWSTR lpName)
*/
HB_FUNC( WINAPI_OPENJOBOBJECTW )
{
  hb_retptr(OpenJobObjectW(static_cast<DWORD>(hb_parnl(1)), hb_parl(2), ( LPCWSTR ) hb_parc(3)));
}

/*
WINBASEAPI WINBOOL WINAPI AssignProcessToJobObject (HANDLE hJob, HANDLE hProcess)
*/
HB_FUNC( WINAPI_ASSIGNPROCESSTOJOBOBJECT )
{
  hb_retl(AssignProcessToJobObject(static_cast<HANDLE>(hb_parptr(1)), static_cast<HANDLE>(hb_parptr(2))));
}

/*
WINBASEAPI WINBOOL WINAPI TerminateJobObject (HANDLE hJob, UINT uExitCode)
*/
HB_FUNC( WINAPI_TERMINATEJOBOBJECT )
{
  hb_retl(TerminateJobObject(static_cast<HANDLE>(hb_parptr(1)), static_cast<UINT>(hb_parni(2))));
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
HB_FUNC( WINAPI_FINDFIRSTVOLUMEA )
{
  hb_retptr(FindFirstVolumeA(( LPSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2))));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeA (HANDLE hFindVolume, LPSTR lpszVolumeName, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_FINDNEXTVOLUMEA )
{
  hb_retl(FindNextVolumeA(static_cast<HANDLE>(hb_parptr(1)), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointA (LPCSTR lpszRootPathName, LPSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_FINDFIRSTVOLUMEMOUNTPOINTA )
{
  hb_retptr(FindFirstVolumeMountPointA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI HANDLE WINAPI FindFirstVolumeMountPointW (LPCWSTR lpszRootPathName, LPWSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_FINDFIRSTVOLUMEMOUNTPOINTW )
{
  hb_retptr(FindFirstVolumeMountPointW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointA (HANDLE hFindVolumeMountPoint, LPSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_FINDNEXTVOLUMEMOUNTPOINTA )
{
  hb_retl(FindNextVolumeMountPointA(static_cast<HANDLE>(hb_parptr(1)), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI FindNextVolumeMountPointW (HANDLE hFindVolumeMountPoint, LPWSTR lpszVolumeMountPoint, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_FINDNEXTVOLUMEMOUNTPOINTW )
{
  hb_retl(FindNextVolumeMountPointW(static_cast<HANDLE>(hb_parptr(1)), ( LPWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI FindVolumeMountPointClose (HANDLE hFindVolumeMountPoint)
*/
HB_FUNC( WINAPI_FINDVOLUMEMOUNTPOINTCLOSE )
{
  hb_retl(FindVolumeMountPointClose(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeMountPointA (LPCSTR lpszVolumeMountPoint, LPCSTR lpszVolumeName)
*/
HB_FUNC( WINAPI_SETVOLUMEMOUNTPOINTA )
{
  hb_retl(SetVolumeMountPointA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetVolumeMountPointW (LPCWSTR lpszVolumeMountPoint, LPCWSTR lpszVolumeName)
*/
HB_FUNC( WINAPI_SETVOLUMEMOUNTPOINTW )
{
  hb_retl(SetVolumeMountPointW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINBASEAPI WINBOOL WINAPI DeleteVolumeMountPointA (LPCSTR lpszVolumeMountPoint)
*/
HB_FUNC( WINAPI_DELETEVOLUMEMOUNTPOINTA )
{
  hb_retl(DeleteVolumeMountPointA(( LPCSTR ) hb_parc(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumeNameForVolumeMountPointA (LPCSTR lpszVolumeMountPoint, LPSTR lpszVolumeName, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_GETVOLUMENAMEFORVOLUMEMOUNTPOINTA )
{
  hb_retl(GetVolumeNameForVolumeMountPointA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
}

/*
WINBASEAPI WINBOOL WINAPI GetVolumePathNameA (LPCSTR lpszFileName, LPSTR lpszVolumePathName, DWORD cchBufferLength)
*/
HB_FUNC( WINAPI_GETVOLUMEPATHNAMEA )
{
  hb_retl(GetVolumePathNameA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3))));
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
HB_FUNC( WINAPI_ADDREFACTCTX )
{
  AddRefActCtx(static_cast<HANDLE>(hb_parptr(1)));
}

/*
WINBASEAPI VOID WINAPI ReleaseActCtx (HANDLE hActCtx)
*/
HB_FUNC( WINAPI_RELEASEACTCTX )
{
  ReleaseActCtx(static_cast<HANDLE>(hb_parptr(1)));
}

/*
WINBASEAPI WINBOOL WINAPI ZombifyActCtx (HANDLE hActCtx)
*/
HB_FUNC( WINAPI_ZOMBIFYACTCTX )
{
  hb_retl(ZombifyActCtx(static_cast<HANDLE>(hb_parptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI ActivateActCtx (HANDLE hActCtx, ULONG_PTR *lpCookie)
*/

/*
WINBASEAPI WINBOOL WINAPI DeactivateActCtx (DWORD dwFlags, ULONG_PTR ulCookie)
*/
HB_FUNC( WINAPI_DEACTIVATEACTCTX )
{
  hb_retl(DeactivateActCtx(static_cast<DWORD>(hb_parnl(1)), ( ULONG_PTR ) hb_parnl(2)));
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
HB_FUNC( WINAPI_WTSGETACTIVECONSOLESESSIONID )
{
  hb_retnl(WTSGetActiveConsoleSessionId());
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

/*
WINBASEAPI WORD WINAPI GetMaximumProcessorGroupCount (VOID)
*/

/*
WINBASEAPI DWORD WINAPI GetActiveProcessorCount (WORD GroupNumber)
*/

/*
WINBASEAPI DWORD WINAPI GetMaximumProcessorCount (WORD GroupNumber)
*/

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

/*
WINBASEAPI HRESULT WINAPI UnregisterApplicationRestart (void)
*/

/*
WINBASEAPI HRESULT WINAPI GetApplicationRecoveryCallback (HANDLE hProcess, APPLICATION_RECOVERY_CALLBACK *pRecoveryCallback, PVOID *ppvParameter, PDWORD pdwPingInterval, PDWORD pdwFlags)
*/

/*
WINBASEAPI HRESULT WINAPI GetApplicationRestartSettings (HANDLE hProcess, PWSTR pwzCommandline, PDWORD pcchSize, PDWORD pdwFlags)
*/

/*
WINBASEAPI HRESULT WINAPI ApplicationRecoveryInProgress (PBOOL pbCancelled)
*/

/*
WINBASEAPI VOID WINAPI ApplicationRecoveryFinished (WINBOOL bSuccess)
*/

/*
WINBASEAPI WINBOOL WINAPI GetFileInformationByHandleEx (HANDLE hFile, FILE_INFO_BY_HANDLE_CLASS FileInformationClass, LPVOID lpFileInformation, DWORD dwBufferSize)
*/

/*
WINBASEAPI HANDLE WINAPI OpenFileById (HANDLE hVolumeHint, LPFILE_ID_DESCRIPTOR lpFileId, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwFlagsAndAttributes)
*/

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkA (LPCSTR lpSymlinkFileName, LPCSTR lpTargetFileName, DWORD dwFlags)
*/

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkW (LPCWSTR lpSymlinkFileName, LPCWSTR lpTargetFileName, DWORD dwFlags)
*/

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkTransactedA (LPCSTR lpSymlinkFileName, LPCSTR lpTargetFileName, DWORD dwFlags, HANDLE hTransaction)
*/

/*
WINBASEAPI BOOLEAN APIENTRY CreateSymbolicLinkTransactedW (LPCWSTR lpSymlinkFileName, LPCWSTR lpTargetFileName, DWORD dwFlags, HANDLE hTransaction)
*/

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
