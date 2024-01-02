/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

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
#include <sysinfoapi.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINBASEAPI WINBOOL WINAPI GetComputerNameExA (COMPUTER_NAME_FORMAT NameType, LPSTR lpBuffer, LPDWORD nSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GetComputerNameExW (COMPUTER_NAME_FORMAT NameType, LPWSTR lpBuffer, LPDWORD nSize)
*/

/*
WINBASEAPI UINT WINAPI EnumSystemFirmwareTables (DWORD FirmwareTableProviderSignature, PVOID pFirmwareTableEnumBuffer, DWORD BufferSize)
*/

/*
WINBASEAPI VOID WINAPI GetLocalTime (LPSYSTEMTIME lpSystemTime)
*/
HB_FUNC( WAGETLOCALTIME )
{
  GetLocalTime(static_cast<LPSYSTEMTIME>(winapi_get_ptr(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetLogicalProcessorInformation (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION Buffer, PDWORD ReturnedLength)
*/

/*
WINBASEAPI WINBOOL WINAPI GetLogicalProcessorInformationEx (LOGICAL_PROCESSOR_RELATIONSHIP RelationshipType, PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX Buffer, PDWORD ReturnedLength)
*/
#if _WIN32_WINNT >= 0x0601
#endif

/*
WINBASEAPI VOID WINAPI GetNativeSystemInfo (LPSYSTEM_INFO lpSystemInfo)
*/
HB_FUNC( WAGETNATIVESYSTEMINFO )
{
  GetNativeSystemInfo(static_cast<LPSYSTEM_INFO>(winapi_get_ptr(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetOsSafeBootMode (PDWORD Flags)
*/
#if _WIN32_WINNT >= 0x0601
#endif

/*
WINBASEAPI WINBOOL WINAPI GetProductInfo (DWORD dwOSMajorVersion, DWORD dwOSMinorVersion, DWORD dwSpMajorVersion, DWORD dwSpMinorVersion, PDWORD pdwReturnedProductType)
*/
#if _WIN32_WINNT >= 0x0600
HB_FUNC( WAGETPRODUCTINFO )
{
  DWORD dwReturnedProductType;
  winapi_ret_BOOL(GetProductInfo(wa_par_DWORD(1), wa_par_DWORD(2), wa_par_DWORD(3), wa_par_DWORD(4), &dwReturnedProductType));
  winapi_stor_DWORD(dwReturnedProductType, 5);
}
#endif

/*
WINBASEAPI UINT WINAPI GetSystemDirectoryA (LPSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI UINT WINAPI GetSystemDirectoryW (LPWSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI UINT WINAPI GetSystemFirmwareTable (DWORD FirmwareTableProviderSignature, DWORD FirmwareTableID, PVOID pFirmwareTableBuffer, DWORD BufferSize)
*/

/*
WINBASEAPI VOID WINAPI GetSystemInfo (LPSYSTEM_INFO lpSystemInfo)
*/
HB_FUNC( WAGETSYSTEMINFO )
{
  GetSystemInfo(static_cast<LPSYSTEM_INFO>(winapi_get_ptr(1)));
}

/*
WINBASEAPI VOID WINAPI GetSystemTime (LPSYSTEMTIME lpSystemTime)
*/
HB_FUNC( WAGETSYSTEMTIME )
{
  GetSystemTime(static_cast<LPSYSTEMTIME>(winapi_get_ptr(1)));
}

/*
WINBASEAPI WINBOOL WINAPI GetSystemTimeAdjustment (PDWORD lpTimeAdjustment, PDWORD lpTimeIncrement, PBOOL lpTimeAdjustmentDisabled)
*/
HB_FUNC( WAGETSYSTEMTIMEADJUSTMENT )
{
  DWORD TimeAdjustment;
  DWORD TimeIncrement;
  BOOL TimeAdjustmentDisabled;
  winapi_ret_BOOL(GetSystemTimeAdjustment(&TimeAdjustment, &TimeIncrement, &TimeAdjustmentDisabled));
  winapi_stor_DWORD(TimeAdjustment, 1);
  winapi_stor_DWORD(TimeIncrement, 2);
  winapi_stor_BOOL(TimeAdjustmentDisabled, 3);
}

/*
WINBASEAPI VOID WINAPI GetSystemTimeAsFileTime (LPFILETIME lpSystemTimeAsFileTime)
*/

/*
WINBASEAPI VOID WINAPI GetSystemTimePreciseAsFileTime (LPFILETIME lpSystemTimeAsFileTime)
*/

/*
WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryA (LPSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryW (LPWSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI DWORD WINAPI GetTickCount (VOID)
*/
HB_FUNC( WAGETTICKCOUNT )
{
  winapi_ret_DWORD(GetTickCount());
}

/*
WINBASEAPI ULONGLONG WINAPI GetTickCount64 (VOID)
*/
#if _WIN32_WINNT >= 0x0600
HB_FUNC( WAGETTICKCOUNT64 )
{
  winapi_ret_ULONGLONG(GetTickCount64());
}
#endif

/*
WINBASEAPI DWORD WINAPI GetVersion (VOID)
*/

/*
WINBASEAPI WINBOOL WINAPI GetVersionExA (LPOSVERSIONINFOA lpVersionInformation)
*/

/*
WINBASEAPI WINBOOL WINAPI GetVersionExW (LPOSVERSIONINFOW lpVersionInformation)
*/

/*
WINBASEAPI UINT WINAPI GetWindowsDirectoryA (LPSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI UINT WINAPI GetWindowsDirectoryW (LPWSTR lpBuffer, UINT uSize)
*/

/*
WINBASEAPI WINBOOL WINAPI GlobalMemoryStatusEx (LPMEMORYSTATUSEX lpBuffer)
*/
HB_FUNC( WAGLOBALMEMORYSTATUSEX )
{
  winapi_ret_BOOL(GlobalMemoryStatusEx(static_cast<LPMEMORYSTATUSEX>(winapi_get_ptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI SetComputerNameExW (COMPUTER_NAME_FORMAT NameType, LPCWSTR lpBuffer)
*/

/*
WINBASEAPI WINBOOL WINAPI SetLocalTime (CONST SYSTEMTIME *lpSystemTime)
*/
HB_FUNC( WASETLOCALTIME )
{
  winapi_ret_BOOL(SetLocalTime(static_cast<CONST SYSTEMTIME *>(winapi_get_ptr(1))));
}

/*
WINBASEAPI WINBOOL WINAPI SetSystemTime (CONST SYSTEMTIME *lpSystemTime)
*/
HB_FUNC( WASETSYSTEMTIME )
{
  winapi_ret_BOOL(SetSystemTime(static_cast<CONST SYSTEMTIME *>(winapi_get_ptr(1))));
}

/*
NTSYSAPI ULONGLONG NTAPI VerSetConditionMask (ULONGLONG ConditionMask, ULONG TypeMask, UCHAR Condition)
*/
