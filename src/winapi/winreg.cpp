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
#include <winreg.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "winapi.h"

/*
WINADVAPI LONG WINAPI RegCloseKey(HKEY hKey)
*/
HB_FUNC( WINAPI_REGCLOSEKEY )
{
  hb_retnl(RegCloseKey(static_cast<HKEY>(hb_parptr(1))));
}

/*
WINADVAPI LONG WINAPI RegOverridePredefKey(HKEY hKey,HKEY hNewHKey)
*/
HB_FUNC( WINAPI_REGOVERRIDEPREDEFKEY )
{
  hb_retnl(RegOverridePredefKey(static_cast<HKEY>(hb_parptr(1)), static_cast<HKEY>(hb_parptr(2))));
}

/*
WINADVAPI LONG WINAPI RegOpenUserClassesRoot(HANDLE hToken,DWORD dwOptions,REGSAM samDesired,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegOpenCurrentUser(REGSAM samDesired,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegDisablePredefinedCache(void)
*/
HB_FUNC( WINAPI_REGDISABLEPREDEFINEDCACHE )
{
  hb_retnl(RegDisablePredefinedCache());
}

/*
WINADVAPI LONG WINAPI RegConnectRegistryA(LPCSTR lpMachineName,HKEY hKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegConnectRegistryW(LPCWSTR lpMachineName,HKEY hKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegConnectRegistryExA(LPCSTR lpMachineName,HKEY hKey,ULONG Flags,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegConnectRegistryExW(LPCWSTR lpMachineName,HKEY hKey,ULONG Flags,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyA(HKEY hKey,LPCSTR lpSubKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyW(HKEY hKey,LPCWSTR lpSubKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyExA(HKEY hKey,LPCSTR lpSubKey,DWORD Reserved,LPSTR lpClass,DWORD dwOptions,REGSAM samDesired,LPSECURITY_ATTRIBUTES lpSecurityAttributes,PHKEY phkResult,LPDWORD lpdwDisposition)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyExW(HKEY hKey,LPCWSTR lpSubKey,DWORD Reserved,LPWSTR lpClass,DWORD dwOptions,REGSAM samDesired,LPSECURITY_ATTRIBUTES lpSecurityAttributes,PHKEY phkResult,LPDWORD lpdwDisposition)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyA(HKEY hKey,LPCSTR lpSubKey)
*/
HB_FUNC( WINAPI_REGDELETEKEYA )
{
  hb_retnl(RegDeleteKeyA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegDeleteKeyW(HKEY hKey,LPCWSTR lpSubKey)
*/
HB_FUNC( WINAPI_REGDELETEKEYW )
{
  hb_retnl(RegDeleteKeyW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegDeleteKeyExA(HKEY hKey,LPCSTR lpSubKey,REGSAM samDesired,DWORD Reserved)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyExW(HKEY hKey,LPCWSTR lpSubKey,REGSAM samDesired,DWORD Reserved)
*/

/*
WINADVAPI LONG WINAPI RegDisableReflectionKey(HKEY hBase)
*/
HB_FUNC( WINAPI_REGDISABLEREFLECTIONKEY )
{
  hb_retnl(RegDisableReflectionKey(static_cast<HKEY>(hb_parptr(1))));
}

/*
WINADVAPI LONG WINAPI RegEnableReflectionKey(HKEY hBase)
*/
HB_FUNC( WINAPI_REGENABLEREFLECTIONKEY )
{
  hb_retnl(RegEnableReflectionKey(static_cast<HKEY>(hb_parptr(1))));
}

/*
WINADVAPI LONG WINAPI RegQueryReflectionKey(HKEY hBase,WINBOOL *bIsReflectionDisabled)
*/

/*
WINADVAPI LONG WINAPI RegDeleteValueA(HKEY hKey,LPCSTR lpValueName)
*/
HB_FUNC( WINAPI_REGDELETEVALUEA )
{
  hb_retnl(RegDeleteValueA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegDeleteValueW(HKEY hKey,LPCWSTR lpValueName)
*/
HB_FUNC( WINAPI_REGDELETEVALUEW )
{
  hb_retnl(RegDeleteValueW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegEnumKeyA(HKEY hKey,DWORD dwIndex,LPSTR lpName,DWORD cchName)
*/
HB_FUNC( WINAPI_REGENUMKEYA )
{
  hb_retnl(RegEnumKeyA(static_cast<HKEY>(hb_parptr(1)), winapi_par_DWORD(2), ( LPSTR ) hb_parc(3), winapi_par_DWORD(4)));
}

/*
WINADVAPI LONG WINAPI RegEnumKeyW(HKEY hKey,DWORD dwIndex,LPWSTR lpName,DWORD cchName)
*/
HB_FUNC( WINAPI_REGENUMKEYW )
{
  hb_retnl(RegEnumKeyW(static_cast<HKEY>(hb_parptr(1)), winapi_par_DWORD(2), ( LPWSTR ) hb_parc(3), winapi_par_DWORD(4)));
}

/*
WINADVAPI LONG WINAPI RegEnumKeyExA(HKEY hKey,DWORD dwIndex,LPSTR lpName,LPDWORD lpcchName,LPDWORD lpReserved,LPSTR lpClass,LPDWORD lpcchClass,PFILETIME lpftLastWriteTime)
*/

/*
WINADVAPI LONG WINAPI RegEnumKeyExW(HKEY hKey,DWORD dwIndex,LPWSTR lpName,LPDWORD lpcchName,LPDWORD lpReserved,LPWSTR lpClass,LPDWORD lpcchClass,PFILETIME lpftLastWriteTime)
*/

/*
WINADVAPI LONG WINAPI RegEnumValueA(HKEY hKey,DWORD dwIndex,LPSTR lpValueName,LPDWORD lpcchValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegEnumValueW(HKEY hKey,DWORD dwIndex,LPWSTR lpValueName,LPDWORD lpcchValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegFlushKey(HKEY hKey)
*/
HB_FUNC( WINAPI_REGFLUSHKEY )
{
  hb_retnl(RegFlushKey(static_cast<HKEY>(hb_parptr(1))));
}

/*
WINADVAPI LONG WINAPI RegGetKeySecurity(HKEY hKey,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,LPDWORD lpcbSecurityDescriptor)
*/

/*
WINADVAPI LONG WINAPI RegLoadKeyA(HKEY hKey,LPCSTR lpSubKey,LPCSTR lpFile)
*/
HB_FUNC( WINAPI_REGLOADKEYA )
{
  hb_retnl(RegLoadKeyA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINADVAPI LONG WINAPI RegLoadKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpFile)
*/
HB_FUNC( WINAPI_REGLOADKEYW )
{
  hb_retnl(RegLoadKeyW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3)));
}

/*
WINADVAPI LONG WINAPI RegNotifyChangeKeyValue(HKEY hKey,WINBOOL bWatchSubtree,DWORD dwNotifyFilter,HANDLE hEvent,WINBOOL fAsynchronous)
*/
HB_FUNC( WINAPI_REGNOTIFYCHANGEKEYVALUE )
{
  hb_retnl(RegNotifyChangeKeyValue(static_cast<HKEY>(hb_parptr(1)), hb_parl(2), winapi_par_DWORD(3), winapi_par_HANDLE(4), hb_parl(5)));
}

/*
WINADVAPI LONG WINAPI RegOpenKeyA(HKEY hKey,LPCSTR lpSubKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegOpenKeyW(HKEY hKey,LPCWSTR lpSubKey,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegOpenKeyExA(HKEY hKey,LPCSTR lpSubKey,DWORD ulOptions,REGSAM samDesired,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegOpenKeyExW(HKEY hKey,LPCWSTR lpSubKey,DWORD ulOptions,REGSAM samDesired,PHKEY phkResult)
*/

/*
WINADVAPI LONG WINAPI RegQueryInfoKeyA(HKEY hKey,LPSTR lpClass,LPDWORD lpcchClass,LPDWORD lpReserved,LPDWORD lpcSubKeys,LPDWORD lpcbMaxSubKeyLen,LPDWORD lpcbMaxClassLen,LPDWORD lpcValues,LPDWORD lpcbMaxValueNameLen,LPDWORD lpcbMaxValueLen,LPDWORD lpcbSecurityDescriptor,PFILETIME lpftLastWriteTime)
*/

/*
WINADVAPI LONG WINAPI RegQueryInfoKeyW(HKEY hKey,LPWSTR lpClass,LPDWORD lpcchClass,LPDWORD lpReserved,LPDWORD lpcSubKeys,LPDWORD lpcbMaxSubKeyLen,LPDWORD lpcbMaxClassLen,LPDWORD lpcValues,LPDWORD lpcbMaxValueNameLen,LPDWORD lpcbMaxValueLen,LPDWORD lpcbSecurityDescriptor,PFILETIME lpftLastWriteTime)
*/

/*
WINADVAPI LONG WINAPI RegQueryValueA(HKEY hKey,LPCSTR lpSubKey,LPSTR lpData,PLONG lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegQueryValueW(HKEY hKey,LPCWSTR lpSubKey,LPWSTR lpData,PLONG lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegQueryMultipleValuesA(HKEY hKey,PVALENTA val_list,DWORD num_vals,LPSTR lpValueBuf,LPDWORD ldwTotsize)
*/

/*
WINADVAPI LONG WINAPI RegQueryMultipleValuesW(HKEY hKey,PVALENTW val_list,DWORD num_vals,LPWSTR lpValueBuf,LPDWORD ldwTotsize)
*/

/*
WINADVAPI LONG WINAPI RegQueryValueExA(HKEY hKey,LPCSTR lpValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegQueryValueExW(HKEY hKey,LPCWSTR lpValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)
*/

/*
WINADVAPI LONG WINAPI RegReplaceKeyA(HKEY hKey,LPCSTR lpSubKey,LPCSTR lpNewFile,LPCSTR lpOldFile)
*/
HB_FUNC( WINAPI_REGREPLACEKEYA )
{
  hb_retnl(RegReplaceKeyA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINADVAPI LONG WINAPI RegReplaceKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpNewFile,LPCWSTR lpOldFile)
*/
HB_FUNC( WINAPI_REGREPLACEKEYW )
{
  hb_retnl(RegReplaceKeyW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4)));
}

/*
WINADVAPI LONG WINAPI RegRestoreKeyA(HKEY hKey,LPCSTR lpFile,DWORD dwFlags)
*/
HB_FUNC( WINAPI_REGRESTOREKEYA )
{
  hb_retnl(RegRestoreKeyA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINADVAPI LONG WINAPI RegRestoreKeyW(HKEY hKey,LPCWSTR lpFile,DWORD dwFlags)
*/
HB_FUNC( WINAPI_REGRESTOREKEYW )
{
  hb_retnl(RegRestoreKeyW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINADVAPI LONG WINAPI RegSaveKeyA(HKEY hKey,LPCSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINADVAPI LONG WINAPI RegSaveKeyW(HKEY hKey,LPCWSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes)
*/

/*
WINADVAPI LONG WINAPI RegSetKeySecurity(HKEY hKey,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor)
*/

/*
WINADVAPI LONG WINAPI RegSetValueA(HKEY hKey,LPCSTR lpSubKey,DWORD dwType,LPCSTR lpData,DWORD cbData)
*/
HB_FUNC( WINAPI_REGSETVALUEA )
{
  hb_retnl(RegSetValueA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), winapi_par_DWORD(3), ( LPCSTR ) hb_parc(4), winapi_par_DWORD(5)));
}

/*
WINADVAPI LONG WINAPI RegSetValueW(HKEY hKey,LPCWSTR lpSubKey,DWORD dwType,LPCWSTR lpData,DWORD cbData)
*/
HB_FUNC( WINAPI_REGSETVALUEW )
{
  hb_retnl(RegSetValueW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), winapi_par_DWORD(3), ( LPCWSTR ) hb_parc(4), winapi_par_DWORD(5)));
}

/*
WINADVAPI LONG WINAPI RegSetValueExA(HKEY hKey,LPCSTR lpValueName,DWORD Reserved,DWORD dwType,CONST BYTE *lpData,DWORD cbData)
*/

/*
WINADVAPI LONG WINAPI RegSetValueExW(HKEY hKey,LPCWSTR lpValueName,DWORD Reserved,DWORD dwType,CONST BYTE *lpData,DWORD cbData)
*/

/*
WINADVAPI LONG WINAPI RegUnLoadKeyA(HKEY hKey,LPCSTR lpSubKey)
*/
HB_FUNC( WINAPI_REGUNLOADKEYA )
{
  hb_retnl(RegUnLoadKeyA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegUnLoadKeyW(HKEY hKey,LPCWSTR lpSubKey)
*/
HB_FUNC( WINAPI_REGUNLOADKEYW )
{
  hb_retnl(RegUnLoadKeyW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}

/*
WINADVAPI LONG WINAPI RegGetValueA(HKEY hkey,LPCSTR lpSubKey,LPCSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
*/
HB_FUNC( WINAPI_REGGETVALUEA )
{
  hb_retnl(RegGetValueA(static_cast<HKEY>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), winapi_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5)), static_cast<PVOID>(hb_parptr(6)), static_cast<LPDWORD>(hb_parptr(7))));
}

/*
WINADVAPI LONG WINAPI RegGetValueW(HKEY hkey,LPCWSTR lpSubKey,LPCWSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
*/
HB_FUNC( WINAPI_REGGETVALUEW )
{
  hb_retnl(RegGetValueW(static_cast<HKEY>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), winapi_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5)), static_cast<PVOID>(hb_parptr(6)), static_cast<LPDWORD>(hb_parptr(7))));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
*/
HB_FUNC( WINAPI_INITIATESYSTEMSHUTDOWNA )
{
  winapi_ret_BOOL(InitiateSystemShutdownA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_DWORD(3), hb_parl(4), hb_parl(5)));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
*/
HB_FUNC( WINAPI_INITIATESYSTEMSHUTDOWNW )
{
  winapi_ret_BOOL(InitiateSystemShutdownW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), winapi_par_DWORD(3), hb_parl(4), hb_parl(5)));
}

/*
WINADVAPI WINBOOL WINAPI AbortSystemShutdownA(LPSTR lpMachineName)
*/
HB_FUNC( WINAPI_ABORTSYSTEMSHUTDOWNA )
{
  winapi_ret_BOOL(AbortSystemShutdownA(( LPSTR ) hb_parc(1)));
}

/*
WINADVAPI WINBOOL WINAPI AbortSystemShutdownW(LPWSTR lpMachineName)
*/
HB_FUNC( WINAPI_ABORTSYSTEMSHUTDOWNW )
{
  winapi_ret_BOOL(AbortSystemShutdownW(( LPWSTR ) hb_parc(1)));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
*/
HB_FUNC( WINAPI_INITIATESYSTEMSHUTDOWNEXA )
{
  winapi_ret_BOOL(InitiateSystemShutdownExA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_DWORD(3), hb_parl(4), hb_parl(5), winapi_par_DWORD(6)));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
*/
HB_FUNC( WINAPI_INITIATESYSTEMSHUTDOWNEXW )
{
  winapi_ret_BOOL(InitiateSystemShutdownExW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), winapi_par_DWORD(3), hb_parl(4), hb_parl(5), winapi_par_DWORD(6)));
}

/*
WINADVAPI LONG WINAPI RegSaveKeyExA(HKEY hKey,LPCSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD Flags)
*/

/*
WINADVAPI LONG WINAPI RegSaveKeyExW(HKEY hKey,LPCWSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD Flags)
*/

/*
WINADVAPI LONG WINAPI Wow64Win32ApiEntry (DWORD dwFuncNumber,DWORD dwFlag,DWORD dwRes)
*/

/*
WINADVAPI LONG WINAPI RegCopyTreeA(HKEY hKeySrc, LPCSTR lpSubKey, HKEY hKeyDest)
*/

/*
WINADVAPI LONG WINAPI RegCopyTreeW(HKEY hKeySrc, LPCWSTR lpSubKey, HKEY hKeyDest)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, DWORD Reserved, LPSTR lpClass, DWORD dwOptions, REGSAM samDesired, const LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition, HANDLE hTransaction, PVOID pExtendedParemeter)
*/

/*
WINADVAPI LONG WINAPI RegCreateKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, DWORD Reserved, LPWSTR lpClass, DWORD dwOptions, REGSAM samDesired, const LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition, HANDLE hTransaction, PVOID pExtendedParemeter)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, REGSAM samDesired, DWORD Reserved, HANDLE hTransaction, PVOID pExtendedParameter)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, REGSAM samDesired, DWORD Reserved, HANDLE hTransaction, PVOID pExtendedParameter)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyValueA(HKEY hKey, LPCSTR lpSubKey, LPCSTR lpValueName)
*/

/*
WINADVAPI LONG WINAPI RegDeleteKeyValueW(HKEY hKey, LPCWSTR lpSubKey, LPCWSTR lpValueName)
*/

/*
WINADVAPI LONG WINAPI RegDeleteTreeA(HKEY hKey, LPCSTR lpSubKey)
*/

/*
WINADVAPI LONG WINAPI RegDeleteTreeW(HKEY hKey, LPCWSTR lpSubKey)
*/

/*
WINADVAPI LONG WINAPI RegDisablePredefinedCacheEx(void)
*/

/*
WINADVAPI LONG WINAPI RegLoadAppKeyA(LPCSTR lpFile, PHKEY phkResult, REGSAM samDesired, DWORD dwOptions, DWORD Reserved)
*/

/*
WINADVAPI LONG WINAPI RegLoadAppKeyW(LPCWSTR lpFile, PHKEY phkResult, REGSAM samDesired, DWORD dwOptions, DWORD Reserved)
*/

/*
WINADVAPI LONG WINAPI RegLoadMUIStringA(HKEY hKey, LPCSTR pszValue, LPSTR pszOutBuf, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCSTR pszDirectory)
*/

/*
WINADVAPI LONG WINAPI RegLoadMUIStringW(HKEY hKey, LPCWSTR pszValue, LPWSTR pszOutBuf, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCWSTR pszDirectory)
*/

/*
WINADVAPI LONG WINAPI RegOpenKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult, HANDLE hTransaction, PVOID pExtendedParameter)
*/

/*
WINADVAPI LONG WINAPI RegOpenKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult, HANDLE hTransaction, PVOID pExtendedParameter)
*/

/*
WINADVAPI LONG WINAPI RegSetKeyValueA(HKEY hKey, LPCSTR lpSubKey, LPCSTR lpValueName, DWORD dwType, LPCVOID lpData, DWORD cbData)
*/

/*
WINADVAPI LONG WINAPI RegSetKeyValueW(HKEY hKey, LPCWSTR lpSubKey, LPCWSTR lpValueName, DWORD dwType, LPCVOID lpData, DWORD cbData)
*/

/*
WINADVAPI DWORD WINAPI InitiateShutdownA(LPSTR lpMachineName, LPSTR lpMessage, DWORD dwGracePeriod, DWORD dwShutdownFlags, DWORD dwReason)
*/

/*
WINADVAPI DWORD WINAPI InitiateShutdownW(LPWSTR lpMachineName, LPWSTR lpMessage, DWORD dwGracePeriod, DWORD dwShutdownFlags, DWORD dwReason)
*/
