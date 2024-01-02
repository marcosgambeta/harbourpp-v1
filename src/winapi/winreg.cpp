/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

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
#include <winreg.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINADVAPI LONG WINAPI RegCloseKey(HKEY hKey)
*/
HB_FUNC( WAREGCLOSEKEY )
{
  winapi_ret_LONG(RegCloseKey(winapi_par_HKEY(1)));
}

/*
WINADVAPI LONG WINAPI RegOverridePredefKey(HKEY hKey,HKEY hNewHKey)
*/
HB_FUNC( WAREGOVERRIDEPREDEFKEY )
{
  winapi_ret_LONG(RegOverridePredefKey(winapi_par_HKEY(1), winapi_par_HKEY(2)));
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
HB_FUNC( WAREGDISABLEPREDEFINEDCACHE )
{
  winapi_ret_LONG(RegDisablePredefinedCache());
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
HB_FUNC( WAREGDELETEKEYA )
{
  winapi_ret_LONG(RegDeleteKeyA(winapi_par_HKEY(1), winapi_par_LPCSTR(2)));
}

/*
WINADVAPI LONG WINAPI RegDeleteKeyW(HKEY hKey,LPCWSTR lpSubKey)
*/
HB_FUNC( WAREGDELETEKEYW )
{
  winapi_ret_LONG(RegDeleteKeyW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2))));
}

HB_FUNC( WAREGDELETEKEY )
{
  void * str2;
  winapi_ret_LONG(RegDeleteKey(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
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
HB_FUNC( WAREGDISABLEREFLECTIONKEY )
{
  winapi_ret_LONG(RegDisableReflectionKey(winapi_par_HKEY(1)));
}

/*
WINADVAPI LONG WINAPI RegEnableReflectionKey(HKEY hBase)
*/
HB_FUNC( WAREGENABLEREFLECTIONKEY )
{
  winapi_ret_LONG(RegEnableReflectionKey(winapi_par_HKEY(1)));
}

/*
WINADVAPI LONG WINAPI RegQueryReflectionKey(HKEY hBase,WINBOOL *bIsReflectionDisabled)
*/
HB_FUNC( WAREGQUERYREFLECTIONKEY )
{
  BOOL bIsReflectionDisabled;
  winapi_ret_LONG(RegQueryReflectionKey(winapi_par_HKEY(1), &bIsReflectionDisabled));
  winapi_stor_BOOL(bIsReflectionDisabled, 2);
}

/*
WINADVAPI LONG WINAPI RegDeleteValueA(HKEY hKey,LPCSTR lpValueName)
*/
HB_FUNC( WAREGDELETEVALUEA )
{
  winapi_ret_LONG(RegDeleteValueA(winapi_par_HKEY(1), winapi_par_LPCSTR(2)));
}

/*
WINADVAPI LONG WINAPI RegDeleteValueW(HKEY hKey,LPCWSTR lpValueName)
*/
HB_FUNC( WAREGDELETEVALUEW )
{
  winapi_ret_LONG(RegDeleteValueW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2))));
}

HB_FUNC( WAREGDELETEVALUE )
{
  void * str2;
  winapi_ret_LONG(RegDeleteValue(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINADVAPI LONG WINAPI RegEnumKeyA(HKEY hKey,DWORD dwIndex,LPSTR lpName,DWORD cchName)
*/
HB_FUNC( WAREGENUMKEYA )
{
  winapi_ret_LONG(RegEnumKeyA(winapi_par_HKEY(1), winapi_par_DWORD(2), const_cast<LPSTR>(hb_parc(3)), winapi_par_DWORD(4)));
}

/*
WINADVAPI LONG WINAPI RegEnumKeyW(HKEY hKey,DWORD dwIndex,LPWSTR lpName,DWORD cchName)
*/
HB_FUNC( WAREGENUMKEYW )
{
  winapi_ret_LONG(RegEnumKeyW(winapi_par_HKEY(1), winapi_par_DWORD(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), winapi_par_DWORD(4)));
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
HB_FUNC( WAREGFLUSHKEY )
{
  winapi_ret_LONG(RegFlushKey(winapi_par_HKEY(1)));
}

/*
WINADVAPI LONG WINAPI RegGetKeySecurity(HKEY hKey,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,LPDWORD lpcbSecurityDescriptor)
*/

/*
WINADVAPI LONG WINAPI RegLoadKeyA(HKEY hKey,LPCSTR lpSubKey,LPCSTR lpFile)
*/
HB_FUNC( WAREGLOADKEYA )
{
  winapi_ret_LONG(RegLoadKeyA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3)));
}

/*
WINADVAPI LONG WINAPI RegLoadKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpFile)
*/
HB_FUNC( WAREGLOADKEYW )
{
  winapi_ret_LONG(RegLoadKeyW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3))));
}

HB_FUNC( WAREGLOADKEY )
{
  void * str2;
  void * str3;
  winapi_ret_LONG(RegLoadKey(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINADVAPI LONG WINAPI RegNotifyChangeKeyValue(HKEY hKey,WINBOOL bWatchSubtree,DWORD dwNotifyFilter,HANDLE hEvent,WINBOOL fAsynchronous)
*/
HB_FUNC( WAREGNOTIFYCHANGEKEYVALUE )
{
  winapi_ret_LONG(RegNotifyChangeKeyValue(winapi_par_HKEY(1), winapi_par_BOOL(2), winapi_par_DWORD(3), winapi_par_HANDLE(4), winapi_par_BOOL(5)));
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
HB_FUNC( WAREGREPLACEKEYA )
{
  winapi_ret_LONG(RegReplaceKeyA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3), winapi_par_LPCSTR(4)));
}

/*
WINADVAPI LONG WINAPI RegReplaceKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpNewFile,LPCWSTR lpOldFile)
*/
HB_FUNC( WAREGREPLACEKEYW )
{
  winapi_ret_LONG(RegReplaceKeyW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3)), reinterpret_cast<LPCWSTR>(hb_parc(4))));
}

HB_FUNC( WAREGREPLACEKEY )
{
  void * str2;
  void * str3;
  void * str4;
  winapi_ret_LONG(RegReplaceKey(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
WINADVAPI LONG WINAPI RegRestoreKeyA(HKEY hKey,LPCSTR lpFile,DWORD dwFlags)
*/
HB_FUNC( WAREGRESTOREKEYA )
{
  winapi_ret_LONG(RegRestoreKeyA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_DWORD(3)));
}

/*
WINADVAPI LONG WINAPI RegRestoreKeyW(HKEY hKey,LPCWSTR lpFile,DWORD dwFlags)
*/
HB_FUNC( WAREGRESTOREKEYW )
{
  winapi_ret_LONG(RegRestoreKeyW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), winapi_par_DWORD(3)));
}

HB_FUNC( WAREGRESTOREKEY )
{
  void * str2;
  winapi_ret_LONG(RegRestoreKey(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), winapi_par_DWORD(3)));
  hb_strfree(str2);
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
HB_FUNC( WAREGSETVALUEA )
{
  winapi_ret_LONG(RegSetValueA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_DWORD(3), winapi_par_LPCSTR(4), winapi_par_DWORD(5)));
}

/*
WINADVAPI LONG WINAPI RegSetValueW(HKEY hKey,LPCWSTR lpSubKey,DWORD dwType,LPCWSTR lpData,DWORD cbData)
*/
HB_FUNC( WAREGSETVALUEW )
{
  winapi_ret_LONG(RegSetValueW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), winapi_par_DWORD(3), reinterpret_cast<LPCWSTR>(hb_parc(4)), winapi_par_DWORD(5)));
}

HB_FUNC( WAREGSETVALUE )
{
  void * str2;
  void * str4;
  winapi_ret_LONG(RegSetValue(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), winapi_par_DWORD(3), HB_PARSTR(4, &str4, nullptr), winapi_par_DWORD(5)));
  hb_strfree(str2);
  hb_strfree(str4);
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
HB_FUNC( WAREGUNLOADKEYA )
{
  winapi_ret_LONG(RegUnLoadKeyA(winapi_par_HKEY(1), winapi_par_LPCSTR(2)));
}

/*
WINADVAPI LONG WINAPI RegUnLoadKeyW(HKEY hKey,LPCWSTR lpSubKey)
*/
HB_FUNC( WAREGUNLOADKEYW )
{
  winapi_ret_LONG(RegUnLoadKeyW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2))));
}

HB_FUNC( WAREGUNLOADKEY )
{
  void * str2;
  winapi_ret_LONG(RegUnLoadKey(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINADVAPI LONG WINAPI RegGetValueA(HKEY hkey,LPCSTR lpSubKey,LPCSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
*/
HB_FUNC( WAREGGETVALUEA )
{
  DWORD Type;
  DWORD Data;
  winapi_ret_LONG(RegGetValueA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3), winapi_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  winapi_stor_DWORD(Type, 5);
  winapi_stor_DWORD(Data, 7);
}

/*
WINADVAPI LONG WINAPI RegGetValueW(HKEY hkey,LPCWSTR lpSubKey,LPCWSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
*/
HB_FUNC( WAREGGETVALUEW )
{
  DWORD Type;
  DWORD Data;
  winapi_ret_LONG(RegGetValueW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3)), winapi_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  winapi_stor_DWORD(Type, 5);
  winapi_stor_DWORD(Data, 7);
}

HB_FUNC( WAREGGETVALUE )
{
  void * str2;
  void * str3;
  DWORD Type;
  DWORD Data;
  winapi_ret_LONG(RegGetValue(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), winapi_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  winapi_stor_DWORD(Type, 5);
  winapi_stor_DWORD(Data, 7);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
*/
HB_FUNC( WAINITIATESYSTEMSHUTDOWNA )
{
  winapi_ret_BOOL(InitiateSystemShutdownA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), winapi_par_DWORD(3), hb_parl(4), hb_parl(5)));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
*/
HB_FUNC( WAINITIATESYSTEMSHUTDOWNW )
{
  winapi_ret_BOOL(InitiateSystemShutdownW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), winapi_par_DWORD(3), hb_parl(4), hb_parl(5)));
}

/*
WINADVAPI WINBOOL WINAPI AbortSystemShutdownA(LPSTR lpMachineName)
*/
HB_FUNC( WAABORTSYSTEMSHUTDOWNA )
{
  winapi_ret_BOOL(AbortSystemShutdownA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINADVAPI WINBOOL WINAPI AbortSystemShutdownW(LPWSTR lpMachineName)
*/
HB_FUNC( WAABORTSYSTEMSHUTDOWNW )
{
  winapi_ret_BOOL(AbortSystemShutdownW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1)))));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
*/
HB_FUNC( WAINITIATESYSTEMSHUTDOWNEXA )
{
  winapi_ret_BOOL(InitiateSystemShutdownExA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), winapi_par_DWORD(3), hb_parl(4), hb_parl(5), winapi_par_DWORD(6)));
}

/*
WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
*/
HB_FUNC( WAINITIATESYSTEMSHUTDOWNEXW )
{
  winapi_ret_BOOL(InitiateSystemShutdownExW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), winapi_par_DWORD(3), hb_parl(4), hb_parl(5), winapi_par_DWORD(6)));
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
#if 0
HB_FUNC( WAREGDELETEKEYVALUEA )
{
  winapi_ret_LONG(RegDeleteKeyValueA(winapi_par_HKEY(1), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3)));
}
#endif

/*
WINADVAPI LONG WINAPI RegDeleteKeyValueW(HKEY hKey, LPCWSTR lpSubKey, LPCWSTR lpValueName)
*/
#if 0
HB_FUNC( WAREGDELETEKEYVALUEW )
{
  winapi_ret_LONG(RegDeleteKeyValueW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3))));
}
#endif

#if 0
HB_FUNC( WAREGDELETEKEYVALUE )
{
  void * str2;
  void * str3;
  winapi_ret_LONG(RegDeleteKeyValue(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
}
#endif

/*
WINADVAPI LONG WINAPI RegDeleteTreeA(HKEY hKey, LPCSTR lpSubKey)
*/
#if 0
HB_FUNC( WAREGDELETETREEA )
{
  winapi_ret_LONG(RegDeleteTreeA(winapi_par_HKEY(1), winapi_par_LPCSTR(2)));
}
#endif

/*
WINADVAPI LONG WINAPI RegDeleteTreeW(HKEY hKey, LPCWSTR lpSubKey)
*/
#if 0
HB_FUNC( WAREGDELETETREEW )
{
  winapi_ret_LONG(RegDeleteTreeW(winapi_par_HKEY(1), reinterpret_cast<LPCWSTR>(hb_parc(2))));
}
#endif

#if 0
HB_FUNC( WAREGDELETETREE )
{
  void * str2;
  winapi_ret_LONG(RegDeleteTree(winapi_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}
#endif

/*
WINADVAPI LONG WINAPI RegDisablePredefinedCacheEx(void)
*/
#if 0
HB_FUNC( WAREGDISABLEPREDEFINEDCACHEEX )
{
  winapi_ret_LONG(RegDisablePredefinedCacheEx());
}
#endif

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
