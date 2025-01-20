//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#define _WIN32_WINNT 0x0600

#include <windows.h>
#include <winreg.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

#define wa_par_REGSAM(n) wa_par_DWORD(n)

// WINADVAPI LONG WINAPI RegCloseKey(HKEY hKey)
HB_FUNC(WAREGCLOSEKEY)
{
  wa_ret_LONG(RegCloseKey(wa_par_HKEY(1)));
}

// WINADVAPI LONG WINAPI RegOverridePredefKey(HKEY hKey,HKEY hNewHKey)
HB_FUNC(WAREGOVERRIDEPREDEFKEY)
{
  wa_ret_LONG(RegOverridePredefKey(wa_par_HKEY(1), wa_par_HKEY(2)));
}

// WINADVAPI LONG WINAPI RegOpenUserClassesRoot(HANDLE hToken,DWORD dwOptions,REGSAM samDesired,PHKEY phkResult)
HB_FUNC(WAREGOPENUSERCLASSESROOT)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenUserClassesRoot(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_REGSAM(3), &hkResult));
  hb_storptr(hkResult, 4);
}

// WINADVAPI LONG WINAPI RegOpenCurrentUser(REGSAM samDesired,PHKEY phkResult)
HB_FUNC(WAREGOPENCURRENTUSER)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenCurrentUser(wa_par_REGSAM(1), &hkResult));
  hb_storptr(hkResult, 2);
}

// WINADVAPI LONG WINAPI RegDisablePredefinedCache(void)
HB_FUNC(WAREGDISABLEPREDEFINEDCACHE)
{
  wa_ret_LONG(RegDisablePredefinedCache());
}

// WINADVAPI LONG WINAPI RegConnectRegistryA(LPCSTR lpMachineName,HKEY hKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCONNECTREGISTRYA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistryA(wa_par_LPCSTR(1), wa_par_HKEY(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

// WINADVAPI LONG WINAPI RegConnectRegistryW(LPCWSTR lpMachineName,HKEY hKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCONNECTREGISTRYW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistryW(wa_par_LPCWSTR(1), wa_par_HKEY(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

HB_FUNC(WAREGCONNECTREGISTRY)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistry(HB_PARSTR(1, &str, nullptr), wa_par_HKEY(2), &hkResult));
  hb_storptr(hkResult, 3);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegConnectRegistryExA(LPCSTR lpMachineName,HKEY hKey,ULONG Flags,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCONNECTREGISTRYEXA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistryExA(wa_par_LPCSTR(1), wa_par_HKEY(2), wa_par_ULONG(3), &hkResult));
  hb_storptr(hkResult, 4);
}
#endif

// WINADVAPI LONG WINAPI RegConnectRegistryExW(LPCWSTR lpMachineName,HKEY hKey,ULONG Flags,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCONNECTREGISTRYEXW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistryExW(wa_par_LPCWSTR(1), wa_par_HKEY(2), wa_par_ULONG(3), &hkResult));
  hb_storptr(hkResult, 4);
}
#endif

HB_FUNC(WAREGCONNECTREGISTRYEX)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegConnectRegistryEx(HB_PARSTR(1, &str, nullptr), wa_par_HKEY(2), wa_par_ULONG(3), &hkResult));
  hb_storptr(hkResult, 4);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegCreateKeyA(HKEY hKey,LPCSTR lpSubKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCREATEKEYA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegCreateKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

// WINADVAPI LONG WINAPI RegCreateKeyW(HKEY hKey,LPCWSTR lpSubKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGCREATEKEYW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegCreateKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

HB_FUNC(WAREGCREATEKEY)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegCreateKey(wa_par_HKEY(1), HB_PARSTR(2, &str, nullptr), &hkResult));
  hb_storptr(hkResult, 3);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegCreateKeyExA(HKEY hKey,LPCSTR lpSubKey,DWORD Reserved,LPSTR lpClass,DWORD dwOptions,REGSAM samDesired,LPSECURITY_ATTRIBUTES lpSecurityAttributes,PHKEY phkResult,LPDWORD lpdwDisposition)

// WINADVAPI LONG WINAPI RegCreateKeyExW(HKEY hKey,LPCWSTR lpSubKey,DWORD Reserved,LPWSTR lpClass,DWORD dwOptions,REGSAM samDesired,LPSECURITY_ATTRIBUTES lpSecurityAttributes,PHKEY phkResult,LPDWORD lpdwDisposition)

// WINADVAPI LONG WINAPI RegDeleteKeyA(HKEY hKey,LPCSTR lpSubKey)
#if 0
HB_FUNC(WAREGDELETEKEYA)
{
  wa_ret_LONG(RegDeleteKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2)));
}
#endif

// WINADVAPI LONG WINAPI RegDeleteKeyW(HKEY hKey,LPCWSTR lpSubKey)
#if 0
HB_FUNC(WAREGDELETEKEYW)
{
  wa_ret_LONG(RegDeleteKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC(WAREGDELETEKEY)
{
  void *str2{};
  wa_ret_LONG(RegDeleteKey(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

// WINADVAPI LONG WINAPI RegDeleteKeyExA(HKEY hKey,LPCSTR lpSubKey,REGSAM samDesired,DWORD Reserved)
#if 0
HB_FUNC(WAREGDELETEKEYEXA)
{
  wa_ret_LONG(RegDeleteKeyExA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_REGSAM(3), wa_par_DWORD(4)));
}
#endif

// WINADVAPI LONG WINAPI RegDeleteKeyExW(HKEY hKey,LPCWSTR lpSubKey,REGSAM samDesired,DWORD Reserved)
#if 0
HB_FUNC(WAREGDELETEKEYEXW)
{
  wa_ret_LONG(RegDeleteKeyExW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_REGSAM(3), wa_par_DWORD(4)));
}
#endif

HB_FUNC(WAREGDELETEKEYEX)
{
  void *str{};
  wa_ret_LONG(RegDeleteKeyEx(wa_par_HKEY(1), HB_PARSTR(2, &str, nullptr), wa_par_REGSAM(3), wa_par_DWORD(4)));
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegDisableReflectionKey(HKEY hBase)
HB_FUNC(WAREGDISABLEREFLECTIONKEY)
{
  wa_ret_LONG(RegDisableReflectionKey(wa_par_HKEY(1)));
}

// WINADVAPI LONG WINAPI RegEnableReflectionKey(HKEY hBase)
HB_FUNC(WAREGENABLEREFLECTIONKEY)
{
  wa_ret_LONG(RegEnableReflectionKey(wa_par_HKEY(1)));
}

// WINADVAPI LONG WINAPI RegQueryReflectionKey(HKEY hBase,WINBOOL *bIsReflectionDisabled)
HB_FUNC(WAREGQUERYREFLECTIONKEY)
{
  BOOL bIsReflectionDisabled{};
  wa_ret_LONG(RegQueryReflectionKey(wa_par_HKEY(1), &bIsReflectionDisabled));
  wa_stor_BOOL(bIsReflectionDisabled, 2);
}

// WINADVAPI LONG WINAPI RegDeleteValueA(HKEY hKey,LPCSTR lpValueName)
#if 0
HB_FUNC(WAREGDELETEVALUEA)
{
  wa_ret_LONG(RegDeleteValueA(wa_par_HKEY(1), wa_par_LPCSTR(2)));
}
#endif

// WINADVAPI LONG WINAPI RegDeleteValueW(HKEY hKey,LPCWSTR lpValueName)
#if 0
HB_FUNC(WAREGDELETEVALUEW)
{
  wa_ret_LONG(RegDeleteValueW(wa_par_HKEY(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC(WAREGDELETEVALUE)
{
  void *str2{};
  wa_ret_LONG(RegDeleteValue(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

// WINADVAPI LONG WINAPI RegEnumKeyA(HKEY hKey,DWORD dwIndex,LPSTR lpName,DWORD cchName)
#if 0
HB_FUNC(WAREGENUMKEYA) // TODO: fix
{
  wa_ret_LONG(RegEnumKeyA(wa_par_HKEY(1), wa_par_DWORD(2), const_cast<LPSTR>(hb_parc(3)), wa_par_DWORD(4)));
}
#endif

// WINADVAPI LONG WINAPI RegEnumKeyW(HKEY hKey,DWORD dwIndex,LPWSTR lpName,DWORD cchName)
#if 0
HB_FUNC(WAREGENUMKEYW) // TODO: fix
{
  wa_ret_LONG(RegEnumKeyW(wa_par_HKEY(1), wa_par_DWORD(2), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(3))), wa_par_DWORD(4)));
}
#endif

HB_FUNC(WAREGENUMKEY) // TODO: fix
{
  wa_ret_LONG(RegEnumKey(wa_par_HKEY(1), wa_par_DWORD(2), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(3))), wa_par_DWORD(4)));
}

// WINADVAPI LONG WINAPI RegEnumKeyExA(HKEY hKey,DWORD dwIndex,LPSTR lpName,LPDWORD lpcchName,LPDWORD lpReserved,LPSTR lpClass,LPDWORD lpcchClass,PFILETIME lpftLastWriteTime)

// WINADVAPI LONG WINAPI RegEnumKeyExW(HKEY hKey,DWORD dwIndex,LPWSTR lpName,LPDWORD lpcchName,LPDWORD lpReserved,LPWSTR lpClass,LPDWORD lpcchClass,PFILETIME lpftLastWriteTime)

// WINADVAPI LONG WINAPI RegEnumValueA(HKEY hKey,DWORD dwIndex,LPSTR lpValueName,LPDWORD lpcchValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)

// WINADVAPI LONG WINAPI RegEnumValueW(HKEY hKey,DWORD dwIndex,LPWSTR lpValueName,LPDWORD lpcchValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)

// WINADVAPI LONG WINAPI RegFlushKey(HKEY hKey)
HB_FUNC(WAREGFLUSHKEY)
{
  wa_ret_LONG(RegFlushKey(wa_par_HKEY(1)));
}

// WINADVAPI LONG WINAPI RegGetKeySecurity(HKEY hKey,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor,LPDWORD lpcbSecurityDescriptor)

// WINADVAPI LONG WINAPI RegLoadKeyA(HKEY hKey,LPCSTR lpSubKey,LPCSTR lpFile)
#if 0
HB_FUNC(WAREGLOADKEYA)
{
  wa_ret_LONG(RegLoadKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

// WINADVAPI LONG WINAPI RegLoadKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpFile)
#if 0
HB_FUNC(WAREGLOADKEYW)
{
  wa_ret_LONG(RegLoadKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC(WAREGLOADKEY)
{
  void *str2{};
  void *str3{};
  wa_ret_LONG(RegLoadKey(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
}

// WINADVAPI LONG WINAPI RegNotifyChangeKeyValue(HKEY hKey,WINBOOL bWatchSubtree,DWORD dwNotifyFilter,HANDLE hEvent,WINBOOL fAsynchronous)
HB_FUNC(WAREGNOTIFYCHANGEKEYVALUE)
{
  wa_ret_LONG(RegNotifyChangeKeyValue(wa_par_HKEY(1), wa_par_BOOL(2), wa_par_DWORD(3), wa_par_HANDLE(4), wa_par_BOOL(5)));
}

// WINADVAPI LONG WINAPI RegOpenKeyA(HKEY hKey,LPCSTR lpSubKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGOPENKEYA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

// WINADVAPI LONG WINAPI RegOpenKeyW(HKEY hKey,LPCWSTR lpSubKey,PHKEY phkResult)
#if 0
HB_FUNC(WAREGOPENKEYW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2), &hkResult));
  hb_storptr(hkResult, 3);
}
#endif

HB_FUNC(WAREGOPENKEY)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKey(wa_par_HKEY(1), HB_PARSTR(2, &str, nullptr), &hkResult));
  hb_storptr(hkResult, 3);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegOpenKeyExA(HKEY hKey,LPCSTR lpSubKey,DWORD ulOptions,REGSAM samDesired,PHKEY phkResult)
#if 0
HB_FUNC(WAREGOPENKEYEXA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKeyExA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_DWORD(3), wa_par_REGSAM(4), &hkResult));
  hb_storptr(hkResult, 5);
}
#endif

// WINADVAPI LONG WINAPI RegOpenKeyExW(HKEY hKey,LPCWSTR lpSubKey,DWORD ulOptions,REGSAM samDesired,PHKEY phkResult)
#if 0
HB_FUNC(WAREGOPENKEYEXW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKeyExW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_DWORD(3), wa_par_REGSAM(4), &hkResult));
  hb_storptr(hkResult, 5);
}
#endif

HB_FUNC(WAREGOPENKEYEX)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegOpenKeyEx(wa_par_HKEY(1), HB_PARSTR(2, &str, nullptr), wa_par_DWORD(3), wa_par_REGSAM(4), &hkResult));
  hb_storptr(hkResult, 5);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegQueryInfoKeyA(HKEY hKey,LPSTR lpClass,LPDWORD lpcchClass,LPDWORD lpReserved,LPDWORD lpcSubKeys,LPDWORD lpcbMaxSubKeyLen,LPDWORD lpcbMaxClassLen,LPDWORD lpcValues,LPDWORD lpcbMaxValueNameLen,LPDWORD lpcbMaxValueLen,LPDWORD lpcbSecurityDescriptor,PFILETIME lpftLastWriteTime)

// WINADVAPI LONG WINAPI RegQueryInfoKeyW(HKEY hKey,LPWSTR lpClass,LPDWORD lpcchClass,LPDWORD lpReserved,LPDWORD lpcSubKeys,LPDWORD lpcbMaxSubKeyLen,LPDWORD lpcbMaxClassLen,LPDWORD lpcValues,LPDWORD lpcbMaxValueNameLen,LPDWORD lpcbMaxValueLen,LPDWORD lpcbSecurityDescriptor,PFILETIME lpftLastWriteTime)

// WINADVAPI LONG WINAPI RegQueryValueA(HKEY hKey,LPCSTR lpSubKey,LPSTR lpData,PLONG lpcbData)

// WINADVAPI LONG WINAPI RegQueryValueW(HKEY hKey,LPCWSTR lpSubKey,LPWSTR lpData,PLONG lpcbData)

// WINADVAPI LONG WINAPI RegQueryMultipleValuesA(HKEY hKey,PVALENTA val_list,DWORD num_vals,LPSTR lpValueBuf,LPDWORD ldwTotsize)

// WINADVAPI LONG WINAPI RegQueryMultipleValuesW(HKEY hKey,PVALENTW val_list,DWORD num_vals,LPWSTR lpValueBuf,LPDWORD ldwTotsize)

// WINADVAPI LONG WINAPI RegQueryValueExA(HKEY hKey,LPCSTR lpValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)

// WINADVAPI LONG WINAPI RegQueryValueExW(HKEY hKey,LPCWSTR lpValueName,LPDWORD lpReserved,LPDWORD lpType,LPBYTE lpData,LPDWORD lpcbData)

// WINADVAPI LONG WINAPI RegReplaceKeyA(HKEY hKey,LPCSTR lpSubKey,LPCSTR lpNewFile,LPCSTR lpOldFile)
#if 0
HB_FUNC(WAREGREPLACEKEYA)
{
  wa_ret_LONG(RegReplaceKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_LPCSTR(4)));
}
#endif

// WINADVAPI LONG WINAPI RegReplaceKeyW(HKEY hKey,LPCWSTR lpSubKey,LPCWSTR lpNewFile,LPCWSTR lpOldFile)
#if 0
HB_FUNC(WAREGREPLACEKEYW)
{
  wa_ret_LONG(RegReplaceKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC(WAREGREPLACEKEY)
{
  void *str2{};
  void *str3{};
  void *str4{};
  wa_ret_LONG(RegReplaceKey(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
  hb_strfree(str4);
}

// WINADVAPI LONG WINAPI RegRestoreKeyA(HKEY hKey,LPCSTR lpFile,DWORD dwFlags)
#if 0
HB_FUNC(WAREGRESTOREKEYA)
{
  wa_ret_LONG(RegRestoreKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_DWORD(3)));
}
#endif

// WINADVAPI LONG WINAPI RegRestoreKeyW(HKEY hKey,LPCWSTR lpFile,DWORD dwFlags)
#if 0
HB_FUNC(WAREGRESTOREKEYW)
{
  wa_ret_LONG(RegRestoreKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_DWORD(3)));
}
#endif

HB_FUNC(WAREGRESTOREKEY)
{
  void *str2{};
  wa_ret_LONG(RegRestoreKey(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3)));
  hb_strfree(str2);
}

// WINADVAPI LONG WINAPI RegSaveKeyA(HKEY hKey,LPCSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes)

// WINADVAPI LONG WINAPI RegSaveKeyW(HKEY hKey,LPCWSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes)

// WINADVAPI LONG WINAPI RegSetKeySecurity(HKEY hKey,SECURITY_INFORMATION SecurityInformation,PSECURITY_DESCRIPTOR pSecurityDescriptor)

// WINADVAPI LONG WINAPI RegSetValueA(HKEY hKey,LPCSTR lpSubKey,DWORD dwType,LPCSTR lpData,DWORD cbData)
#if 0
HB_FUNC(WAREGSETVALUEA)
{
  wa_ret_LONG(RegSetValueA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_DWORD(3), wa_par_LPCSTR(4), wa_par_DWORD(5)));
}
#endif

// WINADVAPI LONG WINAPI RegSetValueW(HKEY hKey,LPCWSTR lpSubKey,DWORD dwType,LPCWSTR lpData,DWORD cbData)
#if 0
HB_FUNC(WAREGSETVALUEW)
{
  wa_ret_LONG(RegSetValueW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_DWORD(3), wa_par_LPCWSTR(4), wa_par_DWORD(5)));
}
#endif

HB_FUNC(WAREGSETVALUE)
{
  void *str2{};
  void *str4{};
  wa_ret_LONG(RegSetValue(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3), HB_PARSTR(4, &str4, nullptr), wa_par_DWORD(5)));
  hb_strfree(str2);
  hb_strfree(str4);
}

// WINADVAPI LONG WINAPI RegSetValueExA(HKEY hKey,LPCSTR lpValueName,DWORD Reserved,DWORD dwType,CONST BYTE *lpData,DWORD cbData)

// WINADVAPI LONG WINAPI RegSetValueExW(HKEY hKey,LPCWSTR lpValueName,DWORD Reserved,DWORD dwType,CONST BYTE *lpData,DWORD cbData)

// WINADVAPI LONG WINAPI RegUnLoadKeyA(HKEY hKey,LPCSTR lpSubKey)
#if 0
HB_FUNC(WAREGUNLOADKEYA)
{
  wa_ret_LONG(RegUnLoadKeyA(wa_par_HKEY(1), wa_par_LPCSTR(2)));
}
#endif

// WINADVAPI LONG WINAPI RegUnLoadKeyW(HKEY hKey,LPCWSTR lpSubKey)
#if 0
HB_FUNC(WAREGUNLOADKEYW)
{
  wa_ret_LONG(RegUnLoadKeyW(wa_par_HKEY(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC(WAREGUNLOADKEY)
{
  void *str2{};
  wa_ret_LONG(RegUnLoadKey(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

// WINADVAPI LONG WINAPI RegGetValueA(HKEY hkey,LPCSTR lpSubKey,LPCSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
#if 0
HB_FUNC(WAREGGETVALUEA)
{
  DWORD Type{};
  DWORD Data{};
  wa_ret_LONG(RegGetValueA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  wa_stor_DWORD(Type, 5);
  wa_stor_DWORD(Data, 7);
}
#endif

// WINADVAPI LONG WINAPI RegGetValueW(HKEY hkey,LPCWSTR lpSubKey,LPCWSTR lpValue,DWORD dwFlags,LPDWORD pdwType,PVOID pvData,LPDWORD pcbData)
#if 0
HB_FUNC(WAREGGETVALUEW)
{
  DWORD Type{};
  DWORD Data{};
  wa_ret_LONG(RegGetValueW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  wa_stor_DWORD(Type, 5);
  wa_stor_DWORD(Data, 7);
}
#endif

HB_FUNC(WAREGGETVALUE)
{
  void *str2{};
  void *str3{};
  DWORD Type{};
  DWORD Data{};
  wa_ret_LONG(RegGetValue(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_DWORD(4), &Type, static_cast<PVOID>(hb_parptr(6)), &Data));
  wa_stor_DWORD(Type, 5);
  wa_stor_DWORD(Data, 7);
  hb_strfree(str2);
  hb_strfree(str3);
}

// WINADVAPI WINBOOL WINAPI InitiateSystemShutdownA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
#if 0
HB_FUNC(WAINITIATESYSTEMSHUTDOWNA) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdownA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3), hb_parl(4), hb_parl(5)));
}
#endif

// WINADVAPI WINBOOL WINAPI InitiateSystemShutdownW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown)
#if 0
HB_FUNC(WAINITIATESYSTEMSHUTDOWNW) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdownW(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(2))), wa_par_DWORD(3), hb_parl(4), hb_parl(5)));
}
#endif

HB_FUNC(WAINITIATESYSTEMSHUTDOWN) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdown(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(2))), wa_par_DWORD(3), hb_parl(4), hb_parl(5)));
}

// WINADVAPI WINBOOL WINAPI AbortSystemShutdownA(LPSTR lpMachineName)
#if 0
HB_FUNC(WAABORTSYSTEMSHUTDOWNA) // TODO: fix
{
  wa_ret_BOOL(AbortSystemShutdownA(const_cast<LPSTR>(hb_parc(1))));
}
#endif

// WINADVAPI WINBOOL WINAPI AbortSystemShutdownW(LPWSTR lpMachineName)
#if 0
HB_FUNC(WAABORTSYSTEMSHUTDOWNW) // TODO: fix
{
  wa_ret_BOOL(AbortSystemShutdownW(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1)))));
}
#endif

HB_FUNC(WAABORTSYSTEMSHUTDOWN) // TODO: fix
{
  wa_ret_BOOL(AbortSystemShutdown(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1)))));
}

// WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExA(LPSTR lpMachineName,LPSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
#if 0
HB_FUNC(WAINITIATESYSTEMSHUTDOWNEXA) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdownExA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3), hb_parl(4), hb_parl(5), wa_par_DWORD(6)));
}
#endif

// WINADVAPI WINBOOL WINAPI InitiateSystemShutdownExW(LPWSTR lpMachineName,LPWSTR lpMessage,DWORD dwTimeout,WINBOOL bForceAppsClosed,WINBOOL bRebootAfterShutdown,DWORD dwReason)
#if 0
HB_FUNC(WAINITIATESYSTEMSHUTDOWNEXW) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdownExW(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(2))), wa_par_DWORD(3), hb_parl(4), hb_parl(5), wa_par_DWORD(6)));
}
#endif

HB_FUNC(WAINITIATESYSTEMSHUTDOWNEX) // TODO: fix
{
  wa_ret_BOOL(InitiateSystemShutdownEx(reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char *>(hb_parc(2))), wa_par_DWORD(3), hb_parl(4), hb_parl(5), wa_par_DWORD(6)));
}

// WINADVAPI LONG WINAPI RegSaveKeyExA(HKEY hKey,LPCSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD Flags)

// WINADVAPI LONG WINAPI RegSaveKeyExW(HKEY hKey,LPCWSTR lpFile,LPSECURITY_ATTRIBUTES lpSecurityAttributes,DWORD Flags)

// WINADVAPI LONG WINAPI Wow64Win32ApiEntry(DWORD dwFuncNumber,DWORD dwFlag,DWORD dwRes)
#if 0
HB_FUNC(WAWOW64WIN32APIENTRY)
{
  wa_ret_LONG(Wow64Win32ApiEntry(wa_par_DWORD(1), wa_par_DWORD(2), wa_par_DWORD(3)));
}
#endif

// WINADVAPI LONG WINAPI RegCopyTreeA(HKEY hKeySrc, LPCSTR lpSubKey, HKEY hKeyDest)
#if 0
HB_FUNC(WAREGCOPYTREEA)
{
  wa_ret_LONG(RegCopyTreeA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_HKEY(3)));
}
#endif

// WINADVAPI LONG WINAPI RegCopyTreeW(HKEY hKeySrc, LPCWSTR lpSubKey, HKEY hKeyDest)
#if 0
HB_FUNC(WAREGCOPYTREEW)
{
  wa_ret_LONG(RegCopyTreeW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_HKEY(3)));
}
#endif

HB_FUNC(WAREGCOPYTREE)
{
  void *str{};
  wa_ret_LONG(RegCopyTree(wa_par_HKEY(1), HB_PARSTR(2, &str, nullptr), wa_par_HKEY(3)));
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegCreateKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, DWORD Reserved, LPSTR lpClass, DWORD dwOptions, REGSAM samDesired, const LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition, HANDLE hTransaction, PVOID pExtendedParemeter)

// WINADVAPI LONG WINAPI RegCreateKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, DWORD Reserved, LPWSTR lpClass, DWORD dwOptions, REGSAM samDesired, const LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition, HANDLE hTransaction, PVOID pExtendedParemeter)

// WINADVAPI LONG WINAPI RegDeleteKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, REGSAM samDesired, DWORD Reserved, HANDLE hTransaction, PVOID pExtendedParameter)

// WINADVAPI LONG WINAPI RegDeleteKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, REGSAM samDesired, DWORD Reserved, HANDLE hTransaction, PVOID pExtendedParameter)

// WINADVAPI LONG WINAPI RegDeleteKeyValueA(HKEY hKey, LPCSTR lpSubKey, LPCSTR lpValueName)
#if 0
HB_FUNC(WAREGDELETEKEYVALUEA)
{
  wa_ret_LONG(RegDeleteKeyValueA(wa_par_HKEY(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

// WINADVAPI LONG WINAPI RegDeleteKeyValueW(HKEY hKey, LPCWSTR lpSubKey, LPCWSTR lpValueName)
#if 0
HB_FUNC(WAREGDELETEKEYVALUEW)
{
  wa_ret_LONG(RegDeleteKeyValueW(wa_par_HKEY(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC(WAREGDELETEKEYVALUE)
{
  void *str2{};
  void *str3{};
  wa_ret_LONG(RegDeleteKeyValue(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str2);
  hb_strfree(str3);
}

// WINADVAPI LONG WINAPI RegDeleteTreeA(HKEY hKey, LPCSTR lpSubKey)
#if 0
HB_FUNC(WAREGDELETETREEA)
{
  wa_ret_LONG(RegDeleteTreeA(wa_par_HKEY(1), wa_par_LPCSTR(2)));
}
#endif

// WINADVAPI LONG WINAPI RegDeleteTreeW(HKEY hKey, LPCWSTR lpSubKey)
#if 0
HB_FUNC(WAREGDELETETREEW)
{
  wa_ret_LONG(RegDeleteTreeW(wa_par_HKEY(1), wa_par_LPCWSTR(2)));
}
#endif

#if 0
HB_FUNC(WAREGDELETETREE)
{
  void *str2{};
  wa_ret_LONG(RegDeleteTree(wa_par_HKEY(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}
#endif

// WINADVAPI LONG WINAPI RegDisablePredefinedCacheEx(void)
#if 0
HB_FUNC(WAREGDISABLEPREDEFINEDCACHEEX)
{
  wa_ret_LONG(RegDisablePredefinedCacheEx());
}
#endif

// WINADVAPI LONG WINAPI RegLoadAppKeyA(LPCSTR lpFile, PHKEY phkResult, REGSAM samDesired, DWORD dwOptions, DWORD Reserved)
#if 0
HB_FUNC(WAREGLOADAPPKEYA)
{
  HKEY hkResult{};
  wa_ret_LONG(RegLoadAppKeyA(wa_par_LPCSTR(1), &hkResult, wa_par_REGSAM(3), wa_par_DWORD(4), wa_par_DWORD(5)));
  hb_storptr(hkResult, 2);
}
#endif

// WINADVAPI LONG WINAPI RegLoadAppKeyW(LPCWSTR lpFile, PHKEY phkResult, REGSAM samDesired, DWORD dwOptions, DWORD Reserved)
#if 0
HB_FUNC(WAREGLOADAPPKEYW)
{
  HKEY hkResult{};
  wa_ret_LONG(RegLoadAppKeyW(wa_par_LPCWSTR(1), &hkResult, wa_par_REGSAM(3), wa_par_DWORD(4), wa_par_DWORD(5)));
  hb_storptr(hkResult, 2);
}
#endif

HB_FUNC(WAREGLOADAPPKEY)
{
  void *str{};
  HKEY hkResult{};
  wa_ret_LONG(RegLoadAppKey(HB_PARSTR(1, &str, nullptr), &hkResult, wa_par_REGSAM(3), wa_par_DWORD(4), wa_par_DWORD(5)));
  hb_storptr(hkResult, 2);
  hb_strfree(str);
}

// WINADVAPI LONG WINAPI RegLoadMUIStringA(HKEY hKey, LPCSTR pszValue, LPSTR pszOutBuf, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCSTR pszDirectory)

// WINADVAPI LONG WINAPI RegLoadMUIStringW(HKEY hKey, LPCWSTR pszValue, LPWSTR pszOutBuf, DWORD cbOutBuf, LPDWORD pcbData, DWORD Flags, LPCWSTR pszDirectory)

// WINADVAPI LONG WINAPI RegOpenKeyTransactedA(HKEY hKey, LPCSTR lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult, HANDLE hTransaction, PVOID pExtendedParameter)

// WINADVAPI LONG WINAPI RegOpenKeyTransactedW(HKEY hKey, LPCWSTR lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult, HANDLE hTransaction, PVOID pExtendedParameter)

// WINADVAPI LONG WINAPI RegSetKeyValueA(HKEY hKey, LPCSTR lpSubKey, LPCSTR lpValueName, DWORD dwType, LPCVOID lpData, DWORD cbData)

// WINADVAPI LONG WINAPI RegSetKeyValueW(HKEY hKey, LPCWSTR lpSubKey, LPCWSTR lpValueName, DWORD dwType, LPCVOID lpData, DWORD cbData)

// WINADVAPI DWORD WINAPI InitiateShutdownA(LPSTR lpMachineName, LPSTR lpMessage, DWORD dwGracePeriod, DWORD dwShutdownFlags, DWORD dwReason)

// WINADVAPI DWORD WINAPI InitiateShutdownW(LPWSTR lpMachineName, LPWSTR lpMessage, DWORD dwGracePeriod, DWORD dwShutdownFlags, DWORD dwReason)
