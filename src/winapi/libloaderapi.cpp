//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
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

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// WINBASEAPI HRSRC WINAPI FindResourceExW (HMODULE hModule, LPCWSTR lpType, LPCWSTR lpName, WORD wLanguage)

// WINBASEAPI DECLSPEC_NORETURN VOID WINAPI FreeLibraryAndExitThread (HMODULE hLibModule, DWORD dwExitCode)

// WINBASEAPI WINBOOL WINAPI FreeResource (HGLOBAL hResData)
HB_FUNC(WAFREERESOURCE)
{
  wa_ret_BOOL(FreeResource(wa_par_HGLOBAL(1)));
}

// WINBASEAPI HMODULE WINAPI GetModuleHandleA (LPCSTR lpModuleName)
#if 0
HB_FUNC(WAGETMODULEHANDLEA)
{
  wa_ret_HMODULE(GetModuleHandleA(wa_par_LPCSTR(1)));
}
#endif

// WINBASEAPI HMODULE WINAPI GetModuleHandleW (LPCWSTR lpModuleName)
#if 0
HB_FUNC(WAGETMODULEHANDLEW)
{
  wa_ret_HMODULE(GetModuleHandleW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC(WAGETMODULEHANDLE)
{
  void *str{};
  wa_ret_HMODULE(GetModuleHandle(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}

// WINBASEAPI HMODULE WINAPI LoadLibraryExA (LPCSTR lpLibFileName, HANDLE hFile, DWORD dwFlags)
#if 0
HB_FUNC(WALOADLIBRARYEXA)
{
  wa_ret_HMODULE(LoadLibraryExA(wa_par_LPCSTR(1), wa_par_HANDLE(2), wa_par_DWORD(3)));
}
#endif

// WINBASEAPI HMODULE WINAPI LoadLibraryExW (LPCWSTR lpLibFileName, HANDLE hFile, DWORD dwFlags)
#if 0
HB_FUNC(WALOADLIBRARYEXW)
{
  wa_ret_HMODULE(LoadLibraryExW(wa_par_LPCWSTR(1), wa_par_HANDLE(2), wa_par_DWORD(3)));
}
#endif

HB_FUNC(WALOADLIBRARYEX)
{
  void *str{};
  wa_ret_HMODULE(LoadLibraryEx(HB_PARSTR(1, &str, nullptr), wa_par_HANDLE(2), wa_par_DWORD(3)));
  hb_strfree(str);
}

// WINBASEAPI HGLOBAL WINAPI LoadResource (HMODULE hModule, HRSRC hResInfo)
HB_FUNC(WALOADRESOURCE)
{
  wa_ret_HGLOBAL(LoadResource(wa_par_HMODULE(1), wa_par_HRSRC(2)));
}

// WINUSERAPI int WINAPI LoadStringA (HINSTANCE hInstance, UINT uID, LPSTR lpBuffer, int cchBufferMax)

// WINUSERAPI int WINAPI LoadStringW (HINSTANCE hInstance, UINT uID, LPWSTR lpBuffer, int cchBufferMax)

// WINBASEAPI LPVOID WINAPI LockResource (HGLOBAL hResData)
HB_FUNC(WALOCKRESOURCE)
{
  wa_ret_LPVOID(LockResource(wa_par_HGLOBAL(1)));
}

// WINBASEAPI DWORD WINAPI SizeofResource (HMODULE hModule, HRSRC hResInfo)
HB_FUNC(WASIZEOFRESOURCE)
{
  wa_ret_DWORD(SizeofResource(wa_par_HMODULE(1), wa_par_HRSRC(2)));
}

// WINBASEAPI DLL_DIRECTORY_COOKIE WINAPI AddDllDirectory (PCWSTR NewDirectory)

// WINBASEAPI WINBOOL WINAPI RemoveDllDirectory (DLL_DIRECTORY_COOKIE Cookie)

// WINBASEAPI WINBOOL WINAPI SetDefaultDllDirectories (DWORD DirectoryFlags)
#if 0
HB_FUNC(WASETDEFAULTDLLDIRECTORIES)
{
  wa_ret_BOOL(SetDefaultDllDirectories(wa_par_DWORD(1)));
}
#endif

// WINBASEAPI WINBOOL WINAPI GetModuleHandleExA (DWORD dwFlags, LPCSTR lpModuleName, HMODULE *phModule)
#if 0
HB_FUNC(WAGETMODULEHANDLEEXA)
{
  HMODULE hModule{};
  wa_ret_BOOL(GetModuleHandleExA(wa_par_DWORD(1), wa_par_LPCSTR(2), &hModule));
  hb_storptr(hModule, 3);
}
#endif

// WINBASEAPI WINBOOL WINAPI GetModuleHandleExW (DWORD dwFlags, LPCWSTR lpModuleName, HMODULE *phModule)
#if 0
HB_FUNC(WAGETMODULEHANDLEEXW)
{
  HMODULE hModule{};
  wa_ret_BOOL(GetModuleHandleExW(wa_par_DWORD(1), wa_par_LPCWSTR(2), &hModule));
  hb_storptr(hModule, 3);
}
#endif

HB_FUNC(WAGETMODULEHANDLEEX)
{
  void *str{};
  HMODULE hModule{};
  wa_ret_BOOL(GetModuleHandleEx(wa_par_DWORD(1), HB_PARSTR(2, &str, nullptr), &hModule));
  hb_storptr(hModule, 3);
  hb_strfree(str);
}

// WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesA(HMODULE hModule,LPCSTR lpType,LPCSTR lpName,ENUMRESLANGPROCA lpEnumFunc,LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumResourceLanguagesW(HMODULE hModule,LPCWSTR lpType,LPCWSTR lpName,ENUMRESLANGPROCW lpEnumFunc,LONG_PTR lParam)

// WINBASEAPI WINBOOL APIENTRY EnumResourceLanguagesExA (HMODULE hModule, LPCSTR lpType, LPCSTR lpName, ENUMRESLANGPROCA lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL APIENTRY EnumResourceLanguagesExW (HMODULE hModule, LPCWSTR lpType, LPCWSTR lpName, ENUMRESLANGPROCW lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI EnumResourceNamesExA (HMODULE hModule, LPCSTR lpType, ENUMRESNAMEPROCA lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI EnumResourceNamesExW (HMODULE hModule, LPCWSTR lpType, ENUMRESNAMEPROCW lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI EnumResourceTypesExA (HMODULE hModule, ENUMRESTYPEPROCA lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI EnumResourceTypesExW (HMODULE hModule, ENUMRESTYPEPROCW lpEnumFunc, LONG_PTR lParam, DWORD dwFlags, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI QueryOptionalDelayLoadedAPI (HMODULE CallerModule, LPCSTR lpDllName, LPCSTR lpProcName, DWORD Reserved)

// WINBASEAPI WINBOOL WINAPI DisableThreadLibraryCalls (HMODULE hLibModule)
HB_FUNC(WADISABLETHREADLIBRARYCALLS)
{
  wa_ret_BOOL(DisableThreadLibraryCalls(wa_par_HMODULE(1)));
}

// WINBASEAPI WINBOOL WINAPI FreeLibrary (HMODULE hLibModule)
HB_FUNC(WAFREELIBRARY)
{
  wa_ret_BOOL(FreeLibrary(wa_par_HMODULE(1)));
}

// WINBASEAPI FARPROC WINAPI GetProcAddress (HMODULE hModule, LPCSTR lpProcName)
HB_FUNC(WAGETPROCADDRESS)
{
  wa_ret_FARPROC(GetProcAddress(wa_par_HMODULE(1), wa_par_LPCSTR(2)));
}

// WINBASEAPI DWORD WINAPI GetModuleFileNameA (HMODULE hModule, LPSTR lpFilename, DWORD nSize)

// WINBASEAPI DWORD WINAPI GetModuleFileNameW (HMODULE hModule, LPWSTR lpFilename, DWORD nSize)

// WINBASEAPI int WINAPI FindStringOrdinal (DWORD dwFindStringOrdinalFlags, LPCWSTR lpStringSource, int cchSource, LPCWSTR lpStringValue, int cchValue, WINBOOL bIgnoreCase)
#if 0 // TODO: Windows 7 or upper
HB_FUNC(WAFINDSTRINGORDINAL)
{
  void *str1{};
  void *str2{};
  wa_ret_int(FindStringOrdinal(wa_par_DWORD(1), HB_PARSTR(2, &str1, nullptr), wa_par_int(3), HB_PARSTR(4, &str2, nullptr), wa_par_int(5), wa_par_BOOL(6)));
  hb_strfree(str1);
  hb_strfree(str2);
}
#endif
