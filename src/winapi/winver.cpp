/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

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

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
DWORD WINAPI VerFindFileA(DWORD uFlags,LPSTR szFileName,LPSTR szWinDir,LPSTR szAppDir,LPSTR szCurDir,PUINT lpuCurDirLen,LPSTR szDestDir,PUINT lpuDestDirLen)
*/

/*
DWORD WINAPI VerFindFileW(DWORD uFlags,LPWSTR szFileName,LPWSTR szWinDir,LPWSTR szAppDir,LPWSTR szCurDir,PUINT lpuCurDirLen,LPWSTR szDestDir,PUINT lpuDestDirLen)
*/

/*
DWORD WINAPI VerInstallFileA(DWORD uFlags,LPSTR szSrcFileName,LPSTR szDestFileName,LPSTR szSrcDir,LPSTR szDestDir,LPSTR szCurDir,LPSTR szTmpFile,PUINT lpuTmpFileLen)
*/

/*
DWORD WINAPI VerInstallFileW(DWORD uFlags,LPWSTR szSrcFileName,LPWSTR szDestFileName,LPWSTR szSrcDir,LPWSTR szDestDir,LPWSTR szCurDir,LPWSTR szTmpFile,PUINT lpuTmpFileLen)
*/

/*
DWORD WINAPI GetFileVersionInfoSizeA(LPCSTR lptstrFilename,LPDWORD lpdwHandle)
DWORD GetFileVersionInfoSizeA([in] LPCSTR lptstrFilename, [out, optional] LPDWORD lpdwHandle)
*/
HB_FUNC( WAGETFILEVERSIONINFOSIZEA )
{
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSizeA(wa_par_LPCSTR(1), &dwHandle));
  wa_stor_DWORD(dwHandle, 2);
}

/*
DWORD WINAPI GetFileVersionInfoSizeW(LPCWSTR lptstrFilename,LPDWORD lpdwHandle)
DWORD GetFileVersionInfoSizeW([in] LPCWSTR lptstrFilename, [out, optional] LPDWORD lpdwHandle)
*/
HB_FUNC( WAGETFILEVERSIONINFOSIZEW )
{
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSizeW(wa_par_LPCWSTR(1), &dwHandle));
  wa_stor_DWORD(dwHandle, 2);
}

HB_FUNC( WAGETFILEVERSIONINFOSIZE )
{
  void * str1{};
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSize(HB_PARSTR(1, &str1, nullptr), &dwHandle));
  wa_stor_DWORD(dwHandle, 2);
  hb_strfree(str1);
}

/*
WINBOOL WINAPI GetFileVersionInfoA(LPCSTR lptstrFilename,DWORD dwHandle,DWORD dwLen,LPVOID lpData)
BOOL GetFileVersionInfoA([in] LPCSTR lptstrFilename, DWORD dwHandle, [in] DWORD dwLen, [out] LPVOID lpData)
*/
HB_FUNC( WAGETFILEVERSIONINFOA )
{
  wa_ret_BOOL(GetFileVersionInfoA(wa_par_LPCSTR(1), 0, wa_par_DWORD(3), static_cast<LPVOID>(hb_parptr(4)))); // TODO: buffer for data
}

/*
WINBOOL WINAPI GetFileVersionInfoW(LPCWSTR lptstrFilename,DWORD dwHandle,DWORD dwLen,LPVOID lpData)
BOOL GetFileVersionInfoW([in] LPCWSTR lptstrFilename, DWORD dwHandle, [in] DWORD dwLen, [out] LPVOID lpData)
*/
HB_FUNC( WAGETFILEVERSIONINFOW )
{
  wa_ret_BOOL(GetFileVersionInfoW(wa_par_LPCWSTR(1), 0, wa_par_DWORD(3), static_cast<LPVOID>(hb_parptr(4)))); // TODO: buffer for data
}

HB_FUNC( WAGETFILEVERSIONINFO )
{
  void * str1{};
  wa_ret_BOOL(GetFileVersionInfo(HB_PARSTR(1, &str1, nullptr), 0, wa_par_DWORD(3), static_cast<LPVOID>(hb_parptr(4)))); // TODO: buffer for data
  hb_strfree(str1);
}

/*
DWORD WINAPI VerLanguageNameA(DWORD wLang,LPSTR szLang,DWORD nSize)
*/

/*
DWORD WINAPI VerLanguageNameW(DWORD wLang,LPWSTR szLang,DWORD nSize)
*/

/*
WINBOOL WINAPI VerQueryValueA(LPCVOID pBlock,LPCSTR lpSubBlock,LPVOID *lplpBuffer,PUINT puLen)
*/

/*
WINBOOL WINAPI VerQueryValueW(LPCVOID pBlock,LPCWSTR lpSubBlock,LPVOID *lplpBuffer,PUINT puLen)
*/

/*
BOOL GetFileVersionInfoExA([in] DWORD dwFlags, [in] LPCSTR lpwstrFilename, DWORD dwHandle, [in] DWORD dwLen, [out] LPVOID lpData)
*/
#if 0
HB_FUNC( WAGETFILEVERSIONINFOEXA )
{
  wa_ret_BOOL(GetFileVersionInfoExA(wa_par_DWORD(1), wa_par_LPCSTR(2), 0, wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)))); // TODO: buffer for data
}
#endif

/*
BOOL GetFileVersionInfoExW([in] DWORD dwFlags, [in] LPCWSTR lpwstrFilename, DWORD dwHandle, [in] DWORD dwLen, [out] LPVOID lpData)
*/
#if 0
HB_FUNC( WAGETFILEVERSIONINFOEXW )
{
  wa_ret_BOOL(GetFileVersionInfoExW(wa_par_DWORD(1), wa_par_LPCWSTR(2), 0, wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)))); // TODO: buffer for data
}
#endif

#if 0
HB_FUNC( WAGETFILEVERSIONINFOEX )
{
  void * str2{};
  wa_ret_BOOL(GetFileVersionInfoEx(wa_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), 0, wa_par_DWORD(4), static_cast<LPVOID>(hb_parptr(5)))); // TODO: buffer for data
  hb_strfree(str2);
}
#endif

/*
DWORD GetFileVersionInfoSizeExA([in] DWORD dwFlags, [in] LPCSTR lpwstrFilename, [out] LPDWORD lpdwHandle)
*/
#if 0
HB_FUNC( WAGETFILEVERSIONINFOSIZEEXA )
{
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSizeExA(wa_par_DWORD(1), wa_par_LPCSTR(2), &dwHandle));
  wa_stor_DWORD(dwHandle, 3);
}
#endif

/*
DWORD GetFileVersionInfoSizeExW([in] DWORD dwFlags, [in] LPCWSTR lpwstrFilename, [out] LPDWORD lpdwHandle)
*/
#if 0
HB_FUNC( WAGETFILEVERSIONINFOSIZEEXW )
{
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSizeExW(wa_par_DWORD(1), wa_par_LPCWSTR(2), &dwHandle));
  wa_stor_DWORD(dwHandle, 3);
}
#endif

#if 0
HB_FUNC( WAGETFILEVERSIONINFOSIZEEX )
{
  void * str2{};
  DWORD dwHandle{};
  wa_ret_DWORD(GetFileVersionInfoSizeEx(wa_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), &dwHandle));
  wa_stor_DWORD(dwHandle, 3);
  hb_strfree(str2);
}
#endif
