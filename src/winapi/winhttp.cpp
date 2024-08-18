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
#include <winhttp.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

#define wa_par_INTERNET_PORT(n)            static_cast<INTERNET_PORT>(hb_parni(n))

/*
WINBOOL WINAPI WinHttpAddRequestHeaders(HINTERNET,LPCWSTR,DWORD,DWORD)
*/
HB_FUNC( WAWINHTTPADDREQUESTHEADERS )
{
  void * str{};
  wa_ret_BOOL(WinHttpAddRequestHeaders(wa_par_HINTERNET(1), HB_PARSTR(2, &str, nullptr), wa_par_DWORD(3), wa_par_DWORD(4)));
  hb_strfree(str);
}

/*
WINBOOL WINAPI WinHttpDetectAutoProxyConfigUrl(DWORD,LPWSTR*)
*/

/*
WINBOOL WINAPI WinHttpCheckPlatform(void)
*/
HB_FUNC( WAWINHTTPCHECKPLATFORM )
{
  wa_ret_BOOL(WinHttpCheckPlatform());
}

/*
WINBOOL WINAPI WinHttpCloseHandle(HINTERNET)
*/
HB_FUNC( WAWINHTTPCLOSEHANDLE )
{
  wa_ret_BOOL(WinHttpCloseHandle(wa_par_HINTERNET(1)));
}

/*
HINTERNET WINAPI WinHttpConnect(HINTERNET,LPCWSTR,INTERNET_PORT,DWORD)
*/
HB_FUNC( WAWINHTTPCONNECT )
{
  void * str{};
  wa_ret_HINTERNET(WinHttpConnect(wa_par_HINTERNET(1), HB_PARSTR(2, &str, nullptr), wa_par_INTERNET_PORT(3), wa_par_DWORD(4)));
  hb_strfree(str);
}

/*
WINBOOL WINAPI WinHttpCrackUrl(LPCWSTR,DWORD,DWORD,LPURL_COMPONENTS)
*/

/*
WINBOOL WINAPI WinHttpCreateUrl(LPURL_COMPONENTS,DWORD,LPWSTR,LPDWORD)
*/

/*
WINBOOL WINAPI WinHttpGetDefaultProxyConfiguration(WINHTTP_PROXY_INFO*)
*/

/*
WINBOOL WINAPI WinHttpGetIEProxyConfigForCurrentUser(WINHTTP_CURRENT_USER_IE_PROXY_CONFIG*)
*/

/*
WINBOOL WINAPI WinHttpGetProxyForUrl(HINTERNET,LPCWSTR,WINHTTP_AUTOPROXY_OPTIONS*,WINHTTP_PROXY_INFO*)
*/

/*
HINTERNET WINAPI WinHttpOpen(LPCWSTR,DWORD,LPCWSTR,LPCWSTR,DWORD)
*/
HB_FUNC( WAWINHTTPOPEN )
{
  void * str1{};
  void * str2{};
  void * str3{};
  wa_ret_HINTERNET(WinHttpOpen(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2), HB_PARSTR(3, &str2, nullptr), HB_PARSTR(4, &str3, nullptr), wa_par_DWORD(5)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
HINTERNET WINAPI WinHttpOpenRequest(HINTERNET,LPCWSTR,LPCWSTR,LPCWSTR,LPCWSTR,LPCWSTR*,DWORD)
*/

/*
WINBOOL WINAPI WinHttpQueryAuthParams(HINTERNET,DWORD,LPVOID*)
*/

/*
WINBOOL WINAPI WinHttpQueryAuthSchemes(HINTERNET,LPDWORD,LPDWORD,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryAuthSchemes([in] HINTERNET hRequest, [out] LPDWORD lpdwSupportedSchemes, [out] LPDWORD lpdwFirstScheme, [out] LPDWORD pdwAuthTarget)
*/
HB_FUNC( WAWINHTTPQUERYAUTHSCHEMES )
{
  DWORD dwSupportedSchemes{};
  DWORD dwFirstScheme{};
  DWORD dwAuthTarget{};
  wa_ret_BOOL(WinHttpQueryAuthSchemes(wa_par_HINTERNET(1), &dwSupportedSchemes, &dwFirstScheme, &dwAuthTarget));
  wa_stor_DWORD(dwSupportedSchemes, 2);
  wa_stor_DWORD(dwFirstScheme, 3);
  wa_stor_DWORD(dwAuthTarget, 4);
}

/*
WINBOOL WINAPI WinHttpQueryDataAvailable(HINTERNET,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryDataAvailable([in] HINTERNET hRequest, [out] LPDWORD lpdwNumberOfBytesAvailable)
*/
HB_FUNC( WAWINHTTPQUERYDATAAVAILABLE )
{
  DWORD dwNumberOfBytesAvailable{};
  wa_ret_BOOL(WinHttpQueryDataAvailable(wa_par_HINTERNET(1), &dwNumberOfBytesAvailable));
  wa_stor_DWORD(dwNumberOfBytesAvailable, 2);
}

/*
WINBOOL WINAPI WinHttpQueryHeaders(HINTERNET,DWORD,LPCWSTR,LPVOID,LPDWORD,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryHeaders([in] HINTERNET hRequest, [in] DWORD dwInfoLevel, [in, optional] LPCWSTR pwszName, [out] LPVOID lpBuffer, [in, out] LPDWORD lpdwBufferLength, [in, out] LPDWORD lpdwIndex)
*/
HB_FUNC( WAWINHTTPQUERYHEADERS )
{
  LPVOID Buffer{};
  DWORD dwBufferLength = wa_par_DWORD(5);
  DWORD dwIndex = wa_par_DWORD(6);
  wa_ret_BOOL(WinHttpQueryHeaders(wa_par_HINTERNET(1), wa_par_DWORD(2), wa_par_LPCWSTR(3), Buffer, &dwBufferLength, &dwIndex));
  wa_stor_LPVOID(Buffer, 4);
  wa_stor_DWORD(dwBufferLength, 5);
  wa_stor_DWORD(dwIndex, 6);
}

/*
WINBOOL WINAPI WinHttpQueryOption(HINTERNET,DWORD,LPVOID,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryOption([in] HINTERNET hInternet, [in] DWORD dwOption, [out] LPVOID lpBuffer, [in, out] LPDWORD lpdwBufferLength)
*/
HB_FUNC( WAWINHTTPQUERYOPTION )
{
  LPVOID Buffer{};
  DWORD dwBufferLength = wa_par_DWORD(4);
  wa_ret_BOOL(WinHttpQueryOption(wa_par_HINTERNET(1), wa_par_DWORD(2), Buffer, &dwBufferLength));
  wa_stor_LPVOID(Buffer, 3);
  wa_stor_DWORD(dwBufferLength, 4);
}

/*
WINBOOL WINAPI WinHttpReadData(HINTERNET,LPVOID,DWORD,LPDWORD)
WINHTTPAPI BOOL WinHttpReadData([in] HINTERNET hRequest, [out] LPVOID lpBuffer, [in] DWORD dwNumberOfBytesToRead, [out] LPDWORD lpdwNumberOfBytesRead)
*/
HB_FUNC( WAWINHTTPREADDATA )
{
  LPVOID Buffer{};
  DWORD dwNumberOfBytesRead{};
  wa_ret_BOOL(WinHttpReadData(wa_par_HINTERNET(1), Buffer, wa_par_DWORD(3), &dwNumberOfBytesRead));
  wa_stor_LPVOID(Buffer, 2);
  wa_stor_DWORD(dwNumberOfBytesRead, 4);
}

/*
WINBOOL WINAPI WinHttpReceiveResponse(HINTERNET,LPVOID)
WINHTTPAPI BOOL WinHttpReceiveResponse([in] HINTERNET hRequest, [in] LPVOID lpReserved)
*/
HB_FUNC( WAWINHTTPRECEIVERESPONSE )
{
  wa_ret_BOOL(WinHttpReceiveResponse(wa_par_HINTERNET(1), wa_par_LPVOID(2)));
}

/*
WINBOOL WINAPI WinHttpSendRequest(HINTERNET,LPCWSTR,DWORD,LPVOID,DWORD,DWORD,DWORD_PTR)
WINHTTPAPI BOOL WinHttpSendRequest([in] HINTERNET hRequest, [in, optional] LPCWSTR lpszHeaders, [in] DWORD dwHeadersLength, [in, optional] LPVOID lpOptional, [in] DWORD dwOptionalLength, [in] DWORD dwTotalLength, [in] DWORD_PTR dwContext)
*/
HB_FUNC( WAWINHTTPSENDREQUEST )
{
  void * str{};
  wa_ret_BOOL(WinHttpSendRequest(wa_par_HINTERNET(1), HB_PARSTR(2, &str, nullptr), wa_par_DWORD(3), wa_par_LPVOID(4), wa_par_DWORD(5), wa_par_DWORD(6), wa_par_DWORD_PTR(7)));
  hb_strfree(str);
}

/*
WINBOOL WINAPI WinHttpSetDefaultProxyConfiguration(WINHTTP_PROXY_INFO*)
*/

/*
WINBOOL WINAPI WinHttpSetCredentials(HINTERNET,DWORD,DWORD,LPCWSTR,LPCWSTR,LPVOID)
*/
HB_FUNC( WAWINHTTPSETCREDENTIALS )
{
  void * str1{};
  void * str2{};
  wa_ret_BOOL(WinHttpSetCredentials(wa_par_HINTERNET(1), wa_par_DWORD(2), wa_par_DWORD(3), HB_PARSTR(4, &str1, nullptr), HB_PARSTR(5, &str2, nullptr), static_cast<LPVOID>(hb_parptr(6))));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINBOOL WINAPI WinHttpSetOption(HINTERNET,DWORD,LPVOID,DWORD)
*/
HB_FUNC( WAWINHTTPSETOPTION )
{
  wa_ret_BOOL(WinHttpSetOption(wa_par_HINTERNET(1), wa_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}

/*
WINHTTP_STATUS_CALLBACK WINAPI WinHttpSetStatusCallback(HINTERNET,WINHTTP_STATUS_CALLBACK,DWORD,DWORD_PTR)
*/

/*
WINBOOL WINAPI WinHttpSetTimeouts(HINTERNET,int,int,int,int)
*/
HB_FUNC( WAWINHTTPSETTIMEOUTS )
{
  wa_ret_BOOL(WinHttpSetTimeouts(wa_par_HINTERNET(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINBOOL WINAPI WinHttpTimeFromSystemTime(const SYSTEMTIME *,LPWSTR)
*/

/*
WINBOOL WINAPI WinHttpTimeToSystemTime(LPCWSTR,SYSTEMTIME*)
*/
HB_FUNC( WAWINHTTPTIMETOSYSTEMTIME )
{
  void * str{};
  wa_ret_BOOL(WinHttpTimeToSystemTime(HB_PARSTR(1, &str, nullptr), wa_par_SYSTEMTIME(2)));
  hb_strfree(str);
}

/*
WINBOOL WINAPI WinHttpWriteData(HINTERNET,LPCVOID,DWORD,LPDWORD)
*/
HB_FUNC( WAWINHTTPWRITEDATA )
{
  wa_ret_BOOL(WinHttpWriteData(wa_par_HINTERNET(1), static_cast<LPCVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}
