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
#include <winhttp.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbwinuni.h"
#include "winapi.h"

/*
WINBOOL WINAPI WinHttpAddRequestHeaders(HINTERNET,LPCWSTR,DWORD,DWORD)
*/
HB_FUNC( WINAPI_WINHTTPADDREQUESTHEADERS )
{
  winapi_ret_BOOL(WinHttpAddRequestHeaders(winapi_par_HINTERNET(1), ( LPCWSTR ) hb_parc(2), winapi_par_DWORD(3), winapi_par_DWORD(4)));
}

/*
WINBOOL WINAPI WinHttpDetectAutoProxyConfigUrl(DWORD,LPWSTR*)
*/

/*
WINBOOL WINAPI WinHttpCheckPlatform(void)
*/
HB_FUNC( WINAPI_WINHTTPCHECKPLATFORM )
{
  winapi_ret_BOOL(WinHttpCheckPlatform());
}

/*
WINBOOL WINAPI WinHttpCloseHandle(HINTERNET)
*/
HB_FUNC( WINAPI_WINHTTPCLOSEHANDLE )
{
  winapi_ret_BOOL(WinHttpCloseHandle(winapi_par_HINTERNET(1)));
}

/*
HINTERNET WINAPI WinHttpConnect(HINTERNET,LPCWSTR,INTERNET_PORT,DWORD)
*/

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
HB_FUNC( WINAPI_WINHTTPOPEN )
{
  winapi_ret_HINTERNET(WinHttpOpen(( LPCWSTR ) hb_parc(1), winapi_par_DWORD(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4), winapi_par_DWORD(5)));
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
HB_FUNC( WINAPI_WINHTTPQUERYAUTHSCHEMES )
{
  DWORD dwSupportedSchemes;
  DWORD dwFirstScheme;
  DWORD dwAuthTarget;
  winapi_ret_BOOL(WinHttpQueryAuthSchemes(winapi_par_HINTERNET(1), &dwSupportedSchemes, &dwFirstScheme, &dwAuthTarget));
  winapi_stor_DWORD(dwSupportedSchemes, 2);
  winapi_stor_DWORD(dwFirstScheme, 3);
  winapi_stor_DWORD(dwAuthTarget, 4);
}

/*
WINBOOL WINAPI WinHttpQueryDataAvailable(HINTERNET,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryDataAvailable([in] HINTERNET hRequest, [out] LPDWORD lpdwNumberOfBytesAvailable)
*/
HB_FUNC( WINAPI_WINHTTPQUERYDATAAVAILABLE )
{
  DWORD dwNumberOfBytesAvailable;
  winapi_ret_BOOL(WinHttpQueryDataAvailable(winapi_par_HINTERNET(1), &dwNumberOfBytesAvailable));
  winapi_stor_DWORD(dwNumberOfBytesAvailable, 2);
}

/*
WINBOOL WINAPI WinHttpQueryHeaders(HINTERNET,DWORD,LPCWSTR,LPVOID,LPDWORD,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryHeaders([in] HINTERNET hRequest, [in] DWORD dwInfoLevel, [in, optional] LPCWSTR pwszName, [out] LPVOID lpBuffer, [in, out] LPDWORD lpdwBufferLength, [in, out] LPDWORD lpdwIndex)
*/
HB_FUNC( WINAPI_WINHTTPQUERYHEADERS )
{
  LPVOID Buffer = nullptr;
  DWORD dwBufferLength = winapi_par_DWORD(5);
  DWORD dwIndex = winapi_par_DWORD(6);
  winapi_ret_BOOL(WinHttpQueryHeaders(winapi_par_HINTERNET(1), winapi_par_DWORD(2), ( LPCWSTR ) hb_parc(3), Buffer, &dwBufferLength, &dwIndex));
  winapi_stor_LPVOID(Buffer, 4);
  winapi_stor_DWORD(dwBufferLength, 5);
  winapi_stor_DWORD(dwIndex, 6);
}

/*
WINBOOL WINAPI WinHttpQueryOption(HINTERNET,DWORD,LPVOID,LPDWORD)
WINHTTPAPI BOOL WinHttpQueryOption([in] HINTERNET hInternet, [in] DWORD dwOption, [out] LPVOID lpBuffer, [in, out] LPDWORD lpdwBufferLength)
*/
HB_FUNC( WINAPI_WINHTTPQUERYOPTION )
{
  LPVOID Buffer = nullptr;
  DWORD dwBufferLength = winapi_par_DWORD(4);
  winapi_ret_BOOL(WinHttpQueryOption(winapi_par_HINTERNET(1), winapi_par_DWORD(2), Buffer, &dwBufferLength));
  winapi_stor_LPVOID(Buffer, 3);
  winapi_stor_DWORD(dwBufferLength, 4);
}

/*
WINBOOL WINAPI WinHttpReadData(HINTERNET,LPVOID,DWORD,LPDWORD)
WINHTTPAPI BOOL WinHttpReadData([in] HINTERNET hRequest, [out] LPVOID lpBuffer, [in] DWORD dwNumberOfBytesToRead, [out] LPDWORD lpdwNumberOfBytesRead)
*/
HB_FUNC( WINAPI_WINHTTPREADDATA )
{
  LPVOID Buffer = nullptr;
  DWORD dwNumberOfBytesRead;
  winapi_ret_BOOL(WinHttpReadData(winapi_par_HINTERNET(1), Buffer, winapi_par_DWORD(3), &dwNumberOfBytesRead));
  winapi_stor_LPVOID(Buffer, 2);
  winapi_stor_DWORD(dwNumberOfBytesRead, 4);
}

/*
WINBOOL WINAPI WinHttpReceiveResponse(HINTERNET,LPVOID)
WINHTTPAPI BOOL WinHttpReceiveResponse([in] HINTERNET hRequest, [in] LPVOID lpReserved)
*/
HB_FUNC( WINAPI_WINHTTPRECEIVERESPONSE )
{
  winapi_ret_BOOL(WinHttpReceiveResponse(winapi_par_HINTERNET(1), winapi_par_LPVOID(2)));
}

/*
WINBOOL WINAPI WinHttpSendRequest(HINTERNET,LPCWSTR,DWORD,LPVOID,DWORD,DWORD,DWORD_PTR)
WINHTTPAPI BOOL WinHttpSendRequest([in] HINTERNET hRequest, [in, optional] LPCWSTR lpszHeaders, [in] DWORD dwHeadersLength, [in, optional] LPVOID lpOptional, [in] DWORD dwOptionalLength, [in] DWORD dwTotalLength, [in] DWORD_PTR dwContext)
*/
HB_FUNC( WINAPI_WINHTTPSENDREQUEST )
{
  winapi_ret_BOOL(WinHttpSendRequest(winapi_par_HINTERNET(1), ( LPCWSTR ) hb_parc(2), winapi_par_DWORD(3), winapi_par_LPVOID(4), winapi_par_DWORD(5), winapi_par_DWORD(6), winapi_par_DWORD_PTR(7)));
}

/*
WINBOOL WINAPI WinHttpSetDefaultProxyConfiguration(WINHTTP_PROXY_INFO*)
*/

/*
WINBOOL WINAPI WinHttpSetCredentials(HINTERNET,DWORD,DWORD,LPCWSTR,LPCWSTR,LPVOID)
*/
HB_FUNC( WINAPI_WINHTTPSETCREDENTIALS )
{
  winapi_ret_BOOL(WinHttpSetCredentials(winapi_par_HINTERNET(1), winapi_par_DWORD(2), winapi_par_DWORD(3), ( LPCWSTR ) hb_parc(4), ( LPCWSTR ) hb_parc(5), static_cast<LPVOID>(hb_parptr(6))));
}

/*
WINBOOL WINAPI WinHttpSetOption(HINTERNET,DWORD,LPVOID,DWORD)
*/
HB_FUNC( WINAPI_WINHTTPSETOPTION )
{
  winapi_ret_BOOL(WinHttpSetOption(winapi_par_HINTERNET(1), winapi_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), winapi_par_DWORD(4)));
}

/*
WINHTTP_STATUS_CALLBACK WINAPI WinHttpSetStatusCallback(HINTERNET,WINHTTP_STATUS_CALLBACK,DWORD,DWORD_PTR)
*/

/*
WINBOOL WINAPI WinHttpSetTimeouts(HINTERNET,int,int,int,int)
*/
HB_FUNC( WINAPI_WINHTTPSETTIMEOUTS )
{
  winapi_ret_BOOL(WinHttpSetTimeouts(winapi_par_HINTERNET(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINBOOL WINAPI WinHttpTimeFromSystemTime(const SYSTEMTIME *,LPWSTR)
*/

/*
WINBOOL WINAPI WinHttpTimeToSystemTime(LPCWSTR,SYSTEMTIME*)
*/
HB_FUNC( WINAPI_WINHTTPTIMETOSYSTEMTIME )
{
  winapi_ret_BOOL(WinHttpTimeToSystemTime(( LPCWSTR ) hb_parc(1), static_cast<SYSTEMTIME*>(winapi_get_ptr(2))));
}

/*
WINBOOL WINAPI WinHttpWriteData(HINTERNET,LPCVOID,DWORD,LPDWORD)
*/
HB_FUNC( WINAPI_WINHTTPWRITEDATA )
{
  winapi_ret_BOOL(WinHttpWriteData(winapi_par_HINTERNET(1), static_cast<LPCVOID>(hb_parptr(2)), winapi_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}
