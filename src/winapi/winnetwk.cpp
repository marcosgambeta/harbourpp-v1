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
#include <winnetwk.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
DWORD WINAPI WNetAddConnectionA(LPCSTR lpRemoteName,LPCSTR lpPassword,LPCSTR lpLocalName)
*/
HB_FUNC( WAWNETADDCONNECTIONA )
{
  winapi_ret_DWORD(WNetAddConnectionA(winapi_par_LPCSTR(1), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3)));
}

/*
DWORD WINAPI WNetAddConnectionW(LPCWSTR lpRemoteName,LPCWSTR lpPassword,LPCWSTR lpLocalName)
*/
HB_FUNC( WAWNETADDCONNECTIONW )
{
  winapi_ret_DWORD(WNetAddConnectionW(reinterpret_cast<LPCWSTR>(hb_parc(1)), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3))));
}

HB_FUNC( WAWNETADDCONNECTION )
{
  void * str1;
  void * str2;
  void * str3;
  winapi_ret_DWORD(WNetAddConnection(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
DWORD WINAPI WNetAddConnection2A(LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WAWNETADDCONNECTION2A )
{
  winapi_ret_DWORD(WNetAddConnection2A(static_cast<LPNETRESOURCEA>(hb_parptr(1)), winapi_par_LPCSTR(2), winapi_par_LPCSTR(3), winapi_par_DWORD(4)));
}

/*
DWORD WINAPI WNetAddConnection2W(LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WAWNETADDCONNECTION2W )
{
  winapi_ret_DWORD(WNetAddConnection2W(static_cast<LPNETRESOURCEW>(hb_parptr(1)), reinterpret_cast<LPCWSTR>(hb_parc(2)), reinterpret_cast<LPCWSTR>(hb_parc(3)), winapi_par_DWORD(4)));
}

HB_FUNC( WAWNETADDCONNECTION2 )
{
  void * str2;
  void * str3;
  winapi_ret_DWORD(WNetAddConnection2(static_cast<LPNETRESOURCEW>(hb_parptr(1)), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), winapi_par_DWORD(4)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
DWORD WINAPI WNetAddConnection3A(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WAWNETADDCONNECTION3A )
{
  winapi_ret_DWORD(WNetAddConnection3A(winapi_par_HWND(1), static_cast<LPNETRESOURCEA>(hb_parptr(2)), winapi_par_LPCSTR(3), winapi_par_LPCSTR(4), winapi_par_DWORD(5)));
}

/*
DWORD WINAPI WNetAddConnection3W(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WAWNETADDCONNECTION3W )
{
  winapi_ret_DWORD(WNetAddConnection3W(winapi_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), reinterpret_cast<LPCWSTR>(hb_parc(3)), reinterpret_cast<LPCWSTR>(hb_parc(4)), winapi_par_DWORD(5)));
}

HB_FUNC( WAWNETADDCONNECTION3 )
{
  void * str3;
  void * str4;
  winapi_ret_DWORD(WNetAddConnection3(winapi_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), winapi_par_DWORD(5)));
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
DWORD WINAPI WNetCancelConnectionA(LPCSTR lpName,WINBOOL fForce)
*/
HB_FUNC( WAWNETCANCELCONNECTIONA )
{
  winapi_ret_DWORD(WNetCancelConnectionA(winapi_par_LPCSTR(1), winapi_par_BOOL(2)));
}

/*
DWORD WINAPI WNetCancelConnectionW(LPCWSTR lpName,WINBOOL fForce)
*/
HB_FUNC( WAWNETCANCELCONNECTIONW )
{
  winapi_ret_DWORD(WNetCancelConnectionW(reinterpret_cast<LPCWSTR>(hb_parc(1)), winapi_par_BOOL(2)));
}

HB_FUNC( WAWNETCANCELCONNECTION )
{
  void * str1;
  winapi_ret_DWORD(WNetCancelConnection(HB_PARSTR(1, &str1, nullptr), winapi_par_BOOL(2)));
  hb_strfree(str1);
}

/*
DWORD WINAPI WNetCancelConnection2A(LPCSTR lpName,DWORD dwFlags,WINBOOL fForce)
*/
HB_FUNC( WAWNETCANCELCONNECTION2A )
{
  winapi_ret_DWORD(WNetCancelConnection2A(winapi_par_LPCSTR(1), winapi_par_DWORD(2), winapi_par_BOOL(3)));
}

/*
DWORD WINAPI WNetCancelConnection2W(LPCWSTR lpName,DWORD dwFlags,WINBOOL fForce)
*/
HB_FUNC( WAWNETCANCELCONNECTION2W )
{
  winapi_ret_DWORD(WNetCancelConnection2W(reinterpret_cast<LPCWSTR>(hb_parc(1)), winapi_par_DWORD(2), winapi_par_BOOL(3)));
}

HB_FUNC( WAWNETCANCELCONNECTION2 )
{
  void * str1;
  winapi_ret_DWORD(WNetCancelConnection2(HB_PARSTR(1, &str1, nullptr), winapi_par_DWORD(2), winapi_par_BOOL(3)));
  hb_strfree(str1);
}

/*
DWORD WINAPI WNetGetConnectionA(LPCSTR lpLocalName,LPSTR lpRemoteName,LPDWORD lpnLength)
*/
HB_FUNC( WAWNETGETCONNECTIONA )
{
  DWORD nLength;
  winapi_ret_DWORD(WNetGetConnectionA(winapi_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), &nLength));
  winapi_stor_DWORD(nLength, 3);
}

/*
DWORD WINAPI WNetGetConnectionW(LPCWSTR lpLocalName,LPWSTR lpRemoteName,LPDWORD lpnLength)
*/
HB_FUNC( WAWNETGETCONNECTIONW )
{
  DWORD nLength;
  winapi_ret_DWORD(WNetGetConnectionW(reinterpret_cast<LPCWSTR>(hb_parc(1)), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), &nLength));
  winapi_stor_DWORD(nLength, 3);
}

HB_FUNC( WAWNETGETCONNECTION )
{
  void * str1;
  DWORD nLength;
  winapi_ret_DWORD(WNetGetConnection(HB_PARSTR(1, &str1, nullptr), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), &nLength));
  winapi_stor_DWORD(nLength, 3);
  hb_strfree(str1);
}

/*
DWORD WINAPI WNetUseConnectionA(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserID,DWORD dwFlags,LPSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
*/
HB_FUNC( WAWNETUSECONNECTIONA )
{
  DWORD BufferSize;
  DWORD Result;
  winapi_ret_DWORD(WNetUseConnectionA(winapi_par_HWND(1), static_cast<LPNETRESOURCEA>(hb_parptr(2)), winapi_par_LPCSTR(3), winapi_par_LPCSTR(4), winapi_par_DWORD(5), const_cast<LPSTR>(hb_parc(6)), &BufferSize, &Result));
  winapi_stor_DWORD(BufferSize, 7);
  winapi_stor_DWORD(Result, 8);
}

/*
DWORD WINAPI WNetUseConnectionW(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserID,DWORD dwFlags,LPWSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
*/
HB_FUNC( WAWNETUSECONNECTIONW )
{
  DWORD BufferSize;
  DWORD Result;
  winapi_ret_DWORD(WNetUseConnectionW(winapi_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), reinterpret_cast<LPCWSTR>(hb_parc(3)), reinterpret_cast<LPCWSTR>(hb_parc(4)), winapi_par_DWORD(5), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(6))), &BufferSize, &Result));
  winapi_stor_DWORD(BufferSize, 7);
  winapi_stor_DWORD(Result, 8);
}

HB_FUNC( WAWNETUSECONNECTION )
{
  void * str3;
  void * str4;
  DWORD BufferSize;
  DWORD Result;
  winapi_ret_DWORD(WNetUseConnection(winapi_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), winapi_par_DWORD(5), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(6))), &BufferSize, &Result));
  winapi_stor_DWORD(BufferSize, 7);
  winapi_stor_DWORD(Result, 8);
  hb_strfree(str3);
  hb_strfree(str4);
}

/*
DWORD WINAPI WNetConnectionDialog(HWND hwnd,DWORD dwType)
*/
HB_FUNC( WAWNETCONNECTIONDIALOG )
{
  winapi_ret_DWORD(WNetConnectionDialog(winapi_par_HWND(1), winapi_par_DWORD(2)));
}

/*
DWORD WINAPI WNetDisconnectDialog(HWND hwnd,DWORD dwType)
*/
HB_FUNC( WAWNETDISCONNECTDIALOG )
{
  winapi_ret_DWORD(WNetDisconnectDialog(winapi_par_HWND(1), winapi_par_DWORD(2)));
}

/*
DWORD WINAPI WNetRestoreSingleConnectionW(HWND hwndParent, LPCWSTR lpDevice, BOOL fUseUI)
*/

/*
DWORD WINAPI WNetRestoreConnectionA(HWND hwndParent,LPCSTR lpDevice)
*/
#if 0
HB_FUNC( WAWNETRESTORECONNECTIONA )
{
  winapi_ret_DWORD(WNetRestoreConnectionA(winapi_par_HWND(1), winapi_par_LPCSTR(2)));
}
#endif

/*
DWORD WINAPI WNetRestoreConnectionW(HWND hwndParent,LPCWSTR lpDevice)
*/
#if 0
HB_FUNC( WAWNETRESTORECONNECTIONW )
{
  winapi_ret_DWORD(WNetRestoreConnectionW(winapi_par_HWND(1), reinterpret_cast<LPCWSTR>(hb_parc(2))));
}
#endif

/*
DWORD WINAPI WNetConnectionDialog1A(LPCONNECTDLGSTRUCTA lpConnDlgStruct)
*/
HB_FUNC( WAWNETCONNECTIONDIALOG1A )
{
  winapi_ret_DWORD(WNetConnectionDialog1A(static_cast<LPCONNECTDLGSTRUCTA>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetConnectionDialog1W(LPCONNECTDLGSTRUCTW lpConnDlgStruct)
*/
HB_FUNC( WAWNETCONNECTIONDIALOG1W )
{
  winapi_ret_DWORD(WNetConnectionDialog1W(static_cast<LPCONNECTDLGSTRUCTW>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetDisconnectDialog1A(LPDISCDLGSTRUCTA lpConnDlgStruct)
*/
HB_FUNC( WAWNETDISCONNECTDIALOG1A )
{
  winapi_ret_DWORD(WNetDisconnectDialog1A(static_cast<LPDISCDLGSTRUCTA>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetDisconnectDialog1W(LPDISCDLGSTRUCTW lpConnDlgStruct)
*/
HB_FUNC( WAWNETDISCONNECTDIALOG1W )
{
  winapi_ret_DWORD(WNetDisconnectDialog1W(static_cast<LPDISCDLGSTRUCTW>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetOpenEnumA(DWORD dwScope,DWORD dwType,DWORD dwUsage,LPNETRESOURCEA lpNetResource,LPHANDLE lphEnum)
*/

/*
DWORD WINAPI WNetOpenEnumW(DWORD dwScope,DWORD dwType,DWORD dwUsage,LPNETRESOURCEW lpNetResource,LPHANDLE lphEnum)
*/

/*
DWORD WINAPI WNetEnumResourceA(HANDLE hEnum,LPDWORD lpcCount,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETENUMRESOURCEA )
{
  DWORD cCount;
  //LPVOID lpBuffer
  DWORD BufferSize;
  winapi_ret_DWORD(WNetEnumResourceA(winapi_par_HANDLE(1), &cCount, static_cast<LPVOID>(hb_parptr(3)), &BufferSize));
  winapi_stor_DWORD(cCount, 2);
  //LPVOID lpBuffer
  winapi_stor_DWORD(BufferSize, 4);

}

/*
DWORD WINAPI WNetEnumResourceW(HANDLE hEnum,LPDWORD lpcCount,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETENUMRESOURCEW )
{
  DWORD cCount;
  //LPVOID lpBuffer
  DWORD BufferSize;
  winapi_ret_DWORD(WNetEnumResourceW(winapi_par_HANDLE(1), &cCount, static_cast<LPVOID>(hb_parptr(3)), &BufferSize));
  winapi_stor_DWORD(cCount, 2);
  //LPVOID lpBuffer
  winapi_stor_DWORD(BufferSize, 4);
}

/*
DWORD WINAPI WNetCloseEnum(HANDLE hEnum)
*/
HB_FUNC( WAWNETCLOSEENUM )
{
  winapi_ret_DWORD(WNetCloseEnum(winapi_par_HANDLE(1)));
}

/*
DWORD WINAPI WNetGetResourceParentA(LPNETRESOURCEA lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
*/
HB_FUNC( WAWNETGETRESOURCEPARENTA )
{
  DWORD cbBuffer;
  winapi_ret_DWORD(WNetGetResourceParentA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), &cbBuffer));
  winapi_stor_DWORD(cbBuffer, 3);
}

/*
DWORD WINAPI WNetGetResourceParentW(LPNETRESOURCEW lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
*/
HB_FUNC( WAWNETGETRESOURCEPARENTW )
{
  DWORD cbBuffer;
  winapi_ret_DWORD(WNetGetResourceParentW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), &cbBuffer));
  winapi_stor_DWORD(cbBuffer, 3);
}

/*
DWORD WINAPI WNetGetResourceInformationA(LPNETRESOURCEA lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer,LPSTR *lplpSystem)
*/

/*
DWORD WINAPI WNetGetResourceInformationW(LPNETRESOURCEW lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer,LPWSTR *lplpSystem)
*/

/*
DWORD WINAPI WNetGetUniversalNameA(LPCSTR lpLocalPath,DWORD dwInfoLevel,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETGETUNIVERSALNAMEA )
{
  winapi_ret_DWORD(WNetGetUniversalNameA(winapi_par_LPCSTR(1), winapi_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetGetUniversalNameW(LPCWSTR lpLocalPath,DWORD dwInfoLevel,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETGETUNIVERSALNAMEW )
{
  winapi_ret_DWORD(WNetGetUniversalNameW(reinterpret_cast<LPCWSTR>(hb_parc(1)), winapi_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetGetUserA(LPCSTR lpName,LPSTR lpUserName,LPDWORD lpnLength)
*/
HB_FUNC( WAWNETGETUSERA )
{
  winapi_ret_DWORD(WNetGetUserA(winapi_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetUserW(LPCWSTR lpName,LPWSTR lpUserName,LPDWORD lpnLength)
*/
HB_FUNC( WAWNETGETUSERW )
{
  winapi_ret_DWORD(WNetGetUserW(reinterpret_cast<LPCWSTR>(hb_parc(1)), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetProviderNameA(DWORD dwNetType,LPSTR lpProviderName,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETGETPROVIDERNAMEA )
{
  winapi_ret_DWORD(WNetGetProviderNameA(winapi_par_DWORD(1), const_cast<LPSTR>(hb_parc(2)), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetProviderNameW(DWORD dwNetType,LPWSTR lpProviderName,LPDWORD lpBufferSize)
*/
HB_FUNC( WAWNETGETPROVIDERNAMEW )
{
  winapi_ret_DWORD(WNetGetProviderNameW(winapi_par_DWORD(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetNetworkInformationA(LPCSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
*/
HB_FUNC( WAWNETGETNETWORKINFORMATIONA )
{
  winapi_ret_DWORD(WNetGetNetworkInformationA(winapi_par_LPCSTR(1), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI WNetGetNetworkInformationW(LPCWSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
*/
HB_FUNC( WAWNETGETNETWORKINFORMATIONW )
{
  winapi_ret_DWORD(WNetGetNetworkInformationW(reinterpret_cast<LPCWSTR>(hb_parc(1)), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI WNetGetLastErrorA(LPDWORD lpError,LPSTR lpErrorBuf,DWORD nErrorBufSize,LPSTR lpNameBuf,DWORD nNameBufSize)
*/
HB_FUNC( WAWNETGETLASTERRORA )
{
  winapi_ret_DWORD(WNetGetLastErrorA(static_cast<LPDWORD>(hb_parptr(1)), const_cast<LPSTR>(hb_parc(2)), winapi_par_DWORD(3), const_cast<LPSTR>(hb_parc(4)), winapi_par_DWORD(5)));
}

/*
DWORD WINAPI WNetGetLastErrorW(LPDWORD lpError,LPWSTR lpErrorBuf,DWORD nErrorBufSize,LPWSTR lpNameBuf,DWORD nNameBufSize)
*/
HB_FUNC( WAWNETGETLASTERRORW )
{
  winapi_ret_DWORD(WNetGetLastErrorW(static_cast<LPDWORD>(hb_parptr(1)), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), winapi_par_DWORD(3), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4))), winapi_par_DWORD(5)));
}

/*
DWORD WINAPI MultinetGetConnectionPerformanceA(LPNETRESOURCEA lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
*/
HB_FUNC( WAMULTINETGETCONNECTIONPERFORMANCEA )
{
  winapi_ret_DWORD(MultinetGetConnectionPerformanceA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI MultinetGetConnectionPerformanceW(LPNETRESOURCEW lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
*/
HB_FUNC( WAMULTINETGETCONNECTIONPERFORMANCEW )
{
  winapi_ret_DWORD(MultinetGetConnectionPerformanceW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}
