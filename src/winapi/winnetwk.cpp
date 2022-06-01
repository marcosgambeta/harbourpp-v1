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
#include <winnetwk.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

/*
DWORD WINAPI WNetAddConnectionA(LPCSTR lpRemoteName,LPCSTR lpPassword,LPCSTR lpLocalName)
*/
HB_FUNC( WINAPI_WNETADDCONNECTIONA )
{
  hb_retnl(WNetAddConnectionA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
DWORD WINAPI WNetAddConnectionW(LPCWSTR lpRemoteName,LPCWSTR lpPassword,LPCWSTR lpLocalName)
*/
HB_FUNC( WINAPI_WNETADDCONNECTIONW )
{
  hb_retnl(WNetAddConnectionW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3)));
}

/*
DWORD WINAPI WNetAddConnection2A(LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WINAPI_WNETADDCONNECTION2A )
{
  hb_retnl(WNetAddConnection2A(static_cast<LPNETRESOURCEA>(hb_parptr(1)), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), static_cast<DWORD>(hb_parnl(4))));
}

/*
DWORD WINAPI WNetAddConnection2W(LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WINAPI_WNETADDCONNECTION2W )
{
  hb_retnl(WNetAddConnection2W(static_cast<LPNETRESOURCEW>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), static_cast<DWORD>(hb_parnl(4))));
}

/*
DWORD WINAPI WNetAddConnection3A(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WINAPI_WNETADDCONNECTION3A )
{
  hb_retnl(WNetAddConnection3A(static_cast<HWND>(hb_parptr(1)), static_cast<LPNETRESOURCEA>(hb_parptr(2)), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
DWORD WINAPI WNetAddConnection3W(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
*/
HB_FUNC( WINAPI_WNETADDCONNECTION3W )
{
  hb_retnl(WNetAddConnection3W(static_cast<HWND>(hb_parptr(1)), static_cast<LPNETRESOURCEW>(hb_parptr(2)), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
DWORD WINAPI WNetCancelConnectionA(LPCSTR lpName,WINBOOL fForce)
*/
HB_FUNC( WINAPI_WNETCANCELCONNECTIONA )
{
  hb_retnl(WNetCancelConnectionA(( LPCSTR ) hb_parc(1), hb_parl(2)));
}

/*
DWORD WINAPI WNetCancelConnectionW(LPCWSTR lpName,WINBOOL fForce)
*/
HB_FUNC( WINAPI_WNETCANCELCONNECTIONW )
{
  hb_retnl(WNetCancelConnectionW(( LPCWSTR ) hb_parc(1), hb_parl(2)));
}

/*
DWORD WINAPI WNetCancelConnection2A(LPCSTR lpName,DWORD dwFlags,WINBOOL fForce)
*/
HB_FUNC( WINAPI_WNETCANCELCONNECTION2A )
{
  hb_retnl(WNetCancelConnection2A(( LPCSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), hb_parl(3)));
}

/*
DWORD WINAPI WNetCancelConnection2W(LPCWSTR lpName,DWORD dwFlags,WINBOOL fForce)
*/
HB_FUNC( WINAPI_WNETCANCELCONNECTION2W )
{
  hb_retnl(WNetCancelConnection2W(( LPCWSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), hb_parl(3)));
}

/*
DWORD WINAPI WNetGetConnectionA(LPCSTR lpLocalName,LPSTR lpRemoteName,LPDWORD lpnLength)
*/
HB_FUNC( WINAPI_WNETGETCONNECTIONA )
{
  hb_retnl(WNetGetConnectionA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetConnectionW(LPCWSTR lpLocalName,LPWSTR lpRemoteName,LPDWORD lpnLength)
*/
HB_FUNC( WINAPI_WNETGETCONNECTIONW )
{
  hb_retnl(WNetGetConnectionW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetRestoreConnectionA(HWND hwndParent,LPCSTR lpDevice)
*/
#if 0
HB_FUNC( WINAPI_WNETRESTORECONNECTIONA )
{
  hb_retnl(WNetRestoreConnectionA(static_cast<HWND>(hb_parptr(1)), ( LPCSTR ) hb_parc(2)));
}
#endif

/*
DWORD WINAPI WNetUseConnectionA(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserID,DWORD dwFlags,LPSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
*/
HB_FUNC( WINAPI_WNETUSECONNECTIONA )
{
  hb_retnl(WNetUseConnectionA(static_cast<HWND>(hb_parptr(1)), static_cast<LPNETRESOURCEA>(hb_parptr(2)), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5)), ( LPSTR ) hb_parc(6), static_cast<LPDWORD>(hb_parptr(7)), static_cast<LPDWORD>(hb_parptr(8))));
}

/*
DWORD WINAPI WNetUseConnectionW(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserID,DWORD dwFlags,LPWSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
*/
HB_FUNC( WINAPI_WNETUSECONNECTIONW )
{
  hb_retnl(WNetUseConnectionW(static_cast<HWND>(hb_parptr(1)), static_cast<LPNETRESOURCEW>(hb_parptr(2)), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5)), ( LPWSTR ) hb_parc(6), static_cast<LPDWORD>(hb_parptr(7)), static_cast<LPDWORD>(hb_parptr(8))));
}

/*
DWORD WINAPI WNetConnectionDialog(HWND hwnd,DWORD dwType)
*/
HB_FUNC( WINAPI_WNETCONNECTIONDIALOG )
{
  hb_retnl(WNetConnectionDialog(static_cast<HWND>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
DWORD WINAPI WNetDisconnectDialog(HWND hwnd,DWORD dwType)
*/
HB_FUNC( WINAPI_WNETDISCONNECTDIALOG )
{
  hb_retnl(WNetDisconnectDialog(static_cast<HWND>(hb_parptr(1)), static_cast<DWORD>(hb_parnl(2))));
}

/*
DWORD WINAPI WNetRestoreSingleConnectionW(HWND hwndParent, LPCWSTR lpDevice, BOOL fUseUI)
*/

/*
DWORD WINAPI WNetRestoreConnectionW(HWND hwndParent,LPCWSTR lpDevice)
*/
#if 0
HB_FUNC( WINAPI_WNETRESTORECONNECTIONW )
{
  hb_retnl(WNetRestoreConnectionW(static_cast<HWND>(hb_parptr(1)), ( LPCWSTR ) hb_parc(2)));
}
#endif

/*
DWORD WINAPI WNetConnectionDialog1A(LPCONNECTDLGSTRUCTA lpConnDlgStruct)
*/
HB_FUNC( WINAPI_WNETCONNECTIONDIALOG1A )
{
  hb_retnl(WNetConnectionDialog1A(static_cast<LPCONNECTDLGSTRUCTA>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetConnectionDialog1W(LPCONNECTDLGSTRUCTW lpConnDlgStruct)
*/
HB_FUNC( WINAPI_WNETCONNECTIONDIALOG1W )
{
  hb_retnl(WNetConnectionDialog1W(static_cast<LPCONNECTDLGSTRUCTW>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetDisconnectDialog1A(LPDISCDLGSTRUCTA lpConnDlgStruct)
*/
HB_FUNC( WINAPI_WNETDISCONNECTDIALOG1A )
{
  hb_retnl(WNetDisconnectDialog1A(static_cast<LPDISCDLGSTRUCTA>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetDisconnectDialog1W(LPDISCDLGSTRUCTW lpConnDlgStruct)
*/
HB_FUNC( WINAPI_WNETDISCONNECTDIALOG1W )
{
  hb_retnl(WNetDisconnectDialog1W(static_cast<LPDISCDLGSTRUCTW>(hb_parptr(1))));
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
HB_FUNC( WINAPI_WNETENUMRESOURCEA )
{
  hb_retnl(WNetEnumResourceA(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetEnumResourceW(HANDLE hEnum,LPDWORD lpcCount,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WINAPI_WNETENUMRESOURCEW )
{
  hb_retnl(WNetEnumResourceW(static_cast<HANDLE>(hb_parptr(1)), static_cast<LPDWORD>(hb_parptr(2)), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetCloseEnum(HANDLE hEnum)
*/
HB_FUNC( WINAPI_WNETCLOSEENUM )
{
  hb_retnl(WNetCloseEnum(static_cast<HANDLE>(hb_parptr(1))));
}

/*
DWORD WINAPI WNetGetResourceParentA(LPNETRESOURCEA lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
*/
HB_FUNC( WINAPI_WNETGETRESOURCEPARENTA )
{
  hb_retnl(WNetGetResourceParentA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetResourceParentW(LPNETRESOURCEW lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
*/
HB_FUNC( WINAPI_WNETGETRESOURCEPARENTW )
{
  hb_retnl(WNetGetResourceParentW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), static_cast<LPDWORD>(hb_parptr(3))));
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
HB_FUNC( WINAPI_WNETGETUNIVERSALNAMEA )
{
  hb_retnl(WNetGetUniversalNameA(( LPCSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetGetUniversalNameW(LPCWSTR lpLocalPath,DWORD dwInfoLevel,LPVOID lpBuffer,LPDWORD lpBufferSize)
*/
HB_FUNC( WINAPI_WNETGETUNIVERSALNAMEW )
{
  hb_retnl(WNetGetUniversalNameW(( LPCWSTR ) hb_parc(1), static_cast<DWORD>(hb_parnl(2)), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
DWORD WINAPI WNetGetUserA(LPCSTR lpName,LPSTR lpUserName,LPDWORD lpnLength)
*/
HB_FUNC( WINAPI_WNETGETUSERA )
{
  hb_retnl(WNetGetUserA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetUserW(LPCWSTR lpName,LPWSTR lpUserName,LPDWORD lpnLength)
*/
HB_FUNC( WINAPI_WNETGETUSERW )
{
  hb_retnl(WNetGetUserW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetProviderNameA(DWORD dwNetType,LPSTR lpProviderName,LPDWORD lpBufferSize)
*/
HB_FUNC( WINAPI_WNETGETPROVIDERNAMEA )
{
  hb_retnl(WNetGetProviderNameA(static_cast<DWORD>(hb_parnl(1)), ( LPSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetProviderNameW(DWORD dwNetType,LPWSTR lpProviderName,LPDWORD lpBufferSize)
*/
HB_FUNC( WINAPI_WNETGETPROVIDERNAMEW )
{
  hb_retnl(WNetGetProviderNameW(static_cast<DWORD>(hb_parnl(1)), ( LPWSTR ) hb_parc(2), static_cast<LPDWORD>(hb_parptr(3))));
}

/*
DWORD WINAPI WNetGetNetworkInformationA(LPCSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
*/
HB_FUNC( WINAPI_WNETGETNETWORKINFORMATIONA )
{
  hb_retnl(WNetGetNetworkInformationA(( LPCSTR ) hb_parc(1), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI WNetGetNetworkInformationW(LPCWSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
*/
HB_FUNC( WINAPI_WNETGETNETWORKINFORMATIONW )
{
  hb_retnl(WNetGetNetworkInformationW(( LPCWSTR ) hb_parc(1), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI WNetGetLastErrorA(LPDWORD lpError,LPSTR lpErrorBuf,DWORD nErrorBufSize,LPSTR lpNameBuf,DWORD nNameBufSize)
*/
HB_FUNC( WINAPI_WNETGETLASTERRORA )
{
  hb_retnl(WNetGetLastErrorA(static_cast<LPDWORD>(hb_parptr(1)), ( LPSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3)), ( LPSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
DWORD WINAPI WNetGetLastErrorW(LPDWORD lpError,LPWSTR lpErrorBuf,DWORD nErrorBufSize,LPWSTR lpNameBuf,DWORD nNameBufSize)
*/
HB_FUNC( WINAPI_WNETGETLASTERRORW )
{
  hb_retnl(WNetGetLastErrorW(static_cast<LPDWORD>(hb_parptr(1)), ( LPWSTR ) hb_parc(2), static_cast<DWORD>(hb_parnl(3)), ( LPWSTR ) hb_parc(4), static_cast<DWORD>(hb_parnl(5))));
}

/*
DWORD WINAPI MultinetGetConnectionPerformanceA(LPNETRESOURCEA lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
*/
HB_FUNC( WINAPI_MULTINETGETCONNECTIONPERFORMANCEA )
{
  hb_retnl(MultinetGetConnectionPerformanceA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}

/*
DWORD WINAPI MultinetGetConnectionPerformanceW(LPNETRESOURCEW lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
*/
HB_FUNC( WINAPI_MULTINETGETCONNECTIONPERFORMANCEW )
{
  hb_retnl(MultinetGetConnectionPerformanceW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}
