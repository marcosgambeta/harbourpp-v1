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
#include <winnetwk.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// DWORD WINAPI WNetAddConnectionA(LPCSTR lpRemoteName,LPCSTR lpPassword,LPCSTR lpLocalName)
#if 0
HB_FUNC( WAWNETADDCONNECTIONA )
{
  wa_ret_DWORD(WNetAddConnectionA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}
#endif

// DWORD WINAPI WNetAddConnectionW(LPCWSTR lpRemoteName,LPCWSTR lpPassword,LPCWSTR lpLocalName)
#if 0
HB_FUNC( WAWNETADDCONNECTIONW )
{
  wa_ret_DWORD(WNetAddConnectionW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WAWNETADDCONNECTION )
{
  void * str1{};
  void * str2{};
  void * str3{};
  wa_ret_DWORD(WNetAddConnection(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}

// DWORD WINAPI WNetAddConnection2A(LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
#if 0
HB_FUNC( WAWNETADDCONNECTION2A )
{
  wa_ret_DWORD(WNetAddConnection2A(static_cast<LPNETRESOURCEA>(hb_parptr(1)), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_DWORD(4)));
}
#endif

// DWORD WINAPI WNetAddConnection2W(LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
#if 0
HB_FUNC( WAWNETADDCONNECTION2W )
{
  wa_ret_DWORD(WNetAddConnection2W(static_cast<LPNETRESOURCEW>(hb_parptr(1)), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_DWORD(4)));
}
#endif

HB_FUNC( WAWNETADDCONNECTION2 )
{
  void * str2{};
  void * str3{};
  wa_ret_DWORD(WNetAddConnection2(static_cast<LPNETRESOURCEW>(hb_parptr(1)), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_DWORD(4)));
  hb_strfree(str2);
  hb_strfree(str3);
}

// DWORD WINAPI WNetAddConnection3A(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserName,DWORD dwFlags)
#if 0
HB_FUNC( WAWNETADDCONNECTION3A )
{
  wa_ret_DWORD(WNetAddConnection3A(wa_par_HWND(1), static_cast<LPNETRESOURCEA>(hb_parptr(2)), wa_par_LPCSTR(3), wa_par_LPCSTR(4), wa_par_DWORD(5)));
}
#endif

// DWORD WINAPI WNetAddConnection3W(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserName,DWORD dwFlags)
#if 0
HB_FUNC( WAWNETADDCONNECTION3W )
{
  wa_ret_DWORD(WNetAddConnection3W(wa_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4), wa_par_DWORD(5)));
}
#endif

HB_FUNC( WAWNETADDCONNECTION3 )
{
  void * str3{};
  void * str4{};
  wa_ret_DWORD(WNetAddConnection3(wa_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), wa_par_DWORD(5)));
  hb_strfree(str3);
  hb_strfree(str4);
}

// DWORD WINAPI WNetCancelConnectionA(LPCSTR lpName,WINBOOL fForce)
#if 0
HB_FUNC( WAWNETCANCELCONNECTIONA )
{
  wa_ret_DWORD(WNetCancelConnectionA(wa_par_LPCSTR(1), wa_par_BOOL(2)));
}
#endif

// DWORD WINAPI WNetCancelConnectionW(LPCWSTR lpName,WINBOOL fForce)
#if 0
HB_FUNC( WAWNETCANCELCONNECTIONW )
{
  wa_ret_DWORD(WNetCancelConnectionW(wa_par_LPCWSTR(1), wa_par_BOOL(2)));
}
#endif

HB_FUNC( WAWNETCANCELCONNECTION )
{
  void * str1{};
  wa_ret_DWORD(WNetCancelConnection(HB_PARSTR(1, &str1, nullptr), wa_par_BOOL(2)));
  hb_strfree(str1);
}

// DWORD WINAPI WNetCancelConnection2A(LPCSTR lpName,DWORD dwFlags,WINBOOL fForce)
#if 0
HB_FUNC( WAWNETCANCELCONNECTION2A )
{
  wa_ret_DWORD(WNetCancelConnection2A(wa_par_LPCSTR(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}
#endif

// DWORD WINAPI WNetCancelConnection2W(LPCWSTR lpName,DWORD dwFlags,WINBOOL fForce)
#if 0
HB_FUNC( WAWNETCANCELCONNECTION2W )
{
  wa_ret_DWORD(WNetCancelConnection2W(wa_par_LPCWSTR(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}
#endif

HB_FUNC( WAWNETCANCELCONNECTION2 )
{
  void * str1{};
  wa_ret_DWORD(WNetCancelConnection2(HB_PARSTR(1, &str1, nullptr), wa_par_DWORD(2), wa_par_BOOL(3)));
  hb_strfree(str1);
}

// DWORD WINAPI WNetGetConnectionA(LPCSTR lpLocalName,LPSTR lpRemoteName,LPDWORD lpnLength)
#if 0
HB_FUNC( WAWNETGETCONNECTIONA )
{
  DWORD nLength{};
  wa_ret_DWORD(WNetGetConnectionA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), &nLength));
  wa_stor_DWORD(nLength, 3);
}
#endif

// DWORD WINAPI WNetGetConnectionW(LPCWSTR lpLocalName,LPWSTR lpRemoteName,LPDWORD lpnLength)
#if 0
HB_FUNC( WAWNETGETCONNECTIONW )
{
  DWORD nLength{};
  wa_ret_DWORD(WNetGetConnectionW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), &nLength));
  wa_stor_DWORD(nLength, 3);
}
#endif

HB_FUNC( WAWNETGETCONNECTION )
{
  void * str1{};
  DWORD nLength{};
  wa_ret_DWORD(WNetGetConnection(HB_PARSTR(1, &str1, nullptr), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), &nLength));
  wa_stor_DWORD(nLength, 3);
  hb_strfree(str1);
}

// DWORD WINAPI WNetUseConnectionA(HWND hwndOwner,LPNETRESOURCEA lpNetResource,LPCSTR lpPassword,LPCSTR lpUserID,DWORD dwFlags,LPSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
#if 0
HB_FUNC( WAWNETUSECONNECTIONA )
{
  DWORD BufferSize{};
  DWORD Result{};
  wa_ret_DWORD(WNetUseConnectionA(wa_par_HWND(1), static_cast<LPNETRESOURCEA>(hb_parptr(2)), wa_par_LPCSTR(3), wa_par_LPCSTR(4), wa_par_DWORD(5), const_cast<LPSTR>(hb_parc(6)), &BufferSize, &Result));
  wa_stor_DWORD(BufferSize, 7);
  wa_stor_DWORD(Result, 8);
}
#endif

// DWORD WINAPI WNetUseConnectionW(HWND hwndOwner,LPNETRESOURCEW lpNetResource,LPCWSTR lpPassword,LPCWSTR lpUserID,DWORD dwFlags,LPWSTR lpAccessName,LPDWORD lpBufferSize,LPDWORD lpResult)
#if 0
HB_FUNC( WAWNETUSECONNECTIONW )
{
  DWORD BufferSize{};
  DWORD Result{};
  wa_ret_DWORD(WNetUseConnectionW(wa_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4), wa_par_DWORD(5), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(6))), &BufferSize, &Result));
  wa_stor_DWORD(BufferSize, 7);
  wa_stor_DWORD(Result, 8);
}
#endif

HB_FUNC( WAWNETUSECONNECTION )
{
  void * str3{};
  void * str4{};
  DWORD BufferSize{};
  DWORD Result{};
  wa_ret_DWORD(WNetUseConnection(wa_par_HWND(1), static_cast<LPNETRESOURCEW>(hb_parptr(2)), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), wa_par_DWORD(5), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(6))), &BufferSize, &Result));
  wa_stor_DWORD(BufferSize, 7);
  wa_stor_DWORD(Result, 8);
  hb_strfree(str3);
  hb_strfree(str4);
}

// DWORD WINAPI WNetConnectionDialog(HWND hwnd,DWORD dwType)
HB_FUNC( WAWNETCONNECTIONDIALOG )
{
  wa_ret_DWORD(WNetConnectionDialog(wa_par_HWND(1), wa_par_DWORD(2)));
}

// DWORD WINAPI WNetDisconnectDialog(HWND hwnd,DWORD dwType)
HB_FUNC( WAWNETDISCONNECTDIALOG )
{
  wa_ret_DWORD(WNetDisconnectDialog(wa_par_HWND(1), wa_par_DWORD(2)));
}

// DWORD WINAPI WNetRestoreSingleConnectionW(HWND hwndParent, LPCWSTR lpDevice, BOOL fUseUI)

// DWORD WINAPI WNetRestoreConnectionA(HWND hwndParent,LPCSTR lpDevice)
#if 0
HB_FUNC( WAWNETRESTORECONNECTIONA )
{
  wa_ret_DWORD(WNetRestoreConnectionA(wa_par_HWND(1), wa_par_LPCSTR(2)));
}
#endif

// DWORD WINAPI WNetRestoreConnectionW(HWND hwndParent,LPCWSTR lpDevice)
#if 0
HB_FUNC( WAWNETRESTORECONNECTIONW )
{
  wa_ret_DWORD(WNetRestoreConnectionW(wa_par_HWND(1), wa_par_LPCWSTR(2)));
}
#endif

// DWORD WINAPI WNetConnectionDialog1A(LPCONNECTDLGSTRUCTA lpConnDlgStruct)
HB_FUNC( WAWNETCONNECTIONDIALOG1A )
{
  wa_ret_DWORD(WNetConnectionDialog1A(static_cast<LPCONNECTDLGSTRUCTA>(hb_parptr(1))));
}

// DWORD WINAPI WNetConnectionDialog1W(LPCONNECTDLGSTRUCTW lpConnDlgStruct)
HB_FUNC( WAWNETCONNECTIONDIALOG1W )
{
  wa_ret_DWORD(WNetConnectionDialog1W(static_cast<LPCONNECTDLGSTRUCTW>(hb_parptr(1))));
}

// DWORD WINAPI WNetDisconnectDialog1A(LPDISCDLGSTRUCTA lpConnDlgStruct)
HB_FUNC( WAWNETDISCONNECTDIALOG1A )
{
  wa_ret_DWORD(WNetDisconnectDialog1A(static_cast<LPDISCDLGSTRUCTA>(hb_parptr(1))));
}

// DWORD WINAPI WNetDisconnectDialog1W(LPDISCDLGSTRUCTW lpConnDlgStruct)
HB_FUNC( WAWNETDISCONNECTDIALOG1W )
{
  wa_ret_DWORD(WNetDisconnectDialog1W(static_cast<LPDISCDLGSTRUCTW>(hb_parptr(1))));
}

// DWORD WINAPI WNetOpenEnumA(DWORD dwScope,DWORD dwType,DWORD dwUsage,LPNETRESOURCEA lpNetResource,LPHANDLE lphEnum)

// DWORD WINAPI WNetOpenEnumW(DWORD dwScope,DWORD dwType,DWORD dwUsage,LPNETRESOURCEW lpNetResource,LPHANDLE lphEnum)

// DWORD WINAPI WNetEnumResourceA(HANDLE hEnum,LPDWORD lpcCount,LPVOID lpBuffer,LPDWORD lpBufferSize)
HB_FUNC( WAWNETENUMRESOURCEA )
{
  DWORD cCount{};
  //LPVOID lpBuffer
  DWORD BufferSize{};
  wa_ret_DWORD(WNetEnumResourceA(wa_par_HANDLE(1), &cCount, static_cast<LPVOID>(hb_parptr(3)), &BufferSize));
  wa_stor_DWORD(cCount, 2);
  //LPVOID lpBuffer
  wa_stor_DWORD(BufferSize, 4);

}

// DWORD WINAPI WNetEnumResourceW(HANDLE hEnum,LPDWORD lpcCount,LPVOID lpBuffer,LPDWORD lpBufferSize)
HB_FUNC( WAWNETENUMRESOURCEW )
{
  DWORD cCount{};
  //LPVOID lpBuffer
  DWORD BufferSize{};
  wa_ret_DWORD(WNetEnumResourceW(wa_par_HANDLE(1), &cCount, static_cast<LPVOID>(hb_parptr(3)), &BufferSize));
  wa_stor_DWORD(cCount, 2);
  //LPVOID lpBuffer
  wa_stor_DWORD(BufferSize, 4);
}

// DWORD WINAPI WNetCloseEnum(HANDLE hEnum)
HB_FUNC( WAWNETCLOSEENUM )
{
  wa_ret_DWORD(WNetCloseEnum(wa_par_HANDLE(1)));
}

// DWORD WINAPI WNetGetResourceParentA(LPNETRESOURCEA lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
HB_FUNC( WAWNETGETRESOURCEPARENTA )
{
  DWORD cbBuffer{};
  wa_ret_DWORD(WNetGetResourceParentA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), &cbBuffer));
  wa_stor_DWORD(cbBuffer, 3);
}

// DWORD WINAPI WNetGetResourceParentW(LPNETRESOURCEW lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer)
HB_FUNC( WAWNETGETRESOURCEPARENTW )
{
  DWORD cbBuffer{};
  wa_ret_DWORD(WNetGetResourceParentW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPVOID>(hb_parptr(2)), &cbBuffer));
  wa_stor_DWORD(cbBuffer, 3);
}

// DWORD WINAPI WNetGetResourceInformationA(LPNETRESOURCEA lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer,LPSTR *lplpSystem)

// DWORD WINAPI WNetGetResourceInformationW(LPNETRESOURCEW lpNetResource,LPVOID lpBuffer,LPDWORD lpcbBuffer,LPWSTR *lplpSystem)

// DWORD WINAPI WNetGetUniversalNameA(LPCSTR lpLocalPath,DWORD dwInfoLevel,LPVOID lpBuffer,LPDWORD lpBufferSize)
HB_FUNC( WAWNETGETUNIVERSALNAMEA )
{
  wa_ret_DWORD(WNetGetUniversalNameA(wa_par_LPCSTR(1), wa_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

// DWORD WINAPI WNetGetUniversalNameW(LPCWSTR lpLocalPath,DWORD dwInfoLevel,LPVOID lpBuffer,LPDWORD lpBufferSize)
HB_FUNC( WAWNETGETUNIVERSALNAMEW )
{
  wa_ret_DWORD(WNetGetUniversalNameW(wa_par_LPCWSTR(1), wa_par_DWORD(2), static_cast<LPVOID>(hb_parptr(3)), static_cast<LPDWORD>(hb_parptr(4))));
}

// DWORD WINAPI WNetGetUserA(LPCSTR lpName,LPSTR lpUserName,LPDWORD lpnLength)
HB_FUNC( WAWNETGETUSERA )
{
  wa_ret_DWORD(WNetGetUserA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), static_cast<LPDWORD>(hb_parptr(3))));
}

// DWORD WINAPI WNetGetUserW(LPCWSTR lpName,LPWSTR lpUserName,LPDWORD lpnLength)
HB_FUNC( WAWNETGETUSERW )
{
  wa_ret_DWORD(WNetGetUserW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), static_cast<LPDWORD>(hb_parptr(3))));
}

// DWORD WINAPI WNetGetProviderNameA(DWORD dwNetType,LPSTR lpProviderName,LPDWORD lpBufferSize)
HB_FUNC( WAWNETGETPROVIDERNAMEA )
{
  wa_ret_DWORD(WNetGetProviderNameA(wa_par_DWORD(1), const_cast<LPSTR>(hb_parc(2)), static_cast<LPDWORD>(hb_parptr(3))));
}

// DWORD WINAPI WNetGetProviderNameW(DWORD dwNetType,LPWSTR lpProviderName,LPDWORD lpBufferSize)
HB_FUNC( WAWNETGETPROVIDERNAMEW )
{
  wa_ret_DWORD(WNetGetProviderNameW(wa_par_DWORD(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), static_cast<LPDWORD>(hb_parptr(3))));
}

// DWORD WINAPI WNetGetNetworkInformationA(LPCSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
HB_FUNC( WAWNETGETNETWORKINFORMATIONA )
{
  wa_ret_DWORD(WNetGetNetworkInformationA(wa_par_LPCSTR(1), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

// DWORD WINAPI WNetGetNetworkInformationW(LPCWSTR lpProvider,LPNETINFOSTRUCT lpNetInfoStruct)
HB_FUNC( WAWNETGETNETWORKINFORMATIONW )
{
  wa_ret_DWORD(WNetGetNetworkInformationW(wa_par_LPCWSTR(1), static_cast<LPNETINFOSTRUCT>(hb_parptr(2))));
}

// DWORD WINAPI WNetGetLastErrorA(LPDWORD lpError,LPSTR lpErrorBuf,DWORD nErrorBufSize,LPSTR lpNameBuf,DWORD nNameBufSize)
HB_FUNC( WAWNETGETLASTERRORA )
{
  wa_ret_DWORD(WNetGetLastErrorA(static_cast<LPDWORD>(hb_parptr(1)), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3), const_cast<LPSTR>(hb_parc(4)), wa_par_DWORD(5)));
}

// DWORD WINAPI WNetGetLastErrorW(LPDWORD lpError,LPWSTR lpErrorBuf,DWORD nErrorBufSize,LPWSTR lpNameBuf,DWORD nNameBufSize)
HB_FUNC( WAWNETGETLASTERRORW )
{
  wa_ret_DWORD(WNetGetLastErrorW(static_cast<LPDWORD>(hb_parptr(1)), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4))), wa_par_DWORD(5)));
}

// DWORD WINAPI MultinetGetConnectionPerformanceA(LPNETRESOURCEA lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
HB_FUNC( WAMULTINETGETCONNECTIONPERFORMANCEA )
{
  wa_ret_DWORD(MultinetGetConnectionPerformanceA(static_cast<LPNETRESOURCEA>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}

// DWORD WINAPI MultinetGetConnectionPerformanceW(LPNETRESOURCEW lpNetResource,LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
HB_FUNC( WAMULTINETGETCONNECTIONPERFORMANCEW )
{
  wa_ret_DWORD(MultinetGetConnectionPerformanceW(static_cast<LPNETRESOURCEW>(hb_parptr(1)), static_cast<LPNETCONNECTINFOSTRUCT>(hb_parptr(2))));
}
