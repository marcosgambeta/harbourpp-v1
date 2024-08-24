//
// WINAPI For Harbour++ - Bindings libraries for Harbour++ and WINAPI
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

#ifndef _WINAPI_WINNETWK_
#define _WINAPI_WINNETWK_

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)

#define RESOURCE_CONNECTED                                           0x00000001
#define RESOURCE_GLOBALNET                                           0x00000002
#define RESOURCE_REMEMBERED                                          0x00000003
#define RESOURCE_RECENT                                              0x00000004
#define RESOURCE_CONTEXT                                             0x00000005

#define RESOURCETYPE_ANY                                             0x00000000
#define RESOURCETYPE_DISK                                            0x00000001
#define RESOURCETYPE_PRINT                                           0x00000002
#define RESOURCETYPE_RESERVED                                        0x00000008

#define RESOURCETYPE_UNKNOWN                                         0xFFFFFFFF

#define RESOURCEUSAGE_CONNECTABLE                                    0x00000001
#define RESOURCEUSAGE_CONTAINER                                      0x00000002
#define RESOURCEUSAGE_NOLOCALDEVICE                                  0x00000004
#define RESOURCEUSAGE_SIBLING                                        0x00000008
#define RESOURCEUSAGE_ATTACHED                                       0x00000010
#define RESOURCEUSAGE_ALL                                            hb_bitor(RESOURCEUSAGE_CONNECTABLE, RESOURCEUSAGE_CONTAINER, RESOURCEUSAGE_ATTACHED)
#define RESOURCEUSAGE_RESERVED                                       0x80000000

#define RESOURCEDISPLAYTYPE_GENERIC                                  0x00000000
#define RESOURCEDISPLAYTYPE_DOMAIN                                   0x00000001
#define RESOURCEDISPLAYTYPE_SERVER                                   0x00000002
#define RESOURCEDISPLAYTYPE_SHARE                                    0x00000003
#define RESOURCEDISPLAYTYPE_FILE                                     0x00000004
#define RESOURCEDISPLAYTYPE_GROUP                                    0x00000005
#define RESOURCEDISPLAYTYPE_NETWORK                                  0x00000006
#define RESOURCEDISPLAYTYPE_ROOT                                     0x00000007
#define RESOURCEDISPLAYTYPE_SHAREADMIN                               0x00000008
#define RESOURCEDISPLAYTYPE_DIRECTORY                                0x00000009
#define RESOURCEDISPLAYTYPE_TREE                                     0x0000000a
#define RESOURCEDISPLAYTYPE_NDSCONTAINER                             0x0000000b

#define NETPROPERTY_PERSISTENT                                       1

#define CONNECT_UPDATE_PROFILE                                       0x00000001
#define CONNECT_UPDATE_RECENT                                        0x00000002
#define CONNECT_TEMPORARY                                            0x00000004
#define CONNECT_INTERACTIVE                                          0x00000008
#define CONNECT_PROMPT                                               0x00000010
#define CONNECT_NEED_DRIVE                                           0x00000020
#define CONNECT_REFCOUNT                                             0x00000040
#define CONNECT_REDIRECT                                             0x00000080
#define CONNECT_LOCALDRIVE                                           0x00000100
#define CONNECT_CURRENT_MEDIA                                        0x00000200
#define CONNECT_DEFERRED                                             0x00000400
#define CONNECT_RESERVED                                             0xFF000000
#define CONNECT_COMMANDLINE                                          0x00000800
#define CONNECT_CMD_SAVECRED                                         0x00001000
// #if WINVER >= 0x0600
#define CONNECT_CRED_RESET                                           0x00002000
// #endif

// #define WNetAddConnection                                            __MINGW_NAME_AW(WNetAddConnection)
// #define WNetAddConnection2                                           __MINGW_NAME_AW(WNetAddConnection2)
// #define WNetAddConnection3                                           __MINGW_NAME_AW(WNetAddConnection3)
// #define WNetCancelConnection                                         __MINGW_NAME_AW(WNetCancelConnection)
// #define WNetCancelConnection2                                        __MINGW_NAME_AW(WNetCancelConnection2)
// #define WNetGetConnection                                            __MINGW_NAME_AW(WNetGetConnection)
// #define WNetRestoreConnection                                        __MINGW_NAME_AW(WNetRestoreConnection)
// #define WNetUseConnection                                            __MINGW_NAME_AW(WNetUseConnection)

// #if (_WIN32_WINNT >= _WIN32_WINNT_LONGHORN)
// #else
// #endif

#define CONNDLG_RO_PATH                                              0x00000001
#define CONNDLG_CONN_POINT                                           0x00000002
#define CONNDLG_USE_MRU                                              0x00000004
#define CONNDLG_HIDE_BOX                                             0x00000008

#define CONNDLG_PERSIST                                              0x00000010
#define CONNDLG_NOT_PERSIST                                          0x00000020

// #define WNetConnectionDialog1                                        __MINGW_NAME_AW(WNetConnectionDialog1)

#define DISC_UPDATE_PROFILE                                          0x00000001
#define DISC_NO_FORCE                                                0x00000040

// #define WNetDisconnectDialog1                                        __MINGW_NAME_AW(WNetDisconnectDialog1)
// #define WNetOpenEnum                                                 __MINGW_NAME_AW(WNetOpenEnum)
// #define WNetEnumResource                                             __MINGW_NAME_AW(WNetEnumResource)
// #define WNetGetResourceParent                                        __MINGW_NAME_AW(WNetGetResourceParent)
// #define WNetGetResourceInformation                                   __MINGW_NAME_AW(WNetGetResourceInformation)

#define UNIVERSAL_NAME_INFO_LEVEL                                    0x00000001
#define REMOTE_NAME_INFO_LEVEL                                       0x00000002

// #define WNetGetUniversalName                                         __MINGW_NAME_AW(WNetGetUniversalName)
// #define WNetGetUser                                                  __MINGW_NAME_AW(WNetGetUser)
// #define WNetGetProviderName                                          __MINGW_NAME_AW(WNetGetProviderName)

#define WNFMT_MULTILINE                                              0x01
#define WNFMT_ABBREVIATED                                            0x02
#define WNFMT_INENUM                                                 0x10
#define WNFMT_CONNECTION                                             0x20

#define NETINFO_DLL16                                                0x00000001
#define NETINFO_DISKRED                                              0x00000004
#define NETINFO_PRINTERRED                                           0x00000008

// #define WNetGetNetworkInformation                                    __MINGW_NAME_AW(WNetGetNetworkInformation)
// #define PFNGETPROFILEPATH                                            __MINGW_NAME_AW(PFNGETPROFILEPATH)
// #define PFNRECONCILEPROFILE                                          __MINGW_NAME_AW(PFNRECONCILEPROFILE)

#define RP_LOGON                                                     0x01
#define RP_INIFILE                                                   0x02

// #define PFNPROCESSPOLICIES                                           __MINGW_NAME_AW(PFNPROCESSPOLICIES)

#define PP_DISPLAYERRORS                                             0x01

// #define WNetGetLastError                                             __MINGW_NAME_AW(WNetGetLastError)

#define WN_SUCCESS                                                   NO_ERROR
#define WN_NO_ERROR                                                  NO_ERROR
#define WN_NOT_SUPPORTED                                             ERROR_NOT_SUPPORTED
#define WN_CANCEL                                                    ERROR_CANCELLED
#define WN_RETRY                                                     ERROR_RETRY
#define WN_NET_ERROR                                                 ERROR_UNEXP_NET_ERR
#define WN_MORE_DATA                                                 ERROR_MORE_DATA
#define WN_BAD_POINTER                                               ERROR_INVALID_ADDRESS
#define WN_BAD_VALUE                                                 ERROR_INVALID_PARAMETER
#define WN_BAD_USER                                                  ERROR_BAD_USERNAME
#define WN_BAD_PASSWORD                                              ERROR_INVALID_PASSWORD
#define WN_ACCESS_DENIED                                             ERROR_ACCESS_DENIED
#define WN_FUNCTION_BUSY                                             ERROR_BUSY
#define WN_WINDOWS_ERROR                                             ERROR_UNEXP_NET_ERR
#define WN_OUT_OF_MEMORY                                             ERROR_NOT_ENOUGH_MEMORY
#define WN_NO_NETWORK                                                ERROR_NO_NETWORK
#define WN_EXTENDED_ERROR                                            ERROR_EXTENDED_ERROR
#define WN_BAD_LEVEL                                                 ERROR_INVALID_LEVEL
#define WN_BAD_HANDLE                                                ERROR_INVALID_HANDLE
#define WN_NOT_INITIALIZING                                          ERROR_ALREADY_INITIALIZED
#define WN_NO_MORE_DEVICES                                           ERROR_NO_MORE_DEVICES
#define WN_NOT_CONNECTED                                             ERROR_NOT_CONNECTED
#define WN_OPEN_FILES                                                ERROR_OPEN_FILES
#define WN_DEVICE_IN_USE                                             ERROR_DEVICE_IN_USE
#define WN_BAD_NETNAME                                               ERROR_BAD_NET_NAME
#define WN_BAD_LOCALNAME                                             ERROR_BAD_DEVICE
#define WN_ALREADY_CONNECTED                                         ERROR_ALREADY_ASSIGNED
#define WN_DEVICE_ERROR                                              ERROR_GEN_FAILURE
#define WN_CONNECTION_CLOSED                                         ERROR_CONNECTION_UNAVAIL
#define WN_NO_NET_OR_BAD_PATH                                        ERROR_NO_NET_OR_BAD_PATH
#define WN_BAD_PROVIDER                                              ERROR_BAD_PROVIDER
#define WN_CANNOT_OPEN_PROFILE                                       ERROR_CANNOT_OPEN_PROFILE
#define WN_BAD_PROFILE                                               ERROR_BAD_PROFILE
#define WN_BAD_DEV_TYPE                                              ERROR_BAD_DEV_TYPE
#define WN_DEVICE_ALREADY_REMEMBERED                                 ERROR_DEVICE_ALREADY_REMEMBERED
#define WN_CONNECTED_OTHER_PASSWORD                                  ERROR_CONNECTED_OTHER_PASSWORD
#define WN_CONNECTED_OTHER_PASSWORD_DEFAULT                          ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT
#define WN_NO_MORE_ENTRIES                                           ERROR_NO_MORE_ITEMS
#define WN_NOT_CONTAINER                                             ERROR_NOT_CONTAINER
#define WN_NOT_AUTHENTICATED                                         ERROR_NOT_AUTHENTICATED
#define WN_NOT_LOGGED_ON                                             ERROR_NOT_LOGGED_ON
#define WN_NOT_VALIDATED                                             ERROR_NO_LOGON_SERVERS

#define WNCON_FORNETCARD                                             0x00000001
#define WNCON_NOTROUTED                                              0x00000002
#define WNCON_SLOWLINK                                               0x00000004
#define WNCON_DYNAMIC                                                0x00000008

// #define MultinetGetConnectionPerformance                             __MINGW_NAME_AW(MultinetGetConnectionPerformance)

// #endif /* WINAPI_PARTITION_DESKTOP.  */

#endif /* _WINAPI_WINNETWK_ */
