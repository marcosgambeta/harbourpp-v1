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

#ifndef _WINAPI_WINREG_
#define _WINAPI_WINREG_

// #ifndef WINVER
// #define WINVER                                                       0x0502
// #endif

#define RRF_RT_REG_NONE                                              0x00000001
#define RRF_RT_REG_SZ                                                0x00000002
#define RRF_RT_REG_EXPAND_SZ                                         0x00000004
#define RRF_RT_REG_BINARY                                            0x00000008
#define RRF_RT_REG_DWORD                                             0x00000010
#define RRF_RT_REG_MULTI_SZ                                          0x00000020
#define RRF_RT_REG_QWORD                                             0x00000040

#define RRF_RT_DWORD                                                 hb_bitor(RRF_RT_REG_BINARY, RRF_RT_REG_DWORD)
#define RRF_RT_QWORD                                                 hb_bitor(RRF_RT_REG_BINARY, RRF_RT_REG_QWORD)
#define RRF_RT_ANY                                                   0x0000ffff

#define RRF_NOEXPAND                                                 0x10000000
#define RRF_ZEROONFAILURE                                            0x20000000

// #define HKEY_CLASSES_ROOT                                            ((HKEY)(ULONG_PTR)((LONG)0x80000000))
// #define HKEY_CURRENT_USER                                            ((HKEY)(ULONG_PTR)((LONG)0x80000001))
// #define HKEY_LOCAL_MACHINE                                           ((HKEY)(ULONG_PTR)((LONG)0x80000002))
// #define HKEY_USERS                                                   ((HKEY)(ULONG_PTR)((LONG)0x80000003))
// #define HKEY_PERFORMANCE_DATA                                        ((HKEY)(ULONG_PTR)((LONG)0x80000004))
// #define HKEY_PERFORMANCE_TEXT                                        ((HKEY)(ULONG_PTR)((LONG)0x80000050))
// #define HKEY_PERFORMANCE_NLSTEXT                                     ((HKEY)(ULONG_PTR)((LONG)0x80000060))
// #define HKEY_CURRENT_CONFIG                                          ((HKEY)(ULONG_PTR)((LONG)0x80000005))
// #define HKEY_DYN_DATA                                                ((HKEY)(ULONG_PTR)((LONG)0x80000006))

#define REG_SECURE_CONNECTION                                        1

// #ifndef _PROVIDER_STRUCTS_DEFINED
// #define _PROVIDER_STRUCTS_DEFINED
#define PROVIDER_KEEPS_VALUE_LENGTH                                  0x1
// #endif

// #define WIN31_CLASS                                                  NULL

// #define RegConnectRegistry                                           __MINGW_NAME_AW(RegConnectRegistry)
// #define RegConnectRegistryEx                                         __MINGW_NAME_AW(RegConnectRegistryEx)
// #define RegCreateKey                                                 __MINGW_NAME_AW(RegCreateKey)
// #define RegCreateKeyEx                                               __MINGW_NAME_AW(RegCreateKeyEx)
// #define RegDeleteKey                                                 __MINGW_NAME_AW(RegDeleteKey)
// #define RegDeleteKeyEx                                               __MINGW_NAME_AW(RegDeleteKeyEx)
// #define RegDeleteValue                                               __MINGW_NAME_AW(RegDeleteValue)
// #define RegEnumKey                                                   __MINGW_NAME_AW(RegEnumKey)
// #define RegEnumKeyEx                                                 __MINGW_NAME_AW(RegEnumKeyEx)
// #define RegEnumValue                                                 __MINGW_NAME_AW(RegEnumValue)
// #define RegLoadKey                                                   __MINGW_NAME_AW(RegLoadKey)
// #define RegOpenKey                                                   __MINGW_NAME_AW(RegOpenKey)
// #define RegOpenKeyEx                                                 __MINGW_NAME_AW(RegOpenKeyEx)
// #define RegQueryInfoKey                                              __MINGW_NAME_AW(RegQueryInfoKey)
// #define RegQueryValue                                                __MINGW_NAME_AW(RegQueryValue)
// #define RegQueryMultipleValues                                       __MINGW_NAME_AW(RegQueryMultipleValues)
// #define RegQueryValueEx                                              __MINGW_NAME_AW(RegQueryValueEx)
// #define RegReplaceKey                                                __MINGW_NAME_AW(RegReplaceKey)
// #define RegRestoreKey                                                __MINGW_NAME_AW(RegRestoreKey)
// #define RegSaveKey                                                   __MINGW_NAME_AW(RegSaveKey)
// #define RegSetValue                                                  __MINGW_NAME_AW(RegSetValue)
// #define RegSetValueEx                                                __MINGW_NAME_AW(RegSetValueEx)
// #define RegUnLoadKey                                                 __MINGW_NAME_AW(RegUnLoadKey)
// #define RegGetValue                                                  __MINGW_NAME_AW(RegGetValue)
// #define InitiateSystemShutdown                                       __MINGW_NAME_AW(InitiateSystemShutdown)
// #define AbortSystemShutdown                                          __MINGW_NAME_AW(AbortSystemShutdown)

#define REASON_SWINSTALL                                             hb_bitor(SHTDN_REASON_MAJOR_SOFTWARE, SHTDN_REASON_MINOR_INSTALLATION)
#define REASON_HWINSTALL                                             hb_bitor(SHTDN_REASON_MAJOR_HARDWARE, SHTDN_REASON_MINOR_INSTALLATION)
#define REASON_SERVICEHANG                                           hb_bitor(SHTDN_REASON_MAJOR_SOFTWARE, SHTDN_REASON_MINOR_HUNG)
#define REASON_UNSTABLE                                              hb_bitor(SHTDN_REASON_MAJOR_SYSTEM, SHTDN_REASON_MINOR_UNSTABLE)
#define REASON_SWHWRECONF                                            hb_bitor(SHTDN_REASON_MAJOR_SOFTWARE, SHTDN_REASON_MINOR_RECONFIG)
#define REASON_OTHER                                                 hb_bitor(SHTDN_REASON_MAJOR_OTHER, SHTDN_REASON_MINOR_OTHER)
#define REASON_UNKNOWN                                               SHTDN_REASON_UNKNOWN
#define REASON_LEGACY_API                                            SHTDN_REASON_LEGACY_API
#define REASON_PLANNED_FLAG                                          SHTDN_REASON_FLAG_PLANNED

#define MAX_SHUTDOWN_TIMEOUT                                         (10*365*24*60*60)

// #define InitiateSystemShutdownEx                                     __MINGW_NAME_AW(InitiateSystemShutdownEx)
// #define RegSaveKeyEx                                                 __MINGW_NAME_AW(RegSaveKeyEx)

// #if (_WIN32_WINNT >= 0x0600)
// #define RegCopyTree                                                  __MINGW_NAME_AW(RegCopyTree)
// #define RegCreateKeyTransacted                                       __MINGW_NAME_AW(RegCreateKeyTransacted)
// #define RegDeleteKeyTransacted                                       __MINGW_NAME_AW(RegDeleteKeyTransacted)
// #define RegDeleteKeyValue                                            __MINGW_NAME_AW(RegDeleteKeyValue)
// #define RegDeleteTree                                                __MINGW_NAME_AW(RegDeleteTree)
// #define RegLoadAppKey                                                __MINGW_NAME_AW(RegLoadAppKey)
// #define RegLoadMUIString                                             __MINGW_NAME_AW(RegLoadMUIString)
// #define RegOpenKeyTransacted                                         __MINGW_NAME_AW(RegOpenKeyTransacted)
// #define RegSetKeyValue                                               __MINGW_NAME_AW(RegSetKeyValue)

#define SHUTDOWN_FORCE_OTHERS                                        0x00000001
#define SHUTDOWN_FORCE_SELF                                          0x00000002
#define SHUTDOWN_RESTART                                             0x00000004
#define SHUTDOWN_POWEROFF                                            0x00000008
#define SHUTDOWN_NOREBOOT                                            0x00000010
#define SHUTDOWN_GRACE_OVERRIDE                                      0x00000020
#define SHUTDOWN_INSTALL_UPDATES                                     0x00000040
#define SHUTDOWN_RESTARTAPPS                                         0x00000080
#define SHUTDOWN_HYBRID                                              0x00000200

// #define InitiateShutdown                                             __MINGW_NAME_AW(InitiateShutdown)

// #endif /* (_WIN32_WINNT >= 0x0600) */

#endif /* _WINAPI_WINREG_ */
