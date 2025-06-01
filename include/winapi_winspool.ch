// WINAPI For Harbour++ - Bindings libraries for Harbour++ and WINAPI
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

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

#ifndef _WINAPI_WINSPOOL_
#define _WINAPI_WINSPOOL_

// #ifdef _WINUSER_
// #endif

#define DSPRINT_PUBLISH                                              0x00000001
#define DSPRINT_UPDATE                                               0x00000002
#define DSPRINT_UNPUBLISH                                            0x00000004
#define DSPRINT_REPUBLISH                                            0x00000008
#define DSPRINT_PENDING                                              0x80000000

#define PRINTER_CONTROL_PAUSE                                        1
#define PRINTER_CONTROL_RESUME                                       2
#define PRINTER_CONTROL_PURGE                                        3
#define PRINTER_CONTROL_SET_STATUS                                   4

#define PRINTER_STATUS_PAUSED                                        0x00000001
#define PRINTER_STATUS_ERROR                                         0x00000002
#define PRINTER_STATUS_PENDING_DELETION                              0x00000004
#define PRINTER_STATUS_PAPER_JAM                                     0x00000008
#define PRINTER_STATUS_PAPER_OUT                                     0x00000010
#define PRINTER_STATUS_MANUAL_FEED                                   0x00000020
#define PRINTER_STATUS_PAPER_PROBLEM                                 0x00000040
#define PRINTER_STATUS_OFFLINE                                       0x00000080
#define PRINTER_STATUS_IO_ACTIVE                                     0x00000100
#define PRINTER_STATUS_BUSY                                          0x00000200
#define PRINTER_STATUS_PRINTING                                      0x00000400
#define PRINTER_STATUS_OUTPUT_BIN_FULL                               0x00000800
#define PRINTER_STATUS_NOT_AVAILABLE                                 0x00001000
#define PRINTER_STATUS_WAITING                                       0x00002000
#define PRINTER_STATUS_PROCESSING                                    0x00004000
#define PRINTER_STATUS_INITIALIZING                                  0x00008000
#define PRINTER_STATUS_WARMING_UP                                    0x00010000
#define PRINTER_STATUS_TONER_LOW                                     0x00020000
#define PRINTER_STATUS_NO_TONER                                      0x00040000
#define PRINTER_STATUS_PAGE_PUNT                                     0x00080000
#define PRINTER_STATUS_USER_INTERVENTION                             0x00100000
#define PRINTER_STATUS_OUT_OF_MEMORY                                 0x00200000
#define PRINTER_STATUS_DOOR_OPEN                                     0x00400000
#define PRINTER_STATUS_SERVER_UNKNOWN                                0x00800000
#define PRINTER_STATUS_POWER_SAVE                                    0x01000000

#define PRINTER_ATTRIBUTE_QUEUED                                     0x00000001
#define PRINTER_ATTRIBUTE_DIRECT                                     0x00000002
#define PRINTER_ATTRIBUTE_DEFAULT                                    0x00000004
#define PRINTER_ATTRIBUTE_SHARED                                     0x00000008
#define PRINTER_ATTRIBUTE_NETWORK                                    0x00000010
#define PRINTER_ATTRIBUTE_HIDDEN                                     0x00000020
#define PRINTER_ATTRIBUTE_LOCAL                                      0x00000040

#define PRINTER_ATTRIBUTE_ENABLE_DEVQ                                0x00000080
#define PRINTER_ATTRIBUTE_KEEPPRINTEDJOBS                            0x00000100
#define PRINTER_ATTRIBUTE_DO_COMPLETE_FIRST                          0x00000200

#define PRINTER_ATTRIBUTE_WORK_OFFLINE                               0x00000400
#define PRINTER_ATTRIBUTE_ENABLE_BIDI                                0x00000800
#define PRINTER_ATTRIBUTE_RAW_ONLY                                   0x00001000
#define PRINTER_ATTRIBUTE_PUBLISHED                                  0x00002000
#define PRINTER_ATTRIBUTE_FAX                                        0x00004000
#define PRINTER_ATTRIBUTE_TS                                         0x00008000

#define NO_PRIORITY                                                  0
#define MAX_PRIORITY                                                 99
#define MIN_PRIORITY                                                 1
#define DEF_PRIORITY                                                 1

#define JOB_CONTROL_PAUSE                                            1
#define JOB_CONTROL_RESUME                                           2
#define JOB_CONTROL_CANCEL                                           3
#define JOB_CONTROL_RESTART                                          4
#define JOB_CONTROL_DELETE                                           5
#define JOB_CONTROL_SENT_TO_PRINTER                                  6
#define JOB_CONTROL_LAST_PAGE_EJECTED                                7

#define JOB_STATUS_PAUSED                                            0x00000001
#define JOB_STATUS_ERROR                                             0x00000002
#define JOB_STATUS_DELETING                                          0x00000004
#define JOB_STATUS_SPOOLING                                          0x00000008
#define JOB_STATUS_PRINTING                                          0x00000010
#define JOB_STATUS_OFFLINE                                           0x00000020
#define JOB_STATUS_PAPEROUT                                          0x00000040
#define JOB_STATUS_PRINTED                                           0x00000080
#define JOB_STATUS_DELETED                                           0x00000100
#define JOB_STATUS_BLOCKED_DEVQ                                      0x00000200
#define JOB_STATUS_USER_INTERVENTION                                 0x00000400
#define JOB_STATUS_RESTART                                           0x00000800
#define JOB_STATUS_COMPLETE                                          0x00001000

#define JOB_POSITION_UNSPECIFIED                                     0

#define DRIVER_KERNELMODE                                            0x00000001
#define DRIVER_USERMODE                                              0x00000002

#define DPD_DELETE_UNUSED_FILES                                      0x00000001
#define DPD_DELETE_SPECIFIC_VERSION                                  0x00000002
#define DPD_DELETE_ALL_FILES                                         0x00000004

#define APD_STRICT_UPGRADE                                           0x00000001
#define APD_STRICT_DOWNGRADE                                         0x00000002
#define APD_COPY_ALL_FILES                                           0x00000004
#define APD_COPY_NEW_FILES                                           0x00000008
#define APD_COPY_FROM_DIRECTORY                                      0x00000010

#define DI_CHANNEL                                                   1
#define DI_READ_SPOOL_JOB                                            3

#define DI_MEMORYMAP_WRITE                                           0x00000001

#define FORM_USER                                                    0x00000000
#define FORM_BUILTIN                                                 0x00000001
#define FORM_PRINTER                                                 0x00000002

#define NORMAL_PRINT                                                 0x00000000
#define REVERSE_PRINT                                                0x00000001

#define PORT_TYPE_WRITE                                              0x0001
#define PORT_TYPE_READ                                               0x0002
#define PORT_TYPE_REDIRECTED                                         0x0004
#define PORT_TYPE_NET_ATTACHED                                       0x0008

#define PORT_STATUS_TYPE_ERROR                                       1
#define PORT_STATUS_TYPE_WARNING                                     2
#define PORT_STATUS_TYPE_INFO                                        3

#define PORT_STATUS_OFFLINE                                          1
#define PORT_STATUS_PAPER_JAM                                        2
#define PORT_STATUS_PAPER_OUT                                        3
#define PORT_STATUS_OUTPUT_BIN_FULL                                  4
#define PORT_STATUS_PAPER_PROBLEM                                    5
#define PORT_STATUS_NO_TONER                                         6
#define PORT_STATUS_DOOR_OPEN                                        7
#define PORT_STATUS_USER_INTERVENTION                                8
#define PORT_STATUS_OUT_OF_MEMORY                                    9
#define PORT_STATUS_TONER_LOW                                        10
#define PORT_STATUS_WARMING_UP                                       11
#define PORT_STATUS_POWER_SAVE                                       12

// #define EnumPrinters                                                 __MINGW_NAME_AW(EnumPrinters)

#define PRINTER_ENUM_DEFAULT                                         0x00000001
#define PRINTER_ENUM_LOCAL                                           0x00000002
#define PRINTER_ENUM_CONNECTIONS                                     0x00000004
#define PRINTER_ENUM_FAVORITE                                        0x00000004
#define PRINTER_ENUM_NAME                                            0x00000008
#define PRINTER_ENUM_REMOTE                                          0x00000010
#define PRINTER_ENUM_SHARED                                          0x00000020
#define PRINTER_ENUM_NETWORK                                         0x00000040

#define PRINTER_ENUM_EXPAND                                          0x00004000
#define PRINTER_ENUM_CONTAINER                                       0x00008000

#define PRINTER_ENUM_ICONMASK                                        0x00ff0000
#define PRINTER_ENUM_ICON1                                           0x00010000
#define PRINTER_ENUM_ICON2                                           0x00020000
#define PRINTER_ENUM_ICON3                                           0x00040000
#define PRINTER_ENUM_ICON4                                           0x00080000
#define PRINTER_ENUM_ICON5                                           0x00100000
#define PRINTER_ENUM_ICON6                                           0x00200000
#define PRINTER_ENUM_ICON7                                           0x00400000
#define PRINTER_ENUM_ICON8                                           0x00800000
#define PRINTER_ENUM_HIDE                                            0x01000000

#define SPOOL_FILE_PERSISTENT                                        0x00000001
#define SPOOL_FILE_TEMPORARY                                         0x00000002

// #define OpenPrinter                                                  __MINGW_NAME_AW(OpenPrinter)
// #define ResetPrinter                                                 __MINGW_NAME_AW(ResetPrinter)
// #define SetJob                                                       __MINGW_NAME_AW(SetJob)
// #define GetJob                                                       __MINGW_NAME_AW(GetJob)
// #define EnumJobs                                                     __MINGW_NAME_AW(EnumJobs)
// #define AddPrinter                                                   __MINGW_NAME_AW(AddPrinter)
// #define SetPrinter                                                   __MINGW_NAME_AW(SetPrinter)
// #define GetPrinter                                                   __MINGW_NAME_AW(GetPrinter)
// #define AddPrinterDriver                                             __MINGW_NAME_AW(AddPrinterDriver)
// #define AddPrinterDriverEx                                           __MINGW_NAME_AW(AddPrinterDriverEx)
// #define EnumPrinterDrivers                                           __MINGW_NAME_AW(EnumPrinterDrivers)
// #define GetPrinterDriver                                             __MINGW_NAME_AW(GetPrinterDriver)
// #define GetPrinterDriverDirectory                                    __MINGW_NAME_AW(GetPrinterDriverDirectory)
// #define DeletePrinterDriver                                          __MINGW_NAME_AW(DeletePrinterDriver)
// #define DeletePrinterDriverEx                                        __MINGW_NAME_AW(DeletePrinterDriverEx)
// #define AddPrintProcessor                                            __MINGW_NAME_AW(AddPrintProcessor)
// #define EnumPrintProcessors                                          __MINGW_NAME_AW(EnumPrintProcessors)
// #define GetPrintProcessorDirectory                                   __MINGW_NAME_AW(GetPrintProcessorDirectory)
// #define EnumPrintProcessorDatatypes                                  __MINGW_NAME_AW(EnumPrintProcessorDatatypes)
// #define DeletePrintProcessor                                         __MINGW_NAME_AW(DeletePrintProcessor)
// #define StartDocPrinter                                              __MINGW_NAME_AW(StartDocPrinter)
// #define AddJob                                                       __MINGW_NAME_AW(AddJob)
// #define DocumentProperties                                           __MINGW_NAME_AW(DocumentProperties)
// #define AdvancedDocumentProperties                                   __MINGW_NAME_AW(AdvancedDocumentProperties)
// #define GetPrinterData                                               __MINGW_NAME_AW(GetPrinterData)
// #define GetPrinterDataEx                                             __MINGW_NAME_AW(GetPrinterDataEx)
// #define EnumPrinterData                                              __MINGW_NAME_AW(EnumPrinterData)
// #define EnumPrinterDataEx                                            __MINGW_NAME_AW(EnumPrinterDataEx)
// #define EnumPrinterKey                                               __MINGW_NAME_AW(EnumPrinterKey)
// #define SetPrinterData                                               __MINGW_NAME_AW(SetPrinterData)
// #define SetPrinterDataEx                                             __MINGW_NAME_AW(SetPrinterDataEx)
// #define DeletePrinterData                                            __MINGW_NAME_AW(DeletePrinterData)
// #define DeletePrinterDataEx                                          __MINGW_NAME_AW(DeletePrinterDataEx)
// #define DeletePrinterKey                                             __MINGW_NAME_AW(DeletePrinterKey)

#define PRINTER_NOTIFY_TYPE                                          0x00
#define JOB_NOTIFY_TYPE                                              0x01

#define PRINTER_NOTIFY_FIELD_SERVER_NAME                             0x00
#define PRINTER_NOTIFY_FIELD_PRINTER_NAME                            0x01
#define PRINTER_NOTIFY_FIELD_SHARE_NAME                              0x02
#define PRINTER_NOTIFY_FIELD_PORT_NAME                               0x03
#define PRINTER_NOTIFY_FIELD_DRIVER_NAME                             0x04
#define PRINTER_NOTIFY_FIELD_COMMENT                                 0x05
#define PRINTER_NOTIFY_FIELD_LOCATION                                0x06
#define PRINTER_NOTIFY_FIELD_DEVMODE                                 0x07
#define PRINTER_NOTIFY_FIELD_SEPFILE                                 0x08
#define PRINTER_NOTIFY_FIELD_PRINT_PROCESSOR                         0x09
#define PRINTER_NOTIFY_FIELD_PARAMETERS                              0x0A
#define PRINTER_NOTIFY_FIELD_DATATYPE                                0x0B
#define PRINTER_NOTIFY_FIELD_SECURITY_DESCRIPTOR                     0x0C
#define PRINTER_NOTIFY_FIELD_ATTRIBUTES                              0x0D
#define PRINTER_NOTIFY_FIELD_PRIORITY                                0x0E
#define PRINTER_NOTIFY_FIELD_DEFAULT_PRIORITY                        0x0F
#define PRINTER_NOTIFY_FIELD_START_TIME                              0x10
#define PRINTER_NOTIFY_FIELD_UNTIL_TIME                              0x11
#define PRINTER_NOTIFY_FIELD_STATUS                                  0x12
#define PRINTER_NOTIFY_FIELD_STATUS_STRING                           0x13
#define PRINTER_NOTIFY_FIELD_CJOBS                                   0x14
#define PRINTER_NOTIFY_FIELD_AVERAGE_PPM                             0x15
#define PRINTER_NOTIFY_FIELD_TOTAL_PAGES                             0x16
#define PRINTER_NOTIFY_FIELD_PAGES_PRINTED                           0x17
#define PRINTER_NOTIFY_FIELD_TOTAL_BYTES                             0x18
#define PRINTER_NOTIFY_FIELD_BYTES_PRINTED                           0x19
#define PRINTER_NOTIFY_FIELD_OBJECT_GUID                             0x1A

#define JOB_NOTIFY_FIELD_PRINTER_NAME                                0x00
#define JOB_NOTIFY_FIELD_MACHINE_NAME                                0x01
#define JOB_NOTIFY_FIELD_PORT_NAME                                   0x02
#define JOB_NOTIFY_FIELD_USER_NAME                                   0x03
#define JOB_NOTIFY_FIELD_NOTIFY_NAME                                 0x04
#define JOB_NOTIFY_FIELD_DATATYPE                                    0x05
#define JOB_NOTIFY_FIELD_PRINT_PROCESSOR                             0x06
#define JOB_NOTIFY_FIELD_PARAMETERS                                  0x07
#define JOB_NOTIFY_FIELD_DRIVER_NAME                                 0x08
#define JOB_NOTIFY_FIELD_DEVMODE                                     0x09
#define JOB_NOTIFY_FIELD_STATUS                                      0x0A
#define JOB_NOTIFY_FIELD_STATUS_STRING                               0x0B
#define JOB_NOTIFY_FIELD_SECURITY_DESCRIPTOR                         0x0C
#define JOB_NOTIFY_FIELD_DOCUMENT                                    0x0D
#define JOB_NOTIFY_FIELD_PRIORITY                                    0x0E
#define JOB_NOTIFY_FIELD_POSITION                                    0x0F
#define JOB_NOTIFY_FIELD_SUBMITTED                                   0x10
#define JOB_NOTIFY_FIELD_START_TIME                                  0x11
#define JOB_NOTIFY_FIELD_UNTIL_TIME                                  0x12
#define JOB_NOTIFY_FIELD_TIME                                        0x13
#define JOB_NOTIFY_FIELD_TOTAL_PAGES                                 0x14
#define JOB_NOTIFY_FIELD_PAGES_PRINTED                               0x15
#define JOB_NOTIFY_FIELD_TOTAL_BYTES                                 0x16
#define JOB_NOTIFY_FIELD_BYTES_PRINTED                               0x17

#define PRINTER_NOTIFY_OPTIONS_REFRESH                               0x01

#define PRINTER_NOTIFY_INFO_DISCARDED                                0x01

// #define BIDI_ACTION_ENUM_SCHEMA                                      L"EnumSchema"
// #define BIDI_ACTION_GET                                              L"Get"
// #define BIDI_ACTION_SET                                              L"Set"
// #define BIDI_ACTION_GET_ALL                                          L"GetAll"

#define BIDI_ACCESS_ADMINISTRATOR                                    0x1
#define BIDI_ACCESS_USER                                             0x2

#define ERROR_BIDI_STATUS_OK                                         0
#define ERROR_BIDI_NOT_SUPPORTED                                     ERROR_NOT_SUPPORTED

#define ERROR_BIDI_ERROR_BASE                                        13000
#define ERROR_BIDI_STATUS_WARNING                                    (ERROR_BIDI_ERROR_BASE+1)
#define ERROR_BIDI_SCHEMA_READ_ONLY                                  (ERROR_BIDI_ERROR_BASE+2)
#define ERROR_BIDI_SERVER_OFFLINE                                    (ERROR_BIDI_ERROR_BASE+3)
#define ERROR_BIDI_DEVICE_OFFLINE                                    (ERROR_BIDI_ERROR_BASE+4)
#define ERROR_BIDI_SCHEMA_NOT_SUPPORTED                              (ERROR_BIDI_ERROR_BASE+5)

#define PRINTER_CHANGE_ADD_PRINTER                                   0x00000001
#define PRINTER_CHANGE_SET_PRINTER                                   0x00000002
#define PRINTER_CHANGE_DELETE_PRINTER                                0x00000004
#define PRINTER_CHANGE_FAILED_CONNECTION_PRINTER                     0x00000008
#define PRINTER_CHANGE_PRINTER                                       0x000000FF
#define PRINTER_CHANGE_ADD_JOB                                       0x00000100
#define PRINTER_CHANGE_SET_JOB                                       0x00000200
#define PRINTER_CHANGE_DELETE_JOB                                    0x00000400
#define PRINTER_CHANGE_WRITE_JOB                                     0x00000800
#define PRINTER_CHANGE_JOB                                           0x0000FF00
#define PRINTER_CHANGE_ADD_FORM                                      0x00010000
#define PRINTER_CHANGE_SET_FORM                                      0x00020000
#define PRINTER_CHANGE_DELETE_FORM                                   0x00040000
#define PRINTER_CHANGE_FORM                                          0x00070000
#define PRINTER_CHANGE_ADD_PORT                                      0x00100000
#define PRINTER_CHANGE_CONFIGURE_PORT                                0x00200000
#define PRINTER_CHANGE_DELETE_PORT                                   0x00400000
#define PRINTER_CHANGE_PORT                                          0x00700000
#define PRINTER_CHANGE_ADD_PRINT_PROCESSOR                           0x01000000
#define PRINTER_CHANGE_DELETE_PRINT_PROCESSOR                        0x04000000
#define PRINTER_CHANGE_PRINT_PROCESSOR                               0x07000000
#define PRINTER_CHANGE_ADD_PRINTER_DRIVER                            0x10000000
#define PRINTER_CHANGE_SET_PRINTER_DRIVER                            0x20000000
#define PRINTER_CHANGE_DELETE_PRINTER_DRIVER                         0x40000000
#define PRINTER_CHANGE_PRINTER_DRIVER                                0x70000000
#define PRINTER_CHANGE_TIMEOUT                                       0x80000000
#define PRINTER_CHANGE_ALL                                           0x7777FFFF

// #define PrinterMessageBox                                            __MINGW_NAME_AW(PrinterMessageBox)
// #define AddForm                                                      __MINGW_NAME_AW(AddForm)
// #define DeleteForm                                                   __MINGW_NAME_AW(DeleteForm)
// #define GetForm                                                      __MINGW_NAME_AW(GetForm)
// #define SetForm                                                      __MINGW_NAME_AW(SetForm)
// #define EnumForms                                                    __MINGW_NAME_AW(EnumForms)
// #define EnumMonitors                                                 __MINGW_NAME_AW(EnumMonitors)
// #define AddMonitor                                                   __MINGW_NAME_AW(AddMonitor)
// #define DeleteMonitor                                                __MINGW_NAME_AW(DeleteMonitor)
// #define EnumPorts                                                    __MINGW_NAME_AW(EnumPorts)
// #define AddPort                                                      __MINGW_NAME_AW(AddPort)
// #define ConfigurePort                                                __MINGW_NAME_AW(ConfigurePort)
// #define DeletePort                                                   __MINGW_NAME_AW(DeletePort)
// #define GetDefaultPrinter                                            __MINGW_NAME_AW(GetDefaultPrinter)
// #define SetDefaultPrinter                                            __MINGW_NAME_AW(SetDefaultPrinter)
// #define SetPort                                                      __MINGW_NAME_AW(SetPort)
// #define AddPrinterConnection                                         __MINGW_NAME_AW(AddPrinterConnection)
// #define DeletePrinterConnection                                      __MINGW_NAME_AW(DeletePrinterConnection)

#define PRINTER_ERROR_INFORMATION                                    0x80000000
#define PRINTER_ERROR_WARNING                                        0x40000000
#define PRINTER_ERROR_SEVERE                                         0x20000000

#define PRINTER_ERROR_OUTOFPAPER                                     0x00000001
#define PRINTER_ERROR_JAM                                            0x00000002
#define PRINTER_ERROR_OUTOFTONER                                     0x00000004

// #define XcvData                                                      XcvDataW

// #define AddPrintProvidor                                             __MINGW_NAME_AW(AddPrintProvidor)
// #define DeletePrintProvidor                                          __MINGW_NAME_AW(DeletePrintProvidor)
// #define IsValidDevmode                                               __MINGW_NAME_AW(IsValidDevmode)

// #define SPLREG_DEFAULT_SPOOL_DIRECTORY                               TEXT("DefaultSpoolDirectory")
// #define SPLREG_PORT_THREAD_PRIORITY_DEFAULT                          TEXT("PortThreadPriorityDefault")
// #define SPLREG_PORT_THREAD_PRIORITY                                  TEXT("PortThreadPriority")
// #define SPLREG_SCHEDULER_THREAD_PRIORITY_DEFAULT                     TEXT("SchedulerThreadPriorityDefault")
// #define SPLREG_SCHEDULER_THREAD_PRIORITY                             TEXT("SchedulerThreadPriority")
// #define SPLREG_BEEP_ENABLED                                          TEXT("BeepEnabled")
// #define SPLREG_NET_POPUP                                             TEXT("NetPopup")
// #define SPLREG_RETRY_POPUP                                           TEXT("RetryPopup")
// #define SPLREG_NET_POPUP_TO_COMPUTER                                 TEXT("NetPopupToComputer")
// #define SPLREG_EVENT_LOG                                             TEXT("EventLog")
// #define SPLREG_MAJOR_VERSION                                         TEXT("MajorVersion")
// #define SPLREG_MINOR_VERSION                                         TEXT("MinorVersion")
// #define SPLREG_ARCHITECTURE                                          TEXT("Architecture")
// #define SPLREG_OS_VERSION                                            TEXT("OSVersion")
// #define SPLREG_OS_VERSIONEX                                          TEXT("OSVersionEx")
// #define SPLREG_DS_PRESENT                                            TEXT("DsPresent")
// #define SPLREG_DS_PRESENT_FOR_USER                                   TEXT("DsPresentForUser")
// #define SPLREG_REMOTE_FAX                                            TEXT("RemoteFax")
// #define SPLREG_RESTART_JOB_ON_POOL_ERROR                             TEXT("RestartJobOnPoolError")
// #define SPLREG_RESTART_JOB_ON_POOL_ENABLED                           TEXT("RestartJobOnPoolEnabled")
// #define SPLREG_DNS_MACHINE_NAME                                      TEXT("DNSMachineName")
// #define SPLREG_ALLOW_USER_MANAGEFORMS                                TEXT("AllowUserManageForms")
// #define SPLREG_WEBSHAREMGMT                                          TEXT("WebShareMgmt")

#define SERVER_ACCESS_ADMINISTER                                     0x00000001
#define SERVER_ACCESS_ENUMERATE                                      0x00000002

#define PRINTER_ACCESS_ADMINISTER                                    0x00000004
#define PRINTER_ACCESS_USE                                           0x00000008

#define JOB_ACCESS_ADMINISTER                                        0x00000010
#define JOB_ACCESS_READ                                              0x00000020

#define SERVER_ALL_ACCESS                                            hb_bitor(STANDARD_RIGHTS_REQUIRED, SERVER_ACCESS_ADMINISTER, SERVER_ACCESS_ENUMERATE)
#define SERVER_READ                                                  hb_bitor(STANDARD_RIGHTS_READ, SERVER_ACCESS_ENUMERATE)
#define SERVER_WRITE                                                 hb_bitor(STANDARD_RIGHTS_WRITE, SERVER_ACCESS_ADMINISTER, SERVER_ACCESS_ENUMERATE)
#define SERVER_EXECUTE                                               hb_bitor(STANDARD_RIGHTS_EXECUTE, SERVER_ACCESS_ENUMERATE)
#define PRINTER_ALL_ACCESS                                           hb_bitor(STANDARD_RIGHTS_REQUIRED, PRINTER_ACCESS_ADMINISTER, PRINTER_ACCESS_USE)
#define PRINTER_READ                                                 hb_bitor(STANDARD_RIGHTS_READ, PRINTER_ACCESS_USE)
#define PRINTER_WRITE                                                hb_bitor(STANDARD_RIGHTS_WRITE, PRINTER_ACCESS_USE)
#define PRINTER_EXECUTE                                              hb_bitor(STANDARD_RIGHTS_EXECUTE, PRINTER_ACCESS_USE)
#define JOB_ALL_ACCESS                                               hb_bitor(STANDARD_RIGHTS_REQUIRED, JOB_ACCESS_ADMINISTER, JOB_ACCESS_READ)
#define JOB_READ                                                     hb_bitor(STANDARD_RIGHTS_READ, JOB_ACCESS_READ)
#define JOB_WRITE                                                    hb_bitor(STANDARD_RIGHTS_WRITE, JOB_ACCESS_ADMINISTER)
#define JOB_EXECUTE                                                  hb_bitor(STANDARD_RIGHTS_EXECUTE, JOB_ACCESS_ADMINISTER)

// #define SPLDS_SPOOLER_KEY                                            TEXT("DsSpooler")
// #define SPLDS_DRIVER_KEY                                             TEXT("DsDriver")
// #define SPLDS_USER_KEY                                               TEXT("DsUser")

// #define SPLDS_ASSET_NUMBER                                           TEXT("assetNumber")
// #define SPLDS_BYTES_PER_MINUTE                                       TEXT("bytesPerMinute")
// #define SPLDS_DESCRIPTION                                            TEXT("description")
// #define SPLDS_DRIVER_NAME                                            TEXT("driverName")
// #define SPLDS_DRIVER_VERSION                                         TEXT("driverVersion")
// #define SPLDS_LOCATION                                               TEXT("location")
// #define SPLDS_PORT_NAME                                              TEXT("portName")
// #define SPLDS_PRINT_ATTRIBUTES                                       TEXT("printAttributes")
// #define SPLDS_PRINT_BIN_NAMES                                        TEXT("printBinNames")
// #define SPLDS_PRINT_COLLATE                                          TEXT("printCollate")
// #define SPLDS_PRINT_COLOR                                            TEXT("printColor")
// #define SPLDS_PRINT_DUPLEX_SUPPORTED                                 TEXT("printDuplexSupported")
// #define SPLDS_PRINT_END_TIME                                         TEXT("printEndTime")
// #define SPLDS_PRINTER_CLASS                                          TEXT("printQueue")
// #define SPLDS_PRINTER_NAME                                           TEXT("printerName")
// #define SPLDS_PRINT_KEEP_PRINTED_JOBS                                TEXT("printKeepPrintedJobs")
// #define SPLDS_PRINT_LANGUAGE                                         TEXT("printLanguage")
// #define SPLDS_PRINT_MAC_ADDRESS                                      TEXT("printMACAddress")
// #define SPLDS_PRINT_MAX_X_EXTENT                                     TEXT("printMaxXExtent")
// #define SPLDS_PRINT_MAX_Y_EXTENT                                     TEXT("printMaxYExtent")
// #define SPLDS_PRINT_MAX_RESOLUTION_SUPPORTED                         TEXT("printMaxResolutionSupported")
// #define SPLDS_PRINT_MEDIA_READY                                      TEXT("printMediaReady")
// #define SPLDS_PRINT_MEDIA_SUPPORTED                                  TEXT("printMediaSupported")
// #define SPLDS_PRINT_MEMORY                                           TEXT("printMemory")
// #define SPLDS_PRINT_MIN_X_EXTENT                                     TEXT("printMinXExtent")
// #define SPLDS_PRINT_MIN_Y_EXTENT                                     TEXT("printMinYExtent")
// #define SPLDS_PRINT_NETWORK_ADDRESS                                  TEXT("printNetworkAddress")
// #define SPLDS_PRINT_NOTIFY                                           TEXT("printNotify")
// #define SPLDS_PRINT_NUMBER_UP                                        TEXT("printNumberUp")
// #define SPLDS_PRINT_ORIENTATIONS_SUPPORTED                           TEXT("printOrientationsSupported")
// #define SPLDS_PRINT_OWNER                                            TEXT("printOwner")
// #define SPLDS_PRINT_PAGES_PER_MINUTE                                 TEXT("printPagesPerMinute")
// #define SPLDS_PRINT_RATE                                             TEXT("printRate")
// #define SPLDS_PRINT_RATE_UNIT                                        TEXT("printRateUnit")
// #define SPLDS_PRINT_SEPARATOR_FILE                                   TEXT("printSeparatorFile")
// #define SPLDS_PRINT_SHARE_NAME                                       TEXT("printShareName")
// #define SPLDS_PRINT_SPOOLING                                         TEXT("printSpooling")
// #define SPLDS_PRINT_STAPLING_SUPPORTED                               TEXT("printStaplingSupported")
// #define SPLDS_PRINT_START_TIME                                       TEXT("printStartTime")
// #define SPLDS_PRINT_STATUS                                           TEXT("printStatus")
// #define SPLDS_PRIORITY                                               TEXT("priority")
// #define SPLDS_SERVER_NAME                                            TEXT("serverName")
// #define SPLDS_SHORT_SERVER_NAME                                      TEXT("shortServerName")
// #define SPLDS_UNC_NAME                                               TEXT("uNCName")
// #define SPLDS_URL                                                    TEXT("url")
// #define SPLDS_FLAGS                                                  TEXT("flags")
// #define SPLDS_VERSION_NUMBER                                         TEXT("versionNumber")

// #define SPLDS_PRINTER_NAME_ALIASES                                   TEXT("printerNameAliases")
// #define SPLDS_PRINTER_LOCATIONS                                      TEXT("printerLocations")
// #define SPLDS_PRINTER_MODEL                                          TEXT("printerModel")

// #if (_WIN32_WINNT >= 0x0600)
// #define AddPrinterConnection2                                        __MINGW_NAME_AW(AddPrinterConnection2)
// #define DeletePrinterDriverPackage                                   __MINGW_NAME_AW(DeletePrinterDriverPackage)
// #define DocumentEvent                                                __MINGW_NAME_AW(DocumentEvent)

#define PRINTER_CONNECTION_MISMATCH                                  0x00000020
#define PRINTER_CONNECTION_NO_UI                                     0x00000040

// #define GetCorePrinterDrivers                                        __MINGW_NAME_AW(GetCorePrinterDrivers)
// #define GetPrinterDriver2                                            __MINGW_NAME_AW(GetPrinterDriver2)
// #define GetPrinterDriverPackagePath                                  __MINGW_NAME_AW(GetPrinterDriverPackagePath)
// #define GetSpoolFileHandle                                           __MINGW_NAME_AW(GetSpoolFileHandle)

// #define OpenPrinter2                                                 __MINGW_NAME_AW(OpenPrinter2)

// #define UploadPrinterDriverPackage                                   __MINGW_NAME_AW(UploadPrinterDriverPackage)

// #endif //(_WIN32_WINNT >= 0x0600)

#endif // _WINAPI_WINSPOOL_
