//
// WINAPI For Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
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

#ifndef _WINAPI_WINBASE_
#define _WINAPI_WINBASE_

// #ifdef __WIDL__
// #define NOWINBASEINTERLOCK                                           1
// #endif

// #ifndef NOWINBASEINTERLOCK
// #define __INTRINSIC_GROUP_WINBASE                                    /*onlydefinetheintrinsicsinthisfile*/
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetCurrentTime()                                             GetTickCount()
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
// #define DefineHandleTable(w)                                         ({(VOID)(w);TRUE;})
// #define LimitEmsPages(dw)
// #define SetSwapAreaSize(w)                                           (w)
// #define LockSegment(w)                                               GlobalFix((HANDLE)(w))
// #define UnlockSegment(w)                                             GlobalUnfix((HANDLE)(w))

// #define Yield()

#define FILE_BEGIN                                                   0
#define FILE_CURRENT                                                 1
#define FILE_END                                                     2

#define WAIT_FAILED                                                  (0xffffffff)
#define WAIT_OBJECT_0                                                ((STATUS_WAIT_0)+0)

#define WAIT_ABANDONED                                               ((STATUS_ABANDONED_WAIT_0)+0)
#define WAIT_ABANDONED_0                                             ((STATUS_ABANDONED_WAIT_0)+0)

#define WAIT_IO_COMPLETION                                           STATUS_USER_APC

// #define SecureZeroMemory                                             RtlSecureZeroMemory
// #define CaptureStackBackTrace                                        RtlCaptureStackBackTrace

#define FILE_FLAG_WRITE_THROUGH                                      0x80000000
#define FILE_FLAG_OVERLAPPED                                         0x40000000
#define FILE_FLAG_NO_BUFFERING                                       0x20000000
#define FILE_FLAG_RANDOM_ACCESS                                      0x10000000
#define FILE_FLAG_SEQUENTIAL_SCAN                                    0x8000000
#define FILE_FLAG_DELETE_ON_CLOSE                                    0x4000000
#define FILE_FLAG_BACKUP_SEMANTICS                                   0x2000000
#define FILE_FLAG_POSIX_SEMANTICS                                    0x1000000
#define FILE_FLAG_SESSION_AWARE                                      0x800000
#define FILE_FLAG_OPEN_REPARSE_POINT                                 0x200000
#define FILE_FLAG_OPEN_NO_RECALL                                     0x100000
#define FILE_FLAG_FIRST_PIPE_INSTANCE                                0x80000
// #if _WIN32_WINNT >= 0x0602
#define FILE_FLAG_OPEN_REQUIRING_OPLOCK                              0x40000
// #endif

#define PROGRESS_CONTINUE                                            0
#define PROGRESS_CANCEL                                              1
#define PROGRESS_STOP                                                2
#define PROGRESS_QUIET                                               3

#define CALLBACK_CHUNK_FINISHED                                      0x0
#define CALLBACK_STREAM_SWITCH                                       0x1

#define COPY_FILE_FAIL_IF_EXISTS                                     0x1
#define COPY_FILE_RESTARTABLE                                        0x2
#define COPY_FILE_OPEN_SOURCE_FOR_WRITE                              0x4
#define COPY_FILE_ALLOW_DECRYPTED_DESTINATION                        0x8
// #if _WIN32_WINNT >= 0x0600
#define COPY_FILE_COPY_SYMLINK                                       0x800
#define COPY_FILE_NO_BUFFERING                                       0x1000
// #endif
// #if _WIN32_WINNT >= 0x0602
#define COPY_FILE_REQUEST_SECURITY_PRIVILEGES                        0x2000
#define COPY_FILE_RESUME_FROM_PAUSE                                  0x4000
#define COPY_FILE_NO_OFFLOAD                                         0x40000
// #endif

#define REPLACEFILE_WRITE_THROUGH                                    0x1
#define REPLACEFILE_IGNORE_MERGE_ERRORS                              0x2
// #if _WIN32_WINNT >= 0x0600
#define REPLACEFILE_IGNORE_ACL_ERRORS                                0x4
// #endif

#define PIPE_ACCESS_INBOUND                                          0x1
#define PIPE_ACCESS_OUTBOUND                                         0x2
#define PIPE_ACCESS_DUPLEX                                           0x3

#define PIPE_CLIENT_END                                              0x0
#define PIPE_SERVER_END                                              0x1

#define PIPE_WAIT                                                    0x0
#define PIPE_NOWAIT                                                  0x1
#define PIPE_READMODE_BYTE                                           0x0
#define PIPE_READMODE_MESSAGE                                        0x2
#define PIPE_TYPE_BYTE                                               0x0
#define PIPE_TYPE_MESSAGE                                            0x4
#define PIPE_ACCEPT_REMOTE_CLIENTS                                   0x0
#define PIPE_REJECT_REMOTE_CLIENTS                                   0x8

#define PIPE_UNLIMITED_INSTANCES                                     255

// #define SECURITY_ANONYMOUS                                           (SecurityAnonymous<<16)
// #define SECURITY_IDENTIFICATION                                      (SecurityIdentification<<16)
// #define SECURITY_IMPERSONATION                                       (SecurityImpersonation<<16)
// #define SECURITY_DELEGATION                                          (SecurityDelegation<<16)

#define SECURITY_CONTEXT_TRACKING                                    0x40000
#define SECURITY_EFFECTIVE_ONLY                                      0x80000

#define SECURITY_SQOS_PRESENT                                        0x100000
#define SECURITY_VALID_SQOS_FLAGS                                    0x1f0000

#define FAIL_FAST_GENERATE_EXCEPTION_ADDRESS                         0x1
#define FAIL_FAST_NO_HARD_ERROR_DLG                                  0x2

// #if defined (__i386__)
// #else
// #endif

#define SP_SERIALCOMM                                                (0x1)
#define PST_UNSPECIFIED                                              (0x0)
#define PST_RS232                                                    (0x1)
#define PST_PARALLELPORT                                             (0x2)
#define PST_RS422                                                    (0x3)
#define PST_RS423                                                    (0x4)
#define PST_RS449                                                    (0x5)
#define PST_MODEM                                                    (0x6)
#define PST_FAX                                                      (0x21)
#define PST_SCANNER                                                  (0x22)
#define PST_NETWORK_BRIDGE                                           (0x100)
#define PST_LAT                                                      (0x101)
#define PST_TCPIP_TELNET                                             (0x102)
#define PST_X25                                                      (0x103)

#define PCF_DTRDSR                                                   (0x1)
#define PCF_RTSCTS                                                   (0x2)
#define PCF_RLSD                                                     (0x4)
#define PCF_PARITY_CHECK                                             (0x8)
#define PCF_XONXOFF                                                  (0x10)
#define PCF_SETXCHAR                                                 (0x20)
#define PCF_TOTALTIMEOUTS                                            (0x40)
#define PCF_INTTIMEOUTS                                              (0x80)
#define PCF_SPECIALCHARS                                             (0x100)
#define PCF_16BITMODE                                                (0x200)

#define SP_PARITY                                                    (0x1)
#define SP_BAUD                                                      (0x2)
#define SP_DATABITS                                                  (0x4)
#define SP_STOPBITS                                                  (0x8)
#define SP_HANDSHAKING                                               (0x10)
#define SP_PARITY_CHECK                                              (0x20)
#define SP_RLSD                                                      (0x40)

#define BAUD_075                                                     (0x1)
#define BAUD_110                                                     (0x2)
#define BAUD_134_5                                                   (0x4)
#define BAUD_150                                                     (0x8)
#define BAUD_300                                                     (0x10)
#define BAUD_600                                                     (0x20)
#define BAUD_1200                                                    (0x40)
#define BAUD_1800                                                    (0x80)
#define BAUD_2400                                                    (0x100)
#define BAUD_4800                                                    (0x200)
#define BAUD_7200                                                    (0x400)
#define BAUD_9600                                                    (0x800)
#define BAUD_14400                                                   (0x1000)
#define BAUD_19200                                                   (0x2000)
#define BAUD_38400                                                   (0x4000)
#define BAUD_56K                                                     (0x8000)
#define BAUD_128K                                                    (0x10000)
#define BAUD_115200                                                  (0x20000)
#define BAUD_57600                                                   (0x40000)
#define BAUD_USER                                                    (0x10000000)

#define DATABITS_5                                                   (0x1)
#define DATABITS_6                                                   (0x2)
#define DATABITS_7                                                   (0x4)
#define DATABITS_8                                                   (0x8)
#define DATABITS_16                                                  (0x10)
#define DATABITS_16X                                                 (0x20)

#define STOPBITS_10                                                  (0x1)
#define STOPBITS_15                                                  (0x2)
#define STOPBITS_20                                                  (0x4)
#define PARITY_NONE                                                  (0x100)
#define PARITY_ODD                                                   (0x200)
#define PARITY_EVEN                                                  (0x400)
#define PARITY_MARK                                                  (0x800)
#define PARITY_SPACE                                                 (0x1000)

#define COMMPROP_INITIALIZED                                         (0xe73cf52e)

#define DTR_CONTROL_DISABLE                                          0x0
#define DTR_CONTROL_ENABLE                                           0x1
#define DTR_CONTROL_HANDSHAKE                                        0x2

#define RTS_CONTROL_DISABLE                                          0x0
#define RTS_CONTROL_ENABLE                                           0x1
#define RTS_CONTROL_HANDSHAKE                                        0x2
#define RTS_CONTROL_TOGGLE                                           0x3

// #define FreeModule(hLibModule)                                       FreeLibrary((hLibModule))
// #define MakeProcInstance(lpProc,hInstance)                           (lpProc)
// #define FreeProcInstance(lpProc)                                     (lpProc)

#define GMEM_FIXED                                                   0x0
#define GMEM_MOVEABLE                                                0x2
#define GMEM_NOCOMPACT                                               0x10
#define GMEM_NODISCARD                                               0x20
#define GMEM_ZEROINIT                                                0x40
#define GMEM_MODIFY                                                  0x80
#define GMEM_DISCARDABLE                                             0x100
#define GMEM_NOT_BANKED                                              0x1000
#define GMEM_SHARE                                                   0x2000
#define GMEM_DDESHARE                                                0x2000
#define GMEM_NOTIFY                                                  0x4000
#define GMEM_LOWER                                                   GMEM_NOT_BANKED
#define GMEM_VALID_FLAGS                                             0x7f72
#define GMEM_INVALID_HANDLE                                          0x8000

#define GHND                                                         hb_bitor(GMEM_MOVEABLE, GMEM_ZEROINIT)
#define GPTR                                                         hb_bitor(GMEM_FIXED, GMEM_ZEROINIT)

// #define GlobalLRUNewest(h)                                           ((HANDLE)(h))
// #define GlobalLRUOldest(h)                                           ((HANDLE)(h))
// #define GlobalDiscard(h)                                             GlobalReAlloc((h),0,GMEM_MOVEABLE)

#define GMEM_DISCARDED                                               0x4000
#define GMEM_LOCKCOUNT                                               0x00ff

#define NUMA_NO_PREFERRED_NODE                                       (-1)

#define DEBUG_PROCESS                                                0x1
#define DEBUG_ONLY_THIS_PROCESS                                      0x2
#define CREATE_SUSPENDED                                             0x4
#define DETACHED_PROCESS                                             0x8
#define CREATE_NEW_CONSOLE                                           0x10
#define NORMAL_PRIORITY_CLASS                                        0x20
#define IDLE_PRIORITY_CLASS                                          0x40
#define HIGH_PRIORITY_CLASS                                          0x80
#define REALTIME_PRIORITY_CLASS                                      0x100
#define CREATE_NEW_PROCESS_GROUP                                     0x200
#define CREATE_UNICODE_ENVIRONMENT                                   0x400
#define CREATE_SEPARATE_WOW_VDM                                      0x800
#define CREATE_SHARED_WOW_VDM                                        0x1000
#define CREATE_FORCEDOS                                              0x2000
#define BELOW_NORMAL_PRIORITY_CLASS                                  0x4000
#define ABOVE_NORMAL_PRIORITY_CLASS                                  0x8000
#define INHERIT_PARENT_AFFINITY                                      0x10000
#define INHERIT_CALLER_PRIORITY                                      0x20000
#define CREATE_PROTECTED_PROCESS                                     0x40000
#define EXTENDED_STARTUPINFO_PRESENT                                 0x80000
#define PROCESS_MODE_BACKGROUND_BEGIN                                0x100000
#define PROCESS_MODE_BACKGROUND_END                                  0x200000
#define CREATE_BREAKAWAY_FROM_JOB                                    0x1000000
#define CREATE_PRESERVE_CODE_AUTHZ_LEVEL                             0x2000000
#define CREATE_DEFAULT_ERROR_MODE                                    0x4000000
#define CREATE_NO_WINDOW                                             0x8000000
#define PROFILE_USER                                                 0x10000000
#define PROFILE_KERNEL                                               0x20000000
#define PROFILE_SERVER                                               0x40000000
#define CREATE_IGNORE_SYSTEM_DEFAULT                                 0x80000000

#define STACK_SIZE_PARAM_IS_A_RESERVATION                            0x10000

#define THREAD_PRIORITY_LOWEST                                       THREAD_BASE_PRIORITY_MIN
#define THREAD_PRIORITY_BELOW_NORMAL                                 (THREAD_PRIORITY_LOWEST+1)
#define THREAD_PRIORITY_NORMAL                                       0
#define THREAD_PRIORITY_HIGHEST                                      THREAD_BASE_PRIORITY_MAX
#define THREAD_PRIORITY_ABOVE_NORMAL                                 (THREAD_PRIORITY_HIGHEST-1)
#define THREAD_PRIORITY_ERROR_RETURN                                 (MAXLONG)

#define THREAD_PRIORITY_TIME_CRITICAL                                THREAD_BASE_PRIORITY_LOWRT
#define THREAD_PRIORITY_IDLE                                         THREAD_BASE_PRIORITY_IDLE

#define THREAD_MODE_BACKGROUND_BEGIN                                 0x00010000
#define THREAD_MODE_BACKGROUND_END                                   0x00020000

#define VOLUME_NAME_DOS                                              0x0
#define VOLUME_NAME_GUID                                             0x1
#define VOLUME_NAME_NT                                               0x2
#define VOLUME_NAME_NONE                                             0x4

#define FILE_NAME_NORMALIZED                                         0x0
#define FILE_NAME_OPENED                                             0x8

// #ifndef __WIDL__
// #endif

#define DRIVE_UNKNOWN                                                0
#define DRIVE_NO_ROOT_DIR                                            1
#define DRIVE_REMOVABLE                                              2
#define DRIVE_FIXED                                                  3
#define DRIVE_REMOTE                                                 4
#define DRIVE_CDROM                                                  5
#define DRIVE_RAMDISK                                                6

// #define GetFreeSpace(w)                                              (__MSABI_LONG(0x100000))

#define FILE_TYPE_UNKNOWN                                            0x0
#define FILE_TYPE_DISK                                               0x1
#define FILE_TYPE_CHAR                                               0x2
#define FILE_TYPE_PIPE                                               0x3
#define FILE_TYPE_REMOTE                                             0x8000

#define STD_INPUT_HANDLE                                             (-10)
#define STD_OUTPUT_HANDLE                                            (-11)
#define STD_ERROR_HANDLE                                             (-12)

#define NOPARITY                                                     0
#define ODDPARITY                                                    1
#define EVENPARITY                                                   2
#define MARKPARITY                                                   3
#define SPACEPARITY                                                  4

#define ONESTOPBIT                                                   0
#define ONE5STOPBITS                                                 1
#define TWOSTOPBITS                                                  2

#define IGNORE                                                       0
#define INFINITE                                                     0xffffffff

#define CBR_110                                                      110
#define CBR_300                                                      300
#define CBR_600                                                      600
#define CBR_1200                                                     1200
#define CBR_2400                                                     2400
#define CBR_4800                                                     4800
#define CBR_9600                                                     9600
#define CBR_14400                                                    14400
#define CBR_19200                                                    19200
#define CBR_38400                                                    38400
#define CBR_56000                                                    56000
#define CBR_57600                                                    57600
#define CBR_115200                                                   115200
#define CBR_128000                                                   128000
#define CBR_256000                                                   256000

#define CE_RXOVER                                                    0x1
#define CE_OVERRUN                                                   0x2
#define CE_RXPARITY                                                  0x4
#define CE_FRAME                                                     0x8
#define CE_BREAK                                                     0x10
#define CE_TXFULL                                                    0x100
#define CE_PTO                                                       0x200
#define CE_IOE                                                       0x400
#define CE_DNS                                                       0x800
#define CE_OOP                                                       0x1000
#define CE_MODE                                                      0x8000

#define IE_BADID                                                     (-1)
#define IE_OPEN                                                      (-2)
#define IE_NOPEN                                                     (-3)
#define IE_MEMORY                                                    (-4)
#define IE_DEFAULT                                                   (-5)
#define IE_HARDWARE                                                  (-10)
#define IE_BYTESIZE                                                  (-11)
#define IE_BAUDRATE                                                  (-12)

#define EV_RXCHAR                                                    0x1
#define EV_RXFLAG                                                    0x2
#define EV_TXEMPTY                                                   0x4
#define EV_CTS                                                       0x8
#define EV_DSR                                                       0x10
#define EV_RLSD                                                      0x20
#define EV_BREAK                                                     0x40
#define EV_ERR                                                       0x80
#define EV_RING                                                      0x100
#define EV_PERR                                                      0x200
#define EV_RX80FULL                                                  0x400
#define EV_EVENT1                                                    0x800
#define EV_EVENT2                                                    0x1000

#define SETXOFF                                                      1
#define SETXON                                                       2
#define SETRTS                                                       3
#define CLRRTS                                                       4
#define SETDTR                                                       5
#define CLRDTR                                                       6
#define RESETDEV                                                     7
#define SETBREAK                                                     8
#define CLRBREAK                                                     9

#define PURGE_TXABORT                                                0x1
#define PURGE_RXABORT                                                0x2
#define PURGE_TXCLEAR                                                0x4
#define PURGE_RXCLEAR                                                0x8

#define LPTx                                                         0x80

#define MS_CTS_ON                                                    (0x10)
#define MS_DSR_ON                                                    (0x20)
#define MS_RING_ON                                                   (0x40)
#define MS_RLSD_ON                                                   (0x80)

#define S_QUEUEEMPTY                                                 0
#define S_THRESHOLD                                                  1
#define S_ALLTHRESHOLD                                               2

#define S_NORMAL                                                     0
#define S_LEGATO                                                     1
#define S_STACCATO                                                   2

#define S_PERIOD512                                                  0
#define S_PERIOD1024                                                 1
#define S_PERIOD2048                                                 2
#define S_PERIODVOICE                                                3
#define S_WHITE512                                                   4
#define S_WHITE1024                                                  5
#define S_WHITE2048                                                  6
#define S_WHITEVOICE                                                 7

#define S_SERDVNA                                                    (-1)
#define S_SEROFM                                                     (-2)
#define S_SERMACT                                                    (-3)
#define S_SERQFUL                                                    (-4)
#define S_SERBDNT                                                    (-5)
#define S_SERDLN                                                     (-6)
#define S_SERDCC                                                     (-7)
#define S_SERDTP                                                     (-8)
#define S_SERDVL                                                     (-9)
#define S_SERDMD                                                     (-10)
#define S_SERDSH                                                     (-11)
#define S_SERDPT                                                     (-12)
#define S_SERDFQ                                                     (-13)
#define S_SERDDR                                                     (-14)
#define S_SERDSR                                                     (-15)
#define S_SERDST                                                     (-16)

#define NMPWAIT_WAIT_FOREVER                                         0xffffffff
#define NMPWAIT_NOWAIT                                               0x1
#define NMPWAIT_USE_DEFAULT_WAIT                                     0x0

#define FS_CASE_IS_PRESERVED                                         FILE_CASE_PRESERVED_NAMES
#define FS_CASE_SENSITIVE                                            FILE_CASE_SENSITIVE_SEARCH
#define FS_UNICODE_STORED_ON_DISK                                    FILE_UNICODE_ON_DISK
#define FS_PERSISTENT_ACLS                                           FILE_PERSISTENT_ACLS
#define FS_VOL_IS_COMPRESSED                                         FILE_VOLUME_IS_COMPRESSED
#define FS_FILE_COMPRESSION                                          FILE_FILE_COMPRESSION
#define FS_FILE_ENCRYPTION                                           FILE_SUPPORTS_ENCRYPTION

#define OF_READ                                                      0x0
#define OF_WRITE                                                     0x1
#define OF_READWRITE                                                 0x2
#define OF_SHARE_COMPAT                                              0x0
#define OF_SHARE_EXCLUSIVE                                           0x10
#define OF_SHARE_DENY_WRITE                                          0x20
#define OF_SHARE_DENY_READ                                           0x30
#define OF_SHARE_DENY_NONE                                           0x40
#define OF_PARSE                                                     0x100
#define OF_DELETE                                                    0x200
#define OF_VERIFY                                                    0x400
#define OF_CANCEL                                                    0x800
#define OF_CREATE                                                    0x1000
#define OF_PROMPT                                                    0x2000
#define OF_EXIST                                                     0x4000
#define OF_REOPEN                                                    0x8000

#define OFS_MAXPATHNAME                                              128

// #ifndef NOWINBASEINTERLOCK
// #ifndef _NTOS_
// #if defined (__ia64__) && !defined (RC_INVOKED)

// #define InterlockedIncrement                                         _InterlockedIncrement
// #define InterlockedIncrementAcquire                                  _InterlockedIncrement_acq
// #define InterlockedIncrementRelease                                  _InterlockedIncrement_rel
// #define InterlockedDecrement                                         _InterlockedDecrement
// #define InterlockedDecrementAcquire                                  _InterlockedDecrement_acq
// #define InterlockedDecrementRelease                                  _InterlockedDecrement_rel
// #define InterlockedExchange                                          _InterlockedExchange
// #define InterlockedExchangeAdd                                       _InterlockedExchangeAdd
// #define InterlockedCompareExchange                                   _InterlockedCompareExchange
// #define InterlockedCompareExchangeAcquire                            _InterlockedCompareExchange_acq
// #define InterlockedCompareExchangeRelease                            _InterlockedCompareExchange_rel
// #define InterlockedExchangePointer                                   _InterlockedExchangePointer
// #define InterlockedCompareExchangePointer                            _InterlockedCompareExchangePointer
// #define InterlockedCompareExchangePointerRelease                     _InterlockedCompareExchangePointer_rel
// #define InterlockedCompareExchangePointerAcquire                     _InterlockedCompareExchangePointer_acq

// #define InterlockedIncrement64                                       _InterlockedIncrement64
// #define InterlockedDecrement64                                       _InterlockedDecrement64
// #define InterlockedExchange64                                        _InterlockedExchange64
// #define InterlockedExchangeAcquire64                                 _InterlockedExchange64_acq
// #define InterlockedExchangeAdd64                                     _InterlockedExchangeAdd64
// #define InterlockedCompareExchange64                                 _InterlockedCompareExchange64
// #define InterlockedCompareExchangeAcquire64                          _InterlockedCompareExchange64_acq
// #define InterlockedCompareExchangeRelease64                          _InterlockedCompareExchange64_rel
// #define InterlockedCompare64Exchange128                              _InterlockedCompare64Exchange128
// #define InterlockedCompare64ExchangeAcquire128                       _InterlockedCompare64Exchange128_acq
// #define InterlockedCompare64ExchangeRelease128                       _InterlockedCompare64Exchange128_rel

// #define InterlockedOr                                                _InterlockedOr
// #define InterlockedOrAcquire                                         _InterlockedOr_acq
// #define InterlockedOrRelease                                         _InterlockedOr_rel
// #define InterlockedOr8                                               _InterlockedOr8
// #define InterlockedOr8Acquire                                        _InterlockedOr8_acq
// #define InterlockedOr8Release                                        _InterlockedOr8_rel
// #define InterlockedOr16                                              _InterlockedOr16
// #define InterlockedOr16Acquire                                       _InterlockedOr16_acq
// #define InterlockedOr16Release                                       _InterlockedOr16_rel
// #define InterlockedOr64                                              _InterlockedOr64
// #define InterlockedOr64Acquire                                       _InterlockedOr64_acq
// #define InterlockedOr64Release                                       _InterlockedOr64_rel
// #define InterlockedXor                                               _InterlockedXor
// #define InterlockedXorAcquire                                        _InterlockedXor_acq
// #define InterlockedXorRelease                                        _InterlockedXor_rel
// #define InterlockedXor8                                              _InterlockedXor8
// #define InterlockedXor8Acquire                                       _InterlockedXor8_acq
// #define InterlockedXor8Release                                       _InterlockedXor8_rel
// #define InterlockedXor16                                             _InterlockedXor16
// #define InterlockedXor16Acquire                                      _InterlockedXor16_acq
// #define InterlockedXor16Release                                      _InterlockedXor16_rel
// #define InterlockedXor64                                             _InterlockedXor64
// #define InterlockedXor64Acquire                                      _InterlockedXor64_acq
// #define InterlockedXor64Release                                      _InterlockedXor64_rel
// #define InterlockedAnd                                               _InterlockedAnd
// #define InterlockedAndAcquire                                        _InterlockedAnd_acq
// #define InterlockedAndRelease                                        _InterlockedAnd_rel
// #define InterlockedAnd8                                              _InterlockedAnd8
// #define InterlockedAnd8Acquire                                       _InterlockedAnd8_acq
// #define InterlockedAnd8Release                                       _InterlockedAnd8_rel
// #define InterlockedAnd16                                             _InterlockedAnd16
// #define InterlockedAnd16Acquire                                      _InterlockedAnd16_acq
// #define InterlockedAnd16Release                                      _InterlockedAnd16_rel
// #define InterlockedAnd64                                             _InterlockedAnd64
// #define InterlockedAnd64Acquire                                      _InterlockedAnd64_acq
// #define InterlockedAnd64Release                                      _InterlockedAnd64_rel

// #if !defined(__WIDL__) && !defined(__CRT__NO_INLINE)
// #ifndef InterlockedAnd
// #define InterlockedAnd                                               InterlockedAnd_Inline
// #endif

// #ifndef InterlockedOr
// #define InterlockedOr                                                InterlockedOr_Inline
// #endif

// #ifndef InterlockedXor
// #define InterlockedXor                                               InterlockedXor_Inline
// #endif

// #ifndef InterlockedAnd64
// #define InterlockedAnd64                                             InterlockedAnd64_Inline
// #endif

// #ifndef InterlockedOr64
// #define InterlockedOr64                                              InterlockedOr64_Inline
// #endif

// #ifndef InterlockedXor64
// #define InterlockedXor64                                             InterlockedXor64_Inline
// #endif

// #ifndef InterlockedBitTestAndSet
// #define InterlockedBitTestAndSet                                     InterlockedBitTestAndSet_Inline
// #endif

// #ifndef InterlockedBitTestAndReset
// #define InterlockedBitTestAndReset                                   InterlockedBitTestAndReset_Inline
// #endif

// #ifndef InterlockedBitTestAndComplement
// #define InterlockedBitTestAndComplement                              InterlockedBitTestAndComplement_Inline
// #endif

// #endif

// #elif defined (__x86_64__) && !defined (RC_INVOKED)

// #define InterlockedIncrement                                         _InterlockedIncrement
// #define InterlockedIncrementAcquire                                  InterlockedIncrement
// #define InterlockedIncrementRelease                                  InterlockedIncrement
// #define InterlockedDecrement                                         _InterlockedDecrement
// #define InterlockedDecrementAcquire                                  InterlockedDecrement
// #define InterlockedDecrementRelease                                  InterlockedDecrement
// #define InterlockedExchange                                          _InterlockedExchange
// #define InterlockedExchangeAdd                                       _InterlockedExchangeAdd
// #define InterlockedCompareExchange                                   _InterlockedCompareExchange
// #define InterlockedCompareExchangeAcquire                            InterlockedCompareExchange
// #define InterlockedCompareExchangeRelease                            InterlockedCompareExchange
// #define InterlockedExchangePointer                                   _InterlockedExchangePointer
// #define InterlockedCompareExchangePointer                            _InterlockedCompareExchangePointer
// #define InterlockedCompareExchangePointerAcquire                     _InterlockedCompareExchangePointer
// #define InterlockedCompareExchangePointerRelease                     _InterlockedCompareExchangePointer
// #define InterlockedAnd64                                             _InterlockedAnd64
// #define InterlockedOr64                                              _InterlockedOr64
// #define InterlockedXor64                                             _InterlockedXor64
// #define InterlockedIncrement64                                       _InterlockedIncrement64
// #define InterlockedDecrement64                                       _InterlockedDecrement64
// #define InterlockedExchange64                                        _InterlockedExchange64
// #define InterlockedExchangeAdd64                                     _InterlockedExchangeAdd64
// #define InterlockedCompareExchange64                                 _InterlockedCompareExchange64
// #define InterlockedCompareExchangeAcquire64                          InterlockedCompareExchange64
// #define InterlockedCompareExchangeRelease64                          InterlockedCompareExchange64

// #define InterlockedAnd8                                              _InterlockedAnd8
// #define InterlockedOr8                                               _InterlockedOr8
// #define InterlockedXor8                                              _InterlockedXor8
// #define InterlockedAnd16                                             _InterlockedAnd16
// #define InterlockedOr16                                              _InterlockedOr16
// #define InterlockedXor16                                             _InterlockedXor16

// #elif (defined (__arm__) || defined (__aarch64__)) && !defined (RC_INVOKED)

// #define InterlockedAnd                                               _InterlockedAnd
// #define InterlockedOr                                                _InterlockedOr
// #define InterlockedXor                                               _InterlockedXor
// #define InterlockedIncrement                                         _InterlockedIncrement
// #define InterlockedDecrement                                         _InterlockedDecrement
// #define InterlockedExchange                                          _InterlockedExchange
// #define InterlockedExchangeAdd                                       _InterlockedExchangeAdd
// #define InterlockedExchangePointer                                   _InterlockedExchangePointer
// #define InterlockedCompareExchange                                   _InterlockedCompareExchange
// #define InterlockedCompareExchangePointer                            _InterlockedCompareExchangePointer
// #define InterlockedAnd64                                             _InterlockedAnd64
// #define InterlockedOr64                                              _InterlockedOr64
// #define InterlockedXor64                                             _InterlockedXor64
// #define InterlockedIncrement64                                       _InterlockedIncrement64
// #define InterlockedDecrement64                                       _InterlockedDecrement64
// #define InterlockedExchange64                                        _InterlockedExchange64
// #define InterlockedExchangeAdd64                                     _InterlockedExchangeAdd64
// #define InterlockedCompareExchange64                                 _InterlockedCompareExchange64

// #else
// #if !defined (__WIDL__) && defined (__MINGW_INTRIN_INLINE)
// #endif

// #ifdef __cplusplus
// #define InterlockedCompareExchangePointer                            __InlineInterlockedCompareExchangePointer
// #else
// #define InterlockedCompareExchangePointer(Destination,               ExChange,Comperand)(PVOID)(LONG_PTR)InterlockedCompareExchange((LONGvolatile*)(Destination),(LONG)(LONG_PTR)(ExChange),(LONG)(LONG_PTR)(Comperand))
// #endif

// #define InterlockedIncrementAcquire                                  InterlockedIncrement
// #define InterlockedIncrementRelease                                  InterlockedIncrement
// #define InterlockedDecrementAcquire                                  InterlockedDecrement
// #define InterlockedDecrementRelease                                  InterlockedDecrement
// #define InterlockedIncrementAcquire                                  InterlockedIncrement
// #define InterlockedIncrementRelease                                  InterlockedIncrement
// #define InterlockedCompareExchangeAcquire                            InterlockedCompareExchange
// #define InterlockedCompareExchangeRelease                            InterlockedCompareExchange
// #define InterlockedCompareExchangeAcquire64                          InterlockedCompareExchange64
// #define InterlockedCompareExchangeRelease64                          InterlockedCompareExchange64
// #define InterlockedCompareExchangePointerAcquire                     InterlockedCompareExchangePointer
// #define InterlockedCompareExchangePointerRelease                     InterlockedCompareExchangePointer
// #endif
// #endif
// #endif

// #define UnlockResource(hResData)                                     ({(VOID)(hResData);0;})
#define MAXINTATOM                                                   0xc000
// #define MAKEINTATOM(i)                                               (LPTSTR)((ULONG_PTR)((WORD)(i)))
#define INVALID_ATOM                                                 (0)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP) || defined(WINSTORECOMPAT)
// #endif
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT >= 0x0600
// #endif
// #if _WIN32_WINNT >= 0x0601
// #endif

#define SCS_32BIT_BINARY                                             0
#define SCS_DOS_BINARY                                               1
#define SCS_WOW_BINARY                                               2
#define SCS_PIF_BINARY                                               3
#define SCS_POSIX_BINARY                                             4
#define SCS_OS216_BINARY                                             5
#define SCS_64BIT_BINARY                                             6

// #ifdef _WIN64
// #define SCS_THIS_PLATFORM_BINARY                                     SCS_64BIT_BINARY
// #else
// #define SCS_THIS_PLATFORM_BINARY                                     SCS_32BIT_BINARY
// #endif

// #if _WIN32_WINNT >= 0x0600
// #endif

// #ifndef UNICODE
// #define SetEnvironmentStrings                                        SetEnvironmentStringsA
// #define GetShortPathName                                             GetShortPathNameA
// #endif

// #define GetBinaryType                                                __MINGW_NAME_AW(GetBinaryType)
// #if _WIN32_WINNT >= 0x0600
// #define GetLongPathNameTransacted                                    __MINGW_NAME_AW(GetLongPathNameTransacted)
// #endif

// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define FIBER_FLAG_FLOAT_SWITCH                                      0x1

// #if _WIN32_WINNT >= 0x0602
#define MEMORY_PRIORITY_LOWEST                                       0
#define MEMORY_PRIORITY_VERY_LOW                                     1
#define MEMORY_PRIORITY_LOW                                          2
#define MEMORY_PRIORITY_MEDIUM                                       3
#define MEMORY_PRIORITY_BELOW_NORMAL                                 4
#define MEMORY_PRIORITY_NORMAL                                       5
// #endif

// #if _WIN32_WINNT >= 0x0600
#define PROCESS_DEP_ENABLE                                           0x00000001
#define PROCESS_DEP_DISABLE_ATL_THUNK_EMULATION                      0x00000002
// #endif

// #if _WIN32_WINNT >= 0x0601
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #if !defined (RC_INVOKED) && defined (WINBASE_DECLARE_RESTORE_LAST_ERROR)
// #define RESTORE_LAST_ERROR_NAME_A                                    "RestoreLastError"
// #define RESTORE_LAST_ERROR_NAME_W                                    L"RestoreLastError"
// #define RESTORE_LAST_ERROR_NAME                                      TEXT("RestoreLastError")
// #endif

// #define HasOverlappedIoCompleted(lpOverlapped)                       (((DWORD)(lpOverlapped)->Internal)!=STATUS_PENDING)

// #if _WIN32_WINNT >= 0x0600
#define FILE_SKIP_COMPLETION_PORT_ON_SUCCESS                         0x1
#define FILE_SKIP_SET_EVENT_ON_HANDLE                                0x2
// #endif

#define SEM_FAILCRITICALERRORS                                       0x0001
#define SEM_NOGPFAULTERRORBOX                                        0x0002
#define SEM_NOALIGNMENTFAULTEXCEPT                                   0x0004
#define SEM_NOOPENFILEERRORBOX                                       0x8000

// #if !defined (__WIDL__) && _WIN32_WINNT >= 0x0600
// #if _WIN32_WINNT >= 0x0601
// #endif
// #endif

// #if _WIN32_WINNT >= 0x0600
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
#define CRITICAL_SECTION_NO_DEBUG_INFO                               RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define HANDLE_FLAG_INHERIT                                          0x1
#define HANDLE_FLAG_PROTECT_FROM_CLOSE                               0x2

#define HINSTANCE_ERROR                                              32

#define GET_TAPE_MEDIA_INFORMATION                                   0
#define GET_TAPE_DRIVE_INFORMATION                                   1

#define SET_TAPE_MEDIA_INFORMATION                                   0
#define SET_TAPE_DRIVE_INFORMATION                                   1

// #define SetFileShortName                                             __MINGW_NAME_AW(SetFileShortName)

// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)

// #ifndef __WIDL__
// #define FormatMessage                                                __MINGW_NAME_AW(FormatMessage)
// #endif

#define FORMAT_MESSAGE_IGNORE_INSERTS                                0x00000200
#define FORMAT_MESSAGE_FROM_STRING                                   0x00000400
#define FORMAT_MESSAGE_FROM_HMODULE                                  0x00000800
#define FORMAT_MESSAGE_FROM_SYSTEM                                   0x00001000
#define FORMAT_MESSAGE_ARGUMENT_ARRAY                                0x00002000
#define FORMAT_MESSAGE_MAX_WIDTH_MASK                                0x000000ff
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define FILE_ENCRYPTABLE                                             0
#define FILE_IS_ENCRYPTED                                            1
#define FILE_SYSTEM_ATTR                                             2
#define FILE_ROOT_DIR                                                3
#define FILE_SYSTEM_DIR                                              4
#define FILE_UNKNOWN                                                 5
#define FILE_SYSTEM_NOT_SUPPORT                                      6
#define FILE_USER_DISALLOWED                                         7
#define FILE_READ_ONLY                                               8
#define FILE_DIR_DISALLOWED                                          9

#define FORMAT_MESSAGE_ALLOCATE_BUFFER                               0x00000100

#define EFS_USE_RECOVERY_KEYS                                        (0x1)

#define CREATE_FOR_IMPORT                                            (1)
#define CREATE_FOR_DIR                                               (2)
#define OVERWRITE_HIDDEN                                             (4)
#define EFSRPC_SECURE_ONLY                                           (8)

// #define CreateMailslot                                               __MINGW_NAME_AW(CreateMailslot)
// #define EncryptFile                                                  __MINGW_NAME_AW(EncryptFile)
// #define DecryptFile                                                  __MINGW_NAME_AW(DecryptFile)
// #define FileEncryptionStatus                                         __MINGW_NAME_AW(FileEncryptionStatus)
// #define OpenEncryptedFileRaw                                         __MINGW_NAME_AW(OpenEncryptedFileRaw)
// #define lstrcmp                                                      __MINGW_NAME_AW(lstrcmp)
// #define lstrcmpi                                                     __MINGW_NAME_AW(lstrcmpi)
// #define lstrcpyn                                                     __MINGW_NAME_AW(lstrcpyn)
// #define lstrcpy                                                      __MINGW_NAME_AW(lstrcpy)
// #define lstrcat                                                      __MINGW_NAME_AW(lstrcat)
// #define lstrlen                                                      __MINGW_NAME_AW(lstrlen)

#define BACKUP_INVALID                                               0x00000000
#define BACKUP_DATA                                                  0x00000001
#define BACKUP_EA_DATA                                               0x00000002
#define BACKUP_SECURITY_DATA                                         0x00000003
#define BACKUP_ALTERNATE_DATA                                        0x00000004
#define BACKUP_LINK                                                  0x00000005
#define BACKUP_PROPERTY_DATA                                         0x00000006
#define BACKUP_OBJECT_ID                                             0x00000007
#define BACKUP_REPARSE_DATA                                          0x00000008
#define BACKUP_SPARSE_BLOCK                                          0x00000009
#define BACKUP_TXFS_DATA                                             0x0000000a

#define STREAM_NORMAL_ATTRIBUTE                                      0x00000000
#define STREAM_MODIFIED_WHEN_READ                                    0x00000001
#define STREAM_CONTAINS_SECURITY                                     0x00000002
#define STREAM_CONTAINS_PROPERTIES                                   0x00000004
#define STREAM_SPARSE_ATTRIBUTE                                      0x00000008

#define STARTF_USESHOWWINDOW                                         0x00000001
#define STARTF_USESIZE                                               0x00000002
#define STARTF_USEPOSITION                                           0x00000004
#define STARTF_USECOUNTCHARS                                         0x00000008
#define STARTF_USEFILLATTRIBUTE                                      0x00000010
#define STARTF_RUNFULLSCREEN                                         0x00000020
#define STARTF_FORCEONFEEDBACK                                       0x00000040
#define STARTF_FORCEOFFFEEDBACK                                      0x00000080
#define STARTF_USESTDHANDLES                                         0x00000100

#define STARTF_USEHOTKEY                                             0x00000200
#define STARTF_TITLEISLINKNAME                                       0x00000800
#define STARTF_TITLEISAPPID                                          0x00001000
#define STARTF_PREVENTPINNING                                        0x00002000

// #if _WIN32_WINNT >= 0x0600
// #endif

#define SHUTDOWN_NORETRY                                             0x1
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP) || defined(WINSTORECOMPAT)
// #define CreateSemaphore                                              __MINGW_NAME_AW(CreateSemaphore)
// #define LoadLibrary                                                  __MINGW_NAME_AW(LoadLibrary)
// #endif
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT >= 0x0600
// #endif

// #ifndef UNICODE
// #define OpenMutex                                                    OpenMutexA
// #define OpenSemaphore                                                OpenSemaphoreA
// #define OpenWaitableTimer                                            OpenWaitableTimerA
// #define CreateFileMapping                                            CreateFileMappingA
// #define OpenFileMapping                                              OpenFileMappingA
// #define GetLogicalDriveStrings                                       GetLogicalDriveStringsA
// #endif

// #define CreateWaitableTimer                                          __MINGW_NAME_AW(CreateWaitableTimer)
// #define LoadLibrary                                                  __MINGW_NAME_AW(LoadLibrary)

// #if _WIN32_WINNT >= 0x0600
// #ifndef UNICODE
// #define CreateSemaphoreEx                                            CreateSemaphoreExA
// #define CreateWaitableTimerEx                                        CreateWaitableTimerExA
// #define CreateFileMappingNuma                                        CreateFileMappingNumaA
// #endif
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP) && _WIN32_WINNT >= 0x0602
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT >= 0x0600

#define PROCESS_NAME_NATIVE                                          0x00000001

// #define QueryFullProcessImageName                                    __MINGW_NAME_AW(QueryFullProcessImageName)

#define PROC_THREAD_ATTRIBUTE_NUMBER                                 0x0000ffff
#define PROC_THREAD_ATTRIBUTE_THREAD                                 0x00010000
#define PROC_THREAD_ATTRIBUTE_INPUT                                  0x00020000
#define PROC_THREAD_ATTRIBUTE_ADDITIVE                               0x00040000

// #ifndef _USE_FULL_PROC_THREAD_ATTRIBUTE
// #if _WIN32_WINNT >= 0x0601
// #endif
// #if _WIN32_WINNT >= 0x0602
// #endif
// #endif

// #define ProcThreadAttributeValue(Number,                             Thread,Input,Additive)(((Number)&PROC_THREAD_ATTRIBUTE_NUMBER)|((Thread!=FALSE)?PROC_THREAD_ATTRIBUTE_THREAD:0)|((Input!=FALSE)?PROC_THREAD_ATTRIBUTE_INPUT:0)|((Additive!=FALSE)?PROC_THREAD_ATTRIBUTE_ADDITIVE:0))

// #define PROC_THREAD_ATTRIBUTE_PARENT_PROCESS                         ProcThreadAttributeValue(ProcThreadAttributeParentProcess,FALSE,TRUE,FALSE)
// #define PROC_THREAD_ATTRIBUTE_HANDLE_LIST                            ProcThreadAttributeValue(ProcThreadAttributeHandleList,FALSE,TRUE,FALSE)
// #endif

// #if _WIN32_WINNT >= 0x0601
// #define PROC_THREAD_ATTRIBUTE_GROUP_AFFINITY                         ProcThreadAttributeValue(ProcThreadAttributeGroupAffinity,TRUE,TRUE,FALSE)
// #define PROC_THREAD_ATTRIBUTE_PREFERRED_NODE                         ProcThreadAttributeValue(ProcThreadAttributePreferredNode,FALSE,TRUE,FALSE)
// #define PROC_THREAD_ATTRIBUTE_IDEAL_PROCESSOR                        ProcThreadAttributeValue(ProcThreadAttributeIdealProcessor,TRUE,TRUE,FALSE)
// #define PROC_THREAD_ATTRIBUTE_UMS_THREAD                             ProcThreadAttributeValue(ProcThreadAttributeUmsThread,TRUE,TRUE,FALSE)
// #define PROC_THREAD_ATTRIBUTE_MITIGATION_POLICY                      ProcThreadAttributeValue(ProcThreadAttributeMitigationPolicy,FALSE,TRUE,FALSE)

#define PROCESS_CREATION_MITIGATION_POLICY_DEP_ENABLE                0x01
#define PROCESS_CREATION_MITIGATION_POLICY_DEP_ATL_THUNK_ENABLE      0x02
#define PROCESS_CREATION_MITIGATION_POLICY_SEHOP_ENABLE              0x04
// #endif

// #if _WIN32_WINNT >= 0x0602
// #define PROC_THREAD_ATTRIBUTE_SECURITY_CAPABILITIES                  ProcThreadAttributeValue(ProcThreadAttributeSecurityCapabilities,FALSE,TRUE,FALSE)

#define PROCESS_CREATION_MITIGATION_POLICY_FORCE_RELOCATE_IMAGES_MASK                 hb_BitShift(0x00000003, 8)
#define PROCESS_CREATION_MITIGATION_POLICY_FORCE_RELOCATE_IMAGES_DEFER                hb_BitShift(0x00000000, 8)
#define PROCESS_CREATION_MITIGATION_POLICY_FORCE_RELOCATE_IMAGES_ALWAYS_ON            hb_BitShift(0x00000001, 8)
#define PROCESS_CREATION_MITIGATION_POLICY_FORCE_RELOCATE_IMAGES_ALWAYS_OFF           hb_BitShift(0x00000002, 8)
#define PROCESS_CREATION_MITIGATION_POLICY_FORCE_RELOCATE_IMAGES_ALWAYS_ON_REQ_RELOCS hb_BitShift(0x00000003, 8)

#define PROCESS_CREATION_MITIGATION_POLICY_HEAP_TERMINATE_MASK       hb_BitShift(0x00000003, 12)
#define PROCESS_CREATION_MITIGATION_POLICY_HEAP_TERMINATE_DEFER      hb_BitShift(0x00000000, 12)
#define PROCESS_CREATION_MITIGATION_POLICY_HEAP_TERMINATE_ALWAYS_ON  hb_BitShift(0x00000001, 12)
#define PROCESS_CREATION_MITIGATION_POLICY_HEAP_TERMINATE_ALWAYS_OFF hb_BitShift(0x00000002, 12)
#define PROCESS_CREATION_MITIGATION_POLICY_HEAP_TERMINATE_RESERVED   hb_BitShift(0x00000003, 12)

#define PROCESS_CREATION_MITIGATION_POLICY_BOTTOM_UP_ASLR_MASK       hb_BitShift(0x00000003, 16)
#define PROCESS_CREATION_MITIGATION_POLICY_BOTTOM_UP_ASLR_DEFER      hb_BitShift(0x00000000, 16)
#define PROCESS_CREATION_MITIGATION_POLICY_BOTTOM_UP_ASLR_ALWAYS_ON  hb_BitShift(0x00000001, 16)
#define PROCESS_CREATION_MITIGATION_POLICY_BOTTOM_UP_ASLR_ALWAYS_OFF hb_BitShift(0x00000002, 16)
#define PROCESS_CREATION_MITIGATION_POLICY_BOTTOM_UP_ASLR_RESERVED   hb_BitShift(0x00000003, 16)

#define PROCESS_CREATION_MITIGATION_POLICY_HIGH_ENTROPY_ASLR_MASK       hb_BitShift(0x00000003, 20)
#define PROCESS_CREATION_MITIGATION_POLICY_HIGH_ENTROPY_ASLR_DEFER      hb_BitShift(0x00000000, 20)
#define PROCESS_CREATION_MITIGATION_POLICY_HIGH_ENTROPY_ASLR_ALWAYS_ON  hb_BitShift(0x00000001, 20)
#define PROCESS_CREATION_MITIGATION_POLICY_HIGH_ENTROPY_ASLR_ALWAYS_OFF hb_BitShift(0x00000002, 20)
#define PROCESS_CREATION_MITIGATION_POLICY_HIGH_ENTROPY_ASLR_RESERVED   hb_BitShift(0x00000003, 20)

#define PROCESS_CREATION_MITIGATION_POLICY_STRICT_HANDLE_CHECKS_MASK       hb_BitShift(0x00000003, 24)
#define PROCESS_CREATION_MITIGATION_POLICY_STRICT_HANDLE_CHECKS_DEFER      hb_BitShift(0x00000000, 24)
#define PROCESS_CREATION_MITIGATION_POLICY_STRICT_HANDLE_CHECKS_ALWAYS_ON  hb_BitShift(0x00000001, 24)
#define PROCESS_CREATION_MITIGATION_POLICY_STRICT_HANDLE_CHECKS_ALWAYS_OFF hb_BitShift(0x00000002, 24)
#define PROCESS_CREATION_MITIGATION_POLICY_STRICT_HANDLE_CHECKS_RESERVED   hb_BitShift(0x00000003, 24)

#define PROCESS_CREATION_MITIGATION_POLICY_WIN32K_SYSTEM_CALL_DISABLE_MASK       hb_BitShift(0x00000003, 28)
#define PROCESS_CREATION_MITIGATION_POLICY_WIN32K_SYSTEM_CALL_DISABLE_DEFER      hb_BitShift(0x00000000, 28)
#define PROCESS_CREATION_MITIGATION_POLICY_WIN32K_SYSTEM_CALL_DISABLE_ALWAYS_ON  hb_BitShift(0x00000001, 28)
#define PROCESS_CREATION_MITIGATION_POLICY_WIN32K_SYSTEM_CALL_DISABLE_ALWAYS_OFF hb_BitShift(0x00000002, 28)
#define PROCESS_CREATION_MITIGATION_POLICY_WIN32K_SYSTEM_CALL_DISABLE_RESERVED   hb_BitShift(0x00000003, 28)

#define PROCESS_CREATION_MITIGATION_POLICY_EXTENSION_POINT_DISABLE_MASK       hb_BitShift(0x00000003ULL, 32)
#define PROCESS_CREATION_MITIGATION_POLICY_EXTENSION_POINT_DISABLE_DEFER      hb_BitShift(0x00000000ULL, 32)
#define PROCESS_CREATION_MITIGATION_POLICY_EXTENSION_POINT_DISABLE_ALWAYS_ON  hb_BitShift(0x00000001ULL, 32)
#define PROCESS_CREATION_MITIGATION_POLICY_EXTENSION_POINT_DISABLE_ALWAYS_OFF hb_BitShift(0x00000002ULL, 32)
#define PROCESS_CREATION_MITIGATION_POLICY_EXTENSION_POINT_DISABLE_RESERVED   hb_BitShift(0x00000003ULL, 32)
// #endif

#define ATOM_FLAG_GLOBAL                                             0x2

// #if _WIN32_WINNT >= 0x0602
// #endif

// #ifndef UNICODE
// #define GetStartupInfo                                               GetStartupInfoA
// #define FindResourceEx                                               FindResourceExA
// #define GetTempPath                                                  GetTempPathA
// #define GetTempFileName                                              GetTempFileNameA
// #endif

// #define FatalAppExit                                                 __MINGW_NAME_AW(FatalAppExit)
// #define GetFirmwareEnvironmentVariable                               __MINGW_NAME_AW(GetFirmwareEnvironmentVariable)
// #define SetFirmwareEnvironmentVariable                               __MINGW_NAME_AW(SetFirmwareEnvironmentVariable)
// #define FindResource                                                 __MINGW_NAME_AW(FindResource)
// #define EnumResourceTypes                                            __MINGW_NAME_AW(EnumResourceTypes)
// #define EnumResourceNames                                            __MINGW_NAME_AW(EnumResourceNames)
// #define EnumResourceLanguages                                        __MINGW_NAME_AW(EnumResourceLanguages)
// #define BeginUpdateResource                                          __MINGW_NAME_AW(BeginUpdateResource)
// #define UpdateResource                                               __MINGW_NAME_AW(UpdateResource)
// #define EndUpdateResource                                            __MINGW_NAME_AW(EndUpdateResource)
// #define GlobalAddAtom                                                __MINGW_NAME_AW(GlobalAddAtom)
// #define GlobalAddAtomEx                                              __MINGW_NAME_AW(GlobalAddAtomEx)
// #define GlobalFindAtom                                               __MINGW_NAME_AW(GlobalFindAtom)
// #define GlobalGetAtomName                                            __MINGW_NAME_AW(GlobalGetAtomName)
// #define AddAtom                                                      __MINGW_NAME_AW(AddAtom)
// #define FindAtom                                                     __MINGW_NAME_AW(FindAtom)
// #define GetAtomName                                                  __MINGW_NAME_AW(GetAtomName)
// #define GetProfileInt                                                __MINGW_NAME_AW(GetProfileInt)
// #define GetProfileString                                             __MINGW_NAME_AW(GetProfileString)
// #define WriteProfileString                                           __MINGW_NAME_AW(WriteProfileString)
// #define GetProfileSection                                            __MINGW_NAME_AW(GetProfileSection)
// #define WriteProfileSection                                          __MINGW_NAME_AW(WriteProfileSection)
// #define GetPrivateProfileInt                                         __MINGW_NAME_AW(GetPrivateProfileInt)
// #define GetPrivateProfileString                                      __MINGW_NAME_AW(GetPrivateProfileString)
// #define WritePrivateProfileString                                    __MINGW_NAME_AW(WritePrivateProfileString)
// #define GetPrivateProfileSection                                     __MINGW_NAME_AW(GetPrivateProfileSection)
// #define WritePrivateProfileSection                                   __MINGW_NAME_AW(WritePrivateProfileSection)
// #define GetPrivateProfileSectionNames                                __MINGW_NAME_AW(GetPrivateProfileSectionNames)
// #define GetPrivateProfileStruct                                      __MINGW_NAME_AW(GetPrivateProfileStruct)
// #define WritePrivateProfileStruct                                    __MINGW_NAME_AW(WritePrivateProfileStruct)

// #if _WIN32_WINNT >= 0x0602
// #define GetFirmwareEnvironmentVariableEx                             __MINGW_NAME_AW(GetFirmwareEnvironmentVariableEx)
// #define SetFirmwareEnvironmentVariableEx                             __MINGW_NAME_AW(SetFirmwareEnvironmentVariableEx)
// #endif

// #ifndef RC_INVOKED

// #define GetSystemWow64Directory                                      __MINGW_NAME_AW(GetSystemWow64Directory)

// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A                          "GetSystemWow64DirectoryA"
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W                          L"GetSystemWow64DirectoryA"
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T                          TEXT("GetSystemWow64DirectoryA")
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A                          "GetSystemWow64DirectoryW"
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W                          L"GetSystemWow64DirectoryW"
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T                          TEXT("GetSystemWow64DirectoryW")

// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A                          __MINGW_NAME_UAW_EXT(GET_SYSTEM_WOW64_DIRECTORY_NAME,A)
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W                          __MINGW_NAME_UAW_EXT(GET_SYSTEM_WOW64_DIRECTORY_NAME,W)
// #define GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T                          __MINGW_NAME_UAW_EXT(GET_SYSTEM_WOW64_DIRECTORY_NAME,T)
// #endif

// #define SetDllDirectory                                              __MINGW_NAME_AW(SetDllDirectory)
// #define GetDllDirectory                                              __MINGW_NAME_AW(GetDllDirectory)

#define BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE                      0x1
#define BASE_SEARCH_PATH_DISABLE_SAFE_SEARCHMODE                     0x10000
#define BASE_SEARCH_PATH_PERMANENT                                   0x8000
#define BASE_SEARCH_PATH_INVALID_FLAGS                               ~0x18001

// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #define CreateDirectoryEx                                            __MINGW_NAME_AW(CreateDirectoryEx)

// #if _WIN32_WINNT >= 0x0600
// #define CreateDirectoryTransacted                                    __MINGW_NAME_AW(CreateDirectoryTransacted)
// #define RemoveDirectoryTransacted                                    __MINGW_NAME_AW(RemoveDirectoryTransacted)
// #define GetFullPathNameTransacted                                    __MINGW_NAME_AW(GetFullPathNameTransacted)
// #endif

#define DDD_RAW_TARGET_PATH                                          0x00000001
#define DDD_REMOVE_DEFINITION                                        0x00000002
#define DDD_EXACT_MATCH_ON_REMOVE                                    0x00000004
#define DDD_NO_BROADCAST_SYSTEM                                      0x00000008
#define DDD_LUID_BROADCAST_DRIVE                                     0x00000010

// #ifndef UNICODE
// #define DefineDosDevice                                              DefineDosDeviceA
// #define QueryDosDevice                                               QueryDosDeviceA
// #endif

#define EXPAND_LOCAL_DRIVES

// #if _WIN32_WINNT >= 0x0600
// #define CreateFileTransacted                                         __MINGW_NAME_AW(CreateFileTransacted)
// #endif

// #if _WIN32_WINNT >= 0x0600
// #define SetFileAttributesTransacted                                  __MINGW_NAME_AW(SetFileAttributesTransacted)
// #define GetFileAttributesTransacted                                  __MINGW_NAME_AW(GetFileAttributesTransacted)
// #endif

// #define GetCompressedFileSize                                        __MINGW_NAME_AW(GetCompressedFileSize)

// #if _WIN32_WINNT >= 0x0600
// #define DeleteFileTransacted                                         __MINGW_NAME_AW(DeleteFileTransacted)
// #define GetCompressedFileSizeTransacted                              __MINGW_NAME_AW(GetCompressedFileSizeTransacted)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #if _WIN32_WINNT >= 0x0600
// #define FindFirstFileTransacted                                      __MINGW_NAME_AW(FindFirstFileTransacted)
// #define CopyFileTransacted                                           __MINGW_NAME_AW(CopyFileTransacted)
// #endif

// #define CheckNameLegalDOS8Dot3                                       __MINGW_NAME_AW(CheckNameLegalDOS8Dot3)
// #define CopyFile                                                     __MINGW_NAME_AW(CopyFile)
// #define CopyFileEx                                                   __MINGW_NAME_AW(CopyFileEx)

// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
// #if _WIN32_WINNT >= 0x0601
#define COPYFILE2_MESSAGE_COPY_OFFLOAD                               (0x00000001)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define MoveFile                                                     __MINGW_NAME_AW(MoveFile)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
// #define MoveFileEx                                                   __MINGW_NAME_AW(MoveFileEx)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define MoveFileWithProgress                                         __MINGW_NAME_AW(MoveFileWithProgress)
// #if _WIN32_WINNT >= 0x0600
// #define MoveFileTransacted                                           __MINGW_NAME_AW(MoveFileTransacted)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
#define MOVEFILE_REPLACE_EXISTING                                    0x00000001
#define MOVEFILE_COPY_ALLOWED                                        0x00000002
#define MOVEFILE_DELAY_UNTIL_REBOOT                                  0x00000004
#define MOVEFILE_WRITE_THROUGH                                       0x00000008
#define MOVEFILE_CREATE_HARDLINK                                     0x00000010
#define MOVEFILE_FAIL_IF_NOT_TRACKABLE                               0x00000020
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #define ReplaceFile                                                  __MINGW_NAME_AW(ReplaceFile)
// #define CreateHardLink                                               __MINGW_NAME_AW(CreateHardLink)

// #if _WIN32_WINNT >= 0x0600
// #define CreateHardLinkTransacted                                     __MINGW_NAME_AW(CreateHardLinkTransacted)
// #endif

// #if _WIN32_WINNT >= 0x0600
// #endif

// #ifndef UNICODE
// #define CreateNamedPipe                                              CreateNamedPipeA
// #define WaitNamedPipe                                                WaitNamedPipeA
// #define GetVolumeInformation                                         GetVolumeInformationA
// #endif

// #define GetNamedPipeHandleState                                      __MINGW_NAME_AW(GetNamedPipeHandleState)
// #define CallNamedPipe                                                __MINGW_NAME_AW(CallNamedPipe)
// #define SetVolumeLabel                                               __MINGW_NAME_AW(SetVolumeLabel)
// #define ClearEventLog                                                __MINGW_NAME_AW(ClearEventLog)
// #define BackupEventLog                                               __MINGW_NAME_AW(BackupEventLog)
// #define OpenEventLog                                                 __MINGW_NAME_AW(OpenEventLog)
// #define RegisterEventSource                                          __MINGW_NAME_AW(RegisterEventSource)
// #define OpenBackupEventLog                                           __MINGW_NAME_AW(OpenBackupEventLog)
// #define ReadEventLog                                                 __MINGW_NAME_AW(ReadEventLog)
// #define ReportEvent                                                  __MINGW_NAME_AW(ReportEvent)

// #if _WIN32_WINNT >= 0x0600 && !defined (UNICODE)
// #define GetNamedPipeClientComputerName                               GetNamedPipeClientComputerNameA
// #endif

#define EVENTLOG_FULL_INFO                                           0

// #if _WIN32_WINNT >= 0x0602

#define OPERATION_API_VERSION                                        1

#define OPERATION_START_TRACE_CURRENT_THREAD                         0x1

#define OPERATION_END_DISCARD                                        0x1

// #endif

// #if _WIN32_WINNT >= 0x0600
// #endif
// #if _WIN32_WINNT >= 0x0601
// #endif

// #ifndef UNICODE
// #define AccessCheckAndAuditAlarm                                     AccessCheckAndAuditAlarmA
// #define AccessCheckByTypeAndAuditAlarm                               AccessCheckByTypeAndAuditAlarmA
// #define AccessCheckByTypeResultListAndAuditAlarm                     AccessCheckByTypeResultListAndAuditAlarmA
// #define AccessCheckByTypeResultListAndAuditAlarmByHandle             AccessCheckByTypeResultListAndAuditAlarmByHandleA
// #define ObjectOpenAuditAlarm                                         ObjectOpenAuditAlarmA
// #define ObjectPrivilegeAuditAlarm                                    ObjectPrivilegeAuditAlarmA
// #define ObjectCloseAuditAlarm                                        ObjectCloseAuditAlarmA
// #define ObjectDeleteAuditAlarm                                       ObjectDeleteAuditAlarmA
// #define PrivilegedServiceAuditAlarm                                  PrivilegedServiceAuditAlarmA
// #define SetFileSecurity                                              SetFileSecurityA
// #define GetFileSecurity                                              GetFileSecurityA
// #endif

// #define IsBadStringPtr                                               __MINGW_NAME_AW(IsBadStringPtr)
// #define LookupAccountSid                                             __MINGW_NAME_AW(LookupAccountSid)
// #define LookupAccountName                                            __MINGW_NAME_AW(LookupAccountName)

// #if _WIN32_WINNT >= 0x0601

// #define LookupAccountNameLocal                                       __MINGW_NAME_AW(LookupAccountNameLocal)
// #define LookupAccountSidLocal                                        __MINGW_NAME_AW(LookupAccountSidLocal)
// #else

// #define LookupAccountNameLocalA(n,                                   s,cs,d,cd,u)LookupAccountNameA(NULL,n,s,cs,d,cd,u)
// #define LookupAccountNameLocalW(n,                                   s,cs,d,cd,u)LookupAccountNameW(NULL,n,s,cs,d,cd,u)
// #define LookupAccountNameLocal(n,                                    s,cs,d,cd,u)__MINGW_NAME_AW(LookupAccountName)(NULL,n,s,cs,d,cd,u)

// #define LookupAccountSidLocalA(s,                                    n,cn,d,cd,u)LookupAccountSidA(NULL,s,n,cn,d,cd,u)
// #define LookupAccountSidLocalW(s,                                    n,cn,d,cd,u)LookupAccountSidW(NULL,s,n,cn,d,cd,u)
// #define LookupAccountSidLocal(s,                                     n,cn,d,cd,u)__MINGW_NAME_AW(LookupAccountSid)(NULL,s,n,cn,d,cd,u)

// #endif

// #define LookupPrivilegeValue                                         __MINGW_NAME_AW(LookupPrivilegeValue)
// #define LookupPrivilegeName                                          __MINGW_NAME_AW(LookupPrivilegeName)
// #define LookupPrivilegeDisplayName                                   __MINGW_NAME_AW(LookupPrivilegeDisplayName)
// #define BuildCommDCB                                                 __MINGW_NAME_AW(BuildCommDCB)
// #define BuildCommDCBAndTimeouts                                      __MINGW_NAME_AW(BuildCommDCBAndTimeouts)
// #define CommConfigDialog                                             __MINGW_NAME_AW(CommConfigDialog)
// #define GetDefaultCommConfig                                         __MINGW_NAME_AW(GetDefaultCommConfig)
// #define SetDefaultCommConfig                                         __MINGW_NAME_AW(SetDefaultCommConfig)

#define MAX_COMPUTERNAME_LENGTH                                      15

// #ifndef UNICODE
// #define SetComputerNameEx                                            SetComputerNameExA
// #endif

// #define GetComputerName                                              __MINGW_NAME_AW(GetComputerName)
// #define SetComputerName                                              __MINGW_NAME_AW(SetComputerName)
// #define DnsHostnameToComputerName                                    __MINGW_NAME_AW(DnsHostnameToComputerName)
// #define GetUserName                                                  __MINGW_NAME_AW(GetUserName)

#define LOGON32_LOGON_INTERACTIVE                                    2
#define LOGON32_LOGON_NETWORK                                        3
#define LOGON32_LOGON_BATCH                                          4
#define LOGON32_LOGON_SERVICE                                        5
#define LOGON32_LOGON_UNLOCK                                         7
#define LOGON32_LOGON_NETWORK_CLEARTEXT                              8
#define LOGON32_LOGON_NEW_CREDENTIALS                                9

#define LOGON32_PROVIDER_DEFAULT                                     0
#define LOGON32_PROVIDER_WINNT35                                     1
#define LOGON32_PROVIDER_WINNT40                                     2
#define LOGON32_PROVIDER_WINNT50                                     3
// #if _WIN32_WINNT >= 0x0600
#define LOGON32_PROVIDER_VIRTUAL                                     4
// #endif

// #ifndef UNICODE
// #define CreateProcessAsUser                                          CreateProcessAsUserA
// #endif

// #define LogonUser                                                    __MINGW_NAME_AW(LogonUser)
// #define LogonUserEx                                                  __MINGW_NAME_AW(LogonUserEx)

#define LOGON_WITH_PROFILE                                           0x00000001
#define LOGON_NETCREDENTIALS_ONLY                                    0x00000002
#define LOGON_ZERO_PASSWORD_BUFFER                                   0x80000000

// #ifndef __WIDL__

// #ifndef UNICODE
// #define CreatePrivateNamespace                                       __MINGW_NAME_AW(CreatePrivateNamespace)
// #endif
// #define OpenPrivateNamespace                                         __MINGW_NAME_AW(OpenPrivateNamespace)
// #ifndef UNICODE
// #define CreateBoundaryDescriptor                                     __MINGW_NAME_AW(CreateBoundaryDescriptor)
// #endif

// #endif

#define HW_PROFILE_GUIDLEN                                           39
#define MAX_PROFILE_LEN                                              80

#define DOCKINFO_UNDOCKED                                            (0x1)
#define DOCKINFO_DOCKED                                              (0x2)
#define DOCKINFO_USER_SUPPLIED                                       (0x4)
#define DOCKINFO_USER_UNDOCKED                                       hb_bitor(DOCKINFO_USER_SUPPLIED, DOCKINFO_UNDOCKED)
#define DOCKINFO_USER_DOCKED                                         hb_bitor(DOCKINFO_USER_SUPPLIED, DOCKINFO_DOCKED)

// #define GetCurrentHwProfile                                          __MINGW_NAME_AW(GetCurrentHwProfile)

// #define VerifyVersionInfo                                            __MINGW_NAME_AW(VerifyVersionInfo)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define TC_NORMAL                                                    0
#define TC_HARDERR                                                   1
#define TC_GP_TRAP                                                   2
#define TC_SIGNAL                                                    3

#define AC_LINE_OFFLINE                                              0x00
#define AC_LINE_ONLINE                                               0x01
#define AC_LINE_BACKUP_POWER                                         0x02
#define AC_LINE_UNKNOWN                                              0xff

#define BATTERY_FLAG_HIGH                                            0x01
#define BATTERY_FLAG_LOW                                             0x02
#define BATTERY_FLAG_CRITICAL                                        0x04
#define BATTERY_FLAG_CHARGING                                        0x08
#define BATTERY_FLAG_NO_BATTERY                                      0x80
#define BATTERY_FLAG_UNKNOWN                                         0xff

#define BATTERY_PERCENTAGE_UNKNOWN                                   0xff

#define BATTERY_LIFE_UNKNOWN                                         0xffffffff

// #if _WIN32_WINNT >= 0x0602
#define MEHC_PATROL_SCRUBBER_PRESENT                                 0x1
// #endif

// #if _WIN32_WINNT >= 0x0600
// #endif

// #ifndef UNICODE
// #define FindFirstVolume                                              FindFirstVolumeA
// #define FindNextVolume                                               FindNextVolumeA
// #define DeleteVolumeMountPoint                                       DeleteVolumeMountPointA
// #define GetVolumeNameForVolumeMountPoint                             GetVolumeNameForVolumeMountPointA
// #define GetVolumePathName                                            GetVolumePathNameA
// #define GetVolumePathNamesForVolumeName                              GetVolumePathNamesForVolumeNameA
// #endif

// #define CreateJobObject                                              __MINGW_NAME_AW(CreateJobObject)
// #define OpenJobObject                                                __MINGW_NAME_AW(OpenJobObject)
// #define FindFirstVolumeMountPoint                                    __MINGW_NAME_AW(FindFirstVolumeMountPoint)
// #define FindNextVolumeMountPoint                                     __MINGW_NAME_AW(FindNextVolumeMountPoint)
// #define SetVolumeMountPoint                                          __MINGW_NAME_AW(SetVolumeMountPoint)

#define ACTCTX_FLAG_PROCESSOR_ARCHITECTURE_VALID                     (0x00000001)
#define ACTCTX_FLAG_LANGID_VALID                                     (0x00000002)
#define ACTCTX_FLAG_ASSEMBLY_DIRECTORY_VALID                         (0x00000004)
#define ACTCTX_FLAG_RESOURCE_NAME_VALID                              (0x00000008)
#define ACTCTX_FLAG_SET_PROCESS_DEFAULT                              (0x00000010)
#define ACTCTX_FLAG_APPLICATION_NAME_VALID                           (0x00000020)
#define ACTCTX_FLAG_SOURCE_IS_ASSEMBLYREF                            (0x00000040)
#define ACTCTX_FLAG_HMODULE_VALID                                    (0x00000080)

// #define CreateActCtx                                                 __MINGW_NAME_AW(CreateActCtx)
#define DEACTIVATE_ACTCTX_FLAG_FORCE_EARLY_DEACTIVATION              (0x00000001)

#define FIND_ACTCTX_SECTION_KEY_RETURN_HACTCTX                       (0x00000001)
#define FIND_ACTCTX_SECTION_KEY_RETURN_FLAGS                         (0x00000002)
#define FIND_ACTCTX_SECTION_KEY_RETURN_ASSEMBLY_METADATA             (0x00000004)

// #define FindActCtxSectionString                                      __MINGW_NAME_AW(FindActCtxSectionString)

// #if !defined (RC_INVOKED) && !defined (ACTIVATION_CONTEXT_BASIC_INFORMATION_DEFINED)
#define ACTIVATION_CONTEXT_BASIC_INFORMATION_DEFINED                 1
// #endif

#define QUERY_ACTCTX_FLAG_USE_ACTIVE_ACTCTX                          (0x00000004)
#define QUERY_ACTCTX_FLAG_ACTCTX_IS_HMODULE                          (0x00000008)
#define QUERY_ACTCTX_FLAG_ACTCTX_IS_ADDRESS                          (0x00000010)
#define QUERY_ACTCTX_FLAG_NO_ADDREF                                  (0x80000000)

// #if _WIN32_WINNT >= 0x0600
// #endif
// #if _WIN32_WINNT >= 0x0601
// #endif

#define RESTART_MAX_CMD_LINE                                         1024

#define RESTART_NO_CRASH                                             1
#define RESTART_NO_HANG                                              2
#define RESTART_NO_PATCH                                             4
#define RESTART_NO_REBOOT                                            8

#define RECOVERY_DEFAULT_PING_INTERVAL                               5000
#define RECOVERY_MAX_PING_INTERVAL                                   (5*60*1000)

// #if _WIN32_WINNT >= 0x0600
// #endif
// #endif

// #if _WIN32_WINNT >= 0x0600
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)

// #if _WIN32_WINNT >= 0x0602

#define STORAGE_INFO_FLAGS_ALIGNED_DEVICE                            0x00000001
#define STORAGE_INFO_FLAGS_PARTITION_ALIGNED_ON_DEVICE               0x00000002

#define STORAGE_INFO_OFFSET_UNKNOWN                                  (0xffffffff)

// #endif

#define REMOTE_PROTOCOL_INFO_FLAG_LOOPBACK                           0x00000001
#define REMOTE_PROTOCOL_INFO_FLAG_OFFLINE                            0x00000002

// #if _WIN32_WINNT >= 0x0602
#define REMOTE_PROTOCOL_INFO_FLAG_PERSISTENT_HANDLE                  0x00000004

#define RPI_FLAG_SMB2_SHARECAP_TIMEWARP                              0x00000002
#define RPI_FLAG_SMB2_SHARECAP_DFS                                   0x00000008
#define RPI_FLAG_SMB2_SHARECAP_CONTINUOUS_AVAILABILITY               0x00000010
#define RPI_FLAG_SMB2_SHARECAP_SCALEOUT                              0x00000020
#define RPI_FLAG_SMB2_SHARECAP_CLUSTER                               0x00000040

#define RPI_SMB2_FLAG_SERVERCAP_DFS                                  0x00000001
#define RPI_SMB2_FLAG_SERVERCAP_LEASING                              0x00000002
#define RPI_SMB2_FLAG_SERVERCAP_LARGEMTU                             0x00000004
#define RPI_SMB2_FLAG_SERVERCAP_MULTICHANNEL                         0x00000008
#define RPI_SMB2_FLAG_SERVERCAP_PERSISTENT_HANDLES                   0x00000010
#define RPI_SMB2_FLAG_SERVERCAP_DIRECTORY_LEASING                    0x00000020
// #endif

// #if _WIN32_WINNT < 0x0602
// #else
// #endif

// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #if _WIN32_WINNT >= 0x0602
// #endif

// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT >= 0x0600

#define SYMBOLIC_LINK_FLAG_DIRECTORY                                 (0x1)
#define SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE                 (0x2)

#define VALID_SYMBOLIC_LINK_FLAGS                                    SYMBOLIC_LINK_FLAG_DIRECTORY

// #define CreateSymbolicLink                                           __MINGW_NAME_AW(CreateSymbolicLink)
// #define CreateSymbolicLinkTransacted                                 __MINGW_NAME_AW(CreateSymbolicLinkTransacted)

// #endif
// #endif

// #if NTDDI_VERSION >= NTDDI_WIN7SP1

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_APP)
// #if defined (__x86_64__) || defined (__i386__)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if defined (__x86_64__) || defined (__i386__)
// #endif
// #if _WIN32_WINNT >= 0x0601
// #endif
// #endif
// #endif

// #if !defined (RC_INVOKED) && !defined (NOWINBASEINTERLOCK) && !defined (_NTOS_) && !defined (MICROSOFT_WINDOWS_WINBASE_INTERLOCKED_CPLUSPLUS_H_INCLUDED)
// #define MICROSOFT_WINDOWS_WINBASE_INTERLOCKED_CPLUSPLUS_H_INCLUDED
// #if !defined (__WIDL__)
// #if !defined (MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS)
// #define MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVE (_WIN32_WINNT>=0x0502||!defined(_WINBASE_))
// #endif
// #if MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS && defined (__cplusplus)
// #if defined(__x86_64__) && defined(__CYGWIN__)
// #define __MINGW_USE_INT64_INTERLOCKED_LONG
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #if defined (_WIN64) || ((_WIN32_WINNT >= 0x0502) && defined (_WINBASE_))
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #if defined (_WIN64) || ((_WIN32_WINNT >= 0x0502) && defined (_WINBASE_))
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #if defined (_WIN64) || ((_WIN32_WINNT >= 0x0502) && defined (_WINBASE_))
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #if defined (_WIN64) || ((_WIN32_WINNT >= 0x0502) && defined (_WINBASE_))
// #endif

// #ifndef __MINGW_USE_INT64_INTERLOCKED_LONG
// #else
// #endif

// #if defined (_WIN64) || ((_WIN32_WINNT >= 0x0502) && defined (_WINBASE_))
// #endif
// #endif

// #undef MICROSOFT_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS
// #define MICROSOFT_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS   0
// #endif

#endif // _WINAPI_WINBASE_
