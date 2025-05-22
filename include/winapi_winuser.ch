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

#ifndef _WINAPI_WINUSER_
#define _WINAPI_WINUSER_

// #ifndef WINVER
// #define WINVER                                                       0x0502
// #endif

// #ifndef NOAPISET
// #endif

#ifndef NOUSER

#ifdef STRICT
#else
#endif

// #ifdef STRICT
// #else
// #endif

// #define IS_INTRESOURCE(_r)                                           ((((ULONG_PTR)(_r))>>16)==0)
// #define MAKEINTRESOURCEA(i)                                          ((LPSTR)((ULONG_PTR)((WORD)(i))))
// #define MAKEINTRESOURCEW(i)                                          ((LPWSTR)((ULONG_PTR)((WORD)(i))))
// #define MAKEINTRESOURCE                                              __MINGW_NAME_AW(MAKEINTRESOURCE)

#ifndef MAKEINTRESOURCE
#define MAKEINTRESOURCE(n)   (n)
#endif

#ifndef NORESOURCE
// #define RT_CURSOR                                                    MAKEINTRESOURCE(1)
// #define RT_BITMAP                                                    MAKEINTRESOURCE(2)
// #define RT_ICON                                                      MAKEINTRESOURCE(3)
// #define RT_MENU                                                      MAKEINTRESOURCE(4)
// #define RT_DIALOG                                                    MAKEINTRESOURCE(5)
// #define RT_STRING                                                    MAKEINTRESOURCE(6)
// #define RT_FONTDIR                                                   MAKEINTRESOURCE(7)
// #define RT_FONT                                                      MAKEINTRESOURCE(8)
// #define RT_ACCELERATOR                                               MAKEINTRESOURCE(9)
// #define RT_RCDATA                                                    MAKEINTRESOURCE(10)
// #define RT_MESSAGETABLE                                              MAKEINTRESOURCE(11)

#define DIFFERENCE                                                   11
// #define RT_GROUP_CURSOR                                              MAKEINTRESOURCE((ULONG_PTR)(RT_CURSOR)+DIFFERENCE)
// #define RT_GROUP_ICON                                                MAKEINTRESOURCE((ULONG_PTR)(RT_ICON)+DIFFERENCE)
// #define RT_VERSION                                                   MAKEINTRESOURCE(16)
// #define RT_DLGINCLUDE                                                MAKEINTRESOURCE(17)
// #define RT_PLUGPLAY                                                  MAKEINTRESOURCE(19)
// #define RT_VXD                                                       MAKEINTRESOURCE(20)
// #define RT_ANICURSOR                                                 MAKEINTRESOURCE(21)
// #define RT_ANIICON                                                   MAKEINTRESOURCE(22)
// #define RT_HTML                                                      MAKEINTRESOURCE(23)
#ifdef RC_INVOKED
#define RT_MANIFEST                                                  24

#define CREATEPROCESS_MANIFEST_RESOURCE_ID                           1
#define ISOLATIONAWARE_MANIFEST_RESOURCE_ID                          2
#define ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID           3
#define MINIMUM_RESERVED_MANIFEST_RESOURCE_ID                        1
#define MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID                        16
#else
// #define RT_MANIFEST                                                  MAKEINTRESOURCE(24)

// #define CREATEPROCESS_MANIFEST_RESOURCE_ID                           MAKEINTRESOURCE(1)
// #define ISOLATIONAWARE_MANIFEST_RESOURCE_ID                          MAKEINTRESOURCE(2)
// #define ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID           MAKEINTRESOURCE(3)
// #define MINIMUM_RESERVED_MANIFEST_RESOURCE_ID                        MAKEINTRESOURCE(1)
// #define MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID                        MAKEINTRESOURCE(16)
#endif
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define wvsprintf                                                    __MINGW_NAME_AW(wvsprintf)
// #define wsprintf                                                     __MINGW_NAME_AW(wsprintf)
// #endif

// #define SETWALLPAPER_DEFAULT                                         ((LPWSTR)-1)

#ifndef NOSCROLL
#define SB_HORZ                                                      0
#define SB_VERT                                                      1
#define SB_CTL                                                       2
#define SB_BOTH                                                      3

#define SB_LINEUP                                                    0
#define SB_LINELEFT                                                  0
#define SB_LINEDOWN                                                  1
#define SB_LINERIGHT                                                 1
#define SB_PAGEUP                                                    2
#define SB_PAGELEFT                                                  2
#define SB_PAGEDOWN                                                  3
#define SB_PAGERIGHT                                                 3
#define SB_THUMBPOSITION                                             4
#define SB_THUMBTRACK                                                5
#define SB_TOP                                                       6
#define SB_LEFT                                                      6
#define SB_BOTTOM                                                    7
#define SB_RIGHT                                                     7
#define SB_ENDSCROLL                                                 8
#endif

#ifndef NOSHOWWINDOW
#define SW_HIDE                                                      0
#define SW_SHOWNORMAL                                                1
#define SW_NORMAL                                                    1
#define SW_SHOWMINIMIZED                                             2
#define SW_SHOWMAXIMIZED                                             3
#define SW_MAXIMIZE                                                  3
#define SW_SHOWNOACTIVATE                                            4
#define SW_SHOW                                                      5
#define SW_MINIMIZE                                                  6
#define SW_SHOWMINNOACTIVE                                           7
#define SW_SHOWNA                                                    8
#define SW_RESTORE                                                   9
#define SW_SHOWDEFAULT                                               10
#define SW_FORCEMINIMIZE                                             11
#define SW_MAX                                                       11

#define HIDE_WINDOW                                                  0
#define SHOW_OPENWINDOW                                              1
#define SHOW_ICONWINDOW                                              2
#define SHOW_FULLSCREEN                                              3
#define SHOW_OPENNOACTIVATE                                          4

#define SW_PARENTCLOSING                                             1
#define SW_OTHERZOOM                                                 2
#define SW_PARENTOPENING                                             3
#define SW_OTHERUNZOOM                                               4
#endif

#define AW_HOR_POSITIVE                                              0x00000001
#define AW_HOR_NEGATIVE                                              0x00000002
#define AW_VER_POSITIVE                                              0x00000004
#define AW_VER_NEGATIVE                                              0x00000008
#define AW_CENTER                                                    0x00000010
#define AW_HIDE                                                      0x00010000
#define AW_ACTIVATE                                                  0x00020000
#define AW_SLIDE                                                     0x00040000
#define AW_BLEND                                                     0x00080000

#define KF_EXTENDED                                                  0x0100
#define KF_DLGMODE                                                   0x0800
#define KF_MENUMODE                                                  0x1000
#define KF_ALTDOWN                                                   0x2000
#define KF_REPEAT                                                    0x4000
#define KF_UP                                                        0x8000

#ifndef NOVIRTUALKEYCODES

#define VK_LBUTTON                                                   0x01
#define VK_RBUTTON                                                   0x02
#define VK_CANCEL                                                    0x03
#define VK_MBUTTON                                                   0x04
#define VK_XBUTTON1                                                  0x05
#define VK_XBUTTON2                                                  0x06
#define VK_BACK                                                      0x08
#define VK_TAB                                                       0x09
#define VK_CLEAR                                                     0x0C
#define VK_RETURN                                                    0x0D
#define VK_SHIFT                                                     0x10
#define VK_CONTROL                                                   0x11
#define VK_MENU                                                      0x12
#define VK_PAUSE                                                     0x13
#define VK_CAPITAL                                                   0x14
#define VK_KANA                                                      0x15
#define VK_HANGEUL                                                   0x15
#define VK_HANGUL                                                    0x15
#define VK_JUNJA                                                     0x17
#define VK_FINAL                                                     0x18
#define VK_HANJA                                                     0x19
#define VK_KANJI                                                     0x19
#define VK_ESCAPE                                                    0x1B
#define VK_CONVERT                                                   0x1C
#define VK_NONCONVERT                                                0x1D
#define VK_ACCEPT                                                    0x1E
#define VK_MODECHANGE                                                0x1F
#define VK_SPACE                                                     0x20
#define VK_PRIOR                                                     0x21
#define VK_NEXT                                                      0x22
#define VK_END                                                       0x23
#define VK_HOME                                                      0x24
#define VK_LEFT                                                      0x25
#define VK_UP                                                        0x26
#define VK_RIGHT                                                     0x27
#define VK_DOWN                                                      0x28
#define VK_SELECT                                                    0x29
#define VK_PRINT                                                     0x2A
#define VK_EXECUTE                                                   0x2B
#define VK_SNAPSHOT                                                  0x2C
#define VK_INSERT                                                    0x2D
#define VK_DELETE                                                    0x2E
#define VK_HELP                                                      0x2F

#define VK_LWIN                                                      0x5B
#define VK_RWIN                                                      0x5C
#define VK_APPS                                                      0x5D
#define VK_SLEEP                                                     0x5F
#define VK_NUMPAD0                                                   0x60
#define VK_NUMPAD1                                                   0x61
#define VK_NUMPAD2                                                   0x62
#define VK_NUMPAD3                                                   0x63
#define VK_NUMPAD4                                                   0x64
#define VK_NUMPAD5                                                   0x65
#define VK_NUMPAD6                                                   0x66
#define VK_NUMPAD7                                                   0x67
#define VK_NUMPAD8                                                   0x68
#define VK_NUMPAD9                                                   0x69
#define VK_MULTIPLY                                                  0x6A
#define VK_ADD                                                       0x6B
#define VK_SEPARATOR                                                 0x6C
#define VK_SUBTRACT                                                  0x6D
#define VK_DECIMAL                                                   0x6E
#define VK_DIVIDE                                                    0x6F
#define VK_F1                                                        0x70
#define VK_F2                                                        0x71
#define VK_F3                                                        0x72
#define VK_F4                                                        0x73
#define VK_F5                                                        0x74
#define VK_F6                                                        0x75
#define VK_F7                                                        0x76
#define VK_F8                                                        0x77
#define VK_F9                                                        0x78
#define VK_F10                                                       0x79
#define VK_F11                                                       0x7A
#define VK_F12                                                       0x7B
#define VK_F13                                                       0x7C
#define VK_F14                                                       0x7D
#define VK_F15                                                       0x7E
#define VK_F16                                                       0x7F
#define VK_F17                                                       0x80
#define VK_F18                                                       0x81
#define VK_F19                                                       0x82
#define VK_F20                                                       0x83
#define VK_F21                                                       0x84
#define VK_F22                                                       0x85
#define VK_F23                                                       0x86
#define VK_F24                                                       0x87
#define VK_NUMLOCK                                                   0x90
#define VK_SCROLL                                                    0x91
#define VK_OEM_NEC_EQUAL                                             0x92
#define VK_OEM_FJ_JISHO                                              0x92
#define VK_OEM_FJ_MASSHOU                                            0x93
#define VK_OEM_FJ_TOUROKU                                            0x94
#define VK_OEM_FJ_LOYA                                               0x95
#define VK_OEM_FJ_ROYA                                               0x96
#define VK_LSHIFT                                                    0xA0
#define VK_RSHIFT                                                    0xA1
#define VK_LCONTROL                                                  0xA2
#define VK_RCONTROL                                                  0xA3
#define VK_LMENU                                                     0xA4
#define VK_RMENU                                                     0xA5
#define VK_BROWSER_BACK                                              0xA6
#define VK_BROWSER_FORWARD                                           0xA7
#define VK_BROWSER_REFRESH                                           0xA8
#define VK_BROWSER_STOP                                              0xA9
#define VK_BROWSER_SEARCH                                            0xAA
#define VK_BROWSER_FAVORITES                                         0xAB
#define VK_BROWSER_HOME                                              0xAC
#define VK_VOLUME_MUTE                                               0xAD
#define VK_VOLUME_DOWN                                               0xAE
#define VK_VOLUME_UP                                                 0xAF
#define VK_MEDIA_NEXT_TRACK                                          0xB0
#define VK_MEDIA_PREV_TRACK                                          0xB1
#define VK_MEDIA_STOP                                                0xB2
#define VK_MEDIA_PLAY_PAUSE                                          0xB3
#define VK_LAUNCH_MAIL                                               0xB4
#define VK_LAUNCH_MEDIA_SELECT                                       0xB5
#define VK_LAUNCH_APP1                                               0xB6
#define VK_LAUNCH_APP2                                               0xB7
#define VK_OEM_1                                                     0xBA
#define VK_OEM_PLUS                                                  0xBB
#define VK_OEM_COMMA                                                 0xBC
#define VK_OEM_MINUS                                                 0xBD
#define VK_OEM_PERIOD                                                0xBE
#define VK_OEM_2                                                     0xBF
#define VK_OEM_3                                                     0xC0
#define VK_OEM_4                                                     0xDB
#define VK_OEM_5                                                     0xDC
#define VK_OEM_6                                                     0xDD
#define VK_OEM_7                                                     0xDE
#define VK_OEM_8                                                     0xDF
#define VK_OEM_AX                                                    0xE1
#define VK_OEM_102                                                   0xE2
#define VK_ICO_HELP                                                  0xE3
#define VK_ICO_00                                                    0xE4
#define VK_PROCESSKEY                                                0xE5
#define VK_ICO_CLEAR                                                 0xE6
#define VK_PACKET                                                    0xE7
#define VK_OEM_RESET                                                 0xE9
#define VK_OEM_JUMP                                                  0xEA
#define VK_OEM_PA1                                                   0xEB
#define VK_OEM_PA2                                                   0xEC
#define VK_OEM_PA3                                                   0xED
#define VK_OEM_WSCTRL                                                0xEE
#define VK_OEM_CUSEL                                                 0xEF
#define VK_OEM_ATTN                                                  0xF0
#define VK_OEM_FINISH                                                0xF1
#define VK_OEM_COPY                                                  0xF2
#define VK_OEM_AUTO                                                  0xF3
#define VK_OEM_ENLW                                                  0xF4
#define VK_OEM_BACKTAB                                               0xF5
#define VK_ATTN                                                      0xF6
#define VK_CRSEL                                                     0xF7
#define VK_EXSEL                                                     0xF8
#define VK_EREOF                                                     0xF9
#define VK_PLAY                                                      0xFA
#define VK_ZOOM                                                      0xFB
#define VK_NONAME                                                    0xFC
#define VK_PA1                                                       0xFD
#define VK_OEM_CLEAR                                                 0xFE
#endif

#ifndef NOWH

#define WH_MIN                                                       (-1)
#define WH_MSGFILTER                                                 (-1)
#define WH_JOURNALRECORD                                             0
#define WH_JOURNALPLAYBACK                                           1
#define WH_KEYBOARD                                                  2
#define WH_GETMESSAGE                                                3
#define WH_CALLWNDPROC                                               4
#define WH_CBT                                                       5
#define WH_SYSMSGFILTER                                              6
#define WH_MOUSE                                                     7
#define WH_HARDWARE                                                  8
#define WH_DEBUG                                                     9
#define WH_SHELL                                                     10
#define WH_FOREGROUNDIDLE                                            11
#define WH_CALLWNDPROCRET                                            12

#define WH_KEYBOARD_LL                                               13
#define WH_MOUSE_LL                                                  14

#define WH_MAX                                                       14

#define WH_MINHOOK                                                   WH_MIN
#define WH_MAXHOOK                                                   WH_MAX

#define HC_ACTION                                                    0
#define HC_GETNEXT                                                   1
#define HC_SKIP                                                      2
#define HC_NOREMOVE                                                  3
#define HC_NOREM                                                     HC_NOREMOVE
#define HC_SYSMODALON                                                4
#define HC_SYSMODALOFF                                               5

#define HCBT_MOVESIZE                                                0
#define HCBT_MINMAX                                                  1
#define HCBT_QS                                                      2
#define HCBT_CREATEWND                                               3
#define HCBT_DESTROYWND                                              4
#define HCBT_ACTIVATE                                                5
#define HCBT_CLICKSKIPPED                                            6
#define HCBT_KEYSKIPPED                                              7
#define HCBT_SYSCOMMAND                                              8
#define HCBT_SETFOCUS                                                9

#define WTS_CONSOLE_CONNECT                                          0x1
#define WTS_CONSOLE_DISCONNECT                                       0x2
#define WTS_REMOTE_CONNECT                                           0x3
#define WTS_REMOTE_DISCONNECT                                        0x4
#define WTS_SESSION_LOGON                                            0x5
#define WTS_SESSION_LOGOFF                                           0x6
#define WTS_SESSION_LOCK                                             0x7
#define WTS_SESSION_UNLOCK                                           0x8
#define WTS_SESSION_REMOTE_CONTROL                                   0x9
#define WTS_SESSION_CREATE                                           0xa
#define WTS_SESSION_TERMINATE                                        0xb

#define MSGF_DIALOGBOX                                               0
#define MSGF_MESSAGEBOX                                              1
#define MSGF_MENU                                                    2
#define MSGF_SCROLLBAR                                               5
#define MSGF_NEXTWINDOW                                              6
#define MSGF_MAX                                                     8
#define MSGF_USER                                                    4096

#define HSHELL_WINDOWCREATED                                         1
#define HSHELL_WINDOWDESTROYED                                       2
#define HSHELL_ACTIVATESHELLWINDOW                                   3
#define HSHELL_WINDOWACTIVATED                                       4
#define HSHELL_GETMINRECT                                            5
#define HSHELL_REDRAW                                                6
#define HSHELL_TASKMAN                                               7
#define HSHELL_LANGUAGE                                              8
#define HSHELL_SYSMENU                                               9
#define HSHELL_ENDTASK                                               10
#define HSHELL_ACCESSIBILITYSTATE                                    11
#define HSHELL_APPCOMMAND                                            12
#define HSHELL_WINDOWREPLACED                                        13
#define HSHELL_WINDOWREPLACING                                       14
// #if _WIN32_WINNT >= 0x0602
#define HSHELL_MONITORCHANGED                                        16
// #endif

#define HSHELL_HIGHBIT                                               0x8000
#define HSHELL_FLASH                                                 hb_bit(HSHELL_REDRAW, HSHELL_HIGHBIT)
#define HSHELL_RUDEAPPACTIVATED                                      hb_bit(HSHELL_WINDOWACTIVATED, HSHELL_HIGHBIT)

#define ACCESS_STICKYKEYS                                            0x0001
#define ACCESS_FILTERKEYS                                            0x0002
#define ACCESS_MOUSEKEYS                                             0x0003

#define APPCOMMAND_BROWSER_BACKWARD                                  1
#define APPCOMMAND_BROWSER_FORWARD                                   2
#define APPCOMMAND_BROWSER_REFRESH                                   3
#define APPCOMMAND_BROWSER_STOP                                      4
#define APPCOMMAND_BROWSER_SEARCH                                    5
#define APPCOMMAND_BROWSER_FAVORITES                                 6
#define APPCOMMAND_BROWSER_HOME                                      7
#define APPCOMMAND_VOLUME_MUTE                                       8
#define APPCOMMAND_VOLUME_DOWN                                       9
#define APPCOMMAND_VOLUME_UP                                         10
#define APPCOMMAND_MEDIA_NEXTTRACK                                   11
#define APPCOMMAND_MEDIA_PREVIOUSTRACK                               12
#define APPCOMMAND_MEDIA_STOP                                        13
#define APPCOMMAND_MEDIA_PLAY_PAUSE                                  14
#define APPCOMMAND_LAUNCH_MAIL                                       15
#define APPCOMMAND_LAUNCH_MEDIA_SELECT                               16
#define APPCOMMAND_LAUNCH_APP1                                       17
#define APPCOMMAND_LAUNCH_APP2                                       18
#define APPCOMMAND_BASS_DOWN                                         19
#define APPCOMMAND_BASS_BOOST                                        20
#define APPCOMMAND_BASS_UP                                           21
#define APPCOMMAND_TREBLE_DOWN                                       22
#define APPCOMMAND_TREBLE_UP                                         23
#define APPCOMMAND_MICROPHONE_VOLUME_MUTE                            24
#define APPCOMMAND_MICROPHONE_VOLUME_DOWN                            25
#define APPCOMMAND_MICROPHONE_VOLUME_UP                              26
#define APPCOMMAND_HELP                                              27
#define APPCOMMAND_FIND                                              28
#define APPCOMMAND_NEW                                               29
#define APPCOMMAND_OPEN                                              30
#define APPCOMMAND_CLOSE                                             31
#define APPCOMMAND_SAVE                                              32
#define APPCOMMAND_PRINT                                             33
#define APPCOMMAND_UNDO                                              34
#define APPCOMMAND_REDO                                              35
#define APPCOMMAND_COPY                                              36
#define APPCOMMAND_CUT                                               37
#define APPCOMMAND_PASTE                                             38
#define APPCOMMAND_REPLY_TO_MAIL                                     39
#define APPCOMMAND_FORWARD_MAIL                                      40
#define APPCOMMAND_SEND_MAIL                                         41
#define APPCOMMAND_SPELL_CHECK                                       42
#define APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE                 43
#define APPCOMMAND_MIC_ON_OFF_TOGGLE                                 44
#define APPCOMMAND_CORRECTION_LIST                                   45
#define APPCOMMAND_MEDIA_PLAY                                        46
#define APPCOMMAND_MEDIA_PAUSE                                       47
#define APPCOMMAND_MEDIA_RECORD                                      48
#define APPCOMMAND_MEDIA_FAST_FORWARD                                49
#define APPCOMMAND_MEDIA_REWIND                                      50
#define APPCOMMAND_MEDIA_CHANNEL_UP                                  51
#define APPCOMMAND_MEDIA_CHANNEL_DOWN                                52
// #if _WIN32_WINNT >= 0x0600
#define APPCOMMAND_DELETE                                            53
#define APPCOMMAND_DWM_FLIP3D                                        54
// #endif

#define FAPPCOMMAND_MOUSE                                            0x8000
#define FAPPCOMMAND_KEY                                              0
#define FAPPCOMMAND_OEM                                              0x1000
#define FAPPCOMMAND_MASK                                             0xF000

// #define GET_APPCOMMAND_LPARAM(lParam)                                ((short)(HIWORD(lParam)&~FAPPCOMMAND_MASK))
// #define GET_DEVICE_LPARAM(lParam)                                    ((WORD)(HIWORD(lParam)&FAPPCOMMAND_MASK))
// #define GET_MOUSEORKEY_LPARAM                                        GET_DEVICE_LPARAM
// #define GET_FLAGS_LPARAM(lParam)                                     (LOWORD(lParam))
// #define GET_KEYSTATE_LPARAM(lParam)                                  GET_FLAGS_LPARAM(lParam)

#define LLKHF_EXTENDED                                               hb_BitShift(KF_EXTENDED, -8)
#define LLKHF_INJECTED                                               0x00000010
#define LLKHF_ALTDOWN                                                hb_BitShift(KF_ALTDOWN, -8)
#define LLKHF_UP                                                     hb_BitShift(KF_UP, -8)

#define LLMHF_INJECTED                                               0x00000001

#endif

#define HKL_PREV                                                     0
#define HKL_NEXT                                                     1

#define KLF_ACTIVATE                                                 0x00000001
#define KLF_SUBSTITUTE_OK                                            0x00000002
#define KLF_REORDER                                                  0x00000008
#define KLF_REPLACELANG                                              0x00000010
#define KLF_NOTELLSHELL                                              0x00000080
#define KLF_SETFORPROCESS                                            0x00000100
#define KLF_SHIFTLOCK                                                0x00010000
#define KLF_RESET                                                    0x40000000

#define INPUTLANGCHANGE_SYSCHARSET                                   0x0001
#define INPUTLANGCHANGE_FORWARD                                      0x0002
#define INPUTLANGCHANGE_BACKWARD                                     0x0004

#define KL_NAMELENGTH                                                9

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadKeyboardLayout                                           __MINGW_NAME_AW(LoadKeyboardLayout)
// #define GetKeyboardLayoutName                                        __MINGW_NAME_AW(GetKeyboardLayoutName)
// #endif

#define GMMP_USE_DISPLAY_POINTS                                      1
#define GMMP_USE_HIGH_RESOLUTION_POINTS                              2

#ifndef NODESKTOP

#define DESKTOP_READOBJECTS                                          0x0001
#define DESKTOP_CREATEWINDOW                                         0x0002
#define DESKTOP_CREATEMENU                                           0x0004
#define DESKTOP_HOOKCONTROL                                          0x0008
#define DESKTOP_JOURNALRECORD                                        0x0010
#define DESKTOP_JOURNALPLAYBACK                                      0x0020
#define DESKTOP_ENUMERATE                                            0x0040
#define DESKTOP_WRITEOBJECTS                                         0x0080
#define DESKTOP_SWITCHDESKTOP                                        0x0100

#define DF_ALLOWOTHERACCOUNTHOOK                                     0x0001

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #ifdef _WINGDI_
// #ifndef NOGDI
// #define CreateDesktop                                                __MINGW_NAME_AW(CreateDesktop)
// #define CreateDesktopEx                                              __MINGW_NAME_AW(CreateDesktopEx)
// #endif
// #endif

// #define OpenDesktop                                                  __MINGW_NAME_AW(OpenDesktop)
// #define EnumDesktops                                                 __MINGW_NAME_AW(EnumDesktops)

// #endif
#endif

#ifndef NOWINDOWSTATION
#define WINSTA_ENUMDESKTOPS                                          0x0001
#define WINSTA_READATTRIBUTES                                        0x0002
#define WINSTA_ACCESSCLIPBOARD                                       0x0004
#define WINSTA_CREATEDESKTOP                                         0x0008
#define WINSTA_WRITEATTRIBUTES                                       0x0010
#define WINSTA_ACCESSGLOBALATOMS                                     0x0020
#define WINSTA_EXITWINDOWS                                           0x0040
#define WINSTA_ENUMERATE                                             0x0100
#define WINSTA_READSCREEN                                            0x0200
#define WINSTA_ALL_ACCESS                                            hb_bitor(WINSTA_ENUMDESKTOPS, WINSTA_READATTRIBUTES, WINSTA_ACCESSCLIPBOARD, WINSTA_CREATEDESKTOP, WINSTA_WRITEATTRIBUTES, WINSTA_ACCESSGLOBALATOMS, WINSTA_EXITWINDOWS, WINSTA_ENUMERATE, WINSTA_READSCREEN)

#define CWF_CREATE_ONLY                                              0x00000001

#define WSF_VISIBLE                                                  0x0001

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define CreateWindowStation                                          __MINGW_NAME_AW(CreateWindowStation)
// #define OpenWindowStation                                            __MINGW_NAME_AW(OpenWindowStation)
// #define EnumWindowStations                                           __MINGW_NAME_AW(EnumWindowStations)
// #endif
#endif

#ifndef NOSECURITY

#define UOI_FLAGS                                                    1
#define UOI_NAME                                                     2
#define UOI_TYPE                                                     3
#define UOI_USER_SID                                                 4
// #if WINVER >= 0x0600
#define UOI_HEAPSIZE                                                 5
#define UOI_IO                                                       6
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetUserObjectInformation                                     __MINGW_NAME_AW(GetUserObjectInformation)
// #define SetUserObjectInformation                                     __MINGW_NAME_AW(SetUserObjectInformation)
// #endif
#endif

#ifndef NOMSG

// #define POINTSTOPOINT(pt,pts)                                        {(pt).x=(LONG)(SHORT)LOWORD(*(LONG*)&pts);(pt).y=(LONG)(SHORT)HIWORD(*(LONG*)&pts);}

// #define POINTTOPOINTS(pt)                                            (MAKELONG((short)((pt).x),(short)((pt).y)))
// #define MAKEWPARAM(l,h)                                              ((WPARAM)(DWORD)MAKELONG(l,h))
// #define MAKELPARAM(l,h)                                              ((LPARAM)(DWORD)MAKELONG(l,h))
// #define MAKELRESULT(l,h)                                             ((LRESULT)(DWORD)MAKELONG(l,h))
#endif

#ifndef NOWINOFFSETS
#define GWL_WNDPROC                                                  (-4)
#define GWL_HINSTANCE                                                (-6)
#define GWL_HWNDPARENT                                               (-8)
#define GWL_STYLE                                                    (-16)
#define GWL_EXSTYLE                                                  (-20)
#define GWL_USERDATA                                                 (-21)
#define GWL_ID                                                       (-12)

#ifdef _WIN64
#undef GWL_WNDPROC
#undef GWL_HINSTANCE
#undef GWL_HWNDPARENT
#undef GWL_USERDATA
#endif

#define GWLP_WNDPROC                                                 (-4)
#define GWLP_HINSTANCE                                               (-6)
#define GWLP_HWNDPARENT                                              (-8)
#define GWLP_USERDATA                                                (-21)
#define GWLP_ID                                                      (-12)

#define GCL_MENUNAME                                                 (-8)
#define GCL_HBRBACKGROUND                                            (-10)
#define GCL_HCURSOR                                                  (-12)
#define GCL_HICON                                                    (-14)
#define GCL_HMODULE                                                  (-16)
#define GCL_CBWNDEXTRA                                               (-18)
#define GCL_CBCLSEXTRA                                               (-20)
#define GCL_WNDPROC                                                  (-24)
#define GCL_STYLE                                                    (-26)
#define GCW_ATOM                                                     (-32)
#define GCL_HICONSM                                                  (-34)

#ifdef _WIN64
#undef GCL_MENUNAME
#undef GCL_HBRBACKGROUND
#undef GCL_HCURSOR
#undef GCL_HICON
#undef GCL_HMODULE
#undef GCL_WNDPROC
#undef GCL_HICONSM
#endif

#define GCLP_MENUNAME                                                (-8)
#define GCLP_HBRBACKGROUND                                           (-10)
#define GCLP_HCURSOR                                                 (-12)
#define GCLP_HICON                                                   (-14)
#define GCLP_HMODULE                                                 (-16)
#define GCLP_WNDPROC                                                 (-24)
#define GCLP_HICONSM                                                 (-34)
#endif

#ifndef NOWINMESSAGES

#define WM_NULL                                                      0x0000
#define WM_CREATE                                                    0x0001
#define WM_DESTROY                                                   0x0002
#define WM_MOVE                                                      0x0003
#define WM_SIZE                                                      0x0005

#define WM_ACTIVATE                                                  0x0006

#define WA_INACTIVE                                                  0
#define WA_ACTIVE                                                    1
#define WA_CLICKACTIVE                                               2

#define WM_SETFOCUS                                                  0x0007
#define WM_KILLFOCUS                                                 0x0008
#define WM_ENABLE                                                    0x000A
#define WM_SETREDRAW                                                 0x000B
#define WM_SETTEXT                                                   0x000C
#define WM_GETTEXT                                                   0x000D
#define WM_GETTEXTLENGTH                                             0x000E
#define WM_PAINT                                                     0x000F
#define WM_CLOSE                                                     0x0010
// #ifndef _WIN32_WCE
#define WM_QUERYENDSESSION                                           0x0011
#define WM_QUERYOPEN                                                 0x0013
#define WM_ENDSESSION                                                0x0016
// #endif
#define WM_QUIT                                                      0x0012
#define WM_ERASEBKGND                                                0x0014
#define WM_SYSCOLORCHANGE                                            0x0015
#define WM_SHOWWINDOW                                                0x0018
#define WM_WININICHANGE                                              0x001A
#define WM_SETTINGCHANGE                                             WM_WININICHANGE
#define WM_DEVMODECHANGE                                             0x001B
#define WM_ACTIVATEAPP                                               0x001C
#define WM_FONTCHANGE                                                0x001D
#define WM_TIMECHANGE                                                0x001E
#define WM_CANCELMODE                                                0x001F
#define WM_SETCURSOR                                                 0x0020
#define WM_MOUSEACTIVATE                                             0x0021
#define WM_CHILDACTIVATE                                             0x0022
#define WM_QUEUESYNC                                                 0x0023
#define WM_GETMINMAXINFO                                             0x0024

#define WM_PAINTICON                                                 0x0026
#define WM_ICONERASEBKGND                                            0x0027
#define WM_NEXTDLGCTL                                                0x0028
#define WM_SPOOLERSTATUS                                             0x002A
#define WM_DRAWITEM                                                  0x002B
#define WM_MEASUREITEM                                               0x002C
#define WM_DELETEITEM                                                0x002D
#define WM_VKEYTOITEM                                                0x002E
#define WM_CHARTOITEM                                                0x002F
#define WM_SETFONT                                                   0x0030
#define WM_GETFONT                                                   0x0031
#define WM_SETHOTKEY                                                 0x0032
#define WM_GETHOTKEY                                                 0x0033
#define WM_QUERYDRAGICON                                             0x0037
#define WM_COMPAREITEM                                               0x0039
// #ifndef _WIN32_WCE
#define WM_GETOBJECT                                                 0x003D
// #endif
#define WM_COMPACTING                                                0x0041
#define WM_COMMNOTIFY                                                0x0044
#define WM_WINDOWPOSCHANGING                                         0x0046
#define WM_WINDOWPOSCHANGED                                          0x0047
#define WM_POWER                                                     0x0048

#define PWR_OK                                                       1
#define PWR_FAIL                                                     (-1)
#define PWR_SUSPENDREQUEST                                           1
#define PWR_SUSPENDRESUME                                            2
#define PWR_CRITICALRESUME                                           3

#define WM_COPYDATA                                                  0x004A
#define WM_CANCELJOURNAL                                             0x004B

#define WM_NOTIFY                                                    0x004E
#define WM_INPUTLANGCHANGEREQUEST                                    0x0050
#define WM_INPUTLANGCHANGE                                           0x0051
#define WM_TCARD                                                     0x0052
#define WM_HELP                                                      0x0053
#define WM_USERCHANGED                                               0x0054
#define WM_NOTIFYFORMAT                                              0x0055

#define NFR_ANSI                                                     1
#define NFR_UNICODE                                                  2
#define NF_QUERY                                                     3
#define NF_REQUERY                                                   4

#define WM_CONTEXTMENU                                               0x007B
#define WM_STYLECHANGING                                             0x007C
#define WM_STYLECHANGED                                              0x007D
#define WM_DISPLAYCHANGE                                             0x007E
#define WM_GETICON                                                   0x007F
#define WM_SETICON                                                   0x0080

#define WM_NCCREATE                                                  0x0081
#define WM_NCDESTROY                                                 0x0082
#define WM_NCCALCSIZE                                                0x0083
#define WM_NCHITTEST                                                 0x0084
#define WM_NCPAINT                                                   0x0085
#define WM_NCACTIVATE                                                0x0086
#define WM_GETDLGCODE                                                0x0087
// #ifndef _WIN32_WCE
#define WM_SYNCPAINT                                                 0x0088
// #endif
#define WM_NCMOUSEMOVE                                               0x00A0
#define WM_NCLBUTTONDOWN                                             0x00A1
#define WM_NCLBUTTONUP                                               0x00A2
#define WM_NCLBUTTONDBLCLK                                           0x00A3
#define WM_NCRBUTTONDOWN                                             0x00A4
#define WM_NCRBUTTONUP                                               0x00A5
#define WM_NCRBUTTONDBLCLK                                           0x00A6
#define WM_NCMBUTTONDOWN                                             0x00A7
#define WM_NCMBUTTONUP                                               0x00A8
#define WM_NCMBUTTONDBLCLK                                           0x00A9

#define WM_NCXBUTTONDOWN                                             0x00AB
#define WM_NCXBUTTONUP                                               0x00AC
#define WM_NCXBUTTONDBLCLK                                           0x00AD
#define WM_INPUT_DEVICE_CHANGE                                       0x00fe
#define WM_INPUT                                                     0x00FF
#define WM_KEYFIRST                                                  0x0100
#define WM_KEYDOWN                                                   0x0100
#define WM_KEYUP                                                     0x0101
#define WM_CHAR                                                      0x0102
#define WM_DEADCHAR                                                  0x0103
#define WM_SYSKEYDOWN                                                0x0104
#define WM_SYSKEYUP                                                  0x0105
#define WM_SYSCHAR                                                   0x0106
#define WM_SYSDEADCHAR                                               0x0107
#define WM_UNICHAR                                                   0x0109
#define WM_KEYLAST                                                   0x0109
#define UNICODE_NOCHAR                                               0xFFFF
#define WM_IME_STARTCOMPOSITION                                      0x010D
#define WM_IME_ENDCOMPOSITION                                        0x010E
#define WM_IME_COMPOSITION                                           0x010F
#define WM_IME_KEYLAST                                               0x010F
#define WM_INITDIALOG                                                0x0110
#define WM_COMMAND                                                   0x0111
#define WM_SYSCOMMAND                                                0x0112
#define WM_TIMER                                                     0x0113
#define WM_HSCROLL                                                   0x0114
#define WM_VSCROLL                                                   0x0115
#define WM_INITMENU                                                  0x0116
#define WM_INITMENUPOPUP                                             0x0117
#define WM_MENUSELECT                                                0x011F
// #if _WIN32_WINNT >= 0x0601
#define WM_GESTURE                                                   0x0119
#define WM_GESTURENOTIFY                                             0x011A
// #endif // _WIN32_WINNT >= 0x0601
#define WM_MENUCHAR                                                  0x0120
#define WM_ENTERIDLE                                                 0x0121
#ifndef _WIN32_WCE
#define WM_MENURBUTTONUP                                             0x0122
#define WM_MENUDRAG                                                  0x0123
#define WM_MENUGETOBJECT                                             0x0124
#define WM_UNINITMENUPOPUP                                           0x0125
#define WM_MENUCOMMAND                                               0x0126
#define WM_CHANGEUISTATE                                             0x0127
#define WM_UPDATEUISTATE                                             0x0128
#define WM_QUERYUISTATE                                              0x0129

#define UIS_SET                                                      1
#define UIS_CLEAR                                                    2
#define UIS_INITIALIZE                                               3

#define UISF_HIDEFOCUS                                               0x1
#define UISF_HIDEACCEL                                               0x2
#define UISF_ACTIVE                                                  0x4
#endif

#define WM_CTLCOLORMSGBOX                                            0x0132
#define WM_CTLCOLOREDIT                                              0x0133
#define WM_CTLCOLORLISTBOX                                           0x0134
#define WM_CTLCOLORBTN                                               0x0135
#define WM_CTLCOLORDLG                                               0x0136
#define WM_CTLCOLORSCROLLBAR                                         0x0137
#define WM_CTLCOLORSTATIC                                            0x0138
#define MN_GETHMENU                                                  0x01E1

#define WM_MOUSEFIRST                                                0x0200
#define WM_MOUSEMOVE                                                 0x0200
#define WM_LBUTTONDOWN                                               0x0201
#define WM_LBUTTONUP                                                 0x0202
#define WM_LBUTTONDBLCLK                                             0x0203
#define WM_RBUTTONDOWN                                               0x0204
#define WM_RBUTTONUP                                                 0x0205
#define WM_RBUTTONDBLCLK                                             0x0206
#define WM_MBUTTONDOWN                                               0x0207
#define WM_MBUTTONUP                                                 0x0208
#define WM_MBUTTONDBLCLK                                             0x0209
#define WM_MOUSEWHEEL                                                0x020A
#define WM_XBUTTONDOWN                                               0x020B
#define WM_XBUTTONUP                                                 0x020C
#define WM_XBUTTONDBLCLK                                             0x020D
// #if _WIN32_WINNT >= 0x0600
#define WM_MOUSEHWHEEL                                               0x020e
// #endif

// #if _WIN32_WINNT >= 0x0600
// #define WM_MOUSELAST                                                 0x020e
// #else
// #define WM_MOUSELAST                                                 0x020d
// #endif

#define WHEEL_DELTA                                                  120
// #define GET_WHEEL_DELTA_WPARAM(wParam)                               ((short)HIWORD(wParam))

// #define WHEEL_PAGESCROLL                                             (UINT_MAX)

// #define GET_KEYSTATE_WPARAM(wParam)                                  (LOWORD(wParam))
// #define GET_NCHITTEST_WPARAM(wParam)                                 ((short)LOWORD(wParam))
// #define GET_XBUTTON_WPARAM(wParam)                                   (HIWORD(wParam))

#define XBUTTON1                                                     0x0001
#define XBUTTON2                                                     0x0002

#define WM_PARENTNOTIFY                                              0x0210
#define WM_ENTERMENULOOP                                             0x0211
#define WM_EXITMENULOOP                                              0x0212
#define WM_NEXTMENU                                                  0x0213
#define WM_SIZING                                                    0x0214
#define WM_CAPTURECHANGED                                            0x0215
#define WM_MOVING                                                    0x0216
#define WM_POWERBROADCAST                                            0x0218

#ifndef _WIN32_WCE
#define PBT_APMQUERYSUSPEND                                          0x0000
#define PBT_APMQUERYSTANDBY                                          0x0001

#define PBT_APMQUERYSUSPENDFAILED                                    0x0002
#define PBT_APMQUERYSTANDBYFAILED                                    0x0003

#define PBT_APMSUSPEND                                               0x0004
#define PBT_APMSTANDBY                                               0x0005

#define PBT_APMRESUMECRITICAL                                        0x0006
#define PBT_APMRESUMESUSPEND                                         0x0007
#define PBT_APMRESUMESTANDBY                                         0x0008

#define PBTF_APMRESUMEFROMFAILURE                                    0x00000001

#define PBT_APMBATTERYLOW                                            0x0009
#define PBT_APMPOWERSTATUSCHANGE                                     0x000A

#define PBT_APMOEMEVENT                                              0x000B
#define PBT_APMRESUMEAUTOMATIC                                       0x0012
// #if _WIN32_WINNT >= 0x0502
#ifndef PBT_POWERSETTINGCHANGE
#define PBT_POWERSETTINGCHANGE                                       32787

#endif
// #endif
#endif

#define WM_DEVICECHANGE                                              0x0219

#define WM_MDICREATE                                                 0x0220
#define WM_MDIDESTROY                                                0x0221
#define WM_MDIACTIVATE                                               0x0222
#define WM_MDIRESTORE                                                0x0223
#define WM_MDINEXT                                                   0x0224
#define WM_MDIMAXIMIZE                                               0x0225
#define WM_MDITILE                                                   0x0226
#define WM_MDICASCADE                                                0x0227
#define WM_MDIICONARRANGE                                            0x0228
#define WM_MDIGETACTIVE                                              0x0229

#define WM_MDISETMENU                                                0x0230
#define WM_ENTERSIZEMOVE                                             0x0231
#define WM_EXITSIZEMOVE                                              0x0232
#define WM_DROPFILES                                                 0x0233
#define WM_MDIREFRESHMENU                                            0x0234
// #if WINVER >= 0x0602
#define WM_POINTERDEVICECHANGE                                       0x238
#define WM_POINTERDEVICEINRANGE                                      0x239
#define WM_POINTERDEVICEOUTOFRANGE                                   0x23a
// #endif
// #if WINVER >= 0x0601
#define WM_TOUCH                                                     0x0240
// #endif
// #if WINVER >= 0x0602
#define WM_NCPOINTERUPDATE                                           0x0241
#define WM_NCPOINTERDOWN                                             0x0242
#define WM_NCPOINTERUP                                               0x0243
#define WM_POINTERUPDATE                                             0x0245
#define WM_POINTERDOWN                                               0x0246
#define WM_POINTERUP                                                 0x0247
#define WM_POINTERENTER                                              0x0249
#define WM_POINTERLEAVE                                              0x024a
#define WM_POINTERACTIVATE                                           0x024b
#define WM_POINTERCAPTURECHANGED                                     0x024c
#define WM_TOUCHHITTESTING                                           0x024d
#define WM_POINTERWHEEL                                              0x024e
#define WM_POINTERHWHEEL                                             0x024f
// #endif

#define WM_IME_SETCONTEXT                                            0x0281
#define WM_IME_NOTIFY                                                0x0282
#define WM_IME_CONTROL                                               0x0283
#define WM_IME_COMPOSITIONFULL                                       0x0284
#define WM_IME_SELECT                                                0x0285
#define WM_IME_CHAR                                                  0x0286
#define WM_IME_REQUEST                                               0x0288
#define WM_IME_KEYDOWN                                               0x0290
#define WM_IME_KEYUP                                                 0x0291

#define WM_MOUSEHOVER                                                0x02A1
#define WM_MOUSELEAVE                                                0x02A3
#define WM_NCMOUSEHOVER                                              0x02A0
#define WM_NCMOUSELEAVE                                              0x02A2
#define WM_WTSSESSION_CHANGE                                         0x02B1
#define WM_TABLET_FIRST                                              0x02c0
#define WM_TABLET_LAST                                               0x02df
// #if WINVER >= 0x0601
#define WM_DPICHANGED                                                0x02e0
// #endif
#define WM_CUT                                                       0x0300
#define WM_COPY                                                      0x0301
#define WM_PASTE                                                     0x0302
#define WM_CLEAR                                                     0x0303
#define WM_UNDO                                                      0x0304
#define WM_RENDERFORMAT                                              0x0305
#define WM_RENDERALLFORMATS                                          0x0306
#define WM_DESTROYCLIPBOARD                                          0x0307
#define WM_DRAWCLIPBOARD                                             0x0308
#define WM_PAINTCLIPBOARD                                            0x0309
#define WM_VSCROLLCLIPBOARD                                          0x030A
#define WM_SIZECLIPBOARD                                             0x030B
#define WM_ASKCBFORMATNAME                                           0x030C
#define WM_CHANGECBCHAIN                                             0x030D
#define WM_HSCROLLCLIPBOARD                                          0x030E
#define WM_QUERYNEWPALETTE                                           0x030F
#define WM_PALETTEISCHANGING                                         0x0310
#define WM_PALETTECHANGED                                            0x0311
#define WM_HOTKEY                                                    0x0312
#define WM_PRINT                                                     0x0317
#define WM_PRINTCLIENT                                               0x0318
#define WM_APPCOMMAND                                                0x0319
#define WM_THEMECHANGED                                              0x031A
#define WM_CLIPBOARDUPDATE                                           0x031d
// #if _WIN32_WINNT >= 0x0600
#define WM_DWMCOMPOSITIONCHANGED                                     0x031e
#define WM_DWMNCRENDERINGCHANGED                                     0x031f
#define WM_DWMCOLORIZATIONCOLORCHANGED                               0x0320
#define WM_DWMWINDOWMAXIMIZEDCHANGE                                  0x0321
// #endif
// #if _WIN32_WINNT >= 0x0601
#define WM_DWMSENDICONICTHUMBNAIL                                    0x0323
#define WM_DWMSENDICONICLIVEPREVIEWBITMAP                            0x0326
// #endif
// #if WINVER >= 0x0600
#define WM_GETTITLEBARINFOEX                                         0x033f
// #endif

#define WM_HANDHELDFIRST                                             0x0358
#define WM_HANDHELDLAST                                              0x035F
#define WM_AFXFIRST                                                  0x0360
#define WM_AFXLAST                                                   0x037F
#define WM_PENWINFIRST                                               0x0380
#define WM_PENWINLAST                                                0x038F
#define WM_APP                                                       0x8000
#define WM_USER                                                      0x0400

#define WMSZ_LEFT                                                    1
#define WMSZ_RIGHT                                                   2
#define WMSZ_TOP                                                     3
#define WMSZ_TOPLEFT                                                 4
#define WMSZ_TOPRIGHT                                                5
#define WMSZ_BOTTOM                                                  6
#define WMSZ_BOTTOMLEFT                                              7
#define WMSZ_BOTTOMRIGHT                                             8

#ifndef NONCMESSAGES

#define HTERROR                                                      (-2)
#define HTTRANSPARENT                                                (-1)
#define HTNOWHERE                                                    0
#define HTCLIENT                                                     1
#define HTCAPTION                                                    2
#define HTSYSMENU                                                    3
#define HTGROWBOX                                                    4
#define HTSIZE                                                       HTGROWBOX
#define HTMENU                                                       5
#define HTHSCROLL                                                    6
#define HTVSCROLL                                                    7
#define HTMINBUTTON                                                  8
#define HTMAXBUTTON                                                  9
#define HTLEFT                                                       10
#define HTRIGHT                                                      11
#define HTTOP                                                        12
#define HTTOPLEFT                                                    13
#define HTTOPRIGHT                                                   14
#define HTBOTTOM                                                     15
#define HTBOTTOMLEFT                                                 16
#define HTBOTTOMRIGHT                                                17
#define HTBORDER                                                     18
#define HTREDUCE                                                     HTMINBUTTON
#define HTZOOM                                                       HTMAXBUTTON
#define HTSIZEFIRST                                                  HTLEFT
#define HTSIZELAST                                                   HTBOTTOMRIGHT
#define HTOBJECT                                                     19
#define HTCLOSE                                                      20
#define HTHELP                                                       21

#define SMTO_NORMAL                                                  0x0000
#define SMTO_BLOCK                                                   0x0001
#define SMTO_ABORTIFHUNG                                             0x0002
#define SMTO_NOTIMEOUTIFNOTHUNG                                      0x0008
// #if WINVER >= 0x0600
#define SMTO_ERRORONEXIT                                             0x0020
// #endif
#endif

#define MA_ACTIVATE                                                  1
#define MA_ACTIVATEANDEAT                                            2
#define MA_NOACTIVATE                                                3
#define MA_NOACTIVATEANDEAT                                          4

#define ICON_SMALL                                                   0
#define ICON_BIG                                                     1
#define ICON_SMALL2                                                  2

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define RegisterWindowMessage                                        __MINGW_NAME_AW(RegisterWindowMessage)
// #endif

#define SIZE_RESTORED                                                0
#define SIZE_MINIMIZED                                               1
#define SIZE_MAXIMIZED                                               2
#define SIZE_MAXSHOW                                                 3
#define SIZE_MAXHIDE                                                 4

#define SIZENORMAL                                                   SIZE_RESTORED
#define SIZEICONIC                                                   SIZE_MINIMIZED
#define SIZEFULLSCREEN                                               SIZE_MAXIMIZED
#define SIZEZOOMSHOW                                                 SIZE_MAXSHOW
#define SIZEZOOMHIDE                                                 SIZE_MAXHIDE

#define WVR_ALIGNTOP                                                 0x0010
#define WVR_ALIGNLEFT                                                0x0020
#define WVR_ALIGNBOTTOM                                              0x0040
#define WVR_ALIGNRIGHT                                               0x0080
#define WVR_HREDRAW                                                  0x0100
#define WVR_VREDRAW                                                  0x0200
#define WVR_REDRAW                                                   hb_bitor(WVR_HREDRAW, WVR_VREDRAW)
#define WVR_VALIDRECTS                                               0x0400

#ifndef NOKEYSTATES

#define MK_LBUTTON                                                   0x0001
#define MK_RBUTTON                                                   0x0002
#define MK_SHIFT                                                     0x0004
#define MK_CONTROL                                                   0x0008
#define MK_MBUTTON                                                   0x0010
#define MK_XBUTTON1                                                  0x0020
#define MK_XBUTTON2                                                  0x0040
#endif

#ifndef NOTRACKMOUSEEVENT
#define TME_HOVER                                                    0x00000001
#define TME_LEAVE                                                    0x00000002
#define TME_NONCLIENT                                                0x00000010
#define TME_QUERY                                                    0x40000000
#define TME_CANCEL                                                   0x80000000

#define HOVER_DEFAULT                                                0xFFFFFFFF
#endif

#endif

#ifndef NOWINSTYLES

#define WS_OVERLAPPED                                                0x00000000
#define WS_POPUP                                                     0x80000000
#define WS_CHILD                                                     0x40000000
#define WS_MINIMIZE                                                  0x20000000
#define WS_VISIBLE                                                   0x10000000
#define WS_DISABLED                                                  0x08000000
#define WS_CLIPSIBLINGS                                              0x04000000
#define WS_CLIPCHILDREN                                              0x02000000
#define WS_MAXIMIZE                                                  0x01000000
#define WS_CAPTION                                                   0x00C00000
#define WS_BORDER                                                    0x00800000
#define WS_DLGFRAME                                                  0x00400000
#define WS_VSCROLL                                                   0x00200000
#define WS_HSCROLL                                                   0x00100000
#define WS_SYSMENU                                                   0x00080000
#define WS_THICKFRAME                                                0x00040000
#define WS_GROUP                                                     0x00020000
#define WS_TABSTOP                                                   0x00010000
#define WS_MINIMIZEBOX                                               0x00020000
#define WS_MAXIMIZEBOX                                               0x00010000
#define WS_TILED                                                     WS_OVERLAPPED
#define WS_ICONIC                                                    WS_MINIMIZE
#define WS_SIZEBOX                                                   WS_THICKFRAME
#define WS_TILEDWINDOW                                               WS_OVERLAPPEDWINDOW

#define WS_OVERLAPPEDWINDOW                                          hb_bitor(WS_OVERLAPPED, WS_CAPTION, WS_SYSMENU, WS_THICKFRAME, WS_MINIMIZEBOX, WS_MAXIMIZEBOX)
#define WS_POPUPWINDOW                                               hb_bitor(WS_POPUP, WS_BORDER, WS_SYSMENU)
#define WS_CHILDWINDOW                                               (WS_CHILD)

#define WS_EX_DLGMODALFRAME                                          0x00000001
#define WS_EX_NOPARENTNOTIFY                                         0x00000004
#define WS_EX_TOPMOST                                                0x00000008
#define WS_EX_ACCEPTFILES                                            0x00000010
#define WS_EX_TRANSPARENT                                            0x00000020
#define WS_EX_MDICHILD                                               0x00000040
#define WS_EX_TOOLWINDOW                                             0x00000080
#define WS_EX_WINDOWEDGE                                             0x00000100
#define WS_EX_CLIENTEDGE                                             0x00000200
#define WS_EX_CONTEXTHELP                                            0x00000400
#define WS_EX_RIGHT                                                  0x00001000
#define WS_EX_LEFT                                                   0x00000000
#define WS_EX_RTLREADING                                             0x00002000
#define WS_EX_LTRREADING                                             0x00000000
#define WS_EX_LEFTSCROLLBAR                                          0x00004000
#define WS_EX_RIGHTSCROLLBAR                                         0x00000000
#define WS_EX_CONTROLPARENT                                          0x00010000
#define WS_EX_STATICEDGE                                             0x00020000
#define WS_EX_APPWINDOW                                              0x00040000

#define WS_EX_OVERLAPPEDWINDOW                                       hb_bitor(WS_EX_WINDOWEDGE, WS_EX_CLIENTEDGE)
#define WS_EX_PALETTEWINDOW                                          hb_bitor(WS_EX_WINDOWEDGE, WS_EX_TOOLWINDOW, WS_EX_TOPMOST)
#define WS_EX_LAYERED                                                0x00080000
#define WS_EX_NOINHERITLAYOUT                                        0x00100000
// #if WINVER >= 0x0602
#define WS_EX_NOREDIRECTIONBITMAP                                    0x00200000
// #endif
#define WS_EX_LAYOUTRTL                                              0x00400000
#define WS_EX_COMPOSITED                                             0x02000000
#define WS_EX_NOACTIVATE                                             0x08000000

#define CS_VREDRAW                                                   0x0001
#define CS_HREDRAW                                                   0x0002
#define CS_DBLCLKS                                                   0x0008
#define CS_OWNDC                                                     0x0020
#define CS_CLASSDC                                                   0x0040
#define CS_PARENTDC                                                  0x0080
#define CS_NOCLOSE                                                   0x0200
#define CS_SAVEBITS                                                  0x0800
#define CS_BYTEALIGNCLIENT                                           0x1000
#define CS_BYTEALIGNWINDOW                                           0x2000
#define CS_GLOBALCLASS                                               0x4000
#define CS_IME                                                       0x00010000
#define CS_DROPSHADOW                                                0x00020000
#endif

#define PRF_CHECKVISIBLE                                             0x00000001
#define PRF_NONCLIENT                                                0x00000002
#define PRF_CLIENT                                                   0x00000004
#define PRF_ERASEBKGND                                               0x00000008
#define PRF_CHILDREN                                                 0x00000010
#define PRF_OWNED                                                    0x00000020

#define BDR_RAISEDOUTER                                              0x0001
#define BDR_SUNKENOUTER                                              0x0002
#define BDR_RAISEDINNER                                              0x0004
#define BDR_SUNKENINNER                                              0x0008

#define BDR_OUTER                                                    hb_bitor(BDR_RAISEDOUTER, BDR_SUNKENOUTER)
#define BDR_INNER                                                    hb_bitor(BDR_RAISEDINNER, BDR_SUNKENINNER)
#define BDR_RAISED                                                   hb_bitor(BDR_RAISEDOUTER, BDR_RAISEDINNER)
#define BDR_SUNKEN                                                   hb_bitor(BDR_SUNKENOUTER, BDR_SUNKENINNER)

#define EDGE_RAISED                                                  hb_bitor(BDR_RAISEDOUTER, BDR_RAISEDINNER)
#define EDGE_SUNKEN                                                  hb_bitor(BDR_SUNKENOUTER, BDR_SUNKENINNER)
#define EDGE_ETCHED                                                  hb_bitor(BDR_SUNKENOUTER, BDR_RAISEDINNER)
#define EDGE_BUMP                                                    hb_bitor(BDR_RAISEDOUTER, BDR_SUNKENINNER)

#define BF_LEFT                                                      0x0001
#define BF_TOP                                                       0x0002
#define BF_RIGHT                                                     0x0004
#define BF_BOTTOM                                                    0x0008

#define BF_TOPLEFT                                                   hb_bitor(BF_TOP, BF_LEFT)
#define BF_TOPRIGHT                                                  hb_bitor(BF_TOP, BF_RIGHT)
#define BF_BOTTOMLEFT                                                hb_bitor(BF_BOTTOM, BF_LEFT)
#define BF_BOTTOMRIGHT                                               hb_bitor(BF_BOTTOM, BF_RIGHT)
#define BF_RECT                                                      hb_bitor(BF_LEFT, BF_TOP, BF_RIGHT, BF_BOTTOM)

#define BF_DIAGONAL                                                  0x0010

#define BF_DIAGONAL_ENDTOPRIGHT                                      hb_bitor(BF_DIAGONAL, BF_TOP, BF_RIGHT)
#define BF_DIAGONAL_ENDTOPLEFT                                       hb_bitor(BF_DIAGONAL, BF_TOP, BF_LEFT)
#define BF_DIAGONAL_ENDBOTTOMLEFT                                    hb_bitor(BF_DIAGONAL, BF_BOTTOM, BF_LEFT)
#define BF_DIAGONAL_ENDBOTTOMRIGHT                                   hb_bitor(BF_DIAGONAL, BF_BOTTOM, BF_RIGHT)

#define BF_MIDDLE                                                    0x0800
#define BF_SOFT                                                      0x1000
#define BF_ADJUST                                                    0x2000
#define BF_FLAT                                                      0x4000
#define BF_MONO                                                      0x8000

#define DFC_CAPTION                                                  1
#define DFC_MENU                                                     2
#define DFC_SCROLL                                                   3
#define DFC_BUTTON                                                   4
#define DFC_POPUPMENU                                                5

#define DFCS_CAPTIONCLOSE                                            0x0000
#define DFCS_CAPTIONMIN                                              0x0001
#define DFCS_CAPTIONMAX                                              0x0002
#define DFCS_CAPTIONRESTORE                                          0x0003
#define DFCS_CAPTIONHELP                                             0x0004

#define DFCS_MENUARROW                                               0x0000
#define DFCS_MENUCHECK                                               0x0001
#define DFCS_MENUBULLET                                              0x0002
#define DFCS_MENUARROWRIGHT                                          0x0004
#define DFCS_SCROLLUP                                                0x0000
#define DFCS_SCROLLDOWN                                              0x0001
#define DFCS_SCROLLLEFT                                              0x0002
#define DFCS_SCROLLRIGHT                                             0x0003
#define DFCS_SCROLLCOMBOBOX                                          0x0005
#define DFCS_SCROLLSIZEGRIP                                          0x0008
#define DFCS_SCROLLSIZEGRIPRIGHT                                     0x0010

#define DFCS_BUTTONCHECK                                             0x0000
#define DFCS_BUTTONRADIOIMAGE                                        0x0001
#define DFCS_BUTTONRADIOMASK                                         0x0002
#define DFCS_BUTTONRADIO                                             0x0004
#define DFCS_BUTTON3STATE                                            0x0008
#define DFCS_BUTTONPUSH                                              0x0010

#define DFCS_INACTIVE                                                0x0100
#define DFCS_PUSHED                                                  0x0200
#define DFCS_CHECKED                                                 0x0400

#define DFCS_TRANSPARENT                                             0x0800
#define DFCS_HOT                                                     0x1000

#define DFCS_ADJUSTRECT                                              0x2000
#define DFCS_FLAT                                                    0x4000
#define DFCS_MONO                                                    0x8000

#define DC_ACTIVE                                                    0x0001
#define DC_SMALLCAP                                                  0x0002
#define DC_ICON                                                      0x0004
#define DC_TEXT                                                      0x0008
#define DC_INBUTTON                                                  0x0010
#define DC_GRADIENT                                                  0x0020
#define DC_BUTTONS                                                   0x1000

#define IDANI_OPEN                                                   1
#define IDANI_CAPTION                                                3

#ifndef NOCLIPBOARD

#define CF_TEXT                                                      1
#define CF_BITMAP                                                    2
#define CF_METAFILEPICT                                              3
#define CF_SYLK                                                      4
#define CF_DIF                                                       5
#define CF_TIFF                                                      6
#define CF_OEMTEXT                                                   7
#define CF_DIB                                                       8
#define CF_PALETTE                                                   9
#define CF_PENDATA                                                   10
#define CF_RIFF                                                      11
#define CF_WAVE                                                      12
#define CF_UNICODETEXT                                               13
#define CF_ENHMETAFILE                                               14
#define CF_HDROP                                                     15
#define CF_LOCALE                                                    16
#define CF_DIBV5                                                     17
#define CF_MAX                                                       18

#define CF_OWNERDISPLAY                                              0x0080
#define CF_DSPTEXT                                                   0x0081
#define CF_DSPBITMAP                                                 0x0082
#define CF_DSPMETAFILEPICT                                           0x0083
#define CF_DSPENHMETAFILE                                            0x008E

#define CF_PRIVATEFIRST                                              0x0200
#define CF_PRIVATELAST                                               0x02FF

#define CF_GDIOBJFIRST                                               0x0300
#define CF_GDIOBJLAST                                                0x03FF
#endif

#define FVIRTKEY                                                     TRUE
#define FNOINVERT                                                    0x02
#define FSHIFT                                                       0x04
#define FCONTROL                                                     0x08
#define FALT                                                         0x10

#define WPF_SETMINPOSITION                                           0x0001
#define WPF_RESTORETOMAXIMIZED                                       0x0002
#define WPF_ASYNCWINDOWPLACEMENT                                     0x0004

#define ODT_MENU                                                     1
#define ODT_LISTBOX                                                  2
#define ODT_COMBOBOX                                                 3
#define ODT_BUTTON                                                   4
#define ODT_STATIC                                                   5

#define ODA_DRAWENTIRE                                               0x0001
#define ODA_SELECT                                                   0x0002
#define ODA_FOCUS                                                    0x0004

#define ODS_SELECTED                                                 0x0001
#define ODS_GRAYED                                                   0x0002
#define ODS_DISABLED                                                 0x0004
#define ODS_CHECKED                                                  0x0008
#define ODS_FOCUS                                                    0x0010
#define ODS_DEFAULT                                                  0x0020
#define ODS_COMBOBOXEDIT                                             0x1000
#define ODS_HOTLIGHT                                                 0x0040
#define ODS_INACTIVE                                                 0x0080
#define ODS_NOACCEL                                                  0x0100
#define ODS_NOFOCUSRECT                                              0x0200

#ifndef NOMSG
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetMessage                                                   __MINGW_NAME_AW(GetMessage)
// #define DispatchMessage                                              __MINGW_NAME_AW(DispatchMessage)
// #define PeekMessage                                                  __MINGW_NAME_AW(PeekMessage)
// #endif

#define PM_NOREMOVE                                                  0x0000
#define PM_REMOVE                                                    0x0001
#define PM_NOYIELD                                                   0x0002

#define PM_QS_INPUT                                                  hb_BitShift(QS_INPUT, 16)
#define PM_QS_POSTMESSAGE                                            hb_BitShift(hb_bitor(QS_POSTMESSAGE, QS_HOTKEY, QS_TIMER), 16)
#define PM_QS_PAINT                                                  hb_BitShift(QS_PAINT, 16)
#define PM_QS_SENDMESSAGE                                            hb_BitShift(QS_SENDMESSAGE, 16)
#endif

#define MOD_ALT                                                      0x0001
#define MOD_CONTROL                                                  0x0002
#define MOD_SHIFT                                                    0x0004
#define MOD_WIN                                                      0x0008
// #if WINVER >= 0x0601
#define MOD_NOREPEAT                                                 0x4000
// #endif

#define IDHOT_SNAPWINDOW                                             (-1)
#define IDHOT_SNAPDESKTOP                                            (-2)

#ifdef WIN_INTERNAL
#ifndef LSTRING
#define NOLSTRING
#endif
#ifndef LFILEIO
#define NOLFILEIO
#endif
#endif

#define ENDSESSION_CLOSEAPP                                          0x00000001
#define ENDSESSION_CRITICAL                                          0x40000000
#define ENDSESSION_LOGOFF                                            0x80000000

#define EWX_LOGOFF                                                   0x00000000
#define EWX_SHUTDOWN                                                 0x00000001
#define EWX_REBOOT                                                   0x00000002
#define EWX_FORCE                                                    0x00000004
#define EWX_POWEROFF                                                 0x00000008
#define EWX_FORCEIFHUNG                                              0x00000010
#define EWX_QUICKRESOLVE                                             0x00000020
// #if _WIN32_WINNT >= 0x0600
#define EWX_RESTARTAPPS                                              0x00000040
// #endif
#define EWX_HYBRID_SHUTDOWN                                          0x00400000
#define EWX_BOOTOPTIONS                                              0x01000000

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define ExitWindows(dwReserved,Code)                                 ExitWindowsEx(EWX_LOGOFF,0xFFFFFFFF)

// #define SendMessage                                                  __MINGW_NAME_AW(SendMessage)
// #define SendMessageTimeout                                           __MINGW_NAME_AW(SendMessageTimeout)
// #define SendNotifyMessage                                            __MINGW_NAME_AW(SendNotifyMessage)
// #define SendMessageCallback                                          __MINGW_NAME_AW(SendMessageCallback)

// #if _WIN32_WINNT >= 0x0602
// #endif

// #define BroadcastSystemMessageEx                                     __MINGW_NAME_AW(BroadcastSystemMessageEx)
// #define BroadcastSystemMessage                                       __MINGW_NAME_AW(BroadcastSystemMessage)

// #endif

#define BSM_ALLCOMPONENTS                                            0x00000000
#define BSM_VXDS                                                     0x00000001
#define BSM_NETDRIVER                                                0x00000002
#define BSM_INSTALLABLEDRIVERS                                       0x00000004
#define BSM_APPLICATIONS                                             0x00000008
#define BSM_ALLDESKTOPS                                              0x00000010

#define BSF_QUERY                                                    0x00000001
#define BSF_IGNORECURRENTTASK                                        0x00000002
#define BSF_FLUSHDISK                                                0x00000004
#define BSF_NOHANG                                                   0x00000008
#define BSF_POSTMESSAGE                                              0x00000010
#define BSF_FORCEIFHUNG                                              0x00000020
#define BSF_NOTIMEOUTIFNOTHUNG                                       0x00000040
#define BSF_ALLOWSFW                                                 0x00000080
#define BSF_SENDNOTIFYMESSAGE                                        0x00000100
#define BSF_RETURNHDESK                                              0x00000200
#define BSF_LUID                                                     0x00000400

#define BROADCAST_QUERY_DENY                                         0x424D5144

#define DEVICE_NOTIFY_WINDOW_HANDLE                                  0x00000000
#define DEVICE_NOTIFY_SERVICE_HANDLE                                 0x00000001
#define DEVICE_NOTIFY_ALL_INTERFACE_CLASSES                          0x00000004

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define RegisterDeviceNotification                                   __MINGW_NAME_AW(RegisterDeviceNotification)
// #define PostMessage                                                  __MINGW_NAME_AW(PostMessage)
// #define PostThreadMessage                                            __MINGW_NAME_AW(PostThreadMessage)
// #define PostAppMessage                                               __MINGW_NAME_AW(PostAppMessage)
// #define DefWindowProc                                                __MINGW_NAME_AW(DefWindowProc)
// #define CallWindowProc                                               __MINGW_NAME_AW(CallWindowProc)
// #define RegisterClass                                                __MINGW_NAME_AW(RegisterClass)
// #define UnregisterClass                                              __MINGW_NAME_AW(UnregisterClass)
// #define GetClassInfo                                                 __MINGW_NAME_AW(GetClassInfo)
// #define RegisterClassEx                                              __MINGW_NAME_AW(RegisterClassEx)
// #define GetClassInfoEx                                               __MINGW_NAME_AW(GetClassInfoEx)

// #if _WIN32_WINNT >= 0x0502
#ifndef _HPOWERNOTIFY_DEF_
#define _HPOWERNOTIFY_DEF_
#endif

// #endif

// #define PostAppMessageA(idThread,                                    wMsg,wParam,lParam)PostThreadMessageA((DWORD)idThread,wMsg,wParam,lParam)
// #define PostAppMessageW(idThread,                                    wMsg,wParam,lParam)PostThreadMessageW((DWORD)idThread,wMsg,wParam,lParam)

// #ifdef STRICT
// #else
// #endif
// #endif

#define CW_USEDEFAULT                                                (0x80000000)

#define HWND_BROADCAST                                               (0xffff)
#define HWND_MESSAGE                                                 (-3)
#define HWND_DESKTOP                                                 (0)

#define ISMEX_NOSEND                                                 0x00000000
#define ISMEX_SEND                                                   0x00000001
#define ISMEX_NOTIFY                                                 0x00000002
#define ISMEX_CALLBACK                                               0x00000004
#define ISMEX_REPLIED                                                0x00000008

// #if (_WIN32_WINNT >= 0x0502)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #define CreateWindowEx                                               __MINGW_NAME_AW(CreateWindowEx)
// #define CreateWindow                                                 __MINGW_NAME_AW(CreateWindow)

// #define CreateWindowA(lpClassName,lpWindowName,dwStyle,x,y,nWidth,nH CreateWindowExA((DWORD)0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam)
// #define CreateWindowW(lpClassName,lpWindowName,dwStyle,x,y,nWidth,nH CreateWindowExW((DWORD)0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam)

// #endif

// #if defined (_WINGDI_) && !defined (NOGDI)
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT < 0x0502
// #endif
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define PW_CLIENTONLY                                                0x00000001

// #endif

#define LWA_COLORKEY                                                 0x00000001
#define LWA_ALPHA                                                    0x00000002

#define ULW_COLORKEY                                                 0x00000001
#define ULW_ALPHA                                                    0x00000002
#define ULW_OPAQUE                                                   0x00000004
#define ULW_EX_NORESIZE                                              0x00000008

#define FLASHW_STOP                                                  0
#define FLASHW_CAPTION                                               0x00000001
#define FLASHW_TRAY                                                  0x00000002
#define FLASHW_ALL                                                   hb_bitor(FLASHW_CAPTION, FLASHW_TRAY)
#define FLASHW_TIMER                                                 0x00000004
#define FLASHW_TIMERNOFG                                             0x0000000c

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #if _WIN32_WINNT >= 0x0601
#define WDA_NONE                                                     0x00000000
#define WDA_MONITOR                                                  0x00000001
// #endif
// #endif

// #ifndef NODEFERWINDOWPOS
// #endif

#define SWP_NOSIZE                                                   0x0001
#define SWP_NOMOVE                                                   0x0002
#define SWP_NOZORDER                                                 0x0004
#define SWP_NOREDRAW                                                 0x0008
#define SWP_NOACTIVATE                                               0x0010
#define SWP_FRAMECHANGED                                             0x0020
#define SWP_SHOWWINDOW                                               0x0040
#define SWP_HIDEWINDOW                                               0x0080
#define SWP_NOCOPYBITS                                               0x0100
#define SWP_NOOWNERZORDER                                            0x0200
#define SWP_NOSENDCHANGING                                           0x0400

#define SWP_DRAWFRAME                                                SWP_FRAMECHANGED
#define SWP_NOREPOSITION                                             SWP_NOOWNERZORDER
#define SWP_DEFERERASE                                               0x2000
#define SWP_ASYNCWINDOWPOS                                           0x4000

#define HWND_TOP                                                     (0)
#define HWND_BOTTOM                                                  (1)
#define HWND_TOPMOST                                                 (-1)
#define HWND_NOTOPMOST                                               (-2)

#ifndef NOCTLMGR

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define CreateDialogParam                                            __MINGW_NAME_AW(CreateDialogParam)
// #define CreateDialogIndirectParam                                    __MINGW_NAME_AW(CreateDialogIndirectParam)
// #define CreateDialog                                                 __MINGW_NAME_AW(CreateDialog)
// #define CreateDialogIndirect                                         __MINGW_NAME_AW(CreateDialogIndirect)
// #define DialogBoxParam                                               __MINGW_NAME_AW(DialogBoxParam)
// #define DialogBoxIndirectParam                                       __MINGW_NAME_AW(DialogBoxIndirectParam)
// #define DialogBox                                                    __MINGW_NAME_AW(DialogBox)
// #define DialogBoxIndirect                                            __MINGW_NAME_AW(DialogBoxIndirect)
// #define SetDlgItemText                                               __MINGW_NAME_AW(SetDlgItemText)
// #define GetDlgItemText                                               __MINGW_NAME_AW(GetDlgItemText)
// #define SendDlgItemMessage                                           __MINGW_NAME_AW(SendDlgItemMessage)
// #define DefDlgProc                                                   __MINGW_NAME_AW(DefDlgProc)

// #define CreateDialogA(hInstance,lpName,hWndParent,lpDialogFunc)      CreateDialogParamA(hInstance,lpName,hWndParent,lpDialogFunc,(LPARAM)0)
// #define CreateDialogW(hInstance,lpName,hWndParent,lpDialogFunc)      CreateDialogParamW(hInstance,lpName,hWndParent,lpDialogFunc,(LPARAM)0)
// #define CreateDialogIndirectA(hInstance,lpTemplate,hWndParent,lpDial CreateDialogIndirectParamA(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)
// #define CreateDialogIndirectW(hInstance,lpTemplate,hWndParent,lpDial CreateDialogIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)

// #define DialogBoxA(hInstance,lpTemplate,hWndParent,lpDialogFunc)     DialogBoxParamA(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)
// #define DialogBoxW(hInstance,lpTemplate,hWndParent,lpDialogFunc)     DialogBoxParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)
// #define DialogBoxIndirectA(hInstance,lpTemplate,hWndParent,lpDialogF DialogBoxIndirectParamA(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)
// #define DialogBoxIndirectW(hInstance,lpTemplate,hWndParent,lpDialogF DialogBoxIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,(LPARAM)0)
// #endif

// #define DLGWINDOWEXTRA                                               30
// #endif

// #ifndef NOMSG
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define CallMsgFilter                                                __MINGW_NAME_AW(CallMsgFilter)
// #endif
// #endif

// #ifndef NOCLIPBOARD
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define RegisterClipboardFormat                                      __MINGW_NAME_AW(RegisterClipboardFormat)
// #define GetClipboardFormatName                                       __MINGW_NAME_AW(GetClipboardFormatName)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define CharToOem                                                    __MINGW_NAME_AW(CharToOem)
// #define OemToChar                                                    __MINGW_NAME_AW(OemToChar)
// #define CharToOemBuff                                                __MINGW_NAME_AW(CharToOemBuff)
// #define OemToCharBuff                                                __MINGW_NAME_AW(OemToCharBuff)
// #define CharUpper                                                    __MINGW_NAME_AW(CharUpper)
// #define CharUpperBuff                                                __MINGW_NAME_AW(CharUpperBuff)
// #define CharLower                                                    __MINGW_NAME_AW(CharLower)
// #define CharLowerBuff                                                __MINGW_NAME_AW(CharLowerBuff)
// #define CharNext                                                     __MINGW_NAME_AW(CharNext)
// #define CharPrev                                                     __MINGW_NAME_AW(CharPrev)
// #endif

// #define AnsiToOem                                                    CharToOemA
// #define OemToAnsi                                                    OemToCharA
// #define AnsiToOemBuff                                                CharToOemBuffA
// #define OemToAnsiBuff                                                OemToCharBuffA
// #define AnsiUpper                                                    CharUpperA
// #define AnsiUpperBuff                                                CharUpperBuffA
// #define AnsiLower                                                    CharLowerA
// #define AnsiLowerBuff                                                CharLowerBuffA
// #define AnsiNext                                                     CharNextA
// #define AnsiPrev                                                     CharPrevA

// #ifndef NOLANGUAGE
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define IsCharAlpha                                                  __MINGW_NAME_AW(IsCharAlpha)
// #define IsCharAlphaNumeric                                           __MINGW_NAME_AW(IsCharAlphaNumeric)
// #define IsCharUpper                                                  __MINGW_NAME_AW(IsCharUpper)
// #define IsCharLower                                                  __MINGW_NAME_AW(IsCharLower)
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetKeyNameText                                               __MINGW_NAME_AW(GetKeyNameText)
// #define VkKeyScan                                                    __MINGW_NAME_AW(VkKeyScan)
// #define VkKeyScanEx                                                  __MINGW_NAME_AW(VkKeyScanEx)
// #endif

#define KEYEVENTF_EXTENDEDKEY                                        0x0001
#define KEYEVENTF_KEYUP                                              0x0002
#define KEYEVENTF_UNICODE                                            0x0004
#define KEYEVENTF_SCANCODE                                           0x0008

#define MOUSEEVENTF_MOVE                                             0x0001
#define MOUSEEVENTF_LEFTDOWN                                         0x0002
#define MOUSEEVENTF_LEFTUP                                           0x0004
#define MOUSEEVENTF_RIGHTDOWN                                        0x0008
#define MOUSEEVENTF_RIGHTUP                                          0x0010
#define MOUSEEVENTF_MIDDLEDOWN                                       0x0020
#define MOUSEEVENTF_MIDDLEUP                                         0x0040
#define MOUSEEVENTF_XDOWN                                            0x0080
#define MOUSEEVENTF_XUP                                              0x0100
#define MOUSEEVENTF_WHEEL                                            0x0800
// #if _WIN32_WINNT >= 0x0600
#define MOUSEEVENTF_HWHEEL                                           0x01000
// #endif
// #if WINVER >= 0x0600
#define MOUSEEVENTF_MOVE_NOCOALESCE                                  0x2000
// #endif
#define MOUSEEVENTF_VIRTUALDESK                                      0x4000
#define MOUSEEVENTF_ABSOLUTE                                         0x8000

#define INPUT_MOUSE                                                  0
#define INPUT_KEYBOARD                                               1
#define INPUT_HARDWARE                                               2

// #if WINVER >= 0x0601
#define TOUCH_COORD_TO_PIXEL(l)                                      ((l)/100)

#define TOUCHEVENTF_MOVE                                             0x0001
#define TOUCHEVENTF_DOWN                                             0x0002
#define TOUCHEVENTF_UP                                               0x0004
#define TOUCHEVENTF_INRANGE                                          0x0008
#define TOUCHEVENTF_PRIMARY                                          0x0010
#define TOUCHEVENTF_NOCOALESCE                                       0x0020
#define TOUCHEVENTF_PEN                                              0x0040
#define TOUCHEVENTF_PALM                                             0x0080

#define TOUCHINPUTMASKF_TIMEFROMSYSTEM                               0x0001
#define TOUCHINPUTMASKF_EXTRAINFO                                    0x0002
#define TOUCHINPUTMASKF_CONTACTAREA                                  0x0004

#define TWF_FINETOUCH                                                (0x00000001)
#define TWF_WANTPALM                                                 (0x00000002)
// #endif

// #if WINVER >= 0x0602
#define POINTER_FLAG_NONE                                            0x00000000
#define POINTER_FLAG_NEW                                             0x00000001
#define POINTER_FLAG_INRANGE                                         0x00000002
#define POINTER_FLAG_INCONTACT                                       0x00000004
#define POINTER_FLAG_FIRSTBUTTON                                     0x00000010
#define POINTER_FLAG_SECONDBUTTON                                    0x00000020
#define POINTER_FLAG_THIRDBUTTON                                     0x00000040
#define POINTER_FLAG_FOURTHBUTTON                                    0x00000080
#define POINTER_FLAG_FIFTHBUTTON                                     0x00000100
#define POINTER_FLAG_PRIMARY                                         0x00002000
#define POINTER_FLAG_CONFIDENCE                                      0x00004000
#define POINTER_FLAG_CANCELED                                        0x00008000
#define POINTER_FLAG_DOWN                                            0x00010000
#define POINTER_FLAG_UPDATE                                          0x00020000
#define POINTER_FLAG_UP                                              0x00040000
#define POINTER_FLAG_WHEEL                                           0x00080000
#define POINTER_FLAG_HWHEEL                                          0x00100000
#define POINTER_FLAG_CAPTURECHANGED                                  0x00200000

#define POINTER_MOD_SHIFT                                            (0x0004)
#define POINTER_MOD_CTRL                                             (0x0008)

#define TOUCH_FLAG_NONE                                              0x00000000

#define TOUCH_MASK_NONE                                              0x00000000
#define TOUCH_MASK_CONTACTAREA                                       0x00000001
#define TOUCH_MASK_ORIENTATION                                       0x00000002
#define TOUCH_MASK_PRESSURE                                          0x00000004

#define PEN_FLAG_NONE                                                0x00000000
#define PEN_FLAG_BARREL                                              0x00000001
#define PEN_FLAG_INVERTED                                            0x00000002
#define PEN_FLAG_ERASER                                              0x00000004

#define PEN_MASK_NONE                                                0x00000000
#define PEN_MASK_PRESSURE                                            0x00000001
#define PEN_MASK_ROTATION                                            0x00000002
#define PEN_MASK_TILT_X                                              0x00000004
#define PEN_MASK_TILT_Y                                              0x00000008

#define POINTER_MESSAGE_FLAG_NEW                                     0x00000001
#define POINTER_MESSAGE_FLAG_INRANGE                                 0x00000002
#define POINTER_MESSAGE_FLAG_INCONTACT                               0x00000004
#define POINTER_MESSAGE_FLAG_FIRSTBUTTON                             0x00000010
#define POINTER_MESSAGE_FLAG_SECONDBUTTON                            0x00000020
#define POINTER_MESSAGE_FLAG_THIRDBUTTON                             0x00000040
#define POINTER_MESSAGE_FLAG_FOURTHBUTTON                            0x00000080
#define POINTER_MESSAGE_FLAG_FIFTHBUTTON                             0x00000100
#define POINTER_MESSAGE_FLAG_PRIMARY                                 0x00002000
#define POINTER_MESSAGE_FLAG_CONFIDENCE                              0x00004000
#define POINTER_MESSAGE_FLAG_CANCELED                                0x00008000

// #define GET_POINTERID_WPARAM(wParam)                                 (LOWORD(wParam))
// #define IS_POINTER_FLAG_SET_WPARAM(wParam,                           flag)(((DWORD)HIWORD(wParam)&(flag))==(flag))
// #define IS_POINTER_NEW_WPARAM(wParam)                                IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_NEW)
// #define IS_POINTER_INRANGE_WPARAM(wParam)                            IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_INRANGE)
// #define IS_POINTER_INCONTACT_WPARAM(wParam)                          IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_INCONTACT)
// #define IS_POINTER_FIRSTBUTTON_WPARAM(wParam)                        IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_FIRSTBUTTON)
// #define IS_POINTER_SECONDBUTTON_WPARAM(wParam)                       IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_SECONDBUTTON)
// #define IS_POINTER_THIRDBUTTON_WPARAM(wParam)                        IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_THIRDBUTTON)
// #define IS_POINTER_FOURTHBUTTON_WPARAM(wParam)                       IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_FOURTHBUTTON)
// #define IS_POINTER_FIFTHBUTTON_WPARAM(wParam)                        IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_FIFTHBUTTON)
// #define IS_POINTER_PRIMARY_WPARAM(wParam)                            IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_PRIMARY)
// #define HAS_POINTER_CONFIDENCE_WPARAM(wParam)                        IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_CONFIDENCE)
// #define IS_POINTER_CANCELED_WPARAM(wParam)                           IS_POINTER_FLAG_SET_WPARAM(wParam,POINTER_MESSAGE_FLAG_CANCELED)

#define PA_ACTIVATE                                                  MA_ACTIVATE
#define PA_NOACTIVATE                                                MA_NOACTIVATE

#define MAX_TOUCH_COUNT                                              256

#define TOUCH_FEEDBACK_DEFAULT                                       0x1
#define TOUCH_FEEDBACK_INDIRECT                                      0x2
#define TOUCH_FEEDBACK_NONE                                          0x3

#define TOUCH_HIT_TESTING_DEFAULT                                    0x0
#define TOUCH_HIT_TESTING_CLIENT                                     0x1
#define TOUCH_HIT_TESTING_NONE                                       0x2

#define TOUCH_HIT_TESTING_PROXIMITY_CLOSEST                          0x0
#define TOUCH_HIT_TESTING_PROXIMITY_FARTHEST                         0xfff

#define GWFS_INCLUDE_ANCESTORS                                       0x00000001
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #define MapVirtualKey                                                __MINGW_NAME_AW(MapVirtualKey)
// #define MapVirtualKeyEx                                              __MINGW_NAME_AW(MapVirtualKeyEx)

// #endif

#define MAPVK_VK_TO_VSC                                              (0)
#define MAPVK_VSC_TO_VK                                              (1)
#define MAPVK_VK_TO_CHAR                                             (2)
#define MAPVK_VSC_TO_VK_EX                                           (3)
// #if WINVER >= 0x0600
#define MAPVK_VK_TO_VSC_EX                                           (4)
// #endif

#define MWMO_WAITALL                                                 0x0001
#define MWMO_ALERTABLE                                               0x0002
#define MWMO_INPUTAVAILABLE                                          0x0004

#define QS_KEY                                                       0x0001
#define QS_MOUSEMOVE                                                 0x0002
#define QS_MOUSEBUTTON                                               0x0004
#define QS_POSTMESSAGE                                               0x0008
#define QS_TIMER                                                     0x0010
#define QS_PAINT                                                     0x0020
#define QS_SENDMESSAGE                                               0x0040
#define QS_HOTKEY                                                    0x0080
#define QS_ALLPOSTMESSAGE                                            0x0100
#define QS_RAWINPUT                                                  0x0400
// #if _WIN32_WINNT >= 0x0602
#define QS_TOUCH                                                     0x0800
#define QS_POINTER                                                   0x1000
// #endif

#define QS_MOUSE                                                     hb_bitor(QS_MOUSEMOVE. QS_MOUSEBUTTON)
// #if _WIN32_WINNT >= 0x602
#define QS_INPUT                                                     hb_bitor(QS_MOUSE, QS_KEY, QS_RAWINPUT, QS_TOUCH, QS_POINTER)
// #else
// #define QS_INPUT                                                     hb_bitor(QS_MOUSE, QS_KEY, QS_RAWINPUT)
// #endif
#define QS_ALLEVENTS                                                 hb_bitor(QS_INPUT, QS_POSTMESSAGE, QS_TIMER, QS_PAINT, QS_HOTKEY)
#define QS_ALLINPUT                                                  hb_bitor(QS_INPUT, QS_POSTMESSAGE, QS_TIMER, QS_PAINT, QS_HOTKEY, QS_SENDMESSAGE)

#define USER_TIMER_MAXIMUM                                           0x7FFFFFFF
#define USER_TIMER_MINIMUM                                           0x0000000A

// #if WINVER >= 0x0601
#define TIMERV_DEFAULT_COALESCING                                    (0)
#define TIMERV_NO_COALESCING                                         (0xffffffff)

#define TIMERV_COALESCING_MIN                                        (1)
#define TIMERV_COALESCING_MAX                                        (0x7ffffff5)
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadAccelerators                                             __MINGW_NAME_AW(LoadAccelerators)
// #define CreateAcceleratorTable                                       __MINGW_NAME_AW(CreateAcceleratorTable)
// #define CopyAcceleratorTable                                         __MINGW_NAME_AW(CopyAcceleratorTable)

// #ifndef NOMSG
// #define TranslateAccelerator                                         __MINGW_NAME_AW(TranslateAccelerator)
// #endif

// #endif

#ifndef NOSYSMETRICS

#define SM_CXSCREEN                                                  0
#define SM_CYSCREEN                                                  1
#define SM_CXVSCROLL                                                 2
#define SM_CYHSCROLL                                                 3
#define SM_CYCAPTION                                                 4
#define SM_CXBORDER                                                  5
#define SM_CYBORDER                                                  6
#define SM_CXDLGFRAME                                                7
#define SM_CYDLGFRAME                                                8
#define SM_CYVTHUMB                                                  9
#define SM_CXHTHUMB                                                  10
#define SM_CXICON                                                    11
#define SM_CYICON                                                    12
#define SM_CXCURSOR                                                  13
#define SM_CYCURSOR                                                  14
#define SM_CYMENU                                                    15
#define SM_CXFULLSCREEN                                              16
#define SM_CYFULLSCREEN                                              17
#define SM_CYKANJIWINDOW                                             18
#define SM_MOUSEPRESENT                                              19
#define SM_CYVSCROLL                                                 20
#define SM_CXHSCROLL                                                 21
#define SM_DEBUG                                                     22
#define SM_SWAPBUTTON                                                23
#define SM_RESERVED1                                                 24
#define SM_RESERVED2                                                 25
#define SM_RESERVED3                                                 26
#define SM_RESERVED4                                                 27
#define SM_CXMIN                                                     28
#define SM_CYMIN                                                     29
#define SM_CXSIZE                                                    30
#define SM_CYSIZE                                                    31
#define SM_CXFRAME                                                   32
#define SM_CYFRAME                                                   33
#define SM_CXMINTRACK                                                34
#define SM_CYMINTRACK                                                35
#define SM_CXDOUBLECLK                                               36
#define SM_CYDOUBLECLK                                               37
#define SM_CXICONSPACING                                             38
#define SM_CYICONSPACING                                             39
#define SM_MENUDROPALIGNMENT                                         40
#define SM_PENWINDOWS                                                41
#define SM_DBCSENABLED                                               42
#define SM_CMOUSEBUTTONS                                             43

#define SM_CXFIXEDFRAME                                              SM_CXDLGFRAME
#define SM_CYFIXEDFRAME                                              SM_CYDLGFRAME
#define SM_CXSIZEFRAME                                               SM_CXFRAME
#define SM_CYSIZEFRAME                                               SM_CYFRAME

#define SM_SECURE                                                    44
#define SM_CXEDGE                                                    45
#define SM_CYEDGE                                                    46
#define SM_CXMINSPACING                                              47
#define SM_CYMINSPACING                                              48
#define SM_CXSMICON                                                  49
#define SM_CYSMICON                                                  50
#define SM_CYSMCAPTION                                               51
#define SM_CXSMSIZE                                                  52
#define SM_CYSMSIZE                                                  53
#define SM_CXMENUSIZE                                                54
#define SM_CYMENUSIZE                                                55
#define SM_ARRANGE                                                   56
#define SM_CXMINIMIZED                                               57
#define SM_CYMINIMIZED                                               58
#define SM_CXMAXTRACK                                                59
#define SM_CYMAXTRACK                                                60
#define SM_CXMAXIMIZED                                               61
#define SM_CYMAXIMIZED                                               62
#define SM_NETWORK                                                   63
#define SM_CLEANBOOT                                                 67
#define SM_CXDRAG                                                    68
#define SM_CYDRAG                                                    69
#define SM_SHOWSOUNDS                                                70
#define SM_CXMENUCHECK                                               71
#define SM_CYMENUCHECK                                               72
#define SM_SLOWMACHINE                                               73
#define SM_MIDEASTENABLED                                            74
#define SM_MOUSEWHEELPRESENT                                         75
#define SM_XVIRTUALSCREEN                                            76
#define SM_YVIRTUALSCREEN                                            77
#define SM_CXVIRTUALSCREEN                                           78
#define SM_CYVIRTUALSCREEN                                           79
#define SM_CMONITORS                                                 80
#define SM_SAMEDISPLAYFORMAT                                         81
#define SM_IMMENABLED                                                82
#define SM_CXFOCUSBORDER                                             83
#define SM_CYFOCUSBORDER                                             84
#define SM_TABLETPC                                                  86
#define SM_MEDIACENTER                                               87
#define SM_STARTER                                                   88
#define SM_SERVERR2                                                  89
// #if _WIN32_WINNT >= 0x0600
#define SM_MOUSEHORIZONTALWHEELPRESENT                               91
#define SM_CXPADDEDBORDER                                            92
// #endif
// #if WINVER >= 0x0601
#define SM_DIGITIZER                                                 94
#define SM_MAXIMUMTOUCHES                                            95
// #endif

// #if WINVER <= 0x501
// #define SM_CMETRICS                                                  91
// #elif WINVER == 0x600
// #define SM_CMETRICS                                                  93
// #else
// #define SM_CMETRICS                                                  97
// #endif

#define SM_REMOTESESSION                                             0x1000
#define SM_SHUTTINGDOWN                                              0x2000
#define SM_REMOTECONTROL                                             0x2001
#define SM_CARETBLINKINGENABLED                                      0x2002
// #if WINVER >= 0x0602
#define SM_CONVERTIBLESLATEMODE                                      0x2003
#define SM_SYSTEMDOCKED                                              0x2004
// #endif

#endif

#ifndef NOMENUS

#define PMB_ACTIVE                                                   0x00000001

#define MNC_IGNORE                                                   0
#define MNC_CLOSE                                                    1
#define MNC_EXECUTE                                                  2
#define MNC_SELECT                                                   3

#define MNS_NOCHECK                                                  0x80000000
#define MNS_MODELESS                                                 0x40000000
#define MNS_DRAGDROP                                                 0x20000000
#define MNS_AUTODISMISS                                              0x10000000
#define MNS_NOTIFYBYPOS                                              0x08000000
#define MNS_CHECKORBMP                                               0x04000000

#define MIM_MAXHEIGHT                                                0x00000001
#define MIM_BACKGROUND                                               0x00000002
#define MIM_HELPID                                                   0x00000004
#define MIM_MENUDATA                                                 0x00000008
#define MIM_STYLE                                                    0x00000010
#define MIM_APPLYTOSUBMENUS                                          0x80000000

#define MND_CONTINUE                                                 0
#define MND_ENDMENU                                                  1

#define MNGOF_TOPGAP                                                 0x00000001
#define MNGOF_BOTTOMGAP                                              0x00000002

#define MNGO_NOINTERFACE                                             0x00000000
#define MNGO_NOERROR                                                 0x00000001

#define MIIM_STATE                                                   0x00000001
#define MIIM_ID                                                      0x00000002
#define MIIM_SUBMENU                                                 0x00000004
#define MIIM_CHECKMARKS                                              0x00000008
#define MIIM_TYPE                                                    0x00000010
#define MIIM_DATA                                                    0x00000020
#define MIIM_STRING                                                  0x00000040
#define MIIM_BITMAP                                                  0x00000080
#define MIIM_FTYPE                                                   0x00000100

#define HBMMENU_CALLBACK                                             (-1)
#define HBMMENU_SYSTEM                                               (1)
#define HBMMENU_MBAR_RESTORE                                         (2)
#define HBMMENU_MBAR_MINIMIZE                                        (3)
#define HBMMENU_MBAR_CLOSE                                           (5)
#define HBMMENU_MBAR_CLOSE_D                                         (6)
#define HBMMENU_MBAR_MINIMIZE_D                                      (7)
#define HBMMENU_POPUP_CLOSE                                          (8)
#define HBMMENU_POPUP_RESTORE                                        (9)
#define HBMMENU_POPUP_MAXIMIZE                                       (10)
#define HBMMENU_POPUP_MINIMIZE                                       (11)

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadMenu                                                     __MINGW_NAME_AW(LoadMenu)
// #define LoadMenuIndirect                                             __MINGW_NAME_AW(LoadMenuIndirect)
// #define ChangeMenu                                                   __MINGW_NAME_AW(ChangeMenu)
// #define GetMenuString                                                __MINGW_NAME_AW(GetMenuString)
// #define InsertMenu                                                   __MINGW_NAME_AW(InsertMenu)
// #define AppendMenu                                                   __MINGW_NAME_AW(AppendMenu)
// #define ModifyMenu                                                   __MINGW_NAME_AW(ModifyMenu)

// #if _WIN32_WINNT >= 0x0601
// #endif

// #define InsertMenuItem                                               __MINGW_NAME_AW(InsertMenuItem)
// #define GetMenuItemInfo                                              __MINGW_NAME_AW(GetMenuItemInfo)
// #define SetMenuItemInfo                                              __MINGW_NAME_AW(SetMenuItemInfo)

#define GMDI_USEDISABLED                                             0x0001
#define GMDI_GOINTOPOPUPS                                            0x0002

#define TPM_LEFTBUTTON                                               0x0000
#define TPM_RIGHTBUTTON                                              0x0002
#define TPM_LEFTALIGN                                                0x0000
#define TPM_CENTERALIGN                                              0x0004
#define TPM_RIGHTALIGN                                               0x0008
#define TPM_TOPALIGN                                                 0x0000
#define TPM_VCENTERALIGN                                             0x0010
#define TPM_BOTTOMALIGN                                              0x0020

#define TPM_HORIZONTAL                                               0x0000
#define TPM_VERTICAL                                                 0x0040
#define TPM_NONOTIFY                                                 0x0080
#define TPM_RETURNCMD                                                0x0100
#define TPM_RECURSE                                                  0x0001
#define TPM_HORPOSANIMATION                                          0x0400
#define TPM_HORNEGANIMATION                                          0x0800
#define TPM_VERPOSANIMATION                                          0x1000
#define TPM_VERNEGANIMATION                                          0x2000
#define TPM_NOANIMATION                                              0x4000
#define TPM_LAYOUTRTL                                                0x8000
// #if _WIN32_WINNT >= 0x0601
#define TPM_WORKAREA                                                 0x10000
// #endif
// #endif
#endif

#define DOF_EXECUTABLE                                               0x8001
#define DOF_DOCUMENT                                                 0x8002
#define DOF_DIRECTORY                                                0x8003
#define DOF_MULTIPLE                                                 0x8004
#define DOF_PROGMAN                                                  0x0001
#define DOF_SHELLDATA                                                0x0002

#define DO_DROPFILE                                                  0x454C4946
#define DO_PRINTFILE                                                 0x544E5250

#ifndef NODRAWTEXT

#define DT_TOP                                                       0x00000000
#define DT_LEFT                                                      0x00000000
#define DT_CENTER                                                    0x00000001
#define DT_RIGHT                                                     0x00000002
#define DT_VCENTER                                                   0x00000004
#define DT_BOTTOM                                                    0x00000008
#define DT_WORDBREAK                                                 0x00000010
#define DT_SINGLELINE                                                0x00000020
#define DT_EXPANDTABS                                                0x00000040
#define DT_TABSTOP                                                   0x00000080
#define DT_NOCLIP                                                    0x00000100
#define DT_EXTERNALLEADING                                           0x00000200
#define DT_CALCRECT                                                  0x00000400
#define DT_NOPREFIX                                                  0x00000800
#define DT_INTERNAL                                                  0x00001000
#define DT_EDITCONTROL                                               0x00002000
#define DT_PATH_ELLIPSIS                                             0x00004000
#define DT_END_ELLIPSIS                                              0x00008000
#define DT_MODIFYSTRING                                              0x00010000
#define DT_RTLREADING                                                0x00020000
#define DT_WORD_ELLIPSIS                                             0x00040000
#define DT_NOFULLWIDTHCHARBREAK                                      0x00080000
#define DT_HIDEPREFIX                                                0x00100000
#define DT_PREFIXONLY                                                0x00200000

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define DrawText                                                     __MINGW_NAME_AW(DrawText)
// #define DrawTextEx                                                   __MINGW_NAME_AW(DrawTextEx)
// #endif
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GrayString                                                   __MINGW_NAME_AW(GrayString)
// #define DrawState                                                    __MINGW_NAME_AW(DrawState)
// #define TabbedTextOut                                                __MINGW_NAME_AW(TabbedTextOut)
// #define GetTabbedTextExtent                                          __MINGW_NAME_AW(GetTabbedTextExtent)
// #endif

#define DST_COMPLEX                                                  0x0000
#define DST_TEXT                                                     0x0001
#define DST_PREFIXTEXT                                               0x0002
#define DST_ICON                                                     0x0003
#define DST_BITMAP                                                   0x0004

#define DSS_NORMAL                                                   0x0000
#define DSS_UNION                                                    0x0010
#define DSS_DISABLED                                                 0x0020
#define DSS_MONO                                                     0x0080
#define DSS_HIDEPREFIX                                               0x0200
#define DSS_PREFIXONLY                                               0x0400
#define DSS_RIGHT                                                    0x8000

#define ASFW_ANY                                                     (-1)

#define LSFW_LOCK                                                    1
#define LSFW_UNLOCK                                                  2

#define DCX_WINDOW                                                   0x00000001
#define DCX_CACHE                                                    0x00000002
#define DCX_NORESETATTRS                                             0x00000004
#define DCX_CLIPCHILDREN                                             0x00000008
#define DCX_CLIPSIBLINGS                                             0x00000010
#define DCX_PARENTCLIP                                               0x00000020
#define DCX_EXCLUDERGN                                               0x00000040
#define DCX_INTERSECTRGN                                             0x00000080
#define DCX_EXCLUDEUPDATE                                            0x00000100
#define DCX_INTERSECTUPDATE                                          0x00000200
#define DCX_LOCKWINDOWUPDATE                                         0x00000400

#define DCX_VALIDATE                                                 0x00200000

#define RDW_INVALIDATE                                               0x0001
#define RDW_INTERNALPAINT                                            0x0002
#define RDW_ERASE                                                    0x0004

#define RDW_VALIDATE                                                 0x0008
#define RDW_NOINTERNALPAINT                                          0x0010
#define RDW_NOERASE                                                  0x0020

#define RDW_NOCHILDREN                                               0x0040
#define RDW_ALLCHILDREN                                              0x0080

#define RDW_UPDATENOW                                                0x0100
#define RDW_ERASENOW                                                 0x0200

#define RDW_FRAME                                                    0x0400
#define RDW_NOFRAME                                                  0x0800

#define SW_SCROLLCHILDREN                                            0x0001
#define SW_INVALIDATE                                                0x0002
#define SW_ERASE                                                     0x0004
#define SW_SMOOTHSCROLL                                              0x0010

#ifndef NOSCROLL

#define ESB_ENABLE_BOTH                                              0x0000
#define ESB_DISABLE_BOTH                                             0x0003
#define ESB_DISABLE_LEFT                                             0x0001
#define ESB_DISABLE_RIGHT                                            0x0002

#define ESB_DISABLE_UP                                               0x0001
#define ESB_DISABLE_DOWN                                             0x0002

#define ESB_DISABLE_LTUP                                             ESB_DISABLE_LEFT
#define ESB_DISABLE_RTDN                                             ESB_DISABLE_RIGHT
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define SetProp                                                      __MINGW_NAME_AW(SetProp)
// #define GetProp                                                      __MINGW_NAME_AW(GetProp)
// #define RemoveProp                                                   __MINGW_NAME_AW(RemoveProp)
// #define EnumPropsEx                                                  __MINGW_NAME_AW(EnumPropsEx)
// #define EnumProps                                                    __MINGW_NAME_AW(EnumProps)
// #define SetWindowText                                                __MINGW_NAME_AW(SetWindowText)
// #define GetWindowText                                                __MINGW_NAME_AW(GetWindowText)
// #define GetWindowTextLength                                          __MINGW_NAME_AW(GetWindowTextLength)
// #endif

#define HELPINFO_WINDOW                                              0x0001
#define HELPINFO_MENUITEM                                            0x0002

#ifndef NOMB

#define MB_OK                                                        0x00000000
#define MB_OKCANCEL                                                  0x00000001
#define MB_ABORTRETRYIGNORE                                          0x00000002
#define MB_YESNOCANCEL                                               0x00000003
#define MB_YESNO                                                     0x00000004
#define MB_RETRYCANCEL                                               0x00000005
#define MB_CANCELTRYCONTINUE                                         0x00000006
#define MB_ICONHAND                                                  0x00000010
#define MB_ICONQUESTION                                              0x00000020
#define MB_ICONEXCLAMATION                                           0x00000030
#define MB_ICONASTERISK                                              0x00000040
#define MB_USERICON                                                  0x00000080
#define MB_ICONWARNING                                               MB_ICONEXCLAMATION
#define MB_ICONERROR                                                 MB_ICONHAND
#define MB_ICONINFORMATION                                           MB_ICONASTERISK
#define MB_ICONSTOP                                                  MB_ICONHAND
#define MB_DEFBUTTON1                                                0x00000000
#define MB_DEFBUTTON2                                                0x00000100
#define MB_DEFBUTTON3                                                0x00000200
#define MB_DEFBUTTON4                                                0x00000300
#define MB_APPLMODAL                                                 0x00000000
#define MB_SYSTEMMODAL                                               0x00001000
#define MB_TASKMODAL                                                 0x00002000
#define MB_HELP                                                      0x00004000
#define MB_NOFOCUS                                                   0x00008000
#define MB_SETFOREGROUND                                             0x00010000
#define MB_DEFAULT_DESKTOP_ONLY                                      0x00020000
#define MB_TOPMOST                                                   0x00040000
#define MB_RIGHT                                                     0x00080000
#define MB_RTLREADING                                                0x00100000
#define MB_SERVICE_NOTIFICATION                                      0x00200000
#define MB_SERVICE_NOTIFICATION_NT3X                                 0x00040000
#define MB_TYPEMASK                                                  0x0000000F
#define MB_ICONMASK                                                  0x000000F0
#define MB_DEFMASK                                                   0x00000F00
#define MB_MODEMASK                                                  0x00003000
#define MB_MISCMASK                                                  0x0000C000

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define MessageBox                                                   __MINGW_NAME_AW(MessageBox)
// #define MessageBoxEx                                                 __MINGW_NAME_AW(MessageBoxEx)
// #define MessageBoxIndirect                                           __MINGW_NAME_AW(MessageBoxIndirect)
// #endif
#endif

#define CWP_ALL                                                      0x0000
#define CWP_SKIPINVISIBLE                                            0x0001
#define CWP_SKIPDISABLED                                             0x0002
#define CWP_SKIPTRANSPARENT                                          0x0004

#ifndef NOCOLOR

#define CTLCOLOR_MSGBOX                                              0
#define CTLCOLOR_EDIT                                                1
#define CTLCOLOR_LISTBOX                                             2
#define CTLCOLOR_BTN                                                 3
#define CTLCOLOR_DLG                                                 4
#define CTLCOLOR_SCROLLBAR                                           5
#define CTLCOLOR_STATIC                                              6
#define CTLCOLOR_MAX                                                 7

#define COLOR_SCROLLBAR                                              0
#define COLOR_BACKGROUND                                             1
#define COLOR_ACTIVECAPTION                                          2
#define COLOR_INACTIVECAPTION                                        3
#define COLOR_MENU                                                   4
#define COLOR_WINDOW                                                 5
#define COLOR_WINDOWFRAME                                            6
#define COLOR_MENUTEXT                                               7
#define COLOR_WINDOWTEXT                                             8
#define COLOR_CAPTIONTEXT                                            9
#define COLOR_ACTIVEBORDER                                           10
#define COLOR_INACTIVEBORDER                                         11
#define COLOR_APPWORKSPACE                                           12
#define COLOR_HIGHLIGHT                                              13
#define COLOR_HIGHLIGHTTEXT                                          14
#define COLOR_BTNFACE                                                15
#define COLOR_BTNSHADOW                                              16
#define COLOR_GRAYTEXT                                               17
#define COLOR_BTNTEXT                                                18
#define COLOR_INACTIVECAPTIONTEXT                                    19
#define COLOR_BTNHIGHLIGHT                                           20

#define COLOR_3DDKSHADOW                                             21
#define COLOR_3DLIGHT                                                22
#define COLOR_INFOTEXT                                               23
#define COLOR_INFOBK                                                 24
#define COLOR_HOTLIGHT                                               26
#define COLOR_GRADIENTACTIVECAPTION                                  27
#define COLOR_GRADIENTINACTIVECAPTION                                28
#define COLOR_MENUHILIGHT                                            29
#define COLOR_MENUBAR                                                30

#define COLOR_DESKTOP                                                COLOR_BACKGROUND
#define COLOR_3DFACE                                                 COLOR_BTNFACE
#define COLOR_3DSHADOW                                               COLOR_BTNSHADOW
#define COLOR_3DHIGHLIGHT                                            COLOR_BTNHIGHLIGHT
#define COLOR_3DHILIGHT                                              COLOR_BTNHIGHLIGHT
#define COLOR_BTNHILIGHT                                             COLOR_BTNHIGHLIGHT

#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#ifndef NOWINOFFSETS

// #define GetWindowLong                                                __MINGW_NAME_AW(GetWindowLong)
// #define SetWindowLong                                                __MINGW_NAME_AW(SetWindowLong)
// #define GetClassLong                                                 __MINGW_NAME_AW(GetClassLong)
// #define SetClassLong                                                 __MINGW_NAME_AW(SetClassLong)

// #ifdef _WIN64
// #define GetWindowLongPtr                                             __MINGW_NAME_AW(GetWindowLongPtr)
// #define SetWindowLongPtr                                             __MINGW_NAME_AW(SetWindowLongPtr)
// #else
// #define GetWindowLongPtr                                             __MINGW_NAME_AW(GetWindowLongPtr)
// #define SetWindowLongPtr                                             __MINGW_NAME_AW(SetWindowLongPtr)
// #define GetWindowLongPtrA                                            GetWindowLongA
// #define GetWindowLongPtrW                                            GetWindowLongW
// #define SetWindowLongPtrA                                            SetWindowLongA
// #define SetWindowLongPtrW                                            SetWindowLongW
// #endif

// #ifdef _WIN64
// #define GetClassLongPtr                                              __MINGW_NAME_AW(GetClassLongPtr)
// #define SetClassLongPtr                                              __MINGW_NAME_AW(SetClassLongPtr)
// #else
// #define GetClassLongPtr                                              __MINGW_NAME_AW(GetClassLongPtr)
// #define SetClassLongPtr                                              __MINGW_NAME_AW(SetClassLongPtr)
// #define GetClassLongPtrA                                             GetClassLongA
// #define GetClassLongPtrW                                             GetClassLongW
// #define SetClassLongPtrA                                             SetClassLongA
// #define SetClassLongPtrW                                             SetClassLongW
// #endif
#endif

// #define FindWindow                                                   __MINGW_NAME_AW(FindWindow)
// #define FindWindowEx                                                 __MINGW_NAME_AW(FindWindowEx)
// #define GetClassName                                                 __MINGW_NAME_AW(GetClassName)

// #define EnumTaskWindows(hTask,lpfn,lParam)                           EnumThreadWindows(HandleToUlong(hTask),lpfn,lParam)

// #define GetNextWindow(hWnd,wCmd)                                     GetWindow(hWnd,wCmd)
// #define GetSysModalWindow()                                          (NULL)
// #define SetSysModalWindow(hWnd)                                      (NULL)

// #define GetWindowTask(hWnd)                                          ((HANDLE)(DWORD_PTR)GetWindowThreadProcessId(hWnd,NULL))

#define GW_HWNDFIRST                                                 0
#define GW_HWNDLAST                                                  1
#define GW_HWNDNEXT                                                  2
#define GW_HWNDPREV                                                  3
#define GW_OWNER                                                     4
#define GW_CHILD                                                     5
#define GW_ENABLEDPOPUP                                              6
#define GW_MAX                                                       6

#ifndef NOWH
// #define SetWindowsHook                                               __MINGW_NAME_AW(SetWindowsHook)
// #define SetWindowsHookEx                                             __MINGW_NAME_AW(SetWindowsHookEx)

// #ifdef STRICT
// #define DefHookProc(nCode,                                           wParam,lParam,phhk)CallNextHookEx(*phhk,nCode,wParam,lParam)
// #else
// #define DefHookProc(nCode,                                           wParam,lParam,phhk)CallNextHookEx((HHOOK)*phhk,nCode,wParam,lParam)
// #endif
#endif
#endif

#ifndef NOMENUS

#define MF_INSERT                                                    0x00000000
#define MF_CHANGE                                                    0x00000080
#define MF_APPEND                                                    0x00000100
#define MF_DELETE                                                    0x00000200
#define MF_REMOVE                                                    0x00001000
#define MF_BYCOMMAND                                                 0x00000000
#define MF_BYPOSITION                                                0x00000400
#define MF_SEPARATOR                                                 0x00000800
#define MF_ENABLED                                                   0x00000000
#define MF_GRAYED                                                    0x00000001
#define MF_DISABLED                                                  0x00000002
#define MF_UNCHECKED                                                 0x00000000
#define MF_CHECKED                                                   0x00000008
#define MF_USECHECKBITMAPS                                           0x00000200
#define MF_STRING                                                    0x00000000
#define MF_BITMAP                                                    0x00000004
#define MF_OWNERDRAW                                                 0x00000100
#define MF_POPUP                                                     0x00000010
#define MF_MENUBARBREAK                                              0x00000020
#define MF_MENUBREAK                                                 0x00000040
#define MF_UNHILITE                                                  0x00000000
#define MF_HILITE                                                    0x00000080
#define MF_DEFAULT                                                   0x00001000
#define MF_SYSMENU                                                   0x00002000
#define MF_HELP                                                      0x00004000
#define MF_RIGHTJUSTIFY                                              0x00004000
#define MF_MOUSESELECT                                               0x00008000
#define MF_END                                                       0x00000080

#define MFT_STRING                                                   MF_STRING
#define MFT_BITMAP                                                   MF_BITMAP
#define MFT_MENUBARBREAK                                             MF_MENUBARBREAK
#define MFT_MENUBREAK                                                MF_MENUBREAK
#define MFT_OWNERDRAW                                                MF_OWNERDRAW
#define MFT_RADIOCHECK                                               0x00000200
#define MFT_SEPARATOR                                                MF_SEPARATOR
#define MFT_RIGHTORDER                                               0x00002000
#define MFT_RIGHTJUSTIFY                                             MF_RIGHTJUSTIFY

#define MFS_GRAYED                                                   0x00000003
#define MFS_DISABLED                                                 MFS_GRAYED
#define MFS_CHECKED                                                  MF_CHECKED
#define MFS_HILITE                                                   MF_HILITE
#define MFS_ENABLED                                                  MF_ENABLED
#define MFS_UNCHECKED                                                MF_UNCHECKED
#define MFS_UNHILITE                                                 MF_UNHILITE
#define MFS_DEFAULT                                                  MF_DEFAULT

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define MF_END                                                       0x00000080
// #endif
#endif

#ifndef NOSYSCOMMANDS

#define SC_SIZE                                                      0xF000
#define SC_MOVE                                                      0xF010
#define SC_MINIMIZE                                                  0xF020
#define SC_MAXIMIZE                                                  0xF030
#define SC_NEXTWINDOW                                                0xF040
#define SC_PREVWINDOW                                                0xF050
#define SC_CLOSE                                                     0xF060
#define SC_VSCROLL                                                   0xF070
#define SC_HSCROLL                                                   0xF080
#define SC_MOUSEMENU                                                 0xF090
#define SC_KEYMENU                                                   0xF100
#define SC_ARRANGE                                                   0xF110
#define SC_RESTORE                                                   0xF120
#define SC_TASKLIST                                                  0xF130
#define SC_SCREENSAVE                                                0xF140
#define SC_HOTKEY                                                    0xF150
#define SC_DEFAULT                                                   0xF160
#define SC_MONITORPOWER                                              0xF170
#define SC_CONTEXTHELP                                               0xF180
#define SC_SEPARATOR                                                 0xF00F

// #if WINVER >= 0x0600
#define SCF_ISSECURE                                                 0x00000001
// #endif

// #define GET_SC_WPARAM(wParam)                                        ((int)wParam&0xfff0)

#define SC_ICON                                                      SC_MINIMIZE
#define SC_ZOOM                                                      SC_MAXIMIZE
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadBitmap                                                   __MINGW_NAME_AW(LoadBitmap)
// #define LoadCursor                                                   __MINGW_NAME_AW(LoadCursor)
// #define LoadCursorFromFile                                           __MINGW_NAME_AW(LoadCursorFromFile)
// #define CopyCursor(pcur)                                             ((HCURSOR)CopyIcon((HICON)(pcur)))
// #endif

#define IDC_ARROW                                                    MAKEINTRESOURCE(32512)
#define IDC_IBEAM                                                    MAKEINTRESOURCE(32513)
#define IDC_WAIT                                                     MAKEINTRESOURCE(32514)
#define IDC_CROSS                                                    MAKEINTRESOURCE(32515)
#define IDC_UPARROW                                                  MAKEINTRESOURCE(32516)
#define IDC_SIZE                                                     MAKEINTRESOURCE(32640)
#define IDC_ICON                                                     MAKEINTRESOURCE(32641)
#define IDC_SIZENWSE                                                 MAKEINTRESOURCE(32642)
#define IDC_SIZENESW                                                 MAKEINTRESOURCE(32643)
#define IDC_SIZEWE                                                   MAKEINTRESOURCE(32644)
#define IDC_SIZENS                                                   MAKEINTRESOURCE(32645)
#define IDC_SIZEALL                                                  MAKEINTRESOURCE(32646)
#define IDC_NO                                                       MAKEINTRESOURCE(32648)
#define IDC_HAND                                                     MAKEINTRESOURCE(32649)
#define IDC_APPSTARTING                                              MAKEINTRESOURCE(32650)
#define IDC_HELP                                                     MAKEINTRESOURCE(32651)

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadIcon                                                     __MINGW_NAME_AW(LoadIcon)
// #define PrivateExtractIcons                                          __MINGW_NAME_AW(PrivateExtractIcons)
// #endif

#define IMAGE_BITMAP                                                 0
#define IMAGE_ICON                                                   1
#define IMAGE_CURSOR                                                 2
#define IMAGE_ENHMETAFILE                                            3

#define LR_DEFAULTCOLOR                                              0x0000
#define LR_MONOCHROME                                                0x0001
#define LR_COLOR                                                     0x0002
#define LR_COPYRETURNORG                                             0x0004
#define LR_COPYDELETEORG                                             0x0008
#define LR_LOADFROMFILE                                              0x0010
#define LR_LOADTRANSPARENT                                           0x0020
#define LR_DEFAULTSIZE                                               0x0040
#define LR_VGACOLOR                                                  0x0080
#define LR_LOADMAP3DCOLORS                                           0x1000
#define LR_CREATEDIBSECTION                                          0x2000
#define LR_COPYFROMRESOURCE                                          0x4000
#define LR_SHARED                                                    0x8000

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadImage                                                    __MINGW_NAME_AW(LoadImage)
// #if _WIN32_WINNT >= 0x0600
// #define GetIconInfoEx                                                __MINGW_NAME_AW(GetIconInfoEx)
// #endif
// #endif

#define DI_MASK                                                      0x0001
#define DI_IMAGE                                                     0x0002
#define DI_NORMAL                                                    0x0003
#define DI_COMPAT                                                    0x0004
#define DI_DEFAULTSIZE                                               0x0008
#define DI_NOMIRROR                                                  0x0010

#define RES_ICON                                                     1
#define RES_CURSOR                                                   2

#ifdef OEMRESOURCE

#define OBM_CLOSE                                                    32754
#define OBM_UPARROW                                                  32753
#define OBM_DNARROW                                                  32752
#define OBM_RGARROW                                                  32751
#define OBM_LFARROW                                                  32750
#define OBM_REDUCE                                                   32749
#define OBM_ZOOM                                                     32748
#define OBM_RESTORE                                                  32747
#define OBM_REDUCED                                                  32746
#define OBM_ZOOMD                                                    32745
#define OBM_RESTORED                                                 32744
#define OBM_UPARROWD                                                 32743
#define OBM_DNARROWD                                                 32742
#define OBM_RGARROWD                                                 32741
#define OBM_LFARROWD                                                 32740
#define OBM_MNARROW                                                  32739
#define OBM_COMBO                                                    32738
#define OBM_UPARROWI                                                 32737
#define OBM_DNARROWI                                                 32736
#define OBM_RGARROWI                                                 32735
#define OBM_LFARROWI                                                 32734

#define OBM_OLD_CLOSE                                                32767
#define OBM_SIZE                                                     32766
#define OBM_OLD_UPARROW                                              32765
#define OBM_OLD_DNARROW                                              32764
#define OBM_OLD_RGARROW                                              32763
#define OBM_OLD_LFARROW                                              32762
#define OBM_BTSIZE                                                   32761
#define OBM_CHECK                                                    32760
#define OBM_CHECKBOXES                                               32759
#define OBM_BTNCORNERS                                               32758
#define OBM_OLD_REDUCE                                               32757
#define OBM_OLD_ZOOM                                                 32756
#define OBM_OLD_RESTORE                                              32755

#define OCR_NORMAL                                                   32512
#define OCR_IBEAM                                                    32513
#define OCR_WAIT                                                     32514
#define OCR_CROSS                                                    32515
#define OCR_UP                                                       32516
#define OCR_SIZE                                                     32640
#define OCR_ICON                                                     32641
#define OCR_SIZENWSE                                                 32642
#define OCR_SIZENESW                                                 32643
#define OCR_SIZEWE                                                   32644
#define OCR_SIZENS                                                   32645
#define OCR_SIZEALL                                                  32646
#define OCR_ICOCUR                                                   32647
#define OCR_NO                                                       32648
#define OCR_HAND                                                     32649
#define OCR_APPSTARTING                                              32650

#define OIC_SAMPLE                                                   32512
#define OIC_HAND                                                     32513
#define OIC_QUES                                                     32514
#define OIC_BANG                                                     32515
#define OIC_NOTE                                                     32516
#define OIC_WINLOGO                                                  32517
#define OIC_WARNING                                                  OIC_BANG
#define OIC_ERROR                                                    OIC_HAND
#define OIC_INFORMATION                                              OIC_NOTE
// #if WINVER >= 0x0600
#define OIC_SHIELD                                                   32518
// #endif
#endif

#define ORD_LANGDRIVER                                               1

#ifndef NOICONS

#ifdef RC_INVOKED
#define IDI_APPLICATION                                              32512
#define IDI_HAND                                                     32513
#define IDI_QUESTION                                                 32514
#define IDI_EXCLAMATION                                              32515
#define IDI_ASTERISK                                                 32516
#define IDI_WINLOGO                                                  32517
// #if WINVER >= 0x0600
#define IDI_SHIELD                                                   32518
// #endif
#else
// #define IDI_APPLICATION                                              MAKEINTRESOURCE(32512)
// #define IDI_HAND                                                     MAKEINTRESOURCE(32513)
// #define IDI_QUESTION                                                 MAKEINTRESOURCE(32514)
// #define IDI_EXCLAMATION                                              MAKEINTRESOURCE(32515)
// #define IDI_ASTERISK                                                 MAKEINTRESOURCE(32516)
// #define IDI_WINLOGO                                                  MAKEINTRESOURCE(32517)
// #if WINVER >= 0x0600
// #define IDI_SHIELD                                                   MAKEINTRESOURCE(32518)
// #endif
#endif

#define IDI_WARNING                                                  IDI_EXCLAMATION
#define IDI_ERROR                                                    IDI_HAND
#define IDI_INFORMATION                                              IDI_ASTERISK
#endif

#ifdef NOAPISET
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define LoadString                                                   __MINGW_NAME_AW(LoadString)
// #endif
#endif

#define IDOK                                                         1
#define IDCANCEL                                                     2
#define IDABORT                                                      3
#define IDRETRY                                                      4
#define IDIGNORE                                                     5
#define IDYES                                                        6
#define IDNO                                                         7
#define IDCLOSE                                                      8
#define IDHELP                                                       9
#define IDTRYAGAIN                                                   10
#define IDCONTINUE                                                   11

#ifndef IDTIMEOUT
#define IDTIMEOUT                                                    32000
#endif

#ifndef NOCTLMGR

#ifndef NOWINSTYLES
#define ES_LEFT                                                      0x0000
#define ES_CENTER                                                    0x0001
#define ES_RIGHT                                                     0x0002
#define ES_MULTILINE                                                 0x0004
#define ES_UPPERCASE                                                 0x0008
#define ES_LOWERCASE                                                 0x0010
#define ES_PASSWORD                                                  0x0020
#define ES_AUTOVSCROLL                                               0x0040
#define ES_AUTOHSCROLL                                               0x0080
#define ES_NOHIDESEL                                                 0x0100
#define ES_OEMCONVERT                                                0x0400
#define ES_READONLY                                                  0x0800
#define ES_WANTRETURN                                                0x1000
#define ES_NUMBER                                                    0x2000
#endif

#define EN_SETFOCUS                                                  0x0100
#define EN_KILLFOCUS                                                 0x0200
#define EN_CHANGE                                                    0x0300
#define EN_UPDATE                                                    0x0400
#define EN_ERRSPACE                                                  0x0500
#define EN_MAXTEXT                                                   0x0501
#define EN_HSCROLL                                                   0x0601
#define EN_VSCROLL                                                   0x0602
#define EN_ALIGN_LTR_EC                                              0x0700
#define EN_ALIGN_RTL_EC                                              0x0701

#define EC_LEFTMARGIN                                                0x0001
#define EC_RIGHTMARGIN                                               0x0002
#define EC_USEFONTINFO                                               0xffff

#define EMSIS_COMPOSITIONSTRING                                      0x0001

#define EIMES_GETCOMPSTRATONCE                                       0x0001
#define EIMES_CANCELCOMPSTRINFOCUS                                   0x0002
#define EIMES_COMPLETECOMPSTRKILLFOCUS                               0x0004

#ifndef NOWINMESSAGES

#define EM_GETSEL                                                    0x00B0
#define EM_SETSEL                                                    0x00B1
#define EM_GETRECT                                                   0x00B2
#define EM_SETRECT                                                   0x00B3
#define EM_SETRECTNP                                                 0x00B4
#define EM_SCROLL                                                    0x00B5
#define EM_LINESCROLL                                                0x00B6
#define EM_SCROLLCARET                                               0x00B7
#define EM_GETMODIFY                                                 0x00B8
#define EM_SETMODIFY                                                 0x00B9
#define EM_GETLINECOUNT                                              0x00BA
#define EM_LINEINDEX                                                 0x00BB
#define EM_SETHANDLE                                                 0x00BC
#define EM_GETHANDLE                                                 0x00BD
#define EM_GETTHUMB                                                  0x00BE
#define EM_LINELENGTH                                                0x00C1
#define EM_REPLACESEL                                                0x00C2
#define EM_GETLINE                                                   0x00C4
#define EM_LIMITTEXT                                                 0x00C5
#define EM_CANUNDO                                                   0x00C6
#define EM_UNDO                                                      0x00C7
#define EM_FMTLINES                                                  0x00C8
#define EM_LINEFROMCHAR                                              0x00C9
#define EM_SETTABSTOPS                                               0x00CB
#define EM_SETPASSWORDCHAR                                           0x00CC
#define EM_EMPTYUNDOBUFFER                                           0x00CD
#define EM_GETFIRSTVISIBLELINE                                       0x00CE
#define EM_SETREADONLY                                               0x00CF
#define EM_SETWORDBREAKPROC                                          0x00D0
#define EM_GETWORDBREAKPROC                                          0x00D1
#define EM_GETPASSWORDCHAR                                           0x00D2
#define EM_SETMARGINS                                                0x00D3
#define EM_GETMARGINS                                                0x00D4
#define EM_SETLIMITTEXT                                              EM_LIMITTEXT
#define EM_GETLIMITTEXT                                              0x00D5
#define EM_POSFROMCHAR                                               0x00D6
#define EM_CHARFROMPOS                                               0x00D7
#define EM_SETIMESTATUS                                              0x00D8
#define EM_GETIMESTATUS                                              0x00D9
#endif

#define WB_LEFT                                                      0
#define WB_RIGHT                                                     1
#define WB_ISDELIMITER                                               2

#define BS_PUSHBUTTON                                                0x00000000
#define BS_DEFPUSHBUTTON                                             0x00000001
#define BS_CHECKBOX                                                  0x00000002
#define BS_AUTOCHECKBOX                                              0x00000003
#define BS_RADIOBUTTON                                               0x00000004
#define BS_3STATE                                                    0x00000005
#define BS_AUTO3STATE                                                0x00000006
#define BS_GROUPBOX                                                  0x00000007
#define BS_USERBUTTON                                                0x00000008
#define BS_AUTORADIOBUTTON                                           0x00000009
#define BS_PUSHBOX                                                   0x0000000A
#define BS_OWNERDRAW                                                 0x0000000B
#define BS_TYPEMASK                                                  0x0000000F
#define BS_LEFTTEXT                                                  0x00000020
#define BS_TEXT                                                      0x00000000
#define BS_ICON                                                      0x00000040
#define BS_BITMAP                                                    0x00000080
#define BS_LEFT                                                      0x00000100
#define BS_RIGHT                                                     0x00000200
#define BS_CENTER                                                    0x00000300
#define BS_TOP                                                       0x00000400
#define BS_BOTTOM                                                    0x00000800
#define BS_VCENTER                                                   0x00000C00
#define BS_PUSHLIKE                                                  0x00001000
#define BS_MULTILINE                                                 0x00002000
#define BS_NOTIFY                                                    0x00004000
#define BS_FLAT                                                      0x00008000
#define BS_RIGHTBUTTON                                               BS_LEFTTEXT

#define BN_CLICKED                                                   0
#define BN_PAINT                                                     1
#define BN_HILITE                                                    2
#define BN_UNHILITE                                                  3
#define BN_DISABLE                                                   4
#define BN_DOUBLECLICKED                                             5
#define BN_PUSHED                                                    BN_HILITE
#define BN_UNPUSHED                                                  BN_UNHILITE
#define BN_DBLCLK                                                    BN_DOUBLECLICKED
#define BN_SETFOCUS                                                  6
#define BN_KILLFOCUS                                                 7

#define BM_GETCHECK                                                  0x00F0
#define BM_SETCHECK                                                  0x00F1
#define BM_GETSTATE                                                  0x00F2
#define BM_SETSTATE                                                  0x00F3
#define BM_SETSTYLE                                                  0x00F4
#define BM_CLICK                                                     0x00F5
#define BM_GETIMAGE                                                  0x00F6
#define BM_SETIMAGE                                                  0x00F7
// #if WINVER >= 0x0600
#define BM_SETDONTCLICK                                              0x00f8
// #endif

#define BST_UNCHECKED                                                0x0000
#define BST_CHECKED                                                  0x0001
#define BST_INDETERMINATE                                            0x0002
#define BST_PUSHED                                                   0x0004
#define BST_FOCUS                                                    0x0008

#define SS_LEFT                                                      0x00000000
#define SS_CENTER                                                    0x00000001
#define SS_RIGHT                                                     0x00000002
#define SS_ICON                                                      0x00000003
#define SS_BLACKRECT                                                 0x00000004
#define SS_GRAYRECT                                                  0x00000005
#define SS_WHITERECT                                                 0x00000006
#define SS_BLACKFRAME                                                0x00000007
#define SS_GRAYFRAME                                                 0x00000008
#define SS_WHITEFRAME                                                0x00000009
#define SS_USERITEM                                                  0x0000000A
#define SS_SIMPLE                                                    0x0000000B
#define SS_LEFTNOWORDWRAP                                            0x0000000C
#define SS_OWNERDRAW                                                 0x0000000D
#define SS_BITMAP                                                    0x0000000E
#define SS_ENHMETAFILE                                               0x0000000F
#define SS_ETCHEDHORZ                                                0x00000010
#define SS_ETCHEDVERT                                                0x00000011
#define SS_ETCHEDFRAME                                               0x00000012
#define SS_TYPEMASK                                                  0x0000001F
#define SS_REALSIZECONTROL                                           0x00000040
#define SS_NOPREFIX                                                  0x00000080
#define SS_NOTIFY                                                    0x00000100
#define SS_CENTERIMAGE                                               0x00000200
#define SS_RIGHTJUST                                                 0x00000400
#define SS_REALSIZEIMAGE                                             0x00000800
#define SS_SUNKEN                                                    0x00001000
#define SS_EDITCONTROL                                               0x00002000
#define SS_ENDELLIPSIS                                               0x00004000
#define SS_PATHELLIPSIS                                              0x00008000
#define SS_WORDELLIPSIS                                              0x0000C000
#define SS_ELLIPSISMASK                                              0x0000C000

#ifndef NOWINMESSAGES

#define STM_SETICON                                                  0x0170
#define STM_GETICON                                                  0x0171
#define STM_SETIMAGE                                                 0x0172
#define STM_GETIMAGE                                                 0x0173
#define STN_CLICKED                                                  0
#define STN_DBLCLK                                                   1
#define STN_ENABLE                                                   2
#define STN_DISABLE                                                  3

#define STM_MSGMAX                                                   0x0174
#endif

// #define WC_DIALOG                                                    (MAKEINTATOM(0x8002))

#define DWL_MSGRESULT                                                0
#define DWL_DLGPROC                                                  4
#define DWL_USER                                                     8

#ifdef _WIN64
#undef DWL_MSGRESULT
#undef DWL_DLGPROC
#undef DWL_USER
#endif

#define DWLP_MSGRESULT                                               0
// #define DWLP_DLGPROC                                                 DWLP_MSGRESULT+sizeof(LRESULT)
// #define DWLP_USER                                                    DWLP_DLGPROC+sizeof(DLGPROC)

#define DDL_READWRITE                                                0x0000
#define DDL_READONLY                                                 0x0001
#define DDL_HIDDEN                                                   0x0002
#define DDL_SYSTEM                                                   0x0004
#define DDL_DIRECTORY                                                0x0010
#define DDL_ARCHIVE                                                  0x0020

#define DDL_POSTMSGS                                                 0x2000
#define DDL_DRIVES                                                   0x4000
#define DDL_EXCLUSIVE                                                0x8000

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #ifndef NOMSG
// #define IsDialogMessage                                              __MINGW_NAME_AW(IsDialogMessage)
// #endif

// #define DlgDirList                                                   __MINGW_NAME_AW(DlgDirList)
// #define DlgDirSelectEx                                               __MINGW_NAME_AW(DlgDirSelectEx)
// #define DlgDirListComboBox                                           __MINGW_NAME_AW(DlgDirListComboBox)
// #define DlgDirSelectComboBoxEx                                       __MINGW_NAME_AW(DlgDirSelectComboBoxEx)

// #endif

#define DS_ABSALIGN                                                  0x01
#define DS_SYSMODAL                                                  0x02
#define DS_LOCALEDIT                                                 0x20
#define DS_SETFONT                                                   0x40
#define DS_MODALFRAME                                                0x80
#define DS_NOIDLEMSG                                                 0x100
#define DS_SETFOREGROUND                                             0x200

#define DS_3DLOOK                                                    0x0004
#define DS_FIXEDSYS                                                  0x0008
#define DS_NOFAILCREATE                                              0x0010
#define DS_CONTROL                                                   0x0400
#define DS_CENTER                                                    0x0800
#define DS_CENTERMOUSE                                               0x1000
#define DS_CONTEXTHELP                                               0x2000

#define DS_SHELLFONT                                                 hb_bitor(DS_SETFONT, DS_FIXEDSYS)

// #if defined (_WIN32_WCE) && (_WIN32_WCE >= 0x0500)
// #define DS_USEPIXELS                                                 0x8000
// #endif

#define DM_GETDEFID                                                  (WM_USER+0)
#define DM_SETDEFID                                                  (WM_USER+1)
#define DM_REPOSITION                                                (WM_USER+2)

#define DC_HASDEFID                                                  0x534B

#define DLGC_WANTARROWS                                              0x0001
#define DLGC_WANTTAB                                                 0x0002
#define DLGC_WANTALLKEYS                                             0x0004
#define DLGC_WANTMESSAGE                                             0x0004
#define DLGC_HASSETSEL                                               0x0008
#define DLGC_DEFPUSHBUTTON                                           0x0010
#define DLGC_UNDEFPUSHBUTTON                                         0x0020
#define DLGC_RADIOBUTTON                                             0x0040
#define DLGC_WANTCHARS                                               0x0080
#define DLGC_STATIC                                                  0x0100
#define DLGC_BUTTON                                                  0x2000

#define LB_CTLCODE                                                   0

#define LB_OKAY                                                      0
#define LB_ERR                                                       (-1)
#define LB_ERRSPACE                                                  (-2)

#define LBN_ERRSPACE                                                 (-2)
#define LBN_SELCHANGE                                                1
#define LBN_DBLCLK                                                   2
#define LBN_SELCANCEL                                                3
#define LBN_SETFOCUS                                                 4
#define LBN_KILLFOCUS                                                5

#ifndef NOWINMESSAGES

#define LB_ADDSTRING                                                 0x0180
#define LB_INSERTSTRING                                              0x0181
#define LB_DELETESTRING                                              0x0182
#define LB_SELITEMRANGEEX                                            0x0183
#define LB_RESETCONTENT                                              0x0184
#define LB_SETSEL                                                    0x0185
#define LB_SETCURSEL                                                 0x0186
#define LB_GETSEL                                                    0x0187
#define LB_GETCURSEL                                                 0x0188
#define LB_GETTEXT                                                   0x0189
#define LB_GETTEXTLEN                                                0x018A
#define LB_GETCOUNT                                                  0x018B
#define LB_SELECTSTRING                                              0x018C
#define LB_DIR                                                       0x018D
#define LB_GETTOPINDEX                                               0x018E
#define LB_FINDSTRING                                                0x018F
#define LB_GETSELCOUNT                                               0x0190
#define LB_GETSELITEMS                                               0x0191
#define LB_SETTABSTOPS                                               0x0192
#define LB_GETHORIZONTALEXTENT                                       0x0193
#define LB_SETHORIZONTALEXTENT                                       0x0194
#define LB_SETCOLUMNWIDTH                                            0x0195
#define LB_ADDFILE                                                   0x0196
#define LB_SETTOPINDEX                                               0x0197
#define LB_GETITEMRECT                                               0x0198
#define LB_GETITEMDATA                                               0x0199
#define LB_SETITEMDATA                                               0x019A
#define LB_SELITEMRANGE                                              0x019B
#define LB_SETANCHORINDEX                                            0x019C
#define LB_GETANCHORINDEX                                            0x019D
#define LB_SETCARETINDEX                                             0x019E
#define LB_GETCARETINDEX                                             0x019F
#define LB_SETITEMHEIGHT                                             0x01A0
#define LB_GETITEMHEIGHT                                             0x01A1
#define LB_FINDSTRINGEXACT                                           0x01A2
#define LB_SETLOCALE                                                 0x01A5
#define LB_GETLOCALE                                                 0x01A6
#define LB_SETCOUNT                                                  0x01A7
#define LB_INITSTORAGE                                               0x01A8
#define LB_ITEMFROMPOINT                                             0x01A9
// #if defined (_WIN32_WCE) && (_WIN32_WCE >= 0x0400)
// #define LB_MULTIPLEADDSTRING                                         0x01B1
// #endif
#define LB_GETLISTBOXINFO                                            0x01B2
#define LB_MSGMAX                                                    0x01B3
#endif

#ifndef NOWINSTYLES

#define LBS_NOTIFY                                                   0x0001
#define LBS_SORT                                                     0x0002
#define LBS_NOREDRAW                                                 0x0004
#define LBS_MULTIPLESEL                                              0x0008
#define LBS_OWNERDRAWFIXED                                           0x0010
#define LBS_OWNERDRAWVARIABLE                                        0x0020
#define LBS_HASSTRINGS                                               0x0040
#define LBS_USETABSTOPS                                              0x0080
#define LBS_NOINTEGRALHEIGHT                                         0x0100
#define LBS_MULTICOLUMN                                              0x0200
#define LBS_WANTKEYBOARDINPUT                                        0x0400
#define LBS_EXTENDEDSEL                                              0x0800
#define LBS_DISABLENOSCROLL                                          0x1000
#define LBS_NODATA                                                   0x2000
#define LBS_NOSEL                                                    0x4000
#define LBS_COMBOBOX                                                 0x8000

#define LBS_STANDARD                                                 hb_bitor(LBS_NOTIFY, LBS_SORT, WS_VSCROLL, WS_BORDER)
#endif

#define CB_OKAY                                                      0
#define CB_ERR                                                       (-1)
#define CB_ERRSPACE                                                  (-2)

#define CBN_ERRSPACE                                                 (-1)
#define CBN_SELCHANGE                                                1
#define CBN_DBLCLK                                                   2
#define CBN_SETFOCUS                                                 3
#define CBN_KILLFOCUS                                                4
#define CBN_EDITCHANGE                                               5
#define CBN_EDITUPDATE                                               6
#define CBN_DROPDOWN                                                 7
#define CBN_CLOSEUP                                                  8
#define CBN_SELENDOK                                                 9
#define CBN_SELENDCANCEL                                             10

#ifndef NOWINSTYLES

#define CBS_SIMPLE                                                   0x0001
#define CBS_DROPDOWN                                                 0x0002
#define CBS_DROPDOWNLIST                                             0x0003
#define CBS_OWNERDRAWFIXED                                           0x0010
#define CBS_OWNERDRAWVARIABLE                                        0x0020
#define CBS_AUTOHSCROLL                                              0x0040
#define CBS_OEMCONVERT                                               0x0080
#define CBS_SORT                                                     0x0100
#define CBS_HASSTRINGS                                               0x0200
#define CBS_NOINTEGRALHEIGHT                                         0x0400
#define CBS_DISABLENOSCROLL                                          0x0800
#define CBS_UPPERCASE                                                0x2000
#define CBS_LOWERCASE                                                0x4000
#endif

#ifndef NOWINMESSAGES
#define CB_GETEDITSEL                                                0x0140
#define CB_LIMITTEXT                                                 0x0141
#define CB_SETEDITSEL                                                0x0142
#define CB_ADDSTRING                                                 0x0143
#define CB_DELETESTRING                                              0x0144
#define CB_DIR                                                       0x0145
#define CB_GETCOUNT                                                  0x0146
#define CB_GETCURSEL                                                 0x0147
#define CB_GETLBTEXT                                                 0x0148
#define CB_GETLBTEXTLEN                                              0x0149
#define CB_INSERTSTRING                                              0x014A
#define CB_RESETCONTENT                                              0x014B
#define CB_FINDSTRING                                                0x014C
#define CB_SELECTSTRING                                              0x014D
#define CB_SETCURSEL                                                 0x014E
#define CB_SHOWDROPDOWN                                              0x014F
#define CB_GETITEMDATA                                               0x0150
#define CB_SETITEMDATA                                               0x0151
#define CB_GETDROPPEDCONTROLRECT                                     0x0152
#define CB_SETITEMHEIGHT                                             0x0153
#define CB_GETITEMHEIGHT                                             0x0154
#define CB_SETEXTENDEDUI                                             0x0155
#define CB_GETEXTENDEDUI                                             0x0156
#define CB_GETDROPPEDSTATE                                           0x0157
#define CB_FINDSTRINGEXACT                                           0x0158
#define CB_SETLOCALE                                                 0x0159
#define CB_GETLOCALE                                                 0x015A
#define CB_GETTOPINDEX                                               0x015b
#define CB_SETTOPINDEX                                               0x015c
#define CB_GETHORIZONTALEXTENT                                       0x015d
#define CB_SETHORIZONTALEXTENT                                       0x015e
#define CB_GETDROPPEDWIDTH                                           0x015f
#define CB_SETDROPPEDWIDTH                                           0x0160
#define CB_INITSTORAGE                                               0x0161
// #if defined (_WIN32_WCE) && (_WIN32_WCE >= 0x0400)
// #define CB_MULTIPLEADDSTRING                                         0x0163
// #endif
#define CB_GETCOMBOBOXINFO                                           0x0164
#define CB_MSGMAX                                                    0x0165
#endif

#ifndef NOWINSTYLES

#define SBS_HORZ                                                     0x0000
#define SBS_VERT                                                     0x0001
#define SBS_TOPALIGN                                                 0x0002
#define SBS_LEFTALIGN                                                0x0002
#define SBS_BOTTOMALIGN                                              0x0004
#define SBS_RIGHTALIGN                                               0x0004
#define SBS_SIZEBOXTOPLEFTALIGN                                      0x0002
#define SBS_SIZEBOXBOTTOMRIGHTALIGN                                  0x0004
#define SBS_SIZEBOX                                                  0x0008
#define SBS_SIZEGRIP                                                 0x0010
#endif

#ifndef NOWINMESSAGES
#define SBM_SETPOS                                                   0x00E0
#define SBM_GETPOS                                                   0x00E1
#define SBM_SETRANGE                                                 0x00E2
#define SBM_SETRANGEREDRAW                                           0x00E6
#define SBM_GETRANGE                                                 0x00E3
#define SBM_ENABLE_ARROWS                                            0x00E4
#define SBM_SETSCROLLINFO                                            0x00E9
#define SBM_GETSCROLLINFO                                            0x00EA
#define SBM_GETSCROLLBARINFO                                         0x00EB

#define SIF_RANGE                                                    0x0001
#define SIF_PAGE                                                     0x0002
#define SIF_POS                                                      0x0004
#define SIF_DISABLENOSCROLL                                          0x0008
#define SIF_TRACKPOS                                                 0x0010
#define SIF_ALL                                                      hb_bitor(SIF_RANGE, SIF_PAGE, SIF_POS, SIF_TRACKPOS)

#endif
#endif

#ifndef NOMDI

#define MDIS_ALLCHILDSTYLES                                          0x0001

#define MDITILE_VERTICAL                                             0x0000
#define MDITILE_HORIZONTAL                                           0x0001
#define MDITILE_SKIPDISABLED                                         0x0002
#define MDITILE_ZORDER                                               0x0004

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define DefFrameProc                                                 __MINGW_NAME_AW(DefFrameProc)
// #define DefMDIChildProc                                              __MINGW_NAME_AW(DefMDIChildProc)
// #define CreateMDIWindow                                              __MINGW_NAME_AW(CreateMDIWindow)
// #ifndef NOMSG
// #endif
// #endif
#endif
#endif

#ifndef NOHELP
// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define WinHelp                                                      __MINGW_NAME_AW(WinHelp)

#define HELP_CONTEXT                                                 0x0001
#define HELP_QUIT                                                    0x0002
#define HELP_INDEX                                                   0x0003
#define HELP_CONTENTS                                                0x0003
#define HELP_HELPONHELP                                              0x0004
#define HELP_SETINDEX                                                0x0005
#define HELP_SETCONTENTS                                             0x0005
#define HELP_CONTEXTPOPUP                                            0x0008
#define HELP_FORCEFILE                                               0x0009
#define HELP_KEY                                                     0x0101
#define HELP_COMMAND                                                 0x0102
#define HELP_PARTIALKEY                                              0x0105
#define HELP_MULTIKEY                                                0x0201
#define HELP_SETWINPOS                                               0x0203
#define HELP_CONTEXTMENU                                             0x000a
#define HELP_FINDER                                                  0x000b
#define HELP_WM_HELP                                                 0x000c
#define HELP_SETPOPUP_POS                                            0x000d

#define HELP_TCARD                                                   0x8000
#define HELP_TCARD_DATA                                              0x0010
#define HELP_TCARD_OTHER_CALLER                                      0x0011

#define IDH_NO_HELP                                                  28440
#define IDH_MISSING_CONTEXT                                          28441
#define IDH_GENERIC_HELP_BUTTON                                      28442
#define IDH_OK                                                       28443
#define IDH_CANCEL                                                   28444
#define IDH_HELP                                                     28445
// #endif
#endif

#define GR_GDIOBJECTS                                                0
#define GR_USEROBJECTS                                               1
// #if WINVER >= 0x0601
#define GR_GDIOBJECTS_PEAK                                           2
#define GR_USEROBJECTS_PEAK                                          4

#define GR_GLOBAL                                                    (-2)
// #endif

#ifndef NOSYSPARAMSINFO

#define SPI_GETBEEP                                                  0x0001
#define SPI_SETBEEP                                                  0x0002
#define SPI_GETMOUSE                                                 0x0003
#define SPI_SETMOUSE                                                 0x0004
#define SPI_GETBORDER                                                0x0005
#define SPI_SETBORDER                                                0x0006
#define SPI_GETKEYBOARDSPEED                                         0x000A
#define SPI_SETKEYBOARDSPEED                                         0x000B
#define SPI_LANGDRIVER                                               0x000C
#define SPI_ICONHORIZONTALSPACING                                    0x000D
#define SPI_GETSCREENSAVETIMEOUT                                     0x000E
#define SPI_SETSCREENSAVETIMEOUT                                     0x000F
#define SPI_GETSCREENSAVEACTIVE                                      0x0010
#define SPI_SETSCREENSAVEACTIVE                                      0x0011
#define SPI_GETGRIDGRANULARITY                                       0x0012
#define SPI_SETGRIDGRANULARITY                                       0x0013
#define SPI_SETDESKWALLPAPER                                         0x0014
#define SPI_SETDESKPATTERN                                           0x0015
#define SPI_GETKEYBOARDDELAY                                         0x0016
#define SPI_SETKEYBOARDDELAY                                         0x0017
#define SPI_ICONVERTICALSPACING                                      0x0018
#define SPI_GETICONTITLEWRAP                                         0x0019
#define SPI_SETICONTITLEWRAP                                         0x001A
#define SPI_GETMENUDROPALIGNMENT                                     0x001B
#define SPI_SETMENUDROPALIGNMENT                                     0x001C
#define SPI_SETDOUBLECLKWIDTH                                        0x001D
#define SPI_SETDOUBLECLKHEIGHT                                       0x001E
#define SPI_GETICONTITLELOGFONT                                      0x001F
#define SPI_SETDOUBLECLICKTIME                                       0x0020
#define SPI_SETMOUSEBUTTONSWAP                                       0x0021
#define SPI_SETICONTITLELOGFONT                                      0x0022
#define SPI_GETFASTTASKSWITCH                                        0x0023
#define SPI_SETFASTTASKSWITCH                                        0x0024
#define SPI_SETDRAGFULLWINDOWS                                       0x0025
#define SPI_GETDRAGFULLWINDOWS                                       0x0026
#define SPI_GETNONCLIENTMETRICS                                      0x0029
#define SPI_SETNONCLIENTMETRICS                                      0x002A
#define SPI_GETMINIMIZEDMETRICS                                      0x002B
#define SPI_SETMINIMIZEDMETRICS                                      0x002C
#define SPI_GETICONMETRICS                                           0x002D
#define SPI_SETICONMETRICS                                           0x002E
#define SPI_SETWORKAREA                                              0x002F
#define SPI_GETWORKAREA                                              0x0030
#define SPI_SETPENWINDOWS                                            0x0031

#define SPI_GETHIGHCONTRAST                                          0x0042
#define SPI_SETHIGHCONTRAST                                          0x0043
#define SPI_GETKEYBOARDPREF                                          0x0044
#define SPI_SETKEYBOARDPREF                                          0x0045
#define SPI_GETSCREENREADER                                          0x0046
#define SPI_SETSCREENREADER                                          0x0047
#define SPI_GETANIMATION                                             0x0048
#define SPI_SETANIMATION                                             0x0049
#define SPI_GETFONTSMOOTHING                                         0x004A
#define SPI_SETFONTSMOOTHING                                         0x004B
#define SPI_SETDRAGWIDTH                                             0x004C
#define SPI_SETDRAGHEIGHT                                            0x004D
#define SPI_SETHANDHELD                                              0x004E
#define SPI_GETLOWPOWERTIMEOUT                                       0x004F
#define SPI_GETPOWEROFFTIMEOUT                                       0x0050
#define SPI_SETLOWPOWERTIMEOUT                                       0x0051
#define SPI_SETPOWEROFFTIMEOUT                                       0x0052
#define SPI_GETLOWPOWERACTIVE                                        0x0053
#define SPI_GETPOWEROFFACTIVE                                        0x0054
#define SPI_SETLOWPOWERACTIVE                                        0x0055
#define SPI_SETPOWEROFFACTIVE                                        0x0056
#define SPI_SETCURSORS                                               0x0057
#define SPI_SETICONS                                                 0x0058
#define SPI_GETDEFAULTINPUTLANG                                      0x0059
#define SPI_SETDEFAULTINPUTLANG                                      0x005A
#define SPI_SETLANGTOGGLE                                            0x005B
#define SPI_GETWINDOWSEXTENSION                                      0x005C
#define SPI_SETMOUSETRAILS                                           0x005D
#define SPI_GETMOUSETRAILS                                           0x005E
#define SPI_SETSCREENSAVERRUNNING                                    0x0061
#define SPI_SCREENSAVERRUNNING                                       SPI_SETSCREENSAVERRUNNING
#define SPI_GETFILTERKEYS                                            0x0032
#define SPI_SETFILTERKEYS                                            0x0033
#define SPI_GETTOGGLEKEYS                                            0x0034
#define SPI_SETTOGGLEKEYS                                            0x0035
#define SPI_GETMOUSEKEYS                                             0x0036
#define SPI_SETMOUSEKEYS                                             0x0037
#define SPI_GETSHOWSOUNDS                                            0x0038
#define SPI_SETSHOWSOUNDS                                            0x0039
#define SPI_GETSTICKYKEYS                                            0x003A
#define SPI_SETSTICKYKEYS                                            0x003B
#define SPI_GETACCESSTIMEOUT                                         0x003C
#define SPI_SETACCESSTIMEOUT                                         0x003D
#define SPI_GETSERIALKEYS                                            0x003E
#define SPI_SETSERIALKEYS                                            0x003F
#define SPI_GETSOUNDSENTRY                                           0x0040
#define SPI_SETSOUNDSENTRY                                           0x0041
#define SPI_GETSNAPTODEFBUTTON                                       0x005F
#define SPI_SETSNAPTODEFBUTTON                                       0x0060
#define SPI_GETMOUSEHOVERWIDTH                                       0x0062
#define SPI_SETMOUSEHOVERWIDTH                                       0x0063
#define SPI_GETMOUSEHOVERHEIGHT                                      0x0064
#define SPI_SETMOUSEHOVERHEIGHT                                      0x0065
#define SPI_GETMOUSEHOVERTIME                                        0x0066
#define SPI_SETMOUSEHOVERTIME                                        0x0067
#define SPI_GETWHEELSCROLLLINES                                      0x0068
#define SPI_SETWHEELSCROLLLINES                                      0x0069
#define SPI_GETMENUSHOWDELAY                                         0x006A
#define SPI_SETMENUSHOWDELAY                                         0x006B
// #if _WIN32_WINNT >= 0x0600
#define SPI_GETWHEELSCROLLCHARS                                      0x006C
#define SPI_SETWHEELSCROLLCHARS                                      0x006D
// #endif
#define SPI_GETSHOWIMEUI                                             0x006E
#define SPI_SETSHOWIMEUI                                             0x006F
#define SPI_GETMOUSESPEED                                            0x0070
#define SPI_SETMOUSESPEED                                            0x0071
#define SPI_GETSCREENSAVERRUNNING                                    0x0072
#define SPI_GETDESKWALLPAPER                                         0x0073
// #if WINVER >= 0x0600
#define SPI_GETAUDIODESCRIPTION                                      0x0074
#define SPI_SETAUDIODESCRIPTION                                      0x0075
#define SPI_GETSCREENSAVESECURE                                      0x0076
#define SPI_SETSCREENSAVESECURE                                      0x0077
// #endif
// #if _WIN32_WINNT >= 0x0601
#define SPI_GETHUNGAPPTIMEOUT                                        0x0078
#define SPI_SETHUNGAPPTIMEOUT                                        0x0079
#define SPI_GETWAITTOKILLTIMEOUT                                     0x007a
#define SPI_SETWAITTOKILLTIMEOUT                                     0x007b
#define SPI_GETWAITTOKILLSERVICETIMEOUT                              0x007c
#define SPI_SETWAITTOKILLSERVICETIMEOUT                              0x007d
#define SPI_GETMOUSEDOCKTHRESHOLD                                    0x007e
#define SPI_SETMOUSEDOCKTHRESHOLD                                    0x007f
#define SPI_GETPENDOCKTHRESHOLD                                      0x0080
#define SPI_SETPENDOCKTHRESHOLD                                      0x0081
#define SPI_GETWINARRANGING                                          0x0082
#define SPI_SETWINARRANGING                                          0x0083
#define SPI_GETMOUSEDRAGOUTTHRESHOLD                                 0x0084
#define SPI_SETMOUSEDRAGOUTTHRESHOLD                                 0x0085
#define SPI_GETPENDRAGOUTTHRESHOLD                                   0x0086
#define SPI_SETPENDRAGOUTTHRESHOLD                                   0x0087
#define SPI_GETMOUSESIDEMOVETHRESHOLD                                0x0088
#define SPI_SETMOUSESIDEMOVETHRESHOLD                                0x0089
#define SPI_GETPENSIDEMOVETHRESHOLD                                  0x008a
#define SPI_SETPENSIDEMOVETHRESHOLD                                  0x008b
#define SPI_GETDRAGFROMMAXIMIZE                                      0x008c
#define SPI_SETDRAGFROMMAXIMIZE                                      0x008d
#define SPI_GETSNAPSIZING                                            0x008e
#define SPI_SETSNAPSIZING                                            0x008f
#define SPI_GETDOCKMOVING                                            0x0090
#define SPI_SETDOCKMOVING                                            0x0091
// #endif
// #if WINVER >= 0x0602
#define SPI_GETTOUCHPREDICTIONPARAMETERS                             0x009c
#define SPI_SETTOUCHPREDICTIONPARAMETERS                             0x009d
#define SPI_GETLOGICALDPIOVERRIDE                                    0x009e
#define SPI_SETLOGICALDPIOVERRIDE                                    0x009f
#define SPI_GETMOUSECORNERCLIPLENGTH                                 0x00a0
#define SPI_SETMOUSECORNERCLIPLENGTH                                 0x00a1
#define SPI_GETMENURECT                                              0x00a2
#define SPI_SETMENURECT                                              0x00a3
// #endif

#define SPI_GETACTIVEWINDOWTRACKING                                  0x1000
#define SPI_SETACTIVEWINDOWTRACKING                                  0x1001
#define SPI_GETMENUANIMATION                                         0x1002
#define SPI_SETMENUANIMATION                                         0x1003
#define SPI_GETCOMBOBOXANIMATION                                     0x1004
#define SPI_SETCOMBOBOXANIMATION                                     0x1005
#define SPI_GETLISTBOXSMOOTHSCROLLING                                0x1006
#define SPI_SETLISTBOXSMOOTHSCROLLING                                0x1007
#define SPI_GETGRADIENTCAPTIONS                                      0x1008
#define SPI_SETGRADIENTCAPTIONS                                      0x1009
#define SPI_GETKEYBOARDCUES                                          0x100A
#define SPI_SETKEYBOARDCUES                                          0x100B
#define SPI_GETMENUUNDERLINES                                        SPI_GETKEYBOARDCUES
#define SPI_SETMENUUNDERLINES                                        SPI_SETKEYBOARDCUES
#define SPI_GETACTIVEWNDTRKZORDER                                    0x100C
#define SPI_SETACTIVEWNDTRKZORDER                                    0x100D
#define SPI_GETHOTTRACKING                                           0x100E
#define SPI_SETHOTTRACKING                                           0x100F
#define SPI_GETMENUFADE                                              0x1012
#define SPI_SETMENUFADE                                              0x1013
#define SPI_GETSELECTIONFADE                                         0x1014
#define SPI_SETSELECTIONFADE                                         0x1015
#define SPI_GETTOOLTIPANIMATION                                      0x1016
#define SPI_SETTOOLTIPANIMATION                                      0x1017
#define SPI_GETTOOLTIPFADE                                           0x1018
#define SPI_SETTOOLTIPFADE                                           0x1019
#define SPI_GETCURSORSHADOW                                          0x101A
#define SPI_SETCURSORSHADOW                                          0x101B
#define SPI_GETMOUSESONAR                                            0x101C
#define SPI_SETMOUSESONAR                                            0x101D
#define SPI_GETMOUSECLICKLOCK                                        0x101E
#define SPI_SETMOUSECLICKLOCK                                        0x101F
#define SPI_GETMOUSEVANISH                                           0x1020
#define SPI_SETMOUSEVANISH                                           0x1021
#define SPI_GETFLATMENU                                              0x1022
#define SPI_SETFLATMENU                                              0x1023
#define SPI_GETDROPSHADOW                                            0x1024
#define SPI_SETDROPSHADOW                                            0x1025
#define SPI_GETBLOCKSENDINPUTRESETS                                  0x1026
#define SPI_SETBLOCKSENDINPUTRESETS                                  0x1027
#define SPI_GETUIEFFECTS                                             0x103E
#define SPI_SETUIEFFECTS                                             0x103F
// #if _WIN32_WINNT >= 0x0600
#define SPI_GETDISABLEOVERLAPPEDCONTENT                              0x1040
#define SPI_SETDISABLEOVERLAPPEDCONTENT                              0x1041
#define SPI_GETCLIENTAREAANIMATION                                   0x1042
#define SPI_SETCLIENTAREAANIMATION                                   0x1043
#define SPI_GETCLEARTYPE                                             0x1048
#define SPI_SETCLEARTYPE                                             0x1049
#define SPI_GETSPEECHRECOGNITION                                     0x104a
#define SPI_SETSPEECHRECOGNITION                                     0x104b
// #endif
// #if WINVER >= 0x0601
#define SPI_GETCARETBROWSING                                         0x104c
#define SPI_SETCARETBROWSING                                         0x104d
#define SPI_GETTHREADLOCALINPUTSETTINGS                              0x104e
#define SPI_SETTHREADLOCALINPUTSETTINGS                              0x104f
#define SPI_GETSYSTEMLANGUAGEBAR                                     0x1050
#define SPI_SETSYSTEMLANGUAGEBAR                                     0x1051
// #endif
#define SPI_GETFOREGROUNDLOCKTIMEOUT                                 0x2000
#define SPI_SETFOREGROUNDLOCKTIMEOUT                                 0x2001
#define SPI_GETACTIVEWNDTRKTIMEOUT                                   0x2002
#define SPI_SETACTIVEWNDTRKTIMEOUT                                   0x2003
#define SPI_GETFOREGROUNDFLASHCOUNT                                  0x2004
#define SPI_SETFOREGROUNDFLASHCOUNT                                  0x2005
#define SPI_GETCARETWIDTH                                            0x2006
#define SPI_SETCARETWIDTH                                            0x2007
#define SPI_GETMOUSECLICKLOCKTIME                                    0x2008
#define SPI_SETMOUSECLICKLOCKTIME                                    0x2009
#define SPI_GETFONTSMOOTHINGTYPE                                     0x200A
#define SPI_SETFONTSMOOTHINGTYPE                                     0x200B

#define FE_FONTSMOOTHINGSTANDARD                                     0x0001
#define FE_FONTSMOOTHINGCLEARTYPE                                    0x0002
#define FE_FONTSMOOTHINGDOCKING                                      0x8000

#define SPI_GETFONTSMOOTHINGCONTRAST                                 0x200C
#define SPI_SETFONTSMOOTHINGCONTRAST                                 0x200D
#define SPI_GETFOCUSBORDERWIDTH                                      0x200E
#define SPI_SETFOCUSBORDERWIDTH                                      0x200F
#define SPI_GETFOCUSBORDERHEIGHT                                     0x2010
#define SPI_SETFOCUSBORDERHEIGHT                                     0x2011
#define SPI_GETFONTSMOOTHINGORIENTATION                              0x2012
#define SPI_SETFONTSMOOTHINGORIENTATION                              0x2013
// #if _WIN32_WINNT >= 0x0600
#define SPI_GETMINIMUMHITRADIUS                                      0x2014
#define SPI_SETMINIMUMHITRADIUS                                      0x2015
#define SPI_GETMESSAGEDURATION                                       0x2016
#define SPI_SETMESSAGEDURATION                                       0x2017
// #endif
// #if WINVER >= 0x0602
#define SPI_GETCONTACTVISUALIZATION                                  0x2018
#define SPI_SETCONTACTVISUALIZATION                                  0x2019
#define SPI_GETGESTUREVISUALIZATION                                  0x201a
#define SPI_SETGESTUREVISUALIZATION                                  0x201b
// #endif

// #if WINVER >= 0x0602
#define CONTACTVISUALIZATION_OFF                                     0x0000
#define CONTACTVISUALIZATION_ON                                      0x0001
#define CONTACTVISUALIZATION_PRESENTATIONMODE                        0x0002

#define GESTUREVISUALIZATION_OFF                                     0x0000
#define GESTUREVISUALIZATION_ON                                      0x001f
#define GESTUREVISUALIZATION_TAP                                     0x0001
#define GESTUREVISUALIZATION_DOUBLETAP                               0x0002
#define GESTUREVISUALIZATION_PRESSANDTAP                             0x0004
#define GESTUREVISUALIZATION_PRESSANDHOLD                            0x0008
#define GESTUREVISUALIZATION_RIGHTTAP                                0x0010

#define MAX_TOUCH_PREDICTION_FILTER_TAPS                             3

#define TOUCHPREDICTIONPARAMETERS_DEFAULT_LATENCY                    8
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_SAMPLETIME                 8
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_USE_HW_TIMESTAMP           1
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_RLS_DELTA                  0.001f
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_RLS_LAMBDA_MIN             0.9f
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_RLS_LAMBDA_MAX             0.999f
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_RLS_LAMBDA_LEARNING_RATE   0.001f
#define TOUCHPREDICTIONPARAMETERS_DEFAULT_RLS_EXPO_SMOOTH_ALPHA      0.99f

#define MAX_LOGICALDPIOVERRIDE                                       2
#define MIN_LOGICALDPIOVERRIDE                                       -2
// #endif

#define FE_FONTSMOOTHINGORIENTATIONBGR                               0x0000
#define FE_FONTSMOOTHINGORIENTATIONRGB                               0x0001

#define SPIF_UPDATEINIFILE                                           0x0001
#define SPIF_SENDWININICHANGE                                        0x0002
#define SPIF_SENDCHANGE                                              SPIF_SENDWININICHANGE

#define METRICS_USEDEFAULT                                           -1
#ifdef _WINGDI_
#ifndef NOGDI
#endif
#endif

#define ARW_BOTTOMLEFT                                               0x0000
#define ARW_BOTTOMRIGHT                                              0x0001
#define ARW_TOPLEFT                                                  0x0002
#define ARW_TOPRIGHT                                                 0x0003
#define ARW_STARTMASK                                                0x0003
#define ARW_STARTRIGHT                                               0x0001
#define ARW_STARTTOP                                                 0x0002

#define ARW_LEFT                                                     0x0000
#define ARW_RIGHT                                                    0x0000
#define ARW_UP                                                       0x0004
#define ARW_DOWN                                                     0x0004
#define ARW_HIDE                                                     0x0008

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #ifdef _WINGDI_
// #ifndef NOGDI
// #endif
// #endif
// #endif

#define SERKF_SERIALKEYSON                                           0x00000001
#define SERKF_AVAILABLE                                              0x00000002
#define SERKF_INDICATOR                                              0x00000004

#define HCF_HIGHCONTRASTON                                           0x00000001
#define HCF_AVAILABLE                                                0x00000002
#define HCF_HOTKEYACTIVE                                             0x00000004
#define HCF_CONFIRMHOTKEY                                            0x00000008
#define HCF_HOTKEYSOUND                                              0x00000010
#define HCF_INDICATOR                                                0x00000020
#define HCF_HOTKEYAVAILABLE                                          0x00000040
#define HCF_LOGONDESKTOP                                             0x00000100
#define HCF_DEFAULTDESKTOP                                           0x00000200

#define CDS_UPDATEREGISTRY                                           0x00000001
#define CDS_TEST                                                     0x00000002
#define CDS_FULLSCREEN                                               0x00000004
#define CDS_GLOBAL                                                   0x00000008
#define CDS_SET_PRIMARY                                              0x00000010
#define CDS_VIDEOPARAMETERS                                          0x00000020
// #if WINVER >= 0x0600
#define CDS_ENABLE_UNSAFE_MODES                                      0x00000100
#define CDS_DISABLE_UNSAFE_MODES                                     0x00000200
// #endif
#define CDS_RESET                                                    0x40000000
#define CDS_RESET_EX                                                 0x20000000
#define CDS_NORESET                                                  0x10000000

#define DISP_CHANGE_SUCCESSFUL                                       0
#define DISP_CHANGE_RESTART                                          1
#define DISP_CHANGE_FAILED                                           -1
#define DISP_CHANGE_BADMODE                                          -2
#define DISP_CHANGE_NOTUPDATED                                       -3
#define DISP_CHANGE_BADFLAGS                                         -4
#define DISP_CHANGE_BADPARAM                                         -5
#define DISP_CHANGE_BADDUALVIEW                                      -6

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
#ifdef _WINGDI_
#ifndef NOGDI
// #define ChangeDisplaySettings                                        __MINGW_NAME_AW(ChangeDisplaySettings)
// #define ChangeDisplaySettingsEx                                      __MINGW_NAME_AW(ChangeDisplaySettingsEx)
// #define EnumDisplaySettings                                          __MINGW_NAME_AW(EnumDisplaySettings)
// #define EnumDisplaySettingsEx                                        __MINGW_NAME_AW(EnumDisplaySettingsEx)
// #define EnumDisplayDevices                                           __MINGW_NAME_AW(EnumDisplayDevices)

#define ENUM_CURRENT_SETTINGS                                        ((DWORD)-1)
#define ENUM_REGISTRY_SETTINGS                                       ((DWORD)-2)

#define EDS_RAWMODE                                                  0x00000002

#define EDD_GET_DEVICE_INTERFACE_NAME                                0x00000001

#endif
#endif

// #define SystemParametersInfo                                         __MINGW_NAME_AW(SystemParametersInfo)

// #endif
#endif

#define FKF_FILTERKEYSON                                             0x00000001
#define FKF_AVAILABLE                                                0x00000002
#define FKF_HOTKEYACTIVE                                             0x00000004
#define FKF_CONFIRMHOTKEY                                            0x00000008
#define FKF_HOTKEYSOUND                                              0x00000010
#define FKF_INDICATOR                                                0x00000020
#define FKF_CLICKON                                                  0x00000040

#define SKF_STICKYKEYSON                                             0x00000001
#define SKF_AVAILABLE                                                0x00000002
#define SKF_HOTKEYACTIVE                                             0x00000004
#define SKF_CONFIRMHOTKEY                                            0x00000008
#define SKF_HOTKEYSOUND                                              0x00000010
#define SKF_INDICATOR                                                0x00000020
#define SKF_AUDIBLEFEEDBACK                                          0x00000040
#define SKF_TRISTATE                                                 0x00000080
#define SKF_TWOKEYSOFF                                               0x00000100
#define SKF_LALTLATCHED                                              0x10000000
#define SKF_LCTLLATCHED                                              0x04000000
#define SKF_LSHIFTLATCHED                                            0x01000000
#define SKF_RALTLATCHED                                              0x20000000
#define SKF_RCTLLATCHED                                              0x08000000
#define SKF_RSHIFTLATCHED                                            0x02000000
#define SKF_LWINLATCHED                                              0x40000000
#define SKF_RWINLATCHED                                              0x80000000
#define SKF_LALTLOCKED                                               0x00100000
#define SKF_LCTLLOCKED                                               0x00040000
#define SKF_LSHIFTLOCKED                                             0x00010000
#define SKF_RALTLOCKED                                               0x00200000
#define SKF_RCTLLOCKED                                               0x00080000
#define SKF_RSHIFTLOCKED                                             0x00020000
#define SKF_LWINLOCKED                                               0x00400000
#define SKF_RWINLOCKED                                               0x00800000

#define MKF_MOUSEKEYSON                                              0x00000001
#define MKF_AVAILABLE                                                0x00000002
#define MKF_HOTKEYACTIVE                                             0x00000004
#define MKF_CONFIRMHOTKEY                                            0x00000008
#define MKF_HOTKEYSOUND                                              0x00000010
#define MKF_INDICATOR                                                0x00000020
#define MKF_MODIFIERS                                                0x00000040
#define MKF_REPLACENUMBERS                                           0x00000080
#define MKF_LEFTBUTTONSEL                                            0x10000000
#define MKF_RIGHTBUTTONSEL                                           0x20000000
#define MKF_LEFTBUTTONDOWN                                           0x01000000
#define MKF_RIGHTBUTTONDOWN                                          0x02000000
#define MKF_MOUSEMODE                                                0x80000000

#define ATF_TIMEOUTON                                                0x00000001
#define ATF_ONOFFFEEDBACK                                            0x00000002

#define SSGF_NONE                                                    0
#define SSGF_DISPLAY                                                 3

#define SSTF_NONE                                                    0
#define SSTF_CHARS                                                   1
#define SSTF_BORDER                                                  2
#define SSTF_DISPLAY                                                 3

#define SSWF_NONE                                                    0
#define SSWF_TITLE                                                   1
#define SSWF_WINDOW                                                  2
#define SSWF_DISPLAY                                                 3
#define SSWF_CUSTOM                                                  4

#define SSF_SOUNDSENTRYON                                            0x00000001
#define SSF_AVAILABLE                                                0x00000002
#define SSF_INDICATOR                                                0x00000004

#ifndef CCHDEVICENAME
#define CCHDEVICENAME                                                32
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if _WIN32_WINNT >= 0x0600
// #endif

// #ifdef WINNT
// #endif
// #if _WIN32_WINNT >= 0x0600
// #endif

// #define GetMonitorInfo                                               __MINGW_NAME_AW(GetMonitorInfo)

// #endif

#define TKF_TOGGLEKEYSON                                             0x00000001
#define TKF_AVAILABLE                                                0x00000002
#define TKF_HOTKEYACTIVE                                             0x00000004
#define TKF_CONFIRMHOTKEY                                            0x00000008
#define TKF_HOTKEYSOUND                                              0x00000010
#define TKF_INDICATOR                                                0x00000020

#define SLE_ERROR                                                    0x00000001
#define SLE_MINORERROR                                               0x00000002
#define SLE_WARNING                                                  0x00000003

#define MONITOR_DEFAULTTONULL                                        0x00000000
#define MONITOR_DEFAULTTOPRIMARY                                     0x00000001
#define MONITOR_DEFAULTTONEAREST                                     0x00000002

#define MONITORINFOF_PRIMARY                                         0x00000001

#ifndef NOWINABLE

#define WINEVENT_OUTOFCONTEXT                                        0x0000
#define WINEVENT_SKIPOWNTHREAD                                       0x0001
#define WINEVENT_SKIPOWNPROCESS                                      0x0002
#define WINEVENT_INCONTEXT                                           0x0004

#define CHILDID_SELF                                                 0
#define INDEXID_OBJECT                                               0
#define INDEXID_CONTAINER                                            0

#define OBJID_WINDOW                                                 (0x00000000)
#define OBJID_SYSMENU                                                (0xFFFFFFFF)
#define OBJID_TITLEBAR                                               (0xFFFFFFFE)
#define OBJID_MENU                                                   (0xFFFFFFFD)
#define OBJID_CLIENT                                                 (0xFFFFFFFC)
#define OBJID_VSCROLL                                                (0xFFFFFFFB)
#define OBJID_HSCROLL                                                (0xFFFFFFFA)
#define OBJID_SIZEGRIP                                               (0xFFFFFFF9)
#define OBJID_CARET                                                  (0xFFFFFFF8)
#define OBJID_CURSOR                                                 (0xFFFFFFF7)
#define OBJID_ALERT                                                  (0xFFFFFFF6)
#define OBJID_SOUND                                                  (0xFFFFFFF5)
#define OBJID_QUERYCLASSNAMEIDX                                      (0xFFFFFFF4)
#define OBJID_NATIVEOM                                               (0xFFFFFFF0)

#define EVENT_MIN                                                    0x00000001
#define EVENT_MAX                                                    0x7FFFFFFF

#define EVENT_SYSTEM_SOUND                                           0x0001
#define EVENT_SYSTEM_ALERT                                           0x0002
#define EVENT_SYSTEM_FOREGROUND                                      0x0003
#define EVENT_SYSTEM_MENUSTART                                       0x0004
#define EVENT_SYSTEM_MENUEND                                         0x0005
#define EVENT_SYSTEM_MENUPOPUPSTART                                  0x0006
#define EVENT_SYSTEM_MENUPOPUPEND                                    0x0007
#define EVENT_SYSTEM_CAPTURESTART                                    0x0008
#define EVENT_SYSTEM_CAPTUREEND                                      0x0009
#define EVENT_SYSTEM_MOVESIZESTART                                   0x000A
#define EVENT_SYSTEM_MOVESIZEEND                                     0x000B
#define EVENT_SYSTEM_CONTEXTHELPSTART                                0x000C
#define EVENT_SYSTEM_CONTEXTHELPEND                                  0x000D
#define EVENT_SYSTEM_DRAGDROPSTART                                   0x000E
#define EVENT_SYSTEM_DRAGDROPEND                                     0x000F
#define EVENT_SYSTEM_DIALOGSTART                                     0x0010
#define EVENT_SYSTEM_DIALOGEND                                       0x0011
#define EVENT_SYSTEM_SCROLLINGSTART                                  0x0012
#define EVENT_SYSTEM_SCROLLINGEND                                    0x0013
#define EVENT_SYSTEM_SWITCHSTART                                     0x0014
#define EVENT_SYSTEM_SWITCHEND                                       0x0015
#define EVENT_SYSTEM_MINIMIZESTART                                   0x0016
#define EVENT_SYSTEM_MINIMIZEEND                                     0x0017
// #if _WIN32_WINNT >= 0x0600
#define EVENT_SYSTEM_DESKTOPSWITCH                                   0x0020
// #endif
// #if _WIN32_WINNT >= 0x0602
#define EVENT_SYSTEM_SWITCHER_APPGRABBED                             0x0024
#define EVENT_SYSTEM_SWITCHER_APPOVERTARGET                          0x0025
#define EVENT_SYSTEM_SWITCHER_APPDROPPED                             0x0026
#define EVENT_SYSTEM_SWITCHER_CANCELLED                              0x0027
// #endif
// #if _WIN32_WINNT >= 0x0602
#define EVENT_SYSTEM_IME_KEY_NOTIFICATION                            0x0029
// #endif
// #if _WIN32_WINNT >= 0x0601
#define EVENT_SYSTEM_END                                             0x00ff
#define EVENT_OEM_DEFINED_START                                      0x0101
#define EVENT_OEM_DEFINED_END                                        0x01ff
#define EVENT_UIA_EVENTID_START                                      0x4e00
#define EVENT_UIA_EVENTID_END                                        0x4eff
#define EVENT_UIA_PROPID_START                                       0x7500
#define EVENT_UIA_PROPID_END                                         0x75ff
// #endif

#define EVENT_CONSOLE_CARET                                          0x4001
#define EVENT_CONSOLE_UPDATE_REGION                                  0x4002
#define EVENT_CONSOLE_UPDATE_SIMPLE                                  0x4003
#define EVENT_CONSOLE_UPDATE_SCROLL                                  0x4004
#define EVENT_CONSOLE_LAYOUT                                         0x4005
#define EVENT_CONSOLE_START_APPLICATION                              0x4006
#define EVENT_CONSOLE_END_APPLICATION                                0x4007

#ifdef _WIN64
#define CONSOLE_APPLICATION_16BIT                                    0x0000
#else
#define CONSOLE_APPLICATION_16BIT                                    0x0001
#endif
#define CONSOLE_CARET_SELECTION                                      0x0001
#define CONSOLE_CARET_VISIBLE                                        0x0002
// #if _WIN32_WINNT >= 0x0601
#define EVENT_CONSOLE_END                                            0x40ff
// #endif

#define EVENT_OBJECT_CREATE                                          0x8000
#define EVENT_OBJECT_DESTROY                                         0x8001
#define EVENT_OBJECT_SHOW                                            0x8002
#define EVENT_OBJECT_HIDE                                            0x8003
#define EVENT_OBJECT_REORDER                                         0x8004
#define EVENT_OBJECT_FOCUS                                           0x8005
#define EVENT_OBJECT_SELECTION                                       0x8006
#define EVENT_OBJECT_SELECTIONADD                                    0x8007
#define EVENT_OBJECT_SELECTIONREMOVE                                 0x8008
#define EVENT_OBJECT_SELECTIONWITHIN                                 0x8009
#define EVENT_OBJECT_STATECHANGE                                     0x800A
#define EVENT_OBJECT_LOCATIONCHANGE                                  0x800B
#define EVENT_OBJECT_NAMECHANGE                                      0x800C
#define EVENT_OBJECT_DESCRIPTIONCHANGE                               0x800D
#define EVENT_OBJECT_VALUECHANGE                                     0x800E
#define EVENT_OBJECT_PARENTCHANGE                                    0x800F
#define EVENT_OBJECT_HELPCHANGE                                      0x8010
#define EVENT_OBJECT_DEFACTIONCHANGE                                 0x8011
#define EVENT_OBJECT_ACCELERATORCHANGE                               0x8012
// #if _WIN32_WINNT >= 0x0600
#define EVENT_OBJECT_INVOKED                                         0x8013
#define EVENT_OBJECT_TEXTSELECTIONCHANGED                            0x8014
#define EVENT_OBJECT_CONTENTSCROLLED                                 0x8015
// #endif
// #if _WIN32_WINNT >= 0x0601
#define EVENT_SYSTEM_ARRANGMENTPREVIEW                               0x8016
// #endif
// #if _WIN32_WINNT >= 0x0602
#define EVENT_OBJECT_CLOAKED                                         0x8017
#define EVENT_OBJECT_UNCLOAKED                                       0x8018
#define EVENT_OBJECT_LIVEREGIONCHANGED                               0x8019
#define EVENT_OBJECT_HOSTEDOBJECTSINVALIDATED                        0x8020
#define EVENT_OBJECT_DRAGSTART                                       0x8021
#define EVENT_OBJECT_DRAGCANCEL                                      0x8022
#define EVENT_OBJECT_DRAGCOMPLETE                                    0x8023
#define EVENT_OBJECT_DRAGENTER                                       0x8024
#define EVENT_OBJECT_DRAGLEAVE                                       0x8025
#define EVENT_OBJECT_DRAGDROPPED                                     0x8026
#define EVENT_OBJECT_IME_SHOW                                        0x8027
#define EVENT_OBJECT_IME_HIDE                                        0x8028
#define EVENT_OBJECT_IME_CHANGE                                      0x8029
// #endif

// #if _WIN32_WINNT >= 0x0601
#define EVENT_OBJECT_END                                             0x80ff
#define EVENT_AIA_START                                              0xa000
#define EVENT_AIA_END                                                0xafff
// #endif

#define SOUND_SYSTEM_STARTUP                                         1
#define SOUND_SYSTEM_SHUTDOWN                                        2
#define SOUND_SYSTEM_BEEP                                            3
#define SOUND_SYSTEM_ERROR                                           4
#define SOUND_SYSTEM_QUESTION                                        5
#define SOUND_SYSTEM_WARNING                                         6
#define SOUND_SYSTEM_INFORMATION                                     7
#define SOUND_SYSTEM_MAXIMIZE                                        8
#define SOUND_SYSTEM_MINIMIZE                                        9
#define SOUND_SYSTEM_RESTOREUP                                       10
#define SOUND_SYSTEM_RESTOREDOWN                                     11
#define SOUND_SYSTEM_APPSTART                                        12
#define SOUND_SYSTEM_FAULT                                           13
#define SOUND_SYSTEM_APPEND                                          14
#define SOUND_SYSTEM_MENUCOMMAND                                     15
#define SOUND_SYSTEM_MENUPOPUP                                       16
#define CSOUND_SYSTEM                                                16

#define ALERT_SYSTEM_INFORMATIONAL                                   1
#define ALERT_SYSTEM_WARNING                                         2
#define ALERT_SYSTEM_ERROR                                           3
#define ALERT_SYSTEM_QUERY                                           4
#define ALERT_SYSTEM_CRITICAL                                        5
#define CALERT_SYSTEM                                                6

#define GUI_CARETBLINKING                                            0x00000001
#define GUI_INMOVESIZE                                               0x00000002
#define GUI_INMENUMODE                                               0x00000004
#define GUI_SYSTEMMENUMODE                                           0x00000008
#define GUI_POPUPMENUMODE                                            0x00000010
#ifdef _WIN64
#define GUI_16BITTASK                                                0x00000000
#else
#define GUI_16BITTASK                                                0x00000020
#endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetWindowModuleFileName                                      __MINGW_NAME_AW(GetWindowModuleFileName)

// #if _WIN32_WINNT >= 0x0600
#define USER_DEFAULT_SCREEN_DPI                                      96
// #endif
// #endif

#ifndef NO_STATE_FLAGS
#define STATE_SYSTEM_UNAVAILABLE                                     0x00000001
#define STATE_SYSTEM_SELECTED                                        0x00000002
#define STATE_SYSTEM_FOCUSED                                         0x00000004
#define STATE_SYSTEM_PRESSED                                         0x00000008
#define STATE_SYSTEM_CHECKED                                         0x00000010
#define STATE_SYSTEM_MIXED                                           0x00000020
#define STATE_SYSTEM_INDETERMINATE                                   STATE_SYSTEM_MIXED
#define STATE_SYSTEM_READONLY                                        0x00000040
#define STATE_SYSTEM_HOTTRACKED                                      0x00000080
#define STATE_SYSTEM_DEFAULT                                         0x00000100
#define STATE_SYSTEM_EXPANDED                                        0x00000200
#define STATE_SYSTEM_COLLAPSED                                       0x00000400
#define STATE_SYSTEM_BUSY                                            0x00000800
#define STATE_SYSTEM_FLOATING                                        0x00001000
#define STATE_SYSTEM_MARQUEED                                        0x00002000
#define STATE_SYSTEM_ANIMATED                                        0x00004000
#define STATE_SYSTEM_INVISIBLE                                       0x00008000
#define STATE_SYSTEM_OFFSCREEN                                       0x00010000
#define STATE_SYSTEM_SIZEABLE                                        0x00020000
#define STATE_SYSTEM_MOVEABLE                                        0x00040000
#define STATE_SYSTEM_SELFVOICING                                     0x00080000
#define STATE_SYSTEM_FOCUSABLE                                       0x00100000
#define STATE_SYSTEM_SELECTABLE                                      0x00200000
#define STATE_SYSTEM_LINKED                                          0x00400000
#define STATE_SYSTEM_TRAVERSED                                       0x00800000
#define STATE_SYSTEM_MULTISELECTABLE                                 0x01000000
#define STATE_SYSTEM_EXTSELECTABLE                                   0x02000000
#define STATE_SYSTEM_ALERT_LOW                                       0x04000000
#define STATE_SYSTEM_ALERT_MEDIUM                                    0x08000000
#define STATE_SYSTEM_ALERT_HIGH                                      0x10000000
#define STATE_SYSTEM_PROTECTED                                       0x20000000
#define STATE_SYSTEM_VALID                                           0x3FFFFFFF
#endif

#define CCHILDREN_TITLEBAR                                           5
#define CCHILDREN_SCROLLBAR                                          5

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

#define CURSOR_SHOWING                                               0x00000001
// #if WINVER >= 0x0602
#define CURSOR_SUPPRESSED                                            0x00000002
// #endif

#define WS_ACTIVECAPTION                                             0x0001

// #endif

#define GA_PARENT                                                    1
#define GA_ROOT                                                      2
#define GA_ROOTOWNER                                                 3

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define RealGetWindowClass                                           __MINGW_NAME_AW(RealGetWindowClass)

// #define GetAltTabInfo                                                __MINGW_NAME_AW(GetAltTabInfo)

// #endif
#endif

// #define GET_RAWINPUT_CODE_WPARAM(wParam)                             ((wParam)&0xff)

#define RIM_INPUT                                                    0
#define RIM_INPUTSINK                                                1

#define RIM_TYPEMOUSE                                                0
#define RIM_TYPEKEYBOARD                                             1
#define RIM_TYPEHID                                                  2

#define RI_MOUSE_LEFT_BUTTON_DOWN                                    0x0001
#define RI_MOUSE_LEFT_BUTTON_UP                                      0x0002
#define RI_MOUSE_RIGHT_BUTTON_DOWN                                   0x0004
#define RI_MOUSE_RIGHT_BUTTON_UP                                     0x0008
#define RI_MOUSE_MIDDLE_BUTTON_DOWN                                  0x0010
#define RI_MOUSE_MIDDLE_BUTTON_UP                                    0x0020
#define RI_MOUSE_BUTTON_4_DOWN                                       0x0040
#define RI_MOUSE_BUTTON_4_UP                                         0x0080
#define RI_MOUSE_BUTTON_5_DOWN                                       0x0100
#define RI_MOUSE_BUTTON_5_UP                                         0x0200
#define RI_MOUSE_WHEEL                                               0x0400

#define RI_MOUSE_BUTTON_1_DOWN                                       RI_MOUSE_LEFT_BUTTON_DOWN
#define RI_MOUSE_BUTTON_1_UP                                         RI_MOUSE_LEFT_BUTTON_UP
#define RI_MOUSE_BUTTON_2_DOWN                                       RI_MOUSE_RIGHT_BUTTON_DOWN
#define RI_MOUSE_BUTTON_2_UP                                         RI_MOUSE_RIGHT_BUTTON_UP
#define RI_MOUSE_BUTTON_3_DOWN                                       RI_MOUSE_MIDDLE_BUTTON_DOWN
#define RI_MOUSE_BUTTON_3_UP                                         RI_MOUSE_MIDDLE_BUTTON_UP

#define MOUSE_MOVE_RELATIVE                                          0
#define MOUSE_MOVE_ABSOLUTE                                          1
#define MOUSE_VIRTUAL_DESKTOP                                        0x02
#define MOUSE_ATTRIBUTES_CHANGED                                     0x04
// #if WINVER >= 0x0600
#define MOUSE_MOVE_NOCOALESCE                                        0x08
// #endif

#define KEYBOARD_OVERRUN_MAKE_CODE                                   0xFF

#define RI_KEY_MAKE                                                  0
#define RI_KEY_BREAK                                                 1
#define RI_KEY_E0                                                    2
#define RI_KEY_E1                                                    4
#define RI_KEY_TERMSRV_SET_LED                                       8
#define RI_KEY_TERMSRV_SHADOW                                        0x10

// #ifdef _WIN64
// #define RAWINPUT_ALIGN(x)                                            (((x)+sizeof(QWORD)-1)&~(sizeof(QWORD)-1))
// #else
// #define RAWINPUT_ALIGN(x)                                            (((x)+sizeof(DWORD)-1)&~(sizeof(DWORD)-1))
// #endif

// #define NEXTRAWINPUTBLOCK(ptr)                                       ((PRAWINPUT)RAWINPUT_ALIGN((ULONG_PTR)((PBYTE)(ptr)+(ptr)->header.dwSize)))

#define RID_INPUT                                                    0x10000003
#define RID_HEADER                                                   0x10000005

#define RIDI_PREPARSEDDATA                                           0x20000005
#define RIDI_DEVICENAME                                              0x20000007
#define RIDI_DEVICEINFO                                              0x2000000b

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #define GetRawInputDeviceInfo                                        __MINGW_NAME_AW(GetRawInputDeviceInfo)
// #endif

#define RIDEV_REMOVE                                                 0x00000001
#define RIDEV_EXCLUDE                                                0x00000010
#define RIDEV_PAGEONLY                                               0x00000020
#define RIDEV_NOLEGACY                                               0x00000030
#define RIDEV_INPUTSINK                                              0x00000100
#define RIDEV_CAPTUREMOUSE                                           0x00000200
#define RIDEV_NOHOTKEYS                                              0x00000200
#define RIDEV_APPKEYS                                                0x00000400
#define RIDEV_EXINPUTSINK                                            0x00001000
#define RIDEV_DEVNOTIFY                                              0x00002000

#define RIDEV_EXMODEMASK                                             0x000000F0
// #define RIDEV_EXMODE(mode)                                           ((mode)&RIDEV_EXMODEMASK)

#define GIDC_ARRIVAL                                                 1
#define GIDC_REMOVAL                                                 2

// #if _WIN32_WINNT >= 0x0601
// #define GET_DEVICE_CHANGE_WPARAM(wParam)                             (LOWORD(wParam))
// #else
// #define GET_DEVICE_CHANGE_LPARAM(lParam)                             (LOWORD(lParam))
// #endif

// #if WINVER >= 0x0602
#define POINTER_DEVICE_PRODUCT_STRING_MAX                            520
#define PDC_ARRIVAL                                                  0x001
#define PDC_REMOVAL                                                  0x002
#define PDC_ORIENTATION_0                                            0x004
#define PDC_ORIENTATION_90                                           0x008
#define PDC_ORIENTATION_180                                          0x010
#define PDC_ORIENTATION_270                                          0x020
#define PDC_MODE_DEFAULT                                             0x040
#define PDC_MODE_CENTERED                                            0x080
#define PDC_MAPPING_CHANGE                                           0x100
#define PDC_RESOLUTION                                               0x200
#define PDC_ORIGIN                                                   0x400
#define PDC_MODE_ASPECTRATIOPRESERVED                                0x800

// #endif

// #if WINVER >= 0x0600
#define MSGFLT_ADD                                                   1
#define MSGFLT_REMOVE                                                2

// #endif
// #if WINVER >= 0x0601
#define MSGFLTINFO_NONE                                              (0)
#define MSGFLTINFO_ALREADYALLOWED_FORWND                             (1)
#define MSGFLTINFO_ALREADYDISALLOWED_FORWND                          (2)
#define MSGFLTINFO_ALLOWED_HIGHER                                    (3)

#define MSGFLT_RESET                                                 (0)
#define MSGFLT_ALLOW                                                 (1)
#define MSGFLT_DISALLOW                                              (2)

// #endif

// #if WINVER >= 0x0601
#define GF_BEGIN                                                     0x00000001
#define GF_INERTIA                                                   0x00000002
#define GF_END                                                       0x00000004

#define GID_BEGIN                                                    1
#define GID_END                                                      2
#define GID_ZOOM                                                     3
#define GID_PAN                                                      4
#define GID_ROTATE                                                   5
#define GID_TWOFINGERTAP                                             6
#define GID_PRESSANDTAP                                              7
#define GID_ROLLOVER                                                 GID_PRESSANDTAP

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)

// #define GID_ROTATE_ANGLE_TO_ARGUMENT(_arg_)                          ((USHORT)((((_arg_)+2.0*3.14159265)/(4.0*3.14159265))*65535.0))
// #define GID_ROTATE_ANGLE_FROM_ARGUMENT(_arg_)                        ((((double)(_arg_)/65535.0)*4.0*3.14159265)-2.0*3.14159265)

// #endif

#define GC_ALLGESTURES                                               0x00000001
#define GC_ZOOM                                                      0x00000001

#define GC_PAN                                                       0x00000001
#define GC_PAN_WITH_SINGLE_FINGER_VERTICALLY                         0x00000002
#define GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY                       0x00000004
#define GC_PAN_WITH_GUTTER                                           0x00000008
#define GC_PAN_WITH_INERTIA                                          0x00000010

#define GC_ROTATE                                                    0x00000001
#define GC_TWOFINGERTAP                                              0x00000001
#define GC_PRESSANDTAP                                               0x00000001
#define GC_ROLLOVER                                                  GC_PRESSANDTAP

#define GESTURECONFIGMAXCOUNT                                        256

#define GCF_INCLUDE_ANCESTORS                                        0x00000001

// #endif

// #if WINVER >= 0x0601
#define NID_INTEGRATED_TOUCH                                         0x00000001
#define NID_EXTERNAL_TOUCH                                           0x00000002
#define NID_INTEGRATED_PEN                                           0x00000004
#define NID_EXTERNAL_PEN                                             0x00000008
#define NID_MULTI_INPUT                                              0x00000040
#define NID_READY                                                    0x00000080
// #endif

#define MAX_STR_BLOCKREASON                                          256

// #if WINVER >= 0x0601

// #ifndef __WIDL__
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #ifndef __WIDL__
// #endif
// #endif
// #endif

// #if WINAPI_FAMILY_PARTITION (WINAPI_PARTITION_DESKTOP)
// #if (_WIN32_WINNT >= 0x0602)
// #endif //(_WIN32_WINNT >= 0x0602)
// #endif

#endif // _WINAPI_WINUSER_
