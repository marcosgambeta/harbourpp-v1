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
#include "hbapi.h"

/*
WINUSERAPI int WINAPI wvsprintfA(LPSTR,LPCSTR,va_list arglist)
*/

/*
WINUSERAPI int WINAPI wvsprintfW(LPWSTR,LPCWSTR,va_list arglist)
*/

/*
WINUSERAPI int WINAPIV wsprintfA(LPSTR,LPCSTR,...)
*/

/*
WINUSERAPI int WINAPIV wsprintfW(LPWSTR,LPCWSTR,...)
*/

/*
WINUSERAPI HKL WINAPI LoadKeyboardLayoutA(LPCSTR pwszKLID,UINT Flags)
*/
HB_FUNC( WINAPI_LOADKEYBOARDLAYOUTA )
{
  hb_retptr( ( HKL ) LoadKeyboardLayoutA( ( LPCSTR ) hb_parc( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HKL WINAPI LoadKeyboardLayoutW(LPCWSTR pwszKLID,UINT Flags)
*/
HB_FUNC( WINAPI_LOADKEYBOARDLAYOUTW )
{
  hb_retptr( ( HKL ) LoadKeyboardLayoutW( ( LPCWSTR ) hb_parc( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HKL WINAPI ActivateKeyboardLayout(HKL hkl,UINT Flags)
*/
HB_FUNC( WINAPI_ACTIVATEKEYBOARDLAYOUT )
{
  hb_retptr( ( HKL ) ActivateKeyboardLayout( ( HKL ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI int WINAPI ToUnicodeEx(UINT wVirtKey,UINT wScanCode,CONST BYTE *lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags,HKL dwhkl)
*/

/*
WINUSERAPI WINBOOL WINAPI UnloadKeyboardLayout(HKL hkl)
*/
HB_FUNC( WINAPI_UNLOADKEYBOARDLAYOUT )
{
  hb_retl( ( WINBOOL ) UnloadKeyboardLayout( ( HKL ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameA(LPSTR pwszKLID)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUTNAMEA )
{
  hb_retl( ( WINBOOL ) GetKeyboardLayoutNameA( ( LPSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameW(LPWSTR pwszKLID)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUTNAMEW )
{
  hb_retl( ( WINBOOL ) GetKeyboardLayoutNameW( ( LPWSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI int WINAPI GetKeyboardLayoutList(int nBuff,HKL *lpList)
*/

/*
WINUSERAPI HKL WINAPI GetKeyboardLayout(DWORD idThread)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUT )
{
  hb_retptr( ( HKL ) GetKeyboardLayout( ( DWORD ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI int WINAPI GetMouseMovePointsEx(UINT cbSize,LPMOUSEMOVEPOINT lppt,LPMOUSEMOVEPOINT lpptBuf,int nBufPoints,DWORD resolution)
*/

/*
WINUSERAPI HDESK WINAPI CreateDesktopA(LPCSTR lpszDesktop,LPCSTR lpszDevice,LPDEVMODEA pDevmode,DWORD dwFlags,ACCESS_MASK dwDesiredAccess,LPSECURITY_ATTRIBUTES lpsa)
*/

/*
WINUSERAPI HDESK WINAPI CreateDesktopW(LPCWSTR lpszDesktop,LPCWSTR lpszDevice,LPDEVMODEW pDevmode,DWORD dwFlags,ACCESS_MASK dwDesiredAccess,LPSECURITY_ATTRIBUTES lpsa)
*/

/*
WINUSERAPI HDESK WINAPI CreateDesktopExA (LPCSTR lpszDesktop, LPCSTR lpszDevice, DEVMODEA *pDevmode, DWORD dwFlags, ACCESS_MASK dwDesiredAccess, LPSECURITY_ATTRIBUTES lpsa, ULONG ulHeapSize, PVOID pvoid)
*/

/*
WINUSERAPI HDESK WINAPI CreateDesktopExW (LPCWSTR lpszDesktop, LPCWSTR lpszDevice, DEVMODEW *pDevmode, DWORD dwFlags, ACCESS_MASK dwDesiredAccess, LPSECURITY_ATTRIBUTES lpsa, ULONG ulHeapSize, PVOID pvoid)
*/

/*
WINUSERAPI HDESK WINAPI OpenDesktopA(LPCSTR lpszDesktop,DWORD dwFlags,WINBOOL fInherit,ACCESS_MASK dwDesiredAccess)
*/

/*
WINUSERAPI HDESK WINAPI OpenDesktopW(LPCWSTR lpszDesktop,DWORD dwFlags,WINBOOL fInherit,ACCESS_MASK dwDesiredAccess)
*/

/*
WINUSERAPI HDESK WINAPI OpenInputDesktop(DWORD dwFlags,WINBOOL fInherit,ACCESS_MASK dwDesiredAccess)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDesktopsA(HWINSTA hwinsta,DESKTOPENUMPROCA lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDesktopsW(HWINSTA hwinsta,DESKTOPENUMPROCW lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDesktopWindows(HDESK hDesktop,WNDENUMPROC lpfn,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI SwitchDesktop(HDESK hDesktop)
*/
HB_FUNC( WINAPI_SWITCHDESKTOP )
{
  hb_retl( ( WINBOOL ) SwitchDesktop( ( HDESK ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetThreadDesktop(HDESK hDesktop)
*/
HB_FUNC( WINAPI_SETTHREADDESKTOP )
{
  hb_retl( ( WINBOOL ) SetThreadDesktop( ( HDESK ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CloseDesktop(HDESK hDesktop)
*/
HB_FUNC( WINAPI_CLOSEDESKTOP )
{
  hb_retl( ( WINBOOL ) CloseDesktop( ( HDESK ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HDESK WINAPI GetThreadDesktop(DWORD dwThreadId)
*/
HB_FUNC( WINAPI_GETTHREADDESKTOP )
{
  hb_retptr( ( HDESK ) GetThreadDesktop( ( DWORD ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI HWINSTA WINAPI CreateWindowStationA(LPCSTR lpwinsta,DWORD dwFlags,ACCESS_MASK dwDesiredAccess,LPSECURITY_ATTRIBUTES lpsa)
*/

/*
WINUSERAPI HWINSTA WINAPI CreateWindowStationW(LPCWSTR lpwinsta,DWORD dwFlags,ACCESS_MASK dwDesiredAccess,LPSECURITY_ATTRIBUTES lpsa)
*/

/*
WINUSERAPI HWINSTA WINAPI OpenWindowStationA(LPCSTR lpszWinSta,WINBOOL fInherit,ACCESS_MASK dwDesiredAccess)
*/

/*
WINUSERAPI HWINSTA WINAPI OpenWindowStationW(LPCWSTR lpszWinSta,WINBOOL fInherit,ACCESS_MASK dwDesiredAccess)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumWindowStationsA(WINSTAENUMPROCA lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumWindowStationsW(WINSTAENUMPROCW lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI CloseWindowStation(HWINSTA hWinSta)
*/
HB_FUNC( WINAPI_CLOSEWINDOWSTATION )
{
  hb_retl( ( WINBOOL ) CloseWindowStation( ( HWINSTA ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessWindowStation(HWINSTA hWinSta)
*/
HB_FUNC( WINAPI_SETPROCESSWINDOWSTATION )
{
  hb_retl( ( WINBOOL ) SetProcessWindowStation( ( HWINSTA ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWINSTA WINAPI GetProcessWindowStation(VOID)
*/
HB_FUNC( WINAPI_GETPROCESSWINDOWSTATION )
{
  hb_retptr( ( HWINSTA ) GetProcessWindowStation() );
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectSecurity(HANDLE hObj,PSECURITY_INFORMATION pSIRequested,PSECURITY_DESCRIPTOR pSID)
*/

/*
WINUSERAPI WINBOOL WINAPI GetUserObjectSecurity(HANDLE hObj,PSECURITY_INFORMATION pSIRequested,PSECURITY_DESCRIPTOR pSID,DWORD nLength,LPDWORD lpnLengthNeeded)
*/

/*
WINUSERAPI WINBOOL WINAPI GetUserObjectInformationA(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength,LPDWORD lpnLengthNeeded)
*/
HB_FUNC( WINAPI_GETUSEROBJECTINFORMATIONA )
{
  hb_retl( ( WINBOOL ) GetUserObjectInformationA( ( HANDLE ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( DWORD ) hb_parnl( 4 ), ( LPDWORD ) hb_parptr( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength,LPDWORD lpnLengthNeeded)
*/
HB_FUNC( WINAPI_GETUSEROBJECTINFORMATIONW )
{
  hb_retl( ( WINBOOL ) GetUserObjectInformationW( ( HANDLE ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( DWORD ) hb_parnl( 4 ), ( LPDWORD ) hb_parptr( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationA(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WINAPI_SETUSEROBJECTINFORMATIONA )
{
  hb_retl( ( WINBOOL ) SetUserObjectInformationA( ( HANDLE ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( DWORD ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WINAPI_SETUSEROBJECTINFORMATIONW )
{
  hb_retl( ( WINBOOL ) SetUserObjectInformationW( ( HANDLE ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( DWORD ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsHungAppWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_ISHUNGAPPWINDOW )
{
  hb_retl( ( WINBOOL ) IsHungAppWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI VOID WINAPI DisableProcessWindowsGhosting(VOID)
*/
HB_FUNC( WINAPI_DISABLEPROCESSWINDOWSGHOSTING )
{
  DisableProcessWindowsGhosting();
}

/*
WINUSERAPI UINT WINAPI RegisterWindowMessageA(LPCSTR lpString)
*/
HB_FUNC( WINAPI_REGISTERWINDOWMESSAGEA )
{
  hb_retni( ( UINT ) RegisterWindowMessageA( ( LPCSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI UINT WINAPI RegisterWindowMessageW(LPCWSTR lpString)
*/
HB_FUNC( WINAPI_REGISTERWINDOWMESSAGEW )
{
  hb_retni( ( UINT ) RegisterWindowMessageW( ( LPCWSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI TrackMouseEvent(LPTRACKMOUSEEVENT lpEventTrack)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawEdge(HDC hdc,LPRECT qrc,UINT edge,UINT grfFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawFrameControl(HDC,LPRECT,UINT,UINT)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawCaption(HWND hwnd,HDC hdc,CONST RECT *lprect,UINT flags)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawAnimatedRects(HWND hwnd,int idAni,CONST RECT *lprcFrom,CONST RECT *lprcTo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/

/*
WINUSERAPI WINBOOL WINAPI TranslateMessage(CONST MSG *lpMsg)
*/

/*
WINUSERAPI LRESULT WINAPI DispatchMessageA(CONST MSG *lpMsg)
*/

/*
WINUSERAPI LRESULT WINAPI DispatchMessageW(CONST MSG *lpMsg)
*/

/*
WINUSERAPI WINBOOL WINAPI SetMessageQueue(int cMessagesMax)
*/
HB_FUNC( WINAPI_SETMESSAGEQUEUE )
{
  hb_retl( ( WINBOOL ) SetMessageQueue( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI PeekMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/

/*
WINUSERAPI WINBOOL WINAPI PeekMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterHotKey(HWND hWnd,int id,UINT fsModifiers,UINT vk)
*/
HB_FUNC( WINAPI_REGISTERHOTKEY )
{
  hb_retl( ( WINBOOL ) RegisterHotKey( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterHotKey(HWND hWnd,int id)
*/
HB_FUNC( WINAPI_UNREGISTERHOTKEY )
{
  hb_retl( ( WINBOOL ) UnregisterHotKey( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ExitWindowsEx(UINT uFlags,DWORD dwReason)
*/
HB_FUNC( WINAPI_EXITWINDOWSEX )
{
  hb_retl( ( WINBOOL ) ExitWindowsEx( ( UINT ) hb_parni( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SwapMouseButton(WINBOOL fSwap)
*/
HB_FUNC( WINAPI_SWAPMOUSEBUTTON )
{
  hb_retl( ( WINBOOL ) SwapMouseButton( ( WINBOOL ) hb_parl( 1 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetMessagePos(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGEPOS )
{
  hb_retnl( ( DWORD ) GetMessagePos() );
}

/*
WINUSERAPI LONG WINAPI GetMessageTime(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGETIME )
{
  hb_retnl( ( LONG ) GetMessageTime() );
}

/*
WINUSERAPI LPARAM WINAPI GetMessageExtraInfo(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGEEXTRAINFO )
{
  hb_retnl( ( LPARAM ) GetMessageExtraInfo() );
}

/*
WINUSERAPI DWORD WINAPI GetUnpredictedMessagePos (VOID)
*/

/*
WINUSERAPI WINBOOL WINAPI IsWow64Message(VOID)
*/
HB_FUNC( WINAPI_ISWOW64MESSAGE )
{
  hb_retl( ( WINBOOL ) IsWow64Message() );
}

/*
WINUSERAPI LPARAM WINAPI SetMessageExtraInfo(LPARAM lParam)
*/
HB_FUNC( WINAPI_SETMESSAGEEXTRAINFO )
{
  hb_retnl( ( LPARAM ) SetMessageExtraInfo( ( LPARAM ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI SendMessageA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDMESSAGEA )
{
  hb_retnl( ( LRESULT ) SendMessageA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI SendMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDMESSAGEW )
{
  hb_retnl( ( LRESULT ) SendMessageW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI SendMessageTimeoutA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,UINT fuFlags,UINT uTimeout,PDWORD_PTR lpdwResult)
*/

/*
WINUSERAPI LRESULT WINAPI SendMessageTimeoutW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,UINT fuFlags,UINT uTimeout,PDWORD_PTR lpdwResult)
*/

/*
WINUSERAPI WINBOOL WINAPI SendNotifyMessageA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDNOTIFYMESSAGEA )
{
  hb_retl( ( WINBOOL ) SendNotifyMessageA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SendNotifyMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDNOTIFYMESSAGEW )
{
  hb_retl( ( WINBOOL ) SendNotifyMessageW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SendMessageCallbackA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,SENDASYNCPROC lpResultCallBack,ULONG_PTR dwData)
*/

/*
WINUSERAPI WINBOOL WINAPI SendMessageCallbackW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,SENDASYNCPROC lpResultCallBack,ULONG_PTR dwData)
*/

/*
WINUSERAPI __LONG32 WINAPI BroadcastSystemMessageExA(DWORD flags,LPDWORD lpInfo,UINT Msg,WPARAM wParam,LPARAM lParam,PBSMINFO pbsmInfo)
*/

/*
WINUSERAPI __LONG32 WINAPI BroadcastSystemMessageExW(DWORD flags,LPDWORD lpInfo,UINT Msg,WPARAM wParam,LPARAM lParam,PBSMINFO pbsmInfo)
*/

/*
WINUSERAPI __LONG32 WINAPI BroadcastSystemMessageA(DWORD flags,LPDWORD lpInfo,UINT Msg,WPARAM wParam,LPARAM lParam)
*/

/*
WINUSERAPI __LONG32 WINAPI BroadcastSystemMessageW(DWORD flags,LPDWORD lpInfo,UINT Msg,WPARAM wParam,LPARAM lParam)
*/

/*
WINUSERAPI HPOWERNOTIFY WINAPI RegisterPowerSettingNotification(HANDLE hRecipient, LPCGUID PowerSettingGuid, DWORD Flags)
*/

/*
WINUSERAPI WINBOOL WINAPI UnregisterPowerSettingNotification(HPOWERNOTIFY Handle)
*/
HB_FUNC( WINAPI_UNREGISTERPOWERSETTINGNOTIFICATION )
{
  hb_retl( ( WINBOOL ) UnregisterPowerSettingNotification( ( HPOWERNOTIFY ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HPOWERNOTIFY WINAPI RegisterSuspendResumeNotification(HANDLE hRecipient, DWORD Flags)
*/
HB_FUNC( WINAPI_REGISTERSUSPENDRESUMENOTIFICATION )
{
  hb_retptr( ( HPOWERNOTIFY ) RegisterSuspendResumeNotification( ( HANDLE ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterSuspendResumeNotification(HPOWERNOTIFY Handle)
*/
HB_FUNC( WINAPI_UNREGISTERSUSPENDRESUMENOTIFICATION )
{
  hb_retl( ( WINBOOL ) UnregisterSuspendResumeNotification( ( HPOWERNOTIFY ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI PostMessageA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTMESSAGEA )
{
  hb_retl( ( WINBOOL ) PostMessageA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI PostMessageW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTMESSAGEW )
{
  hb_retl( ( WINBOOL ) PostMessageW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageA(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTTHREADMESSAGEA )
{
  hb_retl( ( WINBOOL ) PostThreadMessageA( ( DWORD ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageW(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTTHREADMESSAGEW )
{
  hb_retl( ( WINBOOL ) PostThreadMessageW( ( DWORD ) hb_parnl( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AttachThreadInput(DWORD idAttach, DWORD idAttachTo, WINBOOL fAttach)
*/
HB_FUNC( WINAPI_ATTACHTHREADINPUT )
{
  hb_retl( ( WINBOOL ) AttachThreadInput( ( DWORD ) hb_parnl( 1 ), ( DWORD ) hb_parnl( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ReplyMessage(LRESULT lResult)
*/
HB_FUNC( WINAPI_REPLYMESSAGE )
{
  hb_retl( ( WINBOOL ) ReplyMessage( ( LRESULT ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI WaitMessage(VOID)
*/
HB_FUNC( WINAPI_WAITMESSAGE )
{
  hb_retl( ( WINBOOL ) WaitMessage() );
}

/*
WINUSERAPI DWORD WINAPI WaitForInputIdle(HANDLE hProcess, DWORD dwMilliseconds)
*/
HB_FUNC( WINAPI_WAITFORINPUTIDLE )
{
  hb_retnl( ( DWORD ) WaitForInputIdle( ( HANDLE ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefWindowProcA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFWINDOWPROCA )
{
  hb_retnl( ( LRESULT ) DefWindowProcA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefWindowProcW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFWINDOWPROCW )
{
  hb_retnl( ( LRESULT ) DefWindowProcW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI VOID WINAPI PostQuitMessage(int nExitCode)
*/
HB_FUNC( WINAPI_POSTQUITMESSAGE )
{
  PostQuitMessage( ( int ) hb_parni( 1 ) );
}

/*
WINUSERAPI WINBOOL WINAPI InSendMessage(VOID)
*/
HB_FUNC( WINAPI_INSENDMESSAGE )
{
  hb_retl( ( WINBOOL ) InSendMessage() );
}

/*
WINUSERAPI DWORD WINAPI InSendMessageEx(LPVOID lpReserved)
*/
HB_FUNC( WINAPI_INSENDMESSAGEEX )
{
  hb_retnl( ( DWORD ) InSendMessageEx( ( LPVOID ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetDoubleClickTime(VOID)
*/
HB_FUNC( WINAPI_GETDOUBLECLICKTIME )
{
  hb_retni( ( UINT ) GetDoubleClickTime() );
}

/*
WINUSERAPI WINBOOL WINAPI SetDoubleClickTime(UINT)
*/
HB_FUNC( WINAPI_SETDOUBLECLICKTIME )
{
  hb_retl( ( WINBOOL ) SetDoubleClickTime( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI ATOM WINAPI RegisterClassA(CONST WNDCLASSA *lpWndClass)
*/

/*
WINUSERAPI ATOM WINAPI RegisterClassW(CONST WNDCLASSW *lpWndClass)
*/

/*
WINUSERAPI WINBOOL WINAPI UnregisterClassA(LPCSTR lpClassName, HINSTANCE hInstance)
*/
HB_FUNC( WINAPI_UNREGISTERCLASSA )
{
  hb_retl( ( WINBOOL ) UnregisterClassA( ( LPCSTR ) hb_parc( 1 ), ( HINSTANCE ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterClassW(LPCWSTR lpClassName, HINSTANCE hInstance)
*/
HB_FUNC( WINAPI_UNREGISTERCLASSW )
{
  hb_retl( ( WINBOOL ) UnregisterClassW( ( LPCWSTR ) hb_parc( 1 ), ( HINSTANCE ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoA(HINSTANCE hInstance, LPCSTR lpClassName, LPWNDCLASSA lpWndClass)
*/

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoW(HINSTANCE hInstance, LPCWSTR lpClassName, LPWNDCLASSW lpWndClass)
*/

/*
WINUSERAPI ATOM WINAPI RegisterClassExA(CONST WNDCLASSEXA *)
*/

/*
WINUSERAPI ATOM WINAPI RegisterClassExW(CONST WNDCLASSEXW *)
*/

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoExA(HINSTANCE hInstance, LPCSTR lpszClass, LPWNDCLASSEXA lpwcx)
*/

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoExW(HINSTANCE hInstance, LPCWSTR lpszClass, LPWNDCLASSEXW lpwcx)
*/

/*
WINUSERAPI LRESULT WINAPI CallWindowProcA (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI LRESULT WINAPI CallWindowProcW (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI LRESULT WINAPI CallWindowProcA (FARPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI LRESULT WINAPI CallWindowProcW (FARPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationA(HANDLE hRecipient,LPVOID NotificationFilter,DWORD Flags)
*/
HB_FUNC( WINAPI_REGISTERDEVICENOTIFICATIONA )
{
  hb_retptr( ( HDEVNOTIFY ) RegisterDeviceNotificationA( ( HANDLE ) hb_parptr( 1 ), ( LPVOID ) hb_parptr( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationW(HANDLE hRecipient,LPVOID NotificationFilter,DWORD Flags)
*/
HB_FUNC( WINAPI_REGISTERDEVICENOTIFICATIONW )
{
  hb_retptr( ( HDEVNOTIFY ) RegisterDeviceNotificationW( ( HANDLE ) hb_parptr( 1 ), ( LPVOID ) hb_parptr( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterDeviceNotification(HDEVNOTIFY Handle)
*/
HB_FUNC( WINAPI_UNREGISTERDEVICENOTIFICATION )
{
  hb_retl( ( WINBOOL ) UnregisterDeviceNotification( ( HDEVNOTIFY ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI CreateWindowExA(DWORD dwExStyle,LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
HB_FUNC( WINAPI_CREATEWINDOWEXA )
{
  hb_retptr( ( HWND ) CreateWindowExA( ( DWORD ) hb_parnl( 1 ), ( LPCSTR ) hb_parc( 2 ), ( LPCSTR ) hb_parc( 3 ), ( DWORD ) hb_parnl( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( int ) hb_parni( 8 ), ( HWND ) hb_parptr( 9 ), ( HMENU ) hb_parptr( 10 ), ( HINSTANCE ) hb_parptr( 11 ), ( LPVOID ) hb_parptr( 12 ) ) );
}

/*
WINUSERAPI HWND WINAPI CreateWindowExW(DWORD dwExStyle,LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
HB_FUNC( WINAPI_CREATEWINDOWEXW )
{
  hb_retptr( ( HWND ) CreateWindowExW( ( DWORD ) hb_parnl( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( LPCWSTR ) hb_parc( 3 ), ( DWORD ) hb_parnl( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( int ) hb_parni( 8 ), ( HWND ) hb_parptr( 9 ), ( HMENU ) hb_parptr( 10 ), ( HINSTANCE ) hb_parptr( 11 ), ( LPVOID ) hb_parptr( 12 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOW )
{
  hb_retl( ( WINBOOL ) IsWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsMenu(HMENU hMenu)
*/
HB_FUNC( WINAPI_ISMENU )
{
  hb_retl( ( WINBOOL ) IsMenu( ( HMENU ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsChild(HWND hWndParent,HWND hWnd)
*/
HB_FUNC( WINAPI_ISCHILD )
{
  hb_retl( ( WINBOOL ) IsChild( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DestroyWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_DESTROYWINDOW )
{
  hb_retl( ( WINBOOL ) DestroyWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindow(HWND hWnd,int nCmdShow)
*/
HB_FUNC( WINAPI_SHOWWINDOW )
{
  hb_retl( ( WINBOOL ) ShowWindow( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AnimateWindow(HWND hWnd,DWORD dwTime,DWORD dwFlags)
*/
HB_FUNC( WINAPI_ANIMATEWINDOW )
{
  hb_retl( ( WINBOOL ) AnimateWindow( ( HWND ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UpdateLayeredWindow (HWND hWnd, HDC hdcDst, POINT *pptDst, SIZE *psize, HDC hdcSrc, POINT *pptSrc, COLORREF crKey, BLENDFUNCTION *pblend, DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI UpdateLayeredWindowIndirect (HWND hWnd, const UPDATELAYEREDWINDOWINFO *pULWInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetLayeredWindowAttributes (HWND hwnd, COLORREF *pcrKey, BYTE *pbAlpha, DWORD *pdwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI PrintWindow (HWND hwnd, HDC hdcBlt, UINT nFlags)
*/
HB_FUNC( WINAPI_PRINTWINDOW )
{
  hb_retl( ( WINBOOL ) PrintWindow( ( HWND ) hb_parptr( 1 ), ( HDC ) hb_parptr( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetLayeredWindowAttributes (HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags)
*/
HB_FUNC( WINAPI_SETLAYEREDWINDOWATTRIBUTES )
{
  hb_retl( ( WINBOOL ) SetLayeredWindowAttributes( ( HWND ) hb_parptr( 1 ), ( COLORREF ) hb_parnl( 2 ), ( BYTE ) hb_parni( 3 ), ( DWORD ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindowAsync (HWND hWnd, int nCmdShow)
*/
HB_FUNC( WINAPI_SHOWWINDOWASYNC )
{
  hb_retl( ( WINBOOL ) ShowWindowAsync( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindow (HWND hWnd, WINBOOL bInvert)
*/
HB_FUNC( WINAPI_FLASHWINDOW )
{
  hb_retl( ( WINBOOL ) FlashWindow( ( HWND ) hb_parptr( 1 ), ( WINBOOL ) hb_parl( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindowEx (PFLASHWINFO pfwi)
*/

/*
WINUSERAPI WINBOOL WINAPI ShowOwnedPopups (HWND hWnd, WINBOOL fShow)
*/
HB_FUNC( WINAPI_SHOWOWNEDPOPUPS )
{
  hb_retl( ( WINBOOL ) ShowOwnedPopups( ( HWND ) hb_parptr( 1 ), ( WINBOOL ) hb_parl( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI OpenIcon (HWND hWnd)
*/
HB_FUNC( WINAPI_OPENICON )
{
  hb_retl( ( WINBOOL ) OpenIcon( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CloseWindow (HWND hWnd)
*/
HB_FUNC( WINAPI_CLOSEWINDOW )
{
  hb_retl( ( WINBOOL ) CloseWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI MoveWindow (HWND hWnd, int X, int Y, int nWidth, int nHeight, WINBOOL bRepaint)
*/
HB_FUNC( WINAPI_MOVEWINDOW )
{
  hb_retl( ( WINBOOL ) MoveWindow( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( WINBOOL ) hb_parl( 6 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowPos (HWND hWnd, HWND hWndInsertAfter, int X, int Y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WINAPI_SETWINDOWPOS )
{
  hb_retl( ( WINBOOL ) SetWindowPos( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( UINT ) hb_parni( 7 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowPlacement (HWND hWnd, WINDOWPLACEMENT *lpwndpl)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowPlacement (HWND hWnd, CONST WINDOWPLACEMENT *lpwndpl)
*/

/*
WINUSERAPI WINBOOL WINAPI GetWindowDisplayAffinity (HWND hWnd, DWORD *pdwAffinity)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowDisplayAffinity (HWND hWnd, DWORD dwAffinity)
*/

/*
WINUSERAPI HDWP WINAPI BeginDeferWindowPos (int nNumWindows)
*/
HB_FUNC( WINAPI_BEGINDEFERWINDOWPOS )
{
  hb_retptr( ( HDWP ) BeginDeferWindowPos( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI HDWP WINAPI DeferWindowPos (HDWP hWinPosInfo, HWND hWnd, HWND hWndInsertAfter, int x, int y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WINAPI_DEFERWINDOWPOS )
{
  hb_retptr( ( HDWP ) DeferWindowPos( ( HDWP ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( HWND ) hb_parptr( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( UINT ) hb_parni( 8 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EndDeferWindowPos (HDWP hWinPosInfo)
*/
HB_FUNC( WINAPI_ENDDEFERWINDOWPOS )
{
  hb_retl( ( WINBOOL ) EndDeferWindowPos( ( HDWP ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowVisible (HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWVISIBLE )
{
  hb_retl( ( WINBOOL ) IsWindowVisible( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsIconic (HWND hWnd)
*/
HB_FUNC( WINAPI_ISICONIC )
{
  hb_retl( ( WINBOOL ) IsIconic( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AnyPopup (VOID)
*/
HB_FUNC( WINAPI_ANYPOPUP )
{
  hb_retl( ( WINBOOL ) AnyPopup() );
}

/*
WINUSERAPI WINBOOL WINAPI BringWindowToTop (HWND hWnd)
*/
HB_FUNC( WINAPI_BRINGWINDOWTOTOP )
{
  hb_retl( ( WINBOOL ) BringWindowToTop( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsZoomed (HWND hWnd)
*/
HB_FUNC( WINAPI_ISZOOMED )
{
  hb_retl( ( WINBOOL ) IsZoomed( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI CreateDialogParamA(HINSTANCE hInstance,LPCSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI HWND WINAPI CreateDialogParamW(HINSTANCE hInstance,LPCWSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI HWND WINAPI CreateDialogIndirectParamA(HINSTANCE hInstance,LPCDLGTEMPLATEA lpTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI HWND WINAPI CreateDialogIndirectParamW(HINSTANCE hInstance,LPCDLGTEMPLATEW lpTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI INT_PTR WINAPI DialogBoxParamA(HINSTANCE hInstance,LPCSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI INT_PTR WINAPI DialogBoxParamW(HINSTANCE hInstance,LPCWSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI INT_PTR WINAPI DialogBoxIndirectParamA(HINSTANCE hInstance,LPCDLGTEMPLATEA hDialogTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI INT_PTR WINAPI DialogBoxIndirectParamW(HINSTANCE hInstance,LPCDLGTEMPLATEW hDialogTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EndDialog(HWND hDlg,INT_PTR nResult)
*/
HB_FUNC( WINAPI_ENDDIALOG )
{
  hb_retl( ( WINBOOL ) EndDialog( ( HWND ) hb_parptr( 1 ), ( INT_PTR ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetDlgItem(HWND hDlg,int nIDDlgItem)
*/
HB_FUNC( WINAPI_GETDLGITEM )
{
  hb_retptr( ( HWND ) GetDlgItem( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemInt(HWND hDlg,int nIDDlgItem,UINT uValue,WINBOOL bSigned)
*/
HB_FUNC( WINAPI_SETDLGITEMINT )
{
  hb_retl( ( WINBOOL ) SetDlgItemInt( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( WINBOOL ) hb_parl( 4 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetDlgItemInt(HWND hDlg,int nIDDlgItem,WINBOOL *lpTranslated,WINBOOL bSigned)
*/

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPCSTR lpString)
*/
HB_FUNC( WINAPI_SETDLGITEMTEXTA )
{
  hb_retl( ( WINBOOL ) SetDlgItemTextA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LPCSTR ) hb_parc( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_SETDLGITEMTEXTW )
{
  hb_retl( ( WINBOOL ) SetDlgItemTextW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LPCWSTR ) hb_parc( 3 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPSTR lpString,int cchMax)
*/
HB_FUNC( WINAPI_GETDLGITEMTEXTA )
{
  hb_retni( ( UINT ) GetDlgItemTextA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LPSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPWSTR lpString,int cchMax)
*/
HB_FUNC( WINAPI_GETDLGITEMTEXTW )
{
  hb_retni( ( UINT ) GetDlgItemTextW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LPWSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CheckDlgButton(HWND hDlg,int nIDButton,UINT uCheck)
*/
HB_FUNC( WINAPI_CHECKDLGBUTTON )
{
  hb_retl( ( WINBOOL ) CheckDlgButton( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CheckRadioButton(HWND hDlg,int nIDFirstButton,int nIDLastButton,int nIDCheckButton)
*/
HB_FUNC( WINAPI_CHECKRADIOBUTTON )
{
  hb_retl( ( WINBOOL ) CheckRadioButton( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI UINT WINAPI IsDlgButtonChecked(HWND hDlg,int nIDButton)
*/
HB_FUNC( WINAPI_ISDLGBUTTONCHECKED )
{
  hb_retni( ( UINT ) IsDlgButtonChecked( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageA(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDDLGITEMMESSAGEA )
{
  hb_retnl( ( LRESULT ) SendDlgItemMessageA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( WPARAM ) hb_parni( 4 ), ( LPARAM ) hb_parnl( 5 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageW(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDDLGITEMMESSAGEW )
{
  hb_retnl( ( LRESULT ) SendDlgItemMessageW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( WPARAM ) hb_parni( 4 ), ( LPARAM ) hb_parnl( 5 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetNextDlgGroupItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WINAPI_GETNEXTDLGGROUPITEM )
{
  hb_retptr( ( HWND ) GetNextDlgGroupItem( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetNextDlgTabItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WINAPI_GETNEXTDLGTABITEM )
{
  hb_retptr( ( HWND ) GetNextDlgTabItem( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetDlgCtrlID(HWND hWnd)
*/
HB_FUNC( WINAPI_GETDLGCTRLID )
{
  hb_retni( ( int ) GetDlgCtrlID( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI __LONG32 WINAPI GetDialogBaseUnits(VOID)
*/

/*
WINUSERAPI LRESULT WINAPI DefDlgProcA(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFDLGPROCA )
{
  hb_retnl( ( LRESULT ) DefDlgProcA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefDlgProcW(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFDLGPROCW )
{
  hb_retnl( ( LRESULT ) DefDlgProcW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterA(LPMSG lpMsg,int nCode)
*/

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterW(LPMSG lpMsg,int nCode)
*/

/*
WINUSERAPI WINBOOL WINAPI OpenClipboard(HWND hWndNewOwner)
*/
HB_FUNC( WINAPI_OPENCLIPBOARD )
{
  hb_retl( ( WINBOOL ) OpenClipboard( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CloseClipboard(VOID)
*/
HB_FUNC( WINAPI_CLOSECLIPBOARD )
{
  hb_retl( ( WINBOOL ) CloseClipboard() );
}

/*
WINUSERAPI DWORD WINAPI GetClipboardSequenceNumber(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDSEQUENCENUMBER )
{
  hb_retnl( ( DWORD ) GetClipboardSequenceNumber() );
}

/*
WINUSERAPI HWND WINAPI GetClipboardOwner(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDOWNER )
{
  hb_retptr( ( HWND ) GetClipboardOwner() );
}

/*
WINUSERAPI HWND WINAPI SetClipboardViewer(HWND hWndNewViewer)
*/
HB_FUNC( WINAPI_SETCLIPBOARDVIEWER )
{
  hb_retptr( ( HWND ) SetClipboardViewer( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetClipboardViewer(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDVIEWER )
{
  hb_retptr( ( HWND ) GetClipboardViewer() );
}

/*
WINUSERAPI WINBOOL WINAPI ChangeClipboardChain(HWND hWndRemove, HWND hWndNewNext)
*/
HB_FUNC( WINAPI_CHANGECLIPBOARDCHAIN )
{
  hb_retl( ( WINBOOL ) ChangeClipboardChain( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI SetClipboardData(UINT uFormat, HANDLE hMem)
*/
HB_FUNC( WINAPI_SETCLIPBOARDDATA )
{
  hb_retptr( ( HANDLE ) SetClipboardData( ( UINT ) hb_parni( 1 ), ( HANDLE ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI GetClipboardData(UINT uFormat)
*/
HB_FUNC( WINAPI_GETCLIPBOARDDATA )
{
  hb_retptr( ( HANDLE ) GetClipboardData( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatA(LPCSTR lpszFormat)
*/
HB_FUNC( WINAPI_REGISTERCLIPBOARDFORMATA )
{
  hb_retni( ( UINT ) RegisterClipboardFormatA( ( LPCSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatW(LPCWSTR lpszFormat)
*/
HB_FUNC( WINAPI_REGISTERCLIPBOARDFORMATW )
{
  hb_retni( ( UINT ) RegisterClipboardFormatW( ( LPCWSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI int WINAPI CountClipboardFormats(VOID)
*/
HB_FUNC( WINAPI_COUNTCLIPBOARDFORMATS )
{
  hb_retni( ( int ) CountClipboardFormats() );
}

/*
WINUSERAPI UINT WINAPI EnumClipboardFormats(UINT format)
*/
HB_FUNC( WINAPI_ENUMCLIPBOARDFORMATS )
{
  hb_retni( ( UINT ) EnumClipboardFormats( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI int WINAPI GetClipboardFormatNameA(UINT format, LPSTR lpszFormatName, int cchMaxCount)
*/
HB_FUNC( WINAPI_GETCLIPBOARDFORMATNAMEA )
{
  hb_retni( ( int ) GetClipboardFormatNameA( ( UINT ) hb_parni( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetClipboardFormatNameW(UINT format, LPWSTR lpszFormatName, int cchMaxCount)
*/
HB_FUNC( WINAPI_GETCLIPBOARDFORMATNAMEW )
{
  hb_retni( ( int ) GetClipboardFormatNameW( ( UINT ) hb_parni( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EmptyClipboard(VOID)
*/
HB_FUNC( WINAPI_EMPTYCLIPBOARD )
{
  hb_retl( ( WINBOOL ) EmptyClipboard() );
}

/*
WINUSERAPI WINBOOL WINAPI IsClipboardFormatAvailable(UINT format)
*/
HB_FUNC( WINAPI_ISCLIPBOARDFORMATAVAILABLE )
{
  hb_retl( ( WINBOOL ) IsClipboardFormatAvailable( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI int WINAPI GetPriorityClipboardFormat(UINT *paFormatPriorityList, int cFormats)
*/

/*
WINUSERAPI HWND WINAPI GetOpenClipboardWindow(VOID)
*/
HB_FUNC( WINAPI_GETOPENCLIPBOARDWINDOW )
{
  hb_retptr( ( HWND ) GetOpenClipboardWindow() );
}

/*
WINUSERAPI WINBOOL WINAPI AddClipboardFormatListener (HWND hwnd)
*/

/*
WINUSERAPI WINBOOL WINAPI RemoveClipboardFormatListener (HWND hwnd)
*/

/*
WINUSERAPI WINBOOL WINAPI GetUpdatedClipboardFormats (PUINT lpuiFormats, UINT cFormats, PUINT pcFormatsOut)
*/

/*
WINUSERAPI WINBOOL WINAPI CharToOemA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_CHARTOOEMA )
{
  hb_retl( ( WINBOOL ) CharToOemA( ( LPCSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemW(LPCWSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_CHARTOOEMW )
{
  hb_retl( ( WINBOOL ) CharToOemW( ( LPCWSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_OEMTOCHARA )
{
  hb_retl( ( WINBOOL ) OemToCharA( ( LPCSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharW(LPCSTR lpszSrc,LPWSTR lpszDst)
*/
HB_FUNC( WINAPI_OEMTOCHARW )
{
  hb_retl( ( WINBOOL ) OemToCharW( ( LPCSTR ) hb_parc( 1 ), ( LPWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_CHARTOOEMBUFFA )
{
  hb_retl( ( WINBOOL ) CharToOemBuffA( ( LPCSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffW(LPCWSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_CHARTOOEMBUFFW )
{
  hb_retl( ( WINBOOL ) CharToOemBuffW( ( LPCWSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_OEMTOCHARBUFFA )
{
  hb_retl( ( WINBOOL ) OemToCharBuffA( ( LPCSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffW(LPCSTR lpszSrc,LPWSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_OEMTOCHARBUFFW )
{
  hb_retl( ( WINBOOL ) OemToCharBuffW( ( LPCSTR ) hb_parc( 1 ), ( LPWSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI LPSTR WINAPI CharUpperA(LPSTR lpsz)
*/
HB_FUNC( WINAPI_CHARUPPERA )
{
  hb_retc( ( LPSTR ) CharUpperA( ( LPSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI LPWSTR WINAPI CharUpperW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharUpperBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARUPPERBUFFA )
{
  hb_retnl( ( DWORD ) CharUpperBuffA( ( LPSTR ) hb_parc( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI CharUpperBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARUPPERBUFFW )
{
  hb_retnl( ( DWORD ) CharUpperBuffW( ( LPWSTR ) hb_parc( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI LPSTR WINAPI CharLowerA(LPSTR lpsz)
*/
HB_FUNC( WINAPI_CHARLOWERA )
{
  hb_retc( ( LPSTR ) CharLowerA( ( LPSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI LPWSTR WINAPI CharLowerW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharLowerBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARLOWERBUFFA )
{
  hb_retnl( ( DWORD ) CharLowerBuffA( ( LPSTR ) hb_parc( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI CharLowerBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARLOWERBUFFW )
{
  hb_retnl( ( DWORD ) CharLowerBuffW( ( LPWSTR ) hb_parc( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI LPSTR WINAPI CharNextA(LPCSTR lpsz)
*/
HB_FUNC( WINAPI_CHARNEXTA )
{
  hb_retc( ( LPSTR ) CharNextA( ( LPCSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI LPWSTR WINAPI CharNextW(LPCWSTR lpsz)
*/

/*
WINUSERAPI LPSTR WINAPI CharPrevA(LPCSTR lpszStart,LPCSTR lpszCurrent)
*/
HB_FUNC( WINAPI_CHARPREVA )
{
  hb_retc( ( LPSTR ) CharPrevA( ( LPCSTR ) hb_parc( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI LPWSTR WINAPI CharPrevW(LPCWSTR lpszStart,LPCWSTR lpszCurrent)
*/

/*
WINUSERAPI LPSTR WINAPI CharNextExA(WORD CodePage,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WINAPI_CHARNEXTEXA )
{
  hb_retc( ( LPSTR ) CharNextExA( ( WORD ) hb_parni( 1 ), ( LPCSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI LPSTR WINAPI CharPrevExA(WORD CodePage,LPCSTR lpStart,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WINAPI_CHARPREVEXA )
{
  hb_retc( ( LPSTR ) CharPrevExA( ( WORD ) hb_parni( 1 ), ( LPCSTR ) hb_parc( 2 ), ( LPCSTR ) hb_parc( 3 ), ( DWORD ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHAA )
{
  hb_retl( ( WINBOOL ) IsCharAlphaA( ( CHAR ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaW(WCHAR ch)
*/

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHANUMERICA )
{
  hb_retl( ( WINBOOL ) IsCharAlphaNumericA( ( CHAR ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericW(WCHAR ch)
*/

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARUPPERA )
{
  hb_retl( ( WINBOOL ) IsCharUpperA( ( CHAR ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperW(WCHAR ch)
*/

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARLOWERA )
{
  hb_retl( ( WINBOOL ) IsCharLowerA( ( CHAR ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerW(WCHAR ch)
*/

/*
WINUSERAPI HWND WINAPI SetFocus(HWND hWnd)
*/
HB_FUNC( WINAPI_SETFOCUS )
{
  hb_retptr( ( HWND ) SetFocus( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetActiveWindow(VOID)
*/
HB_FUNC( WINAPI_GETACTIVEWINDOW )
{
  hb_retptr( ( HWND ) GetActiveWindow() );
}

/*
WINUSERAPI HWND WINAPI GetFocus(VOID)
*/
HB_FUNC( WINAPI_GETFOCUS )
{
  hb_retptr( ( HWND ) GetFocus() );
}

/*
WINUSERAPI UINT WINAPI GetKBCodePage(VOID)
*/
HB_FUNC( WINAPI_GETKBCODEPAGE )
{
  hb_retni( ( UINT ) GetKBCodePage() );
}

/*
WINUSERAPI SHORT WINAPI GetKeyState(int nVirtKey)
*/
HB_FUNC( WINAPI_GETKEYSTATE )
{
  hb_retni( ( SHORT ) GetKeyState( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI SHORT WINAPI GetAsyncKeyState(int vKey)
*/
HB_FUNC( WINAPI_GETASYNCKEYSTATE )
{
  hb_retni( ( SHORT ) GetAsyncKeyState( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardState(PBYTE lpKeyState)
*/

/*
WINUSERAPI WINBOOL WINAPI SetKeyboardState(LPBYTE lpKeyState)
*/

/*
WINUSERAPI int WINAPI GetKeyNameTextA(LONG lParam,LPSTR lpString,int cchSize)
*/
HB_FUNC( WINAPI_GETKEYNAMETEXTA )
{
  hb_retni( ( int ) GetKeyNameTextA( ( LONG ) hb_parnl( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetKeyNameTextW(LONG lParam,LPWSTR lpString,int cchSize)
*/
HB_FUNC( WINAPI_GETKEYNAMETEXTW )
{
  hb_retni( ( int ) GetKeyNameTextW( ( LONG ) hb_parnl( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetKeyboardType(int nTypeFlag)
*/
HB_FUNC( WINAPI_GETKEYBOARDTYPE )
{
  hb_retni( ( int ) GetKeyboardType( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI int WINAPI ToAscii(UINT uVirtKey,UINT uScanCode,CONST BYTE *lpKeyState,LPWORD lpChar,UINT uFlags)
*/

/*
WINUSERAPI int WINAPI ToAsciiEx(UINT uVirtKey,UINT uScanCode,CONST BYTE *lpKeyState,LPWORD lpChar,UINT uFlags,HKL dwhkl)
*/

/*
WINUSERAPI int WINAPI ToUnicode(UINT wVirtKey,UINT wScanCode,CONST BYTE *lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags)
*/

/*
WINUSERAPI DWORD WINAPI OemKeyScan(WORD wOemChar)
*/
HB_FUNC( WINAPI_OEMKEYSCAN )
{
  hb_retnl( ( DWORD ) OemKeyScan( ( WORD ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanA(CHAR ch)
*/
HB_FUNC( WINAPI_VKKEYSCANA )
{
  hb_retni( ( SHORT ) VkKeyScanA( ( CHAR ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanW(WCHAR ch)
*/

/*
WINUSERAPI SHORT WINAPI VkKeyScanExA(CHAR ch,HKL dwhkl)
*/
HB_FUNC( WINAPI_VKKEYSCANEXA )
{
  hb_retni( ( SHORT ) VkKeyScanExA( ( CHAR ) hb_parni( 1 ), ( HKL ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanExW(WCHAR ch,HKL dwhkl)
*/

/*
WINUSERAPI VOID WINAPI keybd_event(BYTE bVk,BYTE bScan,DWORD dwFlags,ULONG_PTR dwExtraInfo)
*/
HB_FUNC( WINAPI_KEYBD_EVENT )
{
  keybd_event( ( BYTE ) hb_parni( 1 ), ( BYTE ) hb_parni( 2 ), ( DWORD ) hb_parnl( 3 ), ( ULONG_PTR ) hb_parnl( 4 ) );
}

/*
WINUSERAPI UINT WINAPI SendInput(UINT cInputs,LPINPUT pInputs,int cbSize)
*/

/*
WINUSERAPI WINBOOL WINAPI GetTouchInputInfo (HTOUCHINPUT hTouchInput, UINT cInputs, PTOUCHINPUT pInputs, int cbSize)
*/

/*
WINUSERAPI WINBOOL WINAPI CloseTouchInputHandle (HTOUCHINPUT hTouchInput)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterTouchWindow (HWND hwnd, ULONG ulFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI UnregisterTouchWindow (HWND hwnd)
*/

/*
WINUSERAPI WINBOOL WINAPI IsTouchWindow (HWND hwnd, PULONG pulFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI InitializeTouchInjection (UINT32 maxCount, DWORD dwMode)
*/

/*
WINUSERAPI WINBOOL WINAPI InjectTouchInput (UINT32 count, CONST POINTER_TOUCH_INFO *contacts)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerType (UINT32 pointerId, POINTER_INPUT_TYPE *pointerType)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerCursorId (UINT32 pointerId, UINT32 *cursorId)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerInfo (UINT32 pointerId, POINTER_INFO *pointerInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerInfoHistory (UINT32 pointerId, UINT32 *entriesCount, POINTER_INFO *pointerInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFrameInfo (UINT32 pointerId, UINT32 *pointerCount, POINTER_INFO *pointerInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFrameInfoHistory (UINT32 pointerId, UINT32 *entriesCount, UINT32 *pointerCount, POINTER_INFO *pointerInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerTouchInfo (UINT32 pointerId, POINTER_TOUCH_INFO *touchInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerTouchInfoHistory (UINT32 pointerId, UINT32 *entriesCount, POINTER_TOUCH_INFO *touchInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFrameTouchInfo (UINT32 pointerId, UINT32 *pointerCount, POINTER_TOUCH_INFO *touchInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFrameTouchInfoHistory (UINT32 pointerId, UINT32 *entriesCount, UINT32 *pointerCount, POINTER_TOUCH_INFO *touchInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerPenInfo (UINT32 pointerId, POINTER_PEN_INFO *penInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerPenInfoHistory (UINT32 pointerId, UINT32 *entriesCount, POINTER_PEN_INFO *penInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFramePenInfo (UINT32 pointerId, UINT32 *pointerCount, POINTER_PEN_INFO *penInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerFramePenInfoHistory (UINT32 pointerId, UINT32 *entriesCount, UINT32 *pointerCount, POINTER_PEN_INFO *penInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI SkipPointerFrameMessages (UINT32 pointerId)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterPointerInputTarget (HWND hwnd, POINTER_INPUT_TYPE pointerType)
*/

/*
WINUSERAPI WINBOOL WINAPI UnregisterPointerInputTarget (HWND hwnd, POINTER_INPUT_TYPE pointerType)
*/

/*
WINUSERAPI WINBOOL WINAPI EnableMouseInPointer (WINBOOL fEnable)
*/

/*
WINUSERAPI WINBOOL WINAPI IsMouseInPointerEnabled (VOID)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterTouchHitTestingWindow (HWND hwnd, ULONG value)
*/

/*
WINUSERAPI WINBOOL WINAPI EvaluateProximityToRect (const RECT *controlBoundingBox, const TOUCH_HIT_TESTING_INPUT *pHitTestingInput, TOUCH_HIT_TESTING_PROXIMITY_EVALUATION *pProximityEval)
*/

/*
WINUSERAPI WINBOOL WINAPI EvaluateProximityToPolygon (UINT32 numVertices, const POINT *controlPolygon, const TOUCH_HIT_TESTING_INPUT *pHitTestingInput, TOUCH_HIT_TESTING_PROXIMITY_EVALUATION *pProximityEval)
*/

/*
WINUSERAPI LRESULT WINAPI PackTouchHitTestingProximityEvaluation (const TOUCH_HIT_TESTING_INPUT *pHitTestingInput, const TOUCH_HIT_TESTING_PROXIMITY_EVALUATION *pProximityEval)
*/

/*
WINUSERAPI WINBOOL WINAPI GetWindowFeedbackSetting (HWND hwnd, FEEDBACK_TYPE feedback, DWORD dwFlags, UINT32 *pSize, VOID *config)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowFeedbackSetting (HWND hwnd, FEEDBACK_TYPE feedback, DWORD dwFlags, UINT32 size, CONST VOID *configuration)
*/

/*
WINUSERAPI WINBOOL WINAPI GetLastInputInfo(PLASTINPUTINFO plii)
*/

/*
WINUSERAPI UINT WINAPI MapVirtualKeyA(UINT uCode,UINT uMapType)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYA )
{
  hb_retni( ( UINT ) MapVirtualKeyA( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyW(UINT uCode,UINT uMapType)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYW )
{
  hb_retni( ( UINT ) MapVirtualKeyW( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExA(UINT uCode,UINT uMapType,HKL dwhkl)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYEXA )
{
  hb_retni( ( UINT ) MapVirtualKeyExA( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ), ( HKL ) hb_parptr( 3 ) ) );
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExW(UINT uCode,UINT uMapType,HKL dwhkl)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYEXW )
{
  hb_retni( ( UINT ) MapVirtualKeyExW( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ), ( HKL ) hb_parptr( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetInputState(VOID)
*/
HB_FUNC( WINAPI_GETINPUTSTATE )
{
  hb_retl( ( WINBOOL ) GetInputState() );
}

/*
WINUSERAPI DWORD WINAPI GetQueueStatus(UINT flags)
*/
HB_FUNC( WINAPI_GETQUEUESTATUS )
{
  hb_retnl( ( DWORD ) GetQueueStatus( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetCapture(VOID)
*/
HB_FUNC( WINAPI_GETCAPTURE )
{
  hb_retptr( ( HWND ) GetCapture() );
}

/*
WINUSERAPI HWND WINAPI SetCapture(HWND hWnd)
*/
HB_FUNC( WINAPI_SETCAPTURE )
{
  hb_retptr( ( HWND ) SetCapture( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ReleaseCapture(VOID)
*/
HB_FUNC( WINAPI_RELEASECAPTURE )
{
  hb_retl( ( WINBOOL ) ReleaseCapture() );
}

/*
WINUSERAPI DWORD WINAPI MsgWaitForMultipleObjects(DWORD nCount,CONST HANDLE *pHandles,WINBOOL fWaitAll,DWORD dwMilliseconds,DWORD dwWakeMask)
*/

/*
WINUSERAPI DWORD WINAPI MsgWaitForMultipleObjectsEx(DWORD nCount,CONST HANDLE *pHandles,DWORD dwMilliseconds,DWORD dwWakeMask,DWORD dwFlags)
*/

/*
WINUSERAPI UINT_PTR WINAPI SetTimer(HWND hWnd,UINT_PTR nIDEvent,UINT uElapse,TIMERPROC lpTimerFunc)
*/

/*
WINUSERAPI WINBOOL WINAPI KillTimer(HWND hWnd,UINT_PTR uIDEvent)
*/
HB_FUNC( WINAPI_KILLTIMER )
{
  hb_retl( ( WINBOOL ) KillTimer( ( HWND ) hb_parptr( 1 ), ( UINT_PTR ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowUnicode(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWUNICODE )
{
  hb_retl( ( WINBOOL ) IsWindowUnicode( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EnableWindow(HWND hWnd,WINBOOL bEnable)
*/
HB_FUNC( WINAPI_ENABLEWINDOW )
{
  hb_retl( ( WINBOOL ) EnableWindow( ( HWND ) hb_parptr( 1 ), ( WINBOOL ) hb_parl( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowEnabled(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWENABLED )
{
  hb_retl( ( WINBOOL ) IsWindowEnabled( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsA(HINSTANCE hInstance,LPCSTR lpTableName)
*/

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsW(HINSTANCE hInstance,LPCWSTR lpTableName)
*/

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableA(LPACCEL paccel,int cAccel)
*/

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableW(LPACCEL paccel,int cAccel)
*/

/*
WINUSERAPI WINBOOL WINAPI DestroyAcceleratorTable(HACCEL hAccel)
*/

/*
WINUSERAPI int WINAPI CopyAcceleratorTableA(HACCEL hAccelSrc,LPACCEL lpAccelDst,int cAccelEntries)
*/

/*
WINUSERAPI int WINAPI CopyAcceleratorTableW(HACCEL hAccelSrc,LPACCEL lpAccelDst,int cAccelEntries)
*/

/*
WINUSERAPI int WINAPI TranslateAcceleratorA(HWND hWnd,HACCEL hAccTable,LPMSG lpMsg)
*/

/*
WINUSERAPI int WINAPI TranslateAcceleratorW(HWND hWnd,HACCEL hAccTable,LPMSG lpMsg)
*/

/*
WINUSERAPI UINT_PTR WINAPI SetCoalescableTimer (HWND hWnd, UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc, ULONG uToleranceDelay)
*/

/*
WINUSERAPI HMENU WINAPI LoadMenuA(HINSTANCE hInstance,LPCSTR lpMenuName)
*/
HB_FUNC( WINAPI_LOADMENUA )
{
  hb_retptr( ( HMENU ) LoadMenuA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HMENU WINAPI LoadMenuW(HINSTANCE hInstance,LPCWSTR lpMenuName)
*/
HB_FUNC( WINAPI_LOADMENUW )
{
  hb_retptr( ( HMENU ) LoadMenuW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HMENU WINAPI LoadMenuIndirectA(CONST MENUTEMPLATEA *lpMenuTemplate)
*/

/*
WINUSERAPI HMENU WINAPI LoadMenuIndirectW(CONST MENUTEMPLATEW *lpMenuTemplate)
*/

/*
WINUSERAPI HMENU WINAPI GetMenu(HWND hWnd)
*/
HB_FUNC( WINAPI_GETMENU )
{
  hb_retptr( ( HMENU ) GetMenu( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetMenu(HWND hWnd,HMENU hMenu)
*/
HB_FUNC( WINAPI_SETMENU )
{
  hb_retl( ( WINBOOL ) SetMenu( ( HWND ) hb_parptr( 1 ), ( HMENU ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuA(HMENU hMenu,UINT cmd,LPCSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
HB_FUNC( WINAPI_CHANGEMENUA )
{
  hb_retl( ( WINBOOL ) ChangeMenuA( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPCSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuW(HMENU hMenu,UINT cmd,LPCWSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
HB_FUNC( WINAPI_CHANGEMENUW )
{
  hb_retl( ( WINBOOL ) ChangeMenuW( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPCWSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI HiliteMenuItem(HWND hWnd,HMENU hMenu,UINT uIDHiliteItem,UINT uHilite)
*/
HB_FUNC( WINAPI_HILITEMENUITEM )
{
  hb_retl( ( WINBOOL ) HiliteMenuItem( ( HWND ) hb_parptr( 1 ), ( HMENU ) hb_parptr( 2 ), ( UINT ) hb_parni( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI GetMenuStringA(HMENU hMenu,UINT uIDItem,LPSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WINAPI_GETMENUSTRINGA )
{
  hb_retni( ( int ) GetMenuStringA( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI int WINAPI GetMenuStringW(HMENU hMenu,UINT uIDItem,LPWSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WINAPI_GETMENUSTRINGW )
{
  hb_retni( ( int ) GetMenuStringW( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPWSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetMenuState(HMENU hMenu,UINT uId,UINT uFlags)
*/
HB_FUNC( WINAPI_GETMENUSTATE )
{
  hb_retni( ( UINT ) GetMenuState( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DrawMenuBar(HWND hWnd)
*/
HB_FUNC( WINAPI_DRAWMENUBAR )
{
  hb_retl( ( WINBOOL ) DrawMenuBar( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HMENU WINAPI GetSystemMenu(HWND hWnd,WINBOOL bRevert)
*/
HB_FUNC( WINAPI_GETSYSTEMMENU )
{
  hb_retptr( ( HMENU ) GetSystemMenu( ( HWND ) hb_parptr( 1 ), ( WINBOOL ) hb_parl( 2 ) ) );
}

/*
WINUSERAPI HMENU WINAPI CreateMenu(VOID)
*/
HB_FUNC( WINAPI_CREATEMENU )
{
  hb_retptr( ( HMENU ) CreateMenu() );
}

/*
WINUSERAPI HMENU WINAPI CreatePopupMenu(VOID)
*/
HB_FUNC( WINAPI_CREATEPOPUPMENU )
{
  hb_retptr( ( HMENU ) CreatePopupMenu() );
}

/*
WINUSERAPI WINBOOL WINAPI DestroyMenu(HMENU hMenu)
*/
HB_FUNC( WINAPI_DESTROYMENU )
{
  hb_retl( ( WINBOOL ) DestroyMenu( ( HMENU ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI DWORD WINAPI CheckMenuItem(HMENU hMenu,UINT uIDCheckItem,UINT uCheck)
*/
HB_FUNC( WINAPI_CHECKMENUITEM )
{
  hb_retnl( ( DWORD ) CheckMenuItem( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EnableMenuItem(HMENU hMenu,UINT uIDEnableItem,UINT uEnable)
*/
HB_FUNC( WINAPI_ENABLEMENUITEM )
{
  hb_retl( ( WINBOOL ) EnableMenuItem( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI HMENU WINAPI GetSubMenu(HMENU hMenu,int nPos)
*/
HB_FUNC( WINAPI_GETSUBMENU )
{
  hb_retptr( ( HMENU ) GetSubMenu( ( HMENU ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetMenuItemID(HMENU hMenu,int nPos)
*/
HB_FUNC( WINAPI_GETMENUITEMID )
{
  hb_retni( ( UINT ) GetMenuItemID( ( HMENU ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI int WINAPI GetMenuItemCount(HMENU hMenu)
*/
HB_FUNC( WINAPI_GETMENUITEMCOUNT )
{
  hb_retni( ( int ) GetMenuItemCount( ( HMENU ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI InsertMenuA(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_INSERTMENUA )
{
  hb_retl( ( WINBOOL ) InsertMenuA( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT_PTR ) hb_parni( 4 ), ( LPCSTR ) hb_parc( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI InsertMenuW(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_INSERTMENUW )
{
  hb_retl( ( WINBOOL ) InsertMenuW( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT_PTR ) hb_parni( 4 ), ( LPCWSTR ) hb_parc( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AppendMenuA(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_APPENDMENUA )
{
  hb_retl( ( WINBOOL ) AppendMenuA( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parni( 3 ), ( LPCSTR ) hb_parc( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AppendMenuW(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_APPENDMENUW )
{
  hb_retl( ( WINBOOL ) AppendMenuW( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parni( 3 ), ( LPCWSTR ) hb_parc( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuA(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_MODIFYMENUA )
{
  hb_retl( ( WINBOOL ) ModifyMenuA( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT_PTR ) hb_parni( 4 ), ( LPCSTR ) hb_parc( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuW(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_MODIFYMENUW )
{
  hb_retl( ( WINBOOL ) ModifyMenuW( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT_PTR ) hb_parni( 4 ), ( LPCWSTR ) hb_parc( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI RemoveMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WINAPI_REMOVEMENU )
{
  hb_retl( ( WINBOOL ) RemoveMenu( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DeleteMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WINAPI_DELETEMENU )
{
  hb_retl( ( WINBOOL ) DeleteMenu( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuItemBitmaps(HMENU hMenu,UINT uPosition,UINT uFlags,HBITMAP hBitmapUnchecked,HBITMAP hBitmapChecked)
*/
HB_FUNC( WINAPI_SETMENUITEMBITMAPS )
{
  hb_retl( ( WINBOOL ) SetMenuItemBitmaps( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( HBITMAP ) hb_parptr( 4 ), ( HBITMAP ) hb_parptr( 5 ) ) );
}

/*
WINUSERAPI LONG WINAPI GetMenuCheckMarkDimensions(VOID)
*/
HB_FUNC( WINAPI_GETMENUCHECKMARKDIMENSIONS )
{
  hb_retnl( ( LONG ) GetMenuCheckMarkDimensions() );
}

/*
WINUSERAPI WINBOOL WINAPI TrackPopupMenu(HMENU hMenu,UINT uFlags,int x,int y,int nReserved,HWND hWnd,CONST RECT *prcRect)
*/

/*
WINUSERAPI WINBOOL WINAPI TrackPopupMenuEx(HMENU,UINT,int,int,HWND,LPTPMPARAMS)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMenuInfo(HMENU,LPMENUINFO)
*/

/*
WINUSERAPI WINBOOL WINAPI SetMenuInfo(HMENU,LPCMENUINFO)
*/

/*
WINUSERAPI WINBOOL WINAPI EndMenu(VOID)
*/
HB_FUNC( WINAPI_ENDMENU )
{
  hb_retl( ( WINBOOL ) EndMenu() );
}

/*
WINUSERAPI WINBOOL WINAPI CalculatePopupWindowPosition (const POINT *anchorPoint, const SIZE *windowSize, UINT flags, RECT *excludeRect, RECT *popupWindowPosition)
*/

/*
WINUSERAPI WINBOOL WINAPI InsertMenuItemA(HMENU hmenu,UINT item,WINBOOL fByPosition,LPCMENUITEMINFOA lpmi)
*/

/*
WINUSERAPI WINBOOL WINAPI InsertMenuItemW(HMENU hmenu,UINT item,WINBOOL fByPosition,LPCMENUITEMINFOW lpmi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMenuItemInfoA(HMENU hmenu,UINT item,WINBOOL fByPosition,LPMENUITEMINFOA lpmii)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMenuItemInfoW(HMENU hmenu,UINT item,WINBOOL fByPosition,LPMENUITEMINFOW lpmii)
*/

/*
WINUSERAPI WINBOOL WINAPI SetMenuItemInfoA(HMENU hmenu,UINT item,WINBOOL fByPositon,LPCMENUITEMINFOA lpmii)
*/

/*
WINUSERAPI WINBOOL WINAPI SetMenuItemInfoW(HMENU hmenu,UINT item,WINBOOL fByPositon,LPCMENUITEMINFOW lpmii)
*/

/*
WINUSERAPI UINT WINAPI GetMenuDefaultItem(HMENU hMenu,UINT fByPos,UINT gmdiFlags)
*/
HB_FUNC( WINAPI_GETMENUDEFAULTITEM )
{
  hb_retni( ( UINT ) GetMenuDefaultItem( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuDefaultItem(HMENU hMenu,UINT uItem,UINT fByPos)
*/
HB_FUNC( WINAPI_SETMENUDEFAULTITEM )
{
  hb_retl( ( WINBOOL ) SetMenuDefaultItem( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetMenuItemRect(HWND hWnd,HMENU hMenu,UINT uItem,LPRECT lprcItem)
*/

/*
WINUSERAPI int WINAPI MenuItemFromPoint(HWND hWnd,HMENU hMenu,POINT ptScreen)
*/

/*
WINUSERAPI DWORD WINAPI DragObject(HWND hwndParent,HWND hwndFrom,UINT fmt,ULONG_PTR data,HCURSOR hcur)
*/
HB_FUNC( WINAPI_DRAGOBJECT )
{
  hb_retnl( ( DWORD ) DragObject( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( UINT ) hb_parni( 3 ), ( ULONG_PTR ) hb_parnl( 4 ), ( HCURSOR ) hb_parptr( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DragDetect(HWND hwnd,POINT pt)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawIcon(HDC hDC,int X,int Y,HICON hIcon)
*/
HB_FUNC( WINAPI_DRAWICON )
{
  hb_retl( ( WINBOOL ) DrawIcon( ( HDC ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( HICON ) hb_parptr( 4 ) ) );
}

/*
WINUSERAPI int WINAPI DrawTextA(HDC hdc,LPCSTR lpchText,int cchText,LPRECT lprc,UINT format)
*/

/*
WINUSERAPI int WINAPI DrawTextW(HDC hdc,LPCWSTR lpchText,int cchText,LPRECT lprc,UINT format)
*/

/*
WINUSERAPI int WINAPI DrawTextExA(HDC hdc,LPSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp)
*/

/*
WINUSERAPI int WINAPI DrawTextExW(HDC hdc,LPWSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp)
*/

/*
WINUSERAPI WINBOOL WINAPI GrayStringA(HDC hDC,HBRUSH hBrush,GRAYSTRINGPROC lpOutputFunc,LPARAM lpData,int nCount,int X,int Y,int nWidth,int nHeight)
*/

/*
WINUSERAPI WINBOOL WINAPI GrayStringW(HDC hDC,HBRUSH hBrush,GRAYSTRINGPROC lpOutputFunc,LPARAM lpData,int nCount,int X,int Y,int nWidth,int nHeight)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawStateA(HDC hdc,HBRUSH hbrFore,DRAWSTATEPROC qfnCallBack,LPARAM lData,WPARAM wData,int x,int y,int cx,int cy,UINT uFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawStateW(HDC hdc,HBRUSH hbrFore,DRAWSTATEPROC qfnCallBack,LPARAM lData,WPARAM wData,int x,int y,int cx,int cy,UINT uFlags)
*/

/*
WINUSERAPI LONG WINAPI TabbedTextOutA(HDC hdc,int x,int y,LPCSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions,int nTabOrigin)
*/

/*
WINUSERAPI LONG WINAPI TabbedTextOutW(HDC hdc,int x,int y,LPCWSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions,int nTabOrigin)
*/

/*
WINUSERAPI DWORD WINAPI GetTabbedTextExtentA(HDC hdc,LPCSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions)
*/

/*
WINUSERAPI DWORD WINAPI GetTabbedTextExtentW(HDC hdc,LPCWSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions)
*/

/*
WINUSERAPI WINBOOL WINAPI UpdateWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_UPDATEWINDOW )
{
  hb_retl( ( WINBOOL ) UpdateWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI SetActiveWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_SETACTIVEWINDOW )
{
  hb_retptr( ( HWND ) SetActiveWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetForegroundWindow(VOID)
*/
HB_FUNC( WINAPI_GETFOREGROUNDWINDOW )
{
  hb_retptr( ( HWND ) GetForegroundWindow() );
}

/*
WINUSERAPI WINBOOL WINAPI PaintDesktop(HDC hdc)
*/
HB_FUNC( WINAPI_PAINTDESKTOP )
{
  hb_retl( ( WINBOOL ) PaintDesktop( ( HDC ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI VOID WINAPI SwitchToThisWindow(HWND hwnd,WINBOOL fUnknown)
*/
HB_FUNC( WINAPI_SWITCHTOTHISWINDOW )
{
  SwitchToThisWindow( ( HWND ) hb_parptr( 1 ), ( WINBOOL ) hb_parl( 2 ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetForegroundWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_SETFOREGROUNDWINDOW )
{
  hb_retl( ( WINBOOL ) SetForegroundWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI AllowSetForegroundWindow(DWORD dwProcessId)
*/
HB_FUNC( WINAPI_ALLOWSETFOREGROUNDWINDOW )
{
  hb_retl( ( WINBOOL ) AllowSetForegroundWindow( ( DWORD ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI LockSetForegroundWindow(UINT uLockCode)
*/
HB_FUNC( WINAPI_LOCKSETFOREGROUNDWINDOW )
{
  hb_retl( ( WINBOOL ) LockSetForegroundWindow( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI WindowFromDC(HDC hDC)
*/
HB_FUNC( WINAPI_WINDOWFROMDC )
{
  hb_retptr( ( HWND ) WindowFromDC( ( HDC ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HDC WINAPI GetDC(HWND hWnd)
*/
HB_FUNC( WINAPI_GETDC )
{
  hb_retptr( ( HDC ) GetDC( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HDC WINAPI GetDCEx(HWND hWnd,HRGN hrgnClip,DWORD flags)
*/
HB_FUNC( WINAPI_GETDCEX )
{
  hb_retptr( ( HDC ) GetDCEx( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI HDC WINAPI GetWindowDC(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWDC )
{
  hb_retptr( ( HDC ) GetWindowDC( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI int WINAPI ReleaseDC(HWND hWnd,HDC hDC)
*/
HB_FUNC( WINAPI_RELEASEDC )
{
  hb_retni( ( int ) ReleaseDC( ( HWND ) hb_parptr( 1 ), ( HDC ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI HDC WINAPI BeginPaint(HWND hWnd,LPPAINTSTRUCT lpPaint)
*/

/*
WINUSERAPI WINBOOL WINAPI EndPaint(HWND hWnd,CONST PAINTSTRUCT *lpPaint)
*/

/*
WINUSERAPI WINBOOL WINAPI GetUpdateRect(HWND hWnd,LPRECT lpRect,WINBOOL bErase)
*/

/*
WINUSERAPI int WINAPI GetUpdateRgn(HWND hWnd,HRGN hRgn,WINBOOL bErase)
*/
HB_FUNC( WINAPI_GETUPDATERGN )
{
  hb_retni( ( int ) GetUpdateRgn( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI int WINAPI SetWindowRgn(HWND hWnd,HRGN hRgn,WINBOOL bRedraw)
*/
HB_FUNC( WINAPI_SETWINDOWRGN )
{
  hb_retni( ( int ) SetWindowRgn( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WINAPI_GETWINDOWRGN )
{
  hb_retni( ( int ) GetWindowRgn( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowRgnBox(HWND hWnd,LPRECT lprc)
*/

/*
WINUSERAPI int WINAPI ExcludeUpdateRgn(HDC hDC,HWND hWnd)
*/
HB_FUNC( WINAPI_EXCLUDEUPDATERGN )
{
  hb_retni( ( int ) ExcludeUpdateRgn( ( HDC ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI InvalidateRect(HWND hWnd,CONST RECT *lpRect,WINBOOL bErase)
*/

/*
WINUSERAPI WINBOOL WINAPI ValidateRect(HWND hWnd,CONST RECT *lpRect)
*/

/*
WINUSERAPI WINBOOL WINAPI InvalidateRgn(HWND hWnd,HRGN hRgn,WINBOOL bErase)
*/
HB_FUNC( WINAPI_INVALIDATERGN )
{
  hb_retl( ( WINBOOL ) InvalidateRgn( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ValidateRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WINAPI_VALIDATERGN )
{
  hb_retl( ( WINBOOL ) ValidateRgn( ( HWND ) hb_parptr( 1 ), ( HRGN ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI RedrawWindow(HWND hWnd,CONST RECT *lprcUpdate,HRGN hrgnUpdate,UINT flags)
*/

/*
WINUSERAPI WINBOOL WINAPI LockWindowUpdate(HWND hWndLock)
*/
HB_FUNC( WINAPI_LOCKWINDOWUPDATE )
{
  hb_retl( ( WINBOOL ) LockWindowUpdate( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ScrollWindow(HWND hWnd,int XAmount,int YAmount,CONST RECT *lpRect,CONST RECT *lpClipRect)
*/

/*
WINUSERAPI WINBOOL WINAPI ScrollDC(HDC hDC,int dx,int dy,CONST RECT *lprcScroll,CONST RECT *lprcClip,HRGN hrgnUpdate,LPRECT lprcUpdate)
*/

/*
WINUSERAPI int WINAPI ScrollWindowEx(HWND hWnd,int dx,int dy,CONST RECT *prcScroll,CONST RECT *prcClip,HRGN hrgnUpdate,LPRECT prcUpdate,UINT flags)
*/

/*
WINUSERAPI int WINAPI SetScrollPos(HWND hWnd,int nBar,int nPos,WINBOOL bRedraw)
*/
HB_FUNC( WINAPI_SETSCROLLPOS )
{
  hb_retni( ( int ) SetScrollPos( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( WINBOOL ) hb_parl( 4 ) ) );
}

/*
WINUSERAPI int WINAPI GetScrollPos(HWND hWnd,int nBar)
*/
HB_FUNC( WINAPI_GETSCROLLPOS )
{
  hb_retni( ( int ) GetScrollPos( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetScrollRange(HWND hWnd,int nBar,int nMinPos,int nMaxPos,WINBOOL bRedraw)
*/
HB_FUNC( WINAPI_SETSCROLLRANGE )
{
  hb_retl( ( WINBOOL ) SetScrollRange( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( WINBOOL ) hb_parl( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollRange(HWND hWnd,int nBar,LPINT lpMinPos,LPINT lpMaxPos)
*/

/*
WINUSERAPI WINBOOL WINAPI ShowScrollBar(HWND hWnd,int wBar,WINBOOL bShow)
*/
HB_FUNC( WINAPI_SHOWSCROLLBAR )
{
  hb_retl( ( WINBOOL ) ShowScrollBar( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EnableScrollBar(HWND hWnd,UINT wSBflags,UINT wArrows)
*/
HB_FUNC( WINAPI_ENABLESCROLLBAR )
{
  hb_retl( ( WINBOOL ) EnableScrollBar( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetPropA(HWND hWnd,LPCSTR lpString,HANDLE hData)
*/
HB_FUNC( WINAPI_SETPROPA )
{
  hb_retl( ( WINBOOL ) SetPropA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( HANDLE ) hb_parptr( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetPropW(HWND hWnd,LPCWSTR lpString,HANDLE hData)
*/
HB_FUNC( WINAPI_SETPROPW )
{
  hb_retl( ( WINBOOL ) SetPropW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( HANDLE ) hb_parptr( 3 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI GetPropA(HWND hWnd,LPCSTR lpString)
*/
HB_FUNC( WINAPI_GETPROPA )
{
  hb_retptr( ( HANDLE ) GetPropA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI GetPropW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_GETPROPW )
{
  hb_retptr( ( HANDLE ) GetPropW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI RemovePropA(HWND hWnd,LPCSTR lpString)
*/
HB_FUNC( WINAPI_REMOVEPROPA )
{
  hb_retptr( ( HANDLE ) RemovePropA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI RemovePropW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_REMOVEPROPW )
{
  hb_retptr( ( HANDLE ) RemovePropW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI int WINAPI EnumPropsExA(HWND hWnd,PROPENUMPROCEXA lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI int WINAPI EnumPropsExW(HWND hWnd,PROPENUMPROCEXW lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI int WINAPI EnumPropsA(HWND hWnd,PROPENUMPROCA lpEnumFunc)
*/

/*
WINUSERAPI int WINAPI EnumPropsW(HWND hWnd,PROPENUMPROCW lpEnumFunc)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowTextA(HWND hWnd,LPCSTR lpString)
*/
HB_FUNC( WINAPI_SETWINDOWTEXTA )
{
  hb_retl( ( WINBOOL ) SetWindowTextA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowTextW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_SETWINDOWTEXTW )
{
  hb_retl( ( WINBOOL ) SetWindowTextW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowTextA(HWND hWnd,LPSTR lpString,int nMaxCount)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTA )
{
  hb_retni( ( int ) GetWindowTextA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowTextW(HWND hWnd,LPWSTR lpString,int nMaxCount)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTW )
{
  hb_retni( ( int ) GetWindowTextW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowTextLengthA(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTLENGTHA )
{
  hb_retni( ( int ) GetWindowTextLengthA( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI int WINAPI GetWindowTextLengthW(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTLENGTHW )
{
  hb_retni( ( int ) GetWindowTextLengthW( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetClientRect(HWND hWnd,LPRECT lpRect)
*/

/*
WINUSERAPI WINBOOL WINAPI GetWindowRect(HWND hWnd,LPRECT lpRect)
*/

/*
WINUSERAPI WINBOOL WINAPI AdjustWindowRect(LPRECT lpRect,DWORD dwStyle,WINBOOL bMenu)
*/

/*
WINUSERAPI WINBOOL WINAPI AdjustWindowRectEx(LPRECT lpRect,DWORD dwStyle,WINBOOL bMenu,DWORD dwExStyle)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowContextHelpId(HWND,DWORD)
*/
HB_FUNC( WINAPI_SETWINDOWCONTEXTHELPID )
{
  hb_retl( ( WINBOOL ) SetWindowContextHelpId( ( HWND ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetWindowContextHelpId(HWND)
*/
HB_FUNC( WINAPI_GETWINDOWCONTEXTHELPID )
{
  hb_retnl( ( DWORD ) GetWindowContextHelpId( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuContextHelpId(HMENU,DWORD)
*/
HB_FUNC( WINAPI_SETMENUCONTEXTHELPID )
{
  hb_retl( ( WINBOOL ) SetMenuContextHelpId( ( HMENU ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetMenuContextHelpId(HMENU)
*/
HB_FUNC( WINAPI_GETMENUCONTEXTHELPID )
{
  hb_retnl( ( DWORD ) GetMenuContextHelpId( ( HMENU ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI int WINAPI MessageBoxA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType)
*/
HB_FUNC( WINAPI_MESSAGEBOXA )
{
  hb_retni( ( int ) MessageBoxA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( LPCSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI MessageBoxW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType)
*/
HB_FUNC( WINAPI_MESSAGEBOXW )
{
  hb_retni( ( int ) MessageBoxW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( LPCWSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI MessageBoxExA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType,WORD wLanguageId)
*/
HB_FUNC( WINAPI_MESSAGEBOXEXA )
{
  hb_retni( ( int ) MessageBoxExA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( LPCSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ), ( WORD ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI int WINAPI MessageBoxExW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType,WORD wLanguageId)
*/
HB_FUNC( WINAPI_MESSAGEBOXEXW )
{
  hb_retni( ( int ) MessageBoxExW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( LPCWSTR ) hb_parc( 3 ), ( UINT ) hb_parni( 4 ), ( WORD ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI int WINAPI MessageBoxIndirectA(CONST MSGBOXPARAMSA *lpmbp)
*/

/*
WINUSERAPI int WINAPI MessageBoxIndirectW(CONST MSGBOXPARAMSW *lpmbp)
*/

/*
WINUSERAPI WINBOOL WINAPI MessageBeep(UINT uType)
*/
HB_FUNC( WINAPI_MESSAGEBEEP )
{
  hb_retl( ( WINBOOL ) MessageBeep( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI int WINAPI ShowCursor(WINBOOL bShow)
*/
HB_FUNC( WINAPI_SHOWCURSOR )
{
  hb_retni( ( int ) ShowCursor( ( WINBOOL ) hb_parl( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetCursorPos(int X,int Y)
*/
HB_FUNC( WINAPI_SETCURSORPOS )
{
  hb_retl( ( WINBOOL ) SetCursorPos( ( int ) hb_parni( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI SetCursor(HCURSOR hCursor)
*/
HB_FUNC( WINAPI_SETCURSOR )
{
  hb_retptr( ( HCURSOR ) SetCursor( ( HCURSOR ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetCursorPos(LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI ClipCursor(CONST RECT *lpRect)
*/

/*
WINUSERAPI WINBOOL WINAPI GetClipCursor(LPRECT lpRect)
*/

/*
WINUSERAPI HCURSOR WINAPI GetCursor(VOID)
*/
HB_FUNC( WINAPI_GETCURSOR )
{
  hb_retptr( ( HCURSOR ) GetCursor() );
}

/*
WINUSERAPI WINBOOL WINAPI CreateCaret(HWND hWnd,HBITMAP hBitmap,int nWidth,int nHeight)
*/
HB_FUNC( WINAPI_CREATECARET )
{
  hb_retl( ( WINBOOL ) CreateCaret( ( HWND ) hb_parptr( 1 ), ( HBITMAP ) hb_parptr( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetCaretBlinkTime(VOID)
*/
HB_FUNC( WINAPI_GETCARETBLINKTIME )
{
  hb_retni( ( UINT ) GetCaretBlinkTime() );
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretBlinkTime(UINT uMSeconds)
*/
HB_FUNC( WINAPI_SETCARETBLINKTIME )
{
  hb_retl( ( WINBOOL ) SetCaretBlinkTime( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DestroyCaret(VOID)
*/
HB_FUNC( WINAPI_DESTROYCARET )
{
  hb_retl( ( WINBOOL ) DestroyCaret() );
}

/*
WINUSERAPI WINBOOL WINAPI HideCaret(HWND hWnd)
*/
HB_FUNC( WINAPI_HIDECARET )
{
  hb_retl( ( WINBOOL ) HideCaret( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ShowCaret(HWND hWnd)
*/
HB_FUNC( WINAPI_SHOWCARET )
{
  hb_retl( ( WINBOOL ) ShowCaret( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretPos(int X,int Y)
*/
HB_FUNC( WINAPI_SETCARETPOS )
{
  hb_retl( ( WINBOOL ) SetCaretPos( ( int ) hb_parni( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetCaretPos(LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI ClientToScreen(HWND hWnd,LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI ScreenToClient(HWND hWnd,LPPOINT lpPoint)
*/

/*
WINUSERAPI int WINAPI MapWindowPoints(HWND hWndFrom,HWND hWndTo,LPPOINT lpPoints,UINT cPoints)
*/

/*
WINUSERAPI HWND WINAPI WindowFromPoint(POINT Point)
*/

/*
WINUSERAPI HWND WINAPI ChildWindowFromPoint(HWND hWndParent,POINT Point)
*/

/*
WINUSERAPI HWND WINAPI ChildWindowFromPointEx(HWND hwnd,POINT pt,UINT flags)
*/

/*
WINUSERAPI WINBOOL WINAPI SetPhysicalCursorPos (int X, int Y)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPhysicalCursorPos (LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI LogicalToPhysicalPoint (HWND hWnd, LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI PhysicalToLogicalPoint (HWND hWnd, LPPOINT lpPoint)
*/

/*
WINUSERAPI HWND WINAPI WindowFromPhysicalPoint (POINT Point)
*/

/*
WINUSERAPI WINBOOL WINAPI LogicalToPhysicalPointForPerMonitorDPI (HWND hwnd, LPPOINT lpPoint)
*/

/*
WINUSERAPI WINBOOL WINAPI PhysicalToLogicalPointForPerMonitorDPI (HWND hwnd, LPPOINT lpPoint)
*/

/*
WINUSERAPI DWORD WINAPI GetSysColor(int nIndex)
*/
HB_FUNC( WINAPI_GETSYSCOLOR )
{
  hb_retnl( ( DWORD ) GetSysColor( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI HBRUSH WINAPI GetSysColorBrush(int nIndex)
*/
HB_FUNC( WINAPI_GETSYSCOLORBRUSH )
{
  hb_retptr( ( HBRUSH ) GetSysColorBrush( ( int ) hb_parni( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetSysColors(int cElements,CONST INT *lpaElements,CONST COLORREF *lpaRgbValues)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawFocusRect(HDC hDC,CONST RECT *lprc)
*/

/*
WINUSERAPI int WINAPI FillRect(HDC hDC,CONST RECT *lprc,HBRUSH hbr)
*/

/*
WINUSERAPI int WINAPI FrameRect(HDC hDC,CONST RECT *lprc,HBRUSH hbr)
*/

/*
WINUSERAPI WINBOOL WINAPI InvertRect(HDC hDC,CONST RECT *lprc)
*/

/*
WINUSERAPI WINBOOL WINAPI SetRect(LPRECT lprc,int xLeft,int yTop,int xRight,int yBottom)
*/

/*
WINUSERAPI WINBOOL WINAPI SetRectEmpty(LPRECT lprc)
*/

/*
WINUSERAPI WINBOOL WINAPI CopyRect(LPRECT lprcDst,CONST RECT *lprcSrc)
*/

/*
WINUSERAPI WINBOOL WINAPI InflateRect(LPRECT lprc,int dx,int dy)
*/

/*
WINUSERAPI WINBOOL WINAPI IntersectRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/

/*
WINUSERAPI WINBOOL WINAPI UnionRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/

/*
WINUSERAPI WINBOOL WINAPI SubtractRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/

/*
WINUSERAPI WINBOOL WINAPI OffsetRect(LPRECT lprc,int dx,int dy)
*/

/*
WINUSERAPI WINBOOL WINAPI IsRectEmpty(CONST RECT *lprc)
*/

/*
WINUSERAPI WINBOOL WINAPI EqualRect(CONST RECT *lprc1,CONST RECT *lprc2)
*/

/*
WINUSERAPI WINBOOL WINAPI PtInRect(CONST RECT *lprc,POINT pt)
*/

/*
WINUSERAPI WORD WINAPI GetWindowWord(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETWINDOWWORD )
{
  hb_retni( ( WORD ) GetWindowWord( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WORD WINAPI SetWindowWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WINAPI_SETWINDOWWORD )
{
  hb_retni( ( WORD ) SetWindowWord( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( WORD ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI LONG WINAPI GetWindowLongA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETWINDOWLONGA )
{
  hb_retnl( ( LONG ) GetWindowLongA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI LONG WINAPI GetWindowLongW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETWINDOWLONGW )
{
  hb_retnl( ( LONG ) GetWindowLongW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI LONG WINAPI SetWindowLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETWINDOWLONGA )
{
  hb_retnl( ( LONG ) SetWindowLongA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LONG ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI LONG WINAPI SetWindowLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETWINDOWLONGW )
{
  hb_retnl( ( LONG ) SetWindowLongW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LONG ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI LONG_PTR WINAPI GetWindowLongPtrA(HWND hWnd,int nIndex)
*/

/*
WINUSERAPI LONG_PTR WINAPI GetWindowLongPtrW(HWND hWnd,int nIndex)
*/

/*
WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrA(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/

/*
WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrW(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/

/*
WINUSERAPI WORD WINAPI GetClassWord(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSWORD )
{
  hb_retni( ( WORD ) GetClassWord( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI WORD WINAPI SetClassWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WINAPI_SETCLASSWORD )
{
  hb_retni( ( WORD ) SetClassWord( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( WORD ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetClassLongA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGA )
{
  hb_retnl( ( DWORD ) GetClassLongA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetClassLongW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGW )
{
  hb_retnl( ( DWORD ) GetClassLongW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI DWORD WINAPI SetClassLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETCLASSLONGA )
{
  hb_retnl( ( DWORD ) SetClassLongA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LONG ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI DWORD WINAPI SetClassLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETCLASSLONGW )
{
  hb_retnl( ( DWORD ) SetClassLongW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( LONG ) hb_parnl( 3 ) ) );
}

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGPTRA )
{
  hb_retnl( ( ULONG_PTR ) GetClassLongPtrA( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGPTRW )
{
  hb_retnl( ( ULONG_PTR ) GetClassLongPtrW( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrA(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/

/*
WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrW(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/

/*
WINUSERAPI WINBOOL WINAPI GetProcessDefaultLayout(DWORD *pdwDefaultLayout)
*/

/*
WINUSERAPI WINBOOL WINAPI SetProcessDefaultLayout(DWORD dwDefaultLayout)
*/
HB_FUNC( WINAPI_SETPROCESSDEFAULTLAYOUT )
{
  hb_retl( ( WINBOOL ) SetProcessDefaultLayout( ( DWORD ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetDesktopWindow(VOID)
*/
HB_FUNC( WINAPI_GETDESKTOPWINDOW )
{
  hb_retptr( ( HWND ) GetDesktopWindow() );
}

/*
WINUSERAPI HWND WINAPI GetParent(HWND hWnd)
*/
HB_FUNC( WINAPI_GETPARENT )
{
  hb_retptr( ( HWND ) GetParent( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI SetParent(HWND hWndChild,HWND hWndNewParent)
*/
HB_FUNC( WINAPI_SETPARENT )
{
  hb_retptr( ( HWND ) SetParent( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EnumChildWindows(HWND hWndParent,WNDENUMPROC lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI HWND WINAPI FindWindowA(LPCSTR lpClassName,LPCSTR lpWindowName)
*/
HB_FUNC( WINAPI_FINDWINDOWA )
{
  hb_retptr( ( HWND ) FindWindowA( ( LPCSTR ) hb_parc( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HWND WINAPI FindWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName)
*/
HB_FUNC( WINAPI_FINDWINDOWW )
{
  hb_retptr( ( HWND ) FindWindowW( ( LPCWSTR ) hb_parc( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HWND WINAPI FindWindowExA(HWND hWndParent,HWND hWndChildAfter,LPCSTR lpszClass,LPCSTR lpszWindow)
*/
HB_FUNC( WINAPI_FINDWINDOWEXA )
{
  hb_retptr( ( HWND ) FindWindowExA( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( LPCSTR ) hb_parc( 3 ), ( LPCSTR ) hb_parc( 4 ) ) );
}

/*
WINUSERAPI HWND WINAPI FindWindowExW(HWND hWndParent,HWND hWndChildAfter,LPCWSTR lpszClass,LPCWSTR lpszWindow)
*/
HB_FUNC( WINAPI_FINDWINDOWEXW )
{
  hb_retptr( ( HWND ) FindWindowExW( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( LPCWSTR ) hb_parc( 3 ), ( LPCWSTR ) hb_parc( 4 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetShellWindow(VOID)
*/
HB_FUNC( WINAPI_GETSHELLWINDOW )
{
  hb_retptr( ( HWND ) GetShellWindow() );
}

/*
WINUSERAPI WINBOOL WINAPI RegisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_REGISTERSHELLHOOKWINDOW )
{
  hb_retl( ( WINBOOL ) RegisterShellHookWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DeregisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_DEREGISTERSHELLHOOKWINDOW )
{
  hb_retl( ( WINBOOL ) DeregisterShellHookWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI EnumWindows(WNDENUMPROC lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumThreadWindows(DWORD dwThreadId,WNDENUMPROC lpfn,LPARAM lParam)
*/

/*
WINUSERAPI int WINAPI GetClassNameA(HWND hWnd,LPSTR lpClassName,int nMaxCount)
*/
HB_FUNC( WINAPI_GETCLASSNAMEA )
{
  hb_retni( ( int ) GetClassNameA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI int WINAPI GetClassNameW(HWND hWnd,LPWSTR lpClassName,int nMaxCount)
*/
HB_FUNC( WINAPI_GETCLASSNAMEW )
{
  hb_retni( ( int ) GetClassNameW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetTopWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_GETTOPWINDOW )
{
  hb_retptr( ( HWND ) GetTopWindow( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetWindowThreadProcessId(HWND hWnd,LPDWORD lpdwProcessId)
*/
HB_FUNC( WINAPI_GETWINDOWTHREADPROCESSID )
{
  hb_retnl( ( DWORD ) GetWindowThreadProcessId( ( HWND ) hb_parptr( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsGUIThread(WINBOOL bConvert)
*/
HB_FUNC( WINAPI_ISGUITHREAD )
{
  hb_retl( ( WINBOOL ) IsGUIThread( ( WINBOOL ) hb_parl( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetLastActivePopup(HWND hWnd)
*/
HB_FUNC( WINAPI_GETLASTACTIVEPOPUP )
{
  hb_retptr( ( HWND ) GetLastActivePopup( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI GetWindow(HWND hWnd,UINT uCmd)
*/
HB_FUNC( WINAPI_GETWINDOW )
{
  hb_retptr( ( HWND ) GetWindow( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HHOOK WINAPI SetWindowsHookA (int nFilterType, HOOKPROC pfnFilterProc)
*/

/*
WINUSERAPI HHOOK WINAPI SetWindowsHookW (int nFilterType, HOOKPROC pfnFilterProc)
*/

/*
WINUSERAPI HOOKPROC WINAPI SetWindowsHookA (int nFilterType, HOOKPROC pfnFilterProc)
*/

/*
WINUSERAPI HOOKPROC WINAPI SetWindowsHookW (int nFilterType, HOOKPROC pfnFilterProc)
*/

/*
WINUSERAPI WINBOOL WINAPI UnhookWindowsHook (int nCode, HOOKPROC pfnFilterProc)
*/

/*
WINUSERAPI HHOOK WINAPI SetWindowsHookExA (int idHook, HOOKPROC lpfn, HINSTANCE hmod, DWORD dwThreadId)
*/

/*
WINUSERAPI HHOOK WINAPI SetWindowsHookExW (int idHook, HOOKPROC lpfn, HINSTANCE hmod, DWORD dwThreadId)
*/

/*
WINUSERAPI WINBOOL WINAPI UnhookWindowsHookEx (HHOOK hhk)
*/

/*
WINUSERAPI LRESULT WINAPI CallNextHookEx (HHOOK hhk, int nCode, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI CheckMenuRadioItem(HMENU hmenu,UINT first,UINT last,UINT check,UINT flags)
*/
HB_FUNC( WINAPI_CHECKMENURADIOITEM )
{
  hb_retl( ( WINBOOL ) CheckMenuRadioItem( ( HMENU ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( UINT ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapA(HINSTANCE hInstance,LPCSTR lpBitmapName)
*/
HB_FUNC( WINAPI_LOADBITMAPA )
{
  hb_retptr( ( HBITMAP ) LoadBitmapA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapW(HINSTANCE hInstance,LPCWSTR lpBitmapName)
*/
HB_FUNC( WINAPI_LOADBITMAPW )
{
  hb_retptr( ( HBITMAP ) LoadBitmapW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorA(HINSTANCE hInstance,LPCSTR lpCursorName)
*/
HB_FUNC( WINAPI_LOADCURSORA )
{
  hb_retptr( ( HCURSOR ) LoadCursorA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorW(HINSTANCE hInstance,LPCWSTR lpCursorName)
*/
HB_FUNC( WINAPI_LOADCURSORW )
{
  hb_retptr( ( HCURSOR ) LoadCursorW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileA(LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_LOADCURSORFROMFILEA )
{
  hb_retptr( ( HCURSOR ) LoadCursorFromFileA( ( LPCSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileW(LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_LOADCURSORFROMFILEW )
{
  hb_retptr( ( HCURSOR ) LoadCursorFromFileW( ( LPCWSTR ) hb_parc( 1 ) ) );
}

/*
WINUSERAPI HCURSOR WINAPI CreateCursor(HINSTANCE hInst,int xHotSpot,int yHotSpot,int nWidth,int nHeight,CONST VOID *pvANDPlane,CONST VOID *pvXORPlane)
*/

/*
WINUSERAPI WINBOOL WINAPI DestroyCursor(HCURSOR hCursor)
*/
HB_FUNC( WINAPI_DESTROYCURSOR )
{
  hb_retl( ( WINBOOL ) DestroyCursor( ( HCURSOR ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetSystemCursor(HCURSOR hcur,DWORD id)
*/
HB_FUNC( WINAPI_SETSYSTEMCURSOR )
{
  hb_retl( ( WINBOOL ) SetSystemCursor( ( HCURSOR ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI HICON WINAPI LoadIconA(HINSTANCE hInstance,LPCSTR lpIconName)
*/
HB_FUNC( WINAPI_LOADICONA )
{
  hb_retptr( ( HICON ) LoadIconA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI HICON WINAPI LoadIconW(HINSTANCE hInstance,LPCWSTR lpIconName)
*/
HB_FUNC( WINAPI_LOADICONW )
{
  hb_retptr( ( HICON ) LoadIconW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI UINT WINAPI PrivateExtractIconsA(LPCSTR szFileName,int nIconIndex,int cxIcon,int cyIcon,HICON *phicon,UINT *piconid,UINT nIcons,UINT flags)
*/

/*
WINUSERAPI UINT WINAPI PrivateExtractIconsW(LPCWSTR szFileName,int nIconIndex,int cxIcon,int cyIcon,HICON *phicon,UINT *piconid,UINT nIcons,UINT flags)
*/

/*
WINUSERAPI HICON WINAPI CreateIcon(HINSTANCE hInstance,int nWidth,int nHeight,BYTE cPlanes,BYTE cBitsPixel,CONST BYTE *lpbANDbits,CONST BYTE *lpbXORbits)
*/

/*
WINUSERAPI WINBOOL WINAPI DestroyIcon(HICON hIcon)
*/
HB_FUNC( WINAPI_DESTROYICON )
{
  hb_retl( ( WINBOOL ) DestroyIcon( ( HICON ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI int WINAPI LookupIconIdFromDirectory(PBYTE presbits,WINBOOL fIcon)
*/

/*
WINUSERAPI int WINAPI LookupIconIdFromDirectoryEx(PBYTE presbits,WINBOOL fIcon,int cxDesired,int cyDesired,UINT Flags)
*/

/*
WINUSERAPI HICON WINAPI CreateIconFromResource(PBYTE presbits,DWORD dwResSize,WINBOOL fIcon,DWORD dwVer)
*/

/*
WINUSERAPI HICON WINAPI CreateIconFromResourceEx(PBYTE presbits,DWORD dwResSize,WINBOOL fIcon,DWORD dwVer,int cxDesired,int cyDesired,UINT Flags)
*/

/*
WINUSERAPI HANDLE WINAPI LoadImageA(HINSTANCE hInst,LPCSTR name,UINT type,int cx,int cy,UINT fuLoad)
*/
HB_FUNC( WINAPI_LOADIMAGEA )
{
  hb_retptr( ( HANDLE ) LoadImageA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( UINT ) hb_parni( 6 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI LoadImageW(HINSTANCE hInst,LPCWSTR name,UINT type,int cx,int cy,UINT fuLoad)
*/
HB_FUNC( WINAPI_LOADIMAGEW )
{
  hb_retptr( ( HANDLE ) LoadImageW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( UINT ) hb_parni( 6 ) ) );
}

/*
WINUSERAPI HANDLE WINAPI CopyImage(HANDLE h,UINT type,int cx,int cy,UINT flags)
*/
HB_FUNC( WINAPI_COPYIMAGE )
{
  hb_retptr( ( HANDLE ) CopyImage( ( HANDLE ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DrawIconEx(HDC hdc,int xLeft,int yTop,HICON hIcon,int cxWidth,int cyWidth,UINT istepIfAniCur,HBRUSH hbrFlickerFreeDraw,UINT diFlags)
*/
HB_FUNC( WINAPI_DRAWICONEX )
{
  hb_retl( ( WINBOOL ) DrawIconEx( ( HDC ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( HICON ) hb_parptr( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( UINT ) hb_parni( 7 ), ( HBRUSH ) hb_parptr( 8 ), ( UINT ) hb_parni( 9 ) ) );
}

/*
WINUSERAPI HICON WINAPI CreateIconIndirect(PICONINFO piconinfo)
*/

/*
WINUSERAPI HICON WINAPI CopyIcon(HICON hIcon)
*/
HB_FUNC( WINAPI_COPYICON )
{
  hb_retptr( ( HICON ) CopyIcon( ( HICON ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetIconInfo(HICON hIcon,PICONINFO piconinfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetIconInfoExA (HICON hicon, PICONINFOEXA piconinfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetIconInfoExW (HICON hicon, PICONINFOEXW piconinfo)
*/

/*
WINUSERAPI int WINAPI LoadStringA (HINSTANCE hInstance, UINT uID, LPSTR lpBuffer, int cchBufferMax)
*/
HB_FUNC( WINAPI_LOADSTRINGA )
{
  hb_retni( ( int ) LoadStringA( ( HINSTANCE ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI LoadStringW (HINSTANCE hInstance, UINT uID, LPWSTR lpBuffer, int cchBufferMax)
*/
HB_FUNC( WINAPI_LOADSTRINGW )
{
  hb_retni( ( int ) LoadStringW( ( HINSTANCE ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPWSTR ) hb_parc( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageA(HWND hDlg,LPMSG lpMsg)
*/

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageW(HWND hDlg,LPMSG lpMsg)
*/

/*
WINUSERAPI WINBOOL WINAPI MapDialogRect(HWND hDlg,LPRECT lpRect)
*/

/*
WINUSERAPI int WINAPI DlgDirListA(HWND hDlg,LPSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WINAPI_DLGDIRLISTA )
{
  hb_retni( ( int ) DlgDirListA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI int WINAPI DlgDirListW(HWND hDlg,LPWSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WINAPI_DLGDIRLISTW )
{
  hb_retni( ( int ) DlgDirListW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExA(HWND hwndDlg,LPSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTEXA )
{
  hb_retl( ( WINBOOL ) DlgDirSelectExA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExW(HWND hwndDlg,LPWSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTEXW )
{
  hb_retl( ( WINBOOL ) DlgDirSelectExW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxA(HWND hDlg,LPSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WINAPI_DLGDIRLISTCOMBOBOXA )
{
  hb_retni( ( int ) DlgDirListComboBoxA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxW(HWND hDlg,LPWSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WINAPI_DLGDIRLISTCOMBOBOXW )
{
  hb_retni( ( int ) DlgDirListComboBoxW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExA(HWND hwndDlg,LPSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTCOMBOBOXEXA )
{
  hb_retl( ( WINBOOL ) DlgDirSelectComboBoxExA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExW(HWND hwndDlg,LPWSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTCOMBOBOXEXW )
{
  hb_retl( ( WINBOOL ) DlgDirSelectComboBoxExW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI int WINAPI SetScrollInfo(HWND hwnd,int nBar,LPCSCROLLINFO lpsi,WINBOOL redraw)
*/

/*
WINUSERAPI WINBOOL WINAPI GetScrollInfo(HWND hwnd,int nBar,LPSCROLLINFO lpsi)
*/

/*
WINUSERAPI LRESULT WINAPI DefFrameProcA(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFFRAMEPROCA )
{
  hb_retnl( ( LRESULT ) DefFrameProcA( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( UINT ) hb_parni( 3 ), ( WPARAM ) hb_parni( 4 ), ( LPARAM ) hb_parnl( 5 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefFrameProcW(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFFRAMEPROCW )
{
  hb_retnl( ( LRESULT ) DefFrameProcW( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( UINT ) hb_parni( 3 ), ( WPARAM ) hb_parni( 4 ), ( LPARAM ) hb_parnl( 5 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcA(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFMDICHILDPROCA )
{
  hb_retnl( ( LRESULT ) DefMDIChildProcA( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcW(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFMDICHILDPROCW )
{
  hb_retnl( ( LRESULT ) DefMDIChildProcW( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( WPARAM ) hb_parni( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI TranslateMDISysAccel(HWND hWndClient,LPMSG lpMsg)
*/

/*
WINUSERAPI UINT WINAPI ArrangeIconicWindows(HWND hWnd)
*/
HB_FUNC( WINAPI_ARRANGEICONICWINDOWS )
{
  hb_retni( ( UINT ) ArrangeIconicWindows( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI HWND WINAPI CreateMDIWindowA(LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
HB_FUNC( WINAPI_CREATEMDIWINDOWA )
{
  hb_retptr( ( HWND ) CreateMDIWindowA( ( LPCSTR ) hb_parc( 1 ), ( LPCSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( HWND ) hb_parptr( 8 ), ( HINSTANCE ) hb_parptr( 9 ), ( LPARAM ) hb_parnl( 10 ) ) );
}

/*
WINUSERAPI HWND WINAPI CreateMDIWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
HB_FUNC( WINAPI_CREATEMDIWINDOWW )
{
  hb_retptr( ( HWND ) CreateMDIWindowW( ( LPCWSTR ) hb_parc( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( DWORD ) hb_parnl( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( HWND ) hb_parptr( 8 ), ( HINSTANCE ) hb_parptr( 9 ), ( LPARAM ) hb_parnl( 10 ) ) );
}

/*
WINUSERAPI WORD WINAPI TileWindows(HWND hwndParent,UINT wHow,CONST RECT *lpRect,UINT cKids,const HWND *lpKids)
*/

/*
WINUSERAPI WORD WINAPI CascadeWindows(HWND hwndParent,UINT wHow,CONST RECT *lpRect,UINT cKids,const HWND *lpKids)
*/

/*
WINUSERAPI WINBOOL WINAPI WinHelpA(HWND hWndMain,LPCSTR lpszHelp,UINT uCommand,ULONG_PTR dwData)
*/
HB_FUNC( WINAPI_WINHELPA )
{
  hb_retl( ( WINBOOL ) WinHelpA( ( HWND ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( ULONG_PTR ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI WinHelpW(HWND hWndMain,LPCWSTR lpszHelp,UINT uCommand,ULONG_PTR dwData)
*/
HB_FUNC( WINAPI_WINHELPW )
{
  hb_retl( ( WINBOOL ) WinHelpW( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( ULONG_PTR ) hb_parnl( 4 ) ) );
}

/*
WINUSERAPI DWORD WINAPI GetGuiResources(HANDLE hProcess,DWORD uiFlags)
*/
HB_FUNC( WINAPI_GETGUIRESOURCES )
{
  hb_retnl( ( DWORD ) GetGuiResources( ( HANDLE ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINUSERAPI LONG WINAPI ChangeDisplaySettingsA(LPDEVMODEA lpDevMode,DWORD dwFlags)
*/

/*
WINUSERAPI LONG WINAPI ChangeDisplaySettingsW(LPDEVMODEW lpDevMode,DWORD dwFlags)
*/

/*
WINUSERAPI LONG WINAPI ChangeDisplaySettingsExA(LPCSTR lpszDeviceName,LPDEVMODEA lpDevMode,HWND hwnd,DWORD dwflags,LPVOID lParam)
*/

/*
WINUSERAPI LONG WINAPI ChangeDisplaySettingsExW(LPCWSTR lpszDeviceName,LPDEVMODEW lpDevMode,HWND hwnd,DWORD dwflags,LPVOID lParam)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplaySettingsA(LPCSTR lpszDeviceName,DWORD iModeNum,LPDEVMODEA lpDevMode)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplaySettingsW(LPCWSTR lpszDeviceName,DWORD iModeNum,LPDEVMODEW lpDevMode)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplaySettingsExA(LPCSTR lpszDeviceName,DWORD iModeNum,LPDEVMODEA lpDevMode,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplaySettingsExW(LPCWSTR lpszDeviceName,DWORD iModeNum,LPDEVMODEW lpDevMode,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplayDevicesA(LPCSTR lpDevice,DWORD iDevNum,PDISPLAY_DEVICEA lpDisplayDevice,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplayDevicesW(LPCWSTR lpDevice,DWORD iDevNum,PDISPLAY_DEVICEW lpDisplayDevice,DWORD dwFlags)
*/

/*
WINUSERAPI LONG WINAPI GetDisplayConfigBufferSizes (UINT32 flags, UINT32 *numPathArrayElements, UINT32 *numModeInfoArrayElements)
*/

/*
WINUSERAPI LONG WINAPI SetDisplayConfig (UINT32 numPathArrayElements, DISPLAYCONFIG_PATH_INFO *pathArray, UINT32 numModeInfoArrayElements, DISPLAYCONFIG_MODE_INFO *modeInfoArray, UINT32 flags)
*/

/*
WINUSERAPI LONG WINAPI QueryDisplayConfig (UINT32 flags, UINT32 *numPathArrayElements, DISPLAYCONFIG_PATH_INFO *pathArray, UINT32 *numModeInfoArrayElements, DISPLAYCONFIG_MODE_INFO *modeInfoArray, DISPLAYCONFIG_TOPOLOGY_ID *currentTopologyId)
*/

/*
WINUSERAPI LONG WINAPI DisplayConfigGetDeviceInfo (DISPLAYCONFIG_DEVICE_INFO_HEADER *requestPacket)
*/

/*
WINUSERAPI LONG WINAPI DisplayConfigSetDeviceInfo (DISPLAYCONFIG_DEVICE_INFO_HEADER *setPacket)
*/

/*
WINUSERAPI WINBOOL WINAPI SystemParametersInfoA(UINT uiAction,UINT uiParam,PVOID pvParam,UINT fWinIni)
*/
HB_FUNC( WINAPI_SYSTEMPARAMETERSINFOA )
{
  hb_retl( ( WINBOOL ) SystemParametersInfoA( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SystemParametersInfoW(UINT uiAction,UINT uiParam,PVOID pvParam,UINT fWinIni)
*/
HB_FUNC( WINAPI_SYSTEMPARAMETERSINFOW )
{
  hb_retl( ( WINBOOL ) SystemParametersInfoW( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ), ( PVOID ) hb_parptr( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINUSERAPI VOID WINAPI SetDebugErrorLevel (DWORD dwLevel)
*/
HB_FUNC( WINAPI_SETDEBUGERRORLEVEL )
{
  SetDebugErrorLevel( ( DWORD ) hb_parnl( 1 ) );
}

/*
WINUSERAPI VOID WINAPI SetLastErrorEx (DWORD dwErrCode, DWORD dwType)
*/
HB_FUNC( WINAPI_SETLASTERROREX )
{
  SetLastErrorEx( ( DWORD ) hb_parnl( 1 ), ( DWORD ) hb_parnl( 2 ) );
}

/*
WINUSERAPI int WINAPI InternalGetWindowText (HWND hWnd, LPWSTR pString, int cchMaxCount)
*/
HB_FUNC( WINAPI_INTERNALGETWINDOWTEXT )
{
  hb_retni( ( int ) InternalGetWindowText( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI CancelShutdown (VOID)
*/
HB_FUNC( WINAPI_CANCELSHUTDOWN )
{
  hb_retl( ( WINBOOL ) CancelShutdown() );
}

/*
WINUSERAPI HMONITOR WINAPI MonitorFromPoint(POINT pt,DWORD dwFlags)
*/

/*
WINUSERAPI HMONITOR WINAPI MonitorFromRect(LPCRECT lprc,DWORD dwFlags)
*/

/*
WINUSERAPI HMONITOR WINAPI MonitorFromWindow(HWND hwnd,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI EndTask (HWND hWnd, WINBOOL fShutDown, WINBOOL fForce)
*/

/*
WINUSERAPI WINBOOL WINAPI SoundSentry (VOID)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMonitorInfoA(HMONITOR hMonitor,LPMONITORINFO lpmi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMonitorInfoW(HMONITOR hMonitor,LPMONITORINFO lpmi)
*/

/*
WINUSERAPI WINBOOL WINAPI EnumDisplayMonitors(HDC hdc,LPCRECT lprcClip,MONITORENUMPROC lpfnEnum,LPARAM dwData)
*/

/*
WINUSERAPI VOID WINAPI NotifyWinEvent(DWORD event,HWND hwnd,LONG idObject,LONG idChild)
*/
HB_FUNC( WINAPI_NOTIFYWINEVENT )
{
  NotifyWinEvent( ( DWORD ) hb_parnl( 1 ), ( HWND ) hb_parptr( 2 ), ( LONG ) hb_parnl( 3 ), ( LONG ) hb_parnl( 4 ) );
}

/*
WINUSERAPI HWINEVENTHOOK WINAPI SetWinEventHook(DWORD eventMin,DWORD eventMax,HMODULE hmodWinEventProc,WINEVENTPROC pfnWinEventProc,DWORD idProcess,DWORD idThread,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI IsWinEventHookInstalled(DWORD event)
*/
HB_FUNC( WINAPI_ISWINEVENTHOOKINSTALLED )
{
  hb_retl( ( WINBOOL ) IsWinEventHookInstalled( ( DWORD ) hb_parnl( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI UnhookWinEvent(HWINEVENTHOOK hWinEventHook)
*/

/*
WINUSERAPI WINBOOL WINAPI GetGUIThreadInfo(DWORD idThread,PGUITHREADINFO pgui)
*/

/*
WINUSERAPI WINBOOL WINAPI BlockInput (WINBOOL fBlockIt)
*/
HB_FUNC( WINAPI_BLOCKINPUT )
{
  hb_retl( ( WINBOOL ) BlockInput( ( WINBOOL ) hb_parl( 1 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameA(HWND hwnd,LPSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WINAPI_GETWINDOWMODULEFILENAMEA )
{
  hb_retni( ( UINT ) GetWindowModuleFileNameA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameW(HWND hwnd,LPWSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WINAPI_GETWINDOWMODULEFILENAMEW )
{
  hb_retni( ( UINT ) GetWindowModuleFileNameW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessDPIAware (VOID)
*/

/*
WINUSERAPI WINBOOL WINAPI IsProcessDPIAware (VOID)
*/

/*
WINUSERAPI WINBOOL WINAPI GetCursorInfo(PCURSORINFO pci)
*/

/*
WINUSERAPI WINBOOL WINAPI GetWindowInfo(HWND hwnd,PWINDOWINFO pwi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetTitleBarInfo(HWND hwnd,PTITLEBARINFO pti)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMenuBarInfo(HWND hwnd,LONG idObject,LONG idItem,PMENUBARINFO pmbi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetScrollBarInfo(HWND hwnd,LONG idObject,PSCROLLBARINFO psbi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetComboBoxInfo(HWND hwndCombo,PCOMBOBOXINFO pcbi)
*/

/*
WINUSERAPI HWND WINAPI GetAncestor(HWND hwnd,UINT gaFlags)
*/
HB_FUNC( WINAPI_GETANCESTOR )
{
  hb_retptr( ( HWND ) GetAncestor( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINUSERAPI HWND WINAPI RealChildWindowFromPoint(HWND hwndParent,POINT ptParentClientCoords)
*/

/*
WINUSERAPI UINT WINAPI RealGetWindowClassA(HWND hwnd,LPSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WINAPI_REALGETWINDOWCLASSA )
{
  hb_retni( ( UINT ) RealGetWindowClassA( ( HWND ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI UINT WINAPI RealGetWindowClassW(HWND hwnd,LPWSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WINAPI_REALGETWINDOWCLASSW )
{
  hb_retni( ( UINT ) RealGetWindowClassW( ( HWND ) hb_parptr( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetAltTabInfoA(HWND hwnd,int iItem,PALTTABINFO pati,LPSTR pszItemText,UINT cchItemText)
*/

/*
WINUSERAPI WINBOOL WINAPI GetAltTabInfoW(HWND hwnd,int iItem,PALTTABINFO pati,LPWSTR pszItemText,UINT cchItemText)
*/

/*
WINUSERAPI DWORD WINAPI GetListBoxInfo(HWND hwnd)
*/
HB_FUNC( WINAPI_GETLISTBOXINFO )
{
  hb_retnl( ( DWORD ) GetListBoxInfo( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI LockWorkStation(VOID)
*/
HB_FUNC( WINAPI_LOCKWORKSTATION )
{
  hb_retl( ( WINBOOL ) LockWorkStation() );
}

/*
WINUSERAPI WINBOOL WINAPI UserHandleGrantAccess(HANDLE hUserHandle,HANDLE hJob,WINBOOL bGrant)
*/
HB_FUNC( WINAPI_USERHANDLEGRANTACCESS )
{
  hb_retl( ( WINBOOL ) UserHandleGrantAccess( ( HANDLE ) hb_parptr( 1 ), ( HANDLE ) hb_parptr( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINUSERAPI UINT WINAPI GetRawInputData(HRAWINPUT hRawInput,UINT uiCommand,LPVOID pData,PUINT pcbSize,UINT cbSizeHeader)
*/

/*
WINUSERAPI UINT WINAPI GetRawInputDeviceInfoA(HANDLE hDevice,UINT uiCommand,LPVOID pData,PUINT pcbSize)
*/

/*
WINUSERAPI UINT WINAPI GetRawInputDeviceInfoW(HANDLE hDevice,UINT uiCommand,LPVOID pData,PUINT pcbSize)
*/

/*
WINUSERAPI UINT WINAPI GetRawInputBuffer(PRAWINPUT pData,PUINT pcbSize,UINT cbSizeHeader)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterRawInputDevices (PCRAWINPUTDEVICE pRawInputDevices, UINT uiNumDevices, UINT cbSize)
*/

/*
WINUSERAPI UINT WINAPI GetRegisteredRawInputDevices (PRAWINPUTDEVICE pRawInputDevices, PUINT puiNumDevices, UINT cbSize)
*/

/*
WINUSERAPI UINT WINAPI GetRawInputDeviceList (PRAWINPUTDEVICELIST pRawInputDeviceList, PUINT puiNumDevices, UINT cbSize)
*/

/*
WINUSERAPI LRESULT WINAPI DefRawInputProc (PRAWINPUT *paRawInput, INT nInput, UINT cbSizeHeader)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerDevices (UINT32 *deviceCount, POINTER_DEVICE_INFO *pointerDevices)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerDevice (HANDLE device, POINTER_DEVICE_INFO *pointerDevice)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerDeviceProperties (HANDLE device, UINT32 *propertyCount, POINTER_DEVICE_PROPERTY *pointerProperties)
*/

/*
WINUSERAPI WINBOOL WINAPI RegisterPointerDeviceNotifications (HWND window, WINBOOL notifyRange)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerDeviceRects (HANDLE device, RECT *pointerDeviceRect, RECT *displayRect)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerDeviceCursors (HANDLE device, UINT32 *cursorCount, POINTER_DEVICE_CURSOR_INFO *deviceCursors)
*/

/*
WINUSERAPI WINBOOL WINAPI GetRawPointerDeviceData (UINT32 pointerId, UINT32 historyCount, UINT32 propertiesCount, POINTER_DEVICE_PROPERTY *pProperties, LONG *pValues)
*/

/*
WINUSERAPI WINBOOL WINAPI ChangeWindowMessageFilter (UINT message, DWORD dwFlag)
*/

/*
WINUSERAPI WINBOOL WINAPI ChangeWindowMessageFilterEx (HWND hwnd, UINT message, DWORD action, PCHANGEFILTERSTRUCT pChangeFilterStruct)
*/

/*
WINUSERAPI WINBOOL WINAPI GetGestureInfo (HGESTUREINFO hGestureInfo, PGESTUREINFO pGestureInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI GetGestureExtraArgs (HGESTUREINFO hGestureInfo, UINT cbExtraArgs, PBYTE pExtraArgs)
*/

/*
WINUSERAPI WINBOOL WINAPI CloseGestureInfoHandle (HGESTUREINFO hGestureInfo)
*/

/*
WINUSERAPI WINBOOL WINAPI SetGestureConfig (HWND hwnd, DWORD dwReserved, UINT cIDs, PGESTURECONFIG pGestureConfig, UINT cbSize)
*/

/*
WINUSERAPI WINBOOL WINAPI GetGestureConfig (HWND hwnd, DWORD dwReserved, DWORD dwFlags, PUINT pcIDs, PGESTURECONFIG pGestureConfig, UINT cbSize)
*/

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonCreate (HWND hWnd, LPCWSTR pwszReason)
*/
HB_FUNC( WINAPI_SHUTDOWNBLOCKREASONCREATE )
{
  hb_retl( ( WINBOOL ) ShutdownBlockReasonCreate( ( HWND ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonQuery (HWND hWnd, LPWSTR pwszBuff, DWORD *pcchBuff)
*/

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonDestroy (HWND hWnd)
*/
HB_FUNC( WINAPI_SHUTDOWNBLOCKREASONDESTROY )
{
  hb_retl( ( WINBOOL ) ShutdownBlockReasonDestroy( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINUSERAPI WINBOOL WINAPI GetCurrentInputMessageSource (INPUT_MESSAGE_SOURCE *inputMessageSource)
*/

/*
WINUSERAPI WINBOOL WINAPI GetCIMSSM (INPUT_MESSAGE_SOURCE *inputMessageSource)
*/

/*
WINUSERAPI WINBOOL WINAPI GetAutoRotationState (PAR_STATE pState)
*/

/*
WINUSERAPI WINBOOL WINAPI GetDisplayAutoRotationPreferences (ORIENTATION_PREFERENCE *pOrientation)
*/

/*
WINUSERAPI WINBOOL WINAPI GetDisplayAutoRotationPreferencesByProcessId (DWORD dwProcessId, ORIENTATION_PREFERENCE *pOrientation, WINBOOL *fRotateScreen)
*/

/*
WINUSERAPI WINBOOL WINAPI SetDisplayAutoRotationPreferences (ORIENTATION_PREFERENCE orientation)
*/

/*
WINUSERAPI WINBOOL WINAPI IsImmersiveProcess (HANDLE hProcess)
*/

/*
WINUSERAPI WINBOOL WINAPI SetProcessRestrictionExemption (WINBOOL fEnableExemption)
*/

/*
WINUSERAPI WINBOOL WINAPI GetPointerInputTransform(UINT32 pointerId, UINT32 historyCount, UINT32 *inputTransform)
*/

/*
WINUSERAPI WINBOOL WINAPI IsMousePointerEnabled(void)
*/
