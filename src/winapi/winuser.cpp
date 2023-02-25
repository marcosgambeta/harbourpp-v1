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
#include "hbapi.hpp"
#include "hbapiitm.h"
#include "hbapicls.hpp"
#include "hbwinuni.h"
#include "winapi.h"

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
  winapi_ret_HKL(LoadKeyboardLayoutA(( LPCSTR ) hb_parc(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI HKL WINAPI LoadKeyboardLayoutW(LPCWSTR pwszKLID,UINT Flags)
*/
HB_FUNC( WINAPI_LOADKEYBOARDLAYOUTW )
{
  winapi_ret_HKL(LoadKeyboardLayoutW(( LPCWSTR ) hb_parc(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI HKL WINAPI ActivateKeyboardLayout(HKL hkl,UINT Flags)
*/
HB_FUNC( WINAPI_ACTIVATEKEYBOARDLAYOUT )
{
  winapi_ret_HKL(ActivateKeyboardLayout(winapi_par_HKL(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI int WINAPI ToUnicodeEx(UINT wVirtKey,UINT wScanCode,CONST BYTE *lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags,HKL dwhkl)
*/

/*
WINUSERAPI WINBOOL WINAPI UnloadKeyboardLayout(HKL hkl)
*/
HB_FUNC( WINAPI_UNLOADKEYBOARDLAYOUT )
{
  winapi_ret_BOOL(UnloadKeyboardLayout(winapi_par_HKL(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameA(LPSTR pwszKLID)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUTNAMEA )
{
  winapi_ret_BOOL(GetKeyboardLayoutNameA(( LPSTR ) hb_parc(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameW(LPWSTR pwszKLID)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUTNAMEW )
{
  winapi_ret_BOOL(GetKeyboardLayoutNameW(( LPWSTR ) hb_parc(1)));
}

/*
WINUSERAPI int WINAPI GetKeyboardLayoutList(int nBuff,HKL *lpList)
*/

/*
WINUSERAPI HKL WINAPI GetKeyboardLayout(DWORD idThread)
*/
HB_FUNC( WINAPI_GETKEYBOARDLAYOUT )
{
  winapi_ret_HKL(GetKeyboardLayout(winapi_par_DWORD(1)));
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
  winapi_ret_BOOL(SwitchDesktop(winapi_par_HDESK(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetThreadDesktop(HDESK hDesktop)
*/
HB_FUNC( WINAPI_SETTHREADDESKTOP )
{
  winapi_ret_BOOL(SetThreadDesktop(winapi_par_HDESK(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseDesktop(HDESK hDesktop)
*/
HB_FUNC( WINAPI_CLOSEDESKTOP )
{
  winapi_ret_BOOL(CloseDesktop(winapi_par_HDESK(1)));
}

/*
WINUSERAPI HDESK WINAPI GetThreadDesktop(DWORD dwThreadId)
*/
HB_FUNC( WINAPI_GETTHREADDESKTOP )
{
  winapi_ret_HDESK(GetThreadDesktop(winapi_par_DWORD(1)));
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
  winapi_ret_BOOL(CloseWindowStation(winapi_par_HWINSTA(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessWindowStation(HWINSTA hWinSta)
*/
HB_FUNC( WINAPI_SETPROCESSWINDOWSTATION )
{
  winapi_ret_BOOL(SetProcessWindowStation(winapi_par_HWINSTA(1)));
}

/*
WINUSERAPI HWINSTA WINAPI GetProcessWindowStation(VOID)
*/
HB_FUNC( WINAPI_GETPROCESSWINDOWSTATION )
{
  winapi_ret_HWINSTA(GetProcessWindowStation());
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
  DWORD nLengthNeeded;
  winapi_ret_BOOL(GetUserObjectInformationA(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_DWORD(4), &nLengthNeeded));
  winapi_stor_DWORD(nLengthNeeded, 5);
}

/*
WINUSERAPI WINBOOL WINAPI GetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength,LPDWORD lpnLengthNeeded)
*/
HB_FUNC( WINAPI_GETUSEROBJECTINFORMATIONW )
{
  DWORD nLengthNeeded;
  winapi_ret_BOOL(GetUserObjectInformationW(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_DWORD(4), &nLengthNeeded));
  winapi_stor_DWORD(nLengthNeeded, 5);
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationA(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WINAPI_SETUSEROBJECTINFORMATIONA )
{
  winapi_ret_BOOL(SetUserObjectInformationA(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WINAPI_SETUSEROBJECTINFORMATIONW )
{
  winapi_ret_BOOL(SetUserObjectInformationW(winapi_par_HANDLE(1), winapi_par_int(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsHungAppWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_ISHUNGAPPWINDOW )
{
  winapi_ret_BOOL(IsHungAppWindow(winapi_par_HWND(1)));
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
  winapi_ret_UINT(RegisterWindowMessageA(( LPCSTR ) hb_parc(1)));
}

/*
WINUSERAPI UINT WINAPI RegisterWindowMessageW(LPCWSTR lpString)
*/
HB_FUNC( WINAPI_REGISTERWINDOWMESSAGEW )
{
  winapi_ret_UINT(RegisterWindowMessageW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_REGISTERWINDOWMESSAGE )
{
  void * str1;
  winapi_ret_UINT(RegisterWindowMessage(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI WINBOOL WINAPI TrackMouseEvent(LPTRACKMOUSEEVENT lpEventTrack)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawEdge(HDC hdc,LPRECT qrc,UINT edge,UINT grfFlags)
*/
HB_FUNC( WINAPI_DRAWEDGE )
{
  winapi_ret_BOOL(DrawEdge(winapi_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2)), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawFrameControl(HDC,LPRECT,UINT,UINT)
*/
HB_FUNC( WINAPI_DRAWFRAMECONTROL )
{
  winapi_ret_BOOL(DrawFrameControl(winapi_par_HDC(1), static_cast<LPRECT>(winapi_get_ptr(2)), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawCaption(HWND hwnd,HDC hdc,CONST RECT *lprect,UINT flags)
*/
HB_FUNC( WINAPI_DRAWCAPTION )
{
  winapi_ret_BOOL(DrawCaption(winapi_par_HWND(1), winapi_par_HDC(2), static_cast<CONST RECT*>(winapi_get_ptr(3)), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawAnimatedRects(HWND hwnd,int idAni,CONST RECT *lprcFrom,CONST RECT *lprcTo)
*/
HB_FUNC( WINAPI_DRAWANIMATEDRECTS )
{
  winapi_ret_BOOL(DrawAnimatedRects(winapi_par_HWND(1), winapi_par_int(2), static_cast<CONST RECT*>(winapi_get_ptr(3)), static_cast<CONST RECT*>(winapi_get_ptr(4))));
}

/*
WINUSERAPI WINBOOL WINAPI GetMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/
HB_FUNC( WINAPI_GETMESSAGEA )
{
  winapi_ret_BOOL(GetMessageA(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI GetMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/
HB_FUNC( WINAPI_GETMESSAGEW )
{
  winapi_ret_BOOL(GetMessageW(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4)));
}

HB_FUNC( WINAPI_GETMESSAGE )
{
  winapi_ret_BOOL(GetMessage(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI TranslateMessage(CONST MSG *lpMsg)
*/
HB_FUNC( WINAPI_TRANSLATEMESSAGE )
{
  winapi_ret_BOOL(TranslateMessage(static_cast<CONST MSG*>(winapi_get_ptr(1))));
}

/*
WINUSERAPI LRESULT WINAPI DispatchMessageA(CONST MSG *lpMsg)
*/
HB_FUNC( WINAPI_DISPATCHMESSAGEA )
{
  winapi_ret_LRESULT(DispatchMessageA(static_cast<CONST MSG*>(winapi_get_ptr(1))));
}

/*
WINUSERAPI LRESULT WINAPI DispatchMessageW(CONST MSG *lpMsg)
*/
HB_FUNC( WINAPI_DISPATCHMESSAGEW )
{
  winapi_ret_LRESULT(DispatchMessageW(static_cast<CONST MSG*>(winapi_get_ptr(1))));
}

HB_FUNC( WINAPI_DISPATCHMESSAGE )
{
  winapi_ret_LRESULT(DispatchMessage(static_cast<CONST MSG*>(winapi_get_ptr(1))));
}

/*
WINUSERAPI WINBOOL WINAPI SetMessageQueue(int cMessagesMax)
*/
HB_FUNC( WINAPI_SETMESSAGEQUEUE )
{
  winapi_ret_BOOL(SetMessageQueue(winapi_par_int(1)));
}

/*
WINUSERAPI WINBOOL WINAPI PeekMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/
HB_FUNC( WINAPI_PEEKMESSAGEA )
{
  winapi_ret_BOOL(PeekMessageA(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI PeekMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/
HB_FUNC( WINAPI_PEEKMESSAGEW )
{
  winapi_ret_BOOL(PeekMessageW(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

HB_FUNC( WINAPI_PEEKMESSAGE )
{
  winapi_ret_BOOL(PeekMessage(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI RegisterHotKey(HWND hWnd,int id,UINT fsModifiers,UINT vk)
*/
HB_FUNC( WINAPI_REGISTERHOTKEY )
{
  winapi_ret_BOOL(RegisterHotKey(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterHotKey(HWND hWnd,int id)
*/
HB_FUNC( WINAPI_UNREGISTERHOTKEY )
{
  winapi_ret_BOOL(UnregisterHotKey(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ExitWindowsEx(UINT uFlags,DWORD dwReason)
*/
HB_FUNC( WINAPI_EXITWINDOWSEX )
{
  winapi_ret_BOOL(ExitWindowsEx(winapi_par_UINT(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SwapMouseButton(WINBOOL fSwap)
*/
HB_FUNC( WINAPI_SWAPMOUSEBUTTON )
{
  winapi_ret_BOOL(SwapMouseButton(winapi_par_BOOL(1)));
}

/*
WINUSERAPI DWORD WINAPI GetMessagePos(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGEPOS )
{
  winapi_ret_DWORD(GetMessagePos());
}

/*
WINUSERAPI LONG WINAPI GetMessageTime(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGETIME )
{
  winapi_ret_LONG(GetMessageTime());
}

/*
WINUSERAPI LPARAM WINAPI GetMessageExtraInfo(VOID)
*/
HB_FUNC( WINAPI_GETMESSAGEEXTRAINFO )
{
  winapi_ret_LPARAM(GetMessageExtraInfo());
}

/*
WINUSERAPI DWORD WINAPI GetUnpredictedMessagePos(VOID)
*/
#if 0
HB_FUNC( WINAPI_GETUNPREDICTEDMESSAGEPOS )
{
  winapi_ret_DWORD(GetUnpredictedMessagePos());
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsWow64Message(VOID)
*/
HB_FUNC( WINAPI_ISWOW64MESSAGE )
{
  winapi_ret_BOOL(IsWow64Message());
}

/*
WINUSERAPI LPARAM WINAPI SetMessageExtraInfo(LPARAM lParam)
*/
HB_FUNC( WINAPI_SETMESSAGEEXTRAINFO )
{
  winapi_ret_LPARAM(SetMessageExtraInfo(winapi_par_LPARAM(1)));
}

/*
WINUSERAPI LRESULT WINAPI SendMessageA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDMESSAGEA )
{
  winapi_ret_LRESULT(SendMessageA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI LRESULT WINAPI SendMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDMESSAGEW )
{
  winapi_ret_LRESULT(SendMessageW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
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
  winapi_ret_BOOL(SendNotifyMessageA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SendNotifyMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDNOTIFYMESSAGEW )
{
  winapi_ret_BOOL(SendNotifyMessageW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
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
  winapi_ret_BOOL(UnregisterPowerSettingNotification(winapi_par_HPOWERNOTIFY(1)));
}

/*
WINUSERAPI HPOWERNOTIFY WINAPI RegisterSuspendResumeNotification(HANDLE hRecipient, DWORD Flags)
*/
HB_FUNC( WINAPI_REGISTERSUSPENDRESUMENOTIFICATION )
{
  winapi_ret_HPOWERNOTIFY(RegisterSuspendResumeNotification(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterSuspendResumeNotification(HPOWERNOTIFY Handle)
*/
HB_FUNC( WINAPI_UNREGISTERSUSPENDRESUMENOTIFICATION )
{
  winapi_ret_BOOL(UnregisterSuspendResumeNotification(winapi_par_HPOWERNOTIFY(1)));
}

/*
WINUSERAPI WINBOOL WINAPI PostMessageA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTMESSAGEA )
{
  winapi_ret_BOOL(PostMessageA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI PostMessageW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTMESSAGEW )
{
  winapi_ret_BOOL(PostMessageW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageA(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTTHREADMESSAGEA )
{
  winapi_ret_BOOL(PostThreadMessageA(winapi_par_DWORD(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageW(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_POSTTHREADMESSAGEW )
{
  winapi_ret_BOOL(PostThreadMessageW(winapi_par_DWORD(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI AttachThreadInput(DWORD idAttach, DWORD idAttachTo, WINBOOL fAttach)
*/
HB_FUNC( WINAPI_ATTACHTHREADINPUT )
{
  winapi_ret_BOOL(AttachThreadInput(winapi_par_DWORD(1), winapi_par_DWORD(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI ReplyMessage(LRESULT lResult)
*/
HB_FUNC( WINAPI_REPLYMESSAGE )
{
  winapi_ret_BOOL(ReplyMessage(winapi_par_LRESULT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI WaitMessage(VOID)
*/
HB_FUNC( WINAPI_WAITMESSAGE )
{
  winapi_ret_BOOL(WaitMessage());
}

/*
WINUSERAPI DWORD WINAPI WaitForInputIdle(HANDLE hProcess, DWORD dwMilliseconds)
*/
HB_FUNC( WINAPI_WAITFORINPUTIDLE )
{
  winapi_ret_DWORD(WaitForInputIdle(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI LRESULT WINAPI DefWindowProcA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFWINDOWPROCA )
{
  winapi_ret_LRESULT(DefWindowProcA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI LRESULT WINAPI DefWindowProcW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFWINDOWPROCW )
{
  winapi_ret_LRESULT(DefWindowProcW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI VOID WINAPI PostQuitMessage(int nExitCode)
*/
HB_FUNC( WINAPI_POSTQUITMESSAGE )
{
  PostQuitMessage(winapi_par_int(1));
}

/*
WINUSERAPI WINBOOL WINAPI InSendMessage(VOID)
*/
HB_FUNC( WINAPI_INSENDMESSAGE )
{
  winapi_ret_BOOL(InSendMessage());
}

/*
WINUSERAPI DWORD WINAPI InSendMessageEx(LPVOID lpReserved)
*/
HB_FUNC( WINAPI_INSENDMESSAGEEX )
{
  winapi_ret_DWORD(InSendMessageEx(static_cast<LPVOID>(hb_parptr(1))));
}

/*
WINUSERAPI UINT WINAPI GetDoubleClickTime(VOID)
*/
HB_FUNC( WINAPI_GETDOUBLECLICKTIME )
{
  winapi_ret_UINT(GetDoubleClickTime());
}

/*
WINUSERAPI WINBOOL WINAPI SetDoubleClickTime(UINT)
*/
HB_FUNC( WINAPI_SETDOUBLECLICKTIME )
{
  winapi_ret_BOOL(SetDoubleClickTime(winapi_par_UINT(1)));
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
  winapi_ret_BOOL(UnregisterClassA(( LPCSTR ) hb_parc(1), winapi_par_HINSTANCE(2)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterClassW(LPCWSTR lpClassName, HINSTANCE hInstance)
*/
HB_FUNC( WINAPI_UNREGISTERCLASSW )
{
  winapi_ret_BOOL(UnregisterClassW(( LPCWSTR ) hb_parc(1), winapi_par_HINSTANCE(2)));
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
  winapi_ret_HDEVNOTIFY(RegisterDeviceNotificationA(winapi_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), winapi_par_DWORD(3)));
}

/*
WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationW(HANDLE hRecipient,LPVOID NotificationFilter,DWORD Flags)
*/
HB_FUNC( WINAPI_REGISTERDEVICENOTIFICATIONW )
{
  winapi_ret_HDEVNOTIFY(RegisterDeviceNotificationW(winapi_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), winapi_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterDeviceNotification(HDEVNOTIFY Handle)
*/
HB_FUNC( WINAPI_UNREGISTERDEVICENOTIFICATION )
{
  winapi_ret_BOOL(UnregisterDeviceNotification(winapi_par_HDEVNOTIFY(1)));
}

/*
WINUSERAPI HWND WINAPI CreateWindowExA(DWORD dwExStyle,LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
HB_FUNC( WINAPI_CREATEWINDOWEXA )
{
  winapi_ret_HWND(CreateWindowExA(winapi_par_DWORD(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), winapi_par_DWORD(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_HWND(9), winapi_par_HMENU(10), winapi_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
}

/*
WINUSERAPI HWND WINAPI CreateWindowExW(DWORD dwExStyle,LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
HB_FUNC( WINAPI_CREATEWINDOWEXW )
{
  winapi_ret_HWND(CreateWindowExW(winapi_par_DWORD(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), winapi_par_DWORD(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_HWND(9), winapi_par_HMENU(10), winapi_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
}

HB_FUNC( WINAPI_CREATEWINDOWEX )
{
  void * str2;
  void * str3;
  winapi_ret_HWND(CreateWindowEx(winapi_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), winapi_par_DWORD(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_int(8), winapi_par_HWND(9), winapi_par_HMENU(10), winapi_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINUSERAPI WINBOOL WINAPI IsWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOW )
{
  winapi_ret_BOOL(IsWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsMenu(HMENU hMenu)
*/
HB_FUNC( WINAPI_ISMENU )
{
  winapi_ret_BOOL(IsMenu(winapi_par_HMENU(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsChild(HWND hWndParent,HWND hWnd)
*/
HB_FUNC( WINAPI_ISCHILD )
{
  winapi_ret_BOOL(IsChild(winapi_par_HWND(1), winapi_par_HWND(2)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_DESTROYWINDOW )
{
  winapi_ret_BOOL(DestroyWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindow(HWND hWnd,int nCmdShow)
*/
HB_FUNC( WINAPI_SHOWWINDOW )
{
  winapi_ret_BOOL(ShowWindow(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI AnimateWindow(HWND hWnd,DWORD dwTime,DWORD dwFlags)
*/
HB_FUNC( WINAPI_ANIMATEWINDOW )
{
  winapi_ret_BOOL(AnimateWindow(winapi_par_HWND(1), winapi_par_DWORD(2), winapi_par_DWORD(3)));
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
HB_FUNC( WINAPI_GETLAYEREDWINDOWATTRIBUTES )
{
  COLORREF crKey;
  BYTE bAlpha;
  DWORD dwFlags;
  winapi_ret_BOOL(GetLayeredWindowAttributes(winapi_par_HWND(1), &crKey, &bAlpha, &dwFlags));
  winapi_stor_COLORREF(crKey, 2);
  winapi_stor_BYTE(bAlpha, 3);
  winapi_stor_DWORD(dwFlags, 4);
}

/*
WINUSERAPI WINBOOL WINAPI PrintWindow (HWND hwnd, HDC hdcBlt, UINT nFlags)
*/
HB_FUNC( WINAPI_PRINTWINDOW )
{
  winapi_ret_BOOL(PrintWindow(winapi_par_HWND(1), winapi_par_HDC(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetLayeredWindowAttributes (HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags)
*/
HB_FUNC( WINAPI_SETLAYEREDWINDOWATTRIBUTES )
{
  winapi_ret_BOOL(SetLayeredWindowAttributes(winapi_par_HWND(1), winapi_par_COLORREF(2), winapi_par_BYTE(3), winapi_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindowAsync (HWND hWnd, int nCmdShow)
*/
HB_FUNC( WINAPI_SHOWWINDOWASYNC )
{
  winapi_ret_BOOL(ShowWindowAsync(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindow (HWND hWnd, WINBOOL bInvert)
*/
HB_FUNC( WINAPI_FLASHWINDOW )
{
  winapi_ret_BOOL(FlashWindow(winapi_par_HWND(1), winapi_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindowEx (PFLASHWINFO pfwi)
*/

/*
WINUSERAPI WINBOOL WINAPI ShowOwnedPopups (HWND hWnd, WINBOOL fShow)
*/
HB_FUNC( WINAPI_SHOWOWNEDPOPUPS )
{
  winapi_ret_BOOL(ShowOwnedPopups(winapi_par_HWND(1), winapi_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OpenIcon (HWND hWnd)
*/
HB_FUNC( WINAPI_OPENICON )
{
  winapi_ret_BOOL(OpenIcon(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseWindow (HWND hWnd)
*/
HB_FUNC( WINAPI_CLOSEWINDOW )
{
  winapi_ret_BOOL(CloseWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI MoveWindow (HWND hWnd, int X, int Y, int nWidth, int nHeight, WINBOOL bRepaint)
*/
HB_FUNC( WINAPI_MOVEWINDOW )
{
  winapi_ret_BOOL(MoveWindow(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_BOOL(6)));
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowPos (HWND hWnd, HWND hWndInsertAfter, int X, int Y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WINAPI_SETWINDOWPOS )
{
  winapi_ret_BOOL(SetWindowPos(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_UINT(7)));
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowPlacement (HWND hWnd, WINDOWPLACEMENT *lpwndpl)
*/

/*
WINUSERAPI WINBOOL WINAPI SetWindowPlacement (HWND hWnd, CONST WINDOWPLACEMENT *lpwndpl)
*/

/*
WINUSERAPI WINBOOL WINAPI GetWindowDisplayAffinity(HWND hWnd, DWORD *pdwAffinity)
*/
#if 0
HB_FUNC( WINAPI_GETWINDOWDISPLAYAFFINITY )
{
  DWORD dwAffinity;
  winapi_ret_BOOL(GetWindowDisplayAffinity(winapi_par_HWND(1), &dwAffinity));
  winapi_stor_DWORD(dwAffinity, 2);
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetWindowDisplayAffinity(HWND hWnd, DWORD dwAffinity)
*/
#if 0
HB_FUNC( WINAPI_SETWINDOWDISPLAYAFFINITY )
{
  winapi_ret_BOOL(SetWindowDisplayAffinity(winapi_par_HWND(1), winapi_par_DWORD(2)));
}
#endif

/*
WINUSERAPI HDWP WINAPI BeginDeferWindowPos (int nNumWindows)
*/
HB_FUNC( WINAPI_BEGINDEFERWINDOWPOS )
{
  winapi_ret_HDWP(BeginDeferWindowPos(winapi_par_int(1)));
}

/*
WINUSERAPI HDWP WINAPI DeferWindowPos (HDWP hWinPosInfo, HWND hWnd, HWND hWndInsertAfter, int x, int y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WINAPI_DEFERWINDOWPOS )
{
  winapi_ret_HDWP(DeferWindowPos(winapi_par_HDWP(1), winapi_par_HWND(2), winapi_par_HWND(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_UINT(8)));
}

/*
WINUSERAPI WINBOOL WINAPI EndDeferWindowPos (HDWP hWinPosInfo)
*/
HB_FUNC( WINAPI_ENDDEFERWINDOWPOS )
{
  winapi_ret_BOOL(EndDeferWindowPos(winapi_par_HDWP(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowVisible (HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWVISIBLE )
{
  winapi_ret_BOOL(IsWindowVisible(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsIconic (HWND hWnd)
*/
HB_FUNC( WINAPI_ISICONIC )
{
  winapi_ret_BOOL(IsIconic(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI AnyPopup (VOID)
*/
HB_FUNC( WINAPI_ANYPOPUP )
{
  winapi_ret_BOOL(AnyPopup());
}

/*
WINUSERAPI WINBOOL WINAPI BringWindowToTop (HWND hWnd)
*/
HB_FUNC( WINAPI_BRINGWINDOWTOTOP )
{
  winapi_ret_BOOL(BringWindowToTop(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsZoomed (HWND hWnd)
*/
HB_FUNC( WINAPI_ISZOOMED )
{
  winapi_ret_BOOL(IsZoomed(winapi_par_HWND(1)));
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
  winapi_ret_BOOL(EndDialog(winapi_par_HWND(1), winapi_par_INT_PTR(2)));
}

/*
WINUSERAPI HWND WINAPI GetDlgItem(HWND hDlg,int nIDDlgItem)
*/
HB_FUNC( WINAPI_GETDLGITEM )
{
  winapi_ret_HWND(GetDlgItem(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemInt(HWND hDlg,int nIDDlgItem,UINT uValue,WINBOOL bSigned)
*/
HB_FUNC( WINAPI_SETDLGITEMINT )
{
  winapi_ret_BOOL(SetDlgItemInt(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_BOOL(4)));
}

/*
WINUSERAPI UINT WINAPI GetDlgItemInt(HWND hDlg,int nIDDlgItem,WINBOOL *lpTranslated,WINBOOL bSigned)
*/
HB_FUNC( WINAPI_GETDLGITEMINT )
{
  BOOL Translated;
  winapi_ret_UINT(GetDlgItemInt(winapi_par_HWND(1), winapi_par_int(2), &Translated, winapi_par_BOOL(4)));
  winapi_stor_BOOL(Translated, 3);
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPCSTR lpString)
*/
HB_FUNC( WINAPI_SETDLGITEMTEXTA )
{
  winapi_ret_BOOL(SetDlgItemTextA(winapi_par_HWND(1), winapi_par_int(2), ( LPCSTR ) hb_parc(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_SETDLGITEMTEXTW )
{
  winapi_ret_BOOL(SetDlgItemTextW(winapi_par_HWND(1), winapi_par_int(2), ( LPCWSTR ) hb_parc(3)));
}

HB_FUNC( WINAPI_SETDLGITEMTEXT )
{
  void * str3;
  winapi_ret_BOOL(SetDlgItemText(winapi_par_HWND(1), winapi_par_int(2), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str3);
}

/*
WINUSERAPI UINT WINAPI GetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPSTR lpString,int cchMax)
*/
HB_FUNC( WINAPI_GETDLGITEMTEXTA )
{
  winapi_ret_UINT(GetDlgItemTextA(winapi_par_HWND(1), winapi_par_int(2), ( LPSTR ) hb_parc(3), winapi_par_int(4)));
}

/*
WINUSERAPI UINT WINAPI GetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPWSTR lpString,int cchMax)
*/
HB_FUNC( WINAPI_GETDLGITEMTEXTW )
{
  winapi_ret_UINT(GetDlgItemTextW(winapi_par_HWND(1), winapi_par_int(2), ( LPWSTR ) hb_parc(3), winapi_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI CheckDlgButton(HWND hDlg,int nIDButton,UINT uCheck)
*/
HB_FUNC( WINAPI_CHECKDLGBUTTON )
{
  winapi_ret_BOOL(CheckDlgButton(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CheckRadioButton(HWND hDlg,int nIDFirstButton,int nIDLastButton,int nIDCheckButton)
*/
HB_FUNC( WINAPI_CHECKRADIOBUTTON )
{
  winapi_ret_BOOL(CheckRadioButton(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI UINT WINAPI IsDlgButtonChecked(HWND hDlg,int nIDButton)
*/
HB_FUNC( WINAPI_ISDLGBUTTONCHECKED )
{
  winapi_ret_UINT(IsDlgButtonChecked(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageA(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDDLGITEMMESSAGEA )
{
  winapi_ret_LRESULT(SendDlgItemMessageA(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_WPARAM(4), winapi_par_LPARAM(5)));
}

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageW(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_SENDDLGITEMMESSAGEW )
{
  winapi_ret_LRESULT(SendDlgItemMessageW(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_WPARAM(4), winapi_par_LPARAM(5)));
}

/*
WINUSERAPI HWND WINAPI GetNextDlgGroupItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WINAPI_GETNEXTDLGGROUPITEM )
{
  winapi_ret_HWND(GetNextDlgGroupItem(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI HWND WINAPI GetNextDlgTabItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WINAPI_GETNEXTDLGTABITEM )
{
  winapi_ret_HWND(GetNextDlgTabItem(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI GetDlgCtrlID(HWND hWnd)
*/
HB_FUNC( WINAPI_GETDLGCTRLID )
{
  winapi_ret_int(GetDlgCtrlID(winapi_par_HWND(1)));
}

/*
WINUSERAPI __LONG32 WINAPI GetDialogBaseUnits(VOID)
*/
HB_FUNC( WINAPI_GETDIALOGBASEUNITS )
{
  winapi_ret___LONG32(GetDialogBaseUnits());
}

/*
WINUSERAPI LRESULT WINAPI DefDlgProcA(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFDLGPROCA )
{
  winapi_ret_LRESULT(DefDlgProcA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI LRESULT WINAPI DefDlgProcW(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFDLGPROCW )
{
  winapi_ret_LRESULT(DefDlgProcW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterA(LPMSG lpMsg,int nCode)
*/
HB_FUNC( WINAPI_CALLMSGFILTERA )
{
  winapi_ret_BOOL(CallMsgFilterA(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterW(LPMSG lpMsg,int nCode)
*/
HB_FUNC( WINAPI_CALLMSGFILTERW )
{
  winapi_ret_BOOL(CallMsgFilterW(static_cast<LPMSG>(winapi_get_ptr(1)), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OpenClipboard(HWND hWndNewOwner)
*/
HB_FUNC( WINAPI_OPENCLIPBOARD )
{
  winapi_ret_BOOL(OpenClipboard(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseClipboard(VOID)
*/
HB_FUNC( WINAPI_CLOSECLIPBOARD )
{
  winapi_ret_BOOL(CloseClipboard());
}

/*
WINUSERAPI DWORD WINAPI GetClipboardSequenceNumber(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDSEQUENCENUMBER )
{
  winapi_ret_DWORD(GetClipboardSequenceNumber());
}

/*
WINUSERAPI HWND WINAPI GetClipboardOwner(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDOWNER )
{
  winapi_ret_HWND(GetClipboardOwner());
}

/*
WINUSERAPI HWND WINAPI SetClipboardViewer(HWND hWndNewViewer)
*/
HB_FUNC( WINAPI_SETCLIPBOARDVIEWER )
{
  winapi_ret_HWND(SetClipboardViewer(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetClipboardViewer(VOID)
*/
HB_FUNC( WINAPI_GETCLIPBOARDVIEWER )
{
  winapi_ret_HWND(GetClipboardViewer());
}

/*
WINUSERAPI WINBOOL WINAPI ChangeClipboardChain(HWND hWndRemove, HWND hWndNewNext)
*/
HB_FUNC( WINAPI_CHANGECLIPBOARDCHAIN )
{
  winapi_ret_BOOL(ChangeClipboardChain(winapi_par_HWND(1), winapi_par_HWND(2)));
}

/*
WINUSERAPI HANDLE WINAPI SetClipboardData(UINT uFormat, HANDLE hMem)
*/
HB_FUNC( WINAPI_SETCLIPBOARDDATA )
{
  winapi_ret_HANDLE(SetClipboardData(winapi_par_UINT(1), winapi_par_HANDLE(2)));
}

/*
WINUSERAPI HANDLE WINAPI GetClipboardData(UINT uFormat)
*/
HB_FUNC( WINAPI_GETCLIPBOARDDATA )
{
  winapi_ret_HANDLE(GetClipboardData(winapi_par_UINT(1)));
}

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatA(LPCSTR lpszFormat)
*/
HB_FUNC( WINAPI_REGISTERCLIPBOARDFORMATA )
{
  winapi_ret_UINT(RegisterClipboardFormatA(( LPCSTR ) hb_parc(1)));
}

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatW(LPCWSTR lpszFormat)
*/
HB_FUNC( WINAPI_REGISTERCLIPBOARDFORMATW )
{
  winapi_ret_UINT(RegisterClipboardFormatW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_REGISTERCLIPBOARDFORMAT )
{
  void * str1;
  winapi_ret_UINT(RegisterClipboardFormat(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI int WINAPI CountClipboardFormats(VOID)
*/
HB_FUNC( WINAPI_COUNTCLIPBOARDFORMATS )
{
  winapi_ret_int(CountClipboardFormats());
}

/*
WINUSERAPI UINT WINAPI EnumClipboardFormats(UINT format)
*/
HB_FUNC( WINAPI_ENUMCLIPBOARDFORMATS )
{
  winapi_ret_UINT(EnumClipboardFormats(winapi_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI GetClipboardFormatNameA(UINT format, LPSTR lpszFormatName, int cchMaxCount)
*/
HB_FUNC( WINAPI_GETCLIPBOARDFORMATNAMEA )
{
  winapi_ret_int(GetClipboardFormatNameA(winapi_par_UINT(1), ( LPSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetClipboardFormatNameW(UINT format, LPWSTR lpszFormatName, int cchMaxCount)
*/
HB_FUNC( WINAPI_GETCLIPBOARDFORMATNAMEW )
{
  winapi_ret_int(GetClipboardFormatNameW(winapi_par_UINT(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EmptyClipboard(VOID)
*/
HB_FUNC( WINAPI_EMPTYCLIPBOARD )
{
  winapi_ret_BOOL(EmptyClipboard());
}

/*
WINUSERAPI WINBOOL WINAPI IsClipboardFormatAvailable(UINT format)
*/
HB_FUNC( WINAPI_ISCLIPBOARDFORMATAVAILABLE )
{
  winapi_ret_BOOL(IsClipboardFormatAvailable(winapi_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI GetPriorityClipboardFormat(UINT *paFormatPriorityList, int cFormats)
*/

/*
WINUSERAPI HWND WINAPI GetOpenClipboardWindow(VOID)
*/
HB_FUNC( WINAPI_GETOPENCLIPBOARDWINDOW )
{
  winapi_ret_HWND(GetOpenClipboardWindow());
}

/*
WINUSERAPI WINBOOL WINAPI AddClipboardFormatListener (HWND hwnd)
*/
#if 0
HB_FUNC( WINAPI_ADDCLIPBOARDFORMATLISTENER )
{
  winapi_ret_BOOL(AddClipboardFormatListener(winapi_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI RemoveClipboardFormatListener (HWND hwnd)
*/
#if 0
HB_FUNC( WINAPI_REMOVECLIPBOARDFORMATLISTENER )
{
  winapi_ret_BOOL(RemoveClipboardFormatListener(winapi_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetUpdatedClipboardFormats (PUINT lpuiFormats, UINT cFormats, PUINT pcFormatsOut)
*/

/*
WINUSERAPI WINBOOL WINAPI CharToOemA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_CHARTOOEMA )
{
  winapi_ret_BOOL(CharToOemA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemW(LPCWSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_CHARTOOEMW )
{
  winapi_ret_BOOL(CharToOemW(( LPCWSTR ) hb_parc(1), ( LPSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WINAPI_OEMTOCHARA )
{
  winapi_ret_BOOL(OemToCharA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharW(LPCSTR lpszSrc,LPWSTR lpszDst)
*/
HB_FUNC( WINAPI_OEMTOCHARW )
{
  winapi_ret_BOOL(OemToCharW(( LPCSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_CHARTOOEMBUFFA )
{
  winapi_ret_BOOL(CharToOemBuffA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffW(LPCWSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_CHARTOOEMBUFFW )
{
  winapi_ret_BOOL(CharToOemBuffW(( LPCWSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_OEMTOCHARBUFFA )
{
  winapi_ret_BOOL(OemToCharBuffA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffW(LPCSTR lpszSrc,LPWSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WINAPI_OEMTOCHARBUFFW )
{
  winapi_ret_BOOL(OemToCharBuffW(( LPCSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI LPSTR WINAPI CharUpperA(LPSTR lpsz)
*/
HB_FUNC( WINAPI_CHARUPPERA )
{
  hb_retc(( LPSTR ) CharUpperA(( LPSTR ) hb_parc(1)));
}

/*
WINUSERAPI LPWSTR WINAPI CharUpperW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharUpperBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARUPPERBUFFA )
{
  winapi_ret_DWORD(CharUpperBuffA(( LPSTR ) hb_parc(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI CharUpperBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARUPPERBUFFW )
{
  winapi_ret_DWORD(CharUpperBuffW(( LPWSTR ) hb_parc(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI LPSTR WINAPI CharLowerA(LPSTR lpsz)
*/
HB_FUNC( WINAPI_CHARLOWERA )
{
  hb_retc(( LPSTR ) CharLowerA(( LPSTR ) hb_parc(1)));
}

/*
WINUSERAPI LPWSTR WINAPI CharLowerW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharLowerBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARLOWERBUFFA )
{
  winapi_ret_DWORD(CharLowerBuffA(( LPSTR ) hb_parc(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI CharLowerBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WINAPI_CHARLOWERBUFFW )
{
  winapi_ret_DWORD(CharLowerBuffW(( LPWSTR ) hb_parc(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI LPSTR WINAPI CharNextA(LPCSTR lpsz)
*/
HB_FUNC( WINAPI_CHARNEXTA )
{
  hb_retc(( LPSTR ) CharNextA(( LPCSTR ) hb_parc(1)));
}

/*
WINUSERAPI LPWSTR WINAPI CharNextW(LPCWSTR lpsz)
*/
#if 0
HB_FUNC( WINAPI_CHARNEXTW )
{
  hb_retc(( LPWSTR ) CharNextW(( LPCWSTR ) hb_parc(1))); // TODO: fix
}
#endif

/*
WINUSERAPI LPSTR WINAPI CharPrevA(LPCSTR lpszStart,LPCSTR lpszCurrent)
*/
HB_FUNC( WINAPI_CHARPREVA )
{
  hb_retc(( LPSTR ) CharPrevA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI LPWSTR WINAPI CharPrevW(LPCWSTR lpszStart,LPCWSTR lpszCurrent)
*/
#if 0
HB_FUNC( WINAPI_CHARPREVW )
{
  hb_retc(( LPWSTR ) CharPrevW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2))); // TODO: fix
}
#endif

/*
WINUSERAPI LPSTR WINAPI CharNextExA(WORD CodePage,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WINAPI_CHARNEXTEXA )
{
  hb_retc(( LPSTR ) CharNextExA(winapi_par_WORD(1), ( LPCSTR ) hb_parc(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI LPSTR WINAPI CharPrevExA(WORD CodePage,LPCSTR lpStart,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WINAPI_CHARPREVEXA )
{
  hb_retc(( LPSTR ) CharPrevExA(winapi_par_WORD(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), winapi_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHAA )
{
  winapi_ret_BOOL(IsCharAlphaA(winapi_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaW(WCHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHAW )
{
  winapi_ret_BOOL(IsCharAlphaW(winapi_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHANUMERICA )
{
  winapi_ret_BOOL(IsCharAlphaNumericA(winapi_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericW(WCHAR ch)
*/
HB_FUNC( WINAPI_ISCHARALPHANUMERICW )
{
  winapi_ret_BOOL(IsCharAlphaNumericW(winapi_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARUPPERA )
{
  winapi_ret_BOOL(IsCharUpperA(winapi_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperW(WCHAR ch)
*/
HB_FUNC( WINAPI_ISCHARUPPERW )
{
  winapi_ret_BOOL(IsCharUpperW(winapi_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerA(CHAR ch)
*/
HB_FUNC( WINAPI_ISCHARLOWERA )
{
  winapi_ret_BOOL(IsCharLowerA(winapi_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerW(WCHAR ch)
*/
HB_FUNC( WINAPI_ISCHARLOWERW )
{
  winapi_ret_BOOL(IsCharLowerW(winapi_par_WCHAR(1)));
}

/*
WINUSERAPI HWND WINAPI SetFocus(HWND hWnd)
*/
HB_FUNC( WINAPI_SETFOCUS )
{
  winapi_ret_HWND(SetFocus(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetActiveWindow(VOID)
*/
HB_FUNC( WINAPI_GETACTIVEWINDOW )
{
  winapi_ret_HWND(GetActiveWindow());
}

/*
WINUSERAPI HWND WINAPI GetFocus(VOID)
*/
HB_FUNC( WINAPI_GETFOCUS )
{
  winapi_ret_HWND(GetFocus());
}

/*
WINUSERAPI UINT WINAPI GetKBCodePage(VOID)
*/
HB_FUNC( WINAPI_GETKBCODEPAGE )
{
  winapi_ret_UINT(GetKBCodePage());
}

/*
WINUSERAPI SHORT WINAPI GetKeyState(int nVirtKey)
*/
HB_FUNC( WINAPI_GETKEYSTATE )
{
  winapi_ret_SHORT(GetKeyState(winapi_par_int(1)));
}

/*
WINUSERAPI SHORT WINAPI GetAsyncKeyState(int vKey)
*/
HB_FUNC( WINAPI_GETASYNCKEYSTATE )
{
  winapi_ret_SHORT(GetAsyncKeyState(winapi_par_int(1)));
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
  winapi_ret_int(GetKeyNameTextA(winapi_par_LONG(1), ( LPSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetKeyNameTextW(LONG lParam,LPWSTR lpString,int cchSize)
*/
HB_FUNC( WINAPI_GETKEYNAMETEXTW )
{
  winapi_ret_int(GetKeyNameTextW(winapi_par_LONG(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetKeyboardType(int nTypeFlag)
*/
HB_FUNC( WINAPI_GETKEYBOARDTYPE )
{
  winapi_ret_int(GetKeyboardType(winapi_par_int(1)));
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
  winapi_ret_DWORD(OemKeyScan(winapi_par_WORD(1)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanA(CHAR ch)
*/
HB_FUNC( WINAPI_VKKEYSCANA )
{
  winapi_ret_SHORT(VkKeyScanA(winapi_par_CHAR(1)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanW(WCHAR ch)
*/

/*
WINUSERAPI SHORT WINAPI VkKeyScanExA(CHAR ch,HKL dwhkl)
*/
HB_FUNC( WINAPI_VKKEYSCANEXA )
{
  winapi_ret_SHORT(VkKeyScanExA(winapi_par_CHAR(1), winapi_par_HKL(2)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanExW(WCHAR ch,HKL dwhkl)
*/

/*
WINUSERAPI VOID WINAPI keybd_event(BYTE bVk,BYTE bScan,DWORD dwFlags,ULONG_PTR dwExtraInfo)
*/
HB_FUNC( WINAPI_KEYBD_EVENT )
{
  keybd_event(winapi_par_BYTE(1), winapi_par_BYTE(2), winapi_par_DWORD(3), winapi_par_ULONG_PTR(4));
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
#if 0
HB_FUNC( WINAPI_REGISTERTOUCHWINDOW )
{
  winapi_ret_BOOL(RegisterTouchWindow(winapi_par_HWND(1), winapi_par_ULONG(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI UnregisterTouchWindow (HWND hwnd)
*/
#if 0
HB_FUNC( WINAPI_UNREGISTERTOUCHWINDOW )
{
  winapi_ret_BOOL(UnregisterTouchWindow(winapi_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsTouchWindow (HWND hwnd, PULONG pulFlags)
*/
#if 0
HB_FUNC( WINAPI_ISTOUCHWINDOW )
{
  ULONG Flags;
  winapi_ret_BOOL(IsTouchWindow(winapi_par_HWND(1), &Flags));
  winapi_stor_ULONG(Flags, 2);
}
#endif

/*
WINUSERAPI WINBOOL WINAPI InitializeTouchInjection (UINT32 maxCount, DWORD dwMode)
*/
#if 0
HB_FUNC( WINAPI_INITIALIZETOUCHINJECTION )
{
  winapi_ret_BOOL(InitializeTouchInjection(winapi_par_UINT32(1), winapi_par_DWORD(2)));
}
#endif

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
#if 0
HB_FUNC( WINAPI_SKIPPOINTERFRAMEMESSAGES )
{
  winapi_ret_BOOL(SkipPointerFrameMessages(winapi_par_UINT32(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI RegisterPointerInputTarget (HWND hwnd, POINTER_INPUT_TYPE pointerType)
*/

/*
WINUSERAPI WINBOOL WINAPI UnregisterPointerInputTarget (HWND hwnd, POINTER_INPUT_TYPE pointerType)
*/

/*
WINUSERAPI WINBOOL WINAPI EnableMouseInPointer (WINBOOL fEnable)
*/
#if 0
HB_FUNC( WINAPI_ENABLEMOUSEINPOINTER )
{
  winapi_ret_BOOL(EnableMouseInPointer(winapi_par_BOOL(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsMouseInPointerEnabled (VOID)
*/
#if 0
HB_FUNC( WINAPI_ISMOUSEINPOINTERENABLED )
{
  winapi_ret_BOOL(IsMouseInPointerEnabled());
}
#endif

/*
WINUSERAPI WINBOOL WINAPI RegisterTouchHitTestingWindow (HWND hwnd, ULONG value)
*/
#if 0
HB_FUNC( WINAPI_REGISTERTOUCHHITTESTINGWINDOW )
{
  winapi_ret_BOOL(RegisterTouchHitTestingWindow(winapi_par_HWND(1), winapi_par_ULONG(2)));
}
#endif

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
  winapi_ret_UINT(MapVirtualKeyA(winapi_par_UINT(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyW(UINT uCode,UINT uMapType)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYW )
{
  winapi_ret_UINT(MapVirtualKeyW(winapi_par_UINT(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExA(UINT uCode,UINT uMapType,HKL dwhkl)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYEXA )
{
  winapi_ret_UINT(MapVirtualKeyExA(winapi_par_UINT(1), winapi_par_UINT(2), winapi_par_HKL(3)));
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExW(UINT uCode,UINT uMapType,HKL dwhkl)
*/
HB_FUNC( WINAPI_MAPVIRTUALKEYEXW )
{
  winapi_ret_UINT(MapVirtualKeyExW(winapi_par_UINT(1), winapi_par_UINT(2), winapi_par_HKL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI GetInputState(VOID)
*/
HB_FUNC( WINAPI_GETINPUTSTATE )
{
  winapi_ret_BOOL(GetInputState());
}

/*
WINUSERAPI DWORD WINAPI GetQueueStatus(UINT flags)
*/
HB_FUNC( WINAPI_GETQUEUESTATUS )
{
  winapi_ret_DWORD(GetQueueStatus(winapi_par_UINT(1)));
}

/*
WINUSERAPI HWND WINAPI GetCapture(VOID)
*/
HB_FUNC( WINAPI_GETCAPTURE )
{
  winapi_ret_HWND(GetCapture());
}

/*
WINUSERAPI HWND WINAPI SetCapture(HWND hWnd)
*/
HB_FUNC( WINAPI_SETCAPTURE )
{
  winapi_ret_HWND(SetCapture(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ReleaseCapture(VOID)
*/
HB_FUNC( WINAPI_RELEASECAPTURE )
{
  winapi_ret_BOOL(ReleaseCapture());
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
  winapi_ret_BOOL(KillTimer(winapi_par_HWND(1), winapi_par_UINT_PTR(2)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowUnicode(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWUNICODE )
{
  winapi_ret_BOOL(IsWindowUnicode(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableWindow(HWND hWnd,WINBOOL bEnable)
*/
HB_FUNC( WINAPI_ENABLEWINDOW )
{
  winapi_ret_BOOL(EnableWindow(winapi_par_HWND(1), winapi_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowEnabled(HWND hWnd)
*/
HB_FUNC( WINAPI_ISWINDOWENABLED )
{
  winapi_ret_BOOL(IsWindowEnabled(winapi_par_HWND(1)));
}

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsA(HINSTANCE hInstance,LPCSTR lpTableName)
*/
HB_FUNC( WINAPI_LOADACCELERATORSA )
{
  winapi_ret_HACCEL(LoadAcceleratorsA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsW(HINSTANCE hInstance,LPCWSTR lpTableName)
*/
HB_FUNC( WINAPI_LOADACCELERATORSW )
{
  winapi_ret_HACCEL(LoadAcceleratorsW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_LOADACCELERATORS )
{
  void * str2;
  winapi_ret_HACCEL(LoadAccelerators(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableA(LPACCEL paccel,int cAccel)
*/

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableW(LPACCEL paccel,int cAccel)
*/

/*
WINUSERAPI WINBOOL WINAPI DestroyAcceleratorTable(HACCEL hAccel)
*/
HB_FUNC( WINAPI_DESTROYACCELERATORTABLE )
{
  winapi_ret_BOOL(DestroyAcceleratorTable(winapi_par_HACCEL(1)));
}

/*
WINUSERAPI int WINAPI CopyAcceleratorTableA(HACCEL hAccelSrc,LPACCEL lpAccelDst,int cAccelEntries)
*/

/*
WINUSERAPI int WINAPI CopyAcceleratorTableW(HACCEL hAccelSrc,LPACCEL lpAccelDst,int cAccelEntries)
*/

/*
WINUSERAPI int WINAPI TranslateAcceleratorA(HWND hWnd,HACCEL hAccTable,LPMSG lpMsg)
*/
HB_FUNC( WINAPI_TRANSLATEACCELERATORA )
{
  winapi_ret_int(TranslateAcceleratorA(winapi_par_HWND(1), winapi_par_HACCEL(2), static_cast<LPMSG>(winapi_get_ptr(3))));
}

/*
WINUSERAPI int WINAPI TranslateAcceleratorW(HWND hWnd,HACCEL hAccTable,LPMSG lpMsg)
*/
HB_FUNC( WINAPI_TRANSLATEACCELERATORW )
{
  winapi_ret_int(TranslateAcceleratorW(winapi_par_HWND(1), winapi_par_HACCEL(2), static_cast<LPMSG>(winapi_get_ptr(3))));
}

/*
WINUSERAPI UINT_PTR WINAPI SetCoalescableTimer (HWND hWnd, UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc, ULONG uToleranceDelay)
*/

/*
WINUSERAPI HMENU WINAPI LoadMenuA(HINSTANCE hInstance,LPCSTR lpMenuName)
*/
HB_FUNC( WINAPI_LOADMENUA )
{
  winapi_ret_HMENU(LoadMenuA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HMENU WINAPI LoadMenuW(HINSTANCE hInstance,LPCWSTR lpMenuName)
*/
HB_FUNC( WINAPI_LOADMENUW )
{
  winapi_ret_HMENU(LoadMenuW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2)));
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
  winapi_ret_HMENU(GetMenu(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenu(HWND hWnd,HMENU hMenu)
*/
HB_FUNC( WINAPI_SETMENU )
{
  winapi_ret_BOOL(SetMenu(winapi_par_HWND(1), winapi_par_HMENU(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuA(HMENU hMenu,UINT cmd,LPCSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
HB_FUNC( WINAPI_CHANGEMENUA )
{
  winapi_ret_BOOL(ChangeMenuA(winapi_par_HMENU(1), winapi_par_UINT(2), ( LPCSTR ) hb_parc(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuW(HMENU hMenu,UINT cmd,LPCWSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
HB_FUNC( WINAPI_CHANGEMENUW )
{
  winapi_ret_BOOL(ChangeMenuW(winapi_par_HMENU(1), winapi_par_UINT(2), ( LPCWSTR ) hb_parc(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI HiliteMenuItem(HWND hWnd,HMENU hMenu,UINT uIDHiliteItem,UINT uHilite)
*/
HB_FUNC( WINAPI_HILITEMENUITEM )
{
  winapi_ret_BOOL(HiliteMenuItem(winapi_par_HWND(1), winapi_par_HMENU(2), winapi_par_UINT(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI int WINAPI GetMenuStringA(HMENU hMenu,UINT uIDItem,LPSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WINAPI_GETMENUSTRINGA )
{
  winapi_ret_int(GetMenuStringA(winapi_par_HMENU(1), winapi_par_UINT(2), ( LPSTR ) hb_parc(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI GetMenuStringW(HMENU hMenu,UINT uIDItem,LPWSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WINAPI_GETMENUSTRINGW )
{
  winapi_ret_int(GetMenuStringW(winapi_par_HMENU(1), winapi_par_UINT(2), ( LPWSTR ) hb_parc(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI UINT WINAPI GetMenuState(HMENU hMenu,UINT uId,UINT uFlags)
*/
HB_FUNC( WINAPI_GETMENUSTATE )
{
  winapi_ret_UINT(GetMenuState(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawMenuBar(HWND hWnd)
*/
HB_FUNC( WINAPI_DRAWMENUBAR )
{
  winapi_ret_BOOL(DrawMenuBar(winapi_par_HWND(1)));
}

/*
WINUSERAPI HMENU WINAPI GetSystemMenu(HWND hWnd,WINBOOL bRevert)
*/
HB_FUNC( WINAPI_GETSYSTEMMENU )
{
  winapi_ret_HMENU(GetSystemMenu(winapi_par_HWND(1), winapi_par_BOOL(2)));
}

/*
WINUSERAPI HMENU WINAPI CreateMenu(VOID)
*/
HB_FUNC( WINAPI_CREATEMENU )
{
  winapi_ret_HMENU(CreateMenu());
}

/*
WINUSERAPI HMENU WINAPI CreatePopupMenu(VOID)
*/
HB_FUNC( WINAPI_CREATEPOPUPMENU )
{
  winapi_ret_HMENU(CreatePopupMenu());
}

/*
WINUSERAPI WINBOOL WINAPI DestroyMenu(HMENU hMenu)
*/
HB_FUNC( WINAPI_DESTROYMENU )
{
  winapi_ret_BOOL(DestroyMenu(winapi_par_HMENU(1)));
}

/*
WINUSERAPI DWORD WINAPI CheckMenuItem(HMENU hMenu,UINT uIDCheckItem,UINT uCheck)
*/
HB_FUNC( WINAPI_CHECKMENUITEM )
{
  winapi_ret_DWORD(CheckMenuItem(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableMenuItem(HMENU hMenu,UINT uIDEnableItem,UINT uEnable)
*/
HB_FUNC( WINAPI_ENABLEMENUITEM )
{
  winapi_ret_BOOL(EnableMenuItem(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI HMENU WINAPI GetSubMenu(HMENU hMenu,int nPos)
*/
HB_FUNC( WINAPI_GETSUBMENU )
{
  winapi_ret_HMENU(GetSubMenu(winapi_par_HMENU(1), winapi_par_int(2)));
}

/*
WINUSERAPI UINT WINAPI GetMenuItemID(HMENU hMenu,int nPos)
*/
HB_FUNC( WINAPI_GETMENUITEMID )
{
  winapi_ret_UINT(GetMenuItemID(winapi_par_HMENU(1), winapi_par_int(2)));
}

/*
WINUSERAPI int WINAPI GetMenuItemCount(HMENU hMenu)
*/
HB_FUNC( WINAPI_GETMENUITEMCOUNT )
{
  winapi_ret_int(GetMenuItemCount(winapi_par_HMENU(1)));
}

/*
WINUSERAPI WINBOOL WINAPI InsertMenuA(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_INSERTMENUA )
{
  winapi_ret_BOOL(InsertMenuA(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_UINT_PTR(4), ( LPCSTR ) hb_parc(5)));
}

/*
WINUSERAPI WINBOOL WINAPI InsertMenuW(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_INSERTMENUW )
{
  winapi_ret_BOOL(InsertMenuW(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_UINT_PTR(4), ( LPCWSTR ) hb_parc(5)));
}

/*
WINUSERAPI WINBOOL WINAPI AppendMenuA(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_APPENDMENUA )
{
  winapi_ret_BOOL(AppendMenuA(winapi_par_HMENU(1), winapi_par_UINT(3), winapi_par_UINT_PTR(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINUSERAPI WINBOOL WINAPI AppendMenuW(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_APPENDMENUW )
{
  winapi_ret_BOOL(AppendMenuW(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT_PTR(3), ( LPCWSTR ) hb_parc(4)));
}

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuA(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
HB_FUNC( WINAPI_MODIFYMENUA )
{
  winapi_ret_BOOL(ModifyMenuA(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_UINT_PTR(4), ( LPCSTR ) hb_parc(5)));
}

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuW(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
HB_FUNC( WINAPI_MODIFYMENUW )
{
  winapi_ret_BOOL(ModifyMenuW(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_UINT_PTR(4), ( LPCWSTR ) hb_parc(5)));
}

/*
WINUSERAPI WINBOOL WINAPI RemoveMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WINAPI_REMOVEMENU )
{
  winapi_ret_BOOL(RemoveMenu(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI DeleteMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WINAPI_DELETEMENU )
{
  winapi_ret_BOOL(DeleteMenu(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuItemBitmaps(HMENU hMenu,UINT uPosition,UINT uFlags,HBITMAP hBitmapUnchecked,HBITMAP hBitmapChecked)
*/
HB_FUNC( WINAPI_SETMENUITEMBITMAPS )
{
  winapi_ret_BOOL(SetMenuItemBitmaps(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_HBITMAP(4), winapi_par_HBITMAP(5)));
}

/*
WINUSERAPI LONG WINAPI GetMenuCheckMarkDimensions(VOID)
*/
HB_FUNC( WINAPI_GETMENUCHECKMARKDIMENSIONS )
{
  winapi_ret_LONG(GetMenuCheckMarkDimensions());
}

/*
WINUSERAPI WINBOOL WINAPI TrackPopupMenu(HMENU hMenu,UINT uFlags,int x,int y,int nReserved,HWND hWnd,CONST RECT *prcRect)
*/
HB_FUNC( WINAPI_TRACKPOPUPMENU )
{
  winapi_ret_BOOL(TrackPopupMenu(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HWND(6), static_cast<CONST RECT*>(winapi_get_ptr(7))));
}

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
  winapi_ret_BOOL(EndMenu());
}

/*
WINUSERAPI WINBOOL WINAPI CalculatePopupWindowPosition(const POINT *anchorPoint, const SIZE *windowSize, UINT flags, RECT *excludeRect, RECT *popupWindowPosition)
*/
#if 0
HB_FUNC( WINAPI_CALCULATEPOPUPWINDOWPOSITION )
{
  winapi_ret_BOOL(CalculatePopupWindowPosition(static_cast<CONST POINT*>(winapi_get_ptr(1)), static_cast<CONST SIZE*>(winapi_get_ptr(2)), winapi_par_UINT(3), static_cast<RECT*>(winapi_get_ptr(4)), static_cast<RECT*>(winapi_get_ptr(5))));
}
#endif

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
  winapi_ret_UINT(GetMenuDefaultItem(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuDefaultItem(HMENU hMenu,UINT uItem,UINT fByPos)
*/
HB_FUNC( WINAPI_SETMENUDEFAULTITEM )
{
  winapi_ret_BOOL(SetMenuDefaultItem(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI GetMenuItemRect(HWND hWnd,HMENU hMenu,UINT uItem,LPRECT lprcItem)
*/
HB_FUNC( WINAPI_GETMENUITEMRECT )
{
  winapi_ret_BOOL(GetMenuItemRect(winapi_par_HWND(1), winapi_par_HMENU(2), winapi_par_UINT(3), static_cast<LPRECT>(winapi_get_ptr(4))));
}

/*
WINUSERAPI int WINAPI MenuItemFromPoint(HWND hWnd,HMENU hMenu,POINT ptScreen)
*/
#if 0
HB_FUNC( WINAPI_MENUITEMFROMPOINT )
{
  winapi_ret_int(MenuItemFromPoint(winapi_par_HWND(1), winapi_par_HMENU(2), static_cast<POINT>(winapi_get_ptr(3))));
}
#endif

/*
WINUSERAPI DWORD WINAPI DragObject(HWND hwndParent,HWND hwndFrom,UINT fmt,ULONG_PTR data,HCURSOR hcur)
*/
HB_FUNC( WINAPI_DRAGOBJECT )
{
  winapi_ret_DWORD(DragObject(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_ULONG_PTR(4), winapi_par_HCURSOR(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DragDetect(HWND hwnd,POINT pt)
*/

/*
WINUSERAPI WINBOOL WINAPI DrawIcon(HDC hDC,int X,int Y,HICON hIcon)
*/
HB_FUNC( WINAPI_DRAWICON )
{
  winapi_ret_BOOL(DrawIcon(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_HICON(4)));
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
  winapi_ret_BOOL(UpdateWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI SetActiveWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_SETACTIVEWINDOW )
{
  winapi_ret_HWND(SetActiveWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetForegroundWindow(VOID)
*/
HB_FUNC( WINAPI_GETFOREGROUNDWINDOW )
{
  winapi_ret_HWND(GetForegroundWindow());
}

/*
WINUSERAPI WINBOOL WINAPI PaintDesktop(HDC hdc)
*/
HB_FUNC( WINAPI_PAINTDESKTOP )
{
  winapi_ret_BOOL(PaintDesktop(winapi_par_HDC(1)));
}

/*
WINUSERAPI VOID WINAPI SwitchToThisWindow(HWND hwnd,WINBOOL fUnknown)
*/
HB_FUNC( WINAPI_SWITCHTOTHISWINDOW )
{
  SwitchToThisWindow(winapi_par_HWND(1), winapi_par_BOOL(2));
}

/*
WINUSERAPI WINBOOL WINAPI SetForegroundWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_SETFOREGROUNDWINDOW )
{
  winapi_ret_BOOL(SetForegroundWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI AllowSetForegroundWindow(DWORD dwProcessId)
*/
HB_FUNC( WINAPI_ALLOWSETFOREGROUNDWINDOW )
{
  winapi_ret_BOOL(AllowSetForegroundWindow(winapi_par_DWORD(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LockSetForegroundWindow(UINT uLockCode)
*/
HB_FUNC( WINAPI_LOCKSETFOREGROUNDWINDOW )
{
  winapi_ret_BOOL(LockSetForegroundWindow(winapi_par_UINT(1)));
}

/*
WINUSERAPI HWND WINAPI WindowFromDC(HDC hDC)
*/
HB_FUNC( WINAPI_WINDOWFROMDC )
{
  winapi_ret_HWND(WindowFromDC(winapi_par_HDC(1)));
}

/*
WINUSERAPI HDC WINAPI GetDC(HWND hWnd)
*/
HB_FUNC( WINAPI_GETDC )
{
  winapi_ret_HDC(GetDC(winapi_par_HWND(1)));
}

/*
WINUSERAPI HDC WINAPI GetDCEx(HWND hWnd,HRGN hrgnClip,DWORD flags)
*/
HB_FUNC( WINAPI_GETDCEX )
{
  winapi_ret_HDC(GetDCEx(winapi_par_HWND(1), winapi_par_HRGN(2), winapi_par_DWORD(3)));
}

/*
WINUSERAPI HDC WINAPI GetWindowDC(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWDC )
{
  winapi_ret_HDC(GetWindowDC(winapi_par_HWND(1)));
}

/*
WINUSERAPI int WINAPI ReleaseDC(HWND hWnd,HDC hDC)
*/
HB_FUNC( WINAPI_RELEASEDC )
{
  winapi_ret_int(ReleaseDC(winapi_par_HWND(1), winapi_par_HDC(2)));
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
  winapi_ret_int(GetUpdateRgn(winapi_par_HWND(1), winapi_par_HRGN(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI SetWindowRgn(HWND hWnd,HRGN hRgn,WINBOOL bRedraw)
*/
HB_FUNC( WINAPI_SETWINDOWRGN )
{
  winapi_ret_int(SetWindowRgn(winapi_par_HWND(1), winapi_par_HRGN(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI GetWindowRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WINAPI_GETWINDOWRGN )
{
  winapi_ret_int(GetWindowRgn(winapi_par_HWND(1), winapi_par_HRGN(2)));
}

/*
WINUSERAPI int WINAPI GetWindowRgnBox(HWND hWnd,LPRECT lprc)
*/

/*
WINUSERAPI int WINAPI ExcludeUpdateRgn(HDC hDC,HWND hWnd)
*/
HB_FUNC( WINAPI_EXCLUDEUPDATERGN )
{
  winapi_ret_int(ExcludeUpdateRgn(winapi_par_HDC(1), winapi_par_HWND(2)));
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
  winapi_ret_BOOL(InvalidateRgn(winapi_par_HWND(1), winapi_par_HRGN(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI ValidateRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WINAPI_VALIDATERGN )
{
  winapi_ret_BOOL(ValidateRgn(winapi_par_HWND(1), winapi_par_HRGN(2)));
}

/*
WINUSERAPI WINBOOL WINAPI RedrawWindow(HWND hWnd,CONST RECT *lprcUpdate,HRGN hrgnUpdate,UINT flags)
*/

/*
WINUSERAPI WINBOOL WINAPI LockWindowUpdate(HWND hWndLock)
*/
HB_FUNC( WINAPI_LOCKWINDOWUPDATE )
{
  winapi_ret_BOOL(LockWindowUpdate(winapi_par_HWND(1)));
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
  winapi_ret_int(SetScrollPos(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), winapi_par_BOOL(4)));
}

/*
WINUSERAPI int WINAPI GetScrollPos(HWND hWnd,int nBar)
*/
HB_FUNC( WINAPI_GETSCROLLPOS )
{
  winapi_ret_int(GetScrollPos(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetScrollRange(HWND hWnd,int nBar,int nMinPos,int nMaxPos,WINBOOL bRedraw)
*/
HB_FUNC( WINAPI_SETSCROLLRANGE )
{
  winapi_ret_BOOL(SetScrollRange(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_BOOL(5)));
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollRange(HWND hWnd,int nBar,LPINT lpMinPos,LPINT lpMaxPos)
*/

/*
WINUSERAPI WINBOOL WINAPI ShowScrollBar(HWND hWnd,int wBar,WINBOOL bShow)
*/
HB_FUNC( WINAPI_SHOWSCROLLBAR )
{
  winapi_ret_BOOL(ShowScrollBar(winapi_par_HWND(1), winapi_par_int(2), winapi_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableScrollBar(HWND hWnd,UINT wSBflags,UINT wArrows)
*/
HB_FUNC( WINAPI_ENABLESCROLLBAR )
{
  winapi_ret_BOOL(EnableScrollBar(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetPropA(HWND hWnd,LPCSTR lpString,HANDLE hData)
*/
HB_FUNC( WINAPI_SETPROPA )
{
  winapi_ret_BOOL(SetPropA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2), winapi_par_HANDLE(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetPropW(HWND hWnd,LPCWSTR lpString,HANDLE hData)
*/
HB_FUNC( WINAPI_SETPROPW )
{
  winapi_ret_BOOL(SetPropW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2), winapi_par_HANDLE(3)));
}

/*
WINUSERAPI HANDLE WINAPI GetPropA(HWND hWnd,LPCSTR lpString)
*/
HB_FUNC( WINAPI_GETPROPA )
{
  winapi_ret_HANDLE(GetPropA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HANDLE WINAPI GetPropW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_GETPROPW )
{
  winapi_ret_HANDLE(GetPropW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINUSERAPI HANDLE WINAPI RemovePropA(HWND hWnd,LPCSTR lpString)
*/
HB_FUNC( WINAPI_REMOVEPROPA )
{
  winapi_ret_HANDLE(RemovePropA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HANDLE WINAPI RemovePropW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_REMOVEPROPW )
{
  winapi_ret_HANDLE(RemovePropW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2)));
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
  winapi_ret_BOOL(SetWindowTextA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowTextW(HWND hWnd,LPCWSTR lpString)
*/
HB_FUNC( WINAPI_SETWINDOWTEXTW )
{
  winapi_ret_BOOL(SetWindowTextW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_SETWINDOWTEXT )
{
  void * str2;
  winapi_ret_BOOL(SetWindowText(winapi_par_HWND(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI int WINAPI GetWindowTextA(HWND hWnd,LPSTR lpString,int nMaxCount)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTA )
{
  winapi_ret_int(GetWindowTextA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetWindowTextW(HWND hWnd,LPWSTR lpString,int nMaxCount)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTW )
{
  winapi_ret_int(GetWindowTextW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetWindowTextLengthA(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTLENGTHA )
{
  winapi_ret_int(GetWindowTextLengthA(winapi_par_HWND(1)));
}

/*
WINUSERAPI int WINAPI GetWindowTextLengthW(HWND hWnd)
*/
HB_FUNC( WINAPI_GETWINDOWTEXTLENGTHW )
{
  winapi_ret_int(GetWindowTextLengthW(winapi_par_HWND(1)));
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
  winapi_ret_BOOL(SetWindowContextHelpId(winapi_par_HWND(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI GetWindowContextHelpId(HWND)
*/
HB_FUNC( WINAPI_GETWINDOWCONTEXTHELPID )
{
  winapi_ret_DWORD(GetWindowContextHelpId(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuContextHelpId(HMENU,DWORD)
*/
HB_FUNC( WINAPI_SETMENUCONTEXTHELPID )
{
  winapi_ret_BOOL(SetMenuContextHelpId(winapi_par_HMENU(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI GetMenuContextHelpId(HMENU)
*/
HB_FUNC( WINAPI_GETMENUCONTEXTHELPID )
{
  winapi_ret_DWORD(GetMenuContextHelpId(winapi_par_HMENU(1)));
}

/*
WINUSERAPI int WINAPI MessageBoxA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType)
*/
HB_FUNC( WINAPI_MESSAGEBOXA )
{
  winapi_ret_int(MessageBoxA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), winapi_par_UINT(4)));
}

/*
WINUSERAPI int WINAPI MessageBoxW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType)
*/
HB_FUNC( WINAPI_MESSAGEBOXW )
{
  winapi_ret_int(MessageBoxW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), winapi_par_UINT(4)));
}

HB_FUNC( WINAPI_MESSAGEBOX )
{
  void * str2;
  void * str3;
  winapi_ret_int(MessageBox(winapi_par_HWND(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), winapi_par_UINT(4)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINUSERAPI int WINAPI MessageBoxExA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType,WORD wLanguageId)
*/
HB_FUNC( WINAPI_MESSAGEBOXEXA )
{
  winapi_ret_int(MessageBoxExA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3), winapi_par_UINT(4), winapi_par_WORD(5)));
}

/*
WINUSERAPI int WINAPI MessageBoxExW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType,WORD wLanguageId)
*/
HB_FUNC( WINAPI_MESSAGEBOXEXW )
{
  winapi_ret_int(MessageBoxExW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3), winapi_par_UINT(4), winapi_par_WORD(5)));
}

HB_FUNC( WINAPI_MESSAGEBOXEX )
{
  void * str2;
  void * str3;
  winapi_ret_int(MessageBoxEx(winapi_par_HWND(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), winapi_par_UINT(4), winapi_par_WORD(5)));
  hb_strfree(str2);
  hb_strfree(str3);
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
  winapi_ret_BOOL(MessageBeep(winapi_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI ShowCursor(WINBOOL bShow)
*/
HB_FUNC( WINAPI_SHOWCURSOR )
{
  winapi_ret_int(ShowCursor(winapi_par_BOOL(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetCursorPos(int X,int Y)
*/
HB_FUNC( WINAPI_SETCURSORPOS )
{
  winapi_ret_BOOL(SetCursorPos(winapi_par_int(1), winapi_par_int(2)));
}

/*
WINUSERAPI HCURSOR WINAPI SetCursor(HCURSOR hCursor)
*/
HB_FUNC( WINAPI_SETCURSOR )
{
  winapi_ret_HCURSOR(SetCursor(winapi_par_HCURSOR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetCursorPos(LPPOINT lpPoint)
*/
HB_FUNC( WINAPI_GETCURSORPOS )
{
  winapi_ret_BOOL(GetCursorPos(static_cast<LPPOINT>(winapi_get_ptr(1))));
}

/*
WINUSERAPI WINBOOL WINAPI ClipCursor(CONST RECT *lpRect)
*/

/*
WINUSERAPI WINBOOL WINAPI GetClipCursor(LPRECT lpRect)
*/
HB_FUNC( WINAPI_GETCLIPCURSOR )
{
  winapi_ret_BOOL(GetClipCursor(static_cast<LPRECT>(winapi_get_ptr(1))));
}

/*
WINUSERAPI HCURSOR WINAPI GetCursor(VOID)
*/
HB_FUNC( WINAPI_GETCURSOR )
{
  winapi_ret_HCURSOR(GetCursor());
}

/*
WINUSERAPI WINBOOL WINAPI CreateCaret(HWND hWnd,HBITMAP hBitmap,int nWidth,int nHeight)
*/
HB_FUNC( WINAPI_CREATECARET )
{
  winapi_ret_BOOL(CreateCaret(winapi_par_HWND(1), winapi_par_HBITMAP(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI UINT WINAPI GetCaretBlinkTime(VOID)
*/
HB_FUNC( WINAPI_GETCARETBLINKTIME )
{
  winapi_ret_UINT(GetCaretBlinkTime());
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretBlinkTime(UINT uMSeconds)
*/
HB_FUNC( WINAPI_SETCARETBLINKTIME )
{
  winapi_ret_BOOL(SetCaretBlinkTime(winapi_par_UINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyCaret(VOID)
*/
HB_FUNC( WINAPI_DESTROYCARET )
{
  winapi_ret_BOOL(DestroyCaret());
}

/*
WINUSERAPI WINBOOL WINAPI HideCaret(HWND hWnd)
*/
HB_FUNC( WINAPI_HIDECARET )
{
  winapi_ret_BOOL(HideCaret(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowCaret(HWND hWnd)
*/
HB_FUNC( WINAPI_SHOWCARET )
{
  winapi_ret_BOOL(ShowCaret(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretPos(int X,int Y)
*/
HB_FUNC( WINAPI_SETCARETPOS )
{
  winapi_ret_BOOL(SetCaretPos(winapi_par_int(1), winapi_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetCaretPos(LPPOINT lpPoint)
*/
HB_FUNC( WINAPI_GETCARETPOS )
{
  winapi_ret_BOOL(GetCaretPos(static_cast<LPPOINT>(winapi_get_ptr(1))));
}

/*
WINUSERAPI WINBOOL WINAPI ClientToScreen(HWND hWnd,LPPOINT lpPoint)
*/
HB_FUNC( WINAPI_CLIENTOSCREEN )
{
  winapi_ret_BOOL(ClientToScreen(winapi_par_HWND(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINUSERAPI WINBOOL WINAPI ScreenToClient(HWND hWnd,LPPOINT lpPoint)
*/
HB_FUNC( WINAPI_SCREENTOCLIENT )
{
  winapi_ret_BOOL(ScreenToClient(winapi_par_HWND(1), static_cast<LPPOINT>(winapi_get_ptr(2))));
}

/*
WINUSERAPI int WINAPI MapWindowPoints(HWND hWndFrom,HWND hWndTo,LPPOINT lpPoints,UINT cPoints)
*/
HB_FUNC( WINAPI_MAPWINDOWPOINTS )
{
  winapi_ret_int(MapWindowPoints(winapi_par_HWND(1), winapi_par_HWND(2), static_cast<LPPOINT>(winapi_get_ptr(3)), winapi_par_UINT(4)));
}

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
  winapi_ret_DWORD(GetSysColor(winapi_par_int(1)));
}

/*
WINUSERAPI HBRUSH WINAPI GetSysColorBrush(int nIndex)
*/
HB_FUNC( WINAPI_GETSYSCOLORBRUSH )
{
  winapi_ret_HBRUSH(GetSysColorBrush(winapi_par_int(1)));
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
  winapi_ret_WORD(GetWindowWord(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WORD WINAPI SetWindowWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WINAPI_SETWINDOWWORD )
{
  winapi_ret_WORD(SetWindowWord(winapi_par_HWND(1), winapi_par_int(2), winapi_par_WORD(3)));
}

/*
WINUSERAPI LONG WINAPI GetWindowLongA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETWINDOWLONGA )
{
  winapi_ret_LONG(GetWindowLongA(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI LONG WINAPI GetWindowLongW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETWINDOWLONGW )
{
  winapi_ret_LONG(GetWindowLongW(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI LONG WINAPI SetWindowLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETWINDOWLONGA )
{
  winapi_ret_LONG(SetWindowLongA(winapi_par_HWND(1), winapi_par_int(2), winapi_par_LONG(3)));
}

/*
WINUSERAPI LONG WINAPI SetWindowLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETWINDOWLONGW )
{
  winapi_ret_LONG(SetWindowLongW(winapi_par_HWND(1), winapi_par_int(2), winapi_par_LONG(3)));
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
  winapi_ret_WORD(GetClassWord(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI WORD WINAPI SetClassWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WINAPI_SETCLASSWORD )
{
  winapi_ret_WORD(SetClassWord(winapi_par_HWND(1), winapi_par_int(2), winapi_par_WORD(3)));
}

/*
WINUSERAPI DWORD WINAPI GetClassLongA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGA )
{
  winapi_ret_DWORD(GetClassLongA(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI DWORD WINAPI GetClassLongW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGW )
{
  winapi_ret_DWORD(GetClassLongW(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI DWORD WINAPI SetClassLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETCLASSLONGA )
{
  winapi_ret_DWORD(SetClassLongA(winapi_par_HWND(1), winapi_par_int(2), winapi_par_LONG(3)));
}

/*
WINUSERAPI DWORD WINAPI SetClassLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
HB_FUNC( WINAPI_SETCLASSLONGW )
{
  winapi_ret_DWORD(SetClassLongW(winapi_par_HWND(1), winapi_par_int(2), winapi_par_LONG(3)));
}

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrA(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGPTRA )
{
  winapi_ret_ULONG_PTR(GetClassLongPtrA(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrW(HWND hWnd,int nIndex)
*/
HB_FUNC( WINAPI_GETCLASSLONGPTRW )
{
  winapi_ret_ULONG_PTR(GetClassLongPtrW(winapi_par_HWND(1), winapi_par_int(2)));
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
  winapi_ret_BOOL(SetProcessDefaultLayout(winapi_par_DWORD(1)));
}

/*
WINUSERAPI HWND WINAPI GetDesktopWindow(VOID)
*/
HB_FUNC( WINAPI_GETDESKTOPWINDOW )
{
  winapi_ret_HWND(GetDesktopWindow());
}

/*
WINUSERAPI HWND WINAPI GetParent(HWND hWnd)
*/
HB_FUNC( WINAPI_GETPARENT )
{
  winapi_ret_HWND(GetParent(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI SetParent(HWND hWndChild,HWND hWndNewParent)
*/
HB_FUNC( WINAPI_SETPARENT )
{
  winapi_ret_HWND(SetParent(winapi_par_HWND(1), winapi_par_HWND(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumChildWindows(HWND hWndParent,WNDENUMPROC lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI HWND WINAPI FindWindowA(LPCSTR lpClassName,LPCSTR lpWindowName)
*/
HB_FUNC( WINAPI_FINDWINDOWA )
{
  winapi_ret_HWND(FindWindowA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HWND WINAPI FindWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName)
*/
HB_FUNC( WINAPI_FINDWINDOWW )
{
  winapi_ret_HWND(FindWindowW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINUSERAPI HWND WINAPI FindWindowExA(HWND hWndParent,HWND hWndChildAfter,LPCSTR lpszClass,LPCSTR lpszWindow)
*/
HB_FUNC( WINAPI_FINDWINDOWEXA )
{
  winapi_ret_HWND(FindWindowExA(winapi_par_HWND(1), winapi_par_HWND(2), ( LPCSTR ) hb_parc(3), ( LPCSTR ) hb_parc(4)));
}

/*
WINUSERAPI HWND WINAPI FindWindowExW(HWND hWndParent,HWND hWndChildAfter,LPCWSTR lpszClass,LPCWSTR lpszWindow)
*/
HB_FUNC( WINAPI_FINDWINDOWEXW )
{
  winapi_ret_HWND(FindWindowExW(winapi_par_HWND(1), winapi_par_HWND(2), ( LPCWSTR ) hb_parc(3), ( LPCWSTR ) hb_parc(4)));
}

/*
WINUSERAPI HWND WINAPI GetShellWindow(VOID)
*/
HB_FUNC( WINAPI_GETSHELLWINDOW )
{
  winapi_ret_HWND(GetShellWindow());
}

/*
WINUSERAPI WINBOOL WINAPI RegisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_REGISTERSHELLHOOKWINDOW )
{
  winapi_ret_BOOL(RegisterShellHookWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI DeregisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WINAPI_DEREGISTERSHELLHOOKWINDOW )
{
  winapi_ret_BOOL(DeregisterShellHookWindow(winapi_par_HWND(1)));
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
  winapi_ret_int(GetClassNameA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetClassNameW(HWND hWnd,LPWSTR lpClassName,int nMaxCount)
*/
HB_FUNC( WINAPI_GETCLASSNAMEW )
{
  winapi_ret_int(GetClassNameW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI HWND WINAPI GetTopWindow(HWND hWnd)
*/
HB_FUNC( WINAPI_GETTOPWINDOW )
{
  winapi_ret_HWND(GetTopWindow(winapi_par_HWND(1)));
}

/*
WINUSERAPI DWORD WINAPI GetWindowThreadProcessId(HWND hWnd,LPDWORD lpdwProcessId)
*/
HB_FUNC( WINAPI_GETWINDOWTHREADPROCESSID )
{
  DWORD dwProcessId;
  winapi_ret_DWORD(GetWindowThreadProcessId(winapi_par_HWND(1), &dwProcessId));
  winapi_stor_DWORD(dwProcessId, 2);
}

/*
WINUSERAPI WINBOOL WINAPI IsGUIThread(WINBOOL bConvert)
*/
HB_FUNC( WINAPI_ISGUITHREAD )
{
  winapi_ret_BOOL(IsGUIThread(winapi_par_BOOL(1)));
}

/*
WINUSERAPI HWND WINAPI GetLastActivePopup(HWND hWnd)
*/
HB_FUNC( WINAPI_GETLASTACTIVEPOPUP )
{
  winapi_ret_HWND(GetLastActivePopup(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetWindow(HWND hWnd,UINT uCmd)
*/
HB_FUNC( WINAPI_GETWINDOW )
{
  winapi_ret_HWND(GetWindow(winapi_par_HWND(1), winapi_par_UINT(2)));
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
  winapi_ret_BOOL(CheckMenuRadioItem(winapi_par_HMENU(1), winapi_par_UINT(2), winapi_par_UINT(3), winapi_par_UINT(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapA(HINSTANCE hInstance,LPCSTR lpBitmapName)
*/
HB_FUNC( WINAPI_LOADBITMAPA )
{
  winapi_ret_HBITMAP(LoadBitmapA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapW(HINSTANCE hInstance,LPCWSTR lpBitmapName)
*/
HB_FUNC( WINAPI_LOADBITMAPW )
{
  winapi_ret_HBITMAP(LoadBitmapW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_LOADBITMAP )
{
  void * str2;
  winapi_ret_HBITMAP(LoadBitmap(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorA(HINSTANCE hInstance,LPCSTR lpCursorName)
*/
HB_FUNC( WINAPI_LOADCURSORA )
{
  winapi_ret_HCURSOR(LoadCursorA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorW(HINSTANCE hInstance,LPCWSTR lpCursorName)
*/
HB_FUNC( WINAPI_LOADCURSORW )
{
  winapi_ret_HCURSOR(LoadCursorW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_LOADCURSOR )
{
  void * str2;
  winapi_ret_HCURSOR(LoadCursor(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileA(LPCSTR lpFileName)
*/
HB_FUNC( WINAPI_LOADCURSORFROMFILEA )
{
  winapi_ret_HCURSOR(LoadCursorFromFileA(( LPCSTR ) hb_parc(1)));
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileW(LPCWSTR lpFileName)
*/
HB_FUNC( WINAPI_LOADCURSORFROMFILEW )
{
  winapi_ret_HCURSOR(LoadCursorFromFileW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_LOADCURSORFROMFILE )
{
  void * str1;
  winapi_ret_HCURSOR(LoadCursorFromFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI HCURSOR WINAPI CreateCursor(HINSTANCE hInst,int xHotSpot,int yHotSpot,int nWidth,int nHeight,CONST VOID *pvANDPlane,CONST VOID *pvXORPlane)
*/

/*
WINUSERAPI WINBOOL WINAPI DestroyCursor(HCURSOR hCursor)
*/
HB_FUNC( WINAPI_DESTROYCURSOR )
{
  winapi_ret_BOOL(DestroyCursor(winapi_par_HCURSOR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetSystemCursor(HCURSOR hcur,DWORD id)
*/
HB_FUNC( WINAPI_SETSYSTEMCURSOR )
{
  winapi_ret_BOOL(SetSystemCursor(winapi_par_HCURSOR(1), winapi_par_DWORD(2)));
}

/*
WINUSERAPI HICON WINAPI LoadIconA(HINSTANCE hInstance,LPCSTR lpIconName)
*/
HB_FUNC( WINAPI_LOADICONA )
{
  winapi_ret_HICON(LoadIconA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINUSERAPI HICON WINAPI LoadIconW(HINSTANCE hInstance,LPCWSTR lpIconName)
*/
HB_FUNC( WINAPI_LOADICONW )
{
  winapi_ret_HICON(LoadIconW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_LOADICON )
{
  void * str2;
  winapi_ret_HICON(LoadIcon(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
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
  winapi_ret_BOOL(DestroyIcon(winapi_par_HICON(1)));
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
  winapi_ret_HANDLE(LoadImageA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_int(4), winapi_par_int(5), winapi_par_UINT(6)));
}

/*
WINUSERAPI HANDLE WINAPI LoadImageW(HINSTANCE hInst,LPCWSTR name,UINT type,int cx,int cy,UINT fuLoad)
*/
HB_FUNC( WINAPI_LOADIMAGEW )
{
  winapi_ret_HANDLE(LoadImageW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_int(4), winapi_par_int(5), winapi_par_UINT(6)));
}

HB_FUNC( WINAPI_LOADIMAGE )
{
  void * str2;
  winapi_ret_HANDLE(LoadImage(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr), winapi_par_UINT(3), winapi_par_int(4), winapi_par_int(5), winapi_par_UINT(6)));
  hb_strfree(str2);
}

/*
WINUSERAPI HANDLE WINAPI CopyImage(HANDLE h,UINT type,int cx,int cy,UINT flags)
*/
HB_FUNC( WINAPI_COPYIMAGE )
{
  winapi_ret_HANDLE(CopyImage(winapi_par_HANDLE(1), winapi_par_UINT(2), winapi_par_int(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawIconEx(HDC hdc,int xLeft,int yTop,HICON hIcon,int cxWidth,int cyWidth,UINT istepIfAniCur,HBRUSH hbrFlickerFreeDraw,UINT diFlags)
*/
HB_FUNC( WINAPI_DRAWICONEX )
{
  winapi_ret_BOOL(DrawIconEx(winapi_par_HDC(1), winapi_par_int(2), winapi_par_int(3), winapi_par_HICON(4), winapi_par_int(5), winapi_par_int(6), winapi_par_UINT(7), winapi_par_HBRUSH(8), winapi_par_UINT(9)));
}

/*
WINUSERAPI HICON WINAPI CreateIconIndirect(PICONINFO piconinfo)
*/
HB_FUNC( WINAPI_CREATEICONINDIRECT )
{
  winapi_ret_HICON(CreateIconIndirect(static_cast<PICONINFO>(winapi_get_ptr(1))));
}

/*
WINUSERAPI HICON WINAPI CopyIcon(HICON hIcon)
*/
HB_FUNC( WINAPI_COPYICON )
{
  winapi_ret_HICON(CopyIcon(winapi_par_HICON(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetIconInfo(HICON hIcon,PICONINFO piconinfo)
*/
HB_FUNC( WINAPI_GETICONINFO )
{
  winapi_ret_BOOL(GetIconInfo(winapi_par_HICON(1), static_cast<PICONINFO>(winapi_get_ptr(2))));
}

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
  winapi_ret_int(LoadStringA(winapi_par_HINSTANCE(1), winapi_par_UINT(2), ( LPSTR ) hb_parc(3), winapi_par_int(4)));
}

/*
WINUSERAPI int WINAPI LoadStringW (HINSTANCE hInstance, UINT uID, LPWSTR lpBuffer, int cchBufferMax)
*/
HB_FUNC( WINAPI_LOADSTRINGW )
{
  winapi_ret_int(LoadStringW(winapi_par_HINSTANCE(1), winapi_par_UINT(2), ( LPWSTR ) hb_parc(3), winapi_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageA(HWND hDlg,LPMSG lpMsg)
*/
HB_FUNC( WINAPI_ISDIALOGMESSAGEA )
{
  winapi_ret_BOOL(IsDialogMessageA(winapi_par_HWND(1), static_cast<LPMSG>(winapi_get_ptr(2))));
}

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageW(HWND hDlg,LPMSG lpMsg)
*/
HB_FUNC( WINAPI_ISDIALOGMESSAGEW )
{
  winapi_ret_BOOL(IsDialogMessageW(winapi_par_HWND(1), static_cast<LPMSG>(winapi_get_ptr(2))));
}

/*
WINUSERAPI WINBOOL WINAPI MapDialogRect(HWND hDlg,LPRECT lpRect)
*/
HB_FUNC( WINAPI_MAPDIALOGRECT )
{
  winapi_ret_BOOL(MapDialogRect(winapi_par_HWND(1), static_cast<LPRECT>(winapi_get_ptr(2))));
}

/*
WINUSERAPI int WINAPI DlgDirListA(HWND hDlg,LPSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WINAPI_DLGDIRLISTA )
{
  winapi_ret_int(DlgDirListA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI DlgDirListW(HWND hDlg,LPWSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WINAPI_DLGDIRLISTW )
{
  winapi_ret_int(DlgDirListW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExA(HWND hwndDlg,LPSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTEXA )
{
  winapi_ret_BOOL(DlgDirSelectExA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExW(HWND hwndDlg,LPWSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTEXW )
{
  winapi_ret_BOOL(DlgDirSelectExW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxA(HWND hDlg,LPSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WINAPI_DLGDIRLISTCOMBOBOXA )
{
  winapi_ret_int(DlgDirListComboBoxA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxW(HWND hDlg,LPWSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WINAPI_DLGDIRLISTCOMBOBOXW )
{
  winapi_ret_int(DlgDirListComboBoxW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExA(HWND hwndDlg,LPSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTCOMBOBOXEXA )
{
  winapi_ret_BOOL(DlgDirSelectComboBoxExA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExW(HWND hwndDlg,LPWSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WINAPI_DLGDIRSELECTCOMBOBOXEXW )
{
  winapi_ret_BOOL(DlgDirSelectComboBoxExW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINUSERAPI int WINAPI SetScrollInfo(HWND hwnd,int nBar,LPCSCROLLINFO lpsi,WINBOOL redraw)
*/
HB_FUNC( WINAPI_SETSCROLLINFO )
{
  winapi_ret_int(SetScrollInfo(winapi_par_HWND(1), winapi_par_int(2), static_cast<LPCSCROLLINFO>(winapi_get_ptr(3)), winapi_par_BOOL(4)));
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollInfo(HWND hwnd,int nBar,LPSCROLLINFO lpsi)
*/
HB_FUNC( WINAPI_GETSCROLLINFO )
{
  winapi_ret_BOOL(GetScrollInfo(winapi_par_HWND(1), winapi_par_int(2), static_cast<LPSCROLLINFO>(winapi_get_ptr(3))));
}

/*
WINUSERAPI LRESULT WINAPI DefFrameProcA(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFFRAMEPROCA )
{
  winapi_ret_LRESULT(DefFrameProcA(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_WPARAM(4), winapi_par_LPARAM(5)));
}

/*
WINUSERAPI LRESULT WINAPI DefFrameProcW(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFFRAMEPROCW )
{
  winapi_ret_LRESULT(DefFrameProcW(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_UINT(3), winapi_par_WPARAM(4), winapi_par_LPARAM(5)));
}

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcA(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFMDICHILDPROCA )
{
  winapi_ret_LRESULT(DefMDIChildProcA(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcW(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
HB_FUNC( WINAPI_DEFMDICHILDPROCW )
{
  winapi_ret_LRESULT(DefMDIChildProcW(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_WPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI TranslateMDISysAccel(HWND hWndClient,LPMSG lpMsg)
*/
HB_FUNC( WINAPI_TRANSLATEMDISYSACCEL )
{
  winapi_ret_BOOL(TranslateMDISysAccel(winapi_par_HWND(1), static_cast<LPMSG>(winapi_get_ptr(2))));
}

/*
WINUSERAPI UINT WINAPI ArrangeIconicWindows(HWND hWnd)
*/
HB_FUNC( WINAPI_ARRANGEICONICWINDOWS )
{
  winapi_ret_UINT(ArrangeIconicWindows(winapi_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI CreateMDIWindowA(LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
HB_FUNC( WINAPI_CREATEMDIWINDOWA )
{
  winapi_ret_HWND(CreateMDIWindowA(( LPCSTR ) hb_parc(1), ( LPCSTR ) hb_parc(2), winapi_par_DWORD(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_HWND(8), winapi_par_HINSTANCE(9), winapi_par_LPARAM(10)));
}

/*
WINUSERAPI HWND WINAPI CreateMDIWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
HB_FUNC( WINAPI_CREATEMDIWINDOWW )
{
  winapi_ret_HWND(CreateMDIWindowW(( LPCWSTR ) hb_parc(1), ( LPCWSTR ) hb_parc(2), winapi_par_DWORD(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_HWND(8), winapi_par_HINSTANCE(9), winapi_par_LPARAM(10)));
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
  winapi_ret_BOOL(WinHelpA(winapi_par_HWND(1), ( LPCSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_ULONG_PTR(4)));
}

/*
WINUSERAPI WINBOOL WINAPI WinHelpW(HWND hWndMain,LPCWSTR lpszHelp,UINT uCommand,ULONG_PTR dwData)
*/
HB_FUNC( WINAPI_WINHELPW )
{
  winapi_ret_BOOL(WinHelpW(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_ULONG_PTR(4)));
}

/*
WINUSERAPI DWORD WINAPI GetGuiResources(HANDLE hProcess,DWORD uiFlags)
*/
HB_FUNC( WINAPI_GETGUIRESOURCES )
{
  winapi_ret_DWORD(GetGuiResources(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
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
  winapi_ret_BOOL(SystemParametersInfoA(winapi_par_UINT(1), winapi_par_UINT(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SystemParametersInfoW(UINT uiAction,UINT uiParam,PVOID pvParam,UINT fWinIni)
*/
HB_FUNC( WINAPI_SYSTEMPARAMETERSINFOW )
{
  winapi_ret_BOOL(SystemParametersInfoW(winapi_par_UINT(1), winapi_par_UINT(2), static_cast<PVOID>(hb_parptr(3)), winapi_par_UINT(4)));
}

/*
WINUSERAPI VOID WINAPI SetDebugErrorLevel (DWORD dwLevel)
*/
HB_FUNC( WINAPI_SETDEBUGERRORLEVEL )
{
  SetDebugErrorLevel(winapi_par_DWORD(1));
}

/*
WINUSERAPI VOID WINAPI SetLastErrorEx (DWORD dwErrCode, DWORD dwType)
*/
HB_FUNC( WINAPI_SETLASTERROREX )
{
  SetLastErrorEx(winapi_par_DWORD(1), winapi_par_DWORD(2));
}

/*
WINUSERAPI int WINAPI InternalGetWindowText (HWND hWnd, LPWSTR pString, int cchMaxCount)
*/
HB_FUNC( WINAPI_INTERNALGETWINDOWTEXT )
{
  winapi_ret_int(InternalGetWindowText(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_int(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CancelShutdown (VOID)
*/
HB_FUNC( WINAPI_CANCELSHUTDOWN )
{
  winapi_ret_BOOL(CancelShutdown());
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
  NotifyWinEvent(winapi_par_DWORD(1), winapi_par_HWND(2), winapi_par_LONG(3), winapi_par_LONG(4));
}

/*
WINUSERAPI HWINEVENTHOOK WINAPI SetWinEventHook(DWORD eventMin,DWORD eventMax,HMODULE hmodWinEventProc,WINEVENTPROC pfnWinEventProc,DWORD idProcess,DWORD idThread,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI IsWinEventHookInstalled(DWORD event)
*/
HB_FUNC( WINAPI_ISWINEVENTHOOKINSTALLED )
{
  winapi_ret_BOOL(IsWinEventHookInstalled(winapi_par_DWORD(1)));
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
  winapi_ret_BOOL(BlockInput(winapi_par_BOOL(1)));
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameA(HWND hwnd,LPSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WINAPI_GETWINDOWMODULEFILENAMEA )
{
  winapi_ret_UINT(GetWindowModuleFileNameA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameW(HWND hwnd,LPWSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WINAPI_GETWINDOWMODULEFILENAMEW )
{
  winapi_ret_UINT(GetWindowModuleFileNameW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
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
  winapi_ret_HWND(GetAncestor(winapi_par_HWND(1), winapi_par_UINT(2)));
}

/*
WINUSERAPI HWND WINAPI RealChildWindowFromPoint(HWND hwndParent,POINT ptParentClientCoords)
*/

/*
WINUSERAPI UINT WINAPI RealGetWindowClassA(HWND hwnd,LPSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WINAPI_REALGETWINDOWCLASSA )
{
  winapi_ret_UINT(RealGetWindowClassA(winapi_par_HWND(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINUSERAPI UINT WINAPI RealGetWindowClassW(HWND hwnd,LPWSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WINAPI_REALGETWINDOWCLASSW )
{
  winapi_ret_UINT(RealGetWindowClassW(winapi_par_HWND(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
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
  winapi_ret_DWORD(GetListBoxInfo(winapi_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LockWorkStation(VOID)
*/
HB_FUNC( WINAPI_LOCKWORKSTATION )
{
  winapi_ret_BOOL(LockWorkStation());
}

/*
WINUSERAPI WINBOOL WINAPI UserHandleGrantAccess(HANDLE hUserHandle,HANDLE hJob,WINBOOL bGrant)
*/
HB_FUNC( WINAPI_USERHANDLEGRANTACCESS )
{
  winapi_ret_BOOL(UserHandleGrantAccess(winapi_par_HANDLE(1), winapi_par_HANDLE(2), winapi_par_BOOL(3)));
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
  winapi_ret_BOOL(ShutdownBlockReasonCreate(winapi_par_HWND(1), ( LPCWSTR ) hb_parc(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonQuery (HWND hWnd, LPWSTR pwszBuff, DWORD *pcchBuff)
*/

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonDestroy (HWND hWnd)
*/
HB_FUNC( WINAPI_SHUTDOWNBLOCKREASONDESTROY )
{
  winapi_ret_BOOL(ShutdownBlockReasonDestroy(winapi_par_HWND(1)));
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

/*
int GetSystemMetrics([in] int nIndex);
*/
HB_FUNC( WINAPI_GETSYSTEMMETRICS )
{
  winapi_ret_int(GetSystemMetrics(winapi_par_int(1)));
}
