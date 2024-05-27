/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

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

/*
  NOTE: source code generated with the help of a code generator
*/

#define WINVER 0x0600
#define _WIN32_WINNT 0x0600

#include <windows.h>
#include <vector>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

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
#if 0
HB_FUNC( WALOADKEYBOARDLAYOUTA )
{
  wa_ret_HKL(LoadKeyboardLayoutA(wa_par_LPCSTR(1), wa_par_UINT(2)));
}
#endif

/*
WINUSERAPI HKL WINAPI LoadKeyboardLayoutW(LPCWSTR pwszKLID,UINT Flags)
*/
#if 0
HB_FUNC( WALOADKEYBOARDLAYOUTW )
{
  wa_ret_HKL(LoadKeyboardLayoutW(wa_par_LPCWSTR(1), wa_par_UINT(2)));
}
#endif

HB_FUNC( WALOADKEYBOARDLAYOUT )
{
  void * str{};
  wa_ret_HKL(LoadKeyboardLayout(HB_PARSTR(1, &str, nullptr), wa_par_UINT(2)));
  hb_strfree(str);
}

/*
WINUSERAPI HKL WINAPI ActivateKeyboardLayout(HKL hkl,UINT Flags)
*/
HB_FUNC( WAACTIVATEKEYBOARDLAYOUT )
{
  wa_ret_HKL(ActivateKeyboardLayout(wa_par_HKL(1), wa_par_UINT(2)));
}

/*
WINUSERAPI int WINAPI ToUnicodeEx(UINT wVirtKey,UINT wScanCode,CONST BYTE *lpKeyState,LPWSTR pwszBuff,int cchBuff,UINT wFlags,HKL dwhkl)
*/

/*
WINUSERAPI WINBOOL WINAPI UnloadKeyboardLayout(HKL hkl)
*/
HB_FUNC( WAUNLOADKEYBOARDLAYOUT )
{
  wa_ret_BOOL(UnloadKeyboardLayout(wa_par_HKL(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameA(LPSTR pwszKLID)
*/
HB_FUNC( WAGETKEYBOARDLAYOUTNAMEA ) // TODO: fix
{
  wa_ret_BOOL(GetKeyboardLayoutNameA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINUSERAPI WINBOOL WINAPI GetKeyboardLayoutNameW(LPWSTR pwszKLID)
*/
HB_FUNC( WAGETKEYBOARDLAYOUTNAMEW ) // TODO: fix
{
  wa_ret_BOOL(GetKeyboardLayoutNameW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1)))));
}

/*
WINUSERAPI int WINAPI GetKeyboardLayoutList(int nBuff,HKL *lpList)
*/

/*
WINUSERAPI HKL WINAPI GetKeyboardLayout(DWORD idThread)
*/
HB_FUNC( WAGETKEYBOARDLAYOUT )
{
  wa_ret_HKL(GetKeyboardLayout(wa_par_DWORD(1)));
}

/*
WINUSERAPI int WINAPI GetMouseMovePointsEx(UINT cbSize,LPMOUSEMOVEPOINT lppt,LPMOUSEMOVEPOINT lpptBuf,int nBufPoints,DWORD resolution)
*/
HB_FUNC( WAGETMOUSEMOVEPOINTSEX )
{
  wa_ret_int(GetMouseMovePointsEx(wa_par_UINT(1), wa_par_MOUSEMOVEPOINT(2), wa_par_MOUSEMOVEPOINT(3), wa_par_int(4), wa_par_DWORD(5)));
}

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
#if 0
HB_FUNC( WAENUMDESKTOPSA )
{
  wa_ret_BOOL(EnumDesktopsA(wa_par_HWINSTA(1), wa_par_DESKTOPENUMPROCA(2), wa_par_LPARAM(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI EnumDesktopsW(HWINSTA hwinsta,DESKTOPENUMPROCW lpEnumFunc,LPARAM lParam)
*/
#if 0
HB_FUNC( WAENUMDESKTOPSW )
{
  wa_ret_BOOL(EnumDesktopsW(wa_par_HWINSTA(1), wa_par_DESKTOPENUMPROCW(2), wa_par_LPARAM(3)));
}
#endif

HB_FUNC( WAENUMDESKTOPS )
{
  wa_ret_BOOL(EnumDesktops(wa_par_HWINSTA(1), wa_par_DESKTOPENUMPROC(2), wa_par_LPARAM(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumDesktopWindows(HDESK hDesktop,WNDENUMPROC lpfn,LPARAM lParam)
*/
HB_FUNC( WAENUMDESKTOPWINDOWS )
{
  wa_ret_BOOL(EnumDesktopWindows(wa_par_HDESK(1), wa_par_WNDENUMPROC(2), wa_par_LPARAM(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SwitchDesktop(HDESK hDesktop)
*/
HB_FUNC( WASWITCHDESKTOP )
{
  wa_ret_BOOL(SwitchDesktop(wa_par_HDESK(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetThreadDesktop(HDESK hDesktop)
*/
HB_FUNC( WASETTHREADDESKTOP )
{
  wa_ret_BOOL(SetThreadDesktop(wa_par_HDESK(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseDesktop(HDESK hDesktop)
*/
HB_FUNC( WACLOSEDESKTOP )
{
  wa_ret_BOOL(CloseDesktop(wa_par_HDESK(1)));
}

/*
WINUSERAPI HDESK WINAPI GetThreadDesktop(DWORD dwThreadId)
*/
HB_FUNC( WAGETTHREADDESKTOP )
{
  wa_ret_HDESK(GetThreadDesktop(wa_par_DWORD(1)));
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
#if 0
HB_FUNC( WAENUMWINDOWSTATIONSA )
{
  wa_ret_BOOL(EnumWindowStationsA(wa_par_WINSTAENUMPROCA(1), wa_par_LPARAM(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI EnumWindowStationsW(WINSTAENUMPROCW lpEnumFunc,LPARAM lParam)
*/
#if 0
HB_FUNC( WAENUMWINDOWSTATIONSW )
{
  wa_ret_BOOL(EnumWindowStationsW(wa_par_WINSTAENUMPROCW(1), wa_par_LPARAM(2)));
}
#endif

HB_FUNC( WAENUMWINDOWSTATIONS )
{
  wa_ret_BOOL(EnumWindowStations(wa_par_WINSTAENUMPROC(1), wa_par_LPARAM(2)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseWindowStation(HWINSTA hWinSta)
*/
HB_FUNC( WACLOSEWINDOWSTATION )
{
  wa_ret_BOOL(CloseWindowStation(wa_par_HWINSTA(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessWindowStation(HWINSTA hWinSta)
*/
HB_FUNC( WASETPROCESSWINDOWSTATION )
{
  wa_ret_BOOL(SetProcessWindowStation(wa_par_HWINSTA(1)));
}

/*
WINUSERAPI HWINSTA WINAPI GetProcessWindowStation(VOID)
*/
HB_FUNC( WAGETPROCESSWINDOWSTATION )
{
  wa_ret_HWINSTA(GetProcessWindowStation());
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
HB_FUNC( WAGETUSEROBJECTINFORMATIONA )
{
  DWORD nLengthNeeded{};
  wa_ret_BOOL(GetUserObjectInformationA(wa_par_HANDLE(1), wa_par_int(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4), &nLengthNeeded));
  wa_stor_DWORD(nLengthNeeded, 5);
}

/*
WINUSERAPI WINBOOL WINAPI GetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength,LPDWORD lpnLengthNeeded)
*/
HB_FUNC( WAGETUSEROBJECTINFORMATIONW )
{
  DWORD nLengthNeeded{};
  wa_ret_BOOL(GetUserObjectInformationW(wa_par_HANDLE(1), wa_par_int(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4), &nLengthNeeded));
  wa_stor_DWORD(nLengthNeeded, 5);
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationA(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WASETUSEROBJECTINFORMATIONA )
{
  wa_ret_BOOL(SetUserObjectInformationA(wa_par_HANDLE(1), wa_par_int(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SetUserObjectInformationW(HANDLE hObj,int nIndex,PVOID pvInfo,DWORD nLength)
*/
HB_FUNC( WASETUSEROBJECTINFORMATIONW )
{
  wa_ret_BOOL(SetUserObjectInformationW(wa_par_HANDLE(1), wa_par_int(2), static_cast<PVOID>(hb_parptr(3)), wa_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsHungAppWindow(HWND hwnd)
*/
HB_FUNC( WAISHUNGAPPWINDOW )
{
  wa_ret_BOOL(IsHungAppWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI VOID WINAPI DisableProcessWindowsGhosting(VOID)
*/
HB_FUNC( WADISABLEPROCESSWINDOWSGHOSTING )
{
  DisableProcessWindowsGhosting();
}

/*
WINUSERAPI UINT WINAPI RegisterWindowMessageA(LPCSTR lpString)
*/
#if 0
HB_FUNC( WAREGISTERWINDOWMESSAGEA )
{
  wa_ret_UINT(RegisterWindowMessageA(wa_par_LPCSTR(1)));
}
#endif

/*
WINUSERAPI UINT WINAPI RegisterWindowMessageW(LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAREGISTERWINDOWMESSAGEW )
{
  wa_ret_UINT(RegisterWindowMessageW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAREGISTERWINDOWMESSAGE )
{
  void * str1{};
  wa_ret_UINT(RegisterWindowMessage(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI WINBOOL WINAPI TrackMouseEvent(LPTRACKMOUSEEVENT lpEventTrack)
*/
HB_FUNC( WATRACKMOUSEEVENT )
{
  wa_ret_BOOL(TrackMouseEvent(wa_par_TRACKMOUSEEVENT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawEdge(HDC hdc,LPRECT qrc,UINT edge,UINT grfFlags)
*/
HB_FUNC( WADRAWEDGE )
{
  wa_ret_BOOL(DrawEdge(wa_par_HDC(1), wa_par_RECT(2), wa_par_UINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawFrameControl(HDC,LPRECT,UINT,UINT)
*/
HB_FUNC( WADRAWFRAMECONTROL )
{
  wa_ret_BOOL(DrawFrameControl(wa_par_HDC(1), wa_par_RECT(2), wa_par_UINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawCaption(HWND hwnd,HDC hdc,CONST RECT *lprect,UINT flags)
*/
HB_FUNC( WADRAWCAPTION )
{
  wa_ret_BOOL(DrawCaption(wa_par_HWND(1), wa_par_HDC(2), wa_par_RECT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawAnimatedRects(HWND hwnd,int idAni,CONST RECT *lprcFrom,CONST RECT *lprcTo)
*/
HB_FUNC( WADRAWANIMATEDRECTS )
{
  wa_ret_BOOL(DrawAnimatedRects(wa_par_HWND(1), wa_par_int(2), wa_par_RECT(3), wa_par_RECT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI GetMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/
#if 0
HB_FUNC( WAGETMESSAGEA )
{
  wa_ret_BOOL(GetMessageA(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax)
*/
#if 0
HB_FUNC( WAGETMESSAGEW )
{
  wa_ret_BOOL(GetMessageW(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4)));
}
#endif

HB_FUNC( WAGETMESSAGE )
{
  wa_ret_BOOL(GetMessage(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI TranslateMessage(CONST MSG *lpMsg)
*/
HB_FUNC( WATRANSLATEMESSAGE )
{
  wa_ret_BOOL(TranslateMessage(wa_par_MSG(1)));
}

/*
WINUSERAPI LRESULT WINAPI DispatchMessageA(CONST MSG *lpMsg)
*/
#if 0
HB_FUNC( WADISPATCHMESSAGEA )
{
  wa_ret_LRESULT(DispatchMessageA(wa_par_MSG(1)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI DispatchMessageW(CONST MSG *lpMsg)
*/
#if 0
HB_FUNC( WADISPATCHMESSAGEW )
{
  wa_ret_LRESULT(DispatchMessageW(wa_par_MSG(1)));
}
#endif

HB_FUNC( WADISPATCHMESSAGE )
{
  wa_ret_LRESULT(DispatchMessage(wa_par_MSG(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMessageQueue(int cMessagesMax)
*/
HB_FUNC( WASETMESSAGEQUEUE )
{
  wa_ret_BOOL(SetMessageQueue(wa_par_int(1)));
}

/*
WINUSERAPI WINBOOL WINAPI PeekMessageA(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/
#if 0
HB_FUNC( WAPEEKMESSAGEA )
{
  wa_ret_BOOL(PeekMessageA(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4), wa_par_UINT(5)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI PeekMessageW(LPMSG lpMsg,HWND hWnd,UINT wMsgFilterMin,UINT wMsgFilterMax,UINT wRemoveMsg)
*/
#if 0
HB_FUNC( WAPEEKMESSAGEW )
{
  wa_ret_BOOL(PeekMessageW(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4), wa_par_UINT(5)));
}
#endif

HB_FUNC( WAPEEKMESSAGE )
{
  wa_ret_BOOL(PeekMessage(wa_par_MSG(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_UINT(4), wa_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI RegisterHotKey(HWND hWnd,int id,UINT fsModifiers,UINT vk)
*/
HB_FUNC( WAREGISTERHOTKEY )
{
  wa_ret_BOOL(RegisterHotKey(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterHotKey(HWND hWnd,int id)
*/
HB_FUNC( WAUNREGISTERHOTKEY )
{
  wa_ret_BOOL(UnregisterHotKey(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ExitWindowsEx(UINT uFlags,DWORD dwReason)
*/
HB_FUNC( WAEXITWINDOWSEX )
{
  wa_ret_BOOL(ExitWindowsEx(wa_par_UINT(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SwapMouseButton(WINBOOL fSwap)
*/
HB_FUNC( WASWAPMOUSEBUTTON )
{
  wa_ret_BOOL(SwapMouseButton(wa_par_BOOL(1)));
}

/*
WINUSERAPI DWORD WINAPI GetMessagePos(VOID)
*/
HB_FUNC( WAGETMESSAGEPOS )
{
  wa_ret_DWORD(GetMessagePos());
}

/*
WINUSERAPI LONG WINAPI GetMessageTime(VOID)
*/
HB_FUNC( WAGETMESSAGETIME )
{
  wa_ret_LONG(GetMessageTime());
}

/*
WINUSERAPI LPARAM WINAPI GetMessageExtraInfo(VOID)
*/
HB_FUNC( WAGETMESSAGEEXTRAINFO )
{
  wa_ret_LPARAM(GetMessageExtraInfo());
}

/*
WINUSERAPI DWORD WINAPI GetUnpredictedMessagePos(VOID)
*/
#if 0
HB_FUNC( WAGETUNPREDICTEDMESSAGEPOS )
{
  wa_ret_DWORD(GetUnpredictedMessagePos());
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsWow64Message(VOID)
*/
HB_FUNC( WAISWOW64MESSAGE )
{
  wa_ret_BOOL(IsWow64Message());
}

/*
WINUSERAPI LPARAM WINAPI SetMessageExtraInfo(LPARAM lParam)
*/
HB_FUNC( WASETMESSAGEEXTRAINFO )
{
  wa_ret_LPARAM(SetMessageExtraInfo(wa_par_LPARAM(1)));
}

/*
WINUSERAPI LRESULT WINAPI SendMessageA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WASENDMESSAGEA )
{
  wa_ret_LRESULT(SendMessageA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI SendMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WASENDMESSAGEW )
{
  wa_ret_LRESULT(SendMessageW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WASENDMESSAGE )
{
  void * str{};
  wa_ret_LRESULT(SendMessage(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), HB_ISCHAR(4) ? reinterpret_cast<LPARAM>(HB_PARSTR(4, &str, nullptr)) : wa_par_LPARAM(4)));
  hb_strfree(str);
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
#if 0
HB_FUNC( WASENDNOTIFYMESSAGEA )
{
  wa_ret_BOOL(SendNotifyMessageA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SendNotifyMessageW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WASENDNOTIFYMESSAGEW )
{
  wa_ret_BOOL(SendNotifyMessageW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WASENDNOTIFYMESSAGE )
{
  wa_ret_BOOL(SendNotifyMessage(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SendMessageCallbackA(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,SENDASYNCPROC lpResultCallBack,ULONG_PTR dwData)
*/
#if 0
HB_FUNC( WASENDMESSAGECALLBACKA )
{
  wa_ret_BOOL(SendMessageCallbackA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4), wa_par_SENDASYNCPROC(5), wa_par_ULONG_PTR(6)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SendMessageCallbackW(HWND hWnd,UINT Msg,WPARAM wParam,LPARAM lParam,SENDASYNCPROC lpResultCallBack,ULONG_PTR dwData)
*/
#if 0
HB_FUNC( WASENDMESSAGECALLBACKW )
{
  wa_ret_BOOL(SendMessageCallbackW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4), wa_par_SENDASYNCPROC(5), wa_par_ULONG_PTR(6)));
}
#endif

HB_FUNC( WASENDMESSAGECALLBACK )
{
  wa_ret_BOOL(SendMessageCallback(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4), wa_par_SENDASYNCPROC(5), wa_par_ULONG_PTR(6)));
}

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
HB_FUNC( WAUNREGISTERPOWERSETTINGNOTIFICATION )
{
  wa_ret_BOOL(UnregisterPowerSettingNotification(wa_par_HPOWERNOTIFY(1)));
}

/*
WINUSERAPI HPOWERNOTIFY WINAPI RegisterSuspendResumeNotification(HANDLE hRecipient, DWORD Flags)
*/
HB_FUNC( WAREGISTERSUSPENDRESUMENOTIFICATION )
{
  wa_ret_HPOWERNOTIFY(RegisterSuspendResumeNotification(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterSuspendResumeNotification(HPOWERNOTIFY Handle)
*/
HB_FUNC( WAUNREGISTERSUSPENDRESUMENOTIFICATION )
{
  wa_ret_BOOL(UnregisterSuspendResumeNotification(wa_par_HPOWERNOTIFY(1)));
}

/*
WINUSERAPI WINBOOL WINAPI PostMessageA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WAPOSTMESSAGEA )
{
  wa_ret_BOOL(PostMessageA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI PostMessageW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WAPOSTMESSAGEW )
{
  wa_ret_BOOL(PostMessageW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WAPOSTMESSAGE )
{
  wa_ret_BOOL(PostMessage(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageA(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WAPOSTTHREADMESSAGEA )
{
  wa_ret_BOOL(PostThreadMessageA(wa_par_DWORD(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI PostThreadMessageW(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WAPOSTTHREADMESSAGEW )
{
  wa_ret_BOOL(PostThreadMessageW(wa_par_DWORD(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WAPOSTTHREADMESSAGE )
{
  wa_ret_BOOL(PostThreadMessage(wa_par_DWORD(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI AttachThreadInput(DWORD idAttach, DWORD idAttachTo, WINBOOL fAttach)
*/
HB_FUNC( WAATTACHTHREADINPUT )
{
  wa_ret_BOOL(AttachThreadInput(wa_par_DWORD(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI ReplyMessage(LRESULT lResult)
*/
HB_FUNC( WAREPLYMESSAGE )
{
  wa_ret_BOOL(ReplyMessage(wa_par_LRESULT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI WaitMessage(VOID)
*/
HB_FUNC( WAWAITMESSAGE )
{
  wa_ret_BOOL(WaitMessage());
}

/*
WINUSERAPI DWORD WINAPI WaitForInputIdle(HANDLE hProcess, DWORD dwMilliseconds)
*/
HB_FUNC( WAWAITFORINPUTIDLE )
{
  wa_ret_DWORD(WaitForInputIdle(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI LRESULT WINAPI DefWindowProcA(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFWINDOWPROCA )
{
  wa_ret_LRESULT(DefWindowProcA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI DefWindowProcW(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFWINDOWPROCW )
{
  wa_ret_LRESULT(DefWindowProcW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WADEFWINDOWPROC )
{
  wa_ret_LRESULT(DefWindowProc(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI VOID WINAPI PostQuitMessage(int nExitCode)
*/
HB_FUNC( WAPOSTQUITMESSAGE )
{
  PostQuitMessage(wa_par_int(1));
}

/*
WINUSERAPI WINBOOL WINAPI InSendMessage(VOID)
*/
HB_FUNC( WAINSENDMESSAGE )
{
  wa_ret_BOOL(InSendMessage());
}

/*
WINUSERAPI DWORD WINAPI InSendMessageEx(LPVOID lpReserved)
*/
HB_FUNC( WAINSENDMESSAGEEX )
{
  wa_ret_DWORD(InSendMessageEx(static_cast<LPVOID>(hb_parptr(1))));
}

/*
WINUSERAPI UINT WINAPI GetDoubleClickTime(VOID)
*/
HB_FUNC( WAGETDOUBLECLICKTIME )
{
  wa_ret_UINT(GetDoubleClickTime());
}

/*
WINUSERAPI WINBOOL WINAPI SetDoubleClickTime(UINT)
*/
HB_FUNC( WASETDOUBLECLICKTIME )
{
  wa_ret_BOOL(SetDoubleClickTime(wa_par_UINT(1)));
}

/*
WINUSERAPI ATOM WINAPI RegisterClassA(CONST WNDCLASSA *lpWndClass)
*/
#if 0
HB_FUNC( WAREGISTERCLASSA )
{
  wa_ret_ATOM(RegisterClassA(wa_par_WNDCLASSA(1)));
}
#endif

/*
WINUSERAPI ATOM WINAPI RegisterClassW(CONST WNDCLASSW *lpWndClass)
*/
#if 0
HB_FUNC( WAREGISTERCLASSW )
{
  wa_ret_ATOM(RegisterClassW(wa_par_WNDCLASSW(1)));
}
#endif

HB_FUNC( WAREGISTERCLASS )
{
  wa_ret_ATOM(RegisterClass(wa_par_WNDCLASS(1)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterClassA(LPCSTR lpClassName, HINSTANCE hInstance)
*/
#if 0
HB_FUNC( WAUNREGISTERCLASSA )
{
  wa_ret_BOOL(UnregisterClassA(wa_par_LPCSTR(1), wa_par_HINSTANCE(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI UnregisterClassW(LPCWSTR lpClassName, HINSTANCE hInstance)
*/
#if 0
HB_FUNC( WAUNREGISTERCLASSW )
{
  wa_ret_BOOL(UnregisterClassW(wa_par_LPCWSTR(1), wa_par_HINSTANCE(2)));
}
#endif

HB_FUNC( WAUNREGISTERCLASS )
{
  void * str{};
  wa_ret_BOOL(UnregisterClass(HB_PARSTR(1, &str, nullptr), wa_par_HINSTANCE(2)));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoA(HINSTANCE hInstance, LPCSTR lpClassName, LPWNDCLASSA lpWndClass)
*/
#if 0
HB_FUNC( WAGETCLASSINFOA )
{
  wa_ret_BOOL(GetClassInfoA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_WNDCLASSA(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoW(HINSTANCE hInstance, LPCWSTR lpClassName, LPWNDCLASSW lpWndClass)
*/
#if 0
HB_FUNC( WAGETCLASSINFOW )
{
  wa_ret_BOOL(GetClassInfoW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_WNDCLASSW(3)));
}
#endif

HB_FUNC( WAGETCLASSINFO )
{
  void * str{};
  wa_ret_BOOL(GetClassInfoW(wa_par_HINSTANCE(1), HB_PARSTR(2, &str, nullptr), wa_par_WNDCLASS(3)));
  hb_strfree(str);
}

/*
WINUSERAPI ATOM WINAPI RegisterClassExA(CONST WNDCLASSEXA *)
*/
#if 0
HB_FUNC( WAREGISTERCLASSEXA )
{
  wa_ret_ATOM(RegisterClassExA(wa_par_WNDCLASSEXA(1)));
}
#endif

/*
WINUSERAPI ATOM WINAPI RegisterClassExW(CONST WNDCLASSEXW *)
*/
#if 0
HB_FUNC( WAREGISTERCLASSEXW )
{
  wa_ret_ATOM(RegisterClassExW(wa_par_WNDCLASSEXW(1)));
}
#endif

HB_FUNC( WAREGISTERCLASSEX )
{
  wa_ret_ATOM(RegisterClassEx(wa_par_WNDCLASSEX(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoExA(HINSTANCE hInstance, LPCSTR lpszClass, LPWNDCLASSEXA lpwcx)
*/
#if 0
HB_FUNC( WAGETCLASSINFOEXA )
{
  wa_ret_BOOL(GetClassInfoExA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_WNDCLASSEXA(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetClassInfoExW(HINSTANCE hInstance, LPCWSTR lpszClass, LPWNDCLASSEXW lpwcx)
*/
#if 0
HB_FUNC( WAGETCLASSINFOEXW )
{
  wa_ret_BOOL(GetClassInfoExW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_WNDCLASSEXW(3)));
}
#endif

HB_FUNC( WAGETCLASSINFOEX )
{
  void * str{};
  wa_ret_BOOL(GetClassInfoEx(wa_par_HINSTANCE(1), HB_PARSTR(2, &str, nullptr), wa_par_WNDCLASSEX(3)));
  hb_strfree(str);
}

/*
WINUSERAPI LRESULT WINAPI CallWindowProcA (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WACALLWINDOWPROCA )
{
  wa_ret_LRESULT(CallWindowProcA(wa_par_WNDPROC(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI CallWindowProcW (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/
#if 0
HB_FUNC( WACALLWINDOWPROCW )
{
  wa_ret_LRESULT(CallWindowProcW(wa_par_WNDPROC(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WACALLWINDOWPROC )
{
  wa_ret_LRESULT(CallWindowProc(wa_par_WNDPROC(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}

/*
WINUSERAPI LRESULT WINAPI CallWindowProcA (FARPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI LRESULT WINAPI CallWindowProcW (FARPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
*/

/*
WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationA(HANDLE hRecipient,LPVOID NotificationFilter,DWORD Flags)
*/
#if 0
HB_FUNC( WAREGISTERDEVICENOTIFICATIONA )
{
  wa_ret_HDEVNOTIFY(RegisterDeviceNotificationA(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3)));
}
#endif

/*
WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationW(HANDLE hRecipient,LPVOID NotificationFilter,DWORD Flags)
*/
#if 0
HB_FUNC( WAREGISTERDEVICENOTIFICATIONW )
{
  wa_ret_HDEVNOTIFY(RegisterDeviceNotificationW(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3)));
}
#endif

HB_FUNC( WAREGISTERDEVICENOTIFICATION )
{
  wa_ret_HDEVNOTIFY(RegisterDeviceNotification(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI UnregisterDeviceNotification(HDEVNOTIFY Handle)
*/
HB_FUNC( WAUNREGISTERDEVICENOTIFICATION )
{
  wa_ret_BOOL(UnregisterDeviceNotification(wa_par_HDEVNOTIFY(1)));
}

/*
WINUSERAPI HWND WINAPI CreateWindowExA(DWORD dwExStyle,LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
#if 0
HB_FUNC( WACREATEWINDOWEXA )
{
  wa_ret_HWND(CreateWindowExA(wa_par_DWORD(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_DWORD(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_HWND(9), wa_par_HMENU(10), wa_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
}
#endif

/*
WINUSERAPI HWND WINAPI CreateWindowExW(DWORD dwExStyle,LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HMENU hMenu,HINSTANCE hInstance,LPVOID lpParam)
*/
#if 0
HB_FUNC( WACREATEWINDOWEXW )
{
  wa_ret_HWND(CreateWindowExW(wa_par_DWORD(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_DWORD(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_HWND(9), wa_par_HMENU(10), wa_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
}
#endif

HB_FUNC( WACREATEWINDOWEX )
{
  void * str2{};
  void * str3{};
  wa_ret_HWND(CreateWindowEx(wa_par_DWORD(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_DWORD(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_int(8), wa_par_HWND(9), wa_par_HMENU(10), wa_par_HINSTANCE(11), static_cast<LPVOID>(hb_parptr(12))));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINUSERAPI WINBOOL WINAPI IsWindow(HWND hWnd)
*/
HB_FUNC( WAISWINDOW )
{
  wa_ret_BOOL(IsWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsMenu(HMENU hMenu)
*/
HB_FUNC( WAISMENU )
{
  wa_ret_BOOL(IsMenu(wa_par_HMENU(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsChild(HWND hWndParent,HWND hWnd)
*/
HB_FUNC( WAISCHILD )
{
  wa_ret_BOOL(IsChild(wa_par_HWND(1), wa_par_HWND(2)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyWindow(HWND hWnd)
*/
HB_FUNC( WADESTROYWINDOW )
{
  wa_ret_BOOL(DestroyWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindow(HWND hWnd,int nCmdShow)
*/
HB_FUNC( WASHOWWINDOW )
{
  wa_ret_BOOL(ShowWindow(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI AnimateWindow(HWND hWnd,DWORD dwTime,DWORD dwFlags)
*/
HB_FUNC( WAANIMATEWINDOW )
{
  wa_ret_BOOL(AnimateWindow(wa_par_HWND(1), wa_par_DWORD(2), wa_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI UpdateLayeredWindow(HWND hWnd, HDC hdcDst, POINT *pptDst, SIZE *psize, HDC hdcSrc, POINT *pptSrc, COLORREF crKey, BLENDFUNCTION *pblend, DWORD dwFlags)
*/
HB_FUNC( WAUPDATELAYEREDWINDOW )
{
  wa_ret_BOOL(UpdateLayeredWindow(wa_par_HWND(1), wa_par_HDC(2), wa_par_POINT(3), wa_par_SIZE(4), wa_par_HDC(5), wa_par_POINT(6), wa_par_COLORREF(7), wa_par_BLENDFUNCTION(8), wa_par_DWORD(9)));
}

/*
WINUSERAPI WINBOOL WINAPI UpdateLayeredWindowIndirect (HWND hWnd, const UPDATELAYEREDWINDOWINFO *pULWInfo)
*/
HB_FUNC( WAUPDATELAYEREDWINDOWINDIRECT )
{
  wa_ret_BOOL(UpdateLayeredWindowIndirect(wa_par_HWND(1), wa_par_UPDATELAYEREDWINDOWINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetLayeredWindowAttributes (HWND hwnd, COLORREF *pcrKey, BYTE *pbAlpha, DWORD *pdwFlags)
*/
HB_FUNC( WAGETLAYEREDWINDOWATTRIBUTES )
{
  COLORREF crKey{};
  BYTE bAlpha{};
  DWORD dwFlags{};
  wa_ret_BOOL(GetLayeredWindowAttributes(wa_par_HWND(1), &crKey, &bAlpha, &dwFlags));
  wa_stor_COLORREF(crKey, 2);
  wa_stor_BYTE(bAlpha, 3);
  wa_stor_DWORD(dwFlags, 4);
}

/*
WINUSERAPI WINBOOL WINAPI PrintWindow (HWND hwnd, HDC hdcBlt, UINT nFlags)
*/
HB_FUNC( WAPRINTWINDOW )
{
  wa_ret_BOOL(PrintWindow(wa_par_HWND(1), wa_par_HDC(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetLayeredWindowAttributes (HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags)
*/
HB_FUNC( WASETLAYEREDWINDOWATTRIBUTES )
{
  wa_ret_BOOL(SetLayeredWindowAttributes(wa_par_HWND(1), wa_par_COLORREF(2), wa_par_BYTE(3), wa_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowWindowAsync (HWND hWnd, int nCmdShow)
*/
HB_FUNC( WASHOWWINDOWASYNC )
{
  wa_ret_BOOL(ShowWindowAsync(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindow (HWND hWnd, WINBOOL bInvert)
*/
HB_FUNC( WAFLASHWINDOW )
{
  wa_ret_BOOL(FlashWindow(wa_par_HWND(1), wa_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI FlashWindowEx (PFLASHWINFO pfwi)
*/
HB_FUNC( WAFLASHWINDOWEX )
{
  wa_ret_BOOL(FlashWindowEx(wa_par_FLASHWINFO(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowOwnedPopups (HWND hWnd, WINBOOL fShow)
*/
HB_FUNC( WASHOWOWNEDPOPUPS )
{
  wa_ret_BOOL(ShowOwnedPopups(wa_par_HWND(1), wa_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OpenIcon (HWND hWnd)
*/
HB_FUNC( WAOPENICON )
{
  wa_ret_BOOL(OpenIcon(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseWindow (HWND hWnd)
*/
HB_FUNC( WACLOSEWINDOW )
{
  wa_ret_BOOL(CloseWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI MoveWindow (HWND hWnd, int X, int Y, int nWidth, int nHeight, WINBOOL bRepaint)
*/
HB_FUNC( WAMOVEWINDOW )
{
  wa_ret_BOOL(MoveWindow(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_BOOL(6)));
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowPos (HWND hWnd, HWND hWndInsertAfter, int X, int Y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WASETWINDOWPOS )
{
  wa_ret_BOOL(SetWindowPos(wa_par_HWND(1), wa_par_HWND(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_UINT(7)));
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowPlacement (HWND hWnd, WINDOWPLACEMENT *lpwndpl)
*/
HB_FUNC( WAGETWINDOWPLACEMENT )
{
  wa_ret_BOOL(GetWindowPlacement(wa_par_HWND(1), wa_par_WINDOWPLACEMENT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowPlacement (HWND hWnd, CONST WINDOWPLACEMENT *lpwndpl)
*/
HB_FUNC( WASETWINDOWPLACEMENT )
{
  wa_ret_BOOL(SetWindowPlacement(wa_par_HWND(1), wa_par_WINDOWPLACEMENT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowDisplayAffinity(HWND hWnd, DWORD *pdwAffinity)
*/
#if 0
HB_FUNC( WAGETWINDOWDISPLAYAFFINITY )
{
  DWORD dwAffinity{};
  wa_ret_BOOL(GetWindowDisplayAffinity(wa_par_HWND(1), &dwAffinity));
  wa_stor_DWORD(dwAffinity, 2);
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetWindowDisplayAffinity(HWND hWnd, DWORD dwAffinity)
*/
#if 0
HB_FUNC( WASETWINDOWDISPLAYAFFINITY )
{
  wa_ret_BOOL(SetWindowDisplayAffinity(wa_par_HWND(1), wa_par_DWORD(2)));
}
#endif

/*
WINUSERAPI HDWP WINAPI BeginDeferWindowPos (int nNumWindows)
*/
HB_FUNC( WABEGINDEFERWINDOWPOS )
{
  wa_ret_HDWP(BeginDeferWindowPos(wa_par_int(1)));
}

/*
WINUSERAPI HDWP WINAPI DeferWindowPos (HDWP hWinPosInfo, HWND hWnd, HWND hWndInsertAfter, int x, int y, int cx, int cy, UINT uFlags)
*/
HB_FUNC( WADEFERWINDOWPOS )
{
  wa_ret_HDWP(DeferWindowPos(wa_par_HDWP(1), wa_par_HWND(2), wa_par_HWND(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_UINT(8)));
}

/*
WINUSERAPI WINBOOL WINAPI EndDeferWindowPos (HDWP hWinPosInfo)
*/
HB_FUNC( WAENDDEFERWINDOWPOS )
{
  wa_ret_BOOL(EndDeferWindowPos(wa_par_HDWP(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowVisible (HWND hWnd)
*/
HB_FUNC( WAISWINDOWVISIBLE )
{
  wa_ret_BOOL(IsWindowVisible(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsIconic (HWND hWnd)
*/
HB_FUNC( WAISICONIC )
{
  wa_ret_BOOL(IsIconic(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI AnyPopup (VOID)
*/
HB_FUNC( WAANYPOPUP )
{
  wa_ret_BOOL(AnyPopup());
}

/*
WINUSERAPI WINBOOL WINAPI BringWindowToTop (HWND hWnd)
*/
HB_FUNC( WABRINGWINDOWTOTOP )
{
  wa_ret_BOOL(BringWindowToTop(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsZoomed (HWND hWnd)
*/
HB_FUNC( WAISZOOMED )
{
  wa_ret_BOOL(IsZoomed(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI CreateDialogParamA(HINSTANCE hInstance,LPCSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WACREATEDIALOGPARAMA )
{
  wa_ret_HWND(CreateDialogParamA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI HWND WINAPI CreateDialogParamW(HINSTANCE hInstance,LPCWSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WACREATEDIALOGPARAMW )
{
  wa_ret_HWND(CreateDialogParamW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WACREATEDIALOGPARAM )
{
  void * str{};
  wa_ret_HWND(CreateDialogParam(wa_par_HINSTANCE(1), HB_ISCHAR(2) ? HB_PARSTR(2, &str, nullptr) : MAKEINTRESOURCE(hb_parni(2)), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
  hb_strfree(str);
}

/*
WINUSERAPI HWND WINAPI CreateDialogIndirectParamA(HINSTANCE hInstance,LPCDLGTEMPLATEA lpTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WACREATEDIALOGINDIRECTPARAMA )
{
  wa_ret_HWND(CreateDialogIndirectParamA(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATEA(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI HWND WINAPI CreateDialogIndirectParamW(HINSTANCE hInstance,LPCDLGTEMPLATEW lpTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WACREATEDIALOGINDIRECTPARAMW )
{
  wa_ret_HWND(CreateDialogIndirectParamW(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATEW(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WACREATEDIALOGINDIRECTPARAM )
{
  wa_ret_HWND(CreateDialogIndirectParam(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATE(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}

/*
WINUSERAPI INT_PTR WINAPI DialogBoxParamA(HINSTANCE hInstance,LPCSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WADIALOGBOXPARAMA )
{
  wa_ret_INT_PTR(DialogBoxParamA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI INT_PTR WINAPI DialogBoxParamW(HINSTANCE hInstance,LPCWSTR lpTemplateName,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WADIALOGBOXPARAMW )
{
  wa_ret_INT_PTR(DialogBoxParamW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WADIALOGBOXPARAM )
{
  void * str{};
  wa_ret_INT_PTR(DialogBoxParam(wa_par_HINSTANCE(1), HB_ISCHAR(2) ? HB_PARSTR(2, &str, nullptr) : MAKEINTRESOURCE(hb_parni(2)), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
  hb_strfree(str);
}

/*
WINUSERAPI INT_PTR WINAPI DialogBoxIndirectParamA(HINSTANCE hInstance,LPCDLGTEMPLATEA hDialogTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WADIALOGBOXINDIRECTPARAMA )
{
  wa_ret_INT_PTR(DialogBoxIndirectParamA(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATEA(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI INT_PTR WINAPI DialogBoxIndirectParamW(HINSTANCE hInstance,LPCDLGTEMPLATEW hDialogTemplate,HWND hWndParent,DLGPROC lpDialogFunc,LPARAM dwInitParam)
*/
#if 0
HB_FUNC( WADIALOGBOXINDIRECTPARAMW )
{
  wa_ret_INT_PTR(DialogBoxIndirectParamW(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATEW(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WADIALOGBOXINDIRECTPARAM )
{
  wa_ret_INT_PTR(DialogBoxIndirectParam(wa_par_HINSTANCE(1), wa_par_LPCDLGTEMPLATE(2), wa_par_HWND(3), wa_par_DLGPROC(4), wa_par_LPARAM(5)));
}

/*
WINUSERAPI WINBOOL WINAPI EndDialog(HWND hDlg,INT_PTR nResult)
*/
HB_FUNC( WAENDDIALOG )
{
  wa_ret_BOOL(EndDialog(wa_par_HWND(1), wa_par_INT_PTR(2)));
}

/*
WINUSERAPI HWND WINAPI GetDlgItem(HWND hDlg,int nIDDlgItem)
*/
HB_FUNC( WAGETDLGITEM )
{
  wa_ret_HWND(GetDlgItem(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemInt(HWND hDlg,int nIDDlgItem,UINT uValue,WINBOOL bSigned)
*/
HB_FUNC( WASETDLGITEMINT )
{
  wa_ret_BOOL(SetDlgItemInt(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3), wa_par_BOOL(4)));
}

/*
WINUSERAPI UINT WINAPI GetDlgItemInt(HWND hDlg,int nIDDlgItem,WINBOOL *lpTranslated,WINBOOL bSigned)
*/
HB_FUNC( WAGETDLGITEMINT )
{
  BOOL Translated{};
  wa_ret_UINT(GetDlgItemInt(wa_par_HWND(1), wa_par_int(2), &Translated, wa_par_BOOL(4)));
  wa_stor_BOOL(Translated, 3);
}

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPCSTR lpString)
*/
#if 0
HB_FUNC( WASETDLGITEMTEXTA )
{
  wa_ret_BOOL(SetDlgItemTextA(wa_par_HWND(1), wa_par_int(2), wa_par_LPCSTR(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPCWSTR lpString)
*/
#if 0
HB_FUNC( WASETDLGITEMTEXTW )
{
  wa_ret_BOOL(SetDlgItemTextW(wa_par_HWND(1), wa_par_int(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC( WASETDLGITEMTEXT )
{
  void * str3{};
  wa_ret_BOOL(SetDlgItemText(wa_par_HWND(1), wa_par_int(2), HB_PARSTR(3, &str3, nullptr)));
  hb_strfree(str3);
}

/*
WINUSERAPI UINT WINAPI GetDlgItemTextA(HWND hDlg,int nIDDlgItem,LPSTR lpString,int cchMax)
*/
#if 0
HB_FUNC( WAGETDLGITEMTEXTA )
{
  wa_ret_UINT(GetDlgItemTextA(wa_par_HWND(1), wa_par_int(2), const_cast<LPSTR>(hb_parc(3)), wa_par_int(4)));
}
#endif

/*
WINUSERAPI UINT WINAPI GetDlgItemTextW(HWND hDlg,int nIDDlgItem,LPWSTR lpString,int cchMax)
*/
#if 0
HB_FUNC( WAGETDLGITEMTEXTW )
{
  wa_ret_UINT(GetDlgItemTextW(wa_par_HWND(1), wa_par_int(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_int(4)));
}
#endif

HB_FUNC( WAGETDLGITEMTEXT )
{
  auto cchMax = wa_par_int(4) + 1;
  TCHAR * buffer = new TCHAR[cchMax];
  auto result = GetDlgItemText(wa_par_HWND(1), wa_par_int(2), buffer, cchMax);
  wa_ret_UINT(result);
  HB_STORSTRLEN(buffer, result, 3); // TODO: result == 0 (fail)
  delete[] buffer;
}

/*
WINUSERAPI WINBOOL WINAPI CheckDlgButton(HWND hDlg,int nIDButton,UINT uCheck)
*/
HB_FUNC( WACHECKDLGBUTTON )
{
  wa_ret_BOOL(CheckDlgButton(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CheckRadioButton(HWND hDlg,int nIDFirstButton,int nIDLastButton,int nIDCheckButton)
*/
HB_FUNC( WACHECKRADIOBUTTON )
{
  wa_ret_BOOL(CheckRadioButton(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI UINT WINAPI IsDlgButtonChecked(HWND hDlg,int nIDButton)
*/
HB_FUNC( WAISDLGBUTTONCHECKED )
{
  wa_ret_UINT(IsDlgButtonChecked(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageA(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WASENDDLGITEMMESSAGEA )
{
  wa_ret_LRESULT(SendDlgItemMessageA(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI SendDlgItemMessageW(HWND hDlg,int nIDDlgItem,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WASENDDLGITEMMESSAGEW )
{
  wa_ret_LRESULT(SendDlgItemMessageW(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WASENDDLGITEMMESSAGE )
{
  wa_ret_LRESULT(SendDlgItemMessage(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}

/*
WINUSERAPI HWND WINAPI GetNextDlgGroupItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WAGETNEXTDLGGROUPITEM )
{
  wa_ret_HWND(GetNextDlgGroupItem(wa_par_HWND(1), wa_par_HWND(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI HWND WINAPI GetNextDlgTabItem(HWND hDlg,HWND hCtl,WINBOOL bPrevious)
*/
HB_FUNC( WAGETNEXTDLGTABITEM )
{
  wa_ret_HWND(GetNextDlgTabItem(wa_par_HWND(1), wa_par_HWND(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI GetDlgCtrlID(HWND hWnd)
*/
HB_FUNC( WAGETDLGCTRLID )
{
  wa_ret_int(GetDlgCtrlID(wa_par_HWND(1)));
}

/*
WINUSERAPI __LONG32 WINAPI GetDialogBaseUnits(VOID)
*/
HB_FUNC( WAGETDIALOGBASEUNITS )
{
  wa_ret___LONG32(GetDialogBaseUnits());
}

/*
WINUSERAPI LRESULT WINAPI DefDlgProcA(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFDLGPROCA )
{
  wa_ret_LRESULT(DefDlgProcA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI DefDlgProcW(HWND hDlg,UINT Msg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFDLGPROCW )
{
  wa_ret_LRESULT(DefDlgProcW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WADEFDLGPROC )
{
  wa_ret_LRESULT(DefDlgProc(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterA(LPMSG lpMsg,int nCode)
*/
#if 0
HB_FUNC( WACALLMSGFILTERA )
{
  wa_ret_BOOL(CallMsgFilterA(wa_par_MSG(1), wa_par_int(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI CallMsgFilterW(LPMSG lpMsg,int nCode)
*/
#if 0
HB_FUNC( WACALLMSGFILTERW )
{
  wa_ret_BOOL(CallMsgFilterW(wa_par_MSG(1), wa_par_int(2)));
}
#endif

HB_FUNC( WACALLMSGFILTER )
{
  wa_ret_BOOL(CallMsgFilter(wa_par_MSG(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI OpenClipboard(HWND hWndNewOwner)
*/
HB_FUNC( WAOPENCLIPBOARD )
{
  wa_ret_BOOL(OpenClipboard(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CloseClipboard(VOID)
*/
HB_FUNC( WACLOSECLIPBOARD )
{
  wa_ret_BOOL(CloseClipboard());
}

/*
WINUSERAPI DWORD WINAPI GetClipboardSequenceNumber(VOID)
*/
HB_FUNC( WAGETCLIPBOARDSEQUENCENUMBER )
{
  wa_ret_DWORD(GetClipboardSequenceNumber());
}

/*
WINUSERAPI HWND WINAPI GetClipboardOwner(VOID)
*/
HB_FUNC( WAGETCLIPBOARDOWNER )
{
  wa_ret_HWND(GetClipboardOwner());
}

/*
WINUSERAPI HWND WINAPI SetClipboardViewer(HWND hWndNewViewer)
*/
HB_FUNC( WASETCLIPBOARDVIEWER )
{
  wa_ret_HWND(SetClipboardViewer(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetClipboardViewer(VOID)
*/
HB_FUNC( WAGETCLIPBOARDVIEWER )
{
  wa_ret_HWND(GetClipboardViewer());
}

/*
WINUSERAPI WINBOOL WINAPI ChangeClipboardChain(HWND hWndRemove, HWND hWndNewNext)
*/
HB_FUNC( WACHANGECLIPBOARDCHAIN )
{
  wa_ret_BOOL(ChangeClipboardChain(wa_par_HWND(1), wa_par_HWND(2)));
}

/*
WINUSERAPI HANDLE WINAPI SetClipboardData(UINT uFormat, HANDLE hMem)
*/
HB_FUNC( WASETCLIPBOARDDATA )
{
  wa_ret_HANDLE(SetClipboardData(wa_par_UINT(1), wa_par_HANDLE(2)));
}

/*
WINUSERAPI HANDLE WINAPI GetClipboardData(UINT uFormat)
*/
HB_FUNC( WAGETCLIPBOARDDATA )
{
  wa_ret_HANDLE(GetClipboardData(wa_par_UINT(1)));
}

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatA(LPCSTR lpszFormat)
*/
#if 0
HB_FUNC( WAREGISTERCLIPBOARDFORMATA )
{
  wa_ret_UINT(RegisterClipboardFormatA(wa_par_LPCSTR(1)));
}
#endif

/*
WINUSERAPI UINT WINAPI RegisterClipboardFormatW(LPCWSTR lpszFormat)
*/
#if 0
HB_FUNC( WAREGISTERCLIPBOARDFORMATW )
{
  wa_ret_UINT(RegisterClipboardFormatW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WAREGISTERCLIPBOARDFORMAT )
{
  void * str1{};
  wa_ret_UINT(RegisterClipboardFormat(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI int WINAPI CountClipboardFormats(VOID)
*/
HB_FUNC( WACOUNTCLIPBOARDFORMATS )
{
  wa_ret_int(CountClipboardFormats());
}

/*
WINUSERAPI UINT WINAPI EnumClipboardFormats(UINT format)
*/
HB_FUNC( WAENUMCLIPBOARDFORMATS )
{
  wa_ret_UINT(EnumClipboardFormats(wa_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI GetClipboardFormatNameA(UINT format, LPSTR lpszFormatName, int cchMaxCount)
*/
#if 0
HB_FUNC( WAGETCLIPBOARDFORMATNAMEA )
{
  wa_ret_int(GetClipboardFormatNameA(wa_par_UINT(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}
#endif

/*
WINUSERAPI int WINAPI GetClipboardFormatNameW(UINT format, LPWSTR lpszFormatName, int cchMaxCount)
*/
#if 0
HB_FUNC( WAGETCLIPBOARDFORMATNAMEW )
{
  wa_ret_int(GetClipboardFormatNameW(wa_par_UINT(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}
#endif

HB_FUNC( WAGETCLIPBOARDFORMATNAME )
{
  int cchMaxCount = wa_par_int(3) + 1;
  TCHAR * buffer = new TCHAR[cchMaxCount];
  auto result = GetClipboardFormatName(wa_par_UINT(1), buffer, cchMaxCount);
  wa_ret_int(result);
  HB_STORSTRLEN(buffer, result, 2); // TODO: result == 0 (fail)
  delete[] buffer;
}

/*
WINUSERAPI WINBOOL WINAPI EmptyClipboard(VOID)
*/
HB_FUNC( WAEMPTYCLIPBOARD )
{
  wa_ret_BOOL(EmptyClipboard());
}

/*
WINUSERAPI WINBOOL WINAPI IsClipboardFormatAvailable(UINT format)
*/
HB_FUNC( WAISCLIPBOARDFORMATAVAILABLE )
{
  wa_ret_BOOL(IsClipboardFormatAvailable(wa_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI GetPriorityClipboardFormat(UINT *paFormatPriorityList, int cFormats)
*/

/*
WINUSERAPI HWND WINAPI GetOpenClipboardWindow(VOID)
*/
HB_FUNC( WAGETOPENCLIPBOARDWINDOW )
{
  wa_ret_HWND(GetOpenClipboardWindow());
}

/*
WINUSERAPI WINBOOL WINAPI AddClipboardFormatListener (HWND hwnd)
*/
#if 0
HB_FUNC( WAADDCLIPBOARDFORMATLISTENER )
{
  wa_ret_BOOL(AddClipboardFormatListener(wa_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI RemoveClipboardFormatListener (HWND hwnd)
*/
#if 0
HB_FUNC( WAREMOVECLIPBOARDFORMATLISTENER )
{
  wa_ret_BOOL(RemoveClipboardFormatListener(wa_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetUpdatedClipboardFormats (PUINT lpuiFormats, UINT cFormats, PUINT pcFormatsOut)
*/

/*
WINUSERAPI WINBOOL WINAPI CharToOemA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WACHARTOOEMA )
{
  wa_ret_BOOL(CharToOemA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemW(LPCWSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WACHARTOOEMW )
{
  wa_ret_BOOL(CharToOemW(wa_par_LPCWSTR(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharA(LPCSTR lpszSrc,LPSTR lpszDst)
*/
HB_FUNC( WAOEMTOCHARA )
{
  wa_ret_BOOL(OemToCharA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharW(LPCSTR lpszSrc,LPWSTR lpszDst)
*/
HB_FUNC( WAOEMTOCHARW )
{
  wa_ret_BOOL(OemToCharW(wa_par_LPCSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2)))));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WACHARTOOEMBUFFA )
{
  wa_ret_BOOL(CharToOemBuffA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CharToOemBuffW(LPCWSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WACHARTOOEMBUFFW )
{
  wa_ret_BOOL(CharToOemBuffW(wa_par_LPCWSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffA(LPCSTR lpszSrc,LPSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WAOEMTOCHARBUFFA )
{
  wa_ret_BOOL(OemToCharBuffA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_DWORD(3)));
}

/*
WINUSERAPI WINBOOL WINAPI OemToCharBuffW(LPCSTR lpszSrc,LPWSTR lpszDst,DWORD cchDstLength)
*/
HB_FUNC( WAOEMTOCHARBUFFW )
{
  wa_ret_BOOL(OemToCharBuffW(wa_par_LPCSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_DWORD(3)));
}

/*
WINUSERAPI LPSTR WINAPI CharUpperA(LPSTR lpsz)
*/
HB_FUNC( WACHARUPPERA )
{
  hb_retc(( LPSTR ) CharUpperA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINUSERAPI LPWSTR WINAPI CharUpperW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharUpperBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WACHARUPPERBUFFA )
{
  wa_ret_DWORD(CharUpperBuffA(const_cast<LPSTR>(hb_parc(1)), wa_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI CharUpperBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WACHARUPPERBUFFW )
{
  wa_ret_DWORD(CharUpperBuffW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_DWORD(2)));
}

/*
WINUSERAPI LPSTR WINAPI CharLowerA(LPSTR lpsz)
*/
HB_FUNC( WACHARLOWERA )
{
  hb_retc(( LPSTR ) CharLowerA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINUSERAPI LPWSTR WINAPI CharLowerW(LPWSTR lpsz)
*/

/*
WINUSERAPI DWORD WINAPI CharLowerBuffA(LPSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WACHARLOWERBUFFA )
{
  wa_ret_DWORD(CharLowerBuffA(const_cast<LPSTR>(hb_parc(1)), wa_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI CharLowerBuffW(LPWSTR lpsz,DWORD cchLength)
*/
HB_FUNC( WACHARLOWERBUFFW )
{
  wa_ret_DWORD(CharLowerBuffW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_DWORD(2)));
}

/*
WINUSERAPI LPSTR WINAPI CharNextA(LPCSTR lpsz)
*/
HB_FUNC( WACHARNEXTA )
{
  hb_retc(( LPSTR ) CharNextA(wa_par_LPCSTR(1)));
}

/*
WINUSERAPI LPWSTR WINAPI CharNextW(LPCWSTR lpsz)
*/
#if 0
HB_FUNC( WACHARNEXTW )
{
  hb_retc(( LPWSTR ) CharNextW(wa_par_LPCWSTR(1))); // TODO: fix
}
#endif

/*
WINUSERAPI LPSTR WINAPI CharPrevA(LPCSTR lpszStart,LPCSTR lpszCurrent)
*/
HB_FUNC( WACHARPREVA )
{
  hb_retc(( LPSTR ) CharPrevA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}

/*
WINUSERAPI LPWSTR WINAPI CharPrevW(LPCWSTR lpszStart,LPCWSTR lpszCurrent)
*/
#if 0
HB_FUNC( WACHARPREVW )
{
  hb_retc(( LPWSTR ) CharPrevW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2))); // TODO: fix
}
#endif

/*
WINUSERAPI LPSTR WINAPI CharNextExA(WORD CodePage,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WACHARNEXTEXA )
{
  hb_retc(( LPSTR ) CharNextExA(wa_par_WORD(1), wa_par_LPCSTR(2), wa_par_DWORD(3)));
}

/*
WINUSERAPI LPSTR WINAPI CharPrevExA(WORD CodePage,LPCSTR lpStart,LPCSTR lpCurrentChar,DWORD dwFlags)
*/
HB_FUNC( WACHARPREVEXA )
{
  hb_retc(( LPSTR ) CharPrevExA(wa_par_WORD(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaA(CHAR ch)
*/
HB_FUNC( WAISCHARALPHAA )
{
  wa_ret_BOOL(IsCharAlphaA(wa_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaW(WCHAR ch)
*/
HB_FUNC( WAISCHARALPHAW )
{
  wa_ret_BOOL(IsCharAlphaW(wa_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericA(CHAR ch)
*/
HB_FUNC( WAISCHARALPHANUMERICA )
{
  wa_ret_BOOL(IsCharAlphaNumericA(wa_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharAlphaNumericW(WCHAR ch)
*/
HB_FUNC( WAISCHARALPHANUMERICW )
{
  wa_ret_BOOL(IsCharAlphaNumericW(wa_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperA(CHAR ch)
*/
HB_FUNC( WAISCHARUPPERA )
{
  wa_ret_BOOL(IsCharUpperA(wa_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharUpperW(WCHAR ch)
*/
HB_FUNC( WAISCHARUPPERW )
{
  wa_ret_BOOL(IsCharUpperW(wa_par_WCHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerA(CHAR ch)
*/
HB_FUNC( WAISCHARLOWERA )
{
  wa_ret_BOOL(IsCharLowerA(wa_par_CHAR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI IsCharLowerW(WCHAR ch)
*/
HB_FUNC( WAISCHARLOWERW )
{
  wa_ret_BOOL(IsCharLowerW(wa_par_WCHAR(1)));
}

/*
WINUSERAPI HWND WINAPI SetFocus(HWND hWnd)
*/
HB_FUNC( WASETFOCUS )
{
  wa_ret_HWND(SetFocus(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetActiveWindow(VOID)
*/
HB_FUNC( WAGETACTIVEWINDOW )
{
  wa_ret_HWND(GetActiveWindow());
}

/*
WINUSERAPI HWND WINAPI GetFocus(VOID)
*/
HB_FUNC( WAGETFOCUS )
{
  wa_ret_HWND(GetFocus());
}

/*
WINUSERAPI UINT WINAPI GetKBCodePage(VOID)
*/
HB_FUNC( WAGETKBCODEPAGE )
{
  wa_ret_UINT(GetKBCodePage());
}

/*
WINUSERAPI SHORT WINAPI GetKeyState(int nVirtKey)
*/
HB_FUNC( WAGETKEYSTATE )
{
  wa_ret_SHORT(GetKeyState(wa_par_int(1)));
}

/*
WINUSERAPI SHORT WINAPI GetAsyncKeyState(int vKey)
*/
HB_FUNC( WAGETASYNCKEYSTATE )
{
  wa_ret_SHORT(GetAsyncKeyState(wa_par_int(1)));
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
#if 0
HB_FUNC( WAGETKEYNAMETEXTA )
{
  wa_ret_int(GetKeyNameTextA(wa_par_LONG(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}
#endif

/*
WINUSERAPI int WINAPI GetKeyNameTextW(LONG lParam,LPWSTR lpString,int cchSize)
*/
#if 0
HB_FUNC( WAGETKEYNAMETEXTW )
{
  wa_ret_int(GetKeyNameTextW(wa_par_LONG(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}
#endif

HB_FUNC( WAGETKEYNAMETEXT )
{
  int cchSize = wa_par_int(3) + 1;
  TCHAR * buffer = new TCHAR[cchSize];
  auto result = GetKeyNameText(wa_par_LONG(1), buffer, cchSize);
  wa_ret_int(result);
  HB_STORSTRLEN(buffer, result, 2);
  delete[] buffer;
}

/*
WINUSERAPI int WINAPI GetKeyboardType(int nTypeFlag)
*/
HB_FUNC( WAGETKEYBOARDTYPE )
{
  wa_ret_int(GetKeyboardType(wa_par_int(1)));
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
HB_FUNC( WAOEMKEYSCAN )
{
  wa_ret_DWORD(OemKeyScan(wa_par_WORD(1)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanA(CHAR ch)
*/
HB_FUNC( WAVKKEYSCANA )
{
  wa_ret_SHORT(VkKeyScanA(wa_par_CHAR(1)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanW(WCHAR ch)
*/
HB_FUNC( WAVKKEYSCANW )
{
  wa_ret_SHORT(VkKeyScanW(wa_par_WCHAR(1)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanExA(CHAR ch,HKL dwhkl)
*/
HB_FUNC( WAVKKEYSCANEXA )
{
  wa_ret_SHORT(VkKeyScanExA(wa_par_CHAR(1), wa_par_HKL(2)));
}

/*
WINUSERAPI SHORT WINAPI VkKeyScanExW(WCHAR ch,HKL dwhkl)
*/
HB_FUNC( WAVKKEYSCANEXW )
{
  wa_ret_SHORT(VkKeyScanExW(wa_par_WCHAR(1), wa_par_HKL(2)));
}

/*
WINUSERAPI VOID WINAPI keybd_event(BYTE bVk,BYTE bScan,DWORD dwFlags,ULONG_PTR dwExtraInfo)
*/
HB_FUNC( WAKEYBD_EVENT )
{
  keybd_event(wa_par_BYTE(1), wa_par_BYTE(2), wa_par_DWORD(3), wa_par_ULONG_PTR(4));
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
HB_FUNC( WAREGISTERTOUCHWINDOW )
{
  wa_ret_BOOL(RegisterTouchWindow(wa_par_HWND(1), wa_par_ULONG(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI UnregisterTouchWindow (HWND hwnd)
*/
#if 0
HB_FUNC( WAUNREGISTERTOUCHWINDOW )
{
  wa_ret_BOOL(UnregisterTouchWindow(wa_par_HWND(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsTouchWindow (HWND hwnd, PULONG pulFlags)
*/
#if 0
HB_FUNC( WAISTOUCHWINDOW )
{
  ULONG Flags{};
  wa_ret_BOOL(IsTouchWindow(wa_par_HWND(1), &Flags));
  wa_stor_ULONG(Flags, 2);
}
#endif

/*
WINUSERAPI WINBOOL WINAPI InitializeTouchInjection (UINT32 maxCount, DWORD dwMode)
*/
#if 0
HB_FUNC( WAINITIALIZETOUCHINJECTION )
{
  wa_ret_BOOL(InitializeTouchInjection(wa_par_UINT32(1), wa_par_DWORD(2)));
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
HB_FUNC( WASKIPPOINTERFRAMEMESSAGES )
{
  wa_ret_BOOL(SkipPointerFrameMessages(wa_par_UINT32(1)));
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
HB_FUNC( WAENABLEMOUSEINPOINTER )
{
  wa_ret_BOOL(EnableMouseInPointer(wa_par_BOOL(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsMouseInPointerEnabled (VOID)
*/
#if 0
HB_FUNC( WAISMOUSEINPOINTERENABLED )
{
  wa_ret_BOOL(IsMouseInPointerEnabled());
}
#endif

/*
WINUSERAPI WINBOOL WINAPI RegisterTouchHitTestingWindow (HWND hwnd, ULONG value)
*/
#if 0
HB_FUNC( WAREGISTERTOUCHHITTESTINGWINDOW )
{
  wa_ret_BOOL(RegisterTouchHitTestingWindow(wa_par_HWND(1), wa_par_ULONG(2)));
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
#if 0
HB_FUNC( WAMAPVIRTUALKEYA )
{
  wa_ret_UINT(MapVirtualKeyA(wa_par_UINT(1), wa_par_UINT(2)));
}
#endif

/*
WINUSERAPI UINT WINAPI MapVirtualKeyW(UINT uCode,UINT uMapType)
*/
#if 0
HB_FUNC( WAMAPVIRTUALKEYW )
{
  wa_ret_UINT(MapVirtualKeyW(wa_par_UINT(1), wa_par_UINT(2)));
}
#endif

HB_FUNC( WAMAPVIRTUALKEY )
{
  wa_ret_UINT(MapVirtualKey(wa_par_UINT(1), wa_par_UINT(2)));
}

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExA(UINT uCode,UINT uMapType,HKL dwhkl)
*/
#if 0
HB_FUNC( WAMAPVIRTUALKEYEXA )
{
  wa_ret_UINT(MapVirtualKeyExA(wa_par_UINT(1), wa_par_UINT(2), wa_par_HKL(3)));
}
#endif

/*
WINUSERAPI UINT WINAPI MapVirtualKeyExW(UINT uCode,UINT uMapType,HKL dwhkl)
*/
#if 0
HB_FUNC( WAMAPVIRTUALKEYEXW )
{
  wa_ret_UINT(MapVirtualKeyExW(wa_par_UINT(1), wa_par_UINT(2), wa_par_HKL(3)));
}
#endif

HB_FUNC( WAMAPVIRTUALKEYEX )
{
  wa_ret_UINT(MapVirtualKeyEx(wa_par_UINT(1), wa_par_UINT(2), wa_par_HKL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI GetInputState(VOID)
*/
HB_FUNC( WAGETINPUTSTATE )
{
  wa_ret_BOOL(GetInputState());
}

/*
WINUSERAPI DWORD WINAPI GetQueueStatus(UINT flags)
*/
HB_FUNC( WAGETQUEUESTATUS )
{
  wa_ret_DWORD(GetQueueStatus(wa_par_UINT(1)));
}

/*
WINUSERAPI HWND WINAPI GetCapture(VOID)
*/
HB_FUNC( WAGETCAPTURE )
{
  wa_ret_HWND(GetCapture());
}

/*
WINUSERAPI HWND WINAPI SetCapture(HWND hWnd)
*/
HB_FUNC( WASETCAPTURE )
{
  wa_ret_HWND(SetCapture(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ReleaseCapture(VOID)
*/
HB_FUNC( WARELEASECAPTURE )
{
  wa_ret_BOOL(ReleaseCapture());
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
HB_FUNC( WASETTIMER )
{
  wa_ret_UINT_PTR(SetTimer(wa_par_HWND(1), wa_par_UINT_PTR(2), wa_par_UINT(3), wa_par_TIMERPROC(4)));
}

/*
WINUSERAPI WINBOOL WINAPI KillTimer(HWND hWnd,UINT_PTR uIDEvent)
*/
HB_FUNC( WAKILLTIMER )
{
  wa_ret_BOOL(KillTimer(wa_par_HWND(1), wa_par_UINT_PTR(2)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowUnicode(HWND hWnd)
*/
HB_FUNC( WAISWINDOWUNICODE )
{
  wa_ret_BOOL(IsWindowUnicode(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableWindow(HWND hWnd,WINBOOL bEnable)
*/
HB_FUNC( WAENABLEWINDOW )
{
  wa_ret_BOOL(EnableWindow(wa_par_HWND(1), wa_par_BOOL(2)));
}

/*
WINUSERAPI WINBOOL WINAPI IsWindowEnabled(HWND hWnd)
*/
HB_FUNC( WAISWINDOWENABLED )
{
  wa_ret_BOOL(IsWindowEnabled(wa_par_HWND(1)));
}

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsA(HINSTANCE hInstance,LPCSTR lpTableName)
*/
#if 0
HB_FUNC( WALOADACCELERATORSA )
{
  wa_ret_HACCEL(LoadAcceleratorsA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HACCEL WINAPI LoadAcceleratorsW(HINSTANCE hInstance,LPCWSTR lpTableName)
*/
#if 0
HB_FUNC( WALOADACCELERATORSW )
{
  wa_ret_HACCEL(LoadAcceleratorsW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALOADACCELERATORS )
{
  void * str2{};
  wa_ret_HACCEL(LoadAccelerators(wa_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableA(LPACCEL paccel,int cAccel)
*/
#if 0
HB_FUN( WACREATEACCELERATORTABLEA )
{
  std::vector<ACCEL> vec{};
  auto pArray = hb_param(1, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<ACCEL*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  wa_ret_HACCEL(CreateAcceleratorTableA(vec.data(), wa_par_int(2)));
}
#endif

/*
WINUSERAPI HACCEL WINAPI CreateAcceleratorTableW(LPACCEL paccel,int cAccel)
*/
#if 0
HB_FUNC( WACREATEACCELERATORTABLEW )
{
  std::vector<ACCEL> vec{};
  auto pArray = hb_param(1, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<ACCEL*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  wa_ret_HACCEL(CreateAcceleratorTableW(vec.data(), wa_par_int(2)));
}
#endif

HB_FUNC( WACREATEACCELERATORTABLE )
{
  std::vector<ACCEL> vec{};
  auto pArray = hb_param(1, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(*static_cast<ACCEL*>(hb_objDataGetPtr(hb_arrayGetItemPtr(pArray, i + 1), "PTR")));
    }
  }
  wa_ret_HACCEL(CreateAcceleratorTable(vec.data(), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyAcceleratorTable(HACCEL hAccel)
*/
HB_FUNC( WADESTROYACCELERATORTABLE )
{
  wa_ret_BOOL(DestroyAcceleratorTable(wa_par_HACCEL(1)));
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
#if 0
HB_FUNC( WATRANSLATEACCELERATORA )
{
  wa_ret_int(TranslateAcceleratorA(wa_par_HWND(1), wa_par_HACCEL(2), wa_par_MSG(3)));
}
#endif

/*
WINUSERAPI int WINAPI TranslateAcceleratorW(HWND hWnd,HACCEL hAccTable,LPMSG lpMsg)
*/
#if 0
HB_FUNC( WATRANSLATEACCELERATORW )
{
  wa_ret_int(TranslateAcceleratorW(wa_par_HWND(1), wa_par_HACCEL(2), wa_par_MSG(3)));
}
#endif

HB_FUNC( WATRANSLATEACCELERATOR )
{
  wa_ret_int(TranslateAccelerator(wa_par_HWND(1), wa_par_HACCEL(2), wa_par_MSG(3)));
}

/*
WINUSERAPI UINT_PTR WINAPI SetCoalescableTimer (HWND hWnd, UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc, ULONG uToleranceDelay)
*/
#if 0 // TODO: for Windows 8 or upper
HB_FUNC( WASETCOALESCABLETIMER )
{
  wa_ret_UINT_PTR(SetCoalescableTimer(wa_par_HWND(1), wa_par_UINT_PTR(2), wa_par_UINT(3), wa_par_TIMERPROC(4), wa_par_ULONG(5)));
}
#endif

/*
WINUSERAPI HMENU WINAPI LoadMenuA(HINSTANCE hInstance,LPCSTR lpMenuName)
*/
#if 0
HB_FUNC( WALOADMENUA )
{
  wa_ret_HMENU(LoadMenuA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HMENU WINAPI LoadMenuW(HINSTANCE hInstance,LPCWSTR lpMenuName)
*/
#if 0
HB_FUNC( WALOADMENUW )
{
  wa_ret_HMENU(LoadMenuW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALOADMENU )
{
  void * str{};
  wa_ret_HMENU(LoadMenu(wa_par_HINSTANCE(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
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
HB_FUNC( WAGETMENU )
{
  wa_ret_HMENU(GetMenu(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenu(HWND hWnd,HMENU hMenu)
*/
HB_FUNC( WASETMENU )
{
  wa_ret_BOOL(SetMenu(wa_par_HWND(1), wa_par_HMENU(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuA(HMENU hMenu,UINT cmd,LPCSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
#if 0
HB_FUNC( WACHANGEMENUA )
{
  wa_ret_BOOL(ChangeMenuA(wa_par_HMENU(1), wa_par_UINT(2), wa_par_LPCSTR(3), wa_par_UINT(4), wa_par_UINT(5)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI ChangeMenuW(HMENU hMenu,UINT cmd,LPCWSTR lpszNewItem,UINT cmdInsert,UINT flags)
*/
#if 0
HB_FUNC( WACHANGEMENUW )
{
  wa_ret_BOOL(ChangeMenuW(wa_par_HMENU(1), wa_par_UINT(2), wa_par_LPCWSTR(3), wa_par_UINT(4), wa_par_UINT(5)));
}
#endif

HB_FUNC( WACHANGEMENU )
{
  void * str{};
  wa_ret_BOOL(ChangeMenu(wa_par_HMENU(1), wa_par_UINT(2), HB_PARSTR(3, &str, nullptr), wa_par_UINT(4), wa_par_UINT(5)));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI HiliteMenuItem(HWND hWnd,HMENU hMenu,UINT uIDHiliteItem,UINT uHilite)
*/
HB_FUNC( WAHILITEMENUITEM )
{
  wa_ret_BOOL(HiliteMenuItem(wa_par_HWND(1), wa_par_HMENU(2), wa_par_UINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI int WINAPI GetMenuStringA(HMENU hMenu,UINT uIDItem,LPSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WAGETMENUSTRINGA ) // TODO: fix
{
  wa_ret_int(GetMenuStringA(wa_par_HMENU(1), wa_par_UINT(2), const_cast<LPSTR>(hb_parc(3)), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI GetMenuStringW(HMENU hMenu,UINT uIDItem,LPWSTR lpString,int cchMax,UINT flags)
*/
HB_FUNC( WAGETMENUSTRINGW ) // TODO: fix
{
  wa_ret_int(GetMenuStringW(wa_par_HMENU(1), wa_par_UINT(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI UINT WINAPI GetMenuState(HMENU hMenu,UINT uId,UINT uFlags)
*/
HB_FUNC( WAGETMENUSTATE )
{
  wa_ret_UINT(GetMenuState(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawMenuBar(HWND hWnd)
*/
HB_FUNC( WADRAWMENUBAR )
{
  wa_ret_BOOL(DrawMenuBar(wa_par_HWND(1)));
}

/*
WINUSERAPI HMENU WINAPI GetSystemMenu(HWND hWnd,WINBOOL bRevert)
*/
HB_FUNC( WAGETSYSTEMMENU )
{
  wa_ret_HMENU(GetSystemMenu(wa_par_HWND(1), wa_par_BOOL(2)));
}

/*
WINUSERAPI HMENU WINAPI CreateMenu(VOID)
*/
HB_FUNC( WACREATEMENU )
{
  wa_ret_HMENU(CreateMenu());
}

/*
WINUSERAPI HMENU WINAPI CreatePopupMenu(VOID)
*/
HB_FUNC( WACREATEPOPUPMENU )
{
  wa_ret_HMENU(CreatePopupMenu());
}

/*
WINUSERAPI WINBOOL WINAPI DestroyMenu(HMENU hMenu)
*/
HB_FUNC( WADESTROYMENU )
{
  wa_ret_BOOL(DestroyMenu(wa_par_HMENU(1)));
}

/*
WINUSERAPI DWORD WINAPI CheckMenuItem(HMENU hMenu,UINT uIDCheckItem,UINT uCheck)
*/
HB_FUNC( WACHECKMENUITEM )
{
  wa_ret_DWORD(CheckMenuItem(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableMenuItem(HMENU hMenu,UINT uIDEnableItem,UINT uEnable)
*/
HB_FUNC( WAENABLEMENUITEM )
{
  wa_ret_BOOL(EnableMenuItem(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI HMENU WINAPI GetSubMenu(HMENU hMenu,int nPos)
*/
HB_FUNC( WAGETSUBMENU )
{
  wa_ret_HMENU(GetSubMenu(wa_par_HMENU(1), wa_par_int(2)));
}

/*
WINUSERAPI UINT WINAPI GetMenuItemID(HMENU hMenu,int nPos)
*/
HB_FUNC( WAGETMENUITEMID )
{
  wa_ret_UINT(GetMenuItemID(wa_par_HMENU(1), wa_par_int(2)));
}

/*
WINUSERAPI int WINAPI GetMenuItemCount(HMENU hMenu)
*/
HB_FUNC( WAGETMENUITEMCOUNT )
{
  wa_ret_int(GetMenuItemCount(wa_par_HMENU(1)));
}

/*
WINUSERAPI WINBOOL WINAPI InsertMenuA(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
#if 0
HB_FUNC( WAINSERTMENUA )
{
  wa_ret_BOOL(InsertMenuA(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), wa_par_LPCSTR(5)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI InsertMenuW(HMENU hMenu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
#if 0
HB_FUNC( WAINSERTMENUW )
{
  wa_ret_BOOL(InsertMenuW(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), wa_par_LPCWSTR(5)));
}
#endif

HB_FUNC( WAINSERTMENU )
{
  void * str{};
  wa_ret_BOOL(InsertMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), HB_PARSTR(5, &str, nullptr)));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI AppendMenuA(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
#if 0
HB_FUNC( WAAPPENDMENUA )
{
  wa_ret_BOOL(AppendMenuA(wa_par_HMENU(1), wa_par_UINT(3), wa_par_UINT_PTR(3), wa_par_LPCSTR(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI AppendMenuW(HMENU hMenu,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
#if 0
HB_FUNC( WAAPPENDMENUW )
{
  wa_ret_BOOL(AppendMenuW(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT_PTR(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC( WAAPPENDMENU )
{
  void * str{};
  wa_ret_BOOL(AppendMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT_PTR(3), HB_PARSTR(4, &str, nullptr)));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuA(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCSTR lpNewItem)
*/
#if 0
HB_FUNC( WAMODIFYMENUA )
{
  wa_ret_BOOL(ModifyMenuA(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), wa_par_LPCSTR(5)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI ModifyMenuW(HMENU hMnu,UINT uPosition,UINT uFlags,UINT_PTR uIDNewItem,LPCWSTR lpNewItem)
*/
#if 0
HB_FUNC( WAMODIFYMENUW )
{
  wa_ret_BOOL(ModifyMenuW(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), wa_par_LPCWSTR(5)));
}
#endif

HB_FUNC( WAMODIFYMENU )
{
  void * str{};
  wa_ret_BOOL(ModifyMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT_PTR(4), HB_PARSTR(5, &str, nullptr)));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI RemoveMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WAREMOVEMENU )
{
  wa_ret_BOOL(RemoveMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI DeleteMenu(HMENU hMenu,UINT uPosition,UINT uFlags)
*/
HB_FUNC( WADELETEMENU )
{
  wa_ret_BOOL(DeleteMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuItemBitmaps(HMENU hMenu,UINT uPosition,UINT uFlags,HBITMAP hBitmapUnchecked,HBITMAP hBitmapChecked)
*/
HB_FUNC( WASETMENUITEMBITMAPS )
{
  wa_ret_BOOL(SetMenuItemBitmaps(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_HBITMAP(4), wa_par_HBITMAP(5)));
}

/*
WINUSERAPI LONG WINAPI GetMenuCheckMarkDimensions(VOID)
*/
HB_FUNC( WAGETMENUCHECKMARKDIMENSIONS )
{
  wa_ret_LONG(GetMenuCheckMarkDimensions());
}

/*
WINUSERAPI WINBOOL WINAPI TrackPopupMenu(HMENU hMenu,UINT uFlags,int x,int y,int nReserved,HWND hWnd,CONST RECT *prcRect)
*/
HB_FUNC( WATRACKPOPUPMENU )
{
  wa_ret_BOOL(TrackPopupMenu(wa_par_HMENU(1), wa_par_UINT(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), wa_par_HWND(6), wa_par_RECT(7)));
}

/*
WINUSERAPI WINBOOL WINAPI TrackPopupMenuEx(HMENU,UINT,int,int,HWND,LPTPMPARAMS)
*/

/*
WINUSERAPI WINBOOL WINAPI GetMenuInfo(HMENU,LPMENUINFO)
*/
HB_FUNC( WAGETMENUINFO )
{
  wa_ret_BOOL(GetMenuInfo(wa_par_HMENU(1), wa_par_MENUINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuInfo(HMENU,LPCMENUINFO)
*/
HB_FUNC( WASETMENUINFO )
{
  wa_ret_BOOL(SetMenuInfo(wa_par_HMENU(1), wa_par_MENUINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EndMenu(VOID)
*/
HB_FUNC( WAENDMENU )
{
  wa_ret_BOOL(EndMenu());
}

/*
WINUSERAPI WINBOOL WINAPI CalculatePopupWindowPosition(const POINT *anchorPoint, const SIZE *windowSize, UINT flags, RECT *excludeRect, RECT *popupWindowPosition)
*/
#if 0 // TODO: Windows 7
HB_FUNC( WACALCULATEPOPUPWINDOWPOSITION )
{
  wa_ret_BOOL(CalculatePopupWindowPosition(wa_par_POINT(1), wa_par_SIZE(2), wa_par_UINT(3), wa_par_RECT(4)), wa_par_RECT(5)));
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
HB_FUNC( WAGETMENUDEFAULTITEM )
{
  wa_ret_UINT(GetMenuDefaultItem(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuDefaultItem(HMENU hMenu,UINT uItem,UINT fByPos)
*/
HB_FUNC( WASETMENUDEFAULTITEM )
{
  wa_ret_BOOL(SetMenuDefaultItem(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI GetMenuItemRect(HWND hWnd,HMENU hMenu,UINT uItem,LPRECT lprcItem)
*/
HB_FUNC( WAGETMENUITEMRECT )
{
  wa_ret_BOOL(GetMenuItemRect(wa_par_HWND(1), wa_par_HMENU(2), wa_par_UINT(3), wa_par_RECT(4)));
}

/*
WINUSERAPI int WINAPI MenuItemFromPoint(HWND hWnd,HMENU hMenu,POINT ptScreen)
*/
#if 0
HB_FUNC( WAMENUITEMFROMPOINT )
{
  wa_ret_int(MenuItemFromPoint(wa_par_HWND(1), wa_par_HMENU(2), wa_par_POINT(3)));
}
#endif

/*
WINUSERAPI DWORD WINAPI DragObject(HWND hwndParent,HWND hwndFrom,UINT fmt,ULONG_PTR data,HCURSOR hcur)
*/
HB_FUNC( WADRAGOBJECT )
{
  wa_ret_DWORD(DragObject(wa_par_HWND(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_ULONG_PTR(4), wa_par_HCURSOR(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DragDetect(HWND hwnd,POINT pt)
*/
HB_FUNC( WADRAGDETECT )
{
  wa_ret_BOOL(DragDetect(wa_par_HWND(1), *wa_par_POINT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawIcon(HDC hDC,int X,int Y,HICON hIcon)
*/
HB_FUNC( WADRAWICON )
{
  wa_ret_BOOL(DrawIcon(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_HICON(4)));
}

/*
WINUSERAPI int WINAPI DrawTextA(HDC hdc,LPCSTR lpchText,int cchText,LPRECT lprc,UINT format)
*/
#if 0
HB_FUNC( WADRAWTEXTA )
{
  wa_ret_int(DrawTextA(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), wa_par_RECT(4), wa_par_UINT(5)));
}
#endif

/*
WINUSERAPI int WINAPI DrawTextW(HDC hdc,LPCWSTR lpchText,int cchText,LPRECT lprc,UINT format)
*/
#if 0
HB_FUNC( WADRAWTEXTW )
{
  wa_ret_int(DrawTextW(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), wa_par_RECT(4), wa_par_UINT(5)));
}
#endif

HB_FUNC( WADRAWTEXT )
{
  void * str{};
  wa_ret_int(DrawTextW(wa_par_HDC(1), HB_PARSTR(2, &str, nullptr), wa_par_int(3), wa_par_RECT(4), wa_par_UINT(5)));
  hb_strfree(str);
}

/*
WINUSERAPI int WINAPI DrawTextExA(HDC hdc,LPSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp)
*/
#if 0
HB_FUNC( WADRAWTEXTEXA )
{
  wa_ret_int(DrawTextExA(wa_par_HDC(1), (LPSTR) hb_parc(2), wa_par_in(3), wa_par_RECT(4), wa_par_UINT(5), wa_par_DRAWTEXTPARAMS(6)));
}
#endif

/*
WINUSERAPI int WINAPI DrawTextExW(HDC hdc,LPWSTR lpchText,int cchText,LPRECT lprc,UINT format,LPDRAWTEXTPARAMS lpdtp)
*/
#if 0
HB_FUNC( WADRAWTEXTEXW )
{
  wa_ret_int(DrawTextExW(wa_par_HDC(1), (LPWSTR) hb_parc(2), wa_par_int(3), wa_par_RECT(4), wa_par_UINT(5), wa_par_DRAWTEXTPARAMS(6)));
}
#endif

HB_FUNC( WADRAWTEXTEX )
{
  void * str{};
  wa_ret_int(DrawTextEx(wa_par_HDC(1), (LPWSTR) HB_PARSTR(2, &str, nullptr), wa_par_int(3), wa_par_RECT(4), wa_par_UINT(5), wa_par_DRAWTEXTPARAMS(6)));
  hb_strfree(str);
}

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
#if 0
HB_FUNC( WATABBEDTEXTOUTA )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(7, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  wa_ret_LONG(TabbedTextOutA(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCSTR(4), wa_par_int(5), wa_par_int(6), vec.data(), wa_par_int(8)));
}
#endif

/*
WINUSERAPI LONG WINAPI TabbedTextOutW(HDC hdc,int x,int y,LPCWSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions,int nTabOrigin)
*/
#if 0
HB_FUNC( WATABBEDTEXTOUTW )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(7, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  wa_ret_LONG(TabbedTextOutW(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_LPCWSTR(4), wa_par_int(5), wa_par_int(6), vec.data(), wa_par_int(8)));
}
#endif

HB_FUNC( WATABBEDTEXTOUT )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(7, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  void * str{};
  wa_ret_LONG(TabbedTextOut(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), HB_PARSTR(4, &str, nullptr), wa_par_int(5), wa_par_int(6), vec.data(), wa_par_int(8)));
  hb_strfree(str);
}

/*
WINUSERAPI DWORD WINAPI GetTabbedTextExtentA(HDC hdc,LPCSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions)
*/
#if 0
HB_FUNC( WAGETTABBEDTEXTEXTENTA )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  wa_ret_DWORD(GetTabbedTextExtentA(wa_par_HDC(1), wa_par_LPCSTR(2), wa_par_int(3), wa_par_int(4), vec.data()));
}
#endif

/*
WINUSERAPI DWORD WINAPI GetTabbedTextExtentW(HDC hdc,LPCWSTR lpString,int chCount,int nTabPositions,CONST INT *lpnTabStopPositions)
*/
#if 0
HB_FUNC( WAGETTABBEDTEXTEXTENTW )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  wa_ret_DWORD(GetTabbedTextExtentW(wa_par_HDC(1), wa_par_LPCWSTR(2), wa_par_int(3), wa_par_int(4), vec.data()));
}
#endif

HB_FUNC( WAGETTABBEDTEXTEXTENT )
{
  std::vector<INT> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  void * str{};
  wa_ret_DWORD(GetTabbedTextExtent(wa_par_HDC(1), HB_PARSTR(2, &str, nullptr), wa_par_int(3), wa_par_int(4), vec.data()));
  hb_strfree(str);
}

/*
WINUSERAPI WINBOOL WINAPI UpdateWindow(HWND hWnd)
*/
HB_FUNC( WAUPDATEWINDOW )
{
  wa_ret_BOOL(UpdateWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI SetActiveWindow(HWND hWnd)
*/
HB_FUNC( WASETACTIVEWINDOW )
{
  wa_ret_HWND(SetActiveWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetForegroundWindow(VOID)
*/
HB_FUNC( WAGETFOREGROUNDWINDOW )
{
  wa_ret_HWND(GetForegroundWindow());
}

/*
WINUSERAPI WINBOOL WINAPI PaintDesktop(HDC hdc)
*/
HB_FUNC( WAPAINTDESKTOP )
{
  wa_ret_BOOL(PaintDesktop(wa_par_HDC(1)));
}

/*
WINUSERAPI VOID WINAPI SwitchToThisWindow(HWND hwnd,WINBOOL fUnknown)
*/
HB_FUNC( WASWITCHTOTHISWINDOW )
{
  SwitchToThisWindow(wa_par_HWND(1), wa_par_BOOL(2));
}

/*
WINUSERAPI WINBOOL WINAPI SetForegroundWindow(HWND hWnd)
*/
HB_FUNC( WASETFOREGROUNDWINDOW )
{
  wa_ret_BOOL(SetForegroundWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI AllowSetForegroundWindow(DWORD dwProcessId)
*/
HB_FUNC( WAALLOWSETFOREGROUNDWINDOW )
{
  wa_ret_BOOL(AllowSetForegroundWindow(wa_par_DWORD(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LockSetForegroundWindow(UINT uLockCode)
*/
HB_FUNC( WALOCKSETFOREGROUNDWINDOW )
{
  wa_ret_BOOL(LockSetForegroundWindow(wa_par_UINT(1)));
}

/*
WINUSERAPI HWND WINAPI WindowFromDC(HDC hDC)
*/
HB_FUNC( WAWINDOWFROMDC )
{
  wa_ret_HWND(WindowFromDC(wa_par_HDC(1)));
}

/*
WINUSERAPI HDC WINAPI GetDC(HWND hWnd)
*/
HB_FUNC( WAGETDC )
{
  wa_ret_HDC(GetDC(wa_par_HWND(1)));
}

/*
WINUSERAPI HDC WINAPI GetDCEx(HWND hWnd,HRGN hrgnClip,DWORD flags)
*/
HB_FUNC( WAGETDCEX )
{
  wa_ret_HDC(GetDCEx(wa_par_HWND(1), wa_par_HRGN(2), wa_par_DWORD(3)));
}

/*
WINUSERAPI HDC WINAPI GetWindowDC(HWND hWnd)
*/
HB_FUNC( WAGETWINDOWDC )
{
  wa_ret_HDC(GetWindowDC(wa_par_HWND(1)));
}

/*
WINUSERAPI int WINAPI ReleaseDC(HWND hWnd,HDC hDC)
*/
HB_FUNC( WARELEASEDC )
{
  wa_ret_int(ReleaseDC(wa_par_HWND(1), wa_par_HDC(2)));
}

/*
WINUSERAPI HDC WINAPI BeginPaint(HWND hWnd,LPPAINTSTRUCT lpPaint)
*/
HB_FUNC( WABEGINPAINT )
{
  wa_ret_HDC(BeginPaint(wa_par_HWND(1), wa_par_PAINTSTRUCT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EndPaint(HWND hWnd,CONST PAINTSTRUCT *lpPaint)
*/
HB_FUNC( WAENDPAINT )
{
  wa_ret_BOOL(EndPaint(wa_par_HWND(1), wa_par_PAINTSTRUCT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetUpdateRect(HWND hWnd,LPRECT lpRect,WINBOOL bErase)
*/
HB_FUNC( WAGETUPDATERECT )
{
  wa_ret_BOOL(GetUpdateRect(wa_par_HWND(1), wa_par_RECT(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI GetUpdateRgn(HWND hWnd,HRGN hRgn,WINBOOL bErase)
*/
HB_FUNC( WAGETUPDATERGN )
{
  wa_ret_int(GetUpdateRgn(wa_par_HWND(1), wa_par_HRGN(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI SetWindowRgn(HWND hWnd,HRGN hRgn,WINBOOL bRedraw)
*/
HB_FUNC( WASETWINDOWRGN )
{
  wa_ret_int(SetWindowRgn(wa_par_HWND(1), wa_par_HRGN(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI int WINAPI GetWindowRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WAGETWINDOWRGN )
{
  wa_ret_int(GetWindowRgn(wa_par_HWND(1), wa_par_HRGN(2)));
}

/*
WINUSERAPI int WINAPI GetWindowRgnBox(HWND hWnd,LPRECT lprc)
*/
HB_FUNC( WAGETWINDOWRGNBOX )
{
  wa_ret_int(GetWindowRgnBox(wa_par_HWND(1), wa_par_RECT(2)));
}

/*
WINUSERAPI int WINAPI ExcludeUpdateRgn(HDC hDC,HWND hWnd)
*/
HB_FUNC( WAEXCLUDEUPDATERGN )
{
  wa_ret_int(ExcludeUpdateRgn(wa_par_HDC(1), wa_par_HWND(2)));
}

/*
WINUSERAPI WINBOOL WINAPI InvalidateRect(HWND hWnd,CONST RECT *lpRect,WINBOOL bErase)
*/
HB_FUNC( WAINVALIDATERECT )
{
  wa_ret_BOOL(InvalidateRect(wa_par_HWND(1), wa_par_RECT(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI ValidateRect(HWND hWnd,CONST RECT *lpRect)
*/
HB_FUNC( WAVALIDATERECT )
{
  wa_ret_BOOL(ValidateRect(wa_par_HWND(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI InvalidateRgn(HWND hWnd,HRGN hRgn,WINBOOL bErase)
*/
HB_FUNC( WAINVALIDATERGN )
{
  wa_ret_BOOL(InvalidateRgn(wa_par_HWND(1), wa_par_HRGN(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI ValidateRgn(HWND hWnd,HRGN hRgn)
*/
HB_FUNC( WAVALIDATERGN )
{
  wa_ret_BOOL(ValidateRgn(wa_par_HWND(1), wa_par_HRGN(2)));
}

/*
WINUSERAPI WINBOOL WINAPI RedrawWindow(HWND hWnd,CONST RECT *lprcUpdate,HRGN hrgnUpdate,UINT flags)
*/
HB_FUNC( WAREDRAWWINDOW )
{
  wa_ret_BOOL(RedrawWindow(wa_par_HWND(1), wa_par_RECT(2), wa_par_HRGN(3), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI LockWindowUpdate(HWND hWndLock)
*/
HB_FUNC( WALOCKWINDOWUPDATE )
{
  wa_ret_BOOL(LockWindowUpdate(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ScrollWindow(HWND hWnd,int XAmount,int YAmount,CONST RECT *lpRect,CONST RECT *lpClipRect)
*/
HB_FUNC( WASCROLLWINDOW )
{
  wa_ret_BOOL(ScrollWindow(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_RECT(4), wa_par_RECT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI ScrollDC(HDC hDC,int dx,int dy,CONST RECT *lprcScroll,CONST RECT *lprcClip,HRGN hrgnUpdate,LPRECT lprcUpdate)
*/
HB_FUNC( WASCROLLDC )
{
  wa_ret_BOOL(ScrollDC(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_RECT(4), wa_par_RECT(5), wa_par_HRGN(6), wa_par_RECT(7)));
}

/*
WINUSERAPI int WINAPI ScrollWindowEx(HWND hWnd,int dx,int dy,CONST RECT *prcScroll,CONST RECT *prcClip,HRGN hrgnUpdate,LPRECT prcUpdate,UINT flags)
*/
HB_FUNC( WASCROLLWINDOWEX )
{
  wa_ret_int(ScrollWindowEx(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_RECT(4), wa_par_RECT(5), wa_par_HRGN(6), wa_par_RECT(7), wa_par_UINT(8)));
}

/*
WINUSERAPI int WINAPI SetScrollPos(HWND hWnd,int nBar,int nPos,WINBOOL bRedraw)
*/
HB_FUNC( WASETSCROLLPOS )
{
  wa_ret_int(SetScrollPos(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_BOOL(4)));
}

/*
WINUSERAPI int WINAPI GetScrollPos(HWND hWnd,int nBar)
*/
HB_FUNC( WAGETSCROLLPOS )
{
  wa_ret_int(GetScrollPos(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetScrollRange(HWND hWnd,int nBar,int nMinPos,int nMaxPos,WINBOOL bRedraw)
*/
HB_FUNC( WASETSCROLLRANGE )
{
  wa_ret_BOOL(SetScrollRange(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_BOOL(5)));
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollRange(HWND hWnd,int nBar,LPINT lpMinPos,LPINT lpMaxPos)
*/
HB_FUNC( WAGETSCROLLRANGE )
{
  INT MinPos{};
  INT MaxPos{};
  wa_ret_BOOL(GetScrollRange(wa_par_HWND(1), wa_par_int(2), &MinPos, &MaxPos));
  wa_stor_INT(MinPos, 3);
  wa_stor_INT(MaxPos, 4);
}

/*
WINUSERAPI WINBOOL WINAPI ShowScrollBar(HWND hWnd,int wBar,WINBOOL bShow)
*/
HB_FUNC( WASHOWSCROLLBAR )
{
  wa_ret_BOOL(ShowScrollBar(wa_par_HWND(1), wa_par_int(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI EnableScrollBar(HWND hWnd,UINT wSBflags,UINT wArrows)
*/
HB_FUNC( WAENABLESCROLLBAR )
{
  wa_ret_BOOL(EnableScrollBar(wa_par_HWND(1), wa_par_UINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetPropA(HWND hWnd,LPCSTR lpString,HANDLE hData)
*/
#if 0
HB_FUNC( WASETPROPA )
{
  wa_ret_BOOL(SetPropA(wa_par_HWND(1), wa_par_LPCSTR(2), wa_par_HANDLE(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetPropW(HWND hWnd,LPCWSTR lpString,HANDLE hData)
*/
#if 0
HB_FUNC( WASETPROPW )
{
  wa_ret_BOOL(SetPropW(wa_par_HWND(1), wa_par_LPCWSTR(2), wa_par_HANDLE(3)));
}
#endif

HB_FUNC( WASETPROP )
{
  void * str{};
  wa_ret_BOOL(SetProp(wa_par_HWND(1), HB_PARSTR(2, &str, nullptr), wa_par_HANDLE(3)));
  hb_strfree(str);
}

/*
WINUSERAPI HANDLE WINAPI GetPropA(HWND hWnd,LPCSTR lpString)
*/
#if 0
HB_FUNC( WAGETPROPA )
{
  wa_ret_HANDLE(GetPropA(wa_par_HWND(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HANDLE WINAPI GetPropW(HWND hWnd,LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAGETPROPW )
{
  wa_ret_HANDLE(GetPropW(wa_par_HWND(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAGETPROP )
{
  void * str{};
  wa_ret_HANDLE(GetProp(wa_par_HWND(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
}

/*
WINUSERAPI HANDLE WINAPI RemovePropA(HWND hWnd,LPCSTR lpString)
*/
#if 0
HB_FUNC( WAREMOVEPROPA )
{
  wa_ret_HANDLE(RemovePropA(wa_par_HWND(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HANDLE WINAPI RemovePropW(HWND hWnd,LPCWSTR lpString)
*/
#if 0
HB_FUNC( WAREMOVEPROPW )
{
  wa_ret_HANDLE(RemovePropW(wa_par_HWND(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAREMOVEPROP )
{
  void * str{};
  wa_ret_HANDLE(RemoveProp(wa_par_HWND(1), HB_PARSTR(2, &str, nullptr)));
  hb_strfree(str);
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
#if 0
HB_FUNC( WASETWINDOWTEXTA )
{
  wa_ret_BOOL(SetWindowTextA(wa_par_HWND(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetWindowTextW(HWND hWnd,LPCWSTR lpString)
*/
#if 0
HB_FUNC( WASETWINDOWTEXTW )
{
  wa_ret_BOOL(SetWindowTextW(wa_par_HWND(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WASETWINDOWTEXT )
{
  void * str2{};
  wa_ret_BOOL(SetWindowText(wa_par_HWND(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI int WINAPI GetWindowTextA(HWND hWnd,LPSTR lpString,int nMaxCount)
*/
HB_FUNC( WAGETWINDOWTEXTA ) // TODO: fix
{
  wa_ret_int(GetWindowTextA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetWindowTextW(HWND hWnd,LPWSTR lpString,int nMaxCount)
*/
HB_FUNC( WAGETWINDOWTEXTW ) // TODO: fix
{
  wa_ret_int(GetWindowTextW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetWindowTextLengthA(HWND hWnd)
*/
#if 0
HB_FUNC( WAGETWINDOWTEXTLENGTHA )
{
  wa_ret_int(GetWindowTextLengthA(wa_par_HWND(1)));
}
#endif

/*
WINUSERAPI int WINAPI GetWindowTextLengthW(HWND hWnd)
*/
#if 0
HB_FUNC( WAGETWINDOWTEXTLENGTHW )
{
  wa_ret_int(GetWindowTextLengthW(wa_par_HWND(1)));
}
#endif

HB_FUNC( WAGETWINDOWTEXTLENGTH )
{
  wa_ret_int(GetWindowTextLength(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetClientRect(HWND hWnd,LPRECT lpRect)
*/
HB_FUNC( WAGETCLIENTRECT )
{
  wa_ret_BOOL(GetClientRect(wa_par_HWND(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowRect(HWND hWnd,LPRECT lpRect)
*/
HB_FUNC( WAGETWINDOWRECT )
{
  wa_ret_BOOL(GetWindowRect(wa_par_HWND(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI AdjustWindowRect(LPRECT lpRect,DWORD dwStyle,WINBOOL bMenu)
*/
HB_FUNC( WAADJUSTWINDOWRECT )
{
  wa_ret_BOOL(AdjustWindowRect(wa_par_RECT(1), wa_par_DWORD(2), wa_par_BOOL(3)));
}

/*
WINUSERAPI WINBOOL WINAPI AdjustWindowRectEx(LPRECT lpRect,DWORD dwStyle,WINBOOL bMenu,DWORD dwExStyle)
*/
HB_FUNC( WAADJUSTWINDOWRECTEX )
{
  wa_ret_BOOL(AdjustWindowRectEx(wa_par_RECT(1), wa_par_DWORD(2), wa_par_BOOL(3), wa_par_DWORD(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SetWindowContextHelpId(HWND,DWORD)
*/
HB_FUNC( WASETWINDOWCONTEXTHELPID )
{
  wa_ret_BOOL(SetWindowContextHelpId(wa_par_HWND(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI GetWindowContextHelpId(HWND)
*/
HB_FUNC( WAGETWINDOWCONTEXTHELPID )
{
  wa_ret_DWORD(GetWindowContextHelpId(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetMenuContextHelpId(HMENU,DWORD)
*/
HB_FUNC( WASETMENUCONTEXTHELPID )
{
  wa_ret_BOOL(SetMenuContextHelpId(wa_par_HMENU(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI DWORD WINAPI GetMenuContextHelpId(HMENU)
*/
HB_FUNC( WAGETMENUCONTEXTHELPID )
{
  wa_ret_DWORD(GetMenuContextHelpId(wa_par_HMENU(1)));
}

/*
WINUSERAPI int WINAPI MessageBoxA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType)
*/
#if 0
HB_FUNC( WAMESSAGEBOXA )
{
  wa_ret_int(MessageBoxA(wa_par_HWND(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_UINT(4)));
}
#endif

/*
WINUSERAPI int WINAPI MessageBoxW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType)
*/
#if 0
HB_FUNC( WAMESSAGEBOXW )
{
  wa_ret_int(MessageBoxW(wa_par_HWND(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_UINT(4)));
}
#endif

HB_FUNC( WAMESSAGEBOX )
{
  void * str2{};
  void * str3{};
  wa_ret_int(MessageBox(wa_par_HWND(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_UINT(4)));
  hb_strfree(str2);
  hb_strfree(str3);
}

/*
WINUSERAPI int WINAPI MessageBoxExA(HWND hWnd,LPCSTR lpText,LPCSTR lpCaption,UINT uType,WORD wLanguageId)
*/
#if 0
HB_FUNC( WAMESSAGEBOXEXA )
{
  wa_ret_int(MessageBoxExA(wa_par_HWND(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3), wa_par_UINT(4), wa_par_WORD(5)));
}
#endif

/*
WINUSERAPI int WINAPI MessageBoxExW(HWND hWnd,LPCWSTR lpText,LPCWSTR lpCaption,UINT uType,WORD wLanguageId)
*/
#if 0
HB_FUNC( WAMESSAGEBOXEXW )
{
  wa_ret_int(MessageBoxExW(wa_par_HWND(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3), wa_par_UINT(4), wa_par_WORD(5)));
}
#endif

HB_FUNC( WAMESSAGEBOXEX )
{
  void * str2{};
  void * str3{};
  wa_ret_int(MessageBoxEx(wa_par_HWND(1), HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_UINT(4), wa_par_WORD(5)));
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
HB_FUNC( WAMESSAGEBEEP )
{
  wa_ret_BOOL(MessageBeep(wa_par_UINT(1)));
}

/*
WINUSERAPI int WINAPI ShowCursor(WINBOOL bShow)
*/
HB_FUNC( WASHOWCURSOR )
{
  wa_ret_int(ShowCursor(wa_par_BOOL(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetCursorPos(int X,int Y)
*/
HB_FUNC( WASETCURSORPOS )
{
  wa_ret_BOOL(SetCursorPos(wa_par_int(1), wa_par_int(2)));
}

/*
WINUSERAPI HCURSOR WINAPI SetCursor(HCURSOR hCursor)
*/
HB_FUNC( WASETCURSOR )
{
  wa_ret_HCURSOR(SetCursor(wa_par_HCURSOR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetCursorPos(LPPOINT lpPoint)
*/
HB_FUNC( WAGETCURSORPOS )
{
  wa_ret_BOOL(GetCursorPos(wa_par_POINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ClipCursor(CONST RECT *lpRect)
*/
HB_FUNC( WACLIPCURSOR )
{
  wa_ret_BOOL(ClipCursor(wa_par_RECT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetClipCursor(LPRECT lpRect)
*/
HB_FUNC( WAGETCLIPCURSOR )
{
  wa_ret_BOOL(GetClipCursor(wa_par_RECT(1)));
}

/*
WINUSERAPI HCURSOR WINAPI GetCursor(VOID)
*/
HB_FUNC( WAGETCURSOR )
{
  wa_ret_HCURSOR(GetCursor());
}

/*
WINUSERAPI WINBOOL WINAPI CreateCaret(HWND hWnd,HBITMAP hBitmap,int nWidth,int nHeight)
*/
HB_FUNC( WACREATECARET )
{
  wa_ret_BOOL(CreateCaret(wa_par_HWND(1), wa_par_HBITMAP(2), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI UINT WINAPI GetCaretBlinkTime(VOID)
*/
HB_FUNC( WAGETCARETBLINKTIME )
{
  wa_ret_UINT(GetCaretBlinkTime());
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretBlinkTime(UINT uMSeconds)
*/
HB_FUNC( WASETCARETBLINKTIME )
{
  wa_ret_BOOL(SetCaretBlinkTime(wa_par_UINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyCaret(VOID)
*/
HB_FUNC( WADESTROYCARET )
{
  wa_ret_BOOL(DestroyCaret());
}

/*
WINUSERAPI WINBOOL WINAPI HideCaret(HWND hWnd)
*/
HB_FUNC( WAHIDECARET )
{
  wa_ret_BOOL(HideCaret(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ShowCaret(HWND hWnd)
*/
HB_FUNC( WASHOWCARET )
{
  wa_ret_BOOL(ShowCaret(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetCaretPos(int X,int Y)
*/
HB_FUNC( WASETCARETPOS )
{
  wa_ret_BOOL(SetCaretPos(wa_par_int(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetCaretPos(LPPOINT lpPoint)
*/
HB_FUNC( WAGETCARETPOS )
{
  wa_ret_BOOL(GetCaretPos(wa_par_POINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI ClientToScreen(HWND hWnd,LPPOINT lpPoint)
*/
HB_FUNC( WACLIENTTOSCREEN )
{
  wa_ret_BOOL(ClientToScreen(wa_par_HWND(1), wa_par_POINT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ScreenToClient(HWND hWnd,LPPOINT lpPoint)
*/
HB_FUNC( WASCREENTOCLIENT )
{
  wa_ret_BOOL(ScreenToClient(wa_par_HWND(1), wa_par_POINT(2)));
}

/*
WINUSERAPI int WINAPI MapWindowPoints(HWND hWndFrom,HWND hWndTo,LPPOINT lpPoints,UINT cPoints)
*/
HB_FUNC( WAMAPWINDOWPOINTS )
{
  wa_ret_int(MapWindowPoints(wa_par_HWND(1), wa_par_HWND(2), wa_par_POINT(3), wa_par_UINT(4)));
}

/*
WINUSERAPI HWND WINAPI WindowFromPoint(POINT Point)
*/
HB_FUNC( WAWINDOWFROMPOINT )
{
  wa_ret_HWND(WindowFromPoint(*wa_par_POINT(1)));
}

/*
WINUSERAPI HWND WINAPI ChildWindowFromPoint(HWND hWndParent,POINT Point)
*/
HB_FUNC( WACHILDWINDOWFROMPOINT )
{
  wa_ret_HWND(ChildWindowFromPoint(wa_par_HWND(1), *wa_par_POINT(2)));
}

/*
WINUSERAPI HWND WINAPI ChildWindowFromPointEx(HWND hwnd,POINT pt,UINT flags)
*/
HB_FUNC( WACHILDWINDOWFROMPOINTEX )
{
  wa_ret_HWND(ChildWindowFromPointEx(wa_par_HWND(1), *wa_par_POINT(2), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetPhysicalCursorPos (int X, int Y)
*/
HB_FUNC( WASETPHYSICALCURSORPOS )
{
  wa_ret_BOOL(SetPhysicalCursorPos(wa_par_int(1), wa_par_int(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetPhysicalCursorPos (LPPOINT lpPoint)
*/
HB_FUNC( WAGETPHYSICALCURSORPOS )
{
  wa_ret_BOOL(GetPhysicalCursorPos(wa_par_POINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LogicalToPhysicalPoint (HWND hWnd, LPPOINT lpPoint)
*/
HB_FUNC( WALOGICALTOPHYSICALPOINT )
{
  wa_ret_BOOL(LogicalToPhysicalPoint(wa_par_HWND(1), wa_par_POINT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI PhysicalToLogicalPoint (HWND hWnd, LPPOINT lpPoint)
*/
HB_FUNC( WAPHYSICALTOLOGICALPOINT )
{
  wa_ret_BOOL(PhysicalToLogicalPoint(wa_par_HWND(1), wa_par_POINT(2)));
}

/*
WINUSERAPI HWND WINAPI WindowFromPhysicalPoint (POINT Point)
*/
HB_FUNC( WAWINDOWFROMPHYSICALPOINT )
{
  wa_ret_HWND(WindowFromPhysicalPoint(*wa_par_POINT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LogicalToPhysicalPointForPerMonitorDPI (HWND hwnd, LPPOINT lpPoint)
*/
#if 0
HB_FUNC( WALOGICALTOPHYSICALPOINTFORPERMONITORDPI )
{
  wa_ret_BOOL(LogicalToPhysicalPointForPerMonitorDPI(wa_par_HWND(1), wa_par_POINT(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI PhysicalToLogicalPointForPerMonitorDPI (HWND hwnd, LPPOINT lpPoint)
*/
#if 0
HB_FUNC( WAPHYSICALTOLOGICALPOINTFORPERMONITORDPI )
{
  wa_ret_BOOL(PhysicalToLogicalPointForPerMonitorDPI(wa_par_HWND(1), wa_par_POINT(2)));
}
#endif

/*
WINUSERAPI DWORD WINAPI GetSysColor(int nIndex)
*/
HB_FUNC( WAGETSYSCOLOR )
{
  wa_ret_DWORD(GetSysColor(wa_par_int(1)));
}

/*
WINUSERAPI HBRUSH WINAPI GetSysColorBrush(int nIndex)
*/
HB_FUNC( WAGETSYSCOLORBRUSH )
{
  wa_ret_HBRUSH(GetSysColorBrush(wa_par_int(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetSysColors(int cElements,CONST INT *lpaElements,CONST COLORREF *lpaRgbValues)
*/
HB_FUNC( WASETSYSCOLORS )
{
  std::vector<INT> vec1{};
  auto pArray1 = hb_param(2, Harbour::Item::ARRAY);
  if( pArray1 != nullptr )
  {
    const int nLen = hb_arrayLen(pArray1);
    for( auto i = 0; i < nLen; i++ )
    {
      vec1.push_back(static_cast<INT>(hb_arrayGetNI(pArray1, i + 1)));
    }
  }
  std::vector<COLORREF> vec2{};
  auto pArray2 = hb_param(3, Harbour::Item::ARRAY);
  if( pArray2 != nullptr )
  {
    const int nLen = hb_arrayLen(pArray2);
    for( auto i = 0; i < nLen; i++ )
    {
      vec2.push_back(static_cast<COLORREF>(hb_arrayGetNL(pArray2, i + 1)));
    }
  }
  wa_ret_BOOL(SetSysColors(wa_par_int(1), vec1.data(), vec2.data()));
}

/*
WINUSERAPI WINBOOL WINAPI DrawFocusRect(HDC hDC,CONST RECT *lprc)
*/
HB_FUNC( WADRAWFOCUSRECT )
{
  wa_ret_BOOL(DrawFocusRect(wa_par_HDC(1), wa_par_RECT(2)));
}

/*
WINUSERAPI int WINAPI FillRect(HDC hDC,CONST RECT *lprc,HBRUSH hbr)
*/
HB_FUNC( WAFILLRECT )
{
  wa_ret_int(FillRect(wa_par_HDC(1), wa_par_RECT(2), wa_par_HBRUSH(3)));
}

/*
WINUSERAPI int WINAPI FrameRect(HDC hDC,CONST RECT *lprc,HBRUSH hbr)
*/
HB_FUNC( WAFRAMERECT )
{
  wa_ret_int(FrameRect(wa_par_HDC(1), wa_par_RECT(2), wa_par_HBRUSH(3)));
}

/*
WINUSERAPI WINBOOL WINAPI InvertRect(HDC hDC,CONST RECT *lprc)
*/
HB_FUNC( WAINVERTRECT )
{
  wa_ret_BOOL(InvertRect(wa_par_HDC(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI SetRect(LPRECT lprc,int xLeft,int yTop,int xRight,int yBottom)
*/
HB_FUNC( WASETRECT )
{
  wa_ret_BOOL(SetRect(wa_par_RECT(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5)));
}

/*
WINUSERAPI WINBOOL WINAPI SetRectEmpty(LPRECT lprc)
*/
HB_FUNC( WASETRECTEMPTY )
{
  wa_ret_BOOL(SetRectEmpty(wa_par_RECT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI CopyRect(LPRECT lprcDst,CONST RECT *lprcSrc)
*/
HB_FUNC( WACOPYRECT )
{
  wa_ret_BOOL(CopyRect(wa_par_RECT(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI InflateRect(LPRECT lprc,int dx,int dy)
*/
HB_FUNC( WAINFLATERECT )
{
  wa_ret_BOOL(InflateRect(wa_par_RECT(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINUSERAPI WINBOOL WINAPI IntersectRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/
HB_FUNC( WAINTERSECTRECT )
{
  wa_ret_BOOL(IntersectRect(wa_par_RECT(1), wa_par_RECT(2), wa_par_RECT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI UnionRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/
HB_FUNC( WAUNIONRECT )
{
  wa_ret_BOOL(UnionRect(wa_par_RECT(1), wa_par_RECT(2), wa_par_RECT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SubtractRect(LPRECT lprcDst,CONST RECT *lprcSrc1,CONST RECT *lprcSrc2)
*/
HB_FUNC( WASUBTRACTRECT )
{
  wa_ret_BOOL(SubtractRect(wa_par_RECT(1), wa_par_RECT(2), wa_par_RECT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI OffsetRect(LPRECT lprc,int dx,int dy)
*/
HB_FUNC( WAOFFSETRECT )
{
  wa_ret_BOOL(OffsetRect(wa_par_RECT(1), wa_par_int(2), wa_par_int(3)));
}

/*
WINUSERAPI WINBOOL WINAPI IsRectEmpty(CONST RECT *lprc)
*/
HB_FUNC( WAISRECTEMPTY )
{
  wa_ret_BOOL(IsRectEmpty(wa_par_RECT(1)));
}

/*
WINUSERAPI WINBOOL WINAPI EqualRect(CONST RECT *lprc1,CONST RECT *lprc2)
*/
HB_FUNC( WAEQUALRECT )
{
  wa_ret_BOOL(EqualRect(wa_par_RECT(1), wa_par_RECT(2)));
}

/*
WINUSERAPI WINBOOL WINAPI PtInRect(CONST RECT *lprc,POINT pt)
*/
HB_FUNC( WAPTINRECT )
{
  wa_ret_BOOL(PtInRect(wa_par_RECT(1), *wa_par_POINT(2)));
}

/*
WINUSERAPI WORD WINAPI GetWindowWord(HWND hWnd,int nIndex)
*/
HB_FUNC( WAGETWINDOWWORD )
{
  wa_ret_WORD(GetWindowWord(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WORD WINAPI SetWindowWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WASETWINDOWWORD )
{
  wa_ret_WORD(SetWindowWord(wa_par_HWND(1), wa_par_int(2), wa_par_WORD(3)));
}

/*
WINUSERAPI LONG WINAPI GetWindowLongA(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETWINDOWLONGA )
{
  wa_ret_LONG(GetWindowLongA(wa_par_HWND(1), wa_par_int(2)));
}
#endif

/*
WINUSERAPI LONG WINAPI GetWindowLongW(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETWINDOWLONGW )
{
  wa_ret_LONG(GetWindowLongW(wa_par_HWND(1), wa_par_int(2)));
}
#endif

HB_FUNC( WAGETWINDOWLONG )
{
  wa_ret_LONG(GetWindowLong(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI LONG WINAPI SetWindowLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
#if 0
HB_FUNC( WASETWINDOWLONGA )
{
  wa_ret_LONG(SetWindowLongA(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}
#endif

/*
WINUSERAPI LONG WINAPI SetWindowLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
#if 0
HB_FUNC( WASETWINDOWLONGW )
{
  wa_ret_LONG(SetWindowLongW(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}
#endif

HB_FUNC( WASETWINDOWLONG )
{
  wa_ret_LONG(SetWindowLong(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}

/*
WINUSERAPI LONG_PTR WINAPI GetWindowLongPtrA(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETWINDOWLONGPTRA )
{
  wa_ret_LONG_PTR(GetWindowLongPtrA(wa_par_HWND(1), wa_par_int(2)));
}
#endif

/*
WINUSERAPI LONG_PTR WINAPI GetWindowLongPtrW(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETWINDOWLONGPTRW )
{
  wa_ret_LONG_PTR(GetWindowLongPtrW(wa_par_HWND(1), wa_par_int(2)));
}
#endif

HB_FUNC( WAGETWINDOWLONGPTR )
{
  wa_ret_LONG_PTR(GetWindowLongPtr(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrA(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/
#if 0
HB_FUNC( WASETWINDOWLONGPTRA )
{
  wa_ret_LONG_PTR(SetWindowLongPtrA(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}
#endif

/*
WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrW(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/
#if 0
HB_FUNC( WASETWINDOWLONGPTRW )
{
  wa_ret_LONG_PTR(SetWindowLongPtrW(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}
#endif

HB_FUNC( WASETWINDOWLONGPTR )
{
  wa_ret_LONG_PTR(SetWindowLongPtr(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}

/*
WINUSERAPI WORD WINAPI GetClassWord(HWND hWnd,int nIndex)
*/
HB_FUNC( WAGETCLASSWORD )
{
  wa_ret_WORD(GetClassWord(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI WORD WINAPI SetClassWord(HWND hWnd,int nIndex,WORD wNewWord)
*/
HB_FUNC( WASETCLASSWORD )
{
  wa_ret_WORD(SetClassWord(wa_par_HWND(1), wa_par_int(2), wa_par_WORD(3)));
}

/*
WINUSERAPI DWORD WINAPI GetClassLongA(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETCLASSLONGA )
{
  wa_ret_DWORD(GetClassLongA(wa_par_HWND(1), wa_par_int(2)));
}
#endif

/*
WINUSERAPI DWORD WINAPI GetClassLongW(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETCLASSLONGW )
{
  wa_ret_DWORD(GetClassLongW(wa_par_HWND(1), wa_par_int(2)));
}
#endif

HB_FUNC( WAGETCLASSLONG )
{
  wa_ret_DWORD(GetClassLong(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI DWORD WINAPI SetClassLongA(HWND hWnd,int nIndex,LONG dwNewLong)
*/
#if 0
HB_FUNC( WASETCLASSLONGA )
{
  wa_ret_DWORD(SetClassLongA(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}
#endif

/*
WINUSERAPI DWORD WINAPI SetClassLongW(HWND hWnd,int nIndex,LONG dwNewLong)
*/
#if 0
HB_FUNC( WASETCLASSLONGW )
{
  wa_ret_DWORD(SetClassLongW(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}
#endif

HB_FUNC( WASETCLASSLONG )
{
  wa_ret_DWORD(SetClassLong(wa_par_HWND(1), wa_par_int(2), wa_par_LONG(3)));
}

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrA(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETCLASSLONGPTRA )
{
  wa_ret_ULONG_PTR(GetClassLongPtrA(wa_par_HWND(1), wa_par_int(2)));
}
#endif

/*
WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrW(HWND hWnd,int nIndex)
*/
#if 0
HB_FUNC( WAGETCLASSLONGPTRW )
{
  wa_ret_ULONG_PTR(GetClassLongPtrW(wa_par_HWND(1), wa_par_int(2)));
}
#endif

HB_FUNC( WAGETCLASSLONGPTR )
{
  wa_ret_ULONG_PTR(GetClassLongPtr(wa_par_HWND(1), wa_par_int(2)));
}

/*
WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrA(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/
#if 0
HB_FUNC( WASETCLASSLONGPTRA )
{
  wa_ret_ULONG_PTR(SetClassLongPtrA(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}
#endif

/*
WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrW(HWND hWnd,int nIndex,LONG_PTR dwNewLong)
*/
#if 0
HB_FUNC( WASETCLASSLONGPTRW )
{
  wa_ret_ULONG_PTR(SetClassLongPtrW(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}
#endif

HB_FUNC( WASETCLASSLONGPTR )
{
  wa_ret_ULONG_PTR(SetClassLongPtr(wa_par_HWND(1), wa_par_int(2), wa_par_LONG_PTR(3)));
}

/*
WINUSERAPI WINBOOL WINAPI GetProcessDefaultLayout(DWORD *pdwDefaultLayout)
*/
HB_FUNC( WAGETPROCESSDEFAULTLAYOUT )
{
  DWORD dwDefaultLayout{};
  wa_ret_BOOL(GetProcessDefaultLayout(&dwDefaultLayout));
  wa_stor_DWORD(dwDefaultLayout, 1);
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessDefaultLayout(DWORD dwDefaultLayout)
*/
HB_FUNC( WASETPROCESSDEFAULTLAYOUT )
{
  wa_ret_BOOL(SetProcessDefaultLayout(wa_par_DWORD(1)));
}

/*
WINUSERAPI HWND WINAPI GetDesktopWindow(VOID)
*/
HB_FUNC( WAGETDESKTOPWINDOW )
{
  wa_ret_HWND(GetDesktopWindow());
}

/*
WINUSERAPI HWND WINAPI GetParent(HWND hWnd)
*/
HB_FUNC( WAGETPARENT )
{
  wa_ret_HWND(GetParent(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI SetParent(HWND hWndChild,HWND hWndNewParent)
*/
HB_FUNC( WASETPARENT )
{
  wa_ret_HWND(SetParent(wa_par_HWND(1), wa_par_HWND(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumChildWindows(HWND hWndParent,WNDENUMPROC lpEnumFunc,LPARAM lParam)
*/

/*
WINUSERAPI HWND WINAPI FindWindowA(LPCSTR lpClassName,LPCSTR lpWindowName)
*/
#if 0
HB_FUNC( WAFINDWINDOWA )
{
  wa_ret_HWND(FindWindowA(wa_par_LPCSTR(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HWND WINAPI FindWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName)
*/
#if 0
HB_FUNC( WAFINDWINDOWW )
{
  wa_ret_HWND(FindWindowW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WAFINDWINDOW )
{
  void * str1{};
  void * str2{};
  wa_ret_HWND(FindWindow(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINUSERAPI HWND WINAPI FindWindowExA(HWND hWndParent,HWND hWndChildAfter,LPCSTR lpszClass,LPCSTR lpszWindow)
*/
#if 0
HB_FUNC( WAFINDWINDOWEXA )
{
  wa_ret_HWND(FindWindowExA(wa_par_HWND(1), wa_par_HWND(2), wa_par_LPCSTR(3), wa_par_LPCSTR(4)));
}
#endif

/*
WINUSERAPI HWND WINAPI FindWindowExW(HWND hWndParent,HWND hWndChildAfter,LPCWSTR lpszClass,LPCWSTR lpszWindow)
*/
#if 0
HB_FUNC( WAFINDWINDOWEXW )
{
  wa_ret_HWND(FindWindowExW(wa_par_HWND(1), wa_par_HWND(2), wa_par_LPCWSTR(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC( WAFINDWINDOWEX )
{
  void * str1{};
  void * str2{};
  wa_ret_HWND(FindWindowEx(wa_par_HWND(1), wa_par_HWND(2), HB_PARSTR(3, &str1, nullptr), HB_PARSTR(4, &str2, nullptr)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINUSERAPI HWND WINAPI GetShellWindow(VOID)
*/
HB_FUNC( WAGETSHELLWINDOW )
{
  wa_ret_HWND(GetShellWindow());
}

/*
WINUSERAPI WINBOOL WINAPI RegisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WAREGISTERSHELLHOOKWINDOW )
{
  wa_ret_BOOL(RegisterShellHookWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI DeregisterShellHookWindow(HWND hwnd)
*/
HB_FUNC( WADEREGISTERSHELLHOOKWINDOW )
{
  wa_ret_BOOL(DeregisterShellHookWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumWindows(WNDENUMPROC lpEnumFunc,LPARAM lParam)
*/
HB_FUNC( WAENUMWINDOWS )
{
  wa_ret_BOOL(EnumWindows(wa_par_WNDENUMPROC(1), wa_par_LPARAM(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumThreadWindows(DWORD dwThreadId,WNDENUMPROC lpfn,LPARAM lParam)
*/

/*
WINUSERAPI int WINAPI GetClassNameA(HWND hWnd,LPSTR lpClassName,int nMaxCount)
*/
HB_FUNC( WAGETCLASSNAMEA ) // TODO: fix
{
  wa_ret_int(GetClassNameA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3)));
}

/*
WINUSERAPI int WINAPI GetClassNameW(HWND hWnd,LPWSTR lpClassName,int nMaxCount)
*/
HB_FUNC( WAGETCLASSNAMEW ) // TODO: fix
{
  wa_ret_int(GetClassNameW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}

/*
WINUSERAPI HWND WINAPI GetTopWindow(HWND hWnd)
*/
HB_FUNC( WAGETTOPWINDOW )
{
  wa_ret_HWND(GetTopWindow(wa_par_HWND(1)));
}

/*
WINUSERAPI DWORD WINAPI GetWindowThreadProcessId(HWND hWnd,LPDWORD lpdwProcessId)
*/
HB_FUNC( WAGETWINDOWTHREADPROCESSID )
{
  DWORD dwProcessId{};
  wa_ret_DWORD(GetWindowThreadProcessId(wa_par_HWND(1), &dwProcessId));
  wa_stor_DWORD(dwProcessId, 2);
}

/*
WINUSERAPI WINBOOL WINAPI IsGUIThread(WINBOOL bConvert)
*/
HB_FUNC( WAISGUITHREAD )
{
  wa_ret_BOOL(IsGUIThread(wa_par_BOOL(1)));
}

/*
WINUSERAPI HWND WINAPI GetLastActivePopup(HWND hWnd)
*/
HB_FUNC( WAGETLASTACTIVEPOPUP )
{
  wa_ret_HWND(GetLastActivePopup(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI GetWindow(HWND hWnd,UINT uCmd)
*/
HB_FUNC( WAGETWINDOW )
{
  wa_ret_HWND(GetWindow(wa_par_HWND(1), wa_par_UINT(2)));
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
HB_FUNC( WACHECKMENURADIOITEM )
{
  wa_ret_BOOL(CheckMenuRadioItem(wa_par_HMENU(1), wa_par_UINT(2), wa_par_UINT(3), wa_par_UINT(4), wa_par_UINT(5)));
}

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapA(HINSTANCE hInstance,LPCSTR lpBitmapName)
*/
#if 0
HB_FUNC( WALOADBITMAPA )
{
  wa_ret_HBITMAP(LoadBitmapA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HBITMAP WINAPI LoadBitmapW(HINSTANCE hInstance,LPCWSTR lpBitmapName)
*/
#if 0
HB_FUNC( WALOADBITMAPW )
{
  wa_ret_HBITMAP(LoadBitmapW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALOADBITMAP )
{
  void * str2{};
  wa_ret_HBITMAP(LoadBitmap(wa_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorA(HINSTANCE hInstance,LPCSTR lpCursorName)
*/
#if 0
HB_FUNC( WALOADCURSORA )
{
  wa_ret_HCURSOR(LoadCursorA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HCURSOR WINAPI LoadCursorW(HINSTANCE hInstance,LPCWSTR lpCursorName)
*/
#if 0
HB_FUNC( WALOADCURSORW )
{
  wa_ret_HCURSOR(LoadCursorW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALOADCURSOR )
{
  void * str2{};
  wa_ret_HCURSOR(LoadCursor(wa_par_HINSTANCE(1), HB_ISCHAR(2) ? HB_PARSTR(2, &str2, nullptr) : MAKEINTRESOURCE(hb_parni(2))));
  hb_strfree(str2);
}

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileA(LPCSTR lpFileName)
*/
#if 0
HB_FUNC( WALOADCURSORFROMFILEA )
{
  wa_ret_HCURSOR(LoadCursorFromFileA(wa_par_LPCSTR(1)));
}
#endif

/*
WINUSERAPI HCURSOR WINAPI LoadCursorFromFileW(LPCWSTR lpFileName)
*/
#if 0
HB_FUNC( WALOADCURSORFROMFILEW )
{
  wa_ret_HCURSOR(LoadCursorFromFileW(wa_par_LPCWSTR(1)));
}
#endif

HB_FUNC( WALOADCURSORFROMFILE )
{
  void * str1{};
  wa_ret_HCURSOR(LoadCursorFromFile(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINUSERAPI HCURSOR WINAPI CreateCursor(HINSTANCE hInst,int xHotSpot,int yHotSpot,int nWidth,int nHeight,CONST VOID *pvANDPlane,CONST VOID *pvXORPlane)
*/
HB_FUNC( WACREATECURSOR ) // TODO: revise
{
  wa_ret_HCURSOR(CreateCursor(wa_par_HINSTANCE(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5), static_cast<CONST VOID *>(hb_parc(6)), static_cast<CONST VOID *>(hb_parc(7))));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyCursor(HCURSOR hCursor)
*/
HB_FUNC( WADESTROYCURSOR )
{
  wa_ret_BOOL(DestroyCursor(wa_par_HCURSOR(1)));
}

/*
WINUSERAPI WINBOOL WINAPI SetSystemCursor(HCURSOR hcur,DWORD id)
*/
HB_FUNC( WASETSYSTEMCURSOR )
{
  wa_ret_BOOL(SetSystemCursor(wa_par_HCURSOR(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI HICON WINAPI LoadIconA(HINSTANCE hInstance,LPCSTR lpIconName)
*/
#if 0
HB_FUNC( WALOADICONA )
{
  wa_ret_HICON(LoadIconA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2)));
}
#endif

/*
WINUSERAPI HICON WINAPI LoadIconW(HINSTANCE hInstance,LPCWSTR lpIconName)
*/
#if 0
HB_FUNC( WALOADICONW )
{
  wa_ret_HICON(LoadIconW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2)));
}
#endif

HB_FUNC( WALOADICON )
{
  void * str2{};
  wa_ret_HICON(LoadIcon(wa_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr)));
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
HB_FUNC( WACREATEICON ) // TODO: revise
{
  wa_ret_HICON(CreateIcon(wa_par_HINSTANCE(1), wa_par_int(2), wa_par_int(3), wa_par_BYTE(4), wa_par_BYTE(5), (CONST BYTE *) hb_parc(6), (CONST BYTE *) hb_parc(7)));
}

/*
WINUSERAPI WINBOOL WINAPI DestroyIcon(HICON hIcon)
*/
HB_FUNC( WADESTROYICON )
{
  wa_ret_BOOL(DestroyIcon(wa_par_HICON(1)));
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
#if 0
HB_FUNC( WALOADIMAGEA )
{
  wa_ret_HANDLE(LoadImageA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_UINT(3), wa_par_int(4), wa_par_int(5), wa_par_UINT(6)));
}
#endif

/*
WINUSERAPI HANDLE WINAPI LoadImageW(HINSTANCE hInst,LPCWSTR name,UINT type,int cx,int cy,UINT fuLoad)
*/
#if 0
HB_FUNC( WALOADIMAGEW )
{
  wa_ret_HANDLE(LoadImageW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_UINT(3), wa_par_int(4), wa_par_int(5), wa_par_UINT(6)));
}
#endif

HB_FUNC( WALOADIMAGE )
{
  void * str2{};
  wa_ret_HANDLE(LoadImage(wa_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr), wa_par_UINT(3), wa_par_int(4), wa_par_int(5), wa_par_UINT(6)));
  hb_strfree(str2);
}

/*
WINUSERAPI HANDLE WINAPI CopyImage(HANDLE h,UINT type,int cx,int cy,UINT flags)
*/
HB_FUNC( WACOPYIMAGE )
{
  wa_ret_HANDLE(CopyImage(wa_par_HANDLE(1), wa_par_UINT(2), wa_par_int(3), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DrawIconEx(HDC hdc,int xLeft,int yTop,HICON hIcon,int cxWidth,int cyWidth,UINT istepIfAniCur,HBRUSH hbrFlickerFreeDraw,UINT diFlags)
*/
HB_FUNC( WADRAWICONEX )
{
  wa_ret_BOOL(DrawIconEx(wa_par_HDC(1), wa_par_int(2), wa_par_int(3), wa_par_HICON(4), wa_par_int(5), wa_par_int(6), wa_par_UINT(7), wa_par_HBRUSH(8), wa_par_UINT(9)));
}

/*
WINUSERAPI HICON WINAPI CreateIconIndirect(PICONINFO piconinfo)
*/
HB_FUNC( WACREATEICONINDIRECT )
{
  wa_ret_HICON(CreateIconIndirect(wa_par_ICONINFO(1)));
}

/*
WINUSERAPI HICON WINAPI CopyIcon(HICON hIcon)
*/
HB_FUNC( WACOPYICON )
{
  wa_ret_HICON(CopyIcon(wa_par_HICON(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetIconInfo(HICON hIcon,PICONINFO piconinfo)
*/
HB_FUNC( WAGETICONINFO )
{
  wa_ret_BOOL(GetIconInfo(wa_par_HICON(1), wa_par_ICONINFO(2)));
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
#if 0
HB_FUNC( WALOADSTRINGA )
{
  wa_ret_int(LoadStringA(wa_par_HINSTANCE(1), wa_par_UINT(2), const_cast<LPSTR>(hb_parc(3)), wa_par_int(4)));
}
#endif

/*
WINUSERAPI int WINAPI LoadStringW (HINSTANCE hInstance, UINT uID, LPWSTR lpBuffer, int cchBufferMax)
*/
#if 0
HB_FUNC( WALOADSTRINGW )
{
  wa_ret_int(LoadStringW(wa_par_HINSTANCE(1), wa_par_UINT(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_int(4)));
}
#endif

HB_FUNC( WALOADSTRING ) // TODO: fix
{
  wa_ret_int(LoadString(wa_par_HINSTANCE(1), wa_par_UINT(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageA(HWND hDlg,LPMSG lpMsg)
*/
#if 0
HB_FUNC( WAISDIALOGMESSAGEA )
{
  wa_ret_BOOL(IsDialogMessageA(wa_par_HWND(1), wa_par_MSG(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI IsDialogMessageW(HWND hDlg,LPMSG lpMsg)
*/
#if 0
HB_FUNC( WAISDIALOGMESSAGEW )
{
  wa_ret_BOOL(IsDialogMessageW(wa_par_HWND(1), wa_par_MSG(2)));
}
#endif

HB_FUNC( WAISDIALOGMESSAGE )
{
  wa_ret_BOOL(IsDialogMessage(wa_par_HWND(1), wa_par_MSG(2)));
}

/*
WINUSERAPI WINBOOL WINAPI MapDialogRect(HWND hDlg,LPRECT lpRect)
*/
HB_FUNC( WAMAPDIALOGRECT )
{
  wa_ret_BOOL(MapDialogRect(wa_par_HWND(1), wa_par_RECT(2)));
}

/*
WINUSERAPI int WINAPI DlgDirListA(HWND hDlg,LPSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WADLGDIRLISTA ) // TODO: fix
{
  wa_ret_int(DlgDirListA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI DlgDirListW(HWND hDlg,LPWSTR lpPathSpec,int nIDListBox,int nIDStaticPath,UINT uFileType)
*/
HB_FUNC( WADLGDIRLISTW ) // TODO: fix
{
  wa_ret_int(DlgDirListW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExA(HWND hwndDlg,LPSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WADLGDIRSELECTEXA ) // TODO: fix
{
  wa_ret_BOOL(DlgDirSelectExA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectExW(HWND hwndDlg,LPWSTR lpString,int chCount,int idListBox)
*/
HB_FUNC( WADLGDIRSELECTEXW ) // TODO: fix
{
  wa_ret_BOOL(DlgDirSelectExW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxA(HWND hDlg,LPSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WADLGDIRLISTCOMBOBOXA ) // TODO: fix
{
  wa_ret_int(DlgDirListComboBoxA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI int WINAPI DlgDirListComboBoxW(HWND hDlg,LPWSTR lpPathSpec,int nIDComboBox,int nIDStaticPath,UINT uFiletype)
*/
HB_FUNC( WADLGDIRLISTCOMBOBOXW ) // TODO: fix
{
  wa_ret_int(DlgDirListComboBoxW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3), wa_par_int(4), wa_par_UINT(5)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExA(HWND hwndDlg,LPSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WADLGDIRSELECTCOMBOBOXEXA ) // TODO: fix
{
  wa_ret_BOOL(DlgDirSelectComboBoxExA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI WINBOOL WINAPI DlgDirSelectComboBoxExW(HWND hwndDlg,LPWSTR lpString,int cchOut,int idComboBox)
*/
HB_FUNC( WADLGDIRSELECTCOMBOBOXEXW ) // TODO: fix
{
  wa_ret_BOOL(DlgDirSelectComboBoxExW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3), wa_par_int(4)));
}

/*
WINUSERAPI int WINAPI SetScrollInfo(HWND hwnd,int nBar,LPCSCROLLINFO lpsi,WINBOOL redraw)
*/
HB_FUNC( WASETSCROLLINFO )
{
  wa_ret_int(SetScrollInfo(wa_par_HWND(1), wa_par_int(2), wa_par_SCROLLINFO(3), wa_par_BOOL(4)));
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollInfo(HWND hwnd,int nBar,LPSCROLLINFO lpsi)
*/
HB_FUNC( WAGETSCROLLINFO )
{
  wa_ret_BOOL(GetScrollInfo(wa_par_HWND(1), wa_par_int(2), wa_par_SCROLLINFO(3)));
}

/*
WINUSERAPI LRESULT WINAPI DefFrameProcA(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFFRAMEPROCA )
{
  wa_ret_LRESULT(DefFrameProcA(wa_par_HWND(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI DefFrameProcW(HWND hWnd,HWND hWndMDIClient,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFFRAMEPROCW )
{
  wa_ret_LRESULT(DefFrameProcW(wa_par_HWND(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}
#endif

HB_FUNC( WADEFFRAMEPROC )
{
  wa_ret_LRESULT(DefFrameProc(wa_par_HWND(1), wa_par_HWND(2), wa_par_UINT(3), wa_par_WPARAM(4), wa_par_LPARAM(5)));
}

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcA(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFMDICHILDPROCA )
{
  wa_ret_LRESULT(DefMDIChildProcA(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

/*
WINUSERAPI LRESULT WINAPI DefMDIChildProcW(HWND hWnd,UINT uMsg,WPARAM wParam,LPARAM lParam)
*/
#if 0
HB_FUNC( WADEFMDICHILDPROCW )
{
  wa_ret_LRESULT(DefMDIChildProcW(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}
#endif

HB_FUNC( WADEFMDICHILDPROC )
{
  wa_ret_LRESULT(DefMDIChildProc(wa_par_HWND(1), wa_par_UINT(2), wa_par_WPARAM(3), wa_par_LPARAM(4)));
}

/*
WINUSERAPI WINBOOL WINAPI TranslateMDISysAccel(HWND hWndClient,LPMSG lpMsg)
*/
HB_FUNC( WATRANSLATEMDISYSACCEL )
{
  wa_ret_BOOL(TranslateMDISysAccel(wa_par_HWND(1), wa_par_MSG(2)));
}

/*
WINUSERAPI UINT WINAPI ArrangeIconicWindows(HWND hWnd)
*/
HB_FUNC( WAARRANGEICONICWINDOWS )
{
  wa_ret_UINT(ArrangeIconicWindows(wa_par_HWND(1)));
}

/*
WINUSERAPI HWND WINAPI CreateMDIWindowA(LPCSTR lpClassName,LPCSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
#if 0
HB_FUNC( WACREATEMDIWINDOWA )
{
  wa_ret_HWND(CreateMDIWindowA(wa_par_LPCSTR(1), wa_par_LPCSTR(2), wa_par_DWORD(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_HWND(8), wa_par_HINSTANCE(9), wa_par_LPARAM(10)));
}
#endif

/*
WINUSERAPI HWND WINAPI CreateMDIWindowW(LPCWSTR lpClassName,LPCWSTR lpWindowName,DWORD dwStyle,int X,int Y,int nWidth,int nHeight,HWND hWndParent,HINSTANCE hInstance,LPARAM lParam)
*/
#if 0
HB_FUNC( WACREATEMDIWINDOWW )
{
  wa_ret_HWND(CreateMDIWindowW(wa_par_LPCWSTR(1), wa_par_LPCWSTR(2), wa_par_DWORD(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_HWND(8), wa_par_HINSTANCE(9), wa_par_LPARAM(10)));
}
#endif

HB_FUNC( WACREATEMDIWINDOW )
{
  void * str1{};
  void * str2{};
  wa_ret_HWND(CreateMDIWindow(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_DWORD(3), wa_par_int(4), wa_par_int(5), wa_par_int(6), wa_par_int(7), wa_par_HWND(8), wa_par_HINSTANCE(9), wa_par_LPARAM(10)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINUSERAPI WORD WINAPI TileWindows(HWND hwndParent,UINT wHow,CONST RECT *lpRect,UINT cKids,const HWND *lpKids)
*/
HB_FUNC( WATILEWINDOWS )
{
  std::vector<HWND> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<HWND>(hb_arrayGetPtr(pArray, i + 1)));
    }
  }
  wa_ret_WORD(TileWindows(wa_par_HWND(1), wa_par_UINT(2), wa_par_RECT(3), wa_par_UINT(4), vec.data()));
}

/*
WINUSERAPI WORD WINAPI CascadeWindows(HWND hwndParent,UINT wHow,CONST RECT *lpRect,UINT cKids,const HWND *lpKids)
*/
HB_FUNC( WACASCADEWINDOWS )
{
  std::vector<HWND> vec{};
  auto pArray = hb_param(5, Harbour::Item::ARRAY);
  if( pArray != nullptr )
  {
    const int nLen = hb_arrayLen(pArray);
    for( auto i = 0; i < nLen; i++ )
    {
      vec.push_back(static_cast<HWND>(hb_arrayGetPtr(pArray, i + 1)));
    }
  }
  wa_ret_WORD(CascadeWindows(wa_par_HWND(1), wa_par_UINT(2), wa_par_RECT(3), wa_par_UINT(4), vec.data()));
}

/*
WINUSERAPI WINBOOL WINAPI WinHelpA(HWND hWndMain,LPCSTR lpszHelp,UINT uCommand,ULONG_PTR dwData)
*/
#if 0
HB_FUNC( WAWINHELPA )
{
  wa_ret_BOOL(WinHelpA(wa_par_HWND(1), wa_par_LPCSTR(2), wa_par_UINT(3), wa_par_ULONG_PTR(4)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI WinHelpW(HWND hWndMain,LPCWSTR lpszHelp,UINT uCommand,ULONG_PTR dwData)
*/
#if 0
HB_FUNC( WAWINHELPW )
{
  wa_ret_BOOL(WinHelpW(wa_par_HWND(1), wa_par_LPCWSTR(2), wa_par_UINT(3), wa_par_ULONG_PTR(4)));
}
#endif

HB_FUNC( WAWINHELP )
{
  void * str{};
  wa_ret_BOOL(WinHelp(wa_par_HWND(1), HB_PARSTR(2, &str, nullptr), wa_par_UINT(3), wa_par_ULONG_PTR(4)));
  hb_strfree(str);
}

/*
WINUSERAPI DWORD WINAPI GetGuiResources(HANDLE hProcess,DWORD uiFlags)
*/
HB_FUNC( WAGETGUIRESOURCES )
{
  wa_ret_DWORD(GetGuiResources(wa_par_HANDLE(1), wa_par_DWORD(2)));
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
HB_FUNC( WASYSTEMPARAMETERSINFOA ) // TODO: fix
{
  wa_ret_BOOL(SystemParametersInfoA(wa_par_UINT(1), wa_par_UINT(2), static_cast<PVOID>(hb_parptr(3)), wa_par_UINT(4)));
}

/*
WINUSERAPI WINBOOL WINAPI SystemParametersInfoW(UINT uiAction,UINT uiParam,PVOID pvParam,UINT fWinIni)
*/
HB_FUNC( WASYSTEMPARAMETERSINFOW ) // TODO: fix
{
  wa_ret_BOOL(SystemParametersInfoW(wa_par_UINT(1), wa_par_UINT(2), static_cast<PVOID>(hb_parptr(3)), wa_par_UINT(4)));
}

/*
WINUSERAPI VOID WINAPI SetDebugErrorLevel (DWORD dwLevel)
*/
HB_FUNC( WASETDEBUGERRORLEVEL )
{
  SetDebugErrorLevel(wa_par_DWORD(1));
}

/*
WINUSERAPI VOID WINAPI SetLastErrorEx (DWORD dwErrCode, DWORD dwType)
*/
HB_FUNC( WASETLASTERROREX )
{
  SetLastErrorEx(wa_par_DWORD(1), wa_par_DWORD(2));
}

/*
WINUSERAPI int WINAPI InternalGetWindowText (HWND hWnd, LPWSTR pString, int cchMaxCount)
*/
HB_FUNC( WAINTERNALGETWINDOWTEXT )
{
  wa_ret_int(InternalGetWindowText(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_int(3)));
}

/*
WINUSERAPI WINBOOL WINAPI CancelShutdown (VOID)
*/
HB_FUNC( WACANCELSHUTDOWN )
{
  wa_ret_BOOL(CancelShutdown());
}

/*
WINUSERAPI HMONITOR WINAPI MonitorFromPoint(POINT pt,DWORD dwFlags)
*/
HB_FUNC( WAMONITORFROMPOINT )
{
  wa_ret_HMONITOR(MonitorFromPoint(*wa_par_POINT(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI HMONITOR WINAPI MonitorFromRect(LPCRECT lprc,DWORD dwFlags)
*/
HB_FUNC( WAMONITORFROMRECT )
{
  wa_ret_HMONITOR(MonitorFromRect(wa_par_RECT(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI HMONITOR WINAPI MonitorFromWindow(HWND hwnd,DWORD dwFlags)
*/
HB_FUNC( WAMONITORFROMWINDOW )
{
  wa_ret_HMONITOR(MonitorFromWindow(wa_par_HWND(1), wa_par_DWORD(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EndTask (HWND hWnd, WINBOOL fShutDown, WINBOOL fForce)
*/
#if 0
HB_FUNC( WAENDTASK )
{
  wa_ret_BOOL(EndTask(wa_par_HWND(1), wa_par_BOOL(2), wa_par_BOOL(3)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SoundSentry (VOID)
*/
HB_FUNC( WASOUNDSENTRY )
{
  wa_ret_BOOL(SoundSentry());
}

/*
WINUSERAPI WINBOOL WINAPI GetMonitorInfoA(HMONITOR hMonitor,LPMONITORINFO lpmi)
*/
#if 0
HB_FUNC( WAGETMONITORINFOA )
{
  wa_ret_BOOL(GetMonitorInfoA(wa_par_HMONITOR(1),wa_par_MONITORINFO(2)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetMonitorInfoW(HMONITOR hMonitor,LPMONITORINFO lpmi)
*/
#if 0
HB_FUNC( WAGETMONITORINFOW )
{
  wa_ret_BOOL(GetMonitorInfoW(wa_par_HMONITOR(1),wa_par_MONITORINFO(2)));
}
#endif

HB_FUNC( WAGETMONITORINFO )
{
  wa_ret_BOOL(GetMonitorInfo(wa_par_HMONITOR(1),wa_par_MONITORINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI EnumDisplayMonitors(HDC hdc,LPCRECT lprcClip,MONITORENUMPROC lpfnEnum,LPARAM dwData)
*/

/*
WINUSERAPI VOID WINAPI NotifyWinEvent(DWORD event,HWND hwnd,LONG idObject,LONG idChild)
*/
HB_FUNC( WANOTIFYWINEVENT )
{
  NotifyWinEvent(wa_par_DWORD(1), wa_par_HWND(2), wa_par_LONG(3), wa_par_LONG(4));
}

/*
WINUSERAPI HWINEVENTHOOK WINAPI SetWinEventHook(DWORD eventMin,DWORD eventMax,HMODULE hmodWinEventProc,WINEVENTPROC pfnWinEventProc,DWORD idProcess,DWORD idThread,DWORD dwFlags)
*/

/*
WINUSERAPI WINBOOL WINAPI IsWinEventHookInstalled(DWORD event)
*/
HB_FUNC( WAISWINEVENTHOOKINSTALLED )
{
  wa_ret_BOOL(IsWinEventHookInstalled(wa_par_DWORD(1)));
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
HB_FUNC( WABLOCKINPUT )
{
  wa_ret_BOOL(BlockInput(wa_par_BOOL(1)));
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameA(HWND hwnd,LPSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WAGETWINDOWMODULEFILENAMEA ) // TODO: fix
{
  wa_ret_UINT(GetWindowModuleFileNameA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINUSERAPI UINT WINAPI GetWindowModuleFileNameW(HWND hwnd,LPWSTR pszFileName,UINT cchFileNameMax)
*/
HB_FUNC( WAGETWINDOWMODULEFILENAMEW ) // TODO: fix
{
  wa_ret_UINT(GetWindowModuleFileNameW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINUSERAPI WINBOOL WINAPI SetProcessDPIAware (VOID)
*/
HB_FUNC( WASETPROCESSDPIAWARE )
{
  wa_ret_BOOL(SetProcessDPIAware());
}

/*
WINUSERAPI WINBOOL WINAPI IsProcessDPIAware (VOID)
*/
HB_FUNC( WAISPROCESSDPIAWARE )
{
  wa_ret_BOOL(IsProcessDPIAware());
}

/*
WINUSERAPI WINBOOL WINAPI GetCursorInfo(PCURSORINFO pci)
*/
HB_FUNC( WAGETCURSORINFO )
{
  wa_ret_BOOL(GetCursorInfo(wa_par_CURSORINFO(1)));
}

/*
WINUSERAPI WINBOOL WINAPI GetWindowInfo(HWND hwnd,PWINDOWINFO pwi)
*/
HB_FUNC( WAGETWINDOWINFO )
{
  wa_ret_BOOL(GetWindowInfo(wa_par_HWND(1), wa_par_WINDOWINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetTitleBarInfo(HWND hwnd,PTITLEBARINFO pti)
*/
HB_FUNC( WAGETTITLEBARINFO )
{
  wa_ret_BOOL(GetTitleBarInfo(wa_par_HWND(1), wa_par_TITLEBARINFO(2)));
}

/*
WINUSERAPI WINBOOL WINAPI GetMenuBarInfo(HWND hwnd,LONG idObject,LONG idItem,PMENUBARINFO pmbi)
*/
HB_FUNC( WAGETMENUBARINFO )
{
  wa_ret_BOOL(GetMenuBarInfo(wa_par_HWND(1), wa_par_LONG(2), wa_par_LONG(3), wa_par_MENUBARINFO(4)));
}

/*
WINUSERAPI WINBOOL WINAPI GetScrollBarInfo(HWND hwnd,LONG idObject,PSCROLLBARINFO psbi)
*/

/*
WINUSERAPI WINBOOL WINAPI GetComboBoxInfo(HWND hwndCombo,PCOMBOBOXINFO pcbi)
*/
HB_FUNC( WAGETCOMBOBOXINFO )
{
  wa_ret_BOOL(GetComboBoxInfo(wa_par_HWND(1), wa_par_COMBOBOXINFO(2)));
}

/*
WINUSERAPI HWND WINAPI GetAncestor(HWND hwnd,UINT gaFlags)
*/
HB_FUNC( WAGETANCESTOR )
{
  wa_ret_HWND(GetAncestor(wa_par_HWND(1), wa_par_UINT(2)));
}

/*
WINUSERAPI HWND WINAPI RealChildWindowFromPoint(HWND hwndParent,POINT ptParentClientCoords)
*/
HB_FUNC( WAREALCHILDWINDOWFROMPOINT )
{
  wa_ret_HWND(RealChildWindowFromPoint(wa_par_HWND(1), *wa_par_POINT(2)));
}

/*
WINUSERAPI UINT WINAPI RealGetWindowClassA(HWND hwnd,LPSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WAREALGETWINDOWCLASSA ) // TODO: fix
{
  wa_ret_UINT(RealGetWindowClassA(wa_par_HWND(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINUSERAPI UINT WINAPI RealGetWindowClassW(HWND hwnd,LPWSTR ptszClassName,UINT cchClassNameMax)
*/
HB_FUNC( WAREALGETWINDOWCLASSW ) // TODO: fix
{
  wa_ret_UINT(RealGetWindowClassW(wa_par_HWND(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
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
HB_FUNC( WAGETLISTBOXINFO )
{
  wa_ret_DWORD(GetListBoxInfo(wa_par_HWND(1)));
}

/*
WINUSERAPI WINBOOL WINAPI LockWorkStation(VOID)
*/
HB_FUNC( WALOCKWORKSTATION )
{
  wa_ret_BOOL(LockWorkStation());
}

/*
WINUSERAPI WINBOOL WINAPI UserHandleGrantAccess(HANDLE hUserHandle,HANDLE hJob,WINBOOL bGrant)
*/
HB_FUNC( WAUSERHANDLEGRANTACCESS )
{
  wa_ret_BOOL(UserHandleGrantAccess(wa_par_HANDLE(1), wa_par_HANDLE(2), wa_par_BOOL(3)));
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
HB_FUNC( WASHUTDOWNBLOCKREASONCREATE )
{
  wa_ret_BOOL(ShutdownBlockReasonCreate(wa_par_HWND(1), wa_par_LPCWSTR(2)));
}

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonQuery (HWND hWnd, LPWSTR pwszBuff, DWORD *pcchBuff)
*/

/*
WINUSERAPI WINBOOL WINAPI ShutdownBlockReasonDestroy (HWND hWnd)
*/
HB_FUNC( WASHUTDOWNBLOCKREASONDESTROY )
{
  wa_ret_BOOL(ShutdownBlockReasonDestroy(wa_par_HWND(1)));
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
#if 0
HB_FUNC( WAISIMMERSIVEPROCESS )
{
  wa_ret_BOOL(IsImmersiveProcess(wa_par_HANDLE(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI SetProcessRestrictionExemption (WINBOOL fEnableExemption)
*/
#if 0
HB_FUNC( WASETPROCESSRESTRICTIONEXEMPTION )
{
  wa_ret_BOOL(SetProcessRestrictionExemption(wa_par_BOOL(1)));
}
#endif

/*
WINUSERAPI WINBOOL WINAPI GetPointerInputTransform(UINT32 pointerId, UINT32 historyCount, UINT32 *inputTransform)
*/

/*
WINUSERAPI WINBOOL WINAPI IsMousePointerEnabled(void)
*/
#if 0
HB_FUNC( WAISMOUSEPOINTERENABLED )
{
  wa_ret_BOOL(IsMousePointerEnabled());
}
#endif

/*
int GetSystemMetrics([in] int nIndex);
*/
HB_FUNC( WAGETSYSTEMMETRICS )
{
  wa_ret_int(GetSystemMetrics(wa_par_int(1)));
}

// MACROS

HB_FUNC( WAMAKEINTRESOURCE )
{
  hb_retptr(MAKEINTRESOURCE(wa_par_int(1)));
}
