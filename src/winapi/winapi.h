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

#define winapi_get_ptr(n)          hb_itemGetPtr(hb_objSendMsg(hb_param(n, Harbour::Item::OBJECT), "PTR", 0))
// #define winapi_par_STRUCT(s, n)    static_cast<s>(hb_itemGetPtr(hb_objSendMsg(hb_param(n, Harbour::Item::OBJECT), "PTR", 0))

#define winapi_par_ATOM(n)         static_cast<ATOM>(hb_parni(n))
#define winapi_par_BOOL(n)         hb_parl(n)
#define winapi_par_BOOLEAN(n)      hb_parl(n)
#define winapi_par_BYTE(n)         static_cast<BYTE>(hb_parni(n))
#define winapi_par_char(n)         static_cast<char>(hb_parni(n))
#define winapi_par_CHAR(n)         static_cast<CHAR>(hb_parni(n))
#define winapi_par_COLORREF(n)     static_cast<COLORREF>(hb_parnl(n))
#define winapi_par_DWORD(n)        static_cast<DWORD>(hb_parnl(n))
#define winapi_par_DWORD_PTR(n)    static_cast<DWORD_PTR>(hb_parnl(n))
#define winapi_par_FLOAT(n)        static_cast<FLOAT>(hb_parnd(n))
#define winapi_par_HACCEL(n)       static_cast<HACCEL>(hb_parptr(n))
#define winapi_par_HANDLE(n)       static_cast<HANDLE>(hb_parptr(n))
#define winapi_par_HBITMAP(n)      static_cast<HBITMAP>(hb_parptr(n))
#define winapi_par_HBRUSH(n)       static_cast<HBRUSH>(hb_parptr(n))
#define winapi_par_HCOLORSPACE(n)  static_cast<HCOLORSPACE>(hb_parptr(n))
#define winapi_par_HCURSOR(n)      static_cast<HCURSOR>(hb_parptr(n))
#define winapi_par_HDC(n)          static_cast<HDC>(hb_parptr(n))
#define winapi_par_HDESK(n)        static_cast<HDESK>(hb_parptr(n))
#define winapi_par_HDEVNOTIFY(n)   static_cast<HDEVNOTIFY>(hb_parptr(n))
#define winapi_par_HDRVR(n)        static_cast<HDRVR>(hb_parptr(n))
#define winapi_par_HDWP(n)         static_cast<HDWP>(hb_parptr(n))
#define winapi_par_HENHMETAFILE(n) static_cast<HENHMETAFILE>(hb_parptr(n))
#define winapi_par_HFILE(n)        static_cast<HFILE>(hb_parni(n))
#define winapi_par_HFONT(n)        static_cast<HFONT>(hb_parptr(n))
#define winapi_par_HGDIOBJ(n)      static_cast<HGDIOBJ>(hb_parptr(n))
#define winapi_par_HGLOBAL(n)      static_cast<HGLOBAL>(hb_parptr(n))
#define winapi_par_HGLRC(n)        static_cast<HGLRC>(hb_parptr(n))
#define winapi_par_HICON(n)        static_cast<HICON>(hb_parptr(n))
#define winapi_par_HIMAGELIST(n)   static_cast<HIMAGELIST>(hb_parptr(n))
#define winapi_par_HINSTANCE(n)    static_cast<HINSTANCE>(hb_parptr(n))
#define winapi_par_HINTERNET(n)    static_cast<HINTERNET>(hb_parptr(n))
#define winapi_par_HKEY(n)         static_cast<HKEY>(hb_parptr(n))
#define winapi_par_HKL(n)          static_cast<HKL>(hb_parptr(n))
#define winapi_par_HLOCAL(n)       static_cast<HLOCAL>(hb_parptr(n))
#define winapi_par_HMENU(n)        static_cast<HMENU>(hb_parptr(n))
#define winapi_par_HMETAFILE(n)    static_cast<HMETAFILE>(hb_parptr(n))
#define winapi_par_HMIDI(n)        static_cast<HMIDI>(hb_parptr(n))
#define winapi_par_HMIDIIN(n)      static_cast<HMIDIIN>(hb_parptr(n))
#define winapi_par_HMIDIOUT(n)     static_cast<HMIDIOUT>(hb_parptr(n))
#define winapi_par_HMIDISTRM(n)    static_cast<HMIDISTRM>(hb_parptr(n))
#define winapi_par_HMIXER(n)       static_cast<HMIXER>(hb_parptr(n))
#define winapi_par_HMMIO(n)        static_cast<HMMIO>(hb_parptr(n))
#define winapi_par_HMODULE(n)      static_cast<HMODULE>(hb_parptr(n))
#define winapi_par_HPALETTE(n)     static_cast<HPALETTE>(hb_parptr(n))
#define winapi_par_HPOWERNOTIFY(n) static_cast<HPOWERNOTIFY>(hb_parptr(n))
#define winapi_par_HRGN(n)         static_cast<HRGN>(hb_parptr(n))
#define winapi_par_HRSRC(n)        static_cast<HRSRC>(hb_parptr(n))
#define winapi_par_HTASK(n)        static_cast<HTASK>(hb_parptr(n))
#define winapi_par_HWAVEIN(n)      static_cast<HWAVEIN>(hb_parptr(n))
#define winapi_par_HWAVEOUT(n)     static_cast<HWAVEOUT>(hb_parptr(n))
#define winapi_par_HWINSTA(n)      static_cast<HWINSTA>(hb_parptr(n))
#define winapi_par_HWND(n)         static_cast<HWND>(hb_parptr(n))
#define winapi_par_int(n)          hb_parni(n)
#define winapi_par_INT(n)          hb_parni(n)
#define winapi_par_INT_PTR(n)      static_cast<INT_PTR>(hb_parni(n))
#define winapi_par_LONG(n)         static_cast<LONG>(hb_parnl(n))
#define winapi_par_LPARAM(n)       static_cast<LPARAM>(hb_parnl(n))
#define winapi_par_LPCSTR(n)       hb_parc(n)
#define winapi_par_LPVOID(n)       static_cast<LPVOID>(hb_parptr(n))
#define winapi_par_LRESULT(n)      static_cast<LRESULT>(hb_parnl(n))
#define winapi_par_MCIDEVICEID(n)  static_cast<MCIDEVICEID>(hb_parni(n))
#define winapi_par_MCIERROR(n)     static_cast<MCIERROR>(hb_parnl(n))
#define winapi_par_MMRESULT(n)     static_cast<MMRESULT>(hb_parni(n))
#define winapi_par_SIZE_T(n)       static_cast<SIZE_T>(hb_parnl(n))
#define winapi_par_UCHAR(n)        static_cast<UCHAR>(hb_parni(n))
#define winapi_par_UINT(n)         static_cast<UINT>(hb_parni(n))
#define winapi_par_UINT_PTR(n)     static_cast<UINT_PTR>(hb_parni(n))
#define winapi_par_UINT32(n)       static_cast<UINT32>(hb_parni(n))
#define winapi_par_ULONG(n)        static_cast<ULONG>(hb_parnl(n))
#define winapi_par_ULONG_PTR(n)    static_cast<ULONG_PTR>(hb_parnl(n))
#define winapi_par_WCHAR(n)        static_cast<WCHAR>(hb_parni(n))
#define winapi_par_WORD(n)         static_cast<WORD>(hb_parni(n))
#define winapi_par_WPARAM(n)       static_cast<WPARAM>(hb_parni(n))

#define winapi_ret___LONG32(x)     hb_retnl(x)
#define winapi_ret_ATOM(x)         hb_retni(x)
#define winapi_ret_BOOL(x)         hb_retl(x)
#define winapi_ret_BOOLEAN(x)      hb_retl(x)
#define winapi_ret_COLORREF(x)     hb_retnl(x)
#define winapi_ret_DWORD(x)        hb_retnl(x)
#define winapi_ret_HACCEL(x)       hb_retptr(x)
#define winapi_ret_HANDLE(x)       hb_retptr(x)
#define winapi_ret_HBITMAP(x)      hb_retptr(x)
#define winapi_ret_HBRUSH(x)       hb_retptr(x)
#define winapi_ret_HCOLORSPACE(x)  hb_retptr(x)
#define winapi_ret_HCURSOR(x)      hb_retptr(x)
#define winapi_ret_HDC(x)          hb_retptr(x)
#define winapi_ret_HDESK(x)        hb_retptr(x)
#define winapi_ret_HDEVNOTIFY(x)   hb_retptr(x)
#define winapi_ret_HDRVR(x)        hb_retptr(x)
#define winapi_ret_HDWP(x)         hb_retptr(x)
#define winapi_ret_HENHMETAFILE(x) hb_retptr(x)
#define winapi_ret_HFONT(x)        hb_retptr(x)
#define winapi_ret_HGDIOBJ(x)      hb_retptr(x)
#define winapi_ret_HGLOBAL(x)      hb_retptr(x)
#define winapi_ret_HGLRC(x)        hb_retptr(x)
#define winapi_ret_HICON(x)        hb_retptr(x)
#define winapi_ret_HIMAGELIST(x)   hb_retptr(x)
#define winapi_ret_HINSTANCE(x)    hb_retptr(x)
#define winapi_ret_HINTERNET(x)    hb_retptr(x)
#define winapi_ret_HKL(x)          hb_retptr(x)
#define winapi_ret_HLOCAL(x)       hb_retptr(x)
#define winapi_ret_HMENU(x)        hb_retptr(x)
#define winapi_ret_HMETAFILE(x)    hb_retptr(x)
#define winapi_ret_HMIDI(x)        hb_retptr(x)
#define winapi_ret_HMIDIIN(x)      hb_retptr(x)
#define winapi_ret_HMIDIOUT(x)     hb_retptr(x)
#define winapi_ret_HMIDISTRM(x)    hb_retptr(x)
#define winapi_ret_HMIXER(x)       hb_retptr(x)
#define winapi_ret_HMMIO(x)        hb_retptr(x)
#define winapi_ret_HMODULE(x)      hb_retptr(x)
#define winapi_ret_HPALETTE(x)     hb_retptr(x)
#define winapi_ret_HPEN(x)         hb_retptr(x)
#define winapi_ret_HPOWERNOTIFY(x) hb_retptr(x)
#define winapi_ret_HRESULT(x)      hb_retnl(x)
#define winapi_ret_HRGN(x)         hb_retptr(x)
#define winapi_ret_HRSRC(x)        hb_retptr(x)
#define winapi_ret_HTASK(x)        hb_retptr(x)
#define winapi_ret_HWAVEIN(x)      hb_retptr(x)
#define winapi_ret_HWAVEOUT(x)     hb_retptr(x)
#define winapi_ret_HWINSTA(x)      hb_retptr(x)
#define winapi_ret_HWND(x)         hb_retptr(x)
#define winapi_ret_int(x)          hb_retni(x)
#define winapi_ret_LONG(x)         hb_retnl(x)
#define winapi_ret_LPARAM(x)       hb_retnl(x)
#define winapi_ret_LRESULT(x)      hb_retnl(x)
#define winapi_ret_MCIDEVICEID(x)  hb_retni(x)
#define winapi_ret_MCIERROR(x)     hb_retnl(x)
#define winapi_ret_MMRESULT(x)     hb_retni(x)
#define winapi_ret_SHORT(x)        hb_retni(x)
#define winapi_ret_UINT(x)         hb_retni(x)
#define winapi_ret_ULONG_PTR(x)    hb_retnl(x)
#define winapi_ret_WORD(x)         hb_retni(x)
#define winapi_ret_SIZE_T(x)       hb_retnl(x)
#define winapi_ret_HFILE(x)        hb_retni(x)
#define winapi_ret_NTSTATUS(x)     hb_retnl(x)

// #define winapi_stor_ATOM(v, n)         hb_storni(v, n)
#define winapi_stor_BOOL(v, n)         hb_storl(v, n)
// #define winapi_stor_BOOLEAN(v, n)      hb_storl(v, n)
#define winapi_stor_BYTE(v, n)         hb_storni(v, n)
// #define winapi_stor_CHAR(v, n)         hb_storni(v, n)
#define winapi_stor_COLORREF(v, n)     hb_stornl(v, n)
#define winapi_stor_DWORD(v, n)        hb_stornl(v, n)
// #define winapi_stor_DWORD_PTR(v, n)    hb_stornl(v, n)
#define winapi_stor_FLOAT(v, n)        hb_stornd(v, n)
// #define winapi_stor_HACCEL(v, n)       hb_storptr(v, n)
// #define winapi_stor_HANDLE(v, n)       hb_storptr(v, n)
// #define winapi_stor_HBITMAP(v, n)      hb_storptr(v, n)
// #define winapi_stor_HBRUSH(v, n)       hb_storptr(v, n)
// #define winapi_stor_HCOLORSPACE(v, n)  hb_storptr(v, n)
// #define winapi_stor_HCURSOR(v, n)      hb_storptr(v, n)
// #define winapi_stor_HDC(v, n)          hb_storptr(v, n)
// #define winapi_stor_HDESK(v, n)        hb_storptr(v, n)
// #define winapi_stor_HDEVNOTIFY(v, n)   hb_storptr(v, n)
// #define winapi_stor_HDRVR(v, n)        hb_storptr(v, n)
// #define winapi_stor_HDWP(v, n)         hb_storptr(v, n)
// #define winapi_stor_HENHMETAFILE(v, n) hb_storptr(v, n)
// #define winapi_stor_HFILE(v, n)        hb_storni(v, n)
// #define winapi_stor_HFONT(v, n)        hb_storptr(v, n)
// #define winapi_stor_HGDIOBJ(v, n)      hb_storptr(v, n)
// #define winapi_stor_HGLOBAL(v, n)      hb_storptr(v, n)
// #define winapi_stor_HGLRC(v, n)        hb_storptr(v, n)
// #define winapi_stor_HICON(v, n)        hb_storptr(v, n)
// #define winapi_stor_HIMAGELIST(v, n)   hb_storptr(v, n)
// #define winapi_stor_HINSTANCE(v, n)    hb_storptr(v, n)
// #define winapi_stor_HINTERNET(v, n)    hb_storptr(v, n)
// #define winapi_stor_HKEY(v, n)         hb_storptr(v, n)
// #define winapi_stor_HKL(v, n)          hb_storptr(v, n)
// #define winapi_stor_HLOCAL(v, n)       hb_storptr(v, n)
// #define winapi_stor_HMENU(v, n)        hb_storptr(v, n)
// #define winapi_stor_HMETAFILE(v, n)    hb_storptr(v, n)
// #define winapi_stor_HMIDI(v, n)        hb_storptr(v, n)
// #define winapi_stor_HMIDIIN(v, n)      hb_storptr(v, n)
// #define winapi_stor_HMIDIOUT(v, n)     hb_storptr(v, n)
// #define winapi_stor_HMIDISTRM(v, n)    hb_storptr(v, n)
// #define winapi_stor_HMIXER(v, n)       hb_storptr(v, n)
// #define winapi_stor_HMMIO(v, n)        hb_storptr(v, n)
// #define winapi_stor_HMODULE(v, n)      hb_storptr(v, n)
// #define winapi_stor_HPALETTE(v, n)     hb_storptr(v, n)
// #define winapi_stor_HPOWERNOTIFY(v, n) hb_storptr(v, n)
// #define winapi_stor_HRGN(v, n)         hb_storptr(v, n)
// #define winapi_stor_HRSRC(v, n)        hb_storptr(v, n)
// #define winapi_stor_HTASK(v, n)        hb_storptr(v, n)
// #define winapi_stor_HWAVEIN(v, n)      hb_storptr(v, n)
// #define winapi_stor_HWAVEOUT(v, n)     hb_storptr(v, n)
// #define winapi_stor_HWINSTA(v, n)      hb_storptr(v, n)
// #define winapi_stor_HWND(v, n)         hb_storptr(v, n)
#define winapi_stor_int(v, n)          hb_storni(v, n)
#define winapi_stor_INT(v, n)          hb_storni(v, n)
// #define winapi_stor_INT_PTR(v, n)      hb_storni(v, n)
// #define winapi_stor_LONG(v, n)         hb_stornl(v, n)
// #define winapi_stor_LPARAM(v, n)       hb_stornl(v, n)
// #define winapi_stor_LPCSTR(v, n)       hb_storc(v, n)
#define winapi_stor_LPVOID(v, n)       hb_storptr(v, n)
// #define winapi_stor_LRESULT(v, n)      hb_stornl(v, n)
// #define winapi_stor_MCIDEVICEID(v, n)  hb_storni(v, n)
// #define winapi_stor_MCIERROR(v, n)     hb_stornl(v, n)
// #define winapi_stor_MMRESULT(v, n)     hb_storni(v, n)
// #define winapi_stor_SIZE_T(v, n)       hb_stornl(v, n)
#define winapi_stor_UCHAR(v, n)        hb_storni(v, n)
#define winapi_stor_UINT(v, n)         hb_storni(v, n)
// #define winapi_stor_UINT_PTR(v, n)     hb_storni(v, n)
// #define winapi_stor_UINT32(v, n)       hb_storni(v, n)
#define winapi_stor_ULONG(v, n)        hb_stornl(v, n)
// #define winapi_stor_ULONG_PTR(v, n)    hb_stornl(v, n)
// #define winapi_stor_WCHAR(v, n)        hb_storni(v, n)
#define winapi_stor_WORD(v, n)         hb_storni(v, n)
// #define winapi_stor_WPARAM(v, n)       hb_storni(v, n)