//
// Misc Windows API functions
//
// Copyright 2008-2009 Viktor Szakats
// Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com> (win_SysRefresh())
//

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#include "hbwapi.hpp"
#include <hbapiitm.hpp>

#ifndef QS_ALLPOSTMESSAGE
#define QS_ALLPOSTMESSAGE 0x0100
#endif

HB_FUNC(WIN_LOADRESOURCE)
{
  HANDLE hInstance = nullptr;

  // Set default return value
  hb_retc_null();

  if (hb_winmainArgGet(&hInstance, nullptr, nullptr)) {
    HRSRC hRes;

    LPCTSTR szName;
    LPCTSTR szType;

    void *hName;
    void *hType;

    if (HB_ISNUM(1)) {
      szName = MAKEINTRESOURCE(hbwapi_par_INT(1));
      hName = nullptr;
    } else {
      szName = HB_PARSTRDEF(1, &hName, nullptr);
    }

    if (HB_ISNUM(2)) {
      szType = MAKEINTRESOURCE(hbwapi_par_INT(2));
      hType = nullptr;
    } else {
      szType = HB_PARSTRDEF(2, &hType, nullptr);
    }

    hRes = FindResource(static_cast<HMODULE>(hInstance), szName, szType);

    if (hRes) {
      HGLOBAL hMem = LoadResource(nullptr, hRes);

      if (hMem) {
        void *pMem = LockResource(hMem);

        if (pMem) {
          hb_retclen(static_cast<char *>(pMem), SizeofResource(nullptr, hRes));
        }
      }
    }

    hb_strfree(hName);
    hb_strfree(hType);
  }
}

HB_FUNC(WIN_GETCOMMANDLINEPARAM)
{
  LPCTSTR lpCmdLine = GetCommandLine();
  bool fQuote = false;
  long pos;

  // Skip application path
  pos = 0;
  while (lpCmdLine[pos] && (fQuote || !HB_ISSPACE(lpCmdLine[pos]))) {
    if (lpCmdLine[pos] == '"') {
      fQuote = !fQuote;
    }
    pos++;
  }
  while (HB_ISSPACE(lpCmdLine[pos])) {
    pos++;
  }

  HB_RETSTR(lpCmdLine + pos);
}

HB_FUNC(WIN_ANSITOWIDE)
{
  auto nLen = hb_parclen(1);
  LPCSTR lpSrcMB = hb_parcx(1);
  DWORD dwLength = MultiByteToWideChar(CP_ACP, 0, lpSrcMB, static_cast<int>(nLen), nullptr, 0);
  auto lpDstWide = static_cast<LPWSTR>(hb_xgrab((dwLength + 1) * sizeof(wchar_t)));
  MultiByteToWideChar(CP_ACP, 0, lpSrcMB, static_cast<int>(nLen), lpDstWide, dwLength + 1);
  hb_retclen_buffer(reinterpret_cast<char *>(lpDstWide), static_cast<HB_SIZE>(dwLength * sizeof(wchar_t)));
}

HB_FUNC(WIN_WIDETOANSI)
{
  auto nLen = hb_parclen(1);
  auto lpSrcWide = reinterpret_cast<LPCWSTR>(hb_parcx(1));
  DWORD dwLength = WideCharToMultiByte(CP_ACP, 0, lpSrcWide, static_cast<int>(nLen), nullptr, 0, nullptr, nullptr);
  auto lpDstMB = static_cast<LPSTR>(hb_xgrab(dwLength + 1));
  WideCharToMultiByte(CP_ACP, 0, lpSrcWide, static_cast<int>(nLen), lpDstMB, dwLength + 1, nullptr, nullptr);
  hb_retclen_buffer(lpDstMB, static_cast<HB_SIZE>(dwLength));
}

HB_FUNC(WIN_UNICODE)
{
#if defined(UNICODE)
  hb_retl(true);
#else
  hb_retl(false);
#endif
}

HB_FUNC(WIN_HINSTANCE)
{
  HANDLE hInstance;
  hb_winmainArgGet(&hInstance, nullptr, nullptr);
  hb_retptr(hInstance);
}

HB_FUNC(WIN_HPREVINSTANCE)
{
  HANDLE hPrevInstance;
  hb_winmainArgGet(nullptr, &hPrevInstance, nullptr);
  hb_retptr(hPrevInstance);
}

HB_FUNC(WIN_NCMDSHOW)
{
  int nCmdShow;
  hb_winmainArgGet(nullptr, nullptr, &nCmdShow);
  hb_retni(nCmdShow);
}

HB_FUNC(WIN_SYSREFRESH)
{
  HANDLE hDummyEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);

  if (hDummyEvent) {
    auto dwMsec = static_cast<DWORD>(hb_parnl(1));

    // Begin the operation and continue until it is complete
    // or until the user clicks the mouse or presses a key.

    if (MsgWaitForMultipleObjects(1, &hDummyEvent, FALSE, (dwMsec == 0 ? INFINITE : dwMsec),
                                  QS_ALLINPUT | QS_ALLPOSTMESSAGE) == WAIT_OBJECT_0 + 1) {
      MSG msg;

      while (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE)) {
        switch (msg.message) {
        case WM_CLOSE:
          CloseHandle(hDummyEvent);
          hb_retni(1);
          return;
        case WM_QUIT:
          CloseHandle(hDummyEvent);
          hb_retnint(msg.wParam);
          return;
#if 0
               case WM_LBUTTONDOWN:
               case WM_RBUTTONDOWN:
               case WM_KEYDOWN:
               case WM_LBUTTONUP:
               case WM_RBUTTONUP:
               case WM_KEYUP:
                  // Perform any required cleanup.
                  break;
#endif
        default:
          TranslateMessage(&msg);
          DispatchMessage(&msg);
        }
      }
    }

    CloseHandle(hDummyEvent);
  }

  hb_retni(0);
}

HB_FUNC(WIN_QPCOUNTER2SEC)
{
  static HB_MAXDBL s_dFrequence = 0;

  if (s_dFrequence == 0) {
    LARGE_INTEGER frequency;
    if (!QueryPerformanceFrequency(&frequency)) {
      hb_retnd(0);
      return;
    }
    s_dFrequence = static_cast<HB_MAXDBL>(HBWAPI_GET_LARGEUINT(frequency));
  }
  hb_retnd(static_cast<double>(static_cast<HB_MAXDBL>(hb_parnint(1)) / static_cast<HB_MAXDBL>(s_dFrequence)));
}
