/*
 * __win_ProxyDetect()
 *
 * Copyright 2015 Viktor Szakats
 *
 */

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
#include <winhttp.h>

HB_FUNC(__WIN_PROXYDETECT)
{
  WINHTTP_AUTOPROXY_OPTIONS options{};
  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG ieproxy{};

  bool fDetect = true;

  if (WinHttpGetIEProxyConfigForCurrentUser(&ieproxy))
  {
    if (ieproxy.lpszAutoConfigUrl != nullptr)
    {
      options.lpszAutoConfigUrl = ieproxy.lpszAutoConfigUrl;
    }
    else
    {
      fDetect = ieproxy.fAutoDetect;
    }
  }

  if (fDetect)
  {
    HINTERNET hSession =
        WinHttpOpen(nullptr, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

    if (hSession != nullptr)
    {
      WINHTTP_PROXY_INFO proxy{};

      void *hURL;
      LPCTSTR pURL = HB_PARSTRDEF(1, &hURL, nullptr);
      DWORD dwError;

      if (options.lpszAutoConfigUrl != nullptr)
      {
        options.dwFlags = WINHTTP_AUTOPROXY_CONFIG_URL;
      }
      else
      {
        options.dwFlags = WINHTTP_AUTOPROXY_AUTO_DETECT;
        options.dwAutoDetectFlags = WINHTTP_AUTO_DETECT_TYPE_DNS_A;
#if defined(HBWIN_USE_WINHTTP_DHCP)
        /* This flag has issues, according to Chromium code. */
        options.dwAutoDetectFlags |= WINHTTP_AUTO_DETECT_TYPE_DHCP;
#endif
      }

      WinHttpSetTimeouts(hSession, 10000, 10000, 5000, 5000);

      fDetect = WinHttpGetProxyForUrl(hSession, pURL, &options, &proxy);
      hbwapi_SetLastError(dwError = GetLastError());

      if (!fDetect && dwError == ERROR_WINHTTP_LOGIN_FAILURE)
      {
        options.fAutoLogonIfChallenged = TRUE;

        fDetect = WinHttpGetProxyForUrl(hSession, pURL, &options, &proxy);
        hbwapi_SetLastError(GetLastError());
      }

      if (fDetect)
      {
        if (proxy.dwAccessType == WINHTTP_ACCESS_TYPE_NO_PROXY)
        {
          hb_retc_null();
          hb_storc(nullptr, 2);
        }
        else
        {
          HB_RETSTR(proxy.lpszProxy);
          HB_STORSTR(proxy.lpszProxyBypass, 2);
        }
      }
      else
      {
        fDetect = false;
      }

      if (proxy.lpszProxy != nullptr)
      {
        GlobalFree(proxy.lpszProxy);
      }
      if (proxy.lpszProxyBypass != nullptr)
      {
        GlobalFree(proxy.lpszProxyBypass);
      }

      hb_strfree(hURL);

      WinHttpCloseHandle(hSession);
    }
    else
    {
      fDetect = false;
    }
  }

  if (!fDetect)
  {
    hbwapi_SetLastError(GetLastError());
    HB_RETSTR(ieproxy.lpszProxy);
    HB_STORSTR(ieproxy.lpszProxyBypass, 2);
  }

  if (ieproxy.lpszAutoConfigUrl != nullptr)
  {
    GlobalFree(ieproxy.lpszAutoConfigUrl);
  }
  if (ieproxy.lpszProxy != nullptr)
  {
    GlobalFree(ieproxy.lpszProxy);
  }
  if (ieproxy.lpszProxyBypass != nullptr)
  {
    GlobalFree(ieproxy.lpszProxyBypass);
  }
}
