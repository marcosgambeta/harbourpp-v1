//
// Windows API functions (winbase)
//
// Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

HB_FUNC(WAPI_GETLASTERROR)
{
  hb_retnl(static_cast<long>(hbwapi_GetLastError()));
}

HB_FUNC(WAPI_GETCURRENTPROCESSID)
{
  hb_retnint(GetCurrentProcessId());
}

HB_FUNC(WAPI_GETCURRENTTHREADID)
{
  hb_retnint(GetCurrentThreadId());
}

HB_FUNC(WAPI_FORMATMESSAGE)
{
  void *hSource = nullptr;
  LPTSTR lpAllocBuff = nullptr;
  LPTSTR lpBuffer = nullptr;
  HB_SIZE nSize = 0;
  DWORD dwRetVal;

  auto dwFlags = static_cast<DWORD>(hb_parnldef(1, FORMAT_MESSAGE_FROM_SYSTEM));

  if (HB_ISBYREF(5))
  {
    nSize = hb_parns(6);
    if ((dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER) == 0)
    {
      if (nSize == 0 && !HB_ISNUM(6))
      {
        nSize = hb_parclen(5);
      }
      if (nSize > 0)
      {
        lpBuffer = static_cast<LPTSTR>(hb_xgrab(nSize * sizeof(TCHAR)));
      }
      else
      {
        dwFlags |= FORMAT_MESSAGE_ALLOCATE_BUFFER;
      }
    }
  }
  else
  {
    dwFlags = static_cast<DWORD>(~FORMAT_MESSAGE_ALLOCATE_BUFFER);
  }

  if (dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER)
  {
    lpBuffer = reinterpret_cast<LPTSTR>(&lpAllocBuff);
  }

  dwRetVal =
      FormatMessage(dwFlags, HB_ISCHAR(2) ? static_cast<LPCVOID>(HB_PARSTR(2, &hSource, nullptr)) : hb_parptr(2),
                    HB_ISNUM(3) ? static_cast<DWORD>(hb_parnl(3)) : hbwapi_GetLastError() /* dwMessageId */,
                    static_cast<DWORD>(hb_parnldef(4, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT))) /* dwLanguageId */,
                    lpBuffer, static_cast<DWORD>(nSize), nullptr /* TODO: Add support for this parameter. */);

  hbwapi_SetLastError(GetLastError());
  hb_retnl(dwRetVal);

  if (lpBuffer)
  {
    if (dwFlags & FORMAT_MESSAGE_ALLOCATE_BUFFER)
    {
      lpBuffer = lpAllocBuff;
    }
    else
    {
      lpBuffer[nSize - 1] = '\0';
    }

    HB_STORSTR(dwRetVal ? lpBuffer : nullptr, 5);

    if (lpAllocBuff)
    {
      LocalFree(lpAllocBuff);
    }
    else if (lpBuffer)
    {
      hb_xfree(lpBuffer);
    }
  }

  hb_strfree(hSource);
}

/* VOID WINAPI Sleep(__in DWORD dwMilliseconds); */
HB_FUNC(WAPI_SLEEP)
{
  Sleep(static_cast<DWORD>(hb_parnl(1)));
}

HB_FUNC(WAPI_OUTPUTDEBUGSTRING)
{
  void *hOutputString;

  OutputDebugString(HB_PARSTR(1, &hOutputString, nullptr));

  hb_strfree(hOutputString);
}

#define TARGET_PATH_BUFFER_SIZE 4096
HB_FUNC(WAPI_QUERYDOSDEVICE)
{
  void *hDeviceName;
  auto lpTargetPath = static_cast<LPTSTR>(hb_xgrab(TARGET_PATH_BUFFER_SIZE * sizeof(TCHAR)));
  DWORD dwResult;

  dwResult = QueryDosDevice(HB_PARSTR(1, &hDeviceName, nullptr), lpTargetPath, TARGET_PATH_BUFFER_SIZE);
  hbwapi_SetLastError(GetLastError());
  if (dwResult)
  {
    auto pArray = hb_itemArrayNew(0);
    PHB_ITEM pItem = nullptr;
    DWORD dwPos, dwStart;

    dwPos = dwStart = 0;
    while (lpTargetPath[dwPos])
    {
      if (!lpTargetPath[++dwPos])
      {
        pItem = HB_ITEMPUTSTRLEN(pItem, lpTargetPath + dwStart, dwPos - dwStart - 1);
        hb_arrayAdd(pArray, pItem);
        dwStart = ++dwPos;
      }
    }
    hb_itemRelease(pItem);
    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_strfree(hDeviceName);
  hb_xfree(lpTargetPath);
}
