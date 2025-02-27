//
// environment variables access
//
// Copyright 2001-2002 Antonio Linares <alinares@fivetech.com>
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

// NOTE: Notice that this code is needed as ANSI C getenv() crashes
//       badly when used from a Windows DLL.

#include "hbapi.hpp"

#if defined(HB_OS_WIN)
#include <windows.h>
#include "hbwinuni.hpp"
#elif defined(__FreeBSD__)
#include <sys/param.h>
#endif

// NOTE: Warning, this function _may_ return nullptr as a result if
//       the environment variable reading fails form some reason.
//       If the return value is not nullptr, the caller must free
//       the pointer. [vszakats]

char *hb_getenv(const char *szName)
{
  char *pszBuffer = nullptr;

#if defined(HB_OS_WIN)
  {
    LPTSTR lpName = HB_CHARDUP(szName);
    DWORD size = GetEnvironmentVariable(lpName, nullptr, 0);

    if (size != 0)
    {
      LPTSTR lpBuffer = static_cast<LPTSTR>(hb_xgrab(size * sizeof(TCHAR)));
      GetEnvironmentVariable(lpName, lpBuffer, size);
      pszBuffer = HB_OSSTRDUP(lpBuffer);
      hb_xfree(lpBuffer);
    }
    hb_xfree(lpName);
  }
#else
  {
    char *pszTemp, *pszNameFree = nullptr;

    szName = hb_osEncodeCP(szName, &pszNameFree, nullptr);
    pszTemp = getenv(szName);
    if (pszNameFree)
    {
      hb_xfree(pszNameFree);
    }

    if (pszTemp != nullptr)
    {
      pszBuffer = hb_osStrDecode(pszTemp);
    }
  }
#endif

  return pszBuffer;
}

HB_BOOL hb_getenv_buffer(const char *szName, char *szBuffer, int nSize)
{
  auto fRetVal = false;

#if defined(HB_OS_WIN)
  {
    TCHAR lpNameBuffer[64], lpDestBuffer[HB_PATH_MAX];
    LPTSTR lpName = lpNameBuffer, lpBuffer = lpDestBuffer;

    if (szBuffer == nullptr || nSize == 0)
    {
      lpBuffer = nullptr;
    }
    else if (static_cast<HB_SIZE>(nSize) > HB_SIZEOFARRAY(lpDestBuffer))
    {
      lpBuffer = static_cast<LPTSTR>(hb_xgrab(nSize * sizeof(TCHAR)));
    }

    if (strlen(szName) >= HB_SIZEOFARRAY(lpNameBuffer))
    {
      lpName = HB_CHARDUP(szName);
    }

    fRetVal = GetEnvironmentVariable(lpName, lpBuffer, nSize) != 0;

    if (lpName != lpNameBuffer)
    {
      hb_xfree(lpName);
    }

    if (lpBuffer)
    {
      if (fRetVal)
      {
        lpBuffer[nSize - 1] = TEXT('\0');
        HB_OSSTRDUP2(lpBuffer, szBuffer, nSize - 1);
      }
      if (lpBuffer != lpDestBuffer)
      {
        hb_xfree(lpBuffer);
      }
    }
  }
#else
  {
    char *pszTemp, *pszNameFree = nullptr;

    szName = hb_osEncodeCP(szName, &pszNameFree, nullptr);
    pszTemp = getenv(szName);
    if (pszNameFree)
    {
      hb_xfree(pszNameFree);
    }

    if (pszTemp != nullptr)
    {
      fRetVal = true;
      if (szBuffer != nullptr && nSize != 0)
      {
        hb_osStrDecode2(pszTemp, szBuffer, nSize - 1);
      }
    }
    else
    {
      fRetVal = false;
    }
  }
#endif

  if (!fRetVal && szBuffer != nullptr && nSize != 0)
  {
    szBuffer[0] = '\0';
  }

  return fRetVal;
}

// set current process environment variable, if szValue is nullptr delete
// environment variable
HB_BOOL hb_setenv(const char *szName, const char *szValue)
{
  if (szName == nullptr)
  {
    return false;
  }

#if defined(HB_OS_WIN)
  {
    LPTSTR lpName = HB_CHARDUP(szName);
    LPTSTR lpValue = szValue ? HB_CHARDUP(szValue) : nullptr;
    bool fResult = (SetEnvironmentVariable(lpName, lpValue) != 0);
    if (lpValue)
    {
      hb_xfree(lpValue);
    }
    hb_xfree(lpName);
    return fResult;
  }
#elif defined(_BSD_SOURCE) || _POSIX_C_SOURCE >= 200112L || _XOPEN_SOURCE >= 600 || defined(HB_OS_SUNOS) ||            \
    defined(HB_OS_BSD) || defined(HB_OS_DARWIN) || defined(HB_OS_BEOS) || defined(HB_OS_QNX) ||                        \
    defined(HB_OS_VXWORKS) || defined(HB_OS_CYGWIN) || defined(HB_OS_MINIX) || defined(HB_OS_ANDROID)
  {
    auto fResult = false;
    char *pszNameFree = nullptr, *pszValueFree = nullptr;

    szName = hb_osEncodeCP(szName, &pszNameFree, nullptr);
    if (szValue != nullptr)
    {
      szValue = hb_osEncodeCP(szValue, &pszValueFree, nullptr);
      fResult = setenv(szName, szValue, 1) == 0;
      if (pszValueFree)
      {
        hb_xfree(pszValueFree);
      }
    }
    else
    {
#if defined(__OpenBSD__) || defined(HB_OS_QNX) || (defined(__FreeBSD_version) && __FreeBSD_version < 700050) ||        \
    (defined(HB_OS_DARWIN) && !(defined(__DARWIN_UNIX03) && __DARWIN_UNIX03))
      unsetenv(szName);
      fResult = true;
#else
      fResult = unsetenv(szName) == 0;
#endif
    }

    if (pszNameFree)
    {
      hb_xfree(pszNameFree);
    }

    return fResult;
  }
#elif defined(_HB_NO_SETENV_)

  HB_SYMBOL_UNUSED(szValue);

  return false;

#else
  // please add support for other C compilers
  // if such functionality does not exists for given platform/C compiler
  // then please simply added C compiler with necessary OS/version checking
  // to the above #elif ... to eliminate warning [druzus]

  int iTODO;

  HB_SYMBOL_UNUSED(szValue);

  return false;

#endif
}
