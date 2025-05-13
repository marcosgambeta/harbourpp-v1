//
// Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
// Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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

#include "hbwin.hpp"
#include <hbapiitm.hpp>

static HKEY hb_regkeyconv(HB_PTRUINT nKey)
{
  switch (nKey)
  {
  case 1:
    return static_cast<HKEY>(HKEY_CLASSES_ROOT);
  /* NOTE: In xhb, zero value means HKEY_LOCAL_MACHINE. */
  case 0:
  case 2:
    return static_cast<HKEY>(HKEY_CURRENT_USER);
  case 3:
    return static_cast<HKEY>(HKEY_CURRENT_CONFIG);
  case 4:
    return static_cast<HKEY>(HKEY_LOCAL_MACHINE);
  case 5:
    return static_cast<HKEY>(HKEY_USERS);
  }

  return reinterpret_cast<HKEY>(nKey);
}

HB_FUNC(WIN_REGCREATEKEYEX)
{
  void *hKey;
  HKEY hkResult = nullptr;
  DWORD dwDisposition = 0;

  hb_retl(RegCreateKeyEx(hb_regkeyconv(static_cast<HB_PTRUINT>(hb_parnint(1))), HB_PARSTRDEF(2, &hKey, nullptr), 0,
                         nullptr, hb_parnl(5) /* dwOptions */, hb_parnl(6) /* samDesired */,
                         nullptr /* lpSecurityAttributes */, &hkResult, &dwDisposition) == ERROR_SUCCESS);

  hb_storptr(hkResult, 8);
  hb_stornl(dwDisposition, 9);

  hb_strfree(hKey);
}

HB_FUNC(WIN_REGOPENKEYEX)
{
  void *hKey;
  HKEY hkResult = nullptr;

  hb_retl(RegOpenKeyEx(hb_regkeyconv(static_cast<HB_PTRUINT>(hb_parnint(1))), HB_PARSTRDEF(2, &hKey, nullptr),
                       0 /* dwOptions */, hb_parnl(4) /* samDesired */, &hkResult) == ERROR_SUCCESS);

  hb_storptr(hkResult, 5);

  hb_strfree(hKey);
}

HB_FUNC(WIN_REGQUERYVALUEEX)
{
  void *hKey;
  LPCTSTR lpKey = HB_PARSTRDEF(2, &hKey, nullptr);
  DWORD dwType = 0;
  DWORD dwSize = 0;

  if (RegQueryValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, nullptr, &dwType, nullptr, &dwSize) == ERROR_SUCCESS)
  {
    if (dwSize > 0)
    {
      if (dwType == REG_SZ || dwType == REG_EXPAND_SZ || dwType == REG_MULTI_SZ)
      {
        auto lpValue = static_cast<LPBYTE>(hb_xgrab((dwSize + 1) * sizeof(TCHAR)));

        if (RegQueryValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, nullptr, &dwType, lpValue, &dwSize) ==
            ERROR_SUCCESS)
        {
          dwSize /= sizeof(TCHAR);

          HB_STORSTRLEN(reinterpret_cast<LPTSTR>(lpValue), dwSize, 5);
        }
        else
        {
          hb_stor(5);
        }

        hb_xfree(lpValue);
      }
      else
      { /* No translation for binary data */
        auto lpValue = static_cast<LPBYTE>(hb_xgrab(dwSize + 1));

        if (RegQueryValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, nullptr, &dwType, lpValue, &dwSize) ==
            ERROR_SUCCESS)
        {
          if (!hb_storclen_buffer(reinterpret_cast<char *>(lpValue), dwSize, 5))
          {
            hb_xfree(lpValue);
          }
        }
        else
        {
          hb_stor(5);
          hb_xfree(lpValue);
        }
      }
    }
    else
    {
      hb_storc(nullptr, 5);
    }
  }
  else
  {
    hb_stor(5);
  }

  hb_stornl(dwType, 4);
  hb_retnl(dwSize);

  hb_strfree(hKey);
}

HB_FUNC(WIN_REGSETVALUEEX)
{
  void *hKey;
  auto dwType = static_cast<DWORD>(hb_parnl(4));
  LPCTSTR lpKey = HB_PARSTRDEF(2, &hKey, nullptr);

  if (dwType == REG_DWORD)
  {
    auto nSpace = static_cast<DWORD>(hb_parnl(5));
    hb_retl(RegSetValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, 0, dwType, reinterpret_cast<const BYTE *>(&nSpace),
                          sizeof(DWORD)) == ERROR_SUCCESS);
  }
#if defined(REG_QWORD)
  else if (dwType == REG_QWORD)
  {
    auto nSpace = static_cast<HB_U64>(hb_parnint(5));
    hb_retl(RegSetValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, 0, dwType, reinterpret_cast<const BYTE *>(&nSpace),
                          sizeof(HB_U64)) == ERROR_SUCCESS);
  }
#endif
  else if (dwType == REG_SZ || dwType == REG_EXPAND_SZ || dwType == REG_MULTI_SZ)
  {
    void *hValue;
    HB_SIZE nValueLen;
    LPCTSTR lpValue = HB_PARSTR(5, &hValue, &nValueLen);

    ++nValueLen;

    nValueLen *= sizeof(TCHAR);

    hb_retl(RegSetValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, 0, dwType, reinterpret_cast<const BYTE *>(lpValue),
                          static_cast<DWORD>(nValueLen)) == ERROR_SUCCESS);

    hb_strfree(hValue);
  }
  else
  { /* No translation for binary data */
    hb_retl(RegSetValueEx(static_cast<HKEY>(hb_parptr(1)), lpKey, 0, dwType,
                          reinterpret_cast<const BYTE *>(hb_parc(5)) /* cValue */,
                          static_cast<DWORD>(hb_parclen(5)) + 1) == ERROR_SUCCESS);
  }

  hb_strfree(hKey);
}

HB_FUNC(WIN_REGDELETEKEY)
{
  void *hKey;
  hb_retl(RegDeleteKey(hb_regkeyconv(static_cast<HB_PTRUINT>(hb_parnint(1))),
                       static_cast<LPCTSTR>(HB_PARSTRDEF(2, &hKey, nullptr))) == ERROR_SUCCESS);
  hb_strfree(hKey);
}

HB_FUNC(WIN_REGDELETEVALUE)
{
  void *hValue;
  hb_retl(RegDeleteValue(static_cast<HKEY>(hb_parptr(1)), static_cast<LPCTSTR>(HB_PARSTR(2, &hValue, nullptr))) ==
          ERROR_SUCCESS);
  hb_strfree(hValue);
}

HB_FUNC(WIN_REGCLOSEKEY)
{
  hb_retl(RegCloseKey(static_cast<HKEY>(hb_parptr(1))) == ERROR_SUCCESS);
}
