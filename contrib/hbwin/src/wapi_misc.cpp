//
// Low-level Windows worker functions
//
// Copyright 2011 Viktor Szakats (vszakats.net/harbour)
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

HB_SIZE hbwapi_tstrlen(const TCHAR *pText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrlen(%p)", static_cast<const void*>(pText)));
#endif

  HB_SIZE nLen = 0;

  while (pText[nLen] != TEXT('\0'))
  {
    ++nLen;
  }

  return nLen;
}

// NOTE: Based on hb_strdup()
TCHAR *hbwapi_tstrdup(const TCHAR *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrdup(%p)", static_cast<const void*>(pszText)));
#endif

  HB_SIZE nLen = (hbwapi_tstrlen(pszText) + 1) * sizeof(TCHAR);
  auto pszDup = static_cast<TCHAR *>(hb_xgrab(nLen));
  memcpy(pszDup, pszText, nLen);
  return pszDup;
}

// NOTE: Based on hb_strncat()
TCHAR *hbwapi_tstrncat(TCHAR *pDest, const TCHAR *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hbwapi_tstrncat(%p, %p, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<const void*>(pSource), nLen));
#endif

  TCHAR *pBuf = pDest;

  pDest[nLen] = TEXT('\0');

  while (nLen && *pDest)
  {
    pDest++;
    nLen--;
  }

  while (nLen && (*pDest++ = *pSource++) != TEXT('\0'))
  {
    nLen--;
  }

  return pBuf;
}

static TCHAR *hbwapi_FileNameAtSystemDir(const TCHAR *pFileName)
{
  UINT nLen = GetSystemDirectory(nullptr, 0);

  if (nLen)
  {
    if (pFileName)
    {
      nLen += static_cast<UINT>(hbwapi_tstrlen(pFileName)) + 1;
    }

    auto buffer = static_cast<LPTSTR>(hb_xgrab(nLen * sizeof(TCHAR)));

    GetSystemDirectory(buffer, nLen);

    if (pFileName)
    {
      hbwapi_tstrncat(buffer, TEXT("\\"), nLen - 1);
      hbwapi_tstrncat(buffer, pFileName, nLen - 1);
    }

    return buffer;
  }
  else
  {
    return hbwapi_tstrdup(pFileName);
  }
}

#ifndef LOAD_LIBRARY_SEARCH_SYSTEM32
#define LOAD_LIBRARY_SEARCH_SYSTEM32 0x00000800
#endif

HMODULE hbwapi_LoadLibrarySystem(LPCTSTR pFileName)
{
  TCHAR *pLibPath = hbwapi_FileNameAtSystemDir(pFileName);

  // TODO: Replace flag with LOAD_LIBRARY_SEARCH_SYSTEM32 in the future [vszakats]
  HMODULE h = LoadLibraryEx(pLibPath, nullptr, LOAD_WITH_ALTERED_SEARCH_PATH);

  hb_xfree(pLibPath);

  return h;
}
