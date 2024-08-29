//
// hb_fsLink*(), hb_FLink*() functions
//
// Copyright 2010 Viktor Szakats (vszakats.net/harbour)
//

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

#include "hbapi.hpp"
#include "hbapifs.hpp"
#include "hbvm.hpp"

#if defined(HB_OS_WIN)
#include <windows.h>
#if !defined(INVALID_FILE_ATTRIBUTES)
#define INVALID_FILE_ATTRIBUTES (static_cast<DWORD>(-1))
#endif
#include "hbwinuni.hpp"
#elif defined(HB_OS_UNIX)
#include <unistd.h>
#endif

HB_BOOL hb_fsLink(const char *pszExisting, const char *pszNewFile)
{
  auto fResult = false;

  if (pszExisting && pszNewFile)
  {
    hb_vmUnlock();

#if defined(HB_OS_WIN)
    LPTSTR lpFileNameFree;
    LPCTSTR lpFileName = HB_FSNAMECONV(pszNewFile, &lpFileNameFree);
    LPTSTR lpExistingFileNameFree;
    LPCTSTR lpExistingFileName = HB_FSNAMECONV(pszExisting, &lpExistingFileNameFree);

    fResult = CreateHardLink(lpFileName, lpExistingFileName, nullptr) != 0;
    hb_fsSetIOError(fResult, 0);

    if (lpFileNameFree)
    {
      hb_xfree(lpFileNameFree);
    }
    if (lpExistingFileNameFree)
    {
      hb_xfree(lpExistingFileNameFree);
    }
#elif defined(HB_OS_UNIX)
    {
      char *pszExistingFree;
      pszExisting = hb_fsNameConv(pszExisting, &pszExistingFree);
      char *pszNewFileFree;
      pszNewFile = hb_fsNameConv(pszNewFile, &pszNewFileFree);

      fResult = (link(pszExisting, pszNewFile) == 0);
      hb_fsSetIOError(fResult, 0);

      if (pszExistingFree)
      {
        hb_xfree(pszExistingFree);
      }
      if (pszNewFileFree)
      {
        hb_xfree(pszNewFileFree);
      }
    }
#else
    {
      hb_fsSetError(1);
      fResult = false;
    }
#endif

    hb_vmLock();
  }
  else
  {
    hb_fsSetError(2);
    fResult = false;
  }

  return fResult;
}

HB_BOOL hb_fsLinkSym(const char *pszTarget, const char *pszNewFile)
{
  auto fResult = false;

  if (pszTarget && pszNewFile)
  {
    hb_vmUnlock();

#if defined(HB_OS_WIN)
    {
      using _HB_CREATESYMBOLICLINK = BOOL(WINAPI *)(LPCTSTR, LPCTSTR, DWORD);

      static _HB_CREATESYMBOLICLINK s_pCreateSymbolicLink = nullptr;

#ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
#define SYMBOLIC_LINK_FLAG_DIRECTORY 0x1
#endif

      if (!s_pCreateSymbolicLink)
      {
        HMODULE hModule = GetModuleHandle(TEXT("kernel32.dll"));
        if (hModule)
        {
          s_pCreateSymbolicLink = reinterpret_cast<_HB_CREATESYMBOLICLINK>(
              reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESST(hModule, "CreateSymbolicLink")));
        }
      }

      if (s_pCreateSymbolicLink)
      {
        LPTSTR lpSymlinkFileNameFree;
        LPCTSTR lpSymlinkFileName = HB_FSNAMECONV(pszNewFile, &lpSymlinkFileNameFree);
        LPTSTR lpTargetFileNameFree;
        LPCTSTR lpTargetFileName = HB_FSNAMECONV(pszTarget, &lpTargetFileNameFree);

        DWORD dwAttr = GetFileAttributes(lpTargetFileName);
        bool fDir = (dwAttr != INVALID_FILE_ATTRIBUTES) && (dwAttr & FILE_ATTRIBUTE_DIRECTORY);

        fResult =
            s_pCreateSymbolicLink(lpSymlinkFileName, lpTargetFileName, fDir ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0) != 0;
        hb_fsSetIOError(fResult, 0);

        if (lpSymlinkFileNameFree)
        {
          hb_xfree(lpSymlinkFileNameFree);
        }
        if (lpTargetFileNameFree)
        {
          hb_xfree(lpTargetFileNameFree);
        }
      }
      else
      {
        hb_fsSetError(1);
        fResult = false;
      }
    }
#elif defined(HB_OS_UNIX)
    {
      char *pszTargetFree;
      pszTarget = hb_fsNameConv(pszTarget, &pszTargetFree);
      char *pszNewFileFree;
      pszNewFile = hb_fsNameConv(pszNewFile, &pszNewFileFree);

      fResult = (symlink(pszTarget, pszNewFile) == 0);
      hb_fsSetIOError(fResult, 0);

      if (pszTargetFree)
      {
        hb_xfree(pszTargetFree);
      }
      if (pszNewFileFree)
      {
        hb_xfree(pszNewFileFree);
      }
    }
#else
    {
      hb_fsSetError(1);
      fResult = false;
    }
#endif

    hb_vmLock();
  }
  else
  {
    hb_fsSetError(2);
    fResult = false;
  }

  return fResult;
}

/* NOTE: Caller must free the pointer, if not nullptr */
char *hb_fsLinkRead(const char *pszFile)
{
  char *pszLink = nullptr;

  if (pszFile)
  {
    hb_vmUnlock();

#if defined(HB_OS_WIN)
    {
      using _HB_GETFINALPATHNAMEBYHANDLE = DWORD(WINAPI *)(HANDLE, LPTSTR, DWORD, DWORD);

      static _HB_GETFINALPATHNAMEBYHANDLE s_pGetFinalPathNameByHandle = nullptr;

#ifndef VOLUME_NAME_DOS
#define VOLUME_NAME_DOS 0x0
#endif
#ifndef VOLUME_NAME_GUID
#define VOLUME_NAME_GUID 0x1
#endif
#ifndef VOLUME_NAME_NT
#define VOLUME_NAME_NT 0x2
#endif
#ifndef VOLUME_NAME_NONE
#define VOLUME_NAME_NONE 0x4
#endif
#ifndef FILE_NAME_NORMALIZED
#define FILE_NAME_NORMALIZED 0x0
#endif
#ifndef FILE_NAME_OPENED
#define FILE_NAME_OPENED 0x8
#endif

      if (!s_pGetFinalPathNameByHandle)
      {
        HMODULE hModule = GetModuleHandle(TEXT("kernel32.dll"));
        if (hModule)
        {
          s_pGetFinalPathNameByHandle = reinterpret_cast<_HB_GETFINALPATHNAMEBYHANDLE>(
              reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESST(hModule, "GetFinalPathNameByHandle")));
        }
      }

      if (s_pGetFinalPathNameByHandle)
      {
        LPTSTR lpFileNameFree;
        LPCTSTR lpFileName = HB_FSNAMECONV(pszFile, &lpFileNameFree);

        DWORD dwAttr = GetFileAttributes(lpFileName);
        bool fDir = (dwAttr != INVALID_FILE_ATTRIBUTES) && (dwAttr & FILE_ATTRIBUTE_DIRECTORY);

        HANDLE hFile =
            CreateFile(lpFileName, GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
                       fDir ? (FILE_ATTRIBUTE_DIRECTORY | FILE_FLAG_BACKUP_SEMANTICS) : FILE_ATTRIBUTE_NORMAL, nullptr);

        if (hFile == INVALID_HANDLE_VALUE)
        {
          hb_fsSetIOError(false, 0);
        }
        else
        {
          TCHAR lpLink[HB_PATH_MAX];
          DWORD size = s_pGetFinalPathNameByHandle(hFile, lpLink, HB_PATH_MAX, VOLUME_NAME_DOS);
          if (size < HB_PATH_MAX)
          {
            if (size > 0)
            {
              lpLink[size] = TEXT('\0');
              pszLink = HB_OSSTRDUP(lpLink);
            }

            hb_fsSetIOError(true, 0);
          }
          else
          {
            hb_fsSetError(9);
          }
        }

        if (lpFileNameFree)
        {
          hb_xfree(lpFileNameFree);
        }
      }
      else
      {
        hb_fsSetError(1);
      }
    }
#elif defined(HB_OS_UNIX)
    {
      char *pszFileFree;
      pszFile = hb_fsNameConv(pszFile, &pszFileFree);

      pszLink = static_cast<char *>(hb_xgrab(HB_PATH_MAX + 1));
      size_t size = readlink(pszFile, pszLink, HB_PATH_MAX);
      hb_fsSetIOError(size != static_cast<size_t>(-1), 0);
      if (size == static_cast<size_t>(-1))
      {
        hb_xfree(pszLink);
        pszLink = nullptr;
      }
      else
      {
        pszLink[size] = '\0';
        /* Convert from OS codepage */
        pszLink = const_cast<char *>(hb_osDecodeCP(pszLink, nullptr, nullptr));
      }

      if (pszFileFree)
      {
        hb_xfree(pszFileFree);
      }
    }
#else
    {
      hb_fsSetError(1);
    }
#endif

    hb_vmLock();
  }
  else
  {
    hb_fsSetError(2);
  }

  return pszLink;
}

HB_FUNC(HB_FLINK)
{
  HB_ERRCODE uiError = 2;
  auto fResult = false;
  auto pszExisting = hb_parc(1);
  auto pszNewFile = hb_parc(2);

  if (pszExisting && pszNewFile)
  {
    fResult = hb_fsLink(pszExisting, pszNewFile);
    uiError = hb_fsError();
  }
  hb_retni(fResult ? 0 : F_ERROR);
  hb_fsSetFError(uiError);
}

HB_FUNC(HB_FLINKSYM)
{
  HB_ERRCODE uiError = 2;
  auto fResult = false;
  auto pszTarget = hb_parc(1);
  auto pszNewFile = hb_parc(2);

  if (pszTarget && pszNewFile)
  {
    fResult = hb_fsLinkSym(pszTarget, pszNewFile);
    uiError = hb_fsError();
  }
  hb_retni(fResult ? 0 : F_ERROR);
  hb_fsSetFError(uiError);
}

HB_FUNC(HB_FLINKREAD)
{
  HB_ERRCODE uiError = 2;
  char *pszResult = nullptr;
  auto pszFile = hb_parc(1);

  if (pszFile)
  {
    pszResult = hb_fsLinkRead(pszFile);
    uiError = hb_fsError();
  }
  hb_retc_buffer(pszResult);
  hb_fsSetFError(uiError);
}
