//
// Harbour File Find API (C level)
//
// Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
// Copyright 2001-2002 Viktor Szakats (vszakats.net/harbour)
// Copyright 2001-2002 Paul Tucker <ptucker@sympatico.ca>
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

#if !defined(_LARGEFILE64_SOURCE)
#define _LARGEFILE64_SOURCE 1
#endif

#define _HB_FFIND_INTERNAL_

#include "hbapi.hpp"
#include "hbapifs.hpp"
#include "hbvm.hpp"
#include "hbdate.hpp"
#include "hb_io.hpp"

// ---

#if defined(HB_OS_WIN)

#include <windows.h>
#include "hbwinuni.hpp"

struct HB_FFIND_INFO
{
  HANDLE hFindFile;
  WIN32_FIND_DATA pFindFileData;
  DWORD dwAttr;
  bool fLabelDone;
};

using PHB_FFIND_INFO = HB_FFIND_INFO *;

#define _HB_WIN_MASKATTR (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM)
#define _HB_WIN_MATCH()                                                                                                \
  (((info->pFindFileData.dwFileAttributes & _HB_WIN_MASKATTR) == 0) ||                                                 \
   ((info->dwAttr & info->pFindFileData.dwFileAttributes & _HB_WIN_MASKATTR) != 0))

#elif defined(HB_OS_UNIX)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <time.h>

struct HB_FFIND_INFO
{
  DIR *dir;
  struct dirent *entry;
  char pattern[HB_PATH_MAX];
  char path[HB_PATH_MAX];
};

using PHB_FFIND_INFO = HB_FFIND_INFO *;

#else

using HB_FFIND_INFO = void;
using PHB_FFIND_INFO = void *;

#endif

#if !defined(HB_USE_LARGEFILE64) && defined(HB_OS_UNIX)
#if defined(__USE_LARGEFILE64)
// The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
// defined and effectively enables lseek64()/flock64()/ftruncate64()
// functions on 32-bit machines.
#define HB_USE_LARGEFILE64
#elif defined(HB_OS_UNIX) && defined(O_LARGEFILE)
#define HB_USE_LARGEFILE64
#endif
#endif

// ---

HB_FATTR hb_fsAttrFromRaw(HB_FATTR raw_attr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrFromRaw(%u)", raw_attr));
#endif

  HB_FATTR nAttr;

#if defined(HB_OS_WIN)

  nAttr = 0;
  if (raw_attr & FILE_ATTRIBUTE_ARCHIVE)
  {
    nAttr |= HB_FA_ARCHIVE;
  }
  if (raw_attr & FILE_ATTRIBUTE_DIRECTORY)
  {
    nAttr |= HB_FA_DIRECTORY;
  }
  if (raw_attr & FILE_ATTRIBUTE_HIDDEN)
  {
    nAttr |= HB_FA_HIDDEN;
  }
  if (raw_attr & FILE_ATTRIBUTE_READONLY)
  {
    nAttr |= HB_FA_READONLY;
  }
  if (raw_attr & FILE_ATTRIBUTE_SYSTEM)
  {
    nAttr |= HB_FA_SYSTEM;
  }
  if (raw_attr & FILE_ATTRIBUTE_NORMAL)
  {
    nAttr |= HB_FA_NORMAL;
  }

  // Note that FILE_ATTRIBUTE_NORMAL is not needed
  // HB_FA_DEVICE not supported
  // HB_FA_VOLCOMP needs to be checked
  if (raw_attr & FILE_ATTRIBUTE_ENCRYPTED)
  {
    nAttr |= HB_FA_ENCRYPTED;
  }
  if (raw_attr & FILE_ATTRIBUTE_TEMPORARY)
  {
    nAttr |= HB_FA_TEMPORARY;
  }
  if (raw_attr & FILE_ATTRIBUTE_SPARSE_FILE)
  {
    nAttr |= HB_FA_SPARSE;
  }
  if (raw_attr & FILE_ATTRIBUTE_REPARSE_POINT)
  {
    nAttr |= HB_FA_REPARSE;
  }
  if (raw_attr & FILE_ATTRIBUTE_COMPRESSED)
  {
    nAttr |= HB_FA_COMPRESSED;
  }
  if (raw_attr & FILE_ATTRIBUTE_OFFLINE)
  {
    nAttr |= HB_FA_OFFLINE;
  }
  // FILE_ATTRIBUTE_NOT_CONTENT_INDEXED
  // not defined in some older winnt.h
  if (raw_attr & 0x00002000)
  {
    nAttr |= HB_FA_NOTINDEXED;
  }
  if (raw_attr & 0x00008000)
  {
    nAttr |= HB_FA_VOLCOMP;
  }

#elif defined(HB_OS_UNIX)

  nAttr = ((raw_attr & S_IXOTH) ? HB_FA_XOTH : 0) | ((raw_attr & S_IWOTH) ? HB_FA_WOTH : 0) |
          ((raw_attr & S_IROTH) ? HB_FA_ROTH : 0) | ((raw_attr & S_IXGRP) ? HB_FA_XGRP : 0) |
          ((raw_attr & S_IWGRP) ? HB_FA_WGRP : 0) | ((raw_attr & S_IRGRP) ? HB_FA_RGRP : 0) |
          ((raw_attr & S_IXUSR) ? HB_FA_XUSR : 0) | ((raw_attr & S_IWUSR) ? HB_FA_WUSR : 0) |
          ((raw_attr & S_IRUSR) ? HB_FA_RUSR : 0) | ((raw_attr & S_ISVTX) ? HB_FA_SVTX : 0) |
          ((raw_attr & S_ISGID) ? HB_FA_SGID : 0) | ((raw_attr & S_ISUID) ? HB_FA_SUID : 0);

  if (S_ISREG(raw_attr))
  {
    nAttr |= HB_FA_FILE;
  }
  if (S_ISDIR(raw_attr))
  {
    nAttr |= HB_FA_DIRECTORY;
  }
  if (S_ISLNK(raw_attr))
  {
    nAttr |= HB_FA_LINK;
  }
  if (S_ISCHR(raw_attr))
  {
    nAttr |= HB_FA_CHRDEVICE;
  }
  if (S_ISBLK(raw_attr))
  {
    nAttr |= HB_FA_BLKDEVICE;
  }
  if (S_ISFIFO(raw_attr))
  {
    nAttr |= HB_FA_FIFO;
  }
#if !defined(HB_OS_VXWORKS)
  if (S_ISSOCK(raw_attr))
  {
    nAttr |= HB_FA_SOCKET;
  }
#endif

#else

  nAttr = 0;
  HB_SYMBOL_UNUSED(raw_attr);

#endif

  return nAttr;
}

HB_FATTR hb_fsAttrToRaw(HB_FATTR nAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrToRaw(%u)", nAttr));
#endif

  HB_FATTR raw_attr;

#if defined(HB_OS_WIN)

  raw_attr = 0;

  if (nAttr & HB_FA_ARCHIVE)
  {
    raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
  }
  if (nAttr & HB_FA_DIRECTORY)
  {
    raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
  }
  if (nAttr & HB_FA_HIDDEN)
  {
    raw_attr |= FILE_ATTRIBUTE_HIDDEN;
  }
  if (nAttr & HB_FA_READONLY)
  {
    raw_attr |= FILE_ATTRIBUTE_READONLY;
  }
  if (nAttr & HB_FA_SYSTEM)
  {
    raw_attr |= FILE_ATTRIBUTE_SYSTEM;
  }
  if (nAttr & HB_FA_NORMAL)
  {
    raw_attr |= FILE_ATTRIBUTE_NORMAL;
  }

  // Note that FILE_ATTRIBUTE_NORMAL is not needed
  // HB_FA_DEVICE not supported
  // HB_FA_VOLCOMP needs to be checked
  if (nAttr & HB_FA_ENCRYPTED)
  {
    raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
  }
  if (nAttr & HB_FA_TEMPORARY)
  {
    raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
  }
  if (nAttr & HB_FA_SPARSE)
  {
    raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
  }
  if (nAttr & HB_FA_REPARSE)
  {
    raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
  }
  if (nAttr & HB_FA_COMPRESSED)
  {
    raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
  }
  if (nAttr & HB_FA_OFFLINE)
  {
    raw_attr |= FILE_ATTRIBUTE_OFFLINE;
  }
  if (nAttr & HB_FA_NOTINDEXED)
  {
    raw_attr |= 0x00002000; // FILE_ATTRIBUTE_NOT_CONTENT_INDEXED not defined in some older winnt.h
  }
  if (nAttr & HB_FA_VOLCOMP)
  {
    raw_attr |= 0x00008000;
  }

#elif defined(HB_OS_UNIX)

  raw_attr = HB_FA_POSIX_ATTR(nAttr);

  if (nAttr & HB_FA_FILE)
  {
    raw_attr |= S_IFREG;
  }
  if (nAttr & HB_FA_DIRECTORY)
  {
    raw_attr |= S_IFDIR;
  }
  if (nAttr & HB_FA_LINK)
  {
    raw_attr |= S_IFLNK;
  }
  if (nAttr & HB_FA_CHRDEVICE)
  {
    raw_attr |= S_IFCHR;
  }
  if (nAttr & HB_FA_BLKDEVICE)
  {
    raw_attr |= S_IFBLK;
  }
  if (nAttr & HB_FA_FIFO)
  {
    raw_attr |= S_IFIFO;
  }
  if (nAttr & HB_FA_SOCKET)
  {
    raw_attr |= S_IFSOCK;
  }

#else

  HB_SYMBOL_UNUSED(nAttr);
  raw_attr = 0;

#endif

  return raw_attr;
}

// Converts a CA-Cl*pper compatible file attribute string
// to the internal representation.

HB_FATTR hb_fsAttrEncode(const char *szAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrEncode(%p)", static_cast<const void*>(szAttr)));
#endif

  const char *pos = szAttr;
  char ch;
  HB_FATTR nAttr = 0;

  while ((ch = static_cast<char>(HB_TOUPPER(*pos))) != '\0')
  {
    switch (ch)
    {
    case 'R':
      nAttr |= HB_FA_READONLY;
      break;
    case 'H':
      nAttr |= HB_FA_HIDDEN;
      break;
    case 'S':
      nAttr |= HB_FA_SYSTEM;
      break;
    case 'A':
      nAttr |= HB_FA_ARCHIVE;
      break;
    case 'D':
      nAttr |= HB_FA_DIRECTORY;
      break;
    case 'V':
      nAttr |= HB_FA_LABEL;
      break;
    case 'L':
      nAttr |= HB_FA_LINK;
      break;
    }

    pos++;
  }

  return nAttr;
}

// Converts a file attribute (ffind->attr) to the CA-Cl*pper
// compatible file attribute string format.

// NOTE: szAttr buffer must be at least 16 chars long

char *hb_fsAttrDecode(HB_FATTR nAttr, char *szAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrDecode(%u, %p)", nAttr, static_cast<void*>(szAttr)));
#endif

  char *ptr = szAttr;

  // Using the same order as CA-Cl*pper did: RHSVDA.
  if (nAttr & HB_FA_READONLY)
  {
    *ptr++ = 'R';
  }
  if (nAttr & HB_FA_HIDDEN)
  {
    *ptr++ = 'H';
  }
  if (nAttr & HB_FA_SYSTEM)
  {
    *ptr++ = 'S';
  }
  if (nAttr & HB_FA_ARCHIVE)
  {
    *ptr++ = 'A';
  }
  if (nAttr & HB_FA_DIRECTORY)
  {
    *ptr++ = 'D';
  }
  if (nAttr & HB_FA_LABEL)
  {
    *ptr++ = 'V';
  }
  if (nAttr & HB_FA_LINK)
  {
    *ptr++ = 'L';
  }

  *ptr = '\0';

  return szAttr;
}

// Finds the first then the next matching file on
// each call. Does low-level (platform dependent
// filtering if needed.

static bool hb_fsFindNextLow(PHB_FFIND ffind)
{
  auto bFound = false;

  int iYear = 0;
  int iMonth = 0;
  int iDay = 0;

  int iHour = 0;
  int iMin = 0;
  int iSec = 0;
  int iMSec = 0;

  HB_FATTR raw_attr = 0, nAttr = 0;

  // Set the default values in case some platforms don't
  // support some of these, or they may fail on them.

  ffind->szName[0] = '\0';
  ffind->size = 0;

  // Do platform dependent first/next search

  hb_vmUnlock();

#if defined(HB_OS_WIN)

  {
    auto info = static_cast<PHB_FFIND_INFO>(ffind->info);

    bFound = false;

    if ((ffind->attrmask & HB_FA_LABEL) != 0 && !info->fLabelDone)
    {
      TCHAR lpVolName[HB_PATH_MAX];
      LPTSTR lpFileMask = nullptr;
      char *mask = nullptr;

      info->fLabelDone = true;

      if (ffind->pszFileMask && *ffind->pszFileMask)
      {
        PHB_FNAME pFileName = hb_fsFNameSplit(ffind->pszFileMask);
        if (pFileName->szName && pFileName->szName[0])
        {
          mask = hb_strdup(pFileName->szName);
        }
        if (pFileName->szPath && pFileName->szPath[0] &&
            (pFileName->szPath[1] || pFileName->szPath[0] != HB_OS_PATH_DELIM_CHR))
        {
          lpFileMask = HB_CHARDUP(pFileName->szPath);
        }
        hb_xfree(pFileName);
      }
      bFound = GetVolumeInformation(lpFileMask, lpVolName, HB_SIZEOFARRAY(lpVolName), nullptr, nullptr, nullptr,
                                    nullptr, 0) != 0;
      if (bFound)
      {
        HB_OSSTRDUP2(lpVolName, ffind->szName, sizeof(ffind->szName) - 1);
        if (mask && *mask && !hb_strMatchFile(ffind->szName, mask))
        {
          ffind->szName[0] = '\0';
          bFound = false;
        }
      }
      if (lpFileMask)
      {
        hb_xfree(lpFileMask);
      }
      if (mask)
      {
        hb_xfree(mask);
      }
    }

    if (!bFound && (ffind->attrmask & (HB_FA_LABEL | HB_FA_HIDDEN | HB_FA_SYSTEM | HB_FA_DIRECTORY)) != HB_FA_LABEL)
    {
      if (ffind->bFirst)
      {
        LPTSTR lpFileMask = HB_CHARDUP(ffind->pszFileMask);
        ffind->bFirst = false;
        info->dwAttr = static_cast<DWORD>(hb_fsAttrToRaw(ffind->attrmask));
        info->hFindFile = FindFirstFile(lpFileMask, &info->pFindFileData);
        hb_xfree(lpFileMask);

        if ((info->hFindFile != INVALID_HANDLE_VALUE) && _HB_WIN_MATCH())
        {
          bFound = true;
        }
      }

      if (!bFound && info->hFindFile != INVALID_HANDLE_VALUE)
      {
        while (FindNextFile(info->hFindFile, &info->pFindFileData))
        {
          if (_HB_WIN_MATCH())
          {
            bFound = true;
            break;
          }
        }
      }

      // Fill Harbour found file info

      if (bFound)
      {
        HB_OSSTRDUP2(info->pFindFileData.cFileName, ffind->szName, sizeof(ffind->szName) - 1);

        if (info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        {
          ffind->size = 0;
        }
        else
        {
          ffind->size = static_cast<HB_FOFFSET>(info->pFindFileData.nFileSizeLow) +
                        (static_cast<HB_FOFFSET>(info->pFindFileData.nFileSizeHigh) << 32);
        }

        raw_attr = static_cast<HB_FATTR>(info->pFindFileData.dwFileAttributes);

        // NOTE: One of these may fail when searching on an UNC path, I
        //       don't know yet what's the reason. [vszakats]

        {
          FILETIME ft;
          SYSTEMTIME time;

          if (FileTimeToLocalFileTime(&info->pFindFileData.ftLastWriteTime, &ft) && FileTimeToSystemTime(&ft, &time))
          {
            iYear = time.wYear;
            iMonth = time.wMonth;
            iDay = time.wDay;
            iHour = time.wHour;
            iMin = time.wMinute;
            iSec = time.wSecond;
            iMSec = time.wMilliseconds;
          }
        }
      }
    }
    hb_fsSetIOError(bFound, 0);
  }

#elif defined(HB_OS_UNIX)

  {
    auto info = static_cast<PHB_FFIND_INFO>(ffind->info);

    char dirname[HB_PATH_MAX];

    bFound = false;

    // TODO: HB_FA_LABEL handling

    if (ffind->bFirst)
    {
      char *pos;

      ffind->bFirst = false;

      hb_strncpy(dirname, ffind->pszFileMask, sizeof(dirname) - 1);
      pos = strrchr(dirname, HB_OS_PATH_DELIM_CHR);
      if (pos)
      {
        hb_strncpy(info->pattern, pos + 1, sizeof(info->pattern) - 1);
        *(pos + 1) = '\0';
      }
      else
      {
        hb_strncpy(info->pattern, dirname, sizeof(info->pattern) - 1);
        dirname[0] = '.';
        dirname[1] = HB_OS_PATH_DELIM_CHR;
        dirname[2] = '\0';
      }
      if (info->pattern[0] == '.')
      {
        ffind->attrmask |= HB_FA_HIDDEN;
      }

#if 0
         tzset();
#endif

      info->dir = opendir(dirname);
      hb_strncpy(info->path, dirname, sizeof(info->path) - 1);
    }

    if (info->dir && info->pattern[0] != '\0')
    {
      while ((info->entry = readdir(info->dir)) != nullptr)
      {
        if (hb_strMatchFile(info->entry->d_name, info->pattern))
        {
          bFound = true;
          break;
        }
      }
    }

    // Fill Harbour found file info
    if (bFound)
    {
      hb_strncpy(dirname, info->path, sizeof(dirname) - 1);
      hb_strncat(dirname, info->entry->d_name, sizeof(dirname) - 1);
      {
        time_t ftime;
        struct tm lt;
#if defined(HB_USE_LARGEFILE64)
        struct stat64 sStat, sStatL;
        if (lstat64(dirname, &sStat) == 0)
        {
          if (S_ISLNK(sStat.st_mode) && (ffind->attrmask & HB_FA_LINK) == 0)
          {
            if (stat64(dirname, &sStatL) == 0)
            {
              memcpy(&sStat, &sStatL, sizeof(sStat));
            }
            nAttr |= HB_FA_LINK;
          }
#else
        struct stat sStat, sStatL;
        if (lstat(dirname, &sStat) == 0)
        {
          if (S_ISLNK(sStat.st_mode) && (ffind->attrmask & HB_FA_LINK) == 0)
          {
            if (stat(dirname, &sStatL) == 0)
            {
              memcpy(&sStat, &sStatL, sizeof(sStat));
            }
            nAttr |= HB_FA_LINK;
          }
#endif
          if (info->entry->d_name[0] == '.')
          {
            if (info->entry->d_name[1] && (info->entry->d_name[1] != '.' || info->entry->d_name[2]))
            {
              nAttr |= HB_FA_HIDDEN;
            }
          }
          hb_strncpy(ffind->szName, info->entry->d_name, sizeof(ffind->szName) - 1);
          ffind->size = sStat.st_size;

          raw_attr = sStat.st_mode;

          ftime = sStat.st_mtime;
#if defined(HB_HAS_LOCALTIME_R)
          localtime_r(&ftime, &lt);
#else
          lt = *localtime(&ftime);
#endif

          iYear = lt.tm_year + 1900;
          iMonth = lt.tm_mon + 1;
          iDay = lt.tm_mday;

          iHour = lt.tm_hour;
          iMin = lt.tm_min;
          iSec = lt.tm_sec;

#if defined(HB_OS_LINUX) && defined(__GLIBC__) && defined(__GLIBC_MINOR__) &&                                          \
    (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 6))
#if defined(_BSD_SOURCE) || defined(_SVID_SOURCE) ||                                                                   \
    (__GLIBC_MINOR__ >= 12 &&                                                                                          \
     ((defined(_POSIX_C_SOURCE) || _POSIX_C_SOURCE >= 200809L) || (defined(_XOPEN_SOURCE) || _XOPEN_SOURCE >= 700)))
          iMSec = sStat.st_mtim.tv_nsec / 1000000;
#else
          iMSec = sStat.st_mtimensec / 1000000;
#endif
#endif
        }
        else
        {
          bFound = false;
        }
      }
    }
    hb_fsSetIOError(bFound, 0);
  }

#else

  {
    int iTODO; // TODO: for given platform

#if 0
      HB_SYMBOL_UNUSED(ffind);
#endif

    HB_SYMBOL_UNUSED(iYear);
    HB_SYMBOL_UNUSED(iMonth);
    HB_SYMBOL_UNUSED(iDay);
    HB_SYMBOL_UNUSED(iHour);
    HB_SYMBOL_UNUSED(iMin);
    HB_SYMBOL_UNUSED(iSec);
    HB_SYMBOL_UNUSED(iMSec);
    HB_SYMBOL_UNUSED(raw_attr);

    bFound = false;

    hb_fsSetIOError(bFound, 0);
  }

#endif

  // Fill common Harbour found file info

  if (bFound)
  {
    // Do the conversions common for all platforms
    ffind->szName[sizeof(ffind->szName) - 1] = '\0';

#if !defined(HB_OS_WIN)
    // Convert from OS codepage
    {
      char *pszFree = nullptr;
      HB_SIZE nSize = sizeof(ffind->szName);
      const char *pszResult = hb_osDecodeCP(ffind->szName, &pszFree, &nSize);

      if (pszFree)
      {
        hb_strncpy(ffind->szName, pszResult, sizeof(ffind->szName) - 1);
        hb_xfree(pszFree);
      }
    }
#endif
    ffind->attr = hb_fsAttrFromRaw(raw_attr) | nAttr;

    ffind->lDate = hb_dateEncode(iYear, iMonth, iDay);
    ffind->lTime = hb_timeEncode(iHour, iMin, iSec, iMSec);
    hb_dateStrPut(ffind->szDate, iYear, iMonth, iDay);
    ffind->szDate[8] = '\0';

    hb_snprintf(ffind->szTime, sizeof(ffind->szTime), "%02d:%02d:%02d", iHour, iMin, iSec);
  }
  hb_vmLock();

  return bFound;
}

PHB_FFIND hb_fsFindFirst(const char *pszFileMask, HB_FATTR attrmask)
{
  auto ffind = static_cast<PHB_FFIND>(hb_xgrabz(sizeof(HB_FFIND)));

  // Allocate platform dependent file find info storage
  ffind->info = static_cast<void *>(hb_xgrabz(sizeof(HB_FFIND_INFO)));

  // Store search parameters
#if defined(HB_OS_WIN)
  ffind->pszFileMask = pszFileMask;
#else
  // Convert to OS codepage
  ffind->pszFileMask = hb_fsNameConv(pszFileMask, &ffind->pszFree);
#endif
  ffind->attrmask = attrmask;
  ffind->bFirst = true;

  // Find first/next matching file

  if (hb_fsFindNext(ffind))
  {
    return ffind;
  }

  // If no file found at all, free stuff allocated so far and return nullptr.

  hb_fsFindClose(ffind);

  return nullptr;
}

// Finds next matching file, and applies a filter which makes
// searching CA-Cl*pper/MS-DOS compatible.

HB_BOOL hb_fsFindNext(PHB_FFIND ffind)
{
  while (hb_fsFindNextLow(ffind))
  {
    // Filter the result to stay MS-DOS and CA-Cl*pper compatible.

    if (!(((ffind->attrmask & HB_FA_HIDDEN) == 0 && (ffind->attr & HB_FA_HIDDEN) != 0) ||
          ((ffind->attrmask & HB_FA_SYSTEM) == 0 && (ffind->attr & HB_FA_SYSTEM) != 0) ||
          ((ffind->attrmask & HB_FA_LABEL) == 0 && (ffind->attr & HB_FA_LABEL) != 0) ||
          ((ffind->attrmask & HB_FA_DIRECTORY) == 0 && (ffind->attr & HB_FA_DIRECTORY) != 0)))
    {
      return true;
    }
  }

  return false;
}

void hb_fsFindClose(PHB_FFIND ffind)
{
  if (ffind)
  {
    if (ffind->pszFree)
    {
      hb_xfree(ffind->pszFree);
    }

    // Do platform dependent cleanup

    if (ffind->info)
    {
      auto info = static_cast<PHB_FFIND_INFO>(ffind->info);

      if (!ffind->bFirst)
      {
        hb_vmUnlock();

#if defined(HB_OS_WIN)

        if (info->hFindFile != INVALID_HANDLE_VALUE)
        {
          FindClose(info->hFindFile);
        }

#elif defined(HB_OS_UNIX)

        if (info->dir)
        {
          closedir(info->dir);
        }

#else
        {
          // Intentionally do nothing
          int iTODO; // TODO: for given platform
        }
#endif

        hb_vmLock();
      }

      hb_xfree(info);
    }

    hb_xfree(ffind);
  }
}
