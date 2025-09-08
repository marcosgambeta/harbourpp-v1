//
// The FileSys API (C level)
//
// Copyright 1999 {list of individual authors and e-mail addresses}
// Copyright 1999-2010 Viktor Szakats (vszakats.net/harbour)
//    hb_fsSetError(), hb_fsSetDevMode(), hb_fsReadLarge(), hb_fsWriteLarge()
//    hb_fsCurDirBuff(), hb_fsBaseDirBuff()
//    fs_win_get_drive(), fs_win_set_drive()
// Copyright 1999 Jose Lalin <dezac@corevia.com>
//    hb_fsChDrv(), hb_fsCurDrv(), hb_fsIsDrv(), hb_fsIsDevice()
// Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>, David G. Holm <dholm@jsd-llc.com>
//    hb_fsEof()
// Copyright 2001 Jose Gimenez (JFG) <jfgimenez@wanadoo.es>, <tecnico.sireinsa@ctv.es>
//    Added platform check for any compiler to use the Windows
//    API calls to allow opening an unlimited number of files
//    simultaneously.
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

// NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek(fhnd, offset < 0, FS_SET) will
//       set the file pointer to the passed negative value and the subsequent
//       hb_fsWrite() call will fail. In CA-Cl*pper, _fsSeek() will fail,
//       the pointer will not be moved and thus the _fsWrite() call will
//       successfully write the buffer to the current file position. [vszakats]
//
// This has been corrected by ptucker

// *nixes
#if !defined(_LARGEFILE64_SOURCE)
#define _LARGEFILE64_SOURCE 1
#endif
#if !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include "hbapi.hpp"
#include "hbvm.hpp"
#include "hbstack.hpp"
#include "hbapifs.hpp"
#include "hbapierr.hpp"
#include "hbapicdp.hpp"
#include "hbdate.hpp"
#include "hb_io.hpp"
#include "hbset.hpp"

#if defined(HB_OS_UNIX)
#include <unistd.h>
#include <time.h>
#include <utime.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#if !defined(HB_HAS_POLL) && !defined(HB_NO_POLL) && defined(_POSIX_C_SOURCE) && _POSIX_C_SOURCE >= 200112L
// use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
// file handle limit
#define HB_HAS_POLL
#endif
#if defined(HB_HAS_POLL)
#include <poll.h>
#endif
#endif
#if !defined(HB_OS_WIN)
#include <errno.h>
#endif

#if (defined(__BORLANDC__) || defined(__IBMCPP__) || defined(_MSC_VER) || defined(__MINGW32__)) && !defined(HB_OS_UNIX)
#include <sys/stat.h>
#include <fcntl.h>
#include <process.h>
#include <share.h>
#include <direct.h>
#if defined(__BORLANDC__)
#include <dir.h>
#include <dos.h>
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
#include <sys/locking.h>
#define ftruncate _chsize
#if defined(__MINGW32__) && !defined(_LK_UNLCK)
#define _LK_UNLCK _LK_UNLOCK
#endif
#else
#define ftruncate chsize
#endif
#if !defined(HAVE_POSIX_IO)
#define HAVE_POSIX_IO
#endif
#elif defined(__GNUC__) || defined(HB_OS_UNIX)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#if !defined(HAVE_POSIX_IO)
#define HAVE_POSIX_IO
#endif
#endif

#if defined(__MPW__)
#include <fcntl.h>
#endif

#if defined(HB_OS_WIN)
#include <windows.h>
#include "hbwinuni.hpp"
#if !defined(INVALID_SET_FILE_POINTER) && (defined(_MSC_VER))
#define INVALID_SET_FILE_POINTER (static_cast<DWORD>(-1))
#endif
#if !defined(INVALID_FILE_ATTRIBUTES)
#define INVALID_FILE_ATTRIBUTES (static_cast<DWORD>(-1))
#endif
#if defined(HB_OS_WIN_64)
#if !defined(HB_WIN_IOREAD_LIMIT)
#define HB_WIN_IOREAD_LIMIT HB_U32_MAX
#endif
#if !defined(HB_WIN_IOWRITE_LIMIT)
#define HB_WIN_IOWRITE_LIMIT HB_U32_MAX
#endif
#endif
#endif
#if defined(HB_USE_SHARELOCKS) && defined(HB_USE_BSDLOCKS)
#include <sys/file.h>
#endif
#if defined(HB_OS_LINUX)
#define HB_HAS_SELECT_TIMER
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

#if defined(HB_OS_HAS_DRIVE_LETTER)
// 2004-08-27 - <maurilio.longo@libero.it>
//              HB_FS_GETDRIVE() should return a number in the range 0..25 ('A'..'Z')
//              HB_FS_SETDRIVE() should accept a number inside same range.
//
//              If a particular platform/compiler returns/accepts different ranges of
//              values, simply define a branch for that platform.
//
//              NOTE: There is not an implicit "current disk", ALWAYS use
//
//                      my_func(hb_fsCurDrv(), ...)
//
//                    to refer to current disk

#if defined(HB_OS_WIN)

#define HB_FS_GETDRIVE(n)                                                                                              \
  do {                                                                                                                 \
    n = fs_win_get_drive();                                                                                            \
  } while (false)
#define HB_FS_SETDRIVE(n) fs_win_set_drive(n)

#elif defined(__BORLANDC__)
// 0 based version

#define HB_FS_GETDRIVE(n)                                                                                              \
  do {                                                                                                                 \
    n = getdisk();                                                                                                     \
  } while (false)
#define HB_FS_SETDRIVE(n) setdisk(n)

#else // _MSC_VER
// 1 based version

#define HB_FS_GETDRIVE(n)                                                                                              \
  do {                                                                                                                 \
    n = _getdrive() - 1;                                                                                               \
  } while (false)
#define HB_FS_SETDRIVE(n) _chdrive((n) + 1)

#endif
#endif // HB_OS_HAS_DRIVE_LETTER

#ifndef O_BINARY
#define O_BINARY 0 // O_BINARY not defined on Linux
#endif

#ifndef O_LARGEFILE
#define O_LARGEFILE 0 // O_LARGEFILE is used for LFS in 32-bit Linux
#endif

#if !defined(HB_OS_UNIX)
#if !defined(S_IREAD) && defined(S_IRUSR)
#define S_IREAD S_IRUSR
#endif
#if !defined(S_IWRITE) && defined(S_IWUSR)
#define S_IWRITE S_IWUSR
#endif
#if !defined(S_IEXEC) && defined(S_IXUSR)
#define S_IEXEC S_IXUSR
#endif
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__IBMCPP__)
// These compilers use sopen() rather than open(), because their
// versions of open() do not support combined O_ and SH_ flags
#define HB_FS_SOPEN
#endif

#if defined(HB_OS_ANDROID)
// hack for missing functions in android libc library
#define fdatasync fsync
#define ftruncate64 ftruncate
#define pread64 pread
#define pwrite64(f, b, s, o) pwrite(f, (void *)b, s, o)
#elif defined(HB_OS_MINIX)
// hack for functions missing from the Minix C library
#define fdatasync fsync
#define ftruncate64 ftruncate
#endif

#if UINT_MAX == USHRT_MAX
#define HB_FS_IO_16BIT
#endif

#if defined(HB_OS_UNIX) && defined(EINTR)
#define HB_FAILURE_RETRY(ret, exp)                                                                                     \
  do {                                                                                                                 \
    (ret) = (exp);                                                                                                     \
    hb_fsSetIOError((ret) != -1, 0);                                                                                   \
  } while ((ret) == -1 && hb_fsOsError() == static_cast<HB_ERRCODE>(EINTR) && hb_vmRequestQuery() == 0)
#else
#define HB_FAILURE_RETRY(ret, exp)                                                                                     \
  do {                                                                                                                 \
    (ret) = (exp);                                                                                                     \
    hb_fsSetIOError((ret) != -1, 0);                                                                                   \
  } while (false)
#endif

static auto s_fUseWaitLocks = true;

#if defined(HB_OS_WIN) && defined(HB_OS_HAS_DRIVE_LETTER)

static int fs_win_get_drive(void)
{
  TCHAR pBuffer[HB_PATH_MAX];
  LPTSTR lpBuffer = pBuffer;
  DWORD dwResult, dwSize;
  int iDrive = 0;

  dwSize = HB_SIZEOFARRAY(pBuffer);
  dwResult = GetCurrentDirectory(dwSize, lpBuffer);
  if (dwResult > dwSize) {
    dwSize = dwResult;
    lpBuffer = static_cast<TCHAR *>(hb_xgrab(dwSize * sizeof(TCHAR)));
    dwResult = GetCurrentDirectory(dwSize, lpBuffer);
  }
  hb_fsSetIOError(dwResult != 0, 0);
  if (dwResult >= 2 && dwResult < dwSize && lpBuffer[1] == HB_OS_DRIVE_DELIM_CHR) {
    iDrive = HB_TOUPPER(lpBuffer[0]);
    if (iDrive >= 'A' && iDrive <= 'Z') {
      iDrive -= 'A';
    } else {
      iDrive = 0;
    }
  }
  if (lpBuffer != pBuffer) {
    hb_xfree(lpBuffer);
  }
  return iDrive;
}

static void fs_win_set_drive(int iDrive)
{
  if (iDrive >= 0 && iDrive <= 25) {
    TCHAR szBuffer[3];
    auto fResult = false;
    UINT uiErrMode;

    szBuffer[0] = static_cast<TCHAR>(iDrive + 'A');
    szBuffer[1] = TEXT(':');
    szBuffer[2] = TEXT('\0');

    uiErrMode = SetErrorMode(SEM_FAILCRITICALERRORS);
    fResult = SetCurrentDirectory(szBuffer) != FALSE;
    SetErrorMode(uiErrMode);

    hb_fsSetIOError(fResult, 0);
  }
}

#endif

#if defined(HB_OS_WIN)

static HANDLE DosToWinHandle(HB_FHANDLE fHandle)
{
  switch (fHandle) {
  case static_cast<HB_FHANDLE>(FS_ERROR):
    return nullptr;
  case static_cast<HB_FHANDLE>(HB_STDIN_HANDLE):
    return GetStdHandle(STD_INPUT_HANDLE);
  case static_cast<HB_FHANDLE>(HB_STDOUT_HANDLE):
    return GetStdHandle(STD_OUTPUT_HANDLE);
  case static_cast<HB_FHANDLE>(HB_STDERR_HANDLE):
    return GetStdHandle(STD_ERROR_HANDLE);
  }
  return reinterpret_cast<HANDLE>(fHandle);
}

static void convert_open_flags(bool fCreate, HB_FATTR nAttr, HB_USHORT uiFlags, DWORD *dwMode, DWORD *dwShare,
                               DWORD *dwCreat, DWORD *dwAttr)
{
  if (fCreate) {
    *dwCreat = (uiFlags & FO_EXCL) ? CREATE_NEW : CREATE_ALWAYS;
    *dwMode = GENERIC_READ | GENERIC_WRITE;
  } else {
    if (uiFlags & FO_CREAT) {
      if (uiFlags & FO_EXCL) {
        *dwCreat = CREATE_NEW;
      } else if (uiFlags & FO_TRUNC) {
        *dwCreat = CREATE_ALWAYS;
      } else {
        *dwCreat = OPEN_ALWAYS;
      }
    } else if (uiFlags & FO_TRUNC) {
      *dwCreat = TRUNCATE_EXISTING;
    } else {
      *dwCreat = OPEN_EXISTING;
    }

    *dwMode = 0;
    switch (uiFlags & (FO_READ | FO_WRITE | FO_READWRITE)) {
    case FO_READWRITE:
      *dwMode |= GENERIC_READ | GENERIC_WRITE;
      break;
    case FO_WRITE:
      *dwMode |= GENERIC_WRITE;
      break;
    case FO_READ:
      *dwMode |= GENERIC_READ;
      break;
    }
  }

  // shared flags
  switch (uiFlags & (FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE | FO_DENYNONE)) {
  case FO_DENYREAD:
    *dwShare = FILE_SHARE_WRITE;
    break;
  case FO_DENYWRITE:
    *dwShare = FILE_SHARE_READ;
    break;
  case FO_EXCLUSIVE:
    *dwShare = 0;
    break;
  default:
    *dwShare = FILE_SHARE_WRITE | FILE_SHARE_READ;
    break;
  }

  // file attributes flags
  if (nAttr == FC_NORMAL) {
    *dwAttr = FILE_ATTRIBUTE_NORMAL;
  } else {
    *dwAttr = FILE_ATTRIBUTE_ARCHIVE;
    if (nAttr & FC_READONLY) {
      *dwAttr |= FILE_ATTRIBUTE_READONLY;
    }
    if (nAttr & FC_HIDDEN) {
      *dwAttr |= FILE_ATTRIBUTE_HIDDEN;
    }
    if (nAttr & FC_SYSTEM) {
      *dwAttr |= FILE_ATTRIBUTE_SYSTEM;
    }
  }
}

#else

static void convert_open_flags(bool fCreate, HB_FATTR nAttr, HB_USHORT uiFlags, int *flags, unsigned *mode, int *share,
                               int *attr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("convert_open_flags(%d, %u, %hu, %p, %p, %p, %p)", fCreate, nAttr, uiFlags, static_cast<void*>(flags), static_cast<void*>(mode), static_cast<void*>(share), static_cast<void*>(attr)));
#endif

  // file access mode
#if defined(HB_OS_UNIX)
  *mode = HB_FA_POSIX_ATTR(nAttr);
  if (*mode == 0) {
    *mode = S_IRUSR | S_IRGRP | S_IROTH;
    if (!(nAttr & HB_FA_READONLY)) {
      *mode |= S_IWUSR | S_IWGRP | S_IWOTH;
    }
    if (nAttr & HB_FA_SYSTEM) {
      *mode |= S_IXUSR | S_IXGRP | S_IXOTH;
    }
    if (nAttr & HB_FA_HIDDEN) {
      *mode &= S_IRUSR | S_IWUSR | S_IXUSR;
    }
  }
#else
  *mode = S_IREAD | ((nAttr & FC_READONLY) ? 0 : S_IWRITE) | ((nAttr & FC_SYSTEM) ? S_IEXEC : 0);
#endif

  // dos file attributes
  *attr = 0;

  if (fCreate) {
    *flags = O_RDWR | O_CREAT | O_TRUNC | O_BINARY | O_LARGEFILE | ((uiFlags & FO_EXCL) ? O_EXCL : 0);
  } else {
    *flags = O_BINARY | O_LARGEFILE;
    switch (uiFlags & (FO_READ | FO_WRITE | FO_READWRITE)) {
    case FO_READ:
      *flags |= O_RDONLY;
      break;
    case FO_WRITE:
      *flags |= O_WRONLY;
      break;
    case FO_READWRITE:
      *flags |= O_RDWR;
      break;
    default:
      // this should not happen and it's here to force default OS behavior
      *flags |= (O_RDONLY | O_WRONLY | O_RDWR);
      break;
    }

    if (uiFlags & FO_CREAT) {
      *flags |= O_CREAT;
    }
    if (uiFlags & FO_TRUNC) {
      *flags |= O_TRUNC;
    }
    if (uiFlags & FO_EXCL) {
      *flags |= O_EXCL;
    }
  }

  // shared flags (HB_FS_SOPEN)
#if defined(_MSC_VER)
  if ((uiFlags & FO_DENYREAD) == FO_DENYREAD) {
    *share = _SH_DENYRD;
  } else if (uiFlags & FO_EXCLUSIVE) {
    *share = _SH_DENYRW;
  } else if (uiFlags & FO_DENYWRITE) {
    *share = _SH_DENYWR;
  } else if (uiFlags & FO_DENYNONE) {
    *share = _SH_DENYNO;
  } else {
    *share = _SH_COMPAT;
  }
#elif !defined(HB_OS_UNIX)
  if ((uiFlags & FO_DENYREAD) == FO_DENYREAD) {
    *share = SH_DENYRD;
  } else if (uiFlags & FO_EXCLUSIVE) {
    *share = SH_DENYRW;
  } else if (uiFlags & FO_DENYWRITE) {
    *share = SH_DENYWR;
  } else if (uiFlags & FO_DENYNONE) {
    *share = SH_DENYNO;
  } else {
    *share = SH_COMPAT;
  }
#else
  *share = 0;
#endif

#if 0
   HB_TRACE(HB_TR_INFO, ("convert_open_flags: flags=0x%04x, mode=0x%04x, share=0x%04x, attr=0x%04x", *flags, *mode, *share, *attr));
#endif
}
#endif

static HB_USHORT convert_seek_flags(HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("convert_seek_flags(%hu)", uiFlags));
#endif

  // by default FS_SET is set
  HB_USHORT result_flags = SEEK_SET;

  if (uiFlags & FS_RELATIVE) {
    result_flags = SEEK_CUR;
  }

  if (uiFlags & FS_END) {
    result_flags = SEEK_END;
  }

  return result_flags;
}

// filesys.api functions:

HB_FHANDLE hb_fsGetOsHandle(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetOsHandle(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

#if defined(HB_OS_WIN)
  return reinterpret_cast<HB_FHANDLE>(DosToWinHandle(hFileHandle));
#else
  return hFileHandle;
#endif
}

#if defined(HB_OS_UNIX)
// for POSIX systems only, hides low-level select()/poll() access,
// intentionally covered by HB_OS_UNIX macro to generate compile time
// error in code which tries to use it on other platforms

static int hb_fsCanAccess(HB_FHANDLE hFile, HB_MAXINT nTimeOut, bool fRead)
{
  int iResult;

  hb_vmUnlock();

#if defined(HB_HAS_POLL)
  {
    HB_MAXUINT timer = hb_timerInit(nTimeOut);
    struct pollfd fds;
    short int events = fRead ? POLLIN : POLLOUT;

    fds.fd = hFile;
    fds.events = events;
    fds.revents = 0;

    for (;;) {
      bool fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? static_cast<int>(nTimeOut) : 1000;

      iResult = poll(&fds, 1, tout);
      hb_fsSetIOError(iResult >= 0, 0);
      if (iResult > 0 && (fds.revents & events) == 0) {
        if ((fds.revents & (POLLHUP | POLLNVAL | POLLERR)) != 0) {
          iResult = -1;
          break;
        }
        iResult = 0;
      } else if (iResult == -1 && hb_fsOsError() == static_cast<HB_ERRCODE>(EINTR)) {
        iResult = 0;
        fLast = false;
      }

      if (iResult == 0 && !fLast && hb_vmRequestQuery() == 0 && (nTimeOut = hb_timerTest(nTimeOut, &timer)) != 0) {
        continue;
      }

      break;
    }
  }
#else // !HB_HAS_POLL
  {
#if !defined(HB_HAS_SELECT_TIMER)
    HB_MAXUINT timer = hb_timerInit(nTimeOut);
#endif

    for (;;) {
      struct timeval tv;
      fd_set fds;

      if (nTimeOut < 0 || nTimeOut >= 1000) {
        tv.tv_sec = 1;
        tv.tv_usec = 0;
      } else {
        tv.tv_sec = static_cast<long>(nTimeOut) / 1000;
        tv.tv_usec = static_cast<long>(nTimeOut % 1000) * 1000;
      }

      FD_ZERO(&fds);
      FD_SET(hFile, &fds);
      iResult = select(hFile + 1, fRead ? &fds : nullptr, fRead ? nullptr : &fds, nullptr, &tv);
      hb_fsSetIOError(iResult >= 0, 0);

      if (iResult == -1 && hb_fsOsError() == static_cast<HB_ERRCODE>(EINTR)) {
        iResult = 0;
#if defined(HB_HAS_SELECT_TIMER)
        if (nTimeOut > 0) {
          nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
        }
#endif
      }
#if defined(HB_HAS_SELECT_TIMER)
      if (iResult == 0 && nTimeOut > 0) {
        if ((nTimeOut -= 1000) < 0) {
          break;
        }
      }

      if (iResult != 0 || nTimeOut == 0 || hb_vmRequestQuery() != 0) {
        break;
      }
#else
      if (iResult != 0 || (nTimeOut = hb_timerTest(nTimeOut, &timer)) == 0 || hb_vmRequestQuery() != 0) {
        break;
      }
#endif
    }
  }
// #else
//{
//    int iTODO; // TODO: for given platform
//
//    HB_SYMBOL_UNUSED(hFile);
//    HB_SYMBOL_UNUSED(nTimeOut);
//    HB_SYMBOL_UNUSED(fRead);
//    iResult = -1;
// }
#endif // !HB_HAS_POLL

  hb_vmLock();

  return iResult;
}

int hb_fsCanRead(HB_FHANDLE hFileHandle, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCanRead(%p, %" PFHL "d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nTimeOut));
#endif

  return hb_fsCanAccess(hFileHandle, nTimeOut, true);
}

int hb_fsCanWrite(HB_FHANDLE hFileHandle, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCanWrite(%p, %" PFHL "d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nTimeOut));
#endif

  return hb_fsCanAccess(hFileHandle, nTimeOut, false);
}

int hb_fsPoll(PHB_POLLFD pPollSet, int iCount, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPoll(%p, %d, %" PFHL "d)", static_cast<void*>(pPollSet), iCount, nTimeOut));
#endif

  int iResult;

  hb_vmUnlock();

#if defined(HB_HAS_POLL)
  {
    struct pollfd fds[16], *pfds;
    static const bool s_fSamePoll =
        sizeof(struct pollfd) == sizeof(HB_POLLFD) && sizeof(pPollSet->fd) == sizeof(fds[0].fd) &&
        sizeof(pPollSet->events) == sizeof(fds[0].events) && sizeof(pPollSet->revents) == sizeof(fds[0].revents) &&
        HB_POLLIN == POLLIN && HB_POLLPRI == POLLPRI && HB_POLLOUT == POLLOUT && HB_POLLERR == POLLERR &&
        HB_POLLHUP == POLLHUP && HB_POLLNVAL == POLLNVAL;

    HB_MAXUINT timer;
    void *pFree = nullptr;
    int i;

    if (s_fSamePoll) {
      pfds = reinterpret_cast<struct pollfd *>(pPollSet);
    } else {
      if (iCount <= static_cast<int>(HB_SIZEOFARRAY(fds))) {
        pfds = fds;
      } else {
        pfds = static_cast<struct pollfd *>(pFree = hb_xgrab(sizeof(struct pollfd) * iCount));
      }

      for (i = 0; i < iCount; ++i) {
        pfds[i].fd = pPollSet[i].fd;
        pfds[i].events =
            ((pPollSet[i].events & HB_POLLIN) ? POLLIN : 0) | ((pPollSet[i].events & HB_POLLPRI) ? POLLPRI : 0) |
            ((pPollSet[i].events & HB_POLLOUT) ? POLLOUT : 0) | ((pPollSet[i].events & HB_POLLERR) ? POLLERR : 0) |
            ((pPollSet[i].events & HB_POLLHUP) ? POLLHUP : 0) | ((pPollSet[i].events & HB_POLLNVAL) ? POLLNVAL : 0);
        pfds[i].revents = 0;
      }
    }

    timer = hb_timerInit(nTimeOut);
    for (;;) {
      bool fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? static_cast<int>(nTimeOut) : 1000;

      iResult = poll(pfds, iCount, tout);
      hb_fsSetIOError(iResult >= 0, 0);
      if (iResult == -1 && hb_fsOsError() == static_cast<HB_ERRCODE>(EINTR)) {
        iResult = 0;
        fLast = false;
      }
      if (iResult == 0 && !fLast && hb_vmRequestQuery() == 0 && (nTimeOut = hb_timerTest(nTimeOut, &timer)) != 0) {
        continue;
      }

      break;
    }

    if (!s_fSamePoll) {
      for (i = 0; i < iCount; ++i) {
        pPollSet[i].revents =
            ((pfds[i].revents & POLLIN) ? HB_POLLIN : 0) | ((pfds[i].revents & POLLPRI) ? HB_POLLPRI : 0) |
            ((pfds[i].revents & POLLOUT) ? HB_POLLOUT : 0) | ((pfds[i].revents & POLLERR) ? HB_POLLERR : 0) |
            ((pfds[i].revents & POLLHUP) ? HB_POLLHUP : 0) | ((pfds[i].revents & POLLNVAL) ? HB_POLLNVAL : 0);
      }
    }

    if (pFree) {
      hb_xfree(pFree);
    }
  }
#else // !HB_HAS_POLL
  {
#if !defined(HB_HAS_SELECT_TIMER)
    HB_MAXUINT timer = hb_timerInit(nTimeOut);
#endif
    fd_set rfds, wfds, efds;
    int i;

    for (;;) {
      struct timeval tv;
      int iMaxFD = 0;
      bool fLast = nTimeOut >= 0 && nTimeOut <= 1000;

      if (fLast) {
        tv.tv_sec = static_cast<long>(nTimeOut / 1000);
        tv.tv_usec = static_cast<long>(nTimeOut % 1000) * 1000;
      } else {
        tv.tv_sec = 1;
        tv.tv_usec = 0;
      }

      FD_ZERO(&rfds);
      FD_ZERO(&wfds);
      FD_ZERO(&efds);

      for (i = 0; i < iCount; ++i) {
        PHB_POLLFD pSet = pPollSet + i;
        if (pSet->fd >= 0 && (pSet->events & (HB_POLLIN | HB_POLLOUT | HB_POLLPRI))) {
          if (pSet->events & HB_POLLIN) {
            FD_SET(pSet->fd, &rfds);
          }
          if (pSet->events & HB_POLLOUT) {
            FD_SET(pSet->fd, &wfds);
          }
          if (pSet->events & HB_POLLPRI) {
            FD_SET(pSet->fd, &efds);
          }
          if (pSet->fd > iMaxFD) {
            iMaxFD = pSet->fd;
          }
        }
      }

      iResult = select(iMaxFD + 1, &rfds, &wfds, &efds, &tv);
      hb_fsSetIOError(iResult >= 0, 0);

      if (iResult == -1 && hb_fsOsError() == static_cast<HB_ERRCODE>(EINTR)) {
        iResult = 0;
        fLast = false;
#if defined(HB_HAS_SELECT_TIMER)
        if (nTimeOut > 0) {
          nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
        }
#endif
      }
#if defined(HB_HAS_SELECT_TIMER)
      if (iResult == 0 && nTimeOut > 0) {
        if ((nTimeOut -= 1000) < 0) {
          break;
        }
      }

      if (iResult != 0 || fLast || nTimeOut == 0 || hb_vmRequestQuery() != 0) {
        break;
      }
#else
      if (iResult != 0 || fLast || (nTimeOut = hb_timerTest(nTimeOut, &timer)) == 0 || hb_vmRequestQuery() != 0) {
        break;
      }
#endif
    }
    if (iResult > 0) {
      iResult = 0;
      for (i = 0; i < iCount; ++i) {
        PHB_POLLFD pSet = pPollSet + i;
        pSet->revents = 0;
        if (pSet->fd >= 0) {
          if (FD_ISSET(pSet->fd, &rfds)) {
            pSet->revents |= HB_POLLIN;
          }
          if (FD_ISSET(pSet->fd, &wfds)) {
            pSet->revents |= HB_POLLOUT;
          }
          if (FD_ISSET(pSet->fd, &efds)) {
            pSet->revents |= HB_POLLPRI;
          }
          if (pSet->revents != 0) {
            ++iResult;
          }
        }
      }
    }
  }
// #else
//{
//    int iTODO; // TODO: for given platform
//
//    HB_SYMBOL_UNUSED(pPollSet);
//    HB_SYMBOL_UNUSED(iCount);
//    HB_SYMBOL_UNUSED(nTimeOut);
//    iResult = -1;
// }
#endif // !HB_HAS_POLL

  hb_vmLock();

  return iResult;
}
#endif // HB_OS_UNIX

HB_FHANDLE hb_fsPOpen(const char *pszFileName, const char *pszMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPOpen(%p, %s)", static_cast<const void*>(pszFileName), pszMode));
#endif

  HB_FHANDLE hFileHandle = FS_ERROR;

#if defined(HB_OS_UNIX) && !defined(HB_OS_VXWORKS)
  {
    HB_FHANDLE hPipeHandle[2];
    pid_t pid;
    char *pszTmp;
    auto fRead = false;
    HB_SIZE nLen;

    nLen = strlen(pszFileName);
    if (pszMode && (*pszMode == 'r' || *pszMode == 'w')) {
      fRead = (*pszMode == 'r');
    } else {
      if (pszFileName[0] == '|') {
        fRead = false;
      } else if (pszFileName[nLen - 1] == '|') {
        fRead = true;
      } else {
        fRead = false;
      }
    }

    if (pszFileName[0] == '|') {
      ++pszFileName;
      --nLen;
    }
    if (pszFileName[nLen - 1] == '|') {
      pszTmp = hb_strdup(pszFileName);
      pszTmp[--nLen] = 0;
      pszFileName = pszTmp;
    } else {
      pszTmp = nullptr;
    }

    hb_vmUnlock();
    if (pipe(hPipeHandle) == 0) {
      if ((pid = fork()) != -1) {
        if (pid != 0) {
          int iResult, iStatus = 0;

          HB_FAILURE_RETRY(iResult, waitpid(pid, &iStatus, 0));

          iResult = iResult == pid && WIFEXITED(iStatus) && WEXITSTATUS(iStatus) == 0 ? 0 : -1;

          if (iResult != 0) {
            hb_fsClose(hPipeHandle[0]);
            hb_fsClose(hPipeHandle[1]);
          } else if (fRead) {
            hb_fsClose(hPipeHandle[1]);
            hFileHandle = hPipeHandle[0];
          } else {
            hb_fsClose(hPipeHandle[0]);
            hFileHandle = hPipeHandle[1];
          }
        } else {
          HB_FHANDLE hNullHandle;
          int iMaxFD, iResult;

          HB_FAILURE_RETRY(hNullHandle, open("/dev/null", O_RDWR));
          if (fRead) {
            hb_fsClose(hPipeHandle[0]);
            HB_FAILURE_RETRY(iResult, dup2(hPipeHandle[1], 1));
            HB_FAILURE_RETRY(iResult, dup2(hNullHandle, 0));
            HB_FAILURE_RETRY(iResult, dup2(hNullHandle, 2));
          } else {
            hb_fsClose(hPipeHandle[1]);
            HB_FAILURE_RETRY(iResult, dup2(hPipeHandle[0], 0));
            HB_FAILURE_RETRY(iResult, dup2(hNullHandle, 1));
            HB_FAILURE_RETRY(iResult, dup2(hNullHandle, 2));
          }
          iMaxFD = sysconf(_SC_OPEN_MAX);
          if (iMaxFD < 3) {
            iMaxFD = 1024;
          }
          for (hNullHandle = 3; hNullHandle < iMaxFD; ++hNullHandle) {
            hb_fsClose(hNullHandle);
          }

          pid = fork();
          if (pid == 0) {
            const char *argv[4];

            argv[0] = "sh";
            argv[1] = "-c";
            argv[2] = pszFileName;
            argv[3] = 0;

            if (setuid(getuid()) == -1) {
            }
            if (setgid(getgid()) == -1) {
            }
            HB_FAILURE_RETRY(iResult, execv("/bin/sh", static_cast<char **>(HB_UNCONST(argv))));
          }
          _exit(pid > 0 ? EXIT_SUCCESS : EXIT_FAILURE);
        }
      } else {
        hb_fsSetIOError(hFileHandle != FS_ERROR, 0);
        hb_fsCloseRaw(hPipeHandle[0]);
        hb_fsCloseRaw(hPipeHandle[1]);
      }
    } else {
      hb_fsSetIOError(hFileHandle != FS_ERROR, 0);
    }
    hb_vmLock();

    if (pszTmp) {
      hb_xfree(pszTmp);
    }
  }
#else

  HB_SYMBOL_UNUSED(pszFileName);
  HB_SYMBOL_UNUSED(pszMode);

  hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));

#endif

  return hFileHandle;
}

HB_BOOL hb_fsPipeCreate(HB_FHANDLE hPipe[2])
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeCreate(%p)", static_cast<void*>(hPipe)));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    SECURITY_ATTRIBUTES sa;
    HANDLE hPipeRd, hPipeWr;

    memset(&sa, 0, sizeof(sa));
    sa.nLength = sizeof(sa);
    sa.bInheritHandle = TRUE;

    fResult = CreatePipe(&hPipeRd, &hPipeWr, &sa, 0) != 0;
    if (fResult) {
      hPipe[0] = reinterpret_cast<HB_FHANDLE>(hPipeRd);
      hPipe[1] = reinterpret_cast<HB_FHANDLE>(hPipeWr);
    } else {
      hPipe[0] = hPipe[1] = FS_ERROR;
    }
    hb_fsSetIOError(fResult, 0);
  }
#elif defined(HB_OS_UNIX) && !defined(HB_OS_VXWORKS)
  {
    fResult = pipe(hPipe) == 0;
    if (!fResult) {
      hPipe[0] = hPipe[1] = FS_ERROR;
    }
    hb_fsSetIOError(fResult, 0);
  }
#else
  {
    int iTODO; // TODO: for given platform

    hPipe[0] = hPipe[1] = FS_ERROR;
    hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
    fResult = false;
  }
#endif

  return fResult;
}

int hb_fsIsPipeOrSock(HB_FHANDLE hPipeHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsPipeOrSock(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hPipeHandle))));
#endif

#if defined(HB_OS_UNIX)
  {
#if defined(HB_USE_LARGEFILE64)
    struct stat64 statbuf;
    int ret = fstat64(hPipeHandle, &statbuf);
#else
    struct stat statbuf;
    int ret = fstat(hPipeHandle, &statbuf);
#endif
    hb_fsSetIOError(ret == 0, 0);
    return ret == 0 && (S_ISFIFO(statbuf.st_mode) || S_ISSOCK(statbuf.st_mode)) ? 1 : 0;
  }
#elif defined(HB_OS_WIN)
  {
    DWORD type = GetFileType(reinterpret_cast<HANDLE>(hb_fsGetOsHandle(hPipeHandle)));
    hb_fsSetIOError(type != FILE_TYPE_UNKNOWN || GetLastError() == NO_ERROR, 0);
    return type == FILE_TYPE_PIPE ? 1 : 0;
  }
#else
  int iTODO; // TODO: for given platform
  HB_SYMBOL_UNUSED(hPipeHandle);
  hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
  return 0;
#endif
}

HB_BOOL hb_fsPipeUnblock(HB_FHANDLE hPipeHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeUnblock(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hPipeHandle))));
#endif

#if defined(HB_OS_WIN)
  {
    DWORD dwMode = PIPE_NOWAIT;
    auto fResult = false;

    fResult = SetNamedPipeHandleState(reinterpret_cast<HANDLE>(hb_fsGetOsHandle(hPipeHandle)), &dwMode, nullptr,
                                      nullptr) != 0;
    hb_fsSetIOError(fResult, 0);
    return fResult;
  }
#elif defined(HB_OS_UNIX) && !defined(HB_OS_MINIX)
  {
    int ret = fcntl(hPipeHandle, F_GETFL, 0);

    if (ret != -1 && (ret & O_NONBLOCK) == 0) {
      ret = fcntl(hPipeHandle, F_SETFL, ret | O_NONBLOCK);
    }
    hb_fsSetIOError(ret != -1, 0);

    return ret != -1;
  }
#else
  {
    int iTODO; // TODO: for given platform
    HB_SYMBOL_UNUSED(hPipeHandle);
    hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
    return false;
  }
#endif
}

HB_SIZE hb_fsPipeIsData(HB_FHANDLE hPipeHandle, HB_SIZE nBufferSize, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeIsData(%p,%" HB_PFS "u,%" PFHL "d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hPipeHandle)), nBufferSize, nTimeOut));
#endif

  HB_SIZE nToRead = 0;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    HB_MAXUINT timer = hb_timerInit(nTimeOut);
    auto fResult = false;
    DWORD dwAvail;

    do {
      if (fResult) {
        hb_releaseCPU();
      }

      dwAvail = 0;
      fResult = PeekNamedPipe(reinterpret_cast<HANDLE>(hb_fsGetOsHandle(hPipeHandle)), nullptr, 0, nullptr, &dwAvail,
                              nullptr) != 0;
      if (!fResult && GetLastError() == ERROR_BROKEN_PIPE) {
        hb_fsSetError(0);
        break;
      }
      hb_fsSetIOError(fResult, 0);
    } while (fResult && dwAvail == 0 && (nTimeOut = hb_timerTest(nTimeOut, &timer)) != 0 && hb_vmRequestQuery() == 0);

    if (!fResult) {
      nToRead = static_cast<HB_SIZE>(FS_ERROR);
    } else if (dwAvail > 0) {
      nToRead = (static_cast<HB_SIZE>(dwAvail) < nBufferSize) ? dwAvail : nBufferSize;
    }
  }
#elif defined(HB_OS_UNIX)
  {
    int iResult = hb_fsCanRead(hPipeHandle, nTimeOut);

    if (iResult > 0) {
      nToRead = nBufferSize;
    } else if (iResult < 0) {
      nToRead = static_cast<HB_SIZE>(FS_ERROR);
    }
  }
#else
  {
    int iTODO; // TODO: for given platform
    HB_SYMBOL_UNUSED(hPipeHandle);
    HB_SYMBOL_UNUSED(nBufferSize);
    HB_SYMBOL_UNUSED(nTimeOut);
    hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
  }
#endif

  hb_vmLock();

  return nToRead;
}

HB_SIZE hb_fsPipeRead(HB_FHANDLE hPipeHandle, void *buffer, HB_SIZE nSize, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeRead(%p,%p,%" HB_PFS "u,%" PFHL "d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hPipeHandle)), buffer, nSize, nTimeOut));
#endif

  HB_SIZE nRead;

  nRead = hb_fsPipeIsData(hPipeHandle, nSize, nTimeOut);
  if (nRead != static_cast<HB_SIZE>(FS_ERROR) && nRead > 0) {
    nRead = hb_fsReadLarge(hPipeHandle, buffer, nRead);
    if (nRead == 0) {
      nRead = static_cast<HB_SIZE>(FS_ERROR);
    }
  }

  return nRead;
}

HB_SIZE hb_fsPipeWrite(HB_FHANDLE hPipeHandle, const void *buffer, HB_SIZE nSize, HB_MAXINT nTimeOut)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeWrite(%p,%p,%" HB_PFS "u,%" PFHL "d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hPipeHandle)), buffer, nSize, nTimeOut));
#endif

  HB_SIZE nWritten;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    auto hPipe = reinterpret_cast<HANDLE>(hb_fsGetOsHandle(hPipeHandle));
    DWORD dwMode = 0;

    if (GetNamedPipeHandleState(hPipe, &dwMode, nullptr, nullptr, nullptr, nullptr, 0)) {
      HB_MAXUINT timer = hb_timerInit(nTimeOut);
      auto fResult = false;

      if ((dwMode & PIPE_NOWAIT) == 0) {
        DWORD dwNewMode = dwMode | PIPE_NOWAIT;
        SetNamedPipeHandleState(hPipe, &dwNewMode, nullptr, nullptr);
      }

      nWritten = 0;
      do {
        DWORD dwWritten, dwToWrite;

        if (fResult) {
          hb_releaseCPU();
        }

        dwToWrite = static_cast<DWORD>(nSize - nWritten);
        // real life tests show that MSDN is wrong and MS-Windows
        // refuse to accept even single byte if data is longer then
        // size of PIPE buffer in unblocking mode [druzus]
        if (dwToWrite > 4096) {
          dwToWrite = 4096;
        }
        fResult =
            WriteFile(hPipe, static_cast<const HB_BYTE *>(buffer) + nWritten, dwToWrite, &dwWritten, nullptr) != 0;
        if (fResult) {
          nWritten += static_cast<HB_SIZE>(dwWritten);
        } else if (nWritten == 0) {
          nWritten = static_cast<HB_SIZE>(FS_ERROR);
        }
        hb_fsSetIOError(fResult, 0);
      } while (fResult && nWritten < nSize && (nTimeOut = hb_timerTest(nTimeOut, &timer)) != 0 &&
               hb_vmRequestQuery() == 0);

      if ((dwMode & PIPE_NOWAIT) == 0) {
        SetNamedPipeHandleState(hPipe, &dwMode, nullptr, nullptr);
      }
    } else {
      hb_fsSetIOError(false, 0);
      nWritten = static_cast<HB_SIZE>(FS_ERROR);
    }
  }
#elif defined(HB_OS_UNIX)
  {
    int iResult = hb_fsCanWrite(hPipeHandle, nTimeOut);

    if (iResult > 0) {
      int iFlags = -1;

      iResult = fcntl(hPipeHandle, F_GETFL, 0);
      if (iResult != -1 && (iResult & O_NONBLOCK) == 0) {
        iFlags = iResult;
        iResult = fcntl(hPipeHandle, F_SETFL, iResult | O_NONBLOCK);
      }
      if (iResult == -1) {
        hb_fsSetIOError(false, 0);
        nWritten = static_cast<HB_SIZE>(FS_ERROR);
      } else {
        nWritten = hb_fsWriteLarge(hPipeHandle, buffer, nSize);
      }
      if (iFlags != -1) {
        fcntl(hPipeHandle, F_SETFL, iFlags);
      }
    } else {
      nWritten = static_cast<HB_SIZE>(iResult);
    }
  }
#else
  {
    int iTODO; // TODO: for given platform
    HB_SYMBOL_UNUSED(nTimeOut);
    nWritten = hb_fsWriteLarge(hPipeHandle, buffer, nSize);
  }
#endif

  hb_vmLock();

  return nWritten;
}

HB_FHANDLE hb_fsCreate(const char *pszFileName, HB_FATTR nAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%s, %u)", pszFileName, nAttr));
#endif

  return hb_fsOpenEx(pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | FO_EXCLUSIVE, nAttr);
}

HB_FHANDLE hb_fsCreateEx(const char *pszFileName, HB_FATTR nAttr, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreateEx(%s, %u, %hu)", pszFileName, nAttr, uiFlags));
#endif

  uiFlags &= ~(FO_READ | FO_WRITE | FO_READWRITE);

  return hb_fsOpenEx(pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | uiFlags, nAttr);
}

HB_FHANDLE hb_fsOpen(const char *pszFileName, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpen(%s, %hu)", pszFileName, uiFlags));
#endif

  return hb_fsOpenEx(pszFileName, uiFlags, FC_NORMAL);
}

HB_FHANDLE hb_fsOpenEx(const char *pszFileName, HB_USHORT uiFlags, HB_FATTR nAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpenEx(%s, %hu, %u)", pszFileName, uiFlags, nAttr));
#endif

  HB_FHANDLE hFileHandle;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpFileName;
    LPTSTR lpFree;
    DWORD dwMode, dwShare, dwCreat, dwAttr;
    HANDLE hFile;

    lpFileName = HB_FSNAMECONV(pszFileName, &lpFree);

    convert_open_flags(false, nAttr, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr);

    hb_vmUnlock();
    hFile = CreateFile(lpFileName, dwMode, dwShare, nullptr, dwCreat, dwAttr, nullptr);
    hb_fsSetIOError(hFile != static_cast<HANDLE>(INVALID_HANDLE_VALUE), 0);
    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }

    hFileHandle = reinterpret_cast<HB_FHANDLE>(hFile);
  }
#else
  {
    char *pszFree;
    int flags, share, attr;
    unsigned mode;

    pszFileName = hb_fsNameConv(pszFileName, &pszFree);

    convert_open_flags(false, nAttr, uiFlags, &flags, &mode, &share, &attr);

    hb_vmUnlock();

#if defined(_MSC_VER)
    if (share) {
      hFileHandle = _sopen(pszFileName, flags, share, mode);
    } else {
      hFileHandle = _open(pszFileName, flags, mode);
    }
    hb_fsSetIOError(hFileHandle != FS_ERROR, 0);
#elif defined(HB_FS_SOPEN)
    if (share) {
      hFileHandle = sopen(pszFileName, flags, share, mode);
    } else {
      hFileHandle = open(pszFileName, flags, mode);
    }
    hb_fsSetIOError(hFileHandle != FS_ERROR, 0);
#else
    HB_FAILURE_RETRY(hFileHandle, open(pszFileName, flags | share, mode));
#endif

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return hFileHandle;
}

void hb_fsCloseRaw(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCloseRaw(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

  hb_vmUnlock();
#if defined(HB_OS_WIN)
  CloseHandle(DosToWinHandle(hFileHandle));
#else
  {
#if defined(EINTR)
    int ret;
    // ignoring EINTR in close() it's quite common bug when sockets or
    // pipes are used. Without such protection it's not safe to use
    // signals in user code.
    do {
      ret = close(hFileHandle);
    } while (ret == -1 && errno == EINTR);
#else
    close(hFileHandle);
#endif
  }
#endif
  hb_vmLock();
}

void hb_fsClose(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

  hb_vmUnlock();
#if defined(HB_OS_WIN)
  hb_fsSetIOError(CloseHandle(DosToWinHandle(hFileHandle)) != 0, 0);
#else
  {
    int ret;
#if defined(EINTR)
    // ignoring EINTR in close() it's quite common bug when sockets or
    // pipes are used. Without such protection it's not safe to use
    // signals in user code.
    do {
      ret = close(hFileHandle);
    } while (ret == -1 && errno == EINTR);
#else
    ret = close(hFileHandle);
#endif
    hb_fsSetIOError(ret == 0, 0);
  }
#endif
  hb_vmLock();
}

#define FD_TEST 0

int hb_fsSetDevMode(HB_FHANDLE hFileHandle, int iDevMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetDevMode(%p, %d)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), iDevMode));
#endif

  HB_SYMBOL_UNUSED(hFileHandle);

  hb_fsSetError(static_cast<HB_ERRCODE>(iDevMode == FD_TEXT ? FS_ERROR : 0));
  return FD_BINARY;
}

HB_BOOL hb_fsGetFileTime(const char *pszFileName, long *plJulian, long *plMillisec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetFileTime(%s, %p, %p)", pszFileName, static_cast<void*>(plJulian), static_cast<void*>(plMillisec)));
#endif

  auto fResult = false;
  *plJulian = *plMillisec = 0;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpFileName;
    LPTSTR lpFree;
    WIN32_FILE_ATTRIBUTE_DATA attrex;

    lpFileName = HB_FSNAMECONV(pszFileName, &lpFree);

    memset(&attrex, 0, sizeof(attrex));

    if (GetFileAttributesEx(lpFileName, GetFileExInfoStandard, &attrex)) {
      FILETIME local_ft;
      SYSTEMTIME st;

      if (FileTimeToLocalFileTime(&attrex.ftLastWriteTime, &local_ft) && FileTimeToSystemTime(&local_ft, &st)) {
        *plJulian = hb_dateEncode(st.wYear, st.wMonth, st.wDay);
        *plMillisec = hb_timeEncode(st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);

        fResult = true;
      }
    }
    hb_fsSetIOError(fResult, 0);

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#elif defined(HB_OS_UNIX) || defined(__GNUC__)
  {
    char *pszFree;
#if defined(HB_USE_LARGEFILE64)
    struct stat64 statbuf;
    if (stat64(hb_fsNameConv(pszFileName, &pszFree), &statbuf) == 0)
#else
    struct stat statbuf;
    if (stat(hb_fsNameConv(pszFileName, &pszFree), &statbuf) == 0)
#endif
    {
      time_t ftime;
      struct tm ft;

      ftime = statbuf.st_mtime;
#if defined(HB_HAS_LOCALTIME_R)
      localtime_r(&ftime, &ft);
#else
      ft = *localtime(&ftime);
#endif

      *plJulian = hb_dateEncode(ft.tm_year + 1900, ft.tm_mon + 1, ft.tm_mday);
#if defined(HB_OS_LINUX) && (defined(_BSD_SOURCE) || defined(_SVID_SOURCE)) && defined(__GLIBC__) &&                   \
    defined(__GLIBC_MINOR__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 6))
      *plMillisec = hb_timeEncode(ft.tm_hour, ft.tm_min, ft.tm_sec, statbuf.st_mtim.tv_nsec / 1000000);
#else
      *plMillisec = hb_timeEncode(ft.tm_hour, ft.tm_min, ft.tm_sec, 0);
#endif
      fResult = true;
    }
    hb_fsSetIOError(fResult, 0);

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#else
  {
    int iTODO; // TODO: for given platform

    HB_SYMBOL_UNUSED(pszFileName);
  }
#endif

  hb_vmLock();

  return fResult;
}

HB_BOOL hb_fsGetAttr(const char *pszFileName, HB_FATTR *pnAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetAttr(%s, %p)", pszFileName, static_cast<void*>(pnAttr)));
#endif

  auto fResult = false;

  hb_vmUnlock();

  *pnAttr = 0;
#if defined(HB_OS_WIN)
  {
    LPCTSTR lpFileName;
    LPTSTR lpFree;
    DWORD dwAttr;

    lpFileName = HB_FSNAMECONV(pszFileName, &lpFree);

    dwAttr = GetFileAttributes(lpFileName);

    if (dwAttr != INVALID_FILE_ATTRIBUTES) {
      *pnAttr = hb_fsAttrFromRaw(dwAttr);
      fResult = true;
    }
    hb_fsSetIOError(fResult, 0);

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;
    pszFileName = hb_fsNameConv(pszFileName, &pszFree);

#if defined(HB_OS_UNIX)
    {
#if defined(HB_USE_LARGEFILE64)
      struct stat64 statbuf;
      if (stat64(pszFileName, &statbuf) == 0)
#else
      struct stat statbuf;
      if (stat(pszFileName, &statbuf) == 0)
#endif
      {
        *pnAttr = hb_fsAttrFromRaw(statbuf.st_mode);
        fResult = true;
      }
      hb_fsSetIOError(fResult, 0);
    }
#else
    {
      int iTODO; // TODO: for given platform

      HB_SYMBOL_UNUSED(pszFileName);
    }
#endif
    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  hb_vmLock();

  return fResult;
}

HB_BOOL hb_fsSetFileTime(const char *pszFileName, long lJulian, long lMillisec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetFileTime(%s, %ld, %ld)", pszFileName, lJulian, lMillisec));
#endif

  auto fResult = false;
  int iYear, iMonth, iDay;
  int iHour, iMinute, iSecond, iMSec;

  hb_dateDecode(lJulian, &iYear, &iMonth, &iDay);
  hb_timeDecode(lMillisec, &iHour, &iMinute, &iSecond, &iMSec);

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    HB_FHANDLE hFile = hb_fsOpen(pszFileName, FO_READWRITE | FO_SHARED);

    fResult = hFile != FS_ERROR;
    if (fResult) {
      FILETIME local_ft;
      SYSTEMTIME st;

      if (lJulian <= 0 || lMillisec < 0) {
        GetLocalTime(&st);
      } else {
        memset(&st, 0, sizeof(st));
      }

      if (lJulian > 0) {
        st.wYear = static_cast<WORD>(iYear);
        st.wMonth = static_cast<WORD>(iMonth);
        st.wDay = static_cast<WORD>(iDay);
      }
      if (lMillisec >= 0) {
        st.wHour = static_cast<WORD>(iHour);
        st.wMinute = static_cast<WORD>(iMinute);
        st.wSecond = static_cast<WORD>(iSecond);
        st.wMilliseconds = static_cast<WORD>(iMSec);
      }

      if (SystemTimeToFileTime(&st, &local_ft)) {
        FILETIME ft;
        LocalFileTimeToFileTime(&local_ft, &ft);
        fResult = SetFileTime(DosToWinHandle(hFile), nullptr, &ft, &ft) != 0;
      } else {
        fResult = false;
      }

      hb_fsSetIOError(fResult, 0);
      hb_fsClose(hFile);
    }
  }
#elif defined(HB_OS_UNIX)
  {
    char *pszFree;

    pszFileName = hb_fsNameConv(pszFileName, &pszFree);

    if (lJulian <= 0 && lMillisec < 0) {
#if defined(HB_OS_LINUX)
      fResult = utimes(pszFileName, nullptr) == 0;
#else
      fResult = utime(pszFileName, nullptr) == 0;
#endif
    } else {
      struct tm new_value;

      if (lJulian <= 0 || lMillisec < 0) {
        time_t current_time;

        current_time = time(nullptr);
#if defined(HB_HAS_LOCALTIME_R)
        localtime_r(&current_time, &new_value);
#else
        new_value = *localtime(&current_time);
#endif
      } else {
        memset(&new_value, 0, sizeof(new_value));
      }

      if (lJulian > 0) {
        new_value.tm_year = iYear - 1900;
        new_value.tm_mon = iMonth - 1;
        new_value.tm_mday = iDay;
      }
      if (lMillisec >= 0) {
        new_value.tm_hour = iHour;
        new_value.tm_min = iMinute;
        new_value.tm_sec = iSecond;
      }
      new_value.tm_isdst = -1;

#if defined(HB_OS_LINUX)
      {
        struct timeval times[2];
        times[0].tv_sec = times[1].tv_sec = mktime(&new_value);
        times[0].tv_usec = times[1].tv_usec = iMSec * 1000;
        fResult = utimes(pszFileName, times) == 0;
      }
#else
      {
        struct utimbuf buf;
        buf.actime = buf.modtime = mktime(&new_value);
        fResult = utime(pszFileName, &buf) == 0;
      }
#endif
    }
    hb_fsSetIOError(fResult, 0);
    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#else
  {
    int iTODO; // To force warning

    fResult = false;
    hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
  }
#endif

  hb_vmLock();

  return fResult;
}

HB_BOOL hb_fsSetAttr(const char *pszFileName, HB_FATTR nAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetAttr(%s, %u)", pszFileName, nAttr));
#endif

  auto fResult = false;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpFileName;
    LPTSTR lpFree;
    DWORD dwFlags = 0;

    lpFileName = HB_FSNAMECONV(pszFileName, &lpFree);

    if (nAttr & HB_FA_READONLY) {
      dwFlags |= FILE_ATTRIBUTE_READONLY;
    }
    if (nAttr & HB_FA_HIDDEN) {
      dwFlags |= FILE_ATTRIBUTE_HIDDEN;
    }
    if (nAttr & HB_FA_SYSTEM) {
      dwFlags |= FILE_ATTRIBUTE_SYSTEM;
    }
    if (nAttr & HB_FA_ARCHIVE) {
      dwFlags |= FILE_ATTRIBUTE_ARCHIVE;
    }
    if (dwFlags == 0) {
      dwFlags = FILE_ATTRIBUTE_NORMAL;
    }
    fResult = SetFileAttributes(lpFileName, dwFlags) != 0;
    hb_fsSetIOError(fResult, 0);

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;

    pszFileName = hb_fsNameConv(pszFileName, &pszFree);

#if defined(HB_OS_UNIX)
    {
      int iAttr = HB_FA_POSIX_ATTR(nAttr), iResult;
      if (iAttr == 0) {
        iAttr = S_IRUSR | S_IRGRP | S_IROTH;
        if (!(nAttr & HB_FA_READONLY)) {
          iAttr |= S_IWUSR | S_IWGRP | S_IWOTH;
        }
        if (nAttr & HB_FA_SYSTEM) {
          iAttr |= S_IXUSR | S_IXGRP | S_IXOTH;
        }
        if (nAttr & HB_FA_HIDDEN) {
          iAttr &= S_IRUSR | S_IWUSR | S_IXUSR;
        }
      }
      HB_FAILURE_RETRY(iResult, chmod(pszFileName, iAttr));
      fResult = iResult != -1;
    }
#else
    {
      int iTODO; // To force warning

      fResult = false;
      hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
    }
#endif
    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  hb_vmLock();

  return fResult;
}

HB_USHORT hb_fsRead(HB_FHANDLE hFileHandle, void *pBuff, HB_USHORT uiCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsRead(%p, %p, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, uiCount));
#endif

  HB_USHORT uiRead;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    DWORD dwRead;
    BOOL bResult;

    bResult = ReadFile(DosToWinHandle(hFileHandle), pBuff, static_cast<DWORD>(uiCount), &dwRead, nullptr);
    hb_fsSetIOError(bResult != 0, 0);

    uiRead = bResult ? static_cast<HB_USHORT>(dwRead) : 0;
  }
#else
  {
    long lRead;
    HB_FAILURE_RETRY(lRead, read(hFileHandle, pBuff, uiCount));
    uiRead = lRead == -1 ? 0 : static_cast<HB_USHORT>(lRead);
  }
#endif

  hb_vmLock();

  return uiRead;
}

HB_USHORT hb_fsWrite(HB_FHANDLE hFileHandle, const void *pBuff, HB_USHORT uiCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsWrite(%p, %p, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, uiCount));
#endif

  HB_USHORT uiWritten = 0;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    BOOL bResult;

    if (uiCount) {
      DWORD dwWritten = 0;
      bResult = WriteFile(DosToWinHandle(hFileHandle), pBuff, uiCount, &dwWritten, nullptr);
      uiWritten = bResult ? static_cast<HB_USHORT>(dwWritten) : 0;
    } else {
      bResult = SetEndOfFile(DosToWinHandle(hFileHandle));
    }
    hb_fsSetIOError(bResult != 0, 0);
  }
#else
  if (uiCount) {
    long lWritten;
    HB_FAILURE_RETRY(lWritten, write(hFileHandle, pBuff, uiCount));
    uiWritten = lWritten == -1 ? 0 : static_cast<HB_USHORT>(lWritten);
  } else {
    int iResult;
#if defined(HB_USE_LARGEFILE64)
    HB_FAILURE_RETRY(iResult, ftruncate64(hFileHandle, lseek64(hFileHandle, 0L, SEEK_CUR)));
#else
    HB_FAILURE_RETRY(iResult, ftruncate(hFileHandle, lseek(hFileHandle, 0L, SEEK_CUR)));
#endif
  }
#endif

  hb_vmLock();

  return uiWritten;
}

HB_SIZE hb_fsReadLarge(HB_FHANDLE hFileHandle, void *pBuff, HB_SIZE nCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLarge(%p, %p, %" HB_PFS "u)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, nCount));
#endif

  HB_SIZE nRead;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
#if defined(HB_WIN_IOREAD_LIMIT)
    HANDLE hWFileHandle = DosToWinHandle(hFileHandle);
    BOOL bResult = TRUE;

    nRead = 0;

    while (nCount) {
      DWORD dwToRead;
      DWORD dwRead;

      // Determine how much to read this time
      if (nCount > static_cast<HB_SIZE>(HB_WIN_IOREAD_LIMIT)) {
        dwToRead = HB_WIN_IOREAD_LIMIT;
        nCount -= static_cast<HB_SIZE>(dwToRead);
      } else {
        dwToRead = static_cast<DWORD>(nCount);
        nCount = 0;
      }

      bResult = ReadFile(hWFileHandle, static_cast<HB_UCHAR *>(pBuff) + nRead, dwToRead, &dwRead, nullptr);
      if (!bResult) {
        break;
      }

      nRead += static_cast<HB_SIZE>(dwRead);

      if (dwRead != dwToRead) {
        break;
      }
    }
#else
    DWORD dwRead;
    BOOL bResult;

    bResult = ReadFile(DosToWinHandle(hFileHandle), pBuff, nCount, &dwRead, nullptr);
    nRead = bResult ? static_cast<HB_SIZE>(dwRead) : 0;
#endif
    hb_fsSetIOError(bResult != 0, 0);
  }
#elif defined(HB_FS_IO_16BIT)
  {
    nRead = 0;

    while (nCount) {
      unsigned int uiToRead;
      long lRead;

      // Determine how much to read this time
      if (nCount > static_cast<HB_SIZE>(INT_MAX)) {
        uiToRead = INT_MAX;
        nCount -= static_cast<HB_SIZE>(uiToRead);
      } else {
        uiToRead = static_cast<unsigned int>(nCount);
        nCount = 0;
      }

      HB_FAILURE_RETRY(lRead, read(hFileHandle, static_cast<HB_UCHAR *>(pBuff) + nRead, uiToRead));

      if (lRead <= 0) {
        break;
      }

      nRead += lRead;

      if (lRead != static_cast<long>(uiToRead)) {
        break;
      }
    }
  }
#else
  {
    long lRead;
    HB_FAILURE_RETRY(lRead, read(hFileHandle, pBuff, nCount));
    nRead = lRead == -1 ? 0 : lRead;
  }
#endif

  hb_vmLock();

  return nRead;
}

HB_SIZE hb_fsWriteLarge(HB_FHANDLE hFileHandle, const void *pBuff, HB_SIZE nCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteLarge(%p, %p, %" HB_PFS "u)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, nCount));
#endif

  HB_SIZE nWritten = 0;

  hb_vmUnlock();

#if defined(HB_OS_WIN)

  if (nCount) {
#if defined(HB_WIN_IOWRITE_LIMIT)
    HANDLE hWFileHandle = DosToWinHandle(hFileHandle);
    BOOL bResult = TRUE;

    while (nCount) {
      DWORD dwToWrite;
      DWORD dwWritten;

      // Determine how much to write this time
      if (nCount > static_cast<HB_SIZE>(HB_WIN_IOWRITE_LIMIT)) {
        dwToWrite = HB_WIN_IOWRITE_LIMIT;
        nCount -= static_cast<HB_SIZE>(dwToWrite);
      } else {
        dwToWrite = static_cast<DWORD>(nCount);
        nCount = 0;
      }

      bResult =
          WriteFile(hWFileHandle, static_cast<const HB_UCHAR *>(pBuff) + nWritten, dwToWrite, &dwWritten, nullptr);
      if (!bResult) {
        break;
      }

      nWritten += static_cast<HB_SIZE>(dwWritten);

      if (dwWritten != dwToWrite) {
        break;
      }
    }
#else
    DWORD dwWritten;
    BOOL bResult;
    bResult = WriteFile(DosToWinHandle(hFileHandle), pBuff, nCount, &dwWritten, nullptr);
    if (bResult) {
      nWritten = static_cast<HB_SIZE>(dwWritten);
    }
#endif
    hb_fsSetIOError(bResult != 0, 0);
  } else {
    hb_fsSetIOError(SetEndOfFile(DosToWinHandle(hFileHandle)) != 0, 0);
  }

#else

  if (nCount) {
#if defined(HB_FS_IO_16BIT)
    while (nCount) {
      unsigned int uiToWrite;
      long lWritten;

      // Determine how much to write this time
      if (nCount > static_cast<HB_SIZE>(INT_MAX)) {
        uiToWrite = INT_MAX;
        nCount -= static_cast<HB_SIZE>(uiToWrite);
      } else {
        uiToWrite = static_cast<unsigned int>(nCount);
        nCount = 0;
      }

      HB_FAILURE_RETRY(lWritten, write(hFileHandle, static_cast<const HB_UCHAR *>(pBuff) + nWritten, uiToWrite));

      if (lWritten <= 0) {
        break;
      }

      nWritten += lWritten;

      if (lWritten != static_cast<long>(uiToWrite)) {
        break;
      }
    }
#else
    long lWritten;
    HB_FAILURE_RETRY(lWritten, write(hFileHandle, pBuff, nCount));
    nWritten = lWritten == -1 ? 0 : lWritten;
#endif
  } else {
    int iResult;
#if defined(HB_USE_LARGEFILE64)
    HB_FAILURE_RETRY(iResult, ftruncate64(hFileHandle, lseek64(hFileHandle, 0L, SEEK_CUR)));
#else
    HB_FAILURE_RETRY(iResult, ftruncate(hFileHandle, lseek(hFileHandle, 0L, SEEK_CUR)));
#endif
  }
#endif

  hb_vmLock();

  return nWritten;
}

HB_SIZE hb_fsReadAt(HB_FHANDLE hFileHandle, void *pBuff, HB_SIZE nCount, HB_FOFFSET nOffset)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, nCount, nOffset));
#endif

  HB_SIZE nRead;

  hb_vmUnlock();

#if defined(HB_OS_UNIX) && !defined(HB_OS_VXWORKS)
  {
    long lRead;
#if defined(HB_USE_LARGEFILE64)
    HB_FAILURE_RETRY(lRead, pread64(hFileHandle, pBuff, nCount, nOffset));
#else
    HB_FAILURE_RETRY(lRead, pread(hFileHandle, pBuff, nCount, nOffset));
#endif
    nRead = lRead == -1 ? 0 : lRead;
  }
#elif defined(HB_OS_WIN)
#if defined(HB_WIN_IOREAD_LIMIT)
  {
    HANDLE hWFileHandle = DosToWinHandle(hFileHandle);
    OVERLAPPED Overlapped;
    BOOL bResult = TRUE;

    memset(&Overlapped, 0, sizeof(Overlapped));
    Overlapped.Offset = static_cast<DWORD>(nOffset & 0xFFFFFFFF);
    Overlapped.OffsetHigh = static_cast<DWORD>(nOffset >> 32);

    nRead = 0;
    while (nCount) {
      DWORD dwToRead;
      DWORD dwRead;

      if (nCount > static_cast<HB_SIZE>(HB_WIN_IOREAD_LIMIT)) {
        dwToRead = HB_WIN_IOREAD_LIMIT;
        nCount -= static_cast<HB_SIZE>(dwToRead);
      } else {
        dwToRead = static_cast<DWORD>(nCount);
        nCount = 0;
      }

      bResult = ReadFile(hWFileHandle, static_cast<HB_UCHAR *>(pBuff) + nRead, dwToRead, &dwRead, &Overlapped);

      if (!bResult) {
        break;
      }

      nRead += static_cast<HB_SIZE>(dwRead);

      if (dwRead != dwToRead) {
        break;
      }
    }
    hb_fsSetIOError(bResult != 0, 0);
  }
#else
  if (hb_iswinnt()) {
    DWORD dwRead = 0;
    OVERLAPPED Overlapped;
    memset(&Overlapped, 0, sizeof(Overlapped));
    Overlapped.Offset = static_cast<DWORD>(nOffset & 0xFFFFFFFF);
    Overlapped.OffsetHigh = static_cast<DWORD>(nOffset >> 32);
    hb_fsSetIOError(ReadFile(DosToWinHandle(hFileHandle), pBuff, static_cast<DWORD>(nCount), &dwRead, &Overlapped) != 0,
                    0);
    nRead = dwRead;
  } else {
    auto ulOffsetLow = static_cast<ULONG>(nOffset & 0xFFFFFFFF);
    auto ulOffsetHigh = static_cast<ULONG>(nOffset >> 32);
    ulOffsetLow =
        SetFilePointer(DosToWinHandle(hFileHandle), ulOffsetLow, reinterpret_cast<PLONG>(&ulOffsetHigh), SEEK_SET);
    if (ulOffsetLow == static_cast<ULONG>(INVALID_SET_FILE_POINTER) && GetLastError() != NO_ERROR) {
      hb_fsSetIOError(false, 0);
      nRead = 0;
    } else {
      DWORD dwRead = 0;
      hb_fsSetIOError(ReadFile(DosToWinHandle(hFileHandle), pBuff, static_cast<DWORD>(nCount), &dwRead, nullptr) != 0,
                      0);
      nRead = dwRead;
    }
  }
#endif // HB_WIN_IOREAD_LIMIT

// FIXME: below are not atom operations. It has to be fixed for RDD
//        file access with shared file handles in aliased work areas. TOCHECK: OS/2 ?
#elif defined(HB_FS_IO_16BIT)
  if (hb_fsSeekLarge(hFileHandle, nOffset, FS_SET) == nOffset) {
    nRead = hb_fsReadLarge(hFileHandle, pBuff, nCount);
  } else {
    nRead = 0;
  }
#else
  {
#if defined(HB_USE_LARGEFILE64)
    HB_FOFFSET nPos = lseek64(hFileHandle, nOffset, SEEK_SET);
#else
    HB_FOFFSET nPos = lseek(hFileHandle, nOffset, SEEK_SET);
#endif
    if (nPos == static_cast<HB_FOFFSET>(-1)) {
      hb_fsSetIOError(false, 0);
      nRead = 0;
    } else {
      long lRead;
      HB_FAILURE_RETRY(lRead, read(hFileHandle, pBuff, nCount));
      nRead = lRead == -1 ? 0 : lRead;
    }
  }
#endif

  hb_vmLock();

  return nRead;
}

HB_SIZE hb_fsWriteAt(HB_FHANDLE hFileHandle, const void *pBuff, HB_SIZE nCount, HB_FOFFSET nOffset)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), pBuff, nCount, nOffset));
#endif

  HB_SIZE nWritten;

  hb_vmUnlock();

#if defined(HB_OS_UNIX) && !defined(HB_OS_VXWORKS)
  {
    long lWritten;
#if defined(HB_USE_LARGEFILE64)
    HB_FAILURE_RETRY(lWritten, pwrite64(hFileHandle, pBuff, nCount, nOffset));
#else
    HB_FAILURE_RETRY(lWritten, pwrite(hFileHandle, pBuff, nCount, nOffset));
#endif
    nWritten = lWritten == -1 ? 0 : lWritten;
  }
#elif defined(HB_OS_WIN)
#if defined(HB_WIN_IOWRITE_LIMIT)
  {
    HANDLE hWFileHandle = DosToWinHandle(hFileHandle);
    OVERLAPPED Overlapped;
    BOOL bResult = TRUE;

    memset(&Overlapped, 0, sizeof(Overlapped));
    Overlapped.Offset = static_cast<DWORD>(nOffset & 0xFFFFFFFF);
    Overlapped.OffsetHigh = static_cast<DWORD>(nOffset >> 32);

    nWritten = 0;
    while (nCount) {
      DWORD dwToWrite;
      DWORD dwWritten;

      if (nCount > static_cast<HB_SIZE>(HB_WIN_IOWRITE_LIMIT)) {
        dwToWrite = HB_WIN_IOWRITE_LIMIT;
        nCount -= static_cast<HB_SIZE>(dwToWrite);
      } else {
        dwToWrite = static_cast<DWORD>(nCount);
        nCount = 0;
      }

      bResult = WriteFile(hWFileHandle, static_cast<HB_UCHAR *>(const_cast<void *>(pBuff)) + nWritten, dwToWrite,
                          &dwWritten, &Overlapped);

      if (!bResult) {
        break;
      }

      nWritten += static_cast<HB_SIZE>(dwWritten);

      if (dwWritten != dwToWrite) {
        break;
      }
    }
    hb_fsSetIOError(bResult != 0, 0);
  }
#else
  if (hb_iswinnt()) {
    DWORD dwWritten = 0;
    OVERLAPPED Overlapped;
    memset(&Overlapped, 0, sizeof(Overlapped));
    Overlapped.Offset = static_cast<DWORD>(nOffset & 0xFFFFFFFF);
    Overlapped.OffsetHigh = static_cast<DWORD>(nOffset >> 32);
    hb_fsSetIOError(
        WriteFile(DosToWinHandle(hFileHandle), pBuff, static_cast<DWORD>(nCount), &dwWritten, &Overlapped) != 0, 0);
    nWritten = dwWritten;
  } else {
    auto ulOffsetLow = static_cast<ULONG>(nOffset & 0xFFFFFFFF);
    auto ulOffsetHigh = static_cast<ULONG>(nOffset >> 32);
    ulOffsetLow =
        SetFilePointer(DosToWinHandle(hFileHandle), ulOffsetLow, reinterpret_cast<PLONG>(&ulOffsetHigh), SEEK_SET);
    if (ulOffsetLow == static_cast<ULONG>(INVALID_SET_FILE_POINTER) && GetLastError() != NO_ERROR) {
      hb_fsSetIOError(false, 0);
      nWritten = 0;
    } else {
      DWORD dwWritten = 0;
      hb_fsSetIOError(
          WriteFile(DosToWinHandle(hFileHandle), pBuff, static_cast<DWORD>(nCount), &dwWritten, nullptr) != 0, 0);
      nWritten = dwWritten;
    }
  }
#endif // HB_WIN_IOWRITE_LIMIT

// FIXME: below are not atom operations. It has to be fixed for RDD
//        file access with shared file handles in aliased work areas. TOCHECK: OS/2 ?
#elif defined(HB_FS_IO_16BIT)
  if (hb_fsSeekLarge(hFileHandle, nOffset, FS_SET) == nOffset) {
    nWritten = hb_fsWriteLarge(hFileHandle, pBuff, nCount);
  } else {
    nWritten = 0;
  }
#else
  {
#if defined(HB_USE_LARGEFILE64)
    HB_FOFFSET nPos = lseek64(hFileHandle, nOffset, SEEK_SET);
#else
    HB_FOFFSET nPos = lseek(hFileHandle, nOffset, SEEK_SET);
#endif
    if (nPos == static_cast<HB_FOFFSET>(-1)) {
      hb_fsSetIOError(false, 0);
      nWritten = 0;
    } else {
      long lWritten;
      HB_FAILURE_RETRY(lWritten, write(hFileHandle, pBuff, nCount));
      nWritten = lWritten == -1 ? 0 : lWritten;
    }
  }
#endif

  hb_vmLock();

  return nWritten;
}

HB_BOOL hb_fsTruncAt(HB_FHANDLE hFileHandle, HB_FOFFSET nOffset)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsTruncAt(%p, %" PFHL "i)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nOffset));
#endif

  auto fResult = false;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    auto ulOffsetLow = static_cast<ULONG>(nOffset & 0xFFFFFFFF);
    auto ulOffsetHigh = static_cast<ULONG>(nOffset >> 32);

    // This is not atom operation anyhow if someone want to truncate
    // file then he has to made necessary synchronizations in upper level
    // code. We have such situation in our RDD drivers and for us such
    // version is enough. [druzus]

    ulOffsetLow = SetFilePointer(DosToWinHandle(hFileHandle), ulOffsetLow, reinterpret_cast<PLONG>(&ulOffsetHigh),
                                 static_cast<DWORD>(SEEK_SET));
    if (((static_cast<HB_FOFFSET>(ulOffsetHigh) << 32) | ulOffsetLow) == nOffset) {
      fResult = SetEndOfFile(DosToWinHandle(hFileHandle)) != 0;
    } else {
      fResult = false;
    }

    hb_fsSetIOError(fResult, 0);
  }
#else
  {
    int iResult;
#if defined(HB_USE_LARGEFILE64)
    HB_FAILURE_RETRY(iResult, ftruncate64(hFileHandle, nOffset));
#else
    HB_FAILURE_RETRY(iResult, ftruncate(hFileHandle, nOffset));
#endif
    fResult = iResult != -1;
  }
#endif

  hb_vmLock();

  return fResult;
}

void hb_fsCommit(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

  hb_vmUnlock();

#if defined(HB_OS_WIN)

  hb_fsSetIOError(FlushFileBuffers(DosToWinHandle(hFileHandle)) != 0, 0);

#elif defined(HB_OS_UNIX)
  {
    int iResult;
    // We should check here only for _POSIX_SYNCHRONIZED_IO defined
    // and it should be enough to test if fdatasync() declaration
    // exists in <unistd.h>. Unfortunately on some OS-es like Darwin
    // _POSIX_SYNCHRONIZED_IO is defined but fdatasync() does not exists.
    // As workaround we are using this trick to check non zero version
    // number but on some systems it may disable using fdatasync() [druzus]
#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO - 0 > 0
    // faster - flushes data buffers only, without updating directory info
    HB_FAILURE_RETRY(iResult, fdatasync(hFileHandle));
#else
    // slower - flushes all file data buffers and i-node info
    HB_FAILURE_RETRY(iResult, fsync(hFileHandle));
#endif
  }
#else

  // NOTE: close() functions releases all locks regardless if it is an
  // original or duplicated file handle

  // This hack is very dangerous. POSIX standard define that if _ANY_
  // file handle is closed all locks set by the process on the file
  // pointed by this descriptor are removed. It doesn't matter they
  // were done using different descriptor. It means that we now clean
  // all locks on hFileHandle with the code below if the OS is POSIX
  // compliant. I vote to disable it. [druzus]

  {
    int dup_handle;
    auto fResult = false;

    dup_handle = dup(hFileHandle);
    if (dup_handle != -1) {
      close(dup_handle);
      fResult = true;
    }
    hb_fsSetIOError(fResult, 0);
  }

#endif

  hb_vmLock();
}

HB_BOOL hb_fsLock(HB_FHANDLE hFileHandle, HB_ULONG ulStart, HB_ULONG ulLength, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), ulStart, ulLength, uiMode));
#endif

  auto fResult = false;

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  switch (uiMode & FL_MASK) {
  case FL_LOCK:

    if (hb_iswinnt()) {
      OVERLAPPED sOlap;
      DWORD dwFlags;
      memset(&sOlap, 0, sizeof(sOlap));
      sOlap.Offset = static_cast<DWORD>(ulStart);
      dwFlags = (uiMode & FLX_SHARED) ? 0 : LOCKFILE_EXCLUSIVE_LOCK;
      if (!s_fUseWaitLocks || !(uiMode & FLX_WAIT)) {
        dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
      }
      fResult = LockFileEx(DosToWinHandle(hFileHandle), dwFlags, 0, ulLength, 0, &sOlap) != 0;
    } else {
      fResult = LockFile(DosToWinHandle(hFileHandle), ulStart, 0, ulLength, 0) != 0;
    }
    break;

  case FL_UNLOCK:

    if (hb_iswinnt()) {
      OVERLAPPED sOlap;
      memset(&sOlap, 0, sizeof(sOlap));
      sOlap.Offset = static_cast<DWORD>(ulStart);
      fResult = UnlockFileEx(DosToWinHandle(hFileHandle), 0, ulLength, 0, &sOlap) != 0;
    } else {
      fResult = UnlockFile(DosToWinHandle(hFileHandle), ulStart, 0, ulLength, 0) != 0;
    }
    break;

  default:
    fResult = false;
  }
  hb_fsSetIOError(fResult, 0);
#elif defined(_MSC_VER)
  {
    HB_ULONG ulOldPos;

    ulOldPos = lseek(hFileHandle, 0L, SEEK_CUR);
    lseek(hFileHandle, ulStart, SEEK_SET);
    switch (uiMode & FL_MASK) {
    case FL_LOCK:
      fResult = (locking(hFileHandle, _LK_NBLCK, ulLength) == 0);
      break;

    case FL_UNLOCK:
      fResult = (locking(hFileHandle, _LK_UNLCK, ulLength) == 0);
      break;

    default:
      fResult = false;
    }
    hb_fsSetIOError(fResult, 0);
    lseek(hFileHandle, ulOldPos, SEEK_SET);
  }
#elif defined(__MINGW32__)
  {
    HB_ULONG ulOldPos;

    ulOldPos = lseek(hFileHandle, 0L, SEEK_CUR);
    lseek(hFileHandle, ulStart, SEEK_SET);
    switch (uiMode & FL_MASK) {
    case FL_LOCK:
      fResult = (_locking(hFileHandle, _LK_LOCK, ulLength) == 0);
      break;

    case FL_UNLOCK:
      fResult = (_locking(hFileHandle, _LK_UNLCK, ulLength) == 0);
      break;

    default:
      fResult = false;
    }
    hb_fsSetIOError(fResult, 0);
    lseek(hFileHandle, ulOldPos, SEEK_SET);
  }
#elif defined(HB_OS_UNIX)
  {
    struct flock lock_info;
    int iResult;

    switch (uiMode & FL_MASK) {
    case FL_LOCK:

      lock_info.l_type = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
      lock_info.l_start = ulStart;
      lock_info.l_len = ulLength;
      lock_info.l_whence = SEEK_SET; // start from the beginning of the file
      lock_info.l_pid = 0;

      HB_FAILURE_RETRY(iResult, fcntl(hFileHandle, (uiMode & FLX_WAIT) ? F_SETLKW : F_SETLK, &lock_info));
      fResult = iResult != -1;
      break;

    case FL_UNLOCK:

      lock_info.l_type = F_UNLCK; // unlock
      lock_info.l_start = ulStart;
      lock_info.l_len = ulLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid = 0;

      HB_FAILURE_RETRY(iResult, fcntl(hFileHandle, F_SETLK, &lock_info));
      fResult = iResult != -1;
      break;

    default:
      fResult = false;
    }
    hb_fsSetIOError(fResult, 0);
  }
#else

  switch (uiMode & FL_MASK) {
  case FL_LOCK:
    fResult = (lock(hFileHandle, ulStart, ulLength) == 0);
    break;

  case FL_UNLOCK:
    fResult = (unlock(hFileHandle, ulStart, ulLength) == 0);
    break;

  default:
    fResult = false;
  }
  hb_fsSetIOError(fResult, 0);

#endif

  hb_vmLock();

  return fResult;
}

HB_BOOL hb_fsLockLarge(HB_FHANDLE hFileHandle, HB_FOFFSET nStart, HB_FOFFSET nLength, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsLockLarge(%p, %" PFHL "u, %" PFHL "i, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nStart, nLength, uiMode));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    auto dwOffsetLo = static_cast<DWORD>(nStart & 0xFFFFFFFF);
    auto dwOffsetHi = static_cast<DWORD>(nStart >> 32);
    auto dwLengthLo = static_cast<DWORD>(nLength & 0xFFFFFFFF);
    auto dwLengthHi = static_cast<DWORD>(nLength >> 32);

    hb_vmUnlock();
    switch (uiMode & FL_MASK) {
    case FL_LOCK:
      if (hb_iswinnt()) {
        OVERLAPPED sOlap;
        DWORD dwFlags;

        dwFlags = ((uiMode & FLX_SHARED) ? 0 : LOCKFILE_EXCLUSIVE_LOCK);
        if (!s_fUseWaitLocks || !(uiMode & FLX_WAIT)) {
          dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
        }

        memset(&sOlap, 0, sizeof(sOlap));
        sOlap.Offset = dwOffsetLo;
        sOlap.OffsetHigh = dwOffsetHi;

        fResult = LockFileEx(DosToWinHandle(hFileHandle), dwFlags, 0, dwLengthLo, dwLengthHi, &sOlap) != 0;
      } else {
        fResult = LockFile(DosToWinHandle(hFileHandle), dwOffsetLo, dwOffsetHi, dwLengthLo, dwLengthHi) != 0;
      }
      break;

    case FL_UNLOCK:
      if (hb_iswinnt()) {
        OVERLAPPED sOlap;

        memset(&sOlap, 0, sizeof(sOlap));
        sOlap.Offset = dwOffsetLo;
        sOlap.OffsetHigh = dwOffsetHi;

        fResult = UnlockFileEx(DosToWinHandle(hFileHandle), 0, dwLengthLo, dwLengthHi, &sOlap) != 0;
      } else {
        fResult = UnlockFile(DosToWinHandle(hFileHandle), dwOffsetLo, dwOffsetHi, dwLengthLo, dwLengthHi) != 0;
      }
      break;

    default:
      fResult = false;
    }
    hb_fsSetIOError(fResult, 0);
    hb_vmLock();
  }
#elif defined(HB_USE_LARGEFILE64)
  {
    struct flock64 lock_info;
    int iResult;

    hb_vmUnlock();
    switch (uiMode & FL_MASK) {
    case FL_LOCK:

      lock_info.l_type = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
      lock_info.l_start = nStart;
      lock_info.l_len = nLength;
      lock_info.l_whence = SEEK_SET; // start from the beginning of the file
      lock_info.l_pid = 0;

      HB_FAILURE_RETRY(iResult, fcntl(hFileHandle, (uiMode & FLX_WAIT) ? F_SETLKW64 : F_SETLK64, &lock_info));
      fResult = iResult != -1;
      break;

    case FL_UNLOCK:

      lock_info.l_type = F_UNLCK; // unlock
      lock_info.l_start = nStart;
      lock_info.l_len = nLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid = 0;

      HB_FAILURE_RETRY(iResult, fcntl(hFileHandle, F_SETLK64, &lock_info));
      fResult = iResult != -1;
      break;

    default:
      fResult = false;
    }
    hb_fsSetIOError(fResult, 0);
    hb_vmLock();
  }
#else
  fResult = hb_fsLock(hFileHandle, static_cast<HB_SIZE>(nStart), static_cast<HB_SIZE>(nLength), uiMode);
#endif

  return fResult;
}

int hb_fsLockTest(HB_FHANDLE hFileHandle, HB_FOFFSET nStart, HB_FOFFSET nLength, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsLockTest(%p, %" PFHL "u, %" PFHL "i, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nStart, nLength, uiMode));
#endif

  int iResult;

#if defined(HB_OS_UNIX)
  {
#if defined(HB_USE_LARGEFILE64)
    struct flock64 lock_info;

    lock_info.l_type = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
    lock_info.l_start = nStart;
    lock_info.l_len = nLength;
    lock_info.l_whence = SEEK_SET;
    lock_info.l_pid = 0;
    iResult = fcntl(hFileHandle, F_GETLK64, &lock_info) != -1 ? static_cast<int>(lock_info.l_pid) : -1;
#else
    struct flock lock_info;

    lock_info.l_type = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
    lock_info.l_start = nStart;
    lock_info.l_len = nLength;
    lock_info.l_whence = SEEK_SET;
    lock_info.l_pid = 0;
    iResult = fcntl(hFileHandle, F_GETLK, &lock_info) != -1 ? static_cast<int>(lock_info.l_pid) : -1;
#endif
  }
#else
  if (hb_fsLockLarge(hFileHandle, nStart, nLength, (uiMode & FLX_SHARED) | FL_LOCK)) {
    if (!hb_fsLockLarge(hFileHandle, nStart, nLength, FL_UNLOCK)) {
      iResult = -1;
    } else {
      iResult = 0;
    }
  } else {
    iResult = 1;
  }
#endif

  return iResult;
}

HB_ULONG hb_fsSeek(HB_FHANDLE hFileHandle, HB_LONG lOffset, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeek(%p, %ld, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), lOffset, uiFlags));
#endif

  HB_ULONG ulPos;
  HB_USHORT nFlags;

  nFlags = convert_seek_flags(uiFlags);

  hb_vmUnlock();
#if defined(HB_OS_WIN)
  // This DOS hack creates 2 GiB file size limit, Druzus
  if (lOffset < 0 && nFlags == SEEK_SET) {
    ulPos = static_cast<ULONG>(INVALID_SET_FILE_POINTER);
    hb_fsSetError(25); // 'Seek Error'
  } else {
    ulPos =
        static_cast<ULONG>(SetFilePointer(DosToWinHandle(hFileHandle), lOffset, nullptr, static_cast<DWORD>(nFlags)));
    hb_fsSetIOError(ulPos != static_cast<ULONG>(INVALID_SET_FILE_POINTER), 0);
  }

  if (ulPos == static_cast<ULONG>(INVALID_SET_FILE_POINTER)) {
    ulPos = static_cast<ULONG>(SetFilePointer(DosToWinHandle(hFileHandle), 0, nullptr, SEEK_CUR));
    if (ulPos == static_cast<ULONG>(INVALID_SET_FILE_POINTER)) {
      ulPos = 0;
    }
  }
#else
  // This DOS hack creates 2 GiB file size limit, Druzus
  if (lOffset < 0 && nFlags == SEEK_SET) {
    ulPos = static_cast<HB_ULONG>(-1);
    hb_fsSetError(25); // 'Seek Error'
  } else {
    ulPos = lseek(hFileHandle, lOffset, nFlags);
    hb_fsSetIOError(ulPos != static_cast<HB_ULONG>(-1), 0);
#if defined(HB_OS_UNIX)
    // small trick to resolve problem with position reported for directories
    if (ulPos == LONG_MAX && lOffset == 0 && nFlags == SEEK_END) {
      // we do not need to use fstat64() here on 32-bit platforms, [druzus]
      struct stat st;

      if (fstat(hFileHandle, &st) == 0) {
        ulPos = st.st_size;
      }
    }
#endif
  }

  if (ulPos == static_cast<HB_ULONG>(-1)) {
    ulPos = lseek(hFileHandle, 0L, SEEK_CUR);
    if (ulPos == static_cast<HB_ULONG>(-1)) {
      ulPos = 0;
    }
  }
#endif
  hb_vmLock();

  return ulPos;
}

HB_FOFFSET hb_fsSeekLarge(HB_FHANDLE hFileHandle, HB_FOFFSET nOffset, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeekLarge(%p, %" PFHL "i, %hu)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle)), nOffset, uiFlags));
#endif

  HB_FOFFSET nPos;

#if defined(HB_OS_WIN)
  {
    HB_USHORT nFlags = convert_seek_flags(uiFlags);

    auto ulOffsetLow = static_cast<ULONG>(nOffset & 0xFFFFFFFF);
    auto ulOffsetHigh = static_cast<ULONG>(nOffset >> 32);

    hb_vmUnlock();
    if (nOffset < 0 && nFlags == SEEK_SET) {
      nPos = static_cast<HB_FOFFSET>(-1);
      hb_fsSetError(25); // 'Seek Error'
    } else {
      ulOffsetLow = SetFilePointer(DosToWinHandle(hFileHandle), ulOffsetLow, reinterpret_cast<PLONG>(&ulOffsetHigh),
                                   static_cast<DWORD>(nFlags));
      if (ulOffsetLow == static_cast<ULONG>(INVALID_SET_FILE_POINTER) && GetLastError() != NO_ERROR) {
        nPos = static_cast<HB_FOFFSET>(-1);
      } else {
        nPos = (static_cast<HB_FOFFSET>(ulOffsetHigh) << 32) | ulOffsetLow;
      }
      hb_fsSetIOError(nPos != static_cast<HB_FOFFSET>(-1), 0);
    }

    if (nPos == static_cast<HB_FOFFSET>(-1)) {
      ulOffsetHigh = 0;
      ulOffsetLow = SetFilePointer(DosToWinHandle(hFileHandle), 0, reinterpret_cast<PLONG>(&ulOffsetHigh), SEEK_CUR);
      if (ulOffsetLow == static_cast<ULONG>(INVALID_SET_FILE_POINTER) && GetLastError() != NO_ERROR) {
        nPos = 0;
      } else {
        nPos = (static_cast<HB_FOFFSET>(ulOffsetHigh) << 32) | ulOffsetLow;
      }
    }
    hb_vmLock();
  }
#elif defined(HB_USE_LARGEFILE64)
  {
    HB_USHORT nFlags = convert_seek_flags(uiFlags);

    hb_vmUnlock();
    if (nOffset < 0 && nFlags == SEEK_SET) {
      nPos = static_cast<HB_FOFFSET>(-1);
      hb_fsSetError(25); // 'Seek Error'
    } else {
      nPos = lseek64(hFileHandle, nOffset, nFlags);
      hb_fsSetIOError(nPos != static_cast<HB_FOFFSET>(-1), 0);
#if defined(HB_OS_UNIX)
      // small trick to resolve problem with position reported for directories
      if (nPos == LONG_MAX && nOffset == 0 && nFlags == SEEK_END) {
        struct stat64 st;

        if (fstat64(hFileHandle, &st) == 0) {
          nPos = st.st_size;
        }
      }
#endif
    }

    if (nPos == static_cast<HB_FOFFSET>(-1)) {
      nPos = lseek64(hFileHandle, 0L, SEEK_CUR);
      if (nPos == static_cast<HB_FOFFSET>(-1)) {
        nPos = 0;
      }
    }
    hb_vmLock();
  }
#else
  nPos = static_cast<HB_FOFFSET>(hb_fsSeek(hFileHandle, static_cast<HB_ISIZ>(nOffset), uiFlags));
#endif

  return nPos;
}

HB_FOFFSET hb_fsTell(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsTell(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

  return hb_fsSeekLarge(hFileHandle, 0, FS_RELATIVE);
}

HB_FOFFSET hb_fsGetSize(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetSize(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

#if defined(HB_OS_WIN)
  {
    DWORD dwFileSizeLow, dwFileSizeHigh = 0;
    auto fOK = false;

    dwFileSizeLow = GetFileSize(DosToWinHandle(hFileHandle), &dwFileSizeHigh);
    fOK = dwFileSizeLow != INVALID_FILE_SIZE || GetLastError() == NO_ERROR;
    hb_fsSetIOError(fOK, 0);

    return fOK ? (static_cast<HB_FOFFSET>(dwFileSizeHigh) << 32) | dwFileSizeLow : 0;
  }
#else
  return hb_fsSeekLarge(hFileHandle, 0, FS_END);
#endif
}

HB_BOOL hb_fsDelete(const char *pszFileName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", pszFileName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpFileName;
    LPTSTR lpFree;

    lpFileName = HB_FSNAMECONV(pszFileName, &lpFree);

    hb_vmUnlock();

    fResult = DeleteFile(lpFileName) != 0;
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;

    pszFileName = hb_fsNameConv(pszFileName, &pszFree);

    hb_vmUnlock();

    fResult = (remove(pszFileName) == 0);
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return fResult;
}

HB_BOOL hb_fsRename(const char *pOldName, const char *pNewName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", pOldName, pNewName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpOldName, lpNewName;
    LPTSTR lpOldFree, lpNewFree;

    lpOldName = HB_FSNAMECONV(pOldName, &lpOldFree);
    lpNewName = HB_FSNAMECONV(pNewName, &lpNewFree);

    hb_vmUnlock();

    fResult = MoveFile(lpOldName, lpNewName) != 0;
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (lpOldFree) {
      hb_xfree(lpOldFree);
    }
    if (lpNewFree) {
      hb_xfree(lpNewFree);
    }
  }
#else
  {
    char *pszFreeOld, *pszFreeNew;

    pOldName = hb_fsNameConv(pOldName, &pszFreeOld);
    pNewName = hb_fsNameConv(pNewName, &pszFreeNew);

    hb_vmUnlock();

    fResult = (rename(pOldName, pNewName) == 0);
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (pszFreeOld) {
      hb_xfree(pszFreeOld);
    }
    if (pszFreeNew) {
      hb_xfree(pszFreeNew);
    }
  }
#endif

  return fResult;
}

HB_BOOL hb_fsMkDir(const char *pszDirName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", pszDirName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpDirName;
    LPTSTR lpFree;

    lpDirName = HB_FSNAMECONV(pszDirName, &lpFree);

    hb_vmUnlock();

    fResult = CreateDirectory(lpDirName, nullptr) != 0;
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;

    pszDirName = hb_fsNameConv(pszDirName, &pszFree);

    hb_vmUnlock();

#if !defined(HB_OS_UNIX) && (defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__MINGW32__))
    fResult = (mkdir(pszDirName) == 0);
#else
    fResult = (mkdir(pszDirName, S_IRWXU | S_IRWXG | S_IRWXO) == 0);
#endif
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return fResult;
}

HB_BOOL hb_fsChDir(const char *pszDirName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", pszDirName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpDirName;
    LPTSTR lpFree;
    UINT uiErrMode;

    lpDirName = HB_FSNAMECONV(pszDirName, &lpFree);

    hb_vmUnlock();

    uiErrMode = SetErrorMode(SEM_FAILCRITICALERRORS);
    fResult = SetCurrentDirectory(lpDirName) != FALSE;
    SetErrorMode(uiErrMode);
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;

    pszDirName = hb_fsNameConv(pszDirName, &pszFree);

    hb_vmUnlock();

    fResult = (chdir(pszDirName) == 0);
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return fResult;
}

HB_BOOL hb_fsRmDir(const char *pszDirName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", pszDirName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPCTSTR lpDirName;
    LPTSTR lpFree;

    lpDirName = HB_FSNAMECONV(pszDirName, &lpFree);

    hb_vmUnlock();

    fResult = RemoveDirectory(lpDirName) != 0;
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;

    pszDirName = hb_fsNameConv(pszDirName, &pszFree);

    hb_vmUnlock();

    fResult = (rmdir(pszDirName) == 0);
    hb_fsSetIOError(fResult, 0);

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return fResult;
}

// NOTE: This is not thread safe function, it's there for compatibility.
// NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc.

const char *hb_fsCurDir(int iDrive)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDir(%d)", iDrive));
#endif

  char *pszDirBuffer;

  pszDirBuffer = hb_stackDirBuffer();
  hb_fsCurDirBuff(iDrive, pszDirBuffer, HB_PATH_MAX);

  return pszDirBuffer;
}

// NOTE: Thread safe version of hb_fsCurDir()
// NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc.

HB_ERRCODE hb_fsCurDirBuff(int iDrive, char *pszBuffer, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%d)", iDrive));
#endif

  int iCurDrv = iDrive;
  HB_ERRCODE nResult;

  pszBuffer[0] = '\0';

  // do not cover this code by HB_OS_HAS_DRIVE_LETTER macro
  // It will allow us to add drive emulation in hb_fsCurDrv()/hb_fsChDrv()
  // and hb_fsNameConv()

#if defined(HB_OS_WIN) || !(defined(__MINGW32__))
  if (iDrive > 0) {
    iCurDrv = hb_fsCurDrv() + 1;
    if (iDrive != iCurDrv) {
      hb_fsChDrv(iDrive - 1);
    }
  }
#endif

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    auto dwSize = static_cast<DWORD>(nSize);
    auto lpBuffer = static_cast<LPTSTR>(hb_xgrab(dwSize * sizeof(TCHAR)));
    lpBuffer[0] = TEXT('\0');
    hb_fsSetIOError((GetCurrentDirectory(dwSize, lpBuffer) != 0), 0);
    lpBuffer[dwSize - 1] = TEXT('\0');
    HB_OSSTRDUP2(lpBuffer, pszBuffer, nSize - 1);
    hb_xfree(lpBuffer);
  }
#elif defined(__MINGW32__)

  if (iDrive >= 0) {
    hb_fsSetIOError((_getdcwd(iDrive, pszBuffer, nSize) != nullptr), 0);
  } else {
    hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
  }

#else

  hb_fsSetIOError((getcwd(pszBuffer, nSize) != nullptr), 0);

#endif

  hb_vmLock();

  nResult = hb_fsError();

  if (iDrive != iCurDrv) {
    hb_fsChDrv(iCurDrv - 1);
    hb_fsSetError(nResult);
  }

  pszBuffer[nSize - 1] = '\0';

  if (nResult == 0 && pszBuffer[0]) {
    char *pszStart;
    HB_SIZE nLen;

    // Strip the leading drive spec, and leading backslash if there's one.
    // NOTE: A trailing underscore is not returned on this platform,
    //       so we don't need to strip it. [vszakats]

    nLen = strlen(pszBuffer);
    pszStart = pszBuffer;

#if defined(HB_OS_HAS_DRIVE_LETTER)
    if (pszStart[1] == HB_OS_DRIVE_DELIM_CHR) {
      pszStart += 2;
      nLen -= 2;
    }
#endif
    if (strchr(HB_OS_PATH_DELIM_CHR_LIST, static_cast<HB_UCHAR>(pszStart[0]))) {
      pszStart++;
      nLen--;
    }

    // Strip the trailing (back)slash if there's one
    if (nLen && strchr(HB_OS_PATH_DELIM_CHR_LIST, static_cast<HB_UCHAR>(pszStart[nLen - 1]))) {
      nLen--;
    }

    if (nLen && pszBuffer != pszStart) {
      memmove(pszBuffer, pszStart, nLen);
    }

    pszBuffer[nLen] = '\0';

#if !defined(HB_OS_WIN)
    // Convert from OS codepage
    {
      char *pszFree = nullptr;
      const char *pszResult;

      nLen = nSize;
      pszResult = hb_osDecodeCP(pszBuffer, &pszFree, &nLen);

      if (pszResult != pszBuffer) {
        hb_strncpy(pszBuffer, pszResult, nSize - 1);
      }
      if (pszFree) {
        hb_xfree(pszFree);
      }
    }
#endif
  }

  return nResult;
}

HB_BOOL hb_fsGetCWD(char *pszBuffer, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetCWD(%p,%" HB_PFS "u)", static_cast<void*>(pszBuffer), nSize));
#endif

  auto fResult = false;

  pszBuffer[0] = '\0';

  hb_vmUnlock();

#if defined(HB_OS_WIN)
  {
    auto dwSize = static_cast<DWORD>(nSize);
    auto lpBuffer = static_cast<LPTSTR>(hb_xgrab(dwSize * sizeof(TCHAR)));
    lpBuffer[0] = TEXT('\0');
    fResult = GetCurrentDirectory(dwSize, lpBuffer) != 0;
    hb_fsSetIOError(fResult, 0);
    lpBuffer[dwSize - 1] = TEXT('\0');
    HB_OSSTRDUP2(lpBuffer, pszBuffer, nSize - 1);
    hb_xfree(lpBuffer);
  }
#else

  fResult = getcwd(pszBuffer, nSize) != nullptr;
  hb_fsSetIOError(fResult, 0);

#endif

  hb_vmLock();

  pszBuffer[nSize - 1] = '\0';

  if (fResult && pszBuffer[0]) {
    HB_SIZE nLen;
    nLen = strlen(pszBuffer);

    // add the trailing (back)slash if there's no one
    if (nLen + 1 < nSize && strchr(HB_OS_PATH_DELIM_CHR_LIST, static_cast<HB_UCHAR>(pszBuffer[nLen - 1])) == 0) {
      pszBuffer[nLen++] = HB_OS_PATH_DELIM_CHR;
      pszBuffer[nLen] = '\0';
    }

#if !defined(HB_OS_WIN)
    // Convert from OS codepage
    {
      char *pszFree = nullptr;
      const char *pszResult;

      nLen = nSize;
      pszResult = hb_osDecodeCP(pszBuffer, &pszFree, &nLen);

      if (pszResult != pszBuffer) {
        hb_strncpy(pszBuffer, pszResult, nSize - 1);
      }
      if (pszFree) {
        hb_xfree(pszFree);
      }
    }
#endif
  }

  return fResult;
}

HB_BOOL hb_fsSetCWD(const char *pszDirName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetCWD(%s)", pszDirName));
#endif

  auto fResult = false;

#if defined(HB_OS_WIN)
  {
    LPTSTR lpFree;
    LPCTSTR lpDirName = HB_FSNAMECONV(pszDirName, &lpFree);

    hb_vmUnlock();

    UINT uiErrMode = SetErrorMode(SEM_FAILCRITICALERRORS);
    fResult = SetCurrentDirectory(lpDirName) != FALSE;
    hb_fsSetIOError(fResult, 0);
    SetErrorMode(uiErrMode);

    hb_vmLock();

    if (lpFree) {
      hb_xfree(lpFree);
    }
  }
#else
  {
    char *pszFree;
    pszDirName = hb_fsNameConv(pszDirName, &pszFree);

    hb_vmUnlock();

    fResult = (chdir(pszDirName) == 0);
    hb_fsSetIOError(fResult, 0);

#if defined(HB_OS_HAS_DRIVE_LETTER)
    if (fResult && pszDirName[0] != 0 && pszDirName[1] == HB_OS_DRIVE_DELIM_CHR) {
      int iDrive = pszDirName[0];

      if (iDrive >= 'A' && iDrive <= 'Z') {
        iDrive -= 'A';
      } else if (iDrive >= 'a' && iDrive <= 'z') {
        iDrive -= 'a';
      } else {
        iDrive = 0;
      }

      if (iDrive) {
        HB_FS_SETDRIVE(iDrive);
      }
    }
#endif

    hb_vmLock();

    if (pszFree) {
      hb_xfree(pszFree);
    }
  }
#endif

  return fResult;
}

// NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ...

HB_ERRCODE hb_fsChDrv(int iDrive)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", iDrive));
#endif

  HB_ERRCODE nResult;

#if defined(HB_OS_HAS_DRIVE_LETTER)
  {
    int iSave, iNewDrive;

    hb_vmUnlock();

    HB_FS_GETDRIVE(iSave);
    HB_FS_SETDRIVE(iDrive);
    HB_FS_GETDRIVE(iNewDrive);

    if (iDrive == iNewDrive) {
      nResult = 0;
      hb_fsSetError(0);
    } else {
      HB_FS_SETDRIVE(iSave);

      nResult = static_cast<HB_ERRCODE>(FS_ERROR);
      hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));
    }
    hb_vmLock();
  }
#else

  HB_SYMBOL_UNUSED(iDrive);
  nResult = static_cast<HB_ERRCODE>(FS_ERROR);
  hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));

#endif

  return nResult;
}

// NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ...

int hb_fsCurDrv(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));
#endif

  int iDrive;

#if defined(HB_OS_HAS_DRIVE_LETTER)

  hb_vmUnlock();
  HB_FS_GETDRIVE(iDrive);
  hb_fsSetError(0);
  hb_vmLock();

#else

  iDrive = 0;
  hb_fsSetError(static_cast<HB_ERRCODE>(FS_ERROR));

#endif

  return iDrive; // Return the drive number, base 0.
}

// NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ...

HB_ERRCODE hb_fsIsDrv(int iDrive)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", iDrive));
#endif

  HB_ERRCODE nResult;

  if (iDrive >= 0)
#if defined(HB_OS_WIN)
  {
    hb_vmUnlock();
    nResult = ((GetLogicalDrives() >> iDrive) & 1) ? 0 : static_cast<HB_ERRCODE>(F_ERROR);
    hb_vmLock();
    hb_fsSetError(0);
  }
#elif defined(HB_OS_HAS_DRIVE_LETTER)
  {
    int iSave, iNewDrive;

    hb_vmUnlock();

    HB_FS_GETDRIVE(iSave);
    HB_FS_SETDRIVE(iDrive);
    HB_FS_GETDRIVE(iNewDrive);
    nResult = (iDrive == iNewDrive) ? 0 : static_cast<HB_ERRCODE>(FS_ERROR);
    HB_FS_SETDRIVE(iSave);
    hb_fsSetError(0);

    hb_vmLock();
  }
#else
  {
    HB_SYMBOL_UNUSED(iDrive);
    nResult = static_cast<HB_ERRCODE>(FS_ERROR);
    hb_fsSetError(0);
  }
#endif
  else {
    nResult = static_cast<HB_ERRCODE>(FS_ERROR);
    hb_fsSetError(0);
  }

  return nResult;
}

HB_BOOL hb_fsIsDevice(HB_FHANDLE hFileHandle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDevice(%p)", reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFileHandle))));
#endif

  auto fResult = false;

  hb_vmUnlock();

#if defined(HB_OS_WIN)

  fResult = GetFileType(DosToWinHandle(hFileHandle)) == FILE_TYPE_CHAR;
  hb_fsSetIOError(fResult, 0);

#else

#if defined(_MSC_VER) || defined(__MINGW32__)
  fResult = _isatty(hFileHandle) != 0;
#else
  fResult = isatty(hFileHandle) != 0;
#endif
  hb_fsSetIOError(fResult, 0);

#endif

  hb_vmLock();

  return fResult;
}

// convert file name for hb_fsExtOpen()
// caller must free the returned buffer
char *hb_fsExtName(const char *pszFileName, const char *pDefExt, HB_FATTR nExFlags, const char *pPaths)
{
  HB_PATHNAMES *pNextPath;
  auto fIsFile = false;

  auto szPath = static_cast<char *>(hb_xgrab(HB_PATH_MAX));

  PHB_FNAME pFilepath = hb_fsFNameSplit(pszFileName);

  if (pDefExt && ((nExFlags & FXO_FORCEEXT) || !pFilepath->szExtension)) {
    pFilepath->szExtension = pDefExt;
  }

  if (pFilepath->szPath) {
    hb_fsFNameMerge(szPath, pFilepath);
  } else if (nExFlags & FXO_DEFAULTS) {
    const char *szDefault = hb_setGetDefault();
    if (szDefault != nullptr) {
      pFilepath->szPath = szDefault;
      hb_fsFNameMerge(szPath, pFilepath);
      fIsFile = hb_fsFileExists(szPath);
    }
    if (!fIsFile && (nExFlags & (FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE)) == 0 && hb_setGetPath()) {
      pNextPath = hb_setGetFirstSetPath();
      while (!fIsFile && pNextPath) {
        pFilepath->szPath = pNextPath->szPath;
        hb_fsFNameMerge(szPath, pFilepath);
        fIsFile = hb_fsFileExists(szPath);
        pNextPath = pNextPath->pNext;
      }
    }
    if (!fIsFile) {
      pFilepath->szPath = szDefault ? szDefault : nullptr;
      hb_fsFNameMerge(szPath, pFilepath);
    }
  } else if (pPaths && *pPaths) {
    HB_PATHNAMES *pSearchPath = nullptr;
    hb_fsAddSearchPath(pPaths, &pSearchPath);
    pNextPath = pSearchPath;
    while (!fIsFile && pNextPath) {
      pFilepath->szPath = pNextPath->szPath;
      hb_fsFNameMerge(szPath, pFilepath);
      fIsFile = hb_fsFileExists(szPath);
      pNextPath = pNextPath->pNext;
    }
    hb_fsFreeSearchPath(pSearchPath);
    if (!fIsFile) {
      pFilepath->szPath = nullptr;
      hb_fsFNameMerge(szPath, pFilepath);
    }
  } else {
    hb_fsFNameMerge(szPath, pFilepath);
  }

  hb_xfree(pFilepath);

  return szPath;
}

HB_FHANDLE hb_fsExtOpen(const char *pszFileName, const char *pDefExt, HB_FATTR nExFlags, const char *pPaths,
                        PHB_ITEM pError)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fsExtOpen(%s, %s, %u, %p, %p)", pszFileName, pDefExt, nExFlags, static_cast<const void*>(pPaths), static_cast<void*>(pError)));
#endif

  HB_FHANDLE hFile;
  HB_USHORT uiFlags;
  const char *szPath;
  char *szFree = nullptr;

#if 0
#define FXO_TRUNCATE 0x0100         // Create (truncate if exists)
#define FXO_APPEND 0x0200           // Create (append if exists)
#define FXO_UNIQUE 0x0400           // Create unique file FO_EXCL ???
#define FXO_FORCEEXT 0x0800         // Force default extension
#define FXO_DEFAULTS 0x1000         // Use SET command defaults
#define FXO_DEVICERAW 0x2000        // Open devices in raw mode
   // Harbour extension
#define FXO_NOSEEKPOS FXO_DEVICERAW // seek pos not needed in regular file
#define FXO_SHARELOCK 0x4000        // emulate DOS SH_DENY* mode in POSIX OS
#define FXO_COPYNAME 0x8000         // copy final szPath into pszFileName

   hb_errGetFileName(pError);
#endif

  if (pDefExt || pPaths || pError || (nExFlags & (FXO_DEFAULTS | FXO_COPYNAME)) != 0) {
    szPath = szFree = hb_fsExtName(pszFileName, pDefExt, nExFlags, pPaths);
  } else {
    szPath = pszFileName;
  }

  uiFlags = static_cast<HB_USHORT>(nExFlags & 0xff);
  if (nExFlags & (FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE)) {
    uiFlags |= FO_CREAT;
    if (nExFlags & FXO_UNIQUE) {
      uiFlags |= FO_EXCL;
    }
#if defined(HB_USE_SHARELOCKS)
    else if ((nExFlags & (FXO_TRUNCATE | FXO_SHARELOCK)) == FXO_TRUNCATE)
#else
    else if (nExFlags & FXO_TRUNCATE)
#endif
    {
      uiFlags |= FO_TRUNC;
    }
  }

  hFile = hb_fsOpenEx(szPath, uiFlags, FC_NORMAL);

#if defined(HB_USE_SHARELOCKS)
  if (hFile != FS_ERROR && (nExFlags & FXO_SHARELOCK) != 0) {
#if defined(HB_USE_BSDLOCKS)
    int iLock, iResult;
    if ( // (uiFlags & (FO_READ | FO_WRITE | FO_READWRITE)) == FO_READ ||
        (uiFlags & (FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE)) == 0) {
      iLock = LOCK_SH | LOCK_NB;
    } else {
      iLock = LOCK_EX | LOCK_NB;
    }
    hb_vmUnlock();
    HB_FAILURE_RETRY(iResult, flock(hFile, iLock));
    hb_vmLock();
    if (iResult != 0)
#else
    HB_USHORT uiLock;
    if ((uiFlags & (FO_READ | FO_WRITE | FO_READWRITE)) == FO_READ ||
        (uiFlags & (FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE)) == 0) {
      uiLock = FL_LOCK | FLX_SHARED;
    } else {
      uiLock = FL_LOCK | FLX_EXCLUSIVE;
    }

    if (!hb_fsLockLarge(hFile, HB_SHARELOCK_POS, HB_SHARELOCK_SIZE, uiLock))
#endif
    {
      hb_fsClose(hFile);
      hFile = FS_ERROR;
      // fix for NetErr() support and Clipper compatibility,
      // should be revised with a better multi platform solution.
      hb_fsSetError((nExFlags & FXO_TRUNCATE) ? 5 : 32);
    } else if (nExFlags & FXO_TRUNCATE) {
      // truncate the file only if properly locked
      hb_fsSeek(hFile, 0, FS_SET);
      hb_fsTruncAt(hFile, 0);
      if (hb_fsError() != 0) {
        hb_fsClose(hFile);
        hFile = FS_ERROR;
        hb_fsSetError(5);
      }
    }
  }
#elif 1
  // Temporary fix for NetErr() support and Clipper compatibility,
  // should be revised with a better solution.
  if ((nExFlags & (FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE)) == 0 && hb_fsError() == 5) {
    hb_fsSetError(32);
  }
#endif

  if (pError) {
    hb_errPutFileName(pError, szPath);
    if (hFile == FS_ERROR) {
      hb_errPutOsCode(pError, hb_fsError());
      hb_errPutGenCode(pError, static_cast<HB_ERRCODE>((nExFlags & FXO_TRUNCATE) ? EG_CREATE : EG_OPEN));
    }
  }

  if (nExFlags & FXO_COPYNAME && hFile != FS_ERROR) {
    hb_strncpy(const_cast<char *>(pszFileName), szPath, HB_PATH_MAX - 1);
  }

  if (szFree != nullptr) {
    hb_xfree(szFree);
  }

  return hFile;
}

HB_BOOL hb_fsEof(HB_FHANDLE hFileHandle)
{
  auto fResult = false;

  hb_vmUnlock();

  HB_FOFFSET curPos;
  HB_FOFFSET endPos;

  curPos = hb_fsSeekLarge(hFileHandle, 0L, FS_RELATIVE);
  if (curPos != -1) {
    HB_FOFFSET newPos;
    endPos = hb_fsSeekLarge(hFileHandle, 0L, FS_END);
    newPos = hb_fsSeekLarge(hFileHandle, curPos, FS_SET);
    fResult = (endPos != -1 && newPos == curPos);
  } else {
    endPos = -1;
    fResult = false;
  }
  hb_fsSetIOError(fResult, 0);
  fResult = !fResult || curPos >= endPos;

  hb_vmLock();

  return fResult;
}

const char *hb_fsNameConv(const char *pszFileName, char **pszFree)
{
  // Convert file and dir case. The allowed SET options are:
  //    LOWER - Convert all characters of file to lower
  //    UPPER - Convert all characters of file to upper
  //    MIXED - Leave as is
  //
  // The allowed environment options are:
  //    FILECASE - define the case of file
  //    DIRCASE - define the case of path
  //    DIRSEPARATOR - define separator of path (Ex. "/")
  //    TRIMFILENAME - strip trailing and leading spaces (also from extension)

  if (pszFree) {
    *pszFree = nullptr;
  }

  if (!hb_vmIsReady()) {
    return pszFileName;
  }

  bool fTrim = hb_setGetTrimFileName();
  bool fEncodeCP = hb_osUseCP();
  auto cDirSep = static_cast<char>(hb_setGetDirSeparator());
  int iFileCase = hb_setGetFileCase();
  int iDirCase = hb_setGetDirCase();
  if (fTrim) {
    if (strchr(pszFileName, ' ') == nullptr) {
      fTrim = false;
    }
  }
  if (cDirSep != HB_OS_PATH_DELIM_CHR) {
    if (strchr(pszFileName, static_cast<HB_UCHAR>(cDirSep)) == nullptr) {
      cDirSep = HB_OS_PATH_DELIM_CHR;
    }
  }

  if (fTrim || fEncodeCP || cDirSep != HB_OS_PATH_DELIM_CHR || iFileCase != HB_SET_CASE_MIXED ||
      iDirCase != HB_SET_CASE_MIXED) {
    PHB_FNAME pFileName;
    HB_SIZE nLen;
    char *pszPath = nullptr, *pszName = nullptr, *pszExt = nullptr;

    if (pszFree) {
      pszFileName = *pszFree = hb_strncpy(static_cast<char *>(hb_xgrab(HB_PATH_MAX)), pszFileName, HB_PATH_MAX - 1);
    }

    if (cDirSep != HB_OS_PATH_DELIM_CHR) {
      auto p = const_cast<char *>(pszFileName);
      while (*p) {
        if (*p == cDirSep) {
          *p = HB_OS_PATH_DELIM_CHR;
        }
        p++;
      }
    }

    pFileName = hb_fsFNameSplit(pszFileName);

    // strip trailing and leading spaces
    if (fTrim) {
      if (pFileName->szName) {
        nLen = strlen(pFileName->szName);
        nLen = hb_strRTrimLen(pFileName->szName, nLen, false);
        pFileName->szName = hb_strLTrim(pFileName->szName, &nLen);
        (const_cast<char *>(pFileName->szName))[nLen] = '\0';
      }
      if (pFileName->szExtension) {
        nLen = strlen(pFileName->szExtension);
        nLen = hb_strRTrimLen(pFileName->szExtension, nLen, false);
        pFileName->szExtension = hb_strLTrim(pFileName->szExtension, &nLen);
        (const_cast<char *>(pFileName->szExtension))[nLen] = '\0';
      }
    }

    // FILECASE
    if (iFileCase == HB_SET_CASE_LOWER) {
      if (pFileName->szName) {
        pFileName->szName = pszName = hb_cdpnDupLower(hb_vmCDP(), pFileName->szName, nullptr);
      }
      if (pFileName->szExtension) {
        pFileName->szExtension = pszExt = hb_cdpnDupLower(hb_vmCDP(), pFileName->szExtension, nullptr);
      }
    } else if (iFileCase == HB_SET_CASE_UPPER) {
      if (pFileName->szName) {
        pFileName->szName = pszName = hb_cdpnDupUpper(hb_vmCDP(), pFileName->szName, nullptr);
      }
      if (pFileName->szExtension) {
        pFileName->szExtension = pszExt = hb_cdpnDupUpper(hb_vmCDP(), pFileName->szExtension, nullptr);
      }
    }

    // DIRCASE
    if (pFileName->szPath) {
      if (iDirCase == HB_SET_CASE_LOWER) {
        pFileName->szPath = pszPath = hb_cdpnDupLower(hb_vmCDP(), pFileName->szPath, nullptr);
      } else if (iDirCase == HB_SET_CASE_UPPER) {
        pFileName->szPath = pszPath = hb_cdpnDupUpper(hb_vmCDP(), pFileName->szPath, nullptr);
      }
    }

    hb_fsFNameMerge(const_cast<char *>(pszFileName), pFileName);
    hb_xfree(pFileName);
    if (pszPath) {
      hb_xfree(pszPath);
    }
    if (pszName) {
      hb_xfree(pszName);
    }
    if (pszExt) {
      hb_xfree(pszExt);
    }

    if (fEncodeCP) {
      const char *pszPrev = pszFileName;
      nLen = HB_PATH_MAX;
      pszFileName = hb_osEncodeCP(pszFileName, pszFree, &nLen);
      if (pszFree == nullptr && pszFileName != pszPrev) {
        hb_strncpy(const_cast<char *>(pszPrev), pszFileName, HB_PATH_MAX - 1);
        hb_xfree(HB_UNCONST(pszFileName));
        pszFileName = pszPrev;
      }
    }
  }

  return pszFileName;
}

#if defined(HB_OS_WIN)
HB_WCHAR *hb_fsNameConvU16(const char *pszFileName)
{
  // Convert file and dir case. The allowed SET options are:
  //    LOWER - Convert all characters of file to lower
  //    UPPER - Convert all characters of file to upper
  //    MIXED - Leave as is
  //
  // The allowed environment options are:
  //    FILECASE - define the case of file
  //    DIRCASE - define the case of path
  //    DIRSEPARATOR - define separator of path (Ex. "/")
  //    TRIMFILENAME - strip trailing and leading spaces (also from extension)

  if (!hb_vmIsReady()) {
    return hb_mbtowc(pszFileName); // No HVM stack
  }

  auto cdp = hb_vmCDP();
  bool fTrim = hb_setGetTrimFileName();
  auto cDirSep = static_cast<char>(hb_setGetDirSeparator());
  int iFileCase = hb_setGetFileCase();
  int iDirCase = hb_setGetDirCase();
  if (fTrim) {
    if (strchr(pszFileName, ' ') == nullptr) {
      fTrim = false;
    }
  }
  if (cDirSep != HB_OS_PATH_DELIM_CHR) {
    if (strchr(pszFileName, static_cast<HB_UCHAR>(cDirSep)) == nullptr) {
      cDirSep = HB_OS_PATH_DELIM_CHR;
    }
  }

  char *pszBuffer = nullptr;

  if (fTrim || cDirSep != HB_OS_PATH_DELIM_CHR || iFileCase != HB_SET_CASE_MIXED || iDirCase != HB_SET_CASE_MIXED) {
    char *pszPath = nullptr, *pszName = nullptr, *pszExt = nullptr;
    PHB_FNAME pFileName;

    pszFileName = pszBuffer = hb_strncpy(static_cast<char *>(hb_xgrab(HB_PATH_MAX)), pszFileName, HB_PATH_MAX - 1);

    if (cDirSep != HB_OS_PATH_DELIM_CHR) {
      char *p = pszBuffer;
      while (*p) {
        if (*p == cDirSep) {
          *p = HB_OS_PATH_DELIM_CHR;
        }
        p++;
      }
    }

    pFileName = hb_fsFNameSplit(pszBuffer);

    // strip trailing and leading spaces
    if (fTrim) {
      if (pFileName->szName) {
        HB_SIZE nLen = strlen(pFileName->szName);
        nLen = hb_strRTrimLen(pFileName->szName, nLen, false);
        pFileName->szName = hb_strLTrim(pFileName->szName, &nLen);
        (const_cast<char *>(pFileName->szName))[nLen] = '\0';
      }
      if (pFileName->szExtension) {
        HB_SIZE nLen = strlen(pFileName->szExtension);
        nLen = hb_strRTrimLen(pFileName->szExtension, nLen, false);
        pFileName->szExtension = hb_strLTrim(pFileName->szExtension, &nLen);
        (const_cast<char *>(pFileName->szExtension))[nLen] = '\0';
      }
    }

    // FILECASE
    if (iFileCase == HB_SET_CASE_LOWER) {
      if (pFileName->szName) {
        pFileName->szName = pszName = hb_cdpnDupLower(cdp, pFileName->szName, nullptr);
      }
      if (pFileName->szExtension) {
        pFileName->szExtension = pszExt = hb_cdpnDupLower(cdp, pFileName->szExtension, nullptr);
      }
    } else if (iFileCase == HB_SET_CASE_UPPER) {
      if (pFileName->szName) {
        pFileName->szName = pszName = hb_cdpnDupUpper(cdp, pFileName->szName, nullptr);
      }
      if (pFileName->szExtension) {
        pFileName->szExtension = pszExt = hb_cdpnDupUpper(cdp, pFileName->szExtension, nullptr);
      }
    }

    // DIRCASE
    if (pFileName->szPath) {
      if (iDirCase == HB_SET_CASE_LOWER) {
        pFileName->szPath = pszPath = hb_cdpnDupLower(cdp, pFileName->szPath, nullptr);
      } else if (iDirCase == HB_SET_CASE_UPPER) {
        pFileName->szPath = pszPath = hb_cdpnDupUpper(cdp, pFileName->szPath, nullptr);
      }
    }

    hb_fsFNameMerge(pszBuffer, pFileName);
    hb_xfree(pFileName);
    if (pszPath) {
      hb_xfree(pszPath);
    }
    if (pszName) {
      hb_xfree(pszName);
    }
    if (pszExt) {
      hb_xfree(pszExt);
    }
  }

  HB_WCHAR *lpwFileName = hb_cdpStrDupU16(cdp, HB_CDP_ENDIAN_NATIVE, pszFileName);
  if (pszBuffer) {
    hb_xfree(pszBuffer);
  }

  return lpwFileName;
}
#endif // HB_OS_WIN

// NOTE: pszBuffer must be HB_PATH_MAX long.
void hb_fsBaseDirBuff(char *pszBuffer)
{
  char *pszBaseName = hb_cmdargProgName();

  if (pszBaseName) {
    PHB_FNAME pFileName = hb_fsFNameSplit(pszBaseName);
    pFileName->szName = nullptr;
    pFileName->szExtension = nullptr;
    hb_fsFNameMerge(pszBuffer, pFileName);
    hb_xfree(pFileName);
    hb_xfree(pszBaseName);
  } else {
    pszBuffer[0] = '\0';
  }
}

static bool hb_fsDisableWaitLocks(int iSet)
{
  bool fRetVal = s_fUseWaitLocks;

  if (iSet >= 0) {
    s_fUseWaitLocks = (iSet == 0);
  }

  return fRetVal;
}

HB_FUNC(HB_DISABLEWAITLOCKS)
{
  hb_retl(hb_fsDisableWaitLocks(hb_parldef(1, -1)));
}
