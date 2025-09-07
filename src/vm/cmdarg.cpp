//
// Command-line and environment argument management
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

// NOTE: Need to have these before Harbour headers,
//       because in MT mode, they will automatically #include <os2.h>.
#define INCL_DOSPROCESS
#define INCL_DOSERRORS
#define INCL_DOSMODULEMGR

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapifs.hpp"
#include "hbapicdp.hpp"
#include "hbvm.hpp"
#include "hbmemory.ch"
#include "hbstack.hpp"
#include "hbverbld.h"

// Command-line argument management
static int s_argc = 0;
static char **s_argv = nullptr;

#if !defined(HB_OS_WIN)

static char s_szAppName[HB_PATH_MAX];

#else

#include "hbwinuni.hpp"
#include <windows.h>

static LPTSTR *s_lpArgV = nullptr;
#if defined(UNICODE)
static LPSTR *s_lpArgVStr = nullptr;
#endif

static HANDLE s_hInstance = 0;
static HANDLE s_hPrevInstance = 0;
static int s_iCmdShow = 0;
static auto s_WinMainParam = false;

#define HB_WINARG_ALLOC(n) HeapAlloc(GetProcessHeap(), 0, (n))
#define HB_WINARG_FREE(p) HeapFree(GetProcessHeap(), 0, (p))

void hb_winmainArgVBuild(void)
{

  // NOTE: MAX_PATH used intentionally instead of HB_MAX_PATH
  auto lpModuleName = static_cast<LPTSTR>(HB_WINARG_ALLOC((MAX_PATH + 1) * sizeof(TCHAR)));
  HB_SIZE nModuleName = GetModuleFileName(nullptr, lpModuleName, MAX_PATH + 1);
  if (nModuleName) {
    nModuleName++;
  }
  HB_WINARG_FREE(lpModuleName);

  LPCTSTR lpCmdLine = GetCommandLine();
  LPTSTR *lpArgV = nullptr;
  HB_SIZE nSize = 0;
  int iArgC = -1;

  LPTSTR lpDst;
  LPTSTR lpArg;

  while (lpCmdLine && !lpArgV && iArgC != 0) {
    if (nSize != 0) {
      lpArgV = static_cast<LPTSTR *>(HB_WINARG_ALLOC(iArgC * sizeof(LPTSTR) + nSize * sizeof(TCHAR)));
      lpDst = reinterpret_cast<LPTSTR>(lpArgV + iArgC);
      lpArgV[0] = lpDst;
      lpDst += nModuleName;
    } else {
      lpDst = const_cast<LPTSTR>(lpCmdLine);
      nSize = nModuleName;
    }

    LPCTSTR lpSrc = lpCmdLine;
    lpArg = nullptr;
    iArgC = 0;
    auto fQuoted = false;

    while (*lpSrc != 0) {
      if (*lpSrc == TEXT('"')) {
        if (lpArg == nullptr) {
          lpArg = lpDst;
        }
        fQuoted = !fQuoted;
      } else if (fQuoted || !HB_ISSPACE(*lpSrc)) {
        if (lpArg == nullptr) {
          lpArg = lpDst;
        }
        if (iArgC > 0 || nModuleName == 0) {
          if (lpArgV) {
            *lpDst++ = *lpSrc;
          } else {
            nSize++;
          }
        }
      } else {
        if (lpArg) {
          if (iArgC > 0 || nModuleName == 0) {
            if (lpArgV) {
              *lpDst++ = '\0';
              lpArgV[iArgC] = lpArg;
            } else {
              nSize++;
            }
          }
          iArgC++;
          lpArg = nullptr;
        }
      }
      ++lpSrc;
    }
    if (lpArg) {
      if (iArgC > 0 || nModuleName == 0) {
        if (lpArgV) {
          *lpDst = '\0';
          lpArgV[iArgC] = lpArg;
        } else {
          nSize++;
        }
      }
      iArgC++;
    }
  }

  if (iArgC <= 0) {
    if (nModuleName != 0) {
      iArgC = 1;
      lpArgV = static_cast<LPTSTR *>(HB_WINARG_ALLOC(iArgC * sizeof(LPTSTR) + nModuleName * sizeof(TCHAR)));
      lpArgV[0] = reinterpret_cast<LPTSTR>(lpArgV + iArgC);
    } else {
      iArgC = 0;
    }
  }
  if (iArgC > 0 && nModuleName != 0) {
    // NOTE: Manually setup the executable name in Windows,
    //       because in console apps the name may be truncated
    //       in some cases, and in GUI apps it's not filled
    //       at all. [vszakats]
    if (GetModuleFileName(nullptr, lpArgV[0], static_cast<DWORD>(nModuleName)) != 0) {
      // Windows XP does not set trailing 0 if buffer is not large enough [druzus]
      lpArgV[0][nModuleName - 1] = 0;
    }
  }

  hb_winmainArgVFree();

  if (iArgC > 0) {
    s_lpArgV = lpArgV;
    s_argc = iArgC;
#if defined(UNICODE)
    {
      LPSTR lpStr;

      nSize = 0;
      for (iArgC = 0; iArgC < s_argc; ++iArgC) {
        nSize += hb_wctomblen(s_lpArgV[iArgC]) + 1;
      }

      s_lpArgVStr = static_cast<LPSTR *>(HB_WINARG_ALLOC(s_argc * sizeof(LPSTR) + nSize * sizeof(char)));
      lpStr = reinterpret_cast<LPSTR>(s_lpArgVStr + s_argc);
      for (iArgC = 0; iArgC < s_argc; ++iArgC) {
        nSize = hb_wctomblen(s_lpArgV[iArgC]) + 1;
        hb_wcntombcpy(lpStr, s_lpArgV[iArgC], nSize - 1);
        s_lpArgVStr[iArgC] = lpStr;
        lpStr += nSize;
      }
      s_argv = s_lpArgVStr;
    }
#else
    s_argv = s_lpArgV;
#endif
  }
}

void hb_winmainArgVFree(void)
{
  if (s_lpArgV) {
#if defined(UNICODE)
    if (s_lpArgVStr) {
      if (s_argv == s_lpArgVStr) {
        s_argv = nullptr;
      }
      HB_WINARG_FREE(s_lpArgVStr);
      s_lpArgVStr = nullptr;
    }
#else
    if (s_argv == s_lpArgV) {
      s_argv = nullptr;
    }
#endif

    HB_WINARG_FREE(s_lpArgV);
    s_lpArgV = nullptr;
    s_argc = 0;
  }
}

void hb_winmainArgInit(void *hInstance, void *hPrevInstance, int iCmdShow)
{
  s_hInstance = static_cast<HANDLE>(hInstance);
  s_hPrevInstance = static_cast<HANDLE>(hPrevInstance);
  s_iCmdShow = iCmdShow;
  s_WinMainParam = true;
}

HB_BOOL hb_winmainArgGet(void *phInstance, void *phPrevInstance, int *piCmdShow)
{
  if (phInstance) {
    *(static_cast<HANDLE *>(phInstance)) = s_hInstance;
  }
  if (phPrevInstance) {
    *(static_cast<HANDLE *>(phPrevInstance)) = s_hPrevInstance;
  }
  if (piCmdShow) {
    *piCmdShow = s_iCmdShow;
  }

  return s_WinMainParam;
}

#endif

void hb_cmdargInit(int argc, char *argv[])
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargInit(%d, %p)", argc, static_cast<void*>(argv)));
#endif

#if defined(HB_OS_WIN)
  if (s_lpArgV) {
    return;
  }
#endif

  if (argc == 0 || argv == nullptr) {
    s_argc = 0;
    s_argv = nullptr;
  } else {
    s_argc = argc;
    s_argv = argv;
  }
}

int hb_cmdargARGC(void)
{
  return s_argc;
}

char **hb_cmdargARGV(void)
{
  return s_argv;
}

const char *hb_cmdargARGVN(int argc)
{
  return argc >= 0 && argc < s_argc ? s_argv[argc] : nullptr;
}

// NOTE: Pointer must be freed with hb_xfree() if not nullptr

static char *hb_cmdargDup(int argc)
{
#if defined(HB_OS_WIN)
  if (s_lpArgV) {
    return argc >= 0 && argc < s_argc ? HB_OSSTRDUP(s_lpArgV[argc]) : nullptr;
  }
#endif
  return argc >= 0 && argc < s_argc ? hb_osStrDecode(s_argv[argc]) : nullptr;
}

void hb_cmdargUpdate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargUpdate()"));
#endif

#if !defined(HB_OS_WIN)
  if (s_argc > 0) {
    // NOTE: try to create absolute path from s_argv[0] if necessary
    {
      PHB_FNAME pFName = hb_fsFNameSplit(s_argv[0]);
      auto fInPath = false;

      if (!pFName->szPath) {
        char *pszPATH = hb_getenv("PATH");

        if (pszPATH && *pszPATH) {
          HB_PATHNAMES *pSearchPath = nullptr, *pNextPath;
          hb_fsAddSearchPath(pszPATH, &pSearchPath);
          pNextPath = pSearchPath;

          while (pNextPath) {
            pFName->szPath = pNextPath->szPath;
            hb_fsFNameMerge(s_szAppName, pFName);
            if (hb_fsFileExists(s_szAppName)) {
              // even if the file is located using PATH then it does
              // not mean we will have absolute path here. It's not
              // good idea but PATH envvar can also contain relative
              // directories, f.e. "." or "bin" so we should add
              // current directory if necessary in code below.
              hb_xfree(pFName);
              pFName = hb_fsFNameSplit(s_szAppName);
              fInPath = true;
              break;
            }
            pNextPath = pNextPath->pNext;
          }
          hb_fsFreeSearchPath(pSearchPath);
          if (!fInPath) {
            pFName->szPath = nullptr;
          }
        }
        if (pszPATH) {
          hb_xfree(pszPATH);
        }
      }
      if (pFName->szPath) {
#if defined(HB_OS_HAS_DRIVE_LETTER)
        if (pFName->szPath[0] != HB_OS_PATH_DELIM_CHR && !pFName->szDrive) {
#else
        if (pFName->szPath[0] != HB_OS_PATH_DELIM_CHR) {
#endif
          if (pFName->szPath[0] == '.' && pFName->szPath[1] == HB_OS_PATH_DELIM_CHR) {
            pFName->szPath += 2;
          }
          s_szAppName[0] = HB_OS_PATH_DELIM_CHR;
          hb_fsCurDirBuff(0, s_szAppName + 1, HB_PATH_MAX - 1);
          if (s_szAppName[1] != 0) {
            hb_strncat(s_szAppName, HB_OS_PATH_DELIM_CHR_STRING, HB_PATH_MAX - 1);
            hb_strncat(s_szAppName, pFName->szPath, HB_PATH_MAX - 1);
            pFName->szPath = hb_strdup(s_szAppName);
            hb_fsFNameMerge(s_szAppName, pFName);
            hb_xfree(HB_UNCONST(pFName->szPath));
            s_argv[0] = s_szAppName;
          }
        } else if (fInPath) {
          s_argv[0] = s_szAppName;
        }
      }
      hb_xfree(pFName);
    }
  }
#endif
}

// places application parameters on the HVM stack

int hb_cmdargPushArgs(void)
{
  int iArgCount = 0;

  for (auto i = 1; i < s_argc; i++) {
    // Filter out any parameters beginning with //, like //INFO
    if (!hb_cmdargIsInternal(s_argv[i], nullptr)) {
#if defined(HB_OS_WIN)
      if (s_lpArgV) {
        HB_ITEMPUTSTR(hb_stackAllocItem(), s_lpArgV[i]);
      } else
#endif
      {
        hb_vmPushString(s_argv[i], strlen(s_argv[i]));
      }
      iArgCount++;
    }
  }

  return iArgCount;
}

HB_BOOL hb_cmdargIsInternal(const char *szArg, int *piLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargIsInternal(%s, %p)", szArg, static_cast<void*>(piLen)));
#endif

  // NOTE: Not checking for '--' here, as it would filter out
  //       valid command-line options used by applications. [vszakats]

  if (hb_strnicmp(szArg, "--hb:", 5) == 0 || hb_strnicmp(szArg, "//hb:", 5) == 0) {
    if (piLen) {
      *piLen = 5;
    }

    return true;
  } else if (strlen(szArg) >= 2 && szArg[0] == '/' && szArg[1] == '/') {
    if (piLen) {
      *piLen = 2;
    }

    return true;
  }

  return false;
}

static char *hb_cmdargGet(const char *pszName, bool bRetValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargGet(%s, %d)", pszName, static_cast<int>(bRetValue)));
#endif

  int iPrefixLen;

  // Check the command-line first

  for (auto i = 1; i < s_argc; i++) {
    if (hb_cmdargIsInternal(s_argv[i], &iPrefixLen) &&
        hb_strnicmp(s_argv[i] + iPrefixLen, pszName, strlen(pszName)) == 0) {
      if (bRetValue) {
#if defined(HB_OS_WIN)
        if (s_lpArgV) {
          LPCTSTR lpPos = s_lpArgV[i] + iPrefixLen + strlen(pszName);

          if (*lpPos == TEXT(':')) {
            lpPos++;
          }
          return HB_OSSTRDUP(lpPos);
        } else
#endif
        {
          char *pszPos = s_argv[i] + iPrefixLen + strlen(pszName);

          if (*pszPos == ':') {
            pszPos++;
          }

          return hb_osStrDecode(pszPos);
        }
      } else {
        return const_cast<char *>("");
      }
    }
  }

  // Check the environment variable
  char *pszEnvVar = hb_getenv("HARBOUR");
  if (!pszEnvVar || pszEnvVar[0] == '\0') {
    if (pszEnvVar) {
      hb_xfree(pszEnvVar);
    }

    pszEnvVar = hb_getenv("CLIPPER");
  }

  char *pszRetVal = nullptr;

  if (pszEnvVar && pszEnvVar[0] != '\0') {
    char *pszNext = pszEnvVar;

    // Step through all envvar switches.

    // NOTE: CA-Cl*pper doesn't need the switches to be separated by any
    //       chars at all, Harbour is more strict/standard in this respect,
    //       it requires the switches to be separated.

    auto i = static_cast<int>(strlen(pszName));
    while (*pszNext) {
      static const char *s_szSeparator = " ;,\t";
      char *pszEnd;

      // Skip the separators
      while (*pszNext && strchr(s_szSeparator, *pszNext)) {
        pszNext++;
      }

      // The // is optional in the envvar
      if (hb_cmdargIsInternal(pszNext, &iPrefixLen)) {
        pszNext += iPrefixLen;
      }

      pszEnd = pszNext;
      // Search for the end of this switch
      while (*pszEnd && strchr(s_szSeparator, *pszEnd) == nullptr) {
        pszEnd++;
      }

      // Check the switch
      if (hb_strnicmp(pszNext, pszName, i) == 0) {
        if (bRetValue) {
          HB_SIZE nLen;
          pszNext += i;

          // Skip value separator colon.
          if (*pszNext == ':') {
            pszNext++;
          }

          nLen = pszEnd > pszNext ? pszEnd - pszNext : 0;
          pszRetVal = static_cast<char *>(hb_xgrab(nLen + 1));
          hb_strncpy(pszRetVal, pszNext, nLen);
        } else {
          pszRetVal = const_cast<char *>("");
        }
        break;
      }

      // Step to the next switch
      pszNext = pszEnd;
    }
  }

  if (pszEnvVar) {
    hb_xfree(pszEnvVar);
  }

  return pszRetVal;
}

HB_BOOL hb_cmdargCheck(const char *pszName)
{
  return hb_cmdargGet(pszName, false) != nullptr;
}

// NOTE: Pointer must be freed with hb_xfree() if not nullptr

char *hb_cmdargString(const char *pszName)
{
  return hb_cmdargGet(pszName, true);
}

int hb_cmdargNum(const char *pszName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargNum(%s)", pszName));
#endif

  char *pszValue = hb_cmdargGet(pszName, true);
  if (pszValue) {
    int iValue = atoi(pszValue);
    hb_xfree(pszValue);
    return iValue;
  } else {
    return -1;
  }
}

// NOTE: Pointer must be freed with hb_xfree() if not nullptr

char *hb_cmdargProgName(void)
{
  return hb_cmdargDup(0);
}

// NOTE: Pointer must be freed with hb_xfree() if not nullptr

char *hb_cmdargBaseProgName(void)
{
  char *pszBaseProgName = nullptr;

  char *pszProgName = hb_cmdargProgName();
  if (pszProgName) {
    PHB_FNAME pFileName = hb_fsFNameSplit(pszProgName);

    pszBaseProgName = hb_strdup(pFileName->szName);
    hb_xfree(pFileName);
    hb_xfree(pszProgName);
  }

  return pszBaseProgName;
}

// Check if an internal switch has been set

HB_FUNC(HB_ARGCHECK)
{
  hb_retl(HB_ISCHAR(1) ? hb_cmdargCheck(hb_parc(1)) : false);
}

// Returns the value of an internal switch

HB_FUNC(HB_ARGSTRING)
{
  auto pszName = hb_parc(1);

  if (pszName) {
    char *pszValue = hb_cmdargString(pszName);

    if (pszValue) {
      hb_retc_buffer(pszValue);
      return;
    }
  }

  hb_retc_null();
}

// Returns the number of command-line arguments passed to the application, this
// also includes the internal arguments.

HB_FUNC(HB_ARGC)
{
  hb_retni(s_argc - 1);
}

// Returns a command-line argument passed to the application. Calling it with
// the parameter zero or no parameter, it will return the name of the executable,
// as written in the command-line.

HB_FUNC(HB_ARGV)
{
  char *pszArg = hb_cmdargDup(hb_parni(1));

  if (pszArg) {
    hb_retc_buffer(pszArg);
  } else {
    hb_retc_null();
  }
}

HB_FUNC(HB_ARGSHIFT)
{
  int iArg = 1;

  if (hb_parl(1)) {
    while (iArg < s_argc) {
      if (!hb_cmdargIsInternal(s_argv[iArg], nullptr)) {
        s_argv[0] = s_argv[iArg];
#if defined(HB_OS_WIN)
        if (s_lpArgV) {
          s_lpArgV[0] = s_lpArgV[iArg];
        }
#endif
        break;
      }
      ++iArg;
    }
  }
  if (iArg < s_argc) {
    --s_argc;
    while (iArg < s_argc) {
      s_argv[iArg] = s_argv[iArg + 1];
#if defined(HB_OS_WIN)
      if (s_lpArgV) {
        s_lpArgV[iArg] = s_lpArgV[iArg + 1];
      }
#endif
      ++iArg;
    }
  }
}

HB_FUNC(HB_ACMDLINE)
{
  if (s_argc > 1) {
    int iLen = s_argc - 1;
    auto pArray = hb_itemArrayNew(iLen);

    for (auto iPos = 1; iPos <= iLen; ++iPos) {
      hb_arraySetCPtr(pArray, iPos, hb_cmdargDup(iPos));
    }

    hb_itemReturnRelease(pArray);
  } else {
    hb_reta(0);
  }
}

HB_FUNC(HB_CMDLINE)
{
  if (s_argc > 1) {
    HB_SIZE nLen = 0;
    int iArg;

#if defined(HB_OS_WIN)
    if (s_lpArgV) {
      LPTSTR lpBuffer, ptr;

      for (iArg = 1; iArg < s_argc; iArg++) {
        nLen += HB_STRLEN(s_lpArgV[iArg]) + 1;
      }

      ptr = lpBuffer = static_cast<LPTSTR>(hb_xgrab(nLen * sizeof(TCHAR)));
      for (iArg = 1; iArg < s_argc; iArg++) {
        nLen = HB_STRLEN(s_lpArgV[iArg]);
        memcpy(ptr, s_lpArgV[iArg], nLen * sizeof(TCHAR));
        ptr += nLen;
        *ptr++ = TEXT(' ');
      }
      *--ptr = TEXT('\0');

      // Convert from OS codepage
#if defined(UNICODE)
      HB_RETSTR(lpBuffer);
      hb_xfree(lpBuffer);
#else
      hb_retc_buffer(static_cast<char *>(hb_osDecodeCP(lpBuffer, nullptr, nullptr)));
#endif
    } else
#endif
    {
      char *pszBuffer, *ptr;

      for (iArg = 1; iArg < s_argc; iArg++) {
        nLen += strlen(s_argv[iArg]) + 1;
      }

      ptr = pszBuffer = static_cast<char *>(hb_xgrab(nLen));
      for (iArg = 1; iArg < s_argc; iArg++) {
        nLen = strlen(s_argv[iArg]);
        memcpy(ptr, s_argv[iArg], nLen);
        ptr += nLen;
        *ptr++ = ' ';
      }
      *--ptr = '\0';

      // Convert from OS codepage
      hb_retc_buffer(const_cast<char *>(hb_osDecodeCP(pszBuffer, nullptr, nullptr)));
    }
  } else {
    hb_retc_null();
  }
}

// Check for command-line internal arguments
void hb_cmdargProcess(void)
{
#if 0
   int iHandles;
#endif

  if (hb_cmdargCheck("INFO")) {
    {
      char *pszVersion = hb_verHarbour();
      hb_conOutErr(pszVersion, 0);
      hb_conOutErr(hb_conNewLine(), 0);
      hb_xfree(pszVersion);
    }

    {
      char *pszVersion = hb_verPlatform();
      hb_conOutErr(pszVersion, 0);
      hb_conOutErr(hb_conNewLine(), 0);
      hb_xfree(pszVersion);
    }

    {
      char buffer[128];
#if defined(HB_CLP_STRICT)
      hb_snprintf(buffer, sizeof(buffer), "DS avail=%" HB_PFS "uKB  OS avail=%" HB_PFS "uKB  EMM avail=%" HB_PFS "uKB",
                  hb_xquery(HB_MEM_BLOCK), hb_xquery(HB_MEM_VM), hb_xquery(HB_MEM_EMS));
#else
      hb_snprintf(buffer, sizeof(buffer),
                  "DS avail=%" HB_PFS "uKB  OS avail=%" HB_PFS "uKB  EMM avail=%" HB_PFS "uKB  MemStat:%s  MT:%s",
                  hb_xquery(HB_MEM_BLOCK), hb_xquery(HB_MEM_VM), hb_xquery(HB_MEM_EMS),
                  hb_xquery(HB_MEM_STATISTICS) ? "On" : "Off", hb_vmIsMt() ? "On" : "Off");
#endif
      hb_conOutErr(buffer, 0);
      hb_conOutErr(hb_conNewLine(), 0);
    }
  }

  if (hb_cmdargCheck("BUILD")) {
    hb_verBuildInfo();
  }

#if 0
   iHandles = hb_cmdargNum("F");
   if( iHandles > 20 ) {
   } else if( iHandles < 0 ) {
   }
#endif
}

// Source repository revision number
HB_MAXINT hb_verRevision(void)
{
  return HB_VER_REVID;
}

// ChangeLog ID string
const char *hb_verChangeLogID(void)
{
  return HB_VER_CHLID;
}

// ChangeLog last entry string
const char *hb_verChangeLogLastEntry(void)
{
  return HB_VER_LENTRY;
}

#if defined(HB_LEGACY_LEVEL4)

// Source repository revision number
HB_MAXINT hb_verSvnID(void)
{
  return HB_VER_REVID;
}

// ChangeLog ID string
const char *hb_verSvnChangeLogID(void)
{
  return HB_VER_CHLID;
}

// ChangeLog last entry string
const char *hb_verSvnLastEntry(void)
{
  return HB_VER_LENTRY;
}

#endif

// build time C compiler flags in HB_USER_CFLAGS envvar
const char *hb_verFlagsC(void)
{
#ifdef HB_VER_HB_USER_CFLAGS
  return HB_VER_HB_USER_CFLAGS;
#else
  return "";
#endif
}

// build time linker flags in HB_USER_LDFLAGS envvar
const char *hb_verFlagsL(void)
{
#ifdef HB_VER_HB_USER_LDFLAGS
  return HB_VER_HB_USER_LDFLAGS;
#else
  return "";
#endif
}

// build time Harbour compiler flags in HB_USER_PRGFLAGS envvar
const char *hb_verFlagsPRG(void)
{
#ifdef HB_VER_HB_USER_PRGFLAGS
  return HB_VER_HB_USER_PRGFLAGS;
#else
  return "";
#endif
}

// build time Harbour platform setting
const char *hb_verHB_PLAT(void)
{
#ifdef HB_PLATFORM
  return HB_PLATFORM;
#else
  return "";
#endif
}

// build time Harbour compiler setting
const char *hb_verHB_COMP(void)
{
#ifdef HB_COMPILER
  return HB_COMPILER;
#else
  return "";
#endif
}
