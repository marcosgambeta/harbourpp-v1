//
// Version detection functions
//
// Copyright 1999 {list of individual authors and e-mail addresses}
// Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
//    hb_verPlatform() (support for determining the Windows version)
// Copyright 1999 Jose Lalin <dezac@corevia.com>
//    hb_verPlatform() (support for determining many Windows flavours)
//    hb_verCompiler() (support for determining some compiler version/revision)
// Copyright 2000-2014 Viktor Szakats (vszakats.net/harbour)
//    hb_verCPU(), hb_verHostBitWidth(), hb_iswinver(), hb_iswinsp()
//    hb_verPlatform() (support for detecting Windows NT on DOS, Wine, post-Windows 8, cleanups)
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

#include "hbapi.hpp"
#include "hbmemory.ch"

#if defined(HB_OS_WIN)

#include <windows.h>
#include "hbwinuni.hpp"

#ifndef VER_PLATFORM_WIN32_WINDOWS
#define VER_PLATFORM_WIN32_WINDOWS 1
#endif
#ifndef VER_PLATFORM_WIN32_CE
#define VER_PLATFORM_WIN32_CE 3
#endif

#ifndef VER_NT_WORKSTATION
#define VER_NT_WORKSTATION 0x0000001
#endif
#ifndef VER_NT_DOMAIN_CONTROLLER
#define VER_NT_DOMAIN_CONTROLLER 0x0000002
#endif
#ifndef VER_NT_SERVER
#define VER_NT_SERVER 0x0000003
#endif

#ifndef VER_MINORVERSION
#define VER_MINORVERSION 0x0000001
#endif
#ifndef VER_MAJORVERSION
#define VER_MAJORVERSION 0x0000002
#endif
#ifndef VER_SERVICEPACKMINOR
#define VER_SERVICEPACKMINOR 0x0000010
#endif
#ifndef VER_SERVICEPACKMAJOR
#define VER_SERVICEPACKMAJOR 0x0000020
#endif

#ifndef VER_PRODUCT_TYPE
#define VER_PRODUCT_TYPE 0x0000080
#endif
#ifndef VER_EQUAL
#define VER_EQUAL 1
#endif
#ifndef VER_GREATER_EQUAL
#define VER_GREATER_EQUAL 3
#endif

#ifndef SM_SERVERR2
#define SM_SERVERR2 89
#endif

#elif defined(HB_OS_UNIX) && !defined(__CEGCC__)
#include <sys/utsname.h>
#endif

const char *hb_verCPU(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verCPU()"));
#endif

#if defined(HB_CPU_X86)
  return "x86";
#elif defined(HB_CPU_X86_64)
  return "x86-64";
#elif defined(HB_CPU_IA_64)
  return "IA-64";
#elif defined(HB_CPU_PPC)
  return "PPC";
#elif defined(HB_CPU_PPC_64)
  return "PPC64";
#elif defined(HB_CPU_SPARC)
  return "SPARC";
#elif defined(HB_CPU_SPARC_64)
  return "SPARC64";
#elif defined(HB_CPU_ARM)
  return "ARM";
#elif defined(HB_CPU_ARM_64)
  return "ARM64";
#elif defined(HB_CPU_MIPS)
  return "MIPS";
#elif defined(HB_CPU_SH)
  return "SuperH";
#elif defined(HB_CPU_ZARCH)
  return "z/Architecture";
#elif defined(HB_CPU_PARISC)
  return "PA-RISC";
#elif defined(HB_CPU_ALPHA)
  return "Alpha";
#elif defined(HB_CPU_POWER)
  return "POWER";
#elif defined(HB_CPU_M68K)
  return "m68k";
#elif defined(HB_CPU_SYS370)
  return "System/370";
#elif defined(HB_CPU_SYS390)
  return "System/390";
#else
  return "(unrecognized)";
#endif
}

static bool s_win_iswow64(void)
{
  auto bRetVal = false;

#if defined(HB_OS_WIN) && !defined(HB_OS_WIN_64)
  BOOL bIsWow64 = FALSE;

  if (!IsWow64Process(GetCurrentProcess(), &bIsWow64)) {
    // Try alternative method?
  }

  if (bIsWow64) {
    bRetVal = true;
  }
#endif

  return bRetVal;
}

const char *hb_verHostCPU(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verHostCPU()"));
#endif

  if (s_win_iswow64()) {
    return "x86-64";
  }

  return hb_verCPU();
}

int hb_verHostBitWidth(void)
{
  int nBits;

// Inherit the bit width we're building for
#if defined(HB_ARCH_64BIT)
  nBits = 64;
#elif defined(HB_ARCH_32BIT)
  nBits = 32;
#elif defined(HB_ARCH_16BIT)
  nBits = 16;
#else
  nBits = 0;
#endif

  if (s_win_iswow64()) {
    nBits = 64;
  }

  return nBits;
}

// NOTE: OS() function, as a primary goal will detect the version number
//       of the target platform. As an extra it may also detect the host OS.
//       The latter is mainly an issue in DOS, where the host OS can be OS/2
//       WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats]

// NOTE: The caller must free the returned buffer. [vszakats]

// NOTE: The first word of the returned string must describe
//       the OS family as used in __PLATFORM__*. Latter macro
//       will in fact be formed from the string returned
//       by this function. [vszakats]

// NOTE: As it appears in __PLATFORM__* macro
const char *hb_verPlatformMacro(void)
{
#if defined(HB_OS_WIN)
  return "WINDOWS"; // TODO: Change this to WIN for consistency?
#elif defined(HB_OS_LINUX)
  return "LINUX";
#elif defined(HB_OS_DARWIN)
  return "DARWIN";
#elif defined(HB_OS_BSD)
  return "BSD";
#elif defined(HB_OS_SUNOS)
  return "SUNOS";
#elif defined(HB_OS_HPUX)
  return "HPUX";
#elif defined(HB_OS_BEOS)
  return "BEOS";
#elif defined(HB_OS_QNX)
  return "QNX";
#elif defined(HB_OS_VXWORKS)
  return "VXWORKS";
#elif defined(HB_OS_CYGWIN)
  return "CYGWIN";
#else
  return nullptr;
#endif
}

#if defined(HB_OS_WIN)

static auto s_fWinVerInit = false;

static auto s_fWin10 = false;
static auto s_fWin81 = false;
static auto s_fWin8 = false;
static auto s_fWin7 = false;
static auto s_fWinVista = false;
static auto s_fWin2K3 = false;
static auto s_fWin2K = false;
static int s_iWinNT = 0;
static int s_iWin9x = 0;
static int s_iWine = 0;

static auto s_fVerInfoInit = true;

static bool s_hb_winVerifyVersionInit(void)
{
  if (s_fVerInfoInit) {
    HMODULE hModule = GetModuleHandle(TEXT("kernel32.dll"));
    if (hModule) {
    }
    s_fVerInfoInit = false;
  }

  return true;
}

static void s_hb_winVerInit(void)
{
  s_fWin10 = hb_iswinver(10, 0, 0, true);
  s_fWin81 = hb_iswinver(6, 3, 0, true);
  s_fWin8 = hb_iswinver(6, 2, 0, true);
  s_fWin7 = hb_iswinver(6, 1, 0, true);
  s_fWinVista = hb_iswinver(6, 0, 0, true);
  s_fWin2K3 = hb_iswinver(5, 2, VER_NT_SERVER, true) || hb_iswinver(5, 2, VER_NT_DOMAIN_CONTROLLER, true);
  s_fWin2K = hb_iswinver(5, 0, 0, true);

#if !(defined(HB_OS_WIN_64) || (defined(_MSC_VER) && _MSC_VER > 1310))
  {
    OSVERSIONINFO osvi;
    osvi.dwOSVersionInfoSize = sizeof(osvi);
    if (GetVersionEx(&osvi)) {
      // NOTE: Value is VER_PLATFORM_WIN32_CE on WinCE
      if (osvi.dwPlatformId != VER_PLATFORM_WIN32_WINDOWS) {
        s_iWin9x = 0;
      } else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion < 10) {
        s_iWin9x = 5; // 95
      } else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10) {
        s_iWin9x = 8; // 98
      } else {
        s_iWin9x = 9; // ME
      }

      if (osvi.dwPlatformId != VER_PLATFORM_WIN32_NT) {
        s_iWinNT = 0;
      } else if (osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51) {
        s_iWinNT = 3; // 3.51
      } else if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0) {
        s_iWinNT = 4; // 4.0
      } else {
        s_iWinNT = 5; // newer
      }
    }
  }
#endif

  {
    // NOTE: Unofficial Wine detection.
    //       https://www.mail-archive.com/wine-devel@winehq.org/msg48659.html
    HMODULE hntdll = GetModuleHandle(TEXT("ntdll.dll"));
    if (hntdll && HB_WINAPI_GETPROCADDRESS(hntdll, "wine_get_version")) {
      s_iWine = 1;
    }
  }

  if (s_fWin2K) {
    s_iWinNT = 5;
  }

  s_fWinVerInit = true;
}

#endif

// NOTE: Must be larger than 128, which is the maximum size of
//       osvi.szCSDVersion (Windows). [vszakats]
#define PLATFORM_BUF_SIZE 255

char *hb_verPlatform(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verPlatform()"));
#endif

  auto pszPlatform = static_cast<char *>(hb_xgrab(PLATFORM_BUF_SIZE + 1));

#if defined(HB_OS_WIN)

  {
    const char *pszName = "";

    OSVERSIONINFO osvi{};

    // Detection of legacy Windows versions
    switch (hb_iswin9x()) {
    case 5:
      osvi.dwMajorVersion = 4;
      osvi.dwMinorVersion = 0;
      pszName = " 95";
      break;
    case 8:
      osvi.dwMajorVersion = 4;
      osvi.dwMinorVersion = 10;
      pszName = " 98";
      break;
    case 9:
      osvi.dwMajorVersion = 4;
      osvi.dwMinorVersion = 90;
      pszName = " ME";
      break;
    }

    if (pszName[0] == '\0') {
      if (hb_iswinver(11, 0, 0, true)) {
        osvi.dwMajorVersion = 11;
        osvi.dwMinorVersion = 0;
        pszName = " 11 or newer";
      } else if (hb_iswin10()) {
        osvi.dwMajorVersion = 10;
        osvi.dwMinorVersion = 0;
        if (hb_iswinver(10, 0, VER_NT_WORKSTATION, false)) {
          pszName = " 10";
        } else {
          pszName = " Server 2016";
        }
      } else if (hb_iswin81()) {
        osvi.dwMajorVersion = 6;
        osvi.dwMinorVersion = 3;
        if (hb_iswinver(6, 3, VER_NT_WORKSTATION, false)) {
          pszName = " 8.1";
        } else {
          pszName = " Server 2012 R2";
        }
      } else if (hb_iswinvista()) {
        if (hb_iswin8()) {
          osvi.dwMajorVersion = 6;
          osvi.dwMinorVersion = 2;
          if (hb_iswinver(6, 2, VER_NT_WORKSTATION, false)) {
            pszName = " 8";
          } else {
            pszName = " Server 2012";
          }
        } else if (hb_iswinver(6, 1, 0, false)) {
          osvi.dwMajorVersion = 6;
          osvi.dwMinorVersion = 1;
          if (hb_iswinver(6, 1, VER_NT_WORKSTATION, false)) {
            pszName = " 7";
          } else {
            pszName = " Server 2008 R2";
          }
        } else {
          osvi.dwMajorVersion = 6;
          osvi.dwMinorVersion = 0;
          if (hb_iswinver(6, 0, VER_NT_WORKSTATION, false)) {
            pszName = " Vista";
          } else {
            pszName = " Server 2008";
          }
        }
      } else if (hb_iswinver(5, 2, 0, false)) {
        osvi.dwMajorVersion = 5;
        osvi.dwMinorVersion = 2;
        if (hb_iswinver(5, 2, VER_NT_WORKSTATION, false)) {
          pszName = " XP x64";
        } else if (GetSystemMetrics(SM_SERVERR2) != 0) {
          pszName = " Server 2003 R2";
        } else {
          pszName = " Server 2003";
        }
      } else if (hb_iswinver(5, 1, 0, false)) {
        osvi.dwMajorVersion = 5;
        osvi.dwMinorVersion = 1;
        pszName = " XP";
      } else if (hb_iswin2k()) {
        osvi.dwMajorVersion = 5;
        osvi.dwMinorVersion = 0;
        pszName = " 2000";
      } else {
        pszName = " NT";
      }
    }

    hb_snprintf(pszPlatform, PLATFORM_BUF_SIZE + 1, "Windows%s%s %lu.%lu", pszName, s_iWine ? " (Wine)" : "",
                osvi.dwMajorVersion, osvi.dwMinorVersion);

    // Add service pack/other info

    if (hb_iswin2k()) {
      for (auto tmp = 5; tmp > 0; --tmp) {
        if (hb_iswinsp(tmp, true)) {
          char szServicePack[8];
          hb_snprintf(szServicePack, sizeof(szServicePack), " SP%u", tmp);
          hb_strncat(pszPlatform, szServicePack, PLATFORM_BUF_SIZE);
          break;
        }
      }
    }
  }

#elif defined(__CEGCC__)
  {
    hb_snprintf(pszPlatform, PLATFORM_BUF_SIZE + 1, "Windows CE");
  }
#elif defined(HB_OS_UNIX)

  {
    struct utsname un;

    uname(&un);
#if defined(HB_OS_MINIX)
    hb_snprintf(pszPlatform, PLATFORM_BUF_SIZE + 1, "%s Release %s Version %s %s", un.sysname, un.release, un.version,
                un.machine);
#else
    hb_snprintf(pszPlatform, PLATFORM_BUF_SIZE + 1, "%s %s %s", un.sysname, un.release, un.machine);
#endif
  }

#else

  {
    hb_strncpy(pszPlatform, "(unrecognized)", PLATFORM_BUF_SIZE);
  }

#endif

  return pszPlatform;
}

HB_BOOL hb_iswinver(int iMajor, int iMinor, int iType, HB_BOOL fOrUpper)
{
#if defined(HB_OS_WIN)
  if (s_hb_winVerifyVersionInit()) {
    OSVERSIONINFOEXW ver{};
    DWORD dwTypeMask = VER_MAJORVERSION | VER_MINORVERSION;
    DWORDLONG dwlConditionMask = 0;

    ver.dwOSVersionInfoSize = sizeof(ver);
    ver.dwMajorVersion = static_cast<DWORD>(iMajor);
    ver.dwMinorVersion = static_cast<DWORD>(iMinor);

    dwlConditionMask =
        VerSetConditionMask(dwlConditionMask, VER_MAJORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL);
    dwlConditionMask =
        VerSetConditionMask(dwlConditionMask, VER_MINORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL);

    // MSDN says in https://msdn.microsoft.com/library/ms725492
    //   "If you are testing the major version, you must also test the
    //    minor version and the service pack major and minor versions."
    // However, Wine (as of 1.7.53) breaks on this. Since native Windows
    // apparently doesn't care, we're not doing it for now.
    // Wine (emulating Windows 7) will erroneously return false from
    // these calls:
    //   hb_iswinver(6, 1, 0, false);
    //   hb_iswinver(6, 1, VER_NT_WORKSTATION, false);
    // Removing the Service Pack check, or changing HB_FALSE to HB_TRUE
    // in above calls, both fixes the problem. [vszakats]
#if defined(__HB_DISABLE_WINE_VERIFYVERSIONINFO_BUG_WORKAROUND)
    ver.wServicePackMajor = ver.wServicePackMinor = static_cast<WORD>(0);
    dwTypeMask |= VER_SERVICEPACKMAJOR | VER_SERVICEPACKMINOR;
    dwlConditionMask = VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL);
    dwlConditionMask = VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMINOR, VER_GREATER_EQUAL);
#endif

    if (iType) {
      dwTypeMask |= VER_PRODUCT_TYPE;
      ver.wProductType = static_cast<BYTE>(iType);
      dwlConditionMask = VerSetConditionMask(dwlConditionMask, VER_PRODUCT_TYPE, VER_EQUAL);
    }

    return static_cast<HB_BOOL>(VerifyVersionInfo(&ver, dwTypeMask, dwlConditionMask)); // TODO: HB_BOOL -> bool
  }
#else
  HB_SYMBOL_UNUSED(iMajor);
  HB_SYMBOL_UNUSED(iMinor);
  HB_SYMBOL_UNUSED(iType);
  HB_SYMBOL_UNUSED(fOrUpper);
#endif
  return false;
}

HB_BOOL hb_iswinsp(int iServicePackMajor, HB_BOOL fOrUpper)
{
#if defined(HB_OS_WIN)
  if (s_hb_winVerifyVersionInit()) {
    OSVERSIONINFOEXW ver{};
    DWORDLONG dwlConditionMask = 0;

    ver.dwOSVersionInfoSize = sizeof(ver);
    ver.wServicePackMajor = static_cast<WORD>(iServicePackMajor);

    dwlConditionMask =
        VerSetConditionMask(dwlConditionMask, VER_SERVICEPACKMAJOR, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL);

    return static_cast<HB_BOOL>(
        VerifyVersionInfo(&ver, VER_SERVICEPACKMAJOR, dwlConditionMask)); // TODO: HB_BOOL -> bool
  }
#else
  HB_SYMBOL_UNUSED(iServicePackMajor);
  HB_SYMBOL_UNUSED(fOrUpper);
#endif
  return false;
}

int hb_iswine(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_iWine;
#else
  return 0;
#endif
}

HB_BOOL hb_iswin10(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin10;
#else
  return false;
#endif
}

HB_BOOL hb_iswin81(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin81;
#else
  return false;
#endif
}

HB_BOOL hb_iswin8(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin8;
#else
  return false;
#endif
}

HB_BOOL hb_iswin7(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin7;
#else
  return false;
#endif
}

HB_BOOL hb_iswinvista(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWinVista;
#else
  return false;
#endif
}

HB_BOOL hb_iswin2k3(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin2K3;
#else
  return false;
#endif
}

HB_BOOL hb_iswin2k(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_fWin2K;
#else
  return false;
#endif
}

int hb_iswinnt(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_iWinNT;
#else
  return 0;
#endif
}

int hb_iswin9x(void)
{
#if defined(HB_OS_WIN)
  if (!s_fWinVerInit) {
    s_hb_winVerInit();
  }
  return s_iWin9x;
#else
  return 0;
#endif
}

HB_BOOL hb_iswince(void)
{
  return false;
}

// NOTE: The caller must free the returned buffer. [vszakats]

#define COMPILER_BUF_SIZE 80

char *hb_verCompiler(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verCompiler()"));
#endif

  const char *pszName;
  char szSub[64];
  int iVerMajor;
  int iVerMinor;
  int iVerPatch;
  int iVerMicro = 0;
  int iElements = 0;

  auto pszCompiler = static_cast<char *>(hb_xgrab(COMPILER_BUF_SIZE));
  szSub[0] = '\0';

#if defined(__IBMC__) || defined(__IBMCPP__)

#if defined(__IBMC__)
  iVerMajor = __IBMC__;
#else
  iVerMajor = __IBMCPP__;
#endif

  if (iVerMajor >= 300) {
    pszName = "IBM Visual Age C++";
  } else {
    pszName = "IBM C++";
  }

  iVerMajor /= 100;
  iVerMinor = iVerMajor % 100;
  iVerPatch = 0;

#elif defined(__INTEL_COMPILER)

  pszName = "Intel(R) C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __INTEL_COMPILER / 100;
  iVerMinor = (__INTEL_COMPILER % 100) / 10;
  iVerPatch = 0;

#elif defined(__ICL)

  pszName = "Intel(R) C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __ICL / 100;
  iVerMinor = __ICL % 100;
  iVerPatch = 0;

#elif defined(__ICC)

  pszName = "Intel(R) (ICC) C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __ICC / 100;
  iVerMinor = __ICC % 100;
  iVerPatch = 0;

#elif defined(__OPENCC__)

  pszName = "Open64 C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __OPENCC__;
  iVerMinor = __OPENCC_MINOR__;
#if __OPENCC_PATCHLEVEL__ - 0 <= 0
#undef __OPENCC_PATCHLEVEL__
#define __OPENCC_PATCHLEVEL__ 0
#endif
  iVerPatch = __OPENCC_PATCHLEVEL__;

#elif defined(__clang__) && defined(__clang_major__) && !defined(__BORLANDC__)

  // NOTE: keep clang detection before msvc detection.

  pszName = "LLVM/Clang C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __clang_major__;
  iVerMinor = __clang_minor__;
  iVerPatch = __clang_patchlevel__;

#elif defined(__clang__) && !defined(__BORLANDC__)

  pszName = "LLVM/Clang C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  hb_strncat(szSub, " 1.x", sizeof(szSub) - 1);

  iVerMajor = iVerMinor = iVerPatch = 0;

#elif defined(__llvm__) && defined(__GNUC__)

  pszName = "LLVM/GNU C";

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __GNUC__;
  iVerMinor = __GNUC_MINOR__;
#if defined(__GNUC_PATCHLEVEL__)
  iVerPatch = __GNUC_PATCHLEVEL__;
#else
  iVerPatch = 0;
#endif

#elif defined(_MSC_VER)

#if _MSC_VER >= 800
  pszName = "Microsoft Visual C";
#else
  pszName = "Microsoft C";
#endif

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = _MSC_VER / 100;
  iVerMinor = _MSC_VER % 100;

#if defined(_MSC_FULL_VER)
#if _MSC_VER >= 1400
  iVerPatch = _MSC_FULL_VER - (_MSC_VER * 100000);
#else
  iVerPatch = _MSC_FULL_VER - (_MSC_VER * 10000);
#endif
#else
  iVerPatch = 0;
#endif

#elif defined(__BORLANDC__)

#if __BORLANDC__ >= 0x0590 // Version 5.9
#if __BORLANDC__ >= 0x0620 // Version 6.2
  pszName = "Borland/Embarcadero C++";
#else
  pszName = "Borland/CodeGear C++";
#endif
#else
  pszName = "Borland C++";
#endif
#if __BORLANDC__ == 0x0400   // Version 3.0
  iVerMajor = 3;
  iVerMinor = 0;
  iVerPatch = 0;
#elif __BORLANDC__ == 0x0410 // Version 3.1
  iVerMajor = 3;
  iVerMinor = 1;
  iVerPatch = 0;
#elif __BORLANDC__ == 0x0452 // Version 4.0
  iVerMajor = 4;
  iVerMinor = 0;
  iVerPatch = 0;
#elif __BORLANDC__ == 0x0460 // Version 4.5
  iVerMajor = 4;
  iVerMinor = 5;
  iVerPatch = 0;
#elif __BORLANDC__ >= 0x0500 // Version 5.x
  iVerMajor = __BORLANDC__ >> 8;
  iVerMinor = (__BORLANDC__ & 0xFF) >> 4;
  iVerPatch = __BORLANDC__ & 0xF;
#else                        // Version 4.x
  iVerMajor = __BORLANDC__ >> 8;
  iVerMinor = (__BORLANDC__ - 1 & 0xFF) >> 4;
  iVerPatch = 0;
#endif

#elif defined(__MPW__)

  pszName = "MPW C";
  iVerMajor = __MPW__ / 100;
  iVerMinor = __MPW__ % 100;
  iVerPatch = 0;

#elif defined(__DCC__)

  pszName = "Wind River Compiler (diab)";

  iVerMajor = (__VERSION_NUMBER__ / 1000) % 10;
  iVerMinor = (__VERSION_NUMBER__ / 100) % 10;
  iVerPatch = (__VERSION_NUMBER__ / 10) % 10;
  iVerMicro = __VERSION_NUMBER__ % 10;
  iElements = 4;

#elif defined(__GNUC__)

#if defined(__CYGWIN__)
  pszName = "Cygwin GNU C";
#elif defined(__MINGW32__)
  pszName = "MinGW GNU C";
#else
  pszName = "GNU C";
#endif

#if defined(__cplusplus)
  hb_strncpy(szSub, "++", sizeof(szSub) - 1);
#endif

  iVerMajor = __GNUC__;
  iVerMinor = __GNUC_MINOR__;
#if defined(__GNUC_PATCHLEVEL__)
  iVerPatch = __GNUC_PATCHLEVEL__;
#else
  iVerPatch = 0;
#endif

#elif defined(__SUNPRO_C)

  pszName = "Sun C";
#if __SUNPRO_C < 0x1000
  iVerMajor = __SUNPRO_C / 0x100;
  iVerMinor = (__SUNPRO_C & 0xff) / 0x10;
  iVerPatch = __SUNPRO_C & 0xf;
#else
  iVerMajor = __SUNPRO_C / 0x1000;
  iVerMinor = __SUNPRO_C / 0x10 & 0xff;
  iVerMinor = iVerMinor / 0x10 * 0xa + iVerMinor % 0x10;
  iVerPatch = __SUNPRO_C & 0xf;
#endif

#elif defined(__SUNPRO_CC)

  pszName = "Sun C++";
#if __SUNPRO_CC < 0x1000
  iVerMajor = __SUNPRO_CC / 0x100;
  iVerMinor = (__SUNPRO_CC & 0xff) / 0x10;
  iVerPatch = __SUNPRO_CC & 0xf;
#else
  iVerMajor = __SUNPRO_CC / 0x1000;
  iVerMinor = __SUNPRO_CC / 0x10 & 0xff;
  iVerMinor = iVerMinor / 0x10 * 0xa + iVerMinor % 0x10;
  iVerPatch = __SUNPRO_CC & 0xf;
#endif

#else

  pszName = nullptr;
  iVerMajor = iVerMinor = iVerPatch = 0;

#endif

  if (pszName) {
    if (iElements == 4) {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch,
                  iVerMicro);
    } else if (iVerPatch != 0) {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch);
    } else if (iVerMajor != 0 || iVerMinor != 0) {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d", pszName, szSub, iVerMajor, iVerMinor);
    } else {
      hb_snprintf(pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub);
    }
  } else {
    hb_strncpy(pszCompiler, "(unknown)", COMPILER_BUF_SIZE - 1);
  }

#if defined(__clang_version__) && !defined(__BORLANDC__)
  if (strstr(__clang_version__, "(")) {
    // "2.0 (trunk 103176)" -> "(trunk 103176)"
    hb_snprintf(szSub, sizeof(szSub), " %s", strstr(__clang_version__, "("));
  } else {
    hb_snprintf(szSub, sizeof(szSub), " (%s)", __clang_version__);
  }
  hb_strncat(pszCompiler, szSub, COMPILER_BUF_SIZE - 1);
#endif

#if defined(HB_ARCH_16BIT)
  hb_strncat(pszCompiler, " (16-bit)", COMPILER_BUF_SIZE - 1);
#elif defined(HB_ARCH_32BIT)
  hb_strncat(pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1);
#elif defined(HB_ARCH_64BIT)
  hb_strncat(pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1);
#endif

  return pszCompiler;
}

// NOTE: The caller must free the returned buffer. [vszakats]

// NOTE:
// CA-Cl*pper 5.2e returns: "Clipper (R) 5.2e Intl. (x216)  (1995.02.07)"
// CA-Cl*pper 5.3b returns: "Clipper (R) 5.3b Intl. (Rev. 338) (1997.04.25)"

char *hb_verHarbour(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verHarbour()"));
#endif

  auto pszVersion = static_cast<char *>(hb_xgrab(80));
  hb_snprintf(pszVersion, 80, "Harbour++ %d.%d.%d%s (r%lu)", HB_VER_MAJOR, HB_VER_MINOR, HB_VER_RELEASE, HB_VER_STATUS,
              static_cast<HB_ULONG>(hb_verRevision()));
  return pszVersion;
}

char *hb_verPCode(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verPCode()"));
#endif

  auto pszPCode = static_cast<char *>(hb_xgrab(24));
  hb_snprintf(pszPCode, 24, "PCode version: %d.%d", HB_PCODE_VER >> 8, HB_PCODE_VER & 0xFF);
  return pszPCode;
}

char *hb_verBuildDate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_verBuildDate()"));
#endif

  auto pszDate = static_cast<char *>(hb_xgrab(64));
// TODO:
// error: expansion of date or time macro is not reproducible [-Werror,-Wdate-time]
#if defined(__ZIGCOMPILER__)
  hb_snprintf(pszDate, 64, "%s %s", "", "");
#else
  hb_snprintf(pszDate, 64, "%s %s", __DATE__, __TIME__);
#endif
  return pszDate;
}
