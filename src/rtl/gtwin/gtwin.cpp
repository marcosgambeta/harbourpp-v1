//
// Video subsystem for Windows compilers ver.2
// Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
//
// based on
//   Bcc ConIO Video subsystem by
//     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
//     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
//   Video subsystem for Windows compilers
//     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
//     (with 2004 work on Readkey)
//
// Copyright 1999-2010 Viktor Szakats (vszakats.net/harbour)
//    hb_gt_win_CtrlHandler()
//    hb_gt_win_SetCloseButton()
//    hb_gt_win_SetPalette*()
//
// Copyright 1999 David G. Holm <dholm@jsd-llc.com>
//    hb_gt_Tone()
//    hb_gt_ReadKey()
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

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME WIN

/* TODO: include any standard headers here */
/* *********************************************************************** */

#include "hbgtcore.hpp"
#include "hbinit.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbwinuni.hpp"
#include "hbdate.hpp"
#include "hbapicdp.hpp"

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600 /* for hb_gt_win_SetPalette_Vista() */

#include <windows.h>
#include <wincon.h>

#if defined(_MSC_VER)
#include <conio.h>
#endif

#ifndef HB_GTWIN_USE_SETCONSOLEMENUCLOSE_OFF
#define HB_GTWIN_USE_SETCONSOLEMENUCLOSE /* Enable undocumented Windows API function call */
#endif

struct _HB_CONSOLE_SCREEN_BUFFER_INFOEX
{
  ULONG cbSize;
  COORD dwSize;
  COORD dwCursorPosition;
  WORD wAttributes;
  SMALL_RECT srWindow;
  COORD dwMaximumWindowSize;
  WORD wPopupAttributes;
  BOOL bFullscreenSupported;
  COLORREF ColorTable[16];
};

using HB_CONSOLE_SCREEN_BUFFER_INFOEX = _HB_CONSOLE_SCREEN_BUFFER_INFOEX;
using HB_PCONSOLE_SCREEN_BUFFER_INFOEX = HB_CONSOLE_SCREEN_BUFFER_INFOEX *;

#undef CONSOLE_SCREEN_BUFFER_INFOEX
#undef PCONSOLE_SCREEN_BUFFER_INFOEX
#define CONSOLE_SCREEN_BUFFER_INFOEX HB_CONSOLE_SCREEN_BUFFER_INFOEX
#define PCONSOLE_SCREEN_BUFFER_INFOEX HB_PCONSOLE_SCREEN_BUFFER_INFOEX
#if !defined(HB_GTWIN_USE_PCONSOLEINFOEX)
#define HB_GTWIN_USE_PCONSOLEINFOEX
#endif

#ifndef MOUSE_WHEELED
#define MOUSE_WHEELED 0x0004
#endif

#ifndef MOUSE_HWHEELED
#define MOUSE_HWHEELED 0x0008
#endif

#ifndef CONSOLE_FULLSCREEN_HARDWARE
#define CONSOLE_FULLSCREEN_HARDWARE 2
#endif

#ifndef CONSOLE_FULLSCREEN_MODE
#define CONSOLE_FULLSCREEN_MODE 1
#endif

#ifndef CONSOLE_WINDOWED_MODE
#define CONSOLE_WINDOWED_MODE 0
#endif

/* *********************************************************************** */

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
#define HB_GTID_PTR (&s_GtId)

static const COLORREF s_colorsDef[16] = {
    RGB(0x00, 0x00, 0x00), RGB(0x00, 0x00, 0x80), RGB(0x00, 0x80, 0x00), RGB(0x00, 0x80, 0x80),
    RGB(0x80, 0x00, 0x00), RGB(0x80, 0x00, 0x80), RGB(0x80, 0x80, 0x00), RGB(0xC0, 0xC0, 0xC0),
    RGB(0x80, 0x80, 0x80), RGB(0x00, 0x00, 0xFF), RGB(0x00, 0xFF, 0x00), RGB(0x00, 0xFF, 0xFF),
    RGB(0xFF, 0x00, 0x00), RGB(0xFF, 0x00, 0xFF), RGB(0xFF, 0xFF, 0x00), RGB(0xFF, 0xFF, 0xFF)};

static auto s_fWin9x = false;
static COLORREF s_colorsOld[16];
static auto s_fResetColors = false;
static auto s_fOldClosable = false;
static auto s_fClosable = false;
static auto s_fMouseEnable = false;
static auto s_fSpecialKeyHandling = false;
static auto s_fAltKeyHandling = false;
static auto s_fBreak = false;
static auto s_fSuspend = false;
static int s_iCursorStyle;
static int s_iOldCurStyle;
static int s_iCurRow;
static int s_iCurCol;
static int s_iUpdtTop;
static int s_iUpdtBottom;
static int s_iUpdtLeft;
static int s_iUpdtRight;
static CHAR_INFO *s_pCharInfoScreen = nullptr;
static HB_SIZE s_nScreenBuffSize = 0;

static HB_FHANDLE s_hStdIn, s_hStdOut, s_hStdErr;

static HANDLE s_HInput = INVALID_HANDLE_VALUE;
static HANDLE s_HOutput = INVALID_HANDLE_VALUE;
static DWORD s_dwimode, s_dwomode;
static CONSOLE_SCREEN_BUFFER_INFO s_csbi, s_origCsbi; /* active screen mode */
                                                      /* to restore screen mode on exit */

/* faster macro version for use inside this module */
#define _GetScreenWidth() (s_csbi.dwSize.X)
#define _GetScreenHeight() (s_csbi.dwSize.Y)

#define INPUT_BUFFER_LEN 32

static DWORD s_dwNumRead;  /* Ok to use DWORD here, because this is specific... */
static DWORD s_dwNumIndex; /* ...to the Windows API, which defines DWORD, etc.  */
static INPUT_RECORD s_irBuffer[INPUT_BUFFER_LEN];
static auto s_fAltIsDown = false;
static int s_iAltVal = 0;

static int s_mouse_buttons;
static int s_mouse_col;
static int s_mouse_row;

/* *********************************************************************** */

static int hb_gt_win_keyFlags(DWORD dwState)
{
  int iFlags = 0;

  if (dwState & SHIFT_PRESSED)
  {
    iFlags |= HB_KF_SHIFT;
  }
  if (dwState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED))
  {
    iFlags |= HB_KF_CTRL;
  }
  if (dwState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED))
  {
    iFlags |= HB_KF_ALT;
  }

  return iFlags;
}

static int hb_gt_win_getKbdState(void)
{
  int iKbdState = 0;

  if (GetKeyState(VK_SHIFT) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_SHIFT;
  }
  if (GetKeyState(VK_CONTROL) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_CTRL;
  }
  if (GetKeyState(VK_MENU) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_ALT;
  }
  if (GetKeyState(VK_LWIN) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_LWIN;
  }
  if (GetKeyState(VK_RWIN) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_RWIN;
  }
  if (GetKeyState(VK_APPS) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_MENU;
  }
  if (GetKeyState(VK_SCROLL) & 0x01)
  {
    iKbdState |= HB_GTI_KBD_SCROLOCK;
  }
  if (GetKeyState(VK_NUMLOCK) & 0x01)
  {
    iKbdState |= HB_GTI_KBD_NUMLOCK;
  }
  if (GetKeyState(VK_CAPITAL) & 0x01)
  {
    iKbdState |= HB_GTI_KBD_CAPSLOCK;
  }
  if (GetKeyState(VK_INSERT) & 0x01)
  {
    iKbdState |= HB_GTI_KBD_INSERT;
  }
  if (GetKeyState(VK_LSHIFT) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_LSHIFT;
  }
  if (GetKeyState(VK_RSHIFT) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_RSHIFT;
  }
  if (GetKeyState(VK_LCONTROL) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_LCTRL;
  }
  if (GetKeyState(VK_RCONTROL) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_RCTRL;
  }
  if (GetKeyState(VK_LMENU) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_LALT;
  }
  if (GetKeyState(VK_RMENU) & 0x80)
  {
    iKbdState |= HB_GTI_KBD_RALT;
  }

  return iKbdState;
}

/* *********************************************************************** */

static void hb_gt_win_xSetCursorPos(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorPos()"));
#endif

  s_csbi.dwCursorPosition.Y = static_cast<short>(s_iCurRow);
  s_csbi.dwCursorPosition.X = static_cast<short>(s_iCurCol);
  SetConsoleCursorPosition(s_HOutput, s_csbi.dwCursorPosition);
}

/* *********************************************************************** */

static void hb_gt_win_xSetCursorStyle(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xSetCursorStyle()"));
#endif

  CONSOLE_CURSOR_INFO cci;

  switch (s_iCursorStyle)
  {
  case SC_NONE:
    cci.bVisible = FALSE;
    cci.dwSize = 13;
    break;

  case SC_INSERT:
    cci.bVisible = TRUE;
    cci.dwSize = 50;
    break;

  case SC_SPECIAL1:
    cci.bVisible = TRUE;
    cci.dwSize = 99;
    break;

  case SC_SPECIAL2:
    cci.bVisible = TRUE;
    cci.dwSize = 66;
    /* In their infinite wisdom, MS doesn't support cursors that
       don't start at the bottom of the cell */
    break;

  case SC_NORMAL:
  default:
    cci.bVisible = TRUE;
    cci.dwSize = 13;
    break;
  }
  s_iOldCurStyle = s_iCursorStyle;
  SetConsoleCursorInfo(s_HOutput, &cci);
}

/* *********************************************************************** */

static void hb_gt_win_xScreenUpdate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xScreenUpdate()"));
#endif

  if (s_pCharInfoScreen)
  {
    if (s_iUpdtTop <= s_iUpdtBottom)
    {
      COORD coSize;
      coSize.Y = _GetScreenHeight();
      coSize.X = _GetScreenWidth();
      COORD coDest;
      coDest.Y = static_cast<short>(s_iUpdtTop);
      coDest.X = static_cast<short>(s_iUpdtLeft);
      SMALL_RECT srWin;
      srWin.Top = static_cast<short>(s_iUpdtTop);
      srWin.Left = static_cast<short>(s_iUpdtLeft);
      srWin.Bottom = static_cast<short>(s_iUpdtBottom);
      srWin.Right = static_cast<short>(s_iUpdtRight);

      s_iUpdtTop = _GetScreenHeight();
      s_iUpdtLeft = _GetScreenWidth();
      s_iUpdtBottom = s_iUpdtRight = 0;

      WriteConsoleOutput(s_HOutput,         /* output handle */
                         s_pCharInfoScreen, /* data to write */
                         coSize,            /* col/row size of source buffer */
                         coDest,            /* upper-left cell to write data from in src */
                         &srWin);           /* screen buffer rect to write data to */
    }

    if (s_iOldCurStyle != s_iCursorStyle)
    {
      hb_gt_win_xSetCursorStyle();
    }

    if (s_iCursorStyle != SC_NONE && (s_csbi.dwCursorPosition.Y != s_iCurRow || s_csbi.dwCursorPosition.X != s_iCurCol))
    {
      hb_gt_win_xSetCursorPos();
    }
  }
}

/* *********************************************************************** */

static void hb_gt_win_xUpdtSet(int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xUpdtSet(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight));
#endif

  if (iTop < s_iUpdtTop)
  {
    s_iUpdtTop = iTop;
  }
  if (iLeft < s_iUpdtLeft)
  {
    s_iUpdtLeft = iLeft;
  }
  if (iBottom > s_iUpdtBottom)
  {
    s_iUpdtBottom = HB_MIN(iBottom, static_cast<int>(_GetScreenHeight()) - 1);
  }
  if (iRight > s_iUpdtRight)
  {
    s_iUpdtRight = HB_MIN(iRight, static_cast<int>(_GetScreenWidth()) - 1);
  }
}

/* *********************************************************************** */

static BOOL WINAPI hb_gt_win_CtrlHandler(DWORD dwCtrlType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_CtrlHandler(%lu)", static_cast<HB_ULONG>(dwCtrlType)));
#endif

  BOOL bHandled;

  switch (dwCtrlType)
  {
  case CTRL_C_EVENT:
    bHandled = FALSE;
    break;

  case CTRL_CLOSE_EVENT:
  case CTRL_BREAK_EVENT:
    if (!s_fSuspend)
    {
      s_fBreak = true;
    }
    bHandled = TRUE;
    break;

  case CTRL_LOGOFF_EVENT:
  case CTRL_SHUTDOWN_EVENT:
  default:
#if 0
         printf(" Event %lu ", static_cast<HB_ULONG>(dwCtrlType));
#endif
    bHandled = FALSE;
    break;
  }

  return bHandled;
}

/* *********************************************************************** */

static void hb_gt_win_xGetScreenContents(PHB_GT pGT, SMALL_RECT *psrWin)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xGetScreenContents(%p,%p)", static_cast<void*>(pGT), static_cast<void*>(psrWin)));
#endif

  int iCol;

#if !defined(UNICODE)
  PHB_CODEPAGE cdp;
  HB_BYTE bxAttr;
#endif

#if !defined(UNICODE)
  bxAttr = 0;
  cdp = HB_GTSELF_CPTERM(pGT);
  if (!cdp)
  {
    cdp = HB_GTSELF_CPBOX(pGT);
    if (cdp)
    {
      bxAttr = HB_GT_ATTR_BOX;
    }
    else
    {
      cdp = HB_GTSELF_HOSTCP(pGT);
    }
  }
#endif

  for (int iRow = psrWin->Top; iRow <= psrWin->Bottom; ++iRow)
  {
    int i = iRow * _GetScreenWidth() + psrWin->Left;
    for (iCol = psrWin->Left; iCol <= psrWin->Right; ++iCol)
    {
#if defined(UNICODE)
      HB_GTSELF_PUTSCRCHAR(pGT, iRow, iCol, static_cast<HB_UCHAR>(s_pCharInfoScreen[i].Attributes), 0,
                           s_pCharInfoScreen[i].Char.UnicodeChar);
#else
      HB_USHORT usChar = hb_cdpGetU16(cdp, static_cast<HB_UCHAR>(s_pCharInfoScreen[i].Char.AsciiChar));
      HB_GTSELF_PUTSCRCHAR(pGT, iRow, iCol, static_cast<HB_UCHAR>(s_pCharInfoScreen[i].Attributes), bxAttr, usChar);
#endif
      ++i;
    }
  }
  HB_GTSELF_COLDAREA(pGT, psrWin->Top, psrWin->Left, psrWin->Bottom, psrWin->Right);
}

/* *********************************************************************** */

static void hb_gt_win_xInitScreenParam(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_xInitScreenParam(%p)", static_cast<void*>(pGT)));
#endif

  if (GetConsoleScreenBufferInfo(s_HOutput, &s_csbi))
  {
    COORD coDest;
    SMALL_RECT srWin;
    HB_SIZE nSize = static_cast<HB_SIZE>(_GetScreenWidth()) * _GetScreenHeight() * sizeof(CHAR_INFO);

    HB_GTSELF_RESIZE(pGT, _GetScreenHeight(), _GetScreenWidth());

    if (s_pCharInfoScreen == nullptr || nSize != s_nScreenBuffSize)
    {
      if (s_pCharInfoScreen)
      {
        hb_xfree(s_pCharInfoScreen);
      }
      s_nScreenBuffSize = nSize;
      s_pCharInfoScreen = static_cast<CHAR_INFO *>(hb_xgrab(s_nScreenBuffSize));
    }

    s_iCurRow = s_csbi.dwCursorPosition.Y;
    s_iCurCol = s_csbi.dwCursorPosition.X;
    s_iUpdtTop = _GetScreenHeight();
    s_iUpdtLeft = _GetScreenWidth();
    s_iUpdtBottom = s_iUpdtRight = 0;

    /*
     * Unfortunately Windows refuse to read to big area :-(
     * (I do not know why) so we cannot read the whole console
     * buffer { 0, 0, s_csbi.dwSize.Y - 1, s_csbi.dwSize.X - 1 }
     * because it reads nothing, [druzus]
     */
#if 0
      srWin.Top    = 0;
      srWin.Left   = 0;
      srWin.Bottom = _GetScreenHeight() - 1;
      srWin.Right  = _GetScreenWidth() - 1;
#else
    srWin.Top = s_csbi.srWindow.Top;
    srWin.Left = s_csbi.srWindow.Left;
    srWin.Bottom = s_csbi.srWindow.Bottom;
    srWin.Right = s_csbi.srWindow.Right;
#endif

    coDest.Y = srWin.Top;
    coDest.X = srWin.Left;

    /* read the screen rectangle into the buffer */
    if (ReadConsoleOutput(s_HOutput,         /* screen handle */
                          s_pCharInfoScreen, /* transfer area */
                          s_csbi.dwSize,     /* size of destination buffer */
                          coDest,            /* upper-left cell to write data to */
                          &srWin))
    { /* screen buffer rectangle to read from */
      hb_gt_win_xGetScreenContents(pGT, &srWin);
    }
    HB_GTSELF_SETPOS(pGT, s_iCurRow, s_iCurCol);
  }
  else if (s_pCharInfoScreen)
  {
    hb_xfree(s_pCharInfoScreen);
    s_pCharInfoScreen = nullptr;
    s_nScreenBuffSize = 0;
  }
}

#if defined(HB_GTWIN_USE_PCONSOLEINFOEX)

static bool hb_gt_win_SetPalette_Vista(bool bSet, COLORREF *colors)
{
  static auto s_fChecked = false;

  using P_SETCONSOLESCREENBUFFERINFOEX = BOOL(WINAPI *)(HANDLE, PCONSOLE_SCREEN_BUFFER_INFOEX);
  using P_GETCONSOLESCREENBUFFERINFOEX = BOOL(WINAPI *)(HANDLE, PCONSOLE_SCREEN_BUFFER_INFOEX);
  static P_GETCONSOLESCREENBUFFERINFOEX s_pGetConsoleScreenBufferInfoEx = nullptr;
  static P_SETCONSOLESCREENBUFFERINFOEX s_pSetConsoleScreenBufferInfoEx = nullptr;

  auto bDone = false;
  int tmp;

  if (!s_fChecked)
  {
    HMODULE hModule = GetModuleHandle(TEXT("kernel32.dll"));
    if (hModule)
    {
      s_pGetConsoleScreenBufferInfoEx = reinterpret_cast<P_GETCONSOLESCREENBUFFERINFOEX>(
          reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESS(hModule, "GetConsoleScreenBufferInfoEx")));
      s_pSetConsoleScreenBufferInfoEx = reinterpret_cast<P_SETCONSOLESCREENBUFFERINFOEX>(
          reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESS(hModule, "SetConsoleScreenBufferInfoEx")));
    }
    s_fChecked = true;
  }

  if (s_pGetConsoleScreenBufferInfoEx)
  {
    CONSOLE_SCREEN_BUFFER_INFOEX info;

    info.cbSize = sizeof(info);
    bDone = s_pGetConsoleScreenBufferInfoEx(s_HOutput, &info) != 0;
    if (bDone)
    {
      if (!bSet)
      {
        for (tmp = 0; tmp < 16; ++tmp)
        {
          colors[tmp] = info.ColorTable[tmp];
        }
      }
      else if (s_pSetConsoleScreenBufferInfoEx)
      {
        if (!s_fResetColors)
        {
          for (tmp = 0; tmp < 16; ++tmp)
          {
            s_colorsOld[tmp] = info.ColorTable[tmp];
          }
          s_fResetColors = true;
        }
        for (tmp = 0; tmp < 16; ++tmp)
        {
          info.ColorTable[tmp] = colors[tmp];
        }

        /* workaround for console window size reduction when structure
         * filled by GetConsoleScreenBufferInfoEx() is passed directly
         * to SetConsoleScreenBufferInfoEx() [druzus]
         */
        info.srWindow.Right++;
        info.srWindow.Bottom++;
        bDone = s_pSetConsoleScreenBufferInfoEx(s_HOutput, &info) != 0;
      }
      else
      {
        bDone = false;
      }
    }
  }

  if (!bSet && !bDone)
  {
    for (tmp = 0; tmp < 16; ++tmp)
    {
      colors[tmp] = s_colorsDef[tmp];
    }
  }

  return bDone;
}

#endif

static bool hb_gt_win_SetPalette(bool bSet, COLORREF *colors)
{
#if defined(HB_GTWIN_USE_PCONSOLEINFOEX)
  return hb_gt_win_SetPalette_Vista(bSet, colors);
#else
  if (!bSet)
  {
    for (auto tmp = 0; tmp < 16; ++tmp)
    {
      colors[tmp] = s_colorsDef[tmp];
    }
  }

  return false;
#endif
}

static HWND hb_getConsoleWindowHandle(void)
{
  HWND hWnd;

  hWnd = GetConsoleWindow();

  return hWnd;
}

static bool hb_gt_win_SetCloseButton(bool bSet, bool bClosable)
{
  auto bOldClosable = true;

  HWND hWnd = hb_getConsoleWindowHandle();

  if (hWnd)
  {
    HMENU hSysMenu = GetSystemMenu(hWnd, FALSE);

    if (hSysMenu)
    {
      bOldClosable = (GetMenuState(hSysMenu, SC_CLOSE, MF_BYCOMMAND) & MFS_GRAYED) == 0;

      if (bSet)
      {
#if defined(HB_GTWIN_USE_SETCONSOLEMENUCLOSE)
        using P_SETCONSOLEMENUCLOSE = BOOL(WINAPI *)(BOOL);

        static auto s_fChecked = false;
        static P_SETCONSOLEMENUCLOSE s_pSetConsoleMenuClose = nullptr;

        if (!s_fChecked)
        {
          HMODULE hModule = GetModuleHandle(TEXT("kernel32.dll"));
          if (hModule)
          {
            s_pSetConsoleMenuClose = reinterpret_cast<P_SETCONSOLEMENUCLOSE>(
                reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESS(hModule, "SetConsoleMenuClose")));
          }
          s_fChecked = true;
        }

        if (s_pSetConsoleMenuClose)
        {
          s_pSetConsoleMenuClose(bClosable);
        }
#endif
        EnableMenuItem(hSysMenu, SC_CLOSE, MF_BYCOMMAND | (bClosable ? MF_ENABLED : MF_GRAYED));
      }
    }
  }

  return bOldClosable;
}

/* *********************************************************************** */

static void hb_gt_win_Init(PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Init(%p,%p,%p,%p)", static_cast<void*>(pGT), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdin)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdout)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStderr))));
#endif

  s_fWin9x = hb_iswin9x();

  /* stdin && stdout && stderr */
  s_hStdIn = hFilenoStdin;
  s_hStdOut = hFilenoStdout;
  s_hStdErr = hFilenoStderr;

  s_fMouseEnable = true;
  s_fBreak = s_fSuspend = s_fSpecialKeyHandling = s_fAltKeyHandling = false;
  s_dwNumRead = s_dwNumIndex = 0;
  s_iOldCurStyle = s_iCursorStyle = SC_NORMAL;

  /* AllocConsole() initializes standard input, standard output,
     and standard error handles for the new console. [jarabal] */

#ifndef HB_NO_ALLOC_CONSOLE
  /*
   * This is a hack for MSYS console. It does not support full screen output
   * so nothing can be seen on the screen and we have to close the MSYS
   * console to be able to allocate the MS-Windows one.
   * Unfortunately I do not know any method to detect the MSYS console
   * so I used this hack with checking OSTYPE environment variable. [druzus]
   */
  {
    TCHAR lpOsType[16];

    lpOsType[0] = lpOsType[HB_SIZEOFARRAY(lpOsType) - 1] = TEXT('\0');
    DWORD dwLen = GetEnvironmentVariable(TEXT("OSTYPE"), lpOsType, HB_SIZEOFARRAY(lpOsType) - 1);
    if (dwLen > 0 && dwLen < HB_SIZEOFARRAY(lpOsType) - 1)
    {
      if (lstrcmp(lpOsType, TEXT("msys")) == 0)
      {
        FreeConsole();
      }
    }
  }

  /* Try to allocate console if we haven't inherited any */
  AllocConsole();
#endif

  if ((s_HInput = GetStdHandle(STD_INPUT_HANDLE)) == INVALID_HANDLE_VALUE)
  {
#ifdef HB_NO_ALLOC_CONSOLE
    /* allocate console only when debugger is linked */
    if (hb_dynsymFind("__DBGENTRY"))
    {
      AllocConsole(); /* It is a Windows app without a console, so we create one */
      s_HInput = GetStdHandle(STD_INPUT_HANDLE);
    }
#endif
    if (s_HInput == INVALID_HANDLE_VALUE)
    {
      hb_errInternal(10001, "Could not allocate console", nullptr, nullptr);
    }
  }

  /* Add Ctrl+Break handler [vszakats] */
  SetConsoleCtrlHandler(hb_gt_win_CtrlHandler, TRUE);

  HB_GTSUPER_INIT(pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr);

  s_HOutput = CreateFile(TEXT("CONOUT$"),                    /* filename    */
                         GENERIC_READ | GENERIC_WRITE,       /* Access flag */
                         FILE_SHARE_READ | FILE_SHARE_WRITE, /* share mode  */
                         nullptr,                            /* security attributes */
                         OPEN_EXISTING,                      /* create mode */
                         0, 0);

  if (s_HOutput == INVALID_HANDLE_VALUE)
  {
    hb_errInternal(10001, "Could not allocate console (output)", nullptr, nullptr);
  }

  s_HInput = CreateFile(TEXT("CONIN$"),                     /* filename    */
                        GENERIC_READ | GENERIC_WRITE,       /* Access flag */
                        FILE_SHARE_READ | FILE_SHARE_WRITE, /* share mode  */
                        nullptr,                            /* security attributes */
                        OPEN_EXISTING,                      /* create mode */
                        0, 0);

  if (s_HInput == INVALID_HANDLE_VALUE)
  {
    hb_errInternal(10001, "Could not allocate console (input)", nullptr, nullptr);
  }

  GetConsoleScreenBufferInfo(s_HOutput, &s_csbi);

  /* save screen info to restore on exit */
  memcpy(&s_origCsbi, &s_csbi, sizeof(s_csbi));

  s_csbi.srWindow.Top = s_csbi.srWindow.Left = 0;
  s_csbi.srWindow.Right = HB_MIN(s_csbi.srWindow.Right, _GetScreenWidth() - 1);
  s_csbi.srWindow.Bottom = HB_MIN(s_csbi.srWindow.Bottom, _GetScreenHeight() - 1);

  SetConsoleWindowInfo(s_HOutput, TRUE, &s_csbi.srWindow);
  SetConsoleScreenBufferSize(s_HOutput, s_csbi.dwSize);

  hb_gt_win_xInitScreenParam(pGT);

  GetConsoleMode(s_HOutput, &s_dwomode);
  GetConsoleMode(s_HInput, &s_dwimode);

  SetConsoleMode(s_HInput, s_fMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000);

  s_fClosable = s_fOldClosable = hb_gt_win_SetCloseButton(false, false);
  s_fResetColors = false;

  HB_GTSELF_SETFLAG(pGT, HB_GTI_REDRAWMAX, 4);

  if (hb_fsIsDevice(hFilenoStdout))
  {
    HB_GTSELF_SETFLAG(pGT, HB_GTI_STDOUTCON, true);
  }
  if (hb_fsIsDevice(hFilenoStderr))
  {
    HB_GTSELF_SETFLAG(pGT, HB_GTI_STDERRCON, true);
  }
}

/* *********************************************************************** */

static void hb_gt_win_Exit(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Exit(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSELF_REFRESH(pGT);

  hb_gt_win_SetCloseButton(true, s_fOldClosable);
  if (s_fResetColors)
  {
    hb_gt_win_SetPalette(true, s_colorsOld);
  }

  if (s_pCharInfoScreen)
  {
    hb_xfree(s_pCharInfoScreen);
    s_pCharInfoScreen = nullptr;
    s_nScreenBuffSize = 0;
  }

  if (s_HOutput != INVALID_HANDLE_VALUE)
  {
    SetConsoleScreenBufferSize(s_HOutput, s_origCsbi.dwSize);

    s_origCsbi.srWindow.Right -= s_origCsbi.srWindow.Left;
    s_origCsbi.srWindow.Bottom -= s_origCsbi.srWindow.Top;
    s_origCsbi.srWindow.Top = s_origCsbi.srWindow.Left = 0;

    SetConsoleWindowInfo(s_HOutput, TRUE, &s_origCsbi.srWindow);

    CloseHandle(s_HOutput);
  }
  /* Remove Ctrl+Break handler */
  SetConsoleCtrlHandler(hb_gt_win_CtrlHandler, FALSE);

  HB_GTSUPER_EXIT(pGT);
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_SetMode(PHB_GT pGT, int iRows, int iCols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_SetMode(%p,%d,%d)", static_cast<void*>(pGT), iRows, iCols));
#endif

  auto fRet = false;

  if (s_HOutput != INVALID_HANDLE_VALUE && iRows > 0 && iCols > 0)
  {
    COORD coBuf = GetLargestConsoleWindowSize(s_HOutput);

    if (iRows > coBuf.Y)
    {
      iRows = coBuf.Y;
    }
    else
    {
      coBuf.Y = static_cast<short>(iRows);
    }

    if (iCols > coBuf.X)
    {
      iCols = coBuf.X;
    }
    else
    {
      coBuf.X = static_cast<short>(iCols);
    }

    /* new console window size and scroll position */
    SMALL_RECT srWin;
    srWin.Top = srWin.Left = 0;
    srWin.Bottom = static_cast<short>(iRows - 1);
    srWin.Right = static_cast<short>(iCols - 1);

    if (static_cast<int>(_GetScreenWidth()) >= iCols && static_cast<int>(_GetScreenHeight()) >= iRows)
    {
      /* the new dimensions do not exceed the current buffer dimensions so
       * we can safely resize the console window first, then the buffer
       */
      if (SetConsoleWindowInfo(s_HOutput, TRUE, &srWin))
      {
        SetConsoleScreenBufferSize(s_HOutput, coBuf);
        fRet = true;
      }
    }
    else if (static_cast<int>(_GetScreenWidth()) <= iCols && static_cast<int>(_GetScreenHeight()) <= iRows)
    {
      /* none of the current buffer dimensions is larger then the
       * new dimensions so we can safely enlarge the buffer to new
       * dimensions then adjust the console window dimensions
       */
      if (SetConsoleScreenBufferSize(s_HOutput, coBuf))
      {
        SetConsoleWindowInfo(s_HOutput, TRUE, &srWin);
        fRet = true;
      }
    }
    else
    {
      /* one of the new dimensions is smaller and second larger then the
       * current buffer dimensions. Windows API needs to keep the buffer
       * dimensions not smaller then console window size and there is
       * no single API call which allow to change both buffer and console
       * window dimensions. It means that we have to resize one of the
       * above objects in two steps. We can temporary enlarge the buffer
       * dimensions or reduce the console window dimensions.
       * To reduce the possibility that we will exploit some WIN API
       * limits for the maximum buffer size instead of enlarging it we
       * decrease the one of console window dimensions which is larger
       * then the corresponding new one.
       */
      if (static_cast<int>(_GetScreenWidth()) < iCols)
      {
        srWin.Right = static_cast<short>(_GetScreenWidth() - 1);
      }
      else
      {
        srWin.Bottom = static_cast<short>(_GetScreenHeight() - 1);
      }
      if (SetConsoleWindowInfo(s_HOutput, TRUE, &srWin))
      {
        /* now we can safely set the new buffer dimensions because
         * none of them is smaller then corresponding dimensions of
         * just reduced console window and then we set final console
         * window size.
         */
        if (SetConsoleScreenBufferSize(s_HOutput, coBuf))
        {
          srWin.Bottom = static_cast<short>(iRows - 1);
          srWin.Right = static_cast<short>(iCols - 1);
          SetConsoleWindowInfo(s_HOutput, TRUE, &srWin);
        }
        fRet = true;
      }
    }

    if (fRet)
    {
      hb_gt_win_xInitScreenParam(pGT);
    }
  }

  return fRet;
}

/* *********************************************************************** */

static const char *hb_gt_win_Version(PHB_GT pGT, int iType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Version(%p,%d)", static_cast<void*>(pGT), iType));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (iType == 0)
  {
    return HB_GT_DRVNAME(HB_GT_NAME);
  }

  return "Harbour++ Terminal: Windows native console";
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_PostExt(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_PostExt(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSUPER_POSTEXT(pGT);
  if (s_pCharInfoScreen)
  {
    hb_gt_win_xInitScreenParam(pGT);
  }
  return true;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_Suspend(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Suspend(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (s_pCharInfoScreen)
  {
    SetConsoleMode(s_HOutput, s_dwomode);
    SetConsoleMode(s_HInput, s_dwimode);
  }
  s_fSuspend = true;
  return true;
}

static HB_BOOL hb_gt_win_Resume(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Resume(%p)", static_cast<void*>(pGT)));
#endif

  if (s_pCharInfoScreen)
  {
    SetConsoleCtrlHandler(hb_gt_win_CtrlHandler, TRUE);
    SetConsoleMode(s_HOutput, s_dwomode);
    SetConsoleMode(s_HInput, s_fMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000);
    hb_gt_win_xInitScreenParam(pGT);
    hb_gt_win_xSetCursorStyle();
  }
  s_fSuspend = false;
  return true;
}

/* *********************************************************************** */

static int Handle_Alt_Key(INPUT_RECORD *pInRec, bool *pAltIsDown, int *pAltVal)
{
  int iVal = 0;

  switch ((pInRec->Event.KeyEvent.dwControlKeyState & ENHANCED_KEY) == 0 ? pInRec->Event.KeyEvent.wVirtualScanCode : 0)
  {
  case 0x49:
    ++iVal; /* fallthrough */ /* 9 */
  case 0x48:
    ++iVal; /* fallthrough */ /* 8 */
  case 0x47:
    ++iVal; /* fallthrough */ /* 7 */
  case 0x4d:
    ++iVal; /* fallthrough */ /* 6 */
  case 0x4c:
    ++iVal; /* fallthrough */ /* 5 */
  case 0x4b:
    ++iVal; /* fallthrough */ /* 4 */
  case 0x51:
    ++iVal; /* fallthrough */ /* 3 */
  case 0x50:
    ++iVal; /* fallthrough */ /* 2 */
  case 0x4f:
    ++iVal; /* fallthrough */ /* 1 */
  case 0x52:                  /* 0 */
    if (pInRec->Event.KeyEvent.bKeyDown)
    {
      *pAltVal = *pAltVal * 10 + iVal;
    }
    iVal = 0;
    break;
  case 0x38: /* Alt */
    if (pInRec->Event.KeyEvent.bKeyDown)
    {
      break;
    }
    else if (pInRec->Event.KeyEvent.dwControlKeyState & 0x04000000)
    {
#if defined(UNICODE)
      iVal = *pAltVal & 0xFFFF;
#else
      iVal = *pAltVal & 0xFF;
#endif
    }
    /* fallthrough */
  default:
    *pAltIsDown = false;
    break;
  }
  return iVal;
}

static int SpecialHandling(WORD wScan, int iKey, bool fShifted)
{
  int iStd, iShift;

  switch (wScan)
  {
  case 2:
    iStd = '1';
    iShift = '!';
    break;
  case 3:
    iStd = '2';
    iShift = '@';
    break;
  case 4:
    iStd = '3';
    iShift = '#';
    break;
  case 5:
    iStd = '4';
    iShift = '$';
    break;
  case 6:
    iStd = '5';
    iShift = '%';
    break;
  case 7:
    iStd = '6';
    iShift = '^';
    break;
  case 8:
    iStd = '7';
    iShift = '&';
    break;
  case 9:
    iStd = '8';
    iShift = '*';
    break;
  case 10:
    iStd = '9';
    iShift = '(';
    break;
  case 11:
    iStd = '0';
    iShift = ')';
    break;
  case 12:
    iStd = '-';
    iShift = '_';
    break;
  case 13:
    iStd = '=';
    iShift = '+';
    break;
  case 26:
    iStd = '[';
    iShift = '{';
    break;
  case 27:
    iStd = ']';
    iShift = '}';
    break;
  case 39:
    iStd = ';';
    iShift = ':';
    break;
  case 40:
    iStd = '\'';
    iShift = '"';
    break;
  case 41:
    iStd = '`';
    iShift = '~';
    break;
  case 43:
    iStd = '\\';
    iShift = '|';
    break;
  case 51:
    iStd = ',';
    iShift = '<';
    break;
  case 52:
    iStd = '.';
    iShift = '>';
    break;
  case 53:
    iStd = '/';
    iShift = '?';
    break;
  default:
    iStd = iShift = 0;
    break;
  }

  if (iStd != 0 && iKey == (fShifted ? iStd : iShift))
  {
    iKey = fShifted ? iShift : iStd;
  }

  return iKey;
}

static int hb_gt_win_ReadKey(PHB_GT pGT, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_ReadKey(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  HB_SYMBOL_UNUSED(iEventMask);

  int iKey = 0;

  /* First check for Ctrl+Break, which is handled by gtwin.c */
  if (s_fBreak)
  {
    /* Reset the global Ctrl+Break flag */
    s_fBreak = false;
    iKey = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
    /* Check for events only when the event buffer is exhausted. */
  }
  else if (s_dwNumIndex >= s_dwNumRead)
  {
    /* Check for keyboard input */

    s_dwNumRead = 0;
    GetNumberOfConsoleInputEvents(s_HInput, &s_dwNumRead);

    if (s_dwNumRead)
    {
#if defined(UNICODE)
      /* Workaround for UNICOWS bug:
            https://web.archive.org/web/blogs.msdn.com/michkap/archive/2007/01/13/1460724.aspx
         [vszakats] */

      if (s_fWin9x)
      {
        for (DWORD tmp = 0; tmp < INPUT_BUFFER_LEN; ++tmp)
        {
          s_irBuffer[tmp].EventType = 0xFFFF;
        }
      }
#endif

      /* Read keyboard input */
      ReadConsoleInput(s_HInput,         /* input buffer handle    */
                       s_irBuffer,       /* buffer to read into    */
                       INPUT_BUFFER_LEN, /* size of read buffer    */
                       &s_dwNumRead);    /* number of records read */
      /* Set up to process the first input event */
      s_dwNumIndex = 0;

#if defined(UNICODE)
      if (s_fWin9x)
      {
        for (DWORD tmp = 0; tmp < s_dwNumRead; ++tmp)
        {
          if (s_irBuffer[tmp].EventType == 0xFFFF)
          {
            s_irBuffer[tmp].EventType = KEY_EVENT;
          }
        }
      }
#endif

#if defined(_TRACE) || defined(_TRACE_KEYPRESS)
      {
        for (DWORD tmp = 0; tmp < s_dwNumRead; ++tmp)
        {
          INPUT_RECORD *pInRec = &s_irBuffer[tmp];
          if (pInRec->EventType == KEY_EVENT)
          {
#ifndef _TRACE
            switch (pInRec->Event.KeyEvent.wVirtualScanCode)
            {
            case 0x38: /* ALT */
            case 0x1d: /* CTRL */
            case 0x2a: /* LSHIFT */
            case 0x36: /* RSHIFT */
              /* ignore control keys */
              continue;
            default:
              if (!pInRec->Event.KeyEvent.bKeyDown)
              {
                continue;
              }
            }
#endif
            printf("KEY_EVENT "
                   "key=0x%04x "
                   "scan=0x%04x "
                   "state=0x%04x "
                   "uchar=%d "
                   "repeat=%d\n",
                   static_cast<int>(pInRec->Event.KeyEvent.wVirtualKeyCode),   /* VK_* key code */
                   static_cast<int>(pInRec->Event.KeyEvent.wVirtualScanCode),  /* scan code */
                   static_cast<int>(pInRec->Event.KeyEvent.dwControlKeyState), /* state */
                   static_cast<int>(pInRec->Event.KeyEvent.uChar.UnicodeChar), /* char */
                   static_cast<int>(pInRec->Event.KeyEvent.wRepeatCount));     /* repeat */
          }
#ifdef _TRACE
          else if (pInRec->EventType == MOUSE_EVENT)
          {
            printf("MOUSE_EVENT "
                   "buttonState=0x%02x "
                   "eventFlags=0x%02x "
                   "ctrlKeys=0x%04x "
                   "posXY=(%d,%d)\n",
                   static_cast<int>(pInRec->Event.MouseEvent.dwButtonState),
                   static_cast<int>(pInRec->Event.MouseEvent.dwEventFlags),
                   static_cast<int>(pInRec->Event.MouseEvent.dwControlKeyState),
                   static_cast<int>(pInRec->Event.MouseEvent.dwMousePosition.X),
                   static_cast<int>(pInRec->Event.MouseEvent.dwMousePosition.Y));
          }
          else if (pInRec->EventType == WINDOW_BUFFER_SIZE_EVENT)
          {
            printf("WINDOW_BUFFER_SIZE_EVENT x=%d, y=%d\n", pInRec->Event.WindowBufferSizeEvent.dwSize.X,
                   pInRec->Event.WindowBufferSizeEvent.dwSize.Y);
          }
          else if (pInRec->EventType == FOCUS_EVENT)
          {
            printf("FOCUS_EVENT bSetFocus=%d\n", pInRec->Event.FocusEvent.bSetFocus);
          }
          else if (pInRec->EventType == MENU_EVENT)
          {
            printf("MENU_EVENT commandId=%d\n", pInRec->Event.MenuEvent.dwCommandId);
          }
          else
          {
            printf("UNKNOWN_EVENT %d\n", pInRec->EventType);
          }
#endif
        }
      }
#endif
    }
  }

  /* Only process one keyboard event at a time. */
  if (iKey == 0 && s_dwNumIndex < s_dwNumRead)
  {
    INPUT_RECORD *pInRec = &s_irBuffer[s_dwNumIndex];
    auto fPop = true;

    if (pInRec->EventType == KEY_EVENT)
    {
      /* Save the keyboard state and ASCII, scan, key code */
      WORD wScan = pInRec->Event.KeyEvent.wVirtualScanCode;
      WORD wVKey = pInRec->Event.KeyEvent.wVirtualKeyCode;
      DWORD dwState = pInRec->Event.KeyEvent.dwControlKeyState;
      int iFlags = hb_gt_win_keyFlags(dwState);
      int iChar = 0;

      if (pInRec->Event.KeyEvent.wRepeatCount-- > 1)
      {
        fPop = false;
      }

      if (s_fAltKeyHandling)
      {
        if (s_fAltIsDown)
        {
          iChar = Handle_Alt_Key(pInRec, &s_fAltIsDown, &s_iAltVal);
        }
        else if (wScan == 0x38 /* Alt */ && pInRec->Event.KeyEvent.bKeyDown && (dwState & NUMLOCK_ON) == 0)
        {
          s_fAltIsDown = true;
          s_iAltVal = 0;
        }
      }

      if (iChar != 0 || s_fAltIsDown)
      {
        /* Our own routine to process ALT + KeyPad NUMs */
      }
      else if (pInRec->Event.KeyEvent.bKeyDown)
      {
#if defined(UNICODE)
        iChar = pInRec->Event.KeyEvent.uChar.UnicodeChar;
#else
        iChar = static_cast<HB_UCHAR>(pInRec->Event.KeyEvent.uChar.AsciiChar);
#endif

        /*
         * Under Win9x, upper row keys are affected by caps-lock
         * and should not be.  There are 2 solutions - the first
         * is to enable the calling of SpecialHandling below - which
         * will only be activated under Win9x (Preferably under user
         * control, since they know if their keyboard isn't working), or
         * just enable KeyB handling in config.sys, and do not enable the
         * following call.

         * 2004-11-26 Vicente Guerra
         * (With some clarification by Paul Tucker)
         * If making this fix the default under Win98, then it doesn't
         * work for non-US keyboards.  (The default has now been changed)
         * I tried to replicate the problem under Win98SE (Spanish),
         * but it works fine. I hope someone could tell me how the
         * problem appears, for try to fix it.

         * "Microsoft has confirmed this to be a bug in the Microsoft
         * products " Windows 95 & Windows 98 (According to MSDN)
         *
         */

        if (s_fSpecialKeyHandling && (dwState & CAPSLOCK_ON))
        {
          iChar = SpecialHandling(wScan, iChar, (dwState & SHIFT_PRESSED) != 0);
        }

        switch (wVKey)
        {
        case VK_BACK:
          iKey = HB_KX_BS;
          break;
        case VK_TAB:
          iKey = HB_KX_TAB;
          break;
        case VK_RETURN:
          iKey = HB_KX_ENTER;
          if ((dwState & ENHANCED_KEY) != 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_ESCAPE:
          iKey = HB_KX_ESC;
          break;
        case VK_PRIOR:
          iKey = HB_KX_PGUP;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_NEXT:
          iKey = HB_KX_PGDN;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_END:
          iKey = HB_KX_END;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_HOME:
          iKey = HB_KX_HOME;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_LEFT:
          iKey = HB_KX_LEFT;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_UP:
          iKey = HB_KX_UP;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_RIGHT:
          iKey = HB_KX_RIGHT;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_DOWN:
          iKey = HB_KX_DOWN;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_INSERT:
          iKey = HB_KX_INS;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_DELETE:
          iKey = HB_KX_DEL;
          if ((dwState & ENHANCED_KEY) == 0)
          {
            iFlags |= HB_KF_KEYPAD;
          }
          break;

        case VK_F1:
          iKey = HB_KX_F1;
          break;
        case VK_F2:
          iKey = HB_KX_F2;
          break;
        case VK_F3:
          iKey = HB_KX_F3;
          break;
        case VK_F4:
          iKey = HB_KX_F4;
          break;
        case VK_F5:
          iKey = HB_KX_F5;
          break;
        case VK_F6:
          iKey = HB_KX_F6;
          break;
        case VK_F7:
          iKey = HB_KX_F7;
          break;
        case VK_F8:
          iKey = HB_KX_F8;
          break;
        case VK_F9:
          iKey = HB_KX_F9;
          break;
        case VK_F10:
          iKey = HB_KX_F10;
          break;
        case VK_F11:
          iKey = HB_KX_F11;
          break;
        case VK_F12:
          iKey = HB_KX_F12;
          break;

        case VK_SNAPSHOT:
          iKey = HB_KX_PRTSCR;
          break;
        case VK_CANCEL:
          if ((dwState & ENHANCED_KEY) == 0)
          {
            break;
          }
          iFlags |= HB_KF_CTRL;
          /* fallthrough */
        case VK_PAUSE:
          iKey = HB_KX_PAUSE;
          break;

        case VK_CLEAR:
          iKey = HB_KX_CENTER;
          iFlags |= HB_KF_KEYPAD;
          break;

        case VK_NUMPAD0:
        case VK_NUMPAD1:
        case VK_NUMPAD2:
        case VK_NUMPAD3:
        case VK_NUMPAD4:
        case VK_NUMPAD5:
        case VK_NUMPAD6:
        case VK_NUMPAD7:
        case VK_NUMPAD8:
        case VK_NUMPAD9:
          if (iFlags == HB_KF_ALT)
          {
            iKey = iFlags = 0; /* for ALT + <ASCII/UNICODE_VALUE_FROM_KEYPAD> */
          }
          else
          {
            if (iFlags & HB_KF_CTRL)
            {
              iKey = static_cast<int>(wVKey) - VK_NUMPAD0 + '0';
            }
            iFlags |= HB_KF_KEYPAD;
          }
          break;
        case VK_DECIMAL:
        case VK_SEPARATOR:
          iFlags |= HB_KF_KEYPAD;
          if (iFlags & HB_KF_CTRL)
          {
            iKey = '.';
          }
          break;

        case VK_DIVIDE:
          iFlags |= HB_KF_KEYPAD;
          if (iFlags & HB_KF_CTRL)
          {
            iKey = '/';
          }
          break;
        case VK_MULTIPLY:
          iFlags |= HB_KF_KEYPAD;
          if (iFlags & HB_KF_CTRL)
          {
            iKey = '*';
          }
          break;
        case VK_SUBTRACT:
          iFlags |= HB_KF_KEYPAD;
          if (iFlags & HB_KF_CTRL)
          {
            iKey = '-';
          }
          break;
        case VK_ADD:
          iFlags |= HB_KF_KEYPAD;
          if (iFlags & HB_KF_CTRL)
          {
            iKey = '+';
          }
          break;
#ifdef VK_OEM_2
        case VK_OEM_2:
          if ((iFlags & HB_KF_CTRL) != 0 && (iFlags & HB_KF_SHIFT) != 0)
          {
            iKey = '?';
          }
          break;
#endif
#ifdef VK_APPS
        case VK_APPS:
          iKey = HB_K_MENU;
          break;
#endif
        default:
          if ((dwState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) ==
              LEFT_ALT_PRESSED)
          {
            switch (wScan)
            {
            case 2:
              iKey = '1';
              break;
            case 3:
              iKey = '2';
              break;
            case 4:
              iKey = '3';
              break;
            case 5:
              iKey = '4';
              break;
            case 6:
              iKey = '5';
              break;
            case 7:
              iKey = '6';
              break;
            case 8:
              iKey = '7';
              break;
            case 9:
              iKey = '8';
              break;
            case 10:
              iKey = '9';
              break;
            case 11:
              iKey = '0';
              break;
            case 13:
              iKey = '=';
              break;
            case 16:
              iKey = 'Q';
              break;
            case 17:
              iKey = 'W';
              break;
            case 18:
              iKey = 'E';
              break;
            case 19:
              iKey = 'R';
              break;
            case 20:
              iKey = 'T';
              break;
            case 21:
              if (iChar != 'Y' && iChar != 'Z' && iChar != 'y' && iChar != 'z')
              {
                iKey = 'Y';
              }
              else
              {
                iKey = iChar;
              }
              break;
            case 22:
              iKey = 'U';
              break;
            case 23:
              iKey = 'I';
              break;
            case 24:
              iKey = 'O';
              break;
            case 25:
              iKey = 'P';
              break;
            case 30:
              iKey = 'A';
              break;
            case 31:
              iKey = 'S';
              break;
            case 32:
              iKey = 'D';
              break;
            case 33:
              iKey = 'F';
              break;
            case 34:
              iKey = 'G';
              break;
            case 35:
              iKey = 'H';
              break;
            case 36:
              iKey = 'J';
              break;
            case 37:
              iKey = 'K';
              break;
            case 38:
              iKey = 'L';
              break;
            case 44:
              if (iChar != 'Y' && iChar != 'Z' && iChar != 'y' && iChar != 'z')
              {
                iKey = 'Z';
              }
              else
              {
                iKey = iChar;
              }
              break;
            case 45:
              iKey = 'X';
              break;
            case 46:
              iKey = 'C';
              break;
            case 47:
              iKey = 'V';
              break;
            case 48:
              iKey = 'B';
              break;
            case 49:
              iKey = 'N';
              break;
            case 50:
              iKey = 'M';
              break;
            }
          }
          break;
        }
      }
      else if (wVKey == VK_MENU && (dwState & NUMLOCK_ON) != 0)
      {
#if defined(UNICODE)
        iChar = pInRec->Event.KeyEvent.uChar.UnicodeChar;
#else
        iChar = static_cast<HB_UCHAR>(pInRec->Event.KeyEvent.uChar.AsciiChar);
#endif
      }

      if (iKey != 0)
      {
        iKey = HB_INKEY_NEW_KEY(iKey, iFlags);
      }
      else if ((iFlags & HB_KF_CTRL) != 0 && (iChar > 0 && iChar < 32))
      {
        iChar += 'A' - 1;
        iKey = HB_INKEY_NEW_KEY(iChar, iFlags);
      }
      else if (iChar != 0)
      {
#if defined(UNICODE)
        if (iChar >= 127)
        {
          iKey = HB_INKEY_NEW_UNICODEF(iChar, iFlags);
        }
#else
        int u = HB_GTSELF_KEYTRANS(pGT, iChar);
        if (u)
        {
          iKey = HB_INKEY_NEW_UNICODEF(u, iFlags);
        }
#endif
        else if (iChar < 127 && (iFlags & (HB_KF_CTRL | HB_KF_ALT)))
        {
          if (iChar >= 32 && (((iFlags & HB_KF_CTRL) != 0 && (iFlags & HB_KF_ALT) != 0) ||
                              (dwState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) == RIGHT_ALT_PRESSED))
          {
            iFlags &= ~(HB_KF_CTRL | HB_KF_ALT);
            iKey = HB_INKEY_NEW_CHARF(iChar, iFlags);
          }
          else
          {
            iKey = HB_INKEY_NEW_KEY(iChar, iFlags);
          }
        }
        else
        {
          iKey = HB_INKEY_NEW_CHARF(iChar, iFlags);
        }
      }
#ifdef _TRACE
      if (iKey != 0)
      {
        printf("hb_gt_ReadKey(): dwState=0x%04x, wVKey0x%04x, wScan0x%04x, iKey=%d\n", static_cast<int>(dwState), wVKey,
               wScan, iKey);
      }
#endif
    }
    else if (pInRec->EventType == FOCUS_EVENT)
    {
      iKey = pInRec->Event.FocusEvent.bSetFocus ? HB_K_GOTFOCUS : HB_K_LOSTFOCUS;
    }
    else if (pInRec->EventType == WINDOW_BUFFER_SIZE_EVENT)
    {
      if (s_pCharInfoScreen)
      {
        hb_gt_win_xInitScreenParam(pGT);
        iKey = HB_K_RESIZE;
      }
    }
    else if (pInRec->EventType == MOUSE_EVENT)
    {
      int iFlags = hb_gt_win_keyFlags(pInRec->Event.MouseEvent.dwControlKeyState);

      /* mouse wheel events use screen based mouse position */
      if (pInRec->Event.MouseEvent.dwEventFlags == MOUSE_HWHEELED)
      {
        /* unsupported */
      }
      else if (pInRec->Event.MouseEvent.dwEventFlags == MOUSE_WHEELED)
      {
        iKey = (pInRec->Event.MouseEvent.dwButtonState & 0xFF000000) ? K_MWBACKWARD : K_MWFORWARD;
        iKey = HB_INKEY_NEW_MKEY(iKey, iFlags);
      }
      else if (s_mouse_col != pInRec->Event.MouseEvent.dwMousePosition.X ||
               s_mouse_row != pInRec->Event.MouseEvent.dwMousePosition.Y)
      {
        s_mouse_col = pInRec->Event.MouseEvent.dwMousePosition.X;
        s_mouse_row = pInRec->Event.MouseEvent.dwMousePosition.Y;
        iKey = HB_INKEY_NEW_MPOS(s_mouse_col, s_mouse_row);
        fPop = pInRec->Event.MouseEvent.dwEventFlags == MOUSE_MOVED;
      }
      else if (pInRec->Event.MouseEvent.dwButtonState & ~s_mouse_buttons & FROM_LEFT_1ST_BUTTON_PRESSED)
      {
        iKey = pInRec->Event.MouseEvent.dwEventFlags == DOUBLE_CLICK ? K_LDBLCLK : K_LBUTTONDOWN;
        iKey = HB_INKEY_NEW_MKEY(iKey, iFlags);
        s_mouse_buttons |= FROM_LEFT_1ST_BUTTON_PRESSED;
      }
      else if (pInRec->Event.MouseEvent.dwButtonState & ~s_mouse_buttons & RIGHTMOST_BUTTON_PRESSED)
      {
        iKey = pInRec->Event.MouseEvent.dwEventFlags == DOUBLE_CLICK ? K_RDBLCLK : K_RBUTTONDOWN;
        iKey = HB_INKEY_NEW_MKEY(iKey, iFlags);
        s_mouse_buttons |= RIGHTMOST_BUTTON_PRESSED;
      }
      else if (pInRec->Event.MouseEvent.dwButtonState & ~s_mouse_buttons & FROM_LEFT_2ND_BUTTON_PRESSED)
      {
        iKey = pInRec->Event.MouseEvent.dwEventFlags == DOUBLE_CLICK ? K_MDBLCLK : K_MBUTTONDOWN;
        iKey = HB_INKEY_NEW_MKEY(iKey, iFlags);
        s_mouse_buttons |= FROM_LEFT_2ND_BUTTON_PRESSED;
      }
      else if (~pInRec->Event.MouseEvent.dwButtonState & s_mouse_buttons & FROM_LEFT_1ST_BUTTON_PRESSED)
      {
        iKey = HB_INKEY_NEW_MKEY(K_LBUTTONUP, iFlags);
        s_mouse_buttons ^= FROM_LEFT_1ST_BUTTON_PRESSED;
      }
      else if (~pInRec->Event.MouseEvent.dwButtonState & s_mouse_buttons & RIGHTMOST_BUTTON_PRESSED)
      {
        iKey = HB_INKEY_NEW_MKEY(K_RBUTTONUP, iFlags);
        s_mouse_buttons ^= RIGHTMOST_BUTTON_PRESSED;
      }
      else if (~pInRec->Event.MouseEvent.dwButtonState & s_mouse_buttons & FROM_LEFT_2ND_BUTTON_PRESSED)
      {
        iKey = HB_INKEY_NEW_MKEY(K_MBUTTONUP, iFlags);
        s_mouse_buttons ^= FROM_LEFT_2ND_BUTTON_PRESSED;
      }
    }

    if (fPop)
    {
      s_dwNumIndex++;
    }
  }

  return iKey;
}

/* *********************************************************************** */

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_win_Tone(PHB_GT pGT, double dFrequency, double dDuration)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Tone(%p,%lf,%lf)", static_cast<void*>(pGT), dFrequency, dDuration));
#endif

  HB_SYMBOL_UNUSED(pGT);
  hb_gt_BaseUnlock(pGT);
  hb_gt_winapi_tone(dFrequency, dDuration);
  hb_gt_BaseLock(pGT);
}

/* *********************************************************************** */

static bool hb_gt_win_IsFullScreen(void)
{
  DWORD dwModeFlags;

  if (GetConsoleDisplayMode(&dwModeFlags))
  {
    if (dwModeFlags & CONSOLE_FULLSCREEN_HARDWARE)
    {
      return true;
    }
  }

  return false;
}

/* *********************************************************************** */

static bool hb_gt_win_FullScreen(HB_BOOL bFullScreen)
{
  if (bFullScreen)
  {
    return SetConsoleDisplayMode(s_HOutput, CONSOLE_FULLSCREEN_MODE, nullptr);
  }
  else
  {
    return !SetConsoleDisplayMode(s_HOutput, CONSOLE_WINDOWED_MODE, nullptr);
  }

  return false;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Info(%p,%d,%p)", static_cast<void*>(pGT), iType, pInfo));
#endif

  switch (iType)
  {
  case HB_GTI_ISFULLSCREEN:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, hb_gt_win_IsFullScreen());
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL)
    {
      bool fNewValue = pInfo->pNewVal->getL();
      if (static_cast<bool>(hb_itemGetL(pInfo->pResult)) != fNewValue)
      {
        hb_gt_win_FullScreen(fNewValue);
      }
    }
    break;

  case HB_GTI_ISSCREENPOS:
  case HB_GTI_KBDSUPPORT:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, true);
    break;

  case HB_GTI_ISUNICODE:
#if defined(UNICODE)
    pInfo->pResult = hb_itemPutL(pInfo->pResult, true);
#else
    pInfo->pResult = hb_itemPutL(pInfo->pResult, false);
#endif
    break;

  case HB_GTI_CODEPAGE:
  {
    UINT uiCodePage = GetConsoleCP();
    UINT uiCodePageNew = hb_itemGetNI(pInfo->pNewVal);
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, uiCodePage);
    if ((hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC) && uiCodePageNew != uiCodePage)
    {
      SetConsoleCP(uiCodePageNew);
      SetConsoleOutputCP(uiCodePageNew);
    }
    break;
  }

  case HB_GTI_WINTITLE:
  {
    TCHAR buff[256];

    DWORD dwLen = GetConsoleTitle(buff, HB_SIZEOFARRAY(buff));
    pInfo->pResult = HB_ITEMPUTSTRLEN(pInfo->pResult, buff, dwLen);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::STRING)
    {
      void *hTitle;
      SetConsoleTitle(HB_ITEMGETSTR(pInfo->pNewVal, &hTitle, nullptr));
      hb_strfree(hTitle);
    }
    break;
  }

  case HB_GTI_CLOSABLE:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, s_fClosable);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL)
    {
      bool fNewValue = pInfo->pNewVal->getL();
      if (fNewValue != s_fClosable)
      {
        hb_gt_win_SetCloseButton(true, fNewValue);
        s_fClosable = fNewValue;
      }
    }
    break;

  case HB_GTI_CLOSEMODE:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, s_fClosable ? 0 : 2);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC)
    {
      auto iVal = pInfo->pNewVal->getNI();
      if (iVal >= 0 && iVal <= 2 && (s_fClosable ? (iVal != 0) : (iVal == 0)))
      {
        s_fClosable = iVal == 0;
        hb_gt_win_SetCloseButton(true, s_fClosable);
      }
    }
    break;

  case HB_GTI_RESIZABLE:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, false);
    break;

  case HB_GTI_RESIZEMODE:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, HB_GTI_RESIZEMODE_ROWS);
    break;

  case HB_GTI_ALTENTER:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, !hb_iswinvista());
    break;

  case HB_GTI_PALETTE:
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC)
    {
      auto iIndex = pInfo->pNewVal->getNI();

      if (iIndex >= 0 && iIndex < 16)
      {
        COLORREF colors[16];
        bool fGet = hb_gt_win_SetPalette(false, colors);

        pInfo->pResult = hb_itemPutNL(pInfo->pResult, colors[iIndex]);

        if (fGet && (hb_itemType(pInfo->pNewVal2) & Harbour::Item::NUMERIC))
        {
          colors[iIndex] = pInfo->pNewVal2->getNL();
          hb_gt_win_SetPalette(true, colors);
        }
      }
    }
    else
    {
      COLORREF colors[16];

      if (!pInfo->pResult)
      {
        pInfo->pResult = hb_itemNew(nullptr);
      }

      hb_gt_win_SetPalette(false, colors);

      hb_arrayNew(pInfo->pResult, 16);
      for (auto i = 0; i < 16; i++)
      {
        hb_arraySetNL(pInfo->pResult, i + 1, colors[i]);
      }

      if (hb_itemType(pInfo->pNewVal) & Harbour::Item::ARRAY)
      {
        if (hb_arrayLen(pInfo->pNewVal) == 16)
        {
          for (auto i = 0; i < 16; i++)
          {
            colors[i] = hb_arrayGetNL(pInfo->pNewVal, i + 1);
          }

          hb_gt_win_SetPalette(true, colors);
        }
      }
    }
    break;

  case HB_GTI_DESKTOPROWS:
  case HB_GTI_DESKTOPHEIGHT:
  case HB_GTI_VIEWMAXHEIGHT:
  {
    COORD coBuf = GetLargestConsoleWindowSize(s_HOutput);
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, coBuf.Y - 1);
    break;
  }
  case HB_GTI_DESKTOPCOLS:
  case HB_GTI_DESKTOPWIDTH:
  case HB_GTI_VIEWMAXWIDTH:
  {
    COORD coBuf = GetLargestConsoleWindowSize(s_HOutput);
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, coBuf.X - 1);
    break;
  }
  case HB_GTI_SCREENHEIGHT:
  case HB_GTI_VIEWPORTHEIGHT:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, s_csbi.srWindow.Bottom - s_csbi.srWindow.Top);
    break;

  case HB_GTI_SCREENWIDTH:
  case HB_GTI_VIEWPORTWIDTH:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, s_csbi.srWindow.Right - s_csbi.srWindow.Left);
    break;

  case HB_GTI_KBDSHIFTS:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, hb_gt_win_getKbdState());
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC)
    {
      hb_gt_winapi_setKbdState(pInfo->pNewVal->getNI());
    }
    break;

  case HB_GTI_KBDSPECIAL:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, s_fSpecialKeyHandling);
    if (s_fWin9x && hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL)
    {
      s_fSpecialKeyHandling = pInfo->pNewVal->getL();
    }
    break;

  case HB_GTI_KBDALT:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, s_fAltKeyHandling);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL)
    {
      s_fAltKeyHandling = pInfo->pNewVal->getL();
    }
    break;

  case HB_GTI_MOUSESTATUS:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, s_fMouseEnable);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL)
    {
      s_fMouseEnable = pInfo->pNewVal->getL();
      SetConsoleMode(s_HInput, s_fMouseEnable ? ENABLE_MOUSE_INPUT : 0x0000);
    }
    break;

  case HB_GTI_CLIPBOARDDATA:
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::STRING)
#if defined(UNICODE)
      hb_gt_winapi_setClipboard(CF_UNICODETEXT, pInfo->pNewVal);
#else
      hb_gt_winapi_setClipboard(CF_OEMTEXT, pInfo->pNewVal);
#endif
    else
    {
      if (pInfo->pResult == nullptr)
      {
        pInfo->pResult = hb_itemNew(nullptr);
      }
#if defined(UNICODE)
      hb_gt_winapi_getClipboard(CF_UNICODETEXT, pInfo->pResult);
#else
      hb_gt_winapi_getClipboard(CF_OEMTEXT, pInfo->pResult);
#endif
    }
    break;

  case HB_GTI_WINHANDLE:
    pInfo->pResult = hb_itemPutPtr(pInfo->pResult, hb_getConsoleWindowHandle());
    break;

  default:
    return HB_GTSUPER_INFO(pGT, iType, pInfo);
  }

  return true;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_win_mouse_IsPresent(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
  return s_fMouseEnable;
}

static void hb_gt_win_mouse_GetPos(PHB_GT pGT, int *piRow, int *piCol)
{
  HB_SYMBOL_UNUSED(pGT);
  *piRow = s_mouse_row;
  *piCol = s_mouse_col;
}

static void hb_gt_win_mouse_SetPos(PHB_GT pGT, int iRow, int iCol)
{
  HB_SYMBOL_UNUSED(pGT);
  s_mouse_row = iRow;
  s_mouse_col = iCol;
}

static HB_BOOL hb_gt_win_mouse_ButtonState(PHB_GT pGT, int iButton)
{
  HB_SYMBOL_UNUSED(pGT);

  auto fReturn = false;

  if (iButton == 0)
  { /* TODO: switch */
    fReturn = (GetKeyState(VK_LBUTTON) & 0x8000) != 0;
  }
  else if (iButton == 1)
  {
    fReturn = (GetKeyState(VK_RBUTTON) & 0x8000) != 0;
  }
  else if (iButton == 2)
  {
    fReturn = (GetKeyState(VK_MBUTTON) & 0x8000) != 0;
  }

  return fReturn;
}

static int hb_gt_win_mouse_CountButton(PHB_GT pGT)
{
  DWORD dwCount = 0;
  HB_SYMBOL_UNUSED(pGT);
  GetNumberOfConsoleMouseButtons(&dwCount);
  return static_cast<int>(dwCount);
}

/* *********************************************************************** */

static void hb_gt_win_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Redraw(%p,%d,%d,%d)", static_cast<void*>(pGT), iRow, iCol, iSize));
#endif

  if (iSize > 0 && s_pCharInfoScreen && iRow < static_cast<int>(_GetScreenHeight()) &&
      iCol < static_cast<int>(_GetScreenWidth()))
  {
    int iColor;
    HB_BYTE bAttr;
    int iFirst = iCol;
    int i = (iRow * _GetScreenWidth() + iCol);

    while (iSize-- > 0)
    {
#if defined(UNICODE)
      HB_USHORT usChar;
      if (!HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol++, &iColor, &bAttr, &usChar))
      {
        break;
      }
      s_pCharInfoScreen[i].Char.UnicodeChar = hb_cdpGetU16Ctrl(usChar);
#else
      HB_UCHAR uc;
      if (!HB_GTSELF_GETSCRUC(pGT, iRow, iCol++, &iColor, &bAttr, &uc, true))
      {
        break;
      }
      s_pCharInfoScreen[i].Char.AsciiChar = static_cast<char>(uc);
#endif
      s_pCharInfoScreen[i].Attributes = static_cast<WORD>(iColor & 0xFF);
      ++i;
    }

    hb_gt_win_xUpdtSet(iRow, iFirst, iRow, iCol - 1);
  }
}

/* *********************************************************************** */

static void hb_gt_win_Refresh(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_win_Refresh(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSUPER_REFRESH(pGT);
  if (s_pCharInfoScreen)
  {
    int iRow, iCol, iStyle;

    HB_GTSELF_GETSCRCURSOR(pGT, &iRow, &iCol, &iStyle);

    s_iCurRow = iRow;
    s_iCurCol = iCol;

    if (iRow < 0 || iCol < 0 || iRow >= static_cast<int>(_GetScreenHeight()) ||
        iCol >= static_cast<int>(_GetScreenWidth()))
    {
      s_iCursorStyle = SC_NONE;
    }
    else
    {
      s_iCursorStyle = iStyle;
    }

    hb_gt_win_xScreenUpdate();
  }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", static_cast<void*>(pFuncTable)));
#endif

  pFuncTable->Init = hb_gt_win_Init;
  pFuncTable->Exit = hb_gt_win_Exit;
  pFuncTable->SetMode = hb_gt_win_SetMode;
  pFuncTable->Redraw = hb_gt_win_Redraw;
  pFuncTable->Refresh = hb_gt_win_Refresh;
  pFuncTable->Version = hb_gt_win_Version;
  pFuncTable->PostExt = hb_gt_win_PostExt;
  pFuncTable->Suspend = hb_gt_win_Suspend;
  pFuncTable->Resume = hb_gt_win_Resume;
  pFuncTable->Tone = hb_gt_win_Tone;
  pFuncTable->Info = hb_gt_win_Info;
  pFuncTable->ReadKey = hb_gt_win_ReadKey;

  pFuncTable->MouseIsPresent = hb_gt_win_mouse_IsPresent;
  pFuncTable->MouseGetPos = hb_gt_win_mouse_GetPos;
  pFuncTable->MouseSetPos = hb_gt_win_mouse_SetPos;
  pFuncTable->MouseButtonState = hb_gt_win_mouse_ButtonState;
  pFuncTable->MouseCountButton = hb_gt_win_mouse_CountButton;

  return true;
}

/* *********************************************************************** */

#include "hbgtreg.hpp"

/* *********************************************************************** */

/*
TODO: remover código relacionado com Win9x
*/
