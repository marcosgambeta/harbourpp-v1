//
// Video subsystem for ANSI terminals
//
// Copyright 2000 David G. Holm <dholm@jsd-llc.com>
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

// This module is partially based on VIDMGR by Andrew Clarke and modified for Harbour.

// NOTE: User programs should never call this layer directly!

#define HB_GT_NAME PCA

#include "hbgtcore.hpp"
#include "hbinit.hpp"
#include "hbapifs.hpp"
#include "hbapicdp.hpp"
#include "hbapiitm.hpp"
#include "hbdate.hpp"
#include "hb_io.hpp"
#include "inkey.ch"

#include <string.h>

#if (defined(HB_OS_UNIX) && !defined(HB_OS_VXWORKS))
#if !defined(HB_HAS_TERMIOS)
#define HB_HAS_TERMIOS
#endif
#endif

#if defined(HB_OS_UNIX)
#if defined(HB_HAS_TERMIOS)
#include <unistd.h> // read() function requires it
#include <termios.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#endif
#else
#if defined(HB_OS_WIN)
#include <windows.h>
#endif
#if (defined(_MSC_VER))
#include <conio.h>
#endif
#endif

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
#define HB_GTID_PTR (&s_GtId)

static const char s_szBell[] = {HB_CHAR_BEL, 0};
static const int s_AnsiColors[] = {0, 4, 2, 6, 1, 5, 3, 7};

static HB_FHANDLE s_hFilenoStdin;
static HB_FHANDLE s_hFilenoStdout;
static HB_FHANDLE s_hFilenoStderr;
static int s_iRow;
static int s_iCol;
static int s_iLineBufSize = 0;
static char *s_sLineBuf;
static HB_SIZE s_nTransBufSize = 0;
static char *s_sTransBuf;
static const char *s_szCrLf;
static HB_SIZE s_nCrLf;
static int s_iCurrentSGR, s_iFgColor, s_iBgColor, s_iBold, s_iBlink, s_iAM;
static int s_iCursorStyle;
static auto s_bStdinConsole = false;
static auto s_bStdoutConsole = false;
static auto s_bStderrConsole = false;
static auto s_fDispTrans = false;
static PHB_CODEPAGE s_cdpTerm;
static PHB_CODEPAGE s_cdpHost;

static int s_iOutBufSize = 0;
static int s_iOutBufIndex = 0;
static char *s_sOutBuf;

#if defined(HB_HAS_TERMIOS)

static volatile auto s_fRestTTY = false;
static struct termios s_saved_TIO, s_curr_TIO;

#if defined(SIGTTOU)
static void sig_handler(int iSigNo)
{
  switch (iSigNo)
  {
#ifdef SIGCHLD
  case SIGCHLD:
  {
    int e = errno, stat;
    pid_t pid;
    do
    {
      pid = waitpid(-1, &stat, WNOHANG);
    } while (pid > 0);
    errno = e;
    break;
  }
#endif
#ifdef SIGWINCH
  case SIGWINCH:
#if 0
         s_WinSizeChangeFlag = true;
#endif
    break;
#endif
#ifdef SIGINT
  case SIGINT:
#if 0
         s_InetrruptFlag = true;
#endif
    break;
#endif
#ifdef SIGQUIT
  case SIGQUIT:
#if 0
         s_BreakFlag = true;
#endif
    break;
#endif
#ifdef SIGTSTP
  case SIGTSTP:
#if 0
         s_DebugFlag = true;
#endif
    break;
#endif
#ifdef SIGTTOU
  case SIGTTOU:
    s_fRestTTY = false;
    break;
#endif
  }
}
#endif

#endif

static void hb_gt_pca_termFlush(void)
{
  if (s_iOutBufIndex > 0)
  {
    hb_fsWriteLarge(s_hFilenoStdout, s_sOutBuf, s_iOutBufIndex);
    s_iOutBufIndex = 0;
  }
}

static void hb_gt_pca_termOut(const char *szStr, int iLen)
{
  if (s_iOutBufSize)
  {
    while (iLen > 0)
    {
      int i;
      if (s_iOutBufSize == s_iOutBufIndex)
      {
        hb_gt_pca_termFlush();
      }
      i = s_iOutBufSize - s_iOutBufIndex;
      if (i > iLen)
      {
        i = iLen;
      }
      memcpy(s_sOutBuf + s_iOutBufIndex, szStr, i);
      szStr += i;
      s_iOutBufIndex += i;
      iLen -= i;
    }
  }
}

static void hb_gt_pca_AnsiSetAutoMargin(int iAM)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetAutoMargin(%d)", iAM));
#endif

  if (iAM != s_iAM)
  {
    // disabled until I'll find good PC-ANSI terminal documentation with
    // detail Auto Margin and Auto Line Wrapping description, [druzus]
#if 0
      if( iAM != 0 ) {
         hb_gt_pca_termOut("\x1B[=7h", 5);
      } else {
         hb_gt_pca_termOut("\x1B[=7l", 5);
      }
#endif
    s_iAM = iAM;
  }
}

static void hb_gt_pca_AnsiGetCurPos(int *iRow, int *iCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiGetCurPos(%p, %p)", static_cast<void*>(iRow), static_cast<void*>(iCol)));
#endif

  static auto s_fIsAnswer = true;

  if (s_fIsAnswer && s_bStdinConsole && s_bStdoutConsole)
  {
    char rdbuf[64];
    int i, j, n, d, y, x;

    hb_gt_pca_termOut("\x1B[6n", 4);
    hb_gt_pca_termFlush();

    n = j = x = y = 0;

    // wait up to 2 seconds for answer
    HB_MAXINT timeout = 2000;
    HB_MAXUINT timer = hb_timerInit(timeout);
    for (;;)
    {
      // looking for cursor position in "\033[%d;%dR"
      while (j < n && rdbuf[j] != '\033')
      {
        ++j;
      }
      if (n - j >= 6)
      {
        i = j + 1;
        if (rdbuf[i] == '[')
        {
          y = 0;
          d = ++i;
          while (i < n && rdbuf[i] >= '0' && rdbuf[i] <= '9')
          {
            y = y * 10 + (rdbuf[i++] - '0');
          }
          if (i < n && i > d && rdbuf[i] == ';')
          {
            x = 0;
            d = ++i;
            while (i < n && rdbuf[i] >= '0' && rdbuf[i] <= '9')
            {
              x = x * 10 + (rdbuf[i++] - '0');
            }
            if (i < n && i > d && rdbuf[i] == 'R')
            {
              s_fIsAnswer = true;
              break;
            }
          }
        }
        if (i < n)
        {
          j = i;
          continue;
        }
      }
      if (n == static_cast<int>(sizeof(rdbuf)))
      {
        break;
      }

      if ((timeout = hb_timerTest(timeout, &timer)) == 0)
      {
        break;
      }
      else
      {
#if defined(HB_HAS_TERMIOS)
        if (hb_fsCanRead(s_hFilenoStdin, timeout) <= 0)
        {
          break;
        }
        i = read(s_hFilenoStdin, rdbuf + n, sizeof(rdbuf) - n);
#else
        i = getc(stdin);
        if (i > 0)
        {
          rdbuf[n] = static_cast<char>(i);
          i = 1;
        }
#endif
        if (i <= 0)
        {
          break;
        }
        n += i;
      }
    }

    if (s_fIsAnswer)
    {
      *iRow = y - 1;
      *iCol = x - 1;
    }
    else
    {
      *iRow = *iCol = -1;
    }
  }
}

static void hb_gt_pca_AnsiSetCursorPos(int iRow, int iCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetCursorPos(%d, %d)", iRow, iCol));
#endif

  if (s_iRow != iRow || s_iCol != iCol)
  {
    char buff[16];
    hb_snprintf(buff, sizeof(buff), "\x1B[%d;%dH", iRow + 1, iCol + 1);
    hb_gt_pca_termOut(buff, static_cast<int>(strlen(buff)));
    s_iRow = iRow;
    s_iCol = iCol;
  }
}

static void hb_gt_pca_AnsiSetCursorStyle(int iStyle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetCursorStyle(%d)", iStyle));
#endif

  if (s_iCursorStyle != iStyle)
  {
    hb_gt_pca_termOut((iStyle == SC_NONE ? "\x1B[?25l" : "\x1B[?25h"), 6);
    s_iCursorStyle = iStyle;
  }
}

static void hb_gt_pca_AnsiSetAttributes(int iAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiSetAttributes(%d)", iAttr));
#endif

  if (s_iCurrentSGR != iAttr)
  {
    int i, bg, fg, bold, blink;
    char buff[16];

    i = 2;
    buff[0] = 0x1b;
    buff[1] = '[';

    bg = s_AnsiColors[(iAttr >> 4) & 0x07];
    fg = s_AnsiColors[iAttr & 0x07];
    bold = (iAttr & 0x08) ? 1 : 0;
    blink = (iAttr & 0x80) ? 1 : 0;

    if (s_iCurrentSGR == -1)
    {
      buff[i++] = '0';
      buff[i++] = ';';
      if (bold)
      {
        buff[i++] = '1';
        buff[i++] = ';';
      }
      if (blink)
      {
        buff[i++] = '5';
        buff[i++] = ';';
      }
      buff[i++] = '3';
      buff[i++] = '0' + static_cast<char>(fg);
      buff[i++] = ';';
      buff[i++] = '4';
      buff[i++] = '0' + static_cast<char>(bg);
      buff[i++] = 'm';
      s_iBold = bold;
      s_iBlink = blink;
      s_iFgColor = fg;
      s_iBgColor = bg;
    }
    else
    {
      if (s_iBold != bold)
      {
        if (!bold)
        {
          buff[i++] = '2';
        }
        buff[i++] = '1';
        buff[i++] = ';';
        s_iBold = bold;
      }
      if (s_iBlink != blink)
      {
        if (!blink)
        {
          buff[i++] = '2';
        }
        buff[i++] = '5';
        buff[i++] = ';';
        s_iBlink = blink;
      }
      if (s_iFgColor != fg)
      {
        buff[i++] = '3';
        buff[i++] = '0' + static_cast<char>(fg);
        buff[i++] = ';';
        s_iFgColor = fg;
      }
      if (s_iBgColor != bg)
      {
        buff[i++] = '4';
        buff[i++] = '0' + static_cast<char>(bg);
        buff[i++] = ';';
        s_iBgColor = bg;
      }
      buff[i - 1] = 'm';
    }
    s_iCurrentSGR = iAttr;
    if (i > 2)
    {
      hb_gt_pca_termOut(buff, i);
    }
  }
}

static void hb_gt_pca_AnsiInit(void)
{
  s_iCurrentSGR = s_iRow = s_iCol = s_iCursorStyle = s_iAM = -1;
}

static void hb_gt_pca_AnsiPutStr(int iRow, int iCol, int iColor, const char *szStr, int iLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_AnsiPutStr(%d,%d,%d,%p,%d)", iRow, iCol, iColor, static_cast<const void*>(szStr), iLen));
#endif

  hb_gt_pca_AnsiSetAttributes(static_cast<HB_BYTE>(iColor));
  hb_gt_pca_AnsiSetCursorPos(iRow, iCol);
  hb_gt_pca_AnsiSetAutoMargin(0);
  hb_gt_pca_termOut(szStr, iLen);
  s_iCol += iLen;
}

static void hb_gt_pca_Init(PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout,
                           HB_FHANDLE hFilenoStderr) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Init(%p,%p,%p,%p)", static_cast<void*>(pGT), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdin)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdout)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStderr))));
#endif

  int iRows = 25, iCols = 80;

  s_hFilenoStdin = hFilenoStdin;
  s_hFilenoStdout = hFilenoStdout;
  s_hFilenoStderr = hFilenoStderr;

  s_bStdinConsole = hb_fsIsDevice(s_hFilenoStdin);
  s_bStdoutConsole = hb_fsIsDevice(s_hFilenoStdout);
  s_bStderrConsole = hb_fsIsDevice(s_hFilenoStderr);

  s_cdpTerm = s_cdpHost = nullptr;
  s_fDispTrans = false;

  s_szCrLf = hb_conNewLine();
  s_nCrLf = strlen(s_szCrLf);

  hb_fsSetDevMode(s_hFilenoStdout, FD_BINARY);

  HB_GTSUPER_INIT(pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr);

// SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment
#if defined(HB_HAS_TERMIOS) && defined(SA_NOCLDSTOP)
  s_fRestTTY = false;
  if (s_bStdinConsole)
  {
#if defined(SIGTTOU)
    struct sigaction act, old;

    // if( s_saved_TIO.c_lflag & TOSTOP ) != 0
    sigaction(SIGTTOU, nullptr, &old);
    memcpy(&act, &old, sizeof(struct sigaction));
    act.sa_handler = sig_handler;
    // do not use SA_RESTART - new Linux kernels will repeat the operation
#if defined(SA_ONESHOT)
    act.sa_flags = SA_ONESHOT;
#elif defined(SA_RESETHAND)
    act.sa_flags = SA_RESETHAND;
#else
    act.sa_flags = 0;
#endif
    sigaction(SIGTTOU, &act, 0);
#endif

    s_fRestTTY = true;

    tcgetattr(hFilenoStdin, &s_saved_TIO);
    memcpy(&s_curr_TIO, &s_saved_TIO, sizeof(struct termios));
#if 0
      atexit(restore_input_mode);
#endif
    s_curr_TIO.c_lflag &= ~(ICANON | ECHO);
    s_curr_TIO.c_iflag &= ~ICRNL;
#if 0
      s_curr_TIO.c_cc[VMIN] = 0;
#else
    // workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
    // *buntu) in which select() unconditionally accepts stdin for
    // reading if c_cc[VMIN] = 0 [druzus]
    s_curr_TIO.c_cc[VMIN] = 1;
#endif
    s_curr_TIO.c_cc[VTIME] = 0;
    tcsetattr(hFilenoStdin, TCSAFLUSH, &s_curr_TIO);

#if defined(SIGTTOU)
    act.sa_handler = SIG_DFL;
    sigaction(SIGTTOU, &old, nullptr);
#endif
  }

#ifdef TIOCGWINSZ
  if (s_bStdoutConsole)
  {
    struct winsize win;

    if (ioctl(hFilenoStdout, TIOCGWINSZ, reinterpret_cast<char *>(&win)) != -1)
    {
      iRows = win.ws_row;
      iCols = win.ws_col;
    }
  }
#endif
#endif

  if (s_iOutBufSize == 0)
  {
    s_iOutBufIndex = 0;
    s_iOutBufSize = 16384;
    s_sOutBuf = static_cast<char *>(hb_xgrab(s_iOutBufSize));
  }

  HB_GTSELF_RESIZE(pGT, iRows, iCols);
  HB_GTSELF_SETFLAG(pGT, HB_GTI_STDOUTCON, s_bStdoutConsole);
  HB_GTSELF_SETFLAG(pGT, HB_GTI_STDERRCON, s_bStderrConsole && s_bStdoutConsole);

  hb_gt_pca_AnsiInit();
  hb_gt_pca_AnsiGetCurPos(&s_iRow, &s_iCol);
}

static void hb_gt_pca_Exit(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Exit(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSELF_REFRESH(pGT);
  // set default color
  hb_gt_pca_AnsiSetAttributes(0x07);
  hb_gt_pca_AnsiSetCursorStyle(SC_NORMAL);
  hb_gt_pca_AnsiSetAutoMargin(1);
  hb_gt_pca_termFlush();

  HB_GTSUPER_EXIT(pGT);

#if defined(HB_HAS_TERMIOS)
  if (s_fRestTTY)
  {
    tcsetattr(s_hFilenoStdin, TCSANOW, &s_saved_TIO);
  }
#endif
  if (s_iLineBufSize > 0)
  {
    hb_xfree(s_sLineBuf);
    s_iLineBufSize = 0;
  }
  if (s_nTransBufSize > 0)
  {
    hb_xfree(s_sTransBuf);
    s_nTransBufSize = 0;
  }
  if (s_iOutBufSize > 0)
  {
    hb_xfree(s_sOutBuf);
    s_iOutBufSize = s_iOutBufIndex = 0;
  }
  s_bStdinConsole = s_bStdoutConsole = s_bStderrConsole = false;
}

static int hb_gt_pca_ReadKey(PHB_GT pGT, int iEventMask) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_ReadKey(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iEventMask);

  int ch = 0;

#if defined(HB_HAS_TERMIOS)
  if (hb_fsCanRead(s_hFilenoStdin, 0) > 0)
  {
    HB_BYTE bChar;
    if (hb_fsRead(s_hFilenoStdin, &bChar, 1) == 1)
    {
      ch = bChar;
    }
  }
#elif defined(_MSC_VER)
  if (s_bStdinConsole)
  {
    if (_kbhit())
    {
      ch = _getch();
      if ((ch == 0 || ch == 224) && _kbhit())
      {
        // It was a function key lead-in code, so read the actual
        // function key and then offset it by 256
        ch = _getch();
        if (ch != -1)
        {
          ch += 256;
        }
      }
      ch = hb_gt_dos_keyCodeTranslate(ch, 0, HB_GTSELF_CPIN(pGT));
    }
  }
  else if (!_eof(static_cast<int>(s_hFilenoStdin)))
  {
    HB_BYTE bChar;
    if (_read(static_cast<int>(s_hFilenoStdin), &bChar, 1) == 1)
    {
      ch = bChar;
    }
  }
#elif defined(HB_OS_WIN)
  if (!s_bStdinConsole || WaitForSingleObject(reinterpret_cast<HANDLE>(hb_fsGetOsHandle(s_hFilenoStdin)), 0) == 0x0000)
  {
    HB_BYTE bChar;
    if (hb_fsRead(s_hFilenoStdin, &bChar, 1) == 1)
    {
      ch = bChar;
    }
  }
#else
  {
    int iTODO; // TODO:
  }
#endif

  if (ch)
  {
    int u = HB_GTSELF_KEYTRANS(pGT, ch);
    if (u)
    {
      ch = HB_INKEY_NEW_UNICODE(u);
    }
  }

  return ch;
}

static void hb_gt_pca_Tone(PHB_GT pGT, double dFrequency, double dDuration) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Tone(%p, %lf, %lf)", static_cast<void*>(pGT), dFrequency, dDuration));
#endif

  HB_SYMBOL_UNUSED(dFrequency);

  static double s_dLastSeconds = 0;

  // Output an ASCII BEL character to cause a sound
  // but throttle to max once per second, in case of sound
  // effects prgs calling lots of short tone sequences in
  // succession leading to BEL hell on the terminal

  double dCurrentSeconds = hb_dateSeconds();
  if (dCurrentSeconds < s_dLastSeconds || dCurrentSeconds - s_dLastSeconds > 0.5)
  {
    hb_gt_pca_termOut(s_szBell, 1);
    s_dLastSeconds = dCurrentSeconds;
    hb_gt_pca_termFlush();
  }

  // convert Clipper (DOS) timer tick units to seconds ( x / 18.2 )
  hb_gtSleep(pGT, dDuration / 18.2);
}

static void hb_gt_pca_Bell(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Bell(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
  hb_gt_pca_termOut(s_szBell, 1);
  hb_gt_pca_termFlush();
}

static const char *hb_gt_pca_Version(PHB_GT pGT, int iType) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Version(%p,%d)", static_cast<void*>(pGT), iType));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (iType == 0)
  {
    return HB_GT_DRVNAME(HB_GT_NAME);
  }

  return "Harbour++ Terminal: PC ANSI";
}

static HB_BOOL hb_gt_pca_Suspend(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Suspend(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
#if defined(HB_HAS_TERMIOS)
  if (s_fRestTTY)
  {
    tcsetattr(s_hFilenoStdin, TCSANOW, &s_saved_TIO);
  }
#endif
  // Enable line wrap when cursor set after last column
  hb_gt_pca_AnsiSetAutoMargin(1);
  return true;
}

static HB_BOOL hb_gt_pca_Resume(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Resume(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
#if defined(HB_HAS_TERMIOS)
  if (s_fRestTTY)
  {
    tcsetattr(s_hFilenoStdin, TCSANOW, &s_curr_TIO);
  }
#endif
  hb_gt_pca_AnsiInit();

  return true;
}

static HB_BOOL hb_gt_pca_SetDispCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP,
                                   HB_BOOL fBox) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_SetDispCP(%p,%s,%s,%d)", static_cast<void*>(pGT), pszTermCDP, pszHostCDP, static_cast<int>(fBox)));
#endif

  if (HB_GTSUPER_SETDISPCP(pGT, pszTermCDP, pszHostCDP, fBox))
  {
    s_cdpTerm = HB_GTSELF_TERMCP(pGT);
    s_cdpHost = HB_GTSELF_HOSTCP(pGT);
    s_fDispTrans = s_cdpTerm && s_cdpHost && s_cdpTerm != s_cdpHost;
  }

  return true;
}

static void hb_gt_pca_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Redraw(%p,%d,%d,%d)", static_cast<void*>(pGT), iRow, iCol, iSize));
#endif

  int iColor;
  HB_BYTE bAttr;
  HB_USHORT usChar;
  int iLen = 0, iColor2 = 0;

  while (iSize--)
  {
    if (!HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol + iLen, &iColor, &bAttr, &usChar))
    {
      break;
    }

    if (iLen == 0)
    {
      iColor2 = iColor;
    }
    else if (iColor2 != iColor)
    {
      if (s_fDispTrans)
      {
        HB_SIZE nLen = iLen;
        const char *buffer =
            hb_cdpnDup3(s_sLineBuf, nLen, s_sTransBuf, &nLen, &s_sTransBuf, &s_nTransBufSize, s_cdpHost, s_cdpTerm);
        hb_gt_pca_AnsiPutStr(iRow, iCol, iColor2, buffer, static_cast<int>(nLen));
      }
      else
      {
        hb_gt_pca_AnsiPutStr(iRow, iCol, iColor2, s_sLineBuf, iLen);
      }

      iCol += iLen;
      iLen = 0;
      iColor2 = iColor;
    }
    if (usChar < 32 || usChar == 127)
    {
      usChar = '.';
    }
    s_sLineBuf[iLen++] = static_cast<char>(usChar);
  }
  if (iLen)
  {
    if (s_fDispTrans)
    {
      HB_SIZE nLen = iLen;
      const char *buffer =
          hb_cdpnDup3(s_sLineBuf, nLen, s_sTransBuf, &nLen, &s_sTransBuf, &s_nTransBufSize, s_cdpHost, s_cdpTerm);
      hb_gt_pca_AnsiPutStr(iRow, iCol, iColor2, buffer, static_cast<int>(nLen));
    }
    else
    {
      hb_gt_pca_AnsiPutStr(iRow, iCol, iColor2, s_sLineBuf, iLen);
    }
  }
}

static void hb_gt_pca_Refresh(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Refresh(%p)", static_cast<void*>(pGT)));
#endif

  int iWidth, iHeight, iRow, iCol, iStyle;

  HB_GTSELF_GETSIZE(pGT, &iHeight, &iWidth);

  if (s_iLineBufSize == 0)
  {
    s_sLineBuf = static_cast<char *>(hb_xgrab(iWidth));
    s_iLineBufSize = iWidth;
  }
  else if (s_iLineBufSize != iWidth)
  {
    s_sLineBuf = static_cast<char *>(hb_xrealloc(s_sLineBuf, iWidth));
    s_iLineBufSize = iWidth;
  }

  HB_GTSUPER_REFRESH(pGT);

  HB_GTSELF_GETSCRCURSOR(pGT, &iRow, &iCol, &iStyle);
  if (iStyle != SC_NONE)
  {
    if (iRow >= 0 && iCol >= 0 && iRow < iHeight && iCol < iWidth)
    {
      hb_gt_pca_AnsiSetCursorPos(iRow, iCol);
    }
    else
    {
      iStyle = SC_NONE;
    }
  }
  hb_gt_pca_AnsiSetCursorStyle(iStyle);
  hb_gt_pca_termFlush();
}

static HB_BOOL hb_gt_pca_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_pca_Info(%p,%d,%p)", static_cast<void*>(pGT), iType, static_cast<void*>(pInfo)));
#endif

  switch (iType)
  {
  case HB_GTI_ISSCREENPOS:
  case HB_GTI_KBDSUPPORT:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, true);
    break;

  default:
    return HB_GTSUPER_INFO(pGT, iType, pInfo);
  }

  return true;
}

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", static_cast<void*>(pFuncTable)));
#endif

  pFuncTable->Init = hb_gt_pca_Init;
  pFuncTable->Exit = hb_gt_pca_Exit;
  pFuncTable->Redraw = hb_gt_pca_Redraw;
  pFuncTable->Refresh = hb_gt_pca_Refresh;
  pFuncTable->Version = hb_gt_pca_Version;
  pFuncTable->Suspend = hb_gt_pca_Suspend;
  pFuncTable->Resume = hb_gt_pca_Resume;
  pFuncTable->SetDispCP = hb_gt_pca_SetDispCP;
  pFuncTable->Tone = hb_gt_pca_Tone;
  pFuncTable->Bell = hb_gt_pca_Bell;
  pFuncTable->Info = hb_gt_pca_Info;

  pFuncTable->ReadKey = hb_gt_pca_ReadKey;

  return true;
}

// ***********************************************************************

#include "hbgtreg.hpp"

// ***********************************************************************
