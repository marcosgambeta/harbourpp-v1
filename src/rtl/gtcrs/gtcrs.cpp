//
// Video subsystem based on ncurses screen library.
//
// Copyright 2005 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

// NOTE: User programs should never call this layer directly!

// ***********************************************************************

#include "gtcrs.hpp"

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
#define HB_GTID_PTR (&s_GtId)

static volatile bool s_SignalTable[MAX_SIGNO];
#if defined(SA_NOCLDSTOP) && defined(SA_RESTART) && defined(SIGCHLD)
static volatile auto s_SignalFlag = false;
// this variable should be global and checked in main VM loop
static volatile auto s_BreakFlag = false;
static volatile auto s_InetrruptFlag = false;
#endif
static volatile auto s_WinSizeChangeFlag = false;

static int s_iStdIn, s_iStdOut, s_iStdErr;

struct evtFD
{
  int fd;
  int mode;
  int status;
  int index;
  void *data;
  int (*eventFunc)(int, int, void *);
};

struct mouseEvent
{
  int row, col;
  int buttonstate;
  int lbuttons;
  int flags;
  int lbup_row, lbup_col;
  int lbdn_row, lbdn_col;
  int rbup_row, rbup_col;
  int rbdn_row, rbdn_col;
  int mbup_row, mbup_col;
  int mbdn_row, mbdn_col;
  // to analyze DBLCLK on xterm
  int click_delay;
  struct timeval BL_time;
  struct timeval BR_time;
  struct timeval BM_time;
};

struct _keyTab
{
  int ch;
  int key;
  struct _keyTab *nextCh;
  struct _keyTab *otherCh;
};

using keyTab = _keyTab;

struct InOutBase
{
  int terminal_type;

  keyTab *pKeyTab;
  int key_flag;
  int esc_delay;
  int key_counter;
  int nation_mode;
  unsigned char *in_transtbl;
  unsigned char *out_transtbl;
  unsigned char *nation_transtbl;
  int *charmap;

  int cursor, lcursor;
  int row, col;
  int maxrow, maxcol;
  int is_color;
  unsigned int disp_count;

  char *acsc, *beep, *flash, *civis, *cnorm, *cvvis;

  int mouse_type;
  int mButtons;
  int nTermMouseChars;
  unsigned char cTermMouseBuf[3];
  mouseEvent mLastEvt;
#if defined(HB_HAS_GPM)
  Gpm_Connect Conn;
#endif

  int base_infd;
  int base_outfd;
  int stdoutfd;
  int stderrfd;
  pid_t termpid;
  int lTIOsaved;
  struct termios saved_TIO, curr_TIO;

  unsigned char stdin_buf[STDIN_BUFLEN];
  int stdin_ptr_l;
  int stdin_ptr_r;
  int stdin_inbuf;

  PHB_POLLFD pPollSet;
  evtFD **event_fds;
  int efds_size;
  int efds_no;

  // curses data
  SCREEN *basescr;
  WINDOW *hb_stdscr;
  FILE *basein;
  FILE *baseout;
  chtype std_chmap[256];
  chtype box_chmap[256];
  chtype attr_map[256];
  chtype attr_mask;
};

static InOutBase *s_ioBase = nullptr;

static InOutBase **s_ioBaseTab = nullptr;
static int s_iSize_ioBaseTab = 0;
static int s_iActive_ioBase = -1;

static void set_tmevt(unsigned char *cMBuf, mouseEvent *);
static int getMouseKey(mouseEvent *);
static void destroy_ioBase(InOutBase *ioBase);
static void set_sig_handler(int iSig);

static void curs_wrkaround(void);

static int getClipKey(int nKey)
{
  int nRet = 0;

  if (IS_CLIPKEY(nKey))
  {
    nRet = GET_CLIPKEY(nKey);
  }
  else if (HB_INKEY_ISEXT(nKey))
  {
    nRet = nKey;
  }
  else
  {
    int nFlag = 0;
    int n = GET_KEYMASK(nKey);
    nKey = CLR_KEYMASK(nKey);
    if (n & KEY_SHIFTMASK)
    {
      nFlag |= HB_KF_SHIFT;
    }
    if (n & KEY_CTRLMASK)
    {
      nFlag |= HB_KF_CTRL;
    }
    if (n & KEY_ALTMASK)
    {
      nFlag |= HB_KF_ALT;
    }
    if (n & KEY_KPADMASK)
    {
      nFlag |= HB_KF_KEYPAD;
    }

    if (n & KEY_EXTDMASK)
    {
      nRet = HB_INKEY_NEW_KEY(nKey, nFlag);
    }
    else
    {
      if (nKey > 0 && nKey < 32)
      {
        nFlag |= HB_KF_CTRL;
        nKey += ('A' - 1);
      }
      nRet = HB_INKEY_NEW_KEY(nKey, nFlag);
    }
  }

  return nRet;
}

#if defined(SA_NOCLDSTOP) && defined(SA_RESTART) && defined(SIGCHLD)
#if 1
static void sig_handler(int signo)
{
  int e = errno;

  if (signo < MAX_SIGNO)
  {
    s_SignalTable[signo] = true;
    s_SignalFlag = true;
  }

  switch (signo)
  {
  case SIGCHLD:
  {
    int status;
    pid_t pid;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0)
    {
      ;
    }
    break;
  }
  case SIGWINCH:
    s_WinSizeChangeFlag = true;
    break;
  case SIGINT:
    s_InetrruptFlag = true;
    break;
  case SIGQUIT:
    s_BreakFlag = true;
    break;
  case SIGTSTP:
    break;
  default:
    break;
  }

  errno = e;
}

static void set_signals(void)
{
  int sigs[] = {SIGINT, SIGQUIT, SIGTSTP, SIGWINCH, SIGCHLD, 0};

  s_SignalFlag = false;
  for (auto i = 1; i < MAX_SIGNO; ++i)
  {
    s_SignalTable[i] = false;
  }

  // Ignore SIGPIPEs so they don't kill us.
  signal(SIGPIPE, SIG_IGN);

  for (auto i = 0; sigs[i]; ++i)
  {
    set_sig_handler(sigs[i]);
  }
}

#else
static void sig_handler(int signo)
{
  int e = errno;
  char *pszSig;

  switch (signo)
  {
  case SIGCHLD:
  {
    int status;
    pid_t pid;
    pszSig = "SIGCHLD";
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0)
    {
      ;
    }
    break;
  }
  case SIGWINCH:
    pszSig = "SIGWINCH";
    break;
  case SIGPIPE:
    pszSig = "SIGPIPE";
    break;
  case SIGTERM:
    pszSig = "SIGTERM";
    break;
  case SIGINT:
    pszSig = "SIGINT";
    break;
  case SIGQUIT:
    pszSig = "SIGQUIT";
    break;
  case SIGCONT:
    pszSig = "SIGCONT";
    break;
  case SIGTSTP:
    pszSig = "SIGTSTP";
    break;
  case SIGTTOU:
    pszSig = "SIGTTOU";
    break;
  default:
    pszSig = "other signal";
    break;
  }

  printf("\nreceived signal: %d -> %s\n", signo, pszSig);
  fflush(stdout);

  errno = e;
}

static void set_signals(void)
{
  s_SignalFlag = false;
  for (auto i = 1; i < MAX_SIGNO; ++i)
  {
    s_SignalTable[i] = false;
    set_sig_handler(i);
  }
}
#endif
#else
static void set_signals(void)
{
}
#endif

static void set_sig_handler(int iSig)
{
  // SA_NOCLDSTOP in #if is a hack to detect POSIX compatible environment
#if defined(SA_NOCLDSTOP) && defined(SA_RESTART) && defined(SIGCHLD)
  struct sigaction act;
  sigaction(iSig, 0, &act);
  act.sa_handler = sig_handler;
  act.sa_flags = SA_RESTART | (iSig == SIGCHLD ? SA_NOCLDSTOP : 0);
  sigaction(iSig, &act, 0);
#else
  HB_SYMBOL_UNUSED(iSig);
#endif
}

static bool write_ttyseq(InOutBase *ioBase, const char *seq)
{
  auto success = false;

  if (ioBase->baseout != nullptr)
  {
    size_t seqlen = strlen(seq);
    success = (fwrite(seq, seqlen, 1, ioBase->baseout) == seqlen);
    fflush(ioBase->baseout);
  }
  else
  {
    int seqlen = strlen(seq);
    success = (write(ioBase->base_outfd, seq, seqlen) == seqlen);
  }

  return success;
}

static int add_efds(InOutBase *ioBase, int fd, int mode, int (*eventFunc)(int, int, void *), void *data)
{
  evtFD *pefd = nullptr;
  int fl;

  if (eventFunc == nullptr && mode != O_RDONLY)
  {
    return -1;
  }

  if ((fl = fcntl(fd, F_GETFL, 0)) == -1)
  {
    return -1;
  }

  fl &= O_ACCMODE;
  if ((fl == O_RDONLY && mode == O_WRONLY) || (fl == O_WRONLY && mode == O_RDONLY))
  {
    return -1;
  }

  for (auto i = 0; i < ioBase->efds_no && !pefd; i++)
  {
    if (ioBase->event_fds[i]->fd == fd)
    {
      pefd = ioBase->event_fds[i];
    }
  }

  if (pefd)
  {
    pefd->mode = mode;
    pefd->data = data;
    pefd->eventFunc = eventFunc;
    pefd->status = EVTFDSTAT_RUN;
  }
  else
  {
    if (ioBase->efds_size <= ioBase->efds_no)
    {
      ioBase->event_fds =
          static_cast<evtFD **>(hb_xrealloc(ioBase->event_fds, (ioBase->efds_size += 10) * sizeof(evtFD *)));
      ioBase->pPollSet = static_cast<PHB_POLLFD>(hb_xrealloc(ioBase->pPollSet, ioBase->efds_size * sizeof(HB_POLLFD)));
    }

    pefd = static_cast<evtFD *>(hb_xgrab(sizeof(evtFD)));
    pefd->fd = fd;
    pefd->mode = mode;
    pefd->data = data;
    pefd->eventFunc = eventFunc;
    pefd->status = EVTFDSTAT_RUN;
    ioBase->event_fds[ioBase->efds_no++] = pefd;
  }

  return fd;
}

static void del_efds(InOutBase *ioBase, int fd)
{
  int n = -1;

  for (auto i = 0; i < ioBase->efds_no && n == -1; i++)
  {
    if (ioBase->event_fds[i]->fd == fd)
    {
      n = i;
    }
  }

  if (n != -1)
  {
    hb_xfree(ioBase->event_fds[n]);
    ioBase->efds_no--;
    for (int i = n; i < ioBase->efds_no; i++)
    {
      ioBase->event_fds[i] = ioBase->event_fds[i + 1];
    }
  }
}

static void del_all_efds(InOutBase *ioBase)
{
  if (ioBase->event_fds != nullptr)
  {
    for (auto i = 0; i < ioBase->efds_no; i++)
    {
      hb_xfree(ioBase->event_fds[i]);
    }

    hb_xfree(ioBase->event_fds);
    hb_xfree(ioBase->pPollSet);

    ioBase->event_fds = nullptr;
    ioBase->pPollSet = nullptr;
    ioBase->efds_no = ioBase->efds_size = 0;
  }
}

static int getMouseKey(mouseEvent *mEvt)
{
  int nKey = 0;

  if (mEvt->lbuttons != mEvt->buttonstate)
  {
    if (mEvt->buttonstate & M_CURSOR_MOVE)
    {
      nKey = HB_INKEY_NEW_MPOS(mEvt->col, mEvt->row);
      mEvt->buttonstate &= ~M_CURSOR_MOVE;
    }
    else if (mEvt->buttonstate & M_BUTTON_WHEELUP)
    {
      nKey = HB_INKEY_NEW_MKEY(K_MWFORWARD, mEvt->flags);
      mEvt->buttonstate &= ~M_BUTTON_WHEELUP;
    }
    else if (mEvt->buttonstate & M_BUTTON_WHEELDOWN)
    {
      nKey = HB_INKEY_NEW_MKEY(K_MWBACKWARD, mEvt->flags);
      mEvt->buttonstate &= ~M_BUTTON_WHEELDOWN;
    }
    else
    {
      int butt = mEvt->lbuttons ^ mEvt->buttonstate;

      if (butt & M_BUTTON_LEFT)
      {
        if (mEvt->buttonstate & M_BUTTON_LEFT)
        {
          mEvt->lbdn_row = mEvt->row;
          mEvt->lbdn_col = mEvt->col;
        }
        else
        {
          mEvt->lbup_row = mEvt->row;
          mEvt->lbup_col = mEvt->col;
        }
        nKey = (mEvt->buttonstate & M_BUTTON_LEFT) ? ((mEvt->buttonstate & M_BUTTON_LDBLCK) ? K_LDBLCLK : K_LBUTTONDOWN)
                                                   : K_LBUTTONUP;
        nKey = HB_INKEY_NEW_MKEY(nKey, mEvt->flags);
        mEvt->lbuttons ^= M_BUTTON_LEFT;
        mEvt->buttonstate &= ~M_BUTTON_LDBLCK;
      }
      else if (butt & M_BUTTON_RIGHT)
      {
        if (mEvt->buttonstate & M_BUTTON_RIGHT)
        {
          mEvt->rbdn_row = mEvt->row;
          mEvt->rbdn_col = mEvt->col;
        }
        else
        {
          mEvt->rbup_row = mEvt->row;
          mEvt->rbup_col = mEvt->col;
        }
        nKey = (mEvt->buttonstate & M_BUTTON_RIGHT)
                   ? ((mEvt->buttonstate & M_BUTTON_RDBLCK) ? K_RDBLCLK : K_RBUTTONDOWN)
                   : K_RBUTTONUP;
        nKey = HB_INKEY_NEW_MKEY(nKey, mEvt->flags);
        mEvt->lbuttons ^= M_BUTTON_RIGHT;
        mEvt->buttonstate &= ~M_BUTTON_RDBLCK;
      }
      else if (butt & M_BUTTON_MIDDLE)
      {
        if (mEvt->buttonstate & M_BUTTON_MIDDLE)
        {
          mEvt->mbdn_row = mEvt->row;
          mEvt->mbdn_col = mEvt->col;
        }
        else
        {
          mEvt->mbup_row = mEvt->row;
          mEvt->mbup_col = mEvt->col;
        }
        nKey = (mEvt->buttonstate & M_BUTTON_MIDDLE)
                   ? ((mEvt->buttonstate & M_BUTTON_MDBLCK) ? K_MDBLCLK : K_MBUTTONDOWN)
                   : K_MBUTTONUP;
        nKey = HB_INKEY_NEW_MKEY(nKey, mEvt->flags);
        mEvt->lbuttons ^= M_BUTTON_MIDDLE;
        mEvt->buttonstate &= ~M_BUTTON_MDBLCK;
      }
      else
      {
        mEvt->lbuttons = mEvt->buttonstate;
      }
    }
  }

  return nKey;
}

static void chk_mevtdblck(mouseEvent *mEvt)
{
  int newbuttons = (mEvt->buttonstate & ~mEvt->lbuttons) & M_BUTTON_KEYMASK;

  if (newbuttons != 0)
  {
    struct timeval tv;

    TIMEVAL_GET(tv);
    if (newbuttons & M_BUTTON_LEFT)
    {
      if (TIMEVAL_LESS(tv, mEvt->BL_time))
      {
        mEvt->buttonstate |= M_BUTTON_LDBLCK;
      }
      TIMEVAL_ADD(mEvt->BL_time, tv, mEvt->click_delay);
    }
    if (newbuttons & M_BUTTON_MIDDLE)
    {
      if (TIMEVAL_LESS(tv, mEvt->BM_time))
      {
        mEvt->buttonstate |= M_BUTTON_MDBLCK;
      }
      TIMEVAL_ADD(mEvt->BM_time, tv, mEvt->click_delay);
    }
    if (newbuttons & M_BUTTON_RIGHT)
    {
      if (TIMEVAL_LESS(tv, mEvt->BR_time))
      {
        mEvt->buttonstate |= M_BUTTON_RDBLCK;
      }
      TIMEVAL_ADD(mEvt->BR_time, tv, mEvt->click_delay);
    }
  }
}

static void set_tmevt(unsigned char *cMBuf, mouseEvent *mEvt)
{
  mEvt->flags = 0;
  if (cMBuf[0] & 0x04)
  {
    mEvt->flags |= HB_KF_SHIFT;
  }
  if (cMBuf[0] & 0x08)
  {
    mEvt->flags |= HB_KF_ALT;
  }
  if (cMBuf[0] & 0x10)
  {
    mEvt->flags |= HB_KF_CTRL;
  }

  int col = cMBuf[1] - 33;
  int row = cMBuf[2] - 33;
  if (mEvt->row != row || mEvt->col != col)
  {
    mEvt->buttonstate |= M_CURSOR_MOVE;
    mEvt->row = row;
    mEvt->col = col;
  }

  switch (cMBuf[0] & 0xC3)
  {
  case 0x0:
    mEvt->buttonstate |= M_BUTTON_LEFT;
    break;
  case 0x1:
    mEvt->buttonstate |= M_BUTTON_MIDDLE;
    break;
  case 0x2:
    mEvt->buttonstate |= M_BUTTON_RIGHT;
    break;
  case 0x3:
    mEvt->buttonstate &= ~(M_BUTTON_KEYMASK | M_BUTTON_DBLMASK);
    break;
  case 0x40:
    if (cMBuf[0] & 0x20)
    {
      mEvt->buttonstate |= M_BUTTON_WHEELUP;
    }
    break;
  case 0x41:
    if (cMBuf[0] & 0x20)
    {
      mEvt->buttonstate |= M_BUTTON_WHEELDOWN;
    }
    break;
  }
  chk_mevtdblck(mEvt);
#if 0
   printf("\n\rmouse event: %02x, %02x, %02x\n\r", cMBuf[0], cMBuf[1], cMBuf[2]);
#endif
}

#if defined(HB_HAS_GPM)
static int set_gpmevt(int fd, int mode, void *data)
{
  HB_SYMBOL_UNUSED(fd);
  HB_SYMBOL_UNUSED(mode);

  int nKey = 0;
  Gpm_Event gEvt;

  auto mEvt = static_cast<mouseEvent *>(data);

  if (Gpm_GetEvent(&gEvt) > 0)
  {
    mEvt->flags = 0;
    if (gEvt.modifiers & (1 << KG_SHIFT))
    {
      mEvt->flags |= HB_KF_SHIFT;
    }
    if (gEvt.modifiers & (1 << KG_CTRL))
    {
      mEvt->flags |= HB_KF_CTRL;
    }
    if (gEvt.modifiers & (1 << KG_ALT))
    {
      mEvt->flags |= HB_KF_ALT;
    }

    mEvt->row = gEvt.y;
    mEvt->col = gEvt.x;
    if (gEvt.type & GPM_MOVE)
    {
      mEvt->buttonstate |= M_CURSOR_MOVE;
    }
    if (gEvt.type & GPM_DOWN)
    {
      if (gEvt.buttons & GPM_B_LEFT)
      {
        mEvt->buttonstate |= M_BUTTON_LEFT;
      }
      if (gEvt.buttons & GPM_B_MIDDLE)
      {
        mEvt->buttonstate |= M_BUTTON_MIDDLE;
      }
      if (gEvt.buttons & GPM_B_RIGHT)
      {
        mEvt->buttonstate |= M_BUTTON_RIGHT;
      }
    }
    else if (gEvt.type & GPM_UP)
    {
      if (gEvt.buttons & GPM_B_LEFT)
      {
        mEvt->buttonstate &= ~M_BUTTON_LEFT;
      }
      if (gEvt.buttons & GPM_B_MIDDLE)
      {
        mEvt->buttonstate &= ~M_BUTTON_MIDDLE;
      }
      if (gEvt.buttons & GPM_B_RIGHT)
      {
        mEvt->buttonstate &= ~M_BUTTON_RIGHT;
      }
    }
  }
  chk_mevtdblck(mEvt);
  nKey = getMouseKey(mEvt);

  return nKey ? (HB_INKEY_ISEXT(nKey) ? nKey : SET_CLIPKEY(nKey)) : 0;
}

static void flush_gpmevt(mouseEvent *mEvt)
{
  if (gpm_fd >= 0)
  {
    while (hb_fsCanRead(gpm_fd, 0) > 0)
    {
      set_gpmevt(gpm_fd, O_RDONLY, static_cast<void *>(mEvt));
    }

    while (getMouseKey(mEvt))
    {
      ;
    }
  }
}
#endif

static void disp_mousecursor(InOutBase *ioBase)
{
#if defined(HB_HAS_GPM)
  if (ioBase->mouse_type == MOUSE_GPM && gpm_visiblepointer)
  {
    Gpm_DrawPointer(ioBase->mLastEvt.col, ioBase->mLastEvt.row, gpm_consolefd);
  }
#else
  HB_SYMBOL_UNUSED(ioBase);
#endif
}

static void mouse_init(InOutBase *ioBase)
{
  if (ioBase->terminal_type == TERM_XTERM)
  {
    // save old hilit tracking & enable mouse tracking
    write_ttyseq(ioBase, "\033[?1001s\033[?1002h");
    ioBase->mouse_type = MOUSE_XTERM;
    memset(static_cast<void *>(&ioBase->mLastEvt), 0, sizeof(ioBase->mLastEvt));
    ioBase->mLastEvt.click_delay = DBLCLK_DELAY;
    // curses mouse buttons check
    ioBase->mButtons = tigetnum(const_cast<char *>("btns"));
    if (ioBase->mButtons < 1)
    {
      ioBase->mButtons = 3;
    }
  }
#if defined(HB_HAS_GPM)
  else if (ioBase->terminal_type == TERM_LINUX)
  {
    ioBase->Conn.eventMask = GPM_MOVE | GPM_DRAG | GPM_UP | GPM_DOWN | GPM_SINGLE | GPM_DOUBLE;
    // give me move events but handle them anyway
    ioBase->Conn.defaultMask = GPM_MOVE | GPM_HARD;
    // only pure mouse events, no Ctrl,Alt,Shift events
    ioBase->Conn.minMod = 0;
    ioBase->Conn.maxMod = ((1 << KG_SHIFT) | (1 << KG_CTRL) | (1 << KG_ALT));
    gpm_zerobased = 1;
    gpm_visiblepointer = 0;
    if (Gpm_Open(&ioBase->Conn, 0) >= 0 && gpm_fd >= 0)
    {
      int flags;

      if ((flags = fcntl(gpm_fd, F_GETFL, 0)) != -1)
      {
        fcntl(gpm_fd, F_SETFL, flags | O_NONBLOCK);
      }

      ioBase->mouse_type = MOUSE_GPM;
      memset(static_cast<void *>(&ioBase->mLastEvt), 0, sizeof(ioBase->mLastEvt));
      ioBase->mLastEvt.click_delay = DBLCLK_DELAY;
      flush_gpmevt(&ioBase->mLastEvt);
      add_efds(ioBase, gpm_fd, O_RDONLY, set_gpmevt, static_cast<void *>(&ioBase->mLastEvt));

      // In recent GPM versions it produce unpleasure noice on the screen
      // so I covered it with this macro, [druzus]
#ifdef HB_GPM_USE_XTRA
      ioBase->mButtons = Gpm_GetSnapshot(nullptr);
#else
      ioBase->mButtons = 3;
#endif
    }
  }
#endif
}

static void mouse_exit(InOutBase *ioBase)
{
  if (ioBase->mouse_type == MOUSE_XTERM)
  {
    // disable mouse tracking & restore old hilit tracking
    write_ttyseq(ioBase, "\033[?1002l\033[?1001r");
  }
#if defined(HB_HAS_GPM)
  else if (ioBase->mouse_type == MOUSE_GPM && gpm_fd >= 0)
  {
    del_efds(ioBase, gpm_fd);
    Gpm_Close();
  }
#endif
}

static int read_bufch(InOutBase *ioBase, int fd)
{
  int n = 0;

  if (STDIN_BUFLEN > ioBase->stdin_inbuf)
  {
    unsigned char buf[STDIN_BUFLEN];

    n = read(fd, buf, STDIN_BUFLEN - ioBase->stdin_inbuf);

    for (auto i = 0; i < n; i++)
    {
      ioBase->stdin_buf[ioBase->stdin_ptr_r++] = buf[i];
      if (ioBase->stdin_ptr_r == STDIN_BUFLEN)
      {
        ioBase->stdin_ptr_r = 0;
      }
      ioBase->stdin_inbuf++;
    }
  }

  return n;
}

static int get_inch(InOutBase *ioBase, HB_MAXINT timeout)
{
  int nRet = 0, nNext = 0, npfd = -1, nchk = ioBase->efds_no, lRead = 0;
  int mode, i, n, counter;
  evtFD *pefd = nullptr;

  HB_MAXUINT timer = hb_timerInit(timeout);

  do
  {
    for (i = n = 0; i < ioBase->efds_no; i++)
    {
      if (ioBase->event_fds[i]->status == EVTFDSTAT_RUN)
      {
        ioBase->pPollSet[n].fd = ioBase->event_fds[i]->fd;
        ioBase->pPollSet[n].events = 0;
        ioBase->pPollSet[n].revents = 0;
        if (ioBase->event_fds[i]->mode == O_RDWR || ioBase->event_fds[i]->mode == O_RDONLY)
        {
          ioBase->pPollSet[n].events |= HB_POLLIN;
        }
        if (ioBase->event_fds[i]->mode == O_RDWR || ioBase->event_fds[i]->mode == O_WRONLY)
        {
          ioBase->pPollSet[n].events |= HB_POLLOUT;
        }
        ioBase->event_fds[i]->index = n++;
      }
      else
      {
        ioBase->event_fds[i]->index = -1;
        if (ioBase->event_fds[i]->status == EVTFDSTAT_STOP && ioBase->event_fds[i]->eventFunc == nullptr)
        {
          nNext = HB_INKEY_NEW_EVENT(HB_K_TERMINATE);
        }
      }
    }

    counter = ioBase->key_counter;
    if (hb_fsPoll(ioBase->pPollSet, n, timeout) > 0)
    {
      for (i = 0; i < ioBase->efds_no; i++)
      {
        n = ioBase->event_fds[i]->index;
        if (n < 0)
        {
          continue;
        }
        n = ioBase->pPollSet[n].revents;
        n = ((n & HB_POLLIN) ? 1 : 0) | ((n & HB_POLLOUT) ? 2 : 0);
        if (n != 0)
        {
          if (ioBase->event_fds[i]->eventFunc == nullptr)
          {
            lRead = 1;
            n = read_bufch(ioBase, ioBase->event_fds[i]->fd);
            if (n == 0)
            {
              ioBase->event_fds[i]->status = EVTFDSTAT_STOP;
              nRet = HB_INKEY_NEW_EVENT(HB_K_CLOSE);
            }
          }
          else if (nRet == 0 && counter == ioBase->key_counter)
          {
            if (n == 3)
            {
              mode = O_RDWR;
            }
            else if (n == 2)
            {
              mode = O_WRONLY;
            }
            else
            {
              mode = O_RDONLY;
            }
            ioBase->event_fds[i]->status = EVTFDSTAT_STOP;
            n = (ioBase->event_fds[i]->eventFunc)(ioBase->event_fds[i]->fd, mode, ioBase->event_fds[i]->data);
            if (IS_EVTFDSTAT(n))
            {
              ioBase->event_fds[i]->status = n;
              if (nchk > i)
              {
                nchk = i;
              }
            }
            else
            {
              ioBase->event_fds[i]->status = EVTFDSTAT_RUN;
              if (IS_CLIPKEY(n) || HB_INKEY_ISEXT(n))
              {
                nRet = n;
                npfd = ioBase->event_fds[i]->fd;
                if (nchk > i)
                {
                  nchk = i;
                }
              }
            }
          }
        }
      }
    }
    else
    {
      lRead = 1;
    }
  } while (nRet == 0 && lRead == 0 && (timeout = hb_timerTest(timeout, &timer)) != 0);

  for (i = n = nchk; i < ioBase->efds_no; i++)
  {
    if (ioBase->event_fds[i]->status == EVTFDSTAT_DEL)
    {
      hb_xfree(ioBase->event_fds[i]);
    }
    else if (ioBase->event_fds[i]->fd == npfd)
    {
      pefd = ioBase->event_fds[i];
    }
    else
    {
      if (i > n)
      {
        ioBase->event_fds[n] = ioBase->event_fds[i];
      }
      n++;
    }
  }
  if (pefd)
  {
    ioBase->event_fds[n++] = pefd;
  }
  ioBase->efds_no = n;

  return nRet == 0 ? nNext : nRet;
}

static int test_bufch(InOutBase *ioBase, int n, int delay)
{
  int nKey = 0;

  if (ioBase->stdin_inbuf == n)
  {
    nKey = get_inch(ioBase, delay);
  }

  return (IS_CLIPKEY(nKey) || HB_INKEY_ISEXT(nKey))
             ? nKey
             : (ioBase->stdin_inbuf > n ? ioBase->stdin_buf[(ioBase->stdin_ptr_l + n) % STDIN_BUFLEN] : -1);
}

static void free_bufch(InOutBase *ioBase, int n)
{
  if (n > ioBase->stdin_inbuf)
  {
    n = ioBase->stdin_inbuf;
  }
  ioBase->stdin_ptr_l = (ioBase->stdin_ptr_l + n) % STDIN_BUFLEN;
  ioBase->stdin_inbuf -= n;
}

static int wait_key(InOutBase *ioBase, int milisec)
{
  int nKey, esc, n, i, ch, counter;
  keyTab *ptr;

  if (s_WinSizeChangeFlag)
  {
    s_WinSizeChangeFlag = false;
    return K_RESIZE;
  }

restart:
  counter = ++(ioBase->key_counter);
  nKey = esc = n = i = 0;
again:
  if ((nKey = getMouseKey(&ioBase->mLastEvt)) != 0)
  {
    return nKey;
  }

  ch = test_bufch(ioBase, i, ioBase->nTermMouseChars ? ioBase->esc_delay : milisec);
  if (counter != ioBase->key_counter)
  {
    goto restart;
  }

  if (ch >= 0 && ch <= 255)
  {
    ++i;
    if (ioBase->nTermMouseChars)
    {
      ioBase->cTermMouseBuf[3 - ioBase->nTermMouseChars] = ch;
      free_bufch(ioBase, i);
      i = 0;
      if (--ioBase->nTermMouseChars == 0)
      {
        set_tmevt(ioBase->cTermMouseBuf, &ioBase->mLastEvt);
      }
      goto again;
    }

    nKey = ch;
    ptr = ioBase->pKeyTab;
    if (i == 1 && nKey == K_ESC && esc == 0)
    {
      nKey = EXKEY_ESC;
      esc = 1;
    }
    while (ch >= 0 && ch <= 255 && ptr != nullptr)
    {
      if (ptr->ch == ch)
      {
        if (ptr->key != K_UNDEF)
        {
          nKey = ptr->key;
          switch (nKey)
          {
          case K_METAALT:
            ioBase->key_flag |= KEY_ALTMASK;
            break;
          case K_METACTRL:
            ioBase->key_flag |= KEY_CTRLMASK;
            break;
          case K_NATIONAL:
            ioBase->nation_mode = !ioBase->nation_mode;
            break;
          case K_MOUSETERM:
            ioBase->nTermMouseChars = 3;
            break;
          default:
            n = i;
          }
          if (n != i)
          {
            free_bufch(ioBase, i);
            i = n = nKey = 0;
            if (esc == 2)
            {
              break;
            }
            esc = 0;
            goto again;
          }
        }
        ptr = ptr->nextCh;
        if (ptr)
        {
          if ((ch = test_bufch(ioBase, i, ioBase->esc_delay)) != -1)
          {
            ++i;
          }
        }
        if (counter != ioBase->key_counter)
        {
          goto restart;
        }
      }
      else
      {
        ptr = ptr->otherCh;
      }
    }
  }
  if (ch == -1 && ioBase->nTermMouseChars)
  {
    ioBase->nTermMouseChars = 0;
  }

  if (IS_CLIPKEY(ch))
  {
    nKey = GET_CLIPKEY(ch);
  }
  else if (HB_INKEY_ISEXT(ch))
  {
    nKey = ch;
  }
  else
  {
    if (esc == 1 && n == 0 && (ch != -1 || i >= 2))
    {
      nKey = 0;
      esc = 2;
      i = n = 1;
      goto again;
    }
    if (esc == 2)
    {
      if (nKey != 0)
      {
        ioBase->key_flag |= KEY_ALTMASK;
      }
      else
      {
        nKey = EXKEY_ESC;
      }
      if (n == 1 && i > 1)
      {
        n = 2;
      }
    }
    else
    {
      if (nKey != 0 && (ioBase->key_flag & KEY_CTRLMASK) != 0 && (ioBase->key_flag & KEY_ALTMASK) != 0)
      {
        ioBase->key_flag &= ~(KEY_CTRLMASK | KEY_ALTMASK);
        ioBase->key_flag |= KEY_SHIFTMASK;
      }
      if (n == 0 && i > 0)
      {
        n = 1;
      }
    }

    if (n > 0)
    {
      free_bufch(ioBase, n);
    }

    if (ioBase->key_flag != 0 && nKey != 0)
    {
      nKey |= ioBase->key_flag;
      ioBase->key_flag = 0;
    }

    if (ioBase->nation_transtbl && ioBase->nation_mode && nKey >= 32 && nKey < 128 && ioBase->nation_transtbl[nKey])
    {
      nKey = ioBase->nation_transtbl[nKey];
    }
    if (ioBase->in_transtbl && nKey > 0 && nKey <= 255 && ioBase->in_transtbl[nKey])
    {
      nKey = ioBase->in_transtbl[nKey];
    }

    if (nKey)
    {
      nKey = getClipKey(nKey);
    }
  }

  return nKey;
}

static int addKeyMap(InOutBase *ioBase, int nKey, const char *cdesc)
{
  int ret = K_UNDEF, i = 0;

  if (cdesc == nullptr)
  {
    return ret;
  }

  int c = static_cast<unsigned char>(cdesc[i++]);
  keyTab **ptr = &ioBase->pKeyTab;

  while (c)
  {
    if (*ptr == nullptr)
    {
      *ptr = static_cast<keyTab *>(hb_xgrab(sizeof(keyTab)));
      (*ptr)->ch = c;
      (*ptr)->key = K_UNDEF;
      (*ptr)->nextCh = nullptr;
      (*ptr)->otherCh = nullptr;
    }
    if ((*ptr)->ch == c)
    {
      c = static_cast<unsigned char>(cdesc[i++]);
      if (c)
      {
        ptr = &((*ptr)->nextCh);
      }
      else
      {
        ret = (*ptr)->key;
        (*ptr)->key = nKey;
      }
    }
    else
    {
      ptr = &((*ptr)->otherCh);
    }
  }
  return ret;
}

static int removeKeyMap(InOutBase *ioBase, const char *cdesc)
{
  int ret = K_UNDEF, i = 0;

  int c = static_cast<unsigned char>(cdesc[i++]);
  keyTab **ptr = &ioBase->pKeyTab;

  while (c && *ptr != nullptr)
  {
    if ((*ptr)->ch == c)
    {
      c = static_cast<unsigned char>(cdesc[i++]);
      if (!c)
      {
        ret = (*ptr)->key;
        (*ptr)->key = K_UNDEF;
        if ((*ptr)->nextCh == nullptr && (*ptr)->otherCh == nullptr)
        {
          hb_xfree(*ptr);
          *ptr = nullptr;
        }
      }
      else
      {
        ptr = &((*ptr)->nextCh);
      }
    }
    else
    {
      ptr = &((*ptr)->otherCh);
    }
  }
  return ret;
}

static void removeAllKeyMap(keyTab **ptr)
{
  if ((*ptr)->nextCh != nullptr)
  {
    removeAllKeyMap(&((*ptr)->nextCh));
  }
  if ((*ptr)->otherCh != nullptr)
  {
    removeAllKeyMap(&((*ptr)->otherCh));
  }

  hb_xfree(*ptr);
  *ptr = nullptr;
}

static void disp_cursor(InOutBase *ioBase)
{
  if (ioBase->cursor != ioBase->lcursor)
  {
    int lcurs = -1;
    char *cv = nullptr;

    switch (ioBase->cursor)
    {
    case SC_NONE:
      lcurs = 1;
      cv = ioBase->civis;
      break;
    case SC_NORMAL:
      lcurs = 2;
      cv = ioBase->cnorm;
      break;
    case SC_INSERT:
      lcurs = 4;
      cv = ioBase->cvvis;
      break;
    case SC_SPECIAL1:
      lcurs = 8;
      cv = ioBase->cvvis;
      break;
    case SC_SPECIAL2:
      // TODO: find a proper sequence to set a cursor
      // to SC_SPECIAL2 under Linux console?
      // There is no such mode in current stable kernels (2.4.20)
      lcurs = 4;
      cv = ioBase->cvvis;
      break;
    }

    if (lcurs != -1)
    {
      if (ioBase->terminal_type == TERM_LINUX)
      {
        char escseq[64];
        hb_snprintf(escseq, sizeof(escseq), "\033[?25%c\033[?%dc", ioBase->cursor == SC_NONE ? 'l' : 'h', lcurs);
        write_ttyseq(ioBase, escseq);
      }
      else if (cv != nullptr)
      {
// curses cursor shape set
#if 0
            curs_set(ncurs);
#endif
        write_ttyseq(ioBase, cv);
      }
    }

    ioBase->lcursor = ioBase->cursor;
  }
}

static void set_cursor(InOutBase *ioBase, int style)
{
  switch (style)
  {
  case SC_NONE:
  case SC_NORMAL:
  case SC_INSERT:
  case SC_SPECIAL1:
  case SC_SPECIAL2:
    ioBase->cursor = style;
    disp_cursor(ioBase);
    break;
  }
}

static void gt_refresh(InOutBase *ioBase)
{
  if (ioBase->disp_count == 0)
  {
#if 0
      if( ioBase->cursor == SC_NONE ) {
         leaveok(ioBase->hb_stdscr, true);
      } else {
         leaveok(ioBase->hb_stdscr, false);
      }
#endif
    // if( ioBase->cursor != SC_NONE )
    wmove(ioBase->hb_stdscr, ioBase->row, ioBase->col);
    wrefresh(ioBase->hb_stdscr);
    disp_cursor(ioBase);
    disp_mousecursor(ioBase);
  }
}

static void gt_ttyset(InOutBase *ioBase)
{
  if (isatty(ioBase->base_infd))
  {
    tcsetattr(ioBase->base_infd, TCSANOW, &ioBase->curr_TIO);
  }
}

static void gt_ttyrestore(InOutBase *ioBase)
{
  if (ioBase->lTIOsaved)
  {
    tcsetattr(ioBase->base_infd, TCSANOW, &ioBase->saved_TIO);
  }
}

static bool gt_outstr(InOutBase *ioBase, int fd, const char *str, int len)
{
  auto success = false;

  if (ioBase->out_transtbl != nullptr)
  {
    auto buf = static_cast<unsigned char *>(hb_xgrab(len));
    for (auto i = 0; i < len; ++i)
    {
      unsigned char c = str[i];
      if (c != 9 && c != 10 && c != 13 && ioBase->out_transtbl[c])
      {
        buf[i] = ioBase->out_transtbl[c];
      }
      else
      {
        buf[i] = c;
      }
    }
    success = (write(fd, buf, len) == len);
    hb_xfree(buf);
  }
  else
  {
    success = (write(fd, str, len) == len);
  }

  return success;
}

static void gt_outstd(InOutBase *ioBase, const char *str, int len)
{
  gt_outstr(ioBase, ioBase->stdoutfd, str, len);
}

static void gt_outerr(InOutBase *ioBase, const char *str, int len)
{
  gt_outstr(ioBase, ioBase->stderrfd, str, len);
}

static char *tiGetS(const char *capname)
{
  char *ptr = tigetstr(const_cast<char *>(capname));
  if (ptr)
  {
    if (ptr == reinterpret_cast<char *>(-1))
    {
      ptr = nullptr;
    }
    else if (!ptr[0])
    {
      ptr = nullptr;
    }
  }
  return ptr;
}

static void get_acsc(InOutBase *ioBase, unsigned char c, chtype *pch)
{
  unsigned char *ptr;

  if (ioBase->acsc != nullptr)
  {
    for (ptr = reinterpret_cast<unsigned char *>(ioBase->acsc); *ptr && *(ptr + 1); ptr += 2)
    {
      if (*ptr == c)
      {
        *pch = *(ptr + 1) | A_ALTCHARSET;
        return;
      }
    }
  }

  switch (c)
  {
  case '.':
    *pch = 'v' | A_NORMAL;
    break;
  case ',':
    *pch = '<' | A_NORMAL;
    break;
  case '+':
    *pch = '>' | A_NORMAL;
    break;
  case '-':
    *pch = '^' | A_NORMAL;
    break;
  case 'a':
    *pch = '#' | A_NORMAL;
    break;
  case '0':
  case 'h':
    get_acsc(ioBase, 'a', pch);
    break;
  default:
    *pch = c | A_ALTCHARSET;
  }
}

static void init_keys(InOutBase *ioBase)
{
  // virual CTRL/ALT sequences
  addKeyMap(ioBase, K_METACTRL, CTRL_SEQ);
  addKeyMap(ioBase, K_METAALT, ALT_SEQ);
  // national mode key sequences
#ifdef NATION_SEQ
  addKeyMap(ioBase, K_NATIONAL, NATION_SEQ);
#endif

// some harcoded sequences
#if 0
   addKeyMap(ioBase, K_ESC, "\033\033");
#endif
  addKeyMap(ioBase, EXKEY_ENTER, "\r");
  addKeyMap(ioBase, K_MOUSETERM, "\033[M");

  if (ioBase->terminal_type == TERM_XTERM)
  {

    addKeyMap(ioBase, EXKEY_UP, "\033[A");
    addKeyMap(ioBase, EXKEY_DOWN, "\033[B");
    addKeyMap(ioBase, EXKEY_RIGHT, "\033[C");
    addKeyMap(ioBase, EXKEY_LEFT, "\033[D");
    addKeyMap(ioBase, EXKEY_CENTER, "\033[E");
    addKeyMap(ioBase, EXKEY_END, "\033[F");
    addKeyMap(ioBase, EXKEY_HOME, "\033[H");
    addKeyMap(ioBase, EXKEY_HOME, "\033[1~");
    addKeyMap(ioBase, EXKEY_END, "\033[4~");
    addKeyMap(ioBase, EXKEY_BS, "\177");

    addKeyMap(ioBase, EXKEY_F1, "\033[11~");
    addKeyMap(ioBase, EXKEY_F2, "\033[12~");
    addKeyMap(ioBase, EXKEY_F3, "\033[13~");
    addKeyMap(ioBase, EXKEY_F4, "\033[14~");
    addKeyMap(ioBase, EXKEY_F5, "\033[15~");

    addKeyMap(ioBase, EXKEY_UP | KEY_CTRLMASK, "\033[5A");
    addKeyMap(ioBase, EXKEY_DOWN | KEY_CTRLMASK, "\033[5B");
    addKeyMap(ioBase, EXKEY_RIGHT | KEY_CTRLMASK, "\033[5C");
    addKeyMap(ioBase, EXKEY_LEFT | KEY_CTRLMASK, "\033[5D");
    addKeyMap(ioBase, EXKEY_CENTER | KEY_CTRLMASK, "\033[5E");
    addKeyMap(ioBase, EXKEY_END | KEY_CTRLMASK, "\033[5F");
    addKeyMap(ioBase, EXKEY_HOME | KEY_CTRLMASK, "\033[5H");
    addKeyMap(ioBase, EXKEY_INS | KEY_CTRLMASK, "\033[2;5~");
    addKeyMap(ioBase, EXKEY_PGUP | KEY_CTRLMASK, "\033[5;5~");
    addKeyMap(ioBase, EXKEY_PGDN | KEY_CTRLMASK, "\033[6;5~");

    addKeyMap(ioBase, EXKEY_F1 | KEY_SHIFTMASK, "\033O2P");
    addKeyMap(ioBase, EXKEY_F2 | KEY_SHIFTMASK, "\033O2Q");
    addKeyMap(ioBase, EXKEY_F3 | KEY_SHIFTMASK, "\033O2R");
    addKeyMap(ioBase, EXKEY_F4 | KEY_SHIFTMASK, "\033O2S");
    addKeyMap(ioBase, EXKEY_F5 | KEY_SHIFTMASK, "\033[15;2~");
    addKeyMap(ioBase, EXKEY_F6 | KEY_SHIFTMASK, "\033[17;2~");
    addKeyMap(ioBase, EXKEY_F7 | KEY_SHIFTMASK, "\033[18;2~");
    addKeyMap(ioBase, EXKEY_F8 | KEY_SHIFTMASK, "\033[19;2~");
    addKeyMap(ioBase, EXKEY_F9 | KEY_SHIFTMASK, "\033[20;2~");
    addKeyMap(ioBase, EXKEY_F10 | KEY_SHIFTMASK, "\033[21;2~");
    addKeyMap(ioBase, EXKEY_F11 | KEY_SHIFTMASK, "\033[23;2~");
    addKeyMap(ioBase, EXKEY_F12 | KEY_SHIFTMASK, "\033[24;2~");

    addKeyMap(ioBase, EXKEY_TAB | KEY_SHIFTMASK, "\033[Z");

    // key added for gnome-terminal and teraterm

    addKeyMap(ioBase, EXKEY_ENTER | KEY_CTRLMASK, "\033[7;5~");
    addKeyMap(ioBase, EXKEY_DEL | KEY_CTRLMASK, "\033[3;5~");
    addKeyMap(ioBase, EXKEY_TAB | KEY_CTRLMASK, "\033[8;5~");

    addKeyMap(ioBase, EXKEY_UP | KEY_SHIFTMASK, "\033[6A");
    addKeyMap(ioBase, EXKEY_DOWN | KEY_SHIFTMASK, "\033[6B");
    addKeyMap(ioBase, EXKEY_RIGHT | KEY_SHIFTMASK, "\033[6C");
    addKeyMap(ioBase, EXKEY_LEFT | KEY_SHIFTMASK, "\033[6D");
    addKeyMap(ioBase, EXKEY_CENTER | KEY_SHIFTMASK, "\033[6E");
    addKeyMap(ioBase, EXKEY_END | KEY_SHIFTMASK, "\033[6F");
    addKeyMap(ioBase, EXKEY_HOME | KEY_SHIFTMASK, "\033[6H");
    addKeyMap(ioBase, EXKEY_ENTER | KEY_SHIFTMASK, "\033[7;6~");
    addKeyMap(ioBase, EXKEY_INS | KEY_SHIFTMASK, "\033[2;6~");
    addKeyMap(ioBase, EXKEY_DEL | KEY_SHIFTMASK, "\033[3;6~");
    addKeyMap(ioBase, EXKEY_PGUP | KEY_SHIFTMASK, "\033[5;6~");
    addKeyMap(ioBase, EXKEY_PGDN | KEY_SHIFTMASK, "\033[6;6~");

    addKeyMap(ioBase, EXKEY_BS | KEY_SHIFTMASK, "\033[W");

    // end of added
  }
  else if (ioBase->terminal_type == TERM_LINUX)
  {

    addKeyMap(ioBase, EXKEY_F1, "\033[[A");   // kf1
    addKeyMap(ioBase, EXKEY_F2, "\033[[B");   // kf2
    addKeyMap(ioBase, EXKEY_F3, "\033[[C");   // kf3
    addKeyMap(ioBase, EXKEY_F4, "\033[[D");   // kf4
    addKeyMap(ioBase, EXKEY_F5, "\033[[E");   // kf5
    addKeyMap(ioBase, EXKEY_F6, "\033[17~");  // kf6
    addKeyMap(ioBase, EXKEY_F7, "\033[18~");  // kf7
    addKeyMap(ioBase, EXKEY_F8, "\033[19~");  // kf8 
    addKeyMap(ioBase, EXKEY_F9, "\033[20~");  // kf9
    addKeyMap(ioBase, EXKEY_F10, "\033[21~"); // kf10
    addKeyMap(ioBase, EXKEY_F11, "\033[23~"); // kf11
    addKeyMap(ioBase, EXKEY_F12, "\033[24~"); // kf12

    addKeyMap(ioBase, EXKEY_F1 | KEY_SHIFTMASK, "\033[25~");  // kf13
    addKeyMap(ioBase, EXKEY_F2 | KEY_SHIFTMASK, "\033[26~");  // kf14
    addKeyMap(ioBase, EXKEY_F3 | KEY_SHIFTMASK, "\033[28~");  // kf15
    addKeyMap(ioBase, EXKEY_F4 | KEY_SHIFTMASK, "\033[29~");  // kf16
    addKeyMap(ioBase, EXKEY_F5 | KEY_SHIFTMASK, "\033[31~");  // kf17
    addKeyMap(ioBase, EXKEY_F6 | KEY_SHIFTMASK, "\033[32~");  // kf18
    addKeyMap(ioBase, EXKEY_F7 | KEY_SHIFTMASK, "\033[33~");  // kf19
    addKeyMap(ioBase, EXKEY_F8 | KEY_SHIFTMASK, "\033[34~");  // kf20
    addKeyMap(ioBase, EXKEY_F9 | KEY_SHIFTMASK, "\033[35~");  // kf21
    addKeyMap(ioBase, EXKEY_F10 | KEY_SHIFTMASK, "\033[36~"); // kf22
    addKeyMap(ioBase, EXKEY_F11 | KEY_SHIFTMASK, "\033[37~"); // kf23
    addKeyMap(ioBase, EXKEY_F12 | KEY_SHIFTMASK, "\033[38~"); // kf24

    addKeyMap(ioBase, EXKEY_F1 | KEY_CTRLMASK, "\033[39~");  // kf25
    addKeyMap(ioBase, EXKEY_F2 | KEY_CTRLMASK, "\033[40~");  // kf26
    addKeyMap(ioBase, EXKEY_F3 | KEY_CTRLMASK, "\033[41~");  // kf27
    addKeyMap(ioBase, EXKEY_F4 | KEY_CTRLMASK, "\033[42~");  // kf28
    addKeyMap(ioBase, EXKEY_F5 | KEY_CTRLMASK, "\033[43~");  // kf29
    addKeyMap(ioBase, EXKEY_F6 | KEY_CTRLMASK, "\033[44~");  // kf30
    addKeyMap(ioBase, EXKEY_F7 | KEY_CTRLMASK, "\033[45~");  // kf31
    addKeyMap(ioBase, EXKEY_F8 | KEY_CTRLMASK, "\033[46~");  // kf32
    addKeyMap(ioBase, EXKEY_F9 | KEY_CTRLMASK, "\033[47~");  // kf33
    addKeyMap(ioBase, EXKEY_F10 | KEY_CTRLMASK, "\033[48~"); // kf34
    addKeyMap(ioBase, EXKEY_F11 | KEY_CTRLMASK, "\033[49~"); // kf35
    addKeyMap(ioBase, EXKEY_F12 | KEY_CTRLMASK, "\033[50~"); // kf36

    addKeyMap(ioBase, EXKEY_F1 | KEY_ALTMASK, "\033[51~");  // kf37
    addKeyMap(ioBase, EXKEY_F2 | KEY_ALTMASK, "\033[52~");  // kf38
    addKeyMap(ioBase, EXKEY_F3 | KEY_ALTMASK, "\033[53~");  // kf39
    addKeyMap(ioBase, EXKEY_F4 | KEY_ALTMASK, "\033[54~");  // kf40
    addKeyMap(ioBase, EXKEY_F5 | KEY_ALTMASK, "\033[55~");  // kf41
    addKeyMap(ioBase, EXKEY_F6 | KEY_ALTMASK, "\033[56~");  // kf42
    addKeyMap(ioBase, EXKEY_F7 | KEY_ALTMASK, "\033[57~");  // kf43
    addKeyMap(ioBase, EXKEY_F8 | KEY_ALTMASK, "\033[58~");  // kf44
    addKeyMap(ioBase, EXKEY_F9 | KEY_ALTMASK, "\033[59~");  // kf45
    addKeyMap(ioBase, EXKEY_F10 | KEY_ALTMASK, "\033[70~"); // kf46
    addKeyMap(ioBase, EXKEY_F11 | KEY_ALTMASK, "\033[71~"); // kf47
    addKeyMap(ioBase, EXKEY_F12 | KEY_ALTMASK, "\033[72~"); // kf48
  }

  // (curses) termcap/terminfo sequences

  // terminal mouse event
  addKeyMap(ioBase, K_MOUSETERM, "kmous");

  // FlagShip extension
  addKeyMap(ioBase, EXKEY_HOME | KEY_CTRLMASK, tiGetS("ked"));
  addKeyMap(ioBase, EXKEY_END | KEY_CTRLMASK, tiGetS("kel"));
  addKeyMap(ioBase, EXKEY_PGUP | KEY_CTRLMASK, tiGetS("kri"));
  addKeyMap(ioBase, EXKEY_PGDN | KEY_CTRLMASK, tiGetS("kind"));
  addKeyMap(ioBase, EXKEY_RIGHT | KEY_CTRLMASK, tiGetS("kctab"));
  addKeyMap(ioBase, EXKEY_LEFT | KEY_CTRLMASK, tiGetS("khts"));

  // some xterms extension
  addKeyMap(ioBase, EXKEY_HOME, tiGetS("kfnd"));
  addKeyMap(ioBase, EXKEY_END, tiGetS("kslt"));

  // keypad
  addKeyMap(ioBase, EXKEY_CENTER, tiGetS("kb2"));
  addKeyMap(ioBase, EXKEY_HOME, tiGetS("ka1"));
  addKeyMap(ioBase, EXKEY_END, tiGetS("kc1"));
  addKeyMap(ioBase, EXKEY_PGUP, tiGetS("ka3"));
  addKeyMap(ioBase, EXKEY_PGDN, tiGetS("kc3"));

  // other keys
  addKeyMap(ioBase, EXKEY_ENTER, tiGetS("kent"));
  addKeyMap(ioBase, EXKEY_END, tiGetS("kend"));
  addKeyMap(ioBase, EXKEY_PGUP, tiGetS("kpp"));
  addKeyMap(ioBase, EXKEY_PGDN, tiGetS("knp"));
  addKeyMap(ioBase, EXKEY_UP, tiGetS("kcuu1"));
  addKeyMap(ioBase, EXKEY_DOWN, tiGetS("kcud1"));
  addKeyMap(ioBase, EXKEY_RIGHT, tiGetS("kcuf1"));
  addKeyMap(ioBase, EXKEY_LEFT, tiGetS("kcub1"));
  addKeyMap(ioBase, EXKEY_HOME, tiGetS("khome"));
  addKeyMap(ioBase, EXKEY_INS, tiGetS("kich1"));
  addKeyMap(ioBase, EXKEY_DEL, tiGetS("kdch1"));
  addKeyMap(ioBase, EXKEY_TAB, tiGetS("ht"));
  addKeyMap(ioBase, EXKEY_BS, tiGetS("kbs"));
  addKeyMap(ioBase, EXKEY_TAB | KEY_ALTMASK, tiGetS("kcbt"));

  // function keys
  addKeyMap(ioBase, EXKEY_F1, tiGetS("kf1"));
  addKeyMap(ioBase, EXKEY_F2, tiGetS("kf2"));
  addKeyMap(ioBase, EXKEY_F3, tiGetS("kf3"));
  addKeyMap(ioBase, EXKEY_F4, tiGetS("kf4"));
  addKeyMap(ioBase, EXKEY_F5, tiGetS("kf5"));
  addKeyMap(ioBase, EXKEY_F6, tiGetS("kf6"));
  addKeyMap(ioBase, EXKEY_F7, tiGetS("kf7"));
  addKeyMap(ioBase, EXKEY_F8, tiGetS("kf8"));
  addKeyMap(ioBase, EXKEY_F9, tiGetS("kf9"));
  addKeyMap(ioBase, EXKEY_F10, tiGetS("kf10"));
  addKeyMap(ioBase, EXKEY_F11, tiGetS("kf11"));
  addKeyMap(ioBase, EXKEY_F12, tiGetS("kf12"));

  // shifted function keys
  addKeyMap(ioBase, EXKEY_F1 | KEY_SHIFTMASK, tiGetS("kf13"));
  addKeyMap(ioBase, EXKEY_F2 | KEY_SHIFTMASK, tiGetS("kf14"));
  addKeyMap(ioBase, EXKEY_F3 | KEY_SHIFTMASK, tiGetS("kf15"));
  addKeyMap(ioBase, EXKEY_F4 | KEY_SHIFTMASK, tiGetS("kf16"));
  addKeyMap(ioBase, EXKEY_F5 | KEY_SHIFTMASK, tiGetS("kf17"));
  addKeyMap(ioBase, EXKEY_F6 | KEY_SHIFTMASK, tiGetS("kf18"));
  addKeyMap(ioBase, EXKEY_F7 | KEY_SHIFTMASK, tiGetS("kf19"));
  addKeyMap(ioBase, EXKEY_F8 | KEY_SHIFTMASK, tiGetS("kf20"));
  addKeyMap(ioBase, EXKEY_F9 | KEY_SHIFTMASK, tiGetS("kf21"));
  addKeyMap(ioBase, EXKEY_F10 | KEY_SHIFTMASK, tiGetS("kf22"));
  addKeyMap(ioBase, EXKEY_F11 | KEY_SHIFTMASK, tiGetS("kf23"));
  addKeyMap(ioBase, EXKEY_F12 | KEY_SHIFTMASK, tiGetS("kf24"));
}

static void gt_tone(InOutBase *ioBase, double dFrequency, double dDuration)
{
  if (ioBase->terminal_type == TERM_LINUX && ioBase->beep != nullptr)
  {
    char escseq[64];
    hb_snprintf(escseq, sizeof(escseq), "\033[10;%d]\033[11;%d]%s", static_cast<int>(dFrequency),
                static_cast<int>(dDuration * 1000.0 / 18.2), ioBase->beep);
    write_ttyseq(ioBase, escseq);
  }
  else
  {
    // curses beep()
    if (ioBase->beep != nullptr)
    {
      write_ttyseq(ioBase, ioBase->beep);
    }
    else if (ioBase->flash != nullptr)
    {
      write_ttyseq(ioBase, ioBase->flash);
    }
  }
}

static void set_sig_keys(InOutBase *ioBase, int key_int, int key_brk, int key_stp)
{
  if (isatty(ioBase->base_infd))
  {
    // set SIGINT character, default ^C
    if (key_int >= 0 && key_int <= 255)
    {
      ioBase->curr_TIO.c_cc[VINTR] = key_int;
    }

    // set SIGQUIT character, default ^D
    if (key_brk >= 0 && key_brk <= 255)
    {
      ioBase->curr_TIO.c_cc[VQUIT] = key_brk;
    }

    // set SIGTSTP character, default ^Z
    if (key_stp >= 0 && key_stp <= 255)
    {
      ioBase->curr_TIO.c_cc[VSUSP] = key_stp;
    }

    // enable signals from terminal device
    if (ioBase->curr_TIO.c_cc[VINTR] != 0 || ioBase->curr_TIO.c_cc[VQUIT] != 0 || ioBase->curr_TIO.c_cc[VSUSP] != 0)
    {
      ioBase->curr_TIO.c_lflag |= ISIG;
    }

#if 0
      ioctl(ioBase->base_infd, TIOCSCTTY, 0);
#endif
    gt_ttyset(ioBase);
  }
}

static int gt_getsize(InOutBase *ioBase, int *rows, int *cols)
{
  int ret = -1;

  *rows = *cols = 0;

#if defined(TIOCGWINSZ)
  if (isatty(ioBase->base_outfd))
  {
    struct winsize win;

    if (ioctl(ioBase->base_outfd, TIOCGWINSZ, reinterpret_cast<char *>(&win)) != -1)
    {
      *rows = win.ws_row;
      *cols = win.ws_col;
    }
  }
#endif

  if (*rows <= 0 || *cols <= 0)
  {
    char *env;

    if ((env = getenv("COLUMNS")))
    {
      *cols = atoi(env);
    }
    if ((env = getenv("LINES")))
    {
      *rows = atoi(env);
    }
  }
  if (*rows > 0 && *cols > 0)
  {
    ret = (ioBase->maxrow == *rows && ioBase->maxcol == *cols) ? 0 : 1;
  }

  return ret;
}

static int gt_resize(InOutBase *ioBase)
{
  int ret = -1;
  int rows = 0, cols = 0;

  if (gt_getsize(ioBase, &rows, &cols) >= 0)
  {
#if 0
#if defined(NCURSES_VERSION)
      wresize(ioBase->hb_stdscr, rows, cols);
#endif
#endif
    endwin();
    gt_refresh(ioBase);
    ret = 0;
#if 0
#if defined(NCURSES_VERSION)
      if( resize_term(rows, cols) == OK ) {
         ret = 0;
         gt_refresh(ioBase);
      }
#endif
#endif
    getmaxyx(ioBase->hb_stdscr, ioBase->maxrow, ioBase->maxcol);
  }
  return ret;
}

static int gt_setsize(InOutBase *ioBase, int rows, int cols)
{
  int ret = -1;

  if (ioBase->terminal_type == TERM_XTERM)
  {
    int r, c;
    char escseq[64];
    hb_snprintf(escseq, sizeof(escseq), "\033[8;%d;%dt", rows, cols);
    write_ttyseq(ioBase, escseq);
    // dirty hack - wait for SIGWINCH
    if (gt_getsize(ioBase, &r, &c) > 0)
    {
      sleep(3);
    }

    if (s_WinSizeChangeFlag)
    {
      s_WinSizeChangeFlag = false;
      ret = gt_resize(ioBase);
    }
#if defined(TIOCGWINSZ)
    else if (isatty(ioBase->base_outfd))
    {
      struct winsize win;

      if (ioctl(ioBase->base_outfd, TIOCGWINSZ, reinterpret_cast<char *>(&win)) != -1)
      {
        win.ws_row = rows;
        win.ws_col = cols;
        ioctl(ioBase->base_outfd, TIOCSWINSZ, reinterpret_cast<char *>(&win));
      }
      ret = gt_resize(ioBase);
    }
#endif
  }

  return ret;
}

static void setKeyTrans(InOutBase *ioBase, PHB_CODEPAGE cdpTerm, PHB_CODEPAGE cdpHost)
{
  if (cdpTerm && cdpHost && cdpTerm != cdpHost)
  {
    if (ioBase->in_transtbl == nullptr)
    {
      ioBase->in_transtbl = static_cast<unsigned char *>(hb_xgrab(256));
    }

    for (auto i = 0; i < 256; ++i)
    {
      ioBase->in_transtbl[i] = hb_cdpTranslateChar(i, cdpTerm, cdpHost);
    }
  }
  else if (ioBase->in_transtbl != nullptr)
  {
    hb_xfree(ioBase->in_transtbl);
    ioBase->in_transtbl = nullptr;
  }
}

static void setDispTrans(InOutBase *ioBase, PHB_CODEPAGE cdpHost, PHB_CODEPAGE cdpTerm, int transBox)
{
  int aSet = (cdpHost && cdpTerm);

  for (auto i = 0; i < 256; i++)
  {
    chtype ch = ioBase->charmap[i] & 0xffff;
    switch ((ioBase->charmap[i] >> 16) & 0xff)
    {
    case 1:
      ioBase->std_chmap[i] = ioBase->box_chmap[i] = A_NORMAL;
      break;
    case 2:
      ioBase->std_chmap[i] = ioBase->box_chmap[i] = A_ALTCHARSET;
      break;
    case 3:
      ioBase->std_chmap[i] = ioBase->box_chmap[i] = A_PROTECT;
      break;
    case 4:
      ioBase->std_chmap[i] = ioBase->box_chmap[i] = A_ALTCHARSET | A_PROTECT;
      break;
    case 5:
      get_acsc(ioBase, ch & 0xff, &ch);
      ioBase->std_chmap[i] = ioBase->box_chmap[i] = ch & ~A_CHARTEXT;
      ch &= A_CHARTEXT;
      break;
    case 0:
    default:
      ioBase->std_chmap[i] = aSet ? A_ALTCHARSET : A_NORMAL;
      ioBase->box_chmap[i] = A_ALTCHARSET;
      break;
    }
    ioBase->std_chmap[i] |= ch;
    ioBase->box_chmap[i] |= ch;

    if (i != static_cast<int>(ch & A_CHARTEXT) && (ioBase->std_chmap[i] & A_ALTCHARSET) == 0)
    {
      if (ioBase->out_transtbl == nullptr)
      {
        ioBase->out_transtbl = static_cast<unsigned char *>(hb_xgrabz(256));
      }
      ioBase->out_transtbl[i] = ch & A_CHARTEXT;
    }
  }
  if (aSet)
  {
    for (auto i = 0; i < 256; ++i)
    {
      if (hb_cdpIsAlpha(cdpHost, i))
      {
        auto uc = static_cast<unsigned char>(hb_cdpTranslateDispChar(i, cdpHost, cdpTerm));

        ioBase->std_chmap[i] = uc | A_NORMAL;
        if (transBox)
        {
          ioBase->box_chmap[i] = uc | A_NORMAL;
        }
        if (i != static_cast<int>(uc))
        {
          if (ioBase->out_transtbl == nullptr)
          {
            ioBase->out_transtbl = static_cast<unsigned char *>(hb_xgrabz(256));
          }
          ioBase->out_transtbl[i] = uc;
        }
      }
    }
  }
}

static InOutBase *create_ioBase(char *term, int infd, int outfd, int errfd, pid_t termpid)
{
  int bg, fg;
  unsigned int i;
  char buf[256], *ptr, *crsterm = nullptr;

  auto ioBase = static_cast<InOutBase *>(hb_xgrabz(sizeof(InOutBase)));

  if (!term || !*term)
  {
    term = getenv("HB_TERM");
  }
  if (!term || !*term)
  {
    term = getenv("TERM");
  }

  if (term && *term)
  {
    if (strncmp(term, "linux", 5) == 0)
    {
      ioBase->terminal_type = TERM_LINUX;
    }
    else if (strstr(term, "xterm") != nullptr || strncmp(term, "rxvt", 4) == 0 || strstr(term, "putty") == 0)
    {
      ioBase->terminal_type = TERM_XTERM;
    }

    if ((ptr = strchr(term, '/')) != nullptr)
    {
      if ((i = ptr - term) >= sizeof(buf))
      {
        i = sizeof(buf) - 1;
      }
      hb_strncpy(buf, term, i);
      if (i)
      {
        crsterm = buf;
      }
    }
    else
    {
      crsterm = term;
    }
  }

  ioBase->esc_delay = ESC_DELAY;
  ioBase->base_infd = infd;
  ioBase->base_outfd = outfd;
  ioBase->stdoutfd = outfd;
  ioBase->stderrfd = errfd;
  ioBase->termpid = termpid;
  ioBase->cursor = ioBase->lcursor = SC_UNDEF;

  if (!isatty(ioBase->base_outfd) && isatty(ioBase->base_infd))
  {
    ioBase->base_outfd = ioBase->base_infd;
  }

  if (isatty(ioBase->stdoutfd))
  {
    ioBase->stdoutfd = -1;
  }
  if (isatty(ioBase->stderrfd))
  {
    ioBase->stderrfd = -1;
  }

  if (isatty(ioBase->base_infd))
  {
    tcgetattr(ioBase->base_infd, &ioBase->curr_TIO); // save current terminal settings
    memcpy(&ioBase->saved_TIO, &ioBase->curr_TIO, sizeof(struct termios));
    ioBase->lTIOsaved = 1;

    ioBase->curr_TIO.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    ioBase->curr_TIO.c_lflag |= NOFLSH;
    ioBase->curr_TIO.c_cflag &= ~(CSIZE | PARENB);
    ioBase->curr_TIO.c_cflag |= CS8 | CREAD;
    ioBase->curr_TIO.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    ioBase->curr_TIO.c_oflag &= ~OPOST;
#if 0
      ioBase->curr_TIO.c_oflag |= ONLCR | OPOST;
#endif

    memset(ioBase->curr_TIO.c_cc, 0, NCCS);

    // workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
    // *buntu) in which select() unconditionally accepts stdin for
    // reading if c_cc[VMIN] = 0 [druzus]
    ioBase->curr_TIO.c_cc[VMIN] = 1;
  }

  // curses SCREEN initialization
  if (ioBase->base_infd == fileno(stdin))
  {
    ioBase->basein = stdin;
  }
  else
  {
    ioBase->basein = fdopen(dup(ioBase->base_infd), "r");
  }

  if (ioBase->base_outfd == fileno(stdout))
  {
    ioBase->baseout = stdout;
  }
  else
  {
    ioBase->baseout = fdopen(dup(ioBase->base_outfd), "w");
  }

  // curses screen initialization
  ioBase->basescr = newterm(crsterm, ioBase->baseout, ioBase->basein);
#if 0
   def_shell_mode();
   def_prog_mode();
#endif
  curs_wrkaround();

  if (ioBase->basescr == nullptr)
  {
    destroy_ioBase(ioBase);
    return nullptr;
  }

  ioBase->hb_stdscr = stdscr;

  ioBase->flash = tiGetS("flash");
  ioBase->beep = tiGetS("bel");
  ioBase->civis = tiGetS("civis");
  ioBase->cnorm = tiGetS("cnorm");
  ioBase->cvvis = tiGetS("cvvis");
  if (ioBase->cvvis == nullptr)
  {
    ioBase->cvvis = ioBase->cnorm;
  }
  ioBase->acsc = tiGetS("acsc");

  ioBase->charmap = static_cast<int *>(hb_xgrab(256 * sizeof(int)));
  hb_gt_chrmapinit(ioBase->charmap, term, ioBase->terminal_type == TERM_XTERM);
  setDispTrans(ioBase, nullptr, nullptr, 0);

  ioBase->attr_mask = static_cast<chtype>(-1);
  if (has_colors())
  {
    // DOS->CURSES color mapping
    // DOS              -> curses
    // --------------------------------
    //  0 black         -> COLOR_BLACK
    //  1 blue          -> COLOR_BLUE
    //  2 green         -> COLOR_GREEN
    //  3 cyan          -> COLOR_CYAN
    //  4 red           -> COLOR_RED
    //  5 magenta       -> COLOR_MAGENTA
    //  6 yellow        -> COLOR_YELLOW
    //  7 light gray    -> COLOR_WHITE
    //
    //  8 gray          -> BOLD/BLINK BLACK
    //  9 light blue    -> BOLD/BLINK BLUE
    // 10 light green   -> BOLD/BLINK GREEN
    // 11 light cyan    -> BOLD/BLINK CYAN
    // 12 light red     -> BOLD/BLINK RED
    // 13 light magenta -> BOLD/BLINK MAGENTA
    // 14 light yellow  -> BOLD/BLINK YELLOW
    // 15 white         -> BOLD/BLINK WHITE

    static const char color_map[] = {COLOR_BLACK, COLOR_BLUE,    COLOR_GREEN,  COLOR_CYAN,
                                     COLOR_RED,   COLOR_MAGENTA, COLOR_YELLOW, COLOR_WHITE};

    start_color();

#if 0
      for( bg = 0; bg < COLORS; bg++ ) {
         for( fg = 0; fg < COLORS; fg++ ) {
            i = bg * COLORS + fg;
            if( i == 0 ) {
               i = 7;
            } else if( i == 7 ) {
               i = 0;
            }
            init_pair(i, color_map[fg], color_map[bg]);
         }
      }
#endif
    for (i = 0; i < 256; i++)
    {
      unsigned int n;
      bg = (i >> 4) & 0x07; // extract background color bits 4-6
      fg = (i & 0x07);      // extract foreground color bits 0-2
      n = bg * 8 + fg;
      // n = bg * COLORS + fg
      if (n == 0)
      {
        n = 7;
      }
      else if (n == 7)
      {
        n = 0;
      }
      if ((i & 0x88) == 0)
      {
        init_pair(n, color_map[fg], color_map[bg]);
      }
      ioBase->attr_map[i] = COLOR_PAIR(n);
      if (i & 0x08)
      { // highlight foreground bit 3
        ioBase->attr_map[i] |= A_BOLD;
      }
      if (i & 0x80)
      { // blink/highlight background bit 7
        ioBase->attr_map[i] |= A_BLINK;
      }
    }
    ioBase->is_color = 1;
  }
  else
  {
    for (i = 0; i < 256; i++)
    {
      bg = (i >> 4) & 0x07; // extract background color bits 4-6
      fg = (i & 0x07);      // extract foreground color bits 0-2
      ioBase->attr_map[i] = 0;
      if (fg < bg)
      {
        ioBase->attr_map[i] |= A_REVERSE;
      }
      if (fg == 1)
      { // underline?
        ioBase->attr_map[i] |= A_UNDERLINE;
      }
      if (i & 0x08)
      { // highlight foreground bit 3
        ioBase->attr_map[i] |= A_BOLD;
      }
      if (i & 0x80)
      { // blink/highlight background bit 7
        ioBase->attr_map[i] |= A_BLINK;
      }
      ioBase->is_color = 0;
    }
  }

  getmaxyx(ioBase->hb_stdscr, ioBase->maxrow, ioBase->maxcol);
  scrollok(ioBase->hb_stdscr, false);
#if 0
   idlok(ioBase->hb_stdscr, false);
   idcok(ioBase->hb_stdscr, false);
   leaveok(ioBase->hb_stdscr, false);
#endif

  // curses keyboard initialization
  // we have our own keyboard routine so it's unnecessary now
  // but we call raw() to inform some versions of curses that
  // there is no OPOST translation

  raw();

  leaveok(ioBase->hb_stdscr, false);
  curs_set(0);

#if 0
   nonl();
   nodelay(ioBase->hb_stdscr, true);
   keypad(ioBase->hb_stdscr, false);
   timeout(0);
   noecho();
   curs_set(0);
#endif
  wclear(ioBase->hb_stdscr);
  wrefresh(ioBase->hb_stdscr);

  gt_ttyset(ioBase);
  add_efds(ioBase, ioBase->base_infd, O_RDONLY, nullptr, nullptr);

  init_keys(ioBase);
  mouse_init(ioBase);

  return ioBase;
}

static void destroy_ioBase(InOutBase *ioBase)
{
  mouse_exit(ioBase);
  del_all_efds(ioBase);

  if (ioBase->terminal_type == TERM_LINUX)
  {
    // restore a standard bell frequency and duration
    write_ttyseq(ioBase, "\033[10]\033[11]");
  }

  // curses SCREEN delete
  if (ioBase->hb_stdscr != nullptr)
  {
    ioBase->disp_count = 0;
    // on exit restore a cursor share and leave it visible
    // Marek's NOTE: This is incompatible with Clipper
    if (ioBase->cursor != SC_UNDEF)
    {
      set_cursor(ioBase, SC_NORMAL);
    }
    gt_refresh(ioBase);
    endwin();
  }
  if (ioBase->basescr != nullptr)
  {
    delscreen(ioBase->basescr);
  }
  if (ioBase->basein != nullptr && ioBase->basein != stdin)
  {
    fclose(ioBase->basein);
  }
  if (ioBase->baseout != nullptr && ioBase->baseout != stdout)
  {
    fclose(ioBase->baseout);
  }

  // free allocated memory
  if (ioBase->charmap != nullptr)
  {
    hb_xfree(ioBase->charmap);
  }

  if (ioBase->in_transtbl != nullptr)
  {
    hb_xfree(ioBase->in_transtbl);
  }

  if (ioBase->out_transtbl != nullptr)
  {
    hb_xfree(ioBase->out_transtbl);
  }

  if (ioBase->nation_transtbl != nullptr)
  {
    hb_xfree(ioBase->nation_transtbl);
  }

  if (ioBase->pKeyTab != nullptr)
  {
    removeAllKeyMap(&ioBase->pKeyTab);
  }

  // restore terminal settings
  gt_ttyrestore(ioBase);

  // kill terminal process if any
  if (ioBase->termpid > 0)
  {
    kill(ioBase->termpid, SIGTERM);
    waitpid(ioBase->termpid, nullptr, 0);
  }

  hb_xfree(ioBase);
}

static InOutBase *create_newXterm(void)
{
#if defined(HB_OS_LINUX) || defined(HB_OS_BSD)
#if 0
   int masterfd, slavefd, fd;
   pid_t termpid;
   char ptyname[64], buf[64], * ptr;

   if( !getenv("DISPLAY") ) {
      return nullptr;
   }

   if( openpty(&masterfd, &slavefd, ptyname, nullptr, nullptr) == -1 ) {
      return nullptr;
   }

   tcsetpgrp(masterfd, getpgrp());

   if( (termpid = fork()) == -1 ) {
      close(masterfd);
      close(slavefd);
      return nullptr;
   }

   if( termpid == 0 ) {
      if( (ptr = strrchr(ptyname, '/')) ) {
         ++ptr;
      } else {
         ptr = ptyname;
      }
      hb_snprintf(buf, sizeof(buf), "-S%s/%d", ptr, masterfd);
#if 0
      close(0);
      close(1);
      close(2);
#endif
#if 0
      dup2(masterfd, 0);
      dup2(masterfd, 1);
#if 0
      fd = open("/tmp/hb-xterm.log", O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
#endif
      fd = open("/dev/null", O_WRONLY);
      dup2(fd, 2);
#endif
      for( fd = 3; fd < MAXFD; ++fd ) {
         if( fd != masterfd ) {
            close(fd);
         }
      }

      ( void ) setuid(getuid()); // TODO: C++ cast
      ( void ) setgid(getgid()); // TODO: C++ cast
      execlp("xterm", "xterm", buf, "+sb", "-fg", "white", "-bg", "black", "-fn", "fixed", "-T", "HB-XTERM Window", nullptr);
      _exit(EXIT_FAILURE);
   }
   close(masterfd);
   return create_ioBase("xterm", slavefd, slavefd, slavefd, termpid);
#endif
#endif
  return nullptr;
}

static int set_active_ioBase(int iNO_ioBase)
{
  int iPrev = s_iActive_ioBase;

  if (iNO_ioBase >= 0 && iNO_ioBase < s_iSize_ioBaseTab)
  {
    s_iActive_ioBase = iNO_ioBase;
    s_ioBase = s_ioBaseTab[s_iActive_ioBase];
    set_term(s_ioBase->basescr);
  }

  return iPrev;
}

static int add_new_ioBase(InOutBase *ioBase)
{
  int i, add = 0;

  for (i = 0; i < s_iSize_ioBaseTab && !add; ++i)
  {
    if (!s_ioBaseTab[i])
    {
      s_ioBaseTab[i] = ioBase;
      add = 1;
    }
  }

  if (!add)
  {
    if (s_ioBaseTab == nullptr)
    {
      s_ioBaseTab = static_cast<InOutBase **>(hb_xgrab((s_iSize_ioBaseTab += 10) * sizeof(InOutBase *)));
    }
    else
    {
      s_ioBaseTab =
          static_cast<InOutBase **>(hb_xrealloc(s_ioBaseTab, (s_iSize_ioBaseTab += 10) * sizeof(InOutBase *)));
    }
    s_ioBaseTab[i] = ioBase;
    for (int n = i + 1; n < s_iSize_ioBaseTab; n++)
    {
      s_ioBaseTab[n] = nullptr;
    }
  }

  if (!s_ioBase)
  {
    set_active_ioBase(i);
  }

  return i;
}

static int del_ioBase(int iNO_ioBase)
{
  if (iNO_ioBase >= 0 && iNO_ioBase < s_iSize_ioBaseTab)
  {
    destroy_ioBase(s_ioBaseTab[iNO_ioBase]);
    s_ioBaseTab[iNO_ioBase] = nullptr;
    if (s_iActive_ioBase == iNO_ioBase)
    {
      s_iActive_ioBase = -1;
      s_ioBase = nullptr;
      for (auto i = 0; i < s_iSize_ioBaseTab && !s_ioBase; ++i)
      {
        if (s_ioBaseTab[i])
        {
          set_active_ioBase(i);
        }
      }
    }
  }

  return s_iActive_ioBase;
}

static void del_all_ioBase(void)
{
  if (s_ioBaseTab)
  {
    for (auto i = 0; i < s_iSize_ioBaseTab; ++i)
    {
      if (s_ioBaseTab[i])
      {
        destroy_ioBase(s_ioBaseTab[i]);
      }
    }
    hb_xfree(s_ioBaseTab);
    s_ioBaseTab = nullptr;
  }
  s_iActive_ioBase = -1;
  s_ioBase = nullptr;
}

// ***********************************************************************

HB_BOOL HB_GT_FUNC(gt_AddEventHandle(int iFile, int iMode, int (*eventFunc)(int, int, void *), void *data))
{
  return add_efds(s_ioBase, iFile, iMode, eventFunc, data) == iFile;
}

void HB_GT_FUNC(gt_DelEventHandle(int iFileDes))
{
  del_efds(s_ioBase, iFileDes);
}

int HB_GT_FUNC(gt_NewXTerm(void))
{
  int iHandle = -1;

  InOutBase *ioBase = create_newXterm();
  if (ioBase)
  {
    set_sig_keys(ioBase, 'C' - ('A' - 1), 'D' - ('A' - 1), 'Z' - ('A' - 1));
    iHandle = add_new_ioBase(ioBase);
  }
  return iHandle;
}

int HB_GT_FUNC(gt_SetTerm(int iHandle))
{
  return set_active_ioBase(iHandle);
}

int HB_GT_FUNC(gt_CloseTerm(int iHandle))
{
  return del_ioBase(iHandle);
}

int HB_GT_FUNC(gt_WaitKey(double dTimeOut))
{
  return wait_key(s_ioBase, static_cast<int>(dTimeOut >= 0 ? dTimeOut * 1000.0 : -1));
}

int HB_GT_FUNC(gt_AddKeyMap(int iKey, char *szSequence))
{
  return addKeyMap(s_ioBase, SET_CLIPKEY(iKey), szSequence);
}

int HB_GT_FUNC(gt_RemoveKeyMap(char *szSequence))
{
  return removeKeyMap(s_ioBase, szSequence);
}

int HB_GT_FUNC(gt_ESCdelay(int iDelay))
{
  int iRet = s_ioBase->esc_delay;
  s_ioBase->esc_delay = iDelay;
  return iRet;
}

void HB_GT_FUNC(gt_SetInterruptKey(int iInterupt))
{
  set_sig_keys(s_ioBase, iInterupt, -1, -1);
}

void HB_GT_FUNC(gt_SetDebugKey(int iDebug))
{
  set_sig_keys(s_ioBase, -1, iDebug, -1);
}

HB_BOOL HB_GT_FUNC(gt_GetSignalFlag(int iSig))
{
  auto bRetVal = false;

  if (iSig > 0 && iSig < MAX_SIGNO && s_SignalTable[iSig])
  {
    bRetVal = true;
    s_SignalTable[iSig] = false;
  }

  return bRetVal;
}

void HB_GT_FUNC(gt_CatchSignal(int iSig))
{
  set_sig_handler(iSig);
}

// ***********************************************************************

static void hb_gt_crs_Init(PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout,
                           HB_FHANDLE hFilenoStderr) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Init(%p,%p,%p,%p)", static_cast<void*>(pGT), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdin)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdout)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStderr))));
#endif

  if (!s_ioBase)
  {
    InOutBase *ioBase;

    s_iStdIn = hFilenoStdin;
    s_iStdOut = hFilenoStdout;
    s_iStdErr = hFilenoStderr;
#ifdef HB_GT_CRS_TTYHACK
    {
      int ittyfd;

      if ((ittyfd = open("/dev/tty", O_RDWR)) != -1)
      {
        hFilenoStdin = hFilenoStdout = ittyfd;
      }
    }
#endif
    set_signals();
    ioBase = create_ioBase(nullptr, hFilenoStdin, hFilenoStdout, hFilenoStderr, -1);
    if (ioBase)
    {
      add_new_ioBase(ioBase);
      HB_GTSUPER_INIT(pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr);
      HB_GTSELF_RESIZE(pGT, s_ioBase->maxrow, s_ioBase->maxcol);
      HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, false);
      HB_GTSELF_SETBLINK(pGT, true);
    }
  }

  if (!s_ioBase)
  {
    hb_errInternal(9997, "Internal error: screen driver initialization failure", nullptr, nullptr);
  }
}

// ***********************************************************************

static void hb_gt_crs_Exit(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Exit(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSUPER_EXIT(pGT);
  del_all_ioBase();
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_IsColor(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_IsColor(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
  return s_ioBase->is_color;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_SetMode(PHB_GT pGT, int iRows, int iCols) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_SetMode(%p,%d,%d)", static_cast<void*>(pGT), iRows, iCols));
#endif

  if (gt_setsize(s_ioBase, iRows, iCols) == 0)
  {
    HB_GTSELF_RESIZE(pGT, iRows, iCols);
    return true;
  }

  return false;
}

// ***********************************************************************

static void hb_gt_crs_SetBlink(PHB_GT pGT, HB_BOOL fBlink) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_SetBlink(%p, %d)", static_cast<void*>(pGT), static_cast<int>(fBlink)));
#endif

  if (fBlink)
  {
    s_ioBase->attr_mask |= A_BLINK;
  }
  else
  {
    s_ioBase->attr_mask &= ~A_BLINK;
  }

  HB_GTSUPER_SETBLINK(pGT, fBlink);
}

// ***********************************************************************

static void hb_gt_crs_Tone(PHB_GT pGT, double dFrequency, double dDuration) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Tone(%p,%lf,%lf)", static_cast<void*>(pGT), dFrequency, dDuration));
#endif

  gt_tone(s_ioBase, dFrequency, dDuration);

  if (s_ioBase->terminal_type == TERM_LINUX)
  {
    // convert Clipper (DOS) timer tick units to seconds ( x / 18.2 )
    hb_gtSleep(pGT, dDuration / 18.2);
  }
}

// ***********************************************************************

static const char *hb_gt_crs_Version(PHB_GT pGT, int iType) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Version(%p,%d)", static_cast<void*>(pGT), iType));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (iType == 0)
  {
    return HB_GT_DRVNAME(HB_GT_NAME);
  }

  return "Harbour++ Terminal: nCurses";
}

// ***********************************************************************

static void hb_gt_crs_OutStd(PHB_GT pGT, const char *szStr, HB_SIZE nLen) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_OutStd(%p,%s,%lu)", static_cast<void*>(pGT), szStr, nLen));
#endif

  if (s_ioBase)
  {
    if (s_ioBase->stdoutfd == -1)
    {
      HB_GTSELF_WRITECON(pGT, szStr, nLen);
    }
    else
    {
      gt_outstd(s_ioBase, szStr, nLen);
    }
  }
  else
  {
    HB_GTSUPER_OUTSTD(pGT, szStr, nLen);
  }
}

// ***********************************************************************

static void hb_gt_crs_OutErr(PHB_GT pGT, const char *szStr, HB_SIZE nLen) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_OutErr(%p,%s,%lu)", static_cast<void*>(pGT), szStr, nLen));
#endif
  if (s_ioBase)
  {
    if (s_ioBase->stderrfd == -1)
    {
      HB_GTSELF_WRITECON(pGT, szStr, nLen);
    }
    else
    {
      gt_outerr(s_ioBase, szStr, nLen);
    }
  }
  else
  {
    HB_GTSUPER_OUTERR(pGT, szStr, nLen);
  }
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_Suspend(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Suspend(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (s_ioBase)
  {
    gt_refresh(s_ioBase);
    endwin();
    gt_ttyrestore(s_ioBase);
  }

  return true;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_Resume(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Resume(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (s_ioBase)
  {
    s_ioBase->lcursor = SC_UNDEF;
    wrefresh(s_ioBase->hb_stdscr);
    gt_ttyset(s_ioBase);
#if 0
      redrawwin(s_ioBase->hb_stdscr);
#endif
    gt_refresh(s_ioBase);
  }

  return true;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_PreExt(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_PreExt(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (s_ioBase)
  {
    gt_refresh(s_ioBase);
  }

  return true;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_PostExt(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_PostExt(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
  return true;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_mouse_IsPresent(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_IsPresent(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
  return s_ioBase->mouse_type != MOUSE_NONE;
}

// ***********************************************************************

static void hb_gt_crs_mouse_Show(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_Show(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

#if defined(HB_HAS_GPM)
  if (s_ioBase->mouse_type == MOUSE_GPM)
  {
    gpm_visiblepointer = 1;
  }
#endif
  disp_mousecursor(s_ioBase);
}

// ***********************************************************************

static void hb_gt_crs_mouse_Hide(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_Hide(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);

#if defined(HB_HAS_GPM)
  if (s_ioBase->mouse_type == MOUSE_GPM)
  {
    gpm_visiblepointer = 0;
  }
#endif
}

// ***********************************************************************

static void hb_gt_crs_mouse_GetPos(PHB_GT pGT, int *piRow, int *piCol) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_Col(%p,%p,%p)", static_cast<void*>(pGT), static_cast<void*>(piRow), static_cast<void*>(piCol)));
#endif

  HB_SYMBOL_UNUSED(pGT);

  *piRow = s_ioBase->mLastEvt.row;
  *piCol = s_ioBase->mLastEvt.col;
}

// ***********************************************************************

static void hb_gt_crs_mouse_SetPos(PHB_GT pGT, int iRow, int iCol) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_SetPos(%p,%i,%i)", static_cast<void*>(pGT), iRow, iCol));
#endif

  HB_SYMBOL_UNUSED(pGT);

  // it does really nothing
  s_ioBase->mLastEvt.col = iCol;
  s_ioBase->mLastEvt.row = iRow;
  disp_mousecursor(s_ioBase);
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_mouse_ButtonState(PHB_GT pGT, int iButton) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_ButtonState(%p,%i)", static_cast<void*>(pGT), iButton));
#endif

  HB_SYMBOL_UNUSED(pGT);

  auto ret = false;

  if (s_ioBase->mouse_type != 0)
  {
    int mask;

    if (iButton == 0)
    {
      mask = M_BUTTON_LEFT;
    }
    else if (iButton == 1)
    {
      mask = M_BUTTON_RIGHT;
    }
    else if (iButton == 2)
    {
      mask = M_BUTTON_MIDDLE;
    }
    else
    {
      mask = 0;
    }

    ret = (s_ioBase->mLastEvt.buttonstate & mask) != 0;
  }

  return ret;
}

// ***********************************************************************

static int hb_gt_crs_mouse_CountButton(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_CountButton(%p)", static_cast<void*>(pGT)));
#endif

  HB_SYMBOL_UNUSED(pGT);
  return s_ioBase->mButtons;
}

static void hb_gt_crs_mouse_SetDoubleClickSpeed(PHB_GT pGT, int iSpeed) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_mouse_SetDoubleClickSpeed(%p,%d)", static_cast<void*>(pGT), iSpeed));
#endif

  HB_GTSUPER_MOUSESETDOUBLECLICKSPEED(pGT, iSpeed);
  s_ioBase->mLastEvt.click_delay = iSpeed;
}

// ***********************************************************************

static int hb_gt_crs_ReadKey(PHB_GT pGT, int iEventMask) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_ReadKey(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  HB_SYMBOL_UNUSED(iEventMask);

  int iKey = wait_key(s_ioBase, 0);

  if (iKey == K_RESIZE)
  {
    gt_resize(s_ioBase);
    HB_GTSELF_RESIZE(pGT, s_ioBase->maxrow, s_ioBase->maxcol);
    iKey = HB_K_RESIZE;
  }

  return iKey;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_SetDispCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP,
                                   HB_BOOL fBox) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_SetDispCP(%p,%s,%s,%d)", static_cast<void*>(pGT), pszTermCDP, pszHostCDP, static_cast<int>(fBox)));
#endif

  if (HB_GTSUPER_SETDISPCP(pGT, pszTermCDP, pszHostCDP, fBox))
  {
    setDispTrans(s_ioBase, HB_GTSELF_HOSTCP(pGT), HB_GTSELF_TERMCP(pGT), fBox ? 1 : 0);
    return true;
  }
  return false;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_SetKeyCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_SetKeyCP(%p,%s,%s)", static_cast<void*>(pGT), pszTermCDP, pszHostCDP));
#endif

  if (HB_GTSUPER_SETKEYCP(pGT, pszTermCDP, pszHostCDP))
  {
    setKeyTrans(s_ioBase, HB_GTSELF_INCP(pGT), HB_GTSELF_HOSTCP(pGT));
    return true;
  }
  return false;
}

// ***********************************************************************

static HB_BOOL hb_gt_crs_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Info(%p,%d,%p)", static_cast<void*>(pGT), iType, static_cast<void*>(pInfo)));
#endif

  if (s_ioBase)
  {
    switch (iType)
    {
    case HB_GTI_ISSCREENPOS:
    case HB_GTI_KBDSUPPORT:
      pInfo->pResult = hb_itemPutL(pInfo->pResult, true);
      break;

    case HB_GTI_ESCDELAY:
      pInfo->pResult = hb_itemPutNI(pInfo->pResult, s_ioBase->esc_delay);
      if (hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC)
      {
        s_ioBase->esc_delay = pInfo->pNewVal->getNI();
      }
      break;

    default:
      return HB_GTSUPER_INFO(pGT, iType, pInfo);
    }
  }

  return true;
}

// ***********************************************************************

static void hb_gt_crs_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Redraw(%p,%d,%d,%d)", static_cast<void*>(pGT), iRow, iCol, iSize));
#endif

  if (s_ioBase)
  {
    int iColor;
    HB_BYTE bAttr;
    HB_UCHAR uc;
    chtype ch;

    wmove(s_ioBase->hb_stdscr, iRow, iCol);
    while (iSize-- > 0)
    {
      if (!HB_GTSELF_GETSCRUC(pGT, iRow, iCol++, &iColor, &bAttr, &uc, false))
      {
        break;
      }
      ch = (s_ioBase->attr_map[iColor] & s_ioBase->attr_mask) |
           ((bAttr & HB_GT_ATTR_BOX) ? s_ioBase->box_chmap[uc] : s_ioBase->std_chmap[uc]);
      waddch(s_ioBase->hb_stdscr, ch);
    }
  }
}

// ***********************************************************************

static void hb_gt_crs_Refresh(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_crs_Refresh(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSUPER_REFRESH(pGT);
  if (s_ioBase)
  {
    int iRow, iCol, iShape;

    HB_GTSELF_GETSCRCURSOR(pGT, &iRow, &iCol, &iShape);
    s_ioBase->row = iRow;
    s_ioBase->col = iCol;
    set_cursor(s_ioBase, iShape);
    gt_refresh(s_ioBase);
  }
}

// ***********************************************************************

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", static_cast<void*>(pFuncTable)));
#endif

  pFuncTable->Init = hb_gt_crs_Init;
  pFuncTable->Exit = hb_gt_crs_Exit;
  pFuncTable->IsColor = hb_gt_crs_IsColor;
  pFuncTable->SetMode = hb_gt_crs_SetMode;
  pFuncTable->Redraw = hb_gt_crs_Redraw;
  pFuncTable->Refresh = hb_gt_crs_Refresh;
  pFuncTable->SetBlink = hb_gt_crs_SetBlink;
  pFuncTable->Version = hb_gt_crs_Version;
  pFuncTable->Suspend = hb_gt_crs_Suspend;
  pFuncTable->Resume = hb_gt_crs_Resume;
  pFuncTable->PreExt = hb_gt_crs_PreExt;
  pFuncTable->PostExt = hb_gt_crs_PostExt;
  pFuncTable->OutStd = hb_gt_crs_OutStd;
  pFuncTable->OutErr = hb_gt_crs_OutErr;
  pFuncTable->Tone = hb_gt_crs_Tone;
  pFuncTable->Info = hb_gt_crs_Info;
  pFuncTable->SetDispCP = hb_gt_crs_SetDispCP;
  pFuncTable->SetKeyCP = hb_gt_crs_SetKeyCP;

  pFuncTable->ReadKey = hb_gt_crs_ReadKey;

  pFuncTable->MouseIsPresent = hb_gt_crs_mouse_IsPresent;
  pFuncTable->MouseShow = hb_gt_crs_mouse_Show;
  pFuncTable->MouseHide = hb_gt_crs_mouse_Hide;
  pFuncTable->MouseGetPos = hb_gt_crs_mouse_GetPos;
  pFuncTable->MouseSetPos = hb_gt_crs_mouse_SetPos;
  pFuncTable->MouseButtonState = hb_gt_crs_mouse_ButtonState;
  pFuncTable->MouseCountButton = hb_gt_crs_mouse_CountButton;
  pFuncTable->MouseSetDoubleClickSpeed = hb_gt_crs_mouse_SetDoubleClickSpeed;

  return true;
}

// ***********************************************************************

#include "hbgtreg.hpp"

// ***********************************************************************

#if defined(HB_GT_CRS_BCEHACK) && defined(NCURSES_VERSION)
#include <term.h>
static void curs_wrkaround(void)
{
  back_color_erase = false;
#if 0
   cur_term->type.Booleans[28] = 0;
#endif
}
#else
static void curs_wrkaround(void)
{
  ;
}
#endif
