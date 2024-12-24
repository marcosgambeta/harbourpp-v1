/*
 * Windows communications library
 *
 * Copyright 2005-2009 Alex Strickland <sscc@mweb.co.za>
 *
 */

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
#include <hbapierr.hpp>
#include <hbinit.hpp>

static struct
{
  HANDLE hPort;
  int iFunction;
  DWORD dwError;
} s_PortData[256];

static void hb_wincom_init(void)
{
  for (auto i = 0; i < static_cast<int>(HB_SIZEOFARRAY(s_PortData)); i++)
  {
    s_PortData[i].hPort = INVALID_HANDLE_VALUE;
  }
}

HB_FUNC(WIN_COMOPEN)
{
  auto iPort = hb_parni(1);

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)))
  {
    auto dwBaudRate = static_cast<DWORD>(hb_parnl(2));
    auto iParity = hb_parni(3);
    auto iByteSize = hb_parni(4);
    auto iStopBits = hb_parni(5);
    int iPos, i;

    HANDLE hCommPort;
    DCB NewDCB;

    TCHAR szName[11] = {TEXT('\\'), TEXT('\\'), TEXT('.'),  TEXT('\\'), TEXT('C'), TEXT('O'),
                        TEXT('M'),  TEXT('\0'), TEXT('\0'), TEXT('\0'), TEXT('\0')};

    i = iPort + 1;
    iPos = 6;
    while (i > 0)
    {
      iPos++;
      i /= 10;
    }
    i = iPort + 1;
    while (i > 0)
    {
      szName[iPos--] = static_cast<TCHAR>(i % 10 + '0');
      i /= 10;
    }

    s_PortData[iPort].hPort = INVALID_HANDLE_VALUE;
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_CREATEFILE;
    s_PortData[iPort].dwError = 0;

    if ((hCommPort = CreateFile(szName, GENERIC_READ | GENERIC_WRITE, 0, nullptr, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING,
                                nullptr)) == INVALID_HANDLE_VALUE)
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retnl(-1);
      return;
    }

    NewDCB.DCBlength = sizeof(DCB);
    if (!GetCommState(hCommPort, &NewDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      CloseHandle(hCommPort);
      hb_retnl(-1);
      return;
    }

    /* Initialised with NO flow control or control signals! */
    NewDCB.BaudRate = dwBaudRate;
    NewDCB.fBinary = 1;
    NewDCB.fParity = 0;
    NewDCB.fOutxCtsFlow = 0;
    NewDCB.fOutxDsrFlow = 0;
    NewDCB.fDtrControl = DTR_CONTROL_DISABLE;
    NewDCB.fDsrSensitivity = 0;
    NewDCB.fTXContinueOnXoff = 1;
    NewDCB.fOutX = 0;
    NewDCB.fInX = 0;
    NewDCB.fErrorChar = 1;
    NewDCB.fNull = 0;
    NewDCB.fRtsControl = RTS_CONTROL_DISABLE;
    NewDCB.fAbortOnError = 0;
    /*NewDCB.XonLim*/
    /*NewDCB.XoffLim*/
    NewDCB.ByteSize = static_cast<BYTE>(iByteSize);
    NewDCB.Parity = static_cast<BYTE>(iParity);
    NewDCB.StopBits = static_cast<BYTE>(iStopBits);
    /*NewDCB.XonChar*/
    /*NewDCB.XoffChar*/
    NewDCB.ErrorChar = '?';
    /*NewDCB.EofChar*/
    /*NewDCB.EvtChar*/

    /* function reinitializes all hardware and control settings, but it does not empty output or input queues */
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    if (!SetCommState(hCommPort, &NewDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      CloseHandle(hCommPort);
      hb_retnl(-1);
      return;
    }

    s_PortData[iPort].hPort = hCommPort;
    hb_retnl(hCommPort == INVALID_HANDLE_VALUE ? -1 : 0);
  }
  else
  {
    hb_retnl(-1);
  }
}

HB_FUNC(WIN_COMCLOSE)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    long lDrain = hb_parnl(2);

    if (lDrain > 0)
    {
      Sleep(lDrain * 1000);
    }

    s_PortData[iPort].hPort = INVALID_HANDLE_VALUE;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_CLOSEHANDLE;
    s_PortData[iPort].dwError = 0;

    hb_retl(CloseHandle(hCommPort) != 0);
    s_PortData[iPort].dwError = GetLastError();
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMWRITE)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    auto lpBuffer = hb_parcx(2);
    auto dwNumberofBytesToWrite = static_cast<DWORD>(hb_parclen(2));
    DWORD dwNumberofBytesWritten;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_WRITEFILE;
    s_PortData[iPort].dwError = 0;
    if (WriteFile(hCommPort, lpBuffer, dwNumberofBytesToWrite, &dwNumberofBytesWritten, nullptr))
    {
      hb_retnl(dwNumberofBytesWritten);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retnl(-1);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMREAD)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    auto dwNumberOfBytesToRead = static_cast<DWORD>(hb_parclen(2));
    DWORD dwNumberOfBytesRead;

    auto lpBuffer = static_cast<char *>(hb_xgrab(dwNumberOfBytesToRead + 1));
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_READFILE;
    s_PortData[iPort].dwError = 0;
    if (ReadFile(hCommPort, lpBuffer, dwNumberOfBytesToRead, &dwNumberOfBytesRead, nullptr))
    {
      if (!hb_storclen_buffer(lpBuffer, dwNumberOfBytesRead, 2))
      {
        hb_xfree(lpBuffer);
      }
      hb_retnl(dwNumberOfBytesRead);
    }
    else
    {
      hb_storc(nullptr, 2);
      hb_xfree(lpBuffer);
      s_PortData[iPort].dwError = GetLastError();
      hb_retnl(-1);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMRECV)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    auto dwNumberOfBytesToRead = static_cast<DWORD>(hb_parnl(2));
    DWORD dwNumberOfBytesRead;

    auto lpBuffer = static_cast<char *>(hb_xgrab(dwNumberOfBytesToRead + 1));
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_READFILE;
    s_PortData[iPort].dwError = 0;
    if (ReadFile(hCommPort, lpBuffer, dwNumberOfBytesToRead, &dwNumberOfBytesRead, nullptr))
    {
      hb_retclen_buffer(lpBuffer, dwNumberOfBytesRead);
      hb_stornl(dwNumberOfBytesRead, 3);
    }
    else
    {
      hb_retc_null();
      hb_xfree(lpBuffer);
      s_PortData[iPort].dwError = GetLastError();
      hb_stornl(-1, 3);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMSTATUS)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DWORD dwModemStat = 0;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMMODEMSTATUS;
    s_PortData[iPort].dwError = 0;
    if (GetCommModemStatus(hCommPort, &dwModemStat))
    {
      hb_storl((dwModemStat & MS_CTS_ON) != 0, 2);  /* The CTS (clear-to-send) signal is on. */
      hb_storl((dwModemStat & MS_DSR_ON) != 0, 3);  /* The DSR (data-set-ready) signal is on. */
      hb_storl((dwModemStat & MS_RING_ON) != 0, 4); /* The ring indicator signal is on. */
      hb_storl((dwModemStat & MS_RLSD_ON) != 0,
               5); /* The RLSD (receive-line-signal-detect) signal is on. Also is DCD. */

      hb_retl(true);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();

      hb_storl(false, 2);
      hb_storl(false, 3);
      hb_storl(false, 4);
      hb_storl(false, 5);

      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMPURGE)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DWORD dwFlags;

    dwFlags = (hb_parl(2) ? PURGE_RXCLEAR : 0) | (hb_parl(3) ? PURGE_TXCLEAR : 0);
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_PURGECOMM;
    s_PortData[iPort].dwError = 0;
    if (PurgeComm(hCommPort, dwFlags))
    {
      hb_retl(true);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMQUEUESTATUS)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DWORD dwErrors = 0;
    COMSTAT ComStat;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_CLEARCOMMERROR;
    s_PortData[iPort].dwError = 0;
    if (ClearCommError(hCommPort, &dwErrors, &ComStat))
    {
      hb_storl(ComStat.fCtsHold, 2);
      hb_storl(ComStat.fDsrHold, 3);
      hb_storl(ComStat.fRlsdHold, 4);
      hb_storl(ComStat.fXoffHold, 5);
      hb_storl(ComStat.fXoffSent, 6);
      hb_stornl(ComStat.cbInQue, 7);
      hb_stornl(ComStat.cbOutQue, 8); /* This value will be zero for a nonoverlapped write */

      hb_retl(true);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();

      hb_storl(false, 2);
      hb_storl(false, 3);
      hb_storl(false, 4);
      hb_storl(false, 5);
      hb_storl(false, 6);
      hb_stornl(0, 7);
      hb_stornl(0, 8);

      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC(WIN_COMSETRTS)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DWORD dwFunc = hb_parl(2) ? SETRTS : CLRRTS;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_ESCAPECOMMFUNCTION;
    s_PortData[iPort].dwError = 0;
    if (EscapeCommFunction(hCommPort, dwFunc))
    {
      hb_retl(true);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* If handshaking is enabled, it is an error for the application to adjust the line by
   using the EscapeCommFunction function */

HB_FUNC(WIN_COMSETDTR)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DWORD dwFunc = hb_parl(2) ? SETDTR : CLRDTR;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_ESCAPECOMMFUNCTION;
    s_PortData[iPort].dwError = 0;
    if (EscapeCommFunction(hCommPort, dwFunc))
    {
      hb_retl(true);
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMRTSFLOW)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DCB CurDCB;
    auto iRtsControl = hb_parni(2);

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    CurDCB.DCBlength = sizeof(DCB);
    if (!GetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
      return;
    }

    if (iRtsControl == RTS_CONTROL_DISABLE)
    {
      CurDCB.fOutxCtsFlow = 0;
      CurDCB.fRtsControl = RTS_CONTROL_DISABLE;
    }
    else if (iRtsControl == RTS_CONTROL_ENABLE)
    {
      CurDCB.fOutxCtsFlow = 1;
      CurDCB.fRtsControl = RTS_CONTROL_ENABLE;
    }
    else if (iRtsControl == RTS_CONTROL_HANDSHAKE)
    {
      CurDCB.fOutxCtsFlow = 1;
      CurDCB.fRtsControl = RTS_CONTROL_HANDSHAKE;
    }
    else
    { /* RTS_CONTROL_TOGGLE - RS485? */
      hb_retl(false);
      return;
    }

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    if (!SetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
    else
    {
      hb_retl(true);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMDTRFLOW)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DCB CurDCB;
    auto DtrControl = hb_parni(2);

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    CurDCB.DCBlength = sizeof(DCB);
    if (!GetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
      return;
    }

    if (DtrControl == DTR_CONTROL_DISABLE)
    {
      CurDCB.fOutxDsrFlow = 0;
      CurDCB.fDtrControl = DTR_CONTROL_DISABLE;
    }
    else if (DtrControl == DTR_CONTROL_ENABLE)
    {
      CurDCB.fOutxDsrFlow = 1;
      CurDCB.fDtrControl = DTR_CONTROL_ENABLE;
    }
    else if (DtrControl == DTR_CONTROL_HANDSHAKE)
    {
      CurDCB.fOutxDsrFlow = 1;
      CurDCB.fDtrControl = DTR_CONTROL_HANDSHAKE;
    }
    else
    {
      hb_retl(false);
      return;
    }

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    if (!SetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
    else
    {
      hb_retl(true);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMXONXOFFFLOW)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DCB CurDCB;

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    CurDCB.DCBlength = sizeof(DCB);
    if (!GetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
      return;
    }

    if (hb_parl(2))
    {
      CurDCB.fInX = 1;
      CurDCB.fOutX = 1;
    }
    else
    {
      CurDCB.fInX = 0;
      CurDCB.fOutX = 0;
    }

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    if (!SetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
    else
    {
      hb_retl(true);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

static int hb_win_ComSetTimeouts(HANDLE hCommPort, LPCOMMTIMEOUTS Timeouts, DWORD dwBaudRate, int iParity,
                                 int iByteSize, int iStopBits)
{
  COMMTIMEOUTS NewTimeouts;

  /* Maximum time, in milliseconds, allowed to elapse between the arrival of two characters on
     the communications line. During a ReadFile operation, the time period begins when the first
     character is received. If the interval between the arrival of any two characters exceeds this
     amount, the ReadFile operation is completed and any buffered data is returned. A value of zero
     indicates that interval time-outs are not used. */

  /* A value of MAXDWORD, combined with zero values for both the ReadTotalTimeoutConstant and
     ReadTotalTimeoutMultiplier members, specifies that the read operation is to return
     immediately with the characters that have already been received, even if no characters
     have been received. */
  NewTimeouts.ReadIntervalTimeout =
      (Timeouts->ReadIntervalTimeout == static_cast<DWORD>(-1) ? MAXDWORD : Timeouts->ReadIntervalTimeout);

  /* Multiplier, in milliseconds, used to calculate the total time-out period for read operations.
     For each read operation, this value is multiplied by the requested number of bytes to be read. */
  NewTimeouts.ReadTotalTimeoutMultiplier =
      (Timeouts->ReadTotalTimeoutMultiplier == static_cast<DWORD>(-1) ? 0 : Timeouts->ReadTotalTimeoutMultiplier);

  /* Constant, in milliseconds, used to calculate the total time-out period for read operations.
     For each read operation, this value is added to the product of the ReadTotalTimeoutMultiplier
     member and the requested number of bytes. */
  NewTimeouts.ReadTotalTimeoutConstant =
      (Timeouts->ReadTotalTimeoutConstant == static_cast<DWORD>(-1) ? 0 : Timeouts->ReadTotalTimeoutConstant);

  /* A value of zero for both the ReadTotalTimeoutMultiplier and ReadTotalTimeoutConstant members
     indicates that total time-outs are not used for read operations ...
     and MAXDWORD, 0 and 0 are what we use by default */

  /* Multiplier, in milliseconds, used to calculate the total time-out period for write operations.
     For each write operation, this value is multiplied by the number of bytes to be written. */
  if (Timeouts->WriteTotalTimeoutMultiplier == static_cast<DWORD>(-1))
  {
    /* float of 1.0 makes whole expression float */
    NewTimeouts.WriteTotalTimeoutMultiplier =
        HB_MIN(1, static_cast<DWORD>((1.0 / dwBaudRate) *
                                     (iByteSize + 1 + (iParity == NOPARITY ? 0 : 1) +
                                      (iStopBits == ONESTOPBIT     ? 1
                                       : iStopBits == ONE5STOPBITS ? 1.5
                                                                   : 2)) *
                                     1000));
  }
  /* Constant, in milliseconds, used to calculate the total time-out period for write operations.
     For each write operation, this value is added to the product of the WriteTotalTimeoutMultiplier member and the
     number of bytes to be written. */
  else
  {
    NewTimeouts.WriteTotalTimeoutMultiplier = Timeouts->WriteTotalTimeoutMultiplier;
  }

  /* 50 ms is a thumbsuck - seems long enough and not too long! */
  NewTimeouts.WriteTotalTimeoutConstant =
      Timeouts->WriteTotalTimeoutConstant == static_cast<DWORD>(-1) ? 50 : Timeouts->WriteTotalTimeoutConstant;

  /* A value of zero for both the WriteTotalTimeoutMultiplier and WriteTotalTimeoutConstant members
     indicates that total time-outs are not used for write operations ...
     and if flow control is enabled the program will "hang" or if it is not enabled the data will
     be lost (potentially), so we set a minimum of 1ms (baud rates higher than 4800) */

  return SetCommTimeouts(hCommPort, &NewTimeouts);
}

HB_FUNC(WIN_COMSETTIMEOUTS)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    DCB CurDCB;
    COMMTIMEOUTS Timeouts;

    Timeouts.ReadIntervalTimeout = static_cast<DWORD>(hb_parnldef(2, -1));
    Timeouts.ReadTotalTimeoutMultiplier = static_cast<DWORD>(hb_parnldef(3, -1));
    Timeouts.ReadTotalTimeoutConstant = static_cast<DWORD>(hb_parnldef(4, -1));
    Timeouts.WriteTotalTimeoutMultiplier = static_cast<DWORD>(hb_parnldef(5, -1));
    Timeouts.WriteTotalTimeoutConstant = static_cast<DWORD>(hb_parnldef(6, -1));

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMSTATE;
    s_PortData[iPort].dwError = 0;

    CurDCB.DCBlength = sizeof(DCB);
    if (!GetCommState(hCommPort, &CurDCB))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
      return;
    }

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETCOMMTIMEOUTS;
    s_PortData[iPort].dwError = 0;
    if (!hb_win_ComSetTimeouts(hCommPort, &Timeouts, CurDCB.BaudRate, CurDCB.Parity, CurDCB.ByteSize, CurDCB.StopBits))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
    else
    {
      hb_retl(true);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMSETQUEUESIZE)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE && HB_ISNUM(2) && HB_ISNUM(3))
  {
    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_SETUPCOMM;
    s_PortData[iPort].dwError = 0;
    if (!SetupComm(hCommPort, hb_parni(2), hb_parni(3)))
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retl(false);
    }
    else
    {
      hb_retl(true);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMISVALID)
{
  auto iPort = hb_parni(1);

  hb_retl(iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)));
}

HB_FUNC(WIN_COMERRORCLEAR)
{
  auto iPort = hb_parni(1);

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)))
  {
    s_PortData[iPort].dwError = 0;
    s_PortData[iPort].iFunction = 0;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMERROR)
{
  auto iPort = hb_parni(1);

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)))
  {
    hb_retnl(s_PortData[iPort].dwError);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMFUNCLAST)
{
  auto iPort = hb_parni(1);

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)))
  {
    hb_retni(s_PortData[iPort].iFunction);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(WIN_COMDEBUGDCB)
{
  auto iPort = hb_parni(1);
  HANDLE hCommPort;

  if (iPort >= 0 && iPort < static_cast<int>(HB_SIZEOFARRAY(s_PortData)) &&
      (hCommPort = s_PortData[iPort].hPort) != INVALID_HANDLE_VALUE)
  {
    auto iDebugLevel = hb_parnidef(2, HB_WIN_COM_DBGBASIC);
    DCB CurDCB;
    COMMTIMEOUTS CurCOMMTIMEOUTS;
    COMMPROP CurCOMMPROP;
    char szDebugString[1024] = "";
    char buffer[80];

    s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMSTATE;
    s_PortData[iPort].dwError = 0;
    CurDCB.DCBlength = sizeof(DCB);
    if (GetCommState(hCommPort, &CurDCB))
    {
      if (iDebugLevel & HB_WIN_COM_DBGBASIC)
      {
        hb_snprintf(buffer, sizeof(buffer), "Baud     : %lu\n", CurDCB.BaudRate);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "ByteSize : %i\n", CurDCB.ByteSize);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "Parity   : %i\n", CurDCB.Parity);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "StopBits : %i\n", CurDCB.StopBits);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
      }
      if (iDebugLevel & HB_WIN_COM_DBGFLOW)
      {
        hb_strncat(szDebugString, "fRtsControl : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString,
                   CurDCB.fRtsControl == RTS_CONTROL_DISABLE     ? "RTS_CONTROL_DISABLE\n"
                   : CurDCB.fRtsControl == RTS_CONTROL_ENABLE    ? "RTS_CONTROL_ENABLE\n"
                   : CurDCB.fRtsControl == RTS_CONTROL_HANDSHAKE ? "RTS_CONTROL_HANDSHAKE\n"
                                                                 : "RTS_CONTROL_TOGGLE\n",
                   sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fOutxCtsFlow : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fOutxCtsFlow ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fOutX : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fOutX ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fInX : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fInX ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fDtrControl : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString,
                   CurDCB.fDtrControl == DTR_CONTROL_DISABLE  ? "DTR_CONTROL_DISABLE\n"
                   : CurDCB.fDtrControl == DTR_CONTROL_ENABLE ? "DTR_CONTROL_ENABLE\n"
                                                              : "DTR_CONTROL_HANDSHAKE\n",
                   sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fOutxDsrFlow : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fOutxDsrFlow ? "true\n" : "false\n", sizeof(szDebugString) - 1);
      }
      if (iDebugLevel & HB_WIN_COM_DBGXTRAFLOW)
      {
        hb_strncat(szDebugString, "fDsrSensitivity : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fDsrSensitivity ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fTXContinueOnXoff : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fTXContinueOnXoff ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "XonLim : %i\n", CurDCB.XonLim);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "XoffLim : %i\n", CurDCB.XoffLim);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "XonChar : 0x%i\n", CurDCB.XonChar);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "XoffChar : 0x%i\n", CurDCB.XoffChar);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
      }
      if (iDebugLevel & HB_WIN_COM_DBGOTHER)
      {
        hb_strncat(szDebugString, "fBinary : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fBinary ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fParity : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fParity ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fErrorChar : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fErrorChar ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fNull : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fNull ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, "fAbortOnError : ", sizeof(szDebugString) - 1);
        hb_strncat(szDebugString, CurDCB.fAbortOnError ? "true\n" : "false\n", sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "ErrorChar : 0x%i\n", CurDCB.ErrorChar);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "EofChar : 0x%i\n", CurDCB.EofChar);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "EvtChar : 0x%i\n", CurDCB.EvtChar);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
      }
    }
    else
    {
      s_PortData[iPort].dwError = GetLastError();
      hb_retc_null();
      return;
    }

    if (iDebugLevel & HB_WIN_COM_DBGTIMEOUTS)
    {
      s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMTIMEOUTS;
      s_PortData[iPort].dwError = 0;
      if (GetCommTimeouts(hCommPort, &CurCOMMTIMEOUTS))
      {
        hb_snprintf(buffer, sizeof(buffer), "ReadIntervalTimeout : %lu\n", CurCOMMTIMEOUTS.ReadIntervalTimeout);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "ReadTotalTimeoutMultiplier : %ld\n",
                    CurCOMMTIMEOUTS.ReadTotalTimeoutMultiplier);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "ReadTotalTimeoutConstant : %ld\n",
                    CurCOMMTIMEOUTS.ReadTotalTimeoutConstant);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "WriteTotalTimeoutMultiplier : %ld\n",
                    CurCOMMTIMEOUTS.WriteTotalTimeoutMultiplier);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "WriteTotalTimeoutConstant : %ld\n",
                    CurCOMMTIMEOUTS.WriteTotalTimeoutConstant);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
      }
      else
      {
        s_PortData[iPort].dwError = GetLastError();
        hb_retc_null();
        return;
      }
    }

    if (iDebugLevel & HB_WIN_COM_DBGQUEUE)
    {
      s_PortData[iPort].iFunction = HB_WIN_COM_FUN_GETCOMMPROPERTIES;
      s_PortData[iPort].dwError = 0;
      if (GetCommProperties(hCommPort, &CurCOMMPROP))
      {
        hb_snprintf(buffer, sizeof(buffer), "dwCurrentTxQueue : %lu\n", CurCOMMPROP.dwCurrentTxQueue);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
        hb_snprintf(buffer, sizeof(buffer), "dwCurrentRxQueue : %lu\n", CurCOMMPROP.dwCurrentRxQueue);
        hb_strncat(szDebugString, buffer, sizeof(szDebugString) - 1);
      }
      else
      {
        s_PortData[iPort].dwError = GetLastError();
        hb_retc_null();
        return;
      }
    }

    hb_retc(szDebugString);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_CALL_ON_STARTUP_BEGIN(_hb_wincom_init_)
hb_wincom_init();
HB_CALL_ON_STARTUP_END(_hb_wincom_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup _hb_wincom_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY HB_DATASEG_FUNC(_hb_wincom_init_)
#include "hbiniseg.hpp"
#endif
