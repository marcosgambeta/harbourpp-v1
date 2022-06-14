/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbwinuni.h"
#include "winapi.h"

/*
WINBASEAPI WINBOOL WINAPI ClearCommBreak(HANDLE hFile)
*/
HB_FUNC( WINAPI_CLEARCOMMBREAK )
{
  winapi_ret_BOOL(ClearCommBreak(winapi_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI ClearCommError(HANDLE hFile, LPDWORD lpErrors, LPCOMSTAT lpStat)
*/
#if 0
HB_FUNC( WINAPI_CLEARCOMMERROR )
{
  DWORD Errors;
  winapi_ret_BOOL(ClearCommError(winapi_par_HANDLE(1), &Errors, ###));
  winapi_stor_DWORD(Errors, 2);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetupComm(HANDLE hFile, DWORD dwInQueue, DWORD dwOutQueue)
*/
HB_FUNC( WINAPI_SETUPCOMM )
{
  winapi_ret_BOOL(SetupComm(winapi_par_HANDLE(1), winapi_par_DWORD(2), winapi_par_DWORD(3)));
}

/*
WINBASEAPI WINBOOL WINAPI EscapeCommFunction(HANDLE hFile, DWORD dwFunc)
*/
HB_FUNC( WINAPI_ESCAPECOMMFUNCTION )
{
  winapi_ret_BOOL(EscapeCommFunction(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI GetCommConfig(HANDLE hCommDev, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
BOOL GetCommConfig([in] HANDLE hCommDev, [out] LPCOMMCONFIG lpCC, [in, out] LPDWORD lpdwSize)
*/
#if 0
HB_FUNC( WINAPI_GETCOMMCONFIG )
{
  DWORD dwSize = winapi_par_DWORD(3);
  winapi_ret_BOOL(GetCommConfig(winapi_par_HANDLE(1), ###, &dwSize));
  winapi_stor_DWORD(dwSize, 3);
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetCommMask(HANDLE hFile, LPDWORD lpEvtMask)
*/
HB_FUNC( WINAPI_GETCOMMMASK )
{
  DWORD EvtMask;
  winapi_ret_BOOL(GetCommMask(winapi_par_HANDLE(1), &EvtMask));
  winapi_stor_DWORD(EvtMask, 2);
}

/*
WINBASEAPI WINBOOL WINAPI GetCommModemStatus(HANDLE hFile, LPDWORD lpModemStat)
*/
HB_FUNC( WINAPI_GETCOMMMODEMSTATUS )
{
  DWORD ModemStat;
  winapi_ret_BOOL(GetCommModemStatus(winapi_par_HANDLE(1), &ModemStat));
  winapi_stor_DWORD(ModemStat, 2);
}

/*
WINBASEAPI WINBOOL WINAPI GetCommProperties(HANDLE hFile, LPCOMMPROP lpCommProp)
*/
#if 0
HB_FUNC( WINAPI_GETCOMMPROPERTIES )
{
  winapi_ret_BOOL(GetCommProperties(winapi_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetCommState(HANDLE hFile, LPDCB lpDCB)
*/
#if 0
HB_FUNC( WINAPI_GETCOMMSTATE )
{
  winapi_ret_BOOL(GetCommState(winapi_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI GetCommTimeouts(HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/
#if 0
HB_FUNC( WINAPI_GETCOMMTIMEOUTS )
{
  winapi_ret_BOOL(GetCommTimeouts(winapi_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI PurgeComm(HANDLE hFile, DWORD dwFlags)
*/
HB_FUNC( WINAPI_PURGECOMM )
{
  winapi_ret_BOOL(PurgeComm(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommBreak(HANDLE hFile)
*/
HB_FUNC( WINAPI_SETCOMMBREAK )
{
  winapi_ret_BOOL(SetCommBreak(winapi_par_HANDLE(1)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommConfig(HANDLE hCommDev, LPCOMMCONFIG lpCC, DWORD dwSize)
*/
#if 0
HB_FUNC( WINAPI_SETCOMMCONFIG )
{
  winapi_ret_BOOL(SetCommConfig(winapi_par_HANDLE(1), ###, winapi_par_DWORD(3)));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetCommMask(HANDLE hFile, DWORD dwEvtMask)
*/
HB_FUNC( WINAPI_SETCOMMMASK )
{
  winapi_ret_BOOL(SetCommMask(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINBASEAPI WINBOOL WINAPI SetCommState(HANDLE hFile, LPDCB lpDCB)
*/
#if 0
HB_FUNC( WINAPI_SETCOMMSTATE )
{
  winapi_ret_BOOL(SetCommState(winapi_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI SetCommTimeouts(HANDLE hFile, LPCOMMTIMEOUTS lpCommTimeouts)
*/
#if 0
HB_FUNC( WINAPI_SETCOMMTIMEOUTS )
{
  winapi_ret_BOOL(SetCommTimeouts(winapi_par_HANDLE(1), ###));
}
#endif

/*
WINBASEAPI WINBOOL WINAPI TransmitCommChar(HANDLE hFile, char cChar)
*/
HB_FUNC( WINAPI_TRANSMITCOMMCHAR )
{
  winapi_ret_BOOL(TransmitCommChar(winapi_par_HANDLE(1), winapi_par_char(2)));
}

/*
WINBASEAPI WINBOOL WINAPI WaitCommEvent(HANDLE hFile, LPDWORD lpEvtMask, LPOVERLAPPED lpOverlapped)
*/
#if 0
HB_FUNC( WINAPI_WAITCOMMEVENT )
{
  DWORD EvtMask;
  winapi_ret_BOOL(WaitCommEvent(winapi_par_HANDLE(1), &EvtMask, ###));
  winapi_stor_DWORD(EvtMask, 3);
}
#endif
