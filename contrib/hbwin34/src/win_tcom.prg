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

#include <hbclass.ch>
#include "hbwin.ch"

/* The class is a VERY thin layer over the xHarbour functions and the xHarbour functions
   are a VERY thin layer over the Win functions, almost no parameter checking! You get what you
   pay for :)

   I haven't bothered to remember things that you can remember for yourself - the state of DTR (low
   to begin with) or the baud rate for example, you can always sub-class it.

   I've only done the things that I've found useful over the years, for example I never used the
   "BREAK" state of a line so I haven't done it here.

   The class needs the port to be already open to change timeouts and buffer sizes, it doesn't have to
   be that way, but it is.

   Really Windows comms should be done with threads and/or OVERLAPPED I/O - and I haven't. */

CREATE CLASS win_Com

   ACCESS Open()     INLINE ::lOpen
   ACCESS PortName() INLINE ::cPortName

   PROTECT nPort     INIT -1
   PROTECT lOpen     INIT .F.
   PROTECT cPortName INIT ""

   METHOD Init(cPortName, nBaudRate, nParity, nByteSize, nStopBits)
   METHOD QueueSize(nInQueue, nOutQueue)
   METHOD TimeOuts(nReadInterval, nReadMultiplier, nReadConstant, nWriteMultiplier, nWriteConstant)
   METHOD Read(/* @ */ cString, nLength)
   METHOD Recv(nLength, nResult)
   METHOD RecvTo(cDelim, nMaxlen)
   METHOD Write(cString)
   METHOD Status(lCTS, lDSR, lRing, lDCD)
   METHOD QueueStatus(lCTSHold, lDSRHold, lDCDHold, lXoffHold, lXoffSent, nInQueue, nOutQueue)
   METHOD SetRTS(lCTS)
   METHOD SetDTR(lDTR)
   METHOD RTSFlow(nRTS)
   METHOD DTRFlow(nDTR)
   METHOD XonXoffFlow(lXonXoff)
   METHOD Purge(lRXBuffer, lTXBuffer)
   METHOD PurgeRX()
   METHOD PurgeTX()
   METHOD Close(nDrain)
   METHOD DebugDCB(nDebug)
   METHOD ErrorText()
   METHOD Error()

ENDCLASS

METHOD win_Com:Init(cPortName, nBaudRate, nParity, nByteSize, nStopBits)

   ::cPortName := Upper(cPortName)
   IF hb_LeftEq(::cPortName, "COM")
      ::nPort := Val(SubStr(::cPortName, 4)) - 1
      IF win_comOpen(::nPort, nBaudRate, nParity, nByteSize, nStopBits) != -1
         ::lOpen := .T.
      ENDIF
   ENDIF

   RETURN Self

METHOD win_Com:QueueSize(nInQueue, nOutQueue)
   RETURN win_comSetQueueSize(::nPort, nInQueue, nOutQueue)

METHOD win_Com:TimeOuts(nReadInterval, nReadMultiplier, nReadConstant, nWriteMultiplier, nWriteConstant)
   RETURN win_comSetTimeouts(::nPort, nReadInterval, nReadMultiplier, nReadConstant, nWriteMultiplier, nWriteConstant)

METHOD win_Com:Read(/* @ */ cString, nLength)

   cString := Space(nlength)

   RETURN win_comRead(::nPort, @cString)

METHOD win_Com:Recv(nLength, nResult)
   RETURN win_comRecv(::nPort, nLength, @nResult)

METHOD win_Com:RecvTo(cDelim, nMaxlen)

   LOCAL nResult
   LOCAL cRecv := ""
   LOCAL cString

   DO WHILE .T.
      cString := Space(1)
      IF (nResult := win_comRead(::nPort, @cString)) != -1
         IF nResult == 0
            EXIT
         ELSE
            cRecv += cString
            IF HB_IsString(cDelim) .AND. cString == cDelim
               EXIT
            ENDIF
            IF hb_BLen(cRecv) == nMaxlen
               EXIT
            ENDIF
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN cRecv

METHOD win_Com:Write(cString)
   RETURN win_comWrite(::nPort, cString)

METHOD win_Com:Status(lCTS, lDSR, lRing, lDCD)
   RETURN win_comStatus(::nPort, @lCTS, @lDSR, @lRing, @lDCD)

METHOD win_Com:QueueStatus(lCTSHold, lDSRHold, lDCDHold, lXoffHold, lXoffSent, nInQueue, nOutQueue)
   RETURN win_comQueueStatus(::nPort, @lCTSHold, @lDSRHold, @lDCDHold, @lXoffHold, @lXoffSent, @nInQueue, @nOutQueue)

METHOD win_Com:SetRTS(lCTS)
   RETURN win_comSetRTS(::nPort, lCTS) /* boolean return is the status of the call not the line! */

METHOD win_Com:SetDTR(lDTR)
   RETURN win_comSetDTR(::nPort, lDTR) /* boolean return is the status of the call not the line! */

METHOD win_Com:RTSFlow(nRTS)
   RETURN win_comRTSFlow(::nPort, nRTS)

METHOD win_Com:DTRFlow(nDTR)
   RETURN win_comDTRFlow(::nPort, nDTR)

METHOD win_Com:XonXoffFlow(lXonXoff)
   RETURN win_comXonXoffFlow(::nPort, lXonXoff)

METHOD win_Com:Purge(lRXBuffer, lTXBuffer)
   RETURN win_comPurge(::nPort, lRXBuffer, lTXBuffer)

METHOD win_Com:PurgeRX()
   RETURN win_comPurge(::nPort, .T., .F.)

METHOD win_Com:PurgeTX()
   RETURN win_comPurge(::nPort, .F., .T.)

METHOD win_Com:Close(nDrain)
   RETURN win_comClose(::nPort, hb_defaultValue(nDrain, 0))

METHOD win_Com:DebugDCB(nDebug)
   RETURN win_comDebugDCB(::nPort, nDebug)

/* Since the win_Com functions are an amalgamation of Win functions this allows
   you to see what call did the deed when things go wrong. */

METHOD win_Com:ErrorText()

   LOCAL nFcn := win_comFuncLast(::nPort)
   LOCAL cString
   LOCAL nError
   LOCAL cMsg
   LOCAL aWinPortFcns := { ;
      "CreateFile", ;
      "GetCommState", ;
      "SetCommState", ;
      "SetupComm", ;
      "GetCommTimeouts", ;
      "SetCommTimeouts", ;
      "CloseHandle", ;
      "WriteFile", ;
      "ReadFile", ;
      "GetCommModemStatus", ;
      "PurgeComm", ;
      "ClearCommError", ;
      "EscapeCommFunction", ;
      "GetCommProperties"}

   IF nFcn >= 1 .AND. nFcn <= Len(aWinPortFcns)
      cString := aWinPortFcns[nFcn] + "(), "
   ELSE
      cString := "Function number (" + hb_ntos(nFcn) + ") invalid, "
   ENDIF

   /* WinPortError clears the error - don't call it twice */
   cMsg := Space(256)
   wapi_FormatMessage(,, nError := win_comError(::nPort),, @cMsg)

   RETURN cString + "error (" + hb_ntos(nError) + ") : " + cMsg

METHOD win_Com:Error()
   RETURN win_comError(::nPort)
