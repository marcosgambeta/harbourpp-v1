//
// Xbase++ compatible classes to manage threads
//
// Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// Special thanks for Pritpal Bedi for class skeleton with info about
// Xbase++ and to other contributors which I hope will finish and fix
// this code
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

#include <hbclass.ch>
#include <hbthread.ch>

#include "thread.ch"


CREATE CLASS Signal

   VAR cargo      AS USUAL EXPORTED
   VAR mutex      AS USUAL PROTECTED

EXPORTED:

   METHOD new(...)
   METHOD wait( nTimeOut )
   METHOD signal()

ENDCLASS

METHOD Signal:new(...)

   ::mutex := hb_mutexCreate()
   ::Init(...)

   RETURN Self

METHOD Signal:wait( nTimeOut )
   RETURN __clsSyncWait( ::mutex, nTimeOut )

METHOD Signal:signal()

   __clsSyncSignal( ::mutex )

   RETURN Self


CREATE CLASS Thread

EXPORTED:
   VAR active           AS LOGICAL READONLY  INIT .F.
   VAR atEnd            AS USUAL             INIT NIL
   VAR atStart          AS USUAL             INIT NIL
   VAR cargo            AS USUAL
   VAR deltaTime        AS NUMERIC READONLY  INIT 0
   VAR interval         AS USUAL   READONLY  INIT NIL
   VAR priority         AS NUMERIC READONLY  INIT 0
   VAR result           AS USUAL             INIT NIL
   VAR startCount       AS NUMERIC READONLY  INIT 0
   VAR startTime        AS USUAL   READONLY  INIT NIL
   VAR threadID         AS NUMERIC READONLY  INIT 0

PROTECTED:
   VAR maxStackSize     AS USUAL             INIT 50000

HIDDEN:
   VAR pThreadID        AS USUAL             INIT NIL

EXPORTED:
   METHOD new(...)

PROTECTED:
   /* METHOD atEnd() */
   /* METHOD atStart() */
   METHOD execute()

EXPORTED:
   METHOD quit( xResult, nRestart )
   METHOD setInterval(nHSeconds)
   METHOD setPriority( nPriority )
   METHOD setStartTime( nSeconds )
   METHOD start(xAction, ...)
   METHOD synchronize( nTimeOut )

   METHOD threadSelf()

ENDCLASS

METHOD Thread:new(...)

   LOCAL nMaxStackSize

   IF PCount() == 1
      nMaxStackSize := hb_PValue(1)
      IF HB_IsNumeric(nMaxStackSize)
         ::maxStackSize := nMaxStackSize
      ENDIF
      /* TODO: Create new thread here and suspend its execution
       *       Then :START() method only resumes this thread instead
       *       of creating new one.
       *       xBase++ seems to work in such way.
       */

      /* TODO: do not ignore thread stack size set by user in ::maxStackSize */
   ENDIF

   ::Init(...)

   RETURN Self

METHOD PROCEDURE Thread:execute()

   HB_SYMBOL_UNUSED(Self)

   RETURN

METHOD PROCEDURE Thread:quit( xResult, nRestart )

   IF hb_threadSelf() == ::pThreadID
      IF PCount() > 0
         ::result := xResult
      ENDIF
      IF !HB_IsNumeric(nRestart) .OR. nRestart != QUIT_RESTART
         ::interval := NIL
      ENDIF
      QUIT
   ENDIF

   RETURN

METHOD Thread:setInterval(nHSeconds)

   IF HB_IsNumeric(nHSeconds) .AND. Int(nHSeconds) >= 0
      ::interval := Int(nHSeconds)
   ELSEIF PCount() > 0 .OR. nHSeconds == NIL
      ::interval := NIL
   ELSE
      /* TODO: RT Error */
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Thread:setPriority( nPriority )

   /* TODO: add thread priority setting */
   IF HB_IsNumeric(nPriority)
      ::priority := nPriority
   ENDIF

   RETURN .F.

METHOD Thread:setStartTime( nSeconds )

   IF HB_IsNumeric(nSeconds)
      IF nSeconds < 0 .OR. nSeconds > 86400
         RETURN .F.
      ENDIF
      ::startTime := nSeconds
   ELSEIF PCount() > 0 .OR. nSeconds == NIL
      ::startTime := NIL
   ELSE
      /* TODO: RT Error */
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Thread:start(xAction, ...)

   IF ::active
      RETURN .F.
   ELSE
      ::pThreadID := hb_threadStart(HB_THREAD_INHERIT_PUBLIC, ;
            {| ... |
               LOCAL nTime

               ThreadObject( Self )

               ::active := .T.
               ::startCount++

               IF HB_IsNumeric(::startTime)
                  nTime := ::startTime - Seconds()
                  IF nTime < 0
                     nTime += 86400
                  ENDIF
                  hb_idleSleep( nTime )
                  ::startTime := NIL
               ENDIF

               ::atStart(...)
               IF HB_IsBlock(::_atStart)
                  Eval(::_atStart, ...)
               ENDIF

               DO WHILE .T.

                  nTime := hb_MilliSeconds()

                  BEGIN SEQUENCE
                     IF !Empty(xAction) .AND. ValType(xAction) $ "CBS"
                        ::result := Do(xAction, ...)
                     ELSE
                        ::result := ::execute(...)
                     ENDIF
                  ALWAYS
                     __QuitCancel()
                  END SEQUENCE

                  nTime := Int(( hb_MilliSeconds() - nTime ) / 10)
                  ::deltaTime := nTime

                  /* TODO: when ::startTime is set execution is suspended
                   *       but I do not know the exact conditions and how
                   *       it can be resumed
                   */

                  IF !HB_IsNumeric(::interval)
                     EXIT
                  ENDIF

                  nTime := ::interval - ::deltaTime
                  IF nTime > 0
                     hb_idleSleep( nTime / 100 )
                  ENDIF
                  ::startCount++

               ENDDO

               ::atEnd(...)
               IF HB_IsBlock(::_atEnd)
                  Eval(::_atEnd, ...)
               ENDIF
               ::active := .F.

               RETURN NIL
            }, ...)

      ::threadID := IIf(::pThreadID == NIL, 0, hb_threadID( ::pThreadID ))
   ENDIF

   RETURN .T.

METHOD Thread:synchronize( nTimeOut )

   LOCAL pThreadID := ::pThreadID

   IF hb_threadSelf() != pThreadID
      RETURN hb_threadWait( pThreadID, ;
         IIf(HB_IsNumeric(nTimeOut) .AND. nTimeOut != 0, ;
         nTimeOut / 100, NIL) )
   ENDIF

   RETURN .F.

METHOD Thread:threadSelf()
   RETURN ::pThreadID

#if 0
METHOD Thread:threadID()

   LOCAL pThreadID := ::pThreadID

   RETURN IIf(pThreadID == NIL, 0, hb_threadID( pThreadID ))
#endif
