//
// The default error handler
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "error.ch"

Procedure ErrorSys()

   ErrorBlock({|oError|DefError(oError)})

   Return

Static Function DefError(oError)

   Local cMessage
   Local cOSError
   Local aOptions
   Local nChoice
   Local n

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV .AND. oError:canSubstitute
      Return 0
   ENDIF

   // By default, retry on RDD lock error failure
   IF oError:genCode == EG_LOCK .AND. oError:canRetry
      // oError:tries++
      Return True
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. oError:osCode == 32 .AND. oError:canDefault
      NetErr(True)
      Return False
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. oError:canDefault
      NetErr(True)
      Return False
   ENDIF

   cMessage := ErrorMessage(oError)
   IF !Empty(oError:osCode)
      cOSError := hb_StrFormat("(DOS Error %1$d)", oError:osCode)
   ENDIF

   // Build buttons

   aOptions := {}

   AAdd(aOptions, "Quit")

   IF oError:canRetry
      AAdd(aOptions, "Retry")
   ENDIF

   IF oError:canDefault
      AAdd(aOptions, "Default")
   ENDIF

   // Show alert box

   Do While (nChoice := Alert(cMessage + IIf(cOSError == NIL, "", ";" + cOSError), aOptions)) == 0
   EndDo

   IF !Empty(nChoice)  // Alert() may return NIL
      Switch aOptions[nChoice]
      Case "Break"
         Break(oError)
      Case "Retry"
         Return True
      Case "Default"
         Return False
      EndSwitch
   ENDIF

   // "Quit" selected

   IF cOSError != NIL
      cMessage += " " + cOSError
   ENDIF

   OutErr(hb_eol())
   OutErr(cMessage)

   n := 1
   Do While !Empty(ProcName(++n))
      OutErr(hb_eol())
      OutErr(hb_StrFormat("Called from %1$s(%2$d)  ", ProcName(n), ProcLine(n)))
   EndDo

   ErrorLevel(1)
   QUIT

   Return False

Static Function ErrorMessage(oError)

   // start error message
   Local cMessage := IIf(oError:severity > ES_WARNING, "Error", "Warning") + " "

   // add subsystem name if available
   IF hb_IsString(oError:subsystem)
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF hb_IsNumeric(oError:subCode)
      cMessage += "/" + hb_ntos(oError:subCode)
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF hb_IsString(oError:description)
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   Do Case
   Case !Empty(oError:filename)
      cMessage += ": " + oError:filename
   Case !Empty(oError:operation)
      cMessage += ": " + oError:operation
   EndCase

   Return cMessage
