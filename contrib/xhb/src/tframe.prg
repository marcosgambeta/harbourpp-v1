//
// HTMLLIB Frame Class
//
// Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
// Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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
#include "cgi.ch"


CREATE CLASS THtmlFrameSet

   VAR nH
   VAR FName
   VAR cStr INIT ""

   VAR TITLE INIT "FrameSet01"

   METHOD New(cFName, cTitle)
   METHOD StartSet(aRows, aCols, onLoad, onUnload)
   METHOD EndSet()
   METHOD End()
   METHOD Frame(cName, cURL, lBorder, lResize, lScrolling, marginwidth, marginheight, cTarget, cScrolling)

ENDCLASS


METHOD THtmlFrameSet:New(cFName, cTitle)

   LOCAL cStr

   __defaultNIL(@cTitle, "")

   ::FName := cFName
   ::Title := cTitle

   IF hb_IsString(::FName)
      cStr := ""
      ::nH := FCreate(::FName)
   ELSE
      cStr := "Content-Type: text/html" + CRLF() + CRLF()
      ::nH := STD_OUT
   ENDIF

   cStr += "<html>" + CRLF() + ;
      " <head>" + CRLF() + ;
      "  <title>" + ::Title + "</title>" + CRLF() + ;
      " </head>" + CRLF()

   ::cStr += cStr

   RETURN Self

METHOD THtmlFrameSet:StartSet(aRows, aCols, onLoad, onUnload)

   LOCAL cStr
   LOCAL cItem

   cStr := CRLF() + " <frameset "

   IF hb_IsArray(aRows) .AND. !Empty(aRows)

      cStr += ' rows="'

      FOR EACH cItem in aRows
         IF cItem:__enumIndex() > 1
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF hb_IsArray(aCols) .AND. !Empty(aCols)

      cStr += ' cols="'

      FOR EACH cItem IN aCols
         IF cItem:__enumIndex() > 1
            cStr += ","
         ENDIF
         cStr += cItem
      NEXT

      cStr += '"'
   ENDIF

   IF hb_IsString(onLoad)
      cStr += Space(7) + ' onLoad="' + onLoad + '"'
   ENDIF

   IF hb_IsString(onUnLoad)
      cStr += Space(5) + ' onUnLoad="' + onUnLoad + '"'
   ENDIF

   cStr += " >" + CRLF()

   ::cStr += cStr

   RETURN Self


METHOD THtmlFrameSet:Endset()

   ::cStr += " </frameset>" + CRLF()

   RETURN Self


METHOD THtmlFrameSet:End()

   ::cStr += "</html>" + CRLF()

   FWrite(::nH, ::cStr)

   IF ::FName != NIL
      FClose(::nH)
   ENDIF

   RETURN Self


METHOD THtmlFrameSet:Frame(cName, cURL, lBorder, lResize, lScrolling, marginwidth, marginheight, cTarget, cScrolling)

   LOCAL cStr

   __defaultNIL(@lBorder, .T.)
   __defaultNIL(@lResize, .T.)
   __defaultNIL(@lScrolling, .F.)
   __defaultNIL(@cScrolling, "AUTO")
   __defaultNIL(@cTarget, "_self")

   cStr := "  <frame "

   IF hb_IsString(cName)
      cStr += ' name="' + cName + '"'
   ENDIF

   IF hb_IsString(cUrl)
      cStr += ' src="' + cURL + '"'
   ENDIF

   IF hb_IsString(cTarget)
      cStr += ' target="' + cTarget + '"'
   ENDIF

   IF !lBorder
      cStr += ' frameborder="0"'
   ELSE
      cStr += ' frameborder="1"'
   ENDIF

   IF !lResize
      cStr += " noresize"
   ENDIF

   IF hb_IsString(cScrolling)
      cStr += ' scrolling="' + cScrolling + '"'
   ELSE
      IF lScrolling != NIL
         cStr += ' scrolling=' + IIf(lScrolling, '"yes"', '"no"')
      ELSE
         cStr += ' scrolling="auto"'
      ENDIF
   ENDIF

   IF hb_IsNumeric(marginwidth)
      cStr += " marginwidth= " + hb_ntos(marginwidth)
   ENDIF

   IF hb_IsNumeric(marginheight)
      cStr += " marginheight= " + hb_ntos(marginheight)
   ENDIF

   cStr += ">" + CRLF()

   ::cStr += cStr

   RETURN Self
