//
// OLE Automation object
//
// Copyright 2008, 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#define HB_CLS_NOTOBJECT  // avoid definition of method: INIT
#include <hbclass.ch>

CREATE CLASS win_oleAuto

   VAR __hObj
   VAR __hObjEnum
   VAR __hSink
   VAR __cargo

   METHOD __enumStart(enum, lDescend)
   METHOD __enumSkip(enum, lDescend)
   METHOD __enumStop()

   METHOD __OpIndex(xIndex, xValue) OPERATOR "[]"

   ERROR HANDLER __OnError()

ENDCLASS

METHOD win_oleAuto:__enumStart(enum, lDescend)

   LOCAL hObjEnum

   IF !Empty(hObjEnum := __oleEnumCreate(::__hObj, lDescend))
      IF !Empty(::__hObjEnum)
         // small hack - clone the object array for nested FOR EACH calls
         Self := __objClone(Self)
      ENDIF
      ::__hObjEnum := hObjEnum
      // set base value for enumerator
      (@enum):__enumBase(Self)
      RETURN ::__enumSkip(@enum, lDescend)
   ENDIF

   RETURN .F.

METHOD win_oleAuto:__enumSkip(enum, lDescend)

   LOCAL lContinue
   LOCAL xValue

   HB_SYMBOL_UNUSED(lDescend)

   xValue := __oleEnumNext(::__hObjEnum, @lContinue, ::classH)
   // set enumerator value
   (@enum):__enumValue(xValue)

   RETURN lContinue

METHOD PROCEDURE win_oleAuto:__enumStop()

   ::__hObjEnum := NIL     // activate auto-destructor

   RETURN

// OLE functions

FUNCTION win_oleGetActiveObject(...)

   LOCAL oOle
   LOCAL hOle

   IF !Empty(hOle := __oleGetActiveObject(...))
      oOle := win_oleAuto()
      oOle:__hObj := hOle
   ENDIF

   RETURN oOle

FUNCTION win_oleCreateObject(...)

   LOCAL oOle
   LOCAL hOle

   IF !Empty(hOle := __oleCreateObject(...))
      oOle := win_oleAuto()
      oOle:__hObj := hOle
   ENDIF

   RETURN oOle
