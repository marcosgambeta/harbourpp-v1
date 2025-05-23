//
// User input class for debugger
//
// Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#pragma -b-

#define HB_CLS_NOTOBJECT      // do not inherit from HBObject class
#include "hbclass.ch"

#include "inkey.ch"
#include "color.ch"
#include "setcurs.ch"


CREATE CLASS HBDbInput

   HIDDEN:

   VAR nRow    AS INTEGER
   VAR nCol    AS INTEGER
   VAR nWidth  AS INTEGER
   VAR nPos    AS INTEGER     INIT 1
   VAR nFirst  AS INTEGER     INIT 1
   VAR nSize   AS INTEGER
   VAR cValue  AS CHARACTER
   VAR acColor AS ARRAY

   EXPORTED:

   METHOD new(nRow, nCol, nWidth, cValue, cColor, nSize)
   METHOD applyKey(nKey)
   METHOD getValue()
   METHOD setValue(cValue)
   METHOD display()
   METHOD showCursor()
   METHOD newPos(nRow, nCol)
   METHOD setColor(cColor)

ENDCLASS

METHOD HBDbInput:new(nRow, nCol, nWidth, cValue, cColor, nSize)

   ::nRow   := nRow
   ::nCol   := nCol
   ::nWidth := nWidth
   ::nSize  := IIf(hb_IsNumeric(nSize), nSize, nWidth)
   ::cValue := PadR(cValue, ::nSize)

   ::setColor(cColor)

   RETURN Self

METHOD HBDbInput:SetColor(cColor)

   ::acColor := {hb_ColorIndex(cColor, CLR_STANDARD), hb_ColorIndex(cColor, CLR_ENHANCED)}
   IF hb_ColorToN(::acColor[2]) == -1
      ::acColor[2] := ::acColor[1]
   ENDIF

   RETURN Self

METHOD HBDbInput:newPos(nRow, nCol)

   ::nRow := nRow
   ::nCol := nCol

   RETURN Self

METHOD HBDbInput:getValue()
   RETURN ::cValue

METHOD HBDbInput:setValue(cValue)

   ::cValue := PadR(cValue, ::nSize)
   ::nPos := Min(::nSize, Len(RTrim(::cValue)) + 1)

   RETURN Self

METHOD HBDbInput:display()

   IF ::nPos < ::nFirst
      ::nFirst := ::nPos
   ELSEIF ::nPos - ::nFirst >= ::nWidth
      ::nFirst := ::nPos - ::nWidth + 1
   ENDIF
   hb_DispOutAt(::nRow, ::nCol, SubStr(::cValue, ::nFirst, ::nWidth), ::acColor[2])

   RETURN Self

METHOD HBDbInput:showCursor()

   SetPos(::nRow, ::nCol + ::nPos - ::nFirst)
   SetCursor(IIf(Set(_SET_INSERT), SC_INSERT, SC_NORMAL))

   RETURN Self

METHOD HBDbInput:applyKey(nKey)

   LOCAL lUpdate := .T.

   SWITCH nKey
   CASE K_HOME
      ::nPos := 1
      EXIT
   CASE K_END
      ::nPos := Len(RTrim(::cValue)) + 1
      IF ::nPos > ::nSize
         ::nPos := ::nSize
      ENDIF
      EXIT
   CASE K_LEFT
      IF ::nPos > 1
         ::nPos--
      ENDIF
      EXIT
   CASE K_RIGHT
      IF ::nPos < ::nSize
         ::nPos++
      ENDIF
      EXIT
   CASE K_DEL
      ::cValue := Stuff(::cValue, ::nPos, 1, "") + " "
      EXIT
   CASE K_BS
      IF ::nPos > 1
         ::cValue := Stuff(::cValue, --::nPos, 1, "") + " "
      ENDIF
      EXIT
   CASE K_CTRL_Y
   CASE K_CTRL_DEL
      ::cValue := Space(::nSize)
      ::nPos := 1
      EXIT
   CASE K_INS
      Set(_SET_INSERT, !Set(_SET_INSERT))
      EXIT
   OTHERWISE
      IF hb_keyChar(nKey) == ""
         lUpdate := .F.
      ELSE
         IF Set(_SET_INSERT)
            ::cValue := Left(Stuff(::cValue, ::nPos, 0, hb_keyChar(nKey)), ::nSize)
         ELSE
            ::cValue := Stuff(::cValue, ::nPos, 1, hb_keyChar(nKey))
         ENDIF
         IF ::nPos < ::nSize
            ::nPos++
         ENDIF
      ENDIF
   ENDSWITCH

   IF lUpdate
      ::display()
   ENDIF

   RETURN Self
