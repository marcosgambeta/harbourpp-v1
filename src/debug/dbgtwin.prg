//
// The Debugger
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (:Move())
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      // do not inherit from HBObject class
#include "hbclass.ch"

#include "hbmemvar.ch"

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbWindow // Debugger windows and dialogs

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR cCaption
   VAR cBackImage
   VAR cColor
   VAR lFocused     INIT .F.
   VAR bGotFocus
   VAR bLostFocus
   VAR bKeyPressed
   VAR bPainted
   VAR bLButtonDown
   VAR bLDblClick
   VAR lShadow      INIT .F.
   VAR lVisible     INIT .F.
   VAR Cargo
   VAR Browser

   METHOD New(nTop, nLeft, nBottom, nRight, cCaption, cColor)

   METHOD Hide()
   METHOD IsOver(nRow, nCol)
   METHOD nWidth() INLINE ::nRight - ::nLeft + 1
   METHOD Clear()
   METHOD ScrollUp(nLines)
   METHOD SetCaption(cCaption)
   METHOD ShowCaption()
   METHOD SetFocus(lOnOff)
   METHOD Show(lFocused)
   METHOD ShowModal()
   METHOD LButtonDown(nMRow, nMCol)
   METHOD LDblClick(nMRow, nMCol)
   METHOD LoadColors()

   METHOD Move()
   METHOD KeyPressed(nKey)
   METHOD Refresh()
   METHOD Resize(nTop, nLeft, nBottom, nRight)

ENDCLASS

METHOD HBDbWindow:New(nTop, nLeft, nBottom, nRight, cCaption, cColor)

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight
   ::cCaption := cCaption
   ::cColor   := hb_defaultValue(cColor, __dbgColors()[1])

   RETURN Self

METHOD PROCEDURE HBDbWindow:Clear()

   hb_Scroll(::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1, NIL, NIL, ::cColor)

   RETURN

METHOD PROCEDURE HBDbWindow:Hide()

   __dbgRestScreen(::nTop, ::nLeft, ::nBottom + IIf(::lShadow, 1, 0), ::nRight + IIf(::lShadow, 2, 0), ::cBackImage)
   ::cBackImage := NIL
   ::lVisible := .F.

   RETURN

METHOD HBDbWindow:IsOver(nRow, nCol)

   RETURN nRow >= ::nTop .AND. nRow <= ::nBottom .AND. nCol >= ::nLeft .AND. nCol <= ::nRight

METHOD PROCEDURE HBDbWindow:ScrollUp(nLines)

   hb_Scroll(::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1, hb_defaultValue(nLines, 1), NIL, ::cColor)

   RETURN

METHOD PROCEDURE HBDbWindow:SetCaption(cCaption)

   ::cCaption := cCaption

   RETURN

METHOD PROCEDURE HBDbWindow:ShowCaption()

   IF !Empty(::cCaption)
      hb_DispOutAt(::nTop, ::nLeft + ((::nRight - ::nLeft) / 2) - ((Len(::cCaption) + 2) / 2), " " + ::cCaption + " ", ::cColor)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbWindow:SetFocus(lOnOff)

   IF !lOnOff .AND. hb_IsEvalItem(::bLostFocus)
      Eval(::bLostFocus, Self)
   ENDIF

   ::lFocused := lOnOff

   IF lOnOff .AND. hb_IsEvalItem(::bGotFocus)
      Eval(::bGotFocus, Self)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbWindow:Refresh()

   DispBegin()

   hb_DispBox(::nTop, ::nLeft, ::nBottom, ::nRight, IIf(::lFocused, HB_B_DOUBLE_UNI, HB_B_SINGLE_UNI), ::cColor)
   hb_DispOutAtBox(::nTop, ::nLeft + 1, hb_UTF8ToStrBox("[■]"), ::cColor)

   ::ShowCaption(::cCaption)

   IF hb_IsEvalItem(::bPainted)
      Eval(::bPainted, Self)
   ENDIF

   DispEnd()

   RETURN

METHOD PROCEDURE HBDbWindow:Show(lFocused)

   ::cBackImage := __dbgSaveScreen(::nTop, ::nLeft, ::nBottom + IIf(::lShadow, 1, 0), ::nRight + IIf(::lShadow, 2, 0))
   hb_Scroll(::nTop, ::nLeft, ::nBottom, ::nRight, NIL, NIL, ::cColor)
   ::SetFocus(hb_defaultValue(lFocused, ::lFocused))

   IF ::lShadow
      hb_Shadow(::nTop, ::nLeft, ::nBottom, ::nRight)
   ENDIF

   ::Refresh()
   ::lVisible := .T.

   RETURN

METHOD PROCEDURE HBDbWindow:ShowModal()

   LOCAL lExit := .F.
   LOCAL nKey

   ::lShadow := .T.
   ::Show()

   DO WHILE !lExit
      nKey := __dbgInkey()

      IF hb_IsEvalItem(::bKeyPressed)
         Eval(::bKeyPressed, nKey)
      ENDIF

      SWITCH nKey
      CASE K_ESC
         lExit := .T.
         EXIT

      CASE K_LBUTTONDOWN
         IF MRow() == ::nTop .AND. MCol() >= ::nLeft + 1 .AND. ;
            MCol() <= ::nLeft + 3
            lExit := .T.
         ENDIF
         EXIT

      ENDSWITCH
   ENDDO

   ::Hide()

   RETURN

METHOD PROCEDURE HBDbWindow:LButtonDown(nMRow, nMCol)

   IF hb_IsEvalItem(::bLButtonDown)
      Eval(::bLButtonDown, nMRow, nMCol)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbWindow:LDblClick(nMRow, nMCol)

   IF hb_IsEvalItem(::bLDblClick)
      Eval(::bLDblClick, nMRow, nMCol)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbWindow:Move()

   LOCAL nOldTop    := ::nTop
   LOCAL nOldLeft   := ::nLeft
   LOCAL nOldBottom := ::nbottom
   LOCAL nOldRight  := ::nright
   LOCAL nKey

   DO WHILE .T.
      __dbgRestScreen(NIL, NIL, NIL, NIL, ::cBackImage)
      hb_DispBox(::nTop, ::nLeft, ::nRight, ::nBottom, Replicate(hb_UTF8ToStrBox("░"), 8) + " ", ::cColor)

      SWITCH nKey := __dbgInkey()
      CASE K_UP

         IF ::nTop != 0
            ::nTop--
            ::nBottom--
         ENDIF
         EXIT

      CASE K_DOWN

         IF ::nBottom != MaxRow()
            ::nTop++
            ::nBottom++
         ENDIF
         EXIT

      CASE K_LEFT

         IF ::nLeft != 0
            ::nLeft--
            ::nRight--
         ENDIF
         EXIT

      CASE K_RIGHT

         IF ::nBottom != MaxRow()
            ::nLeft++
            ::nRight++
         ENDIF
         EXIT

      CASE K_ESC

         ::nTop    := nOldTop
         ::nLeft   := nOldLeft
         ::nBottom := nOldBottom
         ::nRight  := nOldRight
         EXIT

      ENDSWITCH

      IF nKey == K_ESC .OR. nKey == K_ENTER
         EXIT
      ENDIF
   ENDDO

#if 0
   hb_keySetLast(0)
#endif

   RETURN

METHOD PROCEDURE HBDbWindow:KeyPressed(nKey)

   IF hb_IsEvalItem(::bKeyPressed)
      Eval(::bKeyPressed, nKey, Self)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbWindow:LoadColors()

   LOCAL aClr := __dbgColors()

   ::cColor := aClr[1]

   IF ::Browser != NIL
      ::Browser:ColorSpec := aClr[2] + "," + aClr[5] + "," + aClr[3] + "," + aClr[6]
   ENDIF

   RETURN

METHOD HBDbWindow:Resize(nTop, nLeft, nBottom, nRight)

   LOCAL lShow

   IF (!hb_IsNumeric(nTop) .OR. nTop == ::nTop) .AND. ;
      (!hb_IsNumeric(nLeft) .OR. nLeft == ::nLeft) .AND. ;
      (!hb_IsNumeric(nBottom) .OR. nBottom == ::nBottom) .AND. ;
      (!hb_IsNumeric(nRight) .OR. nRight == ::nRight)
      RETURN Self
   ENDIF

   IF (lShow := ::lVisible)
      ::Hide()
   ENDIF

   IF hb_IsNumeric(nTop)
      ::nTop := nTop
   ENDIF
   IF hb_IsNumeric(nBottom)
      ::nBottom := nBottom
   ENDIF
   IF hb_IsNumeric(nLeft)
      ::nLeft := nLeft
   ENDIF
   IF hb_IsNumeric(nRight)
      ::nRight := nRight
   ENDIF

   IF ::Browser != NIL
      ::Browser:Resize(::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1)
   ENDIF

   IF lShow
      ::Show(::lFocused)
   ENDIF

   RETURN Self
