//
// The Debugger (HBDbMenu class)
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

CREATE CLASS HBDbMenu

   METHOD aMenus SETGET

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR aItems
   VAR cClrHilite
   VAR cClrHotKey
   VAR cClrHotFocus
   VAR cClrPopup
   VAR nOpenPopup                                      // zero if no popup is shown
   VAR lPopup
   VAR cBackImage

   METHOD New()
   METHOD AddItem(oMenuItem)
   METHOD Build()
   METHOD ClosePopup(nPopup)
   METHOD CLOSE() INLINE ::ClosePopup(::nOpenPopup), ::nOpenPopup := 0
   METHOD DeHilite()
   METHOD DISPLAY()
   METHOD EvalAction()
   METHOD GetHotKeyPos(cKey)
   METHOD GetItemByIdent(uIdent)
   METHOD GetItemOrdByCoors(nRow, nCol)
   METHOD GoBottom()
   METHOD GoDown() INLINE ::aItems[::nOpenPopup]:bAction:GoRight()
   METHOD GoLeft()
   METHOD GoRight()
   METHOD GoTop()
   METHOD GoUp() INLINE ::aItems[::nOpenPopup]:bAction:GoLeft()
   METHOD IsOpen() INLINE ::nOpenPopup != 0
   METHOD LoadColors()                                 // Load current debugger colors settings
   METHOD ProcessKey(nKey)
   METHOD Refresh()                                    // Repaints the top bar
   METHOD ShowPopup(nPopup)

ENDCLASS

METHOD HBDbMenu:aMenus(xNewVal)

   THREAD STATIC t_aMenus

   IF PCount() > 0
      t_aMenus := xNewVal
   ENDIF

   RETURN t_aMenus

METHOD HBDbMenu:New()

   IF ::aMenus == NIL
      ::aMenus := {}
      ::lPopup := False
   ELSE
      ::lPopup := True
   ENDIF

   ::nTop         := 0
   ::nLeft        := 0
   ::nBottom      := 0
   ::nRight       := 0
   ::aItems       := {}
   ::LoadColors()
   ::nOpenPopup   := 0

   AAdd(::aMenus, Self)

   Return Self

METHOD HBDbMenu:AddItem(oMenuItem)

   Local oLastMenu := ATail(::aMenus)
   Local oLastMenuItem

   IF oLastMenu:lPopup
      oMenuItem:nRow := Len(oLastMenu:aItems)
      oMenuItem:nCol := oLastMenu:nLeft + 1
   ELSE
      oMenuItem:nRow := 0
      IF Len(oLastMenu:aItems) > 0
         oLastMenuItem := ATail(oLastMenu:aItems)
         oMenuItem:nCol := oLastMenuItem:nCol + ;
            Len(StrTran(oLastMenuItem:cPrompt, "~"))
      ELSE
         oMenuItem:nCol := 0
      ENDIF
   ENDIF

   AAdd(ATail(::aMenus):aItems, oMenuItem)

   RETURN oMenuItem

METHOD PROCEDURE HBDbMenu:Build()

   Local nPos := 0
   Local oMenuItem

   IF Len(::aMenus) == 1           // pulldown menu
      For Each oMenuItem In ::aItems
         oMenuItem:nRow := 0
         oMenuItem:nCol := nPos
         nPos += Len(StrTran(oMenuItem:cPrompt, "~"))
      Next
   ELSE
      oMenuItem := ATail(::aMenus[Len(::aMenus) - 1]:aItems)
      ::nTop    := oMenuItem:nRow + 1
      ::nLeft   := oMenuItem:nCol
      nPos := ::nLeft
      For Each oMenuItem In ::aItems
         oMenuItem:nRow := ::nTop + oMenuItem:__enumIndex()
         oMenuItem:nCol := ::nLeft + 1
         nPos := Max(nPos, ::nLeft + Len(StrTran(oMenuItem:cPrompt, "~")) + 1)
      Next
      ::nRight  := nPos + 1
      ::nBottom := ::nTop + Len(::aItems) + 1
      For Each oMenuItem In ::aItems
         IF !hb_LeftEq(oMenuItem:cPrompt, "-")
            oMenuItem:cPrompt := " " + PadR(oMenuItem:cPrompt, ::nRight - ::nLeft - 1)
         ENDIF
      Next
      ATail(::aMenus[Len(::aMenus) - 1]:aItems):bAction := ATail(::aMenus)
      ::aMenus := ASize(::aMenus, Len(::aMenus) - 1)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:ClosePopup(nPopup)

   Local oPopup

   IF nPopup != 0
      oPopup := ::aItems[nPopup]:bAction
      IF hb_IsObject(oPopup)
         __dbgRestScreen(oPopup:nTop, oPopup:nLeft, oPopup:nBottom + 1, oPopup:nRight + 2, oPopup:cBackImage)
         oPopup:cBackImage := NIL
      ENDIF
      ::aItems[nPopup]:Display(::cClrPopup, ::cClrHotKey)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:DeHilite()

   Local oMenuItem := ::aItems[::nOpenPopup]

   oMenuItem:Display(::cClrPopup, ::cClrHotKey)

   RETURN

METHOD PROCEDURE HBDbMenu:Display()

   Local oMenuItem

   IF ::lPopup
      ::cBackImage := __dbgSaveScreen(::nTop, ::nLeft, ::nBottom + 1, ::nRight + 2)
      hb_DispBox(::nTop, ::nLeft, ::nBottom, ::nRight, HB_B_SINGLE_UNI, ::cClrPopup)
      hb_Shadow(::nTop, ::nLeft, ::nBottom, ::nRight)
   ELSE
      hb_Scroll(0, 0, 0, MaxCol(), NIL, NIL, ::cClrPopup)
   ENDIF

   For Each oMenuItem In ::aItems
      IF oMenuItem:cPrompt == "-"  // Separator
         hb_DispOutAtBox(oMenuItem:nRow, ::nLeft, hb_UTF8ToStrBox("├" + Replicate("─", ::nRight - ::nLeft - 1) + "┤"), ::cClrPopup)
      ELSE
         oMenuItem:Display(::cClrPopup, ::cClrHotKey)
      ENDIF
   Next

   RETURN

METHOD PROCEDURE HBDbMenu:EvalAction()

   Local oPopup := ::aItems[::nOpenPopup]:bAction
   Local oMenuItem := oPopup:aItems[oPopup:nOpenPopup]

   IF oMenuItem:bAction != NIL
      ::Close()
      Eval(oMenuItem:bAction, oMenuItem)
   ENDIF

   RETURN

METHOD HBDbMenu:GetHotKeyPos(cKey)

   Local oMenuItem

   For Each oMenuItem In ::aItems
      IF Upper(SubStr(oMenuItem:cPrompt, At("~", oMenuItem:cPrompt) + 1, 1)) == cKey
         RETURN oMenuItem:__enumIndex()
      ENDIF
   Next

   RETURN 0

METHOD HBDbMenu:GetItemOrdByCoors(nRow, nCol)

   Local oMenuItem

   For Each oMenuItem In ::aItems
      IF oMenuItem:nRow == nRow .AND. nCol >= oMenuItem:nCol .AND. ;
         nCol <= oMenuItem:nCol + Len(oMenuItem:cPrompt) - 2
         RETURN oMenuItem:__enumIndex()
      ENDIF
   Next

   RETURN 0

METHOD HBDbMenu:GetItemByIdent(uIdent)

   Local oMenuItem
   Local oItem

   For Each oMenuItem In ::aItems
      IF hb_IsObject(oMenuItem:bAction)
         IF (oItem := oMenuItem:bAction:GetItemByIdent(uIdent)) != NIL
            RETURN oItem
         ENDIF
      ELSEIF ValType(oMenuItem:Ident) == ValType(uIdent) .AND. ;
         oMenuItem:Ident == uIdent
         RETURN oMenuItem
      ENDIF
   Next

   Return Nil

METHOD PROCEDURE HBDbMenu:GoBottom()

   Local oPopup

   IF ::IsOpen()
      oPopup := ::aItems[::nOpenPopup]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup(Len(oPopup:aItems))
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:GoLeft()

   Local oMenuItem := ::aItems[::nOpenPopup]

   IF ::nOpenPopup != 0
      IF ::lPopup
         oMenuItem:Display(::cClrPopup, ::CClrHotKey)
      ELSE
         ::ClosePopup(::nOpenPopup)
      ENDIF
      IF ::nOpenPopup > 1
         --::nOpenPopup
         Do While ::nOpenPopup > 1 .AND. ;
            hb_LeftEq(::aItems[::nOpenPopup]:cPrompt, "-")
            --::nOpenPopup
         EndDo
         ::ShowPopup(::nOpenPopup)
      ELSE
         ::ShowPopup(::nOpenPopup := Len(::aItems))
      ENDIF
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:GoRight()

   Local oMenuItem := ::aItems[::nOpenPopup]

   IF ::nOpenPopup != 0
      IF ::lPopup
         oMenuItem:Display(::cClrPopup, ::cClrHotKey)
      ELSE
         ::ClosePopup(::nOpenPopup)
      ENDIF
      IF ::nOpenPopup < Len(::aItems)
         ++::nOpenPopup
         Do While ::nOpenPopup < Len(::aItems) .AND. ;
            hb_LeftEq(::aItems[::nOpenPopup]:cPrompt, "-")
            ++::nOpenPopup
         EndDo
         ::ShowPopup(::nOpenPopup)
      ELSE
         ::ShowPopup(::nOpenPopup := 1)
      ENDIF
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:GoTop()

   Local oPopup

   IF ::IsOpen()
      oPopup := ::aItems[::nOpenPopup]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup(1)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:LoadColors()

   Local aColors := __dbgColors()
   Local oMenuItem

   ::cClrPopup    := aColors[ 8]
   ::cClrHotKey   := aColors[ 9]
   ::cClrHilite   := aColors[10]
   ::cClrHotFocus := aColors[11]

   For Each oMenuItem In ::aItems
      IF hb_IsObject(oMenuItem:bAction)
         oMenuItem:bAction:LoadColors()
      ENDIF
   Next

   RETURN

METHOD PROCEDURE HBDbMenu:Refresh()

   Local oMenuItem

   DispBegin()

   IF !::lPopup
      hb_Scroll(0, 0, 0, MaxCol(), NIL, NIL, ::cClrPopup)
   ENDIF

   For Each oMenuItem In ::aItems
      oMenuItem:Display(::cClrPopup, ::cClrHotKey)
   Next

   DispEnd()

   RETURN

METHOD PROCEDURE HBDbMenu:ShowPopup(nPopup)

   ::aItems[nPopup]:Display(::cClrHilite, ::cClrHotFocus)
   ::nOpenPopup := nPopup

   IF hb_IsObject(::aItems[nPopup]:bAction)
      ::aItems[nPopup]:bAction:Display()
      ::aItems[nPopup]:bAction:ShowPopup(1)
   ENDIF

   RETURN

METHOD PROCEDURE HBDbMenu:ProcessKey(nKey)

   Local nPopup
   Local oPopup

   Switch nKey
   Case K_LBUTTONDOWN
      IF MRow() == 0
         IF (nPopup := ::GetItemOrdByCoors(0, MCol())) != 0
            IF nPopup != ::nOpenPopup
               ::ClosePopup(::nOpenPopup)
               ::ShowPopup(nPopup)
            ENDIF
         ENDIF
      ELSE
         oPopup := ::aItems[::nOpenPopup]:bAction
         IF (nPopup := oPopup:GetItemOrdByCoors(MRow(), MCol())) == 0
            ::Close()
         ELSE
            oPopup:DeHilite()
            oPopup:nOpenPopup := nPopup
            oPopup:aItems[nPopup]:Display(::cClrHilite, ::cClrHotFocus)
            ::EvalAction()
         ENDIF
      ENDIF
      Exit

   Case K_ESC
      ::Close()
      Exit

   Case K_LEFT
      ::GoLeft()
      Exit

   Case K_RIGHT
      ::GoRight()
      Exit

   Case K_DOWN
      ::GoDown()
      Exit

   Case K_UP
      ::GoUp()
      Exit

   Case K_ENTER
      ::EvalAction()
      Exit

   Case K_HOME
      ::GoTop()
      Exit

   Case K_END
      ::GoBottom()
      Exit

   Otherwise

      IF ::nOpenPopup > 0
         IF IsAlpha(hb_keyChar(nKey))
            oPopup := ::aItems[::nOpenPopup]:bAction
            IF (nPopup := oPopup:GetHotKeyPos(Upper(hb_keyChar(nKey)))) > 0
               IF oPopup:nOpenPopup != nPopup
                  oPopup:DeHilite()
                  oPopup:ShowPopup(nPopup)
               ENDIF
               ::EvalAction()
            ENDIF
         ENDIF
      ELSEIF (nPopup := ::GetHotKeyPos(__dbgAltToKey(nKey))) != ::nOpenPopup
         ::Close()
         ::ShowPopup(nPopup)
      ENDIF

   EndSwitch

   RETURN

Function __dbgAltToKey(nKey)

   Local nIndex := AScan({ ;
      K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F, ;
      K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L, ;
      K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R, ;
      K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X, ;
      K_ALT_Y, K_ALT_Z, K_ALT_1, K_ALT_2, K_ALT_3, K_ALT_4, ;
      K_ALT_5, K_ALT_6, K_ALT_7, K_ALT_8, K_ALT_9, K_ALT_0 }, nKey)

   RETURN IIf(nIndex > 0, SubStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890", nIndex, 1), "")
