//
// Xbase++ xbpSLE compatible Class
//
// Copyright 2008-2011 Pritpal Bedi <bedipritpal@hotmail.com>
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

//                                EkOnkar
//                          ( The LORD is ONE )

#include <hbclass.ch>
#include <inkey.ch>
#include <hbgtinfo.ch>

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgSLE INHERIT WvgWindow, WvgDataRef

   VAR align INIT WVGSLE_LEFT
   VAR autoKeyboard INIT .T.
   VAR autoSize INIT .F.
   VAR autoTab INIT .F.
   VAR border INIT .T.
   VAR bufferLength INIT 32
   VAR editable INIT .T.
   VAR unReadable INIT .F.

   METHOD new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD configure(oParent, oOwner, aPos, aSize, aPresParams, lVisible) VIRTUAL
   METHOD destroy()
   METHOD handleEvent(nMessage, aNM)

   METHOD clear()
   METHOD copyMarked()
   METHOD cutMarked()
   METHOD delMarked() VIRTUAL
   METHOD editBuffer() VIRTUAL
   METHOD pasteMarked() VIRTUAL
   METHOD queryFirstChar() VIRTUAL
   METHOD queryMarked() VIRTUAL
   METHOD setFirstChar() VIRTUAL
   METHOD setMarked() VIRTUAL

   METHOD setInsertMode(lInsertMode) VIRTUAL

   VAR sl_hScroll
   ACCESS hScroll INLINE ::sl_hScroll
   ASSIGN hScroll(bBlock) INLINE ::sl_hScroll := bBlock

   VAR sl_typeOut
   ACCESS typeOut INLINE ::sl_typeOut
   ASSIGN typeOut(bBlock) INLINE ::sl_typeOut := bBlock

   METHOD changed(lChanged) SETGET

   VAR sl_returnPressed
   METHOD returnPressed(bReturnPressed) SETGET

ENDCLASS

METHOD WvgSLE:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::style := WS_CHILD
   ::className := "EDIT"
   ::objType := objTypeSLE

   RETURN Self

METHOD WvgSLE:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   LOCAL es_ := {ES_LEFT, ES_RIGHT, ES_CENTER}

   ::wvgWindow:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::style += es_[::align]
   ::style += ES_AUTOHSCROLL

   IF ::tabStop
      ::style += WS_TABSTOP
   ENDIF
   IF ::autoSize

   ENDIF
   IF !::editable
      ::style += ES_READONLY
   ENDIF
   IF ::unReadable
      ::style += ES_PASSWORD
   ENDIF
   IF ::border
      ::style += WS_BORDER
   ENDIF

   ::oParent:addChild(Self)

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   IF hb_IsObject(::datalink)
      Eval(::datalink)
   ENDIF

   ::sendMessage(EM_SETLIMITTEXT, ::bufferLength)

   RETURN Self

METHOD WvgSLE:handleEvent(nMessage, aNM)

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::oParent:setFocus()
         ::rePosition()
      ENDIF
      ::sendMessage(WM_SIZE, 0, 0)
      EXIT

   CASE HB_GTE_COMMAND
      SWITCH aNM[NMH_code]
      CASE EN_CHANGE
         EXIT
      CASE EN_UPDATE
         EXIT
      CASE EN_MAXTEXT
         EXIT
      CASE EN_KILLFOCUS
         IF hb_IsBlock(::sl_killInputFocus)
            Eval(::sl_killInputFocus, , , Self)
         ENDIF
         EXIT
      CASE EN_SETFOCUS
         IF hb_IsBlock(::sl_setInputFocus)
            Eval(::sl_setInputFocus, , , Self)
         ENDIF
      ENDSWITCH
      EXIT

   CASE HB_GTE_CTLCOLOR
      IF hb_IsNumeric(::clr_FG)
         wvg_SetTextColor(aNM[1], ::clr_FG)
      ENDIF
      IF hb_IsNumeric(::hBrushBG)
         wvg_SetBkMode(aNM[1], 1)
         RETURN ::hBrushBG
      ELSE
         RETURN wvg_GetCurrentBrush(aNM[1])
      ENDIF
      EXIT

   CASE HB_GTE_ANY
      SWITCH aNM[NMH_code]
      CASE WM_KILLFOCUS
         IF hb_IsBlock(::sl_killInputFocus)
            Eval(::sl_killInputFocus, , , Self)
         ENDIF
         EXIT
      CASE WM_SETFOCUS
         IF hb_IsBlock(::sl_setInputFocus)
            Eval(::sl_setInputFocus, , , Self)
         ENDIF
         EXIT
      CASE WM_KEYDOWN
         DO CASE
         CASE aNM[2] == K_ENTER
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            IF hb_IsBlock(::sl_returnPressed)
               Eval(::sl_returnPressed, , , Self)
            ENDIF
         CASE aNM[2] == VK_TAB
            IF ::isParentCrt()
               ::oParent:setFocus()
               RETURN EVENT_HANDELLED
            ENDIF
         CASE aNM[2] == 65
            // RETURN EVENT_HANDELLED
         ENDCASE
      ENDSWITCH

   ENDSWITCH

   RETURN EVENT_UNHANDELLED

METHOD PROCEDURE WvgSLE:destroy()

   ::wvgWindow:destroy()

   RETURN

METHOD WvgSLE:changed(lChanged)

   LOCAL lChg := ::sendMessage(EM_GETMODIFY, 0, 0)

   IF hb_IsLogical(lChanged)
      ::sendMessage(EM_SETMODIFY, IIf(lChanged, 0, 1), 0)
   ENDIF

   RETURN lChg

METHOD WvgSLE:clear()

   LOCAL cText := ::getData()

   ::setData("")

   RETURN Len(cText)

METHOD WvgSLE:copyMarked()

   LOCAL n
   LOCAL nB
   LOCAL nE

   n := ::sendMessage(EM_GETSEL)
   nB := wvg_LOWORD(n)
   nE := wvg_HIWORD(n)

   IF (n := nE - nB) > 0
      wvt_SetClipboard(SubStr(::getData(), nB, n))
   ENDIF

   RETURN n

METHOD WvgSLE:cutMarked()

   LOCAL n
   LOCAL nB
   LOCAL nE
   LOCAL cText

   n := ::sendMessage(EM_GETSEL)
   nB := wvg_LOWORD(n)
   nE := wvg_HIWORD(n)

   IF (n := nE - nB) > 0
      cText := ::getData()
      ::setData(SubStr(cText, 1, nB - 1) + SubStr(cText, nE))
   ENDIF

   RETURN n

METHOD WvgSLE:returnPressed(bReturnPressed)

   IF hb_IsBlock(bReturnPressed)
      ::sl_returnPressed := bReturnPressed
   ELSEIF hb_IsBlock(::sl_returnPressed)
      Eval(::sl_returnPressed, , , Self)
   ENDIF

   RETURN Self
