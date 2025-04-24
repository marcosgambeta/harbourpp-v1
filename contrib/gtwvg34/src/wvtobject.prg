//
// Wvt*Classes
//
// Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
// Based On:
// Video subsystem for Windows using GUI windows instead of Console
//     Copyright 2003 Peter Rees <peter@rees.co.nz>
//                    Rees Software and Systems Ltd
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
#include <hbgtinfo.ch>
#include <inkey.ch>
#include <setcurs.ch>

#include "wvtwin.ch"

//
// Class WvtObject
//
// Must never be used directly. It is parent class FOR all other objects!
//
CREATE CLASS WvtObject

   VAR    oParent
   VAR    nType
   VAR    nId

   VAR    nTop
   VAR    nLeft
   VAR    nBottom
   VAR    nRight
   VAR    aPxlTLBR                                INIT {}

   VAR    aObjects                                INIT {}
   VAR    aParent                                 INIT {}
   VAR    aChildren                               INIT {}
   VAR    aPaint                                  INIT {}
   VAR    bPaint
   VAR    ClassName                               INIT ""

   VAR    nObjID                                  INIT 900000
   VAR    nPointer
   VAR    cargo
   VAR    xSettings
   VAR    cText
   VAR    cToolTip
   VAR    lActive                                 INIT .T.
   VAR    lAnimate                                INIT .F.
   VAR    lTabStop                                INIT .T.
   VAR    hFont

   VAR    aPopup                                  INIT {}
   VAR    hPopup
   VAR    nPopupItemID                            INIT 700000

   VAR    nMRow                                   INIT 0
   VAR    nMCol                                   INIT 0
   VAR    cColorHilite                            INIT "W+/B*"
   VAR    cColorDehilite                          INIT "W/N*"

   VAR    nTextColor
   VAR    nBackColor
   VAR    nBackMode                               INIT 0 // OPAQUE 1-TRANSPARENT
   VAR    nTextColorHoverOn
   VAR    nTextColorHoverOff
   VAR    nBackColorHoverOn
   VAR    nBackColorHoverOff
   VAR    cFont
   VAR    nFontHeight
   VAR    nFontWidth
   VAR    nFontWeight
   VAR    nFontQuality
   VAR    nCharSet
   VAR    lItalic
   VAR    lUnderline
   VAR    lStrikeOut
   VAR    nAlignHorz
   VAR    nAlignVert
   VAR    nAngle

   ACCESS ToolTip                                 INLINE IIf(::cTooltip == NIL, "", ::cTooltip)
   ASSIGN ToolTip( cTip )                         INLINE ::cToolTip := cTip

   VAR    bHandleEvent
   VAR    bOnCreate                               INIT {|| NIL }
   VAR    bOnSelect                               INIT {|| NIL }
   VAR    bOnFocus                                INIT {|| NIL }
   VAR    bOnRefresh                              INIT {|| NIL }
   VAR    bOnLeftUp                               INIT {|| NIL }
   VAR    bOnLeftDown                             INIT {|| .F. }
   VAR    bOnMMLeftDown                           INIT {|| NIL }
   VAR    bOnLeftPressed                          INIT {|| NIL }
   VAR    bTooltip                                INIT {|| NIL }
   VAR    bSaveSettings                           INIT {|| NIL }
   VAR    bRestSettings                           INIT {|| NIL }
   VAR    bOnHilite                               INIT {|| NIL }
   VAR    bOnDeHilite                             INIT {|| NIL }

   ACCESS nChildren                               INLINE Len(::aChildren)
   VAR    nIndexOrder

   METHOD New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Destroy()
   METHOD CreatePopup()
   METHOD ShowPopup()

   METHOD SetToolTip()                            INLINE wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )
   METHOD Refresh()                               INLINE wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   METHOD Eval(bBlock)                            INLINE IIf(hb_IsEvalItem(bBlock), Eval(bBlock, Self), NIL)
   METHOD AddChild(aChild)                        INLINE AAdd(::aChildren, aChild)
   METHOD AddParent( aParent )                    INLINE AAdd(::aParent, aParent)

   METHOD PaintBlock()                            INLINE NIL
   METHOD Hilite()                                INLINE NIL
   METHOD DeHilite()                              INLINE NIL
   METHOD HandleEvent()                           INLINE .F.
   METHOD LeftDown()                              INLINE .F.
   METHOD LeftUp()                                INLINE .F.
   METHOD MMLeftDown()                            INLINE .F.
   METHOD LeftPressed()                           INLINE .F.
   METHOD HoverOn()                               INLINE NIL
   METHOD HoverOff()                              INLINE NIL
   METHOD OnTimer()                               INLINE NIL
   METHOD SaveSettings()                          INLINE NIL
   METHOD RestSettings()                          INLINE NIL
   METHOD Activate()                              INLINE NIL
   METHOD DeActivate()                            INLINE NIL
   METHOD NotifyChild(/* nChild */)               INLINE NIL

ENDCLASS

METHOD WvtObject:New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )

   IF !hb_IsNumeric(nID)
      nID := ++::nObjID
   ENDIF

   ::oParent   :=  oParent
   ::nType     :=  nType
   ::nId       :=  nID
   ::nTop      :=  nTop
   ::nLeft     :=  nLeft
   ::nBottom   :=  nBottom
   ::nRight    :=  nRight

   SWITCH nType

   CASE DLG_OBJ_BROWSE
      ::ClassName := "WVTBROWSE"
      EXIT

   CASE DLG_OBJ_STATIC
      ::ClassName := "WVTSTATIC"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_GETS
      ::ClassName := "WVTGETS"
      EXIT

   CASE DLG_OBJ_IMAGE
      ::ClassName := "WVTIMAGE"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_PUSHBUTTON
      ::ClassName := "WVTPUSHBUTTON"
      EXIT

   CASE DLG_OBJ_BUTTON
      ::ClassName := "WVTBUTTON"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_TOOLBAR
      ::ClassName := "WVTTOOLBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_LABEL
      ::ClassName := "WVTLABEL"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_SCROLLBAR
      ::ClassName := "WVTSCROLLBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_STATUSBAR
      ::ClassName := "WVTSTATUSBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_BANNER
      ::ClassName := "WVTBANNER"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_TEXTBOX
      ::ClassName := "WVTTEXTBOX"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_PROGRESSBAR
      ::ClassName := "WVTPROGRESSBAR"
      ::lTabStop  := .F.
      EXIT

   ENDSWITCH

   RETURN Self

METHOD WvtObject:Create()

   ::Eval(::bOnCreate)
   ::CreatePopup()

   RETURN Self

METHOD WvtObject:Destroy()

   IF ::hFont != NIL
      wvg_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::hPopup != NIL
      wapi_DestroyMenu( ::hPopup )
      ::hPopup := NIL
   ENDIF

   RETURN NIL

METHOD WvtObject:CreatePopup()

   LOCAL i, nID

   IF !Empty(::aPopup) .AND. ::hPopup == NIL
      ::hPopup := wapi_CreatePopupMenu()

      FOR EACH i IN ::aPopup

         ASize( i, 3 )
         i[3] := nID := ::nPopupItemID++

         wapi_AppendMenu( ::hPopup, WIN_MF_ENABLED + WIN_MF_STRING, nID, i[1] )
      NEXT
   ENDIF

   RETURN Self

METHOD WvtObject:ShowPopup()

   LOCAL lRet := .F., nRet, n, aPos

   IF ::hPopup != NIL
      aPos := wvt_GetCursorPos()

      nRet := wvt_TrackPopupMenu( ::hPopup, WIN_TPM_CENTERALIGN + WIN_TPM_RETURNCMD, ;
         aPos[1], aPos[2], 0, hb_gtInfo( HB_GTI_WINHANDLE ) )
      IF nRet > 0
         IF ( n := AScan(::aPopup, {| e_ | e_[3] == nRet }) ) > 0
            lRet := .T.

            IF hb_IsEvalItem(::aPopup[n][2])
               Eval(::aPopup[n][2])
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN lRet
