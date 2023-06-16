/*
 * Wvt*Classes
 *
 * Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based On:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software and Systems Ltd
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

#define K_LBUTTONPRESSED        1021
#define K_RBUTTONPRESSED        1022
#define K_MBUTTONPRESSED        1023

#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

/* WvtStatusBar */
CREATE CLASS WvtStatusBar INHERIT WvtObject

   VAR    aPanels
   VAR    cColor

   METHOD New(oParent, nID, nTop, nLeft, nBottom, nRight)
   METHOD create()
   METHOD SetPanels(aPanels)
   METHOD SetText(nPanel, cText, cColor)
   METHOD SetIcon(nPanel, cIconFile)
   METHOD Update(nPanel, cText, cColor)
   METHOD PaintBlock()
   METHOD Refresh()

ENDCLASS

METHOD WvtStatusBar:New(oParent, nID, nTop, nLeft, nBottom, nRight)

   __defaultNIL(@nTop, oParent:MaxRow())
   __defaultNIL(@nLeft, 0)
   __defaultNIL(@nBottom, oParent:MaxRow())
   __defaultNIL(@nRight, oParent:MaxCol())

   ::Super:New(oParent, DLG_OBJ_STATUSBAR, nID, nTop, nLeft, nBottom, nRight)

   ::cColor  := "N/W"

   RETURN Self

METHOD WvtStatusBar:Create()

   ::Refresh()
   ::PaintBlock(DLG_OBJ_STATUSBAR, Self)

   ::Super:Create()

   RETURN Self

METHOD WvtStatusBar:PaintBlock()

   LOCAL a_ := {}
   LOCAL nPanels

   AEval(::aPanels, {|o|AAdd(a_, o:nTop), AAdd(a_, o:nLeft), AAdd(a_, o:nBottom), AAdd(a_, o:nRight)})

   a_[Len(a_)]++
   nPanels := Len(::aPanels)

   ::bPaint  := {||wvt_DrawStatusBar(nPanels, a_)}
   AAdd(::aPaint, {::bPaint, {WVT_BLOCK_STATUSBAR, ::nTop, ::nLeft, ::nBottom, ::nRight}})

   RETURN Self

METHOD WvtStatusBar:SetPanels(aPanels)

   LOCAL i
   LOCAL oPanel
   LOCAL nID
   LOCAL nLastCol := ::oParent:MaxCol()

   nID := 200000

   ::aPanels := {}

   oPanel := WvtPanel():New(::oParent, ++nID, ::nTop, 0)

   AAdd(::aPanels, oPanel)

   IF aPanels != NIL
      FOR i := 1 TO Len(aPanels)
         IF ::oParent:MaxCol() > aPanels[i]
            oPanel := WvtPanel():New(::oParent, ++nID, ::nTop, aPanels[i])
            AAdd(::aPanels, oPanel)
         ENDIF
      NEXT
   ENDIF

   ATail(::aPanels):nRight := nLastCol

   FOR i := Len(::aPanels) - 1 TO 1 STEP -1
      oPanel        := ::aPanels[i]
      oPanel:nRight := ::aPanels[i + 1]:nLeft
      oPanel:cColor := ::cColor
   NEXT

   RETURN Self

METHOD WvtStatusBar:Update(nPanel, cText, cColor)

   LOCAL oPanel

   IF nPanel > 0 .AND. nPanel <= Len(::aPanels)
      oPanel        := ::aPanels[nPanel]
      oPanel:Text   := cText
      oPanel:cColor := iif(cColor == NIL, "N/W", cColor)
      oPanel:Refresh()
   ENDIF

   RETURN Self

METHOD WvtStatusBar:SetText(nPanel, cText, cColor)

   LOCAL oPanel

   __defaultNIL(@cColor, ::cColor)

   IF nPanel > 0 .AND. nPanel <= Len(::aPanels)
      oPanel        := ::aPanels[nPanel]
      oPanel:Text   := cText
      oPanel:cColor := cColor
   ENDIF

   RETURN Self

METHOD WvtStatusBar:SetIcon(nPanel, cIconFile)

   IF nPanel > 0 .AND. nPanel <= Len(::aPanels)
      ::aPanels[nPanel]:cIconFile := cIconFile
   ENDIF

   RETURN Self

METHOD WvtStatusBar:Refresh()

   LOCAL i

   FOR i := 1 TO Len(::aPanels)
      ::aPanels[i]:Refresh()
   NEXT

   RETURN NIL
