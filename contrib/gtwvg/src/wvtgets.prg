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

/* Class WvtGets */
CREATE CLASS WvtGets INHERIT WvtObject

   VAR    aGetList                                INIT {}
   VAR    nLastGet                                INIT 1
   VAR    nCurGet                                 INIT 1
   VAR    GetList                                 INIT {}
   VAR    cDesc                                   INIT ""

   METHOD New(oParent, nID, nTop, nLeft, nBottom, nRight)
   METHOD create()
   METHOD KillFocus()
   METHOD SetFocus()
   METHOD HandleEvent(nKey)
   METHOD AddGets(nRow, nCol, xVar, cPic, cColor, bValid, bWhen)
   METHOD PaintBlock(nIndex)
   METHOD READ()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD GetData()
   METHOD SetData()

ENDCLASS

METHOD WvtGets:New(oParent, nID, nTop, nLeft, nBottom, nRight)

   ::Super:New(oParent, DLG_OBJ_GETS, nID, nTop, nLeft, nBottom, nRight)

   RETURN Self

METHOD WvtGets:Create()

   LOCAL i
   LOCAL nCurRow := Row()
   LOCAL nCurCol := Col()

   FOR i := 1 TO Len(::aGetList)

      __defaultNIL(@::aGetList[i][7], "N/W*,N/W*,,,N/GR*")
      __defaultNIL(@::aGetList[i][5], {|| .T. })
      __defaultNIL(@::aGetList[i][6], {|| .T. })

      AAdd(::GetList, Get():New(::aGetList[i][1], ::aGetList[i][2], {| v | iif(PCount() == 0, ::aGetList[i][3], ::aGetList[i][3] := v) }, "::aGetList[i][3]", ::aGetList[i][7]))

      ::GetList[i]:Display()
      ::PaintBlock(i)
   NEXT
   SetPos(nCurRow, nCurCol)

   ::Super:Create()
   ::Dehilite()

   RETURN Self

METHOD WvtGets:PaintBlock(nIndex)

   LOCAL nLen, bPaint

   nLen   := Len(Transform(::aGetList[nIndex][3], ::aGetList[nIndex][4]))

   bPaint := {|| wvt_DrawBoxGet(::aGetList[nIndex][1], ::aGetList[nIndex][2], nLen) }

   AAdd(::aPaint, { bPaint, ;
      { WVT_BLOCK_GETS, ::aGetList[nIndex][1] - 1, ::aGetList[nIndex][2] - 1, ;
      ::aGetList[nIndex][1] - 1,  ::aGetList[nIndex][2] + nLen } })

   RETURN Self

METHOD WvtGets:SetFocus()

   RETURN Self

METHOD WvtGets:KillFocus()

   RETURN Self

METHOD WvtGets:AddGets(nRow, nCol, xVar, cPic, cColor, bValid, bWhen)

   AAdd(::aGetList, { nRow, nCol, xVar, cPic, bValid, bWhen, cColor })

   RETURN Self

METHOD WvtGets:HandleEvent(nKey)

   LOCAL lRet := .F.

   DO CASE
   CASE nKey == K_LDBLCLK
      ::Read()
      lRet := .T.
   ENDCASE

   RETURN lRet

METHOD WvtGets:Read()

   ReadModal(::GetList, ::nCurGet)

   RETURN Self

METHOD WvtGets:GetData()
   RETURN NIL

METHOD WvtGets:SetData(/* aData */)
   RETURN Self

METHOD WvtGets:Hilite()

   hb_DispOutAt(::nTop, ::nLeft, PadR(" " + ::cDesc, ::nRight - ::nLeft + 1), ::cColorHilite)

   RETURN Self

METHOD WvtGets:DeHilite()

   hb_DispOutAt(::nTop, ::nLeft, PadR(" " + ::cDesc, ::nRight - ::nLeft + 1), ::cColorDeHilite)

   RETURN Self
