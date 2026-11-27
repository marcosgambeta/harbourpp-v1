//
// The Debugger Object Inspector
//
// Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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
#include "setcurs.ch"

// object message description
#define OMSG_NAME       1
#define OMSG_VALUE      2
#define OMSG_EDIT       3

CREATE CLASS HBDbObject

   VAR aWindows       INIT {}
   VAR Theobj
   VAR objname
   VAR nCurWindow     INIT 0
   VAR pItems         INIT {}
   VAR ArrayIndex     INIT 1
   VAR lEditable

   METHOD New(oObject, cVarName, lEditable)
   METHOD addWindows(nRow)
   METHOD doGet(oBrowse)
   METHOD SetsKeyPressed(nKey, oBrwSets)

ENDCLASS

METHOD HBDbObject:New(oObject, cVarName, lEditable)

   Local cMsg
   Local cMsgAcc
   Local aMessages
   Local aMethods
   Local xValue

   __dbgSetGo(__dbg():pInfo)

   // create list of object messages
   aMessages := oObject:classSel()
   ASort(aMessages, NIL, NIL, {| x, y | x + Chr(0) < y + Chr(0) })
   aMethods := {}
   For Each cMsg In aMessages
      IF hb_LeftEq(cMsg, "_") .AND. ;
         hb_AScan(aMessages, cMsgAcc := SubStr(cMsg, 2), NIL, NIL, True) > 0
         xValue := __dbgObjGetValue(oObject, cMsgAcc)
         AAdd(::pItems, {cMsgAcc, xValue, True})
      ELSEIF hb_AScan(aMessages, "_" + cMsg, NIL, NIL, True) == 0
         AAdd(aMethods, cMsg)
      ENDIF
   Next
   For Each cMsg In aMethods
      AAdd(::pItems, {Lower(cMsg), "Method", False})
   Next

   ::objname := cVarName
   ::TheObj := oObject
   ::lEditable := hb_defaultValue(lEditable, True)

   ::addWindows()

   Return Self

METHOD HBDbObject:addWindows(nRow)

   Local oBrwSets
   Local nSize := Len(::pItems)
   Local oWndSets
   Local oCol
   Local nMaxLen

   IF nSize < MaxRow() - 2
      IF hb_IsNumeric(nRow)
         oWndSets := HBDbWindow():New(nRow, 5, IIf(nRow + nSize + 1 < MaxRow() - 2, nRow + nSize + 1, MaxRow() - 2), MaxCol() - 5, ;
            ::objname + " is of class: " + ::TheObj:ClassName(), "N/W")
      ELSE
         oWndSets := HBDbWindow():New(1, 5, 2 + nSize, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W")
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New(1, 5, MaxRow() - 2, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W")
   ENDIF

   ::nCurWindow++
   oWndSets:lFocused := True
   AAdd(::aWindows, oWndSets)

   oBrwSets := HBDbBrowser():New(oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1)

   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:GoTopBlock := {|| ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := {|| ::arrayindex := Len(::pItems) }
   oBrwSets:SkipBlock := {|nSkip, nPos|nPos := ::arrayindex, ;
                                       ::arrayindex := Max(1, Min(::arrayindex + nSkip, Len(::pItems))), ;
                                       ::arrayindex - nPos}

   nMaxLen := 0
   AEval(::pItems, {| x | nMaxLen := Max(nMaxLen, Len(x[OMSG_NAME])) })
   oBrwSets:AddColumn(oCol := HBDbColumnNew("", {|| ::pItems[::arrayindex][OMSG_NAME] }))
   oCol:width := nMaxLen
   oCol:DefColor := {1, 2}
   oBrwSets:Freeze := 1

   oBrwSets:AddColumn(oCol := HBDbColumnNew("", {|| IIf(!::pItems[::ArrayIndex][OMSG_EDIT], ;
      ::pItems[::ArrayIndex][OMSG_VALUE], ;
      __dbgValToExp(__dbgObjGetValue(::TheObj, ::pItems[::arrayindex][OMSG_NAME]))) }))

   oCol:DefColor := {1, 3}
   oCol:width := oWndSets:nRight - oWndSets:nLeft - nMaxLen - 2
   oBrwSets:colPos := 2

   ::aWindows[::nCurWindow]:bPainted    := {|| oBrwSets:ForceStable() }
   ::aWindows[::nCurWindow]:bKeyPressed := {| nKey | ::SetsKeyPressed(nKey, oBrwSets) }
   ::aWindows[::nCurwindow]:cCaption := ::objname + " is of class: " + ::TheObj:ClassName()

   ::aWindows[::nCurWindow]:ShowModal()

   Return Self

METHOD PROCEDURE HBDbObject:doGet(oBrowse)

   Local oErr
   Local cValue
   Local lCanAcc
   Local aItemRef

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   aItemRef := ::pItems[::ArrayIndex]
   cValue := __dbgObjGetValue(::TheObj, aItemRef[OMSG_NAME], @lCanAcc)
   IF !lCanAcc
      __dbgAlert(cValue)
      RETURN
   ENDIF
   cValue := __dbgValToExp(cValue)

   IF __dbgInput(Row(), oBrowse:nLeft + oBrowse:GetColumn(1):width + 1, oBrowse:getColumn(oBrowse:colPos):Width, @cValue, __dbgExprValidBlock(), __dbgColors()[2], 256)
      BEGIN SEQUENCE WITH __BreakBlock()
         __dbgObjSetValue(::TheObj, aItemRef[OMSG_NAME], &cValue)
      RECOVER USING oErr
         __dbgAlert(oErr:description)
      END SEQUENCE
   ENDIF

   RETURN

METHOD PROCEDURE HBDbObject:SetsKeyPressed(nKey, oBrwSets)

   Local aItemRef

   Switch nKey
   Case K_UP
      oBrwSets:Up()
      Exit

   Case K_DOWN
      oBrwSets:Down()
      Exit

   Case K_HOME
   Case K_CTRL_PGUP
   Case K_CTRL_HOME
      oBrwSets:GoTop()
      Exit

   Case K_END
   Case K_CTRL_PGDN
   Case K_CTRL_END
      oBrwSets:GoBottom()
      Exit

   Case K_PGDN
      oBrwSets:pageDown()
      Exit

   Case K_PGUP
      oBrwSets:PageUp()
      Exit

   Case K_ENTER

      aItemRef := ::pItems[::ArrayIndex]
      Do Case
      Case hb_IsArray(aItemRef[OMSG_VALUE])
         IF Len(aItemRef[OMSG_VALUE]) > 0
            HBDbArray():New(aItemRef[OMSG_VALUE], aItemRef[OMSG_NAME])
         ENDIF
      Case hb_IsHash(aItemRef[OMSG_VALUE])
         IF Len(aItemRef[OMSG_VALUE]) > 0
            HBDbHash():New(aItemRef[OMSG_VALUE], aItemRef[OMSG_NAME])
         ENDIF
      Case hb_IsObject(aItemRef[OMSG_VALUE])
         HBDbObject():New(aItemRef[OMSG_VALUE], aItemRef[OMSG_NAME])
      Case !aItemRef[OMSG_EDIT] .OR. ;
           hb_IsBlock(aItemRef[OMSG_VALUE]) .OR. ;
           hb_IsPointer(aItemRef[OMSG_VALUE]) .OR. ;
           !::lEditable
         __dbgAlert("Value cannot be edited")
      Otherwise
         oBrwSets:RefreshCurrent()
         ::doGet(oBrwSets)
         oBrwSets:RefreshCurrent()
         oBrwSets:ForceStable()
      EndCase

   EndSwitch

   oBrwSets:ForceStable()

   RETURN

Function __dbgObject(oObject, cVarName, lEditable)
   RETURN HBDbObject():New(oObject, cVarName, lEditable)

Static Function __dbgObjGetValue(oObject, cVar, lCanAcc)

   Local nProcLevel := __dbg():nProcLevel
   Local xResult
   Local oErr

   BEGIN SEQUENCE WITH {|| Break() }
      xResult := __dbgSendMsg(nProcLevel, oObject, cVar)
      lCanAcc := True
   RECOVER
      BEGIN SEQUENCE WITH __BreakBlock()
         // Try to access variables using class code level
         xResult := __dbgSendMsg(0, oObject, cVar)
         lCanAcc := True
      RECOVER USING oErr
         xResult := oErr:description
         lCanAcc := False
      END SEQUENCE
   END SEQUENCE

   RETURN xResult

Static Function __dbgObjSetValue(oObject, cVar, xValue)

   Local nProcLevel := __dbg():nProcLevel
   Local oErr

   BEGIN SEQUENCE WITH {|| Break() }
      __dbgSendMsg(nProcLevel, oObject, "_" + cVar, xValue)
   RECOVER
      BEGIN SEQUENCE WITH __BreakBlock()
         // Try to access variables using class code level
         __dbgSendMsg(0, oObject, "_" + cVar, xValue)
      RECOVER USING oErr
         __dbgAlert(oErr:description)
      END SEQUENCE
   END SEQUENCE

   RETURN xValue
