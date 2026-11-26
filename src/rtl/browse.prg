//
// Quick Clipper Browse()
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

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

Function Browse(nTop, nLeft, nBottom, nRight)

   LOCAL oBrw
   LOCAL lContinue
   LOCAL lAppend
   LOCAL lKeyPressed
   LOCAL lRefresh
   LOCAL n
   LOCAL nOldCursor
   LOCAL nKey
   LOCAL cOldScreen
   LOCAL bAction

   IF !Used()
      Return False
   ENDIF

   lAppend := lKeyPressed := lRefresh := False

   IF PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   ENDIF

   DispBegin()

   nOldCursor := SetCursor(SC_NONE)
   cOldScreen := SaveScreen(nTop, nLeft, nBottom, nRight)

   hb_DispBox(nTop, nLeft, nBottom, nRight, HB_B_DOUBLE_SINGLE_UNI)
   hb_DispOutAtBox(nTop + 3, nLeft, hb_UTF8ToStrBox("╞"))
   hb_DispOutAtBox(nTop + 3, nRight, hb_UTF8ToStrBox("╡"))
   hb_DispOutAt(nTop + 1, nLeft + 1, Space(nRight - nLeft - 1))

   oBrw := TBrowseDB(nTop + 2, nLeft + 1, nBottom - 1, nRight - 1)
   oBrw:HeadSep := " " + hb_UTF8ToStrBox("═")
   oBrw:SkipBlock := {|nRecs|Skipped(nRecs, lAppend)}

   FOR n := 1 TO FCount()
      oBrw:AddColumn(TBColumnNew(FieldName(n), FieldBlock(FieldName(n))))
   NEXT

   IF Eof()
      dbGoTop()
   ENDIF

   oBrw:ForceStable()

   DispEnd()

   IF LastRec() == 0
      nKey := K_DOWN
      lKeyPressed := True
   ENDIF

   lContinue := True

   Do While lContinue

      Do While !lKeyPressed .AND. !oBrw:Stabilize()
         lKeyPressed := (nKey := Inkey()) != 0
      EndDo

      IF lKeyPressed
         lKeyPressed := False
      ELSE
         IF oBrw:HitBottom .AND. (!lAppend .OR. RecNo() != LastRec() + 1)
            IF lAppend
               oBrw:RefreshCurrent()
               oBrw:ForceStable()
               dbGoBottom()
            ELSE
               lAppend := True
               SetCursor(IIf(ReadInsert(), SC_INSERT, SC_NORMAL))
            ENDIF
            oBrw:Down()
            oBrw:ForceStable()
            oBrw:ColorRect({oBrw:RowPos, 1, oBrw:RowPos, oBrw:ColCount}, {2, 2})
         ENDIF

         StatLine(oBrw, lAppend)

         oBrw:ForceStable()

         nKey := Inkey(0)
         IF (bAction := SetKey(nKey)) != NIL
            Eval(bAction, ProcName(1), ProcLine(1), "")
            LOOP
         ENDIF
      ENDIF

      Switch nKey

#ifdef HB_COMPAT_C53
      Case K_LBUTTONDOWN
      Case K_LDBLCLK
         TBMouse(oBrw, MRow(), MCol())
         EXIT
#endif
#ifndef HB_CLP_STRICT
      Case K_MWFORWARD
#endif
      Case K_UP
         IF lAppend
            lRefresh := True
         ELSE
            oBrw:Up()
         ENDIF
         EXIT

#ifndef HB_CLP_STRICT
      Case K_MWBACKWARD
#endif
      Case K_DOWN
         IF lAppend
            oBrw:HitBottom := True
         ELSE
            oBrw:Down()
         ENDIF
         EXIT

      Case K_PGUP
         IF lAppend
            lRefresh := True
         ELSE
            oBrw:PageUp()
         ENDIF
         EXIT

      Case K_PGDN
         IF lAppend
            oBrw:HitBottom := True
         ELSE
            oBrw:PageDown()
         ENDIF
         EXIT

      Case K_CTRL_PGUP
         IF lAppend
            lRefresh := True
         ELSE
            oBrw:GoTop()
         ENDIF
         EXIT

      Case K_CTRL_PGDN
         IF lAppend
            lRefresh := True
         ELSE
            oBrw:GoBottom()
         ENDIF
         EXIT

      Case K_LEFT
         oBrw:Left()
         EXIT

      Case K_RIGHT
         oBrw:Right()
         EXIT

      Case K_HOME
         oBrw:Home()
         EXIT

      Case K_END
         oBrw:End()
         EXIT

      Case K_CTRL_LEFT
         oBrw:panLeft()
         EXIT

      Case K_CTRL_RIGHT
         oBrw:panRight()
         EXIT

      Case K_CTRL_HOME
         oBrw:panHome()
         EXIT

      Case K_CTRL_END
         oBrw:panEnd()
         EXIT

      Case K_INS
         IF lAppend
            SetCursor(IIf(ReadInsert(!ReadInsert()), SC_NORMAL, SC_INSERT))
         ENDIF
         EXIT

      Case K_DEL
         IF RecNo() != LastRec() + 1
            IF Deleted()
               dbRecall()
            ELSE
               dbDelete()
            ENDIF
         ENDIF
         EXIT

      Case K_ENTER
         IF lAppend .OR. RecNo() != LastRec() + 1
            lKeyPressed := (nKey := DoGet(oBrw, lAppend)) != 0
         ELSE
            nKey := K_DOWN
            lKeyPressed := True
         ENDIF
         EXIT

      Case K_ESC
         lContinue := False
         EXIT

      Otherwise
         IF !hb_keyChar(nKey) == ""
            hb_keyIns(nKey)
            nKey := K_ENTER
            lKeyPressed := True
         ENDIF
      EndSwitch

      IF lRefresh
         lRefresh := lAppend := False
         FreshOrder(oBrw)
         SetCursor(SC_NONE)
      ENDIF

   EndDo

   RestScreen(nTop, nLeft, nBottom, nRight, cOldScreen)
   SetCursor(nOldCursor)

   Return True

Static Procedure StatLine(oBrw, lAppend)

   LOCAL nTop   := oBrw:nTop - 1
   LOCAL nRight := oBrw:nRight
   LOCAL nRecNo := RecNo()
   LOCAL nLastRec := LastRec()

   hb_DispOutAt(nTop, nRight - 27, "Record ")

   IF nLastRec == 0 .AND. !lAppend
      hb_DispOutAt(nTop, nRight - 20, "<none>               ")
   ELSEIF nRecNo == nLastRec + 1
      hb_DispOutAt(nTop, nRight - 40, "         ")
      hb_DispOutAt(nTop, nRight - 20, "                <new>")
   ELSE
      hb_DispOutAt(nTop, nRight - 40, IIf(Deleted(), "<Deleted>", "         "))
      hb_DispOutAt(nTop, nRight - 20, PadR(hb_ntos(nRecNo) + "/" + hb_ntos(nLastRec), 16) + IIf(oBrw:HitTop, "<bof>", "     "))
   ENDIF

   RETURN

Static Function DoGet(oBrw, lAppend)

   LOCAL lScore
   LOCAL lExit
   LOCAL bIns
   LOCAL nCursor
   LOCAL oCol
   LOCAL oGet
   LOCAL cIndexKey
   LOCAL cForExp
   LOCAL xKeyValue
   LOCAL bIndexKey
   LOCAL lSuccess
   LOCAL nKey
   LOCAL xValue

   oBrw:HitTop := False
   StatLine(oBrw, lAppend)
   oBrw:ForceStable()

   lScore := Set(_SET_SCOREBOARD, False)
   lExit := Set(_SET_EXIT, True)
   bIns := SetKey(K_INS, {||SetCursor(IIf(ReadInsert(!ReadInsert()), SC_NORMAL, SC_INSERT))})
   nCursor := SetCursor(IIf(ReadInsert(), SC_INSERT, SC_NORMAL))
   IF !Empty(cIndexKey := IndexKey(0))
      xKeyValue := Eval(bIndexKey := hb_macroBlock(cIndexKey))
   ENDIF

   oCol := oBrw:GetColumn(oBrw:ColPos)
   xValue := Eval(oCol:Block)
   oGet := GetNew(Row(), Col(), {|xNewVal|IIf(PCount() == 0, xValue, xValue := xNewVal)}, "mGetVar", NIL, oBrw:ColorSpec)
   lSuccess := False
   IF ReadModal({oGet})
      IF lAppend .AND. RecNo() == LastRec() + 1
         dbAppend()
      ENDIF
      Eval(oCol:Block, xValue)

      IF !lAppend .AND. !Empty(cForExp := ordFor(IndexOrd())) .AND. !Eval(hb_macroBlock(cForExp))
         dbGoTop()
      ENDIF
      IF !lAppend .AND. !Empty(bIndexKey) .AND. !xKeyValue == Eval(bIndexKey)
         lSuccess := True
      ENDIF
   ENDIF

   IF lSuccess
      FreshOrder(oBrw)
      nKey := 0
   ELSE
      oBrw:RefreshCurrent()
      nKey := ExitKey(lAppend)
   ENDIF

   IF lAppend
      oBrw:ColorRect({oBrw:RowPos, 1, oBrw:RowPos, oBrw:ColCount}, {2, 2})
   ENDIF

   SetCursor(nCursor)
   SetKey(K_INS, bIns)
   Set(_SET_EXIT, lExit)
   Set(_SET_SCOREBOARD, lScore)

   RETURN nKey

Static Function ExitKey(lAppend)

   LOCAL nKey

   Switch nKey := LastKey()
   Case K_PGDN
      RETURN IIf(lAppend, 0, K_DOWN)
   Case K_PGUP
      RETURN IIf(lAppend, 0, K_UP)
   Case K_DOWN
   Case K_UP
      RETURN nKey
   EndSwitch

   RETURN IIf(nKey == K_ENTER .OR. !(hb_keyChar(nKey) == ""), K_RIGHT, 0)

Static Procedure FreshOrder(oBrw)

   LOCAL nRec := RecNo()

   oBrw:RefreshAll()
   oBrw:ForceStable()

   IF nRec != LastRec() + 1
      Do While RecNo() != nRec .AND. !Bof()
         oBrw:Up()
         oBrw:ForceStable()
      EndDo
   ENDIF

   RETURN

Static Function Skipped(nRecs, lAppend)

   LOCAL nSkipped := 0

   IF LastRec() != 0
      Do Case
      Case nRecs == 0
         dbSkip(0)
      Case nRecs > 0 .AND. RecNo() != LastRec() + 1
         Do While nSkipped < nRecs
            dbSkip()
            IF Eof()
               IF lAppend
                  ++nSkipped
               ELSE
                  dbSkip(-1)
               ENDIF
               EXIT
            ENDIF
            ++nSkipped
         EndDo
      Case nRecs < 0
         Do While nSkipped > nRecs
            dbSkip(-1)
            IF Bof()
               EXIT
            ENDIF
            --nSkipped
         EndDo
      EndCase
   ENDIF

   RETURN nSkipped
