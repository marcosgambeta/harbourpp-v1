//
// dbEdit() function
//
// Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "dbedit.ch"
#include "inkey.ch"
#include "setcurs.ch"

// NOTE: Extension: Harbour supports codeblocks and function pointers
//       as the xUserFunc parameter. [vszakats]
// NOTE: Clipper is buggy and will throw an error if the number of
//       columns is zero. (Check: dbEdit(0,0,20,20,{})) [vszakats]
// NOTE: Clipper will throw an error if there's no database open [vszakats]
// NOTE: The NG says that the return value is NIL, but it's not. [vszakats]
// NOTE: Harbour is multithreading ready and Clipper only reentrant safe
//       [vszakats]

Function dbEdit(nTop, nLeft, nBottom, nRight, acColumns, xUserFunc, xColumnSayPictures, xColumnHeaders, ;
      xHeadingSeparators, xColumnSeparators, xFootingSeparators, xColumnFootings)

   Local nOldCUrsor
   Local nKey
   Local lContinue
   Local nPos
   Local nAliasPos
   Local nColCount
   Local lDoIdleCall
   Local lAppend
   Local lFlag
   Local cHeading
   Local cBlock
   Local bBlock
   Local oBrowse
   Local oColumn
   Local aCol

   IF !Used()
      Return False
   ELSEIF Eof()
      dbGoBottom()
   ENDIF

   IF !hb_IsNumeric(nTop) .OR. nTop < 0
      nTop := 0
   ENDIF
   IF !hb_IsNumeric(nLeft) .OR. nLeft < 0
      nLeft := 0
   ENDIF
   IF !hb_IsNumeric(nBottom) .OR. nBottom > MaxRow() .OR. nBottom < nTop
      nBottom := MaxRow()
   ENDIF
   IF !hb_IsNumeric(nRight) .OR. nRight > MaxCol() .OR. nRight < nLeft
      nRight := MaxCol()
   ENDIF

   oBrowse := TBrowseDB(nTop, nLeft, nBottom, nRight)
   oBrowse:headSep   := IIf(hb_IsString(xHeadingSeparators), xHeadingSeparators, hb_UTF8ToStrBox("═╤═"))
   oBrowse:colSep    := IIf(hb_IsString(xColumnSeparators), xColumnSeparators, hb_UTF8ToStrBox(" │ "))
   oBrowse:footSep   := hb_defaultValue(xFootingSeparators, "")
   oBrowse:skipBlock := {|nRecs|Skipped(nRecs, lAppend)}
   oBrowse:autoLite  := False  // Set to False just like in CA-Cl*pper. [vszakats]

   IF hb_IsArray(acColumns)
      nColCount := 0
      For Each aCol In acColumns
         IF hb_IsString(aCol) .AND. !Empty(aCol)
            nColCount++
         ELSE
            EXIT
         ENDIF
      Next
   ELSE
      nColCount := FCount()
   ENDIF

   IF nColCount == 0
      Return False
   ENDIF

   // Generate the TBrowse columns

   FOR nPos := 1 TO nColCount

      IF hb_IsArray(acColumns)
         cBlock := acColumns[nPos]
         IF (nAliasPos := At("->", cBlock)) > 0
            cHeading := Left(cBlock, nAliasPos - 1) + "->;" + SubStr(cBlock, nAliasPos + 2)
         ELSE
            cHeading := cBlock
         ENDIF
      ELSE
         cBlock := FieldName(nPos)
         cHeading := cBlock
      ENDIF

      // Simplified logic compared to CA-Cl*pper. In the latter there
      // is logic to detect several typical cBlock types (memvar,
      // aliased field, field) and using MemVarBlock()/FieldWBlock()/FieldBlock()
      // calls to create codeblocks for them if possible. In Harbour,
      // simple macro compilation will result in faster code for all
      // situations. As Maurilio Longo has pointed, there is no point in
      // creating codeblocks which are able to _assign_ values, as dbEdit()
      // is a read-only function. [vszakats]

      bBlock := IIf(Type(cBlock) == "M", {||"  <Memo>  "}, hb_macroBlock(cBlock))

      Do Case
      Case hb_IsArray(xColumnHeaders) .AND. Len(xColumnHeaders) >= nPos .AND. hb_IsString(xColumnHeaders[nPos])
         cHeading := xColumnHeaders[nPos]
      Case hb_IsString(xColumnHeaders)
         cHeading := xColumnHeaders
      EndCase

      oColumn := TBColumnNew(cHeading, bBlock)

      Do Case
      Case hb_IsArray(xColumnSayPictures) .AND. nPos <= Len(xColumnSayPictures) .AND. hb_IsString(xColumnSayPictures[nPos]) .AND. !Empty(xColumnSayPictures[nPos])
         oColumn:picture := xColumnSayPictures[nPos]
      Case hb_IsString(xColumnSayPictures) .AND. !Empty(xColumnSayPictures)
         oColumn:picture := xColumnSayPictures
      EndCase

      Do Case
      Case hb_IsArray(xColumnFootings) .AND. nPos <= Len(xColumnFootings) .AND. hb_IsString(xColumnFootings[nPos])
         oColumn:footing := xColumnFootings[nPos]
      Case hb_IsString(xColumnFootings)
         oColumn:footing := xColumnFootings
      EndCase

      IF hb_IsArray(xHeadingSeparators) .AND. nPos <= Len(xHeadingSeparators) .AND. hb_IsString(xHeadingSeparators[nPos])
         oColumn:headSep := xHeadingSeparators[nPos]
      ENDIF

      IF hb_IsArray(xColumnSeparators) .AND. nPos <= Len(xColumnSeparators) .AND. hb_IsString(xColumnSeparators[nPos])
         oColumn:colSep := xColumnSeparators[nPos]
      ENDIF

      IF hb_IsArray(xFootingSeparators) .AND. nPos <= Len(xFootingSeparators) .AND. hb_IsString(xFootingSeparators[nPos])
         oColumn:footSep := xFootingSeparators[nPos]
      ENDIF

      oBrowse:addColumn(oColumn)
   NEXT

   nOldCUrsor := SetCursor(SC_NONE)

   // Go into the processing loop

   lAppend := False
   lFlag := True
   lDoIdleCall := True
   lContinue := True

   Do While lContinue

      Do While True
         nKey := Inkey()
         IF oBrowse:stabilize()
            EXIT
         ENDIF
#ifdef HB_COMPAT_C53
         IF nKey != 0 .AND. nKey != K_MOUSEMOVE
            EXIT
         ENDIF
#else
         IF nKey != 0
            EXIT
         ENDIF
#endif
      EndDo

      IF nKey == 0
         IF lDoIdleCall
            lContinue := CallUser(oBrowse, xUserFunc, 0, @lAppend, @lFlag)
            oBrowse:forceStable()
         ENDIF
         IF lContinue .AND. lFlag
            oBrowse:hiLite()
#ifdef HB_COMPAT_C53
            Do While (nKey := Inkey(0)) == K_MOUSEMOVE
            EndDo
#else
            nKey := Inkey(0)
#endif
            oBrowse:deHilite()
            IF (bBlock := SetKey(nKey)) != NIL
               Eval(bBlock, ProcName(1), ProcLine(1), "")
               LOOP
            ENDIF
         ELSE
            lFlag := True
         ENDIF
      ENDIF

      lDoIdleCall := True

      IF nKey != 0
#ifdef HB_CLP_UNDOC
         IF lAppend
            Switch nKey
            Case K_DOWN
            Case K_PGDN
            Case K_CTRL_PGDN
               oBrowse:hitBottom := True
               LOOP
            Case K_UP
            Case K_PGUP
            Case K_CTRL_PGUP
               oBrowse:hitTop := True
               LOOP
            EndSwitch
         ENDIF
#endif
         Switch nKey
#ifdef HB_COMPAT_C53
         Case K_LBUTTONDOWN
         Case K_LDBLCLK
            TBMouse(oBrowse, MRow(), MCol())
            EXIT
#endif
         Case K_DOWN          ; oBrowse:down()     ; EXIT
         Case K_UP            ; oBrowse:up()       ; EXIT
         Case K_PGDN          ; oBrowse:pageDown() ; EXIT
         Case K_PGUP          ; oBrowse:pageUp()   ; EXIT
         Case K_CTRL_PGUP     ; oBrowse:goTop()    ; EXIT
         Case K_CTRL_PGDN     ; oBrowse:goBottom() ; EXIT
         Case K_RIGHT         ; oBrowse:right()    ; EXIT
         Case K_LEFT          ; oBrowse:left()     ; EXIT
         Case K_HOME          ; oBrowse:home()     ; EXIT
         Case K_END           ; oBrowse:end()      ; EXIT
         Case K_CTRL_LEFT     ; oBrowse:panLeft()  ; EXIT
         Case K_CTRL_RIGHT    ; oBrowse:panRight() ; EXIT
         Case K_CTRL_HOME     ; oBrowse:panHome()  ; EXIT
         Case K_CTRL_END      ; oBrowse:panEnd()   ; EXIT
         Otherwise
            lContinue := CallUser(oBrowse, xUserFunc, nKey, @lAppend, @lFlag)
            lDoIdleCall := False
         EndSwitch
      ENDIF
   EndDo

   SetCursor(nOldCUrsor)

   Return True


// NOTE: CA-Cl*pper uses intermediate function CallUser()
//       to execute user function. We're replicating this behavior
//       for code which may check ProcName() results in user function
Static Function CallUser(oBrowse, xUserFunc, nKey, lAppend, lFlag)

   Local nPrevRecNo
   Local nAction
   Local nMode := ;
      IIf(nKey != 0,                  DE_EXCEPT,    ;
      IIf(!lAppend .AND. IsDbEmpty(), DE_EMPTY,     ;
      IIf(oBrowse:hitBottom,          DE_HITBOTTOM, ;
      IIf(oBrowse:hitTop,             DE_HITTOP, DE_IDLE))))

   oBrowse:forceStable()

   nPrevRecNo := RecNo()

   // NOTE: CA-Cl*pper won't check the type of the return value here,
   //       and will crash if it's a non-NIL, non-numeric type. We're
   //       replicating this behavior.
   nAction := IIf(hb_IsEvalItem(xUserFunc), ;
                                 Eval(xUserFunc, nMode, oBrowse:colPos), ;
              IIf(hb_IsString(xUserFunc) .AND. !Empty(xUserFunc), ;
                                 &xUserFunc(nMode, oBrowse:colPos), ;  // NOTE: Macro operator!
              IIf(nKey == K_ENTER .OR. nKey == K_ESC, DE_ABORT, DE_CONT)))

   IF !lAppend .AND. Eof() .AND. !IsDbEmpty()
      dbSkip(-1)
   ENDIF

#ifdef HB_CLP_UNDOC
   IF nAction == DE_APPEND

      IF (lAppend := !(lAppend .AND. Eof()))
         dbGoBottom()
         oBrowse:down()
      ELSE
         oBrowse:refreshAll():forceStable()
      ENDIF
      lFlag := False
      Return True
   ENDIF
#endif

   IF nAction == DE_REFRESH .OR. nPrevRecNo != RecNo()

      IF nAction != DE_ABORT

         lAppend := False

         IF (Set(_SET_DELETED) .AND. Deleted()) .OR. (!Empty(dbFilter()) .AND. !Eval(hb_macroBlock(dbFilter())))
            dbSkip()
         ENDIF
         IF Eof()
            dbGoBottom()
         ENDIF

         nPrevRecNo := RecNo()
         oBrowse:refreshAll():forceStable()
         Do While nPrevRecNo != RecNo()
            oBrowse:Up():forceStable()
         EndDo

         lFlag := False

      ENDIF
   ELSE
      oBrowse:refreshCurrent()
   ENDIF

   RETURN nAction != DE_ABORT


// helper function to detect empty tables. It's not perfect but
// it functionally uses the same conditions as CA-Cl*pper
Static Function IsDbEmpty()
   RETURN LastRec() == 0 .OR. (Bof() .AND. (Eof() .OR. RecNo() == LastRec() + 1))

// Helper function: TBrowse skipBlock
Static Function Skipped(nRecs, lAppend)

   Local nSkipped := 0

   IF LastRec() != 0
      Do Case
      Case nRecs == 0
         IF Eof() .AND. !lAppend
            dbSkip(-1)
            nSkipped := -1
         ELSE
            dbSkip(0)
         ENDIF
      Case nRecs > 0 .AND. RecNo() != LastRec() + 1
         Do While nSkipped < nRecs
            dbSkip()
            IF Eof()
               IF lAppend
                  nSkipped++
               ELSE
                  dbSkip(-1)
               ENDIF
               EXIT
            ENDIF
            nSkipped++
         EndDo
      Case nRecs < 0
         Do While nSkipped > nRecs
            dbSkip(-1)
            IF Bof()
               EXIT
            ENDIF
            nSkipped--
         EndDo
      EndCase
   ENDIF

   RETURN nSkipped
