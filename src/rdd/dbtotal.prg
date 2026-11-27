//
// __dbTotal() function
//
// Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "dbstruct.ch"
#include "error.ch"

// NOTE: Compared to CA-Cl*pper, Harbour:
//       - will accept character expressions and symbols for xKey, xFor and xWhile.
//       - has three extra parameters (cRDD, nConnection, cCodePage).
//       - will default to active index key for xKey parameter.
//       - won't crash with "No exported method: EVAL" if xKey is not
//         block and table is not indexed.

Function __dbTotal(cFile, xKey, aFields, xFor, xWhile, nNext, nRec, lRest, cRDD, nConnection, cCodePage)

   Local nOldArea
   Local nNewArea
   Local aNewDbStruct
   Local aGetField
   Local aFieldsSum
   Local lDbTransRecord
   Local xCurKey
   Local bWhileBlock
   Local bForBlock
   Local bKeyBlock
   Local oError
   Local lError := False

   Do Case
   Case hb_IsEvalItem(xWhile)
      bWhileBlock := xWhile
      lRest := True
   Case hb_IsString(xWhile) .AND. !Empty(xWhile)
      bWhileBlock := hb_macroBlock(xWhile)
      lRest := True
   Otherwise
      bWhileBlock := {|| True }
   EndCase

   Do Case
   Case hb_IsEvalItem(xFor)
      bForBlock := xFor
   Case hb_IsString(xFor) .AND. !Empty(xFor)
      bForBlock := hb_macroBlock(xFor)
   Otherwise
      bForBlock := {|| True }
   EndCase

   __defaultNIL(@lRest, False)

   IF nRec != NIL
      dbGoto(nRec)
      nNext := 1
   ELSEIF nNext == NIL
      nNext := -1
      IF !lRest
         dbGoTop()
      ENDIF
   ELSE
      lRest := True
   ENDIF

   nOldArea := Select()

   aNewDbStruct := {}
   AEval(dbStruct(), {| aField | IIf(aField[DBS_TYPE] == "M", NIL, AAdd(aNewDbStruct, aField)) })
   IF Empty(aNewDbStruct)
      Return False
   ENDIF

   BEGIN SEQUENCE

      IF hb_IsEvalItem(xKey)
         bKeyBlock := xKey
      ELSE
         IF Empty(xKey)
            xKey := ordKey()
         ENDIF
         IF hb_IsString(xKey) .AND. !Empty(xKey)
            bKeyBlock := hb_macroBlock(xKey)
         ELSE
            bKeyBlock := {|| NIL }
         ENDIF
      ENDIF

      aGetField := {}
      AEval(aFields, {| cField | AAdd(aGetField, __GetField(cField)) })
      aFieldsSum := Array(Len(aGetField))

      // Keep it open after creating it.
      dbCreate(cFile, aNewDbStruct, cRDD, True, "", NIL, cCodePage, nConnection)
      nNewArea := Select()

      dbSelectArea(nOldArea)
      Do While !Eof() .AND. nNext != 0 .AND. Eval(bWhileBlock)

         lDbTransRecord := False

         AFill(aFieldsSum, 0)

         xCurKey := Eval(bKeyBlock)

         Do While !Eof() .AND. nNext-- != 0 .AND. Eval(bWhileBlock) .AND. ;
               xCurKey == Eval(bKeyBlock)

            IF Eval(bForBlock)
               IF !lDbTransRecord
                  __dbTransRec(nNewArea, aNewDbStruct)
                  dbSelectArea(nOldArea)
                  lDbTransRecord := True
               ENDIF
               AEval(aGetField, {| bFieldBlock, nFieldPos | aFieldsSum[nFieldPos] += Eval(bFieldBlock) })
            ENDIF

            dbSkip()
         EndDo

         IF lDbTransRecord
            dbSelectArea(nNewArea)
            AEval(aGetField, {| bFieldBlock, nFieldPos | Eval(bFieldBlock, aFieldsSum[nFieldPos]) })
            dbSelectArea(nOldArea)
         ENDIF

      EndDo

   RECOVER USING oError
      lError := True
   END SEQUENCE

   IF nNewArea != NIL
      dbSelectArea(nNewArea)
      dbCloseArea()
   ENDIF

   dbSelectArea(nOldArea)

   IF lError
      Break(oError)
   ENDIF

   Return True

Static Function __GetField(cField)

   Local nCurrArea := Select()
   Local nPos
   Local oError

   // Is the field aliased?
   IF (nPos := At("->", cField)) > 0

      IF Select(Left(cField, nPos - 1)) != nCurrArea

         oError := ErrorNew()
         oError:severity   := ES_ERROR
         oError:genCode    := EG_SYNTAX
         oError:subSystem  := "DBCMD"
         oError:canDefault := True
         oError:operation  := cField
         oError:subCode    := 1101

         IF hb_defaultValue(Eval(ErrorBlock(), oError), True)
            __errInHandler()
         ENDIF

         Break(oError)
      ENDIF

      cField := SubStr(cField, nPos + 2)
   ENDIF

   RETURN FieldBlock(cField)

Function __dbTransRec(nDstArea, aFieldsStru)
   RETURN __dbTrans(nDstArea, aFieldsStru, NIL, NIL, 1)
