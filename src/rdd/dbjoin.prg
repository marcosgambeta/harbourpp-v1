//
// __dbJoin() function
//
// Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
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

// NOTE: Compared to CA-Cl*pper, Harbour has three extra parameters
//       (cRDD, nConnection, cCodePage).

Function __dbJoin(cAlias, cFile, aFields, bFor, cRDD, nConnection, cCodePage)

   Local nMaster := Select()
   Local nDetail := Select(cAlias)
   Local nResult
   Local aStruct
   Local aJoinList
   Local oError
   Local lError := False

   dbSelectArea(nMaster)
   IF Empty(aStruct := __FieldTwo(cAlias, aFields))
      // NOTE: CA-Cl*pper will leave the wrong workarea (cAlias) selected here.
      //       Harbour is bug compatible. [vszakats]
      Return False
   ENDIF

   BEGIN SEQUENCE

      dbCreate(cFile, aStruct, cRDD, True, "", NIL, cCodePage, nConnection)
      nResult := Select()
      aJoinList := __JoinList(nMaster, nDetail, nResult, aStruct)

      dbSelectArea(nMaster)
      dbGoTop()
      Do While !Eof()

         dbSelectArea(nDetail)
         dbGoTop()
         Do While !Eof()

            dbSelectArea(nMaster)
            IF Eval(bFor)
               __doJoinList(aJoinList)
            ENDIF

            dbSelectArea(nDetail)
            dbSkip()
         EndDo

         dbSelectArea(nMaster)
         dbSkip()
      EndDo

   RECOVER USING oError
      lError := True
   END SEQUENCE

   IF nResult != NIL
      dbSelectArea(nResult)
      dbCloseArea()
   ENDIF

   dbSelectArea(nMaster)

   IF lError
      Break(oError)
   ENDIF

   Return True

Static Function __FieldTwo(cAlias, aFields)

   Local aFldTemp
   Local bFind
   Local aStruct
   Local cField

   IF Empty(aFields)
      RETURN dbStruct()
   ENDIF

   aFldTemp := {}
   AEval(aFields, {| cFld | AAdd(aFldTemp, RTrim(Upper(cFld))) })

   aFields := aFldTemp

   aStruct := {}
   bFind := {| cFld | cFld == cField }
   AEval(dbStruct(), {|aFld|cField := aFld[DBS_NAME], IIf(AScan(aFields, bFind) == 0, NIL, AAdd(aStruct, aFld))})

   dbSelectArea(cAlias)
   bFind := {| cFld | "->" $ cFld .AND. SubStr(cFld, At("->", cFld) + 2) == cField }
   AEval(dbStruct(), {|aFld|cField := aFld[DBS_NAME], IIf(AScan(aFields, bFind) == 0, NIL, AAdd(aStruct, aFld))})

   RETURN aStruct

Static Function __JoinList(nMaster, nDetail, nResult, aStruct)

   Local aList := {}
   Local nPos
   Local i

   For i := 1 To Len(aStruct)
      Do Case
      Case (nPos := (nMaster)->(FieldPos(aStruct[i][DBS_NAME]))) != 0
         AAdd(aList, { nResult, nMaster, nPos, i })
      Case (nPos := (nDetail)->(FieldPos(aStruct[i][DBS_NAME]))) != 0
         AAdd(aList, { nResult, nDetail, nPos, i })
      EndCase
   Next

   RETURN aList

Static Procedure __doJoinList(aList)

   Local aJoin

   IF Len(aList) > 0

      (aList[1][1])->(dbAppend())

      For Each aJoin In aList
         (aJoin[1])->(FieldPut(aJoin[4], (aJoin[2])->(FieldGet(aJoin[3]))))
      Next
   ENDIF

   RETURN
