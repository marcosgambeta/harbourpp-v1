//
// SIX compatible functions:
//       sxChar()
//       sxNum()
//       sxDate()
//       sxLog()
//       sx_Compress()
//       sx_Decompress()
//       sx_TagInfo()
//       sx_TagCount()
//       sx_Tags()
//       sx_SetTag()
//       sx_KillTag()
//       sx_FileOrder()
//       sx_SetFileOrd()
//       rdd_Count()
//       rdd_Name()
//       rdd_Info()
//       sx_IsDBT()
//       sx_AutoOpen()
//       sx_AutoShare()
//       sx_Blob2File()
//       sx_File2Blob()
//       sx_dbCreate()
//       sx_VSigLen()
//       sx_MemoExt()
//       sx_MemoBlk()
//       sx_SetMemoBlock()
//       sx_StrXCheck()
//       sx_LockRetry()
//       sx_IsLocked()
//       sx_SetTrigger()
//       sx_VFGet()
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "dbinfo.ch"
#include "dbstruct.ch"
#include "fileio.ch"
#include "ord.ch"
#include "hbsxdef.ch"

FUNCTION sxChar(nLen, /* @ */ xKeyVal)

   SWITCH ValType(xKeyVal)
   CASE "C"
   CASE "M"
      EXIT
   CASE "N"
      xKeyVal := Str(xKeyVal)
      EXIT
   CASE "D"
      xKeyVal := DToS(xKeyVal)
      EXIT
   CASE "T"
      xKeyVal := hb_TToS(xKeyVal)
      EXIT
   CASE "L"
      xKeyVal := IIf(xKeyVal, "T", "F")
      EXIT
   OTHERWISE
      xKeyVal := IIf(hb_IsNumeric(nLen), "", Space(10))
      EXIT
   ENDSWITCH

   IF hb_IsNumeric(nLen)
      xKeyVal := PadR(LTrim(xKeyVal), nLen)
   ENDIF

   RETURN xKeyVal

FUNCTION sxNum(/* @ */ xKeyVal)

   SWITCH ValType(xKeyVal)
   CASE "N"
      EXIT
   CASE "C"
   CASE "M"
      xKeyVal := Val(xKeyVal)
      EXIT
   CASE "T"
   CASE "D"
      xKeyVal := xKeyVal - hb_SToD()
      EXIT
   CASE "L"
      xKeyVal := IIf(xKeyVal, 1, 0)
      EXIT
   OTHERWISE
      xKeyVal := 0.00
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sxDate(/* @ */ xKeyVal)

   SWITCH ValType(xKeyVal)
   CASE "D"
      EXIT
   CASE "T"
      xKeyVal := hb_TToD(xKeyVal)
      EXIT
   CASE "C"
   CASE "M"
      xKeyVal := CToD(xKeyVal)
      EXIT
   CASE "N"
      xKeyVal := hb_SToD() + xKeyVal
      EXIT
   OTHERWISE
      xKeyVal := hb_SToD()
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sxLog(/* @ */ xKeyVal)

   SWITCH ValType(xKeyVal)
   CASE "L"
      EXIT
   CASE "C"
   CASE "M"
      SWITCH xKeyVal
      CASE  "T";  CASE  "t";  CASE  "Y";  CASE  "y"
      CASE ".T."; CASE ".t."; CASE ".Y."; CASE ".y."
         xKeyVal := .T.
         EXIT
      OTHERWISE
         xKeyVal := .F.
         EXIT
      ENDSWITCH
      EXIT
   CASE "N"
      xKeyVal := xKeyVal != 0
      EXIT
   OTHERWISE
      xKeyVal := .F.
      EXIT
   ENDSWITCH

   RETURN xKeyVal

FUNCTION sx_Compress(xVal)

   LOCAL xRetVal

   SWITCH ValType(xVal)
   CASE "C"
   CASE "M"
      RETURN _sx_StrCompress(xVal)
   CASE "A"
      xRetVal := Array(Len(xVal))
      AEval(xVal, {| x, i | xRetVal[i] := sx_Compress(x) })
      RETURN xRetVal
   ENDSWITCH

   RETURN xVal

FUNCTION sx_Decompress(xVal)

   LOCAL xRetVal

   SWITCH ValType(xVal)
   CASE "C"
   CASE "M"
      RETURN _sx_StrDecompress(xVal)
   CASE "A"
      xRetVal := Array(Len(xVal))
      AEval(xVal, {| x, i | xRetVal[i] := sx_Decompress(x) })
      RETURN xRetVal
   ENDSWITCH

   RETURN xVal

FUNCTION sx_TagInfo(cIndex)

   LOCAL aInfo
   LOCAL nOrds
   LOCAL nFirst
   LOCAL i

   IF Used() .AND. (nOrds := ordCount(cIndex)) > 0
      aInfo := Array(nOrds, 6)
      IF hb_IsString(cIndex)
         nFirst := dbOrderInfo(DBOI_BAGORDER, cIndex)
         nOrds += nFirst - 1
      ELSE
         nFirst := 1
      ENDIF
      FOR i := nFirst TO nOrds
         aInfo[i][1] := ordName(i)
         aInfo[i][2] := ordKey(i)
         aInfo[i][3] := ordFor(i)
         aInfo[i][4] := ordIsUnique(i)
         aInfo[i][5] := ordDescend(i)
         aInfo[i][6] := ordCustom(i)
      NEXT
      RETURN aInfo
   ENDIF

   RETURN {}

FUNCTION sx_TagCount(xIndex)

   LOCAL nTags := 0
   LOCAL cIndex
   LOCAL nOrder

   IF Used()
      DO CASE
      CASE hb_IsNumeric(xIndex)
         IF (nOrder := sx_TagOrder(1, xIndex)) != 0
            cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
         ENDIF
      CASE hb_IsString(xIndex) .AND. !Empty(xIndex)
         cIndex := xIndex
      OTHERWISE
         cIndex := dbOrderInfo(DBOI_FULLPATH)
      ENDCASE
      IF !Empty(cIndex)
         nTags := ordCount(cIndex)
      ENDIF
   ENDIF

   RETURN nTags

FUNCTION sx_Tags(xIndex)

   LOCAL aTagNames := {}
   LOCAL nOrder
   LOCAL nTags

   IF Used()
      DO CASE
      CASE hb_IsNumeric(xIndex)
         nOrder := sx_TagOrder(1, xIndex)
      CASE hb_IsString(xIndex) .AND. !Empty(xIndex)
         nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex)
      OTHERWISE
         nOrder := ordNumber()
      ENDCASE
      IF nOrder != 0
         nTags := ordCount(dbOrderInfo(DBOI_FULLPATH, NIL, nOrder))
         DO WHILE --nTags >= 0
            AAdd(aTagNames, ordName(nOrder++))
         ENDDO
      ENDIF
   ENDIF

   RETURN aTagNames

FUNCTION sx_SetTag(xTag, xIndex)

   LOCAL lRet := .F.
   LOCAL nOrder := 0
   LOCAL nOldOrd
   LOCAL cIndex

   IF Used() .AND. ValType(xTag) $ "CN"
      IF hb_IsNumeric(xTag)
         IF Empty(xIndex) .OR. !ValType(xIndex) $ "CN"
            nOrder := xTag
         ELSEIF hb_IsString(xIndex)
            IF xTag >= 1 .AND. xTag <= ordCount(xIndex)
               nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex) + xTag - 1
            ENDIF
         ELSE
            nOrder := sx_TagOrder(xTag, xIndex)
         ENDIF
      ELSE
         IF Empty(xIndex) .OR. !ValType(xIndex) $ "CN"
            nOrder := ordNumber(xTag)
         ELSEIF hb_IsString(xIndex)
            nOrder := sx_TagOrder(xTag, xIndex)
         ELSE
            nOrder := sx_TagOrder(1, xIndex)
            IF nOrder != 0
               cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
               IF Empty(cIndex)
                  nOrder := 0
               ELSE
                  nOrder := sx_TagOrder(xTag, cIndex)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF nOrder != 0
         nOldOrd := ordNumber()
         ordSetFocus(nOrder)
         lRet := nOrder == ordNumber()
         IF !lRet
            ordSetFocus(nOldOrd)
         ENDIF
      ELSEIF Empty(xTag)
         ordSetFocus(0)
         lRet := .T.
      ENDIF
   ENDIF

   RETURN lRet

FUNCTION sx_KillTag(xTag, xIndex)

   LOCAL lRet := .F.
   LOCAL nOrder
   LOCAL cIndex

   IF hb_IsLogical(xTag)
      IF xTag
         DO CASE
         CASE Empty(xIndex)
            cIndex := sx_IndexName()
         CASE hb_IsNumeric(xIndex)
            cIndex := sx_IndexName(1, xIndex)
         CASE hb_IsString(xIndex)
            nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex)
            IF nOrder != 0
               cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
            ENDIF
         ENDCASE
         IF !Empty(cIndex)
            IF ordBagClear(cIndex)
               lRet := FErase(cIndex) != F_ERROR
            ENDIF
         ENDIF
      ENDIF
   ELSE
      IF hb_IsNumeric(xTag)
         IF Empty(xIndex) .OR. !ValType(xIndex) $ "CN"
            nOrder := xTag
         ELSEIF hb_IsString(xIndex)
            IF xTag >= 1 .AND. xTag <= ordCount(xIndex)
               nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex) + xTag - 1
            ELSE
               nOrder := 0
            ENDIF
         ELSE
            nOrder := sx_TagOrder(xTag, xIndex)
         ENDIF
      ELSE
         IF Empty(xIndex) .OR. !ValType(xIndex) $ "CN"
            nOrder := ordNumber(xTag)
         ELSEIF hb_IsString(xIndex)
            nOrder := sx_TagOrder(xTag, xIndex)
         ELSE
            IF (nOrder := sx_TagOrder(1, xIndex)) != 0
               cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
               IF Empty(cIndex)
                  nOrder := 0
               ELSE
                  nOrder := sx_TagOrder(xTag, cIndex)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF nOrder != 0
         lRet := ordDestroy(nOrder)
      ENDIF
   ENDIF

   RETURN lRet

FUNCTION sx_FileOrder()
   RETURN dbOrderInfo(DBOI_BAGNUMBER)

FUNCTION sx_SetFileOrd(nIndex)
   RETURN IIf(hb_IsNumeric(nIndex), ordSetFocus(sx_TagOrder(1, nIndex)), ordSetFocus())

FUNCTION rdd_Count()
   RETURN Len(rddList())

FUNCTION rdd_Name(nRDD)

   LOCAL aRDD

   IF hb_IsNumeric(nRDD) .AND. nRDD >= 1
      aRDD := rddList()
      IF nRDD <= Len(aRDD)
         RETURN aRDD[nRDD]
      ENDIF
   ENDIF

   RETURN ""

FUNCTION rdd_Info(xID)

   LOCAL cRDD

   DO CASE
   CASE hb_IsNumeric(xID)
      IF !Alias(xID) == ""
         (xID)->(rddName())
      ENDIF
   CASE hb_IsString(xID)
      cRDD := Upper(AllTrim(xID))
      IF AScan(rddList(), {| x | Upper(x) == cRDD }) == 0
         cRDD := NIL
      ENDIF
   CASE xID == NIL
      cRDD := rddSetDefault()
   ENDCASE

   IF Empty(cRDD)
      RETURN {}
   ENDIF

   RETURN { ;
      cRDD, ;
      .T., ;
      hb_rddInfo(RDDI_TABLEEXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_ORDBAGEXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_ORDEREXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_MEMOEXT, NIL, cRDD) }

FUNCTION sx_IsDBT(cRDD)
   RETURN hb_rddInfo(RDDI_MEMOTYPE, NIL, cRDD) == DB_MEMO_DBT

FUNCTION sx_MemoExt(cNewExt, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOEXT, cNewExt, cRDD)

FUNCTION sx_MemoBlk(nNewBlock, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD)

FUNCTION sx_SetMemoBlock(nNewBlock, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD)

FUNCTION sx_StrXCheck(lStrict, cRDD)
   RETURN hb_rddInfo(RDDI_STRICTSTRUCT, lStrict, cRDD)

FUNCTION sx_LockRetry(nRetry, cRDD)
   RETURN hb_rddInfo(RDDI_LOCKRETRY, nRetry, cRDD)

FUNCTION sx_AutoOpen(lAuto, cRDD)
   RETURN hb_rddInfo(RDDI_AUTOOPEN, lAuto, cRDD)

FUNCTION sx_AutoShare(lAuto, cRDD)
   RETURN hb_rddInfo(RDDI_AUTOSHARE, lAuto, cRDD)

FUNCTION sx_Blob2File(cFileName, cFldName)
   RETURN dbFileGet(cFldName, cFileName, FILEGET_OVERWRITE)

FUNCTION sx_File2Blob(cFileName, cFldName, nActionCode)

   LOCAL nAction := 0

   IF hb_bitAnd(nActionCode, BLOB_FILECOMPRESS) != 0
      nAction := hb_bitOr(nAction, FILEPUT_COMPRESS)
   ENDIF
   IF hb_bitAnd(nActionCode, BLOB_FILEENCRYPT) != 0
      nAction := hb_bitOr(nAction, FILEPUT_ENCRYPT)
   ENDIF

   RETURN dbFilePut(cFldName, cFileName, nAction)

FUNCTION sx_dbCreate(cFileName, aStruct, cRDD)

   LOCAL aField
   LOCAL aDbStruct

   aDbStruct := AClone(aStruct)
   FOR EACH aField IN aDbStruct
      SWITCH aField[DBS_TYPE]
      CASE "V"
         aField[DBS_LEN] += 6
         EXIT
      CASE "D"
         IF aField[DBS_LEN] == 3
            aField[DBS_TYPE] := "V"
         ENDIF
         EXIT
      CASE "I"
         IF aField[DBS_LEN] == 4
            aField[DBS_TYPE] := "V"
         ENDIF
         EXIT
      ENDSWITCH
   NEXT

   RETURN dbCreate(cFileName, aDbStruct, cRDD)

FUNCTION sx_VSigLen(xField)

   LOCAL nResult := 0
   LOCAL nField := 0

   IF Used()
      DO CASE
      CASE hb_IsString(xField)
         nField := FieldPos(xField)
      CASE hb_IsNumeric(xField)
         nField := xField
      ENDCASE
      IF nField >= 1 .AND. nField <= FCount()
         nResult := FieldLen(nField)
         IF FieldType(nField) == "V" .AND. nResult >= 6
            nResult -= 6
         ENDIF
      ENDIF
   ENDIF

   RETURN nResult

FUNCTION sx_VFGet(cExpr, nLen)

   /* Our RDDs does not use any internal flags to cut V-Fields so
      we can simply evaluate given expression */

   IF Used() .AND. PCount() == 2
      RETURN PadR(&cExpr, nLen)  /* NOTE: Macro operator! */
   ENDIF

   RETURN NIL

FUNCTION sx_IsLocked(xRec)

   LOCAL xRecord

   IF Used()
      xRecord := IIf(xRec == NIL, RecNo(), xRec)
      /* Don't be confused by function name.
         Even if it looks strange and results are not very usable due
         to possible race condition then this is what SIX3 exactly does. */
      IF sx_Rlock(xRecord)
         sx_Unlock(xRecord)
      ELSE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION sx_SetTrigger(nAction, cTriggerName, cRDD /* Harbour extensions */)

   LOCAL cPrevTrigger := ""

   IF hb_IsNumeric(nAction)
      IF nAction == TRIGGER_PENDING
         IF hb_IsString(cTriggerName)
            hb_rddInfo(RDDI_PENDINGTRIGGER, cTriggerName, cRDD)
         ENDIF
      ELSEIF Used()
         cPrevTrigger := dbInfo(DBI_TRIGGER)
         SWITCH nAction
         CASE TRIGGER_ENABLE
            dbInfo(DBI_TRIGGER, .T.)
            EXIT
         CASE TRIGGER_DISABLE
            dbInfo(DBI_TRIGGER, .F.)
            EXIT
         CASE TRIGGER_REMOVE
            dbInfo(DBI_TRIGGER, "")
            EXIT
         CASE TRIGGER_INSTALL
            IF hb_IsString(cTriggerName)
               dbInfo(DBI_TRIGGER, cTriggerName)
            ENDIF
            EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN cPrevTrigger
