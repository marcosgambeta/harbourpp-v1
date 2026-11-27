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

Function sxChar(nLen, /* @ */ xKeyVal)

   Switch ValType(xKeyVal)
   Case "C"
   Case "M"
      EXIT
   Case "N"
      xKeyVal := Str(xKeyVal)
      EXIT
   Case "D"
      xKeyVal := DToS(xKeyVal)
      EXIT
   Case "T"
      xKeyVal := hb_TToS(xKeyVal)
      EXIT
   Case "L"
      xKeyVal := IIf(xKeyVal, "T", "F")
      EXIT
   Otherwise
      xKeyVal := IIf(hb_IsNumeric(nLen), "", Space(10))
      EXIT
   EndSwitch

   IF hb_IsNumeric(nLen)
      xKeyVal := PadR(LTrim(xKeyVal), nLen)
   ENDIF

   RETURN xKeyVal

Function sxNum(/* @ */ xKeyVal)

   Switch ValType(xKeyVal)
   Case "N"
      EXIT
   Case "C"
   Case "M"
      xKeyVal := Val(xKeyVal)
      EXIT
   Case "T"
   Case "D"
      xKeyVal := xKeyVal - hb_SToD()
      EXIT
   Case "L"
      xKeyVal := IIf(xKeyVal, 1, 0)
      EXIT
   Otherwise
      xKeyVal := 0.00
      EXIT
   EndSwitch

   RETURN xKeyVal

Function sxDate(/* @ */ xKeyVal)

   Switch ValType(xKeyVal)
   Case "D"
      EXIT
   Case "T"
      xKeyVal := hb_TToD(xKeyVal)
      EXIT
   Case "C"
   Case "M"
      xKeyVal := CToD(xKeyVal)
      EXIT
   Case "N"
      xKeyVal := hb_SToD() + xKeyVal
      EXIT
   Otherwise
      xKeyVal := hb_SToD()
      EXIT
   EndSwitch

   RETURN xKeyVal

Function sxLog(/* @ */ xKeyVal)

   Switch ValType(xKeyVal)
   Case "L"
      EXIT
   Case "C"
   Case "M"
      Switch xKeyVal
      Case  "T";  Case  "t";  Case  "Y";  Case  "y"
      Case ".T."; Case ".t."; Case ".Y."; Case ".y."
         xKeyVal := True
         EXIT
      Otherwise
         xKeyVal := False
         EXIT
      EndSwitch
      EXIT
   Case "N"
      xKeyVal := xKeyVal != 0
      EXIT
   Otherwise
      xKeyVal := False
      EXIT
   EndSwitch

   RETURN xKeyVal

Function sx_Compress(xVal)

   Local xRetVal

   Switch ValType(xVal)
   Case "C"
   Case "M"
      RETURN _sx_StrCompress(xVal)
   Case "A"
      xRetVal := Array(Len(xVal))
      AEval(xVal, {| x, i | xRetVal[i] := sx_Compress(x) })
      RETURN xRetVal
   EndSwitch

   RETURN xVal

Function sx_Decompress(xVal)

   Local xRetVal

   Switch ValType(xVal)
   Case "C"
   Case "M"
      RETURN _sx_StrDecompress(xVal)
   Case "A"
      xRetVal := Array(Len(xVal))
      AEval(xVal, {| x, i | xRetVal[i] := sx_Decompress(x) })
      RETURN xRetVal
   EndSwitch

   RETURN xVal

Function sx_TagInfo(cIndex)

   Local aInfo
   Local nOrds
   Local nFirst
   Local i

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

Function sx_TagCount(xIndex)

   Local nTags := 0
   Local cIndex
   Local nOrder

   IF Used()
      Do Case
      Case hb_IsNumeric(xIndex)
         IF (nOrder := sx_TagOrder(1, xIndex)) != 0
            cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
         ENDIF
      Case hb_IsString(xIndex) .AND. !Empty(xIndex)
         cIndex := xIndex
      Otherwise
         cIndex := dbOrderInfo(DBOI_FULLPATH)
      EndCase
      IF !Empty(cIndex)
         nTags := ordCount(cIndex)
      ENDIF
   ENDIF

   RETURN nTags

Function sx_Tags(xIndex)

   Local aTagNames := {}
   Local nOrder
   Local nTags

   IF Used()
      Do Case
      Case hb_IsNumeric(xIndex)
         nOrder := sx_TagOrder(1, xIndex)
      Case hb_IsString(xIndex) .AND. !Empty(xIndex)
         nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex)
      Otherwise
         nOrder := ordNumber()
      EndCase
      IF nOrder != 0
         nTags := ordCount(dbOrderInfo(DBOI_FULLPATH, NIL, nOrder))
         Do While --nTags >= 0
            AAdd(aTagNames, ordName(nOrder++))
         EndDo
      ENDIF
   ENDIF

   RETURN aTagNames

Function sx_SetTag(xTag, xIndex)

   Local lRet := False
   Local nOrder := 0
   Local nOldOrd
   Local cIndex

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
         lRet := True
      ENDIF
   ENDIF

   RETURN lRet

Function sx_KillTag(xTag, xIndex)

   Local lRet := False
   Local nOrder
   Local cIndex

   IF hb_IsLogical(xTag)
      IF xTag
         Do Case
         Case Empty(xIndex)
            cIndex := sx_IndexName()
         Case hb_IsNumeric(xIndex)
            cIndex := sx_IndexName(1, xIndex)
         Case hb_IsString(xIndex)
            nOrder := dbOrderInfo(DBOI_BAGORDER, xIndex)
            IF nOrder != 0
               cIndex := dbOrderInfo(DBOI_FULLPATH, NIL, nOrder)
            ENDIF
         EndCase
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

Function sx_FileOrder()
   RETURN dbOrderInfo(DBOI_BAGNUMBER)

Function sx_SetFileOrd(nIndex)
   RETURN IIf(hb_IsNumeric(nIndex), ordSetFocus(sx_TagOrder(1, nIndex)), ordSetFocus())

Function rdd_Count()
   RETURN Len(rddList())

Function rdd_Name(nRDD)

   Local aRDD

   IF hb_IsNumeric(nRDD) .AND. nRDD >= 1
      aRDD := rddList()
      IF nRDD <= Len(aRDD)
         RETURN aRDD[nRDD]
      ENDIF
   ENDIF

   RETURN ""

Function rdd_Info(xID)

   Local cRDD

   Do Case
   Case hb_IsNumeric(xID)
      IF !Alias(xID) == ""
         (xID)->(rddName())
      ENDIF
   Case hb_IsString(xID)
      cRDD := Upper(AllTrim(xID))
      IF AScan(rddList(), {| x | Upper(x) == cRDD }) == 0
         cRDD := NIL
      ENDIF
   Case xID == NIL
      cRDD := rddSetDefault()
   EndCase

   IF Empty(cRDD)
      RETURN {}
   ENDIF

   RETURN { ;
      cRDD, ;
      True, ;
      hb_rddInfo(RDDI_TABLEEXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_ORDBAGEXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_ORDEREXT, NIL, cRDD), ;
      hb_rddInfo(RDDI_MEMOEXT, NIL, cRDD) }

Function sx_IsDBT(cRDD)
   RETURN hb_rddInfo(RDDI_MEMOTYPE, NIL, cRDD) == DB_MEMO_DBT

Function sx_MemoExt(cNewExt, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOEXT, cNewExt, cRDD)

Function sx_MemoBlk(nNewBlock, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD)

Function sx_SetMemoBlock(nNewBlock, cRDD)
   RETURN hb_rddInfo(RDDI_MEMOBLOCKSIZE, nNewBlock, cRDD)

Function sx_StrXCheck(lStrict, cRDD)
   RETURN hb_rddInfo(RDDI_STRICTSTRUCT, lStrict, cRDD)

Function sx_LockRetry(nRetry, cRDD)
   RETURN hb_rddInfo(RDDI_LOCKRETRY, nRetry, cRDD)

Function sx_AutoOpen(lAuto, cRDD)
   RETURN hb_rddInfo(RDDI_AUTOOPEN, lAuto, cRDD)

Function sx_AutoShare(lAuto, cRDD)
   RETURN hb_rddInfo(RDDI_AUTOSHARE, lAuto, cRDD)

Function sx_Blob2File(cFileName, cFldName)
   RETURN dbFileGet(cFldName, cFileName, FILEGET_OVERWRITE)

Function sx_File2Blob(cFileName, cFldName, nActionCode)

   Local nAction := 0

   IF hb_bitAnd(nActionCode, BLOB_FILECOMPRESS) != 0
      nAction := hb_bitOr(nAction, FILEPUT_COMPRESS)
   ENDIF
   IF hb_bitAnd(nActionCode, BLOB_FILEENCRYPT) != 0
      nAction := hb_bitOr(nAction, FILEPUT_ENCRYPT)
   ENDIF

   RETURN dbFilePut(cFldName, cFileName, nAction)

Function sx_dbCreate(cFileName, aStruct, cRDD)

   Local aField
   Local aDbStruct

   aDbStruct := AClone(aStruct)
   For Each aField In aDbStruct
      Switch aField[DBS_TYPE]
      Case "V"
         aField[DBS_LEN] += 6
         EXIT
      Case "D"
         IF aField[DBS_LEN] == 3
            aField[DBS_TYPE] := "V"
         ENDIF
         EXIT
      Case "I"
         IF aField[DBS_LEN] == 4
            aField[DBS_TYPE] := "V"
         ENDIF
         EXIT
      EndSwitch
   Next

   RETURN dbCreate(cFileName, aDbStruct, cRDD)

Function sx_VSigLen(xField)

   Local nResult := 0
   Local nField := 0

   IF Used()
      Do Case
      Case hb_IsString(xField)
         nField := FieldPos(xField)
      Case hb_IsNumeric(xField)
         nField := xField
      EndCase
      IF nField >= 1 .AND. nField <= FCount()
         nResult := FieldLen(nField)
         IF FieldType(nField) == "V" .AND. nResult >= 6
            nResult -= 6
         ENDIF
      ENDIF
   ENDIF

   RETURN nResult

Function sx_VFGet(cExpr, nLen)

   /* Our RDDs does not use any internal flags to cut V-Fields so
      we can simply evaluate given expression */

   IF Used() .AND. PCount() == 2
      RETURN PadR(&cExpr, nLen)  /* NOTE: Macro operator! */
   ENDIF

   Return Nil

Function sx_IsLocked(xRec)

   Local xRecord

   IF Used()
      xRecord := IIf(xRec == NIL, RecNo(), xRec)
      /* Don't be confused by function name.
         Even if it looks strange and results are not very usable due
         to possible race condition then this is what SIX3 exactly does. */
      IF sx_Rlock(xRecord)
         sx_Unlock(xRecord)
      ELSE
         Return True
      ENDIF
   ENDIF

   Return False

Function sx_SetTrigger(nAction, cTriggerName, cRDD /* Harbour extensions */)

   Local cPrevTrigger := ""

   IF hb_IsNumeric(nAction)
      IF nAction == TRIGGER_PENDING
         IF hb_IsString(cTriggerName)
            hb_rddInfo(RDDI_PENDINGTRIGGER, cTriggerName, cRDD)
         ENDIF
      ELSEIF Used()
         cPrevTrigger := dbInfo(DBI_TRIGGER)
         Switch nAction
         Case TRIGGER_ENABLE
            dbInfo(DBI_TRIGGER, True)
            EXIT
         Case TRIGGER_DISABLE
            dbInfo(DBI_TRIGGER, False)
            EXIT
         Case TRIGGER_REMOVE
            dbInfo(DBI_TRIGGER, "")
            EXIT
         Case TRIGGER_INSTALL
            IF hb_IsString(cTriggerName)
               dbInfo(DBI_TRIGGER, cTriggerName)
            ENDIF
            EXIT
         EndSwitch
      ENDIF
   ENDIF

   RETURN cPrevTrigger
