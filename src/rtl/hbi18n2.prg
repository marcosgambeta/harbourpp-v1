//
// I18N helper functions to manage .pot files
//
// Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "fileio.ch"

#define _I18N_NONE         0
#define _I18N_CONTEXT      1
#define _I18N_MSGID        2
#define _I18N_MSGSTR       3
#define _I18N_PLURAL       4
#define _I18N_SOURCE       5

#define _I18N_ITEM         {"", {}, {}, .F., NIL}

#define _I18N_EOL          Chr(10)
#define _I18N_DELIM        (Chr(0) + Chr(3) + Chr(0))

STATIC FUNCTION __i18n_fileName(cFileName)

   IF Set(_SET_DEFEXTENSIONS)
      cFileName := hb_FNameExtSetDef(cFileName, ".pot")
   ENDIF

   RETURN cFileName

STATIC FUNCTION __i18n_strEncode(cStr)
   RETURN SubStr(hb_StrToExp(cStr, .T.), 2)

STATIC FUNCTION __i18n_strDecode(cLine, cValue, lCont)

   LOCAL lRet := .F.
   LOCAL cText

   cText := hb_StrCDecode(cLine, @lCont)
   IF cText != NIL
      lRet := .T.
      IF cValue == NIL
         cValue := cText
      ELSE
         cValue += cText
      ENDIF
   ENDIF

   RETURN lRet

#define _BOM_VALUE      0xFEFF
#define _UTF8_BOM       hb_utf8Chr(_BOM_VALUE) // e"\xEF\xBB\xBF"

FUNCTION __i18n_potArrayLoad(cFile, /* @ */ cErrorMsg)

   LOCAL cLine
   LOCAL cValue
   LOCAL nMode
   LOCAL nIndex
   LOCAL nOldIndex
   LOCAL nLine
   LOCAL n
   LOCAL aTrans
   LOCAL aItem
   LOCAL aLines
   LOCAL lCont
   LOCAL hFile

   __i18n_fileName(@cFile)
   IF (hFile := FOpen(cFile)) == F_ERROR
      cErrorMsg := hb_StrFormat("cannot open file: %1$s", cFile)
      RETURN NIL
   ENDIF
   cValue := Space(FSeek(hFile, 0, FS_END))
   FSeek(hFile, 0, FS_SET)
   n := FRead(hFile, @cValue, hb_BLen(cValue))
   FClose(hFile)
   IF n != hb_BLen(cValue)
      cErrorMsg := hb_StrFormat("cannot read from file: %1$s", cFile)
      RETURN NIL
   ENDIF
   // Strip UTF-8 BOM
   IF hb_utf8Asc(cValue) == _BOM_VALUE
      cValue := SubStr(cValue, Len(_UTF8_BOM) + 1)
   ENDIF
   IF !hb_eol() == _I18N_EOL
      cValue := StrTran(cValue, hb_eol(), _I18N_EOL)
   ENDIF
   IF !hb_eol() == Chr(13) + Chr(10)
      cValue := StrTran(cValue, Chr(13) + Chr(10), _I18N_EOL)
   ENDIF
   aLines := hb_ATokens(cValue, _I18N_EOL)

   cErrorMsg := NIL
   lCont := .F.
   nLine := 0
   nIndex := 1
   nMode := _I18N_NONE
   aTrans := {}
   aItem := _I18N_ITEM
   cValue := NIL
   FOR EACH cLine IN aLines
      cLine := AllTrim(cLine)
      ++nLine
      IF lCont
         cValue += hb_eol()
      ELSE
         IF hb_LeftEq(cLine, "#") .AND. nMode == _I18N_NONE
            IF hb_LeftEq(cLine, "#:")   // source code references
               IF Empty(aItem[_I18N_SOURCE])
                  aItem[_I18N_SOURCE] := ""
               ELSE
                  aItem[_I18N_SOURCE] += " "
               ENDIF
               aItem[_I18N_SOURCE] += StrTran(LTrim(SubStr(cLine, 3)), "\", "/")
#if 0
            ELSEIF hb_LeftEq(cLine, "#,")  // flags
               cLine := LTrim(SubStr(cLine, 3))
               IF cLine == "c-format"
               ELSE
                  cErrorMsg := hb_StrFormat("unsupported flag: %1$s", cLine)
                  EXIT
               ENDIF
#endif
            ENDIF
            cLine := ""
         ELSEIF hb_LeftEq(cLine, "msgctxt ") .AND. nMode == _I18N_NONE
            cLine := LTrim(SubStr(cLine, 9))
            nMode := _I18N_CONTEXT
            cValue := NIL
         ELSEIF hb_LeftEq(cLine, "msgid ") .AND. (nMode == _I18N_NONE .OR. nMode == _I18N_CONTEXT)
            nIndex := 1
            cLine := LTrim(SubStr(cLine, 7))
            IF nMode == _I18N_CONTEXT
               IF cValue == NIL
                  cErrorMsg := "undefined msgctxt value"
                  EXIT
               ENDIF
               aItem[_I18N_CONTEXT] := cValue
            ENDIF
            nMode := _I18N_MSGID
            cValue := NIL
         ELSEIF hb_LeftEq(cLine, "msgid_plural") .AND. nMode == _I18N_MSGID
            nOldIndex := nIndex
            nIndex := 2
            n := 13
            IF IsDigit(SubStr(cLine, n, 1))
               nIndex := Val(SubStr(cLine, n)) + 1
               IF nIndex < 1
                  cErrorMsg := "wrong plural form index"
                  EXIT
               ENDIF
               DO WHILE IsDigit(SubStr(cLine, n, 1))
                  ++n
               ENDDO
            ENDIF
            IF nIndex != nOldIndex + 1
               cErrorMsg := "wrong plural form index"
               EXIT
            ENDIF
            IF cValue == NIL
               cErrorMsg := "undefined msgid value"
               EXIT
            ENDIF
            aItem[_I18N_PLURAL] := .T.
            AAdd(aItem[_I18N_MSGID], cValue)
            cLine := LTrim(SubStr(cLine, n))
            cValue := NIL
         ELSEIF hb_LeftEq(cLine, "msgstr ") .AND. nMode == _I18N_MSGID
            nIndex := 0
            cLine := LTrim(SubStr(cLine, 8))
            nMode := _I18N_MSGSTR
            IF cValue == NIL
               cErrorMsg := "undefined msgid value"
               EXIT
            ENDIF
            AAdd(aItem[_I18N_MSGID], cValue)
            cValue := NIL
         ELSEIF hb_LeftEq(cLine, "msgstr[") .AND. (nMode == _I18N_MSGID .OR. nMode == _I18N_MSGSTR)
            nOldIndex := IIf(nMode == _I18N_MSGSTR, nIndex, -1)
            nIndex := 0
            n := 8
            IF IsDigit(SubStr(cLine, n, 1))
               nIndex := Val(SubStr(cLine, n))
               DO WHILE IsDigit(SubStr(cLine, n, 1))
                  ++n
               ENDDO
            ENDIF
            DO WHILE SubStr(cLine, n, 1) == " "
               ++n
            ENDDO
            IF !SubStr(cLine, n, 1) == "]"
               nIndex := -1
            ENDIF
            cLine := LTrim(SubStr(cLine, n + 1))
            IF nIndex < 0 .OR. nIndex != nOldIndex + 1
               cErrorMsg := "wrong translation index"
               EXIT
            ENDIF
            IF cValue == NIL
               cErrorMsg := hb_StrFormat("undefined %1$s value", IIf(nMode == _I18N_MSGID, "msgid", "msgstr"))
               EXIT
            ENDIF
            aItem[_I18N_PLURAL] := .T.
            AAdd(aItem[nMode], cValue)
            nMode := _I18N_MSGSTR
            cValue := NIL
         ELSEIF Empty(cLine) .AND. nMode == _I18N_MSGSTR
            IF cValue == NIL
               cErrorMsg := "undefined msgstr value"
               EXIT
            ENDIF
            AAdd(aItem[_I18N_MSGSTR], cValue)
            AAdd(aTrans, aItem)
            aItem := _I18N_ITEM
            nMode := _I18N_NONE
            cValue := NIL
         ENDIF
      ENDIF

      IF lCont .OR. !Empty(cLine)
         IF (nMode != _I18N_CONTEXT .AND. nMode != _I18N_MSGID .AND. nMode != _I18N_MSGSTR) .OR. !__i18n_strDecode(cLine, @cValue, @lCont)
            cErrorMsg := "unrecognized line"
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF cErrorMsg == NIL
      IF lCont
         cErrorMsg := "unclosed string"
      ELSEIF nMode == _I18N_MSGSTR
         IF cValue == NIL
            cErrorMsg := "undefined msgstr value"
         ELSE
            AAdd(aItem[_I18N_MSGSTR], cValue)
            AAdd(aTrans, aItem)
         ENDIF
      ELSEIF nMode != _I18N_NONE
         cErrorMsg := "unclosed translation definition"
      ENDIF
   ENDIF
   IF cErrorMsg == NIL .AND. Empty(aTrans)
      cErrorMsg := "cannot find any translation rules"
   ENDIF

   IF cErrorMsg != NIL
      cErrorMsg := hb_StrFormat("%1$s:%2$d;%3$s", cFile, nLine, cErrorMsg)
      aTrans := NIL
   ENDIF

   RETURN aTrans

STATIC FUNCTION __i18n_IsBOM_UTF8(cFileName)

   LOCAL fhnd
   LOCAL cBuffer

   IF (fhnd := FOpen(cFileName)) != F_ERROR
      cBuffer := hb_FReadLen(fhnd, hb_BLen(_UTF8_BOM))
      FClose(fhnd)
      IF cBuffer == _UTF8_BOM
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION __i18n_potArraySort(aTrans)
   RETURN ASort(aTrans, NIL, NIL, {|item1, item2|__i18n_ItemToStr(item1) < __i18n_ItemToStr(item2)})

STATIC FUNCTION __i18n_ItemToStr(item)

   LOCAL cSource := item[_I18N_SOURCE]
   LOCAL tmp

   hb_default(@cSource, "")

   // first source occurrence
   IF (tmp := At(" ", cSource)) > 0
      cSource := Left(cSource, tmp - 1)
   ENDIF

   IF (tmp := RAt(":", cSource)) > 0
      cSource := "~" + Left(cSource, tmp - 1) + Str(Val(SubStr(cSource, tmp + 1)), 10, 0)
   ENDIF

   RETURN cSource + item[_I18N_MSGID][1]

FUNCTION __i18n_potArrayClean(aTrans, lKeepSource, lKeepVoidTranslations, bTransformTranslation)

   LOCAL item
   LOCAL cString
   LOCAL lVoid
   LOCAL tmp

   hb_default(@lKeepSource, .T.)
   hb_default(@lKeepVoidTranslations, .T.)

   FOR EACH item IN aTrans
      IF hb_IsEvalItem(bTransformTranslation)
         FOR EACH cString IN item[_I18N_MSGSTR]
            tmp := Eval(bTransformTranslation, cString, item[_I18N_MSGID][cString:__enumIndex()])
            IF hb_IsString(tmp)
               cString := tmp
            ENDIF
         NEXT
      ENDIF
      IF !lKeepSource
         item[_I18N_SOURCE] := ""
      ENDIF
   NEXT

   IF !lKeepVoidTranslations
      FOR EACH item IN aTrans DESCEND
         lVoid := .T.
         FOR EACH cString IN item[_I18N_MSGSTR]
            IF !Empty(cString) .AND. !cString == item[_I18N_MSGID][cString:__enumIndex()]
               lVoid := .F.
               EXIT
            ENDIF
         NEXT
         IF lVoid
            hb_ADel(item:__enumBase(), item:__enumIndex(), .T.)
            LOOP
         ENDIF
      NEXT
   ENDIF

   RETURN aTrans

FUNCTION __i18n_potArraySave(cFile, aTrans, /* @ */ cErrorMsg, lVersionNo, lSourceRef)

   LOCAL aItem
   LOCAL hFile
   LOCAL lRet
   LOCAL lPlural
   LOCAL cEOL
   LOCAL cPOT
   LOCAL cFlg
   LOCAL msg

   hb_default(@lVersionNo, .T.)
   hb_default(@lSourceRef, .T.)

   lRet := .F.
   cEOL := hb_eol()
   cFlg := "#, c-format" + cEOL
   cPOT := IIf(hb_FileExists(cFile) .AND. __i18n_IsBOM_UTF8(cFile), _UTF8_BOM + cEOL, "") + ; // Put it in separate line to less confuse non-BOM aware parsers
      "#" + cEOL + ;
      "# This file is generated by " + IIf(lVersionNo, hb_Version(), "Harbour") + cEOL + ;
      "#" + cEOL
   FOR EACH aItem IN aTrans
      cPOT += cEOL
      IF lSourceRef .AND. !Empty(aItem[_I18N_SOURCE])
         cPOT += "#: "
         cPOT += aItem[_I18N_SOURCE]
         cPOT += cEOL
      ENDIF
      cPOT += cFlg
      IF !aItem[_I18N_CONTEXT] == ""
         cPOT += "msgctxt "
         cPOT += __i18n_strEncode(aItem[_I18N_CONTEXT])
         cPOT += cEOL
      ENDIF
      FOR EACH msg IN aItem[_I18N_MSGID]
         SWITCH msg:__enumIndex()
         CASE 1
            cPOT += "msgid "
            EXIT
         CASE 2
            cPOT += "msgid_plural "
            EXIT
         OTHERWISE
            cPOT += "msgid_plural" + hb_ntos(msg:__enumIndex() - 1) + " "
         ENDSWITCH
         cPOT += __i18n_strEncode(msg)
         cPOT += cEOL
      NEXT
      lPlural := aItem[_I18N_PLURAL] .OR. Len(aItem[_I18N_MSGSTR]) > 1
      FOR EACH msg IN aItem[_I18N_MSGSTR]
         IF lPlural
            cPOT += "msgstr[" + hb_ntos(msg:__enumIndex() - 1) + "] "
         ELSE
            cPOT += "msgstr "
         ENDIF
         cPOT += __i18n_strEncode(msg)
         cPOT += cEOL
      NEXT
   NEXT

   __i18n_fileName(@cFile)
   IF (hFile := FCreate(cFile)) == F_ERROR
      cErrorMsg := hb_StrFormat("cannot create translation file: %1$s", cFile)
   ELSEIF FWrite(hFile, cPOT) != hb_BLen(cPOT)
      cErrorMsg := hb_StrFormat("cannot write to file: %1$s", cFile)
   ELSE
      cErrorMsg := NIL
      lRet := .T.
   ENDIF
   FClose(hFile)

   RETURN lRet

FUNCTION __i18n_potArrayToHash(aTrans, lEmpty, hI18N)

   LOCAL aItem
   LOCAL cContext
   LOCAL hTrans
   LOCAL hContext

   hb_default(@lEmpty, .F.)
   IF !hb_IsHash(hI18N)
      hI18N := {"CONTEXT" => {"" => {=>}}}
   ENDIF
   hTrans := hI18N["CONTEXT"]

   FOR EACH aItem IN aTrans
      IF lEmpty .OR. !Empty(aItem[_I18N_MSGSTR][1])
         cContext := aItem[_I18N_CONTEXT]
         IF cContext $ hTrans
            hContext := hTrans[cContext]
         ELSE
            hTrans[cContext] := hContext := {=>}
         ENDIF
         IF Empty(aItem[_I18N_MSGSTR][1])
            IF !aItem[_I18N_MSGID][1] $ hContext
               hContext[aItem[_I18N_MSGID][1]] := IIf(aItem[_I18N_PLURAL], AClone(aItem[_I18N_MSGID]), aItem[_I18N_MSGID][1])
            ENDIF
         ELSE
            hContext[aItem[_I18N_MSGID][1]] := IIf(aItem[_I18N_PLURAL], AClone(aItem[_I18N_MSGSTR]), aItem[_I18N_MSGSTR][1])
         ENDIF
      ENDIF
   NEXT

   RETURN hI18N

FUNCTION __i18n_potArrayTrans(aTrans, hI18N)

   LOCAL aItem
   LOCAL hContext
   LOCAL cContext
   LOCAL hTrans
   LOCAL xTrans

   hTrans := hI18N["CONTEXT"]

   FOR EACH aItem IN aTrans
      cContext := aItem[_I18N_CONTEXT]
      IF cContext $ hTrans
         hContext := hTrans[cContext]
         IF Empty(aItem[_I18N_MSGSTR][1])
            IF aItem[_I18N_MSGID][1] $ hContext
               xTrans := hContext[aItem[_I18N_MSGID][1]]
               IF aItem[_I18N_PLURAL]
                  aItem[_I18N_MSGSTR] := IIf(hb_IsArray(xTrans), AClone(xTrans), {xTrans})
               ELSE
                  aItem[_I18N_MSGSTR] := IIf(hb_IsArray(xTrans), {xTrans[1]}, {xTrans})
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN aTrans

FUNCTION __i18n_hashJoin(hTrans, hTrans2)

   LOCAL hContext
   LOCAL hCtx
   LOCAL hDstCtx
   LOCAL xTrans

   hContext := hTrans["CONTEXT"]
   FOR EACH hCtx in hTrans2["CONTEXT"]
      IF hCtx:__enumKey() $ hContext
         hDstCtx := hContext[hCtx:__enumKey()]
         FOR EACH xTrans IN hCtx
            IF !Empty(xTrans) .AND. (!xTrans:__enumKey() $ hDstCtx .OR. Empty(hDstCtx[xTrans:__enumKey()]))
               hDstCtx[xTrans:__enumKey()] := IIf(hb_IsArray(xTrans), AClone(xTrans), xTrans)
            ENDIF
         NEXT
      ELSE
         hContext[hCtx:__enumKey()] := hb_HClone(hCtx)
      ENDIF
   NEXT

   RETURN hTrans

FUNCTION __i18n_potArrayJoin(aTrans, aTrans2, hIndex)

   LOCAL aItem
   LOCAL aDest
   LOCAL aSrc
   LOCAL ctx

   IF !hb_IsHash(hIndex)
      hIndex := {=>}
      FOR EACH aItem in aTrans
         ctx := aItem[_I18N_CONTEXT] + _I18N_DELIM + aItem[_I18N_MSGID][1]
         hIndex[ctx] := aItem:__enumIndex()
      NEXT
   ENDIF

   FOR EACH aItem in aTrans2
      ctx := aItem[_I18N_CONTEXT] + _I18N_DELIM + aItem[_I18N_MSGID][1]
      IF ctx $ hIndex
         aDest := aTrans[hIndex[ctx]]
         IF aItem[_I18N_PLURAL]
            aDest[_I18N_PLURAL] := .T.
         ENDIF
         IF !Empty(aItem[_I18N_SOURCE])
            IF Empty(aDest[_I18N_SOURCE])
               aDest[_I18N_SOURCE] := aItem[_I18N_SOURCE]
            ELSE
               aSrc := hb_ATokens(aDest[_I18N_SOURCE])
               FOR EACH ctx IN hb_ATokens(aItem[_I18N_SOURCE])
                  IF hb_AScan(aSrc, ctx, NIL, NIL, .T.) == 0
                     aDest[_I18N_SOURCE] += " "
                     aDest[_I18N_SOURCE] += ctx
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         IF !Empty(aItem[_I18N_MSGSTR]) .AND. (Empty(aDest[_I18N_MSGSTR]) .OR. (Len(aDest[_I18N_MSGSTR]) == 1 .AND. Empty(aDest[_I18N_MSGSTR][1])))
            aDest[_I18N_MSGSTR] := AClone(aItem[_I18N_MSGSTR])
         ENDIF
      ELSE
         AAdd(aTrans, AClone(aItem))
         hIndex[ctx] := Len(aTrans)
      ENDIF
   NEXT

   RETURN aTrans

FUNCTION hb_i18n_LoadPOT(cFile, pI18N, cErrorMsg)

   LOCAL aTrans
   LOCAL hI18N

   IF (aTrans := __i18n_potArrayLoad(cFile, @cErrorMsg)) != NIL
      IF hb_IsPointer(pI18N)
         hI18N := __i18n_hashTable(pI18N)
      ENDIF
      IF hI18N == NIL
         pI18N := __i18n_hashTable(__i18n_potArrayToHash(aTrans))
      ELSE
         __i18n_potArrayToHash(aTrans, __i18n_hashTable(pI18N))
      ENDIF
   ENDIF

   RETURN pI18N

FUNCTION hb_i18n_SavePOT(cFile, pI18N, /* @ */ cErrorMsg)

   LOCAL hI18N
   LOCAL hFile
   LOCAL lRet := .T.
   LOCAL cEOL
   LOCAL cPOT
   LOCAL cFlg
   LOCAL context
   LOCAL trans
   LOCAL msgctxt
   LOCAL msgstr

   IF hb_IsPointer(pI18N)
      hI18N := __i18n_hashTable(pI18N)
   ENDIF
   IF hI18N == NIL
      cErrorMsg := "wrong translation set item"
      lRet := .F.
   ELSE
      cEOL := hb_eol()
      cFlg := "#, c-format" + cEOL
      cPOT := "#" + cEOL + "# This file is generated by " + hb_Version() + cEOL + "#" + cEOL
      FOR EACH context IN hI18N["CONTEXT"]
         msgctxt := IIf(context:__enumKey() == "", NIL, "msgctxt " + __i18n_strEncode(context:__enumKey()) + cEOL)
         FOR EACH trans IN context
            cPOT += cEOL + cFlg
            IF msgctxt != NIL
               cPOT += msgctxt
            ENDIF
            cPOT += "msgid "
            cPOT += __i18n_strEncode(trans:__enumKey())
            cPOT += cEOL
            IF hb_IsArray(trans)
               FOR EACH msgstr IN trans
                  cPOT += "msgstr["
                  cPOT += hb_ntos(msgstr:__enumIndex() - 1)
                  cPOT += "] "
                  cPOT += __i18n_strEncode(msgstr)
                  cPOT += cEOL
               NEXT
            ELSE
               cPOT += "msgstr "
               cPOT += __i18n_strEncode(trans)
               cPOT += cEOL
            ENDIF
         NEXT
      NEXT

      __i18n_fileName(@cFile)
      IF (hFile := FCreate(cFile)) == F_ERROR
         cErrorMsg := hb_StrFormat("cannot create translation file: %1$s", cFile)
         lRet := .F.
      ELSEIF FWrite(hFile, cPOT) != hb_BLen(cPOT)
         cErrorMsg := hb_StrFormat("cannot write to file: %1$s", cFile)
         lRet := .F.
      ELSE
         cErrorMsg := NIL
      ENDIF
      FClose(hFile)

   ENDIF

   RETURN lRet
