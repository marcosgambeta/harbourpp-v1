//
// Handling .ini files
//
// Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
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

// This small procedure reads a .ini file in the standard .ini format into
// an hash array:
//    ; A line starting with a ';' is a comment
//    # Also, a '#' marks a comment up to the end of the line
//    [NewSection]
//    Variable=Value
//    OtherVariable: Value
//
// You can pass a list of "potential" .ini files in a ';' separated path;
// the first readable file will be loaded.
//
// On error, the function returns NIL. On success, you will have an hash
// array of this form:
//
//    {"MAIN" => {"Key1" => "Val1", ... , "KeyN" => "ValN"},
//     "Section1" => {"Key1" => "Val1", ... , "KeyN" => "ValN"},
//     ...
//     "SectionN" => {"Key1" => "Val1", ... , "KeyN" => "ValN"}
//    }
//
// 'MAIN' is the default section (variables that are declared without a section).

#include "fileio.ch"

STATIC s_cLineComment := ";"
STATIC s_cHalfLineComment := "#"

Procedure hb_iniSetComment(cLc, cHlc)

   IF hb_IsString(cLc)
      s_cLineComment := cLc
   ENDIF

   IF hb_IsString(cHlc)
      s_cHalfLineComment := cHlc
   ENDIF

   Return

Function hb_iniNew(lAutoMain)

   Local hIni := {=>}

   IF hb_defaultValue(lAutoMain, True)
      hIni["MAIN"] := {=>}
   ENDIF

   Return hIni

Function hb_iniRead(cFileSpec, lKeyCaseSens, cSplitters, lAutoMain)
   Return hb_iniReadStr(IIf(hb_IsString(cFileSpec), hb_iniFileLow(cFileSpec), ""), lKeyCaseSens, cSplitters, lAutoMain)

Function hb_iniReadStr(cData, lKeyCaseSens, cSplitters, lAutoMain)

   Local hIni := {=>}

   // Default case sensitiveness for keys
   hb_default(@lKeyCaseSens, True)
   hb_default(@lAutoMain, True)

   hb_HCaseMatch(hIni, lKeyCaseSens)

   IF lAutoMain
      hIni["MAIN"] := {=>}
   ENDIF

   Return hb_iniStringLow(hIni, hb_defaultValue(cData, ""), lKeyCaseSens, hb_defaultValue(cSplitters, "="), lAutoMain)

Static Function hb_iniFileLow(cFileSpec)

   Local cFile
   Local nLen
   Local hFile
   Local cData
   Local aFiles := hb_ATokens(cFileSpec, hb_osPathListSeparator())

   IF Empty(aFiles)
      aFiles := {cFileSpec}
   ENDIF

   hFile := F_ERROR
   For Each cFile In aFiles
      IF !Empty(cFile) .AND. hb_FileExists(cFile)
         IF (hFile := FOpen(cFile)) != F_ERROR
            Exit
         ENDIF
      ENDIF
   Next

   IF hFile == F_ERROR
      Return ""
   ENDIF

   // we'll read the whole file, then we'll break it in lines.
   cData := Space(FSeek(hFile, 0, FS_END))
   FSeek(hFile, 0, FS_SET)
   nLen := FRead(hFile, @cData, hb_BLen(cData))
   cData := hb_BLeft(cData, nLen)
   FClose(hFile)

   Return cData

Static Function hb_iniStringLow(hIni, cData, lKeyCaseSens, cSplitters, lAutoMain)

   Local aKeyVal
   Local hCurrentSection
   Local cLine
   Local reComment
   Local reInclude
   Local reSection
   Local reSplitters

   reComment := hb_regexComp(s_cHalfLineComment + "|^[ \t]*" + s_cLineComment)
   reInclude := hb_regexComp("include (.*)")
   reSection := hb_regexComp("[[](.*)[]]")
   reSplitters := hb_regexComp(cSplitters)

   // Always begin with the 'MAIN' section
   hCurrentSection := IIf(lAutoMain, hIni["MAIN"], hIni)

   cLine := ""
   For Each cData In hb_ATokens(cData, True)
      cLine += AllTrim(cData)

      // Sum up lines terminating with "<space>||" ...
      IF Right(cLine, 3) == " ||"
         cLine := hb_StrShrink(cLine, 2)
         // ... but proceed if stream over
         IF !cData:__enumIsLast()
            Loop
         ENDIF
      ENDIF

      // Skip void lines
      IF Empty(cLine)
         Loop
      ENDIF

      // remove eventual comments
      IF !Empty(aKeyVal := hb_regexSplit(reComment, cLine))
         IF Empty(cLine := AllTrim(aKeyVal[1]))
            // Skip all comment lines
            Loop
         ENDIF
      ENDIF

      // Is it an "INCLUDE" statement ?
      IF !Empty(aKeyVal := hb_regex(reInclude, cLine))
         // ignore void includes
         aKeyVal[2] := AllTrim(aKeyVal[2])
         IF Len(aKeyVal[2]) == 0
            Loop
         ENDIF
         hb_iniStringLow(hIni, hb_iniFileLow(aKeyVal[2]), lKeyCaseSens, cSplitters, lAutoMain)
      // Is it a NEW section?
      ELSEIF !Empty(aKeyVal := hb_regex(reSection, cLine))
         cLine := AllTrim(aKeyVal[2])
         IF Len(cLine) != 0
            hCurrentSection := {=>}
            IF !lKeyCaseSens
               cLine := Upper(cLine)
            ENDIF
            hIni[cLine] := hCurrentSection
         ENDIF
      // Is it a valid key
      ELSEIF Len(aKeyVal := hb_regexSplit(reSplitters, cLine, NIL, NIL, 1)) == 1
         // TODO: Signal error
      ELSE
         // If not case sensitive, use upper keys
         IF !lKeyCaseSens
            aKeyVal[1] := Upper(aKeyVal[1])
         ENDIF
         hCurrentSection[AllTrim(aKeyVal[1])] := AllTrim(aKeyVal[2])
      ENDIF

      cLine := ""
   Next

   Return hIni

Function hb_iniWrite(xFileName, hIni, cCommentBegin, cCommentEnd, lAutoMain)

   Local hFile
   Local lClose
   Local cBuffer

   cBuffer := hb_iniWriteStr(hIni, cCommentBegin, cCommentEnd, lAutoMain)

   IF !hb_IsString(cBuffer)
      Return False
   ENDIF

   IF hb_IsString(xFileName)
      hFile := FCreate(xFileName)
      lClose := True
   ELSEIF hb_IsNumeric(xFileName)
      hFile := xFileName
      lClose := False
   ELSE
      Return False
   ENDIF

   IF hFile == F_ERROR
      Return False
   ENDIF

   IF FWrite(hFile, cBuffer) != hb_BLen(cBuffer)
      IF lClose
         FClose(hFile)
      ENDIF
      Return False
   ENDIF

   IF lClose
      FClose(hFile)
   ENDIF

   Return True

Function hb_iniWriteStr(hIni, cCommentBegin, cCommentEnd, lAutoMain)

   Local cNewLine := hb_eol()
   Local cSection
   Local cBuffer := ""

   IF !hb_IsHash(hIni)
      Return Nil
   ENDIF

   IF hb_IsString(cCommentBegin) .AND. !Empty(cCommentBegin)
      cBuffer += cCommentBegin + cNewLine
   ENDIF

   hb_default(@lAutoMain, True)

   // Fix if lAutoMain is True but I haven't a MAIN section
   IF lAutoMain .AND. !("MAIN" $ hIni)
      lAutoMain := False
   ENDIF

   // Write top-level section
   IF lAutoMain
      // When lAutoMain is on, write the 'main' section
      hb_HEval(hIni["MAIN"], {|cKey, xVal|cBuffer += hb_CStr(cKey) + "=" + hb_CStr(xVal) + cNewLine})
   ELSE
      // When lAutoMain is off, just write all the top-level variables.
      hb_HEval(hIni, {|cKey, xVal|IIf(hb_IsHash(xVal), /* nothing */, cBuffer += hb_CStr(cKey) + "=" + hb_CStr(xVal) + cNewLine)})
   ENDIF

   For Each cSection In hIni

      // Avoid re-processing 'MAIN' section
      IF lAutoMain
         // When lAutoMain is on, skip section named 'MAIN'
         IF cSection:__enumKey == "MAIN"
            Loop
         ENDIF
      ELSE
         // When lAutoMain is off, skip all the top-level variables.
         IF !hb_IsHash(cSection)
            Loop
         ENDIF
      ENDIF

      cBuffer += cNewLine + "[" + hb_CStr(cSection:__enumKey) + "]" + cNewLine

      hb_HEval(cSection, {|cKey, xVal|cBuffer += hb_CStr(cKey) + "=" + hb_CStr(xVal) + cNewLine})
   Next

   IF hb_IsString(cCommentEnd) .AND. !Empty(cCommentEnd)
      cBuffer += cCommentEnd + cNewLine
   ENDIF

   Return IIf(Empty(cBuffer), NIL, cBuffer)
