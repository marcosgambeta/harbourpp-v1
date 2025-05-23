//
// Get Class
//
// Copyright 2007-2008 Viktor Szakats (vszakats.net/harbour)
// Copyright 1999 Ignacio Ortiz de Zuniga <ignacio@fivetech.com>
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

#include "hbclass.ch"
#include "hblang.ch"

#include "color.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"

/* FIXME: ::Minus [vszakats] */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1
#define GET_CLR_CAPTION         2
#define GET_CLR_ACCEL           3

/* NOTE: In CA-Cl*pper, TGET class does not inherit from any other classes. */

CREATE CLASS Get

   PROTECTED:

   /* === Start of CA-Cl*pper compatible TGet instance area === */
   VAR bBlock                           /* 01. */
   VAR xSubScript                       /* 02. */
   VAR cPicture                         /* 03. */
   VAR bPostBlock                       /* 04. */
   VAR bPreBlock                        /* 05. */
   VAR xCargo                           /* 06. */
   VAR cName                            /* 07. */
   VAR cInternal1     HIDDEN            /* 08. U2Bin(::nRow) + U2Bin(::nCol) + trash. Not implemented in Harbour. */
   VAR xExitState                       /* 09. */
   VAR bReader                          /* 10. */
#ifdef HB_COMPAT_C53
   VAR oControl                         /* 11. CA-Cl*pper 5.3 only. */
   VAR cCaption                 INIT "" /* 12. CA-Cl*pper 5.3 only. */
   VAR nCapCol                  INIT 0  /* 13. CA-Cl*pper 5.3 only. */
   VAR nCapRow                  INIT 0  /* 14. CA-Cl*pper 5.3 only. */
   VAR cMessage                 INIT "" /* 15. CA-Cl*pper 5.3 only. */
   VAR nDispLen                         /* 16. CA-Cl*pper 5.3 places it here. */
#endif
   VAR cType                            /* +1. Only accessible in CA-Cl*pper when ::hasFocus == .T. In CA-Cl*pper the field may contain random chars after the first one, which is the type. */
   VAR cBuffer                          /* +2. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   VAR xVarGet                          /* +3. Only accessible in CA-Cl*pper when ::hasFocus == .T. */
   /* === End of CA-Cl*pper compatible TGet instance area === */

   EXPORTED:

   VAR decPos         INIT 0   READONLY /* CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   VAR hasFocus       INIT .F. READONLY
   VAR original                READONLY
   VAR rejected       INIT .F. READONLY
   VAR typeOut        INIT .F. READONLY

   METHOD Init(nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec) /* NOTE: This method is a Harbour extension [vszakats] */

   METHOD assign()
   METHOD badDate()
   METHOD block(bBlock) SETGET
   ACCESS buffer METHOD getBuffer()
   ASSIGN buffer METHOD setBuffer(cBuffer)
   ACCESS changed METHOD getChanged()
   ASSIGN changed METHOD setChanged(lChanged)
   ACCESS clear METHOD getClear()
   ASSIGN clear METHOD setClear(lClear)
   ACCESS col METHOD getCol()
   ASSIGN col METHOD setCol(nCol)
   METHOD colorDisp(cColorSpec)
   ACCESS colorSpec METHOD getColorSpec()
   ASSIGN colorSpec METHOD setColorSpec(cColorSpec)
   METHOD display()
#ifdef HB_COMPAT_C53
   METHOD hitTest(nMRow, nMCol)
   METHOD control(oControl) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD message(cMessage) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD caption(cCaption) SETGET    /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capRow(nCapRow) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD capCol(nCapCol) SETGET      /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
#endif
   METHOD killFocus()
   ACCESS minus METHOD getMinus()
   ASSIGN minus METHOD setMinus(lMinus)
   METHOD name(cName) SETGET
   METHOD picture(cPicture) SETGET
   ACCESS pos METHOD getPos()
   ASSIGN pos METHOD setPos(nPos)
#ifdef HB_CLP_UNDOC
   METHOD reform()
#endif
   METHOD reset()
   ACCESS row METHOD getRow()
   ASSIGN row METHOD setRow(nRow)
   METHOD setFocus()
   METHOD type()
   METHOD undo()
   METHOD unTransform()
   METHOD updateBuffer()
   METHOD varGet()
   METHOD varPut(xValue)

   METHOD end()
   METHOD home()
   METHOD left()
   METHOD right()
   METHOD toDecPos()
   METHOD wordLeft()
   METHOD wordRight()

   METHOD backSpace()
   METHOD delete()
   METHOD delEnd()
   METHOD delLeft()
   METHOD delRight()
   METHOD delWordLeft()
   METHOD delWordRight()

   METHOD insert(cChar)
   METHOD overStrike(cChar)

   METHOD subScript(xValue) SETGET
   METHOD postBlock(xValue) SETGET
   METHOD preBlock(xValue) SETGET
   METHOD cargo(xValue) SETGET
   METHOD exitState(xValue) SETGET
   METHOD reader(xValue) SETGET

   PROTECTED:

#ifndef HB_COMPAT_C53
   VAR nDispLen                /* NOTE: This one is placed inside the instance area for CA-Cl*pper 5.3 [vszakats] */
#endif
   VAR cColorSpec
   VAR nPos           INIT 0
   VAR lChanged       INIT .F.
   VAR lClear         INIT .F.
   VAR nRow
   VAR nCol
   VAR lRejected      INIT .F.
   VAR lHideInput     INIT .F.
   VAR cStyle         INIT "*" /* NOTE: First char is to be used as mask character when :hideInput is .T. [vszakats] */
   VAR nMaxLen
   VAR lEdit          INIT .F.
   VAR nDispPos       INIT 1
   VAR nOldPos        INIT 0
   VAR nMaxEdit
   VAR lMinus         INIT .F.
   VAR lMinus2        INIT .F.
   VAR lMinusPrinted  INIT .F.
   VAR lSuppDisplay   INIT .F.

   VAR nPicLen
   VAR cPicMask       INIT ""
   VAR cPicFunc       INIT ""
   VAR lPicComplex    INIT .F.
   VAR lPicBlankZero  INIT .F.

   METHOD leftLow()
   METHOD rightLow()
   METHOD backSpaceLow()
   METHOD deleteLow()

   METHOD DeleteAll()
   METHOD IsEditable(nPos)
   METHOD Input(cChar)
   METHOD PutMask(xValue, lEdit)
   METHOD FirstEditable()
   METHOD LastEditable()

ENDCLASS

METHOD Get:assign()

   LOCAL xValue

   IF ::hasFocus
      xValue := ::unTransform()
      IF ::cType == "C"
         xValue += SubStr(::original, Len(xValue) + 1)
      ENDIF
      ::varPut(xValue)
   ENDIF

   RETURN Self

METHOD Get:updateBuffer()

   IF ::hasFocus
      ::cBuffer := ::PutMask(::varGet())
      ::xVarGet := ::original
      ::display()
   ELSE
      ::varGet()
   ENDIF

   RETURN Self

METHOD Get:display()

   LOCAL nOldCursor := SetCursor(SC_NONE)
   LOCAL cBuffer
   LOCAL nDispPos
   LOCAL nRowPos
   LOCAL nColPos
#ifdef HB_COMPAT_C53
   LOCAL nPos
   LOCAL cCaption
#endif

   IF ::hasFocus
      cBuffer   := ::cBuffer
   ELSE
      ::cType   := ValType(::xVarGet := ::varGet())
      ::picture := ::cPicture
      cBuffer   := ::PutMask(::xVarGet)
   ENDIF

   ::nMaxLen := Len(cBuffer)
   ::nDispLen := IIf(::nPicLen == NIL, ::nMaxLen, ::nPicLen)

   IF ::cType == "N" .AND. ::hasFocus .AND. !::lMinusPrinted .AND. ;
      ::decPos != 0 .AND. ::lMinus2 .AND. ;
      ::nPos > ::decPos .AND. Val(Left(cBuffer, ::decPos - 1)) == 0

      /* Display "-." only in case when value on the left side of
         the decimal point is equal 0 */
      cBuffer := Stuff(cBuffer, ::decPos - 1, 2, "-.")
   ENDIF

   IF ::nDispLen != ::nMaxLen .AND. ::nPos != 0 /* has scroll? */
      IF ::nDispLen > 8
         nDispPos := Max(1, Min(::nPos - ::nDispLen + 4       , ::nMaxLen - ::nDispLen + 1))
      ELSE
         nDispPos := Max(1, Min(::nPos - Int(::nDispLen / 2), ::nMaxLen - ::nDispLen + 1))
      ENDIF
   ELSE
      nDispPos := 1
   ENDIF

#ifdef HB_COMPAT_C53

   /* Handle C5.3 caption. */

   IF !Empty(::cCaption)

      cCaption := ::cCaption
      IF (nPos := At("&", cCaption)) > 0
         IF nPos == Len(cCaption)
            nPos := 0
         ELSE
            cCaption := Stuff(cCaption, nPos, 1, "")
         ENDIF
      ENDIF

      hb_DispOutAt(::nCapRow, ::nCapCol, cCaption, hb_ColorIndex(::cColorSpec, GET_CLR_CAPTION))
      IF nPos > 0
         hb_DispOutAt(::nCapRow, ::nCapCol + nPos - 1, SubStr(cCaption, nPos, 1), hb_ColorIndex(::cColorSpec, GET_CLR_ACCEL))
      ENDIF

      /* should we set fixed cursor position here?
       * The above code which can left cursor in the middle of shown screen
       * suggests that we shouldn't. If necessary please fix me.
       */
      /*
      nRowPos := ::nCapRow
      nColPos := ::nCapCol + Len(cCaption)
      */

   ENDIF

#endif

   /* Display the GET */

   IF !::lSuppDisplay .OR. nDispPos != ::nOldPos

      hb_DispOutAt(::nRow, ::nCol, ;
                   IIf(::lHideInput, PadR(Replicate(Left(::cStyle, 1), Len(RTrim(cBuffer))), ::nDispLen), SubStr(cBuffer, nDispPos, ::nDispLen)), ;
                   hb_ColorIndex(::cColorSpec, IIf(::hasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED)))

      nRowPos := ::nRow
      nColPos := ::nCol + Min(::nDispLen, Len(cBuffer))

      IF Set(_SET_DELIMITERS) .AND. !::hasFocus
#ifdef HB_COMPAT_C53
         hb_DispOutAt(nRowPos, ::nCol - 1, SubStr(Set(_SET_DELIMCHARS), 1, 1), hb_ColorIndex(::cColorSpec, GET_CLR_UNSELECTED))
         hb_DispOutAt(nRowPos, nColPos   , SubStr(Set(_SET_DELIMCHARS), 2, 1), hb_ColorIndex(::cColorSpec, GET_CLR_UNSELECTED))
#else
         /* NOTE: C5.2 will use the default color. We're replicating this here. [vszakats] */
         hb_DispOutAt(nRowPos, ::nCol - 1, SubStr(Set(_SET_DELIMCHARS), 1, 1))
         hb_DispOutAt(nRowPos, nColPos   , SubStr(Set(_SET_DELIMCHARS), 2, 1))
#endif
         ++nColPos
      ENDIF
   ENDIF

   IF ::nPos != 0
      SetPos(::nRow, ::nCol + ::nPos - nDispPos)
   ELSEIF nRowPos != NIL
      SetPos(nRowPos, nColPos)
   ENDIF

   ::nOldPos := nDispPos
   ::lSuppDisplay := .F.

   SetCursor(nOldCursor)

   RETURN Self

/* ------------------------------------------------------------------------- */

METHOD Get:colorDisp(cColorSpec)

   ::colorSpec := cColorSpec
   ::display()

   RETURN Self

METHOD Get:end()

   LOCAL nLastCharPos
   LOCAL nPos
   LOCAL nFor

   IF ::hasFocus
      nLastCharPos := Len(RTrim(::cBuffer)) + 1
      /* check for spaces before non-template chars */
      IF nLastCharPos > 2 .AND. !::IsEditable(nLastCharPos - 1)
         FOR nFor := nLastCharPos - 2 TO ::FirstEditable() STEP -1
            IF ::IsEditable(nFor)
               IF Empty(SubStr(::cBuffer, nFor, 1))
                  nLastCharPos := nFor
               ELSE
                  EXIT
               ENDIF
            ENDIF
         NEXT
      ENDIF
      nLastCharPos := Min(nLastCharPos, ::nMaxEdit)
      IF ::nPos < nLastCharPos .OR. ::nPos == ::LastEditable()
         nPos := nLastCharPos
      ELSE
         nPos := ::nMaxEdit
      ENDIF
      FOR nFor := nPos TO ::FirstEditable() STEP -1
         IF ::IsEditable(nFor)
            ::pos := nFor
            EXIT
         ENDIF
      NEXT
      ::lClear := .F.
      ::typeOut := (::nPos == 0)
      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD Get:home()

   IF ::hasFocus
      ::pos := ::FirstEditable()
      ::lClear := .F.
      ::typeOut := (::nPos == 0)
      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD Get:reset()

   IF ::hasFocus
      ::cBuffer  := ::PutMask(::varGet(), .F.)
      ::xVarGet  := ::original
      ::cType    := ValType(::xVarGet)
      ::pos      := ::FirstEditable() /* Simple 0 in CA-Cl*pper [vszakats] */
      ::lClear   := ("K" $ ::cPicFunc .OR. ::cType == "N")
      ::lEdit    := .F.
      ::lMinus   := .F.
      ::rejected := .F.
      ::typeOut  := !(::type $ "CNDTL") .OR. (::nPos == 0) /* Simple .F. in CA-Cl*pper [vszakats] */
      ::display()
   ENDIF

   RETURN Self

METHOD Get:undo()

   IF ::hasFocus
      IF ::original != NIL
         ::varPut(::original)
      ENDIF
      ::reset()
      ::lChanged := .F.
   ENDIF

   RETURN Self

METHOD Get:setFocus()

   LOCAL xVarGet

   IF !::hasFocus

      xVarGet := ::xVarGet := ::varGet()

      ::hasFocus := .T.
      ::rejected := .F.

      ::original := xVarGet
      ::cType    := ValType(xVarGet)
      ::picture  := ::cPicture
      ::cBuffer  := ::PutMask(xVarGet, .F.)

      ::lChanged := .F.
      ::lClear   := ("K" $ ::cPicFunc .OR. ::cType == "N")
      ::lEdit    := .F.
      ::pos      := 1

      ::lMinusPrinted := .F.
      ::lMinus        := .F.

      IF ::cType == "N"
         ::decPos := At(IIf("E" $ ::cPicFunc, ",", "."), ::cBuffer)
         IF ::decPos == 0
            ::decPos := Len(::cBuffer) + 1
         ENDIF
         ::lMinus2 := (::xVarGet < 0)
      ELSE
         ::decPos := 0 /* CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD Get:killFocus()

   LOCAL lHadFocus := ::hasFocus

   ::hasFocus := .F.
   ::nPos     := 0
   ::lClear   := .F.
   ::lMinus   := .F.
   ::lChanged := .F.
   ::decPos   := 0 /* CA-Cl*pper NG says that it contains NIL, but in fact it contains zero. [vszakats] */
   ::typeOut  := .F.

   IF lHadFocus
      ::display()
   ENDIF

   ::xVarGet  := NIL
   ::original := NIL
   ::cBuffer  := NIL

   RETURN Self

METHOD Get:varPut(xValue)

   LOCAL aSubs
   LOCAL nLen
   LOCAL i
   LOCAL aValue

   IF hb_IsEvalItem(::bBlock) .AND. ValType(xValue) $ "CNDTLU"
      aSubs := ::xSubScript
      IF hb_IsArray(aSubs) .AND. !Empty(aSubs)
         nLen := Len(aSubs)
         aValue := Eval(::bBlock)
         FOR i := 1 TO nLen - 1
            IF hb_IsNumeric(aSubs[i]) .OR. (hb_IsHash(aValue) .AND. ValType(aSubs[i]) $ "CDT")
               aValue := aValue[aSubs[i]]
            ELSE
               EXIT
            ENDIF
         NEXT
         IF hb_IsNumeric(aSubs[i]) .OR. (hb_IsHash(aValue) .AND. ValType(aSubs[i]) $ "CDT")
            aValue[aSubs[i]] := xValue
         ENDIF
      ELSE
         Eval(::bBlock, xValue)
      ENDIF
   ELSE
      xValue := NIL
   ENDIF

   RETURN xValue

METHOD Get:varGet()

   LOCAL aSubs
   LOCAL nLen
   LOCAL i
   LOCAL xValue

   IF hb_IsEvalItem(::bBlock)
      aSubs := ::xSubScript
      IF hb_IsArray(aSubs) .AND. !Empty(aSubs)
         nLen := Len(aSubs)
         xValue := Eval(::bBlock)
         FOR i := 1 TO nLen
            IF hb_IsNumeric(aSubs[i]) .OR. (hb_IsHash(xValue) .AND. ValType(aSubs[i]) $ "CDT")
               xValue := xValue[aSubs[i]]
            ELSE
               EXIT
            ENDIF
         NEXT
      ELSE
         xValue := Eval(::bBlock)
      ENDIF
   ELSE
      xValue := ::xVarGet
   ENDIF

   RETURN xValue

/* NOTE: CA-Cl*pper will corrupt memory if cChar contains
         multiple chars. [vszakats] */

METHOD Get:overStrike(cChar)

   IF ::hasFocus .AND. hb_IsString(cChar)

      IF ::cType == "N" .AND. !::lEdit .AND. ::lClear
         ::pos := ::FirstEditable()
      ENDIF

      IF ::pos <= ::nMaxEdit

         cChar := ::Input(Left(cChar, 1))

         IF cChar == ""
            ::rejected := .T.
         ELSE
            ::rejected := .F.

            IF ::lClear .AND. ::nPos == ::FirstEditable()
               ::DeleteAll()
               ::lClear := .F.
            ENDIF

            ::lEdit := .T.

            IF ::nPos == 0
               ::pos := 1
            ENDIF

            DO WHILE !::IsEditable(::nPos) .AND. ::nPos <= ::nMaxEdit .AND. !::typeOut
               ::pos++
            ENDDO

            IF ::nPos > ::nMaxEdit
               ::pos := ::FirstEditable()
            ENDIF
            ::cBuffer := Stuff(::cBuffer, ::nPos, 1, cChar)

            ::lChanged := .T.

            ::rightLow()
         ENDIF
      ENDIF

      ::display()
   ENDIF

   RETURN Self

/* NOTE: CA-Cl*pper will corrupt memory if cChar contains
         multiple chars. [vszakats] */

METHOD Get:insert(cChar)

   LOCAL nFor
   LOCAL nMaxEdit

   IF ::hasFocus .AND. hb_IsString(cChar)

      nMaxEdit := ::nMaxEdit

      IF ::cType == "N" .AND. !::lEdit .AND. ::lClear
         ::pos := ::FirstEditable()
      ENDIF

      IF ::nPos <= ::nMaxEdit

         cChar := ::Input(Left(cChar, 1))

         IF cChar == ""
            ::rejected := .T.
         ELSE
            ::rejected := .F.

            IF ::lClear .AND. ::nPos == ::FirstEditable()
               ::DeleteAll()
               ::lClear := .F.
            ENDIF

            ::lEdit := .T.

            IF ::nPos == 0
               ::pos := 1
            ENDIF

            DO WHILE !::IsEditable(::nPos) .AND. ::nPos <= ::nMaxEdit .AND. !::typeOut
               ::pos++
            ENDDO

            IF ::nPos > ::nMaxEdit
               ::pos := ::FirstEditable()
            ENDIF

            IF ::lPicComplex
               /* Calculating different nMaxEdit for ::lPicComplex */
               FOR nFor := ::nPos TO nMaxEdit
                  IF !::IsEditable(nFor)
                     EXIT
                  ENDIF
               NEXT
               nMaxEdit := nFor
               ::cBuffer := Left(Stuff(Left(::cBuffer, nMaxEdit - 2), ::nPos, 0, cChar) + SubStr(::cBuffer, nMaxEdit), ::nMaxLen)
            ELSE
               ::cBuffer := Left(Stuff(::cBuffer, ::nPos, 0, cChar), ::nMaxEdit)
            ENDIF

            ::lChanged := .T.

            ::rightLow()
         ENDIF
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD Get:right()

   IF ::hasFocus .AND. ::rightLow()

      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD Get:left()

   IF ::hasFocus .AND. ::leftLow()

      ::lSuppDisplay := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD Get:wordLeft()

   LOCAL nPos

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::FirstEditable()
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         nPos := IIf(SubStr(::cBuffer, ::nPos, 1) == " ", ::nPos, ::nPos - 1)

         DO WHILE nPos > 1 .AND. SubStr(::cBuffer, nPos, 1) == " "
            nPos--
         ENDDO
         DO WHILE nPos > 1 .AND. !(SubStr(::cBuffer, nPos, 1) == " ")
            nPos--
         ENDDO

         ::pos := IIf(nPos > 1, nPos + 1, 1)

         ::lSuppDisplay := .T.
         ::display()
      ENDIF
   ENDIF

   RETURN Self

METHOD Get:wordRight()

   LOCAL nPos

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::nMaxEdit
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         nPos := ::nPos

         DO WHILE nPos < ::nMaxEdit .AND. !(SubStr(::cBuffer, nPos, 1) == " ")
            nPos++
         ENDDO
         DO WHILE nPos < ::nMaxEdit .AND. SubStr(::cBuffer, nPos, 1) == " "
            nPos++
         ENDDO

         ::pos := nPos

         ::lSuppDisplay := .T.
         ::display()
      ENDIF
   ENDIF

   RETURN Self

METHOD Get:toDecPos()

   IF ::hasFocus

      IF ::lClear
         ::delEnd()
      ENDIF

      ::cBuffer := ::PutMask(::unTransform(), .F.)
      ::pos := ::decPos
      ::lChanged := .T.

      IF ::type == "N" .AND. ::lMinus .AND. ::unTransform() == 0
         ::backSpace()
         ::overStrike("-")
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD Get:backSpace()

   IF ::hasFocus .AND. ::backSpaceLow()

      ::display()
   ENDIF

   RETURN Self

METHOD Get:delete()

   IF ::hasFocus
      ::deleteLow()
      ::display()
   ENDIF

   RETURN Self

METHOD Get:delEnd()

   LOCAL nPos

   IF ::hasFocus

      nPos := ::nPos
      ::pos := ::nMaxEdit

      ::deleteLow()
      DO WHILE ::nPos > nPos
         ::backSpaceLow()
      ENDDO

      ::display()
   ENDIF

   RETURN Self

METHOD Get:delLeft()

   ::leftLow()
   ::deleteLow()
   ::right()

   RETURN Self

METHOD Get:delRight()

   ::rightLow()
   ::deleteLow()
   ::left()

   RETURN Self

/* ::wordLeft()
   ::delWordRight() */

METHOD Get:delWordLeft()

   IF ::hasFocus

      IF !(SubStr(::cBuffer, ::nPos, 1) == " ")
         IF SubStr(::cBuffer, ::nPos - 1, 1) == " "
            ::backSpaceLow()
         ELSE
            ::wordRight()
            ::left()
         ENDIF
      ENDIF

      IF SubStr(::cBuffer, ::nPos, 1) == " "
         ::deleteLow()
      ENDIF

      DO WHILE ::nPos > 1 .AND. !(SubStr(::cBuffer, ::nPos - 1, 1) == " ")
         ::backSpaceLow()
      ENDDO

      ::display()
   ENDIF

   RETURN Self

METHOD Get:delWordRight()

   IF ::hasFocus

      ::lClear := .F.

      IF ::nPos == ::nMaxEdit
         ::typeOut := .T.
      ELSE
         ::typeOut := .F.

         DO WHILE ::nPos <= ::nMaxEdit .AND. !(SubStr(::cBuffer, ::nPos, 1) == " ")
            ::deleteLow()
         ENDDO

         IF ::nPos <= ::nMaxEdit
            ::deleteLow()
         ENDIF

         ::display()
      ENDIF
   ENDIF

   RETURN Self

/* The METHOD ColorSpec and VAR cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD Get:getColorSpec()
   RETURN ::cColorSpec

METHOD Get:setColorSpec(cColorSpec)

   LOCAL nClrUns
   LOCAL nClrOth

   IF hb_IsString(cColorSpec)

#ifdef HB_COMPAT_C53
      ::cColorSpec := hb_NToColor(nClrUns := Max(hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_UNSELECTED)), 0)) + ;
                      "," + hb_NToColor(IIf((nClrOth := hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_ENHANCED))) != -1, nClrOth, nClrUns)) + ;
                      "," + hb_NToColor(IIf((nClrOth := hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_CAPTION))) != -1, nClrOth, nClrUns)) + ;
                      "," + hb_NToColor(IIf((nClrOth := hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_ACCEL))) != -1, nClrOth, nClrUns))
#else
      ::cColorSpec := hb_NToColor(nClrUns := Max(hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_UNSELECTED)), 0)) + ;
                      "," + hb_NToColor(IIf((nClrOth := hb_ColorToN(hb_ColorIndex(cColorSpec, GET_CLR_ENHANCED))) != -1, nClrOth, nClrUns))
#endif

   /* NOTE: CA-Cl*pper oddity. [vszakats] */
   ELSEIF ValType(cColorSpec) $ "UNDTBA"

      RETURN NIL

#ifdef HB_COMPAT_C53
   /* NOTE: This code doesn't seem to make any sense, but seems to
            replicate some original C5.3 behaviour. */
   ELSE
      IF Set(_SET_INTENSITY)
         ::cColorSpec := ;
            hb_ColorIndex(SetColor(), CLR_UNSELECTED) + "," + ;
            hb_ColorIndex(SetColor(), CLR_ENHANCED) + "," + ;
            hb_ColorIndex(SetColor(), CLR_STANDARD) + "," + ;
            hb_ColorIndex(SetColor(), CLR_BACKGROUND)
      ELSE
         ::cColorSpec := ;
            hb_ColorIndex(SetColor(), CLR_STANDARD) + "," + ;
            hb_ColorIndex(SetColor(), CLR_STANDARD) + "," + ;
            hb_ColorIndex(SetColor(), CLR_STANDARD) + "," + ;
            hb_ColorIndex(SetColor(), CLR_STANDARD)
      ENDIF
#endif
   ENDIF

   RETURN cColorSpec

METHOD Get:getPos()
   RETURN ::nPos

METHOD Get:setPos(nPos)

   LOCAL tmp

   IF hb_IsNumeric(nPos)

      nPos := Int(nPos)

      IF ::hasFocus

         DO CASE
         CASE nPos > ::nMaxLen

            ::nPos := IIf(::nMaxLen == 0, 1, ::nMaxLen)
            ::typeOut := .T.

         CASE nPos > 0

            /* NOTE: CA-Cl*pper has a bug where negative nPos value will be translated to 16bit unsigned int,
                     so the behaviour will be different in this case. [vszakats] */

            FOR tmp := nPos TO ::nMaxLen
               IF ::IsEditable(tmp)
                  ::nPos := tmp
                  RETURN nPos
               ENDIF
            NEXT
            FOR tmp := nPos - 1 TO 1 STEP -1
               IF ::IsEditable(tmp)
                  ::nPos := tmp
                  RETURN nPos
               ENDIF
            NEXT

            ::nPos := ::nMaxLen + 1
            ::typeOut := .T.

         ENDCASE
      ENDIF

      RETURN nPos
   ENDIF

   RETURN 0

/* The METHOD Picture and VAR cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD Get:picture(cPicture)

   LOCAL nAt
   LOCAL nFor
   LOCAL cNum
   LOCAL cChar

   IF PCount() > 0

      IF cPicture != NIL

         ::cPicture      := cPicture
         ::nPicLen       := NIL
         ::cPicFunc      := ""
         ::cPicMask      := ""
         ::lPicBlankZero := .F.

         IF hb_IsString(cPicture)

            cNum := ""

            IF hb_LeftEq(cPicture, "@")

               IF (nAt := At(" ", cPicture)) == 0
                  ::cPicFunc := hb_asciiUpper(cPicture)
                  ::cPicMask := ""
               ELSE
                  ::cPicFunc := hb_asciiUpper(Left(cPicture, nAt - 1))
                  ::cPicMask := SubStr(cPicture, nAt + 1)
               ENDIF

               IF "D" $ ::cPicFunc

                  ::cPicMask := Set(_SET_DATEFORMAT)
                  FOR EACH cChar IN "yYmMdD"
                     ::cPicMask := StrTran(::cPicMask, cChar, "9")
                  NEXT

               ELSEIF "T" $ ::cPicFunc

                  ::cPicMask := Set(_SET_TIMEFORMAT)
                  FOR EACH cChar IN "yYmMdDhHsSfF"
                     ::cPicMask := StrTran(::cPicMask, cChar, "9")
                  NEXT

               ENDIF

               IF (nAt := At("S", ::cPicFunc)) > 0
                  FOR nFor := nAt + 1 TO Len(::cPicFunc)
                     IF IsDigit(SubStr(::cPicFunc, nFor, 1))
                        cNum += SubStr(::cPicFunc, nFor, 1)
                     ELSE
                        EXIT
                     ENDIF
                  NEXT
                  IF Val(cNum) > 0
                     ::nPicLen := Val(cNum)
                  ENDIF
                  ::cPicFunc := Left(::cPicFunc, nAt - 1) + SubStr(::cPicFunc, nFor)
               ENDIF

               IF "Z" $ ::cPicFunc
                  ::lPicBlankZero := .T.
                  ::cPicFunc := StrTran(::cPicFunc, "Z")
               ENDIF

               IF ::cPicFunc == "@"
                  ::cPicFunc := ""
               ELSEIF "R" $ ::cPicFunc .AND. "E" $ ::cPicFunc
                  ::cPicFunc := StrTran(::cPicFunc, "R")
               ENDIF
            ELSE
               ::cPicMask := cPicture
            ENDIF

            IF ::cType == "D" .OR. ::cType == "T"
               ::cPicMask := LTrim(::cPicMask)
            ENDIF
         ENDIF
      ENDIF

      /* Generate default picture mask if not specified. */

      IF ::cType != NIL .AND. (Empty(::cPicMask) .OR. ::cPicture == NIL .OR. ::cType == "D")

         SWITCH ::cType
         CASE "D"

            ::cPicMask := Set(_SET_DATEFORMAT)
            FOR EACH cChar IN "yYmMdD"
               ::cPicMask := StrTran(::cPicMask, cChar, "9")
            NEXT
            EXIT

         CASE "T"

            ::cPicMask := Set(_SET_DATEFORMAT) + " " + Set(_SET_TIMEFORMAT)
            FOR EACH cChar IN "yYmMdDhHsSfF"
               ::cPicMask := StrTran(::cPicMask, cChar, "9")
            NEXT
            EXIT

         CASE "N"

            IF ::xVarGet != NIL
               cNum := Str(::xVarGet)
               IF (nAt := At(".", cNum)) > 0
                  ::cPicMask := Replicate("9", nAt - 1) + "."
                  ::cPicMask += Replicate("9", Len(cNum) - Len(::cPicMask))
               ELSE
                  ::cPicMask := Replicate("9", Len(cNum))
               ENDIF
            ENDIF
            EXIT

         CASE "C"

            IF ::xVarGet != NIL
               IF ::cPicFunc == "@9"
                  ::cPicMask := Replicate("9", Len(::xVarGet))
                  ::cPicFunc := ""
               ENDIF
            ENDIF
            EXIT

         ENDSWITCH

      ENDIF

      /* To verify if it has non-modifiable embedded characters in the group. */

      ::lPicComplex := .F.
      IF !Empty(::cPicMask)
         FOR EACH cChar IN hb_asciiUpper(::cPicMask)
            IF !(cChar $ "!ANX9#")
               ::lPicComplex := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN ::cPicture

METHOD Get:PutMask(xValue, lEdit)

   LOCAL cChar
   LOCAL cBuffer
   LOCAL cPicFunc := ::cPicFunc
   LOCAL cPicMask := ::cPicMask
   LOCAL nFor

   hb_default(@lEdit, ::hasFocus)

   IF !(ValType(xValue) $ "CNDTL")
      xValue := ""
   ENDIF

   IF ::hasFocus
      cPicFunc := StrTran(cPicfunc, "B")
      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF
   ENDIF
   IF lEdit .AND. ::lEdit
      IF "*" $ cPicMask .OR. "$" $ cPicMask
         cPicMask := hb_StrReplace(cPicMask, "*$", "99")
      ENDIF
   ENDIF

   cBuffer := Transform(xValue, ;
                        IIf(Empty(cPicFunc), ;
                            IIf(::lPicBlankZero .AND. !::hasFocus, "@Z ", ""), ;
                            cPicFunc + IIf(::lPicBlankZero .AND. !::hasFocus, "Z", "") + " ") + ;
                        cPicMask)

   IF ::cType == "N"
      IF ("(" $ cPicFunc .OR. ")" $ cPicFunc) .AND. xValue >= 0
         cBuffer += " "
      ENDIF

      IF (("C" $ cPicFunc .AND. xValue < 0) .OR. ("X" $ cPicFunc .AND. xValue >= 0)) .AND. !("X" $ cPicFunc .AND. "C" $ cPicFunc)
         cBuffer += "   "
      ENDIF

      ::lMinusPrinted := (xValue < 0)
   ENDIF

   ::nMaxLen  := Len(cBuffer)
   ::nMaxEdit := ::nMaxLen

   IF lEdit .AND. ::cType == "N" .AND. !Empty(cPicMask)
      FOR nFor := 1 TO ::nMaxLen
         cChar := SubStr(cPicMask, nFor, 1)
         IF cChar $ ",." .AND. SubStr(cBuffer, nFor, 1) $ ",." // " " FIXME
            IF "E" $ cPicFunc
               cChar := IIf(cChar == ",", ".", ",")
            ENDIF
            cBuffer := Stuff(cBuffer, nFor, 1, cChar)
         ENDIF
      NEXT
      IF ::lEdit .AND. Empty(xValue)
         cBuffer := StrTran(cBuffer, "0", " ")
      ENDIF
   ENDIF

   IF ::cType == "N"
      IF "(" $ ::cPicFunc .OR. ")" $ ::cPicFunc
         ::nMaxEdit--
      ENDIF
      IF "C" $ ::cPicFunc .OR. "X" $ ::cPicFunc
         ::nMaxEdit -= 3
      ENDIF
   ENDIF

   IF (::cType == "D" .OR. ::cType == "T") .AND. ::badDate
      cBuffer := ::cBuffer
   ENDIF

   ::nMaxLen := Len(cBuffer)

   RETURN cBuffer

METHOD Get:unTransform()

   LOCAL cBuffer
   LOCAL xValue
   LOCAL nFor
   LOCAL lMinus
   LOCAL lHasDec

   IF ::hasFocus

      cBuffer := ::cBuffer

      IF hb_IsString(cBuffer) .AND. ::cType != NIL

         SWITCH ::cType
         CASE "C"

            IF "R" $ ::cPicFunc
               xValue := ""
               FOR nFor := 1 TO Len(::cPicMask)
                  IF hb_asciiUpper(SubStr(::cPicMask, nFor, 1)) $ "ANX9#!LY"
                     xValue += SubStr(cBuffer, nFor, 1)
                  ENDIF
               NEXT
            ELSE
               xValue := cBuffer
            ENDIF
            EXIT

         CASE "N"

            lMinus := .F.
            IF "X" $ ::cPicFunc .AND. Right(cBuffer, 2) == "DB"
               lMinus := .T.
            ENDIF
            IF !lMinus
               FOR nFor := 1 TO ::nMaxLen
                  IF ::IsEditable(nFor) .AND. IsDigit(SubStr(cBuffer, nFor, 1))
                     EXIT
                  ENDIF
                  IF SubStr(cBuffer, nFor, 1) $ "-(" .AND. !(SubStr(cBuffer, nFor, 1) == SubStr(::cPicMask, nFor, 1))
                     lMinus := .T.
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            cBuffer := Space(::FirstEditable() - 1) + SubStr(cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1)

            /* Readd leading decimal point, if any */
            IF ::decPos <= ::FirstEditable() - 1
               cBuffer := Left(cBuffer, ::decPos - 1) + "." + SubStr(cBuffer, ::decPos + 1)
            ENDIF

            IF "D" $ ::cPicFunc .OR. "T" $ ::cPicFunc
               FOR nFor := ::FirstEditable() TO ::LastEditable()
                  IF !::IsEditable(nFor)
                     cBuffer := Left(cBuffer, nFor - 1) + Chr(1) + SubStr(cBuffer, nFor + 1)
                  ENDIF
               NEXT
            ELSE
               IF "E" $ ::cPicFunc
                  cBuffer := Left(cBuffer, ::FirstEditable() - 1) + ;
                             hb_StrReplace(SubStr(cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1), ".,", " .") + ;
                             SubStr(cBuffer, ::LastEditable() + 1)
               ELSE
                  cBuffer := Left(cBuffer, ::FirstEditable() - 1) + ;
                             StrTran(SubStr(cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1), ",", " ") + ;
                             SubStr(cBuffer, ::LastEditable() + 1)
               ENDIF

               lHasDec := .F.
               FOR nFor := ::FirstEditable() TO ::LastEditable()
                  IF ::IsEditable(nFor)
                     IF lHasDec .AND. SubStr(cBuffer, nFor, 1) == " "
                        cBuffer := Left(cBuffer, nFor - 1) + "0" + SubStr(cBuffer, nFor + 1)
                     ENDIF
                  ELSE
                     IF SubStr(cBuffer, nFor, 1) == "."
                        lHasDec := .T.
                     ELSE
                        cBuffer := Left(cBuffer, nFor - 1) + Chr(1) + SubStr(cBuffer, nFor + 1)
                     ENDIF
                  ENDIF
               NEXT
            ENDIF

            cBuffer := StrTran(cBuffer, Chr(1))

            cBuffer := hb_StrReplace(cBuffer, "$*-()", "     ")

            cBuffer := PadL(StrTran(cBuffer, " "), Len(cBuffer))

            IF lMinus
               FOR nFor := 1 TO Len(cBuffer)
                  IF IsDigit(SubStr(cBuffer, nFor, 1)) .OR. SubStr(cBuffer, nFor, 1) == "."
                     EXIT
                  ENDIF
               NEXT
               nFor--
               IF nFor > 0
                  cBuffer := Left(cBuffer, nFor - 1) + "-" + SubStr(cBuffer, nFor + 1)
               ELSE
                  cBuffer := "-" + cBuffer
               ENDIF
            ENDIF

            xValue := hb_Val(cBuffer)

            EXIT

         CASE "L"

            cBuffer := Upper(cBuffer)
            xValue := "T" $ cBuffer .OR. "Y" $ cBuffer .OR. hb_langMessage(HB_LANG_ITEM_BASE_TEXT + 1) $ cBuffer
            EXIT

         CASE "D"

            IF "E" $ ::cPicFunc
               cBuffer := SubStr(cBuffer, 4, 3) + SubStr(cBuffer, 1, 3) + SubStr(cBuffer, 7)
            ENDIF
            xValue := CToD(cBuffer)
            EXIT

         CASE "T"

            xValue := hb_CToT(cBuffer)
            EXIT

         ENDSWITCH

      ELSE
         ::lClear  := .F.
         ::decPos  := 0
         ::nPos    := 0
         ::typeOut := .F.
      ENDIF
   ENDIF

   RETURN xValue

METHOD Get:type()
   RETURN ::cType := ValType(IIf(::hasFocus, ::xVarGet, ::varGet()))

/* The METHOD Block and VAR bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD Get:block(bBlock)

   IF PCount() == 0 .OR. bBlock == NIL
      RETURN ::bBlock
   ENDIF

   ::bBlock   := bBlock
   ::xVarGet  := ::original
   ::cType    := ValType(::xVarGet)

   RETURN bBlock

METHOD Get:firstEditable()

   LOCAL nFor

   IF ::nMaxLen != NIL

      IF ::IsEditable(1)
         RETURN 1
      ENDIF

      FOR nFor := 2 TO ::nMaxLen
         IF ::IsEditable(nFor)
            RETURN nFor
         ENDIF
      NEXT
   ENDIF

   RETURN 0

METHOD Get:lastEditable()

   LOCAL nFor

   IF ::nMaxLen != NIL

      FOR nFor := ::nMaxLen TO 1 STEP -1
         IF ::IsEditable(nFor)
            RETURN nFor
         ENDIF
      NEXT
   ENDIF

   RETURN 0

METHOD Get:badDate()

   LOCAL xValue

   IF ::hasFocus
      SWITCH ::type
      CASE "D"
         RETURN (xValue := ::unTransform()) == hb_SToD() .AND. !(::cBuffer == Transform(xValue, ::cPicture))
      CASE "T"
         RETURN (xValue := ::unTransform()) == hb_SToT() .AND. !(::cBuffer == Transform(xValue, ::cPicture))
      ENDSWITCH
   ENDIF

   RETURN .F.

#ifdef HB_CLP_UNDOC

METHOD Get:reform()

   IF ::hasFocus
      ::cBuffer := ::PutMask(::unTransform(), .F.)
      ::nDispLen := IIf(::nPicLen == NIL, ::nMaxLen, ::nPicLen) // ?
   ENDIF

   RETURN Self

#endif

#ifdef HB_COMPAT_C53

METHOD Get:hitTest(nMRow, nMCol)

   IF hb_IsObject(::oControl)
      RETURN ::oControl:hitTest(nMRow, nMCol)
   ELSE
      DO CASE
      CASE nMRow == ::nRow .AND. nMCol >= ::nCol .AND. nMCol < ::nCol + IIf(::nDispLen == NIL, 0, ::nDispLen)
         RETURN HTCLIENT
      CASE nMRow == ::nCapRow .AND. nMCol >= ::nCapCol .AND. nMCol < ::nCapCol + Len(::cCaption) /* NOTE: C5.3 doesn't care about the shortcut key. */
         RETURN HTCAPTION
      ENDCASE
   ENDIF

   RETURN HTNOWHERE

METHOD Get:control(oControl)

   IF PCount() == 1 .AND. (oControl == NIL .OR. hb_IsObject(oControl))
      ::oControl := oControl
   ENDIF

   RETURN ::oControl

METHOD Get:caption(cCaption)

   IF hb_IsString(cCaption)
      ::cCaption := cCaption
   ENDIF

   RETURN ::cCaption

METHOD Get:capRow(nCapRow)

   IF hb_IsNumeric(nCapRow)
      ::nCapRow := Int(nCapRow)
   ENDIF

   RETURN ::nCapRow

METHOD Get:capCol(nCapCol)

   IF hb_IsNumeric(nCapCol)
      ::nCapCol := Int(nCapCol)
   ENDIF

   RETURN ::nCapCol

METHOD Get:message(cMessage)

   IF hb_IsString(cMessage)
      ::cMessage := cMessage
   ENDIF

   RETURN ::cMessage

#endif

/* ------------------------------------------------------------------------- */

METHOD Get:rightLow()

   LOCAL nPos

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::nMaxEdit
      ::typeOut := .T.
      RETURN .F.
   ENDIF

   nPos := ::nPos + 1

   DO WHILE !::IsEditable(nPos) .AND. nPos <= ::nMaxEdit
      nPos++
   ENDDO

   IF nPos <= ::nMaxEdit
      ::pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF

   RETURN .T.

METHOD Get:leftLow()

   LOCAL nPos

   ::typeOut := .F.
   ::lClear  := .F.

   IF ::nPos == ::FirstEditable()
      ::typeOut := .T.
      RETURN .F.
   ENDIF

   nPos := ::nPos - 1

   DO WHILE !::IsEditable(nPos) .AND. nPos > 0
      nPos--
   ENDDO

   IF nPos > 0
      ::pos := nPos
   ELSE
      ::typeOut := .T.
   ENDIF

   RETURN .T.

METHOD Get:backSpaceLow()

   LOCAL nMinus
   LOCAL nPos := ::nPos

   IF nPos > 1 .AND. nPos == ::FirstEditable() .AND. ::lMinus2

      /* To delete the parenthesis (negative indicator) in a non editable position */

      IF (nMinus := At("(", Left(::cBuffer, nPos - 1))) > 0 .AND. !(SubStr(::cPicMask, nMinus, 1) == "(")

         ::cBuffer := Stuff(::cBuffer, nMinus, 1, " ")

         ::lEdit := .T.
         ::lChanged := .T.

         RETURN .T.
      ENDIF
   ENDIF

   ::left()

   IF ::nPos < nPos
      ::deleteLow()
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD Get:deleteLow()

   LOCAL nMaxLen := ::nMaxLen
   LOCAL n

   ::lClear := .F.
   ::lEdit := .T.

   IF ::lPicComplex
      /* Calculating different nMaxLen for ::lPicComplex */
      FOR n := ::nPos TO nMaxLen
         IF !::IsEditable(n)
            EXIT
         ENDIF
      NEXT
      nMaxLen := n - 1
   ENDIF

   IF ::cType == "N" .AND. SubStr(::cBuffer, ::nPos, 1) $ "(-"
      ::lMinus2 := .F.
   ENDIF

   ::cBuffer := PadR(Stuff(Stuff(::cBuffer, ::nPos, 1, ""), nMaxLen, 0, " "), ::nMaxLen)

   ::lChanged := .T.

   RETURN NIL

METHOD Get:DeleteAll()

   LOCAL xValue

   IF ::hasFocus

      ::lEdit := .T.

      SWITCH ::cType
      CASE "C"
         xValue := Space(::nMaxlen)
         EXIT
      CASE "N"
         xValue := 0
         ::lMinus2 := .F.
         EXIT
      CASE "D"
         xValue := hb_SToD()
         EXIT
      CASE "T"
         xValue := hb_SToT()
         EXIT
      CASE "L"
         xValue := .F.
         EXIT
      ENDSWITCH

      ::cBuffer := ::PutMask(xValue)
      ::pos     := ::FirstEditable()
   ENDIF

   RETURN Self

METHOD Get:IsEditable(nPos)

   LOCAL cChar

   IF Empty(::cPicMask)
      RETURN .T.
   ENDIF

   /* This odd behaviour helps to be more compatible with CA-Cl*pper in some rare situations.
      xVar := 98 ; o := _GET_(xVar, "xVar") ; o:SetFocus() ; o:picture := "99999" ; o:UnTransform() -> result
      We're still not 100% compatible in slighly different situations because the CA-Cl*pper
      behaviour is pretty much undefined here. [vszakats] */
   IF nPos > Len(::cPicMask) .AND. nPos <= ::nMaxLen
      RETURN .T.
   ENDIF

   IF ::cType != NIL
      cChar := SubStr(::cPicMask, nPos, 1)
      SWITCH ::cType
      CASE "C" ; RETURN hb_asciiUpper(cChar) $ "!ANX9#LY"
      CASE "N" ; RETURN cChar $ "9#$*"
      CASE "D"
      CASE "T" ; RETURN cChar == "9"
      CASE "L" ; RETURN hb_asciiUpper(cChar) $ "LY#" /* CA-Cl*pper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      ENDSWITCH
   ENDIF

   RETURN .F.

METHOD Get:Input(cChar)

   LOCAL cPic

   IF ::cType != NIL

      SWITCH ::cType
      CASE "N"

         DO CASE
         CASE cChar == "-"
            ::lMinus2 := .T.  /* The minus symbol can be written in any place */
            ::lMinus := .T.

         CASE cChar $ ".,"
            ::toDecPos()
            RETURN ""

         CASE !(cChar $ "0123456789+")
            RETURN ""
         ENDCASE
         EXIT

      CASE "D"

         IF !(cChar $ "0123456789")
            RETURN ""
         ENDIF
         EXIT

      CASE "T"

         IF !(cChar $ "0123456789")
            RETURN ""
         ENDIF
         EXIT

      CASE "L"

         IF !(Upper(cChar) $ "YNTF")
            RETURN ""
         ENDIF
         EXIT

      ENDSWITCH
   ENDIF

   IF !Empty(::cPicFunc)
      cChar := Left(Transform(cChar, ::cPicFunc), 1) /* Left needed for @D */
   ENDIF

   IF !Empty(::cPicMask)
      cPic  := hb_asciiUpper(SubStr(::cPicMask, ::nPos, 1))

      //    cChar := Transform(cChar, cPic)
      // Above line eliminated because some get picture template symbols for
      // numeric input not work in text input. eg: $ and *

      DO CASE
      CASE cPic == "A"
         IF !IsAlpha(cChar)
            cChar := ""
         ENDIF

      CASE cPic == "N"
         IF !IsAlpha(cChar) .AND. !IsDigit(cChar)
            cChar := ""
         ENDIF

      CASE cPic == "9"
         IF !IsDigit(cChar) .AND. !cChar $ "-+"
            cChar := ""
         ENDIF
         IF !(::cType == "N") .AND. cChar $ "-+"
            cChar := ""
         ENDIF

      /* Clipper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      CASE cPic == "L" .OR. (cPic == "#" .AND. ::cType == "L")
         IF !(Upper(cChar) $ "YNTF" + hb_langMessage(HB_LANG_ITEM_BASE_TEXT + 1) + hb_langMessage(HB_LANG_ITEM_BASE_TEXT + 2))
            cChar := ""
         ENDIF

      CASE cPic == "#"
         IF !IsDigit(cChar) .AND. !(cChar == " ") .AND. !(cChar $ ".+-")
            cChar := ""
         ENDIF

      CASE cPic == "Y"
         cChar := Upper(cChar)
         IF !(cChar $ "YN")
            cChar := ""
         ENDIF

      CASE (cPic == "$" .OR. cPic == "*") .AND. ::cType == "N"
         IF !IsDigit(cChar) .AND. !(cChar == "-")
            cChar := ""
         ENDIF
      OTHERWISE
         cChar := Transform(cChar, cPic)
      ENDCASE
   ENDIF

   RETURN cChar

/* ------------------------------------------------------------------------- */

METHOD Get:getBuffer()
   RETURN ::cBuffer

METHOD Get:setBuffer(cBuffer)
   RETURN IIf(::hasFocus, ::cBuffer := cBuffer, cBuffer)

/* NOTE: In contrary to CA-Cl*pper docs, this var is assignable. [vszakats] */

METHOD Get:getChanged()
   RETURN ::lChanged

METHOD Get:setChanged(lChanged)

   IF hb_IsLogical(lChanged)
      RETURN IIf(::hasFocus, ::lChanged := lChanged, lChanged)
   ENDIF

   RETURN .F.

METHOD Get:getClear()
   RETURN ::lClear

METHOD Get:setClear(lClear)

   IF hb_IsLogical(lClear)
      RETURN IIf(::hasFocus, ::lClear := lClear, lClear)
   ENDIF

   RETURN .F.

METHOD Get:getMinus()
   RETURN ::lMinus

METHOD Get:setMinus(lMinus)

   IF hb_IsLogical(lMinus)
      RETURN IIf(::hasFocus, ::lMinus := lMinus, lMinus)
   ENDIF

   RETURN .F.

/* NOTE: CA-Cl*pper has a bug where negative nRow value will be translated to 16bit unsigned int,
         so the behaviour will be different in this case. [vszakats] */

METHOD Get:getRow()
   RETURN ::nRow

METHOD Get:setRow(nRow)
   RETURN ::nRow := IIf(hb_IsNumeric(nRow), Int(nRow), 0)

/* NOTE: CA-Cl*pper has a bug where negative nCol value will be translated to 16bit unsigned int,
         so the behaviour will be different in this case. [vszakats] */

METHOD Get:getCol()
   RETURN ::nCol

METHOD Get:setCol(nCol)
   RETURN ::nCol := IIf(hb_IsNumeric(nCol), Int(nCol), 0)

METHOD Get:name(cName)

   IF PCount() > 0 .AND. cName != NIL
      ::cName := cName
   ENDIF

   RETURN ::cName

METHOD Get:SubScript(xValue)

   IF xValue != NIL
      ::xSubScript := xValue
   ENDIF

   RETURN ::xSubScript

METHOD Get:PostBlock(xValue)

   IF xValue != NIL
      ::bPostBlock := xValue
   ENDIF

   RETURN ::bPostBlock

METHOD Get:PreBlock(xValue)

   IF xValue != NIL
      ::bPreBlock := xValue
   ENDIF

   RETURN ::bPreBlock

METHOD Get:Cargo(xValue)

   IF xValue != NIL
      ::xCargo := xValue
   ENDIF

   RETURN ::xCargo

METHOD Get:ExitState(xValue)

   IF xValue != NIL
      ::xExitState := xValue
   ENDIF

   RETURN ::xExitState

METHOD Get:Reader(xValue)

   IF xValue != NIL
      ::bReader := xValue
   ENDIF

   RETURN ::bReader

/* ------------------------------------------------------------------------- */

METHOD Get:Init(nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec)

   IF nRow == NIL
      nRow := Row()
   ENDIF
   IF nCol == NIL
      nCol := Col() + IIf(Set(_SET_DELIMITERS), 1, 0)
   ENDIF
   __defaultNIL(@cVarName, "")
   IF bVarBlock == NIL
      bVarBlock := IIf(hb_IsString(cVarName), MemVarBlock(cVarName), NIL)
   ENDIF
   IF cColorSpec == NIL
      cColorSpec := SetColor()
#ifdef HB_COMPAT_C53
      cColorSpec := ;
         hb_ColorIndex(cColorSpec, IIf(Set(_SET_INTENSITY), CLR_UNSELECTED, CLR_STANDARD)) + "," + ;
         hb_ColorIndex(cColorSpec, IIf(Set(_SET_INTENSITY), CLR_ENHANCED, CLR_STANDARD)) + "," + ;
         hb_ColorIndex(cColorSpec, CLR_STANDARD) + "," + ;
         IIf(IsDefColor(), IIf(Set(_SET_INTENSITY), "W+/N", "W/N"), hb_ColorIndex(cColorSpec, IIf(Set(_SET_INTENSITY), CLR_BACKGROUND, CLR_STANDARD)))
#else
      cColorSpec := ;
         hb_ColorIndex(cColorSpec, IIf(Set(_SET_INTENSITY), CLR_UNSELECTED, CLR_STANDARD)) + "," + ;
         hb_ColorIndex(cColorSpec, IIf(Set(_SET_INTENSITY), CLR_ENHANCED, CLR_STANDARD))
#endif
   ENDIF

   ::nRow      := nRow
   ::nCol      := nCol
   ::bBlock    := bVarBlock
   ::cName     := cVarName
   ::picture   := cPicture
   ::colorSpec := cColorSpec

   RETURN Self

FUNCTION GetNew(nRow, nCol, bVarBlock, cVarName, cPicture, cColor)
   RETURN Get():New(nRow, nCol, bVarBlock, cVarName, cPicture, cColor)
