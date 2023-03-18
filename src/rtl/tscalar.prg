/*
 * Harbour implementation of Class(y) Scalar classes
 *
 * Copyright 2004 Antonio Linares <alinares@fivetechsoft.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Class(y) documentation is located at:
   https://harbour.github.io/ng/classy/menu.html */

#include "hbclass.ch"

/* --- */

CREATE CLASS ScalarObject FUNCTION HBScalar

   METHOD Copy()
   METHOD IsScalar()
   METHOD AsString()
   METHOD AsExpStr()

   // TODO: replace Harbour code with C++ code
   METHOD isNumeric
   METHOD isLogical
   METHOD isDate
   METHOD isDateTime
   METHOD isTimeStamp
   METHOD isBlock
   METHOD isPointer
   METHOD isSymbol
   METHOD isString
   METHOD isChar
   METHOD isMemo
   METHOD isArray
   METHOD isObject
   METHOD isHash
   METHOD isHashKey
   METHOD isEvalItem
   METHOD isNull

   MESSAGE Become    METHOD BecomeErr()  /* a scalar cannot "become" another object */
   MESSAGE DeepCopy  METHOD Copy()

ENDCLASS

METHOD Copy() CLASS ScalarObject
   RETURN Self

METHOD IsScalar() CLASS ScalarObject
   RETURN .T.

METHOD AsString() CLASS ScalarObject

   SWITCH ValType(Self)
   CASE "B" ; RETURN "{ || ... }"
   CASE "M"
   CASE "C" ; RETURN Self
   CASE "D" ; RETURN DToC(Self)
   CASE "T" ; RETURN hb_TToC(Self)
   CASE "H" ; RETURN "{ ... => ... }"
   CASE "L" ; RETURN iif(Self, ".T.", ".F.")
   CASE "N" ; RETURN hb_ntos(Self)
   CASE "S" ; RETURN "@" + ::name + "()"
   CASE "P" ; RETURN "<0x...>"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "Error!"

METHOD AsExpStr() CLASS ScalarObject

   SWITCH ValType(Self)
   CASE "M"
   CASE "C" ; RETURN '"' + Self + '"'
   CASE "D" ; RETURN 'CToD("' + DToC(Self) + '")'
   CASE "T" ; RETURN 'hb_CToT("' + hb_TToC(Self) + '")'
   ENDSWITCH

   RETURN ::AsString()

METHOD isNumeric() CLASS ScalarObject
   RETURN HB_ISNUMERIC(Self)

METHOD isLogical() CLASS ScalarObject
   RETURN HB_ISLOGICAL(Self)

METHOD isDate() CLASS ScalarObject
   RETURN HB_ISDATE(Self)

METHOD isDateTime() CLASS ScalarObject
   RETURN HB_ISDATETIME(Self)

METHOD isTimeStamp() CLASS ScalarObject
   RETURN HB_ISTIMESTAMP(Self)

METHOD isBlock() CLASS ScalarObject
   RETURN HB_ISBLOCK(Self)

METHOD isPointer() CLASS ScalarObject
   RETURN HB_ISPOINTER(Self)

METHOD isSymbol() CLASS ScalarObject
   RETURN HB_ISSYMBOL(Self)

METHOD isString() CLASS ScalarObject
   RETURN HB_ISSTRING(Self)

METHOD isChar() CLASS ScalarObject
   RETURN HB_ISCHAR(Self)

METHOD isMemo() CLASS ScalarObject
   RETURN HB_ISMEMO(Self)

METHOD isArray() CLASS ScalarObject
   RETURN HB_ISARRAY(Self)

METHOD isObject() CLASS ScalarObject
   RETURN HB_ISOBJECT(Self)

METHOD isHash() CLASS ScalarObject
   RETURN HB_ISHASH(Self)

METHOD isHashKey() CLASS ScalarObject
   RETURN HB_ISHASHKEY(Self)

METHOD isEvalItem() CLASS ScalarObject
   RETURN HB_ISEVALITEM(Self)

METHOD isNull() CLASS ScalarObject
   RETURN HB_ISNULL(Self)

METHOD PROCEDURE BecomeErr() CLASS ScalarObject

#if 0
   // Not implemented yet
   ::error(CSYERR_BECOME, "Message 'become' illegally sent to scalar", ::ClassName())
#endif

   RETURN

/* --- */

CREATE CLASS Array INHERIT HBScalar FUNCTION __HBArray

   METHOD Init(nElements)

   METHOD AsString()
   METHOD At(n)
   METHOD AtPut(n, x)
   METHOD Add(x)
   METHOD AddAll(aOtherCollection)
   METHOD Collect(b)
   METHOD Copy()
   METHOD Do(b)
   METHOD DeleteAt(n)
   METHOD InsertAt(n, x)
   METHOD IndexOf(x)
   METHOD IsScalar()
   METHOD Remove(e)
   METHOD Scan(b)
   METHOD _Size(newSize)  // assignment method

   MESSAGE Append  METHOD Add

ENDCLASS

METHOD Init(nElements) CLASS Array

   ::size := iif(nElements == NIL, 0, nElements)

   RETURN Self

METHOD AddAll(aOtherCollection) CLASS Array

   aOtherCollection:Do({|e|::Add(e)})

   RETURN Self

METHOD AsString() CLASS Array
   RETURN "{ ... }"

METHOD At(n) CLASS Array
   RETURN Self[n]

METHOD AtPut(n, x) CLASS Array
   RETURN Self[n] := x

METHOD Add(x) CLASS Array

   AAdd(Self, x)

   RETURN .T.

METHOD Collect(b) CLASS Array

   LOCAL elem
   LOCAL result := {}

   FOR EACH elem IN Self
      IF Eval(b, elem)
         AAdd(result, elem)
      ENDIF
   NEXT

   RETURN result

METHOD Copy() CLASS Array
   RETURN ACopy(Self, Array(Len(Self)))

METHOD DeleteAt(n) CLASS Array

   IF n >= 1 .AND. n <= Len(Self)
      hb_ADel(Self, n, .T.)
   ENDIF

   RETURN Self

METHOD InsertAt(n, x) CLASS Array

   DO CASE
   CASE n > Len(Self)
      ASize(Self, n)
      Self[n] := x
   CASE n >= 1
      hb_AIns(Self, n, x, .T.)
   ENDCASE

   RETURN Self

METHOD IsScalar() CLASS Array
   RETURN .T.

METHOD Do(b) CLASS Array

   LOCAL i

   FOR i := 1 TO Len(Self)
      b:Eval(Self[i], i)
   NEXT

   RETURN Self

METHOD IndexOf(x) CLASS Array

   LOCAL elem

   FOR EACH elem IN Self
      IF elem == x
         RETURN elem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD PROCEDURE Remove(e) CLASS Array

   ::DeleteAt(::IndexOf(e))

   RETURN

METHOD Scan(b) CLASS Array
   RETURN AScan(Self, b)

METHOD _Size(newSize) CLASS Array

   ASize(Self, newSize)

   RETURN newSize  // so that assignment works according to standard rules

/* --- */

CREATE CLASS Block INHERIT HBScalar FUNCTION __HBBlock

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Block
   RETURN "{ || ... }"

/* --- */

CREATE CLASS Character INHERIT HBScalar FUNCTION __HBCharacter

   METHOD AsString()
   METHOD AsExpStr()

   METHOD at
   METHOD asc
   METHOD empty
   METHOD isAlpha
   METHOD isDigit
   METHOD isLower
   METHOD isUpper
   METHOD left
   METHOD len
   METHOD lower
   METHOD ltrim
   // METHOD memoedit
   // METHOD memoline
   // METHOD memotran
   // METHOD memowrit
   // METHOD mlcount
   // METHOD mlctopos
   // METHOD mlpos
   // METHOD mpostolc
   METHOD padl
   METHOD padc
   METHOD padr
   METHOD rat
   METHOD replicate
   METHOD right
   METHOD rtrim
   METHOD soundex
   METHOD strtran
   METHOD stuff
   METHOD substr
   METHOD transform
   METHOD trim
   METHOD upper
   METHOD val

ENDCLASS

METHOD AsString() CLASS Character
   RETURN Self

METHOD AsExpStr() CLASS Character
   RETURN '"' + Self + '"'

METHOD at(c) CLASS Character
   RETURN at(c, Self)

METHOD asc() CLASS Character
   RETURN asc(Self)

METHOD empty() CLASS Character
   RETURN empty(Self)

METHOD isAlpha() CLASS Character
   RETURN isAlpha(Self)

METHOD isDigit() CLASS Character
   RETURN isDigit(Self)

METHOD isLower() CLASS Character
   RETURN isLower(Self)

METHOD isUpper() CLASS Character
   RETURN isUpper(Self)

METHOD left(n) CLASS Character
   RETURN left(Self, n)

METHOD len() CLASS Character
   RETURN len(Self)

METHOD lower() CLASS Character
   RETURN lower(Self)

METHOD ltrim() CLASS Character
   RETURN ltrim(Self)

METHOD padl(...) CLASS Character
   RETURN padl(Self, ...)

METHOD padc(...) CLASS Character
   RETURN padc(Self, ...)

METHOD padr(...) CLASS Character
   RETURN padr(Self, ...)

METHOD rat(c) CLASS Character
   RETURN rat(c, Self)

METHOD replicate(n) CLASS Character
   RETURN replicate(Self, n)

METHOD right(n) CLASS Character
   RETURN right(Self, n)

METHOD rtrim() CLASS Character
   RETURN rtrim(Self)

METHOD soundex() CLASS Character
   RETURN soundex(Self)

METHOD strtran(...) CLASS Character
   RETURN strtran(Self, ...)

METHOD stuff(...) CLASS Character
   RETURN stuff(Self, ...)

METHOD substr(...) CLASS Character
   RETURN substr(Self, ...)

METHOD transform(c) CLASS Character
   RETURN transform(Self, c)

METHOD trim() CLASS Character
   RETURN trim(Self)

METHOD upper() CLASS Character
   RETURN upper(Self)

METHOD val() CLASS Character
   RETURN val(Self)

/* --- */

CREATE CLASS Date INHERIT HBScalar FUNCTION __HBDate

   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS Date
   RETURN DToC(Self)

METHOD AsExpStr() CLASS Date
   RETURN 'CToD("' + ::AsString() + '")'

METHOD Year() CLASS Date
   RETURN Year(Self)

METHOD Month() CLASS Date
   RETURN Month(Self)

METHOD Day() CLASS Date
   RETURN Day(Self)

/* --- */

CREATE CLASS TimeStamp INHERIT HBScalar FUNCTION __HBTimeStamp

   METHOD Date()
   METHOD Time()
   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD Hour()
   METHOD Minute()
   METHOD Sec()

   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS TimeStamp
   RETURN hb_TToS(Self)

METHOD AsExpStr() CLASS TimeStamp
   RETURN 'hb_SToT("' + ::AsString() + '")'

METHOD Date() CLASS TimeStamp
   RETURN hb_TToC(Self, NIL, "")

METHOD Time() CLASS TimeStamp
   RETURN hb_TToC(Self, "", "hh:mm:ss")

METHOD Year() CLASS TimeStamp
   RETURN Year(Self)

METHOD Month() CLASS TimeStamp
   RETURN Month(Self)

METHOD Day() CLASS TimeStamp
   RETURN Day(Self)

METHOD Hour() CLASS TimeStamp
   RETURN hb_Hour(Self)

METHOD Minute() CLASS TimeStamp
   RETURN hb_Minute(Self)

METHOD Sec() CLASS TimeStamp
   RETURN hb_Sec(Self)

/* --- */

CREATE CLASS Hash INHERIT HBScalar FUNCTION __HBHash

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Hash
   RETURN "{ ... => ... }"

/* --- */

CREATE CLASS Logical INHERIT HBScalar FUNCTION __HBLogical

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Logical
   RETURN iif(Self, ".T.", ".F.")

/* --- */

CREATE CLASS NIL INHERIT HBScalar FUNCTION __HBNil

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS NIL
   RETURN "NIL"

/* --- */

CREATE CLASS Numeric INHERIT HBScalar FUNCTION __HBNumeric

   METHOD AsString()

   METHOD abs
   METHOD chr
   METHOD empty
   // METHOD exp
   METHOD int
   // METHOD max
   // METHOD min
   METHOD round
   METHOD sqrt
   METHOD str
   METHOD transform

ENDCLASS

METHOD AsString() CLASS Numeric
   RETURN hb_ntos(Self)

METHOD abs() CLASS Numeric
   RETURN abs(Self)

METHOD chr() CLASS Numeric
   RETURN chr(Self)

METHOD empty() CLASS Numeric
   RETURN empty(Self)

METHOD int() CLASS Numeric
   RETURN int(Self)

METHOD round(n) CLASS Numeric
   RETURN round(Self, n)

METHOD sqrt() CLASS Numeric
   RETURN sqrt(Self)

METHOD str(...) CLASS Numeric
   RETURN str(Self, ...)

METHOD transform(...) CLASS Numeric
   RETURN transform(Self, ...)

/* --- */

CREATE CLASS Symbol INHERIT HBScalar FUNCTION __HBSymbol

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Symbol
   RETURN "@" + ::name + "()"

/* --- */

CREATE CLASS Pointer INHERIT HBScalar FUNCTION __HBPointer

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Pointer
   RETURN "<0x...>"
