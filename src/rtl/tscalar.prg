//
// Harbour implementation of Class(y) Scalar classes
//
// Copyright 2004 Antonio Linares <alinares@fivetechsoft.com>
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

METHOD ScalarObject:Copy()
   RETURN Self

METHOD ScalarObject:IsScalar()
   RETURN .T.

METHOD ScalarObject:AsString()

   SWITCH ValType(Self)
   CASE "B" ; RETURN "{ || ... }"
   CASE "M"
   CASE "C" ; RETURN Self
   CASE "D" ; RETURN DToC(Self)
   CASE "T" ; RETURN hb_TToC(Self)
   CASE "H" ; RETURN "{ ... => ... }"
   CASE "L" ; RETURN IIf(Self, ".T.", ".F.")
   CASE "N" ; RETURN hb_ntos(Self)
   CASE "S" ; RETURN "@" + ::name + "()"
   CASE "P" ; RETURN "<0x...>"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "Error!"

METHOD ScalarObject:AsExpStr()

   SWITCH ValType(Self)
   CASE "M"
   CASE "C" ; RETURN '"' + Self + '"'
   CASE "D" ; RETURN 'CToD("' + DToC(Self) + '")'
   CASE "T" ; RETURN 'hb_CToT("' + hb_TToC(Self) + '")'
   ENDSWITCH

   RETURN ::AsString()

METHOD ScalarObject:isNumeric()
   RETURN hb_IsNumeric(Self)

METHOD ScalarObject:isLogical()
   RETURN hb_IsLogical(Self)

METHOD ScalarObject:isDate()
   RETURN hb_IsDate(Self)

METHOD ScalarObject:isDateTime()
   RETURN hb_IsDateTime(Self)

METHOD ScalarObject:isTimeStamp()
   RETURN hb_IsTimestamp(Self)

METHOD ScalarObject:isBlock()
   RETURN hb_IsBlock(Self)

METHOD ScalarObject:isPointer()
   RETURN hb_IsPointer(Self)

METHOD ScalarObject:isSymbol()
   RETURN hb_IsSymbol(Self)

METHOD ScalarObject:isString()
   RETURN hb_IsString(Self)

METHOD ScalarObject:isChar()
   RETURN hb_IsChar(Self)

METHOD ScalarObject:isMemo()
   RETURN hb_IsMemo(Self)

METHOD ScalarObject:isArray()
   RETURN hb_IsArray(Self)

METHOD ScalarObject:isObject()
   RETURN hb_IsObject(Self)

METHOD ScalarObject:isHash()
   RETURN hb_IsHash(Self)

METHOD ScalarObject:isHashKey()
   RETURN hb_IsHashKey(Self)

METHOD ScalarObject:isEvalItem()
   RETURN hb_IsEvalItem(Self)

METHOD ScalarObject:isNull()
   RETURN hb_IsNull(Self)

METHOD PROCEDURE ScalarObject:BecomeErr()

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

METHOD Array:Init(nElements)

   ::size := IIf(nElements == NIL, 0, nElements)

   RETURN Self

METHOD Array:AddAll(aOtherCollection)

   aOtherCollection:Do({|e|::Add(e)})

   RETURN Self

METHOD Array:AsString()
   RETURN "{ ... }"

METHOD Array:At(n)
   RETURN Self[n]

METHOD Array:AtPut(n, x)
   RETURN Self[n] := x

METHOD Array:Add(x)

   AAdd(Self, x)

   RETURN .T.

METHOD Array:Collect(b)

   LOCAL elem
   LOCAL result := {}

   FOR EACH elem IN Self
      IF Eval(b, elem)
         AAdd(result, elem)
      ENDIF
   NEXT

   RETURN result

METHOD Array:Copy()
   RETURN ACopy(Self, Array(Len(Self)))

METHOD Array:DeleteAt(n)

   IF n >= 1 .AND. n <= Len(Self)
      hb_ADel(Self, n, .T.)
   ENDIF

   RETURN Self

METHOD Array:InsertAt(n, x)

   DO CASE
   CASE n > Len(Self)
      ASize(Self, n)
      Self[n] := x
   CASE n >= 1
      hb_AIns(Self, n, x, .T.)
   ENDCASE

   RETURN Self

METHOD Array:IsScalar()
   RETURN .T.

METHOD Array:Do(b)

   LOCAL i

   FOR i := 1 TO Len(Self)
      b:Eval(Self[i], i)
   NEXT

   RETURN Self

METHOD Array:IndexOf(x)

   LOCAL elem

   FOR EACH elem IN Self
      IF elem == x
         RETURN elem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD PROCEDURE Array:Remove(e)

   ::DeleteAt(::IndexOf(e))

   RETURN

METHOD Array:Scan(b)
   RETURN AScan(Self, b)

METHOD Array:_Size(newSize)

   ASize(Self, newSize)

   RETURN newSize  // so that assignment works according to standard rules

/* --- */

CREATE CLASS Block INHERIT HBScalar FUNCTION __HBBlock

   METHOD AsString()

ENDCLASS

METHOD Block:AsString()
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

METHOD Character:AsString()
   RETURN Self

METHOD Character:AsExpStr()
   RETURN '"' + Self + '"'

METHOD Character:at(c)
   RETURN at(c, Self)

METHOD Character:asc()
   RETURN asc(Self)

METHOD Character:empty()
   RETURN empty(Self)

METHOD Character:isAlpha()
   RETURN isAlpha(Self)

METHOD Character:isDigit()
   RETURN isDigit(Self)

METHOD Character:isLower()
   RETURN isLower(Self)

METHOD Character:isUpper()
   RETURN isUpper(Self)

METHOD Character:left(n)
   RETURN left(Self, n)

METHOD Character:len()
   RETURN len(Self)

METHOD Character:lower()
   RETURN lower(Self)

METHOD Character:ltrim()
   RETURN ltrim(Self)

METHOD Character:padl(...)
   RETURN padl(Self, ...)

METHOD Character:padc(...)
   RETURN padc(Self, ...)

METHOD Character:padr(...)
   RETURN padr(Self, ...)

METHOD Character:rat(c)
   RETURN rat(c, Self)

METHOD Character:replicate(n)
   RETURN replicate(Self, n)

METHOD Character:right(n)
   RETURN right(Self, n)

METHOD Character:rtrim()
   RETURN rtrim(Self)

METHOD Character:soundex()
   RETURN soundex(Self)

METHOD Character:strtran(...)
   RETURN strtran(Self, ...)

METHOD Character:stuff(...)
   RETURN stuff(Self, ...)

METHOD Character:substr(...)
   RETURN substr(Self, ...)

METHOD Character:transform(c)
   RETURN transform(Self, c)

METHOD Character:trim()
   RETURN trim(Self)

METHOD Character:upper()
   RETURN upper(Self)

METHOD Character:val()
   RETURN val(Self)

/* --- */

CREATE CLASS Date INHERIT HBScalar FUNCTION __HBDate

   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD Date:AsString()
   RETURN DToC(Self)

METHOD Date:AsExpStr()
   RETURN 'CToD("' + ::AsString() + '")'

METHOD Date:Year()
   RETURN Year(Self)

METHOD Date:Month()
   RETURN Month(Self)

METHOD Date:Day()
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

METHOD TimeStamp:AsString()
   RETURN hb_TToS(Self)

METHOD TimeStamp:AsExpStr()
   RETURN 'hb_SToT("' + ::AsString() + '")'

METHOD TimeStamp:Date()
   RETURN hb_TToC(Self, NIL, "")

METHOD TimeStamp:Time()
   RETURN hb_TToC(Self, "", "hh:mm:ss")

METHOD TimeStamp:Year()
   RETURN Year(Self)

METHOD TimeStamp:Month()
   RETURN Month(Self)

METHOD TimeStamp:Day()
   RETURN Day(Self)

METHOD TimeStamp:Hour()
   RETURN hb_Hour(Self)

METHOD TimeStamp:Minute()
   RETURN hb_Minute(Self)

METHOD TimeStamp:Sec()
   RETURN hb_Sec(Self)

/* --- */

CREATE CLASS Hash INHERIT HBScalar FUNCTION __HBHash

   METHOD AsString()

ENDCLASS

METHOD Hash:AsString()
   RETURN "{ ... => ... }"

/* --- */

CREATE CLASS Logical INHERIT HBScalar FUNCTION __HBLogical

   METHOD AsString()

ENDCLASS

METHOD Logical:AsString()
   RETURN IIf(Self, ".T.", ".F.")

/* --- */

CREATE CLASS NIL INHERIT HBScalar FUNCTION __HBNil

   METHOD AsString()

ENDCLASS

METHOD NIL:AsString()
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

METHOD Numeric:AsString()
   RETURN hb_ntos(Self)

METHOD Numeric:abs()
   RETURN abs(Self)

METHOD Numeric:chr()
   RETURN chr(Self)

METHOD Numeric:empty()
   RETURN empty(Self)

METHOD Numeric:int()
   RETURN int(Self)

METHOD Numeric:round(n)
   RETURN round(Self, n)

METHOD Numeric:sqrt()
   RETURN sqrt(Self)

METHOD Numeric:str(...)
   RETURN str(Self, ...)

METHOD Numeric:transform(...)
   RETURN transform(Self, ...)

/* --- */

CREATE CLASS Symbol INHERIT HBScalar FUNCTION __HBSymbol

   METHOD AsString()

ENDCLASS

METHOD Symbol:AsString()
   RETURN "@" + ::name + "()"

/* --- */

CREATE CLASS Pointer INHERIT HBScalar FUNCTION __HBPointer

   METHOD AsString()

ENDCLASS

METHOD Pointer:AsString()
   RETURN "<0x...>"
