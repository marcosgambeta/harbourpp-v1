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
   Return Self

METHOD ScalarObject:IsScalar()
   Return True

METHOD ScalarObject:AsString()

   Switch ValType(Self)
   Case "B" ; Return "{ || ... }"
   Case "M"
   Case "C" ; Return Self
   Case "D" ; Return DToC(Self)
   Case "T" ; Return hb_TToC(Self)
   Case "H" ; Return "{ ... => ... }"
   Case "L" ; Return IIf(Self, ".T.", ".F.")
   Case "N" ; Return hb_ntos(Self)
   Case "S" ; Return "@" + ::name + "()"
   Case "P" ; Return "<0x...>"
   Case "U" ; Return "NIL"
   EndSwitch

   Return "Error!"

METHOD ScalarObject:AsExpStr()

   Switch ValType(Self)
   Case "M"
   Case "C" ; Return '"' + Self + '"'
   Case "D" ; Return 'CToD("' + DToC(Self) + '")'
   Case "T" ; Return 'hb_CToT("' + hb_TToC(Self) + '")'
   EndSwitch

   Return ::AsString()

METHOD ScalarObject:isNumeric()
   Return hb_IsNumeric(Self)

METHOD ScalarObject:isLogical()
   Return hb_IsLogical(Self)

METHOD ScalarObject:isDate()
   Return hb_IsDate(Self)

METHOD ScalarObject:isDateTime()
   Return hb_IsDateTime(Self)

METHOD ScalarObject:isTimeStamp()
   Return hb_IsTimestamp(Self)

METHOD ScalarObject:isBlock()
   Return hb_IsBlock(Self)

METHOD ScalarObject:isPointer()
   Return hb_IsPointer(Self)

METHOD ScalarObject:isSymbol()
   Return hb_IsSymbol(Self)

METHOD ScalarObject:isString()
   Return hb_IsString(Self)

METHOD ScalarObject:isChar()
   Return hb_IsChar(Self)

METHOD ScalarObject:isMemo()
   Return hb_IsMemo(Self)

METHOD ScalarObject:isArray()
   Return hb_IsArray(Self)

METHOD ScalarObject:isObject()
   Return hb_IsObject(Self)

METHOD ScalarObject:isHash()
   Return hb_IsHash(Self)

METHOD ScalarObject:isHashKey()
   Return hb_IsHashKey(Self)

METHOD ScalarObject:isEvalItem()
   Return hb_IsEvalItem(Self)

METHOD ScalarObject:isNull()
   Return hb_IsNull(Self)

METHOD PROCEDURE ScalarObject:BecomeErr()

#if 0
   // Not implemented yet
   ::error(CSYERR_BECOME, "Message 'become' illegally sent to scalar", ::ClassName())
#endif

   Return

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

   Return Self

METHOD Array:AddAll(aOtherCollection)

   aOtherCollection:Do({|e|::Add(e)})

   Return Self

METHOD Array:AsString()
   Return "{ ... }"

METHOD Array:At(n)
   Return Self[n]

METHOD Array:AtPut(n, x)
   Return Self[n] := x

METHOD Array:Add(x)

   AAdd(Self, x)

   Return True

METHOD Array:Collect(b)

   Local elem
   Local result := {}

   For Each elem In Self
      IF Eval(b, elem)
         AAdd(result, elem)
      ENDIF
   Next

   Return result

METHOD Array:Copy()
   Return ACopy(Self, Array(Len(Self)))

METHOD Array:DeleteAt(n)

   IF n >= 1 .AND. n <= Len(Self)
      hb_ADel(Self, n, True)
   ENDIF

   Return Self

METHOD Array:InsertAt(n, x)

   Do Case
   Case n > Len(Self)
      ASize(Self, n)
      Self[n] := x
   Case n >= 1
      hb_AIns(Self, n, x, True)
   EndCase

   Return Self

METHOD Array:IsScalar()
   Return True

METHOD Array:Do(b)

   Local i

   For i := 1 To Len(Self)
      b:Eval(Self[i], i)
   Next

   Return Self

METHOD Array:IndexOf(x)

   Local elem

   For Each elem In Self
      IF elem == x
         Return elem:__enumIndex()
      ENDIF
   Next

   Return 0

METHOD PROCEDURE Array:Remove(e)

   ::DeleteAt(::IndexOf(e))

   Return

METHOD Array:Scan(b)
   Return AScan(Self, b)

METHOD Array:_Size(newSize)

   ASize(Self, newSize)

   Return newSize  // so that assignment works according to standard rules

/* --- */

CREATE CLASS Block INHERIT HBScalar FUNCTION __HBBlock

   METHOD AsString()

ENDCLASS

METHOD Block:AsString()
   Return "{ || ... }"

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
   Return Self

METHOD Character:AsExpStr()
   Return '"' + Self + '"'

METHOD Character:at(c)
   Return at(c, Self)

METHOD Character:asc()
   Return asc(Self)

METHOD Character:empty()
   Return empty(Self)

METHOD Character:isAlpha()
   Return isAlpha(Self)

METHOD Character:isDigit()
   Return isDigit(Self)

METHOD Character:isLower()
   Return isLower(Self)

METHOD Character:isUpper()
   Return isUpper(Self)

METHOD Character:left(n)
   Return left(Self, n)

METHOD Character:len()
   Return len(Self)

METHOD Character:lower()
   Return lower(Self)

METHOD Character:ltrim()
   Return ltrim(Self)

METHOD Character:padl(...)
   Return padl(Self, ...)

METHOD Character:padc(...)
   Return padc(Self, ...)

METHOD Character:padr(...)
   Return padr(Self, ...)

METHOD Character:rat(c)
   Return rat(c, Self)

METHOD Character:replicate(n)
   Return replicate(Self, n)

METHOD Character:right(n)
   Return right(Self, n)

METHOD Character:rtrim()
   Return rtrim(Self)

METHOD Character:soundex()
   Return soundex(Self)

METHOD Character:strtran(...)
   Return strtran(Self, ...)

METHOD Character:stuff(...)
   Return stuff(Self, ...)

METHOD Character:substr(...)
   Return substr(Self, ...)

METHOD Character:transform(c)
   Return transform(Self, c)

METHOD Character:trim()
   Return trim(Self)

METHOD Character:upper()
   Return upper(Self)

METHOD Character:val()
   Return val(Self)

/* --- */

CREATE CLASS Date INHERIT HBScalar FUNCTION __HBDate

   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD Date:AsString()
   Return DToC(Self)

METHOD Date:AsExpStr()
   Return 'CToD("' + ::AsString() + '")'

METHOD Date:Year()
   Return Year(Self)

METHOD Date:Month()
   Return Month(Self)

METHOD Date:Day()
   Return Day(Self)

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
   Return hb_TToS(Self)

METHOD TimeStamp:AsExpStr()
   Return 'hb_SToT("' + ::AsString() + '")'

METHOD TimeStamp:Date()
   Return hb_TToC(Self, NIL, "")

METHOD TimeStamp:Time()
   Return hb_TToC(Self, "", "hh:mm:ss")

METHOD TimeStamp:Year()
   Return Year(Self)

METHOD TimeStamp:Month()
   Return Month(Self)

METHOD TimeStamp:Day()
   Return Day(Self)

METHOD TimeStamp:Hour()
   Return hb_Hour(Self)

METHOD TimeStamp:Minute()
   Return hb_Minute(Self)

METHOD TimeStamp:Sec()
   Return hb_Sec(Self)

/* --- */

CREATE CLASS Hash INHERIT HBScalar FUNCTION __HBHash

   METHOD AsString()

ENDCLASS

METHOD Hash:AsString()
   Return "{ ... => ... }"

/* --- */

CREATE CLASS Logical INHERIT HBScalar FUNCTION __HBLogical

   METHOD AsString()

ENDCLASS

METHOD Logical:AsString()
   Return IIf(Self, ".T.", ".F.")

/* --- */

CREATE CLASS NIL INHERIT HBScalar FUNCTION __HBNil

   METHOD AsString()

ENDCLASS

METHOD NIL:AsString()
   Return "NIL"

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
   Return hb_ntos(Self)

METHOD Numeric:abs()
   Return abs(Self)

METHOD Numeric:chr()
   Return chr(Self)

METHOD Numeric:empty()
   Return empty(Self)

METHOD Numeric:int()
   Return int(Self)

METHOD Numeric:round(n)
   Return round(Self, n)

METHOD Numeric:sqrt()
   Return sqrt(Self)

METHOD Numeric:str(...)
   Return str(Self, ...)

METHOD Numeric:transform(...)
   Return transform(Self, ...)

/* --- */

CREATE CLASS Symbol INHERIT HBScalar FUNCTION __HBSymbol

   METHOD AsString()

ENDCLASS

METHOD Symbol:AsString()
   Return "@" + ::name + "()"

/* --- */

CREATE CLASS Pointer INHERIT HBScalar FUNCTION __HBPointer

   METHOD AsString()

ENDCLASS

METHOD Pointer:AsString()
   Return "<0x...>"
