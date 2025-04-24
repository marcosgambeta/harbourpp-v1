//
// Misc CA-T*ols functions
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include <color.ch>
#include <setcurs.ch>

#include <hbmemory.ch>

FUNCTION AlloFree(lMode)

   hb_default(@lMode, .F.)

   RETURN Memory(IIf(lMode, HB_MEM_CHAR, HB_MEM_BLOCK))

FUNCTION Center(c, n, p, lMode)

   LOCAL cRet

   IF !hb_IsNumeric(n)
      n := MaxCol() + 1 - Col() * 2
   ENDIF
   IF !hb_IsString(c)
      c := ""
   ENDIF

   IF hb_IsLogical(p)
      lMode := p
      p := NIL
   ELSE
      IF !hb_IsLogical(lMode)
         lMode := .F.
      ENDIF
   ENDIF

   cRet := PadC(RTrim(c), n, p)

   RETURN IIf(lMode, cRet, RTrim(cRet))

FUNCTION CSetCurs(l)

   IF !hb_IsLogical(l)
      RETURN SetCursor() != SC_NONE
   ENDIF

   RETURN SetCursor(IIf(l, SC_NORMAL, SC_NONE)) != SC_NONE

FUNCTION CSetKey(n)

   RETURN SetKey(n)

FUNCTION CSetCent(nCentury)

   RETURN __SetCentury(nCentury)

FUNCTION LToC(l)

   RETURN IIf(l, "T", "F")

FUNCTION DosParam()

   LOCAL cRet := ""
   LOCAL nCount := hb_argc(), i

   FOR i := 1 TO nCount
      cRet += IIf(i == 1, "", " ") + hb_argv(i)
   NEXT

   RETURN cRet

FUNCTION ExeName()

   RETURN hb_ProgName()
