//
// hb_ValToExp(), hb_CStr()
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

Function hb_CStr(xVal)

   LOCAL v := ValType(xVal)

   Switch v
   Case "C"
   Case "M" ; RETURN xVal
   Case "N" ; RETURN Str(xVal)
   Case "D" ; RETURN IIf(Empty(xVal), "0d00000000", "0d" + DToS(xVal))
   Case "T" ; RETURN 't"' + hb_TSToStr(xVal, True) + '"'
   Case "L" ; RETURN IIf(xVal, ".T.", ".F.")
   Case "S" ; RETURN "@" + xVal:name + "()"
   Case "B" ; RETURN "{|| ... }"
   Case "O" ; RETURN "{ " + xVal:className() + " Object }"
   Case "A" ; RETURN "{ Array of " + hb_ntos(Len(xVal)) + " Items }"
   Case "H" ; RETURN "{ Hash of " + hb_ntos(Len(xVal)) + " Items }"
   Case "P" ; RETURN "<pointer>"
   Otherwise
      IF xVal == NIL
         RETURN "NIL"
      ENDIF
   EndSwitch

   RETURN "???:" + v

Function hb_ValToExp(xVal, lRaw)

   RETURN s_valToExp(xVal, hb_defaultValue(lRaw, False))

Static Function s_valToExp(xVal, lRaw, cInd, hRefs, cRefs, cObjs)

   LOCAL cVal
   LOCAL cKey
   LOCAL cClass
   LOCAL tmp
   LOCAL v := ValType(xVal)

   Switch v
   Case "C"
   Case "M" ; RETURN hb_StrToExp(xVal)
   Case "N" ; RETURN hb_ntos(xVal)
   Case "D" ; RETURN IIf(Empty(xVal), "0d00000000", "0d" + DToS(xVal))
   Case "T" ; RETURN 't"' + hb_TSToStr(xVal, True) + '"'
   Case "L" ; RETURN IIf(xVal, ".T.", ".F.")
   Case "S" ; RETURN "@" + xVal:name + "()"
   Case "O"
      cClass := xVal:className()
   Case "A"
   Case "H"
      tmp := __vmItemID(xVal)
      IF cInd == NIL
         cInd := cRefs := ""
         hRefs := {tmp => cInd}
      ELSEIF tmp $ hRefs
         IF !(cRefs == "")
            cRefs += ","
         ENDIF
         cRefs += "{{" + cInd + "}," + hRefs[tmp] + "}"
         RETURN "NIL"
      ELSE
         hRefs[tmp] := "{" + cInd + "}"
         cInd += ","
      ENDIF

      IF v == "H"
         IF Empty(xVal)
            cVal := "{=>}"
         ELSE
            cVal := "{"
            For Each tmp In xVal
               cKey := s_valToExp(tmp:__enumKey(), lRaw)
               cVal += IIf(tmp:__enumIsFirst(), "", ", ") + cKey + "=>" + s_valToExp(tmp, lRaw, cInd + cKey, hRefs, @cRefs, @cObjs)
            Next
            cVal += "}"
         ENDIF
      ELSE
         cVal := "{"
         IF !lRaw .AND. v == "O"
            For Each tmp In __objGetIVars(xVal)
               cVal += IIf(tmp:__enumIsFirst(), '{"', ', {"') + ;
                       tmp[1] + '", ' + ;
                       s_valToExp(tmp[2], lRaw, cInd + hb_ntos(tmp:__enumIndex()) + ",2", hRefs, @cRefs, @cObjs) + "}"
            Next
         ELSE
            For Each tmp In xVal
               cVal += IIf(tmp:__enumIsFirst(), "", ", ") + s_valToExp(tmp, lRaw, cInd + hb_ntos(tmp:__enumIndex()), hRefs, @cRefs, @cObjs)
            Next
         ENDIF
         cVal += "}"
      ENDIF

      IF v == "O"
         IF cObjs == NIL
            cObjs := ""
         ELSE
            cObjs += ","
         ENDIF
         cObjs += '{"' + cClass + '",'
         IF !cInd == ""
            cObjs += "{" + hb_StrShrink(cInd) + "}"
         ENDIF
         cObjs += "}"
      ENDIF
      IF cInd == ""
         IF !Empty(cRefs)
            cVal := "__itemSetRef( " + cVal + ", {" + cRefs + "} )"
         ENDIF
         IF !Empty(cObjs)
            cVal := IIf(lRaw, "__itemSetObjRaw( ", "__itemSetObj( ") + cVal + ", {" + cObjs + "} )"
         ENDIF
      ENDIF
      EXIT
   Case "P" ; RETURN "<pointer>"
   Case "B" ; RETURN "{|| ... }"
   Otherwise
      IF xVal == NIL
         cVal := "NIL"
      ELSE
         cVal := "???:" + v
      ENDIF
   EndSwitch

   RETURN cVal

Function __itemSetRef(xVal, aRefs)

   LOCAL aRef

   For Each aRef In aRefs
      xVal[hb_ArrayToParams(aRef[1])] := IIf(aRef[2] == NIL, xVal, xVal[hb_ArrayToParams(aRef[2])])
   Next

   RETURN xVal

Function __itemSetObj(xVal, aObjs)

   LOCAL aRef

   For Each aRef In aObjs
      __objRestoreIVars(IIf(aRef[2] == NIL, xVal, xVal[hb_ArrayToParams(aRef[2])]), aRef[1])
   Next

   RETURN xVal

Function __itemSetObjRaw(xVal, aObjs)

   LOCAL aRef

   For Each aRef In aObjs
      __objSetClass(IIf(aRef[2] == NIL, xVal, xVal[hb_ArrayToParams(aRef[2])]), aRef[1])
   Next

   RETURN xVal
