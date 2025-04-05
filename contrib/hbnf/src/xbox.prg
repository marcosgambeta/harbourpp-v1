/*
 * Author....: Don Opperthauser
 * CIS ID....: ?
 *
 * This is an original work by Don Opperthauser and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:47:06   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:05:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 17:55:50   GLENN
 * Fixed bug where extra blank line was displayed in the box.
 *
 *    Rev 1.0   01 Apr 1991 01:02:34   GLENN
 * Nanforum Toolkit
 *
 */

#include <box.ch>

/* NOTE: In original NF, flag parameters were also accepted when
         having extra characters (f.e. "DOUBLE" instead of "D"),
         but only if _SET_EXACT was set to .F., Harbour accepts them
         that way regardless of _SET_EXACT setting. [vszakats] */

FUNCTION ft_XBox( ;
      cJustType, ; // "L" -> left, otherwise centered
      cRetWait, ; // "W" -> wait for keypress before continuing
      cBorType, ; // "D" -> double, anything else single border
      cBorColor, ; // color string for border
      cBoxColor, ; // color string for text
      nStartRow, ; // upper row of box.  99=center vertically
      nStartCol, ; // left edge of box.  99=center horizontally
      cLine1, cLine2, cLine3, cLine4, cLine5, cLine6, cLine7, cLine8)

   LOCAL nLLen := 0
   LOCAL nLCol
   LOCAL nRCol
   LOCAL nTRow
   LOCAL nBRow
   LOCAL nLoop
   LOCAL nSayRow
   LOCAL nSayCol
   LOCAL nNumRows
   LOCAL aLines_[8]

   hb_default(@cJustType, "")
   hb_default(@cRetWait, "")
   hb_default(@cBorType, "")
   hb_default(@cBorColor, "N/W")
   hb_default(@cBoxColor, "W/N")
   hb_default(@nStartRow, 99)
   hb_default(@nStartCol, 99)

   cJustType := Upper(cJustType)
   cRetWait  := Upper(cRetWait)
   cBorType  := Upper(cBorType)

   nNumRows := Min(PCount() - 7, 8)

   // establish array of strings to be displayed
   aLines_[1] := IIf(HB_IsString(cLine1), AllTrim(Left(cLine1, 74)), "")
   aLines_[2] := IIf(HB_IsString(cLine2), AllTrim(Left(cLine2, 74)), "")
   aLines_[3] := IIf(HB_IsString(cLine3), AllTrim(Left(cLine3, 74)), "")
   aLines_[4] := IIf(HB_IsString(cLine4), AllTrim(Left(cLine4, 74)), "")
   aLines_[5] := IIf(HB_IsString(cLine5), AllTrim(Left(cLine5, 74)), "")
   aLines_[6] := IIf(HB_IsString(cLine6), AllTrim(Left(cLine6, 74)), "")
   aLines_[7] := IIf(HB_IsString(cLine7), AllTrim(Left(cLine7, 74)), "")
   aLines_[8] := IIf(HB_IsString(cLine8), AllTrim(Left(cLine8, 74)), "")
   ASize(aLines_, Min(nNumRows, 8))

   // determine longest line
   nLoop := 1
   AEval(aLines_, {||nLLen := Max(nLLen, Len(aLines_[nLoop])), nLoop++})

   // calculate corners
   nLCol := IIf(nStartCol == 99, Int((76 - nLLen) / 2 ), Min(nStartCol, 74 - nLLen))
   nRCol := nLCol + nLLen + 3
   nTRow := IIf(nStartRow == 99, Int((24 - nNumRows) / 2), Min(nStartRow, 22 - nNumRows))
   nBRow := nTRow + nNumRows + 1

   // form box and border

   // save screen color and set new color
   hb_Scroll(nTRow, nLCol, nBRow, nRCol)

   // draw border
   SetColor(cBorColor)
   IF Left(cBorType, 1) == "D"
      hb_DispBox(nTRow, nLCol, nBRow, nRCol, HB_B_DOUBLE_UNI)
   ELSE
      hb_DispBox(nTRow, nLCol, nBRow, nRCol, HB_B_SINGLE_UNI)
   ENDIF

   // write shadow
   hb_Shadow(nTRow, nLCol, nBRow, nRCol)

   // print text in box
   SetColor(cBoxColor)
   nLoop := 1
   AEval(aLines_, {|cSayStr| ;
      nSayRow := nTRow + nLoop, ;
      nSayCol := IIf(Left(cJustType, 1) == "L", ;
      nLCol + 2, ;
      nLCol + 2 + (nLLen - Int(Len(aLines_[nLoop]))) / 2), ;
      nLoop++, ;
      hb_DispOutAt(nSayRow, nSayCol, cSayStr);
      })

   // wait for keypress if desired
   IF Left(cRetWait, 1) == "W"
      Inkey(0)
   ENDIF

   RETURN NIL
