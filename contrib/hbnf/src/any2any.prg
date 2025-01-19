/*
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:34   GLENN
 * Nanforum Toolkit
 *
 */

#define CASE_AT(x, y, z)             z[At(x, y) + 1]
#define NULL                         ""
#define EARLIEST_DATE                hb_SToD("01000101")
#define BLANK_DATE                   hb_SToD()

#define _XTOC(x)             CASE_AT(ValType(x), "CNDLM", ;
      {NULL, ;
      x, ;
      iif(HB_IsNumeric(x), hb_ntos(x), NULL), ;
      iif(HB_IsDate(x), DToC(x), NULL), ;
      iif(HB_IsLogical(x), iif(x, ".T.", ".F."), NULL), ;
      x})

FUNCTION ft_XToY(xValueToConvert, cTypeToConvertTo, lWantYesNo)

   __defaultNIL(@lWantYesNo, .F.)

   DO CASE

   CASE cTypeToConvertTo == "C" .AND. ; // They Want a Character String
      !HB_IsString(xValueToConvert)

      xValueToConvert := _XTOC(xValueToConvert)

   CASE cTypeToConvertTo == "D" .AND. ; // They Want a Date
      !HB_IsDate(xValueToConvert)

      xValueToConvert := iif(HB_IsString(xValueToConvert), ;
         ; // Convert from a Character
      CToD(xValueToConvert), ;
         iif(HB_IsNumeric(xValueToConvert), ;
         ; // Convert from a Number
      xValueToConvert + EARLIEST_DATE, ;
         iif(HB_IsLogical(xValueToConvert), ;
         ; // Convert from a Logical
      iif(xValueToConvert, Date(), BLANK_DATE), ;
         ; // Unsupported Type
      BLANK_DATE)))

   CASE cTypeToConvertTo == "N" .AND. ; // They Want a Number
      !HB_IsNumeric(xValueToConvert)

      xValueToConvert := iif(HB_IsString(xValueToConvert), ;
         ; // Convert from a Character
      Val(xValueToConvert), ;
         iif(HB_IsDate(xValueToConvert), ;
         ; // Convert from a Date
      xValueToConvert - EARLIEST_DATE, ;
         iif(HB_IsLogical(xValueToConvert), ;
         ; // Convert from a Logical
      iif(xValueToConvert, 1, 0), ;
         ; // Unsupported Type
      0)))

   CASE cTypeToConvertTo == "L" .AND. ; // They Want a Logical
      !HB_IsLogical(xValueToConvert)

      xValueToConvert := iif(HB_IsString(xValueToConvert), ;
         ; // Convert from a Character
      Upper(xValueToConvert) == iif(lWantYesNo, "Y", ".T."), ;
         iif(HB_IsDate(xValueToConvert), ;
         ; // Convert from a Date
      !Empty(xValueToConvert), ;
         iif(HB_IsNumeric(xValueToConvert), ;
         ; // Convert from a Number
      xValueToConvert != 0, ;
         ; // Unsupported Type
      .F.)))

   CASE cTypeToConvertTo == "A" .AND. ; // They Want an Array
      !HB_IsArray(xValueToConvert)

      xValueToConvert := {xValueToConvert}

   CASE cTypeToConvertTo == "B" .AND. ; // They Want a Code Block
      !HB_IsBlock(xValueToConvert)

      xValueToConvert := {||xValueToConvert}

   ENDCASE

   RETURN xValueToConvert
