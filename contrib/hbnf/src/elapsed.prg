/*
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: ?
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:37:56   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:18   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_Elapsed(dStart, dEnd, cTimeStart, cTimeEnd)

   LOCAL nTotalSec
   LOCAL nCtr
   LOCAL nConstant
   LOCAL nTemp
   LOCAL aRetVal[4][2]

   IF hb_IsString(dStart)
      cTimeStart := dStart
      dStart     := Date()
   ELSEIF !hb_IsDate(dStart)
      dStart     := Date()
   ENDIF

   IF hb_IsString(dEnd)
      cTimeEnd := dEnd
      dEnd     := Date()
   ELSEIF !hb_IsDate(dEnd)
      dEnd     := Date()
   ENDIF

   IF !hb_IsString(cTimeStart)
      cTimeStart := "00:00:00"
   ENDIF
   IF !hb_IsString(cTimeEnd)
      cTimeEnd   := "00:00:00"
   ENDIF

   nTotalSec := (dEnd - dStart) * 86400 + ;
      Val(cTimeEnd) *  3600 + ;
      Val(SubStr(cTimeEnd, At(":", cTimeEnd) + 1, 2)) * 60 + ;
      IIf(RAt(":", cTimeEnd) == At(":", cTimeEnd), 0, ;
      Val(SubStr(cTimeEnd, RAt(":", cTimeEnd) + 1))) - ;
      Val(cTimeStart) * 3600 - ;
      Val(SubStr(cTimeStart, At(":", cTimeStart) + 1, 2)) * 60 - ;
      IIf(RAt(":", cTimeStart) == At(":", cTimeStart), 0, ;
      Val(SubStr(cTimeStart, RAt(":", cTimeStart) + 1)))

   nTemp := nTotalSec

   FOR nCtr := 1 TO 4
      nConstant := IIf(nCtr == 1, 86400, IIf(nCtr == 2, 3600, IIf(nCtr == 3, 60, 1)))
      aRetVal[nCtr][1] := Int(nTemp / nConstant)
      aRetval[nCtr][2] := nTotalSec / nConstant
      nTemp -= aRetVal[nCtr][1] * nConstant
   NEXT

   RETURN aRetVal
