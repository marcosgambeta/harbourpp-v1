#require "hbnf"

PROCEDURE Main( cCk, cStr, nOcc, xCase )

   LOCAL nFind

   IF PCount() != 4
      ? "usage: findith cCk cStr nOcc xCase"
   ELSE
      xCase := IIf(xCase == "Y", .T., .F.)
      nOcc  := Val( nOcc )
      ? IIf(xCase, "Ignoring ", "Observing ") + "case:"

      ? cStr
      nFind := ft_FindITh( cCk, cStr, nOcc, xCase )
      ? IIf(nFind > 0, Space( nFind - 1 ) + "^", "Not found")
   ENDIF

   RETURN
