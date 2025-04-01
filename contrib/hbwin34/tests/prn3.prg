/* Copyright 2009 Viktor Szakats */

#require "hbwin34"

#include "simpleio.ch"

PROCEDURE Main()

   Dump( win_printerList( .F., .F. ) )
   Dump( win_printerList( .F., .T. ) )
   Dump( win_printerList( .T., .F. ) )
   Dump( win_printerList( .T., .T. ) )

   ? "win_printerGetDefault():", ">" + win_printerGetDefault() + "<"
   ? "win_printerStatus():", hb_ntos(win_printerStatus())

   RETURN

STATIC PROCEDURE Dump( a )

   LOCAL b, c

   ? "==="
   FOR EACH b IN a
      ?
      IF HB_IsArray(b)
         FOR EACH c IN b
            ?? c:__enumIndex(), c
            IF c:__enumIndex() == 2
               ?? "", ;
                  ">>" + win_printerPortToName( c ) + "<<", ;
                  "|>>" + win_printerPortToName( c, .T. ) + "<<|"
            ENDIF
            ?
         NEXT
         ? "---"
      ELSE
         ? b, win_printerExists( b ), win_printerStatus( b )
      ENDIF
   NEXT

   RETURN
