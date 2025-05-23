//
// Copyright 2009 Viktor Szakats (vszakats.net/harbour)
//

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   Dump( win_printerList( .F., .F. ) )
   Dump( win_printerList( .F., .T. ) )
   Dump( win_printerList( .T., .F. ) )
   Dump( win_printerList( .T., .T. ) )

   ? "WIN_PRINTERGETDEFAULT:", ">" + win_printerGetDefault() + "<"
   ? "WIN_PRINTERSTATUS:", win_printerStatus()

   RETURN

STATIC PROCEDURE Dump( a )

   LOCAL b, c

   ? "=================="
   FOR EACH b IN a
      ?
      IF hb_IsArray(b)
         FOR EACH c IN b
            ?? c:__enumIndex(), c
            IF c:__enumIndex() == 2
               ?? ">>" + win_printerPortToName( c ) + "<<",  "|>>" + win_printerPortToName( c, .T. ) + "<<|"
            ENDIF
            ?
         NEXT
         ? "-----"
      ELSE
         ? b, win_printerExists( b ), win_printerStatus( b )
      ENDIF
   NEXT

   RETURN
