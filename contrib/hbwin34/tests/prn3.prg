// Copyright 2009 Viktor Szakats

#require "hbwin34"

#include "simpleio.ch"

PROCEDURE Main()

   Dump( win_printerList( FALSE, FALSE ) )
   Dump( win_printerList( FALSE, TRUE ) )
   Dump( win_printerList( TRUE, FALSE ) )
   Dump( win_printerList( TRUE, TRUE ) )

   ? "win_printerGetDefault():", ">" + win_printerGetDefault() + "<"
   ? "win_printerStatus():", hb_ntos(win_printerStatus())

   RETURN

STATIC PROCEDURE Dump( a )

   LOCAL b, c

   ? "==="
   FOR EACH b IN a
      ?
      IF hb_IsArray(b)
         FOR EACH c IN b
            ?? c:__enumIndex(), c
            IF c:__enumIndex() == 2
               ?? "", ;
                  ">>" + win_printerPortToName( c ) + "<<", ;
                  "|>>" + win_printerPortToName( c, TRUE ) + "<<|"
            ENDIF
            ?
         NEXT
         ? "---"
      ELSE
         ? b, win_printerExists( b ), win_printerStatus( b )
      ENDIF
   NEXT

   RETURN
