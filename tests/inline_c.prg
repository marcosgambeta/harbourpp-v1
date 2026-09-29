PROCEDURE Main()

   ? C_FUNC()

   ? EndDumpTest()

   RETURN

#pragma begindump

#include "hbapi.hpp"

HB_FUNC( C_FUNC )
{
   hb_retc( "returned from C_FUNC()\n" );
}

#pragma enddump

FUNCTION EndDumpTest()
   RETURN "End Dump Test"
