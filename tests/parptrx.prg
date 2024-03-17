PROCEDURE Main()

   ? MyFunc1()
   ? MyFunc1(GETPOINTER())
   ? MyFunc1(65535)
   ? MyFunc1(0xFFFFFFFF)
   ? MyFunc1("abc")
   ? MyFunc1(NIL)

   ?

   ? MyFunc2()
   ? MyFunc2(GETPOINTER())
   ? MyFunc2(65535)
   ? MyFunc2(0xFFFFFFFF)
   ? MyFunc2("abc")
   ? MyFunc2(NIL)

RETURN

#pragma BEGINDUMP

#include <hbapi.hpp>

HB_FUNC(MYFUNC1)
{
  hb_retptr(hb_parptrx(1));
}

HB_FUNC(MYFUNC2)
{
  hb_retptr(hb_parptr(1));
}

HB_FUNC(GETPOINTER)
{
  hb_retptr(reinterpret_cast<void *>(0xFF00FF00));
}

#pragma ENDDUMP
