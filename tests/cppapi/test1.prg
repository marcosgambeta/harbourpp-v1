//
// C++ API test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// Compile with:
// hbmk2 test1 -cflag=-D_HB_API_INTERNAL_

#define VALUE 1000000000 // 1.000.000.000

PROCEDURE Main()

   LOCAL nSum
   LOCAL n

   nSum := 0
   ? time()
   FOR n := 1 TO VALUE
      nSum += TEST1()
   NEXT n
   ? time()
   ? nSum

   nSum := 0
   ? time()
   FOR n := 1 TO VALUE
      nSum += TEST2()
   NEXT n
   ? time()
   ? nSum

RETURN

#pragma BEGINDUMP

#include <hbapi.hpp>
#include <hbapiitm.hpp>

// using stack (more fast)
HB_FUNC(TEST1)
{
  _HB_ITEM value{};
  value.putNI(1);
  hb_retni(value.getNI());
  value.clear();
}

// using heap (more slow)
HB_FUNC(TEST2)
{
  auto value = hb_itemNew(nullptr);
  value->putNI(1);
  hb_retni(value->getNI());
  hb_itemRelease(value);
}

#pragma ENDDUMP
