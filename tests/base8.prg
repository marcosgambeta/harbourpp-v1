// test with base 8

PROCEDURE Main()

   //? 0o // syntax error
   //? 0O // syntax error
   //? 0o8 // syntax error
   //? 0o9 // syntax error

   ? 0o0
   ? 0o1
   ? 0o2
   ? 0o3
   ? 0o4
   ? 0o5
   ? 0o6
   ? 0o7

   ? 0o10
   ? 0o11
   ? 0o12
   ? 0o13
   ? 0o14
   ? 0o15
   ? 0o16
   ? 0o17

   ?

   ? -0o0
   ? -0o1
   ? -0o2
   ? -0o3
   ? -0o4
   ? -0o5
   ? -0o6
   ? -0o7

   ? -0o10
   ? -0o11
   ? -0o12
   ? -0o13
   ? -0o14
   ? -0o15
   ? -0o16
   ? -0o17

   ?

   ? 0o10          // result = 8
   ? 0O777         // result = 511
   ? 0o0           // result = 0
   ? 0o17 + 0o1    // result = 16
   ? -0o10         // result = -8

   ?

   ? 0o377 + 0o1
   ? 0o377 * 2
   ? 0o377 * 3
   ? 0o377 * 4
   ? 0o377 / 2
   ? 0o377 / 3
   ? 0o377 / 4

   WAIT

RETURN
