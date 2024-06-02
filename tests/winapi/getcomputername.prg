/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include <winapi_winbase.ch>

PROCEDURE Main()

   ? "test1"
   test1()

   ?
   ?
   ?

   ? "test2"
   test2()
   
   WAIT

RETURN

STATIC FUNCTION test1()

   LOCAL cComputerName
   LOCAL nSize

   cComputerName := space(MAX_COMPUTERNAME_LENGTH)
   nSize := len(cComputerName)

   ? waGetComputerName(@cComputerName, @nSize)

   ? cComputerName
   ? len(cComputerName)
   ? nSize

RETURN NIL

STATIC FUNCTION test2()

   LOCAL cComputerName
   LOCAL nSize

   nSize := MAX_COMPUTERNAME_LENGTH

   ? waGetComputerName(@cComputerName, @nSize)

   ? cComputerName
   ? len(cComputerName)
   ? nSize

RETURN NIL
