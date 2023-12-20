/*
 * WinApi test
 *
 * Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL oPoint

   oPoint := WINAPI_STRUCT_POINT():new()

   ? "pointer=", oPoint:ptr

   ? "waGetCursorPos(oPoint)=", waGetCursorPos(oPoint)

   ? "X=", oPoint:x
   ? "Y=", oPoint:y

RETURN
