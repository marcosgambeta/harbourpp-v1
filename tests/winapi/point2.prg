/*
 * WinApi test
 *
 * Copyright 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL oPoint

   oPoint := WINAPI_STRUCT_POINT():new()

   ? "pointer=", oPoint:ptr

   ? "WinApi_GetCursorPos(oPoint)=", WinApi_GetCursorPos(oPoint)

   ? "X=", oPoint:GetX()
   ? "Y=", oPoint:GetY()

RETURN