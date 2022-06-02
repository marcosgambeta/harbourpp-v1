/*
 * WinApi test
 *
 * Copyright 2012 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL oPoint

   oPoint := WINAPI_POINT():new()

   ? "pointer=", oPoint:pointer

   ? "WinApi_GetCursorPos(oPoint)=", WinApi_GetCursorPos(oPoint)

   ? "X=", oPoint:GetX()
   ? "Y=", oPoint:GetY()

RETURN
