/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL oPoint

   oPoint := wasPOINT()

   ? "pointer=", oPoint:ptr

   ? "waGetCursorPos(oPoint)=", waGetCursorPos(oPoint)

   ? "X=", oPoint:GetX()
   ? "Y=", oPoint:GetY()

RETURN
