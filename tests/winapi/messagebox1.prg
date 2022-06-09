/*
 * WinApi test
 *
 * Copyright 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

  ? WinApi_MessageBox(NIL, "message", "title", 0)
  ? WinApi_MessageBox(NIL, NIL, "title", 0)
  ? WinApi_MessageBox(NIL, "message", NIL, 0)

RETURN
