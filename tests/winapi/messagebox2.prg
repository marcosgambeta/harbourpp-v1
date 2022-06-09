/*
 * WinApi test
 *
 * Copyright 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "winapi_winuser.ch"

PROCEDURE Main()

  ? WinApi_MessageBox(NIL, "message", "title", MB_OK)
  ? WinApi_MessageBox(NIL, "message", "title", MB_OKCANCEL)
  ? WinApi_MessageBox(NIL, "message", "title", MB_ABORTRETRYIGNORE)
  ? WinApi_MessageBox(NIL, "message", "title", MB_YESNOCANCEL)
  ? WinApi_MessageBox(NIL, "message", "title", MB_YESNO)
  ? WinApi_MessageBox(NIL, "message", "title", MB_RETRYCANCEL)
  ? WinApi_MessageBox(NIL, "message", "title", MB_CANCELTRYCONTINUE)

RETURN
