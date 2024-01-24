/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "winapi_winuser.ch"

PROCEDURE Main()

  ? waMessageBox(NIL, "message", "title", MB_OK)
  ? waMessageBox(NIL, "message", "title", MB_OKCANCEL)
  ? waMessageBox(NIL, "message", "title", MB_ABORTRETRYIGNORE)
  ? waMessageBox(NIL, "message", "title", MB_YESNOCANCEL)
  ? waMessageBox(NIL, "message", "title", MB_YESNO)
  ? waMessageBox(NIL, "message", "title", MB_RETRYCANCEL)
  ? waMessageBox(NIL, "message", "title", MB_CANCELTRYCONTINUE)

RETURN
