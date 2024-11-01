//
// WinApi test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

PROCEDURE Main()

  ? waMessageBox(NIL, "message", "title", 0)
  ? waMessageBox(NIL, NIL, "title", 0)
  ? waMessageBox(NIL, "message", NIL, 0)
  
  WAIT

RETURN
