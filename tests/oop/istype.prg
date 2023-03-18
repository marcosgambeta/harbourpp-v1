REQUEST HBNUMERIC
REQUEST HBCHARACTER
REQUEST HBLOGICAL
REQUEST HBDATE
REQUEST HBARRAY
REQUEST HBBLOCK
REQUEST HBHASH

PROCEDURE Main()

   LOCAL n := 0
   LOCAL c := "string"
   LOCAL l := .T.
   LOCAL d := date()
   LOCAL a := {}
   LOCAL b := {||NIL}
   LOCAL h := {=>}

   ? "n", n
   ? "n:isNumeric()", n:isNumeric() // .T.

   ? "c", c
   ? "c:isString()", c:isString() // .T.
   ? "c:isChar()", c:isChar() // .T.
   ? "c:isMemo()", c:isMemo() // .F.
   ? "c:isNull()", c:isNull() // .F.

   ? "l", l
   ? "l:isLogical()", l:isLogical() // .T.

   ? "d", d
   ? "d:isDate()", d:isDate() // .T.

   ? "a", a
   ? "a:isArray()", a:isArray() // .T.
   ? "a:isNull()", a:isNull() // .T.

   ? "b", b
   ? "b:isBlock()", b:isBlock() // .T.

   ? "h", h
   ? "h:isHash()", h:isHash() // .T.
   ? "h:isNull()", h:isNull() // .T.

RETURN
