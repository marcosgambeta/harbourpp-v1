REQUEST HBNUMERIC

PROCEDURE Main()

   LOCAL n := 123.45

   ? "n", n
   ? "n:className()", n:className()
   ? "n:asString()", n:asString()
   ? "n:asExpStr()", n:asExpStr()
   ? "n:abs()", n:abs()
   ? "n:chr()", n:chr()
   ? "n:empty()", n:empty()
   ? "n:int()", n:int()
   ? "n:round(4)", n:round(4)
   ? "n:sqrt()", n:sqrt()
   ? "n:str()", n:str()
   ? "n:transform('$9,999,999.99')", n:transform("$9,999,999.99")

RETURN
