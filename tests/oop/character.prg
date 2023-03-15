REQUEST HBCHARACTER

PROCEDURE Main()

   LOCAL c := "testing class HBCHARACTER"

   ? "c", c
   ? "c:className()", c:className()
   ? "c:asString()", c:asString()
   ? "c:asExpStr()", c:asExpStr()
   ? "c:at('s')", c:at("s")
   ? "c:asc()", c:asc()
   ? "c:empty()", c:empty()
   ? "c:isAlpha()", c:isAlpha()
   ? "c:isDigit()", c:isDigit()
   ? "c:isLower()", c:isLower()
   ? "c:isUpper()", c:isUpper()
   ? "c:left(5)", c:left(5)
   ? "c:len()", c:len()
   ? "c:lower()", c:lower()
   ? "c:ltrim()", c:ltrim()
   ? "c:padl(40, '.')", c:padl(40, ".")
   ? "c:padc(40, '.')", c:padc(40, ".")
   ? "c:padr(40, '.')", c:padr(40, ".")
   ? "c:rat('s')", c:rat("s")
   ? "c:replicate(2)", c:replicate(2)
   ? "c:right(5)", c:right(5)
   ? "c:rtrim()", c:rtrim()
   ? "c:soundex()", c:soundex()
   ? "c:strtran('class', 'CLASS')", c:strtran("class", "CLASS")
   ? "c:stuff(9, 5, 'CLASS')", c:stuff(9, 5, "CLASS")
   ? "c:substr(9, 5)", c:substr(9, 5)
   ? "c:transform('@!')", c:transform("@!")
   ? "c:trim()", c:trim()
   ? "c:upper()", c:upper()
   ? "c:val()", c:val()

RETURN
