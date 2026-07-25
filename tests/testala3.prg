// Testing parameter -ala
// hbmk2 testala3 (error)
// hbmk2 testala3 -ala (OK)

PROCEDURE Main()

   LOCAL n := ADir("*.prg")

   ? n

   LOCAL aName := array(n)
   LOCAL aSize := array(n)
   LOCAL aDate := array(n)
   LOCAL aTime := array(n)
   LOCAL aAttr := array(n)

   ADir("*.prg", aName, aSize, aDate, aTime, aAttr)

   LOCAL i

   FOR i := 1 TO n
      ? aName[i], aSize[i], aDate[i], aTime[i], aAttr[i]
   NEXT n

RETURN
