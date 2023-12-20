/*
 * WinApi test
 *
 * Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := wasSYSTEMTIME():new()

   ? "wYear........:", o:wYear
   ? "wMonth.......:", o:wMonth
   ? "wDayOfWeek...:", o:wDayOfWeek
   ? "wDay.........:", o:wDay
   ? "wHour........:", o:wHour
   ? "wMinute......:", o:wMinute
   ? "wSecond......:", o:wSecond
   ? "wMilliseconds:", o:wMilliseconds

   ?

   ? "calling waGetLocalTime"

   waGetLocalTime(o)

   ?

   ? "wYear........:", o:wYear
   ? "wMonth.......:", o:wMonth
   ? "wDayOfWeek...:", o:wDayOfWeek
   ? "wDay.........:", o:wDay
   ? "wHour........:", o:wHour
   ? "wMinute......:", o:wMinute
   ? "wSecond......:", o:wSecond
   ? "wMilliseconds:", o:wMilliseconds

RETURN
