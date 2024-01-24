/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := wasSYSTEMTIME()

   ? "wYear........:", o:wYear
   ? "wMonth.......:", o:wMonth
   ? "wDayOfWeek...:", o:wDayOfWeek
   ? "wDay.........:", o:wDay
   ? "wHour........:", o:wHour
   ? "wMinute......:", o:wMinute
   ? "wSecond......:", o:wSecond
   ? "wMilliseconds:", o:wMilliseconds

   ?

   ? "calling waGetSystemTime"

   waGetSystemTime(o)

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
