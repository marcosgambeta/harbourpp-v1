/*
 * WinApi test
 *
 * Copyright 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := WINAPI_STRUCT_SYSTEMTIME():new()

   ? "wYear........:", o:wYear
   ? "wMonth.......:", o:wMonth
   ? "wDayOfWeek...:", o:wDayOfWeek
   ? "wDay.........:", o:wDay
   ? "wHour........:", o:wHour
   ? "wMinute......:", o:wMinute
   ? "wSecond......:", o:wSecond
   ? "wMilliseconds:", o:wMilliseconds

   ?

   ? "calling WinApi_GetSystemTime"

   WinApi_GetSystemTime(o)
   
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
