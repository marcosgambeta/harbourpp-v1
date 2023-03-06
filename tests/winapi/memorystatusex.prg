/*
 * WinApi test
 *
 * Copyright 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := WINAPI_STRUCT_MEMORYSTATUSEX():new()

   ? "dwLength...............:", o:dwLength
   ? "dwMemoryLoad...........:", o:dwMemoryLoad
   ? "ullTotalPhys...........:", o:ullTotalPhys
   ? "ullAvailPhys...........:", o:ullAvailPhys
   ? "ullTotalPageFile.......:", o:ullTotalPageFile
   ? "ullAvailPageFile.......:", o:ullAvailPageFile
   ? "ullTotalVirtual........:", o:ullTotalVirtual
   ? "ullAvailVirtual........:", o:ullAvailVirtual
   ? "ullAvailExtendedVirtual:", o:ullAvailExtendedVirtual

   ?

   ? "calling WinApi_GlobalMemoryStatusEx"

   ? WinApi_GlobalMemoryStatusEx(o)

   ?

   ? "dwLength...............:", o:dwLength
   ? "dwMemoryLoad...........:", o:dwMemoryLoad
   ? "ullTotalPhys...........:", o:ullTotalPhys
   ? "ullAvailPhys...........:", o:ullAvailPhys
   ? "ullTotalPageFile.......:", o:ullTotalPageFile
   ? "ullAvailPageFile.......:", o:ullAvailPageFile
   ? "ullTotalVirtual........:", o:ullTotalVirtual
   ? "ullAvailVirtual........:", o:ullAvailVirtual
   ? "ullAvailExtendedVirtual:", o:ullAvailExtendedVirtual

RETURN
