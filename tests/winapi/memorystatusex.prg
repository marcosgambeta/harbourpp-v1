/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := wasMEMORYSTATUSEX()

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

   ? "calling waGlobalMemoryStatusEx"

   ? waGlobalMemoryStatusEx(o)

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
   
   WAIT

RETURN
