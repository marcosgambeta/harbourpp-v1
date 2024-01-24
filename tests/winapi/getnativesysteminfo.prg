/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := wasSYSTEM_INFO()

   ? "dwOemId....................:", o:dwOemId
   ? "wProcessorArchitecture.....:", o:wProcessorArchitecture
   ? "wReserved..................:", o:wReserved
   ? "dwPageSize.................:", o:dwPageSize
   ? "lpMinimumApplicationAddress:", o:lpMinimumApplicationAddress
   ? "lpMaximumApplicationAddress:", o:lpMaximumApplicationAddress
   ? "dwActiveProcessorMask......:", o:dwActiveProcessorMask
   ? "dwNumberOfProcessors.......:", o:dwNumberOfProcessors
   ? "dwProcessorType............:", o:dwProcessorType
   ? "dwAllocationGranularity....:", o:dwAllocationGranularity
   ? "wProcessorLevel............:", o:wProcessorLevel
   ? "wProcessorRevision.........:", o:wProcessorRevision

   ?

   ? "calling waGetNativeSystemInfo"

   waGetNativeSystemInfo(o)

   ?

   ? "dwOemId....................:", o:dwOemId
   ? "wProcessorArchitecture.....:", o:wProcessorArchitecture
   ? "wReserved..................:", o:wReserved
   ? "dwPageSize.................:", o:dwPageSize
   ? "lpMinimumApplicationAddress:", o:lpMinimumApplicationAddress
   ? "lpMaximumApplicationAddress:", o:lpMaximumApplicationAddress
   ? "dwActiveProcessorMask......:", o:dwActiveProcessorMask
   ? "dwNumberOfProcessors.......:", o:dwNumberOfProcessors
   ? "dwProcessorType............:", o:dwProcessorType
   ? "dwAllocationGranularity....:", o:dwAllocationGranularity
   ? "wProcessorLevel............:", o:wProcessorLevel
   ? "wProcessorRevision.........:", o:wProcessorRevision

RETURN
