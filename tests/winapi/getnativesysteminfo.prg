/*
 * WinApi test
 *
 * Copyright 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

PROCEDURE Main()

   LOCAL o := WINAPI_STRUCT_SYSTEM_INFO():new()

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

   ? "calling WinApi_GetNativeSystemInfo"

   WinApi_GetNativeSystemInfo(o)

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
