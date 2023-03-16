/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

CLASS WINAPI_STRUCT_MEMORYSTATUSEX

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD dwLength
#if 0
   ASSIGN dwLength(n) INLINE ::setdwLength(n)
#endif
   ACCESS dwLength INLINE ::getdwLength()
#if 0
   METHOD setdwLength
#endif
   METHOD getdwLength

   // DWORD dwMemoryLoad
   ASSIGN dwMemoryLoad(n) INLINE ::setdwMemoryLoad(n)
   ACCESS dwMemoryLoad INLINE ::getdwMemoryLoad()
   METHOD setdwMemoryLoad
   METHOD getdwMemoryLoad

   // DWORDLONG ullTotalPhys
   ASSIGN ullTotalPhys(n) INLINE ::setullTotalPhys(n)
   ACCESS ullTotalPhys INLINE ::getullTotalPhys()
   METHOD setullTotalPhys
   METHOD getullTotalPhys

   // DWORDLONG ullAvailPhys
   ASSIGN ullAvailPhys(n) INLINE ::setullAvailPhys(n)
   ACCESS ullAvailPhys INLINE ::getullAvailPhys()
   METHOD setullAvailPhys
   METHOD getullAvailPhys

   // DWORDLONG ullTotalPageFile
   ASSIGN ullTotalPageFile(n) INLINE ::setullTotalPageFile(n)
   ACCESS ullTotalPageFile INLINE ::getullTotalPageFile()
   METHOD setullTotalPageFile
   METHOD getullTotalPageFile

   // DWORDLONG ullAvailPageFile
   ASSIGN ullAvailPageFile(n) INLINE ::setullAvailPageFile(n)
   ACCESS ullAvailPageFile INLINE ::getullAvailPageFile()
   METHOD setullAvailPageFile
   METHOD getullAvailPageFile

   // DWORDLONG ullTotalVirtual
   ASSIGN ullTotalVirtual(n) INLINE ::setullTotalVirtual(n)
   ACCESS ullTotalVirtual INLINE ::getullTotalVirtual()
   METHOD setullTotalVirtual
   METHOD getullTotalVirtual

   // DWORDLONG ullAvailVirtual
   ASSIGN ullAvailVirtual(n) INLINE ::setullAvailVirtual(n)
   ACCESS ullAvailVirtual INLINE ::getullAvailVirtual()
   METHOD setullAvailVirtual
   METHOD getullAvailVirtual

   // DWORDLONG ullAvailExtendedVirtual
   ASSIGN ullAvailExtendedVirtual(n) INLINE ::setullAvailExtendedVirtual(n)
   ACCESS ullAvailExtendedVirtual INLINE ::getullAvailExtendedVirtual()
   METHOD setullAvailExtendedVirtual
   METHOD getullAvailExtendedVirtual

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_MEMORYSTATUSEX
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_NEW )
{
  auto obj = new MEMORYSTATUSEX();
  obj->dwLength = sizeof(MEMORYSTATUSEX);
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_DELETE )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD dwLength

#if 0
HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETDWLENGTH )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwLength = winapi_par_DWORD(1);
  }
}
#endif

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETDWLENGTH )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwLength);
  }
}

// DWORD dwMemoryLoad

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETDWMEMORYLOAD )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwMemoryLoad = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETDWMEMORYLOAD )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwMemoryLoad);
  }
}

// DWORDLONG ullTotalPhys

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLTOTALPHYS )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullTotalPhys = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLTOTALPHYS )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullTotalPhys);
  }
}

// DWORDLONG ullAvailPhys

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLAVAILPHYS )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullAvailPhys = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLAVAILPHYS )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullAvailPhys);
  }
}

// DWORDLONG ullTotalPageFile

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLTOTALPAGEFILE )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullTotalPageFile = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLTOTALPAGEFILE )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullTotalPageFile);
  }
}

// DWORDLONG ullAvailPageFile

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLAVAILPAGEFILE )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullAvailPageFile = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLAVAILPAGEFILE )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullAvailPageFile);
  }
}

// DWORDLONG ullTotalVirtual

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLTOTALVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullTotalVirtual = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLTOTALVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullTotalVirtual);
  }
}

// DWORDLONG ullAvailVirtual

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLAVAILVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullAvailVirtual = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLAVAILVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullAvailVirtual);
  }
}

// DWORDLONG ullAvailExtendedVirtual

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_SETULLAVAILEXTENDEDVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ullAvailExtendedVirtual = winapi_par_DWORDLONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MEMORYSTATUSEX_GETULLAVAILEXTENDEDVIRTUAL )
{
  auto obj = static_cast<MEMORYSTATUSEX*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORDLONG(obj->ullAvailExtendedVirtual);
  }
}

/*
typedef struct _MEMORYSTATUSEX {
  DWORD     dwLength;
  DWORD     dwMemoryLoad;
  DWORDLONG ullTotalPhys;
  DWORDLONG ullAvailPhys;
  DWORDLONG ullTotalPageFile;
  DWORDLONG ullAvailPageFile;
  DWORDLONG ullTotalVirtual;
  DWORDLONG ullAvailVirtual;
  DWORDLONG ullAvailExtendedVirtual;
} MEMORYSTATUSEX, *LPMEMORYSTATUSEX;
*/

#pragma ENDDUMP
