//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include "hbclass.ch"

FUNCTION wasSYSTEM_INFO()
RETURN was_SYSTEM_INFO():new()

CLASS WAS_SYSTEM_INFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD dwOemId
   ASSIGN dwOemId(n) INLINE ::setdwOemId(n)
   ACCESS dwOemId INLINE ::getdwOemId()
   METHOD setdwOemId
   METHOD getdwOemId

   // WORD wProcessorArchitecture
   ASSIGN wProcessorArchitecture(n) INLINE ::setwProcessorArchitecture(n)
   ACCESS wProcessorArchitecture INLINE ::getwProcessorArchitecture()
   METHOD setwProcessorArchitecture
   METHOD getwProcessorArchitecture

   // WORD wReserved
   ASSIGN wReserved(n) INLINE ::setwReserved(n)
   ACCESS wReserved INLINE ::getwReserved()
   METHOD setwReserved
   METHOD getwReserved

   // DWORD dwPageSize
   ASSIGN dwPageSize(n) INLINE ::setdwPageSize(n)
   ACCESS dwPageSize INLINE ::getdwPageSize()
   METHOD setdwPageSize
   METHOD getdwPageSize

   // LPVOID lpMinimumApplicationAddress
   ASSIGN lpMinimumApplicationAddress(n) INLINE ::setlpMinimumApplicationAddress(n)
   ACCESS lpMinimumApplicationAddress INLINE ::getlpMinimumApplicationAddress()
   METHOD setlpMinimumApplicationAddress
   METHOD getlpMinimumApplicationAddress

   // LPVOID lpMaximumApplicationAddress
   ASSIGN lpMaximumApplicationAddress(n) INLINE ::setlpMaximumApplicationAddress(n)
   ACCESS lpMaximumApplicationAddress INLINE ::getlpMaximumApplicationAddress()
   METHOD setlpMaximumApplicationAddress
   METHOD getlpMaximumApplicationAddress

   // DWORD_PTR dwActiveProcessorMask
   ASSIGN dwActiveProcessorMask(n) INLINE ::setdwActiveProcessorMask(n)
   ACCESS dwActiveProcessorMask INLINE ::getdwActiveProcessorMask()
   METHOD setdwActiveProcessorMask
   METHOD getdwActiveProcessorMask

   // DWORD dwNumberOfProcessors
   ASSIGN dwNumberOfProcessors(n) INLINE ::setdwNumberOfProcessors(n)
   ACCESS dwNumberOfProcessors INLINE ::getdwNumberOfProcessors()
   METHOD setdwNumberOfProcessors
   METHOD getdwNumberOfProcessors

   // DWORD dwProcessorType
   ASSIGN dwProcessorType(n) INLINE ::setdwProcessorType(n)
   ACCESS dwProcessorType INLINE ::getdwProcessorType()
   METHOD setdwProcessorType
   METHOD getdwProcessorType

   // DWORD dwAllocationGranularity
   ASSIGN dwAllocationGranularity(n) INLINE ::setdwAllocationGranularity(n)
   ACCESS dwAllocationGranularity INLINE ::getdwAllocationGranularity()
   METHOD setdwAllocationGranularity
   METHOD getdwAllocationGranularity

   // WORD wProcessorLevel
   ASSIGN wProcessorLevel(n) INLINE ::setwProcessorLevel(n)
   ACCESS wProcessorLevel INLINE ::getwProcessorLevel()
   METHOD setwProcessorLevel
   METHOD getwProcessorLevel

   // WORD wProcessorRevision
   ASSIGN wProcessorRevision(n) INLINE ::setwProcessorRevision(n)
   ACCESS wProcessorRevision INLINE ::getwProcessorRevision()
   METHOD setwProcessorRevision
   METHOD getwProcessorRevision

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_SYSTEM_INFO
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

HB_FUNC_STATIC(WAS_SYSTEM_INFO_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new SYSTEM_INFO());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_DELETE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD dwOemId

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWOEMID)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwOemId = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWOEMID)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwOemId);
  }
}

// WORD wProcessorArchitecture

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETWPROCESSORARCHITECTURE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wProcessorArchitecture = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETWPROCESSORARCHITECTURE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wProcessorArchitecture);
  }
}

// WORD wReserved

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETWRESERVED)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wReserved = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETWRESERVED)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wReserved);
  }
}

// DWORD dwPageSize

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWPAGESIZE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwPageSize = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWPAGESIZE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwPageSize);
  }
}

// LPVOID lpMinimumApplicationAddress

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETLPMINIMUMAPPLICATIONADDRESS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lpMinimumApplicationAddress = wa_par_LPVOID(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETLPMINIMUMAPPLICATIONADDRESS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LPVOID(obj->lpMinimumApplicationAddress);
  }
}

// LPVOID lpMaximumApplicationAddress

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETLPMAXIMUMAPPLICATIONADDRESS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lpMaximumApplicationAddress = wa_par_LPVOID(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETLPMAXIMUMAPPLICATIONADDRESS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LPVOID(obj->lpMaximumApplicationAddress);
  }
}

// DWORD_PTR dwActiveProcessorMask

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWACTIVEPROCESSORMASK)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwActiveProcessorMask = wa_par_DWORD_PTR(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWACTIVEPROCESSORMASK)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD_PTR(obj->dwActiveProcessorMask);
  }
}

// DWORD dwNumberOfProcessors

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWNUMBEROFPROCESSORS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwNumberOfProcessors = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWNUMBEROFPROCESSORS)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwNumberOfProcessors);
  }
}

// DWORD dwProcessorType

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWPROCESSORTYPE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwProcessorType = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWPROCESSORTYPE)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwProcessorType);
  }
}

// DWORD dwAllocationGranularity

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETDWALLOCATIONGRANULARITY)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwAllocationGranularity = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETDWALLOCATIONGRANULARITY)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwAllocationGranularity);
  }
}

// WORD wProcessorLevel

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETWPROCESSORLEVEL)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wProcessorLevel = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETWPROCESSORLEVEL)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wProcessorLevel);
  }
}

// WORD wProcessorRevision

HB_FUNC_STATIC(WAS_SYSTEM_INFO_SETWPROCESSORREVISION)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wProcessorRevision = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_SYSTEM_INFO_GETWPROCESSORREVISION)
{
  auto obj = static_cast<SYSTEM_INFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wProcessorRevision);
  }
}

/*
typedef struct _SYSTEM_INFO {
  union {
    DWORD dwOemId;
    struct {
      WORD wProcessorArchitecture;
      WORD wReserved;
    } DUMMYSTRUCTNAME;
  } DUMMYUNIONNAME;
  DWORD     dwPageSize;
  LPVOID    lpMinimumApplicationAddress;
  LPVOID    lpMaximumApplicationAddress;
  DWORD_PTR dwActiveProcessorMask;
  DWORD     dwNumberOfProcessors;
  DWORD     dwProcessorType;
  DWORD     dwAllocationGranularity;
  WORD      wProcessorLevel;
  WORD      wProcessorRevision;
} SYSTEM_INFO, *LPSYSTEM_INFO;
*/

#pragma ENDDUMP
