//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

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

// NOTE: source code generated with the help of a code generator

#include "hbclass.ch"

FUNCTION wasSECURITY_ATTRIBUTES()
RETURN was_SECURITY_ATTRIBUTES():new()

CLASS WAS_SECURITY_ATTRIBUTES

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD nLength
   //ASSIGN nLength(n) INLINE ::setnLength(n)
   ACCESS nLength INLINE ::getnLength()
   //METHOD setnLength
   METHOD getnLength

   // LPVOID lpSecurityDescriptor
   ASSIGN lpSecurityDescriptor(n) INLINE ::setlpSecurityDescriptor(n)
   ACCESS lpSecurityDescriptor INLINE ::getlpSecurityDescriptor()
   METHOD setlpSecurityDescriptor
   METHOD getlpSecurityDescriptor

   // BOOL bInheritHandle
   ASSIGN bInheritHandle(n) INLINE ::setbInheritHandle(n)
   ACCESS bInheritHandle INLINE ::getbInheritHandle()
   METHOD setbInheritHandle
   METHOD getbInheritHandle

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_SECURITY_ATTRIBUTES
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

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_NEW )
{
  auto obj = new SECURITY_ATTRIBUTES();
  obj->nLength = sizeof(SECURITY_ATTRIBUTES);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_DELETE )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD nLength

// HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_SETNLENGTH )
// {
//   auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->nLength = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_GETNLENGTH )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->nLength);
  }
}

// LPVOID lpSecurityDescriptor

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_SETLPSECURITYDESCRIPTOR )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lpSecurityDescriptor = wa_par_LPVOID(1); // TODO: SecurityDescriptor is a structure
  }
}

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_GETLPSECURITYDESCRIPTOR )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LPVOID(obj->lpSecurityDescriptor); // TODO: SecurityDescriptor is a structure
  }
}

// BOOL bInheritHandle

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_SETBINHERITHANDLE )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bInheritHandle = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WAS_SECURITY_ATTRIBUTES_GETBINHERITHANDLE )
{
  auto obj = static_cast<SECURITY_ATTRIBUTES*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->bInheritHandle);
  }
}

/*
typedef struct _SECURITY_ATTRIBUTES {
  DWORD  nLength;
  LPVOID lpSecurityDescriptor;
  BOOL   bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;
*/

#pragma ENDDUMP
