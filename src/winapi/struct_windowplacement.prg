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

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

FUNCTION wasWINDOWPLACEMENT()
RETURN was_WINDOWPLACEMENT():new()

CLASS WAS_WINDOWPLACEMENT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT length
   //ASSIGN length(n) INLINE ::setlength(n)
   ACCESS length INLINE ::getlength()
   //METHOD setlength
   METHOD getlength

   // UINT flags
   ASSIGN flags(n) INLINE ::setflags(n)
   ACCESS flags INLINE ::getflags()
   METHOD setflags
   METHOD getflags

   // UINT showCmd
   ASSIGN showCmd(n) INLINE ::setshowCmd(n)
   ACCESS showCmd INLINE ::getshowCmd()
   METHOD setshowCmd
   METHOD getshowCmd

   // POINT ptMinPosition

   // POINT ptMaxPosition

   // RECT rcNormalPosition

   // RECT rcDevice

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_WINDOWPLACEMENT
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

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_NEW )
{
  auto obj = new WINDOWPLACEMENT();
  obj->length = sizeof(WINDOWPLACEMENT);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_DELETE )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT length

// HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_SETLENGTH )
// {
//   auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->length = wa_par_UINT(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_GETLENGTH )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->length);
  }
}

// UINT flags

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_SETFLAGS )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->flags = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_GETFLAGS )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->flags);
  }
}

// UINT showCmd

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_SETSHOWCMD )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->showCmd = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPLACEMENT_GETSHOWCMD )
{
  auto obj = static_cast<WINDOWPLACEMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->showCmd);
  }
}

// POINT ptMinPosition

// POINT ptMaxPosition

// RECT rcNormalPosition

// RECT rcDevice

/*
typedef struct tagWINDOWPLACEMENT {
  UINT  length;
  UINT  flags;
  UINT  showCmd;
  POINT ptMinPosition;
  POINT ptMaxPosition;
  RECT  rcNormalPosition;
  RECT  rcDevice;
} WINDOWPLACEMENT;
*/

#pragma ENDDUMP
