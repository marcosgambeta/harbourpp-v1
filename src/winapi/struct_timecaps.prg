/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_TIMECAPS

   DATA pointer
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT wPeriodMin
   ASSIGN wPeriodMin(n) INLINE ::setwPeriodMin(n)
   ACCESS wPeriodMin INLINE ::getwPeriodMin()
   METHOD setwPeriodMin
   METHOD getwPeriodMin

   // UINT wPeriodMax
   ASSIGN wPeriodMax(n) INLINE ::setwPeriodMax(n)
   ACCESS wPeriodMax INLINE ::getwPeriodMax()
   METHOD setwPeriodMax
   METHOD getwPeriodMax

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_TIMECAPS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_NEW )
{
  auto obj = new TIMECAPS();
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_pointer", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_DELETE )
{
  auto obj = static_cast<TIMECAPS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "POINTER", 0)));

  if( obj != nullptr )
  {
    delete obj;
    obj = nullptr;
    PHB_ITEM self = hb_stackSelfItem();
    PHB_ITEM ptr = hb_itemPutPtr( nullptr, nullptr );
    hb_objSendMsg( self, "_pointer", 1, ptr );
    hb_itemRelease( ptr );
  }

  hb_itemReturn( hb_stackSelfItem() );
}

// UINT wPeriodMin

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_SETWPERIODMIN )
{
  auto obj = static_cast<TIMECAPS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "POINTER", 0)));

  if( obj != nullptr )
  {
    obj->wPeriodMin = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_GETWPERIODMIN )
{
  auto obj = static_cast<TIMECAPS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "POINTER", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wPeriodMin);
  }
}

// UINT wPeriodMax

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_SETWPERIODMAX )
{
  auto obj = static_cast<TIMECAPS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "POINTER", 0)));

  if( obj != nullptr )
  {
    obj->wPeriodMax = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_TIMECAPS_GETWPERIODMAX )
{
  auto obj = static_cast<TIMECAPS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "POINTER", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wPeriodMax);
  }
}

/*
typedef struct timecaps_tag {
  UINT wPeriodMin;
  UINT wPeriodMax;
} TIMECAPS, *PTIMECAPS, *NPTIMECAPS, *LPTIMECAPS;
*/

#pragma ENDDUMP
