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

CLASS WINAPI_STRUCT_SYSTEMTIME

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // WORD wYear
   ASSIGN wYear(n) INLINE ::setwYear(n)
   ACCESS wYear INLINE ::getwYear()
   METHOD setwYear
   METHOD getwYear

   // WORD wMonth
   ASSIGN wMonth(n) INLINE ::setwMonth(n)
   ACCESS wMonth INLINE ::getwMonth()
   METHOD setwMonth
   METHOD getwMonth

   // WORD wDayOfWeek
   ASSIGN wDayOfWeek(n) INLINE ::setwDayOfWeek(n)
   ACCESS wDayOfWeek INLINE ::getwDayOfWeek()
   METHOD setwDayOfWeek
   METHOD getwDayOfWeek

   // WORD wDay
   ASSIGN wDay(n) INLINE ::setwDay(n)
   ACCESS wDay INLINE ::getwDay()
   METHOD setwDay
   METHOD getwDay

   // WORD wHour
   ASSIGN wHour(n) INLINE ::setwHour(n)
   ACCESS wHour INLINE ::getwHour()
   METHOD setwHour
   METHOD getwHour

   // WORD wMinute
   ASSIGN wMinute(n) INLINE ::setwMinute(n)
   ACCESS wMinute INLINE ::getwMinute()
   METHOD setwMinute
   METHOD getwMinute

   // WORD wSecond
   ASSIGN wSecond(n) INLINE ::setwSecond(n)
   ACCESS wSecond INLINE ::getwSecond()
   METHOD setwSecond
   METHOD getwSecond

   // WORD wMilliseconds
   ASSIGN wMilliseconds(n) INLINE ::setwMilliseconds(n)
   ACCESS wMilliseconds INLINE ::getwMilliseconds()
   METHOD setwMilliseconds
   METHOD getwMilliseconds

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_SYSTEMTIME
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_NEW )
{
  auto obj = new SYSTEMTIME();
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_ptr", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_DELETE )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    obj = nullptr;
    PHB_ITEM self = hb_stackSelfItem();
    PHB_ITEM ptr = hb_itemPutPtr( nullptr, nullptr );
    hb_objSendMsg( self, "_ptr", 1, ptr );
    hb_itemRelease( ptr );
  }

  hb_itemReturn( hb_stackSelfItem() );
}

// WORD wYear

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWYEAR )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wYear = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWYEAR )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wYear);
  }
}

// WORD wMonth

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWMONTH )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wMonth = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWMONTH )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wMonth);
  }
}

// WORD wDayOfWeek

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWDAYOFWEEK )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wDayOfWeek = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWDAYOFWEEK )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wDayOfWeek);
  }
}

// WORD wDay

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWDAY )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wDay = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWDAY )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wDay);
  }
}

// WORD wHour

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWHOUR )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wHour = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWHOUR )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wHour);
  }
}

// WORD wMinute

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWMINUTE )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wMinute = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWMINUTE )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wMinute);
  }
}

// WORD wSecond

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWSECOND )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wSecond = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWSECOND )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wSecond);
  }
}

// WORD wMilliseconds

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_SETWMILLISECONDS )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wMilliseconds = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SYSTEMTIME_GETWMILLISECONDS )
{
  auto obj = static_cast<SYSTEMTIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->wMilliseconds);
  }
}

/*
typedef struct _SYSTEMTIME {
  WORD wYear;
  WORD wMonth;
  WORD wDayOfWeek;
  WORD wDay;
  WORD wHour;
  WORD wMinute;
  WORD wSecond;
  WORD wMilliseconds;
} SYSTEMTIME, *PSYSTEMTIME, *LPSYSTEMTIME;
*/

#pragma ENDDUMP