/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2022-2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022-2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_COLORADJUSTMENT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // WORD caSize
   ASSIGN caSize(n) INLINE ::setcaSize(n)
   ACCESS caSize INLINE ::getcaSize()
   METHOD setcaSize
   METHOD getcaSize

   // WORD caFlags
   ASSIGN caFlags(n) INLINE ::setcaFlags(n)
   ACCESS caFlags INLINE ::getcaFlags()
   METHOD setcaFlags
   METHOD getcaFlags

   // WORD caIlluminantIndex
   ASSIGN caIlluminantIndex(n) INLINE ::setcaIlluminantIndex(n)
   ACCESS caIlluminantIndex INLINE ::getcaIlluminantIndex()
   METHOD setcaIlluminantIndex
   METHOD getcaIlluminantIndex

   // WORD caRedGamma
   ASSIGN caRedGamma(n) INLINE ::setcaRedGamma(n)
   ACCESS caRedGamma INLINE ::getcaRedGamma()
   METHOD setcaRedGamma
   METHOD getcaRedGamma

   // WORD caGreenGamma
   ASSIGN caGreenGamma(n) INLINE ::setcaGreenGamma(n)
   ACCESS caGreenGamma INLINE ::getcaGreenGamma()
   METHOD setcaGreenGamma
   METHOD getcaGreenGamma

   // WORD caBlueGamma
   ASSIGN caBlueGamma(n) INLINE ::setcaBlueGamma(n)
   ACCESS caBlueGamma INLINE ::getcaBlueGamma()
   METHOD setcaBlueGamma
   METHOD getcaBlueGamma

   // WORD caReferenceBlack
   ASSIGN caReferenceBlack(n) INLINE ::setcaReferenceBlack(n)
   ACCESS caReferenceBlack INLINE ::getcaReferenceBlack()
   METHOD setcaReferenceBlack
   METHOD getcaReferenceBlack

   // WORD caReferenceWhite
   ASSIGN caReferenceWhite(n) INLINE ::setcaReferenceWhite(n)
   ACCESS caReferenceWhite INLINE ::getcaReferenceWhite()
   METHOD setcaReferenceWhite
   METHOD getcaReferenceWhite

   // SHORT caContrast
   ASSIGN caContrast(n) INLINE ::setcaContrast(n)
   ACCESS caContrast INLINE ::getcaContrast()
   METHOD setcaContrast
   METHOD getcaContrast

   // SHORT caBrightness
   ASSIGN caBrightness(n) INLINE ::setcaBrightness(n)
   ACCESS caBrightness INLINE ::getcaBrightness()
   METHOD setcaBrightness
   METHOD getcaBrightness

   // SHORT caColorfulness
   ASSIGN caColorfulness(n) INLINE ::setcaColorfulness(n)
   ACCESS caColorfulness INLINE ::getcaColorfulness()
   METHOD setcaColorfulness
   METHOD getcaColorfulness

   // SHORT caRedGreenTint
   ASSIGN caRedGreenTint(n) INLINE ::setcaRedGreenTint(n)
   ACCESS caRedGreenTint INLINE ::getcaRedGreenTint()
   METHOD setcaRedGreenTint
   METHOD getcaRedGreenTint

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_COLORADJUSTMENT
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new COLORADJUSTMENT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_DELETE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// WORD caSize

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCASIZE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caSize = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCASIZE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caSize);
  }
}

// WORD caFlags

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAFLAGS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caFlags = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAFLAGS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caFlags);
  }
}

// WORD caIlluminantIndex

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAILLUMINANTINDEX )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caIlluminantIndex = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAILLUMINANTINDEX )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caIlluminantIndex);
  }
}

// WORD caRedGamma

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAREDGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caRedGamma = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAREDGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caRedGamma);
  }
}

// WORD caGreenGamma

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAGREENGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caGreenGamma = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAGREENGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caGreenGamma);
  }
}

// WORD caBlueGamma

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCABLUEGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caBlueGamma = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCABLUEGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caBlueGamma);
  }
}

// WORD caReferenceBlack

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAREFERENCEBLACK )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caReferenceBlack = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAREFERENCEBLACK )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caReferenceBlack);
  }
}

// WORD caReferenceWhite

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAREFERENCEWHITE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caReferenceWhite = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAREFERENCEWHITE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caReferenceWhite);
  }
}

// SHORT caContrast

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCACONTRAST )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caContrast = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCACONTRAST )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caContrast);
  }
}

// SHORT caBrightness

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCABRIGHTNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caBrightness = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCABRIGHTNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caBrightness);
  }
}

// SHORT caColorfulness

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCACOLORFULNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caColorfulness = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCACOLORFULNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caColorfulness);
  }
}

// SHORT caRedGreenTint

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_SETCAREDGREENTINT )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->caRedGreenTint = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_COLORADJUSTMENT_GETCAREDGREENTINT )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->caRedGreenTint);
  }
}

/*
typedef struct tagCOLORADJUSTMENT {
  WORD  caSize;
  WORD  caFlags;
  WORD  caIlluminantIndex;
  WORD  caRedGamma;
  WORD  caGreenGamma;
  WORD  caBlueGamma;
  WORD  caReferenceBlack;
  WORD  caReferenceWhite;
  SHORT caContrast;
  SHORT caBrightness;
  SHORT caColorfulness;
  SHORT caRedGreenTint;
} COLORADJUSTMENT, *PCOLORADJUSTMENT, *LPCOLORADJUSTMENT;
*/

#pragma ENDDUMP
