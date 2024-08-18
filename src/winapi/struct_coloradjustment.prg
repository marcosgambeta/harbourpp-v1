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

FUNCTION wasCOLORADJUSTMENT()
RETURN was_COLORADJUSTMENT():new()

CLASS WAS_COLORADJUSTMENT

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

PROCEDURE destroyObject() CLASS WAS_COLORADJUSTMENT
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

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new COLORADJUSTMENT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_DELETE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// WORD caSize

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCASIZE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caSize = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCASIZE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caSize);
  }
}

// WORD caFlags

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAFLAGS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caFlags = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAFLAGS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caFlags);
  }
}

// WORD caIlluminantIndex

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAILLUMINANTINDEX )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caIlluminantIndex = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAILLUMINANTINDEX )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caIlluminantIndex);
  }
}

// WORD caRedGamma

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAREDGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caRedGamma = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAREDGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caRedGamma);
  }
}

// WORD caGreenGamma

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAGREENGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caGreenGamma = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAGREENGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caGreenGamma);
  }
}

// WORD caBlueGamma

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCABLUEGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caBlueGamma = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCABLUEGAMMA )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caBlueGamma);
  }
}

// WORD caReferenceBlack

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAREFERENCEBLACK )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caReferenceBlack = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAREFERENCEBLACK )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caReferenceBlack);
  }
}

// WORD caReferenceWhite

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAREFERENCEWHITE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caReferenceWhite = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAREFERENCEWHITE )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->caReferenceWhite);
  }
}

// SHORT caContrast

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCACONTRAST )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caContrast = wa_par_SHORT(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCACONTRAST )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_SHORT(obj->caContrast);
  }
}

// SHORT caBrightness

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCABRIGHTNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caBrightness = wa_par_SHORT(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCABRIGHTNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_SHORT(obj->caBrightness);
  }
}

// SHORT caColorfulness

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCACOLORFULNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caColorfulness = wa_par_SHORT(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCACOLORFULNESS )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_SHORT(obj->caColorfulness);
  }
}

// SHORT caRedGreenTint

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_SETCAREDGREENTINT )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->caRedGreenTint = wa_par_SHORT(1);
  }
}

HB_FUNC_STATIC( WAS_COLORADJUSTMENT_GETCAREDGREENTINT )
{
  auto obj = static_cast<COLORADJUSTMENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_SHORT(obj->caRedGreenTint);
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
