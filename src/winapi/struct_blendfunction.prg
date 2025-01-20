//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
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

FUNCTION wasBLENDFUNCTION()
RETURN was_BLENDFUNCTION():new()

CLASS WAS_BLENDFUNCTION

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // BYTE BlendOp
   ASSIGN BlendOp(n) INLINE ::setBlendOp(n)
   ACCESS BlendOp INLINE ::getBlendOp()
   METHOD setBlendOp
   METHOD getBlendOp

   // BYTE BlendFlags
   ASSIGN BlendFlags(n) INLINE ::setBlendFlags(n)
   ACCESS BlendFlags INLINE ::getBlendFlags()
   METHOD setBlendFlags
   METHOD getBlendFlags

   // BYTE SourceConstantAlpha
   ASSIGN SourceConstantAlpha(n) INLINE ::setSourceConstantAlpha(n)
   ACCESS SourceConstantAlpha INLINE ::getSourceConstantAlpha()
   METHOD setSourceConstantAlpha
   METHOD getSourceConstantAlpha

   // BYTE AlphaFormat
   ASSIGN AlphaFormat(n) INLINE ::setAlphaFormat(n)
   ACCESS AlphaFormat INLINE ::getAlphaFormat()
   METHOD setAlphaFormat
   METHOD getAlphaFormat

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_BLENDFUNCTION
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

HB_FUNC_STATIC(WAS_BLENDFUNCTION_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new BLENDFUNCTION());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_BLENDFUNCTION_DELETE)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// BYTE BlendOp

HB_FUNC_STATIC(WAS_BLENDFUNCTION_SETBLENDOP)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->BlendOp = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_BLENDFUNCTION_GETBLENDOP)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->BlendOp);
  }
}

// BYTE BlendFlags

HB_FUNC_STATIC(WAS_BLENDFUNCTION_SETBLENDFLAGS)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->BlendFlags = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_BLENDFUNCTION_GETBLENDFLAGS)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->BlendFlags);
  }
}

// BYTE SourceConstantAlpha

HB_FUNC_STATIC(WAS_BLENDFUNCTION_SETSOURCECONSTANTALPHA)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->SourceConstantAlpha = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_BLENDFUNCTION_GETSOURCECONSTANTALPHA)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->SourceConstantAlpha);
  }
}

// BYTE AlphaFormat

HB_FUNC_STATIC(WAS_BLENDFUNCTION_SETALPHAFORMAT)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->AlphaFormat = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_BLENDFUNCTION_GETALPHAFORMAT)
{
  auto obj = static_cast<BLENDFUNCTION *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->AlphaFormat);
  }
}

/*
typedef struct _BLENDFUNCTION {
  BYTE BlendOp;
  BYTE BlendFlags;
  BYTE SourceConstantAlpha;
  BYTE AlphaFormat;
} BLENDFUNCTION, *PBLENDFUNCTION;*/

#pragma ENDDUMP
