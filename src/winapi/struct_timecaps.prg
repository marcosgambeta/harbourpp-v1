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

// clang-format off

#include "hbclass.ch"

FUNCTION wasTIMECAPS()
RETURN was_TIMECAPS():new()

CLASS WAS_TIMECAPS

   DATA ptr
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

PROCEDURE destroyObject() CLASS WAS_TIMECAPS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

// clang-format on

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC(WAS_TIMECAPS_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new TIMECAPS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_TIMECAPS_DELETE)
{
  auto obj = static_cast<TIMECAPS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT wPeriodMin

HB_FUNC_STATIC(WAS_TIMECAPS_SETWPERIODMIN)
{
  auto obj = static_cast<TIMECAPS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->wPeriodMin = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_TIMECAPS_GETWPERIODMIN)
{
  auto obj = static_cast<TIMECAPS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_UINT(obj->wPeriodMin);
  }
}

// UINT wPeriodMax

HB_FUNC_STATIC(WAS_TIMECAPS_SETWPERIODMAX)
{
  auto obj = static_cast<TIMECAPS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->wPeriodMax = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_TIMECAPS_GETWPERIODMAX)
{
  auto obj = static_cast<TIMECAPS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_UINT(obj->wPeriodMax);
  }
}

/*
typedef struct timecaps_tag {
  UINT wPeriodMin;
  UINT wPeriodMax;
} TIMECAPS, *PTIMECAPS, *NPTIMECAPS, *LPTIMECAPS;
*/

#pragma ENDDUMP
