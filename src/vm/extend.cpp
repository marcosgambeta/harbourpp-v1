/*
 * The Extend API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * Copyright 1999-2009 Viktor Szakats (vszakats.net/harbour) (hb_stor(), hb_retn*len(), hb_retdl(), hb_parn*def())
 * Copyright 2000 Jose Lalin <dezac@corevia.com> (hb_retd())
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbvmopt.h"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbset.h"
#include "hbdate.h"
#include "hbstack.h"

/* NOTE: iParam = -1 can be used to access the return value. */
/* NOTE: iParam = 0 can be used to access the SELF object. */

PHB_ITEM hb_param(int iParam, long lMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_param(%d, %ld)", iParam, lMask));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( pItem->type & Harbour::Item::BYREF )
      {
         pItem = hb_itemUnRef(pItem);
         if( static_cast<HB_TYPE>(lMask) == Harbour::Item::BYREF )
         {
            return pItem;
         }
      }

      if( (pItem->type & static_cast<HB_TYPE>(lMask)) || static_cast<HB_TYPE>(lMask) == Harbour::Item::ANY )
      {
         return pItem;
      }
   }

   return nullptr;
}

PHB_ITEM hb_paramError(int iParam)
{
   static HB_ITEM s_NIL;

   PHB_ITEM pParam = hb_param(iParam, Harbour::Item::ANY);

   if( pParam == nullptr )
   {
      hb_itemClear(&s_NIL);
      pParam = &s_NIL;
   }

   return pParam;
}

HB_ULONG hb_parinfo(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parinfo(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == 0 )
   {
      return static_cast<HB_ULONG>(hb_pcount());
   }
   else
   {
      if( iParam >= -1 && iParam <= hb_pcount() )
      {
         PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
         HB_TYPE uiType = HB_ITEM_TYPE(pItem);

         if( uiType & Harbour::Item::BYREF )
         {
            uiType |= HB_ITEM_TYPE(hb_itemUnRef(pItem));
         }

         return static_cast<HB_ULONG>(uiType);
      }
      else
      {
         return 0;
      }
   }
}

HB_SIZE hb_parinfa(int iParamNum, HB_SIZE nArrayIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parinfa(%d, %" HB_PFS "u)", iParamNum, nArrayIndex));
#endif

   PHB_ITEM pArray;

   pArray = hb_param(iParamNum, Harbour::Item::ARRAY);

   if( pArray )
   {
      if( nArrayIndex == 0 )
      {
         return hb_arrayLen(pArray);
      }
      else
      {
         return static_cast<HB_ISIZ>(hb_arrayGetType(pArray, nArrayIndex));
      }
   }
   else
   {
      return 0;
   }
}

HB_BOOL hb_extIsNil(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_extIsNil(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
   {
      pItem = hb_stackReturnItem();
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      pItem = hb_stackItemFromBase(iParam);
   }
   else
   {
      return true;
   }

   if( HB_IS_BYREF(pItem) )
   {
      pItem = hb_itemUnRef(pItem);
   }

   return HB_IS_NIL(pItem);
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an array item */

HB_BOOL hb_extIsArray(int iParam)
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
   {
      pItem = hb_stackReturnItem();
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      pItem = hb_stackItemFromBase(iParam);
   }
   else
   {
      return false;
   }

   if( HB_IS_BYREF(pItem) )
   {
      pItem = hb_itemUnRef(pItem);
   }

   return HB_IS_ARRAY(pItem) && !HB_ARRAY_OBJ(pItem);
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an object item */

HB_BOOL hb_extIsObject(int iParam)
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
   {
      pItem = hb_stackReturnItem();
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      pItem = hb_stackItemFromBase(iParam);
   }
   else
   {
      return false;
   }

   if( HB_IS_BYREF(pItem) )
   {
      pItem = hb_itemUnRef(pItem);
   }

   return HB_IS_OBJECT(pItem);
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * hb_parc(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parc(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.value;
      }
   }

   return nullptr;
}

const char * hb_parcx(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parcx(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.value;
      }
   }

   return "";
}

HB_SIZE hb_parclen(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parclen(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.length;
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

HB_SIZE hb_parcsiz(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parcsiz(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      /* NOTE: hb_parcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);

         if( HB_IS_STRING(pItem) )
         {
            return pItem->item.asString.length + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using hb_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * hb_pards(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_pards(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_dateDecStr(hb_stackDateBuffer(), pItem->item.asDateTime.julian);
      }
   }

   return hb_dateDecStr(hb_stackDateBuffer(), 0);
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char * hb_pardsbuff(char * szDate, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_pardsbuff(%p, %d)", static_cast<void*>(szDate), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_dateDecStr(szDate, pItem->item.asDateTime.julian);
      }
   }

   return hb_dateDecStr(szDate, 0);
}

/* retrieve a date as long integer - number of days from Julian's day */

long hb_pardl(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_pardl(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return pItem->item.asDateTime.julian;
      }
   }

   return hb_itemGetDL(nullptr);
}

double hb_partd(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_partd(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_timeStampPackDT(pItem->item.asDateTime.julian, pItem->item.asDateTime.time);
      }
   }

   return 0;
}

HB_BOOL hb_partdt(long * plJulian, long * plMilliSec, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_partdt(%p,%p,%d)", static_cast<void*>(plJulian), static_cast<void*>(plMilliSec), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return true;
      }
   }

   return false;
}

int  hb_parl(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parl(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LOGICAL(pItem) )
      {
         return pItem->item.asLogical.value ? 1 : 0;
      }
   }

   return 0;
}

int  hb_parldef(int iParam, int iDefValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parldef(%d,%d)", iParam, iDefValue));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LOGICAL(pItem) )
      {
         return pItem->item.asLogical.value ? 1 : 0;
      }
   }

   return iDefValue;
}

double  hb_parnd(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnd(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DOUBLE(pItem) )
      {
         return pItem->item.asDouble.value;
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<double>(pItem->item.asInteger.value);
      }
      else if( HB_IS_LONG(pItem) )
      {
         return static_cast<double>(pItem->item.asLong.value);
      }
   }

   return 0;
}

int  hb_parni(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parni(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_INTEGER(pItem) )
      {
         return pItem->item.asInteger.value;
      }
      else if( HB_IS_LONG(pItem) )
      {
         return static_cast<int>(pItem->item.asLong.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_INT(pItem->item.asDouble.value);
      }
   }

   return 0;
}

int  hb_parnidef(int iParam, int iDefValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parni(%d, %d)", iParam, iDefValue));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_INTEGER(pItem) )
      {
         return pItem->item.asInteger.value;
      }
      else if( HB_IS_LONG(pItem) )
      {
         return static_cast<int>(pItem->item.asLong.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_INT(pItem->item.asDouble.value);
      }
   }

   return iDefValue;
}

long  hb_parnl(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnl(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<long>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<long>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_LONG(pItem->item.asDouble.value);
      }
   }

   return 0;
}

long  hb_parnldef(int iParam, long lDefValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnldef(%d, %ld)", iParam, lDefValue));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<long>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<long>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_LONG(pItem->item.asDouble.value);
      }
   }

   return lDefValue;
}

HB_ISIZ hb_parns(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parns(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_ISIZ(pItem->item.asDouble.value);
      }
   }

   return 0;
}

HB_ISIZ hb_parnsdef(int iParam, HB_ISIZ nDefValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnsdef(%d, %" HB_PFS "d)", iParam, nDefValue));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_ISIZ(pItem->item.asDouble.value);
      }
   }

   return nDefValue;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG  hb_parnll(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnll(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_LONGLONG>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_LONGLONG>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_LONGLONG(pItem->item.asDouble.value);
      }
   }

   return 0;
}
#endif

HB_MAXINT hb_parnint(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnint(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_MAXINT(pItem->item.asDouble.value);
      }
   }

   return 0;
}

HB_MAXINT hb_parnintdef(int iParam, HB_MAXINT nDefValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parnintdef(%d, %" PFHL "d)", iParam, nDefValue));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_MAXINT(pItem->item.asDouble.value);
      }
   }

   return nDefValue;
}

void * hb_parptr(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parptr(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_POINTER(pItem) )
      {
         return pItem->item.asPointer.value;
      }
   }

   return nullptr;
}

void * hb_parptrGC(const HB_GC_FUNCS * pFuncs, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parptrGC(%p,%d)", static_cast<const void*>(pFuncs), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_POINTER(pItem) && pItem->item.asPointer.collect && hb_gcFuncs(pItem->item.asPointer.value) == pFuncs )
      {
         return pItem->item.asPointer.value;
      }
   }

   return nullptr;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * hb_parvc(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvc(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.value;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         pItem = hb_arrayGetItemPtr(pItem, nArrayIndex);
         return pItem && HB_IS_STRING(pItem) ? hb_itemGetCPtr(pItem) : nullptr;
      }
   }

   return nullptr;
}

const char * hb_parvcx(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvcx(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.value;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetCPtr(pItem, nArrayIndex);
      }
   }

   return "";
}

HB_SIZE hb_parvclen(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvclen(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_STRING(pItem) )
      {
         return pItem->item.asString.length;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetCLen(pItem, nArrayIndex);
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

HB_SIZE hb_parvcsiz(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvcsiz(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      /* NOTE: hb_parvcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);

         if( HB_IS_STRING(pItem) )
         {
            return pItem->item.asString.length + 1;
         }
         else if( HB_IS_ARRAY(pItem) )
         {
            va_list va;
            HB_SIZE nArrayIndex;

            va_start(va, iParam);
            nArrayIndex = va_arg(va, HB_SIZE);
            va_end(va);

            return hb_arrayGetCLen(pItem, nArrayIndex) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using hb_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * hb_parvds(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvds(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_dateDecStr(hb_stackDateBuffer(), pItem->item.asDateTime.julian);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetDS(pItem, nArrayIndex, hb_stackDateBuffer());
      }
   }

   return hb_dateDecStr(hb_stackDateBuffer(), 0);
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char  * hb_parvdsbuff(char * szDate, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvdsbuff(%p, %d, ...)", static_cast<void*>(szDate), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_dateDecStr(szDate, pItem->item.asDateTime.julian);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetDS(pItem, nArrayIndex, szDate);
      }
   }

   return hb_dateDecStr(szDate, 0);
}

/* retrieve a date as long integer - number of days from Julian's day */

long hb_parvdl(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvdl(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return pItem->item.asDateTime.julian;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetDL(pItem, nArrayIndex);
      }
   }

   return hb_itemGetDL(nullptr);
}

double hb_parvtd(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvtd(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         return hb_timeStampPackDT(pItem->item.asDateTime.julian, pItem->item.asDateTime.time);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetTD(pItem, nArrayIndex);
      }
   }

   return 0;
}

HB_BOOL hb_parvtdt(long * plJulian, long * plMilliSec, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvtdt(%p,%p,%d, ...)", static_cast<void*>(plJulian), static_cast<void*>(plMilliSec), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DATETIME(pItem) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return true;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetTDT(pItem, nArrayIndex, plJulian, plMilliSec);
      }
   }

   return false;
}

int  hb_parvl(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvl(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LOGICAL(pItem) )
      {
         return pItem->item.asLogical.value ? 1 : 0;
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return pItem->item.asInteger.value != 0 ? 1 : 0;
      }
      else if( HB_IS_LONG(pItem) )
      {
         return pItem->item.asLong.value != 0 ? 1 : 0;
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return pItem->item.asDouble.value != 0.0 ? 1 : 0;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetL(pItem, nArrayIndex) ? 1 : 0;
      }
   }

   return 0;
}

double  hb_parvnd(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvnd(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_DOUBLE(pItem) )
      {
         return pItem->item.asDouble.value;
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<double>(pItem->item.asInteger.value);
      }
      else if( HB_IS_LONG(pItem) )
      {
         return static_cast<double>(pItem->item.asLong.value);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetND(pItem, nArrayIndex);
      }
   }

   return 0;
}

int  hb_parvni(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvni(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_INTEGER(pItem) )
      {
         return pItem->item.asInteger.value;
      }
      else if( HB_IS_LONG(pItem) )
      {
         return static_cast<int>(pItem->item.asLong.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_INT(pItem->item.asDouble.value);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetNI(pItem, nArrayIndex);
      }
   }

   return 0;
}

long  hb_parvnl(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvnl(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<long>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<long>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_LONG(pItem->item.asDouble.value);
      }
      /* CA-Cl*pper does it */
      else if( HB_IS_DATETIME(pItem) )
      {
         return static_cast<long>(pItem->item.asDateTime.julian);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetNL(pItem, nArrayIndex);
      }
   }

   return 0;
}

HB_ISIZ hb_parvns(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvns(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_ISIZ>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_ISIZ(pItem->item.asDouble.value);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetNS(pItem, nArrayIndex);
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_parvnll(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvnll(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_LONGLONG>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_LONGLONG>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_LONGLONG(pItem->item.asDouble.value);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetNLL(pItem, nArrayIndex);
      }
   }

   return 0;
}
#endif

HB_MAXINT hb_parvnint(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvnint(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_LONG(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asLong.value);
      }
      else if( HB_IS_INTEGER(pItem) )
      {
         return static_cast<HB_MAXINT>(pItem->item.asInteger.value);
      }
      else if( HB_IS_DOUBLE(pItem) )
      {
         return HB_CAST_MAXINT(pItem->item.asDouble.value);
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetNInt(pItem, nArrayIndex);
      }
   }

   return 0;
}

void * hb_parvptr(int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvptr(%d, ...)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_POINTER(pItem) )
      {
         return pItem->item.asPointer.value;
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         return hb_arrayGetPtr(pItem, nArrayIndex);
      }
   }

   return nullptr;
}

void * hb_parvptrGC(const HB_GC_FUNCS * pFuncs, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parvptrGC(%p,%d, ...)", static_cast<const void*>(pFuncs), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_POINTER(pItem) )
      {
         if( pItem->item.asPointer.collect && hb_gcFuncs(pItem->item.asPointer.value) == pFuncs )
         {
            return pItem->item.asPointer.value;
         }
      }
      else if( HB_IS_ARRAY(pItem) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start(va, iParam);
         nArrayIndex = va_arg(va, HB_SIZE);
         va_end(va);

         pItem = hb_arrayGetItemPtr(pItem, nArrayIndex);
         if( pItem && HB_IS_POINTER(pItem) && pItem->item.asPointer.collect && hb_gcFuncs(pItem->item.asPointer.value) == pFuncs )
         {
            return pItem->item.asPointer.value;
         }
      }
   }

   return nullptr;
}

#undef hb_ret
void hb_ret(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ret()"));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemClear(hb_stackReturnItem());
}

#undef hb_reta
void hb_reta(HB_SIZE nLen)  /* undocumented hb_reta() */
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_reta(%" HB_PFS "u)", nLen));
#endif

   HB_STACK_TLS_PRELOAD

   hb_arrayNew(hb_stackReturnItem(), nLen);
}

#undef hb_retc
void hb_retc(const char * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retc(%s)", szText));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutC(hb_stackReturnItem(), szText);
}

#undef hb_retc_null
void hb_retc_null(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retc_null()"));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutC(hb_stackReturnItem(), nullptr);
}

#undef hb_retc_buffer
void hb_retc_buffer(char * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retc_buffer(%s)", szText));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutCPtr(hb_stackReturnItem(), szText);
}

#undef hb_retc_const
void hb_retc_const(const char * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retc_const(%s)", szText));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutCConst(hb_stackReturnItem(), szText);
}

#undef hb_retclen
void hb_retclen(const char * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retclen(%.*s, %" HB_PFS "u)", static_cast<int>(nLen), szText, nLen));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutCL(hb_stackReturnItem(), szText, nLen);
}

#undef hb_retclen_buffer
void hb_retclen_buffer(char * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retclen_buffer(%.*s, %" HB_PFS "u)", static_cast<int>(nLen), szText, nLen));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutCLPtr(hb_stackReturnItem(), szText, nLen);
}

#undef hb_retclen_const
void hb_retclen_const(const char * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retclen_const(%.*s, %" HB_PFS "u)", static_cast<int>(nLen), szText, nLen));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutCLConst(hb_stackReturnItem(), szText, nLen);
}

/* szDate must have YYYYMMDD format */

#undef hb_retds
void hb_retds(const char * szDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retds(%s)", szDate));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutDS(hb_stackReturnItem(), szDate);
}

#undef hb_retd
void hb_retd(int iYear, int iMonth, int iDay)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retd(%04i, %02i, %02i)", iYear, iMonth, iDay));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutD(hb_stackReturnItem(), iYear, iMonth, iDay);
}

#undef hb_retdl
void hb_retdl(long lJulian)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retdl(%ld)", lJulian));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutDL(hb_stackReturnItem(), lJulian);
}

#undef hb_rettd
void hb_rettd(double dTimeStamp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rettd(%lf)", dTimeStamp));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutTD(hb_stackReturnItem(), dTimeStamp);
}

#undef hb_rettdt
void hb_rettdt(long lJulian, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_rettdt(%ld, %ld)", lJulian, lMilliSec));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutTDT(hb_stackReturnItem(), lJulian, lMilliSec);
}

#undef hb_retl
void hb_retl(int iLogical)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retl(%d)", iLogical));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutL(hb_stackReturnItem(), iLogical ? true : false);
}

#undef hb_retnd
void hb_retnd(double dNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnd(%lf)", dNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutND(hb_stackReturnItem(), dNumber);
}

#undef hb_retni
void hb_retni(int iNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retni(%d)", iNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNI(hb_stackReturnItem(), iNumber);
}

#undef hb_retnl
void hb_retnl(long lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnl(%ld)", lNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNL(hb_stackReturnItem(), lNumber);
}

#undef hb_retns
void hb_retns(HB_ISIZ nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retns(%" HB_PFS "d )", nNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNS(hb_stackReturnItem(), nNumber);
}

#ifndef HB_LONG_LONG_OFF
#undef hb_retnll
void hb_retnll(HB_LONGLONG llNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnll(%" PFLL "d)", llNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNLL(hb_stackReturnItem(), llNumber);
}
#endif

#undef hb_retnint
void hb_retnint(HB_MAXINT nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnl(%" PFHL "d )", nNumber));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNInt(hb_stackReturnItem(), nNumber);
}

#undef hb_retnlen
void hb_retnlen(double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnlen(%lf, %d, %d)", dNumber, iWidth, iDec));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNLen(hb_stackReturnItem(), dNumber, iWidth, iDec);
}

#undef hb_retndlen
void hb_retndlen(double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retndlen(%lf, %d, %d)", dNumber, iWidth, iDec));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNDLen(hb_stackReturnItem(), dNumber, iWidth, iDec);
}

#undef hb_retnilen
void hb_retnilen(int iNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnilen(%d, %d)", iNumber, iWidth));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNILen(hb_stackReturnItem(), iNumber, iWidth);
}

#undef hb_retnllen
void hb_retnllen(long lNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnllen(%ld, %d)", lNumber, iWidth));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNLLen(hb_stackReturnItem(), lNumber, iWidth);
}

#ifndef HB_LONG_LONG_OFF
#undef hb_retnlllen
void hb_retnlllen(HB_LONGLONG llNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnlllen(%" PFLL "d, %d)", llNumber, iWidth));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNLLLen(hb_stackReturnItem(), llNumber, iWidth);
}
#endif

#undef hb_retnintlen
void hb_retnintlen(HB_MAXINT nNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retnintlen(%" PFHL "d, %d)", nNumber, iWidth));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutNIntLen(hb_stackReturnItem(), nNumber, iWidth);
}

#undef hb_retptr
void hb_retptr(void * pointer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retptr(%p)", pointer));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutPtr(hb_stackReturnItem(), pointer);
}

#undef hb_retptrGC
void hb_retptrGC(void * pointer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retptrGC(%p)", pointer));
#endif

   HB_STACK_TLS_PRELOAD

   hb_itemPutPtrGC(hb_stackReturnItem(), pointer);
}

int hb_stor(int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stor(%d)", iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemClear(hb_stackReturnItem());
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemClear(hb_itemUnRef(pItem));
         return 1;
      }
   }

   return 0;
}

int hb_storc(const char * szText, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storc(%s, %d)", szText, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutC(hb_stackReturnItem(), szText);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutC(hb_itemUnRef(pItem), szText);
         return 1;
      }
   }

   return 0;
}

int hb_storclen(const char * szText, HB_SIZE nLen, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storclen(%.*s, %" HB_PFS "u, %d)", static_cast<int>(nLen), szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutCL(hb_stackReturnItem(), szText, nLen);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutCL(hb_itemUnRef(pItem), szText, nLen);
         return 1;
      }
   }

   return 0;
}

int hb_storclen_buffer(char * szText, HB_SIZE nLen, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storclen_buffer(%.*s, %" HB_PFS "u, %d)", static_cast<int>(nLen), szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutCLPtr(hb_stackReturnItem(), szText, nLen);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutCLPtr(hb_itemUnRef(pItem), szText, nLen);
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int hb_stords(const char * szDate, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stords(%s, %d)", szDate, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutDS(hb_stackReturnItem(), szDate);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutDS(hb_itemUnRef(pItem), szDate);
         return 1;
      }
   }

   return 0;
}

int hb_stordl(long lJulian, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stordl(%ld, %d)", lJulian, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutDL(hb_stackReturnItem(), lJulian);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutDL(hb_itemUnRef(pItem), lJulian);
         return 1;
      }
   }

   return 0;
}

int hb_stortd(double dTimeStamp, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%lf, %d)", dTimeStamp, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutTD(hb_stackReturnItem(), dTimeStamp);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutTD(hb_itemUnRef(pItem), dTimeStamp);
         return 1;
      }
   }

   return 0;
}

int hb_stortdt(long lJulian, long lMilliSec, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%ld, %ld, %d)", lJulian, lMilliSec, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutTDT(hb_stackReturnItem(), lJulian, lMilliSec);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutTDT(hb_itemUnRef(pItem), lJulian, lMilliSec);
         return 1;
      }
   }

   return 0;
}

int hb_storl(int iLogical, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storl(%d, %d)", iLogical, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutL(hb_stackReturnItem(), iLogical ? true : false);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutL(hb_itemUnRef(pItem), iLogical ? true : false);
         return 1;
      }
   }

   return 0;
}

int hb_storni(int iValue, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storni(%d, %d)", iValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutNI(hb_stackReturnItem(), iValue);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutNI(hb_itemUnRef(pItem), iValue);
         return 1;
      }
   }

   return 0;
}

int hb_stornl(long lValue, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stornl(%ld, %d)", lValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutNL(hb_stackReturnItem(), lValue);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutNL(hb_itemUnRef(pItem), lValue);
         return 1;
      }
   }

   return 0;
}

int hb_storns(HB_ISIZ nValue, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storns(%" HB_PFS "d, %d)", nValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutNS(hb_stackReturnItem(), nValue);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutNS(hb_itemUnRef(pItem), nValue);
         return 1;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
int hb_stornll(HB_LONGLONG llValue, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stornll(%" PFLL "d, %d)", llValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutNLL(hb_stackReturnItem(), llValue);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutNLL(hb_itemUnRef(pItem), llValue);
         return 1;
      }
   }

   return 0;
}
#endif

int hb_stornint(HB_MAXINT nValue, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stornint(%" PFHL "d, %d)", nValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutNInt(hb_stackReturnItem(), nValue);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutNInt(hb_itemUnRef(pItem), nValue);
         return 1;
      }
   }

   return 0;
}

int hb_stornd(double dNumber, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stornd(%lf, %d)", dNumber, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutND(hb_stackReturnItem(), dNumber);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutND(hb_itemUnRef(pItem), dNumber);
         return 1;
      }
   }

   return 0;
}

int hb_storptr(void * pointer, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storptr(%p, %d)", pointer, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutPtr(hb_stackReturnItem(), pointer);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutPtr(hb_itemUnRef(pItem), pointer);
         return 1;
      }
   }

   return 0;
}

int hb_storptrGC(void * pointer, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storptrGC(%p, %d)", pointer, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 )
   {
      hb_itemPutPtrGC(hb_stackReturnItem(), pointer);
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) )
      {
         hb_itemPutPtrGC(hb_itemUnRef(pItem), pointer);
         return 1;
      }
   }

   return 0;
}

/* hb_storv*() similar to hb_stor*() but they accepts optional array index
 * just like Cl*pper's _stor*() functions
 */

int hb_storvc(const char * szText, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvc(%s, %d, ...)", szText, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetC(pItem, va_arg(va, HB_SIZE), szText) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutC(pItem, szText);
         return 1;
      }
   }

   return 0;
}

int hb_storvclen(const char * szText, HB_SIZE nLen, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvclen(%.*s, %" HB_PFS "u, %d, ...)", static_cast<int>(nLen), szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetCL(pItem, va_arg(va, HB_SIZE), szText, nLen) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCL(pItem, szText, nLen);
         return 1;
      }
   }

   return 0;
}

int hb_storvclen_buffer(char * szText, HB_SIZE nLen, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvclen_buffer(%.*s, %" HB_PFS "u, %d, ...)", static_cast<int>(nLen), szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetCLPtr(pItem, va_arg(va, HB_SIZE), szText, nLen) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCLPtr(pItem, szText, nLen);
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int hb_storvds(const char * szDate, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvds(%s, %d, ...)", szDate, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetDS(pItem, va_arg(va, HB_SIZE), szDate) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutDS(pItem, szDate);
         return 1;
      }
   }

   return 0;
}

int hb_storvdl(long lJulian, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvdl(%ld, %d, ...)", lJulian, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetDL(pItem, va_arg(va, HB_SIZE), lJulian) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutDL(pItem, lJulian);
         return 1;
      }
   }

   return 0;
}

int hb_storvtd(double dTimeStamp, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvtd(%lf, %d, ...)", dTimeStamp, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetTD(pItem, va_arg(va, HB_SIZE), dTimeStamp) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutTD(pItem, dTimeStamp);
         return 1;
      }
   }

   return 0;
}

int hb_storvtdt(long lJulian, long lMilliSec, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvtd(%ld, %ld, %d, ...)", lJulian, lMilliSec, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetTDT(pItem, va_arg(va, HB_SIZE), lJulian, lMilliSec) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutTDT(pItem, lJulian, lMilliSec);
         return 1;
      }
   }

   return 0;
}

int hb_storvl(int iLogical, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvl(%d, %d, ...)", iLogical, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetL(pItem, va_arg(va, HB_SIZE), iLogical ? true : false) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutL(pItem, iLogical ? true : false);
         return 1;
      }
   }

   return 0;
}

int hb_storvni(int iValue, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvni(%d, %d, ...)", iValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetNI(pItem, va_arg(va, HB_SIZE), iValue) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNI(pItem, iValue);
         return 1;
      }
   }

   return 0;
}

int hb_storvnl(long lValue, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvnl(%ld, %d, ...)", lValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetNL(pItem, va_arg(va, HB_SIZE), lValue) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNL(pItem, lValue);
         return 1;
      }
   }

   return 0;
}

int hb_storvns(HB_ISIZ nValue, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvns(%" HB_PFS "d, %d, ...)", nValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetNS(pItem, va_arg(va, HB_SIZE), nValue) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNS(pItem, nValue);
         return 1;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
int hb_storvnll(HB_LONGLONG llValue, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvnll(%" PFLL "d, %d, ...)", llValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetNLL(pItem, va_arg(va, HB_SIZE), llValue) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNLL(pItem, llValue);
         return 1;
      }
   }

   return 0;
}
#endif

int hb_storvnint(HB_MAXINT nValue, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvnint(%" PFHL "d, %d, ...)", nValue, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetNInt(pItem, va_arg(va, HB_SIZE), nValue) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNInt(pItem, nValue);
         return 1;
      }
   }

   return 0;
}

int hb_storvnd(double dNumber, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvnd(%lf, %d, ...)", dNumber, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetND(pItem, va_arg(va, HB_SIZE), dNumber) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutND(pItem, dNumber);
         return 1;
      }
   }

   return 0;
}

int hb_storvptr(void * pointer, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvptr(%p, %d, ...)", pointer, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetPtr(pItem, va_arg(va, HB_SIZE), pointer) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutPtr(pItem, pointer);
         return 1;
      }
   }

   return 0;
}

int hb_storvptrGC(void * pointer, int iParam, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storvptrGC(%p, %d, ...)", pointer, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);
      HB_BOOL bByRef = HB_IS_BYREF(pItem);

      if( bByRef )
      {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) )
      {
         int iRetVal;
         va_list va;
         va_start(va, iParam);
         iRetVal = hb_arraySetPtrGC(pItem, va_arg(va, HB_SIZE), pointer) ? 1 : 0;
         va_end(va);
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutPtrGC(pItem, pointer);
         return 1;
      }
   }

   return 0;
}

#undef hb_pcount
int hb_pcount(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_pcount()"));
#endif

   HB_STACK_TLS_PRELOAD

   return static_cast<int>((hb_stackBaseItem())->item.asSymbol.paramcnt);
}
