/*
 * C Structure Support.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
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

#include "hbvmint.h" /* FIXME: clean the code to not access any internal HVM structures */
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbctypes.ch"

static PHB_ITEM hb_itemPutCRaw( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCRaw(%p, %s, %" HB_PFS "u)", static_cast< void * >( pItem ), szText, nLen ) );
#endif

   if( pItem )
   {
      if( HB_IS_COMPLEX(pItem) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew(nullptr);

   if( nLen == 0 )
   {
      if( szText )
         hb_xfree(const_cast<char*>(szText));
      szText = "";
   }
   pItem->type = Harbour::Item::STRING;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.value     = const_cast< char * >( szText );
   pItem->item.asString.allocated = nLen;

   return pItem;
}

#undef hb_itemPutCRawStatic
static PHB_ITEM hb_itemPutCRawStatic( PHB_ITEM pItem, const char * szText, HB_SIZE nLen )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutCRawStatic(%p, %s, %" HB_PFS "u)", static_cast< void * >( pItem ), szText, nLen ) );
#endif

   if( pItem )
   {
      if( HB_IS_COMPLEX(pItem) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew(nullptr);

   pItem->type = Harbour::Item::STRING;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.length    = nLen;
   pItem->item.asString.value     = const_cast< char * >( szText );

   return pItem;
}

#if 0

#undef hb_retclenAdoptRaw
void hb_retclenAdoptRaw( const char * szText, HB_SIZE nLen )
{
   hb_itemPutCRaw( hb_stackReturnItem(), szText, nLen );
}

#undef hb_retclenStatic
void hb_retclenStatic( const char * szText, HB_SIZE nLen )
{
   hb_itemPutCRawStatic( hb_stackReturnItem(), szText, nLen );
}

#endif

static HB_UINT SizeOfCStructure( PHB_ITEM aDef, HB_UINT uiAlign )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_UINT       uiSize = 0, uiMemberSize;
   HB_BYTE       cShift;
   HB_UINT       uiPad;

   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      if( ( pBaseDef->pItems + nIndex )->type != Harbour::Item::INTEGER )
      {
         hb_errRT_BASE( EG_ARG, 2023, nullptr, "SizeOfCStructure", 1, hb_paramError(1) );
         return 0;
      }

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            uiMemberSize = sizeof(char);
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof(char*);
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof(short);
            break;

         case CTYPE_SHORT_PTR:          /* short */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short */
            uiMemberSize = sizeof(short*);
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            uiMemberSize = sizeof(int);
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            uiMemberSize = sizeof(int*);
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            uiMemberSize = sizeof(long);
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof(long*);
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof(float);
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof(float*);
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof(double);
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof(double*);
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof(void*);
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof(void*);
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( nullptr, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease(pID);

               if( HB_IS_OBJECT(pStructure) )
               {
                  hb_objSendMsg(pStructure, "SizeOf", 0);
                  uiMemberSize = static_cast< HB_UINT >( hb_parns( -1 ) );
                  hb_itemRelease(pStructure);
               }
               else
               {
                  hb_itemRelease(pStructure);
                  hb_errRT_BASE( EG_ARG, 2023, nullptr, "SizeOfCStructure", 1, hb_paramError(1) );
                  return 0;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "SizeOfCStructure", 1, hb_paramError(1) );
               return 0;
            }
      }

      if( uiSize )
      {
         uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = static_cast< HB_BYTE >( uiSize % uiPad ) ) > 0 )
            uiSize += uiPad - cShift;
      }

      uiSize += uiMemberSize;

      #if 0
      printf( "#%" HB_PFS "u Size: %u Align: %u Pad: %u Shift %i Size: %u\n", nIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize );
      #endif
   }

   if( ( cShift = static_cast< HB_BYTE >( uiSize % uiAlign ) ) > 0 )
      uiSize += uiAlign - cShift;

   #if 0
   printf( "#%" HB_PFS "u Size: %u Align: %u Pad: %u Shift %i Size: %u\n", nIndex, uiMemberSize, uiAlign, uiPad, cShift, uiSize );
   #endif

   return uiSize;
}

HB_FUNC( HB_SIZEOFCSTRUCTURE )
{
   PHB_ITEM aDef = hb_param(1, Harbour::Item::ARRAY);

   if( aDef )
   {
      PHB_ITEM pAlign = hb_param(2, Harbour::Item::INTEGER);
      HB_UINT  uiAlign;

      if( pAlign )
         uiAlign = static_cast< HB_BYTE >( pAlign->item.asInteger.value );
      else
         uiAlign = 8;

      hb_retni(SizeOfCStructure(aDef, uiAlign));
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, nullptr, "SizeOfCStructure", 2, hb_paramError(1), hb_paramError(2) );
}

static HB_BYTE * ArrayToStructure( PHB_ITEM aVar, PHB_ITEM aDef, HB_UINT uiAlign, HB_UINT * puiSize )
{
   PHB_BASEARRAY pBaseVar = aVar->item.asArray.value;
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_BYTE *     Buffer;
   HB_UINT       uiOffset = 0, uiMemberSize;
   HB_BYTE       cShift;

   *puiSize = SizeOfCStructure( aDef, uiAlign );

   #if 0
   printf( "Size: %i\n", *puiSize );
   #endif

   Buffer = static_cast<HB_BYTE*>(hb_xgrab(*puiSize + 1));

   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      #if 0
      printf( "#: %i\n", nIndex );
      #endif

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            if( ( pBaseVar->pItems + nIndex )->type && !HB_IS_NUMERIC(pBaseVar->pItems + nIndex) )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(char);
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::STRING &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(char*);
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof(short);
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(short*);
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof(int);
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(int*);
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            /* Type check performed in actual translation... */
            uiMemberSize = sizeof(long);
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(long*);
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + nIndex )->type && ( pBaseVar->pItems + nIndex )->type != Harbour::Item::DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(float);
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::DOUBLE &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(float*);
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::DOUBLE )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(double);
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::DOUBLE &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(double*);
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            if( ( pBaseVar->pItems + nIndex )->type &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::POINTER &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::LONG &&
                ( pBaseVar->pItems + nIndex )->type != Harbour::Item::STRING )
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            uiMemberSize = sizeof(void*);
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof(void*);
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value >= CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( nullptr, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease(pID);

               if( HB_IS_OBJECT(pStructure) )
               {
                  hb_objSendMsg(pStructure, "SizeOf", 0);
                  uiMemberSize = static_cast< HB_UINT >( hb_parns( -1 ) );
                  hb_itemRelease(pStructure);
               }
               else
               {
                  hb_itemRelease(pStructure);
                  hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
                  return nullptr;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
      }

      if( uiOffset )
      {
         HB_UINT uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = static_cast< HB_BYTE >( uiOffset % uiPad ) ) > 0 )
            uiOffset += uiPad - cShift;
      }

      #if 0
      printf( "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );
      #endif

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            if( ( pBaseVar->pItems + nIndex )->type )
               *( reinterpret_cast< char * >( Buffer + uiOffset ) ) = static_cast< char >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else
               *( reinterpret_cast< char * >( Buffer + uiOffset ) ) = 0;
            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            if( ( pBaseVar->pItems + nIndex )->type )
               *(static_cast<HB_BYTE*>(Buffer + uiOffset)) = static_cast<HB_BYTE>((pBaseVar->pItems + nIndex)->item.asInteger.value);
            else
               *(static_cast<HB_BYTE*>(Buffer + uiOffset)) = 0;
            break;

         case CTYPE_CHAR_PTR:  /* char * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::STRING:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + nIndex )->item.asString.value;
                  break;

               case Harbour::Item::POINTER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = static_cast< char * >( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;
#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = reinterpret_cast< char * >( static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value ) );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = reinterpret_cast< char * >( static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value ) );
                  break;

               default:
                  *( ( char ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::STRING:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = reinterpret_cast<HB_BYTE*>((pBaseVar->pItems + nIndex)->item.asString.value);
                  break;

               case Harbour::Item::POINTER:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = static_cast<HB_BYTE*>((pBaseVar->pItems + nIndex)->item.asPointer.value);
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = reinterpret_cast<HB_BYTE*>(static_cast<HB_PTRUINT>((pBaseVar->pItems + nIndex)->item.asInteger.value));
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = reinterpret_cast<HB_BYTE*>(static_cast<HB_PTRUINT>((pBaseVar->pItems + nIndex)->item.asLong.value));
                  break;

               default:
                  *( ( HB_BYTE ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_SHORT:  /* short */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( short * ) ( Buffer + uiOffset ) ) = static_cast< short >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( short * ) ( Buffer + uiOffset ) ) = static_cast< short >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( short * ) ( Buffer + uiOffset ) ) = static_cast< short >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( short * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = static_cast< unsigned short >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = static_cast< unsigned short >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = static_cast< unsigned short >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( unsigned short * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
            break;

         case CTYPE_SHORT_PTR:  /* short * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = ( short * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( short ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_SHORT_PTR:  /* unsigned short * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = ( unsigned short * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned short ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_INT:  /* int */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( int * ) ( Buffer + uiOffset ) ) = static_cast< int >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( int * ) ( Buffer + uiOffset ) ) = static_cast< int >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( int * ) ( Buffer + uiOffset ) ) = static_cast< int >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( int * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = static_cast< unsigned int >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = static_cast< unsigned int >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = static_cast< unsigned int >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( unsigned int * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }

            break;

         case CTYPE_INT_PTR:  /* int * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = ( int * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( int ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_INT_PTR:  /* unsigned int * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = ( unsigned int * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned int ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_LONG:  /* long */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( long * ) ( Buffer + uiOffset ) ) = static_cast< long >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( long * ) ( Buffer + uiOffset ) ) = static_cast< long >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( long * ) ( Buffer + uiOffset ) ) = static_cast< long >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( long * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::INTEGER )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = static_cast< unsigned long >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::LONG )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = static_cast< unsigned long >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::DOUBLE )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = static_cast< unsigned long >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else if( ( pBaseVar->pItems + nIndex )->type == Harbour::Item::NIL )
               *( ( unsigned long * ) ( Buffer + uiOffset ) ) = 0;
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return nullptr;
            }
            break;

         case CTYPE_LONG_PTR:  /* long * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = ( long * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( long ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_UNSIGNED_LONG_PTR:  /* unsigned long * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = ( unsigned long * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               default:
                  *( ( unsigned long ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_FLOAT:  /* float */
            if( ( pBaseVar->pItems + nIndex )->type )
               *( ( float * ) ( Buffer + uiOffset ) ) = static_cast< float >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
            else
               *( ( float * ) ( Buffer + uiOffset ) ) = 0;
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = ( float * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case Harbour::Item::DOUBLE:
                  **( ( float ** ) ( Buffer + uiOffset ) ) = static_cast< float >( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( float ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_DOUBLE:  /* double */
            if( ( pBaseVar->pItems + nIndex )->type )
               *( ( double * ) ( Buffer + uiOffset ) ) = ( pBaseVar->pItems + nIndex )->item.asDouble.value;
            else
               *( ( double * ) ( Buffer + uiOffset ) ) = 0;
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) ( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = ( double * ) static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value );
                  break;

               /* Is this correct??? IMHO It's a bug */
               case Harbour::Item::DOUBLE:
                  **( ( double ** ) ( Buffer + uiOffset ) ) = ( ( pBaseVar->pItems + nIndex )->item.asDouble.value );
                  break;

               default:
                  *( ( double ** ) ( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         case CTYPE_VOID_PTR:  /* void * */
            switch( ( pBaseVar->pItems + nIndex )->type )
            {
               case Harbour::Item::POINTER:
                  *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = static_cast< void * >( ( pBaseVar->pItems + nIndex )->item.asPointer.value );
                  break;

#if UINT_MAX == ULONG_MAX
               case Harbour::Item::INTEGER:
                  *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asInteger.value ) );
                  break;
#endif
               case Harbour::Item::LONG:
                  *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( ( pBaseVar->pItems + nIndex )->item.asLong.value ) );
                  break;

               default:
                  *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = nullptr;
                  break;
            }
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pStructure = pBaseVar->pItems + nIndex;

               if( HB_IS_LONG(pStructure) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( pStructure->item.asLong.value ) );
                  else
                     memcpy( static_cast< void * >( Buffer + uiOffset ), reinterpret_cast< void * >( static_cast< HB_PTRUINT >( pStructure->item.asLong.value ) ), uiMemberSize );
               }
#if UINT_MAX == ULONG_MAX
               else if( HB_IS_INTEGER(pStructure) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( pStructure->item.asInteger.value ) );
                  else
                     memcpy( static_cast< void * >( Buffer + uiOffset ), reinterpret_cast< void * >( static_cast< HB_PTRUINT >( pStructure->item.asInteger.value ) ), uiMemberSize );
               }
#endif
               else if( HB_IS_NIL(pStructure) )
               {
                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = nullptr;
                  else
                     memset( static_cast< void * >( Buffer + uiOffset ), 0, uiMemberSize );
               }
               else if( strncmp( hb_objGetClsName( pStructure ), "C Structure", 11 ) == 0 )
               {
                  PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
                  PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;

                  hb_objSendMsg(pStructure, "VALUE", 0);

                  if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
                     *( reinterpret_cast< void ** >( Buffer + uiOffset ) ) = static_cast< void * >( pInternalBuffer->item.asString.value );
                  else
                     memcpy( static_cast< void * >( Buffer + uiOffset ), static_cast< void * >( pInternalBuffer->item.asString.value ), uiMemberSize );
               }
               else
                  hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
            }
            else
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      }

      #if 0
      printf( "Wrote %i bytes at Offset %i\n", uiMemberSize, uiOffset );
      #endif

      uiOffset += uiMemberSize;
   }

   return Buffer;
}

HB_FUNC( HB_ARRAYTOSTRUCTURE )
{
   PHB_ITEM aVar   = hb_param(1, Harbour::Item::ARRAY);
   PHB_ITEM aDef   = hb_param(2, Harbour::Item::ARRAY);
   PHB_ITEM pAlign = hb_param(3, Harbour::Item::INTEGER);

   if( aVar && aDef )
   {
      HB_UINT   uiSize;
      HB_UINT   uiAlign;
      HB_BYTE * Buffer;

      if( pAlign )
         uiAlign = static_cast< HB_BYTE >( pAlign->item.asInteger.value );
      else
         uiAlign = 8;

      Buffer = ArrayToStructure( aVar, aDef, uiAlign, &uiSize );

      hb_retclen_buffer( reinterpret_cast< char * >( Buffer ), uiSize );
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, nullptr, "ArrayToStructure", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
}

static PHB_ITEM StructureToArray( HB_BYTE * Buffer, HB_SIZE nBufferLen, PHB_ITEM aDef, HB_UINT uiAlign, HB_BOOL bAdoptNested, PHB_ITEM pRet )
{
   PHB_BASEARRAY pBaseDef = aDef->item.asArray.value;
   HB_SIZE       nLen     = pBaseDef->nLen;
   HB_SIZE       nIndex;
   HB_UINT       uiOffset, uiMemberSize;
   HB_BYTE       cShift;
#if 0
   PHB_ITEM pRet = hb_itemNew(nullptr);
#endif
   PHB_BASEARRAY pBaseVar;

   #if 0
   TraceLog( nullptr, "StructureToArray(%p, %p, %u, %i) ->%u\n", static_cast< const void * >( Buffer ), static_cast< void * >( aDef ), uiAlign, bAdoptNested, nLen );
   #endif

   #if 0
   hb_arrayNew(pRet, nLen);
   #endif
   pBaseVar = pRet->item.asArray.value;

   uiOffset = 0;
   for( nIndex = 0; nIndex < nLen; nIndex++ )
   {
      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:          /* char */
         case CTYPE_UNSIGNED_CHAR: /* unsigned char */
            uiMemberSize = sizeof(char);
            break;

         case CTYPE_CHAR_PTR:          /* char * */
         case CTYPE_UNSIGNED_CHAR_PTR: /* unsigned char * */
            uiMemberSize = sizeof(char*);
            break;

         case CTYPE_SHORT:          /* short */
         case CTYPE_UNSIGNED_SHORT: /* unsigned short */
            uiMemberSize = sizeof(short);
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            uiMemberSize = sizeof(short*);
            break;

         case CTYPE_INT:          /* int */
         case CTYPE_UNSIGNED_INT: /* unsigned int */
            uiMemberSize = sizeof(int);
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            uiMemberSize = sizeof(int*);
            break;

         case CTYPE_LONG:          /* long */
         case CTYPE_UNSIGNED_LONG: /* unsigned long */
            uiMemberSize = sizeof(long);
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            uiMemberSize = sizeof(long*);
            break;

         case CTYPE_FLOAT:  /* float */
            uiMemberSize = sizeof(float);
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            uiMemberSize = sizeof(float*);
            break;

         case CTYPE_DOUBLE:  /* double */
            uiMemberSize = sizeof(double);
            break;

         case CTYPE_DOUBLE_PTR:  /* double * */
            uiMemberSize = sizeof(double*);
            break;

         case CTYPE_VOID_PTR:  /* void * (pointer) */
            uiMemberSize = sizeof(void*);
            break;

         default:
            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               uiMemberSize = sizeof(void*);
            }
            else if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE )
            {
               PHB_ITEM pID        = hb_itemPutNI( nullptr, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
               PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

               hb_itemRelease(pID);

               if( HB_IS_OBJECT(pStructure) )
               {
                  hb_objSendMsg(pStructure, "SizeOf", 0);
                  uiMemberSize = static_cast< HB_UINT >( hb_parns( -1 ) );
                  hb_itemRelease(pStructure);
               }
               else
               {
                  hb_itemRelease(pStructure);
                  hb_errRT_BASE( EG_ARG, 2023, nullptr, "StructureToArray", 1, hb_paramError(1) );
                  return pRet;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "StructureToArray", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
               return pRet;
            }
      }

      if( uiOffset )
      {
         HB_UINT uiPad = ( ( uiMemberSize < uiAlign ) ? uiMemberSize : uiAlign );

         if( ( cShift = static_cast< HB_BYTE >( uiOffset % uiPad ) ) > 0 )
            uiOffset += uiPad - cShift;

         #if 0
         TraceLog( nullptr, "* Size: %i Offset: %i Pad: %i\n", uiMemberSize, uiOffset, uiPad );
         #endif
      }
      else
      {
         #if 0
         TraceLog( nullptr, "* Size: %i Offset: %i\n", uiMemberSize, uiOffset );
         #endif
      }

      if( ( uiOffset + uiMemberSize ) > nBufferLen )
         break;

      switch( ( pBaseDef->pItems + nIndex )->item.asInteger.value )
      {
         case CTYPE_CHAR:  /* char */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *( reinterpret_cast< char * >( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_CHAR:  /* unsigned char */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *(static_cast<HB_BYTE*>(Buffer + uiOffset)) );
            break;

         case CTYPE_CHAR_PTR:  /* char * */
            if( HB_IS_STRING(pBaseVar->pItems + nIndex) && ( pBaseVar->pItems + nIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               #if 0
               TraceLog( nullptr, "IDENTICAL: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
            }
            else if( !bAdoptNested )
            {
               #if 0
               TraceLog( nullptr, "Static: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
               hb_itemPutCConst( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            else
            {
               #if 0
               TraceLog( nullptr, "Adopt: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
               hb_itemPutC( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            break;

         case CTYPE_UNSIGNED_CHAR_PTR:  /* unsigned char * */
            if( HB_IS_STRING(pBaseVar->pItems + nIndex) && ( pBaseVar->pItems + nIndex )->item.asString.value == *( ( char ** ) ( Buffer + uiOffset ) ) )
            {
               #if 0
               TraceLog( nullptr, "IDENTICAL: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
            }
            else if( !bAdoptNested )
            {
               #if 0
               TraceLog( nullptr, "Static: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
               hb_itemPutCConst( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            else
            {
               #if 0
               TraceLog( nullptr, "Adopt: %s\n", *( ( char ** ) ( Buffer + uiOffset ) ) );
               #endif
               hb_itemPutC( pBaseVar->pItems + nIndex, *( ( char ** ) ( Buffer + uiOffset ) ) );
            }
            break;

         case CTYPE_SHORT:  /* short */
            hb_itemPutNI( pBaseVar->pItems + nIndex, *( ( short * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_SHORT:  /* unsigned short */
            hb_itemPutNI( pBaseVar->pItems + nIndex, static_cast< short >( *( ( unsigned short * ) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_SHORT_PTR:          /* short * */
         case CTYPE_UNSIGNED_SHORT_PTR: /* unsigned short * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, static_cast< void * >( Buffer + uiOffset ) );
            break;

         case CTYPE_INT:  /* int */
            hb_itemPutNI( pBaseVar->pItems + nIndex, *( ( int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_INT:  /* unsigned int */
            hb_itemPutNI( pBaseVar->pItems + nIndex, ( int ) *( ( unsigned int * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_INT_PTR:          /* int * */
         case CTYPE_UNSIGNED_INT_PTR: /* unsigned int * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, static_cast< void * >( Buffer + uiOffset ) );
            break;

         case CTYPE_LONG:  /* long */
            hb_itemPutNL( pBaseVar->pItems + nIndex, *( ( long * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_UNSIGNED_LONG:  /* unsigned long */
            hb_itemPutNL( pBaseVar->pItems + nIndex, static_cast< long >( *( ( unsigned long * ) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_LONG_PTR:          /* long * */
         case CTYPE_UNSIGNED_LONG_PTR: /* unsigned long * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, static_cast< void * >( Buffer + uiOffset ) );
            break;

         case CTYPE_FLOAT:  /* float */
            hb_itemPutND( pBaseVar->pItems + nIndex, static_cast< double >( *( ( float * ) ( Buffer + uiOffset ) ) ) );
            break;

         case CTYPE_FLOAT_PTR:  /* float * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, static_cast< void * >( Buffer + uiOffset ) );
            break;

         case CTYPE_DOUBLE:  /* double */
            hb_itemPutND( pBaseVar->pItems + nIndex, *( ( double * ) ( Buffer + uiOffset ) ) );
            break;

         case CTYPE_DOUBLE_PTR: /* double * */
         case CTYPE_VOID_PTR:   /* void * */
            hb_itemPutPtr( pBaseVar->pItems + nIndex, static_cast< void * >( Buffer + uiOffset ) );
            break;

         default:
         {
            HB_UINT  uiNestedSize /*, uiNestedAlign */;
            PHB_ITEM pID        = hb_itemPutNI( nullptr, ( pBaseDef->pItems + nIndex )->item.asInteger.value );
            PHB_ITEM pStructure = hb_itemDoC( "HB_CSTRUCTUREFROMID", 1, pID );

            hb_itemRelease(pID);

            if( !HB_IS_OBJECT(pStructure) )
            {
               hb_itemRelease(pStructure);
               hb_errRT_BASE( EG_ARG, 2023, nullptr, "StructureToArray", 2, hb_paramError(1), hb_paramError(2) );
               return pRet;
            }

            hb_objSendMsg(pStructure, "NALIGN", 0);
            hb_objSendMsg(pStructure, "SizeOf", 0);
            uiNestedSize = static_cast< HB_UINT >( hb_parns( -1 ) );

            #if 0
            TraceLog( nullptr, "* NestedSize: %i Offset: %i\n", uiNestedSize, uiOffset );
            #endif

            if( ( pBaseDef->pItems + nIndex )->item.asInteger.value > CTYPE_STRUCTURE_PTR )
            {
               #if 0
               printf( "Offset %i Pointer: %p\n", uiOffset, *( char ** ) ( (long ** )( Buffer + uiOffset ) ) );
               #endif

               if( *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ) )
               {
                  PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
                  PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;

                  if( !bAdoptNested )
                     hb_itemPutCRawStatic( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );
                  else
                     hb_itemPutCRaw( pInternalBuffer, *( char ** ) ( ( long ** ) ( Buffer + uiOffset ) ), uiNestedSize );

                  hb_objSendMsg(pStructure, "DEVALUE", 0);
               }
               else
               {
#if 0
                  hb_objSendMsg(pStructure, "RESET", 0);
#endif
                  hb_itemClear( pStructure );
               }
            }
            else
            {
               PHB_BASEARRAY pBaseStructure  = pStructure->item.asArray.value;
               PHB_ITEM      pInternalBuffer = pBaseStructure->pItems + pBaseStructure->nLen - 1;
               HB_ITEM       Adopt;

               Adopt.type = Harbour::Item::LOGICAL;
               Adopt.item.asLogical.value = bAdoptNested;

               #if 0
               TraceLog( nullptr, "Before Devalue\n" );
               #endif

               hb_itemPutCRawStatic( pInternalBuffer, reinterpret_cast< char * >( static_cast< HB_BYTE * >( Buffer + uiOffset ) ), uiNestedSize );

               hb_objSendMsg(pStructure, "DEVALUE", 1, &Adopt);

               #if 0
               TraceLog( nullptr, "After Devalue\n" );
               #endif
            }

            hb_itemMove(pBaseVar->pItems + nIndex, pStructure);

            hb_itemRelease(pStructure);
         }
      }

      uiOffset += uiMemberSize;

      #if 0
      TraceLog( nullptr, "AFTER Size: %i Offset: %i\n", uiMemberSize, uiOffset );
      #endif
   }

   return pRet;
}

HB_FUNC( HB_STRUCTURETOARRAY )
{
   PHB_ITEM Structure = hb_param(1, Harbour::Item::STRING);
   PHB_ITEM aDef      = hb_param(2, Harbour::Item::ARRAY);
   PHB_ITEM pAlign    = hb_param(3, Harbour::Item::INTEGER);
   PHB_ITEM pAdopt    = hb_param(4, Harbour::Item::LOGICAL);
   PHB_ITEM pRet      = hb_param(5, Harbour::Item::ARRAY);

   if( Structure && aDef )
   {
      HB_BYTE * Buffer = reinterpret_cast<HB_BYTE*>(Structure->item.asString.value);
      HB_UINT   uiAlign;
      HB_BOOL   bAdopt;

      if( pAlign )
         uiAlign = static_cast< HB_BYTE >( pAlign->item.asInteger.value );
      else
         uiAlign = 8;

      if( pAdopt )
         bAdopt = pAdopt->item.asLogical.value;
      else
         bAdopt = HB_FALSE;

      hb_itemReturnForward( StructureToArray( Buffer, Structure->item.asString.length, aDef, uiAlign, bAdopt, pRet ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2023, nullptr, "StructureToArray", 2, hb_paramError(1), hb_paramError(2) );
}

HB_FUNC( HB_POINTER2STRING )
{
   PHB_ITEM pPointer = hb_param(1, Harbour::Item::ANY);
   PHB_ITEM pLen     = hb_param(2, Harbour::Item::NUMERIC);

   if( HB_IS_POINTER(pPointer) && pLen )
      hb_retclen(static_cast<char*>(hb_itemGetPtr(pPointer)), hb_itemGetNS(pLen));
   else if( HB_IS_INTEGER(pPointer) && pLen )
      hb_retclen(reinterpret_cast<char*>(static_cast<HB_PTRUINT>(hb_itemGetNI(pPointer))), hb_itemGetNS(pLen));
   else if( HB_IS_LONG(pPointer) && pLen )
      hb_retclen(reinterpret_cast<char*>(static_cast<HB_PTRUINT>(hb_itemGetNL(pPointer))), hb_itemGetNS(pLen));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, HB_ERR_FUNCNAME, 2, hb_paramError(1), hb_paramError(2));
}

HB_FUNC( HB_STRING2POINTER )
{
   const char * pszString = hb_parc(1);

   if( pszString )
      hb_retptr(const_cast<char*>(pszString));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, HB_ERR_FUNCNAME, 1, hb_paramError(1));
}

HB_FUNC( __CSTR_COPYTO )
{
   static PHB_DYNS s_pVALUE = nullptr;
   PHB_ITEM        pTarget  = hb_param(1, Harbour::Item::ANY);
   PHB_ITEM        pStructure;
   void *          pPointer;

   if( s_pVALUE == nullptr )
      s_pVALUE = hb_dynsymGetCase( "VALUE" );

   if( HB_IS_LONG(pTarget) )
      pPointer = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( hb_itemGetNInt( pTarget ) ) );
#if UINT_MAX == ULONG_MAX
   else if( HB_IS_INTEGER(pTarget) )
      pPointer = reinterpret_cast< void * >( static_cast< HB_PTRUINT >( hb_itemGetNInt( pTarget ) ) );
#endif
   else if( HB_IS_POINTER(pTarget) )
      pPointer = hb_itemGetPtr( pTarget );
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 1099, nullptr, "C Structure:CopyTo()", 1, hb_paramError(1));
      return;
   }

   pStructure = hb_stackSelfItem();
   hb_vmPushDynSym(s_pVALUE);
   hb_vmPush(pStructure);
   hb_vmSend(0);

   memcpy( pPointer, hb_parc(-1), hb_arrayGetNI( pStructure, hb_arrayLen(pStructure) - 2 ) );
}
