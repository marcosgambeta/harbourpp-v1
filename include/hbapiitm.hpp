// Header file for the Item API
// Copyright 1999 Antonio Linares <alinares@fivetech.com>

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#ifndef HB_APIITM_H_
#define HB_APIITM_H_

#include "hbapi.hpp"

HB_EXTERN_BEGIN

#define HB_EVAL_PARAM_MAX_ 9

typedef struct
{
   uint16_t paramCount;
   HB_ITEM *pItems[ HB_EVAL_PARAM_MAX_ + 1 ];
} HB_EVALINFO, * PHB_EVALINFO;

extern HB_EXPORT HB_ITEM *    hb_evalLaunch    ( PHB_EVALINFO pEvalInfo );
extern HB_EXPORT HB_BOOL      hb_evalNew       ( PHB_EVALINFO pEvalInfo, HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_evalPutParam  ( PHB_EVALINFO pEvalInfo, HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_evalRelease   ( PHB_EVALINFO pEvalInfo );

extern HB_EXPORT void         hb_evalBlock( HB_ITEM *pCodeBlock, ... );
extern HB_EXPORT void         hb_evalBlock0( HB_ITEM *pCodeBlock );
extern HB_EXPORT void         hb_evalBlock1( HB_ITEM *pCodeBlock, HB_ITEM *pParam );

extern HB_EXPORT HB_BOOL      hb_execFromArray ( HB_ITEM *pParam );

extern HB_EXPORT HB_ITEM     *hb_itemDo        ( HB_ITEM *pItem, HB_ULONG ulPCount, ... );
extern HB_EXPORT HB_ITEM     *hb_itemDoC       ( const char * szFunc, HB_ULONG ulPCount, ... );

extern HB_EXPORT HB_ITEM     *hb_itemArrayGet  ( HB_ITEM *pArray, HB_SIZE nIndex );
extern HB_EXPORT HB_ITEM     *hb_itemArrayNew  ( HB_SIZE nLen );
extern HB_EXPORT HB_ITEM     *hb_itemArrayPut  ( HB_ITEM *pArray, HB_SIZE nIndex, HB_ITEM *pItem );
extern HB_EXPORT HB_SIZE      hb_itemCopyC     ( HB_ITEM *pItem, char * szBuffer, HB_SIZE nLen );
extern HB_EXPORT HB_BOOL      hb_itemFreeC     ( char * szText );
extern HB_EXPORT const char * hb_itemGetCRef   ( HB_ITEM *pItem, void ** phRef, HB_SIZE * pnLen );
extern HB_EXPORT void         hb_itemFreeCRef  ( void * hRef );
extern HB_EXPORT char *       hb_itemGetC      ( HB_ITEM *pItem );
extern HB_EXPORT const char * hb_itemGetCPtr   ( HB_ITEM *pItem );
extern HB_EXPORT HB_SIZE      hb_itemGetCLen   ( HB_ITEM *pItem );
extern HB_EXPORT char *       hb_itemGetDS     ( HB_ITEM *pItem, char * szDate );
extern HB_EXPORT char *       hb_itemGetTS     ( HB_ITEM *pItem, char * szDateTime );
extern HB_EXPORT long         hb_itemGetDL     ( HB_ITEM *pItem );
extern HB_EXPORT double       hb_itemGetTD     ( HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemGetTDT    ( HB_ITEM *pItem, long * plJulian, long * plMilliSec );
extern HB_EXPORT HB_BOOL      hb_itemGetL      ( HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemGetLX     ( HB_ITEM *pItem );
extern HB_EXPORT double       hb_itemGetND     ( HB_ITEM *pItem );
extern HB_EXPORT double       hb_itemGetNDDec  ( HB_ITEM *pItem, int32_t * piDec );
extern HB_EXPORT int32_t          hb_itemGetNI     ( HB_ITEM *pItem );
extern HB_EXPORT long         hb_itemGetNL     ( HB_ITEM *pItem );
extern HB_EXPORT HB_ISIZ      hb_itemGetNS     ( HB_ITEM *pItem );
extern HB_EXPORT HB_MAXINT    hb_itemGetNInt   ( HB_ITEM *pItem );
extern HB_EXPORT void         hb_itemGetNLen   ( HB_ITEM *pItem, int32_t * piWidth, int32_t * piDec );
extern HB_EXPORT void *       hb_itemGetPtr    ( HB_ITEM *pItem );
extern HB_EXPORT void *       hb_itemGetPtrGC  ( HB_ITEM *pItem, const HB_GC_FUNCS * pFuncs );
extern HB_EXPORT HB_SYMB *    hb_itemGetSymbol ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemNew       ( HB_ITEM *pNull );
extern HB_EXPORT void         hb_itemInit      ( HB_ITEM *pItem );
extern HB_EXPORT uint16_t    hb_itemPCount    ( void );
extern HB_EXPORT HB_ITEM     *hb_itemParam     ( uint16_t uiParam );
extern HB_EXPORT HB_ITEM     *hb_itemPutC      ( HB_ITEM *pItem, const char * szText );
extern HB_EXPORT HB_ITEM     *hb_itemPutCL     ( HB_ITEM *pItem, const char * szText, HB_SIZE nLen );
extern HB_EXPORT HB_ITEM     *hb_itemPutCConst ( HB_ITEM *pItem, const char * szText );
extern HB_EXPORT HB_ITEM     *hb_itemPutCLConst( HB_ITEM *pItem, const char * szText, HB_SIZE nLen );
extern HB_EXPORT HB_ITEM     *hb_itemPutCPtr   ( HB_ITEM *pItem, char * szText );
extern HB_EXPORT HB_ITEM     *hb_itemPutCLPtr  ( HB_ITEM *pItem, char * szText, HB_SIZE nLen );
extern HB_EXPORT void         hb_itemSetCMemo  ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemPutD      ( HB_ITEM *pItem, int32_t iYear, int32_t iMonth, int32_t iDay );
extern HB_EXPORT HB_ITEM     *hb_itemPutDS     ( HB_ITEM *pItem, const char * szDate );
extern HB_EXPORT HB_ITEM     *hb_itemPutTS     ( HB_ITEM *pItem, const char * szDateTime );
extern HB_EXPORT HB_ITEM     *hb_itemPutDL     ( HB_ITEM *pItem, long lJulian );
extern HB_EXPORT HB_ITEM     *hb_itemPutTD     ( HB_ITEM *pItem, double dTimeStamp );
extern HB_EXPORT HB_ITEM     *hb_itemPutTDT    ( HB_ITEM *pItem, long lJulian, long lMilliSec );
extern HB_EXPORT HB_ITEM     *hb_itemPutL      ( HB_ITEM *pItem, HB_BOOL bValue );
extern HB_EXPORT HB_ITEM     *hb_itemPutND     ( HB_ITEM *pItem, double dNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNI     ( HB_ITEM *pItem, int32_t iNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNL     ( HB_ITEM *pItem, long lNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNS     ( HB_ITEM *pItem, HB_ISIZ nNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNInt   ( HB_ITEM *pItem, HB_MAXINT nNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNIntLen( HB_ITEM *pItem, HB_MAXINT nNumber, int32_t iWidth );
extern HB_EXPORT HB_ITEM     *hb_itemPutNLen   ( HB_ITEM *pItem, double dNumber, int32_t iWidth, int32_t iDec );
extern HB_EXPORT HB_ITEM     *hb_itemPutNDLen  ( HB_ITEM *pItem, double dNumber, int32_t iWidth, int32_t iDec );
extern HB_EXPORT HB_ITEM     *hb_itemPutNDDec  ( HB_ITEM *pItem, double dNumber, int32_t iDec );
extern HB_EXPORT HB_ITEM     *hb_itemPutNILen  ( HB_ITEM *pItem, int32_t iNumber, int32_t iWidth );
extern HB_EXPORT HB_ITEM     *hb_itemPutNLLen  ( HB_ITEM *pItem, long lNumber, int32_t iWidth );
extern HB_EXPORT HB_ITEM     *hb_itemPutNumType( HB_ITEM *pItem, double dNumber, int32_t iDec, int32_t iType1, int32_t iType2 );
extern HB_EXPORT HB_ITEM     *hb_itemPutPtr    ( HB_ITEM *pItem, void * pValue );
extern HB_EXPORT HB_ITEM     *hb_itemPutPtrGC  ( HB_ITEM *pItem, void * pValue );
extern HB_EXPORT HB_ITEM     *hb_itemPutSymbol ( HB_ITEM *pItem, HB_SYMB *pSym );
extern HB_EXPORT HB_ITEM     *hb_itemPutNil    ( HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemRelease   ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemReturn    ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemReturnForward( HB_ITEM *pItem );
extern HB_EXPORT void         hb_itemReturnRelease( HB_ITEM *pItem );
extern HB_EXPORT HB_SIZE      hb_itemSize      ( HB_ITEM *pItem );
extern HB_EXPORT HB_TYPE      hb_itemType      ( HB_ITEM *pItem );
extern HB_EXPORT const char * hb_itemTypeStr   ( HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemTypeCmp   ( HB_ITEM *pItem1, HB_ITEM *pItem2 );
#ifndef HB_LONG_LONG_OFF
extern HB_EXPORT int64_t  hb_itemGetNLL    ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemPutNLL    ( HB_ITEM *pItem, int64_t lNumber );
extern HB_EXPORT HB_ITEM     *hb_itemPutNLLLen ( HB_ITEM *pItem, int64_t lNumber, int32_t iWidth );
#endif

/* Non Clipper compliant internal API */

extern HB_EXPORT HB_ITEM     *hb_itemParamPtr  ( uint16_t uiParam, long lMask );
extern HB_EXPORT HB_BOOL      hb_itemParamStore( uint16_t uiParam, HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemParamStoreForward( uint16_t uiParam, HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemParamStoreRelease( uint16_t uiParam, HB_ITEM *pItem );
extern HB_EXPORT HB_BOOL      hb_itemEqual     ( HB_ITEM *pItem1, HB_ITEM *pItem2 );
extern HB_EXPORT HB_BOOL      hb_itemCompare   ( HB_ITEM *pItem1, HB_ITEM *pItem2, HB_BOOL bForceExact, int32_t * piResult ); /* For compatible types compare pItem1 with pItem2 setting piResult to -1, 0 or 1 if pItem1 is <, == or > then pItem2 and return true otherwise return false. */
extern HB_EXPORT int32_t          hb_itemStrCmp    ( HB_ITEM *pFirst, HB_ITEM *pSecond, HB_BOOL bForceExact ); /* our string compare */
extern HB_EXPORT int32_t          hb_itemStrICmp   ( HB_ITEM *pFirst, HB_ITEM *pSecond, HB_BOOL bForceExact ); /* our string compare */
extern HB_EXPORT void         hb_itemCopy      ( HB_ITEM *pDest, HB_ITEM *pSource ); /* copies an item to one place to another respecting its content */
extern HB_EXPORT void         hb_itemCopyToRef ( HB_ITEM *pDest, HB_ITEM *pSource );
extern HB_EXPORT void         hb_itemCopyFromRef( HB_ITEM *pDest, HB_ITEM *pSource );
extern HB_EXPORT void         hb_itemMove      ( HB_ITEM *pDest, HB_ITEM *pSource ); /* moves the value of an item without incrementing of reference counters, source is cleared */
extern HB_EXPORT void         hb_itemMoveRef   ( HB_ITEM *pDest, HB_ITEM *pSource );
extern HB_EXPORT void         hb_itemMoveToRef ( HB_ITEM *pDest, HB_ITEM *pSource );
extern HB_EXPORT void         hb_itemMoveFromRef( HB_ITEM *pDest, HB_ITEM *pSource );
extern HB_EXPORT void         hb_itemClear     ( HB_ITEM *pItem );
extern HB_EXPORT HB_ITEM     *hb_itemUnRef     ( HB_ITEM *pItem ); /* de-references passed variable */
extern HB_EXPORT HB_ITEM     *hb_itemUnRefOnce ( HB_ITEM *pItem ); /* de-references passed variable, one step*/
extern HB_EXPORT HB_ITEM     *hb_itemUnRefRefer( HB_ITEM *pItem ); /* de-references passed variable, leaving the last reference */
extern HB_EXPORT HB_ITEM     *hb_itemUnRefWrite( HB_ITEM *pItem, HB_ITEM *pSource ); /* de-references passed variable for writing */
extern HB_EXPORT HB_ITEM     *hb_itemUnShare   ( HB_ITEM *pItem ); /* un-share given string item */
extern HB_EXPORT HB_ITEM     *hb_itemUnShareString( HB_ITEM *pItem ); /* un-share given string item - the pItem have to be valid unrefed string item */
extern HB_EXPORT HB_ITEM     *hb_itemReSizeString( HB_ITEM *pItem, HB_SIZE nSize ); /* Resize string buffer of given string item - the pItem have to be valid unrefed string item */
extern HB_EXPORT HB_BOOL      hb_itemGetWriteCL( HB_ITEM *pItem, char ** pszValue, HB_SIZE * pnLen );
extern HB_EXPORT HB_ITEM     *hb_itemClone     ( HB_ITEM *pItem ); /* clone the given item */
extern HB_EXPORT void         hb_itemCloneTo   ( HB_ITEM *pDest, HB_ITEM *pSource ); /* clone the given item */
extern HB_EXPORT char *       hb_itemStr       ( HB_ITEM *pNumber, HB_ITEM *pWidth, HB_ITEM *pDec ); /* convert a number to a string */
extern HB_EXPORT char *       hb_itemString    ( HB_ITEM *pItem, HB_SIZE * nLen, HB_BOOL * bFreeReq );  /* Convert any scalar to a string */
extern HB_EXPORT HB_BOOL      hb_itemStrBuf    ( char *szResult, HB_ITEM *pNumber, int32_t iSize, int32_t iDec ); /* convert a number to a string */
extern HB_EXPORT HB_ITEM     *hb_itemValToStr  ( HB_ITEM *pItem ); /* Convert any scalar to a string */
extern HB_EXPORT char *       hb_itemPadConv   ( HB_ITEM *pItem, HB_SIZE * pnSize, HB_BOOL * bFreeReq );
extern HB_EXPORT void         hb_itemSwap      ( HB_ITEM *pItem1, HB_ITEM *pItem2 );

extern HB_EXPORT char *       hb_itemSerialize( HB_ITEM *pItem, int32_t iFlags, HB_SIZE * pnSize );
extern HB_EXPORT HB_ITEM     *hb_itemDeserialize( const char ** pBufferPtr, HB_SIZE * pnSize );

#if defined(_HB_API_INTERNAL_)

extern HB_ITEM *hb_itemPutPtrRawGC( HB_ITEM *pItem, void * pValue );

#  define hb_itemSetNil( item )           do { \
                                             if( HB_IS_COMPLEX( item ) ) \
                                                hb_itemClear( item ); \
                                             else \
                                                (item)->type = HB_IT_NIL; \
                                          } while( 0 )

#  define hb_itemRawCpy( dst, src )       do { *(dst) = *(src); } while( 0 )

#  define hb_itemRawSwap( dst, src )      do { \
                                             HB_ITEM temp; \
                                             hb_itemRawCpy( &temp, dst ); \
                                             hb_itemRawCpy( dst, src ); \
                                             hb_itemRawCpy( src, &temp ); \
                                          } while( 0 )

#if 1
#  define hb_itemRawMove( dst, src )      do { \
                                             hb_itemRawCpy( dst, src ); \
                                             (src)->type = HB_IT_NIL; \
                                          } while( 0 )
#else /* _HB_API_INTERNAL_ */
#  define hb_itemRawMove( dst, src )      hb_itemMove( (dst), (src) )
#endif

   /* intentional low-level hack to eliminate race condition in
    * unprotected readonly access in few places in core code only.
    * hb_item[Raw]Move() moves HB_ITEM structure members first coping
    * 'type' and then 'item' parts of HB_ITEM. In this macro the order
    * is reverted. [druzus]
    */
#  define hb_itemSafeMove( dst, src )  do { \
                                             (dst)->item = (src)->item; \
                                             (dst)->type = (src)->type; \
                                             (src)->type = HB_IT_NIL; \
                                          } while( 0 )

#else

#  define hb_itemSetNil( item )           hb_itemClear( (item) )

#  define hb_itemRawMove( dst, src )      hb_itemMove( (dst), (src) )

#endif // _HB_API_INTERNAL_

HB_EXTERN_END

#endif // HB_APIITM_H_
