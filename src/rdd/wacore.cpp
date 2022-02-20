/*
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"

#define HB_SET_WA( n )  \
   do \
   { \
      pRddInfo->uiCurrArea = n; \
      pRddInfo->pCurrArea  = ( ( pRddInfo->uiCurrArea < pRddInfo->uiWaNumMax ) ? \
                               pRddInfo->waList[ pRddInfo->waNums[ pRddInfo->uiCurrArea ] ] : \
                               nullptr ); \
   } while( 0 )

/*
 * Insert new WorkArea node at current WA position
 */
static void hb_waNodeInsert( PHB_STACKRDD pRddInfo, AREAP pArea )
{
   HB_USHORT uiWaPos;

   if( pRddInfo->uiCurrArea >= pRddInfo->uiWaNumMax )
   {
      int iSize = ( ( static_cast<int>( pRddInfo->uiCurrArea ) + 256 ) >> 8 ) << 8;

      if( iSize > HB_RDD_MAX_AREA_NUM )
      {
         iSize = HB_RDD_MAX_AREA_NUM;
      }

      if( pRddInfo->uiWaNumMax == 0 )
      {
         pRddInfo->waNums = static_cast<HB_USHORT*>( hb_xgrab( iSize * sizeof( HB_USHORT ) ) );
      }
      else
      {
         pRddInfo->waNums = static_cast<HB_USHORT*>( hb_xrealloc( pRddInfo->waNums, iSize * sizeof( HB_USHORT ) ) );
      }

      memset( &pRddInfo->waNums[ pRddInfo->uiWaNumMax ], 0, ( iSize - pRddInfo->uiWaNumMax ) * sizeof( HB_USHORT ) );
      pRddInfo->uiWaNumMax = static_cast<HB_USHORT>( iSize );
   }

   if( pRddInfo->uiWaSpace == 0 )
   {
      pRddInfo->uiWaSpace = 256;
      pRddInfo->waList = static_cast<void**>( hb_xgrabz( pRddInfo->uiWaSpace * sizeof( void * ) ) );
      uiWaPos = 1;
      pRddInfo->uiWaMax = 2;
   }
   else
   {
      uiWaPos = pRddInfo->uiWaMax++;
      if( pRddInfo->uiWaMax > pRddInfo->uiWaSpace )
      {
         int iSize = ( ( static_cast<int>( pRddInfo->uiWaMax ) + 256 ) >> 8 ) << 8;

         if( iSize > HB_RDD_MAX_AREA_NUM )
         {
            iSize = HB_RDD_MAX_AREA_NUM;
         }

         pRddInfo->uiWaSpace = static_cast<HB_USHORT>( iSize );
         pRddInfo->waList = static_cast<void**>( hb_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) ) );
         memset( &pRddInfo->waList[ pRddInfo->uiWaMax ], 0, ( pRddInfo->uiWaSpace - pRddInfo->uiWaMax ) * sizeof( void * ) );
      }
      while( uiWaPos > 1 )
      {
         if( ( static_cast<AREAP>( pRddInfo->waList[ uiWaPos - 1 ] ) )->uiArea < pRddInfo->uiCurrArea )
         {
            break;
         }
         pRddInfo->waList[ uiWaPos ] = pRddInfo->waList[ uiWaPos - 1 ];
         pRddInfo->waNums[ ( static_cast<AREAP>( pRddInfo->waList[ uiWaPos ] ) )->uiArea ] = uiWaPos;
         uiWaPos--;
      }
   }
   pRddInfo->waNums[ pRddInfo->uiCurrArea ] = uiWaPos;
   pRddInfo->pCurrArea = pRddInfo->waList[ uiWaPos ] = pArea;
   pArea->uiArea = pRddInfo->uiCurrArea;
}

/*
 * Remove current WorkArea node
 */
static void hb_waNodeDelete( PHB_STACKRDD pRddInfo )
{
   HB_USHORT uiWaPos;

   uiWaPos = pRddInfo->waNums[ pRddInfo->uiCurrArea ];
   pRddInfo->waNums[ pRddInfo->uiCurrArea ] = 0;
   pRddInfo->uiWaMax--;
   if( pRddInfo->uiWaMax <= 1 )
   {
      pRddInfo->uiWaSpace = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      hb_xfree( pRddInfo->waList );
      hb_xfree( pRddInfo->waNums );
      pRddInfo->waList = nullptr;
      pRddInfo->waNums = nullptr;
   }
   else
   {
      while( uiWaPos < pRddInfo->uiWaMax )
      {
         pRddInfo->waList[ uiWaPos ] = pRddInfo->waList[ uiWaPos + 1 ];
         pRddInfo->waNums[ ( static_cast<AREAP>( pRddInfo->waList[ uiWaPos ] ) )->uiArea ] = uiWaPos;
         uiWaPos++;
      }
      pRddInfo->waList[ pRddInfo->uiWaMax ] = nullptr;
      if( pRddInfo->uiWaSpace - pRddInfo->uiWaMax > 256 )
      {
         int iSize = ( ( static_cast<int>( pRddInfo->uiWaMax ) + 256 ) >> 8 ) << 8;

         if( iSize > HB_RDD_MAX_AREA_NUM )
         {
            iSize = HB_RDD_MAX_AREA_NUM;
         }

         pRddInfo->uiWaSpace = static_cast<HB_USHORT>( iSize );
         pRddInfo->waList = static_cast<void**>( hb_xrealloc( pRddInfo->waList, pRddInfo->uiWaSpace * sizeof( void * ) ) );
      }
   }
   pRddInfo->pCurrArea = nullptr;
}

/*
 * Return the next free WorkArea for later use.
 */
HB_ERRCODE hb_rddSelectFirstAvailable( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddSelectFirstAvailable()" ) );
#endif

   PHB_STACKRDD pRddInfo;
   HB_USHORT uiArea;

   pRddInfo = hb_stackRDD();

   uiArea = 1;
   while( uiArea < pRddInfo->uiWaNumMax )
   {
      if( pRddInfo->waNums[ uiArea ] == 0 )
      {
         break;
      }
      uiArea++;
   }
   if( uiArea >= HB_RDD_MAX_AREA_NUM )
   {
      return HB_FAILURE;
   }
   HB_SET_WA( uiArea );
   return HB_SUCCESS;
}

/*
 * Create and insert the new WorkArea node
 */
HB_USHORT hb_rddInsertAreaNode( const char * szDriver )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddInsertAreaNode(%s)", szDriver ) );
#endif

   PHB_STACKRDD pRddInfo;
   LPRDDNODE pRddNode;
   HB_USHORT uiRddID;
   AREAP pArea;

   pRddInfo = hb_stackRDD();
   if( pRddInfo->uiCurrArea && pRddInfo->pCurrArea )
   {
      return 0;
   }

   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( ! pRddNode )
   {
      return 0;
   }

   if( pRddInfo->uiCurrArea == 0 )
   {
      if( hb_rddSelectFirstAvailable() != HB_SUCCESS )
      {
         return 0;
      }
   }

   pArea = static_cast<AREAP>( hb_rddNewAreaNode( pRddNode, uiRddID ) );
   if( ! pArea )
   {
      return 0;
   }

   hb_waNodeInsert( pRddInfo, pArea );

   return pRddInfo->uiCurrArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
void hb_rddReleaseCurrentArea( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddReleaseCurrentArea()" ) );
#endif

   PHB_STACKRDD pRddInfo;
   AREAP pArea;

   pRddInfo = hb_stackRDD();
   pArea = static_cast<AREAP>( pRddInfo->pCurrArea );
   if( ! pArea )
   {
      return;
   }

   if( SELF_CLOSE( pArea ) == HB_FAILURE )
   {
      return;
   }

   SELF_RELEASE( pArea );

   hb_waNodeDelete( pRddInfo );
}

/*
 * Closes all WorkAreas.
 */
void hb_rddCloseAll( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddCloseAll()" ) );
#endif

   PHB_STACKRDD pRddInfo;

   pRddInfo = hb_stackRDD();
   if( pRddInfo->uiWaMax > 0 )
   {
      HB_BOOL isParents, isFinish = HB_FALSE;
      AREAP pArea;

      do
      {
         isParents = HB_FALSE;
         for( HB_USHORT uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
         {
            pArea = static_cast<AREAP>( pRddInfo->waList[ uiIndex ] );
            HB_SET_WA( pArea->uiArea );
            if( isFinish )
            {
               SELF_RELEASE( pArea );
               pRddInfo->waNums[ pRddInfo->uiCurrArea ] = 0;
               pRddInfo->pCurrArea = nullptr;
            }
            else if( pArea->uiParents )
            {
               isParents = HB_TRUE;
            }
            else
            {
               SELF_CLOSE( pArea );
            }
         }
         if( ! isParents && ! isFinish )
         {
            isParents = isFinish = HB_TRUE;
         }
      }
      while( isParents );

      pRddInfo->uiWaSpace = pRddInfo->uiWaMax = pRddInfo->uiWaNumMax = 0;
      hb_xfree( pRddInfo->waList );
      hb_xfree( pRddInfo->waNums );
      pRddInfo->waList = nullptr;
      pRddInfo->waNums = nullptr;
      HB_SET_WA( 1 );
   }
}

void hb_rddFlushAll( void )
{
   PHB_STACKRDD pRddInfo = hb_stackRDD();
   HB_USHORT uiArea = static_cast<HB_AREANO>( hb_rddGetCurrentWorkAreaNumber() );

   for( HB_USHORT uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( ( static_cast<AREAP>( pRddInfo->waList[ uiIndex ] ) )->uiArea );
      SELF_FLUSH( static_cast<AREAP>( pRddInfo->pCurrArea ) );
   }
   hb_rddSelectWorkAreaNumber( uiArea );
}

void hb_rddUnLockAll( void )
{
   PHB_STACKRDD pRddInfo = hb_stackRDD();
   HB_USHORT uiArea = static_cast<HB_AREANO>( hb_rddGetCurrentWorkAreaNumber() );

   for( HB_USHORT uiIndex = 1; uiIndex < pRddInfo->uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( ( static_cast<AREAP>( pRddInfo->waList[ uiIndex ] ) )->uiArea );
      SELF_UNLOCK( static_cast<AREAP>( pRddInfo->pCurrArea ), nullptr );
   }
   hb_rddSelectWorkAreaNumber( uiArea );
}

/*
 * call a pCallBack function with all open workareas ###
 */
HB_ERRCODE hb_rddIterateWorkAreas( WACALLBACK pCallBack, void * cargo )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddIterateWorkAreas(%p, %p)", static_cast<void*>( pCallBack ), cargo ) );
#endif

   PHB_STACKRDD pRddInfo;
   HB_ERRCODE errCode = HB_SUCCESS;

   pRddInfo = hb_stackRDD();
   for( HB_USHORT uiIndex = 1; uiIndex < pRddInfo->uiWaMax; uiIndex++ )
   {
      AREAP pArea = static_cast<AREAP>( pRddInfo->waList[ uiIndex ] );
      errCode = pCallBack( pArea, cargo );
      if( errCode != HB_SUCCESS )
      {
         break;
      }
      if( uiIndex >= pRddInfo->uiWaMax || pArea != static_cast<AREAP>( pRddInfo->waList[ uiIndex ] ) )
      {
         uiIndex--;
      }
   }
   return errCode;
}

HB_BOOL hb_rddGetNetErr( void )
{
   return hb_stackRDD()->fNetError;
}

void hb_rddSetNetErr( HB_BOOL fNetErr )
{
   hb_stackRDD()->fNetError = fNetErr;
}

/*
 * Get (/set) default RDD driver
 */
const char * hb_rddDefaultDrv( const char * szDriver )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddDefaultDrv(%s)", szDriver ) );
#endif

   PHB_STACKRDD pRddInfo;

   pRddInfo = hb_stackRDD();

   if( szDriver && *szDriver )
   {
      char szNewDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
      LPRDDNODE pRddNode;

      hb_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      pRddNode = hb_rddFindNode( szNewDriver, nullptr );
      if( ! pRddNode )
      {
         return nullptr;
      }

      pRddInfo->szDefaultRDD = pRddNode->szName;
   }
   else if( ! pRddInfo->szDefaultRDD && hb_rddGetNode( 0 ) )
   {
      const char * szDrvTable[] = { "DBFNTX", "DBFCDX", "DBFFPT", "DBF" };

      pRddInfo->szDefaultRDD = "";
      for( int i = 0; i < static_cast<int>( HB_SIZEOFARRAY( szDrvTable ) ); ++i )
      {
         if( hb_rddFindNode( szDrvTable[ i ], nullptr ) )
         {
            pRddInfo->szDefaultRDD = szDrvTable[ i ];
            break;
         }
      }
   }

   return pRddInfo->szDefaultRDD;
}

/*
 * Get default RDD driver respecting passed table/file name
 */
const char * hb_rddFindDrv( const char * szDriver, const char * szFileName )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddFindDrv(%s, %s)", szDriver, szFileName ) );
#endif

   LPRDDNODE pRddNode = nullptr;

   if( szDriver && *szDriver )
   {
      char szNewDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

      hb_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      pRddNode = hb_rddFindNode( szNewDriver, nullptr );
   }
   else
   {
      PHB_STACKRDD pRddInfo = hb_stackRDD();

      if( pRddInfo->szDefaultRDD )
      {
         if( pRddInfo->szDefaultRDD[ 0 ] )
         {
            pRddNode = hb_rddFindNode( pRddInfo->szDefaultRDD, nullptr );
         }
      }
      else if( hb_rddGetNode( 0 ) )
      {
         const char * szDrvTable[] = { "DBFNTX", "DBFCDX", "DBFFPT", "DBF" };

         pRddInfo->szDefaultRDD = "";
         for( int i = 0; i < static_cast<int>( HB_SIZEOFARRAY( szDrvTable ) ); ++i )
         {
            pRddNode = hb_rddFindNode( szDrvTable[ i ], nullptr );
            if( pRddNode )
            {
               pRddInfo->szDefaultRDD = szDrvTable[ i ];
               break;
            }
         }
      }
   }

   return pRddNode ? hb_rddFindFileNode( pRddNode, szFileName )->szName : nullptr;
}

/*
 * Function for getting given workarea pointer
 */
void * hb_rddGetWorkAreaPointer( int iArea )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetWorkAreaPointer(%d)", iArea ) );
#endif

   PHB_STACKRDD pRddInfo;

   pRddInfo = hb_stackRDD();

   if( iArea == 0 )
   {
      return pRddInfo->pCurrArea;
   }
   else if( iArea >= 1 && static_cast<HB_UINT>( iArea ) < static_cast<HB_UINT>( pRddInfo->uiWaNumMax ) )
   {
      return pRddInfo->waList[ pRddInfo->waNums[ iArea ] ];
   }
   else
   {
      return nullptr;
   }
}

/*
 * Function for getting current workarea pointer
 */
void * hb_rddGetCurrentWorkAreaPointer( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetCurrentWorkAreaPointer()" ) );
#endif

   return hb_stackRDD()->pCurrArea;
}

/*
 * Return the current WorkArea number.
 */
int hb_rddGetCurrentWorkAreaNumber( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetCurrentWorkAreaNumber()" ) );
#endif

   return hb_stackRDD()->uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
HB_ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddSelectWorkAreaNumber(%d)", iArea ) );
#endif

   PHB_STACKRDD pRddInfo;

   pRddInfo = hb_stackRDD();
   if( iArea < 1 || iArea > HB_RDD_MAX_AREA_NUM )
   {
      HB_SET_WA( 0 );
   }
   else
   {
      HB_SET_WA( static_cast<HB_AREANO>( iArea ) );
   }

   return ( pRddInfo->pCurrArea == nullptr ) ? HB_FAILURE : HB_SUCCESS;
}

/* Moving workareas between threads */

static HB_CRITICAL_NEW( s_waMtx );
static HB_COND_NEW( s_waCond );
static PHB_ITEM s_pDetachedAreas = nullptr;

static HB_GARBAGE_FUNC( hb_waHolderDestructor )
{
   AREAP * pHolder = static_cast<AREAP*>( Cargo );

   if( *pHolder )
   {
      AREAP pArea;
      int iArea;

      pArea = *pHolder;
      *pHolder = nullptr;

      iArea = hb_rddGetCurrentWorkAreaNumber();

      if( hb_rddSelectFirstAvailable() != HB_SUCCESS )
      {
         /* workarea number HB_RDD_MAX_AREA_NUM is reserved
            for this destructor and used when all other workareas
            are active [druzus] */
         hb_rddSelectWorkAreaNumber( HB_RDD_MAX_AREA_NUM );
      }
      hb_waNodeInsert( hb_stackRDD(), pArea );
      hb_rddReleaseCurrentArea();

      hb_rddSelectWorkAreaNumber( iArea );
   }
}

static const HB_GC_FUNCS s_gcWAFuncs =
{
   hb_waHolderDestructor,
   hb_gcDummyMark
};

void hb_rddCloseDetachedAreas( void )
{
   PHB_ITEM pDetachedArea;

   /* protect by critical section access to s_pDetachedAreas array */
   hb_threadEnterCriticalSectionGC( &s_waMtx );
   pDetachedArea = s_pDetachedAreas;
   s_pDetachedAreas = nullptr;
   /* leave critical section */
   hb_threadLeaveCriticalSection( &s_waMtx );
   /* release detached areas */
   if( pDetachedArea )
   {
      hb_itemRelease( pDetachedArea );
   }
}

HB_ERRCODE hb_rddDetachArea( AREAP pArea, PHB_ITEM pCargo )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddDetachArea(%p, %p)", static_cast<void*>( pArea ), static_cast<void*>( pCargo ) ) );
#endif

   AREAP * pHolder;
   PHB_ITEM pDetachedArea;
   HB_SIZE nPos;
   int iArea;

   /* save current WA number */
   iArea = hb_rddGetCurrentWorkAreaNumber();
   /* select given WA */
   hb_rddSelectWorkAreaNumber( pArea->uiArea );
   /* flush buffers */
   SELF_GOCOLD( pArea );

   /* tests shows that Xbase++ does not remove locks */
   #if 0
   SELF_UNLOCK( pArea, nullptr );
   #endif

   /* Xbase++ documentation says that child areas are also detached but
    * but tests shows that it's not true and either child or parent relations
    * are still active and corresponding WA are not detached together.
    * Harbour clears all child and parent relations.
    */
   SELF_CLEARREL( pArea );
   hb_rddCloseAllParentRelations( pArea );

   /* detach WA and alias */
   hb_waNodeDelete( hb_stackRDD() );
   pArea->uiArea = 0;
   if( pArea->atomAlias )
   {
      hb_dynsymSetAreaHandle( static_cast<PHB_DYNS>( pArea->atomAlias ), 0 );
   }

   /* restore previous WA number */
   hb_rddSelectWorkAreaNumber( iArea );

   /* protect by critical section access to s_pDetachedAreas array */
   hb_threadEnterCriticalSectionGC( &s_waMtx );
   if( ! s_pDetachedAreas )
   {
      s_pDetachedAreas = hb_itemArrayNew( 1 );
      nPos = 1;
   }
   else
   {
      nPos = hb_arrayLen( s_pDetachedAreas ) + 1;
      hb_arraySize( s_pDetachedAreas, nPos );
   }
   pDetachedArea = hb_arrayGetItemPtr( s_pDetachedAreas, nPos );
   hb_arrayNew( pDetachedArea, 2 );
   if( pCargo )
   {
      hb_arraySet( pDetachedArea, 2, pCargo );
   }
   pHolder = static_cast<AREAP*>( hb_gcAllocate( sizeof( AREAP ), &s_gcWAFuncs ) );
   *pHolder = pArea;
   hb_arraySetPtrGC( pDetachedArea, 1, pHolder );
   /* siagnal waiting processes that new area is available */
   hb_threadCondBroadcast( &s_waCond );
   /* leave critical section */
   hb_threadLeaveCriticalSection( &s_waMtx );

   return HB_SUCCESS;
}

AREAP hb_rddRequestArea( const char * szAlias, PHB_ITEM pCargo, HB_BOOL fNewArea, HB_ULONG ulMilliSec )
{
   PHB_DYNS pSymAlias = nullptr;
   AREAP pArea = nullptr;

   if( pCargo )
   {
      hb_itemClear( pCargo );
   }

   /* close current WA or chose 1st free available */
   if( ! fNewArea )
   {
      hb_rddReleaseCurrentArea();
   }
   else if( hb_rddSelectFirstAvailable() != HB_SUCCESS )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME );
      return nullptr;
   }

   if( szAlias )
   {
      pSymAlias = hb_dynsymGet( szAlias );

      /* verify if the alias name is valid symbol */
      if( hb_rddVerifyAliasName( szAlias ) != HB_SUCCESS )
      {
         hb_errRT_DBCMD_Ext( EG_BADALIAS, EDBCMD_BADALIAS, nullptr, szAlias, EF_CANDEFAULT );
         return nullptr;
      }
      /* verify if the alias is already in use */
      if( hb_dynsymAreaHandle( pSymAlias ) != 0 )
      {
         hb_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, nullptr, szAlias, EF_CANDEFAULT );
         return nullptr;
      }
   }

   /* protect by critical section access to s_pDetachedAreas array */
   hb_threadEnterCriticalSectionGC( &s_waMtx );
   for( ;; )
   {
      if( s_pDetachedAreas )
      {
         HB_SIZE nLen = hb_arrayLen( s_pDetachedAreas ), nPos = 1;
         if( pSymAlias )
         {
            for( nPos = 1; nPos <= nLen; ++nPos )
            {
               AREAP * pDetachedArea = static_cast<AREAP*>( hb_arrayGetPtrGC( hb_arrayGetItemPtr( s_pDetachedAreas, nPos ), 1, &s_gcWAFuncs ) );
               if( pSymAlias == static_cast<PHB_DYNS>( ( *pDetachedArea )->atomAlias ) )
               {
                  break;
               }
            }
         }
         if( nPos <= nLen )
         {
            PHB_ITEM pArray = hb_arrayGetItemPtr( s_pDetachedAreas, nPos );
            AREAP * pDetachedArea = static_cast<AREAP*>( hb_arrayGetPtrGC( pArray, 1, &s_gcWAFuncs ) );

            pArea = *pDetachedArea;
            *pDetachedArea = nullptr;
            if( pCargo )
            {
               hb_arrayGet( pArray, 2, pCargo );
            }
            hb_arrayDel( s_pDetachedAreas, nPos );
            hb_arraySize( s_pDetachedAreas, nLen - 1 );
         }
      }

      if( pArea || ulMilliSec == 0 )
      {
         break;
      }

      hb_vmUnlock();
      /* wait for detached workareas */
      if( ulMilliSec == HB_THREAD_INFINITE_WAIT )
      {
         hb_threadCondWait( &s_waCond, &s_waMtx );
      }
      else if( ! hb_threadCondTimedWait( &s_waCond, &s_waMtx, ulMilliSec ) )
      {
         ulMilliSec = 0;
      }
      hb_vmLock();

      if( ulMilliSec == 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
   }
   /* leave critical section */
   hb_threadLeaveCriticalSection( &s_waMtx );

   /* attach WA and set alias */
   if( pArea )
   {
      hb_waNodeInsert( hb_stackRDD(), pArea );
      if( pArea->atomAlias )
      {
         if( hb_dynsymAreaHandle( static_cast<PHB_DYNS>( pArea->atomAlias ) ) == 0 )
         {
            hb_dynsymSetAreaHandle( static_cast<PHB_DYNS>( pArea->atomAlias ), pArea->uiArea );
         }
      }
   }

   return pArea;
}

PHB_ITEM hb_rddDetachedList( void )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddDetachedList()" ) );
#endif

   PHB_ITEM pArray;

   pArray = hb_itemArrayNew( 0 );
   /* protect by critical section access to s_pDetachedAreas array */
   hb_threadEnterCriticalSectionGC( &s_waMtx );
   if( s_pDetachedAreas )
   {
      HB_SIZE nLen = hb_arrayLen( s_pDetachedAreas );

      hb_arraySize( pArray, nLen );
      for( HB_SIZE nPos = 1; nPos <= nLen; ++nPos )
      {
         AREAP * pDetachedArea = static_cast<AREAP*>( hb_arrayGetPtrGC( hb_arrayGetItemPtr( s_pDetachedAreas, nPos ), 1, &s_gcWAFuncs ) );
         PHB_DYNS pAlias = static_cast<PHB_DYNS>( ( *pDetachedArea )->atomAlias );
         hb_arraySetC( pArray, nPos, hb_dynsymName( pAlias ) );
      }
   }
   /* leave critical section */
   hb_threadLeaveCriticalSection( &s_waMtx );

   return pArray;
}
