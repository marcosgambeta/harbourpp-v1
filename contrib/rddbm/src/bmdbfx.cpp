/*
 * Alternative BMDBF* implementation which respects RDD inheritance
 * scheme and gives similar functionality and PRG functions as modified
 * by Miguel Angel Marchuet <miguelangel@marchuet.net> DBFCDX with
 * directly hardcoded bitmap filters.
 * This code is completely new implementation and does not contain
 * any code created by Miguel.
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapirdd.hpp"
#include "hbapierr.hpp"
#include "hbdbferr.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"
#include "hbinit.hpp"
#include "rddsys.ch"


/* now this function is RDD independent and can work with any RDD supporting
 * DBOI_SKIPWILD and DBOI_SKIPWILDBACK
 */
HB_FUNC( BM_DBSEEKWILD )
{
   auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

   if( pArea != nullptr )
   {
      auto szPattern = hb_parc(1);

      if( szPattern )
      {
         HB_BOOL fSoft, fBack, fCont, fAll, fFound;
         DBORDERINFO OrderInfo;
         HB_ERRCODE errCode;
         PHB_ITEM pArray = nullptr;
         int iOrder = 0;

         fSoft  = hb_parldef(2, hb_setGetSoftSeek());
         fBack  = hb_parl(3);
         fCont  = hb_parl(4);
         fAll   = hb_parl(5);
         fFound = false;

         if( fAll )
         {
            pArray = hb_itemArrayNew(0);
            fCont = false;
         }

         memset(&OrderInfo, 0, sizeof(OrderInfo));
         OrderInfo.itmResult = hb_itemNew(nullptr);

         errCode = SELF_ORDINFO(pArea, DBOI_NUMBER, &OrderInfo);
         if( errCode == Harbour::SUCCESS )
            iOrder = hb_itemGetNI(OrderInfo.itmResult);

         if( iOrder != 0 )
         {
            HB_BOOL fUnlock;
            OrderInfo.itmNewVal = OrderInfo.itmResult;
            hb_itemPutL(OrderInfo.itmNewVal, true);
            if( SELF_ORDINFO(pArea, DBOI_READLOCK, &OrderInfo) == Harbour::SUCCESS )
               fUnlock = hb_itemGetL(OrderInfo.itmResult);
            else
               fUnlock = false;
            OrderInfo.itmNewVal = nullptr;

            if( !fCont )
            {
               if( fBack )
                  errCode = SELF_GOBOTTOM(pArea);
               else
                  errCode = SELF_GOTOP(pArea);

               if( errCode == Harbour::SUCCESS )
               {
                  errCode = SELF_ORDINFO(pArea, DBOI_KEYVAL, &OrderInfo);
                  if( errCode == Harbour::SUCCESS )
                     fFound = hb_strMatchWild( hb_itemGetCPtr(OrderInfo.itmResult), szPattern );
               }
            }

            if( !fFound && errCode == Harbour::SUCCESS )
            {
               OrderInfo.itmNewVal = hb_param(1, Harbour::Item::STRING);
               errCode = SELF_ORDINFO(pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD, &OrderInfo);
               if( errCode == Harbour::SUCCESS )
                  fFound = hb_itemGetL(OrderInfo.itmResult);
            }

            if( fAll && errCode == Harbour::SUCCESS )
            {
               OrderInfo.itmNewVal = hb_param(1, Harbour::Item::STRING);
               do
               {
                  errCode = SELF_RECID(pArea, OrderInfo.itmResult);
                  if( errCode != Harbour::SUCCESS )
                     break;
                  hb_arrayAddForward( pArray, OrderInfo.itmResult );
                  errCode = SELF_ORDINFO(pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD, &OrderInfo);
                  if( errCode == Harbour::SUCCESS )
                     fFound = hb_itemGetL(OrderInfo.itmResult);
                  else
                     fFound = false;
               }
               while( fFound );
            }
            if( fUnlock )
            {
               OrderInfo.itmNewVal = OrderInfo.itmResult;
               hb_itemPutL(OrderInfo.itmNewVal, false);
               SELF_ORDINFO(pArea, DBOI_READLOCK, &OrderInfo);
            }
         }

         hb_itemRelease(OrderInfo.itmResult);

         if( !fFound && !fSoft && errCode == Harbour::SUCCESS )
            SELF_GOTO(pArea, 0);

         if( pArray )
            hb_itemReturnRelease(pArray);
         else
            hb_retl(fFound);
      }
      else
         hb_errRT_DBCMD(EG_ARG, EDBCMD_SEEK_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
   }
   else
      hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
}

HB_FUNC( BM_TURBO )
{
   hb_retl(false);
}


typedef struct
{
   HB_U32 maxrec;
   HB_U32 map[1];
} BM_FILTER, * PBM_FILTER;

#define BM_GETFILTER( p )  ( ( PBM_FILTER ) ( p )->dbfi.lpvCargo )
#define BM_ITEMSIZE( n )   ( ( ( n ) + 31 ) >> 5 )
#define BM_BYTESIZE( n )   ( ( ( ( n ) + 31 ) >> 5 ) * sizeof(HB_U32) )

#define BM_SETREC( p, r )  \
   do { if( (r) > 0 && (r) <= (p)->maxrec ) \
           ( p )->map[( ( r ) - 1 ) >> 5] |= ( 1 << ( ( ( r ) - 1 ) & 0x1f ) ); \
   } while( 0 )

#define BM_CLRREC( p, r )  \
   do { if( (r) > 0 && (r) <= (p)->maxrec ) \
           ( p )->map[( ( r ) - 1 ) >> 5] &= ~( 1 << ( ( ( r ) - 1 ) & 0x1f ) ); \
   } while( 0 )

#define BM_GETREC( p, r )  ( ( ( r ) > 0 && ( r ) <= ( p )->maxrec ) && \
                             ( ( p )->map[( ( r ) - 1 ) >> 5] & ( 1 << ( ( ( r ) - 1 ) & 0x1f ) ) ) != 0 )

#define SUPERTABLE  ( hb_bmGetRdd( pArea->rddID ) )

#define BM_RDD_MAX  8

static HB_USHORT s_uiRdds[BM_RDD_MAX];
static int s_iRddCount = 0;

static void hb_bmSetRdd( HB_USHORT uiRddId )
{
   if( s_iRddCount < BM_RDD_MAX )
      s_uiRdds[s_iRddCount++] = uiRddId;
}

static const RDDFUNCS * hb_bmGetRdd( HB_USHORT uiRddId )
{
   int i;

   for( i = 0; i < s_iRddCount; ++i )
   {
      if( hb_rddIsDerivedFrom( uiRddId, s_uiRdds[i] ) )
         return &( hb_rddGetNode( s_uiRdds[i] )->pSuperTable );
   }
   return nullptr;
}

static void hb_bmResetFilterOpt(AREAP pArea)
{
   DBORDERINFO OrderInfo;

   memset(&OrderInfo, 0, sizeof(OrderInfo));
   SELF_ORDINFO(pArea, DBOI_RESETPOS, &OrderInfo);
   if( OrderInfo.itmResult )
      hb_itemRelease(OrderInfo.itmResult);
}

static HB_BOOL hb_bmCheckRecordFilter(AREAP pArea, HB_ULONG ulRecNo)
{
   HB_BOOL lResult = false;
   HB_BOOL fDeleted = hb_setGetDeleted();

   if( pArea->dbfi.itmCobExpr || fDeleted )
   {
      HB_ULONG ulRec;

      if( SELF_RECNO(pArea, &ulRec) == Harbour::SUCCESS )
      {
         if( ulRec != ulRecNo )
            SELF_GOTO(pArea, ulRecNo);

         if( fDeleted )
            SELF_DELETED(pArea, &lResult);

         if( !lResult && pArea->dbfi.itmCobExpr )
         {
            PHB_ITEM pResult = hb_vmEvalBlock(pArea->dbfi.itmCobExpr);
            lResult = HB_IS_LOGICAL(pResult) && !hb_itemGetL(pResult);
         }
      }
   }
   return !lResult;
}

static AREAP hb_bmGetCurrentWorkArea(void)
{
   auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

   if( !pArea )
      hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
   else if( hb_bmGetRdd( pArea->rddID ) == nullptr )
   {
      hb_errRT_DBCMD(EG_UNSUPPORTED, EDBF_UNSUPPORTED, nullptr, HB_ERR_FUNCNAME);
      pArea = nullptr;
   }

   return pArea;
}

static PHB_ITEM hb_bmGetArrayParam( int iParam )
{
   auto pArray = hb_param(iParam, Harbour::Item::ARRAY);

   if( !pArray )
      hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);

   return pArray;
}

static PBM_FILTER hb_bmCreate(AREAP pArea, HB_BOOL fFull)
{
   PBM_FILTER pBM = nullptr;
   HB_ULONG ulRecCount;

   if( SELF_RECCOUNT(pArea, &ulRecCount) == Harbour::SUCCESS )
   {
      HB_SIZE nSize = sizeof(BM_FILTER) + BM_BYTESIZE( ulRecCount );
      pBM = static_cast<PBM_FILTER>(memset(hb_xgrab(nSize), fFull ? 0xFF : 0x00, nSize));
      pBM->maxrec = static_cast<HB_U32>(ulRecCount);
   }
   return pBM;
}

HB_FUNC( BM_DBGETFILTERARRAY )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea != nullptr )
   {
      PBM_FILTER pBM = BM_GETFILTER(pArea);
      auto pArray = hb_itemArrayNew(0);

      if( pBM && pArea->dbfi.fOptimized )
      {
         HB_ULONG ulItems = BM_ITEMSIZE( pBM->maxrec );
         HB_ULONG ulRecNo;

         if( SELF_RECNO(pArea, &ulRecNo) == Harbour::SUCCESS )
         {
            auto pItem = hb_itemNew(nullptr);
            HB_ULONG ul;

            for( ul = 0; ul < ulItems; ul++ )
            {
               if( pBM->map[ul] )
               {
                  HB_U32 nBits = pBM->map[ul];
                  HB_ULONG ulRec = ul << 5;

                  do
                  {
                     ++ulRec;
                     if( nBits & 1 )
                     {
                        if( hb_bmCheckRecordFilter(pArea, ulRec) )
                           hb_arrayAddForward( pArray, hb_itemPutNL(pItem, ulRec) );
                     }
                     nBits >>= 1;
                  }
                  while( nBits );
               }
            }
            hb_itemRelease(pItem);

            SELF_GOTO(pArea, ulRecNo);
         }
      }
      hb_itemReturnRelease(pArray);
   }
}

HB_FUNC( BM_DBSETFILTERARRAY )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea != nullptr )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam(1);

      if( pArray )
      {
         if( SELF_CLEARFILTER(pArea) == Harbour::SUCCESS )
         {
            PBM_FILTER pBM = hb_bmCreate(pArea, false);

            if( pBM )
            {
               HB_SIZE nPos;

               pArea->dbfi.lpvCargo   = pBM;
               pArea->dbfi.fOptimized = true;
               pArea->dbfi.fFilter    = true;

               for( nPos = hb_arrayLen(pArray); nPos; nPos-- )
               {
                  HB_ULONG ulRec = static_cast<HB_ULONG>(hb_arrayGetNL(pArray, nPos));
                  BM_SETREC( pBM, ulRec );
               }
            }
         }
      }
   }
}

HB_FUNC( BM_DBSETFILTERARRAYADD )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea && pArea->dbfi.fOptimized )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam(1);

      if( pArray )
      {
         PBM_FILTER pBM = BM_GETFILTER(pArea);

         if( pBM )
         {
            HB_SIZE nPos;

            for( nPos = hb_arrayLen(pArray); nPos; nPos-- )
            {
               HB_ULONG ulRec = static_cast<HB_ULONG>(hb_arrayGetNL(pArray, nPos));
               BM_SETREC( pBM, ulRec );
            }
            hb_bmResetFilterOpt(pArea);
         }
      }
   }
}

HB_FUNC( BM_DBSETFILTERARRAYDEL )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea && pArea->dbfi.fOptimized )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam(1);

      if( pArray )
      {
         PBM_FILTER pBM = BM_GETFILTER(pArea);

         if( pBM )
         {
            HB_SIZE nPos;

            for( nPos = hb_arrayLen(pArray); nPos; nPos-- )
            {
               HB_ULONG ulRec = static_cast<HB_ULONG>(hb_arrayGetNL(pArray, nPos));
               BM_CLRREC( pBM, ulRec );
            }
            hb_bmResetFilterOpt(pArea);
         }
      }
   }
}


static HB_BOOL hb_bmEvalFilter(AREAP pArea, HB_BOOL fUpdate)
{
   PBM_FILTER pBM = BM_GETFILTER(pArea);
   HB_BOOL fResult = true;
   HB_ULONG ulRecNo = 0;

   if( pBM )
   {
      SELF_RECNO(pArea, &ulRecNo);
      if( !fUpdate && !BM_GETREC( pBM, ulRecNo ) )
         return false;
   }

   if( pArea->dbfi.itmCobExpr )
   {
      PHB_ITEM pResult = hb_vmEvalBlock(pArea->dbfi.itmCobExpr);
      fResult = !HB_IS_LOGICAL(pResult) || hb_itemGetL(pResult);
   }
   if( fResult && hb_setGetDeleted() )
   {
      SELF_DELETED(pArea, &fResult);
      fResult = !fResult;
   }

   if( pBM )
   {
      if( ulRecNo > pBM->maxrec && fResult )
      {
         HB_SIZE nSize = sizeof(BM_FILTER) + BM_BYTESIZE( ulRecNo ),
                 nOldSize = sizeof(BM_FILTER) + BM_BYTESIZE( pBM->maxrec );
         if( nSize > nOldSize )
         {
            pArea->dbfi.lpvCargo = pBM = static_cast<PBM_FILTER>(hb_xrealloc(pBM, nSize));
            memset(reinterpret_cast<HB_BYTE*>(pBM) + nOldSize, 0xFF, nSize - nOldSize);
         }
         pBM->maxrec = static_cast<HB_U32>(ulRecNo);
      }
      if( fResult )
         BM_SETREC( pBM, ulRecNo );
      else
         BM_CLRREC( pBM, ulRecNo );
   }
   return fResult;
}

static HB_ERRCODE hb_bmSkipFilter(AREAP pArea, HB_LONG lUpDown)
{
   HB_BOOL fBottom;
   HB_ERRCODE errCode;

   if( !hb_setGetDeleted() && pArea->dbfi.itmCobExpr == nullptr && !BM_GETFILTER(pArea) )
      return Harbour::SUCCESS;

   lUpDown = ( lUpDown < 0  ? -1 : 1 );
   fBottom = pArea->fBottom;
   while( !pArea->fBof && !pArea->fEof && !hb_bmEvalFilter(pArea, false) )
   {
      errCode = SELF_SKIPRAW(pArea, lUpDown);
      if( errCode != Harbour::SUCCESS )
         return errCode;
   }

   if( pArea->fBof && lUpDown < 0 )
   {
      if( fBottom )
         errCode = SELF_GOTO(pArea, 0);
      else
      {
         errCode = SELF_GOTOP(pArea);
         pArea->fBof = true;
      }
   }
   else
      errCode = Harbour::SUCCESS;

   return errCode;
}

static HB_ERRCODE hb_bmPutRec(AREAP pArea, const HB_BYTE * pBuffer)
{
   HB_ERRCODE errCode = SUPER_PUTREC(pArea, pBuffer);

   if( pBuffer == nullptr && errCode == Harbour::SUCCESS && BM_GETFILTER(pArea) )
      hb_bmEvalFilter(pArea, true);

   return errCode;
}

static HB_ERRCODE hb_bmCountScope(AREAP pArea, void * pPtr, HB_LONG * plRec)
{
   if( pPtr == nullptr )
   {
      PBM_FILTER pBM = BM_GETFILTER(pArea);

      if( pBM && pArea->dbfi.fFilter && !BM_GETREC( pBM, static_cast<HB_ULONG>(*plRec)) )
         *plRec = 0;

      return Harbour::SUCCESS;
   }
   return SUPER_COUNTSCOPE(pArea, pPtr, plRec);
}

static HB_ERRCODE hb_bmClearFilter(AREAP pArea)
{
   HB_ERRCODE errCode = SUPER_CLEARFILTER(pArea);

   if( pArea->dbfi.lpvCargo )
   {
      hb_xfree(pArea->dbfi.lpvCargo);
      pArea->dbfi.lpvCargo = nullptr;
   }

   return errCode;
}

static HB_ERRCODE hb_bmSetFilter(AREAP pArea, LPDBFILTERINFO pFilterInfo)
{
   HB_ERRCODE errCode = SUPER_SETFILTER(pArea, pFilterInfo);

   if( errCode == Harbour::SUCCESS )
   {
      if( hb_setGetOptimize() )
      {
         PBM_FILTER pBM = hb_bmCreate(pArea, true);

         if( pBM )
         {
            pArea->dbfi.lpvCargo   = pBM;
            pArea->dbfi.fOptimized = true;
            pArea->dbfi.fFilter    = true;

            if( hb_setGetForceOpt() )
            {
               HB_ULONG ulRecNo;

               if( SELF_RECNO(pArea, &ulRecNo) == Harbour::SUCCESS )
               {
                  HB_ULONG ulRec;

                  for( ulRec = 1; ulRec <= pBM->maxrec; ulRec++ )
                  {
                     SELF_GOTO(pArea, ulRec);
                     hb_bmEvalFilter(pArea, true);
                  }
                  SELF_GOTO(pArea, ulRecNo);
               }
            }
         }
      }
   }
   return errCode;
}

static const RDDFUNCS bmTable =
{
   /* Movement and positioning methods */
   ( DBENTRYP_BP )    nullptr,              /* Bof        */
   ( DBENTRYP_BP )    nullptr,              /* Eof        */
   ( DBENTRYP_BP )    nullptr,              /* Found      */
   ( DBENTRYP_V )     nullptr,              /* GoBottom   */
   ( DBENTRYP_UL )    nullptr,              /* GoTo       */
   ( DBENTRYP_I )     nullptr,              /* GoToId     */
   ( DBENTRYP_V )     nullptr,              /* GoTop      */
   ( DBENTRYP_BIB )   nullptr,              /* Seek       */
   ( DBENTRYP_L )     nullptr,              /* Skip       */
   ( DBENTRYP_L )     hb_bmSkipFilter,   /* SkipFilter */
   ( DBENTRYP_L )     nullptr,              /* SkipRaw    */

   /* Data management */
   ( DBENTRYP_VF )    nullptr,              /* AddField       */
   ( DBENTRYP_B )     nullptr,              /* Append         */
   ( DBENTRYP_I )     nullptr,              /* CreateFields   */
   ( DBENTRYP_V )     nullptr,              /* DeleteRec      */
   ( DBENTRYP_BP )    nullptr,              /* Deleted        */
   ( DBENTRYP_SP )    nullptr,              /* FieldCount     */
   ( DBENTRYP_VF )    nullptr,              /* FieldDisplay   */
   ( DBENTRYP_SSI )   nullptr,              /* FieldInfo      */
   ( DBENTRYP_SCP )   nullptr,              /* FieldName      */
   ( DBENTRYP_V )     nullptr,              /* Flush          */
   ( DBENTRYP_PP )    nullptr,              /* GetRec         */
   ( DBENTRYP_SI )    nullptr,              /* GetValue       */
   ( DBENTRYP_SVL )   nullptr,              /* GetVarLen      */
   ( DBENTRYP_V )     nullptr,              /* GoCold         */
   ( DBENTRYP_V )     nullptr,              /* GoHot          */
   ( DBENTRYP_P )     hb_bmPutRec,       /* PutRec         */
   ( DBENTRYP_SI )    nullptr,              /* PutValue       */
   ( DBENTRYP_V )     nullptr,              /* Recall         */
   ( DBENTRYP_ULP )   nullptr,              /* RecCount       */
   ( DBENTRYP_ISI )   nullptr,              /* RecInfo        */
   ( DBENTRYP_ULP )   nullptr,              /* RecNo          */
   ( DBENTRYP_I )     nullptr,              /* RecId          */
   ( DBENTRYP_S )     nullptr,              /* SetFieldExtent */

   /* WorkArea/Database management */
   ( DBENTRYP_CP )    nullptr,              /* Alias       */
   ( DBENTRYP_V )     nullptr,              /* Close       */
   ( DBENTRYP_VO )    nullptr,              /* Create      */
   ( DBENTRYP_SI )    nullptr,              /* Info        */
   ( DBENTRYP_V )     nullptr,              /* NewArea     */
   ( DBENTRYP_VO )    nullptr,              /* Open        */
   ( DBENTRYP_V )     nullptr,              /* Release     */
   ( DBENTRYP_SP )    nullptr,              /* StructSize  */
   ( DBENTRYP_CP )    nullptr,              /* SysName     */
   ( DBENTRYP_VEI )   nullptr,              /* Eval        */
   ( DBENTRYP_V )     nullptr,              /* Pack        */
   ( DBENTRYP_LSP )   nullptr,              /* PackRec     */
   ( DBENTRYP_VS )    nullptr,              /* Sort        */
   ( DBENTRYP_VT )    nullptr,              /* Trans       */
   ( DBENTRYP_VT )    nullptr,              /* TransRec    */
   ( DBENTRYP_V )     nullptr,              /* Zap         */

   /* Relational Methods */
   ( DBENTRYP_VR )    nullptr,              /* ChildEnd      */
   ( DBENTRYP_VR )    nullptr,              /* ChildStart    */
   ( DBENTRYP_VR )    nullptr,              /* ChildSync     */
   ( DBENTRYP_V )     nullptr,              /* SyncChildren  */
   ( DBENTRYP_V )     nullptr,              /* ClearRel      */
   ( DBENTRYP_V )     nullptr,              /* ForceRel      */
   ( DBENTRYP_SSP )   nullptr,              /* RelArea       */
   ( DBENTRYP_VR )    nullptr,              /* RelEval       */
   ( DBENTRYP_SI )    nullptr,              /* RelText       */
   ( DBENTRYP_VR )    nullptr,              /* SetRel        */

   /* Order Management */
   ( DBENTRYP_VOI )   nullptr,              /* OrderListAdd      */
   ( DBENTRYP_V )     nullptr,              /* OrderListClear    */
   ( DBENTRYP_VOI )   nullptr,              /* OrderListDelete   */
   ( DBENTRYP_VOI )   nullptr,              /* OrderListFocus    */
   ( DBENTRYP_V )     nullptr,              /* OrderListRebuild  */
   ( DBENTRYP_VOO )   nullptr,              /* OrderCondition    */
   ( DBENTRYP_VOC )   nullptr,              /* OrderCreate       */
   ( DBENTRYP_VOI )   nullptr,              /* OrderDestroy      */
   ( DBENTRYP_SVOI )  nullptr,              /* OrderInfo         */

   /* Filters and Scope Settings */
   ( DBENTRYP_V )     hb_bmClearFilter,  /* ClearFilter  */
   ( DBENTRYP_V )     nullptr,              /* ClearLocate  */
   ( DBENTRYP_V )     nullptr,              /* ClearScope   */
   ( DBENTRYP_VPLP )  hb_bmCountScope,   /* CountScope   */
   ( DBENTRYP_I )     nullptr,              /* FilterText   */
   ( DBENTRYP_SI )    nullptr,              /* ScopeInfo    */
   ( DBENTRYP_VFI )   hb_bmSetFilter,    /* SetFilter    */
   ( DBENTRYP_VLO )   nullptr,              /* SetLocate    */
   ( DBENTRYP_VOS )   nullptr,              /* SetScope     */
   ( DBENTRYP_VPL )   nullptr,              /* SkipScope    */
   ( DBENTRYP_B )     nullptr,              /* Locate       */

   /* Miscellaneous */
   ( DBENTRYP_CC )    nullptr,              /* Compile    */
   ( DBENTRYP_I )     nullptr,              /* Error      */
   ( DBENTRYP_I )     nullptr,              /* EvalBlock  */

   /* Network operations */
   ( DBENTRYP_VSP )   nullptr,              /* RawLock  */
   ( DBENTRYP_VL )    nullptr,              /* Lock     */
   ( DBENTRYP_I )     nullptr,              /* UnLock   */

   /* Memofile functions */
   ( DBENTRYP_V )     nullptr,              /* CloseMemFile   */
   ( DBENTRYP_VO )    nullptr,              /* CreateMemFile  */
   ( DBENTRYP_SCCS )  nullptr,              /* GetValueFile   */
   ( DBENTRYP_VO )    nullptr,              /* OpenMemFile    */
   ( DBENTRYP_SCCS )  nullptr,              /* PutValueFile   */

   /* Database file header handling */
   ( DBENTRYP_V )     nullptr,              /* ReadDBHeader   */
   ( DBENTRYP_V )     nullptr,              /* WriteDBHeader  */

   /* non WorkArea functions */
   ( DBENTRYP_R )     nullptr,              /* Init    */
   ( DBENTRYP_R )     nullptr,              /* Exit    */
   ( DBENTRYP_RVVL )  nullptr,              /* Drop    */
   ( DBENTRYP_RVVL )  nullptr,              /* Exists  */
   ( DBENTRYP_RVVVL ) nullptr,              /* Rename  */
   ( DBENTRYP_RSLV )  nullptr,              /* RddInfo */

   /* Special and reserved methods */
   ( DBENTRYP_SVP )   nullptr               /* WhoCares */
};


static void hb_bmGetFuncTable( const char * szSuper )
{
   RDDFUNCS * pTable, * pSuperTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = static_cast<HB_USHORT*>(hb_parptr(1));
   pTable = static_cast<RDDFUNCS*>(hb_parptr(2));
   pSuperTable = static_cast<RDDFUNCS*>(hb_parptr(3));
   uiRddId = static_cast<HB_USHORT>(hb_parni(4));
   puiSuperRddId = static_cast<HB_USHORT*>(hb_parptr(5));

#if 0
   HB_TRACE( HB_TR_DEBUG, ( "BM%s_GETFUNCTABLE(%p, %p, %p, %hu, %p)", szSuper, static_cast<void*>(puiCount), pTable, pSuperTable, uiRddId, puiSuperRddId ) );
#endif

   if( puiCount && pTable && pSuperTable && puiSuperRddId )
   {
      HB_ERRCODE errCode;

      *puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &bmTable, pSuperTable, szSuper, puiSuperRddId );
      if( errCode == Harbour::SUCCESS )
         hb_bmSetRdd( uiRddId );
      hb_retni(errCode);
   }
   else
      hb_retni(Harbour::FAILURE);
}

static void hb_bmRddInit( void * cargo )
{
   HB_BOOL fError;

   HB_SYMBOL_UNUSED(cargo);

   fError = hb_rddRegister( "DBF", RDT_FULL ) > 1;
   if( !fError )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "DBFCDX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFCDX", RDT_FULL ) > 1 )
            fError = true;
      }
      if( !fError && hb_rddRegister( "DBFNTX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFNTX", RDT_FULL ) > 1 )
            fError = true;
      }
      if( !fError && hb_rddRegister( "DBFNSX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFNSX", RDT_FULL ) > 1 )
            fError = true;
      }
   }

   if( fError )
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
}

HB_FUNC( _BMDBF ) { ; }
HB_FUNC_STATIC( BMDBFCDX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFCDX" ); }
HB_FUNC_STATIC( BMDBFNTX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFNTX" ); }
HB_FUNC_STATIC( BMDBFNSX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFNSX" ); }

HB_INIT_SYMBOLS_BEGIN( _hb_bm_InitSymbols_ )
{ "BMDBFCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFCDX_GETFUNCTABLE )}, nullptr },
{ "BMDBFNTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFNTX_GETFUNCTABLE )}, nullptr },
{ "BMDBFNSX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFNSX_GETFUNCTABLE )}, nullptr }
HB_INIT_SYMBOLS_END( _hb_bm_InitSymbols_ )

HB_CALL_ON_STARTUP_BEGIN( _hb_bm_rdd_init_ )
   hb_vmAtInit(hb_bmRddInit, nullptr);
HB_CALL_ON_STARTUP_END( _hb_bm_rdd_init_ )

#if defined(HB_PRAGMA_STARTUP)
#  pragma startup _hb_bm_InitSymbols_
#  pragma startup _hb_bm_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_bm_InitSymbols_ ) \
                              HB_DATASEG_FUNC( _hb_bm_rdd_init_ )
   #include "hbiniseg.hpp"
#endif
