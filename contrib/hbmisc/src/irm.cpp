/*
 * IRM (Independent Record Map) API
 *
 * Released to Public Domain in 2010 by Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 *
 * DISCLAIMER: USE AT YOUR OWN RISK. NO GUARANTEES.
 *
 */

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapirdd.hpp>

typedef struct
{
   HB_BYTE * pBits;
   HB_ULONG  ulSize;       /* in bits */
   HB_ULONG  ulAlloc;      /* in bits */
} HB_IRMMAP, * PHB_IRMMAP;


PHB_IRMMAP hb_irmMapAlloc(HB_ULONG ulSize)
{
   auto pMap = static_cast<PHB_IRMMAP>(hb_xgrab(sizeof(HB_IRMMAP)));

   if( ulSize == 0 )
      ulSize = 256;

   pMap->ulSize  = ulSize;
   pMap->ulAlloc = ( ulSize + 7 ) & ~7UL;
   pMap->pBits   = static_cast<HB_BYTE*>(hb_xgrab(pMap->ulAlloc >> 3));
   memset(pMap->pBits, 0, pMap->ulAlloc >> 3);
   return pMap;
}


void hb_irmMapFree(PHB_IRMMAP pMap)
{
   hb_xfree(pMap->pBits);
   hb_xfree(pMap);
}


int hb_irmMapBitGet(PHB_IRMMAP pMap, HB_ULONG ulRecNo)
{
   --ulRecNo;
   return ( pMap->pBits[ulRecNo >> 3] >> ( ulRecNo & 7 ) ) & 1;
}


void hb_irmMapBitSet(PHB_IRMMAP pMap, HB_ULONG ulRecNo)
{
   --ulRecNo;
   pMap->pBits[ulRecNo >> 3] |= 1 << (ulRecNo & 7);
}


void hb_irmMapBitClear(PHB_IRMMAP pMap, HB_ULONG ulRecNo)
{
   --ulRecNo;
   pMap->pBits[ulRecNo >> 3] &= ~(1 << (ulRecNo & 7));
}


HB_ULONG hb_irmMapNext( PHB_IRMMAP pMap, HB_ULONG ulRecNo )
{
   HB_ULONG ulNext;

   if( ulRecNo >= pMap->ulSize )
      return 0;

   ulNext = ( ulRecNo + 7 ) & ~7UL;
   if( ulNext > pMap->ulSize )
      ulNext = pMap->ulSize;

   /* test the first byte */
   while( ulRecNo < ulNext )
   {
      if( (pMap->pBits[ulRecNo >> 3] >> (ulRecNo & 7)) & 1 )
         return ulRecNo + 1;
      ulRecNo++;
   }

   while( ulRecNo < pMap->ulSize )
   {
      if( pMap->pBits[ulRecNo >> 3] != 0 )
      {
         while( ulRecNo < pMap->ulSize )
         {
            if( (pMap->pBits[ulRecNo >> 3] >> (ulRecNo & 7)) & 1 )
               return ulRecNo + 1;
            ulRecNo++;
         }
         break;
      }
      ulRecNo += 8;
   }
   return 0;
}


HB_ULONG hb_irmMapCount( PHB_IRMMAP pMap )
{
   static const HB_BYTE s_bitcnt[] =
   {
      0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
      4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
   };

   HB_ULONG ul;
   HB_ULONG ulCount;
   HB_ULONG ulSize = pMap->ulSize >> 3;

   ulCount = 0;
   for( ul = 0; ul < ulSize; ++ul )
      ulCount += s_bitcnt[pMap->pBits[ul]];

   return ulCount;
}


static HB_GARBAGE_FUNC(hb_irmMapDestroy)
{
   auto ppMap = static_cast<PHB_IRMMAP*>(Cargo);

   hb_irmMapFree(*ppMap);
}


static const HB_GC_FUNCS s_irmMapFuncs =
{
   hb_irmMapDestroy,
   hb_gcDummyMark
};


static PHB_IRMMAP hb_irmMapParam( int iParam )
{
   auto ppMap = static_cast<PHB_IRMMAP*>(hb_parptrGC(&s_irmMapFuncs, iParam));

   if( ppMap && *ppMap )
      return *ppMap;

   hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   return nullptr;
}


static void hb_irmMapMarkCallback( HB_ULONG ulRecNo, unsigned char * pKey, unsigned short uiLen, void * pMap )
{
   HB_SYMBOL_UNUSED(pKey);
   HB_SYMBOL_UNUSED(uiLen);
   hb_irmMapBitSet(static_cast<PHB_IRMMAP>(pMap), ulRecNo);
}

/*
 *  Expression operators:
 *    "&", expr1, expr2, ...
 *    "|", expr1, expr2, ...
 *  Filter operators:
 *    "=", tag, bag, value                    ordKeyVal() = value
 *    "<=", tag, bag, value                   ordKeyVal() <= value
 *    ">=", tag, bag, value                   ordKeyVal() >= value
 *    "<=<=", tag, bag, value1, value2        value1 <= ordKeyVal() <= value2
 */
PHB_IRMMAP hb_irmExecute(PHB_ITEM pItem)
{
   const char * szOper;

   if( pItem->isArray() && ( szOper = hb_arrayGetCPtr(pItem, 1) ) != nullptr )
   {
      HB_ULONG   ulLen, ulSize;
      PHB_IRMMAP pMap;

      /* Expression operators */
      if( (!strcmp(szOper, "&") || !strcmp(szOper, "|")) && (ulLen = static_cast<HB_ULONG>(hb_arrayLen(pItem))) > 1 )
      {
         HB_ULONG     ul, ul2;

         --ulLen;
         auto pMapArray = static_cast<PHB_IRMMAP*>(hb_xgrab(sizeof(PHB_IRMMAP) * ulLen));
         for( ul = 0; ul < ulLen; ++ul )
            pMapArray[ul] = hb_irmExecute(hb_arrayGetItemPtr(pItem, ul + 2));
         ulSize = ( pMapArray[0]->ulSize + 7 ) >> 3;
         if( !strcmp( szOper, "&" ) )
         {
            for( ul = 0; ul < ulSize; ++ul )
               for( ul2 = 1; ul2 < ulLen; ++ul2 )
                  pMapArray[0]->pBits[ul] &= pMapArray[ul2]->pBits[ul];
         }
         else
         {
            for( ul = 0; ul < ulSize; ++ul )
               for( ul2 = 1; ul2 < ulLen; ++ul2 )
                  pMapArray[0]->pBits[ul] |= pMapArray[ul2]->pBits[ul];
         }
         pMap = pMapArray[0];
         for( ul = 1; ul < ulLen; ++ul )
         {
            hb_irmMapFree(pMapArray[ul]);
         }
         hb_xfree(pMapArray);
         return pMap;
      }
      /* Filter operators */
      else if( (!strcmp(szOper, "=") && (hb_arrayLen(pItem) == 4)) ||
               (!strcmp(szOper, "<=") && (hb_arrayLen(pItem) == 4)) ||
               (!strcmp(szOper, ">=") && (hb_arrayLen(pItem) == 4)) ||
               (!strcmp(szOper, "<=<=") && (hb_arrayLen(pItem) == 5)) )
      {
         auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
         if( pArea != nullptr )
         {
            DBORDERINFO dboi;

            SELF_RECCOUNT(pArea, &ulSize);
            pMap = hb_irmMapAlloc(ulSize);

            dboi.itmOrder    = hb_arrayGetItemPtr(pItem, 2);
            dboi.atomBagName = hb_arrayGetItemPtr(pItem, 3);
            dboi.itmResult   = hb_itemNew(nullptr);
            dboi.itmNewVal   = hb_itemArrayNew(DBRMI_SIZE);
            hb_arraySetPtr(dboi.itmNewVal, DBRMI_FUNCTION, reinterpret_cast<void*>(hb_irmMapMarkCallback));
            hb_arraySetPtr(dboi.itmNewVal, DBRMI_PARAM, static_cast<void*>(pMap));
            if( !strcmp( szOper, "=" ) )
            {
               hb_arraySet(dboi.itmNewVal, DBRMI_LOVAL, hb_arrayGetItemPtr(pItem, 4));
               hb_arraySet(dboi.itmNewVal, DBRMI_HIVAL, hb_arrayGetItemPtr(pItem, 4));
            }
            else if( !strcmp( szOper, "<=" ) )
            {
               hb_arraySet(dboi.itmNewVal, DBRMI_HIVAL, hb_arrayGetItemPtr(pItem, 4));
            }
            else if( !strcmp( szOper, ">=" ) )
            {
               hb_arraySet(dboi.itmNewVal, DBRMI_LOVAL, hb_arrayGetItemPtr(pItem, 4));
            }
            else if( !strcmp( szOper, "<=<=" ) )
            {
               hb_arraySet(dboi.itmNewVal, DBRMI_LOVAL, hb_arrayGetItemPtr(pItem, 4));
               hb_arraySet(dboi.itmNewVal, DBRMI_HIVAL, hb_arrayGetItemPtr(pItem, 5));
            }
            SELF_ORDINFO(pArea, DBOI_SCOPEEVAL, &dboi);
            #if 0
            bitcount ulSize = hb_itemGetNL(dboi.itmResult);
            #endif
            hb_itemRelease(dboi.itmNewVal);
            hb_itemRelease(dboi.itmResult);
            return pMap;
         }
         else
            hb_errRT_BASE_SubstR(EG_NOTABLE, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);

   return nullptr;
}


/* irmExecute( aFilterTree ) --> pMap */
HB_FUNC(IRMEXECUTE)
{
   PHB_IRMMAP pMap = hb_irmExecute(hb_param(1, Harbour::Item::ANY));

   if( pMap )
   {
      auto ppMap = static_cast<PHB_IRMMAP*>(hb_gcAllocate(sizeof(PHB_IRMMAP), &s_irmMapFuncs));
      *ppMap = pMap;
      hb_retptrGC(ppMap);
   }
}

/*
 * ulRecNo := 0
 * DO WHILE irmMapNext( pMap, @ulRecNo )
 *    dbGoto( nRecNo )
 *    ...
 * ENDDO
 */
HB_FUNC(IRMMAPNEXT)
{
   PHB_IRMMAP pMap = hb_irmMapParam(1);

   if( pMap )
   {
      HB_ULONG ulRecNo = hb_parnl(2);
      ulRecNo = hb_irmMapNext(pMap, ulRecNo);
      hb_stornl(ulRecNo, 2);
      hb_retl(ulRecNo != 0);
   }
}


/*
 * ulRecNo := 0
 * DO WHILE irmMapSkip( pMap, @ulRecNo )
 *    ...
 * ENDDO
 */
HB_FUNC(IRMMAPSKIP)
{
   PHB_IRMMAP pMap = hb_irmMapParam(1);

   if( pMap )
   {
      HB_ULONG ulRecNo = hb_parnl(2);
      ulRecNo = hb_irmMapNext(pMap, ulRecNo);
      hb_stornl(ulRecNo, 2);
      if( ulRecNo != 0 )
         hb_retl(SELF_GOTO(static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer()), ulRecNo) == Harbour::SUCCESS);
      else
         hb_retl(false);
   }
}


/* irmMapCount( pMap ) --> nRecCount */
HB_FUNC(IRMMAPCOUNT)
{
   PHB_IRMMAP pMap = hb_irmMapParam(1);

   if( pMap )
      hb_retnl(hb_irmMapCount(pMap));
}
