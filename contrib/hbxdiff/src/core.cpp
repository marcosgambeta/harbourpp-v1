/*
 * LIBXDIFF functions wrapper
 *
 * Copyright 2010 Petr Chornyj <myorg63@mail.ru>
 *
 */

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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>
#include <hbinit.hpp>
#include <hbstack.hpp>
#include <hbvm.hpp>

#include "xdiff.h"

#define HB_MMF_SIGN                         8000001

#define HB_ERR_MEMSTRU_NOT_MEM_BLOCK        4001
#define HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK  4002
#define HB_ERR_MEMSTRU_DESTROYED            4003

#define XDLT_STD_BLKSIZE                    (1024 * 8)
#define XDLT_MAX_LINE_SIZE                  80

static PHB_ITEM hb_mmf_itemPut(PHB_ITEM pItem, void * pMemAddr, int iType);
static void *   hb_mmf_itemGet(PHB_ITEM pItem, int iType, HB_BOOL fError);
static void     hb_mmf_ret(void * pMemAddr, int iType);
static void *   hb_mmf_param(int iParam, int iType, HB_BOOL fError);

static void xdiff_init(void);

typedef struct
{
   mmfile_t * mmf;
} HB_MMF, * PHB_MMF;

typedef struct
{
   int      type;
   HB_MMF * hb_mmf;
} HB_MMF_HOLDER, * PHB_MMF_HOLDER;

static HB_GARBAGE_FUNC(hb_mmf_destructor)
{
   auto pStructHolder = static_cast<PHB_MMF_HOLDER>(Cargo);

   if( pStructHolder->hb_mmf )
   {
      if( pStructHolder->hb_mmf->mmf )
      {
         xdl_free_mmfile(pStructHolder->hb_mmf->mmf);
         hb_xfree(pStructHolder->hb_mmf->mmf);

         pStructHolder->hb_mmf->mmf = nullptr;
      }
      hb_xfree(pStructHolder->hb_mmf);
      pStructHolder->hb_mmf = nullptr;
   }
}

static const HB_GC_FUNCS s_gc_xdiffFuncs =
{
   hb_mmf_destructor,
   hb_gcDummyMark
};

static PHB_ITEM hb_mmf_itemPut(PHB_ITEM pItem, void * pMemAddr, int iType)
{
   if( pItem != nullptr )
   {
      if( pItem->isComplex() )
         hb_itemClear(pItem);
   }
   else
      pItem = hb_itemNew(pItem);

   auto pStructHolder = static_cast<PHB_MMF_HOLDER>(hb_gcAllocate(sizeof(HB_MMF_HOLDER), &s_gc_xdiffFuncs));
   pStructHolder->hb_mmf = ( HB_MMF * ) pMemAddr;
   pStructHolder->type = iType;

   return hb_itemPutPtrGC(pItem, pStructHolder);
}

static void * hb_mmf_itemGet(PHB_ITEM pItem, int iType, HB_BOOL fError)
{
   auto pStructHolder = static_cast<PHB_MMF_HOLDER>(hb_itemGetPtrGC(pItem, &s_gc_xdiffFuncs));
   int iError = 0;

   HB_SYMBOL_UNUSED(iError);

   if( !pStructHolder )
      iError = HB_ERR_MEMSTRU_NOT_MEM_BLOCK;
   else if( pStructHolder->type != iType )
      iError = HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK;
   else if( !pStructHolder->hb_mmf )
      iError = HB_ERR_MEMSTRU_DESTROYED;
   else
      return pStructHolder->hb_mmf;

   if( fError )
      hb_errRT_BASE_SubstR(EG_ARG, iError, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);

   return nullptr;
}

static void hb_mmf_ret( void * pMemAddr, int iType )
{
   hb_mmf_itemPut(hb_stackReturnItem(), pMemAddr, iType);
}

static void * hb_mmf_param( int iParam, int iType, HB_BOOL fError )
{
   return hb_mmf_itemGet(hb_param(iParam, Harbour::Item::POINTER), iType, fError);
}

/* int xdl_init_mmfile(mmfile_t *mmf, long bsize, unsigned long flags) */

HB_FUNC(XDL_INIT_MMFILE)
{
   auto mmf = static_cast<mmfile_t*>(hb_xgrab(sizeof(mmfile_t)));

   if( xdl_init_mmfile( mmf, hb_parnldef(1, XDLT_STD_BLKSIZE), static_cast<unsigned long>(hb_parnl(3)) ) == 0 )
   {
      auto phb_mmf = static_cast<HB_MMF*>(hb_xgrab(sizeof(HB_MMF)));
      hb_xmemset(phb_mmf, 0, sizeof(HB_MMF));
      phb_mmf->mmf = mmf;
      hb_mmf_ret(phb_mmf, HB_MMF_SIGN);
   }
   else
      hb_xfree(mmf);
}

#if 0
HB_FUNC(XDL_FREE_MMFILE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
   {
      xdl_free_mmfile(phb_mmf->mmf);
      phb_mmf->mmf = nullptr;
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}
#endif

/* int xdl_mmfile_iscompact(mmfile_t *mmf) */

HB_FUNC(XDL_MMFILE_ISCOMPACT)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
      hb_retl(xdl_mmfile_iscompact(phb_mmf->mmf));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* int xdl_seek_mmfile(mmfile_t *mmf, long off) */

HB_FUNC(XDL_SEEK_MMFILE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
      hb_retni(xdl_seek_mmfile(phb_mmf->mmf, hb_parnldef(2, 0)));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* long xdl_read_mmfile(mmfile_t *mmf, void *data, long size) */

HB_FUNC(XDL_READ_MMFILE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
   {
      PHB_ITEM pData = HB_ISBYREF(2) ? hb_param(2, Harbour::Item::STRING) : nullptr;
      char *   data;
      HB_SIZE  size;

      if( pData )
      {
         if( !hb_itemGetWriteCL( pData, &data, &size ) )
            data = nullptr;
      }
      else
      {
         size = (HB_ISNUM(3) && hb_parns(3) >= 0) ? hb_parns(3) : xdl_mmfile_size(phb_mmf->mmf);

         data = static_cast<char*>(hb_xalloc(size + 1));
      }

      if( data && size )
      {
         long lResult = xdl_read_mmfile(phb_mmf->mmf, data, static_cast<long>(size));

         if( lResult == -1 )
         {
            hb_retc_null();
            hb_stornl(-1, 4);
         }
         else
         {
            hb_stornl(lResult, 4);

            if( pData )
               hb_retclen(data, lResult);
            else
               hb_retclen_buffer(data, lResult);
         }
      }
      else
      {
         hb_retc_null();
         hb_stornl(-1, 4);
      }
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* long xdl_write_mmfile(mmfile_t *mmf, void const *data, long size) */

HB_FUNC(XDL_WRITE_MMFILE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
   {
      if( HB_ISCHAR(2) )
      {
         auto lSize = static_cast<long>(hb_parclen(2));

         if( hb_pcount() > 2 )
            lSize = hb_parnldef(3, lSize);

         hb_retnl(xdl_write_mmfile(phb_mmf->mmf, hb_parcx(2), lSize));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/*
   long xdl_writem_mmfile(mmfile_t *mmf, mmbuffer_t *mb, int nbuf);
   void *xdl_mmfile_writeallocate(mmfile_t *mmf, long size);
   long xdl_mmfile_ptradd(mmfile_t *mmf, char *ptr, long size, unsigned long flags);
   void *xdl_mmfile_first(mmfile_t *mmf, long *size);
   void *xdl_mmfile_next(mmfile_t *mmf, long *size);
 */

/* long xdl_mmfile_size(mmfile_t *mmf) */

HB_FUNC(XDL_MMFILE_SIZE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
      hb_retnl(xdl_mmfile_size(phb_mmf->mmf));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* int xdl_mmfile_cmp(mmfile_t *mmf1, mmfile_t *mmf2) */

HB_FUNC(XDL_MMFILE_CMP)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
      hb_retl(xdl_mmfile_cmp(phb_mmf1->mmf, phb_mmf2->mmf) == 0);
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/*
   int xdl_mmfile_compact(mmfile_t *mmfo, mmfile_t *mmfc, long bsize, unsigned long flags);
 */

HB_FUNC(XDL_MMFILE_COMPACT)
{
   HB_MMF *   phb_mmfo = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   auto mmfc = static_cast<mmfile_t*>(hb_xgrab(sizeof(mmfile_t)));

   if( xdl_mmfile_compact( phb_mmfo->mmf, mmfc, hb_parnldef(1, XDLT_STD_BLKSIZE), static_cast<unsigned long>(hb_parnl(3)) ) == 0 )
   {
      auto phb_mmf = static_cast<HB_MMF*>(hb_xgrab(sizeof(HB_MMF)));
      hb_xmemset(phb_mmf, 0, sizeof(HB_MMF));
      phb_mmf->mmf = mmfc;
      hb_mmf_ret(phb_mmf, HB_MMF_SIGN);

      hb_stornl(0, 4);
   }
   else
   {
      hb_xfree(mmfc);
      hb_stornl(-1, 4);
   }
}

/* callbacks */

#define hb_ptrToHandle(p)   static_cast<HB_FHANDLE>(reinterpret_cast<HB_PTRUINT>(p))
#define hb_parHandlePtr(n)  reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hb_numToHandle(hb_parnint(n))))

static int xdlt_outf( void * priv, mmbuffer_t * mb, int nbuf )
{
   int i;

   for( i = 0; i < nbuf; i++ )
   {
      hb_fsWriteLarge(hb_ptrToHandle(priv), mb[i].ptr, static_cast<HB_SIZE>(mb[i].size));

      if( hb_fsError() != 0 )
         return -1;
   }
   return 0;
}

static int xdlt_outb(void * priv, mmbuffer_t * mb, int nbuf)
{
   auto pCallback = static_cast<PHB_ITEM>(priv);

   if( pCallback && hb_vmRequestReenter() )
   {
      hb_vmPushEvalSym();
      hb_vmPush(pCallback);

      for( auto i = 0; i < nbuf; i++ )
         hb_vmPushString(static_cast<const char*>(mb[i].ptr), mb[i].size);

      hb_vmSend(static_cast<HB_USHORT>(nbuf));
      auto iResult = hb_parnidef(-1, 0);

      hb_vmRequestRestore();

      return iResult;
   }
   else
      return -1;
}

/* int xdl_diff(mmfile_t *mmf1, mmfile_t *mmf2, xpparam_t const *xpp, xdemitconf_t const *xecfg, xdemitcb_t *ecb) */

HB_FUNC(XDL_DIFF)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xpparam_t    xpp;
      xdemitconf_t xecfg;
      xdemitcb_t   ecb;

      xpp.flags    = static_cast<unsigned long>(hb_parnldef(3, 0));
      xecfg.ctxlen = hb_parnldef(4, 3);

      if( HB_ISNUM(5) )
      {
         ecb.priv = hb_parHandlePtr(5);
         ecb.outf = xdlt_outf;

         hb_retni(xdl_diff(phb_mmf1->mmf, phb_mmf2->mmf, &xpp, &xecfg, &ecb));
      }
      else if( HB_ISBLOCK(5) || HB_ISSYMBOL(5) )
      {
         auto pCallback = hb_param(5, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

         ecb.priv = static_cast<void*>(pCallback);
         ecb.outf = xdlt_outb;

         hb_retni(xdl_diff(phb_mmf1->mmf, phb_mmf2->mmf, &xpp, &xecfg, &ecb));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* int xdl_patch(mmfile_t *mmf, mmfile_t *mmfp, int mode, xdemitcb_t *ecb, xdemitcb_t *rjecb) */

HB_FUNC(XDL_PATCH)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      if( HB_ISNUM(4) && HB_ISNUM(5) )
      {
         auto mode = hb_parnidef(3, XDL_PATCH_NORMAL);
         xdemitcb_t ecb;
         xdemitcb_t rjecb;

         ecb.priv = hb_parHandlePtr(4);
         ecb.outf = xdlt_outf;

         rjecb.priv = hb_parHandlePtr(5);
         rjecb.outf = xdlt_outf;

         hb_retni(xdl_patch(phb_mmf1->mmf, phb_mmf2->mmf, mode, &ecb, &rjecb));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/*
   int xdl_merge3(mmfile_t *mmfo, mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb, xdemitcb_t *rjecb);
   int xdl_bdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, bdiffparam_t const *bdp, xdemitcb_t *ecb);
 */

/* int xdl_bdiff(mmfile_t *mmf1, mmfile_t *mmf2, bdiffparam_t const *bdp, xdemitcb_t *ecb) */

HB_FUNC(XDL_BDIFF)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      bdiffparam_t bdp;
      xdemitcb_t   ecb;

      bdp.bsize = hb_parnldef(3, 32); /* from 16 to 64 */

      if( HB_ISNUM(4) )
      {
         ecb.priv = hb_parHandlePtr(4);
         ecb.outf = xdlt_outf;

         hb_retni(xdl_bdiff(phb_mmf1->mmf, phb_mmf2->mmf, &bdp, &ecb));
      }
      else if( HB_ISBLOCK(4) || HB_ISSYMBOL(4) )
      {
         auto pCallback = hb_param(4, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

         ecb.priv = static_cast<void*>(pCallback);
         ecb.outf = xdlt_outb;

         hb_retni(xdl_bdiff(phb_mmf1->mmf, phb_mmf2->mmf, &bdp, &ecb));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/*
   int xdl_rabdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, xdemitcb_t *ecb);
 */

/* int xdl_rabdiff(mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb) */

HB_FUNC(XDL_RABDIFF)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xdemitcb_t ecb;

      if( HB_ISNUM(3) )
      {
         ecb.priv = hb_parHandlePtr(3);
         ecb.outf = xdlt_outf;

         hb_retni(xdl_rabdiff(phb_mmf1->mmf, phb_mmf2->mmf, &ecb));
      }
      else if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
      {
         auto pCallback = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

         ecb.priv = static_cast<void*>(pCallback);
         ecb.outf = xdlt_outb;

         hb_retni(xdl_rabdiff(phb_mmf1->mmf, phb_mmf2->mmf, &ecb));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* int xdl_bpatch(mmfile_t *mmf, mmfile_t *mmfp, xdemitcb_t *ecb) */

HB_FUNC(XDL_BPATCH)
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param(2, HB_MMF_SIGN, true);

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xdemitcb_t ecb;

      if( HB_ISNUM(3) )
      {
         ecb.priv = hb_parHandlePtr(3);
         ecb.outf = xdlt_outf;

         hb_retni(xdl_bpatch(phb_mmf1->mmf, phb_mmf2->mmf, &ecb));
      }
      else if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
      {
         auto pCallback = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

         ecb.priv = static_cast<void*>(pCallback);
         ecb.outf = xdlt_outb;

         hb_retni(xdl_bpatch(phb_mmf1->mmf, phb_mmf2->mmf, &ecb));
      }
      else
         hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* long xdl_bdiff_tgsize(mmfile_t *mmfp) */

HB_FUNC(XDL_BDIFF_TGSIZE)
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param(1, HB_MMF_SIGN, true);

   if( phb_mmf && phb_mmf->mmf )
      hb_retnl(xdl_bdiff_tgsize(phb_mmf->mmf));
   else
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

static void * wf_malloc(void * priv, unsigned int size)
{
   HB_SYMBOL_UNUSED(priv);

   return hb_xgrab(size);
}

static void wf_free( void * priv, void * ptr )
{
   HB_SYMBOL_UNUSED(priv);

   hb_xfree(ptr);
}

static void * wf_realloc(void * priv, void * ptr, unsigned int size)
{
   HB_SYMBOL_UNUSED(priv);

   return hb_xrealloc(ptr, size);
}

static void xdiff_init(void)
{
   memallocator_t malt;

   malt.priv = nullptr;
   malt.malloc = wf_malloc;
   malt.free = wf_free;
   malt.realloc = wf_realloc;
   xdl_set_allocator(&malt);
}

HB_CALL_ON_STARTUP_BEGIN( _xdiff_init_ )
xdiff_init();
HB_CALL_ON_STARTUP_END( _xdiff_init_ )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup _xdiff_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC(_xdiff_init_)
   #include "hbiniseg.hpp"
#endif
