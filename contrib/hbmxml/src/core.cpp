/*
 * MINIXML functions wrapper
 *
 * Copyright 2010-2011 Petr Chornyj <myorg63@mail.ru>
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

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>
#include <hbapistr.hpp>
#include <hbstack.hpp>
#include <hbvm.hpp>

#include "mxml.h"
#include "config.h"

#define MXML_ERR_ARGS()  (hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS))

struct HBMXML_NODE /* placeholder for mxml_node_t */
{
   mxml_node_t * node;
};

/* callback function support */

struct HB_CBS_VAR
{
   PHB_ITEM type_cb;
   PHB_ITEM save_cb;
   PHB_ITEM sax_cb;
   void * hText;
};

struct HB_CUSTOM_CBS_VAR
{
   PHB_ITEM load_cb;
   PHB_ITEM save_cb;
};

struct HB_ERROR_CB_VAR
{
   PHB_ITEM error_cb;
};

static void hb_cbs_var_init(void * cargo)
{
   auto pCbs = static_cast<HB_CBS_VAR *>(cargo);

   pCbs->type_cb = nullptr;
   pCbs->save_cb = nullptr;
   pCbs->sax_cb = nullptr;
}

static void hb_custom_cbs_var_init(void * cargo)
{
   auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(cargo);

   pCCbs->load_cb = nullptr;
   pCCbs->save_cb = nullptr;
}

static void hb_custom_cbs_var_release(void * cargo)
{
   auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(cargo);

   if( pCCbs->load_cb )
   {
      hb_itemRelease(pCCbs->load_cb);
   }
   if( pCCbs->save_cb )
   {
      hb_itemRelease(pCCbs->save_cb);
   }
}

static void hb_error_cb_var_init(void * cargo)
{
   auto pError_cb = static_cast<HB_ERROR_CB_VAR *>(cargo);

   pError_cb->error_cb = nullptr;
}

static void hb_error_cb_var_release(void * cargo)
{
   auto pError_cb = static_cast<HB_ERROR_CB_VAR *>(cargo);

   if( pError_cb->error_cb )
   {
      hb_itemRelease(pError_cb->error_cb);
   }
}

static HB_TSD_NEW(s_cbs_var, sizeof(HB_CBS_VAR), hb_cbs_var_init, nullptr);
static HB_TSD_NEW(s_custom_cbs_var, sizeof(HB_CUSTOM_CBS_VAR), hb_custom_cbs_var_init, hb_custom_cbs_var_release);
static HB_TSD_NEW(s_error_cb_var, sizeof(HB_ERROR_CB_VAR), hb_error_cb_var_init, hb_error_cb_var_release);

/* mxml_node_t * support */

static void hbmxml_release(mxml_node_t * node)
{
   if( mxmlGetRefCount(node) <= (mxmlGetParent(node) ? 2 : 1) )
   {
      void * user_data;

      /* remove userData when last HVM item with given node is cleared
       * It's not nice solution because node can still exists and can
       * be extracted yet but there is not destructor for userData
       * in MXML library so we have no choice :-( [druzus]
       */
      if( (user_data = mxmlGetUserData(node)) != nullptr )
      {
         mxmlSetUserData(node, nullptr);
         hb_itemRelease(static_cast<PHB_ITEM>(user_data));
      }
   }

   mxmlRelease(node);
}

static HB_GARBAGE_FUNC(hbmxml_nodeDestructor)
{
   auto pHbnode = static_cast<HBMXML_NODE *>(Cargo);

   if( pHbnode->node )
   {
      hbmxml_release(pHbnode->node);
      pHbnode->node = nullptr;
   }
}

static const HB_GC_FUNCS s_gc_mxml_nodeFuncs =
{
   hbmxml_nodeDestructor,
   hb_gcDummyMark
};

static mxml_node_t * mxml_node_param(int iParam)
{
   auto pHbnode = static_cast<HBMXML_NODE*>(hb_parptrGC(&s_gc_mxml_nodeFuncs, iParam));

   return (pHbnode && pHbnode->node) ? pHbnode->node : nullptr;
}

static HBMXML_NODE * mxml_node_new(mxml_node_t * node, int iNew)
{
   auto pHbnode = static_cast<HBMXML_NODE*>(hb_gcAllocate(sizeof(HBMXML_NODE), &s_gc_mxml_nodeFuncs));

   pHbnode->node = node;
   if( iNew == 0 )
   {
      mxmlRetain(node);
   }

   return pHbnode;
}

static void mxml_node_push(mxml_node_t * node, int iNew)
{
   hb_itemPutPtrGC(hb_stackAllocItem(), mxml_node_new(node, iNew));
}

static void mxml_node_ret(mxml_node_t * node, int iNew)
{
   if( node )
   {
      hb_itemPutPtrGC(hb_stackReturnItem(), mxml_node_new(node, iNew));
   }
}

/* mxml_index_t * support */

static HB_GARBAGE_FUNC(hbmxml_indexDestructor)
{
   auto ppMxml_index = static_cast<mxml_index_t **>(Cargo);

   if( *ppMxml_index )
   {
      mxmlIndexDelete(*ppMxml_index);
      *ppMxml_index = nullptr;
   }
}

static const HB_GC_FUNCS s_gc_mxml_indexFuncs =
{
   hbmxml_indexDestructor,
   hb_gcDummyMark
};

static mxml_index_t * mxml_index_param(int iParam)
{
   auto ppMxml_index = static_cast<mxml_index_t **>(hb_parptrGC(&s_gc_mxml_indexFuncs, iParam));

   return (ppMxml_index && *ppMxml_index) ? *ppMxml_index : nullptr;
}

static mxml_index_t ** mxml_index_new(mxml_index_t * index)
{
   auto ppMxml_index = static_cast<mxml_index_t **>(hb_gcAllocate(sizeof(mxml_index_t*), &s_gc_mxml_indexFuncs));

   *ppMxml_index = index;

   return ppMxml_index;
}

static void mxml_index_ret(mxml_index_t * index)
{
   hb_itemPutPtrGC(hb_stackReturnItem(), mxml_index_new(index));
}

/* non-wrap hb_mxml*() functions */

HB_FUNC(HB_MXMLVERSION)
{
   hb_retc(MXML_VERSION);
}

/* mxml_*() wrapper functions */

/*
 * - mxmlEntityAddCallback
 * - mxmlEntityRemoveCallback
 * - mxmlLoadFd
 * - mxmlSAXLoadFd
 * - mxmlSaveFd
 */

/* void mxmlAdd(mxml_node_t * parent, int where, mxml_node_t * child, mxml_node_t * node) */

/* mxmlAdd() does not update reference counters, we use our own
 * wrapper which does it [druzus]
 */

static void mxmlAddRef(mxml_node_t * parent, int where, mxml_node_t * child, mxml_node_t * node)
{
   mxml_node_t * old_parent = mxmlGetParent(node);

   mxmlAdd(parent, where, (child != nullptr) ? child : MXML_ADD_TO_PARENT, node);
   if( !old_parent )
   {
      mxmlRetain(node);
   }
}

HB_FUNC(MXMLADD)
{
   mxml_node_t * parent = mxml_node_param(1);
   auto where = hb_parnidef(2, MXML_ADD_BEFORE);
   mxml_node_t * child = mxml_node_param(3);
   mxml_node_t * node = mxml_node_param(4);

   if( parent && node )
   {
      where = (where == MXML_ADD_BEFORE) ? MXML_ADD_BEFORE : MXML_ADD_AFTER;

      mxmlAddRef(parent, where, (child != nullptr) ? child : MXML_ADD_TO_PARENT, node);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void mxmlElementDeleteAttr(mxml_node_t * node, const char * name) */

HB_FUNC(MXMLELEMENTDELETEATTR)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hName;
      mxmlElementDeleteAttr(node, hb_parstr_utf8(2, &hName, nullptr));
      hb_strfree(hName);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* const char * mxmlElementGetAttr(mxml_node_t * node, const char * name) */

HB_FUNC(MXMLELEMENTGETATTR)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hName;
      hb_retstr_utf8(mxmlElementGetAttr(node, hb_parstr_utf8(2, &hName, nullptr)));
      hb_strfree(hName);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void mxmlElementSetAttr(mxml_node_t * node, const char * name, const char * value) */

HB_FUNC(MXMLELEMENTSETATTR)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hName;
      void * hValue;
      mxmlElementSetAttr(node, hb_parstr_utf8(2, &hName, nullptr), hb_parstr_utf8(3, &hValue, nullptr));
      hb_strfree(hName);
      hb_strfree(hValue);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* const char * mxmlEntityGetName(int val) */

HB_FUNC(MXMLENTITYGETNAME)
{
   hb_retstr_utf8(mxmlEntityGetName(hb_parni(1)));
}

/* int mxmlEntityGetValue(const char * name) */

HB_FUNC(MXMLENTITYGETVALUE)
{
   void * hName;
   hb_retni(mxmlEntityGetValue(hb_parstr_utf8(1, &hName, nullptr)));
   hb_strfree(hName);
}

/*
   mxml_node_t * mxmlFindElement(mxml_node_t * node, mxml_node_t * top,
                                 const char * name, const char * attr,
                                 const char * value, int descend);
 */

HB_FUNC(MXMLFINDELEMENT)
{
   void * hName;
   void * hAttr;
   void * hValue;
   mxml_node_t * node = mxml_node_param(1);
   mxml_node_t * top = mxml_node_param(2);

   if( node && top )
   {
      mxml_node_t * nodef = mxmlFindElement(node, top,
                                            hb_parstr_utf8(3, &hName, nullptr),
                                            hb_parstr_utf8(4, &hAttr, nullptr),
                                            hb_parstr_utf8(5, &hValue, nullptr),
                                            hb_parni(6));

      hb_strfree(hName);
      hb_strfree(hAttr);
      hb_strfree(hValue);

      mxml_node_ret(nodef, 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlFindPath(mxml_node_t * node, const char * path) */

HB_FUNC(MXMLFINDPATH)
{
   void * hPath;
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_t * nodef = mxmlFindPath(node, hb_parstr_utf8(2, &hPath, nullptr));
      mxml_node_ret(nodef, 0);
      hb_strfree(hPath);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* char * mxmlGetCDATA(mxml_node_t * node) */

HB_FUNC(MXMLGETCDATA)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retstr_utf8(mxmlGetCDATA(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* char * mxmlGetElement(mxml_node_t * node) */

HB_FUNC(MXMLGETELEMENT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retstr_utf8(mxmlGetElement(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlGetFirstChild(mxml_node_t * node) */

HB_FUNC(MXMLGETFIRSTCHILD)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_ret(mxmlGetFirstChild(node), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlGetInteger(mxml_node_t * node) */

HB_FUNC(MXMLGETINTEGER)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlGetInteger(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlGetLastChild(mxml_node_t * node) */

HB_FUNC(MXMLGETLASTCHILD)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_ret(mxmlGetLastChild(node), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlGetNextSibling(mxml_node_t * node) */

HB_FUNC(MXMLGETNEXTSIBLING)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_ret(mxmlGetNextSibling(node), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* char * mxmlGetOpaque(mxml_node_t * node) */

HB_FUNC(MXMLGETOPAQUE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retstr_utf8(mxmlGetOpaque(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlGetParent(mxml_node_t * node) */

HB_FUNC(MXMLGETPARENT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_ret(mxmlGetParent(node), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlGetPrevSibling(mxml_node_t *node) */

HB_FUNC(MXMLGETPREVSIBLING)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_node_ret(mxmlGetPrevSibling(node), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* double mxmlGetReal(mxml_node_t * node) */

HB_FUNC(MXMLGETREAL)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retnd(mxmlGetReal(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlGetRefCount(mxml_node_t * node) */

HB_FUNC(MXMLGETREFCOUNT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlGetRefCount(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* char * mxmlGetText(mxml_node_t * node, int * whitespace) */

HB_FUNC(MXMLGETTEXT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      auto whitespace = hb_parni(2);

      hb_retstr_utf8(mxmlGetText(node, &whitespace));
      hb_storni(whitespace, 2);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_type_t mxmlGetType(mxml_node_t * node) */

HB_FUNC(MXMLGETTYPE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(static_cast<int>(mxmlGetType(node)));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void * mxmlGetUserData(mxml_node_t * node) */

HB_FUNC(MXMLGETUSERDATA)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      auto pItem = static_cast<PHB_ITEM>(mxmlGetUserData(node));

      if( pItem != nullptr )
      {
         hb_itemCopy(hb_stackReturnItem(), pItem);
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlIndexEnum(mxml_index_t * ind) */

HB_FUNC(MXMLINDEXENUM)
{
   mxml_index_t * index = mxml_index_param(1);

   if( index )
   {
      mxml_node_ret(mxmlIndexEnum(index), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlIndexFind(mxml_index_t * ind, const char * element, const char * value) */

HB_FUNC(MXMLINDEXFIND)
{
   mxml_index_t * index = mxml_index_param(1);

   if( index )
   {
      void * hElement;
      void * hValue;
      mxml_node_t * nodef = mxmlIndexFind(index, hb_parstr_utf8(2, &hElement, nullptr), hb_parstr_utf8(3, &hValue, nullptr));
      hb_strfree(hElement);
      hb_strfree(hValue);

      mxml_node_ret(nodef, 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void mxmlIndexDelete(mxml_index_t * ind) */

HB_FUNC(MXMLINDEXDELETE)
{
   auto ppIndex = static_cast<mxml_index_t **>(hb_parptrGC(&s_gc_mxml_indexFuncs, 1));

   if( ppIndex && *ppIndex )
   {
      mxmlIndexDelete(*ppIndex);
      *ppIndex = nullptr;
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlIndexGetCount(mxml_index_t * ind) */

HB_FUNC(MXMLINDEXGETCOUNT)
{
   mxml_index_t * index = mxml_index_param(1);

   if( index )
   {
      hb_retni(mxmlIndexGetCount(index));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_index_t * mxmlIndexNew(mxml_node_t * node, const char * element, const char * attr) */

HB_FUNC(MXMLINDEXNEW)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hElement;
      void * hAttribute;
      mxml_index_t * index = mxmlIndexNew(node, hb_parstr_utf8(2, &hElement, nullptr), hb_parstr_utf8(3, &hAttribute, nullptr));
      hb_strfree(hElement);
      hb_strfree(hAttribute);

      if( index )
      {
         mxml_index_ret(index);
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlIndexReset(mxml_index_t * ind) */

HB_FUNC(MXMLINDEXRESET)
{
   mxml_index_t * index = mxml_index_param(1);

   if( index )
   {
      mxml_node_ret(mxmlIndexReset(index), 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_type_t type_cb(mxml_node_t * node) */

static mxml_type_t type_cb(mxml_node_t * node)
{
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackTestTSD(&s_cbs_var));

   if( pCbs != nullptr )
   {
      PHB_ITEM pCallback = pCbs->type_cb;

      if( pCallback && hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         mxml_node_push(node, 0);

         hb_vmSend(1);

         auto iResult = hb_parnidef(-1, MXML_TEXT);

         hb_vmRequestRestore();
         return static_cast<mxml_type_t>(iResult);
      }
   }

   return MXML_TEXT;
}

HB_FUNC(MXMLLOADFILE)
{
   mxml_node_t * node_top;
   mxml_node_t * node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param(1);

      if( !node_top )
      {
         MXML_ERR_ARGS();
         return;
      }
   }

   if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
   {
      pCbs->type_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb = type_cb;
   }
   else if( HB_ISNUM(3) )
   {
      switch( hb_parni(3) )
      {
         case 0:  cb = MXML_NO_CALLBACK;       break;
         case 1:  cb = MXML_INTEGER_CALLBACK;  break;
         case 2:  cb = MXML_OPAQUE_CALLBACK;   break;
         case 3:  cb = MXML_REAL_CALLBACK;     break;
         case 4:  cb = MXML_TEXT_CALLBACK;     break;
         case 5:  cb = MXML_IGNORE_CALLBACK;   break;
         default: cb = MXML_NO_CALLBACK;
      }
   }

   auto file = hb_fopen(hb_parc(2), "rb");
   if( file )
   {
      node = mxmlLoadFile(node_top, file, cb);
      mxml_node_ret(node, (node_top == MXML_NO_PARENT) ? 1 : 0);
      fclose(file);
   }

   pCbs->type_cb = nullptr;
}

/*
   mxml_node_t * mxmlLoadString(mxml_node_t * top, const char * s,
                                mxml_type_t (*cb)(mxml_node_t *))
 */

HB_FUNC(MXMLLOADSTRING)
{
   void * hFree;
   mxml_node_t * node_top;
   mxml_node_t * node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param(1);

      if( !node_top )
      {
         MXML_ERR_ARGS();
         return;
      }
   }

   if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
   {
      pCbs->type_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb = type_cb;
   }
   else if( HB_ISNUM(3) )
   {
      switch( hb_parni(3) )
      {
         case 0:  cb = MXML_NO_CALLBACK;       break;
         case 1:  cb = MXML_INTEGER_CALLBACK;  break;
         case 2:  cb = MXML_OPAQUE_CALLBACK;   break;
         case 3:  cb = MXML_REAL_CALLBACK;     break;
         case 4:  cb = MXML_TEXT_CALLBACK;     break;
         case 5:  cb = MXML_IGNORE_CALLBACK;   break;
         default: cb = MXML_NO_CALLBACK;
      }
   }

   node = mxmlLoadString(node_top, hb_parstr_utf8(2, &hFree, nullptr), cb);
   pCbs->type_cb = nullptr;

   mxml_node_ret(node, (node_top == MXML_NO_PARENT) ? 1 : 0);

   hb_strfree(hFree);
}

/* mxml_node_t * mxmlNewCDATA(mxml_node_t * parent, const char * string) */

HB_FUNC(MXMLNEWCDATA)
{
   void * hString;
   mxml_node_t * node;

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node = mxmlNewCDATA(MXML_NO_PARENT, hb_parstr_utf8(2, &hString, nullptr));
      mxml_node_ret(node, 1);
      hb_strfree(hString);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         node = mxmlNewCDATA(node_parent, hb_parstr_utf8(2, &hString, nullptr));
         mxml_node_ret(node, 0);
         hb_strfree(hString);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewElement(mxml_node_t * parent, const char * name) */

HB_FUNC(MXMLNEWELEMENT)
{
   void * hName;
   mxml_node_t * node;

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node = mxmlNewElement(MXML_NO_PARENT, hb_parstr_utf8(2, &hName, nullptr));
      mxml_node_ret(node, 1);
      hb_strfree(hName);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         node = mxmlNewElement(node_parent, hb_parstr_utf8(2, &hName, nullptr));
         mxml_node_ret(node, 0);
         hb_strfree(hName);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewInteger(mxml_node_t * parent, int integer) */

HB_FUNC(MXMLNEWINTEGER)
{
   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      mxml_node_ret(mxmlNewInteger(MXML_NO_PARENT, hb_parni(2)), 1);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         mxml_node_ret(mxmlNewInteger(node_parent, hb_parni(2)), 0);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewOpaque(mxml_node_t *parent, const char * opaque) */

HB_FUNC(MXMLNEWOPAQUE)
{
   void * hOpaque;
   mxml_node_t * node;

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node = mxmlNewOpaque(MXML_NO_PARENT, hb_parstr_utf8(2, &hOpaque, nullptr));
      mxml_node_ret(node, 1);
      hb_strfree(hOpaque);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         node = mxmlNewOpaque(node_parent, hb_parstr_utf8(2, &hOpaque, nullptr));
         mxml_node_ret(node, 0);
         hb_strfree(hOpaque);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewReal(mxml_node_t * parent, double real) */

HB_FUNC(MXMLNEWREAL)
{
   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      mxml_node_ret(mxmlNewReal(MXML_NO_PARENT, hb_parnd(2)), 1);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         mxml_node_ret(mxmlNewReal(node_parent, hb_parnd(2)), 0);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewText(mxml_node_t * parent, int whitespace, const char * string) */

HB_FUNC(MXMLNEWTEXT)
{
   void * hString;
   mxml_node_t * node;

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node = mxmlNewText(MXML_NO_PARENT, hb_parnidef(2, 0), hb_parstr_utf8(3, &hString, nullptr));
      mxml_node_ret(node, 1);
      hb_strfree(hString);
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param(1);

      if( node_parent )
      {
         node = mxmlNewText(node_parent, hb_parnidef(2, 0), hb_parstr_utf8(3, &hString, nullptr));
         mxml_node_ret(node, 0);
         hb_strfree(hString);
      }
      else
      {
         MXML_ERR_ARGS();
      }
   }
}

/* mxml_node_t * mxmlNewXML(const char * version) */

HB_FUNC(MXMLNEWXML)
{
   mxml_node_t * node;

   if( HB_ISCHAR(1) )
   {
      void * hVersion;
      node = mxmlNewXML(hb_parstr_utf8(1, &hVersion, nullptr));
      hb_strfree(hVersion);
   }
   else
   {
      node = mxmlNewXML(nullptr);
   }

   mxml_node_ret(node, 1);
}

/* void mxmlRemove(mxml_node_t * node) */

/* mxmlRemove() does not update reference counters, we use our own
 * wrapper which does it [druzus]
 */

static void mxmlRemoveRef(mxml_node_t * node)
{
   if( mxmlGetParent(node) )
   {
      mxmlRemove(node);
      mxmlRelease(node);
   }
}

HB_FUNC(MXMLREMOVE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxmlRemoveRef(node);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void mxmlDelete(mxml_node_t * node) */

HB_FUNC(MXMLDELETE)
{
   auto pHbnode = static_cast<HBMXML_NODE *>(hb_parptrGC(&s_gc_mxml_nodeFuncs, 1));

   if( pHbnode && pHbnode->node )
   {
      mxmlRemoveRef(pHbnode->node);
      hbmxml_release(pHbnode->node);
      pHbnode->node = nullptr;
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

#if 0
/* int mxmlRelease(mxml_node_t * node) */

HB_FUNC(MXMLRELEASE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlRelease(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlRetain(mxml_node_t * node) */

HB_FUNC(MXMLRETAIN)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlRetain(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}
#endif

/* void (* mxml_sax_cb_t)(mxml_node_t *, mxml_sax_event_t, void *) */

static void sax_cb(mxml_node_t * node, mxml_sax_event_t event, void * data)
{
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackTestTSD(&s_cbs_var));

   if( node != nullptr && pCbs != nullptr )
   {
      PHB_ITEM pCallback = pCbs->sax_cb;

      if( pCallback && hb_vmRequestReenter() )
      {
         HB_USHORT uPCount = 2;

         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         mxml_node_push(node, 0);
         hb_vmPushInteger(static_cast<int>(event + 1));

         if( data != nullptr )
         {
            hb_vmPush(static_cast<PHB_ITEM>(data));
            uPCount++;
         }
         hb_vmSend(uPCount);

         hb_vmRequestRestore();
      }
   }
}

/* mxml_node_t * mxmlSAXLoadFile(mxml_node_t * top,
                                 FILE * fp,
                                 mxml_load_cb_t cb,
                                 mxml_sax_cb_t sax_cb,
                                 void * sax_data)
 */
HB_FUNC(MXMLSAXLOADFILE)
{
   mxml_node_t * node_top;
   mxml_node_t * node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   mxml_sax_cb_t cb_sax = MXML_NO_CALLBACK;
   PHB_ITEM pData = (hb_pcount() > 4) ? hb_param(5, Harbour::Item::ANY) : nullptr;
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param(1);

      if( !node_top )
      {
         MXML_ERR_ARGS();
         return;
      }
   }

   if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
   {
      pCbs->type_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb = type_cb;
   }
   else if( HB_ISNUM(3) )
   {
      switch( hb_parni(3) )
      {
         case 0:  cb = MXML_NO_CALLBACK;       break;
         case 1:  cb = MXML_INTEGER_CALLBACK;  break;
         case 2:  cb = MXML_OPAQUE_CALLBACK;   break;
         case 3:  cb = MXML_REAL_CALLBACK;     break;
         case 4:  cb = MXML_TEXT_CALLBACK;     break;
         case 5:  cb = MXML_IGNORE_CALLBACK;   break;
         default: cb = MXML_NO_CALLBACK;
      }
   }

   if( HB_ISBLOCK(4) || HB_ISSYMBOL(4) )
   {
      pCbs->sax_cb = hb_param(4, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb_sax = sax_cb;
   }

   auto file = hb_fopen(hb_parc(2), "rb");
   if( file )
   {
      node = mxmlSAXLoadFile(node_top, file, cb, cb_sax, pData);
      mxml_node_ret(node, (node_top == MXML_NO_PARENT) ? 1 : 0);
      fclose(file);
   }

   pCbs->type_cb = nullptr;
   pCbs->sax_cb = nullptr;
}

/* mxml_node_t * mxmlSAXLoadString(mxml_node_t * top,
                                   const char * s,
                                   mxml_load_cb_t cb,
                                   mxml_sax_cb_t sax_cb,
                                   void * sax_data)
 */
HB_FUNC(MXMLSAXLOADSTRING)
{
   void * hFree;
   mxml_node_t * node_top;
   mxml_node_t * node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   mxml_sax_cb_t cb_sax = MXML_NO_CALLBACK;
   PHB_ITEM pData = (hb_pcount() > 4) ? hb_param(5, Harbour::Item::ANY) : nullptr;
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));
   const char * s;

   if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param(1);

      if( !node_top )
      {
         MXML_ERR_ARGS();
         return;
      }
   }

   if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
   {
      pCbs->type_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb = type_cb;
   }
   else if( HB_ISNUM(3) )
   {
      switch( hb_parni(3) )
      {
         case 0:  cb = MXML_NO_CALLBACK;       break;
         case 1:  cb = MXML_INTEGER_CALLBACK;  break;
         case 2:  cb = MXML_OPAQUE_CALLBACK;   break;
         case 3:  cb = MXML_REAL_CALLBACK;     break;
         case 4:  cb = MXML_TEXT_CALLBACK;     break;
         case 5:  cb = MXML_IGNORE_CALLBACK;   break;
         default: cb = MXML_NO_CALLBACK;
      }
   }

   if( HB_ISBLOCK(4) || HB_ISSYMBOL(4) )
   {
      pCbs->sax_cb = hb_param(4, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
      cb_sax = sax_cb;
   }

   s = hb_parstr_utf8(2, &hFree, nullptr);
   if( s )
   {
      node = mxmlSAXLoadString(node_top, s, cb, cb_sax, pData);
      mxml_node_ret(node, (node_top == MXML_NO_PARENT) ? 1 : 0);
   }

   pCbs->type_cb = nullptr;
   pCbs->sax_cb = nullptr;

   hb_strfree(hFree);
}

/* const char *(* mxml_save_cb_t)(mxml_node_t *, int) */

static const char * save_cb(mxml_node_t * node, int where)
{
   auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackTestTSD(&s_cbs_var));

   if( node != nullptr && pCbs != nullptr )
   {
      PHB_ITEM pCallback = pCbs->save_cb;

      if( pCbs->hText )
      {
         hb_strfree(pCbs->hText);
         pCbs->hText = nullptr;
      }

      if( pCallback && hb_vmRequestReenter() )
      {
         const char * pszResult;

         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         mxml_node_push(node, 0);
         hb_vmPushInteger(where);
         hb_vmSend(2);

         pszResult = hb_itemGetStrUTF8(hb_param(-1, Harbour::Item::ANY), &pCbs->hText, nullptr);

         hb_vmRequestRestore();

         return pszResult;
      }
   }
   return nullptr;
}

/* char * mxmlSaveAllocString(mxml_node_t *node, mxml_save_cb_t cb) */

HB_FUNC(MXMLSAVEALLOCSTRING)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));
      char buffer[8192];
      int  bytes;

      if( HB_ISBLOCK(2) || HB_ISSYMBOL(2) )
      {
         pCbs->save_cb = hb_param(2, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
         cb = save_cb;
      }

      bytes = mxmlSaveString(node, buffer, sizeof(buffer), cb);
      pCbs->save_cb = nullptr;
      if( pCbs->hText )
      {
         hb_strfree(pCbs->hText);
         pCbs->hText = nullptr;
      }

      if( bytes <= 0 )
      {
         hb_retc_null();
      }
      else if( bytes < static_cast<int>(sizeof(buffer)) )
      {
         hb_retclen(buffer, bytes);
      }
      else
      {
         auto s = static_cast<char*>(hb_xalloc(bytes + 1));

         if( s == nullptr )
         {
            hb_retc_null();
         }
         else
         {
            mxmlSaveString(node, s, bytes + 1, cb);
            hb_retclen_buffer(s, bytes);
         }
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

HB_FUNC(MXMLSAVEFILE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node && HB_ISCHAR(2) )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));

      if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
      {
         pCbs->save_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
         cb = save_cb;
      }

      auto file = hb_fopen(hb_parc(2), "wb");
      if( file )
      {
         hb_retni(mxmlSaveFile(node, file, cb));
         fclose(file);
      }

      pCbs->save_cb = nullptr;
      if( pCbs->hText )
      {
         hb_strfree(pCbs->hText);
         pCbs->hText = nullptr;
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSaveString(mxml_node_t * node, char * buffer, int bufsize, mxml_save_cb_t cb) */

HB_FUNC(MXMLSAVESTRING)
{
   mxml_node_t * node = mxml_node_param(1);
   auto pBuffer = hb_param(2, Harbour::Item::STRING);

   if( node )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      auto pCbs = static_cast<HB_CBS_VAR *>(hb_stackGetTSD(&s_cbs_var));

      if( pBuffer && HB_ISBYREF(2) && hb_parcsiz(2) > 0 )
      {
         char * buffer;
         HB_SIZE buffer_size;
         int bytes;

         if( hb_itemGetWriteCL(pBuffer, &buffer, &buffer_size) )
         {
            if( HB_ISBLOCK(3) || HB_ISSYMBOL(3) )
            {
               pCbs->save_cb = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
               cb = save_cb;
            }

            bytes = mxmlSaveString(node, buffer, static_cast<int>(buffer_size + 1), cb);

            if( bytes > 0 && static_cast<HB_SIZE>(bytes) <= buffer_size )
            {
               hb_storclen(buffer, bytes, 2);
            }

            hb_retni(bytes);

            pCbs->save_cb = nullptr;
            if( pCbs->hText )
            {
               hb_strfree(pCbs->hText);
               pCbs->hText = nullptr;
            }
            return;
         }
      }
   }

   MXML_ERR_ARGS();
}

/* int mxmlSetCDATA(mxml_node_t * node, const char * data) */

HB_FUNC(MXMLSETCDATA)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hData;
      hb_retni(mxmlSetCDATA(node, hb_parstr_utf8(2, &hData, nullptr)));
      hb_strfree(hData);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSetElement(mxml_node_t * node, const char * name) */

HB_FUNC(MXMLSETELEMENT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hFree;
      hb_retni(mxmlSetElement(node, hb_parstr_utf8(2, &hFree, nullptr)));
      hb_strfree(hFree);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void (* mxml_error_cb_t)(const char *) */

static void error_cb(const char * pszErrorMsg)
{
   auto pError_cb = static_cast<HB_ERROR_CB_VAR *>(hb_stackTestTSD(&s_error_cb_var));

   if( pError_cb != nullptr )
   {
      PHB_ITEM pCallback = pError_cb->error_cb;

      if( pCallback && hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         hb_itemPutC(hb_stackAllocItem(), pszErrorMsg);

         hb_vmSend(1);
         hb_vmRequestRestore();
      }
   }
}

HB_FUNC(MXMLSETERRORCALLBACK)
{
   auto pError = hb_param(1, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

   if( pError )
   {
      auto pError_cb = static_cast<HB_ERROR_CB_VAR *>(hb_stackGetTSD(&s_error_cb_var));

      if( pError_cb->error_cb )
      {
         hb_itemRelease(pError_cb->error_cb);
      }
      pError_cb->error_cb = hb_itemNew(pError);

      mxmlSetErrorCallback(error_cb);
   }
   else
   {
      auto pError_cb = static_cast<HB_ERROR_CB_VAR *>(hb_stackTestTSD(&s_error_cb_var));

      if( pError_cb && pError_cb->error_cb )
      {
         hb_itemRelease(pError_cb->error_cb);
         pError_cb->error_cb = nullptr;
      }

      mxmlSetErrorCallback(nullptr);
   }
}

/* int mxmlSetInteger(mxml_node_t * node, int integer) */

HB_FUNC(MXMLSETINTEGER)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlSetInteger(node, hb_parnidef(2, 0)));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSetOpaque(mxml_node_t * node, const char * opaque) */

HB_FUNC(MXMLSETOPAQUE)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hOpaque;
      hb_retni(mxmlSetOpaque(node, hb_parstr_utf8(2, &hOpaque, nullptr)));
      hb_strfree(hOpaque);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSetReal(mxml_node_t * node, double real) */

HB_FUNC(MXMLSETREAL)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      hb_retni(mxmlSetReal(node, hb_parnd(2)));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSetText(mxml_node_t * node, int whitespace, const char * string) */

HB_FUNC(MXMLSETTEXT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      void * hString;
      hb_retni(mxmlSetText(node, hb_parnidef(2, 1), hb_parstr_utf8(3, &hString, nullptr)));
      hb_strfree(hString);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int mxmlSetUserData(mxml_node_t * node, void * data ) */

HB_FUNC(MXMLSETUSERDATA)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      PHB_ITEM pItem = hb_itemClone(hb_param(2, Harbour::Item::ANY));

      if( pItem != nullptr )
      {
         hb_retni(mxmlSetUserData(node, pItem));
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void mxmlSetWrapMargin(int column) */

HB_FUNC(MXMLSETWRAPMARGIN)
{
   mxmlSetWrapMargin(hb_parni(1));
}

/* mxml_node_t * mxmlWalkNext(mxml_node_t * node, mxml_node_t * top, int descend) */

HB_FUNC(MXMLWALKNEXT)
{
   mxml_node_t * node = mxml_node_param(1);
   mxml_node_t * top = mxml_node_param(2);

   if( node && top )
   {
      mxml_node_t * node_next = mxmlWalkNext(node, top, hb_parnidef(3, MXML_NO_DESCEND));

      mxml_node_ret(node_next, 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* mxml_node_t * mxmlWalkPrev(mxml_node_t * node, mxml_node_t * top, int descend) */

HB_FUNC(MXMLWALKPREV)
{
   mxml_node_t * node = mxml_node_param(1);
   mxml_node_t * top = mxml_node_param(2);

   if( node && top )
   {
      mxml_node_t * node_prev = mxmlWalkPrev(node, top, hb_parnidef(3, MXML_NO_DESCEND));

      mxml_node_ret(node_prev, 0);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* const void * mxmlGetCustom(mxml_node_t * node) */

HB_FUNC(MXMLGETCUSTOM)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node )
   {
      auto pItem = static_cast<PHB_ITEM>(const_cast<void*>(mxmlGetCustom(node)));

      if( pItem != nullptr )
      {
         hb_itemReturn(pItem);
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* void (* mxml_custom_destroy_cb_t)(void *) */

static void custom_destroy_cb(void * Cargo)
{
   auto pItem = static_cast<PHB_ITEM>(Cargo);

   if( pItem != nullptr )
   {
      hb_itemRelease(pItem);
   }
}

/* mxml_node_t * mxmlNewCustom(mxml_node_t * parent,
                               void * data, mxml_custom_destroy_cb_t destroy)
 */
HB_FUNC(MXMLNEWCUSTOM)
{
   if( hb_pcount() > 1 )
   {
      PHB_ITEM pItem = hb_itemClone(hb_param(2, Harbour::Item::ANY));
      mxml_node_t * parent = MXML_NO_PARENT;
      mxml_node_t * node = nullptr;

      if( HB_ISNIL(1) || (HB_ISNUM(1) && hb_parni(1) == MXML_NO_PARENT) )
      {
         node = mxmlNewCustom(MXML_NO_PARENT, pItem, custom_destroy_cb);
      }
      else
      {
         parent = mxml_node_param(1);

         if( parent != nullptr )
         {
            node = mxmlNewCustom(parent, pItem, custom_destroy_cb);
         }
      }

      if( node )
      {
         mxml_node_ret(node, (parent == MXML_NO_PARENT) ? 1 : 0);
         return;
      }

      hb_itemRelease(pItem);
   }

   MXML_ERR_ARGS();
}

/* int mxmlSetCustom (mxml_node_t * node,
                      void * data, mxml_custom_destroy_cb_t destroy)
 */
HB_FUNC(MXMLSETCUSTOM)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node && hb_pcount() > 1 )
   {
      PHB_ITEM pItem = hb_itemClone(hb_param(2, Harbour::Item::ANY));

      if( pItem != nullptr )
      {
         int iResult = mxmlSetCustom(node, pItem, custom_destroy_cb);

         if( iResult < 0 )
         {
            hb_itemRelease(pItem);
         }

         hb_retni(iResult);
      }
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

/* int (* mxml_custom_load_cb_t)(mxml_node_t *, const char *) */

static int custom_load_cb(mxml_node_t * node, const char * data)
{
   auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(hb_stackTestTSD(&s_custom_cbs_var));

   if( node != nullptr && pCCbs != nullptr && data != nullptr )
   {
      PHB_ITEM pCallback = pCCbs->load_cb;

      if( pCallback && hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         mxml_node_push(node, 0);
         hb_itemPutC(hb_stackAllocItem(), data);

         hb_vmSend(2);

         auto iResult = hb_parnidef(-1, 1);

         hb_vmRequestRestore();
         return iResult;
      }
   }
   return 1;
}

/* char * (* mxml_custom_save_cb_t)(mxml_node_t *) */

static char * custom_save_cb(mxml_node_t * node)
{
   auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(hb_stackTestTSD(&s_custom_cbs_var));

   if( node != nullptr && pCCbs != nullptr )
   {
      PHB_ITEM pCallback = pCCbs->save_cb;

      if( pCallback && hb_vmRequestReenter() )
      {
         char * pszResult;
         const char * pszText;
         void * hText;

         hb_vmPushEvalSym();
         hb_vmPush(pCallback);
         mxml_node_push(node, 0);

         hb_vmSend(1);

         pszText = hb_parstr_utf8(-1, &hText, nullptr);
         pszResult = pszText ? strdup(pszText) : nullptr;
         hb_strfree(hText);

         hb_vmRequestRestore();
         return pszResult;
      }
   }
   return nullptr;
}

/* void mxmlSetCustomHandlers(mxml_custom_load_cb_t load, mxml_custom_save_cb_t save) */

HB_FUNC(MXMLSETCUSTOMHANDLERS)
{
   auto pLoad = hb_param(1, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
   auto pSave = hb_param(2, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

   if( pLoad && pSave )
   {
      auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(hb_stackGetTSD(&s_custom_cbs_var));

      if( pCCbs->load_cb )
      {
         hb_itemRelease(pCCbs->load_cb);
      }
      pCCbs->load_cb = hb_itemNew(pLoad);

      if( pCCbs->save_cb )
      {
         hb_itemRelease(pCCbs->save_cb);
      }
      pCCbs->save_cb = hb_itemNew(pSave);

      mxmlSetCustomHandlers(custom_load_cb, custom_save_cb);
   }
   else
   {
      auto pCCbs = static_cast<HB_CUSTOM_CBS_VAR *>(hb_stackTestTSD(&s_custom_cbs_var));

      if( pCCbs )
      {
         if( pCCbs->load_cb )
         {
            hb_itemRelease(pCCbs->load_cb);
            pCCbs->load_cb = nullptr;
         }
         if( pCCbs->save_cb )
         {
            hb_itemRelease(pCCbs->save_cb);
            pCCbs->save_cb = nullptr;
         }
      }

      mxmlSetCustomHandlers(nullptr, nullptr);
   }
}

/* temporary solution before final 2.7 release */

HB_FUNC(HB_MXMLGETATTRSCOUNT)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node && mxmlGetType(node) == MXML_ELEMENT )
   {
      hb_retni(mxmlElementGetAttrCount(node));
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

HB_FUNC(HB_MXMLGETATTRSARRAY)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node && mxmlGetType(node) == MXML_ELEMENT )
   {
      auto pAttrs = hb_itemArrayNew(0);
      auto pAttr = hb_itemNew(nullptr);
      int i, count;
      const char *name, *value;

      for( i = 0, count = mxmlElementGetAttrCount(node); i < count; i++ )
      {
         value = mxmlElementGetAttrByIndex(node, i, &name);
         hb_arrayNew(pAttr, 2);
         hb_arraySetStrUTF8(pAttr, 1, name);
         hb_arraySetStrUTF8(pAttr, 2, value);
         hb_arrayAddForward(pAttrs, pAttr);
      }

      hb_itemRelease(pAttr);

      hb_itemReturnRelease(pAttrs);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}

HB_FUNC(HB_MXMLGETATTRS)
{
   mxml_node_t * node = mxml_node_param(1);

   if( node && mxmlGetType(node) == MXML_ELEMENT )
   {
      PHB_ITEM pAttrs = hb_hashNew(hb_itemNew(nullptr));
      PHB_ITEM pKey = nullptr;
      PHB_ITEM pValue = nullptr;
      int i, count;
      const char *name, *value;

      for( i = 0, count = mxmlElementGetAttrCount(node); i < count; i++ )
      {
         value = mxmlElementGetAttrByIndex(node, i, &name);
         pKey = hb_itemPutStrUTF8(pKey, name);
         pValue = hb_itemPutStrUTF8(pValue, value);

         hb_hashAdd(pAttrs, pKey, pValue);
      }

      hb_itemRelease(pKey);
      hb_itemRelease(pValue);

      hb_itemReturnRelease(pAttrs);
   }
   else
   {
      MXML_ERR_ARGS();
   }
}
