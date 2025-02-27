//
// .prg interface to preprocessor
//
// Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//

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

#include "hbapi.hpp"
#include "hbpp.hpp"
#include "hbapiitm.hpp"
#include "hbapifs.hpp"
#include "hbapierr.hpp"
#include "hbvm.hpp"

HB_EXTERN_BEGIN

static void hb_pp_ErrorMessage(void *cargo, const char *const szMsgTable[], char cPrefix, int iCode,
                               const char *szParam1, const char *szParam2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_pp_ErrorGen(%p, %p, %c, %d, %s, %s)", cargo, static_cast<const void*>(szMsgTable), cPrefix, iCode, szParam1, szParam2));
#endif

  HB_SYMBOL_UNUSED(cargo);

  /* ignore all warning messages and errors when break or quit request */
  if (cPrefix != 'W' && hb_vmRequestQuery() == 0)
  {
    char szMsgBuf[1024];
    hb_snprintf(szMsgBuf, sizeof(szMsgBuf), szMsgTable[iCode - 1], szParam1, szParam2);
    auto pError = hb_errRT_New(ES_ERROR, "PP", 1001, static_cast<HB_ERRCODE>(iCode), szMsgBuf, nullptr, 0,
                               EF_NONE | EF_CANDEFAULT);
    hb_errLaunch(pError);
    hb_errRelease(pError);
  }
}

static void hb_pp_Disp(void *cargo, const char *szMessage)
{
  /* ignore stdout messages when PP used as library */
  HB_SYMBOL_UNUSED(cargo);
  HB_SYMBOL_UNUSED(szMessage);
}

static HB_BOOL hb_pp_CompilerSwitch(void *cargo, const char *szSwitch, int *piValue, HB_BOOL fSet)
{
  /* ignore all compiler switches */
  HB_SYMBOL_UNUSED(cargo);
  HB_SYMBOL_UNUSED(szSwitch);
  HB_SYMBOL_UNUSED(piValue);
  HB_SYMBOL_UNUSED(fSet);

  return false;
}

/* PP destructor */
static HB_GARBAGE_FUNC(hb_pp_Destructor)
{
  PHB_PP_STATE *pStatePtr = static_cast<PHB_PP_STATE *>(Cargo);

  if (*pStatePtr)
  {
    hb_pp_free(*pStatePtr);
    *pStatePtr = nullptr;
  }
}

HB_EXTERN_END

static const HB_GC_FUNCS s_gcPPFuncs = {hb_pp_Destructor, hb_gcDummyMark};

static void hb_pp_StdRules(PHB_ITEM ppItem)
{
  static auto s_fInit = true;
  static PHB_DYNS s_pDynSym;

  if (s_fInit)
  {
    s_pDynSym = hb_dynsymFind("__PP_STDRULES");
    s_fInit = false;
  }

  if (s_pDynSym)
  {
    hb_vmPushDynSym(s_pDynSym);
    hb_vmPushNil();
    hb_vmPush(ppItem);
    hb_vmProc(1);
  }
}

PHB_PP_STATE hb_pp_Param(int iParam)
{
  PHB_PP_STATE *pStatePtr = static_cast<PHB_PP_STATE *>(hb_parptrGC(&s_gcPPFuncs, iParam));

  if (pStatePtr)
  {
    return *pStatePtr;
  }
  else
  {
    return nullptr;
  }
}

/*
 * initialize new PP context and return pointer to it.
 * __pp_Init([<cIncludePath>], [<cStdChFile> ] [, <lArchDefs>]) --> <pPP>
 * when <cStdChFile> is empty string ("") then no default rules are used
 * only the dynamically created #defines like __HARBOUR__, __DATE__, __TIME__
 */
HB_FUNC(__PP_INIT)
{
  PHB_PP_STATE pState = hb_pp_new();

  if (pState)
  {
    PHB_PP_STATE *pStatePtr;
    auto szPath = hb_parc(1);
    auto szStdCh = hb_parc(2);
    bool fArchDefs = hb_parldef(3, true);

    pStatePtr = static_cast<PHB_PP_STATE *>(hb_gcAllocate(sizeof(PHB_PP_STATE), &s_gcPPFuncs));
    *pStatePtr = pState;
    auto ppItem = hb_itemPutPtrGC(nullptr, static_cast<void *>(pStatePtr));

    hb_pp_init(pState, true, false, 0, nullptr, nullptr, nullptr, hb_pp_ErrorMessage, hb_pp_Disp, nullptr, nullptr,
               hb_pp_CompilerSwitch);

    if (szPath != nullptr)
    {
      hb_pp_addSearchPath(pState, szPath, true);
    }

    if (!szStdCh)
    {
      hb_pp_StdRules(ppItem);
    }
    else if (*szStdCh)
    {
      hb_pp_readRules(pState, szStdCh);
    }

    hb_pp_initDynDefines(pState, fArchDefs);
    hb_pp_setStdBase(pState);

    hb_itemReturnRelease(ppItem);
  }
  else
  {
    hb_ret();
  }
}

/*
 * add new (or replace previous) include paths.
 * __pp_Path(<pPP>, <cPath> [, <lClearPrev>]) --> NIL
 */
HB_FUNC(__PP_PATH)
{
  PHB_PP_STATE pState = hb_pp_Param(1);

  if (pState)
  {
    hb_pp_addSearchPath(pState, hb_parc(2), hb_parl(3));
  }
}

/*
 * reset the PP context (remove all rules added by user or preprocessed code)
 * __pp_Reset(<pPP>) --> NIL
 */
HB_FUNC(__PP_RESET)
{
  PHB_PP_STATE pState = hb_pp_Param(1);

  if (pState)
  {
    hb_pp_reset(pState);
  }
}

/*
 * preprocess and execute new preprocessor directive
 * __pp_AddRule(<pPP>, <cDirective>) --> <lOK>
 */
HB_FUNC(__PP_ADDRULE)
{
  PHB_PP_STATE pState = hb_pp_Param(1);

  if (pState)
  {
    auto szText = hb_parc(2);
    auto nLen = hb_parclen(2);

    if (szText != nullptr)
    {
      while (nLen && (szText[0] == ' ' || szText[0] == '\t'))
      {
        ++szText;
        --nLen;
      }
    }

    if (szText != nullptr && nLen && szText[0] == '#')
    {
      hb_pp_parseLine(pState, szText, &nLen);

      /* probably for parsing #included files the old code was making
         something like that */
      do
      {
        if (hb_vmRequestQuery() != 0)
        {
          return;
        }
      } while (hb_pp_nextLine(pState, nullptr));

      hb_retl(true);
      return;
    }
  }
  hb_retl(false);
}

/*
 * preprocess given code and return result
 * __pp_Process(<pPP>, <cCode>) --> <cPreprocessedCode>
 */
HB_FUNC(__PP_PROCESS)
{
  PHB_PP_STATE pState = hb_pp_Param(1);

  if (pState)
  {
    auto nLen = hb_parclen(2);

    if (nLen)
    {
      char *szText = hb_pp_parseLine(pState, hb_parc(2), &nLen);
      hb_retclen(szText, nLen);
      return;
    }
  }

  hb_retc_null();
}
