//
// ActiveX core module
//
// Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbwapi.hpp"
#include "hbwinole.hpp"

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4201) // warning C4201: nonstandard extension used: nameless struct/union
#endif
#include <olectl.h>
#if defined(_MSC_VER)
#pragma warning(pop)
#endif

#include <hbapistr.hpp>

using PHB_AX_WININIT = BOOL(CALLBACK *)(void);
using PHB_AX_GETCTRL = HRESULT(CALLBACK *)(HWND, IUnknown **);

static HMODULE s_hLib = nullptr;

static PHB_AX_GETCTRL s_pAtlAxGetControl = nullptr;

static void hb_errRT_OLE(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode, const char *szDescription,
                         const char *szOperation)
{
  auto pError =
      hb_errRT_New(ES_ERROR, "WINOLE", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);

  if (hb_pcount() != 0)
  {
    // HB_ERR_ARGS_BASEPARAMS
    PHB_ITEM pArray = hb_arrayBaseParams();
    hb_errPutArgsArray(pError, pArray);
    hb_itemRelease(pArray);
  }
  hb_errLaunch(pError);
  hb_errRelease(pError);
}

static void hb_oleAxExit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (s_hLib)
  {
    s_pAtlAxGetControl = nullptr;

    FreeLibrary(s_hLib);

    s_hLib = nullptr;
  }
}

HB_BOOL hb_oleAxInit(void)
{
  hb_oleInit();

  if (s_hLib == nullptr)
  {
    s_hLib = hbwapi_LoadLibrarySystem(TEXT("atl.dll"));
    if (reinterpret_cast<HB_PTRUINT>(s_hLib) <= 32)
    {
      s_hLib = nullptr;
      return false;
    }
    auto pAtlAxWinInit =
        reinterpret_cast<PHB_AX_WININIT>(reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESS(s_hLib, "AtlAxWinInit")));
    s_pAtlAxGetControl =
        reinterpret_cast<PHB_AX_GETCTRL>(reinterpret_cast<void *>(HB_WINAPI_GETPROCADDRESS(s_hLib, "AtlAxGetControl")));

    if (pAtlAxWinInit)
    {
      (*pAtlAxWinInit)();
    }

    hb_vmAtQuit(hb_oleAxExit, nullptr);
  }
  return true;
}

HB_FUNC(WIN_AXINIT)
{
  hb_retl(hb_oleAxInit());
}

PHB_ITEM hb_oleAxControlNew(PHB_ITEM pItem, HWND hWnd)
{
  IUnknown *pUnk = nullptr;
  IDispatch *pDisp = nullptr;

  if (pItem)
  {
    hb_itemClear(pItem);
  }

  if (!hb_oleAxInit() || !s_pAtlAxGetControl)
  {
    hb_oleSetError(S_OK);
    hb_errRT_OLE(EG_UNSUPPORTED, 1010, 0, "ActiveX not initialized", HB_ERR_FUNCNAME);
  }
  else
  {
    HRESULT lOleError = (*s_pAtlAxGetControl)(hWnd, &pUnk);

    if (lOleError == S_OK)
    {
      lOleError = HB_VTBL(pUnk)->QueryInterface(HB_THIS_(pUnk) HB_ID_REF(IID_IDispatch),
                                                static_cast<void **>(static_cast<void *>(&pDisp)));

      if (lOleError == S_OK)
      {
        pItem = hb_oleItemPut(pItem, pDisp);
      }

      HB_VTBL(pUnk)->Release(HB_THIS(pUnk));
    }

    hb_oleSetError(lOleError);

    if (lOleError != S_OK)
    {
      hb_errRT_OLE(EG_ARG, 1011, static_cast<HB_ERRCODE>(lOleError), nullptr, HB_ERR_FUNCNAME);
    }
  }

  return pItem;
}

HB_FUNC(__AXGETCONTROL) // ( hWnd ) --> pDisp
{
  HWND hWnd = hbwapi_par_raw_HWND(1);

  if (!hWnd)
  {
    hb_errRT_OLE(EG_ARG, 1012, 0, nullptr, HB_ERR_FUNCNAME);
  }
  else
  {
    hb_oleAxControlNew(hb_stackReturnItem(), hWnd);
  }
}

HB_FUNC(__AXDOVERB) // ( hWndAx, iVerb ) --> hResult
{
  HWND hWnd = hbwapi_par_raw_HWND(1);
  IUnknown *pUnk = nullptr;
  HRESULT lOleError;

  if (!hb_oleAxInit() || !s_pAtlAxGetControl)
  {
    hb_oleSetError(S_OK);
    hb_errRT_OLE(EG_UNSUPPORTED, 1013, 0, "ActiveX not initialized", HB_ERR_FUNCNAME);
    return;
  }

  lOleError = (*s_pAtlAxGetControl)(hWnd, &pUnk);

  if (lOleError == S_OK)
  {
    IOleObject *lpOleObject = nullptr;

    lOleError = HB_VTBL(pUnk)->QueryInterface(HB_THIS_(pUnk) HB_ID_REF(IID_IOleObject),
                                              static_cast<void **>(static_cast<void *>(&lpOleObject)));
    if (lOleError == S_OK)
    {
      IOleClientSite *lpOleClientSite = nullptr;

      lOleError = HB_VTBL(lpOleObject)->GetClientSite(HB_THIS_(lpOleObject) & lpOleClientSite);
      if (lOleError == S_OK)
      {
        MSG Msg{};
        RECT rc;

        GetClientRect(hWnd, &rc);
        HB_VTBL(lpOleObject)->DoVerb(HB_THIS_(lpOleObject) hb_parni(2), &Msg, lpOleClientSite, 0, hWnd, &rc);
      }
      HB_VTBL(lpOleObject)->Release(HB_THIS(lpOleObject));
    }
  }

  hb_oleSetError(lOleError);

  hb_retnint(lOleError);
}

// --- Event handler support ---

#if !defined(HB_OLE_C_API)
typedef struct
{
  HRESULT(STDMETHODCALLTYPE *QueryInterface)(IDispatch *, REFIID, void **);
  ULONG(STDMETHODCALLTYPE *AddRef)(IDispatch *);
  ULONG(STDMETHODCALLTYPE *Release)(IDispatch *);
  HRESULT(STDMETHODCALLTYPE *GetTypeInfoCount)(IDispatch *, UINT *);
  HRESULT(STDMETHODCALLTYPE *GetTypeInfo)(IDispatch *, UINT, LCID, ITypeInfo **);
  HRESULT(STDMETHODCALLTYPE *GetIDsOfNames)(IDispatch *, REFIID, LPOLESTR *, UINT, LCID, DISPID *);
  HRESULT(STDMETHODCALLTYPE *Invoke)
  (IDispatch *, DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT *);
} IDispatchVtbl;
#endif

typedef struct
{
  const IDispatchVtbl *lpVtbl;
  DWORD count;
  IConnectionPoint *pConnectionPoint;
  DWORD dwCookie;
  IID rriid;
  PHB_ITEM pItemHandler;
  HB_USHORT uiClass;
} ISink;

static HRESULT STDMETHODCALLTYPE QueryInterface(IDispatch *lpThis, REFIID riid, void **ppRet)
{
  if (IsEqualIID(riid, HB_ID_REF(IID_IUnknown)) || IsEqualIID(riid, HB_ID_REF(IID_IDispatch)) ||
      IsEqualIID(riid, HB_ID_REF((reinterpret_cast<ISink *>(lpThis))->rriid)))
  {
    *ppRet = static_cast<void *>(lpThis);
    HB_VTBL(lpThis)->AddRef(HB_THIS(lpThis));
    return S_OK;
  }
  *ppRet = nullptr;
  return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE AddRef(IDispatch *lpThis)
{
  return ++(reinterpret_cast<ISink *>(lpThis))->count;
}

static ULONG STDMETHODCALLTYPE Release(IDispatch *lpThis)
{
  auto pSink = reinterpret_cast<ISink *>(lpThis);

  if (--pSink->count == 0)
  {
    if (pSink->pItemHandler)
    {
      hb_itemRelease(pSink->pItemHandler);
      pSink->pItemHandler = nullptr;
    }
    if (pSink->pConnectionPoint)
    {
      HB_VTBL(pSink->pConnectionPoint)->Unadvise(HB_THIS_(pSink->pConnectionPoint) pSink->dwCookie);
      HB_VTBL(pSink->pConnectionPoint)->Release(HB_THIS(pSink->pConnectionPoint));
      pSink->pConnectionPoint = nullptr;
      pSink->dwCookie = 0;
    }
    hb_xfree(pSink); // TODO: GlobalAlloc()/GlobalFree() GMEM_FIXED ???
    return 0;
  }
  return pSink->count;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfoCount(IDispatch *lpThis, UINT *pInfoCount)
{
  HB_SYMBOL_UNUSED(lpThis);
  HB_SYMBOL_UNUSED(pInfoCount);
  return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE GetTypeInfo(IDispatch *lpThis, UINT iTInfo, LCID lcid, ITypeInfo **ppTypeInfo)
{
  HB_SYMBOL_UNUSED(lpThis);
  HB_SYMBOL_UNUSED(iTInfo);
  HB_SYMBOL_UNUSED(lcid);
  HB_SYMBOL_UNUSED(ppTypeInfo);
  return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE GetIDsOfNames(IDispatch *lpThis, REFIID riid, LPOLESTR *rgszNames, UINT cNames,
                                               LCID lcid, DISPID *rgDispId)
{
  HB_SYMBOL_UNUSED(lpThis);
  HB_SYMBOL_UNUSED(riid);
  HB_SYMBOL_UNUSED(rgszNames);
  HB_SYMBOL_UNUSED(cNames);
  HB_SYMBOL_UNUSED(lcid);
  HB_SYMBOL_UNUSED(rgDispId);
  return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE Invoke(IDispatch *lpThis, DISPID dispid, REFIID riid, LCID lcid, WORD wFlags,
                                        DISPPARAMS *pParams, VARIANT *pVarResult, EXCEPINFO *pExcepInfo, UINT *puArgErr)
{
  PHB_ITEM pAction;
  HRESULT hr;

  HB_SYMBOL_UNUSED(lcid);
  HB_SYMBOL_UNUSED(wFlags);
  HB_SYMBOL_UNUSED(pExcepInfo);
  HB_SYMBOL_UNUSED(puArgErr);

  if (!IsEqualIID(riid, HB_ID_REF(IID_NULL)))
  {
    return DISP_E_UNKNOWNINTERFACE;
  }

  hr = DISP_E_MEMBERNOTFOUND;

  pAction = (reinterpret_cast<ISink *>(lpThis))->pItemHandler;
  if (pAction)
  {
    auto pKey = hb_itemPutNL(hb_stackAllocItem(), static_cast<long>(dispid));

    if (pAction && pAction->isHash())
    {
      pAction = hb_hashGetItemPtr(pAction, pKey, 0);
      pKey = nullptr;
    }

    if (pAction && hb_oleDispInvoke(nullptr, pAction, pKey, pParams, pVarResult, nullptr,
                                    (reinterpret_cast<ISink *>(lpThis))->uiClass))
    {
      hr = S_OK;
    }

    hb_stackPop();
  }

  return hr;
}

static const IDispatchVtbl ISink_Vtbl = {QueryInterface, AddRef,        Release, GetTypeInfoCount,
                                         GetTypeInfo,    GetIDsOfNames, Invoke};

#if 0
// Debug helper function
static char * GUID2String(GUID * pID)
{
   static char strguid[128];
   wchar_t olestr[128];
   int iLen;

   StringFromGUID2(pID, olestr, HB_SIZEOFARRAY(olestr));
   iLen = WideCharToMultiByte(CP_ACP, 0, olestr, -1, strguid, sizeof(strguid), nullptr, nullptr);
   if( iLen )
   {
      strguid[iLen - 1] = 0;
   }
   return strguid;
}
#endif

static HRESULT _get_default_sink(IDispatch *iDisp, const char *szEvent, IID *piid)
{
  ITypeInfo *iTI;
  ITypeInfo *iTISink;
  TYPEATTR *pTypeAttr;
  HREFTYPE hRefType;
  HRESULT hr;
  int iFlags;

  if (!szEvent)
  {
    IProvideClassInfo2 *iPCI2;
    IProvideClassInfo *iPCI;

    // Method 1: using IProvideClassInfo2

    hr = HB_VTBL(iDisp)->QueryInterface(HB_THIS_(iDisp) HB_ID_REF(IID_IProvideClassInfo2),
                                        static_cast<void **>(static_cast<void *>(&iPCI2)));
    if (hr == S_OK)
    {
      HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() IProvideClassInfo2 OK"));
      hr = HB_VTBL(iPCI2)->GetGUID(HB_THIS_(iPCI2) GUIDKIND_DEFAULT_SOURCE_DISP_IID, piid);
      HB_VTBL(iPCI2)->Release(HB_THIS(iPCI2));

      if (hr == S_OK)
      {
        return S_OK;
      }
    }
    else
    {
      HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() IProvideClassInfo2 obtain error %08lX", hr));
    }

    // Method 2: using IProvideClassInfo and searching for default source in ITypeInfo

    hr = HB_VTBL(iDisp)->QueryInterface(HB_THIS_(iDisp) HB_ID_REF(IID_IProvideClassInfo),
                                        static_cast<void **>(static_cast<void *>(&iPCI)));
    if (hr == S_OK)
    {
      HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() IProvideClassInfo OK"));

      iTI = nullptr;

      hr = HB_VTBL(iPCI)->GetClassInfo(HB_THIS_(iPCI) & iTI);
      if (hr == S_OK)
      {
        pTypeAttr = nullptr;

        hr = HB_VTBL(iTI)->GetTypeAttr(HB_THIS_(iTI) & pTypeAttr);
        if (hr == S_OK)
        {
          for (auto i = 0; i < pTypeAttr->cImplTypes; i++)
          {
            hr = HB_VTBL(iTI)->GetImplTypeFlags(HB_THIS_(iTI) i, &iFlags);
            if (hr == S_OK && (iFlags & IMPLTYPEFLAG_FDEFAULT) && (iFlags & IMPLTYPEFLAG_FSOURCE))
            {
              if (HB_VTBL(iTI)->GetRefTypeOfImplType(HB_THIS_(iTI) i, &hRefType) == S_OK &&
                  HB_VTBL(iTI)->GetRefTypeInfo(HB_THIS_(iTI) hRefType, &iTISink) == S_OK)
              {
                HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() Method 2: default source is found"));

                hr = HB_VTBL(iTISink)->GetTypeAttr(HB_THIS_(iTISink) & pTypeAttr);
                if (hr == S_OK)
                {
                  *piid = pTypeAttr->guid;
                  HB_VTBL(iTISink)->ReleaseTypeAttr(HB_THIS_(iTISink) pTypeAttr);

                  HB_VTBL(iTI)->ReleaseTypeAttr(HB_THIS_(iTI) pTypeAttr);
                  HB_VTBL(iPCI)->Release(HB_THIS(iPCI));
                  return S_OK;
                }
              }
            }
          }
          HB_VTBL(iTI)->ReleaseTypeAttr(HB_THIS_(iTI) pTypeAttr);
        }
      }
      HB_VTBL(iPCI)->Release(HB_THIS(iPCI));
    }
    else
    {
      HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() IProvideClassInfo obtain error %08lX", hr));
    }
  }

  // Method 3: using CoClass

  hr = HB_VTBL(iDisp)->GetTypeInfo(HB_THIS_(iDisp) 0, LOCALE_SYSTEM_DEFAULT, &iTI);
  if (hr == S_OK)
  {
    ITypeLib *iTL = nullptr;
    TYPEATTR *pTypeAttr2;

    hr = HB_VTBL(iTI)->GetContainingTypeLib(HB_THIS_(iTI) & iTL, nullptr);
    HB_VTBL(iTI)->Release(HB_THIS(iTI));

    if (hr == S_OK)
    {
      int iCount = HB_VTBL(iTL)->GetTypeInfoCount(HB_THIS(iTL));
      for (auto i = 0; i < iCount; i++)
      {
        hr = HB_VTBL(iTL)->GetTypeInfo(HB_THIS_(iTL) i, &iTI);
        if (hr == S_OK)
        {
          hr = HB_VTBL(iTI)->GetTypeAttr(HB_THIS_(iTI) & pTypeAttr);
          if (hr == S_OK)
          {
            if (pTypeAttr->typekind == TKIND_COCLASS)
            {
              for (auto j = 0; j < pTypeAttr->cImplTypes; j++)
              {
                if (szEvent)
                {
                  if (HB_VTBL(iTI)->GetRefTypeOfImplType(HB_THIS_(iTI) j, &hRefType) == S_OK &&
                      HB_VTBL(iTI)->GetRefTypeInfo(HB_THIS_(iTI) hRefType, &iTISink) == S_OK)
                  {
                    BSTR bstr;

                    hr = HB_VTBL(iTISink)->GetDocumentation(HB_THIS_(iTISink) - 1, &bstr, nullptr, nullptr, nullptr);
                    if (hr == S_OK)
                    {
                      char str[256];
                      int iLen;

                      iLen = WideCharToMultiByte(CP_ACP, 0, bstr, -1, str, sizeof(str), nullptr, nullptr);
                      if (iLen > 0)
                      {
                        str[iLen - 1] = '\0';
                        if (!strcmp(szEvent, str))
                        {
                          hr = HB_VTBL(iTISink)->GetTypeAttr(HB_THIS_(iTISink) & pTypeAttr2);
                          if (hr == S_OK)
                          {
                            *piid = pTypeAttr2->guid;
                            HB_VTBL(iTISink)->ReleaseTypeAttr(HB_THIS_(iTISink) pTypeAttr2);

                            HB_VTBL(iTISink)->Release(HB_THIS(iTISink));
                            HB_VTBL(iTI)->ReleaseTypeAttr(HB_THIS_(iTI) pTypeAttr);
                            HB_VTBL(iTI)->Release(HB_THIS(iTI));
                            HB_VTBL(iTL)->Release(HB_THIS(iTL));
                            return S_OK;
                          }
                        }
                      }
                    }
                    HB_VTBL(iTISink)->Release(HB_THIS(iTISink));
                  }
                }
                else // szEvent == nullptr
                {
                  hr = HB_VTBL(iTI)->GetImplTypeFlags(HB_THIS_(iTI) j, &iFlags);
                  if (hr == S_OK && (iFlags & IMPLTYPEFLAG_FDEFAULT) && (iFlags & IMPLTYPEFLAG_FSOURCE))
                  {
                    if (HB_VTBL(iTI)->GetRefTypeOfImplType(HB_THIS_(iTI) j, &hRefType) == S_OK &&
                        HB_VTBL(iTI)->GetRefTypeInfo(HB_THIS_(iTI) hRefType, &iTISink) == S_OK)
                    {
                      hr = HB_VTBL(iTISink)->GetTypeAttr(HB_THIS_(iTISink) & pTypeAttr2);
                      if (hr == S_OK)
                      {
#if 0
// Debug code. You can also comment out iFlags condition, to list more interfaces [Mindaugas]
                                    BSTR bstr;
                                    char str[256];
                                    int iLen;

                                    HB_VTBL(iTISink)->GetDocumentation(HB_THIS_(iTISink) - 1, &bstr, nullptr, nullptr, nullptr);
                                    iLen = WideCharToMultiByte(CP_ACP, 0, bstr, -1, str, sizeof(str), nullptr, nullptr);
                                    str[iLen - 1] = '\0';
                                    HB_TRACE(HB_TR_DEBUG, ("_get_default_sink() Method 3: iFlags=%d guid=%s class=%s", iFlags, GUID2String(&(pTypeAttr2->guid)), str));
#endif
                        *piid = pTypeAttr2->guid;
                        HB_VTBL(iTISink)->ReleaseTypeAttr(HB_THIS_(iTISink) pTypeAttr2);

                        HB_VTBL(iTI)->ReleaseTypeAttr(HB_THIS_(iTI) pTypeAttr);
                        HB_VTBL(iTI)->Release(HB_THIS(iTI));
                        HB_VTBL(iTL)->Release(HB_THIS(iTL));
                        return S_OK;
                      }
                    }
                  }
                }
              }
            }
            HB_VTBL(iTI)->ReleaseTypeAttr(HB_THIS_(iTI) pTypeAttr);
          }
          HB_VTBL(iTI)->Release(HB_THIS(iTI));
        }
      }
      HB_VTBL(iTL)->Release(HB_THIS(iTL));
    }
  }
  return E_NOINTERFACE;
}

static void hb_sink_destruct(void *cargo)
{
  auto pSink = static_cast<ISink *>(cargo);

  if (pSink->pConnectionPoint)
  {
    IConnectionPoint *pConnectionPoint = pSink->pConnectionPoint;
    DWORD dwCookie = pSink->dwCookie;

    // Unadvise() may activate pSink destructor so clear these
    // items as protection against recursive Unadvise() call.

    pSink->pConnectionPoint = nullptr;
    pSink->dwCookie = 0;

    HB_VTBL(pConnectionPoint)->Unadvise(HB_THIS_(pConnectionPoint) dwCookie);
    HB_VTBL(pConnectionPoint)->Release(HB_THIS(pConnectionPoint));
  }
}

HB_FUNC(__AXREGISTERHANDLER) // ( pDisp, bHandler [, cIID] ) --> pSink
{
  IDispatch *pDisp = hb_oleParam(1);

  if (pDisp)
  {
    auto pItemBlock = hb_param(2, Harbour::Item::EVALITEM | Harbour::Item::HASH);

    if (pItemBlock)
    {
      IConnectionPointContainer *pCPC = nullptr;
      IConnectionPoint *pCP = nullptr;
      HRESULT lOleError;
      IID rriid;
      void *hCLSID;

      hb_oleInit();

      auto szIID = hb_parc(3);
      if (szIID && szIID[0] == '{')
      {
        const wchar_t *wCLSID = hb_parstr_u16(3, HB_CDP_ENDIAN_NATIVE, &hCLSID, nullptr);
        lOleError = CLSIDFromString(static_cast<LPCOLESTR>(wCLSID), &rriid);
        hb_strfree(hCLSID);
      }
      else
      {
        lOleError = _get_default_sink(pDisp, szIID, &rriid);
      }

      if (lOleError == S_OK)
      {
#if 0
            HB_TRACE(HB_TR_DEBUG, ("__axRegisterHandler() using sink %s", GUID2String(&rriid)));
#endif
        lOleError = HB_VTBL(pDisp)->QueryInterface(HB_THIS_(pDisp) HB_ID_REF(IID_IConnectionPointContainer),
                                                   static_cast<void **>(static_cast<void *>(&pCPC)));

        if (lOleError == S_OK)
        {
          lOleError = HB_VTBL(pCPC)->FindConnectionPoint(HB_THIS_(pCPC) HB_ID_REF(rriid), &pCP);

          if (lOleError == S_OK)
          {
            PHB_ITEM pOleItem;
            DWORD dwCookie = 0;

            auto pSink = static_cast<ISink *>(hb_xgrab(sizeof(ISink))); // TODO: GlobalAlloc/Free GMEM_FIXED ???

            pSink->lpVtbl = &ISink_Vtbl;
            pSink->count = 0;
            pSink->pItemHandler = hb_itemNew(pItemBlock);
            pSink->rriid = rriid;
            pSink->uiClass = 0;
            if ((lOleError = HB_VTBL(pCP)->Advise(HB_THIS_(pCP) reinterpret_cast<IUnknown *>(pSink), &dwCookie)) !=
                S_OK)
            {
              dwCookie = 0;
            }
            pSink->pConnectionPoint = pCP;
            pSink->dwCookie = dwCookie;

            HB_VTBL(pDisp)->AddRef(HB_THIS(pDisp));
            pOleItem = hb_oleItemPut(hb_stackReturnItem(), static_cast<IDispatch *>(pDisp));
            // Bind call back handler item with returned object
            hb_oleItemSetCallBack(pOleItem, &pSink->pItemHandler);
            // Add additional destructor
            hb_oleItemSetDestructor(pOleItem, hb_sink_destruct, static_cast<void *>(pSink));
          }
          HB_VTBL(pCPC)->Release(HB_THIS(pCPC));
        }
      }

      hb_oleSetError(lOleError);
      if (lOleError != S_OK)
      {
        hb_ret();
      }
    }
    else
    {
      hb_errRT_OLE(EG_ARG, 1015, 0, nullptr, HB_ERR_FUNCNAME);
    }
  }
}
