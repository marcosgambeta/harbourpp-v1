//
// OLE server
//
// Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if !defined(HB_DYNLIB)

#include <hbapi.hpp>

#include <hbwinuni.hpp>
#include "hbwinole.hpp"
#include <olectl.h>

#define MAX_CLSID_SIZE 64
#define MAX_CLSNAME_SIZE 256
#define MAX_REGSTR_SIZE (MAX_CLSNAME_SIZE + 64)

static const LPCTSTR s_regTable[][3] = {{TEXT("CLSID\\@"), nullptr, TEXT("$")},
                                        {TEXT("CLSID\\@\\InprocServer32"), nullptr, reinterpret_cast<LPCTSTR>(-1)},
                                        {TEXT("CLSID\\@\\InprocServer32"), TEXT("ThreadingModel"), TEXT("Apartment")},
                                        {TEXT("CLSID\\@\\ProgId"), nullptr, TEXT("$")},
                                        {TEXT("$"), nullptr, TEXT("$")},
                                        {TEXT("$\\CLSID"), nullptr, TEXT("@")}};

static LONG s_lLockCount;
static LONG s_lObjectCount;

static GUID s_IID_IHbOleServer;

static TCHAR s_lpClsId[MAX_CLSID_SIZE] = TEXT("");
static TCHAR s_lpClsName[MAX_CLSNAME_SIZE] = TEXT("");

static bool s_fServerReady = false;
static bool s_fHashClone = false;
static PHB_ITEM s_pAction = nullptr;
static PHB_ITEM s_pMsgHash = nullptr;

static HINSTANCE s_hInstDll;

static HB_BOOL s_objItemToVariant(VARIANT *pVariant, PHB_ITEM pItem);

// helper functions

static DISPID hb_dynsymToDispId(PHB_DYNS pDynSym)
{
  return static_cast<DISPID>(hb_dynsymToNum(pDynSym));
}

static PHB_DYNS hb_dispIdToDynsym(DISPID dispid)
{
  if (static_cast<LONG>(dispid) > 0) {
    return hb_dynsymFromNum(static_cast<HB_SYMCNT>(dispid));
  } else {
    return nullptr;
  }
}

static void hb_errRT_OLESRV(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode,
                            const char *szDescription, const char *szOperation)
{
  auto pError =
      hb_errRT_New(ES_ERROR, "OLESERVER", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
  if (hb_pcount() != 0) {
    // HB_ERR_ARGS_BASEPARAMS
    PHB_ITEM pArray = hb_arrayBaseParams();
    hb_errPutArgsArray(pError, pArray);
    hb_itemRelease(pArray);
  }
  hb_errLaunch(pError);
  hb_errRelease(pError);
}

static HB_BOOL s_hashWithNumKeys(PHB_ITEM pHash)
{
  HB_SIZE nLen = hb_hashLen(pHash);

  for (HB_SIZE n = 1; n <= nLen; ++n) {
    PHB_ITEM pKey = hb_hashGetKeyAt(pHash, n);
    if (!pKey || !pKey->isNumeric()) {
      return false;
    }
  }

  return true;
}

static int s_WideToAnsiBuffer(const wchar_t *wszString, char *szBuffer, int iLen)
{
  int iResult = WideCharToMultiByte(CP_ACP, 0, wszString, -1, szBuffer, iLen, nullptr, nullptr);
  szBuffer[iLen - 1] = '\0';
  return iResult;
}

static HB_BOOL s_getKeyValue(LPCTSTR lpKey, LPTSTR lpBuffer, int iLen)
{
  LPTSTR lpPtr;
  int iSize, iPos, iCount;

  if (lpKey == reinterpret_cast<LPCTSTR>(-1)) {
    return GetModuleFileName(s_hInstDll, lpBuffer, iLen);
  }

  lpPtr = lpBuffer;
  iSize = iLen - 1;
  iPos = 0;
  for (;;) {
    TCHAR c = lpKey[iPos++];
    if (c == TEXT('$') || c == TEXT('@') || c == TEXT('\0')) {
      if (--iPos) {
        iCount = HB_MIN(iPos, iSize);
        memcpy(lpPtr, lpKey, iCount * sizeof(TCHAR));
        lpKey += iPos;
        lpPtr += iCount;
        iSize -= iCount;
        if (iSize == 0) {
          break;
        }
        iPos = 0;
      }
      if (c == TEXT('\0')) {
        break;
      } else {
        LPCTSTR lpVal = c == TEXT('$') ? s_lpClsName : s_lpClsId;
        iCount = static_cast<int>(HB_STRNLEN(lpVal, iSize));
        memcpy(lpPtr, lpVal, iCount * sizeof(TCHAR));
        lpKey++;
        lpPtr += iCount;
        iSize -= iCount;
        if (iSize == 0) {
          break;
        }
      }
    }
  }
  *lpPtr = TEXT('\0');

  return iSize != 0;
}

// IHbOleServer

#if !defined(HB_OLE_C_API)
struct IDispatchVtbl
{
  HRESULT(STDMETHODCALLTYPE *QueryInterface)(IDispatch *, REFIID, void **);
  ULONG(STDMETHODCALLTYPE *AddRef)(IDispatch *);
  ULONG(STDMETHODCALLTYPE *Release)(IDispatch *);
  HRESULT(STDMETHODCALLTYPE *GetTypeInfoCount)(IDispatch *, UINT *);
  HRESULT(STDMETHODCALLTYPE *GetTypeInfo)(IDispatch *, UINT, LCID, ITypeInfo **);
  HRESULT(STDMETHODCALLTYPE *GetIDsOfNames)(IDispatch *, REFIID, LPOLESTR *, UINT, LCID, DISPID *);
  HRESULT(STDMETHODCALLTYPE *Invoke)
  (IDispatch *, DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT *);
};
#endif

struct IHbOleServer
{
  const IDispatchVtbl *lpVtbl;
  DWORD count;
  PHB_ITEM pAction;
  HB_BOOL fGuids;
};

static HRESULT STDMETHODCALLTYPE QueryInterface(IDispatch *lpThis, REFIID riid, void **ppRet)
{
  if (IsEqualIID(riid, HB_ID_REF(IID_IUnknown)) || IsEqualIID(riid, HB_ID_REF(IID_IDispatch))) {
    *ppRet = static_cast<void *>(lpThis);
    HB_VTBL(lpThis)->AddRef(HB_THIS(lpThis));
    return S_OK;
  }
  *ppRet = nullptr;
  return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE AddRef(IDispatch *lpThis)
{
  return ++(reinterpret_cast<IHbOleServer *>(lpThis))->count;
}

static ULONG STDMETHODCALLTYPE Release(IDispatch *lpThis)
{
  auto pHbOleServer = reinterpret_cast<IHbOleServer *>(lpThis);

  if (--pHbOleServer->count == 0) {
    if (pHbOleServer->pAction) {
      hb_itemRelease(pHbOleServer->pAction);
      pHbOleServer->pAction = nullptr;
    }
    hb_xfree(pHbOleServer);
    InterlockedDecrement(&s_lObjectCount);
    return 0;
  }
  return pHbOleServer->count;
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
  HRESULT hr = S_OK;

  HB_SYMBOL_UNUSED(lcid);

  if (!IsEqualIID(riid, HB_ID_REF(IID_NULL))) {
    hr = DISP_E_UNKNOWNINTERFACE;
  } else if (((IHbOleServer *)lpThis)->fGuids) {
    return E_NOTIMPL;
  } else if (cNames > 0) {
    char szName[HB_SYMBOL_NAME_LEN + 1];
    DISPID dispid = 0;

    if (s_WideToAnsiBuffer(rgszNames[0], szName, static_cast<int>(sizeof(szName))) != 0) {
      PHB_ITEM pAction;

      pAction = (reinterpret_cast<IHbOleServer *>(lpThis))->pAction;
      if (!pAction) {
        pAction = s_pAction;
      }
      if (pAction) {
        if (s_pMsgHash) {
          HB_SIZE nPos = hb_hashGetCItemPos(s_pMsgHash, szName);

          if (nPos == 0) {
            auto pKey = hb_itemPutC(hb_stackAllocItem(), szName);
            if (hb_hashAdd(s_pMsgHash, pKey, nullptr)) {
              hb_hashScan(s_pMsgHash, pKey, &nPos);
            }
            hb_stackPop();
          }
          dispid = static_cast<DISPID>(nPos);
        } else if (pAction->isHash()) {
          HB_SIZE nPos = hb_hashGetCItemPos(pAction, szName);

          if (nPos) {
            dispid = static_cast<DISPID>(nPos);
          }
        } else if (pAction->isObject()) {
          auto pDynSym = hb_dynsymFindName(szName);

          if (pDynSym && hb_objHasMessage(pAction, pDynSym)) {
            dispid = hb_dynsymToDispId(pDynSym);
          }
        }
      } else {
        auto pDynSym = hb_dynsymFindName(szName);

        if (pDynSym && (hb_dynsymIsFunction(pDynSym) || hb_dynsymIsMemvar(pDynSym))) {
          dispid = hb_dynsymToDispId(pDynSym);
        }
      }
    }

    for (UINT ui = 0; ui < cNames; ++ui) {
      rgDispId[ui] = DISPID_UNKNOWN;
    }

    hr = DISP_E_UNKNOWNNAME;
    if (dispid) {
      rgDispId[0] = dispid;
      if (cNames == 1) {
        hr = S_OK;
      }
    }
  }

  return hr;
}

static HRESULT STDMETHODCALLTYPE Invoke(IDispatch *lpThis, DISPID dispid, REFIID riid, LCID lcid, WORD wFlags,
                                        DISPPARAMS *pParams, VARIANT *pVarResult, EXCEPINFO *pExcepInfo, UINT *puArgErr)
{
  PHB_DYNS pDynSym;
  PHB_ITEM pAction;
  HB_USHORT uiClass = 0;

  HB_SYMBOL_UNUSED(lcid);
  HB_SYMBOL_UNUSED(pExcepInfo);
  HB_SYMBOL_UNUSED(puArgErr);

  if (!IsEqualIID(riid, HB_ID_REF(IID_NULL))) {
    return DISP_E_UNKNOWNINTERFACE;
  }

  pAction = (reinterpret_cast<IHbOleServer *>(lpThis))->pAction;
  if (!pAction) {
    pAction = s_pAction;
  }

  if (pAction) {
    HB_BOOL fResult = false;

    if (s_pMsgHash) {
      if ((wFlags & DISPATCH_METHOD) != 0 || ((wFlags & DISPATCH_PROPERTYGET) != 0 && pParams->cArgs == 0) ||
          ((wFlags & DISPATCH_PROPERTYPUT) != 0 && pParams->cArgs == 1)) {
        fResult = hb_oleDispInvoke(nullptr, pAction, hb_hashGetKeyAt(s_pMsgHash, static_cast<HB_SIZE>(dispid)), pParams,
                                   pVarResult, s_objItemToVariant, uiClass);
      }
    } else if (pAction->isHash()) {
      PHB_ITEM pItem;

      if ((reinterpret_cast<IHbOleServer *>(lpThis))->fGuids) {
        auto pKey = hb_itemPutNL(hb_stackAllocItem(), static_cast<long>(dispid));
        pItem = hb_hashGetItemPtr(pAction, pKey, 0);
        hb_stackPop();
      } else {
        pItem = hb_hashGetValueAt(pAction, static_cast<HB_SIZE>(dispid));
      }

      if (pItem != nullptr) {
        if (pItem->isEvalItem()) {
          if ((wFlags & DISPATCH_METHOD) != 0) {
            PHB_SYMB pSym = hb_itemGetSymbol(pItem);
            fResult = hb_oleDispInvoke(pSym, pSym ? pAction : pItem, nullptr, pParams, pVarResult, s_objItemToVariant,
                                       uiClass);
          }
        } else if ((wFlags & DISPATCH_PROPERTYGET) != 0 && pParams->cArgs == 0) {
          if (pVarResult) {
            hb_oleItemToVariantEx(pVarResult, pItem, s_objItemToVariant);
          }
          fResult = true;
        } else if ((wFlags & DISPATCH_PROPERTYPUT) != 0 && pParams->cArgs == 1) {
          hb_oleVariantToItemEx(pItem, &pParams->rgvarg[0], uiClass);
          fResult = true;
        }
      }
    } else if (pAction->isObject()) {
      pDynSym = hb_dispIdToDynsym(dispid);
      if (pDynSym && (wFlags & DISPATCH_PROPERTYPUT) != 0) {
        if (pParams->cArgs == 1) {
          char szName[HB_SYMBOL_NAME_LEN + 1];
          szName[0] = '_';
          hb_strncpy(szName + 1, hb_dynsymName(pDynSym), sizeof(szName) - 2);
          pDynSym = hb_dynsymFindName(szName);
        } else {
          pDynSym = nullptr;
        }
      }
      if (pDynSym && hb_objHasMessage(pAction, pDynSym)) {
        fResult = hb_oleDispInvoke(hb_dynsymSymbol(pDynSym), pAction, nullptr, pParams, pVarResult, s_objItemToVariant,
                                   uiClass);
      }
    }
    if (!fResult) {
      return DISP_E_MEMBERNOTFOUND;
    }
  } else {
    pDynSym = hb_dispIdToDynsym(dispid);
    if (!pDynSym) {
      return DISP_E_MEMBERNOTFOUND;
    }

    if (wFlags & DISPATCH_PROPERTYPUT) {
      if (pParams->cArgs == 1 && hb_dynsymIsMemvar(pDynSym)) {
        auto pItem = hb_stackAllocItem();

        hb_oleVariantToItemEx(pItem, &pParams->rgvarg[0], uiClass);
        hb_memvarSetValue(hb_dynsymSymbol(pDynSym), pItem);
        hb_stackPop();
        return S_OK;
      } else {
        return DISP_E_MEMBERNOTFOUND;
      }
    } else if ((wFlags & DISPATCH_PROPERTYGET) && pParams->cArgs == 0 && hb_dynsymIsMemvar(pDynSym)) {
      if (pVarResult) {
        auto pItem = hb_stackAllocItem();
        hb_memvarGet(pItem, hb_dynsymSymbol(pDynSym));
        hb_oleItemToVariantEx(pVarResult, pItem, s_objItemToVariant);
        hb_stackPop();
      }
      return S_OK;
    } else if ((wFlags & DISPATCH_METHOD) == 0 || !hb_dynsymIsFunction(pDynSym)) {
      return DISP_E_MEMBERNOTFOUND;
    } else if (!hb_oleDispInvoke(hb_dynsymSymbol(pDynSym), nullptr, nullptr, pParams, pVarResult, s_objItemToVariant,
                                 uiClass)) {
      return DISP_E_MEMBERNOTFOUND;
    }
  }

  return S_OK;
}

static const IDispatchVtbl IHbOleServer_Vtbl = {QueryInterface, AddRef,        Release, GetTypeInfoCount,
                                                GetTypeInfo,    GetIDsOfNames, Invoke};

// IClassFactory object

#if !defined(HB_OLE_C_API)
struct IClassFactoryVtbl
{
  HRESULT(STDMETHODCALLTYPE *QueryInterface)(IClassFactory *, REFIID, void **);
  ULONG(STDMETHODCALLTYPE *AddRef)(IClassFactory *);
  ULONG(STDMETHODCALLTYPE *Release)(IClassFactory *);
  HRESULT(STDMETHODCALLTYPE *CreateInstance)(IClassFactory *, IUnknown *, REFIID, void **);
  HRESULT(STDMETHODCALLTYPE *LockServer)(IClassFactory *, BOOL);
};
#endif

struct IHbClassFactory
{
  const IClassFactoryVtbl *lpVtbl;
};

static IHbClassFactory s_IClassFactoryObj;

static HRESULT STDMETHODCALLTYPE classQueryInterface(IClassFactory *lpThis, REFIID riid, void **ppRet)
{
  if (IsEqualIID(riid, HB_ID_REF(IID_IUnknown)) || IsEqualIID(riid, HB_ID_REF(IID_IClassFactory))) {
    *ppRet = static_cast<void *>(lpThis);
    HB_VTBL(lpThis)->AddRef(HB_THIS(lpThis));
    return S_OK;
  }
  *ppRet = nullptr;
  return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE classAddRef(IClassFactory *lpThis)
{
  HB_SYMBOL_UNUSED(lpThis);

  InterlockedIncrement(&s_lObjectCount);
  return 1;
}

static ULONG STDMETHODCALLTYPE classRelease(IClassFactory *lpThis)
{
  HB_SYMBOL_UNUSED(lpThis);

  return InterlockedDecrement(&s_lObjectCount);
}

static HRESULT s_createHbOleObject(REFIID riid, void **ppvObj, PHB_ITEM pAction, HB_BOOL fGuids)
{
  HRESULT hr;
  auto thisobj = static_cast<IHbOleServer *>(hb_xalloc(sizeof(IHbOleServer)));

  if (!thisobj) {
    if (pAction) {
      hb_itemRelease(pAction);
    }
    hr = E_OUTOFMEMORY;
  } else {
    InterlockedIncrement(&s_lObjectCount);

    thisobj->lpVtbl = &IHbOleServer_Vtbl;
    thisobj->count = 1;
    thisobj->pAction = pAction;
    thisobj->fGuids = fGuids;

    hr = IHbOleServer_Vtbl.QueryInterface(reinterpret_cast<IDispatch *>(thisobj), riid, ppvObj);
    IHbOleServer_Vtbl.Release(reinterpret_cast<IDispatch *>(thisobj));
  }
  return hr;
}

static HB_BOOL s_objItemToVariant(VARIANT *pVariant, PHB_ITEM pItem)
{
  void *pvObj;

  VariantClear(pVariant);

  if (s_createHbOleObject(HB_ID_REF(IID_IDispatch), &pvObj, hb_itemNew(pItem), false) == S_OK) {
    V_VT(pVariant) = VT_DISPATCH;
    V_DISPATCH(pVariant) = static_cast<IDispatch *>(pvObj);
    return true;
  }
  return false;
}

static HRESULT STDMETHODCALLTYPE classCreateInstance(IClassFactory *lpThis, IUnknown *punkOuter, REFIID riid,
                                                     void **ppvObj)
{
  HRESULT hr;

  HB_SYMBOL_UNUSED(lpThis);

  *ppvObj = nullptr;

  if (punkOuter) {
    hr = CLASS_E_NOAGGREGATION;
  } else {
    PHB_ITEM pAction = nullptr;
    HB_BOOL fGuids = false;

    if (s_pAction) {
      if (s_pAction->isEvalItem()) {
        if (hb_vmRequestReenter()) {
          hb_vmPushEvalSym();
          hb_vmPush(s_pAction);
          hb_vmProc(0);
          pAction = hb_itemNew(hb_stackReturnItem());
          hb_vmRequestRestore();
        }
      } else if (s_pAction->isHash()) {
        if (s_fHashClone) {
          pAction = hb_itemClone(s_pAction);
        } else if (!s_pMsgHash && s_hashWithNumKeys(s_pAction)) {
          fGuids = true;
        }
      }
    }
    hr = s_createHbOleObject(riid, ppvObj, pAction, fGuids);
  }
  return hr;
}

static HRESULT STDMETHODCALLTYPE classLockServer(IClassFactory *lpThis, BOOL fLock)
{
  HB_SYMBOL_UNUSED(lpThis);

  if (fLock) {
    InterlockedIncrement(&s_lLockCount);
  } else {
    InterlockedDecrement(&s_lLockCount);
  }

  return S_OK;
}

static const IClassFactoryVtbl IClassFactory_Vtbl = {classQueryInterface, classAddRef, classRelease,
                                                     classCreateInstance, classLockServer};

// OLE InProc DLL server API

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void **ppv)
{
  HRESULT hr;

  if (IsEqualCLSID(rclsid, HB_ID_REF(s_IID_IHbOleServer))) {
    hr = classQueryInterface(static_cast<IClassFactory *>(static_cast<void *>(&s_IClassFactoryObj)), riid, ppv);
  } else {
    *ppv = nullptr;
    hr = CLASS_E_CLASSNOTAVAILABLE;
  }

  return hr;
}

STDAPI DllCanUnloadNow(void)
{
  return (s_lObjectCount | s_lLockCount) ? S_FALSE : S_OK;
}

// server register/unregister code

STDAPI DllUnregisterServer(void)
{
  TCHAR lpKeyName[MAX_REGSTR_SIZE];

  for (int i = static_cast<int>(HB_SIZEOFARRAY(s_regTable)) - 1; i >= 0; --i) {
    if (s_getKeyValue(s_regTable[i][0], lpKeyName, MAX_REGSTR_SIZE)) {
      RegDeleteKey(HKEY_CLASSES_ROOT, lpKeyName);
    }
  }

  return S_OK;
}

#ifndef SELFREG_E_CLASS
#ifndef SELFREG_E_FIRST
#define SELFREG_E_FIRST MAKE_SCODE(SEVERITY_ERROR, FACILITY_ITF, 0x0200)
#endif
#define SELFREG_E_CLASS (SELFREG_E_FIRST + 1)
#endif

STDAPI DllRegisterServer(void)
{
  TCHAR lpKeyName[MAX_REGSTR_SIZE];
  TCHAR lpNameBuf[MAX_REGSTR_SIZE];
  TCHAR lpValue[MAX_REGSTR_SIZE];
  LPCTSTR lpValName;
  HRESULT hr = S_OK;
  HKEY hKey;

  for (auto i = 0; i < static_cast<int>(HB_SIZEOFARRAY(s_regTable)); ++i) {
    long err;

    s_getKeyValue(s_regTable[i][0], lpKeyName, MAX_REGSTR_SIZE);
    if (s_regTable[i][1]) {
      s_getKeyValue(s_regTable[i][1], lpNameBuf, MAX_REGSTR_SIZE);
      lpValName = lpNameBuf;
    } else {
      lpValName = nullptr;
    }
    s_getKeyValue(s_regTable[i][2], lpValue, MAX_REGSTR_SIZE);

    err = RegCreateKeyEx(HKEY_CLASSES_ROOT, lpKeyName, 0, nullptr, REG_OPTION_NON_VOLATILE,
                         KEY_SET_VALUE | KEY_CREATE_SUB_KEY, nullptr, &hKey, nullptr);

    if (err == ERROR_SUCCESS) {
      err = RegSetValueEx(hKey, lpValName, 0, REG_SZ, reinterpret_cast<const BYTE *>(lpValue),
                          (lstrlen(lpValue) + 1) * sizeof(TCHAR));
      RegCloseKey(hKey);
    }
    if (err != ERROR_SUCCESS) {
      DllUnregisterServer();
      hr = SELFREG_E_CLASS;
      break;
    }
  }

  return hr;
}

BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD dwReason, PVOID pvReserved)
{
  static HB_BOOL s_fInit = false;
  BOOL fResult = TRUE;

  HB_SYMBOL_UNUSED(pvReserved);

  switch (dwReason) {
  case DLL_PROCESS_ATTACH:
    s_hInstDll = static_cast<HINSTANCE>(hInstance);
    s_lLockCount = s_lObjectCount = 0;
    s_IClassFactoryObj.lpVtbl = const_cast<IClassFactoryVtbl *>(&IClassFactory_Vtbl);

    DisableThreadLibraryCalls(static_cast<HMODULE>(hInstance));

    s_fInit = !hb_vmIsActive();
    if (s_fInit) {
      hb_vmInit(false);
    }

    hb_oleInit();

    if (!s_fServerReady) {
      auto pDynSym = hb_dynsymFind("DLLMAIN");

      if (pDynSym && hb_dynsymIsFunction(pDynSym) && hb_vmRequestReenter()) {
        hb_vmPushDynSym(pDynSym);
        hb_vmPushNil();
        hb_vmProc(0);
        hb_vmRequestRestore();
      }
    }
    fResult = s_fServerReady ? TRUE : FALSE;
    break;

  case DLL_PROCESS_DETACH:
    s_fServerReady = false;
    if (s_pAction) {
      hb_itemRelease(s_pAction);
      s_pAction = nullptr;
    }
    if (s_pMsgHash) {
      hb_itemRelease(s_pMsgHash);
      s_pMsgHash = nullptr;
    }
    if (s_fInit) {
      hb_vmQuit();
      s_fInit = false;
    }
    break;
  }

  return fResult;
}

// win_oleServerInit(<cClassID>, <cServerName>, [<hAction> | <oAction> | <bAction> | <sAction>], [<lHashClone> |
// <lAcceptAll>])
HB_FUNC(WIN_OLESERVERINIT)
{
  HB_ERRCODE errCode = 0;

  if (!s_fServerReady) {
    void *hClsId, *hClsName;
    LPCTSTR lpClsId, lpClsName;

    lpClsId = HB_PARSTR(1, &hClsId, nullptr);
    lpClsName = HB_PARSTR(2, &hClsName, nullptr);

    if (lpClsId && lpClsName) {
      void *hOleClsId;
      LPCOLESTR lpOleClsId;

      lpOleClsId = hb_parstr_u16(1, HB_CDP_ENDIAN_NATIVE, &hOleClsId, nullptr);
      if (CLSIDFromString(const_cast<LPOLESTR>(lpOleClsId), &s_IID_IHbOleServer) == S_OK) {
        s_fHashClone = false;
        if (s_pMsgHash) {
          hb_itemRelease(s_pMsgHash);
          s_pMsgHash = nullptr;
        }

        auto pAction = hb_param(3, Harbour::Item::HASH | Harbour::Item::BLOCK | Harbour::Item::SYMBOL);
        if (!pAction && HB_ISOBJECT(3)) {
          pAction = hb_param(3, Harbour::Item::OBJECT);
        }
        if (pAction) {
          if (s_pAction) {
            hb_itemRelease(s_pAction);
          }
          s_pAction = hb_itemNew(pAction);

          if (HB_ISLOG(4)) {
            if (hb_parl(4)) {
              if (s_pAction->isHash()) {
                s_fHashClone = true;
              } else {
                s_pMsgHash = hb_hashNew(hb_itemNew(nullptr));
              }
            }
          } else if (!HB_ISNIL(4)) {
            errCode = 1001;
          }
        } else if (!HB_ISNIL(3)) {
          errCode = 1001;
        }

        HB_STRNCPY(s_lpClsId, lpClsId, HB_SIZEOFARRAY(s_lpClsId) - 1);
        HB_STRNCPY(s_lpClsName, lpClsName, HB_SIZEOFARRAY(s_lpClsName) - 1);

        s_fServerReady = true;
      } else {
        errCode = 1002;
      }

      hb_strfree(hOleClsId);
    } else {
      errCode = 1001;
    }

    hb_strfree(hClsId);
    hb_strfree(hClsName);
  }

  if (errCode) {
    hb_errRT_OLESRV(EG_ARG, errCode, 0, nullptr, HB_ERR_FUNCNAME);
  } else {
    hb_retl(s_fServerReady);
  }
}

#endif
