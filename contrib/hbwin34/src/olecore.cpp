/*
 * OLE library
 *
 * Copyright 2000, 2003 Jose F. Gimenez (JFG) <jfgimenez@wanadoo.es>
 * Copyright 2008, 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbwinole.hpp"
#include "hbapicdp.hpp"
#include "hbapilng.hpp"
#include "hbapistr.hpp"
#include "hbinit.hpp"

/* enable workaround for wrong OLE variant structure definition */
#if (defined(_MSC_VER) && (_MSC_VER <= 1500))
#define HB_OLE_NO_LL
#endif

#if defined(__MINGW32__) || (defined(_MSC_VER) && (_MSC_VER <= 1500))
#define HB_OLE_NO_LLREF
#endif

#if !defined(HB_OLE_NO_SAFEARRAYGETVARTYPE) && defined(__MINGW32CE__)
#define HB_OLE_NO_SAFEARRAYGETVARTYPE
#endif

/* base date value in OLE (1899-12-30) as Julian day */
#define HB_OLE_DATE_BASE 0x0024D9AB

static PHB_DYNS s_pDyns_hb_oleauto;
static PHB_DYNS s_pDyns_hObjAccess;
static PHB_DYNS s_pDyns_hObjAssign;

typedef struct
{
  IDispatch *pDisp;
  PHB_ITEM *pCallBack;
  HB_OLE_DESTRUCTOR_FUNC pDestructorFunc;
  void *cargo;
} HB_OLE;

typedef struct
{
  HRESULT lOleError;
  HB_BOOL fNullDate;
  HB_BOOL fNil2Null;
  int iInit;
} HB_OLEDATA, *PHB_OLEDATA;

static void hb_oleDataInit(void *cargo)
{
  auto pOleData = static_cast<PHB_OLEDATA>(cargo);

  /* default settings: */
  pOleData->fNullDate = false;
  pOleData->fNil2Null = false;

  if (OleInitialize(nullptr) == S_OK)
  {
    pOleData->iInit = 1;
  }
}

static void hb_oleDataRelease(void *cargo)
{
  auto pOleData = static_cast<PHB_OLEDATA>(cargo);

  if (pOleData->iInit)
  {
    OleUninitialize();
  }
}

static HB_TSD_NEW(s_oleData, sizeof(HB_OLEDATA), hb_oleDataInit, hb_oleDataRelease);
#define hb_getOleData() (static_cast<PHB_OLEDATA>(hb_stackGetTSD(&s_oleData)))

HB_FUNC_EXTERN(WIN_OLEAUTO);

HB_BOOL hb_oleInit(void)
{
  return hb_getOleData()->iInit != 0;
}

void hb_oleSetError(HRESULT lOleError)
{
  hb_getOleData()->lOleError = lOleError;
}

HRESULT hb_oleGetError(void)
{
  return hb_getOleData()->lOleError;
}

static void hb_oleSetNullDateFlag(HB_BOOL fNullDate)
{
  hb_getOleData()->fNullDate = fNullDate;
}

static HB_BOOL hb_oleGetNullDateFlag(void)
{
  return hb_getOleData()->fNullDate;
}

static void hb_oleSetNil2NullFlag(HB_BOOL fNil2Null)
{
  hb_getOleData()->fNil2Null = fNil2Null;
}

static HB_BOOL hb_oleGetNil2NullFlag(void)
{
  return hb_getOleData()->fNil2Null;
}

static void hb_olecore_init(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  s_pDyns_hb_oleauto = hb_dynsymGetCase("WIN_OLEAUTO");
  s_pDyns_hObjAccess = hb_dynsymGetCase("__HOBJ");
  s_pDyns_hObjAssign = hb_dynsymGetCase("___HOBJ");

  if (s_pDyns_hObjAccess == s_pDyns_hObjAssign)
  {
    /* Never executed. Just force linkage */
    HB_FUNC_EXEC(WIN_OLEAUTO);
  }
}

static HB_GARBAGE_FUNC(hb_ole_destructor)
{
  auto pOle = static_cast<HB_OLE *>(Cargo);
  IDispatch *pDisp = pOle->pDisp;

  if (pDisp)
  {
    pOle->pDisp = nullptr;
    if (pOle->pCallBack && *pOle->pCallBack)
    {
      PHB_ITEM pCallBack = *pOle->pCallBack;
      *pOle->pCallBack = nullptr;
      pOle->pCallBack = nullptr;
      hb_itemRelease(pCallBack);
    }
    if (pOle->pDestructorFunc)
    {
      pOle->pDestructorFunc(pOle->cargo);
      pOle->pDestructorFunc = nullptr;
    }
    HB_VTBL(pDisp)->Release(HB_THIS(pDisp));
  }
}

static HB_GARBAGE_FUNC(hb_ole_mark)
{
  auto pOle = static_cast<HB_OLE *>(Cargo);

  if (pOle->pCallBack && *pOle->pCallBack)
  {
    hb_gcMark(*pOle->pCallBack);
  }
}

static const HB_GC_FUNCS s_gcOleFuncs = {hb_ole_destructor, hb_ole_mark};

static HB_GARBAGE_FUNC(hb_oleenum_destructor)
{
  auto ppEnum = static_cast<IEnumVARIANT **>(Cargo);
  IEnumVARIANT *pEnum = *ppEnum;

  if (pEnum)
  {
    *ppEnum = nullptr;
    HB_VTBL(pEnum)->Release(HB_THIS(pEnum));
  }
}

static const HB_GC_FUNCS s_gcOleenumFuncs = {hb_oleenum_destructor, hb_gcDummyMark};

static HB_GARBAGE_FUNC(hb_olevariant_destructor)
{
  auto pVariant = static_cast<VARIANT *>(Cargo);

  VariantClear(pVariant);
}

static const HB_GC_FUNCS s_gcVariantFuncs = {hb_olevariant_destructor, hb_gcDummyMark};

static void hb_errRT_OLE(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode, const char *szDescription,
                         const char *szOperation, const char *szFileName)
{
  auto pError =
      hb_errRT_New(ES_ERROR, "WINOLE", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);

  if (hb_pcount() != 0)
  {
    /* HB_ERR_ARGS_BASEPARAMS */
    PHB_ITEM pArray = hb_arrayBaseParams();
    hb_errPutArgsArray(pError, pArray);
    hb_itemRelease(pArray);
  }

  if (szFileName)
  {
    hb_errPutFileName(pError, szFileName);
  }

  hb_errLaunch(pError);
  hb_errRelease(pError);
}

static void hb_oleExcepDescription(EXCEPINFO *pExcep, char **pszDescription, char **pszSource)
{
  if (pExcep->pfnDeferredFillIn)
  {
    (*pExcep->pfnDeferredFillIn)(pExcep);
  }

  if (pExcep->bstrSource)
  {
    int iLen;
    auto iStrLen = static_cast<int>(SysStringLen(pExcep->bstrSource));
    iLen = WideCharToMultiByte(CP_ACP, 0, pExcep->bstrSource, iStrLen, nullptr, 0, nullptr, nullptr);
    *pszSource = static_cast<char *>(hb_xgrab((iLen + 1) * sizeof(char)));
    WideCharToMultiByte(CP_ACP, 0, pExcep->bstrSource, iStrLen, *pszSource, iLen + 1, nullptr, nullptr);
    (*pszSource)[iLen] = '\0';
    SysFreeString(pExcep->bstrSource);
  }

  if (pExcep->bstrHelpFile)
  {
    SysFreeString(pExcep->bstrHelpFile);
  }

  if (pExcep->bstrDescription)
  {
    int iLen;
    auto iStrLen = static_cast<int>(SysStringLen(pExcep->bstrDescription));
    iLen = WideCharToMultiByte(CP_ACP, 0, pExcep->bstrDescription, iStrLen, nullptr, 0, nullptr, nullptr);
    *pszDescription = static_cast<char *>(hb_xgrab((iLen + 14 + 1) * sizeof(char)));
    WideCharToMultiByte(CP_ACP, 0, pExcep->bstrDescription, iStrLen, *pszDescription, iLen + 1, nullptr, nullptr);
    (*pszDescription)[iLen] = '\0';
    SysFreeString(pExcep->bstrDescription);
  }
  else
  {
    *pszDescription = static_cast<char *>(hb_xgrab((14 + 1) * sizeof(char)));
    (*pszDescription)[0] = '\0';
  }

  if (pExcep->wCode)
  {
    hb_snprintf((*pszDescription) + strlen(*pszDescription), 14, " (%d)", pExcep->wCode);
  }
  else
  {
    hb_snprintf((*pszDescription) + strlen(*pszDescription), 14, " (0x%08lX)", pExcep->scode);
  }
}

IDispatch *hb_oleParam(int iParam)
{
  auto pOle = static_cast<HB_OLE *>(hb_parptrGC(&s_gcOleFuncs, iParam));

  if (pOle && pOle->pDisp)
  {
    return pOle->pDisp;
  }

  hb_errRT_OLE(EG_ARG, 1001, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
  return nullptr;
}

IDispatch *hb_oleItemGet(PHB_ITEM pItem)
{
  auto pOle = static_cast<HB_OLE *>(hb_itemGetPtrGC(pItem, &s_gcOleFuncs));

  return pOle ? pOle->pDisp : nullptr;
}

PHB_ITEM hb_oleItemPut(PHB_ITEM pItem, IDispatch *pDisp)
{
  auto pOle = static_cast<HB_OLE *>(hb_gcAllocate(sizeof(HB_OLE), &s_gcOleFuncs));

  pOle->pDisp = pDisp;
  pOle->pCallBack = nullptr;
  pOle->pDestructorFunc = nullptr;
  pOle->cargo = nullptr;
  return hb_itemPutPtrGC(pItem, pOle);
}

PHB_ITEM hb_oleItemGetCallBack(PHB_ITEM pItem)
{
  auto pOle = static_cast<HB_OLE *>(hb_itemGetPtrGC(pItem, &s_gcOleFuncs));

  return pOle && pOle->pCallBack ? *pOle->pCallBack : nullptr;
}

void hb_oleItemSetCallBack(PHB_ITEM pItem, PHB_ITEM *pCallBack)
{
  auto pOle = static_cast<HB_OLE *>(hb_itemGetPtrGC(pItem, &s_gcOleFuncs));

  if (pOle)
  {
    if (pOle->pCallBack && *pOle->pCallBack)
    {
      PHB_ITEM pCallBackPrev = *pOle->pCallBack;
      *pOle->pCallBack = nullptr;
      pOle->pCallBack = nullptr;
      hb_itemRelease(pCallBackPrev);
    }
    if (pCallBack)
    {
      pOle->pCallBack = pCallBack;
      hb_gcUnlock(*pCallBack);
    }
  }
}

void hb_oleItemSetDestructor(PHB_ITEM pItem, HB_OLE_DESTRUCTOR_FUNC pFunc, void *cargo)
{
  auto pOle = static_cast<HB_OLE *>(hb_itemGetPtrGC(pItem, &s_gcOleFuncs));

  if (pOle)
  {
    pOle->pDestructorFunc = pFunc;
    pOle->cargo = cargo;
  }
}

static IEnumVARIANT *hb_oleenumParam(int iParam)
{
  auto ppEnum = static_cast<IEnumVARIANT **>(hb_parptrGC(&s_gcOleenumFuncs, iParam));

  if (ppEnum && *ppEnum)
  {
    return *ppEnum;
  }

  hb_errRT_OLE(EG_ARG, 1002, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
  return nullptr;
}

static VARIANT *hb_oleVariantParam(int iParam)
{
  auto pVariant = static_cast<VARIANT *>(hb_parptrGC(&s_gcVariantFuncs, iParam));

  if (!pVariant)
  {
    hb_errRT_OLE(EG_ARG, 1017, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
  }

  return pVariant;
}

VARIANT *hb_oleItemGetVariant(PHB_ITEM pItem)
{
  return static_cast<VARIANT *>(hb_itemGetPtrGC(pItem, &s_gcVariantFuncs));
}

PHB_ITEM hb_oleItemPutVariant(PHB_ITEM pItem, VARIANT *pVariant, HB_BOOL fMove)
{
  auto pDestVariant = static_cast<VARIANT *>(hb_gcAllocate(sizeof(VARIANT), &s_gcVariantFuncs));

  if (fMove)
  {
    *pDestVariant = *pVariant;
    VariantInit(pVariant);
  }
  else
  {
    VariantInit(pDestVariant);
    VariantCopy(pDestVariant, pVariant);
  }

  return hb_itemPutPtrGC(pItem, pDestVariant);
}

/* Unicode string management */

static wchar_t *AnsiToWide(const char *szString)
{
  int iLen = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, szString, -1, nullptr, 0);
  auto szWide = static_cast<wchar_t *>(hb_xgrab(iLen * sizeof(wchar_t)));
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, szString, -1, szWide, iLen);
  return szWide;
}

static void AnsiToWideBuffer(const char *szString, wchar_t *szWide, int iLen)
{
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, szString, -1, szWide, iLen);
  szWide[iLen - 1] = L'\0';
}

static BSTR hb_oleItemToString(PHB_ITEM pItem)
{
  auto uiStrLen = static_cast<UINT>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_NATIVE, nullptr, UINT_MAX));
  BSTR strVal = SysAllocStringLen(nullptr, uiStrLen);

  hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_NATIVE, strVal, uiStrLen + 1);

  return strVal;
}

static void hb_oleStringToItem(BSTR strVal, PHB_ITEM pItem)
{
  hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_NATIVE, strVal, SysStringLen(strVal));
}

static void hb_oleVariantRef(VARIANT *pVariant, VARIANT *pVarRef)
{
  if (V_VT(pVariant) & VT_BYREF)
  {
    VariantCopy(pVarRef, pVariant);
    return;
  }

  V_VT(pVarRef) = V_VT(pVariant) | VT_BYREF;

  switch (V_VT(pVariant))
  {
  case VT_I1:
    V_I1REF(pVarRef) = &V_I1(pVariant);
    break;
  case VT_UI1:
    V_UI1REF(pVarRef) = &V_UI1(pVariant);
    break;
  case VT_I2:
    V_I2REF(pVarRef) = &V_I2(pVariant);
    break;
  case VT_UI2:
    V_UI2REF(pVarRef) = &V_UI2(pVariant);
    break;
  case VT_I4:
    V_I4REF(pVarRef) = &V_I4(pVariant);
    break;
  case VT_UI4:
    V_UI4REF(pVarRef) = &V_UI4(pVariant);
    break;
  case VT_I8:
#if defined(HB_OLE_NO_LLREF) || defined(HB_OLE_NO_LL)
    /* workaround for wrong OLE variant structure definition */
    V_R8REF(pVarRef) = &V_R8(pVariant);
#else
    V_I8REF(pVarRef) = &V_I8(pVariant);
#endif
    break;
  case VT_UI8:
#if defined(HB_OLE_NO_LLREF) || defined(HB_OLE_NO_LL)
    /* workaround for wrong OLE variant structure definition */
    V_R8REF(pVarRef) = &V_R8(pVariant);
#else
    V_I8REF(pVarRef) = &V_I8(pVariant);
#endif
    break;
  case VT_INT:
    V_INTREF(pVarRef) = &V_INT(pVariant);
    break;
  case VT_UINT:
    V_UINTREF(pVarRef) = &V_UINT(pVariant);
    break;
  case VT_ERROR:
    V_ERRORREF(pVarRef) = &V_ERROR(pVariant);
    break;
  case VT_BOOL:
    V_BOOLREF(pVarRef) = &V_BOOL(pVariant);
    break;
  case VT_R4:
    V_R4REF(pVarRef) = &V_R4(pVariant);
    break;
  case VT_R8:
    V_R8REF(pVarRef) = &V_R8(pVariant);
    break;
  case VT_CY:
    V_CYREF(pVarRef) = &V_CY(pVariant);
    break;
  case VT_DATE:
    V_R8REF(pVarRef) = &V_R8(pVariant);
    break;
  case VT_BSTR:
    V_BSTRREF(pVarRef) = &V_BSTR(pVariant);
    break;
  default:
    if (V_VT(pVariant) & VT_ARRAY)
    {
      V_ARRAYREF(pVarRef) = &V_ARRAY(pVariant);
    }
    else
    {
      V_VT(pVarRef) = VT_VARIANT | VT_BYREF;
      V_BYREF(pVarRef) = pVariant;
    }
    break;
  }
}

static HB_BOOL hb_oleSafeArrayFill(SAFEARRAY *pSafeArray, VARTYPE vt, PHB_ITEM pItem, int iDims, int iDim, long *plSize,
                                   long *plIndex)
{
  const char *pStr;
  void *pData;
  UINT uiPos;

  if (pItem == nullptr || pItem->isNil())
  {
    return true;
  }

  if (HB_IS_STRING(pItem))
  {
    pStr = hb_itemGetCPtr(pItem);
    uiPos = static_cast<UINT>(hb_itemGetCLen(pItem));
  }
  else if (HB_IS_ARRAY(pItem))
  {
    pStr = nullptr;
    uiPos = static_cast<UINT>(hb_arrayLen(pItem));
  }
  else
  {
    return false;
  }

  auto cElements = static_cast<UINT>(plSize[iDim - 1]);
  if (uiPos < cElements)
  {
    cElements = uiPos;
  }

  if (iDim < iDims)
  {
    if (pStr)
    {
      return false;
    }

    for (uiPos = 1; uiPos <= cElements; ++uiPos)
    {
      plIndex[iDim - 1] = static_cast<long>(uiPos - 1);
      if (!hb_oleSafeArrayFill(pSafeArray, vt, hb_arrayGetItemPtr(pItem, uiPos), iDims, iDim + 1, plSize, plIndex))
      {
        return false;
      }
    }
    return true;
  }

  if (pStr && (vt == VT_I1 || vt == VT_UI1) && SafeArrayGetDim(pSafeArray) == 1 &&
      SafeArrayAccessData(pSafeArray, &pData) == S_OK)
  {
    memcpy(pData, pStr, cElements);
    SafeArrayUnaccessData(pSafeArray);
    return true;
  }

  for (uiPos = 1; uiPos <= cElements; ++uiPos)
  {
    VARIANT v;
    void *ptr = nullptr;

    switch (vt)
    {
    case VT_I1:
      V_I1(&v) = pStr ? static_cast<HB_I8>(static_cast<signed char>(pStr[uiPos - 1]))
                      : static_cast<HB_I8>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_I1(&v);
      break;
    case VT_UI1:
      V_UI1(&v) = pStr ? static_cast<HB_U8>(static_cast<unsigned char>(pStr[uiPos - 1]))
                       : static_cast<HB_U8>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_UI1(&v);
      break;
    case VT_I2:
      V_I2(&v) = pStr ? static_cast<HB_I16>(static_cast<unsigned char>(pStr[uiPos - 1]))
                      : static_cast<HB_I16>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_I2(&v);
      break;
    case VT_UI2:
      V_UI2(&v) = pStr ? static_cast<HB_U16>(static_cast<unsigned char>(pStr[uiPos - 1]))
                       : static_cast<HB_U16>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_UI2(&v);
      break;
    case VT_I4:
      V_I4(&v) = pStr ? static_cast<HB_I32>(static_cast<unsigned char>(pStr[uiPos - 1]))
                      : static_cast<HB_I32>(hb_arrayGetNL(pItem, uiPos));
      ptr = &V_I4(&v);
      break;
    case VT_UI4:
      V_UI4(&v) = pStr ? static_cast<HB_U32>(static_cast<unsigned char>(pStr[uiPos - 1]))
                       : static_cast<HB_U32>(hb_arrayGetNL(pItem, uiPos));
      ptr = &V_UI4(&v);
      break;
#if !defined(HB_LONG_LONG_OFF)
    case VT_I8:
#if defined(HB_OLE_NO_LL)
      /* workaround for wrong OLE variant structure definition */
      ptr = &V_I4(&v);
      *(static_cast<HB_LONGLONG *>(ptr)) = pStr ? static_cast<HB_I64>(static_cast<unsigned char>(pStr[uiPos - 1]))
                                                : static_cast<HB_I64>(hb_arrayGetNInt(pItem, uiPos));
#else
      V_I8(&v) = pStr ? static_cast<HB_I64>(static_cast<unsigned char>(pStr[uiPos - 1]))
                      : static_cast<HB_I64>(hb_arrayGetNInt(pItem, uiPos));
      ptr = &V_I8(&v);
#endif
      break;
    case VT_UI8:
#if defined(HB_OLE_NO_LL)
      /* workaround for wrong OLE variant structure definition */
      ptr = &V_UI4(&v);
      *(static_cast<HB_ULONGLONG *>(ptr)) = pStr ? static_cast<HB_U64>(static_cast<unsigned char>(pStr[uiPos - 1]))
                                                 : static_cast<HB_U64>(hb_arrayGetNInt(pItem, uiPos));
#else
      V_UI8(&v) = pStr ? static_cast<HB_U64>(static_cast<unsigned char>(pStr[uiPos - 1]))
                       : static_cast<HB_U64>(hb_arrayGetNInt(pItem, uiPos));
      ptr = &V_UI8(&v);
#endif
      break;
#endif
    case VT_INT:
      V_INT(&v) = pStr ? static_cast<HB_INT>(static_cast<unsigned char>(pStr[uiPos - 1]))
                       : static_cast<HB_INT>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_INT(&v);
      break;
    case VT_UINT:
      V_UINT(&v) = pStr ? static_cast<HB_UINT>(static_cast<unsigned char>(pStr[uiPos - 1]))
                        : static_cast<HB_UINT>(hb_arrayGetNI(pItem, uiPos));
      ptr = &V_UINT(&v);
      break;
    case VT_ERROR:
      if (pStr == nullptr)
      {
        V_ERROR(&v) = static_cast<SCODE>(hb_arrayGetNL(pItem, uiPos));
        ptr = &V_ERROR(&v);
      }
      break;
    case VT_BOOL:
      if (pStr == nullptr)
      {
        V_BOOL(&v) = hb_arrayGetL(pItem, uiPos) ? TRUE : FALSE;
        ptr = &V_BOOL(&v);
      }
      break;
    case VT_R4:
      if (pStr == nullptr)
      {
        V_R4(&v) = static_cast<float>(hb_arrayGetND(pItem, uiPos));
        ptr = &V_R4(&v);
      }
      break;
    case VT_R8:
      if (pStr == nullptr)
      {
        V_R8(&v) = hb_arrayGetND(pItem, uiPos);
        ptr = &V_R8(&v);
      }
      break;
    case VT_CY:
      if (pStr == nullptr)
      {
        VarCyFromR8(hb_arrayGetND(pItem, uiPos), &V_CY(&v));
        ptr = &V_CY(&v);
      }
      break;
    case VT_DATE:
      if (pStr == nullptr)
      {
        V_R8(&v) = hb_arrayGetTD(pItem, uiPos) - HB_OLE_DATE_BASE;
        ptr = &V_R8(&v);
      }
      break;
    case VT_BSTR:
      if (pStr == nullptr)
      {
        ptr = hb_oleItemToString(hb_arrayGetItemPtr(pItem, uiPos));
      }
      break;
    case VT_VARIANT:
      if (pStr == nullptr)
      {
        VariantInit(&v);
        hb_oleItemToVariant(&v, hb_arrayGetItemPtr(pItem, uiPos));
        ptr = &v;
      }
      break;
    }

    if (ptr != nullptr)
    {
      plIndex[iDim - 1] = static_cast<long>(uiPos - 1);
      SafeArrayPutElement(pSafeArray, plIndex, ptr);
      if (vt == VT_VARIANT)
      {
        VariantClear(&v);
      }
    }
    else
    {
      return false;
    }
  }
  return true;
}

static SAFEARRAY *hb_oleSafeArrayFromItem(PHB_ITEM pItem, VARTYPE vt, int iDims, long *plSize)
{
  SAFEARRAYBOUND boundbuf[16], *sabound;
  SAFEARRAY *pSafeArray;
  long *plIndex;

  if (iDims == 0)
  {
    if (pItem)
    {
      if (HB_IS_STRING(pItem))
      {
        plSize[0] = static_cast<UINT>(hb_itemGetCLen(pItem));
      }
      else if (HB_IS_ARRAY(pItem))
      {
        plSize[0] = static_cast<UINT>(hb_arrayLen(pItem));
      }
      else if (!pItem->isNil())
      {
        return nullptr;
      }
    }
    iDims = 1;
  }

  sabound = iDims > static_cast<int>(HB_SIZEOFARRAY(boundbuf))
                ? static_cast<SAFEARRAYBOUND *>(hb_xgrab(sizeof(SAFEARRAYBOUND) * iDims))
                : boundbuf;
  /* use the same buffer for dimensions and indexes */
  plIndex = &sabound[0].lLbound;

  if (iDims == 1 && plSize[0] > 0)
  {
    pSafeArray = SafeArrayCreateVector(vt, 0, plSize[0]);
  }
  else
  {
    for (auto iDim = 0; iDim < iDims; ++iDim)
    {
      sabound[iDim].lLbound = 0;
      sabound[iDim].cElements = plSize[iDim];
    }
    pSafeArray = SafeArrayCreate(vt, iDims, sabound);
  }

  if (pSafeArray && pItem && iDims && plSize[0])
  {
    if (!hb_oleSafeArrayFill(pSafeArray, vt, pItem, iDims, 1, plSize, plIndex))
    {
      SafeArrayDestroy(pSafeArray);
      pSafeArray = nullptr;
    }
  }

  if (sabound != boundbuf)
  {
    hb_xfree(sabound);
  }

  return pSafeArray;
}

static HB_BOOL hb_oleSafeArrayToString(PHB_ITEM pItem, SAFEARRAY *pSafeArray)
{
  long lFrom, lTo;
  VARTYPE vt = VT_UI1;

  if (SafeArrayGetElemsize(pSafeArray) == 1 &&
#if !defined(HB_OLE_NO_SAFEARRAYGETVARTYPE)
      SafeArrayGetVartype(pSafeArray, &vt) == S_OK &&
#endif
      (vt == VT_I1 || vt == VT_UI1) && SafeArrayGetLBound(pSafeArray, 1, &lFrom) == S_OK &&
      SafeArrayGetUBound(pSafeArray, 1, &lTo) == S_OK && lFrom <= lTo + 1) /* accept empty arrays */
  {
    void *pData;
    if (SafeArrayAccessData(pSafeArray, &pData) == S_OK)
    {
      hb_itemPutCL(pItem, static_cast<const char *>(pData), lTo - lFrom + 1);
      SafeArrayUnaccessData(pSafeArray);
      return true;
    }
  }
  return false;
}

IDispatch *hb_oleItemGetDispatch(PHB_ITEM pItem)
{
  if (HB_IS_OBJECT(pItem))
  {
    if (hb_objHasMessage(pItem, s_pDyns_hObjAccess))
    {
      hb_vmPushDynSym(s_pDyns_hObjAccess);
      hb_vmPush(pItem);
      hb_vmSend(0);

      return hb_oleParam(-1);
    }
  }
  return hb_oleItemGet(pItem);
}

static void hb_oleDispatchToVariant(VARIANT *pVariant, IDispatch *pDisp, VARIANT *pVarRef)
{
  /* pVariant will be freed using VariantClear().
     We increment reference count to keep OLE object alive */
  HB_VTBL(pDisp)->AddRef(HB_THIS(pDisp));
  V_VT(pVariant) = VT_DISPATCH;
  V_DISPATCH(pVariant) = pDisp;
  if (pVarRef)
  {
    V_VT(pVarRef) = VT_DISPATCH | VT_BYREF;
    V_DISPATCHREF(pVarRef) = &V_DISPATCH(pVariant);
  }
}

/* Item <-> Variant conversion */

static void hb_oleItemToVariantRef(VARIANT *pVariant, PHB_ITEM pItem, VARIANT *pVarRef, HB_OLEOBJ_FUNC pObjFunc)
{
  VariantClear(pVariant); /* VT_T(pVariant) = VT_EMPTY; */

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    V_VT(pVariant) = VT_BSTR;
    V_BSTR(pVariant) = hb_oleItemToString(pItem);
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_BSTR | VT_BYREF;
      V_BSTRREF(pVarRef) = &V_BSTR(pVariant);
    }
    break;

  case Harbour::Item::LOGICAL:
    V_VT(pVariant) = VT_BOOL;
    V_BOOL(pVariant) = hb_itemGetL(pItem) ? VARIANT_TRUE : VARIANT_FALSE;
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_BOOL | VT_BYREF;
      V_BOOLREF(pVarRef) = &V_BOOL(pVariant);
    }
    break;

  case Harbour::Item::INTEGER:
    V_VT(pVariant) = VT_I4;
    V_I4(pVariant) = hb_itemGetNL(pItem);
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_I4 | VT_BYREF;
      V_I4REF(pVarRef) = &V_I4(pVariant);
    }
    break;

  case Harbour::Item::LONG:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    V_VT(pVariant) = VT_I4;
    V_I4(pVariant) = hb_itemGetNL(pItem);
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_I4 | VT_BYREF;
      V_I4REF(pVarRef) = &V_I4(pVariant);
    }
#else
    V_VT(pVariant) = VT_I8;
#if defined(HB_OLE_NO_LL)
    /* workaround for wrong OLE variant structure definition */
    *(static_cast<HB_LONGLONG *>(&V_I4(pVariant))) = hb_itemGetNInt(pItem);
#else
    V_I8(pVariant) = hb_itemGetNInt(pItem);
#endif
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_I8 | VT_BYREF;
#if defined(HB_OLE_NO_LLREF) || defined(HB_OLE_NO_LL)
      /* workaround for wrong OLE variant structure definition */
      V_R8REF(pVarRef) = &V_R8(pVariant);
#else
      V_I8REF(pVarRef) = &V_I8(pVariant);
#endif
    }
#endif
    break;

  case Harbour::Item::DOUBLE:
    V_VT(pVariant) = VT_R8;
    V_R8(pVariant) = hb_itemGetND(pItem);
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_R8 | VT_BYREF;
      V_R8REF(pVarRef) = &V_R8(pVariant);
    }
    break;

  case Harbour::Item::DATE: {
    long lDate = hb_itemGetDL(pItem);

    if (lDate == 0 && hb_oleGetNullDateFlag())
    {
      V_VT(pVariant) = VT_NULL;
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_VARIANT | VT_BYREF;
        V_VARIANTREF(pVarRef) = pVariant;
      }
    }
    else
    {
      V_VT(pVariant) = VT_DATE;
      V_R8(pVariant) = static_cast<double>(lDate - HB_OLE_DATE_BASE);
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_DATE | VT_BYREF;
        V_R8REF(pVarRef) = &V_R8(pVariant);
      }
    }
    break;
  }
  case Harbour::Item::TIMESTAMP: {
    double dDate = hb_itemGetTD(pItem);

    if (dDate == 0 && hb_oleGetNullDateFlag())
    {
      V_VT(pVariant) = VT_NULL;
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_VARIANT | VT_BYREF;
        V_VARIANTREF(pVarRef) = pVariant;
      }
    }
    else
    {
      V_VT(pVariant) = VT_DATE;
      V_R8(pVariant) = dDate - HB_OLE_DATE_BASE;
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_DATE | VT_BYREF;
        V_R8REF(pVarRef) = &V_R8(pVariant);
      }
    }
    break;
  }
  case Harbour::Item::POINTER: {
    IDispatch *pDisp;
    VARIANT *pVarPtr;

    if ((pDisp = hb_oleItemGet(pItem)) != nullptr)
    {
      hb_oleDispatchToVariant(pVariant, pDisp, pVarRef);
    }
    else if ((pVarPtr = hb_oleItemGetVariant(pItem)) != nullptr)
    {
      if (pVarRef)
      {
        hb_oleVariantRef(pVarPtr, pVarRef);
      }
      else
      {
        VariantCopy(pVariant, pVarPtr);
      }
    }
#ifdef HB_OLE_PASS_POINTERS
    else
    {
      V_VT(pVariant) = VT_PTR;
      V_BYREF(pVariant) = hb_itemGetPtr(pItem);
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_PTR | VT_BYREF;
        V_BYREF(pVarRef) = &V_BYREF(pVariant);
      }
    }
#endif
    break;
  }
  case Harbour::Item::ARRAY: /* or OBJECT */
    if (HB_IS_OBJECT(pItem))
    {
      IDispatch *pDisp = hb_oleItemGetDispatch(pItem);

      if (pDisp)
      {
        hb_oleDispatchToVariant(pVariant, pDisp, pVarRef);
      }
      else if (pObjFunc)
      {
        pObjFunc(pVariant, pItem);
      }
    }
    else
    {
      SAFEARRAY *pSafeArray;
      SAFEARRAYBOUND sabound[1];
      HB_SIZE nLen;

      nLen = hb_arrayLen(pItem);

      sabound[0].lLbound = 0;
      sabound[0].cElements = static_cast<long>(nLen);

      pSafeArray = SafeArrayCreate(VT_VARIANT, 1, sabound);
      V_VT(pVariant) = VT_VARIANT | VT_ARRAY;
      V_ARRAY(pVariant) = pSafeArray;
      if (pVarRef)
      {
        V_VT(pVarRef) = VT_VARIANT | VT_ARRAY | VT_BYREF;
        V_ARRAYREF(pVarRef) = &V_ARRAY(pVariant);
      }

      for (HB_SIZE n = 0; n < nLen; n++)
      {
        VARIANT vItem;
        long lIndex[1];

        VariantInit(&vItem);
        hb_oleItemToVariantRef(&vItem, hb_arrayGetItemPtr(pItem, n + 1), nullptr, pObjFunc);
        lIndex[0] = static_cast<long>(n);
        SafeArrayPutElement(pSafeArray, lIndex, &vItem);
        VariantClear(&vItem);
      }
    }
    break;

  case Harbour::Item::NIL:
    if (hb_oleGetNil2NullFlag())
    {
      V_VT(pVariant) = VT_NULL;
    }
    /* fallthrough */

  default:
    if (pVarRef)
    {
      V_VT(pVarRef) = VT_VARIANT | VT_BYREF;
      V_VARIANTREF(pVarRef) = pVariant;
    }
  }

/* enabling this code may allow to exchange parameters by reference
 * without strong typing restrictions but I do not know if such method
 * is honored by other OLE code
 */
#if 0
   if( pVarRef )
   {
      V_VT(pVarRef) = VT_VARIANT | VT_BYREF;
      V_VARIANTREF(pVarRef) = pVariant;
   }
#endif
}

void hb_oleItemToVariant(VARIANT *pVariant, PHB_ITEM pItem)
{
  hb_oleItemToVariantRef(pVariant, pItem, nullptr, nullptr);
}

void hb_oleItemToVariantEx(VARIANT *pVariant, PHB_ITEM pItem, HB_OLEOBJ_FUNC pObjFunc)
{
  hb_oleItemToVariantRef(pVariant, pItem, nullptr, pObjFunc);
}

static void hb_oleSafeArrayToItem(PHB_ITEM pItem, SAFEARRAY *pSafeArray, int iDims, int iDim, long *plIndex, VARTYPE vt,
                                  HB_USHORT uiClass)
{
  long lFrom, lTo;

  if (SafeArrayGetLBound(pSafeArray, iDim, &lFrom) == S_OK && SafeArrayGetUBound(pSafeArray, iDim, &lTo) == S_OK &&
      lFrom <= lTo)
  {
    HB_SIZE nIndex = 0;

    hb_arrayNew(pItem, lTo - lFrom + 1);
    if (iDim == iDims)
    {
      VARIANT vItem;
      VariantInit(&vItem);
      do
      {
        plIndex[iDim - 1] = lFrom;
        /* hack: for non VT_VARIANT arrays create VARIANT dynamically
         *       using pointer to union in variant structure which
         *       holds all variant values except VT_DECIMAL which is
         *       stored in different place.
         */
        if (SafeArrayGetElement(pSafeArray, plIndex,
                                vt == VT_VARIANT
                                    ? static_cast<void *>(&vItem)
                                    : (vt == VT_DECIMAL ? static_cast<void *>(&HB_WIN_U1(&vItem, decVal))
                                                        : static_cast<void *>(&HB_WIN_U3(&vItem, bVal)))) == S_OK)
        {
          if (vt != VT_VARIANT)
          {
            V_VT(&vItem) = vt; /* it's reserved in VT_DECIMAL structure */
          }
          hb_oleVariantToItemEx(hb_arrayGetItemPtr(pItem, ++nIndex), &vItem, uiClass);
          VariantClear(&vItem);
        }
      } while (++lFrom <= lTo);
    }
    else
    {
      do
      {
        plIndex[iDim - 1] = lFrom;
        hb_oleSafeArrayToItem(hb_arrayGetItemPtr(pItem, ++nIndex), pSafeArray, iDims, iDim + 1, plIndex, vt, uiClass);
      } while (++lFrom <= lTo);
    }
  }
  else
  {
    hb_arrayNew(pItem, 0);
  }
}

void hb_oleDispatchToItem(PHB_ITEM pItem, IDispatch *pdispVal, HB_USHORT uiClass)
{
  if (pdispVal)
  {
    if (hb_vmRequestReenter())
    {
      PHB_SYMB pClassFunc;
      PHB_ITEM pPtrGC;

      pClassFunc = hb_clsFuncSym(uiClass);
      if (!pClassFunc)
      {
        pClassFunc = hb_dynsymSymbol(s_pDyns_hb_oleauto);
      }

      hb_vmPushSymbol(pClassFunc);
      hb_vmPushNil();
      hb_vmDo(0);

      auto pObject = hb_itemNew(hb_stackReturnItem());

      pPtrGC = hb_oleItemPut(nullptr, pdispVal);
      /* Item is one more copy of the object */
      HB_VTBL(pdispVal)->AddRef(HB_THIS(pdispVal));

      hb_vmPushDynSym(s_pDyns_hObjAssign);
      hb_vmPush(pObject);
      hb_vmPush(pPtrGC);
      hb_vmSend(1);
      hb_itemRelease(pPtrGC);
      hb_vmRequestRestore();

      /* We should store object to pItem after hb_vmRequestRestore(),
       * because pItem actually can be stack's return item!
       */
      hb_itemMove(pItem, pObject);
      hb_itemRelease(pObject);
    }
  }
}

void hb_oleVariantToItemEx(PHB_ITEM pItem, VARIANT *pVariant, HB_USHORT uiClass)
{
  if (V_VT(pVariant) == (VT_VARIANT | VT_BYREF))
  {
    pVariant = V_VARIANTREF(pVariant);
  }

  switch (V_VT(pVariant))
  {
  case VT_UNKNOWN:
  case VT_UNKNOWN | VT_BYREF: {
    IDispatch *pdispVal = nullptr;
    IUnknown *punkVal = V_VT(pVariant) == VT_UNKNOWN ? V_UNKNOWN(pVariant) : *V_UNKNOWNREF(pVariant);
    hb_itemClear(pItem);
    if (punkVal && HB_VTBL(punkVal)->QueryInterface(HB_THIS_(punkVal) HB_ID_REF(IID_IDispatch),
                                                    static_cast<void **>(static_cast<void *>(&pdispVal))) == S_OK)
    {
      hb_oleDispatchToItem(pItem, pdispVal, uiClass);
      HB_VTBL(pdispVal)->Release(HB_THIS(pdispVal));
    }
    break;
  }

  case VT_DISPATCH:
  case VT_DISPATCH | VT_BYREF:
    hb_itemClear(pItem);
    hb_oleDispatchToItem(pItem, V_VT(pVariant) == VT_DISPATCH ? V_DISPATCH(pVariant) : *V_DISPATCHREF(pVariant),
                         uiClass);
    break;

  case VT_BSTR:
    hb_oleStringToItem(V_BSTR(pVariant), pItem);
    break;

  case VT_BSTR | VT_BYREF:
    hb_oleStringToItem(*V_BSTRREF(pVariant), pItem);
    break;

  case VT_BOOL:
    hb_itemPutL(pItem, V_BOOL(pVariant) ? true : false);
    break;

  case VT_BOOL | VT_BYREF:
    hb_itemPutL(pItem, *V_BOOLREF(pVariant) ? true : false);
    break;

  case VT_I1:
    hb_itemPutNI(pItem, static_cast<signed char>(V_I1(pVariant)));
    break;

  case VT_I1 | VT_BYREF:
    hb_itemPutNI(pItem, static_cast<signed char>(*V_I1REF(pVariant)));
    break;

  case VT_I2:
    hb_itemPutNI(pItem, static_cast<short>(V_I2(pVariant)));
    break;

  case VT_I2 | VT_BYREF:
    hb_itemPutNI(pItem, static_cast<short>(*V_I2REF(pVariant)));
    break;

  case VT_I4:
    hb_itemPutNL(pItem, V_I4(pVariant));
    break;

  case VT_I4 | VT_BYREF:
    hb_itemPutNL(pItem, *V_I4REF(pVariant));
    break;

  case VT_I8:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(V_I4(pVariant)));
#elif defined(HB_OLE_NO_LL)
    /* workaround for wrong OLE variant structure definition */
    hb_itemPutNInt(pItem, *(static_cast<HB_LONGLONG *>(&V_I4(pVariant))));
#else
    hb_itemPutNInt(pItem, V_I8(pVariant));
#endif
    break;

  case VT_I8 | VT_BYREF:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(*V_I4REF(pVariant)));
#elif defined(HB_OLE_NO_LLREF)
    /* workaround for wrong OLE variant structure definition */
    hb_itemPutNInt(pItem, *reinterpret_cast<HB_LONGLONG *>(V_R8REF(pVariant)));
#else
    hb_itemPutNInt(pItem, *V_I8REF(pVariant));
#endif
    break;

  case VT_UI1:
    hb_itemPutNI(pItem, static_cast<unsigned char>(V_UI1(pVariant)));
    break;

  case VT_UI1 | VT_BYREF:
    hb_itemPutNI(pItem, static_cast<unsigned char>(*V_UI1REF(pVariant)));
    break;

  case VT_UI2:
    hb_itemPutNI(pItem, static_cast<unsigned short>(V_UI2(pVariant)));
    break;

  case VT_UI2 | VT_BYREF:
    hb_itemPutNI(pItem, static_cast<unsigned short>(*V_UI2REF(pVariant)));
    break;

  case VT_UI4:
    hb_itemPutNInt(pItem, V_UI4(pVariant));
    break;

  case VT_UI4 | VT_BYREF:
    hb_itemPutNInt(pItem, *V_UI4REF(pVariant));
    break;

  case VT_UI8:
    /* TODO: sign is lost. Conversion to double will lose significant digits. */
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(V_UI4(pVariant)));
#elif defined(HB_OLE_NO_LL)
    /* workaround for wrong OLE variant structure definition */
    hb_itemPutNInt(pItem, *(static_cast<HB_LONGLONG *>(&V_UI4(pVariant))));
#else
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(V_UI8(pVariant)));
#endif
    break;

  case VT_UI8 | VT_BYREF:
    /* TODO: sign is lost. Conversion to double will lose significant digits. */
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(*V_UI4REF(pVariant)));
#elif defined(HB_OLE_NO_LLREF)
    /* workaround for wrong OLE variant structure definition */
    hb_itemPutNInt(pItem, *reinterpret_cast<HB_LONGLONG *>(V_R8REF(pVariant)));
#else
    hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(*V_UI8REF(pVariant)));
#endif
    break;

  case VT_INT:
    hb_itemPutNI(pItem, V_INT(pVariant));
    break;

  case VT_INT | VT_BYREF:
    hb_itemPutNI(pItem, *V_INTREF(pVariant));
    break;

  case VT_UINT:
    hb_itemPutNInt(pItem, V_UINT(pVariant));
    break;

  case VT_UINT | VT_BYREF:
    hb_itemPutNInt(pItem, *V_UINTREF(pVariant));
    break;

  case VT_ERROR:
    hb_itemPutNInt(pItem, V_ERROR(pVariant));
    break;

  case VT_ERROR | VT_BYREF:
    hb_itemPutNInt(pItem, *V_ERRORREF(pVariant));
    break;

  case VT_R4:
    hb_itemPutND(pItem, static_cast<double>(V_R4(pVariant)));
    break;

  case VT_R4 | VT_BYREF:
    hb_itemPutND(pItem, static_cast<double>(*V_R4REF(pVariant)));
    break;

  case VT_R8:
    hb_itemPutND(pItem, V_R8(pVariant));
    break;

  case VT_R8 | VT_BYREF:
    hb_itemPutND(pItem, *V_R8REF(pVariant));
    break;

  case VT_CY:
  case VT_CY | VT_BYREF: {
    double dblVal;
    if (VarR8FromCy(V_VT(pVariant) == VT_CY ? V_CY(pVariant) : *V_CYREF(pVariant), &dblVal) != S_OK)
    {
      dblVal = 0;
    }
    hb_itemPutND(pItem, dblVal);
#if 0
         hb_itemPutNDLen(pItem, dblVal, 0, 4);
#endif
    break;
  }

  case VT_DECIMAL:
  case VT_DECIMAL | VT_BYREF: {
    double dblVal;
    if (VarR8FromDec(V_VT(pVariant) == VT_DECIMAL ? &HB_WIN_U1(pVariant, decVal) : V_DECIMALREF(pVariant), &dblVal) !=
        S_OK)
    {
      dblVal = 0;
    }
    hb_itemPutND(pItem, dblVal);
    break;
  }

  case VT_DATE:
  case VT_DATE | VT_BYREF: {
    long lJulian, lMilliSec;
    double dblVal = V_VT(pVariant) == VT_DATE ? V_R8(pVariant) : *V_R8REF(pVariant);

    hb_timeStampUnpackDT(dblVal + HB_OLE_DATE_BASE, &lJulian, &lMilliSec);
    if (lMilliSec)
    {
      hb_itemPutTDT(pItem, lJulian, lMilliSec);
    }
    else
    {
      hb_itemPutDL(pItem, lJulian);
    }
    break;
  }

#ifdef HB_OLE_PASS_POINTERS
  case VT_PTR:
  case VT_PTR | VT_BYREF:
  case VT_BYREF:
    hb_itemPutPtr(pItem, V_BYREF(pVariant));
    break;
#endif

  case VT_EMPTY:
  case VT_EMPTY | VT_BYREF:
  case VT_NULL:
  case VT_NULL | VT_BYREF:
    hb_itemClear(pItem);
    break;

  default:
    if (V_VT(pVariant) & VT_ARRAY)
    {
      SAFEARRAY *pSafeArray = (V_VT(pVariant) & VT_BYREF) ? *V_ARRAYREF(pVariant) : V_ARRAY(pVariant);
      if (pSafeArray)
      {
        auto iDims = static_cast<int>(SafeArrayGetDim(pSafeArray));

        if (iDims >= 1)
        {
          if (iDims > 1 || !hb_oleSafeArrayToString(pItem, pSafeArray))
          {
            auto plIndex = static_cast<long *>(hb_xgrab(iDims * sizeof(long)));
            hb_oleSafeArrayToItem(pItem, pSafeArray, iDims, 1, plIndex,
                                  static_cast<VARTYPE>(V_VT(pVariant) & ~(VT_ARRAY | VT_BYREF)), uiClass);
            hb_xfree(plIndex);
          }
        }
        else
        {
          hb_arrayNew(pItem, 0);
        }
        break;
      }
    }
    /* possible RT error - unsupported variant */
    hb_itemClear(pItem);
  }
}

void hb_oleVariantToItem(PHB_ITEM pItem, VARIANT *pVariant)
{
  hb_oleVariantToItemEx(pItem, pVariant, 0);
}

void hb_oleVariantUpdate(VARIANT *pVariant, PHB_ITEM pItem, HB_OLEOBJ_FUNC pObjFunc)
{
  switch (V_VT(pVariant))
  {
  case VT_DISPATCH | VT_BYREF: {
    IDispatch *pDisp = hb_oleItemGetDispatch(pItem);

    if (pDisp)
    {
      IDispatch *pdispVal = *V_DISPATCHREF(pVariant);
      if (pdispVal != pDisp)
      {
        HB_VTBL(pDisp)->AddRef(HB_THIS(pDisp));
        *V_DISPATCHREF(pVariant) = pDisp;
        if (pdispVal)
        {
          HB_VTBL(pdispVal)->Release(HB_THIS(pdispVal));
        }
      }
    }
    else if (pObjFunc && HB_IS_OBJECT(pItem))
    {
      VARIANTARG variant;
      VariantInit(&variant);
      if (pObjFunc(&variant, pItem))
      {
        IDispatch *pdispVal = *V_DISPATCHREF(pVariant);
        *V_DISPATCHREF(pVariant) = V_DISPATCH(&variant);
        if (pdispVal)
        {
          HB_VTBL(pdispVal)->Release(HB_THIS(pdispVal));
        }
      }
    }
    break;
  }

  case VT_UNKNOWN | VT_BYREF: {
    IDispatch *pDisp = hb_oleItemGetDispatch(pItem);

    if (pDisp)
    {
      IUnknown *pUnk = nullptr;

      if (HB_VTBL(pDisp)->QueryInterface(HB_THIS_(pDisp) HB_ID_REF(IID_IEnumVARIANT),
                                         static_cast<void **>(static_cast<void *>(&pUnk))) == S_OK)
      {
        IUnknown *punkVal = *V_UNKNOWNREF(pVariant);
        if (punkVal)
        {
          HB_VTBL(punkVal)->Release(HB_THIS(punkVal));
        }
        *V_UNKNOWNREF(pVariant) = pUnk;
      }
    }
    else if (pObjFunc && HB_IS_OBJECT(pItem))
    {
      VARIANTARG variant;
      VariantInit(&variant);
      if (pObjFunc(&variant, pItem))
      {
        IDispatch *pdispVal = *V_DISPATCHREF(pVariant);
        *V_DISPATCHREF(pVariant) = V_DISPATCH(&variant);
        if (pdispVal)
        {
          HB_VTBL(pdispVal)->Release(HB_THIS(pdispVal));
        }
      }
    }
    break;
  }

  case VT_BSTR | VT_BYREF:
    SysFreeString(*V_BSTRREF(pVariant));
    *V_BSTRREF(pVariant) = hb_oleItemToString(pItem);
    break;

  case VT_BOOL | VT_BYREF:
    *V_BOOLREF(pVariant) = static_cast<VARIANT_BOOL>(hb_itemGetL(pItem));
    break;

  case VT_I1 | VT_BYREF:
    *V_I1REF(pVariant) = static_cast<signed char>(hb_itemGetNI(pItem));
    break;

  case VT_I2 | VT_BYREF:
    *V_I2REF(pVariant) = static_cast<short>(hb_itemGetNI(pItem));
    break;

  case VT_I4 | VT_BYREF:
    *V_I4REF(pVariant) = hb_itemGetNL(pItem);
    break;

  case VT_I8 | VT_BYREF:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    *V_I4REF(pVariant) = static_cast<long>(hb_itemGetNInt(pItem));
#elif defined(HB_OLE_NO_LLREF)
    /* workaround for wrong OLE variant structure definition */
    *reinterpret_cast<HB_LONGLONG *>(V_R8REF(pVariant)) = static_cast<HB_LONGLONG>(hb_itemGetNInt(pItem));
#else
    *V_I8REF(pVariant) = static_cast<HB_LONGLONG>(hb_itemGetNInt(pItem));
#endif
    break;

  case VT_UI1 | VT_BYREF:
    *V_UI1REF(pVariant) = static_cast<unsigned char>(hb_itemGetNI(pItem));
    break;

  case VT_UI2 | VT_BYREF:
    *V_UI2REF(pVariant) = static_cast<unsigned short>(hb_itemGetNI(pItem));
    break;

  case VT_UI4 | VT_BYREF:
    *V_UI4REF(pVariant) = static_cast<unsigned long>(hb_itemGetNL(pItem));
    break;

  case VT_UI8 | VT_BYREF:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
    *V_UI4REF(pVariant) = static_cast<unsigned long>(hb_itemGetNInt(pItem));
#elif defined(HB_OLE_NO_LLREF)
    /* workaround for wrong OLE variant structure definition */
    *reinterpret_cast<HB_ULONGLONG *>(V_R8REF(pVariant)) = static_cast<HB_ULONGLONG>(hb_itemGetNInt(pItem));
#else
    *V_UI8REF(pVariant) = static_cast<HB_ULONGLONG>(hb_itemGetNInt(pItem));
#endif
    break;

  case VT_INT | VT_BYREF:
    *V_INTREF(pVariant) = hb_itemGetNI(pItem);
    break;

  case VT_UINT | VT_BYREF:
    *V_UINTREF(pVariant) = static_cast<unsigned int>(hb_itemGetNI(pItem));
    break;

  case VT_ERROR | VT_BYREF:
    *V_ERRORREF(pVariant) = static_cast<SCODE>(hb_itemGetNL(pItem));
    break;

  case VT_R4 | VT_BYREF:
    *V_R4REF(pVariant) = static_cast<float>(hb_itemGetND(pItem));
    break;

  case VT_R8 | VT_BYREF:
    *V_R8REF(pVariant) = hb_itemGetND(pItem);
    break;

  case VT_CY | VT_BYREF:
    VarCyFromR8(hb_itemGetND(pItem), V_CYREF(pVariant));
    break;

  case VT_DECIMAL | VT_BYREF:
    VarDecFromR8(hb_itemGetND(pItem), V_DECIMALREF(pVariant));
    break;

  case VT_DATE | VT_BYREF:
    *V_R8REF(pVariant) = hb_itemGetTD(pItem) - HB_OLE_DATE_BASE;
    break;

#ifdef HB_OLE_PASS_POINTERS
  case VT_PTR | VT_BYREF:
    V_BYREF(pVariant) = hb_itemGetPtr(pItem);
    break;
#endif

  case VT_BYREF | VT_VARIANT:
    hb_oleItemToVariantRef(V_VARIANTREF(pVariant), pItem, nullptr, pObjFunc);
    break;

  case VT_VARIANT | VT_ARRAY | VT_BYREF:
    /* TODO: */
    break;
  }
}

/* Invoke IDispatch method */

typedef struct
{
  PHB_ITEM item;
  VARIANT *variant;
} HB_OLE_PARAM_REF;

HB_BOOL hb_oleDispInvoke(PHB_SYMB pSym, PHB_ITEM pObject, PHB_ITEM pParam, DISPPARAMS *pParams, VARIANT *pVarResult,
                         HB_OLEOBJ_FUNC pObjFunc, HB_USHORT uiClass)
{
  if (!pSym && HB_IS_SYMBOL(pObject))
  {
    pSym = hb_itemGetSymbol(pObject);
    pObject = nullptr;
  }

  if ((pSym || pObject) && hb_vmRequestReenter())
  {
    HB_OLE_PARAM_REF refArray[32];
    int i, ii, iParams, iCount, iRefs;

    iParams = iCount = pParams->cArgs;

    for (i = iRefs = 0; i < iCount && iRefs < static_cast<int>(HB_SIZEOFARRAY(refArray)); i++)
    {
      if (V_VT(&pParams->rgvarg[i]) & VT_BYREF)
      {
        refArray[iRefs++].item = hb_stackAllocItem();
      }
    }

    if (pSym)
    {
      hb_vmPushSymbol(pSym);
    }
    else
    {
      hb_vmPushEvalSym();
    }
    if (pObject)
    {
      hb_vmPush(pObject);
    }
    else
    {
      hb_vmPushNil();
    }

    if (pParam)
    {
      hb_vmPush(pParam);
      iParams++;
    }

    for (i = 1, ii = 0; i <= iCount; i++)
    {
      if ((V_VT(&pParams->rgvarg[iCount - i]) & VT_BYREF) && (ii < iRefs))
      {
        refArray[ii].variant = &pParams->rgvarg[iCount - i];
        hb_oleVariantToItemEx(refArray[ii].item, refArray[ii].variant, uiClass);
        hb_vmPushItemRef(refArray[ii++].item);
      }
      else
      {
        hb_oleVariantToItemEx(hb_stackAllocItem(), &pParams->rgvarg[iCount - i], uiClass);
      }
    }

    if (pObject && !HB_IS_HASH(pObject))
    {
      hb_vmSend(static_cast<HB_USHORT>(iParams));
    }
    else
    {
      hb_vmProc(static_cast<HB_USHORT>(iParams));
    }

    if (pVarResult)
    {
      hb_oleItemToVariantRef(pVarResult, hb_stackReturnItem(), nullptr, pObjFunc);
    }

    for (i = 0; i < iRefs; i++)
    {
      hb_oleVariantUpdate(refArray[i].variant, refArray[i].item, pObjFunc);
    }

    for (i = 0; i < iRefs; i++)
    {
      hb_stackPop();
    }

    hb_vmRequestRestore();
    return true;
  }
  return false;
}

/* IDispatch parameters, return value handling */

static void GetParams(DISPPARAMS *dispparam, HB_UINT uiOffset, HB_BOOL fUseRef, UINT uiNamedArgs, PHB_ITEM *pNamedArgs,
                      DISPID *pDispIds)
{
  VARIANTARG *pArgs = nullptr;

  auto uiArgCount = static_cast<UINT>(hb_pcount());
  if (uiOffset > uiArgCount)
  {
    uiArgCount = 0;
  }
  else
  {
    uiArgCount -= uiOffset;
  }

  if (uiArgCount > 0 || uiNamedArgs > 0)
  {
    VARIANTARG *pRefs;
    UINT uiArg, uiRefs;

    uiRefs = 0;
    if (fUseRef)
    {
      for (uiArg = 1; uiArg <= uiArgCount; uiArg++)
      {
        if (HB_ISBYREF(uiOffset + uiArg))
        {
          uiRefs++;
        }
      }
    }

    pArgs = static_cast<VARIANTARG *>(hb_xgrab(sizeof(VARIANTARG) * (uiArgCount + uiRefs + uiNamedArgs)));
    pRefs = &pArgs[uiArgCount + uiNamedArgs];

    for (uiArg = 0; uiArg < uiNamedArgs; uiArg++)
    {
      VARIANT *pVariant = &pArgs[uiArg];
      VariantInit(pVariant);
      hb_oleItemToVariantRef(pVariant, pNamedArgs[uiArg], nullptr, nullptr);
    }

    for (uiArg = 0; uiArg < uiArgCount; uiArg++)
    {
      VARIANT *pVariant = &pArgs[uiArg + uiNamedArgs];
      auto iParam = static_cast<int>(uiOffset + uiArgCount - uiArg);

      VariantInit(pVariant);
      if (fUseRef && HB_ISBYREF(iParam))
      {
        VariantInit(pRefs);
        hb_oleItemToVariantRef(pRefs, hb_param(iParam, Harbour::Item::ANY), pVariant, nullptr);
        ++pRefs;
      }
      else
      {
        hb_oleItemToVariantRef(pVariant, hb_param(iParam, Harbour::Item::ANY), nullptr, nullptr);
      }
    }
  }

  dispparam->rgvarg = pArgs;
  dispparam->cArgs = uiArgCount + uiNamedArgs;
  dispparam->rgdispidNamedArgs = pDispIds;
  dispparam->cNamedArgs = uiNamedArgs;
}

#define HB_OLE_MAX_NAMEDARGS 32

static HRESULT GetNamedParams(IDispatch *pDisp, OLECHAR *szMethodName, PHB_ITEM pHash, UINT *puiNamedArgs,
                              PHB_ITEM *pArgs, DISPID *pDispIds)
{
  OLECHAR *pNames[HB_OLE_MAX_NAMEDARGS + 1];
  void *phStrings[HB_OLE_MAX_NAMEDARGS];
  HB_SIZE nLen = hb_hashLen(pHash);
  int iArgs = 0;
  HRESULT lOleError;

  pNames[0] = szMethodName;

  for (HB_SIZE nPos = 1; nPos <= nLen; ++nPos)
  {
    PHB_ITEM pKey = hb_hashGetKeyAt(pHash, nPos);
    if (HB_IS_STRING(pKey))
    {
      pNames[iArgs + 1] =
          const_cast<HB_WCHAR *>(hb_itemGetStrU16(pKey, HB_CDP_ENDIAN_NATIVE, &phStrings[iArgs], nullptr));
      if (pNames[iArgs + 1] != nullptr)
      {
        pArgs[iArgs] = hb_hashGetValueAt(pHash, nPos);
        if (++iArgs == HB_OLE_MAX_NAMEDARGS)
        {
          break;
        }
      }
    }
  }
  *puiNamedArgs = static_cast<UINT>(iArgs);

  lOleError = HB_VTBL(pDisp)->GetIDsOfNames(HB_THIS_(pDisp) HB_ID_REF(IID_NULL), pNames, iArgs + 1, LOCALE_USER_DEFAULT,
                                            pDispIds);
  for (auto iArg = 0; iArg < iArgs; ++iArg)
  {
    hb_strfree(phStrings[iArg]);
  }

  return lOleError;
}

static void PutParams(DISPPARAMS *dispparam, HB_UINT uiOffset, HB_USHORT uiClass)
{
  VARIANTARG *pRefs = &dispparam->rgvarg[dispparam->cArgs];
  PHB_ITEM pItem = nullptr;

  if (dispparam->cNamedArgs > 0)
  {
    ++uiOffset;
  }

  for (UINT uiArg = dispparam->cNamedArgs; uiArg < dispparam->cArgs; uiArg++)
  {
    auto uiParam = static_cast<HB_USHORT>(uiOffset + dispparam->cArgs - uiArg);

    if (HB_ISBYREF(uiParam))
    {
      if (!pItem)
      {
        pItem = hb_itemNew(nullptr);
      }
      hb_oleVariantToItemEx(pItem, &dispparam->rgvarg[uiArg], uiClass);
      hb_itemParamStoreForward(uiParam, pItem);
      VariantClear(pRefs);
      pRefs++;
    }
  }
  if (pItem)
  {
    hb_itemRelease(pItem);
  }
}

static void FreeParams(DISPPARAMS *dispparam)
{
  if (dispparam->cArgs > 0)
  {
    for (UINT ui = 0; ui < dispparam->cArgs; ui++)
    {
      VariantClear(&dispparam->rgvarg[ui]);
    }

    hb_xfree(dispparam->rgvarg);
  }
}

/* PRG level functions and methods */

HB_FUNC(__OLEISDISP)
{
  hb_retl(hb_oleItemGet(hb_param(1, Harbour::Item::ANY)) != nullptr);
}

HB_FUNC(WIN_OLECLASSEXISTS) /* ( cOleName | cCLSID ) */
{
  bool fExists = false;
  auto cOleName = hb_parc(1);

  if (cOleName)
  {
    CLSID ClassID;
    wchar_t *cCLSID = AnsiToWide(cOleName);
    if (cOleName[0] == '{')
    {
      fExists = (CLSIDFromString(static_cast<LPOLESTR>(cCLSID), &ClassID) == NOERROR);
    }
    else
    {
      fExists = (CLSIDFromProgID(static_cast<LPCOLESTR>(cCLSID), &ClassID) == S_OK);
    }
    hb_xfree(cCLSID);
  }

  hb_retl(fExists);
}

HB_FUNC(__OLECREATEOBJECT) /* (cOleName | cCLSID  [, cIID ]) */
{
  IDispatch *pDisp = nullptr;
  auto cOleName = hb_parc(1);
  HRESULT lOleError;

  hb_oleInit();

  if (cOleName)
  {
    GUID ClassID, iid = IID_IDispatch;
    auto cID = hb_parc(2);

    wchar_t *cCLSID = AnsiToWide(cOleName);

    if (cOleName[0] == '{')
    {
      lOleError = CLSIDFromString(static_cast<LPOLESTR>(cCLSID), &ClassID);
    }
    else
    {
      lOleError = CLSIDFromProgID(static_cast<LPCOLESTR>(cCLSID), &ClassID);
    }
    hb_xfree(cCLSID);

    if (cID)
    {
      if (cID[0] == '{')
      {
        cCLSID = AnsiToWide(cID);
        lOleError = CLSIDFromString(static_cast<LPOLESTR>(cCLSID), &iid);
        hb_xfree(cCLSID);
      }
      else if (hb_parclen(2) == static_cast<HB_SIZE>(sizeof(iid)))
      {
        memcpy(static_cast<LPVOID>(&iid), cID, sizeof(iid));
      }
    }

    if (lOleError == S_OK)
    {
      lOleError = CoCreateInstance(HB_ID_REF(ClassID), nullptr, CLSCTX_SERVER, HB_ID_REF(iid),
                                   static_cast<void **>(static_cast<void *>(&pDisp)));
    }
  }
  else
  {
    lOleError = CO_E_CLASSSTRING;
  }

  hb_oleSetError(lOleError);
  if (lOleError == S_OK)
  {
    hb_oleItemPut(hb_stackReturnItem(), pDisp);
  }
  else
  {
    hb_ret();
  }
}

HB_FUNC(__OLEGETACTIVEOBJECT) /* (cOleName | cCLSID  [, cIID ]) */
{
  IDispatch *pDisp = nullptr;
  auto cOleName = hb_parc(1);
  HRESULT lOleError;

  hb_oleInit();

  if (cOleName)
  {
    IID ClassID, iid = IID_IDispatch;
    IUnknown *pUnk = nullptr;
    auto cID = hb_parc(2);

    auto wCLSID = static_cast<BSTR>(AnsiToWide(cOleName));

    if (cOleName[0] == '{')
    {
      lOleError = CLSIDFromString(wCLSID, static_cast<LPCLSID>(&ClassID));
    }
    else
    {
      lOleError = CLSIDFromProgID(wCLSID, static_cast<LPCLSID>(&ClassID));
    }
    hb_xfree(wCLSID);

    if (cID)
    {
      if (cID[0] == '{')
      {
        wCLSID = static_cast<BSTR>(AnsiToWide(cID));
        lOleError = CLSIDFromString(wCLSID, &iid);
        hb_xfree(wCLSID);
      }
      else if (hb_parclen(2) == static_cast<HB_SIZE>(sizeof(iid)))
      {
        memcpy(static_cast<LPVOID>(&iid), cID, sizeof(iid));
      }
    }

    if (lOleError == S_OK)
    {
      lOleError = GetActiveObject(HB_ID_REF(ClassID), nullptr, &pUnk);

      if (lOleError == S_OK)
      {
        lOleError = HB_VTBL(pUnk)->QueryInterface(HB_THIS_(pUnk) HB_ID_REF(iid),
                                                  static_cast<void **>(static_cast<void *>(&pDisp)));
        HB_VTBL(pUnk)->Release(HB_THIS(pUnk));
      }
    }
  }
  else
  {
    lOleError = CO_E_CLASSSTRING;
  }

  hb_oleSetError(lOleError);
  if (lOleError == S_OK)
  {
    hb_oleItemPut(hb_stackReturnItem(), pDisp);
  }
  else
  {
    hb_ret();
  }
}

HB_FUNC(__OLEENUMCREATE) /* (__hObj) */
{
  IDispatch *pDisp;
  IEnumVARIANT *pEnum;
  VARIANTARG variant;
  DISPPARAMS dispparam{};
  EXCEPINFO excep{};
  UINT uiArgErr;
  HRESULT lOleError;

  pDisp = hb_oleParam(1);
  if (!pDisp)
  {
    return;
  }

  if (hb_parl(2))
  {
    hb_oleSetError(S_OK);
    hb_errRT_OLE(EG_UNSUPPORTED, 1003, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
    return;
  }

  VariantInit(&variant);

  lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) DISPID_NEWENUM, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                     DISPATCH_PROPERTYGET, &dispparam, &variant, &excep, &uiArgErr);

  if (lOleError == S_OK)
  {
    if (V_VT(&variant) == VT_UNKNOWN)
    {
      lOleError = HB_VTBL(V_UNKNOWN(&variant))
                      ->QueryInterface(HB_THIS_(HB_WIN_U3(&variant, punkVal)) HB_ID_REF(IID_IEnumVARIANT),
                                       static_cast<void **>(static_cast<void *>(&pEnum)));
    }
    else if (V_VT(&variant) == VT_DISPATCH)
    {
      lOleError = HB_VTBL(HB_WIN_U3(&variant, pdispVal))
                      ->QueryInterface(HB_THIS_(HB_WIN_U3(&variant, pdispVal)) HB_ID_REF(IID_IEnumVARIANT),
                                       static_cast<void **>(static_cast<void *>(&pEnum)));
    }
    else
    {
      VariantClear(&variant);
      hb_oleSetError(lOleError);
      hb_errRT_OLE(EG_ARG, 1004, static_cast<HB_ERRCODE>(lOleError), nullptr, HB_ERR_FUNCNAME, nullptr);
      return;
    }

    VariantClear(&variant);

    if (lOleError == S_OK)
    {
      hb_oleSetError(S_OK);

      auto ppEnum = static_cast<IEnumVARIANT **>(hb_gcAllocate(sizeof(IEnumVARIANT *), &s_gcOleenumFuncs));
      *ppEnum = pEnum;
      hb_retptrGC(ppEnum);
      return;
    }
  }
  hb_oleSetError(lOleError);
  hb_errRT_OLE(EG_ARG, 1005, static_cast<HB_ERRCODE>(lOleError), nullptr, HB_ERR_FUNCNAME, nullptr);
}

HB_FUNC(__OLEENUMNEXT)
{
  IEnumVARIANT *pEnum = hb_oleenumParam(1);
  bool fResult = false;

  if (pEnum)
  {
    VARIANTARG variant;

    hb_oleInit();

    VariantInit(&variant);
    if (HB_VTBL(pEnum)->Next(HB_THIS_(pEnum) 1, &variant, nullptr) == S_OK)
    {
      hb_oleVariantToItemEx(hb_stackReturnItem(), &variant, static_cast<HB_USHORT>(hb_parni(3)));
      VariantClear(&variant);
      fResult = true;
    }
  }
  hb_storl(fResult, 2);
}

HB_FUNC(WIN_OLEERROR)
{
  hb_retnint(hb_oleGetError());
}

#ifndef DISP_E_BUFFERTOOSMALL
#define DISP_E_BUFFERTOOSMALL 0x80020013L
#endif

HB_FUNC(WIN_OLEERRORTEXT)
{
  HRESULT lOleError = HB_ISNUM(1) ? hb_parnl(1) : hb_oleGetError();

  switch (lOleError)
  {
  case S_OK:
    hb_retc_null();
    break;
  case CO_E_CLASSSTRING:
    hb_retc_const("CO_E_CLASSSTRING");
    break;
  case OLE_E_WRONGCOMPOBJ:
    hb_retc_const("OLE_E_WRONGCOMPOBJ");
    break;
  case REGDB_E_CLASSNOTREG:
    hb_retc_const("REGDB_E_CLASSNOTREG");
    break;
  case REGDB_E_WRITEREGDB:
    hb_retc_const("REGDB_E_WRITEREGDB");
    break;
  case E_PENDING:
    hb_retc_const("E_PENDING");
    break;
  case E_NOTIMPL:
    hb_retc_const("E_NOTIMPL");
    break;
  case E_NOINTERFACE:
    hb_retc_const("E_NOINTERFACE");
    break;
  case E_POINTER:
    hb_retc_const("E_POINTER");
    break;
  case E_ABORT:
    hb_retc_const("E_ABORT");
    break;
  case E_FAIL:
    hb_retc_const("E_FAIL");
    break;
  case E_UNEXPECTED:
    hb_retc_const("E_UNEXPECTED");
    break;
  case E_ACCESSDENIED:
    hb_retc_const("E_ACCESSDENIED");
    break;
  case E_HANDLE:
    hb_retc_const("E_HANDLE");
    break;
  case E_OUTOFMEMORY:
    hb_retc_const("E_OUTOFMEMORY");
    break;
  case E_INVALIDARG:
    hb_retc_const("E_INVALIDARG");
    break;
  case DISP_E_UNKNOWNINTERFACE:
    hb_retc_const("DISP_E_UNKNOWNINTERFACE");
    break;
  case DISP_E_MEMBERNOTFOUND:
    hb_retc_const("DISP_E_MEMBERNOTFOUND");
    break;
  case DISP_E_PARAMNOTFOUND:
    hb_retc_const("DISP_E_PARAMNOTFOUND");
    break;
  case DISP_E_TYPEMISMATCH:
    hb_retc_const("DISP_E_TYPEMISMATCH");
    break;
  case DISP_E_UNKNOWNNAME:
    hb_retc_const("DISP_E_UNKNOWNNAME");
    break;
  case DISP_E_NONAMEDARGS:
    hb_retc_const("DISP_E_NONAMEDARGS");
    break;
  case DISP_E_BADVARTYPE:
    hb_retc_const("DISP_E_BADVARTYPE");
    break;
  case DISP_E_EXCEPTION:
    hb_retc_const("DISP_E_EXCEPTION");
    break;
  case DISP_E_OVERFLOW:
    hb_retc_const("DISP_E_OVERFLOW");
    break;
  case DISP_E_BADINDEX:
    hb_retc_const("DISP_E_BADINDEX");
    break;
  case DISP_E_UNKNOWNLCID:
    hb_retc_const("DISP_E_UNKNOWNLCID");
    break;
  case DISP_E_ARRAYISLOCKED:
    hb_retc_const("DISP_E_ARRAYISLOCKED");
    break;
  case DISP_E_BADPARAMCOUNT:
    hb_retc_const("DISP_E_BADPARAMCOUNT");
    break;
  case DISP_E_PARAMNOTOPTIONAL:
    hb_retc_const("DISP_E_PARAMNOTOPTIONAL");
    break;
  case DISP_E_BADCALLEE:
    hb_retc_const("DISP_E_BADCALLEE");
    break;
  case DISP_E_NOTACOLLECTION:
    hb_retc_const("DISP_E_NOTACOLLECTION");
    break;
  case DISP_E_DIVBYZERO:
    hb_retc_const("DISP_E_DIVBYZERO");
    break;
  case DISP_E_BUFFERTOOSMALL:
    hb_retc_const("DISP_E_BUFFERTOOSMALL");
    break;
  default: {
    char buf[16];
    hb_snprintf(buf, 16, "0x%08x", static_cast<UINT>(static_cast<HB_PTRUINT>(lOleError)));
    hb_retc(buf);
  }
  }
}

/*

VBScript and Harbour syntax and IDispatch:Invoke() usage differences

VBScript syntax          dispid        DISPATCH_* flags  argcnt | Harbour syntax      :Invoke parameters
================================================================+=======================================
obj                      DISPID_VALUE  METHOD+PROPERTYGET  0    | Same                :Invoke is not used
obj()                    DISPID_VALUE  METHOD              0    | Not supported
obj(param)               DISPID_VALUE  METHOD+PROPERTYGET  1    | obj[param]          Same
obj.name                 name          METHOD+PROPERTYGET  0    | Same, =obj.name()   Same
obj.name()               name          METHOD              0    | Same, =obj.name     flags=METHOD+PROPERTYGET
obj.name(param)          name          METHOD+PROPERTYGET  1    | Same                Same
                                                                |
obj = value              obj reassigned, :Invoke is not used    | Same
obj() = value            DISPID_VALUE  PROPERTYPUT         1    | Not supported
obj(param) = value       DISPID_VALUE  PROPERTYPUT         2    | obj[param] = value
obj.name = value         name          PROPERTYPUT         1    | Same                Same
obj.name() = value       name          PROPERTYPUT         1    | Not supported, use obj.name = value
obj.name(param) = value  name          PROPERTYPUT         2    | Not supported, workaround obj._name(param, value)

*/

HB_FUNC(WIN_OLEAUTO___ONERROR)
{
  IDispatch *pDisp;
  const char *szMethod;
  wchar_t szMethodWide[HB_SYMBOL_NAME_LEN + 1];
  OLECHAR *pMemberArray;
  DISPID dispid = 0;
  DISPPARAMS dispparam;
  VARIANTARG variant;
  EXCEPINFO excep;
  UINT uiArgErr;
  HRESULT lOleError;
  HB_USHORT uiClass;

  hb_oleInit();

  uiClass = hb_objGetClass(hb_stackSelfItem());
  if (uiClass == 0)
  {
    return;
  }

  /* Get object handle */
  hb_vmPushDynSym(s_pDyns_hObjAccess);
  hb_vmPush(hb_stackSelfItem());
  hb_vmSend(0);

  pDisp = hb_oleParam(-1);
  if (!pDisp)
  {
    return;
  }

  auto iPCount = hb_pcount();

  szMethod = hb_itemGetSymbol(hb_stackBaseItem())->szName;
  AnsiToWideBuffer(szMethod, szMethodWide, static_cast<int>(HB_SIZEOFARRAY(szMethodWide)));

  /* Try property put */

  if (szMethod[0] == '_' && iPCount >= 1)
  {
    pMemberArray = &szMethodWide[1];
    lOleError = HB_VTBL(pDisp)->GetIDsOfNames(HB_THIS_(pDisp) HB_ID_REF(IID_NULL), &pMemberArray, 1,
                                              LOCALE_USER_DEFAULT, &dispid);

    if (lOleError == S_OK)
    {
      DISPID lPropPut = DISPID_PROPERTYPUT;

      memset(&excep, 0, sizeof(excep));
      GetParams(&dispparam, 0, false, 0, nullptr, nullptr);
      dispparam.rgdispidNamedArgs = &lPropPut;
      dispparam.cNamedArgs = 1;

      lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) dispid, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                         DISPATCH_PROPERTYPUT, &dispparam, nullptr, &excep, &uiArgErr);
      FreeParams(&dispparam);

      /* assign method should return assigned value */
      hb_itemReturn(hb_param(iPCount, Harbour::Item::ANY));

      hb_oleSetError(lOleError);
      if (lOleError != S_OK)
      {
        char *szDescription = nullptr;
        char *szSource = nullptr;

        if (lOleError == DISP_E_EXCEPTION)
        {
          hb_oleExcepDescription(&excep, &szDescription, &szSource);
        }

        hb_errRT_OLE(EG_ARG, 1006, static_cast<HB_ERRCODE>(lOleError), szDescription, HB_ERR_FUNCNAME, szSource);

        if (szSource)
        {
          hb_xfree(szSource);
        }

        if (szDescription)
        {
          hb_xfree(szDescription);
        }
      }
      return;
    }
  }

  /* Try property get and invoke */

  if (iPCount >= 1 && HB_ISHASH(1))
  {
    /* named parameters are passed in hash array */
    PHB_ITEM pArgs[HB_OLE_MAX_NAMEDARGS];
    DISPID pDispIds[HB_OLE_MAX_NAMEDARGS + 1];
    UINT uiNamedArgs;

    lOleError = GetNamedParams(pDisp, szMethodWide, hb_param(1, Harbour::Item::HASH), &uiNamedArgs, pArgs, pDispIds);
    if (lOleError == S_OK)
    {
      dispid = pDispIds[0];
      GetParams(&dispparam, 1, true, uiNamedArgs, pArgs, &pDispIds[1]);
    }
  }
  else
  {
    pMemberArray = szMethodWide;
    lOleError = HB_VTBL(pDisp)->GetIDsOfNames(HB_THIS_(pDisp) HB_ID_REF(IID_NULL), &pMemberArray, 1,
                                              LOCALE_USER_DEFAULT, &dispid);
    if (lOleError == S_OK)
    {
      GetParams(&dispparam, 0, true, 0, nullptr, nullptr);
    }
  }

  if (lOleError == S_OK)
  {
    memset(&excep, 0, sizeof(excep));
    VariantInit(&variant);

    lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) dispid, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                       DISPATCH_PROPERTYGET | DISPATCH_METHOD, &dispparam, &variant, &excep, &uiArgErr);

    PutParams(&dispparam, 0, uiClass);
    FreeParams(&dispparam);

    hb_oleVariantToItemEx(hb_stackReturnItem(), &variant, uiClass);
    VariantClear(&variant);

    hb_oleSetError(lOleError);
    if (lOleError != S_OK)
    {
      char *szDescription = nullptr;
      char *szSource = nullptr;

      if (lOleError == DISP_E_EXCEPTION)
      {
        hb_oleExcepDescription(&excep, &szDescription, &szSource);
      }

      hb_errRT_OLE(EG_ARG, 1007, static_cast<HB_ERRCODE>(lOleError), szDescription, HB_ERR_FUNCNAME, szSource);

      if (szSource)
      {
        hb_xfree(szSource);
      }

      if (szDescription)
      {
        hb_xfree(szDescription);
      }
    }
    return;
  }

  hb_oleSetError(lOleError);

  /* TODO: add description containing TypeName of the object */
  if (szMethod[0] == '_')
  {
    hb_errRT_OLE(EG_NOVARMETHOD, 1008, static_cast<HB_ERRCODE>(lOleError), nullptr, szMethod + 1, nullptr);
  }
  else
  {
    hb_errRT_OLE(EG_NOMETHOD, 1009, static_cast<HB_ERRCODE>(lOleError), nullptr, szMethod, nullptr);
  }
}

HB_FUNC(WIN_OLEAUTO___OPINDEX)
{
  IDispatch *pDisp;
  DISPPARAMS dispparam;
  VARIANTARG variant;
  EXCEPINFO excep;
  UINT uiArgErr;
  HRESULT lOleError;
  bool fAssign;
  HB_USHORT uiClass;

  hb_oleInit();

  uiClass = hb_objGetClass(hb_stackSelfItem());
  if (uiClass == 0)
  {
    return;
  }

  /* Get object handle */
  hb_vmPushDynSym(s_pDyns_hObjAccess);
  hb_vmPush(hb_stackSelfItem());
  hb_vmSend(0);

  pDisp = hb_oleParam(-1);
  if (!pDisp)
  {
    return;
  }

  fAssign = hb_pcount() > 1;

  if (fAssign)
  {
    /* Assign */
    DISPID lPropPut = DISPID_PROPERTYPUT;

    memset(&excep, 0, sizeof(excep));
    GetParams(&dispparam, 0, false, 0, nullptr, nullptr);
    dispparam.rgdispidNamedArgs = &lPropPut;
    dispparam.cNamedArgs = 1;

    lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) DISPID_VALUE, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                       DISPATCH_PROPERTYPUT, &dispparam, nullptr, &excep, &uiArgErr);
    FreeParams(&dispparam);

    /* assign method should return assigned value */
    hb_itemReturn(hb_param(hb_pcount(), Harbour::Item::ANY));
  }
  else
  {
    /* Access */
    memset(&excep, 0, sizeof(excep));
    VariantInit(&variant);
    GetParams(&dispparam, 0, true, 0, nullptr, nullptr);

    lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) DISPID_VALUE, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                       DISPATCH_PROPERTYGET | DISPATCH_METHOD, &dispparam, &variant, &excep, &uiArgErr);

    PutParams(&dispparam, 0, uiClass);
    FreeParams(&dispparam);

    hb_oleVariantToItemEx(hb_stackReturnItem(), &variant, uiClass);
    VariantClear(&variant);
  }

  hb_oleSetError(lOleError);

  if (lOleError != S_OK)
  {
    /* Try to detect if object is a collection */
    char *szDescription = nullptr;
    char *szSource = nullptr;
    HRESULT lOleErrorEnum;

    if (lOleError == DISP_E_EXCEPTION)
    {
      hb_oleExcepDescription(&excep, &szDescription, &szSource);
    }

    memset(&excep, 0, sizeof(excep));
    memset(&dispparam, 0, sizeof(dispparam));
    VariantInit(&variant);
    lOleErrorEnum = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) DISPID_NEWENUM, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT,
                                           DISPATCH_PROPERTYGET, &dispparam, &variant, &excep, &uiArgErr);
    VariantClear(&variant);

    hb_errRT_OLE(lOleErrorEnum == S_OK ? EG_BOUND : EG_ARG, 1016, static_cast<HB_ERRCODE>(lOleError),
                 lOleErrorEnum == S_OK ? hb_langDGetErrorDesc(fAssign ? EG_ARRASSIGN : EG_ARRACCESS) : szDescription,
                 nullptr, szSource);

    if (szDescription)
    {
      hb_xfree(szDescription);
    }

    if (szSource)
    {
      hb_xfree(szSource);
    }
  }
}

HB_FUNC(__OLEGETNAMEID)
{
  IDispatch *pDisp;

  pDisp = hb_oleParam(1);
  if (pDisp)
  {
    OLECHAR *pwszMethod;
    HRESULT lOleError;
    void *hMethod;
    DISPID dispid;

    pwszMethod = const_cast<OLECHAR *>(hb_parstr_u16(2, HB_CDP_ENDIAN_NATIVE, &hMethod, nullptr));
    lOleError = HB_VTBL(pDisp)->GetIDsOfNames(HB_THIS_(pDisp) HB_ID_REF(IID_NULL), &pwszMethod, 1, LOCALE_USER_DEFAULT,
                                              &dispid);
    hb_strfree(hMethod);
    if (lOleError == S_OK)
    {
      hb_retnint(dispid);
    }
  }
}

static void hb_oleInvokeCall(WORD wFlags)
{
  HB_USHORT uiOffset = 0;
  IDispatch *pDisp;

  auto pObject = hb_stackSelfItem();
  if (pObject->isNil())
  {
    pObject = hb_param(++uiOffset, Harbour::Item::ANY);
  }

  pDisp = pObject ? hb_oleItemGetDispatch(pObject) : nullptr;
  if (pDisp)
  {
    DISPID dispid;
    DISPPARAMS dispparam;
    VARIANTARG variant;
    EXCEPINFO excep;
    UINT uiArgErr;
    HRESULT lOleError;
    HB_USHORT uiClass;
    OLECHAR *pwszMethod;
    void *hMethod;

    uiClass = hb_objGetClass(pObject);
    ++uiOffset;
    pwszMethod = const_cast<OLECHAR *>(hb_parstr_u16(uiOffset, HB_CDP_ENDIAN_NATIVE, &hMethod, nullptr));
    if (pwszMethod)
    {
      lOleError = HB_VTBL(pDisp)->GetIDsOfNames(HB_THIS_(pDisp) HB_ID_REF(IID_NULL), &pwszMethod, 1,
                                                LOCALE_USER_DEFAULT, &dispid);
      hb_strfree(hMethod);
    }
    else
    {
      dispid = static_cast<DISPID>(hb_parnint(uiOffset));
      lOleError = S_OK;
    }
    if (lOleError == S_OK)
    {
      DISPID lPropPut = wFlags;
      HB_BOOL fPut = wFlags == DISPATCH_PROPERTYPUT;

      memset(&excep, 0, sizeof(excep));
      VariantInit(&variant);
      GetParams(&dispparam, uiOffset, !fPut, 0, nullptr, nullptr);
      if (fPut)
      {
        dispparam.rgdispidNamedArgs = &lPropPut;
        dispparam.cNamedArgs = 1;
      }

      lOleError = HB_VTBL(pDisp)->Invoke(HB_THIS_(pDisp) dispid, HB_ID_REF(IID_NULL), LOCALE_USER_DEFAULT, wFlags,
                                         &dispparam, &variant, &excep, &uiArgErr);

      if (!fPut)
      {
        PutParams(&dispparam, uiOffset, uiClass);
      }
      FreeParams(&dispparam);

      hb_oleVariantToItemEx(hb_stackReturnItem(), &variant, uiClass);
      VariantClear(&variant);

      hb_oleSetError(lOleError);
      if (lOleError != S_OK)
      {
        char *szExcepDescription = nullptr;
        char *szExcepSource = nullptr;

        if (lOleError == DISP_E_EXCEPTION)
        {
          hb_oleExcepDescription(&excep, &szExcepDescription, &szExcepSource);
        }

        hb_errRT_OLE(EG_ARG, 1007, static_cast<HB_ERRCODE>(lOleError), szExcepDescription, HB_ERR_FUNCNAME,
                     szExcepSource);

        if (szExcepDescription)
        {
          hb_xfree(szExcepDescription);
        }

        if (szExcepSource)
        {
          hb_xfree(szExcepSource);
        }
      }
      return;
    }
    hb_errRT_OLE(EG_NOMETHOD, 1009, static_cast<HB_ERRCODE>(lOleError), nullptr, hb_parc(uiOffset), nullptr);
  }
  else
  {
    hb_errRT_OLE(EG_ARG, 1001, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
  }
}

HB_FUNC(__OLEINVOKEMETHOD)
{
  hb_oleInvokeCall(DISPATCH_METHOD);
}

HB_FUNC(__OLEINVOKEGET)
{
  hb_oleInvokeCall(DISPATCH_PROPERTYGET);
}

HB_FUNC(__OLEINVOKEPUT)
{
  hb_oleInvokeCall(DISPATCH_PROPERTYPUT);
}

/* __oleVariantGetValue(<pVariant>) --> <xValue> */
HB_FUNC(__OLEVARIANTGETVALUE)
{
  VARIANT *pVariant = hb_oleVariantParam(1);

  if (pVariant)
  {
    hb_oleVariantToItemEx(hb_stackReturnItem(), pVariant, 0);
  }
}

/* __oleVariantGetType(<pVariant>) --> <nVariantType> */
HB_FUNC(__OLEVARIANTGETTYPE)
{
  VARIANT *pVariant = hb_oleVariantParam(1);

  if (pVariant)
  {
    hb_retni(V_VT(pVariant));
  }
}

/* __oleVariantNew(<nVariantType>, [<xInitValue>], [<nDims,...>]) --> <pVariant> */
HB_FUNC(__OLEVARIANTNEW)
{
  auto iType = hb_parni(1);
  auto pInit = hb_param(2, Harbour::Item::ANY);
  IDispatch *pDisp;
  VARIANT variant;

  V_VT(&variant) = VT_ILLEGAL;

  if (pInit && HB_IS_ARRAY(pInit) && iType == (iType & VT_TYPEMASK))
  {
    iType |= VT_ARRAY;
  }

  switch (iType)
  {
  case VT_EMPTY:
  case VT_NULL:
    V_VT(&variant) = static_cast<VARTYPE>(iType);
    break;

  case VT_BOOL:
    if (pInit == nullptr || HB_IS_LOGICAL(pInit))
    {
      V_VT(&variant) = VT_BOOL;
      V_BOOL(&variant) = hb_itemGetL(pInit) ? TRUE : FALSE;
    }
    break;

  case VT_I1:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_I1;
      V_I1(&variant) = static_cast<signed char>(hb_itemGetNI(pInit));
    }
    break;

  case VT_I2:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_I2;
      V_I2(&variant) = static_cast<short>(hb_itemGetNI(pInit));
    }
    break;

  case VT_I4:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_I4;
      V_I4(&variant) = hb_itemGetNL(pInit);
    }
    break;

  case VT_I8:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_I8;
#if defined(HB_OLE_NO_LL)
      /* workaround for wrong OLE variant structure definition */
      *(static_cast<HB_LONGLONG *>(&V_I4(&variant))) = hb_itemGetNInt(pInit);
#else
      V_I8(&variant) = hb_itemGetNInt(pInit);
#endif
    }
    break;

  case VT_UI1:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_UI1;
      V_UI1(&variant) = static_cast<unsigned char>(hb_itemGetNI(pInit));
    }
    break;

  case VT_UI2:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_UI2;
      V_UI2(&variant) = static_cast<unsigned short>(hb_itemGetNI(pInit));
    }
    break;

  case VT_UI4:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_UI4;
      V_UI4(&variant) = hb_itemGetNL(pInit);
    }
    break;

  case VT_UI8:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_UI8;
#if defined(HB_OLE_NO_LL)
      /* workaround for wrong OLE variant structure definition */
      *(static_cast<HB_ULONGLONG *>(&V_I4(&variant))) = hb_itemGetNInt(pInit);
#else
      V_UI8(&variant) = hb_itemGetNInt(pInit);
#endif
    }
    break;

  case VT_INT:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_INT;
      V_INT(&variant) = hb_itemGetNI(pInit);
    }
    break;

  case VT_UINT:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_UINT;
      V_UINT(&variant) = static_cast<UINT>(hb_itemGetNI(pInit));
    }
    break;

  case VT_ERROR:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_ERROR;
      V_ERROR(&variant) = static_cast<SCODE>(hb_itemGetNL(pInit));
    }
    break;

  case VT_R4:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_R4;
      V_R4(&variant) = static_cast<float>(hb_itemGetND(pInit));
    }
    break;

  case VT_R8:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_R8;
      V_R8(&variant) = hb_itemGetND(pInit);
    }
    break;

  case VT_CY:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_CY;
      VarCyFromR8(hb_itemGetND(pInit), &V_CY(&variant));
    }
    break;

  case VT_DECIMAL:
    if (pInit == nullptr || HB_IS_NUMERIC(pInit))
    {
      V_VT(&variant) = VT_DECIMAL;
      VarDecFromR8(hb_itemGetND(pInit), &HB_WIN_U1(&variant, decVal) /*&V_DECIMAL(&variant)*/);
    }
    break;

  case VT_DATE:
    if (pInit == nullptr || HB_IS_DATETIME(pInit))
    {
      V_VT(&variant) = VT_DATE;
      V_R8(&variant) = hb_itemGetTD(pInit) - HB_OLE_DATE_BASE;
    }
    break;

#ifdef HB_OLE_PASS_POINTERS
  case VT_PTR:
    if (pInit == nullptr || HB_IS_POINTER(pInit))
    {
      V_VT(&variant) = VT_PTR;
      V_BYREF(&variant) = hb_itemGetPtr(pInit);
    }
    break;
#endif

  case VT_BSTR:
    if (pInit == nullptr || HB_IS_STRING(pInit))
    {
      V_VT(&variant) = VT_BSTR;
      V_BSTR(&variant) = hb_oleItemToString(pInit);
    }
    break;

  case VT_UNKNOWN:
    pDisp = hb_oleItemGetDispatch(pInit);
    if (pDisp)
    {
      if (HB_VTBL(pDisp)->QueryInterface(HB_THIS_(pDisp) HB_ID_REF(IID_IEnumVARIANT),
                                         static_cast<void **>(static_cast<void *>(&V_UNKNOWN(&variant)))) == S_OK)
      {
        V_VT(&variant) = VT_UNKNOWN;
      }
    }
    break;

  case VT_DISPATCH:
    pDisp = hb_oleItemGetDispatch(pInit);
    if (pDisp)
    {
      V_VT(&variant) = VT_DISPATCH;
      V_DISPATCH(&variant) = pDisp;
      HB_VTBL(pDisp)->AddRef(HB_THIS(pDisp));
    }
    break;

  case VT_VARIANT:
    VariantInit(&variant);
    if (pInit != nullptr)
    {
      hb_oleItemToVariant(&variant, pInit);
    }
    break;

  default:
    if (iType & VT_ARRAY)
    {
      iType &= ~VT_ARRAY;
      if (iType == (iType & VT_TYPEMASK))
      {
        long plBuf[16], *plSize;
        int iPCount = hb_pcount() - 2, iDims;

        if (iPCount < 0)
        {
          iPCount = 0;
        }
        iDims = iPCount;

        plSize = iDims < static_cast<int>(HB_SIZEOFARRAY(plBuf)) ? plBuf
                                                                 : static_cast<long *>(hb_xgrab(sizeof(long) * iDims));

        while (iPCount > 0 && HB_ISNIL(iPCount + 2))
        {
          --iPCount;
        }
        while (iPCount > 0)
        {
          --iPCount;
          plSize[iPCount] = hb_parnl(iPCount + 3);
          if (plSize[iPCount] <= 0)
          {
            break;
          }
        }

        if (iPCount <= 0)
        {
          V_ARRAY(&variant) = hb_oleSafeArrayFromItem(pInit, static_cast<VARTYPE>(iType), iDims, plSize);
          if (V_ARRAY(&variant))
          {
            V_VT(&variant) = static_cast<VARTYPE>(iType | VT_ARRAY);
          }
        }
        if (plSize != plBuf)
        {
          hb_xfree(plBuf);
        }
        break;
      }
    }
    break;
  }

  if (V_VT(&variant) != VT_ILLEGAL)
  {
    hb_oleItemPutVariant(hb_stackReturnItem(), &variant, true);
  }
  else
  {
    hb_errRT_OLE(EG_ARG, 1018, 0, nullptr, HB_ERR_FUNCNAME, nullptr);
  }
}

/* __oleVariantNullDate([<lNewNullDateFlag>]) --> <lPrevNullDateFlag> */
HB_FUNC(__OLEVARIANTNULLDATE)
{
  hb_retl(hb_oleGetNullDateFlag());
  if (HB_ISLOG(1))
  {
    hb_oleSetNullDateFlag(hb_parl(1));
  }
}

/* __oleVariantNil2Null([<lNewNil2NullFlag>]) --> <lPrevNil2NullFlag> */
HB_FUNC(__OLEVARIANTNIL2NULL)
{
  hb_retl(hb_oleGetNil2NullFlag());
  if (HB_ISLOG(1))
  {
    hb_oleSetNil2NullFlag(hb_parl(1));
  }
}

HB_CALL_ON_STARTUP_BEGIN(_hb_olecore_init_)
hb_vmAtInit(hb_olecore_init, nullptr);
HB_CALL_ON_STARTUP_END(_hb_olecore_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup _hb_olecore_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY HB_DATASEG_FUNC(_hb_olecore_init_)
#include "hbiniseg.hpp"
#endif
