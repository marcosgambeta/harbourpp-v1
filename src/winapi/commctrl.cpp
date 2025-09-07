//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <vector>
#include <commctrl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// WINCOMMCTRLAPI void WINAPI InitCommonControls(void)
HB_FUNC(WAINITCOMMONCONTROLS)
{
  InitCommonControls();
}

// WINCOMMCTRLAPI WINBOOL WINAPI InitCommonControlsEx(const INITCOMMONCONTROLSEX *)
HB_FUNC(WAINITCOMMONCONTROLSEX)
{
  wa_ret_BOOL(InitCommonControlsEx(wa_par_INITCOMMONCONTROLSEX(1)));
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Create(int cx,int cy,UINT flags,int cInitial,int cGrow)
HB_FUNC(WAIMAGELIST_CREATE)
{
  wa_ret_HIMAGELIST(ImageList_Create(wa_par_int(1), wa_par_int(2), wa_par_UINT(3), wa_par_int(4), wa_par_int(5)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Destroy(HIMAGELIST himl)
HB_FUNC(WAIMAGELIST_DESTROY)
{
  wa_ret_BOOL(ImageList_Destroy(wa_par_HIMAGELIST(1)));
}

// WINCOMMCTRLAPI int WINAPI ImageList_GetImageCount(HIMAGELIST himl)
HB_FUNC(WAIMAGELIST_GETIMAGECOUNT)
{
  wa_ret_int(ImageList_GetImageCount(wa_par_HIMAGELIST(1)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetImageCount(HIMAGELIST himl,UINT uNewCount)
HB_FUNC(WAIMAGELIST_SETIMAGECOUNT)
{
  wa_ret_BOOL(ImageList_SetImageCount(wa_par_HIMAGELIST(1), wa_par_UINT(2)));
}

// WINCOMMCTRLAPI int WINAPI ImageList_Add(HIMAGELIST himl,HBITMAP hbmImage,HBITMAP hbmMask)
HB_FUNC(WAIMAGELIST_ADD)
{
  wa_ret_int(ImageList_Add(wa_par_HIMAGELIST(1), wa_par_HBITMAP(2), wa_par_HBITMAP(3)));
}

// WINCOMMCTRLAPI int WINAPI ImageList_ReplaceIcon(HIMAGELIST himl,int i,HICON hicon)
HB_FUNC(WAIMAGELIST_REPLACEICON)
{
  wa_ret_int(ImageList_ReplaceIcon(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HICON(3)));
}

// WINCOMMCTRLAPI COLORREF WINAPI ImageList_SetBkColor(HIMAGELIST himl,COLORREF clrBk)
HB_FUNC(WAIMAGELIST_SETBKCOLOR)
{
  wa_ret_COLORREF(ImageList_SetBkColor(wa_par_HIMAGELIST(1), wa_par_COLORREF(2)));
}

// WINCOMMCTRLAPI COLORREF WINAPI ImageList_GetBkColor(HIMAGELIST himl)
HB_FUNC(WAIMAGELIST_GETBKCOLOR)
{
  wa_ret_COLORREF(ImageList_GetBkColor(wa_par_HIMAGELIST(1)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetOverlayImage(HIMAGELIST himl,int iImage,int iOverlay)
HB_FUNC(WAIMAGELIST_SETOVERLAYIMAGE)
{
  wa_ret_BOOL(ImageList_SetOverlayImage(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_int(3)));
}

// int ImageList_AddIcon(HIMAGELIST himl,HICON  hicon)
HB_FUNC(WAIMAGELIST_ADDICON)
{
  wa_ret_int(ImageList_AddIcon(wa_par_HIMAGELIST(1), wa_par_HICON(2)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Draw(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,UINT fStyle)
HB_FUNC(WAIMAGELIST_DRAW)
{
  wa_ret_BOOL(
      ImageList_Draw(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HDC(3), wa_par_int(4), wa_par_int(5), wa_par_UINT(6)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Replace(HIMAGELIST himl,int i,HBITMAP hbmImage,HBITMAP hbmMask)
HB_FUNC(WAIMAGELIST_REPLACE)
{
  wa_ret_BOOL(ImageList_Replace(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HBITMAP(3), wa_par_HBITMAP(4)));
}

// WINCOMMCTRLAPI int WINAPI ImageList_AddMasked(HIMAGELIST himl,HBITMAP hbmImage,COLORREF crMask)
HB_FUNC(WAIMAGELIST_ADDMASKED)
{
  wa_ret_int(ImageList_AddMasked(wa_par_HIMAGELIST(1), wa_par_HBITMAP(2), wa_par_COLORREF(3)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawEx(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,int dx,int dy,COLORREF
// rgbBk,COLORREF rgbFg,UINT fStyle)
HB_FUNC(WAIMAGELIST_DRAWEX)
{
  wa_ret_BOOL(ImageList_DrawEx(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HDC(3), wa_par_int(4), wa_par_int(5),
                               wa_par_int(6), wa_par_int(7), wa_par_COLORREF(8), wa_par_COLORREF(9), wa_par_UINT(10)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawIndirect(IMAGELISTDRAWPARAMS *pimldp)
HB_FUNC(WAIMAGELIST_DRAWINDIRECT)
{
  wa_ret_BOOL(ImageList_DrawIndirect(wa_par_IMAGELISTDRAWPARAMS(1)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Remove(HIMAGELIST himl,int i)
HB_FUNC(WAIMAGELIST_REMOVE)
{
  wa_ret_BOOL(ImageList_Remove(wa_par_HIMAGELIST(1), wa_par_int(2)));
}

// WINCOMMCTRLAPI HICON WINAPI ImageList_GetIcon(HIMAGELIST himl,int i,UINT flags)
HB_FUNC(WAIMAGELIST_GETICON)
{
  wa_ret_HICON(ImageList_GetIcon(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_UINT(3)));
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageA(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask,UINT
// uType,UINT uFlags)
#if 0
HB_FUNC(WAIMAGELIST_LOADIMAGEA)
{
  wa_ret_HIMAGELIST(ImageList_LoadImageA(wa_par_HINSTANCE(1), wa_par_LPCSTR(2), wa_par_int(3), wa_par_int(4), wa_par_COLORREF(5), wa_par_UINT(6), wa_par_UINT(7)));
}
#endif

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageW(HINSTANCE hi,LPCWSTR lpbmp,int cx,int cGrow,COLORREF
// crMask,UINT uType,UINT uFlags)
#if 0
HB_FUNC(WAIMAGELIST_LOADIMAGEW)
{
  wa_ret_HIMAGELIST(ImageList_LoadImageW(wa_par_HINSTANCE(1), wa_par_LPCWSTR(2), wa_par_int(3), wa_par_int(4), wa_par_COLORREF(5), wa_par_UINT(6), wa_par_UINT(7)));
}
#endif

HB_FUNC(WAIMAGELIST_LOADIMAGE)
{
  void *str{};
  wa_ret_HIMAGELIST(ImageList_LoadImageW(wa_par_HINSTANCE(1), HB_PARSTR(2, &str, nullptr), wa_par_int(3), wa_par_int(4),
                                         wa_par_COLORREF(5), wa_par_UINT(6), wa_par_UINT(7)));
  hb_strfree(str);
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Copy(HIMAGELIST himlDst,int iDst,HIMAGELIST himlSrc,int iSrc,UINT uFlags)
HB_FUNC(WAIMAGELIST_COPY)
{
  wa_ret_BOOL(ImageList_Copy(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HIMAGELIST(3), wa_par_int(4), wa_par_UINT(5)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_BeginDrag(HIMAGELIST himlTrack,int iTrack,int dxHotspot,int dyHotspot)
HB_FUNC(WAIMAGELIST_BEGINDRAG)
{
  wa_ret_BOOL(ImageList_BeginDrag(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_int(3), wa_par_int(4)));
}

// WINCOMMCTRLAPI void WINAPI ImageList_EndDrag(void)
HB_FUNC(WAIMAGELIST_ENDDRAG)
{
  ImageList_EndDrag();
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragEnter(HWND hwndLock,int x,int y)
HB_FUNC(WAIMAGELIST_DRAGENTER)
{
  wa_ret_BOOL(ImageList_DragEnter(wa_par_HWND(1), wa_par_int(2), wa_par_int(3)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragLeave(HWND hwndLock)
HB_FUNC(WAIMAGELIST_DRAGLEAVE)
{
  wa_ret_BOOL(ImageList_DragLeave(wa_par_HWND(1)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragMove(int x,int y)
HB_FUNC(WAIMAGELIST_DRAGMOVE)
{
  wa_ret_BOOL(ImageList_DragMove(wa_par_int(1), wa_par_int(2)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetDragCursorImage(HIMAGELIST himlDrag,int iDrag,int dxHotspot,int dyHotspot)
HB_FUNC(WAIMAGELIST_SETDRAGCURSORIMAGE)
{
  wa_ret_BOOL(ImageList_SetDragCursorImage(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_int(3), wa_par_int(4)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragShowNolock(WINBOOL fShow)
HB_FUNC(WAIMAGELIST_DRAGSHOWNOLOCK)
{
  wa_ret_BOOL(ImageList_DragShowNolock(hb_parl(1)));
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_GetDragImage(POINT *ppt,POINT *pptHotspot)
HB_FUNC(WAIMAGELIST_GETDRAGIMAGE)
{
  wa_ret_HIMAGELIST(ImageList_GetDragImage(wa_par_POINT(1), wa_par_POINT(2)));
}

// WINBOOL ImageList_RemoveAll(HIMAGELIST himl)
HB_FUNC(WAIMAGELIST_REMOVEALL)
{
  wa_ret_BOOL(ImageList_RemoveAll(wa_par_HIMAGELIST(1)));
}

// HICON ImageList_ExtractIcon(HINSTANCE hi,HIMAGELIST himl,int i)
HB_FUNC(WAIMAGELIST_EXTRACTICON)
{
  wa_ret_HICON(ImageList_ExtractIcon(wa_par_HINSTANCE(1), wa_par_HIMAGELIST(2), wa_par_int(3)));
}

// HIMAGELIST ImageList_LoadBitmap(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask)
HB_FUNC(WAIMAGELIST_LOADBITMAP)
{
  void *str2{};
  wa_ret_HIMAGELIST(ImageList_LoadBitmap(wa_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr), wa_par_int(3),
                                         wa_par_int(4), wa_par_COLORREF(5)));
  hb_strfree(str2);
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Read(LPSTREAM pstm)

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Write(HIMAGELIST himl,LPSTREAM pstm)

// WINCOMMCTRLAPI HRESULT WINAPI ImageList_ReadEx(DWORD dwFlags,LPSTREAM pstm,REFIID riid,PVOID *ppv)

// WINCOMMCTRLAPI HRESULT WINAPI ImageList_WriteEx(HIMAGELIST himl,DWORD dwFlags,LPSTREAM pstm)

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_GetIconSize(HIMAGELIST himl,int *cx,int *cy)
HB_FUNC(WAIMAGELIST_GETICONSIZE)
{
  int cx{};
  int cy{};
  wa_ret_BOOL(ImageList_GetIconSize(wa_par_HIMAGELIST(1), &cx, &cy));
  wa_stor_int(cx, 2);
  wa_stor_int(cy, 3);
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetIconSize(HIMAGELIST himl,int cx,int cy)
HB_FUNC(WAIMAGELIST_SETICONSIZE)
{
  wa_ret_BOOL(ImageList_SetIconSize(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_int(3)));
}

// WINCOMMCTRLAPI WINBOOL WINAPI ImageList_GetImageInfo(HIMAGELIST himl,int i,IMAGEINFO *pImageInfo)
HB_FUNC(WAIMAGELIST_GETIMAGEINFO)
{
  wa_ret_BOOL(ImageList_GetImageInfo(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_IMAGEINFO(3)));
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Merge(HIMAGELIST himl1,int i1,HIMAGELIST himl2,int i2,int dx,int dy)
HB_FUNC(WAIMAGELIST_MERGE)
{
  wa_ret_HIMAGELIST(ImageList_Merge(wa_par_HIMAGELIST(1), wa_par_int(2), wa_par_HIMAGELIST(3), wa_par_int(4),
                                    wa_par_int(5), wa_par_int(6)));
}

// WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Duplicate(HIMAGELIST himl)
HB_FUNC(WAIMAGELIST_DUPLICATE)
{
  wa_ret_HIMAGELIST(ImageList_Duplicate(wa_par_HIMAGELIST(1)));
}

// WINCOMMCTRLAPI HRESULT WINAPI HIMAGELIST_QueryInterface(HIMAGELIST himl, REFIID riid, void **ppv)

// WINCOMMCTRLAPI HWND WINAPI CreateToolbarEx(HWND hwnd,DWORD ws,UINT wID,int nBitmaps,HINSTANCE hBMInst,UINT_PTR
// wBMID,LPCTBBUTTON lpButtons,int iNumButtons,int dxButton,int dyButton,int dxBitmap,int dyBitmap,UINT uStructSize)

// WINCOMMCTRLAPI HBITMAP WINAPI CreateMappedBitmap(HINSTANCE hInstance,INT_PTR idBitmap,UINT wFlags,LPCOLORMAP
// lpColorMap,int iNumMaps)
HB_FUNC(WACREATEMAPPEDBITMAP)
{
  wa_ret_HBITMAP(
      CreateMappedBitmap(wa_par_HINSTANCE(1), wa_par_INT_PTR(2), wa_par_UINT(3), wa_par_COLORMAP(3), wa_par_int(4)));
}

// WINCOMMCTRLAPI void WINAPI DrawStatusTextA(HDC hDC,LPCRECT lprc,LPCSTR pszText,UINT uFlags)
#if 0
HB_FUNC(WADRAWSTATUSTEXTA)
{
  DrawStatusTextA(wa_par_HDC(1), wa_par_RECT(2), wa_par_LPCSTR(3), wa_par_UINT(4));
}
#endif

// WINCOMMCTRLAPI void WINAPI DrawStatusTextW(HDC hDC,LPCRECT lprc,LPCWSTR pszText,UINT uFlags)
#if 0
HB_FUNC(WADRAWSTATUSTEXTW)
{
  DrawStatusTextW(wa_par_HDC(1), wa_par_RECT(2), wa_par_LPCWSTR(3), wa_par_UINT(4));
}
#endif

HB_FUNC(WADRAWSTATUSTEXT)
{
  void *str{};
  DrawStatusText(wa_par_HDC(1), wa_par_RECT(2), HB_PARSTR(3, &str, nullptr), wa_par_UINT(4));
  hb_strfree(str);
}

// WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowA(LONG style,LPCSTR lpszText,HWND hwndParent,UINT wID)
#if 0
HB_FUNC(WACREATESTATUSWINDOWA)
{
  wa_ret_HWND(CreateStatusWindowA(hb_parnl(1), wa_par_LPCSTR(2), wa_par_HWND(3), wa_par_UINT(4)));
}
#endif

// WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowW(LONG style,LPCWSTR lpszText,HWND hwndParent,UINT wID)
#if 0
HB_FUNC(WACREATESTATUSWINDOWW)
{
  wa_ret_HWND(CreateStatusWindowW(hb_parnl(1), wa_par_LPCWSTR(2), wa_par_HWND(3), wa_par_UINT(4)));
}
#endif

HB_FUNC(WACREATESTATUSWINDOW)
{
  void *str{};
  wa_ret_HWND(CreateStatusWindow(hb_parnl(1), HB_PARSTR(2, &str, nullptr), wa_par_HWND(3), wa_par_UINT(4)));
  hb_strfree(str);
}

// WINCOMMCTRLAPI void WINAPI MenuHelp(UINT uMsg,WPARAM wParam,LPARAM lParam,HMENU hMainMenu,HINSTANCE hInst,HWND
// hwndStatus,UINT *lpwIDs)
HB_FUNC(WAMENUHELP)
{
  std::vector<UINT> vec{};
  auto pArray = hb_param(7, Harbour::Item::ARRAY);
  if (pArray != nullptr) {
    const std::size_t nLen = hb_arrayLen(pArray);
    for (std::size_t i = 0; i < nLen; i++) {
      vec.push_back(static_cast<UINT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  MenuHelp(wa_par_UINT(1), wa_par_WPARAM(2), wa_par_LPARAM(3), wa_par_HMENU(4), wa_par_HINSTANCE(5), wa_par_HWND(6),
           vec.data());
}

// WINCOMMCTRLAPI WINBOOL WINAPI ShowHideMenuCtl(HWND hWnd,UINT_PTR uFlags,LPINT lpInfo)
HB_FUNC(WASHOWHIDEMENUCTL)
{
  std::vector<INT> vec{};
  auto pArray = hb_param(3, Harbour::Item::ARRAY);
  if (pArray != nullptr) {
    const std::size_t nLen = hb_arrayLen(pArray);
    for (std::size_t i = 0; i < nLen; i++) {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  wa_ret_BOOL(ShowHideMenuCtl(wa_par_HWND(1), wa_par_UINT_PTR(2), vec.data()));
}

// WINCOMMCTRLAPI void WINAPI GetEffectiveClientRect(HWND hWnd,LPRECT lprc,const INT *lpInfo)
HB_FUNC(WAGETEFFECTIVECLIENTRECT)
{
  std::vector<INT> vec{};
  auto pArray = hb_param(3, Harbour::Item::ARRAY);
  if (pArray != nullptr) {
    const std::size_t nLen = hb_arrayLen(pArray);
    for (std::size_t i = 0; i < nLen; i++) {
      vec.push_back(static_cast<INT>(hb_arrayGetNI(pArray, i + 1)));
    }
  }
  GetEffectiveClientRect(wa_par_HWND(1), wa_par_RECT(2), vec.data());
}

// WINCOMMCTRLAPI WINBOOL WINAPI MakeDragList(HWND hLB)
HB_FUNC(WAMAKEDRAGLIST)
{
  wa_ret_BOOL(MakeDragList(wa_par_HWND(1)));
}

// WINCOMMCTRLAPI void WINAPI DrawInsert(HWND handParent,HWND hLB,int nItem)
HB_FUNC(WADRAWINSERT)
{
  DrawInsert(wa_par_HWND(1), wa_par_HWND(2), wa_par_int(3));
}

// WINCOMMCTRLAPI int WINAPI LBItemFromPt(HWND hLB,POINT pt,WINBOOL bAutoScroll)
HB_FUNC(WALBITEMFROMPT)
{
  wa_ret_int(LBItemFromPt(wa_par_HWND(1), *wa_par_POINT(2), wa_par_BOOL(3)));
}

// WINCOMMCTRLAPI HWND WINAPI CreateUpDownControl(DWORD dwStyle,int x,int y,int cx,int cy,HWND hParent,int nID,HINSTANCE
// hInst,HWND hBuddy,int nUpper,int nLower,int nPos)
HB_FUNC(WACREATEUPDOWNCONTROL)
{
  wa_ret_HWND(CreateUpDownControl(wa_par_DWORD(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), wa_par_int(5),
                                  wa_par_HWND(6), wa_par_int(7), wa_par_HINSTANCE(8), wa_par_HWND(9), wa_par_int(10),
                                  wa_par_int(11), wa_par_int(12)));
}

// WINCOMMCTRLAPI HRESULT WINAPI TaskDialogIndirect(const TASKDIALOGCONFIG *pTaskConfig, int *pnButton, int
// *pnRadioButton, WINBOOL *pfVerificationFlagChecked)

// WINCOMMCTRLAPI HRESULT WINAPI TaskDialog(HWND hwndOwner, HINSTANCE hInstance, PCWSTR pszWindowTitle, PCWSTR
// pszMainInstruction, PCWSTR pszContent, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons, PCWSTR pszIcon, int *pnButton)
#if 0
HB_FUNC(WATASKDIALOG)
{
  void *str3{};
  void *str4{};
  void *str5{};
  void *str7{};
  int nButton{};
  wa_ret_HRESULT(TaskDialog(wa_par_HWND(1), wa_par_HINSTANCE(2), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), HB_PARSTR(5, &str5, nullptr), static_cast<TASKDIALOG_COMMON_BUTTON_FLAGS>(hb_parni(6)), HB_PARSTR(7, &str7, nullptr), &nButton));
  hb_strfree(str3);
  hb_strfree(str4);
  hb_strfree(str5);
  hb_strfree(str7);
  wa_stor_int(nButton, 8);
}
#endif

// WINCOMMCTRLAPI HDSA WINAPI DSA_Create(int cbItem, int cItemGrow)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_Destroy(HDSA hdsa)

// WINCOMMCTRLAPI void WINAPI DSA_DestroyCallback(HDSA hdsa, PFNDAENUMCALLBACK pfnCB, void *pData)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_DeleteItem(HDSA hdsa, int i)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_DeleteAllItems(HDSA hdsa)

// WINCOMMCTRLAPI void WINAPI DSA_EnumCallback(HDSA hdsa, PFNDAENUMCALLBACK pfnCB, void *pData)

// WINCOMMCTRLAPI int WINAPI DSA_InsertItem(HDSA hdsa, int i, const void *pitem)

// WINCOMMCTRLAPI PVOID WINAPI DSA_GetItemPtr(HDSA hdsa, int i)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_GetItem(HDSA hdsa, int i, void *pitem)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_SetItem(HDSA hdsa, int i, const void *pitem)

// WINCOMMCTRLAPI HDSA WINAPI DSA_Clone(HDSA hdsa)

// WINCOMMCTRLAPI ULONGLONG WINAPI DSA_GetSize(HDSA hdsa)

// WINCOMMCTRLAPI WINBOOL WINAPI DSA_Sort(HDSA pdsa, PFNDACOMPARE pfnCompare, LPARAM lParam)

// WINCOMMCTRLAPI HDPA WINAPI DPA_Create(int cItemGrow)

// WINCOMMCTRLAPI HDPA WINAPI DPA_CreateEx(int cpGrow, HANDLE hheap)

// WINCOMMCTRLAPI HDPA WINAPI DPA_Clone(const HDPA hdpa, HDPA hdpaNew)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_Destroy(HDPA hdpa)

// WINCOMMCTRLAPI void WINAPI DPA_DestroyCallback(HDPA hdpa, PFNDAENUMCALLBACK pfnCB, void *pData)

// WINCOMMCTRLAPI PVOID WINAPI DPA_DeletePtr(HDPA hdpa, int i)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_DeleteAllPtrs(HDPA hdpa)

// WINCOMMCTRLAPI void WINAPI DPA_EnumCallback(HDPA hdpa, PFNDAENUMCALLBACK pfnCB, void *pData)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_Grow(HDPA pdpa, int cp)

// WINCOMMCTRLAPI int WINAPI DPA_InsertPtr(HDPA hdpa, int i, void *p)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_SetPtr(HDPA hdpa, int i, void *p)

// WINCOMMCTRLAPI PVOID WINAPI DPA_GetPtr(HDPA hdpa, INT_PTR i)

// WINCOMMCTRLAPI int WINAPI DPA_GetPtrIndex(HDPA hdpa, const void *p)

// WINCOMMCTRLAPI ULONGLONG WINAPI DPA_GetSize(HDPA hdpa)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_Sort(HDPA hdpa, PFNDACOMPARE pfnCompare, LPARAM lParam)

// WINCOMMCTRLAPI HRESULT WINAPI DPA_LoadStream(HDPA *phdpa, PFNDPASTREAM pfn, struct IStream *pstream, void
// *pvInstData)

// WINCOMMCTRLAPI HRESULT WINAPI DPA_SaveStream(HDPA hdpa, PFNDPASTREAM pfn, struct IStream *pstream, void *pvInstData)

// WINCOMMCTRLAPI WINBOOL WINAPI DPA_Merge(HDPA hdpaDest, HDPA hdpaSrc, DWORD dwFlags, PFNDACOMPARE pfnCompare,
// PFNDPAMERGE pfnMerge, LPARAM lParam)

// WINCOMMCTRLAPI int WINAPI DPA_Search(HDPA hdpa, void *pFind, int iStart, PFNDACOMPARE pfnCompare, LPARAM lParam, UINT
// options)

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_EnableScrollBar(HWND,int,UINT)
#if 0
HB_FUNC(WAFLATSB_ENABLESCROLLBAR)
{
  wa_ret_BOOL(FlatSB_EnableScrollBar(wa_par_HWND(1), wa_par_int(2), wa_par_UINT(3)));
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_ShowScrollBar(HWND,int code,WINBOOL)
#if 0
HB_FUNC(WAFLATSB_SHOWSCROLLBAR)
{
  wa_ret_BOOL(FlatSB_ShowScrollBar(wa_par_HWND(1), wa_par_int(2), hb_parl(3)));
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollRange(HWND,int code,LPINT,LPINT)
#if 0
HB_FUNC(WAFLATSB_GETSCROLLRANGE)
{
  INT i1{};
  INT i2{};
  wa_ret_BOOL(FlatSB_GetScrollRange(wa_par_HWND(1), wa_par_int(2), &i1, &i2));
  wa_stor_INT(i1, 3);
  wa_stor_INT(i2, 4);
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollInfo(HWND,int code,LPSCROLLINFO)
#if 0
HB_FUNC(WAFLATSB_GETSCROLLINFO)
{
  wa_ret_BOOL(FlatSB_GetScrollInfo(wa_par_HWND(1), wa_par_int(2), wa_par_SCROLLINFO(3)));
}
#endif

// WINCOMMCTRLAPI int WINAPI FlatSB_GetScrollPos(HWND,int code)
#if 0
HB_FUNC(WAFLATSB_GETSCROLLPOS)
{
  wa_ret_int(FlatSB_GetScrollPos(wa_par_HWND(1), wa_par_int(2)));
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollProp(HWND,int propIndex,LPINT)
#if 0
HB_FUNC(WAFLATSB_SCROLLPROP)
{
  INT i{};
  wa_ret_BOOL(FlatSB_GetScrollProp(wa_par_HWND(1), wa_par_int(2), &i));
  wa_stor_INT(i, 3);
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollPropPtr(HWND,int propIndex,PINT_PTR)

// WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollPos(HWND,int code,int pos,WINBOOL fRedraw)
#if 0
HB_FUNC(WAFLATSB_SETSCROLLPOS)
{
  wa_ret_int(FlatSB_SetScrollPos(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), hb_parl(4)));
}
#endif

// WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollInfo(HWND,int code,LPSCROLLINFO,WINBOOL fRedraw)
#if 0
HB_FUNC(WAFLATSB_SETSCROLLINFO)
{
  wa_ret_int(FlatSB_SetScrollInfo(wa_par_HWND(1), wa_par_int(2), wa_par_SCROLLINFO(3), wa_par_BOOL(4)));
}
#endif

// WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollRange(HWND,int code,int min,int max,WINBOOL fRedraw)
#if 0
HB_FUNC(WAFLATSB_SETSCROLLRANGE)
{
  wa_ret_int(FlatSB_SetScrollRange(wa_par_HWND(1), wa_par_int(2), wa_par_int(3), wa_par_int(4), hb_parl(5)));
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_SetScrollProp(HWND,UINT index,INT_PTR newValue,WINBOOL)
#if 0
HB_FUNC(WAFLATSB_SETSCROLLPROP)
{
  wa_ret_BOOL(FlatSB_SetScrollProp(wa_par_HWND(1), wa_par_UINT(2), wa_par_INT_PTR(3), hb_parl(4)));
}
#endif

// WINCOMMCTRLAPI WINBOOL WINAPI InitializeFlatSB(HWND)
#if 0
HB_FUNC(WAINITIALIZEFLATSB)
{
  wa_ret_BOOL(InitializeFlatSB(wa_par_HWND(1)));
}
#endif

// WINCOMMCTRLAPI HRESULT WINAPI UninitializeFlatSB(HWND)
#if 0
HB_FUNC(WAUNINITIALIZEFLATSB)
{
  wa_ret_HRESULT(UninitializeFlatSB(wa_par_HWND(1)));
}
#endif

// WINCOMMCTRLAPI HRESULT WINAPI LoadIconMetric(HINSTANCE hinst, PCWSTR pszName, int lims, HICON *phico)
#if 0
HB_FUNC(WALOADICONMETRIC)
{
  HICON *phico{};
  wa_ret_HRESULT(LoadIconMetric(wa_par_HINSTANCE(1), wa_par_PCWSTR(2), wa_par_int(3), &phico));
  hb_storptr(phico, 4);
}
#endif

// WINCOMMCTRLAPI HRESULT WINAPI LoadIconWithScaleDown(HINSTANCE hinst, PCWSTR pszName, int cx, int cy, HICON *phico)
#if 0
HB_FUNC(WALOADICONWITHSCALEDOWN)
{
  HICON *phico{};
  wa_ret_HRESULT(LoadIconWithScaleDown(wa_par_HINSTANCE(1), wa_par_PCWSTR(2), wa_par_int(3), wa_par_int(4), &phico));
  hb_storptr(phico, 4);
}
#endif
