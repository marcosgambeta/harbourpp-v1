/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include <commctrl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINCOMMCTRLAPI void WINAPI InitCommonControls(void)
*/
HB_FUNC( WAINITCOMMONCONTROLS )
{
  InitCommonControls();
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI InitCommonControlsEx(const INITCOMMONCONTROLSEX *)
*/
HB_FUNC( WAINITCOMMONCONTROLSEX )
{
  winapi_ret_BOOL(InitCommonControlsEx(static_cast<const INITCOMMONCONTROLSEX*>(winapi_get_ptr(1))));
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Create(int cx,int cy,UINT flags,int cInitial,int cGrow)
*/
HB_FUNC( WAIMAGELIST_CREATE )
{
  winapi_ret_HIMAGELIST(ImageList_Create(winapi_par_int(1), winapi_par_int(2), winapi_par_UINT(3), winapi_par_int(4), winapi_par_int(5)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Destroy(HIMAGELIST himl)
*/
HB_FUNC( WAIMAGELIST_DESTROY )
{
  winapi_ret_BOOL(ImageList_Destroy(winapi_par_HIMAGELIST(1)));
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_GetImageCount(HIMAGELIST himl)
*/
HB_FUNC( WAIMAGELIST_GETIMAGECOUNT )
{
  winapi_ret_int(ImageList_GetImageCount(winapi_par_HIMAGELIST(1)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetImageCount(HIMAGELIST himl,UINT uNewCount)
*/
HB_FUNC( WAIMAGELIST_SETIMAGECOUNT )
{
  winapi_ret_BOOL(ImageList_SetImageCount(winapi_par_HIMAGELIST(1), winapi_par_UINT(2)));
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_Add(HIMAGELIST himl,HBITMAP hbmImage,HBITMAP hbmMask)
*/
HB_FUNC( WAIMAGELIST_ADD )
{
  winapi_ret_int(ImageList_Add(winapi_par_HIMAGELIST(1), winapi_par_HBITMAP(2), winapi_par_HBITMAP(3)));
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_ReplaceIcon(HIMAGELIST himl,int i,HICON hicon)
*/
HB_FUNC( WAIMAGELIST_REPLACEICON )
{
  winapi_ret_int(ImageList_ReplaceIcon(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HICON(3)));
}

/*
WINCOMMCTRLAPI COLORREF WINAPI ImageList_SetBkColor(HIMAGELIST himl,COLORREF clrBk)
*/
HB_FUNC( WAIMAGELIST_SETBKCOLOR )
{
  winapi_ret_COLORREF(ImageList_SetBkColor(winapi_par_HIMAGELIST(1), winapi_par_COLORREF(2)));
}

/*
WINCOMMCTRLAPI COLORREF WINAPI ImageList_GetBkColor(HIMAGELIST himl)
*/
HB_FUNC( WAIMAGELIST_GETBKCOLOR )
{
  winapi_ret_COLORREF(ImageList_GetBkColor(winapi_par_HIMAGELIST(1)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetOverlayImage(HIMAGELIST himl,int iImage,int iOverlay)
*/
HB_FUNC( WAIMAGELIST_SETOVERLAYIMAGE )
{
  winapi_ret_BOOL(ImageList_SetOverlayImage(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
int ImageList_AddIcon(HIMAGELIST himl,HICON  hicon)
*/
HB_FUNC( WAIMAGELIST_ADDICON )
{
  winapi_ret_int(ImageList_AddIcon(winapi_par_HIMAGELIST(1), winapi_par_HICON(2)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Draw(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,UINT fStyle)
*/
HB_FUNC( WAIMAGELIST_DRAW )
{
  winapi_ret_BOOL(ImageList_Draw(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HDC(3), winapi_par_int(4), winapi_par_int(5), winapi_par_UINT(6)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Replace(HIMAGELIST himl,int i,HBITMAP hbmImage,HBITMAP hbmMask)
*/
HB_FUNC( WAIMAGELIST_REPLACE )
{
  winapi_ret_BOOL(ImageList_Replace(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HBITMAP(3), winapi_par_HBITMAP(4)));
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_AddMasked(HIMAGELIST himl,HBITMAP hbmImage,COLORREF crMask)
*/
HB_FUNC( WAIMAGELIST_ADDMASKED )
{
  winapi_ret_int(ImageList_AddMasked(winapi_par_HIMAGELIST(1), winapi_par_HBITMAP(2), winapi_par_COLORREF(3)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawEx(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,int dx,int dy,COLORREF rgbBk,COLORREF rgbFg,UINT fStyle)
*/
HB_FUNC( WAIMAGELIST_DRAWEX )
{
  winapi_ret_BOOL(ImageList_DrawEx(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HDC(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6), winapi_par_int(7), winapi_par_COLORREF(8), winapi_par_COLORREF(9), winapi_par_UINT(10)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawIndirect(IMAGELISTDRAWPARAMS *pimldp)
*/
HB_FUNC( WAIMAGELIST_DRAWINDIRECT )
{
  winapi_ret_BOOL(ImageList_DrawIndirect(static_cast<IMAGELISTDRAWPARAMS*>(winapi_get_ptr(1))));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Remove(HIMAGELIST himl,int i)
*/
HB_FUNC( WAIMAGELIST_REMOVE )
{
  winapi_ret_BOOL(ImageList_Remove(winapi_par_HIMAGELIST(1), winapi_par_int(2)));
}

/*
WINCOMMCTRLAPI HICON WINAPI ImageList_GetIcon(HIMAGELIST himl,int i,UINT flags)
*/
HB_FUNC( WAIMAGELIST_GETICON )
{
  winapi_ret_HICON(ImageList_GetIcon(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_UINT(3)));
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageA(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask,UINT uType,UINT uFlags)
*/
HB_FUNC( WAIMAGELIST_LOADIMAGEA )
{
  winapi_ret_HIMAGELIST(ImageList_LoadImageA(winapi_par_HINSTANCE(1), ( LPCSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_COLORREF(5), winapi_par_UINT(6), winapi_par_UINT(7)));
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageW(HINSTANCE hi,LPCWSTR lpbmp,int cx,int cGrow,COLORREF crMask,UINT uType,UINT uFlags)
*/
HB_FUNC( WAIMAGELIST_LOADIMAGEW )
{
  winapi_ret_HIMAGELIST(ImageList_LoadImageW(winapi_par_HINSTANCE(1), ( LPCWSTR ) hb_parc(2), winapi_par_int(3), winapi_par_int(4), winapi_par_COLORREF(5), winapi_par_UINT(6), winapi_par_UINT(7)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Copy(HIMAGELIST himlDst,int iDst,HIMAGELIST himlSrc,int iSrc,UINT uFlags)
*/
HB_FUNC( WAIMAGELIST_COPY )
{
  winapi_ret_BOOL(ImageList_Copy(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HIMAGELIST(3), winapi_par_int(4), winapi_par_UINT(5)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_BeginDrag(HIMAGELIST himlTrack,int iTrack,int dxHotspot,int dyHotspot)
*/
HB_FUNC( WAIMAGELIST_BEGINDRAG )
{
  winapi_ret_BOOL(ImageList_BeginDrag(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINCOMMCTRLAPI void WINAPI ImageList_EndDrag(void)
*/
HB_FUNC( WAIMAGELIST_ENDDRAG )
{
  ImageList_EndDrag();
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragEnter(HWND hwndLock,int x,int y)
*/
HB_FUNC( WAIMAGELIST_DRAGENTER )
{
  winapi_ret_BOOL(ImageList_DragEnter(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragLeave(HWND hwndLock)
*/
HB_FUNC( WAIMAGELIST_DRAGLEAVE )
{
  winapi_ret_BOOL(ImageList_DragLeave(winapi_par_HWND(1)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragMove(int x,int y)
*/
HB_FUNC( WAIMAGELIST_DRAGMOVE )
{
  winapi_ret_BOOL(ImageList_DragMove(winapi_par_int(1), winapi_par_int(2)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetDragCursorImage(HIMAGELIST himlDrag,int iDrag,int dxHotspot,int dyHotspot)
*/
HB_FUNC( WAIMAGELIST_SETDRAGCURSORIMAGE )
{
  winapi_ret_BOOL(ImageList_SetDragCursorImage(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragShowNolock(WINBOOL fShow)
*/
HB_FUNC( WAIMAGELIST_DRAGSHOWNOLOCK )
{
  winapi_ret_BOOL(ImageList_DragShowNolock(hb_parl(1)));
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_GetDragImage(POINT *ppt,POINT *pptHotspot)
*/
HB_FUNC( WAIMAGELIST_GETDRAGIMAGE )
{
  winapi_ret_HIMAGELIST(ImageList_GetDragImage(static_cast<POINT*>(winapi_get_ptr(1)), static_cast<POINT*>(winapi_get_ptr(2))));
}

/*
WINBOOL ImageList_RemoveAll(HIMAGELIST himl)
*/
HB_FUNC( WAIMAGELIST_REMOVEALL )
{
  winapi_ret_BOOL(ImageList_RemoveAll(winapi_par_HIMAGELIST(1)));
}

/*
HICON ImageList_ExtractIcon(HINSTANCE hi,HIMAGELIST himl,int i)
*/
HB_FUNC( WAIMAGELIST_EXTRACTICON )
{
  winapi_ret_HICON(ImageList_ExtractIcon(winapi_par_HINSTANCE(1), winapi_par_HIMAGELIST(2), winapi_par_int(3)));
}

/*
HIMAGELIST ImageList_LoadBitmap(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask)
*/
HB_FUNC( WAIMAGELIST_LOADBITMAP )
{
  void * str2;
  winapi_ret_HIMAGELIST(ImageList_LoadBitmap(winapi_par_HINSTANCE(1), HB_PARSTR(2, &str2, nullptr), winapi_par_int(3), winapi_par_int(4), winapi_par_COLORREF(5)));
  hb_strfree(str2);
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Read(LPSTREAM pstm)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Write(HIMAGELIST himl,LPSTREAM pstm)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI ImageList_ReadEx(DWORD dwFlags,LPSTREAM pstm,REFIID riid,PVOID *ppv)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI ImageList_WriteEx(HIMAGELIST himl,DWORD dwFlags,LPSTREAM pstm)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_GetIconSize(HIMAGELIST himl,int *cx,int *cy)
*/
HB_FUNC( WAIMAGELIST_GETICONSIZE )
{
  int cx;
  int cy;
  winapi_ret_BOOL(ImageList_GetIconSize(winapi_par_HIMAGELIST(1), &cx, &cy));
  winapi_stor_int(cx, 2);
  winapi_stor_int(cy, 3);
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetIconSize(HIMAGELIST himl,int cx,int cy)
*/
HB_FUNC( WAIMAGELIST_SETICONSIZE )
{
  winapi_ret_BOOL(ImageList_SetIconSize(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_int(3)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_GetImageInfo(HIMAGELIST himl,int i,IMAGEINFO *pImageInfo)
*/

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Merge(HIMAGELIST himl1,int i1,HIMAGELIST himl2,int i2,int dx,int dy)
*/
HB_FUNC( WAIMAGELIST_MERGE )
{
  winapi_ret_HIMAGELIST(ImageList_Merge(winapi_par_HIMAGELIST(1), winapi_par_int(2), winapi_par_HIMAGELIST(3), winapi_par_int(4), winapi_par_int(5), winapi_par_int(6)));
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Duplicate(HIMAGELIST himl)
*/
HB_FUNC( WAIMAGELIST_DUPLICATE )
{
  winapi_ret_HIMAGELIST(ImageList_Duplicate(winapi_par_HIMAGELIST(1)));
}

/*
WINCOMMCTRLAPI HRESULT WINAPI HIMAGELIST_QueryInterface (HIMAGELIST himl, REFIID riid, void **ppv)
*/

/*
WINCOMMCTRLAPI HWND WINAPI CreateToolbarEx(HWND hwnd,DWORD ws,UINT wID,int nBitmaps,HINSTANCE hBMInst,UINT_PTR wBMID,LPCTBBUTTON lpButtons,int iNumButtons,int dxButton,int dyButton,int dxBitmap,int dyBitmap,UINT uStructSize)
*/

/*
WINCOMMCTRLAPI HBITMAP WINAPI CreateMappedBitmap(HINSTANCE hInstance,INT_PTR idBitmap,UINT wFlags,LPCOLORMAP lpColorMap,int iNumMaps)
*/

/*
WINCOMMCTRLAPI void WINAPI DrawStatusTextA(HDC hDC,LPCRECT lprc,LPCSTR pszText,UINT uFlags)
*/
HB_FUNC( WADRAWSTATUSTEXTA )
{
  DrawStatusTextA(winapi_par_HDC(1), static_cast<LPCRECT>(winapi_get_ptr(2)), ( LPCSTR ) hb_parc(3), winapi_par_UINT(4));
}

/*
WINCOMMCTRLAPI void WINAPI DrawStatusTextW(HDC hDC,LPCRECT lprc,LPCWSTR pszText,UINT uFlags)
*/
HB_FUNC( WADRAWSTATUSTEXTW )
{
  DrawStatusTextW(winapi_par_HDC(1), static_cast<LPCRECT>(winapi_get_ptr(2)), ( LPCWSTR ) hb_parc(3), winapi_par_UINT(4));
}

/*
WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowA(LONG style,LPCSTR lpszText,HWND hwndParent,UINT wID)
*/
HB_FUNC( WACREATESTATUSWINDOWA )
{
  winapi_ret_HWND(CreateStatusWindowA(hb_parnl(1), ( LPCSTR ) hb_parc(2), winapi_par_HWND(3), winapi_par_UINT(4)));
}

/*
WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowW(LONG style,LPCWSTR lpszText,HWND hwndParent,UINT wID)
*/
HB_FUNC( WACREATESTATUSWINDOWW )
{
  winapi_ret_HWND(CreateStatusWindowW(hb_parnl(1), ( LPCWSTR ) hb_parc(2), winapi_par_HWND(3), winapi_par_UINT(4)));
}

/*
WINCOMMCTRLAPI void WINAPI MenuHelp(UINT uMsg,WPARAM wParam,LPARAM lParam,HMENU hMainMenu,HINSTANCE hInst,HWND hwndStatus,UINT *lpwIDs)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI ShowHideMenuCtl(HWND hWnd,UINT_PTR uFlags,LPINT lpInfo)
*/

/*
WINCOMMCTRLAPI void WINAPI GetEffectiveClientRect(HWND hWnd,LPRECT lprc,const INT *lpInfo)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI MakeDragList(HWND hLB)
*/
HB_FUNC( WAMAKEDRAGLIST )
{
  winapi_ret_BOOL(MakeDragList(winapi_par_HWND(1)));
}

/*
WINCOMMCTRLAPI void WINAPI DrawInsert(HWND handParent,HWND hLB,int nItem)
*/
HB_FUNC( WADRAWINSERT )
{
  DrawInsert(winapi_par_HWND(1), winapi_par_HWND(2), winapi_par_int(3));
}

/*
WINCOMMCTRLAPI int WINAPI LBItemFromPt(HWND hLB,POINT pt,WINBOOL bAutoScroll)
*/
#if 0
HB_FUNC( WALBITEMFROMPT )
{
  winapi_ret_int(LBItemFromPt(winapi_par_HWND(1), static_cast<POINT>(winapi_get_ptr(2)), hb_parl(3))); // TODO: fix
}
#endif

/*
WINCOMMCTRLAPI HWND WINAPI CreateUpDownControl(DWORD dwStyle,int x,int y,int cx,int cy,HWND hParent,int nID,HINSTANCE hInst,HWND hBuddy,int nUpper,int nLower,int nPos)
*/
HB_FUNC( WACREATEUPDOWNCONTROL )
{
  winapi_ret_HWND(CreateUpDownControl(winapi_par_DWORD(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), winapi_par_int(5), winapi_par_HWND(6), winapi_par_int(7), winapi_par_HINSTANCE(8), winapi_par_HWND(9), winapi_par_int(10), winapi_par_int(11), winapi_par_int(12)));
}

/*
WINCOMMCTRLAPI HRESULT WINAPI TaskDialogIndirect (const TASKDIALOGCONFIG *pTaskConfig, int *pnButton, int *pnRadioButton, WINBOOL *pfVerificationFlagChecked)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI TaskDialog (HWND hwndOwner, HINSTANCE hInstance, PCWSTR pszWindowTitle, PCWSTR pszMainInstruction, PCWSTR pszContent, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons, PCWSTR pszIcon, int *pnButton)
*/

/*
WINCOMMCTRLAPI HDSA WINAPI DSA_Create (int cbItem, int cItemGrow)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_Destroy (HDSA hdsa)
*/

/*
WINCOMMCTRLAPI void WINAPI DSA_DestroyCallback (HDSA hdsa, PFNDAENUMCALLBACK pfnCB, void *pData)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_DeleteItem (HDSA hdsa, int i)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_DeleteAllItems (HDSA hdsa)
*/

/*
WINCOMMCTRLAPI void WINAPI DSA_EnumCallback (HDSA hdsa, PFNDAENUMCALLBACK pfnCB, void *pData)
*/

/*
WINCOMMCTRLAPI int WINAPI DSA_InsertItem (HDSA hdsa, int i, const void *pitem)
*/

/*
WINCOMMCTRLAPI PVOID WINAPI DSA_GetItemPtr (HDSA hdsa, int i)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_GetItem (HDSA hdsa, int i, void *pitem)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_SetItem (HDSA hdsa, int i, const void *pitem)
*/

/*
WINCOMMCTRLAPI HDSA WINAPI DSA_Clone (HDSA hdsa)
*/

/*
WINCOMMCTRLAPI ULONGLONG WINAPI DSA_GetSize (HDSA hdsa)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DSA_Sort (HDSA pdsa, PFNDACOMPARE pfnCompare, LPARAM lParam)
*/

/*
WINCOMMCTRLAPI HDPA WINAPI DPA_Create (int cItemGrow)
*/

/*
WINCOMMCTRLAPI HDPA WINAPI DPA_CreateEx (int cpGrow, HANDLE hheap)
*/

/*
WINCOMMCTRLAPI HDPA WINAPI DPA_Clone (const HDPA hdpa, HDPA hdpaNew)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_Destroy (HDPA hdpa)
*/

/*
WINCOMMCTRLAPI void WINAPI DPA_DestroyCallback (HDPA hdpa, PFNDAENUMCALLBACK pfnCB, void *pData)
*/

/*
WINCOMMCTRLAPI PVOID WINAPI DPA_DeletePtr (HDPA hdpa, int i)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_DeleteAllPtrs (HDPA hdpa)
*/

/*
WINCOMMCTRLAPI void WINAPI DPA_EnumCallback (HDPA hdpa, PFNDAENUMCALLBACK pfnCB, void *pData)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_Grow (HDPA pdpa, int cp)
*/

/*
WINCOMMCTRLAPI int WINAPI DPA_InsertPtr (HDPA hdpa, int i, void *p)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_SetPtr (HDPA hdpa, int i, void *p)
*/

/*
WINCOMMCTRLAPI PVOID WINAPI DPA_GetPtr (HDPA hdpa, INT_PTR i)
*/

/*
WINCOMMCTRLAPI int WINAPI DPA_GetPtrIndex (HDPA hdpa, const void *p)
*/

/*
WINCOMMCTRLAPI ULONGLONG WINAPI DPA_GetSize (HDPA hdpa)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_Sort (HDPA hdpa, PFNDACOMPARE pfnCompare, LPARAM lParam)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI DPA_LoadStream (HDPA *phdpa, PFNDPASTREAM pfn, struct IStream *pstream, void *pvInstData)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI DPA_SaveStream (HDPA hdpa, PFNDPASTREAM pfn, struct IStream *pstream, void *pvInstData)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI DPA_Merge (HDPA hdpaDest, HDPA hdpaSrc, DWORD dwFlags, PFNDACOMPARE pfnCompare, PFNDPAMERGE pfnMerge, LPARAM lParam)
*/

/*
WINCOMMCTRLAPI int WINAPI DPA_Search (HDPA hdpa, void *pFind, int iStart, PFNDACOMPARE pfnCompare, LPARAM lParam, UINT options)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_EnableScrollBar(HWND,int,UINT)
*/
HB_FUNC( WAFLATSB_ENABLESCROLLBAR )
{
  winapi_ret_BOOL(FlatSB_EnableScrollBar(winapi_par_HWND(1), winapi_par_int(2), winapi_par_UINT(3)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_ShowScrollBar(HWND,int code,WINBOOL)
*/
HB_FUNC( WAFLATSB_SHOWSCROLLBAR )
{
  winapi_ret_BOOL(FlatSB_ShowScrollBar(winapi_par_HWND(1), winapi_par_int(2), hb_parl(3)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollRange(HWND,int code,LPINT,LPINT)
*/
HB_FUNC( WAFLATSB_GETSCROLLRANGE )
{
  INT i1;
  INT i2;
  winapi_ret_BOOL(FlatSB_GetScrollRange(winapi_par_HWND(1), winapi_par_int(2), &i1, &i2));
  winapi_stor_INT(i1, 3);
  winapi_stor_INT(i2, 4);
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollInfo(HWND,int code,LPSCROLLINFO)
*/
HB_FUNC( WAFLATSB_GETSCROLLINFO )
{
  winapi_ret_BOOL(FlatSB_GetScrollInfo(winapi_par_HWND(1), winapi_par_int(2), static_cast<LPSCROLLINFO>(winapi_get_ptr(3))));
}

/*
WINCOMMCTRLAPI int WINAPI FlatSB_GetScrollPos(HWND,int code)
*/
HB_FUNC( WAFLATSB_GETSCROLLPOS )
{
  winapi_ret_int(FlatSB_GetScrollPos(winapi_par_HWND(1), winapi_par_int(2)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollProp(HWND,int propIndex,LPINT)
*/
HB_FUNC( WAFLATSB_SCROLLPROP )
{
  INT i;
  winapi_ret_BOOL(FlatSB_GetScrollProp(winapi_par_HWND(1), winapi_par_int(2), &i));
  winapi_stor_INT(i, 3);
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollPropPtr(HWND,int propIndex,PINT_PTR)
*/

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollPos(HWND,int code,int pos,WINBOOL fRedraw)
*/
HB_FUNC( WAFLATSB_SETSCROLLPOS )
{
  winapi_ret_int(FlatSB_SetScrollPos(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), hb_parl(4)));
}

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollInfo(HWND,int code,LPSCROLLINFO,WINBOOL fRedraw)
*/
HB_FUNC( WAFLATSB_SETSCROLLINFO )
{
  winapi_ret_int(FlatSB_SetScrollInfo(winapi_par_HWND(1), winapi_par_int(2), static_cast<LPSCROLLINFO>(winapi_get_ptr(3)), winapi_par_BOOL(4)));
}

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollRange(HWND,int code,int min,int max,WINBOOL fRedraw)
*/
HB_FUNC( WAFLATSB_SETSCROLLRANGE )
{
  winapi_ret_int(FlatSB_SetScrollRange(winapi_par_HWND(1), winapi_par_int(2), winapi_par_int(3), winapi_par_int(4), hb_parl(5)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_SetScrollProp(HWND,UINT index,INT_PTR newValue,WINBOOL)
*/
HB_FUNC( WAFLATSB_SETSCROLLPROP )
{
  winapi_ret_BOOL(FlatSB_SetScrollProp(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_INT_PTR(3), hb_parl(4)));
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI InitializeFlatSB(HWND)
*/
HB_FUNC( WAINITIALIZEFLATSB )
{
  winapi_ret_BOOL(InitializeFlatSB(winapi_par_HWND(1)));
}

/*
WINCOMMCTRLAPI HRESULT WINAPI UninitializeFlatSB(HWND)
*/
HB_FUNC( WAUNINITIALIZEFLATSB )
{
  winapi_ret_HRESULT(UninitializeFlatSB(winapi_par_HWND(1)));
}

/*
WINCOMMCTRLAPI HRESULT WINAPI LoadIconMetric (HINSTANCE hinst, PCWSTR pszName, int lims, HICON *phico)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI LoadIconWithScaleDown (HINSTANCE hinst, PCWSTR pszName, int cx, int cy, HICON *phico)
*/
