/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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
#include "hbapi.h"

/*
WINCOMMCTRLAPI void WINAPI InitCommonControls(void)
*/
HB_FUNC( WINAPI_INITCOMMONCONTROLS )
{
  InitCommonControls();
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI InitCommonControlsEx(const INITCOMMONCONTROLSEX *)
*/

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Create(int cx,int cy,UINT flags,int cInitial,int cGrow)
*/
HB_FUNC( WINAPI_IMAGELIST_CREATE )
{
  hb_retptr( ( HIMAGELIST ) ImageList_Create( ( int ) hb_parni( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Destroy(HIMAGELIST himl)
*/
HB_FUNC( WINAPI_IMAGELIST_DESTROY )
{
  hb_retl( ( WINBOOL ) ImageList_Destroy( ( HIMAGELIST ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_GetImageCount(HIMAGELIST himl)
*/
HB_FUNC( WINAPI_IMAGELIST_GETIMAGECOUNT )
{
  hb_retni( ( int ) ImageList_GetImageCount( ( HIMAGELIST ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetImageCount(HIMAGELIST himl,UINT uNewCount)
*/
HB_FUNC( WINAPI_IMAGELIST_SETIMAGECOUNT )
{
  hb_retl( ( WINBOOL ) ImageList_SetImageCount( ( HIMAGELIST ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_Add(HIMAGELIST himl,HBITMAP hbmImage,HBITMAP hbmMask)
*/
HB_FUNC( WINAPI_IMAGELIST_ADD )
{
  hb_retni( ( int ) ImageList_Add( ( HIMAGELIST ) hb_parptr( 1 ), ( HBITMAP ) hb_parptr( 2 ), ( HBITMAP ) hb_parptr( 3 ) ) );
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_ReplaceIcon(HIMAGELIST himl,int i,HICON hicon)
*/
HB_FUNC( WINAPI_IMAGELIST_REPLACEICON )
{
  hb_retni( ( int ) ImageList_ReplaceIcon( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HICON ) hb_parptr( 3 ) ) );
}

/*
WINCOMMCTRLAPI COLORREF WINAPI ImageList_SetBkColor(HIMAGELIST himl,COLORREF clrBk)
*/
HB_FUNC( WINAPI_IMAGELIST_SETBKCOLOR )
{
  hb_retnl( ( COLORREF ) ImageList_SetBkColor( ( HIMAGELIST ) hb_parptr( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}

/*
WINCOMMCTRLAPI COLORREF WINAPI ImageList_GetBkColor(HIMAGELIST himl)
*/
HB_FUNC( WINAPI_IMAGELIST_GETBKCOLOR )
{
  hb_retnl( ( COLORREF ) ImageList_GetBkColor( ( HIMAGELIST ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetOverlayImage(HIMAGELIST himl,int iImage,int iOverlay)
*/
HB_FUNC( WINAPI_IMAGELIST_SETOVERLAYIMAGE )
{
  hb_retl( ( WINBOOL ) ImageList_SetOverlayImage( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
int ImageList_AddIcon(HIMAGELIST himl,HICON  hicon)
*/
HB_FUNC( WINAPI_IMAGELIST_ADDICON )
{
  hb_retni( ( int ) ImageList_AddIcon( ( HIMAGELIST ) hb_parptr( 1 ), ( HICON ) hb_parptr( 2 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Draw(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,UINT fStyle)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAW )
{
  hb_retl( ( WINBOOL ) ImageList_Draw( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HDC ) hb_parptr( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( UINT ) hb_parni( 6 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Replace(HIMAGELIST himl,int i,HBITMAP hbmImage,HBITMAP hbmMask)
*/
HB_FUNC( WINAPI_IMAGELIST_REPLACE )
{
  hb_retl( ( WINBOOL ) ImageList_Replace( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HBITMAP ) hb_parptr( 3 ), ( HBITMAP ) hb_parptr( 4 ) ) );
}

/*
WINCOMMCTRLAPI int WINAPI ImageList_AddMasked(HIMAGELIST himl,HBITMAP hbmImage,COLORREF crMask)
*/
HB_FUNC( WINAPI_IMAGELIST_ADDMASKED )
{
  hb_retni( ( int ) ImageList_AddMasked( ( HIMAGELIST ) hb_parptr( 1 ), ( HBITMAP ) hb_parptr( 2 ), ( COLORREF ) hb_parnl( 3 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawEx(HIMAGELIST himl,int i,HDC hdcDst,int x,int y,int dx,int dy,COLORREF rgbBk,COLORREF rgbFg,UINT fStyle)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAWEX )
{
  hb_retl( ( WINBOOL ) ImageList_DrawEx( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HDC ) hb_parptr( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ), ( int ) hb_parni( 7 ), ( COLORREF ) hb_parnl( 8 ), ( COLORREF ) hb_parnl( 9 ), ( UINT ) hb_parni( 10 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DrawIndirect(IMAGELISTDRAWPARAMS *pimldp)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Remove(HIMAGELIST himl,int i)
*/
HB_FUNC( WINAPI_IMAGELIST_REMOVE )
{
  hb_retl( ( WINBOOL ) ImageList_Remove( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINCOMMCTRLAPI HICON WINAPI ImageList_GetIcon(HIMAGELIST himl,int i,UINT flags)
*/
HB_FUNC( WINAPI_IMAGELIST_GETICON )
{
  hb_retptr( ( HICON ) ImageList_GetIcon( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageA(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask,UINT uType,UINT uFlags)
*/
HB_FUNC( WINAPI_IMAGELIST_LOADIMAGEA )
{
  hb_retptr( ( HIMAGELIST ) ImageList_LoadImageA( ( HINSTANCE ) hb_parptr( 1 ), ( LPCSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( COLORREF ) hb_parnl( 5 ), ( UINT ) hb_parni( 6 ), ( UINT ) hb_parni( 7 ) ) );
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_LoadImageW(HINSTANCE hi,LPCWSTR lpbmp,int cx,int cGrow,COLORREF crMask,UINT uType,UINT uFlags)
*/
HB_FUNC( WINAPI_IMAGELIST_LOADIMAGEW )
{
  hb_retptr( ( HIMAGELIST ) ImageList_LoadImageW( ( HINSTANCE ) hb_parptr( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( COLORREF ) hb_parnl( 5 ), ( UINT ) hb_parni( 6 ), ( UINT ) hb_parni( 7 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_Copy(HIMAGELIST himlDst,int iDst,HIMAGELIST himlSrc,int iSrc,UINT uFlags)
*/
HB_FUNC( WINAPI_IMAGELIST_COPY )
{
  hb_retl( ( WINBOOL ) ImageList_Copy( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HIMAGELIST ) hb_parptr( 3 ), ( int ) hb_parni( 4 ), ( UINT ) hb_parni( 5 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_BeginDrag(HIMAGELIST himlTrack,int iTrack,int dxHotspot,int dyHotspot)
*/
HB_FUNC( WINAPI_IMAGELIST_BEGINDRAG )
{
  hb_retl( ( WINBOOL ) ImageList_BeginDrag( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINCOMMCTRLAPI void WINAPI ImageList_EndDrag(void)
*/
HB_FUNC( WINAPI_IMAGELIST_ENDDRAG )
{
  ImageList_EndDrag();
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragEnter(HWND hwndLock,int x,int y)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAGENTER )
{
  hb_retl( ( WINBOOL ) ImageList_DragEnter( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragLeave(HWND hwndLock)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAGLEAVE )
{
  hb_retl( ( WINBOOL ) ImageList_DragLeave( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragMove(int x,int y)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAGMOVE )
{
  hb_retl( ( WINBOOL ) ImageList_DragMove( ( int ) hb_parni( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetDragCursorImage(HIMAGELIST himlDrag,int iDrag,int dxHotspot,int dyHotspot)
*/
HB_FUNC( WINAPI_IMAGELIST_SETDRAGCURSORIMAGE )
{
  hb_retl( ( WINBOOL ) ImageList_SetDragCursorImage( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_DragShowNolock(WINBOOL fShow)
*/
HB_FUNC( WINAPI_IMAGELIST_DRAGSHOWNOLOCK )
{
  hb_retl( ( WINBOOL ) ImageList_DragShowNolock( ( WINBOOL ) hb_parl( 1 ) ) );
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_GetDragImage(POINT *ppt,POINT *pptHotspot)
*/

/*
WINBOOL ImageList_RemoveAll(HIMAGELIST himl)
*/
HB_FUNC( WINAPI_IMAGELIST_REMOVEALL )
{
  hb_retl( ( WINBOOL ) ImageList_RemoveAll( ( HIMAGELIST ) hb_parptr( 1 ) ) );
}

/*
HICON ImageList_ExtractIcon(HINSTANCE hi,HIMAGELIST himl,int i)
*/
HB_FUNC( WINAPI_IMAGELIST_EXTRACTICON )
{
  hb_retptr( ( HICON ) ImageList_ExtractIcon( ( HINSTANCE ) hb_parptr( 1 ), ( HIMAGELIST ) hb_parptr( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
HIMAGELIST ImageList_LoadBitmap(HINSTANCE hi,LPCSTR lpbmp,int cx,int cGrow,COLORREF crMask)
*/

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

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_SetIconSize(HIMAGELIST himl,int cx,int cy)
*/
HB_FUNC( WINAPI_IMAGELIST_SETICONSIZE )
{
  hb_retl( ( WINBOOL ) ImageList_SetIconSize( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI ImageList_GetImageInfo(HIMAGELIST himl,int i,IMAGEINFO *pImageInfo)
*/

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Merge(HIMAGELIST himl1,int i1,HIMAGELIST himl2,int i2,int dx,int dy)
*/
HB_FUNC( WINAPI_IMAGELIST_MERGE )
{
  hb_retptr( ( HIMAGELIST ) ImageList_Merge( ( HIMAGELIST ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( HIMAGELIST ) hb_parptr( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( int ) hb_parni( 6 ) ) );
}

/*
WINCOMMCTRLAPI HIMAGELIST WINAPI ImageList_Duplicate(HIMAGELIST himl)
*/
HB_FUNC( WINAPI_IMAGELIST_DUPLICATE )
{
  hb_retptr( ( HIMAGELIST ) ImageList_Duplicate( ( HIMAGELIST ) hb_parptr( 1 ) ) );
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

/*
WINCOMMCTRLAPI void WINAPI DrawStatusTextW(HDC hDC,LPCRECT lprc,LPCWSTR pszText,UINT uFlags)
*/

/*
WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowA(LONG style,LPCSTR lpszText,HWND hwndParent,UINT wID)
*/
HB_FUNC( WINAPI_CREATESTATUSWINDOWA )
{
  hb_retptr( ( HWND ) CreateStatusWindowA( ( LONG ) hb_parnl( 1 ), ( LPCSTR ) hb_parc( 2 ), ( HWND ) hb_parptr( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINCOMMCTRLAPI HWND WINAPI CreateStatusWindowW(LONG style,LPCWSTR lpszText,HWND hwndParent,UINT wID)
*/
HB_FUNC( WINAPI_CREATESTATUSWINDOWW )
{
  hb_retptr( ( HWND ) CreateStatusWindowW( ( LONG ) hb_parnl( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( HWND ) hb_parptr( 3 ), ( UINT ) hb_parni( 4 ) ) );
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
HB_FUNC( WINAPI_MAKEDRAGLIST )
{
  hb_retl( ( WINBOOL ) MakeDragList( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI void WINAPI DrawInsert(HWND handParent,HWND hLB,int nItem)
*/
HB_FUNC( WINAPI_DRAWINSERT )
{
  DrawInsert( ( HWND ) hb_parptr( 1 ), ( HWND ) hb_parptr( 2 ), ( int ) hb_parni( 3 ) );
}

/*
WINCOMMCTRLAPI int WINAPI LBItemFromPt(HWND hLB,POINT pt,WINBOOL bAutoScroll)
*/

/*
WINCOMMCTRLAPI HWND WINAPI CreateUpDownControl(DWORD dwStyle,int x,int y,int cx,int cy,HWND hParent,int nID,HINSTANCE hInst,HWND hBuddy,int nUpper,int nLower,int nPos)
*/
HB_FUNC( WINAPI_CREATEUPDOWNCONTROL )
{
  hb_retptr( ( HWND ) CreateUpDownControl( ( DWORD ) hb_parnl( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( int ) hb_parni( 5 ), ( HWND ) hb_parptr( 6 ), ( int ) hb_parni( 7 ), ( HINSTANCE ) hb_parptr( 8 ), ( HWND ) hb_parptr( 9 ), ( int ) hb_parni( 10 ), ( int ) hb_parni( 11 ), ( int ) hb_parni( 12 ) ) );
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
HB_FUNC( WINAPI_FLATSB_ENABLESCROLLBAR )
{
  hb_retl( ( WINBOOL ) FlatSB_EnableScrollBar( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_ShowScrollBar(HWND,int code,WINBOOL)
*/
HB_FUNC( WINAPI_FLATSB_SHOWSCROLLBAR )
{
  hb_retl( ( WINBOOL ) FlatSB_ShowScrollBar( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( WINBOOL ) hb_parl( 3 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollRange(HWND,int code,LPINT,LPINT)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollInfo(HWND,int code,LPSCROLLINFO)
*/

/*
WINCOMMCTRLAPI int WINAPI FlatSB_GetScrollPos(HWND,int code)
*/
HB_FUNC( WINAPI_FLATSB_GETSCROLLPOS )
{
  hb_retni( ( int ) FlatSB_GetScrollPos( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollProp(HWND,int propIndex,LPINT)
*/

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_GetScrollPropPtr(HWND,int propIndex,PINT_PTR)
*/

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollPos(HWND,int code,int pos,WINBOOL fRedraw)
*/
HB_FUNC( WINAPI_FLATSB_SETSCROLLPOS )
{
  hb_retni( ( int ) FlatSB_SetScrollPos( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( WINBOOL ) hb_parl( 4 ) ) );
}

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollInfo(HWND,int code,LPSCROLLINFO,WINBOOL fRedraw)
*/

/*
WINCOMMCTRLAPI int WINAPI FlatSB_SetScrollRange(HWND,int code,int min,int max,WINBOOL fRedraw)
*/
HB_FUNC( WINAPI_FLATSB_SETSCROLLRANGE )
{
  hb_retni( ( int ) FlatSB_SetScrollRange( ( HWND ) hb_parptr( 1 ), ( int ) hb_parni( 2 ), ( int ) hb_parni( 3 ), ( int ) hb_parni( 4 ), ( WINBOOL ) hb_parl( 5 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI FlatSB_SetScrollProp(HWND,UINT index,INT_PTR newValue,WINBOOL)
*/
HB_FUNC( WINAPI_FLATSB_SETSCROLLPROP )
{
  hb_retl( ( WINBOOL ) FlatSB_SetScrollProp( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( INT_PTR ) hb_parni( 3 ), ( WINBOOL ) hb_parl( 4 ) ) );
}

/*
WINCOMMCTRLAPI WINBOOL WINAPI InitializeFlatSB(HWND)
*/
HB_FUNC( WINAPI_INITIALIZEFLATSB )
{
  hb_retl( ( WINBOOL ) InitializeFlatSB( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI HRESULT WINAPI UninitializeFlatSB(HWND)
*/
HB_FUNC( WINAPI_UNINITIALIZEFLATSB )
{
  hb_retnl( ( HRESULT ) UninitializeFlatSB( ( HWND ) hb_parptr( 1 ) ) );
}

/*
WINCOMMCTRLAPI HRESULT WINAPI LoadIconMetric (HINSTANCE hinst, PCWSTR pszName, int lims, HICON *phico)
*/

/*
WINCOMMCTRLAPI HRESULT WINAPI LoadIconWithScaleDown (HINSTANCE hinst, PCWSTR pszName, int cx, int cy, HICON *phico)
*/