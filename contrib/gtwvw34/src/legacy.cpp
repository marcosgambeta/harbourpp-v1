/* Copyright 2014 Viktor Szakats */

#include "hbdefs.hpp"

/* legacy non-prefixed names mapped to proper local ones and to hbwin */
#if 1
HB_FUNC_TRANSLATE(ADDTOOLTIPEX, WVW_ADDTOOLTIPEX)
HB_FUNC_TRANSLATE(BRINGTOTOP1, WVW_BRINGTOTOP1)
HB_FUNC_TRANSLATE(CREATEFONT, WIN_CREATEFONT)
HB_FUNC_TRANSLATE(CREATEHATCHBRUSH, WVW_CREATEHATCHBRUSH)
HB_FUNC_TRANSLATE(CREATEIMAGELIST, WVW_CREATEIMAGELIST)
HB_FUNC_TRANSLATE(CREATESOLIDBRUSH, WVW_CREATESOLIDBRUSH)
HB_FUNC_TRANSLATE(DRAWBITMAP, WVW_DRAWBITMAP)
HB_FUNC_TRANSLATE(DRAWICON, WVW_DRAWICON)
HB_FUNC_TRANSLATE(GETBITMAPSIZE, WVW_GETBITMAPSIZE)
HB_FUNC_TRANSLATE(GETICONSIZE, WVW_GETICONSIZE)
HB_FUNC_TRANSLATE(GETSYSCOLOR, WAGETSYSCOLOR)
HB_FUNC_TRANSLATE(IMAGELIST_ADD, WVW_IMAGELIST_ADD)
HB_FUNC_TRANSLATE(IMAGELIST_ADDMASKED, WVW_IMAGELIST_ADDMASKED)
HB_FUNC_TRANSLATE(INVALIDATERECT, WIN_INVALIDATERECT)
HB_FUNC_TRANSLATE(ISWINDOW, WAPI_ISWINDOW)
HB_FUNC_TRANSLATE(LOADBITMAP, WVW_LOADBITMAP)
HB_FUNC_TRANSLATE(LOADBITMAPEX, WVW_LOADBITMAPEX)
HB_FUNC_TRANSLATE(LOADICON, WVW_LOADICON)
HB_FUNC_TRANSLATE(LOADIMAGE, WVW_LOADIMAGE)
HB_FUNC_TRANSLATE(OPENBITMAP, WVW_OPENBITMAP)
HB_FUNC_TRANSLATE(OPENIMAGE, WVW_OPENIMAGE)
HB_FUNC_TRANSLATE(REDRAWWINDOW, WVW_REDRAWWINDOW)
HB_FUNC_TRANSLATE(RGB, WAPI_RGB)
HB_FUNC_TRANSLATE(SELECTFONT, WVW_SELECTFONT)
HB_FUNC_TRANSLATE(SENDMESSAGE, WAPI_SENDMESSAGE)
HB_FUNC_TRANSLATE(SETBITMAPRESOURCEID, WVW_SETBITMAPRESOURCEID)
HB_FUNC_TRANSLATE(SETBKCOLOR, WAPI_SETBKCOLOR)
HB_FUNC_TRANSLATE(SETPARENT, WVW_SETPARENT)
HB_FUNC_TRANSLATE(SETTEXTCOLOR, WAPI_SETTEXTCOLOR)
HB_FUNC_TRANSLATE(TOOLBARADDBUTTONS, WVW_TOOLBARADDBUTTONS)
HB_FUNC_TRANSLATE(WINDOW2BITMAP, WVW_WINDOW2BITMAP)
#endif

/* mappings to hbwin */
HB_FUNC_TRANSLATE(WIN_CHECKDLGBUTTON, WAPI_CHECKDLGBUTTON)
HB_FUNC_TRANSLATE(WIN_CHECKRADIOBUTTON, WAPI_CHECKRADIOBUTTON)
HB_FUNC_TRANSLATE(WIN_DRAWTEXT, WAPI_DRAWTEXT)
HB_FUNC_TRANSLATE(WIN_GETDIALOGBASEUNITS, WAPI_GETDIALOGBASEUNITS)
HB_FUNC_TRANSLATE(WIN_GETDLGITEM, WAPI_GETDLGITEM)
HB_FUNC_TRANSLATE(WIN_GETDLGITEMTEXT, WAPI_GETDLGITEMTEXT)
HB_FUNC_TRANSLATE(WIN_GETSTOCKOBJECT, WAPI_GETSTOCKOBJECT)
HB_FUNC_TRANSLATE(WIN_GETSYSCOLOR, WAGETSYSCOLOR)
HB_FUNC_TRANSLATE(WIN_ISDLGBUTTONCHECKED, WAPI_ISDLGBUTTONCHECKED)
HB_FUNC_TRANSLATE(WIN_MESSAGEBOX, WAPI_MESSAGEBOX)
HB_FUNC_TRANSLATE(WIN_MULDIV, WAPI_MULDIV)
HB_FUNC_TRANSLATE(WIN_SENDDLGITEMMESSAGE, WAPI_SENDDLGITEMMESSAGE)
HB_FUNC_TRANSLATE(WIN_SENDMESSAGE, WAPI_SENDMESSAGE)
HB_FUNC_TRANSLATE(WIN_SETBKCOLOR, WAPI_SETBKCOLOR)
HB_FUNC_TRANSLATE(WIN_SETDLGITEMTEXT, WAPI_SETDLGITEMTEXT)
HB_FUNC_TRANSLATE(WIN_SETFOCUS, WAPI_SETFOCUS)
HB_FUNC_TRANSLATE(WIN_SETMENU, WAPI_SETMENU)
HB_FUNC_TRANSLATE(WIN_SETTEXTCOLOR, WAPI_SETTEXTCOLOR)
HB_FUNC_TRANSLATE(WVW_CREATEHATCHBRUSH, WAPI_CREATEHATCHBRUSH)
HB_FUNC_TRANSLATE(WVW_CREATEMENU, WAPI_CREATEMENU)
HB_FUNC_TRANSLATE(WVW_CREATEPOPUPMENU, WAPI_CREATEPOPUPMENU)
HB_FUNC_TRANSLATE(WVW_CREATESOLIDBRUSH, WAPI_CREATESOLIDBRUSH)
HB_FUNC_TRANSLATE(WVW_DELETEMENU, WAPI_DELETEMENU)
HB_FUNC_TRANSLATE(WVW_DESTROYMENU, WAPI_DESTROYMENU)
HB_FUNC_TRANSLATE(WVW_ENABLEMENUITEM, WAPI_ENABLEMENUITEM)
HB_FUNC_TRANSLATE(WVW_ENDMENU, WAPI_ENDMENU)
HB_FUNC_TRANSLATE(WVW_GETKEYSTATE, WAGETKEYSTATE)
HB_FUNC_TRANSLATE(WVW_HIWORD, WAPI_HIWORD)
HB_FUNC_TRANSLATE(WVW_ISWINDOW, WAPI_ISWINDOW)
HB_FUNC_TRANSLATE(WVW_LOWORD, WAPI_LOWORD)
HB_FUNC_TRANSLATE(WVW_RECTANGLE, WAPI_RECTANGLE)
HB_FUNC_TRANSLATE(WVW_SETBKMODE, WAPI_SETBKMODE)
HB_FUNC_TRANSLATE(WVW__MAKEDLGTEMPLATE, __WAPI_DLGTEMPLATE_RAW_NEW)

#ifdef HB_LEGACY_LEVEL5
/* local synonyms (deprecated) */
HB_FUNC_TRANSLATE(WIN_DELETEOBJECT, WVW_DELETEOBJECT)
HB_FUNC_TRANSLATE(WVW_XBSHOW, WVW_XBVISIBLE)
#endif
