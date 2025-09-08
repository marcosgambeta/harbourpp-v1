//
// {Video subsystem template}
//
// Copyright 1999 {list of individual authors and e-mail addresses}
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

// NOTE: User programs should never call this layer directly!

// TODO: include any standard headers here

#include "hbgtcore.hpp"
#include "hbinit.hpp"
static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)
#define HB_GTID_PTR (&s_GtId)

static void hb_gt_tpl_Init(PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout,
                           HB_FHANDLE hFilenoStderr) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_Init(%p,%p,%p,%p)", static_cast<void*>(pGT), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdin)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStdout)), reinterpret_cast<void*>(static_cast<HB_PTRUINT>(hFilenoStderr))));
#endif

  // TODO:

  HB_GTSUPER_INIT(pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr);
}

static void hb_gt_tpl_Exit(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_Exit(%p)", static_cast<void*>(pGT)));
#endif

  HB_GTSUPER_EXIT(pGT);

  // TODO:
}

static int hb_gt_tpl_ReadKey(PHB_GT pGT, int iEventMask) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_ReadKey(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iEventMask);

  // TODO: check the input queue (incoming mouse and keyboard events)
  //       and return the inkey code if any

  return 0;
}

static const char *hb_gt_tpl_Version(PHB_GT pGT, int iType) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_Version(%p,%d)", static_cast<void*>(pGT), iType));
#endif

  HB_SYMBOL_UNUSED(pGT);

  if (iType == 0)
    return HB_GT_DRVNAME(HB_GT_NAME);

  return "Terminal: (template)";
}

static HB_BOOL hb_gt_tpl_SetMode(PHB_GT pGT, int iRows, int iCols) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_SetMode(%p,%d,%d)", static_cast<void*>(pGT), iRows, iCols));
#endif

  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iRows);
  HB_SYMBOL_UNUSED(iCols);

  // TODO: if possible change the size of the screen and return true

  return false;
}

static void hb_gt_tpl_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_Redraw(%p,%d,%d,%d)", static_cast<void*>(pGT), iRow, iCol, iSize));
#endif

  int iColor;
  HB_BYTE bAttr;
  HB_USHORT usChar;

  while (iSize--) {
    if (!HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol, &iColor, &bAttr, &usChar))
      break;
    // TODO: display usChar at iRow, iCol position with color bColor
    ++iCol;
  }
}

static void hb_gt_tpl_Refresh(PHB_GT pGT) // FuncTable
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_tpl_Refresh(%p)", static_cast<void*>(pGT)));
#endif

  int iRow, iCol, iStyle;

  HB_GTSUPER_REFRESH(pGT);
  HB_GTSELF_GETSCRCURSOR(pGT, &iRow, &iCol, &iStyle);

  // TODO: set cursor position and shape
}

// ***********************************************************************

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", static_cast<void*>(pFuncTable)));
#endif

  pFuncTable->Init = hb_gt_tpl_Init;
  pFuncTable->Exit = hb_gt_tpl_Exit;
  pFuncTable->ReadKey = hb_gt_tpl_ReadKey;
  pFuncTable->Version = hb_gt_tpl_Version;
  pFuncTable->SetMode = hb_gt_tpl_SetMode;
  pFuncTable->Redraw = hb_gt_tpl_Redraw;
  pFuncTable->Refresh = hb_gt_tpl_Refresh;

  return true;
}

// ***********************************************************************

#include "hbgtreg.hpp"

// ***********************************************************************
