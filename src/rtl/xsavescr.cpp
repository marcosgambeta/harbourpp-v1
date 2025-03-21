//
// __XSaveScreen()/__XRestScreen() functions
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) (Rewritten in C)
// Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
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

#include "hbapigt.hpp"
#include "hbstack.hpp"

// NOTE: In original CA-Cl*pper 5.x these functions are written in Clipper
//       [vszakats]

struct HB_SCRDATA
{
  int row;
  int col;
  int maxrow;
  int maxcol;
  void *buffer;
};

using PHB_SCRDATA = HB_SCRDATA *;

static void hb_xSaveRestRelease(void *cargo)
{
  auto pScrData = static_cast<PHB_SCRDATA>(cargo);

  if (pScrData->buffer)
  {
    hb_xfree(pScrData->buffer);
  }
}

static HB_TSD_NEW(s_scrData, sizeof(HB_SCRDATA), nullptr, hb_xSaveRestRelease);

HB_FUNC(__XSAVESCREEN)
{
  auto pScrData = static_cast<PHB_SCRDATA>(hb_stackGetTSD(&s_scrData));
  HB_SIZE nSize;

  hb_gtGetPos(&pScrData->row, &pScrData->col);
  pScrData->maxrow = hb_gtMaxRow();
  pScrData->maxcol = hb_gtMaxCol();
  hb_gtRectSize(0, 0, pScrData->maxrow, pScrData->maxcol, &nSize);
  if (pScrData->buffer)
  {
    hb_xfree(pScrData->buffer);
  }
  pScrData->buffer = hb_xgrab(nSize);
  hb_gtSave(0, 0, pScrData->maxrow, pScrData->maxcol, pScrData->buffer);
}

// NOTE: There's no check about the screen size on restore, so this will
//       fail if the user has changed the screen resolution between calling
//       save and restore.
//       [vszakats]

HB_FUNC(__XRESTSCREEN)
{
  auto pScrData = static_cast<PHB_SCRDATA>(hb_stackTestTSD(&s_scrData));

  if (pScrData && pScrData->buffer)
  {
    hb_gtRest(0, 0, pScrData->maxrow, pScrData->maxcol, pScrData->buffer);
    hb_xfree(pScrData->buffer);
    pScrData->buffer = nullptr;
    hb_gtSetPos(pScrData->row, pScrData->col);
  }
}
