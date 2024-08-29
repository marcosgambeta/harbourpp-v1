/*
 * Mouse API
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 * Copyright 1999 Jose Lalin <dezac@corevia.com> (API proposal)
 *
 */

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

#include "hbgtcore.hpp"

/* NOTE: Mouse initialization is called directly from low-level GT driver
 * because it possible that mouse subsystem can depend on the terminal
 * (for example, mouse subsystem cannot be initialized before ncurses
 * driver is initialized).
 */
/* C callable interface */

HB_BOOL hb_mouseIsPresent(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseIsPresent()"));
#endif

  auto fPresent = false;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    fPresent = HB_GTSELF_MOUSEISPRESENT(pGT);
    hb_gt_BaseFree(pGT);
  }
  return fPresent;
}

HB_BOOL hb_mouseGetCursor(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetCursor()"));
#endif

  auto fVisible = false;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    fVisible = HB_GTSELF_MOUSEGETCURSOR(pGT);
    hb_gt_BaseFree(pGT);
  }
  return fVisible;
}

void hb_mouseSetCursor(HB_BOOL fVisible)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetCursor(%d)", static_cast<int>(fVisible)));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSESETCURSOR(pGT, fVisible);
    hb_gt_BaseFree(pGT);
  }
}

int hb_mouseCol(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseCol()"));
#endif

  int iCol = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iCol = HB_GTSELF_MOUSECOL(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iCol;
}

int hb_mouseRow(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseRow()"));
#endif

  int iRow = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iRow = HB_GTSELF_MOUSEROW(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iRow;
}

void hb_mouseGetPos(int *piRow, int *piCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetPos(%p, %p)", static_cast<void*>(piRow), static_cast<void*>(piCol)));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSEGETPOS(pGT, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseSetPos(int iRow, int iCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetPos(%d, %d)", iRow, iCol));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSESETPOS(pGT, iRow, iCol);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseSetBounds(int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetBounds(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSESETBOUNDS(pGT, iTop, iLeft, iBottom, iRight);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseGetBounds(int *piTop, int *piLeft, int *piBottom, int *piRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetBounds(%p, %p, %p, %p)", static_cast<void*>(piTop), static_cast<void*>(piLeft), static_cast<void*>(piBottom), static_cast<void*>(piRight)));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSEGETBOUNDS(pGT, piTop, piLeft, piBottom, piRight);
    hb_gt_BaseFree(pGT);
  }
}

int hb_mouseStorageSize(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseStorageSize()"));
#endif

  int iSize = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iSize = HB_GTSELF_MOUSESTORAGESIZE(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iSize;
}

void hb_mouseSaveState(void *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSaveState(%p)", pBuffer));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSESAVESTATE(pGT, pBuffer);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseRestoreState(const void *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseRestoreState(%p)", pBuffer));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSERESTORESTATE(pGT, pBuffer);
    hb_gt_BaseFree(pGT);
  }
}

int hb_mouseGetDoubleClickSpeed(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseGetDoubleClickSpeed()"));
#endif

  int iSpeed = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iSpeed = HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iSpeed;
}

void hb_mouseSetDoubleClickSpeed(int iSpeed)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseSetDoubleClickSpeed(%d)", iSpeed));
#endif

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    HB_GTSELF_MOUSESETDOUBLECLICKSPEED(pGT, iSpeed);
    hb_gt_BaseFree(pGT);
  }
}

int hb_mouseCountButton(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseCountButton()"));
#endif

  int iButtons = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iButtons = HB_GTSELF_MOUSECOUNTBUTTON(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iButtons;
}

HB_BOOL hb_mouseButtonState(int iButton)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonState(%d)", iButton));
#endif

  auto fPressed = false;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    fPressed = HB_GTSELF_MOUSEBUTTONSTATE(pGT, iButton);
    hb_gt_BaseFree(pGT);
  }
  return fPressed;
}

HB_BOOL hb_mouseButtonPressed(int iButton, int *piRow, int *piCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonPressed(%d,%p,%p)", iButton, static_cast<void*>(piRow), static_cast<void*>(piCol)));
#endif

  auto fPressed = false;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    fPressed = HB_GTSELF_MOUSEBUTTONPRESSED(pGT, iButton, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
  return fPressed;
}

HB_BOOL hb_mouseButtonReleased(int iButton, int *piRow, int *piCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseButtonReleased(%d,%p,%p)", iButton, static_cast<void*>(piRow), static_cast<void*>(piCol)));
#endif

  auto fReleased = false;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    fReleased = HB_GTSELF_MOUSEBUTTONRELEASED(pGT, iButton, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
  return fReleased;
}

int hb_mouseReadKey(int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_mouseReadKey(%d)", iEventMask));
#endif

  int iKey = 0;

  PHB_GT pGT = hb_gt_Base();
  if (pGT)
  {
    iKey = HB_GTSELF_MOUSEREADKEY(pGT, iEventMask);
    hb_gt_BaseFree(pGT);
  }
  return iKey;
}
