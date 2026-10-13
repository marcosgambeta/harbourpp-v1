//
// Mouse API
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
// Copyright 1999 Jose Lalin <dezac@corevia.com> (API proposal)
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

#include "hbgtcore.hpp"

// NOTE: Mouse initialization is called directly from low-level GT driver
// because it possible that mouse subsystem can depend on the terminal
// (for example, mouse subsystem cannot be initialized before ncurses
// driver is initialized).

// C callable interface

HB_BOOL hb_mouseIsPresent(void)
{
  auto fPresent = false;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    fPresent = HB_GTSELF_MOUSEISPRESENT(pGT);
    hb_gt_BaseFree(pGT);
  }
  return fPresent;
}

HB_BOOL hb_mouseGetCursor(void)
{
  auto fVisible = false;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    fVisible = HB_GTSELF_MOUSEGETCURSOR(pGT);
    hb_gt_BaseFree(pGT);
  }
  return fVisible;
}

void hb_mouseSetCursor(HB_BOOL fVisible)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSESETCURSOR(pGT, fVisible);
    hb_gt_BaseFree(pGT);
  }
}

int32_t hb_mouseCol(void)
{
  int32_t iCol = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iCol = HB_GTSELF_MOUSECOL(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iCol;
}

int32_t hb_mouseRow(void)
{
  int32_t iRow = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iRow = HB_GTSELF_MOUSEROW(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iRow;
}

void hb_mouseGetPos(int32_t *piRow, int32_t *piCol)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSEGETPOS(pGT, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseSetPos(int32_t iRow, int32_t iCol)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSESETPOS(pGT, iRow, iCol);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseSetBounds(int32_t iTop, int32_t iLeft, int32_t iBottom, int32_t iRight)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSESETBOUNDS(pGT, iTop, iLeft, iBottom, iRight);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseGetBounds(int32_t *piTop, int32_t *piLeft, int32_t *piBottom, int32_t *piRight)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSEGETBOUNDS(pGT, piTop, piLeft, piBottom, piRight);
    hb_gt_BaseFree(pGT);
  }
}

int32_t hb_mouseStorageSize(void)
{
  int32_t iSize = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iSize = HB_GTSELF_MOUSESTORAGESIZE(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iSize;
}

void hb_mouseSaveState(void *pBuffer)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSESAVESTATE(pGT, pBuffer);
    hb_gt_BaseFree(pGT);
  }
}

void hb_mouseRestoreState(const void *pBuffer)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSERESTORESTATE(pGT, pBuffer);
    hb_gt_BaseFree(pGT);
  }
}

int32_t hb_mouseGetDoubleClickSpeed(void)
{
  int32_t iSpeed = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iSpeed = HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iSpeed;
}

void hb_mouseSetDoubleClickSpeed(int32_t iSpeed)
{
  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    HB_GTSELF_MOUSESETDOUBLECLICKSPEED(pGT, iSpeed);
    hb_gt_BaseFree(pGT);
  }
}

int32_t hb_mouseCountButton(void)
{
  int32_t iButtons = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iButtons = HB_GTSELF_MOUSECOUNTBUTTON(pGT);
    hb_gt_BaseFree(pGT);
  }
  return iButtons;
}

HB_BOOL hb_mouseButtonState(int32_t iButton)
{
  auto fPressed = false;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    fPressed = HB_GTSELF_MOUSEBUTTONSTATE(pGT, iButton);
    hb_gt_BaseFree(pGT);
  }
  return fPressed;
}

HB_BOOL hb_mouseButtonPressed(int32_t iButton, int32_t *piRow, int32_t *piCol)
{
  auto fPressed = false;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    fPressed = HB_GTSELF_MOUSEBUTTONPRESSED(pGT, iButton, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
  return fPressed;
}

HB_BOOL hb_mouseButtonReleased(int32_t iButton, int32_t *piRow, int32_t *piCol)
{
  auto fReleased = false;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    fReleased = HB_GTSELF_MOUSEBUTTONRELEASED(pGT, iButton, piRow, piCol);
    hb_gt_BaseFree(pGT);
  }
  return fReleased;
}

int32_t hb_mouseReadKey(int32_t iEventMask)
{
  int32_t iKey = 0;

  HB_GT *pGT = hb_gt_Base();
  if (pGT) {
    iKey = HB_GTSELF_MOUSEREADKEY(pGT, iEventMask);
    hb_gt_BaseFree(pGT);
  }
  return iKey;
}
