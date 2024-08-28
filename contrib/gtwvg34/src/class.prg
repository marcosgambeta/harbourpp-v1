/*
 * Wvt*Classes
 *
 * Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based On:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software and Systems Ltd
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

#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

#define K_LBUTTONPRESSED        1021
#define K_RBUTTONPRESSED        1022
#define K_MBUTTONPRESSED        1023

#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

/* TBrowseWvg From TBrowse */
#define _TBCI_COLOBJECT       1   /* column object                          */
#define _TBCI_COLWIDTH        2   /* width of the column                    */
#define _TBCI_COLPOS          3   /* column position on screen              */
#define _TBCI_CELLWIDTH       4   /* width of the cell                      */
#define _TBCI_CELLPOS         5   /* cell position in column                */
#define _TBCI_COLSEP          6   /* column separator                       */
#define _TBCI_SEPWIDTH        7   /* width of the separator                 */
#define _TBCI_HEADING         8   /* column heading                         */
#define _TBCI_FOOTING         9   /* column footing                         */
#define _TBCI_HEADSEP         10  /* heading separator                      */
#define _TBCI_FOOTSEP         11  /* footing separator                      */
#define _TBCI_DEFCOLOR        12  /* default color                          */
#define _TBCI_FROZENSPACE     13  /* space after frozen columns             */
#define _TBCI_LASTSPACE       14  /* space after last visible column        */
#define _TBCI_SIZE            14  /* size of array with TBrowse column data */

CREATE CLASS TBrowseWvg INHERIT TBrowse

   VAR    aColumnsSep                             INIT {}

   METHOD SetVisible()

ENDCLASS

METHOD TBrowseWvg:SetVisible()

   LOCAL lFirst, aCol, nColPos

   ::Super:SetVisible()
   ::aColumnsSep := {}

   lFirst := .T.
   FOR EACH aCol IN ::aColData
      IF aCol[ _TBCI_COLPOS ] != NIL
         IF lFirst
            lFirst := .F.

         ELSE
            nColPos := aCol[ _TBCI_COLPOS ]

            IF aCol[ _TBCI_SEPWIDTH ] > 0
               nColPos += Int( aCol[ _TBCI_SEPWIDTH ] / 2 )
            ENDIF

            AAdd( ::aColumnsSep, nColPos )
         ENDIF
      ENDIF
   NEXT

   RETURN Self
