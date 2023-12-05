/*
 * Xbase++ Compatible xbpFont Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/* Class WvgFont() */
CREATE CLASS WvgFont

   VAR    hFont
   VAR    oPS
   VAR    hdc

   VAR    familyName                            INIT ""
   VAR    height                                INIT 0
   VAR    nominalPointSize                      INIT 0

   VAR    width                                 INIT 0
   VAR    widthClass                            INIT .F.

   VAR    bold                                  INIT .F.
   VAR    weightClass                           INIT WIN_FW_DONTCARE

   VAR    italic                                INIT .F.
   VAR    strikeout                             INIT .F.
   VAR    underscore                            INIT .F.
   VAR    codePage                              INIT WIN_DEFAULT_CHARSET

   VAR    fixed                                 INIT .F.
   VAR    antiAliased                           INIT .F.

   VAR    compoundName                          INIT ""
   METHOD setCompoundName( cName )              INLINE ::compoundName := cName

   VAR    generic                               INIT .T.

   VAR    baseLine                              INIT 0 READONLY
   VAR    dbcs                                  INIT .F.
   VAR    kerning                               INIT .F.
   VAR    mbcs                                  INIT .F.
   VAR    vector                                INIT .F.
   VAR    outlined                              INIT .F.

   VAR    aFontInfo                             INIT {}

   METHOD new( oPS )
   METHOD create( cFontName )
   METHOD configure( cFontName )
   METHOD list()
   METHOD createFont()

   DESTRUCTOR destroy()

ENDCLASS

METHOD WvgFont:new( oPS )

   __defaultNIL( @oPS, ::oPS )

   ::oPS := oPS

   RETURN Self

METHOD WvgFont:create( cFontName )

   __defaultNIL( @cFontName, ::familyName )

   ::familyName := cFontName

   ::createFont()

   RETURN Self

METHOD WvgFont:configure( cFontName )

   __defaultNIL( @cFontName, ::familyName )

   ::familyName := cFontName

   ::createFont()

   RETURN Self

METHOD WvgFont:destroy()

   IF ::hFont != NIL
      wvg_DeleteObject( ::hFont )
   ENDIF

   RETURN Self

METHOD WvgFont:list()
   RETURN {}

METHOD WvgFont:createFont()

   LOCAL aFont

   IF ::hFont != NIL
      wvg_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::oPS != NIL
      ::height := wvg_PointSizeToHeight( ::oPS:hdc, ::nominalPointSize )
   ENDIF

   ::aFontInfo := Array( 15 )

   ::aFontInfo[  1 ] := ::familyName
   ::aFontInfo[  2 ] := ::height
   ::aFontInfo[  3 ] := ::width
   ::aFontInfo[  4 ] := iif(::bold, WIN_FW_BOLD, 0)
   ::aFontInfo[  5 ] := ::italic
   ::aFontInfo[  6 ] := ::underscore
   ::aFontInfo[  7 ] := ::strikeout
   ::aFontInfo[  8 ] := ::codePage
   ::aFontInfo[  9 ] := 0
   ::aFontInfo[ 10 ] := 0
   ::aFontInfo[ 11 ] := 0
   ::aFontInfo[ 12 ] := 0
   ::aFontInfo[ 13 ] := WIN_DEFAULT_QUALITY
   ::aFontInfo[ 14 ] := NIL

   aFont := wvg_FontCreate( ::aFontInfo )

   IF Empty( aFont[ 1 ] )
      RETURN NIL
   ENDIF

   ::hFont     := aFont[ 15 ]
   ::aFontInfo := aFont

   RETURN ::hFont
