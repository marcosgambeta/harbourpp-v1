/*
 * CT3 video functions:
 * CharPix(), VGAPalette(), VideoType(), SetFont()
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
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

#include "hbapi.h"
#include "hbapigt.h"

#include "ctvideo.ch"

HB_FUNC( CHARPIX )
{
   hb_retni(0);
}

HB_FUNC( VGAPALETTE )
{
   const char * color_string;
   char red, green, blue;
   int attr;

   if( hb_pcount() < 4 )
   {
      /* Resetting palette registers to default values is not supported yet */
      hb_retl(false);
      return;
   }

   color_string = hb_parc(1);
   if( color_string )
   {
      attr = hb_gtColorToN(color_string);
   }
   else if( HB_ISNUM(1) )
   {
      attr = hb_parni(1);
   }
   else
   {
      attr = -1;
   }

   if( attr < 0 || attr >= 16 )
   {
      /* An invalid argument */
      hb_retl(false);
      return;
   }

   red = static_cast<char>(hb_parni(2));
   green = static_cast<char>(hb_parni(3));
   blue = static_cast<char>(hb_parni(4));

   HB_SYMBOL_UNUSED(blue);
   HB_SYMBOL_UNUSED(green);
   HB_SYMBOL_UNUSED(red);
   hb_retl(false);
}

HB_FUNC( VIDEOTYPE )
{
}

HB_FUNC( SETFONT )
{
   const char * font = hb_parcx(1);
   unsigned len = static_cast<unsigned>(hb_parclen(1));
   int area = hb_parni(2);
   int offset = 0;
   int count = 256;
   int height = 16;

   if( !area )
   {
      area = 1;
   }
   if( HB_ISNUM(3) )
   {
      offset = hb_parni(3);
   }
   if( HB_ISNUM(4) )
   {
      count = hb_parni(4);
   }
   if( HB_ISLOG(3) && hb_parl(3) && count != 0 )
   {
      height = len / count;
   }

   HB_SYMBOL_UNUSED(font);
   HB_SYMBOL_UNUSED(height);
   HB_SYMBOL_UNUSED(offset);
   HB_SYMBOL_UNUSED(area);
   hb_retni(-2);
}
