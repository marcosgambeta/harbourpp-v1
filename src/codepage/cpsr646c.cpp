// National Collation Support Module (SR646C)
// Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>

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

#define HB_CP_ID        SR646C
constexpr const char *HB_CP_INFO = "Serbian ISO-646C (Cyrillic YUSCII)"; //#define HB_CP_INFO      "Serbian ISO-646C (Cyrillic YUSCII)"
#define HB_CP_UNITB     HB_UNITB_646YUC
#define HB_CP_ACSORT    HB_CDP_ACSORT_NONE
#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "\xD0\x90\xD0\x91\xD0\xA6\xD0\xA7\xD0\x8B\xD0\x94\xD0\x82\xD0\x95\xD0\xA4\xD0\x93\xD0\xA5\xD0\x98\xD0\x88\xD0\x9A\xD0\x9B\xD0\x9C\xD0\x9D\xD0\x9E\xD0\x9F\xD0\x89\xD0\xA0\xD0\xA1\xD0\xA8\xD0\xA2\xD0\xA3\xD0\x92\xD0\x8A\xD0\x8F\xD0\x85\xD0\x97\xD0\x96"
constexpr const char *HB_CP_UPPER = "\xD0\x90\xD0\x91\xD0\xA6\xD0\xA7\xD0\x8B\xD0\x94\xD0\x82\xD0\x95\xD0\xA4\xD0\x93\xD0\xA5\xD0\x98\xD0\x88\xD0\x9A\xD0\x9B\xD0\x9C\xD0\x9D\xD0\x9E\xD0\x9F\xD0\x89\xD0\xA0\xD0\xA1\xD0\xA8\xD0\xA2\xD0\xA3\xD0\x92\xD0\x8A\xD0\x8F\xD0\x85\xD0\x97\xD0\x96";
//#define HB_CP_LOWER     "\xD0\xB0\xD0\xB1\xD1\x86\xD1\x87\xD1\x9B\xD0\xB4\xD1\x92\xD0\xB5\xD1\x84\xD0\xB3\xD1\x85\xD0\xB8\xD1\x98\xD0\xBA\xD0\xBB\xD0\xBC\xD0\xBD\xD0\xBE\xD0\xBF\xD1\x99\xD1\x80\xD1\x81\xD1\x88\xD1\x82\xD1\x83\xD0\xB2\xD1\x9A\xD1\x9F\xD1\x95\xD0\xB7\xD0\xB6"
constexpr const char *HB_CP_LOWER = "\xD0\xB0\xD0\xB1\xD1\x86\xD1\x87\xD1\x9B\xD0\xB4\xD1\x92\xD0\xB5\xD1\x84\xD0\xB3\xD1\x85\xD0\xB8\xD1\x98\xD0\xBA\xD0\xBB\xD0\xBC\xD0\xBD\xD0\xBE\xD0\xBF\xD1\x99\xD1\x80\xD1\x81\xD1\x88\xD1\x82\xD1\x83\xD0\xB2\xD1\x9A\xD1\x9F\xD1\x95\xD0\xB7\xD0\xB6";
#else
//#define HB_CP_UPPER     "АБЦЧЋДЂЕФГХИЈКЛМНОПЉРСШТУВЊЏЅЗЖ"
constexpr const char *HB_CP_UPPER = "АБЦЧЋДЂЕФГХИЈКЛМНОПЉРСШТУВЊЏЅЗЖ";
//#define HB_CP_LOWER     "абцчћдђефгхијклмнопљрсштувњџѕзж"
constexpr const char *HB_CP_LOWER = "абцчћдђефгхијклмнопљрсштувњџѕзж";
#endif
#define HB_CP_UTF8

// include CP registration code
#include "hbcdpreg.hpp"
