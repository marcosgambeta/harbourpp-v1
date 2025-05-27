//
// National Collation Support Module (IT437)
//
// Copyright 2004 Maurilio Longo <maurilio.longo@libero.it>
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

#define HB_CP_ID        IT437
constexpr const char *HB_CP_INFO = "Italian CP-437"; // #define HB_CP_INFO      "Italian CP-437"
#define HB_CP_UNITB     HB_UNITB_437
#define HB_CP_ACSORT    HB_CDP_ACSORT_NONE
#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "AAAA\xC3\x84\xC3\x85" "BCDEE\xC3\x89" "FGHIIIJKLMNOOOPQRSTUUUVWXYZ"
constexpr const char *HB_CP_UPPER = "AAAA\xC3\x84\xC3\x85" "BCDEE\xC3\x89" "FGHIIIJKLMNOOOPQRSTUUUVWXYZ";
//#define HB_CP_LOWER     "a\xC3\xA0\xC3\xA1\xC3\xA2\xC3\xA4\xC3\xA5" "bcde\xC3\xA8\xC3\xA9" "fghi\xC3\xAC\xC3\xAD" "jklmno\xC3\xB2\xC3\xB3" "pqrstu\xC3\xB9\xC3\xBA" "vwxyz"
constexpr const char *HB_CP_LOWER = "a\xC3\xA0\xC3\xA1\xC3\xA2\xC3\xA4\xC3\xA5" "bcde\xC3\xA8\xC3\xA9" "fghi\xC3\xAC\xC3\xAD" "jklmno\xC3\xB2\xC3\xB3" "pqrstu\xC3\xB9\xC3\xBA" "vwxyz";
#else
//#define HB_CP_UPPER     "AAAAÄÅBCDEEÉFGHIIIJKLMNOOOPQRSTUUUVWXYZ"
constexpr const char *HB_CP_UPPER = "AAAAÄÅBCDEEÉFGHIIIJKLMNOOOPQRSTUUUVWXYZ";
//#define HB_CP_LOWER     "aàáâäåbcdeèéfghiìíjklmnoòópqrstuùúvwxyz"
constexpr const char *HB_CP_LOWER = "aàáâäåbcdeèéfghiìíjklmnoòópqrstuùúvwxyz";
#endif
#define HB_CP_UTF8

// include CP registration code
#include "hbcdpreg.hpp"
