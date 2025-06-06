//
// hb_regexReplace(cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch) --> cReturn
//
// Copyright 2006 Francesco Saverio Giudice <info/at/fsgiudice.com>
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

#define MATCH_STRING  1
#define MATCH_START   2
#define MATCH_END     3

FUNCTION hb_regexReplace(cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch)

   LOCAL aMatches, aMatch
   LOCAL cReturn
   LOCAL nOffSet := 0
   LOCAL cSearch, nStart, nLenSearch, nLenReplace

   aMatches := hb_regexAll(cRegEx, cString, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch, .F.)
   cReturn := cString

   IF !Empty(aMatches)
      FOR EACH aMatch IN aMatches
         IF hb_IsArray(aMatch) .AND. Len(aMatch) >= 1 .AND. hb_IsArray(aMatch[1])
            aMatch := aMatch[1]
            IF Len(aMatch) == 3 // if regex matches I must have an array of 3 elements
               cSearch := aMatch[MATCH_STRING]
               nStart  := aMatch[MATCH_START]
               nLenSearch  := Len(cSearch)
               nLenReplace := Len(cReplace)
               cReturn := Stuff(cReturn, nStart - nOffSet, nLenSearch, cReplace)
               nOffSet += nLenSearch - nLenReplace
            ENDIF
         ENDIF
      NEXT

   ENDIF

   RETURN cReturn
