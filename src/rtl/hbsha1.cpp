//
// SHA1 Harbour wrappers
//
// Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.hpp"

#include "sha1.hpp"

HB_FUNC(HB_SHA1)
{
  sha1_byte digest[SHA1_DIGEST_LENGTH];
  SHA_CTX ctx;

  hb_SHA1_Init(&ctx);

#if HB_SIZE_MAX > UINT_MAX
  {
    auto buffer = hb_parcx(1);
    auto nCount = hb_parclen(1);
    HB_SIZE nDone = 0;

    while (nCount)
    {
      unsigned int uiChunk;

      if (nCount > static_cast<HB_SIZE>(UINT_MAX))
      {
        uiChunk = UINT_MAX;
        nCount -= static_cast<HB_SIZE>(uiChunk);
      }
      else
      {
        uiChunk = static_cast<unsigned int>(nCount);
        nCount = 0;
      }

      hb_SHA1_Update(&ctx, buffer + nDone, uiChunk);

      nDone += static_cast<HB_SIZE>(uiChunk);
    }
  }
#else
  hb_SHA1_Update(&ctx, hb_parcx(1), hb_parclen(1));
#endif

  hb_SHA1_Final(digest, &ctx);

  if (!hb_parl(2))
  {
    char hex[(sizeof(digest) * 2) + 1];
    hb_strtohex(reinterpret_cast<char *>(digest), sizeof(digest), hex);
    hb_retclen(hex, HB_SIZEOFARRAY(hex) - 1);
  }
  else
  {
    hb_retclen(reinterpret_cast<char *>(digest), sizeof(digest));
  }
}
