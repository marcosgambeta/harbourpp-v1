//
// Descend() function
//
// Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "hbapiitm.hpp"

void hb_strDescend(char *szStringTo, const char *szStringFrom, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strDescend(%p, %s, %" HB_PFS "u)", static_cast<void*>(szStringTo), szStringFrom, nLen));
#endif

  if (nLen == 1 && szStringFrom[0] == '\0')
  {
    szStringTo[0] = '\0';
  }
  else
  {
    for (; nLen--; szStringTo++, szStringFrom++)
    {
      *szStringTo = static_cast<char>(256 - *szStringFrom);
    }
  }
}

HB_FUNC(DESCEND)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr)
  {
    if (pItem->isString())
    {
      auto nLen = pItem->getCLen();

      if (nLen > 0)
      {
        auto szBuffer = static_cast<char *>(hb_xgrab(nLen + 1));
        hb_strDescend(szBuffer, pItem->getCPtr(), nLen);
        hb_retclen_buffer(szBuffer, nLen);
      }
      else
      {
        hb_retc_null();
      }
    }
    else if (pItem->isDate())
    {
      hb_retnl(5231808 - pItem->getDL());
    }
    else if (pItem->isNumeric())
    {
      hb_retnd(-1 * pItem->getND());
    }
    else if (pItem->isLogical())
    {
      hb_retl(!pItem->getL());
    }
  }
}
