//
// dbf structure related functions:
//    FieldSize(), FieldDeci(), FieldNum(), DbfSize()
//
// Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapirdd.hpp>

HB_FUNC_TRANSLATE(FIELDSIZE, FIELDLEN)
HB_FUNC_TRANSLATE(FIELDDECI, FIELDDEC)
HB_FUNC_TRANSLATE(FIELDNUM, FIELDPOS)

HB_FUNC(DBFSIZE)
{
  HB_MAXINT llSize = 0;
  AREAP pArea;

  if ((pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer())) != nullptr)
  {
    auto pSize = hb_itemNew(nullptr);

    if (SELF_INFO(pArea, DBI_GETHEADERSIZE, pSize) == Harbour::SUCCESS)
    {
      llSize = hb_itemGetNL(pSize) + 1;
      if (SELF_INFO(pArea, DBI_GETRECSIZE, pSize) == Harbour::SUCCESS)
      {
        HB_ULONG ulRecSize, ulRecCount;
        ulRecSize = hb_itemGetNL(pSize);
        if (SELF_RECCOUNT(pArea, &ulRecCount) == Harbour::SUCCESS)
        {
          llSize += static_cast<HB_MAXINT>(ulRecCount) * ulRecSize;
        }
      }
    }
    hb_itemRelease(pSize);
  }

  hb_retnint(llSize);
}
