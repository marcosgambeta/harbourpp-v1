//
// MemoWrit()/MemoRead() functions
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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
#include "hbapifs.hpp"

// NOTE: CA-Cl*pper has ~64 KiB (65516 bytes exactly) limit on read, in Harbour
//       this limit is extended, so we are not *strictly* compatible here.
//       [vszakats]

static void hb_memoread(HB_BOOL bHandleEOF)
{
  auto pszFileName = hb_parc(1);

  if (pszFileName)
  {
    HB_SIZE nSize;
    auto pBuffer = reinterpret_cast<char *>(hb_fileLoad(pszFileName, 0, &nSize));

    if (pBuffer)
    {
      // Don't read the file terminating EOF character
      if (bHandleEOF && nSize > 0)
      {
        if (pBuffer[nSize - 1] == HB_CHAR_EOF)
        {
          --nSize;
        }
      }
      hb_retclen_buffer(pBuffer, nSize);
    }
    else
    {
      hb_retc_null();
    }
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(HB_MEMOREAD)
{
  hb_memoread(false);
}

HB_FUNC(MEMOREAD)
{
  hb_memoread(true);
}

static HB_BOOL hb_memowrit(HB_BOOL bHandleEOF)
{
  auto pszFileName = hb_parc(1);
  auto pString = hb_param(2, Harbour::Item::STRING);
  HB_BOOL bRetVal = false;

  if (pszFileName && pString)
  {
    PHB_FILE pFile =
        hb_fileExtOpen(pszFileName, nullptr, FO_READWRITE | FO_EXCLUSIVE | FO_PRIVATE | FXO_TRUNCATE | FXO_SHARELOCK,
                       nullptr, nullptr);

    if (pFile != nullptr)
    {
      auto nSize = pString->getCLen();
      auto pData = pString->getCPtr();

      while (nSize > 0)
      {
        HB_SIZE nWritten = hb_fileWrite(pFile, pData, nSize, 0);
        if (nWritten == 0 || nWritten == static_cast<HB_SIZE>(FS_ERROR))
        {
          break;
        }
        nSize -= nWritten;
        pData += nWritten;
      }
      bRetVal = nSize == 0;

      // NOTE: CA-Cl*pper will add the EOF even if the write failed. [vszakats]
      // NOTE: CA-Cl*pper will not return .F. when the EOF could not be written. [vszakats]
      if (bHandleEOF && bRetVal)
      { // if true, then write EOF
        char cEOF = HB_CHAR_EOF;
        hb_fileWrite(pFile, &cEOF, sizeof(char), -1);
      }

      hb_fileClose(pFile);
    }
  }

  return bRetVal;
}

HB_FUNC(HB_MEMOWRIT)
{
  hb_retl(hb_memowrit(false));
}

HB_FUNC(MEMOWRIT)
{
  hb_retl(hb_memowrit(true));
}
