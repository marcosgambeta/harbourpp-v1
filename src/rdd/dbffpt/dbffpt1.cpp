//
// DBFFPT RDD
//
// Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
//
// The SIX memo conversion algorithms and some piece of code taken from
// DBFCDX and DBFFPT
//    Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
//    Copyright 2000-2003 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#if defined(HB_FPT_NO_READLOCK)
#undef HB_MEMO_SAFELOCK
#else
// #  define HB_MEMO_SAFELOCK
#endif

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapistr.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbinit.hpp"
#include "hbset.hpp"
#include "hbstack.hpp"
#include "hbvm.hpp"
#include "hbdate.hpp"
#include "hbrddfpt.hpp"
#include "hbsxfunc.hpp"
#include "rddsys.ch"

#include "hbapicdp.hpp"

#define FPT_TRANS_NONE 0
#define FPT_TRANS_CP 1
#define FPT_TRANS_UNICODE 2

#define FPT_DIRECT_TRANS(p) (hb_vmCDP() != (p)->area.cdPage ? FPT_TRANS_CP : FPT_TRANS_NONE)

#define FPT_BLOCK_OFFSET(b) (static_cast<HB_FOFFSET>(b) * static_cast<HB_FOFFSET>(pArea->ulMemoBlockSize))

// temporary cast to suppress 32/64-bit Windows warnings
#define HB_ULONGCAST HB_ULONG

static HB_USHORT s_uiRddIdBLOB = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdFPT = static_cast<HB_USHORT>(-1);

static RDDFUNCS fptSuper;

// generate Run-Time error
static HB_ERRCODE hb_memoErrorRT(FPTAREAP pArea, HB_ERRCODE uiGenCode, HB_ERRCODE uiSubCode, const char *szFileName,
                                 HB_ERRCODE uiOsCode, HB_USHORT uiFlags)
{
  HB_ERRCODE errCode = Harbour::FAILURE;

  if (hb_vmRequestQuery() == 0)
  {
    auto pError = hb_errNew();

    if (uiGenCode == 0)
    {
      uiGenCode = hb_dbfGetEGcode(uiSubCode);
    }
    if (uiOsCode == 0 && uiSubCode != EDBF_DATATYPE && uiSubCode != EDBF_DATAWIDTH)
    {
      uiOsCode = hb_fsError();
    }

    hb_errPutGenCode(pError, uiGenCode);
    hb_errPutSubCode(pError, uiSubCode);
    if (uiOsCode)
    {
      hb_errPutOsCode(pError, uiOsCode);
    }
    hb_errPutDescription(pError, hb_langDGetErrorDesc(uiGenCode));
    if (szFileName != nullptr)
    {
      hb_errPutFileName(pError, szFileName);
    }
    if (uiFlags)
    {
      hb_errPutFlags(pError, uiFlags);
    }
    errCode = SELF_ERROR(&pArea->area, pError);
    hb_errRelease(pError);
  }
  return errCode;
}

static const char *hb_memoDefaultFileExt(int iType, HB_USHORT uiRdd)
{
  if (uiRdd == s_uiRddIdBLOB)
  {
    return DBV_MEMOEXT;
  }

  switch (iType)
  {
  case DB_MEMO_DBT:
    return DBT_MEMOEXT;
  case DB_MEMO_FPT:
    return FPT_MEMOEXT;
  case DB_MEMO_SMT:
    return SMT_MEMOEXT;
  }
  return nullptr;
}

static int hb_memoDefaultType(LPRDDNODE pRDD, HB_ULONG ulConnect)
{
  int iType = DB_MEMO_FPT;
  auto pItem = hb_stackAllocItem();

  hb_itemClear(pItem);
  if (SELF_RDDINFO(pRDD, RDDI_MEMOTYPE, ulConnect, pItem) == Harbour::SUCCESS)
  {
    iType = pItem->getNI();
  }
  hb_stackPop();

  return iType;
}

// Exclusive lock memo file.
static bool hb_fptFileLockEx(FPTAREAP pArea, bool fWait)
{
  auto fRet = false;

  if (!pArea->fShared)
  {
    fRet = true;
  }
  else
  {
    for (;;)
    {
      fRet = hb_fileLock(pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_LOCK | FLX_EXCLUSIVE | (fWait ? FLX_WAIT : 0));
      if (fRet || !fWait)
      {
        break;
      }
      hb_releaseCPU();
    }
  }
  return fRet;
}

// Shared lock memo file.
static bool hb_fptFileLockSh(FPTAREAP pArea, bool fWait)
{
  auto fRet = false;

  if (!pArea->fShared)
  {
    fRet = true;
  }
  else
  {
    for (;;)
    {
      fRet = hb_fileLock(pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_LOCK | FLX_SHARED | (fWait ? FLX_WAIT : 0));
      if (fRet || !fWait)
      {
        break;
      }
      hb_releaseCPU();
    }
  }
  return fRet;
}

// Unlock memo file - exclusive lock.
static bool hb_fptFileUnLockEx(FPTAREAP pArea)
{
  if (pArea->fShared)
  {
    hb_fileFlush(pArea->pMemoFile, false);
    return hb_fileLock(pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_UNLOCK);
  }
  else
  {
    return true;
  }
}

// Unlock memo file - shared lock
static bool hb_fptFileUnLockSh(FPTAREAP pArea)
{
  if (pArea->fShared)
  {
    hb_fileFlush(pArea->pMemoFile, false);
    return hb_fileLock(pArea->pMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_UNLOCK);
  }
  else
  {
    return true;
  }
}

// Check if MEMO file has direct access to data
static bool hb_fptHasDirectAccess(FPTAREAP pArea)
{
  return pArea->bMemoType == DB_MEMO_FPT &&
         (pArea->uiMemoVersion == DB_MEMOVER_FLEX || pArea->uiMemoVersion == DB_MEMOVER_CLIP);
}

// Lock root block pointer.
static bool hb_fptRootBlockLock(FPTAREAP pArea)
{
  auto fRet = false;

  if (!pArea->fShared)
  {
    fRet = true;
  }
  else
  {
    fRet = hb_fileLock(pArea->pMemoFile, FPT_ROOTBLOCK_OFFSET, 4, FL_LOCK | FLX_EXCLUSIVE);
  }
  return fRet;
}

// Unlock root block pointer.
static bool hb_fptRootBlockUnLock(FPTAREAP pArea)
{
  if (pArea->fShared)
  {
    hb_fileFlush(pArea->pMemoFile, false);
    return hb_fileLock(pArea->pMemoFile, FPT_ROOTBLOCK_OFFSET, 4, FL_UNLOCK);
  }
  else
  {
    return true;
  }
}

// Read root block pointer.
static HB_ERRCODE hb_fptGetRootBlock(FPTAREAP pArea, HB_ULONG *pulBlock)
{
  *pulBlock = 0;

  if (hb_fptHasDirectAccess(pArea))
  {
    HB_BYTE buffer[4];

    if (hb_fileReadAt(pArea->pMemoFile, buffer, 4, FPT_ROOTBLOCK_OFFSET) == 4)
    {
      *pulBlock = HB_GET_LE_UINT32(buffer);
      return Harbour::SUCCESS;
    }
    else
    {
      return EDBF_READ;
    }
  }
  return EDBF_UNSUPPORTED;
}

// Write root block pointer.
static HB_ERRCODE hb_fptPutRootBlock(FPTAREAP pArea, HB_ULONG ulBlock)
{
  if (hb_fptHasDirectAccess(pArea))
  {
    HB_BYTE buffer[4];

    HB_PUT_LE_UINT32(buffer, ulBlock);
    if (hb_fileWriteAt(pArea->pMemoFile, buffer, 4, FPT_ROOTBLOCK_OFFSET) == 4)
    {
      return Harbour::SUCCESS;
    }
    else
    {
      return EDBF_WRITE;
    }
  }
  return EDBF_UNSUPPORTED;
}

// GARBAGE COLLECTOR:
// I don't have any documentation about it. All I know is reverse engineering
// or analyzes of other sources. If any one can tell me something more about it
// then I will be really glad. I use method one for SixMemo and method 2 for
// FLEX memos.
//
// Method 1.
// FPTHEADER->reserved2[492]     is a list of free pages,
//                               6 bytes for each page
//                                  size[2]  (size in blocks) (little endian)
//                                  block[4] (block number) (little endian)
//                               signature1[12] has to be cut down to
//                               10 bytes. The last 2 bytes becomes the
//                               number of entries in free block list (max 82)
//
// Method 2.
// FPTHEADER->flexDir[4]         is a little endian offset to page
//                               (1024 bytes size) where header is:
//                                  type[4] = 1000 (big endian)
//                                  size[4] = 1010 (big endian)
//                               then
//                                  nItem[2] number of item (little endian)
//                               then 1008 bytes with free blocks list
//                               (max 126 entries) in format:
//                                  offset[4]   (little endian)
//                                  size[4]     (little endian)
//                               nItem is always odd and after read we have
//                               to recalculate it:
//                                  nItem = (nItem - 3) / 4
//          if FPTHEADER->flexDir = 0 then we can create it by allocating
//          two 1024 bytes pages for flexRev and flexDir page.
//             FPTHEADER->flexRev[4] 1024 bytes in next free block
//             FPTHEADER->flexDir[4] next 1024 bytes
//          flexRev page is copy of flexDir page but the items are stored
//          in reversed form size[4] first then offset[4]
//             size[4]     (little endian)
//             offset[4]   (little endian)
//          before writing GC pages (dir and rev, both has to be synced)
//          we should first sort the entries moving the shortest blocks
//          to the beginning so when we where looking for free block we
//          can scan the list from the beginning finding the first one
//          large enough. unused bytes in GC page should be filled with 0xAD
//          when we free fpt block we should set in its header:
//             type[4] = 1001 (big endian)
//             size[4] = rest of block size (block size - 8) (big endian)
//
// TODO: Clipper 5.3 can use more then one GC page. I don't have any
// documentation for that and don't have time for farther hacking
// binary files to find the algorithm. If you have any documentation
// about it, please send it to me.
// OK. I've found a while for analyzing the FPT file created by Clipper
// and I think I know this structure. It's a tree. The node type
// is marked in the first two bytes of GC page encoded as bit field with
// the number of items 2 - means branch node, 3-leaf node. The value in
// GC node is calculated as:
//    ( nItem << 2 ) | FPTGCNODE_TYPE
// Each item in branch node has 12 bytes and inside them 3 32-bit little
// endian values in pages sorted by offset the are:
//    offset,size,subpage
// and in pages sorted by size:
//    size,offset,subpage
// size and offset is the biggest (the last one) value in subpage(s)
// and subpage is offset of subpage int the file.
// All values in GC pages are in bytes not blocks - it creates the
// FPT file size limit 2^32 - if they will be in blocks then the
// the FPT file size will be limited by 2^32*block_size
// It's time to implement it ;-)

// Sort GC free memo block list by size.
static void hb_fptSortGCitems(LPMEMOGCTABLE pGCtable)
{
  auto fMoved = true;
  int l;

  // this table should be already quite good sorted so this simple
  // algorithms will be the most efficient one.
  // It will need only one or two passes
  l = pGCtable->usItems - 1;
  while (fMoved)
  {
    int j;

    fMoved = false;
    j = l;
    for (auto i = 0; i < j; i++)
    {
      if (pGCtable->pGCitems[i].ulSize > pGCtable->pGCitems[i + 1].ulSize)
      {
        HB_ULONG ulOffset, ulSize;
        auto fChanged = false;

        ulOffset = pGCtable->pGCitems[i + 1].ulOffset;
        ulSize = pGCtable->pGCitems[i + 1].ulSize;
        fChanged = pGCtable->pGCitems[i + 1].fChanged;
        pGCtable->pGCitems[i + 1].ulSize = pGCtable->pGCitems[i].ulSize;
        pGCtable->pGCitems[i + 1].ulOffset = pGCtable->pGCitems[i].ulOffset;
        pGCtable->pGCitems[i + 1].fChanged = pGCtable->pGCitems[i].fChanged;
        pGCtable->pGCitems[i].ulSize = ulSize;
        pGCtable->pGCitems[i].ulOffset = ulOffset;
        pGCtable->pGCitems[i].fChanged = fChanged;
        fMoved = true;
        pGCtable->bChanged |= 2;
        l = i;
      }
    }
  }
}

// Pack GC free memo block list - try to join free blocks.
static void hb_fptPackGCitems(LPMEMOGCTABLE pGCtable)
{
  int i, j;

  // TODO: better algorithm this primitive one can be too slow for big
  // free block list table
  for (i = 0; i < pGCtable->usItems; i++)
  {
    if (pGCtable->pGCitems[i].ulOffset != 0 && pGCtable->pGCitems[i].ulSize != 0)
    {
      HB_ULONG ulEnd = pGCtable->pGCitems[i].ulOffset + pGCtable->pGCitems[i].ulSize;
      if (ulEnd == pGCtable->ulNextBlock)
      {
        pGCtable->ulNextBlock -= pGCtable->pGCitems[i].ulSize;
        pGCtable->pGCitems[i].ulOffset = pGCtable->pGCitems[i].ulSize = 0;
        pGCtable->bChanged |= 2;
        i = -1;
      }
      else
      {
        for (j = i + 1; j < pGCtable->usItems; j++)
        {
          if (ulEnd == pGCtable->pGCitems[j].ulOffset)
          {
            pGCtable->pGCitems[i].ulSize += pGCtable->pGCitems[j].ulSize;
            pGCtable->pGCitems[i].fChanged = true;
            pGCtable->pGCitems[j].ulOffset = pGCtable->pGCitems[j].ulSize = 0;
            pGCtable->bChanged |= 2;
            i = -1;
            break;
          }
        }
      }
    }
  }

  // remove empty items
  for (i = j = 0; i < pGCtable->usItems; i++)
  {
    if (pGCtable->pGCitems[i].ulOffset != 0 && pGCtable->pGCitems[i].ulSize != 0)
    {
      if (i > j)
      {
        pGCtable->pGCitems[j].ulOffset = pGCtable->pGCitems[i].ulOffset;
        pGCtable->pGCitems[j].ulSize = pGCtable->pGCitems[i].ulSize;
        pGCtable->pGCitems[j].fChanged = pGCtable->pGCitems[i].fChanged;
      }
      j++;
    }
  }
  pGCtable->usItems = static_cast<HB_USHORT>(j);
}

// Write proper header into modified GC free memo blocks.
static HB_ERRCODE hb_fptWriteGCitems(FPTAREAP pArea, LPMEMOGCTABLE pGCtable, HB_USHORT usItem)
{
  FPTBLOCK fptBlock;
  HB_ERRCODE errCode = Harbour::SUCCESS;
#if 0
   int iStart, iStop;
#endif

  HB_SYMBOL_UNUSED(usItem);

#if 0
   if( usItem == 0 ) {
      iStart = 0;
      iStop = pGCtable->usItems;
   } else {
      iStart = usItem;
      iStop = usItem + 1;
   }
#endif

  for (auto i = 0; i < pGCtable->usItems; i++)
  {
    if (pGCtable->pGCitems[i].fChanged)
    {
      if ((pArea->uiMemoVersion == DB_MEMOVER_FLEX || pArea->uiMemoVersion == DB_MEMOVER_CLIP) &&
          // TODO: check what FLEX/CL53 exactly does in such situations
          // Tests show that FLEX/CL53 does not reuse larger blocks
          // which can leave 8 or less dummy bytes so such problem
          // does not exists. [druzus]
          pGCtable->pGCitems[i].ulSize * pArea->ulMemoBlockSize >= sizeof(FPTBLOCK))
      {
        HB_PUT_BE_UINT32(fptBlock.type, FPTIT_FLEX_UNUSED);
        HB_PUT_BE_UINT32(fptBlock.size, pArea->ulMemoBlockSize * pGCtable->pGCitems[i].ulSize - sizeof(FPTBLOCK));
        if (hb_fileWriteAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK),
                           FPT_BLOCK_OFFSET(pGCtable->pGCitems[i].ulOffset)) != sizeof(FPTBLOCK))
        {
          errCode = EDBF_WRITE;
        }
        pArea->fMemoFlush = true;
      }
      pGCtable->pGCitems[i].fChanged = false;
    }
  }
  return errCode;
}

// Add new block to GC free memo blocks list.
static HB_ERRCODE hb_fptGCfreeBlock(FPTAREAP pArea, LPMEMOGCTABLE pGCtable, HB_ULONG ulOffset, HB_ULONG ulByteSize,
                                    bool fRaw)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;
  HB_ULONG ulSize;

  if (pArea->bMemoType == DB_MEMO_DBT)
  {
    return Harbour::SUCCESS;
  }
  else if (pArea->bMemoType == DB_MEMO_FPT && !fRaw)
  {
    if (ulByteSize == 0)
    {
      FPTBLOCK fptBlock;

      if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), FPT_BLOCK_OFFSET(ulOffset)) == sizeof(FPTBLOCK))
      {
        ulByteSize = HB_GET_BE_UINT32(fptBlock.size) + sizeof(FPTBLOCK);
      }
    }
    else
    {
      ulByteSize += sizeof(FPTBLOCK);
    }
  }

  ulSize = (ulByteSize + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;

  if (ulSize == 0)
  {
    return EDBF_CORRUPT;
  }

  if (ulOffset + ulSize == pGCtable->ulNextBlock)
  {
    pGCtable->ulNextBlock -= ulSize;
    pGCtable->bChanged |= 1;
    hb_fptPackGCitems(pGCtable);
  }
  else
  {
    HB_BOOL fChanged = false;

    for (auto i = 0; i < pGCtable->usItems; i++)
    {
      if (pGCtable->pGCitems[i].ulOffset + pGCtable->pGCitems[i].ulSize == ulOffset)
      {
        ulOffset = pGCtable->pGCitems[i].ulOffset;
        ulSize = pGCtable->pGCitems[i].ulSize += ulSize;
        fChanged = pGCtable->pGCitems[i].fChanged = true;
        break;
      }
      if (pGCtable->pGCitems[i].ulOffset == ulOffset + ulSize)
      {
        pGCtable->pGCitems[i].ulOffset = ulOffset;
        ulSize = pGCtable->pGCitems[i].ulSize += ulSize;
        fChanged = pGCtable->pGCitems[i].fChanged = true;
        break;
      }
    }
    if (!fChanged)
    {
      if (pGCtable->usItems <= pGCtable->usMaxItem)
      {
        if (pGCtable->pGCitems == nullptr)
        {
          pGCtable->pGCitems = static_cast<LPMEMOGCITEM>(hb_xgrab(sizeof(MEMOGCITEM) * (pGCtable->usMaxItem + 1)));
        }
        pGCtable->pGCitems[pGCtable->usItems].ulOffset = ulOffset;
        pGCtable->pGCitems[pGCtable->usItems].ulSize = ulSize;
        pGCtable->pGCitems[pGCtable->usItems].fChanged = fChanged = true;
        pGCtable->usItems++;
      }
      else if (pGCtable->pGCitems[0].ulSize < ulSize)
      {
        if (pGCtable->ulNextBlock == pGCtable->pGCitems[0].ulOffset + pGCtable->pGCitems[0].ulSize)
        {
          pGCtable->ulNextBlock -= pGCtable->pGCitems[0].ulSize;
        }
        else if (pGCtable->pGCitems[0].fChanged)
        {
          errCode = hb_fptWriteGCitems(pArea, pGCtable, 0);
        }
        pGCtable->pGCitems[0].ulOffset = ulOffset;
        pGCtable->pGCitems[0].ulSize = ulSize;
        pGCtable->pGCitems[0].fChanged = fChanged = true;
      }
    }

    if (fChanged)
    {
      pGCtable->bChanged |= 2;
      hb_fptPackGCitems(pGCtable);
      hb_fptSortGCitems(pGCtable);
    }
  }

  return errCode;
}

// Get free memo block from GC free memo blocks list or allocate new one.
static HB_ERRCODE hb_fptGCgetFreeBlock(FPTAREAP pArea, LPMEMOGCTABLE pGCtable, HB_ULONG *ulOffset, HB_ULONG ulByteSize,
                                       bool fRaw)
{
  auto fAlloc = false;
  HB_ULONG ulSize;

  if (pArea->bMemoType == DB_MEMO_SMT || fRaw)
  {
    ulSize = (ulByteSize + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
  }
  else if (pArea->bMemoType == DB_MEMO_FPT)
  {
    ulSize = (ulByteSize + sizeof(FPTBLOCK) + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
  }
  else if (pArea->bMemoType == DB_MEMO_DBT)
  {
    ulSize = (ulByteSize + pArea->ulMemoBlockSize) / pArea->ulMemoBlockSize;
  }
  else
  {
    ulSize = (ulByteSize + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
  }

  for (auto i = 0; i < pGCtable->usItems; i++)
  {
    if (pGCtable->pGCitems[i].ulSize >= ulSize)
    {
      *ulOffset = pGCtable->pGCitems[i].ulOffset;
      pGCtable->pGCitems[i].ulOffset += ulSize;
      pGCtable->pGCitems[i].ulSize -= ulSize;
      if (pGCtable->pGCitems[i].ulSize == 0)
      {
        while (++i < pGCtable->usItems)
        {
          pGCtable->pGCitems[i - 1].ulOffset = pGCtable->pGCitems[i].ulOffset;
          pGCtable->pGCitems[i - 1].ulSize = pGCtable->pGCitems[i].ulSize;
        }
        pGCtable->usItems--;
      }
      else
      {
        pGCtable->pGCitems[i].fChanged = true;
        hb_fptSortGCitems(pGCtable);
      }
      pGCtable->bChanged |= 2;
      fAlloc = true;
      break;
    }
  }
  if (!fAlloc)
  {
    *ulOffset = pGCtable->ulNextBlock;
    pGCtable->ulNextBlock += ulSize;
    pGCtable->bChanged |= 1;
  }
  return Harbour::SUCCESS;
}

// Init GC table free memo block list.
static void hb_fptInitGCdata(LPMEMOGCTABLE pGCtable)
{
  memset(pGCtable, 0, sizeof(MEMOGCTABLE));
}

// Clean GC table free memo block list.
static void hb_fptDestroyGCdata(LPMEMOGCTABLE pGCtable)
{
  if (pGCtable->pGCitems != nullptr)
  {
    hb_xfree(pGCtable->pGCitems);
    pGCtable->pGCitems = nullptr;
    pGCtable->usItems = 0;
  }
  pGCtable->bChanged = 0;
}

// Read GC table from memo file.
static HB_ERRCODE hb_fptReadGCdata(FPTAREAP pArea, LPMEMOGCTABLE pGCtable)
{
  HB_SIZE nRead;

  hb_fptDestroyGCdata(pGCtable);
  memset(&pGCtable->fptHeader, 0, sizeof(FPTHEADER));

  nRead = hb_fileReadAt(pArea->pMemoFile, &pGCtable->fptHeader, sizeof(FPTHEADER), 0);
  if (nRead >= 512 && nRead != static_cast<HB_SIZE>(FS_ERROR))
  {
    int i;

    if (pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT)
    {
      pGCtable->ulNextBlock = HB_GET_LE_UINT32(pGCtable->fptHeader.nextBlock);
    }
    else
    {
      pGCtable->ulNextBlock = HB_GET_BE_UINT32(pGCtable->fptHeader.nextBlock);
    }
    pGCtable->ulPrevBlock = pGCtable->ulNextBlock;

    if (pArea->uiMemoVersion == DB_MEMOVER_SIX || pArea->bMemoType == DB_MEMO_SMT)
    {
      pGCtable->bType = DB_MEMOVER_SIX;
      pGCtable->usMaxItem = MAX_SIXFREEBLOCKS;
      pGCtable->usItems = HB_GET_LE_UINT16(pGCtable->fptHeader.nGCitems);
      if (pGCtable->usItems > pGCtable->usMaxItem)
      {
        return EDBF_CORRUPT;
      }

      pGCtable->pGCitems = static_cast<LPMEMOGCITEM>(hb_xgrab(sizeof(MEMOGCITEM) * (pGCtable->usMaxItem + 1)));

      for (i = 0; i < pGCtable->usItems; i++)
      {
        pGCtable->pGCitems[i].ulSize = HB_GET_LE_UINT16(&pGCtable->fptHeader.reserved2[i * 6]);
        pGCtable->pGCitems[i].ulOffset = HB_GET_LE_UINT32(&pGCtable->fptHeader.reserved2[i * 6 + 2]);
        pGCtable->pGCitems[i].fChanged = false;
      }
    }
    else if (pArea->bMemoType == DB_MEMO_FPT &&
             (pArea->uiMemoVersion == DB_MEMOVER_FLEX || pArea->uiMemoVersion == DB_MEMOVER_CLIP))
    {
      FPTBLOCK fptBlock;
      HB_BYTE *bPageBuf;

      pGCtable->bType = DB_MEMOVER_FLEX;
      pGCtable->usMaxItem = MAX_FLEXFREEBLOCKS;
      pGCtable->ulRevPage = HB_GET_LE_UINT32(pGCtable->fptHeader.flexRev);
      pGCtable->ulDirPage = HB_GET_LE_UINT32(pGCtable->fptHeader.flexDir);
      pGCtable->ulCounter = HB_GET_LE_UINT32(pGCtable->fptHeader.counter);
      if (pGCtable->ulDirPage)
      {
        if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), pGCtable->ulDirPage) != sizeof(FPTBLOCK) ||
            HB_GET_BE_UINT32(fptBlock.type) != FPTIT_FLEX_GC)
        {
          return EDBF_CORRUPT;
        }
        pGCtable->ulSize = HB_GET_BE_UINT32(fptBlock.size);
        bPageBuf = static_cast<HB_BYTE *>(hb_xgrab(pGCtable->ulSize));
        if (hb_fileReadAt(pArea->pMemoFile, bPageBuf, pGCtable->ulSize, pGCtable->ulDirPage + sizeof(FPTBLOCK)) !=
            pGCtable->ulSize)
        {
          hb_xfree(bPageBuf);
          return EDBF_CORRUPT;
        }
        pGCtable->usMaxItem = static_cast<HB_USHORT>((pGCtable->ulSize - 2) >> 3);
        pGCtable->usItems = (HB_GET_LE_UINT16(bPageBuf) - 3) >> 2;

        pGCtable->pGCitems = static_cast<LPMEMOGCITEM>(
            hb_xgrab(sizeof(MEMOGCITEM) * (HB_MIN(pGCtable->usItems, pGCtable->usMaxItem) + 1)));

        for (i = 0; i < pGCtable->usItems; i++)
        {
          pGCtable->pGCitems[i].ulOffset = HB_GET_LE_UINT32(&bPageBuf[i * 8 + 2]) / pArea->ulMemoBlockSize;
          pGCtable->pGCitems[i].ulSize = HB_GET_LE_UINT32(&bPageBuf[i * 8 + 6]) / pArea->ulMemoBlockSize;
          pGCtable->pGCitems[i].fChanged = false;
        }
        hb_xfree(bPageBuf);
      }
    }

    if (pGCtable->pGCitems)
    {
      hb_fptSortGCitems(pGCtable);
    }

    return Harbour::SUCCESS;
  }
  return EDBF_READ;
}

// Write GC table into memo file.
static HB_ERRCODE hb_fptWriteGCdata(FPTAREAP pArea, LPMEMOGCTABLE pGCtable)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pGCtable->bChanged > 0)
  {
    HB_ULONG ulHdrSize = 512;
    int i, j;

    if (pGCtable->bType == DB_MEMOVER_SIX)
    {
      HB_USHORT usItems = HB_MIN(pGCtable->usItems, pGCtable->usMaxItem);
      HB_PUT_LE_UINT16(pGCtable->fptHeader.nGCitems, usItems);
      memset(pGCtable->fptHeader.reserved2, 0, sizeof(pGCtable->fptHeader.reserved2));
      j = pGCtable->usItems - usItems;
      for (i = j; i < pGCtable->usItems; i++)
      {
        HB_PUT_LE_UINT16(&pGCtable->fptHeader.reserved2[(i - j) * 6],
                         (static_cast<HB_USHORT>(pGCtable->pGCitems[i].ulSize)));
        HB_PUT_LE_UINT32(&pGCtable->fptHeader.reserved2[(i - j) * 6 + 2], pGCtable->pGCitems[i].ulOffset);
      }
    }
    else if (pGCtable->bType == DB_MEMOVER_FLEX)
    {
      ulHdrSize = sizeof(FPTHEADER);
      pGCtable->ulCounter++;
      if (pGCtable->usItems == 0 && pGCtable->ulDirPage)
      {
        HB_ULONG ulOffset = pGCtable->ulDirPage;
        HB_ULONG ulSize = (pGCtable->ulSize + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
        if (pGCtable->ulRevPage)
        {
          ulSize <<= 1;
          if (pGCtable->ulDirPage > pGCtable->ulRevPage)
          {
            ulOffset = pGCtable->ulRevPage;
          }
        }
        ulOffset /= pArea->ulMemoBlockSize;
        if (ulOffset + ulSize == pGCtable->ulNextBlock)
        {
          pGCtable->ulDirPage = pGCtable->ulRevPage = 0;
          pGCtable->ulNextBlock -= ulSize;
        }
      }
      else if (pGCtable->usItems > 0 && !pGCtable->ulDirPage)
      {
        pGCtable->ulSize = FLEXGCPAGE_SIZE;
        errCode = hb_fptGCgetFreeBlock(pArea, pGCtable, &pGCtable->ulDirPage, pGCtable->ulSize, false);
        if (errCode == Harbour::SUCCESS)
        {
          pGCtable->ulDirPage *= pArea->ulMemoBlockSize;
          errCode = hb_fptGCgetFreeBlock(pArea, pGCtable, &pGCtable->ulRevPage, pGCtable->ulSize, false);
          pGCtable->ulRevPage *= pArea->ulMemoBlockSize;
        }
        pGCtable->bChanged |= 2;
      }
      if (pGCtable->ulDirPage && pGCtable->bChanged > 1)
      {
        FPTBLOCK fptBlock;
        HB_USHORT usItems = HB_MIN(pGCtable->usItems, pGCtable->usMaxItem);

        HB_PUT_BE_UINT32(fptBlock.type, FPTIT_FLEX_GC);
        HB_PUT_BE_UINT32(fptBlock.size, pGCtable->ulSize);
        auto bPageBuf = static_cast<HB_BYTE *>(hb_xgrab(pGCtable->ulSize));
        memset(bPageBuf, 0xAD, pGCtable->ulSize);
        HB_PUT_LE_UINT16(bPageBuf, (static_cast<HB_USHORT>(usItems) << 2) + 3);
        j = pGCtable->usItems - usItems;
        for (i = j; i < pGCtable->usItems; i++)
        {
          HB_PUT_LE_UINT32(&bPageBuf[(i - j) * 8 + 2], pGCtable->pGCitems[i].ulOffset * pArea->ulMemoBlockSize);
          HB_PUT_LE_UINT32(&bPageBuf[(i - j) * 8 + 6], pGCtable->pGCitems[i].ulSize * pArea->ulMemoBlockSize);
        }
        if (hb_fileWriteAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), pGCtable->ulDirPage) != sizeof(FPTBLOCK) ||
            hb_fileWriteAt(pArea->pMemoFile, bPageBuf, pGCtable->ulSize, pGCtable->ulDirPage + sizeof(FPTBLOCK)) !=
                pGCtable->ulSize)
        {
          errCode = EDBF_WRITE;
        }
        else if (pGCtable->ulRevPage)
        {
          for (i = j; i < pGCtable->usItems; i++)
          {
            HB_PUT_LE_UINT32(&bPageBuf[(i - j) * 8 + 2],
                             (static_cast<HB_USHORT>(pGCtable->pGCitems[i].ulSize) * pArea->ulMemoBlockSize));
            HB_PUT_LE_UINT32(&bPageBuf[(i - j) * 8 + 6], pGCtable->pGCitems[i].ulOffset * pArea->ulMemoBlockSize);
          }
          if (hb_fileWriteAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), pGCtable->ulRevPage) != sizeof(FPTBLOCK) ||
              hb_fileWriteAt(pArea->pMemoFile, bPageBuf, pGCtable->ulSize, pGCtable->ulRevPage + sizeof(FPTBLOCK)) !=
                  pGCtable->ulSize)
          {
            errCode = EDBF_WRITE;
          }
        }
        hb_xfree(bPageBuf);
      }
      HB_PUT_LE_UINT32(pGCtable->fptHeader.flexRev, pGCtable->ulRevPage);
      HB_PUT_LE_UINT32(pGCtable->fptHeader.flexDir, pGCtable->ulDirPage);
      HB_PUT_LE_UINT32(pGCtable->fptHeader.counter, pGCtable->ulCounter);
    }

    if (pGCtable->bChanged > 1 && errCode == Harbour::SUCCESS)
    {
      errCode = hb_fptWriteGCitems(pArea, pGCtable, 0);
    }
    if (errCode == Harbour::SUCCESS)
    {
      if (pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT)
      {
        HB_PUT_LE_UINT32(pGCtable->fptHeader.nextBlock, pGCtable->ulNextBlock);
      }
      else
      {
        HB_PUT_BE_UINT32(pGCtable->fptHeader.nextBlock, pGCtable->ulNextBlock);
      }
      if (hb_fileWriteAt(pArea->pMemoFile, &pGCtable->fptHeader, ulHdrSize, 0) != ulHdrSize)
      {
        errCode = EDBF_WRITE;
      }
      else if (pGCtable->ulNextBlock < pGCtable->ulPrevBlock)
      {
        // trunc file
        hb_fileTruncAt(pArea->pMemoFile, FPT_BLOCK_OFFSET(pGCtable->ulNextBlock));
      }
    }
    pArea->fMemoFlush = true;
    pGCtable->bChanged = 0;
  }
  return errCode;
}

// Return the size of memo.
static HB_ULONG hb_fptGetMemoLen(FPTAREAP pArea, HB_USHORT uiIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetMemoLen(%p, %hu)", static_cast<void*>(pArea), uiIndex));
#endif

  HB_ULONG ulBlock, ulSize, ulType;

  if (hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulSize, &ulType) == Harbour::SUCCESS)
  {
    if (ulBlock != 0)
    {
      if (ulSize == 0 && (pArea->bMemoType == DB_MEMO_DBT || pArea->bMemoType == DB_MEMO_FPT))
      {
        HB_FOFFSET fOffset = FPT_BLOCK_OFFSET(ulBlock);
        FPTBLOCK fptBlock;

        if (pArea->bMemoType == DB_MEMO_DBT)
        {
          HB_BYTE pBlock[DBT_DEFBLOCKSIZE];
          HB_SIZE n;

          do
          {
            HB_SIZE nLen = hb_fileReadAt(pArea->pMemoFile, pBlock, DBT_DEFBLOCKSIZE, fOffset);
            if (nLen == 0 || nLen == static_cast<HB_SIZE>(FS_ERROR))
            {
              break;
            }
            fOffset += nLen;
            n = 0;
            while (n < nLen && pBlock[n] != 0x1A)
            {
              n++;
            }
            ulSize += static_cast<HB_ULONGCAST>(n);
          } while (n == DBT_DEFBLOCKSIZE);
        }
        else if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), fOffset) == sizeof(FPTBLOCK))
        {
          ulSize = HB_GET_BE_UINT32(fptBlock.size);
        }
      }
      return ulSize;
    }
  }
  return 0;
}

// Return the type of memo.
static const char *hb_fptGetMemoType(FPTAREAP pArea, HB_USHORT uiIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetMemoType(%p, %hu)", static_cast<void*>(pArea), uiIndex));
#endif

  HB_ULONG ulBlock, ulSize, ulType;

  if (hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulSize, &ulType) == Harbour::SUCCESS)
  {
    if (ulBlock != 0)
    {
      if (ulType == 0 && pArea->bMemoType == DB_MEMO_FPT)
      {
        FPTBLOCK fptBlock;
        if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), FPT_BLOCK_OFFSET(ulBlock)) != sizeof(FPTBLOCK))
        {
          return "U";
        }
        ulType = HB_GET_BE_UINT32(fptBlock.type);
      }
    }

    if (ulType == 0)
    {
      return "M";
    }

    if (pArea->bMemoType == DB_MEMO_FPT)
    {
      switch (ulType)
      {
      case FPTIT_SIX_LNUM:
      case FPTIT_SIX_DNUM:
        return "N";
      case FPTIT_SIX_LDATE:
        return "D";
      case FPTIT_SIX_LOG:
        return "L";
      case FPTIT_SIX_CHAR:
        return "M";
      case FPTIT_SIX_ARRAY:
        return "A";
#if 0
            case FPTIT_SIX_BLOCK:
            case FPTIT_SIX_VREF:
            case FPTIT_SIX_MREF:
#endif
      case FPTIT_FLEX_ARRAY:
      case FPTIT_FLEX_VOARR:
        return "A";
      case FPTIT_FLEX_OBJECT:
      case FPTIT_FLEX_VOOBJ:
        return "O";
      case FPTIT_FLEX_NIL:
        return "U";
      case FPTIT_FLEX_TRUE:
      case FPTIT_FLEX_FALSE:
        return "L";
      case FPTIT_FLEX_LDATE:
        return "D";
      case FPTIT_FLEX_CHAR:
      case FPTIT_FLEX_UCHAR:
      case FPTIT_FLEX_SHORT:
      case FPTIT_FLEX_USHORT:
      case FPTIT_FLEX_LONG:
      case FPTIT_FLEX_ULONG:
      case FPTIT_FLEX_DOUBLE:
      case FPTIT_FLEX_LDOUBLE:
        return "N";
      case FPTIT_TEXT:
        return "M";
      case FPTIT_PICT:
      case FPTIT_FLEX_COMPRCH:
        return "C";
      }
      return "U";
    }
    else if (pArea->bMemoType == DB_MEMO_SMT)
    {
      switch (ulType)
      {
      case SMT_IT_NIL:
        return "U";
      case SMT_IT_CHAR:
        return "M";
      case SMT_IT_INT:
      case SMT_IT_DOUBLE:
        return "N";
      case SMT_IT_DATE:
        return "D";
      case SMT_IT_LOGICAL:
        return "L";
      case SMT_IT_ARRAY:
        return "A";
      }
      return "U";
    }
    return "M";
  }

  return "U";
}

// Calculate the size of SMT memo item
static HB_ULONG hb_fptCountSMTItemLength(FPTAREAP pArea, PHB_ITEM pItem, HB_ULONG *pulArrayCount, int iTrans)
{
  HB_ULONG ulLen, ulSize;

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY: // Harbour::Item::OBJECT == Harbour::Item::ARRAY
    (*pulArrayCount)++;
    ulSize = 3;
    ulLen = static_cast<HB_ULONGCAST>(hb_arrayLen(pItem));
    if (ulLen > 0xFFFF)
    {
      ulLen = 0xFFFF;
    }
    for (HB_ULONG u = 1; u <= ulLen; u++)
    {
      ulSize += hb_fptCountSMTItemLength(pArea, hb_arrayGetItemPtr(pItem, u), pulArrayCount, iTrans);
    }
    break;
  case Harbour::Item::MEMO:
  case Harbour::Item::STRING:
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulLen =
          static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, 0xFFFF)) * sizeof(HB_WCHAR);
    }
    else
    {
      ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
      if (iTrans == FPT_TRANS_CP && ulLen > 0)
      {
        ulLen =
            static_cast<HB_ULONGCAST>(hb_cdpnDup2Len(pItem->getCPtr(), ulLen, 0xFFFF, hb_vmCDP(), pArea->area.cdPage));
      }
      else
      {
        if (ulLen > 0xFFFF)
        {
          ulLen = 0xFFFF;
        }
      }
    }
    ulSize = ulLen + 3;
    break;
  case Harbour::Item::LOGICAL:
    ulSize = 2;
    break;
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
    ulSize = 5;
    break;
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  {
    HB_MAXINT iVal = pItem->getNInt();
    if (HB_LIM_INT32(iVal))
    {
      ulSize = 5;
      break;
    }
  }
  // fallthrough
  case Harbour::Item::DOUBLE:
    ulSize = 11;
    break;
  case Harbour::Item::NIL:
  default:
    ulSize = 1;
  }
  return ulSize;
}

// Calculate the size of SMT memo data
static HB_ERRCODE hb_fptCountSMTDataLength(FPTAREAP pArea, HB_FOFFSET *pfOffset)
{
  HB_USHORT uiSize;
  HB_BYTE buffer[2];

  if (hb_fileReadAt(pArea->pMemoFile, buffer, 1, *pfOffset) != 1)
  {
    return EDBF_READ;
  }

  *pfOffset += 1;
  switch (buffer[0])
  {
  case SMT_IT_ARRAY:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 2, *pfOffset) != 2)
    {
      return EDBF_READ;
    }
    *pfOffset += 2;
    uiSize = HB_GET_LE_UINT16(buffer);
    for (HB_USHORT u = 0; u < uiSize; u++)
    {
      HB_ERRCODE errCode = hb_fptCountSMTDataLength(pArea, pfOffset);
      if (errCode != Harbour::SUCCESS)
      {
        return errCode;
      }
    }
    break;

  case SMT_IT_CHAR:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 2, *pfOffset) != 2)
    {
      return EDBF_READ;
    }
    uiSize = HB_GET_LE_UINT16(buffer);
    *pfOffset += uiSize + 2;
    break;

  case SMT_IT_INT:
  case SMT_IT_DATE:
    *pfOffset += 4;
    break;

  case SMT_IT_DOUBLE:
    *pfOffset += 10;
    break;

  case SMT_IT_LOGICAL:
    *pfOffset += 1;
    break;

  case SMT_IT_NIL:
    break;

  default:
    return EDBF_CORRUPT;
  }

  return Harbour::SUCCESS;
}

// Write VM item as SMT memos.
static void hb_fptStoreSMTItem(FPTAREAP pArea, PHB_ITEM pItem, HB_BYTE **bBufPtr, int iTrans)
{
  HB_ULONG ulLen, u;

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY:
    *(*bBufPtr)++ = SMT_IT_ARRAY;
    ulLen = static_cast<HB_ULONGCAST>(hb_arrayLen(pItem));
    if (ulLen > 0xFFFF)
    {
      ulLen = 0xFFFF;
    }
    HB_PUT_LE_UINT16(*bBufPtr, ulLen);
    *bBufPtr += 2;
    for (u = 1; u <= ulLen; u++)
    {
      hb_fptStoreSMTItem(pArea, hb_arrayGetItemPtr(pItem, u), bBufPtr, iTrans);
    }
    break;

  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    *(*bBufPtr)++ = SMT_IT_CHAR;
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulLen = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, 0xFFFF));
      ulLen = static_cast<HB_ULONGCAST>(
          hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<HB_WCHAR *>(*bBufPtr) + 2, ulLen));
      ulLen *= sizeof(HB_WCHAR);
    }
    else
    {
      ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
      if (ulLen > 0)
      {
        u = 0xFFFF;
        if (iTrans == FPT_TRANS_CP)
        {
          HB_SIZE nSize = u;
          hb_cdpnDup2(pItem->getCPtr(), ulLen, reinterpret_cast<char *>(*bBufPtr) + 2, &nSize, hb_vmCDP(),
                      pArea->area.cdPage);
          ulLen = static_cast<HB_ULONG>(nSize);
        }
        else
        {
          if (ulLen > u)
          {
            ulLen = u;
          }
          memcpy(*bBufPtr + 2, pItem->getCPtr(), ulLen);
        }
      }
    }
    HB_PUT_LE_UINT16(*bBufPtr, ulLen);
    *bBufPtr += ulLen + 2;
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  {
    HB_MAXINT iVal = pItem->getNInt();
    if (HB_LIM_INT32(iVal))
    {
      *(*bBufPtr)++ = SMT_IT_INT;
      HB_PUT_LE_UINT32(*bBufPtr, iVal);
      *bBufPtr += 4;
      break;
    }
  }
  // fallthrough
  case Harbour::Item::DOUBLE:
  {
    auto dVal = pItem->getND();
    int iWidth, iDec;
    hb_itemGetNLen(pItem, &iWidth, &iDec);
    if (iDec)
    {
      iWidth += iDec + 1;
    }
    *(*bBufPtr)++ = SMT_IT_DOUBLE;
    *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
    *(*bBufPtr)++ = static_cast<HB_BYTE>(iDec);
    HB_PUT_LE_DOUBLE(*bBufPtr, dVal);
    *bBufPtr += 8;
    break;
  }
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
  {
    HB_LONG lVal;
    *(*bBufPtr)++ = SMT_IT_DATE;
    lVal = pItem->getDL();
    HB_PUT_LE_UINT32(*bBufPtr, lVal);
    *bBufPtr += 4;
    break;
  }
  case Harbour::Item::LOGICAL:
    *(*bBufPtr)++ = SMT_IT_LOGICAL;
    *(*bBufPtr)++ = pItem->getL() ? 1 : 0;
    break;

  case Harbour::Item::NIL:
  default:
    *(*bBufPtr)++ = SMT_IT_NIL;
    break;
  }
}

// Read SMT item from file
static HB_ERRCODE hb_fptReadRawSMTItem(FPTAREAP pArea, PHB_ITEM pItem, HB_FOFFSET *pfOffset, int iTrans)
{
  HB_ULONG ulLen, u;
  HB_BYTE buffer[10];
  char *pBuffer;

  if (hb_fileReadAt(pArea->pMemoFile, buffer, 1, *pfOffset) != 1)
  {
    return EDBF_READ;
  }

  *pfOffset += 1;
  switch (buffer[0])
  {
  case SMT_IT_ARRAY:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 2, *pfOffset) != 2)
    {
      return EDBF_READ;
    }

    *pfOffset += 2;
    ulLen = HB_GET_LE_UINT16(buffer);
    hb_arrayNew(pItem, ulLen);
    for (u = 1; u <= ulLen; u++)
    {
      HB_ERRCODE errCode = hb_fptReadRawSMTItem(pArea, hb_arrayGetItemPtr(pItem, u), pfOffset, iTrans);
      if (errCode != Harbour::SUCCESS)
      {
        return errCode;
      }
    }
    break;

  case SMT_IT_CHAR:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 2, *pfOffset) != 2)
    {
      return EDBF_READ;
    }
    *pfOffset += 2;
    ulLen = HB_GET_LE_UINT16(buffer);
    pBuffer = static_cast<char *>(hb_xgrab(ulLen + 1));
    if (ulLen > 0 && hb_fileReadAt(pArea->pMemoFile, pBuffer, ulLen, *pfOffset) != ulLen)
    {
      hb_xfree(pBuffer);
      return EDBF_READ;
    }
    *pfOffset += ulLen;
    if (iTrans == FPT_TRANS_UNICODE)
    {
      hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pBuffer), ulLen >> 1);
      hb_xfree(pBuffer);
    }
    else
    {
      if (iTrans == FPT_TRANS_CP && ulLen > 0)
      {
        HB_SIZE nSize = ulLen + 1;
        HB_SIZE nLen = ulLen;
        hb_cdpnDup3(pBuffer, ulLen, pBuffer, &nLen, &pBuffer, &nSize, pArea->area.cdPage, hb_vmCDP());
        ulLen = static_cast<HB_ULONG>(nLen);
      }
      hb_itemPutCLPtr(pItem, pBuffer, ulLen);
    }
    break;

  case SMT_IT_INT:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 4, *pfOffset) != 4)
    {
      return EDBF_READ;
    }
    *pfOffset += 4;
    hb_itemPutNInt(pItem, static_cast<HB_LONG>(HB_GET_LE_UINT32(buffer)));
    break;

  case SMT_IT_DOUBLE:
  {
    int iWidth, iDec;
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 10, *pfOffset) != 10)
    {
      return EDBF_READ;
    }
    *pfOffset += 10;
    iWidth = buffer[0];
    iDec = buffer[1];
    if (iDec)
    {
      iWidth -= iDec + 1;
    }
    hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(&buffer[2]), iWidth, iDec);
    break;
  }
  case SMT_IT_DATE:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 4, *pfOffset) != 4)
    {
      return EDBF_READ;
    }
    *pfOffset += 4;
    hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_UINT32(buffer)));
    break;

  case SMT_IT_LOGICAL:
    if (hb_fileReadAt(pArea->pMemoFile, buffer, 1, *pfOffset) != 1)
    {
      return EDBF_READ;
    }
    *pfOffset += 1;
    hb_itemPutL(pItem, buffer[0] != 0);
    break;

  case SMT_IT_NIL:
    hb_itemClear(pItem);
    break;

  default:
    hb_itemClear(pItem);
    return EDBF_CORRUPT;
  }

  return Harbour::SUCCESS;
}

// Read SMT item from memory buffer.
static HB_ERRCODE hb_fptReadSMTItem(FPTAREAP pArea, HB_BYTE **pbMemoBuf, HB_BYTE *bBufEnd, PHB_ITEM pItem, int iTrans)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (bBufEnd - (*pbMemoBuf) >= 1)
  {
    HB_ULONG ulLen, u;

    switch (*(*pbMemoBuf)++)
    {
    case SMT_IT_ARRAY:
      if (bBufEnd - (*pbMemoBuf) < 2)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      ulLen = HB_GET_LE_UINT16(*pbMemoBuf);
      *pbMemoBuf += 2;
      if (bBufEnd - (*pbMemoBuf) < static_cast<HB_LONG>(ulLen))
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      hb_arrayNew(pItem, ulLen);
      for (u = 1; u <= ulLen; u++)
      {
        errCode = hb_fptReadSMTItem(pArea, pbMemoBuf, bBufEnd, hb_arrayGetItemPtr(pItem, u), iTrans);
        if (errCode != Harbour::SUCCESS)
        {
          break;
        }
      }
      break;

    case SMT_IT_CHAR:
      if (bBufEnd - (*pbMemoBuf) < 2)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      ulLen = HB_GET_LE_UINT16(*pbMemoBuf);
      *pbMemoBuf += 2;
      if (bBufEnd - (*pbMemoBuf) < static_cast<HB_LONG>(ulLen))
      {
        errCode = EDBF_CORRUPT;
      }
      else
      {
        char *pszStr = reinterpret_cast<char *>(*pbMemoBuf);
        *pbMemoBuf += ulLen;
        if (iTrans == FPT_TRANS_UNICODE)
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pszStr), ulLen >> 1);
        }
        else if (iTrans == FPT_TRANS_CP && ulLen != 0)
        {
          HB_SIZE nLen = ulLen;
          pszStr = hb_cdpnDup(pszStr, &nLen, pArea->area.cdPage, hb_vmCDP());
          ulLen = static_cast<HB_ULONG>(nLen);
          hb_itemPutCLPtr(pItem, pszStr, ulLen);
        }
        else
        {
          hb_itemPutCL(pItem, pszStr, ulLen);
        }
      }
      break;

    case SMT_IT_INT:
      if (bBufEnd - (*pbMemoBuf) < 4)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      hb_itemPutNInt(pItem, static_cast<HB_LONG>(HB_GET_LE_UINT32(*pbMemoBuf)));
      *pbMemoBuf += 4;
      break;

    case SMT_IT_DOUBLE:
    {
      int iWidth, iDec;
      if (bBufEnd - (*pbMemoBuf) < 10)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      iWidth = *(*pbMemoBuf)++;
      iDec = *(*pbMemoBuf)++;
      if (iDec)
      {
        iWidth -= iDec + 1;
      }
      hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(*pbMemoBuf), iWidth, iDec);
      *pbMemoBuf += 8;
      break;
    }
    case SMT_IT_DATE:
      if (bBufEnd - (*pbMemoBuf) < 4)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_UINT32(*pbMemoBuf)));
      *pbMemoBuf += 4;
      break;

    case SMT_IT_LOGICAL:
      if (bBufEnd - (*pbMemoBuf) < 1)
      {
        errCode = EDBF_CORRUPT;
        break;
      }
      hb_itemPutL(pItem, *(*pbMemoBuf)++ != 0);
      break;

    case SMT_IT_NIL:
      hb_itemClear(pItem);
      break;

    default:
      hb_itemClear(pItem);
      errCode = EDBF_CORRUPT;
      break;
    }
  }
  else
  {
    errCode = EDBF_CORRUPT;
  }

  return errCode;
}

// Calculate the size of SIX memo item
static HB_ULONG hb_fptCountSixItemLength(FPTAREAP pArea, PHB_ITEM pItem, HB_ULONG *pulArrayCount, int iTrans)
{
  HB_ULONG ulLen, u, ulSize;

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY: // Harbour::Item::OBJECT == Harbour::Item::ARRAY
    (*pulArrayCount)++;
    ulSize = SIX_ITEM_BUFSIZE;
    ulLen = static_cast<HB_ULONGCAST>(hb_arrayLen(pItem));
    if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
    {
      // only 2 bytes (HB_SHORT) for SIX compatibility
      ulLen = HB_MIN(ulLen, 0xFFFF);
    }
    for (u = 1; u <= ulLen; u++)
    {
      ulSize += hb_fptCountSixItemLength(pArea, hb_arrayGetItemPtr(pItem, u), pulArrayCount, iTrans);
    }
    break;
  case Harbour::Item::MEMO:
  case Harbour::Item::STRING:
    ulSize = SIX_ITEM_BUFSIZE;
    // only 2 bytes (HB_SHORT) for SIX compatibility
    u = pArea->uiMemoVersion == DB_MEMOVER_SIX ? 0xFFFF : ULONG_MAX;
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulLen = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, u)) * sizeof(HB_WCHAR);
    }
    else
    {
      ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
      if (iTrans == FPT_TRANS_CP && ulLen > 0)
      {
        ulLen = static_cast<HB_ULONGCAST>(hb_cdpnDup2Len(pItem->getCPtr(), ulLen, u, hb_vmCDP(), pArea->area.cdPage));
      }
      else
      {
        if (ulLen > u)
        {
          ulLen = u;
        }
      }
    }
    ulSize += ulLen;
    break;
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
  case Harbour::Item::LOGICAL:
  default:
    ulSize = SIX_ITEM_BUFSIZE;
  }
  return ulSize;
}

// Write fpt vartype as SIX memos.
static HB_ULONG hb_fptStoreSixItem(FPTAREAP pArea, PHB_ITEM pItem, HB_BYTE **bBufPtr, int iTrans)
{
  HB_ULONG ulLen, u, ulSize;
  int iWidth, iDec;

  memset(*bBufPtr, '\0', SIX_ITEM_BUFSIZE);
  ulSize = SIX_ITEM_BUFSIZE;
  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY: // Harbour::Item::OBJECT == Harbour::Item::ARRAY
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_ARRAY);
    ulLen = static_cast<HB_ULONGCAST>(hb_arrayLen(pItem));
    if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
    {
      // only 2 bytes (HB_SHORT) for SIX compatibility
      ulLen = HB_MIN(ulLen, 0xFFFF);
    }
    HB_PUT_LE_UINT32(&(*bBufPtr)[2], ulLen);
    *bBufPtr += SIX_ITEM_BUFSIZE;
    for (u = 1; u <= ulLen; u++)
    {
      ulSize += hb_fptStoreSixItem(pArea, hb_arrayGetItemPtr(pItem, u), bBufPtr, iTrans);
    }
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  {
    HB_MAXINT iVal = pItem->getNInt();
    hb_itemGetNLen(pItem, &iWidth, &iDec);
    if (HB_LIM_INT32(iVal))
    {
      HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_LNUM);
      HB_PUT_LE_UINT16(&(*bBufPtr)[2], iWidth);
      HB_PUT_LE_UINT16(&(*bBufPtr)[4], iDec);
      HB_PUT_LE_UINT32(&(*bBufPtr)[6], iVal);
      *bBufPtr += SIX_ITEM_BUFSIZE;
    }
    else
    {
      HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_DNUM);
      HB_PUT_LE_UINT16(&(*bBufPtr)[2], iWidth);
      HB_PUT_LE_UINT16(&(*bBufPtr)[4], iDec);
      HB_PUT_LE_DOUBLE(&(*bBufPtr)[6], static_cast<double>(iVal));
      *bBufPtr += SIX_ITEM_BUFSIZE;
    }
    break;
  }
  case Harbour::Item::DOUBLE:
  {
    auto dVal = pItem->getND();
    hb_itemGetNLen(pItem, &iWidth, &iDec);
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_DNUM);
    HB_PUT_LE_UINT16(&(*bBufPtr)[2], iWidth);
    HB_PUT_LE_UINT16(&(*bBufPtr)[4], iDec);
    HB_PUT_LE_DOUBLE(&(*bBufPtr)[6], dVal);
    *bBufPtr += SIX_ITEM_BUFSIZE;
    break;
  }
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
  {
    HB_LONG lVal = pItem->getDL();
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_LDATE);
    HB_PUT_LE_UINT32(&(*bBufPtr)[6], lVal);
    *bBufPtr += SIX_ITEM_BUFSIZE;
    break;
  }
  case Harbour::Item::LOGICAL:
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_LOG);
    (*bBufPtr)[6] = pItem->getL() ? 1 : 0;
    *bBufPtr += SIX_ITEM_BUFSIZE;
    break;

  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_CHAR);
    // only 2 bytes (HB_SHORT) for SIX compatibility
    u = pArea->uiMemoVersion == DB_MEMOVER_SIX ? 0xFFFF : ULONG_MAX;
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulLen = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, u));
      ulLen = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(
          pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<HB_WCHAR *>(*bBufPtr) + SIX_ITEM_BUFSIZE, ulLen));
      ulLen *= sizeof(HB_WCHAR);
    }
    else
    {
      ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
      if (ulLen > 0)
      {
        if (iTrans == FPT_TRANS_CP)
        {
          HB_SIZE nSize = u;
          hb_cdpnDup2(pItem->getCPtr(), ulLen, reinterpret_cast<char *>(*bBufPtr) + SIX_ITEM_BUFSIZE, &nSize,
                      hb_vmCDP(), pArea->area.cdPage);
          ulLen = static_cast<HB_ULONG>(nSize);
        }
        else
        {
          if (ulLen > u)
          {
            ulLen = u;
          }
          memcpy(*bBufPtr + SIX_ITEM_BUFSIZE, pItem->getCPtr(), ulLen);
        }
      }
    }
    HB_PUT_LE_UINT32(&(*bBufPtr)[2], ulLen);
    *bBufPtr += ulLen + SIX_ITEM_BUFSIZE;
    break;
  default:
    HB_PUT_LE_UINT16(&(*bBufPtr)[0], FPTIT_SIX_NIL);
    *bBufPtr += SIX_ITEM_BUFSIZE;
    break;
  }
  return ulSize;
}

// Read SIX item from memo.
static HB_ERRCODE hb_fptReadSixItem(FPTAREAP pArea, HB_BYTE **pbMemoBuf, HB_BYTE *bBufEnd, PHB_ITEM pItem, int iTrans)
{
  HB_ULONG ulLen, u;
  HB_ERRCODE errCode = Harbour::SUCCESS;

  ulLen = SIX_ITEM_BUFSIZE;
  if (bBufEnd - (*pbMemoBuf) >= static_cast<HB_LONG>(ulLen))
  {
    HB_USHORT usType = HB_GET_LE_UINT16(&(*pbMemoBuf)[0]);
    switch (usType)
    {
    case FPTIT_SIX_LNUM:
      hb_itemPutNL(pItem, static_cast<long>(HB_GET_LE_UINT32(&(*pbMemoBuf)[6])));
      break;

    case FPTIT_SIX_DNUM:
      hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(&(*pbMemoBuf)[6]), HB_GET_LE_UINT16(&(*pbMemoBuf)[2]),
                      HB_GET_LE_UINT16(&(*pbMemoBuf)[4]));
      break;

    case FPTIT_SIX_LDATE:
      hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_UINT32(&(*pbMemoBuf)[6])));
      break;

    case FPTIT_SIX_LOG:
      hb_itemPutL(pItem, HB_GET_LE_UINT16(&(*pbMemoBuf)[6]) != 0);
      break;

    case FPTIT_SIX_CHAR:
      ulLen = HB_GET_LE_UINT32(&(*pbMemoBuf)[2]);
      if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
      {
        ulLen &= 0xFFFF; // only 2 bytes (HB_SHORT) for SIX compatibility
      }
      (*pbMemoBuf) += SIX_ITEM_BUFSIZE;
      if (bBufEnd - (*pbMemoBuf) >= static_cast<HB_LONG>(ulLen))
      {
        char *pszStr = reinterpret_cast<char *>(*pbMemoBuf);

        if (iTrans == FPT_TRANS_UNICODE)
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pszStr), ulLen >> 1);
        }
        else
        {
          if (iTrans == FPT_TRANS_CP && ulLen > 0)
          {
            HB_SIZE nSize = ulLen;
            pszStr = hb_cdpnDup(pszStr, &nSize, pArea->area.cdPage, hb_vmCDP());
            hb_itemPutCLPtr(pItem, pszStr, nSize);
          }
          else
          {
            hb_itemPutCL(pItem, pszStr, ulLen);
          }
        }
      }
      else
      {
        errCode = EDBF_CORRUPT;
      }

      break;
#if 0
         case FPTIT_SIX_BLOCK:
         case FPTIT_SIX_VREF:
         case FPTIT_SIX_MREF:
#endif
    case FPTIT_SIX_ARRAY:
      ulLen = HB_GET_LE_UINT32(&(*pbMemoBuf)[2]);
      if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
      {
        ulLen &= 0xFFFF; // only 2 bytes (HB_SHORT) for SIX compatibility
      }
      (*pbMemoBuf) += SIX_ITEM_BUFSIZE;
      hb_arrayNew(pItem, ulLen);
      for (u = 1; u <= ulLen; u++)
      {
        errCode = hb_fptReadSixItem(pArea, pbMemoBuf, bBufEnd, hb_arrayGetItemPtr(pItem, u), iTrans);
        if (errCode != Harbour::SUCCESS)
        {
          break;
        }
      }
      ulLen = 0;
      break;

    case FPTIT_SIX_NIL:
      hb_itemClear(pItem);
      break;

    default:
      errCode = EDBF_CORRUPT;
      hb_itemClear(pItem);
      break;
    }
    *pbMemoBuf += ulLen;
  }
  else
  {
    errCode = EDBF_CORRUPT;
  }

  return errCode;
}

// Calculate the size of FLEX memo item
static HB_ULONG hb_fptCountFlexItemLength(FPTAREAP pArea, PHB_ITEM pItem, HB_ULONG *pulArrayCount, int iTrans)
{
  HB_ULONG ulLen, u, ulSize = 1;
  HB_MAXINT iVal;

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY:
    (*pulArrayCount)++;
    ulSize += 2;
    ulLen = hb_arrayLen(pItem) & 0xFFFF;
    for (u = 1; u <= ulLen; u++)
    {
      ulSize += hb_fptCountFlexItemLength(pArea, hb_arrayGetItemPtr(pItem, u), pulArrayCount, iTrans);
    }
    break;
  case Harbour::Item::MEMO:
  case Harbour::Item::STRING:
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulLen =
          static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, 0xFFFF)) * sizeof(HB_WCHAR);
    }
    else
    {
      ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
      if (iTrans == FPT_TRANS_CP && ulLen > 0)
      {
        ulLen =
            static_cast<HB_ULONGCAST>(hb_cdpnDup2Len(pItem->getCPtr(), ulLen, 0xFFFF, hb_vmCDP(), pArea->area.cdPage));
      }
      else
      {
        if (ulLen > 0xFFFF)
        {
          ulLen = 0xFFFF;
        }
      }
    }
    if (ulLen > 0)
    {
      ulSize += ulLen + 2;
    }
    break;
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
    ulSize += 4;
    break;
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
    iVal = pItem->getNInt();
    ulSize += (HB_LIM_INT8(iVal) ? 2 : (HB_LIM_INT16(iVal) ? 3 : (HB_LIM_INT32(iVal) ? 5 : 10)));
    break;
  case Harbour::Item::DOUBLE:
    ulSize += 10;
    break;
  }
  return ulSize;
}

// Store in buffer fpt vartype as FLEX memos.
static void hb_fptStoreFlexItem(FPTAREAP pArea, PHB_ITEM pItem, HB_BYTE **bBufPtr, int iTrans)
{
  HB_ULONG ulLen, u;
  int iWidth, iDec;

  switch (hb_itemType(pItem))
  {
  case Harbour::Item::ARRAY:
    ulLen = hb_arrayLen(pItem) & 0xFFFF;
    *(*bBufPtr)++ = FPTIT_FLEXAR_ARAY;
    HB_PUT_LE_UINT16(*bBufPtr, static_cast<HB_USHORT>(ulLen));
    *bBufPtr += 2;
    for (u = 1; u <= ulLen; u++)
    {
      hb_fptStoreFlexItem(pArea, hb_arrayGetItemPtr(pItem, u), bBufPtr, iTrans);
    }
    break;
  case Harbour::Item::MEMO:
  case Harbour::Item::STRING:
    ulLen = static_cast<HB_ULONGCAST>(pItem->getCLen());
    if (ulLen == 0)
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_NUL;
    }
    else
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_STR;
      u = 0xFFFF;
      if (iTrans == FPT_TRANS_UNICODE)
      {
        ulLen = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, u));
        ulLen = static_cast<HB_ULONGCAST>(
            hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<HB_WCHAR *>(*bBufPtr) + 2, ulLen));
        ulLen *= sizeof(HB_WCHAR);
      }
      else if (iTrans == FPT_TRANS_CP)
      {
        HB_SIZE nSize = u;
        hb_cdpnDup2(pItem->getCPtr(), ulLen, reinterpret_cast<char *>(*bBufPtr) + 2, &nSize, hb_vmCDP(),
                    pArea->area.cdPage);
        ulLen = static_cast<HB_ULONG>(nSize);
      }
      else
      {
        if (ulLen > u)
        {
          ulLen = u;
        }
        memcpy(*bBufPtr + 2, pItem->getCPtr(), ulLen);
      }
      HB_PUT_LE_UINT16(*bBufPtr, static_cast<HB_USHORT>(ulLen));
      *bBufPtr += ulLen + 2;
    }
    break;
  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
  {
    HB_LONG lVal;
    *(*bBufPtr)++ = FPTIT_FLEXAR_DATEJ;
    lVal = pItem->getDL();
    HB_PUT_LE_UINT32(*bBufPtr, lVal);
    *bBufPtr += 4;
    break;
  }
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  {
    HB_MAXINT iVal = pItem->getNInt();
    hb_itemGetNLen(pItem, &iWidth, &iDec);
    if (HB_LIM_INT8(iVal))
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_CHAR1;
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iVal);
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
    }
    else if (HB_LIM_INT16(iVal))
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_SHORT1;
      HB_PUT_LE_UINT16(*bBufPtr, iVal);
      *bBufPtr += 2;
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
    }
    else if (HB_LIM_INT32(iVal))
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_LONG1;
      HB_PUT_LE_UINT32(*bBufPtr, iVal);
      *bBufPtr += 4;
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
    }
    else
    {
      *(*bBufPtr)++ = FPTIT_FLEXAR_DOUBLE2;
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
      *(*bBufPtr)++ = static_cast<HB_BYTE>(iDec);
      HB_PUT_LE_DOUBLE(*bBufPtr, static_cast<double>(iVal));
      *bBufPtr += 8;
    }
    break;
  }
  case Harbour::Item::DOUBLE:
  {
    auto dVal = pItem->getND();
    hb_itemGetNLen(pItem, &iWidth, &iDec);
    if (iDec)
    {
      iWidth += iDec + 1;
    }
    *(*bBufPtr)++ = FPTIT_FLEXAR_DOUBLE2;
    *(*bBufPtr)++ = static_cast<HB_BYTE>(iWidth);
    *(*bBufPtr)++ = static_cast<HB_BYTE>(iDec);
    HB_PUT_LE_DOUBLE(*bBufPtr, dVal);
    *bBufPtr += 8;
    break;
  }
  case Harbour::Item::LOGICAL:
    *(*bBufPtr)++ = pItem->getL() ? FPTIT_FLEXAR_TRUE : FPTIT_FLEXAR_FALSE;
    break;
  case Harbour::Item::NIL:
  default:
    *(*bBufPtr)++ = FPTIT_FLEXAR_NIL;
  }
}

// Read FLEX item from memo.
static HB_ERRCODE hb_fptReadFlexItem(FPTAREAP pArea, HB_BYTE **pbMemoBuf, HB_BYTE *bBufEnd, PHB_ITEM pItem, bool bRoot,
                                     int iTrans)
{
  HB_BYTE usType;
  HB_ULONG ulLen, i;
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (bRoot)
  {
    usType = FPTIT_FLEXAR_ARAY;
  }
  else if (bBufEnd - (*pbMemoBuf) > 0)
  {
    usType = *(*pbMemoBuf)++;
  }
  else
  {
    return EDBF_CORRUPT;
  }

  switch (usType)
  {
  case FPTIT_FLEXAR_NIL:
    hb_itemClear(pItem);
    break;
  case FPTIT_FLEXAR_TRUE:
    hb_itemPutL(pItem, true);
    break;
  case FPTIT_FLEXAR_FALSE:
    hb_itemPutL(pItem, false);
    break;
  case FPTIT_FLEXAR_LOGIC:
    if (bBufEnd - (*pbMemoBuf) >= 1)
    {
      hb_itemPutL(pItem, *(*pbMemoBuf)++ != 0);
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_DATEJ:
  case FPTIT_FLEXAR_DATEX:
    if (bBufEnd - (*pbMemoBuf) >= 4)
    {
      hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_UINT32(*pbMemoBuf)));
      *pbMemoBuf += 4;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_CHAR:
    if (bBufEnd - (*pbMemoBuf) >= 1)
    {
      hb_itemPutNI(pItem, static_cast<signed char>(*(*pbMemoBuf)++));
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_CHAR1:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      hb_itemPutNILen(pItem, static_cast<signed char>(**pbMemoBuf), (*pbMemoBuf)[1]);
      *pbMemoBuf += 2;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_CHAR2:
    if (bBufEnd - (*pbMemoBuf) >= 3)
    {
      int iLen = (*pbMemoBuf)[1], iDec = (*pbMemoBuf)[2];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<signed char>(**pbMemoBuf), iLen, iDec);
      }
      else
      {
        hb_itemPutNILen(pItem, static_cast<signed char>(**pbMemoBuf), iLen);
      }
      *pbMemoBuf += 3;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_UCHAR:
    if (bBufEnd - (*pbMemoBuf) >= 1)
    {
      hb_itemPutNI(pItem, static_cast<unsigned char>(*(*pbMemoBuf)++));
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_UCHAR1:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      hb_itemPutNILen(pItem, static_cast<unsigned char>(**pbMemoBuf), (*pbMemoBuf)[1]);
      *pbMemoBuf += 2;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_UCHAR2:
    if (bBufEnd - (*pbMemoBuf) >= 3)
    {
      int iLen = (*pbMemoBuf)[1], iDec = (*pbMemoBuf)[2];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<unsigned char>(**pbMemoBuf), iLen, iDec);
      }
      else
      {
        hb_itemPutNILen(pItem, static_cast<unsigned char>(**pbMemoBuf), iLen);
      }
      *pbMemoBuf += 3;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_SHORT:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      hb_itemPutNI(pItem, static_cast<HB_SHORT>(HB_GET_LE_UINT16(*pbMemoBuf)));
      *pbMemoBuf += 2;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_SHORT1:
    if (bBufEnd - (*pbMemoBuf) >= 3)
    {
      hb_itemPutNILen(pItem, static_cast<HB_SHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), (*pbMemoBuf)[2]);
      *pbMemoBuf += 3;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_SHORT2:
    if (bBufEnd - (*pbMemoBuf) >= 4)
    {
      int iLen = (*pbMemoBuf)[2], iDec = (*pbMemoBuf)[3];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<HB_SHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), iLen, iDec);
      }
      else
      {
        hb_itemPutNILen(pItem, static_cast<HB_SHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), iLen);
      }
      *pbMemoBuf += 4;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_USHORT:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      hb_itemPutNInt(pItem, static_cast<HB_USHORT>(HB_GET_LE_UINT16(*pbMemoBuf)));
      *pbMemoBuf += 2;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_USHORT1:
    if (bBufEnd - (*pbMemoBuf) >= 3)
    {
      hb_itemPutNIntLen(pItem, static_cast<HB_USHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), (*pbMemoBuf)[2]);
      *pbMemoBuf += 3;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_USHORT2:
    if (bBufEnd - (*pbMemoBuf) >= 4)
    {
      int iLen = (*pbMemoBuf)[2], iDec = (*pbMemoBuf)[3];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<HB_USHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), iLen, iDec);
      }
      else
      {
        hb_itemPutNIntLen(pItem, static_cast<HB_USHORT>(HB_GET_LE_UINT16(*pbMemoBuf)), iLen);
      }
      *pbMemoBuf += 4;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_LONG:
    if (bBufEnd - (*pbMemoBuf) >= 4)
    {
      hb_itemPutNL(pItem, static_cast<long>(HB_GET_LE_UINT32(*pbMemoBuf)));
      *pbMemoBuf += 4;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_LONG1:
    if (bBufEnd - (*pbMemoBuf) >= 5)
    {
      hb_itemPutNLLen(pItem, static_cast<HB_LONG>(HB_GET_LE_UINT32(*pbMemoBuf)), (*pbMemoBuf)[4]);
      *pbMemoBuf += 5;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_LONG2:
    if (bBufEnd - (*pbMemoBuf) >= 6)
    {
      int iLen = (*pbMemoBuf)[4], iDec = (*pbMemoBuf)[5];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<HB_LONG>(HB_GET_LE_UINT32(*pbMemoBuf)), iLen, iDec);
      }
      else
      {
        hb_itemPutNLLen(pItem, static_cast<HB_LONG>(HB_GET_LE_UINT32(*pbMemoBuf)), iLen);
      }
      *pbMemoBuf += 6;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_ULONG2:
    if (bBufEnd - (*pbMemoBuf) >= 6)
    {
      int iLen = (*pbMemoBuf)[4], iDec = (*pbMemoBuf)[5];
      if (iDec)
      {
        iLen -= iDec + 1;
        hb_itemPutNDLen(pItem, static_cast<HB_ULONG>(HB_GET_LE_UINT32(*pbMemoBuf)), iLen, iDec);
      }
      else
      {
        hb_itemPutNIntLen(pItem, static_cast<HB_ULONG>(HB_GET_LE_UINT32(*pbMemoBuf)), iLen);
      }
      *pbMemoBuf += 6;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_DOUBLE:
    if (bBufEnd - (*pbMemoBuf) >= 8)
    {
      hb_itemPutND(pItem, HB_GET_LE_DOUBLE(*pbMemoBuf));
      *pbMemoBuf += 8;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_DOUBLE2:
    if (bBufEnd - (*pbMemoBuf) >= 10)
    {
      int iLen = (*pbMemoBuf)[0], iDec = (*pbMemoBuf)[1];
      if (iDec)
      {
        iLen -= iDec + 1;
      }
      hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(*pbMemoBuf + 2), iLen, iDec);
      *pbMemoBuf += 10;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_LDOUBLE:
    if (bBufEnd - (*pbMemoBuf) >= 10)
    {
      // TODO: write a cross platform converter from
      //       10 digit long double to double
      hb_itemPutND(pItem, 0.0 /* HB_GET_LE_DOUBLE(*pbMemoBuf) */);
      *pbMemoBuf += 10;
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  case FPTIT_FLEXAR_NUL:
    hb_itemPutCL(pItem, nullptr, 0);
    break;

  case FPTIT_FLEXAR_STR:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      ulLen = HB_GET_LE_UINT16(*pbMemoBuf);
      *pbMemoBuf += 2;
      if (bBufEnd - (*pbMemoBuf) >= static_cast<HB_LONG>(ulLen))
      {
        char *pszStr = reinterpret_cast<char *>(*pbMemoBuf);
        *pbMemoBuf += ulLen;

        if (iTrans == FPT_TRANS_UNICODE)
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pszStr), ulLen >> 1);
        }
        else if (iTrans == FPT_TRANS_CP && ulLen != 0)
        {
          HB_SIZE nLen = ulLen;
          pszStr = hb_cdpnDup(pszStr, &nLen, pArea->area.cdPage, hb_vmCDP());
          hb_itemPutCLPtr(pItem, pszStr, nLen);
        }
        else
        {
          hb_itemPutCL(pItem, pszStr, ulLen);
        }
      }
      else
      {
        errCode = EDBF_CORRUPT;
      }
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;

  case FPTIT_FLEXAR_ARAY:
    if (bBufEnd - (*pbMemoBuf) >= 2)
    {
      ulLen = HB_GET_LE_UINT16(*pbMemoBuf);
      *pbMemoBuf += 2;
      if (bBufEnd - (*pbMemoBuf) >= static_cast<HB_LONG>(ulLen))
      {
        hb_arrayNew(pItem, ulLen);
        for (i = 1; i <= ulLen; i++)
        {
          errCode = hb_fptReadFlexItem(pArea, pbMemoBuf, bBufEnd, hb_arrayGetItemPtr(pItem, i), false, iTrans);
          if (errCode != Harbour::SUCCESS)
          {
            break;
          }
        }
      }
      else
      {
        errCode = EDBF_CORRUPT;
      }
    }
    else
    {
      errCode = EDBF_CORRUPT;
    }
    break;
  default:
#if 0
         fprintf(stderr, "Unknown FLEX array item: 0x%x = %d\n", usType, usType);
         fflush(stderr);
#endif
    errCode = EDBF_CORRUPT;
    hb_itemClear(pItem);
    break;
  }
  return errCode;
}

static HB_ERRCODE hb_fptCopyToRawFile(PHB_FILE pSrc, HB_FOFFSET from, PHB_FILE pDst, HB_FOFFSET size)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (size)
  {
    HB_FOFFSET written = 0;
    HB_SIZE nBufSize;

    nBufSize = static_cast<HB_SIZE>(HB_MIN(0x10000, size));
    auto pBuffer = static_cast<HB_BYTE *>(hb_xgrab(nBufSize));

    do
    {
      HB_SIZE nRead =
          hb_fileReadAt(pSrc, pBuffer, static_cast<HB_SIZE>(HB_MIN(static_cast<HB_FOFFSET>(nBufSize), size - written)),
                        from + written);
      if (nRead == 0 || nRead == static_cast<HB_SIZE>(FS_ERROR))
      {
        errCode = EDBF_READ;
      }
      else if (hb_fileWrite(pDst, pBuffer, nRead, -1) != nRead)
      {
        errCode = EDBF_WRITE;
      }
      else
      {
        written += nRead;
      }
    } while (errCode == Harbour::SUCCESS && written < size);

    hb_xfree(pBuffer);
  }

  return errCode;
}

static HB_ERRCODE hb_fptCopyToFile(PHB_FILE pSrc, HB_FOFFSET from, PHB_FILE pDst, HB_FOFFSET to, HB_FOFFSET size)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (size)
  {
    HB_FOFFSET written = 0;
    HB_SIZE nBufSize;

    nBufSize = static_cast<HB_SIZE>(HB_MIN(0x10000, size));
    auto pBuffer = static_cast<HB_BYTE *>(hb_xgrab(nBufSize));

    do
    {
      HB_SIZE nRead =
          hb_fileReadAt(pSrc, pBuffer, static_cast<HB_SIZE>(HB_MIN(static_cast<HB_FOFFSET>(nBufSize), size - written)),
                        from + written);
      if (nRead == 0 || nRead == static_cast<HB_SIZE>(FS_ERROR))
      {
        errCode = EDBF_READ;
      }
      else if (hb_fileWriteAt(pDst, pBuffer, nRead, to + written) != nRead)
      {
        errCode = EDBF_WRITE;
      }
      else
      {
        written += nRead;
      }
    } while (errCode == Harbour::SUCCESS && written < size);

    hb_xfree(pBuffer);
  }

  return errCode;
}

static HB_ERRCODE hb_fptReadRawBlock(FPTAREAP pArea, HB_BYTE *bBuffer, PHB_FILE pFile, HB_ULONG ulBlock,
                                     HB_ULONG ulSize)
{
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (ulBlock == 0)
  {
    return EDBF_CORRUPT;
  }

  if (pFile != nullptr)
  {
    errCode = hb_fptCopyToRawFile(pArea->pMemoFile, FPT_BLOCK_OFFSET(ulBlock), pFile, ulSize);
  }
  else
  {
    if (hb_fileReadAt(pArea->pMemoFile, bBuffer, ulSize, FPT_BLOCK_OFFSET(ulBlock)) != ulSize)
    {
      errCode = EDBF_READ;
    }
  }

  return errCode;
}

static HB_ERRCODE hb_fptReadBlobBlock(FPTAREAP pArea, PHB_ITEM pItem, PHB_FILE pFile, HB_ULONG ulBlock,
                                      HB_USHORT uiMode)
{
  HB_ULONG ulSize;
  HB_BYTE buffer[4];

  if (ulBlock == 0)
  {
    return EDBF_CORRUPT;
  }

  // TODO: uiMode => BLOB_IMPORT_COMPRESS, BLOB_IMPORT_ENCRYPT
  HB_SYMBOL_UNUSED(uiMode);

  if (hb_fileReadAt(pArea->pMemoFile, buffer, 4, FPT_BLOCK_OFFSET(ulBlock)) != 4)
  {
    return EDBF_READ;
  }

  ulSize = HB_GET_LE_UINT32(buffer);
  if (pFile != nullptr)
  {
    return hb_fptCopyToRawFile(pArea->pMemoFile, FPT_BLOCK_OFFSET(ulBlock) + 4, pFile, ulSize);
  }

  if (ulSize == 0)
  {
    hb_itemPutC(pItem, nullptr);
  }
  else
  {
    HB_BYTE *bBuffer = static_cast<HB_BYTE *>(hb_xalloc(ulSize + 1));

    if (!bBuffer)
    {
      // in most cases this means that file is corrupted
      return EDBF_CORRUPT;
    }
    if (hb_fileReadAt(pArea->pMemoFile, bBuffer, ulSize, FPT_BLOCK_OFFSET(ulBlock) + 4) != ulSize)
    {
      hb_xfree(bBuffer);
      return EDBF_READ;
    }
    hb_itemPutCLPtr(pItem, reinterpret_cast<char *>(bBuffer), ulSize);
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_fptReadSMTBlock(FPTAREAP pArea, PHB_ITEM pItem, HB_ULONG ulBlock, HB_ULONG ulSize, int iTrans)
{
  if (ulBlock == 0)
  {
    return EDBF_CORRUPT;
  }

  if (ulSize == 0)
  {
    HB_FOFFSET fOffset = FPT_BLOCK_OFFSET(ulBlock);
    return hb_fptReadRawSMTItem(pArea, pItem, &fOffset, iTrans);
  }
  else
  {
    HB_ERRCODE errCode;
    HB_BYTE *bBuffer = static_cast<HB_BYTE *>(hb_xalloc(ulSize)), *bMemoBuf;

    if (!bBuffer)
    {
      // in most cases this means that file is corrupted
      return EDBF_CORRUPT;
    }

    if (hb_fileReadAt(pArea->pMemoFile, bBuffer, ulSize, FPT_BLOCK_OFFSET(ulBlock)) != ulSize)
    {
      errCode = EDBF_READ;
    }
    else
    {
      bMemoBuf = bBuffer;
      errCode = hb_fptReadSMTItem(pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans);
    }
    hb_xfree(bBuffer);
    return errCode;
  }
}

// Read fpt vartype memos.
static HB_ERRCODE hb_fptGetMemo(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, PHB_FILE pFile, HB_ULONG ulBlock,
                                HB_ULONG ulStart, HB_ULONG ulCount, int iTrans)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetMemo(%p, %hu, %p, %p, %lu, %lu, %d)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem), static_cast<void*>(pFile), ulStart, ulCount, iTrans));
#endif

  HB_ERRCODE errCode;
  HB_ULONG ulSize = 0, ulType = 0;
  char *pBuffer;
  HB_BYTE *bMemoBuf;
  FPTBLOCK fptBlock;

  if (uiIndex)
  {
    errCode = hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulSize, &ulType);
  }
  else if (!hb_fptHasDirectAccess(pArea))
  {
    errCode = EDBF_UNSUPPORTED;
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }

  if (errCode != Harbour::SUCCESS)
  {
    return errCode;
  }

  if (ulBlock > 0)
  {
    HB_FOFFSET fOffset = FPT_BLOCK_OFFSET(ulBlock);
    if (pArea->bMemoType == DB_MEMO_FPT)
    {
      if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), fOffset) != sizeof(FPTBLOCK))
      {
        return EDBF_READ;
      }
      fOffset += sizeof(FPTBLOCK);
      ulType = HB_GET_BE_UINT32(fptBlock.type);
      ulSize = HB_GET_BE_UINT32(fptBlock.size);
    }
    else
    {
      if (pArea->bMemoType == DB_MEMO_DBT)
      {
        ulSize = hb_fptGetMemoLen(pArea, uiIndex);
        ulType = FPTIT_BINARY;
      }
    }

    if (ulStart || ulCount)
    {
      if (pArea->bMemoType == DB_MEMO_FPT)
      {
        if (ulType != FPTIT_TEXT && ulType != FPTIT_PICT)
        {
          ulStart = ulCount = 0;
        }
      }
      else if (pArea->bMemoType == DB_MEMO_SMT)
      {
        if (ulType != SMT_IT_CHAR)
        {
          ulStart = ulCount = 0;
        }
      }
    }

    if (ulStart >= ulSize)
    {
      ulSize = 0;
    }
    else
    {
      ulSize -= ulStart;
    }
    if (ulCount && ulCount < ulSize)
    {
      ulSize = ulCount;
    }
    if (ulStart && ulSize)
    {
      fOffset += ulStart;
    }

    if (pFile != nullptr)
    {
      return hb_fptCopyToRawFile(pArea->pMemoFile, fOffset, pFile, ulSize);
    }

    if (pArea->bMemoType == DB_MEMO_FPT)
    {
      pBuffer = static_cast<char *>(hb_xalloc(HB_MAX(ulSize + 1, 8)));
      if (pBuffer)
      {
        memset(pBuffer, '\0', 8);
      }
    }
    else
    {
      pBuffer = static_cast<char *>(hb_xalloc(ulSize + 1));
    }

    if (!pBuffer)
    {
      // in most cases this means that file is corrupted
      return EDBF_CORRUPT;
    }

    if (ulSize != 0 && hb_fileReadAt(pArea->pMemoFile, pBuffer, ulSize, fOffset) != ulSize)
    {
      errCode = EDBF_READ;
    }
    else if (pArea->bMemoType == DB_MEMO_DBT)
    {
      if (iTrans == FPT_TRANS_UNICODE)
      {
        hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pBuffer), ulSize >> 1);
        hb_xfree(pBuffer);
      }
      else
      {
        if (iTrans == FPT_TRANS_CP && ulSize != 0)
        {
          HB_SIZE nSize = ulSize;
          HB_SIZE nBufSize = ulSize + 1;
          hb_cdpnDup3(pBuffer, ulSize, pBuffer, &nSize, &pBuffer, &nBufSize, pArea->area.cdPage, hb_vmCDP());
          ulSize = static_cast<HB_ULONG>(nSize);
        }
        hb_itemPutCLPtr(pItem, pBuffer, ulSize);
      }
      hb_itemSetCMemo(pItem);
      pBuffer = nullptr;
    }
    else if (pArea->bMemoType == DB_MEMO_SMT)
    {
      if (ulType == SMT_IT_CHAR)
      {
        if (iTrans == FPT_TRANS_UNICODE)
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pBuffer), ulSize >> 1);
          hb_xfree(pBuffer);
        }
        else
        {
          if (iTrans == FPT_TRANS_CP && ulSize != 0)
          {
            HB_SIZE nSize = ulSize;
            HB_SIZE nBufSize = ulSize + 1;
            hb_cdpnDup3(pBuffer, ulSize, pBuffer, &nSize, &pBuffer, &nBufSize, pArea->area.cdPage, hb_vmCDP());
            ulSize = static_cast<HB_ULONG>(nSize);
          }
          hb_itemPutCLPtr(pItem, pBuffer, ulSize);
        }
        hb_itemSetCMemo(pItem);
        pBuffer = nullptr;
      }
      else if (!ulSize || pBuffer[0] != static_cast<char>(ulType))
      {
        errCode = EDBF_CORRUPT;
        hb_itemClear(pItem);
      }
      else
      {
        bMemoBuf = reinterpret_cast<HB_BYTE *>(pBuffer);
        errCode = hb_fptReadSMTItem(pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans);
      }
    }
    else
    {
      switch (ulType)
      {
      case FPTIT_SIX_LNUM:
      case FPTIT_SIX_DNUM:
      case FPTIT_SIX_LDATE:
      case FPTIT_SIX_LOG:
      case FPTIT_SIX_CHAR:
      case FPTIT_SIX_ARRAY:
#if 0
            case FPTIT_SIX_BLOCK:
            case FPTIT_SIX_VREF:
            case FPTIT_SIX_MREF:
#endif
        bMemoBuf = reinterpret_cast<HB_BYTE *>(pBuffer);
        errCode = hb_fptReadSixItem(pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, iTrans);
        break;
      case FPTIT_FLEX_ARRAY:
        bMemoBuf = reinterpret_cast<HB_BYTE *>(pBuffer);
        errCode = hb_fptReadFlexItem(pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, true, iTrans);
        break;
      case FPTIT_FLEX_NIL:
        hb_itemClear(pItem);
        break;
      case FPTIT_FLEX_TRUE:
        hb_itemPutL(pItem, true);
        break;
      case FPTIT_FLEX_FALSE:
        hb_itemPutL(pItem, false);
        break;
      case FPTIT_FLEX_LDATE:
        hb_itemPutDL(pItem, static_cast<long>(HB_GET_LE_UINT32(pBuffer)));
        break;
      case FPTIT_FLEX_CHAR:
        hb_itemPutNI(pItem, static_cast<signed char>(pBuffer[0]));
        break;
      case FPTIT_FLEX_UCHAR:
        hb_itemPutNI(pItem, static_cast<unsigned char>(pBuffer[0]));
        break;
      case FPTIT_FLEX_SHORT:
        hb_itemPutNI(pItem, static_cast<short>(HB_GET_LE_UINT16(pBuffer)));
        break;
      case FPTIT_FLEX_USHORT:
        hb_itemPutNInt(pItem, HB_GET_LE_UINT16(pBuffer));
        break;
      case FPTIT_FLEX_LONG:
        hb_itemPutNL(pItem, static_cast<long>(HB_GET_LE_UINT32(pBuffer)));
        break;
      case FPTIT_FLEX_ULONG:
        hb_itemPutNInt(pItem, HB_GET_LE_UINT32(pBuffer));
        break;
      case FPTIT_FLEX_DOUBLE:
        hb_itemPutND(pItem, HB_GET_LE_DOUBLE(pBuffer));
        break;
      case FPTIT_FLEX_LDOUBLE:
        // TODO: write a cross platform converter from
        //       10 digit long double to double
        hb_itemPutND(pItem, 0.0 /* HB_GET_LE_DOUBLE(pBuffer) */);
        break;
      case FPTIT_TEXT:
        if (iTrans == FPT_TRANS_UNICODE)
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pBuffer), ulSize >> 1);
          hb_xfree(pBuffer);
        }
        else
        {
          if (iTrans == FPT_TRANS_CP && ulSize != 0)
          {
            HB_SIZE nSize = ulSize;
            HB_SIZE nBufSize = ulSize + 1;
            hb_cdpnDup3(pBuffer, ulSize, pBuffer, &nSize, &pBuffer, &nBufSize, pArea->area.cdPage, hb_vmCDP());
            ulSize = static_cast<HB_ULONG>(nSize);
          }
          hb_itemPutCLPtr(pItem, pBuffer, ulSize);
        }
        pBuffer = nullptr;
        hb_itemSetCMemo(pItem);
        break;
      case FPTIT_PICT:
        hb_itemPutCLPtr(pItem, pBuffer, ulSize);
        pBuffer = nullptr;
        break;
      default:
        hb_itemClear(pItem);
        break;
      }
    }
    if (pBuffer)
    {
      hb_xfree(pBuffer);
    }
  }
  else
  {
    hb_itemPutC(pItem, nullptr);
    hb_itemSetCMemo(pItem);
  }
  return errCode;
}

// Write memo data.
static HB_ERRCODE hb_fptWriteMemo(FPTAREAP pArea, HB_ULONG ulBlock, HB_ULONG ulSize, const HB_BYTE *bBufPtr,
                                  PHB_FILE pFile, HB_ULONG ulType, HB_ULONG ulLen, HB_ULONG *pulStoredBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptWriteMemo(%p, %lu, %lu, %p, %p, %lu, %lu, %p)", static_cast<void*>(pArea), ulBlock, ulSize, static_cast<const void*>(bBufPtr), static_cast<void*>(pFile), ulType, ulLen, static_cast<void*>(pulStoredBlock)));
#endif

  MEMOGCTABLE fptGCtable;
  auto bWrite = false;

  bWrite = (ulLen != 0 || (pArea->bMemoType == DB_MEMO_FPT && ulType != FPTIT_TEXT && ulType != FPTIT_BINARY &&
                           ulType != FPTIT_DUMMY));

  if (ulBlock == 0 && !bWrite)
  {
    *pulStoredBlock = 0;
    return Harbour::SUCCESS;
  }

  hb_fptInitGCdata(&fptGCtable);
  HB_ERRCODE errCode = hb_fptReadGCdata(pArea, &fptGCtable);
  if (errCode != Harbour::SUCCESS)
  {
    return errCode;
  }

  if (ulBlock > 0)
  {
    errCode = hb_fptGCfreeBlock(pArea, &fptGCtable, ulBlock, ulSize, ulType == FPTIT_DUMMY);
    if (errCode != Harbour::SUCCESS)
    {
      hb_fptDestroyGCdata(&fptGCtable);
      return errCode;
    }
  }

  // Write memo header and data
  if (bWrite)
  {
    HB_FOFFSET fOffset;

    errCode = hb_fptGCgetFreeBlock(pArea, &fptGCtable, pulStoredBlock, ulLen, ulType == FPTIT_DUMMY);
    if (errCode != Harbour::SUCCESS)
    {
      hb_fptDestroyGCdata(&fptGCtable);
      return errCode;
    }

    fOffset = FPT_BLOCK_OFFSET(*pulStoredBlock);
    if (pArea->bMemoType == DB_MEMO_FPT && ulType != FPTIT_DUMMY)
    {
      FPTBLOCK fptBlock;
      HB_PUT_BE_UINT32(fptBlock.type, ulType);
      HB_PUT_BE_UINT32(fptBlock.size, ulLen);
      if (hb_fileWriteAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), fOffset) != sizeof(FPTBLOCK))
      {
        errCode = EDBF_WRITE;
      }
      else
      {
        fOffset += sizeof(FPTBLOCK);
      }
    }

    if (errCode == Harbour::SUCCESS && ulLen > 0)
    {
      // TODO: uiMode => BLOB_IMPORT_COMPRESS, BLOB_IMPORT_ENCRYPT
      if (pFile != nullptr)
      {
        HB_SIZE nWritten = 0, nBufSize = HB_MIN((1 << 16), ulLen);
        auto bBuffer = static_cast<HB_BYTE *>(hb_xgrab(nBufSize));

        do
        {
          HB_SIZE nRead = hb_fileRead(pFile, bBuffer, HB_MIN(nBufSize, ulLen - nWritten), -1);
          if (nRead == 0 || nRead == static_cast<HB_SIZE>(FS_ERROR))
          {
            errCode = EDBF_READ;
          }
          else if (hb_fileWriteAt(pArea->pMemoFile, bBuffer, nRead, fOffset) != nRead)
          {
            errCode = EDBF_WRITE;
          }
          else
          {
            nWritten += nRead;
            fOffset += nRead;
          }
        } while (errCode == Harbour::SUCCESS && nWritten < ulLen);

        hb_xfree(bBuffer);
      }
      else
      {
        if (hb_fileWriteAt(pArea->pMemoFile, bBufPtr, ulLen, fOffset) != ulLen)
        {
          errCode = EDBF_WRITE;
        }
        else
        {
          fOffset += ulLen;
        }
      }
    }
    // if written block is smaller then block size we should write at last
    // block byte 0xAF to be FLEX compatible
    if (errCode == Harbour::SUCCESS)
    {
      if (pArea->bMemoType == DB_MEMO_DBT)
      {
        hb_fileWriteAt(pArea->pMemoFile, "\x1A\x1A", 2, fOffset);
      }
      else if (pArea->uiMemoVersion == DB_MEMOVER_FLEX && (ulLen + sizeof(FPTBLOCK)) % pArea->ulMemoBlockSize != 0)
      {
        HB_ULONG ulBlocks = (ulLen + sizeof(FPTBLOCK) + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
        hb_fileWriteAt(pArea->pMemoFile, "\xAF", 1, FPT_BLOCK_OFFSET(*pulStoredBlock + ulBlocks) - 1);
      }
    }
    pArea->fMemoFlush = true;
  }
  else
  {
    *pulStoredBlock = 0;
  }

  if (errCode == Harbour::SUCCESS)
  {
    errCode = hb_fptWriteGCdata(pArea, &fptGCtable);
  }
  hb_fptDestroyGCdata(&fptGCtable);

  return errCode;
}

// Assign a value to the specified memo field.
static HB_ERRCODE hb_fptPutMemo(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, HB_ULONG *pulBlock, int iTrans)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutMemo(%p, %hu, %p, %p)", pArea, uiIndex, pItem, pulBlock));
#endif

  HB_ULONG ulBlock = 0, ulSize, ulType, ulOldSize = 0, ulOldType = 0, ulArrayCount = 0;
  HB_BYTE itmBuffer[FLEX_ITEM_BUFSIZE];
  const HB_BYTE *bBufPtr = nullptr;
  HB_BYTE *bBufAlloc = nullptr, *pbTmp;
  HB_ERRCODE errCode;
  HB_MAXINT iVal;
  HB_LONG lVal;

  if (pItem->isString())
  {
    ulType = FPTIT_TEXT;
    if (iTrans == FPT_TRANS_UNICODE)
    {
      ulSize = static_cast<HB_ULONGCAST>(hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, nullptr, 0)) * sizeof(HB_WCHAR);
      if (ulSize > 0)
      {
        bBufPtr = bBufAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulSize));
        hb_itemCopyStrU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<HB_WCHAR *>(bBufAlloc),
                          ulSize / sizeof(HB_WCHAR));
      }
    }
    else
    {
      ulSize = static_cast<HB_ULONGCAST>(pItem->getCLen());
      bBufPtr = reinterpret_cast<const HB_BYTE *>(pItem->getCPtr());
      if (iTrans == FPT_TRANS_CP && ulSize > 0)
      {
        HB_SIZE nSize = ulSize;
        bBufAlloc = reinterpret_cast<HB_BYTE *>(
            hb_cdpnDup(reinterpret_cast<const char *>(bBufPtr), &nSize, hb_vmCDP(), pArea->area.cdPage));
        bBufPtr = bBufAlloc;
        ulSize = static_cast<HB_ULONG>(nSize);
      }
    }
    if (pArea->bMemoType == DB_MEMO_SMT)
    {
      ulType = SMT_IT_CHAR;
    }
  }
  else if (pArea->bMemoType == DB_MEMO_DBT)
  {
    return EDBF_DATATYPE;
  }
  else if (pArea->bMemoType == DB_MEMO_SMT)
  {
    ulSize = hb_fptCountSMTItemLength(pArea, pItem, &ulArrayCount, iTrans);
    if (ulSize == 0)
    {
      return EDBF_DATATYPE;
    }
    pbTmp = bBufAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulSize));
    hb_fptStoreSMTItem(pArea, pItem, &pbTmp, iTrans);
    ulType = static_cast<HB_ULONG>(bBufAlloc[0]);
    bBufPtr = bBufAlloc;
  }
  else if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
  {
    if (pItem->isNil())
    {
      ulType = FPTIT_SIX_NIL;
      ulSize = 0;
    }
    else
    {
      ulSize = hb_fptCountSixItemLength(pArea, pItem, &ulArrayCount, iTrans);
      if (ulSize > 0)
      {
        pbTmp = bBufAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulSize));
        hb_fptStoreSixItem(pArea, pItem, &pbTmp, iTrans);
        ulType = static_cast<HB_ULONG>(HB_GET_LE_UINT16(bBufAlloc));
        bBufPtr = bBufAlloc;
      }
      else
      {
        return EDBF_DATATYPE;
      }
    }
  }
  else if (pArea->uiMemoVersion == DB_MEMOVER_FLEX)
  {
    switch (hb_itemType(pItem))
    {
    case Harbour::Item::ARRAY:
      ulType = FPTIT_FLEX_ARRAY;
      ulSize = hb_fptCountFlexItemLength(pArea, pItem, &ulArrayCount, iTrans) - 1;
      if (ulSize > 0)
      {
        pbTmp = bBufAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulSize + 1));
        hb_fptStoreFlexItem(pArea, pItem, &pbTmp, iTrans);
        bBufPtr = bBufAlloc + 1; // FLEX doesn't store the first byte of array ID
      }
      break;
    case Harbour::Item::NIL:
      ulType = FPTIT_FLEX_NIL;
      ulSize = 0;
      break;
    case Harbour::Item::LOGICAL:
      ulType = pItem->getL() ? FPTIT_FLEX_TRUE : FPTIT_FLEX_FALSE;
      ulSize = 0;
      break;
    case Harbour::Item::DATE:
    case Harbour::Item::TIMESTAMP:
      ulType = FPTIT_FLEX_LDATE;
      ulSize = 4;
      lVal = pItem->getDL();
      HB_PUT_LE_UINT32(itmBuffer, lVal);
      bBufPtr = itmBuffer;
      break;
    case Harbour::Item::INTEGER:
    case Harbour::Item::LONG:
      iVal = pItem->getNInt();
      if (HB_LIM_INT8(iVal))
      {
        ulType = FPTIT_FLEX_CHAR;
        ulSize = 1;
        *itmBuffer = static_cast<HB_BYTE>(iVal);
        bBufPtr = itmBuffer;
      }
      else if (HB_LIM_INT16(iVal))
      {
        ulType = FPTIT_FLEX_SHORT;
        ulSize = 2;
        HB_PUT_LE_UINT16(itmBuffer, iVal);
        bBufPtr = itmBuffer;
      }
      else if (HB_LIM_INT32(iVal))
      {
        ulType = FPTIT_FLEX_LONG;
        ulSize = 4;
        HB_PUT_LE_UINT32(itmBuffer, iVal);
        bBufPtr = itmBuffer;
      }
      else
      {
        double d = static_cast<double>(iVal);
        ulType = FPTIT_FLEX_DOUBLE;
        ulSize = 8;
        HB_PUT_LE_DOUBLE(itmBuffer, d);
        bBufPtr = itmBuffer;
      }
      break;
    case Harbour::Item::DOUBLE:
    {
      auto d = pItem->getND();
      ulType = FPTIT_FLEX_DOUBLE;
      ulSize = 8;
      HB_PUT_LE_DOUBLE(itmBuffer, d);
      bBufPtr = itmBuffer;
      break;
    }
    default:
      ulType = FPTIT_BINARY;
      ulSize = 0;
      break;
    }
  }
  else
  {
    return EDBF_DATATYPE;
  }

  if (uiIndex)
  {
    errCode = hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulOldSize, &ulOldType);
  }
  else if (!pulBlock || !hb_fptHasDirectAccess(pArea))
  {
    errCode = EDBF_UNSUPPORTED;
  }
  else
  {
    ulBlock = *pulBlock;
    errCode = Harbour::SUCCESS;
  }

  if (errCode == Harbour::SUCCESS)
  {
    errCode = hb_fptWriteMemo(pArea, ulBlock, ulOldSize, bBufPtr, nullptr, ulType, ulSize, &ulBlock);
  }

  if (bBufAlloc != nullptr)
  {
    hb_xfree(bBufAlloc);
  }

  if (errCode == Harbour::SUCCESS)
  {
    if (uiIndex)
    {
      hb_dbfSetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, ulBlock, ulSize, ulType);
    }
    else
    {
      *pulBlock = ulBlock;
    }
  }
  return errCode;
}

#ifdef HB_MEMO_SAFELOCK
// Check if memo field has any data
static bool hb_fptHasMemoData(FPTAREAP pArea, HB_USHORT uiIndex)
{
  if (--uiIndex < pArea->area.uiFieldCount)
  {
    LPFIELD pField = pArea->area.lpFields + uiIndex;

    if (pField->uiType == Harbour::DB::Field::ANY)
    {
      if (pField->uiLen >= 6)
      {
        HB_BYTE *pFieldBuf = pArea->pRecord + pArea->pFieldOffset[uiIndex];
        HB_USHORT uiType = HB_GET_LE_UINT16(pFieldBuf + pField->uiLen - 2);

        switch (uiType)
        {
        case HB_VF_ARRAY:
        case HB_VF_BLOB:
        case HB_VF_BLOBCOMPRESS:
        case HB_VF_BLOBENCRYPT:
          return true;
        case HB_VF_DNUM:
          return pField->uiLen <= 12;
        default:
          return uiType <= HB_VF_CHAR && pField->uiLen - 2 < uiType;
        }
      }
    }
    else if (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
             pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE)
    {
      HB_BYTE *pFieldBuf = pArea->pRecord + pArea->pFieldOffset[uiIndex];
      HB_USHORT uiLen = pField->uiLen;

      if (uiLen == 4)
      {
        return HB_GET_LE_UINT32(pFieldBuf) != 0;
      }
      if (uiLen == 10)
      {
        if (pArea->bMemoType == DB_MEMO_SMT)
        {
          return HB_GET_LE_UINT32((static_cast<LPSMTFIELD>(pFieldBuf))->block) != 0;
        }
        do
        {
          if (*pFieldBuf >= '1' && *pFieldBuf <= '9')
          {
            return true;
          }
          ++pFieldBuf;
        } while (--uiLen);
      }
    }
  }
  return false;
}
#endif

static HB_ERRCODE hb_fptLockForRead(FPTAREAP pArea, HB_USHORT uiIndex, bool *fUnLock)
{
  HB_ERRCODE errCode;
  HB_BOOL fLocked;

  *fUnLock = false;
#ifdef HB_MEMO_SAFELOCK
  if (pArea->lpdbPendingRel)
  {
    errCode = SELF_FORCEREL(&pArea->area);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }
  }

  if ((uiIndex > 0 && pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::ANY &&
       pArea->area.lpFields[uiIndex - 1].uiLen < 6) ||
      !pArea->fPositioned || !pArea->fShared || pArea->fFLocked || pArea->fRecordChanged)
  {
    fLocked = true;
  }
  else
  {
    auto pRecNo = hb_itemNew(nullptr);
    auto pResult = hb_itemNew(nullptr);

    errCode = SELF_RECINFO(&pArea->area, pRecNo, DBRI_LOCKED, pResult);
    fLocked = pResult->getL();
    hb_itemRelease(pRecNo);
    hb_itemRelease(pResult);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }
  }

  if (!fLocked)
  {
    if (!pArea->fValidBuffer || uiIndex == 0 || hb_fptHasMemoData(pArea, uiIndex))
    {
      if (!hb_fptFileLockSh(pArea, true))
      {
        return Harbour::FAILURE;
      }

      *fUnLock = true;
      pArea->fValidBuffer = false;
    }
  }
#else
  HB_SYMBOL_UNUSED(uiIndex);
#endif
  // update any pending relations and reread record if necessary
  errCode = SELF_DELETED(&pArea->area, &fLocked);

  return errCode;
}

static HB_ERRCODE hb_fptGetVarField(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem, PHB_FILE pFile)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetVarField(%p, %hu, %p, %p)", pArea, uiIndex, pItem, pFile));
#endif

  LPFIELD pField;
  HB_ERRCODE errCode;
  HB_BYTE *pFieldBuf;
  auto fUnLock = false;

  pField = pArea->area.lpFields + uiIndex - 1;

  if (pField->uiType == Harbour::DB::Field::ANY)
  {
    HB_USHORT uiType;

    errCode = hb_fptLockForRead(pArea, uiIndex, &fUnLock);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    pFieldBuf = pArea->pRecord + pArea->pFieldOffset[uiIndex - 1];
    if (pField->uiLen >= 6)
    {
      uiType = HB_GET_LE_UINT16(pFieldBuf + pField->uiLen - 2);
    }
    else
    {
      uiType = 0;
    }

    if (pField->uiLen == 3 || uiType == HB_VF_DATE)
    {
      hb_itemPutDL(pItem, hb_sxPtoD(reinterpret_cast<char *>(pFieldBuf)));
    }
    else if (pField->uiLen == 4 || uiType == HB_VF_INT)
    {
      hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(HB_GET_LE_INT32(pFieldBuf)), 10);
    }
    else if (pField->uiLen == 2)
    {
      hb_itemPutNIntLen(pItem, static_cast<int>(HB_GET_LE_INT16(pFieldBuf)), 10);
    }
    else if (pField->uiLen == 1)
    {
      hb_itemPutNILen(pItem, static_cast<signed char>(pFieldBuf[0]), 4);
    }
    else if (pField->uiLen >= 6)
    {
      HB_ULONG ulBlock = HB_GET_LE_UINT32(pFieldBuf + pField->uiLen - 6);

      if (uiType <= HB_VF_CHAR)
      { // 64000 max string size
        const char *pString;
        char *pAlloc = nullptr, *pPtr;
        HB_ULONG ulLen = uiType;

        if (uiType <= pField->uiLen - 2)
        {
          pString = reinterpret_cast<const char *>(pFieldBuf);
          if (ulLen > 0)
          {
            if ((pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage)
            {
              HB_SIZE nLen = ulLen;
              pString = pAlloc = hb_cdpnDup(pString, &nLen, pArea->area.cdPage, hb_vmCDP());
              ulLen = static_cast<HB_ULONG>(nLen);
            }
          }
        }
        else
        {
          HB_ULONG ulSize = ulLen;
          pString = pPtr = pAlloc = static_cast<char *>(hb_xgrab(ulLen + 1));

          if (pField->uiLen > 6)
          {
            HB_USHORT uiVLen = pField->uiLen - 6;
            memcpy(pPtr, pFieldBuf, uiVLen);
            ulSize -= uiVLen;
            pPtr += uiVLen;
          }
          errCode = hb_fptReadRawBlock(pArea, reinterpret_cast<HB_BYTE *>(pPtr), nullptr, ulBlock, ulSize);
          if (errCode == Harbour::SUCCESS && ulLen > 0 && (pField->uiFlags & HB_FF_BINARY) == 0 &&
              hb_vmCDP() != pArea->area.cdPage)
          {
            HB_SIZE nLen;
            HB_SIZE nSize;
            ulSize = ulLen + 1;
            nLen = ulLen;
            nSize = ulSize;
            pString = hb_cdpnDup3(pString, ulLen, pAlloc, &nLen, &pAlloc, &nSize, pArea->area.cdPage, hb_vmCDP());
            ulLen = static_cast<HB_ULONG>(nLen);
          }
        }

        if (errCode == Harbour::SUCCESS)
        {
          if (pFile != nullptr)
          {
            if (hb_fileWrite(pFile, pString, ulLen, -1) != ulLen)
            {
              errCode = EDBF_WRITE;
            }
          }
          else if (pAlloc)
          {
            hb_itemPutCLPtr(pItem, pAlloc, ulLen);
            pAlloc = nullptr;
          }
          else
          {
            hb_itemPutCL(pItem, pString, ulLen);
          }
        }
        if (pAlloc)
        {
          hb_xfree(pAlloc);
        }
      }
      else if (uiType == HB_VF_LOG)
      {
        if (pFile != nullptr)
        {
          errCode = EDBF_DATATYPE;
        }
        else
        {
          hb_itemPutL(pItem, pFieldBuf[0] != 0);
        }
      }
      else if (uiType == HB_VF_DNUM)
      { // n>12 VFIELD else MEMO (bLen[1],bDec[1],dVal[8])
        if (pFile != nullptr)
        {
          errCode = EDBF_DATATYPE;
        }
        else
        {
          HB_BYTE pBuffer[11];
          int iWidth, iDec;

          // should be <= 11 - it's SIX bug but I replicated it for
          // compatibility
          if (pField->uiLen <= 12)
          {
            errCode = hb_fptReadRawBlock(pArea, pBuffer, nullptr, ulBlock, 11);
            if (errCode == Harbour::SUCCESS)
            {
              if (pBuffer[0] == SMT_IT_DOUBLE)
              {
                pFieldBuf = pBuffer + 1;
              }
              else
              {
                errCode = EDBF_CORRUPT;
              }
            }
          }
          if (errCode == Harbour::SUCCESS)
          {
            iWidth = *pFieldBuf++;
            iDec = *pFieldBuf++;
            if (iDec)
            {
              iWidth += iDec + 1;
            }
            hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(pFieldBuf), iWidth, iDec);
          }
        }
      }
      else if (uiType == HB_VF_ARRAY)
      { // MEMO only as SMT ARRAY
        if (pFile != nullptr)
        {
          errCode = EDBF_DATATYPE;
        }
        else
        {
          errCode = hb_fptReadSMTBlock(pArea, pItem, ulBlock, 0,
                                       (pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage
                                           ? FPT_TRANS_CP
                                           : FPT_TRANS_NONE);
        }
      }
      else if (uiType == HB_VF_BLOB)
      {
        errCode = hb_fptReadBlobBlock(pArea, pItem, pFile, ulBlock, 0);
      }
      else if (uiType == HB_VF_BLOBCOMPRESS)
      {
        errCode = hb_fptReadBlobBlock(pArea, pItem, pFile, ulBlock, BLOB_IMPORT_COMPRESS);
      }
      else if (uiType == HB_VF_BLOBENCRYPT)
      {
        errCode = hb_fptReadBlobBlock(pArea, pItem, pFile, ulBlock, BLOB_IMPORT_ENCRYPT);
      }
      else
      {
        errCode = EDBF_DATATYPE;
      }
    }
  }
  else if (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
           pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE)
  {
    errCode = hb_fptLockForRead(pArea, uiIndex, &fUnLock);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    errCode = hb_fptGetMemo(pArea, uiIndex, pItem, pFile, 0, 0, 0,
                            (pField->uiFlags & HB_FF_UNICODE) != 0
                                ? FPT_TRANS_UNICODE
                                : ((pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage
                                       ? FPT_TRANS_CP
                                       : FPT_TRANS_NONE));
  }
  else if (pFile == nullptr)
  {
    return SUPER_GETVALUE(&pArea->area, uiIndex, pItem);
  }
  else
  {
    return Harbour::FAILURE;
  }

  if (fUnLock)
  {
    hb_fptFileUnLockSh(pArea);
  }

  return errCode;
}

static HB_ERRCODE hb_fptGetVarFile(FPTAREAP pArea, HB_ULONG ulBlock, const char *szFile, HB_USHORT uiMode, int iTrans)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetVarFile(%p, %lu, %s, %hu, %d)", pArea, ulBlock, szFile, uiMode, iTrans));
#endif

  HB_ERRCODE errCode;
  PHB_FILE pFile;

  pFile = hb_fileExtOpen(szFile, nullptr,
                         FO_WRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK |
                             (uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE),
                         nullptr, nullptr);

  if (pFile == nullptr)
  {
    errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
  }
  else
  {
    hb_fileSeek(pFile, 0, FS_END);
    errCode = hb_fptGetMemo(pArea, 0, nullptr, pFile, ulBlock, 0, 0, iTrans);
    hb_fileClose(pFile);
  }

  // Exit if any error
  if (errCode != Harbour::SUCCESS)
  {
    if (errCode != Harbour::FAILURE)
    {
      hb_memoErrorRT(
          pArea, 0, errCode,
          errCode == EDBF_OPEN_DBF || errCode == EDBF_CREATE || errCode == EDBF_WRITE ? szFile : pArea->szMemoFileName,
          0, 0);
    }
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

static HB_ULONG hb_fptPutVarFile(FPTAREAP pArea, HB_ULONG ulBlock, const char *szFile)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutVarFile(%p, %lu, %s)", pArea, ulBlock, szFile));
#endif

  HB_ERRCODE errCode;
  PHB_FILE pFile;

  pFile = hb_fileExtOpen(szFile, nullptr, FO_READ | FO_DENYNONE | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, nullptr);
  if (pFile == nullptr)
  {
    errCode = EDBF_OPEN_DBF;
  }
  else
  {
    HB_ULONG ulSize;
    HB_FOFFSET size = hb_fileSize(pFile);
    hb_fileSeek(pFile, 0, FS_SET);
    if (static_cast<HB_FOFFSET>(size & 0xFFFFFFFFUL) == size)
    {
      ulSize = HB_MIN(static_cast<HB_ULONG>(size), 0xFFFFFFFFUL - sizeof(FPTBLOCK));
    }
    else
    {
      ulSize = static_cast<HB_ULONG>(HB_MIN(size, static_cast<HB_FOFFSET>(0xFFFFFFFFUL - sizeof(FPTBLOCK))));
    }

    if (hb_fptFileLockEx(pArea, true))
    {
      errCode = hb_fptWriteMemo(pArea, ulBlock, 0, nullptr, pFile, 0, ulSize, &ulBlock);
      hb_fptFileUnLockEx(pArea);
    }
    else
    {
      errCode = EDBF_LOCK;
    }
    hb_fileClose(pFile);
  }

  if (errCode != Harbour::SUCCESS)
  {
    hb_memoErrorRT(pArea, 0, errCode, errCode == EDBF_OPEN_DBF || errCode == EDBF_READ ? szFile : pArea->szMemoFileName,
                   0, 0);
    ulBlock = 0;
  }

  return ulBlock;
}

static HB_ERRCODE hb_fptPutVarField(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutVarField(%p, %hu, %p)", pArea, uiIndex, pItem));
#endif

  LPFIELD pField;

  pField = pArea->area.lpFields + uiIndex - 1;

  if (pField->uiType == Harbour::DB::Field::ANY || pField->uiType == Harbour::DB::Field::MEMO ||
      pField->uiType == Harbour::DB::Field::IMAGE || pField->uiType == Harbour::DB::Field::BLOB ||
      pField->uiType == Harbour::DB::Field::OLE)
  {
    HB_BYTE *pFieldBuf = pArea->pRecord + pArea->pFieldOffset[uiIndex - 1];
    HB_BOOL bDeleted;

    // update any pending relations and reread record if necessary
    HB_ERRCODE errCode = SELF_DELETED(&pArea->area, &bDeleted);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    if (!pArea->fPositioned)
    {
      return Harbour::SUCCESS;
    }

    // Buffer is hot?
    if (!pArea->fRecordChanged)
    {
      errCode = SELF_GOHOT(&pArea->area);
      if (errCode != Harbour::SUCCESS)
      {
        return errCode;
      }
    }

    if (pField->uiType != Harbour::DB::Field::ANY)
    {
      if (!hb_fptFileLockEx(pArea, true))
      {
        return EDBF_LOCK;
      }
      errCode = hb_fptPutMemo(pArea, uiIndex, pItem, nullptr,
                              (pField->uiFlags & HB_FF_UNICODE) != 0
                                  ? FPT_TRANS_UNICODE
                                  : ((pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage
                                         ? FPT_TRANS_CP
                                         : FPT_TRANS_NONE));
#if defined(HB_MEMO_SAFELOCK)
      if (errCode == Harbour::SUCCESS)
      {
        // Force writer record to eliminate race condition
        SELF_GOCOLD(&pArea->area);
      }
#endif
      hb_fptFileUnLockEx(pArea);
    }
    else if (pField->uiLen == 3)
    {
      if (!pItem->isDateTime())
      {
        return EDBF_DATATYPE;
      }
      hb_sxDtoP(reinterpret_cast<char *>(pFieldBuf), pItem->getDL());
    }
    else if (pField->uiLen == 4)
    {
      HB_MAXINT lVal;

      if (!pItem->isNumber())
      {
        return EDBF_DATATYPE;
      }
      lVal = pItem->getNInt();
      if (pItem->isDouble() ? !HB_DBL_LIM_INT32(pItem->getND()) : !HB_LIM_INT32(lVal))
      {
        return EDBF_DATAWIDTH;
      }
      HB_PUT_LE_UINT32(pFieldBuf, static_cast<HB_U32>(lVal));
    }
    else if (pField->uiLen < 6)
    {
      return EDBF_DATATYPE;
    }
    else
    {
      HB_BYTE buffer[11], *pAlloc = nullptr, *pbTmp;
      const HB_BYTE *pBlock = nullptr;
      HB_ULONG ulOldBlock = 0, ulOldSize = 0, ulNewSize = 0;
      HB_USHORT uiType = HB_GET_LE_UINT16(pFieldBuf + pField->uiLen - 2);

      if ((uiType <= HB_VF_CHAR && uiType > pField->uiLen - 2) || (uiType == HB_VF_DNUM && pField->uiLen <= 12) ||
          uiType == HB_VF_ARRAY || uiType == HB_VF_BLOB || uiType == HB_VF_BLOBCOMPRESS || uiType == HB_VF_BLOBENCRYPT)
      {
        ulOldBlock = HB_GET_LE_UINT32(pFieldBuf + pField->uiLen - 6);
        if (ulOldBlock)
        {
          if (uiType <= HB_VF_CHAR)
          {
            ulOldSize = uiType - (pField->uiLen - 6);
          }
          else if (uiType == HB_VF_DNUM)
          {
            ulOldSize = 11;
          }
          else if (uiType == HB_VF_ARRAY)
          {
            HB_FOFFSET fOffset = FPT_BLOCK_OFFSET(ulOldBlock);
            if (hb_fptCountSMTDataLength(pArea, &fOffset) != Harbour::SUCCESS)
            {
              ulOldSize = 0;
            }
            else
            {
              ulOldSize = static_cast<HB_ULONG>(fOffset - FPT_BLOCK_OFFSET(ulOldBlock));
            }
          }
        }
      }

      if (pItem->isDateTime())
      {
        hb_sxDtoP(reinterpret_cast<char *>(pFieldBuf), pItem->getDL());
        uiType = HB_VF_DATE;
      }
      else if (pItem->isLogical())
      {
        pFieldBuf[0] = pItem->getL() ? 1 : 0;
        uiType = HB_VF_LOG;
      }
      else if (pItem->isNil())
      {
        uiType = 0;
      }
      else if (pItem->isNumber())
      {
        HB_MAXINT lVal;
        lVal = pItem->getNInt();

        if (!pItem->isDouble() && HB_LIM_INT32(lVal))
        {
          HB_PUT_LE_UINT32(pFieldBuf, static_cast<HB_U32>(lVal));
          uiType = HB_VF_INT;
        }
        else
        {
          auto dVal = pItem->getND();
          int iWidth, iDec;

          hb_itemGetNLen(pItem, &iWidth, &iDec);
          if (iDec)
          {
            iWidth += iDec + 1;
          }
          buffer[0] = SMT_IT_DOUBLE;
          buffer[1] = static_cast<HB_BYTE>(iWidth);
          buffer[2] = static_cast<HB_BYTE>(iDec);
          HB_PUT_LE_DOUBLE(&buffer[3], dVal);
          uiType = HB_VF_DNUM;
          if (pField->uiLen > 12)
          {
            memcpy(pFieldBuf, buffer + 1, 10);
          }
          else
          {
            pBlock = buffer;
            ulNewSize = 11;
          }
        }
      }
      else if (pItem->isString())
      {
        auto nLen = pItem->getCLen();

        pBlock = reinterpret_cast<const HB_BYTE *>(pItem->getCPtr());
        if (nLen > HB_VF_CHAR)
        {
          nLen = HB_VF_CHAR;
        }
        if (nLen > 0 && (pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage)
        {
          pBlock = pAlloc = reinterpret_cast<HB_BYTE *>(
              hb_cdpnDup(reinterpret_cast<const char *>(pBlock), &nLen, hb_vmCDP(), pArea->area.cdPage));
          if (nLen > HB_VF_CHAR)
          {
            nLen = HB_VF_CHAR;
          }
        }
        uiType = static_cast<HB_USHORT>(nLen);
        if (uiType <= pField->uiLen - 2)
        {
          memcpy(pFieldBuf, pBlock, uiType);
        }
        else
        {
          ulNewSize = uiType;
          if (pField->uiLen > 6)
          {
            memcpy(pFieldBuf, pBlock, pField->uiLen - 6);
            ulNewSize -= pField->uiLen - 6;
            pBlock += pField->uiLen - 6;
          }
        }
      }
      else if (pItem->isArray())
      {
        HB_ULONG ulArrayCount = 0;
        int iTrans;

#if 0
        if ((pField->uiFlags & HB_FF_UNICODE) != 0)
        {
           iTrans = FPT_TRANS_UNICODE;
        }
        else
#endif
        if ((pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage)
        {
          iTrans = FPT_TRANS_CP;
        }
        else
        {
          iTrans = FPT_TRANS_NONE;
        }

        ulNewSize = hb_fptCountSMTItemLength(pArea, pItem, &ulArrayCount, iTrans);
        pbTmp = pAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulNewSize));
        hb_fptStoreSMTItem(pArea, pItem, &pbTmp, iTrans);
        pBlock = pAlloc;
        uiType = HB_VF_ARRAY;
      }
      else
      {
        return EDBF_DATATYPE;
      }

      HB_PUT_LE_UINT16(pFieldBuf + pField->uiLen - 2, uiType);
      if (ulNewSize)
      {
        HB_PUT_LE_UINT32(pFieldBuf + pField->uiLen - 6, 0);
      }
      if (ulOldBlock != 0 || ulNewSize != 0)
      {
        if (!hb_fptFileLockEx(pArea, true))
        {
          errCode = EDBF_LOCK;
        }
        else
        {
          errCode = hb_fptWriteMemo(pArea, ulOldBlock, ulOldSize, pBlock, nullptr, FPTIT_DUMMY, ulNewSize, &ulOldBlock);
          if (errCode == Harbour::SUCCESS)
          {
            if (ulNewSize)
            {
              HB_PUT_LE_UINT32(pFieldBuf + pField->uiLen - 6, ulOldBlock);
            }
#if defined(HB_MEMO_SAFELOCK)
            // Force writer record to eliminate race condition
            SELF_GOCOLD(&pArea->area);
#endif
          }
          hb_fptFileUnLockEx(pArea);
        }
      }
      if (pAlloc)
      {
        hb_xfree(pAlloc);
      }
    }

    return errCode;
  }
  return SUPER_PUTVALUE(&pArea->area, uiIndex, pItem);
}

// FPT METHODS

// Open a data store in the WorkArea.
// ( DBENTRYP_VO )    hb_fptOpen            : nullptr

// Retrieve the size of the WorkArea structure.
// ( DBENTRYP_SP )    hb_fptStructSize
static HB_ERRCODE hb_fptStructSize(FPTAREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptStrucSize(%p, %p)", pArea, uiSize));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(FPTAREA);
  return Harbour::SUCCESS;
}

// Obtain the length of a field value.
// ( DBENTRYP_SVL )   hb_fptGetVarLen
static HB_ERRCODE hb_fptGetVarLen(FPTAREAP pArea, HB_USHORT uiIndex, HB_ULONG *pLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength));
#endif

  if (pArea->fHasMemo && pArea->pMemoFile &&
      (pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::MEMO ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::IMAGE ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::BLOB ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::OLE))
  {
    auto fUnLock = false;

    HB_ERRCODE errCode = hb_fptLockForRead(pArea, uiIndex, &fUnLock);
    if (errCode == Harbour::SUCCESS)
    {
      *pLength = hb_fptGetMemoLen(pArea, uiIndex);
    }
    else
    {
      *pLength = 0;
    }

    if (fUnLock)
    {
      hb_fptFileUnLockSh(pArea);
    }

    return errCode;
  }

  return SUPER_GETVARLEN(&pArea->area, uiIndex, pLength);
}

// Obtain the current value of a field.
// ( DBENTRYP_SI )    hb_fptGetValue
static HB_ERRCODE hb_fptGetValue(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));
#endif

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = hb_fptGetVarField(pArea, uiIndex, pItem, nullptr);

  if (errCode != Harbour::SUCCESS)
  {
    if (errCode == Harbour::FAILURE)
    {
      return Harbour::FAILURE;
    }
    hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
  }
  return Harbour::SUCCESS;
}

// Assign a value to a field.
// ( DBENTRYP_SI )    hb_fptPutValue
static HB_ERRCODE hb_fptPutValue(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));
#endif

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = hb_fptPutVarField(pArea, uiIndex, pItem);
  if (errCode != Harbour::SUCCESS)
  {
    if (errCode == Harbour::FAILURE)
    {
      return Harbour::FAILURE;
    }
    hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, EF_CANDEFAULT);
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_V )     hb_fptCloseMemFile    : nullptr

// Create a memo file in the WorkArea.
// ( DBENTRYP_VO )    hb_fptCreateMemFile
static HB_ERRCODE hb_fptCreateMemFile(FPTAREAP pArea, LPDBOPENINFO pCreateInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptCreateMemFile(%p, %p)", pArea, pCreateInfo));
#endif

  FPTHEADER fptHeader;
  HB_ULONG ulNextBlock, ulSize, ulLen;

  if (pCreateInfo)
  {
    char szFileName[HB_PATH_MAX];
    PHB_FNAME pFileName;
    PHB_ITEM pError = nullptr, pItem = nullptr;
    auto bRetry = false;

    if (!pArea->bMemoType)
    {
      pItem = hb_itemPutNil(pItem);
      if (SELF_INFO(&pArea->area, DBI_MEMOTYPE, pItem) != Harbour::SUCCESS)
      {
        hb_itemRelease(pItem);
        return Harbour::FAILURE;
      }
      pArea->bMemoType = static_cast<HB_BYTE>(hb_itemGetNI(pItem));
#if 0
         if( !pArea->bMemoType ) {
            pArea->bMemoType = DB_MEMO_FPT;
            pArea->uiMemoVersion = DB_MEMOVER_FLEX;
         }
#endif
      if (pArea->bMemoType != DB_MEMO_DBT && pArea->bMemoType != DB_MEMO_FPT && pArea->bMemoType != DB_MEMO_SMT)
      {
        hb_memoErrorRT(pArea, EG_CREATE, EDBF_MEMOTYPE, pCreateInfo->abName, 0, 0);
        hb_itemRelease(pItem);
        return Harbour::FAILURE;
      }
    }
    if (!pArea->uiMemoVersion)
    {
      if (pArea->bMemoType == DB_MEMO_SMT)
      {
        pArea->uiMemoVersion = DB_MEMOVER_SIX;
      }
      else if (pArea->bMemoType == DB_MEMO_FPT)
      {
        pItem = hb_itemPutNil(pItem);
        if (SELF_INFO(&pArea->area, DBI_MEMOVERSION, pItem) != Harbour::SUCCESS)
        {
          hb_itemRelease(pItem);
          return Harbour::FAILURE;
        }
        pArea->uiMemoVersion = static_cast<HB_USHORT>(hb_itemGetNI(pItem));
      }
      else
      {
        pArea->uiMemoVersion = DB_MEMOVER_STD;
      }
    }
    if (!pArea->ulMemoBlockSize)
    {
      pItem = hb_itemPutNil(pItem);
      if (SELF_INFO(&pArea->area, DBI_MEMOBLOCKSIZE, pItem) != Harbour::SUCCESS)
      {
        hb_itemRelease(pItem);
        return Harbour::FAILURE;
      }
      pArea->ulMemoBlockSize = hb_itemGetNL(pItem);
    }

    if (!pArea->fTemporary)
    {
      // create file name
      pFileName = hb_fsFNameSplit(pCreateInfo->abName);
      if (!pFileName->szExtension)
      {
        pItem = hb_itemPutNil(pItem);
        if (SELF_INFO(&pArea->area, DBI_MEMOEXT, pItem) == Harbour::SUCCESS)
        {
          pFileName->szExtension = hb_itemGetCPtr(pItem);
          hb_fsFNameMerge(szFileName, pFileName);
        }
      }
      else
      {
        hb_strncpy(szFileName, pCreateInfo->abName, sizeof(szFileName) - 1);
      }
      hb_xfree(pFileName);
    }

    if (pItem != nullptr)
    {
      hb_itemRelease(pItem);
    }

    // Try create
    do
    {
      if (pArea->fTemporary)
      {
        pArea->pMemoFile = hb_fileCreateTempEx(szFileName, nullptr, nullptr, nullptr, FC_NORMAL);
      }
      else
      {
        pArea->pMemoFile = hb_fileExtOpen(szFileName, nullptr,
                                          FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE | FXO_DEFAULTS | FXO_SHARELOCK |
                                              FXO_COPYNAME | FXO_NOSEEKPOS,
                                          nullptr, pError);
      }
      if (!pArea->pMemoFile)
      {
        if (!pError)
        {
          pError = hb_errNew();
          hb_errPutGenCode(pError, EG_CREATE);
          hb_errPutSubCode(pError, EDBF_CREATE_MEMO);
          hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_CREATE));
          hb_errPutFileName(pError, static_cast<char *>(szFileName));
          hb_errPutFlags(pError, EF_CANRETRY);
        }
        hb_errPutOsCode(pError, hb_fsError());
        bRetry = (SELF_ERROR(&pArea->area, pError) == E_RETRY);
      }
      else
      {
        bRetry = false;
      }
    } while (bRetry);

    if (pError)
    {
      hb_itemRelease(pError);
    }

    if (!pArea->pMemoFile)
    {
      return Harbour::FAILURE;
    }

    pArea->szMemoFileName = hb_strdup(static_cast<char *>(szFileName));
  }
  // else -> For zap file

  memset(&fptHeader, 0, sizeof(fptHeader));
  ulSize = 512;
  if (pArea->uiMemoVersion == DB_MEMOVER_SIX)
  {
    memcpy(fptHeader.signature1, "SIxMemo", 8);
  }
  else
  {
    memcpy(fptHeader.signature1, "Harbour", 8);
    if (pArea->uiMemoVersion == DB_MEMOVER_FLEX || pArea->uiMemoVersion == DB_MEMOVER_CLIP)
    {
      memcpy(fptHeader.signature2, "FlexFile3\003", 11);
      ulSize = sizeof(FPTHEADER);
      if (pArea->area.rddID == s_uiRddIdBLOB)
      {
        HB_PUT_LE_UINT16(fptHeader.flexSize, static_cast<HB_U16>(pArea->ulMemoBlockSize));
      }
    }
  }
  ulNextBlock = (ulSize + pArea->ulMemoBlockSize - 1) / pArea->ulMemoBlockSize;
  if (pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT)
  {
    HB_PUT_LE_UINT32(fptHeader.nextBlock, ulNextBlock);
    HB_PUT_LE_UINT32(fptHeader.blockSize, static_cast<HB_U32>(pArea->ulMemoBlockSize));
  }
  else
  {
    HB_PUT_BE_UINT32(fptHeader.nextBlock, ulNextBlock);
    HB_PUT_BE_UINT32(fptHeader.blockSize, static_cast<HB_U32>(pArea->ulMemoBlockSize));
  }
  if (hb_fileWriteAt(pArea->pMemoFile, &fptHeader, ulSize, 0) != ulSize)
  {
    return Harbour::FAILURE;
  }

  ulLen = ulNextBlock * pArea->ulMemoBlockSize - ulSize;
  if (ulLen > ulSize)
  {
    memset(&fptHeader, 0, sizeof(fptHeader));
    do
    {
      HB_ULONG ulWrite = HB_MIN(ulLen - ulSize, sizeof(FPTHEADER));
      if (hb_fileWriteAt(pArea->pMemoFile, &fptHeader, ulWrite, ulSize) != ulWrite)
      {
        return Harbour::FAILURE;
      }
      ulSize += ulWrite;
    } while (ulLen > ulSize);
  }
  // trunc file
  hb_fileTruncAt(pArea->pMemoFile, ulSize);
  pArea->fMemoFlush = true;
  return Harbour::SUCCESS;
}

// BLOB2FILE - retrieve memo contents into file
// ( DBENTRYP_SCCS )  hb_fptGetValueFile
static HB_ERRCODE hb_fptGetValueFile(FPTAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode));
#endif

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  if (pArea->fHasMemo && pArea->pMemoFile &&
      (pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::MEMO ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::IMAGE ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::BLOB ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::OLE ||
       pArea->area.lpFields[uiIndex - 1].uiType == Harbour::DB::Field::ANY))
  {
    HB_ERRCODE errCode;
    PHB_FILE pFile;

    pFile = hb_fileExtOpen(szFile, nullptr,
                           FO_WRITE | FO_EXCLUSIVE | FXO_DEFAULTS | FXO_SHARELOCK |
                               (uiMode == FILEGET_APPEND ? FXO_APPEND : FXO_TRUNCATE),
                           nullptr, nullptr);

    if (pFile == nullptr)
    {
      errCode = uiMode != FILEGET_APPEND ? EDBF_CREATE : EDBF_OPEN_DBF;
    }
    else
    {
      hb_fileSeek(pFile, 0, FS_END);
      errCode = hb_fptGetVarField(pArea, uiIndex, nullptr, pFile);
      hb_fileClose(pFile);
    }

    // Exit if any error
    if (errCode != Harbour::SUCCESS)
    {
      if (errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode,
                       errCode == EDBF_OPEN_DBF || errCode == EDBF_CREATE || errCode == EDBF_WRITE
                           ? szFile
                           : pArea->szMemoFileName,
                       0, 0);
      }
      return Harbour::FAILURE;
    }
    return Harbour::SUCCESS;
  }
  return SUPER_GETVALUEFILE(&pArea->area, uiIndex, szFile, uiMode);
}

// Open a memo file in the specified WorkArea.
// ( DBENTRYP_VO )    hb_fptOpenMemFile
static HB_ERRCODE hb_fptOpenMemFile(FPTAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptOpenMemFile(%p, %p)", pArea, pOpenInfo));
#endif

  char szFileName[HB_PATH_MAX];
  PHB_FNAME pFileName;
  PHB_ITEM pError;
  HB_FATTR nFlags;
  auto bRetry = false;

  if (pArea->area.rddID == s_uiRddIdBLOB)
  {
    pArea->bMemoType = DB_MEMO_FPT;
    pArea->uiMemoVersion = DB_MEMOVER_FLEX;
  }
  else if (pArea->bMemoType != DB_MEMO_DBT && pArea->bMemoType != DB_MEMO_FPT && pArea->bMemoType != DB_MEMO_SMT)
  {
    hb_memoErrorRT(pArea, EG_OPEN, EDBF_MEMOTYPE, pOpenInfo->abName, 0, 0);
    return Harbour::FAILURE;
  }

  // create file name
  pFileName = hb_fsFNameSplit(pOpenInfo->abName);
  if (!pFileName->szExtension)
  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_INFO(&pArea->area, DBI_MEMOEXT, pItem) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pItem);
      hb_fsFNameMerge(szFileName, pFileName);
    }
    hb_itemRelease(pItem);
  }
  else
  {
    hb_strncpy(szFileName, pOpenInfo->abName, sizeof(szFileName) - 1);
  }
  hb_xfree(pFileName);

  nFlags = (pOpenInfo->fReadonly ? FO_READ : FO_READWRITE) | (pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE) |
           FXO_DEFAULTS | FXO_SHARELOCK | FXO_NOSEEKPOS;
  pError = nullptr;

  // Try open
  do
  {
    pArea->pMemoFile = hb_fileExtOpen(szFileName, nullptr, nFlags, nullptr, pError);
    if (!pArea->pMemoFile)
    {
      if (!pError)
      {
        pError = hb_errNew();
        hb_errPutGenCode(pError, EG_OPEN);
        hb_errPutSubCode(pError, EDBF_OPEN_MEMO);
        hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_OPEN));
        hb_errPutOsCode(pError, hb_fsError());
        hb_errPutFileName(pError, static_cast<char *>(szFileName));
        hb_errPutFlags(pError, EF_CANRETRY | EF_CANDEFAULT);
      }
      bRetry = (SELF_ERROR(&pArea->area, pError) == E_RETRY);
    }
    else
    {
      bRetry = false;
    }
  } while (bRetry);

  if (pError)
  {
    hb_itemRelease(pError);
  }

  if (!pArea->pMemoFile)
  {
    return Harbour::FAILURE;
  }

  pArea->szMemoFileName = hb_strdup(static_cast<char *>(szFileName));

  if (pArea->bMemoType == DB_MEMO_DBT)
  {
    pArea->ulMemoBlockSize = DBT_DEFBLOCKSIZE;
  }
  else
  {
    FPTHEADER fptHeader;
    memset(&fptHeader, 0, sizeof(fptHeader));
    if (hb_fptFileLockSh(pArea, true))
    {
      HB_SIZE nRead = hb_fileReadAt(pArea->pMemoFile, &fptHeader, sizeof(FPTHEADER), 0);
      if (nRead >= 512 && nRead != static_cast<HB_SIZE>(FS_ERROR))
      {
        pArea->uiMemoVersion = DB_MEMOVER_STD;
        if (pArea->bMemoType == DB_MEMO_SMT)
        {
          pArea->ulMemoBlockSize = HB_GET_LE_UINT32(fptHeader.blockSize);
        }
        else
        {
          pArea->ulMemoBlockSize = HB_GET_BE_UINT32(fptHeader.blockSize);
        }
        // hack for some buggy 3-rd part memo code implementations
        if (pArea->ulMemoBlockSize > 0x10000 && (pArea->ulMemoBlockSize & 0xFFFF) != 0)
        {
          pArea->ulMemoBlockSize &= 0xFFFF;
        }
        // Check for compatibility with SIX memo headers
        if (memcmp(fptHeader.signature1, "SIxMemo", 7) == 0)
        {
          pArea->uiMemoVersion = DB_MEMOVER_SIX;
          // Check for compatibility with CLIP (www.itk.ru) memo headers
        }
        else if (memcmp(fptHeader.signature1, "Made by CLIP", 12) == 0)
        {
          pArea->uiMemoVersion = DB_MEMOVER_CLIP;
        }
        // Check for compatibility with Clipper 5.3/FlexFile3 malformed memo headers
        if (pArea->uiMemoVersion != DB_MEMOVER_SIX && memcmp(fptHeader.signature2, "FlexFile3\003", 10) == 0)
        {
          HB_USHORT usSize = HB_GET_LE_UINT16(fptHeader.flexSize);
          pArea->uiMemoVersion = DB_MEMOVER_FLEX;
          if (usSize != 0 && (pArea->ulMemoBlockSize == 0 || pArea->area.rddID == s_uiRddIdBLOB))
          {
            pArea->ulMemoBlockSize = usSize;
          }
        }
      }
      hb_fptFileUnLockSh(pArea);
    }
  }

  if (pArea->ulMemoBlockSize == 0)
  {
    hb_memoErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, static_cast<char *>(pArea->szMemoFileName), 0, 0);
    return Harbour::FAILURE;
  }

  return Harbour::SUCCESS;
}

// FILE2BLOB - store file contents in MEMO
// ( DBENTRYP_SCCS )   hb_fptPutValueFile
static HB_ERRCODE hb_fptPutValueFile(FPTAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutValueFile(%p, %hu, %s, %hu)", pArea, uiIndex, szFile, uiMode));
#endif

  LPFIELD pField;

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->area.lpFields + uiIndex - 1;

  if (pArea->fHasMemo && pArea->pMemoFile &&
      (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
       pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE ||
       (pField->uiType == Harbour::DB::Field::ANY && pField->uiLen >= 6)))
  {
    HB_BOOL bDeleted;
    PHB_FILE pFile;

    // update any pending relations and reread record if necessary
    HB_ERRCODE errCode = SELF_DELETED(&pArea->area, &bDeleted);
    if (errCode != Harbour::SUCCESS)
    {
      return errCode;
    }

    if (!pArea->fPositioned)
    {
      return Harbour::FAILURE;
    }

    // Buffer is hot?
    if (!pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE)
    {
      return Harbour::FAILURE;
    }

    pFile = hb_fileExtOpen(szFile, nullptr, FO_READ | FO_DENYNONE | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, nullptr);
    if (pFile == nullptr)
    {
      errCode = EDBF_OPEN_DBF;
    }
    else if (pField->uiType == Harbour::DB::Field::ANY)
    {
      HB_ULONG ulSize;
      HB_FOFFSET size = hb_fileSize(pFile);

      ulSize = static_cast<HB_ULONG>(HB_MIN(size, HB_VF_CHAR));
      auto pAlloc = static_cast<HB_BYTE *>(hb_xgrab(ulSize + 1));
      if (hb_fileReadAt(pFile, pAlloc, ulSize, 0) != ulSize)
      {
        errCode = EDBF_READ;
        hb_xfree(pAlloc);
      }
      else
      {
        pAlloc[ulSize] = '\0';
      }
      hb_fileClose(pFile);
      if (errCode == Harbour::SUCCESS)
      {
        auto pItem = hb_itemPutCLPtr(nullptr, reinterpret_cast<char *>(pAlloc), ulSize);
        errCode = hb_fptPutVarField(pArea, uiIndex, pItem);
        hb_itemRelease(pItem);
      }
    }
    else if (!hb_fptFileLockEx(pArea, true))
    {
      hb_fileClose(pFile);
      errCode = EDBF_LOCK;
    }
    else
    {
      HB_ULONG ulSize, ulBlock, ulType, ulOldSize, ulOldType;
      HB_FOFFSET size = hb_fileSize(pFile);

      hb_fileSeek(pFile, 0, FS_SET);
      if (static_cast<HB_FOFFSET>(size & 0xFFFFFFFFUL) == size)
      {
        ulSize = HB_MIN(static_cast<HB_ULONG>(size), 0xFFFFFFFFUL - sizeof(FPTBLOCK));
      }
      else
      {
        ulSize = static_cast<HB_ULONG>(HB_MIN(size, static_cast<HB_FOFFSET>(0xFFFFFFFFUL - sizeof(FPTBLOCK))));
      }

      if (pArea->bMemoType == DB_MEMO_SMT)
      {
        ulType = SMT_IT_CHAR;
      }
      else
      {
        ulType = FPTIT_BINARY;
      }

      errCode = hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulOldSize, &ulOldType);
      if (errCode == Harbour::SUCCESS)
      {
        errCode = hb_fptWriteMemo(pArea, ulBlock, ulOldSize, nullptr, pFile, ulType, ulSize, &ulBlock);
      }
      if (errCode == Harbour::SUCCESS)
      {
        errCode = hb_dbfSetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, ulBlock, ulSize, ulType);
      }
#if defined(HB_MEMO_SAFELOCK)
      if (errCode == Harbour::SUCCESS)
      {
        // Force writer record to eliminate race condition
        SELF_GOCOLD(&pArea->area);
      }
#endif
      hb_fptFileUnLockEx(pArea);
      hb_fileClose(pFile);
    }
    // Exit if any error
    if (errCode != Harbour::SUCCESS)
    {
      hb_memoErrorRT(pArea, 0, errCode,
                     errCode == EDBF_OPEN_DBF || errCode == EDBF_READ ? szFile : pArea->szMemoFileName, 0, 0);
      return Harbour::FAILURE;
    }
    return Harbour::SUCCESS;
  }

  return SUPER_PUTVALUEFILE(&pArea->area, uiIndex, szFile, uiMode);
}

static HB_ERRCODE hb_fptDoPackRec(FPTAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptDoPackRec(%p)", pArea));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;
  HB_ULONG ulBlock, ulSize, ulType;
  HB_FOFFSET pos, from, size;

  // TODO: implement memo pack operation
  for (HB_USHORT uiField = 0; uiField < pArea->area.uiFieldCount; ++uiField)
  {
    LPFIELD pField = pArea->area.lpFields + uiField;

    if (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
        pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE)
    {
      errCode = hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiField, &ulBlock, &ulSize, &ulType);
      if (errCode == Harbour::SUCCESS && ulBlock != 0)
      {
        // Buffer is hot?
        if (!pArea->fRecordChanged)
        {
          errCode = SELF_GOHOT(&pArea->area);
        }
        if (ulSize == 0 && errCode == Harbour::SUCCESS)
        {
          if (pArea->bMemoType == DB_MEMO_DBT)
          {
            ulSize = hb_fptGetMemoLen(pArea, uiField + 1);
            if (ulSize)
            {
              ++ulSize;
            }
          }
          else if (pArea->bMemoType == DB_MEMO_FPT)
          {
            FPTBLOCK fptBlock;
            if (hb_fileReadAt(pArea->pMemoFile, &fptBlock, sizeof(FPTBLOCK), FPT_BLOCK_OFFSET(ulBlock)) ==
                sizeof(FPTBLOCK))
            {
              ulSize = HB_GET_BE_UINT32(fptBlock.size) + sizeof(FPTBLOCK);
            }
          }
        }
        if (ulSize && errCode == Harbour::SUCCESS)
        {
          from = FPT_BLOCK_OFFSET(ulBlock);
          pos = hb_fileSize(pArea->pMemoTmpFile);
          ulBlock = static_cast<HB_ULONG>((pos + pArea->ulNewBlockSize - 1) / pArea->ulNewBlockSize);
          pos = static_cast<HB_FOFFSET>(ulBlock) * static_cast<HB_FOFFSET>(pArea->ulNewBlockSize);
          errCode = hb_fptCopyToFile(pArea->pMemoFile, from, pArea->pMemoTmpFile, pos, ulSize);
        }
        else
        {
          ulBlock = ulType = 0;
        }

        if (errCode == Harbour::SUCCESS)
        {
          errCode = hb_dbfSetMemoData(static_cast<DBFAREAP>(pArea), uiField, ulBlock, ulSize, ulType);
        }
      }
    }
    else if (pField->uiType == Harbour::DB::Field::ANY && pField->uiLen >= 6)
    {
      HB_BYTE *pFieldBuf = pArea->pRecord + pArea->pFieldOffset[uiField];
      HB_BYTE buffer[4];

      ulBlock = HB_GET_LE_UINT32(pFieldBuf + pField->uiLen - 6);
      ulType = HB_GET_LE_UINT16(pFieldBuf + pField->uiLen - 2);
      size = 0;

      switch (ulType)
      {
      case HB_VF_BLOB:
      case HB_VF_BLOBCOMPRESS:
      case HB_VF_BLOBENCRYPT:
        if (hb_fileReadAt(pArea->pMemoFile, buffer, 4, FPT_BLOCK_OFFSET(ulBlock)) != 4)
        {
          errCode = EDBF_READ;
        }
        else
        {
          size = HB_GET_LE_UINT32(buffer) + 4;
        }
        break;
      case HB_VF_ARRAY:
        from = FPT_BLOCK_OFFSET(ulBlock);
        errCode = hb_fptCountSMTDataLength(pArea, &from);
        size = from - FPT_BLOCK_OFFSET(ulBlock);
        break;
      case HB_VF_DNUM:
        if (pField->uiLen <= 12)
        {
          size = 11;
        }
        break;
      default:
        if (ulType <= HB_VF_CHAR && (pField->uiLen - 2) < static_cast<int>(ulType))
        {
          size = ulType - (pField->uiLen - 6);
        }
        break;
      }
      if (errCode == Harbour::SUCCESS && size)
      {
        // Buffer is hot?
        if (!pArea->fRecordChanged)
        {
          errCode = SELF_GOHOT(&pArea->area);
        }
        if (errCode == Harbour::SUCCESS)
        {
          from = FPT_BLOCK_OFFSET(ulBlock);
          pos = hb_fileSize(pArea->pMemoTmpFile);
          ulBlock = static_cast<HB_ULONG>((pos + pArea->ulNewBlockSize - 1) / pArea->ulNewBlockSize);
          pos = static_cast<HB_FOFFSET>(ulBlock) * static_cast<HB_FOFFSET>(pArea->ulNewBlockSize);
          errCode = hb_fptCopyToFile(pArea->pMemoFile, from, pArea->pMemoTmpFile, pos, size);
          if (errCode == Harbour::SUCCESS)
          {
            HB_PUT_LE_UINT32(pFieldBuf + pField->uiLen - 6, ulBlock);
          }
        }
      }
    }
  }

  return errCode;
}

static HB_ERRCODE hb_fptDoPack(FPTAREAP pArea, HB_ULONG ulBlockSize, PHB_ITEM pEvalBlock, HB_LONG lEvalStep)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptDoPack(%p,%lu,%p,%ld)", pArea, ulBlockSize, pEvalBlock, lEvalStep));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->fReadonly)
  {
    errCode = EDBF_READONLY;
  }
  else if (pArea->fShared)
  {
    errCode = EDBF_SHARED;
  }
  else if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
  {
    return Harbour::FAILURE;
  }
  else if (pArea->fHasMemo && pArea->pMemoFile && pArea->pDataFile)
  {
    char szFile[HB_PATH_MAX];
    HB_ULONG ulRecords;
    HB_LONG lStep = lEvalStep;

    if (pEvalBlock && !pEvalBlock->isBlock())
    {
      pEvalBlock = nullptr;
    }

    errCode = SELF_RECCOUNT(&pArea->area, &ulRecords);
    if (errCode == Harbour::SUCCESS && ulRecords)
    {
      pArea->ulNewBlockSize = ulBlockSize && pArea->bMemoType != DB_MEMO_DBT ? ulBlockSize : pArea->ulMemoBlockSize;
      pArea->pMemoTmpFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szFile);
      if (pArea->pMemoTmpFile)
      {
        HB_ULONG ulMemoBlockSize = pArea->ulMemoBlockSize;
        PHB_FILE pFile = pArea->pMemoFile;

        pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
        pArea->pMemoFile = pArea->pMemoTmpFile;
        errCode = SELF_CREATEMEMFILE(&pArea->area, nullptr);
        pArea->pMemoFile = pFile;
        pArea->ulMemoBlockSize = ulMemoBlockSize;
        if (errCode == Harbour::SUCCESS)
        {
          if (pEvalBlock)
          {
            SELF_GOTO(&pArea->area, 0);
            pArea->area.fEof = false;
            hb_vmEvalBlock(pEvalBlock);
          }

          for (HB_ULONG ulRecNo = 1; ulRecNo <= ulRecords; ++ulRecNo)
          {
            HB_BOOL fDeleted;

            errCode = SELF_GOTO(&pArea->area, ulRecNo);
            if (errCode != Harbour::SUCCESS)
            {
              break;
            }
            if (pEvalBlock)
            {
              if (--lStep <= 0)
              {
                hb_vmEvalBlock(pEvalBlock);
                lStep = lEvalStep;
              }
            }

            // read record into bugger
            errCode = SELF_DELETED(&pArea->area, &fDeleted);
            if (errCode != Harbour::SUCCESS)
            {
              break;
            }
            errCode = hb_fptDoPackRec(pArea);
            if (errCode != Harbour::SUCCESS)
            {
              break;
            }
            errCode = SELF_GOCOLD(&pArea->area);
            if (errCode != Harbour::SUCCESS)
            {
              break;
            }
          }

          if (errCode == Harbour::SUCCESS && pEvalBlock)
          {
            SELF_GOTO(&pArea->area, 0);
            pArea->area.fBof = false;
            hb_vmEvalBlock(pEvalBlock);
          }
        }
        if (errCode == Harbour::SUCCESS)
        {
          HB_FOFFSET size = hb_fileSize(pArea->pMemoTmpFile);
          HB_ULONG ulNextBlock;
          HB_BYTE buffer[4];

          ulNextBlock = static_cast<HB_ULONG>((size + pArea->ulNewBlockSize - 1) / pArea->ulNewBlockSize);
          if (pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT)
          {
            HB_PUT_LE_UINT32(buffer, ulNextBlock);
          }
          else
          {
            HB_PUT_BE_UINT32(buffer, ulNextBlock);
          }
          hb_fileWriteAt(pArea->pMemoTmpFile, buffer, sizeof(buffer), 0);
          errCode = hb_fptCopyToFile(pArea->pMemoTmpFile, 0, pArea->pMemoFile, 0, size);
          hb_fileTruncAt(pArea->pMemoFile, size);
          pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
          if (errCode != Harbour::SUCCESS)
          {
            hb_memoErrorRT(pArea, 0, errCode,
                           errCode == EDBF_READ ? static_cast<char *>(szFile) : pArea->szMemoFileName, 0, 0);
            errCode = Harbour::FAILURE;
          }
        }
        hb_fileClose(pArea->pMemoTmpFile);
        hb_fileDelete(szFile);
        pArea->pMemoTmpFile = nullptr;
      }
    }
  }

  if (errCode != Harbour::SUCCESS && errCode != Harbour::FAILURE)
  {
    hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
    errCode = Harbour::FAILURE;
  }

  return errCode;
}

// Pack helper function called for each packed record
// ( DBENTRYP_LSP )   hb_fptPackRec,
static HB_ERRCODE hb_fptPackRec(FPTAREAP pArea, HB_ULONG ulRecNo, HB_BOOL *pfWritten)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPackRec(%p, %lu, %p)", pArea, ulRecNo, pfWritten));
#endif

  if (pArea->fPackMemo)
  {
    HB_ERRCODE errCode = SUPER_PACKREC(&pArea->area, ulRecNo, pfWritten);
    if (errCode == Harbour::SUCCESS && *pfWritten)
    {
      errCode = hb_fptDoPackRec(pArea);
      if (errCode != Harbour::SUCCESS && errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
        errCode = Harbour::FAILURE;
      }
    }
    return errCode;
  }

  return SUPER_PACKREC(&pArea->area, ulRecNo, pfWritten);
}

// Remove records marked for deletion from a database.
// ( DBENTRYP_V )     hb_fptPack,
static HB_ERRCODE hb_fptPack(FPTAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptPack(%p)", pArea));
#endif

  if (!pArea->fReadonly && !pArea->fShared && pArea->fHasMemo && pArea->pMemoFile && pArea->pDataFile)
  {
    char szFile[HB_PATH_MAX];

    if (SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }

    pArea->pMemoTmpFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szFile);
    if (pArea->pMemoTmpFile)
    {
      PHB_FILE pFile = pArea->pMemoFile;

      pArea->ulNewBlockSize = pArea->ulMemoBlockSize;

      pArea->pMemoFile = pArea->pMemoTmpFile;
      HB_ERRCODE errCode = SELF_CREATEMEMFILE(&pArea->area, nullptr);
      pArea->pMemoFile = pFile;

      if (errCode == Harbour::SUCCESS)
      {
        pArea->fPackMemo = true;
        errCode = SUPER_PACK(&pArea->area);
        pArea->fPackMemo = false;
        if (errCode == Harbour::SUCCESS)
        {
          HB_FOFFSET size = hb_fileSize(pArea->pMemoTmpFile);
          HB_ULONG ulNextBlock;
          HB_BYTE buffer[4];

          ulNextBlock = static_cast<HB_ULONG>((size + pArea->ulNewBlockSize - 1) / pArea->ulNewBlockSize);
          if (pArea->bMemoType == DB_MEMO_SMT || pArea->bMemoType == DB_MEMO_DBT)
          {
            HB_PUT_LE_UINT32(buffer, ulNextBlock);
          }
          else
          {
            HB_PUT_BE_UINT32(buffer, ulNextBlock);
          }
          hb_fileWriteAt(pArea->pMemoTmpFile, buffer, sizeof(buffer), 0);

          errCode = hb_fptCopyToFile(pArea->pMemoTmpFile, 0, pArea->pMemoFile, 0, size);
          hb_fileTruncAt(pArea->pMemoFile, size);
          pArea->ulMemoBlockSize = pArea->ulNewBlockSize;
          if (errCode != Harbour::SUCCESS)
          {
            hb_memoErrorRT(pArea, 0, errCode,
                           errCode == EDBF_READ ? static_cast<char *>(szFile) : pArea->szMemoFileName, 0, 0);
            errCode = Harbour::FAILURE;
          }
        }
      }
      hb_fileClose(pArea->pMemoTmpFile);
      hb_fileDelete(szFile);
      pArea->pMemoTmpFile = nullptr;
      return errCode;
    }
  }

  return SUPER_PACK(&pArea->area);
}

// Retrieve information about the current driver.
// ( DBENTRYP_SI )    hb_fptInfo
static HB_ERRCODE hb_fptInfo(FPTAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptInfo(%p, %hu, %p)", pArea, uiIndex, pItem));
#endif

  switch (uiIndex)
  {
  case DBI_MEMOEXT:
    if (pArea->fHasMemo && pArea->pMemoFile)
    {
      PHB_FNAME pFileName;

      pFileName = hb_fsFNameSplit(pArea->szMemoFileName);
      hb_itemPutC(pItem, pFileName->szExtension);
      hb_xfree(pFileName);
    }
    else
    {
      LPDBFDATA pData = DBFAREA_DATA(pArea);
      const char *szExt;
      if (pData->szMemoExt[0])
      {
        hb_itemPutC(pItem, pData->szMemoExt);
      }
      else if (pArea->bMemoType == DB_MEMO_FPT && (szExt = hb_setGetMFileExt()) != nullptr && *szExt)
      {
        hb_itemPutC(pItem, szExt);
      }
      else
      {
        szExt = hb_memoDefaultFileExt(pArea->bMemoType, pArea->area.rddID);
        if (!szExt)
        {
          szExt = hb_memoDefaultFileExt(hb_memoDefaultType(SELF_RDDNODE(&pArea->area), 0), pArea->area.rddID);
        }
        hb_itemPutC(pItem, szExt);
      }
    }
    break;

  case DBI_MEMOBLOCKSIZE:
    if (pArea->fHasMemo && pArea->pMemoFile)
    {
      hb_itemPutNL(pItem, pArea->ulMemoBlockSize);
    }
    else if (pArea->bMemoType && pArea->ulMemoBlockSize)
    {
      hb_itemPutNL(pItem, pArea->ulMemoBlockSize);
    }
    else if (pArea->bMemoType == DB_MEMO_DBT)
    {
      hb_itemPutNI(pItem, DBT_DEFBLOCKSIZE);
    }
    else
    {
      hb_itemClear(pItem);
      return SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_MEMOBLOCKSIZE, 0, pItem);
    }
    break;

  case DBI_MEMOTYPE:
    if (pArea->fHasMemo && pArea->pMemoFile)
    {
      hb_itemPutNI(pItem, pArea->bMemoType);
    }
    else if (pArea->bMemoType)
    {
      hb_itemPutNI(pItem, pArea->bMemoType);
    }
    else
    {
      hb_itemClear(pItem);
      return SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_MEMOTYPE, 0, pItem);
    }
    break;

  case DBI_MEMOVERSION:
    if (pArea->fHasMemo && pArea->pMemoFile)
    {
      hb_itemPutNI(pItem, pArea->uiMemoVersion);
    }
    else if (pArea->bMemoType != DB_MEMO_NONE && pArea->uiMemoVersion != 0)
    {
      hb_itemPutNI(pItem, pArea->uiMemoVersion);
    }
    else
    {
      hb_itemClear(pItem);
      return SELF_RDDINFO(SELF_RDDNODE(&pArea->area), RDDI_MEMOVERSION, 0, pItem);
    }
    break;

  case DBI_MEMOPACK:
    return hb_fptDoPack(pArea, hb_arrayGetNL(pItem, 1), hb_arrayGetItemPtr(pItem, 2), hb_arrayGetNL(pItem, 3));

    // case DBI_RDD_VERSION

  case DBI_BLOB_DIRECT_EXPORT: // BLOBDirectExport() { <nPointer>, <cTargetFile>, <kMOde> }
  {
    HB_ERRCODE errCode = Harbour::FAILURE;

    if (pItem->isArray())
    {
      HB_ULONG ulBlock = hb_arrayGetNL(pItem, 1);
      auto szFile = hb_arrayGetCPtr(pItem, 2);

      if (ulBlock && szFile && *szFile)
      {
        errCode = hb_fptGetVarFile(pArea, ulBlock, szFile, static_cast<HB_USHORT>(hb_arrayGetNI(pItem, 3)),
                                   FPT_DIRECT_TRANS(pArea));
      }
    }
    hb_itemPutL(pItem, errCode == Harbour::SUCCESS);
    break;
  }
  case DBI_BLOB_DIRECT_GET: // BLOBDirectGet() { <nPointer>, <nStart>, <nCount> }
    // pItem := { <nPointer>, <nStart>, <nCount> }
  {
    HB_ULONG ulBlock, ulStart, ulCount;

    if (pItem->isArray())
    {
      ulBlock = hb_arrayGetNL(pItem, 1);
      ulStart = hb_arrayGetNL(pItem, 2);
      if (ulStart)
      {
        --ulStart;
      }
      ulCount = hb_arrayGetNL(pItem, 3);
    }
    else
    {
      ulBlock = ulStart = ulCount = 0;
    }
    HB_ERRCODE errCode = hb_fptGetMemo(pArea, 0, pItem, nullptr, ulBlock, ulStart, ulCount, FPT_DIRECT_TRANS(pArea));
    if (errCode != Harbour::SUCCESS)
    {
      if (errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
      }
      return Harbour::FAILURE;
    }
    break;
  }
  case DBI_BLOB_DIRECT_IMPORT: // BLOBDirectImport() { <nOldPointer>, <cSourceFile> }
    if (pItem->isArray())
    {
      hb_itemPutNInt(pItem, hb_fptPutVarFile(pArea, hb_arrayGetNL(pItem, 1), hb_arrayGetCPtr(pItem, 2)));
    }
    else
    {
      hb_itemPutNI(pItem, 0);
    }
    break;

  case DBI_BLOB_DIRECT_PUT: // BLOBDirectPut() { <nOldPointer>, <xBlob> }
    // pItem := { <nOldPointer>, <xBlob> }
  {
    HB_ERRCODE errCode = EDBF_UNSUPPORTED;
    HB_ULONG ulBlock = 0;

    if (pItem->isArray())
    {
      auto pValue = hb_arrayGetItemPtr(pItem, 2);
      ulBlock = hb_arrayGetNL(pItem, 1);
      if (pValue)
      {
        if (hb_fptFileLockEx(pArea, true))
        {
          errCode = hb_fptPutMemo(pArea, 0, pValue, &ulBlock, FPT_DIRECT_TRANS(pArea));
          hb_fptFileUnLockEx(pArea);
        }
        else
        {
          errCode = EDBF_LOCK;
        }
      }
    }
    hb_itemPutNInt(pItem, ulBlock);
    if (errCode != Harbour::SUCCESS)
    {
      if (errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
      }
      return Harbour::FAILURE;
    }
    break;
  }
  case DBI_BLOB_ROOT_GET: // BLOBRootGet()
  {
    HB_ULONG ulBlock;

    HB_ERRCODE errCode = hb_fptGetRootBlock(pArea, &ulBlock);
    if (errCode == Harbour::SUCCESS)
    {
      errCode = hb_fptGetMemo(pArea, 0, pItem, nullptr, ulBlock, 0, 0, FPT_DIRECT_TRANS(pArea));
    }
    if (errCode != Harbour::SUCCESS)
    {
      if (errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
      }
      hb_itemClear(pItem);
      return Harbour::FAILURE;
    }
    break;
  }
  case DBI_BLOB_ROOT_PUT: // BLOBRootPut(<xBlob>)
  {
    HB_ULONG ulBlock;

    HB_ERRCODE errCode = hb_fptGetRootBlock(pArea, &ulBlock);
    if (errCode == Harbour::SUCCESS)
    {
      if (hb_fptFileLockEx(pArea, true))
      {
        errCode = hb_fptPutMemo(pArea, 0, pItem, &ulBlock, FPT_DIRECT_TRANS(pArea));
        hb_fptFileUnLockEx(pArea);
        if (errCode == Harbour::SUCCESS)
        {
          errCode = hb_fptPutRootBlock(pArea, ulBlock);
        }
      }
      else
      {
        errCode = EDBF_LOCK;
      }
    }
    if (errCode != Harbour::SUCCESS)
    {
      if (errCode != Harbour::FAILURE)
      {
        hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
      }
      hb_itemPutL(pItem, false);
      return Harbour::FAILURE;
    }
    hb_itemPutL(pItem, true);
    break;
  }
  case DBI_BLOB_ROOT_LOCK: // BLOBRootLock()
    hb_itemPutL(pItem, hb_fptRootBlockLock(pArea));
    break;

  case DBI_BLOB_ROOT_UNLOCK: // BLOBRootUnlock()
    hb_itemPutL(pItem, hb_fptRootBlockUnLock(pArea));
    break;

  case DBI_BLOB_DIRECT_LEN:
  case DBI_BLOB_DIRECT_TYPE:
  case DBI_BLOB_INTEGRITY:
  case DBI_BLOB_OFFSET:
  case DBI_BLOB_RECOVER:
    // TODO: implement it
    break;

  default:
    return SUPER_INFO(&pArea->area, uiIndex, pItem);
  }

  return Harbour::SUCCESS;
}

// Retrieve information about a field.
// ( DBENTRYP_SSI )   hb_fptFieldInfo
static HB_ERRCODE hb_fptFieldInfo(FPTAREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem));
#endif

  LPFIELD pField;

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = &pArea->area.lpFields[uiIndex - 1];

  if (pArea->fHasMemo && pArea->pMemoFile &&
      (pField->uiType == Harbour::DB::Field::MEMO || pField->uiType == Harbour::DB::Field::IMAGE ||
       pField->uiType == Harbour::DB::Field::BLOB || pField->uiType == Harbour::DB::Field::OLE))
  {
    HB_ULONG ulBlock, ulSize, ulType;
    HB_BOOL bDeleted;

    SELF_DELETED(&pArea->area, &bDeleted);
    switch (uiType)
    {
    case DBS_BLOB_GET: // BLOBGet() { <nStart>, <nCount> }
      // pItem := { <nStart>, <nCount> }
    {
      HB_ULONG ulStart, ulCount;
      int iTrans;

      if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        iTrans = FPT_TRANS_UNICODE;
      }
      else if ((pField->uiFlags & HB_FF_BINARY) == 0 && hb_vmCDP() != pArea->area.cdPage)
      {
        iTrans = FPT_TRANS_CP;
      }
      else
      {
        iTrans = FPT_TRANS_NONE;
      }

      if (pItem->isArray())
      {
        ulStart = hb_arrayGetNL(pItem, 1);
        if (ulStart)
        {
          --ulStart;
        }
        ulCount = hb_arrayGetNL(pItem, 2);
      }
      else
      {
        ulStart = ulCount = 0;
      }
      if (ulStart || ulCount)
      {
        iTrans = FPT_TRANS_NONE;
      }
      HB_ERRCODE errCode = hb_fptGetMemo(pArea, uiIndex, pItem, nullptr, 0, ulStart, ulCount, iTrans);
      if (errCode != Harbour::SUCCESS)
      {
        if (errCode != Harbour::FAILURE)
        {
          hb_memoErrorRT(pArea, 0, errCode, pArea->szMemoFileName, 0, 0);
        }
        return Harbour::FAILURE;
      }
      return Harbour::SUCCESS;
    }
    case DBS_BLOB_LEN:
      hb_itemPutNL(pItem, hb_fptGetMemoLen(pArea, uiIndex));
      return Harbour::SUCCESS;
    case DBS_BLOB_OFFSET:
      // Clipper 5.3 does not support it :-( [druzus]
      hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulSize, &ulType);
      hb_itemPutNInt(pItem, static_cast<HB_FOFFSET>(ulBlock) * pArea->ulMemoBlockSize +
                                (pArea->bMemoType == DB_MEMO_FPT ? sizeof(FPTBLOCK) : 0));
      return Harbour::SUCCESS;
    case DBS_BLOB_POINTER:
      // Clipper 5.3 it returns the same value as DBS_BLOB_OFFSET
      // in Harbour - it's a Clipper bug [druzus]
      hb_dbfGetMemoData(static_cast<DBFAREAP>(pArea), uiIndex - 1, &ulBlock, &ulSize, &ulType);
      hb_itemPutNL(pItem, ulBlock);
      return Harbour::SUCCESS;
    case DBS_BLOB_TYPE:
      hb_itemPutC(pItem, hb_fptGetMemoType(pArea, uiIndex));
      return Harbour::SUCCESS;
    }
  }
  return SUPER_FIELDINFO(&pArea->area, uiIndex, uiType, pItem);
}

// Retrieve (set) information about RDD
// ( DBENTRYP_RSLV )   hb_fptRddInfo
static HB_ERRCODE hb_fptRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_fptRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));
#endif

  LPDBFDATA pData;

  pData = DBFNODE_DATA(pRDD);

  switch (uiIndex)
  {
  case RDDI_MEMOEXT:
  {
    auto szExt = hb_itemGetCPtr(pItem);
    char *szNewVal;

    szNewVal = szExt[0] == '.' && szExt[1] ? hb_strdup(szExt) : nullptr;

    if (pData->szMemoExt[0])
    {
      hb_itemPutC(pItem, pData->szMemoExt);
    }
    else
    {
      int iType = hb_memoDefaultType(pRDD, ulConnect);

      if (iType == DB_MEMO_FPT && pRDD->rddID != s_uiRddIdBLOB && (szExt = hb_setGetMFileExt()) != nullptr && *szExt)
      {
        hb_itemPutC(pItem, szExt);
      }
      else
      {
        hb_itemPutC(pItem, hb_memoDefaultFileExt(iType ? iType : DB_MEMO_FPT, pRDD->rddID));
      }
    }
    if (szNewVal != nullptr)
    {
      hb_strncpy(pData->szMemoExt, szNewVal, sizeof(pData->szMemoExt) - 1);
      hb_xfree(szNewVal);
    }
    break;
  }
  case RDDI_MEMOBLOCKSIZE:
  {
    int iSize = hb_itemGetNI(pItem), iOldSize;

    if (pData->ulMemoBlockSize)
    {
      hb_itemPutNL(pItem, pData->ulMemoBlockSize);
    }
    else if ((iOldSize = hb_setGetMBlockSize()) > 0 && ((iOldSize <= 0x10000) || (iOldSize & 0xFFFF) == 0))
    {
      hb_itemPutNI(pItem, iOldSize);
    }
    else
    {
      switch (hb_memoDefaultType(pRDD, ulConnect))
      {
      case DB_MEMO_DBT:
        hb_itemPutNI(pItem, DBT_DEFBLOCKSIZE);
        break;
      case DB_MEMO_SMT:
        hb_itemPutNI(pItem, SMT_DEFBLOCKSIZE);
        break;
      default:
        hb_itemPutNI(pItem, FPT_DEFBLOCKSIZE);
        break;
      }
    }
    if (iSize > 0 && (iSize <= 0x10000 || (iSize & 0xFFFF) == 0))
    {
      pData->ulMemoBlockSize = iSize;
    }
    break;
  }
  case RDDI_MEMOTYPE:
  {
    int iType = hb_itemGetNI(pItem);

    hb_itemPutNI(pItem, pData->bMemoType ? pData->bMemoType : DB_MEMO_FPT);

    if (pRDD->rddID != s_uiRddIdBLOB)
    {
      switch (iType)
      {
      case DB_MEMO_DBT:
      case DB_MEMO_FPT:
      case DB_MEMO_SMT:
        pData->bMemoType = static_cast<HB_BYTE>(iType);
      }
    }
    break;
  }

  case RDDI_MEMOVERSION:
  {
    int iType = hb_itemGetNI(pItem);

    hb_itemPutNI(pItem, pData->bMemoExtType ? pData->bMemoExtType : DB_MEMOVER_FLEX);
    switch (iType)
    {
    case DB_MEMOVER_STD:
    case DB_MEMOVER_SIX:
    case DB_MEMOVER_FLEX:
    case DB_MEMOVER_CLIP:
      pData->bMemoExtType = static_cast<HB_BYTE>(iType);
    }
    break;
  }

  case RDDI_MEMOGCTYPE:
    hb_itemPutNI(pItem, 0);
    break;

  case RDDI_MEMOREADLOCK:
#if defined(HB_MEMO_SAFELOCK)
    hb_itemPutL(pItem, pRDD->rddID != s_uiRddIdBLOB);
#else
    hb_itemPutL(pItem, false);
#endif
    break;
  case RDDI_MEMOREUSE:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_BLOB_SUPPORT:
    hb_itemPutL(pItem, pRDD->rddID == s_uiRddIdBLOB);
    break;

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

static const RDDFUNCS fptTable = {
    // Movement and positioning methods

    (DBENTRYP_BP) nullptr,  // hb_fptBof
    (DBENTRYP_BP) nullptr,  // hb_fptEof
    (DBENTRYP_BP) nullptr,  // hb_fptFound
    (DBENTRYP_V) nullptr,   // hb_fptGoBottom
    (DBENTRYP_UL) nullptr,  // hb_fptGoTo
    (DBENTRYP_I) nullptr,   // hb_fptGoToId
    (DBENTRYP_V) nullptr,   // hb_fptGoTop
    (DBENTRYP_BIB) nullptr, // hb_fptSeek
    (DBENTRYP_L) nullptr,   // hb_fptSkip
    (DBENTRYP_L) nullptr,   // hb_fptSkipFilter
    (DBENTRYP_L) nullptr,   // hb_fptSkipRaw

    // Data management

    (DBENTRYP_VF) nullptr,                                                            // hb_fptAddField
    (DBENTRYP_B) nullptr,                                                             // hb_fptAppend
    (DBENTRYP_I) nullptr,                                                             // hb_fptCreateFields
    (DBENTRYP_V) nullptr,                                                             // hb_fptDeleteRec
    (DBENTRYP_BP) nullptr,                                                            // hb_fptDeleted
    (DBENTRYP_SP) nullptr,                                                            // hb_fptFieldCount
    (DBENTRYP_VF) nullptr,                                                            // hb_fptFieldDisplay
    (DBENTRYP_SSI)hb_fptFieldInfo, (DBENTRYP_SCP) nullptr,                            // hb_fptFieldName
    (DBENTRYP_V) nullptr,                                                             // hb_fptFlush
    (DBENTRYP_PP) nullptr,                                                            // hb_fptGetRec
    (DBENTRYP_SI)hb_fptGetValue, (DBENTRYP_SVL)hb_fptGetVarLen, (DBENTRYP_V) nullptr, // hb_fptGoCold
    (DBENTRYP_V) nullptr,                                                             // hb_fptGoHot
    (DBENTRYP_P) nullptr,                                                             // hb_fptPutRec
    (DBENTRYP_SI)hb_fptPutValue, (DBENTRYP_V) nullptr,                                // hb_fptRecall
    (DBENTRYP_ULP) nullptr,                                                           // hb_fptRecCount
    (DBENTRYP_ISI) nullptr,                                                           // hb_fptRecInfo
    (DBENTRYP_ULP) nullptr,                                                           // hb_fptRecNo
    (DBENTRYP_I) nullptr,                                                             // hb_fptRecId
    (DBENTRYP_S) nullptr,                                                             // hb_fptSetFieldExtent

    // WorkArea/Database management

    (DBENTRYP_CP) nullptr,                                                      // hb_fptAlias
    (DBENTRYP_V) nullptr,                                                       // hb_fptClose
    (DBENTRYP_VO) nullptr,                                                      // hb_fptCreate
    (DBENTRYP_SI)hb_fptInfo, (DBENTRYP_V) nullptr,                              // hb_fptNewArea
    (DBENTRYP_VO) nullptr,                                                      // hb_fptOpen
    (DBENTRYP_V) nullptr,                                                       // hb_fptRelease
    (DBENTRYP_SP)hb_fptStructSize, (DBENTRYP_CP) nullptr,                       // hb_fptSysName
    (DBENTRYP_VEI) nullptr,                                                     // hb_fptEval
    (DBENTRYP_V)hb_fptPack, (DBENTRYP_LSP)hb_fptPackRec, (DBENTRYP_VS) nullptr, // hb_fptSort
    (DBENTRYP_VT) nullptr,                                                      // hb_fptTrans
    (DBENTRYP_VT) nullptr,                                                      // hb_fptTransRec
    (DBENTRYP_V) nullptr,                                                       // hb_fptZap

    // Relational Methods

    (DBENTRYP_VR) nullptr,  // hb_fptChildEnd
    (DBENTRYP_VR) nullptr,  // hb_fptChildStart
    (DBENTRYP_VR) nullptr,  // hb_fptChildSync
    (DBENTRYP_V) nullptr,   // hb_fptSyncChildren
    (DBENTRYP_V) nullptr,   // hb_fptClearRel
    (DBENTRYP_V) nullptr,   // hb_fptForceRel
    (DBENTRYP_SSP) nullptr, // hb_fptRelArea
    (DBENTRYP_VR) nullptr,  // hb_fptRelEval
    (DBENTRYP_SI) nullptr,  // hb_fptRelText
    (DBENTRYP_VR) nullptr,  // hb_fptSetRel

    // Order Management

    (DBENTRYP_VOI) nullptr,  // hb_fptOrderListAdd
    (DBENTRYP_V) nullptr,    // hb_fptOrderListClear
    (DBENTRYP_VOI) nullptr,  // hb_fptOrderListDelete
    (DBENTRYP_VOI) nullptr,  // hb_fptOrderListFocus
    (DBENTRYP_V) nullptr,    // hb_fptOrderListRebuild
    (DBENTRYP_VOO) nullptr,  // hb_fptOrderCondition
    (DBENTRYP_VOC) nullptr,  // hb_fptOrderCreate
    (DBENTRYP_VOI) nullptr,  // hb_fptOrderDestroy
    (DBENTRYP_SVOI) nullptr, // hb_fptOrderInfo

    // Filters and Scope Settings

    (DBENTRYP_V) nullptr,    // hb_fptClearFilter
    (DBENTRYP_V) nullptr,    // hb_fptClearLocate
    (DBENTRYP_V) nullptr,    // hb_fptClearScope
    (DBENTRYP_VPLP) nullptr, // hb_fptCountScope
    (DBENTRYP_I) nullptr,    // hb_fptFilterText
    (DBENTRYP_SI) nullptr,   // hb_fptScopeInfo
    (DBENTRYP_VFI) nullptr,  // hb_fptSetFilter
    (DBENTRYP_VLO) nullptr,  // hb_fptSetLocate
    (DBENTRYP_VOS) nullptr,  // hb_fptSetScope
    (DBENTRYP_VPL) nullptr,  // hb_fptSkipScope
    (DBENTRYP_B) nullptr,    // hb_fptLocate

    // Miscellaneous

    (DBENTRYP_CC) nullptr, // hb_fptCompile
    (DBENTRYP_I) nullptr,  // hb_fptError
    (DBENTRYP_I) nullptr,  // hb_fptEvalBlock

    // Network operations

    (DBENTRYP_VSP) nullptr, // hb_fptRawLock
    (DBENTRYP_VL) nullptr,  // hb_fptLock
    (DBENTRYP_I) nullptr,   // hb_fptUnLock

    // Memofile functions

    (DBENTRYP_V) nullptr, // hb_fptCloseMemFile
    (DBENTRYP_VO)hb_fptCreateMemFile, (DBENTRYP_SCCS)hb_fptGetValueFile, (DBENTRYP_VO)hb_fptOpenMemFile,
    (DBENTRYP_SCCS)hb_fptPutValueFile,

    // Database file header handling

    (DBENTRYP_V) nullptr, // hb_fptReadDBHeader
    (DBENTRYP_V) nullptr, // hb_fptWriteDBHeader

    // non WorkArea functions

    (DBENTRYP_R) nullptr,     // hb_fptInit
    (DBENTRYP_R) nullptr,     // hb_fptExit
    (DBENTRYP_RVVL) nullptr,  // hb_fptDrop
    (DBENTRYP_RVVL) nullptr,  // hb_fptExists
    (DBENTRYP_RVVVL) nullptr, // hb_fptRename
    (DBENTRYP_RSLV)hb_fptRddInfo,

    // Special and reserved methods

    (DBENTRYP_SVP) nullptr // hb_fptWhoCares
};

HB_FUNC_TRANSLATE(DBFFPT, _DBF)

HB_FUNC(DBFDBT)
{
  ;
}

HB_FUNC(DBFSMT)
{
  ;
}

HB_FUNC(DBFBLOB)
{
  ;
}

static void hb_dbffptRegisterRDD(HB_USHORT *pusRddId)
{
  RDDFUNCS *pTable;
  HB_USHORT *puiCount, uiRddId, *puiSuperRddId;

  puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  pTable = static_cast<RDDFUNCS *>(hb_parptr(2));
  uiRddId = static_cast<HB_USHORT>(hb_parni(4));
  puiSuperRddId = static_cast<HB_USHORT *>(hb_parptr(5));

#if 0
   HB_TRACE(HB_TR_DEBUG, ("DBFFPT_GETFUNCTABLE(%p, %p)", puiCount, pTable));
#endif

  if (pTable)
  {
    if (puiCount)
    {
      *puiCount = RDDFUNCSCOUNT;
    }

    HB_ERRCODE errCode = hb_rddInheritEx(pTable, &fptTable, &fptSuper, "DBF", puiSuperRddId);
    if (errCode == Harbour::SUCCESS)
    {
      *pusRddId = uiRddId;
    }
    hb_retni(errCode);
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

HB_FUNC_STATIC(DBFFPT_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("DBFFPT_GETFUNCTABLE()"));
#endif

  hb_dbffptRegisterRDD(&s_uiRddIdFPT);
}

HB_FUNC_STATIC(DBFBLOB_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("DBFBLOB_GETFUNCTABLE()"));
#endif

  hb_dbffptRegisterRDD(&s_uiRddIdBLOB);
}

static void hb_dbffptRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("DBF", RDT_FULL) > 1 || hb_rddRegister("DBFFPT", RDT_FULL) > 1 ||
      hb_rddRegister("DBFBLOB", RDT_FULL) > 1)
  {
    hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
  }
}

HB_INIT_SYMBOLS_BEGIN(dbffpt1__InitSymbols){"DBFFPT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(DBFFPT)}, nullptr},
    {"DBFFPT_GETFUNCTABLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(DBFFPT_GETFUNCTABLE)}, nullptr},
    {"DBFBLOB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(DBFBLOB)}, nullptr},
    {"DBFBLOB_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(DBFBLOB_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(dbffpt1__InitSymbols)

        HB_CALL_ON_STARTUP_BEGIN(_hb_dbffpt_rdd_init_) hb_vmAtInit(hb_dbffptRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_dbffpt_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup dbffpt1__InitSymbols
#pragma startup _hb_dbffpt_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(dbffpt1__InitSymbols)                                                                                \
  HB_DATASEG_FUNC(_hb_dbffpt_rdd_init_)
#include "hbiniseg.hpp"
#endif
