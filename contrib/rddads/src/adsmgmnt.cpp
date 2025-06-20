// Advantage Database Server RDD (Management functions)
// Copyright 2008 Viktor Szakats (vszakats.net/harbour)
// Copyright 2001 Brian Hays <bhays@abacuslaw.com>

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

#include "rddads.hpp"

static ADSHANDLE s_hMgmtHandle = 0;

HB_FUNC(ADSMGCONNECT)
{
  hb_retnl(AdsMgConnect(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parcx(1))) /* pucServerName */,
                        reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parc(2))) /* pucUserName */,
                        reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parc(3))) /* pucPassword */,
                        &s_hMgmtHandle));
}

HB_FUNC(ADSMGDISCONNECT)
{
  hb_retnl(AdsMgDisconnect(s_hMgmtHandle));

  s_hMgmtHandle = 0;
}

HB_FUNC(ADSMGGETHANDLE)
{
  hb_retnl(static_cast<long>(s_hMgmtHandle));
}

HB_FUNC(ADSMGSETHANDLE)
{
  s_hMgmtHandle = hb_parnl(1);
  hb_retl(true);
}

HB_FUNC(ADSMGKILLUSER)
{
  hb_retnl(static_cast<UNSIGNED16>(AdsMgKillUser(s_hMgmtHandle,
                                                 reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parc(1))),
                                                 static_cast<UNSIGNED16>(hb_parni(2)))));
}

// Determine OS ADS is running on; see ADS_MGMT_* constants
HB_FUNC(ADSMGGETSERVERTYPE)
{
  UNSIGNED16 usServerType = 0;

  hb_retnl(AdsMgGetServerType(s_hMgmtHandle, &usServerType) == AE_SUCCESS ? usServerType : 0);
}

HB_FUNC(ADSMGGETINSTALLINFO)
{
  ADS_MGMT_INSTALL_INFO stInstallInfo;
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_INSTALL_INFO);

  if (AdsMgGetInstallInfo(s_hMgmtHandle, &stInstallInfo, &usStructSize) == AE_SUCCESS)
  {
    hb_reta(8);
    hb_storvnl(stInstallInfo.ulUserOption, -1, 1);                                // User option purchased
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucRegisteredOwner), -1, 2); // Registered owner
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucVersionStr), -1, 3);      // Advantage version
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucInstallDate), -1, 4);     // Install date string
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucOemCharName), -1, 5);     // OEM char language
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucAnsiCharName), -1, 6);    // ANSI char language
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucEvalExpireDate), -1, 7);  // Eval expiration date
    hb_storvc(reinterpret_cast<char *>(stInstallInfo.aucSerialNumber), -1, 8);    // Serial number string
  }
  else
  {
    hb_reta(0);
  }

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_INSTALL_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetInstallInfo()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETACTIVITYINFO)
{
  ADS_MGMT_ACTIVITY_INFO stActivityInfo;
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_ACTIVITY_INFO);

  if (AdsMgGetActivityInfo(s_hMgmtHandle, &stActivityInfo, &usStructSize) == AE_SUCCESS)
  {
    switch (hb_parni(1) /* iOption */)
    {
    case 1:
      hb_retnl(stActivityInfo.ulOperations); // Number operations since started
      break;

    case 2:
      hb_retnl(stActivityInfo.ulLoggedErrors); // Number logged errors
      break;

    case 3:
      hb_reta(4); // Length of time ADS has been up
      hb_storvnl(stActivityInfo.stUpTime.usDays, -1, 1);
      hb_storvnl(stActivityInfo.stUpTime.usHours, -1, 2);
      hb_storvnl(stActivityInfo.stUpTime.usMinutes, -1, 3);
      hb_storvnl(stActivityInfo.stUpTime.usSeconds, -1, 4);
      break;

    case 4:
      hb_reta(3); // Users in use, max, rejected
      hb_storvnl(stActivityInfo.stUsers.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stUsers.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stUsers.ulRejected, -1, 3);
      break;

    case 5:
      hb_reta(3); // Conns in use, max, rejected
      hb_storvnl(stActivityInfo.stConnections.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stConnections.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stConnections.ulRejected, -1, 3);
      break;

    case 6:
      hb_reta(3); // WAs in use, max, rejected
      hb_storvnl(stActivityInfo.stWorkAreas.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stWorkAreas.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stWorkAreas.ulRejected, -1, 3);
      break;

    case 7:
      hb_reta(3); // Tables in use, max, rejected
      hb_storvnl(stActivityInfo.stTables.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stTables.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stTables.ulRejected, -1, 3);
      break;

    case 8:
      hb_reta(3); // Indexes in use, max, rejected
      hb_storvnl(stActivityInfo.stIndexes.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stIndexes.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stIndexes.ulRejected, -1, 3);
      break;

    case 9:
      hb_reta(3); // Locks in use, max, rejected
      hb_storvnl(stActivityInfo.stLocks.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stLocks.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stLocks.ulRejected, -1, 3);
      break;

    case 10:
      hb_reta(3); // TPS header elems in use, max
      hb_storvnl(stActivityInfo.stTpsHeaderElems.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stTpsHeaderElems.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stTpsHeaderElems.ulRejected, -1, 3);
      break;

    case 11:
      hb_reta(3); // TPS vis elems in use, max
      hb_storvnl(stActivityInfo.stTpsVisElems.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stTpsVisElems.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stTpsVisElems.ulRejected, -1, 3);
      break;

    case 12:
      hb_reta(3); // TPS memo elems in use, max
      hb_storvnl(stActivityInfo.stTpsMemoElems.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stTpsMemoElems.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stTpsMemoElems.ulRejected, -1, 3);
      break;

    case 13:
      hb_reta(3); // Worker threads in use, max
      hb_storvnl(stActivityInfo.stWorkerThreads.ulInUse, -1, 1);
      hb_storvnl(stActivityInfo.stWorkerThreads.ulMaxUsed, -1, 2);
      hb_storvnl(stActivityInfo.stWorkerThreads.ulRejected, -1, 3);
      break;

    default:
      hb_reta(0);
    }
  }
  else
  {
    hb_reta(0);
  }

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_ACTIVITY_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetActivityInfo()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETCOMMSTATS)
{
  ADS_MGMT_COMM_STATS stCommStats;
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_COMM_STATS);

  if (AdsMgGetCommStats(s_hMgmtHandle, &stCommStats, &usStructSize) == AE_SUCCESS)
  {
    hb_reta(11);
    hb_storvnd(stCommStats.dPercentCheckSums, -1, 1);   // % of pkts with checksum failures
    hb_storvnl(stCommStats.ulTotalPackets, -1, 2);      // Total packets received
    hb_storvnl(stCommStats.ulRcvPktOutOfSeq, -1, 3);    // Receive packets out of sequence
    hb_storvnl(stCommStats.ulNotLoggedIn, -1, 4);       // Packet owner not logged in
    hb_storvnl(stCommStats.ulRcvReqOutOfSeq, -1, 5);    // Receive requests out of sequence
    hb_storvnl(stCommStats.ulCheckSumFailures, -1, 6);  // Checksum failures
    hb_storvnl(stCommStats.ulDisconnectedUsers, -1, 7); // Server initiated disconnects
    hb_storvnl(stCommStats.ulPartialConnects, -1, 8);   // Removed partial connections 
    hb_storvnl(stCommStats.ulInvalidPackets, -1, 9);    // Rcvd invalid packets (NT only)
    hb_storvnl(stCommStats.ulRecvFromErrors, -1, 10);   // RecvFrom failed (NT only)
    hb_storvnl(stCommStats.ulSendToErrors, -1, 11);     // SendTo failed (NT only)
  }
  else
  {
    hb_reta(0);
  }

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_COMM_STATS))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetCommStats()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGRESETCOMMSTATS)
{
  hb_retnl(s_hMgmtHandle ? static_cast<long>(AdsMgResetCommStats(s_hMgmtHandle)) : -1);
}

HB_FUNC(ADSMGGETCONFIGINFO)
{
  ADS_MGMT_CONFIG_PARAMS stConfigValues;
  ADS_MGMT_CONFIG_MEMORY stConfigMemory;
  UNSIGNED16 usConfigValuesStructSize = sizeof(ADS_MGMT_CONFIG_PARAMS);
  UNSIGNED16 usConfigMemoryStructSize = sizeof(ADS_MGMT_CONFIG_MEMORY);

  if (AdsMgGetConfigInfo(s_hMgmtHandle, &stConfigValues, &usConfigValuesStructSize, &stConfigMemory,
                         &usConfigMemoryStructSize) == AE_SUCCESS)
  {
    switch (hb_parnidef(1, 1) /* iOption */)
    { // Pass 0 for Values, 1 for memory
    case 0:
      hb_reta(25);
      hb_storvnl(stConfigValues.ulNumConnections, -1, 1);         // number connections
      hb_storvnl(stConfigValues.ulNumWorkAreas, -1, 2);           // number work areas
      hb_storvnl(stConfigValues.ulNumTables, -1, 3);              // number tables
      hb_storvnl(stConfigValues.ulNumIndexes, -1, 4);             // number indexes
      hb_storvnl(stConfigValues.ulNumLocks, -1, 5);               // number locks
      hb_storvnl(stConfigValues.ulUserBufferSize, -1, 6);         // user buffer
      hb_storvnl(stConfigValues.ulStatDumpInterval, -1, 7);       // statistics dump interval
      hb_storvnl(stConfigValues.ulErrorLogMax, -1, 8);            // max size of error log
      hb_storvnl(stConfigValues.ulNumTPSHeaderElems, -1, 9);      // number TPS header elems
      hb_storvnl(stConfigValues.ulNumTPSVisibilityElems, -1, 10); // number TPS vis elems
      hb_storvnl(stConfigValues.ulNumTPSMemoTransElems, -1, 11);  // number TPS memo elems
      hb_storvnl(stConfigValues.usNumReceiveECBs, -1, 12);        // number rcv ECBs (NLM only)
      hb_storvnl(stConfigValues.usNumSendECBs, -1, 13);           // number send ECBs (NLM only)
      hb_storvnd(stConfigValues.usNumBurstPackets, -1, 14);       // number packets per burst
      hb_storvnl(stConfigValues.usNumWorkerThreads, -1, 15);      // number worker threads
#if ADS_LIB_VERSION >= 810
      hb_storvnl(stConfigValues.ulSortBuffSize, -1, 16); // index sort buffer size
      hb_storvni(0, -1, 17);                             // reserved
      hb_storvni(0, -1, 18);                             // reserved
#elif ADS_LIB_VERSION < 810
      hb_storvnl(stConfigValues.usSortBuffSize, -1, 16); // index sort buffer size
      hb_storvni(stConfigValues.ucReserved1, -1, 17);    // reserved
      hb_storvni(stConfigValues.ucReserved2, -1, 18);    // reserved
#else // not currently used
      hb_storvnl(0, -1, 16); // index sort buffer size
      hb_storvni(0, -1, 17); // reserved
      hb_storvni(0, -1, 18); // reserved
#endif
      hb_storvc(reinterpret_cast<char *>(stConfigValues.aucErrorLog), -1, 19);    // error log path
      hb_storvc(reinterpret_cast<char *>(stConfigValues.aucSemaphore), -1, 20);   // semaphore file path
      hb_storvc(reinterpret_cast<char *>(stConfigValues.aucTransaction), -1, 21); // TPS log file path
      hb_storvni(stConfigValues.ucReserved3, -1, 22);                             // reserved
      hb_storvni(stConfigValues.ucReserved4, -1, 23);                             // reserved
      hb_storvnl(stConfigValues.usSendIPPort, -1, 24);                            // NT Service IP send port #
      hb_storvnl(stConfigValues.usReceiveIPPort, -1, 25);                         // NT Service IP rcv port #
      // hb_storvnl(stConfigValues.usReserved5             , -1, 26);     reserved
      break;

    case 1:
      hb_reta(13);
      hb_storvnd(stConfigMemory.ulTotalConfigMem, -1, 1);       // Total memory taken by config parameters
      hb_storvnl(stConfigMemory.ulConnectionMem, -1, 2);        // memory taken by connections
      hb_storvnl(stConfigMemory.ulWorkAreaMem, -1, 3);          // memory taken by work areas
      hb_storvnl(stConfigMemory.ulTableMem, -1, 4);             // memory taken by tables
      hb_storvnl(stConfigMemory.ulIndexMem, -1, 5);             // memory taken by indexes
      hb_storvnl(stConfigMemory.ulLockMem, -1, 6);              // memory taken by locks
      hb_storvnl(stConfigMemory.ulUserBufferMem, -1, 7);        // memory taken by user buffer
      hb_storvnl(stConfigMemory.ulTPSHeaderElemMem, -1, 8);     // memory taken by TPS hdr elems
      hb_storvnl(stConfigMemory.ulTPSVisibilityElemMem, -1, 9); // memory taken by TPS vis elems
      hb_storvnl(stConfigMemory.ulTPSMemoTransElemMem, -1, 10); // memory taken by TPS memo elems
      hb_storvnl(stConfigMemory.ulReceiveEcbMem, -1, 11);       // memory taken by rcv ECBs (NLM)
      hb_storvnl(stConfigMemory.ulSendEcbMem, -1, 12);          // memory taken by send ECBs (NLM)
      hb_storvnl(stConfigMemory.ulWorkerThreadMem, -1, 13);     // memory taken by worker threads
      break;

    default:
      hb_reta(0);
    }
  }
  else
  {
    hb_reta(0);
  }

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usConfigValuesStructSize > sizeof(ADS_MGMT_CONFIG_PARAMS))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetConfigInfo()" ) );
#endif
  }

  if (usConfigMemoryStructSize > sizeof(ADS_MGMT_CONFIG_MEMORY))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetConfigInfo()" ) );
#endif
  }
#endif
}

// ADS_MGMT_USER_INFO astUserInfo[MAX_NUM_USERS];
// bh:  Enhancement:  Get # of tables from ADS_MGMT_ACTIVITY_INFO.stUsers instead of set size.

// Return array of connected users
HB_FUNC(ADSMGGETUSERNAMES)
{
  auto usArrayLen = static_cast<UNSIGNED16>(
      hb_parnidef(2, 2000)); // needed for array memory allocation; caller can set with 2nd arg
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_USER_INFO);
  auto pastUserInfo = static_cast<ADS_MGMT_USER_INFO *>(hb_xgrab(sizeof(ADS_MGMT_USER_INFO) * usArrayLen));

  if (AdsMgGetUserNames(s_hMgmtHandle, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parc(1))) /* pucFileName */,
                        pastUserInfo, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      auto pArrayItm = hb_arrayGetItemPtr(pArray, ulCount);
      hb_arrayNew(pArrayItm, 6);

      hb_arraySetC(pArrayItm, 1, reinterpret_cast<char *>(pastUserInfo[ulCount - 1].aucUserName));
      hb_arraySetNL(pArrayItm, 2, pastUserInfo[ulCount - 1].usConnNumber);
#if ADS_LIB_VERSION >= 600
      hb_arraySetC(pArrayItm, 3, reinterpret_cast<char *>(pastUserInfo[ulCount - 1].aucAddress));
#else
      hb_arraySetC(pArrayItm, 3, nullptr);
#endif
#if ADS_LIB_VERSION >= 800
      hb_arraySetC(pArrayItm, 4, reinterpret_cast<char *>(pastUserInfo[ulCount - 1].aucAuthUserName));
      hb_arraySetC(pArrayItm, 5, reinterpret_cast<char *>(pastUserInfo[ulCount - 1].aucOSUserLoginName));
#else
      hb_arraySetC(pArrayItm, 4, nullptr);
      hb_arraySetC(pArrayItm, 5, nullptr);
#endif
#if ADS_LIB_VERSION >= 810
      hb_arraySetC(pArrayItm, 6, reinterpret_cast<char *>(pastUserInfo[ulCount - 1].aucTSAddress));
#else
      hb_arraySetC(pArrayItm, 6, nullptr);
#endif
    }
    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(pastUserInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_USER_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetUserNames()" ) );
#endif
  }
#endif
}

// NOTE: returns an array of 5 elements if successful
//       [1] Client machine name when server runs on NT/2000
//           Client Username when server runs on NetWare
//       [2] NetWare connection number
//       [3] Login user name for data dictionary connections (ADS 6.0 and above)
//       [4] Client machine IP address (ADS 6.0 and above)
//       [5] lock type ADS_MGMT_NO_LOCK ADS_MGMT_RECORD_LOCK ADS_MGMT_FILE_LOCK
//
//       returns the advantage error code if it fails.
HB_FUNC(ADSMGGETLOCKOWNER)
{
  UNSIGNED16 pusLockType = 0;
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_USER_INFO);
  auto pstUserInfo = static_cast<ADS_MGMT_USER_INFO *>(hb_xgrab(sizeof(ADS_MGMT_USER_INFO)));

  if (AdsMgGetLockOwner(s_hMgmtHandle,
                        reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_parcx(1))) /* pucTableName */,
                        static_cast<UNSIGNED32>(hb_parnl(2)) /* ulRecordNumber */, pstUserInfo, &usStructSize,
                        &pusLockType) == AE_SUCCESS)
  {
    hb_reta(5);
    hb_storvc(reinterpret_cast<char *>(pstUserInfo->aucUserName), -1, 1);  // Machine name under NT
    hb_storvnl(static_cast<UNSIGNED16>(pstUserInfo->usConnNumber), -1, 2); // NetWare conn # (NLM only)
#if ADS_LIB_VERSION >= 600
    hb_storvc(reinterpret_cast<char *>(pstUserInfo->aucAuthUserName), -1, 3); // logon name with Data Dictionary
    hb_storvc(reinterpret_cast<char *>(pstUserInfo->aucAddress), -1, 4);      // IP address
#else
    hb_storvc(nullptr, -1, 3); // logon name with Data Dictionary
    hb_storvc(nullptr, -1, 4); // IP address
#endif
    hb_storvnl(pusLockType, -1, 5); // type of lock
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(pstUserInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_USER_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetLockOwner()" ) );
#endif
  }
#endif
}

// NOTE: For a newer edition of this function, which also returns lock type
//       info, see AdsMgGetOpenTables2().
HB_FUNC(ADSMGGETOPENTABLES) // nMaxNumberOfFilesToReturn, cUserName, nConnection
{
  auto usArrayLen = static_cast<UNSIGNED16>(hb_parnidef(1, 300));
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_TABLE_INFO);
  auto astOpenTableInfo = static_cast<ADS_MGMT_TABLE_INFO *>(hb_xgrab(sizeof(ADS_MGMT_TABLE_INFO) * usArrayLen));

  if (AdsMgGetOpenTables(
          s_hMgmtHandle,
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(2) > 0 ? const_cast<char *>(hb_parc(2)) : nullptr) /* pucUserName */,
          static_cast<UNSIGNED16>(
              hb_parni(3)) /* usConnNumber */, /* = HB_ADS_PARCONNECTION(3) only valid for NetWare so don't default to
                                                  current, only take a passed value */
          astOpenTableInfo, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      hb_arraySetC(pArray, static_cast<HB_ULONG>(ulCount),
                   reinterpret_cast<char *>(astOpenTableInfo[ulCount - 1].aucTableName));
    }

    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(astOpenTableInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_TABLE_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetOpenTables()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETOPENTABLES2) // nMaxNumberOfFilesToReturn, cUserName, nConnection
{
  auto usArrayLen = static_cast<UNSIGNED16>(hb_parnidef(1, 300));
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_TABLE_INFO);
  auto astOpenTableInfo = static_cast<ADS_MGMT_TABLE_INFO *>(hb_xgrab(sizeof(ADS_MGMT_TABLE_INFO) * usArrayLen));

  if (AdsMgGetOpenTables(
          s_hMgmtHandle,
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(2) > 0 ? const_cast<char *>(hb_parc(2)) : nullptr) /* pucUserName */,
          static_cast<UNSIGNED16>(
              hb_parni(3)) /* usConnNumber */, /* = HB_ADS_PARCONNECTION(3) only valid for NetWare so don't default to
                                                  current, only take a passed value */
          astOpenTableInfo, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      auto pArrayItm = hb_arrayGetItemPtr(pArray, ulCount);
      hb_arrayNew(pArrayItm, 2);

      hb_arraySetC(pArrayItm, 1, reinterpret_cast<char *>(astOpenTableInfo[ulCount - 1].aucTableName));
      hb_arraySetNI(pArrayItm, 2, astOpenTableInfo[ulCount - 1].usLockType); // Advantage locking mode
    }

    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(astOpenTableInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_TABLE_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetOpenTables()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETOPENINDEXES) // nMaxNumberOfFilesToReturn, cTableName, cUserName, nConnection
{
  auto usArrayLen = static_cast<UNSIGNED16>(hb_parnidef(1, 300));
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_INDEX_INFO);
  auto astOpenIndexInfo = static_cast<ADS_MGMT_INDEX_INFO *>(hb_xgrab(sizeof(ADS_MGMT_INDEX_INFO) * usArrayLen));

  if (AdsMgGetOpenIndexes(
          s_hMgmtHandle,
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(2) > 0
                                            ? const_cast<char *>(hb_parc(2))
                                            : nullptr) /* pucTableName */, // fully qualified path to that table
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(3) > 0 ? const_cast<char *>(hb_parc(3)) : nullptr) /* pucUserName */,
          static_cast<UNSIGNED16>(
              hb_parni(4)) /* usConnNumber */, /* = HB_ADS_PARCONNECTION(4) only valid for NetWare so don't default to
                                                  current, only take a passed value */
          astOpenIndexInfo, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      hb_arraySetC(pArray, static_cast<HB_ULONG>(ulCount),
                   reinterpret_cast<char *>(astOpenIndexInfo[ulCount - 1].aucIndexName));
    }

    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(astOpenIndexInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_INDEX_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetOpenIndexes()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETLOCKS)
{
  auto usArrayLen = static_cast<UNSIGNED16>(hb_parnidef(1, 2000));
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_RECORD_INFO);
  auto astRecordInfo = static_cast<ADS_MGMT_RECORD_INFO *>(hb_xgrab(sizeof(ADS_MGMT_RECORD_INFO) * usArrayLen));

  if (AdsMgGetLocks(
          s_hMgmtHandle,
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(2) > 0
                                            ? const_cast<char *>(hb_parc(2))
                                            : nullptr) /* pucTableName */, // fully qualified path to that table
          reinterpret_cast<UNSIGNED8 *>(hb_parclen(3) > 0 ? const_cast<char *>(hb_parc(3)) : nullptr) /* pucUserName */,
          static_cast<UNSIGNED16>(
              hb_parni(4)) /* usConnNumber */, /* = HB_ADS_PARCONNECTION(4) only valid for NetWare so don't default to
                                                  current, only take a passed value */
          astRecordInfo, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      hb_arraySetNL(pArray, static_cast<HB_ULONG>(ulCount), astRecordInfo[ulCount - 1].ulRecordNumber);
    }

    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(astRecordInfo);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_RECORD_INFO))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetLocks()" ) );
#endif
  }
#endif
}

HB_FUNC(ADSMGGETWORKERTHREADACTIVITY)
{
  auto usArrayLen = static_cast<UNSIGNED16>(hb_parnidef(1, 2000));
  UNSIGNED16 usStructSize = sizeof(ADS_MGMT_THREAD_ACTIVITY);
  auto astWorkerThreadActivity =
      static_cast<ADS_MGMT_THREAD_ACTIVITY *>(hb_xgrab(sizeof(ADS_MGMT_THREAD_ACTIVITY) * usArrayLen));

  if (AdsMgGetWorkerThreadActivity(s_hMgmtHandle, astWorkerThreadActivity, &usArrayLen, &usStructSize) == AE_SUCCESS)
  {
    auto pArray = hb_itemArrayNew(usArrayLen);

    for (UNSIGNED16 ulCount = 1; ulCount <= usArrayLen; ulCount++)
    {
      auto pArrayItm = hb_arrayGetItemPtr(pArray, ulCount);
      hb_arrayNew(pArrayItm, 6);

      hb_arraySetNL(pArrayItm, 1, astWorkerThreadActivity[ulCount - 1].ulThreadNumber);
      hb_arraySetNI(pArrayItm, 2, astWorkerThreadActivity[ulCount - 1].usOpCode);
      hb_arraySetC(pArrayItm, 3, reinterpret_cast<char *>(astWorkerThreadActivity[ulCount - 1].aucUserName));
      hb_arraySetNI(pArrayItm, 4, astWorkerThreadActivity[ulCount - 1].usConnNumber);
      hb_arraySetNI(pArrayItm, 5, astWorkerThreadActivity[ulCount - 1].usReserved1);
#if ADS_LIB_VERSION >= 800
      hb_arraySetC(pArrayItm, 6, reinterpret_cast<char *>(astWorkerThreadActivity[ulCount - 1].aucOSUserLoginName));
#else
      hb_arraySetC(pArrayItm, 6, nullptr);
#endif
    }
    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }

  hb_xfree(astWorkerThreadActivity);

#if HB_TR_LEVEL >= HB_TR_INFO
  if (usStructSize > sizeof(ADS_MGMT_THREAD_ACTIVITY))
  {
#if 0
      HB_TRACE( HB_TR_INFO, ( "%s returned extra data; available with newer client lib.", "AdsMgGetWorkerThreadActivity()" ) );
#endif
  }
#endif
}
