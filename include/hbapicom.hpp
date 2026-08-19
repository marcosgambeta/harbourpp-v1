// Serial communication functions and constant values
// Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>

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

#ifndef HB_APICOM_H_
#define HB_APICOM_H_

#include "hbcom.ch"
#include "hbapi.hpp"

HB_EXTERN_BEGIN

#if defined(__cplusplus)
constexpr int32_t HB_COM_PORT_MAX = 256;
#else
#define HB_COM_PORT_MAX       256
#endif

#if defined(__cplusplus)
constexpr int32_t HB_COM_DEV_NAME_MAX = 64;
#else
#define HB_COM_DEV_NAME_MAX   64
#endif

#if defined(__cplusplus)
constexpr int32_t HB_COM_ANY = -1;
constexpr int32_t HB_COM_DISABLED = 0;
constexpr int32_t HB_COM_ENABLED = 1;
constexpr int32_t HB_COM_OPEN = 2;
#else
#define HB_COM_ANY            -1
#define HB_COM_DISABLED       0
#define HB_COM_ENABLED        1
#define HB_COM_OPEN           2
#endif

extern HB_EXPORT int32_t hb_comLastNum(void);
extern HB_EXPORT int32_t hb_comFindPort(const char *pszDevName, HB_BOOL fCreate);
extern HB_EXPORT int32_t hb_comOpen(int32_t iPort);
extern HB_EXPORT int32_t hb_comClose(int32_t iPort);
extern HB_EXPORT int32_t hb_comInit(int32_t iPort, int32_t iBaud, int32_t iParity, int32_t iSize, int32_t iStop);
extern HB_EXPORT long hb_comSend(int32_t iPort, const void *data, long len, HB_MAXINT timeout);
extern HB_EXPORT long hb_comRecv(int32_t iPort, void *data, long len, HB_MAXINT timeout);
extern HB_EXPORT void hb_comSetError(int32_t iPort, int32_t iError);
extern HB_EXPORT int32_t hb_comGetError(int32_t iPort);
extern HB_EXPORT int32_t hb_comGetOsError(int32_t iPort);
extern HB_EXPORT int32_t hb_comInputCount(int32_t iPort);
extern HB_EXPORT int32_t hb_comOutputCount(int32_t iPort);
extern HB_EXPORT int32_t hb_comFlush(int32_t iPort, int32_t iType);
extern HB_EXPORT int32_t hb_comMCR(int32_t iPort, int32_t *piValue, int32_t iClr, int32_t iSet);
extern HB_EXPORT int32_t hb_comMSR(int32_t iPort, int32_t *piValue);
extern HB_EXPORT int32_t hb_comLSR(int32_t iPort, int32_t *piValue);
extern HB_EXPORT int32_t hb_comSendBreak(int32_t iPort, int32_t iDurationInMilliSecs);
extern HB_EXPORT int32_t hb_comFlowControl(int32_t iPort, int32_t *piFlow, int32_t iFlow);
extern HB_EXPORT int32_t hb_comFlowSet(int32_t iPort, int32_t iFlow);
extern HB_EXPORT int32_t hb_comFlowChars(int32_t iPort, int32_t iXONchar, int32_t iXOFFchar);
extern HB_EXPORT int32_t hb_comDiscardChar(int32_t iPort, int32_t iChar);
extern HB_EXPORT int32_t hb_comErrorChar(int32_t iPort, int32_t iChar);
extern HB_EXPORT int32_t hb_comOutputState(int32_t iPort);
extern HB_EXPORT int32_t hb_comInputState(int32_t iPort);
extern HB_EXPORT int32_t hb_comSetDevice(int32_t iPort, const char *pszDevName);
extern HB_EXPORT const char *hb_comGetDevice(int32_t iPort, char *buffer, int32_t size);
extern HB_EXPORT HB_FHANDLE hb_comGetDeviceHandle(int32_t iPort);

HB_EXTERN_END

#endif // HB_APICOM_H_
