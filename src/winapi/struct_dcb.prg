/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

CLASS WASDCB

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD DCBlength
   ASSIGN DCBlength(n) INLINE ::setDCBlength(n)
   ACCESS DCBlength INLINE ::getDCBlength()
   METHOD setDCBlength
   METHOD getDCBlength

   // DWORD BaudRate
   ASSIGN BaudRate(n) INLINE ::setBaudRate(n)
   ACCESS BaudRate INLINE ::getBaudRate()
   METHOD setBaudRate
   METHOD getBaudRate

   // DWORD fBinary : 1
   ASSIGN fBinary(n) INLINE ::setfBinary(n)
   ACCESS fBinary INLINE ::getfBinary()
   METHOD setfBinary
   METHOD getfBinary

   // DWORD fParity : 1
   ASSIGN fParity(n) INLINE ::setfParity(n)
   ACCESS fParity INLINE ::getfParity()
   METHOD setfParity
   METHOD getfParity

   // DWORD fOutxCtsFlow : 1
   ASSIGN fOutxCtsFlow(n) INLINE ::setfOutxCtsFlow(n)
   ACCESS fOutxCtsFlow INLINE ::getfOutxCtsFlow()
   METHOD setfOutxCtsFlow
   METHOD getfOutxCtsFlow

   // DWORD fOutxDsrFlow : 1
   ASSIGN fOutxDsrFlow(n) INLINE ::setfOutxDsrFlow(n)
   ACCESS fOutxDsrFlow INLINE ::getfOutxDsrFlow()
   METHOD setfOutxDsrFlow
   METHOD getfOutxDsrFlow

   // DWORD fDtrControl : 2
   ASSIGN fDtrControl(n) INLINE ::setfDtrControl(n)
   ACCESS fDtrControl INLINE ::getfDtrControl()
   METHOD setfDtrControl
   METHOD getfDtrControl

   // DWORD fDsrSensitivity : 1
   ASSIGN fDsrSensitivity(n) INLINE ::setfDsrSensitivity(n)
   ACCESS fDsrSensitivity INLINE ::getfDsrSensitivity()
   METHOD setfDsrSensitivity
   METHOD getfDsrSensitivity

   // DWORD fTXContinueOnXoff : 1
   ASSIGN fTXContinueOnXoff(n) INLINE ::setfTXContinueOnXoff(n)
   ACCESS fTXContinueOnXoff INLINE ::getfTXContinueOnXoff()
   METHOD setfTXContinueOnXoff
   METHOD getfTXContinueOnXoff

   // DWORD fOutX : 1
   ASSIGN fOutX(n) INLINE ::setfOutX(n)
   ACCESS fOutX INLINE ::getfOutX()
   METHOD setfOutX
   METHOD getfOutX

   // DWORD fInX : 1
   ASSIGN fInX(n) INLINE ::setfInX(n)
   ACCESS fInX INLINE ::getfInX()
   METHOD setfInX
   METHOD getfInX

   // DWORD fErrorChar : 1
   ASSIGN fErrorChar(n) INLINE ::setfErrorChar(n)
   ACCESS fErrorChar INLINE ::getfErrorChar()
   METHOD setfErrorChar
   METHOD getfErrorChar

   // DWORD fNull : 1
   ASSIGN fNull(n) INLINE ::setfNull(n)
   ACCESS fNull INLINE ::getfNull()
   METHOD setfNull
   METHOD getfNull

   // DWORD fRtsControl : 2
   ASSIGN fRtsControl(n) INLINE ::setfRtsControl(n)
   ACCESS fRtsControl INLINE ::getfRtsControl()
   METHOD setfRtsControl
   METHOD getfRtsControl

   // DWORD fAbortOnError : 1
   ASSIGN fAbortOnError(n) INLINE ::setfAbortOnError(n)
   ACCESS fAbortOnError INLINE ::getfAbortOnError()
   METHOD setfAbortOnError
   METHOD getfAbortOnError

   // DWORD fDummy2 : 17
   ASSIGN fDummy2(n) INLINE ::setfDummy2(n)
   ACCESS fDummy2 INLINE ::getfDummy2()
   METHOD setfDummy2
   METHOD getfDummy2

   // WORD wReserved
   ASSIGN wReserved(n) INLINE ::setwReserved(n)
   ACCESS wReserved INLINE ::getwReserved()
   METHOD setwReserved
   METHOD getwReserved

   // WORD XonLim
   ASSIGN XonLim(n) INLINE ::setXonLim(n)
   ACCESS XonLim INLINE ::getXonLim()
   METHOD setXonLim
   METHOD getXonLim

   // WORD XoffLim
   ASSIGN XoffLim(n) INLINE ::setXoffLim(n)
   ACCESS XoffLim INLINE ::getXoffLim()
   METHOD setXoffLim
   METHOD getXoffLim

   // BYTE ByteSize
   ASSIGN ByteSize(n) INLINE ::setByteSize(n)
   ACCESS ByteSize INLINE ::getByteSize()
   METHOD setByteSize
   METHOD getByteSize

   // BYTE Parity
   ASSIGN Parity(n) INLINE ::setParity(n)
   ACCESS Parity INLINE ::getParity()
   METHOD setParity
   METHOD getParity

   // BYTE StopBits
   ASSIGN StopBits(n) INLINE ::setStopBits(n)
   ACCESS StopBits INLINE ::getStopBits()
   METHOD setStopBits
   METHOD getStopBits

   // char XonChar
   ASSIGN XonChar(n) INLINE ::setXonChar(n)
   ACCESS XonChar INLINE ::getXonChar()
   METHOD setXonChar
   METHOD getXonChar

   // char XoffChar
   ASSIGN XoffChar(n) INLINE ::setXoffChar(n)
   ACCESS XoffChar INLINE ::getXoffChar()
   METHOD setXoffChar
   METHOD getXoffChar

   // char ErrorChar
   ASSIGN ErrorChar(n) INLINE ::setErrorChar(n)
   ACCESS ErrorChar INLINE ::getErrorChar()
   METHOD setErrorChar
   METHOD getErrorChar

   // char EofChar
   ASSIGN EofChar(n) INLINE ::setEofChar(n)
   ACCESS EofChar INLINE ::getEofChar()
   METHOD setEofChar
   METHOD getEofChar

   // char EvtChar
   ASSIGN EvtChar(n) INLINE ::setEvtChar(n)
   ACCESS EvtChar INLINE ::getEvtChar()
   METHOD setEvtChar
   METHOD getEvtChar

   // WORD wReserved1
   ASSIGN wReserved1(n) INLINE ::setwReserved1(n)
   ACCESS wReserved1 INLINE ::getwReserved1()
   METHOD setwReserved1
   METHOD getwReserved1

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WASDCB
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WASDCB_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new DCB());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WASDCB_DELETE )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD DCBlength

HB_FUNC_STATIC( WASDCB_SETDCBLENGTH )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->DCBlength = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETDCBLENGTH )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->DCBlength);
  }
}

// DWORD BaudRate

HB_FUNC_STATIC( WASDCB_SETBAUDRATE )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->BaudRate = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETBAUDRATE )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->BaudRate);
  }
}

// DWORD fBinary : 1

HB_FUNC_STATIC( WASDCB_SETFBINARY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fBinary = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFBINARY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fBinary);
  }
}

// DWORD fParity : 1

HB_FUNC_STATIC( WASDCB_SETFPARITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fParity = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFPARITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fParity);
  }
}

// DWORD fOutxCtsFlow : 1

HB_FUNC_STATIC( WASDCB_SETFOUTXCTSFLOW )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fOutxCtsFlow = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFOUTXCTSFLOW )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fOutxCtsFlow);
  }
}

// DWORD fOutxDsrFlow : 1

HB_FUNC_STATIC( WASDCB_SETFOUTXDSRFLOW )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fOutxDsrFlow = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFOUTXDSRFLOW )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fOutxDsrFlow);
  }
}

// DWORD fDtrControl : 2

HB_FUNC_STATIC( WASDCB_SETFDTRCONTROL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDtrControl = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFDTRCONTROL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fDtrControl);
  }
}

// DWORD fDsrSensitivity : 1

HB_FUNC_STATIC( WASDCB_SETFDSRSENSITIVITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDsrSensitivity = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFDSRSENSITIVITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fDsrSensitivity);
  }
}

// DWORD fTXContinueOnXoff : 1

HB_FUNC_STATIC( WASDCB_SETFTXCONTINUEONXOFF )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fTXContinueOnXoff = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFTXCONTINUEONXOFF )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fTXContinueOnXoff);
  }
}

// DWORD fOutX : 1

HB_FUNC_STATIC( WASDCB_SETFOUTX )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fOutX = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFOUTX )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fOutX);
  }
}

// DWORD fInX : 1

HB_FUNC_STATIC( WASDCB_SETFINX )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fInX = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFINX )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fInX);
  }
}

// DWORD fErrorChar : 1

HB_FUNC_STATIC( WASDCB_SETFERRORCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fErrorChar = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFERRORCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fErrorChar);
  }
}

// DWORD fNull : 1

HB_FUNC_STATIC( WASDCB_SETFNULL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fNull = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFNULL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fNull);
  }
}

// DWORD fRtsControl : 2

HB_FUNC_STATIC( WASDCB_SETFRTSCONTROL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fRtsControl = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFRTSCONTROL )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fRtsControl);
  }
}

// DWORD fAbortOnError : 1

HB_FUNC_STATIC( WASDCB_SETFABORTONERROR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fAbortOnError = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFABORTONERROR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fAbortOnError);
  }
}

// DWORD fDummy2 : 17

HB_FUNC_STATIC( WASDCB_SETFDUMMY2 )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDummy2 = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETFDUMMY2 )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->fDummy2);
  }
}

// WORD wReserved

HB_FUNC_STATIC( WASDCB_SETWRESERVED )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wReserved = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETWRESERVED )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->wReserved);
  }
}

// WORD XonLim

HB_FUNC_STATIC( WASDCB_SETXONLIM )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->XonLim = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETXONLIM )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->XonLim);
  }
}

// WORD XoffLim

HB_FUNC_STATIC( WASDCB_SETXOFFLIM )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->XoffLim = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETXOFFLIM )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->XoffLim);
  }
}

// BYTE ByteSize

HB_FUNC_STATIC( WASDCB_SETBYTESIZE )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ByteSize = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETBYTESIZE )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->ByteSize);
  }
}

// BYTE Parity

HB_FUNC_STATIC( WASDCB_SETPARITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Parity = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETPARITY )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->Parity);
  }
}

// BYTE StopBits

HB_FUNC_STATIC( WASDCB_SETSTOPBITS )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->StopBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETSTOPBITS )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->StopBits);
  }
}

// char XonChar

HB_FUNC_STATIC( WASDCB_SETXONCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->XonChar = winapi_par_char(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETXONCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_char(obj->XonChar);
  }
}

// char XoffChar

HB_FUNC_STATIC( WASDCB_SETXOFFCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->XoffChar = winapi_par_char(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETXOFFCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_char(obj->XoffChar);
  }
}

// char ErrorChar

HB_FUNC_STATIC( WASDCB_SETERRORCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->ErrorChar = winapi_par_char(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETERRORCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_char(obj->ErrorChar);
  }
}

// char EofChar

HB_FUNC_STATIC( WASDCB_SETEOFCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->EofChar = winapi_par_char(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETEOFCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_char(obj->EofChar);
  }
}

// char EvtChar

HB_FUNC_STATIC( WASDCB_SETEVTCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->EvtChar = winapi_par_char(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETEVTCHAR )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_char(obj->EvtChar);
  }
}

// WORD wReserved1

HB_FUNC_STATIC( WASDCB_SETWRESERVED1 )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wReserved1 = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASDCB_GETWRESERVED1 )
{
  auto obj = static_cast<DCB*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->wReserved1);
  }
}

/*
typedef struct _DCB {
  DWORD DCBlength;
  DWORD BaudRate;
  DWORD fBinary : 1;
  DWORD fParity : 1;
  DWORD fOutxCtsFlow : 1;
  DWORD fOutxDsrFlow : 1;
  DWORD fDtrControl : 2;
  DWORD fDsrSensitivity : 1;
  DWORD fTXContinueOnXoff : 1;
  DWORD fOutX : 1;
  DWORD fInX : 1;
  DWORD fErrorChar : 1;
  DWORD fNull : 1;
  DWORD fRtsControl : 2;
  DWORD fAbortOnError : 1;
  DWORD fDummy2 : 17;
  WORD  wReserved;
  WORD  XonLim;
  WORD  XoffLim;
  BYTE  ByteSize;
  BYTE  Parity;
  BYTE  StopBits;
  char  XonChar;
  char  XoffChar;
  char  ErrorChar;
  char  EofChar;
  char  EvtChar;
  WORD  wReserved1;
} DCB, *LPDCB;
*/

#pragma ENDDUMP
