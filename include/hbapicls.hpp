// Harbour class API
// Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>

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

#ifndef HB_APICLS_H_
#define HB_APICLS_H_

#include "hbapi.hpp"

HB_EXTERN_BEGIN

#ifdef _HB_API_INTERNAL_

// TODO: enum ?
#if defined(__cplusplus)
constexpr HB_USHORT HB_OO_OP_PLUS = 0;
constexpr HB_USHORT HB_OO_OP_MINUS = 1;
constexpr HB_USHORT HB_OO_OP_MULT = 2;
constexpr HB_USHORT HB_OO_OP_DIVIDE = 3;
constexpr HB_USHORT HB_OO_OP_MOD = 4;
constexpr HB_USHORT HB_OO_OP_POWER = 5;
constexpr HB_USHORT HB_OO_OP_INC = 6;
constexpr HB_USHORT HB_OO_OP_DEC = 7;
constexpr HB_USHORT HB_OO_OP_EQUAL = 8;
constexpr HB_USHORT HB_OO_OP_EXACTEQUAL = 9;
constexpr HB_USHORT HB_OO_OP_NOTEQUAL = 10;
constexpr HB_USHORT HB_OO_OP_LESS = 11;
constexpr HB_USHORT HB_OO_OP_LESSEQUAL = 12;
constexpr HB_USHORT HB_OO_OP_GREATER = 13;
constexpr HB_USHORT HB_OO_OP_GREATEREQUAL = 14;
constexpr HB_USHORT HB_OO_OP_ASSIGN = 15;
constexpr HB_USHORT HB_OO_OP_INSTRING = 16;
constexpr HB_USHORT HB_OO_OP_INCLUDE = 17;
constexpr HB_USHORT HB_OO_OP_NOT = 18;
constexpr HB_USHORT HB_OO_OP_AND = 19;
constexpr HB_USHORT HB_OO_OP_OR = 20;
constexpr HB_USHORT HB_OO_OP_ARRAYINDEX = 21;
constexpr HB_USHORT HB_OO_OP_ENUMINDEX = 22;
constexpr HB_USHORT HB_OO_OP_ENUMBASE = 23;
constexpr HB_USHORT HB_OO_OP_ENUMVALUE = 24;
constexpr HB_USHORT HB_OO_OP_ENUMSTART = 25;
constexpr HB_USHORT HB_OO_OP_ENUMSKIP = 26;
constexpr HB_USHORT HB_OO_OP_ENUMSTOP = 27;
constexpr HB_USHORT HB_OO_OP_ENUMISFIRST = 28;
constexpr HB_USHORT HB_OO_OP_ENUMISLAST = 29;
#else
#define HB_OO_OP_PLUS         0
#define HB_OO_OP_MINUS        1
#define HB_OO_OP_MULT         2
#define HB_OO_OP_DIVIDE       3
#define HB_OO_OP_MOD          4
#define HB_OO_OP_POWER        5
#define HB_OO_OP_INC          6
#define HB_OO_OP_DEC          7
#define HB_OO_OP_EQUAL        8
#define HB_OO_OP_EXACTEQUAL   9
#define HB_OO_OP_NOTEQUAL     10
#define HB_OO_OP_LESS         11
#define HB_OO_OP_LESSEQUAL    12
#define HB_OO_OP_GREATER      13
#define HB_OO_OP_GREATEREQUAL 14
#define HB_OO_OP_ASSIGN       15
#define HB_OO_OP_INSTRING     16
#define HB_OO_OP_INCLUDE      17
#define HB_OO_OP_NOT          18
#define HB_OO_OP_AND          19
#define HB_OO_OP_OR           20
#define HB_OO_OP_ARRAYINDEX   21
#define HB_OO_OP_ENUMINDEX    22
#define HB_OO_OP_ENUMBASE     23
#define HB_OO_OP_ENUMVALUE    24
#define HB_OO_OP_ENUMSTART    25
#define HB_OO_OP_ENUMSKIP     26
#define HB_OO_OP_ENUMSTOP     27
#define HB_OO_OP_ENUMISFIRST  28
#define HB_OO_OP_ENUMISLAST   29
#endif

#if defined(__cplusplus)
constexpr HB_USHORT HB_OO_MAX_OPERATOR = 29;
#else
#define HB_OO_MAX_OPERATOR    29
#endif

// initialize Classy/OO system at HVM startup
extern void hb_clsInit(void);
// initialize Classy/OO system .prg functions
extern void hb_clsDoInit(void);
// releases all defined classes
extern void hb_clsReleaseAll(void);
// classes.c - mark all class internals as used
extern void hb_clsIsClassRef(void);
extern HB_BOOL hb_clsHasDestructor(HB_USHORT uiClass);
// returns the real method symbol for given stack symbol
extern PHB_SYMB hb_clsMethodSym(PHB_ITEM pBaseSymbol);

// returns the method pointer of an object class
extern PHB_SYMB hb_objGetMethod(PHB_ITEM pObject, PHB_SYMB pSymMsg, PHB_STACK_STATE pStack);
// create object variable reference
extern HB_BOOL hb_objGetVarRef(PHB_ITEM pObject, PHB_SYMB pMessage, PHB_STACK_STATE pStack);
extern HB_BOOL hb_objHasOperator(PHB_ITEM pObject, HB_USHORT uiOperator);
extern HB_BOOL hb_objOperatorCall(HB_USHORT uiOperator, PHB_ITEM pResult, PHB_ITEM pObject, PHB_ITEM pMsgArg1, PHB_ITEM pMsgArg2);
extern void hb_objDestructorCall(PHB_ITEM pObject);
extern PHB_ITEM hb_objCloneTo(PHB_ITEM pDest, PHB_ITEM pObject);
extern void hb_objCloneBody(PHB_ITEM pDest, PHB_ITEM pObject, PHB_NESTED_CLONED pClonedList);

#ifndef HB_NO_PROFILER
// profiler for object management
extern void hb_mthAddTime(HB_ULONG ulClockTicks); // profiler from classes.c
#endif

#endif // _HB_API_INTERNAL_

// class management
extern HB_EXPORT const char *hb_clsName(HB_USHORT uiClass);
extern HB_EXPORT const char *hb_clsFuncName(HB_USHORT uiClass);
extern HB_EXPORT const char *hb_clsMethodName(HB_USHORT uiClass, HB_USHORT uiMethod);
extern HB_EXPORT PHB_SYMB hb_clsFuncSym(HB_USHORT uiClass);
// is a class handle inherited from szParentName Class ?
extern HB_EXPORT HB_BOOL hb_clsIsParent(HB_USHORT uiClass, const char *szParentName);
extern HB_EXPORT HB_SIZE hb_clsGetVarIndex(HB_USHORT uiClass, PHB_DYNS pVarSym);
extern HB_EXPORT HB_USHORT hb_clsFindClass(const char *szClass, const char *szClassFunc);

// object management
// get object class handle
extern HB_EXPORT HB_USHORT hb_objGetClass(PHB_ITEM pItem);
// set object class handle using class name and class function name
extern HB_EXPORT HB_USHORT hb_objSetClass(PHB_ITEM pItem, const char *szClass, const char *szFunc);
// retrieves an object class name
extern HB_EXPORT const char *hb_objGetClsName(PHB_ITEM pObject);
// retrieves an object class name for a specific message
extern HB_EXPORT const char *hb_objGetRealClsName(PHB_ITEM pObject, const char *szString);

// returns HB_TRUE/HB_FALSE whether szString is an existing message for object
extern HB_EXPORT HB_BOOL hb_objHasMsg(PHB_ITEM pObject, const char *szString);
extern HB_EXPORT HB_BOOL hb_objHasMessage(PHB_ITEM pObject, PHB_DYNS pMessage);
extern HB_EXPORT PHB_ITEM hb_objSendMsg(PHB_ITEM pObj, const char *sMsg, HB_ULONG ulArg, ...);
extern HB_EXPORT PHB_ITEM hb_objSendMessage(PHB_ITEM pObj, PHB_DYNS pMessage, HB_ULONG ulArg, ...);

// DATA Put/Get (experimental/work in progress)
extern HB_EXPORT PHB_ITEM hb_objDataPutPtr(PHB_ITEM pObj, const char *sMsg, void *value);
extern HB_EXPORT void *hb_objDataGetPtr(PHB_ITEM pObj, const char *sMsg);
extern HB_EXPORT PHB_ITEM hb_objDataPutL(PHB_ITEM pObj, const char *sMsg, HB_BOOL value);
extern HB_EXPORT HB_BOOL hb_objDataGetL(PHB_ITEM pObj, const char *sMsg);
extern HB_EXPORT PHB_ITEM hb_objDataPutNI(PHB_ITEM pObj, const char *sMsg, int value);
extern HB_EXPORT int hb_objDataGetNI(PHB_ITEM pObj, const char *sMsg);
extern HB_EXPORT PHB_ITEM hb_objDataPutNL(PHB_ITEM pObj, const char *sMsg, long value);
extern HB_EXPORT long hb_objDataGetNL(PHB_ITEM pObj, const char *sMsg);
//

extern HB_EXPORT PHB_ITEM hb_objGetVarPtr(PHB_ITEM pObject, PHB_DYNS pVarMsg);

// send message which allows to set execution context for debugger
extern HB_EXPORT void hb_dbg_objSendMessage(int iProcLevel, PHB_ITEM pObject, PHB_ITEM pMessage, int iParamOffset);

// Harbour equivalent for Clipper internal __mdCreate()
extern HB_EXPORT HB_USHORT hb_clsCreate(HB_USHORT usSize, const char *szClassName);
// Harbour equivalent for Clipper internal __mdAdd()
extern HB_EXPORT void hb_clsAdd(HB_USHORT usClassH, const char *szMethodName, PHB_FUNC pFuncPtr);
// Harbour equivalent for Clipper internal __mdAssociate()
extern HB_EXPORT void hb_clsAssociate(HB_USHORT usClassH);

HB_EXTERN_END

#endif // HB_APICLS_H_
