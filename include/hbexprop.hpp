// Header file for the Harbour Compiler
// Copyright 1999 Ryszard Glab

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

#ifndef HB_EXPROP_H_
#define HB_EXPROP_H_

#include "hbapi.hpp"

HB_EXTERN_BEGIN

/* Definitions of function templates used in expression's message
 * handling
 */
#define  HB_EXPR_FUNC( proc )  HB_EXPR * proc( HB_EXPR *pSelf, HB_EXPR_MESSAGE iMessage, HB_COMP_DECL )
typedef  HB_EXPR_FUNC( ( * PHB_EXPR_FUNC ) );

#if defined(HB_MACRO_SUPPORT)
#define hb_comp_ExprTable     hb_macro_ExprTable
#endif

#if !defined(HB_COMMON_SUPPORT)
extern const PHB_EXPR_FUNC hb_comp_ExprTable[ HB_EXPR_COUNT ];
#define  HB_EXPR_USE( pSelf, iMessage )  \
         hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), HB_COMP_PARAM )
#endif

extern HB_EXPORT_INT HB_EXPR *hb_compExprNewEmpty( HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewNil( HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewDouble( double, uint8_t, uint8_t, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewLong( HB_MAXINT nValue, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewDate( long lDate, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewTimeStamp( long lDate, long lTime, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewString( const char * szValue, HB_SIZE nLen, HB_BOOL fDealloc, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewLogical( int32_t iValue, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewSelf( HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewCodeBlock( char * string, HB_SIZE nLen, int32_t iFlags, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewVar( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewAliasVar( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewAliasExpr( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMacro( HB_EXPR *, unsigned char cMacroOp, const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewFunName( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewRTVar( const char * szName, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewAlias( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewEQ( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewNE( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewLT( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewLE( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewGT( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewGE( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewIN( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPlus( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMinus( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMult( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewDiv( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMod( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPower( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewAssign( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewEqual( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPlusEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMinusEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMultEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewDivEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewModEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewExpEq( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPostInc( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPostDec( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPreInc( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewPreDec( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewAnd( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewOr( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewNot( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewNegate( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewVarRef( const char * szVarName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewFunRef( const char * szFunName, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewFunCall( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewRef( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewCodeblockExpr( HB_EXPR *, HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewSend( const char *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMacroSend( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMethodObject( HB_EXPR *, HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewMethodCall( HB_EXPR *, HB_EXPR *);
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewList( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewArgList( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewArgRef( HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewArray( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewHash( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewArrayAt( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprAddListExpr( HB_EXPR *, HB_EXPR *);
extern HB_EXPORT_INT HB_EXPR *hb_compExprCBVarAdd( HB_EXPR *, const char * szVarName, uint8_t bType, HB_COMP_DECL );
extern HB_EXPORT_INT void hb_compExprCBVarDel( PHB_CBVAR );
extern HB_EXPORT_INT HB_EXPR *hb_compExprAddCodeblockExpr( HB_EXPR *, HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprSetCodeblockBody( HB_EXPR *pExpr, uint8_t * pCode, HB_SIZE nLen );
extern HB_EXPORT_INT HB_EXPR *hb_compExprNewIIF( HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprMacroAsAlias( HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprAssign( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprEqual( HB_EXPR *, HB_EXPR * );
extern HB_EXPORT_INT HB_EXPR *hb_compExprAssignStatic( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprListTypeCheck( HB_EXPR *pExpr, HB_EXPRTYPE ExprType );
extern HB_EXPORT_INT HB_ULONG hb_compExprListLen( HB_EXPR * );
extern HB_EXPORT_INT HB_ULONG hb_compExprParamListLen( HB_EXPR * );
extern HB_EXPORT_INT HB_SIZE hb_compExprParamListCheck( HB_COMP_DECL, HB_EXPR * );

extern HB_EXPORT_INT const char * hb_compExprDescription( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprType( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprIsInteger( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprIsLong( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprAsInteger( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprAsNumSign( HB_EXPR * );
extern HB_EXPORT_INT int32_t hb_compExprIsString( HB_EXPR * );
extern HB_EXPORT_INT HB_SIZE hb_compExprAsStringLen( HB_EXPR * );
extern HB_EXPORT_INT HB_MAXINT hb_compExprAsLongNum( HB_EXPR * );
extern HB_EXPORT_INT const char * hb_compExprAsString( HB_EXPR * );
extern HB_EXPORT_INT const char * hb_compExprAsSymbol( HB_EXPR * );
extern HB_EXPORT_INT HB_BOOL hb_compExprIsArrayToParams( HB_EXPR * );

extern HB_EXPORT_INT HB_EXPR *hb_compExprListStrip( HB_EXPR *, HB_COMP_DECL );

extern HB_EXPORT_INT HB_EXPR *hb_compExprSetOperand( HB_EXPR *, HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprSetGetBlock( HB_EXPR *pExpr, HB_COMP_DECL );

extern HB_EXPORT_INT void hb_compExprDelOperator( HB_EXPR *, HB_COMP_DECL );

extern HB_EXPORT_INT HB_EXPR *hb_compExprReducePower( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceMod( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceDiv( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceMult( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceMinus( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReducePlus( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceNegate( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceIN( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceNE( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceGE( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceLE( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceGT( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceLT( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceEQ( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceAnd( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceOr( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceNot( HB_EXPR *pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT HB_EXPR *hb_compExprReduceIIF( HB_EXPR *, HB_COMP_DECL );

extern HB_EXPORT_INT HB_BOOL hb_compExprReduceAT( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceCHR( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBCHAR( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceLEN( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceASC( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBCODE( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceINT( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceEMPTY( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceSTOT( HB_EXPR *, uint16_t usCount, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceSTOD( HB_EXPR *, uint16_t usCount, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceDTOS( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceCTOD( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceUPPER( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceMIN( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceMAX( HB_EXPR *, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBitFunc( HB_EXPR *, HB_MAXINT nResult, HB_BOOL fBool, HB_COMP_DECL );

HB_EXTERN_END

#endif  /* HB_EXPROP_H_ */
