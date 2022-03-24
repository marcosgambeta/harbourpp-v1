/*
 * Generate table with jump labels
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbcomp.h"
#include "hbassert.h"

#define HB_LABEL_FUNC(func)  HB_PCODE_FUNC(func, PHB_LABEL_INFO)
typedef HB_LABEL_FUNC(HB_LABEL_FUNC_);
typedef HB_LABEL_FUNC_ * PHB_LABEL_FUNC;

/*
 * jump functions
 */
static HB_LABEL_FUNC(hb_p_jumpnear)
{
   HB_SIZE nNewPos = nPCodePos + static_cast<signed char>(pFunc->pCode[nPCodePos + 1]);
   cargo->pnLabels[nNewPos]++;
   return 2;
}

static HB_LABEL_FUNC(hb_p_jump)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT(pAddr);
   cargo->pnLabels[nNewPos]++;
   return 3;
}

static HB_LABEL_FUNC(hb_p_jumpfar)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   cargo->pnLabels[nNewPos]++;
   return 4;
}

static HB_LABEL_FUNC(hb_p_jumpfalsenear)
{
   HB_SIZE nNewPos = nPCodePos + static_cast<signed char>(pFunc->pCode[nPCodePos + 1]);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 2;
}

static HB_LABEL_FUNC(hb_p_jumpfalse)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT(pAddr);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 3;
}

static HB_LABEL_FUNC(hb_p_jumpfalsefar)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 4;
}

static HB_LABEL_FUNC(hb_p_jumptruenear)
{
   HB_SIZE nNewPos = nPCodePos + static_cast<signed char>(pFunc->pCode[nPCodePos + 1]);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 2;
}

static HB_LABEL_FUNC(hb_p_jumptrue)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT(pAddr);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 3;
}

static HB_LABEL_FUNC(hb_p_jumptruefar)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   cargo->fCondJump = HB_TRUE;
   cargo->pnLabels[nNewPos]++;
   return 4;
}

static HB_LABEL_FUNC(hb_p_seqalways)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nAlwaysPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   if( cargo->fSetSeqBegin )
   {
      cargo->pnLabels[nAlwaysPos]++;
   }
   return 4;
}

static HB_LABEL_FUNC(hb_p_alwaysbegin)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nAlwaysEndPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   if( cargo->fSetSeqBegin )
   {
      cargo->pnLabels[nAlwaysEndPos]++;
   }
   return 4;
}

static HB_LABEL_FUNC(hb_p_seqbegin)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_SIZE nRecoverPos = nPCodePos + HB_PCODE_MKINT24(pAddr);
   if( cargo->fSetSeqBegin )
   {
      cargo->pnLabels[nRecoverPos]++;
   }
   return 4;
}

static HB_LABEL_FUNC(hb_p_seqend)
{
   HB_BYTE * pAddr = &pFunc->pCode[nPCodePos + 1];
   HB_ISIZ nOffset = HB_PCODE_MKINT24(pAddr);
   HB_SIZE nNewPos = nPCodePos + nOffset;
   if( cargo->fSetSeqBegin || nOffset != 4 )
   {
      cargo->pnLabels[nNewPos]++;
   }
   return 4;
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PHB_LABEL_FUNC s_GenLabelFuncTable[] =
{
   nullptr,                       /* HB_P_AND                   */
   nullptr,                       /* HB_P_ARRAYPUSH             */
   nullptr,                       /* HB_P_ARRAYPOP              */
   nullptr,                       /* HB_P_ARRAYDIM              */
   nullptr,                       /* HB_P_ARRAYGEN              */
   nullptr,                       /* HB_P_EQUAL                 */
   nullptr,                       /* HB_P_ENDBLOCK              */
   nullptr,                       /* HB_P_ENDPROC               */
   nullptr,                       /* HB_P_EXACTLYEQUAL          */
   nullptr,                       /* HB_P_FALSE                 */
   nullptr,                       /* HB_P_FORTEST               */
   nullptr,                       /* HB_P_FUNCTION              */
   nullptr,                       /* HB_P_FUNCTIONSHORT         */
   nullptr,                       /* HB_P_FRAME                 */
   nullptr,                       /* HB_P_FUNCPTR               */
   nullptr,                       /* HB_P_GREATER               */
   nullptr,                       /* HB_P_GREATEREQUAL          */
   nullptr,                       /* HB_P_DEC                   */
   nullptr,                       /* HB_P_DIVIDE                */
   nullptr,                       /* HB_P_DO                    */
   nullptr,                       /* HB_P_DOSHORT               */
   nullptr,                       /* HB_P_DUPLICATE             */
   nullptr,                       /* HB_P_PUSHTIMESTAMP         */
   nullptr,                       /* HB_P_INC                   */
   nullptr,                       /* HB_P_INSTRING              */
   hb_p_jumpnear,              /* HB_P_JUMPNEAR              */
   hb_p_jump,                  /* HB_P_JUMP                  */
   hb_p_jumpfar,               /* HB_P_JUMPFAR               */
   hb_p_jumpfalsenear,         /* HB_P_JUMPFALSENEAR         */
   hb_p_jumpfalse,             /* HB_P_JUMPFALSE             */
   hb_p_jumpfalsefar,          /* HB_P_JUMPFALSEFAR          */
   hb_p_jumptruenear,          /* HB_P_JUMPTRUENEAR          */
   hb_p_jumptrue,              /* HB_P_JUMPTRUE              */
   hb_p_jumptruefar,           /* HB_P_JUMPTRUEFAR           */
   nullptr,                       /* HB_P_LESSEQUAL             */
   nullptr,                       /* HB_P_LESS                  */
   nullptr,                       /* HB_P_LINE                  */
   nullptr,                       /* HB_P_LOCALNAME             */
   nullptr,                       /* HB_P_MACROPOP              */
   nullptr,                       /* HB_P_MACROPOPALIASED       */
   nullptr,                       /* HB_P_MACROPUSH             */
   nullptr,                       /* HB_P_MACROARRAYGEN         */
   nullptr,                       /* HB_P_MACROPUSHLIST         */
   nullptr,                       /* HB_P_MACROPUSHINDEX        */
   nullptr,                       /* HB_P_MACROPUSHPARE         */
   nullptr,                       /* HB_P_MACROPUSHALIASED      */
   nullptr,                       /* HB_P_MACROSYMBOL           */
   nullptr,                       /* HB_P_MACROTEXT             */
   nullptr,                       /* HB_P_MESSAGE               */
   nullptr,                       /* HB_P_MINUS                 */
   nullptr,                       /* HB_P_MODULUS               */
   nullptr,                       /* HB_P_MODULENAME            */
                               /* start: pcodes generated by macro compiler */
   nullptr,                       /* HB_P_MMESSAGE              */
   nullptr,                       /* HB_P_MPOPALIASEDFIELD      */
   nullptr,                       /* HB_P_MPOPALIASEDVAR        */
   nullptr,                       /* HB_P_MPOPFIELD             */
   nullptr,                       /* HB_P_MPOPMEMVAR            */
   nullptr,                       /* HB_P_MPUSHALIASEDFIELD     */
   nullptr,                       /* HB_P_MPUSHALIASEDVAR       */
   nullptr,                       /* HB_P_MPUSHBLOCK            */
   nullptr,                       /* HB_P_MPUSHFIELD            */
   nullptr,                       /* HB_P_MPUSHMEMVAR           */
   nullptr,                       /* HB_P_MPUSHMEMVARREF        */
   nullptr,                       /* HB_P_MPUSHSYM              */
   nullptr,                       /* HB_P_MPUSHVARIABLE         */
                               /* end: */
   nullptr,                       /* HB_P_MULT                  */
   nullptr,                       /* HB_P_NEGATE                */
   nullptr,                       /* HB_P_NOOP                  */
   nullptr,                       /* HB_P_NOT                   */
   nullptr,                       /* HB_P_NOTEQUAL              */
   nullptr,                       /* HB_P_OR                    */
   nullptr,                       /* HB_P_PARAMETER             */
   nullptr,                       /* HB_P_PLUS                  */
   nullptr,                       /* HB_P_POP                   */
   nullptr,                       /* HB_P_POPALIAS              */
   nullptr,                       /* HB_P_POPALIASEDFIELD       */
   nullptr,                       /* HB_P_POPALIASEDFIELDNEAR   */
   nullptr,                       /* HB_P_POPALIASEDVAR         */
   nullptr,                       /* HB_P_POPFIELD              */
   nullptr,                       /* HB_P_POPLOCAL              */
   nullptr,                       /* HB_P_POPLOCALNEAR          */
   nullptr,                       /* HB_P_POPMEMVAR             */
   nullptr,                       /* HB_P_POPSTATIC             */
   nullptr,                       /* HB_P_POPVARIABLE           */
   nullptr,                       /* HB_P_POWER                 */
   nullptr,                       /* HB_P_PUSHALIAS             */
   nullptr,                       /* HB_P_PUSHALIASEDFIELD      */
   nullptr,                       /* HB_P_PUSHALIASEDFIELDNEAR  */
   nullptr,                       /* HB_P_PUSHALIASEDVAR        */
   nullptr,                       /* HB_P_PUSHBLOCK             */
   nullptr,                       /* HB_P_PUSHBLOCKSHORT        */
   nullptr,                       /* HB_P_PUSHFIELD             */
   nullptr,                       /* HB_P_PUSHBYTE              */
   nullptr,                       /* HB_P_PUSHINT               */
   nullptr,                       /* HB_P_PUSHLOCAL             */
   nullptr,                       /* HB_P_PUSHLOCALNEAR         */
   nullptr,                       /* HB_P_PUSHLOCALREF          */
   nullptr,                       /* HB_P_PUSHLONG              */
   nullptr,                       /* HB_P_PUSHMEMVAR            */
   nullptr,                       /* HB_P_PUSHMEMVARREF         */
   nullptr,                       /* HB_P_PUSHNIL               */
   nullptr,                       /* HB_P_PUSHDOUBLE            */
   nullptr,                       /* HB_P_PUSHSELF              */
   nullptr,                       /* HB_P_PUSHSTATIC            */
   nullptr,                       /* HB_P_PUSHSTATICREF         */
   nullptr,                       /* HB_P_PUSHSTR               */
   nullptr,                       /* HB_P_PUSHSTRSHORT          */
   nullptr,                       /* HB_P_PUSHSYM               */
   nullptr,                       /* HB_P_PUSHSYMNEAR           */
   nullptr,                       /* HB_P_PUSHVARIABLE          */
   nullptr,                       /* HB_P_RETVALUE              */
   nullptr,                       /* HB_P_SEND                  */
   nullptr,                       /* HB_P_SENDSHORT             */
   hb_p_seqbegin,              /* HB_P_SEQBEGIN              */
   hb_p_seqend,                /* HB_P_SEQEND                */
   nullptr,                       /* HB_P_SEQRECOVER            */
   nullptr,                       /* HB_P_SFRAME                */
   nullptr,                       /* HB_P_STATICS               */
   nullptr,                       /* HB_P_STATICNAME            */
   nullptr,                       /* HB_P_SWAPALIAS             */
   nullptr,                       /* HB_P_TRUE                  */
   nullptr,                       /* HB_P_ZERO                  */
   nullptr,                       /* HB_P_ONE                   */
   nullptr,                       /* HB_P_MACROFUNC             */
   nullptr,                       /* HB_P_MACRODO               */
   nullptr,                       /* HB_P_MPUSHSTR              */
   nullptr,                       /* HB_P_LOCALNEARADDINT       */
   nullptr,                       /* HB_P_MACROPUSHREF          */
   nullptr,                       /* HB_P_PUSHLONGLONG          */
   nullptr,                       /* HB_P_ENUMSTART             */
   nullptr,                       /* HB_P_ENUMNEXT              */
   nullptr,                       /* HB_P_ENUMPREV              */
   nullptr,                       /* HB_P_ENUMEND               */
   nullptr,                       /* HB_P_SWITCH                */
   nullptr,                       /* HB_P_PUSHDATE              */
                               /* optimization of inlined math operations */
   nullptr,                       /* HB_P_PLUSEQPOP             */
   nullptr,                       /* HB_P_MINUSEQPOP            */
   nullptr,                       /* HB_P_MULTEQPOP             */
   nullptr,                       /* HB_P_DIVEQPOP              */
   nullptr,                       /* HB_P_PLUSEQ                */
   nullptr,                       /* HB_P_MINUSEQ               */
   nullptr,                       /* HB_P_MULTEQ                */
   nullptr,                       /* HB_P_DIVEQ                 */
   nullptr,                       /* HB_P_WITHOBJECTSTART       */
   nullptr,                       /* HB_P_WITHOBJECTMESSAGE     */
   nullptr,                       /* HB_P_WITHOBJECTEND         */
   nullptr,                       /* HB_P_MACROSEND             */
   nullptr,                       /* HB_P_PUSHOVARREF           */
   nullptr,                       /* HB_P_ARRAYPUSHREF          */
   nullptr,                       /* HB_P_VFRAME                */
   nullptr,                       /* HB_P_LARGEFRAME            */
   nullptr,                       /* HB_P_LARGEVFRAME           */
   nullptr,                       /* HB_P_PUSHSTRHIDDEN         */
   nullptr,                       /* HB_P_LOCALADDINT           */
   nullptr,                       /* HB_P_MODEQPOP              */
   nullptr,                       /* HB_P_EXPEQPOP              */
   nullptr,                       /* HB_P_MODEQ                 */
   nullptr,                       /* HB_P_EXPEQ                 */
   nullptr,                       /* HB_P_DUPLUNREF             */
   nullptr,                       /* HB_P_MPUSHBLOCKLARGE       */
   nullptr,                       /* HB_P_MPUSHSTRLARGE         */
   nullptr,                       /* HB_P_PUSHBLOCKLARGE        */
   nullptr,                       /* HB_P_PUSHSTRLARGE          */
   nullptr,                       /* HB_P_SWAP                  */
   nullptr,                       /* HB_P_PUSHVPARAMS           */
   nullptr,                       /* HB_P_PUSHUNREF             */
   hb_p_seqalways,             /* HB_P_SEQALWAYS             */
   hb_p_alwaysbegin,           /* HB_P_ALWAYSBEGIN           */
   nullptr,                       /* HB_P_ALWAYSEND             */
   nullptr,                       /* HB_P_DECEQPOP              */
   nullptr,                       /* HB_P_INCEQPOP              */
   nullptr,                       /* HB_P_DECEQ                 */
   nullptr,                       /* HB_P_INCEQ                 */
   nullptr,                       /* HB_P_LOCALDEC              */
   nullptr,                       /* HB_P_LOCALINC              */
   nullptr,                       /* HB_P_LOCALINCPUSH          */
   nullptr,                       /* HB_P_PUSHFUNCSYM           */
   nullptr,                       /* HB_P_HASHGEN               */
   nullptr,                       /* HB_P_SEQBLOCK              */
   nullptr,                       /* HB_P_THREADSTATICS         */
   nullptr                        /* HB_P_PUSHAPARAMS           */
};

void hb_compGenLabelTable(PHB_HFUNC pFunc, PHB_LABEL_INFO label_info)
{
   const PHB_LABEL_FUNC * pFuncTable = s_GenLabelFuncTable;
   HB_SIZE nLabel = 0;

   assert(HB_P_LAST_PCODE == sizeof(s_GenLabelFuncTable) / sizeof(PHB_LABEL_FUNC));

   hb_compPCodeEval(pFunc, reinterpret_cast<const PHB_PCODE_FUNC*>(pFuncTable), static_cast<void*>(label_info));

   for( HB_SIZE n = 0; n < pFunc->nPCodePos; ++n )
   {
      if( label_info->pnLabels[n] )
      {
         label_info->pnLabels[n] = ++nLabel;
      }
   }
}
