//
// Clipper compatible preprocessor
//
// Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* #define HB_CLP_STRICT */
/* #define HB_PP_STRICT_LINEINFO_TOKEN */

#define _HB_PP_INTERNAL

#include "hbpp.hpp"
#include "hbdate.hpp"

// clang-format off

constexpr int HB_PP_WARN_EXPLICIT = 1; // #define HB_PP_WARN_EXPLICIT                     1     /* C10?? */
constexpr int HB_PP_WARN_DEFINE_REDEF = 2; // #define HB_PP_WARN_DEFINE_REDEF                 2     /* C1005 */

constexpr int HB_PP_ERR_ILLEGAL_CHAR = 1; // #define HB_PP_ERR_ILLEGAL_CHAR                  1     /* C2004 */
constexpr int HB_PP_ERR_STRING_TERMINATOR = 2; // #define HB_PP_ERR_STRING_TERMINATOR             2     /* C2007 */
constexpr int HB_PP_ERR_MISSING_ENDTEXT = 3; // #define HB_PP_ERR_MISSING_ENDTEXT               3     /* C2033 */
constexpr int HB_PP_ERR_DEFINE_SYNTAX = 4; // #define HB_PP_ERR_DEFINE_SYNTAX                 4     /* C2055 */
constexpr int HB_PP_ERR_LABEL_MISSING_IN_DEFINE = 5; // #define HB_PP_ERR_LABEL_MISSING_IN_DEFINE       5     /* C2057 */
constexpr int HB_PP_ERR_PARE_MISSING_IN_DEFINE = 6; // #define HB_PP_ERR_PARE_MISSING_IN_DEFINE        6     /* C2058 */
constexpr int HB_PP_ERR_MISSING_PATTERN_SEP = 7; // #define HB_PP_ERR_MISSING_PATTERN_SEP           7     /* C2059 */
constexpr int HB_PP_ERR_UNKNOWN_RESULT_MARKER = 8; // #define HB_PP_ERR_UNKNOWN_RESULT_MARKER         8     /* C2060 */
constexpr int HB_PP_ERR_WRONG_LABEL = 9; // #define HB_PP_ERR_WRONG_LABEL                   9     /* C2061 */
constexpr int HB_PP_ERR_BAD_MATCH_MARKER = 10; // #define HB_PP_ERR_BAD_MATCH_MARKER              10    /* C2062 */
constexpr int HB_PP_ERR_EMPTY_OPTIONAL = 11; // #define HB_PP_ERR_EMPTY_OPTIONAL                11    /* C2065 */
constexpr int HB_PP_ERR_UNCLOSED_OPTIONAL = 12; // #define HB_PP_ERR_UNCLOSED_OPTIONAL             12    /* C2066 */
constexpr int HB_PP_ERR_DIRECTIVE_IFDEF = 13; // #define HB_PP_ERR_DIRECTIVE_IFDEF               13    /* C2068 */
constexpr int HB_PP_ERR_DIRECTIVE_ENDIF = 14; // #define HB_PP_ERR_DIRECTIVE_ENDIF               14    /* C2069 */
constexpr int HB_PP_ERR_DIRECTIVE_ELSE = 15; // #define HB_PP_ERR_DIRECTIVE_ELSE                15    /* C2070 */
constexpr int HB_PP_ERR_DIRECTIVE_UNDEF = 16; // #define HB_PP_ERR_DIRECTIVE_UNDEF               16    /* C2071 */
constexpr int HB_PP_ERR_AMBIGUOUS_MATCH_PATTERN = 17; // #define HB_PP_ERR_AMBIGUOUS_MATCH_PATTERN       17    /* C2072 */
constexpr int HB_PP_ERR_NESTED_OPTIONAL = 18; // #define HB_PP_ERR_NESTED_OPTIONAL               18    /* C2073 */
constexpr int HB_PP_ERR_EXPLICIT = 19; // #define HB_PP_ERR_EXPLICIT                      19    /* C2074 */
constexpr int HB_PP_ERR_CYCLIC_DEFINE = 20; // #define HB_PP_ERR_CYCLIC_DEFINE                 20    /* C2078 */
constexpr int HB_PP_ERR_CYCLIC_TRANSLATE = 21; // #define HB_PP_ERR_CYCLIC_TRANSLATE              21    /* C2079 */
constexpr int HB_PP_ERR_CYCLIC_COMMAND = 22; // #define HB_PP_ERR_CYCLIC_COMMAND                22    /* C2080 */
constexpr int HB_PP_ERR_UNTERMINATED_COMMENT = 23; // #define HB_PP_ERR_UNTERMINATED_COMMENT          23    /* C2083 */
constexpr int HB_PP_ERR_PRAGMA = 24; // #define HB_PP_ERR_PRAGMA                        24    /* C20?? */
constexpr int HB_PP_ERR_DIRECTIVE_IF = 25; // #define HB_PP_ERR_DIRECTIVE_IF                  25    /* C20?? */
constexpr int HB_PP_ERR_CANNOT_OPEN_INPUT = 26; // #define HB_PP_ERR_CANNOT_OPEN_INPUT             26    /* C30?? */
constexpr int HB_PP_ERR_FILE_TOO_LONG = 27; // #define HB_PP_ERR_FILE_TOO_LONG                 27    /* C30?? */
constexpr int HB_PP_ERR_CANNOT_CREATE_FILE = 28; // #define HB_PP_ERR_CANNOT_CREATE_FILE            28    /* C3006 */
constexpr int HB_PP_ERR_CANNOT_OPEN_FILE = 29; // #define HB_PP_ERR_CANNOT_OPEN_FILE              29    /* C3007 */
constexpr int HB_PP_ERR_WRONG_FILE_NAME = 30; // #define HB_PP_ERR_WRONG_FILE_NAME               30    /* C3008 */
constexpr int HB_PP_ERR_NESTED_INCLUDES = 31; // #define HB_PP_ERR_NESTED_INCLUDES               31    /* C3009 */
constexpr int HB_PP_ERR_INVALID_DIRECTIVE = 32; // #define HB_PP_ERR_INVALID_DIRECTIVE             32    /* C3010 */
constexpr int HB_PP_ERR_CANNOT_OPEN_RULES = 33; // #define HB_PP_ERR_CANNOT_OPEN_RULES             33    /* C3011 */
constexpr int HB_PP_ERR_WRITE_FILE = 34; // #define HB_PP_ERR_WRITE_FILE                    34    /* C3029 */

/* warning messages */
static const char * const s_pp_szWarnings[] =
{
   "1%s",                                                               /* C10?? */
   "1Redefinition or duplicate definition of #define %s"                /* C1005 */
};

/* error messages */
static const char * const s_pp_szErrors[] =
{
   "Illegal character '\\x%s'",                                         /* C2004 */
   "Unterminated string '%s'",                                          /* C2007 */
   "Missing ENDTEXT",                                                   /* C2033 */
   "Syntax error in #define",                                           /* C2055 */
   "Label missing in #define",                                          /* C2057 */
   "Comma or right parenthesis missing in #define",                     /* C2058 */
   "Missing => in #translate/#command",                                 /* C2059 */
   "Unknown result marker in #translate/#command",                      /* C2060 */
   "Label error in #translate/#command",                                /* C2061 */
   "Bad match marker in #translate/#command",                           /* C2062 */
   "Empty optional clause in #translate/#command",                      /* C2065 */
   "Unclosed optional clause in #translate/#command",                   /* C2066 */
   "Error in #ifdef",                                                   /* C2068 */
   "#endif does not match #ifdef",                                      /* C2069 */
   "#else does not match #ifdef",                                       /* C2070 */
   "Error in #undef",                                                   /* C2071 */
   "Ambiguous match pattern in #translate/#command",                    /* C2072 */
   "Result pattern contains nested clauses in #translate/#command",     /* C2073 */
   "#error '%s'",                                                       /* C2074 */
   "Circularity detected in #define '%s'",                              /* C2078 */
   "Circularity detected in #translate '%s'",                           /* C2079 */
   "Circularity detected in #command '%s'",                             /* C2080 */
   "Unterminated /* */ comment",                                        /* C2083 */

   "Error in #pragma",                                                  /* C20?? */
   "Error in #if expression",                                           /* C20?? */

   "Cannot open input file '%s'",                                       /* C30?? */

   "File %s is too long",                                               /* C30?? */

   "Can't create preprocessed output file",                             /* C3006 */
   "Can't open #include file '%s'",                                     /* C3007 */
   "Bad filename in #include",                                          /* C3008 */
   "Too many nested #includes",                                         /* C3009 */
   "Invalid name follows #",                                            /* C3010 */
   "Can't open standard rule file '%s'",                                /* C3011 */
   "Write error to intermediate file '%s'"                              /* C3029 */
};

static const HB_PP_OPERATOR s_operators[] =
{
   { ".NOT.", 5, "!"    , HB_PP_TOKEN_NOT       | HB_PP_TOKEN_STATIC },
   { ".AND.", 5, ".AND.", HB_PP_TOKEN_AND       | HB_PP_TOKEN_STATIC },
   { ".OR." , 4, ".OR." , HB_PP_TOKEN_OR        | HB_PP_TOKEN_STATIC },
#ifndef HB_CLP_STRICT
   { "..."  , 3, "..."  , HB_PP_TOKEN_EPSILON   | HB_PP_TOKEN_STATIC },
#endif
   { "**="  , 3, "^="   , HB_PP_TOKEN_EXPEQ     | HB_PP_TOKEN_STATIC },
   { "**"   , 2, "^"    , HB_PP_TOKEN_POWER     | HB_PP_TOKEN_STATIC },
   { "++"   , 2, "++"   , HB_PP_TOKEN_INC       | HB_PP_TOKEN_STATIC },
   { "--"   , 2, "--"   , HB_PP_TOKEN_DEC       | HB_PP_TOKEN_STATIC },
   { "=="   , 2, "=="   , HB_PP_TOKEN_EQUAL     | HB_PP_TOKEN_STATIC },
   { ":="   , 2, ":="   , HB_PP_TOKEN_ASSIGN    | HB_PP_TOKEN_STATIC },
   { "+="   , 2, "+="   , HB_PP_TOKEN_PLUSEQ    | HB_PP_TOKEN_STATIC },
   { "-="   , 2, "-="   , HB_PP_TOKEN_MINUSEQ   | HB_PP_TOKEN_STATIC },
   { "*="   , 2, "*="   , HB_PP_TOKEN_MULTEQ    | HB_PP_TOKEN_STATIC },
   { "/="   , 2, "/="   , HB_PP_TOKEN_DIVEQ     | HB_PP_TOKEN_STATIC },
   { "%="   , 2, "%="   , HB_PP_TOKEN_MODEQ     | HB_PP_TOKEN_STATIC },
   { "^="   , 2, "^="   , HB_PP_TOKEN_EXPEQ     | HB_PP_TOKEN_STATIC },
   { "<="   , 2, "<="   , HB_PP_TOKEN_LE        | HB_PP_TOKEN_STATIC },
   { ">="   , 2, ">="   , HB_PP_TOKEN_GE        | HB_PP_TOKEN_STATIC },
   { "!="   , 2, "<>"   , HB_PP_TOKEN_NE        | HB_PP_TOKEN_STATIC },
   { "<>"   , 2, "<>"   , HB_PP_TOKEN_NE        | HB_PP_TOKEN_STATIC },
   { "->"   , 2, "->"   , HB_PP_TOKEN_ALIAS     | HB_PP_TOKEN_STATIC },
   { "@"    , 1, "@"    , HB_PP_TOKEN_REFERENCE | HB_PP_TOKEN_STATIC },
   { "("    , 1, "("    , HB_PP_TOKEN_LEFT_PB   | HB_PP_TOKEN_STATIC },
   { ")"    , 1, ")"    , HB_PP_TOKEN_RIGHT_PB  | HB_PP_TOKEN_STATIC },
   { "["    , 1, "["    , HB_PP_TOKEN_LEFT_SB   | HB_PP_TOKEN_STATIC },
   { "]"    , 1, "]"    , HB_PP_TOKEN_RIGHT_SB  | HB_PP_TOKEN_STATIC },
   { "{"    , 1, "{"    , HB_PP_TOKEN_LEFT_CB   | HB_PP_TOKEN_STATIC },
   { "}"    , 1, "}"    , HB_PP_TOKEN_RIGHT_CB  | HB_PP_TOKEN_STATIC },
   { ","    , 1, ","    , HB_PP_TOKEN_COMMA     | HB_PP_TOKEN_STATIC },
   { "\\"   , 1, "\\"   , HB_PP_TOKEN_BACKSLASH | HB_PP_TOKEN_STATIC },
   { "|"    , 1, "|"    , HB_PP_TOKEN_PIPE      | HB_PP_TOKEN_STATIC },
   { "."    , 1, "."    , HB_PP_TOKEN_DOT       | HB_PP_TOKEN_STATIC },
   { "&"    , 1, "&"    , HB_PP_TOKEN_AMPERSAND | HB_PP_TOKEN_STATIC },
   { ":"    , 1, ":"    , HB_PP_TOKEN_SEND      | HB_PP_TOKEN_STATIC },
   { "!"    , 1, "!"    , HB_PP_TOKEN_NOT       | HB_PP_TOKEN_STATIC },
   { "="    , 1, "="    , HB_PP_TOKEN_EQ        | HB_PP_TOKEN_STATIC },
   { "<"    , 1, "<"    , HB_PP_TOKEN_LT        | HB_PP_TOKEN_STATIC },
   { ">"    , 1, ">"    , HB_PP_TOKEN_GT        | HB_PP_TOKEN_STATIC },
   { "#"    , 1, "#"    , HB_PP_TOKEN_HASH      | HB_PP_TOKEN_STATIC },
   { "$"    , 1, "$"    , HB_PP_TOKEN_IN        | HB_PP_TOKEN_STATIC },
   { "+"    , 1, "+"    , HB_PP_TOKEN_PLUS      | HB_PP_TOKEN_STATIC },
   { "-"    , 1, "-"    , HB_PP_TOKEN_MINUS     | HB_PP_TOKEN_STATIC },
   { "*"    , 1, "*"    , HB_PP_TOKEN_MULT      | HB_PP_TOKEN_STATIC },
   { "/"    , 1, "/"    , HB_PP_TOKEN_DIV       | HB_PP_TOKEN_STATIC },
   { "%"    , 1, "%"    , HB_PP_TOKEN_MOD       | HB_PP_TOKEN_STATIC },
   { "^"    , 1, "^"    , HB_PP_TOKEN_POWER     | HB_PP_TOKEN_STATIC }
/* unused: ? ~ " ' ` */
/* not accessible: " ' `  */
/* illegal in Clipper: ~ */
};

// clang-format on

static const char s_pp_dynamicResult = 0;

static void hb_pp_disp(PHB_PP_STATE pState, const char *szMessage)
{
  if (!pState->pDispFunc)
  {
    printf("%s", szMessage);
    fflush(stdout);
  }
  else
  {
    (pState->pDispFunc)(pState->cargo, szMessage);
  }
}

static void hb_pp_error(PHB_PP_STATE pState, char type, int iError, const char *szParam)
{
  const char *const *szMsgTable = type == 'W' ? s_pp_szWarnings : s_pp_szErrors;

  if (pState->pErrorFunc)
  {
    (pState->pErrorFunc)(pState->cargo, szMsgTable, type, iError, szParam, nullptr);
  }
  else
  {
    char line[16];
    char msg[200];
    char buffer[256];

    if (pState->pFile)
    {
      hb_snprintf(line, sizeof(line), "(%d) ", pState->pFile->iCurrentLine);
    }
    else
    {
      line[0] = '\0';
    }
    hb_snprintf(msg, sizeof(msg), szMsgTable[iError - 1], szParam);
    hb_snprintf(buffer, sizeof(buffer), "%s%s: %s\n", line,
                type == 'F'   ? "Fatal"
                : type == 'W' ? "Warning"
                              : "Error",
                msg);
    hb_pp_disp(pState, buffer);
  }
  if (type != 'W')
  {
    pState->fError = true;
    pState->iErrors++;
  }
}

static void hb_pp_operatorsFree(PHB_PP_OPERATOR pOperators, int iOperators)
{
  PHB_PP_OPERATOR pOperator = pOperators;

  while (--iOperators >= 0)
  {
    hb_xfree(HB_UNCONST(pOperator->name));
    hb_xfree(HB_UNCONST(pOperator->value));
    ++pOperator;
  }
  hb_xfree(pOperators);
}

static const HB_PP_OPERATOR *hb_pp_operatorFind(PHB_PP_STATE pState, char *buffer, HB_SIZE nLen)
{
  const HB_PP_OPERATOR *pOperator = pState->pOperators;
  int i = pState->iOperators;

  while (--i >= 0)
  {
    if (pOperator->len <= nLen && pOperator->name[0] == buffer[0] &&
        (pOperator->len == 1 || hb_strnicmp(pOperator->name + 1, buffer + 1, pOperator->len - 1) == 0))
    {
      return pOperator;
    }

    ++pOperator;
  }

  pOperator = s_operators;
  i = HB_SIZEOFARRAY(s_operators);

  do
  {
    if (pOperator->len <= nLen && pOperator->name[0] == buffer[0] &&
        (pOperator->len == 1 ||
         (pOperator->len >= 4
              ? hb_strnicmp(pOperator->name + 1, buffer + 1, pOperator->len - 1) == 0
              : (pOperator->name[1] == buffer[1] && (pOperator->len == 2 || pOperator->name[2] == buffer[2])))))
    {
      return pOperator;
    }

    ++pOperator;
  } while (--i > 0);

  return nullptr;
}

constexpr HB_SIZE HB_MEMBUF_DEFAULT_SIZE = 256; // #define HB_MEMBUF_DEFAULT_SIZE 256

static PHB_MEM_BUFFER hb_membufNew(void)
{
  auto pBuffer = static_cast<PHB_MEM_BUFFER>(hb_xgrab(sizeof(HB_MEM_BUFFER)));

  pBuffer->nLen = 0;
  pBuffer->nAllocated = HB_MEMBUF_DEFAULT_SIZE;
  pBuffer->pBufPtr = static_cast<char *>(hb_xgrab(pBuffer->nAllocated));

  return pBuffer;
}

static void hb_membufFree(PHB_MEM_BUFFER pBuffer)
{
  hb_xfree(pBuffer->pBufPtr);
  hb_xfree(pBuffer);
}

static void hb_membufFlush(PHB_MEM_BUFFER pBuffer)
{
  pBuffer->nLen = 0;
}

static void hb_membufRemove(PHB_MEM_BUFFER pBuffer, HB_SIZE nLeft)
{
  if (nLeft < pBuffer->nLen)
  {
    pBuffer->nLen = nLeft;
  }
}

static HB_SIZE hb_membufLen(const PHB_MEM_BUFFER pBuffer)
{
  return pBuffer->nLen;
}

static char *hb_membufPtr(const PHB_MEM_BUFFER pBuffer)
{
  return pBuffer->pBufPtr;
}

static void hb_membufAddCh(PHB_MEM_BUFFER pBuffer, char ch)
{
  if (pBuffer->nLen == pBuffer->nAllocated)
  {
    pBuffer->nAllocated <<= 1;
    pBuffer->pBufPtr = static_cast<char *>(hb_xrealloc(pBuffer->pBufPtr, pBuffer->nAllocated));
  }
  pBuffer->pBufPtr[pBuffer->nLen++] = ch;
}

static void hb_membufAddData(PHB_MEM_BUFFER pBuffer, const char *data, HB_SIZE nLen)
{
  if (pBuffer->nLen + nLen > pBuffer->nAllocated)
  {
    do
    {
      pBuffer->nAllocated <<= 1;
    } while (pBuffer->nLen + nLen > pBuffer->nAllocated);
    pBuffer->pBufPtr = static_cast<char *>(hb_xrealloc(pBuffer->pBufPtr, pBuffer->nAllocated));
  }

  memcpy(&pBuffer->pBufPtr[pBuffer->nLen], data, nLen);
  pBuffer->nLen += nLen;
}

static void hb_membufAddStr(PHB_MEM_BUFFER pBuffer, const char *szText)
{
  hb_membufAddData(pBuffer, szText, strlen(szText));
}

static void hb_pp_tokenFree(PHB_PP_TOKEN pToken)
{
  if (HB_PP_TOKEN_ALLOC(pToken->type))
  {
    hb_xfree(HB_UNCONST(pToken->value));
  }
  if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_MMARKER_RESTRICT ||
      HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_MMARKER_OPTIONAL ||
      HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_RMARKER_OPTIONAL)
  {
    while (pToken->pMTokens)
    {
      PHB_PP_TOKEN pMTokens = pToken->pMTokens;
      pToken->pMTokens = pMTokens->pNext;
      hb_pp_tokenFree(pMTokens);
    }
  }
  hb_xfree(pToken);
}

static void hb_pp_tokenListFree(PHB_PP_TOKEN *pTokenPtr)
{
  if (*pTokenPtr && !HB_PP_TOKEN_ISPREDEF(*pTokenPtr))
  {
    do
    {
      PHB_PP_TOKEN pToken = *pTokenPtr;
      *pTokenPtr = pToken->pNext;
      hb_pp_tokenFree(pToken);
    } while (*pTokenPtr);
  }
}

static int hb_pp_tokenListFreeCmd(PHB_PP_TOKEN *pTokenPtr)
{
  auto fStop = false;
  int iLines = 0;

  while (*pTokenPtr && !fStop)
  {
    PHB_PP_TOKEN pToken = *pTokenPtr;
    *pTokenPtr = pToken->pNext;
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EOL)
    {
      ++iLines;
    }
    fStop = HB_PP_TOKEN_ISEOC(pToken);
    hb_pp_tokenFree(pToken);
  }
  return *pTokenPtr ? iLines : 0;
}

static void hb_pp_tokenMoveCommand(PHB_PP_STATE pState, PHB_PP_TOKEN *pDestPtr, PHB_PP_TOKEN *pSrcPtr)
{
  PHB_PP_TOKEN pToken;
  int iLines = 0;

  while (*pSrcPtr)
  {
    pToken = *pSrcPtr;
    *pSrcPtr = pToken->pNext;
    *pDestPtr = pToken;
    pDestPtr = &pToken->pNext;
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EOL)
    {
      ++iLines;
    }
    if (HB_PP_TOKEN_ISEOC(pToken))
    {
      break;
    }
  }
  *pDestPtr = nullptr;

  if (iLines)
  {
    pState->pFile->iLastLine = pState->pFile->iCurrentLine + iLines;
    if (*pSrcPtr)
    {
      pState->pFile->iCurrentLine += iLines;
    }
  }
}

static PHB_PP_TOKEN hb_pp_tokenResultEnd(PHB_PP_TOKEN *pTokenPtr, bool fDirect)
{
  PHB_PP_TOKEN pNext = nullptr;

#ifdef HB_CLP_STRICT
  HB_SYMBOL_UNUSED(fDirect);
#endif

  while (*pTokenPtr)
  {
    if (HB_PP_TOKEN_ISEOP(*pTokenPtr, fDirect))
    {
      pNext = *pTokenPtr;
      *pTokenPtr = nullptr;
      break;
    }
    pTokenPtr = &(*pTokenPtr)->pNext;
  }

  return pNext;
}

static PHB_PP_TOKEN hb_pp_tokenNew(const char *value, HB_SIZE nLen, HB_SIZE nSpaces, HB_USHORT type)
{
  auto pToken = static_cast<PHB_PP_TOKEN>(hb_xgrab(sizeof(HB_PP_TOKEN)));

  if (HB_PP_TOKEN_ALLOC(type))
  {
    if (nLen <= 1)
    {
      pToken->value = hb_szAscii[nLen ? static_cast<HB_UCHAR>(value[0]) : 0];
      type |= HB_PP_TOKEN_STATIC;
    }
    else
    {
      char *val = static_cast<char *>(memcpy(hb_xgrab(nLen + 1), value, nLen));
      val[nLen] = '\0';
      pToken->value = val;
    }
  }
  else
  {
    pToken->value = value;
  }

  pToken->len = nLen;
  pToken->spaces = nSpaces;
  pToken->type = type;
  pToken->index = 0;
  pToken->pNext = nullptr;
  pToken->pMTokens = nullptr;

  return pToken;
}

static void hb_pp_tokenSetValue(PHB_PP_TOKEN pToken, const char *value, HB_SIZE nLen)
{
  if (HB_PP_TOKEN_ALLOC(pToken->type))
  {
    hb_xfree(HB_UNCONST(pToken->value));
  }
  if (nLen <= 1)
  {
    pToken->value = hb_szAscii[nLen ? static_cast<HB_UCHAR>(value[0]) : 0];
    pToken->type |= HB_PP_TOKEN_STATIC;
  }
  else
  {
    char *val = static_cast<char *>(memcpy(hb_xgrab(nLen + 1), value, nLen));
    val[nLen] = '\0';
    pToken->value = val;
    pToken->type &= ~HB_PP_TOKEN_STATIC;
  }
  pToken->len = nLen;
}

static PHB_PP_TOKEN hb_pp_tokenClone(PHB_PP_TOKEN pSource)
{
  auto pDest = static_cast<PHB_PP_TOKEN>(hb_xgrab(sizeof(HB_PP_TOKEN)));

  memcpy(pDest, pSource, sizeof(HB_PP_TOKEN));
  if (HB_PP_TOKEN_ALLOC(pDest->type))
  {
    char *val = static_cast<char *>(memcpy(hb_xgrab(pDest->len + 1), pSource->value, pDest->len));
    val[pDest->len] = '\0';
    pDest->value = val;
  }
  pDest->pNext = nullptr;

  return pDest;
}

static void hb_pp_tokenAdd(PHB_PP_TOKEN **pTokenPtr, const char *value, HB_SIZE nLen, HB_SIZE nSpaces, HB_USHORT type)
{
  PHB_PP_TOKEN pToken = hb_pp_tokenNew(value, nLen, nSpaces, type);

  **pTokenPtr = pToken;
  *pTokenPtr = &pToken->pNext;
}

static void hb_pp_tokenAddCmdSep(PHB_PP_STATE pState)
{
  hb_pp_tokenAdd(&pState->pNextTokenPtr, ";", 1, pState->nSpacesNL, HB_PP_TOKEN_EOC | HB_PP_TOKEN_STATIC);
  pState->pFile->iTokens++;
  pState->fNewStatement = true;
  pState->fCanNextLine = false;
  if (pState->iBlockState)
  {
    if (pState->iBlockState == 5)
    {
      pState->iNestedBlock++;
    }
    pState->iBlockState = 0;
  }
}

static void hb_pp_tokenAddNext(PHB_PP_STATE pState, const char *value, HB_SIZE nLen, HB_USHORT type)
{
  if (pState->fCanNextLine)
  {
    hb_pp_tokenAddCmdSep(pState);
  }

  if (!pState->fDirective)
  {
    if (pState->iNestedBlock && pState->fNewStatement && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_RIGHT_CB)
    {
      pState->iBlockState = 0;
      pState->iNestedBlock--;
    }
    else if (pState->usLastType == HB_PP_TOKEN_LEFT_CB && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_PIPE)
    {
      pState->iBlockState = 1;
    }
    else if (pState->iBlockState)
    {
      if ((pState->iBlockState == 1 || pState->iBlockState == 2 || pState->iBlockState == 4) &&
          HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_PIPE)
      {
        pState->iBlockState = 5;
      }
      else if (pState->iBlockState == 1 && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_KEYWORD)
      {
        pState->iBlockState = 2;
      }
      else if (pState->iBlockState == 1 && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_EPSILON)
      {
        pState->iBlockState = 4;
      }
      else if (pState->iBlockState == 2 && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_COMMA)
      {
        pState->iBlockState = 1;
      }
      else
      {
        pState->iBlockState = 0;
      }
    }

    if (pState->fNewStatement && nLen == 1 && *value == '#')
    {
      pState->fDirective = true;
      value = "#";
      type = HB_PP_TOKEN_DIRECTIVE | HB_PP_TOKEN_STATIC;
    }
  }

#ifndef HB_CLP_STRICT
  if (pState->nSpacesMin != 0 && pState->nSpaces == 0 && HB_PP_TOKEN_TYPE(type) == HB_PP_TOKEN_KEYWORD)
  {
    pState->nSpaces = pState->nSpacesMin;
  }
#endif
  hb_pp_tokenAdd(&pState->pNextTokenPtr, value, nLen, pState->nSpaces, type);
  pState->pFile->iTokens++;
  pState->fNewStatement = false;

  pState->nSpaces = pState->nSpacesMin = 0;
  pState->usLastType = HB_PP_TOKEN_TYPE(type);

  if (pState->iInLineState != HB_PP_INLINE_OFF)
  {
    if (pState->iInLineState == HB_PP_INLINE_START && pState->usLastType == HB_PP_TOKEN_LEFT_PB)
    {
      pState->iInLineState = HB_PP_INLINE_PARAM;
      pState->iInLineBraces = 1;
    }
    else if (pState->iInLineState == HB_PP_INLINE_PARAM)
    {
      if (pState->usLastType == HB_PP_TOKEN_LEFT_PB)
      {
        pState->iInLineBraces++;
      }
      else if (pState->usLastType == HB_PP_TOKEN_RIGHT_PB)
      {
        if (--pState->iInLineBraces == 0)
        {
          pState->iInLineState = HB_PP_INLINE_BODY;
        }
      }
    }
    else
    {
      pState->iInLineState = HB_PP_INLINE_OFF;
    }
  }
}

static void hb_pp_tokenAddStreamFunc(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, const char *value, HB_SIZE nLen)
{
  while (pToken)
  {
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_RMARKER_STRDUMP)
    {
      if (value)
      {
        hb_pp_tokenAdd(&pState->pNextTokenPtr, value, nLen, pToken->spaces, HB_PP_TOKEN_STRING);
        pState->pFile->iTokens++;
      }
    }
    else
    {
      *pState->pNextTokenPtr = hb_pp_tokenClone(pToken);
      pState->pNextTokenPtr = &(*pState->pNextTokenPtr)->pNext;
      pState->pFile->iTokens++;
    }
    pToken = pToken->pNext;
  }
  pState->fNewStatement = true;
}

static void hb_pp_readLine(PHB_PP_STATE pState)
{
  int ch, iLine = 0, iBOM = pState->pFile->iCurrentLine == 0 ? 1 : 0;

  for (;;)
  {
    if (pState->pFile->pLineBuf)
    {
      if (pState->pFile->nLineBufLen)
      {
        ch = static_cast<HB_UCHAR>(pState->pFile->pLineBuf[0]);
        pState->pFile->pLineBuf++;
        pState->pFile->nLineBufLen--;
      }
      else
      {
        break;
      }
    }
    else
    {
      ch = fgetc(pState->pFile->file_in);
      if (ch == EOF)
      {
        pState->pFile->fEof = true;
        break;
      }
    }
    iLine = 1;
    /* In Clipper ^Z works like \n */
    if (ch == '\n' || ch == '\x1a')
    {
      break;
    }
    else if (ch != '\r')
    {
      /* Clipper strips \r characters even from quoted strings */
      hb_membufAddCh(pState->pBuffer, static_cast<char>(ch));

      /* strip UTF-8 BOM signature */
      if (iBOM && ch == 0xBF && hb_membufLen(pState->pBuffer) == 3)
      {
        iBOM = 0;
        if (hb_membufPtr(pState->pBuffer)[0] == '\xEF' && hb_membufPtr(pState->pBuffer)[1] == '\xBB')
        {
          hb_membufFlush(pState->pBuffer);
        }
      }
    }
  }
  pState->iLineTot += iLine;
  iLine = ++pState->pFile->iCurrentLine / 100;
  if (!pState->fQuiet && pState->fGauge && iLine != pState->pFile->iLastDisp)
  {
    char szLine[12];

    pState->pFile->iLastDisp = iLine;
    hb_snprintf(szLine, sizeof(szLine), "\r%i00\r", iLine);
    hb_pp_disp(pState, szLine);
  }
}

static bool hb_pp_canQuote(bool fQuote, char *pBuffer, HB_SIZE nLen, HB_SIZE n, HB_SIZE *pnAt)
{
  char cQuote = 0;

  /*
   * TODO: this is Clipper compatible but it breaks valid code so we may
   *       think about changing this condition in the future.
   */
  while (n < nLen)
  {
    if (pBuffer[n] == ']')
    {
      if (cQuote && !fQuote)
      {
        HB_SIZE u = n + 1;
        cQuote = 0;
        while (u < nLen)
        {
          if (cQuote)
          {
            if (pBuffer[u] == cQuote)
            {
              cQuote = 0;
            }
          }
          else if (pBuffer[u] == '`')
          {
            cQuote = '\'';
          }
          else if (pBuffer[u] == '\'' || pBuffer[u] == '"')
          {
            cQuote = pBuffer[u];
          }
          else if (pBuffer[u] == '[')
          {
            hb_pp_canQuote(true, pBuffer, nLen, u + 1, &u);
          }
          ++u;
        }
        fQuote = cQuote == 0;
      }
      if (fQuote)
      {
        *pnAt = n;
      }
      return fQuote;
    }
    else if (!fQuote)
    {
      if (cQuote)
      {
        if (pBuffer[n] == cQuote)
        {
          cQuote = 0;
        }
      }
      else if (pBuffer[n] == '`')
      {
        cQuote = '\'';
      }
      else if (pBuffer[n] == '\'' || pBuffer[n] == '"')
      {
        cQuote = pBuffer[n];
      }
      else if (HB_PP_ISILLEGAL(pBuffer[n]))
      {
        fQuote = true;
      }
    }
    ++n;
  }
  return false;
}

static bool hb_pp_hasCommand(char *pBuffer, HB_SIZE nLen, HB_SIZE *pnAt, int iCmds, ...)
{
  HB_SIZE n = 0;
  va_list va;
  int i;

  va_start(va, iCmds);
  for (i = 0; i < iCmds && n < nLen; ++i)
  {
    HB_SIZE nl;
    char *cmd = va_arg(va, char *);
    nl = strlen(cmd);
    while (n < nLen && HB_PP_ISBLANK(pBuffer[n]))
    {
      ++n;
    }
    if (n + nl > nLen || hb_strnicmp(cmd, pBuffer + n, nl) != 0)
    {
      break;
    }
    n += nl;
    if (n < nLen && (HB_PP_ISNEXTIDCHAR(cmd[nl - 1]) || HB_PP_ISTEXTCHAR(cmd[nl - 1])) &&
        (HB_PP_ISNEXTIDCHAR(pBuffer[n]) || HB_PP_ISTEXTCHAR(pBuffer[n])))
    {
      break;
    }
  }
  va_end(va);

  if (i == iCmds)
  {
    while (n < nLen && HB_PP_ISBLANK(pBuffer[n]))
    {
      ++n;
    }

    if (n + 1 < nLen && (pBuffer[n] == '/' || pBuffer[n] == '&') && pBuffer[n] == pBuffer[n + 1])
    {
      /* strip the rest of line with // or && comment */
      n = nLen;
    }

    if (n == nLen || pBuffer[n] == ';' || (n + 1 < nLen && pBuffer[n] == '/' && pBuffer[n + 1] == '*'))
    {
      *pnAt = n;
      return true;
    }
  }
  return false;
}

static void hb_pp_dumpEnd(PHB_PP_STATE pState)
{
  pState->iStreamDump = HB_PP_STREAM_OFF;
  if (pState->iCondCompile)
  {
    hb_membufFlush(pState->pDumpBuffer);
  }
  else if (pState->pDumpFunc)
  {
    (pState->pDumpFunc)(pState->cargo, hb_membufPtr(pState->pDumpBuffer), hb_membufLen(pState->pDumpBuffer),
                        pState->iDumpLine + 1);

    /* I do not like it - dump data should be separated from
       preprocessed .prg code. What is inside DUMP area and
       how it will be interpreted depends on backend not on
       PP itself */
    if (pState->fWritePreprocesed)
    {
      int iLines = 0;
      char *pBuffer;
      HB_SIZE nLen;

      if (pState->pFile->fGenLineInfo)
      {
        fprintf(pState->file_out, "#line %d", pState->iDumpLine);
        if (pState->pFile->szFileName)
        {
          fprintf(pState->file_out, " \"%s\"", pState->pFile->szFileName);
        }
        fputc('\n', pState->file_out);
        pState->pFile->fGenLineInfo = false;
      }
      else if (pState->pFile->iLastLine < pState->iDumpLine)
      {
        do
        {
          fputc('\n', pState->file_out);
        } while (++pState->pFile->iLastLine < pState->iDumpLine);
      }
      pBuffer = hb_membufPtr(pState->pDumpBuffer);
      nLen = hb_membufLen(pState->pDumpBuffer);
      fputs("#pragma BEGINDUMP\n", pState->file_out);
      if (fwrite(pBuffer, sizeof(char), nLen, pState->file_out) != nLen)
      {
        hb_pp_error(pState, 'F', HB_PP_ERR_WRITE_FILE, pState->szOutFileName);
      }
      fputs("#pragma ENDDUMP\n", pState->file_out);

      while (nLen--)
      {
        if (*pBuffer++ == '\n')
        {
          ++iLines;
        }
      }
      pState->pFile->iLastLine = pState->iDumpLine + iLines + 2;
    }
    hb_membufFlush(pState->pDumpBuffer);
  }
}

static void hb_pp_getLine(PHB_PP_STATE pState)
{
  PHB_PP_TOKEN *pInLinePtr, *pEolTokenPtr;
  char *pBuffer;
  auto fDump = false;
  int iLines = 0, iStartLine;

  pInLinePtr = pEolTokenPtr = nullptr;
  hb_pp_tokenListFree(&pState->pFile->pTokenList);
  pState->pNextTokenPtr = &pState->pFile->pTokenList;
  pState->pFile->iTokens = 0;
  pState->nSpaces = pState->nSpacesMin = 0;
  pState->fCanNextLine = pState->fDirective = false;
  pState->fNewStatement = true;
  pState->usLastType = HB_PP_TOKEN_NUL;
  pState->iInLineState = HB_PP_INLINE_OFF;
  pState->iInLineBraces = 0;
  pState->iBlockState = pState->iNestedBlock = 0;
  iStartLine = pState->pFile->iCurrentLine + 1;

  do
  {
    HB_SIZE nLen, n;

    hb_membufFlush(pState->pBuffer);
    hb_pp_readLine(pState);
    pBuffer = hb_membufPtr(pState->pBuffer);
    nLen = hb_membufLen(pState->pBuffer);
    if (pState->fCanNextLine)
    {
      pState->nSpaces = pState->nSpacesNL;
      /*
       * set minimum number of leading spaces to 1 to avoid problems
       * with automatic word concatenation which is not Clipper compatible
       */
      pState->nSpacesMin = 1;
      pState->fCanNextLine = false;
      /* Clipper left only last leading blank character from
         concatenated lines */
      if (nLen > 1 && HB_PP_ISBLANK(pBuffer[0]))
      {
        while (nLen > 1 && HB_PP_ISBLANK(pBuffer[1]))
        {
          --nLen;
          ++pBuffer;
        }
      }
    }
    else if (pState->iStreamDump && nLen == 0)
    {
      pBuffer[0] = '\0';
      fDump = true;
    }
    n = 0;
    while (n < nLen || fDump)
    {
      char ch = pBuffer[0];
      if (pState->iStreamDump)
      {
        fDump = false;
        if (pState->iStreamDump == HB_PP_STREAM_COMMENT)
        {
          if (nLen > 0)
          {
            ++n;
            if (nLen > 1 && ch == '*' && pBuffer[1] == '/')
            {
              pState->iStreamDump = HB_PP_STREAM_OFF;
              /* Clipper clear number of leading spaces when multiline
                 comment ends */
              pState->nSpaces = 0;
              /*
               * but we cannot make the same because we have automatic
               * word concatenation which is not Clipper compatible and
               * will break code like:
               */
#if 0
                     "//   if /**/lVar; endif" /* enclosed in double-quotes to make commit checker happy */
#endif
              pState->nSpacesMin = 1;
              ++n;
            }
          }
        }
        else if (pState->iStreamDump == HB_PP_STREAM_INLINE_C)
        {
          if (nLen > 0)
          {
            ++n;
            switch (pState->iInLineState)
            {
            case HB_PP_INLINE_QUOTE1:
              if (ch == '\'')
              {
                pState->iInLineState = HB_PP_INLINE_OFF;
              }
              else if (ch == '\\' && nLen > 1)
              {
                ++n;
              }
              break;

            case HB_PP_INLINE_QUOTE2:
              if (ch == '"')
              {
                pState->iInLineState = HB_PP_INLINE_OFF;
              }
              else if (ch == '\\' && nLen > 1)
              {
                ++n;
              }
              break;

            case HB_PP_INLINE_COMMENT:
              if (nLen > 1 && ch == '*' && pBuffer[1] == '/')
              {
                pState->iInLineState = HB_PP_INLINE_OFF;
                ++n;
              }
              break;

            default:
              if (ch == '\'')
              {
                pState->iInLineState = HB_PP_INLINE_QUOTE1;
              }
              else if (ch == '"')
              {
                pState->iInLineState = HB_PP_INLINE_QUOTE2;
              }
              else if (ch == '{')
              {
                ++pState->iInLineBraces;
              }
              else if (ch == '}')
              {
                if (--pState->iInLineBraces == 0)
                {
                  pState->iStreamDump = HB_PP_STREAM_OFF;
                }
              }
              else if (nLen > 1)
              {
                if (ch == '/' && pBuffer[1] == '*')
                {
                  pState->iInLineState = HB_PP_INLINE_COMMENT;
                  ++n;
                }
                else if (ch == '/' && pBuffer[1] == '/')
                {
                  nLen = n = 0;
                }
              }
            }
          }
          if (n)
          {
            hb_membufAddData(pState->pStreamBuffer, pBuffer, n);
          }

          if (nLen == n || pState->iStreamDump == HB_PP_STREAM_OFF)
          {
            hb_membufAddCh(pState->pStreamBuffer, '\n');
            if (pState->iStreamDump == HB_PP_STREAM_OFF)
            {
              if (pState->iCondCompile)
              {
                ;
              }
              else if (pState->pInLineFunc)
              {
                char szFunc[24];
                hb_snprintf(szFunc, sizeof(szFunc), "HB_INLINE_%03d", ++pState->iInLineCount);
                if (pInLinePtr && *pInLinePtr)
                {
                  hb_pp_tokenSetValue(*pInLinePtr, szFunc, strlen(szFunc));
                }
                pState->pInLineFunc(pState->cargo, szFunc, hb_membufPtr(pState->pStreamBuffer),
                                    hb_membufLen(pState->pStreamBuffer), pState->iDumpLine);
              }
              else
              {
                hb_pp_tokenAddNext(pState, hb_membufPtr(pState->pStreamBuffer), hb_membufLen(pState->pStreamBuffer),
                                   HB_PP_TOKEN_TEXT);
              }
              hb_membufFlush(pState->pStreamBuffer);
            }
          }
        }
        else if (pState->iStreamDump == HB_PP_STREAM_DUMP_C)
        {
          if (hb_pp_hasCommand(pBuffer, nLen, &n, 3, "#", "pragma", "enddump"))
          {
            hb_pp_dumpEnd(pState);
          }
          else
          {
            n = nLen;
            hb_membufAddData(pState->pDumpBuffer, pBuffer, n);
            hb_membufAddCh(pState->pDumpBuffer, '\n');
          }
        }
        else if (hb_pp_hasCommand(pBuffer, nLen, &n, 1, "ENDTEXT") ||
                 hb_pp_hasCommand(pBuffer, nLen, &n, 3, "#", "pragma", "__endtext"))
        {
          if (pState->iStreamDump == HB_PP_STREAM_CLIPPER)
          {
            if (pState->pFuncEnd)
            {
              hb_pp_tokenAddStreamFunc(pState, pState->pFuncEnd, nullptr, 0);
            }
          }
          else
          {
            /* HB_PP_STREAM_PRG, HB_PP_STREAM_C */
            hb_pp_tokenAddStreamFunc(pState, pState->pFuncOut, hb_membufPtr(pState->pStreamBuffer),
                                     hb_membufLen(pState->pStreamBuffer));
            if (pState->pFuncEnd)
            {
              if (pState->pFuncOut)
              {
                hb_pp_tokenAddCmdSep(pState);
              }
              hb_pp_tokenAddStreamFunc(pState, pState->pFuncEnd, hb_membufPtr(pState->pStreamBuffer),
                                       hb_membufLen(pState->pStreamBuffer));
            }
            hb_membufFlush(pState->pStreamBuffer);
          }
          hb_pp_tokenListFree(&pState->pFuncOut);
          hb_pp_tokenListFree(&pState->pFuncEnd);
          pState->iStreamDump = HB_PP_STREAM_OFF;
        }
        else if (pState->iStreamDump == HB_PP_STREAM_CLIPPER)
        {
          n = nLen;
          hb_pp_tokenAddStreamFunc(pState, pState->pFuncOut, pBuffer, n);
        }
        else
        { /* HB_PP_STREAM_PRG, HB_PP_STREAM_C */
          n = nLen;
          if (pState->iStreamDump == HB_PP_STREAM_C)
          {
            hb_strRemEscSeq(pBuffer, &n);
          }
          hb_membufAddData(pState->pStreamBuffer, pBuffer, n);
          hb_membufAddCh(pState->pStreamBuffer, '\n');
          n = nLen; /* hb_strRemEscSeq() above could change n */
        }
      }
#ifndef HB_CLP_STRICT
      else if (((ch == 'e' || ch == 'E') && nLen > 1 && pBuffer[1] == '"') || (ch == '"' && pState->fEscStr))
      {
        HB_SIZE nStrip, u;

        if (ch != '"')
        {
          ++n;
        }
        while (++n < nLen && pBuffer[n] != '"')
        {
          if (pBuffer[n] == '\\')
          {
            if (++n == nLen)
            {
              break;
            }
          }
        }
        if (pState->fMultiLineStr)
        {
          while (n == nLen)
          {
            u = 1;
            while (n > u && pBuffer[n - u] == ' ')
            {
              ++u;
            }
            if (n >= u && pBuffer[n - u] == ';')
            {
              n -= u;
              nLen -= u;
              u = hb_membufLen(pState->pBuffer) - u;
              hb_membufRemove(pState->pBuffer, u);
              hb_pp_readLine(pState);
              nLen += hb_membufLen(pState->pBuffer) - u;
              pBuffer = hb_membufPtr(pState->pBuffer) + u - n;
              --n;
              while (++n < nLen && pBuffer[n] != '"')
              {
                if (pBuffer[n] == '\\')
                {
                  if (++n == nLen)
                  {
                    break;
                  }
                }
              }
            }
            else
            {
              break;
            }
          }
        }
        u = ch != '"' ? 2 : 1;
        nStrip = n - u;
        hb_strRemEscSeq(pBuffer + u, &nStrip);
        hb_pp_tokenAddNext(pState, pBuffer + u, nStrip, HB_PP_TOKEN_STRING);
        if (n == nLen)
        {
          HB_SIZE nSkip = pBuffer - hb_membufPtr(pState->pBuffer);
          hb_membufAddCh(pState->pBuffer, '\0');
          pBuffer = hb_membufPtr(pState->pBuffer) + nSkip;
          hb_pp_error(pState, 'E', HB_PP_ERR_STRING_TERMINATOR, pBuffer + u - 1);
        }
        else
        {
          ++n;
        }
      }
      else if ((ch == 't' || ch == 'T') && nLen > 1 && pBuffer[1] == '"')
      {
        ++n;
        while (++n < nLen && pBuffer[n] != '"')
        {
          ;
        }
        hb_pp_tokenAddNext(pState, pBuffer + 2, n - 2, HB_PP_TOKEN_TIMESTAMP);
        if (n == nLen)
        {
          HB_SIZE nSkip = pBuffer - hb_membufPtr(pState->pBuffer) + 1;
          hb_membufAddCh(pState->pBuffer, '\0');
          pBuffer = hb_membufPtr(pState->pBuffer) + nSkip;
          hb_pp_error(pState, 'E', HB_PP_ERR_STRING_TERMINATOR, pBuffer);
        }
        else
        {
          ++n;
        }
      }
      else if ((ch == 'd' || ch == 'D') && nLen > 1 && pBuffer[1] == '"')
      {
        ++n;
        while (++n < nLen && pBuffer[n] != '"')
        {
          ;
        }
        hb_pp_tokenAddNext(pState, pBuffer + 2, n - 2, HB_PP_TOKEN_DATE);
        if (n == nLen)
        {
          HB_SIZE nSkip = pBuffer - hb_membufPtr(pState->pBuffer) + 1;
          hb_membufAddCh(pState->pBuffer, '\0');
          pBuffer = hb_membufPtr(pState->pBuffer) + nSkip;
          hb_pp_error(pState, 'E', HB_PP_ERR_STRING_TERMINATOR, pBuffer);
        }
        else
        {
          ++n;
        }
      }
#endif
      else if (ch == '"' || ch == '\'' || ch == '`')
      {
        if (ch == '`')
        {
          ch = '\'';
        }
        while (++n < nLen && pBuffer[n] != ch)
        {
          ;
        }
        if (pState->fMultiLineStr)
        {
          while (n == nLen)
          {
            HB_SIZE u = 1;
            while (n > u && pBuffer[n - u] == ' ')
            {
              ++u;
            }
            if (n >= u && pBuffer[n - u] == ';')
            {
              n -= u;
              nLen -= u;
              u = hb_membufLen(pState->pBuffer) - u;
              hb_membufRemove(pState->pBuffer, u);
              hb_pp_readLine(pState);
              nLen += hb_membufLen(pState->pBuffer) - u;
              pBuffer = hb_membufPtr(pState->pBuffer) + u - n;
              --n;
              while (++n < nLen && pBuffer[n] != ch)
              {
                ;
              }
            }
            else
            {
              n = nLen;
              break;
            }
          }
        }
        hb_pp_tokenAddNext(pState, pBuffer + 1, n - 1, HB_PP_TOKEN_STRING);
        if (n == nLen)
        {
          HB_SIZE nSkip = pBuffer - hb_membufPtr(pState->pBuffer) + 1;
          hb_membufAddCh(pState->pBuffer, '\0');
          pBuffer = hb_membufPtr(pState->pBuffer) + nSkip;
          hb_pp_error(pState, 'E', HB_PP_ERR_STRING_TERMINATOR, pBuffer);
        }
        else
        {
          ++n;
        }
      }
      else if (ch == '[' && !pState->fDirective &&
               hb_pp_canQuote(pState->fCanNextLine || HB_PP_TOKEN_CANQUOTE(pState->usLastType), pBuffer, nLen, 1, &n))
      {
        hb_pp_tokenAddNext(pState, pBuffer + 1, n - 1, HB_PP_TOKEN_STRING);
        ++n;
      }
      else if ((ch == '/' || ch == '&') && nLen > 1 && pBuffer[1] == ch)
      {
        /* strip the rest of line with // or && comment */
        n = nLen;
      }
      else if (ch == '*' && pState->pFile->iTokens == 0)
      {
        /* strip the rest of line with * comment */
        n = nLen;
      }
      else if (ch == '/' && nLen > 1 && pBuffer[1] == '*')
      {
#ifdef HB_CLP_STRICT
        /* In Clipper multiline comments used after ';' flushes
           the EOC token what causes that ';' is always command
           separator and cannot be used as line concatenator just
           before multiline comments */
        if (pState->fCanNextLine)
        {
          hb_pp_tokenAddCmdSep(pState);
        }
#endif
        pState->iStreamDump = HB_PP_STREAM_COMMENT;
        n += 2;
      }
      else if (ch == ' ' || ch == '\t')
      {
        do
        {
          if (pBuffer[n] == ' ')
          {
            pState->nSpaces++;
          }
          else if (pBuffer[n] == '\t')
          {
            pState->nSpaces += 4;
          }
          else
          {
            break;
          }
        } while (++n < nLen);
      }
      else if (ch == ';')
      {
        if (pState->fCanNextLine)
        {
          hb_pp_tokenAddCmdSep(pState);
        }
        pState->fCanNextLine = true;
        pState->nSpacesNL = pState->nSpaces;
        pState->nSpaces = 0;
        ++n;
      }
      else if (HB_PP_ISFIRSTIDCHAR(ch))
      {
        while (++n < nLen && HB_PP_ISNEXTIDCHAR(pBuffer[n]))
        {
          ;
        }

        /*
         * In Clipper note can be used only as 1st token and after
         * statement separator ';' it does not work like a single line
         * comment.
         */
#ifdef HB_CLP_STRICT
        if (pState->pFile->iTokens == 0 &&
#else
        if (pState->fNewStatement &&
#endif
            n == 4 && hb_strnicmp("NOTE", pBuffer, 4) == 0)
        {
          /* strip the rest of line */
          n = nLen;
        }
        else
        {
          if (n < nLen && pBuffer[n] == '&')
          {
            /*
             * [<keyword>][&<keyword>[.[<nextidchars>]]]+ is a single
             * token in Clipper and this fact is important in later
             * preprocessing so we have to replicate it
             */
            while (nLen - n > 1 && pBuffer[n] == '&' && HB_PP_ISFIRSTIDCHAR(pBuffer[n + 1]))
            {
              while (++n < nLen && HB_PP_ISNEXTIDCHAR(pBuffer[n]))
              {
                ;
              }
              if (n < nLen && pBuffer[n] == '.')
              {
                while (++n < nLen && HB_PP_ISNEXTIDCHAR(pBuffer[n]))
                {
                  ;
                }
              }
            }
            if (n < nLen && pBuffer[n] == '&')
            {
              ++n;
            }
            hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_MACROTEXT);
          }
          else if (pState->pInLineFunc && pState->iInLineState == HB_PP_INLINE_OFF && n == 9 &&
                   hb_strnicmp("hb_inline", pBuffer, 9) == 0)
          {
            if (pState->fCanNextLine)
            {
              hb_pp_tokenAddCmdSep(pState);
            }
            pInLinePtr = pState->pNextTokenPtr;
            hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_KEYWORD);
            pState->iInLineState = HB_PP_INLINE_START;
            pState->iInLineBraces = 0;
          }
          else
          {
            hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_KEYWORD);
          }
        }
      }
      /* This is Clipper incompatible token - such characters are illegal
         and error message generated, to replicate this behavior is enough
         to change HB_PP_ISILLEGAL() macro */
      else if (HB_PP_ISTEXTCHAR(ch))
      {
        while (++n < nLen && HB_PP_ISTEXTCHAR(pBuffer[n]))
        {
          ;
        }

        hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_TEXT);
      }
      else if (HB_PP_ISILLEGAL(ch))
      {
        char szCh[3];

        hb_pp_tokenAddNext(pState, pBuffer, ++n, HB_PP_TOKEN_NUL);
        hb_snprintf(szCh, sizeof(szCh), "%02x", ch & 0xff);
        hb_pp_error(pState, 'E', HB_PP_ERR_ILLEGAL_CHAR, szCh);
      }
      else if (HB_PP_ISDIGIT(ch))
      {
        if (nLen >= 3 && pBuffer[0] == '0' && (pBuffer[1] == 'x' || pBuffer[1] == 'X') && HB_PP_ISHEX(pBuffer[2]))
        {
          n = 2;
          while (++n < nLen && HB_PP_ISHEX(pBuffer[n]))
          {
            ;
          }

          /* (LEX: mark token as hex?) */
          hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_NUMBER);
        }
        else if (nLen >= 3 && pBuffer[0] == '0' && (pBuffer[1] == 'd' || pBuffer[1] == 'D') &&
                 HB_PP_ISDIGIT(pBuffer[2]))
        {
          n = 2;
          while (++n < nLen && HB_PP_ISDIGIT(pBuffer[n]))
          {
            ;
          }

          hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_DATE);
        }
        else
        {
          while (++n < nLen && HB_PP_ISDIGIT(pBuffer[n]))
          {
            ;
          }
          if (nLen - n > 1 && pBuffer[n] == '.' && HB_PP_ISDIGIT(pBuffer[n + 1]))
          {
            ++n;
            while (++n < nLen && HB_PP_ISDIGIT(pBuffer[n]))
            {
              ;
            }
          }
          hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_NUMBER);
        }
      }
      else if (ch == '.' && nLen > 1 && HB_PP_ISDIGIT(pBuffer[1]))
      {
        while (++n < nLen && HB_PP_ISDIGIT(pBuffer[n]))
        {
          ;
        }

        hb_pp_tokenAddNext(pState, pBuffer, n, HB_PP_TOKEN_NUMBER);
      }
      else if (ch == '.' && nLen >= 3 && pBuffer[2] == '.' && (HB_PP_ISTRUE(pBuffer[1]) || HB_PP_ISFALSE(pBuffer[1])))
      {
        const char *value = HB_PP_ISTRUE(pBuffer[1]) ? ".T." : ".F.";

        n = 3;
        hb_pp_tokenAddNext(pState, value, n, HB_PP_TOKEN_LOGICAL | HB_PP_TOKEN_STATIC);
      }
      else if (ch == '&' && nLen > 1 && HB_PP_ISFIRSTIDCHAR(pBuffer[1]))
      {
        int iParts = 0;
        /*
         * [<keyword>][&<keyword>[.[<nextidchars>]]]+ is a single token in Clipper
         * and this fact is important in later preprocessing so we have
         * to replicate it
         */
        while (nLen - n > 1 && pBuffer[n] == '&' && HB_PP_ISFIRSTIDCHAR(pBuffer[n + 1]))
        {
          ++iParts;
          while (++n < nLen && HB_PP_ISNEXTIDCHAR(pBuffer[n]))
          {
            ;
          }
          if (n < nLen && pBuffer[n] == '.')
          {
            while (++n < nLen && HB_PP_ISNEXTIDCHAR(pBuffer[n]))
            {
              ++iParts;
            }
          }
        }
        if (n < nLen && pBuffer[n] == '&')
        {
          ++iParts;
          ++n;
        }
        hb_pp_tokenAddNext(pState, pBuffer, n, iParts == 1 ? HB_PP_TOKEN_MACROVAR : HB_PP_TOKEN_MACROTEXT);
      }
      else if (ch == '{' && !pState->fCanNextLine &&
               (pState->iInLineState == HB_PP_INLINE_BODY || pState->iInLineState == HB_PP_INLINE_START))
      {
        if (pState->iInLineState == HB_PP_INLINE_START)
        {
          hb_pp_tokenAddNext(pState, "(", 1, HB_PP_TOKEN_LEFT_PB | HB_PP_TOKEN_STATIC);
          hb_pp_tokenAddNext(pState, ")", 1, HB_PP_TOKEN_RIGHT_PB | HB_PP_TOKEN_STATIC);
        }
        pState->iInLineState = HB_PP_INLINE_OFF;
        pState->iStreamDump = HB_PP_STREAM_INLINE_C;
        pState->iDumpLine = pState->pFile->iCurrentLine - 1;
        if (pState->pStreamBuffer)
        {
          hb_membufFlush(pState->pStreamBuffer);
        }
        else
        {
          pState->pStreamBuffer = hb_membufNew();
        }
      }
      else
      {
        const HB_PP_OPERATOR *pOperator = hb_pp_operatorFind(pState, pBuffer, nLen);

        if (pOperator)
        {
          hb_pp_tokenAddNext(pState, pOperator->value, strlen(pOperator->value), pOperator->type);
          n = pOperator->len;
        }
        else
        {
          hb_pp_tokenAddNext(pState, pBuffer, ++n, HB_PP_TOKEN_OTHER);
        }
      }
      pBuffer += n;
      nLen -= n;
      n = 0;
    }

    if (pEolTokenPtr && (pEolTokenPtr != pState->pNextTokenPtr ||
                         (pState->iNestedBlock && pState->pFile->iTokens &&
                          (pState->pFile->pLineBuf ? pState->pFile->nLineBufLen == 0 : pState->pFile->fEof))))
    {
      PHB_PP_TOKEN pToken = *pEolTokenPtr;

      while (iStartLine < pState->pFile->iCurrentLine)
      {
        hb_pp_tokenAdd(&pEolTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
        pState->pFile->iTokens++;
        iStartLine++;
        iLines++;
      }
      if (pToken == nullptr)
      {
        pState->pNextTokenPtr = pEolTokenPtr;
      }
      *pEolTokenPtr = pToken;
    }

    if (!pState->fCanNextLine && !(pState->iStreamDump && pState->iStreamDump != HB_PP_STREAM_CLIPPER) &&
        (pState->iNestedBlock || pState->iBlockState == 5))
    {
      pEolTokenPtr = pState->pNextTokenPtr;
      pState->nSpaces = pState->nSpacesMin = 0;
      pState->fNewStatement = true;
      pState->fDirective = false;
      if (pState->iBlockState)
      {
        if (pState->iBlockState == 5)
        {
          pState->iNestedBlock++;
        }
        pState->iBlockState = 0;
      }
    }
  } while ((pState->pFile->pLineBuf ? pState->pFile->nLineBufLen != 0 : !pState->pFile->fEof) &&
           (pState->fCanNextLine || pState->iNestedBlock ||
            (pState->iStreamDump && pState->iStreamDump != HB_PP_STREAM_CLIPPER)));

  if (pState->iStreamDump)
  {
    if (pState->iStreamDump == HB_PP_STREAM_COMMENT)
    {
      hb_pp_error(pState, 'E', HB_PP_ERR_UNTERMINATED_COMMENT, nullptr);
    }
    else if (pState->iStreamDump == HB_PP_STREAM_DUMP_C)
    {
      hb_pp_dumpEnd(pState);
    }
    else if (pState->pFile->pLineBuf ? !pState->pFile->nLineBufLen : pState->pFile->fEof)
    {
      hb_pp_error(pState, 'E', HB_PP_ERR_MISSING_ENDTEXT, nullptr);
    }
  }

  if (pState->pFile->iTokens != 0)
  {
    hb_pp_tokenAdd(&pState->pNextTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
    pState->pFile->iTokens++;
  }
  pState->pFile->iCurrentLine -= iLines;
}

static int hb_pp_tokenStr(PHB_PP_TOKEN pToken, PHB_MEM_BUFFER pBuffer, bool fSpaces, bool fQuote, HB_USHORT ltype)
{
  int iLines = 0;
  HB_ISIZ nSpace = fSpaces ? pToken->spaces : 0;

  /* This is workaround for stringify token list and later decoding by FLEX
     which breaks Clipper compatible code */
  if (nSpace == 0 && fQuote && ltype && ltype >= HB_PP_TOKEN_ASSIGN && ltype != HB_PP_TOKEN_EQ &&
      HB_PP_TOKEN_TYPE(pToken->type) >= HB_PP_TOKEN_ASSIGN && HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_EQ)
  {
    nSpace = 1;
  }

  if (nSpace > 0)
  {
    do
    {
      hb_membufAddCh(pBuffer, ' ');
    } while (--nSpace);
  }

  if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_STRING)
  {
    int iq = 7;
    HB_SIZE n;
    char ch;

    for (n = 0; iq && n < pToken->len; ++n)
    {
      switch (pToken->value[n])
      {
      case '"':
        iq &= ~1;
        break;
      case '\'':
        iq &= ~2;
        break;
      case ']':
        iq &= ~4;
        break;
      case '\n':
      case '\r':
      case '\0':
        iq = 0;
        break;
      }
    }
    if (iq == 0 && fQuote)
    {
      /* generate string with 'e' prefix before opening '"' and quote
         control characters inside, f.e.:
            e"line1\nline2"
       */

      hb_membufAddCh(pBuffer, 'e');
      hb_membufAddCh(pBuffer, '"');
      for (n = 0; n < pToken->len; ++n)
      {
        ch = pToken->value[n];
        switch (ch)
        {
        case '\r':
          iq = ch = 'r';
          break;
        case '\n':
          iq = ch = 'n';
          break;
        case '\t':
          iq = ch = 't';
          break;
        case '\b':
          iq = ch = 'b';
          break;
        case '\f':
          iq = ch = 'f';
          break;
        case '\v':
          iq = ch = 'v';
          break;
        case '\a':
          iq = ch = 'a';
          break;
        case '\0':
          iq = ch = '0';
          break;
        case '"':
        case '\\':
          iq = 1;
          break;
        default:
          iq = 0;
          break;
        }
        if (iq)
        {
          hb_membufAddCh(pBuffer, '\\');
        }
        hb_membufAddCh(pBuffer, ch);
      }
      hb_membufAddCh(pBuffer, '"');
    }
    else
    {
      if (iq & 1)
      {
        ch = '"';
      }
      else if (iq & 2)
      {
        ch = '\'';
      }
      else
      {
        ch = '[';
      }

      hb_membufAddCh(pBuffer, ch);
      hb_membufAddData(pBuffer, pToken->value, pToken->len);
      hb_membufAddCh(pBuffer, static_cast<char>(ch == '[' ? ']' : ch));
    }
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_TIMESTAMP)
  {
    if (pToken->len >= 2 && pToken->value[0] == '0' && (pToken->value[1] == 'T' || pToken->value[1] == 't'))
    {
      hb_membufAddData(pBuffer, pToken->value, pToken->len);
    }
    else
    {
      hb_membufAddStr(pBuffer, "t\"");
      hb_membufAddData(pBuffer, pToken->value, pToken->len);
      hb_membufAddCh(pBuffer, '"');
    }
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_DATE)
  {
    if (pToken->len >= 2 && pToken->value[0] == '0' && (pToken->value[1] == 'D' || pToken->value[1] == 'd'))
    {
      hb_membufAddData(pBuffer, pToken->value, pToken->len);
    }
    else
    {
      hb_membufAddStr(pBuffer, "d\"");
      hb_membufAddData(pBuffer, pToken->value, pToken->len);
      hb_membufAddCh(pBuffer, '"');
    }
  }
  else
  {
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EOL)
    {
      ++iLines;
    }
    hb_membufAddData(pBuffer, pToken->value, pToken->len);
  }

  return iLines;
}

static bool hb_pp_tokenValueCmp(PHB_PP_TOKEN pToken, const char *szValue, HB_USHORT mode)
{
  if (pToken->len)
  {
    if (mode == HB_PP_CMP_CASE)
    {
      return memcmp(szValue, pToken->value, pToken->len) == 0;
    }
    if (mode == HB_PP_CMP_DBASE && pToken->len >= 4 &&
        (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD ||
         HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_STRING || HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_TEXT))
    {
      return hb_strnicmp(szValue, pToken->value, pToken->len) == 0;
    }
    else
    {
      return hb_stricmp(szValue, pToken->value) == 0;
    }
  }
  return false;
}

static bool hb_pp_tokenEqual(PHB_PP_TOKEN pToken, PHB_PP_TOKEN pMatch, HB_USHORT mode)
{
  return pToken == pMatch ||
         (mode != HB_PP_CMP_ADDR && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_TYPE(pMatch->type) &&
          (pToken->len == pMatch->len ||
           (mode == HB_PP_CMP_DBASE && pMatch->len > 4 && pToken->len >= 4 && pMatch->len > pToken->len)) &&
          hb_pp_tokenValueCmp(pToken, pMatch->value, mode));
}

static void hb_pp_patternClearResults(PHB_PP_RULE pRule)
{
  PHB_PP_MARKER pMarker = pRule->pMarkers;
  int i = pRule->markers;

  while (--i >= 0)
  {
    pMarker->matches = 0;
    while (pMarker->pResult)
    {
      PHB_PP_RESULT pResult = pMarker->pResult;
      pMarker->pResult = pResult->pNext;
      hb_xfree(pResult);
    }
    ++pMarker;
  }
  pRule->pNextExpr = nullptr;
}

static bool hb_pp_patternAddResult(PHB_PP_RULE pRule, HB_USHORT marker, PHB_PP_TOKEN pFirst, PHB_PP_TOKEN pNext)
{
  PHB_PP_MARKER pMarker = &pRule->pMarkers[marker - 1];

  if (pMarker->matches == 0 || pMarker->canrepeat)
  {
    PHB_PP_RESULT *pResultPtr;
    auto pResult = static_cast<PHB_PP_RESULT>(hb_xgrab(sizeof(HB_PP_RESULT)));
    pMarker->matches++;
    pResult->pFirstToken = pFirst;
    pResult->pNextExpr = pNext;
    pResult->pNext = nullptr;
    pResultPtr = &pMarker->pResult;
    while (*pResultPtr)
    {
      pResultPtr = &(*pResultPtr)->pNext;
    }
    *pResultPtr = pResult;
    return true;
  }

  return false;
}

static PHB_PP_RULE hb_pp_ruleNew(PHB_PP_TOKEN pMatch, PHB_PP_TOKEN pResult, HB_USHORT mode, HB_USHORT markers,
                                 PHB_PP_MARKER pMarkers)
{
  auto pRule = static_cast<PHB_PP_RULE>(hb_xgrab(sizeof(HB_PP_RULE)));

  pRule->pPrev = nullptr;
  pRule->mode = mode;
  pRule->pMatch = pMatch;
  pRule->pResult = pResult;
  pRule->markers = markers;
  pRule->pMarkers = pMarkers;
  pRule->pNextExpr = nullptr;

  return pRule;
}

static void hb_pp_ruleFree(PHB_PP_RULE pRule)
{
  hb_pp_tokenListFree(&pRule->pMatch);
  hb_pp_tokenListFree(&pRule->pResult);
  hb_pp_patternClearResults(pRule);
  if (pRule->pMarkers)
  {
    hb_xfree(pRule->pMarkers);
  }
  hb_xfree(pRule);
}

static void hb_pp_ruleListFree(PHB_PP_RULE *pRulePtr)
{
  PHB_PP_RULE pRule;

  while (*pRulePtr)
  {
    pRule = *pRulePtr;
    *pRulePtr = pRule->pPrev;
    hb_pp_ruleFree(pRule);
  }
}

static void hb_pp_ruleListNonStdFree(PHB_PP_RULE *pRulePtr)
{
  PHB_PP_RULE pRule;

  while (*pRulePtr)
  {
    pRule = *pRulePtr;
    if ((pRule->mode & HB_PP_STD_RULE) != 0)
    {
      pRulePtr = &pRule->pPrev;
    }
    else
    {
      *pRulePtr = pRule->pPrev;
      hb_pp_ruleFree(pRule);
    }
  }
}

static void hb_pp_ruleListSetStd(PHB_PP_RULE pRule)
{
  while (pRule)
  {
    pRule->mode |= HB_PP_STD_RULE;
    pRule = pRule->pPrev;
  }
}

static void hb_pp_ruleSetId(PHB_PP_STATE pState, PHB_PP_TOKEN pMatch, HB_BYTE id)
{
  if (HB_PP_TOKEN_ISMATCH(pMatch))
  {
    for (auto i = 0; i < HB_PP_HASHID_MAX; ++i)
    {
      pState->pMap[i] |= id;
    }
  }
  else
  {
    pState->pMap[HB_PP_HASHID(pMatch)] |= id;
  }
}

static void hb_pp_ruleListSetId(PHB_PP_STATE pState, PHB_PP_RULE pRule, HB_BYTE id)
{
  while (pRule)
  {
    hb_pp_ruleSetId(pState, pRule->pMatch, id);
    if (HB_PP_TOKEN_ISMATCH(pRule->pMatch))
    {
      break;
    }
    pRule = pRule->pPrev;
  }
}

static PHB_PP_RULE hb_pp_defineFind(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  PHB_PP_RULE pRule = pState->pDefinitions;

  /* TODO% create binary tree or hash table - the #define keyword token has
           to be unique so it's not necessary to keep the stack list,
           it will increase the speed when there is a lot of #define values */

  while (pRule && !hb_pp_tokenEqual(pToken, pRule->pMatch, HB_PP_CMP_CASE))
  {
    pRule = pRule->pPrev;
  }

  return pRule;
}

static void hb_pp_defineAdd(PHB_PP_STATE pState, HB_USHORT mode, HB_USHORT markers, PHB_PP_MARKER pMarkers,
                            PHB_PP_TOKEN pMatch, PHB_PP_TOKEN pResult)
{
  PHB_PP_RULE pRule = hb_pp_defineFind(pState, pMatch);

  if (pRule)
  {
    hb_pp_tokenListFree(&pRule->pMatch);
    hb_pp_tokenListFree(&pRule->pResult);
    hb_pp_patternClearResults(pRule);
    if (pRule->pMarkers)
    {
      hb_xfree(pRule->pMarkers);
    }
    pRule->pMatch = pMatch;
    pRule->pResult = pResult;
    pRule->pMarkers = pMarkers;
    pRule->markers = markers;
    pRule->mode = mode;
    hb_pp_error(pState, 'W', HB_PP_WARN_DEFINE_REDEF, pMatch->value);
  }
  else
  {
    pRule = hb_pp_ruleNew(pMatch, pResult, mode, markers, pMarkers);
    pRule->pPrev = pState->pDefinitions;
    pState->pDefinitions = pRule;
    pState->iDefinitions++;
  }
  hb_pp_ruleSetId(pState, pMatch, HB_PP_DEFINE);
}

static void hb_pp_defineDel(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  PHB_PP_RULE *pRulePtr = &pState->pDefinitions, pRule;

  while (*pRulePtr)
  {
    pRule = *pRulePtr;
    if (hb_pp_tokenEqual(pToken, pRule->pMatch, HB_PP_CMP_CASE))
    {
      *pRulePtr = pRule->pPrev;
      hb_pp_ruleFree(pRule);
      pState->iDefinitions--;
      return;
    }
    pRulePtr = &pRule->pPrev;
  }
}

static PHB_PP_FILE hb_pp_FileNew(PHB_PP_STATE pState, const char *szFileName, HB_BOOL fSysFile, HB_BOOL *pfNested,
                                 FILE *file_in, HB_BOOL fSearchPath, PHB_PP_OPEN_FUNC pOpenFunc, HB_BOOL fBinary)
{
  char szFileNameBuf[HB_PATH_MAX];
  const char *pLineBuf = nullptr;
  HB_SIZE nLineBufLen = 0;
  HB_BOOL fFree = false;
  PHB_PP_FILE pFile;

  if (!file_in)
  {
    int iAction = HB_PP_OPEN_FILE;

    if (pOpenFunc)
    {
      hb_strncpy(szFileNameBuf, szFileName, sizeof(szFileNameBuf) - 1);
      iAction = (pOpenFunc)(pState->cargo, szFileNameBuf, true, fSysFile, fBinary,
                            fSearchPath ? pState->pIncludePath : nullptr, pfNested, &file_in, &pLineBuf, &nLineBufLen,
                            &fFree);
      if (iAction == HB_PP_OPEN_OK)
      {
        szFileName = szFileNameBuf;
      }
    }

    if (iAction == HB_PP_OPEN_FILE)
    {
      PHB_FNAME pFileName = hb_fsFNameSplit(szFileName);
      auto fNested = false;

      pFileName->szName = szFileName;
      pFileName->szExtension = nullptr;
      if (!fSysFile)
      {
        if (pFileName->szPath)
        {
          file_in = hb_fopen(szFileName, fBinary ? "rb" : "r");
        }
        if (!file_in &&
            (!pFileName->szPath ||
             (!pFileName->szDrive && !strchr(HB_OS_PATH_DELIM_CHR_LIST, static_cast<HB_UCHAR>(pFileName->szPath[0])))))
        {
          char *szFirstFName = nullptr;
          pFile = pState->pFile;
          while (pFile)
          {
            if (pFile->szFileName)
            {
              szFirstFName = pFile->szFileName;
            }
            pFile = pFile->pPrev;
          }
          if (szFirstFName != nullptr)
          {
            PHB_FNAME pFirstFName = hb_fsFNameSplit(szFirstFName);
            pFileName->szPath = pFirstFName->szPath;
            hb_fsFNameMerge(szFileNameBuf, pFileName);
            hb_xfree(pFirstFName);
            szFileName = szFileNameBuf;
          }
          if (!pFileName->szPath || szFirstFName)
          {
            file_in = hb_fopen(szFileName, fBinary ? "rb" : "r");
          }
        }
        if (file_in)
        {
          iAction = HB_PP_OPEN_OK;
        }
        else
        {
          fNested = hb_fsMaxFilesError();
        }
      }

      if (iAction != HB_PP_OPEN_OK)
      {
        if (fNested)
        {
          if (pfNested)
          {
            *pfNested = true;
          }
        }
        else if (pState->pIncludePath && fSearchPath)
        {
          HB_PATHNAMES *pPath = pState->pIncludePath;
          do
          {
            pFileName->szPath = pPath->szPath;
            hb_fsFNameMerge(szFileNameBuf, pFileName);
            file_in = hb_fopen(szFileNameBuf, fBinary ? "rb" : "r");
            if (file_in != nullptr)
            {
              iAction = HB_PP_OPEN_OK;
              szFileName = szFileNameBuf;
              break;
            }
            pPath = pPath->pNext;
          } while (pPath);
        }

        if (iAction != HB_PP_OPEN_OK && pOpenFunc && !fNested)
        {
          hb_strncpy(szFileNameBuf, pFileName->szName, sizeof(szFileNameBuf) - 1);
          iAction = (pOpenFunc)(pState->cargo, szFileNameBuf, false, fSysFile, fBinary,
                                fSearchPath ? pState->pIncludePath : nullptr, pfNested, &file_in, &pLineBuf,
                                &nLineBufLen, &fFree);
          if (iAction == HB_PP_OPEN_OK)
          {
            szFileName = szFileNameBuf;
          }
        }
      }
      hb_xfree(pFileName);
    }

    if (iAction != HB_PP_OPEN_OK)
    {
      return nullptr;
    }

    if (pState->pIncFunc)
    {
      (pState->pIncFunc)(pState->cargo, szFileName);
    }
  }

  pFile = static_cast<PHB_PP_FILE>(hb_xgrabz(sizeof(HB_PP_FILE)));

  pFile->szFileName = hb_strdup(szFileName);
  pFile->file_in = file_in;
  pFile->fFree = fFree;
  pFile->pLineBuf = pLineBuf;
  pFile->nLineBufLen = nLineBufLen;
  pFile->iLastLine = 1;

  return pFile;
}

static PHB_PP_FILE hb_pp_FileBufNew(const char *pLineBuf, HB_SIZE nLineBufLen)
{
  auto pFile = static_cast<PHB_PP_FILE>(hb_xgrabz(sizeof(HB_PP_FILE)));

  pFile->fFree = false;
  pFile->pLineBuf = pLineBuf;
  pFile->nLineBufLen = nLineBufLen;
  pFile->iLastLine = 1;

  return pFile;
}

static void hb_pp_FileFree(PHB_PP_STATE pState, PHB_PP_FILE pFile, PHB_PP_CLOSE_FUNC pCloseFunc)
{
  if (pFile->file_in)
  {
    if (pCloseFunc)
    {
      (pCloseFunc)(pState->cargo, pFile->file_in);
    }
    else
    {
      fclose(pFile->file_in);
    }
  }

  if (pFile->szFileName)
  {
    hb_xfree(pFile->szFileName);
  }

  if (pFile->fFree && pFile->pLineBuf)
  {
    hb_xfree(HB_UNCONST(pFile->pLineBuf));
  }

  hb_pp_tokenListFree(&pFile->pTokenList);
  hb_xfree(pFile);
}

static void hb_pp_InFileFree(PHB_PP_STATE pState)
{
  while (pState->pFile)
  {
    PHB_PP_FILE pFile = pState->pFile;
    pState->pFile = pFile->pPrev;
    hb_pp_FileFree(pState, pFile, pState->pCloseFunc);
  }
  pState->iFiles = 0;
}

static void hb_pp_OutFileFree(PHB_PP_STATE pState)
{
  if (pState->file_out)
  {
    fclose(pState->file_out);
    pState->file_out = nullptr;
  }
  if (pState->szOutFileName)
  {
    hb_xfree(pState->szOutFileName);
    pState->szOutFileName = nullptr;
  }
  pState->fWritePreprocesed = false;
}

static void hb_pp_TraceFileFree(PHB_PP_STATE pState)
{
  if (pState->file_trace)
  {
    fclose(pState->file_trace);
    pState->file_trace = nullptr;
  }
  if (pState->szTraceFileName)
  {
    hb_xfree(pState->szTraceFileName);
    pState->szTraceFileName = nullptr;
  }
  pState->fWriteTrace = false;
}

static PHB_PP_STATE hb_pp_stateNew(void)
{
  auto pState = static_cast<PHB_PP_STATE>(hb_xgrabz(sizeof(HB_PP_STATE)));

  /* create new line buffer */
  pState->pBuffer = hb_membufNew();

  /* set default maximum number of translations */
  pState->iMaxCycles = HB_PP_MAX_CYCLES;

  return pState;
}

static void hb_pp_stateFree(PHB_PP_STATE pState)
{
  hb_pp_InFileFree(pState);
  hb_pp_OutFileFree(pState);
  hb_pp_TraceFileFree(pState);

  if (pState->pIncludePath)
  {
    hb_fsFreeSearchPath(pState->pIncludePath);
  }

  if (pState->iOperators > 0)
  {
    hb_pp_operatorsFree(pState->pOperators, pState->iOperators);
  }

  hb_pp_ruleListFree(&pState->pDefinitions);
  hb_pp_ruleListFree(&pState->pTranslations);
  hb_pp_ruleListFree(&pState->pCommands);

  hb_pp_tokenListFree(&pState->pTokenOut);

  hb_membufFree(pState->pBuffer);
  if (pState->pDumpBuffer)
  {
    hb_membufFree(pState->pDumpBuffer);
  }
  if (pState->pOutputBuffer)
  {
    hb_membufFree(pState->pOutputBuffer);
  }
  if (pState->pStreamBuffer)
  {
    hb_membufFree(pState->pStreamBuffer);
  }

  if (pState->pCondStack)
  {
    hb_xfree(pState->pCondStack);
  }

  hb_pp_tokenListFree(&pState->pFuncOut);
  hb_pp_tokenListFree(&pState->pFuncEnd);

  hb_xfree(pState);
}

static PHB_PP_TOKEN hb_pp_streamFuncGet(PHB_PP_TOKEN pToken, PHB_PP_TOKEN *pFuncPtr)
{
  hb_pp_tokenListFree(pFuncPtr);

  if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_PIPE && !HB_PP_TOKEN_ISEOC(pToken->pNext))
  {
    PHB_PP_TOKEN *pStartPtr, *pEndPtr, pStart, pNext;
    pStartPtr = pEndPtr = &pToken->pNext;
    while (!HB_PP_TOKEN_ISEOC(*pEndPtr) && HB_PP_TOKEN_TYPE((*pEndPtr)->type) != HB_PP_TOKEN_PIPE)
    {
      pEndPtr = &(*pEndPtr)->pNext;
    }

    pToken = *pEndPtr;
    *pEndPtr = nullptr;
    *pFuncPtr = pStart = *pStartPtr;
    *pStartPtr = pToken;
    /* replace %s with HB_PP_RMARKER_STRDUMP marker */
    while (pStart && pStart->pNext)
    {
      pNext = pStart->pNext;
      if (HB_PP_TOKEN_TYPE(pStart->type) == HB_PP_TOKEN_MOD && HB_PP_TOKEN_TYPE(pNext->type) == HB_PP_TOKEN_KEYWORD &&
          pNext->len == 1 && pNext->value[0] == 's')
      {
        HB_PP_TOKEN_SETTYPE(pStart, HB_PP_RMARKER_STRDUMP);
        pStart->pNext = pNext->pNext;
        hb_pp_tokenFree(pNext);
        pNext = pStart->pNext;
      }
      pStart = pNext;
    }
  }
  return pToken;
}

/* #pragma {__text,__stream,__cstream}|functionOut|functionEnd|functionStart */
static bool hb_pp_pragmaStream(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  auto fError = false;

  pToken = hb_pp_streamFuncGet(pToken, &pState->pFuncOut);
  pToken = hb_pp_streamFuncGet(pToken, &pState->pFuncEnd);
  if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_PIPE)
  {
    hb_pp_tokenSetValue(pToken, ";", 1);
    HB_PP_TOKEN_SETTYPE(pToken, HB_PP_TOKEN_EOC);
  }

  return fError;
}

constexpr HB_SIZE MAX_STREAM_SIZE = 0x1000000; // #define MAX_STREAM_SIZE 0x1000000

static void hb_pp_pragmaStreamFile(PHB_PP_STATE pState, const char *szFileName)
{
  PHB_PP_FILE pFile = hb_pp_FileNew(pState, szFileName, false, nullptr, nullptr, true, pState->pOpenFunc,
                                    pState->iStreamDump == HB_PP_STREAM_BINARY);

  if (pFile)
  {
    HB_SIZE nSize;

    if (pFile->file_in)
    {
      (void)fseek(pFile->file_in, 0L, SEEK_END); // TODO: C++ cast
      nSize = ftell(pFile->file_in);
      (void)fseek(pFile->file_in, 0L, SEEK_SET); // TODO: C++ cast
    }
    else
    {
      nSize = pFile->nLineBufLen;
    }

    if (nSize > MAX_STREAM_SIZE)
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_FILE_TOO_LONG, szFileName);
    }
    else if (pState->pFuncOut || pState->pFuncEnd)
    {
      PHB_PP_TOKEN pToken;
      auto fEOL = false;

      if (!pState->pStreamBuffer)
      {
        pState->pStreamBuffer = hb_membufNew();
      }

      if (nSize)
      {
        if (pFile->file_in == nullptr && pState->iStreamDump != HB_PP_STREAM_C)
        {
          hb_membufAddData(pState->pStreamBuffer, pFile->pLineBuf, nSize);
        }
        else
        {
          auto pBuffer = static_cast<char *>(hb_xgrab(nSize * sizeof(char)));

          if (pFile->file_in)
          {
            nSize = static_cast<HB_SIZE>(fread(pBuffer, sizeof(char), nSize, pFile->file_in));
          }
          else
          {
            memcpy(pBuffer, pFile->pLineBuf, nSize);
          }

          if (pState->iStreamDump == HB_PP_STREAM_C)
          {
            hb_strRemEscSeq(pBuffer, &nSize);
          }

          hb_membufAddData(pState->pStreamBuffer, pBuffer, nSize);
          hb_xfree(pBuffer);
        }
      }

      /* insert new tokens into incoming buffer
       * so they can be preprocessed
       */
      pState->pNextTokenPtr = &pState->pFile->pTokenList;
      while (!HB_PP_TOKEN_ISEOS(*pState->pNextTokenPtr))
      {
        pState->pNextTokenPtr = &(*pState->pNextTokenPtr)->pNext;
      }
      if (*pState->pNextTokenPtr == nullptr)
      {
        hb_pp_tokenAdd(&pState->pNextTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
        pState->pFile->iTokens++;
      }
      else if (HB_PP_TOKEN_TYPE((*pState->pNextTokenPtr)->type) == HB_PP_TOKEN_EOL)
      {
        hb_pp_tokenSetValue(*pState->pNextTokenPtr, ";", 1);
        HB_PP_TOKEN_SETTYPE(*pState->pNextTokenPtr, HB_PP_TOKEN_EOC);
        fEOL = true;
      }
      pState->pNextTokenPtr = &(*pState->pNextTokenPtr)->pNext;
      pToken = *pState->pNextTokenPtr;

      if (pState->pFuncOut)
      {
        hb_pp_tokenAddStreamFunc(pState, pState->pFuncOut, hb_membufPtr(pState->pStreamBuffer),
                                 hb_membufLen(pState->pStreamBuffer));
      }
      if (pState->pFuncEnd)
      {
        if (pState->pFuncOut)
        {
          hb_pp_tokenAddCmdSep(pState);
        }
        hb_pp_tokenAddStreamFunc(pState, pState->pFuncEnd, hb_membufPtr(pState->pStreamBuffer),
                                 hb_membufLen(pState->pStreamBuffer));
      }
      if (fEOL)
      {
        hb_pp_tokenAdd(&pState->pNextTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
      }
      else
      {
        hb_pp_tokenAdd(&pState->pNextTokenPtr, ";", 1, 0, HB_PP_TOKEN_EOC | HB_PP_TOKEN_STATIC);
      }
      pState->pFile->iTokens++;
      pState->fNewStatement = true;
      *pState->pNextTokenPtr = pToken;
      hb_membufFlush(pState->pStreamBuffer);
    }
    hb_pp_FileFree(pState, pFile, pState->pCloseFunc);
  }
  else
  {
    hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_OPEN_FILE, szFileName);
  }

  hb_pp_tokenListFree(&pState->pFuncOut);
  hb_pp_tokenListFree(&pState->pFuncEnd);
}

static bool hb_pp_pragmaOperatorNew(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  auto fError = true;

  if (!HB_PP_TOKEN_ISEOC(pToken) && HB_PP_TOKEN_CANJOIN(pToken->type))
  {
    HB_SIZE nLen;

    hb_membufFlush(pState->pBuffer);
    do
    {
      hb_membufAddData(pState->pBuffer, pToken->value, pToken->len);
      pToken = pToken->pNext;
    } while (!HB_PP_TOKEN_ISEOC(pToken) && pToken->spaces == 0);
    nLen = hb_membufLen(pState->pBuffer);
    if (!HB_PP_TOKEN_ISEOC(pToken))
    {
      do
      {
        hb_membufAddData(pState->pBuffer, pToken->value, pToken->len);
        pToken = pToken->pNext;
      } while (!HB_PP_TOKEN_ISEOC(pToken) && pToken->spaces == 0);
    }
    if (HB_PP_TOKEN_ISEOC(pToken) && nLen > 0)
    {
      PHB_PP_OPERATOR pOperator;
      char *pBuffer = hb_membufPtr(pState->pBuffer), *pDstBuffer;
      HB_SIZE nDstLen = hb_membufLen(pState->pBuffer) - nLen;

      if (nDstLen)
      {
        pDstBuffer = pBuffer + nLen;
      }
      else
      {
        pDstBuffer = pBuffer;
        nDstLen = nLen;
      }
      if (pState->iOperators)
      {
        pState->pOperators = static_cast<PHB_PP_OPERATOR>(
            hb_xrealloc(pState->pOperators, sizeof(HB_PP_OPERATOR) * (pState->iOperators + 1)));
      }
      else
      {
        pState->pOperators = static_cast<PHB_PP_OPERATOR>(hb_xgrab(sizeof(HB_PP_OPERATOR) * (pState->iOperators + 1)));
      }
      pOperator = &pState->pOperators[pState->iOperators++];
      pOperator->name = hb_strndup(pBuffer, nLen);
      pOperator->len = nLen;
      pOperator->value = hb_strndup(pDstBuffer, nDstLen);
      pOperator->type = HB_PP_TOKEN_OTHER;
      fError = false;
    }
  }
  return fError;
}

static bool hb_pp_setCompilerSwitch(PHB_PP_STATE pState, const char *szSwitch, int iValue)
{
  auto fError = true;

  switch (szSwitch[0])
  {
  case 'p':
  case 'P':
    if (szSwitch[1] == '\0')
    {
      pState->fWritePreprocesed = pState->file_out != nullptr && iValue != 0;
      fError = false;
    }
    else if (szSwitch[1] == '+' && szSwitch[2] == '\0')
    {
      pState->fWriteTrace = pState->file_trace != nullptr && iValue != 0;
      fError = false;
    }
    break;

  case 'q':
  case 'Q':
    if (szSwitch[1] == '\0')
    {
      pState->fQuiet = iValue != 0;
      fError = false;
    }
    break;
  }

  if (pState->pSwitchFunc)
  {
    fError = (pState->pSwitchFunc)(pState->cargo, szSwitch, &iValue, true);
  }

  return fError;
}

static bool hb_pp_getCompilerSwitch(PHB_PP_STATE pState, const char *szSwitch, int *piValue)
{
  auto fError = true;

  if (pState->pSwitchFunc)
  {
    fError = (pState->pSwitchFunc)(pState->cargo, szSwitch, piValue, false);
  }

  if (fError)
  {
    switch (szSwitch[0])
    {
    case 'p':
    case 'P':
      if (szSwitch[1] == '\0')
      {
        *piValue = pState->fWritePreprocesed ? 1 : 0;
        fError = false;
      }
      else if (szSwitch[1] == '+' && szSwitch[2] == '\0')
      {
        *piValue = pState->fWriteTrace ? 1 : 0;
        fError = false;
      }
      break;

    case 'q':
    case 'Q':
      if (szSwitch[1] == '\0')
      {
        *piValue = pState->fQuiet ? 1 : 0;
        fError = false;
      }
      break;
    }
  }

  return fError;
}

static PHB_PP_TOKEN hb_pp_pragmaGetLogical(PHB_PP_TOKEN pToken, HB_BOOL *pfValue)
{
  PHB_PP_TOKEN pValue = nullptr;

  if (pToken && pToken->pNext && HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_KEYWORD)
  {
    if ((HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EQ && HB_PP_TOKEN_ISEOC(pToken->pNext->pNext)) ||
        (pToken->pNext->pNext && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_PB &&
         HB_PP_TOKEN_TYPE(pToken->pNext->pNext->type) == HB_PP_TOKEN_RIGHT_PB &&
         HB_PP_TOKEN_ISEOC(pToken->pNext->pNext->pNext)))
    {
      pValue = pToken->pNext;
      if (hb_stricmp(pValue->value, "ON") == 0)
      {
        *pfValue = true;
      }
      else if (hb_stricmp(pValue->value, "OFF") == 0)
      {
        *pfValue = false;
      }
      else
      {
        pValue = nullptr;
      }
    }
  }
  return pValue;
}

static PHB_PP_TOKEN hb_pp_pragmaGetInt(PHB_PP_TOKEN pToken, int *piValue)
{
  PHB_PP_TOKEN pValue = nullptr;

  if (pToken && pToken->pNext && HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_NUMBER)
  {
    if ((HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EQ && HB_PP_TOKEN_ISEOC(pToken->pNext->pNext)) ||
        (pToken->pNext->pNext && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_PB &&
         HB_PP_TOKEN_TYPE(pToken->pNext->pNext->type) == HB_PP_TOKEN_RIGHT_PB &&
         HB_PP_TOKEN_ISEOC(pToken->pNext->pNext->pNext)))
    {
      pValue = pToken->pNext;
      *piValue = atoi(pValue->value);
    }
  }
  return pValue;
}

static PHB_PP_TOKEN hb_pp_pragmaGetSwitch(PHB_PP_TOKEN pToken, int *piValue)
{
  PHB_PP_TOKEN pValue = nullptr;

  if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD)
  {
    HB_BOOL fNum = pToken->len > 1 && HB_PP_ISDIGIT(pToken->value[pToken->len - 1]);

    if (HB_PP_TOKEN_ISEOC(pToken->pNext))
    {
      if (fNum)
      {
        pValue = pToken;
        *piValue = pValue->value[pToken->len - 1] - '0';
      }
    }
    else if (HB_PP_TOKEN_ISEOC(pToken->pNext->pNext) && !fNum)
    {
      if (HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_MINUS)
      {
        pValue = pToken;
        *piValue = 0;
      }
      else if (HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_PLUS)
      {
        pValue = pToken;
        *piValue = 1;
      }
      else if (HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_NUMBER)
      {
        pValue = pToken;
        *piValue = atoi(pValue->pNext->value);
      }
    }
  }
  return pValue;
}

static void hb_pp_pragmaNew(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  PHB_PP_TOKEN pValue = nullptr;
  auto fError = false;
  HB_BOOL fValue = false;
  int iValue = 0;

  if (!pToken)
  {
    fError = true;
  }
  else if (pToken->len == 1 && HB_ISOPTSEP(pToken->value[0]))
  {
    if (!pState->iCondCompile)
    {
      pToken = pToken->pNext;
      pValue = hb_pp_pragmaGetSwitch(pToken, &iValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, pValue->value, iValue);
      }
      else
      {
        fError = true;
      }
    }
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD)
  {
    if (hb_pp_tokenValueCmp(pToken, "begindump", HB_PP_CMP_DBASE))
    {
      pState->iStreamDump = HB_PP_STREAM_DUMP_C;
      pState->iDumpLine = pState->pFile->iCurrentLine;
      if (!pState->pDumpBuffer)
      {
        pState->pDumpBuffer = hb_membufNew();
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "enddump", HB_PP_CMP_DBASE))
    {
      pState->iStreamDump = HB_PP_STREAM_OFF;
    }
    else if (hb_pp_tokenValueCmp(pToken, "__text", HB_PP_CMP_DBASE))
    {
      fError = hb_pp_pragmaStream(pState, pToken->pNext);
      if (!fError)
      {
        pState->iStreamDump = HB_PP_STREAM_CLIPPER;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__stream", HB_PP_CMP_DBASE))
    {
      fError = hb_pp_pragmaStream(pState, pToken->pNext);
      if (!fError)
      {
        pState->iStreamDump = HB_PP_STREAM_PRG;
        if (!pState->pStreamBuffer)
        {
          pState->pStreamBuffer = hb_membufNew();
        }
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__cstream", HB_PP_CMP_DBASE))
    {
      fError = hb_pp_pragmaStream(pState, pToken->pNext);
      if (!fError)
      {
        pState->iStreamDump = HB_PP_STREAM_C;
        if (!pState->pStreamBuffer)
        {
          pState->pStreamBuffer = hb_membufNew();
        }
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__streaminclude", HB_PP_CMP_DBASE))
    {
      if (pToken->pNext && HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_STRING)
      {
        fError = hb_pp_pragmaStream(pState, pToken->pNext->pNext);
        if (!fError && !pState->iCondCompile)
        {
          pState->iStreamDump = HB_PP_STREAM_PRG;
          hb_pp_pragmaStreamFile(pState, pToken->pNext->value);
          pState->iStreamDump = HB_PP_STREAM_OFF;
        }
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__cstreaminclude", HB_PP_CMP_DBASE))
    {
      if (pToken->pNext && HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_STRING)
      {
        fError = hb_pp_pragmaStream(pState, pToken->pNext->pNext);
        if (!fError && !pState->iCondCompile)
        {
          pState->iStreamDump = HB_PP_STREAM_C;
          hb_pp_pragmaStreamFile(pState, pToken->pNext->value);
          pState->iStreamDump = HB_PP_STREAM_OFF;
        }
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__binarystreaminclude", HB_PP_CMP_DBASE))
    {
      if (pToken->pNext && HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_STRING)
      {
        fError = hb_pp_pragmaStream(pState, pToken->pNext->pNext);
        if (!fError && !pState->iCondCompile)
        {
          pState->iStreamDump = HB_PP_STREAM_BINARY;
          hb_pp_pragmaStreamFile(pState, pToken->pNext->value);
          pState->iStreamDump = HB_PP_STREAM_OFF;
        }
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "__endtext", HB_PP_CMP_DBASE))
    {
      pState->iStreamDump = HB_PP_STREAM_OFF;
    }
    else if (pState->iCondCompile)
    {
      /* conditional compilation - other preprocessing and output disabled */
    }
    else if (hb_pp_tokenValueCmp(pToken, "AUTOMEMVAR", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "a", static_cast<int>(fValue));
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "DEBUGINFO", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "b", static_cast<int>(fValue));
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "DYNAMICMEMVAR", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "v", static_cast<int>(fValue));
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "ENABLEWARNINGS", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "w", fValue ? 1 : 0);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "ESCAPEDSTRINGS", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &pState->fEscStr);
      fError = pValue == nullptr;
    }
    else if (hb_pp_tokenValueCmp(pToken, "MULTILINESTRINGS", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &pState->fMultiLineStr);
      fError = pValue == nullptr;
    }
    else if (hb_pp_tokenValueCmp(pToken, "EXITSEVERITY", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetInt(pToken->pNext, &iValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "es", iValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "LINENUMBER", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "l", fValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "NOSTARTPROC", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetInt(pToken->pNext, &iValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "n", iValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "OPERATOR", HB_PP_CMP_DBASE))
    {
      fError = hb_pp_pragmaOperatorNew(pState, pToken->pNext);
    }
    else if (hb_pp_tokenValueCmp(pToken, "PREPROCESSING", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "p", fValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "SHORTCUT", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "z", fValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "RECURSELEVEL", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetInt(pToken->pNext, &pState->iMaxCycles);
      fError = pValue == nullptr;
    }
    else if (hb_pp_tokenValueCmp(pToken, "TEXTHIDDEN", HB_PP_CMP_DBASE))
    {
      /* xHarbour extension */
      pValue = hb_pp_pragmaGetInt(pToken->pNext, &iValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, pToken->value, iValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "TRACE", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &fValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "p+", fValue);
      }
      else
      {
        fError = true;
      }
    }
    else if (hb_pp_tokenValueCmp(pToken, "TRACEPRAGMAS", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetLogical(pToken->pNext, &pState->fTracePragmas);
      fError = pValue == nullptr;
    }
    else if (hb_pp_tokenValueCmp(pToken, "WARNINGLEVEL", HB_PP_CMP_DBASE))
    {
      pValue = hb_pp_pragmaGetInt(pToken->pNext, &iValue);
      if (pValue)
      {
        fError = hb_pp_setCompilerSwitch(pState, "w", iValue);
      }
      else
      {
        fError = true;
      }
    }
    else
    {
      fError = true;
    }
  }
  else
  {
    fError = true;
  }

  if (pState->iCondCompile)
  {
    ;
  }
  else if (fError)
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_PRAGMA, nullptr);
  }
  else if (pState->fTracePragmas || pState->fWriteTrace)
  {
    char szLine[12];

    hb_snprintf(szLine, sizeof(szLine), "%d", pState->pFile->iCurrentLine);
    hb_membufFlush(pState->pBuffer);
    hb_membufAddCh(pState->pBuffer, '(');
    hb_membufAddStr(pState->pBuffer, szLine);
    hb_membufAddStr(pState->pBuffer, ") #pragma ");
    hb_membufAddStr(pState->pBuffer, pToken->value);
    if (pValue && pValue != pToken)
    {
      hb_membufAddStr(pState->pBuffer, " set to '");
      hb_membufAddStr(pState->pBuffer, pValue->value);
      hb_membufAddCh(pState->pBuffer, '\'');
    }
    hb_membufAddCh(pState->pBuffer, '\n');
    if (pState->fWriteTrace)
    {
      if (fwrite(hb_membufPtr(pState->pBuffer), sizeof(char), hb_membufLen(pState->pBuffer), pState->file_trace) !=
          hb_membufLen(pState->pBuffer))
      {
        hb_pp_error(pState, 'F', HB_PP_ERR_WRITE_FILE, pState->szTraceFileName);
      }
    }
    if (pState->fTracePragmas)
    {
      hb_membufAddCh(pState->pBuffer, '\0');
      hb_pp_disp(pState, hb_membufPtr(pState->pBuffer));
    }
  }
}

static void hb_pp_defineNew(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, HB_BOOL fDirect)
{
  PHB_PP_TOKEN pMatch = pToken ? pToken->pNext : nullptr;

  if (!pMatch || HB_PP_TOKEN_TYPE(pMatch->type) != HB_PP_TOKEN_KEYWORD)
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_DEFINE_SYNTAX, nullptr);
  }
  else
  {
    PHB_PP_TOKEN pResult, pLast = pMatch->pNext, pParam;
    PHB_PP_MARKER pMarkers = nullptr;
    HB_USHORT usPCount = 0, usParam;

    /* pseudo function? */
    if (pLast && HB_PP_TOKEN_TYPE(pLast->type) == HB_PP_TOKEN_LEFT_PB && pLast->spaces == 0)
    {
      HB_USHORT type = HB_PP_TOKEN_KEYWORD;
      for (;;)
      {
        pLast = pLast->pNext;
        if (pLast && (usPCount == 0 || type == HB_PP_TOKEN_COMMA) &&
            HB_PP_TOKEN_TYPE(pLast->type) == HB_PP_TOKEN_RIGHT_PB)
        {
          break;
        }
        if (!pLast || type != HB_PP_TOKEN_TYPE(pLast->type))
        {
          if (type == HB_PP_TOKEN_KEYWORD)
          {
            hb_pp_error(pState, 'E', HB_PP_ERR_LABEL_MISSING_IN_DEFINE, nullptr);
          }
          else
          {
            hb_pp_error(pState, 'E', HB_PP_ERR_PARE_MISSING_IN_DEFINE, nullptr);
          }
          return;
        }
        else if (type == HB_PP_TOKEN_KEYWORD)
        {
          ++usPCount;
          type = HB_PP_TOKEN_COMMA;
        }
        else
        {
          type = HB_PP_TOKEN_KEYWORD;
        }
      }
    }
    else
    { /* simple keyword define */
      pLast = pMatch;
    }
    pResult = pLast->pNext;
    pLast->pNext = nullptr;
    pToken->pNext = hb_pp_tokenResultEnd(&pResult, fDirect);
    if (usPCount)
    {
      usPCount = 0;
      pParam = pMatch->pNext->pNext;
      while (HB_PP_TOKEN_TYPE(pParam->type) == HB_PP_TOKEN_KEYWORD)
      {
        usParam = 0;
        /* Check if it's not repeated ID */
        pLast = pMatch->pNext->pNext;
        while (pLast != pParam && !hb_pp_tokenEqual(pParam, pLast, HB_PP_CMP_CASE))
        {
          pLast = pLast->pNext;
        }
        if (pLast == pParam)
        {
          pLast = pResult;
          /* replace parameter tokens in result pattern with regular
             result markers */
          while (pLast)
          {
            if (hb_pp_tokenEqual(pParam, pLast, HB_PP_CMP_CASE))
            {
              HB_PP_TOKEN_SETTYPE(pLast, HB_PP_RMARKER_REGULAR);
              if (usParam == 0)
              {
                usParam = ++usPCount;
              }
              pLast->index = usParam;
            }
            pLast = pLast->pNext;
          }
        }
        HB_PP_TOKEN_SETTYPE(pParam, HB_PP_MMARKER_REGULAR);
        pParam->index = usParam;
        pParam = pParam->pNext;
        if (HB_PP_TOKEN_TYPE(pParam->type) == HB_PP_TOKEN_COMMA)
        {
          pParam = pParam->pNext;
        }
      }
      if (usPCount)
      {
        /* create regular match and result markers from parameters */
        pMarkers = static_cast<PHB_PP_MARKER>(hb_xgrabz(usPCount * sizeof(HB_PP_MARKER)));
      }
    }
    hb_pp_defineAdd(pState, HB_PP_CMP_CASE, usPCount, pMarkers, pMatch, pResult);
  }
}

static HB_BOOL hb_pp_tokenUnQuotedGet(PHB_PP_TOKEN **pTokenPtr, HB_BOOL *pfQuoted, HB_BOOL fFree)
{
  PHB_PP_TOKEN pToken = **pTokenPtr;

  *pfQuoted = false;
  if (pToken)
  {
    if (fFree)
    {
      **pTokenPtr = pToken->pNext;
      hb_pp_tokenFree(pToken);
    }
    else
    {
      *pTokenPtr = &pToken->pNext;
    }
    pToken = **pTokenPtr;
    if (pToken)
    {
      if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_BACKSLASH)
      {
        *pfQuoted = true;
        if (pToken->pNext)
        {
          pToken->pNext->spaces = pToken->spaces;
        }
        **pTokenPtr = pToken->pNext;
        hb_pp_tokenFree(pToken);
        pToken = **pTokenPtr;
      }
    }
  }

  return pToken != nullptr;
}

static HB_BOOL hb_pp_matchMarkerNew(PHB_PP_TOKEN *pTokenPtr, PHB_PP_MARKERLST *pMarkerListPtr)
{
  HB_USHORT type = HB_PP_TOKEN_NUL;
  PHB_PP_TOKEN pMarkerId = nullptr, pMTokens = nullptr;
  HB_BOOL fQuoted;

  /* At start pTokenPtr points to '<' token */

  if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted)
  {
    if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
    {
      pMarkerId = *pTokenPtr;
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted)
      {
        if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_MMARKER_REGULAR;
        }
        else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_COMMA)
        {
          int i = 3;
          do
          {
            if (!hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) || fQuoted)
            {
              break;
            }
            if (i == 3 && HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_EPSILON)
            {
              i = 0;
              break;
            }
            if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) != HB_PP_TOKEN_DOT)
            {
              break;
            }
          } while (--i > 0);
          if (i == 0 && hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
              HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
          {
            type = HB_PP_MMARKER_LIST;
          }
        }
        else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_SEND)
        {
          if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true))
          {
            PHB_PP_TOKEN pLast = nullptr;
            do
            {
              if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT && !fQuoted)
              {
                if (pLast)
                {
                  pMTokens = pMarkerId->pNext;
                  pMarkerId->pNext = *pTokenPtr;
                  pTokenPtr = &pMarkerId->pNext;
                  pLast->pNext = nullptr;
                }
                type = HB_PP_MMARKER_RESTRICT;
                break;
              }
              pLast = *pTokenPtr;
            } while (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false));
          }
        }
      }
    }
    else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_MULT)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_MULT &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_MMARKER_WILD;
        }
      }
    }
    else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LEFT_PB)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_RIGHT_PB &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_MMARKER_EXTEXP;
        }
      }
    }
    else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_NOT)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_NOT &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_MMARKER_NAME;
        }
      }
    }
  }

  if (type != HB_PP_TOKEN_NUL)
  {
    PHB_PP_MARKERLST pMrkLst = *pMarkerListPtr, pMrkPrev = nullptr;
    PHB_PP_MARKERPTR pMrkPtr;

    while (pMrkLst && !hb_pp_tokenEqual(pMrkLst->pMatchMarkers->pToken, pMarkerId, HB_PP_CMP_CASE))
    {
      pMrkPrev = pMrkLst;
      pMrkLst = pMrkLst->pNext;
    }
    if (!pMrkLst)
    {
      pMrkLst = static_cast<PHB_PP_MARKERLST>(hb_xgrab(sizeof(HB_PP_MARKERLST)));
      if (pMrkPrev)
      {
        pMrkPrev->pNext = pMrkLst;
      }
      else
      {
        *pMarkerListPtr = pMrkLst;
      }
      pMrkLst->pNext = nullptr;
      pMrkLst->pMatchMarkers = nullptr;
      pMrkLst->canrepeat = true;
      pMrkLst->index = 0;
    }
    pMrkPtr = static_cast<PHB_PP_MARKERPTR>(hb_xgrab(sizeof(HB_PP_MARKERPTR)));
    pMrkPtr->pNext = pMrkLst->pMatchMarkers;
    pMrkLst->pMatchMarkers = pMrkPtr;
    pMrkPtr->pToken = pMarkerId;
    pMrkPtr->pMTokens = pMTokens;
    pMrkPtr->type = type;
    /* mark non restricted markers for later detection two consecutive
       optional match markers */
    if (type != HB_PP_MMARKER_RESTRICT)
    {
      pMarkerId->type |= HB_PP_TOKEN_MATCHMARKER;
    }
    /* free the trailing '>' marker token */
    pMTokens = *pTokenPtr;
    *pTokenPtr = pMTokens->pNext;
    hb_pp_tokenFree(pMTokens);
    return true;
  }
  return false;
}

static HB_BOOL hb_pp_matchHasKeywords(PHB_PP_TOKEN pToken)
{
  /* Now we are strictly Clipper compatible here though the nested
     optional markers which have keywords on deeper levels are not
     recognized. Exactly the same makes Clipper PP */
  while (HB_PP_TOKEN_ISMATCH(pToken))
  {
    pToken = pToken->pNext;
  }
  return pToken != nullptr;
}

static HB_BOOL hb_pp_matchPatternNew(PHB_PP_STATE pState, PHB_PP_TOKEN *pTokenPtr, PHB_PP_MARKERLST *pMarkerListPtr,
                                     PHB_PP_TOKEN **pOptional)
{
  PHB_PP_TOKEN *pLastPtr = nullptr;
  HB_BOOL fQuoted = false;

  if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_BACKSLASH)
  {
    PHB_PP_TOKEN pToken = *pTokenPtr;
    *pTokenPtr = pToken->pNext;
    hb_pp_tokenFree(pToken);
    fQuoted = true;
  }

  do
  {
    if (!fQuoted)
    {
      if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LT)
      {
        if (!hb_pp_matchMarkerNew(pTokenPtr, pMarkerListPtr))
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_BAD_MATCH_MARKER, nullptr);
          return false;
        }
        /* now pTokenPtr points to marker keyword, all other tokens
           have been stripped */
      }
      else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_RIGHT_SB)
      {
        if (pOptional)
        {
          *pOptional = pTokenPtr;
          return true;
        }
      }
      else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LEFT_SB)
      {
        PHB_PP_TOKEN *pStopOptPtr = nullptr;
        if (!(*pTokenPtr)->pNext)
        {
          /* assign pOptional only to force error below */
          pOptional = &pTokenPtr;
          break;
        }
        else if (!hb_pp_matchPatternNew(pState, &(*pTokenPtr)->pNext, pMarkerListPtr, &pStopOptPtr))
        {
          return false;
        }
        else if (*pStopOptPtr == (*pTokenPtr)->pNext)
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_EMPTY_OPTIONAL, nullptr);
          return false;
        }
        else
        {
          PHB_PP_TOKEN pToken, pOptTok = (*pTokenPtr)->pNext;
          pToken = *pStopOptPtr;
          *pStopOptPtr = nullptr;
          (*pTokenPtr)->pNext = pToken->pNext;
          hb_pp_tokenFree(pToken);
          /* create new optional match marker */
          HB_PP_TOKEN_SETTYPE(*pTokenPtr, HB_PP_MMARKER_OPTIONAL);
          if ((*pTokenPtr)->spaces > 1)
          {
            (*pTokenPtr)->spaces = 1;
          }
          (*pTokenPtr)->type |= HB_PP_TOKEN_MATCHMARKER;
          (*pTokenPtr)->pMTokens = pOptTok;
          if (pLastPtr && !hb_pp_matchHasKeywords(*pLastPtr))
          {
            if (!hb_pp_matchHasKeywords(pOptTok))
            {
              hb_pp_error(pState, 'E', HB_PP_ERR_AMBIGUOUS_MATCH_PATTERN, nullptr);
              return false;
            }
            /* replace the order for these optional tokens to keep
               the ones with keywords 1st */
            (*pTokenPtr)->pMTokens = *pLastPtr;
            *pLastPtr = pOptTok;
          }
          pLastPtr = &(*pTokenPtr)->pMTokens;
          /* to skip resetting pLastPtr below */
          continue;
        }
      }
    }
    pLastPtr = nullptr;
  } while (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false));

  if (pOptional)
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_UNCLOSED_OPTIONAL, nullptr);
    return false;
  }

  return true;
}

static HB_BOOL hb_pp_resultMarkerNew(PHB_PP_STATE pState, PHB_PP_TOKEN *pTokenPtr, PHB_PP_MARKERLST *pMarkerListPtr,
                                     HB_BOOL fDump, HB_BOOL fOptional, HB_USHORT *pusPCount, HB_SIZE spaces)
{
  HB_USHORT type = HB_PP_TOKEN_NUL, rtype;
  PHB_PP_TOKEN pMarkerId = nullptr, pToken;
  HB_BOOL fQuoted;

  /* At start pTokenPtr points to '<' token */
  if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted)
  {
    rtype = HB_PP_TOKEN_TYPE((*pTokenPtr)->type);
    if (rtype == HB_PP_TOKEN_KEYWORD || rtype == HB_PP_TOKEN_STRING)
    { /* TODO: switch ? */
      pMarkerId = *pTokenPtr;
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
      {
        if (rtype == HB_PP_TOKEN_STRING)
        {
          type = HB_PP_RMARKER_STRSTD;
          HB_PP_TOKEN_SETTYPE(pMarkerId, HB_PP_TOKEN_KEYWORD);
        }
        else
        {
          type = fDump ? HB_PP_RMARKER_STRDUMP : HB_PP_RMARKER_REGULAR;
        }
      }
    }
    else if (rtype == HB_PP_TOKEN_LEFT_PB)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_RIGHT_PB &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_RMARKER_STRSMART;
        }
      }
    }
    else if (rtype == HB_PP_TOKEN_LEFT_CB)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_RIGHT_CB &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_RMARKER_BLOCK;
        }
      }
    }
    else if (rtype == HB_PP_TOKEN_DOT)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_DOT &&
            hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
            HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
        {
          type = HB_PP_RMARKER_LOGICAL;
        }
      }
    }
    else if (rtype == HB_PP_TOKEN_MINUS)
    {
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
      {
        pMarkerId = *pTokenPtr;
        if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false) && !fQuoted)
        {
          /* <-id-> was bad choice for marker type because -> is single
             ALIAS token so we have to add workaround for it now */
          if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_ALIAS ||
              (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_MINUS &&
               hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
               HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT))
          {
            type = HB_PP_RMARKER_NUL;
          }
        }
      }
    }
    else if (rtype == HB_PP_TOKEN_REFERENCE)
    {
      /* <@> */
      if (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, true) && !fQuoted &&
          HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_GT)
      {
        type = HB_PP_RMARKER_REFERENCE;
      }
    }
  }

  if (type == HB_PP_TOKEN_NUL)
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_WRONG_LABEL, nullptr);
  }
  else if (type == HB_PP_RMARKER_REFERENCE)
  {
    hb_pp_tokenSetValue(*pTokenPtr, "~", 1);
    HB_PP_TOKEN_SETTYPE(*pTokenPtr, type);
    return true;
  }
  else
  {
    PHB_PP_MARKERLST pMrkLst = *pMarkerListPtr;

    while (pMrkLst && !hb_pp_tokenEqual(pMrkLst->pMatchMarkers->pToken, pMarkerId, HB_PP_CMP_CASE))
    {
      pMrkLst = pMrkLst->pNext;
    }

    if (!pMrkLst)
    {
      hb_pp_error(pState, 'E', HB_PP_ERR_UNKNOWN_RESULT_MARKER, nullptr);
    }
    else
    {
      if (!pMrkLst->index)
      {
        pMrkLst->index = ++(*pusPCount);
      }
      if (!fOptional)
      {
        pMrkLst->canrepeat = false;
      }
      HB_PP_TOKEN_SETTYPE(pMarkerId, type);
      pMarkerId->index = pMrkLst->index;
      pMarkerId->spaces = spaces;
      /* free the trailing '>' marker token */
      pToken = *pTokenPtr;
      *pTokenPtr = pToken->pNext;
      hb_pp_tokenFree(pToken);
      return true;
    }
  }
  return false;
}

static HB_BOOL hb_pp_patternCompare(PHB_PP_TOKEN pToken1, PHB_PP_TOKEN pToken2)
{
  while (pToken1 && pToken2)
  {
    if (!hb_pp_tokenEqual(pToken1, pToken2, HB_PP_CMP_STD))
    {
      break;
    }
    if (HB_PP_TOKEN_TYPE(pToken1->type) == HB_PP_MMARKER_RESTRICT ||
        HB_PP_TOKEN_TYPE(pToken1->type) == HB_PP_MMARKER_OPTIONAL ||
        HB_PP_TOKEN_TYPE(pToken1->type) == HB_PP_RMARKER_OPTIONAL)
    {
      if (!hb_pp_patternCompare(pToken1->pMTokens, pToken2->pMTokens))
      {
        break;
      }
    }
    pToken1 = pToken1->pNext;
    pToken2 = pToken2->pNext;
  }
  return !pToken1 && !pToken2;
}

static void hb_pp_directiveDel(PHB_PP_STATE pState, PHB_PP_TOKEN pMatch, HB_USHORT markers, PHB_PP_MARKER pMarkers,
                               HB_USHORT mode, HB_BOOL fCommand)
{
  PHB_PP_RULE pRule, *pRulePtr = fCommand ? &pState->pCommands : &pState->pTranslations;

  while (*pRulePtr)
  {
    pRule = *pRulePtr;
    if (HB_PP_CMP_MODE(pRule->mode) == mode && pRule->markers == markers)
    {
      HB_USHORT u;
      for (u = 0; u < markers; ++u)
      {
        if (pRule->pMarkers[u].canrepeat != pMarkers[u].canrepeat)
        {
          break;
        }
      }
      if (u == markers && hb_pp_patternCompare(pRule->pMatch, pMatch))
      {
        *pRulePtr = pRule->pPrev;
        hb_pp_ruleFree(pRule);
        if (fCommand)
        {
          pState->iCommands--;
        }
        else
        {
          pState->iTranslations--;
        }
        return;
      }
    }
    pRulePtr = &pRule->pPrev;
  }
}

static void hb_pp_directiveNew(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, HB_USHORT mode, HB_BOOL fCommand,
                               HB_BOOL fDirect, HB_BOOL fDelete)
{
  PHB_PP_TOKEN pResult, pMatch, pStart, pLast;
  auto fValid = false;

#ifdef HB_CLP_STRICT
  HB_SYMBOL_UNUSED(fDirect);
#endif

  pMatch = pResult = pLast = nullptr;
  if (pToken->pNext)
  {
    pStart = pToken->pNext;
    while (!HB_PP_TOKEN_ISEOP(pStart, fDirect))
    {
      if (pMatch)
      {
        /* Clipper PP makes something like that for result pattern of
         #[x]translate and #[x]command */
        if (pStart->spaces > 1)
        {
          pStart->spaces = 1;
        }
      }
      else if (pStart->pNext && HB_PP_TOKEN_TYPE(pStart->type) == HB_PP_TOKEN_EQ &&
               HB_PP_TOKEN_TYPE(pStart->pNext->type) == HB_PP_TOKEN_GT)
      {
        fValid = true;
        if (!pLast)
        {
          break;
        }

        pLast->pNext = nullptr;
        pMatch = pToken->pNext;
        pToken->pNext = pStart;
        pToken = pStart = pStart->pNext;
      }
      pLast = pStart;
      pStart = pStart->pNext;
    }
    if (pMatch && pLast != pToken)
    {
      pLast->pNext = nullptr;
      pResult = pToken->pNext;
      pToken->pNext = pStart;
    }
  }

  if (!fValid)
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_MISSING_PATTERN_SEP, nullptr);
  }
  else if (pMatch)
  { /* isn't dummy directive? */
    PHB_PP_MARKERLST pMarkerList = nullptr, pMrkLst;
    PHB_PP_MARKERPTR pMrkPtr;
    PHB_PP_MARKER pMarkers = nullptr;
    HB_USHORT usPCount = 0;

    fValid = hb_pp_matchPatternNew(pState, &pMatch, &pMarkerList, nullptr);
    if (fValid)
    {
      if (pResult)
      {
        PHB_PP_TOKEN *pTokenPtr, *pDumpPtr = nullptr, *pOptStart = nullptr;
        HB_BOOL fQuoted = false;

        if (HB_PP_TOKEN_TYPE(pResult->type) == HB_PP_TOKEN_BACKSLASH)
        {
          fQuoted = true;
          pLast = pResult;
          pResult = pResult->pNext;
          hb_pp_tokenFree(pLast);
        }
        pTokenPtr = &pResult;
        do
        {
          if (!fQuoted)
          {
            if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_HASH)
            {
              pDumpPtr = pTokenPtr;
              /* to skip pDumpPtr reseting below */
              continue;
            }
            else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LT)
            {
              HB_SIZE spaces = (*pTokenPtr)->spaces;
              /* Free the string dump token: '#'. Clipper PP always
                 does it without checking type of next marker */
              if (pDumpPtr)
              {
                pLast = *pDumpPtr;
                spaces = pLast->spaces;
                *pDumpPtr = pLast->pNext;
                hb_pp_tokenFree(pLast);
                pTokenPtr = pDumpPtr;
              }

              if (!hb_pp_resultMarkerNew(pState, pTokenPtr, &pMarkerList, pDumpPtr != nullptr, pOptStart != nullptr,
                                         &usPCount, spaces))
              {
                fValid = false;
                break;
              }
              /* now pTokenPtr points to marker keyword, all other tokens
                 have been stripped */
            }
            else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LEFT_SB)
            {
              if (pOptStart)
              {
                fValid = false;
                hb_pp_error(pState, 'E', HB_PP_ERR_NESTED_OPTIONAL, nullptr);
                break;
              }
              pOptStart = pTokenPtr;
            }
            else if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_RIGHT_SB && pOptStart)
            {
              pLast = *pTokenPtr;
              *pTokenPtr = nullptr;
              (*pOptStart)->pMTokens = (*pOptStart)->pNext;
              (*pOptStart)->pNext = pLast->pNext;
              HB_PP_TOKEN_SETTYPE(*pOptStart, HB_PP_RMARKER_OPTIONAL);
#ifndef HB_CLP_STRICT
              /* This is not Clipper compatible but we have word
                 concatenation and without this modification we
                 will introduce very serious bug */
              if ((*pOptStart)->pMTokens && (*pOptStart)->pMTokens->spaces == 0 && (*pOptStart)->spaces > 0 &&
                  HB_PP_TOKEN_TYPE((*pOptStart)->pMTokens->type) != HB_PP_TOKEN_COMMA)
              {
                (*pOptStart)->pMTokens->spaces = 1;
              }
#endif
              pTokenPtr = pOptStart;
              pOptStart = nullptr;
              hb_pp_tokenFree(pLast);
            }
          }
          /* reset pDumpPtr */
          pDumpPtr = nullptr;
        } while (hb_pp_tokenUnQuotedGet(&pTokenPtr, &fQuoted, false));

        if (fValid && pOptStart)
        {
          fValid = false;
          hb_pp_error(pState, 'E', HB_PP_ERR_UNKNOWN_RESULT_MARKER, nullptr);
        }
      }
    }

    if (fValid && usPCount)
    {
      /* create regular match and result markers from parameters */
      pMarkers = static_cast<PHB_PP_MARKER>(hb_xgrabz(usPCount * sizeof(HB_PP_MARKER)));
    }

    /* free marker index list */
    while (pMarkerList)
    {
      pMrkLst = pMarkerList;
      while (pMrkLst->pMatchMarkers)
      {
        pMrkPtr = pMrkLst->pMatchMarkers;
        pMrkLst->pMatchMarkers = pMrkPtr->pNext;
        /* set match token type and parameters */
        if (pMarkers && pMrkLst->index)
        {
          pMarkers[pMrkLst->index - 1].canrepeat = pMrkLst->canrepeat;
          pMrkPtr->pToken->index = pMrkLst->index;
        }
        pMrkPtr->pToken->pMTokens = pMrkPtr->pMTokens;
        HB_PP_TOKEN_SETTYPE(pMrkPtr->pToken, pMrkPtr->type);
        hb_xfree(pMrkPtr);
      }
      pMarkerList = pMarkerList->pNext;
      hb_xfree(pMrkLst);
    }

    if (fValid)
    {
      if (fDelete)
      {
        hb_pp_directiveDel(pState, pMatch, usPCount, pMarkers, mode, fCommand);
        if (pMarkers)
        {
          hb_xfree(pMarkers);
        }
      }
      else
      {
        PHB_PP_RULE pRule;
        pRule = hb_pp_ruleNew(pMatch, pResult, mode, usPCount, pMarkers);
        if (fCommand)
        {
          pRule->pPrev = pState->pCommands;
          pState->pCommands = pRule;
          pState->iCommands++;
          hb_pp_ruleSetId(pState, pMatch, HB_PP_COMMAND);
        }
        else
        {
          pRule->pPrev = pState->pTranslations;
          pState->pTranslations = pRule;
          pState->iTranslations++;
          hb_pp_ruleSetId(pState, pMatch, HB_PP_TRANSLATE);
        }
        pMatch = pResult = nullptr;
      }
    }
  }
  hb_pp_tokenListFree(&pMatch);
  hb_pp_tokenListFree(&pResult);
}

static bool hb_pp_tokenStartExtBlock(PHB_PP_TOKEN *pTokenPtr)
{
  PHB_PP_TOKEN pToken = *pTokenPtr;

  if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_CB && pToken->pNext &&
      HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_PIPE)
  {
    HB_USHORT prevtype = HB_PP_TOKEN_COMMA;
    pToken = pToken->pNext->pNext;
    while (pToken)
    {
      HB_USHORT type = HB_PP_TOKEN_TYPE(pToken->type);
      if (((type == HB_PP_TOKEN_KEYWORD || type == HB_PP_TOKEN_EPSILON) && prevtype == HB_PP_TOKEN_COMMA) ||
          (type == HB_PP_TOKEN_COMMA && prevtype == HB_PP_TOKEN_KEYWORD))
      {
        prevtype = type;
        pToken = pToken->pNext;
      }
      else
      {
        break;
      }
    }
    if (pToken && pToken->pNext && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_PIPE &&
        HB_PP_TOKEN_ISEOC(pToken->pNext))
    {
      *pTokenPtr = pToken->pNext;
      return true;
    }
  }
  return false;
}

static bool hb_pp_tokenStopExtBlock(PHB_PP_TOKEN *pTokenPtr)
{
  PHB_PP_TOKEN pToken = *pTokenPtr;

  if (HB_PP_TOKEN_ISEOC(pToken) && pToken->pNext)
  {
    pToken = pToken->pNext;
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_CB)
    {
      *pTokenPtr = pToken->pNext;
      return true;
    }
    if (pToken->pNext && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD &&
        HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_KEYWORD)
    {
      PHB_PP_TOKEN pFirst = pToken;

      if (hb_pp_tokenValueCmp(pToken, "INIT", HB_PP_CMP_DBASE) ||
          hb_pp_tokenValueCmp(pToken, "EXIT", HB_PP_CMP_DBASE) ||
          hb_pp_tokenValueCmp(pToken, "STATIC", HB_PP_CMP_DBASE))
      {
        pToken = pToken->pNext;
      }

      if (hb_pp_tokenValueCmp(pToken, "FUNCTION", HB_PP_CMP_DBASE) ||
          hb_pp_tokenValueCmp(pToken, "PROCEDURE", HB_PP_CMP_DBASE))
      {
        if (pToken != pFirst || HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_KEYWORD)
        {
          *pTokenPtr = pFirst;
        }
        return true;
      }
    }
  }
  return false;
}

static bool hb_pp_tokenSkipExp(PHB_PP_TOKEN *pTokenPtr, PHB_PP_TOKEN pStop, HB_USHORT mode, HB_BOOL *pfStop)
{
  HB_USHORT curtype, prevtype = 0, lbrtype = 0, rbrtype = 0;
  PHB_PP_TOKEN pToken = *pTokenPtr, pPrev;
  int iBraces = 0;
  auto fMatch = false;

  if (pfStop)
  {
    *pfStop = false;
  }

  for (;;)
  {
    pPrev = pToken;
    if (hb_pp_tokenStartExtBlock(&pToken))
    {
      int iExtBlock = 1;
      while (pToken)
      {
        if (hb_pp_tokenStartExtBlock(&pToken))
        {
          iExtBlock++;
        }
        else if (hb_pp_tokenStopExtBlock(&pToken))
        {
          if (--iExtBlock == 0)
          {
            break;
          }
        }
        else
        {
          pToken = pToken->pNext;
        }
      }
      if (iExtBlock)
      {
        pToken = pPrev;
      }
    }

    if (mode == HB_PP_CMP_ADDR ? pToken == pStop : HB_PP_TOKEN_ISEOC(pToken))
    {
      if (pfStop)
      {
        *pfStop = true;
      }
      break;
    }
    curtype = HB_PP_TOKEN_TYPE(pToken->type);
    if (iBraces)
    {
      if (curtype == lbrtype)
      {
        ++iBraces;
      }
      else if (curtype == rbrtype)
      {
        --iBraces;
      }
    }
    else if (curtype == HB_PP_TOKEN_COMMA)
    {
      if (pfStop)
      {
        if (mode != HB_PP_CMP_ADDR && HB_PP_TOKEN_NEEDRIGHT(prevtype))
        {
          *pfStop = true;
        }
        else
        {
          pToken = pToken->pNext;
        }
      }
      break;
    }
    else if (mode != HB_PP_CMP_ADDR &&
             (HB_PP_TOKEN_CLOSE_BR(curtype) || (!HB_PP_TOKEN_CANJOIN(curtype) && !HB_PP_TOKEN_CANJOIN(prevtype)) ||
              (HB_PP_TOKEN_NEEDRIGHT(prevtype) && !HB_PP_TOKEN_ISEXPTOKEN(pToken)) ||
              (pStop && hb_pp_tokenEqual(pToken, pStop, mode))))
    {
      if (pfStop)
      {
        *pfStop = true;
      }
      break;
    }
    else if (HB_PP_TOKEN_OPEN_BR(curtype))
    {
      lbrtype = curtype;
      rbrtype = (curtype == HB_PP_TOKEN_LEFT_PB
                     ? HB_PP_TOKEN_RIGHT_PB
                     : (curtype == HB_PP_TOKEN_LEFT_SB ? HB_PP_TOKEN_RIGHT_SB : HB_PP_TOKEN_RIGHT_CB));
      ++iBraces;
    }
    if (!HB_PP_TOKEN_ISNEUTRAL(curtype))
    {
      prevtype = curtype;
    }
    pToken = pToken->pNext;
  }

  fMatch = pToken != *pTokenPtr;
  *pTokenPtr = pToken;

  return fMatch;
}

static bool hb_pp_tokenCanStartExp(PHB_PP_TOKEN pToken)
{
  if (!HB_PP_TOKEN_NEEDLEFT(pToken) && !HB_PP_TOKEN_ISEOC(pToken))
  {
    if (HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_LEFT_SB)
    {
      return true;
    }
    else
    {
      PHB_PP_TOKEN pEoc = nullptr;

      pToken = pToken->pNext;
      while (!HB_PP_TOKEN_ISEOL(pToken))
      {
        if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_SB)
        {
          if (pEoc)
          {
            do
            {
              if (HB_PP_TOKEN_TYPE(pEoc->type) == HB_PP_TOKEN_EOC)
              {
                HB_PP_TOKEN_SETTYPE(pEoc, HB_PP_TOKEN_TEXT);
              }
              pEoc = pEoc->pNext;
            } while (pEoc != pToken);
          }
          return true;
        }
        if (!pEoc && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_EOC)
        {
          pEoc = pToken;
        }
        pToken = pToken->pNext;
      }
    }
  }
  return false;
}

static bool hb_pp_tokenMatch(PHB_PP_TOKEN pMatch, PHB_PP_TOKEN *pTokenPtr, PHB_PP_TOKEN pStop, HB_USHORT mode)
{
  auto fMatch = false;
  HB_USHORT type;

  type = HB_PP_TOKEN_TYPE(pMatch->type);
  if (type == HB_PP_MMARKER_REGULAR)
  {
    if (hb_pp_tokenCanStartExp(*pTokenPtr))
    {
      if (!pStop)
      {
        pStop = pMatch->pNext;
      }
      fMatch = hb_pp_tokenSkipExp(pTokenPtr, pStop, mode, nullptr);
    }
  }
  else if (type == HB_PP_MMARKER_LIST)
  {
    if (hb_pp_tokenCanStartExp(*pTokenPtr))
    {
      HB_BOOL fStop = false;
      if (!pStop)
      {
        pStop = pMatch->pNext;
      }
      do
      {
        if (!hb_pp_tokenSkipExp(pTokenPtr, pStop, mode, &fStop))
        {
          break;
        }
        fMatch = true;
      } while (!fStop);
    }
  }
  else if (type == HB_PP_MMARKER_RESTRICT)
  {
    PHB_PP_TOKEN pRestrict = pMatch->pMTokens, pToken = *pTokenPtr;

    /*
     * Here we are strictly Clipper compatible. Clipper accepts dummy
     * restrict marker which starts from comma, <id: ,[ something,...]>
     * which always match empty expression. The same effect can be
     * reached by giving ,, in the world list on other positions.
     */
    while (pRestrict)
    {
      if (HB_PP_TOKEN_TYPE(pRestrict->type) == HB_PP_TOKEN_COMMA)
      {
        *pTokenPtr = pToken;
        fMatch = true;
        break;
      }
      else if (HB_PP_TOKEN_TYPE(pRestrict->type) == HB_PP_TOKEN_AMPERSAND &&
               (!pRestrict->pNext || HB_PP_TOKEN_TYPE(pRestrict->pNext->type) == HB_PP_TOKEN_COMMA) &&
               (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROVAR ||
                HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROTEXT ||
                (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_AMPERSAND && pToken->pNext &&
                 HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_LEFT_PB)))
      {
        if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROVAR ||
            HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROTEXT)
        {
          *pTokenPtr = pToken->pNext;
        }
        else
        {
          int iBraces = 1;
          pToken = pToken->pNext->pNext;
          while (iBraces > 0 && !HB_PP_TOKEN_ISEOC(pToken))
          {
            if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_PB)
            {
              ++iBraces;
            }
            else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_PB)
            {
              --iBraces;
            }
            pToken = pToken->pNext;
          }
          *pTokenPtr = pToken;
        }
        fMatch = true;
        break;
      }
      else if (!HB_PP_TOKEN_ISEOC(pToken) && hb_pp_tokenEqual(pToken, pRestrict, mode))
      {
        pToken = pToken->pNext;
        pRestrict = pRestrict->pNext;
        if (!pRestrict)
        {
          *pTokenPtr = pToken;
          fMatch = true;
          break;
        }
      }
      else
      {
        pToken = *pTokenPtr;
        do
        {
          type = HB_PP_TOKEN_TYPE(pRestrict->type);
          pRestrict = pRestrict->pNext;
        } while (pRestrict && type != HB_PP_TOKEN_COMMA);
      }
    }
  }
  else if (type == HB_PP_MMARKER_WILD)
  {
    /* TODO? now we are strictly Clipper compatible, but we may
       want to add some additional stop markers in the future here
       to support wild match markers also as not the last expression */
    if (!HB_PP_TOKEN_ISEOS(*pTokenPtr))
    {
      fMatch = true;
      do
      {
        *pTokenPtr = (*pTokenPtr)->pNext;
      } while (!HB_PP_TOKEN_ISEOS(*pTokenPtr));
    }
  }
  else if (type == HB_PP_MMARKER_EXTEXP)
  {
    if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) != HB_PP_TOKEN_RIGHT_PB &&
        HB_PP_TOKEN_TYPE((*pTokenPtr)->type) != HB_PP_TOKEN_RIGHT_SB &&
        HB_PP_TOKEN_TYPE((*pTokenPtr)->type) != HB_PP_TOKEN_COMMA && hb_pp_tokenCanStartExp(*pTokenPtr))
    {
      if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_LEFT_PB)
      {
        if (!pStop)
        {
          pStop = pMatch->pNext;
        }
        fMatch = hb_pp_tokenSkipExp(pTokenPtr, pStop, mode, nullptr);
      }
      else
      {
        do
        {
          *pTokenPtr = (*pTokenPtr)->pNext;
        } while (!HB_PP_TOKEN_ISEOC(*pTokenPtr) && (*pTokenPtr)->spaces == 0 &&
                 HB_PP_TOKEN_TYPE((*pTokenPtr)->type) != HB_PP_TOKEN_COMMA);

        fMatch = true;
      }
    }
  }
  else if (type == HB_PP_MMARKER_NAME)
  {
    if (HB_PP_TOKEN_TYPE((*pTokenPtr)->type) == HB_PP_TOKEN_KEYWORD)
    {
      *pTokenPtr = (*pTokenPtr)->pNext;
      fMatch = true;
    }
  }
  else if (hb_pp_tokenEqual(*pTokenPtr, pMatch, mode))
  {
    *pTokenPtr = (*pTokenPtr)->pNext;
    fMatch = true;
  }

  return fMatch;
}

static bool hb_pp_patternMatch(PHB_PP_TOKEN pMatch, PHB_PP_TOKEN *pTokenPtr, PHB_PP_TOKEN pStop, HB_USHORT mode,
                               PHB_PP_RULE pRule)
{
  PHB_PP_TOKEN pToken = *pTokenPtr;
  PHB_PP_TOKEN pFirst;
  auto fOverflow = false;

  while (pMatch && !HB_PP_TOKEN_ISEOS(pToken))
  {
    if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_MMARKER_OPTIONAL)
    {
      PHB_PP_TOKEN pOptional = pMatch, pLast, pNewStop = pMatch->pNext;

      while (pNewStop && HB_PP_TOKEN_TYPE(pNewStop->type) == HB_PP_MMARKER_OPTIONAL)
      {
        pNewStop = pNewStop->pNext;
      }

      do
      {
        pLast = pOptional;
        pFirst = pToken;
        if (hb_pp_patternMatch(pOptional->pMTokens, &pToken, pNewStop, mode, nullptr) && pFirst != pToken)
        {
          if (pRule && !hb_pp_patternMatch(pOptional->pMTokens, &pFirst, pNewStop, mode, pRule))
          {
            fOverflow = true;
            break;
          }
          pOptional = pMatch;
        }
        else
        {
          pOptional = pOptional->pNext;
        }
      } while (pOptional && HB_PP_TOKEN_TYPE(pOptional->type) == HB_PP_MMARKER_OPTIONAL && !HB_PP_TOKEN_ISEOS(pToken));
      pMatch = pLast;
    }
    else
    {
      pFirst = pToken;
      if (hb_pp_tokenMatch(pMatch, &pToken, pStop, mode))
      {
        if (pRule && pMatch->index && pFirst != pToken)
        {
          if (!hb_pp_patternAddResult(pRule, pMatch->index, pFirst, pToken))
          {
            fOverflow = true;
            break;
          }
        }
      }
      else
      {
        break;
      }
    }

    pMatch = pMatch->pNext;
  }

  if (!fOverflow)
  {
    while (pMatch && HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_MMARKER_OPTIONAL)
    {
      pMatch = pMatch->pNext;
    }
    if (pMatch == nullptr)
    {
      *pTokenPtr = pToken;
      if (pRule)
      {
        pRule->pNextExpr = pToken;
      }
      return true;
    }
  }
  return false;
}

static bool hb_pp_patternCmp(PHB_PP_RULE pRule, PHB_PP_TOKEN pToken, HB_BOOL fCommand)
{
  PHB_PP_TOKEN pFirst = pToken;

  if (hb_pp_patternMatch(pRule->pMatch, &pToken, nullptr, HB_PP_CMP_MODE(pRule->mode), nullptr))
  {
    if (!fCommand || HB_PP_TOKEN_ISEOC(pToken))
    {
      if (hb_pp_patternMatch(pRule->pMatch, &pFirst, nullptr, HB_PP_CMP_MODE(pRule->mode), pRule))
      {
        return true;
      }
      else
      {
        hb_pp_patternClearResults(pRule);
      }
    }
  }
  return false;
}

static PHB_PP_RESULT hb_pp_matchResultGet(PHB_PP_RULE pRule, HB_USHORT usMatch, HB_USHORT usIndex)
{
  PHB_PP_MARKER pMarker = &pRule->pMarkers[usIndex - 1];
  PHB_PP_RESULT pMarkerResult;

  /* Clipper PP does not check status of match marker but only how many
     different values were assigned to match pattern */
  if (pMarker->matches == 1)
  {
    pMarkerResult = pMarker->pResult;
  }
  else if (usMatch < pMarker->matches)
  {
    pMarkerResult = pMarker->pResult;
    while (usMatch--)
    {
      pMarkerResult = pMarkerResult->pNext;
    }
  }
  else
  {
    pMarkerResult = nullptr;
  }

  return pMarkerResult;
}

static PHB_PP_TOKEN *hb_pp_matchResultLstAdd(PHB_PP_STATE pState, HB_SIZE spaces, HB_USHORT type,
                                             PHB_PP_TOKEN *pResultPtr, PHB_PP_TOKEN pToken, PHB_PP_TOKEN pStop)
{
  PHB_PP_TOKEN pNext;
  auto fFirst = true;
  HB_BOOL fStop = false;

  for (;;)
  {
    pNext = pToken;
    if (hb_pp_tokenSkipExp(&pNext, pStop, HB_PP_CMP_ADDR, &fStop) && (fStop ? pToken : pToken->pNext) != pNext)
    {
      /* Check for '&' token followed by single keyword or '('
         token and do not stringify such expressions but
         clone them */
      if (type == HB_PP_RMARKER_BLOCK)
      {
        HB_BOOL fBlock = HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_CB && pToken->pNext &&
                         (fStop ? pToken->pNext : pToken->pNext->pNext) != pNext &&
                         HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_PIPE;

        if (!fBlock)
        {
          hb_pp_tokenAdd(&pResultPtr, "{", 1, fFirst ? spaces : 1, HB_PP_TOKEN_LEFT_CB | HB_PP_TOKEN_STATIC);
          hb_pp_tokenAdd(&pResultPtr, "|", 1, 0, HB_PP_TOKEN_PIPE | HB_PP_TOKEN_STATIC);
          hb_pp_tokenAdd(&pResultPtr, "|", 1, 0, HB_PP_TOKEN_PIPE | HB_PP_TOKEN_STATIC);
          fFirst = false;
        }
        do
        {
          *pResultPtr = hb_pp_tokenClone(pToken);
          if (fFirst)
          {
            (*pResultPtr)->spaces = spaces;
            fFirst = false;
          }
          pResultPtr = &(*pResultPtr)->pNext;
          pToken = pToken->pNext;
        } while ((fStop ? pToken : pToken->pNext) != pNext);
        if (!fBlock)
        {
          hb_pp_tokenAdd(&pResultPtr, "}", 1, 0, HB_PP_TOKEN_RIGHT_CB | HB_PP_TOKEN_STATIC);
        }
      }
      else if ((HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROVAR ||
                HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROTEXT) &&
               (fStop ? pToken->pNext : pToken->pNext->pNext) == pNext)
      {
        if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROVAR)
        {
          hb_pp_tokenAdd(&pResultPtr, pToken->value + 1, pToken->len - (pToken->value[pToken->len - 1] == '.' ? 2 : 1),
                         fFirst ? spaces : pToken->spaces, HB_PP_TOKEN_KEYWORD);
        }
        else
        {
          hb_membufFlush(pState->pBuffer);
          hb_pp_tokenStr(pToken, pState->pBuffer, false, false, 0);
          hb_pp_tokenAdd(&pResultPtr, hb_membufPtr(pState->pBuffer), hb_membufLen(pState->pBuffer),
                         fFirst ? spaces : pToken->spaces, HB_PP_TOKEN_STRING);
        }
        pToken = pToken->pNext;
        fFirst = false;
      }
      else if ((type == HB_PP_RMARKER_STRSMART && (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_STRING ||
                                                   HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_PB)) ||
               (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_AMPERSAND && pToken->pNext &&
                (fStop ? pToken->pNext : pToken->pNext->pNext) != pNext &&
                HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_LEFT_PB))
      {
        if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_AMPERSAND)
        {
          pToken = pToken->pNext;
        }
        do
        {
          *pResultPtr = hb_pp_tokenClone(pToken);
          if (fFirst)
          {
            (*pResultPtr)->spaces = spaces;
            fFirst = false;
          }
          pResultPtr = &(*pResultPtr)->pNext;
          pToken = pToken->pNext;
        } while ((fStop ? pToken : pToken->pNext) != pNext);
      }
      else
      {
        /* leading spaces calculation in Clipper is broken when
           separate tokens are stringified, it can be quite
           easy checked that it will interact with translation
           done just before - spaces are partially inherited.
           It means that Clipper PP does not clear some static
           buffers where holds this information.
           I decided to keep original internal spacing except the
           first token */
        auto fSpaces = false;
        if (!fFirst)
        {
          spaces = pToken->spaces;
        }
        hb_membufFlush(pState->pBuffer);
        do
        {
          hb_pp_tokenStr(pToken, pState->pBuffer, fSpaces, false, 0);
          fSpaces = true;
          pToken = pToken->pNext;
        } while ((fStop ? pToken : pToken->pNext) != pNext);
        hb_pp_tokenAdd(&pResultPtr, hb_membufPtr(pState->pBuffer), hb_membufLen(pState->pBuffer), spaces,
                       HB_PP_TOKEN_STRING);
        fFirst = false;
      }
    }
    if (fStop)
    {
      break;
    }
    /* clone comma token */
    *pResultPtr = hb_pp_tokenClone(pToken);
    if (fFirst)
    {
      (*pResultPtr)->spaces = spaces;
      fFirst = false;
    }
    pResultPtr = &(*pResultPtr)->pNext;
    pToken = pNext;
  }

  return pResultPtr;
}

static PHB_PP_TOKEN *hb_pp_matchResultAdd(PHB_PP_STATE pState, PHB_PP_RULE pRule, PHB_PP_TOKEN *pResultPtr,
                                          PHB_PP_TOKEN pMatch, HB_USHORT usMatch)
{
  PHB_PP_RESULT pMarkerResult = hb_pp_matchResultGet(pRule, usMatch, pMatch->index);
  PHB_PP_TOKEN pToken, pStop;

  if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_REGULAR)
  {
    if (pMarkerResult)
    {
      HB_BOOL fFirst = true;
      pToken = pMarkerResult->pFirstToken;
      pStop = pMarkerResult->pNextExpr;
      if (pToken != pStop)
      {
        do
        {
          *pResultPtr = hb_pp_tokenClone(pToken);
          if (fFirst)
          {
            (*pResultPtr)->spaces = pMatch->spaces;
            fFirst = false;
          }
          pResultPtr = &(*pResultPtr)->pNext;
          pToken = pToken->pNext;
        } while (pToken != pStop);
      }
    }
  }
  else if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_STRDUMP)
  {
    hb_membufFlush(pState->pBuffer);
    if (pMarkerResult)
    {
      pToken = pMarkerResult->pFirstToken;
      pStop = pMarkerResult->pNextExpr;
      if (pToken != pStop)
      {
        HB_BOOL fSpaces = false;
        do
        {
          hb_pp_tokenStr(pToken, pState->pBuffer, fSpaces, false, 0);
          fSpaces = true;
          pToken = pToken->pNext;
        } while (pToken != pStop);
      }
    }
    hb_pp_tokenAdd(&pResultPtr, hb_membufPtr(pState->pBuffer), hb_membufLen(pState->pBuffer), pMatch->spaces,
                   HB_PP_TOKEN_STRING);
  }
  else if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_STRSTD ||
           HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_STRSMART ||
           HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_BLOCK)
  {
    if (pMarkerResult)
    {
      pToken = pMarkerResult->pFirstToken;
      pStop = pMarkerResult->pNextExpr;
      /* We have to divide the expression to comma separated ones */
      if (pToken != pStop)
      {
        pResultPtr =
            hb_pp_matchResultLstAdd(pState, pMatch->spaces, HB_PP_TOKEN_TYPE(pMatch->type), pResultPtr, pToken, pStop);
      }
    }
  }
  else if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_LOGICAL)
  {
    /* Clipper documentation is wrong and Clipper PP only checks
       if such pattern was assigned not is non empty */
    hb_pp_tokenAdd(&pResultPtr, pMarkerResult ? ".T." : ".F.", 3, pMatch->spaces,
                   HB_PP_TOKEN_LOGICAL | HB_PP_TOKEN_STATIC);
  }
  else if (HB_PP_TOKEN_TYPE(pMatch->type) == HB_PP_RMARKER_NUL)
  {
    /* nothing to stuff */
  }
  else
  {
    /* TODO? internal error? */
  }

  return pResultPtr;
}

static PHB_PP_TOKEN *hb_pp_patternStuff(PHB_PP_STATE pState, PHB_PP_RULE pRule, HB_USHORT usMatch,
                                        PHB_PP_TOKEN pResultPattern, PHB_PP_TOKEN *pResultPtr)
{
  while (pResultPattern)
  {
    if (pResultPattern->index)
    {
      pResultPtr = hb_pp_matchResultAdd(pState, pRule, pResultPtr, pResultPattern, usMatch);
    }
    else if (HB_PP_TOKEN_TYPE(pResultPattern->type) == HB_PP_RMARKER_OPTIONAL)
    {
      HB_USHORT usMaxMatch = 0, matches;
      PHB_PP_TOKEN pToken = pResultPattern->pMTokens;
      while (pToken)
      {
        if (pToken->index)
        {
          matches = pRule->pMarkers[pToken->index - 1].matches;
          if (matches > usMaxMatch)
          {
            usMaxMatch = matches;
          }
        }
        pToken = pToken->pNext;
      }
      for (matches = 0; matches < usMaxMatch; ++matches)
      {
        pResultPtr = hb_pp_patternStuff(pState, pRule, matches, pResultPattern->pMTokens, pResultPtr);
      }
    }
    else if (HB_PP_TOKEN_TYPE(pResultPattern->type) == HB_PP_RMARKER_DYNVAL)
    {
      if (hb_pp_tokenValueCmp(pResultPattern, "__FILE__", HB_PP_CMP_CASE))
      {
        const char *szFileName = pState->pFile ? pState->pFile->szFileName : nullptr;
        if (!szFileName)
        {
          szFileName = "";
        }
        *pResultPtr = hb_pp_tokenNew(szFileName, strlen(szFileName), 0, HB_PP_TOKEN_STRING);
        pResultPtr = &(*pResultPtr)->pNext;
      }
      else if (hb_pp_tokenValueCmp(pResultPattern, "__LINE__", HB_PP_CMP_CASE))
      {
        char line[16];
        hb_snprintf(line, sizeof(line), "%d", pState->pFile ? pState->pFile->iCurrentLine : 0);
        *pResultPtr = hb_pp_tokenNew(line, strlen(line), 0, HB_PP_TOKEN_NUMBER);
        pResultPtr = &(*pResultPtr)->pNext;
      }
    }
    else if (HB_PP_TOKEN_TYPE(pResultPattern->type) == HB_PP_RMARKER_REFERENCE)
    {
      PHB_PP_TOKEN *pTokenPtr = pResultPtr;
      hb_pp_tokenAdd(&pResultPtr, "<@>", 3, pResultPattern->spaces, HB_PP_RMARKER_REFERENCE | HB_PP_TOKEN_STATIC);
      (*pTokenPtr)->pMTokens = pRule->pMatch;
    }
    else
    {
      *pResultPtr = hb_pp_tokenClone(pResultPattern);
      pResultPtr = &(*pResultPtr)->pNext;
    }
    pResultPattern = pResultPattern->pNext;
  }

  return pResultPtr;
}

static char *hb_pp_tokenListStr(PHB_PP_TOKEN pToken, PHB_PP_TOKEN pStop, HB_BOOL fStop, PHB_MEM_BUFFER pBuffer,
                                HB_BOOL fQuote, HB_BOOL fEol)
{
  HB_USHORT ltype = HB_PP_TOKEN_NUL;
  auto fSpaces = false;

  hb_membufFlush(pBuffer);
  while (pToken && (fStop ? pToken != pStop : !HB_PP_TOKEN_ISEOC(pToken)))
  {
    hb_pp_tokenStr(pToken, pBuffer, fSpaces, fQuote, ltype);
    ltype = HB_PP_TOKEN_TYPE(pToken->type);
    fSpaces = true;
    pToken = pToken->pNext;
  }
  if (fEol)
  {
    hb_membufAddCh(pBuffer, '\n');
  }
  hb_membufAddCh(pBuffer, '\0');

  return hb_membufPtr(pBuffer);
}

static void hb_pp_patternReplace(PHB_PP_STATE pState, PHB_PP_RULE pRule, PHB_PP_TOKEN *pTokenPtr, const char *szType)
{
  PHB_PP_TOKEN pFinalResult = nullptr, *pResultPtr, pSource;

  pResultPtr = hb_pp_patternStuff(pState, pRule, 0, pRule->pResult, &pFinalResult);

  /* store original matched token pointer */
  pSource = *pTokenPtr;

  /* Copy number of leading spaces from the first matched token
     to the first result token */
  if (pFinalResult && pSource)
  {
    pFinalResult->spaces = pSource->spaces;
  }

  /* Write trace information */
  if (pState->fWriteTrace)
  {
    fprintf(pState->file_trace, "%s(%d) >%s<\n",
            pState->pFile && pState->pFile->szFileName ? pState->pFile->szFileName : "",
            pState->pFile ? pState->pFile->iCurrentLine : 0,
            /* the source string */
            hb_pp_tokenListStr(pSource, pRule->pNextExpr, true, pState->pBuffer, true, false));
    fprintf(pState->file_trace, "#%s%s >%s<\n", pRule->mode == HB_PP_CMP_STD ? "x" : "", szType,
            /* the result string */
            hb_pp_tokenListStr(pFinalResult, *pResultPtr, true, pState->pBuffer, true, false));
  }

  /* Replace matched tokens with result pattern */
  *pResultPtr = pRule->pNextExpr;
  *pTokenPtr = pFinalResult;

  /* Free the matched tokens */
  while (pSource != pRule->pNextExpr)
  {
    PHB_PP_TOKEN pToken = pSource;
    pSource = pSource->pNext;
    hb_pp_tokenFree(pToken);
  }

  hb_pp_patternClearResults(pRule);
}

static void hb_pp_processCondDefined(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  PHB_PP_TOKEN pNext;

  while (!HB_PP_TOKEN_ISEOS(pToken))
  {
    pNext = pToken->pNext;
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD &&
        (hb_pp_tokenValueCmp(pToken, "defined", HB_PP_CMP_CASE) ||
         hb_pp_tokenValueCmp(pToken, "__pragma", HB_PP_CMP_CASE)) &&
        pNext && HB_PP_TOKEN_TYPE(pNext->type) == HB_PP_TOKEN_LEFT_PB && pNext->pNext &&
        HB_PP_TOKEN_TYPE(pNext->pNext->type) == HB_PP_TOKEN_KEYWORD && pNext->pNext->pNext &&
        HB_PP_TOKEN_TYPE(pNext->pNext->pNext->type) == HB_PP_TOKEN_RIGHT_PB)
    {
      const char *szValue = nullptr;
      char buffer[32];

      if (pToken->value[0] == '_')
      {
        const char *szSwitch;

        if (hb_pp_tokenValueCmp(pNext->pNext, "AUTOMEMVAR", HB_PP_CMP_DBASE))
        {
          szSwitch = "a";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "DEBUGINFO", HB_PP_CMP_DBASE))
        {
          szSwitch = "b";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "DYNAMICMEMVAR", HB_PP_CMP_DBASE))
        {
          szSwitch = "v";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "EXITSEVERITY", HB_PP_CMP_DBASE))
        {
          szSwitch = "es";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "LINENUMBER", HB_PP_CMP_DBASE))
        {
          szSwitch = "l";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "NOSTARTPROC", HB_PP_CMP_DBASE))
        {
          szSwitch = "n";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "PREPROCESSING", HB_PP_CMP_DBASE))
        {
          szSwitch = "p";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "SHORTCUT", HB_PP_CMP_DBASE))
        {
          szSwitch = "z";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "TEXTHIDDEN", HB_PP_CMP_DBASE))
        {
          szSwitch = "TEXTHIDDEN";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "TRACE", HB_PP_CMP_DBASE))
        {
          szSwitch = "p+";
        }
        else if (hb_pp_tokenValueCmp(pNext->pNext, "WARNINGLEVEL", HB_PP_CMP_DBASE))
        {
          szSwitch = "w";
        }
        else
        {
          szSwitch = pNext->pNext->value;
        }

        if (szSwitch != nullptr)
        {
          int iValue = 0;
          if (!hb_pp_getCompilerSwitch(pState, szSwitch, &iValue))
          {
            szValue = hb_numToStr(buffer, sizeof(buffer), iValue);
          }
        }
      }
      else
      {
        szValue = hb_pp_defineFind(pState, pNext->pNext) != nullptr ? "1" : "0";
      }

      if (szValue != nullptr)
      {
        hb_pp_tokenSetValue(pToken, szValue, strlen(szValue));
        HB_PP_TOKEN_SETTYPE(pToken, HB_PP_TOKEN_NUMBER);
        pToken->pNext = pNext->pNext->pNext->pNext;
        pNext->pNext->pNext->pNext = nullptr;
        hb_pp_tokenListFree(&pNext);
      }
    }
    pToken = pToken->pNext;
  }
}

static bool hb_pp_processDefine(PHB_PP_STATE pState, PHB_PP_TOKEN *pFirstPtr)
{
  PHB_PP_TOKEN *pPrevPtr;
  auto fSubst = false;
  auto fRepeat = false;
  int iCycle = 0;

  do
  {
    pPrevPtr = nullptr;
    fRepeat = false;
    while (!HB_PP_TOKEN_ISEOS(*pFirstPtr))
    {
      if (HB_PP_TOKEN_TYPE((*pFirstPtr)->type) == HB_PP_TOKEN_KEYWORD &&
          (pState->pMap[HB_PP_HASHID(*pFirstPtr)] & HB_PP_DEFINE))
      {
        PHB_PP_RULE pRule = hb_pp_defineFind(pState, *pFirstPtr);
        if (pRule)
        {
          if (hb_pp_patternCmp(pRule, *pFirstPtr, false))
          {
            hb_pp_patternReplace(pState, pRule, pFirstPtr, "define");
            fSubst = fRepeat = true;
            if (++pState->iCycle > pState->iMaxCycles || ++iCycle > HB_PP_MAX_REPEATS + pState->iDefinitions)
            {
              pState->iCycle = pState->iMaxCycles + 1;
              hb_pp_error(pState, 'E', HB_PP_ERR_CYCLIC_DEFINE, pRule->pMatch->value);
              return true;
            }
            continue;
          }
          if (!pPrevPtr)
          {
            pPrevPtr = pFirstPtr;
          }
        }
      }
      iCycle = 0;
      pFirstPtr = &(*pFirstPtr)->pNext;
    }
    pFirstPtr = pPrevPtr;
  } while (pFirstPtr && fRepeat);

  return fSubst;
}

static bool hb_pp_processTranslate(PHB_PP_STATE pState, PHB_PP_TOKEN *pFirstPtr)
{
  auto fSubst = false;
  auto fRepeat = false;
  int iCycle = 0;

  do
  {
    PHB_PP_TOKEN *pTokenPtr = pFirstPtr;
    fRepeat = false;
    while (!HB_PP_TOKEN_ISEOS(*pTokenPtr))
    {
      if (pState->pMap[HB_PP_HASHID(*pTokenPtr)] & HB_PP_TRANSLATE)
      {
        PHB_PP_RULE pRule = pState->pTranslations;
        while (pRule)
        {
          if (hb_pp_patternCmp(pRule, *pTokenPtr, false))
          {
            hb_pp_patternReplace(pState, pRule, pTokenPtr, "translate");
            fSubst = fRepeat = true;
            if (++pState->iCycle > pState->iMaxCycles || ++iCycle > HB_PP_MAX_REPEATS + pState->iTranslations)
            {
              pState->iCycle = pState->iMaxCycles + 1;
              hb_pp_error(pState, 'E', HB_PP_ERR_CYCLIC_TRANSLATE, pRule->pMatch->value);
              return true;
            }
            pRule = pState->pTranslations;
            continue;
          }
          pRule = pRule->pPrev;
        }
      }
      iCycle = 0;
      pTokenPtr = &(*pTokenPtr)->pNext;
    }
  } while (fRepeat);

  return fSubst;
}

static bool hb_pp_processCommand(PHB_PP_STATE pState, PHB_PP_TOKEN *pFirstPtr)
{
  PHB_PP_RULE pRule;
  auto fSubst = false;
  auto fRepeat = true;
  int iCycle = 0;

  while (fRepeat && !HB_PP_TOKEN_ISEOC(*pFirstPtr) && (pState->pMap[HB_PP_HASHID(*pFirstPtr)] & HB_PP_COMMAND))
  {
    fRepeat = false;
    pRule = pState->pCommands;
    while (pRule)
    {
      if (hb_pp_patternCmp(pRule, *pFirstPtr, true))
      {
        hb_pp_patternReplace(pState, pRule, pFirstPtr, "command");
        fSubst = fRepeat = true;
        if (++pState->iCycle > pState->iMaxCycles || ++iCycle > HB_PP_MAX_REPEATS + pState->iCommands)
        {
          pState->iCycle = pState->iMaxCycles + 1;
          hb_pp_error(pState, 'E', HB_PP_ERR_CYCLIC_COMMAND, pRule->pMatch->value);
          return true;
        }
        break;
      }
      pRule = pRule->pPrev;
    }
  }

  /* This is strictly compatible with Clipper PP which internally supports
        text <!linefunc!>,<!endfunc!>
     as stream begin directive */
  if (!HB_PP_TOKEN_ISEOC(*pFirstPtr) && hb_pp_tokenValueCmp(*pFirstPtr, "TEXT", HB_PP_CMP_DBASE))
  {
    PHB_PP_TOKEN pToken = (*pFirstPtr)->pNext, *pFuncPtr;

    if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD && pToken->pNext &&
        HB_PP_TOKEN_TYPE(pToken->pNext->type) == HB_PP_TOKEN_COMMA && pToken->pNext->pNext &&
        HB_PP_TOKEN_TYPE(pToken->pNext->pNext->type) == HB_PP_TOKEN_KEYWORD &&
        HB_PP_TOKEN_ISEOC(pToken->pNext->pNext->pNext))
    {
      hb_pp_tokenListFree(&pState->pFuncOut);
      hb_pp_tokenListFree(&pState->pFuncEnd);

      pFuncPtr = &pState->pFuncOut;
      hb_pp_tokenAdd(&pFuncPtr, pToken->value, pToken->len, 0, HB_PP_TOKEN_KEYWORD);
      hb_pp_tokenAdd(&pFuncPtr, "(", 1, 0, HB_PP_TOKEN_LEFT_PB | HB_PP_TOKEN_STATIC);
      hb_pp_tokenAdd(&pFuncPtr, "%", 1, 1, HB_PP_RMARKER_STRDUMP | HB_PP_TOKEN_STATIC);
      hb_pp_tokenAdd(&pFuncPtr, ")", 1, 1, HB_PP_TOKEN_RIGHT_PB | HB_PP_TOKEN_STATIC);

      pToken = pToken->pNext->pNext;
      pFuncPtr = &pState->pFuncEnd;
      hb_pp_tokenAdd(&pFuncPtr, pToken->value, pToken->len, 0, HB_PP_TOKEN_KEYWORD);
      hb_pp_tokenAdd(&pFuncPtr, "(", 1, 0, HB_PP_TOKEN_LEFT_PB | HB_PP_TOKEN_STATIC);
      hb_pp_tokenAdd(&pFuncPtr, ")", 1, 1, HB_PP_TOKEN_RIGHT_PB | HB_PP_TOKEN_STATIC);
      pState->iStreamDump = HB_PP_STREAM_CLIPPER;
      hb_pp_tokenListFreeCmd(pFirstPtr);
      fSubst = true;
    }
  }

  return fSubst;
}

static bool hb_pp_concatenateKeywords(PHB_PP_STATE pState, PHB_PP_TOKEN *pFirstPtr)
{
  PHB_PP_TOKEN pToken = *pFirstPtr, pNext;
  auto fChanged = false;

  while (pToken && pToken->pNext)
  {
    pNext = pToken->pNext;
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD && pNext->spaces == 0 &&
        HB_PP_TOKEN_TYPE(pNext->type) == HB_PP_TOKEN_KEYWORD)
    {
      hb_membufFlush(pState->pBuffer);
      hb_membufAddData(pState->pBuffer, pToken->value, pToken->len);
      hb_membufAddData(pState->pBuffer, pNext->value, pNext->len);

      /* Write trace information */
      if (pState->fWriteTrace)
      {
        fprintf(pState->file_trace, "%s(%d) >%s %s<\n(concatenate) >%s%s<\n",
                pState->pFile && pState->pFile->szFileName ? pState->pFile->szFileName : "",
                pState->pFile ? pState->pFile->iCurrentLine : 0, pToken->value, pNext->value, pToken->value,
                pNext->value);
      }

      hb_pp_tokenSetValue(pToken, hb_membufPtr(pState->pBuffer), hb_membufLen(pState->pBuffer));
      pToken->pNext = pNext->pNext;
      hb_pp_tokenFree(pNext);
      fChanged = true;
    }
    else
    {
      pToken = pNext;
    }
  }

  return fChanged;
}

static PHB_PP_TOKEN hb_pp_calcPrecedence(PHB_PP_TOKEN pToken, int *piNextOper, int *piNextPrec)
{
  PHB_PP_TOKEN pNext = pToken->pNext;

  *piNextOper = HB_PP_TOKEN_TYPE(pToken->type);
  switch (*piNextOper)
  {
  /* not */
  case HB_PP_TOKEN_NOT:
    *piNextPrec = HB_PP_PREC_NOT;
    break;

  case HB_PP_TOKEN_LT:
  case HB_PP_TOKEN_GT:
    if (pNext && HB_PP_TOKEN_TYPE(pNext->type) == *piNextOper && pNext->spaces == 0)
    {
      *piNextPrec = HB_PP_PREC_BIT;
      *piNextOper = *piNextOper == HB_PP_TOKEN_LT ? HB_PP_TOKEN_SHIFTL : HB_PP_TOKEN_SHIFTR;
      pNext = pNext->pNext;
      break;
    }
    /* fallthrough */
  /* relational */
  case HB_PP_TOKEN_EQUAL:
  case HB_PP_TOKEN_HASH:
  case HB_PP_TOKEN_NE:
  case HB_PP_TOKEN_LE:
  case HB_PP_TOKEN_GE:
    *piNextPrec = HB_PP_PREC_REL;
    break;

  /* logical */
  case HB_PP_TOKEN_AND:
  case HB_PP_TOKEN_OR:
    *piNextPrec = HB_PP_PREC_LOG;
    break;

  /* bit */
  case HB_PP_TOKEN_PIPE:
    if (pNext && HB_PP_TOKEN_TYPE(pNext->type) == HB_PP_TOKEN_PIPE && pNext->spaces == 0)
    {
      *piNextPrec = HB_PP_PREC_LOG;
      *piNextOper = HB_PP_TOKEN_OR;
      pNext = pNext->pNext;
    }
    else
    {
      *piNextPrec = HB_PP_PREC_BIT;
    }
    break;
  case HB_PP_TOKEN_AMPERSAND:
    /* It will not work because && will be stripped as comment */
    if (pNext && HB_PP_TOKEN_TYPE(pNext->type) == HB_PP_TOKEN_AMPERSAND && pNext->spaces == 0)
    {
      *piNextPrec = HB_PP_PREC_LOG;
      *piNextOper = HB_PP_TOKEN_AND;
      pNext = pNext->pNext;
    }
    else
    {
      *piNextPrec = HB_PP_PREC_BIT;
    }
    break;
  case HB_PP_TOKEN_POWER:
    *piNextPrec = HB_PP_PREC_BIT;
    break;

  case HB_PP_TOKEN_BITXOR:
  case HB_PP_TOKEN_SHIFTL:
  case HB_PP_TOKEN_SHIFTR:
    *piNextPrec = HB_PP_PREC_BIT;
    break;

  /* math plus/minus */
  case HB_PP_TOKEN_PLUS:
  case HB_PP_TOKEN_MINUS:
    *piNextPrec = HB_PP_PREC_PLUS;
    break;

  /* math mult/div/mode */
  case HB_PP_TOKEN_MULT:
  case HB_PP_TOKEN_DIV:
  case HB_PP_TOKEN_MOD:
    *piNextPrec = HB_PP_PREC_MULT;
    break;

  default:
    *piNextPrec = HB_PP_PREC_NUL;
    break;
  }

  return pNext;
}

static bool hb_pp_calcReduce(HB_MAXINT *plValue, int iOperation)
{
  switch (iOperation)
  {
  case HB_PP_TOKEN_AND:
    if (*plValue == 0)
    {
      return true;
    }
    break;
  case HB_PP_TOKEN_OR:
    if (*plValue)
    {
      *plValue = 1;
      return true;
    }
    break;
  }

  return false;
}

static HB_MAXINT hb_pp_calcOperation(HB_MAXINT lValueLeft, HB_MAXINT lValueRight, int iOperation, HB_BOOL *pfError)
{
  switch (iOperation)
  {
  case HB_PP_TOKEN_EQUAL:
    lValueLeft = (lValueLeft == lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_HASH:
  case HB_PP_TOKEN_NE:
    lValueLeft = (lValueLeft != lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_LE:
    lValueLeft = (lValueLeft <= lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_GE:
    lValueLeft = (lValueLeft >= lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_LT:
    lValueLeft = (lValueLeft < lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_GT:
    lValueLeft = (lValueLeft > lValueRight) ? 1 : 0;
    break;

  case HB_PP_TOKEN_AND:
    lValueLeft = (lValueLeft && lValueRight) ? 1 : 0;
    break;
  case HB_PP_TOKEN_OR:
    lValueLeft = (lValueLeft || lValueRight) ? 1 : 0;
    break;

  case HB_PP_TOKEN_PIPE:
    lValueLeft |= lValueRight;
    break;
  case HB_PP_TOKEN_AMPERSAND:
    lValueLeft &= lValueRight;
    break;
  case HB_PP_TOKEN_POWER:
  case HB_PP_TOKEN_BITXOR:
    lValueLeft ^= lValueRight;
    break;
  case HB_PP_TOKEN_SHIFTL:
    lValueLeft <<= lValueRight;
    break;
  case HB_PP_TOKEN_SHIFTR:
    lValueLeft >>= lValueRight;
    break;

  case HB_PP_TOKEN_PLUS:
    lValueLeft += lValueRight;
    break;
  case HB_PP_TOKEN_MINUS:
    lValueLeft -= lValueRight;
    break;
  case HB_PP_TOKEN_MULT:
    lValueLeft *= lValueRight;
    break;
  case HB_PP_TOKEN_DIV:
    if (lValueRight == 0)
    {
      *pfError = true;
    }
    else
    {
      lValueLeft /= lValueRight;
    }
    break;
  case HB_PP_TOKEN_MOD:
    if (lValueRight == 0)
    {
      *pfError = true;
    }
    else
    {
      lValueLeft %= lValueRight;
    }
    break;
  }

  return lValueLeft;
}

static PHB_PP_TOKEN hb_pp_calcValue(PHB_PP_TOKEN pToken, int iPrecedense, HB_MAXINT *plValue, HB_BOOL *pfError,
                                    HB_BOOL *pfUndef)
{
  if (HB_PP_TOKEN_ISEOC(pToken))
  {
    *pfError = true;
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MINUS)
  {
    pToken = hb_pp_calcValue(pToken->pNext, HB_PP_PREC_NEG, plValue, pfError, pfUndef);
    *plValue = -*plValue;
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_PLUS)
  {
    pToken = hb_pp_calcValue(pToken->pNext, iPrecedense, plValue, pfError, pfUndef);
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_NOT)
  {
    pToken = hb_pp_calcValue(pToken->pNext, HB_PP_PREC_NOT, plValue, pfError, pfUndef);
    *plValue = *plValue ? 0 : 1;
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_PB)
  {
    *pfError = true;
    pToken = hb_pp_calcValue(pToken->pNext, HB_PP_PREC_NUL, plValue, pfError, pfUndef);
    if (!*pfError && !HB_PP_TOKEN_ISEOC(pToken) && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_PB)
    {
      pToken = pToken->pNext;
    }
    else
    {
      *pfError = true;
    }
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_PB)
  {
    return pToken;
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_NUMBER)
  {
    int iOverflow;
    *plValue = hb_strValInt(pToken->value, &iOverflow);
    if (iOverflow)
    {
      *pfError = true;
    }
    else
    {
      *pfError = false;
      pToken = pToken->pNext;
    }
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LOGICAL)
  {
    *plValue = HB_PP_ISTRUE(pToken->value[1]) ? 1 : 0;
    *pfError = false;
    pToken = pToken->pNext;
  }
  else if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD)
  {
    *plValue = 0;
    pToken = pToken->pNext;
    *pfUndef = true;
    *pfError = false;
  }
  else
  {
    *pfError = true;
  }

  while (!(*pfError || HB_PP_TOKEN_ISEOC(pToken) || HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_RIGHT_PB))
  {
    int iNextOper, iNextPrec;
    PHB_PP_TOKEN pNext;
    pNext = hb_pp_calcPrecedence(pToken, &iNextOper, &iNextPrec);
    if (iNextPrec < HB_PP_PREC_LOG)
    {
      *pfError = true;
    }
    else if (iNextPrec > iPrecedense)
    {
      HB_BOOL fDefined = (!*pfUndef) && hb_pp_calcReduce(plValue, iNextOper);
      HB_MAXINT lValue = 0;
      *pfError = true;
      pToken = hb_pp_calcValue(pNext, iNextPrec, &lValue, pfError, pfUndef);
      if (!*pfError)
      {
        *plValue = hb_pp_calcOperation(*plValue, lValue, iNextOper, pfError);
      }
      if (fDefined)
      {
        *pfUndef = false;
      }
    }
    else
    {
      break;
    }
  }

  return pToken;
}

static HB_MAXINT hb_pp_calculateValue(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, HB_BOOL fNoError)
{
  HB_BOOL fError = true, fUndef = false;
  HB_MAXINT lValue = 0;

  pToken = hb_pp_calcValue(pToken, HB_PP_PREC_NUL, &lValue, &fError, &fUndef);
  if (!HB_PP_TOKEN_ISEOC(pToken) || fUndef)
  {
    fError = true;
  }

  if (fError)
  {
    if (!fNoError)
    {
      hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_IF, nullptr);
    }
    lValue = 0;
  }

  return lValue;
}

static void hb_pp_conditionPush(PHB_PP_STATE pState, HB_BOOL fCond)
{
  if (pState->iCondCount == pState->iCondStackSize)
  {
    pState->iCondStackSize += 5;
    if (pState->pCondStack)
    {
      pState->pCondStack =
          static_cast<int *>(hb_xrealloc(pState->pCondStack, pState->iCondStackSize * sizeof(HB_BOOL)));
    }
    else
    {
      pState->pCondStack = static_cast<int *>(hb_xgrab(pState->iCondStackSize * sizeof(HB_BOOL)));
    }
  }
  pState->pCondStack[pState->iCondCount++] = pState->iCondCompile;
  pState->iCondCompile = pState->iCondCompile ? HB_PP_COND_DISABLE : (fCond ? 0 : HB_PP_COND_ELSE);
}

static void hb_pp_condCompile(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, HB_BOOL fNot)
{
  if (!pToken || HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_KEYWORD || !HB_PP_TOKEN_ISEOC(pToken->pNext))
  {
    hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_IFDEF, nullptr);
  }
  else
  {
    auto fCond = false;

    if (pState->iCondCompile == 0)
    {
      fCond = hb_pp_defineFind(pState, pToken) != nullptr;
      if (!fNot)
      {
        fCond = !fCond;
      }
    }
    hb_pp_conditionPush(pState, fCond);
  }
}

static void hb_pp_condCompileIf(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  /* preprocess all define(s) */
  hb_pp_processCondDefined(pState, pToken->pNext);
  hb_pp_processDefine(pState, &pToken->pNext);
  hb_pp_conditionPush(pState, hb_pp_calculateValue(pState, pToken->pNext, pState->iCondCompile != 0) != 0);
}

static void hb_pp_condCompileElif(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  if ((pState->iCondCompile & HB_PP_COND_DISABLE) == 0)
  {
    if (pState->iCondCompile)
    {
      /* preprocess all define(s) */
      hb_pp_processCondDefined(pState, pToken->pNext);
      hb_pp_processDefine(pState, &pToken->pNext);
      if (hb_pp_calculateValue(pState, pToken->pNext, false) != 0)
      {
        pState->iCondCompile ^= HB_PP_COND_ELSE;
      }
    }
    else
    {
      pState->iCondCompile = HB_PP_COND_DISABLE;
    }
  }
}

static void hb_pp_lineTokens(PHB_PP_TOKEN **pTokenPtr, const char *szFileName, int iLine)
{
  char szLine[12];

  hb_snprintf(szLine, sizeof(szLine), "%d", iLine);
  hb_pp_tokenAdd(pTokenPtr, "#", 1, 0, HB_PP_TOKEN_DIRECTIVE | HB_PP_TOKEN_STATIC);
  hb_pp_tokenAdd(pTokenPtr, "line", 4, 0, HB_PP_TOKEN_KEYWORD | HB_PP_TOKEN_STATIC);
  hb_pp_tokenAdd(pTokenPtr, szLine, strlen(szLine), 1, HB_PP_TOKEN_NUMBER);
  if (szFileName != nullptr)
  {
    hb_pp_tokenAdd(pTokenPtr, szFileName, strlen(szFileName), 1, HB_PP_TOKEN_STRING);
  }
  hb_pp_tokenAdd(pTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
}

static void hb_pp_genLineTokens(PHB_PP_STATE pState)
{
  pState->pNextTokenPtr = &pState->pTokenOut;

  if (pState->pFile->fGenLineInfo)
  {
    hb_pp_lineTokens(&pState->pNextTokenPtr, pState->pFile->szFileName, pState->pFile->iCurrentLine);
    pState->pFile->iLastLine = pState->pFile->iCurrentLine;
    pState->pFile->fGenLineInfo = false;
  }
  else if (pState->pFile->iLastLine < pState->pFile->iCurrentLine)
  {
    do
    {
      hb_pp_tokenAdd(&pState->pNextTokenPtr, "\n", 1, 0, HB_PP_TOKEN_EOL | HB_PP_TOKEN_STATIC);
    } while (++pState->pFile->iLastLine < pState->pFile->iCurrentLine);
  }
  hb_pp_tokenMoveCommand(pState, pState->pNextTokenPtr, &pState->pFile->pTokenList);
}

static void hb_pp_includeFile(PHB_PP_STATE pState, const char *szFileName, HB_BOOL fSysFile)
{
  if (pState->iFiles >= HB_PP_MAX_INCLUDED_FILES)
  {
    hb_pp_error(pState, 'F', HB_PP_ERR_NESTED_INCLUDES, nullptr);
  }
  else
  {
    HB_BOOL fNested = false;
    PHB_PP_FILE pFile = hb_pp_FileNew(pState, szFileName, fSysFile, &fNested, nullptr, true, pState->pOpenFunc, false);
    if (pFile)
    {
#if defined(HB_PP_STRICT_LINEINFO_TOKEN)
      pState->pNextTokenPtr = &pState->pTokenOut;
      if (pState->pFile->fGenLineInfo)
      {
        hb_pp_lineTokens(&pState->pNextTokenPtr, pState->pFile->szFileName, pState->pFile->iCurrentLine);
        pState->pFile->iLastLine = pState->pFile->iCurrentLine;
        pState->pFile->fGenLineInfo = false;
      }
      hb_pp_lineTokens(&pState->pNextTokenPtr, szFileName, 1);
#else
      pFile->fGenLineInfo = true;
#endif
      pFile->pPrev = pState->pFile;
      pState->pFile = pFile;
      pState->iFiles++;
    }
    else if (fNested)
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_NESTED_INCLUDES, nullptr);
    }
    else
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_OPEN_FILE, szFileName);
    }
  }
}

static void hb_pp_includeClose(PHB_PP_STATE pState)
{
  PHB_PP_FILE pFile = pState->pFile;

  pState->pFile = pFile->pPrev;
  pState->iFiles--;

#if defined(HB_PP_STRICT_LINEINFO_TOKEN)
  if (pFile->fGenLineInfo)
  {
    pState->pNextTokenPtr = &pState->pTokenOut;
    hb_pp_lineTokens(&pState->pNextTokenPtr, pFile->szFileName, pFile->iCurrentLine + 1);
  }
#endif
  if (pState->pFile)
  {
    pState->pFile->fGenLineInfo = true;
  }

  hb_pp_FileFree(pState, pFile, pState->pCloseFunc);
}

static void hb_pp_preprocessToken(PHB_PP_STATE pState)
{
  while (!pState->pTokenOut && pState->pFile)
  {
    if (!pState->pFile->pTokenList)
    {
      while (pState->pFile->pLineBuf ? pState->pFile->nLineBufLen != 0 : !pState->pFile->fEof)
      {
        hb_pp_getLine(pState);
        if (pState->pFile->pTokenList /* || pState->fError */)
        {
          break;
        }
      }

      if (!pState->pFile->pTokenList)
      {
#if 0 /* disabled for files included from buffer */
            if( pState->pFile->pLineBuf ) {
               break;
            }
#endif
        /* this condition is only for compiler core code compatibility */
        if (!pState->pFile->pPrev)
        {
          break;
        }
        hb_pp_includeClose(pState);
        continue;
      }
    }

    if (HB_PP_TOKEN_ISDIRECTIVE(pState->pFile->pTokenList))
    {
      auto fError = false;
      auto fDirect = false;
      /* Store it here to avoid possible problems after #INCLUDE */
      PHB_PP_TOKEN *pFreePtr = &pState->pFile->pTokenList;
      PHB_PP_TOKEN pToken = *pFreePtr;

      fDirect = HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_DIRECTIVE;

      pToken = pToken->pNext;
      if (!pToken)
      {
        fError = true;
      }
#ifndef HB_CLP_STRICT
      /* Harbour PP extension */
      else if (fDirect && pState->pFile->iCurrentLine == 1 && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_NOT &&
               pToken->spaces == 0 && pState->pFile->pTokenList->spaces == 0)
      {
        /* ignore first line if it begins with "#!"
           minor extension which allow to use the same source code
           as scripts in *nix system and compile it, this feature
           will be necessary also when we integrate compiler with HVM and
           add support for direct execution compiled .prg files */
      }
#endif
      else if (HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_KEYWORD)
      {
        fError = true;
      }
      else if (hb_pp_tokenValueCmp(pToken, "IFDEF", HB_PP_CMP_DBASE))
      {
        hb_pp_condCompile(pState, pToken->pNext, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "IFNDEF", HB_PP_CMP_DBASE))
      {
        hb_pp_condCompile(pState, pToken->pNext, false);
      }
#ifndef HB_CLP_STRICT
      /* Harbour PP extension */
      else if (hb_pp_tokenValueCmp(pToken, "IF", HB_PP_CMP_DBASE))
      {
        hb_pp_condCompileIf(pState, pToken);
      }
      else if (hb_pp_tokenValueCmp(pToken, "ELIF", HB_PP_CMP_DBASE))
      {
        if (pState->iCondCount)
        {
          hb_pp_condCompileElif(pState, pToken);
        }
        else
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_ELSE, nullptr);
        }
      }
#endif
      else if (hb_pp_tokenValueCmp(pToken, "ENDIF", HB_PP_CMP_DBASE))
      {
        if (pState->iCondCount)
        {
          pState->iCondCompile = pState->pCondStack[--pState->iCondCount];
        }
        else
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_ENDIF, nullptr);
        }
      }
      else if (hb_pp_tokenValueCmp(pToken, "ELSE", HB_PP_CMP_DBASE))
      {
        if (pState->iCondCount)
        {
          pState->iCondCompile ^= HB_PP_COND_ELSE;
        }
        else
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_ELSE, nullptr);
        }
      }
      /* #pragma support is always enabled even in strict compatibility
         mode to allow control by programmer some PP issues */
      else if (hb_pp_tokenValueCmp(pToken, "PRAGMA", HB_PP_CMP_DBASE))
      {
        hb_pp_pragmaNew(pState, pToken->pNext);
      }
      else if (pState->iCondCompile)
      {
        /* conditional compilation - other preprocessing and output disabled */
      }
      else if (hb_pp_tokenValueCmp(pToken, "INCLUDE", HB_PP_CMP_DBASE))
      {
        pToken = pToken->pNext;
        if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_STRING)
        {
          hb_pp_includeFile(pState, pToken->value, false);
        }
        else if (pToken && HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LT)
        {
          pToken = pToken->pNext;
          hb_membufFlush(pState->pBuffer);
          while (!HB_PP_TOKEN_ISEOC(pToken) && HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_GT)
          {
            hb_membufAddData(pState->pBuffer, pToken->value, pToken->len);
            pToken = pToken->pNext;
          }
          if (hb_membufLen(pState->pBuffer) > 0 && !HB_PP_TOKEN_ISEOC(pToken) &&
              HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_GT)
          {
            hb_membufAddCh(pState->pBuffer, '\0');
            hb_pp_includeFile(pState, hb_membufPtr(pState->pBuffer), true);
          }
          else
          {
            hb_pp_error(pState, 'F', HB_PP_ERR_WRONG_FILE_NAME, nullptr);
          }
        }
        else
        {
          hb_pp_error(pState, 'F', HB_PP_ERR_WRONG_FILE_NAME, nullptr);
        }
      }
      else if (hb_pp_tokenValueCmp(pToken, "REQUIRE", HB_PP_CMP_STD))
      {
        /* do nothing. this directive is processed by hbmk2 to
           pull in external modules. */
      }
      else if (hb_pp_tokenValueCmp(pToken, "STDOUT", HB_PP_CMP_DBASE))
      {
        hb_pp_disp(pState, hb_pp_tokenListStr(pToken->pNext, nullptr, false, pState->pBuffer, false, true));
      }
      else if (hb_pp_tokenValueCmp(pToken, "ERROR", HB_PP_CMP_DBASE))
      {
        hb_pp_error(pState, 'E', HB_PP_ERR_EXPLICIT,
                    hb_pp_tokenListStr(pToken->pNext, nullptr, false, pState->pBuffer, false, false));
      }
      else if (hb_pp_tokenValueCmp(pToken, "WARNING", HB_PP_CMP_DBASE))
      {
        hb_pp_error(pState, 'W', HB_PP_WARN_EXPLICIT,
                    hb_pp_tokenListStr(pToken->pNext, nullptr, false, pState->pBuffer, false, false));
      }
      else if (hb_pp_tokenValueCmp(pToken, "DEFINE", HB_PP_CMP_DBASE))
      {
        hb_pp_defineNew(pState, pToken, fDirect);
      }
      else if (hb_pp_tokenValueCmp(pToken, "UNDEF", HB_PP_CMP_DBASE))
      {
        pToken = pToken->pNext;
        if (!pToken || HB_PP_TOKEN_TYPE(pToken->type) != HB_PP_TOKEN_KEYWORD || !HB_PP_TOKEN_ISEOC(pToken->pNext))
        {
          hb_pp_error(pState, 'E', HB_PP_ERR_DIRECTIVE_UNDEF, nullptr);
        }
        else
        {
          hb_pp_defineDel(pState, pToken);
        }
      }
      else if (hb_pp_tokenValueCmp(pToken, "TRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_DBASE, false, fDirect, false);
      }
      else if (hb_pp_tokenValueCmp(pToken, "XTRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_STD, false, fDirect, false);
      }
#ifndef HB_CLP_STRICT
      else if (hb_pp_tokenValueCmp(pToken, "YTRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_CASE, false, fDirect, false);
      }
#endif
      else if (hb_pp_tokenValueCmp(pToken, "COMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_DBASE, true, fDirect, false);
      }
      else if (hb_pp_tokenValueCmp(pToken, "XCOMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_STD, true, fDirect, false);
      }
#ifndef HB_CLP_STRICT
      else if (hb_pp_tokenValueCmp(pToken, "YCOMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_CASE, true, fDirect, false);
        /* Harbour PP extensions */
      }
      else if (hb_pp_tokenValueCmp(pToken, "UNTRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_DBASE, false, fDirect, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "XUNTRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_STD, false, fDirect, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "YUNTRANSLATE", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_CASE, false, fDirect, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "UNCOMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_DBASE, true, fDirect, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "XUNCOMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_STD, true, fDirect, true);
      }
      else if (hb_pp_tokenValueCmp(pToken, "YUNCOMMAND", HB_PP_CMP_DBASE))
      {
        hb_pp_directiveNew(pState, pToken, HB_PP_CMP_CASE, true, fDirect, true);
        /* Clipper PP does not accept #line and generates error */
      }
      else if (hb_pp_tokenValueCmp(pToken, "LINE", HB_PP_CMP_DBASE))
      {
        /* ignore #line directives */
      }
#endif
      else
      {
        fError = true;
      }

      if (fError)
      {
        hb_pp_error(pState, 'F', HB_PP_ERR_INVALID_DIRECTIVE, nullptr);
      }
      pState->pFile->iCurrentLine += hb_pp_tokenListFreeCmd(pFreePtr);
      continue;
    }
    else if (pState->iCondCompile)
    {
      pState->pFile->iCurrentLine += hb_pp_tokenListFreeCmd(&pState->pFile->pTokenList);
    }
    else
    {
      auto fDirective = false;

      pState->iCycle = 0;
      while (!HB_PP_TOKEN_ISEOC(pState->pFile->pTokenList) && pState->iCycle <= pState->iMaxCycles)
      {
        if (HB_PP_TOKEN_ISDIRECTIVE(pState->pFile->pTokenList))
        {
          fDirective = true;
          break;
        }
#ifndef HB_CLP_STRICT
        /* Harbour extension: concatenate keywords without spaces between
           them */
        hb_pp_concatenateKeywords(pState, &pState->pFile->pTokenList);
#endif
        if (hb_pp_processDefine(pState, &pState->pFile->pTokenList))
        {
          continue;
        }
        if (hb_pp_processTranslate(pState, &pState->pFile->pTokenList))
        {
          continue;
        }
        if (hb_pp_processCommand(pState, &pState->pFile->pTokenList))
        {
          continue;
        }
        break;
      }
      if (!fDirective && pState->pFile->pTokenList)
      {
        hb_pp_genLineTokens(pState);
      }
    }
  }
}

/*
 * exported functions
 */

/*
 * internal function to initialize predefined PP rules
 */
void hb_pp_initRules(PHB_PP_RULE *pRulesPtr, int *piRules, const HB_PP_DEFRULE pDefRules[], int iDefRules)
{
  PHB_PP_MARKER pMarkers;
  PHB_PP_RULE pRule;

  hb_pp_ruleListFree(pRulesPtr);
  *piRules = iDefRules;

  while (--iDefRules >= 0)
  {
    const HB_PP_DEFRULE *pDefRule = pDefRules + iDefRules;
    if (pDefRule->markers > 0)
    {
      HB_USHORT marker;
      HB_ULONG ulBit;

      pMarkers = static_cast<PHB_PP_MARKER>(hb_xgrabz(pDefRule->markers * sizeof(HB_PP_MARKER)));
      for (marker = 0, ulBit = 1; marker < pDefRule->markers; ++marker, ulBit <<= 1)
      {
        if (pDefRule->repeatbits & ulBit)
        {
          pMarkers[marker].canrepeat = true;
        }
      }
    }
    else
    {
      pMarkers = nullptr;
    }
    pRule = hb_pp_ruleNew(pDefRule->pMatch, pDefRule->pResult, pDefRule->mode, pDefRule->markers, pMarkers);
    pRule->pPrev = *pRulesPtr;
    *pRulesPtr = pRule;
  }
}

/*
 * get preprocessed token
 */
PHB_PP_TOKEN hb_pp_tokenGet(PHB_PP_STATE pState)
{
  pState->fError = false;

  if (pState->pTokenOut)
  {
    PHB_PP_TOKEN pToken = pState->pTokenOut;
    pState->pTokenOut = pToken->pNext;
    hb_pp_tokenFree(pToken);
  }

  for (;;)
  {
    if (!pState->pTokenOut)
    {
      hb_pp_preprocessToken(pState);
      if (!pState->pTokenOut)
      {
        break;
      }
    }
    while (pState->pTokenOut && HB_PP_TOKEN_TYPE(pState->pTokenOut->type) == HB_PP_RMARKER_REFERENCE)
    {
      PHB_PP_TOKEN pToken = pState->pTokenOut;
      pState->pTokenOut = pToken->pNext;
      hb_pp_tokenFree(pToken);
    }
    if (pState->pTokenOut)
    {
      break;
    }
  }

  if (pState->fWritePreprocesed && pState->pTokenOut)
  {
    hb_membufFlush(pState->pBuffer);
    hb_pp_tokenStr(pState->pTokenOut, pState->pBuffer, true, true, pState->usLastType);
    pState->usLastType = HB_PP_TOKEN_TYPE(pState->pTokenOut->type);
    if (fwrite(hb_membufPtr(pState->pBuffer), sizeof(char), hb_membufLen(pState->pBuffer), pState->file_out) !=
        hb_membufLen(pState->pBuffer))
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_WRITE_FILE, pState->szOutFileName);
    }
  }

  return pState->pTokenOut;
}

/*
 * create new PP context
 */
PHB_PP_STATE hb_pp_new(void)
{
  return hb_pp_stateNew();
}

/*
 * free PP context
 */
void hb_pp_free(PHB_PP_STATE pState)
{
  hb_pp_stateFree(pState);
}

/*
 * initialize PP context
 */
void hb_pp_init(PHB_PP_STATE pState, HB_BOOL fQuiet, HB_BOOL fGauge, int iCycles, void *cargo,
                PHB_PP_OPEN_FUNC pOpenFunc, PHB_PP_CLOSE_FUNC pCloseFunc, PHB_PP_ERROR_FUNC pErrorFunc,
                PHB_PP_DISP_FUNC pDispFunc, PHB_PP_DUMP_FUNC pDumpFunc, PHB_PP_INLINE_FUNC pInLineFunc,
                PHB_PP_SWITCH_FUNC pSwitchFunc)
{
  pState->fQuiet = pState->fQuietSet = fQuiet;
  pState->fGauge = fGauge;
  pState->iMaxCycles = pState->iMaxCyclesSet = (iCycles > 0) ? iCycles : HB_PP_MAX_CYCLES;
  pState->cargo = cargo;
  pState->pOpenFunc = pOpenFunc;
  pState->pCloseFunc = pCloseFunc;
  pState->pErrorFunc = pErrorFunc;
  pState->pDispFunc = pDispFunc;
  pState->pDumpFunc = pDumpFunc;
  pState->pInLineFunc = pInLineFunc;
  pState->pSwitchFunc = pSwitchFunc;
}

void hb_pp_setIncFunc(PHB_PP_STATE pState, PHB_PP_INC_FUNC pIncFunc)
{
  pState->pIncFunc = pIncFunc;
}

/*
 * reset PP context, used for multiple .prg file compilation
 * with DO ... or *.clp files
 */
void hb_pp_reset(PHB_PP_STATE pState)
{
  pState->fError = false;
  pState->iErrors = 0;
  pState->iLineTot = 0;
  pState->fEscStr = false;
  pState->fMultiLineStr = false;
  pState->fTracePragmas = false;
  pState->fQuiet = pState->fQuietSet;
  pState->iMaxCycles = pState->iMaxCyclesSet;
  pState->iCondCompile = 0;
  pState->iCondCount = 0;
  pState->iStreamDump = HB_PP_STREAM_OFF;

  hb_pp_tokenListFree(&pState->pFuncOut);
  hb_pp_tokenListFree(&pState->pFuncEnd);

  hb_pp_InFileFree(pState);
  hb_pp_OutFileFree(pState);
  hb_pp_TraceFileFree(pState);

  if (pState->iOperators > 0)
  {
    hb_pp_operatorsFree(pState->pOperators, pState->iOperators);
    pState->pOperators = nullptr;
    pState->iOperators = 0;
  }

  hb_pp_ruleListNonStdFree(&pState->pDefinitions);
  hb_pp_ruleListNonStdFree(&pState->pTranslations);
  hb_pp_ruleListNonStdFree(&pState->pCommands);
}

/*
 * add search path for included files
 */
void hb_pp_addSearchPath(PHB_PP_STATE pState, const char *szPath, HB_BOOL fReplace)
{
  if (fReplace && pState->pIncludePath)
  {
    hb_fsFreeSearchPath(pState->pIncludePath);
    pState->pIncludePath = nullptr;
  }

  if (szPath != nullptr && *szPath)
  {
    hb_fsAddSearchPath(szPath, &pState->pIncludePath);
  }
}

/*
 * mark current rules as standard ones
 */
void hb_pp_setStdBase(PHB_PP_STATE pState)
{
  pState->fError = false;
  hb_pp_ruleListSetStd(pState->pDefinitions);
  hb_pp_ruleListSetStd(pState->pTranslations);
  hb_pp_ruleListSetStd(pState->pCommands);
  memset(pState->pMap, 0, sizeof(pState->pMap));
  hb_pp_ruleListSetId(pState, pState->pDefinitions, HB_PP_DEFINE);
  hb_pp_ruleListSetId(pState, pState->pTranslations, HB_PP_TRANSLATE);
  hb_pp_ruleListSetId(pState, pState->pCommands, HB_PP_COMMAND);

  /* clear total number of preprocessed lines so we will report only
   * lines in compiled .prg files
   */
  pState->iLineTot = 0;
}

/*
 * initialize dynamic definitions
 */
void hb_pp_initDynDefines(PHB_PP_STATE pState, HB_BOOL fArchDefs)
{
  char szResult[65];
  int iYear, iMonth, iDay;
  long lDate, lTime;

  if (fArchDefs)
  {
    static const char *s_szPlatform = "__PLATFORM__%s";

    char szDefine[65];

    if (hb_verPlatformMacro())
    {
      hb_snprintf(szDefine, sizeof(szDefine), s_szPlatform, hb_verPlatformMacro());
      hb_pp_addDefine(pState, szDefine, nullptr);
    }
#if defined(HB_OS_UNIX)
    hb_snprintf(szDefine, sizeof(szDefine), s_szPlatform, "UNIX");
    hb_pp_addDefine(pState, szDefine, nullptr);
#endif

    hb_snprintf(szResult, sizeof(szResult), "%d", static_cast<int>(sizeof(void *)));
#if defined(HB_ARCH_16BIT)
    hb_pp_addDefine(pState, "__ARCH16BIT__", szResult);
#elif defined(HB_ARCH_32BIT)
    hb_pp_addDefine(pState, "__ARCH32BIT__", szResult);
#elif defined(HB_ARCH_64BIT)
    hb_pp_addDefine(pState, "__ARCH64BIT__", szResult);
#endif

#if defined(HB_LITTLE_ENDIAN)
    hb_pp_addDefine(pState, "__LITTLE_ENDIAN__", szResult);
#elif defined(HB_BIG_ENDIAN)
    hb_pp_addDefine(pState, "__BIG_ENDIAN__", szResult);
#elif defined(HB_PDP_ENDIAN)
    hb_pp_addDefine(pState, "__PDP_ENDIAN__", szResult);
#endif
  }

#if defined(__HARBOUR__)
  hb_snprintf(szResult, sizeof(szResult), "0x%02X%02X%02X", HB_VER_MAJOR & 0xFF, HB_VER_MINOR & 0xFF,
              HB_VER_RELEASE & 0xFF);
  hb_pp_addDefine(pState, "__HARBOUR__", szResult);
#endif

  /* __DATE__ */
  hb_dateToday(&iYear, &iMonth, &iDay);
  hb_dateStrPut(szResult + 1, iYear, iMonth, iDay);
  szResult[0] = '"';
  szResult[9] = '"';
  szResult[10] = '\0';
  hb_pp_addDefine(pState, "__DATE__", szResult);

  /* __TIME__ */
  hb_dateTimeStr(szResult + 1);
  szResult[0] = '"';
  szResult[9] = '"';
  szResult[10] = '\0';
  hb_pp_addDefine(pState, "__TIME__", szResult);

  /* __TIMESTAMP__ */
  szResult[0] = 't';
  szResult[1] = '"';
  hb_timeStampGet(&lDate, &lTime);
  hb_timeStampStr(szResult + 2, lDate, lTime);
  auto i = static_cast<int>(strlen(szResult));
  szResult[i++] = '"';
  szResult[i] = '\0';
  hb_pp_addDefine(pState, "__TIMESTAMP__", szResult);

  hb_pp_addDefine(pState, "__FILE__", &s_pp_dynamicResult);
  hb_pp_addDefine(pState, "__LINE__", &s_pp_dynamicResult);

#ifdef HB_START_PROCEDURE
  hb_pp_addDefine(pState, "__HB_MAIN__", HB_START_PROCEDURE);
#endif
}

/*
 * read preprocess rules from file
 */
void hb_pp_readRules(PHB_PP_STATE pState, const char *szRulesFile)
{
  char szFileName[HB_PATH_MAX];
  PHB_PP_FILE pFile = pState->pFile;
  PHB_FNAME pFileName;

  pFileName = hb_fsFNameSplit(szRulesFile);
  if (!pFileName->szExtension)
  {
    pFileName->szExtension = ".ch";
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  pState->pFile = hb_pp_FileNew(pState, szFileName, false, nullptr, nullptr, true, pState->pOpenFunc, false);
  if (!pState->pFile)
  {
    pState->pFile = pFile;
    hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_OPEN_RULES, szFileName);
  }
  else
  {
    auto fError = false;

    pState->iFiles++;
    pState->usLastType = HB_PP_TOKEN_NUL;
    while (hb_pp_tokenGet(pState))
    {
      if (pState->fError)
      {
        fError = true;
      }
    }
    if (pState->pFile)
    {
      hb_pp_FileFree(pState, pState->pFile, pState->pCloseFunc);
      pState->iFiles--;
    }
    pState->pFile = pFile;
    if (fError)
    {
      pState->fError = true;
    }
  }
}

/*
 * close all open input files and set the given buffer as input stream
 */
HB_BOOL hb_pp_inBuffer(PHB_PP_STATE pState, const char *szFileName, const char *pBuffer, HB_SIZE nLen, int iStartLine)
{
  hb_pp_InFileFree(pState);

  pState->fError = false;

  pState->pFile = hb_pp_FileBufNew(pBuffer, nLen);
  if (szFileName != nullptr)
  {
    pState->pFile->szFileName = hb_strdup(szFileName);
  }
  pState->pFile->iCurrentLine = iStartLine;
  pState->pFile->iLastLine = iStartLine + 1;
  pState->iFiles++;
  return true;
}

/*
 * close all open input files and set the given one as new
 */
HB_BOOL hb_pp_inFile(PHB_PP_STATE pState, const char *szFileName, HB_BOOL fSearchPath, FILE *file_in, HB_BOOL fError)
{
  hb_pp_InFileFree(pState);

  pState->fError = false;

  pState->pFile = hb_pp_FileNew(pState, szFileName, false, nullptr, file_in, fSearchPath, nullptr, false);
  if (pState->pFile)
  {
    pState->iFiles++;
    return true;
  }
  if (fError)
  {
    hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_OPEN_INPUT, szFileName);
  }
  return false;
}

/*
 * set output (.ppo) file
 */
HB_BOOL hb_pp_outFile(PHB_PP_STATE pState, const char *szOutFileName, FILE *file_out)
{
  pState->fError = false;
  hb_pp_OutFileFree(pState);

  if (szOutFileName != nullptr)
  {

    if (file_out)
    {
      pState->file_out = file_out;
    }
    else
    {
      pState->file_out = hb_fopen(szOutFileName, "w");
    }

    if (pState->file_out)
    {
      pState->szOutFileName = hb_strdup(szOutFileName);
      pState->fWritePreprocesed = true;
    }
    else
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_CREATE_FILE, szOutFileName);
    }
  }
  return !pState->fError;
}

/*
 * set trace (.ppt) file
 */
HB_BOOL hb_pp_traceFile(PHB_PP_STATE pState, const char *szTraceFileName, FILE *file_trace)
{
  pState->fError = false;
  hb_pp_TraceFileFree(pState);

  if (szTraceFileName != nullptr)
  {

    if (file_trace)
    {
      pState->file_trace = file_trace;
    }
    else
    {
      pState->file_trace = hb_fopen(szTraceFileName, "w");
    }

    if (pState->file_trace)
    {
      pState->szTraceFileName = hb_strdup(szTraceFileName);
      pState->fWriteTrace = true;
    }
    else
    {
      hb_pp_error(pState, 'F', HB_PP_ERR_CANNOT_CREATE_FILE, szTraceFileName);
    }
  }
  return !pState->fError;
}

/*
 * check error status of last PP operation
 */
HB_BOOL hb_pp_lasterror(PHB_PP_STATE pState)
{
  return pState->fError;
}

/*
 * retrieve number of errors which appeared during preprocessing
 */
int hb_pp_errorCount(PHB_PP_STATE pState)
{
  return pState->iErrors;
}

/*
 * return currently preprocessed file name
 */
char *hb_pp_fileName(PHB_PP_STATE pState)
{
  if (pState->pFile)
  {
    return pState->pFile->szFileName;
  }
  else
  {
    return nullptr;
  }
}

/*
 * return currently preprocessed line number
 */
int hb_pp_line(PHB_PP_STATE pState)
{
  if (pState->pFile)
  {
    return pState->pFile->iCurrentLine;
  }
  else
  {
    return 0;
  }
}

int hb_pp_lineTot(PHB_PP_STATE pState)
{
  return pState->iLineTot;
}

/*
 * return output file name (.ppo)
 */
char *hb_pp_outFileName(PHB_PP_STATE pState)
{
  return pState->szOutFileName;
}

/*
 * return trace output file name (.ppt)
 */
char *hb_pp_traceFileName(PHB_PP_STATE pState)
{
  return pState->szTraceFileName;
}

/*
 * return if EOF was reached
 */
HB_BOOL hb_pp_eof(PHB_PP_STATE pState)
{
  return pState->pFile->fEof;
}

/*
 * add new define value
 */
void hb_pp_addDefine(PHB_PP_STATE pState, const char *szDefName, const char *szDefValue)
{
  PHB_PP_TOKEN pMatch, pResult, pToken;
  PHB_PP_FILE pFile;

  pState->fError = false;

  pFile = hb_pp_FileBufNew(szDefName, strlen(szDefName));
  pFile->pPrev = pState->pFile;
  pState->pFile = pFile;
  pState->iFiles++;
  hb_pp_getLine(pState);
  pMatch = pState->pFile->pTokenList;
  pState->pFile->pTokenList = nullptr;
  pToken = hb_pp_tokenResultEnd(&pMatch, true);
  hb_pp_tokenListFree(&pToken);

  if (szDefValue != nullptr && !pState->fError)
  {
    if (szDefValue == &s_pp_dynamicResult)
    {
      pResult = hb_pp_tokenNew(szDefName, strlen(szDefName), 0, HB_PP_RMARKER_DYNVAL | HB_PP_TOKEN_STATIC);
    }
    else
    {
      pFile->pLineBuf = szDefValue;
      pFile->nLineBufLen = strlen(szDefValue);
      hb_pp_getLine(pState);
      pResult = pState->pFile->pTokenList;
      pState->pFile->pTokenList = nullptr;
      pToken = hb_pp_tokenResultEnd(&pResult, true);
      hb_pp_tokenListFree(&pToken);
    }
  }
  else
  {
    pResult = nullptr;
  }

  if (pState->fError || !pMatch)
  {
    hb_pp_tokenListFree(&pMatch);
    hb_pp_tokenListFree(&pResult);
  }
  else
  {
    hb_pp_defineAdd(pState, HB_PP_CMP_CASE, 0, nullptr, pMatch, pResult);
  }
  pState->pFile = pFile->pPrev;
  hb_pp_FileFree(pState, pFile, nullptr);
  pState->iFiles--;
}

/*
 * delete define value
 */
void hb_pp_delDefine(PHB_PP_STATE pState, const char *szDefName)
{
  PHB_PP_TOKEN pToken;

  pToken = hb_pp_tokenNew(szDefName, strlen(szDefName), 0, HB_PP_TOKEN_KEYWORD);
  hb_pp_defineDel(pState, pToken);
  hb_pp_tokenFree(pToken);
}

/*
 * set stream mode
 */
void hb_pp_setStream(PHB_PP_STATE pState, int iMode)
{
  pState->fError = false;
  switch (iMode)
  {
  case HB_PP_STREAM_DUMP_C:
    pState->iDumpLine = pState->pFile ? pState->pFile->iCurrentLine : 0;
    if (!pState->pDumpBuffer)
    {
      pState->pDumpBuffer = hb_membufNew();
    }
    pState->iStreamDump = iMode;
    break;

  case HB_PP_STREAM_INLINE_C:
    pState->iDumpLine = pState->pFile ? pState->pFile->iCurrentLine : 0;
    /* fallthrough */
  case HB_PP_STREAM_CLIPPER:
  case HB_PP_STREAM_PRG:
  case HB_PP_STREAM_C:
    if (!pState->pStreamBuffer)
    {
      pState->pStreamBuffer = hb_membufNew();
    }
    /* fallthrough */
  case HB_PP_STREAM_OFF:
  case HB_PP_STREAM_COMMENT:
    pState->iStreamDump = iMode;
    break;

  default:
    pState->fError = true;
  }
}

/*
 * return next preprocessed line
 */
char *hb_pp_nextLine(PHB_PP_STATE pState, HB_SIZE *pnLen)
{
  if (pState->pFile)
  {
    PHB_PP_TOKEN pToken;
    auto fError = false;
    HB_USHORT ltype;

    if (!pState->pOutputBuffer)
    {
      pState->pOutputBuffer = hb_membufNew();
    }
    else
    {
      hb_membufFlush(pState->pOutputBuffer);
    }

    pState->usLastType = ltype = HB_PP_TOKEN_NUL;
    while ((pToken = hb_pp_tokenGet(pState)) != nullptr)
    {
      if (pState->fError)
      {
        fError = true;
      }
      if (hb_pp_tokenStr(pToken, pState->pOutputBuffer, true, true, ltype))
      {
        break;
      }
      /* only single command in one call */
      if (!pState->pTokenOut->pNext)
      {
        break;
      }
      ltype = HB_PP_TOKEN_TYPE(pToken->type);
    }
    if (fError)
    {
      pState->fError = true;
    }

    if (pnLen)
    {
      *pnLen = hb_membufLen(pState->pOutputBuffer);
    }
    hb_membufAddCh(pState->pOutputBuffer, '\0');

    return hb_membufPtr(pState->pOutputBuffer);
  }

  if (pnLen)
  {
    *pnLen = 0;
  }
  return nullptr;
}

/*
 * preprocess given buffer
 */
char *hb_pp_parseLine(PHB_PP_STATE pState, const char *pLine, HB_SIZE *pnLen)
{
  PHB_PP_TOKEN pToken;
  PHB_PP_FILE pFile;
  auto fError = false;
  HB_USHORT ltype;
  HB_SIZE nLen;

  if (!pState->pOutputBuffer)
  {
    pState->pOutputBuffer = hb_membufNew();
  }
  else
  {
    hb_membufFlush(pState->pOutputBuffer);
  }

  nLen = pnLen ? *pnLen : strlen(pLine);

  pFile = hb_pp_FileBufNew(pLine, nLen);
  pFile->pPrev = pState->pFile;
  pState->pFile = pFile;
  pState->iFiles++;

  pState->usLastType = ltype = HB_PP_TOKEN_NUL;
  while ((pToken = hb_pp_tokenGet(pState)) != nullptr)
  {
    if (pState->fError)
    {
      fError = true;
    }
    hb_pp_tokenStr(pToken, pState->pOutputBuffer, true, true, ltype);
    ltype = HB_PP_TOKEN_TYPE(pToken->type);
  }
  if (fError)
  {
    pState->fError = true;
  }

  if ((nLen && pLine[nLen - 1] == '\n') || hb_membufLen(pState->pOutputBuffer) == 0 ||
      hb_membufPtr(pState->pOutputBuffer)[hb_membufLen(pState->pOutputBuffer) - 1] != '\n')
  {
    hb_membufAddCh(pState->pOutputBuffer, '\0');
  }
  else
  {
    hb_membufPtr(pState->pOutputBuffer)[hb_membufLen(pState->pOutputBuffer) - 1] = '\0';
  }

  if (pnLen)
  {
    *pnLen = hb_membufLen(pState->pOutputBuffer) - 1;
  }

  if (pState->pFile == pFile)
  {
    pState->pFile = pFile->pPrev;
    hb_pp_FileFree(pState, pFile, nullptr);
    pState->iFiles--;
  }

  return hb_membufPtr(pState->pOutputBuffer);
}

/*
 * create new PP context for macro compiler
 */
PHB_PP_STATE hb_pp_lexNew(const char *pMacroString, HB_SIZE nLen)
{
  PHB_PP_STATE pState = hb_pp_new();

  pState->fQuiet = true;
  pState->fGauge = false;
  pState->pFile = hb_pp_FileBufNew(pMacroString, nLen);
  hb_pp_getLine(pState);
  pState->pTokenOut = pState->pFile->pTokenList;
  pState->pFile->pTokenList = nullptr;
  hb_pp_FileFree(pState, pState->pFile, nullptr);
  pState->pFile = nullptr;
  if (pState->fError)
  {
    hb_pp_free(pState);
    pState = nullptr;
  }
  else
  {
    pState->pNextTokenPtr = &pState->pTokenOut;
  }

  return pState;
}

PHB_PP_TOKEN hb_pp_lexGet(PHB_PP_STATE pState)
{
  PHB_PP_TOKEN pToken = *pState->pNextTokenPtr;

  if (pToken)
  {
    pState->pNextTokenPtr = &pToken->pNext;
  }

  return pToken;
}

HB_BOOL hb_pp_tokenNextExp(PHB_PP_TOKEN *pTokenPtr)
{
  if (hb_pp_tokenCanStartExp(*pTokenPtr))
  {
    HB_BOOL fStop = false;
    if (hb_pp_tokenSkipExp(pTokenPtr, nullptr, HB_PP_CMP_STD, &fStop) && !fStop)
    {
      return true;
    }
  }

  return false;
}

/*
 * convert token letters to upper cases
 * strip leading '&' and trailing '.' (if any) from macrovar token
 */
void hb_pp_tokenUpper(PHB_PP_TOKEN pToken)
{
  if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_MACROVAR)
  {
    if (pToken->len > HB_SYMBOL_NAME_LEN + 1)
    {
      pToken->len = HB_SYMBOL_NAME_LEN + 1;
    }
    if (pToken->value[pToken->len - 1] == '.')
    {
      pToken->len -= 2;
    }
    else
    {
      pToken->len--;
    }

    if (pToken->len <= 1)
    {
      HB_UCHAR ucVal = pToken->len ? static_cast<HB_UCHAR>(pToken->value[1]) : 0;
      if (HB_PP_TOKEN_ALLOC(pToken->type))
      {
        hb_xfree(HB_UNCONST(pToken->value));
        pToken->type |= HB_PP_TOKEN_STATIC;
      }
      pToken->value = hb_szAscii[ucVal];
    }
    else
    {
      if (!HB_PP_TOKEN_ALLOC(pToken->type))
      {
        pToken->value = static_cast<char *>(memcpy(hb_xgrab(pToken->len + 1), pToken->value + 1, pToken->len));
        pToken->type &= ~HB_PP_TOKEN_STATIC;
      }
      else
      {
        memmove(HB_UNCONST(pToken->value), pToken->value + 1, pToken->len);
      }
      (static_cast<char *>(HB_UNCONST(pToken->value)))[pToken->len] = '\0';
    }
  }
  else if (pToken->len > 1)
  {
    if (!HB_PP_TOKEN_ALLOC(pToken->type))
    {
      auto value = static_cast<char *>(hb_xgrab(pToken->len + 1));
      memcpy(value, pToken->value, pToken->len + 1);
      pToken->value = value;
      pToken->type &= ~HB_PP_TOKEN_STATIC;
    }
    if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_KEYWORD && pToken->len > HB_SYMBOL_NAME_LEN)
    {
      pToken->len = HB_SYMBOL_NAME_LEN;
      (static_cast<char *>(HB_UNCONST(pToken->value)))[HB_SYMBOL_NAME_LEN] = '\0';
    }
  }

  if (pToken->len <= 1)
  {
    HB_UCHAR ucVal = static_cast<HB_UCHAR>(HB_PP_UPPER(pToken->value[0]));
    if (HB_PP_TOKEN_ALLOC(pToken->type))
    {
      hb_xfree(HB_UNCONST(pToken->value));
      pToken->type |= HB_PP_TOKEN_STATIC;
    }
    pToken->value = hb_szAscii[ucVal];
  }
  else
  {
    hb_strupr(static_cast<char *>(HB_UNCONST(pToken->value)));
  }
}

/*
 * convert tokens between '[' and ']' tokens into single string token
 * and replace the converted tokens with the new string
 */
void hb_pp_tokenToString(PHB_PP_STATE pState, PHB_PP_TOKEN pToken)
{
  auto fError = true;

  pState->fError = false;
  hb_membufFlush(pState->pBuffer);
  if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_SB)
  {
    PHB_PP_TOKEN pTok, pFirst, pLast = nullptr;
    pFirst = pTok = pToken->pNext;
    while (!HB_PP_TOKEN_ISEOL(pTok))
    {
      pLast = pTok;
      if (HB_PP_TOKEN_TYPE(pTok->type) == HB_PP_TOKEN_RIGHT_SB)
      {
        while (pTok->spaces > 0)
        {
          hb_membufAddCh(pState->pBuffer, ' ');
          pTok->spaces--;
        }
        fError = false;
        pTok = pTok->pNext;
        break;
      }
      else if (HB_PP_TOKEN_TYPE(pTok->type) == HB_PP_TOKEN_EOC && !pTok->pNext && pState->pFile->pTokenList)
      {
        hb_pp_tokenMoveCommand(pState, &pTok->pNext, &pState->pFile->pTokenList);
      }
      hb_pp_tokenStr(pTok, pState->pBuffer, true, false, 0);
      pTok = pTok->pNext;
    }
    if (pLast)
    {
      pLast->pNext = nullptr;
      pToken->pNext = pTok;
      hb_pp_tokenListFree(&pFirst);
    }
    hb_pp_tokenSetValue(pToken, hb_membufPtr(pState->pBuffer), hb_membufLen(pState->pBuffer));
    HB_PP_TOKEN_SETTYPE(pToken, HB_PP_TOKEN_STRING);
    if (pState->fWritePreprocesed)
    {
      if (!fError)
      {
        hb_membufAddCh(pState->pBuffer, ']');
      }
      if (fwrite(hb_membufPtr(pState->pBuffer), sizeof(char), hb_membufLen(pState->pBuffer), pState->file_out) !=
          hb_membufLen(pState->pBuffer))
      {
        hb_pp_error(pState, 'F', HB_PP_ERR_WRITE_FILE, pState->szOutFileName);
      }
    }
  }

  if (fError)
  {
    hb_membufAddCh(pState->pBuffer, '\0');
    hb_pp_error(pState, 'E', HB_PP_ERR_STRING_TERMINATOR, hb_membufPtr(pState->pBuffer));
  }
}

char *hb_pp_tokenBlockString(PHB_PP_STATE pState, PHB_PP_TOKEN pToken, int *piType, HB_SIZE *pnLen)
{
  *piType = 0;
  hb_membufFlush(pState->pBuffer);
  if (HB_PP_TOKEN_TYPE(pToken->type) == HB_PP_TOKEN_LEFT_CB)
  {
    HB_USHORT ltype = HB_PP_TOKEN_NUL;
    int iBraces = 0;
    do
    {
      hb_pp_tokenStr(pToken, pState->pBuffer, ltype != HB_PP_TOKEN_NUL, true, ltype);
      ltype = HB_PP_TOKEN_TYPE(pToken->type);
      switch (ltype)
      {
      case HB_PP_TOKEN_MACROVAR:
      case HB_PP_TOKEN_MACROTEXT:
        *piType |= HB_BLOCK_MACROVAR;
        break;
      case HB_PP_TOKEN_RIGHT_CB:
        --iBraces;
        break;
      case HB_PP_TOKEN_LEFT_CB:
        ++iBraces;
        break;
      }
      pToken = pToken->pNext;
    } while (iBraces && !HB_PP_TOKEN_ISEOC(pToken));
  }
  *pnLen = hb_membufLen(pState->pBuffer);
  hb_membufAddCh(pState->pBuffer, '\0');
  return hb_membufPtr(pState->pBuffer);
}
