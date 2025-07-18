// Header file for the Extend API, Array API, misc API and base declarations
// Copyright 1999 Antonio Linares <alinares@fivetech.com>

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

// FIXME: There are several things in this file which are not part of the
//        standard Harbour API, in other words these things are not
//        guaranteed to remain unchanged. To avoid confusion these should be
//        moved to somewhere else (like hbrtl.h). [vszakats] */

#ifndef HB_APIEXT_H_
#define HB_APIEXT_H_

#include "hbvmpub.hpp"

HB_EXTERN_BEGIN

// this definition signals that number of decimal places for double value
// was not specified at compile time (the value is a result of optimization
// performed by the compiler)
#if defined(__cplusplus)
constexpr int HB_DEFAULT_WIDTH = 255;
constexpr int HB_DEFAULT_DECIMALS = 255;
#else
#define HB_DEFAULT_WIDTH     255
#define HB_DEFAULT_DECIMALS  255
#endif

// items types and type checking macros
// NOTE: maintained for compatibility with C language
#define HB_IT_NIL       0x00000
#define HB_IT_POINTER   0x00001
#define HB_IT_INTEGER   0x00002
#define HB_IT_HASH      0x00004
#define HB_IT_LONG      0x00008
#define HB_IT_DOUBLE    0x00010
#define HB_IT_DATE      0x00020
#define HB_IT_TIMESTAMP 0x00040
#define HB_IT_LOGICAL   0x00080
#define HB_IT_SYMBOL    0x00100
#define HB_IT_ALIAS     0x00200
#define HB_IT_STRING    0x00400
#define HB_IT_MEMOFLAG  0x00800
#define HB_IT_MEMO      (HB_IT_MEMOFLAG | HB_IT_STRING)
#define HB_IT_BLOCK     0x01000
#define HB_IT_BYREF     0x02000
#define HB_IT_MEMVAR    0x04000
#define HB_IT_ARRAY     0x08000
#define HB_IT_ENUM      0x10000
#define HB_IT_EXTREF    0x20000
#define HB_IT_DEFAULT   0x40000
#define HB_IT_RECOVER   0x80000
#define HB_IT_OBJECT    HB_IT_ARRAY
#define HB_IT_NUMERIC   (HB_IT_INTEGER | HB_IT_LONG | HB_IT_DOUBLE)
#define HB_IT_NUMINT    (HB_IT_INTEGER | HB_IT_LONG)
#define HB_IT_DATETIME  (HB_IT_DATE | HB_IT_TIMESTAMP)
#define HB_IT_ANY       0xFFFFFFFF
#define HB_IT_COMPLEX   (HB_IT_BLOCK | HB_IT_ARRAY | HB_IT_HASH | HB_IT_POINTER | /* HB_IT_MEMVAR | HB_IT_ENUM | HB_IT_EXTREF |*/ HB_IT_BYREF | HB_IT_STRING)
#define HB_IT_GCITEM    (HB_IT_BLOCK | HB_IT_ARRAY | HB_IT_HASH | HB_IT_POINTER | HB_IT_BYREF)
#define HB_IT_EVALITEM  (HB_IT_BLOCK | HB_IT_SYMBOL)
#define HB_IT_HASHKEY   (HB_IT_INTEGER | HB_IT_LONG | HB_IT_DOUBLE | HB_IT_DATE | HB_IT_TIMESTAMP | HB_IT_STRING | HB_IT_POINTER)

// NOTE:
// . for internal usage
// . used in src and contrib
#if defined(__cplusplus)
namespace Harbour
{
  namespace Item
  {
    enum Type : unsigned int
    {
      NIL       = 0x00000,
      POINTER   = 0x00001,
      INTEGER   = 0x00002,
      HASH      = 0x00004,
      LONG      = 0x00008,
      DOUBLE    = 0x00010,
      DATE      = 0x00020,
      TIMESTAMP = 0x00040,
      LOGICAL   = 0x00080,
      SYMBOL    = 0x00100,
      ALIAS     = 0x00200,
      STRING    = 0x00400,
      MEMOFLAG  = 0x00800,
      MEMO      = (MEMOFLAG | STRING),
      BLOCK     = 0x01000,
      BYREF     = 0x02000,
      MEMVAR    = 0x04000,
      ARRAY     = 0x08000,
      ENUM      = 0x10000,
      EXTREF    = 0x20000,
      DEFAULT   = 0x40000,
      RECOVER   = 0x80000,
      OBJECT    = ARRAY,
      NUMERIC   = (INTEGER | LONG | DOUBLE),
      NUMINT    = (INTEGER | LONG),
      DATETIME  = (DATE | TIMESTAMP),
      ANY       = 0xFFFFFFFF,
      COMPLEX   = (BLOCK | ARRAY | HASH | POINTER | /* HB_IT_MEMVAR | HB_IT_ENUM | HB_IT_EXTREF |*/ BYREF | STRING),
      GCITEM    = (BLOCK | ARRAY | HASH | POINTER | BYREF),
      EVALITEM  = (BLOCK | SYMBOL),
      HASHKEY   = (INTEGER | LONG | DOUBLE | DATE | TIMESTAMP | STRING | POINTER)
    };
  }
}
#endif

#if 0

// In Harbour VM HB_IT_BYREF is never ORed with item type. It can be used
// as stand alone type for locals and statics passed by reference or with
// HB_IT_MEMVAR for memvars passed by reference so this macro is less usable.
// only the hb_parinfo() function can return HB_TYPE as HB_IT_BYREF ORed
// with real type but this value is never set as item type.

#define HB_IS_OF_TYPE(p, t) ((HB_ITEM_TYPE(p) & ~HB_IT_BYREF) == t)

// These macros are slower but can be usable in debugging some code.
// They are a little bit more safe in buggy code but they can
// also hide bugs which should be exploited as soon as possible to
// know that something is wrong and has to be fixed.
// the version below which check only chosen bits allow compiler to
// use some optimizations if used CPU supports it. F.e. on standard
// x86 machines they can save few CPU cycles. [druzus]

#define HB_IS_NIL(p)        HB_IS_OF_TYPE(p, HB_IT_NIL)
#define HB_IS_ARRAY(p)      HB_IS_OF_TYPE(p, HB_IT_ARRAY)
#define HB_IS_BLOCK(p)      HB_IS_OF_TYPE(p, HB_IT_BLOCK)
#define HB_IS_DATE(p)       HB_IS_OF_TYPE(p, HB_IT_DATE)
#define HB_IS_TIMESTAMP(p)  HB_IS_OF_TYPE(p, HB_IT_TIMESTAMP)
#define HB_IS_DOUBLE(p)     HB_IS_OF_TYPE(p, HB_IT_DOUBLE)
#define HB_IS_INTEGER(p)    HB_IS_OF_TYPE(p, HB_IT_INTEGER)
#define HB_IS_LOGICAL(p)    HB_IS_OF_TYPE(p, HB_IT_LOGICAL)
#define HB_IS_LONG(p)       HB_IS_OF_TYPE(p, HB_IT_LONG)
#define HB_IS_SYMBOL(p)     HB_IS_OF_TYPE(p, HB_IT_SYMBOL)
#define HB_IS_POINTER(p)    HB_IS_OF_TYPE(p, HB_IT_POINTER)
#define HB_IS_HASH(p)       HB_IS_OF_TYPE(p, HB_IT_HASH)
#define HB_IS_MEMO(p)       HB_IS_OF_TYPE(p, HB_IT_MEMO)
#define HB_IS_MEMVAR(p)     HB_IS_OF_TYPE(p, HB_IT_MEMVAR)
#define HB_IS_ENUM(p)       HB_IS_OF_TYPE(p, HB_IT_ENUM)
#define HB_IS_EXTREF(p)     HB_IS_OF_TYPE(p, HB_IT_EXTREF)
#define HB_IS_STRING(p)     ((HB_ITEM_TYPE(p) & ~(HB_IT_BYREF | HB_IT_MEMOFLAG)) == HB_IT_STRING)
#define HB_IS_BYREF(p)      ((HB_ITEM_TYPE(p) & HB_IT_BYREF) != 0)
#define HB_IS_NUMERIC(p)    ((HB_ITEM_TYPE(p) & HB_IT_NUMERIC) != 0)
#define HB_IS_NUMINT(p)     ((HB_ITEM_TYPE(p) & HB_IT_NUMINT) != 0)
#define HB_IS_DATETIME(p)   ((HB_ITEM_TYPE(p) & HB_IT_DATETIME) != 0)
#define HB_IS_COMPLEX(p)    ((HB_ITEM_TYPE(p) & HB_IT_COMPLEX) != 0)
#define HB_IS_GCITEM(p)     ((HB_ITEM_TYPE(p) & HB_IT_GCITEM) != 0)
#define HB_IS_EVALITEM(p)   ((HB_ITEM_TYPE(p) & HB_IT_EVALITEM) != 0)
#define HB_IS_HASHKEY(p)    ((HB_ITEM_TYPE(p) & HB_IT_HASHKEY) != 0)
#define HB_IS_BADITEM(p)    ((HB_ITEM_TYPE(p) & HB_IT_COMPLEX) != 0 && (HB_ITEM_TYPE(p) & ~(HB_IT_COMPLEX | HB_IT_MEMOFLAG)) != 0)
#define HB_IS_OBJECT(p)     (HB_IS_ARRAY(p) && HB_ARRAY_OBJ(p))
#define HB_IS_NUMBER(p)     HB_IS_NUMERIC(p)

#elif 0

// these macros illustrates possible HB_TYPE bit combinations in HVM,
// they are the safest one in buggy code which may produce wrong item
// signatures but also they can be slower on some machines
#define HB_IS_NIL(p)        (HB_ITEM_TYPE(p) == HB_IT_NIL)
#define HB_IS_ARRAY(p)      (HB_ITEM_TYPE(p) == HB_IT_ARRAY)
#define HB_IS_BLOCK(p)      (HB_ITEM_TYPE(p) == HB_IT_BLOCK)
#define HB_IS_DATE(p)       (HB_ITEM_TYPE(p) == HB_IT_DATE)
#define HB_IS_TIMESTAMP(p)  (HB_ITEM_TYPE(p) == HB_IT_TIMESTAMP)
#define HB_IS_DOUBLE(p)     (HB_ITEM_TYPE(p) == HB_IT_DOUBLE)
#define HB_IS_INTEGER(p)    (HB_ITEM_TYPE(p) == HB_IT_INTEGER)
#define HB_IS_LOGICAL(p)    (HB_ITEM_TYPE(p) == HB_IT_LOGICAL)
#define HB_IS_LONG(p)       (HB_ITEM_TYPE(p) == HB_IT_LONG)
#define HB_IS_SYMBOL(p)     (HB_ITEM_TYPE(p) == HB_IT_SYMBOL)
#define HB_IS_POINTER(p)    (HB_ITEM_TYPE(p) == HB_IT_POINTER)
#define HB_IS_HASH(p)       (HB_ITEM_TYPE(p) == HB_IT_HASH)
#define HB_IS_MEMO(p)       (HB_ITEM_TYPE(p) == HB_IT_MEMO)
#define HB_IS_MEMVAR(p)     (HB_ITEM_TYPE(p) == (HB_IT_MEMVAR | HB_IT_BYREF))
#define HB_IS_ENUM(p)       (HB_ITEM_TYPE(p) == (HB_IT_ENUM | HB_IT_BYREF))
#define HB_IS_EXTREF(p)     (HB_ITEM_TYPE(p) == (HB_IT_EXTREF | HB_IT_BYREF))
#define HB_IS_STRING(p)     ((HB_ITEM_TYPE(p) & ~HB_IT_MEMOFLAG) == HB_IT_STRING)
#define HB_IS_BYREF(p)      ((HB_ITEM_TYPE(p) & ~HB_IT_MEMVAR) == HB_IT_BYREF)
#define HB_IS_NUMERIC(p)    ((HB_ITEM_TYPE(p) & HB_IT_NUMERIC) != 0)
#define HB_IS_NUMINT(p)     ((HB_ITEM_TYPE(p) & HB_IT_NUMINT) != 0)
#define HB_IS_DATETIME(p)   ((HB_ITEM_TYPE(p) & HB_IT_DATETIME) != 0)
#define HB_IS_COMPLEX(p)    ((HB_ITEM_TYPE(p) & HB_IT_COMPLEX) != 0)
#define HB_IS_GCITEM(p)     ((HB_ITEM_TYPE(p) & HB_IT_GCITEM) != 0)
#define HB_IS_EVALITEM(p)   ((HB_ITEM_TYPE(p) & HB_IT_EVALITEM) != 0)
#define HB_IS_HASHKEY(p)    ((HB_ITEM_TYPE(p) & HB_IT_HASHKEY) != 0)
#define HB_IS_BADITEM(p)    ((HB_ITEM_TYPE(p) & HB_IT_COMPLEX) != 0 && (HB_ITEM_TYPE(p) & ~(HB_IT_COMPLEX | HB_IT_MEMOFLAG)) != 0)
#define HB_IS_OBJECT(p)     (HB_IS_ARRAY(p) && HB_ARRAY_OBJ(p))
#define HB_IS_NUMBER(p)     HB_IS_NUMERIC(p)

#else

// these ones are can be the most efficiently optimized on some CPUs
#define HB_IS_NIL(p)        (HB_ITEM_TYPE(p) == HB_IT_NIL)
#define HB_IS_ARRAY(p)      ((HB_ITEM_TYPERAW(p) & HB_IT_ARRAY) != 0)
#define HB_IS_BLOCK(p)      ((HB_ITEM_TYPERAW(p) & HB_IT_BLOCK) != 0)
#define HB_IS_DATE(p)       ((HB_ITEM_TYPERAW(p) & HB_IT_DATE) != 0)
#define HB_IS_TIMESTAMP(p)  ((HB_ITEM_TYPERAW(p) & HB_IT_TIMESTAMP) != 0)
#define HB_IS_DOUBLE(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_DOUBLE) != 0)
#define HB_IS_INTEGER(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_INTEGER) != 0)
#define HB_IS_LOGICAL(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_LOGICAL) != 0)
#define HB_IS_LONG(p)       ((HB_ITEM_TYPERAW(p) & HB_IT_LONG) != 0)
#define HB_IS_SYMBOL(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_SYMBOL) != 0)
#define HB_IS_POINTER(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_POINTER) != 0)
#define HB_IS_HASH(p)       ((HB_ITEM_TYPERAW(p) & HB_IT_HASH) != 0)
#define HB_IS_MEMO(p)       ((HB_ITEM_TYPERAW(p) & HB_IT_MEMOFLAG) != 0)
#define HB_IS_STRING(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_STRING) != 0)
#define HB_IS_MEMVAR(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_MEMVAR) != 0)
#define HB_IS_ENUM(p)       ((HB_ITEM_TYPERAW(p) & HB_IT_ENUM) != 0)
#define HB_IS_EXTREF(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_EXTREF) != 0)
#define HB_IS_BYREF(p)      ((HB_ITEM_TYPERAW(p) & HB_IT_BYREF) != 0)
#define HB_IS_NUMERIC(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_NUMERIC) != 0)
#define HB_IS_NUMINT(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_NUMINT) != 0)
#define HB_IS_DATETIME(p)   ((HB_ITEM_TYPERAW(p) & HB_IT_DATETIME) != 0)
#define HB_IS_COMPLEX(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_COMPLEX) != 0)
#define HB_IS_GCITEM(p)     ((HB_ITEM_TYPERAW(p) & HB_IT_GCITEM) != 0)
#define HB_IS_EVALITEM(p)   ((HB_ITEM_TYPERAW(p) & HB_IT_EVALITEM) != 0)
#define HB_IS_HASHKEY(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_HASHKEY) != 0)
#define HB_IS_BADITEM(p)    ((HB_ITEM_TYPERAW(p) & HB_IT_COMPLEX) != 0 && (HB_ITEM_TYPERAW(p) & ~(HB_IT_COMPLEX | HB_IT_MEMOFLAG | HB_IT_DEFAULT)) != 0)
#define HB_IS_OBJECT(p)     (HB_IS_ARRAY(p) && HB_ARRAY_OBJ(p))
#define HB_IS_NUMBER(p)     HB_IS_NUMERIC(p)

#endif

#define HB_ISNIL(n)         (hb_extIsNil(n)) // NOTE: Intentionally using a different method
#define HB_ISCHAR(n)        (hb_param(n, HB_IT_STRING) != NULL)
#define HB_ISNUM(n)         (hb_param(n, HB_IT_NUMERIC) != NULL)
#define HB_ISLOG(n)         (hb_param(n, HB_IT_LOGICAL) != NULL)
#define HB_ISDATE(n)        (hb_param(n, HB_IT_DATE) != NULL)
#define HB_ISTIMESTAMP(n)   (hb_param(n, HB_IT_TIMESTAMP) != NULL)
#define HB_ISMEMO(n)        (hb_param(n, HB_IT_MEMOFLAG) != NULL)
#define HB_ISBYREF(n)       ((hb_parinfo(n) & HB_IT_BYREF) != 0) // NOTE: Intentionally using a different method
#define HB_ISARRAY(n)       (hb_param(n, HB_IT_ARRAY) != NULL)
#define HB_ISOBJECT(n)      (hb_extIsObject(n))
#define HB_ISBLOCK(n)       (hb_param(n, HB_IT_BLOCK) != NULL)
#define HB_ISPOINTER(n)     (hb_param(n, HB_IT_POINTER) != NULL)
#define HB_ISHASH(n)        (hb_param(n, HB_IT_HASH) != NULL)
#define HB_ISSYMBOL(n)      (hb_param(n, HB_IT_SYMBOL) != NULL)
#define HB_ISEVALITEM(n)    (hb_param(n, HB_IT_EVALITEM) != NULL)
#define HB_ISDATETIME(n)    (hb_param(n, HB_IT_DATETIME) != NULL)

// Compatibility #defines, deprecated
#if defined(HB_LEGACY_LEVEL4) && defined(HB_LEGACY_TYPES_ON)
   #define ISNIL(n)            HB_ISNIL(n)
   #define ISCHAR(n)           HB_ISCHAR(n)
   #define ISNUM(n)            HB_ISNUM(n)
   #define ISLOG(n)            HB_ISLOG(n)
   #define ISDATE(n)           HB_ISDATE(n)
   #define ISTIMESTAMP(n)      HB_ISTIMESTAMP(n)
   #define ISMEMO(n)           HB_ISMEMO(n)
   #define ISBYREF(n)          HB_ISBYREF(n)
   #define ISARRAY(n)          HB_ISARRAY(n)
   #define ISOBJECT(n)         HB_ISOBJECT(n)
   #define ISBLOCK(n)          HB_ISBLOCK(n)
   #define ISPOINTER(n)        HB_ISPOINTER(n)
   #define ISHASH(n)           HB_ISHASH(n)
   #define ISSYMBOL(n)         HB_ISSYMBOL(n)
   #define ISDATETIME(n)       HB_ISDATETIME(n)
#endif

#ifdef _HB_API_INTERNAL_

// forward declarations
struct _HB_CODEBLOCK;
struct _HB_BASEARRAY;
struct _HB_BASEHASH;
struct _HB_ITEM;
struct _HB_EXTREF;

typedef struct _HB_STACK_STATE
{
  HB_ISIZ   nBaseItem;        // stack base offset of previous func/proc
  HB_SIZE   nPrivateBase;     // memvars base offset of previous func/proc
  void *    pStatics;         // statics frame of previous func/proc
  HB_USHORT uiClass;          // class when message is sent
  HB_USHORT uiMethod;         // number of class method
  HB_USHORT uiLineNo;         // current line number
  HB_USHORT fDebugging;       // debugger active
} HB_STACK_STATE, * PHB_STACK_STATE; // used to save/restore stack state in hb_vmDo)_

// Internal structures that holds data
struct hb_struArray
{
  struct _HB_BASEARRAY *value;
};

struct hb_struHash
{
  struct _HB_BASEHASH *value;
};

struct hb_struBlock
{
  struct _HB_CODEBLOCK *value;
  HB_USHORT paramcnt;
  HB_USHORT lineno;
  HB_USHORT hclass;
  HB_USHORT method;
};

struct hb_struPointer
{
  void *value;
  HB_BOOL collect;
  HB_BOOL single;
};

struct hb_struDateTime
{
  long julian;
  long time;
};

struct hb_struDouble
{
  double value;
  HB_USHORT length;
  HB_USHORT decimal;
};

struct hb_struInteger
{
  int value;
  HB_USHORT length;
};

struct hb_struLong
{
  HB_MAXINT value;
  HB_USHORT length;
};

struct hb_struLogical
{
  HB_BOOL value;
};

struct hb_struMemvar
{
  struct _HB_ITEM *value;
};

struct hb_struRefer
{
  union
  {
    struct _HB_BASEARRAY *array;       // array (statics and array item references)
    struct _HB_CODEBLOCK *block;       // codeblock
    struct _HB_ITEM *itemPtr;          // item pointer
    struct _HB_ITEM ** *itemsbasePtr;   // local variables
  } BasePtr;
  HB_ISIZ offset;                        // 0 for static variables
  HB_ISIZ value;
};

struct hb_struEnum
{
  struct _HB_ITEM *basePtr;             // base item pointer
  struct _HB_ITEM *valuePtr;            // value item pointer
  HB_ISIZ offset;
};

struct hb_struExtRef
{
  void *value;                          // value item pointer
  const struct _HB_EXTREF *func;        // extended reference functions
};

struct hb_struString
{
  HB_SIZE length;
  HB_SIZE allocated;     // size of memory block allocated for string value, 0 for static strings
  char *value;
};

struct hb_struSymbol
{
  PHB_SYMB        value;
  PHB_STACK_STATE stackstate;      // function stack state
  HB_USHORT       paramcnt;        // number of passed parameters in function call
  HB_USHORT       paramdeclcnt;    // number of declared parameters in function definition
};

struct hb_struRecover
{
  const HB_BYTE *recover;    // address of recover code
  HB_SIZE        base;       // previous recover base
  HB_USHORT      flags;      // previous recovery state and recover type
  HB_USHORT      request;    // requested action
};

// items hold at the virtual machine stack
typedef struct _HB_ITEM
{
  HB_TYPE type;
  union
  {
    struct hb_struArray     asArray;
    struct hb_struBlock     asBlock;
    struct hb_struDateTime  asDateTime;
    struct hb_struDouble    asDouble;
    struct hb_struInteger   asInteger;
    struct hb_struLogical   asLogical;
    struct hb_struLong      asLong;
    struct hb_struPointer   asPointer;
    struct hb_struHash      asHash;
    struct hb_struMemvar    asMemvar;
    struct hb_struRefer     asRefer;
    struct hb_struEnum      asEnum;
    struct hb_struExtRef    asExtRef;
    struct hb_struString    asString;
    struct hb_struSymbol    asSymbol;
    struct hb_struRecover   asRecover;
  } item;

// for internal use (core)
#if defined(__cplusplus)
  //
  HB_TYPE rawType();
  void setType(HB_TYPE type);
  //
  bool isNil();
  bool isArray();
  bool isBlock();
  bool isDate();
  bool isTimeStamp();
  bool isDouble();
  bool isInteger();
  bool isLogical();
  bool isLong();
  bool isSymbol();
  bool isPointer();
  bool isHash();
  bool isMemo();
  bool isString();
  bool isMemVar();
  bool isEnum();
  bool isExtRef();
  bool isByRef();
  bool isNumeric();
  bool isNumInt();
  bool isDateTime();
  bool isComplex();
  bool isGCItem();
  bool isEvalItem();
  bool isHashKey();
  bool isBadItem();
  bool isObject();
  bool isNumber(); // TODO: same as isNumeric()
  //
  HB_EXPORT int getNI();
  HB_EXPORT long getNL();
  HB_EXPORT double getND();
  HB_EXPORT HB_BOOL getL();
  HB_EXPORT char *getC();
  HB_EXPORT const char *getCPtr();
  HB_EXPORT HB_SIZE getCLen();
  HB_EXPORT HB_MAXINT getNInt();
  HB_EXPORT char *getDS(char *szDate);
  HB_EXPORT long getDL();
  HB_EXPORT double getTD();
  HB_EXPORT HB_BOOL getTDT(long *plJulian, long *plMilliSec);
  HB_EXPORT void *getPtr();
  //HB_EXPORT void *getPtrGC(const HB_GC_FUNCS *pFuncs); TODO:
  HB_EXPORT PHB_SYMB getSymbol();
  //
  HB_EXPORT _HB_ITEM *putNI(int iNumber);
  HB_EXPORT _HB_ITEM *putNL(long lNumber);
  HB_EXPORT _HB_ITEM *putL(HB_BOOL bValue);
  HB_EXPORT _HB_ITEM *putC(const char *szText);
  HB_EXPORT _HB_ITEM *putCL(const char *szText, HB_SIZE nLen);
  HB_EXPORT _HB_ITEM *putCConst(const char *szText);
  HB_EXPORT _HB_ITEM *putCLConst(const char *szText, HB_SIZE nLen);
  HB_EXPORT _HB_ITEM *putCPtr(char *szText);
  HB_EXPORT _HB_ITEM *putCLPtr(char *szText, HB_SIZE nLen);
  HB_EXPORT void setCMemo();
  //
  HB_BOOL logicalValue();
  void setLogicalValue(HB_BOOL bValue);
  //
  int integerValue();
  void setIntegerValue(int iValue);
  HB_USHORT integerLength();
  void setIntegerLength(HB_USHORT length);
  //
  HB_MAXINT longValue();
  void setLongValue(HB_MAXINT lValue);
  HB_USHORT longLength();
  void setLongLength(HB_USHORT length);
  //
  double doubleValue();
  void setDoubleValue(double dValue);
  HB_USHORT doubleLength();
  void setDoubleLength(HB_USHORT length);
  HB_USHORT doubleDecimal();
  void setDoubleDecimal(HB_USHORT decimal);
  //
  char *stringValue();
  void setStringValue(char *sValue);
  HB_SIZE stringLength();
  void setStringLength(HB_SIZE length);
  HB_SIZE stringAllocated();
  void setStringAllocated(HB_SIZE allocated);
  //
  void *pointerValue();
  void setPointerValue(void *pValue);
  HB_BOOL pointerCollect();
  void setPointerCollect(HB_BOOL b);
  HB_BOOL pointerSingle();
  void setPointerSingle(HB_BOOL b);
  //
  _HB_BASEARRAY *arrayValue();
  void setArrayValue(_HB_BASEARRAY *pValue);
  _HB_ITEM *arrayItems();
  bool isValidIndex(HB_SIZE nIndex);
  _HB_ITEM *arrayItem(HB_SIZE nIndex);
  HB_SIZE arrayLen();
  //
  long dateTimeJulian();
  void setDateTimeJulian(long lValue);
  long dateTimeTime();
  void setDateTimeTime(long lValue);
  //
  PHB_SYMB symbolValue();
  void setSymbolValue(PHB_SYMB pValue);
  PHB_STACK_STATE symbolStackState();
  void setSymbolStackState(PHB_STACK_STATE pValue);
  HB_USHORT symbolParamCnt();
  void setSymbolParamCnt(HB_USHORT usValue);
  HB_USHORT symbolParamDeclCnt();
  void setSymbolParamDeclCnt(HB_USHORT usValue);
  //
  _HB_CODEBLOCK *blockValue();
  void setBlockValue(_HB_CODEBLOCK *pValue);
  HB_USHORT blockParamCnt();
  void setBlockParamCnt(HB_USHORT usValue);
  HB_USHORT blockLineNo();
  void setBlockLineNo(HB_USHORT usValue);
  HB_USHORT blockHClass();
  void setBlockHClass(HB_USHORT usValue);
  HB_USHORT blockMethod();
  void setBlockMethod(HB_USHORT usValue);
  //
  _HB_BASEHASH *hashValue();
  void setHashValue(_HB_BASEHASH *pValue);
  //
  HB_EXPORT void clear();
#endif
} HB_ITEM, * PHB_ITEM;

#if defined(__cplusplus)

inline HB_TYPE _HB_ITEM::rawType()
{
  return this->type;
}

inline void _HB_ITEM::setType(HB_TYPE _type)
{
  this->type = _type;
}

inline bool _HB_ITEM::isNil()
{
  return (HB_ITEM_TYPE(this) == Harbour::Item::NIL);
}

inline bool _HB_ITEM::isArray()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::ARRAY) != 0);
}

inline bool _HB_ITEM::isBlock()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::BLOCK) != 0);
}

inline bool _HB_ITEM::isDate()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::DATE) != 0);
}

inline bool _HB_ITEM::isTimeStamp()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::TIMESTAMP) != 0);
}

inline bool _HB_ITEM::isDouble()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::DOUBLE) != 0);
}

inline bool _HB_ITEM::isInteger()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::INTEGER) != 0);
}

inline bool _HB_ITEM::isLogical()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::LOGICAL) != 0);
}

inline bool _HB_ITEM::isLong()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::LONG) != 0);
}

inline bool _HB_ITEM::isSymbol()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::SYMBOL) != 0);
}

inline bool _HB_ITEM::isPointer()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::POINTER) != 0);
}

inline bool _HB_ITEM::isHash()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::HASH) != 0);
}

inline bool _HB_ITEM::isMemo()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::MEMOFLAG) != 0);
}

inline bool _HB_ITEM::isString()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::STRING) != 0);
}

inline bool _HB_ITEM::isMemVar()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::MEMVAR) != 0);
}

inline bool _HB_ITEM::isEnum()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::ENUM) != 0);
}

inline bool _HB_ITEM::isExtRef()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::EXTREF) != 0);
}

inline bool _HB_ITEM::isByRef()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::BYREF) != 0);
}

inline bool _HB_ITEM::isNumeric()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::NUMERIC) != 0);
}

inline bool _HB_ITEM::isNumInt()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::NUMINT) != 0);
}

inline bool _HB_ITEM::isDateTime()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::DATETIME) != 0);
}

inline bool _HB_ITEM::isComplex()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::COMPLEX) != 0);
}

inline bool _HB_ITEM::isGCItem()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::GCITEM) != 0);
}

inline bool _HB_ITEM::isEvalItem()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::EVALITEM) != 0);
}

inline bool _HB_ITEM::isHashKey()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::HASHKEY) != 0);
}

inline bool _HB_ITEM::isBadItem()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::COMPLEX) != 0 && (HB_ITEM_TYPERAW(this) & ~(Harbour::Item::COMPLEX | Harbour::Item::MEMOFLAG | Harbour::Item::DEFAULT)) != 0);
}

inline bool _HB_ITEM::isNumber()
{
  return ((HB_ITEM_TYPERAW(this) & Harbour::Item::NUMERIC) != 0);
}

// logical

inline HB_BOOL _HB_ITEM::logicalValue()
{
  return this->item.asLogical.value;
}

inline void _HB_ITEM::setLogicalValue(HB_BOOL bValue)
{
  this->item.asLogical.value = bValue;
}

// integer

inline int _HB_ITEM::integerValue()
{
  return this->item.asInteger.value;
}

inline void _HB_ITEM::setIntegerValue(int iValue)
{
  this->item.asInteger.value = iValue;
}

inline HB_USHORT _HB_ITEM::integerLength()
{
  return this->item.asInteger.length;
}

inline void _HB_ITEM::setIntegerLength(HB_USHORT length)
{
  this->item.asInteger.length = length;
}

// long

inline HB_MAXINT _HB_ITEM::longValue()
{
  return this->item.asLong.value;
}

inline void _HB_ITEM::setLongValue(HB_MAXINT lValue)
{
  this->item.asLong.value = lValue;
}

inline HB_USHORT _HB_ITEM::longLength()
{
  return this->item.asLong.length;
}

inline void _HB_ITEM::setLongLength(HB_USHORT length)
{
  this->item.asLong.length = length;
}

// double

inline double _HB_ITEM::doubleValue()
{
  return this->item.asDouble.value;
}

inline void _HB_ITEM::setDoubleValue(double dValue)
{
  this->item.asDouble.value = dValue;
}

inline HB_USHORT _HB_ITEM::doubleLength()
{
  return this->item.asDouble.length;
}

inline void _HB_ITEM::setDoubleLength(HB_USHORT length)
{
  this->item.asDouble.length = length;
}

inline HB_USHORT _HB_ITEM::doubleDecimal()
{
  return this->item.asDouble.decimal;
}

inline void _HB_ITEM::setDoubleDecimal(HB_USHORT decimal)
{
  this->item.asDouble.decimal = decimal;
}

// string

inline char *_HB_ITEM::stringValue()
{
  return this->item.asString.value;
}

inline void _HB_ITEM::setStringValue(char *sValue)
{
  this->item.asString.value = sValue;
}

inline HB_SIZE _HB_ITEM::stringLength()
{
  return this->item.asString.length;
}

inline void _HB_ITEM::setStringLength(HB_SIZE length)
{
  this->item.asString.length = length;
}

inline HB_SIZE _HB_ITEM::stringAllocated()
{
  return this->item.asString.allocated;
}

inline void _HB_ITEM::setStringAllocated(HB_SIZE allocated)
{
  this->item.asString.allocated = allocated;
}

// pointer

inline void *_HB_ITEM::pointerValue()
{
  return this->item.asPointer.value;
}

inline void _HB_ITEM::setPointerValue(void *pValue)
{
  this->item.asPointer.value = pValue;
}

inline HB_BOOL _HB_ITEM::pointerCollect()
{
  return this->item.asPointer.collect;
}

inline void _HB_ITEM::setPointerCollect(HB_BOOL b)
{
  this->item.asPointer.collect = b;
}

inline HB_BOOL _HB_ITEM::pointerSingle()
{
  return this->item.asPointer.single;
}

inline void _HB_ITEM::setPointerSingle(HB_BOOL b)
{
  this->item.asPointer.single = b;
}

// array

inline _HB_BASEARRAY *_HB_ITEM::arrayValue()
{
  return this->item.asArray.value;
}

inline void _HB_ITEM::setArrayValue(_HB_BASEARRAY *pValue)
{
  this->item.asArray.value = pValue;
}

// datetime

inline long _HB_ITEM::dateTimeJulian()
{
  return this->item.asDateTime.julian;
}

inline void _HB_ITEM::setDateTimeJulian(long lValue)
{
  this->item.asDateTime.julian = lValue;
}

inline long _HB_ITEM::dateTimeTime()
{
  return this->item.asDateTime.time;
}

inline void _HB_ITEM::setDateTimeTime(long lValue)
{
  this->item.asDateTime.time = lValue;
}

// symbol

inline PHB_SYMB _HB_ITEM::symbolValue()
{
  return this->item.asSymbol.value;
}

inline void _HB_ITEM::setSymbolValue(PHB_SYMB pValue)
{
  this->item.asSymbol.value = pValue;
}

inline PHB_STACK_STATE _HB_ITEM::symbolStackState()
{
  return this->item.asSymbol.stackstate;
}

inline void _HB_ITEM::setSymbolStackState(PHB_STACK_STATE pValue)
{
  this->item.asSymbol.stackstate = pValue;
}

inline HB_USHORT _HB_ITEM::symbolParamCnt()
{
  return this->item.asSymbol.paramcnt;
}

inline void _HB_ITEM::setSymbolParamCnt(HB_USHORT usValue)
{
  this->item.asSymbol.paramcnt = usValue;
}

inline HB_USHORT _HB_ITEM::symbolParamDeclCnt()
{
  return this->item.asSymbol.paramdeclcnt;
}

inline void _HB_ITEM::setSymbolParamDeclCnt(HB_USHORT usValue)
{
  this->item.asSymbol.paramdeclcnt = usValue;
}

// block

inline _HB_CODEBLOCK *_HB_ITEM::blockValue()
{
  return this->item.asBlock.value;
}

inline void _HB_ITEM::setBlockValue(_HB_CODEBLOCK *pValue)
{
  this->item.asBlock.value = pValue;
}

inline HB_USHORT _HB_ITEM::blockParamCnt()
{
  return this->item.asBlock.paramcnt;
}

inline void _HB_ITEM::setBlockParamCnt(HB_USHORT usValue)
{
  this->item.asBlock.paramcnt = usValue;
}

inline HB_USHORT _HB_ITEM::blockLineNo()
{
  return this->item.asBlock.lineno;
}

inline void _HB_ITEM::setBlockLineNo(HB_USHORT usValue)
{
  this->item.asBlock.lineno = usValue;
}

inline HB_USHORT _HB_ITEM::blockHClass()
{
  return this->item.asBlock.hclass;
}

inline void _HB_ITEM::setBlockHClass(HB_USHORT usValue)
{
  this->item.asBlock.hclass = usValue;
}

inline HB_USHORT _HB_ITEM::blockMethod()
{
  return this->item.asBlock.method;
}

inline void _HB_ITEM::setBlockMethod(HB_USHORT usValue)
{
  this->item.asBlock.method = usValue;
}

// hash

inline _HB_BASEHASH *_HB_ITEM::hashValue()
{
  return this->item.asHash.value;
}

inline void _HB_ITEM::setHashValue(_HB_BASEHASH *pValue)
{
  this->item.asHash.value = pValue;
}

#endif

// internal structure for arrays
typedef struct _HB_BASEARRAY
{
  PHB_ITEM    pItems;       // pointer to the array items
  HB_SIZE     nLen;         // number of items in the array
  HB_SIZE     nAllocated;   // number of allocated items
  HB_USHORT   uiClass;      // offset to the classes base if it is an object
  HB_USHORT   uiPrevCls;    // for fixing after access super
} HB_BASEARRAY, * PHB_BASEARRAY;

#if defined(__cplusplus)

inline bool _HB_ITEM::isObject()
{
  return (HB_IS_ARRAY(this) && HB_ARRAY_OBJ(this));
}

inline _HB_ITEM *_HB_ITEM::arrayItems()
{
  return this->item.asArray.value->pItems;
}

inline bool _HB_ITEM::isValidIndex(HB_SIZE nIndex)
{
  return nIndex > 0 && nIndex <= this->arrayLen();
}

inline _HB_ITEM *_HB_ITEM::arrayItem(HB_SIZE nIndex)
{
  return this->item.asArray.value->pItems + (nIndex - 1);
}

inline HB_SIZE _HB_ITEM::arrayLen()
{
  return this->item.asArray.value->nLen;
}

#endif

#ifndef _HB_HASH_INTERNAL_
// internal structure for hashes
typedef struct _HB_BASEHASH
{
  void *value;
} HB_BASEHASH, * PHB_BASEHASH;
#endif

// internal structure for codeblocks
typedef struct _HB_CODEBLOCK
{
  const HB_BYTE *pCode;     // codeblock pcode
  PHB_SYMB    pSymbols;     // codeblocks symbols
  PHB_SYMB    pDefSymb;     // symbol where the codeblock was created
  PHB_ITEM    pLocals;      // table with referenced local variables
  void *      pStatics;     // STATICs base frame
  HB_USHORT   uiLocals;     // number of referenced local variables
  HB_SHORT    dynBuffer;    // is pcode buffer allocated dynamically, SHORT used instead of HB_BOOL intentionally to force optimal alignment
} HB_CODEBLOCK, * PHB_CODEBLOCK;

typedef void     (*HB_EXTREF_FUNC0)(void *);
typedef PHB_ITEM (*HB_EXTREF_FUNC1)(PHB_ITEM);
typedef PHB_ITEM (*HB_EXTREF_FUNC2)(PHB_ITEM, PHB_ITEM);
typedef void     (*HB_EXTREF_FUNC3)(PHB_ITEM);

typedef struct _HB_EXTREF
{
  HB_EXTREF_FUNC1 read;
  HB_EXTREF_FUNC2 write;
  HB_EXTREF_FUNC3 copy;
  HB_EXTREF_FUNC0 clear;
  HB_EXTREF_FUNC0 mark;
} HB_EXTREF, * PHB_EXTREF;

typedef struct
{
  void *value;
  PHB_ITEM pDest;
} HB_NESTED_REF, * PHB_NESTED_REF;

typedef struct
{
  HB_SIZE        nSize;
  HB_SIZE        nCount;
  PHB_NESTED_REF pRefs;
} HB_NESTED_CLONED, * PHB_NESTED_CLONED;

#endif // _HB_API_INTERNAL_

// RDD method return codes
typedef unsigned int HB_ERRCODE;

// NOTA: mantido para compatibilidade
#define HB_SUCCESS         0
#define HB_FAILURE         1

// NOTA: usar em src e contrib
#if defined(__cplusplus)
namespace Harbour
{
  enum Result : unsigned int
  {
    SUCCESS = 0,
    FAILURE = 1
  };
}
#endif

#if defined(_HB_API_INTERNAL_)
// NOTE: Deprecated. Use 'hb_vmPushEvalSym()' instead of 'hb_vmPushSymbol(&hb_symEval)'
extern HB_SYMB hb_symEval;
#endif

// Initialize fixed memory subsystem
extern HB_EXPORT void hb_xinit(void);
// Deinitialize fixed memory subsystem
extern HB_EXPORT void hb_xexit(void);
// allocates memory, returns NULL on failure
extern HB_EXPORT void *hb_xalloc(HB_SIZE nSize);
// allocates memory, exits on failure
extern HB_EXPORT void *hb_xgrab(HB_SIZE nSize) HB_MALLOC_ATTR HB_ALLOC_SIZE_ATTR(1);
// frees memory
extern HB_EXPORT void hb_xfree(void *pMem);
// reallocates memory
extern HB_EXPORT void *hb_xrealloc(void *pMem, HB_SIZE nSize) HB_ALLOC_SIZE_ATTR(2);
// returns the size of an allocated memory block
extern HB_EXPORT HB_SIZE hb_xsize(void *pMem);
// return allocation place (function name and line number)
extern HB_EXPORT const char *hb_xinfo(void *pMem, HB_USHORT *puiLine);
// Query different types of memory information
extern HB_EXPORT HB_SIZE hb_xquery(int iMode);
extern HB_EXPORT HB_BOOL hb_xtraced(void);
extern HB_EXPORT void hb_xsetfilename(const char *szValue);
extern HB_EXPORT void hb_xsetinfo(const char *szValue);
#ifdef _HB_API_INTERNAL_
extern void hb_xinit_thread(void);
extern void hb_xexit_thread(void);
extern void hb_xclean(void);
#endif

#ifdef _HB_API_INTERNAL_
// increment reference counter
extern void hb_xRefInc(void *pMem);
// decrement reference counter, return HB_TRUE when 0 reached
extern HB_BOOL hb_xRefDec(void *pMem);
// decrement reference counter and free the block when 0 reached
extern void hb_xRefFree(void *pMem);
// return number of references
extern HB_COUNTER hb_xRefCount(void *pMem);
// reallocates memory, create copy if reference counter greater then 1
extern void *hb_xRefResize(void *pMem, HB_SIZE nSave, HB_SIZE nSize, HB_SIZE *pnAllocated);

#if 0

// I used this macros only to test some speed overhead,
// They may not be supported in the future so please do
// not create any code which needs them. [druzus]

#define hb_xRefInc(p)             (++(*HB_COUNTER_PTR(p)))
#define hb_xRefDec(p)             (--(*HB_COUNTER_PTR(p))==0)
#define hb_xRefFree(p)            do { \
                                       if (hb_xRefDec(p)) \
                                          hb_xfree(p); \
                                    } while (0)
#define hb_xRefCount(p)           (*HB_COUNTER_PTR(p))

#endif

#endif // _HB_API_INTERNAL_

#if defined(HB_LEGACY_LEVEL4)
#  define HB_ITEM_PTR         PHB_ITEM
#  define HB_BASEARRAY_PTR    PHB_BASEARRAY
#  define HB_CODEBLOCK_PTR    PHB_CODEBLOCK
#  define HB_MACRO_PTR        PHB_MACRO
#  define HB_ERROR_INFO_PTR   PHB_ERROR_INFO
#  define HB_HASH_TABLE_PTR   PHB_HASH_TABLE
#  define HB_GARBAGE_FUNC_PTR PHB_GARBAGE_FUNC
#endif

#define hb_xgrabz(n)        memset(hb_xgrab((n)), 0, (n))
#define hb_xmemdup(p, n)    memcpy(hb_xgrab((n)), (p), (n))
#define hb_xreallocz(p, n)  memset(hb_xrealloc((p), (n)), 0, (n))

// #if UINT_MAX == ULONG_MAX
// it fails on 64-bit platforms where int has 32 bits and long has 64 bits.
// we need these functions only when max(size_t) < max(long)
// and only on 16-bit platforms, so the below condition seems to be
// more reasonable.
#if UINT_MAX > USHRT_MAX
   // NOTE: memcpy()/memset() can work with HB_SIZE data blocks
   #define  hb_xmemcpy  memcpy
   #define  hb_xmemset  memset
#else
   // NOTE: otherwise, the hb_xmemcpy() and hb_xmemset() functions
   //       will be used to copy and/or set HB_SIZE data blocks
// copy more than memcpy() can
extern HB_EXPORT void *hb_xmemcpy(void *pDestArg, const void *pSourceArg, HB_SIZE nLen);
// set more than memset() can
extern HB_EXPORT void *hb_xmemset(void *pDestArg, int iFill, HB_SIZE nLen);
#endif

// virtual memory
typedef unsigned long HB_VMHANDLE;

extern HB_EXPORT HB_VMHANDLE hb_xvalloc(HB_SIZE nSize, HB_USHORT nFlags);
extern HB_EXPORT void hb_xvfree(HB_VMHANDLE h);
extern HB_EXPORT HB_VMHANDLE hb_xvrealloc(HB_VMHANDLE h, HB_SIZE nSize, HB_USHORT nFlags);
extern HB_EXPORT void *hb_xvlock(HB_VMHANDLE h);
extern HB_EXPORT void hb_xvunlock(HB_VMHANDLE h);
extern HB_EXPORT void *hb_xvwire(HB_VMHANDLE h);
extern HB_EXPORT void hb_xvunwire(HB_VMHANDLE h);
extern HB_EXPORT HB_SIZE hb_xvlockcount(HB_VMHANDLE h);
extern HB_EXPORT HB_SIZE hb_xvsize(HB_VMHANDLE h);
extern HB_EXPORT HB_VMHANDLE hb_xvheapnew(HB_SIZE nSize);
extern HB_EXPORT void hb_xvheapdestroy(HB_VMHANDLE h);
extern HB_EXPORT HB_VMHANDLE hb_xvheapresize(HB_VMHANDLE h, HB_SIZE nSize);
extern HB_EXPORT HB_SIZE hb_xvheapalloc(HB_VMHANDLE h, HB_SIZE nSize);
extern HB_EXPORT void hb_xvheapfree(HB_VMHANDLE h, HB_SIZE nOffset);
extern HB_EXPORT void *hb_xvheaplock(HB_VMHANDLE h, HB_SIZE nOffset);
extern HB_EXPORT void hb_xvheapunlock(HB_VMHANDLE h, HB_SIZE nOffset);

// garbage collector
// callback function for cleaning garbage memory pointer
#define HB_GARBAGE_FUNC(hbfunc)   void hbfunc(void *Cargo)
typedef HB_GARBAGE_FUNC((*PHB_GARBAGE_FUNC));

typedef struct
{
  PHB_GARBAGE_FUNC  clear;
  PHB_GARBAGE_FUNC  mark;
} HB_GC_FUNCS;

// allocates a memory controlled by the garbage collector
extern HB_EXPORT void *hb_gcAllocate(HB_SIZE nSize, const HB_GC_FUNCS *pFuncs);
// deallocates a memory allocated by the garbage collector
extern HB_EXPORT void hb_gcFree(void *pAlloc);
// do not release passed memory block
extern HB_EXPORT void *hb_gcLock(void *pAlloc);
// passed block is allowed to be released
extern HB_EXPORT void *hb_gcUnlock(void *pAlloc);
// mark given block as used
extern HB_EXPORT void hb_gcMark(void *pAlloc);
// increment reference counter
extern HB_EXPORT void hb_gcRefInc(void *pAlloc);
// decrement reference counter and free the block when 0 reached
extern HB_EXPORT void hb_gcRefFree(void *pAlloc);

// dummy GC clear function
extern HB_EXPORT void hb_gcDummyClear(void *Cargo);
// dummy GC mark function
extern HB_EXPORT void hb_gcDummyMark(void *Cargo);

extern PHB_ITEM hb_gcGripGet(PHB_ITEM pItem);
extern void hb_gcGripDrop(PHB_ITEM pItem);

#ifdef _HB_API_INTERNAL_
// return cleanup function pointer
extern const HB_GC_FUNCS *hb_gcFuncs(void *pBlock);
extern void hb_gcAttach(void *pBlock);
// allocates a memory controlled by the garbage collector
extern void *hb_gcAllocRaw(HB_SIZE nSize, const HB_GC_FUNCS *pFuncs);
// mark complex variables inside given item as used
extern void hb_gcGripMark(void *Cargo);
// mark complex variables inside given item as used
extern void hb_gcItemRef(PHB_ITEM pItem);
// hvm.c - mark all local variables as used
extern void hb_vmIsStackRef(void);
// hvm.c - mark all static variables as used
extern void hb_vmIsStaticRef(void);
// release all memory blocks unconditionally
extern void hb_gcReleaseAll(void);

// return number of references
extern HB_COUNTER hb_gcRefCount(void *pAlloc);

#if 0
#define hb_gcRefInc(p)      hb_xRefInc(HB_GC_PTR(p))
#define hb_gcRefCount(p)    hb_xRefCount(HB_GC_PTR(p))
#define hb_gcFunc(p)        (HB_GC_PTR(p)->pFunc)
#endif

#endif // _HB_API_INTERNAL_
// checks if a single memory block can be released
extern HB_EXPORT void hb_gcCollect(void);
// checks if all memory blocks can be released
extern HB_EXPORT void hb_gcCollectAll(HB_BOOL fForce);

// Extend API
// Determine the param count or data type
extern HB_EXPORT HB_ULONG hb_parinfo(int iParam);
// retrieve length or element type of an array parameter
extern HB_EXPORT HB_SIZE hb_parinfa(int iParamNum, HB_SIZE nArrayIndex);
// retrieve a generic parameter
extern HB_EXPORT PHB_ITEM hb_param(int iParam, long lMask);
// Returns either the generic parameter or a NIL item if param not provided
extern HB_EXPORT PHB_ITEM hb_paramError(int iParam);
extern HB_EXPORT HB_BOOL hb_extIsNil(int iParam);
extern HB_EXPORT HB_BOOL hb_extIsArray(int iParam);
extern HB_EXPORT HB_BOOL hb_extIsObject(int iParam);

// retrieve a string parameter
extern HB_EXPORT const char *hb_parc(int iParam);
// retrieve a string parameter
extern HB_EXPORT const char *hb_parcx(int iParam);
// retrieve a string parameter length
extern HB_EXPORT HB_SIZE hb_parclen(int iParam);
// retrieve a by-reference string parameter length, including terminator
extern HB_EXPORT HB_SIZE hb_parcsiz(int iParam);
// retrieve a date as a string YYYYMMDD
extern HB_EXPORT const char *hb_pards(int iParam);
// retrieve a date as a string YYYYMMDD
extern HB_EXPORT char *hb_pardsbuff(char *szDate, int iParam);
// retrieve a date as a long integer
extern HB_EXPORT long hb_pardl(int iParam);
// retrieve a timestamp as a double number
extern HB_EXPORT double hb_partd(int iParam);
// retrieve a timestamp as two long numbers
extern HB_EXPORT HB_BOOL hb_partdt(long *plJulian, long *plMilliSec , int iParam);
// retrieve a logical parameter as an int
extern HB_EXPORT int hb_parl(int iParam);
// retrieve a logical parameter as an int, return default value if parameter isn't logical
extern HB_EXPORT int hb_parldef(int iParam, int iDefValue);
// retrieve a numeric parameter as a double
extern HB_EXPORT double hb_parnd(int iParam);
// retrieve a numeric parameter as a integer
extern HB_EXPORT int hb_parni(int iParam);
// retrieve a numeric parameter as a integer, return default value if parameter isn't numeric
extern HB_EXPORT int hb_parnidef(int iParam, int iDefValue);
// retrieve a numeric parameter as a long
extern HB_EXPORT long hb_parnl(int iParam);
// retrieve a numeric parameter as a long, return default value if parameter isn't numeric
extern HB_EXPORT long hb_parnldef(int iParam, long lDefValue);
// retrieve a numeric parameter as a HB_SIZE
extern HB_EXPORT HB_ISIZ hb_parns(int iParam);
// retrieve a numeric parameter as a HB_SIZE, return default value if parameter isn't numeric
extern HB_EXPORT HB_ISIZ hb_parnsdef(int iParam, HB_ISIZ nDefValue);
// retrieve a numeric parameter as a HB_MAXINT
extern HB_EXPORT HB_MAXINT hb_parnint(int iParam);
// retrieve a numeric parameter as a HB_MAXINT, return default value if parameter isn't numeric
extern HB_EXPORT HB_MAXINT hb_parnintdef(int iParam, HB_MAXINT nDefValue);
// retrieve a parameter as a pointer
extern HB_EXPORT void *hb_parptr(int iParam);
// retrieve a parameter (NIL, pointer, number and string) as a pointer
extern HB_EXPORT void *hb_parptrx(int iParam);
// retrieve a parameter as a pointer if it's a pointer to GC allocated block
extern HB_EXPORT void *hb_parptrGC(const HB_GC_FUNCS *pFuncs, int iParam);
#ifndef HB_LONG_LONG_OFF
// retrieve a numeric parameter as a long long
extern HB_EXPORT HB_LONGLONG hb_parnll(int iParam);
#endif

// retrieve a string parameter
extern HB_EXPORT const char *hb_parvc(int iParam, ...);
// retrieve a string parameter
extern HB_EXPORT const char *hb_parvcx(int iParam, ...);
// retrieve a string parameter length
extern HB_EXPORT HB_SIZE hb_parvclen(int iParam, ...);
// retrieve a by-reference string parameter length, including terminator
extern HB_EXPORT HB_SIZE hb_parvcsiz(int iParam, ...);
// retrieve a date as a string YYYYMMDD
extern HB_EXPORT const char *hb_parvds(int iParam, ...);
// retrieve a date as a string YYYYMMDD
extern HB_EXPORT char *hb_parvdsbuff(char *szDate, int iParam, ...);
// retrieve a date as a long integer
extern HB_EXPORT long hb_parvdl(int iParam, ...);
// retrieve a timestamp as a double number
extern HB_EXPORT double hb_parvtd(int iParam, ...);
// retrieve a timestamp as two long numbers
extern HB_EXPORT HB_BOOL hb_parvtdt(long *plJulian, long *plMilliSec , int iParam, ...);
// retrieve a logical parameter as an int
extern HB_EXPORT int hb_parvl(int iParam, ...);
// retrieve a numeric parameter as a double
extern HB_EXPORT double hb_parvnd(int iParam, ...);
// retrieve a numeric parameter as a integer
extern HB_EXPORT int hb_parvni(int iParam, ...);
// retrieve a numeric parameter as a long
extern HB_EXPORT long hb_parvnl(int iParam, ...);
// retrieve a numeric parameter as a HB_SIZE
extern HB_EXPORT HB_ISIZ hb_parvns(int iParam, ...);
// retrieve a numeric parameter as a HB_MAXINT
extern HB_EXPORT HB_MAXINT hb_parvnint(int iParam, ...);
// retrieve a parameter as a pointer
extern HB_EXPORT void *hb_parvptr(int iParam, ...);
// retrieve a parameter as a pointer if it's a pointer to GC allocated block
extern HB_EXPORT void *hb_parvptrGC(const HB_GC_FUNCS *pFuncs, int iParam, ...);
#ifndef HB_LONG_LONG_OFF
// retrieve a numeric parameter as a long long
extern HB_EXPORT HB_LONGLONG hb_parvnll(int iParam, ...);
#endif

// returns the number of supplied parameters
extern HB_EXPORT int hb_pcount(void);
// post a NIL return value
extern HB_EXPORT void hb_ret(void);
// returns a string
extern HB_EXPORT void hb_retc(const char *szText);
// returns an empty string
extern HB_EXPORT void hb_retc_null(void);
// same as above, but accepts an allocated buffer
extern HB_EXPORT void hb_retc_buffer(char *szText);
// returns a string as a pcode based string
extern HB_EXPORT void hb_retc_const(const char *szText);
// returns a string with a specific length
extern HB_EXPORT void hb_retclen(const char *szText, HB_SIZE nLen);
// same as above, but accepts an allocated buffer
extern HB_EXPORT void hb_retclen_buffer(char *szText, HB_SIZE nLen);
// returns a string with a specific length formed from a constant buffer
extern HB_EXPORT void hb_retclen_const(const char *szText, HB_SIZE nLen);
// returns a date, must use YYYYMMDD format
extern HB_EXPORT void hb_retds(const char *szDate);
// returns a date
extern HB_EXPORT void hb_retd(int iYear, int iMonth, int iDay);
// returns a long value as a Julian date
extern HB_EXPORT void hb_retdl(long lJulian);
// returns a double value as a timestamp
extern HB_EXPORT void hb_rettd(double dTimeStamp);
// returns two long values as a timestamp
extern HB_EXPORT void hb_rettdt(long lJulian, long lMilliSec);
// returns a logical integer
extern HB_EXPORT void hb_retl(int iTrueFalse);
// returns a double
extern HB_EXPORT void hb_retnd(double dNumber);
// returns a integer number
extern HB_EXPORT void hb_retni(int iNumber);
// returns a long number
extern HB_EXPORT void hb_retnl(long lNumber);
// returns a size
extern HB_EXPORT void hb_retns(HB_ISIZ nNumber);
// returns a long number
extern HB_EXPORT void hb_retnint(HB_MAXINT nNumber);
// returns a double, with specific width and decimals
extern HB_EXPORT void hb_retnlen(double dNumber, int iWidth, int iDec);
// returns a double, with specific width and decimals
extern HB_EXPORT void hb_retndlen(double dNumber, int iWidth, int iDec);
// returns a integer number, with specific width
extern HB_EXPORT void hb_retnilen(int iNumber, int iWidth);
// returns a long number, with specific width
extern HB_EXPORT void hb_retnllen(long lNumber, int iWidth);
// returns a long long number, with specific width
extern HB_EXPORT void hb_retnintlen(HB_MAXINT nNumber, int iWidth);
// returns an array with a specific length
extern HB_EXPORT void hb_reta(HB_SIZE nLen);
// returns a pointer
extern HB_EXPORT void hb_retptr(void *ptr);
// returns a pointer to an allocated memory, collected by GC
extern HB_EXPORT void hb_retptrGC(void *ptr);
#ifndef HB_LONG_LONG_OFF
// returns a long long number
extern HB_EXPORT void hb_retnll(HB_LONGLONG lNumber);
// returns a long long number, with specific width
extern HB_EXPORT void hb_retnlllen(HB_LONGLONG lNumber, int iWidth);
#endif

#define HB_IS_VALID_INDEX(idx, max)  ((idx) > 0 && static_cast<HB_SIZE>(idx) <= (max))

#ifdef _HB_API_MACROS_

#define hb_pcount()                          (static_cast<int>((hb_stackBaseItem())->item.asSymbol.paramcnt))

#define hb_ret()                             hb_itemClear(hb_stackReturnItem())
#define hb_reta(nLen)                        hb_arrayNew(hb_stackReturnItem(), nLen)
#define hb_retc(szText)                      hb_itemPutC(hb_stackReturnItem(), szText)
#define hb_retc_null()                       hb_itemPutC(hb_stackReturnItem(), nullptr)
#define hb_retc_buffer(szText)               hb_itemPutCPtr(hb_stackReturnItem(), szText)
#define hb_retc_const(szText)                hb_itemPutCConst(hb_stackReturnItem(), szText)
#define hb_retclen(szText, nLen)             hb_itemPutCL(hb_stackReturnItem(), szText, nLen)
#define hb_retclen_buffer(szText, nLen)      hb_itemPutCLPtr(hb_stackReturnItem(), szText, nLen)
#define hb_retclen_const(szText, nLen)       hb_itemPutCLConst(hb_stackReturnItem(), szText, nLen)
#define hb_retds(szDate)                     hb_itemPutDS(hb_stackReturnItem(), szDate)
#define hb_retd(iYear, iMonth, iDay)         hb_itemPutD(hb_stackReturnItem(), iYear, iMonth, iDay)
#define hb_retdl(lJulian)                    hb_itemPutDL(hb_stackReturnItem(), lJulian)
#define hb_rettd(dTimeStamp)                 hb_itemPutTD(hb_stackReturnItem(), dTimeStamp)
#define hb_rettdt(lJulian, lMilliSec)        hb_itemPutTDT(hb_stackReturnItem(), lJulian, lMilliSec)
#define hb_retl(iLogical)                    hb_itemPutL(hb_stackReturnItem(), (iLogical) ? HB_TRUE : HB_FALSE)
#define hb_retnd(dNumber)                    hb_itemPutND(hb_stackReturnItem(), dNumber)
#define hb_retni(iNumber)                    hb_itemPutNI(hb_stackReturnItem(), iNumber)
#define hb_retnl(lNumber)                    hb_itemPutNL(hb_stackReturnItem(), lNumber)
#define hb_retns(nNumber)                    hb_itemPutNS(hb_stackReturnItem(), nNumber)
#define hb_retnll(lNumber)                   hb_itemPutNLL(hb_stackReturnItem(), lNumber)
#define hb_retnlen(dNumber, iWidth, iDec)    hb_itemPutNLen(hb_stackReturnItem(), dNumber, iWidth, iDec)
#define hb_retndlen(dNumber, iWidth, iDec)   hb_itemPutNDLen(hb_stackReturnItem(), dNumber, iWidth, iDec)
#define hb_retnilen(iNumber, iWidth)         hb_itemPutNILen(hb_stackReturnItem(), iNumber, iWidth)
#define hb_retnllen(lNumber, iWidth)         hb_itemPutNLLen(hb_stackReturnItem(), lNumber, iWidth)
#define hb_retnlllen(lNumber, iWidth)        hb_itemPutNLLLen(hb_stackReturnItem(), lNumber, iWidth)
#define hb_retnint(iNumber)                  hb_itemPutNInt(hb_stackReturnItem(), iNumber)
#define hb_retnintlen(lNumber, iWidth)       hb_itemPutNIntLen(hb_stackReturnItem(), lNumber, iWidth)
#define hb_retptr(pointer)                   hb_itemPutPtr(hb_stackReturnItem(), pointer)
#define hb_retptrGC(pointer)                 hb_itemPutPtrGC(hb_stackReturnItem(), pointer)

#endif // _HB_API_MACROS_

// stores a NIL on a variable by reference
extern HB_EXPORT int hb_stor(int iParam);
// stores a szString on a variable by reference
extern HB_EXPORT int hb_storc(const char *szText, int iParam);
// stores a fixed length string on a variable by reference
extern HB_EXPORT int hb_storclen(const char *szText, HB_SIZE nLength, int iParam);
// stores a fixed length string buffer on a variable by reference
extern HB_EXPORT int hb_storclen_buffer(char *szText, HB_SIZE nLength, int iParam);
// szDate must have YYYYMMDD format
extern HB_EXPORT int hb_stords(const char *szDate, int iParam);
// lJulian must be a date in Julian format
extern HB_EXPORT int hb_stordl(long lJulian, int iParam);
// stores a double value as timestamp on a variable by reference
extern HB_EXPORT int hb_stortd(double dTimeStamp, int iParam);
// stores two long values as timestamp on a variable by reference
extern HB_EXPORT int hb_stortdt(long lJulian, long lMilliSec, int iParam);
// stores a logical integer on a variable by reference
extern HB_EXPORT int hb_storl(int iLogical, int iParam);
// stores an integer on a variable by reference
extern HB_EXPORT int hb_storni(int iValue, int iParam);
// stores a long on a variable by reference
extern HB_EXPORT int hb_stornl(long lValue, int iParam);
// stores a HB_SIZE on a variable by reference
extern HB_EXPORT int hb_storns(HB_ISIZ nValue, int iParam);
// stores a double on a variable by reference
extern HB_EXPORT int hb_stornd(double dValue, int iParam);
// stores a HB_MAXINT on a variable by reference
extern HB_EXPORT int hb_stornint(HB_MAXINT nValue, int iParam);
// stores a pointer on a variable by reference
extern HB_EXPORT int hb_storptr(void *pointer, int iParam);
// stores a pointer to GC block on a variable by reference
extern HB_EXPORT int hb_storptrGC(void *pointer, int iParam);
#ifndef HB_LONG_LONG_OFF
// stores a long long on a variable by reference
extern HB_EXPORT int hb_stornll(HB_LONGLONG llValue, int iParam);
#endif

// stores a szString on a variable by reference
extern HB_EXPORT int hb_storvc(const char *szText, int iParam, ...);
// stores a fixed length string on a variable by reference
extern HB_EXPORT int hb_storvclen(const char *szText, HB_SIZE nLength, int iParam, ...);
// stores a fixed length string buffer on a variable by reference
extern HB_EXPORT int hb_storvclen_buffer(char *szText, HB_SIZE nLength, int iParam, ...);
// szDate must have YYYYMMDD format
extern HB_EXPORT int hb_storvds(const char *szDate, int iParam, ...);
// lJulian must be a date in Julian format
extern HB_EXPORT int hb_storvdl(long lJulian, int iParam, ...);
// stores a double value as timestamp on a variable by reference
extern HB_EXPORT int hb_storvtd(double dTimeStamp, int iParam, ...);
// stores two long values as timestamp on a variable by reference
extern HB_EXPORT int hb_storvtdt(long lJulian, long lMilliSec, int iParam, ...);
// stores a logical integer on a variable by reference
extern HB_EXPORT int hb_storvl(int iLogical, int iParam, ...);
// stores an integer on a variable by reference
extern HB_EXPORT int hb_storvni(int iValue, int iParam, ...);
// stores a long on a variable by reference
extern HB_EXPORT int hb_storvnl(long lValue, int iParam, ...);
// stores a HB_SIZE on a variable by reference
extern HB_EXPORT int hb_storvns(HB_ISIZ nValue, int iParam, ...);
// stores a double on a variable by reference
extern HB_EXPORT int hb_storvnd(double dValue, int iParam, ...);
// stores a HB_MAXINT on a variable by reference
extern HB_EXPORT int hb_storvnint(HB_MAXINT nValue, int iParam, ...);
// stores a pointer on a variable by reference
extern HB_EXPORT int hb_storvptr(void *pointer, int iParam, ...);
// stores a pointer to GC block on a variable by reference
extern HB_EXPORT int hb_storvptrGC(void *pointer, int iParam, ...);
#ifndef HB_LONG_LONG_OFF
// stores a long long on a variable by reference
extern HB_EXPORT int hb_storvnll(HB_LONGLONG llValue, int iParam, ...);
#endif

// array management
// creates a new array
extern HB_EXPORT HB_BOOL hb_arrayNew(PHB_ITEM pItem, HB_SIZE nLen);
// retrieves the array length
extern HB_EXPORT HB_SIZE hb_arrayLen(PHB_ITEM pArray);
// retrieves if the array is an object
extern HB_EXPORT HB_BOOL hb_arrayIsObject(PHB_ITEM pArray);
// retrieves the array unique ID
extern HB_EXPORT void *hb_arrayId(PHB_ITEM pArray);
// retrieves numer of references to the array
extern HB_EXPORT HB_COUNTER hb_arrayRefs(PHB_ITEM pArray);
//
extern HB_EXPORT PHB_ITEM hb_arrayFromId(PHB_ITEM pItem, void *pArrayId);
// add a new item to the end of an array item
extern HB_EXPORT HB_BOOL hb_arrayAdd(PHB_ITEM pArray, PHB_ITEM pItemValue);
// add a new item to the end of an array item with no incrementing of reference counters
extern HB_EXPORT HB_BOOL hb_arrayAddForward(PHB_ITEM pArray, PHB_ITEM pValue);
// insert a nil item into an array, without changing the length
extern HB_EXPORT HB_BOOL hb_arrayIns(PHB_ITEM pArray, HB_SIZE nIndex);
// delete an array item, without changing length
extern HB_EXPORT HB_BOOL hb_arrayDel(PHB_ITEM pArray, HB_SIZE nIndex);
// sets the array total length
extern HB_EXPORT HB_BOOL hb_arraySize(PHB_ITEM pArray, HB_SIZE nLen);
// retrieve last item in an array
extern HB_EXPORT HB_BOOL hb_arrayLast(PHB_ITEM pArray, PHB_ITEM pResult);
// retrieves an item
extern HB_EXPORT HB_BOOL hb_arrayGet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem);
// create a reference to an array element
extern HB_EXPORT HB_BOOL hb_arrayGetItemRef(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem);
// returns pointer to specified element of the array
// WARNING: hb_arrayGetItemPtr() is dangerous, be sure that base ARRAY value will not be changed (f.e. resized)
extern HB_EXPORT PHB_ITEM hb_arrayGetItemPtr(PHB_ITEM pArray, HB_SIZE nIndex);
// copy a string from an array item
extern HB_EXPORT HB_SIZE hb_arrayCopyC(PHB_ITEM pArray, HB_SIZE nIndex, char *szBuffer, HB_SIZE nLen);
// retrieves the string contained on an array element
extern HB_EXPORT char *hb_arrayGetC(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the string pointer on an array element
extern HB_EXPORT const char *hb_arrayGetCPtr(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the string length contained on an array element
extern HB_EXPORT HB_SIZE hb_arrayGetCLen(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the pointer contained on an array element
extern HB_EXPORT void *hb_arrayGetPtr(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the GC pointer contained on an array element
extern HB_EXPORT void *hb_arrayGetPtrGC(PHB_ITEM pArray, HB_SIZE nIndex, const HB_GC_FUNCS *pFuncs);
// retrieves symbol contained on an array element
extern HB_EXPORT PHB_SYMB hb_arrayGetSymbol(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the logical value contained on an array element
extern HB_EXPORT HB_BOOL hb_arrayGetL(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the int value contained on an array element
extern HB_EXPORT int hb_arrayGetNI(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the long numeric value contained on an array element
extern HB_EXPORT long hb_arrayGetNL(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the HB_SIZE value contained on an array element
extern HB_EXPORT HB_ISIZ hb_arrayGetNS(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the HB_MAXINT value contained on an array element
extern HB_EXPORT HB_MAXINT hb_arrayGetNInt(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the double value contained on an array element
extern HB_EXPORT double hb_arrayGetND(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the date value contained in an array element
extern HB_EXPORT char *hb_arrayGetDS(PHB_ITEM pArray, HB_SIZE nIndex, char *szDate);
// retrieves the date value contained in an array element, as a long integer
extern HB_EXPORT long hb_arrayGetDL(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the timestamp value contained in an array element, as a double value
extern HB_EXPORT double hb_arrayGetTD(PHB_ITEM pArray, HB_SIZE nIndex);
// retrieves the timestamp value contained in an array element, as two long values
extern HB_EXPORT HB_BOOL hb_arrayGetTDT(PHB_ITEM pArray, HB_SIZE nIndex, long *plJulian, long *plMilliSec);
// retrieves the type of an array item
extern HB_EXPORT HB_TYPE hb_arrayGetType(PHB_ITEM pArray, HB_SIZE nIndex);
// sets an array element
extern HB_EXPORT HB_BOOL hb_arraySet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem);
// sets an array element by forwarding it's value
extern HB_EXPORT HB_BOOL hb_arraySetForward(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem);
extern HB_EXPORT HB_BOOL hb_arraySetDS(PHB_ITEM pArray, HB_SIZE nIndex, const char *szDate);
extern HB_EXPORT HB_BOOL hb_arraySetDL(PHB_ITEM pArray, HB_SIZE nIndex, long lDate);
extern HB_EXPORT HB_BOOL hb_arraySetTD(PHB_ITEM pArray, HB_SIZE nIndex, double dTimeStamp);
extern HB_EXPORT HB_BOOL hb_arraySetTDT(PHB_ITEM pArray, HB_SIZE nIndex, long lJulian, long lMilliSec);
extern HB_EXPORT HB_BOOL hb_arraySetL(PHB_ITEM pArray, HB_SIZE nIndex, HB_BOOL fValue);
extern HB_EXPORT HB_BOOL hb_arraySetNI(PHB_ITEM pArray, HB_SIZE nIndex, int iNumber);
extern HB_EXPORT HB_BOOL hb_arraySetNL(PHB_ITEM pArray, HB_SIZE nIndex, long lNumber);
extern HB_EXPORT HB_BOOL hb_arraySetNS(PHB_ITEM pArray, HB_SIZE nIndex, HB_ISIZ nNumber);
#ifndef HB_LONG_LONG_OFF
extern HB_EXPORT HB_BOOL hb_arraySetNLL(PHB_ITEM pArray, HB_SIZE nIndex, HB_LONGLONG llNumber);
#endif
extern HB_EXPORT HB_BOOL hb_arraySetNInt(PHB_ITEM pArray, HB_SIZE nIndex, HB_MAXINT nNumber);
extern HB_EXPORT HB_BOOL hb_arraySetND(PHB_ITEM pArray, HB_SIZE nIndex, double dNumber);
extern HB_EXPORT HB_BOOL hb_arraySetC(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText);
extern HB_EXPORT HB_BOOL hb_arraySetCL(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText, HB_SIZE nLen);
extern HB_EXPORT HB_BOOL hb_arraySetCPtr(PHB_ITEM pArray, HB_SIZE nIndex, char *szText);
extern HB_EXPORT HB_BOOL hb_arraySetCLPtr(PHB_ITEM pArray, HB_SIZE nIndex, char *szText, HB_SIZE nLen);
extern HB_EXPORT HB_BOOL hb_arraySetCConst(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText);
extern HB_EXPORT HB_BOOL hb_arraySetPtr(PHB_ITEM pArray, HB_SIZE nIndex, void *pValue);
extern HB_EXPORT HB_BOOL hb_arraySetPtrGC(PHB_ITEM pArray, HB_SIZE nIndex, void *pValue);
extern HB_EXPORT HB_BOOL hb_arraySetSymbol(PHB_ITEM pArray, HB_SIZE nIndex, PHB_SYMB pSymbol);
// fill an array with a given item
extern HB_EXPORT HB_BOOL hb_arrayFill(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount);
// scan an array for a given item, or until code-block item returns HB_TRUE
extern HB_EXPORT HB_SIZE hb_arrayScan(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_BOOL fExact);
// scan an array for a given item, or until code-block item returns HB_TRUE in reverted order
extern HB_EXPORT HB_SIZE hb_arrayRevScan(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_BOOL fExact);
// execute a code-block for every element of an array item
extern HB_EXPORT HB_BOOL hb_arrayEval(PHB_ITEM pArray, PHB_ITEM bBlock, HB_SIZE *pnStart, HB_SIZE *pnCount);
// copy items from one array to another
extern HB_EXPORT HB_BOOL hb_arrayCopy(PHB_ITEM pSrcArray, PHB_ITEM pDstArray, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_SIZE *pnTarget);
// returns a duplicate of an existing array, including all nested items
extern HB_EXPORT PHB_ITEM hb_arrayClone(PHB_ITEM pArray);
// returns a duplicate of an existing array, including all nested items
extern HB_EXPORT PHB_ITEM hb_arrayCloneTo(PHB_ITEM pDest, PHB_ITEM pArray);
// sorts an array item
extern HB_EXPORT HB_BOOL hb_arraySort(PHB_ITEM pArray, HB_SIZE *pnStart, HB_SIZE *pnCount, PHB_ITEM pBlock);
// Creates and returns an Array of n Elements from the Eval Stack - Does NOT pop the items.
extern HB_EXPORT PHB_ITEM hb_arrayFromStack(HB_USHORT uiLen);
// Creates and returns an Array of Generic Parameters for a given call level
extern HB_EXPORT PHB_ITEM hb_arrayFromParams(int iLevel);
// Creates and returns an Array of Generic Parameters for current base symbol.
extern HB_EXPORT PHB_ITEM hb_arrayBaseParams(void);
// Creates and returns an Array of Generic Parameters for current base symbol with self item
extern HB_EXPORT PHB_ITEM hb_arraySelfParams(void);
#ifndef HB_LONG_LONG_OFF
// retrieves the long long numeric value contained on an array element
extern HB_EXPORT HB_LONGLONG hb_arrayGetNLL(PHB_ITEM pArray, HB_SIZE nIndex);
#endif
#ifdef _HB_API_INTERNAL_
// internal array API not exported
extern void hb_arrayPushBase(PHB_BASEARRAY pBaseArray);
extern void hb_arraySwap(PHB_ITEM pArray1, PHB_ITEM pArray2);
extern void hb_nestedCloneInit(PHB_NESTED_CLONED pClonedList, void *pValue, PHB_ITEM pDest);
extern void hb_nestedCloneFree(PHB_NESTED_CLONED pClonedList);
extern void hb_nestedCloneDo(PHB_ITEM pDstItem, PHB_ITEM pSrcItem, PHB_NESTED_CLONED pClonedList);
extern void hb_hashCloneBody(PHB_ITEM pDest, PHB_ITEM pHash, PHB_NESTED_CLONED pClonedList);
#endif

// hash management
extern HB_EXPORT PHB_ITEM hb_hashNew(PHB_ITEM pItem);
extern HB_EXPORT HB_SIZE hb_hashLen(PHB_ITEM pHash);
extern HB_EXPORT HB_BOOL hb_hashDel(PHB_ITEM pHash, PHB_ITEM pKey);
extern HB_EXPORT HB_BOOL hb_hashAdd(PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue);
extern HB_EXPORT HB_BOOL hb_hashAddNew(PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue);
extern HB_EXPORT HB_BOOL hb_hashRemove(PHB_ITEM pHash, PHB_ITEM pItem);
extern HB_EXPORT HB_BOOL hb_hashClear(PHB_ITEM pHash);
extern HB_EXPORT HB_BOOL hb_hashAllocNewPair(PHB_ITEM pHash, PHB_ITEM *pKeyPtr, PHB_ITEM *pValPtr);
extern HB_EXPORT void hb_hashSort(PHB_ITEM pHash);
extern HB_EXPORT PHB_ITEM hb_hashClone(PHB_ITEM pHash);
extern HB_EXPORT PHB_ITEM hb_hashCloneTo(PHB_ITEM pDest, PHB_ITEM pHash);
extern HB_EXPORT void hb_hashJoin(PHB_ITEM pDest, PHB_ITEM pSource, int iType);
extern HB_EXPORT HB_BOOL hb_hashScan(PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE *pnPos);
extern HB_EXPORT HB_BOOL hb_hashScanSoft(PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE *pnPos);
extern HB_EXPORT void hb_hashPreallocate(PHB_ITEM pHash, HB_SIZE nNewSize);
extern HB_EXPORT PHB_ITEM hb_hashGetKeys(PHB_ITEM pHash);
extern HB_EXPORT PHB_ITEM hb_hashGetValues(PHB_ITEM pHash);
extern HB_EXPORT void hb_hashSetDefault(PHB_ITEM pHash, PHB_ITEM pValue);
extern HB_EXPORT PHB_ITEM hb_hashGetDefault(PHB_ITEM pHash);
extern HB_EXPORT void hb_hashSetFlags(PHB_ITEM pHash, int iFlags);
extern HB_EXPORT void hb_hashClearFlags(PHB_ITEM pHash, int iFlags);
extern HB_EXPORT int hb_hashGetFlags(PHB_ITEM pHash);
// retrieves the hash unique ID
extern HB_EXPORT void *hb_hashId(PHB_ITEM pHash);
// retrieves numer of references to the hash
extern HB_EXPORT HB_COUNTER hb_hashRefs(PHB_ITEM pHash);

// WARNING: these hb_hashGet*() functions are dangerous, be sure that base HASH value will not be changed
extern HB_EXPORT PHB_ITEM hb_hashGetItemPtr(PHB_ITEM pHash, PHB_ITEM pKey, int iFlags);
extern HB_EXPORT PHB_ITEM hb_hashGetItemRefPtr(PHB_ITEM pHash, PHB_ITEM pKey);
extern HB_EXPORT PHB_ITEM hb_hashGetCItemPtr(PHB_ITEM pHash, const char *pszKey);
extern HB_EXPORT HB_SIZE hb_hashGetCItemPos(PHB_ITEM pHash, const char *pszKey);
extern HB_EXPORT PHB_ITEM hb_hashGetKeyAt(PHB_ITEM pHash, HB_SIZE nPos);
extern HB_EXPORT PHB_ITEM hb_hashGetValueAt(PHB_ITEM pHash, HB_SIZE nPos);

extern HB_EXPORT HB_BOOL hb_hashDelAt(PHB_ITEM pHash, HB_SIZE nPos);

#if defined(__cplusplus)

// hash item flags
constexpr int HB_HASH_AUTOADD_NEVER = 0x00;
constexpr int HB_HASH_AUTOADD_ACCESS = 0x01;
constexpr int HB_HASH_AUTOADD_ASSIGN = 0x02;
constexpr int HB_HASH_AUTOADD_ALWAYS = (HB_HASH_AUTOADD_ACCESS | HB_HASH_AUTOADD_ASSIGN);
constexpr int HB_HASH_AUTOADD_REFERENCE = HB_HASH_AUTOADD_ALWAYS;
constexpr int HB_HASH_AUTOADD_MASK = 0x03;

constexpr int HB_HASH_RESORT = 0x08;

constexpr int HB_HASH_IGNORECASE = 0x10;
constexpr int HB_HASH_BINARY = 0x20;
constexpr int HB_HASH_KEEPORDER = 0x40;

constexpr int HB_HASH_FLAG_MASK = 0xFFFF;
constexpr int HB_HASH_FLAG_DEFAULT = (HB_HASH_AUTOADD_ASSIGN | HB_HASH_BINARY | HB_HASH_KEEPORDER);

constexpr int HB_HASH_UNION = 0; // logical OR  on items in two hash tables
constexpr int HB_HASH_INTERSECT = 1; // logical AND on items in two hash tables
constexpr int HB_HASH_DIFFERENCE = 2; // logical XOR on items in two hash tables
constexpr int HB_HASH_REMOVE = 3; // h1 & (h1 ^ h2)

#else

// hash item flags
#define HB_HASH_AUTOADD_NEVER       0x00
#define HB_HASH_AUTOADD_ACCESS      0x01
#define HB_HASH_AUTOADD_ASSIGN      0x02
#define HB_HASH_AUTOADD_ALWAYS      (HB_HASH_AUTOADD_ACCESS | HB_HASH_AUTOADD_ASSIGN)
#define HB_HASH_AUTOADD_REFERENCE   HB_HASH_AUTOADD_ALWAYS
#define HB_HASH_AUTOADD_MASK        0x03

#define HB_HASH_RESORT              0x08

#define HB_HASH_IGNORECASE          0x10
#define HB_HASH_BINARY              0x20
#define HB_HASH_KEEPORDER           0x40

#define HB_HASH_FLAG_MASK           0xFFFF
#define HB_HASH_FLAG_DEFAULT        (HB_HASH_AUTOADD_ASSIGN | HB_HASH_BINARY | HB_HASH_KEEPORDER)

#define HB_HASH_UNION      0 // logical OR  on items in two hash tables
#define HB_HASH_INTERSECT  1 // logical AND on items in two hash tables
#define HB_HASH_DIFFERENCE 2 // logical XOR on items in two hash tables
#define HB_HASH_REMOVE     3 // h1 & (h1 ^ h2)

#endif

// string management

extern const char * const hb_szAscii[256]; // array of 1 character length strings

// compare two strings without regards to case
extern HB_EXPORT int hb_stricmp(const char *s1, const char *s2);
// compare two string without regards to case, limited by length
extern HB_EXPORT int hb_strnicmp(const char *s1, const char *s2, HB_SIZE nLen);
// convert a string in-place to upper-case
extern HB_EXPORT char *hb_strupr(char *pszText);
// convert a string in-place to lower-case
extern HB_EXPORT char *hb_strlow(char *pszText);
// returns a pointer to a newly allocated copy of the source string
extern HB_EXPORT char *hb_strdup(const char *pszText);
// returns a pointer to a newly allocated copy of the source string not longer then nLen
extern HB_EXPORT char *hb_strndup(const char *pszText, HB_SIZE nLen);
// returns a pointer to a newly allocated copy of the trimmed source string
extern HB_EXPORT char *hb_strduptrim(const char *pszText);
// like strlen() but result is the length of trimmed text
extern HB_EXPORT HB_SIZE hb_strlentrim(const char *pszText);
// like strlen() but result is limited to nLen
extern HB_EXPORT HB_SIZE hb_strnlen(const char *pszText, HB_SIZE nLen);
// Concatenates multiple strings into a single result
extern HB_EXPORT char *hb_xstrcat(char *dest, const char *src, ...);
// Concatenates multiple strings into a single result
extern HB_EXPORT char *hb_xstrcpy(char *szDest, const char *szSrc, ...);
// converts string to number, sets iDec, iWidth and returns HB_TRUE if results is double, used by compiler
extern HB_EXPORT HB_BOOL hb_compStrToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *pnVal, double *pdVal, int *piDec, int *piWidth);
// converts string to number, sets iDec, iWidth and returns HB_TRUE if results is double, used by Val()
extern HB_EXPORT HB_BOOL hb_valStrnToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *pnVal, double *pdVal, int *piDec, int *piWidth);
// converts string to number, returns HB_TRUE if results is double
extern HB_EXPORT HB_BOOL hb_strToNum(const char *szNum, HB_MAXINT *pnVal, double *pdVal);
// converts string to number, returns HB_TRUE if results is double
extern HB_EXPORT HB_BOOL hb_strnToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *pnVal, double *pdVal);
// snprintf() equivalent
extern HB_EXPORT int hb_snprintf(char *buffer, size_t bufsize, const char *format, ...) HB_PRINTF_FORMAT(3, 4);
// vsnprintf() equivalent
extern HB_EXPORT int hb_vsnprintf(char *buffer, size_t bufsize, const char *format, va_list ap);
extern HB_EXPORT int hb_printf_params(const char *format);

// compare two strings using platform dependent rules for file matching
extern HB_EXPORT HB_BOOL hb_strMatchFile(const char *pszString, const char *szPattern);
// compare two strings using a regular expression pattern
extern HB_EXPORT HB_BOOL hb_strMatchRegExp(const char *szString, const char *szPattern);
// compare two strings using pattern with wildcard (?*) - pattern have to be prefix of given string
extern HB_EXPORT HB_BOOL hb_strMatchWild(const char *szString, const char *szPattern);
// compare two strings using pattern with wildcard (?*) - pattern have to cover whole string
extern HB_EXPORT HB_BOOL hb_strMatchWildExact(const char *szString, const char *szPattern);
// compare two strings using pattern with wildcard (?*) ignoring the case of the characters - pattern have to cover whole string
extern HB_EXPORT HB_BOOL hb_strMatchCaseWildExact(const char *szString, const char *szPattern);
// returns whether a string contains only white space
extern HB_EXPORT HB_BOOL hb_strEmpty(const char *szText, HB_SIZE nLen);
// copy a string to a buffer, inverting each character
extern HB_EXPORT void hb_strDescend(char *szStringTo, const char *szStringFrom, HB_SIZE nLen);
// returns an index to a sub-string within another string
extern HB_EXPORT HB_SIZE hb_strAt(const char *szSub, HB_SIZE nSubLen, const char *szText, HB_SIZE nLen);
// returns an index to a sub-string within another, ignore the case of the characters
extern HB_EXPORT HB_SIZE hb_strAtI(const char *szSub, HB_SIZE nSubLen, const char *szText, HB_SIZE nLen);
extern HB_EXPORT HB_ISIZ hb_strAtTBM(const char *needle, HB_ISIZ m, const char *haystack, HB_ISIZ n);

// WARNING: this functions works only with byte oriented CPs
// convert an existing string buffer to upper case
extern HB_EXPORT char *hb_strUpper(char *szText, HB_SIZE nLen);
// convert an existing string buffer to lower case
extern HB_EXPORT char *hb_strLower(char *szText, HB_SIZE nLen);
extern HB_EXPORT HB_BOOL hb_charIsDigit(int iChar);
extern HB_EXPORT HB_BOOL hb_charIsAlpha(int iChar);
extern HB_EXPORT HB_BOOL hb_charIsLower(int iChar);
extern HB_EXPORT HB_BOOL hb_charIsUpper(int iChar);
// converts iChar to upper case
extern HB_EXPORT int hb_charUpper(int iChar);
// converts iChar to lower case
extern HB_EXPORT int hb_charLower(int iChar);

extern HB_EXPORT HB_BOOL hb_strIsDigit(const char *szChar);
extern HB_EXPORT HB_BOOL hb_strIsAlpha(const char *szChar);
extern HB_EXPORT HB_BOOL hb_strIsLower(const char *szChar);
extern HB_EXPORT HB_BOOL hb_strIsUpper(const char *szChar);
// copy at most nLen bytes from string buffer to another buffer and _always_ set 0 in destination buffer
extern HB_EXPORT char *hb_strncpy(char *pDest, const char *pSource, HB_SIZE nLen);
// copy at most nLen-strlen(pDest) bytes from string buffer to another buffer and _always_ set 0 in destination buffer
extern HB_EXPORT char *hb_strncat(char *pDest, const char *pSource, HB_SIZE nLen);
extern HB_EXPORT char *hb_strncpyTrim(char *pDest, const char *pSource, HB_SIZE nLen);
// copy an existing string buffer to another buffer, as lower case
extern HB_EXPORT char *hb_strncpyLower(char *pDest, const char *pSource, HB_SIZE nLen);
// copy an existing string buffer to another buffer, as upper case
extern HB_EXPORT char *hb_strncpyUpper(char *pDest, const char *pSource, HB_SIZE nLen);
extern HB_EXPORT char *hb_strncpyUpperTrim(char *pDest, const char *pSource, HB_SIZE nLen);
// return a pointer to the first non-white space character
extern HB_EXPORT const char *hb_strLTrim(const char *szText, HB_SIZE *nLen);
// return length of a string, ignoring trailing white space (or true spaces)
extern HB_EXPORT HB_SIZE hb_strRTrimLen(const char *szText, HB_SIZE nLen, HB_BOOL bAnySpace);
// return the numeric value of a character string representation of a number
extern HB_EXPORT double hb_strVal(const char *szText, HB_SIZE nLen);
extern HB_EXPORT HB_MAXINT hb_strValInt(const char *szText, int *iOverflow);
// remove C ESC sequences and converts them to Clipper chars
extern HB_EXPORT char *hb_strRemEscSeq(char *szText, HB_SIZE *nLen);
extern HB_EXPORT char *hb_numToStr(char *szBuf, HB_SIZE nSize, HB_MAXINT nNumber);
extern HB_EXPORT char *hb_dblToStr(char *szBuf, HB_SIZE nSize, double dNumber, int iMaxDec);
// round a number to a specific number of digits
extern HB_EXPORT double hb_numRound(double dResult, int iDec);
// take the integer part of the number
extern HB_EXPORT double hb_numInt(double dNum);
extern HB_EXPORT void hb_random_seed(HB_I32 seed);
extern HB_EXPORT double hb_random_num(void);
extern HB_EXPORT double hb_random_num_secure(void);
extern HB_EXPORT void hb_random_block(void *data, HB_SIZE len);
extern HB_EXPORT double hb_numDecConv(double dNum, int iDec);
extern HB_EXPORT double hb_numExpConv(double dNum, int iDec);
extern HB_EXPORT void hb_strtohex(const char *pSource, HB_SIZE size, char *pDest);

extern HB_EXPORT PHB_ITEM  hb_strFormat(PHB_ITEM pItemReturn, PHB_ITEM pItemFormat, int iCount, PHB_ITEM *pItemArray);

// architecture dependent number conversions
extern HB_EXPORT void hb_put_ieee754(HB_BYTE *ptr, double d);
extern HB_EXPORT double hb_get_ieee754(const HB_BYTE *ptr);
extern HB_EXPORT void hb_put_ord_ieee754(HB_BYTE *ptr, double d);
extern HB_EXPORT double hb_get_ord_ieee754(const HB_BYTE *ptr);
extern HB_EXPORT double hb_get_rev_double(const HB_BYTE *ptr);
extern HB_EXPORT double hb_get_std_double(const HB_BYTE *ptr);

#if defined(HB_LONG_LONG_OFF)
extern HB_EXPORT double hb_get_le_int64(const HB_BYTE *ptr);
extern HB_EXPORT double hb_get_le_uint64(const HB_BYTE *ptr);
extern HB_EXPORT void hb_put_le_uint64(const HB_BYTE *ptr, double d);
#endif

// dynamic symbol table management
// finds and creates a dynamic symbol if not found
extern HB_EXPORT PHB_DYNS hb_dynsymGet(const char *szName);
// finds and creates a dynamic symbol if not found - case sensitive
extern HB_EXPORT PHB_DYNS hb_dynsymGetCase(const char *szName);
// creates a new dynamic symbol based on a local one
extern HB_EXPORT PHB_DYNS hb_dynsymNew(PHB_SYMB pSymbol);
// finds a dynamic symbol
extern HB_EXPORT PHB_DYNS hb_dynsymFind(const char *szName);
// converts to uppercase and finds a dynamic symbol
extern HB_EXPORT PHB_DYNS hb_dynsymFindName(const char *szName);
// releases the memory of the dynamic symbol table
extern HB_EXPORT void hb_dynsymRelease(void);
// enumerates all dynamic symbols
extern HB_EXPORT void hb_dynsymEval(PHB_DYNS_FUNC pFunction, void *Cargo);
// enumerates all dynamic symbols with global symbol table locked - can be used ONLY when user function does not try to access dynamic symbol table
extern HB_EXPORT void hb_dynsymProtectEval(PHB_DYNS_FUNC pFunction, void *Cargo);
// finds and creates a dynamic symbol if not found and return pointer to its HB_SYMB structure
extern HB_EXPORT PHB_SYMB hb_dynsymGetSymbol(const char *szName);
// finds a dynamic symbol and return pointer to its HB_SYMB structure
extern HB_EXPORT PHB_SYMB hb_dynsymFindSymbol(const char *szName);
extern HB_EXPORT PHB_SYMB hb_dynsymSymbol(PHB_DYNS pDynSym);
// return dynamic symbol name
extern HB_EXPORT const char *hb_dynsymName(PHB_DYNS pDynSym);
extern HB_EXPORT HB_BOOL hb_dynsymIsFunction(PHB_DYNS pDynSym);
extern HB_EXPORT HB_BOOL hb_dynsymIsMemvar(PHB_DYNS pDynSym);
// return work area number bound with given dynamic symbol
extern HB_EXPORT int hb_dynsymAreaHandle(PHB_DYNS pDynSym);
// set work area number for a given dynamic symbol
extern HB_EXPORT void hb_dynsymSetAreaHandle(PHB_DYNS pDynSym, int iArea);
extern HB_EXPORT HB_SYMCNT hb_dynsymToNum(PHB_DYNS pDynSym);
extern HB_EXPORT PHB_DYNS hb_dynsymFromNum(HB_SYMCNT iSymNum);
#ifdef _HB_API_INTERNAL_
// return memvar handle number bound with given dynamic symbol
extern PHB_ITEM hb_dynsymGetMemvar(PHB_DYNS pDynSym);
// set memvar handle for a given dynamic symbol
extern void hb_dynsymSetMemvar(PHB_DYNS pDynSym, PHB_ITEM pMemvar);
// number of dynamic symbols
extern HB_LONG hb_dynsymCount(void);
#endif

// Symbol management
// create a new symbol
extern HB_EXPORT PHB_SYMB hb_symbolNew(const char *szName);

// Command-line and environment argument management
// initialize command-line argument API's
extern HB_EXPORT void hb_cmdargInit(int argc, char *argv[]);
// retrieve command-line argument count
extern HB_EXPORT int hb_cmdargARGC(void);
// retrieve command-line argument buffer pointer
extern HB_EXPORT char **hb_cmdargARGV(void);
// retrieve given command-line argument
extern HB_EXPORT const char *hb_cmdargARGVN(int argc);
// determine if a string is an internal setting
extern HB_EXPORT HB_BOOL hb_cmdargIsInternal(const char *szArg, int *piLen);
// return application name with path or NULL if not set, caller must free returned value with hb_xfree() if not NULL
extern HB_EXPORT char *hb_cmdargProgName(void);
// return application name without path or NULL if not set, caller must free returned value with hb_xfree() if not NULL
extern HB_EXPORT char *hb_cmdargBaseProgName(void);
// places application parameters on the HVM stack
extern int hb_cmdargPushArgs(void);
// update arguments after HVM initialization
extern void hb_cmdargUpdate(void);
// Check if a given internal switch (like //INFO) was set
extern HB_BOOL hb_cmdargCheck(const char *pszName);
// Returns the string value of an internal switch (like //GT:cgi)
extern char *hb_cmdargString(const char *pszName);
// Returns the numeric value of an internal switch (like //F:90)
extern int hb_cmdargNum(const char *pszName);
// Check for command-line internal arguments
extern void hb_cmdargProcess(void);
#if defined(HB_OS_WIN)
// Set WinMain() parameters
extern HB_EXPORT void hb_winmainArgInit(void *hInstance, void *hPrevInstance, int iCmdShow);
// Retrieve WinMain() parameters
extern HB_EXPORT HB_BOOL hb_winmainArgGet(void *phInstance, void *phPrevInstance, int *piCmdShow);
extern HB_EXPORT void hb_winmainArgVBuild(void);
extern HB_EXPORT void hb_winmainArgVFree(void);
#endif

// Codeblock management
// retrieves the codeblock unique ID
extern HB_EXPORT void *hb_codeblockId(PHB_ITEM pItem);
// retrieves numer of references to the codeblock
extern HB_EXPORT HB_COUNTER hb_codeblockRefs(PHB_ITEM pItem);
// create a code-block
extern PHB_CODEBLOCK hb_codeblockNew(const HB_BYTE *pBuffer, HB_USHORT uiLocals, const HB_BYTE *pLocalPosTable, PHB_SYMB pSymbols, HB_SIZE nLen);
extern PHB_CODEBLOCK hb_codeblockMacroNew(const HB_BYTE *pBuffer, HB_SIZE nLen);
// get local variable referenced in a codeblock
extern PHB_ITEM hb_codeblockGetVar(PHB_ITEM pItem, int iItemPos);
// get local variable passed by reference
extern PHB_ITEM hb_codeblockGetRef(PHB_CODEBLOCK pCBlock, int iItemPos);

// memvars subsystem
// clear all PUBLIC and PRIVATE variables optionally without GetList PUBLIC variable
extern void hb_memvarsClear(HB_BOOL fAll);
// copy an item into a symbol
extern HB_EXPORT void hb_memvarSetValue(PHB_SYMB pMemvarSymb, PHB_ITEM pItem);
// copy an symbol value into an item
extern HB_EXPORT HB_ERRCODE hb_memvarGet(PHB_ITEM pItem, PHB_SYMB pMemvarSymb);
// copy an symbol value into an item, with error trapping
extern void hb_memvarGetValue(PHB_ITEM pItem, PHB_SYMB pMemvarSymb);
// copy a reference to a symbol value into an item, with error trapping
extern void hb_memvarGetRefer(PHB_ITEM pItem, PHB_SYMB pMemvarSymb);
// retrieve current PRIVATE variables stack base
extern HB_SIZE hb_memvarGetPrivatesBase(void);
// release PRIVATE variables created after specified base
extern void hb_memvarSetPrivatesBase(HB_SIZE nBase);
// Update PRIVATE base offset so they will not be removed when function return
extern void hb_memvarUpdatePrivatesBase(void);
extern void hb_memvarNewParameter(PHB_SYMB pSymbol, PHB_ITEM pValue);
extern char *hb_memvarGetStrValuePtr(char *szVarName, HB_SIZE *pnLen);
extern void hb_memvarCreateFromItem(PHB_ITEM pMemvar, int iScope, PHB_ITEM pValue);
// retrieve scope of a dynamic variable symbol
extern int hb_memvarScope(const char *szVarName, HB_SIZE nLength);
// Detach a local variable from the eval stack
extern PHB_ITEM hb_memvarDetachLocal(PHB_ITEM pLocal);
extern HB_EXPORT PHB_ITEM hb_memvarGetValueBySym(PHB_DYNS pDynSym);
// create array with visible memvar references or copies respecting given memvars scope
extern HB_EXPORT PHB_ITEM hb_memvarSaveInArray(int iScope, HB_BOOL fCopy);
extern void hb_memvarRestoreFromArray(PHB_ITEM pArray);

#ifdef _HB_API_INTERNAL_
// increase the reference count of a global value
extern void hb_memvarValueIncRef(PHB_ITEM pValue);
// decrease the reference count of a global value
extern void hb_memvarValueDecRef(PHB_ITEM pValue);
extern PHB_ITEM hb_memvarGetItem(PHB_SYMB pMemvarSymb);
#if defined(_HB_API_MACROS_)
#  define hb_memvarValueIncRef(p)       hb_xRefInc(p)
#endif // _HB_API_MACROS_
#endif // _HB_API_INTERNAL_

// console I/O subsystem
// initialize the console API system
extern void hb_conInit(void);
// release the console API system
extern void hb_conRelease(void);
// retrieve a pointer to a static buffer containing new-line characters
extern HB_EXPORT const char *hb_conNewLine(void);
// output an string to STDOUT
extern HB_EXPORT void hb_conOutStd(const char *pStr, HB_SIZE nLen);
// output an string to STDERR
extern HB_EXPORT void hb_conOutErr(const char *pStr, HB_SIZE nLen);
// output an string to the screen and/or printer and/or alternate
extern HB_EXPORT void hb_conOutAlt(const char *pStr, HB_SIZE nLen);
// retrieve and optionally set cursor shape
extern HB_EXPORT int hb_conSetCursor(HB_BOOL bSetCursor, int iNewCursor);
// retrieve and optionally set console color
extern HB_EXPORT const char *hb_conSetColor(const char *szColor);

// compiler and macro compiler
extern HB_EXPORT_INT char *hb_compEncodeString(int iMethod, const char *szText, HB_SIZE *pnLen);
extern char *hb_compDecodeString(int iMethod, const char *szText, HB_SIZE *pnLen);

// misc
// retrieve a procedure name into a buffer
extern HB_EXPORT char *hb_procname(int iLevel, char *szName, HB_BOOL bskipBlock);
extern HB_EXPORT HB_BOOL hb_procinfo(int iLevel, char *szName, HB_USHORT *puiLine, char *szFile);

// macro compiler
#if defined(HB_MACRO_SUPPORT)
struct HB_MACRO_;
typedef struct HB_MACRO_ * PHB_MACRO;
#else
typedef void * PHB_MACRO;
#endif
// retrieve results of a macro expansion
extern HB_EXPORT void hb_macroGetValue(PHB_ITEM pItem, int iContext, int flags);
// assign a value to a macro-expression item
extern void hb_macroSetValue(PHB_ITEM pItem, int flags);
// push reference to given expression
extern void hb_macroPushReference(PHB_ITEM pItem);
// macro text substitution
extern HB_EXPORT void hb_macroTextValue(PHB_ITEM pItem);
// handle a macro function calls, e.g. var := &macro()
extern void hb_macroPushSymbol(PHB_ITEM pItem);
// executes pcode compiled by macro compiler
extern void hb_macroRun(PHB_MACRO pMacro);
// compile a string and return a pcode buffer
extern PHB_MACRO hb_macroCompile(const char *szString);
// release all memory allocated for macro evaluation
extern void hb_macroDelete(PHB_MACRO pMacro);
// substitute macro variables occurrences within a given string and check if result is a valid function or variable name
extern char *hb_macroTextSymbol(const char *szString, HB_SIZE nLength, HB_BOOL *pfNewString);
// expands valid '&' operator
extern char *hb_macroExpandString(const char *szString, HB_SIZE nLength, HB_BOOL *pfNewString);
// compiles and evaluates an aliased macro expression
extern void hb_macroPopAliasedValue(PHB_ITEM pAlias, PHB_ITEM pVar, int flags);
// compiles and evaluates an aliased macro expression
extern void hb_macroPushAliasedValue(PHB_ITEM pAlias, PHB_ITEM pVar, int flags);
// determine the type of an expression
extern HB_EXPORT const char *hb_macroGetType(PHB_ITEM pItem);

// idle states
extern HB_EXPORT void hb_releaseCPU(void);
// services a single idle state
extern HB_EXPORT void hb_idleState(void);
// services a single idle state
extern HB_EXPORT void hb_idleReset(void);
// sleep for a given time serving idle task
extern HB_EXPORT void hb_idleSleep(double dSeconds);

// I18N public API
extern PHB_ITEM hb_i18n_ngettext(PHB_ITEM pNum, PHB_ITEM pMsgID, PHB_ITEM pContext);
extern PHB_ITEM hb_i18n_gettext(PHB_ITEM pMsgID, PHB_ITEM pContext);
// I18N internal HVM API
#if defined(_HB_API_INTERNAL_) || defined(_HB_I18N_INTERNAL_)
extern void *hb_vmI18N(void);
extern void hb_vmSetI18N(void *);
extern void hb_i18n_init(void);
extern void hb_i18n_exit(void);
extern void hb_i18n_release(void *cargo);
extern void *hb_i18n_alloc(void *cargo);
#endif // _HB_API_INTERNAL_ || _HB_I18N_INTERNAL_

extern HB_EXPORT void hb_vmSetLinkedMain(const char *szMain);
extern HB_EXPORT void hb_vmSetDefaultGT(const char *szGtName);
extern HB_EXPORT HB_BOOL hb_vmInternalsEnabled(void);

extern HB_EXPORT PHB_FUNC hb_vmProcAddress(const char *szFuncName);

extern HB_EXPORT PHB_ITEM hb_libLoad(PHB_ITEM pLibName, PHB_ITEM pArgs);
extern HB_EXPORT HB_BOOL hb_libFree(PHB_ITEM pDynLib);
extern HB_EXPORT void *hb_libHandle(PHB_ITEM pDynLib);
extern HB_EXPORT void *hb_libSymAddr(PHB_ITEM pDynLib, const char *pszSymbol);

extern HB_EXPORT void hb_dynCall(int iFuncFlags, void *pFunction, int iParams, int iFirst, int *piArgFlags);

// misc
// retrieves a constant string with host OS CPU architecture
extern HB_EXPORT const char *hb_verHostCPU(void);
// retrieves bit width of host OS
extern HB_EXPORT int hb_verHostBitWidth(void);
// retrieves a constant string with CPU architecture
extern HB_EXPORT const char *hb_verCPU(void);
// retrieves a constant string with OS platform (as it appears in __PLATFORM__* macro)
extern HB_EXPORT const char *hb_verPlatformMacro(void);
// retrieves a newly allocated buffer containing platform version
extern HB_EXPORT char *hb_verPlatform(void);
// retrieves a newly allocated buffer containing compiler version
extern HB_EXPORT char *hb_verCompiler(void);
// retrieves a newly allocated buffer containing Harbour version
extern HB_EXPORT char *hb_verHarbour(void);
// retrieves a newly allocated buffer containing PCode version
extern HB_EXPORT char *hb_verPCode(void);
// retrieves a newly allocated buffer containing build date and time
extern HB_EXPORT char *hb_verBuildDate(void);
// display harbour, compiler, and platform versions to standard console
extern HB_EXPORT void hb_verBuildInfo(void);
// retrieves source repository revision number
extern HB_EXPORT HB_MAXINT hb_verRevision(void);
// retrieves a static buffer containing ChangeLog ID string
extern HB_EXPORT const char *hb_verChangeLogID(void);
// retrieves a static buffer containing ChangeLog last entry string
extern HB_EXPORT const char *hb_verChangeLogLastEntry(void);
#if defined(HB_LEGACY_LEVEL4)
// retrieves source repository revision number
extern HB_EXPORT HB_MAXINT hb_verSvnID(void);
// retrieves a static buffer containing ChangeLog ID string
extern HB_EXPORT const char *hb_verSvnChangeLogID(void);
// retrieves a static buffer containing ChangeLog last entry string
extern HB_EXPORT const char *hb_verSvnLastEntry(void);
#endif
// retrieves a static buffer containing build time C compiler flags in HB_USER_CFLAGS envvar
extern HB_EXPORT const char *hb_verFlagsC(void);
// retrieves a static buffer containing build time linker flags in HB_USER_LDFLAGS envvar
extern HB_EXPORT const char *hb_verFlagsL(void);
// retrieves a static buffer containing build time Harbour compiler flags in HB_USER_PRGFLAGS envvar
extern HB_EXPORT const char *hb_verFlagsPRG(void);
// retrieves a static buffer containing build time HB_PLATFORM setting
extern HB_EXPORT const char *hb_verHB_PLAT(void);
// retrieves a static buffer containing build time HB_COMPILER setting
extern HB_EXPORT const char *hb_verHB_COMP(void);

// return non-zero if OS == Wine
extern HB_EXPORT int hb_iswine(void);
// return non-zero if OS == Windows 9x, ME
extern HB_EXPORT int hb_iswin9x(void);
// return non-zero if OS == Windows NT or newer
extern HB_EXPORT int hb_iswinnt(void);
// return HB_TRUE if OS == Windows 2000 or newer
extern HB_EXPORT HB_BOOL hb_iswin2k(void);
// return HB_TRUE if OS == Windows 2003 Server or newer
extern HB_EXPORT HB_BOOL hb_iswin2k3(void);
// return HB_TRUE if OS == Windows Vista or newer
extern HB_EXPORT HB_BOOL hb_iswinvista(void);
// return HB_TRUE if OS == Windows 7 or newer
extern HB_EXPORT HB_BOOL hb_iswin7(void);
// return HB_TRUE if OS == Windows 8 or newer
extern HB_EXPORT HB_BOOL hb_iswin8(void);
// return HB_TRUE if OS == Windows 8.1 or newer
extern HB_EXPORT HB_BOOL hb_iswin81(void);
// return HB_TRUE if OS == Windows 10 or newer
extern HB_EXPORT HB_BOOL hb_iswin10(void);
// return HB_TRUE if OS is Windows CE or Windows Mobile
extern HB_EXPORT HB_BOOL hb_iswince(void);
extern HB_EXPORT HB_BOOL hb_iswinver(int iMajor, int iMinor, int iType, HB_BOOL fOrUpper);
extern HB_EXPORT HB_BOOL hb_iswinsp(int iServicePackMajor, HB_BOOL fOrUpper);

extern HB_EXPORT HB_BOOL hb_printerIsReady(const char *pszPrinterName);

// OS/Harbour codepage conversion
// Is OS<->Harbour codepage conversion enabled?
extern HB_EXPORT HB_BOOL hb_osUseCP(void);
// Convert a string sent to a system call, from Harbour codepage.
extern HB_EXPORT const char *hb_osEncodeCP(const char *szName, char **pszFree, HB_SIZE *pnSize);
// Convert a string received from a system call, to Harbour codepage.
extern HB_EXPORT const char *hb_osDecodeCP(const char *szName, char **pszFree, HB_SIZE *pnSize);

extern HB_EXPORT char *hb_osStrEncode(const char *pszName);
extern HB_EXPORT char *hb_osStrEncodeN(const char *pszName, HB_SIZE nLen);
extern HB_EXPORT char *hb_osStrEncode2(const char *pszName, char *pszBuffer, HB_SIZE nSize);
extern HB_EXPORT char *hb_osStrDecode(const char *pszName);
extern HB_EXPORT char *hb_osStrDecode2(const char *pszName, char *pszBuffer, HB_SIZE nSize);
#if defined(HB_OS_WIN)
extern HB_EXPORT HB_WCHAR *hb_osStrU16Encode(const char *pszName);
extern HB_EXPORT HB_WCHAR *hb_osStrU16EncodeN(const char *pszName, HB_SIZE nLen);
extern HB_EXPORT HB_WCHAR *hb_osStrU16Encode2(const char *pszName, HB_WCHAR *pszBufferW, HB_SIZE nSize);
extern HB_EXPORT char *hb_osStrU16Decode(const HB_WCHAR *pszNameW);
extern HB_EXPORT char *hb_osStrU16Decode2(const HB_WCHAR *pszNameW, char *pszBuffer, HB_SIZE nSize);
#endif

// environment variables access
extern HB_EXPORT HB_BOOL hb_getenv_buffer(const char *szName, char *szBuffer, int nSize);
// WARNING: This returned pointer must be freed if not NULL using hb_xfree(ptr);
extern HB_EXPORT char *hb_getenv(const char *name);
// set or delete (szValue==NULL) environment variable
extern HB_EXPORT HB_BOOL hb_setenv(const char *szName, const char *szValue);
extern HB_EXPORT char *hb_netname(void);
extern HB_EXPORT char *hb_username(void);

// Translation related things

// Dummy define for start
#define HB_I_(x) x

HB_EXTERN_END

#if defined(HB_MACRO_SUPPORT)
#include "hbcompdf.hpp"
#endif

#endif // HB_APIEXT_H_
