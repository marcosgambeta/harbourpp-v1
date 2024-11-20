# Harbour++ changes

## English

### New C++ API (work in progress)

structure _HB_ITEM

HB_TYPE rawType()  
void setType(HB_TYPE type)  

bool isNil()  
bool isArray()  
bool isBlock()  
bool isDate()  
bool isTimeStamp()  
bool isDouble()  
bool isInteger()  
bool isLogical()  
bool isLong()  
bool isSymbol()  
bool isPointer()  
bool isHash()  
bool isMemo()  
bool isString()  
bool isMemVar()  
bool isEnum()  
bool isExtRef()  
bool isByRef()  
bool isNumeric()  
bool isNumInt()  
bool isDateTime()  
bool isComplex()  
bool isGCItem()  
bool isEvalItem()  
bool isHashKey()  
bool isBadItem()  
bool isObject()  
bool isNumber()  

int getNI()  
long getNL()  
double getND()  
HB_BOOL getL()  
char *getC()  
const char *getCPtr()  
HB_SIZE getCLen()  
HB_MAXINT getNInt()  
char *getDS(char *szDate)  
long getDL()  
double getTD()  
HB_BOOL getTDT(long *plJulian, long *plMilliSec)  
void *getPtr()  
PHB_SYMB getSymbol()  

_HB_ITEM *putNI(int iNumber)  
_HB_ITEM *putNL(long lNumber)  
_HB_ITEM *putL(HB_BOOL bValue)  
_HB_ITEM *putC(const char *szText)  
_HB_ITEM *putCL(const char *szText, HB_SIZE nLen)  
_HB_ITEM *putCConst(const char *szText)  
_HB_ITEM *putCLConst(const char *szText, HB_SIZE nLen)  
_HB_ITEM *putCPtr(char *szText)  
_HB_ITEM *putCLPtr(char *szText, HB_SIZE nLen)  
void setCMemo()  

HB_BOOL logicalValue()  
void setLogicalValue(HB_BOOL bValue)  

int integerValue()  
void setIntegerValue(int iValue)  
HB_USHORT integerLength()  
void setIntegerLength(HB_USHORT length)  

HB_MAXINT longValue()  
void setLongValue(HB_MAXINT lValue)  
HB_USHORT longLength()  
void setLongLength(HB_USHORT length)  

double doubleValue()  
void setDoubleValue(double dValue)  
HB_USHORT doubleLength()  
void setDoubleLength(HB_USHORT length)  
HB_USHORT doubleDecimal()  
void setDoubleDecimal(HB_USHORT decimal)  

char *stringValue()  
void setStringValue(char *sValue)  
HB_SIZE stringLength()  
void setStringLength(HB_SIZE length)  
HB_SIZE stringAllocated()  
void setStringAllocated(HB_SIZE allocated)  

void *pointerValue()  
void setPointerValue(void *pValue)  
HB_BOOL pointerCollect()  
void setPointerCollect(HB_BOOL b)  
HB_BOOL pointerSingle()  
void setPointerSingle(HB_BOOL b)  

_HB_BASEARRAY *arrayValue()  
void setArrayValue(_HB_BASEARRAY *pValue)  
_HB_ITEM *arrayItems()  
bool isValidIndex(HB_SIZE nIndex)  
_HB_ITEM *arrayItem(HB_SIZE nIndex)  
HB_SIZE arrayLen()  

long dateTimeJulian()  
void setDateTimeJulian(long lValue)  
long dateTimeTime()  
void setDateTimeTime(long lValue)  

PHB_SYMB symbolValue()  
void setSymbolValue(PHB_SYMB pValue)  
PHB_STACK_STATE symbolStackState()  
void setSymbolStackState(PHB_STACK_STATE pValue)  
HB_USHORT symbolParamCnt()  
void setSymbolParamCnt(HB_USHORT usValue)  
HB_USHORT symbolParamDeclCnt()  
void setSymbolParamDeclCnt(HB_USHORT usValue)  

_HB_CODEBLOCK *blockValue()  
void setBlockValue(_HB_CODEBLOCK *pValue)  
HB_USHORT blockParamCnt()  
void setBlockParamCnt(HB_USHORT usValue)  
HB_USHORT blockLineNo()  
void setBlockLineNo(HB_USHORT usValue)  
HB_USHORT blockHClass()  
void setBlockHClass(HB_USHORT usValue)  
HB_USHORT blockMethod()  
void setBlockMethod(HB_USHORT usValue)  

_HB_BASEHASH *hashValue()  
void setHashValue(_HB_BASEHASH *pValue)  

void clear()  

### Notes

This document is a work in progress.

## Portuguese

. NULL alterado para nullptr  
. Casts no estilo C alterados para const_cast, static_cast e reinterpret_cast  
. Inicialização de variáveis no for( T var = value; ...; ... )  
. Alterações no estilo e formatação do código-fonte  
. Remoção de código relacionado com compiladores obsoletos  
. Biblioteca SQLite atualizada para a versão 3.39.3  
. Suporte para OS/2 descontinuado  
. Suporte para Symbian descontinuado  
. Suporte para DOS descontinuado  
. Suporte para Windows 95/98/ME descontinuado  
. Suporte para Windows NT/2000 descontinuado  
. Suporte para Windows CE descontinuado  
. Suporte para 16-bit descontinuado  
. Adicionado enumeration Harbour::Item::Type  
. Adicionado enumeration Harbour::Result  
. Uso de new e delete para reservar/liberar memória (parcial)  
. Uso de bool/true/false no lugar de HB_BOOL/HB_TRUE/HB_FALSE (parcial)  
. zlib atualizada para a versão 1.2.12
. libpng atualizada para a versão 1.6.38

### Nova API C++ (trabalho em progresso)

structure _HB_ITEM

HB_TYPE rawType()  
void setType(HB_TYPE type)  

bool isNil()  
bool isArray()  
bool isBlock()  
bool isDate()  
bool isTimeStamp()  
bool isDouble()  
bool isInteger()  
bool isLogical()  
bool isLong()  
bool isSymbol()  
bool isPointer()  
bool isHash()  
bool isMemo()  
bool isString()  
bool isMemVar()  
bool isEnum()  
bool isExtRef()  
bool isByRef()  
bool isNumeric()  
bool isNumInt()  
bool isDateTime()  
bool isComplex()  
bool isGCItem()  
bool isEvalItem()  
bool isHashKey()  
bool isBadItem()  
bool isObject()  
bool isNumber()  

int getNI()  
long getNL()  
double getND()  
HB_BOOL getL()  
char *getC()  
const char *getCPtr()  
HB_SIZE getCLen()  
HB_MAXINT getNInt()  
char *getDS(char *szDate)  
long getDL()  
double getTD()  
HB_BOOL getTDT(long *plJulian, long *plMilliSec)  
void *getPtr()  
PHB_SYMB getSymbol()  

_HB_ITEM *putNI(int iNumber)  
_HB_ITEM *putNL(long lNumber)  
_HB_ITEM *putL(HB_BOOL bValue)  
_HB_ITEM *putNI(int iNumber)  
_HB_ITEM *putNL(long lNumber)  
_HB_ITEM *putL(HB_BOOL bValue)  
_HB_ITEM *putC(const char *szText)  
_HB_ITEM *putCL(const char *szText, HB_SIZE nLen)  
_HB_ITEM *putCConst(const char *szText)  
_HB_ITEM *putCLConst(const char *szText, HB_SIZE nLen)  
_HB_ITEM *putCPtr(char *szText)  
_HB_ITEM *putCLPtr(char *szText, HB_SIZE nLen)  
void setCMemo()  

HB_BOOL logicalValue()  
void setLogicalValue(HB_BOOL bValue)  

int integerValue()  
void setIntegerValue(int iValue)  
HB_USHORT integerLength()  
void setIntegerLength(HB_USHORT length)  

HB_MAXINT longValue()  
void setLongValue(HB_MAXINT lValue)  
HB_USHORT longLength()  
void setLongLength(HB_USHORT length)  

double doubleValue()  
void setDoubleValue(double dValue)  
HB_USHORT doubleLength()  
void setDoubleLength(HB_USHORT length)  
HB_USHORT doubleDecimal()  
void setDoubleDecimal(HB_USHORT decimal)  

char *stringValue()  
void setStringValue(char *sValue)  
HB_SIZE stringLength()  
void setStringLength(HB_SIZE length)  
HB_SIZE stringAllocated()  
void setStringAllocated(HB_SIZE allocated)  

void *pointerValue()  
void setPointerValue(void *pValue)  
HB_BOOL pointerCollect()  
void setPointerCollect(HB_BOOL b)  
HB_BOOL pointerSingle()  
void setPointerSingle(HB_BOOL b)  

_HB_BASEARRAY *arrayValue()  
void setArrayValue(_HB_BASEARRAY *pValue)  
_HB_ITEM *arrayItems()  
bool isValidIndex(HB_SIZE nIndex)  
_HB_ITEM *arrayItem(HB_SIZE nIndex)  
HB_SIZE arrayLen()  

long dateTimeJulian()  
void setDateTimeJulian(long lValue)  
long dateTimeTime()  
void setDateTimeTime(long lValue)  

PHB_SYMB symbolValue()  
void setSymbolValue(PHB_SYMB pValue)  
PHB_STACK_STATE symbolStackState()  
void setSymbolStackState(PHB_STACK_STATE pValue)  
HB_USHORT symbolParamCnt()  
void setSymbolParamCnt(HB_USHORT usValue)  
HB_USHORT symbolParamDeclCnt()  
void setSymbolParamDeclCnt(HB_USHORT usValue)  

_HB_CODEBLOCK *blockValue()  
void setBlockValue(_HB_CODEBLOCK *pValue)  
HB_USHORT blockParamCnt()  
void setBlockParamCnt(HB_USHORT usValue)  
HB_USHORT blockLineNo()  
void setBlockLineNo(HB_USHORT usValue)  
HB_USHORT blockHClass()  
void setBlockHClass(HB_USHORT usValue)  
HB_USHORT blockMethod()  
void setBlockMethod(HB_USHORT usValue)  

_HB_BASEHASH *hashValue()  
void setHashValue(_HB_BASEHASH *pValue)  

void clear()  

### Notas

Este documento é um trabalho em progresso.
