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
long getDL()  

_HB_ITEM *putNI(int iNumber)  
_HB_ITEM *putL(HB_BOOL bValue)  

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
HB_SIZE arrayLen()  

long dateTimeJulian()  
void setDateTimeJulian(long lValue)  
long dateTimeTime()  
void setDateTimeTime(long lValue)  

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
long getDL()  

_HB_ITEM *putNI(int iNumber)  
_HB_ITEM *putL(HB_BOOL bValue)  

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
HB_SIZE arrayLen()  

long dateTimeJulian()  
void setDateTimeJulian(long lValue)  
long dateTimeTime()  
void setDateTimeTime(long lValue)  

void clear()  

### Notas

Este documento é um trabalho em progresso.
