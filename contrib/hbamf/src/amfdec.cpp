/* by Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
 *
 * Decoding AMF3 to Harbour items
 *
 * Contains portions from
 * Dave Thompson's MIT licensed
 * AmFast C library for Python
 */

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapistr.hpp>
#include <hbapicls.hpp> /* for hb_objSetClass() */
#include <hbstack.hpp>
#include "amf.h"

#include <hbdate.hpp>
#include <hbmath.hpp>

#include <hbvm.hpp>

struct amfContext
{
  const char *cBuf;
  HB_ISIZ position;
  HB_ISIZ length;
  PHB_ITEM obj_ref;
  PHB_ITEM str_ref;
  PHB_ITEM class_ref;
  PHB_ITEM conv_function;
};

static bool amf3_getItem(amfContext *context, PHB_ITEM pItem);
extern HB_BOOL hbamf_is_cls_externalizable(HB_USHORT uiClass);

static PHB_ITEM hbamf_cls_externalizable_instance(PHB_ITEM pClassFuncStr)
{
  auto pSymbol = hb_dynsymGet(hb_itemGetCPtr(pClassFuncStr));

  if (pSymbol)
  {
    auto pRetCopy = hb_itemNew(nullptr);
    auto pNewItem = hb_itemNew(nullptr);
    hb_itemMove(pRetCopy, hb_stackReturnItem());

    hb_vmPushDynSym(pSymbol);
    hb_vmPushNil();
    hb_vmDo(0);

    hb_objSendMsg(hb_stackReturnItem(), "NEW", 0);

    hb_itemMove(pNewItem, hb_stackReturnItem());
    hb_itemMove(hb_stackReturnItem(), pRetCopy);

    hb_itemRelease(pRetCopy);

    if (pNewItem)
    {
      if (!pNewItem->isObject())
      {
        hb_itemRelease(pNewItem);
        pNewItem = nullptr;
      }
    }

    return pNewItem;
  }

  return nullptr;
}

static const char *readByte(amfContext *context)
{
  HB_ISIZ new_position = context->position + 1;
  const char *byte_ref;

  if (new_position < 0 || new_position > context->length)
  {
    return nullptr;
  }

  byte_ref = context->cBuf + context->position;
  context->position = new_position;
  return byte_ref;
}

static const char *readBytes(amfContext *context, HB_ISIZ len)
{
  HB_ISIZ new_position = context->position + len;
  const char *result;

  if (new_position < 0 || new_position > context->length)
  {
    return nullptr;
  }

  result = context->cBuf + context->position;
  context->position = new_position;
  return result;
}

static bool amfX_decode_double(amfContext *context, double *val)
{
#ifndef HB_BIG_ENDIAN
  char c_val[8];
#endif
  const char *bytes = readBytes(context, 8);
  if (!bytes)
  {
    return false;
  }

  /*
   * Put bytes from byte array into double
   * FIXME: does this alignment work on any platform?

     union aligned
     {
        double d_val;
        char c_val[8];
     } d;

   */

  /* AMF transmitted double is always in network byte order */
#ifdef HB_BIG_ENDIAN
  memcpy(val, bytes, 8);
#else
  /* Flip endianness */
  c_val[0] = bytes[7];
  c_val[1] = bytes[6];
  c_val[2] = bytes[5];
  c_val[3] = bytes[4];
  c_val[4] = bytes[3];
  c_val[5] = bytes[2];
  c_val[6] = bytes[1];
  c_val[7] = bytes[0];
  memcpy(val, c_val, 8);
#endif

  return true;
}

static bool amf3_decode_int(amfContext *context, int *iVal)
{
  const char *byte_ref;
  char byte;
  int result = 0;
  int byte_cnt = 0;

  byte_ref = readByte(context);
  if (!byte_ref)
  {
    return false;
  }

  byte = byte_ref[0];

  /* If 0x80 is set, int includes the next byte, up to 4 total bytes */
  while ((byte & 0x80) && (byte_cnt < 3))
  {
    result <<= 7;
    result |= byte & 0x7F;
    byte_ref = readByte(context);
    if (!byte_ref)
    {
      return false;
    }
    byte = byte_ref[0];
    byte_cnt++;
  }

  /* shift bits in last byte */
  if (byte_cnt < 3)
  {
    result <<= 7; /* shift by 7, since the 1st bit is reserved for next byte flag */
    result |= byte & 0x7F;
  }
  else
  {
    result <<= 8; /* shift by 8, since no further bytes are possible and 1st bit is not used for flag. */
    result |= byte & 0xff;
  }

  /* Move sign bit, since we're converting 29-bit -> 32-bit */
  if (result & 0x10000000)
  {
    result -= 0x20000000;
  }

  *iVal = result;
  return true;
}

#if 0

static bool amf3_decode_reference(PHB_ITEM pHash, int val, PHB_ITEM pRefItem)
{
   /* Check for index reference */
   if( (val & REFERENCE_BIT) == 0 ) {
      auto pKey = hb_itemNew(nullptr);
      hb_itemPutNI(pKey, val >> 1);

      pRefItem = hb_hashGetItemPtr(pHash, pKey, 0);

      hb_itemRelease(pKey);
      return true;
   }

   return false;
}

#endif

static PHB_ITEM amf3_decode_reference(PHB_ITEM pHash, int val)
{
  /* Check for index reference */
  if ((val & REFERENCE_BIT) == 0)
  {
    auto pKey = hb_itemNew(nullptr);
    PHB_ITEM pRefItem;
    hb_itemPutNI(pKey, val >> 1);

    pRefItem = hb_hashGetItemPtr(pHash, pKey, 0);
    if (!pRefItem)
    {
      hb_itemPutL(pRefItem, false);
    }

    hb_itemRelease(pKey);
    return pRefItem;
  }

  return nullptr;
}

static void amf3_add_reference(PHB_ITEM pHash, PHB_ITEM pItem)
{
  HB_SIZE iRef = hb_hashLen(pHash);
  auto pKey = hb_itemNew(nullptr);

  /* the reference id in AMF starts from 0, and increase
     sequentially - this means we can also use an array,
     not hash for the purpose */

  hb_itemPutNS(pKey, /* ++ first one was 0 */ iRef);
  hb_hashAdd(pHash, pKey, pItem);

  hb_itemRelease(pKey);
}

static bool amfX_decode_string(amfContext *context, PHB_ITEM pItem, unsigned int string_size)
{
  const char *str = readBytes(context, string_size);

  if (!str)
  {
    return false;
  }

  hb_itemPutStrLenUTF8(pItem, str, string_size);
  return true;
}

static bool amf3_deserialize_string(amfContext *context, PHB_ITEM pItem)
{
  int header;
  int *header_p = &header;
  PHB_ITEM pRefItem;
  PHB_ITEM pHash = context->str_ref;

  if (!amf3_decode_int(context, header_p))
  {
    return false;
  }

  /* Check for null string */
  if (header == EMPTY_STRING_TYPE)
  {
    hb_itemPutC(pItem, nullptr);
    return true;
  }

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }
    /* Copies string from reference hash pRefItem -> pItem */
    hb_itemCopy(pItem, pRefItem);
    return true;
  }

  /* No reference found */
  if (!amfX_decode_string(context, pItem, header >> 1))
  {
    return false;
  }

  /* Adds reference */
  amf3_add_reference(pHash, pItem);

  return true;
}

/* Add the dynamic attributes of an encoded obj to a dict. */
static bool amf3_decode_dynamic_dict(amfContext *context, PHB_ITEM pItem)
{
  for (;;)
  {
    bool result;

    auto pKey = hb_itemNew(nullptr);
    if (!amf3_deserialize_string(context, pKey))
    {
      hb_itemRelease(pKey);
      return false;
    }

    if (hb_itemGetCLen(pKey) == 0)
    {
      /* Empty string marks end of name/value pairs */
      hb_itemRelease(pKey);
      return true;
    }

    auto pValue = hb_itemNew(nullptr);
    if (!amf3_getItem(context, pValue))
    {
      hb_itemRelease(pKey);
      hb_itemRelease(pValue);
      return false;
    }

    result = hb_hashAdd(pItem, pKey, pValue);
    hb_itemRelease(pKey);
    hb_itemRelease(pValue);
    if (!result)
    {
      return false;
    }
  }
}

/* Populate an array with values from the buffer. */
static bool decode_dynamic_array_AMF3(amfContext *context, PHB_ITEM pItem, int array_len, bool dict)
{
  int i;
  bool lRet;

  if (dict)
  {
    /* Object is a dict, set item index as key. */
    for (i = 0; i < array_len; i++)
    {
      auto pValue = hb_itemNew(nullptr);
      lRet = false;

      if (amf3_getItem(context, pValue))
      {
        auto pKey = hb_itemNew(nullptr);
        hb_itemPutNI(pKey, i);

        if (hb_hashAdd(pItem, pKey, pValue))
        {
          lRet = true;
        }

        hb_itemRelease(pKey);
      }

      hb_itemRelease(pValue);

      if (!lRet)
      {
        return false;
      }
    }
  }
  else
  {
    /* Standard array. */
    for (i = 0; i < array_len; i++)
    {
      auto pValue = hb_itemNew(nullptr);
      lRet = false;

      if (amf3_getItem(context, pValue))
      {
        if (hb_arraySet(pItem, i + 1, pValue))
        {
          lRet = true;
        }
      }

      hb_itemRelease(pValue);

      if (!lRet)
      {
        return false;
      }
    }
  }

  return true;
}

static bool amf3_deserialize_array(amfContext *context, PHB_ITEM pItem, bool collection)
{
  int header;
  int *header_p = &header;
  PHB_ITEM pRefItem;
  PHB_ITEM pHash = context->obj_ref;
  bool mixed; /* if the result will be a Hash with both numbers and strings as keys */
  const char *byte_ref;

  if (!amf3_decode_int(context, header_p))
  {
    return false;
  }

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }

    /* Reference found */
    if (collection)
    {
      /* Map ArrayCollection idx to ref, since
         it points to the same list. */
      amf3_add_reference(pHash, pRefItem);
    }
    /* Copies string from reference hash pRefItem -> pItem */
    hb_itemCopy(pItem, pRefItem);
    return true;
  }

  auto array_len = static_cast<int>(header >> 1);
  /* Original Python comment was:
     Cannot use array_len to create a list of known
     length, see ticket #46
     I think that this is not a problem for Harbour */

  /* Determine if array is mixed (associative) or not */
  mixed = false;
  byte_ref = readByte(context);
  if (byte_ref == NULL)
  {
    return false;
  }

  if (byte_ref[0] == EMPTY_STRING_TYPE)
  {
    /* Dense array */
    hb_arrayNew(pItem, array_len);
  }
  else
  {
    context->position--;
    hb_hashNew(pItem);
    hb_hashPreallocate(pItem, array_len);

    if (!amf3_decode_dynamic_dict(context, pItem))
    {
      return false;
    }

    mixed = true;
  }

  amf3_add_reference(pHash, pItem);

  /* Originally a python comment.
     I don't understand reasons for following,
     but let it be like this.
     ADD: oh, maybe it's because parsing of
     ArrayCollection starts as Object
     --
     If this is an ArrayCollection,
     we need to add another reference,
     so there is one that
     points to the array and one that points
     to the collection.
   */

  if (collection)
  {
    amf3_add_reference(pHash, pItem);
  }

  return decode_dynamic_array_AMF3(context, pItem, array_len, mixed);

#if 0
   return true;
#endif
}

/* Decode a date. */
static bool amf3_decode_epoch(amfContext *context, PHB_ITEM pItem)
{
  double epoch_millisecs;

  if (!amfX_decode_double(context, &epoch_millisecs))
  {
    return false;
  }

  hb_itemPutTD(pItem, (epoch_millisecs + 210866803200000.4) / HB_MILLISECS_PER_DAY);

  return true;
}

/* Deserialize date. */
static bool amf3_deserialize_date(amfContext *context, PHB_ITEM pItem)
{
  int header;
  int *header_p = &header;
  PHB_ITEM pRefItem;
  PHB_ITEM pHash = context->obj_ref;

  if (!amf3_decode_int(context, header_p))
  {
    return false;
  }

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }
    /* Copies date from reference hash pRefItem -> pItem */
    hb_itemCopy(pItem, pRefItem);
    return true;
  }

  if (!amf3_decode_epoch(context, pItem))
  {
    return false;
  }

  /* Add reference */
  amf3_add_reference(pHash, pItem);

  return true;
}

/* Decode a byte array. */
static bool amf3_decode_byte_array(amfContext *context, PHB_ITEM pItem, int byte_len)
{
  const char *str = readBytes(context, byte_len);

  if (!str)
  {
    return false;
  }

  hb_itemPutStrLen(pItem, hb_vmCDP(), str, byte_len);
  return true;
}

/* Deserialize a byte array. */
static bool amf3_deserialize_byte_array(amfContext *context, PHB_ITEM pItem)
{
  int header;
  int *header_p = &header;
  PHB_ITEM pRefItem;
  PHB_ITEM pHash = context->obj_ref;

  if (!amf3_decode_int(context, header_p))
  {
    return false;
  }

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }
    /* Copies ByteArray (string) from reference hash pRefItem -> pItem */
    hb_itemCopy(pItem, pRefItem);
    return true;
  }

  if (!amf3_decode_byte_array(context, pItem, header >> 1))
  {
    return false;
  }

  /* Add reference */
  amf3_add_reference(pHash, pItem);

  return true;
}

/* Get an object's class def - nearly a copy/paste from decoder */
static PHB_ITEM class_def_from_classname(/* amfContext * context, */ PHB_ITEM pClassName)
{
  HB_USHORT uiClass;
  PHB_ITEM pClass;
  char *pszBuffer = hb_itemGetC(pClassName);
  auto nLen = hb_itemGetCLen(pClassName);

  hb_strUpper(pszBuffer, nLen);

  /* get Harbour's class id/handle */
  uiClass = hb_clsFindClass(pszBuffer, nullptr);

  hb_strfree(pszBuffer);

#if 0
   uiClass = hb_objGetClass(pItem);
#endif
  if (!uiClass)
  {
    return nullptr;
  }

  pClass = hb_hashNew(nullptr);

  auto pKey = hb_itemPutC(nullptr, "CLASS_DEF");
  auto pValue = hb_itemNew(nullptr);
  if (!hb_hashAdd(pClass, pKey, pValue))
  {
    hb_itemRelease(pKey);
    hb_itemRelease(pValue);
    hb_itemRelease(pClass);
    return nullptr;
  }
  hb_itemRelease(pKey);
  hb_itemRelease(pValue);

  pKey = hb_itemPutC(nullptr, "alias");
  pValue = hb_itemPutC(nullptr, hb_clsName(uiClass));
  if (!hb_hashAdd(pClass, pKey, pValue))
  {
    hb_itemRelease(pKey);
    hb_itemRelease(pValue);
    hb_itemRelease(pClass);
    return nullptr;
  }
  hb_itemRelease(pKey);
  hb_itemRelease(pValue);

  if (hbamf_is_cls_externalizable(uiClass))
  {
    pKey = hb_itemPutC(nullptr, "EXTERNALIZABLE_CLASS_DEF");
    pValue = hb_itemNew(nullptr);
    if (!hb_hashAdd(pClass, pKey, pValue))
    {
      hb_itemRelease(pKey);
      hb_itemRelease(pValue);
      hb_itemRelease(pClass);
      return nullptr;
    }
    hb_itemRelease(pKey);
    hb_itemRelease(pValue);
  }

  return pClass;
}

/*
 * Decode a ClassDef.
 *
 * Header argument is the obj header.
 */
static bool amf3_decode_class_def(amfContext *context, PHB_ITEM pClass, int header)
{
  auto pStrAlias = hb_itemNew(nullptr);
  PHB_ITEM pMappedClassDef = nullptr;
  PHB_ITEM pKey;
  PHB_ITEM pValue;
  PHB_ITEM pAttrs;
  int i;

  if (!amf3_deserialize_string(context, pStrAlias))
  {
    hb_itemRelease(pStrAlias);
    return false;
  }

  /* If this alias string is empty then the object is anonymous */
  if (hb_itemGetCLen(pStrAlias) > 0)
  {
    /* Retrieve a ClassDef from a class alias string. */
    pMappedClassDef = class_def_from_classname(pStrAlias);
    if (!pMappedClassDef)
    {
      pMappedClassDef = hb_itemNew(nullptr);
      hb_hashNew(pMappedClassDef); /* empty hash emulation for now */
    }

    /* PyObject_CallMethodObjArgs(context->class_mapper,
        context->class_def_name, alias, NULL); */
  }
  hb_itemRelease(pStrAlias);

  /* Create a dict with class def information
     specific to this decode context. */
  hb_hashNew(pClass);

  if (pMappedClassDef)
  {
    pKey = hb_itemPutC(nullptr, "class_def"); /* hb_itemNew(nullptr); */

    if (!hb_hashAdd(pClass, pKey, pMappedClassDef))
    {
      hb_itemRelease(pKey);
      hb_itemRelease(pMappedClassDef);
      return false;
    }
    hb_itemRelease(pKey);

    pKey = hb_itemPutC(nullptr, "EXTERNALIZABLE_CLASS_DEF");
    if (hb_hashScan(pMappedClassDef, pKey, nullptr))
    {
      /* There is nothing else we need to do
         with externalizable ClassDefs */
      hb_itemRelease(pKey);
      hb_itemRelease(pMappedClassDef);
      return true;
    }
    hb_itemRelease(pKey);

    /* this item should be now referenced in the hash */
    hb_itemRelease(pMappedClassDef);
  }

  if ((header & 0x07FFFFFF) == EXTERNALIZABLE)
  {
    /* If the class is externalizable, but the ClassDef isn't,
       we have a big problem, because we don't know how to read
       the raw bytes. */

    /* TODO: introduce similar RTE?
       PyErr_SetString(amfast_DecodeError, "Encoded class is externalizable, but ClassDef is not."); */
    return false;
  }

  /* Set dynamic flag */
  pKey = hb_itemPutC(nullptr, "dynamic");
  pValue = hb_itemPutL(nullptr, (header & DYNAMIC) == DYNAMIC);
  if (!hb_hashAdd(pClass, pKey, pValue))
  {
    hb_itemRelease(pKey);
    hb_itemRelease(pValue);
    return false;
  }
  hb_itemRelease(pKey);
  hb_itemRelease(pValue);

  /* Decode static attr names */
  auto static_attr_len = static_cast<int>(header >> 4);
  pAttrs = hb_itemNew(nullptr);
  hb_arrayNew(pAttrs, static_attr_len);

  for (i = 0; i < static_attr_len; i++)
  {
    pValue = hb_itemNew(nullptr);
    if (!amf3_deserialize_string(context, pValue))
    {
      hb_itemRelease(pAttrs);
      hb_itemRelease(pValue);
      return false;
    }

    /* steals ref to attr_name */
    if (!hb_arraySet(pAttrs, i + 1, pValue))
    {
      hb_itemRelease(pAttrs);
      hb_itemRelease(pValue);
      return false;
    }

    hb_itemRelease(pValue);
  }

  /* Set decoded attrs onto ClassDef */
  pKey = hb_itemPutC(nullptr, "static_attrs");
  if (!hb_hashAdd(pClass, pKey, pAttrs))
  {
    hb_itemRelease(pKey);
    hb_itemRelease(pAttrs);
    return false;
  }
  hb_itemRelease(pKey);
  hb_itemRelease(pAttrs);

  return true;
}

/*
 * Deserialize a ClassDef.
 *
 * header argument is the parsed obj header.
 */
static bool amf3_deserialize_class_def(amfContext *context, PHB_ITEM pClass, int header)
{
  PHB_ITEM pHash = context->class_ref;
  PHB_ITEM pRefItem;

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }
    /* Copies ClassDef from reference hash pRefItem -> pClass */
    hb_itemCopy(pClass, pRefItem);
    return true;
  }

  if (!amf3_decode_class_def(context, pClass, header))
  {
    return false;
  }

  /* Add reference to obj */
  amf3_add_reference(pHash, pClass);

  return true;
}

/* Returns a dict with values from an object. */
static bool amf3_decode_obj_attrs(amfContext *context, PHB_ITEM pHash, PHB_ITEM pClass)
{
  PHB_ITEM pArray;
  PHB_ITEM pValue;
  HB_SIZE static_attr_len;
  HB_SIZE i;

  /* Put decoded attributes into pHash */

  /* Decode static attrs */
  pArray = hb_hashGetCItemPtr(pClass, "static_attrs");
  if (!pArray)
  {
    return false;
  }

  /* maybe hb_arrayGetItemPtr() could be used? */

  static_attr_len = hb_arrayLen(pArray);

  for (i = 0; i < static_attr_len; i++)
  {
    bool result;

    pValue = hb_itemNew(nullptr);
    if (!amf3_getItem(context, pValue))
    {
      hb_itemRelease(pValue);
      return false;
    }

    auto pKey = hb_itemNew(nullptr);
    if (!hb_arrayGet(pArray, i + 1, pKey))
    {
      hb_itemRelease(pValue);
      hb_itemRelease(pKey);
      return false;
    }

    result = hb_hashAdd(pHash, pKey, pValue);
    hb_itemRelease(pValue);
    hb_itemRelease(pKey);

    if (!result)
    {
      return false;
    }
  }

  /* Decode dynamic attrs */
  pValue = hb_hashGetCItemPtr(pClass, "dynamic");
  if (!pValue)
  {
    return false;
  }

  if (pValue->isLogical() && hb_itemGetL(pValue))
  {
    if (!amf3_decode_dynamic_dict(context, pHash))
      return false;
  }

  return true;
}

/* Decode an anonymous obj. */
static bool amf3_decode_anon_obj(amfContext *context, PHB_ITEM pItem, PHB_ITEM pClass)
{
  auto pAnonHash = hb_itemNew(nullptr);
  bool result = false;

  /* Original Python comment which I don't understand:
     We're using merge instead of populating the dict
     directly, because we have to setup a reference to the
     object before decoding it. ?????? */

  /* we (Harbourers) are supplying already initialized hash to next function */
  if (hb_arrayGet(pItem, OBJAMF_VAR_HASH, pAnonHash))
  {
    result = amf3_decode_obj_attrs(context, pAnonHash, pClass);
  }

  hb_itemRelease(pAnonHash);

  return result;
}

static bool amf3_decode_externalizable(amfContext *context, PHB_ITEM pItem)
{
  const char *position;
  auto pRetCopy = hb_itemNew(nullptr);
  PHB_ITEM pPos;
  bool result = true;
  PHB_ITEM pObject;

  if (pItem == hb_stackReturnItem())
  {
    pObject = pRetCopy;
  }
  else
  {
    pObject = pItem;
  }

  hb_itemMove(pRetCopy, hb_stackReturnItem());

  position = context->cBuf + context->position;
  auto pStr = hb_itemNew(nullptr);
  pStr = hb_itemPutCLConst(pStr, position, context->length - context->position);
  pPos = hb_objSendMsg(pObject, "READEXTERNAL", 1, pStr);
  if (pPos->isInteger())
  {
    if (!readBytes(context, hb_itemGetNI(pPos)))
    {
      result = false;
    }
  }
  else
  {
    result = false;
  }

  hb_itemMove(hb_stackReturnItem(), pRetCopy);

  hb_itemRelease(pRetCopy);
  hb_itemRelease(pStr);
  return result;
}

/*
 * Deserialize an obj.
 *
 * proxy is flag indicating that the obj being deserialized is within an ObjectProxy
 */
static bool amf3_deserialize_obj(amfContext *context, PHB_ITEM pItem, bool proxy)
{
  int header;
  int *header_p = &header;
  PHB_ITEM pRefItem;
  PHB_ITEM pHash = context->obj_ref;
  PHB_ITEM pClass;
  PHB_ITEM pMappedClassDef;
  PHB_ITEM pValue;
  int obj_type; /* 0 = anonymous, 1 == externalizable, 2 == typed */
  bool result;

  if (!amf3_decode_int(context, header_p))
  {
    return false;
  }

  /* Check for reference */
  pRefItem = amf3_decode_reference(pHash, header);
  if (pRefItem)
  {
    if (pRefItem->isLogical())
    {
      /* Logical value means a problem getting reference from the hash */
      hb_itemRelease(pRefItem);
      return false;
    }

    /* Reference found */
    if (proxy)
    {
      /* Map ObjectProxy idx to ref, since
         it points to the same obj. */
      amf3_add_reference(pHash, pRefItem);
    }

    /* Copies ByteArray (string) from reference hash pRefItem -> pItem */
    hb_itemCopy(pItem, pRefItem);
    return true;
  }

  pClass = hb_itemNew(nullptr);
  if (!amf3_deserialize_class_def(context, pClass, header))
  {
    hb_itemRelease(pClass);
    return false;
  }

  pMappedClassDef = hb_hashGetCItemPtr(pClass, "class_def");
  if (!pMappedClassDef)
  {
    /* Anonymous obj. */
    obj_type = 0;
  }
  else if (hb_hashGetCItemPos(pMappedClassDef, "EXTERNALIZABLE_CLASS_DEF") != 0)
  {
    if (hb_hashGetCItemPos(pMappedClassDef, "ARRAY_COLLECTION_CLASS_DEF") != 0)
    {
      hb_itemRelease(pClass);

      if (!readByte(context))
      { /* Skip array type marker */
        return false;
      }

      return amf3_deserialize_array(context, pItem, true);
    }

    if (hb_hashGetCItemPos(pMappedClassDef, "OBJECT_PROXY_CLASS_DEF") != 0)
    {
      hb_itemRelease(pClass);

      if (!readByte(context))
      { /* Skip array type marker */
        return false;
      }

      return amf3_deserialize_obj(context, pItem, true);
    }

    obj_type = 1;
  }
  else
  {
    obj_type = 2;
  }

  /* Instantiate new obj */
  if (obj_type == 0)
  {
    /* Anonymous obj == OBJAMF */
    hb_arrayNew(pItem, OBJAMF_VAR_COUNT);
    /* performance FIXME, cache class id (in context maybe)
       to not scan all classes by name every time */
    hb_objSetClass(pItem, "AMF_OBJ", "AMF_OBJ");
    pValue = hb_itemPutNI(nullptr, OBJAMF_VER);
    hb_arraySet(pItem, OBJAMF_VAR_VER, pValue);
    hb_itemRelease(pValue);
    pValue = hb_itemPutC(nullptr, "ANONYMOUS");
    hb_arraySet(pItem, OBJAMF_VAR_NAME, pValue);
    hb_itemRelease(pValue);
    pValue = hb_hashNew(nullptr);
    hb_arraySet(pItem, OBJAMF_VAR_HASH, pValue);
    hb_itemRelease(pValue);
  }
  else if (obj_type == 1)
  {
    /* externalizable object should allow a constructor without parameters,
       and the object should not do anything dangerous, because it could
       be instantiated on clients request */

    pValue = hb_hashGetCItemPtr(pMappedClassDef, "alias");

    if (!pValue)
    {
      hb_itemRelease(pClass);
      return false;
    }

    pValue = hbamf_cls_externalizable_instance(pValue);
    if (!pValue)
    {
      hb_itemRelease(pClass);
      return false;
    }

    hb_itemMove(pItem, pValue);
    hb_itemRelease(pValue);
  }
  else
  {
    /* Create obj_val for all typed objects. */
#if 0
      obj_val = PyObject_CallMethod(class_def, "getInstance", nullptr);
#endif
  }

  if (!pItem->isObject())
  {
    hb_itemRelease(pClass);
    return false;
  }

  /* Reference must be added before children (to allow for recursion). */
  amf3_add_reference(pHash, pItem);

  if (proxy)
  {
    /* If this is an ObjectProxy,
       we need to add another reference,
       so there is one that
       points to the obj and one that points
       to the proxy. */

    amf3_add_reference(pHash, pItem);
  }

  result = false;
  if (obj_type == 0)
  {
    result = amf3_decode_anon_obj(context, pItem, pClass);
  }
  else if (obj_type == 1)
  {
#if 0
      result = true;
#endif
    result = amf3_decode_externalizable(context, pItem /*, pMappedClassDef */);
  }
  else if (obj_type == 2)
  {
#if 0
      result = decode_typed_obj_AMF3(context, obj_val, class_def_dict);
#endif
  }

  hb_itemRelease(pClass);

  return result;
}

static void amf3_conversion_in(amfContext *context, PHB_ITEM pItem)
{
  auto pRetCopy = hb_itemNew(nullptr);
  PHB_SYMB pSym = hb_itemGetSymbol(context->conv_function);

  if (pItem == hb_stackReturnItem())
  {
    hb_vmPushSymbol(pSym);
    hb_vmPushNil();
    hb_vmPush(pItem);
    hb_vmDo(1);
  }
  else
  {
    hb_itemMove(pRetCopy, hb_stackReturnItem());
    hb_vmPushSymbol(pSym);
    hb_vmPushNil();
    hb_vmPush(pItem);
    hb_vmDo(1);
    hb_itemMove(pItem, hb_stackReturnItem());
    hb_itemMove(hb_stackReturnItem(), pRetCopy);
  }
  hb_itemRelease(pRetCopy);
}

/* much of deserialize_* functions are so much similar that we may
   generalize them in one, f.e. adding another parameter specifying
   pointer of a final decoding function... in case reference checking
   returns nothing */

static bool amf3_getItem(amfContext *context, PHB_ITEM pItem)
{
  char byte;
  const char *byte_ref;
  bool lRet = true;

  byte_ref = readByte(context);

  if (!byte_ref)
  {
    return false;
  }
  byte = byte_ref[0];

  switch (byte)
  {
  case UNDEFINED_TYPE:
  case NULL_TYPE:
    lRet = true;
    break;

  case FALSE_TYPE:
    hb_itemPutL(pItem, false);
    lRet = true;
    break;

  case TRUE_TYPE:
    hb_itemPutL(pItem, true);
    lRet = true;
    break;

  case INT_TYPE: {
    int iVal;
    if (amf3_decode_int(context, &iVal))
    {
      hb_itemPutNI(pItem, iVal);
    }
    else
    {
      lRet = false;
    }
  }
  break;

  case DOUBLE_TYPE: {
    double dVal;
    if (amfX_decode_double(context, &dVal))
    {
      hb_itemPutND(pItem, dVal);
    }
    else
    {
      lRet = false;
    }
  }
  break;

  case STRING_TYPE:
    lRet = amf3_deserialize_string(context, pItem);
    break;

  case XML_DOC_TYPE:
    /* don't touch xml encoding right now */
    lRet = amf3_deserialize_byte_array(context, pItem);
    break;

  case DATE_TYPE:
    lRet = amf3_deserialize_date(context, pItem);
    break;

  case ARRAY_TYPE:
    lRet = amf3_deserialize_array(context, pItem, false);
    break;

  case OBJECT_TYPE:
    lRet = amf3_deserialize_obj(context, pItem, false);
    break;

  case XML_TYPE:
    /* don't touch xml encoding right now */
    lRet = amf3_deserialize_byte_array(context, pItem);
    break;

  case BYTE_ARRAY_TYPE:
    lRet = amf3_deserialize_byte_array(context, pItem);
    break;

  case AMF3_AMF0:
    lRet = amf3_getItem(context, pItem);
    break;

  default:
    lRet = false;
    break;
  }

  if (context->conv_function)
  {
    amf3_conversion_in(context, pItem);
  }

  return lRet;
}

HB_FUNC(AMF3_DECODE)
{
  PHB_ITEM pItem = hb_stackReturnItem();

#if defined(_DEBUG)
  auto pDebugBlock = hb_param(2, Harbour::Item::BLOCK);
#endif
  auto pFuncSym = hb_param(2, Harbour::Item::SYMBOL);

  auto szBuffer = hb_parc(1);

  if (!szBuffer)
  {
    return;
  }

  auto context = static_cast<amfContext *>(hb_xgrab(sizeof(amfContext)));
  memset(context, 0, sizeof(amfContext));

  context->cBuf = szBuffer;
  context->position = 0;
  context->length = hb_parclen(1);
  context->obj_ref = hb_hashNew(nullptr);
  context->str_ref = hb_hashNew(nullptr);
  context->class_ref = hb_hashNew(nullptr);
  context->conv_function = pFuncSym;

  amf3_getItem(context, pItem);

#if defined(_DEBUG)
  if (pDebugBlock)
  {
    hb_vmPushEvalSym();
    hb_vmPush(pDebugBlock);
    hb_vmPush(context->obj_ref);
    hb_vmPush(context->str_ref);
    hb_vmPush(context->class_ref);
    hb_vmSend(static_cast<HB_USHORT>(3));
  }
#endif

  hb_itemRelease(context->obj_ref);
  hb_itemRelease(context->str_ref);
  hb_itemRelease(context->class_ref);

#if 0
   if( context->conv_function ) {
      hb_itemRelease(context->conv_function);
   }
#endif

  hb_xfree(context);
}
