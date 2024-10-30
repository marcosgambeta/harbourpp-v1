/* Ilina Stoilkovska <anili100/at/gmail.com> 2011
 * Aleksander Czajczynski <hb/at/fki.pl> 2011-2017
 *
 * Encoding Harbour items to AMF3
 *
 * Contains portions from
 * Dave Thompson's MIT licensed
 * AmFast C library for Python
 */

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapistr.hpp"
#include "hbapicls.hpp" /* for hb_objSetClass() */
#include "hbstack.hpp"

#include "hbapirdd.hpp" /* for amf3_FromWA() */
#include "hbapierr.hpp" /* as above */

#include "amf.h"

#include "hbdate.hpp"
#include "hbmath.hpp"

#include "hbvm.hpp"

struct amfContext
{
  char *cBuf;
  HB_ISIZ position;
  HB_ISIZ length;
  bool use_refs;
  bool use_strstr;
  bool str_rtrim;
  HB_SIZE strstr_count;  /* used only when str_ref is disabled */
  HB_SIZE objnref_count; /* items that should normally appear in obj_ref, but GC says that they are not referenced */
  PHB_ITEM obj_ref;
  PHB_ITEM str_ref;
  PHB_ITEM strstr_ref;
  PHB_ITEM class_ref;
  PHB_ITEM conv_function;
  bool encode_ba;
};

static bool amf3_encode(amfContext *context, PHB_ITEM pItem);
extern HB_BOOL hbamf_is_cls_externalizable(HB_USHORT uiClass);

static void _ref_realItemPtr(PHB_ITEM pKey, PHB_ITEM pItem)
{
  if (pItem->isString())
  {
    hb_itemPutPtr(pKey, const_cast<char *>(hb_itemGetCPtr(pItem)));
  }
  else if (pItem->isArray())
  {
    hb_itemPutPtr(pKey, hb_arrayId(pItem));
  }
  else if (pItem->isHash())
  {
    hb_itemPutPtr(pKey, hb_hashId(pItem));
  }
  else if (pItem->isDateTime())
  {
    hb_itemCopy(pKey, pItem);
  }
}

static HB_ISIZ bufferGrow(amfContext *context, HB_ISIZ len)
{
  HB_ISIZ new_len = context->position + len;
  HB_ISIZ current_len = context->length;

  while (new_len > current_len)
  {
    current_len *= 2;
  }

  if (current_len != context->length)
  {
    context->cBuf = static_cast<char *>(hb_xrealloc(context->cBuf, sizeof(char) * current_len));
    if (!context->cBuf)
    {
      return -1;
    }

    context->length = current_len;
  }
  return current_len;
}

static int writeByte(amfContext *context, char byte)
{
  if (bufferGrow(context, 1) == -1)
  {
    return 0;
  }

  memcpy(context->cBuf + context->position, &byte, 1);
  context->position += 1;
  return 1;
}

static HB_ISIZ writeBuffer(amfContext *context, const char *str, HB_ISIZ len)
{
  if (bufferGrow(context, len) == -1)
  {
    return 0;
  }

  memcpy(context->cBuf + context->position, str, len);
  context->position += len;
  return len;
}

static bool amfX_encode_double(amfContext *context, double value)
{
  char c_value[8];
  char *c_value_ref = &c_value[0];

#ifndef HB_BIG_ENDIAN
  char flipped[8];
#endif

  memcpy(c_value_ref, &value, 8);

#ifdef HB_BIG_ENDIAN
  if (writeBuffer(context, c_value_ref, 8) == 0)
  {
    return false;
  }
#else
  flipped[0] = c_value[7];
  flipped[1] = c_value[6];
  flipped[2] = c_value[5];
  flipped[3] = c_value[4];
  flipped[4] = c_value[3];
  flipped[5] = c_value[2];
  flipped[6] = c_value[1];
  flipped[7] = c_value[0];
  if (writeBuffer(context, flipped, 8) == 0)
  {
    return false;
  }
#endif

  return true;
}

static bool amfX_write_double(amfContext *context, PHB_ITEM pItem)
{
  auto d = hb_itemGetND(pItem);

  if (!writeByte(context, DOUBLE_TYPE))
  {
    return false;
  }
  return amfX_encode_double(context, d);
}

static bool amf3_encode_int(amfContext *context, int value)
{
  char tmp[4];
  HB_SIZE tmp_size;

  /*
   * Int can be up to 4 bytes long.
   *
   * The first bit of the first 3 bytes
   * is set if another byte follows.
   *
   * The integer value is the last 7 bits from
   * the first 3 bytes and the 8 bits of the last byte
   * (29 bits).
   *
   * The int is negative if the 1st bit of the 29 int is set.
   */
  value &= 0x1fffffff; /* Ignore 1st 3 bits of 32-bit int, since we're encoding to 29 bit. */
  if (value < 0x80)
  {
    tmp_size = 1;
    tmp[0] = static_cast<char>(value); /* TODO: some explicit casts in here to keep the compiler silent */
  }
  else if (value < 0x4000)
  {
    tmp_size = 2;
    tmp[0] =
        static_cast<char>((value >> 7) & 0x7f) | 0x80; /* Shift bits by 7 to fill 1st byte and set next byte flag */
    tmp[1] = static_cast<char>(value) & 0x7f; /* Shift bits by 7 to fill 2nd byte, leave next byte flag unset */
  }
  else if (value < 0x200000)
  {
    tmp_size = 3;
    tmp[0] = static_cast<char>((value >> 14) & 0x7f) | 0x80;
    tmp[1] = static_cast<char>((value >> 7) & 0x7f) | 0x80;
    tmp[2] = static_cast<char>(value) & 0x7f;
  }
  else if (value < 0x40000000)
  {
    tmp_size = 4;
    tmp[0] = static_cast<char>((value >> 22) & 0x7f) | 0x80;
    tmp[1] = static_cast<char>((value >> 15) & 0x7f) | 0x80;
    tmp[2] =
        static_cast<char>((value >> 8) & 0x7f) | 0x80; /* Shift bits by 8, since we can use all bits in the 4th byte */
    tmp[3] = static_cast<char>(value & 0xff);
  }
  else
  {
    return false;
  }

  if (static_cast<HB_SIZE>(writeBuffer(context, tmp, tmp_size)) != tmp_size)
  {
    return false;
  }

  return true;
}

static bool amf3_write_int(amfContext *context, PHB_ITEM pItem)
{
#ifndef HB_LONG_LONG_OFF
  HB_LONGLONG n = hb_itemGetNLL(pItem);
#else
  long n = hb_itemGetNL(pItem);
#endif

  if (n > MIN_INT && n < MAX_INT)
  {
    if (!writeByte(context, INT_TYPE))
    {
      return false;
    }
    return amf3_encode_int(context, static_cast<int>(n));
  }
  else
  {
    if (!writeByte(context, DOUBLE_TYPE))
    {
      return false;
    }
    return amfX_encode_double(context, static_cast<double>(n));
  }
}

#if 0
static bool amf3_encode_float(amfContext * context, PHB_ITEM pItem)
{
   auto n = static_cast<float>(hb_itemGetND(pItem));

   return amfX_encode_double(context, static_cast<double>(n));
}
#endif

static bool amf3_encode_nil(amfContext *context)
{
  return writeByte(context, NULL_TYPE);
}

static bool amf3_encode_bool(amfContext *context, PHB_ITEM pItem)
{
  if (hb_itemGetL(pItem))
  {
    return writeByte(context, TRUE_TYPE);
  }
  else
  {
    return writeByte(context, FALSE_TYPE);
  }
}

static bool amf3_encode_string(amfContext *context, PHB_ITEM pItem)
{
  void *hStr = nullptr; /* = hb_itemGetCPtr(pItem); not needed with UTF-8 conversion */
  HB_SIZE len;
  const char *utf8str = hb_itemGetStrUTF8(pItem, &hStr, &len);
  bool result;

  if (!hStr)
  {
    return false;
  }

  if (context->str_rtrim)
  {
    len = hb_strRTrimLen(utf8str, len, false);
  }

  if (!amf3_encode_int(context, (static_cast<int>(len) << 1) | REFERENCE_BIT))
  {
    return false;
  }

  result = (writeBuffer(context, utf8str, len) != 0);

  hb_strfree(hStr);

  return result;
}

static int amf3_add_index(amfContext *context, PHB_ITEM pHash, PHB_ITEM pItem)
{
  PHB_ITEM pVal;
  int result = 0;

  if (context->use_refs)
  {
    auto pKey = hb_itemNew(nullptr);

    _ref_realItemPtr(pKey, pItem);
    if (!pKey->isPointer() && !pKey->isDateTime())
    {
      hb_itemRelease(pKey);
      return -1;
    }

    if (pHash == context->str_ref)
    {
      result = static_cast<int>(hb_hashLen(pHash) + context->strstr_count);
      /* ->strstr_count > 0 only when some inner context inside
       * user-defined conversion function uses only strstr mode
       * like amf3_FromWA() function f.e. */
    }
    else if (pHash == context->obj_ref)
    {
      result = static_cast<int>(hb_hashLen(pHash) + context->objnref_count);
    }
    else
    {
      result = static_cast<int>(hb_hashLen(pHash));
    }

    pVal = hb_itemPutNS(nullptr, result);

    if (!hb_hashAdd(pHash, pKey, pVal))
    {
      hb_itemRelease(pKey);
      hb_itemRelease(pVal);
      return -1;
    }
    hb_itemRelease(pVal);

    hb_itemRelease(pKey);
  }

  if ((pItem->isString() || pItem->isMemo()) && context->use_strstr)
  {
    auto str_len = hb_itemGetCLen(pItem);
    if (str_len > 3 && str_len < 32)
    { /* do this only for mid-sized strings */
      if (!context->use_refs)
      {
        result = static_cast<int>(context->strstr_count);
      }

      pVal = hb_itemPutNS(nullptr, result); /* put the AMF reference id as value */
      hb_hashAdd(context->strstr_ref, pItem, pVal);
      hb_itemRelease(pVal);
    }
    if (!context->use_refs)
    {
      context->strstr_count++;
    }
  }

  return result;
}

static int amf3_get_index(amfContext *context, PHB_ITEM pHash, PHB_ITEM pItem)
{
  if (context->use_refs)
  {
    auto pKey = hb_itemNew(nullptr);
    HB_SIZE nPos;
    PHB_ITEM pVal;

    _ref_realItemPtr(pKey, pItem);
    if (!pKey->isPointer() && !pKey->isDouble())
    {
      hb_itemRelease(pKey);
      return -1;
    }

    if (context->objnref_count)
    {
      pVal = hb_hashGetItemPtr(pHash, pKey, 0);
      hb_itemRelease(pKey);
      if (pVal)
      {
        return static_cast<int>(hb_itemGetNS(pVal));
      }
    }
    else if (hb_hashScan(pHash, pKey, &nPos))
    {
      hb_itemRelease(pKey);
      return static_cast<int>(nPos - 1);
    }
    else
    {
      hb_itemRelease(pKey);
    }
  }

  if ((pItem->isString() || pItem->isMemo()) && context->use_strstr)
  {
    auto str_len = hb_itemGetCLen(pItem);
    if (str_len > 3 && str_len < 32)
    { /* do this only for mid-sized strings */
      PHB_ITEM pStrIdx = hb_hashGetItemPtr(context->strstr_ref, pItem, 0);
      if (pStrIdx)
      {
        return static_cast<int>(hb_itemGetNS(pStrIdx));
      }
    }
  }

  return -1;
}

static int amf3_encode_reference(amfContext *context, PHB_ITEM pHash, PHB_ITEM pItem, int bit)
{
  int idx;

  if (pItem == nullptr)
  {
    return -1;
  }

  idx = amf3_get_index(context, pHash, pItem);
  if (idx > -1)
  {
    if (idx < MAX_INT)
    {
      if (!amf3_encode_int(context, (idx << (bit + 1)) | (0x00 + bit)))
      {
        return 0;
      }
      return 1;
    }
  }

  if (amf3_add_index(context, pHash, pItem) == -1)
  {
    return 0;
  }

  return -1;
}

static bool amf3_serialize_string(amfContext *context, PHB_ITEM pItem)
{
  int result;
  auto len = hb_itemGetCLen(pItem);

  if (len == 0)
  {
    return writeByte(context, EMPTY_STRING_TYPE);
  }
  else if (context->str_rtrim && hb_strRTrimLen(hb_itemGetCPtr(pItem), len, false) == 0)
  {
    return writeByte(context, EMPTY_STRING_TYPE);
  }

  result = amf3_encode_reference(context, context->str_ref, pItem, 0);
  if (result > -1)
  {
    return result;
  }

  return amf3_encode_string(context, pItem);
}

#if 0
static bool amf3_serialize_object_as_string(amfContext * context, PHB_ITEM pItem)
{
   PHB_ITEM pStr;
   bool result;

   if( pItem->isString() || pItem->isMemo() ) {
      return amf3_serialize_string(context, pItem);
   }

   if( !hb_itemPutC(pStr, hb_itemGetCPtr(pItem)) ) {
      return false;
   }

   result = amf3_serialize_string(context, pStr);

   hb_itemRelease(pStr);
   return result;
}
#endif

static bool amf3_encode_hash(amfContext *context, PHB_ITEM pItem)
{
  PHB_ITEM pKey;
  PHB_ITEM pVal;
  HB_ISIZ i;
  HB_ISIZ len = hb_hashLen(pItem);
  HB_ISIZ nIntKeys = 0;

  if ((((hb_hashGetFlags(pItem) & HB_HASH_KEEPORDER) != 0 && hb_hashGetKeyAt(pItem, 1)->isInteger()) ||
       hb_hashGetFlags(pItem) & HB_HASH_KEEPORDER) == 0)
  {
    for (i = 1; i <= len; i++)
    {
      pKey = hb_hashGetKeyAt(pItem, i);
      if (pKey->isInteger())
      {
        nIntKeys++;
      }
    }
  }

  if (!amf3_encode_int(context, static_cast<int>((nIntKeys << 1) | REFERENCE_BIT)))
  {
    return false;
  }

  for (i = 1; i <= len; i++)
  {
    pKey = hb_hashGetKeyAt(pItem, i);
    pVal = hb_hashGetValueAt(pItem, i);
    if (pKey->isString())
    {
      if (!amf3_serialize_string(context, pKey))
      {
        return false;
      }
      if (!amf3_encode(context, pVal))
      {
        return false;
      }
    }
  }

  if (!writeByte(context, EMPTY_STRING_TYPE))
  {
    return false;
  }

  if (nIntKeys > 0)
  {
    for (i = 1; i <= len; i++)
    {
      pKey = hb_hashGetKeyAt(pItem, i);
      pVal = hb_hashGetValueAt(pItem, i);
      if (pKey->isInteger())
      {
        if (!amf3_encode(context, pVal))
        {
          return false;
        }
      }
    }
  }

  return true;
}

static bool amf3_encode_dynamic_dict(amfContext *context, PHB_ITEM pItem)
{
  HB_ISIZ i;
  HB_ISIZ len = hb_hashLen(pItem);

  for (i = 1; i <= len; i++)
  {
    PHB_ITEM pKey = hb_hashGetKeyAt(pItem, i);
    PHB_ITEM pVal = hb_hashGetValueAt(pItem, i);
    if (pKey->isString())
    {
      if (!amf3_serialize_string(context, pKey))
      {
        return false;
      }
      if (!amf3_encode(context, pVal))
      {
        return false;
      }
    }
  }

  if (!writeByte(context, EMPTY_STRING_TYPE))
  {
    return false;
  }

  return true;
}

static bool amf3_serialize_hash(amfContext *context, PHB_ITEM pItem)
{
  if (context->use_refs)
  {
    if (hb_hashRefs(pItem) > 1)
    {
      HB_BOOL result = amf3_encode_reference(context, context->obj_ref, pItem, 0); // TODO: HB_BOOL -> bool

      if (result > -1)
      {
        return result;
      }
    }
    else
    {
      context->objnref_count++;
    }
  }

  return amf3_encode_hash(context, pItem);
}

static bool amf3_write_hash(amfContext *context, PHB_ITEM pItem)
{
  if (!writeByte(context, ARRAY_TYPE))
  {
    return 0;
  }

  return amf3_serialize_hash(context, pItem);
}

static HB_ISIZ amf3_encode_byte_array(amfContext *context, PHB_ITEM pItem)
{
  HB_ISIZ item_len;
  const char *bytes;

  if (pItem->isString() || pItem->isMemo())
  {
    item_len = hb_itemGetCLen(pItem);
    bytes = hb_itemGetCPtr(pItem);
  }
  else
  {
    return false;
  }

  if (!bytes)
  {
    return false;
  }

  return writeBuffer(context, bytes, item_len);
}

static HB_ISIZ amf3_serialize_byte_array(amfContext *context, PHB_ITEM pItem)
{
  int result;

  if (hb_itemGetCLen(pItem) == 0)
  {
    return writeByte(context, EMPTY_STRING_TYPE);
  }

  result = amf3_encode_reference(context, context->obj_ref, pItem, 0);
  if (result > -1)
  {
    return result;
  }

  if (!amf3_encode_int(context, (static_cast<int>(hb_itemGetCLen(pItem)) << 1) | REFERENCE_BIT))
  {
    return false;
  }

  return amf3_encode_byte_array(context, pItem);
}

static int amf3_encode_date(amfContext *context, PHB_ITEM pItem)
{
  double timestamp;

  if (!amf3_encode_int(context, REFERENCE_BIT))
  {
    return 0;
  }

  timestamp = ((hb_itemGetTD(pItem) - 2440587.5) * 86400000);

  return amfX_encode_double(context, timestamp);
}

static int amf3_serialize_date(amfContext *context, PHB_ITEM pItem)
{
  int result = amf3_encode_reference(context, context->obj_ref, pItem, 0);

  if (result > -1)
  {
    return result;
  }

  return amf3_encode_date(context, pItem);
}

static bool amf3_encode_array(amfContext *context, PHB_ITEM pItem)
{
  HB_SIZE item_len = hb_arrayLen(pItem);
  int i;

  if (!amf3_encode_int(context, (static_cast<int>(item_len) << 1) | REFERENCE_BIT))
  {
    return false;
  }

  if (!writeByte(context, NULL_TYPE))
  {
    return false;
  }

  for (i = 1; i <= static_cast<int>(item_len); i++)
  {
    int result;

    auto pArrayItem = hb_arrayGetItemPtr(pItem, i);
    if (!pArrayItem)
    {
      return false;
    }

    result = amf3_encode(context, pArrayItem);
    if (!result)
    {
      return false;
    }
  }

  return true;
}

static bool amf3_serialize_array(amfContext *context, PHB_ITEM pItem)
{
  if (context->use_refs)
  {
    if (hb_arrayRefs(pItem) > 1)
    {
      int result = amf3_encode_reference(context, context->obj_ref, pItem, 0);

      if (result > -1)
      {
        return result;
      }
    }
    else
    {
      context->objnref_count++;
    }
  }

  return amf3_encode_array(context, pItem);
}

static int amf3_encode_class_def(amfContext *context, PHB_ITEM pClass)
{
  int header;
  int result;
  HB_ISIZ static_attr_len;
  HB_ISIZ i;
  PHB_ITEM class_alias;
  PHB_ITEM static_attrs;
#if 0
   PHB_ITEM attr_len = nullptr;
#endif

  if (!pClass)
  {
    if (!writeByte(context, DYNAMIC))
    {
      return 0;
    }

    if (!writeByte(context, EMPTY_STRING_TYPE))
    {
      return 0;
    }
    return 1;
  }

  if (hb_hashGetCItemPos(pClass, "CLASS_DEF") == 0)
  {
    return 0;
  }

  if (hb_hashGetCItemPos(pClass, "EXTERNALIZABLE_CLASS_DEF") != 0)
  {
    header = EXTERNALIZABLE;
  }
  else if (hb_hashGetCItemPos(pClass, "DYNAMIC_CLASS_DEF") != 0)
  {
    header = DYNAMIC;
  }
  else
  {
    header = STATIC;
  }

  class_alias = hb_hashGetCItemPtr(pClass, "alias");
  if (!class_alias)
  {
    return 0;
  }

  if (header == EXTERNALIZABLE)
  {
    if (!amf3_encode_int(context, header))
    {
      return 0;
    }

    result = amf3_serialize_string(context, class_alias);
    return result;
  }

  static_attrs = hb_hashGetCItemPtr(pClass, "static_attrs");
  if (!static_attrs)
  {
    return 0;
  }

  static_attr_len = hb_arrayLen(static_attrs); /* array this is -- hb_itemGetCLen(static_attrs); */
  if (static_attr_len == -1 || static_attr_len > (MAX_INT >> 4))
  {
    return 0;
  }

  header |= (static_cast<int>(static_attr_len)) << 4;
  if (!amf3_encode_int(context, header))
  {
    return 0;
  }

  result = amf3_serialize_string(context, class_alias);
  /* not needed hb_itemRelease(class_alias); */
  if (!result)
  {
    return 0;
  }

  for (i = 0; i < static_attr_len; i++)
  {
    PHB_ITEM attr_name = hb_itemArrayGet(static_attrs, i);
    if (!attr_name)
    {
      /* not needed hb_itemRelease(static_attrs); */
      return 0;
    }
    result = amf3_serialize_string(context, attr_name);
    hb_itemRelease(attr_name);
    if (!result)
    {
      return 0;
    }
  }

  /* not needed hb_itemRelease(static_attrs); */
  return 1;
}

static int amf3_serialize_class_def(amfContext *context, PHB_ITEM pClass)
{
  int result = amf3_encode_reference(context, context->class_ref, pClass, 0);

  if (result > -1)
  {
    return result;
  }

  return amf3_encode_class_def(context, pClass);
}

/* Get an object's class def. */
static PHB_ITEM class_def_from_class(/* amfContext * context, */ PHB_ITEM pItem)
{
  HB_USHORT uiClass;
  PHB_ITEM pClass;

  /* get Harbour's class id/handle */
  uiClass = hb_objGetClass(pItem);
  if (!uiClass)
  {
    return nullptr;
  }

  /* we left this (python-originated) indirect method of storing
     class "properties" in a hash, this it may be easier in the
     future to implement some additional class mapper instead of
     "tags" which are put into the Harbour class definitions
     right now */

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

/* if we ever want to store the objects dynamically */
#if 0
   hb_itemPutC(pKey, "DYNAMIC_CLASS_DEF");
   hb_itemNew(pValue);
   if( !hb_hashAdd(pClass, pKey, pValue) ) {
      hb_itemRelease(pKey);
      hb_itemRelease(pValue);
      hb_itemRelease(pClass);
      return nullptr;
   }
#endif

  return pClass;
}

static bool amf3_encode_object(amfContext *context, PHB_ITEM pItem)
{
  bool result;
  PHB_ITEM pClass;

  /* serialize emulated ActionScript dynamic object */
  if (strcmp(hb_clsName(hb_objGetClass(pItem)), "AMF_OBJ") == 0)
  {
    auto pAnonHash = hb_itemNew(nullptr);

    if (amf3_serialize_class_def(context, nullptr) == 0)
    {
      hb_itemRelease(pAnonHash);
      return false;
    }

    hb_arrayGet(pItem, OBJAMF_VAR_HASH, pAnonHash);
    result = amf3_encode_dynamic_dict(context, pAnonHash);
    hb_itemRelease(pAnonHash);
    return result;
  }

  pClass = class_def_from_class(/* context, */ pItem);
  if (!pClass)
  {
    return false;
  }

  if (!amf3_serialize_class_def(context, pClass))
  {
    hb_itemRelease(pClass);
    return false;
  }

  if (hb_hashGetCItemPos(pClass, "EXTERNALIZABLE_CLASS_DEF") != 0)
  {
    PHB_ITEM pStr;
    auto pRetCopy = hb_itemNew(nullptr);
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

    pStr = hb_objSendMsg(pObject, "WRITEEXTERNAL", 0);
    if (!pStr)
    {
      hb_itemMove(hb_stackReturnItem(), pRetCopy);
      hb_itemRelease(pRetCopy);
      hb_itemRelease(pClass);
      return false;
    }

    result = amf3_encode_byte_array(context, pStr) != 0;
    hb_itemRelease(pClass);

    hb_itemMove(hb_stackReturnItem(), pRetCopy);
    hb_itemRelease(pRetCopy);

    return result;
  }

  hb_itemRelease(pClass);
  return false;

#if 0
   /* Encoding attributes of class mapped objects is still a TODO, so the Python code is left for reference. */

   /* Encode static attrs */
   PyObject * static_attrs = static_attr_vals_from_class_def(context, class_def, value);
   if( !static_attrs ) {
      Py_DECREF(class_def);
      return 0;
   }

   Py_ssize_t static_attr_len = PySequence_Size(static_attrs);
   if( static_attr_len == -1 ) {
      Py_DECREF(class_def);
      Py_DECREF(static_attrs);
      return 0;
   }

   int i;
   for( i = 0; i < static_attr_len; i++ ) {
      PyObject * static_attr = PySequence_GetItem(static_attrs, i);
      if( !static_attr ) {
         Py_DECREF(static_attrs);
         Py_DECREF(class_def);
         return 0;
      }

      int result = encode_AMF3(context, static_attr);
      Py_DECREF(static_attr);
      if( !result ) {
         Py_DECREF(static_attrs);
         Py_DECREF(class_def);
         return 0;
      }
   }
   Py_DECREF(static_attrs);

   /* Encode dynamic attrs */
   if( PyObject_HasAttrString(class_def, "DYNAMIC_CLASS_DEF") ) {
      PyObject * dynamic_attrs = dynamic_attrs_from_class_def(context, class_def, value);
      if( !dynamic_attrs ) {
         Py_DECREF(class_def);
         return 0;
      }

      int result = encode_dynamic_dict_AMF3(context, dynamic_attrs);
      Py_DECREF(dynamic_attrs);
      if( !result ) {
         Py_DECREF(class_def);
         return 0;
      }
   }

   Py_DECREF(class_def);
   return 1;

#endif
}

static bool amf3_serialize_object(amfContext *context, PHB_ITEM pItem)
{
  int result;

  if (strcmp(hb_clsName(hb_objGetClass(pItem)), "AMF_RAW") == 0)
  {
    auto pStr = hb_itemNew(nullptr);
    hb_arrayGet(pItem, 1, pStr);
    context->position--;
    result = amf3_encode_byte_array(context, pStr) != 0;
    hb_itemRelease(pStr);
    return result;
  }

  if (context->use_refs)
  {
    if (hb_arrayRefs(pItem) > 1)
    {
      result = amf3_encode_reference(context, context->obj_ref, pItem, 0);

      if (result > -1)
      {
        return result;
      }
    }
    else
    {
      context->objnref_count++;
    }
  }

  return amf3_encode_object(context, pItem);
}

static void amf3_conversion_out(amfContext *context, PHB_ITEM pItem)
{
  auto pRetCopy = hb_itemNew(nullptr);
  auto pOuterContext = hb_itemPutPtr(nullptr, context);
  PHB_SYMB pSym = hb_itemGetSymbol(context->conv_function);

  if (pItem == hb_stackReturnItem())
  {
    hb_vmPushSymbol(pSym);
    hb_vmPushNil();
    hb_vmPush(pItem);
    hb_vmPush(pOuterContext);
    hb_vmDo(2);
  }
  else
  {
    hb_itemMove(pRetCopy, hb_stackReturnItem());
    hb_vmPushSymbol(pSym);
    hb_vmPushNil();
    hb_vmPush(pItem);
    hb_vmPush(pOuterContext);
    hb_vmDo(2);
    hb_itemMove(pItem, hb_stackReturnItem());
    hb_itemMove(hb_stackReturnItem(), pRetCopy);
  }
  hb_itemRelease(pOuterContext);
  hb_itemRelease(pRetCopy);
}

static bool amf3_encode(amfContext *context, PHB_ITEM pItem)
{
  bool result = false;

  if (context->conv_function)
  {
    amf3_conversion_out(context, pItem);
  }

  if (pItem->isNil())
  {
    result = amf3_encode_nil(context);
  }
  else if (pItem->isLogical())
  {
    result = amf3_encode_bool(context, pItem);
  }
  else if (pItem->isInteger() || pItem->isLong())
  {
    result = amf3_write_int(context, pItem);
  }
  else if (pItem->isDouble())
  {
    result = amfX_write_double(context, pItem);
  }
  else if (pItem->isString() || pItem->isMemo())
  {
    if (context->encode_ba)
    {
      if (!writeByte(context, BYTE_ARRAY_TYPE))
      {
        result = false;
      }
      else
      {
        result = amf3_serialize_byte_array(context, pItem) != 0;
      }
    }
    else
    {
      if (!writeByte(context, STRING_TYPE))
      {
        result = false;
      }
      else
      {
        result = amf3_serialize_string(context, pItem);
      }
    }
  }
  else if (pItem->isDateTime())
  {
    if (!writeByte(context, DATE_TYPE))
    {
      result = false;
    }
    else
    {
      result = amf3_serialize_date(context, pItem);
    }
  }
  else if (pItem->isObject())
  {
    if (!writeByte(context, OBJECT_TYPE))
    {
      result = false;
    }
    else
    {
      result = amf3_serialize_object(context, pItem);
    }
  }
  else if (pItem->isArray())
  {
    if (!writeByte(context, ARRAY_TYPE))
    {
      result = false;
    }
    else
    {
      result = amf3_serialize_array(context, pItem);
    }
  }
  else if (pItem->isHash())
  {
    result = amf3_write_hash(context, pItem);
  }

  return result;
}

static amfContext *context_setup(PHB_ITEM pFuncSym, bool use_refs, bool str_rtrim, amfContext *outer_context)
{
  auto context = static_cast<amfContext *>(hb_xgrab(sizeof(amfContext)));
  memset(context, 0, sizeof(amfContext));

  context->cBuf = static_cast<char *>(hb_xgrab(sizeof(char) * 8));
  context->position = 0;
  context->length = sizeof(char) * 8;
  context->str_rtrim = str_rtrim;
  context->use_refs = use_refs;
  if (use_refs)
  {
    if (outer_context && outer_context->use_refs)
    {
      context->obj_ref = outer_context->obj_ref;
      context->str_ref = outer_context->str_ref;
      context->class_ref = outer_context->class_ref;
      context->objnref_count = outer_context->objnref_count;
    }
    else
    {
      context->obj_ref = hb_hashNew(nullptr);
      context->str_ref = hb_hashNew(nullptr);
      context->class_ref = hb_hashNew(nullptr);
      context->objnref_count = 0;
    }
  }
  else
  {
    context->obj_ref = nullptr;
    context->str_ref = nullptr;
    context->class_ref = nullptr;
  }

  context->conv_function = pFuncSym;

  /* "strstr" is another optional idea of catching similar strings,
     key in this hash is not the pointer to C char, but the string
     itself and the value is id of the reference */
  context->use_strstr = true;

  if (outer_context)
  {
    context->strstr_count = outer_context->strstr_count;
    if (outer_context->use_strstr)
    {
      context->strstr_ref = outer_context->strstr_ref;
    }
    else
    {
      context->strstr_ref = hb_hashNew(nullptr);
    }

    if (!context->use_refs && outer_context->use_refs)
    {
      context->strstr_count += hb_hashLen(outer_context->str_ref);
    }
  }
  else
  {
    context->strstr_count = 0;
    context->strstr_ref = hb_hashNew(nullptr);
  }

  return context;
}

static void context_release(amfContext *context, amfContext *outer_context)
{
  if (outer_context && outer_context->use_refs)
  {
    outer_context->objnref_count = context->objnref_count;
  }
  else if (context->use_refs)
  {
    hb_itemRelease(context->obj_ref);
    hb_itemRelease(context->str_ref);
    hb_itemRelease(context->class_ref);
  }

  if (context->use_strstr)
  {
    if (outer_context)
    {
      if (!context->use_refs && outer_context->use_refs)
      {
        context->strstr_count -= hb_hashLen(outer_context->str_ref);
      }

      outer_context->strstr_count = context->strstr_count;

      if (!outer_context->strstr_ref)
      {
        hb_itemRelease(context->strstr_ref);
      }
    }
    else
    {
      hb_itemRelease(context->strstr_ref);
    }
  }
}

HB_FUNC(AMF3_FROMWA)
{
  auto pWhile = hb_param(1, Harbour::Item::BLOCK);
  auto pFor = hb_param(2, Harbour::Item::BLOCK);
  auto pFields = hb_param(3, Harbour::Item::ARRAY);
  HB_ULONG nCount = hb_parnldef(4, 0);
  bool str_rtrim = hb_parldef(5, true);
  auto nPkg = static_cast<HB_USHORT>(hb_parnidef(6, 0));
  amfContext *outer_context = static_cast<amfContext *>(hb_parptr(7));

  DBORDERINFO pInfo;
  int iOrd;
  HB_USHORT uiFields;
  HB_ULONG uiRecCount = 0;
  HB_ULONG uiRecNo = 0;
  bool bNoFieldPassed = (pFields == nullptr || hb_arrayLen(pFields) == 0);
  HB_BOOL bEof = false;
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  PHB_ITEM pItem;
  HB_USHORT uiFieldCopy = 0; /* GCC knows better (warns) */
  HB_USHORT uiIter;
  amfContext *context;
  bool bPredictLen = (!pWhile && !pFor);

  bool bAsArray = !nPkg;
  PHB_ITEM pFieldNames = nullptr; /* again GCC knows better */
  PHB_ITEM pField;

  if (pArea != nullptr)
  {
    memset(&pInfo, 0, sizeof(pInfo));
    pInfo.itmResult = hb_itemPutNI(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_NUMBER, &pInfo);
    iOrd = hb_itemGetNI(pInfo.itmResult);
    if (iOrd > 0)
    {
      SELF_ORDINFO(pArea, DBOI_KEYCOUNT, &pInfo);
      uiRecCount = hb_itemGetNL(pInfo.itmResult);
      SELF_ORDINFO(pArea, DBOI_POSITION, &pInfo);
      uiRecNo = hb_itemGetNL(pInfo.itmResult);

      hb_itemRelease(pInfo.itmResult);
    }
    else
    {
      hb_itemRelease(pInfo.itmResult);

      if (SELF_RECCOUNT(pArea, &uiRecCount) != Harbour::SUCCESS)
      {
        return;
      }

      if (SELF_RECNO(pArea, &uiRecNo) != Harbour::SUCCESS)
      {
        return;
      }
    }

    pItem = hb_itemNew(nullptr);

    context = context_setup(nullptr, false, str_rtrim, outer_context);

    if (bPredictLen)
    {
      if (nCount == 0 || uiRecNo + nCount > uiRecCount)
      {
        nCount = uiRecCount - uiRecNo + 1;
      }
    }
    else
    {
      uiRecCount = 0;
      while ((nCount == 0 || uiRecCount < nCount) && (!pWhile || hb_itemGetL(hb_vmEvalBlock(pWhile))))
      {
        if (SELF_EOF(pArea, &bEof) != Harbour::SUCCESS)
        {
          break;
        }

        if (bEof)
        {
          break;
        }

        if (!pFor || hb_itemGetL(hb_vmEvalBlock(pFor)))
        {
          uiRecCount++;
        }

        if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
        {
          break;
        }
      }
      nCount = uiRecCount;

      if (iOrd > 0)
      {
        memset(&pInfo, 0, sizeof(pInfo));
        pInfo.itmNewVal = hb_itemPutNL(nullptr, uiRecNo);
        pInfo.itmResult = hb_itemPutL(nullptr, false);
        SELF_ORDINFO(pArea, DBOI_POSITION, &pInfo);
        hb_itemRelease(pInfo.itmNewVal);
        hb_itemRelease(pInfo.itmResult);
      }
      else
      {
        SELF_GOTO(pArea, uiRecNo);
      }
    }

    /* TODO: should be if( writeByte() ), before we make a variant that operates on streams directly */

    writeByte(context, ARRAY_TYPE);
    amf3_encode_int(context, (static_cast<int>(nCount) << 1) | REFERENCE_BIT);
    writeByte(context, NULL_TYPE);

    SELF_FIELDCOUNT(pArea, &uiFields);

    if (!bNoFieldPassed)
    {
      uiFieldCopy = static_cast<HB_USHORT>(hb_arrayLen(pFields));

      for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
      {
        auto szFieldName = hb_arrayGetCPtr(pFields, uiIter);
        if (szFieldName)
        {
          int iPos = hb_rddFieldIndex(pArea, szFieldName);

          if (iPos)
          {
            auto pFieldNum = hb_itemPutNI(nullptr, iPos);
            hb_itemArrayPut(pFields, uiIter, pFieldNum);
            hb_itemRelease(pFieldNum);
            continue;
          }
        }

        if (hb_arrayDel(pFields, uiIter))
        {
          hb_arraySize(pFields, hb_arrayLen(pFields) - 1);
          uiIter--;
          uiFieldCopy--;
        }
      }
    }

    if (!bAsArray)
    {
      pFieldNames = hb_itemNew(nullptr);
      if (bNoFieldPassed)
      {
        hb_arrayNew(pFieldNames, uiFields);
        for (uiIter = 1; uiIter <= uiFields; uiIter++)
        {
          auto szName = static_cast<char *>(hb_xgrab(pArea->uiMaxFieldNameLength + 1));
          pField = hb_itemNew(nullptr);
          szName[0] = '\0';
          SELF_FIELDNAME(pArea, uiIter, szName);
          hb_itemPutCPtr(pField, szName);
          hb_arraySet(pFieldNames, uiIter, pField);
          hb_itemRelease(pField);
        }
      }
      else
      {
        hb_arrayNew(pFieldNames, uiFieldCopy);
        for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
        {
          auto szName = static_cast<char *>(hb_xgrab(pArea->uiMaxFieldNameLength + 1));
          pField = hb_itemNew(nullptr);
          szName[0] = '\0';
          SELF_FIELDNAME(pArea, static_cast<HB_USHORT>(hb_itemGetNI(hb_arrayGetItemPtr(pFields, uiIter))), szName);
          hb_itemPutCPtr(pField, szName);
          hb_arraySet(pFieldNames, uiIter, pField);
          hb_itemRelease(pField);
        }
      }
    }

    uiRecCount = 0;
    while ((nCount == 0 || uiRecCount <= nCount) && (!pWhile || hb_itemGetL(hb_vmEvalBlock(pWhile))))
    {
      if (SELF_EOF(pArea, &bEof) != Harbour::SUCCESS)
      {
        break;
      }

      if (bEof)
      {
        break;
      }

      if (!pFor || hb_itemGetL(hb_vmEvalBlock(pFor)))
      {
        if (bAsArray)
        {
          writeByte(context, ARRAY_TYPE);
          if (bNoFieldPassed)
          {
            amf3_encode_int(context, (static_cast<int>(uiFields) << 1) | REFERENCE_BIT);
            writeByte(context, NULL_TYPE);
            for (uiIter = 1; uiIter <= uiFields; uiIter++)
            {
              SELF_GETVALUE(pArea, uiIter, pItem);
              amf3_encode(context, pItem);
            }
          }
          else
          {
            amf3_encode_int(context, (static_cast<int>(uiFieldCopy) << 1) | REFERENCE_BIT);
            writeByte(context, NULL_TYPE);
            for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
            {
              SELF_GETVALUE(pArea,
                            static_cast<HB_USHORT>(
                                hb_itemGetNI(hb_arrayGetItemPtr(pFields, uiIter))) /* hb_arrayGetNI(pFields, uiIter) */,
                            pItem);
              amf3_encode(context, pItem);
            }
          }
        }
        else
        {
          auto pValue = hb_itemNew(nullptr);

          writeByte(context, OBJECT_TYPE);
#if 0
               amf3_encode_int(context, (static_cast<int>(1)) << 1 | REFERENCE_BIT);
#endif
          writeByte(context, DYNAMIC);
          writeByte(context, EMPTY_STRING_TYPE);
          if (bNoFieldPassed)
          {
            for (uiIter = 1; uiIter <= uiFields; uiIter++)
            {
              SELF_GETVALUE(pArea, uiIter, pValue);
              amf3_serialize_string(context, hb_arrayGetItemPtr(pFieldNames, uiIter));
              amf3_encode(context, pValue);
            }
          }
          else
          {
            for (uiIter = 1; uiIter <= uiFieldCopy; uiIter++)
            {
              SELF_GETVALUE(pArea, static_cast<HB_USHORT>(hb_itemGetNI(hb_arrayGetItemPtr(pFields, uiIter))), pValue);
              amf3_serialize_string(context, hb_arrayGetItemPtr(pFieldNames, uiIter));
              amf3_encode(context, pValue);
            }
          }

          hb_itemRelease(pValue);
          writeByte(context, EMPTY_STRING_TYPE);
        }
        uiRecCount++;
      }

      if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
      {
        break;
      }
    }

    hb_itemRelease(pItem);

    if (!bAsArray)
    {
      hb_itemRelease(pFieldNames);
    }

    context->cBuf = static_cast<char *>(hb_xrealloc(context->cBuf, sizeof(char) * context->position + 1));

    hb_retclen_buffer(context->cBuf, context->position);

    context_release(context, outer_context);

    hb_xfree(context);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(AMF3_ENCODE)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);
  auto pFuncSym = hb_param(2, Harbour::Item::SYMBOL);
  bool lBA = hb_parldef(3, false);
  bool lRetval;

  if (!pItem)
  {
    return;
  }

  auto context = static_cast<amfContext *>(hb_xgrab(sizeof(amfContext)));
  memset(context, 0, sizeof(amfContext));

  context->cBuf = static_cast<char *>(hb_xgrab(sizeof(char) * 8));
  context->position = 0;
  context->length = sizeof(char) * 8;
  context->str_rtrim = false;
  context->obj_ref = hb_hashNew(nullptr);
  context->str_ref = hb_hashNew(nullptr);
  context->class_ref = hb_hashNew(nullptr);
  context->use_refs = true;
  context->conv_function = pFuncSym;
  context->encode_ba = lBA;
  context->objnref_count = 0;

  /* "strstr" is another optional idea of catching similar strings,
     key in this hash is not the pointer to C char, but the string
     itself and the value is id of the reference */
  context->use_strstr = true;
  context->strstr_count = 0;
  context->strstr_ref = hb_hashNew(nullptr);

  lRetval = amf3_encode(context, pItem);

  if (context->use_refs)
  {
    hb_itemRelease(context->obj_ref);
    hb_itemRelease(context->str_ref);
    hb_itemRelease(context->class_ref);
  }

  if (context->use_strstr)
  {
    hb_itemRelease(context->strstr_ref);
  }

#if 0
   if( context->conv_function ) {
      hb_itemRelease(context->conv_function);
   }
#endif

  if (!lRetval)
  {
    hb_xfree(context->cBuf);
    hb_xfree(context);
    return;
  }

  context->cBuf = static_cast<char *>(hb_xrealloc(context->cBuf, sizeof(char) * context->position + 1));

  hb_retclen_buffer(context->cBuf, context->position);
  hb_xfree(context);
}
