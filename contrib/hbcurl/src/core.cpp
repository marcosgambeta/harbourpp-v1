//
// libcurl 'easy' API - Harbour interface.
//
// Copyright 2008-2010 Viktor Szakats (vszakats.net/harbour)
// originally based on:
// Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
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

#include <curl/curl.h>
#if LIBCURL_VERSION_NUM < 0x070A03
#include <curl/easy.h>
#endif
#if LIBCURL_VERSION_NUM < 0x070C00
#include <curl/types.h>
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>
#include <hbvm.hpp>
#include "hbhash.hpp"

#include "hbcurl.ch"

#define HB_CURL_OPT_BOOL(n) (HB_ISLOG(n) ? (long)hb_parl(n) : hb_parnldef(n, 1))
#define HB_CURL_OPT_LARGENUM(n) ((curl_off_t)hb_parnint(n))

// NOTE: Harbour requires libcurl 7.17.0 or upper.
//       This was the version where curl_easy_setopt() started to
//       make copies of passed strings, which we currently require.
//       Update: This requirement is now sorted out by local string
//               buffering logic used with pre-7.17.0 versions of
//               libcurl.
//       [vszakats]

#if LIBCURL_VERSION_NUM < 0x071100
#ifndef HB_CURL_HASH_STRINGS
#define HB_CURL_HASH_STRINGS
#endif
#endif

// Fall back to return simple error if special abort signal is not available.
#if !defined(CURL_READFUNC_ABORT) // Introduced in LIBCURL_VERSION_NUM >= 0x070C01
#define CURL_READFUNC_ABORT ((size_t)-1)
#endif

typedef struct _HB_CURL
{
  CURL *curl;

#if LIBCURL_VERSION_NUM >= 0x073800
  curl_mime *mime;
#else
  struct curl_httppost *pHTTPPOST_First;
  struct curl_httppost *pHTTPPOST_Last;
#endif
  struct curl_slist *pHTTPHEADER;
  struct curl_slist *pHTTP200ALIASES;
  struct curl_slist *pQUOTE;
  struct curl_slist *pPOSTQUOTE;
  struct curl_slist *pPREQUOTE;
  struct curl_slist *pTELNETOPTIONS;
  struct curl_slist *pMAIL_RCPT;
  struct curl_slist *pRESOLVE;

  char *ul_name;
  HB_FHANDLE ul_handle;

  char *dl_name;
  HB_FHANDLE dl_handle;

  unsigned char *ul_ptr;
  size_t ul_len;
  size_t ul_pos;

  unsigned char *dl_ptr;
  size_t dl_len;
  size_t dl_pos;

  char *er_ptr;
  size_t er_len;

  PHB_ITEM pProgressCallback;
  PHB_ITEM pDebugCallback;
  PHB_ITEM pWriteFunctionCallback;

#ifdef HB_CURL_HASH_STRINGS
  PHB_HASH_TABLE pHash;
#endif

} HB_CURL, *PHB_CURL;

// Multi interface
// ---------------

#if LIBCURL_VERSION_NUM >= 0x070906
typedef struct _HB_CURLM
{
  CURLM *curlm;
} HB_CURLM, *PHB_CURLM;
#endif

// functions to keep passed string values accessible even if HVM
// destroy them. It's necessary for old CURL versions which do not
// make own copy of passed strings

#ifdef HB_CURL_HASH_STRINGS

#define HB_CURL_HASH_TABLE_SIZE 509UL

// returns a hash key
static HB_HASH_FUNC(hb_curl_HashKey) // HB_SIZE func(const void * Value, const void * Cargo)
{
  HB_SIZE ulSum = 0;
  auto szName = static_cast<const char *>(Value);

  while (*szName)
  {
    ulSum += *szName++;
  }

  HB_SYMBOL_UNUSED(HashPtr);
  HB_SYMBOL_UNUSED(Cargo);

  return ulSum % HB_CURL_HASH_TABLE_SIZE;
}

// deletes a string
static HB_HASH_FUNC(hb_curl_HashDel)
{
  hb_xfree(static_cast<void *>(Value));
  HB_SYMBOL_UNUSED(HashPtr);
  HB_SYMBOL_UNUSED(Cargo);
  return 1;
}

// compares two strings
static HB_HASH_FUNC(hb_curl_HashCmp)
{
  HB_SYMBOL_UNUSED(HashPtr);
  return strcmp(static_cast<const char *>(Value), static_cast<const char *>(Cargo));
}

static const char *hb_curl_StrHashNew(PHB_CURL hb_curl, const char *szValue)
{
  if (szValue != nullptr)
  {
    if (!hb_curl->pHash)
    {
      hb_curl->pHash = hb_hashTableCreate(HB_CURL_HASH_TABLE_SIZE, hb_curl_HashKey, hb_curl_HashDel, hb_curl_HashCmp);
    }

    auto szHash = static_cast<char *>(hb_hashTableFind(hb_curl->pHash, szValue));
    if (!szHash)
    {
      szHash = hb_strdup(szValue);
      hb_hashTableAdd(hb_curl->pHash, szHash, szHash);
    }
    return szHash;
  }
  else
  {
    return nullptr;
  }
}

#define hb_curl_StrHash(c, s) hb_curl_StrHashNew((c), (s))

#else

#define hb_curl_StrHash(c, s) (s)

#endif // HB_CURL_HASH_STRINGS

#if LIBCURL_VERSION_NUM >= 0x075500
#define hb_bitShift(b, s) ((b) << (s))
static char *hb_curl_protnames(unsigned long bitmask)
{
  // now we need 166 bytes for 30 protocol names and trailing 0 so 200 is enough
  HB_SIZE nLen = 200, nDst = 0;
  auto buffer = static_cast<char *>(hb_xgrab(nLen));
  unsigned long prot;

  for (prot = 1; prot != 0 && prot <= bitmask; prot <<= 1)
  {
    if (prot & bitmask)
    {
      const char *szProt = nullptr;
      switch (prot & bitmask)
      {
      case HB_CURLPROTO_HTTP:
        szProt = "HTTP";
        break;
      case HB_CURLPROTO_HTTPS:
        szProt = "HTTPS";
        break;
      case HB_CURLPROTO_FTP:
        szProt = "FTP";
        break;
      case HB_CURLPROTO_FTPS:
        szProt = "FTPS";
        break;
      case HB_CURLPROTO_SCP:
        szProt = "SCP";
        break;
      case HB_CURLPROTO_SFTP:
        szProt = "SFTP";
        break;
      case HB_CURLPROTO_TELNET:
        szProt = "TELNET";
        break;
      case HB_CURLPROTO_LDAP:
        szProt = "LDAP";
        break;
      case HB_CURLPROTO_LDAPS:
        szProt = "LDAPS";
        break;
      case HB_CURLPROTO_DICT:
        szProt = "DICT";
        break;
      case HB_CURLPROTO_FILE:
        szProt = "FILE";
        break;
      case HB_CURLPROTO_TFTP:
        szProt = "TFTP";
        break;
      case HB_CURLPROTO_IMAP:
        szProt = "IMAP";
        break;
      case HB_CURLPROTO_IMAPS:
        szProt = "IMAPS";
        break;
      case HB_CURLPROTO_POP3:
        szProt = "POP3";
        break;
      case HB_CURLPROTO_POP3S:
        szProt = "POP3S";
        break;
      case HB_CURLPROTO_SMTP:
        szProt = "SMTP";
        break;
      case HB_CURLPROTO_SMTPS:
        szProt = "SMTPS";
        break;
      case HB_CURLPROTO_RTSP:
        szProt = "RTSP";
        break;
      case HB_CURLPROTO_RTMP:
        szProt = "RTMP";
        break;
      case HB_CURLPROTO_RTMPT:
        szProt = "RTMPT";
        break;
      case HB_CURLPROTO_RTMPE:
        szProt = "RTMPE";
        break;
      case HB_CURLPROTO_RTMPTE:
        szProt = "RTMPTE";
        break;
      case HB_CURLPROTO_RTMPS:
        szProt = "RTMPS";
        break;
      case HB_CURLPROTO_RTMPTS:
        szProt = "RTMPTS";
        break;
      case HB_CURLPROTO_GOPHER:
        szProt = "GOPHER";
        break;
      case HB_CURLPROTO_SMB:
        szProt = "SMB";
        break;
      case HB_CURLPROTO_SMBS:
        szProt = "SMBS";
        break;
      case HB_CURLPROTO_MQTT:
        szProt = "MQTT";
        break;
      case HB_CURLPROTO_GOPHERS:
        szProt = "GOPHERS";
        break;
      }
      if (szProt != nullptr)
      {
        HB_SIZE l = strlen(szProt);
        if (nDst + l + (nDst ? 1 : 0) < nLen)
        {
          if (nDst)
          {
            buffer[nDst++] = ',';
          }
          memcpy(&buffer[nDst], szProt, l);
          nDst += l;
        }
      }
    }
  }
  buffer[nDst] = '\0';
  return buffer;
}
#endif

// Global initialization/deinitialization
// --------------------------------------

static void *hb_curl_xgrab(size_t size)
{
  return size > 0 ? hb_xgrab(size) : nullptr;
}

static void hb_curl_xfree(void *p)
{
  if (p != nullptr)
  {
    hb_xfree(p);
  }
}

static void *hb_curl_xrealloc(void *p, size_t size)
{
  return size > 0 ? (p ? hb_xrealloc(p, size) : hb_xgrab(size)) : nullptr;
}

static char *hb_curl_strdup(const char *s)
{
  return hb_strdup(s);
}

static void *hb_curl_calloc(size_t nelem, size_t elsize)
{
  size_t size = nelem * elsize;
  void *ptr = hb_xgrab(size);

  memset(ptr, '\0', size);

  return ptr;
}

static void hb_curl_retcode(CURLcode code)
{
  hb_retni(static_cast<int>(code));
}

HB_FUNC(CURL_GLOBAL_INIT)
{
#if LIBCURL_VERSION_NUM >= 0x070A08 // Not documented. GUESS.
  hb_curl_retcode(curl_global_init_mem(hb_parnldef(1, CURL_GLOBAL_ALL), hb_curl_xgrab, hb_curl_xfree, hb_curl_xrealloc,
                                       hb_curl_strdup, hb_curl_calloc));
#else
  hb_curl_retcode(curl_global_init_mem(hb_parnldef(1, CURL_GLOBAL_ALL)));
#endif
}

HB_FUNC(CURL_GLOBAL_CLEANUP)
{
  curl_global_cleanup();
}

// Callbacks
// ---------

static size_t hb_curl_read_dummy_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  HB_SYMBOL_UNUSED(buffer);
  HB_SYMBOL_UNUSED(size);
  HB_SYMBOL_UNUSED(nmemb);
  HB_SYMBOL_UNUSED(Cargo);

  return 0;
}

static size_t hb_curl_read_file_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    if (hb_curl->ul_handle == FS_ERROR)
    {
      hb_curl->ul_handle = hb_fsOpen(hb_curl->ul_name, FO_READ);

      if (hb_curl->ul_handle == FS_ERROR)
      {
        return static_cast<size_t>(-1);
      }
    }

    auto ret = static_cast<size_t>(hb_fsReadLarge(hb_curl->ul_handle, buffer, size * nmemb));

    return hb_fsError() ? CURL_READFUNC_ABORT : ret;
  }

  return static_cast<size_t>(-1);
}

static size_t hb_curl_read_fhandle_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    if (hb_curl->ul_handle == FS_ERROR)
    {
      return static_cast<size_t>(-1);
    }

    auto ret = static_cast<size_t>(hb_fsReadLarge(hb_curl->ul_handle, buffer, size * nmemb));

    return hb_fsError() ? CURL_READFUNC_ABORT : ret;
  }

  return static_cast<size_t>(-1);
}

static size_t hb_curl_read_buff_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    size_t nTodo = size * nmemb;
    size_t nLeft = hb_curl->ul_len - hb_curl->ul_pos;

    if (nTodo > nLeft)
    {
      nTodo = nLeft;
    }

    hb_xmemcpy(buffer, hb_curl->ul_ptr + hb_curl->ul_pos, nTodo);

    hb_curl->ul_pos += nTodo;

    return nTodo;
  }

  return (size_t)-1;
}

static size_t hb_curl_write_file_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    if (hb_curl->dl_handle == FS_ERROR)
    {
      hb_curl->dl_handle = hb_fsCreate(hb_curl->dl_name, FC_NORMAL);

      if (hb_curl->dl_handle == FS_ERROR)
      {
        return static_cast<size_t>(-1);
      }
    }

    return hb_fsWriteLarge(hb_curl->dl_handle, buffer, size * nmemb);
  }

  return static_cast<size_t>(-1);
}

static size_t hb_curl_write_fhandle_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    if (hb_curl->dl_handle == FS_ERROR)
    {
      return static_cast<size_t>(-1);
    }

    return hb_fsWriteLarge(hb_curl->dl_handle, buffer, size * nmemb);
  }

  return static_cast<size_t>(-1);
}

#define HB_CURL_DL_BUFF_SIZE_INIT (CURL_MAX_WRITE_SIZE * 4)
#define HB_CURL_DL_BUFF_SIZE_INCR (CURL_MAX_WRITE_SIZE * 4)
#if LIBCURL_VERSION_NUM >= 0x070100
#define HB_CURL_ER_BUFF_SIZE_INIT (CURL_ERROR_SIZE)
#endif

static size_t hb_curl_write_buff_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  auto hb_curl = static_cast<PHB_CURL>(Cargo);

  if (hb_curl != nullptr)
  {
    size_t nTodo = size * nmemb;
    size_t nLeft = hb_curl->dl_len - hb_curl->dl_pos;

    if (nTodo > nLeft)
    {
      hb_curl->dl_len += HB_CURL_DL_BUFF_SIZE_INCR;
      hb_curl->dl_ptr = static_cast<unsigned char *>(hb_xrealloc(hb_curl->dl_ptr, hb_curl->dl_len));
    }

    hb_xmemcpy(hb_curl->dl_ptr + hb_curl->dl_pos, buffer, nTodo);

    hb_curl->dl_pos += nTodo;

    return nTodo;
  }

  return static_cast<size_t>(-1);
}

#if LIBCURL_VERSION_NUM >= 0x072000
static int hb_curl_xferinfo_callback(void *Cargo, curl_off_t dltotal, curl_off_t dlnow, curl_off_t ultotal,
                                     curl_off_t ulnow)
#else
static int hb_curl_progress_callback(void *Cargo, double dltotal, double dlnow, double ultotal, double ulnow)
#endif
{
  if (Cargo)
  {
    if (hb_vmRequestReenter())
    {
      hb_vmPushEvalSym();
      hb_vmPush(static_cast<PHB_ITEM>(Cargo));
#if LIBCURL_VERSION_NUM >= 0x072000
      hb_vmPushNumInt(static_cast<HB_MAXINT>(ulnow > 0 ? ulnow : dlnow));
      hb_vmPushNumInt(static_cast<HB_MAXINT>(ultotal > 0 ? ultotal : dltotal));
#else
      hb_vmPushDouble(ulnow > 0 ? ulnow : dlnow, HB_DEFAULT_DECIMALS);
      hb_vmPushDouble(ultotal > 0 ? ultotal : dltotal, HB_DEFAULT_DECIMALS);
#endif
      hb_vmSend(2);

      if (hb_parl(-1))
      {
        return 1; // Abort
      }

      hb_vmRequestRestore();
    }
  }

  return 0;
}

static int hb_curl_debug_callback(CURL *handle, curl_infotype type, char *data, size_t size, void *Cargo)
{
  HB_SYMBOL_UNUSED(handle);

  if (Cargo)
  {
    auto hb_curl = static_cast<PHB_CURL>(Cargo);
    if (hb_curl->pDebugCallback && hb_vmRequestReenter())
    {
      hb_vmPushEvalSym();
      hb_vmPush(hb_curl->pDebugCallback);
      hb_vmPushInteger(type);
      hb_vmPushString(data, size);
      hb_vmSend(2);

      hb_vmRequestRestore();
    }
  }

  return 0;
}

static size_t hb_curl_writefunction_callback(void *buffer, size_t size, size_t nmemb, void *Cargo)
{
  PHB_CURL hb_curl = (PHB_CURL)Cargo;

  if (hb_curl->pWriteFunctionCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(hb_curl->pWriteFunctionCallback);
    hb_vmPushString((const char *)buffer, size * nmemb);
    hb_vmSend(1);
    hb_vmRequestRestore();
    return size * nmemb;
  }
  return 0;
}

// Helpers
// -------

#if LIBCURL_VERSION_NUM < 0x073800
static void hb_curl_form_free(struct curl_httppost **ptr)
{
  if (ptr && *ptr)
  {
    curl_formfree(*ptr);
    *ptr = nullptr;
  }
}
#endif

static void hb_curl_slist_free(struct curl_slist **ptr)
{
  if (ptr && *ptr)
  {
    curl_slist_free_all(*ptr);
    *ptr = nullptr;
  }
}

static void hb_curl_file_ul_free(PHB_CURL hb_curl)
{
  if (hb_curl != nullptr && hb_curl->ul_name)
  {
    hb_xfree(hb_curl->ul_name);
    hb_curl->ul_name = nullptr;

    if (hb_curl->ul_handle != FS_ERROR)
    {
      hb_fsClose(hb_curl->ul_handle);
      hb_curl->ul_handle = FS_ERROR;
    }
  }
}

static void hb_curl_file_dl_free(PHB_CURL hb_curl)
{
  if (hb_curl != nullptr && hb_curl->dl_name)
  {
    hb_xfree(hb_curl->dl_name);
    hb_curl->dl_name = nullptr;

    if (hb_curl->dl_handle != FS_ERROR)
    {
      hb_fsClose(hb_curl->dl_handle);
      hb_curl->dl_handle = FS_ERROR;
    }
  }
}

static void hb_curl_buff_ul_free(PHB_CURL hb_curl)
{
  if (hb_curl != nullptr && hb_curl->ul_ptr)
  {
    hb_xfree(hb_curl->ul_ptr);
    hb_curl->ul_ptr = nullptr;
    hb_curl->ul_len = 0;
    hb_curl->ul_pos = 0;
  }
}

#if LIBCURL_VERSION_NUM >= 0x070100
static void hb_curl_buff_er_free(PHB_CURL hb_curl)
{
  if (hb_curl && hb_curl->er_ptr)
  {
    hb_xfree(hb_curl->er_ptr);
    hb_curl->er_ptr = nullptr;
    hb_curl->er_len = 0;
  }
}
#endif

static void hb_curl_buff_dl_free(PHB_CURL hb_curl)
{
  if (hb_curl != nullptr && hb_curl->dl_ptr)
  {
    hb_xfree(hb_curl->dl_ptr);
    hb_curl->dl_ptr = nullptr;
    hb_curl->dl_len = 0;
    hb_curl->dl_pos = 0;
  }
}

// Constructor/Destructor
// ----------------------

static void PHB_CURL_free(PHB_CURL hb_curl, HB_BOOL bFree)
{
  curl_easy_setopt(hb_curl->curl, CURLOPT_READFUNCTION, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_READDATA, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEDATA, nullptr);
#if LIBCURL_VERSION_NUM >= 0x072000
  curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFOFUNCTION, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFODATA, nullptr);
#else
  curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSFUNCTION, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSDATA, nullptr);
#endif

  // Some extra safety. Set these to NULL, before freeing their pointers.
#if LIBCURL_VERSION_NUM < 0x073800
  curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPPOST, nullptr);
#endif
  curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPHEADER, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP200ALIASES, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_QUOTE, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_POSTQUOTE, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_PREQUOTE, nullptr);
  curl_easy_setopt(hb_curl->curl, CURLOPT_TELNETOPTIONS, nullptr);
#if LIBCURL_VERSION_NUM >= 0x071400
  curl_easy_setopt(hb_curl->curl, CURLOPT_MAIL_RCPT, nullptr);
#endif

#if LIBCURL_VERSION_NUM >= 0x073800
  if (hb_curl->mime)
  {
    curl_mime_free(hb_curl->mime);
    hb_curl->mime = nullptr;
  }
#else
  hb_curl_form_free(&hb_curl->pHTTPPOST_First);
  hb_curl->pHTTPPOST_Last = nullptr;
#endif
  hb_curl_slist_free(&hb_curl->pHTTPHEADER);
  hb_curl_slist_free(&hb_curl->pHTTP200ALIASES);
  hb_curl_slist_free(&hb_curl->pQUOTE);
  hb_curl_slist_free(&hb_curl->pPOSTQUOTE);
  hb_curl_slist_free(&hb_curl->pPREQUOTE);
  hb_curl_slist_free(&hb_curl->pTELNETOPTIONS);
  hb_curl_slist_free(&hb_curl->pMAIL_RCPT);
  hb_curl_slist_free(&hb_curl->pRESOLVE);

  hb_curl_file_ul_free(hb_curl);
  hb_curl_file_dl_free(hb_curl);

  hb_curl_buff_ul_free(hb_curl);
  hb_curl_buff_dl_free(hb_curl);
#if LIBCURL_VERSION_NUM >= 0x070100
  hb_curl_buff_er_free(hb_curl);
#endif

  if (hb_curl->pProgressCallback)
  {
    hb_itemRelease(hb_curl->pProgressCallback);
    hb_curl->pProgressCallback = nullptr;
  }

  if (hb_curl->pDebugCallback)
  {
    hb_itemRelease(hb_curl->pDebugCallback);
    hb_curl->pDebugCallback = nullptr;
  }

  if (hb_curl->pWriteFunctionCallback)
  {
    hb_itemRelease(hb_curl->pWriteFunctionCallback);
    hb_curl->pWriteFunctionCallback = nullptr;
  }

#ifdef HB_CURL_HASH_STRINGS
  if (hb_curl->pHash)
  {
    hb_hashTableKill(hb_curl->pHash);
    hb_curl->pHash = nullptr;
  }
#endif

  if (bFree)
  {
    curl_easy_cleanup(hb_curl->curl);
    hb_xfree(hb_curl);
  }
#if LIBCURL_VERSION_NUM >= 0x070C01
  else
  {
    curl_easy_reset(hb_curl->curl);
  }
#endif
}

// NOTE: Will create a new one. If 'from' is specified, the new one
//       will be based on the 'from' one.

static PHB_CURL PHB_CURL_create(CURL *from)
{
  CURL *curl = from ? curl_easy_duphandle(from) : curl_easy_init();

  if (curl)
  {
    auto hb_curl = static_cast<PHB_CURL>(hb_xgrab(sizeof(HB_CURL)));

    memset(hb_curl, 0, sizeof(HB_CURL));
    hb_curl->curl = curl;

    return hb_curl;
  }
  else
  {
    return nullptr;
  }
}

static HB_GARBAGE_FUNC(PHB_CURL_release)
{
  auto hb_curl_ptr = static_cast<PHB_CURL *>(Cargo);

  // Check if pointer is not NULL to avoid multiple freeing
  if (hb_curl_ptr && *hb_curl_ptr)
  {
    // Destroy the object
    PHB_CURL_free(*hb_curl_ptr, true);
    *hb_curl_ptr = nullptr;
  }
}

static HB_GARBAGE_FUNC(PHB_CURL_mark)
{
  auto hb_curl_ptr = static_cast<PHB_CURL *>(Cargo);

  if (hb_curl_ptr && *hb_curl_ptr)
  {
    PHB_CURL hb_curl = *hb_curl_ptr;

    if (hb_curl->pProgressCallback)
    {
      hb_gcMark(hb_curl->pProgressCallback);
    }

    if (hb_curl->pDebugCallback)
    {
      hb_gcMark(hb_curl->pDebugCallback);
    }

    if (hb_curl->pWriteFunctionCallback)
    {
      hb_gcMark(hb_curl->pWriteFunctionCallback);
    }
  }
}

static const HB_GC_FUNCS s_gcCURLFuncs = {PHB_CURL_release, PHB_CURL_mark};

static void PHB_CURL_ret(PHB_CURL from)
{
  auto ph = static_cast<void **>(hb_gcAllocate(sizeof(PHB_CURL), &s_gcCURLFuncs));

  *ph = PHB_CURL_create(from);

  hb_retptrGC(ph);
}

static void *PHB_CURL_is(int iParam)
{
  return hb_parptrGC(&s_gcCURLFuncs, iParam);
}

static PHB_CURL PHB_CURL_par(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gcCURLFuncs, iParam));

  return ph ? static_cast<PHB_CURL>(*ph) : nullptr;
}

// Harbour interface
// -----------------

HB_FUNC(CURL_EASY_INIT)
{
  PHB_CURL_ret(nullptr);
}

HB_FUNC(CURL_EASY_DUPLICATE)
{
  if (PHB_CURL_is(1))
  {
    PHB_CURL_ret(PHB_CURL_par(1));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_CLEANUP)
{
  if (PHB_CURL_is(1))
  {
    auto ph = static_cast<void **>(hb_parptrGC(&s_gcCURLFuncs, 1));

    if (ph && *ph)
    {
      // Destroy the object
      PHB_CURL_free(static_cast<PHB_CURL>(*ph), true);
      *ph = nullptr;
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_RESET)
{
  if (PHB_CURL_is(1))
  {
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      PHB_CURL_free(hb_curl, false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_PAUSE)
{
  if (PHB_CURL_is(1))
  {
#if LIBCURL_VERSION_NUM >= 0x071200
    auto hb_curl = PHB_CURL_par(1);

    hb_curl_retcode(hb_curl ? curl_easy_pause(hb_curl->curl, hb_parni(2)) : static_cast<CURLcode>(HB_CURLE_ERROR));
#else
    hb_curl_retcode(static_cast<CURLcode>(HB_CURLE_ERROR));
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_PERFORM)
{
  if (PHB_CURL_is(1))
  {
    auto hb_curl = PHB_CURL_par(1);

    hb_curl_retcode(hb_curl ? curl_easy_perform(hb_curl->curl) : static_cast<CURLcode>(HB_CURLE_ERROR));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// NOTE: curl_easy_send(curl, cBuffer, @nSentBytes) -> nResult
HB_FUNC(CURL_EASY_SEND)
{
  if (PHB_CURL_is(1))
  {
    auto res = static_cast<CURLcode>(HB_CURLE_ERROR);
#if LIBCURL_VERSION_NUM >= 0x071202
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      size_t size = 0;

      res = curl_easy_send(hb_curl->curl, hb_parcx(2), static_cast<size_t>(hb_parclen(2)), &size);

      hb_storns(size, 3);
    }
#endif
    hb_curl_retcode(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// NOTE: curl_easy_recv( curl, @cBuffer ) -> nResult
HB_FUNC(CURL_EASY_RECV)
{
  if (PHB_CURL_is(1))
  {
    auto res = static_cast<CURLcode>(HB_CURLE_ERROR);
#if LIBCURL_VERSION_NUM >= 0x071202
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      auto size = static_cast<size_t>(hb_parclen(2));
      void *buffer;

      if (size < 1024)
      {
        size = 1024;
      }

      buffer = hb_xgrab(size + 1);

      res = curl_easy_recv(hb_curl->curl, buffer, size, &size);

      if (!hb_storclen_buffer(static_cast<char *>(buffer), size, 2))
      {
        hb_xfree(buffer);
      }
    }
#endif
    hb_curl_retcode(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_SETOPT)
{
  if (PHB_CURL_is(1) && HB_ISNUM(2))
  {
    auto hb_curl = PHB_CURL_par(1);
    auto res = static_cast<CURLcode>(HB_CURLE_ERROR);

    if (hb_curl != nullptr)
    {
      switch (hb_parni(2))
      {
        // Behavior

      case HB_CURLOPT_VERBOSE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_VERBOSE, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_HEADER:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HEADER, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_NOPROGRESS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NOPROGRESS, HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070A00
      case HB_CURLOPT_NOSIGNAL:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NOSIGNAL, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
      case HB_CURLOPT_WILDCARDMATCH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_WILDCARDMATCH, HB_CURL_OPT_BOOL(3));
        break;
#endif

        // Callback

      case HB_CURLOPT_WRITEFUNCTION: {
        PHB_ITEM pWriteFunctionCallback = hb_param(3, HB_IT_EVALITEM);

        if (hb_curl->pWriteFunctionCallback)
        {
          curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, nullptr);

          hb_itemRelease(hb_curl->pWriteFunctionCallback);
          hb_curl->pWriteFunctionCallback = nullptr;
        }

        if (pWriteFunctionCallback)
        {
          hb_curl->pWriteFunctionCallback = hb_itemNew(pWriteFunctionCallback);
          // unlock the item so GC will not mark them as used
          hb_gcUnlock(hb_curl->pWriteFunctionCallback);

          curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_writefunction_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEDATA, hb_curl);
        }
      }
      break;

      // These are hidden on the Harbour level:
      // HB_CURLOPT_WRITEDATA
      // HB_CURLOPT_READFUNCTION
      // HB_CURLOPT_READDATA
#if LIBCURL_VERSION_NUM >= 0x070C03
      // HB_CURLOPT_IOCTLFUNCTION
      // HB_CURLOPT_IOCTLDATA
#endif
      // HB_CURLOPT_SEEKFUNCTION
      // HB_CURLOPT_SEEKDATA
      // HB_CURLOPT_SOCKOPTFUNCTION
      // HB_CURLOPT_SOCKOPTDATA
      // HB_CURLOPT_OPENSOCKETFUNCTION
      // HB_CURLOPT_OPENSOCKETDATA
      // HB_CURLOPT_PROGRESSFUNCTION
      // HB_CURLOPT_PROGRESSDATA
      // HB_CURLOPT_XFERINFOFUNCTION
      // HB_CURLOPT_XFERINFODATA
      // HB_CURLOPT_HEADERFUNCTION
      // HB_CURLOPT_HEADERDATA / CURLOPT_WRITEHEADER
      // HB_CURLOPT_DEBUGFUNCTION
      // HB_CURLOPT_DEBUGDATA
#if LIBCURL_VERSION_NUM >= 0x070B00
      // HB_CURLOPT_SSL_CTX_FUNCTION
      // HB_CURLOPT_SSL_CTX_DATA
#endif
      // HB_CURLOPT_CONV_TO_NETWORK_FUNCTION
      // HB_CURLOPT_CONV_FROM_NETWORK_FUNCTION
      // HB_CURLOPT_CONV_FROM_UTF8_FUNCTION

      // Error
#if LIBCURL_VERSION_NUM >= 0x070100
      case HB_CURLOPT_ER_BUFF_SETUP:
        hb_curl_buff_er_free(hb_curl);
        hb_curl->er_len = hb_parnldef(3, HB_CURL_ER_BUFF_SIZE_INIT);
        hb_curl->er_ptr = (char *)hb_xgrab(hb_curl->er_len);
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ERRORBUFFER, hb_curl->er_ptr);
        break;
#endif
        // HB_CURLOPT_STDERR

      case HB_CURLOPT_FAILONERROR:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FAILONERROR, HB_CURL_OPT_BOOL(3));
        break;

      // Network

      // This is the only option that must be set before curl_easy_perform() is called.
      case HB_CURLOPT_URL:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_URL, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_PROXY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXY, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_PROXYPORT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYPORT, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070A00
      case HB_CURLOPT_PROXYTYPE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYTYPE, hb_parnl(3));
        break;
#endif
      case HB_CURLOPT_HTTPPROXYTUNNEL:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPPROXYTUNNEL, HB_CURL_OPT_BOOL(3));
        break;
#if 0
      case HB_CURLOPT_SOCKS5_RESOLVE_LOCAL:
         res = curl_easy_setopt(hb_curl->curl, CURLOPT_SOCKS5_RESOLVE_LOCAL, HB_CURL_OPT_BOOL(3));
         break;
#endif
      case HB_CURLOPT_INTERFACE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_INTERFACE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM >= 0x070F02
      case HB_CURLOPT_LOCALPORT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_LOCALPORT, hb_parnl(3));
        break;
      case HB_CURLOPT_LOCALPORTRANGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_LOCALPORTRANGE, hb_parnl(3));
        break;
#endif
      case HB_CURLOPT_DNS_CACHE_TIMEOUT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070B01 && LIBCURL_VERSION_NUM < 0x073E00
      case HB_CURLOPT_DNS_USE_GLOBAL_CACHE: // OBSOLETE
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_DNS_USE_GLOBAL_CACHE, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A00
      case HB_CURLOPT_BUFFERSIZE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_BUFFERSIZE, hb_parnl(3));
        break;
#endif
      case HB_CURLOPT_PORT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PORT, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070A08 // Not documented. GUESS.
      case HB_CURLOPT_TCP_NODELAY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TCP_NODELAY, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071300
      case HB_CURLOPT_ADDRESS_SCOPE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ADDRESS_SCOPE, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
      case HB_CURLOPT_PROTOCOLS:
#if LIBCURL_VERSION_NUM >= 0x075500
      {
        char *szProtocols = hb_curl_protnames(hb_parnl(3));
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROTOCOLS_STR, szProtocols);
        hb_xfree(szProtocols);
      }
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROTOCOLS, hb_parnl(3));
#endif
      break;
#if LIBCURL_VERSION_NUM >= 0x075500
      case HB_CURLOPT_PROTOCOLS_STR:
        if (HB_ISCHAR(3))
        {
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROTOCOLS_STR, hb_parc(3));
        }
        break;
#endif
      case HB_CURLOPT_REDIR_PROTOCOLS:
#if LIBCURL_VERSION_NUM >= 0x075500
      {
        char *szProtocols = hb_curl_protnames(hb_parnl(3));
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_REDIR_PROTOCOLS_STR, szProtocols);
        hb_xfree(szProtocols);
      }
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_REDIR_PROTOCOLS, hb_parnl(3));
#endif
      break;
#if LIBCURL_VERSION_NUM >= 0x075500
      case HB_CURLOPT_REDIR_PROTOCOLS_STR:
        if (HB_ISCHAR(3))
        {
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_REDIR_PROTOCOLS_STR, hb_parc(3));
        }
        break;
#endif
      case HB_CURLOPT_NOPROXY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NOPROXY, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM >= 0x072B00
      case HB_CURLOPT_PROXY_SERVICE_NAME:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXY_SERVICE_NAME, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
      case HB_CURLOPT_SOCKS5_GSSAPI_SERVICE:
#if LIBCURL_VERSION_NUM >= 0x073100
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXY_SERVICE_NAME, hb_curl_StrHash(hb_curl, hb_parc(3)));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_SERVICE, hb_curl_StrHash(hb_curl, hb_parc(3)));
#endif
        break;
#endif
      case HB_CURLOPT_SOCKS5_GSSAPI_NEC:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_NEC, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
      case HB_CURLOPT_TCP_KEEPALIVE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TCP_KEEPALIVE, hb_parnl(3));
        break;
      case HB_CURLOPT_TCP_KEEPIDLE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TCP_KEEPIDLE, hb_parnl(3));
        break;
      case HB_CURLOPT_TCP_KEEPINTVL:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TCP_KEEPINTVL, hb_parnl(3));
        break;
#endif

        // Names and passwords options (Authentication)

      case HB_CURLOPT_NETRC:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NETRC, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070A09
      case HB_CURLOPT_NETRC_FILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NETRC_FILE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
      case HB_CURLOPT_USERPWD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_USERPWD, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM >= 0x071301
      case HB_CURLOPT_USERNAME:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_USERNAME, hb_parc(3));
        break;
      case HB_CURLOPT_PASSWORD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PASSWORD, hb_parc(3));
        break;
#endif
      case HB_CURLOPT_PROXYUSERPWD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYUSERPWD, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM >= 0x071301
      case HB_CURLOPT_PROXYUSERNAME:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYUSERNAME, hb_parc(3));
        break;
      case HB_CURLOPT_PROXYPASSWORD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYPASSWORD, hb_parc(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A06
      case HB_CURLOPT_HTTPAUTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPAUTH, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
      case HB_CURLOPT_PROXYAUTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXYAUTH, hb_parnl(3));
        break;
#endif

        // HTTP options

      case HB_CURLOPT_AUTOREFERER:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_AUTOREFERER, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_ACCEPT_ENCODING:
#if LIBCURL_VERSION_NUM >= 0x071506
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ACCEPT_ENCODING, hb_curl_StrHash(hb_curl, hb_parc(3)));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ENCODING, hb_curl_StrHash(hb_curl, hb_parc(3)));
#endif
        break;
#if LIBCURL_VERSION_NUM >= 0x071506
      case HB_CURLOPT_TRANSFER_ENCODING:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TRANSFER_ENCODING, hb_parnl(3));
        break;
#endif
      case HB_CURLOPT_FOLLOWLOCATION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FOLLOWLOCATION, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_UNRESTRICTED_AUTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_UNRESTRICTED_AUTH, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_MAXREDIRS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAXREDIRS, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071101
      case HB_CURLOPT_POSTREDIR:
#if LIBCURL_VERSION_NUM >= 0x071301
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_POSTREDIR, HB_CURL_OPT_BOOL(3));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_POST301, HB_CURL_OPT_BOOL(3));
#endif
        break;
#endif
      case HB_CURLOPT_PUT:
#if LIBCURL_VERSION_NUM >= 0x070C01
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL(3));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PUT, HB_CURL_OPT_BOOL(3));
#endif
        break;
      case HB_CURLOPT_POST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_POST, HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071101
      case HB_CURLOPT_POSTFIELDS:
      case HB_CURLOPT_COPYPOSTFIELDS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COPYPOSTFIELDS, hb_parc(3));
        break;
#endif
      case HB_CURLOPT_POSTFIELDSIZE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_POSTFIELDSIZE, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070B01
      case HB_CURLOPT_POSTFIELDSIZE_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_POSTFIELDSIZE_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
#endif
      case HB_CURLOPT_HTTPPOST:
      case HB_CURLOPT_MIMEPOST:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

#if LIBCURL_VERSION_NUM >= 0x073800
          if (hb_curl->mime || nLen > 0)
          {
            if (!hb_curl->mime)
            {
              hb_curl->mime = curl_mime_init(hb_curl->curl);
            }

            for (nPos = 1; nPos <= nLen; ++nPos)
            {
              PHB_ITEM pSubArray = hb_arrayGetItemPtr(pArray, nPos);
              curl_mimepart *part = curl_mime_addpart(hb_curl->mime);

              curl_mime_name(part, hb_arrayGetCPtr(pSubArray, 1));
              curl_mime_filedata(part, hb_arrayGetCPtr(pSubArray, 2));
            }
            res = curl_easy_setopt(hb_curl->curl, CURLOPT_MIMEPOST, hb_curl->mime);
          }
#else
          for (nPos = 1; nPos <= nLen; ++nPos)
          {
            PHB_ITEM pSubArray = hb_arrayGetItemPtr(pArray, nPos);

            curl_formadd(&hb_curl->pHTTPPOST_First, &hb_curl->pHTTPPOST_Last, CURLFORM_COPYNAME,
                         hb_arrayGetCPtr(pSubArray, 1), CURLFORM_NAMELENGTH, hb_arrayGetCLen(pSubArray, 1),
                         CURLFORM_FILE, hb_curl_StrHash(hb_curl, hb_arrayGetCPtr(pSubArray, 2)), CURLFORM_END);
          }
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First);
#endif
        }
      }
      break;
      case HB_CURLOPT_REFERER:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_REFERER, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_USERAGENT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_USERAGENT, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_HTTPHEADER:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPHEADER, nullptr);
        hb_curl_slist_free(&hb_curl->pHTTPHEADER);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pHTTPHEADER = curl_slist_append(hb_curl->pHTTPHEADER, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPHEADER, hb_curl->pHTTPHEADER);
        }
      }
      break;
#if LIBCURL_VERSION_NUM >= 0x070A03
      case HB_CURLOPT_HTTP200ALIASES:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP200ALIASES, nullptr);
        hb_curl_slist_free(&hb_curl->pHTTP200ALIASES);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pHTTP200ALIASES = curl_slist_append(hb_curl->pHTTP200ALIASES, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP200ALIASES, hb_curl->pHTTP200ALIASES);
        }
      }
      break;
#endif
      case HB_CURLOPT_COOKIE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COOKIE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_COOKIEFILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COOKIEFILE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_COOKIEJAR:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COOKIEJAR, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_COOKIESESSION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COOKIESESSION, HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070E01
      case HB_CURLOPT_COOKIELIST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_COOKIELIST, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
      case HB_CURLOPT_HTTPGET:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTPGET, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_HTTP_VERSION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP_VERSION, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070E01
      case HB_CURLOPT_IGNORE_CONTENT_LENGTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_IGNORE_CONTENT_LENGTH, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071002
      case HB_CURLOPT_HTTP_CONTENT_DECODING:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP_CONTENT_DECODING, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_HTTP_TRANSFER_DECODING:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_HTTP_TRANSFER_DECODING, HB_CURL_OPT_BOOL(3));
        break;
#endif

        // SMTP options

#if LIBCURL_VERSION_NUM >= 0x071400
      case HB_CURLOPT_MAIL_FROM:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAIL_FROM, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_MAIL_RCPT:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_MAIL_RCPT, nullptr);
        hb_curl_slist_free(&hb_curl->pMAIL_RCPT);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pMAIL_RCPT = curl_slist_append(hb_curl->pMAIL_RCPT, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAIL_RCPT, hb_curl->pMAIL_RCPT);
        }
      }
      break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
      case HB_CURLOPT_MAIL_AUTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAIL_AUTH, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif

        // TFTP options

#if LIBCURL_VERSION_NUM >= 0x071304
      case HB_CURLOPT_TFTP_BLKSIZE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TFTP_BLKSIZE, hb_parnl(3));
        break;
#endif

        // FTP options

      case HB_CURLOPT_FTPPORT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTPPORT, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_QUOTE:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_QUOTE, nullptr);
        hb_curl_slist_free(&hb_curl->pQUOTE);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pQUOTE = curl_slist_append(hb_curl->pQUOTE, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_QUOTE, hb_curl->pQUOTE);
        }
      }
      break;
      case HB_CURLOPT_POSTQUOTE:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_POSTQUOTE, nullptr);
        hb_curl_slist_free(&hb_curl->pPOSTQUOTE);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pPOSTQUOTE = curl_slist_append(hb_curl->pPOSTQUOTE, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_POSTQUOTE, hb_curl->pPOSTQUOTE);
        }
      }
      break;
      case HB_CURLOPT_PREQUOTE:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_PREQUOTE, nullptr);
        hb_curl_slist_free(&hb_curl->pPREQUOTE);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pQUOTE = curl_slist_append(hb_curl->pPREQUOTE, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_PREQUOTE, hb_curl->pPREQUOTE);
        }
      }
      break;
      case HB_CURLOPT_DIRLISTONLY: // HB_CURLOPT_FTPLISTONLY
#if LIBCURL_VERSION_NUM > 0x071004
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_DIRLISTONLY, HB_CURL_OPT_BOOL(3));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTPLISTONLY, HB_CURL_OPT_BOOL(3));
#endif
        break;
      case HB_CURLOPT_APPEND: // HB_CURLOPT_FTPAPPEND
#if LIBCURL_VERSION_NUM > 0x071004
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_APPEND, HB_CURL_OPT_BOOL(3));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTPAPPEND, HB_CURL_OPT_BOOL(3));
#endif
        break;
#if LIBCURL_VERSION_NUM >= 0x070A05
      case HB_CURLOPT_FTP_USE_EPRT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_USE_EPRT, HB_CURL_OPT_BOOL(3));
        break;
#endif
      case HB_CURLOPT_FTP_USE_EPSV:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_USE_EPSV, HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071400
      case HB_CURLOPT_FTP_USE_PRET:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_USE_PRET, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
      case HB_CURLOPT_FTP_CREATE_MISSING_DIRS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08
      case HB_CURLOPT_FTP_RESPONSE_TIMEOUT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_RESPONSE_TIMEOUT, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F05
      case HB_CURLOPT_FTP_ALTERNATIVE_TO_USER:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_ALTERNATIVE_TO_USER, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070E02
      case HB_CURLOPT_FTP_SKIP_PASV_IP:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_SKIP_PASV_IP, HB_CURL_OPT_BOOL(3));
        break;
#endif
      case HB_CURLOPT_USE_SSL:
#if LIBCURL_VERSION_NUM > 0x071004
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_USE_SSL, hb_parnl(3));
#elif LIBCURL_VERSION_NUM >= 0x070B00
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_SSL, hb_parnl(3));
#endif
        break;
#if LIBCURL_VERSION_NUM >= 0x070C02
      case HB_CURLOPT_FTPSSLAUTH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTPSSLAUTH, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
      case HB_CURLOPT_FTP_SSL_CCC:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_SSL_CCC, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070D00
      case HB_CURLOPT_FTP_ACCOUNT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_ACCOUNT, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F01
      case HB_CURLOPT_FTP_FILEMETHOD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FTP_FILEMETHOD, hb_parnl(3));
        break;
#endif

        // RTSP

#if LIBCURL_VERSION_NUM >= 0x071400
      case HB_CURLOPT_RTSP_REQUEST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_REQUEST, hb_parnl(3));
        break;
      case HB_CURLOPT_RTSP_SESSION_ID:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_SESSION_ID, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_RTSP_STREAM_URI:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_STREAM_URI, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_RTSP_TRANSPORT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_TRANSPORT, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_RTSP_CLIENT_CSEQ:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_CLIENT_CSEQ, hb_parnl(3));
        break;
      case HB_CURLOPT_RTSP_SERVER_CSEQ:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RTSP_SERVER_CSEQ, hb_parnl(3));
        break;
#endif

        // Protocol

      case HB_CURLOPT_TRANSFERTEXT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TRANSFERTEXT, HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071200
      case HB_CURLOPT_PROXY_TRANSFER_MODE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROXY_TRANSFER_MODE, HB_CURL_OPT_BOOL(3));
        break;
#endif
      case HB_CURLOPT_CRLF:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CRLF, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_RANGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RANGE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_RESUME_FROM:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RESUME_FROM, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070B00
      case HB_CURLOPT_RESUME_FROM_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RESUME_FROM_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
#endif
      case HB_CURLOPT_CUSTOMREQUEST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CUSTOMREQUEST, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_FILETIME:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FILETIME, hb_parnl(3));
        break;
      case HB_CURLOPT_NOBODY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NOBODY, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_INFILESIZE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_INFILESIZE, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070B00
      case HB_CURLOPT_INFILESIZE_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_INFILESIZE_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
#endif
      case HB_CURLOPT_UPLOAD:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_DOWNLOAD: // Harbour extension
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_UPLOAD, !HB_CURL_OPT_BOOL(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070A08 // Not documented. GUESS.
      case HB_CURLOPT_MAXFILESIZE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAXFILESIZE, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070B00
      case HB_CURLOPT_MAXFILESIZE_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAXFILESIZE_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
#endif
      case HB_CURLOPT_TIMECONDITION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TIMECONDITION, hb_parnl(3));
        break;
      case HB_CURLOPT_TIMEVALUE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TIMEVALUE, hb_parnl(3));
        break;

        // Connection

      case HB_CURLOPT_TIMEOUT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TIMEOUT, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071002
      case HB_CURLOPT_TIMEOUT_MS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_TIMEOUT_MS, hb_parnl(3));
        break;
#endif
      case HB_CURLOPT_LOW_SPEED_LIMIT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_LOW_SPEED_LIMIT, hb_parnl(3));
        break;
      case HB_CURLOPT_LOW_SPEED_TIME:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x070F05
      case HB_CURLOPT_MAX_SEND_SPEED_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAX_SEND_SPEED_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
      case HB_CURLOPT_MAX_RECV_SPEED_LARGE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAX_RECV_SPEED_LARGE, HB_CURL_OPT_LARGENUM(3));
        break;
#endif
      case HB_CURLOPT_MAXCONNECTS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAXCONNECTS, hb_parnl(3));
        break;
      case HB_CURLOPT_CLOSEPOLICY: // OBSOLETE, does nothing.
        res = curl_easy_setopt(hb_curl->curl, static_cast<CURLoption>(CURLOPT_CLOSEPOLICY), hb_parnl(3));
        break;
      case HB_CURLOPT_FRESH_CONNECT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FRESH_CONNECT, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_FORBID_REUSE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_FORBID_REUSE, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_CONNECTTIMEOUT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CONNECTTIMEOUT, hb_parnl(3));
        break;
#if LIBCURL_VERSION_NUM >= 0x071002
      case HB_CURLOPT_CONNECTTIMEOUT_MS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CONNECTTIMEOUT_MS, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08 // Not documented. GUESS.
      case HB_CURLOPT_IPRESOLVE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_IPRESOLVE, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F02
      case HB_CURLOPT_CONNECT_ONLY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CONNECT_ONLY, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071503
      case HB_CURLOPT_RESOLVE:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_RESOLVE, nullptr);
        hb_curl_slist_free(&hb_curl->pRESOLVE);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pRESOLVE = curl_slist_append(hb_curl->pRESOLVE, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_RESOLVE, hb_curl->pRESOLVE);
        }
      }
      break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071800
      case HB_CURLOPT_DNS_SERVERS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_DNS_SERVERS, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_ACCEPTTIMEOUT_MS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ACCEPTTIMEOUT_MS, hb_parnl(3));
        break;
#endif

        // SSL and Security

      case HB_CURLOPT_SSLCERT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLCERT, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM >= 0x070903
      case HB_CURLOPT_SSLCERTTYPE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLCERTTYPE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
      case HB_CURLOPT_SSLKEY:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLKEY, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_SSLKEYTYPE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLKEYTYPE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_KEYPASSWD:
#if LIBCURL_VERSION_NUM > 0x071004
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_KEYPASSWD, hb_curl_StrHash(hb_curl, hb_parc(3)));
#elif LIBCURL_VERSION_NUM > 0x070902
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLKEYPASSWD, hb_curl_StrHash(hb_curl, hb_parc(3)));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLCERTPASSWD, hb_curl_StrHash(hb_curl, hb_parc(3)));
#endif
        break;
      case HB_CURLOPT_SSLENGINE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLENGINE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_SSLENGINE_DEFAULT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl(3));
        break;
      case HB_CURLOPT_SSLVERSION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSLVERSION, hb_parnl(3));
        break;
      case HB_CURLOPT_SSL_VERIFYPEER:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSL_VERIFYPEER, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_CAINFO:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CAINFO, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_CAPATH:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CAPATH, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#if LIBCURL_VERSION_NUM < 0x075400
      case HB_CURLOPT_RANDOM_FILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_RANDOM_FILE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_EGDSOCKET:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_EGDSOCKET, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
      case HB_CURLOPT_SSL_VERIFYHOST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSL_VERIFYHOST, hb_parnl(3));
        break;
      case HB_CURLOPT_SSL_CIPHER_LIST:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSL_CIPHER_LIST, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_SSL_SESSIONID_CACHE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSL_VERIFYHOST, HB_CURL_OPT_BOOL(3));
        break;
      case HB_CURLOPT_KRBLEVEL: // HB_CURLOPT_KRB4LEVEL
#if LIBCURL_VERSION_NUM > 0x071003
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_KRBLEVEL, hb_curl_StrHash(hb_curl, hb_parc(3)));
#else
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_KRB4LEVEL, hb_curl_StrHash(hb_curl, hb_parc(3)));
#endif
        break;
#if LIBCURL_VERSION_NUM >= 0x071300
      case HB_CURLOPT_CRLFILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CRLFILE, hb_parc(3));
        break;
      case HB_CURLOPT_ISSUERCERT:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_ISSUERCERT, hb_parc(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
      case HB_CURLOPT_CERTINFO:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_CERTINFO, HB_CURL_OPT_BOOL(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071600
      case HB_CURLOPT_GSSAPI_DELEGATION:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_GSSAPI_DELEGATION, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
      case HB_CURLOPT_SSL_OPTIONS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSL_OPTIONS, hb_parnl(3));
        break;
#endif

        // SSH options

#if LIBCURL_VERSION_NUM >= 0x071001
      case HB_CURLOPT_SSH_AUTH_TYPES:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSH_AUTH_TYPES, hb_parnl(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071101
      case HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, hb_parc(3));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
      case HB_CURLOPT_SSH_PUBLIC_KEYFILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSH_PUBLIC_KEYFILE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
      case HB_CURLOPT_SSH_PRIVATE_KEYFILE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSH_PRIVATE_KEYFILE, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071306
      case HB_CURLOPT_SSH_KNOWNHOSTS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_SSH_KNOWNHOSTS, hb_curl_StrHash(hb_curl, hb_parc(3)));
        break;
#endif

        // Other options

#if LIBCURL_VERSION_NUM >= 0x070A03
      case HB_CURLOPT_PRIVATE:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_PRIVATE, hb_parptr(3));
        break;
#endif

        // HB_CURLOPT_SHARE

#if LIBCURL_VERSION_NUM >= 0x071004
      case HB_CURLOPT_NEW_FILE_PERMS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NEW_FILE_PERMS, hb_parnl(3));
        break;
      case HB_CURLOPT_NEW_DIRECTORY_PERMS:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_NEW_DIRECTORY_PERMS, hb_parnl(3));
        break;
#endif

        // Telnet options

      case HB_CURLOPT_TELNETOPTIONS:
      {
        auto pArray = hb_param(3, Harbour::Item::ARRAY);

        curl_easy_setopt(hb_curl->curl, CURLOPT_TELNETOPTIONS, nullptr);
        hb_curl_slist_free(&hb_curl->pTELNETOPTIONS);

        if (pArray != nullptr)
        {
          HB_SIZE nPos;
          HB_SIZE nLen = hb_arrayLen(pArray);

          for (nPos = 0; nPos < nLen; ++nPos)
          {
            hb_curl->pTELNETOPTIONS = curl_slist_append(hb_curl->pTELNETOPTIONS, hb_arrayGetCPtr(pArray, nPos + 1));
          }

          res = curl_easy_setopt(hb_curl->curl, CURLOPT_TELNETOPTIONS, hb_curl->pTELNETOPTIONS);
        }
      }
      break;

        // Undocumented

        // HB_CURLOPT_WRITEINFO

        // Harbour specials

      case HB_CURLOPT_PROGRESSBLOCK:
      {
        auto pProgressCallback = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

        if (hb_curl->pProgressCallback)
        {
#if LIBCURL_VERSION_NUM >= 0x072000
          curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFOFUNCTION, nullptr);
          curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFODATA, nullptr);
#else
          curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSFUNCTION, nullptr);
          curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSDATA, nullptr);
#endif
          hb_itemRelease(hb_curl->pProgressCallback);
          hb_curl->pProgressCallback = nullptr;
        }

        if (pProgressCallback)
        {
          hb_curl->pProgressCallback = hb_itemNew(pProgressCallback);
          // unlock the item so GC will not mark them as used
          hb_gcUnlock(hb_curl->pProgressCallback);

#if LIBCURL_VERSION_NUM >= 0x072000
          curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFOFUNCTION, hb_curl_xferinfo_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_XFERINFODATA, hb_curl->pProgressCallback);
#else
          curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSFUNCTION, hb_curl_progress_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_PROGRESSDATA, hb_curl->pProgressCallback);
#endif
        }
      }
      break;

      case HB_CURLOPT_UL_FILE_SETUP:
        hb_curl_file_ul_free(hb_curl);

        if (HB_ISCHAR(3))
        {
          hb_curl->ul_name = hb_strdup(hb_parc(3));
          hb_curl->ul_handle = FS_ERROR;

          curl_easy_setopt(hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_file_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_READDATA, hb_curl);
        }
        break;

      case HB_CURLOPT_UL_FHANDLE_SETUP:
        hb_curl_file_ul_free(hb_curl);

        if (HB_ISNUM(3))
        {
          hb_curl->ul_name = nullptr;
          hb_curl->ul_handle = hb_numToHandle(hb_parnint(3));

          curl_easy_setopt(hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_fhandle_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_READDATA, hb_curl);
        }
        break;

      case HB_CURLOPT_UL_FILE_CLOSE:
        hb_curl_file_ul_free(hb_curl);
        res = CURLE_OK;
        break;

      case HB_CURLOPT_DL_FILE_SETUP:
        hb_curl_file_dl_free(hb_curl);

        if (HB_ISCHAR(3))
        {
          hb_curl->dl_name = hb_strdup(hb_parc(3));
          hb_curl->dl_handle = FS_ERROR;

          curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_file_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEDATA, hb_curl);
        }
        break;

      case HB_CURLOPT_DL_FHANDLE_SETUP:
        hb_curl_file_dl_free(hb_curl);

        if (HB_ISNUM(3))
        {
          hb_curl->dl_name = nullptr;
          hb_curl->dl_handle = hb_numToHandle(hb_parnint(3));

          curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_fhandle_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEDATA, hb_curl);
        }
        break;

      case HB_CURLOPT_DL_FILE_CLOSE:
        hb_curl_file_dl_free(hb_curl);
        res = CURLE_OK;
        break;

      case HB_CURLOPT_UL_BUFF_SETUP:
        hb_curl_buff_ul_free(hb_curl);

        if (HB_ISCHAR(3))
        {
          hb_curl->ul_pos = 0;
          hb_curl->ul_len = hb_parclen(3);
          hb_curl->ul_ptr = static_cast<unsigned char *>(hb_xgrab(hb_curl->ul_len));

          hb_xmemcpy(hb_curl->ul_ptr, hb_parc(3), hb_curl->ul_len);

          curl_easy_setopt(hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_buff_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_READDATA, hb_curl);
        }
        break;

      case HB_CURLOPT_DL_BUFF_SETUP:
        hb_curl_buff_dl_free(hb_curl);

        hb_curl->dl_pos = 0;
        hb_curl->dl_len = hb_parnldef(3, HB_CURL_DL_BUFF_SIZE_INIT);
        hb_curl->dl_ptr = static_cast<unsigned char *>(hb_xgrab(hb_curl->dl_len));

        curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_buff_callback);
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_WRITEDATA, hb_curl);
        break;

      case HB_CURLOPT_DL_BUFF_GET:
        hb_storclen(reinterpret_cast<char *>(hb_curl->dl_ptr), hb_curl->dl_pos, 3);
        if (hb_curl->dl_ptr)
        {
          res = CURLE_OK;
        }
        break;

      case HB_CURLOPT_UL_NULL_SETUP:
        hb_curl_file_ul_free(hb_curl);
        hb_curl_buff_ul_free(hb_curl);

        curl_easy_setopt(hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_dummy_callback);
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_READDATA, hb_curl);
        break;

      case HB_CURLOPT_DEBUGBLOCK:
      {
        auto pDebugCallback = hb_param(3, Harbour::Item::BLOCK | Harbour::Item::SYMBOL);

        if (hb_curl->pDebugCallback)
        {
          curl_easy_setopt(hb_curl->curl, CURLOPT_DEBUGFUNCTION, nullptr);
          curl_easy_setopt(hb_curl->curl, CURLOPT_DEBUGDATA, nullptr);

          hb_itemRelease(hb_curl->pDebugCallback);
          hb_curl->pDebugCallback = nullptr;
        }

        if (pDebugCallback)
        {
          hb_curl->pDebugCallback = hb_itemNew(pDebugCallback);
          // unlock the item so GC will not mark them as used
          hb_gcUnlock(hb_curl->pDebugCallback);

          curl_easy_setopt(hb_curl->curl, CURLOPT_DEBUGFUNCTION, hb_curl_debug_callback);
          res = curl_easy_setopt(hb_curl->curl, CURLOPT_DEBUGDATA, hb_curl);
        }
      }
      break;

#if LIBCURL_VERSION_NUM >= 0x075000
      case HB_CURLOPT_MAXLIFETIME_CONN:
        res = curl_easy_setopt(hb_curl->curl, CURLOPT_MAXLIFETIME_CONN, hb_parnl(3));
        break;
#endif
      }
    }

    hb_curl_retcode(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// Harbour extension.
HB_FUNC(CURL_EASY_DL_BUFF_GET)
{
  if (PHB_CURL_is(1))
  {
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      hb_retclen(reinterpret_cast<char *>(hb_curl->dl_ptr), hb_curl->dl_pos);
    }
    else
    {
      hb_retc_null();
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_ER_BUFF_GET)
{
  if (PHB_CURL_is(1))
  {
#if LIBCURL_VERSION_NUM >= 0x070100
    PHB_CURL hb_curl = PHB_CURL_par(1);

    if (hb_curl)
    {
      hb_retc(hb_curl->er_ptr);
    }
    else
#endif
      hb_retc_null();
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

static void hb_curl_slist_array(PHB_ITEM pArray, struct curl_slist *slist)
{
  struct curl_slist *walk_slist;
  int nCount;

  // Count
  for (walk_slist = slist, nCount = 0; walk_slist->next; nCount++)
  {
    walk_slist = walk_slist->next;
  }

  // Fill
  hb_arrayNew(pArray, nCount);
  for (walk_slist = slist, nCount = 1; walk_slist->next;)
  {
    hb_arraySetC(pArray, nCount++, walk_slist->data);
    walk_slist = walk_slist->next;
  }
}

#define HB_CURL_INFO_TYPE_INVALID 0
#define HB_CURL_INFO_TYPE_STR 1
#define HB_CURL_INFO_TYPE_PTR 2
#define HB_CURL_INFO_TYPE_LONG 3
#define HB_CURL_INFO_TYPE_DOUBLE 4
#define HB_CURL_INFO_TYPE_OFFSET 5
#define HB_CURL_INFO_TYPE_SOCKET 6
#define HB_CURL_INFO_TYPE_SLIST 7
#define HB_CURL_INFO_TYPE_CERTINFO 8

#define HB_CURL_EASY_GETINFO(hb_curl, n, p)                                                                            \
  (hb_curl ? curl_easy_getinfo(hb_curl->curl, n, p) : static_cast<CURLcode>(HB_CURLE_ERROR))

// NOTE: curl_easy_getinfo( curl, x, @nError ) -> xValue
HB_FUNC(CURL_EASY_GETINFO)
{
  if (PHB_CURL_is(1) && HB_ISNUM(2))
  {
    auto hb_curl = PHB_CURL_par(1);
    auto res = static_cast<CURLcode>(HB_CURLE_ERROR);

    int type = HB_CURL_INFO_TYPE_INVALID;

    char *ret_string = nullptr;
    char *ret_ptr = nullptr;
    long ret_long = 0;
    struct curl_slist *ret_slist = nullptr;
    struct curl_certinfo *ret_certinfo = nullptr;
    double ret_double = 0.0;
    curl_socket_t ret_socket = 0;
    curl_off_t ret_offset = 0;

    switch (hb_parni(2))
    {
    case HB_CURLINFO_EFFECTIVE_URL:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_EFFECTIVE_URL, &ret_string);
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_RESPONSE_CODE:
#if LIBCURL_VERSION_NUM > 0x070A07
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_RESPONSE_CODE, &ret_long);
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_HTTP_CODE, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_HTTP_CONNECTCODE:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_HTTP_CONNECTCODE, &ret_long);
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_FILETIME:
#if LIBCURL_VERSION_NUM >= 0x070500
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_FILETIME, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_TOTAL_TIME:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_TOTAL_TIME, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_NAMELOOKUP_TIME:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_NAMELOOKUP_TIME, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_CONNECT_TIME:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONNECT_TIME, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_PRETRANSFER_TIME:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_PRETRANSFER_TIME, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_STARTTRANSFER_TIME:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_STARTTRANSFER_TIME, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_REDIRECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x070907
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_REDIRECT_TIME, &ret_double);
#endif
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_REDIRECT_COUNT:
#if LIBCURL_VERSION_NUM >= 0x070907
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_REDIRECT_COUNT, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_REDIRECT_URL:
#if LIBCURL_VERSION_NUM >= 0x071202
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_REDIRECT_URL, &ret_string);
#endif
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_SIZE_UPLOAD:
    case HB_CURLINFO_SIZE_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SIZE_UPLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SIZE_UPLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_SIZE_DOWNLOAD:
    case HB_CURLINFO_SIZE_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SIZE_DOWNLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SIZE_DOWNLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_SPEED_DOWNLOAD:
    case HB_CURLINFO_SPEED_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SPEED_DOWNLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SPEED_DOWNLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_SPEED_UPLOAD:
    case HB_CURLINFO_SPEED_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SPEED_UPLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SPEED_UPLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_HEADER_SIZE:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_HEADER_SIZE, &ret_long);
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_REQUEST_SIZE:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_REQUEST_SIZE, &ret_long);
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_SSL_VERIFYRESULT:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SSL_VERIFYRESULT, &ret_long);
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_SSL_ENGINES:
#if LIBCURL_VERSION_NUM >= 0x071203
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_SSL_ENGINES, &ret_slist);
#endif
      type = HB_CURL_INFO_TYPE_SLIST;
      break;
    case HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD:
    case HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_CONTENT_LENGTH_UPLOAD:
    case HB_CURLINFO_CONTENT_LENGTH_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONTENT_LENGTH_UPLOAD_T, &ret_offset);
      type = HB_CURL_INFO_TYPE_OFFSET;
#else
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONTENT_LENGTH_UPLOAD, &ret_double);
      type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
      break;
    case HB_CURLINFO_CONTENT_TYPE:
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONTENT_TYPE, &ret_string);
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_PRIVATE:
#if LIBCURL_VERSION_NUM >= 0x070A03
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_PRIVATE, &ret_ptr);
#endif
      type = HB_CURL_INFO_TYPE_PTR;
      break;
    case HB_CURLINFO_HTTPAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_HTTPAUTH_AVAIL, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_PROXYAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_PROXYAUTH_AVAIL, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_OS_ERRNO:
#if LIBCURL_VERSION_NUM >= 0x070C02
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_OS_ERRNO, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_NUM_CONNECTS:
#if LIBCURL_VERSION_NUM >= 0x070C03
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_NUM_CONNECTS, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_COOKIELIST:
#if LIBCURL_VERSION_NUM >= 0x070E01
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_COOKIELIST, &ret_slist);
#endif
      type = HB_CURL_INFO_TYPE_SLIST;
      break;
    case HB_CURLINFO_LASTSOCKET:
    case HB_CURLINFO_ACTIVESOCKET:
#if LIBCURL_VERSION_NUM >= 0x072D00
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_ACTIVESOCKET, &ret_socket);
      type = HB_CURL_INFO_TYPE_SOCKET;
#else
#if LIBCURL_VERSION_NUM >= 0x070F02
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_LASTSOCKET, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
#endif
      break;
    case HB_CURLINFO_FTP_ENTRY_PATH:
#if LIBCURL_VERSION_NUM >= 0x070F04
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_FTP_ENTRY_PATH, &ret_string);
#endif
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_PRIMARY_IP:
#if LIBCURL_VERSION_NUM >= 0x071300
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_PRIMARY_IP, &ret_string);
#endif
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_APPCONNECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x071300
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_APPCONNECT_TIME, &ret_double);
#endif
      type = HB_CURL_INFO_TYPE_DOUBLE;
      break;
    case HB_CURLINFO_CERTINFO:
#if LIBCURL_VERSION_NUM >= 0x071301
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CERTINFO, &ret_certinfo);
#endif
      type = HB_CURL_INFO_TYPE_CERTINFO;
      break;
    case HB_CURLINFO_CONDITION_UNMET:
#if LIBCURL_VERSION_NUM >= 0x071304
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_CONDITION_UNMET, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_RTSP_SESSION_ID:
#if LIBCURL_VERSION_NUM >= 0x071400
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_RTSP_SESSION_ID, &ret_string);
#endif
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_RTSP_CLIENT_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_RTSP_CLIENT_CSEQ, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_RTSP_SERVER_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_RTSP_SERVER_CSEQ, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_RTSP_CSEQ_RECV:
#if LIBCURL_VERSION_NUM >= 0x071400
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_RTSP_CSEQ_RECV, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_PRIMARY_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_PRIMARY_PORT, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    case HB_CURLINFO_LOCAL_IP:
#if LIBCURL_VERSION_NUM >= 0x071500
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_LOCAL_IP, &ret_string);
#endif
      type = HB_CURL_INFO_TYPE_STR;
      break;
    case HB_CURLINFO_LOCAL_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
      res = HB_CURL_EASY_GETINFO(hb_curl, CURLINFO_LOCAL_PORT, &ret_long);
#endif
      type = HB_CURL_INFO_TYPE_LONG;
      break;
    }

    switch (type)
    {
    case HB_CURL_INFO_TYPE_STR:
      hb_retc(ret_string);
      break;
    case HB_CURL_INFO_TYPE_PTR:
      hb_retptr(ret_ptr);
      break;
    case HB_CURL_INFO_TYPE_LONG:
      hb_retnl(ret_long);
      break;
    case HB_CURL_INFO_TYPE_OFFSET:
      hb_retnint((HB_MAXINT)ret_offset);
      break;
    case HB_CURL_INFO_TYPE_SOCKET:
      hb_retnint((HB_MAXINT)ret_socket);
      break;
    case HB_CURL_INFO_TYPE_DOUBLE:
      hb_retnd(ret_double);
      break;
    case HB_CURL_INFO_TYPE_CERTINFO:
      if (ret_certinfo && ret_certinfo->num_of_certs > 0)
      {
        PHB_ITEM pArray = hb_itemArrayNew(ret_certinfo->num_of_certs);
        int num;

        for (num = 1; num <= ret_certinfo->num_of_certs; num++, ret_certinfo->certinfo++)
        {
          hb_curl_slist_array(hb_arrayGetItemPtr(pArray, num), *ret_certinfo->certinfo);
        }
        hb_itemReturnRelease(pArray);
      }
      else
      {
        hb_reta(0);
      }
      break;
    case HB_CURL_INFO_TYPE_SLIST:
      if (ret_slist)
      {
        PHB_ITEM pArray = hb_itemNew(nullptr);
        hb_curl_slist_array(pArray, ret_slist);
        hb_itemReturnRelease(pArray);
        curl_slist_free_all(ret_slist);
      }
      else
      {
        hb_reta(0);
      }
      break;
    }

    hb_stornl(static_cast<long>(res), 3);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_ESCAPE)
{
  if (PHB_CURL_is(1))
  {
#if LIBCURL_VERSION_NUM >= 0x070F04
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      char *buffer = curl_easy_escape(hb_curl->curl, hb_parcx(2), static_cast<int>(hb_parclen(2)));
      hb_retc(buffer);
      curl_free(buffer);
    }
    else
#endif
      hb_retc_null();
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CURL_EASY_UNESCAPE)
{
  if (PHB_CURL_is(1))
  {
#if LIBCURL_VERSION_NUM >= 0x070F04
    auto hb_curl = PHB_CURL_par(1);

    if (hb_curl != nullptr)
    {
      int nLen = 0;
      char *buffer = curl_easy_unescape(hb_curl->curl, hb_parcx(2), static_cast<int>(hb_parclen(2)), &nLen);
      hb_retclen(buffer, nLen);
      curl_free(buffer);
    }
    else
#endif
      hb_retc_null();
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// Harbour interface (session independent)
// ---------------------------------------

HB_FUNC(CURL_VERSION)
{
  hb_retc(curl_version());
}

HB_FUNC(CURL_VERSION_INFO)
{
  curl_version_info_data *data = curl_version_info(CURLVERSION_NOW);

  if (data)
  {
    PHB_ITEM pArray = hb_itemArrayNew(13);

    hb_arraySetC(pArray, 1, data->version);          // LIBCURL_VERSION
    hb_arraySetNI(pArray, 2, data->version_num);     // LIBCURL_VERSION_NUM
    hb_arraySetC(pArray, 3, data->host);             // OS/host/cpu/machine when configured
    hb_arraySetNI(pArray, 4, data->features);        // bitmask, see defines below
    hb_arraySetC(pArray, 5, data->ssl_version);      // human readable string
    hb_arraySetNI(pArray, 6, data->ssl_version_num); // not used anymore, always 0
    hb_arraySetC(pArray, 7, data->libz_version);     // human readable string
#if defined(CURLVERSION_SECOND)
    hb_arraySetC(pArray, 9, data->age >= CURLVERSION_SECOND ? data->ares : nullptr);
    hb_arraySetNI(pArray, 10, data->age >= CURLVERSION_SECOND ? data->ares_num : 0);
#else
    hb_arraySetC(pArray, 9, nullptr);
    hb_arraySetNI(pArray, 10, 0);
#endif
#if defined(CURLVERSION_THIRD)
    hb_arraySetC(pArray, 11, data->age >= CURLVERSION_THIRD ? data->libidn : nullptr);
#else
    hb_arraySetC(pArray, 11, nullptr);
#endif
#if defined(CURLVERSION_FOURTH)
    hb_arraySetNI(pArray, 12,
                  data->age >= CURLVERSION_FOURTH ? data->iconv_ver_num
                                                  : 0); // Same as '_libiconv_version' if built with HAVE_ICONV
#else
    hb_arraySetNI(pArray, 12, 0);
#endif
// Just a guess. It's not documented in which libcurl version this member got added.
#if defined(CURLVERSION_FOURTH) && LIBCURL_VERSION_NUM >= 0x071001
    hb_arraySetC(pArray, 13, data->age >= CURLVERSION_FOURTH ? data->libssh_version : nullptr); // human readable string
#else
    hb_arraySetC(pArray, 13, nullptr);
#endif
    {
      PHB_ITEM pProtocols;
      int nCount = 0;
      const char *const *prot = data->protocols;

      while (*(prot++))
      {
        nCount++;
      }

      pProtocols = hb_arrayGetItemPtr(pArray, 8);
      hb_arrayNew(pProtocols, nCount);

      for (prot = data->protocols, nCount = 1; *prot; prot++)
      {
        hb_arraySetC(pProtocols, nCount++, *prot);
      }
    }

    hb_itemReturnRelease(pArray);
  }
  else
  {
    hb_reta(0);
  }
}

HB_FUNC(CURL_EASY_STRERROR)
{
  if (HB_ISNUM(1))
  {
#if LIBCURL_VERSION_NUM >= 0x070C00
    hb_retc(curl_easy_strerror(static_cast<CURLcode>(hb_parnl(1))));
#else
    hb_retc_null();
#endif
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// NOTE: This returns the number of seconds since January 1st 1970 in the UTC time zone.
HB_FUNC(CURL_GETDATE)
{
  if (HB_ISCHAR(1))
  {
    hb_retnint(static_cast<HB_MAXINT>(curl_getdate(hb_parc(1), nullptr)));
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// CURLcode curl_ws_send(struct Curl_easy *data,
//                       const void *buffer, size_t buflen,
//                       size_t *sent,
//                       curl_off_t framesize,
//                       unsigned int sendflags)
HB_FUNC(CURL_WS_SEND)
{
  if (PHB_CURL_is(1))
  {
    CURLcode res = (CURLcode)HB_CURLE_ERROR;
    size_t sent = 0;

#if LIBCURL_VERSION_NUM >= 0x075600
    PHB_CURL hb_curl = PHB_CURL_par(1);

    if (hb_curl)
      res = curl_ws_send(hb_curl->curl, (const void *)hb_parc(2), (size_t)hb_parclen(2), &sent,
                         (curl_off_t)hb_parnint(4), (unsigned int)hb_parnl(5));
#endif

    hb_storns((HB_SIZE)sent, 3);
    hb_retnl(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// CURLcode curl_ws_recv(struct Curl_easy *data,
//                       void *buffer, size_t buflen,
//                       size_t *recv, struct curl_ws_frame **meta)
HB_FUNC(CURL_WS_RECV)
{
  if (PHB_CURL_is(1) && HB_ISBYREF(2))
  {
    CURLcode res = (CURLcode)HB_CURLE_ERROR;
    size_t recv = 0;

#if LIBCURL_VERSION_NUM >= 0x075600
    PHB_CURL hb_curl = PHB_CURL_par(1);

    const struct curl_ws_frame *meta = nullptr;

    PHB_ITEM pBuffer = hb_param(2, HB_IT_STRING);
    char *buffer;
    HB_SIZE buflen;

    if (hb_itemGetWriteCL(pBuffer, &buffer, &buflen))
      res = curl_ws_recv(hb_curl->curl, (void *)buffer, (size_t)buflen, &recv, &meta);

    hb_stornl(meta ? meta->flags : 0, 4);
    hb_stornint(meta ? meta->offset : 0, 5);
    hb_stornint(meta ? meta->bytesleft : 0, 6);
#else
    hb_stornl(0, 4);
    hb_stornint(0, 5);
    hb_stornint(0, 6);
#endif

    hb_storns((HB_SIZE)recv, 3);
    hb_retnl(res);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// Multi interface Constructor/Destructor
// --------------------------------------

#if LIBCURL_VERSION_NUM >= 0x070906
static void PHB_CURLM_free(PHB_CURLM hb_curlm)
{
  curl_multi_cleanup(hb_curlm->curlm);
  hb_xfree(hb_curlm);
}

static PHB_CURLM PHB_CURLM_create(void)
{
  CURLM *curlm = curl_multi_init();

  if (curlm)
  {
    PHB_CURLM hb_curlm = (PHB_CURLM)hb_xgrab(sizeof(HB_CURLM));

    memset(hb_curlm, 0, sizeof(HB_CURLM));
    hb_curlm->curlm = curlm;

    return hb_curlm;
  }
  else
  {
    return nullptr;
  }
}

static HB_GARBAGE_FUNC(PHB_CURLM_release)
{
  PHB_CURLM *hb_curlm_ptr = (PHB_CURLM *)Cargo;

  // Check if pointer is not nullptr to avoid multiple freeing
  if (hb_curlm_ptr && *hb_curlm_ptr)
  {
    // Destroy the object
    PHB_CURLM_free(*hb_curlm_ptr);
    *hb_curlm_ptr = nullptr;
  }
}

static const HB_GC_FUNCS s_gcCURLMFuncs = {PHB_CURLM_release, hb_gcDummyMark};

static void PHB_CURLM_ret()
{
  void **ph = (void **)hb_gcAllocate(sizeof(PHB_CURLM), &s_gcCURLMFuncs);

  *ph = PHB_CURLM_create();

  hb_retptrGC(ph);
}

static void *PHB_CURLM_is(int iParam)
{
  return hb_parptrGC(&s_gcCURLMFuncs, iParam);
}

static PHB_CURLM PHB_CURLM_par(int iParam)
{
  void **ph = (void **)hb_parptrGC(&s_gcCURLMFuncs, iParam);

  return ph ? (PHB_CURLM)*ph : nullptr;
}
#endif

// Harbour multi interface
// -----------------------

HB_FUNC(CURL_MULTI_INIT)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  PHB_CURLM_ret();
#endif
}

HB_FUNC(CURL_MULTI_CLEANUP)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  if (PHB_CURLM_is(1))
  {
    void **ph = (void **)hb_parptrGC(&s_gcCURLMFuncs, 1);

    if (ph && *ph)
    {
      // Destroy the object
      PHB_CURLM_free((PHB_CURLM)*ph);
      *ph = nullptr;
    }
  }
  else
#endif
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(CURL_MULTI_ADD_HANDLE)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  if (PHB_CURLM_is(1) && PHB_CURL_is(2))
  {
    PHB_CURLM hb_curlm = PHB_CURLM_par(1);
    PHB_CURL hb_curl = PHB_CURL_par(2);

    hb_retnl(hb_curlm && hb_curl ? (long)curl_multi_add_handle(hb_curlm->curlm, hb_curl->curl) : HB_CURLM_ERROR);
  }
  else
#endif
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(CURL_MULTI_REMOVE_HANDLE)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  if (PHB_CURLM_is(1) && PHB_CURL_is(2))
  {
    PHB_CURLM hb_curlm = PHB_CURLM_par(1);
    PHB_CURL hb_curl = PHB_CURL_par(2);

    hb_retnl(hb_curlm && hb_curl ? (long)curl_multi_remove_handle(hb_curlm->curlm, hb_curl->curl) : HB_CURLM_ERROR);
  }
  else
#endif
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(CURL_MULTI_PERFORM)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  if (PHB_CURLM_is(1) && HB_ISBYREF(2))
  {

    CURLMcode res = (CURLMcode)HB_CURLM_ERROR;
    PHB_CURLM hb_curlm = PHB_CURLM_par(1);

    if (hb_curlm)
    {
      int running_handles = 0;
      res = curl_multi_perform(hb_curlm->curlm, &running_handles);
      hb_stornl(running_handles, 2);
    }

    hb_retnl((long)res);
  }
  else
#endif
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(CURL_MULTI_POLL)
{
#if LIBCURL_VERSION_NUM >= 0x074200
  if (PHB_CURLM_is(1) && HB_ISNUM(2))
  {
    CURLMcode res = (CURLMcode)HB_CURLM_ERROR;
    PHB_CURLM hb_curlm = PHB_CURLM_par(1);

    if (hb_curlm)
    {
      res = curl_multi_poll(hb_curlm->curlm, nullptr, 0, hb_parni(2), nullptr);
    }

    hb_retnl((long)res);
  }
  else
#endif
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(CURL_MULTI_INFO_READ)
{
#if LIBCURL_VERSION_NUM >= 0x070906
  if (PHB_CURLM_is(1))
  {
    PHB_CURLM hb_curlm = PHB_CURLM_par(1);

    if (hb_curlm)
    {
      int msgs_in_queue = 0;
      long response_code = 0;
      struct CURLMsg *msg = curl_multi_info_read(hb_curlm->curlm, &msgs_in_queue);

      if (msg && curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code) == CURLE_OK)
      {
        PHB_ITEM pHandles = hb_param(2, HB_IT_ARRAY);
        PHB_ITEM pReturn = hb_itemArrayNew(HB_CURLMSG_RESP_LAST);

        hb_arraySetNI(pReturn, HB_CURLMSG_RESP_LEN, msgs_in_queue);
        hb_arraySetNL(pReturn, HB_CURLMSG_RESP_RESPONSE_CODE, response_code);
        hb_arraySetNL(pReturn, HB_CURLMSG_RESP_MSG, (long)msg->msg);
        hb_arraySetNL(pReturn, HB_CURLMSG_RESP_RESULT, (long)msg->data.result);
        hb_arraySetNI(pReturn, HB_CURLMSG_RESP_HPOS, 0);

        if (pHandles)
        {
          HB_SIZE handles_count = hb_arrayLen(pHandles);
          HB_SIZE i;

          for (i = 1; i <= handles_count; i++)
          {
            void **ph = (void **)hb_arrayGetPtrGC(pHandles, i, &s_gcCURLFuncs);
            if (ph && *ph)
            {
              PHB_CURL hbcurl = (PHB_CURL)*ph;

              if (hbcurl->curl == msg->easy_handle)
              {
                hb_arraySetNL(pReturn, HB_CURLMSG_RESP_HPOS, (long)i);
                hb_arraySet(pReturn, HB_CURLMSG_RESP_HANDLE, hb_arrayGetItemPtr(pHandles, i));
              }
            }
          }
        }
        hb_itemReturnRelease(pReturn);
      }
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#endif
}

// NOTE: Obsolete, superseded by curl_easy_escape()
HB_FUNC(CURL_ESCAPE)
{
  if (HB_ISCHAR(1))
  {
    char *buffer = curl_escape(hb_parc(1), static_cast<int>(hb_parclen(1)));
    hb_retc(buffer);
    curl_free(buffer);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// NOTE: Obsolete, superseded by curl_easy_unescape()
HB_FUNC(CURL_UNESCAPE)
{
  if (HB_ISCHAR(1))
  {
    char *buffer = curl_unescape(hb_parc(1), static_cast<int>(hb_parclen(1)));
    hb_retc(buffer);
    curl_free(buffer);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
