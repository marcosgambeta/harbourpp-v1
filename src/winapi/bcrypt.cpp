//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <bcrypt.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// NTSTATUS WINAPI BCryptOpenAlgorithmProvider (BCRYPT_ALG_HANDLE *phAlgorithm, LPCWSTR pszAlgId, LPCWSTR pszImplementation, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTOPENALGORITHMPROVIDER )
{
  void * str2{};
  void * str3{};
  wa_ret_NTSTATUS(BCryptOpenAlgorithmProvider(###, HB_PARSTR(2, &str2, nullptr), HB_PARSTR(3, &str3, nullptr), wa_par_ULONG(4)));
  hb_strfree(str2);
  hb_strfree(str3);
}
#endif

// NTSTATUS WINAPI BCryptEnumAlgorithms (ULONG dwAlgOperations, ULONG *pAlgCount, BCRYPT_ALGORITHM_IDENTIFIER **ppAlgList, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTENUMALGORITHMS )
{
  ULONG AlgCount{};
  wa_ret_NTSTATUS(BCryptEnumAlgorithms(wa_par_ULONG(1), &AlgCount, ###, wa_par_ULONG(4)));
  wa_stor_ULONG(AlgCount, 2);
}
#endif

// NTSTATUS WINAPI BCryptEnumProviders (LPCWSTR pszAlgId, ULONG *pImplCount, BCRYPT_PROVIDER_NAME **ppImplList, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTENUMPROVIDERS )
{
  void * str1{};
  ULONG ImplCount{};
  wa_ret_NTSTATUS(BCryptEnumProviders(HB_PARSTR(1, &str1, nullptr), &ImplCount, ###, wa_par_ULONG(4)));
  wa_stor_ULONG(ImplCount, 2);
  hb_strfree(str1);
}
#endif

// NTSTATUS WINAPI BCryptGetProperty(BCRYPT_HANDLE hObject, LPCWSTR pszProperty, PUCHAR pbOutput, ULONG cbOutput, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTGETPROPERTY )
{
  void * str2{};
  UCHAR bOutput{};
  ULONG cbResult{};
  wa_ret_NTSTATUS(BCryptGetProperty(wa_par_BCRYPT_HANDLE(1), HB_PARSTR(2, &str2, nullptr), &bOutput, wa_par_ULONG(4), &cbResult, wa_par_ULONG(6)));
  wa_stor_UCHAR(bOutput, 3);
  wa_stor_ULONG(cbResult, 5);
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptSetProperty (BCRYPT_HANDLE hObject, LPCWSTR pszProperty, PUCHAR pbInput, ULONG cbInput, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTSETPROPERTY )
{
  void * str2{};
  UCHAR bInput{};
  wa_ret_NTSTATUS(BCryptSetProperty(wa_par_BCRYPT_HANDLE(1), HB_PARSTR(2, &str2, nullptr), &bInput, wa_par_ULONG(4), wa_par_ULONG(5)));
  wa_stor_UCHAR(bInput, 3);
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptCloseAlgorithmProvider (BCRYPT_ALG_HANDLE hAlgorithm, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTCLOSEALGORITHMPROVIDER )
{
  wa_ret_NTSTATUS(BCryptCloseAlgorithmProvider(wa_par_BCRYPT_ALG_HANDLE(1), wa_par_ULONG(2)));
}
#endif

// VOID WINAPI BCryptFreeBuffer (PVOID pvBuffer)
#if 0
HB_FUNC( WABCRYPTFREEBUFFER )
{
  BCryptFreeBuffer(wa_par_PVOID(1));
}
#endif

// NTSTATUS WINAPI BCryptGenerateSymmetricKey (BCRYPT_ALG_HANDLE hAlgorithm, BCRYPT_KEY_HANDLE *phKey, PUCHAR pbKeyObject, ULONG cbKeyObject, PUCHAR pbSecret, ULONG cbSecret, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTGENERATESYMMETRICKEY )
{
  UCHAR bKeyObject{};
  UCHAR bSecret{};
  wa_ret_NTSTATUS(BCryptGenerateSymmetricKey(wa_par_BCRYPT_ALG_HANDLE(1), ###, &bKeyObject, wa_par_ULONG(4), &bSecret, wa_par_ULONG(6), wa_par_ULONG(7)));
  wa_stor_UCHAR(bKeyObject, 3);
  wa_stor_UCHAR(bSecret, 5);
}
#endif

// NTSTATUS WINAPI BCryptGenerateKeyPair (BCRYPT_ALG_HANDLE hAlgorithm, BCRYPT_KEY_HANDLE *phKey, ULONG dwLength, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTGENERATEKEYPAIR )
{
  wa_ret_NTSTATUS(BCryptGenerateKeyPair(wa_par_BCRYPT_ALG_HANDLE(1), ###, wa_par_ULONG(3), wa_par_ULONG(4)));
}
#endif

// NTSTATUS WINAPI BCryptEncrypt (BCRYPT_KEY_HANDLE hKey, PUCHAR pbInput, ULONG cbInput, VOID *pPaddingInfo, PUCHAR pbIV, ULONG cbIV, PUCHAR pbOutput, ULONG cbOutput, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTENCRYPT )
{
  UCHAR bInput{};
  UCHAR bIV{};
  UCHAR bOutput{};
  wa_ret_NTSTATUS(BCryptEncrypt(wa_par_BCRYPT_KEY_HANDLE(1), &bInput, wa_par_ULONG(3), ###, &bIV, wa_par_ULONG(6), &bOutput, wa_par_ULONG(8), ###, wa_par_ULONG(10)));
  wa_stor_UCHAR(bInput, 2);
  wa_stor_UCHAR(bIV, 5);
  wa_stor_UCHAR(bOutput, 7);
}
#endif

// NTSTATUS WINAPI BCryptDecrypt (BCRYPT_KEY_HANDLE hKey, PUCHAR pbInput, ULONG cbInput, VOID *pPaddingInfo, PUCHAR pbIV, ULONG cbIV, PUCHAR pbOutput, ULONG cbOutput, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDECRYPT )
{
  UCHAR bInput{};
  UCHAR bIV{};
  UCHAR bOutput{};
  wa_ret_NTSTATUS(BCryptDecrypt(wa_par_BCRYPT_KEY_HANDLE(1), &bInput, wa_par_ULONG(3), ###, &bIV, wa_par_ULONG(6), &bOutput, wa_par_ULONG(8), ###, wa_par_ULONG(10)));
  wa_stor_UCHAR(bInput, 2);
  wa_stor_UCHAR(bIV, 5);
  wa_stor_UCHAR(bOutput, 7);
}
#endif

// NTSTATUS WINAPI BCryptExportKey (BCRYPT_KEY_HANDLE hKey, BCRYPT_KEY_HANDLE hExportKey, LPCWSTR pszBlobType, PUCHAR pbOutput, ULONG cbOutput, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTEXPORTKEY )
{
  void * str3{};
  wa_ret_NTSTATUS(BCryptExportKey( ###, ###, HB_PARSTR(3, &str3, nullptr), ###, wa_par_ULONG(5), ###, wa_par_ULONG(7) ));
  hb_strfree(str3);
}
#endif

// NTSTATUS WINAPI BCryptImportKey (BCRYPT_ALG_HANDLE hAlgorithm, BCRYPT_KEY_HANDLE hImportKey, LPCWSTR pszBlobType, BCRYPT_KEY_HANDLE *phKey, PUCHAR pbKeyObject, ULONG cbKeyObject, PUCHAR pbInput, ULONG cbInput, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTIMPORTKEY )
{
  void * str3{};
  wa_ret_NTSTATUS(BCryptImportKey( ###, ###, HB_PARSTR(3, &str3, nullptr), ###, ###, wa_par_ULONG(6), ###, wa_par_ULONG(8), wa_par_ULONG(9) ));
  hb_strfree(str3);
}
#endif

// NTSTATUS WINAPI BCryptImportKeyPair (BCRYPT_ALG_HANDLE hAlgorithm, BCRYPT_KEY_HANDLE hImportKey, LPCWSTR pszBlobType, BCRYPT_KEY_HANDLE *phKey, PUCHAR pbInput, ULONG cbInput, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTIMPORTKEYPAIR )
{
  void * str3{};
  wa_ret_NTSTATUS(BCryptImportKeyPair( ###, ###, HB_PARSTR(3, &str3, nullptr), ###, ###, wa_par_ULONG(6), wa_par_ULONG(7) ));
  hb_strfree(str3);
}
#endif

// NTSTATUS WINAPI BCryptDuplicateKey (BCRYPT_KEY_HANDLE hKey, BCRYPT_KEY_HANDLE *phNewKey, PUCHAR pbKeyObject, ULONG cbKeyObject, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDUPLICATEKEY )
{
  wa_ret_NTSTATUS(BCryptDuplicateKey( ###, ###, ###, wa_par_ULONG(4), wa_par_ULONG(5) ));
}
#endif

// NTSTATUS WINAPI BCryptFinalizeKeyPair (BCRYPT_KEY_HANDLE hKey, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTFINALIZEKEYPAIR )
{
  wa_ret_NTSTATUS(BCryptFinalizeKeyPair( ###, wa_par_ULONG(2) ));
}
#endif

// NTSTATUS WINAPI BCryptDestroyKey (BCRYPT_KEY_HANDLE hKey)
#if 0
HB_FUNC( WABCRYPTDESTROYKEY )
{
  wa_ret_NTSTATUS(BCryptDestroyKey( ### ));
}
#endif

// NTSTATUS WINAPI BCryptDestroySecret (BCRYPT_SECRET_HANDLE hSecret)
#if 0
HB_FUNC( WABCRYPTDESTROYSECRET )
{
  wa_ret_NTSTATUS(BCryptDestroySecret( ### ));
}
#endif

// NTSTATUS WINAPI BCryptSignHash (BCRYPT_KEY_HANDLE hKey, VOID *pPaddingInfo, PUCHAR pbInput, ULONG cbInput, PUCHAR pbOutput, ULONG cbOutput, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTSIGNHASH )
{
  wa_ret_NTSTATUS(BCryptSignHash( ###, ###, ###, wa_par_ULONG(4), ###, wa_par_ULONG(6), ###, wa_par_ULONG(8) ));
}
#endif

// NTSTATUS WINAPI BCryptVerifySignature (BCRYPT_KEY_HANDLE hKey, VOID *pPaddingInfo, PUCHAR pbHash, ULONG cbHash, PUCHAR pbSignature, ULONG cbSignature, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTVERIFYSIGNATURE )
{
  wa_ret_NTSTATUS(BCryptVerifySignature( ###, ###, ###, wa_par_ULONG(4), ###, wa_par_ULONG(6), wa_par_ULONG(7) ));
}
#endif

// NTSTATUS WINAPI BCryptSecretAgreement (BCRYPT_KEY_HANDLE hPrivKey, BCRYPT_KEY_HANDLE hPubKey, BCRYPT_SECRET_HANDLE *phAgreedSecret, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTSECRETAGREEMENT )
{
  wa_ret_NTSTATUS(BCryptSecretAgreement( ###, ###, ###, wa_par_ULONG(4) ));
}
#endif

// NTSTATUS WINAPI BCryptDeriveKey (BCRYPT_SECRET_HANDLE hSharedSecret, LPCWSTR pwszKDF, BCryptBufferDesc *pParameterList, PUCHAR pbDerivedKey, ULONG cbDerivedKey, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDERIVEKEY )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptDeriveKey( ###, HB_PARSTR(2, &str2, nullptr), ###, ###, wa_par_ULONG(5), ###, wa_par_ULONG(7) ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptKeyDerivation (BCRYPT_KEY_HANDLE hKey, BCryptBufferDesc *pParameterList, PUCHAR pbDerivedKey, ULONG cbDerivedKey, ULONG *pcbResult, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTKEYDERIVATION )
{
  wa_ret_NTSTATUS(BCryptKeyDerivation( ###, ###, ###, wa_par_ULONG(4), ###, wa_par_ULONG(6) ));
}
#endif

// NTSTATUS WINAPI BCryptCreateHash (BCRYPT_ALG_HANDLE hAlgorithm, BCRYPT_HASH_HANDLE *phHash, PUCHAR pbHashObject, ULONG cbHashObject, PUCHAR pbSecret, ULONG cbSecret, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTCREATEHASH )
{
  wa_ret_NTSTATUS(BCryptCreateHash( ###, ###, ###, wa_par_ULONG(4), ###, wa_par_ULONG(6), wa_par_ULONG(7) ));
}
#endif

// NTSTATUS WINAPI BCryptHashData (BCRYPT_HASH_HANDLE hHash, PUCHAR pbInput, ULONG cbInput, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTHASHDATA )
{
  wa_ret_NTSTATUS(BCryptHashData( ###, ###, wa_par_ULONG(3), wa_par_ULONG(4) ));
}
#endif

// NTSTATUS WINAPI BCryptFinishHash (BCRYPT_HASH_HANDLE hHash, PUCHAR pbOutput, ULONG cbOutput, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTFINISHHASH )
{
  wa_ret_NTSTATUS(BCryptFinishHash( ###, ###, wa_par_ULONG(3), wa_par_ULONG(4) ));
}
#endif

// NTSTATUS WINAPI BCryptDuplicateHash (BCRYPT_HASH_HANDLE hHash, BCRYPT_HASH_HANDLE *phNewHash, PUCHAR pbHashObject, ULONG cbHashObject, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDUPLICATEHASH )
{
  wa_ret_NTSTATUS(BCryptDuplicateHash( ###, ###, ###, wa_par_ULONG(4), wa_par_ULONG(5) ));
}
#endif

// NTSTATUS WINAPI BCryptDestroyHash (BCRYPT_HASH_HANDLE hHash)
#if 0
HB_FUNC( WABCRYPTDESTROYHASH )
{
  wa_ret_NTSTATUS(BCryptDestroyHash( ### ));
}
#endif

// NTSTATUS WINAPI BCryptGenRandom (BCRYPT_ALG_HANDLE hAlgorithm, PUCHAR pbBuffer, ULONG cbBuffer, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTGENRANDOM )
{
  wa_ret_NTSTATUS(BCryptGenRandom( ###, ###, wa_par_ULONG(3), wa_par_ULONG(4) ));
}
#endif

// NTSTATUS WINAPI BCryptDeriveKeyCapi (BCRYPT_HASH_HANDLE hHash, BCRYPT_ALG_HANDLE hTargetAlg, PUCHAR pbDerivedKey, ULONG cbDerivedKey, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDERIVEKEYCAPI )
{
  wa_ret_NTSTATUS(BCryptDeriveKeyCapi( ###, ###, ###, wa_par_ULONG(4), wa_par_ULONG(5) ));
}
#endif

// NTSTATUS WINAPI BCryptDeriveKeyPBKDF2 (BCRYPT_ALG_HANDLE hPrf, PUCHAR pbPassword, ULONG cbPassword, PUCHAR pbSalt, ULONG cbSalt, ULONGLONG cIterations, PUCHAR pbDerivedKey, ULONG cbDerivedKey, ULONG dwFlags)
#if 0
HB_FUNC( WABCRYPTDERIVEKEYPBKDF2 )
{
  wa_ret_NTSTATUS(BCryptDeriveKeyPBKDF2( ###, ###, wa_par_ULONG(3), ###, wa_par_ULONG(5), ###, ###, wa_par_ULONG(8), wa_par_ULONG(9) ));
}
#endif

// NTSTATUS WINAPI BCryptResolveProviders (LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, LPCWSTR pszProvider, ULONG dwMode, ULONG dwFlags, ULONG *pcbBuffer, PCRYPT_PROVIDER_REFS *ppBuffer)
#if 0
HB_FUNC( WABCRYPTRESOLVEPROVIDERS )
{
  void * str1{};
  void * str3{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptResolveProviders( HB_PARSTR(1, &str1, nullptr), wa_par_ULONG(2), HB_PARSTR(3, &str3, nullptr), HB_PARSTR(4, &str4, nullptr), wa_par_ULONG(5), wa_par_ULONG(6), ###, ### ));
  hb_strfree(str1);
  hb_strfree(str2);
  hb_strfree(str3);
}
#endif

// NTSTATUS WINAPI BCryptGetFipsAlgorithmMode (BOOLEAN *pfEnabled)
#if 0
HB_FUNC( WABCRYPTGETFIPSALGORITHMMODE )
{
  wa_ret_NTSTATUS(BCryptGetFipsAlgorithmMode( ### ));
}
#endif

// NTSTATUS WINAPI BCryptQueryProviderRegistration (LPCWSTR pszProvider, ULONG dwMode, ULONG dwInterface, ULONG *pcbBuffer, PCRYPT_PROVIDER_REG *ppBuffer)
#if 0
HB_FUNC( WABCRYPTQUERYPROVIDERREGISTRATION )
{
  void * str1{};
  wa_ret_NTSTATUS(BCryptQueryProviderRegistration( HB_PARSTR(1, &str1, nullptr), wa_par_ULONG(2), wa_par_ULONG(3), ###, ### ));
  hb_strfree(str1);
}
#endif

// NTSTATUS WINAPI BCryptEnumRegisteredProviders (ULONG *pcbBuffer, PCRYPT_PROVIDERS *ppBuffer)
#if 0
HB_FUNC( WABCRYPTENUMREGISTEREDPROVIDERS )
{
  wa_ret_NTSTATUS(BCryptEnumRegisteredProviders( ###, ### ));
}
#endif

// NTSTATUS WINAPI BCryptCreateContext (ULONG dwTable, LPCWSTR pszContext, PCRYPT_CONTEXT_CONFIG pConfig)
#if 0
HB_FUNC( WABCRYPTCREATECONTEXT )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptCreateContext( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), ### ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptDeleteContext (ULONG dwTable, LPCWSTR pszContext)
#if 0
HB_FUNC( WABCRYPTDELETECONTEXT )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptDeleteContext( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr) ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptEnumContexts (ULONG dwTable, ULONG *pcbBuffer, PCRYPT_CONTEXTS *ppBuffer)
#if 0
HB_FUNC( WABCRYPTENUMCONTEXTS )
{
  wa_ret_NTSTATUS(BCryptEnumContexts( wa_par_ULONG(1), ###, ### ));
}
#endif

// NTSTATUS WINAPI BCryptConfigureContext (ULONG dwTable, LPCWSTR pszContext, PCRYPT_CONTEXT_CONFIG pConfig)
#if 0
HB_FUNC( WABCRYPTCONFIGURECONTEXT )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptConfigureContext( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), ### ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptQueryContextConfiguration (ULONG dwTable, LPCWSTR pszContext, ULONG *pcbBuffer, PCRYPT_CONTEXT_CONFIG *ppBuffer)
#if 0
HB_FUNC( WABCRYPTQUERYCONTEXTCONFIGURATION )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptQueryContextConfiguration( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), ###, ### ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptAddContextFunction (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, ULONG dwPosition)
#if 0
HB_FUNC( WABCRYPTADDCONTEXTFUNCTION )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptAddContextFunction( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), wa_par_ULONG(5) ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptRemoveContextFunction (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction)
#if 0
HB_FUNC( WABCRYPTREMOVECONTEXTFUNCTION )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptRemoveContextFunction( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr) ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptEnumContextFunctions (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, ULONG *pcbBuffer, PCRYPT_CONTEXT_FUNCTIONS *ppBuffer)
#if 0
HB_FUNC( WABCRYPTENUMCONTEXTFUNCTIONS )
{
  void * str2{};
  wa_ret_NTSTATUS(BCryptEnumContextFunctions( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), ###, ### ));
  hb_strfree(str2);
}
#endif

// NTSTATUS WINAPI BCryptConfigureContextFunction (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, PCRYPT_CONTEXT_FUNCTION_CONFIG pConfig)
#if 0
HB_FUNC( WABCRYPTCONFIGURECONTEXTFUNCTION )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptConfigureContextFunction( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), ### ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptQueryContextFunctionConfiguration (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, ULONG *pcbBuffer, PCRYPT_CONTEXT_FUNCTION_CONFIG *ppBuffer)
#if 0
HB_FUNC( WABCRYPTQUERYCONTEXTFUNCTIONCONFIGURATION )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptQueryContextFunctionConfiguration( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), ###, ### ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptEnumContextFunctionProviders (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, ULONG *pcbBuffer, PCRYPT_CONTEXT_FUNCTION_PROVIDERS *ppBuffer)
#if 0
HB_FUNC( WABCRYPTENUMCONTEXTFUNCTIONPROVIDERS )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptEnumContextFunctionProviders( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), ###, ### ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptSetContextFunctionProperty (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, LPCWSTR pszProperty, ULONG cbValue, PUCHAR pbValue)
#if 0
HB_FUNC( WABCRYPTSETCONTEXTFUNCTIONPROPERTY )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptSetContextFunctionProperty( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), HB_PARSTR(5, &str5, nullptr), wa_par_ULONG(6), ### ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptQueryContextFunctionProperty (ULONG dwTable, LPCWSTR pszContext, ULONG dwInterface, LPCWSTR pszFunction, LPCWSTR pszProperty, ULONG *pcbValue, PUCHAR *ppbValue)
#if 0
HB_FUNC( WABCRYPTQUERYCONTEXTFUNCTIONPROPERTY )
{
  void * str2{};
  void * str4{};
  wa_ret_NTSTATUS(BCryptQueryContextFunctionProperty( wa_par_ULONG(1), HB_PARSTR(2, &str2, nullptr), wa_par_ULONG(3), HB_PARSTR(4, &str4, nullptr), HB_PARSTR(5, &str5, nullptr), ###, ### ));
  hb_strfree(str2);
  hb_strfree(str4);
}
#endif

// NTSTATUS WINAPI BCryptRegisterConfigChangeNotify (HANDLE *phEvent)
#if 0
HB_FUNC( WABCRYPTREGISTERCONFIGCHANGENOTIFY )
{
  wa_ret_NTSTATUS(BCryptRegisterConfigChangeNotify( ### ));
}
#endif

// NTSTATUS WINAPI BCryptRegisterConfigChangeNotify (PRKEVENT pEvent)
#if 0
HB_FUNC( WABCRYPTREGISTERCONFIGCHANGENOTIFY )
{
  wa_ret_NTSTATUS(BCryptRegisterConfigChangeNotify( ### ));
}
#endif

// NTSTATUS WINAPI BCryptUnregisterConfigChangeNotify (PRKEVENT pEvent)
#if 0
HB_FUNC( WABCRYPTUNREGISTERCONFIGCHANGENOTIFY )
{
  wa_ret_NTSTATUS(BCryptUnregisterConfigChangeNotify( ### ));
}
#endif

// NTSTATUS WINAPI BCryptUnregisterConfigChangeNotify (HANDLE hEvent)
#if 0
HB_FUNC( WABCRYPTUNREGISTERCONFIGCHANGENOTIFY )
{
  wa_ret_NTSTATUS(BCryptUnregisterConfigChangeNotify(wa_par_HANDLE(1)));
}
#endif
