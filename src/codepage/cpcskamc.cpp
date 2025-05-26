//
// National Collation Support Module (CSKAMC)
//
// Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//
// This file is generated automatically by cpinfo.prg
//

#define HB_CP_ID        CSKAMC
constexpr const char *HB_CP_INFO = "Czech Kamenicky (ntxcz895.obj compatible)"; // #define HB_CP_INFO      "Czech Kamenicky (ntxcz895.obj compatible)"
#define HB_CP_UNITB     HB_UNITB_KAM
#define HB_CP_ACSORT    HB_CDP_ACSORT_NONE
#define HB_CP_CSSORT    HB_CDP_CSSORT_MIXED
#if defined(__BORLANDC__)
#define HB_CP_UPPER     "A\xC3\x81\xC3\x84" "BC\xC4\x8C" "D\xC4\x8E" "E\xC3\x89\xC4\x9A" "FGHI\xC3\x8D" "JKL\xC4\xB9\xC4\xBD" "MN\xC5\x87" "O\xC3\x93\xC3\x94\xC3\x96" "PQR\xC5\x94\xC5\x98" "S\xC5\xA0" "T\xC5\xA4" "U\xC3\x9A\xC5\xAE\xC3\x9C" "VWXY\xC3\x9D" "Z\xC5\xBD"
#define HB_CP_LOWER     "a\xC3\xA1\xC3\xA4" "bc\xC4\x8D" "d\xC4\x8F" "e\xC3\xA9\xC4\x9B" "fghi\xC3\xAD" "jkl\xC4\xBA\xC4\xBE" "mn\xC5\x88" "o\xC3\xB3\xC3\xB4\xC3\xB6" "pqr\xC5\x95\xC5\x99" "s\xC5\xA1" "t\xC5\xA5" "u\xC3\xBA\xC5\xAF\xC3\xBC" "vwxy\xC3\xBD" "z\xC5\xBE"
#else
#define HB_CP_UPPER     "AÁÄBCČDĎEÉĚFGHIÍJKLĹĽMNŇOÓÔÖPQRŔŘSŠTŤUÚŮÜVWXYÝZŽ"
#define HB_CP_LOWER     "aáäbcčdďeéěfghiíjklĺľmnňoóôöpqrŕřsštťuúůüvwxyýzž"
#endif
#define HB_CP_UTF8

// include CP registration code
#include "hbcdpreg.hpp"
