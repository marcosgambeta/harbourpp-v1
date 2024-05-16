/*
 * National Collation Support Module (FI850)
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 * This file is generated automatically by cpinfo.prg
 */

#define HB_CP_ID        FI850
#define HB_CP_INFO      "Finnish CP-850 (ntxfin.obj compatible)"
#define HB_CP_UNITB     HB_UNITB_850
#define HB_CP_ACSORT    HB_CDP_ACSORT_NONE
#if defined(__BORLANDC__)
#define HB_CP_UPPER     "ABCDE\xC3\x89" "FGHIJKLMNOPQRSTUVWXY\xC3\x9C" "Z\xC3\x85\xC3\x84\xC3\x96"
#define HB_CP_LOWER     "abcde\xC3\xA9" "fghijklmnopqrstuvwxy\xC3\xBC" "z\xC3\xA5\xC3\xA4\xC3\xB6"
#else
#define HB_CP_UPPER     "ABCDEÉFGHIJKLMNOPQRSTUVWXYÜZÅÄÖ"
#define HB_CP_LOWER     "abcdeéfghijklmnopqrstuvwxyüzåäö"
#endif
#define HB_CP_UTF8

/* include CP registration code */
#include "hbcdpreg.hpp"
