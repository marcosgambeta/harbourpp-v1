// Mindaugas Kavaliauskas <dbtopas@dbtopas.lt>

#if defined(__BORLANDC__)
#define HB_CP_UPPER     "A\xC4\x84" "BC\xC4\x8C" "DE\xC4\x98\xC4\x96" "FGHI\xC4\xAE" "YJKLMNOPQRS\xC5\xA0" "TU\xC5\xB2\xC5\xAA" "VWXZ\xC5\xBD"
#define HB_CP_LOWER     "a\xC4\x85" "bc\xC4\x8D" "de\xC4\x99\xC4\x97" "fghi\xC4\xAF" "yjklmnopqrs\xC5\xA1" "tu\xC5\xB3\xC5\xAB" "vwxz\xC5\xBE"
#else
#define HB_CP_UPPER     "AĄBCČDEĘĖFGHIĮYJKLMNOPQRSŠTUŲŪVWXZŽ"
#define HB_CP_LOWER     "aąbcčdeęėfghiįyjklmnopqrsštuųūvwxzž"
#endif
