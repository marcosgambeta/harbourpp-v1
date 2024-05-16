/* Viktor Szakats (vszakats.net/harbour) */

#if defined(__BORLANDC__)
#define HB_CP_UPPER     "A\xC4\x82\xC3\x82" "BCDEFGHI\xC3\x8E" "JKLMNOPQRS\xC5\x9E" "T\xC5\xA2" "UVWXYZ"
#define HB_CP_LOWER     "a\xC4\x83\xC3\xA2" "bcdefghi\xC3\xAE" "jklmnopqrs\xC5\x9F" "t\xC5\xA3" "uvwxyz"
#else
#define HB_CP_UPPER     "AĂÂBCDEFGHIÎJKLMNOPQRSŞTŢUVWXYZ"
#define HB_CP_LOWER     "aăâbcdefghiîjklmnopqrsştţuvwxyz"
#endif
