// Przemyslaw Czerpak <druzus / at / priv.onet.pl>

#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "ABC\xC4\x8C\xC4\x86" "D\xC4\x90" "EFGHIJKLMNOPQRS\xC5\xA0" "TUVWXYZ\xC5\xBD"
constexpr const char *HB_CP_UPPER = "ABC\xC4\x8C\xC4\x86" "D\xC4\x90" "EFGHIJKLMNOPQRS\xC5\xA0" "TUVWXYZ\xC5\xBD";
//#define HB_CP_LOWER     "abc\xC4\x8D\xC4\x87" "d\xC4\x91" "efghijklmnopqrs\xC5\xA1" "tuvwxyz\xC5\xBE"
constexpr const char *HB_CP_LOWER = "abc\xC4\x8D\xC4\x87" "d\xC4\x91" "efghijklmnopqrs\xC5\xA1" "tuvwxyz\xC5\xBE";
#else
//#define HB_CP_UPPER     "ABCČĆDĐEFGHIJKLMNOPQRSŠTUVWXYZŽ"
constexpr const char *HB_CP_UPPER = "ABCČĆDĐEFGHIJKLMNOPQRSŠTUVWXYZŽ";
//#define HB_CP_LOWER     "abcčćdđefghijklmnopqrsštuvwxyzž"
constexpr const char *HB_CP_LOWER = "abcčćdđefghijklmnopqrsštuvwxyzž";
#endif
