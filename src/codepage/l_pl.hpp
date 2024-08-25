// Jacek Kubica <kubica@wssk.wroc.pl>

#if defined(__BORLANDC__)
#define HB_CP_UPPER     "A\xC4\x84" "BC\xC4\x86" "DE\xC4\x98" "FGHIJKL\xC5\x81" "MN\xC5\x83" "O\xC3\x93" "PQRS\xC5\x9A" "TUVWXYZ\xC5\xB9\xC5\xBB"
#define HB_CP_LOWER     "a\xC4\x85" "bc\xC4\x87" "de\xC4\x99" "fghijkl\xC5\x82" "mn\xC5\x84" "o\xC3\xB3" "pqrs\xC5\x9B" "tuvwxyz\xC5\xBA\xC5\xBC"
#else
#define HB_CP_UPPER     "AĄBCĆDEĘFGHIJKLŁMNŃOÓPQRSŚTUVWXYZŹŻ"
#define HB_CP_LOWER     "aąbcćdeęfghijklłmnńoópqrsśtuvwxyzźż"
#endif
