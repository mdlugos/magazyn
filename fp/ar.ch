#define AR_SMB       1
#define AR_NAME      2
#define AR_DBF       3
#define AR_REL       4
#define AR_HEADER    5
#define AR_LINES     6
#define AR_DOKUMENTY 7

#define AR_LTH       7

#define ARR_FROM     1
#define ARR_TO       2
#define ARR_TOORD    3
#define ARR_KEY      4

#define ARR_LTH      4

#define AD_SMB       1
#define AD_NAME      2
#define AD_POLA      3
#define AD_VALID     4
#define AD_FLAGS     5
#define AD_KOPI      6
#define AD_DPROC     7
#define AD_PREPROC   8
#define AD_JPK_GDZIE 9
#define AD_JPK_CO   10

#define AD_LTH      10

#define AP_NAME      1
#define AP_FIELD     2
#define AP_PICTURE   3
#define AP_VALID     4
#define AP_WHEN      5
#define AP_POZWN     6
#define AP_KTWN      7
#define AP_POZMA     8
#define AP_KTMA      9
#define AP_WNVAL    10
#define AP_MAVAL    11
#define AP_IDWN     12
#define AP_IDMA     13

#define AP_LTH      13

#define STAT_WRT     1 //nie zadekretowany
#define STAT_BADDEK  2 //źle zadekretowany
#define STAT_DEK     3 //zadekretowany
#define STAT_NOWRT   4 //zamknięty
#ifndef STAT_ZATW
#define STAT_ZATW    STAT_NOWRT //zatwierdzony
#endif
