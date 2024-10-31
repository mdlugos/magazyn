#include "lan.ch"
#define A_DDBF
#command INIT SCREEN => //__run("font852")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define PC852
#define PLWIN
#define UpP(x) UPPER(x)
#define A_ZLEC11
#define A_CDX DBFCDX
#define A_GREX
#define A_NOZAP
#define A_DRUKCOMP
#define A_STYLUS
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(SubStr(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SUMK  -47291241801
#define A_KOMU_N  'Powiatowy Dom Pomocy Społecznej "Feniks" w Skoczowie'
#define A_KOMU_A  "Skoczów, ul. Sportowa 13"
#define A_AUTOR   "A.D. 1992-2006, Marek Długosz, Cieszyn, ul. Równa 16, tel. 0338522553"
#define iS_spec .t.
//#define hAslo_spec(x) ALARM("TYLKO WERSJA DEMO NIE POSIADA ZABEZPIECZENIA PRZED NIEUPOWŻNIONYM DOSTĘPEM DO TEJ FUNKCJI.")
#define A_WA
#define A_FIFO
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_LFIRST
#define INDEXPIC "XXXX"
#define A_MYSZ
//#define A_NOZAP
#define A_DLINK
#define A_SB
