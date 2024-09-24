//#define A_UNICODE 'UTF8MD'
#include "lan.ch"
#define A_DDBF
#command INIT SCREEN => //__run("font852")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define UpP(x) UPPER(x)
#define A_XPRN
#define A_NORMY
#define PC852 'PL852M'
#define PLWIN
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_DILTH 9
#define A_ZAP_DAN
#define A_CDX DBFCDX
#define A_GREX
#define A_DRUKCOMP
#define A_STYLUS
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_PCL
#define isPrinter() .t.
#define DatE()    MEMVAR->dzisiaj
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SUMK  -47291241801
#define A_KOMU_N  'Powiatowy Dom Pomocy Społecznej "Feniks" w Skoczowie'
#define A_KOMU_A  "Skoczów, ul. Sportowa 13"
#define A_AUTOR   "A.D. 1992-2006, Marek Długosz, Cieszyn, ul. Równa 16, tel. 0338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_WADO eval(memvar->podpisy)
#define A_NOZAP
#define A_LPNUM 2
//#define A_NOZAP
