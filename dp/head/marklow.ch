#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
#define A_WIN_PRN eval(MEMVAR->do_getoprn)
#endif
#define A_ADS 1
#define A_CDX ADSCDX

#define ZAP_BIEZ MEMVAR->zap_biez
#define MAG_BIEZ '10'
#define A_NORMY
#define A_SUMOS 3
#define A_GREX
#define PC852 'PL852M'
//#define PLWIN
//#define A_WAGI
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_DEKDUZE
//#define A_POLOWA
#define A_DILTH 9
#define A_ZAP_DAN
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_PCL
#define A_XPRN
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define isPrinter() .t.
#define A_15CPI
#define DTOV(dat) tranr(SubStr(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define A_DDBF
#define STANY   INDX_MAT
#define A_SUMK    -54629700500
#define A_KOMU_N  'Gminne Przedszkole Publiczne w Zebrzydowicach o/Marklowice Górne'
#define A_KOMU_A  'Marklowice Górne, ul. Szkolna 28'
#define A_AUTOR   "A.D. 2021, Marek Długosz, Cieszyn ul. Równa 16, tel. 601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_LPNUM 2
#define A_WADO eval(MEMVAR->podpis)
