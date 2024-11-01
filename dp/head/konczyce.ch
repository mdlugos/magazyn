#include "lan.ch"
//#define ZAP_BIEZ MEMVAR->zap_biez
//#define MAG_BIEZ '10'
#define A_NORMY
//#define A_SUMOS 3
#define A_GREX
#define PC852
#define PLWIN
//#define A_WAGI
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_DEKDUZE
//#define A_POLOWA
#define A_DILTH 9
#define A_ZAP_DAN
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_PCL
#define A_XPRN
#define A_WIN_PRN eval(MEMVAR->do_getoprn)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define isPrinter() .t.
#define A_15CPI
#define DTOV(dat) tranr(SubStr(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define A_DDBF
#define STANY   INDX_MAT
#define A_SUMK    -55705546984
#define A_KOMU_N  'Gminne Przedszkole Publiczne w Zebrzydowicach o/Ko�czyce Ma�e'
#define A_KOMU_A  'Ko�czyce Ma�e, ul. Jagiello�ska 22a'
#define A_AUTOR   "A.D. 2022, Marek D�ugosz, Cieszyn ul. R�wna 16, tel. 601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_LPNUM 2
#define A_WADO eval(MEMVAR->podpis)
