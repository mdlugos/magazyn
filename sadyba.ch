#include "lan.ch"
//#define A_GOCZ
//-------------------------
//#define sur_naZ 1
//#define jmaG jm
//#define jedN jm_opcja
//#define indx_maT index
#define A_CDX DBFCDX
//#define A_MYSZ
//-------------------------
//#define A_CENPOS
#define A_WAGI
#define PROC_EN
#define A_WO_JAD 3
#define A_POLOWA
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
//#define A_DEKDUZE
#define PC852
#define A_PCL
#define A_XPRN
//#define A_OKI4W
#define A_STYLUS
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -46064109576
#define A_KOMU_N  'Dom Pomocy Spoˆecznej "SADYBA"'
#define A_KOMU_A  "Bystra Krakowska, ul. Klimczoka 80"
#define A_AUTOR   "A.D. 2010, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA
#define A_FILELIMIT '45'
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
#define A_WADO eval(MEMVAR->podpis)
