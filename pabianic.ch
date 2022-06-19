#include "lan.ch"
//#define A_GOCZ
#define A_WAGI
#define PROC_EN
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_DEKDUZE
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
#define A_SUMK    -44286103578
#define A_KOMU_N  "Przedszkole Nr 10 im. Jana Pawˆa II"
#define A_KOMU_A  "Pabianice, ul Warszawska 36a"
#define A_AUTOR   "A.D. 1992-2003, Marek Dˆugosz, http://www.polbox.com/m/mdlugosz, tel.0601842030"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
