#include "std.ch"
#define UpP(x) UPPER(x)
#define A_CDX DBFCDX
#define A_WAGI
#define PROC_EN
#define A_POLOWA
#command INITIALIZE SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define PC852
#define A_PCL
#define A_XPRN
#define isPrinter .t.
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() MEMVAR->dzisiaj
#define A_STYLUS
#define A_LPNUM 2
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -34224402615
#define A_KOMU_N  'Ewangelicki Dom Opieki "Emaus"'
#define A_KOMU_A  "Dzi©giel¢w 257"
#define A_AUTOR   "A.D. 1991-1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_WADO eval(MEMVAR->podpis)
