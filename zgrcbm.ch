#include "std.ch"
#command INIT SCREEN =>
#define PC852
#define A_STYLUS
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_DRUKCOMP
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_PCL
#define UpP(x) UPPER(x)
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SUMK    -40639611417
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Dzieci"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. ½ywiecka 20"
#define A_AUTOR   "A.D. 1992-1997, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
//#define A_LPNUM
#define A_WADO eval(memvar->podpisy)
