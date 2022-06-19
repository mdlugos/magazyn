#include "std.ch"
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define MAG_BIEZ " 2"
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -37855126305
#define A_KOMU_N  "Dom Spokojnej Starožci"
#define A_KOMU_A  "Cieszyn, ul. Mickiewicza 13"
#define A_AUTOR   "A.D. 1992-2000, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define DatE()    MEMVAR->dzisiaj
#define A_LPNUM   2
#define A_DRUKCOMP
#define A_POLOWA
#define A_WADO eval(memvar->podpis)
