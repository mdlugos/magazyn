#include "mdstd.ch"
#define PC852
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_STYLUS
#define A_PCL
#define A_XPRN
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_EXT __RUN
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
//#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_FIFO
#define A_SUMK    -38490019688
#define A_KOMU_N  '"CENTRUM" —rodowiskowy Dom Samopomocy'
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Sobieskiego 158"
#define A_AUTOR   "A.D. 1992-2005, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_SHORTIND
//#define A_SWW
//#define A_TRWALOSC
//#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "#X##"
#define A_LFIRST
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_LPNUM 3
//#define A_MULTIDI
//#define A_KOMOR
#define A_WADO "Wystawiˆ:                                               Zatwierdziˆ:"
