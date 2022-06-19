#include "std.ch"
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define UpP(x) UPPER(x)
#define A_GETLPT eval(MEMVAR->getport)
#define A_PRINT(x) eval(MEMVAR->doprint,x)
#define A_EXT fcreateu,__run
#define A_STYLUS
#define A_PCL
#define A_15CPI
#define A_XPRN
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_FIFO
#define A_SUMK    -39077800247
#define A_KOMU_N  "Dom Pomocy Spoˆecznej"
#define A_KOMU_A  "Drogomy˜l, ul. Modrzewiowa 1"
#define A_AUTOR   "A.D. 1992-98, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define A_SHORTIND
//#define A_SWW
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_LPNUM 3
#define A_WADO eval(memvar->podpisy)
//#define A_MULTIDI
