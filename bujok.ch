#include "lan.ch"
#define A_CDX DBFCDX
#define PC852
#define UpP(x) UPPER(x)
#define A_FIFO
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
//qqout("&l3A(17U(s10h12V")
#define A_DRUKCOMP
#define A_XPRN
//#define A_STYLUS
#define A_PCL
#define A_WA
#define A_SB
//#define A_SHORTIND
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -31818741052
#define A_KOMU_N  "DOM POMOCY SPOECZNEJ"
#define A_KOMU_A  "Cieszyn, ul. Korfantego 1"
#define A_AUTOR   "A.D. 1991-2001, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define DatE()    MEMVAR->dzisiaj
#define A_TRWALOSC
#define A_FILELIMIT '35'
#define A_DIETA   alias()#"INDX_MAT"
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "######"
#define A_LPNUM 3
#define A_SWW "PKWiU"
//#define A_SWWKTM
