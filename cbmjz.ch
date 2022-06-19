#include "lan.ch"
#define PC852
//#define A_CDX DBFMDX
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define UpP(x)     UPPER(x)
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_PCL
#define A_XPRN
#define A_STYLUS
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -43823264680
#define A_KOMU_N  "Zgr. C¢rek Bo¾ej Miˆo˜ci Zakˆad Leczniczo-Opiekuäczy"
#define A_KOMU_A  "Jastrz©bie Zdr¢j, ul. Pszczyäska 11"
#define A_AUTOR   "A.D. 1992-1999, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_WA
#define A_FIFO
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_SB
#define A_FILELIMIT '35'
//#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_LPNUM 3
