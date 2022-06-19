#include "lan.ch"
#define PC852
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x,getenv('MDSLPT'))
//#define A_MM
#define A_MAGDI " "
#define A_MULTIDI memvar->multidi
#define A_PCL
#define A_XPRN
#define A_STYLUS
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
//#define STANY     INDX_MAT
#define A_SUMK    -39581597578
#define A_KOMU_N  "Przedszkole Nr 40 Zgr. C¢rek Bo¾ej Miˆo˜ci"
#define A_KOMU_A  "Krak¢w P©dzich¢w 16"
#define A_AUTOR   "A.D. 1992-2004, Marek Dˆugosz, http://www.polbox.com/m/mdlugosz, tel.0601842030"
#define A_WA
#define A_FIFO
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_SB
#define A_FILELIMIT '45'
//#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "###!"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_LPNUM 3
#define A_KHNAZ 20
#define A_DOKFAK
