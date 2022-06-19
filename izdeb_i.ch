#include "lan.ch"
#define A_CDX DBFCDX
#define PC852
#command INIT SCREEN =>
//#command INIT PRINTER => qqout(if(wasbad,memoread('c:\hpljutil\rcp\852.pjl'),MEMVAR->p_init));wasbad:=.f.
#define UpP(x) UPPER(x)
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_15CPI
#define A_XPRN
#define A_DRUKCOMP
#define A_PCL
#define A_STYLUS
#define isPrinter() .t.
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() memvar->dzisiaj
//#define A_WA
//#define A_FIFO
#define A_LPNUM 3
#define A_SET_DAT GERMAN
#define STANY INDX_MAT
#define A_SUMK    -34686481916
#define A_KOMU_N  "DOM POMOCY SPOùECZNEJ"
#define A_KOMU_A  "Izdebnik 3"
#define A_AUTOR   "A.D.1991-2000, Marek Dàugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_SHORTIND
#define A_TRWALOSC
#define A_FILELIMIT '35'
#define A_DIETA   alias()#"INDX_MAT"
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
