#include "lan.ch"
#define A_CDX DBFCDX
#define PC852
#command INIT SCREEN =>
//#command INIT PRINTER => qqout(if(wasbad,memoread('c:\hpljutil\rcp\852.pjl'),MEMVAR->p_init));wasbad:=.f.
#define UpP(x) UPPER(x)
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_WIN_PRN memvar->win_prn
#endif
#define A_BACKUP memvar->bejkap
#define A_NOZAP
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_ZLEC11
#define A_15CPI
#define A_XPRN
#define A_DRUKCOMP
#define A_PCL
#define A_STYLUS
#define isPrinter() .t.
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() memvar->dzisiaj
#define A_WA
#define A_WE
#define A_FIFO
#define A_LPNUM 3
#define A_SET_DAT GERMAN
#define STANY INDX_MAT
#define A_SUMK    -33435619564
#define A_KOMU_N  "DOM POMOCY SPOùECZNEJ"
#define A_KOMU_A  "Izdebnik 3"
#define A_AUTOR   "A.D.1991-2012, Marek Dàugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_SHORTIND
#define A_TRWALOSC
#define A_FILELIMIT '35'
#define A_DIETA   alias()#"INDX_MAT"
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
