#include "lan.ch"
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#ifdef __PLATFORM__WINDOWS
 #define A_WIN_PRN .t.
 #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
 #define PLWIN
#endif
#define A_DOKCOMP 8
#define A_DRUKCOMP
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_15CPI
#define A_XPRN
#define A_PCL
#define A_STYLUS
#define A_ZLEC11
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -34772187218
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Dzieci"
#define A_KOMU_A  "Strumieä, ul. 1-go Maja 12"
#define A_AUTOR   "A.D. 1992-2017, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_WA
#define A_FIFO
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define DatE() MEMVAR->dzisiaj
