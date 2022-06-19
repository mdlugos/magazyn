#include "mdstd.ch"
#define PC852
#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
  #define A_WIN_PRN .t.
#endif
#define UpP(x) UPPER(x)
#define A_LPNUM 3
#command INIT SCREEN => //__run("uniznak 8 r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_STYLUS
#define A_PCL
#define A_XPRN
#define isPrinter() .t.
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_WE
#define A_FIFO
#define A_SUMK    -37349365841
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Dzieci"
#define A_KOMU_A  "Skocz¢w, ul. Mickiewicza 36"
#define A_AUTOR   "A.D. 1992-2012, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_SHORTIND
//#define A_SWW
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
//#define A_LFIRST
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
//#define A_MULTIDI
#define A_EXT __RUN
#define A_BACKUP  memvar->bejkap
