#include "mdstd.ch"
#define PLWIN
#command INIT SCREEN => //__run("font")
#command INIT PRINTER => specout(eval(MEMVAR->P_INIT,wasbad))
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_WA
#define A_DLINK
#define A_KHNAZ 20
//#define A_SWWKTM 'Kontrakt'
#define A_SWW 'CPV:'
//#define A_PR_ZW
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
//  #define A_WIN_PRN .t.
#define A_PCL
#define A_DRUKCOMP
#define A_STYLUS
#define A_15CPI
#define A_XPRN
#define UpP(x) UPPER(x)
#define A_WADO eval(MEMVAR->podpis)
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define PC852
#define A_FIFO
#define A_CEOKR
#define A_SET_DAT GERMAN
#define A_SHORTIND
#define STANY INDX_MAT
#define A_SUMK    -34434014895
#define A_KOMU_N  "Dom Pomocy Spoˆecznej"
#define A_KOMU_A  "Bobrek, ul. Ksi©¾nej Ogiäskiej 2"
#define A_AUTOR   "A.D. 1995-2006, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "######"
//#define A_FDO
#define A_ZLEC11
#define A_RODZ_S
#define A_FDO
//#define A_KOMOR
#define A_MYSZ
#define A_NOZAP
