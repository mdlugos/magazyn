#include "lan.ch"
#define PC852
#define PLWIN
#define UpP(x) UPPER(x)
#command INIT SCREEN => //__run("uniznak r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_GETLPT eval(memvar->do_getlpt)
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define A_EXT WIN_PRINTFILERAW,WIN_PRINTERGETDEFAULT,WIN_PRINTERLIST,WIN_PRINTERSTATUS
#define isPrinter() .t.
#define DTOV(dat) left(dtoc(dat),5)
#define A_DLINK
#define A_DOKCOMP 8
#define A_DRUKCOMP
#define A_XPRN
#define A_WA
//#define A_WE
//#define A_FIFO
#define A_SET_DAT GERMAN
//#define A_SHORTIND
#define A_SWW 'CPV:'
//#define A_PKWiU
#define A_ZLEC11
#define A_LFIRST
#define STANY INDX_MAT
#define A_SUMK    -32566727102
#define A_KOMU_N  "Dom Pomocy Spoˆecznej"
#define A_KOMU_A  "Wadowice, ul. Parkowa 1"
#define A_AUTOR   "A.D. 1992-2011, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "XXXXXX"
//#define A_KOMOR
#define A_MYSZ
#define A_NOZAP
#define DatE() memvar->dzisiaj
#define A_BACKUP MEMVAR->backup
#define A_JMALT
//#define A_SB
