#define PC852
#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
 #define A_WIN_PRN .t.
 #define PLWIN
 #define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
//#define A_CDX DBFCDX
#define A_MYSZ
//#define A_WAGI
#define PROC_EN memvar->proc_en
#define A_KODY "Kod"
#define A_NORMY
#define A_JMALT
#define A_WO_JAD '  3'
#define A_ZAP_DAN
#define A_POLOWA
#define A_GREX
#define A_DILTH 9
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_PCL
#define A_XPRN
#define A_15CPI
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#ifdef A_UNICODE
#define A_CDX VFPCDX
#else
#define A_SUMK    -39081393255
#define A_KOMU_N  'Centrum Reumatologii Sp. z o.o.'
#define A_KOMU_A  "Ustroä, ul. Szpitalna 11"
#define A_AUTOR   "A.D. 2016, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#endif
#define A_DIETA
#define A_FILELIMIT '45'
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
#define A_WADO eval(MEMVAR->podpis)
