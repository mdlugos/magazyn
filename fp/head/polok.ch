#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_WIN_PRN eval(MEMVAR->do_getoprn)
  #define A_IDENT min(hb_fieldlen('ident'),18)
#else
  #define A_IDENT min(hb_fieldlen('ident'),24)
#endif
#define A_STOPKA 'Program: System Fi-Ks, '+wersja()+', producent: Firma Usług Informatycznych Marek Długosz, 43-400 Cieszyn, ul. Równa 16'
#include "lan.ch"
#define B_SKIP  {|x|dbskip(x),__dbLocate( for,while,,,.t.),found()}
#define A_NOREFRESH
#define PC852 "PL852M"
#define A_BACKUP memvar->backup
#define UpP(x) UPPER(x)
#define A_DRUKCOMP
#define isPrinter() .t.
#define A_GETLPT   eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define iS_spec .t.
#define hAslo_spec(x) setpos(x,0)
#command INIT SCREEN  => //__run("852 r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define A_SET_DAT GERMAN
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define DatE() MEMVAR->dzisiaj
#define A_KTL 8
#define A_WAL 11
#define A_15CALI
#define A_PROWMAX MEMVAR->p_rown
#define A_FILELIMIT '50'
#define A_NRLTH 5
#define A_SUMK   -39301482331
#define A_KOMU_N "FH AMIPOL Sp.J. Polok & Polok"
#define A_KOMU_A "Cieszyn, ul. Filasiewicza 3"
#define A_AUTOR  "A.D. 1995-2016, Marek Długosz, 43-400 Cieszyn, ul. Równa 16, tel. 338522553"
#define A_MYSZ
#define A_DSPLIT
#define A_LPNUM 3
#define LP(x) x
