#include "std.ch"
#define A_CDX DBFMDX
#define A_MDX
#define UpP(x) UPPER(x)
#define A_LPNUM 3
#define A_KHSEP
#define PC852
#command init screen  => //__run("uniznak r 8")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_ZAZNACZ
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_DLINK
//#define A_FIFO
//#define A_WA
#define A_SET_DAT   GERMAN
#define A_SWW
#define STANY     indx_mat
#define A_SUMK    -46322971219
#define A_KOMU_N  'Korespondencyjna Szkoˆa Biblijna'
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Kochanowskiego 2"
#define A_AUTOR   "A.D. 2001, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_FILELIMIT '36'
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "XXXXXX"
#define A_LFIRST
#define A_MYSZ
#define A_DMDATA
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_FFULL
