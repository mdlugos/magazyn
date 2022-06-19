#include "lan.ch"
#define A_CDX DBFCDX
#define PC852
#define PLWIN
#define UpP(x) UPPER(x)
#define A_FIFO
#define A_ZAZNACZ
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_WIN_PRN   eval(memvar->do_getoprn)
#define A_DRUKCOMP
//#define A_DOKCOMP 10
#define A_XPRN
#define A_15CPI
#define A_PCL
#define A_WA
#define A_WE
#define A_ZLEC11
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -40007923682
#define A_KOMU_N  'POWIATOWY DOM POMOCY SPOECZNEJ "Pogodna Jesieä"'
#define A_KOMU_A  "Cieszyn, ul. Korfantego 1"
#define A_AUTOR   "A.D. 1991-2015, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define DatE()    MEMVAR->dzisiaj
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3

#define A_LPNUM 3
#define A_ZAZNACZ
#define A_LFIRST
#define A_FILTERNAZ
#define A_IZ
//#define A_SHORTIND
#define INDEXPIC "#########-XX"
#define A_MINUS
#define A_VAT
#define A_NVAT
#define A_FA
#define A_BACKUP memvar->backup
#define A_KHNAZ 31
#define A_DOKFAK

#define ILEVAT(i,c,pv,df) (     pv*ROUND(round(1000*i,0)*round(100*c,0),-3)/10000000)
#define ILEVATGR(i,c,pv,df) (     pv*ROUND(round(1000*i,0)*round(100*c,0),-3)/100000)
#define WbezVAT(i,c,pv,df) W(i,c,pv,df)
#define WbezVATGR(i,c,pv,df) WGR(i,c,pv,df)
#define WzVAT(i,c,pv,df) ((100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3)/10000000)
#define WzVATGR(i,c,pv,df) ((100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3)/100000)
#define W(i,c,pv,df)     (ROUND(round(1000*i,0)*round(100*c,0),-3)/100000)
#define WGR(i,c,pv,df)     (ROUND(round(1000*i,0)*round(100*c,0),-3)/1000)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define cenA_zaK cena_przy
