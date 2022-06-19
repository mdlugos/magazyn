#include "lan.ch"
#define PC852
#define PLWIN
#define A_ZLEC11
#define A_CEOKR
#define A_WIN_PRN .f.
#define A_CDX DBFCDX
#define A_ZATW
#define UpP(x) UPPER(x)
#define A_KHSEP
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define A_15CPI
#define A_STYLUS
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define isPrinter (.t.)
#command init screen =>
#define STANY indx_mat
#define A_CEOKR
#define A_FIFO
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_SWW "PKWiU:"
#define INDEXPIC "XXXXXX"
//#define A_LFIRST
#define A_MULTIDI memvar->multidi
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO
#define A_NOZAP
#define A_SB
#define A_SUMK    -43181607442
#define A_KOMU_N  'Sanatorium Uzdrowiskowe "MALWA"'
#define A_KOMU_A  "Ustroä, ul. Szpitalna 45"
#define A_AUTOR   "A.D. 2006, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_BACKUP MEMVAR->backup
#define DatE() MEMVAR->dzisiaj
#define A_VAT
#define A_FA
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
