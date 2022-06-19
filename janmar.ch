#include "std.ch"
#define UpP(x) UPPER(x)
//#define A_LPNUM 4
//#define A_KHSEP
#define A_KHNAZ 23
#define PC852
#command init screen  => //__run("uniznak r 8")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
//#define A_ZAZNACZ
#define A_STYLUS
#define A_AUTOMAR
#define A_NOPV
#define A_XPRN
//#define A_PCL
#define isPrinter() .t.
#define A_DRUKCOMP
//#define A_DF      13
#define A_DOKCOMP 13
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_DLINK
#define A_FIFO
#define A_WA
#define A_SET_DAT   GERMAN
#define A_SWWKTM 'PKWiU'
#define A_7
//#define A_PKWiU
//#define A_SWWKTM
#define STANY     indx_mat
#define A_SUMK    -25314008602
#define A_KOMU_N  'FH JANMAR'
#define A_KOMU_A  "Cieszyn, ul. Polna 1/20"
#define A_AUTOR   "A.D. 1993-2004, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0-601842030"
#define A_FILELIMIT '40'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_FA
#define A_FAT
//#define A_NAZWISKO
#define A_VAT
#define INDEXPIC "XXXXXXX"
#define A_LFIRST
//#define A_SWWKTM
//#define A_CENVAT
#define A_NVAT


#define WbezVATGR(i,c,pv,df) ROUND(round(1000*i,0)*ROUND(100*c,0),-3)/1000
#define ILEVATGR(i,c,pv,df)  WbezVATGR(i,c,pv,df)*pv/100
#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVATGR(i,c,pv,df)   ROUND(WbezVATGR(i,c,pv,df),0)*(100+pv)/100
#define WzVAT(i,c,pv,df)     ROUND(WbezVATGR(i,c,pv,df),0)*(100+pv)/10000
#define WbezVAT(i,c,pv,df)   WbezVATGR(i,c,pv,df)/100

#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)

#define cenA_zaK cena_przy
#define A_15CPI
#define A_MYSZ
#define A_DMDATA
#define A_KPR 10
#define A_KPRwVAT
#define A_IZ 9
#define A_FDO
#define A_FFULL
#define A_FK 12
//#define A_AF
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_ALTCEN
//#define A_JMTOT
//#define A_CF8
