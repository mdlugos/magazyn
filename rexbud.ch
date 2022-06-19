#define PC852
#include "lan.ch"
//#define A_CDX DBFCDX
//#define A_NOMZ
#define isPrinter .t.
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define UpP(x) UPPER(x)
#command init screen =>  //__run("font.com")
#define A_XPRN
//#define A_STYLUS
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
//#define A_15CALI
//#define STANY indx_mat
#define A_A
#define A_DLINK
#define A_FIFO
//#define A_LIFO
#define A_SWWKTM "PKWiU:"
//#define A_SUBDOK
#define DTOV(dat) transform(subs(dtos(dat),5),"@R ##.##")
#define DatE() MEMVAR->dzisiaj
#define A_WA
#define A_F9
#define A_MM
#define A_KHNAZ 31
#define A_KHSEP
//#define A_NOMZ
#define A_ZLEC11
#define A_JMALTTOT(a,b,c,d) (a*(c)->przel)
#define A_IZ
#define A_SET_DAT GERMAN
#define A_SUMK    -27087571773
#define A_KOMU_N  'REX BUD Tartak B¥k¢w'
#define A_KOMU_A  "B¥k¢w, ul. Tartaczna 12"
#define A_AUTOR   "A.D. 1991-2007, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_FILELIMIT '45'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_LPNUM 3
//#define A_CENSPEC
#define A_LFIRST
#define INDEXPIC "XXXXX-XX-XXX"
#define A_FILTERNAZ
#define A_JMTOT
#define A_FA
#define A_VAT
#define A_FAT
#define A_NVAT
//#define A_NOPV
#define cenA_zaK cena_przy
#define A_DOKCOMP 11
#define A_DF 10
//#define A_DFP
#define A_WP
//#define A_POSNET

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)

#define WPZ(w)               ROUND(w,A_ZAOKR)
#define WPZGR(w)             ROUND(w*100,0)
#define VATPZ(w,pv)          (pv*round(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*round(100*w,0)/100)
#define A_DRUKCOMP
#define A_FDO
#define A_FFULL
#define A_NOZAP
#define A_BIECZ
#define A_SPECYF
#define A_KOBSUR kh+LEFT(indx_mat->index,6)+subs(nz,3,4)
//#define A_AF
#define A_PLUDRY
