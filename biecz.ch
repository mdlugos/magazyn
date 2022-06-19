#include "lan.ch"
#define isPrinter .t.
#command INIT PRINTER => qqout(MEMVAR->p_init)
#command init screen =>  __run("font.com")
#define A_XPRN
#define A_15CALI
#define STANY indx_mat
#define A_A
#define A_DLINK
#define A_FIFO
#define A_LIFO
#define A_SWW
#define A_SUBDOK
#define DTOV(dat) left(dtoc(dat),5)
#define A_WA
#define A_IZ
#define A_SET_DAT GERMAN
#define A_SUMK    -33404644437
#define A_KOMU_N  'PPH "PRODREW" sp. z o.o.'
#define A_KOMU_A  "Biecz, ul. —wierczewskiego 21"
#define A_AUTOR   "A.D. 1991-1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_LFIRST
#define INDEXPIC "XXXXXX"
#define A_JMTOT
#define A_FA
#define A_VAT
#define A_FAT
#define A_NVAT
//#define A_NOPV
#define cenA_zaK cena_przy
#define A_DF 10
#define A_DFP
#define A_POSNET

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
//#define A_FDO
#define A_FFULL
#define A_NOZAP
#define A_BIECZ
#define A_SPECYF
#define A_KOBSUR
#define A_AF
