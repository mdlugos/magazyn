#include "lan.ch"
*
#define isPrinter .t.
#command INIT PRINTER => qqout("@P%1")
#command init screen =>
//#define A_15CALI
/*
#command init screen =>
#command INIT PRINTER =>       ;
  x:=SET(17,.f.)               ;
; if wasbad .and. isprinter()  ;
;   QQOUT(MEMOREAD("star.852"));
;   wasbad:=.f.                ;
; end                          ;
; qqout("@P%1")            ;
; set(17,x)
*/
#define A_A
#define A_DMDATA
#define A_WB
//#define A_SZYM
#define A_IZ
#define A_F9
#define A_ZLEC11
#define A_NOMZ
#define STANY indx_mat
#define A_SWWKTM
#define A_SUBDOK
#define DTOV(dat) SUBS(dtoc(dat),6)
#define cenA_zaK cena_przy
#define A_WA
#define A_SET_DAT   ANSI
#define A_SUMK    -42288790736
#define A_KOMU_N  "ZDPW Tartak i Stolarnia Kobi¢r"
#define A_KOMU_A  "43-210 Kobi¢r, ul. Centralna 56"
#define A_AUTOR   "A.D. 1996, Marek Dlugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '35'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_VAT
#define A_REGON
#define A_FAT
#define A_MINUS // tymczasowo
//#define A_ZERUJSTAN
#define INDEXPIC "XXXX-XXXXX-X"
#define A_LFIRST
#define A_JMTOT
//#define A_JMALTTOT(nz,lam,x) if((lam)->info="NTP",val(nz),(lam)->przel)
#define A_JMALTTOT(nz,lam,x) (x:=asize(getlines(if((lam)->info="NTP",nz,(lam)->info),";"),2),if(x[2]=NIL,x[2]:='1',),round(val(x[1])*val(x[2]),2))
#define A_DF 10
#define A_DFP
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
#define VATPZ(w,pv)          (pv*round(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*round(100*w,0)/100)
#define A_DRUKCOMP
#define A_WTYM
#define A_KOB
#define A_FDO
#define A_NVAT
#define A_NOPV
#define A_NOZAP
#define A_FFULL
#define A_AF //anielskiego firmy
#define A_DOKCOMP 11
#define A_WP //wart_par
