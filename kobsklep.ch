#include "std.ch"
#define PC852
#command init screen => __run('uniznak r')
#command INIT PRINTER => qqout(memvar->p_init)
#define A_XPRN
#define A_WB
/*
#command INIT PRINTER =>                                 ;
  x:=SET(17,.f.)                                         ;
; if wasbad .and. isprinter()                            ;
;   QQOUT(MEMOREAD("star.852"))                         ;
;   wasbad:=.f.                                          ;
; end                                                    ;
; qqout("@P%1")                                      ;
; set(17,x)
*/
#define STANY indx_mat
#define DTOV(dat) left(dtoc(dat),5)
//#define cenA_zaK cena_przy
#define A_WA
#define A_IZ
#define A_FAT
#define A_DF 10
#define A_DOKCOMP 11
#define A_DFP
#define A_WTYM
#define A_SET_DAT  GERMAN
#define A_SUMK    -42288790735
#define A_KOMU_N  "ZDPW Tartak i Stolarnia Kobi¢r"
#define A_KOMU_A  "43-210 Kobi¢r, ul. Centralna 56"
#define A_AUTOR   "A.D. 1995, Marek Dlugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '35'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_VAT
#define A_REGON
#define A_DLINK
#define INDEXPIC "XXXX-XXX-XXX"
#define A_SWWKTM
#define A_DRUKCOMP
#define A_NVAT
//#define A_NOPV
#define A_NOZAP
#define A_CENVAT
#define A_AUTOMAR

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)
/*
#define ILEVAT(i,c,pv,df)  if(df, pv/(100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3)/100000,      pv*ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3)/10000000)
#define ILEVATGR(i,c,pv,df)  if(df, pv/(100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3)/1000,      pv*ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3)/100000)
#define WbezVAT(i,c,pv,df) if(df,100/(100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3),                ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3))/100000
#define WbezVATGR(i,c,pv,df) if(df,100/(100+pv)*ROUND(round(1000*i,0)*round(100*c,0),-3),                ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3))/1000
#define WzVAT(i,c,pv,df)   if(df,             ROUND(round(1000*i,0)*round(100*c,0),-3)/100000,(100+pv)*ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3)/10000000)
#define WzVATGR(i,c,pv,df)   if(df,             ROUND(round(1000*i,0)*round(100*c,0),-3)/1000,(100+pv)*ROUND(round(1000*i,0)*ROUND(c*10000/(100+pv),0),-3)/100000)
#define W(i,c,pv,df)       WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)
*/
#define WPZ(w)        ROUND(w,A_ZAOKR)
#define VATPZ(w,pv)   (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define A_FFULL
#define A_FDO
#define A_FP600
