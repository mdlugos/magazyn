#include "lan.ch"
#command INIT SCREEN =>
#define PC852
*
#define A_PCL
#define A_XPRN
#define A_DRUKCOMP
#define A_NAZWISKO
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define isPrinter() (.t.)
#define A_DF 10
#define A_DFP
#define A_WP
#define A_WB //winno by†
//#define A_KASA
#define A_KONTROLA
#define A_DOKCOMP 11
#define STANY indx_mat
#define A_ZLEC11
#define A_7
#define A_KKR
#define A_NOPV
#define A_NOMZ
#define A_WA
#define A_FIFO
#define A_CENFIX
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT  GERMAN
#define A_SWWKTM
#define A_SUMK    -33568879599
#define A_KOMU_N  'P.P.H. "KAMIX"'
#define A_KOMU_A  "Tychy, ul. Przemysˆowa 60"
#define A_AUTOR   "A.D. 1995-2000, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_FILELIMIT '45'
#define A_FA
#define A_FAT
#define A_VAT
#define A_NVAT
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,0),-3)/1000
#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)
#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)
#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)

#define A_FFULL
#define A_FDO
#define A_AUTOMAR
#define A_PROCMAR
#define cenA_zaK cena_przy
#define A_IZ
//#define A_ALTCEN
#define A_NOZAP
//#define A_JMALTTOT(nz,lam,x) (lam)->przel
