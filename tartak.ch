#include "lan.ch"
#command init screen => __run("font852.com")
/*
#command INIT PRINTER => qqout("@")
#define A_MSCPI
*/
#define A_PCL
#command INIT PRINTER => qqout("(17U(s1q4099t0p0s0b12v10H")
*
#define STANY INDX_MAT
#define A_CENFIX
#define A_15CALI
#define A_STYLUS
#define isPrinter .T.
#define DTOV(dat) tran(subs(dtos(dat),5),"@R XX.XX")
#define A_WA
#define A_WE
#define A_KHSEP
#define A_DRUKCOMP 7
#define A_DOKCOMP  11
#define A_SUBDOK
#define A_DLINK
#define A_SET_DAT   GERMAN
#define A_SUMK     -42775790866
#define A_KOMU_N  "Tartak i Stolarnia Kobi¢r"
#define A_KOMU_A  "Kobi¢r, ul. Centralna 56"
#define A_AUTOR   "A.D. 1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '41'
#define A_NAZBEG 7
#define A_NRLTH 5
//#define INDEXPIC "XXX#-###-###"
#define INDEXPIC "XXXX-XXX-XXX"
#define A_SWWKTM
#define A_NOZAP
#define A_INOWA
#define A_KOBSUR
#define A_FFULL
//#define A_NOPV
#define A_KOB
#define A_FA
#define A_FAT
#define A_A
#define A_AF
#define A_VAT
#define A_REGON
#define A_IZ
#define A_ZLEC11
#define A_LFIRST
#define A_JMTOT
//#define A_JMALTTOT(nz,lam,x) (lam)->przel
#define A_DF 10
#define A_DFP
#define A_NVAT
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
#define cenA_zaK cena_przy
#define A_FDO
