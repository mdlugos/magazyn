#include "lan.ch"
#define A_CDX DBFCDX
#define PC852
#define UpP(x) UPPER(x)
#command init screen => //__RUN("FONT852.COM")
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad)) //"(17U(s1q4099t0p0s0b12v10H")
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define isPrinter (.t.)
#define A_PCL
#define A_XPRN
#define A_KHNAZ 31
#define A_DRUKCOMP
#define A_DF 10
//#define A_DFP
#define A_WP
#define A_DOKCOMP 11
#define STANY indx_mat
#define A_SWWKTM
#define A_ZLEC11
#define A_NOMZ
#define A_DMDATA
#define A_DLINK
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define cenA_zaK cena_przy
#define A_WA
#define A_SET_DAT GERMAN
#define A_SUMK    -44372171308
#define A_KOMU_N  'ZAKADY DRZEWNE "TiS" S.C. Zakˆad Nr 2'
#define A_KOMU_A  "43-100 Tychy, ul. Murarska 19"
#define A_AUTOR   "A.D. 1995, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_VAT
#define A_FAT
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST
#define A_JMTOT
#define A_JMALTTOT(il,nz,lam,x) (il*eval(memvar->jmalttot,nz,(lam)->info,x))
//(x:=asize(getlines(if((lam)->info="NTP",nz,(lam)->info),";"),2),if(x[2]=NIL,0,val(x[1])*val(x[2])))
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
#define A_KOB
#define A_FDO
#define A_NVAT
#define A_NOPV
//#define A_MYSZ
#define A_NOZAP
#define A_FFULL
#define A_IZ
