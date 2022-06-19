#include "lan.ch"
#define A_SPECYF
#define PC852
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_LPNUM 3
#define A_KHSEP
#define A_NAZWISKO
#define A_F9
#define A_STYLUS
//#define A_NOREFRESH
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define isPrinter (.t.)
//#define A_PCL
#command init screen =>
#define STANY indx_mat
#define A_FIFO
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define cenA_zaK cena_przy
#define A_WB
#define A_KORALL
#define A_WA
#define A_WE
#define A_IZ
#define A_FAT
#define A_DF 10
#define A_DOKCOMP 11
//#define A_DFP
#define A_WTYM
#define A_JMTOT
//#define A_JMALTTOT(nz,s,x) (1/(s)->przel)
#define A_SET_DAT  GERMAN
#define A_SUMK    -32347757223
#define A_KOMU_N  'ZPHU "KABEX" S.C.'
#define A_KOMU_A  "42-253 Jan¢w, ul. ½urawska 41"
#define A_AUTOR   "A.D. 2001, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
//#define A_FK 10
#define A_DATA
#define A_KHNAZ 31
#define A_DOKFAK
#define A_VAT
#define A_DLINK
#define A_ZLEC11
#define INDEXPIC "XXXX-XXX-XXX"
#define A_7
#define A_LFIRST
#define A_SWWKTM
//#define A_SHORTIND
#define A_DRUKCOMP
#define A_NVAT
#define A_NOPV
#define A_NOZAP
#define A_INOWA
#define A_KOBSUR
#define A_BIECZ
//#define A_AUTOMAR
//#define A_PROCMAR

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,1),-3)/1000
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
//#define A_FP600
#define A_A
#define A_AF //anielskiego firmy
#define A_TPD
#define A_MINUS
