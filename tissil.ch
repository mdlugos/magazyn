#define PC852
//#define A_CDX DBFCDX
//#command INIT PRINTER => qqout("(17U(s1q4099t0p0s0b12v10H")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#ifdef __HARBOUR__
  #define A_DFP
  #define A_DFPDOS
  #define A_EXT rs_init //, rs_done, rs_send, rs_get
#else
  //#define A_DFP
  #define UpP(x) UPPER(x)
#endif
#include "lan.ch"
#define A_XPRN
#define A_KHNUM
#define A_KHNAZ 31
#define A_DOKFAK
#define A_ZAZNACZ
//#define A_KHSEP
#define A_NAZWISKO
#define A_KHKONTO
#define A_LPNUM 3
#define A_JMTOT
#define A_JMALTTOT(a,b,c,d) eval(MEMVAR->jmalttot,a,b,c,d)
#define DatE() MEMVAR->dzisiaj
#define isPrinter (.t.)
#define A_PCL
#command init screen => //__run('uniznak r')
#define STANY indx_mat
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define cenA_zaK cena_przy
#define A_WA
#define A_WE
#define A_IZ
#define A_FAT
#define A_FK 10
#define A_DF 11
#define A_DOKCOMP 12
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
//#define A_WTYM
#define A_SET_DAT  GERMAN
#define A_SUMK    -39937302075
#define A_KOMU_N  'ZAKùADY DRZEWNE "TiS" Antoni Szwed S.J.'
#define A_KOMU_A  "43-210 Kobi¢r, ul. Kobi¢rska 2"
#define A_AUTOR   "A.D. 2006, Marek Dàugosz, Cieszyn, ul. R¢wna 16, tel. 0-338522553"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_VAT
//#define A_REGON
#define A_DLINK
#define INDEXPIC "XXXXXXXXXXXX"
#define A_SWWKTM
#define A_DRUKCOMP
#define A_NVAT
#define A_NOPV
#define A_NOZAP
//#define A_CENVAT
#define A_AUTOMAR

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)
#define WPZ(w)        ROUND(w,A_ZAOKR)
#define VATPZ(w,pv)   (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define A_FFULL
#define A_FDO
//#define A_FP600
#define A_MYSZ
#define A_ZLEC11
#define A_NOMZ
#define A_ZERUJSTAN
#define A_KOB // przepisuj cene na FV
