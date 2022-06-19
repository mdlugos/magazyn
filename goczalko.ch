#include "lan.ch"
#define PC852
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_KHNAZ 31
#define A_KODY "Nr kodowy"
#define A_PV
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
//#define A_PRINT(x) __run('start /m/w dosprint '+x); ferase(x)
//#define A_PZBRUT
#define A_WEBRUT
//#define cenA_zaK cena_przy
#define A_DF 10
#define A_DOKCOMP 11
#define A_IZ
#define A_FA
#define A_ODDO
#define A_A
#define A_FAT
#define A_NAZWISKO
#define A_VAT
#define A_NVAT
/*
#define A_CENVAT

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)
*/
#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100

#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)
#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)

#define WPZ(w)               (ROUND(100*w,0)/100)
#define VATPZ(w,pv)          (pv*ROUND(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*ROUND(100*w,0)/100)

#command INIT SCREEN => __run("uniznak 8 r")
//#command INIT PRINTER => qqout(if(MEMVAR->p_pcl,"(17U&l26a0O&a0L(s1q4099t0s0b0p10h12V",'@!0x0'))
//#command INIT PRINTER => qqout(if(MEMVAR->p_pcl,"&a0L(s1q0s0b0p10h12V",'@'))
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_DRUKCOMP
#define A_STYLUS
//#define A_15CALI
//#define A_15CPI
//#define A_MSCPI
#define A_XPRN
#define A_PCL
//#define A_FIFO
#define A_CENFIX
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
//#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_WE
#define A_SUMK  -41656365372
#define A_KOMU_N  "W.O.R.R. Sanat. Rehab. Goczaˆkowice Zdr¢j"
#define A_KOMU_A  "Goczaˆkowice, ul. Uzdrowiskowa 54"
#define A_AUTOR   "A.D. 1992-1999, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
//#define A_SHORTIND
//#define A_SWW
#define A_7
#define A_SWWKTM "Przetarg/PKWiU"
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_LPNUM 3
#define INDEXPIC "######X"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_MULTIDI memvar->multidi
#define A_GOCZ
#define A_KHKONTO
#define A_SB
#define A_JMALT
