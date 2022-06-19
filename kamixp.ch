#include "std.ch"
#command INIT SCREEN => __run("uniznak.exe 8 r")
#define PC852
*
#define A_PCL
#command INIT PRINTER => qqout("(17U&l26a0O(s1q4099t0p0s0b12v10H")
//#command INIT PRINTER => qqout("(s1q12v10H")
/*
#command INIT PRINTER => qqout("@P")
#define A_15CALI
#define A_STYLUS
*/
#define A_DFPDOS
#define isPrinter() (.t.)
#define A_DF 10
#define A_DFP
//#define A_POSNET
//#define STANY indx_mat
#define A_KONTROLA
#define A_DOKCOMP 11
#define A_ZLEC11
//#define A_KASA
#define A_7
#define A_NOPV
#define A_NOMZ
#define A_WA
#define A_FIFO
//#define A_LIFO
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT  GERMAN
#define A_SWWKTM
#define A_SUMK -32529573662
#define A_KOMU_N  'P.P.H. "KAMIX"'
#define A_KOMU_A  "Tychy, ul. Przemysˆowa 60"
#define A_AUTOR   "A.D. 1995, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '40'
#define A_FA
#define A_FAT
#define A_VAT
#define A_NVAT
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST
#define A_CENVAT

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(c*if(df,100,10000/(100+pv)),0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100

#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)
#define W(i,c,pv,df)         WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define A_DRUKCOMP
#define A_FFULL
#define A_FDO
//#define A_AUTOMAR
#define A_PROCMAR
#define cenA_zaK cena_przy
#define A_IZ
#define A_ALTCEN
#define A_NOZAP
