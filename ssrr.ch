#include "lan.ch"
#define PC852
 #define A_WIN_PRN .t.
 #define PLWIN
 #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define A_WINPRN
#define A_STOPKA
//#define A_CDX DBFCDX
//#define A_ZATW
#define UpP(x) UPPER(x)
#define A_KHSEP
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define isPrinter (.t.)
#define A_BACKUP memvar->backup
#command init screen =>
#define STANY indx_mat
#define A_JMALT
#define A_FIFO
#define A_CEOKR
//#define A_GRAM
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_WE
#define A_DMDATA
#define A_KHNUM
#define A_KHNAZ 31
#define A_DOKFAK
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_ZLEC11
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_SWW "PKWiU:"
#define INDEXPIC "XXXXXX"
//#define A_LFIRST
//#define A_MULTIDI memvar->multidi
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO
#define A_NOZAP
#define A_SB
#define A_SUMK    -47710382599
#define A_KOMU_N  '—l¥ski Szpital Reumatologiczno-Rehabilitacyjny'
#define A_KOMU_A  "Ustroä, ul. Szpitalna 11"
#define A_AUTOR   "A.D. 2010, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
//#define A_BACKUP MEMVAR->backup
/*
#define A_VAT
#define A_NVAT
//#define A_WEBRUT
//#define A_PZBRUT
#define A_PV
#define A_FA
#define A_DF 10
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
*/
