#define PLWIN
#define PC852
#include "lan.ch"
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usług Informatycznych Marek Długosz, 43-400 Cieszyn, ul. Równa 16'
#define A_WIN_PRN .t.
#define DatE() memvar->dzisiaj
#define A_15CPI
#define A_BACKUP memvar->bejkap
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_PCL
#define A_KHSEP
#define A_MAGDI " "
#define A_MULTIDI "TM"
#define A_ONEPOS
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_STYLUS
#define A_XPRN
#define isPrinter (.t.)
#command init screen =>
//#define STANY indx_mat
#define A_FIFO
#define A_CEOKR
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_WE
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
//#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_SWW "PKWiU"
#define INDEXPIC "####XX"
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO
#define A_NOZAP
//#define A_SURLINK
#define A_SB
//#define A_SBSHOW
#define A_SUMK    -28112406324
#define A_KOMU_N  'Żłobki Miejskie'
#define A_KOMU_A  'Cieszyn, ul. Srebrna 2'
#define A_AUTOR   'A.D. 2002-2004, Marek Długosz, Cieszyn, ul. Równa 16, tel. 0-601842030'

//#define A_DF 10
#define A_DOKCOMP 11
#define A_IZ
#define A_CENVAT

#define A_PV
#define A_WEBRUT             //if(year(da)>2016.and.mag_biez$' 3 4',DatY->get_prop,1) *
#define A_VAT
#define A_FA

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)

#define WPZ(w)               (ROUND(100*w,0)/100)
#define VATPZ(w,pv)          (pv*ROUND(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*ROUND(100*w,0)/100)
