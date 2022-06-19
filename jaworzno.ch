#include "lan.ch"
#define UpP(x) UPPER(x)
#define PC852
#define PLWIN
#define A_WIN_PRN .t.
#define PLWIN
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#command INIT SCREEN => //__run("uniznak m r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_KONTROLA .t.
#define A_KODY "Dz"
#define A_KODVAL {||kw:=UpP(kw),.t.}
//#define A_STYLUS
#define A_15CALI
#define A_LPNUM 3
#define A_15CPI
//#define A_MSCPI
#define A_XPRN
#define A_PCL
#define A_FIFO
#define A_ZLEC11
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_DMDATA
#define A_KHNUM
#define A_KHNAZ   32
#define A_DOKFAK
#define A_SUMK    -45598752484
#define A_KOMU_N  "Dom Pomocy Spoˆecznej w Jaworznie"
#define A_KOMU_A  "Jaworzno, ul. Obroäc¢w Poczty Gdaäskiej 63"
#define A_AUTOR   "A.D. 2000-2017, Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16"
//#define A_SHORTIND
#define A_SWW "PKWiU"
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "######"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
//#define A_MULTIDI
#define A_SB
//#define A_JMALTTOT(a,sel,x) (sel)->przel
#define A_BACKUP memvar->backup
#define A_VAT
#define A_NVAT
//#define A_WEBRUT
#define cenA_zaK cena_przy
#define A_IZ
#define A_WE
#define A_PV
#define A_FA
#define A_DF 10
#define A_CENVAT
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
