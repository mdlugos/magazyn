#ifdef __PLATFORM__WINDOWS
    #define PLWIN
    #define A_WIN_PRN memvar->p_win
    #define A_15CPI
    #define A_PRINT(x) eval(memvar->do_print,x)
    #define A_GETLPT eval(memvar->do_getlpt)
    #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#include "lan.ch"
#define PC852
#define UpP(x) UPPER(x)
#define A_KHSEP
#define A_KHNAZ 31
#define A_DOKFAK
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define isPrinter (.t.)
#define DatE() MEMVAR->dzisiaj
#command init screen =>
#define STANY indx_mat
#define A_CENFIX
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_SWW "PKWiU"
#define INDEXPIC "XXXXXX"
#define A_ZLEC11
#define A_LFIRST
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO 'Sporz¥dziˆ: '+memvar->operator
#define A_NOZAP
#define A_SB
#define A_SUMK    -26017207231
#define A_KOMU_N  'Dom Pomocy Spoˆecznej w Pˆazie'
#define A_KOMU_A  'Pˆaza, ul. Wiosny Lud¢w 4'
#define A_AUTOR   'A.D. 2002-2017, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553'
#define A_BACKUP MEMVAR->backup
#define A_VAT
#define A_NVAT
#define A_FA
#define A_WE
#define A_WEBRUT
//#define A_CENVAT

#define WGR(i,c,pv,df)       Round((Round(i*1000,0)*Round(c*100,0)),-3)/1000

#define ILEVATGR(i,c,pv,df)  WGR(i,c,pv,df)*pv/100
#define WzVATGR(i,c,pv,df)   WGR(i,c,pv,df)*(100+pv)/100
#define WbezVATGR(i,c,pv,df) WGR(i,c,pv,df)

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     WzVATGR(i,c,pv,df)/100
#define WbezVAT(i,c,pv,df)   ROUND(WbezVATGR(i,c,pv,df),0)/100

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)

#define WPZ(w)               (ROUND(100*w,0)/100)
#define VATPZ(w,pv)          (pv*ROUND(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*ROUND(100*w,0)/100)

