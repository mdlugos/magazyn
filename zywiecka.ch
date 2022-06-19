#include "lan.ch"
#command INIT SCREEN => //__run("font852")
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define cenA cena_przy
#define A_XPRN
#define A_DOKCOMP 10
#define A_DRUKCOMP
#define A_FIFO
#define UpP(x) UPPER(x)
#define PC852
#define A_BACKUP memvar->backup
#ifdef __HARBOUR__
    #define A_PCL
    #define A_15CPI
    #define A_PRINT(x) eval(memvar->do_print,x)
    #define A_GETLPT eval(memvar->do_getlpt)
    #define PLWIN
    #define A_WIN_PRN memvar->p_win
    #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#define isPrinter .t.
#define DTOV(dat) left(dtoc(dat),5)
#define A_WA
#define A_SB
#define A_SWWKTM "CPV"
#define A_DLINK
#define A_SET_DAT GERMAN
#define A_SHORTIND
#define STANY INDX_MAT
#define A_SUMK    -42315376231
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Os¢b Starszych"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. ½ywiecka 15"
#define A_AUTOR   "AD 1991-2016, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
//#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_ZLEC11
//#define A_MULTIDI "WM"
#define A_WADO eval(memvar->podpis)
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
