#include "lan.ch"
#command INIT SCREEN => //__run("font852")
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_SB
#define UpP(x) UPPER(x)
#define A_MULTIDI 'WM'
//#define A_MINUS
#define A_XPRN
#define A_DRUKCOMP
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define PC852
#define A_PCL
#define A_BACKUP eval(memvar->do_backup)
#ifdef __PLATFORM__WINDOWS
//#define A_EXT HB_CODEPAGE_PLMAZ,HB_TRANSLATE
  #define PLWIN
  #define A_WIN_PRN eval(memvar->do_getoprn)
  #define A_GETLPT eval(memvar->do_getlpt)
  #define A_PRINT(x) eval(memvar->do_print,x)
#endif
#define A_STYLUS
#define A_17CPI
#define DatE() M->dzisiaj
#define A_DLINK
#define A_WA
#define A_FIFO
#define A_SET_DAT GERMAN
#define A_SHORTIND
#define STANY INDX_MAT
#define A_SUMK    -41781008022
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla przewlekle chorych"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Olimpijska 11"
#define A_AUTOR   "A.D. 1992-2006, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_SWWKTM "Rodzaj"
#define A_KOMOR
#define A_MYSZ
#define A_KHNAZ 31
#define A_DOKFAK

//#define A_NOZAP

#define A_VAT
#define A_NVAT
#define A_FA
#define A_IZ
#define A_WE
#define A_WEBRUT
#define A_CENVAT

#define WGR(i,c,pv,df)       Round((Round(i*1000,0)*Round(c*100,0)),-3)/1000

#define ILEVATGR(i,c,pv,df)  WGR(i,c,pv,df)*pv/(100+pv)
#define WzVATGR(i,c,pv,df)   WGR(i,c,pv,df)
#define WbezVATGR(i,c,pv,df) WGR(i,c,pv,df)*100/(100+pv)

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     WzVATGR(i,c,pv,df)/100
#define WbezVAT(i,c,pv,df)   ROUND(WbezVATGR(i,c,pv,df),0)/100

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)

#define WPZ(w)               (ROUND(100*w,0)/100)
#define VATPZ(w,pv)          (pv*ROUND(100*w,0)/10000)
#define VATPZGR(w,pv)        (pv*ROUND(100*w,0)/100)
