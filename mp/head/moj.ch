#ifndef __PLATFORM__DOS
  #define A_KSEF
  //#define A_UNICODE 'UTF8MD'
  #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usług Informatycznych Marek Długosz, 43-400 Cieszyn, ul. Równa 16'
#endif

//#include "lan.ch"
#include "mdstd.ch"

#ifdef __PLATFORM__UNIX
//  #define A_EXT cupsPrintFile,cupsGetDefault
#endif
#ifdef __PLATFORM__WINDOWS
//    #define PLWIN
  #define A_WIN_PRN eval(memvar->do_getoprn)
  #define A_ZEBRA
#endif
#define A_ADS 1
//#define A_CDX ADSCDX

//#define A_ADS 1
//#define UpP(x) uPp(x,A_UNICODE)

//upper z UTF8MD już gotowe, podobnie z PL852M
#define UpP(x) UPPER(x)
#define PC852 'PL852M'
//#define A_CDX VFPCDX
//#define A_HBGET
//#include "md_add.ch"
#define A_LPNUM 3
#define A_LPKPR
#define A_KHSEP
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_GETLPT eval(memvar->do_getlpt)
//#define A_NOREFRESH
#command init screen  =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_WL
#define A_DLINK
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
//#define A_DF 12
//#define A_TP //TP-LINE
#define A_DRUKCOMP
#define A_WTYM
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
//#define A_LIFO
//#define A_FIFO
#define A_WA
#define A_SET_DAT   GERMAN
#define A_SHORTIND
#define STANY     indx_mat
#ifdef A_UNICODE
#define A_SUMK    -28441314785
#else
#define A_SUMK    -32508120377
#endif
#define A_KOMU_N  "Firma Usług Informatycznych Marek Długosz"
#define A_KOMU_A  "Cieszyn, ul. Równa 16"
#define A_AUTOR   "A.D. 1991-2024, Marek Długosz, Cieszyn, ul. Równa 16, tel. 338522553"
#define A_FILELIMIT '36'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_FA
#define A_VAT
#define A_MINUS
#define iS_spec .t.
#define hAslo_spec(x)
#define INDEXPIC "####"
//#define A_SWWKTM "CPV"
#define A_NVAT
#define WbezVATGR(i,c,pv,df) ROUND(round(1000*i,0)*ROUND(100*c,0),-3)/1000
#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   WbezVATGR(i,c,pv,df)/100
#define ILEVATGR(i,c,pv,df)  WbezVATGR(i,c,pv,df)*pv/100
#define WzVATGR(i,c,pv,df)   WbezVATGR(i,c,pv,df)*(100+pv)/100
#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)

#define cenA_zaK cena_przy
#define A_15CPI
#define A_MYSZ
#define A_DMDATA
#define A_KPR 10
//#define A_DF      12
#define A_DOKCOMP 12
//#define A_LPTN 13
#define A_KHNAZ 37
#define A_IZ
#define A_FDO
#define A_FFULL
//#define A_AF
#define A_REGON
#define DatE() MEMVAR->dzisiaj
