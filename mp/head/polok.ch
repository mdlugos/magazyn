#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_WIN_PRN eval(memvar->do_getoprn)
  #define A_ZEBRA
  #define A_EXT hb_hextonum, hb_numtohex, curdir
#endif
//#define A_UNICODE 'UTF8EX'
//#define A_KSEF
//#define A_REGON
//#define A_TPD
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usług Informatycznych Marek Długosz, 43-400 Cieszyn, ul. Równa 16'
#define A_BACKUP MEMVAR->backup
#define A_VIEWCZAK
//#define A_VIEWVAT
#define A_WL
#define UpP(x) UPPER(x)
#define PC852 "PL852M"
#include "lan.ch"
#define A_DDBF
//#define A_NOREFRESH
#define A_JMALTTOT(il,nz,lam,x) ((il)*(lam)->przel)
#define A_ALTCEN
#define A_KHFILLZERO
//#define A_KHKONTO
//#define A_KHNAZ 23
#define A_KHNAZ 37
#define A_DOKFAK 
#define A_LPNUM 4
#define isPrinter() .t.
#define A_GETLPT   eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#command INIT SCREEN  => //__run("852.exe r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define A_STYLUS
#define A_DRUKCOMP
#define A_DOKCOMP 14
#define A_DLINK
#define A_ZATW
#define A_NAZWISKO
#define A_SET_DAT GERMAN
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define DatE() MEMVAR->dzisiaj
#define A_FK 12
#define A_F9
#define A_A
#define A_WA
#define A_WE
#define A_SWWKTM
#define A_PKWiU
#define STANY indx_mat
#ifdef A_UNICODE
#define A_SUMK   -30134572416
#else
#define A_SUMK   -36559898897
#endif
#define A_KOMU_N "FH AMIPOL Sp.J. Polok & Polok"
#define A_KOMU_A "Cieszyn, ul. Filasiewicza 3"
#define A_AUTOR  "A.D. 1995-2024, Marek Długosz, Cieszyn, ul. Równa 16, tel. 338522553"
#define A_FILELIMIT '41'
#define A_NAZBEG 7 //5
#define A_NRLTH 5 //3
//#define A_NRLTH3
#define A_FA
#define A_WB
//#define A_KORALL
#define A_FAT
#define A_VAT
#define INDEXPIC "XXXXXXXXXXXX"
#define A_NOZAP
#define A_LFIRST
#define A_CENVAT
#define A_NVAT
*
#define A_DF 13
//#define A_DFP
//#define A_POSNET
#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,2),-3)/1000

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
#define A_AUTOMAR
#define A_DMDATA  //index wg daty
#define A_KPR 10
#define A_IZ
#define A_WTYM
#define A_FIFO
#define A_MYSZ
#define A_FFULL
#define A_FDO
#define A_ANKER
#define A_KODY "Kod"
#define A_ENAZ
#define A_MINUS
#define A_ZAZNACZ
#define A_TRWALOSC
