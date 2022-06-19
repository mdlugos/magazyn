#include "lan.ch"
#command INIT SCREEN => //__run("uniznak.exe 8 r")
#define PC852
#define A_XPRN
#define A_MYSZ
#define UpP(x) UPPER(x)
#define A_STSIMPLE
#define A_PCL
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define isPrinter() (.t.)
#define A_BACKUP memvar->bejkap
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#ifdef __PLATFORM__WINDOWS
 #define PLWIN
 #define A_DFPDOS
 #define A_DFP 9600
 #define A_WIN_PRN .f.
// #define A_EXT WIN_PRINTERGETDEFAULT
#endif
#ifdef __PLATFORM__UNIX
  #define A_DFP 9600
//  #define A_DFPDOS
#endif
#define A_DF 10
#define A_WB //winno by†
#define A_KORALL
#define A_SPECYF
#define A_GRAM
#define A_JMTOT
#define A_ZAZNACZ
#define A_KONTROLA .t.
#define A_DOKCOMP 11
#define A_ZLEC11
#define A_KAMIX
#define A_KHNAZ 31
#define A_DOKFAK
#define A_F9
#define A_7
#define A_NOPV
#define A_NOMZ
#define A_WA
#define A_WE
#define A_FIFO
#define A_LIFO
#define DTOV(dat) right(dtoc(dat),5)
#define A_SET_DAT  ANSI
#define A_SWWKTM "Nr st.:"
#define A_SUMK -27765399104
#define A_KOMU_N  'P.P.H. "KAMIX"'
#define A_KOMU_A  "Tychy, ul. Przemysˆowa 60"
#define A_AUTOR   "A.D. 1995-2012, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_FILELIMIT '40'
#define A_FA
#define A_FAT
#define A_VAT
#define A_NVAT
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100+pv,100)*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100

#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)
#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define A_DRUKCOMP
#define A_FFULL
#define A_FDO
#define A_PROCMAR
#define cenA_zaK cena_przy
#define A_IZ
#define A_ALTCEN
#define A_NOZAP
