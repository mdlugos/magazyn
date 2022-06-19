#include "lan.ch"
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define A_MULTIDI 'PS'
#define A_MAGDI '10'
#define PC852
#define PLWIN
#define UpP(x) UPPER(x)
#define A_CDX DBFCDX
#define A_KHNAZ 31
#define A_DOKFAK
#define A_KHSEP
#define A_ZLEC11
#command INIT SCREEN => //run('uniznak r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_BACKUP MEMVAR->do_backup
//#define A_STYLUS
#define A_15CPI
#define A_XPRN
#define A_PCL
#define A_WIN_PRN eval(MEMVAR->do_getoprn)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
//#define cenA_zaK cena_przy
#define A_WA
#define A_WE
#define A_FIFO
#define A_NOZAP
#define A_SUMK    -54629700500
#define A_KOMU_N  'Gminne Przedszkole Publiczne w Zebrzydowicach o/Marklowice G¢rne'
#define A_KOMU_A  'Marklowice G¢rne, ul. Szkolna 28'
#define A_AUTOR   "A.D. 2021, Marek Dˆugosz, Cieszyn ul. R¢wna 16, tel. 601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_DRUKCOMP
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA  .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_LFIRST
#define INDEXPIC "XXXX"
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO eval(memvar->podpisy)
#define A_SB
#define A_VAT
#define A_NVAT
#define A_FA
//#define A_IZ
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
