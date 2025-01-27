#define PC852 "PLWIN"

#ifndef __PLATFORM__DOS
  #define A_UNICODE 'UTF8EX'
  #define A_KSEF
  #define A_WL
#endif

#include "lan.ch"
//#define A_NETIO '192.168.10.111'
#ifdef __PLATFORM__UNIX
// #define A_TERMINAL
 #define A_DFP 19200
 #define A_EXT HB_TRANSLATE, DBFNTX, HB_COMMCR, HB_STRXOR, HB_CODEPAGE_PLWIN
 //,HB_INETINIT,HB_INETCLEANUP,HB_INETCREATE,HB_INETCLOSE,HB_INETFD,HB_INETSTATUS,HB_INETERRORCODE,HB_INETERRORDESC,HB_INETCLEARERROR,HB_INETCOUNT,HB_INETADDRESS,HB_INETPORT,HB_INETTIMEOUT,HB_INETCLEARTIMEOUT,HB_INETTIMELIMIT,HB_INETCLEARTIMELIMIT,HB_INETPERIODCALLBACK,HB_INETCLEARPERIODCALLBACK,HB_INETGETSNDBUFSIZE,HB_INETGETRCVBUFSIZE,HB_INETSETSNDBUFSIZE,HB_INETSETRCVBUFSIZE,HB_INETRECV,HB_INETRECVALL,HB_INETRECVLINE,HB_INETRECVENDBLOCK,HB_INETDATAREADY,HB_INETSEND,HB_INETSENDALL,HB_INETGETHOSTS,HB_INETGETALIAS,HB_INETIFINFO,HB_INETSERVER,HB_INETACCEPT,HB_INETCONNECT,HB_INETCONNECTIP,HB_INETISSOCKET,HB_INETCRLF
// #define A_ADS 3
#else
#ifdef __PLATFORM__WINDOWS
#ifndef SIMPLE
 #define A_EXT HB_CODEPAGE_PLWIN,STOD,HB_TRANSLATE,DBFNTX,HB_COMMCR,HB_STRXOR,mxmlNewXML,mxmlLoadFile,mxmlfindelement,mxmlGetFirstChild,mxmlgetText,mxmldelete,mxmlgetcData,mxmlsetwrapmargin,mxmlNewElement,mxmlElementSetAttr,mxmlNewText,mxmlSaveFile,mxmlGetElement,mxmlGetParent
#endif
 //,HB_INETINIT,HB_INETCLEANUP,HB_INETCREATE,HB_INETCLOSE,HB_INETFD,HB_INETSTATUS,HB_INETERRORCODE,HB_INETERRORDESC,HB_INETCLEARERROR,HB_INETCOUNT,HB_INETADDRESS,HB_INETPORT,HB_INETTIMEOUT,HB_INETCLEARTIMEOUT,HB_INETTIMELIMIT,HB_INETCLEARTIMELIMIT,HB_INETPERIODCALLBACK,HB_INETCLEARPERIODCALLBACK,HB_INETGETSNDBUFSIZE,HB_INETGETRCVBUFSIZE,HB_INETSETSNDBUFSIZE,HB_INETSETRCVBUFSIZE,HB_INETRECV,HB_INETRECVALL,HB_INETRECVLINE,HB_INETRECVENDBLOCK,HB_INETDATAREADY,HB_INETSEND,HB_INETSENDALL,HB_INETGETHOSTS,HB_INETGETALIAS,HB_INETIFINFO,HB_INETSERVER,HB_INETACCEPT,HB_INETCONNECT,HB_INETCONNECTIP,HB_INETISSOCKET,HB_INETCRLF
// #define PLWIN
 #define A_DFP 19200
 #define A_ZEBRA
 #define A_WIN_PRN eval(MEMVAR->do_getoprn)
 // #define A_ADS 1 
 // ^ bo i tak łąaczy sie z cen to moze lepiej ten sam silnik - nie idex corupted
#else
 #define A_EXT DBFNTX
#endif
#endif
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usług Informatycznych Marek Długosz, 43-400 Cieszyn, ul. Równa 16'
#define A_SB
#define A_ZLEC11
#define stanprzeD(a,b,c) (STANY->stan - if(a,0,MAIN->ilosc))
#define UpP(x) upper(x)
#define A_ZAGRODA
#define A_DATAVAT
//#define A_VIEWCZAK
#define A_DOKCOLOR if(kto_pisal=chr(255),"GB+/N+",if(kto_pisal>chr(1),_snorm,"R+/N+"))
#define A_CDX DBFCDX
//#define A_VIEWVAT
#define A_LPNUM 3
#define A_KHSEP
#define A_KHNAZ 37
#define A_DOKFAK 
#define A_KHKONTO
#define A_KHFILLZERO
#command init screen  => //__run("uniznak r 8")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_ZAZNACZ
#define A_TRWALOSC
#define A_GRAM
#define A_STYLUS
#define A_AUTOMAR
#define A_ENAZ
#define A_ANKER
#define A_KODY "Kod"
#define A_NOPV
#define A_XPRN
#define A_PCL
#define A_NAZWISKO
#define isPrinter() .t.
#define A_DRUKCOMP
#define A_IZ 9
#define A_KPR 10
#define A_FK 12
#define A_DF      13
#define A_DOKCOMP 14
#define A_F9
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_DLINK
#define A_FIFO
#define A_WA
#define A_WE
#define A_SET_DAT   GERMAN
//#define A_SWW
#define A_SWWKTM
#define A_PKWiU
//#define STANY     indx_mat
#define A_STSIMPLE
#ifdef A_UNICODE
#define A_SUMK    -36182628928
#else
#define A_SUMK    -38145335094
#endif
#define A_KOMU_N  'PHU "Zagroda" Władysław Aniela Szlaur'
#define A_KOMU_A  "Cieszyn, ul. gen. J. Hallera 116a"
#define A_AUTOR   "A.D. 1998-2010, Marek Długosz, Cieszyn, ul. Równa 16, tel. 338522553"
#define A_FILELIMIT '36'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_WB
#define A_FAT "Nazwisko:"
#define A_VAT
//#define iS_spec .t.
//#define hAslo_spec(x)
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST
#define A_FILTERNAZ
#define A_CENVAT
#define A_NVAT
#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,0),-3)/1000

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

#define cenA_zaK cena_przy
#define A_15CPI
#define A_MYSZ
#define A_DMDATA
//#define A_KPRwVAT
#define A_FDO
#define A_FFULL
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_ALTCEN
#define A_JMTOT
//#define A_JMALT
#define A_CF8
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_GETLPT  eval(memvar->do_getlpt)
#define A_BACKUP MEMVAR->backup
