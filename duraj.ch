#include "lan.ch"
 #define A_EXT HB_TRANSLATE,DBFNTX,HB_COMMCR,HB_STRXOR,mxmlNewXML,mxmlLoadFile,mxmlfindelement,mxmlGetFirstChild,mxmlgetText,mxmldelete,mxmlgetcData,mxmlsetwrapmargin,mxmlNewElement,mxmlElementSetAttr,mxmlNewText,mxmlSaveFile,mxmlGetElement,mxmlGetParent
 #define PLWIN
 #define A_DFP 9600
 #define A_ZEBRA
 #define A_WIN_PRN .f.
#define A_SB
#define UpP(x) UPPER(x)
#define A_ZAGRODA
#define A_DOKCOLOR
#define A_CDX DBFCDX
#define A_LPNUM 3
#define A_KHSEP
#define A_KHNAZ 37
#define A_DOKFAK 
#define A_KHKONTO
#define A_KHFILLZERO
#define PC852
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
#define A_FILELIMIT '36'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_WB
#define A_FAT "Nazwisko:"
#define A_VAT
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
#define A_FDO
#define A_FFULL
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_ALTCEN
#define A_JMTOT
#define A_CF8
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_GETLPT  eval(memvar->do_getlpt)
#define A_BACKUP MEMVAR->backup
#define STANY     indx_mat
#define A_SUMK    -34715636370
#define A_KOMU_N  'ALTERBUD Marek Duraj'
#define A_KOMU_A  "43-450 Ustroä, ul. J¢zefa Kreta 3A"
#define A_AUTOR   "A.D. 1991-2015, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
