#include "lan.ch"
#define A_SHARP
#define A_CDX DBFMDX
#define UpP(x) UPPER(x)
//#define A_NOREFRESH
//#define A_KHFILLZERO
#define A_LPNUM 3
#define PC852
#define isPrinter() .t.
//#define A_GETLPT MEMVAR->p_lpt
#command INIT SCREEN  => __run("uniznak.exe r")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
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
//#define A_WE
#define A_SWW
#define A_SWWKTM
#define A_PKWiU
#define STANY indx_mat
#define A_SUMK   -36547496816
#define A_KOMU_N 'Restauracja "Pod Przyˆazem"'
#define A_KOMU_A "Brenna, ul. Le˜nica 107"
#define A_AUTOR  "A.D. 2003, Marek D’ugosz, http://www.polbox.com/m/mdlugosz tel. 0601842030"
#define A_FILELIMIT '41'
#define A_NAZBEG 5
#define A_NRLTH 3
//#define A_NRLTH3
#define A_FA
#define A_FAT
#define A_VAT
#define INDEXPIC "XXXXXX"
#define A_NOZAP
//#define A_LFIRST
#define A_CENVAT
#define A_NVAT
*
#define A_DF 13
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
//#define A_AUTOMAR
#define A_DMDATA  //index wg daty
#define A_KPR 10
#define A_IZ
//#define A_WTYM
#define A_FIFO
//#define A_VIEWCZAK
#define A_MYSZ
#define A_FFULL
#define A_FDO
#define A_ENAZ
#define A_MINUS
#define A_ZAZNACZ
#define A_TRWALOSC
#define cenA_zaK CENA_PRZY
#define A_PROCMAR
