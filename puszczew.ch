#include "lan.ch"
//#include "mdstd.ch"
//#define A_DDBF
#ifdef __PLATFORM__WINDOWS
 #define PLWIN
// #define A_WIN_PRN .f.
#endif
//#define A_MM
#define A_BACKUP memvar->backup
#define A_SPECYF
#define PC852
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_LPNUM 3
#define A_KHFILLZERO
#define A_KHSEP
#define A_NAZWISKO
#define A_F9
//#define A_STYLUS
//#define A_NOREFRESH
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->get_port)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_XPRN
#define isPrinter (.t.)
#define A_PCL
#command init screen => //__run('uniznak.exe r')
#define STANY indx_mat
#define A_FIFO
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define cenA_zaK cena_przy
#define A_WB
#define A_WBX
#define A_WA
#define A_WE
#define A_IZ
#define A_FAT
#define A_DF 10
#define A_DOKCOMP 11
#define A_WALUTA 12
//#define A_DFP
#define A_WTYM
#define A_JMTOT
#define A_SET_DAT  GERMAN
#define A_SUMK    -45253913498
#define A_KOMU_N  'Zakˆad Stolarsko-Tartaczny Ryszard Winecki'
#define A_KOMU_A  "Puszczew 80, 42-133 W©glowice"
#define A_AUTOR   "A.D. 2002, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
//#define A_FK 10
#define A_DATA
#define A_KHNAZ 31
#define A_DOKFAK
#define A_VAT
#define A_DLINK
#define A_ZLEC11
//#define A_NOMZ
#define INDEXPIC "XXXXXXXXXX"
#define A_7
#define A_LFIRST
#define A_SWWKTM "PKWiU"
#define A_DRUKCOMP
#define A_NVAT
//#define A_NOPV
#define A_NOZAP
#define A_INOWA
#define A_KOBSUR kh+indx_mat->index
#define A_BIECZ
//#define A_AUTOMAR
//#define A_PROCMAR

//#define A_CENVAT

#define WDFGR(i,c,pv,df)     ROUND(ROUND(round(1000*i,0)*if(df,100+pv,100)*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
//#define WbezVATGR(i,c,pv,df) (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x))
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WbezVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WbezVATGR(i,c,pv,df)

#define WPZ(w) ROUND(w,A_ZAOKR)
#define VATPZ(w,pv) (pv*w/100)
#define VATPZGR(w,pv) (pv*w)

#define A_FFULL
#define A_FDO
//#define A_FP600
#define A_A
//#define A_AF //anielskiego firmy
#define A_TPD
#define A_MINUS
#define A_MYSZ
#define A_ZAZNACZ
#define A_DATAVAT
