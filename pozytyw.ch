#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_DFP 9600
  #define A_POSNET
  #define A_F9
  #define A_WIN_PRN memvar->p_win
#endif
#define A_BACKUP memvar->bejkap
#define A_EXT descend
#define UpP(x) UPPER(x)
#define PC852
#include "lan.ch"
#define A_CDX DBFCDX
#define A_LPNUM 3
#define A_KHSEP
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_GETLPT eval(memvar->do_getlpt)
#define B_SKIP  {|x|dbskip(x),__dbLocate( for,while,,,.t.),found()}
#command init screen  =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
//#define A_ELZAB
#define A_ALTCEN
//#define A_CENSPEC
#define A_FAT
#define A_NOPV
#define A_PROCMAR
#define A_AUTOMAR
#define A_DRUKCOMP
#define A_NOZAP
#define A_DF 10
#define A_DOKCOMP 11
//#define A_KODY "Kasa"
//#define A_KODVAL {||.t.}
#define nazwA (if(field->przel<>1,left(field->nazwa,36)+str(field->przel,5,3)+' '+Lower(field->jm_opcja),field->nazwa))
//#define nazWa (if(empty(field->sww),field->nazwa,field->sww))
#define A_IZ
#define A_ZAZNACZ
#define A_DOKCOLOR eval(MEMVAR->do_getcolor,x,_s)
//if(kto_pisal=chr(255),"GB+/N+",if(kto_pisal>chr(1),_snorm,"R+/N+"))
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_DLINK
#define A_FIFO
#define A_MINUS
#define A_WA
#define A_SET_DAT   GERMAN
#define A_SHORTIND
#define A_SWWKTM "PKWiU"
//#define A_KASA
#define STANY     indx_mat
#define A_SUMK    -37434066382
#define A_KOMU_N  'Gastronomia i Catering Andrzej Stabrawa'
#define A_KOMU_A  "43-426 D©bowiec, Ogrodzona 96"
#define A_AUTOR   "A.D. 1991-2013, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_FILELIMIT '36'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_FA
#define A_VAT
#define INDEXPIC "XXXX"
#define A_NVAT
#define A_CENVAT
#define A_CF8
#define A_JMALT
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
#define A_KHNAZ 31
#define A_DOKFAK
#define DatE() MEMVAR->dzisiaj
#define A_FFULL
