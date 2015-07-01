#ifdef __PLATFORM__WINDOWS
  #define PLWIN
#endif
#define A_EXT descend
#define UpP(x) UPPER(x)
#define PC852
#include "lan.ch"
#define A_CDX DBFCDX
#define A_SB
#define A_BACKUP memvar->bejkap
#define A_LPNUM 3
#define A_KHSEP
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_WIN_PRN eval(memvar->do_getwprn)
#define A_GETLPT eval(memvar->do_getlpt)
#define B_SKIP  {|x|dbskip(x),__dbLocate( for,while,,,.t.),found()}
#command init screen  =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define A_STYLUS
#define A_15CPI
#define isPrinter() .t.
#define A_ELZAB
#define A_PROCMAR
#define A_AUTOMAR
#define A_DRUKCOMP
#define A_NOZAP
#define A_DF 10
#define A_DOKCOMP 11
#define A_ZLEC11
#define A_KODY "Kasa"
#define A_KODVAL {||.t.}
#define nazwA (if(field->przel<>1,left(field->nazwa,32)+str(field->przel,5,3)+' '+Lower(field->jm_opcja),field->nazwa))
#define A_IZ
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_DLINK
#define A_FIFO
#define A_DIETA .t.
#define A_WA
#define A_SET_DAT   GERMAN
#define A_SHORTIND
#define A_SWWKTM "Naz kas"
#define STANY     indx_mat
#define A_SUMK    -34656590983
#define A_KOMU_N  'C.W.S. GWAREK Adrian Podolski'
#define A_KOMU_A  "Ustroä, ul. Wczasowa 49"
#define A_AUTOR   "A.D. 1991-2012, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
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
#define A_FFULL
#define A_FDO
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
#define A_MYSZ
#define A_DMDATA
#define A_KHNAZ 23
#define DatE() MEMVAR->dzisiaj
