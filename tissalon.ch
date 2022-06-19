#include "lan.ch"
#define UpP(x) UPPER(x)
#define PC852
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_KHNUM
#define A_KHNAZ 31
#define A_DOKFAK
#define A_ZAZNACZ
#define A_KHSEP
#define A_NAZWISKO
#define A_KHKONTO
#define A_LPNUM 3
#define isPrinter (.t.)
#command init screen => //__run('uniznak r')
#define STANY indx_mat
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define cenA_zaK cena_przy
#define A_WA
#define A_WE
#define A_IZ
#define A_FAT
#define A_FK 10
#define A_DF 11
#define A_DOKCOMP 12
#define A_DFP
#define A_SET_DAT  GERMAN
#define A_SUMK    -38083309601
#define A_KOMU_N  'ZAKADY DRZEWNE "TiS" S.C.'
#define A_KOMU_A  "43-210 Kobi¢r, ul. Centralna 57/36"
#define A_AUTOR   "A.D. 2000, Marek Dugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_FILELIMIT '40'
#define A_NAZBEG 7
#define A_NRLTH 5
#define A_FA
#define A_VAT
#define A_DLINK
#define INDEXPIC "XXXX-XXX-XXX"
#define A_SWWKTM "SWW:"
#define A_DRUKCOMP
#define A_NVAT
#define A_NOPV
#define A_NOZAP
#define A_CENVAT
#define A_AUTOMAR

#define WDFGR(i,c,pv,df)     ROUND(round(1000*i,0)*ROUND(if(df,100,10000/(100+pv))*c,0),-3)/1000

#define ILEVAT(i,c,pv,df)    ROUND(ILEVATGR(i,c,pv,df),0)/100
#define WzVAT(i,c,pv,df)     ROUND(WzVATGR(i,c,pv,df),0)/100
#define WbezVAT(i,c,pv,df)   (x:=WDFGR(i,c,pv,df),if(df,x-ROUND(x*pv/(100+pv),0),x)/100)

#define ILEVATGR(i,c,pv,df)  WDFGR(i,c,pv,df)*pv/if(df,(100+pv),100)
#define WzVATGR(i,c,pv,df)   WDFGR(i,c,pv,df)*if(df,1,(100+pv)/100)
#define WbezVATGR(i,c,pv,df) WDFGR(i,c,pv,df)*if(df,100/(100+pv),1)

#define W(i,c,pv,df)         WzVAT(i,c,pv,df)
#define WGR(i,c,pv,df)       WzVATGR(i,c,pv,df)
#define WPZ(w)        ROUND(w,A_ZAOKR)
#define VATPZ(w,pv)   (pv*w/100)
#define VATPZGR(w,pv) (pv*w)
#define A_FFULL
#define A_FDO
#define A_FP600