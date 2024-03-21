#include "lan.ch"
#define A_EXT hb_jsonencode,hb_jsondecode,hb_hautoadd
#define A_CDX DBFCDX
#define PC852 'PL852M'
#define PLWIN
#define UpP(x) UPPER(x)
#define A_FIFO
#define A_WL
#define A_ZAZNACZ
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define A_DRUKCOMP
//#define A_DOKCOMP 10
#define A_XPRN
#define A_15CPI
#define A_PCL
#define A_WA
#define A_WE
#define A_SB
#define A_SHORTIND
#define A_ZLEC11
#define A_JMALT
#define A_KODY "CPV"
#define KoD sww
#define A_KODVAL {|g,y,z,_s|z:=select(),sel('CPV','CPV_CODE'),_s:=array(33),_s[12]:=!g:changed .or. dbseek(y),_s[10]:=UPPER(trim(y)),_s[5]:=if(asc(y)>64,12,1),if(_s[5]=1,,ordsetfocus((indexord()%2)+1)),_s[6]:=len(_s[10]),_s[12]:=_s[12].or.szukam(_s).and.(kw:=field->code,.t.),dbselectar(z),.t.}
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -32558939352
#define A_KOMU_N  "DOM POMOCY SPOECZNEJ"
#define A_KOMU_A  "Cieszyn, ul. Korfantego 1"
#define A_AUTOR   "A.D. 1991-2010, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define DatE()    MEMVAR->dzisiaj
#define A_TRWALOSC
#define A_KONTROLA (.t.)
#define A_FILELIMIT '35'
#define A_DIETA   alias()#"INDX_MAT"
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_LPNUM 3
#define A_ZAZNACZ

#define A_FFULL
#define A_VAT
#define A_NVAT
#define A_FA
//#define A_IZ
#define A_WEBRUT
#define A_CENVAT
#define A_BACKUP memvar->backup

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
