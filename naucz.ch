#include "lan.ch"
#define PC852
#define PLWIN
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_GETLPT eval(memvar->do_getlpt)
#define A_WIN_PRN memvar->p_win
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define A_ZAZNACZ
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_MYSZ
#define A_XPRN
#define A_DOKCOMP 10
#define A_15CPI
#define A_15CALI
#define A_DRUKCOMP
#define UpP(x) UPPER(x)
//#define DTOV(dat) left(dtoc(dat),5)
#define DTOV(dat) TRAN(subs(dtoS(dat),5),"@R XX.XX")
#define A_WA
#define A_SET_DAT GERMAN
#define A_SB
#define A_SHORTIND
#define STANY     INDX_MAT
#define A_SUMK    -43138968948
#define A_KOMU_N  'Dom Pomocy Spoˆecznej - "Dom Nauczyciela"'
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Pocztowa 14a"
#define A_AUTOR   "A.D. 1991-2005, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. (0-33)8522553"
#define A_TRWALOSC
#define DatE() MEMVAR->dzisiaj
#define isPrinter() .t.
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_JMALT
#define A_NOZAP
#define A_KODY "CPV"
#define KoD sww
#define A_KODVAL {||kw:=upper(kw),.t.}
//{|g,y,z,_s|z:=select(),sel('CPV','CPV_CODE'),_s:=array(33),_s[12]:=!g:changed .or. dbseek(y),_s[10]:=UPPER(trim(y)),_s[5]:=if(asc(y)>64,12,1),if(_s[5]=1,,ordsetfocus((indexord()%2)+1)),_s[6]:=len(_s[10]),_s[12]:=_s[12].or.szukam(_s).and.(kw:=field->code,.t.),dbselectar(z),_s[12]}
//#define A_SWWKTM "CPV"
//#define A_SWWVAL {|g,y,z,_s|z:=select(),sel('CPV','CPV_CODE'),_s:=array(33),_s[12]:=!g:changed .or. dbseek(y),_s[10]:=UPPER(trim(y)),_s[5]:=if(asc(y)>64,12,1),if(_s[5]=1,,ordsetfocus((indexord()%2)+1)),_s[6]:=len(_s[10]),_s[12]:=_s[12].or.szukam(_s).and.(s:=field->code,.t.),dbselectar(z),_s[12]}
//#define posilek konto
#define A_KOMOR

#define A_VAT
#define A_NVAT
#define A_FA
#define A_IZ
#define A_WE
#define A_WEBRUT
#define A_CENVAT
#define A_BACKUP defa+'bejkap.bat'


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
#define A_KHNAZ 31
#define A_DOKFAK
