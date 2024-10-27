//#define FIXDOK
#include "dbinfo.ch"

#define I hb_UTF8ToStr("│")

memvar strona,stawki,stawkizby,it_zesmnu,oprn,defa
#ifdef A_KHSEP
#define D_KH kontrahent
#define D_KH1 dost_odb
#else
#define D_KH left(dost_odb,A_NRLTH)
#define D_KH1 if(val(dost_odb)="0",dost_odb,subs(dost_odb,A_NRLTH+2))
#endif
#include "inkey.ch"
#ifdef A_NVAT
#define D_NVAT "D_VAT"
#else
#define D_NVAT "WART_VAT"
//#define d2biN(x) ((x)/100)
#define field2biN(x,y) fieldput(x,(y)/100)
#define bin2D(x) (100*(x))
#endif
#ifdef A_DF
#define wartosC (bin2d(binfieldget('d_wartosc')))
#endif

#ifdef A_JMTOT
  #define D_JMTOT
#endif

#ifdef A_GRAM
  #define D_GRAM
#endif

#ifdef A_MM
#define KEY_DOK smb_dow
#define D_MM ''
#else
#define KEY_DOK nr_mag+smb_dow
#define D_MM nr_mag
#endif

//#define speC(x) if(set(_SET_PRINTER),x,"")

#ifdef A_PCL
  #ifndef A_15CALI
    #define A_15CALI
  #endif
  #ifdef A_XPRN
memvar landscape
  #else
static landscape:=.f.
  #endif
#define P_ROWN if(landscape,p_rownl,p_rown)
#define P_COLN if(landscape,p_colnl,p_coln)
#endif

#ifdef A_XPRN
memvar P_12LPI,P_6LPI,P_SUPON,P_SUPOFF,P_UON,P_UOFF,p_rown,p_coln,p_rownl,p_colnl,p_land,p_margin
#else
#ifdef A_PCL
#undef P_ROWN
#undef P_COLN
#define P_MARGIN {|x,y|HB_BCHAR(0x1B)+'&'+'a'+lTrim(sTr(x))+'L'}
#define P_LAND {|x|if((landscape:=x),'',HB_BCHAR(0x1B)+"&"+"l"+IF(x,"1","0")+"O")}
#define P_ROWN if(landscape,42,58)
#define P_COLN if(landscape,113,78)
#define P_12LPI HB_BCHAR(0x1B)+'&l12D'
#define P_6LPI  HB_BCHAR(0x1B)+'&l6D'
#define P_SUPON HB_BCHAR(0x1B)+'(s8V'
#define P_SUPOFF HB_BCHAR(0x1B)+'(s12V'
#define P_UON   HB_BCHAR(0x1B)+'&d0D'
#define P_UOFF   HB_BCHAR(0x1B)+'&d@'
#else
#define P_ROWN  60
#define P_COLN  80
#define P_12LPI HB_BCHAR(0x1B)+'3'+HB_BCHAR(0x12)
#define P_6LPI  HB_BCHAR(0x1B)+'2'
#define P_SUPON HB_BCHAR(0x1B)+'S1'
#define P_SUPOFF HB_BCHAR(0x1B)+'T'
#define P_UON   HB_BCHAR(0x1B)+'-1'
#define P_UOFF   HB_BCHAR(0x1B)+'-0'
#endif
#endif


#ifdef A_FFULL
#define D_FFULL longname
#else
#define D_FFULL nazwa
#endif

#define TrAN(a,b,c,d) strpic(a,d,c,b)
#define TraN(a,b) strpic(a,,,b)
//TraN(W,"@E ",A_ZAOKR,15)
#ifdef A_ZLEC11
#define nr_zleC pad(strtran(nr_zlec,'     '),6)
#endif

#ifdef A_OLZBY
#define WANAZ "WAGA kg"
#else
#define WANAZ "Wartość"
#endif
#ifdef A_SZTUKI
#define ILDEC 0
#define ILPIC " ###"
#else
#define ILDEC 3
#define ILPIC ".###"
#endif


#ifdef A_ADS
#ifndef A_STSIMPLE
#define A_STSIMPLE
#endif
#endif

MEMVAR magazyny,adres_mag,;
       firma_n,firma_a,rodz_sprzed,zamowienie,stanowis,miar_opcja

MEMVAR GETLIST,NAZWA_MAG,mag_poz,MAG_BIEZ,;
       dok_par,dokumenty,dok_rozch,dok_zewn,dok_ewid,pm

FIELD WARTOSC,ILOSC,DATA,INDEX,nr_mag,smb_dow,nr_dowodu,pozycja,nr_zlec,dost_odb,kontrahent,;
      przelewem,czekiem,cena,proc_vat,nr_faktury,jm,jm_opcja,przel,info,wart_vat,;
      numer_kol,uwagi,nazwa,adres,rodz_opak,gram,wart_net,nr_kpr,data_dost,;
      ZAMK_MIES1,ZAMK_MIES2,ZAMKN_ROKU,wart_MIES1,wart_MIES2,wart_ROKU,longname,;
      sub_dok,wart_ewid,d_wartosc,konto,opis_koszt,SJMO,SJMO_MIES1,SJMO_MIES2,SJMO_ROKU,;
      data_vat,cena_k

#ifdef A_LPNUM
#define D_LP0 str(0,A_LPNUM) //'  0'
#define D_LP1 str(1,A_LPNUM) //'  1'
#define D_LPVAL(x) val(x)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPSTR1(x) str(D_LPVAL(x),1)
#else
#define D_LP0 '0'
#define D_LP1 '1'
#define D_LPVAL(x) (HB_BCODE(binfieldget([x]))-48)
#define D_LPSTR(x) str(HB_BCODE(binfieldget([x]))-48,3)
#define D_LPSTR1(x) x
#endif

#ifdef A_KAMIX
  #undef D_GRAM
  #undef D_JMTOT
#endif

#ifdef D_GRAM
#ifdef NAZWA
#undef NAZWA
#define NAZWA (trim(nazwa)+" "+trim(rodz_opak)+" "+lTrim(sTr(gram)))
#endif
#endif

#ifdef A_IZ
field ilosc_f
#else
#define ilosc_f ilosc
#endif

#define dok_df  dok_par[MAG_POZ,i,A_DF]
#define dok_wal dok_par[MAG_POZ,i,A_WALUTA]

PROCEDURE zestawienia(_menu)
MEMVAR menuzest
local a:={},r,f2,f3,f4,f5,f6,x
   if _menu=NIL
      _menu:=menuzest
      setpos(6,maxcol()/2+1)
      SETKEY(4,{||KIBORD(HB_BCHAR(27)+HB_BCHAR(24)+HB_BCHAR(13))})
      SETKEY(19,{||KIBORD(HB_BCHAR(27)+HB_BCHAR(5)+HB_BCHAR(13))})
#ifndef A_NOMZ
          a:={hb_UTF8ToStr('R ZESTAWIENIE ROZCHODÓW WEDŁUG KONT'),;
              hb_UTF8ToStr('P ZESTAWIENIE PRZYCHODÓW WEDŁUG KONT')}
#endif
       aadd(a,hb_UTF8ToStr('K KARTOTEKA MATERIAŁOWA'))
       aadd(a,hb_UTF8ToStr('O DOKUMENTY OBROTU MATERIAŁOWEGO'))
       aadd(a,hb_UTF8ToStr('D DOKUMENTY WEDŁUG KODU MATERIAŁU'))
#ifdef A_FA
       aadd(a,hb_UTF8ToStr('S REJESTR SPRZEDAŻY'))
       aadd(a,hb_UTF8ToStr('Z REJESTR ZAKUPÓW'))
#endif
#ifdef A_KPR
       aadd(a,hb_UTF8ToStr('KSIĘGA PRZYCHODÓW I ROZCHODÓW'))
#endif
#ifdef A_OLZA
       aadd(a,hb_UTF8ToStr('KK DOKUMENTY WEDŁUG KONTA KOSZTÓW'))
       aadd(a,hb_UTF8ToStr('SK DOKUMENTY WEDŁUG STANOWISKA KOSZTÓW'))
#endif
#ifdef A_DRUKCOMP
       aadd(a,'ZESTAWIENIA DODATKOWE')
#endif

   if !aczojs(a,"",@_menu)
      SET KEY 4 TO
      SET KEY 19 TO
      RETURN
   ENDIF
   SET KEY 4 TO
   SET KEY 19 TO
   menuzest:=_menu
   endif

BEGIN SEQUENCE

SETPRC(99,0)
private strona:=0

@ 7,0 SAY 'Aby zablokować wysuw ostatniej kartki naciśnij klawisz [Delete] podczas wydruku' UNICODE
@ 18,0 CLEAR

#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif

#ifdef A_DDBF
   select daty
   goto 1
#else
#define DatY MEMVAR
#endif

select FIRMY
  set order to TAG FIRM_NUM

SELECT DM
  SET ORDER TO 1

SELECT INDX_MAT
  set order to TAG INDX_NUM

#ifndef STANY
SELECT STANY
  SET ORDER TO TAG STAN_MAG
  SET RELATION TO INDEX INTO indx_mat
#endif
  GO TOP

SELECT MAIN
  set order to TAG MAIN_IND
  pm:=1
  f2:=setkey(-1,{|p,g,k,s|g:=getactive(),k:=setkey(-1,),s:=push_stat(),katalog(,g),pop_stat(s),setkey(-1,k)})

  f3:=setkey(-2,{|p,g,k,s|g:=getactive(),k:=setkey(-2,),s:=push_stat(),help3(g),pop_stat(s),setkey(-2,k)})
#ifdef A_FA
#ifndef A_RODZ_S
#define A_RODZ_S
#endif
#endif
#ifdef A_RODZ_S
  f4:=setkey(-3,{|k|k:=setkey(-3,),aczojs(rodz_sprzed[mag_poz],"",0),setkey(-3,k)})
#endif
  f5:=setkey(-4,{|k|k:=setkey(-4,),aczojs(zamowienie[mag_poz],"",0),setkey(-4,k)})
#ifdef A_OBR
  f6:=setkey(-5,{|k,s|k:=setkey(-2,),s:=push_stat(),dbselectar("stanowis"),ordsetfocus("kont_naz"),szukam({0,10,maxrow(),,8,0,"",{||konto+I+opis_koszt},{|_skey|_skey=K_ESC},""}),pop_stat(s),setkey(-5,k)})
#else
  f6:=setkey(-5,{|k|k:=setkey(-5,),aczojs(stanowis[mag_poz],"",0),setkey(-5,k)})
#endif
r:=.t.

#ifdef __PLATFORM__UNIX
#define nl HB_BCHAR(10)
#else
#define nl HB_BCHAR(13)+HB_BCHAR(10)
#endif
#ifdef A_HPDF
  #define D_HWPRN A_HPDF
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
#ifdef A_PCL
MEMVAR->landscape:=.f.
#endif
#ifdef D_HWPRN
oprn:=D_HWPRN
#command ?  [<exp,...>]         => wQ()[;?? <exp>]
#command ?? <exp1> [,<expn>]    => wqq(<exp1>)[;wQQ(<expn>)]
#command TEXT <st> => aeval(getlines(strtran(<st>,";",nl)),{|x|wq(x)})
#define QQOUT(x) WQQ(x)
#define QOUT(x) WQ(x)
#else
#command ? [<List,...>] => qout() [;?? <List>]
#command ?? <exp1> [,<expn>]    => qqout(<exp1>)[;QQout(<expn>)]
#command TEXT <st> => aeval(getlines(strtran(<st>,";",nl)),{|x|qout(x)})
#endif
DO CASE
#ifndef A_NOMZ
  CASE a[_menu]='R ' ; druk(w_zlec())
  CASE a[_menu]='P ' ; druk(w_zam())
#endif
  case a[_menu]='K ' ; w_p_r()
  case a[_menu]='O ' ; w_DOK()
  case a[_menu]='D ' ; druk(w_as())
#ifdef A_FA
  case a[_menu]='S ' ; w_REJ()
  case a[_menu]='Z ' ; w_zak()
#endif
#ifdef A_KPR
  case a[_menu]='KS'; w_kpr()
#endif
#ifdef A_OLZA
  case a[_menu]='KK' ; w_DOK_k()
  case a[_menu]='SK' ; w_DOK_s()
#endif
#ifdef A_DRUKCOMP
  otherwise
     public it_zesmnu
     if valtype(it_zesmnu)='L'
        it_zesmnu:=NIL
     endif
     _menu:=it_zesmnu
     r:=w_zes(@_menu)
     if _menu#NIL
        it_zesmnu:=_menu
     endif

#endif
endcase
IF STRONA>0
#ifdef A_WADO
? ccpi(4)
? A_WADO
//'Sporządził:                                             Zatwierdził:'
#endif
   ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
#ifdef A_PCL
/**********
if landscape
   ?? speC(eval(P_LAND,.f.)) //HB_BCHAR(0x1B)+'&l0O')
endif
********/
//   ?? speC(HB_BCHAR(0x1B)+"E")

//   landscape:=.f.
#endif
#ifdef D_HWPRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
   setprc(0,0)
   set print off
//   set printer to
   if set(_SET_ALTERNATE,.f.).and. file(a:=set(_SET_ALTFILE,""))
      fview(a)
      ferase(a)
   endif
#ifdef A_PRINT
     x:=set(_SET_PRINTFILE,'')
     if ! set(_SET_PRINTFILE)==x .and. File(x)
        A_PRINT(x)
     endif
#endif
ELSEIF r=.t.
   alarm("BRAK TAKICH DANYCH !",,,3)
ENDIF

end
  setkey( -1, f2)
  setkey( -2, f3)
  setkey( -3, f4)
  setkey( -4, f5)
  setkey( -5, f6)

SET PRINTER off
set printer to
set alternate to
set alternate off
UNLOCK ALL

RETURN
*****************
func help3(g)
memvar d_o,kh
local r,s:='',t,a
d_o:=kh:=space(A_NRLTH)
if valtype(g)='O'
   t:=g:varget()
/*
   if len(t)>=len(main->nr_zlec))
     s:=trim(subs(t,len(main->nr_zlec)-len(firmy->numer_kol)+1,len(firmy->numer_kol)))
   else
     s:=t
   endif
*/
endif
r:=szukam({1,14,maxrow(),,1,0,'FIRMY',{||numer_kol+if(""=uwagi,I,"*")+nazwa},{|_skey,_s|if(_skey=13,_s[12]:=.t.,gfirma(_skey,_s,getlist))},""})
      if r.and.valtype(g)='O'
         a:=MAIN->(len(EvAlDb(IndexKey(3)))-10-hb_fieldlen('index'))
      if len(t)>=main->(hb_fieldlen('nr_zlec'))
         t:=firmy->(left(t,a-hb_fieldlen('numer_kol'))+FIELD->numer_kol)+subs(t,a+1)
      else
         t:=FIRMY->(FIELD->numer_kol+subs(t,hb_fieldlen('numer_kol')+1))
      endif
         g:varput(t)
      endif
return r
#ifndef A_NOMZ
*********************
func w_zlec
local od,do,i_od,i_do,bkey,bpic,i_gr,a
OD := DatY->DATA_GRAN+1
DO := IF(DatY->DATA_GRAN>DatY->D_Z_MIES2,DatE(),DatY->D_Z_MIES1)
#ifdef A_FA
@ 13,10 SAY "ZESTAWIENIE DOKUMENTÓW SPRZEDAŻY DLA POSZCZEGÓLNTCH FIRM" UNICODE
#else
@ 13,10 SAY "ZESTAWIENIE DOKUMENTÓW ROZCHODU DLA POSZCZEGÓLNTCH KONT" UNICODE
#endif

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

pm:=-1

  SET ORDER TO TAG MAIN_ZLE
  SET RELATION TO KEY_DOK+nr_dowodu INTO DM

  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
#ifdef A_OLZA
  BKEY:={||nr_mag+UpP(NR_ZLEC)+SMB_DOW+index}
  bpic:="##-XXXXXXXXX-XX/"+ INDEXPIC
  I_OD:=i_do:=EvaldB(bkey)
  i_gr:=13
  @ 19,5 SAY 'MAG-ZLEC-DOK-INDEKS  od :' GET I_OD PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,5 SAY 'MAG-ZLEC-DOK-INDEKS  do :' GET I_DO PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
#else
  i_od:=IndexkeY()
  BKEY:=EvAlDb('{||'+LEFT(i_od,RAT('+DTOS(',UpP(i_od))-1)+'}') //UpP(nr_zlec)+NR_MAG+index}
  I_OD:=I_DO:=EvaldB(bkey)
#ifdef A_ZLEC11
  i_gr:=len(i_od)
  bpic:=padl("XXXXXXXXXXX-XX/"+ INDEXPIC ,i_gr+2+len( INDEXPIC )-hb_fieldlen('index'),"X")
  i_gr:=11
#else
  i_gr:=8
  bpic:="XXXXXX-XX/"+ INDEXPIC
#endif
#ifdef A_FA
  if len(magazyny)=1
     i_gr-=2
  endif
#endif
  @ 19,5 SAY 'KONTO-MAGAZYN-INDEKS od :' GET I_OD PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,5 SAY 'KONTO-MAGAZYN-INDEKS do :' GET I_DO PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
#endif
  @ 22,10 SAY 'Grupowanie według pierwszych' UNICODE GET I_gr picture "##" valid i_gr>=0 .and. i_gr<=len(i_od)
  SAYL " znaków klucza" UNICODE
READ
if readkey()=27
  break
endif

I_OD=UpP(TRIM(I_OD))
I_DO=UpP(TRIM(I_DO))
a:=len(EvAlDb(IndexKey()))-10-hb_fieldlen('index')
#ifdef A_OBR
stanowis->(ordsetfocus("kont_num"))
return({od,do,i_od,i_do,i_gr,bkey,bpic,-1,hb_UTF8ToStr("ZESTAWIENIE ROZCHODÓW W/G KONT"),{||NIL},{|x,y|;
  if(i_gr>=a.and.stanowis->(dbseek(left(main->nr_zlec,a))),QQOUT(stanowis->opis_koszt),)}})
#else
#ifdef A_FA
return({od,do,i_od,i_do,i_gr,bkey,bpic,-1,hb_UTF8ToStr("ZESTAWIENIE SPRZEDAŻY W/G KONTRAHENTÓW"),{||NIL},{||;
  FIRMY->(IF(i_gr>=a.and.DBSEEK(subs(MAIN->NR_ZLEC,a+1-A_NRLTH,A_NRLTH)),(QOUT("  "+nazwa),QOUT(adres)),)),if(i_gr>=a+2,QOUT(subs(magazyny[ascan(magazyny,nr_mag)],4)),)}})
#else
return({od,do,i_od,i_do,i_gr,bkey,bpic,-1,hb_UTF8ToStr("ZESTAWIENIE ROZCHODÓW W/G KONT"),{||NIL},{|x,y,z|z:='',;
  if(i_gr>=a+2.and.(x:=ascan(magazyny,nr_mag),y:=if(x=0,x,ascan(stanowis[x],{|x|nr_zlec=strtran(trim(left(x,a)),'*')})),y#0),z:=subs(stanowis[x,y],8),),;
  FIRMY->(IF(i_gr>=a.and.DBSEEK(subs(MAIN->NR_ZLEC,a+1-A_NRLTH,A_NRLTH)),(QOUT("  "+nazwa),QOUT(adres)),)),z}})
#endif
#endif
***********************************
func w_zam
local od,do,i_od,i_do,bkey,bpic,i_gr,a

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)

@ 13,10 SAY "ZESTAWIENIE PRZYCHODÓW W/G SYMBOLI KONT" UNICODE

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

  SET ORDER TO TAG MAIN_ZLE
  SET RELATION TO KEY_DOK+nr_dowodu INTO DM

  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
#ifdef A_OLZA
  BKEY:={||nr_mag+UpP(NR_ZLEC)+SMB_DOW+index}
  I_OD:=i_do:=EvaldB(bkey)
  bpic:="##-XXXXXXXXX-XX/"+ INDEXPIC
  i_gr:=13
  @ 19,10 SAY 'MAG-KONTO-DOK-INDEKS od :' GET I_OD PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,10 SAY 'MAG-KONTO-DOK-INDEKS do :' GET I_DO PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
#else
  i_od:=IndexkeY()
  BKEY:=EvAlDb('{||'+LEFT(i_od,RAT('+DTOS(',UpP(i_od))-1)+'}') //UpP(nr_zlec)+NR_MAG+index}
  I_OD:=I_DO:=EvaldB(bkey)
#ifdef A_ZLEC11
  i_gr:=len(i_od)
  bpic:=padl("XXXXXXXXXXX-XX/"+ INDEXPIC ,i_gr+2+len( INDEXPIC )-hb_fieldlen('index'),"X")
  i_gr:=13
#else
  i_gr:=8
  bpic:="XXXXXX-XX/"+ INDEXPIC
#endif
  @ 19,15 SAY 'KONTO-MAGAZYN-INDEKS od :' GET I_OD PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,15 SAY 'KONTO-MAGAZYN-INDEKS do :' GET I_DO PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
  if len(magazyny)=1
     i_gr-=2
  endif
#endif
  @ 22,10 SAY 'Grupowanie według pierwszych' UNICODE GET I_gr picture "##" valid i_gr>=0 .and. i_gr<=len(i_od)
  devout(" znaków klucza")
READ
if readkey()=27
  break
endif
I_OD=UpP(TRIM(I_OD))
I_DO=UpP(TRIM(I_DO))
a:=len(EvAlDb(INDEXKEY()))-10-hb_fieldlen('index')

return({od,do,i_od,i_do,i_gr,bkey,bpic,1,hb_UTF8ToStr("ZESTAWIENIE PRZYCHODÓW W/G KONT"),{||NIL},{||;
  FIRMY->(IF(i_gr>=a.and.DBSEEK(subs(MAIN->NR_ZLEC,a+1-A_NRLTH,A_NRLTH)),(QOUT("  "+nazwa),QOUT(adres)),)),HB_BCHAR(13)+replicate('_',78)}})
#endif
****************
FUNCTION PRZYCH(n)
field smb_dow,nr_mag
#ifdef A_PR_ZW
if !KEY_DOK$dok_zewn
#else
if KEY_DOK$dok_rozch
#endif
   N:=0
ENDIF
RETURN(N)
***************
FUNCTION ROZCH(n)
field smb_dow,nr_mag
#ifdef A_PR_ZW
if KEY_DOK$dok_zewn
#else
if !KEY_DOK$dok_rozch
#endif
   N:=0
ENDIF
RETURN(-N)
***********
func w_as

local od,do,i_od,i_do,bkey,bpic,i_gr,i
static dok_sp,d

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)

@ 12,10 SAY "ZESTAWIENIE DOKUMENTÓW W/G SYMBOLU MATERIAŁU" UNICODE

@ 14,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

#ifdef A_FA
  dok_sp:=""
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|if(dok_par[i,b,1]="F".and.!left(y,2)$dok_sp,dok_sp+=left(y,2)+" ",)})})
  dok_sp:=padr(dok_sp,40)
#else
  dok_sp:=""
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|if(dok_par[i,b,1]#"P".and.!left(y,2)$dok_sp,dok_sp+=left(y,2)+" ",)})})
  dok_sp:=padr(dok_sp,40)
#endif
SELECT STANY
  bkey:={||nr_mag+index}
  bpic:="XX/"+ INDEXPIC
  SEEK MAG_BIEZ LAST
  I_DO:=MAG_BIEZ+INDEX
  SEEK MAG_BIEZ
  I_OD:=MAG_BIEZ+INDEX
  @ 16,10 SAY 'DLA DOKUMENTÓW:' UNICODE GET dok_sp PICTURE "@K!"
  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
  @ 20,05 SAY 'MAT. "od" :' GET I_OD PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,45 SAY 'MAT. "do" :' GET I_DO PICTURE "@KR! "+bpic valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
  i_gr:=2
  @ 22,10 SAY 'grupowanie według pierwszych' UNICODE GET I_gr picture "##" valid i_gr>=0 .and. i_gr<=hb_fieldlen('index')
  SAYL " znaków klucza" UNICODE
  READ
  if readkey()=27
    break
  endif
  d:=dok_sp:=alltrim(dok_sp)
  if len(d)=2 .and. (i:=ascan(dokumenty[mag_poz],d))>0
     d:=dokumenty[mag_poz,i]
  endif
  I_OD=UpP(TRIM(I_OD))
  I_DO=UpP(TRIM(I_DO))

  SELECT MAIN
  SET RELATION TO KEY_DOK+nr_dowodu INTO DM
  SET ORDER TO TAG MAIN_IND
  SET FILTER TO smb_dow$dok_sp
  seek i_od

return {od,do,i_od,i_do,i_gr,bkey,bpic,if(D_MM+left(dok_sp,2)$dok_rozch,-1,1),hb_UTF8ToStr("ZESTAWIENIE W/G SYMBOLU MATERIAŁU"),{||QOUT("Dokumenty: "+d)},{||NIL}}
***********
proc druk(i)

LOCAL od,do,W,TXT,i_od,i_do,sel,bkey,WTOT,DFLAG,IR,IW,IN,multiflag,zby_flag,;
      bw,bc,t_gr,i_gr,i_mul,gri,grt,jm_o,wat,wai,pm,v,VTOT,iv,bv,bpic,nz,;
      _dnazwa,_dhead,_dhead2,pzdok,dfdok,j,x,wd,vd,txth,bi,wa_flag

      asize(i,max(len(i),17))
     od:=i[1]
     do:=i[2]
   i_od:=i[3]
   i_do:=i[4]
   i_gr:=i[5]
   bkey:=i[6]
   bpic:=i[7]
     pm:=i[8]
_dnazwa:=i[9]
_dhead :=i[10]
_dhead2:=i[11]

DEFAULT _dnazwa TO ''
DEFAULT _dhead TO {||NIL}
DEFAULT _dhead2 TO {||NIL}

#ifdef A_FA
   zby_flag:=i[12]
#endif
   wa_flag:=i[13]

   default wa_flag TO .t.


#ifdef A_IZ
     bi:=i[14]
#define D_ILOSC eval(bi)
#else
#ifdef A_JMALTTOT
#define D_ILOSC if(jm_o,A_JMALTTOT(pm*ilosc,nr_zlec,sel,x),pm*ilosc)
#else
#define D_ILOSC pm*ilosc
#endif
#endif

#ifdef A_FA
     bc:=i[15]
     bw:=i[16]
     bv:=i[17]
#define D_CENA eval(bc)
#define D_WARTOSC eval(bw)
#define D_VAT eval(bv)

  if zby_flag=NIL
     zby_flag:=!tak("CZY PO CENACH EWIDENCYJNYCH",23)
  endif
  if zby_flag

#ifdef A_IZ
#ifdef A_JMALTTOT
     DEFAULT bi TO {||if(jm_o,A_JMALTTOT(pm*ilosc_f,nr_zlec,sel,x),pm*ilosc_f)}
#else
     DEFAULT bi TO {||pm*ilosc_f}
#endif
#endif

     pzdok:=""
     aeval(dok_par,{|x,y|i:=y,aeval(x,{|z,j|if(z[1]#"F",pzdok+=left(magazyny[i],2)+left(dokumenty[i,j],2),)})})
#ifdef A_DF
     dfdok:=""
#ifdef A_SUBDOK
     aeval(dok_par,{|x,y|i:=y,aeval(x,{|z,j|if(z[1]="F".and.z[A_DF],dfdok+=left(magazyny[i],2)+left(dokumenty[i,j],4),)})})
     //set relation to KEY_DOK+nr_dowodu into DM
#define D_DF KEY_DOK+dm->sub_dok$dfdok
#else
     aeval(dok_par,{|x,y|i:=y,aeval(x,{|z,j|if(z[1]="F".and.z[A_DF],dfdok+=left(magazyny[i],2)+left(dokumenty[i,j],2),)})})
#define D_DF KEY_DOK$dfdok
#endif
#endif
#ifdef A_CENVAT
     if pm=1 // netto
     DEFAULT bw TO {|x|if(KEY_DOK$pzdok,cena,WbezVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#ifdef A_WEBRUT
     DEFAULT bv TO {|x|if(KEY_DOK$pzdok,if(KEY_DOK$dok_ewid,wartosc-cena,VATPZ(cena,val( proc_vat))),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#else
     DEFAULT bv TO {|x|if(KEY_DOK$pzdok,VATPZ(cena,val( proc_vat)),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#endif
     DEFAULT bc TO {|x|if(KEY_DOK$pzdok,cena/ilosc_f,WbezVAT(1,cena,val(proc_vat),D_DF))}
     else // brutto
     DEFAULT bw TO {|x|-if(KEY_DOK$pzdok,cena+VATPZ(cena,val( proc_vat)),WGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#ifdef A_WEBRUT
     DEFAULT bv TO {|x|-if(KEY_DOK$pzdok,if(KEY_DOK$dok_ewid,wartosc-cena,VATPZ(cena,val( proc_vat))),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#else
     DEFAULT bv TO {|x|-if(KEY_DOK$pzdok,VATPZ(cena,val( proc_vat)),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#endif
     DEFAULT bc TO {|x|if(KEY_DOK$pzdok,(cena+VATPZ(cena,val( proc_vat)))/ilosc_f,cena)}
     endif
#else
     DEFAULT bw TO {|x|pm*if(KEY_DOK$pzdok,cena,WGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#ifdef A_WEBRUT
     DEFAULT bv TO {|x|pm*if(KEY_DOK$pzdok,if(KEY_DOK$dok_ewid,wartosc-cena,VATPZ(cena,val( proc_vat))),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#else
     DEFAULT bv TO {|x|pm*if(KEY_DOK$pzdok,VATPZ(cena,val( proc_vat)),ILEVATGR(ilosc_f,cena,val(proc_vat),D_DF)/100)}
#endif
     DEFAULT bc TO {|x|if(KEY_DOK$pzdok,cena/ilosc_f,cena)}
#endif
#undef D_DF
  else
#ifdef A_IZ
#ifdef A_JMALTTOT
     DEFAULT bi TO {||if(jm_o,A_JMALTTOT(pm*ilosc,nr_zlec,sel,x),pm*ilosc)}
#else
     DEFAULT bi TO {||pm*ilosc}
#endif
#endif
#ifdef A_WA
     DEFAULT bw TO {||pm*wartosc}
#ifdef A_CK
     if fieldpos('cena_k')=0
       DEFAULT bc TO {||wartosc/ilosc}
     else
       DEFAULT bc TO {||cena_k}
     endif
#else
     DEFAULT bc TO {||wartosc/ilosc}
#endif
#else
     DEFAULT bw TO {||pm*ilosc*(sel)->cenA}
     DEFAULT bc TO {||(sel)->cenA}
#endif
     DEFAULT bv TO {||0}
  endif
#else
#ifdef A_WA
  #define D_WARTOSC pm*wartosc
#ifdef A_CK
  #define D_CENA    if(fieldpos('cena_k')=0,wartosc/ilosc,cena_k)
#else
  #define D_CENA    wartosc/ilosc
#endif
#else
  #define D_WARTOSC pm*ilosc*(sel)->cenA
  #define D_CENA    (sel)->cenA
#endif
#endif

#ifdef D_JMTOT
  #define A_JMTOT_OR_GRAM
#else
  #ifdef D_GRAM
    #define A_JMTOT_OR_GRAM
  #endif
#endif

#ifdef A_JMTOT_OR_GRAM
  #define D_POS 20
  #ifdef D_JMTOT
     grt:={}
  #endif
  #ifdef D_GRAM
     #define D_WAGA pm*ilosc*(sel)->gram/1000
     wat:=0
  #endif
#else
  #define D_POS 31
#endif
  i_mul:=WTOT:=0
#ifdef A_FA
  VTOT:=0
#endif

i:=NIL

seek I_OD
IF TAK("CZY TYLKO GRUPY",23,,.F.,.f.)
  @ 8,0 CLEAR
#ifdef A_JMALTTOT
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
#define jM (if(jm_o,jm_opcja,jm))
#endif

DO WHILE EvaldB(bkey)<=I_DO .AND. !EOF()
  ++i_mul
  TXT:=EvaldB(bkey)
  seek TXT+DTOS(od)
  IF EvaldB(bkey)+DTOS(DATA)>TXT+DTOS(do) .OR. EOF()
     IF EvaldB(bkey)=txt
        seek TXT+"Z"
     ENDIF
     LOOP
  ENDIF
  W:=0
#ifdef A_FA
  V:=0
#define addVAT(x,y) x+=y
#define arrVAT(x)   ,x
#else
#define addVAT(x,y)
#define arrVAT(x)
#endif
#ifdef D_GRAM
  wai:=0
#endif
#ifdef D_JMTOT
  gri:={}
  IF prow()+3+len(grt)>P_ROWN
#else
  IF prow()+3>P_ROWN
#endif
    IF STRONA>0
       specout(ccpi(4)+HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
       setprc(0,0)
    else
       print()
    ENDIF
    ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
    ?
    ? "SYNTETYCZNE ",_dnazwa," OD DNIA ",od," DO ",do,"."
/*
    if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
    ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
    ENDIF
*/
    ?
    if wa_flag
#ifdef A_FA
    IF zby_flag
#ifdef A_CENVAT
        ?? if(pm=1,"PO CENACH ZAKUPU NETTO","PO CENACH ZBYTU BRUTTO")
#else
        ?? "PO CENACH ZAKUPU/ZBYTU"
#endif
    ELSE
        ?? "PO CENACH EWIDENCYJNYCH"
    ENDIF
    ?
#endif
    endif
    ?? "Strona"+str(++strona,3)
    eval(_dhead)
  if wa_flag
    ? ccpi(5)
  else
    ?
  endif
  ENDIF

  //if i_gr>=len(nr_zlec)
  txth:=eval(_dhead2)
  if ! (valtype(txth)$'MC')
     txth:=''
  endif
  //endif
  ? padl("GRUPA:"+TranR(left(EvaldB(bkey),i_gr),bpic),24)+":"
  do while txt=left(EvaldB(bkey),i_gr) .and. EvaldB(bkey)<=i_do .and. !EOF()
     IF EvaldB(bkey)+DTOS(DATA)>TXT+DTOS(do)
        IF EvaldB(bkey)=txt
           seek TXT+"Z"
        ENDIF
        IF txt=left(EvaldB(bkey),i_gr)
           txt:=EvaldB(bkey)
           seek TXT+DTOS(od)
        ENDIF
        LOOP
     ENDIF
#ifdef A_JMTOT_OR_GRAM
#ifdef STANY
     indx_mat->(dbseek(main->nr_mag+main->index,.f.))
#else
     indx_mat->(dbseek(main->index,.f.))
#endif
     do while txt=EvaldB(bkey) .and. data<=do
        sel:=i_lam(data)
        W+=wd:=D_WARTOSC
        addVAT(V,vd:=D_VAT)
#ifdef D_GRAM
        wai+=D_WAGA
        wat+=D_WAGA
#endif
#ifdef D_JMTOT
        if 0=(i:=ascan(gri,{|x|x[1]=(sel)->jM}))
           aadd(gri,{(sel)->jM,D_ILOSC,wd arrVAT(vd)})
        else
           gri[i,2]+=D_ILOSC
           gri[i,3]+=wd
           addVAT(gri[i,4],vd)
        endif
#endif
        skip
     enddo
#else
  #ifndef A_WA
    #ifdef A_FA
if !zby_flag
    #endif
    #ifdef STANY
     indx_mat->(dbseek(main->nr_mag+main->index,.f.))
    #else
     indx_mat->(dbseek(main->index,.f.))
    #endif
    #ifdef A_FA
     EXEC {||sel:=i_lam(data),w+=D_WARTOSC,v+=D_VAT} rest while EvaldB(bkey)=txt .and. data<=do
else
     EXEC {||w+=D_WARTOSC,v+=D_VAT} rest while EvaldB(bkey)=txt .and. data<=do
endif
    #else
     EXEC {||sel:=i_lam(data),w+=D_WARTOSC} rest while EvaldB(bkey)=txt .and. data<=do
    #endif
  #else
    #ifdef A_FA
     EXEC {||w+=D_WARTOSC,v+=D_VAT} rest while EvaldB(bkey)=txt .and. data<=do
    #else
     EXEC {||w+=D_WARTOSC} rest while EvaldB(bkey)=txt .and. data<=do
    #endif
  #endif
#endif
  enddo
  WTOT+=W
#ifdef D_GRAM
  wat+=wai
  ?? str(wai,10,2)," kg   "
#else
#ifdef D_JMTOT
  if len(gri)=1
     ?? str(gri[1,2],10,ILDEC),' ',gri[1,1]+" "
  else
     ?? space(16)
  endif
#endif
#endif
   ?? TrAN(W,"@E ",A_ZAOKR,15)
#ifdef A_FA
  if zby_flag
       VTOT+=V
#ifdef A_CENVAT
       if pm=-1
         ?? " - "+TrAN(v,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(w-v,"@E ",A_ZAOKR,15)
       else
#endif
       ?? " + "+TrAN(v,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(w+v,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
  endif
#endif
?? ' ',txth
#ifdef D_JMTOT
#ifndef D_GRAM
  if len(gri)>1
#endif
     ? padr("   W TYM:",25),spec(HB_BCHAR(13))
     for i=1 to len(gri)
      if 0=(j:=ascan(grt,{|x|x[1]=gri[i,1]}))
          aadd(grt,gri[i])
      else
          grt[j,2]+=gri[i,2]
          grt[j,3]+=gri[i,3]
          addVAT(grt[j,4],gri[i,4])
      endif
       ? space(25)+str(gri[i,2],10,ILDEC),' ',gri[i,1],' ',TrAN(gri[i,3],"@E ",A_ZAOKR,15)
#ifdef A_FA
       if zby_flag
#ifdef A_CENVAT
       if pm=-1
          ?? " - "+TrAN(gri[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(gri[i,3]-gri[i,4],"@E ",A_ZAOKR,15)
       else
#endif
          ?? " + "+TrAN(gri[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(gri[i,3]+gri[i,4],"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
       endif
#endif
     next
#ifndef D_GRAM
  else
      i:=1
      if 0=(j:=ascan(grt,{|x|x[1]=gri[i,1]}))
          aadd(grt,gri[i])
      else
          grt[j,2]+=gri[i,2]
          grt[j,3]+=gri[i,3]
          addVAT(grt[j,4],gri[i,4])
      endif
  endif
#endif
#endif
ENDDO
if strona>0 .and. i_mul>1
  ? padr("RAZEM",25)
#ifdef D_GRAM
     ?? str(wat,10,2)," kg   "
#else
#ifdef D_JMTOT
  if len(grt)=1
     ?? str(grt[1,2],10,ILDEC),' ',grt[1,1]+" "
  else
     ?? space(16)
  endif
#endif
#endif
  ?? TrAN(Wtot,"@E ",A_ZAOKR,15)
#ifdef A_FA
  if zby_flag
#ifdef A_CENVAT
       if pm=-1
       ?? " - "+TrAN(vtot,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(wtot-vtot,"@E ",A_ZAOKR,15)
       else
#endif
       ?? " + "+TrAN(vtot,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(wtot+vtot,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
  endif
#endif
#ifdef D_JMTOT
#ifndef D_GRAM
  if len(grt)>1
#endif
     ? padr("   W TYM:",25),spec(HB_BCHAR(13))
     for i=1 to len(grt)
       ? space(25)+str(grt[i,2],10,ILDEC),' ',grt[i,1],' ',TrAN(grt[i,3],"@E ",A_ZAOKR,15)
#ifdef A_FA
       if zby_flag
#ifdef A_CENVAT
       if pm=-1
         ?? " - "+TrAN(grt[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(grt[i,3]-grt[i,4],"@E ",A_ZAOKR,15)
       else
#endif
         ?? " + "+TrAN(grt[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(grt[i,3]+grt[i,4],"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
       endif
#endif
     next
#ifndef D_GRAM
  endif
#endif
#endif
  ?? ccpi(4)
endif
********
ELSE
********
DFLAG:=TAK(hb_UTF8ToStr("CZY ROZBIJAĆ NA POJEDYNCZE DOKUMENTY"),23,,.F.,.F.)
#ifdef A_JMO
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
#define D_ILOUT(x,p) if(jm_o,if(x%p=0,str(x/p,6)+"    ",stuff(str(int(x/p)+x%p/1000,10,3),7,1,"r")),str(x,10,ILDEC))
#define jM (if(jm_o,jm_opcja,jm))
#else
#define D_ILOUT(x,p) str(x,10,ILDEC)
#ifdef A_JMALTTOT
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
#endif
#endif
  @ 8,0 CLEAR 

#ifdef D_JMTOT
  grt:={}
#endif
DO WHILE EvaldB(bkey)<=I_DO .AND. !EOF()

  TXT:=EvaldB(bkey)
  seek TXT+DTOS(od)
  IF EvaldB(bkey)+DTOS(DATA)>TXT+DTOS(do) .OR. EOF()
     IF EvaldB(bkey)=txt
        seek TXT+"Z"
     ENDIF
     LOOP
  ENDIF
#ifdef A_FA
  v:=0
#endif
  W:=0
#ifdef D_GRAM
  wai:=0
#endif
#ifdef D_JMTOT
  gri:={}
  IF prow()+5+2*len(grt)>P_ROWN
#else
  IF prow()+5>P_ROWN
#endif
      IF STRONA>0
         specout(ccpi(4)+HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
         setprc(0,0)
      else
         print()
      ENDIF
    ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
    ?
    ? _dnazwa," OD DNIA ",od," DO ",do,"."
/*
    if DatY->d_z_mies1>=do
      ? "PO ZAMKNIĘCIU MIESIĄCA"
    ELSE
      ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
    ENDIF
*/
    ?
#ifdef A_FA
    IF ZBY_FLAG
#ifdef A_CENVAT
        ?? if(pm=1,"PO CENACH ZAKUPU NETTO","PO CENACH ZBYTU BRUTTO")
#else
        ?? "PO CENACH ZAKUPU/ZBYTU"
#endif
    ELSE
        ?? "PO CENACH EWIDENCYJNYCH"
    ENDIF
    ?
#endif
    ?? "Strona"+str(++strona,3)
    eval(_dhead)
    ?
#ifdef D_GRAM
#define HEADER_1 hb_UTF8ToStr("DOKUMENT   | cena  |"+if(pm=1,"        PRZYCHÓD          ","        ROZCHÓD           ")+"|  WAGA    |  KOD i NAZWA MATERIAŁU")
#define HEADER_2 hb_UTF8ToStr("  ile |   śr. cena |  ILOŚĆ   |JEDN| "+WANAZ+"  |   kg     |")
#else
#define HEADER_1 hb_UTF8ToStr("DOKUMENT   | cena  |"+if(pm=1,"        PRZYCHÓD          ","        ROZCHÓD           ")+"|  KOD i NAZWA MATERIAŁU")
#define HEADER_2 hb_UTF8ToStr("  ile |   śr. cena |  ILOŚĆ   |JEDN| "+WANAZ+"  |")
#endif
#define HEADER_10 hb_UTF8ToStr("DOKUMENT        |"+if(pm=1,"    PRZYCHÓD   ","    ROZCHÓD    ")+"|  KOD i NAZWA MATERIAŁU")
#define HEADER_20 hb_UTF8ToStr("            ile |  ILOŚĆ   |JEDN|")
if wa_flag
    ? ccpi(5)
    ? padr(HEADER_1,96)
    ?? speC(P_UON)
    ? padr(HEADER_2,96)
else
    ? padr(HEADER_10,96)
    ?? speC(P_UON)
    ? padr(HEADER_20,96)
endif
    ?? speC(P_UOFF)

  ENDIF

  ? ccpi(4)
  ? "GRUPA: ",TranR(left(EvaldB(bkey),i_gr),bpic)
  //if i_gr>=len(nr_zlec)
  txth:=eval(_dhead2)
  if valtype(txth)$'MC'
     ?? txth
  endif
  //endif
  ?
  if wa_flag
    specout(ccpi(5))
  endif
  multiflag:=ir:=iw:=0
#ifdef A_FA
  iv:=0
#endif
  do while txt=left(EvaldB(bkey),i_gr) .and. EvaldB(bkey)<=i_do .and. !EOF()
     IF EvaldB(bkey)+DTOS(DATA)>TXT+DTOS(do)
        IF EvaldB(bkey)=txt
           seek TXT+"Z"
        ENDIF
        IF txt=left(EvaldB(bkey),i_gr)
           txt:=EvaldB(bkey)
           seek TXT+DTOS(od)
        ENDIF
        multiflag:=ir:=iw:=0
#ifdef A_FA
        iv:=0
#endif
        LOOP
     ENDIF
     ++multiflag
     IF prow()+1>P_ROWN
        ?? speC(HB_BCHAR(13)+P_UON+SPACE(96))
        ?? SPEC(P_UOFF)
if wa_flag
        ? "DO PRZENIESIENIA   |          |"+TrAN(W,"@E ",A_ZAOKR,15)
#ifdef D_GRAM
        ?? "|"+str(wai,10,2)
#endif
#ifdef A_FA
        if zby_flag
#ifdef A_CENVAT
           if pm=-1
              ?? " -"+TrAN(V,"@E ",A_ZAOKR,15)+" VAT ="+TrAN(W-V,"@E ",A_ZAOKR,15)
           else
#endif
              ?? " +"+TrAN(V,"@E ",A_ZAOKR,15)+" VAT ="+TrAN(W+V,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
           endif
#endif
        endif
#endif
endif
        specout(ccpi(4)+HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
        setprc(0,0)
        ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
        ?
    ? _dnazwa," OD DNIA ",od," DO ",do,"."
/*
    if DatY->d_z_mies1>=do
      ? "PO ZAMKNIĘCIU MIESIĄCA"
    ELSE
      ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
    ENDIF
*/
    ?
#ifdef A_FA
    IF ZBY_FLAG
#ifdef A_CENVAT
        ?? if(pm=1,"PO CENACH ZAKUPU NETTO"," PO CENACH ZBYTU BRUTTO")
#else
        ?? "PO CENACH ZAKUPU/ZBYTU"
#endif
    ELSE
        ?? "PO CENACH EWIDENCYJNYCH"
    ENDIF
    ?
#endif
    ?? "Strona"+str(++strona,3)
    eval(_dhead)
    ?
if wa_flag
    ? ccpi(5)
    ? padr(HEADER_1,96)
    ?? speC(P_UON)
    ? padr(HEADER_2,96)
      ? " Z PRZENIESIENIA "+padr(TranR(EvaldB(bkey),bpic),13)+"|"+TrAN(W,"@E ",A_ZAOKR,15)
#ifdef D_GRAM
        ?? "|"+str(wai,10,2)
#endif
#ifdef A_FA
        if zby_flag
#ifdef A_CENVAT
           if pm=-1
              ?? " -"+TrAN(V,"@E ",A_ZAOKR,15)+" VAT ="+TrAN(W-V,"@E ",A_ZAOKR,15)
           else
#endif
              ?? " +"+TrAN(V,"@E ",A_ZAOKR,15)+" VAT ="+TrAN(W+V,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
     endif
#endif
        endif
#endif
      ?? speC(space(96-pcol()))
      ?? spec(P_UOFF)
else
    ?
    ? padr(HEADER_10,96)
    ? speC(P_UON)+padr(HEADER_20,96)
    ?? speC(P_UOFF)
endif
    ENDIF
#ifdef STANY
      INDX_MAT->(DBSEEK(main->nr_mag+main->index,.F.))
#else
      INDX_MAT->(DBSEEK(MAIN->INDEX,.F.))
#endif
      j:=INDX_MAT->(recno())
      SEL:=I_LAM(MAIN->DATA)
#ifdef A_PLUDRY
      nz:=MAIN->nr_zlec
#endif
    IF DFLAG
    if wa_flag
      W+=wd:=D_WARTOSC
      addVAT(V,vd:=D_VAT)
      addVAT(iv,vd)
      iw+=wd
    else
      wd:=0
      vd:=0
    endif
      ir+=D_ILOSC
#ifdef D_JMTOT
      if 0=(i:=ascan(gri,{|x|x[1]=(sel)->jM}))
         aadd(gri,{(sel)->jM,D_ILOSC,wd arrVAT(vd)})
      else
          gri[i,2]+=D_ILOSC
          gri[i,3]+=wd
          addVAT(gri[i,4],vd)
      endif
#endif
if wa_flag
      ? ccpi(7)+SMB_DOW+NR_DOWODU+"/"+str(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"+strpic(D_CENA,10,A_ZAOKR,"@E ",.t.),speC(ccpi(5)+HB_BCHAR(13)+space(19)),"|"
      setprc(prow(),20)
else
      ? SMB_DOW+NR_DOWODU+"/"+str(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"
endif
      ?? D_ILOUT(D_ILOSC,(sel)->przel)+"|"+(SEL)->jM+"|"
if wa_flag
      ?? TrAN(wd,"@EZ ",A_ZAOKR,10)+"|"
endif
#ifdef D_GRAM
      ?? str(D_WAGA,10,2)+"|"
      wai+=D_WAGA
#endif
#ifdef A_PLUDRY
      nz:=nr_zlec
#endif
      skip
      INDX_MAT->(dbgoto(j))
       IF EvaldB(bkey)+DTOS(DATA)>TXT+DTOS(do) .or. eof() .or. (sel)->data_popr#(i_lam(data))->data_popr
         if multiflag>1
if wa_flag
            ? STR(MULTIFLAG,6)+"|"+;
              TrAN(iw/ir,"@EZ ",A_ZAOKR,13)+"|"+;
              D_ILOUT(ir,(sel)->przel)+"|"+;
              TrAN(IW,"@EZ ",A_ZAOKR,15)+"|"
#ifdef D_GRAM
            ?? str(IR*(sel)->gram/1000,10,2)+"|"
#endif
else
            ? STR(MULTIFLAG,16)+"|"+;
              D_ILOUT(ir,(sel)->przel)+"|"+(SEL)->jM+"|"
endif
         endif
       elseIF prow()<P_ROWN
         loop
       ENDIF
    ELSE
#ifdef A_FA
      SUM D_ILOSC,D_WARTOSC,D_VAT,1 TO IR,IW,iv,MULTIFLAG rest WHILE EvaldB(bkey)+DTOS(DATA)<=TXT+DTOS(do) .and. (sel)->data_popr=(i_lam(data))->data_popr
#else
      SUM D_ILOSC,D_WARTOSC,1 TO IR,IW,MULTIFLAG rest WHILE EvaldB(bkey)+DTOS(DATA)<=TXT+DTOS(do) .and. (sel)->data_popr=(i_lam(data))->data_popr
#endif
      INDX_MAT->(dbgoto(j))
if wa_flag
        W+=IW
        addVAT(v,iv)
else
        iw:=iv:=0
endif
#ifdef D_JMTOT
      if 0=(i:=ascan(gri,{|x|x[1]=(sel)->jM}))
          aadd(gri,{(sel)->jM,ir,iw arrVAT(iv)})
      else
          gri[i,2]+=ir
          gri[i,3]+=iw
          addVAT(gri[i,4],iv)
      endif
#endif
if wa_flag
      ?  STR(MULTIFLAG,6)+"|"+;
         TrAN(iw/ir,"@EZ ",A_ZAOKR,13)+"|"+;
         D_ILOUT(ir,(sel)->przel)+"|"+(SEL)->jM+"|"+;
         TrAN(IW,"@EZ ",A_ZAOKR,10)+"|"
#ifdef D_GRAM
      ?? str(IR*(sel)->gram/1000,10,2)+"|"
      wai+=IR*(sel)->gram/1000
#endif
else
      ?  STR(MULTIFLAG,16)+"|"+;
         D_ILOUT(ir,(sel)->przel)+"|"+(SEL)->jM+"|"
endif
    ENDIF
      ?? Tran((SEL)->INDEX,"@R "+ INDEXPIC )+"|"
#ifdef A_PLUDRY
      ?? subs(nz,3,4)+'|'
#endif
      ?? CPAD((sel)->NAZWA,(P_COLN-pcol())*ccpi()*.1,,0)
  enddo
  wtot+=w
  ?? speC(HB_BCHAR(13)+P_UON+SPACE(96))
  ?? SPEC(P_UOFF)
  ? padr("RAZEM GRUPA "+TranR(left(txt,I_gr),bpic),D_POS-1)+":"
  ++i_mul
#ifdef D_GRAM
  wat+=wai
  ?? str(wai,10,2)," kg   "
#else
#ifdef D_JMTOT
  if len(gri)=1
     ?? str(gri[1,2],10,ILDEC),' ',gri[1,1]+" "
  else
     ?? space(16)
  endif
#endif
#endif
if wa_flag
   ?? TrAN(W,"@E ",A_ZAOKR,15)
#ifdef A_FA
  if zby_flag
       VTOT+=V
#ifdef A_CENVAT
       if pm=-1
       ?? " - "+TrAN(v,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(w-v,"@E ",A_ZAOKR,15)
       else
#endif
       ?? " + "+TrAN(v,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(w+v,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
  endif
#endif
endif
#ifdef D_JMTOT
#ifndef D_GRAM
  if len(gri)>1
#endif
     ? padr("   W TYM:",D_POS),spec(HB_BCHAR(13))
     for i=1 to len(gri)
      if 0=(j:=ascan(grt,{|x|x[1]=gri[i,1]}))
          aadd(grt,gri[i])
      else
          grt[j,2]+=gri[i,2]
          grt[j,3]+=gri[i,3]
          addVAT(grt[j,4],gri[i,4])
      endif
if wa_flag
       ? space(D_POS)+str(gri[i,2],10,ILDEC),' ',gri[i,1],' ',TrAN(gri[i,3],"@E ",A_ZAOKR,15)
#ifdef A_FA
       if zby_flag
#ifdef A_CENVAT
       if pm=-1
          ?? " - "+TrAN(gri[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(gri[i,3]-gri[i,4],"@E ",A_ZAOKR,15)
       else
#endif
          ?? " + "+TrAN(gri[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(gri[i,3]+gri[i,4],"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
       endif
#endif
else
       ? space(D_POS)+str(gri[i,2],10,ILDEC),' ',gri[i,1]
endif
     next
#ifndef D_GRAM
  else
      i:=1
      if 0=(j:=ascan(grt,{|x|x[1]=gri[i,1]}))
          aadd(grt,gri[i])
      else
          grt[j,2]+=gri[i,2]
          grt[j,3]+=gri[i,3]
          addVAT(grt[j,4],gri[i,4])
      endif
  endif
#endif
#endif
  ?? ccpi(4)

ENDDO
if strona>0 .and. i_mul>1
if wa_flag
  ? ccpi(5)
else
  ?
endif
  ? padr("RAZEM",D_POS)
#ifdef D_GRAM
     ?? str(wat,10,2)+" kg   "
#else
#ifdef D_JMTOT
  if len(grt)=1
     ?? str(grt[1,2],10,ILDEC),' ',grt[1,1]+" "
  else
     ?? space(16)
  endif
#endif
#endif
if wa_flag
  ?? TrAN(Wtot,"@E ",A_ZAOKR,15)
#ifdef A_FA
  if zby_flag
#ifdef A_CENVAT
       if pm=-1
       ?? " - "+TrAN(vtot,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(wtot-vtot,"@E ",A_ZAOKR,15)
       else
#endif
       ?? " + "+TrAN(vtot,"@E ",A_ZAOKR,15)+" VAT = "+TrAN(wtot+vtot,"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
  endif
#endif
endif
#ifdef D_JMTOT
#ifndef D_GRAM
  if len(grt)>1
#endif
     ? padr("   W TYM:",D_POS),spec(HB_BCHAR(13))
     for i=1 to len(grt)
if wa_flag
       ? space(D_POS)+str(grt[i,2],10,ILDEC),' ',grt[i,1],' ',TrAN(grt[i,3],"@E ",A_ZAOKR,15)
#ifdef A_FA
       if zby_flag
#ifdef A_CENVAT
       if pm=-1
         ?? " - "+TrAN(grt[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(grt[i,3]-grt[i,4],"@E ",A_ZAOKR,15)
       else
#endif
         ?? " + "+TrAN(grt[i,4],"@E ",A_ZAOKR,15)+" VAT = "+TrAN(grt[i,3]+grt[i,4],"@E ",A_ZAOKR,15)
#ifdef A_CENVAT
       endif
#endif
       endif
#endif
else
       ? space(D_POS)+str(grt[i,2],10,ILDEC),' ',grt[i,1]
endif
     next
#ifndef D_GRAM
  endif
#endif
#endif
  ?? ccpi(4)
endif
ENDIF
return
#undef HEADER_1
#undef HEADER_2
#undef D_CENA
#undef D_WAGA
#undef D_WARTOSC
#undef D_ILOSC
#undef D_POS
#undef D_VAT
#undef arrVAT
#undef addVAT
#undef jM
**************
#ifdef A_FA
************
static PROCEDURE W_REJ()

MEMVAR GETLIST,NAZWA_MAG,dok_par,dokumenty
LOCAL OD,DO,TXT,i,wd,wp,wc,dok_i,wv,W,v,was,j,k,wn,wat,wnt:=0,wag,wvt:=0,wpt:=0,wct:=0,dok_sp,wdt:=0,mag_poz,mag_biez,cl,cc,kl,x,lp
local ok

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)
  dok_sp:=""
#ifdef A_SUBDOK
#define D_SUBKEY +SUB_DOK
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|y:=left(y,4),if(dok_par[i,b,1]="F".and.!y$dok_sp,dok_sp+=y+" ",)})})
#else
#define D_SUBKEY
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|if(dok_par[i,b,1]="F".and.!left(y,2)$dok_sp,dok_sp+=left(y,2)+" ",)})})
#endif
  dok_sp+=space(40)

@ 12,20 SAY "REJESTR SPRZEDAŻY" UNICODE

@ 14,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

  @ 16,10 SAY 'DLA DOKUMENTÓW:' UNICODE GET dok_sp PICTURE "@K!S60"
  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
READ
if readkey()=27
  break
endif
dok_sp:=upper(alltrim(dok_sp))+'  '
    set order to TAG MAIN_NRK
#ifndef STANY
  SET RELATION TO INDEX INTO INDX_MAT
#else
  SET RELATION TO NR_MAG+INDEX INTO INDX_MAT
#endif
  SELECT DM

#ifdef A_MM
#ifdef A_SUBDOK
  #define mkdmbIs() mkindex('DM_BIS','SMB_DOW+SUB_DOK+DTOS(DATA)')
#else
  #define mkdmbIs() mkindex('DM_BIS','SMB_DOW+DTOS(DATA)')
#endif
#else
#ifdef A_SUBDOK
  #define mkdmbIs() mkindex('DM_BIS','NR_MAG+SMB_DOW+SUB_DOK+DTOS(DATA)')
#else
  #define mkdmbIs() mkindex('DM_BIS','NR_MAG+SMB_DOW+DTOS(DATA)')
#endif
#endif

  mkdmbIs()

  SET FILTER TO POZYCJA>D_LP0
  go top
  if eof()
     return
  endif
#ifdef A_MM
  mag_poz:=1
  mag_biez:=left(magazyny[mag_poz],2)
#else
  mag_biez:=nr_mag
  mag_poz:=ascan(magazyny,mag_biez)
#endif
  wag:={}
  ok:=EvAlDb('{||'+IndexkeY(0)+'}')
  i:=0
IF TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.T.,.T.)
  @ 8,0 CLEAR
  DO WHILE .t.
    if mag_poz=0
       j:=errornew()
       j:severity:=2
       j:candefault:=.t.
       j:description:=hb_UTF8ToStr('Nieprawidłowy numer magazynu')
       j:filename:='DM.DBF'
       j:operation:='RECNO'
       j:args:={recno()}
       if !eval(errorblock(),j)
          skip
          mag_biez:=nr_mag
          mag_poz:=ascan(magazyny,mag_biez)
          loop
       endif
    endif
    i:=ASCAN(DOK_PAR[MAG_POZ],{|X|X[1]="F"},i+1)
    IF i=0
#ifdef A_MM
       EXIT
#else
       if mag_biez=D_MM
          seek D_MM + HB_UCHAR(0x00A0)
       endif
       if eof()
          EXIT
       endif
       mag_biez:=nr_mag
       mag_poz:=ascan(magazyny,mag_biez)
       loop
#endif
    ENDIF
#ifdef A_SUBDOK
    txt:=left(dokumenty[mag_poz,i],4)
#else
    txt:=left(dokumenty[mag_poz,i],2)
#endif
    if !txt$dok_sp
       loop
    endif
#ifndef A_MM
    txt:=mag_biez+txt
#endif
    SEEK TXT+DTOS(OD)
    IF eof() .or. EvaldB(ok)>txt + dtos(DO)
      LOOP
    ENDIF

    IF prow()+12>P_ROWN
      IF STRONA>0
         ?? speC(HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
         setprc(0,0)
      else
         print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)+" dnia "+dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE SYNTETYCZNE SPRZEDAŻY ZA OKRES OD "),OD," DO ",DO,"."
      ?
      ? "Strona"+str(++strona,3)
    ENDIF
    ?
    ?
    j:=recno()
#ifdef A_MM
    ? "DOKUMENTY ",txt,"   RAZEM : "
#else
    ? magazyny[mag_poz]
#ifdef A_SUBDOK
    ? "DOKUMENTY ",TranR(txt,"##/XXXX")," RAZEM : "
#else
    ? "DOKUMENTY ",TranR(txt,"##/XX")," RAZEM : "
#endif
#endif
    sum wartoSC,przelewem,czekiem,warT_vaT to wd,wp,wc,wv while !eof().and. EvaldB(ok)<=txt + dtos(DO)
    wat:={}
    if dok_par[mag_poz,i,2]$"UV"
#ifdef A_CENVAT
      wn:=wd-wv
#else
      wn:=wd
      wd+=wv
#endif
    go j
    do while !eof().and.EvaldB(ok)<=txt + dtos(DO)
      was:={}
      aeval(stawkizby,{|x|if(val(x)#0,aadd(was,{x,0,bin2D(binfieldget(D_NVAT+ltrim(x)))}),)})
      SELECT MAIN
      SEEK dm->(KEY_DOK+nr_dowodu)
      WHILE dm->(KEY_DOK+nr_dowodu)=KEY_DOK+nr_dowodu
#ifndef A_DF
#define WDFGR(i,c,v,d) WGR(i,c,v,d)
#endif
         w:=WDFGR(-ilosc_f,cena,val(proc_vat),dok_df)
         j:=ascan(was,{|x|x[1]=proc_vat})
         if j=0
           if ascan(stawkizby,proc_vat)=0
              ? hb_UTF8ToStr("BŁĄD STAWKI VAT DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
              ?
           endif
           aadd(was,{proc_vat,w,0})
         else
           was[j,2]+=w
         endif
         skip
      enddo
      SELECT DM
#ifdef A_WALUTA
         if dok_wal
            w:=rat(' ',trim(DM->nr_spec))
            w:=round(val(subs(DM->nr_spec,w))*10000,0)
            aeval(was,{|v|v[2]:=(v[2]*w)/10000})
         endif
#endif
#ifdef A_FIXDOK
//#ifdef A_NVAT
#ifdef A_DF
         if dok_df //brutto
            aeval(was,{|v|v[3]:=v[2]*val(v[1])/(100+val(v[1]))})
         else //netto
            aeval(was,{|v|v[3]:=v[2]*val(v[1])/100})
         endif
#else
#ifdef A_CENVAT
         aeval(was,{|v|v[3]:=v[2]*val(v[1])/(100+val(v[1]))})
#else
         aeval(was,{|v|v[3]:=v[2]*val(v[1])/100})
#endif
#endif
         lock
         aeval(was,{|x|if(val(x[1])#0,field2biN(fieldpos(D_NVAT+ltrim(x[1])),x[3]),)})
         unlock
//#endif
#endif
      w:=v:=0
      for k=1 to len(was)

         was[k,3]:=ROUND(was[k,3],0)/100
#ifdef A_DF
         was[k,2]:=ROUND(was[k,2],0)/100
        if dok_df //brutto
         was[k,2]-=was[k,3]
        endif
#else
#ifdef A_CENVAT
         was[k,2]:=ROUND(was[k,2],0)/100-was[k,3]
#else
         was[k,2]:=ROUND(was[k,2],0)/100
#endif
#endif
         w+=was[k,2]
         v+=was[k,3]
         j:=ascan(wat,{|x|x[1]=was[k,1]})
         if j=0
            aadd(wat,aclone(was[k]))
         else
            wat[j,2]+=was[k,2]
            wat[j,3]+=was[k,3]
         endif
      next
#ifdef A_CENVAT
      w+=v
#endif
      if ROUND(w-wartosc,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wartosc:=w
                unlock
#else
#ifndef A_PLUDRY
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
#endif
      endif
      if ROUND(v-warT_vaT,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wart_vat:=v
                unlock
#else
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI PODATKU DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
      endif
      skip
    enddo
    else
      wv:=0
      wn:=0
    endif
    ?? ltrim(TrAN(Wd,"@E ",A_ZAOKR,15))
    ?  hb_UTF8ToStr("SPOSOB ZAPŁATY:")
      j:=0
      if wd-wp-wc#0
      ?  hb_UTF8ToStr("                  gotówką ")+TrAN(wd-wp-wc,"@E ",A_ZAOKR,15)
      ++j
      endif
      if wp#0
      ?  "             +  przelewem "+TrAN(wp,"@E ",A_ZAOKR,15)
      ++j
      endif
      if wc#0
      ?  hb_UTF8ToStr("             +      kartą ")+TrAN(wc,"@E ",A_ZAOKR,15)
      ++j
      endif
      if j>1
      ?  "             ----------------------------------"
      ?  "             =      RAZEM "+TrAN(wd,"@E ",A_ZAOKR,15)
      endif
      ?
      w:=v:=0
      for j:=1 to len(wat)
         w+=wat[j,2]
         v+=wat[j,3]
         ?  "STAWKA "+wat[j,1]+". : "+TrAN(wat[j,2],"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wat[j,3],"@E ",A_ZAOKR,15)+" = "+TrAN(wat[j,2]+wat[j,3],"@E ",A_ZAOKR,15)
      next
      if j>2
      ?  "---------------------------------------------------------------------------"
      ?  "       NETTO "+TrAN(wn,"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wv,"@E ",A_ZAOKR,15)+" = "+TrAN(wn+wv,"@E ",A_ZAOKR,15)
      endif
      wdt+=wd
      wnt+=wn
      wvt+=wv
      wpt+=wp
      wct+=wc
      for k=1 to len(wat)
             j:=ascan(wag,{|x|x[1]=wat[k,1]})
             if j=0
               aadd(wag,aclone(wat[k]))
             else
               wag[j,2]+=wat[k,2]
               wag[j,3]+=wat[k,3]
             endif
      next
  ENDDO
    IF STRONA>0
      IF prow()+12>P_ROWN
      ?? speC(HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
      setprc(0,0)
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE SYNTETYCZNE DOKUMENTÓW SPRZEDAŻY ZA OKRES OD "),OD," DO ",DO,"."
      ?
      ? "Strona"+str(++strona,3)
      ENDIF
      ?
      ?
      ? "ZESTAWIENIE RAZEM: "+ltrim(TrAN(Wdt,"@E ",A_ZAOKR,15))
      ?  hb_UTF8ToStr("SPOSÓB ZAPŁATY:")
      j:=0
      if wdt-wpt-wct#0
      ?  hb_UTF8ToStr("                  gotówką ")+TrAN(wdt-wpt-wct,"@E ",A_ZAOKR,15)
      ++j
      endif
      if wpt#0
      ?  "             +  przelewem "+TrAN(wpt,"@E ",A_ZAOKR,15)
      ++j
      endif
      if wct#0
      ?  hb_UTF8ToStr("             +      kartą ")+TrAN(wct,"@E ",A_ZAOKR,15)
      ++j
      endif
      if j>1
      ?  "             ----------------------------------"
      ?  "             =      RAZEM "+TrAN(wdt,"@E ",A_ZAOKR,15)
      endif
      ?
      for j:=1 to len(wag)
      ?  "STAWKA "+wag[j,1]+". : "+TrAN(wag[j,2],"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wag[j,3],"@E ",A_ZAOKR,15)+" = "+TrAN(wag[j,2]+wag[j,3],"@E ",A_ZAOKR,15)
      next
      if j>2
      ?  "---------------------------------------------------------------------------"
      ?  "       NETTO "+TrAN(wnt,"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wvt,"@E ",A_ZAOKR,15)+" = "+TrAN(wnt+wvt,"@E ",A_ZAOKR,15)
      endif
   ENDIF
******
ELSE
*****
#ifdef A_SUBDOK
#define D_SUBDOK if(sub_dok=="  ",SMB_DOW,SUB_DOK)+NR_DOWODU+"/"+str(year(data)%100,2)
#else
#define D_SUBDOK SMB_DOW+NR_DOWODU+"/"+str(year(data)%100,2)
#endif

  @ 8,0 CLEAR

#ifdef A_15CALI
 #define D_KL +space(kl)
 #ifdef A_PCL
  #define D_COLN 110
  #define D_CPI 2
 #else
  #define D_COLN 130
  #ifdef A_17CPI
   #define D_CPI 1.7
  #else
   #define D_CPI 2
  #endif
 #endif
#else
 #define D_KL
 #define D_COLN P_COLN
 #define D_CPI 2
#endif

  cc:=len(stawkizby)+3
  aeval(stawkizby,{|x|if(val(x)#0,++cc,)})
  cl:=13
#ifdef A_15CALI
  do while 30+35+cc*cl>D_COLN*D_CPI .and. cl>11
     --cl
  enddo
#endif
  do while 35+cc*cl>D_COLN*D_CPI
     --cl
  enddo
#ifdef A_15CALI
  kl:=D_COLN*D_CPI-35-cc*cl
#endif
  DO WHILE .t.

    if mag_poz=0
#ifdef A_MM
       EXIT
#else
       j:=errornew()
       j:severity:=2
       j:candefault:=.t.
       j:description:=hb_UTF8ToStr('Nieprawidłowy numer magazynu')
       j:filename:='DM.DBF'
       j:operation:='RECNO'
       j:args:={recno()}
       if !eval(errorblock(),j)
          skip
          mag_biez:=nr_mag
          mag_poz:=ascan(magazyny,mag_biez)
          loop
       endif
#endif
    endif
    i:=ASCAN(DOK_PAR[MAG_POZ],{|X|X[1]="F"},i+1)
    IF i=0
#ifdef A_MM
       EXIT
#else
       if mag_biez=D_MM
          seek mag_biez + HB_UCHAR(0x00A0)
       endif
       if eof()
          EXIT
       endif
       mag_biez:=nr_mag
       mag_poz:=ascan(magazyny,mag_biez)
       loop
#endif
    ENDIF
#ifdef A_SUBDOK
    txt:=left(dokumenty[mag_poz,i],4)
#else
    txt:=left(dokumenty[mag_poz,i],2)
#endif
    if !txt$dok_sp
       loop
    endif
#ifndef A_MM
    txt:=mag_biez+txt
#endif
    SEEK TXT+DTOS(OD)
    IF eof() .or. EvaldB(ok)>txt + dtos(DO)
      LOOP
    ENDIF
    wd:=WN:=wv:=lp:=0
    wat:={{stawkizby[1],0,0}}
    dok_i:=""#dok_par[mag_poz,i,3]
    DO WHILE !eof().and.EvaldB(ok)<=txt + dtos(DO)
      if prow()+10>P_ROWN
        IF STRONA>0
         ?? speC(HB_BCHAR(0x0D)+HB_BCHAR(0x0C))
         setprc(0,0)
        else
#ifdef D_HWPRN
         memvar->landscape:=.t.
#endif
         print()
#ifdef A_PCL
         memvar->landscape:=.f.
         ?? speC(eval(P_LAND,.t.)) //+"&l1O")
#endif
        ENDIF
#ifdef A_15CALI
        ?? padr(firma_n+", "+firma_a,D_COLN-16)," dnia ",dtoc(DatE())
#else
        ?? padr(firma_n,D_COLN-16)," dnia ",dtoc(DatE())
#endif
        ?
        ? hb_UTF8ToStr("REJESTR SPRZEDAŻY ZA OKRES OD "),OD," DO ",DO,"."
/*
        if DatY->d_z_mies1>=do
          ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
          ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
*/
        ?
        ? "Strona"+str(++strona,3)
      endif
      ?
#ifndef A_MM
      ? magazyny[mag_poz]
#endif
      ? "DOKUMENTY ",LEFT(txt,2)+"/"+DOKUMENTY[MAG_POZ,i]
      ?
#ifdef A_17CPI
        specout(ccpi(7)+P_UON)
#else
        specout(ccpi(8)+P_UON)
#endif
      ? "Lp.| Dokument |Data |Identyfikator"
#ifdef A_15CALI
      if kl>cl
         ?? "|"+padc("Kontrahent",kl-1)
      else
         ?? pad("|",kl)
      endif
#endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         ?? "|"+padc(stawkizby[k],cl-1)
         ++k
      enddo
      ?? "|"+padc("BRUTTO",cl-1)
      for k:=k to len(stawkizby)
          j:=stawkizby[k]
          if val(j)=0
             ?? "|"+padc(j,cl-1)
          else
             ?? "|"+padc("netto "+j+"%",cl-1)+"|"+padc("vat "+j+"%",cl-1)
          endif
      next k
      ?? "|"+padc("NETTO",cl-1)+"|"+padc("VAT",cl-1)
      if wd#0 .or. wv#0
#ifdef A_MM
        ? "    DOKUMENTY ",txt," Z PRZENIESIENIA:" D_KL
#else
#ifdef A_SUBDOK
        ? "    DOKUMENTY ",TranR(txt,"##/XXXX")," Z PRZENIES.:" D_KL
#else
        ? "    DOKUMENTY ",TranR(txt,"##/XX")," Z PRZENIESIEN:" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawkizby[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawkizby)
          j:=ascan(wat,{|x|x[1]=stawkizby[k]})
          if j=0
             ?? pad("|",cl*if(val(stawkizby[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawkizby[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ENDIF
      specout(P_UOFF)
      do while !eof().and.EvaldB(ok)<=txt + dtos(DO) .and. prow()+3<P_ROWN
#ifdef A_15CALI
            ? str(++lp,3)+"|"+D_SUBDOK+"|"+DTOV(DATA)
            if dok_i .and. FIRMY->(dbseek(DM->(D_KH),.f.))
#ifdef A_FFULL
               j:=FIRMY->(if(len(trim(longname))>kl-15,nazwa,longname))
               k:=min(len(trim(j)),kl-15)
               ?? "|"+FIRMY->ident +"|"+pad(j,k)+padl(trim(subs(FIRMY->adres,7)),kl-1-k)
#else
               k:=min(len(trim(FIRMY->nazwa)),kl-15)
               ?? "|"+FIRMY->ident +"|"+pad(FIRMY->nazwa,k)+padl(trim(subs(FIRMY->adres,7)),kl-1-k)
#endif
            else
               ?? "|"+pad(nr_faktury,13) +"|"+pad(dm->dost_odb,kl-1)
            endif

#else
            specout(P_UOFF)
            ? str(++lp,3)+"|"+D_SUBDOK+"|"+DTOV(DATA)
            if dok_i .and. FIRMY->(dbseek(DM->(D_KH),.f.))
               ?? "|"+FIRMY->ident
#ifdef A_FFULL
               ?? "|"+padr(firmy->longname,70)+padr(firmy->adres,50)
#else
               ?? "|"+padr(firmy->nazwa,70)+padr(firmy->adres,50)
#endif
            else
               ?? "|"+pad(nr_faktury,13)+"|"+left(dm->dost_odb,120)
            endif
            specout(P_UON)
            ? space(34)
#endif

            if dok_par[mag_poz,i,2]="Z"
              wd+=wartosc
              k:=1
              do while k<=len(stawkizby) .and. stawkizby[k]>"9"
                if k=1
                   wat[1,2]+=wartoSC
                   ?? "|"+strpic(wartosc,cl-1,A_ZAOKR,"@E ")
                else
                   ?? pad("|",cl)
                endif
                ++k
              enddo
              ?? "|"+strpic(wartoSC,cl-1,A_ZAOKR,"@E ")+"|"
              SKIP
              loop
            endif
          was:={}
          aeval(stawkizby,{|x|if(val(x)#0,aadd(was,{x,0,bin2D(binfieldget(D_NVAT+ltrim(x)))}),)})
          SELECT MAIN
          SEEK dm->(KEY_DOK+nr_dowodu)
          WHILE dm->(KEY_DOK+nr_dowodu)=KEY_DOK+nr_dowodu
             w:=WDFGR(-ilosc_f,cena,val(proc_vat),dok_df)
             j:=ascan(was,{|x|x[1]=proc_vat})
             if j=0
               if ascan(stawkizby,proc_vat)=0
                ? hb_UTF8ToStr("BŁĄD STAWKI VAT DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
               endif
               aadd(was,{proc_vat,w,0})
             else
               was[j,2]+=w
             endif
             skip
          enddo
          select dm
#ifdef A_WALUTA
         if dok_wal
            w:=rat(' ',trim(DM->nr_spec))
            w:=round(val(subs(DM->nr_spec,w))*10000,0)
            aeval(was,{|v|v[2]:=(v[2]*w)/10000})
         endif
#endif
#ifdef A_FIXDOK
//#ifdef A_NVAT
#ifdef A_DF
         if dok_df //brutto
            aeval(was,{|v|v[3]:=v[2]*val(v[1])/(100+val(v[1]))})
         else //netto
            aeval(was,{|v|v[3]:=v[2]*val(v[1])/100})
         endif
#else
#ifdef A_CENVAT
         aeval(was,{|v|v[3]:=v[2]*val(v[1])/(100+val(v[1]))})
#else
         aeval(was,{|v|v[3]:=v[2]*val(v[1])/100})
#endif
#endif
         lock
         aeval(was,{|x|if(val(x[1])#0,field2biN(fieldpos(D_NVAT+ltrim(x[1])),x[3]),)})
         unlock
//#endif
#endif
          w:=v:=0
          for k=1 to len(was)
         was[k,3]:=ROUND(was[k,3],0)/100
#ifdef A_DF
         was[k,2]:=ROUND(was[k,2],0)/100
        if dok_df //brutto
         was[k,2]-=was[k,3]
        endif
#else
#ifdef A_CENVAT
         was[k,2]:=ROUND(was[k,2],0)/100-was[k,3]
#else
         was[k,2]:=ROUND(was[k,2],0)/100
#endif
#endif
             w+=was[k,2]
             v+=was[k,3]
             j:=ascan(wat,{|x|x[1]=was[k,1]})
             if j=0
               aadd(wat,aclone(was[k]))
             else
               wat[j,2]+=was[k,2]
               wat[j,3]+=was[k,3]
             endif
             j:=ascan(waG,{|x|x[1]=was[k,1]})
             if j=0
               aadd(waG,aclone(was[k]))
             else
               waG[j,2]+=was[k,2]
               waG[j,3]+=was[k,3]
             endif
          next
#ifdef A_CENVAT
      w+=v
#endif
      if ROUND(w-wartosc,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wartosc:=w
                unlock
#else
#ifndef A_PLUDRY
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
#endif
      endif
      if ROUND(v-warT_vaT,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wart_vat:=v
                unlock
#else
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI PODATKU DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
      endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         j:=ascan(was,{|x|x[1]=stawkizby[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(was[j,2],cl-1,A_ZAOKR,"@ZE ")
            wd+=was[j,2]
         endif
         ++k
      enddo
#ifdef A_CENVAT
      ?? "|"+strpic(wartosc,cl-1,A_ZAOKR,"@E ")
#else
      ?? "|"+strpic(wartosc+wart_vat,cl-1,A_ZAOKR,"@E ")
#endif
      v:=w:=0
      for k:=k to len(stawkizby)
          j:=ascan(was,{|x|x[1]=stawkizby[k]})
          if j=0
             ?? pad("|",cl)
          else
             ?? "|"+strpic(was[j,2],cl-1,A_ZAOKR,"@EZ ")
             w+=was[j,2]
             v+=was[j,3]
             if val(stawkizby[k])#0
                ?? "|"+strpic(was[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      wd+=w+v
      wn+=w
      wv+=v
      ?? "|"+strpic(w,cl-1,A_ZAOKR,"@E ")+"|"+strpic(v,cl-1,A_ZAOKR,"@E ")
        SKIP
      enddo
#ifndef A_15CALI
      specout(P_UOFF)
#endif
      if !eof().and.EvaldB(ok)<=txt + dtos(DO)
        ?
#ifdef A_MM
        ? "    DOKUMENTY ",txt," DO PRZENIESIENIA:" D_KL
#else
#ifdef A_SUBDOK
        ? "    DOKUMENTY ",TranR(txt,"##/XXXX")," DO PRZENIES." D_KL
#else
        ? "    DOKUMENTY ",TranR(txt,"##/XX")," PRZENIESIENIE:" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawkizby[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawkizby)
          j:=ascan(wat,{|x|x[1]=stawkizby[k]})
          if j=0
             ?? pad("|",cl*if(val(stawkizby[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawkizby[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      ENDIF
    ENDDO

        ?      //31
#ifdef A_MM
        ? "    DOKUMENTY ",txt,"            RAZEM:" D_KL
#else
#ifdef A_SUBDOK
        ? "    DOKUMENTY ",TranR(txt,"##/XXXX"),"      RAZEM :" D_KL
#else
        ? "    DOKUMENTY ",TranR(txt,"##/XX"),"        RAZEM :" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawkizby[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawkizby)
          j:=ascan(wat,{|x|x[1]=stawkizby[k]})
          if j=0
             ?? pad("|",cl*if(val(stawkizby[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawkizby[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      wdt+=wd
      wnt+=wn
      wvt+=wv
  enddo
  IF STRONA>0
#ifdef A_17CPI
        ? ccpi(7)
        ? CPAD("ZESTAWIENIE RAZEM:",34,17,1) D_KL
#else
        ? ccpi(8)
        ? CPAD("ZESTAWIENIE RAZEM:",34,20,1) D_KL
#endif
      k:=1
      do while k<=len(stawkizby) .and. stawkizby[k]>"9"
         j:=ascan(wag,{|x|x[1]=stawkizby[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wag[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wdt,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawkizby)
          j:=ascan(wag,{|x|x[1]=stawkizby[k]})
          if j=0
             ?? pad("|",cl*if(val(stawkizby[k])=0,1,2))
          else
             ?? "|"+strpic(wag[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawkizby[k])#0
                ?? "|"+strpic(wag[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wnt,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wvt,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      ENDIF
ENDIF
RETURN
*********************
static PROCEDURE W_zak()

MEMVAR GETLIST,NAZWA_MAG,dok_par,dokumenty
LOCAL OD,DO,TXT,i,wd,wn,wv,dok_i,was,j,k,wat,wdt:=0,wnt:=0,wag,wvt:=0,dok_zak,w,v,mag_poz,mag_biez,cc,cl,kl,lp
local ok

  dok_zak:=""
#ifdef A_SUBDOK
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|y:=left(y,4),if(dok_par[i,b,1]="P".and.dok_par[i,b,2]="V".and.!y$dok_zak,dok_zak+=y+" ",)})})
#else
  aeval(dokumenty,{|x,a|i:=a,aeval(x,{|y,b|if(dok_par[i,b,1]="P".and.dok_par[i,b,2]="V".and.!left(y,2)$dok_zak,dok_zak+=left(y,2)+" ",)})})
#endif
  dok_zak+=space(40)
  i:=0

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)
@ 12,20 SAY "REJESTR ZAKUPÓW" UNICODE

@ 14,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

  @ 16,10 SAY 'DLA DOKUMENTÓW:' UNICODE GET dok_zak PICTURE "@KS60!"
  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
READ
if readkey()=27
  break
endif
dok_zak:=upper(alltrim(dok_zak))+'  '


    set order to TAG MAIN_NRK
#ifndef STANY
  SET RELATION TO INDEX INTO INDX_MAT
#else
  SET RELATION TO NR_MAG+INDEX INTO INDX_MAT
#endif
  SELECT DM

  mkdmbIs()

  SET FILTER TO POZYCJA>D_LP0
  go top
  if eof()
     return
  endif
#ifdef A_MM
  mag_poz:=1
  mag_biez:=left(magazyny[mag_poz],2)
#else
  mag_biez:=nr_mag
  mag_poz:=ascan(magazyny,mag_biez)
#endif
  wag:={}
  ok:=EvaldB('{||'+IndexkeY(0)+'}')
IF TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.T.,.T.)
  @ 8,0 CLEAR
  DO WHILE .t.
    if mag_poz=0
#ifdef A_MM
       EXIT
#else
       j:=errornew()
       j:severity:=2
       j:candefault:=.t.
       j:description:=hb_UTF8ToStr('Nieprawidłowy numer magazynu')
       j:filename:='DM.DBF'
       j:operation:='RECNO'
       j:args:={recno()}
       if !eval(errorblock(),j)
          skip
          mag_biez:=nr_mag
          mag_poz:=ascan(magazyny,mag_biez)
          loop
       endif
#endif
    endif

    i:=ASCAN(DOK_PAR[MAG_POZ],{|X|X[1]="P".AND. X[2]="V"},i+1)
    IF i=0
#ifdef A_MM
       EXIT
#else
       if mag_biez=nr_mag
          seek mag_biez + HB_UCHAR(0x00A0)
       endif
       if eof()
          EXIT
       endif
       mag_biez:=nr_mag
       mag_poz:=ascan(magazyny,mag_biez)
       loop
#endif
    ENDIF
#ifdef A_SUBDOK
    txt:=left(dokumenty[mag_poz,i],4)
#else
    txt:=left(dokumenty[mag_poz,i],2)
#endif
    if !txt$dok_zak
       loop
    endif
#ifndef A_MM
    txt:=mag_biez+txt
#endif
    SEEK TXT+DTOS(OD)
    IF eof() .or. EvaldB(ok)>txt + dtos(DO)
      LOOP
    ENDIF

    IF prow()+12>P_ROWN
       IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
         print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE SYNTETYCZNE ZAKUPÓW ZA OKRES OD "),OD," DO ",DO,"."
/*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      //? trim(subs(magazyny[mag_poz],4)),adres_mag[mag_poz]
      ?
      ? "Strona"+str(++strona,3)
    ENDIF
    ?
    ?
    j:=recno()
#ifdef A_MM
    ? "DOKUMENTY ",txt,"    RAZEM : "
#else
    ? magazyny[mag_poz]
#ifdef A_SUBDOK
    ? "DOKUMENTY ",TranR(txt,"##/XXXX")," RAZEM : "
#else
    ? "DOKUMENTY ",TranR(txt,"##/XX")," RAZEM : "
#endif
#endif
    sum wartoSC,warT_vaT to wd,wv rest while !eof().and.EvaldB(ok)<=txt + dtos(DO)
    wat:={}
    if dok_par[mag_poz,i,2]$"UV"
      wn:=wd
      wd+=wv
    go j
    do while !eof().and.EvaldB(ok)<=txt + dtos(DO)
      was:={}
      aeval(stawki,{|x|if(val(x)#0,aadd(was,{x,0,bin2D(binfieldget(D_NVAT+ltrim(x)))}),)})
      SELECT MAIN
      SEEK dm->(KEY_DOK+nr_dowodu)
      WHILE dm->(KEY_DOK+nr_dowodu)=KEY_DOK+nr_dowodu
         j:=ascan(was,{|x|x[1]=proc_vat})
         if j=0
           if ascan(stawki,proc_vat)=0 //.and. val(proc_vat)#0
              ? hb_UTF8ToStr("BŁĄD STAWKI VAT DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
              ?
           endif
           aadd(was,{proc_vat,cena,0})
         else
           was[j,2]+=cena
         endif
         skip
      enddo
      SELECT DM
      w:=v:=0
      for k=1 to len(was)
             was[k,3]:=ROUND(was[k,3],0)/100
             w+=was[k,2]
             v+=was[k,3]
             j:=ascan(wat,{|x|x[1]=was[k,1]})
             if j=0
               aadd(wat,aclone(was[k]))
             else
               wat[j,2]+=was[k,2]
               wat[j,3]+=was[k,3]
             endif
      next
      if ROUND(w-wartosc,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wartosc:=w
                unlock
#else
#ifndef A_PLUDRY
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
#endif
      endif
      if ROUND(v-warT_vaT,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wart_vat:=v
                unlock
#else
                ? ("BŁĄD WARTOŚCI PODATKU DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
      endif
      skip
    enddo
    else
      wv:=0
      wn:=0
    endif
      ?? ltrim(TrAN(Wd,"@E ",A_ZAOKR,15))
      ?
      for j:=1 to len(wat)
         ?  "STAWKA "+wat[j,1]+". : "+TrAN(wat[j,2],"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wat[j,3],"@E ",A_ZAOKR,15)+" = "+TrAN(wat[j,2]+wat[j,3],"@E ",A_ZAOKR,15)
      next
      if j>2
      ?  "---------------------------------------------------------------------------"
      ?  "       NETTO "+TrAN(wn,"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wv,"@E ",A_ZAOKR,15)+" = "+TrAN(wn+wv,"@E ",A_ZAOKR,15)
      endif
      wdt+=wd
      wnt+=wn
      wvt+=wv
      for k=1 to len(wat)
             j:=ascan(wag,{|x|x[1]=wat[k,1]})
             if j=0
               aadd(wag,aclone(wat[k]))
             else
               wag[j,2]+=wat[k,2]
               wag[j,3]+=wat[k,3]
             endif
      next
  ENDDO
    IF STRONA>0
      IF prow()+12>P_ROWN
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
      setprc(0,0)
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE SYNTETYCZNE ZAKUPÓW ZA OKRES OD "),OD," DO ",DO,"."
/*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      //? trim(subs(magazyny[mag_poz],4)),adres_mag[mag_poz]
      ?
      ? "Strona"+str(++strona,3)
      ENDIF
      ?
      ?
      ? "ZESTAWIENIE RAZEM: "+ltrim(TrAN(Wdt,"@E ",A_ZAOKR,15))
      ?
      for j:=1 to len(wag)
      ?  "STAWKA "+wag[j,1]+". : "+TrAN(wag[j,2],"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wag[j,3],"@E ",A_ZAOKR,15)+" = "+TrAN(wag[j,2]+wag[j,3],"@E ",A_ZAOKR,15)
      next
      if j>2
      ?  "---------------------------------------------------------------------------"
      ?  "       NETTO "+TrAN(wnt,"@E ",A_ZAOKR,15)+" + VAT "+TrAN(wvt,"@E ",A_ZAOKR,15)+" = "+TrAN(wnt+wvt,"@E ",A_ZAOKR,15)
      endif
   ENDIF
******
ELSE
*****
#ifdef A_KPRwVAT
#define D_KPR +12
#else
#ifdef A_DATAVAT
#define D_KPR +11
#else
#define D_KPR
#endif
#endif
  @ 8,0 CLEAR

  cc:=len(stawki)+3
  aeval(stawki,{|x|if(val(x)#0,++cc,)})
  cl:=13
#ifdef A_15CALI
  do while 30+41+cc*cl D_KPR >D_COLN*D_CPI .and. cl>11
     --cl
  enddo
#endif
  do while 41+cc*cl D_KPR >D_COLN*D_CPI
     --cl
  enddo
#ifdef A_15CALI
  kl:=D_COLN*D_CPI-(41 D_KPR )-cc*cl
#endif

  DO WHILE .t.
    if mag_poz=0
#ifdef A_MM
     EXIT
#else
       j:=errornew()
       j:severity:=2
       j:candefault:=.t.
       j:description:=hb_UTF8ToStr('Nieprawidłowy numer magazynu')
       j:filename:='DM.DBF'
       j:operation:='RECNO'
       j:args:={recno()}
       if !eval(errorblock(),j)
          skip
          mag_biez:=nr_mag
          mag_poz:=ascan(magazyny,mag_biez)
          loop
       endif
#endif
    endif

    i:=ASCAN(DOK_PAR[MAG_POZ],{|X|X[1]="P".AND. X[2]="V"},i+1)
    IF i=0
#ifdef A_MM
     EXIT
#else
       if mag_biez=nr_mag
          seek mag_biez + HB_UCHAR(0x00A0)
       endif
       if eof()
          EXIT
       endif
       mag_biez:=nr_mag
       mag_poz:=ascan(magazyny,mag_biez)
       loop
#endif
    ENDIF
#ifdef A_SUBDOK
    txt:=left(dokumenty[mag_poz,i],4)
#else
    txt:=left(dokumenty[mag_poz,i],2)
#endif
    if !txt$dok_zak
       loop
    endif
#ifndef A_MM
    txt:=mag_biez+txt
#endif
    SEEK TXT+DTOS(OD)
    IF eof() .or. eval(ok)>txt + dtos(DO)
      LOOP
    ENDIF
    lp:=wd:=WN:=wv:=0
    wat:={{stawki[1],0,0}}
    dok_i:=""#dok_par[mag_poz,i,3]
    do while !eof().and.EvaldB(ok)<=txt + dtos(DO)
      if prow()+10>P_ROWN
        IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
        setprc(0,0)
      else
#ifdef D_HWPRN
         memvar->Landscape:=.t.
#endif
         print()
#ifdef A_PCL
         memvar->landscape:=.f.
         ?? speC(eval(P_LAND,.t.)) //+"&l1O")
#endif
        ENDIF
#ifdef A_15CALI
        ?? padr(firma_n+", "+firma_a,D_COLN-16)," dnia ",dtoc(DatE())
#else
        ?? padr(firma_n,D_COLN-16)," dnia ",dtoc(DatE())
#endif
        ?
        ? hb_UTF8ToStr("REJESTR ZAKUPÓW ZA OKRES OD "),OD," DO ",DO,"."
/*
        if DatY->d_z_mies1>=do
          ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
          ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
*/
        ?
        ? "Strona"+str(++strona,3)
      endif
      ?
#ifndef A_MM
      ? magazyny[mag_poz]
#endif
      ? "DOKUMENTY "+LEFT(txt,2)+"/"+DOKUMENTY[MAG_POZ,i]
      ?
#ifdef A_17CPI
        specout(ccpi(7)+P_UON)
#else
        specout(ccpi(8)+P_UON)
#endif
#ifdef A_KPRwVAT
      ? "Lp.|Prawo|Nr KP| Dokument z dnia|Data |Identyfikator"
#else
#ifdef A_DATAVAT
      ? "Lp.|Prawo dnia| Dokument z dnia|Data |Identyfikator"
#else
      ? "Lp.| Dokument z dnia|Data |Identyfikator"
#endif
#endif
#ifdef A_15CALI
      if kl>cl
         ?? "|"+padc("Kontrahent",kl-1)
      else
         ?? pad("|",kl)
      endif
#endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         ?? "|"+padc(stawki[k],cl-1)
         ++k
      enddo
      ?? "|"+padc("BRUTTO",cl-1)
      for k:=k to len(stawki)
          j:=stawki[k]
          if val(j)=0
             ?? "|"+padc(j,cl-1)
          else
             ?? "|"+padc("netto "+j+"%",cl-1)+"|"+padc("vat "+j+"%",cl-1)
          endif
      next k
      ?? "|"+padc("NETTO",cl-1)+"|"+padc("VAT",cl-1)
      if wd#0 .or. wv#0
#ifdef A_MM
            ? padl("DOKUMENTY",14 D_KPR),txt,"         Z PRZENIESIEN.:" D_KL
#else
#ifdef A_SUBDOK
            ? padl("DOKUMENTY",14 D_KPR),TranR(txt,"##/XXXX"),"    Z PRZENIESIEN.:" D_KL
#else
            ? padl("DOKUMENTY",14 D_KPR),TranR(txt,"##/XX"),"      Z PRZENIESIEN.:" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawki[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawki)
          j:=ascan(wat,{|x|x[1]=stawki[k]})
          if j=0
             ?? pad("|",cl*if(val(stawki[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawki[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ENDIF
      specout(P_UOFF)
        do while !eof().and.EvaldB(ok)<=txt + dtos(DO) .and. prow()+3<P_ROWN
#ifdef A_15CALI
#ifdef A_KPRwVAT
            ? str(++lp,3)+'|'+DTOV(data_vat)+'|'+nr_kpr
#else
#ifdef A_DATAVAT
            ? str(++lp,3)+'|'+DTOC(data_vat)
#else
            ? str(++lp,3)
#endif
#endif
            ?? '|'+pad(nr_faktury,10)+"|"+DTOV(DATA_dost)+"|"+DTOV(DATA)
            if dok_i .and. FIRMY->(dbseek(DM->(D_KH),.f.))
#ifdef A_FFULL
               j:=FIRMY->(if(len(trim(longname))>kl-15,nazwa,longname))
               k:=min(len(trim(j)),kl-15)
               ?? "|"+FIRMY->ident +"|"+pad(j,k)+padl(trim(subs(FIRMY->adres,7)),kl-1-k)
#else
               k:=min(len(trim(FIRMY->nazwa)),kl-15)
               ?? "|"+FIRMY->ident +"|"+pad(FIRMY->nazwa,k)+padl(trim(subs(FIRMY->adres,7)),kl-1-k)
#endif
            else
               ?? pad("|",14)+"|"+pad(dm->dost_odb,kl-1)
            endif
#else
            specout(P_UOFF)
#ifdef A_KPRwVAT
            ? str(++lp,3)+'|'+DTOV(data_vat)+'|'+nr_kpr
#else
#ifdef A_DATAVAT
            ? str(++lp,3)+'|'+DTOC(data_vat)
#else
            ? str(++lp,3)
#endif
#endif
            ?? '|'+pad(nr_faktury,10)+"|"+DTOV(DATA_dost)+"|"+DTOV(DATA)
            if dok_i .and. FIRMY->(dbseek(DM->(D_KH),.f.))
               ?? "|"+FIRMY->ident
#ifdef A_FFULL
               ?? "|"+padr(firmy->longname,70)+padr(firmy->adres,50)
#else
               ?? "|"+padr(firmy->nazwa,70)+padr(firmy->adres,50)
#endif
            else
               ?? pad("|",14)+"|"+dm->dost_odb
            endif
            specout(P_UON)
            ? space(40 D_KPR )
#endif
            if dok_par[mag_poz,i,2]="Z"
              wd+=wartosc
              k:=1
              do while k<=len(stawki) .and. stawki[k]>"9"
                if k=1
                   wat[1,2]+=wartoSC
                   ?? "|"+strpic(wartosc,cl-1,A_ZAOKR,"@E ")
                else
                   ?? pad("|",cl)
                endif
                ++k
              enddo
              ?? "|"+strpic(wartoSC,cl-1,A_ZAOKR,"@E ")+"|"
              SKIP
              loop
            endif
            was:={}
            aeval(stawki,{|x|if(val(x)#0,aadd(was,{x,0,bin2D(binfieldget(D_NVAT+ltrim(x)))}),)})
          SELECT MAIN
          SEEK dm->(KEY_DOK+nr_dowodu)
          WHILE dm->(KEY_DOK+nr_dowodu)=KEY_DOK+nr_dowodu
             j:=ascan(was,{|x|x[1]=proc_vat})
             if j=0
               if ascan(stawki,proc_vat)=0 //.and. val(proc_vat)#0
                ? hb_UTF8ToStr("BŁĄD STAWKI VAT DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
               endif
               aadd(was,{proc_vat,cena,0})
             else
               was[j,2]+=cena
             endif
             skip
          enddo
          select dm
          w:=v:=0
          for k=1 to len(was)
             was[k,3]:=ROUND(was[k,3],0)/100
             w+=was[k,2]
             v+=was[k,3]
             j:=ascan(wat,{|x|x[1]=was[k,1]})
             if j=0
               aadd(wat,aclone(was[k]))
             else
               wat[j,2]+=was[k,2]
               wat[j,3]+=was[k,3]
             endif
             j:=ascan(waG,{|x|x[1]=was[k,1]})
             if j=0
               aadd(waG,aclone(was[k]))
             else
               waG[j,2]+=was[k,2]
               waG[j,3]+=was[k,3]
             endif
          next
      if ROUND(w-wartosc,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wartosc:=w
                unlock
#else
#ifndef A_PLUDRY
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
#endif
      endif
      if ROUND(v-warT_vaT,A_ZAOKR)#0
#ifdef FIXDOK
                lock
                wart_vat:=v
                unlock
#else
                ? hb_UTF8ToStr("BŁĄD WARTOŚCI PODATKU DOKUMENTU: "),nr_mag+'/'+smb_dow+' '+nr_dowodu+'/'+pozycja," !!!"+replicate(HB_BCHAR(7),4)
                ?
#endif
      endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         j:=ascan(was,{|x|x[1]=stawki[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(was[j,2],cl-1,A_ZAOKR,"@ZE ")
            wd+=was[j,2]
         endif
         ++k
      enddo
      ?? "|"+strpic(wartosc+wart_vat,cl-1,A_ZAOKR,"@E ")
      v:=w:=0
      for k:=k to len(stawki)
          j:=ascan(was,{|x|x[1]=stawki[k]})
          if j=0
             ?? pad("|",cl)
          else
             ?? "|"+strpic(was[j,2],cl-1,A_ZAOKR,"@ZE ")
             w+=was[j,2]
             v+=was[j,3]
             if val(stawki[k])#0
                ?? "|"+strpic(was[j,3],cl-1,A_ZAOKR,"@ZE ")
             endif
          endif
      next k
      wd+=w+v
      wn+=w
      wv+=v
      ?? "|"+strpic(w,cl-1,A_ZAOKR,"@E ")+"|"+strpic(v,cl-1,A_ZAOKR,"@E ")
          SKIP
        enddo
#ifndef A_15CALI
      specout(P_UOFF)
#endif
      if !eof().and.EvaldB(ok)<=txt + dtos(DO)
        ?      //31
#ifdef A_MM
        ? padl("DOKUMENTY",14 D_KPR ),txt,"         PRZENIESIENIE :" D_KL
#else
#ifdef A_SUBDOK
        ? padl("DOKUMENTY",14 D_KPR ),TranR(txt,"##/XXXX"),"    PRZENIESIENIE :" D_KL
#else
        ? padl("DOKUMENTY",14 D_KPR ),TranR(txt,"##/XX"),"      PRZENIESIENIE :" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawki[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawki)
          j:=ascan(wat,{|x|x[1]=stawki[k]})
          if j=0
             ?? pad("|",cl*if(val(stawki[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawki[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      ENDIF
    ENDDO
#ifdef A_MM
        ? padl("DOKUMENTY",14 D_KPR ),txt,"                 RAZEM :" D_KL
#else
#ifdef A_SUBDOK
        ? padl("DOKUMENTY",14 D_KPR ),TranR(txt,"##/XXXX"),"          RAZEM   :" D_KL
#else
        ? padl("DOKUMENTY",14 D_KPR ),TranR(txt,"##/XX"),"            RAZEM   :" D_KL
#endif
#endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         j:=ascan(wat,{|x|x[1]=stawki[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wd,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawki)
          j:=ascan(wat,{|x|x[1]=stawki[k]})
          if j=0
             ?? pad("|",cl*if(val(stawki[k])=0,1,2))
          else
             ?? "|"+strpic(wat[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawki[k])#0
                ?? "|"+strpic(wat[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wn,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wv,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      wdt+=wd
      wnt+=wn
      wvt+=wv
  enddo
  IF STRONA>0

#ifdef A_17CPI
        ? ccpi(7)
        ? CPAD("ZESTAWIENIE RAZEM:",40 D_KPR ,17,1) D_KL
#else
        ? ccpi(8)
        ? CPAD("ZESTAWIENIE RAZEM:",40 D_KPR ,20,1) D_KL
#endif
      k:=1
      do while k<=len(stawki) .and. stawki[k]>"9"
         j:=ascan(wag,{|x|x[1]=stawki[k]})
         if j=0
            ?? pad("|",cl)
         else
            ?? "|"+strpic(wag[j,2],cl-1,A_ZAOKR,"@EZ ")
         endif
         ++k
      enddo
      ?? "|"+strpic(wdt,cl-1,A_ZAOKR,"@E ")
      for k:=k to len(stawki)
          j:=ascan(wag,{|x|x[1]=stawki[k]})
          if j=0
             ?? pad("|",cl*if(val(stawki[k])=0,1,2))
          else
             ?? "|"+strpic(wag[j,2],cl-1,A_ZAOKR,"@EZ ")
             if val(stawki[k])#0
                ?? "|"+strpic(wag[j,3],cl-1,A_ZAOKR,"@EZ ")
             endif
          endif
      next k
      ?? "|"+strpic(wnt,cl-1,A_ZAOKR,"@E ")+"|"+strpic(wvt,cl-1,A_ZAOKR,"@E ")
      ?? ccpi(4)
      ENDIF
ENDIF
RETURN
 #undef D_KL
 //#undef D_COLN
 #undef D_CPI
#endif
***********************************
PROCEDURE w_p_r(OD,DO,wa_flag,i_od,i_do,i_snt,i_gr,t_gr,kg_flag,jm_o)

MEMVAR GETLIST,NAZWA_MAG,adres_mag,grupy_indx

LOCAL D_G,SP,SR,WP,WR,S,W,TXT,mag_poz,sel,SPT:=0,PT:=0,RT:=0,;
SKT:=0,SPTOT:=0,PTOT:=0,RTOT:=0,SKTOT:=0,lam,flag,bw,nmat,njob,cedat,;
last_gr:="",spgr,pgr,rgr,skgr,gri,ws,is,bkey,x


#ifdef A_JMO
#define izreS(x,p) if(jm_o,if(x%p=0,Tran(x/p,"@Z ######.   "),stuff(str(int(x/p)+x%p/1000,10,3),7,1,"r")),TraN(x,"@Z ######"+ILPIC))
#define D_JMO /
#else
#define izreS(x,p) TraN(x,"@Z ######"+ILPIC)
#endif

#ifdef A_JMALTTOT
#define D_ILOSC if(jm_o,A_JMALTTOT(ilosc,nr_zlec,sel,x),ilosc)
#define D_JMO *
#else
#define D_ILOSC ilosc
#endif

#ifdef D_GRAM
local wag,wagsp,wagp,wagr,wagsk,wagspt:=0,wagpt:=0,wagrt:=0,wagskt:=0,wagsptot:=0,;
wagptot:=0,wagrtot:=0,wagsktot:=0,wagspgr,wagpgr,wagrgr,wagskgr
//#define TraN(a,b,c,d) strpic(a,d,c,b)    //TraN(W,"@E ",A_ZAOKR,15)
#define D_A_ZAOKR if(kg_flag,2,A_ZAOKR)
#else
#define D_A_ZAOKR A_ZAOKR
#endif

if wa_flag=NIL
   wa_flag:=.t.
endif

DEFAULT OD TO DatY->data_gran+1
DEFAULT DO TO IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)
@ 13,10 SAY "ZESTAWIENIE PRZYCHODÓW I ROZCHODÓW DLA POSZCZEGÓLNTCH MATERIAŁÓW" UNICODE

if pcount()<2

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

@ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
@ 18,50 say "DATA do   :" get do valid {||do:=max(od-1,do),.t.}
READ
if readkey()=27
  break
endif

endif

select MAIN

#ifdef A_IZ
#ifdef A_WA
set filter to ilosc#0 .or. wartosc#0
#else
set filter to ilosc#0
#endif
#endif

SELECT STANY

if pcount()<5

#ifndef A_STSIMPLE
IF tak(hb_UTF8ToStr("CZY KOJEJNOŚĆ W/G NAZW"),20,,.F.,.F.)
   set order to 1
   bkey:=EvAlDb('{||'+IndexkeY(0)+'}')
   SEEK MAG_BIEZ LAST
   I_DO:=MAG_BIEZ+subs(EvaldB(bkey),3)
   SEEK MAG_BIEZ
   I_OD:=MAG_BIEZ+subs(EvaldB(bkey),3)
   @ 19,05 SAY 'MAT. "od" :' GET I_OD PICTURE "@KR! ##/"+repl("X",len(i_do)-2) valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
   @ 20,05 SAY 'MAT. "do" :' GET I_DO PICTURE "@KR! ##/"+repl("X",len(i_do)-2) valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
ELSE
#endif
   bkey:=EvAlDb('{||'+IndexkeY(0)+'}')
    SEEK MAG_BIEZ LAST
    I_DO:=MAG_BIEZ+subs(EvaldB(bkey),3)
    SEEK MAG_BIEZ
    I_OD:=MAG_BIEZ+subs(EvaldB(bkey),3)
    @ 20,05 SAY 'MAT. "od" :' GET I_OD PICTURE "@KR! ##/"+ INDEXPIC valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
    @ 20,45 SAY 'MAT. "do" :' GET I_DO PICTURE "@KR! ##/"+ INDEXPIC valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
#ifndef A_STSIMPLE
ENDIF
#endif
READ
if readkey()=27
  break
endif

I_OD=UpP(TRIM(I_OD))
I_DO=UpP(TRIM(I_DO))

else

   bkey:=EvAlDb('{||'+IndexkeY(0)+'}')

endif

SEEK I_OD

IF od>do
   i_snt:=.f.
ENDIF

IF i_snt=NIL
   i_snt:=TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.T.,.T.)
ENDIF
***********
if i_snt
*************
IF i_gr=NIL

i_gr:=2

@ 22,10 SAY 'grupowanie według pierwszych' UNICODE GET I_gr picture "##" valid i_gr>=0 .and. i_gr<=len(EvaldB(bkey))
  SAYL " znaków klucza" UNICODE
read

if readkey()=27
  break
endif

ENDIF

if t_gr=NIL
   t_gr:=TAK("CZY TYLKO GRUPY",23,,.F.,.f.)
endif
#ifdef D_GRAM
if kg_flag=NIL
   kg_flag:=TAK(hb_UTF8ToStr("CZY ILOŚCI W KILOGRAMACH ZAMIAST WARTOŚCI"),,,.F.,.F.)
endif
#endif
#ifdef A_JMO
if t_gr
   jm_o:=.f.
elseif jm_o=NIL
   jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
endif
#else
#ifdef A_JMALTTOT
IF jm_o=NIL
   jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
ENDIF
#endif
#endif

private mag_biez
mag_biez:=nr_mag
mag_poz:=max(1,ascan(magazyny,mag_biez))

@ 8,0 CLEAR 
DO CASE
  CASE OD>DatY->d_z_MIES1
    D_G:=DatY->d_z_MIES1
  CASE OD>DatY->d_z_MIES2
    D_G:=DatY->d_z_MIES2
  OTHERWISE
    D_G:=DatY->d_z_ROK
ENDCASE

spgr:=pgr:=rgr:=skgr:=0
#ifdef D_JMTOT
gri:={}
#endif
DO WHILE EvaldB(BKEY)<=I_DO .AND. !EOF()

   TXT:=NR_MAG+INDEX

#undef HEADER_1
#undef HEADER_2
#ifdef A_SHORTIND
#define HEADER_1 "    |"
#define HEADER_2 " ... "
#define D_KOLC +11
#else
#define D_KOLC
   #ifdef A_KTM
#define HEADER_1 "                |"
#define HEADER_2 "             ... "
   #else
      #ifdef A_OLZA
#define HEADER_1 "              |"
#define HEADER_2 "           ... "
      #else
#ifdef A_SWW
#define HEADER_1 "      |"
#define HEADER_2 "   ... "
#undef D_KOLC
#define D_KOLC +11
#else
#define HEADER_1 "            |"
#define HEADER_2 "         ... "
      #endif
      #endif
   #endif
#endif
#ifdef D_GRAM
  #define HEADER_3 hb_UTF8ToStr(if(kg_flag,"  ILOŚĆ   |  WAGA kg  |   ILOŚĆ  |  WAGA kg  |   ILOŚĆ  |  WAGA kg  |  ILOŚĆ   |  WAGA kg  |    |",;
                              "  ILOŚĆ   |  "+WANAZ+"  |   ILOŚĆ  |  "+WANAZ+"  |   ILOŚĆ  |  "+WANAZ+"  |  ILOŚĆ   |  "+WANAZ+"  |    |"))
#else
  #define HEADER_3 hb_UTF8ToStr("  ILOŚĆ   |  "+WANAZ+"  |   ILOŚĆ  |  "+WANAZ+"  |   ILOŚĆ  |  "+WANAZ+"  |  ILOŚĆ   |  "+WANAZ+"  |    |")
#endif
#ifdef A_WA
#ifdef A_JMALTTOT
   if jm_o
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=SJMO_ROKU
    W:=WART_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=SJMO_MIES2
    W:=WART_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=SJMO_MIES1
    W:=WART_MIES1
  ENDCASE
   else
#endif
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=ZAMKN_ROKU
    W:=WART_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=ZAMK_MIES2
    W:=WART_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=ZAMK_MIES1
    W:=WART_MIES1
  ENDCASE
#ifdef A_JMALTTOT
endif
#endif
#else
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=ZAMKN_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=ZAMK_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=ZAMK_MIES1
  ENDCASE
#endif

  SELECT MAIN

    seek TXT+DTOS(D_G)+"Z"
#ifdef D_JMO
#define F_JMO sel:=i_lam(data),
#define jM (if(jm_o,jm_opcja,jm))
#else
#define F_JMO
#endif
#ifdef A_WA
#ifdef D_GRAM
    if kg_flag
      w:=s
      EXECUTE {||F_JMO w+=ilosc,S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
      w:=w*(sel:=i_lam(od))->gram/1000
    else
    EXECUTE {||F_JMO W+=WARTOSC,S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
    sel:=i_lam(od)
    endif
    #define D_SEL sel
#else
    EXECUTE {||F_JMO W+=WARTOSC,S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
    #define D_SEL sel:=i_lam(od)
#endif
#else
    w:=s
    EXECUTE {||F_JMO w:=w+ilosc,S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
#ifdef D_GRAM
    w:=w*(sel:=i_lam(od))->gram/1000
#else
    w:=w*(sel:=i_lam(od))->cenA
#endif
    #define D_SEL sel
#endif

    if s#0 .or. w#0
       lam:=(D_SEL)->(recno())
    else
       lam:=(SEL:=I_LAM(data))->(recno())
    endif

#undef D_SEL

    SPT+=w
    SPTOT+=W
    spgr+=w
#ifdef D_JMTOT
    //if s<>0
    if 0=(nmat:=ascan(gri,{|x|x[1]=(sel)->jM}))
     aadd(gri,{(sel)->jM,s,0,0,0})
     nmat:=len(gri)
    else
     gri[nmat,2]+=s
    endif
    //endif
#endif
    //njob:=NIL

  do while s#0 .or. w#0 .or. !eof() .and. NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO)

    SP:=SR:=WP:=WR:=0
    last_gr:=left(STANY->(EvaldB(BKEY)),i_gr)

#ifdef D_JMTOT
  IF prow()+len(gri)>P_ROWN .OR. STANY->NR_MAG#MAG_BIEZ
#else
  IF prow()+1>P_ROWN .OR. STANY->NR_MAG#MAG_BIEZ
#endif
    SPTOT-=W // tam ...
    if strona>0
       if i_gr#2 .or. mag_biez=STANY->nr_mag
       if wa_flag
          ?? speC(HB_BCHAR(13)+P_UON+SPACE(136))
          ?? speC(P_UOFF)
          ? HEADER_1+TrAN(SPTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(PTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(RTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(SKTOT,"@E ",D_A_ZAOKR,22)+"|    |"
          IF STANY->NR_MAG#MAG_BIEZ
            ?? "... RAZEM MAGAZYN "+mag_biez+" ..."
          ELSE
            ?? " ... DO PRZENIESIENIA .."
          ENDIF
       else
          ?? speC(HB_BCHAR(13)+P_UON+SPACE(90))
          ?? SPEC(P_UOFF)
       endif
       endif
       IF STANY->NR_MAG#MAG_BIEZ
          mag_biez=STANY->nr_mag
          mag_poz:=max(1,ascan(magazyny,mag_biez))
          SPTOT:=PTOT:=RTOT:=SKTOT:=0
       endif
       ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
       setprc(0,0)
    else
         print()
#ifdef A_OKI4W
         memvar->landscape=.f.
         ?? speC(eval(P_LAND,.t.)) //+"&l1O")
#endif
    ENDIF
    ?? ccpi(4)
    ?? padr(firma_n,P_COLN-16),"dnia",dtoc(DatE())
    ?
    ?? hb_UTF8ToStr("STAN POCZĄTKOWY I KOŃCOWY ORAZ RUCHY ZA OKRES OD "),OD," DO ",DO,"."
/*
  if DatY->d_z_mies1>=do
    ? "PO ZAMKNIĘCIU MIESIĄCA"
  ELSE
    ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
  ENDIF
*/
  ? trim(subs(magazyny[mag_poz],4))+' '+adres_mag[mag_poz]
  ?
  ? "Strona"+str(++strona,3)
  if wa_flag
  ? ccpi(7)
  ? HEADER_1+hb_UTF8ToStr("    STAN POCZĄTKOWY   |   PRZYCHÓD           |    ROZCHÓD           |    STAN KOŃCOWY      |JEDN|NAZWA")
  ?? speC(P_UON)
  IF t_GR .AND. STRONA=1
  ? padr(Tran(STANY->INDEX,"@R "+ INDEXPIC )+"|"+HEADER_3,136)
  ELSE
  ? padr(HEADER_1+HEADER_3,136)
  ENDIF
  IF SPTOT#0 .OR.PTOT#0 .OR.RTOT#0 .OR.SKTOT#0
      ? padr(HEADER_1+TrAN(SPTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(PTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(RTOT,"@E ",D_A_ZAOKR,22)+"|"+TrAN(SKTOT,"@E ",D_A_ZAOKR,22)+"|    |   ... Z PRZENIESIENIA ....",136)
  ENDIF
  else
  ? spec(ccpi(5)+P_UON)
  ? HEADER_1+hb_UTF8ToStr("STAN POCZ.| PRZYCHÓD |  ROZCHÓD |STAN KOŃC.|JEDN|NAZWA")
  endif
  ?? speC(P_UOFF)
  SPTOT+=W // i z powrotem ...
  ENDIF
  if !t_gr
  if wa_flag
    //if njob=NIL
       ? Tran(INDX_MAT->INDEX,"@R "+ INDEXPIC )+"|"+izreS(S,(sel)->przel)+"|"+TrAN(W,"@E ",D_A_ZAOKR,11)+"|"
    /*else
       ? HEADER_2,space(22)
    endif*/
  else
    //if njob=NIL
       ? Tran(INDX_MAT->INDEX,"@R "+ INDEXPIC )+"|"+izreS(S,(sel)->przel)+"|"
    /*else
       ? HEADER_2,space(10)
    endif*/
  endif
  endif
#ifdef A_WA
#ifdef D_GRAM
if kg_flag
      EXECUTE {|x|S+=D_ILOSC,;
        SP+=PRZYCH(D_ILOSC),SR+=ROZCH(D_ILOSC),;
        WP+=PRZYCH(x:=ilosc*(sel)->gram/1000),WR+=rozch(x),W+=x};
      rest while NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO);
       FOR {||lam:=(sel:=i_lam(data))->(recno()),.t.}
        //(sel)->(dbgoto(lam))
        //w:=s*(sel)->gram/1000
else
#endif
      EXECUTE {|x|W+=WARTOSC,S+=D_ILOSC,;
        SP+=PRZYCH(D_ILOSC),SR+=ROZCH(D_ILOSC),;
        WP+=PRZYCH(WARTOSC),WR+=rozch(wartosc)};
      rest while NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO);
      FOR {||lam:=(sel:=i_lam(data))->(recno()),.t.}
       // (sel)->(dbgoto(lam))
#ifdef D_GRAM
endif
#endif
#else
#ifdef D_GRAM
if kg_flag
      EXECUTE {|x|S+=D_ILOSC,;
        SP+=PRZYCH(D_ILOSC),SR+=ROZCH(D_ILOSC),;
        WP+=PRZYCH(x:=ilosc*(sel)->gram/1000),WR+=rozch(x),W+=x};
      rest while NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO);
      FOR {||lam:=(sel:=i_lam(data))->(recno()),.t.}
      //  (sel)->(dbgoto(lam))
        //w:=s*(sel)->gram/1000
else
#endif
      EXECUTE {|x|S+=D_ILOSC,;
        SP+=PRZYCH(D_ILOSC),SR+=ROZCH(D_ILOSC),;
        WP+=PRZYCH(x:=ilosc*(sel)->cenA),WR+=rozch(x),W+=x};
      rest while NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO);
      FOR {||lam:=(sel:=i_lam(data))->(recno()),.t.}
      // .and. sel=i_lam(data) .and. lam=(sel)->(recno())
        //(sel)->(dbgoto(lam))
        //w:=s*(sel)->cenA
#ifdef D_GRAM
endif
#endif
#endif

    PTOT+=WP
    RTOT+=WR
    pgr+=wp
    rgr+=wr
    PT+=WP
    RT+=WR
#ifdef D_JMTOT
      gri[nmat,3]+=sp
      gri[nmat,4]+=sr
#endif

  if !t_gr

    ?? izreS(SP,(sel)->przel)+"|"
    if wa_flag
    ?? TrAN(WP,"@EZ ",D_A_ZAOKR,11)+"|"
    endif
    ?? izreS(SR,(sel)->przel)+"|"
    if wa_flag
    ?? TrAN(WR,"@EZ ",D_A_ZAOKR,11)+"|"
    endif
/*
    if NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO) .and. !eof()
       njob:=lam:=(SEL:=I_LAM(data))->(recno())
       loop
    endif
*/
    ?? izreS(S,(sel)->przel)+"|"
    if wa_flag
    ?? TrAN(W,"@E ",D_A_ZAOKR,11)+"|"
    endif
    ?? (SEL)->jM+"|"
    if wa_flag
#ifdef A_PCL
#define D_PCL 130
#else
#define D_PCL 136
#endif
njob:=D_PCL-pcol()

#ifdef A_17CPI
    if len(trim((sel)->NAZWA))>njob
#else
    if len(trim((sel)->NAZWA))/1.2>njob
#endif
    do while substr((sel)->NAZWA,njob+1,1)>="0";--njob;enddo
    ?? left((SEL)->NAZWA,njob)
    ?  padl(trim(subs((SEL)->NAZWA,njob+1)),D_PCL)
    else
    ?? cpad((SEL)->NAZWA,D_PCL-pcol(),17,0)
    endif
    else
    ?? cpad((SEL)->NAZWA,1.2*P_COLN-pcol(),12,0)
    endif
  endif
#undef D_PCL
    if NR_MAG+INDEX+DTOS(DATA)>STANY->(NR_MAG+index)+DTOS(DO) .or. eof()
#ifdef A_WA
      exit
#else
      if s=0 .or. (sel)->cenA=(SEL:=I_LAM(do))->cenA
         exit
      endif
      lam:=(SEL)->(recno())
      w:=s*(sel)->cenA
#endif
    else
       lam:=(SEL:=I_LAM(data))->(recno())
    endif
#ifdef D_JMTOT
  if 0=(nmat:=ascan(gri,{|x|x[1]=(sel)->jM}))
     aadd(gri,{(sel)->jM,0,0,0,0})
     nmat:=len(gri)
  endif
#endif
  enddo

    SKT+=w
    SKTOT+=W
    skgr+=w
#ifdef D_JMTOT
    gri[nmat,5]+=s
#endif
  SELECT STANY
  SKIP

  if EvaldB(BKEY)#last_gr
  if wa_flag
#ifndef D_JMTOT
     if !t_gr
#endif
       ?? speC(HB_BCHAR(13)+P_UON+SPACE(136))
       ?? spec(P_UOFF)
#ifdef D_JMTOT
       ? HEADER_1+TraN(gri[1,2],"######"+ILPIC)+"|"+TrAN(spgr,"@E ",D_A_ZAOKR,11)+"|"+;
       TraN(gri[1,3],"@Z ######"+ILPIC)+"|"+TrAN(pgr,"@EZ ",D_A_ZAOKR,11)+"|"+;
       TraN(gri[1,4],"@Z ######"+ILPIC)+"|"+TrAN(rgr,"@EZ ",D_A_ZAOKR,11)+"|"+;
       TraN(gri[1,5],"######"+ILPIC)+"|"+TrAN(skgr,"@E ",D_A_ZAOKR,11)+"|"+;
       gri[1,1]+if(i_gr=2,"|RAZEM MAGAZYN ","|RAZEM GRUPA ")+TranR(last_gr,"##/"+ INDEXPIC )
       for nmat=2 to len(gri)
          if gri[nmat,2]=0 .and. gri[nmat,3]=0 .and. gri[nmat,4]=0 .and. gri[nmat,5]=0
             continue
          endif
          ? HEADER_1+TraN(gri[nmat,2],"######"+ILPIC)+"            |"+;
          TraN(gri[nmat,3],"######"+ILPIC)+"            |"+;
          TraN(gri[nmat,4],"######"+ILPIC)+"            |"+;
          TraN(gri[nmat,5],"######"+ILPIC)+"            |"+gri[nmat,1]+"|"
       next
       gri:={}
#else
     endif
      ? HEADER_1+TrAN(SPgr,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(Pgr,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(Rgr,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(SKgr,"@E ",D_A_ZAOKR,22)+"|    |"+if(i_gr=2,"RAZEM MAGAZYN ","RAZEM GRUPA ")+TranR(last_gr,"##/"+ INDEXPIC )
#endif
  else
       ?? speC(HB_BCHAR(13)+P_UON+SPACE(90))
       ?? spec(P_UOFF)
  endif
     if !t_gr
       ?
     endif
      spgr:=pgr:=rgr:=skgr:=0
     last_gr:=""
  endif

ENDDO
if strona>0
   if wa_flag
   ?? speC(HB_BCHAR(13)+P_UON+SPACE(136))
   ?? spec(P_UOFF)
   IF t_GR
     ? Tran(SUBS(EvaldB(BKEY),3),"@R "+ INDEXPIC )+"|"
   ELSE
     ? HEADER_1
   ENDIF
   IF (SPT#SPTOT .OR. PT#PTOT .OR. RT#RTOT .OR. SKT # SKTOT) .and. i_gr#2
      ?? TrAN(SPTOT,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(PTOT,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(RTOT,"@E ",D_A_ZAOKR,22)+"|"+;
      TrAN(SKTOT,"@E ",D_A_ZAOKR,22)+"|    |... RAZEM MAGAZYN "+mag_biez+" ..."
      ? HEADER_1
   ENDIF
   ?? TrAN(SPT,"@E ",D_A_ZAOKR,22)+"|"+;
   TrAN(PT,"@E ",D_A_ZAOKR,22)+"|"+;
   TrAN(RT,"@E ",D_A_ZAOKR,22)+"|"+;
   TrAN(SKT,"@E ",D_A_ZAOKR,22)+"|    | ..... RAZEM ......."
   else
       ?? speC(HB_BCHAR(13)+P_UON+SPACE(90))
       ?? spec(P_UOFF)
   endif
endif
#undef D_A_ZAOKR
#undef HEADER_3
//#undef F_JMO
//#undef D_ILOSC
************
ELSE
************
flag:=od>do.or.tak(hb_UTF8ToStr("CZY DRUKOWAĆ STANY MATERIAŁÓW BEZ RUCHU"),22)
#ifdef A_JMO
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",22,,miar_opcja,.F.)
#else
#ifdef A_JMALTTOT
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
#endif
#endif
cedat:=od
#ifndef A_WA
if od>do.and.tak(hb_UTF8ToStr("CZY STAN KOŃCOWY Z DNIA ")+dtoc(do),22)
   cedat:=do
endif
#endif
private mag_biez
mag_biez:=nr_mag
mag_poz:=max(1,ascan(magazyny,mag_biez))

@ 8,0 CLEAR 

DO CASE
  CASE OD>DatY->d_z_MIES1
    D_G:=DatY->d_z_MIES1
  CASE OD>DatY->d_z_MIES2
    D_G:=DatY->d_z_MIES2
  OTHERWISE
    D_G:=DatY->d_z_ROK
ENDCASE

DO WHILE EvaldB(BKEY)<=I_DO .AND. !EOF()

   TXT:=NR_MAG+INDEX+DTOS(d_g)+"Z"
#ifdef A_WA
#ifdef A_JMALTTOT
   if jm_o
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=SJMO_ROKU
    W:=WART_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=SJMO_MIES2
    W:=WART_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=SJMO_MIES1
    W:=WART_MIES1
  ENDCASE
   else
#endif
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=ZAMKN_ROKU
    W:=WART_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=ZAMK_MIES2
    W:=WART_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=ZAMK_MIES1
    W:=WART_MIES1
  ENDCASE
#ifdef A_JMALTTOT
 endif
#endif
#else
   DO CASE
  CASE D_G=DatY->d_z_ROK
    S:=ZAMKN_ROKU
  CASE D_G=DatY->d_z_MIES2
    S:=ZAMK_MIES2
  CASE D_G=DatY->d_z_MIES1
    S:=ZAMK_MIES1
  ENDCASE
#endif

  SELECT MAIN

    seek TXT
#ifdef A_WA
    EXECUTE {||W+=WARTOSC,S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
#else
    EXECUTE {||S+=D_ILOSC} rest while NR_MAG+INDEX+DTOS(DATA)<STANY->(NR_MAG+index)+DTOS(OD)
#endif
    if od>do
       sel:=i_lam(cedat)
       if nmat=NIL
          nmat:=(sel)->jM
          i_gr:=0
       endif
       if nmat=(sel)->jM
/*
#ifdef D_JMO
          i_gr+=if(jm_o,s D_JMO (sel)->przel,s)
#else
*/
          i_gr+=s
//#endif
       else
          nmat:=""
          i_gr:=0
       endif
    elseif round(s,ILDEC)#0 .or. round(w,A_ZAOKR)#0
       if flag .and. (nr_mag+INDEX+DTOS(DATA)>STANY->(nr_mag+index)+DTOS(DO) .or. eof())
         lam:=(SEL:=I_LAM(do))->(recno())
       else
         lam:=(sel:=i_lam(od))->(recno())
       endif
    else
       lam:=(SEL:=I_LAM(data))->(recno())
       w:=s:=0
    endif
#ifndef A_WA
    w:=s*(sel)->cenA
#endif
    njob:=.f.

  do while flag .and. (round(s,ILDEC)#0 .or. round(w,A_ZAOKR)#0) .or. nr_mag+INDEX+DTOS(DATA)<=STANY->(nr_mag+index)+DTOS(DO) .and. !eof()

    SP:=SR:=WP:=WR:=0

    IF prow()+5>P_ROWN .OR. STANY->NR_MAG#MAG_BIEZ
      IF STRONA>0
      IF STANY->NR_MAG#MAG_BIEZ
        ?? speC(HB_BCHAR(13)+P_UON+SPACE(91))
        ?? spec(P_UOFF)
        if od>do
#ifdef D_GRAM
          ? " RAZEM MAGAZYN",mag_biez,"....                "+TraN(rtot,"#### ### ###.##")+"|"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#else
          ? " RAZEM MAGAZYN",mag_biez,"....                               |"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#endif
          nmat:=NIL
        else
          ? " RAZEM MAGAZYN",mag_biez,"....             |"+TrAN(PTOT,"@E ",A_ZAOKR,22)+"|"+TrAN(RTOT,"@E ",A_ZAOKR,22)+"|"+TrAN(sktot,"@E ",A_ZAOKR,33)
        endif
        mag_biez=STANY->nr_mag
        mag_poz:=max(1,ascan(magazyny,mag_biez))
        PT+=PTOT
        RT+=RTOT
        PTOT:=RTOT:=SKTOT:=0
      ELSEIF od>do
          ?? speC(HB_BCHAR(13)+P_UON+SPACE(115))
          ?? spec(P_UOFF)
#ifdef D_GRAM
          ? " Do przeniesienia....                 "+TraN(rtot,"#### ### ###.##")+"|"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#else
          ? " Do przeniesienia....                                |"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+" "+TrAN(sktot,"@E ",A_ZAOKR,16)
#endif
      ENDIF
        specout(ccpi(4)+HB_BCHAR(13)+HB_BCHAR(12))
        setprc(0,0)
      else
         print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      if od>do
      #ifdef A_WA
      ? hb_UTF8ToStr("STAN POCZĄTKOWY MAGAZYNU DNIA "),OD,"."
      #else
      if cedat=od
      ? hb_UTF8ToStr("STAN POCZĄTKOWY MAGAZYNU DNIA "),OD,"."
      else
      ? hb_UTF8ToStr("STAN KOŃCOWY MAGAZYNU DNIA "),DO,"."
      endif
      #endif
      else
      ? hb_UTF8ToStr("KARTOTEKA MATERIAŁOWA OD DNIA "),OD," DO ",DO,"."
      endif
/*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      ?
      ? trim(subs(magazyny[mag_poz],4))+' '+adres_mag[mag_poz]
      ?
      ? "Strona"+str(++strona,3)
      if od<=do
      ? ccpi(7)
        ? hb_UTF8ToStr("DOKUMENT   |DATA |KOSZTY|   CENA   |  PRZYCHÓD            |   ROZCHÓD            |        STAN                     |")
        ?? speC(P_UON)
        ? hb_UTF8ToStr("           |     |      |          |  ILOŚĆ   |  "+WANAZ+"  |  ILOŚĆ   |  "+WANAZ+"  |   ILOŚĆ  |   CENA   |  "+WANAZ+"  |")
        ?? speC(P_UOFF)
      else
      ? CCPI(5)
#ifdef D_GRAM
        ? speC(P_UON)+hb_UTF8ToStr(" KOD I NAZWA MATERIAŁU                   | WAGA [kg] | CENA EWID.|   STAN   MIARA|  "+WANAZ+"   ")
        ?? speC(P_UOFF)
#else
        ? speC(P_UON)+hb_UTF8ToStr(" KOD I NAZWA MATERIAŁU                               | CENA EWID.|   STAN   MIARA|  "+WANAZ+"   ")
        ?? speC(P_UOFF)
#endif
      if sktot#0
#ifdef D_GRAM
          ? "  Z przeniesienia....                 "+TraN(rtot,"#### ### ###.##")+"|"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#else
          ? "  Z przeniesienia....                                |"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+" "+TrAN(sktot,"@E ",A_ZAOKR,16)
#endif
          ?? speC(HB_BCHAR(13)+P_UON+SPACE(93))
          ?? spec(P_UOFF)
      endif
      endif
    ENDIF
    if njob
       ? HEADER_2
    else
       ? Tran((SEL)->INDEX,"@R "+ INDEXPIC )+" "
    endif
    if (flag .or. njob) .and. (nr_mag+INDEX+DTOS(DATA)>STANY->(nr_mag+index)+DTOS(DO) .or. eof())
       if od>do
#ifdef D_GRAM
          rtot+=wag:=s*(sel)->gram/1000
          ?? cpad((SEL)->NAZWA,40-pcol(),12,0)
          ?? spec(HB_BCHAR(13)+space(41))+"|"+Tran(waG,"#### ###.##")+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,11)+"|"+izreS(S,(sel)->przel)+" "+(SEL)->jM+"|"+TrAN(w,"@E ",A_ZAOKR,11)
#else
          ?? cpad((SEL)->NAZWA,52-pcol(),12,0)
          ?? spec(HB_BCHAR(13)+space(53))+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,11)+"|"+izreS(S,(sel)->przel)+" "+(SEL)->jM+"|"+TrAN(w,"@E ",A_ZAOKR,11)
#endif
       else
          ?? (SEL)->NAZWA
          ?? spec(HB_BCHAR(13)+space(77))+(sel)->jm+"|"+izreS(S,(sel)->przel)+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,10)+"|"+TrAN(w,"@E ",A_ZAOKR,11)+"|"
       endif
       exit
    endif
    ?? (SEL)->NAZWA
    ?? spec(HB_BCHAR(13)+space(77))+(sel)->jm+"|"+izreS(S,(sel)->przel)+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,10)+"|"+TrAN(w,"@E ",A_ZAOKR,11)+"|"
    do while NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO) .AND. !EOF()
      lam:=(sel:=i_lam(data))->(recno())
    // .and. sel=i_lam(data) .and. lam=(sel)->(recno())
      S+=is:=D_ILOSC
#ifdef A_WA
      W+=ws:=WARTOSC
      x:=ws/is
#ifdef A_CK
      IF fieldpos('cena_k')<>0
         x:=cena_k
      ENDIF
#endif
#else
      x:=(sel)->cenA
      ws:=ilosc*x
      w:=s*x
#endif
#ifdef A_OLZA
      DM->(DBSEEK(MAIN->(KEY_DOK+MAIN->NR_DOWODU),.F.))
      ? ccpi(7)+SMB_DOW,NR_DOWODU+"/"+STR(D_LPVAL(POZYCJA),3)+"|"+DTOV(DATA)+"|"+;
        dm->konto_kosz+"|"+dm->stano_kosz+"|"+NR_ZLEC+"|"
        ?? ccpi(4)+;
        izreS(PRZYCH(is),(sel)->przel)+"|"+;
        TrAN(PRZYCH(ws),"@EZ ",A_ZAOKR,11)+"|"+;
        izreS(ROZCH(Is),(sel)->przel)+"|"+;
        TrAN(rozch(ws),"@EZ ",A_ZAOKR,11)+"|"+;
        izreS(S,(sel)->przel)+"|"+;
        TrAN(W,"@E ",A_ZAOKR,11)+"|"
#else
      ? SMB_DOW,NR_DOWODU+"/"+STR(D_LPVAL(POZYCJA),3)+"|"+DTOV(DATA)+"|"+nr_zleC+"|"+;
        TrAN(x,"@EZ ",A_ZAOKR,10)+"|"+;
        izreS(PRZYCH(is),(sel)->przel)+"|"+;
        TrAN(PRZYCH(ws),"@EZ ",A_ZAOKR,11)+"|"+;
        izreS(ROZCH(Is),(sel)->przel)+"|"+;
        TrAN(rozch(ws),"@EZ ",A_ZAOKR,11)+"|"+;
        izreS(S,(sel)->przel)+"|"+;
        TrAN(if(round(s,ILDEC)=0,0,w/s),"@EZ ",A_ZAOKR,10)+"|"+;
        TrAN(W,"@E ",A_ZAOKR,11)+"|"
#endif
      SP+=PRZYCH(Is)
      SR+=ROZCH(Is)
      WP+=PRZYCH(ws)
      WR+=rozch(ws)
      skip
      IF prow()+1>P_ROWN .and. NR_MAG+INDEX+DTOS(DATA)<=STANY->(NR_MAG+index)+DTOS(DO) .AND. !EOF()
        ?? speC(HB_BCHAR(13)+P_UON+SPACE(94+22))
        ?? spec(P_UOFF)
        ? " DO PRZENIESIENIA .....            |"+izreS(Sp,(sel)->przel)+"|"+TrAN(wP,"@E ",A_ZAOKR,11)+"|"+izreS(Sr,(sel)->przel)+"|"+TrAN(wR,"@E ",A_ZAOKR,11)+"|"+izreS(s,(sel)->przel)+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,10)+"|"+TrAN(w,"@E ",A_ZAOKR,11)+"|"
        specout(ccpi(4)+HB_BCHAR(13)+HB_BCHAR(12))
        setprc(0,0)
        ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
        ?
        ? hb_UTF8ToStr("KARTOTEKA MATERIAŁOWA OD DNIA "),OD," DO ",DO,"."
/*
        if DatY->d_z_mies1>=do
          ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
          ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
*/
        ?
        ? trim(subs(magazyny[mag_poz],4))+' '+adres_mag[mag_poz]
        ?
        ? "Strona"+str(++strona,3)
        ? ccpi(7)
        ? hb_UTF8ToStr("DOKUMENT   |DATA |KOSZTY|   CENA   |  PRZYCHÓD            |   ROZCHÓD            |        STAN                     |")
        ?? speC(P_UON)
        ? hb_UTF8ToStr("           |     |      |          |  ILOŚĆ   |  "+WANAZ+"  |  ILOŚĆ   |  "+WANAZ+"  |   ILOŚĆ  |   CENA   |  "+WANAZ+"  |")
        ? " ... KONTYNUACJA "+Tran((SEL)->INDEX,"@R "+ INDEXPIC )+" "+(SEL)->NAZWA
        ?? speC(HB_BCHAR(13)+P_UON+space(77)),(SEL)->jM+"|"+izreS(S,(sel)->przel)+"|"+TrAN(if(round(s,ILDEC)=0,0,w/s),"@E ",A_ZAOKR,10)+"|"+TrAN(w,"@E ",A_ZAOKR,11)+"|"
        ?? speC(P_UOFF)
      ENDIF
    enddo
    PTOT+=WP
    RTOT+=WR
    (sel)->(dbgoto(lam))
    ?? speC(HB_BCHAR(13)+P_UON+SPACE(94+22))
    ?? spec(P_UOFF)
    //if nr_mag+INDEX+DTOS(DATA)>STANY->(nr_mag+index)+DTOS(DO) .or. eof()
#ifndef A_WA
      if round(s,ILDEC)=0 .or. (sel)->cenA=(SEL:=I_LAM(do))->cenA
#endif
         ? PADR("RAZEM "+Tran(STANY->INDEX,"@R "+ INDEXPIC )+" : ....",35)+"|"+izreS(Sp,(sel)->przel)+"|"+TrAN(wP,"@E ",A_ZAOKR,11)+"|"+izreS(Sr,(sel)->przel)+"|"+TrAN(wR,"@E ",A_ZAOKR,11)+"|                                 |"
         ?
         exit
#ifndef A_WA
      endif
      lam:=(SEL)->(recno())
      w:=s*(sel)->cenA
#endif
/*
    else
      lam:=(SEL:=I_LAM(data))->(recno())
    endif
*/
    ? PADR(" .... "+Tran(STANY->INDEX,"@R "+ INDEXPIC )+" ....",35)+"|"+izreS(Sp,(sel)->przel)+"|"+TrAN(wP,"@E ",A_ZAOKR,11)+"|"+izreS(Sr,(sel)->przel)+"|"+TrAN(wR,"@E ",A_ZAOKR,11)+"|                                 |"
    njob:=.t.
  ENDdo
  select STANY
  sktot+=W
  skip
ENDDO
IF STRONA>0
PT+=PTOT
RT+=RTOT
SKT+=SKTOT
if od>do
?? speC(HB_BCHAR(13)+P_UON+SPACE(94))
?? spec(P_UOFF)
#ifdef D_GRAM
     ? " RAZEM MAGAZYN ",mag_biez," ....                "+TraN(rtot,"#### ### ###.##")+"|"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#else
     ? " RAZEM MAGAZYN ",mag_biez," ....                               |"+TraN(i_gr,"@Z ## ### ### ### ###"+ILPIC)+"|"+TrAN(sktot,"@E ",A_ZAOKR,16)
#endif
IF PT#PTOT .OR. RT#RTOT .OR. SKT # SKTOT
     ?
#ifdef D_GRAM
     ? " RAZEM ZESTAWIENIE ....               "+TraN(rt,"#### ### ###.##")+"|                    "+TrAN(skt,"@E ",A_ZAOKR,19)
#else
     ? " RAZEM ZESTAWIENIE .... |                                        |"+TrAN(skt,"@E ",A_ZAOKR,27)
#endif
endif
else
?? speC(HB_BCHAR(13)+P_UON+SPACE(94+22))
?? spec(P_UOFF)
? " RAZEM MAGAZYN ",mag_biez,"....              |"+TrAN(PTOT,"@E ",A_ZAOKR,22)+"|"+TrAN(RTOT,"@E ",A_ZAOKR,22)+"|"+TrAN(sktot,"@E ",A_ZAOKR,33)
IF PT#PTOT .OR. RT#RTOT .OR. SKT # SKTOT
?
? " RAZEM ZESTAWIENIE ....            |"+TrAN(PT,"@E ",A_ZAOKR,22)+"|"+TrAN(RT,"@E ",A_ZAOKR,22)+"|"+TrAN(skt,"@E ",A_ZAOKR,33)
ENDIF
endif
ENDIF
ENDIF
RETURN
#undef HEADER_2
#undef HEADER_1
#undef izreS
#undef jM
****************************
static PROCEDURE W_DOK()
MEMVAR GETLIST,NAZWA_MAG,mag_poz,MAG_BIEZ,dok_par,dokumenty,dok_rozch

LOCAL OD,DO,W:=0,TXT,R,RODZ_DOK,i:=0,dok_k,dok_w,dok_i,LASTDOK,wd,vd,wpos,flag,main_flag,zby_flag,wp,wc,dok_f,wv,fi,jm_o,dok_v,longdok,wassub
local wt:=0,vt:=0,ct:=0,pt:=0,pm,df,x
OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)
  rodz_dok:=""
  aeval(dokumenty,{|x,a|i:=dok_par[a],aeval(x,{|y,b|if(i[b,1]#"P".and.!left(y,2)$rodz_dok,rodz_dok+=left(y,2)+" ",)})})
  i:=0
  rodz_dok:=rodz_dok+space(40)

@ 13,20 SAY "DOKUMENTY OBROTU MATERIAŁOWEGO " UNICODE

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
@ 20,20 SAY 'DLA DOKUMENTÓW:' UNICODE GET RODZ_DOK PICTURE "@KS40!"
READ
if readkey()=27
  break
endif

  RODZ_DOK:=ALLTRIM(RODZ_DOK)

    set order to TAG MAIN_NRK
#ifndef STANY
  SET RELATION TO INDEX INTO INDX_MAT
#else
  SET RELATION TO NR_MAG+INDEX INTO INDX_MAT
#endif
  SELECT DM
#ifdef A_ADS
  x:='DATA>="'+dtoc(OD)+'" .AND. DATA<="'+dtoc(DO)+'" .and. pozycja#'+D_LP0
  SET FILTER TO &x
#else
  SET FILTER TO DATA>=OD .AND. DATA<=DO .and. pozycja#D_LP0
#endif
  SEEK mag_biez
#ifdef A_FA
  zby_flag:=!tak("CZY PO CENACH EWIDENCYJNYCH",22)
#endif
#ifdef A_SUBDOK
  wassub:=""
#endif
IF TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.F.,.F.)
  @ 8,0 CLEAR
  DO WHILE .t.

    i:=ASCAN(DOKUMENTY[MAG_POZ],{|X|LEFT(X,2)$RODZ_DOK},++i)
    IF i=0
      EXIT
    ENDIF
    txt:=LEFT(DOKUMENTY[MAG_POZ,i],2)
#ifndef A_MM
    txt:=mag_biez+txt
#endif
#ifdef A_SUBDOK
    if txt$wassub
       loop
    else
       wassub+=txt
    endif
#endif
    IF !DBSEEK(TXT)
      LOOP
    ENDIF
    pm:=if(dok_par[mag_poz,i,1]="P",1,-1)
#ifdef A_FA
    dok_v:=zby_flag
    dok_f:=dok_par[mag_poz,i,1]="F" .and. dok_v
#else
#ifdef A_VAT
    dok_v:=dok_par[mag_poz,i,2]$"UV"
#endif
#endif

#ifdef A_VAT
#ifdef A_CENVAT
  #define WG w-wp-wc
  #define WGt wt-pt-ct
#else
  #define WG w+wv-wp-wc
  #define WGt wt+vt-pt-ct
#endif
#else
  #define WG w-wp-wc
  #define WGt wt-pt-ct
#endif

    IF prow()+1>P_ROWN
       IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
         print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE SYNTETYCZNE DOKUMENTÓW OBROTU ZA OKRES OD "),OD," DO ",DO,"."
/*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      ?
#ifdef A_FA
      if zby_flag
         ?? " PO CENACH ZAKUPU/ZBYTU"
      Else
         ?? " PO CENACH EWIDENCYJNYCH"
      endif
      ?
#endif
      ? trim(subs(magazyny[mag_poz],4)),adres_mag[mag_poz]
      ?
      ? "Strona"+str(++strona,3)
      ?
#ifdef A_FA
      if dok_f
         ? hb_UTF8ToStr("      gotówką        przelewem            kartą")
      endif
#endif
    ENDIF

#ifdef A_MM
    ? "DOKUMENTY ",txt,"    RAZEM : "
#else
    ? "DOKUMENTY ",TranR(txt,"##/XX")," RAZEM : "
#endif
#ifdef A_FA
    if dok_f
      sum wartoSC,przelewem,czekiem,warT_vaT to w,wp,wc,wv rest while txt=KEY_DOK
      wt+=w
      vt+=wv
      ct+=wc
      pt+=wp
#ifdef A_CENVAT
      ?? ltrim(TrAN(W,"@E ",A_ZAOKR,15))+" - "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15)),"VAT =",ltrim(TrAN(W-wv,"@E ",A_ZAOKR,15))
#else
      ?? ltrim(TrAN(W,"@E ",A_ZAOKR,15))+" + "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15)),"VAT =",ltrim(TrAN(W+wv,"@E ",A_ZAOKR,15))
#endif
      ?  TrAN(WG,"@EZ ",A_ZAOKR,15)+' '+TrAN(wp,"@EZ ",A_ZAOKR,15)+' '+TrAN(wc,"@EZ ",A_ZAOKR,15)
    ELSE
#endif
#ifdef A_VAT
    if dok_v
#ifdef A_FA
       sum wartoSC,warT_vaT to w,wv rest while txt=KEY_DOK
      wt+=w
      vt+=wv
    else
#else
       w:=recno()
       sum warT_vaT to wv rest while txt=KEY_DOK
       go w
      vt+=wv
    endif
#endif
#endif
#ifdef A_WE
    sum warT_ewiD to w rest while txt=KEY_DOK
#else
    W:=0
    do while txt=KEY_DOK
      SELECT MAIN
      DBSEEK(TXT+dm->nr_dowodu)
#ifdef A_WA
      exec w+=wartosc rest WHILE TXT=KEY_DOK .AND. DATA>=OD .AND. DATA<=DO
#else
#ifdef STANY
      exec {||indx_mat->(dbseek(main->(nr_mag+index),.f.)),w+=ROUND(ilosc*(i_lam(data))->cenA,A_ZAOKR)} rest WHILE TXT=KEY_DOK .AND. DATA>=OD .AND. DATA<=DO
#else
      exec {||indx_mat->(dbseek(main->index,.f.)),w+=ROUND(ilosc*(i_lam(data))->cenA,A_ZAOKR)} rest WHILE TXT=KEY_DOK .AND. DATA>=OD .AND. DATA<=DO
#endif
#endif
      if TXT#KEY_DOK
         select dm
         exit
      endif
      SELECT DM
      SEEK TXT+main->nr_dowodu  // soft seek
    enddo
#endif
      w*=pm

      wt+=w
#ifdef A_FA
    endif
#endif
  
      ?? ltrim(TrAN(W,"@E ",A_ZAOKR,19))

#ifdef A_VAT
    if dok_v
      ?? " + "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W+wv,"@E ",A_ZAOKR,15))
    endif
#endif
#ifdef A_FA
    endif
#endif

  ENDDO
if strona>0
    ? "**** RAZEM ZESTAWIENIE: "
#ifdef A_FA
    if zby_flag
#ifdef A_CENVAT
      ?? ltrim(TrAN(Wt,"@E ",A_ZAOKR,15))+" - "+ltrim(TrAN(vt,"@E ",A_ZAOKR,15)),"VAT =",ltrim(TrAN(Wt-vt,"@E ",A_ZAOKR,15))
#else
      ?? ltrim(TrAN(Wt,"@E ",A_ZAOKR,15))+" + "+ltrim(TrAN(vt,"@E ",A_ZAOKR,15)),"VAT =",ltrim(TrAN(Wt+vt,"@E ",A_ZAOKR,15))
#endif
    ELSE
#endif
      ?? ltrim(TrAN(Wt,"@E ",A_ZAOKR,19))
#ifndef A_FA
#ifdef A_VAT
    if dok_v
      ?? " + "+ltrim(TrAN(vt,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(Wt+vt,"@E ",A_ZAOKR,15))
    endif
#endif
#endif
#ifdef A_FA
    endif
#endif
endif
******
ELSE
*****
  main_flag:=tak(hb_UTF8ToStr("CZY ROZBIJAĆ DOKUMENTY NA POZYCJE"),,,.f.,.f.)
#ifdef A_JMO
  if main_flag
     jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
  endif
#define jM    (if(jm_o,jm_opcja,jm))
#endif
  @ 8,0 CLEAR
  DO WHILE .t.

    i:=ASCAN(DOKUMENTY[MAG_POZ],{|X|LEFT(X,2)$RODZ_DOK},i+1)
    IF i=0
      EXIT
    ENDIF
    txt:=LEFT(DOKUMENTY[MAG_POZ,i],2)
#ifndef A_MM
    txt:=mag_biez+txt
#endif
#ifdef A_SUBDOK
    if txt$wassub
       loop
    else
       wassub+=txt
    endif
#endif
    IF !DBSEEK(TXT)
      LOOP
    ENDIF
    dok_k:=""#dok_par[MAG_POZ,i,3]
    dok_w:=dok_par[MAG_POZ,i,2]="W"
    pm:=if(dok_par[mag_poz,i,1]="P",1,-1)
    W:=0
#ifdef A_VAT
    dok_i:=""#dok_par[mag_poz,i,3] // DOK_KON
    wv:=0
#endif
#ifdef A_FA
    wp:=wc:=0
    dok_v:=zby_flag
    dok_f:=dok_par[mag_poz,i,1]="F" .and. dok_v
#else
#ifdef A_VAT
    dok_v:=dok_par[mag_poz,i,2]$"UV"
#endif
#endif
    longdok:=.f.
    DO WHILE TXT=KEY_DOK
      if prow()+if(main_flag,D_LPVAL(pozycja)+3,3)>P_ROWN .and. prow()>15
        IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
        else
         print()
#ifdef A_OKI4W
         //landscape:=.t.
         ?? speC(eval(P_LAND,.t.)) //+"&l1O")
#endif
        ENDIF
        ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
        ?
        ? hb_UTF8ToStr("DOKUMENTY OBROTU MATERIAŁOWEGO ZA OKRES OD "),OD," DO ",DO,"."
/*
        if DatY->d_z_mies1>=do
          ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
          ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
*/
        ?
#ifdef A_FA
        if zby_flag
         ?? " PO CENACH ZAKUPU/ZBYTU"
        Else
          ?? " PO CENACH EWIDENCYJNYCH"
        endif
        ?
#endif
        ? trim(subs(magazyny[mag_poz],4))+' '+adres_mag[mag_poz]
        ?
        ? "Strona"+str(++strona,3)
      endif
      ?
#ifdef A_MM
      ? "DOKUMENTY    ",txt
#else
      ? "DOKUMENTY ",TranR(txt,"##/XX")
#endif
      IF W#0
         ?? " Z PRZENIESIENIA: "+ltrim(TrAN(W,"@E ",A_ZAOKR,15))
#ifdef A_VAT
#ifdef A_CENVAT
        if dok_f
            ?? " - "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W-wv,"@E ",A_ZAOKR,15))
        elseif dok_v
#else
        if dok_v
#endif
            ?? " + "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W+wv,"@E ",A_ZAOKR,15))
        endif
#endif
      else
       ?? subs(dokumenty[mag_poz,i],3)
      endif
      if main_flag
#ifdef A_FA
        if dok_f .and. w#0
         ? hb_UTF8ToStr("      gotówką        przelewem            kartą")
         ? TrAN(WG,"@E ",A_ZAOKR,16),' ',TrAN(Wp,"@E ",A_ZAOKR,15),' ',TrAN(Wc,"@E ",A_ZAOKR,15)
        endif
#endif
        ? speC(ccpi(7)+P_UON)
#ifdef A_MM
#define D_MM1 left(hb_UTF8ToStr("MG|Kod materiału"),3+len( INDEXPIC ))
#define D_MM2 "|"+nr_mag+"|"+Tran(index,"@R "+ INDEXPIC )+"|"
#else
#define D_MM1 left(hb_UTF8ToStr("Kod materiału"),len( INDEXPIC ))
#define D_MM2 "|"+Tran(index,"@R "+ INDEXPIC )+"|"
#endif
#ifdef A_SHORTIND
        ? hb_UTF8ToStr(" Dokument |Data |"+if(dok_k,"Konto |","")+"Pełne określenie materiału             |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
#else
   #ifdef A_KTM
      #ifdef A_OBR
        ? hb_UTF8ToStr(" Dokument |Data |"+if(dok_k,"    Konto    |","")+"Pełne określenie materiału             |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
      #else
        ? hb_UTF8ToStr(" Dokument |Data |"+if(dok_k,"Konto |","")+"Pełne określenie materiału             |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
      #endif
   #else
      #ifdef A_OLZA
        ? hb_UTF8ToStr(" Dokument|Data |"+if(dok_k,if(dok_w,"ko.ko| st.ko|","")+"Zlecenie |","")+"Pełne określenie materiału             |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
      #else
      #ifdef A_SWW
        ? hb_UTF8ToStr(" Dokument |Data |"+if(dok_k,"Konto |","")+"    Nazwa wyrobu                       |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
      #else
        ? hb_UTF8ToStr(" Dokument |Data |"+if(dok_k,"Konto |","")+"Pełne określenie materiału             |"+D_MM1+"|Cena jedn|   Ilość  |Jedn|"+WANAZ+"   |")
      #endif
      #endif
   #endif
#endif
        if pcol()<=124
          ?? padr("Dok. RAZEM",136-pcol())
        endif
        ?? speC(P_UOFF)

      else

        ? speC(P_UON)
#ifdef A_OLZA
        ? " Dokument |Data |"+if(dok_k.and.dok_w,"ko.ko| st.ko|","")
#else
#ifdef A_FA
        if dok_f
          ? ccpi(7)+hb_UTF8ToStr(" Dokument |Data | Gotówką     |Przelewem   |Kartą       |")
        else
#endif
          ? " Dokument |Data |Na podstawie |"
#ifdef A_FA
        endif
#endif
#endif

#ifdef A_VAT
          if dok_v
             ?? "    VAT     |"
          endif
#endif
        ?? hb_UTF8ToStr(""+WANAZ+"     |")
        ?? speC(ccpi(4)+P_UOFF)
      endif
      if main_flag
        do while txt=KEY_DOK .and. (prow()+D_LPVAL(pozycja)+3<=P_ROWN .or. prow()<=15)
          if !longdok
             LASTDOK:=NR_DOWODU
             wd:=0
#ifdef A_SUBOK
#ifdef A_DF
#undef dok_df
#define dok_df df
            if zby_flag.and.dok_f
               df:=dok_par[MAG_POZ,ascan(dokumenty[mag_poz],smb_dow+sub_dok,i),A_DF]
            endif
#endif
#endif
             flag:=0
          endif
          SELECT MAIN
          if !longdok
             DBSEEK(TXT+LASTDOK)
          endif
          DO WHILE TXT+lastdok=KEY_DOK+nr_dowodu
            if longdok:=prow()+4>P_ROWN
               exit
            endif
            ++flag
            r:=i_lam(dm->data)
#ifdef A_OLZA
            ? SMB_DOW+NR_DOWODU+"/"+D_LPSTR1(pozycja)+"|"+DTOV(DATA)+"|"
            if dok_k .and. dok_w
              ?? dm->konto_kosz+"|"+dm->stano_kosz+"|"
            endif
#else
            ? smb_dow+nr_dowodu+"/"+Str(D_LPVAL(pozycja),2)+"|"+DTOV(data)+"|"
#endif
            if dok_k
              ?? nr_zleC+"|"
            endif
            ?? cpad((r)->NAZWA,39,17,1)+ D_MM2
#ifdef A_FA
            if zby_flag
            if dok_f
              ?? TrAN(cena,"@E ",A_ZAOKR,9)+"|"+D_ILOUT(pm*ilosc_f,(r)->przel)+"|"+(r)->jM+"|"
              wd+=wpos:=WGR(pm*ilosc_f,cena,val(proc_vat),dok_df)/100
            else
              ?? TrAN(cena/ilosc_f,"@E ",A_ZAOKR,9)+"|"+D_ILOUT(pm*ilosc_f,(r)->przel)+"|"+(r)->jM+"|"
              wd+=wpos:=pm*cena
            endif
            else
#endif
#ifdef A_WA
              wd+=wpos:=pm*wartosc
              ?? TrAN(wartosc/ilosc,"@E ",A_ZAOKR,9)+"|"+D_ILOUT(pm*ilosc,(r)->przel)+"|"+(r)->jM+"|"
#else
              wd+=wpos:=pm*ilosc*(r)->cenA
              ?? TrAN((r)->cenA,"@E ",A_ZAOKR,9)+"|"+D_ILOUT(pm*ilosc,(r)->przel)+"|"+(r)->jM+"|"
#endif
#ifdef A_FA
            endif
#endif
            ?? TrAN(wpos,"@E ",A_ZAOKR,10)
            skip
          ENDDO
          SELECT DM
          if !longdok
#ifdef A_VAT
          if dok_v
            WV+=warT_vaT
#ifdef A_FA
            if ROUND(wd-wartosC,A_ZAOKR)#0// .or. ROUND(warT_vaT-vd,A_ZAOKR)#0
               ? hb_UTF8ToStr("NIE ZGADZA SIĘ SUMA WSZYSTKICH POZYCJI !!!")+replicate(HB_BCHAR(7),4)
            endif
            wd:=wartoSC
#endif
          endif
#endif
#ifdef A_FA
          if dok_f
#ifndef A_DF
            if ROUND(wd-wartosC,A_ZAOKR)#0// .or. ROUND(warT_vaT-vd,A_ZAOKR)#0
               ? hb_UTF8ToStr("NIE ZGADZA SIĘ SUMA WSZYSTKICH POZYCJI !!!")+replicate(HB_BCHAR(7),4)
            endif
#endif
            wd:=wartoSC
            WP+=PRZELEWEM
            WC+=CZEKIEM
          endif
#endif
#ifdef A_WE
#ifdef A_FA
          if !zby_flag
#endif
            if ROUND(pm*wd-warT_ewiD,A_ZAOKR)#0
               ? hb_UTF8ToStr("NIE ZGADZA SIĘ SUMA WSZYSTKICH POZYCJI !!!")+replicate(HB_BCHAR(7),4)
               ?? 'DOKONANO KOREKTY Z',pm*warT_ewiD,'NA',wd
               //wd:=pm*warT_ewiD
               LOCK
               warT_ewiD:=pm*wd
               UNLOCK
            endif

#ifdef A_FA
          endif
#endif
#endif
          if flag>0
            if pcol()>123 .and. set(_SET_PRINTER)
              if flag>1
                ? replicate("=",pcol()-16)+;
                   "|"+TrAN(Wd,"@E ",A_ZAOKR,15)
              endif
              ?? "*"
            else
              ?? "|"+TrAN(wd,"@E ",A_ZAOKR,10)
            endif
          endif
          w+=wd
          SKIP
          endif
        enddo

      else

        do while txt=KEY_DOK .and. prow()+3<P_ROWN
#ifdef A_VAT
          if dok_v
            WV+=warT_vaT
          endif
#endif
#ifdef A_OLZA
          ? SMB_DOW+NR_DOWODU+"/"+str(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"
          if dok_k .and. dok_w
            ?? dm->konto_kosz+"|"+dm->stano_kosz+"|"
          endif
#ifdef A_WE
          wd:=pm*warT_ewiD
#endif
#else
  #ifdef A_FA
          if dok_f
            wd:=wartoSC
            WP+=PRZELEWEM
            WC+=CZEKIEM
            ? ccpi(7)+SMB_DOW+NR_DOWODU+"/"+str(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"
#ifdef A_CENVAT
            ?? TrAN(wartoSC-przelewem-czekiem,"@E ",A_ZAOKR,12)+"|"
#else
            ?? TrAN(wartoSC-przelewem-czekiem+warT_vaT,"@E ",A_ZAOKR,12)+"|"
#endif
            ?? TrAN(przelewem,"@E ",A_ZAOKR,12)+"|"+;
               TrAN(czekiem,"@E ",A_ZAOKR,12)+"|"
          else
  #endif
            ? SMB_DOW+NR_DOWODU+"/"+str(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"+PAD(NR_FAKTURY,13)+"|"
#ifdef A_FA
          if dok_v
            wd:=wartoSC
          else
#endif
#ifdef A_WE
            wd:=warT_ewiD
#else
            select main
            seek txt+dm->nr_dowodu
#ifdef A_WA
            sum wartosc to wd rest while txt+dm->nr_dowodu=KEY_DOK+nr_dowodu
#else
            wd:=0
#ifdef STANY
            exec {||indx_mat->(dbseek(main->(nr_mag+index),.f.)),wd+=ilosc*(i_lam(data))->cenA} rest while txt+dm->nr_dowodu=KEY_DOK+nr_dowodu
#else
            exec {||indx_mat->(dbseek(main->index,.f.)),wd+=ilosc*(i_lam(data))->cenA} rest while txt+dm->nr_dowodu=KEY_DOK+nr_dowodu
#endif
#endif
            select dm
#endif
              wd*=pm
#endif
#ifdef A_FA
          endif
          endif
#endif
#ifdef A_VAT
          if dok_v
#ifdef A_FA
            ?? TrAN(warT_vaT,"@E ",A_ZAOKR,12)+"|"+;
               TrAN(wd,"@E ",A_ZAOKR,12)+"|"
            if dok_f
               ?? cpad(dost_odb,P_COLN*1.7-pcol(),17,0)
            else
               ?? cpad(dost_odb,P_COLN-pcol(),10,0)
            endif
#else
            ?? TrAN(warT_vaT,"@E ",A_ZAOKR,12)+"|"+;
               TrAN(wd,"@E ",A_ZAOKR,12)+"|"
            ?? cpad(dost_odb,P_COLN-pcol(),10,0)
#endif
          else
#endif
          ?? TrAN(wd,"@E ",A_ZAOKR,12)+"|"
          ?? cpad(dost_odb,P_COLN-pcol(),10,0)
#ifdef A_VAT
          endif
#endif
          W+=wd
          SKIP
        enddo
      endif
      ?? ccpi(4)
      if txt=KEY_DOK
        ?
#ifdef A_MM
        ? "DOKUMENTY    ",txt
#else
        ? "DOKUMENTY ",TranR(txt,"##/XX")
#endif
        ?? " PRZENIESIENIE: ", ltrim(TrAN(W,"@E ",A_ZAOKR,15))
#ifdef A_VAT
#ifdef A_CENVAT
        if dok_f
            ?? " - "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W-wv,"@E ",A_ZAOKR,15))
        elseif dok_v
#else
        if dok_v
#endif
            ?? " + "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W+wv,"@E ",A_ZAOKR,15))
        endif
#endif
#ifdef A_FA
        if dok_f
         ? hb_UTF8ToStr("      gotówką        przelewem            kartą")
         ? TrAN(WG,"@E ",A_ZAOKR,16),' ',TrAN(Wp,"@E ",A_ZAOKR,15),' ',TrAN(Wc,"@E ",A_ZAOKR,15)
        ENDIF
#endif
      ENDIF
    ENDDO
    ?
#ifdef A_MM
        ? "DOKUMENTY ",txt,"    RAZEM : "
#else
        ? "DOKUMENTY ",TranR(txt,"##/XX")," RAZEM: "
#endif
        ?? ltrim(TrAN(W,"@E ",A_ZAOKR,15))
#ifdef A_VAT
#ifdef A_CENVAT
        if dok_f
            ?? " - "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W-wv,"@E ",A_ZAOKR,15))
        elseif dok_v
#else
        if dok_v
#endif
            ?? " + "+ltrim(TrAN(wv,"@E ",A_ZAOKR,15))+" VAT = "+ltrim(TrAN(W+wv,"@E ",A_ZAOKR,15))
        endif
#endif
#ifdef A_FA
    if dok_f
         ? hb_UTF8ToStr("      gotówką        przelewem            kartą")
         ? TrAN(WG,"@E ",A_ZAOKR,16),' ',TrAN(Wp,"@E ",A_ZAOKR,15),' ',TrAN(Wc,"@E ",A_ZAOKR,15)
    ENDIF
#endif
  enddo
ENDIF
RETURN
#undef WG
#undef D_ILOUT
#undef jM
#undef FIRMY
**************
#ifdef A_KPR
static procedure w_kpr()
local od,do,dm,coltot[20],strontot[20],i,mag_poz,r,wb,lp,nr,x,y,z,v
OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)
@ 13,10 SAY "WYDRUK PODATKOWEJ KSIĘGI PRZYCHODÓW I ROZCHODÓW" UNICODE

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

select DM

  SET ORDER TO TAG DM_DATA

  @ 18,10 say "DATA od:" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,30 say "DATA do:" get do valid {||do:=max(od,do),.t.}
READ
if readkey()=27
  break
endif
set filter to !empty(val(nr_kpr))
x:=dtos(od)
#ifndef A_MM
    x:=mag_biez+x
#endif
seek x
if mag_biez#D_MM .or. data>do
   return
endif
i:=month(do)#month(od).and.day(od)=1 .and. day(do+1)=1 .and. TAK("CZY ZESTAWIENIE ZBIORCZE",22,,.T.,.F.)
#ifdef A_LPKPR
if !i
   nr:=val(nr_kpr)
   @ 18,50 say "LP od:" get nr
   READ

endif
#endif
#ifdef D_HWPRN
   memvar->Landscape:=.t.
#endif
   print()
IF i
    if data<=do .and. mag_biez=D_MM
      @ 8,0 CLEAR
      setprc(0,0)
      lp:=0
      STRONA:=1
#ifdef A_PCL
      memvar->landscape:=.f.
      ?? speC(eval(P_LAND,.t.)+eval(P_MARGIN,10))
#endif
      ?? firma_n,' ',year(data)," r."
      ?? padl(left(firma_a,1+at(", ",firma_a))+dtoc(DatE()),80-pcol())
      ?
      afill(coltot,0)
      ? hb_UTF8ToStr("PODATKOWA KSIĘGA PRZYCHODÓW I ROZCHODÓW - ZESTAWIENIE ZBIORCZE")
#ifdef A_PCL
      ? ccpi(7)
#else
      ? ccpi(8)
#endif
IF dtos(od)>'2006'
?? speC(P_12LPI)
? hb_UTF8ToStr("┌───────────────┬───────────────────────────────────┬───────────┬───────────┬───────────────────────────────────────────────┬───────────┬───────────┬───────────┐")
? hb_UTF8ToStr("                               Przychód                 Zakup       Koszty             Wydatki (koszty)")
? hb_UTF8ToStr("│               ├───────────┬───────────┬───────────┤           │           ├───────────┬───────────┬───────────┬───────────┤           │           │           │")
? hb_UTF8ToStr("                 "+speC(P_SUPON)+"  Wartość  "+speC(P_SUPOFF)+"  pozostałe      razem    towarów      uboczne   "+speC(P_SUPON)+" wynagrodz."+speC(P_SUPOFF)+"  pozostałe    razem")
? hb_UTF8ToStr("│               │"+speC(P_SUPON)+"sprzedanych"+speC(P_SUPOFF)+"│           │           │           │           │"+speC(P_SUPON)+" w gotówce "+speC(P_SUPOFF)+"│           │           │           │           │           │           │")
? hb_UTF8ToStr("                 "+speC(P_SUPON)+"  towarów  "+speC(P_SUPOFF)+"  przychody    przychód  handlowych     zakupu   "+speC(P_SUPON)+"i w naturze"+speC(P_SUPOFF)+"   wydatki    wydatki")
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?              "                      7           8           9          10          11          12          13          14          15          16          17          18"
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?? speC(P_6LPI)
      do while data<=do .and. mag_biez=D_MM
      AFILL(STRONTOT,0)
      dm:=data+(32-day(data))
      dm:=min(do,dm-day(dm))
      do while data<=dm .and. mag_biez=D_MM

        r:=max(1,ascan(dokumenty[mag_poz:=max(1,ascan(magazyny,nr_mag))],smb_dow))
        if (x:=dok_par[mag_poz,r,A_KPR+1]) <> '{|'
          wb:=hb_macroBlock(x) 
        else
          wb:=&x
        endif
        aeval(getlines(dok_par[mag_poz,r,A_KPR],","),{|i,j|i:=val(i),j:=eval(wb,i),if(valtype(j)='N',(coltot[i]+=j,STRONTOT[i]+=j),)})
        skip
      enddo
      ? "|"+str(++lp,2) + padc(cmonth(dm),13)
      for i:=7 to 18
         if i<>12
            ?? "|"+strpic(strontot[i],11,A_ZAOKR,"@EZ ")
         endif
      next
      ?? "|"
      ENDDO
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
      ? '|'+PADC("RAZEM",15)
      for i:=7 to 18
         if i<>12
         ?? "|"+strpic(coltot[i],11,A_ZAOKR,"@EZ ")
         endif
      next
      ?? "|"
? hb_UTF8ToStr("└───────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┘")
ELSE
? hb_UTF8ToStr("┌───────────────┬───────────────────────────────────┬───────────┬───────────┬───────────────────────────────────────────────┬───────────┬───────────┬───────────┐")
?? speC(P_12LPI)
? hb_UTF8ToStr("                               Przychód                 Zakup       Koszty             Wydatki (koszty)")
? hb_UTF8ToStr("│               ├───────────┬───────────┬───────────┤           │           ├───────────┬───────────┬───────────┬───────────┤           │           │           │")
? hb_UTF8ToStr("                 "+speC(P_SUPON)+"  Wartość  "+speC(P_SUPOFF)+"  pozostałe      razem    towarów      uboczne   "+speC(P_SUPON)+"  koszty     wynagrodz."+speC(P_SUPOFF)+"  pozostałe    razem")
? hb_UTF8ToStr("│               │"+speC(P_SUPON)+"sprzedanych"+speC(P_SUPOFF)+"│           │           │           │           │"+speC(P_SUPON)+"  reklamy  "+speC(P_SUPOFF)+"│"+speC(P_SUPON)+" w gotówce "+speC(P_SUPOFF)+"│           │           │           │           │           │")
? hb_UTF8ToStr("                 "+speC(P_SUPON)+"  towarów  "+speC(P_SUPOFF)+"  przychody    przychód  handlowych     zakupu   "+speC(P_SUPON)+"limitowane  i w naturze"+speC(P_SUPOFF)+"   wydatki    wydatki")
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?              "                      7           8           9          10          11          12          13          14          15          16          17          18"
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?? speC(P_6LPI)
      do while data<=do .and. mag_biez=D_MM
      AFILL(STRONTOT,0)
      dm:=data+(32-day(data))
      dm:=min(do,dm-day(dm))
      do while data<=dm .and. mag_biez=D_MM
        r:=max(1,ascan(dokumenty[mag_poz:=max(1,ascan(magazyny,nr_mag))],smb_dow))
        if (x:=dok_par[mag_poz,r,A_KPR+1]) <> '{|'
          wb:=hb_macroBlock(x) 
        else
          wb:=&x
        endif
        aeval(getlines(dok_par[mag_poz,r,A_KPR],","),{|i,j|i:=val(i),j:=eval(wb,i),coltot[i]+=j,STRONTOT[i]+=j})
        skip
      enddo
      ? "|"+str(++lp,2) + padc(cmonth(dm),13)
      for i:=7 to 18
         ?? "|"+strpic(strontot[i],11,A_ZAOKR,"@EZ ")
      next
      ?? "|"
      ENDDO
? hb_UTF8ToStr("├───────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
      ? '|'+PADC("RAZEM",15)
      for i:=7 to 18
         ?? "|"+strpic(coltot[i],11,A_ZAOKR,"@EZ ")
      next
      ?? "|"
? hb_UTF8ToStr("└───────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┘")
ENDIF
   endif
ELSE
  dm:=od+(32-day(od))
  dm:=min(do,dm-day(dm))
#ifdef A_LPKPR
  set order to 1
#ifndef A_MM
  seek mag_biez
#endif

#ifdef A_LAN
  #define D_LOCK .and. reclock()
#else
  #define D_LOCK
#endif

  replace nr_kpr with '99999' for data>=od .and. data<=do D_LOCK WHILE mag_biez=D_MM

  SET ORDER TO TAG DM_DATA
    x:=dtos(od)
#ifndef A_MM
    x:=mag_biez+x
#endif
    seek x
#endif

  @ 8,0 CLEAR
#ifdef A_PCL
  memvar->landscape:=.f.
  ?? speC(eval(P_LAND,.t.))
#endif
  afill(coltot,0)
  DO WHILE data<=do .and. mag_biez=D_MM
      IF STRONA>0
#ifdef A_PCL
      ? CPAD(hb_UTF8ToStr("| RAZEM - DO PRZENIESIENIA NA NASTĘPNĄ STRONĘ"),120,20,1)
      for i:=7 to 27-v
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
         ?? "|"+strpic(coltot[i],v-1,A_ZAOKR,"@EZ ")
#else
      ? CPAD(hb_UTF8ToStr("| RAZEM - DO PRZENIESIENIA NA NASTĘPNĄ STRONĘ"),139,20,1)
      for i:=7 to 16
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
         ?? "|"+strpic(coltot[i],11,A_ZAOKR,"@EZ ")
#endif
      next
      ?? "|"
#ifdef A_PCL
      ? hb_UTF8ToStr("└───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────"+repl(padr("┴",v,"─"),20-v)+"┘")
#else
      ? hb_UTF8ToStr("└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┘")
#endif
      speCout(ccpi(4)+HB_BCHAR(13)+HB_BCHAR(12))
      setprc(0,0)
      endif
      if data>dm
           afill(coltot,0)
           dm:=data+(32-day(data))
           dm:=min(do,dm-day(dm))
           strona=0
           ?? ccpi(4)
      endif
      ?? firma_n,cmonth(dm),year(data),"r., strona:",Str(++strona,2)
      ?? padl(left(firma_a,1+at(", ",firma_a))+dtoc(DatE()),D_COLN-pcol())
      ?
      afill(strontot,0)
      ? hb_UTF8ToStr("PODATKOWA KSIĘGA PRZYCHODÓW I ROZCHODÓW")
      ? ccpi(8) //272 -12

IF dtos(od)>'2006'
#ifdef A_PCL
v:=10
? hb_UTF8ToStr("┌─────┬─────┬──────────┬───────────────────────────────────────────────────────────────────────────┬────────────────────┬─────────────────────────────┬─────────┬─────────┬───────────────────────────────────────┬─────────┐")
?? speC(P_12LPI)
? hb_UTF8ToStr("       Dzień    Numer                               Kontrahent                                                                     Przychód              Zakup    Koszty          Wydatki (koszty)")
? hb_UTF8ToStr("│     │     │          ├───────────────────────────────────────┬───────────────────────────────────┤   Opis zdarzenia   ├─────────┬─────────┬─────────┤         │         ├─────────┬─────────┬─────────┬─────────┤         │")
? hb_UTF8ToStr("  Lp.  zdarz.  dowodu            imię i nazwisko                                                                         "+speC(P_SUPON)+" Wartość "+speC(P_SUPOFF)+" pozostałe  razem    towarów   uboczne   "+speC(P_SUPON)+"wynagrodz."+speC(P_SUPOFF)+"pozostałe   razem               Uwagi")
? hb_UTF8ToStr("│     │     │          │                                       │                    adres          │   gospodarczego    │"+speC(P_SUPON)+" sprzed. "+speC(P_SUPOFF)+"│         │         │         │         │"+speC(P_SUPON)+"w gotówce"+speC(P_SUPOFF)+"│         │         │         │         │")
? hb_UTF8ToStr("       gosp. księgowego              (firma)                                                                             "+speC(P_SUPON)+" towarów "+speC(P_SUPOFF)+" przychody przychód  handlowych zakupu   "+speC(P_SUPON)+"w naturze"+speC(P_SUPOFF)+"  wydatki   wydatki")
? hb_UTF8ToStr("├─────┼─────┼──────────┼───────────────────────────────────────┼───────────────────────────────────┼────────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤")
?              "   1     2        3                   4                                        5                             6               7         8         9        10        11        12        13        14        15        16"
? hb_UTF8ToStr("├─────┼─────┼──────────┼───────────────────────────────────────┼───────────────────────────────────┼────────────────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┼─────────┤")
?? speC(P_6LPI)
#else
? hb_UTF8ToStr("┌─────┬─────┬────────────┬─────────────────────────────────────────────────────────────────────────────────────────┬───────────────────────┬───────────────────────────────────┬───────────┬───────────┬───────────────────────────────────────────────┬───────────┐")
?? speC(P_12LPI)
? hb_UTF8ToStr("                Numer                        Kontrahent                                                                                                   Przychód                 Zakup       Koszty             Wydatki (koszty)")
? hb_UTF8ToStr("│     │Dzień│            ├────────────────────────────────────────────┬────────────────────────────────────────────┤    Opis zdarzenia     ├───────────┬───────────┬───────────┤           │           ├───────────┬───────────┬───────────┬───────────┤           │")
? hb_UTF8ToStr("  Lp.          dowodu                        imię i nazwisko                                                                                "+speC(P_SUPON)+"  Wartość  "+speC(P_SUPOFF)+"  pozostałe      razem    towarów      uboczne   "+speC(P_SUPON)+" wynagrodz."+speC(P_SUPOFF)+"  pozostałe    razem")
? hb_UTF8ToStr("│     │mies.│            │                                            │              adres                         │    gospodarczego      │"+speC(P_SUPON)+"sprzedanych"+speC(P_SUPOFF)+"│           │           │           │           │"+speC(P_SUPON)+" w gotówce "+speC(P_SUPOFF)+"│           │           │           │           │")
? hb_UTF8ToStr("              księgowego                         (firma)                                                                                    "+speC(P_SUPON)+"  towarów  "+speC(P_SUPOFF)+"  przychody    przychód  handlowych     zakupu   "+speC(P_SUPON)+"i w naturze"+speC(P_SUPOFF)+"   wydatki    wydatki")
? hb_UTF8ToStr("├─────┼─────┼────────────┼────────────────────────────────────────────┼────────────────────────────────────────────┼───────────────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?              "   1     2        3                           4                                         5                                       6                7           8           9          10          11          12          13          14          15          16"
? hb_UTF8ToStr("├─────┼─────┼────────────┼────────────────────────────────────────────┼────────────────────────────────────────────┼───────────────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?? speC(P_6LPI)
#endif
ELSE
#ifdef A_PCL
v:=11
? hb_UTF8ToStr("┌─────┬─────┬──────────┬───────────────────────────────────────────────────────────────────────────┬────────────────────┬────────────────────────────────┬──────────┬──────────┬───────────────────────────────────────────┐")
?? speC(P_12LPI)
? hb_UTF8ToStr("                Numer                               Kontrahent                                                                       Przychód               Zakup      Koszty          Wydatki (koszty)")
? hb_UTF8ToStr("│     │Dzień│          ├───────────────────────────────────────┬───────────────────────────────────┤   Opis zdarzenia   ├──────────┬──────────┬──────────┤          │          ├──────────┬──────────┬──────────┬──────────┤")
? hb_UTF8ToStr("  Lp.          dowodu            imię i nazwisko                                                                         "+speC(P_SUPON)+" Wartość  "+speC(P_SUPOFF)+"  pozostałe   razem     towarów    uboczne   "+speC(P_SUPON)+" koszty    wynagrodz."+speC(P_SUPOFF)+"  pozostałe    razem")
? hb_UTF8ToStr("│     │mies.│          │                                       │                    adres          │   gospodarczego    │"+speC(P_SUPON)+" sprzed.  "+speC(P_SUPOFF)+"│          │          │          │          │"+speC(P_SUPON)+"  reklamy "+speC(P_SUPOFF)+"│"+speC(P_SUPON)+"w gotówce "+speC(P_SUPOFF)+"│          │          │")
? hb_UTF8ToStr("             księgowego              (firma)                                                                             "+speC(P_SUPON)+" towarów  "+speC(P_SUPOFF)+"  przychody  przychód  handlowych   zakupu   "+speC(P_SUPON)+"limitowane  w naturze"+speC(P_SUPOFF)+"   wydatki    wydatki")
? hb_UTF8ToStr("├─────┼─────┼──────────┼───────────────────────────────────────┼───────────────────────────────────┼────────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤")+speC(P_6LPI)
? "   1     2        3                   4                                        5                             6                7          8          9         10         11         12         13         14         15"
? hb_UTF8ToStr("├─────┼─────┼──────────┼───────────────────────────────────────┼───────────────────────────────────┼────────────────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤")
?? speC(P_6LPI)
#else
? hb_UTF8ToStr("┌─────┬─────┬────────────┬─────────────────────────────────────────────────────────────────────────────────────────┬───────────────────────┬───────────────────────────────────┬───────────┬───────────┬───────────────────────────────────────────────┬───────────┐")
?? speC(P_12LPI)
? hb_UTF8ToStr("                Numer                        Kontrahent                                                                                                   Przychód                 Zakup       Koszty             Wydatki (koszty)")
? hb_UTF8ToStr("│     │Dzień│            ├────────────────────────────────────────────┬────────────────────────────────────────────┤    Opis zdarzenia     ├───────────┬───────────┬───────────┤           │           ├───────────┬───────────┬───────────┬───────────┤           │")
? hb_UTF8ToStr("  Lp.          dowodu                        imię i nazwisko                                                                                "+speC(P_SUPON)+"  Wartość  "+speC(P_SUPOFF)+"  pozostałe      razem    towarów      uboczne   "+speC(P_SUPON)+"  koszty     wynagrodz."+speC(P_SUPOFF)+"  pozostałe    razem")
? hb_UTF8ToStr("│     │mies.│            │                                            │              adres                         │    gospodarczego      │"+speC(P_SUPON)+"sprzedanych"+speC(P_SUPOFF)+"│           │           │           │           │"+speC(P_SUPON)+"  reklamy  "+speC(P_SUPOFF)+"│"+speC(P_SUPON)+" w gotówce "+speC(P_SUPOFF)+"│           │           │           │")
? hb_UTF8ToStr("              księgowego                         (firma)                                                                                    "+speC(P_SUPON)+"  towarów  "+speC(P_SUPOFF)+"  przychody    przychód  handlowych     zakupu   "+speC(P_SUPON)+"limitowane  i w naturze"+speC(P_SUPOFF)+"   wydatki    wydatki")
? hb_UTF8ToStr("├─────┼─────┼────────────┼────────────────────────────────────────────┼────────────────────────────────────────────┼───────────────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
? "   1     2        3                           4                                         5                                       6                7           8           9          10          11          12          13          14          15          16"
? hb_UTF8ToStr("├─────┼─────┼────────────┼────────────────────────────────────────────┼────────────────────────────────────────────┼───────────────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
?? speC(P_6LPI)
#endif
ENDIF
      do while data<=dm .and. prow()<=P_ROWN .and. mag_biez=D_MM
#ifdef A_SUBDOK
#define F_SUBDOK 6
#else
#define F_SUBDOK 4
#endif
        r:=max(1,ascan(dokumenty[mag_poz:=max(1,ascan(magazyny,nr_mag))],smb_dow D_SUBKEY))
#ifdef A_LPKPR
        LOCK
        nr_kpr:=str(nr++,5)
        unlock
#endif
#ifdef A_PCL
#ifdef A_FFULL
        ? "|"+nr_kpr+"|"+str(day(data),3)+"  |"+padr(if(dok_par[mag_poz,r,1]="F",D_SUBDOK,nr_faktury),10)+;
          "|"+if(FIRMY->(dbseek(DM->(D_KH))),padr(FIRMY->D_FFULL,39)+"|"+padr(FIRMY->adres,35),padr(dost_odb,75))+;
          "|"+padr(subs(dokumenty[mag_poz,r],F_SUBDOK),20)
#else
        ? "|"+nr_kpr+"|"+str(day(data),3)+"  |"+padr(if(dok_par[mag_poz,r,1]="F",D_SUBDOK,nr_faktury),10)+;
          "|"+if(FIRMY->(dbseek(DM->(D_KH))),padr(FIRMY->D_FFULL,39)+"|"+padr(alltrim(left(FIRMY->adres,24))+','+subs(FIRMY->adres,25),35),padr(dost_odb,75))+;
          "|"+padr(subs(dokumenty[mag_poz,r],F_SUBDOK),20)
#endif
#else
        ? "|"+nr_kpr+"|"+str(day(data),3)+"  |"+padr(if(dok_par[mag_poz,r,1]="F",D_SUBDOK,nr_faktury),12)+;
          "|"+if(FIRMY->(dbseek(DM->(D_KH))),padr(FIRMY->D_FFULL,44)+"|"+padr(FIRMY->adres,44),padr(dost_odb,89))+;
          "|"+padr(subs(dokumenty[mag_poz,r],F_SUBDOK),23)
#endif
#undef D_SUBDOK
#undef D_SUBKEY
#undef F_SUBDOK
        if (x:=dok_par[mag_poz,r,A_KPR+1]) <> '{|'
          wb:=hb_macroBlock(x) 
        else
          wb:=&x
        endif
        x:=getlines(dok_par[mag_poz,r,A_KPR],",")
        z:=array(len(x))
        aeval(x,{|a,b|a:=val(a),x[b]:=a,z[b]:=eval(wb,a)})
#ifdef A_PCL
        for i:=7 to 27-v
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
          y:=ascan(x,i)
          if y<>0
             if valtype(z[y])='N'
               ?? "|"+strpic(z[y],v-1,A_ZAOKR,"@EZ ")
               coltot[i]+=z[y]
               strontot[i]+=z[y]
             else
               ?? padc(tran(z[y],),v)
             endif
          else
             ?? space(v)
          endif
#else
        for i:=7 to 16
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
          y:=ascan(x,i)
          if y<>0
             ?? "|"+strpic(z[y],11,A_ZAOKR,"@EZ ")
             coltot[i]+=z[y]
             strontot[i]+=z[y]
          else
             ?? space(12)
          endif
#endif
        next
        ?? "|"
        skip
      enddo
#ifdef A_PCL
? hb_UTF8ToStr("├─────┼─────┼──────────┼───────────────────────────────────────┼───────────────────────────────────┼────────────────────"+repl(padr("┼",v,"─"),20-v)+"┤")
#else
? hb_UTF8ToStr("├─────┼─────┼────────────┼────────────────────────────────────────────┼────────────────────────────────────────────┼───────────────────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┤")
#endif
//    ? repl("-",272)
      if strona>1
#ifdef A_PCL
         ? padr(I+" PODSUMOWANIE STRONY",120)
         for i:=7 to 27-v
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
            ?? I+strpic(strontot[i],v-1,A_ZAOKR,"@EZ ")
         next
         ?? I
#else
         ? padr("| PODSUMOWANIE STRONY",139)
         for i:=7 to 16
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
            ?? "|"+strpic(strontot[i],11,A_ZAOKR,"@EZ ")
         next
         ?? "|"
#endif
#ifdef A_PCL
         ? PADR(I+" PRZENIESIENIE Z POPRZEDNIEJ STRONY",120)
         for i:=7 to 27-v
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
            ?? I+strpic(coltot[i]-strontot[i],v-1,A_ZAOKR,"@EZ ")
         next
         ?? I
#else
         ? PADR("| PRZENIESIENIE Z POPRZEDNIEJ STRONY",139)
         for i:=7 to 16
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
            ?? "|"+strpic(coltot[i]-strontot[i],11,A_ZAOKR,"@EZ ")
         next
         ?? "|"
#endif
      endif
  ENDDO
  if strona>0
#ifdef A_PCL
      ? CPAD(I+" RAZEM",120,20,1)
      for i:=7 to 27-v
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
         ?? "|"+strpic(coltot[i],v-1,A_ZAOKR,"@EZ ")
      next
      ?? "|"
      ? hb_UTF8ToStr("└───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────"+repl(padr("┴",v,"─"),20-v)+"┘")
#else
      ? CPAD("| RAZEM",139,20,1)
      for i:=7 to 16
         if i=12 .and. dtos(od)>'2006'
            loop
         endif
         ?? "|"+strpic(coltot[i],11,A_ZAOKR,"@EZ ")
      next
      ?? "|"
      ? hb_UTF8ToStr("└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┘")
#endif
  endif
ENDIF
return
#endif
*************
#ifdef A_OLZA
static procedure w_dok_k()
MEMVAR GETLIST,KONT_KOS

LOCAL OD,DO,W,TXT,i_od,i_do,WTOT:=0,IW,j,main_flag,dok_kon,dok_p_r,r

FIELD STANO_KOSZ,KONTO_KOSZ

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)

@ 13,10 SAY "ZESTAWIENIE DOKUMENTÓW ROZCHODU DLA POSZCZEGÓLNTCH KONT KOSZTÓW" UNICODE

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

select DM

  SET ORDER TO 2
  I_OD:=I_DO:=nr_mag+smb_dow+konto_kosz
  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
  @ 20,10 SAY 'MAG-DOK-KONTO od :' GET I_OD PICTURE "@KR! ##-XX-XXXXX" valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,45 SAY 'MAG_DOK_KONTO do :' GET I_DO PICTURE "@KR! ##-XX-XXXXX" valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
READ
if readkey()=27
  break
endif

I_OD=UpP(TRIM(I_OD))
I_DO=UpP(TRIM(I_DO))

SEEK I_OD
IF TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.T.,.T.)
  @ 8,0 CLEAR

DO WHILE nr_mag+smb_dow+konto_kosz<=I_DO .AND. !EOF()

   TXT:=nr_mag+smb_dow+konto_kosz
   seek TXT+DTOS(od)
   sum -warT_ewiD to w rest while data<=do .and. nr_mag+smb_dow+konto_kosz=txt
   if nr_mag+smb_dow+konto_kosz=txt
      seek txt + HB_UCHAR(0x00A0)
   endif
   if w=0
      loop
   endif
   IF prow()>P_ROWN
      IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
     print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G KONT KOSZTÓW OD DNIA "),OD," DO ",DO,"."
/*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      ?
      ? "Strona"+str(++strona,3)
      ?
      ? hb_UTF8ToStr("     KONTO                "+WANAZ+"             NARASTAJĄCO")
   ENDIF
   ? TranR(txt,"XX-XX-XXXXX"),' ',TrAN(W,"@E ",A_ZAOKR,19),' ',TrAN(Wtot+=w,"@E ",A_ZAOKR,19)
ENDDO
******
ELSE
******
  IF main_flag:=tak(hb_UTF8ToStr("CZY ROZBIJAĆ DOKUMENTY NA POZYCJE"))
     MAIN->(ORDSETFOCUS("MAIN_NRK"))
  ENDIF
  @ 8,0 CLEAR
DO WHILE nr_mag+smb_dow+konto_kosz<=I_DO .AND. !EOF()

   TXT:=nr_mag+smb_dow+konto_kosz
   seek TXT+DTOS(od)
   IF nr_mag+smb_dow+konto_kosz+DTOS(DATA)>TXT+DTOS(DO) .OR. EOF()
      IF nr_mag+smb_dow+konto_kosz=txt
         seek TXT + HB_UCHAR(0x00A0)
      ENDIF
      LOOP
   ENDIF
   W:=0
   IF MAIN_FLAG .AND. D_LPVAL(pozycja)+4+prow()>P_ROWN .or. prow()+4>P_ROWN
      IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
     print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G KONT KOSZTÓW OD DNIA "),OD," DO ",DO,"."
/*
      if DatY->d_z_mies1>=do
         ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
         ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
*/
      ?
      ? "Strona"+str(++strona,3)
      ? speC(P_UON)
      ? hb_UTF8ToStr("DOKUMENT   |DATA | ST.KO|"+WANAZ+"     |ODBIORCA                                   ")
      ?? speC(P_UOFF)
  ENDIF
  IF MAIN_flag
     j:=max(1,ascan(dokumenty[MAG_POZ],dm->smb_dow))
    dok_p_r:=dok_par[mag_poz,j,1]
    dok_kon:=dok_par[mag_poz,j,3]
    ? speC(P_UON)+hb_UTF8ToStr("Lp|Pełne określenie materiału |")
    ?? ccpi(5)+hb_UTF8ToStr(" Kod materiału|")
    ?? if(""=dok_kon,"",ccpi(7)+"Zlecenie |")
    ?? ccpi(5)+hb_UTF8ToStr("    Cena |   Ilość  |Jedn|"+WANAZ+"   ")
    ?? speC(ccpi(4)+P_UOFF)
  endif
  ?
  ? TranR(txt,"XX-XX-XXXXX")
  ?
  iw:=0
  do while nr_mag+smb_dow+konto_kosz=txt .AND. data<=do
     IF MAIN_FLAG .AND. D_LPVAL(pozycja)+1+prow()>P_ROWN .or. prow()+1>P_ROWN
        ?? speC(HB_BCHAR(13)+P_UON+SPACE(80))
        ?? spec(P_UOFF)
        ? "DO PRZENIESIENIA |"+TrAN(W,"@E ",A_ZAOKR,18)+"|                                           "
        ?? speC(P_UOFF)
        ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
        setprc(0,0)
        ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
        ?
        ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G KONT KOSZTÓW OD DNIA "),OD," DO ",DO,"."
/*
        if DatY->d_z_mies1>=do
           ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
           ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
*/
        ?
        ? "Strona"+str(++strona,3)
        ? speC(P_UON)
        ? hb_UTF8ToStr("DOKUMENT   |DATA | ST.KO|"+WANAZ+"     |ODBIORCA                                   ")
        ? " Z PRZENIESIENIA |"+TrAN(W,"@E ",A_ZAOKR,18)+"|                                           "
        ?? speC(P_UOFF)
        IF MAIN_flag
          ? speC(P_UON)+hb_UTF8ToStr("Lp|Pełne określenie materiału |")
          ?? ccpi(5)+hb_UTF8ToStr(" Kod materiału|")
          ?? if(""=dok_kon,"",ccpi(7)+"Zlecenie |")
          ?? ccpi(5)+hb_UTF8ToStr("    Cena |   Ilość  |Jedn|"+WANAZ+"   ")
          ?? speC(ccpi(4)+P_UOFF)
        endif
    ENDIF
    W-=warT_ewiD
    iw-=warT_ewiD
    ? SMB_DOW+' '+NR_DOWODU+"/"+STR(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"
    ?? stano_kosz+"|"
    ?? strpic(-warT_ewiD,12,A_ZAOKR,"@E ")+"|"
    ?? cpad(dost_odb,80-pcol(),10,0)
    IF MAIN_FLAG .AND. MAIN->(DBSEEK(DM->(KEY_DOK+NR_DOWODU)))
    SELECT MAIN
    DO while KEY_DOK+nr_dowodu=dm->(KEY_DOK+NR_DOWODU)
#ifdef STANY
      indx_mat->(dbseek(main->nr_mag+main->index,.f.))
#else
      indx_mat->(dbseek(main->index,.f.))
#endif
      r:=i_lam(dm->data)
      if pozycja=dm->pozycja
        ?? speC(P_UON)
      endif
      ? str(D_LPVAL(pozycja),2)+"|",cpad((r)->NAZWA,27,10,1),"|",ccpi(5)+tran(index,"@R "+ INDEXPIC )+"|"+if(""=dok_kon,"",ccpi(7)+nr_zleC+"|"),ccpi(5)+TrAN(waRTOSC/ILOSC,"@E ",A_ZAOKR,9)+"|"
      ?? strTraN(strpic(if(dok_p_r#"P",-ilosc,ilosc),10,3),".000",".   ")+"|"+(r)->jm+"|"+strpic(if(dok_p_r#"P",-WARTOSC,wartosc),10,A_ZAOKR,"@E "),ccpi(4)
      skip
    ENDDO
    ?? speC(P_UOFF)
    SELECT DM
    ENDIF
    skip
  enddo
  if nr_mag+smb_dow+konto_kosz=txt
     seek txt + HB_UCHAR(0x00A0)
  endif
  ?? speC(HB_BCHAR(13)+P_UON+SPACE(80))
  ?? spec(P_UOFF)
  ? " RAZEM "+TranR(txt,"XX-XX-XXXXX")+"|"+TrAN(W,"@E ",A_ZAOKR,18)+hb_UTF8ToStr("|  NARASTAJĄCO: ")+TrAN(Wtot+=w,"@E ",A_ZAOKR,18)
ENDDO
ENDIF
return
********************************************
static procedure w_dok_s()
MEMVAR GETLIST,stanowis

LOCAL OD,DO,W,TXT,i_od,i_do,WTOT:=0,IW,j,main_flag,dok_kon,dok_p_r,r

FIELD STANO_KOSZ,KONTO_KOSZ

OD := DatY->data_gran+1
DO := IF(DatY->data_gran>DatY->d_z_MIES2,DatE(),DatY->d_z_MIES1)

@ 13,10 SAY "ZESTAWIENIE DOKUMENTÓW ROZCHODU DLA POSZCZEGÓLNTCH STANOWISK KOSZTÓW" UNICODE

@ 15,20 say "PODAJ ZAKRES DANYCH DLA WYDRUKU"

select DM

  SET ORDER TO 3
  I_OD:=I_DO:=nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)
  @ 18,20 say "DATA od   :" get od valid {||od:=max(od,DatY->d_z_rok+1),do:=max(od,do),.t.}
  @ 18,50 say "DATA do   :" get do valid {||do:=max(od,do),.t.}
  @ 20,10 SAY 'MAG-DOK-KONTO od :' GET I_OD PICTURE "@KR! ##-XX-XXXXXX/X" valid {||if(i_do<i_od,(i_do:=i_od),),.t.}
  @ 20,45 SAY 'MAG-DOK-KONTO do :' GET I_DO PICTURE "@KR! ##-XX-XXXXXX/X" valid {||if(i_do<i_od,(i_do:=i_od)=NIL,.t.)}
READ
if readkey()=27
  break
endif

I_OD=UpP(TRIM(I_OD))
I_DO=UpP(TRIM(I_DO))

SEEK I_OD
IF TAK("CZY ZESTAWIENIE SYNTETYCZNE",22,,.T.,.T.)
  @ 8,0 CLEAR

DO WHILE nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)<=I_DO .AND. !EOF()

   TXT:=nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)
   seek TXT+DTOS(od)
   sum -warT_ewiD to w rest while data<=do .and. nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)=txt
   if nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)=txt
      seek txt + HB_UCHAR(0x00A0)
   endif
   if w=0
      loop
   endif
   IF prow()>P_ROWN
      IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
     print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G STANOWISK KOSZTÓW OD DNIA "),OD," DO ",DO,"."
      /*
      if DatY->d_z_mies1>=do
        ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
        ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
      */
      ?
      ? "Strona"+str(++strona,3)
      ?
      ? hb_UTF8ToStr("     STANOWISKO                 "+WANAZ+"          NARASTAJĄCO")
   ENDIF
   ? TranR(txt,"XX-XX-XXXXXX/X")+' '+TrAN(W,"@E ",A_ZAOKR,15)+' '+TrAN(Wtot+=w,"@E ",A_ZAOKR,19)
ENDDO
*******
ELSE
*******
  IF main_flag:=tak(hb_UTF8ToStr("CZY ROZBIJAĆ DOKUMENTY NA POZYCJE"))
     MAIN->(ordsetfocus("MAIN_NRK"))
  ENDIF
  @ 8,0 CLEAR 
DO WHILE nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)<=I_DO .AND. !EOF()

   TXT:=nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)
   seek TXT+DTOS(od)
   IF nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)+DTOS(DATA)>TXT+DTOS(DO) .OR. EOF()
      IF nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)=txt
         seek TXT + HB_UCHAR(0x00A0)
      ENDIF
      LOOP
   ENDIF
   W:=0
   IF MAIN_FLAG .AND. D_LPVAL(pozycja)+4+prow()>P_ROWN .or. prow()+4>P_ROWN
      IF STRONA>0
         ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
         setprc(0,0)
      else
     print()
      ENDIF
      ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
      ?
      ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G STANOWISK KOSZTÓW OD DNIA "),OD," DO ",DO,"."
      /*
      if DatY->d_z_mies1>=do
         ? "PO ZAMKNIĘCIU MIESIĄCA"
      ELSE
         ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
      ENDIF
      */
      ?
      ? "Strona"+str(++strona,3)
      ? speC(P_UON)
      ? hb_UTF8ToStr("DOKUMENT   |DATA |KO.KO|"+WANAZ+"     |ODBIORCA                                   ")
      ?? speC(P_UOFF)
  ENDIF
  IF MAIN_flag
    j:=max(1,ascan(dokumenty[MAG_POZ],dm->smb_dow))
    dok_p_r:=dok_par[mag_poz,j,1]
    dok_kon:=dok_par[mag_poz,j,3]
    ? speC(P_UON)+hb_UTF8ToStr("Lp|Pełne określenie materiału |")
    ?? ccpi(5)+hb_UTF8ToStr(" Kod materiału|")
    ?? if(""=dok_kon,"",ccpi(7)+"Zlecenie |")
    ?? ccpi(5)+hb_UTF8ToStr("    Cena |   Ilość  |Jedn|"+WANAZ+"   ")
    ?? speC(ccpi(4)+P_UOFF)
  endif
  ?
  ? TranR(txt,"XX-XX-XXXXXX/X")
  ?
  iw:=0
  do while nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)=txt .AND. data<=do
     IF MAIN_FLAG .AND. D_LPVAL(pozycja)+1+prow()>P_ROWN .or. prow()+1>P_ROWN
        ?? speC(HB_BCHAR(13)+P_UON+SPACE(80))
        ?? spec(P_UOFF)
        ? "DO PRZENIESIENIA |"+TrAN(W,"@E ",A_ZAOKR,17)+"|                                           "
        ?? speC(P_UOFF)
        ?? speC(HB_BCHAR(13)+HB_BCHAR(12))
        setprc(0,0)
        ?? padr(firma_n,P_COLN-16)," dnia ",dtoc(DatE())
        ?
        ? hb_UTF8ToStr("ZESTAWIENIE ROZCHODU W/G STANOWISK KOSZTÓW OD DNIA "),OD," DO ",DO,"."
        /*
        if DatY->d_z_mies1>=do
           ? "PO ZAMKNIĘCIU MIESIĄCA"
        ELSE
           ? "PRZED ZAMKNIĘCIEM MIESIĄCA"
        ENDIF
        */
        ?
        ? "Strona"+str(++strona,3)
        ? speC(P_UON)
        ? hb_UTF8ToStr("DOKUMENT   |DATA |KO.KO|"+WANAZ+"     |ODBIORCA                                   ")
        ? " Z PRZENIESIENIA |"+TrAN(W,"@E ",A_ZAOKR,17)+"|                                           "
        ?? speC(P_UOFF)
        IF MAIN_flag
          ? speC(P_UON)+hb_UTF8ToStr("Lp|Pełne określenie materiału |")
          ?? ccpi(5)+hb_UTF8ToStr(" Kod materiału|")
          ?? if(""=dok_kon,"",ccpi(7)+"Zlecenie |")
          ?? ccpi(5)+hb_UTF8ToStr("    Cena |   Ilość  |Jedn|"+WANAZ+"   ")
          ?? speC(ccpi(4)+P_UOFF)
        endif
    ENDIF
    W-=warT_ewiD
    iw-=warT_ewiD
    ? SMB_DOW+' '+NR_DOWODU+"/"+STR(D_LPVAL(POZYCJA),2)+"|"+DTOV(DATA)+"|"
    ?? KONTO_kosz+"|"
    ?? strpic(-warT_ewiD,12,A_ZAOKR,"@E ")+"|"
    ?? cpad(dost_odb,80-pcol(),10,0)
    IF MAIN_FLAG .AND. MAIN->(DBSEEK(DM->(KEY_DOK+NR_DOWODU)))
    SELECT MAIN
    DO while KEY_DOK+nr_dowodu=dm->(KEY_DOK+NR_DOWODU)
#ifdef STANY
      indx_mat->(dbseek(main->nr_mag+main->index,.f.))
#else
      indx_mat->(dbseek(main->index,.f.))
#endif
      r:=i_lam(dm->data)
      if pozycja=dm->pozycja
        ?? speC(P_UON)
      endif
      ? str(D_LPVAL(pozycja),2)+"|",cpad((r)->NAZWA,27,10,1),"|",ccpi(5)+Tran(index,"@R "+ INDEXPIC )+"|",if(""=dok_kon,"",ccpi(7)+nr_zleC+"|"),ccpi(5)+TrAN(waRTOSC/ILOSC,"@E ",A_ZAOKR,9)+"|"
      ?? strTraN(strpic(if(dok_p_r#"P",-ilosc,ilosc),10,3),".000",".   ")+"|"+(r)->jm+"|"+strpic(if(dok_p_r#"P",-WARTOSC,wartosc),10,A_ZAOKR,"@E "),ccpi(4)
      skip
    ENDDO
    ?? speC(P_UOFF)
    SELECT DM
    ENDIF
    skip
  enddo
  if nr_mag+smb_dow+stano_kosz+subs(konto_kosz,5)=txt
     seek txt + HB_UCHAR(0x00A0)
  endif
  ?? speC(HB_BCHAR(13)+P_UON+SPACE(80))
  ?? spec(P_UOFF)
  ? "RAZEM "+TranR(txt,"XX-XX-XXXXXX/X")+"|"+TrAN(W,"@E ",A_ZAOKR,17)+"|  NARASTAJĄCO: "+TrAN(Wtot+=w,"@E ",A_ZAOKR,17)
ENDDO
ENDIF
return
#endif
***************
