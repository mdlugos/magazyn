#include "dbinfo.ch"
#include "dm_form.ch"
#include "inkey.ch"
#include "getexitm.ch"

#define I hb_UTF8ToStr("│")

#ifdef A_ZAGRODA
  #define A_KASA
  #define D_KODY 55
  #undef A_SB
#endif

#define CEOKR 2

#ifdef A_ADS
 #ifndef A_STSIMPLE
 #define A_STSIMPLE
 #endif
#endif

#ifdef A_OLZBY
 #define WANAZ "WAGA kg"
#else
 #define WANAZ "Wartość"
#endif

#ifdef A_SZTUKI
 #define ILDEC 0
#else
 #define ILDEC 3
#endif

#ifdef A_ANKER
 #define nazwA (nazwa+strpic(field->cenA,11,2,"@E ")+hb_UTF8ToStr(" zł ")+field->proc_vat+"%")
#endif

#ifdef A_FP600
 #define nazwA (nazwa+strpic(field->cenA,11,2,"@E ")+hb_UTF8ToStr(" zł ")+field->proc_vat+"%")
#endif

#ifndef STANY
  #define LARMG '┬MG'
#else
  #define LARMG '──'
#endif

#ifdef A_KODY
  #ifndef D_KODY
    #define D_KODY 48
  #endif
#endif

#ifdef A_SWW
 #ifdef A_VIEWCZAK
  #define TOPLINE 'Kod──┬Nazwa────────────────────────────┬dn.┬C.Zak.'+LARMG+'┬─────Stan┬─jm─┬Cena'
 #else
  #ifdef A_VIEWVAT
   #define TOPLINE 'Kod──┬Nazwa────────────────────────────────────Vat'+LARMG+'┬─────Stan┬─jm─┬Cena'
  #else
   #define TOPLINE 'Kod──┬Nazwa───────────────────────────────────────'+LARMG+'┬─────Stan┬─jm─┬Cena'
  #endif
 #endif
 #ifdef A_SHARP
  #undef TOPLINE
  #define TOPLINE 'Kod──┬Nazwa─────────────────────────────────────┬Klaw┬─────Stan┬─jm─┬Cena'
 #endif
#else
 #ifdef A_SHORTIND
      #ifdef A_KODY
       #define TOPLINE 'Kod┬Nazwa'+repl('─',40-len(KoD))+'┬'+hb_upadr(A_KODY,len(KoD),'─')+'┬──────Stan┬─jm─┬Cena'
       #undef D_KODY
       #define D_KODY 48
      #else
       #define TOPLINE 'Kod┬Nazwa───────────────────────────────────────'+LARMG+'┬──────Stan┬─jm─┬Cena'
      #endif
 #else
  #ifdef A_KTM
   #ifdef A_OBR
    #undef LARMG
    #ifndef STANY
     #define LARMG '┬M'
    #else
     #define LARMG '─'
    #endif
    #define TOPLINE 'Kod materiału──┬Nazwa─────────────────────────Nr rys i półki─'+LARMG+'┬─────Stan┬jm'
   #else
    #define TOPLINE 'Kod materiału──┬Nazwa───────────────────────────────────────'+LARMG+'┬─────Stan┬jm'
   #endif
  #else
   #ifdef A_OLZA
    #define TOPLINE 'Kod materiału┬Nazwa───────────────────────────────────────'+LARMG+'┬──────Stan┬jm'
   #else
    #ifdef A_ANKER
     #ifdef A_VIEWVAT
      #define TOPLINE 'Kod kresk.─┬Nazwa────────────────────────────┬Kod──┬Vat──────Stan┬─jm─┬Cena'
     #else
      #ifdef A_VIEWCZAK
       #define TOPLINE 'Kod kresk.─┬Nazwa────────────────────────────┬Kod──┬C.Zak┬───Stan┬─jm─┬Cena'
      #else
#ifdef STANY
       #define TOPLINE 'Kod kresk.─┬Nazwa───────────────────────────────────┬──────────┬'+hb_upadr(A_KODY,len(KoD),'─')+'┬───Stan┬─jm─┬Cena'
#else
       #define TOPLINE 'Kod kresk.─┬Nazwa───────────────────────────────────┬──────────┬'+hb_upadr(A_KODY,len(KoD),'─')+LARMG+'┬───Stan┬─jm─┬Cena'
#endif
      #endif
     #endif
    #else
     #ifdef A_7
      #ifdef A_KODY
       #define TOPLINE 'Kod───┬Nazwa'+repl('─',49-len(KoD))+'┬'+hb_upadr(A_KODY,len(KoD),'─')+'┬─────Stan┬jm'
       #define D_KODY 55
      #else
       #define TOPLINE 'Kod───┬Nazwa──────────────────────────────────────'+LARMG+'┬─────Stan┬─jm─┬Cena'
      #endif
     #else
      #define TOPLINE 'Kod mater.─┬Nazwa───────────────────────────────────────'+LARMG+'┬──────Stan┬jm'
     #endif
    #endif
   #endif
  #endif
 #endif
#endif


MEMVAR mag_biez,mag_poz,magazyny,mag_link,adres_mag,changed,srec,hlink,stary_rok,;
       dok,jmiar,miar_opcja,iord,s,SB,se,s3,s2,s1,da,i,W,Wb,We,W3,W2,W1,il,chg,;
       k_pola,irec

field nr_mag,index,nazwa,stan,smb_dow,nr_dowodu,nr_zlec,ilosc,KTO_PISAL,kod,;
      przel,WAZNOSC,nr_rys,rodz_opak,gram,polka,jm,jm_opcja,cena,proc_vat,data_popr,;
      cena_zak,proc_mar,zaznacz,wartosc,sww

function KATALOG(_s,g)
      local r,t
      private da,il // dla TAB
#ifdef A_HLINK
private hlink:=NIL
#endif
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
      da:=max(DatY->d_z_mies1+1,DatE())
      SETPOS(10,0)

#ifdef A_MYSZ
   #define D_MYSZ ,bx,cx,dx,myszflag
#else
   #define D_MYSZ
#endif

#ifdef A_MINUS
   #define D_MINUS .t.
#else
   #define D_MINUS .f.
#endif
      if _s=NIL
         _s:=array(_sLEN)
      else
         asize(_s,max(len(_s),_sLEN))
      endif
#command DEFAULT <x> TO <y> => IF (<x>)=NIL;<x>:=<y>;ENDIF
      if _spocz=NIL .and. g=NIL
         DEFAULT _sinfo TO {|skey,s D_MYSZ|(skey#13 .or. 9=(skey:=9)).and.peoma(skey,s D_MYSZ)}
         _spocz:=''
      else
         DEFAULT _sinfo TO {|skey,s D_MYSZ|peoma(skey,s D_MYSZ)}
         if valtype(g)='O' .and. _spocz=NIL .and. _slth=NIL .and. _sbeg=NIL
            _spocz:=trim(right(g:varget(),indx_mat->(hb_fieldlen(index))))
         endif
      endif
      DEFAULT _spocz TO ''
      DEFAULT _srowb TO 1
      DEFAULT _scol1 TO 0
      DEFAULT _srowe TO maxrow()
      DEFAULT _sbeg  TO 1
      DEFAULT _slth  TO 0
      //DEFAULT _snagl TO TOPLINE
      DEFAULT _skon    TO D_MINUS
#undef D_MINUS
#ifndef STANY
  #ifdef A_IZ
      select main
      set order to TAG MAIN_IND
  #endif
#endif
      r:=szukam(_s)
      if r.and.valtype(g)='O'
         t:=g:varget()
         t:=right(STANY->(left(t,len(t)-HB_fieldlen('nr_mag')-hb_fieldlen('index'))+nr_mag+index),len(t))
         g:varput(t)
      endif
return r
*********************************
FUNCTION LPEOMA(_s)

field data_zmian,data_popr,lamus,jm_opcja,jm,przel

local RET,sel,s,w


#ifdef A_SB
memvar pm,dok_di
  IF il<>NIL .and. dok$dok_di
     stanprzeD(.t.,'',@s,@w)
  ELSE
     s:=STANY->stan
     w:=STANY->wartosc
  ENDIF

 #define D_ST s
 #define D_WA w

#else
 #define D_ST STANY->stan
 #define D_WA STANY->wartosc

#endif

#ifdef A_FA
 #ifdef A_DIETA
  #undef cenA
  #define cenA (if((D_ST)=0,STANY->cena_przy, D_WA / D_ST))
 #endif
#endif

#ifndef STANY
 #ifdef A_OBR
  #define LARTXT left(if(polka=" ",if(nr_rys=" ",nazwA,left(nazwA,37)+if(len(trim(nazwA))>37,chr(26)," ")+nr_rys),if(nr_rys=" ",left(nazwA,38)+;
if(len(trim(nazwA))>38,chr(26)," "),left(nazwA,30)+if(len(trim(nazwA))>30,chr(26)," ")+left(nr_rys,7)+" ")+polka) ,45) + if(""=INDX_MAT->UWAGI,RET,"&")+subs(STANY->NR_MAG,2)
 #else
  #define LARTXT pad(nazwA,44)+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
 #endif
  RET:=IF(ALIAS(sel:=I_LAM(STANY->(if(il=NIL.and.da>data_zmian.and.stan#0,data_zmian,da))))="IND_LAM","┼","│")
  if _sbeg=1
  else
    //sel:=select("INDX_MAT")
    RET:=IF((sel)->DATA_POPR>DA .OR. STANY->STAN#0 .AND.(SEL)->DATA_POPR>STANY->DATA_ZMIAN,"▒",ret)
  ENDIF
#else
 #ifdef A_KODY
  #define LARTXT pad(nazwA,45-len(KoD))+ret+INDX_MAT->KoD
 #else
  #define LARTXT nazwA
 #endif
  RET:=IF(ALIAS(sel:=I_LAM(if(il=NIL.and.da>STANY->data_zmian.and.STANY->stan#0,STANY->data_zmian,da)))="IND_LAM","┼","│")
#endif
ret := hb_UTF8ToStr(ret)
#ifdef A_TRWALOSC
 #define TRWALTXT IF(WAZNOSC>0.AND.STANY->STAN>0.AND.STANY->DATA_PRZY+WAZNOSC<DA,hb_UTF8ToStr("°"),IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET))
#else
 #ifdef A_WA
  #define TRWALTXT IF(STANY->STAN>0.AND.STANY->WARTOSC=0,"$",IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET))
 #else
  #define TRWALTXT IF(STANY->STAN>0.AND.indx_mat->cenA=0,"$",IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET))
 #endif
#endif

#ifdef A_KTM
 #define OBRTXT 9
#else
 #ifdef A_SWW
  #define OBRTXT 9
 #else
  #define OBRTXT 10
 #endif
#endif

#ifdef A_7
 #undef OBRTXT
 #define OBRTXT 9
 #undef INDEXPIC
 #define INDEXPIC "XXXXXXX"
 #undef LARTXT
 #ifdef STANY
  #define LARTXT pad(nazwA,46)
 #else
  #define LARTXT pad(nazwA,43)+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
 #endif
#endif


#ifdef A_JMALTTOT
  #define A_JMALT
#endif
#ifdef A_CF8
 #ifdef A_JMALT
  #define D_CF8 /if(miar_opcja,PRZEL,1)
 #else
  #define D_CF8 *if(miar_opcja,PRZEL,1)
 #endif
#else
 #define D_CF8
#endif

#ifdef A_SHORTIND
 #ifdef A_FA
  #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,9,CEOKR,"@E ",.t.)
 #else
  #ifdef A_WA
   #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic( if((D_ST)=0,STANY->cena_przy, D_WA / D_ST) D_CF8,9,CEOKR,"@E ",.t.)
  #else
   #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,9,CEOKR,"@E ",.t.)
  #endif
 #endif
#else
 #ifdef A_SWW
  #ifdef A_FA
   #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,8,CEOKR,"@E ",.t.)
  #else
   #ifdef A_WA
    #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if((D_ST)=0,STANY->cena_przy, D_WA / D_ST) D_CF8,8,CEOKR,"@E ",.t.)
   #else
    #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,8,CEOKR,"@E ",.t.)
   #endif
  #endif
  #ifdef A_VIEWCZAK
   #undef LARTXT
   #ifndef STANY
    #define LARTXT pad(nazwA,34)+ret+str(da-STANY->data_przy,3)+ret+strpic(STANY->cenA_zaK D_CF8,6,CEOKR,"@E ",.t.)+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
   #else
    #define LARTXT pad(nazwA,37)+IF(""=INDX_MAT->UWAGI,ret,"&")+strpic(STANY->cenA_zaK D_CF8,8,CEOKR,"@E ",.t.)
   #endif
  #else
   #ifdef A_VIEWVAT
    #undef LARTXT
    #ifndef STANY
     #define LARTXT pad(nazwA,30)+ret+INDX_MAT->proc_vat+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
    #else
     #define LARTXT pad(nazwA,43)+IF(""=INDX_MAT->UWAGI,ret,"&")+INDX_MAT->proc_vat
    #endif
   #endif
  #endif
  #ifdef A_SHARP
   #undef LARTXT
   #define LARTXT pad(nazwA,42)+IF(""=INDX_MAT->UWAGI,ret,"&")+strpic(STANY->kasa_shift+STANY->kasa_klaw*.01,4,2,"@E ",.t.)
  #endif
 #else
  #ifdef A_ANKER
   #undef LARTXT
   #undef ILDEC
   #undef OBRTXT
   #define ILDEC 2
   #ifdef A_VIEWVAT
    #define OBRTXT 10
    #define LARTXT pad(nazwA,33)+IF(""=INDX_MAT->UWAGI,{"0",I,"!","3","4","5","6","7"}[INDX_MAT->status%8+1],"&")+KoD+ret+INDX_MAT->proc_vat
   #else
    #define OBRTXT 7
    #ifdef A_VIEWCZAK
     #define LARTXT pad(nazwA,33)+IF(""=INDX_MAT->UWAGI,{"0",I,"!","3","4","5","6","7"}[INDX_MAT->status%8+1],"&")+KoD+ret+strpic(STANY->cenA_zaK D_CF8,5,CEOKR,"@E ")
    #else
     #undef nazwA
     #define nazwA (nazwa+I+left(indx_mat->shortname,10))
     #ifndef STANY
      #define LARTXT nazwA+IF(""=INDX_MAT->UWAGI,{"0",I,"!","3","4","5","6","7"}[INDX_MAT->status%8+1],"&")+INDX_MAT->KoD+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
     #else
      #define LARTXT nazwA+IF(""=INDX_MAT->UWAGI,{"0",I,"!","3","4","5","6","7"}[INDX_MAT->status%8+1],"&")+INDX_MAT->KoD
      //+IF(""=INDX_MAT->UWAGI,ret,"&")+STANY->NR_MAG
     #endif
    #endif
   #endif
   #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,6,CEOKR,"@E ")
  #else
   #ifdef A_7
    #ifdef A_KODY
     #undef LARTXT
     #define LARTXT pad(nazwA,54-len(KoD))+ret+INDX_MAT->KoD
     #define SHORTTXT
    #else
     #ifdef A_FA
      #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,8,CEOKR,"@E ",.t.)
     #else
      #ifdef A_WA
       #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if((D_ST)=0,STANY->cena_przy, D_WA / D_ST) D_CF8,8,CEOKR,"@E ",.t.)
      #else
       #define SHORTTXT +IF(STANY->DATA_ZMIAN>DatY->data_gran,hb_UTF8ToStr("■"),RET)+strpic(if(il=NIL,indx_mat->cenA,cenA) D_CF8,8,CEOKR,"@E ",.t.)
      #endif
     #endif
    #endif
   #else
    #define SHORTTXT
   #endif
  #endif
 #endif
#endif


#ifdef A_JMO
 #define restouT(x,p) if(x%p=0,strtran(str(x/p,OBRTXT,3),".000","    "),strtran(str(int(x/p)+x%p/1000,OBRTXT,3),".","r"))
 #define D_JMO if(miar_opcja,restouT( D_ST , PRZEL )+ret+JM_OPCJA,str( D_ST , OBRTXT,ILDEC)+if(przel=1,ret,"#")+JM)
#else
 #ifdef A_JMALT
  #define D_JMO str(if(miar_opcja, D_ST * PRZEL,D_ST),OBRTXT,ILDEC)+if(przel=1,ret,"#")+if(miar_opcja,JM_OPCJA,JM)
 #else
  #define D_JMO str(if(miar_opcja, D_ST / PRZEL,D_ST),OBRTXT,ILDEC)+if(przel=1,ret,"#")+if(miar_opcja,JM_OPCJA,JM)
 #endif
#endif

#ifdef A_SHORTIND
 #define SSBEG 6
 #define SSEND -10
#else
 #ifdef A_KTM
  #define SSBEG 18
  #define SSEND -15
 #else
  #ifdef A_SWW
   #define SSBEG 8
   #define SSEND -9
  #else
   #ifdef A_7
    #define SSEND -9
    #define SSBEG 9
   #else
    #ifdef A_ANKER
     #define SSEND -6
    #else
     #define SSEND -16
    #endif
    #ifdef A_OLZA
     #define SSBEG 16
    #else
     #define SSBEG 14
    #endif
   #endif
  #endif
 #endif
#endif

RETURN (sel)->(tran(index,"@R "+ INDEXPIC )+IF(indx_mat->LAMUS#0 .and. ret=I,"*",ret)+;
    (if(_sbeg=SSBEG,'INDX_MAT',sel))->(LARTXT)+TRWALTXT+D_JMO SHORTTXT)
#undef D_CF8
#undef D_ST
#undef D_WA
***************************************
FUNCTION I_LAM(DA,do)
local REC,SEL:=SELECT()
field lamus,data_popr
SELECT INDX_MAT
DO WHILE DATA_POPR>DA .and. lamus > 0
  do:=data_popr
  REC:=LAMUS
  SELECT IND_LAM
  GO REC
  if index#indx_mat->index
     select indx_mat
     exit
  endif
ENDDO   
REC:=SELECT()
SELECT (SEL)
RETURN (REC)
******************
FUNCTION peoma(_skey,_s D_MYSZ)
local txt,bl,getlist,na,a,b,c,d


field zamk_mies1,zamk_mies2,zamkn_roku,zapas_min,zapas_max,przel
#ifdef A_WA
field wart_mies1,wart_mies2,wart_roku
#endif

DO CASE
  CASE _skey=0
#ifdef A_ZAZNACZ
      DEFAULT _sprompt TO {|d,s,z,x|x:=lpeoma(s),if(z=.t.,x,(devout(x,{_snorm,"GR+/N+","R+/N+","G+/N+","GB+/N+","BR+/N",12,13,14,15}[indx_mat->zaznacz+1]),''))}
      DEFAULT _scol2 TO min(maxcol(),1+len(lpeoma(_s)))
#else
      DEFAULT _sprompt TO {|d,s|lpeoma(s)}
#endif
     select indx_mat
       set order to TAG INDX_NUM
#ifdef A_ANKER
       if _sbeg#1
       set order to TAG INDX_KOD
       endif
#endif
       set relation to
       set filter to
    txt:=_spocz
#ifdef STANY
    _spocz:=mag_biez+txt
#else
 #ifdef A_IZ
     select main
       set order to TAG MAIN_IND
 #endif
     select STANY
       set relation to
       set filter to
       SET ORDER TO TAG STAN_IND
     select indx_mat
//       SET RELATION TO INDEX+mag_biez INTO STANY
//       GOTO recno()
#endif
#ifdef A_7
    _spform={|p,l|right(p,l)}
#else
    _spform={|p,l|TranR(right(p,l), INDEXPIC )}
#endif
#ifdef A_KKR
    if val(txt)<>0 .and. len(txt)>=8
        SET ORDER TO TAG INDX_KKR
 #ifdef A_MM
        IF dbseek(txt) .and. txt=trim(INDX_MAT->info)
 #else
        IF dbseek(_spocz) .and. txt=trim(INDX_MAT->info)
 #endif
           if jm='szt' .and. il=0
              il:=memvar->pm
/*#ifdef A_MM
              kibord(chr(3)+chr(13))
#else*/
              kibord(chr(3))
//#endif
           endif
 #ifndef STANY
           STANY->(dbseek(INDX_MAT->index+mag_biez),.f.)
 #endif
           RETURN (_sret:=.T.)
        ENDIF
        SET ORDER TO TAG INDX_NUM
    endif
#endif
    _slth:=LEN(txt)
    DO CASE
      CASE ""=txt
#ifdef A_ENAZ
        SET ORDER TO TAG INDX_NAZ
        _sbeg=SSBEG
        _spform={|p,l|right(p,l)}
#endif
#ifdef A_LFIRST
      CASE !DBSEEK(_spocz)
 #ifdef A_FILTERNAZ
         _slth:=0
         txt:=UpP(txt)
         _sfilt:='['+txt+']$UPPER(INDX_MAT->naZwa)'
         _sfilb:={||txt$UPPER(INDX_MAT->naZwa)}
         _spocz:=left(_spocz,len(_spocz)-len(txt))
#ifdef A_ZAZNACZ
         _sprompt:={|d,s,z,x,l,k,c|c:={_snorm,"GR+/N+","R+/N+","G+/N+","GB+/N+","BR+/N",12,13,14,15}[indx_mat->zaznacz+1],x:=lpeoma(s),if(z=.t.,x,(l:=if(empty(txt),0,at(txt,UpP(x))),k:=if(l=0,0,len(txt)),devout(left(x,l-1),c),devout(subs(x,l,k),_sel),devout(subs(x,l+k),c),''))}
#else
         _sprompt:={|d,s,z,x,l,k,c|c:=_snorm,x:=lpeoma(s),if(z=.t.,x,(l:=if(empty(txt),0,at(txt,UpP(x))),k:=if(l=0,0,len(txt)),devout(left(x,l-1),c),devout(subs(x,l,k),_sel),devout(subs(x,l+k),c),''))}
#endif
 #endif
#else
      CASE ASC(txt)>57
#endif
        SET ORDER TO TAG INDX_NAZ
        SEEK _spocz
        _sbeg=SSBEG
        _spform={|p,l|right(p,l)}
#ifdef A_ANKER
#ifdef A_ZAGRODA
      CASE pad(txt,hb_fieldlen('index'))=index .and. if(len(trim(field->tandem))>=6,dbseek(pad(field->tandem,hb_fieldlen('index'))),.t.)
#else
      CASE pad(txt,hb_fieldlen('index'))=index
#endif
#else
 #ifdef A_LFIRST
      CASE txt=index
 #else
      CASE DBSEEK(_spocz) .and. txt=index
 #endif
#endif
#ifndef STANY
        STANY->(dbseek(INDX_MAT->index+mag_biez,.f.))
#endif
        RETURN (_sret:=.T.)
      case txt=trim(index) .and. !eof()
        bl:=recno()
        na:=index
        locate for index#na while index=txt
        if !found()
           go bl
#ifndef STANY
           STANY->(dbseek(INDX_MAT->index+mag_biez,.f.))
#endif
           RETURN (_sret:=.T.)
        endif
        go bl
    ENDCASE
    _snagkol:=1
    IF _slth>0
      _sfilt:=NIL
      _sfilb:=NIL
    ENDIF
    DEFAULT _snagl TO hb_UTF8ToStr(TOPLINE)
#ifndef STANY
    IF _skon
      SET RELATION TO INDEX+mag_biez INTO STANY
      _sret:=1
    ELSE
      _sret:=2
      a:=ordnumber()
 #ifdef A_STSIMPLE
      IF a=1
        SET RELATION TO INDEX+mag_biez INTO STANY
      else
 #endif
      _spocz=mag_biez+txt
      set order to TAG INDX_NUM
      select STANY
      SET ORDER TO (a)
      seek mag_biez+indx_mat->(EvAlDb(IndexkeY(a)))
      SET RELATION TO INDEX INTO INDX_MAT
 #ifdef A_STSIMPLE
      endif
 #endif
 #ifdef A_WA
      _sfor:={||STANY->STAN#0 .OR. STANY->WARTOSC#0}
 #else
      _sfor:={||STANY->STAN#0}
 #endif
    ENDIF
#else
    _sret:=2
    if !_skon
 #ifdef A_WA
      _sfor:={||STAN#0 .OR. WARTOSC#0}
 #else
      _sfor:={||STAN#0}
 #endif
    endif
#endif
    _skon:=NIL
    set cursor on
#ifdef A_MYSZ
    _skproc[14]:=NIL
#endif

   case _skey=27
#ifdef A_HLINK
        if hlink#NIL
         select ind_lam
         nuse ind_lam
         select indx_mat
         nuse indx_mat
 #ifdef A_CDX
         set order to TAG indx_num in indx_mat
 #else
         set index to INDX_naz,INDX_num
 #endif
         hlink:=NIL
         @ 0,0 say magazyny[mag_poz]
        endif
#endif
 #ifdef A_FILTERNAZ
         IF !empty(_sfilt) .and. '"$UPPER(INDX_MAT->naZwa)'$_sfilt
            _sfilt:=''
            _sfilb:=NIL
         ENDIF
 #endif
 #ifdef A_STSIMPLE
         IF !empty(_sfilt) .and. _sfilt=='STANY->NR_MAG="'+mag_biez+'"'
            _sfilt:=''
            _sfilb:=NIL
         ENDIF
 #endif
        _sret:=.f.
        return .t.


   CASE _skey=2 .or. _skey=26
#ifdef STANY
 #ifdef A_KODY
#ifdef A_ZAGRODA
    if if(_skey=2,_sbeg=SSBEG,_sbeg=66)
       _spform:={|p,l|TranR(right(p,l),'XXXXXXXXXX|XXXXX')}
       _sbeg:=D_KODY
       _slth:=0
       _spocz:=mag_biez
       SET ORDER TO TAG INDX_PRO
    elseif if(_skey=2,_sbeg=D_KODY,_sbeg=1)
       _spform:={|p,l|TranR(right(p,l),'XXXXX')}
       _sbeg:=66
       _slth:=0
       _spocz:=mag_biez
       SET ORDER TO TAG INDX_KOD
    elseif _sbeg=if(_skey=2,1,D_KODY) // ^>
#else
    if _sbeg=if(_skey=2,SSBEG,1)
       SET ORDER TO TAG INDX_KOD
      _spform:={|p,l|right(p,l)}
  #ifdef A_ANKER
      _sbeg:=D_KODY
    elseif _sbeg=if(_skey=2,1,D_KODY) // ^>
  #else
      _sbeg:=len( INDEXPIC )+D_KODY-Len(KoD)
    elseif _sbeg=if(_skey=2,1,len( INDEXPIC )+D_KODY-Len(KoD)) // ^>
  #endif
#endif

 #else
    if _sbeg=1 // ^>
 #endif
      _spform:={|p,l|right(p,l)}
      _sbeg:=SSBEG
       SET ORDER TO TAG INDX_NAZ
    else
       SET ORDER TO TAG INDX_NUM
#else

 #ifdef A_KODY
    if if(_skey=2,_sbeg=SSBEG,_sbeg>45)
       _spform:={|p,l|TranR(left(p,l),'XXXXXXXXXX|XXXXX')}
       _sbeg:=D_KODY

        if select() = select('STANY') //_sret<>1
           set relation to
           SET ORDER TO TAG STAN_IND
           _sfilt:=""
           _sfilb:=NIL
           select indx_mat
           if _sret=2
              set relation to index+mag_biez into STANY
              _sfilt:='STANY->NR_MAG="'+mag_biez+'"'
              _sfilb:=hb_macroBlock(_sfilt)
           else
              set relation to index into STANY
           endif
           //_sret:=1
           _srec[_sm]:=recno()
        endif
        _slth:=0
        _spocz:=''
        SET ORDER TO TAG INDX_PRO
    elseif if(_skey=2,_sbeg=D_KODY,_sbeg=1)
       _spform:={|p,l|TranR(left(p,l),'XXXXX')}
       _sbeg:=66

        if select() = select('STANY') //_sret<>1
           set relation to
           SET ORDER TO TAG STAN_IND
           _sfilt:=""
           _sfilb:=NIL
           select indx_mat
           if _sret=2
              set relation to index+mag_biez into STANY
              _sfilt:='STANY->NR_MAG="'+mag_biez+'"'
              _sfilb:=hb_macroBlock(_sfilt)
           else
              set relation to index into STANY
           endif
           //_sret:=1
           _srec[_sm]:=recno()
        endif
        _slth:=0
        _spocz:=''
        SET ORDER TO TAG INDX_KOD
    elseif _sbeg=if(_skey=2,1,D_KODY) // ^>
 #else
    if _sbeg=1 // ^>
 #endif
      _spform:={|p,l|right(p,l)}
      _sbeg:=SSBEG
 #ifdef A_STSIMPLE
        if _sret<>1
           set relation to
           SET ORDER TO TAG STAN_IND
           _sfilt:=""
           _sfilb:=NIL
           select indx_mat
           if _sret=2
              set relation to index+mag_biez into STANY
              _sfilt:='STANY->NR_MAG="'+mag_biez+'"'
              _sfilb:=hb_macroBlock(_sfilt)
              _spocz:=subs(_spocz,3)
           else
              set relation to index into STANY
           endif
           //_sret:=1
           _srec[_sm]:=recno()
        endif
        SET ORDER TO TAG INDX_NAZ
    else
        SET ORDER TO TAG INDX_NUM
        if _sret<>1
           set relation to
           select STANY
           if _sret=2
              _sfilt:=""
              _sfilb:=NIL
              SET ORDER TO TAG STAN_MAG
              _spocz:=mag_biez+_spocz
           ELSE
              SET ORDER TO TAG STAN_IND
           ENDIF
           set relation to index into indx_mat
           _srec[_sm]:=recno()
        endif
 #else
        if _sret=0
           SET ORDER TO TAG STAN_ALL
        elseif _sret=1
           SET ORDER TO TAG INDX_NAZ
        else
           SET ORDER TO TAG STAN_NAZ
        endif
    else
        if _sret=0
           SET ORDER TO TAG STAN_IND
        elseif _sret=1
           SET ORDER TO TAG INDX_NUM
        else
           SET ORDER TO TAG STAN_MAG
        endif
 #endif
#endif
#ifdef A_7
    _spform={|p,l|right(p,l)}
#else
    _spform={|p,l|TranR(right(p,l), INDEXPIC )}
#endif
       _sbeg:=1
    endif
    _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _spocz:=LEFT(_spocz,len(_spocz)-_slth)
    _slth:=0
    REFRESH LINE _sm+_srow1-1 DIRECTION 0
    refresh(1,_s)

   CASE _skey=22 // ins
      if _si=0
         _srec[1]:=recno()
         _sm:=1
      endif
      KIM_EDIT(_s)


#ifdef A_SZYM
   case _skey=43 .and. il#NIL .and. dok="ZL"
   //.and. firmy->(if(il=NIL,szukam({1,14,maxrow(),,1,0,'FIRMY',{||numer_kol+if(""=uwagi,I,"*")+nazwa},{|_skey,_s|_sret:=_skey=13,_skey=13 .or. _skey=27},""}),eof()))
      if _si=0
         _srec[1]:=recno()
         _sm:=1
      endif
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
      goto _srec[_sm]
      KIM_EDIT(_s,FIRMY->numer_kol+right(dm->nr_dowodu,3))
      return .t.
#else
   CASE _skey=43 // plus
      if _si=0
         _srec[1]:=recno()
         _sm:=1
      endif
      _slth:=_slth-1
      _spocz:=left(_spocz,LEN(_spocz)-1)
      goto _srec[_sm]
      KIM_EDIT(_s,.t.)
      return .t.
#endif

#ifndef STANY
  CASE _skey=-2 .and. _sret#2// F3 - MAG_BIEZ
 #ifdef A_STSIMPLE
      IF _sbeg=1
 #endif
      IF _sret=1
        set order to TAG INDX_NUM
        set relation to
        select STANY
        _sfilt:=""
        _sfilb:=NIL
        IF EOF()
           IF _sbeg=1
              SET ORDER TO TAG STAN_IND
              SEEK INDX_MAT->INDEX
           ELSE
              SET ORDER TO TAG STAN_ALL
              SEEK UpP(INDX_MAT->nazwa)
           ENDIF
        ENDIF
        set relation to index into indx_mat
      ENDIF
        set order to if(_sbeg=1,"STAN_MAG","STAN_NAZ")
        _spocz:=mag_biez+_spocz
 #ifdef A_STSIMPLE
      ELSE
         set relation to index+mag_biez into STANY
         _sfilt:='STANY->NR_MAG="'+mag_biez+'"'
         _sfilb:=hb_macroBlock(_sfilt)
      ENDIF
 #endif
        _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
        _sret:=2
    REFRESH(,_s)
    @ _srow2,_scol1+1 say hb_UTF8ToStr(if(_sfor=NIL,"WSZYSTKIE MATERIAŁY ","STAN NIEZEROWY "))+trim(magazyny[mag_poz]) COLOR _slinia

  CASE _skey=-3 .AND. _sret#1// f4   INDX_MAT
      SELECT STANY
      set relation to
      SET ORDER TO TAG STAN_IND
      _sfilt:=""
      _sfilb:=NIL
      select indx_mat
      set relation to index+mag_biez into STANY
 #ifdef A_STSIMPLE
      if _sret=2 .and. _sbeg=1
 #else
      if _sret=2
 #endif
        _spocz:=substr(_spocz,3)
      endif
      set order to if(_sbeg=1,"INDX_NUM","INDX_NAZ")
      _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
      _sfor:=NIL
      REFRESH(,_s)
      @ _srow2,_scol1+1 say "MATERIAŁY Z WSZYSTKICH MAGAZYNÓW" UNICODE COLOR _slinia
      _sret=1

  CASE _skey=-4 .AND. _sret#0 // f5
 #ifdef A_STSIMPLE
   IF _sbeg=1
 #endif
      IF _sret=1
        set order to tag indx_num
        set relation to
        _sfilt:=""
        _sfilb:=NIL
        select STANY
        IF EOF()
           IF _sbeg=1
              SET ORDER TO TAG STAN_IND
              SEEK INDX_MAT->INDEX
           ELSE
              SET ORDER TO TAG STAN_ALL
              SEEK UpP(INDX_MAT->nazwa)
           ENDIF
        ENDIF
        set relation to index into indx_mat
      ELSE
        _spocz=substr(_spocz,3)
      ENDIF
      set order to IF(_sbeg=1,"stan_ind","stan_all")
 #ifdef A_STSIMPLE
   elseif _sret=2
      _sfilt:=""
      _sfilb:=NIL
   endif
 #endif
      _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _sret:=0
    REFRESH(,_s)
    @ _srow2,_scol1+1 say "WSZYSTKIE MAGAZYNY" COLOR _slinia
    if _sfor#NIL
      DEVOUT(" - STAN NIEZEROWY",_slinia)
    endif
#endif

  CASE _skey=-1 .or. _skey=7 // POKAZ/UKRYJ ZEROWKI
    IF _sfor=NIL
#ifdef A_WA
      _sfor:={||STANY->STAN#0 .OR. STANY->WARTOSC#0}
#else
      _sfor:={||STANY->STAN#0}
#endif
      REFRESH(,_s)
    else 
      _sfor:=NIL
      if _si=0
         go _srec[1]
      endif
      REFRESH(,_s)
    endif
    @ _srow2,_scol1+1 say if(_sfor=NIL,"WSZYSTKIE MATERIAŁY","STAN NIEZEROWY") UNICODE COLOR _slinia
    if _sret=2
      DEVOUT(" "+trim(magazyny[mag_poz]),_slinia)
    endif

#ifdef A_MYSZ
   case _skey=14
        if bx=1 .and. dx=row() .and. cx=_scol1+SSBEG-2
          peoma(2,_s)

        elseif bx=1 .and. dx=row() .and. cx<_scol1+SSBEG-2
          peoma(9,_s)

        elseif bx=1 .and. dx=row() .and. cx>_scol2 SSEND

          peoma(22,_s)

        else
          return mysz(_s,bx,cx,dx,myszflag)
        endif
#endif

#ifdef A_ZAZNACZ
      case chr(_skey)$"[\];',./"
      //case _skey>=91 .and. _skey<=93
         goto _srec[_sm]
         a:=select()
         select indx_mat
         LOCK recno()
         zaznacz:=at(chr(_skey),"[\];',./")-1
         if zaznacz<>0 .and. fieldpos('data_zazn')<>0
            FIELD->data_zazn:=DatE()
         endif
         unlock recno()
         select (a)
         _spocz:=left(_spocz,len(_spocz)-1)
         --_slth
         REFRESH LINE _sm+_srow1-1 DIRECTION 0
         _SDOL(_s,24)
         return .t.
#endif

#ifndef A_LFIRST
   case _skey>31 .and. _skey<256
    do case
      case _skey>64 .AND. _sbeg=1
 #ifdef STANY
        SET ORDER TO TAG INDX_NAZ
 #else
        if _sret=0
           SET ORDER TO TAG STAN_ALL
        elseif _sret=1
           SET ORDER TO TAG INDX_NAZ
        else
           SET ORDER TO TAG STAN_NAZ
        endif
 #endif
        _spocz:=LEFT(_spocz,len(_spocz)-_slth)+UpP(hb_keyChar(_SKEY))
        _slth=1
        _spform={|p,l|right(p,l)}
        _sbeg=SSBEG

      case _sbeg#1  .and. _slth=1 .and. _skey<58
 #ifdef STANY
        SET ORDER TO TAG INDX_NUM
 #else
        if _sret=0
           SET ORDER TO TAG STAN_IND
        elseif _sret=1
           SET ORDER TO TAG INDX_NUM
        else
           SET ORDER TO TAG STAN_MAG
        endif
        //SET ORDER TO (IF(_sret=0,"STAN_IND","STAN_MAG"))
 #endif
        _sbeg=1
 #ifdef A_7
    _spform={|p,l|right(p,l)}
 #else
    _spform={|p,l|TranR(right(p,l), INDEXPIC )}
 #endif
      OTHERWISE
        RETURN .F.
    ENDCASE
    _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(,_s)
    return .t.
#endif

   case _skey=-8
      _sfil(_s)

   CASE _si=0

   CASE _skey=13
#ifdef A_HLINK
        if hlink#NIL
         _skey:={nazwa,index,if(hlink[3],cena,cena_zak),proc_vat,jm,przel,jm_opcja,if(hlink[3],0,proc_mar)}
         select ind_lam
         nuse ind_lam
         select indx_mat
         nuse indx_mat
 #ifdef A_CDX
         set order to TAG indx_num in indx_mat
 #else
         set index to INDX_naz,INDX_num
 #endif
         @ 0,0 say magazyny[mag_poz]
         if dbseek(mag_biez+_skey[2],.f.) .and. stan#0 .and. cenA_zaK#_skey[3]
 #ifdef A_7
    _spform={|p,l|right(p,l)}
 #else
    _spform={|p,l|TranR(right(p,l), INDEXPIC )}
 #endif
            _sfor:=NIL
            _sbeg:=1
            _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
            _spocz:=mag_biez+left(index,7)
            _slth:=7
            refresh(0,_s)
            getlist:={}
            bl:=message(hb_UTF8ToStr("NIE POTRAFIĘ PRZEPISAĆ DO WŁASNEJ KARTOTEKI;- INDEKS ZAJĘTY;PROSZĘ PODAĆ NOWY SYTMBOL:"))
            @ bl[3]-1,bl[4]-18 get _skey[2] picture "@RK ####-###-###-###" valid !(dbseek(mag_biez+_skey[2],.f.) .and. stan#0 .and. cenA_zaK#_skey[3])
            read
            message(bl)
            if !updated()
               hlink:=NIL
               return (_sret:=.f.)
            endif
         endif
         if eof()
            append blank
            nr_mag:=mag_biez
 #ifndef A_WA
         elseif da#indx_mat->data_popr
            SELECT IND_LAM
            APPEND BLANK
            for bl=1 to fcount()
             fieldput(bl,indx_mat->(fieldget(bl)))
            next
            INDX_MAT->lamus:=recno()
            select STANY
 #endif
         endif
         data_popr:=da
         nazwa:=_skey[1]
         index:=_skey[2]
         cenA_zaK:=_skey[3]
         proc_vat:=_skey[4]
         jm:=_skey[5]
         przel:=_skey[6]
         jm_opcja:=_skey[7]
         proc_mar:=_skey[8]
         if hlink[3]
            getlist:={}
            _skey:=message(hb_UTF8ToStr("PROSZĘ PODAĆ PROCENT MARŻY:    %"))
            @ _skey[1]+1,_skey[4]-5 get proc_mar picture "@K ##"
            read
            message(_skey)
         endif
         hlink:=NIL
        endif
#endif

 #ifdef A_FILTERNAZ
         IF !empty(_sfilt) .and. '"$UPPER(INDX_MAT->naZwa)'$_sfilt
            _sfilt:=''
            _sfilb:=NIL
         ENDIF
 #endif
 #ifdef A_STSIMPLE
         IF !empty(_sfilt) .and. _sfilt=='STANY->NR_MAG="'+mag_biez+'"'
            _sfilt:=''
            _sfilb:=NIL
         ENDIF
 #endif

#ifndef STANY
 #ifdef A_MM
         IF !empty(STANY->nr_mag)
            mag_biez:=STANY->nr_mag
         ENDIF
 #endif
#endif
         return (_sret:=.T.)

#ifndef STANY
 #ifdef A_IZ
  CASE _skey=9 .and. (!STANY->(eof()) .or. MAIN->(dbseek(mag_biez+indx_mat->index))) // tab
 #else
  CASE _skey=9 .and. !STANY->(eof()) // tab
 #endif
    a:=push_stat()
    select indx_mat
    set order to TAG INDX_NUM
    set relation to
    private irec:=recno()
    select STANY
    set order to TAG STAN_MAG
#else
  CASE _skey=9
    a:=push_stat()
    set relation to
#endif
    PRIVATE w,s,wb,SB,we,se,w3,s3,w2,s2,w1,s1,_sramka:="GR+/B",chg:=.f.
    private srec:=STANY->(recno())
#ifdef A_LAN
   select daty
   goto 1
   select STANY
#endif
#ifdef A_WA
    do case
      case DatY->data_gran=DatY->d_z_rok
        sb:=zamkn_roku
        wb:=wart_roku
      case DatY->data_gran<=DatY->d_z_mies2
        sb:=zamk_mies2
        wb:=wart_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
        wb:=wart_mies1
    endcase
    se := stan
    we := wartOSC
#else
    do case
      case DatY->data_gran=DatY->d_z_rok
        sb:=zamkn_roku
      case DatY->data_gran<=DatY->d_z_mies2
        sb:=zamk_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
    endcase
    wb:=sb*(i_lam(DatY->data_gran))->cenA
    se := stan
    we := stan*(i_lam(STANY->data_zmian))->cenA
#endif
#ifdef A_JMO
  #define smiaR if(miar_opcja,(txt:=(i_lam(DatY->data_gran))->przel,restouT(sb,txt)),str(sb,10,3))
#else
  #define smiaR str(sb,10,3)
#endif
#ifndef STANY
 #ifdef A_IZ
if stany->(eof())
   select main
endif
 #endif
#endif
#ifdef A_OLZA
    szukam({1,max(0,maxcol()-79),maxrow(),maxcol(),0,0,hb_UTF8ToStr("Data┬─Dokument┬Zlecenie─┬─Przychód┬──Rozchód┬───"+WANAZ+"┬"+smiaR+"┬")+str(wb,10,CEOKR),{|D,_s|ltab(D,_s)},{|_skey,_s|tab(_skey,_s)},NR_MAG+INDEX+dtos(DatY->DATA_GRAN)+"Z",NR_MAG+INDEX+"Z"})
#else
    szukam({1,max(0,maxcol()-77),maxrow(),maxcol(),0,0,hb_UTF8ToStr("Data┬──Dokument┬Koszty┬─Przychód┬─Rozchód─┬───"+WANAZ+"┬"+smiaR+"┬")+str(wb,10,CEOKR),{|D,_s|ltab(D,_s)},{|_skey,_s|tab(_skey,_s)},NR_MAG+INDEX+dtos(DatY->DATA_GRAN)+"Z",NR_MAG+INDEX+"Z"})
#endif
#undef smiaR
    set filter to
    select main
    set filter to
    select indx_mat
#ifndef STANY
    set filter to
#endif
    pop_norec(a)
      if chg
         go _srec[_sm]
         cut(_s,.f.)
      endif

      IF LASTKEY()=4
       do while !(_sm=_si .and. _sef)
          if _sm=_si
            _sdol(_s,4)
          else
            RESTORE LINE _sm+_srow1-1
            ++_sm
            SAVE LINE _sm+_srow1-1
          endif
          if getscrtxt(savescreen(_sm+_srow1-1,_scol2 SSEND,_sm+_srow1-1,_scol2 SSEND))=hb_UTF8ToStr("■")
             KiBorD(chr(9))
             exit
          endif
       enddo
      ElseIF LASTKEY()=19
       do while !(_sm=1 .and. _sbf)
          if _sm=1
            _sgora(_s,19)
          else
            RESTORE LINE _sm+_srow1-1
            --_sm
            SAVE LINE _sm+_srow1-1
          endif
          if getscrtxt(savescreen(_sm+_srow1-1,_scol2 SSEND,_sm+_srow1-1,_scol2 SSEND))=hb_UTF8ToStr("■")
             KiBorD(chr(9))
             exit
          endif
       enddo
      ENDIF
   
   case _skey=-7 //F8
      miar_opcja=!miar_opcja
      aeval(_srec,{|x,i|dbgoto(x),setpos(_srow1+i-1,_scol1),dispout(padr(eval(_sprompt,0,_s),_scol2-COL()))},1,_si)
      SAVE LINE _sm+_srow1-1
#ifdef A_LAN
      go _srec[_sm] // onerefresh off
#endif
     
   case _skey=-9
      _slist(,_s)

   case _skey=-6
      _slist(".\i*.frm",_s)

   case _skey=-5
      getlist:={}
      na:=INDX_MAT->(dbstruct())
      txt:={}
      aeval(na,{|x|aadd(txt,pad(x[1],10))},2)
      bl:=window(5,58)
      if type('k_pola')='A'
         a:=k_pola
      else
         a:={'          ','          ','          ','          '}
      endif
      b:=aclone(a)
      @ bl[1]+1,bl[2]+2 Say "Wybierz pola:"
      @ bl[1]+2,bl[2]+2 GET a[1] picture "@K" valid {|g,r|r:=!empty(a[1]).and.aczojs(txt),if(r.and.len(getlist)<5,(SetPos( bl[1]+2, bl[2]+14 ),b[1]:=indx_mat->(&(a[1])),AAdd(GetList,_GET_( b[1], "b[1]", "@KS46", , ):display())) ,),empty(a[1]).or.r}
      @ bl[1]+3,bl[2]+2 GET a[2] picture "@K" valid {|g,r|r:=!empty(a[2]).and.aczojs(txt),if(r.and.len(getlist)<6,(SetPos( bl[1]+3, bl[2]+14 ),b[2]:=indx_mat->(&(a[2])),AAdd(GetList,_GET_( b[2], "b[2]", "@KS46", , ):display())) ,),empty(a[2]).or.r}
      @ bl[1]+4,bl[2]+2 GET a[3] picture "@K" valid {|g,r|r:=!empty(a[3]).and.aczojs(txt),if(r.and.len(getlist)<7,(SetPos( bl[1]+4, bl[2]+14 ),b[3]:=indx_mat->(&(a[3])),AAdd(GetList,_GET_( b[3], "b[3]", "@KS46", , ):display())) ,),empty(a[3]).or.r}
      @ bl[1]+5,bl[2]+2 GET a[4] picture "@K" valid {|g,r|r:=!empty(a[4]).and.aczojs(txt),if(r.and.len(getlist)<8,(SetPos( bl[1]+5, bl[2]+14 ),b[4]:=indx_mat->(&(a[4])),AAdd(GetList,_GET_( b[4], "b[4]", "@KS46", , ):display())) ,),empty(a[4]).or.r}
      @ bl[1]+1,bl[2]+20 Say "Podaj wartość:" UNICODE
      read
      window(bl)
      if readkey()=K_ESC .or. ALARM(hb_UTF8ToStr("CZY NA PEWNO CHCESZ ZAMIENIĆ WSZYSTKIE TOWARY ZACZYNAJĄCE SIĘ NA:")+_spocz,{"TAK","NIE"},2,2)#1
         return .f.
      endif
      k_pola:=a
      evakey(K_CTRL_PGUP,_s)
      do while .t.
        go _srec[_sm]
        c:=.f.
        for d:=1 to 4
          c:=c.or.!empty(a[d]) .and. b[d]#indx_mat->(fieldget(fieldpos(a[d])))
        next
        if c
           txt:=select()
           SELECT INDX_MAT
           LOCK
           if data_popr<=DatY->d_z_mies1 .and. DatY->d_z_mies1>DatY->d_z_rok+1
              SELECT IND_LAM
              APPEND BLANK
              for d:=1 to fcount()
                  fieldput(d,indx_mat->(fieldget(d)))
              next d
              INDX_MAT->lamus:=recno()
              SELECT INDX_MAT
              field->data_popr:=DatY->d_z_mies1+1
           ENDIF
           for d:=1 to 4
            if !empty(a[d]) .and. b[d]#fieldget(fieldpos(a[d]))
               fieldput(fieldpos(a[d]),b[d])
            endif
           next d
           UNLOCK IN INDX_MAT
           select (txt)
        endif
        if (_sef .and. _sm=_si) .or. nextkey()#0
           exit
        endif
        evakey(K_DOWN,_s)
      enddo

#ifdef A_KASA
#else
 #ifdef A_HLINK
   case _skey=-5 .and. ( hlink#NIL .or. aczojs((txt:=array(len(mag_link)),aeval(mag_link,{|x,i|txt[i]:=x[1]}),txt),"",@_skey))
       na:=UpP(trim(nazwa))
       if hlink#NIL
         nuse indx_mat
  #ifdef A_CDX
         set order to TAG indx_num in indx_mat
  #else
         set index to INDX_naz,INDX_num
  #endif
         select ind_lam
         nuse ind_lam
         select indx_mat
         seek mag_biez
         hlink:=NIL
         _sfor:=NIL
       else
         hlink:=mag_link[_skey]
         txt:=set(_SET_DEFAULT,hlink[2]+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+'\')
         begin sequence

         bl:=errorblock({|e|if(e:severity > 1, e:candefault:=.f.,),deferror(e)})
         nuse indx_mat
  #ifdef A_CDX
         set order to TAG indx_num in indx_mat
  #else
         set index to INDX_naz,INDX_num
  #endif
         select ind_lam
         nuse ind_lam
         select indx_mat
         seek left(hlink[1],2)
         _sfor:={||STANY->STAN#0}
         set(_SET_DEFAULT,txt)

         recover

         set(_SET_DEFAULT,txt)
         select (select("ind_lam"))
         nuse ind_lam
         select (select("indx_mat"))
         nuse indx_mat
  #ifdef A_CDX
         set order to TAG indx_num in indx_mat
  #else
         set index to INDX_naz,INDX_num
  #endif
         _sfor:=NIL
         hlink:=NIL
         seek mag_biez
         end sequence
         errorblock(bl)

       endif
       seek nr_mag+na
       _spform={|p,l|right(p,l)}
       _sbeg=SSBEG
       _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
       _spocz=nr_mag
       _slth=0
       @ 0,0 say magazyny[ascan(magazyny,nr_mag)]
       refresh(,_s)
 #else
  #ifdef A_GRAM
   case _skey=-5
  if _sfilb#NIL
    if _sfor=NIL
       bl:=_sfilb
    else
       bl:={||eval(_sfor).and.eval(_sfilb)}
    endif
  else
    bl:=_sfor
  endif
  txt:=message(hb_UTF8ToStr("Liczę stan,;proszę czekać."))
  seek _spocz
  _skey:=0
  DBEval( {||_skey+=STANY->stan*(i_lam(da))->gram},bl,{|| EvaldB(_swar,_spocz,_skon).AND.nextkey()=0},,, .T. )
  message(txt)
  if nextkey()=0
     @ _srow2,_scol2-15 say str(_skey/1000,9,2)+" kg" color _sramka
  endif
  #endif
 #endif
#endif


ENDCASE

RETURN(.F.)
*******************
