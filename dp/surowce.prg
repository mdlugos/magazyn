#define I hb_UTF8ToStr("│")
#include "dm_form.ch"

field ilosc,jedn,jmaG,skladnik,nazwa,indx_maT,przel,gram,element,danie,data,;
      posilek,cena,pozycja,kod,opis,dieta,gramatura

static poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw,op

MEMVAR CHANGED,diety,posilki,grupy,mies_rob

#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif

#define D_REST 4
************************
func sur_in(deep,n)

local stat,r,vars:={poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw,op}
poprec:=startrec:=oldrec:=0
if keyp#NIL
   deep=.t.
endif
keyp:=skladnik
nowy:=n .or. EOF()

stat:=push_stat()

begin sequence
#ifdef A_ELZ
  select cennik
     set order to 1
#endif
  select sklad
      set order to tag skl_skl
  select zapot
      set order to tag zap_skl
  select elementy
      SET ORDER TO tag ele_kod
  
  r:=min(maxcol(),hb_FieldLen([nazwa])+22)

  select SUROWCE
      SET ORDER TO tag sur_kod

  dbseek(keyp,.f.)
  curprompt:=trim(nazwa)+" "+jmaG

  r:=min(r,hb_FieldLen([nazwa])+min(7,hb_FieldLen([indx_maT]))+13)
  if col()+r>maxcol()
     setpos(row(),maxcol()-r)
  endif
  
  select zawar
  SET ORDER TO tag zaw_skl

 if dbseek(keyp)
    kibord(chr(3))
 endif

  select surowce

if deep
   inkey()
   if !eof()
    FORM_EDIT({col(),col()+r,5,1,999,;
{|f|sDOK1(f)},;
{|f|sdok2(f,{},deep)},;
{||setcursor(0)},;
{||DBSELECTAREA("zawar"),ordsetfocus("zaw_skl")},;
{|f|sdok4(f,{},deep)},;
{|f|dok6(f)}})
    endif
else
    lock
    FORM_EDIT({col(),col()+r,5,1,999,;
{|f|sDOK1(f)},;
{|f,g|sdok2(f,g,deep)},;
{|f|sdok3(f)},;
{||DBSELECTAREA("zawar"),ordsetfocus("zaw_skl")},;
{|f,g|sdok4(f,g,deep)},;
{|f|sdok5(f)}})
endif

end sequence

select surowce
r:=recno()
unlock

pop_stat(stat)

poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]
curprompt:=vars[5]
nowy:=vars[6]
na:=vars[7]
in:=vars[8]
jem:=vars[9]
prz:=vars[10]
jed:=vars[11]
gra:=vars[12]
il:=vars[13]
ce:=vars[14]
da:=vars[15]
keyf9:=vars[16]
kw:=vars[17]
op:=vars[18]
RETURN r
**************
stat pROC sDOK1(_f)
SET CURSOR OFF
SET COLOR TO (_sbkgr)
  @ 0,_fco1,4,_fco2 BOX UNICODE '╔═╗║║ ║║ '
  @ 5,_fco1,7,_fco2 BOX UNICODE '╠═╣║╝═╚║ '
  @ 5,_fco1+3 say 'F9'
  @ 0,_fco1+1 say "ZAWARTOŚĆ ELEMENTÓW" UNICODE
  @ 1,_fco1+9 say "składnik" UNICODE
  @ 1,_fco2-13 SAY 'kod magazynu'
#ifdef A_KODY
  @  3,_fco1+2  say A_KODY+':'
#else
#ifdef A_ELZ
  @ 3,_fco1+5 say 'cena            ważna od' UNICODE
#endif
#endif
#ifdef A_ODPADKI
  @ 4,_fco1+2 say left('1      =          , potrzeba        na 100 g części jadalnej',_fco2-_fco1-2) UNICODE
#else
  @ 4,_fco1+2 say '1      =          , zawartość w     :' UNICODE
#endif

RETURN
***********
stat proc sdok2(_f,getlist,deep)
  local now,greader,i,od
  field index,stan,jm,waznosc,data_przy,jedN
  select surowce
  if !nowy
     lock
  endif
  na:=nazwa
  in:=indx_maT
  prz:=przel
  jed:=jedN
  jem:=jmaG
  gra:=gram
  now:=if(nowy,"NOWY   ","POPRAWA")
#ifdef A_KODY
  kw:=kod
  if fieldpos([OPIS])<>0
     op:=opis
  endif
#endif
  @  5,_fco1+3 BOX if(empty(deep),'F9','══') UNICODE COLOR (_sbkgr)
  @  2,_fco1+2  get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  i:=min(7,hb_fieldlen('indx_maT'))
  @  2,_fco1+10 get NA picture "@KS"+hb_ntoc(_fco2-_fco1-i-12)
  @  2,_fco2-i-1 get in picture "@!KS"+hb_ntoc(i)
  @  4,_fco1+4  get jem picture "@K"
  @  4,_fco1+11 get prz picture "@K 9999" valid prz#0 .or.alarm(hb_UTF8ToStr("MUSI BYĆ RÓŻNY OD ZERA"),,3,3)=NIL
  @  4,_fco1+16 get jed picture "@K"
  @  4,_fco1+31 get gra PICTURE "@K 9999.9"
#ifdef A_ODPADKI
  select zawar
  i:=recno()
  SET ORDER TO tag zaw_ele
  if dbseek(A_ODPADKI + keyp,.f.) .and. elementy->(dbseek(A_ODPADKI,.f.))
    od:=zawar->ilosc
    if elementy->jedn='g'
      atail(getlist):postblock:={|g|if(g:CHANGED,hb_DispOutAt(4,_fco1+41,pad(hb_ntoc(gra-od,0),4),_sbnorm),),.t.}
      @  4,_fco1+41 SAY pad(hb_ntoc(gra-od,0),4) COLOR _sbnorm
    elseif elementy->jedn='%'
      @  4,_fco1+41 SAY pad(hb_ntoc(gra*(100-od)/100,0),4) COLOR _sbnorm
      atail(getlist):postblock:={|g|if(g:CHANGED,hb_DispOutAt(4,_fco1+41,pad(hb_ntoc(gra*(100-od)/100,0),4),_sbnorm),),.t.}
    endif
  endif
  SET ORDER TO tag zaw_skl
  goto i
  select surowce
#endif
#ifdef A_KODY
  @  3,_fco1+len(A_KODY)+4  get kw WHEN .f. COLOR _sbnorm
  if !nowy .and. fieldpos('OPIS')<>0
    getl op PICTURE "@S"+lTrim(sTr(_fco2-col()-1)) WHEN .f. COLOR _sbnorm
  endif
#else
#ifdef A_ELZ
  cennik->(dbseek(keyp,.f.))
  ce:=cennik->cena
  da:=cennik->data
  @  3,_fco1+10 get ce picture "@KE #####.##" valid {||if(!Empty(ce).and.empty(da),da:=DatE(),),getlist[5]:display(),.t.}
  @  3,_fco1+30 get da
#endif
#endif
if empty(deep)
  greader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
#ifndef A_ELZ
#ifdef A_KODYyy
#define D_KODY +I+KOD
#else
#define D_KODY
#endif
  getlist[1]:reader:=getlist[4]:reader:=getlist[5]:reader:=getlist[6]:reader:=getlist[7]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,,,1,0,hb_UTF8ToStr("PRZEGLĄD MAGAZYNU SPOŻYWCZEGO"),{||INDEX+I+NAZWA D_KODY;
  +hb_UTF8ToStr(IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"°","│"));
  +STR(STANY->STAN)+" "+JM},{|k,s D_MYSZ|stanmag(k,s D_MYSZ)},"",.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
  getlist[2]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,,,1,0,hb_UTF8ToStr("PRZEGLĄD MAGAZYNU SPOŻYWCZEGO"),{||INDEX+I+NAZWA D_KODY;
  +hb_UTF8ToStr(IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"°","│"));
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(na)),.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
  getlist[3]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,,,1,0,hb_UTF8ToStr("PRZEGLĄD MAGAZYNU SPOŻYWCZEGO"),{||INDEX+I+NAZWA D_KODY;
  +hb_UTF8ToStr(IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"°","│"));
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(in)),.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
#endif
endif
  __setproc(procname(0))
return
************
#ifndef A_ELZ
stat func showhead(getlist)

  Na:=LEFT(INDX_MAT->NAZWA,LEN(Na))
  In:=INDX_MAT->INDEX
#ifdef A_KODY
//  kw:=INDX_MAT->kod
#endif

  if INDX_MAT->JM="kg"
     jed:="gram"
     prz:=1000
  elseif INDX_MAT->JM="litr"
     jed:="ml  "
     prz:=1000
#ifdef A_JMALT
  elseif INDX_MAT->jm_opcja="KG"
     jed:="gram"
     prz:=INDX_MAT->przel*1000
  elseif INDX_MAT->jm_opcja="G"
     jed:="gram"
     prz:=INDX_MAT->przel
  elseif INDX_MAT->JM_OPCJA="L"
     jed:="ml  "
     prz:=INDX_MAT->przel*1000
#endif
  elseif INDX_MAT->JM#jem
     jed:=INDX_MAT->JM
     prz:=1
  endif
  Jem:=INDX_MAT->JM
  aeval(getlist,{|g|g:display()},2,5)
return .t.
#endif
***********
stat proc sdok3(_f)
  local skl
  field jedN
  if updated()
      keyp:=skladnik
      if startrec=0
         if zawar->(dbseek(keyp))
           keyf9:=keyp
           startrec:=zawar->(recno())
           curprompt:=trim(nazwa)+" "+jmaG
         //@ 5,_fco1+3 say 'F9' color _sbkgr
         endif
      endif
      changed:=.t.
      _fj:=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag sur_naZ
      if dbseek(UpP(na)) .and. alarm(hb_UTF8ToStr("TAKA NAZWA JUŻ ISTNIEJE;CZY DOPISAĆ MIMO WSZYSTKO"),{"TAK","NIE"})=2 .OR. empty(NA)
#ifdef __PLATFORM__DOS        
        @ 6,_fco1,7,_fco2 BOX UNICODE '║ ║║╝─╚║ ' color _sbkgr
#else
        @ 6,_fco1,7,_fco2 BOX UNICODE '║ ║║╜─╙║ ' color _sbkgr
#endif        
        RESTSCREEN(1+2*_fskip+_frow,_fco1,MaxRow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow-_fscr0)*D_REST+1))
        _fpopkey:=.f.
        return
      endif
      @ 6,_fco1,7,_fco2 BOX UNICODE '║ ║║╝═╚║ ' color _sbkgr
      _fpopkey:=.t.
      SET ORDER TO tag sur_kod
#ifdef A_LPNUM
      go bottom
      skl:=str(val(skladnik)+1,hb_fieldlen('skladnik'))
#else
      GO lastrec()
      skl:=if(eof(),chr(0)+chr(0),i2bin(bin2w(skladnik)+1))
#endif
      APPEND BLANK
      skladnik:=skl
#ifdef A_DEMO
      poprec:=0
#else
      poprec:=startrec
#endif
    elseif empty(na)
      lock
      select sklad
      set order to tag skl_skl
      select zapot
      set order to tag zap_skl
      if !dbseek(surowce->skladnik) .and. !sklad->(dbseek(surowce->skladnik))
        select zawar
        set order to tag zaw_skl
        dbseek(surowce->skladnik)
#ifdef A_LAN
        delete while skladnik==surowce->skladnik rest FOR reclock()
#else
        delete while skladnik==surowce->skladnik rest
#endif
        unlock
#ifdef A_ELZ
        select cennik
        dbseek(surowce->skladnik,.f.)
#ifdef A_LAN
        delete while skladnik==surowce->skladnik rest FOR reclock()
#else
        delete while skladnik==surowce->skladnik rest
#endif
        unlock
#endif
        select surowce
        delete
      endif
      select surowce
      unlock
      break
    else
      lock
    endif
    nazwa:=na
    indx_maT:=in
    przel:=prz
    jedN:=jed
    jmaG:=jem
    gram:=gra
#ifdef A_KODY
    kod:=kw
    if fieldpos([opis])>0
       opis:=op
    endif
#endif
#ifdef A_ELZ
    select cennik
    if if(dbseek(surowce->skladnik,.f.),cena<>ce .or. data<>da,ce<>0)
       if !eof() .and. da=data .and. ce#cena .or. da<=data .and. ce=cena
          lock
       else
          dbappend()
          skladnik:=surowce->skladnik
       endif
       cena:=ce
       data:=da
       unlock
    endif
    select surowce
#endif
endif

return
***********
stat proc sDOK4(_f,getlist,deep)
static fpstart:=0
  _fnowy:=!skladnik==surowce->skladnik
#ifdef A_LAN
  if !_fnowy .and. _fpopkey
    lock
  endif
#endif
  if _fnowy .and. poprec#0
     oldrec:=recno()
     go poprec
     _fpopkey:=.t.
     _fpos:=fpstart
  else
     oldrec:=0
  endif

    if _fnowy .and. oldrec=0
     if !_fpopkey .and. _fi>1
       if !deep
          kibord(chr(27))
       endif
       return
     endif
     il:=0
     _fpos:=1
      na:=space(elementy->(hb_fieldlen('nazwa')))
    else
      il:=ilosc
      elementy->(dbseek(zawar->element,.f.))
      na:=elementy->nazwa
      showel(_f)
      if oldrec#0
         startrec:=poprec
         skip
         poprec:=if(skladnik==keyf9,recno(),0)
         go oldrec
      endif
    endif

    @ _fk,_fco1+6 GET na PICTURE "@KS"+hb_nToc(_fco2-_fco1-21) VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=elval(_f,@na,@poprec,oldrec,startrec) .and. showel(_f,getlist),setkey(-8,k),r}
    GETL il picture "#####.###" valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),.t.}
     __setproc(procname(0))
    fpstart:=0
if empty(deep)
     getlist[1]:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||skladnik==surowce->skladnik}))})
#endif
endif
RETURN
********
stat proc f9(g,_f,getlist)
   local r:=elementy->(recno()),curprompt,s:=surowce->(recno()),are:=select(),lr
   oldrec:=recno()
   if startrec=0
      poprec:=0
      if surowce(.f.)
         select surowce
         set order to tag sur_kod
         curprompt:=trim(nazwa)+" "+jmaG
         select zawar
         seek surowce->skladnik
         keyf9:=surowce->skladnik
         startrec:=recno()
         if surowce->(select())=are
            poprec:=startrec
#ifdef A_KODY
            varput(getlist,'kw',surowce->kod)
            if surowce->(fieldpos([opis])>0)
               varput(getlist,'op',surowce->opis)
            endif
#endif
            varput(getlist,'gra',surowce->gram)
            updated(.t.)
            @ 6,_fco1,7,_fco2 BOX '║ ║║╝─╚║ ' color _sbkgr
            RESTSCREEN(1+2*_fskip+_frow,_fco1,MaxRow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow-_fscr0)*D_REST+1))
            surowce->(dbgoto(s))
            dbseek(surowce->skladnik)
#ifdef A_LAN
            dbeval({||dbdelete()},{||reclock()},{||skladnik=surowce->skladnik})
#else
            dbeval({||dbdelete()},,{||skladnik=surowce->skladnik})
#endif
            select surowce
            return
         endif
         surowce->(dbgoto(s))
      else
         select surowce
         set order to tag sur_kod
         go s
         select elementy
         go r
         select zawar
         go oldrec
         select(are)
         return
      endif
   else
      go startrec
      keyf9:=skladnik
      select surowce
      set order to tag sur_kod
      dbseek(keyf9)
      curprompt:=trim(nazwa)+" "+jmaG
      go s
      select zawar
   endif
   set relation to element into elementy
   go if(poprec=0,startrec,poprec)
   lr:=min(maxcol()-16,elemety->(hb_fieldlen([nazwa])))
   if szukam({2,col(),,,0,0,curprompt,;
     {||left(elementy->nazwa,lr)+I+str(ilosc)+" "+elementy->jedn},;
     {|k,_s|(_sret:=k=13).or.ele(k,_s,.f.)},keyf9})
    set relation to
    poprec:=recno()
    g:killfocus()
    il:=ilosc
    na:=elementy->nazwa
    g:setfocus()
    showel(_f)
    getlist[2]:display()
    startrec:=poprec
    skip
    poprec:=if(skladnik==keyf9,recno(),0)
    updated(g:changed:=.t.)
   else
    set relation to
    elementy->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
RETURN
*********
stat PROC sDOK5(_f)
local totrec
    if (updated() .or. oldrec#0) .and. !((il=0 .or. _fkey=27).and. _fnowy)
      changed:=.t.


        if _fnowy
          _fnowy:=.f.
          append blank
          SKLADNIK:=SUROWCE->SKLADNIK
#ifdef A_LPNUM
          pozycja:=str(_fi,hb_fieldlen('pozycja'))
#endif
        endif

      element:=elementy->element
#ifdef A_ODPADKI
      if oldrec#0 .or. element#A_ODPADKI .or. round(il-ilosc,3)=0
      elseif elementy->jedn='g'
        if 1=alarm(hb_UTF8ToStr('Czy zmienić wydajność części jadalnej?'),{'Tak','Nie'})
          gra:=surowce->gram:=gra+il-ilosc
        endif
        @  4,_fco1+31 SAY gra PICTURE "@K 9999.9" COLOR _sbnorm
        @  4,_fco1+41 SAY pad(hb_ntoc(gra-IL,0),4) COLOR _sbnorm
      elseif elementy->jedn='%'
        if 1=alarm(hb_UTF8ToStr('Czy przeliczyć wydajność na 100 g części jadalnej?'),{'Tak','Nie'})
          gra:=surowce->gram:=round(10000/(100-IL),1)
        endif
        @  4,_fco1+31 SAY gra PICTURE "@K 9999.9" COLOR _sbnorm
        @  4,_fco1+41 SAY pad(hb_ntoc(gra*(100-IL)/100,0),4) COLOR _sbnorm
      endif
#endif        

      ILOSC:=IL
      if il=0
          delete
          _fnowy:=.t.
#ifdef A_LPNUM
          if _fi<_flp
          totrec:=recno()
          skip -1  // po zmianie pozycji byłby niepewny układ, w dm_form i tak skip
          IF !BOF()
             totrec:=recno()
          ELSE
             GO totrec
          ENDIF
          skip
#ifdef A_LAN
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while SKLADNIK=SUROWCE->SKLADNIK for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while SKLADNIK=SUROWCE->SKLADNIK
          go totrec
#endif
          endif
#endif
        endif
    elseif oldrec#0 .and. _fkey#27
      _fkey:=13
      _fnowy:=.f.
    endif
    unlock
RETURN
********
stat func showel(_f)
  @ _fk,_fco2-5 SAY elementy->jedn color _sbkgr
return .t.
*****************
func SURowce(upden)
  local r:=6
DEFAULT upden TO .t.
SELECT SUROWCE
SET ORDER TO tag sur_naZ
#ifdef A_ELZ
select cennik
set order to 1
r+=hb_fieldLen([cena])+12
SELECT SUROWCE
#else
#ifdef A_KODY
r+=hb_FieldLen([KOD])+1
#endif
#endif
r:=min(hb_FieldLen([nazwa]),maxcol()-r)
select zawar
set order to tag "zaw_skl"
#ifdef A_ELZ
select surowce
set relation to skladnik into cennik
return SZUKAM({0,,,,1,0,hb_UTF8ToStr("PRZEGLĄD SUROWCÓW CENA DATA"),{||Left(NAZWA,r)+if(zawar->(found()),I,"!")+jmaG+I+str(cennik->cena)+I+dtoc(cennik->data)},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#else
select surowce
set relation to skladnik into zawar
#ifdef A_KODY
return SZUKAM({0,,,,12,0,hb_UTF8ToStr("PRZEGLĄD SUROWCÓW"),{||KOD+"│"+Left(NAZWA,r)+if(zawar->(found()),I,"!")+jmaG},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#else
return SZUKAM({0,,,,1,0,hb_UTF8ToStr("PRZEGLĄD SUROWCÓW"),{||Left(NAZWA,r)+if(zawar->(found()),I,"!")+jmaG},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#endif
#endif
*******************
FUNCTION SURVAL(_f,getlist,na,poprec,oldrec,startrec,aflag,apos)

local r:=if(alias()="ZAPOT",6,12),da,sk,su,re,za,znalaz,sel

/*
#ifdef A_GOCZ
if empty(na)
   return .t.
endif
#endif
*/
if na=surowce->nazwa .and. !surowce->(eof())
   return .t.
endif
sel:=select()
da:=dania->(recno())
sk:=sklad->(recno())
re:=relewy->(recno())
za:=zapot->(recno())
#ifdef A_ELZ
select cennik
set order to 1
r+=hb_fieldLen([cena])+12
select surowce
su:=recno()
SET ORDER TO tag sur_naZ
set relation to skladnik into cennik
r:=min(hb_FieldLen([nazwa]),maxcol()-r)
znalaz:=SZUKAM({0,,,,1,len(trim(na)),hb_UTF8ToStr("PRZEGLĄD SUROWCÓW"),if(alias(sel)="ZAPOT",{||left(NAZWA,r)+I+jmaG+I+str(cennik->cena)+I+dtoc(cennik->data)},{||left(NAZWA,r)+"("+jmaG+hb_UTF8ToStr(")│")+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#else
select surowce
su:=recno()
SET ORDER TO tag sur_naZ
#ifdef A_KODY
r+=hb_FieldLen([KOD])+1
r:=min(hb_FieldLen([nazwa]),maxcol()-r)
znalaz:=SZUKAM({0,,,,12,len(trim(na)),hb_UTF8ToStr("PRZEGLĄD SUROWCÓW"),if(alias(sel)="ZAPOT",{||KOD+I+left(NAZWA,r)+I+jmaG},{||KOD+I+left(NAZWA,r)+"("+jmaG+hb_UTF8ToStr(")│")+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#else
r:=min(hb_FieldLen([nazwa]),maxcol()-r)
znalaz:=SZUKAM({0,,,,1,len(trim(na)),hb_UTF8ToStr("PRZEGLĄD SUROWCÓW"),if(alias(sel)="ZAPOT",{||left(NAZWA,r)+I+jmaG},{||left(NAZWA,r)+"("+jmaG+hb_UTF8ToStr(")│")+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#endif
#endif
  set order to tag sur_kod
  select zapot
  SET ORDER TO tag zap_rel
    SET RELATION TO
    goto za
  select relewy
    goto re
  select dania
    SET ORDER TO tag dan_kod
    goto da
  select sklad
    SET ORDER TO tag skl_dan
    SET RELATION TO
  goto sk
  select (sel)
if znalaz
  IF _fnowy .and. oldrec#0 .and. surowce->(recno())#su
    if aflag
       --apos
    else
       poprec:=startrec
    endif
  ENDIF
  na:=surowce->nazwa
  //getlist[1]:varput(surowce->nazwa)
  updated(.t.)
  return .t.
endif
aflag:=.f.
surowce->(dbgoto(su))
RETURN .F.
**************************
function sur(_skey,_s,upden D_MYSZ)
field index,nazwa,stan,jm,waznosc,data_przy
static choice:=0
local stat,r
do case
  CASE _SKEY=0 .and. alias()="SUROWCE"
    set order to tag sur_naz
    //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    if ! ( (EvaldB(_swar,_spocz).or.dbseek(_spocz)).and._skip(0,,_s) )
      _spocz=LEFT(_spocz,len(_spocz)-_slth)
      _slth=0
      _sef:=.f.
      if !EvaldB(_swar,_spocz)
         _skip(-1,,_s)
         if !EvaldB(_swar,_spocz)
            seek _spocz
         endif
      endif
    endif
    set cursor on
#ifdef A_MYSZ
    _skproc[14]:=NIL

   case _skey=14
    if bx=1.and.dx=_srow1+_sm-1.and.cx>_scol2-5.and.cx<_scol2
       evakey(22,_s)
    else
       return mysz(_s,bx,cx,dx,myszflag)
    endif
#endif

  case _skey=27
    return .t.

   case _skey=43
      go _srec[_sm]
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
    private changed:=.f.
    _skey:=sur_in(!upden,.t.)
    if changed==.t.
      if alias()='SUROWCE'
         go _skey
      endif
      if ! EvaldB(_swar,_spocz)
        if _slth=0
           dbseek(_spocz)
        else
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
       endif
      endif
      refresh(,_s)
    elseif upden
      go _srec[_sm]
      REFRESH LINE _srow1-1+_sm DIRECTION 0
    endif
    return .t.

      
  case _skey=22

     private changed:=.f.

    _skey:=sur_in(!upden,.f.)

    if changed==.t.
      if alias()='SUROWCE'
         go _skey
      endif
      if ! EvaldB(_swar,_spocz)
        if _slth=0
           dbseek(_spocz)
        else
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
       endif
      endif
      refresh(,_s)
    elseif upden
      go _srec[_sm]
      REFRESH LINE _srow1-1+_sm DIRECTION 0
    endif

  CASE _si=0
  case _skey=9 .or. _skey=hb_keyCode('\')
    stat:=push_stat()
#ifdef A_ELZ
    if _skey=92
       go _srec[_sm]
       _slth=_slth-1
       _spocz=left(_spocz,LEN(_spocz)-1)
       choice:=4
    else
       alarm(hb_UTF8ToStr("WYBIERZ COŚ:"),{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE","CENNIK"},@choice)
    endif
    if choice=1
#else
    if alarm(hb_UTF8ToStr("WYBIERZ COŚ:"),{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE"},@choice)=1
#endif
       select dania
       set order to tag dan_kod
       r:=min(maxcol()-12-A_DILTH,hb_fieldlen([nazwa]))
       select sklad
       set order to tag skl_skl
       set relation to danie into dania
       dbseek(surowce->skladnik,.f.)
       szukam({1,col(),,,0,0,,{||dania->(left(nazwa,r)+if(""=opis,I,"&")+left(dieta,A_DILTH)+I+gramatura+" "+jedn)},{|_skey,_s D_MYSZ|_skey:=if(_skey=13,9,_skey),danszuk(_skey,_s,upden,r D_MYSZ)},surowce->skladnik})
    elseif choice=2
       select zapot
       set order to tag zap_skl
        set relation to RELEWY->(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,'')) into relewy
       dbseek(surowce->skladnik,.f.)
       szukam({1,min(maxcol()-27,col()+5),MaxRow(),,1,6,hb_UTF8ToStr("Data───┬P┬")+padc(hb_UTF8ToStr("Dieta"),A_DILTH,hb_UTF8ToStr("─"))+hb_UTF8ToStr("┬ile ")+trim(surowce->jmaG),{||tran(Dtos(data)+posilek+dieta,hb_UTF8ToStr("@R ####.##.##│X│")+REPLICATE("X",A_DILTH))+I+str(ILOSC)},{|k,s|RELe(k,s,upden)},surowce->skladnik+left(dtos(mies_rob),6)})
    elseif choice=3
       SZUKAM({1,1,,,1,0,hb_UTF8ToStr("PRZEGLĄD MAGAZYNU SPOŻYWCZEGO"),{||INDEX+I+NAZWA;
       +hb_UTF8ToStr(IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"°","│"));
       +STR(STANY->STAN)+" "+JM},{|_skey,_s D_MYSZ|if(_skey=13,.f.,STANMAG(_skey,_s D_MYSZ))},trim(surowce->indx_mat),.F.})
#ifdef A_ELZ
    elseif choice=4
       select cennik
       setpos(row()+2,col())
       dbseek(surowce->skladnik)
       szukam({1,,,,0,,'Cena za '+surowce->jmaG,{||dtoc(data)+I+str(cena)},{|k,s|cen(k,s)},surowce->skladnik+chr(0),surowce->skladnik+hb_UCHAR(0x0A0)})
       dbseek(surowce->skladnik,.f.)
       select surowce
       REFRESH LINE _srow1-1+_sm DIRECTION 0
#endif
    endif
    pop_stat(stat)
    if _skey>=32
       return .t.
    endif

   case _skey=13
      return _sret:=.t.
   case _skey=-8
      _sfil(_s)
      
   case _skey=-9
      _slist("."+HB_ps()+left(alias(),3)+"*.frm",_s)
#ifdef A_KODY
   case alias()<>'SUROWCE'
      return .f.

   CASE _skey=2 .AND. _sbeg=1 // ^>
    SET ORDER TO tag sur_naz
    _sbeg:=hb_fieldlen('kod')+2
    //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _spform:={|p|tranr(p,"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    _spform:={|p,l|RIGHT(p,l)}
    _spocz:=''
    _slth:=0
    refresh(1,_s)

   CASE _skey=26 .AND. _sbeg#1 // ^<
    SET ORDER TO tag sur_smb
    _spocz:=''
    _spform:={|p|tranr(p,repl("X",hb_fieldlen('kod'))+"|"+repl("X",hb_fieldlen('nazwa')))}
    _slth:=0
    _sbeg:=1
    //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(1,_s)
#endif
endcase
return .f.
#ifdef A_ELZ
**************************
function cen(_skey,_s)
LOCAL c,d,getlist
do case
   case _skey=27
    return .t.
   case _skey=22 .or. _skey=hb_keyCode('+')
    d:=DatE()
    if _si=0
         _srec[1]=recno()
         _sm=1
         go lastrec()+1
    elseif _skey=hb_keyCode('+')
       go _srec[_sm]
       _slth=_slth-1
       _spocz=left(_spocz,LEN(_spocz)-1)
    else
         lock
         d:=data
    endif
    c:=cena
    getlist:={}
    @ _srow1+_sm-1,_scol1 get d
    getl c picture "@K"
    read
    if readkey()#27 .and. updated()
      if empty(c) .or. empty(d)
         delete
      elseif d=data .and. c#cena .or. d<=data .and. c=cena
         cena:=c
         data:=d
      else
        append blank
        data:=d
        cena:=c
        skladnik:=surowce->skladnik
      endif
    endif
    refresh(,_s)
    if _skey>=32
       return .t.
    endif

  case _skey=19
       kibord(chr(27)+chr(5)+'\')

  case _skey=4
       kibord(chr(27)+chr(24)+'\')

  CASE _si=0

  case _skey=-8
      _sfil(_s)
      
  case _skey=-9
      _slist("."+HB_ps()+left(alias(),3)+"*.frm",_s)

endcase
return .f.
***********
#endif
********************
