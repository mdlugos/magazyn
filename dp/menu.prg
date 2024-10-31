#define I hb_UTF8ToStr("│")
#include "dm_form.ch"
#include "inkey.ch"
#include "getexit.ch"
#define WARTOSC bin2d(field->d_wartosc)
MEMVAR CHANGED,diety,posilki,grupy,mies_rob,posstr,posgr
static poprec,oldrec,keyp,startrec,dan,die,pg
#ifdef A_DODATKI
static ilp
#endif
#ifdef A_POLOWA
  #define D_ILPIC "@KZE ###.#"
#else
  #define D_ILPIC "@KZ #####"
#endif

field danie,posilek,DATA,nazwa,gramatura,jedn,dieta,ilosc,ile_pos,d_wartosc,opis,pozycja

*********************
func jad_in(deep)


LOCAL F3,stat,r,vars:={poprec,oldrec,keyp,startrec,dan,die}
if keyp#NIL
   deep=.t.
endif
poprec:=startrec:=oldrec:=0
keyp:={data,posilek}
stat:=push_stat()
begin sequence

  select zapot
      SET ORDER TO tag zap_rel
  select menu
      SET ORDER TO tag menu_rel
  select dania
      SET ORDER TO tag dan_kod
  SELECT RELEWY
  dbseek(dseek(,'data,posilek,dieta',keyp[1],keyp[2],''),.f.)
  keyp:=dtos(keyp[1])+keyp[2]

 select menu

 if dbseek(keyp)
    kibord(chr(3))
 endif

 SELECT RELEWY

  if deep
  inkey()
    if !eof()
    F3:=setkey(-2,NIL)
    FORM_EDIT({14-A_DILTH,76,3,1,999,;
{|f|MDOK1(f)},;
{||mdok2({})},;
{||setcursor(0)},;
{||DBSELECTAREA("MENU"),ordsetfocus("menu_rel")},;
{|f|Mdok4(f,{},deep)},;
{|f|dok6(f,-3)}})
    setkey(-2,F3)
    endif
else
    lock

    FORM_EDIT({14-A_DILTH,76,3,1,999,;
{|f|MDOK1(f)},;
{|f,g|mdok2(g)},;
{|f,g|dok3(f,g,@poprec,@keyp,@startrec),showh(relewy->data,relewy->posilek)},;
{||DBSELECTAREA("MENU"),ordsetfocus("menu_rel")},;
{|f,g|Mdok4(f,g,deep)},;
{|f|Mdok5(f)}})
endif
end sequence
  SELECT RELEWY
  r:=recno()
  unlock

pop_stat(stat)

poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]
dan:=vars[5]
die:=vars[6]
RETURN r
**************
static PROCEDURE MDOK1(_f)

 SET CURSOR OFF
 SET COLOR TO (_SBKGR)
  hb_scroll(0,_fco1,0,_fco2)
  @ 0,_fco1,3,_fco2 BOX UNICODE '╔═╗║║ ║║ '
  @ 0,_fco1+1 SAY "JADŁOSPIS" UNICODE
  @ 1,10 say "Ilość" UNICODE
  @ 1,20 say "Cena" 
  @ 1,40 say "Data"
  @ 1,46 say "Pos."
  @ 1,53 say "Zapotrzebowanie"
  @ 3,_fco1,5,_fco2 BOX UNICODE '╠═╣║╝═╚║ '
  @ 3,12 SAY '════Danie═══════════════════════════════════Gramatura═Dieta═Ilp' UNICODE

return
********
static proc showh(da,kon)
  @ 2,10 say ile_pos picture D_ILPIC color _sbkgr
  @ 2,17 say strpic(WARTOSC/ile_pos,7,A_ZAOKR,"@E ",.t.) color _sbkgr
  @ 2,49 say SubStr(posilki[max(1,ascan(posilki,kon))],3,11) COLOR _sbkgr
  @ 2,60 say if(zapot->(dbseek(dtos(da)+kon,.f.)),"jest","brak") color _sbkgr

#ifdef A_WAGI
  pg:=SubStr(PosGr,at(kon,PosStr),1)
#else
  pg:=kon
#endif
return
********
static proc mdok2(getlist)
  local da,kon
  select relewy
  if eof()
     da:=DatE()
     kon:=left(posilki[1],1)
  else
     da:=data
     kon:=posilek
  endif
  showh(da,kon)
  @ 2,35 get da valid {||showh(da,kon),.t.}
  @ 2,47 get kon valid {|y|y:=aczojs(posilki),showh(da,kon),y}
  __setproc(procname(0))

return
*************
static proc mDOK4(_f,getlist,deep)
  static fpstart:=0
  _fnowy:=DTOS(data)+posilek#relewy->(DTOS(data)+posilek)
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
  if !_fnowy .or. oldrec#0
    dania->(dbseek(menu->danie,.f.))
    dan:=dania->nazwa
    DIE:=dieta
#ifdef A_DODATKI
    ilp:=ile_pos
#endif
    if !deep .and. dania->posilek#pg
      lock in dania
      DANIA->posilek:=pg
      unlock in dania
    endif
    if oldrec#0
       startrec:=poprec
       skip
       poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
       go oldrec
    endif
    showgram(_f)
  elseif !_fpopkey .and. _fi>1
     if !deep
        kibord(chr(27))
     endif
     return
  else
    dan:=space(dania->(hb_fieldlen('nazwa')))
    DIE:=space(hb_fieldlen('dieta'))
#ifdef A_DODATKI
    ilp:=0
#endif
    _fpos:=1
  endif
  if !_fnowy
     lock
  endif
  //@ _fk, 15 SAY chr(29)
  @ _fk, _fco1+6 GET dan PICTURE "@KS"+lTrim(sTr(49-A_DILTH)) VALID {|r,k|if(r:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=danval(_f,getlist) .and. showgram(_f),setkey(-8,k),r}
  //SAYL chr(9)
  @ _fk, 69-A_DILTH GET DIE PICTURE "@KS"+HB_MACRO2STRING(A_DILTH) VALID {|g|if(g:changed.and.fpstart=0,fpstart:=2,),dival(g).and.showgram(_f)}
#ifdef A_DODATKI
  //SAYL chr(9)
  @ _fk, 70 GET ilp PICTURE D_ILPIC VALID showgram(_f) SEND block:={|x|if(x<>NIL,ilp:=if(round(x-ipcalc(die),1)<>0,x,0),if(ilp=0,ipcalc(die),ilp))}
#endif
  //SAYL chr(3)
  __setproc(procname(0))
  getlist[1]:reader:=getlist[2]:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
  fpstart:=0
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||dtos(data)+posilek=relewy->(dtos(data)+posilek)}))})
#endif
RETURN
#ifdef A_LPNUM
*****************
proc doinsline(_f,getlist,g,bl)
local a:={},b:={},i
   for i:=1 to fcount()
      aadd(b,fieldget(i))
   next i
#ifdef A_LAN
#define D_LAN {||reclock(,,,,recno())}
#else
#define D_LAN
#endif
   dbeval({||aadd(a,recno())},D_LAN,bl)
#undef D_LAN
   AEVAL(a,{|r,i|dbgoto(r),FIELD->pozycja:=STR(_fi+i,hb_fieldlen('pozycja'))})
   append blank
   aeval(b,{|p,i|fieldput(i,p)})
   if alias()="MAIN"
      FIELD->ile_pos:=0
   endif
   pozycja:=str(_fi,hb_fieldlen('pozycja'))
#ifdef A_DODATKI
   if alias()="MENU"
      i:=hb_fieldlen('pozycja')
      select ZAPOT
      seek keyp
      dbeval({||FIELD->danie:=str(val(danie)+1,i)},{||len(trim(danie))=i.and.danie>=menu->pozycja.and.reclock()},{||dtos(data)+posilek=keyp})
      UNLOCK
      select MENU
   endif
#endif
   LOCK
   //dbcommit()
   ++_flp
   if _fskip*(_fl-_fj+2)+_frow>maxrow()
      if _fl=_flp-1
#ifdef __PLATFORM__DOS
         @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╚'+replicate('─',_fco2-_fco1-1)+'╝' UNICODE color _sbkgr
#else         
         @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╙'+replicate('─',_fco2-_fco1-1)+'╜' UNICODE color _sbkgr
#endif         
      endif
   else
      ++_fl
      @ (_fl-_fj)*_fskip+_frow,_fco1,(_fl-_fj+1)*_fskip+_frow,_fco2 BOX UNICODE '║ ║║╝═╚║ ' color _sbkgr
      @ (_fl-_fj)*_fskip+_frow,_fco1+1 SAY str(_fl,3)+'.' color _sbkgr
   endif
   g:killfocus()
   a:=savescreen(_fk,_fco1+5,_fk+_fskip-1,_fco2-1)
   hb_scroll(_fk,_fco1+5,_frow+_fskip*(_fl-_fj+1)-1,_fco2-1,-_fskip)
   restscreen(_fk,_fco1+5,_fk+_fskip-1,_fco2-1,a)
   g:exitState:=GE_TOP
   g:setfocus()
   updated(.t.)
return
#endif
********
stat proc f9(g,_f,getlist)
local r:=dania->(recno())
   oldrec:=recno()
   set relation to danie into dania
   if startrec#0
      go if(poprec=0,startrec,poprec)
   endif
   if szukam({2,min(col(),maxcol()-69),maxrow(),,1,9,;
     hb_UTF8ToStr("Jadłospis"),;
     {||tran(dtos(data)+posilek,"@R XXXX.XX.XX|X")+I+dania->nazwa+I+dieta+I+dania->gramatura+" "+dania->jedn},;
     {|k,_s|(_sret:=k=13).or.rele(k,_s,.f.)},keyp})
    set relation to
    poprec:=recno()
    g:killfocus()
      dan:=dania->nazwa
      die:=dieta
#ifdef A_DODATKI
      ilp:=ile_pos
#endif
    if dania->posilek#pg
      lock in dania
      DANIA->posilek:=pg
      unlock in dania
    endif
    showgram(_f)
    g:setfocus()
    aeval(getlist,{|g|g:display()})
    startrec:=poprec
    keyp:=dtos(data)+posilek
    skip
    poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
    aeval(getlist,{|g|g:display()})
    updated(.t.)
   else
    set relation to
    poprec:=0
    dania->(dbgoto(r))
   endif
   go oldrec
return
*********
static PROC mDOK5(_F)
local totrec
#ifdef A_LPNUM
  setkey(402,NIL)
#endif
  if (updated() .or. oldrec#0) .and. !(_fnowy .and. (_fkey=27 .or. empty(dan)))
    changed:=.t.
    if empty(dan)
#ifdef A_DODATKI
       totrec:=hb_fieldlen('pozycja')
       select ZAPOT
       seek keyp
       dbeval({||FIELD->danie:=MENU->danie},{||len(trim(danie))=totrec.and.danie=menu->pozycja.and.reclock()},{||dtos(data)+posilek=keyp})
       seek keyp
       dbeval({||FIELD->danie:=str(val(danie)-1,totrec)},{||len(trim(danie))=totrec.and.danie>menu->pozycja.and.reclock()},{||dtos(data)+posilek=keyp})
       UNLOCK
       select MENU
#endif
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
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek) for reclock()
#else
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek)
#endif
          go totrec
       endif
#endif
       unlock
       return
    endif
    if _fnowy
      append blank
      DATA:=RELEWY->DATA
      posilek:=RELEWY->posilek
      _fnowy:=.f.
#ifdef A_LPNUM
      pozycja:=str(_fi,hb_fieldlen('pozycja'))
#endif
    endif
    danie:=dania->danie
    dieta:=DIE
#ifdef A_DODATKI
    ile_pos:=ilp
#endif
    if dania->posilek#pg
      lock in dania
      DANIA->posilek:=pg
      unlock in dania
    endif

  elseif oldrec#0 .and. _fkey#27
    _fkey:=13
    _fnowy:=.f.
  ENDIF
  unlock
return
**********
static function showgram(_f)

@ _fk,60 - A_DILTH SAY dania->gramatura color _sbkgr
SAYL dania->jedn color _sbkgr
#ifdef A_DODATKI
if ilp=0
   @ _fk, 75 SAY ' ' color _sbkgr
   return .t.
endif
@ _fk, 75 BOX '■' UNICODE color _sbkgr
#else
  @ _fk, 70 SAY ipcalc(die) PICTURE D_ILPIC COLOR _sbkgr
#endif

return .t.
***************************************
func dand(d,di,da,po)
local a,b,c,i,j,k
  d:=trim(d)
  di:=trim(di)
  if empty(da) .or. empty(po)
#ifdef A_GREX
    i:=at('/',d)
    j:=at('/',di)
    if i<=2 .and. len(d)<=i+1
     return dind(d,di)
    elseif j<=2 .and. len(di)<=j+1
     return dind(di,d)
    endif
    b:=len(diety)
    a:=array(b*len(grupy))
    c:=ascan(a,{|x,y|x:=len(diety),x:=left(diety[(y-1)%b+1],1)+'/'+left(grupy[int((y-1)/b)+1],1),dind(x,d).and.dind(x,di)})<>0
  else
    b:={select(),RELEWY->(recno())}
    select RELEWY
    set order to 1
    set filter to
    set relation to
    dbseek(dseek(,'data,posilek,dieta',da,po,'0'))
    c:=.f.
    dbeval({||c:=.t.},{||ile_pos<>0 .and. dieta>='0' .and. SubStr(dieta,2,1)='/' .and. SubStr(dieta,3,1)>='0' .and. empty(SubStr(dieta,4)) .and. dind(dieta,d) .and. dind(dieta,di)},{||!c .and. data=da .and. posilek=po})
#else
    if len(d)<=1
     return dind(d,di)
    elseif len(di)<=1
     return dind(di,d)
    endif
    c:=ascan(diety,{|x|x:=left(x,1),dind(x,d).and.dind(x,di)})<>0
  else
    b:={select(),RELEWY->(recno())}
    select RELEWY
    set order to 1
    set filter to
    set relation to
    dbseek(dseek(,'data,posilek,dieta',da,po,'0'))
    c:=.f.
    dbeval({||c:=.t.},{||ile_pos<>0 .and. dieta>='0' .and. empty(SubStr(dieta,2)) .and. dind(dieta,d) .and. dind(dieta,di)},{||!c .and. data=da .and. posilek=po})
#endif
    dbgoto(b[2])
    dbselectarea(b[1])
  endif
return c
***************************************
func dind(d,di)
#ifdef A_GREX
local g:='',gi:='',i,j,a

d:=strtran(d,' ')
i:=at('/',d)
if i>0
  g:=SubStr(d,i+1)
  d:=left(d,i-1)
  if !empty(g) .and. (g<'0' .or. len(g)>1)
    return .f. //dind bez weryfikacji po wszystkich dietach
  endif
endif
#endif

if !empty(d) .and. (d<'0' .or. len(d)>1)
   return .f.
endif

di:=strtran(di,' ')
if empty(di)
   return .t.
endif


#ifdef A_GREX
a:=getlines(di,',')
for j:=1 to len(a)
  di:=a[j]
  i:=at('/',di)
  if '/'$di
    gi:=SubStr(di,i+1)
    di:=left(di,i-1)
    if ! (''=gi .or. XOR(gi="-",g$gi))
      loop
    endif
  endif
  if ''=di .or. XOR(di="-",d$di)
     return .t.
  endif
next j
return .f.
#else
return XOR(di="-",d$di)
#endif
***************************************
func dival(get,d)
local r:=.t.,l,c,e,g,f,a,b
if d=NIL
   d:=get:buffer
endif
if empty(d)
   return .t.
endif
e:=len(d)
d:=UpP(strtran(d,' '))
g:=diety
f:=.f.
a:=''
b:=1
for l:=1 to len(d)
    c:=SubStr(d,l,1)
#ifdef A_GREX
    if c=','
       if l=b
          d:=stuff(d,l,1,'')
          --l
       else
         f:=.f.
         g:=diety
         a:=''
         b:=l+1
       endif
       loop
    endif
    if c='/'
       if f
          d:=stuff(d,l,1,'')
          --l
       else
          f:=.t.
          g:=grupy
          a:=''
          b:=l+1
       endif
       loop
    endif
#endif
    if c$'+-'
       if l>b
         d:=stuff(d,l,1,'')
         --l
       endif
       loop
    endif
    if c$a
       d:=stuff(d,l,1,'')
       --l
       loop
    endif
    if r:=aczojs(g,@c,,,"Zamiast "+c)
      d:=stuff(d,l,1,c)
      a+=c
    else
      exit
    endif
next
if right(d,1)$'+-/,'
   d:=left(d,len(d)-1)
endif

d:=pad(d,e)
if r .and. get#NIL .and. !get:buffer==d
  get:buffer:=d
  get:ASSIGN()
  updated(@r)
endif
return r
#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif
***************************************
static function danval(_f,getlist)

field danie,posilek,DATA,nazwa,gramatura,jedn,dieta

LOCAL DAC,ZNALAZ,recme,recr,recd,_s
  dan:=pad(dan,dania->(hb_fieldlen('nazwa')))
  if empty(dan) .or. dan=dania->nazwa
     return .t.
  endif
  recme=recno()
  recr:=relewy->(recno())
  select dania
 set order to tag dan_naz
 recd:=recno()
 ZNALAZ:=dbseek(dseek(,'posilek,nazwa',pg,dan))

 if !ZNALAZ
    _s:={0,min(maxcol()-60,col()),maxrow(),,1,len(trim(dan))+1,;
         'Danie',{||posilek+"/"+left(nazwa,40)+if(""=opis,if(sklad->(dbseek(dania->danie)),I,"!"),"&")+left(dieta,A_DILTH)+I+gramatura+" "+jedn},;
         {|k,s D_MYSZ|danszuk(k,s,.t. D_MYSZ)},trim(dseek(,'posilek,nazwa',pg,dan))}
    if ordnumber('dan_uni')>0 .and. UpP(nazwa)<>UpP(trim(dan))
       set order to tag dan_uni
       if dbseek(UpP(trim(dan)))
         _slth:=0
         _spocz:=''
       else
         dbgoto(recd)
       endif
       set order to tag dan_naz
    else
       dbgoto(recd)
    endif
    ZNALAZ:=szukam(_s)
 ENDIF
    SET ORDER TO tag dan_kod
    SET RELATION TO
    select relewy
    go recr
  select zapot
    SET ORDER TO tag zap_rel
    SET RELATION TO
    SELECT Menu
    SET ORDER TO tag menu_rel
    SET RELATION TO
    go recme

  IF ZNALAZ
    if _fnowy .and. oldrec#0 .and. dania->(recno())#recd
       poprec:=startrec
    endif
    dan:=dania->nazwa
    if die=" "
       die:=dania->dieta
       getlist[2]:display()
    endif
    updated(.t.)
    RETURN .T.
   ELSE
    dania->(dbgoto(recd))
  ENDIF
RETURN .F.
******************
func dan_kat(upden)
DEFAULT upden TO .t.
//  private kon:=" "
  select sklad
  SET ORDER TO tag skl_dan
  select dania
 set order to tag dan_naz
return  szukam({0,maxcol()/2-30,maxrow(),,1,0,;
     'Danie',{||posilek+"/"+left(nazwa,40)+if(""=opis,if(sklad->(dbseek(dania->danie)),I,"!"),"&")+left(dieta,A_DILTH)+I+gramatura+" "+jedn},;
    {|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),danszuk(_skey,_s,upden D_MYSZ)},""})
******************
FUNCTION danszuk(_skey,_s,upden D_MYSZ)
local o
//memvar changed

DO CASE
  CASE _skey=0 .and. alias()="DANIA"
    _spform:={|p|tranr(p,"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    if [at(] $ lower(INDEXKEY(0))
       _sp2s:={|x|dseek(,'posilek,nazwa',left(x,1),SubStr(x,2))}
       _ss2p:={|x|if(len(x)>0,SubStr(posstr,asc(x)%16,1)+SubStr(x,2),"")}
       _spform:={|p,l|tranr(eval(_ss2p,p,l),"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    endif
    if _slth>0
    _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
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
    endif
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
        _sret:=.f.
        return .t.


   CASE _skey=2 .AND. _sbeg=1 // ^>
    if empty(ordnumber('dan_uni'))
       return .f.
    endif

    go _srec[_sm]
    SET ORDER TO tag dan_uni
    _sbeg:=3
    _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _spocz:=SubStr(_spocz,2)
    _slth:=len(_spocz)
    _spform:={|p|p}
    _sp2s:=NIL
    _ss2p:={|x|x}
    refresh(1,_s)

   CASE _skey=26 .AND. _sbeg#1 // ^<
    if empty(ordnumber('dan_uni'))
       return .f.
    endif

    go _srec[_sm]
    SET ORDER TO tag dan_naz
    _slth+=1
    _sbeg:=1
    _spocz:=dseek(,'nazwa',_spocz)
    _swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _spform:={|p|tranr(p,"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    if [at(] $ lower(INDEXKEY(0))
       _sp2s:={|x|dseek(,'posilek,nazwa',left(x,1),SubStr(x,2))}
       _ss2p:={|x|if(len(x)>0,SubStr(posstr,asc(x)%16,1)+SubStr(x,2),"")}
       _spform:={|p,l|tranr(Eval(_ss2p,p,l),"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    endif
    refresh(1,_s)

   case _skey=43
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
      go _srec[_sm]
      
    private changed:=.f.

    _skey:=rec_in(!upden,.t.)

    if changed==.t.
      if alias()='DANIA'
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

   CASE _skey=22 // ins
      
    private changed:=.f.

    _skey:=rec_in(!upden,.f.)

    if changed==.t.
      if alias()='DANIA'
         go _skey
      endif
      if ! EvaldB(_swar,_spocz)
        if _slth=0
           dbseek(_spocz)
        else
          _spocz:=LEFT(_spocz,len(_spocz)-_slth)
          _slth:=0
        endif
      endif
      refresh(,_s)
    elseif upden
      go _srec[_sm]
      REFRESH LINE _srow1-1+_sm DIRECTION 0
    endif

   CASE _si=0

   CASE _SKEY=13
    RETURN(_sret:=.T.)

   case _skey=9

    PRIVATE _sramka
    _sramka="GR+"
    _skey:=push_stat()
      SELECT Menu
      SET ORDER TO tag menu_dan
        set relation to RELEWY->(dseek(,'data,posilek,dieta',MENU->data,MENU->posilek,'')) into relewy
        SEEK dania->(danie)

szukam({1,min(col()+5,maxcol()-30),maxrow(),,1,6,"DATA",;
{||tran(Dtos(data)+posilek+dieta,hb_UTF8ToStr("@R ####.##.##│X│XXXX"))},{|k,s|RELe(k,s,upden)},dania->danie+left(dtos(mies_rob),6)})
    pop_stat(_skey)

   CASE _SKEY=-7 .AND. 1=alarm(hb_UTF8ToStr("CZY DRUKOWAĆ RECEPTURĘ"),{"TAK","NIE"},1,2)
       _skey:=push_stat()
       WYDRUK_REC()
       pop_stat(_skey)

   case _skey=-8
      _sfil(_s)
      
   case _skey=-9
      _slist(".\"+left(alias(),2)+"*.frm",_s)

ENDCASE

RETURN(.F.)
**************************
