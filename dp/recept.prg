#define I hb_UTF8ToStr("│")
#include "dm_form.ch"

static poprec,oldrec,keyp,startrec,curprompt,nowy,pg,na,gra,jed,die,op,il,di

MEMVAR CHANGED,diety,posilki,grupy,mies_rob

field posilek,danie,ilosc,gramatura,jedn,skladnik,nazwa,indx_mat,przel,dieta,gram,data,opis,jmaG,cena,pozycja

#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif
#ifdef __HARBOUR__
#define D_REST 4
#else
#define D_REST 2
#endif
*****************
func rec_in(deep,n)

local stat,r,vars:={poprec,oldrec,keyp,startrec,curprompt,nowy,pg,na,gra,jed,die,op,il,di}
if keyp#NIL
   deep=.t.
endif
poprec:=startrec:=oldrec:=0
keyp:=danie
nowy:=n .or. eof()
stat:=push_stat()

begin sequence

  select menu
      SET ORDER TO tag menu_dan

  select surowce
      SET ORDER TO tag sur_kod

  select dania
      SET ORDER TO tag dan_kod

  dbseek(keyp,.f.)

  curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)

  pg:=posilek

  select sklad
    SET ORDER TO tag skl_dan

 if dbseek(keyp)
    kibord(chr(3))
 endif

select dania

if deep
   inkey()
   if !eof()
    FORM_EDIT({20,61+A_DILTH,4,1,999,;
{|f|RDOK1(f)},;
{|f|rdok2(f,{})},;
{||setcursor(0)},;
{||DBSELECTAREA("SKLAD"),ordsetfocus("skl_dan")},;
{|f|Rdok4(f,{},deep)},;
{|f|dok6(f,-4)}})
    endif
else
    lock
    FORM_EDIT({20,61+A_DILTH,4,1,999,;
{|f|RDOK1(f)},;
{|f,g|rdok2(f,g)},;
{|f,g|rdok3(f,g)},;
{||DBSELECTAREA("SKLAD"),ordsetfocus("skl_dan")},;
{|f,g|Rdok4(f,g,deep)},;
{|f|Rdok5(f)}})
endif

end sequence

select dania
r:=recno()
unlock

pop_stat(stat)
poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]
curprompt:=vars[5]
nowy:=vars[6]
pg:=vars[7]
na:=vars[8]
gra:=vars[9]
jed:=vars[10]
die:=vars[11]
op:=vars[12]
il:=vars[13]
di:=vars[14]

RETURN r
**************
stat PROC RDOK1(_f)
  SET CURSOR OFF
  SET COLOR TO (_SBKGR)
  @ 0,_fco1,3,_fco2 BOX UNICODE '╔═╗║║ ║║ '
  @ 4,_fco1,6,_fco2 BOX UNICODE '╠═╣║╝═╚║ '
  @ 4,_fco1+3 say 'F9'
  @ 0,_fco1+1 SAY "DANIE - NAZWA - SKŁADNIKI" UNICODE
  @ 2,_fco1+10 say "Gramatura:"
  @ 2,_fco1+31 say "Dieta:"
  @ 4,_fco1+6 say 'Składnik' UNICODE
  @ 4,_fco1+26 say 'Ilość' UNICODE
  @ 4,_fco2-6 say 'Dieta'
RETURN
***********
stat proc rdok2(_f,getlist)
  memvar posgr
  local now,apg

  select dania
  na:=nazwa
  gra:=gramatura
  jed:=jedn
  die:=dieta
  op:=if(""=opis,"bez uwag",opis)
  now:=if(nowy,"NOWY   ","POPRAWA")
#ifdef A_WAGI
#define posIlki apg
  apg:={}
  aeval(posilki,{|x,y|if(left(x,1)$posgr,aadd(apg,x),)})
#endif
  @  1,_fco1+1 get pg valid aczojs(posIlki)
#ifdef posIlki
  #undef posIlki
#endif
  @  1,_fco1+3 get NA picture "@KS"+ltrim(str(_fco2-_fco1-4))
  @  2,_fco1+2 get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  @  2,_fco1+21 get gra picture "@K" valid {||gra:=padl(trim(gra),4),.t.}
  @  2,_fco1+26 get jed picture "@K"
  @  2,_fco1+38 get die picture "@KS"+ltrim(str(A_DILTH)) VALID {|g|dival(g)}
  @  3,_fco1+2  get op picture "@KS"+ltrim(str(_fco2-_fco1-3)) send cargo:=.t.
  __setproc(procname(0))
return
************
stat proc rdok3(_f)
  local dan
  if updated()
      keyp:=danie
      if sklad->(dbseek(keyp))
         startrec:=sklad->(recno())
         curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)
      else
         startrec:=0
      endif
      changed:=.t.
      _fj=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag dan_naz
      if dbseek(dseek(,'posilek,nazwa',pg,na)).and.gra+jed+die=gramatura+jedn+dieta .OR. empty(NA)
        @ 5,_fco1,6,_fco2 BOX UNICODE '║ ║║╝─╚║ ' color _sbkgr
        RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow)*D_REST+1))
        _fpopkey:=.f.
        return
      endif
      @ 5,_fco1,6,_fco2 bOX UNICODE '║ ║║╝═╚║ ' color _sbkgr
      _fpopkey:=.t.
      SET ORDER TO tag dan_kod
#ifdef A_LPNUM
      go bottom
      dan:=str(val(danie)+1,hb_fieldlen('danie'))
#else
      GO lastrec()
      dan:=if(eof(),chr(0)+chr(0),i2bin(bin2w(DANIE)+1))
#endif
      APPEND BLANK
      DANIE:=DAN
#ifdef A_DEMO
      poprec:=0
#else
      poprec:=startrec
#endif
    elseif empty(na)
      select menu
      set order to tag menu_dan
      if !dbseek(dania->danie)
        if 1=alarm(hb_UTF8ToStr("Czy skasować danie ?"),{"Tak","Nie"},2)
           lock
           select sklad
           set order to tag skl_dan
           dbseek(dania->danie)
#ifdef A_LAN
           delete while danie==dania->danie rest for reclock()
#else
           delete while danie==dania->danie rest
#endif
           unlock
           select dania
           delete
           commit
        endif
      else
        alarm(hb_UTF8ToStr("Występuje w jadłospisie dnia "+dtoc(menu->data)+";nie można wykasować."),,,3)
      endif
      select dania
      break
    else
      lock
    endif
    NAZWA:=NA
    posilek:=pg
    dieta:=die
    jedn:=jed
    gramatura:=gra
    op:=trim(op)
    opis:=if("bez uwag"=op,"",op)
  endif

return
***********
stat proc RDOK4(_f,getlist,deep)
  static fpstart:=0
  _fnowy:=!danie==dania->danie
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
     na:=space(surowce->(hb_fieldlen('nazwa')))
     di:=space(hb_fieldlen('dieta'))
     _fpos:=1
   else
      il:=ilosc
      di:=dieta
      surowce->(dbseek(sklad->skladnik,.f.))
      na:=surowce->nazwa
      showzaw(_f)
    if oldrec#0
       startrec:=poprec
       skip
       poprec:=if(danie==keyp,recno(),0)
       go oldrec
    endif
    endif

    @ _fk,_fco1+6 GET na PICTURE "@KS20" VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=surval(_f,getlist,@na,@poprec,oldrec,startrec,.f.) .and. showzaw(_f),setkey(-8,k),r}
    GETL il picture "####.##" valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),showzaw(_f)}
    @ _fk,_fco2-A_DILTH-1 GET di PICTURE "@KS"+ltrim(str(A_DILTH)) valid {|g|if(g:changed.and.fpstart=0,fpstart:=3,),dival(g)}
     __setproc(procname(0))
  //if startrec#0
     aeval(getlist,{|g|g:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}})
  //endif
  fpstart:=0
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||danie==dania->danie}))})
#endif
RETURN
********
stat proc f9(g,_f,getlist)
local r:=surowce->(recno()),s:=dania->(recno())
   oldrec:=recno()

   if startrec=0
      poprec:=0
      if dan_kat(.f.)
         SELECT DANIA
         set order to tag dan_kod
         keyp:=danie
         curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)
         SELECT SKLAD
         seek keyp
         startrec:=recno()
         dania->(dbgoto(s))
      else
         select dania
         set order to tag dan_kod
         go s
         select surowce
         go r
         select sklad
         go oldrec
         return
      endif
   endif

   set relation to skladnik into surowce
   go if(poprec=0,startrec,poprec)
    if szukam({2,min(col(),maxcol()-60),maxrow(),,0,0,curprompt,;
     {||surowce->nazwa+I+str(ilosc)+" "+surowce->jedN},;
     {|k,_s D_MYSZ|(_sret:=k=13).or.sur(k,_s,.f. D_MYSZ)},keyp})
    set relation to
    poprec:=recno()
    g:killfocus()
    il:=ilosc
    di:=dieta
    na:=surowce->nazwa
    g:setfocus()
    showzaw(_f)
    getlist[2]:display()
    startrec:=poprec
    skip
    poprec:=if(danie==keyp,recno(),0)
    updated(g:changed:=.t.)
   else
    set relation to
    surowce->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
RETURN
*********
stat PROC RDOK5(_f)
local totrec
    if (oldrec#0 .or. updated()) .and. !((il=0 .or. _fkey=27).and. _fnowy)
      changed:=.t.

        if _fnowy
          _fnowy:=.f.
          append blank
          DANIE:=DANIA->DANIE
#ifdef A_LPNUM
          pozycja:=str(_fi,hb_fieldlen('pozycja'))
#endif
        endif

     SKLADNIK:=if(empty(na),'',SUROWCE->SKLADNIK)
     ILOSC:=IL
     DIETA:=di
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
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DANIE=DANIA->DANIE for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DANIE=DANIA->DANIE
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
***************
stat func showzaw(_f)
  @ _fk,_fco1+35 say surowce->jedN color _sbkgr
return .t.
*****************
