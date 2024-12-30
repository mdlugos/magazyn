#define I hb_UTF8ToStr("│")
#ifndef MAG_BIEZ
 #define MAG_BIEZ " 1"
#endif
#ifdef A_GOCZ
 #ifndef A_ZAP_DAN
  #define A_ZAP_DAN
 #endif
#endif
#include "dm_form.ch"
#include "inkey.ch"
#include "getexit.ch"
#ifdef A_POLOWA
  #define D_ILPIC "@KZE ###.#"
#else
  #define D_ILPIC "@KZ #####"
#endif

#command REPLACE wartosc WITH <x> => field2bin([d_wartosc],<x>)
#define WARTOSC bin2d(field->d_wartosc)

field data,posilek,nazwa,d_wartosc,ile_pos,skladnik,ilosc,danie,dieta,pozycja,jedn,gramatura,kod

MEMVAR CHANGED,diety,posilki,grupy,posstr,defa
static poprec,oldrec,keyp,startrec
static aZap,apos,adat,aflag
static na,il,di,dan,chg

*********************
func zap_in(deep)

LOCAL stat,p,r,vars:={poprec,oldrec,keyp,startrec,aZap,apos,adat,aflag,na,il,di,dan,chg},d,e,f,g
if keyp#NIL
   deep=.t.
endif
poprec:=startrec:=oldrec:=0
//keyp:=dtos(data)+posilek
keyp:={data,posilek}
adat:=""
aflag:=.f.
chg:=NIL

stat:=push_stat()
begin sequence
#ifdef A_ELZ
  select cennik
      set order to 1
#endif
  select dania
      SET ORDER TO tag dan_kod
      r:=hb_fieldlen([nazwa])
  select sklad
      SET ORDER TO tag skl_dan
  select menu
      SET ORDER TO tag menu_rel
   select surowce
      set order to tag sur_kod
      r+=hb_fieldlen([nazwa])
   select zapot
      set order to tag zap_rel
   SELECT RELEWY
  dbseek(dseek(,'data,posilek,dieta',keyp[1],keyp[2],''),.f.)
  keyp:=dtos(keyp[1])+keyp[2]
  select zapot
 #ifdef A_LPNUM      
  if dbseek(keyp,,.t.)
    p:=-val(pozycja)
  endif
 #endif
 
 if dbseek(keyp)
    kibord(chr(3))
 endif

  SELECT RELEWY

  if deep
  inkey()
  aZap:={{},{},{}};apos:=1
    if !eof()
    FORM_EDIT({p,-r-42-A_DILTH,3,1,999,;
{|f|zDOK1(f)},;
{|f|zdok2(f,{})},;
{|f|setcursor(0)},;
{|_f|DBSELECTAREA("ZAPOT"),ordsetfocus("zap_rel"),dbseek(keyp+str(_fi,hb_FieldLen([pozycja])))},;
{|f|zdok4(f,{},deep)},;
{|f|dok6(f,-4)}})
    endif
else
    lock
    FORM_EDIT({p,-r-42-A_DILTH,3,1,999,;
{|f|zDOK1(f)},;
{|f,g|zdok2(f,g)},;
{|f,g|zdok3(f,g,@poprec,@keyp,@startrec)},;
{|_f|DBSELECTAREA("ZAPOT"),ordsetfocus("zap_rel"),dbseek(keyp+str(_fi,hb_FieldLen([pozycja])))},;
{|f,g|zdok4(f,g,deep)},;
{|f|zdok5(f)}})
/*
#ifdef A_ELZ
  if chg#NIL
     r:=recno()
     d:={};e:={}
     select zapot
     set order to tag zap_rel
     seek chg
     exec {|a|cennik->(dbseek(zapot->skladnik+left(chg,8)),if(skladnik=zapot->skladnik,,dbgoto(lastrec()+1))),a:=ascan(d,dieta),if(a=0,(aadd(d,dieta),aadd(e,ilosc*cennik->cena)),e[a]+=ilosc*cennik->cena)} rest while dtos(data)+posilek=chg
     set relation to
     select relewy
     seek dseek(,'data,posilek,dieta',stod(left(chg,8)),SubStr(chg,9,1),SubStr(chg,10))
#ifdef A_LAN
     #define D_LAN .and. reclock()
#else
     #define D_LAN
#endif
     exec {|a|a:=ascan(d,dieta),f:=if(a=0,0,e[a]),g:=left(dieta,1),if(g<'0',if(a=0,,e[a]:=e[a]/ile_pos),aeval(d,{|x,i|if(x<'0'.and.dind(g,x),f+=ile_pos*e[i],)})),relewy->(field2bin([d_wartosc],f))} rest while dtos(data)+posilek=chg D_LAN
     goto r
  endif
#endif
*/
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
aZap:=vars[5]
apos:=vars[6]
adat:=vars[7]
aflag:=vars[8]
na:=vars[9]
il:=vars[10]
di:=vars[11]
dan:=vars[12]
chg:=vars[13]
RETURN r
**********************************************
static procedure zdok1(_f)

   set cursor off
   SET COLOR TO (_sbkgr)
  @ _fscr0+0,_fco1,_fscr0+3,_fco2 BOX UNICODE '╔═╗║║ ║║ '
  @ _fscr0+0,_fco1+1 SAY "ZAPOTRZEBOWANIE NA PRODUKTY"
  @ _fscr0+1,_fco1+2 say "Ilość" UNICODE
  @ _fscr0+1,_fco1+9 say "Cena"
  @ _fscr0+1,_fco1+20 say "Data"
  @ _fscr0+1,_fco1+25 say "Pos."
  @ _fscr0+1,_fco1+41 say "Jadłospis" UNICODE
  @ _fscr0+3,_fco1,_fscr0+5,_fco2 BOX UNICODE '╠═╣║╝═╚║ '
  @ _fscr0+3,_fco2-48 say 'Surowiec═════════Dieta════Ilość════════Gram═Ilp' UNICODE
return
***********
static proc zdok2(_f,getlist)
  local da,kon
  select relewy
  if eof()
     da:=DatE()
     kon:=left(posilki[1],1)
  else
     da:=data
     kon:=posilek
  endif
  @ _fscr0+2,_fco1+29 say posilki[max(1,ascan(posilki,kon))] COLOR _sbkgr
  @ _fscr0+2,_fco1+1 say ile_pos picture D_ILPIC color _sbkgr
  @ _fscr0+2,_fco1+7 say strpic(WARTOSC/ile_pos,7,A_ZAOKR,"@E ",.t.) color _sbkgr
  @ _fscr0+2,_fco1+41 say if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak") color _sbkgr
  @ _fscr0+2,_fco1+18 get da  picture "@D" valid {||setpos(_fscr0+2,_fco1+41),devout(if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),.t.}
  @ _fscr0+2,_fco1+29 get kon valid {|y|y:=aczojs(posilki),devout(posilki[max(1,ascan(posilki,kon))],_sbkgr),setpos(_fscr0+2,_fco1+41),devout(if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),y}
  __setproc(procname(0))
return
*************
proc mkdarr(f,da,po)
local rec, s:=select(),a
  select relewy
  rec:=recno()
  seek dseek(,'data,posilek,dieta',da,po,'0')
  if valtype(f)<>'A'
     f:={}
  endif
#ifdef A_GREX
  EXEC aadd(f,{left(dieta,1),SubStr(dieta,3,1),,left(dieta,3),ile_pos}) WHILE data=da .and. posilek=po FOR ile_pos<>0 .and. SubStr(dieta,2)='/' .and. SubStr(dieta,3)>'0' .and. empty(SubStr(dieta,4))
#else
  EXEC {|x|x:=left(dieta,1),aadd(f,{x,ascan(diety,x),,x,ile_pos})} WHILE data=da .and. posilek=po FOR ile_pos<>0 .and. empty(SubStr(dieta,2))
  asort(f,,,{|x,y|x[2]<y[2]})
#endif
  go rec
  select (s)

return
*************
func diand(d1,d2,f,da,po,il)
 if empty(f) .and. pcount()>=5
    mkdarr(@f,da,po)
 endif
 il:=0
 aeval(f,{|x|x[3]:=dind(x[4],d1) .and. dind(x[4],d2),if(x[3],il+=x[5],)})
 if empty(d2)
    return d1
 elseif empty(d1)
    return d2
 endif
return trimd(f)
********************
func dieq(d1,d2,f)
 local l:=.t.
 aeval(f,{|x|l := l .and. dind(x[4],d1) == dind(x[4],d2)})
return l
**********************
func dior(d1,d2,f)
 if empty(d1) .or. empty(d2)
   return space(relewy->(hb_fieldlen('dieta')))
 endif
 aeval(f,{|x|x[3]:=dind(x[4],d1) .or. dind(x[4],d2)})
return trimd(f)
*************
func trimd(f)
local c,d
#ifdef A_GREX
local i,j,k,l,a,b,g:='',h:=''
   d:={}
   aeval(f,{|x|if(x[3],;
               aadd(d,{x[1],x[2],,}),;
             ),;
             if(x[1]$g,,;
                g+=x[1];
             ),;
             if(x[2]$h,,;
                h+=x[2];
             ) })
   if empty(d)
      return pad(HB_UCHAR(0x0A0),relewy->(hb_fieldlen('dieta')))
   endif
   if len(d)=len(f)
      return space(relewy->(hb_fieldlen('dieta')))
   endif
   c:=''
   for i:=1 to len(d)
       for k:=len(d) to i+1 step -1
           l:=.t.
           a := d[i,1]+d[k,1]+'/'+d[i,2]+d[k,2]
           for j:=1 to len(f)
              l:= l .and. (f[j,3] .or. !dind(f[j,4],a))
           next j
           if l
              d[i,1]+=d[k,1]
              d[i,2]+=d[k,2]
              adel(d,k)
              asize(d,len(d)-1)
           endif
       next k

       //if len(d[i,2])>1
          a:=''
          b:=''
          aeval(grupy,{|x|x:=left(x,1),if(x$h,if(x$d[i,2],a+=x,b+=x),)})
          if len(b)=0
             d[i,2]:=''
          elseif len(a)>len(b)
             d[i,2]:='-'+b
          else
             d[i,2]:=a
          endif
          d[i,4]:=b
       //endif

       //if len(d[i,1])>1
          a:=''
          b:=''
          aeval(diety,{|x|x:=left(x,1),if(x$g,if(x$d[i,1],a+=x,b+=x),)})
          if len(b)=0
             d[i,1]:=''
          elseif len(a)>len(b)
             d[i,1]:='-'+b
          else
             d[i,1]:=a
          endif
          d[i,3]:=b
       //endif

       a:=.t.
       b:=.t.
       for j:=1 to len(f)
          a:= a .and. (f[j,3] .or. !dind(f[j,4],d[i,1]))
          b:= b .and. (f[j,3] .or. !dind(f[j,4],'/'+d[i,2]))
       next j

       if a .and. (!b .or. len(d[i,2])>=len(d[i,1]) )
          d[i,2]:=''
          d[i,4]:=''
       elseif b
          d[i,1]:=''
          d[i,3]:=''
       endif

       if len(d[i,4])>1
          for k:=len(d[i,4]) to 1 step -1
             a:=stuff(d[i,4],k,1,'')
             b:=d[i,1]+'/-'+a
             l:=.t.
             for j:=1 to len(f)
               l:= l .and. (f[j,3] .or. !dind(f[j,4],b))
             next j
             if l
               d[i,4]:=a
             endif
          next k
          if d[i,2]='-' .or. len(d[i,2])>len(d[i,4])+1
             d[i,2]:='-'+d[i,4]
          endif
       endif

       if len(d[i,3])>1
          for k:=len(d[i,3]) to 1 step -1
             a:=stuff(d[i,3],k,1,'')
             b:='-'+a+'/'+d[i,2]
             l:=.t.
             for j:=1 to len(f)
               l:= l .and. (f[j,3] .or. !dind(f[j,4],b))
             next j
             if l
               d[i,3]:=a
             endif
          next k
          if d[i,1]='-' .or. len(d[i,1])>len(d[i,3])+1
             d[i,1]:='-'+d[i,3]
          endif
       endif

       c+=','+d[i,1]
       if !empty(d[i,2])
          c+='/'+d[i,2]
       endif
   next i
   d:=SubStr(c,2)
#else
   c:=''
   d:=''
   aeval(f,{|x|if(x[3],d+=x[1],c+=x[1])})
   if empty(d)
      return pad(HB_UCHAR(0x0A0),relewy->(hb_fieldlen('dieta')))
   endif
   if empty(c)
      return space(relewy->(hb_fieldlen('dieta')))
   endif
   if len(d)>len(c)
      d:='-'+c
   endif
#endif

return pad(d,relewy->(hb_fieldlen('dieta')))
***************
func mk_azap(aDat)
local aZap:={{},{},{}},rec,mes,d,e,f:={},i,j
#ifdef A_DODATKI
  if menu->(dbseek(adat)) .and. (menu->ile_pos<>0 .or. relewy->ile_pos<>0)
#else
  if ile_pos#0 .and. menu->(dbseek(adat))
#endif
     mes:=message(hb_UTF8ToStr("Chwileczkę "))
     rec:=recno()
     select sklad
      SET ORDER TO tag skl_dan
      SET RELATION TO skladnik into surowce
     select menu
      SET ORDER TO tag menu_rel
      SET RELATION TO
      go recno()
     do while DTOS(data)+posilek=adat
        select sklad
        if dbseek(menu->danie)
           dania->(dbseek(menu->danie,.f.))
           do while danie==menu->danie
              d:=diand(menu->dieta,sklad->dieta,f,stod(left(adat,8)),SubStr(adat,9,1),@e)
#ifdef A_DODATKI
              if menu->ile_pos<>0
                 e:=menu->ile_pos  //ipcalc(d,menu->danie,menu->ile_pos)
              endif
#define A_ZAP_DAN
#endif
           if e#0

              message(10)
#ifdef A_ZAP_DAN
                 aadd(aZap[1],{surowce->(recno()),d,dania->(recno()),menu->(recno())})
                 aadd(aZap[2],{ilosc,e})
                 aadd(aZap[3],trim(surowce->nazwa)+" ("+left(dania->nazwa,at(" ",dania->nazwa)-1)+") "+hb_ntoc(e*ilosc/surowce->przel,3)+" "+trim(surowce->jmaG))
#else
#ifdef A_DISUM
              i:=ascan(aZap[1],surowce->(recno()))
              if i<>0 .and. round(azap[2,i,1]-ilosc,3)=0 .and. diand(d,SubStr(azap[1,i],3),f)=HB_UCHAR(0x0A0)
                 aZap[1,i,2]:=dior(d,azap[1,i,2],f)
                 aZap[2,i,2]+=e
                 aZap[3,i]+=",("+left(dania->nazwa,at(" ",dania->nazwa)-1)+") "+hb_ntoc(e*ilosc/surowce->przel,3)+" "+trim(surowce->jmaG)
              else
#endif
                 aadd(aZap[1],{surowce->(recno()),d})
                 aadd(aZap[2],{ilosc,e})          //e*ilosc/surowce->przel)
                 aadd(aZap[3],trim(surowce->nazwa)+" ("+left(dania->nazwa,at(" ",dania->nazwa)-1)+") "+hb_ntoc(e*ilosc/surowce->przel,3)+" "+trim(surowce->jmaG))
#ifdef A_DISUM
            endif
#endif


#endif
           endif
              skip
           enddo
        endif
        select menu
        skip
     enddo
#ifdef A_DISUM
     for i:=1 to len(azap[1])-1
       for j:=len(azap[1]) to i+1 step -1
         if azap[1,i,1]==azap[1,j,1] .and. dieq(azap[1,i,2],azap[1,j,2],f)
            aZap[2,i,1]+=aZap[2,j,1]
            aZap[3,i]+=','+SubStr(azap[3,j],1+at(" (",azap[3,j]))
            adel(azap[1],j)
            adel(azap[2],j)
            adel(azap[3],j)
            asize(azap[1],len(azap[1])-1)
            asize(azap[2],len(azap[2])-1)
            asize(azap[3],len(azap[3])-1)
         endif
       next j
     next i
#endif
     message(mes)
     select sklad
     set relation to
     select relewy
     go rec
     lock
  endif

return aZap
***************
static proc zdok3(_f,getlist,poprec,keyp,startrec)
//static proc zdok3(_f)
  local i,j,k,l,rec,mes,a,b,c,d,e,f:={},g,h
  dok3(_f,getlist,@poprec,@keyp,@startrec)
  @ _fscr0+2,_fco1+7 SAY strpic(WARTOSC/ile_pos,7,A_ZAOKR,"@E ",.t.) color _sbkgr
  if adat#dtos(relewy->data)+relewy->posilek
     adat:=dtos(relewy->data)+relewy->posilek
     azap:=mk_azap(adat)
     //aZap:={{},{},{}}
     apos:=1
     aflag:=.f.
  endif
  if len(azap[1])>0
     @ _fscr0+3,_fco1+15 say 'F8' color _sbkgr
     if zapot->(dtos(data)+posilek)=dtos(data)+posilek
        apos:=len(azap[1])+1
        aflag:=.f.
     else
        poprec:=0
        @ _fscr0+5,_fco1+2 say "══════════════podpowiedź z jadłospisu pod F8" UNICODE color _sbkgr
#ifdef A_DEMO
        aflag:=.f.
#else
        aflag:=.t.
#endif
     endif
  else
     @ _fscr0+3,_fco1+15 box '══' unicode color _sbkgr
     aflag:=.f.
  endif

return
*************
static proc zDOK4(_f,getlist,deep)
  static fpstart:=0,gram:=0,ip:=0
  local g,h
  #ifdef A_ZAP_DAN
  #define D_G +1
  #else
  #define D_G
  #endif
  _fnowy:=DTOS(data)+posilek#relewy->(DTOS(data)+posilek)
  if _fnowy .and. poprec#0
     oldrec:=recno()
     go poprec
     _fpopkey:=.t.
     _fpos:=fpstart
  else
     oldrec:=0
  endif
  setpos(_fk,_fco2-16)
  IF !_fnowy .or. oldrec#0
    surowce->(dbseek(zapot->skladnik,.f.))
#ifdef A_ZAP_DAN
#ifdef A_DODATKI
    if len(trim(danie))=menu->(hb_fieldlen('pozycja'))
       menu->(dbseek(dtos(zapot->data)+zapot->posilek+trim(zapot->danie),.f.))
       dania->(dbseek(menu->danie,.f.))
    else
       dania->(dbseek(zapot->danie,.f.))
       menu->(dbgoto(0))
    endif
#else
    dania->(dbseek(zapot->danie,.f.))
#endif
    dan:=pad(trim(dania->nazwa)+" "+dania->gramatura+" "+dania->jedn,dania->(hb_fieldlen('nazwa')))
#endif
    na:=surowce->nazwa
    il:=ilosc
    di:=dieta
    dispout(surowce->jmaG,_sbkgr)
    if oldrec#0
       startrec:=poprec
       skip
       poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
       go oldrec
    endif
  elseif !_fpopkey .and. _fi>1
     if !deep
        kibord(chr(27))
     endif
     return
  else
    if aflag .and. len(azap[1])>=apos
       oldrec:=recno()
       surowce->(dbgoto(aZap[1,apos,1]))
       di:=aZap[1,apos,2]
#ifdef A_ZAP_DAN
       dania->(dbgoto(aZap[1,apos,3]))
       dan:=pad(trim(dania->nazwa)+" "+dania->gramatura+" "+dania->jedn,dania->(hb_fieldlen('nazwa')))
#ifdef A_DODATKI
       menu->(dbgoto(aZap[1,apos,4]))
       if menu->ile_pos=0
          menu->(dbgoto(0))
       endif
#endif
#endif
       na:=surowce->nazwa
       il:=aZap[2,apos,1]*aZap[2,apos,2]/surowce->przel
       dispout(surowce->jmaG,_sbkgr)
       ++apos
       _fpos:=fpstart
    else
       //surowce->(dbgoto(lastrec()+1))
#ifdef A_ZAP_DAN
       if dan=NIL
          dan:=space(dania->(hb_fieldlen('nazwa')))
       endif
       dania->(dbgoto(0))
#ifdef A_DODATKI
       menu->(dbgoto(0))
#endif
#endif
       na:=space(surowce->(hb_fieldlen('nazwa')))
       il:=0
       di:=space(hb_fieldlen('dieta'))
       _fpos:=1 D_G
    endif
  endif
  ip:=ipcalc(di,dania->danie)
  gram:=il*surowce->przel/ip
  showwar(_f)
  setpos(_fk,_fco1+5)
  h:=surowce->(hb_fieldlen([nazwa]))
#ifdef A_ZAP_DAN
  g:=dania->(hb_fieldlen([nazwa]))
  GETL dan PICTURE "@KS"+hb_ntoc(g*(_fco2-_fco1-42-A_DILTH)/(g+h),0) VALID danval(_f,getlist)
#endif
  GETL na PICTURE "@KS"+hb_ntoc(h*(_fco2-_fco1-42-A_DILTH)/(g+h),0) VALID {|k,r,p|if(k:changed.and.fpstart=0,fpstart:=1 D_G,),p:=setkey(-8,NIL),k:=setkey(-7,NIL),r:=surval(_f,getlist,@na,@poprec,oldrec,startrec,aflag,@apos),setkey(-7,k),setkey(-8,p),if(r,(setpos(_fk,_fco2-16),dispout(surowce->jmaG,_sbkgr),if(ip*gram=0,,il:=ip*gram/surowce->przel),getlist[3 D_G]:display()),),r}
  @ _fk,_fco2-26-A_DILTH GET di picture "@KS"+HB_MACRO2STRING(A_DILTH) valid {|g,r|r:=!g:changed .or. dival(g),if(g:changed .and. r,(ip:=ipcalc(di,dania->danie),setpos(_fk,_fco2 -5 ),dispout(tran(ip,D_ILPIC),_sbkgr),if(gram=0,(gram:=il*surowce->przel/ip,getlist[4 D_G]:display()),(il:=ip*gram/surowce->przel,getlist[3 D_G]:display()))),),showwar(_f),r}
  @ _fk,_fco2-25 GET il PICTURE '@K ####.###' VALID {|k|if(k:changed.and.fpstart=0,fpstart:=3 D_G,),gram:=il*surowce->przel/ip,getlist[4 D_G]:display(),showwar(_f)}
  @ _fk,_fco2-11 get gram PICTURE "###.##" VALID {|k|!k:changed.or.(il:=ip*gram/surowce->przel,getlist[3 D_G]:display(),showwar(_f))}
  @ _fk,_fco2 -5 say ip PICTURE D_ILPIC color _sbkgr
  #undef D_G
  __setproc(procname(0))
  if !empty(azap[1])
     aeval(getlist,{|g|g:reader:={|g|setkey(-8,{|p,g,r|g:=getactive(),P:=setkey(-8,NIL),r:=setkey(-7,NIL),f9(g,_f,getlist,@ip,@gram),setkey(-8,p),setkey(-7,r)}),setkey(-7,{|p,g,r|g:=getactive(),p:=setkey(-7,NIL),r:=setkey(-8,NIL),azapchoice(g,_f,getlist,@ip,@gram),setkey(-7,p),setkey(-8,r)}),getreader(g),setkey(-7,NIL),setkey(-8,NIL)}})
  else
     aeval(getlist,{|g|g:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist,@ip,@gram),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}})
  endif
  g:=getlist[len(getlist)-1]
  h:=g:reader
  g:reader:={|a,d|d:=setkey(61,{||zzaokr(_f,g)}),eval(h,a),setkey(61,d)}
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||dtos(data)+posilek=relewy->(dtos(data)+posilek)}))})
#endif
  fpstart:=0
RETURN
***********************
stat proc zzaokr(_f,g)
local i,f,r:=recno()

   if !empty(na)
      f:=ordsetfocus('ZAP_SKL')
      seek surowce->skladnik+dtos(RELEWY->data)
      SUM ilosc TO i WHILE skladnik=surowce->skladnik .and. data=RELEWY->data
      SET ORDER TO TAG (f)
      GOTO r
      IF !_fnowy
         i-=ilosc
      ENDIF
   endif
   i+=il

for f:=10 to 8 step -1
  r:=SubStr(str(i,10,3),f,1)
  if r='0'
  else
     il+=round(i,f-8)-i
     g:updatebuffer()
     g:changed:=.t.
     getpostvalidate(g)
     Exit
  endif
next f
return
*****************
function ipcalc(di,dan)
local ip:=0,totrec,r:=select(),mo,mr,rl
#ifdef A_DODATKI
  if !empty(dan) .and. menu->danie=dan .and. menu->ile_pos<>0
     return menu->ile_pos
  endif
#endif
  select relewy
  totrec:=recno()
#ifdef A_LAN
  rl:=ascan(dbrlocklist(),totrec)<>0
#endif
  ip:=dicalc(data,posilek,di)
  go totrec
#ifdef A_LAN
  IF rl
     lock
  endif
#endif
  select (r)
return ip
***************************
static function showwar(_f)
static s:='',i:=0

local f,r

   if valtype(_f)='N'
      i+=_f
      return .t.
   elseif empty(na)
      s:=''
      i:=0
   elseif s<>(s:=surowce->skladnik+dtos(RELEWY->data))
         i:=0
         r:=recno()
         f:=ordsetfocus('ZAP_SKL')
         seek surowce->skladnik+dtos(RELEWY->data)
         SUM ilosc TO i WHILE skladnik=surowce->skladnik .and. data=RELEWY->data
         SET ORDER TO TAG (f)
         GOTO r
   endif
   @ _fk,_fco2-35-A_DILTH say i+il-if(_fnowy,0,ilosc) picture "@Z ####.###" color _sbkgr

return .t.
***************************
#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif
#ifdef A_ZAP_DAN
static function danval(_f,getlist)

field danie,posilek,DATA,nazwa,gramatura,jedn,dieta,opis

LOCAL Z,s,pg,_s,r
  if empty(dan)
     dania->(dbgoto(0))
     menu->(dbgoto(0))
     return .t.
  endif
  if dan==pad(dania->(trim(nazwa)+" "+gramatura+" "+jedn),dania->(hb_fieldlen('nazwa')))
     return .t.
  endif
  s:=push_stat()

#ifdef A_DODATKI
  if val(dan)<>0
    select menu
    set order to tag menu_rel
    if keyp#dtos(data)+posilek
       seek keyp
    endif
  
    if len(trim(zapot->danie))=hb_fieldlen('pozycja')
      locate for zapot->danie=pozycja while dtos(data)+posilek=keyp
    else
      locate for zapot->danie=danie while dtos(data)+posilek=keyp
    endif

    if !found()
       seek keyp+str(val(dan),hb_fieldlen('pozycja'))
    endif

    select dania
    set order to tag dan_kod
    r:=min(maxcol()-17-A_DILTH,hb_fieldlen([nazwa]))
    select menu
    set relation to danie into dania

    Z:=szukam({0,,,,,,hb_UTF8ToStr('Jadłospis'),{||pozycja+I+dania->(left(nazwa,r)+I+dieta+I+gramatura+" "+jedn)},,keyp})

  else
#endif

#ifdef A_WAGI
  pg:=SubStr(Memvar->PosGr,at(relewy->posilek,PosStr),1)
#else
  pg:=relewy->posilek
#endif
 select dania
 set order to tag dan_naz
    r:=min(maxcol()-15-A_DILTH,hb_fieldlen([nazwa]))
    _s:={0,,,,1,len(trim(dan))+1,;
         ,{||posilek+I+left(nazwa,r)+if(""=opis,if(sklad->(dbseek(dania->danie)),I,"!"),"&")+dieta+I+gramatura+" "+jedn},;
         {|k,s D_MYSZ|danszuk(k,s,.t.,r D_MYSZ)},trim(dseek(,'posilek,nazwa',pg,dan))}

    if !dbseek(_spocz) .and. ordnumber('dan_uni')>0
       set order to tag dan_uni
       if dbseek(UpP(trim(dan)))
         _slth:=0
         _spocz:=''
       endif
       set order to tag dan_naz
    endif

    Z:=szukam(_s)
 SET ORDER TO tag dan_kod
#ifdef A_DODATKI
 endif
#endif
 SET RELATION TO

  IF Z
    dan:=pad(dania->(trim(nazwa)+" "+gramatura+" "+jedn),dania->(hb_fieldlen('nazwa')))
    z:={dania->(recno()),menu->(recno())}
    pop_stat(s)
    dania->(dbgoto(z[1]))
#ifdef A_DODATKI
    menu->(dbgoto(z[2]))
    if menu->ile_pos=0
       menu->(dbgoto(0))
    endif
#endif
    getlist[3]:display()
    eval(getlist[3]:postblock,getlist[3])
    updated(.t.)
    RETURN .T.
  ENDIF
  pop_stat(s)
RETURN .F.
#endif
********
stat proc f9(g,_f,getlist,ip,gram)
local r:=surowce->(recno()),lr
   oldrec:=recno()
   set relation to skladnik into surowce
   if startrec#0
      go if(poprec=0,startrec,poprec)
   endif
   lr :=min(maxcol()-29-A_DILTH,surowce->(hb_fieldlen([nazwa])))
   if szukam({2,col(),,,1,9,;
     "Zapotrzebowania",;
     {||tran(dtos(data)+posilek,"@R XXXX.XX.XX|X")+I+left(surowce->nazwa,lr)+I+str(ilosc)+" "+surowce->jmaG+I+dieta},;
     {|k,_s|(_sret:=k=13).or.rele(k,_s,.f.)},keyp})
    set relation to
    poprec:=recno()
    g:killfocus()
    na:=surowce->nazwa
    di:=dieta
    il:=ilosc
    gram:=il*surowce->przel/ip
#ifdef A_ZAP_DAN
#ifdef A_DODATKI
    if len(trim(danie))=menu->(hb_fieldlen('pozycja'))
       menu->(dbseek(dtos(zapot->data)+zapot->posilek+trim(zapot->danie),.f.))
       dania->(dbseek(menu->danie,.f.))
    else
       dania->(dbseek(zapot->danie,.f.))
    endif
    menu->(dbgoto(0))
#else
    dania->(dbseek(zapot->danie,.f.))
#endif
    dan:=dania->(trim(nazwa)+" "+gramatura+" "+jedn)
#endif
    ip:=ipcalc(di)
    setpos(_fk,_fco2-5)
    dispout(Tran(ip,D_ILPIC),_sbkgr)
    g:setfocus()
    setpos(_fk,_fco2-16)
    dispout(surowce->jmaG,_sbkgr)
    aeval(getlist,{|g|g:display()})
    startrec:=poprec
    keyp:=dtos(data)+posilek
    skip
    poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
    updated(.t.)
   else
    set relation to
    surowce->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
  RETURN
********
static proc azapchoice(g,_f,getlist,ip,gram)
local pos:=apos
       if aczojs(azap[3],"",@pos,,hb_UTF8ToStr("receptura x ilość osób"))
          apos:=pos
          oldrec:=recno()
          g:killfocus()
          surowce->(dbgoto(aZap[1,apos,1]))
          di:=aZap[1,apos,2]
#ifdef A_ZAP_DAN
          dania->(dbgoto(aZap[1,apos,3]))
          dan:=pad(trim(dania->nazwa)+" "+dania->gramatura+" "+dania->jedn,dania->(hb_fieldlen('nazwa')))
#ifdef A_DODATKI
          menu->(dbgoto(aZap[1,apos,4]))
          if menu->ile_pos=0
            menu->(dbgoto(0))
          endif
#endif
#endif
          ip:=ipcalc(di,dania->danie)
          setpos(_fk,_fco2-5)
          dispout(tran(ip,D_ILPIC),_sbkgr)
          na:=surowce->nazwa
          il:=aZap[2,apos,1]*aZap[2,apos,2]/surowce->przel
          gram:=il*surowce->przel/ip
          g:setfocus()
          ++apos
          updated(aflag:=.t.)
          aeval(getlist,{|g|g:display()})
          setpos(_fk,_fco2-16)
          dispout(surowce->jmaG,_sbkgr)
       endif
return
********
static PROC zDOK5(_F)
  local totrec
#ifdef A_LPNUM
  setkey(402,NIL)
#endif
      if (updated().or. oldrec#0) .and. !(_fnowy .and.(_fkey=27 .or. il=0))
        LOCK
        changed:=.t.
        chg:=adat
        if _fnowy
          _fnowy:=.f.
          append blank
          DATA:=RELEWY->DATA
          posilek:=RELEWY->posilek
#ifdef A_LPNUM
          pozycja:=str(_fi,hb_fieldlen('pozycja'))
#endif
        endif
    if dieta#di .and. di>" "
       select relewy
       totrec:=recno()
       dicalc(data,posilek,di)
       go totrec
       lock
       select zapot
    endif
#ifdef A_ZAP_DAN
        if dan==pad(trim(dania->nazwa)+" "+dania->gramatura+" "+dania->jedn,dania->(hb_fieldlen('nazwa')))
#ifdef A_DODATKI
           if menu->(dtos(data)+posilek)==keyp .and. menu->danie == dania->danie .and. menu->ile_pos<>0
              danie:=menu->pozycja
           else
              danie:=dania->danie
           endif
#else
           danie:=dania->danie
#endif
        endif
#endif
        skladnik:=if(empty(na),"",surowce->skladnik)
        showwar(il-ilosc)
        ilosc:=il
        dieta:=di
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
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek) for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,hb_fieldlen('pozycja')) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek)
          go totrec
#endif
          endif
#endif
        endif
        dbcommit()
      elseif oldrec#0 .and. _fkey#27
        _fkey:=13
        _fnowy:=.f.
      ENDIF
      unlock
RETURN
**********
stat func dicalc(da,kon,di)
local i:=0,w:=0
    if dbseek(dseek(,'data,posilek,dieta',da,kon,di),.f.)
       i:=ile_pos
    else
       aeval(mkfxar(da,kon),{|x|if(dind(x[1],di),(w+=x[2],i+=x[3]),)})
       if i<>0 .or. w<>0
          append blank
          data:=da
          posilek:=kon
          dieta:=di
          ile_pos:=i
          REPLACE wartosc with w
       else
          goto lastrec()+1
       endif
    endif
return i
**********
//#ifndef A_ELZ
proc magazyn
FIELD INDEX,NAZWA,stan,jm,data_przy,waznosc
#ifdef A_KODY
#define D_KODY +I+KOD
#else
#define D_KODY
#endif
        SZUKAM({0,1,MaxRow(),,1,0,hb_UTF8ToStr("PRZEGLĄD MAGAZYNU SPOŻYWCZEGO"),{||INDEX+I+NAZWA D_KODY;
        +hb_UTF8ToStr(IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"°","│"));
        +STR(STANY->STAN)+" "+JM},{|_skey,_s D_MYSZ|if(_skey=13,.f.,STANMAG(_skey,_s D_MYSZ))},"",.F.})
return
*************************
FUNCTION STANMAG(_skey,_s D_MYSZ)
local _stxt
static x
field index,stan,wartosc,zamk_mies1,zamk_mies2,zamkn_roku,;
Wart_mies1,wart_mies2,wart_roku,zapas_min,zapas_max,data_popr,nazwa,jm,;
jm_opcja,przel,lamus,UWAGI,nr_mag,data_zmian,waznosc,data_przy,cena_przy,info,kod,baza  
do case
  case _skey=0
    if select("INDX_MAT")=0
      sel("indeks",,.t.,.t.)
      locate for {||lower(expand(trim(baza)))=="indx_mat"}
      if found()
        sel("INDX_MAT","INDX_NUM",.t.,.t.)
#ifndef STANY
        select indeks
        locate for {||lower(expand(trim(baza)))=="stany"}
        if found()
           sel("STANY","STAN_MAG",.t.,.t.)
        endif
#endif
      endif
   endif
   if select("INDX_MAT")=0
      select 0
      _STXT:=getenv("MAGDEF")

      if empty(_stxt)
         _skey:=rat(HB_ps(),left(defa,len(defa)-1))
         _stxt:=hb_PathNormalize( if(_skey=0,defa+'..'+HB_ps(),left(defa,_skey))+'magazyn'+HB_ps() )
      endif
      _STXT+="roboczy"+HB_ps()
      if file(_stxt+"indx_mat.dbf")
      begin sequence
#ifdef STANY
      select select('INDX_MAT')
      use (_stxt+"indx_mat") READONLY SHARED
#ifdef A_CDX
      SET index to (_stxt+"indx_mat")
#else
      SET index TO (_STXT+"indx_NAZ"),(_STXT+"indx_NUM")
#endif
#else
      select select('stany')
      use (_stxt+"stany") READONLY SHARED
#ifdef A_CDX
      SET index to (_stxt+"stany")
      SET ORDER TO TAG STAN_MAG
#else
      SET index TO (_STXT+"stan_mag")
#endif
      select select('INDX_MAT')
      use (_stxt+"indx_mat") READONLY SHARED
#ifdef A_CDX
      SET index to (_stxt+"indx_mat")
#else
      SET index TO (_STXT+"indx_NAZ"),(_STXT+"indx_NUM")
#endif
      set relation to MAG_BIEZ+index into stany
#endif
      recover
      use
      return .t.
      end sequence
      else
       alarm("BRAK BAZY DANYCH :"+_stxt+hb_UTF8ToStr("indx_mat.dbf; Proszę ustawić prawidłowo zmienną środowiska DOS: MAGDEF"))
       return .t.
      end
   else
      select indx_mat
#ifndef STANY
      set relation to MAG_BIEZ+index into stany
#endif
   endif
   set order to tag indx_num
    _slth:=LEN(_spocz)
    _snagkol:=1
    _stxt:=_spocz
#ifdef STANY
    _spocz:=MAG_BIEZ+_spocz
#endif
    if !_skon
      _sfor={||STANY->STAN#0 .OR. STANY->wartosc#0}
    endif
    _skon:=NIL
    if _stxt>"9"
        SET ORDER TO tag indx_naz
        SEEK _spocz
        _sbeg=len(index)+2
    elseif ""=_stxt
        if index#trim(surowce->indx_mat)
           seek _spocz+trim(surowce->indx_mat)
        endif
    else
        seek _spocz
    endif
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
    x:=setkey(-1,)
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
        x:=setkey(-1,x)
        return .t.

   case _skey>31 .and. _skey<256
    do case
      case _skey>64 .AND. _sbeg=1
        SET ORDER TO tag indx_naz
        _spocz=LEFT(_spocz,len(_spocz)-_slth)+UpP(CHR(_SKEY))
        _slth=1
        _sbeg=hb_fieldlen('index')+2

      case _sbeg#1  .and. _slth=1 .and. _skey<58
        SET ORDER TO tag indx_num
        _sbeg=1
      OTHERWISE
        RETURN .F.
    ENDCASE       
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
    refresh(,_s)
    return .t.

   CASE _skey=2 .AND. _sbeg=1 // ^>
    SET ORDER TO tag indx_naz
    _sbeg:=len(index)+2
    //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    _spocz:=LEFT(_spocz,len(_spocz)-_slth)
    _slth:=0
    refresh(1,_s)

   CASE _skey=26 .AND. _sbeg#1 // ^<
    SET ORDER TO tag indx_num
    _spocz=LEFT(_spocz,len(_spocz)-_slth)
    _slth:=0
    _sbeg:=1
    //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(1,_s)

  CASE _skey=7 .or. _skey=-1// del - POKAZ/UKRYJ ZEROWKI
    IF _sfor=NIL
      _sfor:={||STANY->STAN#0 .OR. STANY->wartosc#0}
      REFRESH(,_s)
      if _si=0
         go _srec[1]
         _spocz=LEFT(_spocz,len(_spocz)-_slth)
         _slth=0
         refresh(,_s)
      endif
    else 
			_sfor:=NIL
      if _si=0
         go _srec[1]
      endif
      REFRESH(,_s)
    endif
    @ _srow2,_scol1+1 say if(_sfor=NIL,hb_UTF8ToStr("WSZYSTKIE MATERIAŁY"),"STAN NIEZEROWY") COLOR _slinia

  case _skey=13
    x:=setkey(-1,x)
    return(_sret:=.t.)

  case _skey=22
     set cursor off
     _stxt:={9,1,22,78,savescreen(9,1,22,78)}
     @ 9,1,22,78 BOX UNICODE '╔═╗║╝═╚║' color 'RG+/BG'
     @ 9,3 SAY 'Tylko do oglądania' UNICODE color 'RG+/BG'
     SET COLOR TO BG+/B
     hb_scroll(10,2,21,77,0)
     SELECT STANY
     @ 17,3   say "Data:"
     @ 18,3   say "Stan:"
     @ 19,3   say "Wartość:" UNICODE
     @ 17,12  SAY data_zmian
     @ 18,12  SAY stan
     @ 18,30  SAY zamk_mies1
     @ 18,48  SAY zamk_mies2
     @ 18,66  SAY zamkn_roku
     @ 19,12  SAY strpic(wartosc,9,A_ZAOKR,"@E ",.t.)
     @ 19,30  SAY strpic(wart_mies1,9,A_ZAOKR,"@E ",.t.)
     @ 19,48  SAY strpic(wart_mies2,9,A_ZAOKR,"@E ",.t.)
     @ 19,66  SAY strpic(wart_roku,9,A_ZAOKR,"@E ",.t.)
     @ 20,3   SAY "Cena:"
     @ 20,12  SAY strpic(wartosc/stan,9,A_ZAOKR,"@E ",.t.)
     @ 20,30  SAY strpic(wart_mies1/zamk_mies1,9,A_ZAOKR,"@E ",.t.)
     @ 20,48  SAY strpic(wart_mies2/zamk_mies2,9,A_ZAOKR,"@E ",.t.)
     @ 20,66  SAY strpic(wart_roku /zamkn_roku,9,A_ZAOKR,"@E ",.t.)
     @ 21,3   SAY "Cena ostatniej dostawy z dnia "+dtoc(data_przy)+" wynosi"+strpic(cena_przy,7,A_ZAOKR,"@E",.t.)+hb_UTF8ToStr(" zł.")
     SELECT INDX_MAT
    if lamus#0
    @ 10,3 say "*"
    endif
    @ 11,3  say data_popr
    @ 11,14 say nazwa color _sunsel
    @ 11,61 say index color _sunsel
#ifdef A_KODY
    @ 12,3 SAY A_KODY+':'
    sayl kod color _sunsel
#endif
    setpos(13,2)
    SAYl 'Miara:'
    sayl jm color _sunsel
#ifdef A_JMALT
    sayl "( ="
    sayl przel color _sunsel
#else
    sayl "(*"
    sayl przel color _sunsel
    SAYl '= 1'
#endif
    sayl jm_opcja color _sunsel
    sayl ")"
#ifdef A_NOZAP
    SAYl 'Informacja dodatkowa:'
    SAYl info picture "@KS14" color _sunsel
#else
    SAYl 'Zapas min:'
    sayl zapas_min color _sunsel
    SAYl 'max:'
    sayl zapas_max color _sunsel
#endif
    @ 15,3  say "Ważność" UNICODE
    sayl waznosc color _sunsel
    @ 15,15 say "dni"
    sayl padr(if(""=uwagi,"bez uwag",uwagi),58) color _sunsel
    WHILE (_skey:=inkey(0, INKEY_KEYBOARD + INKEY_LDOWN),_skey=K_LBUTTONDOWN .and. mousedrag({1,mcol(),mrow()},_stxt));ENDDO
    restscreen(_stxt[1],_stxt[2],_stxt[3],_stxt[4],_stxt[5])
    set cursor on

   case _skey=-8
      _sfil(_s)
      
   case _skey=-9
      _slist(".\M*.frm",_s)

endcase

RETURN .F.
//#endif
*********************
