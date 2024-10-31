#include "ar.ch"
#include "inkey.ch"
#include "getexitm.ch"
#ifdef A_DSPLIT
field smb_dow,nr_dowodu
#define dowoD smb_dow+nr_dowodu
#else
field dowod
#define smb_doW dowod
#define nr_dowodU SubStr(dowod,3)
#endif
#ifndef A_IDENT
  #define A_IDENT len(FIELD->ident)
#endif
field lp,pozycja,data,ident,kontrahent,konto,kwota,rejestr,nazwa,numer_kol,czyma,link,nazwisko
field sald_bie_m,sald_bie_w,syntet,khflag
memvar da
proc rejf9(ar,ad,f9dok,gl)
local stat,i,f,g,h,a
   stat:=push_stat()
   IF !EMPTY(f9dok)
      sel(ar[AR_DBF],1)
      seek(SubStr(f9dok,3,5))
   ENDIF
   if rejbrow(ar,.t.)
    f9dok:=ar[AR_SMB]+lp
    a:=array(Fcount())
    aeval(a,{|x,i|a[i]:=Fieldget(i)})
    pop_stat(stat)
    g:=gl[i:=findget(gl,'l_k')]
    f:=FieldPos("IDENT")
    if empty(g:varget()) .and. !empty(a[f]) .and. GetPreValidate(g, gl, i)
          if g:hasfocus
             g:killfocus()
          endif
       g:setfocus()
       g:varput(a[f])
       g:UpdateBuffer()
       g:changed:=.t.
       updated(.t.)
       GetPostValidate(g, gl, i)
    endif
    if "K"$ad[AD_FLAGS]
    g:=gl[i:=findget(gl,'kh')]
    f:=FieldPos("KONTRAHENT")
    if val(g:varget())=0 .and. val(a[f])<>0 .and. GetPreValidate(g, gl, i)
        sel("firmy","firm_num")
        dbseek(a[f],.f.)
        select (ar[AR_DBF])
          if g:hasfocus
             g:killfocus()
          endif
        g:setfocus()
        g:varput(a[f]) //+' '+nazwA)
        g:UpdateBuffer()
        g:changed:=.t.
        updated(.t.)
        GetPostValidate(g, gl, i)
    endif
    endif
    for i:=i+1 To Len(gl)
        g:=gl[i]
        f:=fieldpos(g:name)
        if f<>0 .and. empty(g:varget()) .and. !empty(a[f]) //.and. GetPreValidate(g, gl, i)
          if h:=g:hasfocus
             g:killfocus()
          endif
          g:varput(a[f])
          if h
            g:setfocus()
          else
            g:display()
          endif
          //g:UpdateBuffer()
          //g:changed:=.t.
          //updated(.t.)
          //GetPostValidate(g, gl, i)
        endif
    next i
    for i:=i+1 To Len(gl)
        g:=gl[i]
        f:=fieldpos(g:name)
        if f<>0 .and. GetPreValidate(g, gl, i)
          h:=g:hasfocus
          if !h
            g:setfocus()
          endif
            GetPostValidate(g, gl, i)
          if !h
            g:killfocus()
          endif
          //g:UpdateBuffer()
          //g:changed:=.t.
          //updated(.t.)
          //GetPostValidate(g, gl, i)
        endif
    next i
   else
    pop_stat(stat)
   endif
return
**************************
func khfix(k,oldkh,kh)
local v,x,r:=select(),p
if empty(oldkh) .or. empty(kh)
   return .t.
endif
select main
p:=recno()
seek k
do while rejestr+lp=k
   if right(konto,A_NRLTH)=oldkh
      v:=left(konto,A_KTL-A_NRLTH)+left(kh,A_NRLTH)
      select konta
      if !dbseek(v,.f.)
         dbseek(left(konto,A_KTL-A_NRLTH),.f.)
         if !khflag
           select main
           goto p
           select (r)
           return .f.
         endif
           x:=trim(firmy->nazwA)+" - "+nazwa
           append blank
           konto:=v
           nazwa:=x
      endif
      select main
      konlink(v)
   endif
   skip
enddo
goto p
select (r)
//kontrahent:=kh
return .t.
***********************
func pkbrow(b)
local Key,x
       b:autolite:=.f.
       //b:colpos:=1
       //b:hilite()
       do while .t.
          if b:stable
             @ b:rowpos+b:ntop,b:nleft SAY chr(16)
             key:=inkey(0)
          else
             b:stabilize()
             key:=inkey()
             if key=0
                loop
             endif
          endif

          @ b:rowpos+b:ntop,b:nleft SAY " "

          if key=K_ESC .or. key=K_F10 .or. key=K_CTRL_L .or. key=K_CTRL_RET .or. key=K_ALT_RETURN
             exit
          elseif key=K_UP
             b:up()
          elseif key=K_DOWN
             b:down()
          elseif key=K_PGUP
             b:Pageup()
          elseif key=K_PGDN
             b:Pagedown()
          elseif (x:=setkey(key))<>NIL
             b:forcestable()
             eval(x,procname(0),b)
          endif
       enddo
       b:forcestable()
       b:autolite:=.t.

return key
************************
func pkedit(b,ar,win,edit,l,kh,l_p,ap,f9dok)
local  c,key,chg:=.f.,k:=ar[AR_SMB]+l_p,kol,f9rec,f9arr,i
   if b=NIL
       b:=tbrowsenew(win[1]+l+3,win[2]+1,win[3]-1,win[4]-1)
       b:colorspec:=if(iscolor(),"W+/BR,I",'W,I')
#ifdef A_LPNUM
       b:addcolumn(tbcolumnnew("Lp.",{||LP(pozycja)}))
#else
       b:addcolumn(tbcolumnnew("Lp.",{||str(asc(pozycja)-48,3)}))
#endif
       b:addcolumn(tbcolumnnew(hb_UTF8ToStr("Treść"),{||padl(right(trim(ident),A_IDENT),A_IDENT)}))
       b:addcolumn(tbcolumnnew("Konto",{||konto}))
#ifdef A_LPNUM
       b:addcolumn(tbcolumnnew(hb_UTF8ToStr(" ─>"),{||LP(link)}))
#else
       b:addcolumn(tbcolumnnew(hb_UTF8ToStr(" ─>"),{||str(asc(link)-48,3)}))
#endif
       b:addcolumn(tbcolumnnew("Kwota Wn",{||if(czyma,space(A_WAL-1)+if(link>pozycja,"/","\"),tran(kwota,"@E "+REPLICATE("#",A_WAL-3)+".##"))}))
       b:addcolumn(tbcolumnnew("Kwota Ma",{||if(czyma.or.eof(),tran(kwota,"@E "+REPLICATE("#",A_WAL-3)+".##"),if(link>pozycja,"\","/")+space(A_WAL-1))}))
       kol:=win[4]-win[2]-2*A_WAL-A_KTL-A_IDENT-13
       b:addcolumn(tbcolumnnew("Nazwa Konta",{||if(empty(konto),space(kol),konta->(dbseek(main->konto),left(nazwa,kol)))}))
       b:colpos:=2

       b:gotopblock:={||b:cargo[1]:=if(dbseek(k,.f.),recno(),0)}
       b:gobottomblock:={||dbseek(left(k,6)+chr(asc(SubStr(k,7))+1)),dbskip(-1),b:cargo[2]:=if(rejestr+lp#k,(dbgoto(0),0),recno())}
       b:skipblock:={|n|bskip(b,n,k)}
       b:cargo:={0,0}
       eval(b:gotopblock)
    else //bo mi się k zmienia
       b:gotopblock:={||b:cargo[1]:=if(dbseek(k,.f.),recno(),0)}
       b:gobottomblock:={||dbseek(left(k,6)+chr(asc(SubStr(k,7))+1)),dbskip(-1),b:cargo[2]:=if(rejestr+lp#k,(dbgoto(0),0),recno())}
       b:skipblock:={|n|bskip(b,n,k)}
    endif
       b:gotopblock:={||b:cargo[1]:=if(dbseek(k,.f.),recno(),0)}
       b:gobottomblock:={||dbseek(left(k,6)+chr(asc(SubStr(k,7))+1)),dbskip(-1),b:cargo[2]:=if(rejestr+lp#k,(dbgoto(0),0),recno())}
       b:skipblock:={|n|bskip(b,n,k)}

       c:=array(31)

       c[K_DOWN]     :={|b|b:down()}
       c[K_UP]       :={|b|b:up()}
       c[K_LEFT]     :={|b|if(b:colpos>2,b:left(),)}
       c[K_TAB]:=;
       c[K_RIGHT]    :={|b|if(b:colpos<6,b:right(),)}
       c[K_PGUP]     :={|b|b:pageup()}
       c[K_PGDN]     :={|b|b:pagedown()}
       c[K_CTRL_PGUP]:={|b|b:gotop()}
       c[K_CTRL_PGDN]:={|b|b:gobottom()}
       c[K_END]      :={|b|b:end(),b:stabilize(),b:left()}
       c[K_HOME]     :={|b|b:home(),b:stabilize(),b:right()}
       c[K_DEL]      :={|b|del(b,k)}
       c[K_INS]      :={|b|ins(b,k)}


       if !edit
          if !eof()
             b:autolite:=.f.
             b:forcestable()
             b:autolite:=.t.
          endif
          return .f.
       endif

       b:hilite()

       if eof()
          if empty((ar[AR_DBF])->smb_doW)
             return .f.
          endif
          if !empty(f9dok) .and. dbseek(f9dok,.f.)
             f9dok:=rejestr+lp+pozycja
             if f9arr=NIL
                f9arr:=array(Fcount())
              endif
              aeval(f9arr,{|x,i|f9arr[i]:=Fieldget(i)})
              f9rec:=recno()
          endif

          plus(b,ar,@f9rec,f9dok,f9arr)
       endif

       __setproc(procname(0))

       do while .t.
          if b:stable
             key:=inkey(0)
          else
             b:stabilize()
             key:=inkey()
             if key=0
                loop
             endif
          endif

          if key=K_SH_TAB
             key=K_LEFT
          endif

          if key=K_ESC .or. key=K_F10 .or. key=K_CTRL_L .or. key=K_PGUP .and. (b:cargo[1]=recno() .or. eof())
             b:forcestable()
             exit
          elseif key=K_F9 .and. kwota=0
             kol:=push_stat()
             begin sequence
               if !empty(f9dok)
                  dbseek(f9dok)
                  f9dok:=''
               endif
               f9rec:=0
               if dziennik(3)
                  f9dok:=rejestr+lp+pozycja
                  if f9arr=NIL
                    f9arr:=array(Fcount())
                  endif
                  aeval(f9arr,{|x,i|f9arr[i]:=Fieldget(i)})
                  f9rec:=recno()
               endif
             end sequence
             pop_stat(kol)
             if f9rec<>0
               b:forcestable()
               LOCK
               for i:=1 to len(f9arr)
                    do case
                    case fieldname(i) = 'DATA'
                    case fieldname(i) = 'KWOTA'
                    case fieldname(i) = 'LP'
                    case fieldname(i) = 'REJESTR'
                    case fieldname(i) = 'POZYCJA'
                    case fieldname(i) = 'LINK' .and. f9arr[FieldPos('POZYCJA')]<>pozycja
                    otherwise
                         Fieldput(i,f9arr[i])
                    endcase
               next
               UNLOCK
               b:refreshcurrent()
             endif

          elseif key=43 .or. key=K_PGDN .and. (b:cargo[2]=recno() .or. eof())
             plus(b,ar,@f9rec,f9dok,f9arr)

          elseif key=K_ENTER .or. key>31 .and. key<256
             b:forcestable()
             if key#K_ENTER
                kibord(chr(key))
             endif
             if eof()
                kibord("+")
                loop
             endif
             doget(b,kh,k,ap)
             b:forcestable()
          elseif key>0 .and. key<32 .and. c[key]#NIL
             eval(c[key],b)
          elseif (i:=setkey(key))<>NIL
             b:forcestable()
             eval(i,procname(0),b,ar,ap)
          endif
       enddo

       b:dehilite()

return chg
**********
stat func bskip(b,n,k)
local i:=0,r
if n=0
elseif eof() .or. rejestr+lp!=k
  b:cargo:={0,0}
  dbgoto(0)
elseif n<0
   do while i>n
      r:=recno()
      skip -1
      if rejestr+lp!=k .or. bof()
         goto r
         b:cargo[1]:=r
         exit
      endif
      --i
   enddo
else
   do while i<n
      r:=recno()
      skip
      if rejestr+lp!=k //.or. eof()
         goto r
         b:cargo[2]:=r
         exit
      endif
      ++i
   enddo
endif
return i
************
stat func doget(b,kh,k,ap)
local get,v,i,x,r
r:=recno()
i:=b:colpos
lock
get:=getnew(row(),col(),{|x|if(x=NIL,v,v:=x)},,"@!K")

if i=2
   get:block:={|x|if(x=NIL,v:=ident,ident:=v:=x)}
   get:name:='ident'
   get:picture:="@KS"+lTrim(sTr(A_IDENT))

elseif i=3
   v:=konto
   if !empty(v)
     konta->(dbseek(v))
   endif
   get:postblock:={|g|konval(g,,kh).and.konlink(v)}
   get:name:='konto'

elseif i=4
   v:=link
#ifdef A_LPNUM
   get:block:={|x|if(x=NIL,val(v),v:=if(x=0,space(A_LPNUM),str(x,A_LPNUM)))}
#else
   get:block:={|x|if(x=NIL,asc(v)-48,v:=if(x=0,' ',chr(x+48)))}
#endif
   get:picture:="@KZ ###"
   get:postblock:={|g|!g:changed.or.fixlnk(v,k,b)}
   get:name:='link'
#ifdef A_HBGET
   get:display()
#endif


else
   v:=kwota
   if (i=5)=czyma
     v:=-kwota
     if !fixlink(0,b,k,.t.)
        b:refreshcurrent()
        b:forcestable()
        return .f.
     endif
     czyma:=!czyma
     if !fixlink(v,b,k,.t.)
        b:refreshcurrent()
        b:forcestable()
        return .f.
     endif
     b:refreshcurrent()
     b:forcestable()
   endif
   get:picture:="@KE "+REPLICATE("#",A_WAL-3)+".##"
   get:postblock:={|g|!g:changed.or.fixlink(@v,b,k)}
   get:name:='konto'

endif

setcursor(if(set(_SET_INSERT),2,1))
x:=SET(_SET_EXIT,.t.)
begin sequence
if ap[AP_WHEN]=NIL .or. eval(ap[AP_WHEN],get,v,i)
  if readmodal({get})
    //get:changed:=.t.
    if  ap[AP_VALID]#NIL .and. !eval(ap[AP_VALID],get,v,i)
      break
    endif
  endif
endif
recover
   get:exitstate:=GE_ESCAPE

end sequence
set(_SET_EXIT,x)
setcursor(0)
UNLOCK
select main
goto r
r:=ReadkeY()
b:refreshcurrent()
b:forcestable()

if get:exitstate=GE_ESCAPE
   return .f.
endif


if r=K_ENTER
   if i=3 .and. link#pozycja
      b:dehilite()
      b:colpos:=if(czyma,6,5)
   elseif b:colpos<6
      b:right()
   endif
elseif r=K_LEFT
   if i>4
      b:dehilite()
      b:colpos:=3
   else
      b:left()
   endif
elseif r=K_UP
   b:up()
elseif r=K_DOWN
   b:down()
#ifdef A_HBGET
elseif get:exitstate=GE_WRITE
#else
elseif get:exitstate=GE_WRITE .or. get:exitstate=GE_PGUP .or. get:exitstate=GE_PGDN
#endif
   kibord(chr(r))
endif
return updated()
*****************
stat func fixlink(v,b,k,noswap)
local r,c,s,p,g
c:=round(v-kwota,A_ZAOKR)
r:=recno()
if c=0
   return .t.
endif
do while .t.
 if !konta->(dbseek(main->konto,.f.))
   tone(130,3)
   b:colpos:=3
   kibord(chr(K_ESC))
   if r#recno()
      goto r
      c:=link
      if link>pozycja
          while rejestr+lp=k .and. c>pozycja ; b:down() ; b:stabilize() ; enddo
      else
          while rejestr+lp=k .and. c<pozycja ; b:up() ; b:stabilize() ; enddo
      endif
   endif
   return .f.
 endif
#ifdef A_LAN
   kklock(konto)
#endif
   if r#recno()
      goto r
      exit
   endif
   if !dbseek(k+link,.f.)
      goto r
      tone(130,3)
      b:colpos:=4
      kibord(chr(K_ESC))
      return .f.
   endif
   g:=recno()
   lock g
enddo
do while .t.
   kwota+=c
   kk(konto,c,czyma)
   if r#recno()
      //UNLOCK recno()
      goto r
      exit
   endif
   s:=czyma
   p:=pozycja
   dbgoto(g) //dbseek(k+link,.f.)
   if v<>0 .and. noswap<>.t. .and. kwota=0 //empty(link)
      link:=p
   endif
   if s=czyma
     if kwota=0 .and. noswap<>.t.
       czyma:=!czyma
     else
       c*=-1
     endif
   endif
enddo
displink(b,k)
unlock in KONTA
return .t.
***********
stat func fixlnk(v,k,b)
local r,c,p

p:=pozycja
if v=p
   return .f.
endif

if kwota=0 //.and. empty(link)
   link:=v
   return .t.
endif

r:=recno()
c:=czyma

if dbseek(k+v,.f.)

   if kwota=0 .and. link<>p
      lock recno()
      link:=p
      UNLOCK recno()
   endif
   goto r
   link:=v
   displink(b,k)
   return .t.
endif
goto r
return .f.
***********
stat proc displink(b,k)
local c,r,i
#ifdef A_LPNUM
  i:=VAL(link)-VAL(pozycja)
#else
  i:=ASC(link)-ASC(pozycja)
#endif
  i+=b:RowPos
  IF i>0 .and. i<=b:RowCount
      b:autolite:=.f.
      c:=link
      r:=pozycja
      if i:=link>pozycja
          while rejestr+lp=k .and. c>pozycja ; b:down() ; b:stabilize() ; enddo
      else
          while rejestr+lp=k .and. c<pozycja ; b:up() ; b:stabilize() ; enddo
      endif
      b:forcestable()
      b:refreshcurrent()
      b:forcestable()
      if !i
          while rejestr+lp=k .and. r>pozycja ; b:down() ; b:stabilize() ; enddo
      else
          while rejestr+lp=k .and. r<pozycja ; b:up() ; b:stabilize() ; enddo
      endif
      b:autolite:=.t.
      b:forcestable()
  endif
return
***********
stat func konlink(v)
   if kwota=0
      konto:=v
      return .t.
   endif
#ifdef A_LAN
   kklock(konto)
   kklock(v)
#endif
   kk(konto,-kwota,czyma)
   konto:=v
   kk(konto,kwota,czyma)
   unlock in KONTA
return .t.
***********
stat func linksto(k)
local a:={},r:=recno(),p:=pozycja
seek k
exec aadd(a,recno()) rest while rejestr+lp=k for link=p .and. kwota#0
goto r
return a
***********
stat proc del(b,k)
local r,p
if kwota#0 .or. !empty(p:=linksto(k))
   if kwota=0 .and. len(p)=1
      r:=recno()
      lock r
      goto p[1]
      if empty(p:=linksto(k))
         lock recno()
         ALARM(hb_UTF8ToStr('PROSZĘ WYKONAĆ KONTROLĘ SALD!;Konto: ')+trim(konto)+hb_UTF8ToStr(' nie zagadza się o: ')+lTrim(sTr(kwota)))
         kwota:=0
      endif
      goto r
   endif
   if kwota#0 .or. !empty(p)
     tone(130,3)
     b:dehilite()
     b:colpos:=if(czyma,6,5)
     return
   endif
endif
lock
p:=pozycja
delete
skip
r:=recno()
#ifdef A_LAN
  #define D_LAN reclock(.F.,,,,),
#else
  #define D_LAN
#endif
#ifdef A_LPNUM
exec {||D_LAN pozycja:=str(val(pozycja)-1,A_LPNUM)} rest while rejestr+lp=k
#else
exec {||D_LAN pozycja:=chr(asc(pozycja)-1)} rest while rejestr+lp=k
#endif
//goto r
dbseek(k,.f.)
#ifdef A_LPNUM
exec {||D_LAN link:=str(val(link)-1,A_LPNUM)} rest while rejestr+lp=k for link>=p
#else
exec {||D_LAN link:=chr(asc(link)-1)} rest while rejestr+lp=k for link>=p
#endif
#undef D_LAN
goto r
if rejestr+lp#k
   eval(b:gobottomblock)
endif
UNLOCK
//freshorder(b)
  B:refreshAll()
if eof()
   kibord(chr(K_PGUP))
   return
endif
return
****************
stat proc ins(b,k)
local p,r
r:=recno()
p:=pozycja
eval(b:gobottomblock)
#ifdef A_LPNUM
if pozycja=repl('9',A_LPNUM)
#else
if pozycja=HB_UCHAR(0x0A0)
#endif
   alarm(hb_UTF8ToStr("Osiągnięto maksimum ilości pozycji pojedynczego dokumentu !"),,3,3)
   goto r
   return
endif
do while rejestr+lp=k .and. !bof()
   if pozycja>=p
      lock recno()
#ifdef A_LPNUM
      pozycja:=str(val(pozycja)+1,A_LPNUM)
#else
      pozycja:=chr(asc(pozycja)+1)
#endif
   endif
   if link>=p
      lock recno()
#ifdef A_LPNUM
      link:=str(val(link)+1,A_LPNUM)
#else
      link:=chr(asc(link)+1)
#endif
   endif
   skip -1
enddo
//UNLOCK
goto r
r:={rejestr,lp,konto,czyma,link,ident}
append blank
rejestr:=r[1]
lp:=r[2]
data:=da
//konto:=r[3]
czyma:=r[4]
link:=r[5]
ident:=r[6]
pozycja:=p
UNLOCK
b:colpos:=3
//freshorder(b)
  B:refreshAll()
return
***********************
stat proc plus(b,ar,f9rec,f9dok,f9arr)
local k,id
if eof() .or. rejestr#ar[AR_SMB] .or. lp#(ar[AR_DBF])->lp

#ifdef A_LPNUM
   k:={str(1,A_LPNUM),str(2,A_LPNUM),.f.}
#else
   k:={'1','2',.f.}
#endif
   b:gotop()
   id:=(AR[AR_DBF])->IDENT
else
   k:=recno()
   eval(b:gobottomblock)
   id:=ident
#ifdef A_LPNUM
if pozycja=repl('9',A_LPNUM)
#else
if pozycja=HB_UCHAR(0x0A0)
#endif
   alarm(hb_UTF8ToStr("Osiągnięto maksimum ilości pozycji pojedynczego dokumentu !"),,3,3)
   goto k
   return
endif
#ifdef A_LPNUM
   k:={str(val(pozycja)+1,A_LPNUM),if(kwota=0,pozycja,link),czyma}
#else
   k:={chr(asc(pozycja)+1),if(kwota=0,pozycja,link),czyma}
#endif
   if !empty(f9rec)
      goto f9rec
      skip
      if f9dok#rejestr+lp
         f9rec:=0
      else
         f9rec:=recno()
         f9arr:=array(Fcount())
         aeval(f9arr,{|x,i|f9arr[i]:=Fieldget(i)})
      end
   endif
endif
   append blank
   if empty(f9rec)
      b:colpos:=3
   else
      aeval(f9arr,{|x,i|Fieldput(i,x)})
      kwota:=0
      //konto:=''
   endif
   rejestr:=ar[AR_SMB]
   lp:=(ar[AR_DBF])->lp
   if empty(IDENT)
     ident:=id
   endif
   if POZYCJA#K[1]
     POZYCJA:=K[1]
     link:=k[2]
     czyma:=k[3]
   endif
   data:=da
   UNLOCK
   b:gobottom()
return
****************************
