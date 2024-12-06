//#define SIMPLE
#include "dbinfo.ch"
#include "inkey.ch"
#include "set.ch"

#ifdef A_SX
#include "dbinfo.ch"
#endif

#ifdef SIMPLE
#undef A_MYSZ
#endif
#ifdef A_MYSZ
static apro:={}
#endif



//ANNOUNCE RDDSYS

static cdpstack:={},cdpptr:=0
static _a:={}
*******************************
#ifdef mkdir
function MKDIR(x)
return mkdir(x)
#endif
********************************
func ean13(x)
local a:=0,i

if len(alltrim(x))<12
   return pad(x,13)
endif

for i:=1 to 12 step 2
   a+=val(SubStr(x,i,1))+3*val(SubStr(x,i+1,1))
next i

return left(x,12)+str((220-a)%10,1)
********************************
#ifdef UpP
function UPP(x)
RETURN UpP(x)
#endif
******************
function expand
parameters x
memvar x
if ! '"'$x
  return &('"&x"')
elseif ! "'"$x
  return &("'&x'")
elseif ! "]"$x
  return &("[&x]")
endif
return x
******************
function compile(x)
return if(empty(x),,&x)
******************
//#ifndef XOR
function xor(a,b)
return if(a,!b,b)
//#endif
******************
func netdo(ip,port,timeout,block)
local a,ad,socket,s

hb_inetInit()
socket := hb_inetServer(port, , ip)


if hb_inetErrorCode(socket) <> 0
  alarm( tran(hb_inetErrorCode(socket),) + ' '+hb_inetErrorDesc(socket))

  return .f.
endif

hb_inetTimeout( socket, timeout * 1000 )

while !EMPTY(s:=hb_inetRecvLine(socket))
     hb_idlestate()
     ad:=hb_inetAddress(socket)

     a := eval(block, s, ad)

     if !empty(a)
         hb_inetSend(socket, ad, port, a)

         hb_idlestate()
     endif
end
hb_inetClose(socket)

return .t.
******************
function sign(x)
return if(x<0,-1,if(x>0,1,0))
******************
func zaokr(x,d)
local l,a,s:=sign(x)
  x:=abs(x)
  if d=NIL
     d:=2
  endif
  if x<>0
     l:=log(x)/log(10)-d
     a:=l-int(l)
     if a<0
        a+=1
     endif
     l:=l-a
     if a>.7
        a:=1
     elseif a>.3 .or. abs(x-round(x*5,-l)/5)>abs(x-round(x*2,-l)/2)
        a:=2
     else
        a:=5
     endif
     x:=s*round(x*a,-l)/a
  endif
return x
*******************************
proc rel(arel) //{SKĄD DOKĄD TAMINDEX RELACJA}
local s:=select()
aeval(arel,{|a|sel(a[2],a[3]),sel(a[1])})
aeval(arel,{|a|dbselectar(a[1]),dbsetrelat(a[2],EvAlDb('{||'+a[4]+'}'),expand(a[4]))})
select (s)
return
******************************
func make_subar(a,l,nul)

aeval(a,{|x,i|a[i]:=getlines(x,";")})
if l#NIL
   aeval(a,{|x,y|y:=len(x),asize(x,max(y,l)),afill(x,nul,y+1)})
endif

return a
************************
func netuse(a,b,file,alias,shared,rdo,cp)   // w netio sciezka serwera
#ifdef A_NETIO
memvar netio
 if !empty(netio) .and. file=netio
    file:='net:'+SubStr(file,len(netio)+1)
 endif
return dbusearea(a,b,file,alias,shared,rdo,cp)
#else
return NetusE(a,b,file,alias,shared,rdo,cp)
#endif
**************************
func linpath(x)
#ifdef __PLATFORM__UNIX
return(Lower(Strtran(strtran(x,';',':'),'\','/')))
#else
return x
#endif
**************************
func sel(alias,order,shared,rdo,cp)
field baza,path,nazwa,plik,codepage
local s,a,b,c

#ifdef A_NETIO
  memvar netio
  #define D_NETIO ,netio
#else
  #define D_NETIO
#endif
//memvar a,b,c
//private a,b,c
   alias:=lower(trim(alias))
   s:=select(alias)
   if s=0
      if alias=="indeks"
         s:=11
         do while (++s)->(used())
         end
         select (s)

         b:=findfile(alias+".dbf" D_NETIO)
         if !empty(b)
           a:=b
         endif
         //NUSE (a) ALIAS (alias) CODEPAGE cp
         NetusE(NIL,NIL,a,alias,shared,rdo,cp)
      else
         a:=b:=alias
         s:=select()
#ifdef A_CDX
         if sel("indeks",,.t.,.t.)#0
            locate for {||c:=trim(baza), lower(expand(c))==alias}
            if found()
               a:=trim(path)
               if !empty(a)
                if a="&:"
                  a:=trim(&(SubStr(a,3)))
                endif
                a:=expand(a)
                if right(a,1)<>HB_ps()
                  a+=HB_ps()
                endif
               endif
               if fieldpos([PLIK])#0 .and. !empty(plik)
                  a+=trim(plik)
               else
                  a+=lower(c)
               endif
               a:=expand(a)
               if fieldpos([codepage])#0 .and. empty(cp)
                  cp:=expand(trim(codepage))
                  if empty(cp)
                     cp:=NIL
                  endif
               endif
            endif
         endif
         select (s)
         if used()
            select 0
         endif
         b:=findfile(a+".dbf" D_NETIO)
         if !empty(b)
           a:=b
         endif
         NetusE(NIL,NIL,a,alias,shared,rdo,cp)
         if !used()
            select (s)
            return 0
         endif
         if !empty(a)
            a:=left(a,len(a)-4)
         endif
         s:=select()
         if !empty(order) .and. empty(ordbagname(order)) .and. select("indeks")#0 .and. indeks->(found())
            select indeks
            if valtype(order)='C'
              b:=upper(order)
              locate while trim(baza)==c for trim(nazwa)==b
            else
              skip (order-1)
            endif
            select (s)
          if trim(indeks->baza)==c
            ordlistadd(a+ordbagext())
            ordsetfocus(order)
            if empty(ordbagname(order))
              c:=errornew()
              c:filename:=a+ordbagext()
              c:subsystem:=rddSetDefault()
              c:subcode:=1003
              c:candefault:=.t.
              eval(errorblock(),c)
            endif
          endif
         elseif !empty(ordbagname(1))
            ordsetfocus(1)
         endif
#else
         if sel("indeks",,.t.,.t.)=0
            select (s)
            if used()
              select 0
            endif
            b:=findfile(alias+".dbf" D_NETIO)
            if !empty(b)
              a:=b
            endif
            NetusE(NIL,NIL,a,alias,shared,rdo,cp)
            if !used()
               select (s)
               return 0
            endif
            s:=select()
         else
            locate for {||c:=trim(baza), lower(expand(c))==alias}
            if found()
               a:=trim(path)
               if !empty(a)
                if a="&:"
                  a:=trim(&(SubStr(a,3)))
                endif
                a:=expand(a)
                if right(a,1)<>HB_ps()
                  a+=HB_ps()
                endif
               endif
               b:=a
               if fieldpos([plik])#0 .and. !empty(plik)
                  a+=trim(plik)
               else
                  a+=lower(c)
               endif
               a:=expand(a)
               if fieldpos([codepage])#0 .and. empty(cp)
                  cp:=expand(trim(codepage))
                  if empty(cp)
                     cp:=NIL
                  endif
               endif
            endif
            b:=findfile(a+'.dbf' D_NETIO)
            if !Empty(b)
               a:=b
            endif
            b:=left(a,rat(HB_ps(),a))
            select (s)
            if used()
               select 0
            endif
            NetusE(NIL,NIL,a,alias,shared,rdo,cp)
            if !used()
               select (s)
               return 0
            endif
            s:=select()
            select indeks
            if found()
               a:=''
#ifdef A_ADS
//za drugim razem doda jeżeli za pierwszym był odtworzony
#define D_CHECK ,if(indexord()=0,ordlistadd(a),)
#else
#define D_CHECK
#endif
               exec {|c|a:=expand(b+lower(trim(nazwa))),(s)->(ordlistadd(a) D_CHECK )} rest while c==trim(baza) for !empty(nazwa)
#undef D_CHECK
            endif
            select (s)
         endif
#endif
      endif
      if !used()
         return 0
      endif
      if order#NIL
        go lastrec()
      endif
   else
      select (s)
      set filter to
      set relation to
   endif
   if order#NIL
      ordsetfocus(order)
   endif
return s
*****************************
FUNC DSEEK(lE,cFieldList,...)
static ab:={}
local txt:='{|'+if(cFieldList=NIL,'',UpP(cFieldList))+'|'+IndexkeY(0)+'}'
local i:=ascan(ab,{|x|x[1]==txt})
local b,r:=recno()
if i=0
   b:=EvAlDb(txt)
   aadd(ab,{txt,b})
else
   b:=ab[i,2]
endif
  if Le=.f.
     Return b
  endif
  if Le=.t.
    dbgoto(0)
  endif
  txt:=EvaldB(b,...)
  if Le=.t.
    dbgoto(r)
  endif
RETURN txt
******************************
func get_relat()
local ar:={},j:=0
do while dbrselect(++j)#0
   aadd(ar,{dbrselect(j),dbrelation(j)})
enddo
set relation to
return ar
******************
#ifdef SIMPLE
#command ? [<list,...>]   =>  ?? HB_EOL() [; ?? <list>]
#command ?? <x1> [,<xn>] => OuterR(<x1>)[;OuterR(<xn>)]

function alert(x,y)
return alarm(x,y)

function alarm(txt,parr,i)

  ? txt
  if empty(parr)
    ?? hb_UTF8ToStr(" - Naciśnij [Enter]:")+chr(7)
    xfr(0,@txt,254)
    return 1
  else
    ?? " -"
  for i:=1 to len(parr)
    ?? " ["+left(parr[i],1)+"]"+trim(SubStr(parr[i],2))
  next
    ?? " :"
  endif
  do while .t.
     ?? chr(7)
     txt:=' '
     fread(0,@txt,1)
     txt:=upper(txt)
     i:=ascan(parr,txt)
     if i#0
        ?? txt
        exit
     endif
     loop
  enddo
return i
***************************
function message(txt)
   static h:=1,i:=0
   local m:=valtype(txt)

 if m="N"
    if --h=0
       ?? chr(8)+{"-","\","|","/"}[i:=i%4+1]
       h:=txt
    endif
 elseif m$"MC"
   i:=0
   ? txt+' '
 endif

 return i
**********
#else
**********************
func push_stat
local i:=0,array:={select(),{},setcolor(),Set(_SET_INSERT),setcursor(),row(),col()}
do while (++i)->(used())
   (i)->(aadd(array[2],{get_relat(),ordnumber(),recno(),dbrlocklist()}))
enddo
return(array)
*********************
proc pop_stat(array)
setcolor(array[3])
Set(_SET_INSERT,array[4])
setcursor(array[5])
setpos(array[6],array[7])
aeval(array[2],{|x,i|dbselectarea(i),dbclearrelation()})
aeval(array[2],{|x,i|;
dbselectarea(i),;
ordsetfocus(x[2]),;
if(recno()<>x[3],dbgoto(x[3]),),;
if(ascan(x[4],x[3])<>0.and.ascan(dbrlocklist(),x[3])=0,dbrlock(x[3]),),;
aeval(x[1],{|x|dbsetrelation(x[1],EvAlDb('{||'+x[2]+'}'),x[2])});
})
select (array[1])
return
******************
proc pop_norec(array)
setcolor(array[3])
Set(_SET_INSERT,array[4])
setcursor(array[5])
setpos(array[6],array[7])
aeval(array[2],{|x,i|dbselectarea(i),dbclearrelation()})
aeval(array[2],{|x,i|;
dbselectarea(i),;
ordsetfocus(x[2]),;
aeval(x[1],{|x|dbsetrelation(x[1],EvAlDb('{||'+x[2]+'}'),x[2])});
})
select (array[1])
return
******************
function alarm(txt,parr,ps,pe,keep)

  local i,k,l,m,n,rs,cs,sc,cu,lc,ink_flag,w

  ink_flag:=parr=NIL

  if empty(parr)
     parr:={hb_UTF8ToStr("Naciśnij Enter")}
  endif
  IF PS=NIL
     PS:=0
  ENDIF
  IF PE=NIL
     pe:=0
  ENDIF
  n:=m:=0
  l:=1
  aeval(parr,{|x|l+=len(x)+1})
#ifdef __PLATFORM__UNIX  
  txt:=strtran(txt,hb_BChar(0x0D)+hb_BChar(0x0A),HB_EOL())
#endif
  txt:=strtran(txt,";",HB_EOL())
  //txt:=strtran(txt,HB_EOL(),hb_BChar(0x8D)+hb_BChar(0x0A))
  k:=mlcount(txt,cs:=maxcol()-4)
  for i:=1 to k
    m:=max(m,len(TRIM(memoline(txt,cs,i))))
  next
  if l>cs
     rs:=l:=1
     for i:=1 to len(parr)
       if rs+len(parr[i])+1>cs
          ++k
          ++n
          rs:=1
       endif
       rs+=len(parr[i])+1
       l:=max(l,rs)
     next
  endif
  m:=max(l,m)
  w:=window(k+if(ink_flag.and.ps#0,0,2),m,if(iscolor(),"W+/R,I,,R+/R,W+/R","I,W+,,I,I"))
  for i:=1 to k-n
    @ w[1]+i,w[2]+2 say memoline(txt,m,i)
  next
  if ink_flag
  tone(130,pe)
  if ps=0
     @ w[3]-1,w[2]+2 say padc(parr[1],m)
  endif
  ps:=INkey(ps)
else
  i:=w[3]-n-1
  n:=max(0,int((m-l)/(len(parr)+1))) // odstep
  l+=(len(parr)+1)*n
  ++n
  k:=l:=w[2]+1+n+int((m-l+2)/2)      // start
  for m:=1 to len(parr)
    if l+len(parr[m])>=w[4]-1
       ++i
       l:=k
    endif
    @ i,l-1 prompt " "+parr[m]+" "
    l+=n+len(parr[m])
  next

  MENU TO PS
  iF PS=0
     PS:=PE
  ENDIF
  if PS=0
     txt:=""
  else
     txt:=upper(left(parr[ps],1))
  endif
endif

if keep=.t.
   keep:=w
else
   window(w)
endif

return ps
******************************
func strpic(x,y,z,zero,odst)
local pic:=""
if y=NIL
   return tran(x,zero)
endif
if zero=NIL
   zero:=""
endif
if odst=NIL
#ifdef A_DIETA
#define D_ODST 8
#else
#define D_ODST 12
#endif
   odst:=y>D_ODST
#undef D_ODST
endif
if z#NIL .and. z>0
   pic:="."+replicate("#",z)
   y-=z+1
endif
pic:=zero+if(odst,padl(replicate(",###",int((y-1)/4)),y,"#"),replicate("#",y))+pic
return tran(x,pic)
**********************************
function dtov(d)
return DTOV(d)
******************************
FUNCTION KIBORD(txt)
  HB_KEYINS(txt)
RETURN(.t.)
***************************
function window(rown,coln,color)
   local r1,c1,r2,c2,sc,rs,cs,cu,lc
   //    1  2  3  4  5  6  7  8  9

   sc:=valtype(rown)
   if sc="N"

   r1=1+ROW()
   c1=MAX(0,int(col()-coln/2-2))
   
   c2=c1+3+coln
   r2=r1+1+rown

   IF c2>maxcol() // poza ekran
      c1-=c2-maxcol()
      c2=maxcol()
      IF c1<0
         c1:=0
      ENDIF
   ENDIF

   IF r2>maxrow() // poza ekran
      r1-=r2-maxrow()
      r2=maxrow()
      IF r1<0
         r1:=0
      ENDIF
   ENDIF

   rs := ROW()
   cs := COL()
   cu := setcursOR(0)
   sc := SAVESCREEN(r1,c1,r2,c2)
   if color=NIL
      color:=if(iscolor(),"W+/GR,I,,GR+/GR,W+/B","I,W+,,I,W+")
   endif
   lc := SETCOLOR(color)
   colorselect(3)
   @ r1,c1,r2,c2 BOX UNICODE '┌─┐│┘─└│ '
   colorselect(0)
   return({r1,c1,r2,c2,sc,rs,cs,cu,lc})
 elseif sc="A"
   RESTSCREEN(rown[1],rown[2],rown[3],rown[4],rown[5])
   setpos(rown[6],rown[7])
   SETCURSOR(rown[8])
   SETCOLOR(rown[9])
 endif

return rown
***************************
function message(txt)
   static h:=1,i:=0
   local rs,cs,w,k,m,n
   m:=valtype(txt)

 if m="N"
    if --h=0
       rs := ROW()
       cs := COL()
       dispout({"-","\","|","/"}[i:=i%4+1])
       setpos(rs,cs)
       h:=txt
    endif
 elseif m$"MC"
   i:=0
 #ifdef __PLATFORM__UNIX  
   txt:=strtran(txt,hb_BChar(0x0D)+hb_BChar(0x0A),HB_EOL())
 #endif
   txt:=strtran(txt,";",HB_EOL())
   k:=mlcount(txt,maxcol()-4)
   m:=1
   for n:=1 to k
     m:=max(m,len(TRIM(memoline(txt,maxcol()-4,n))))
   next
   m:=min(maxcol()-4,m)  // ilosc column
   w:=window(k,m)

   for n:=1 to k
     @ w[1]+n,w[2]+2 say memoline(txt,m,n)
   next
   return w
 elseif m="A"
   window(txt)
 endif

 return i

***************************
FUNCTION tak(_prompth,r,c,l,l1,col) // zadaje pytanie

local    _x,_x1,_pm1,_scr,_c,_cur:=setcursor(0),k,b

k:=hb_setkeyget(K_F10,@b)

_x1 = LEN(_prompth)

IF _x1 < maxcol()-11
  if c#NIL
    _x := min(c,maxcol()-_x1-11)
  else
    _x := INT((maxcol()-_x1-11)/2 )
  endif
  _x1:= _x + _x1
ELSE
  _x:=0
  _x1:=maxcol()-11
  _prompth:=left(_prompth,_x1)
ENDIF
if r=NIL
   r:=row()
endif
_scr:=savescreen(r,_x,r,_x1+12)

_c:=setcolor(if(col=NIL,if(iscolor(),"W+/B,I,,,W+/B","W+,I,,,W+"),col))

@ r,_x SAY _prompth+' (   /   ) ?'

tone(262,2)

DO WHILE .T.
  _pm1 = IF(l=NIL.or.l,1,2)
  @ r,_x1+2 PROMPT 'Tak'
  @ r,_x1+6 PROMPT 'Nie'
  MENU TO _pm1
  IF _pm1 != 0
    l:=_pm1=1
    exit
  endif
  if l1#NIL // co gdy esc
     l:=l1
     exit
  endif
ENDDO

setkey(K_F10,k,b)
restscreen(r,_x,r,_x1+12,_scr)
setpos(r,_x)
setcolor(_c)
SetCursor(_cur)

RETURN(l)
****************************************************************************
FUNCTION ACZOJS(ARRAY,var,_e,alog,tyt) // sciagawka w valid

local _a,_b,_c,_d,_s,_get,_l,_COLOR,_cur,r,c,sl,sx,sp,i

if empty(array)
   _e:=0
   return .t.
endif

if var = NIL
   _get=getactive()
   var:=_get:buffer
endif

SX:=SL:=_l:=len(var) // długość var

IF !EMPTY(VAR)
   sl:=hb_at(" ",var,sl-len(ltrim(var)))
if sl=0 // pełne pole
   sl:=_l // sl - dlugosc kodu z okienka
//   sx:=_l // sx - dlugosc kodu z array (gdy mniejsze od okienka - *)
endif
endif

if valtype(_e)#'N'
   _e:=0

   For i:=1 to len (array)

     if alog=NIL.or.alog[i]
        sx:=at("*",array[i])-1
        if sx<0
          sx:=sl
        endif
        sp:=at("?",array[i])
        if sp>sx
          sp=0
        endif

        if UpP(if(sp>0,stuff(var,sp,1,"?"),var))=UpP(left(array[i],sx))
          _e:=i
          Exit
        endif
     endif
   next i
/*
   AEVAL(ARRAY,{|x,i|;
     if(_e=0.and.(alog=NIL.or.alog[i]),;
        (;
        sx:=at("*",x)-1,;
        if(sx<0,sx:=sl,),;
        sp:=at("?",x),;
        if(sp>sx,sp=0,),;
        if(UpP(if(sp>0,stuff(var,sp,1,"?"),var))=UpP(left(x,sx)),_e:=i,);
        ),;
     );
   })
*/
    // szukam do pierwszej spacji lub gwiazdki omijam ?
   _c:=_e=0 .or. _l+1<at(" ",array[_e]) // nie znalazlem
else
   _c:=.t.
endif

IF _c
   _cur:=setcursor(0)

   * _a,_b,_c,_d - wspolzedne sciagawki

   _a=ROW()+1    // sciaga pod polem
   _b=COL()-1

   IF _b<0
      _b=0
   ENDIF
   _c:=0
   aeval(array,{|x|_c:=max(_c,len(x))})
   _d:=_b+_c+1            // ilosc kolumn
   _c:=_a+LEN(ARRAY)+1    // ilosc wierszy

   IF _d>maxcol() // poza ekran
      _b=_b-_d+maxcol()
      _d=maxcol()
      IF _b<0
         _b=0
      ENDIF
   ENDIF

   IF _c>maxrow() // poza ekran
      _a=_a-_c+maxrow()
      _c=maxrow()
      IF _a<0
         _a=0
      ENDIF
   ENDIF

   _s=SAVESCREEN(_a,_b,_c,_d)
   r:=row()
   c:=col()
   IF LEN(ARRAY)<maxrow()
      @ _a,_b,_c,_d BOX UNICODE '╔═╗║╝═╚║' COLOR "GR+/GR"
    ELSE
#ifdef __PLATFORM__DOS
      @ _a,_b,_c,_d BOX UNICODE '╔─╗║╝─╚║' COLOR "GR+/GR"
#else
      @ _a,_b,_c,_d BOX UNICODE '╓─╖║╜─╙║' COLOR "GR+/GR"
#endif
   ENDIF
   if tyt#NIL
      @ _a,_b+1 SAY left(tyt,_d-_b) COLOR "GR+/GR"
   endif
   if _e=0
      tone(130,3)
   endif

   * wybieraj
   _COLOR:=SETCOLOR(if(iscolor(),"W+/GR","W,I,,,X"))

   _e:=ACHOICE(_a+1,_b+1,_c-1,_d-1,ARRAY,alog,,_e)
   
   SETCOLOR(_COLOR)

   RESTSCREEN(_a,_b,_c,_d,_s)
   setpos(r,c)
   setcursor(_cur)

   IF _e=0 
      RETURN .F.
   ENDIF
   sx:=at("*",array[_e])-1
   if sx<0
      sx:=sl
   endif
   sp:=at("?",array[_e])
   if sp>sx
      sp:=0
   endif
ENDIF
   r:=.t.
   IF ARRAY[_e]#var
      _a:=array[_e]
      if sp>0
         _a:=stuff(_a,sp,1,SubStr(var,sp,1))
      endif
      if sx=sl
         var:=LEFT(_a,_l)
      else
         var:=stuff(var,1,sx,left(_a,sx))
      endif
      if _get#NIL .and. !_get:buffer==var
         _get:buffer:=var
         _get:VARPUT(_get:UNTRANSFORM())
         updated(@r)
      endif
   ENDIF

RETURN r   // teraz ok
**************
static proc ins(a,i,c)
   asize(a,len(a)+1)
   ains(a,i)
   //a[i]:=array(len(a[i+1]))
   a[i]:=aclone(a[i+1])
   aeval(a[i+1],{|x,y|a[i,y]:={NIL,'',ctod(''),0,.f.}[1+at(valtype(x),'CDNL')]},c)
return
**************
static proc add(a,i,c)
local j
   if ascan(atail(a),{|x|!empty(x)},c)=0
      return
   endif
   j:=len(a)+1
   asize(a,j)
   a[j]:=aclone(a[i])
   aeval(a[i],{|x,y|a[j,y]:={NIL,'',ctod(''),0,.f.}[1+at(valtype(x),'CDNL')]},c)
return
***************
static proc del(b,a,i)
local j,z
   z:=len(a)-1
   if z=0
      aeval(a[1],{|x,y|a[1,y]:={NIL,'',ctod(''),0,.f.}[1+at(valtype(x),'CDNL')]})
   elseif i>z
      b:up()
      b:forcestable()
      asize(a,z)
   else
      adel(a,i)
      asize(a,z)
      b:refreshall()
      b:forcestable()
   endif
return
*************
static function mkblock(j)
return {||_a[j]}
*************
static proc do_enter(b,a,i,c,n,p,v,w,k)
local j,x
             b:forcestable()
             j:=b:colpos-1
             if valtype(w[j])='L' .and. !w[j]
                k:= - K_TAB
                return
             endif
             if !empty(k) .and. k#K_ENTER
                kibord({k})
             endif
             k:=_GET_( a[i,j],'a[i,j]', p[j], v[j], w[j] )
             if valtype(a[i,j])='C'
                if empty(k:picture)
                   k:picture:="@KS"
                endif
                x:=(b:GetColumn(j+1)):Width
                if right(k:picture,1)='S'
                   k:picture+=lTrim(sTr(x))
                endif
                if "S"$k:picture
                   k:cargo:=.t.
                endif
                if len(a[i,j])<x
                   a[i,j]:=pad(a[i,j],x)
                endif
             endif

             set(_SET_EXIT,.t.)
             begin sequence
             readmodal({k})
             k:=lastkey()
             if k=K_ENTER
                k:=K_TAB
             endif
             k := - k
             recover
                k:=0
             end sequence
             //_a:=a[i]
             b:refreshcurrent()
             set(_SET_EXIT,.f.)
return
*************
FUNCTION ARREDIT(a,n,p,v,w,r,t,tb)
local saved_a:=_a,i,win,b,j,k,c,x,fr:=1
j:=len(a[1])

if j=0
   return .f.
endif

DEFAULT p TO {}
DEFAULT v TO {}
DEFAULT w TO {}
DEFAULT r TO 1
DEFAULT tb TO {||NIL}
asize(p,j)
asize(v,j)
asize(w,j)

i:=max(5,len(a))
if t<>NIL
   i+=2
endif
if valtype(n)='A'
  i+=2
  asize(n,j)
else
  n:=array(j)
endif
_a:=a[1]
 aeval(_a,{|x,y|if(x=NIL,x:=_a[y]:=space(5),),j+=max(if(n[y]=NIL,0,len(n[y])),len(Tran(x,p[y])))})

win:=window(i,j)

       i:=1
       b:=tbrowsenew(win[1]+1,win[2]+1,win[3]-1,win[4]-1)
       b:colsep:=hb_UTF8ToStrBox('│')
       if valtype(n)='A'
          b:headsep:=hb_UTF8ToStrBox('┬─')
       endif
       b:gotopblock:={||i:=1,_a:=a[i],i}
       b:gobottomblock:={||i:=len(a),_a:=a[i],i}
       b:skipblock:={|n,l|l:=i,i+=n,i:=max(1,min(i,len(a))),_a:=a[i],i-l}
       if t<>NIL
          b:footsep:=hb_UTF8ToStrBox('┴─')
       endif

       c:=tbcolumnnew(,{||i})
       c:picture:='##'
       c:heading:='Lp'
       b:addcolumn(c)


       for j:=1 to len(a[1])
         c:=tbcolumnnew(,mkblock(j))
         if valtype(n)='A'
           c:heading:=n[j]
         endif
         c:picture:=p[j]
         c:width:=max(if(n[j]=NIL,0,len(n[j])),len(Tran(a[1,j],p[j])))
         if t<>NIL
            c:Footing:=Space(c:width)
         endif
         b:addcolumn(c)


             if valtype(x:=v[j])='C'
               if x<>'{|'
                  v[j]:=hb_macroBlock(x) 
               else
                  v[j]:=&x
               endif
             endif

             if valtype(x:=w[j])='C'
               if x<>'{|'
                  w[j]:=hb_macroBlock(x) 
               else
                  w[j]:=&x
               endif
             endif

             if valtype(w[j])='L' .and. w[j]
                w[j]:=NIL
             endif

       next j

       for j:=1 to len(a[1])
           if valtype(w[j])='L'
              Loop
           endi
           fr:=b:freeze:=j
           b:colpos:=j+1
           Exit
       next j

       c:=array(31)

       c[K_UP]       :={|b|b:up()}
       c[K_LEFT]     :={|b|if(b:colpos>fr+1,b:left(),)}
       c[K_TAB]      :={|b|b:right()}
       c[K_RIGHT]    :={|b|b:right()}
       c[K_PGUP]     :={|b|b:pageup()}
       c[K_PGDN]     :={|b|b:pagedown()}
       c[K_CTRL_PGUP]:={|b|b:gotop()}
       c[K_CTRL_PGDN]:={|b|b:gobottom()}
       c[K_END]      :={|b|b:end()}
       c[K_HOME]     :={|b|b:colpos:=fr+1,b:refreshcurrent()}
       c[K_ENTER]    :={|b,a,i,c,n,p,v,w,k|do_enter(b,a,i,c,n,p,v,w,@k)}
       if valtype(n)='A'
       c[K_DEL]      :={||del(b,a,i)}
       c[K_INS]      :={||ins(a,i,r),b:RefreshAll()}
       c[K_DOWN]     :={|b|if(i=len(a),(add(a,i,r),b:dehilite(),b:ColPos:=r+1),),b:down()}
       else
       c[K_DOWN]     :={|b|b:down()}
       endif

       k:=0

       j:=eval(tb,b,a,i,0,c,n,p,v,w,@k)

       if t<>NIL .and. j<>NIL
          x:=(b:GetColumn(t+1))
          if .not.( x:Footing == (x:Footing:=Tran(j,x:picture)) )
             b:SetColumn(t+1,x)
          endif
       endif

       b:hilite()

       do while .t.
          if k<>0
          elseif b:stable
             k:=inkey(0)
          else
             b:stabilize()
             k:=inkey()
             if k=0
                loop
             endif
          endif

          if k=K_SH_TAB
             k:=K_LEFT
          endif

          if k=K_ESC .or. k=K_F10 .or. k=K_CTRL_L
             //b:forcestable()
             exit
          endif

          x:=k
          k:=0

          if x>0 .and. x<32 .and. c[x]#NIL .or. !hb_keyChar(x)==""
             eval(c[if(x>31,K_ENTER,x)],b,a,i,c,n,p,v,w,@x)
             if x<0 .and. x>-31
                k:=-x
             endif
          elseif (x:=setkey(x))<>NIL
             b:forcestable()
             eval(x,procname(0),b,a,i,c,n,p,v,w)
          endif
          _a:=a[i]
          j:=eval(tb,b,a,i,1,c,n,p,v,w,@k)
          if t<>NIL .and. j<>NIL
             x:=(b:GetColumn(t+1))
             if .not.( x:Footing == (x:Footing:=Tran(j,x:picture)) )
                b:SetColumn(t+1,x)
             endif
          endif
       enddo

       For j:=Len(a) TO 1 step -1
         if Len(a)=1
            Exit
         endif
         if ascan(a[j],{|x|!empty(x)})=0
            adel(a,j)
            asize(a,len(a)-1)
         endif
       next j

window(win)

_a:=saved_a
RETURN k<>K_ESC
#endif
*******************
#ifdef A_MYSZ
proc __atprompt(row,col,pro,msg)

aadd(apro,{int(row),int(col),pro,msg})
@ row,col say pro color SubStr(setcolor(),rat(",",setcolor())+1)

return
*******************
func __menuto(mcb,varname)

local p,b,_p,x:=0,y:=0,clr,sel,unsel,key,ckey,mesbuf,mesrow:=set(_SET_MESSAGE),mescent:=set(_SET_MCENTER),crsr,array,fpass:=.t.,bkey

if empty(apro)
   return 0
endif

   array:=aclone(apro)
   apro:={} //nesting
   p:=eval(mcb)
   if p=NIL
      p:=1
   else
      p:=min(max(1,p),len(array))
   endif
   clr:=setcolor()
   crsr:=setcursor()
   sel:=SubStr(clr,at(",",clr)+1)
   unsel:=SubStr(clr,rat(",",clr)+1)
   do while .t.

      if mesrow#NIL .and. array[p,4]#NIL
         mesbuf:=savescreen(mesrow,0,mesrow,maxcol())
         @ mesrow,if(SET(_SET_MCENTER)=.t.,(maxcol()-len(array[p,4]))*.5,0) say array[p,4] color clr
      endif

      @ array[p,1],array[p,2] say array[p,3] color sel

      if key#K_ENTER
         MSHOW()
         b:=0
         do while (key:=inkey(0, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN + HB_INKEY_GTEVENT ),key>=K_MINMOUSE .and. key<=K_MAXMOUSE )
             if key=1002
                b:=1
             elseif key=1004 .or. key=HB_K_CLOSE
                b:=2
             Else
                loop
             endif
             x:=mcol()
             y:=mrow()
             key:=0
             fpass:=.f.
            _p:=ascan(array,{|pr|y=pr[1] .and. x>=pr[2] .and. x<pr[2]+len(pr[3])})
            if b=2 .or. b=1 .and. _p=0
               if fpass
                  loop
               endif
               key:=K_ESC
            endif
            exit
         enddo
         MHIDE()

      endif
      if (bkey:=setkey(key))#NIL
         eval(mcb,p)
         eval(bkey,procname(1),procline(1),varname,{b,x,y})
         key:=0
         loop
      elseif key==K_ENTER
         exit
      elseif key==K_ESC
         p:=0
         exit
      endif
   
      if mesbuf#NIL
         restscreen(mesrow,0,mesrow,maxcol(),mesbuf)
         mesbuf:=NIL
      endif

      @ array[p,1],array[p,2] say array[p,3] color unsel

      if key==K_UP .or. key==K_LEFT
         p:=max(1,p-1)
      elseif key==K_DOWN .or. key==K_RIGHT
         p:=min(len(array),p+1)
      elseif key==K_HOME
         p:=1
      elseif key==K_END
         p:=len(array)
      elseif key==0
         if _p#0
            p:=_p
         endif
         if b=1
            key:=K_ENTER
         endif
      elseif (ckey:=upper(chr(key)),key:=ascan(array,{|pr|upper(ltrim(pr[3]))=ckey}))#0
         p:=key
         key:=K_ENTER
      endif

   enddo

   setcursor(crsr)

return p
***************
func ACHOICE(r1,c1,r2,c2,a1,a2,ub,p)

local key,b,x,y,ckey,fpass:=.t.,o,c,ld:=1,bkey

c:=tbcolumnnew("",{||a1[p]})
c:width:=c2-c1+1
if valtype(a2)="L"
if !a2
   c:colorblock:={||{5,1}}
endif
elseif valtype(a2)="A"
   c:colorblock:={||if(a2[p],{1,2},{5,2})}
else
   a2:=.t.
endif

o:=tbrowsenew(r1,c1,r2,c2)
o:gobottomblock:={||p:=len(a1)}
o:gotopblock:={||p:=1}
o:skipblock:={|s,old|old:=p,p:=min(max(1,p+s),len(a1)),p-old}

o:addcolumn(c)

y:=min(max(p,1),len(a1))
//o:RowPos:=min(p,o:RowCount)
p:=1
o:gotop()
for x:=2 to y
  o:down()
  o:stabilize()
next

do while .t.
      b:=0
      o:forcestable()

      do while b<3 .and. valtype(a2)="A" .and. !a2[p]
         if ld>0
            x:=ascan(a2,.t.,p+1,len(a2)-p)
            if x#0
               for y=p+1 to x
                 o:down()
                 o:stabilize()
               next
               exit
            endif
         else
            for x:=p-1 to 1 step -1
              if a2[x]
                 exit
              endif
            next
            if x>0
               for y=p-1 to x step -1
                 o:up()
                 o:stabilize()
               next
               exit
            endif
         endif
         ++b
         Ld*=-1
      enddo

      o:forcestable()

      MSHOW()

      b:=0
      key:=inkey(0,INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)
      if key=1002
         b:=1
         key:=14
         y:=mrow()
         x:=mcol()
      elseif key=1004
         b:=2
         key:=14
      endif
      fpass:=.f.
      MHIDE()

      do case
         case (bkey:=setkey(key))#NIL
           eval(bkey,procname(1),procline(1),{b,x,y})
         case key==14
           if b=2 .or. x<c1 .or. x>c2 .or. y>r2+1 .or. y<r1-1
              if !fpass
                p:=0
                exit
              endif
           elseif y=r1-1
              while o:rowpos>1
                o:up()
                o:stabilize()
              enddo
              o:up()
              ld:=-1
           elseif y=r2+1
              while o:rowpos<o:rowcount
                o:down()
                o:stabilize()
              enddo
              o:down()
              ld:=1
           elseif valtype(a2)="L" .and. a2 .or. valtype(a2)="A" .and. a2[p]
              p+=y-row()
              exit
           endif
         case key==K_DOWN
              o:down()
              ld:=1
         case key==K_UP
              o:up()
              ld:=-1
         case key==K_PGUP
              o:pageup()
              ld:=-1
         case key==K_PGDN
              o:pagedown()
              ld:=1
         case key==K_CTRL_PGUP
              o:gotop()
              ld:=1
         case key==K_CTRL_PGDN
              o:gobottom()
              ld:=-1
         case key==K_ENTER .and. ( valtype(a2)="L" .and. a2 .or. valtype(a2)="A" .and. a2[p] )
              exit
         case key==K_ESC
              p:=0
              exit
         case (ckey:=upper(chr(key)),key:=ascan(a1,{|pr|upper(ltrim(pr))=ckey}))#0
              p:=key
              if valtype(a2)="L" .and. a2 .or. valtype(a2)="A" .and. a2[p]
                exit
              endif
      endcase

enddo

return p

#endif
***************
