//#define SIMPLE
#include "getexit.ch"
#include "inkey.ch"
#include "set.ch"
#include "dbinfo.ch"
#ifdef SIMPLE
#undef A_MYSZ
#else
#endif

#ifdef A_DRUKCOMP
#ifndef A_FA
#define A_FA
#endif
#endif

#ifdef A_HBGET
  static oed:=NIL
  static win:=NIL
#endif
static emptyprn:=NIL
memvar oprn
#ifdef A_XPRN
memvar  p_rown,p_cpi,p_pcl,P_4XON,P_4XOFF,P_COLN,P_BON,P_BOFF,P_UON,P_UOFF,;
        P_36LPI,P_12LPI,P_8LPI,P_7LPI,P_6LPI,P_SUPON,P_SUPOFF,p_margin,P_PON,;
        P_LPI,P_POFF,P_HALFPAGE,landscape,p_port,p_land,p_eject,p_rownl,p_rownp,;
        p_init,p_colnl,p_col
#endif
*******************************
func openorcreate(a,s,k,ar)
local b
select (ar)
if .not. lower(alias())==lower(a)
  b:=findfile(a+'.dbf')
if !empty(b)
   nuse (b)
else
   b:=findfile(s+'.dbf')
   nuse (b)
   copy structure to (a)
   nuse (a)
endif
endif
if k#NIL
   if empty(ordbagname(1))
     b:=findfile(a+ordbagext())
     if !empty(b)
      set index to (b)
     else
      ordcreate(,a,k,{||&k})
     endif
   endif
   set order to 1
endif
return .t.
*******************************
stat func icv(b,o)
local t
     t:=valtype(b)
#ifdef __HARBOUR__
     if ! t$'PB'
        o:=hb_ValToExp(b)
#else
     if t$"MC"
        if ! "'" $ b
           o:="'"+b+"'"
        elseif ! '"' $ b
           o:='"'+b+'"'
        else
           o:='['+b+']'
        endif
     elseif t="N"
        o:=ltrim(str(b))
     elseif t="D"
        o:="ctod('"+dtoc(b)+"')"
     elseif t="L"
        o:=if(b,".t.",".f.")
     elseif t="A"
        t:=''
        begin sequence
          aeval(b,{|x|t+=","+icv(x)})
          o:="{"+substr(t,2)+"}"
        recover
          if o=NIL
             break
          endif
        end
     elseif t='U'
        o:='NIL'
#endif
     elseif pcount()<2
        break
     endif
return o
/*****************
x:="magazyn.ini";do while inirest(@x);(&x,x:=NIL);enddo
****************/
stat func extractleft(c)
local b,d,y,r
    //c:=strtran(a,' ')
    b:={{'"',0,'"'},{"'",0,"'"},{'[',0,']'}} //,{'{',,'}'},{'(',,')'}}
    while .t.
      aeval(b,{|x,y|x[2]:=hb_at(x[1],c,x[2])})
      asort(b,,,{|x,y|y[2]=0 .or. x[2]<>0 .and. x[2]<y[2]})
      y:=b[1]
      if y[2]=0
         exit
      endif
      if y[1]='['.and.y[2]>1.and.(isalpha(r:=subs(c,y[2]-1,1)).or.isdigit(r).or.r$'_}')
         y[2]++
         LOOP
      endif
      d:=(hb_at(y[3],c,y[2]+1)-y[2]+1)
      if d<2
       (d:=errornew(),;
       d:description:="Syntax error",;
       d:operation:=y[3],;
       d:subsystem:="PPR",;
       d:subcode:=1003,;
       d:severity:=2,;
       eval(errorblock(),d))
       d:=0
      endif
      c:=stuff(c,y[2],d,'')
      aeval(b,{|y|if(y[2]>0,y[2]-=d,)},2)
    enddo
    // probuje usunąć funkcje i tablice itp
    d:=0
    while !empty(d:=hb_at(']',c,1+d))
       y:=rat('[',left(c,d))
       while y>1 .and. isalpha(r:=subs(c,y-1,1)) .or. isdigit(r) .or. r='_'
          --y
       enddo
       c:=stuff(c,y,b:=d-y+1,'')
       d-=b
    enddo
    d:=0
    while !empty(d:=hb_at(')',c,1+d))
       y:=rat('(',left(c,d))
       while y>1 .and. isalpha(r:=subs(c,y-1,1)) .or. isdigit(r) .or. r='_'
          --y
       enddo
       c:=stuff(c,y,b:=d-y+1,'')
       d-=b
    enddo
    d:=0
    while !empty(d:=hb_at('}',c,1+d))
       y:=rat('{',left(c,d))
       c:=stuff(c,y,b:=d-y+1,'')
       d-=b
    enddo
    // teraz dopiero przecinek oddziela mi zmienne
    b:=rat(':=',c)
    if b>1
       c:=strtran(left(c,b-1),':=',',')
    endif
    b:=getlines(c,',')
    for d:=len(b) To 1 step -1
       c:=b[d]:=alltrim(b[d])
       y:=len(c)
       while y>0 .and. (isalpha(r:=subs(c,y,1)) .or. isdigit(r) .or. r='_')
          --y
       enddo
       if c=='' .or. y>0
          adel(b,d)
          asize(b,len(b)-1)
       endif
    next d
return b
***************************
function inirestold(x)
static a,l,i
if a=NIL
   x:=findfile(x)
   if ""#x
      a:=memoread(x)
#ifndef __HARBOUR__
      i:=at(chr(10),a)
      if i>1 .and. subs(a,i-1,1)=chr(13)
         i:=chr(13)+chr(10)
      else
         i:=chr(10)
      endif
#endif
      a:=getlines(a,i)
      i:=0
      l:=len(a)
   else
      i:=l:=0
   endif
endif
do while .t.
   ++i
   if i>l
      x:=a:=NIL
      return .f.
   endif
   x:=a[i]
   if x='&:'
      x:=subs(x,3)
   elseif '&:'$x
      x:=Trim(left(x,at('&:',x)-1))
   endif
   if x#';'
      exit
   endif
enddo
return .t.
****************
function inirest(x)
local a,l:=0,i,j,c,y

   x:=findfile(x)
   if ""#x
      a:=memoread(x)
#ifndef __HARBOUR__
      i:=at(chr(10),a)
      if i>1 .and. subs(a,i-1,1)=chr(13)
         i:=chr(13)+chr(10)
      else
         i:=chr(10)
      endif
#endif
      a:=getlines(a,i)
      l:=len(a)
   endif

for i:=1 to l
   x:=a[i]
   if empty(x) .or. x=';' 
      loop
   elseif x='&:'
      x:=subs(x,3)
   elseif '&:'$x
      x:=Trim(left(x,at('&:',x)-1))
   endif
    c:=extractleft(x)

    if !empty(c)
#ifdef __HARBOUR__
    __mvPublic(c)
#else
    for j:=1 to len(c)
      y:=c[j]
      PUBLIC &y
    next j
#endif
    endif

    begin sequence
    (&x,x:=NIL)
    end sequence
next

return .f.
/***********
function inirest(x)
static a,l,i
if a=NIL
   x:=findfile(x)
   if ""#x
      a:=memoread(x)
#ifndef __HARBOUR__
      i:=at(chr(10),a)
      if i>1 .and. subs(a,i-1,1)=chr(13)
         i:=chr(13)+chr(10)
      else
         i:=chr(10)
      endif
#endif
      a:=getlines(a,i)
      i:=0
      l:=len(a)
   else
      i:=l:=0
   endif
endif
do while .t.
   ++i
   if i>l
      x:=a:=NIL
      return .f.
   endif
   x:=a[i]
   if x='&:'
      x:=subs(x,3)
   elseif '&:'$x
      x:=Trim(left(x,at('&:',x)-1))
   endif
   if x#';'
      exit
   endif
enddo
return .t.
**************/
procedure inisave(name)
local i,txt,j,b,c
   name:=findfile(name)
   txt:=memoread(name)

#ifndef __HARBOUR__
      i:=at(chr(10),txt)
      if i>1 .and. subs(txt,i-1,1)=chr(13)
         i:=chr(13)+chr(10)
      else
         i:=chr(10)
      endif
#endif
   txt:=getlines(txt,i)
   for i:=1 to len(txt)
     if txt[i]#';' .and. txt[i]#'&:'
        j:=at(":=",txt[i])
        if j=0
           loop
        endif
        b:=subs(txt[i],j+2)
        if '&:' $ b
          b:=subs(b,at('&:',b)+2)
          txt[i]:=left(txt[i],j+1)+icv(&b,b)+' &:'+b
          STORE &b TO &(left(txt[i],j-1))
        else
          txt[i]:=left(txt[i],j+1)+icv(&(left(txt[i],j-1)),b)
        endif
     endif
   next i
   j:=''
   for i:=1 to len(txt)
     j+=txt[i]+HB_EOL()
   next i
#ifdef __HARBOUR__
   hb_memowrit(name,j)
   hb_idlestate()
#else
   memowrit(name,j)
#endif
/*
   j:=fcreate(name)
   for i:=1 to len(txt)
     fwrite(j,txt[i])
     fwrite(j,HB_EOL())
   next i
   fclose(j)
*/
return
******************************
func findfile(x,netio)
local a,l,i,y:=""
 if (HB_ps()$x)
    if file(x)
       y:=x
#ifdef __PLATFORM__UNIX
    elseif file(l:=Lower(strtran(x,'\','/')))
       y:=l
#endif
    else
       x:=subs(x,rat(HB_ps(),x)+1)
    endif
 endif
 if y==""
    a:=getlines(set(_SET_DEFAULT)+HB_OsPathListSeparator()+set(_SET_PATH),HB_OsPathListSeparator())
    l:=len(a)
    for i:=1 to l
/*
       if empty(a[i])
          y:=x
          if file('.'+HB_ps()+x)
             exit
          endif
       else
*/
          y:=a[i]+x
          if file(y)
            exit
          endif
#ifdef __PLATFORM__UNIX
          y:=strtran(a[i]+Lower(x),'\','/')
          if file(y)
             exit
          endif
#endif
//       endif
    next
    if i>l
       y:=""
    endif
 endif
#ifdef A_NETIO
 if !empty(netio) .and. y=netio
    y:='net:'+subs(y,len(netio)+1)
 endif
#endif
return y
*********************
#ifndef SIMPLE
FUNCTION PRINT(l,lpt)
static wasbad:=.t.,NCHOICE
local x,y,z,c:=set(_SET_CONSOLE),f,h

  if lpt='NUL'
     return .f.
  endif
setprc(0,0)
if l=NIL
   l:=1
else
   c:=.f.
endif
if c .and. 1#alarm("CZY DRUKOWAC ?",{"TAK","NIE"},1,2)
#ifdef A_HPDF
  #define D_HWPRN A_HPDF
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
//#ifdef D_HWPRN
   oprn:=NIL
//#endif
   set printer to
   set print off
   f:='.'+HB_ps()+lower(left(procname(l),8))+'.txt'
#ifdef A_LAN
   x:=errorblock({|e|if(e:gencode=20,break(e),eval(x,e))})
   do while .t.
   begin sequence
     SET alternate TO (f)
     errorblock(x)
   recover using y
     errorblock(x)
     if y:gencode<>20
         break(y)
     endif
     f:=set(_SET_DEFAULT)
     h:=fcreateu(@f)
     if h=-1
       if eval(x,y)
          loop
       endif
     else
       fclose(h)
       if !"."$right(f,4)
         f+='.'
       endif
       loop
     endif
   end sequence
     exit
   enddo
#else
   set alternate to (f)
#endif
   set alternate on
   return .f.
endif
#ifdef D_HWPRN
  if oprn=NIL
     oprn:=D_HWPRN
  endif
#endif
//#ifdef A_LPTN
  if lpt=NIL
//#endif
#ifdef A_GETLPT
    lpt:=A_GETLPT
#else
    lpt:=getenv("MDSLPT")
#endif
//#ifdef A_LPTN
  else
    lpt:=trim(lpt)
  endif
//#endif
#ifdef D_HWPRN
  if !empty(oprn)
#ifdef A_HPDF
    if !valtype(oprn)='O'
       oprn:=Pdf_Prn():New( LPT )
    endif
    if !oprn:Create()
       oprn:Destroy()
       oprn:=NIL
       Alarm('Błąd Drukarki')
       Return .f.
    endif
    ccpi(,4)
    setprc(0,0)
    INIT PRINTER
    if ! oprn:StartDoc()
       oprn:Destroy()
       oprn:=NIL
       Alarm('Błąd Drukarki')
       Return .f.
    endif
#endif
#ifdef A_WIN_PRN
    if !valtype(oprn)='O'
       oprn:=Win_Prn():New( LPT )
    endif
    oprn:BKMode:=1
    oprn:Landscape:=Landscape
    if !oprn:Create()
       oprn:Destroy()
       oprn:=NIL
       Alarm('Błąd Drukarki')
       Return .f.
    endif
    oprn:LeftMargin:=0
    oprn:RightMargin:=oprn:PageWidth*2
    oprn:BottomMargin:=oprn:PageHeight - 3 * oprn:LineHeight + 1
    oprn:SetFont('Courier New',12,-10,,,,255)
    oprn:TopMargin:=oprn:LineHeight
    ccpi(,4)
    INIT PRINTER
    if ! oprn:StartDoc()
       oprn:Destroy()
       oprn:=NIL
       Alarm('Błąd Drukarki')
       Return .f.
    endif
    setprc(oprn:PRow(),oprn:PCol())
#endif
    return .t.
  else
    //oprn:=NIL
    x:=set(_SET_DEFAULT,"")
    if emptyprn=NIL
      z:=SET(_SET_PRINTFILE,'',.t.)
      emptyprn:=SET(_SET_PRINTFILE)
      if ! z==emptyprn
        SET PRINTER TO (z) ADDITIVE
      endif
    endif 
    if ! (SET(_SET_PRINTFILE)==emptyprn)
      //set printer to (z) additive
      set default to (x)
      set print on
      return .t.
    endif
    set(_SET_DEFAULT,x)
  endif
#command ?  [<explist,...>]         => (WQ(),WQQ( <explist> ))
#command ?? [<explist,...>]         => WQQ( <explist> )
#endif
//wasbad:=wasbad .or. !SET(_SET_PRINTER)
x:=set(_SET_DEFAULT,"")
    if emptyprn=NIL
      z:=SET(_SET_PRINTFILE,'',.t.)
      emptyprn:=SET(_SET_PRINTFILE)
      if ! z==emptyprn
        SET PRINTER TO (z) ADDITIVE
      endif
    endif 
if SET(_SET_PRINTFILE)==emptyprn .and. !empty(lpt)
   set printer to (lpt) additive
   binmode()
   wasbad:=.t.
endif
set default to (x)
ccpi(,4)
SET PRINT ON
init printer
wasbad:=!SET(_SET_PRINTER)
setprc(0,0)

RETURN .t.
**************
proc cpadout(n,p,cpi,t)

local r:=set(_SET_PRINTER,.f.),s:=set(_SET_CONSOLE,.f.)

if s
   if t=NIL .or. t=0
     dispout(trim(n))
   else
     dispout(padr(n,p))
   endif
endif
#ifdef D_HWPRN
if valtype(oprn)='O'
   wqq(cpad(n,@p,@cpi,t))
elseif r
#else
if r
#endif
   SET PRINTER (.t.)
   qqout(cpad(n,@p,@cpi,t))
endif
set console (s)
return
**************
func cpad(n,p,cpi,t,cb,ce)
local l,a,b,c,d,e,f,g,h,lf,oc,nc
memvar p_pcl
//,p_push,p_pop
if cb=NIL
   cb:=""
endif
if ce=NIL
   ce:=""
endif
if cpi=NIL
   cpi:=ccpi()
endif
if p=NIL
   p=int(len(n)*cpi/50*3+.9)
else
   p=int(p+.1)
endif
if t=NIL
   t:=0
endif
#ifndef A_OKI4W
#ifdef A_XPRN
if p_pcl
a:=int(p*20/cpi)
b:=int(p*50/3/cpi)
c:=int(p*15/cpi)
else
#ifndef A_17CPI
a:=int(p*20/cpi)
#endif
b:=int(p*50/3/cpi)
#ifdef A_15CPI
c:=int(p*15/cpi)
#endif
endif
#else
#ifndef A_17CPI
a:=int(p*20/cpi)
#endif
b:=int(p*50/3/cpi)
#ifdef A_15CPI
c:=int(p*15/cpi)
#endif
#endif
#endif
d:=int(p*12/cpi)
e:=int(p*10/cpi)
f:=int(p*25/3/cpi)
g:=int(p*6/cpi)
h:=int(p*5/cpi)
#ifdef A_STYLUS
n:=trim(n)
l:=max(p,len(n))
n:=pad(n,l)
#else
if t%4>=2
  n:=trim(n)
  l:=max(p,len(n))
  n:=pad(n,l)
else
  n:=trim(n)
  l:=len(n)
endif
#endif
do case
   case cpi<= 5 ; oc := 1
   case cpi<= 6 ; oc := 2
   case cpi<= 9 ; oc := 3
   case cpi<=10 ; oc := 4
   case cpi<=12 ; oc := 5
   case cpi<=15 ; oc := 6
   case cpi<=17 ; oc := 7
   case cpi<=20 ; oc := 8
endcase
do case
   case p<g .and. l<=p; lf:=p; nc:=oc
   case p<f .and. l<=g; lf:=g; cpi:=6    ; nc:=2
   case p<e .and. l<=f; lf:=f; cpi:=25/3 ; nc:=3
   case l<=e; lf:=e; cpi:=10 ; nc:=4
#ifdef A_OKI4W
   otherwise; lf:=d; cpi:=12 ; nc:=5
#else
   case l<=d; lf:=d; cpi:=12 ; nc:=5
   case c<>NIL .and. l<=c; lf:=c; cpi:=15 ; nc:=6

   case a=NIL .or. l<=b; lf:=b; cpi:=50/3 ;nc:=7
   otherwise; lf:=a; cpi:=20 ;nc:=8
#endif
endcase
    l:=min(l,lf)
#ifdef D_HWPRN
    if valtype(oprn)='O'
       if nc<>oc
         ?? ccpi(nc,oc)
         oprn:TextOut(left(n,l),,.f.,24)
         ?? ccpi(oc,nc)
         return space(p)
       else
         return padr(n,lf)
       endif
    endif
#endif
    if !set(_SET_PRINTER)
      if t=NIL .or. t=0
        return(trim(n))
      else
        return(padr(n,p))
      endif
    endif
    cb+=ccpi(nc,oc)
    ce+=ccpi(oc,nc)
    if t=0 .or. t%2=1 .and. lf=p
       setprc(prow(),pcol()-len(cb+ce)-lf+p)
       return cb+padr(n,lf)+ce
    elseif t%2=1
       setprc(prow(),pcol()-len(cb+ce)-2*l)
       return cb+left(n,l)+replicate(chr(8),l)+ce+space(p)
    endif
    return cb+left(n,l)+ce
*******************
proc specout(x)
local cons,r,c

#ifdef D_HWPRN
if valtype(oprn)='O'
   spec(x)
   return
endif
#endif
if !set(_SET_PRINTER)
   return
endif

cons:=set(_SET_CONSOLE,.f.)
r:=prow()
c:=pcol()

qqout(x)
setprc(r,c)
set(_SET_CONSOLE,cons)
return
*******************
#ifdef A_ZEBRA

func DrawBarcode( cType, cCode, nFlags, ... )
  LOCAL hZebra, nLineHeight
  if valtype(oprn)<>'O'
    return cCode
  endif 

  SWITCH cType
   CASE "EAN13"      ; hZebra := hb_zebra_create_ean13( cCode, nFlags )   ; EXIT
   CASE "EAN8"       ; hZebra := hb_zebra_create_ean8( cCode, nFlags )    ; EXIT
   CASE "UPCA"       ; hZebra := hb_zebra_create_upca( cCode, nFlags )    ; EXIT
   CASE "UPCE"       ; hZebra := hb_zebra_create_upce( cCode, nFlags )    ; EXIT
   CASE "CODE39"     ; hZebra := hb_zebra_create_code39( cCode, nFlags )  ; EXIT
   CASE "ITF"        ; hZebra := hb_zebra_create_itf( cCode, nFlags )     ; EXIT
   CASE "MSI"        ; hZebra := hb_zebra_create_msi( cCode, nFlags )     ; EXIT
   CASE "CODABAR"    ; hZebra := hb_zebra_create_codabar( cCode, nFlags ) ; EXIT
   CASE "CODE93"     ; hZebra := hb_zebra_create_code93( cCode, nFlags )  ; EXIT
   CASE "CODE11"     ; hZebra := hb_zebra_create_code11( cCode, nFlags )  ; EXIT
   CASE "CODE128"    ; hZebra := hb_zebra_create_code128( cCode, nFlags ) ; EXIT
   CASE "PDF417"     ; hZebra := hb_zebra_create_pdf417( cCode, nFlags ); EXIT //nLineHeight := nLineWidth * 3 ; EXIT
   CASE "DATAMATRIX" ; hZebra := hb_zebra_create_datamatrix( cCode, nFlags ); EXIT //nLineHeight := nLineWidth ; EXIT
   CASE "QRCODE"     ; hZebra := HB_ZEBRA_CREATE_QRCODE(cCode, nFlags); EXIT
  ENDSWITCH


  IF hZebra != NIL
     IF hb_zebra_geterror( hZebra ) == 0
        hb_zebra_draw( hZebra, {| x, y, w, h | wapi_FillRect( oprn:hPrinterDC, { Int( x + .5 ), Int( y + .5 ), Int( x + .5 ) + Int( w ), Int( y + .5 ) + Int( h ) + 1 }, wapi_CreateSolidBrush( 0 ) ) }, ... )
     ENDIF
     hb_zebra_destroy( hZebra )
  ENDIF

RETURN cCode
#endif
func oprn(x)
#ifdef D_HWPRN
   if valtype(oprn)='O'
      return mkbl(x)
   endif
   return {||NIL}
#else
   return ''
#endif
*******************
static func getnum(x,i,c)
local y:='',wasdot:=.f.
DEFAULT i TO 1
for i:=i to len(x)
   c:=subs(x,i,1)
   if isdigit(c) .or. (!wasdot .and. (wasdot:=(c='.'))).or.(''=y .and. c$'+-')
      y+=c
   else
      exit
   endif
next i
return y
******************
func spec(x)
static fw:=NIL,fw4:=NIL,fs4:=NIL,fsu:=NIL
local cons,i,j,k,c,d,z
  local b:={|y,b,k,m|z:=y$x,if(z.or.(k:=(m:=left(y,3))+lower(subs(y,4)))$x,(x:=if(z,strtran(x,y),strtran(x,k,m)),.t.),.f.)}
#ifdef D_HWPRN
while valtype(oprn)='O' .and. ""<>x

#ifdef A_WIN_PRN
   if eval(b,p_bon)
      oprn:Bold(700)
      if !z
        loop
      endif
   elseif eval(b,p_boff)
      oprn:Bold(400)
      if !z
        loop
      endif
   endif
#else
   if eval(b,p_bon)
      oprn:Bold(.t.)
      if !z
        loop
      endif
   elseif eval(b,p_boff)
      oprn:Bold(.f.)
      if !z
        loop
      endif
   endif
#endif
   if eval(b,p_uon)
      oprn:Underline(.t.)
      if !z
        loop
      endif
   elseif eval(b,p_uoff)
      oprn:Underline(.f.)
      if !z
        loop
      endif
   endif


   if eval(b,p_4xon) .and. fs4=NIL
      fw4:=oprn:FontWidth
      fs4:=oprn:FontPointSize
      oprn:SetFont(,fs4*2,if(empty(fw4[1]),,{fw4[1]*2,fw4[2]}))
      if !z
        loop
      endif
   elseif eval(b,p_4xoff) .and. fs4<>NIL
      oprn:SetFont(,fs4,fw4)
      fs4:=NIL
      if !z
        loop
      endif
   endif
   if eval(b,p_supon) //.and. fsu=NIL
      fsu:=oprn:FontPointSize
      oprn:SetFont(,fsu*7/12)
      if !z
        loop
      endif
   elseif fsu<>NIL .and. eval(b,p_supoff)
      oprn:SetFont(,fsu)
      fsu:=NIL
      if !z
        loop
      endif
   endif
   if eval(b,p_pon)
      if fw=NIL
        fw:=oprn:FontWidth
      endif
#ifdef A_WIN_PRN
      oprn:SetFont('Arial',,{0,0},,,,255)
#else
      oprn:SetFont('Helvetica',,{0,0},,,,255)
#endif
      if !z
        loop
      endif
   elseif eval(b,p_poff) // .and. fw<>NIL
#ifdef A_WIN_PRN
      oprn:SetFont('Courier New',,fw,,,,255)
#else
      oprn:SetFont('Courier',,fw,,,,255)
#endif
      fw:=NIL
      if !z
        loop
      endif
   endif

   i:=at(chr(27)+'(s',x)
   if i>0
      j:=i+3
      c:=val(d:=getnum(x,@j,@k))
      k:=lower(k)
      if k$'vh'
        if k='v'
          if fw=NIL //fixedwidth
             k:=oprn:FontWidth
          else
             k:={0,0} //proporcjonalna
          endif
          oprn:SetFont(,c,k,,,,255)
        elseif k='h'
          if c=16.67
             k:={3,-50}
          elseif c=8
             k:={3,-25}
          else
             k:=-c
          endif
          oprn:SetFont(,,k,,,,255)
          fw:=NIL
        endif
        if isupper(subs(x,j,1))
           x:=stuff(x,i,j-i+1,'')
        else
           x:=stuff(x,i+3,j-i-2,'')
           loop
        endif
      endif
   endif

   i:=at(chr(27)+'&'+'a',x)
   if i>0
      j:=i+3
      c:=val(d:=getnum(x,@j,@k))
      k:=lower(k)
      if k='l'
        k:=oprn:PosX-oprn:Leftmargin
        oprn:Leftmargin:=oprn:CharWidth*c
        oprn:PosX:=oprn:Leftmargin+k
        if isupper(subs(x,j,1))
           x:=stuff(x,i,j-i+1,'')
        else
           x:=stuff(x,i+3,j-i-2,'')
           loop
        endif
      elseif k='c'
        if d<'.' // plus minus relatywnie
           oprn:PosX+=oprn:CharWidth*c
        else
           oprn:PosX:=oprn:CharWidth*c
        endif
        if isupper(subs(x,j,1))
           x:=stuff(x,i,j-i+1,'')
        else
           x:=stuff(x,i+3,j-i-2,'')
           loop
        endif
      endif
   endif

   i:=at(chr(27)+'&'+'l',x)
   if i>0
      j:=i+3
      c:=val(d:=getnum(x,@j,@k))
      k:=lower(k)
      if k$'cd'
         if k='c'
            c:=48/c
         endif
         oprn:LineHeight:=Int(oprn:PixelsPerInchY/c)
         oprn:SetFont(,74/c)
        if isupper(subs(x,j,1))
           x:=stuff(x,i,j-i+1,'')
        else
           x:=stuff(x,i+3,j-i-2,'')
           loop
        endif
      elseif k='h' //paper source
        //if !oprn:Printing
        oprn:Destroy()
        oprn:BinNumber:=c
        oprn:Create()
        oprn:BottomMargin:=oprn:PageHeight - 3 * oprn:LineHeight + 1
        oprn:TopMargin:=oprn:LineHeight
        oprn:StartDoc()
        setprc(oprn:PRow(),oprn:PCol())
        if isupper(subs(x,j,1))
           x:=stuff(x,i,j-i+1,'')
        else
           x:=stuff(x,i+3,j-i-2,'')
           loop
        endif
      endif
   endif

   for i:=1 To len(x)
      c:=asc(subs(x,i,1))
   if c=13
      qqout(chr(c))
      oprn:PosX := oprn:LeftMargin
      setprc(oprn:pRow(),oprn:pCol())
   elseif c=12
      qqout(chr(c))
#ifdef A_STOPKA
      c:={oprn:fontName,oprn:FontPointSize,oprn:FontWidth}
      oprn:setfont('Arial',8,{0,0},,,,255)
      oprn:Line( oprn:LeftMargin, oprn:BottomMargin -oprn:LineHeight, oprn:RightMargin, oprn:BottomMargin - oprn:LineHeight )
      oprn:TextOutAt(oprn:LeftMargin ,oprn:BottomMargin ,A_STOPKA,,,24)
      oprn:setfont(c[1],c[2],c[3],,,,255)
#endif
      oprn:NewPage(.t.)
      setprc(oprn:Prow(),oprn:Pcol())
   elseif c=10
      qqout(chr(c))
      c:=oprn:PosX
      if oprn:PosY+2*oprn:LineHeight>oprn:BottomMargin
#ifdef A_STOPKA
      c:={oprn:fontName,oprn:FontPointSize,oprn:FontWidth,c}
      oprn:setfont('Arial',8,{0,0},,,,255)
      oprn:Line( oprn:LeftMargin, oprn:BottomMargin -oprn:LineHeight, oprn:RightMargin, oprn:BottomMargin - oprn:LineHeight )
      oprn:TextOutAt(oprn:LeftMargin ,oprn:BottomMargin ,A_STOPKA,,,24)
      oprn:setfont(c[1],c[2],c[3],,,,255)
      c:=c[4]
#endif
        oprn:NewPage(.t.)
      else
        oprn:NewLine()
      endif
      oprn:PosX:=c
      setprc(oprn:Prow(),oprn:Pcol())
   elseif c=9
      qqout(chr(c))
      oprn:setprc(oprn:Prow(),int((oprn:PCol()+8)/8)*8)
      setprc(oprn:Prow(),oprn:Pcol())
   elseif c=8
      qqout(chr(c))
      oprn:PosX:=max(oprn:LeftMargin,oprn:PosX-oprn:CharWidth)
      setprc(oprn:Prow(),oprn:Pcol())
   elseif c=32
      oprn:TextOut(' ',,,24)
   elseif c=95
      oprn:TextOut('_',,,24)
   endif
   next i
   return ''
enddo
#endif
if !set(_SET_PRINTER)
   return ''
endif
cons:=set(_SET_CONSOLE,.f.)
setprc(prow(),pcol()-len(x))
set(_SET_CONSOLE,cons)
return x
***************************
proc p_reset
#ifdef A_XPRN
local txt

public  p_rown,p_cpi,p_pcl,P_4XON,P_4XOFF,P_COLN,P_BON,P_BOFF,P_UON,P_UOFF,;
        P_36LPI,P_12LPI,P_8LPI,P_7LPI,P_6LPI,P_SUPON,P_SUPOFF,p_margin,P_PON,;
        P_LPI,P_POFF,P_HALFPAGE,landscape,p_port,p_land,p_eject,p_rownl,p_rownp,;
        p_init,p_colnl,p_col
//        ,p_push,p_pop

   landscape:=!empty(landscape)
   p_rown :=58
   p_rownl:=40
   p_colnl:=113
   if p_pcl=.t.
      P_INIT  := {|x|if(x,chr(0x1B)+"(17U"+chr(0x1B)+"&"+"l26a"+if(landscape,'1','0')+"O"+chr(0x1B)+"&"+"a0L",'')+chr(0x1B)+"(s1q0s0b10h12V"}
//      P_PUSH  := chr(0x1B)+'&'+'f0S'
//      P_POP   := chr(0x1B)+'&'+'f1S'
      P_4XON  := chr(0x1B)+'(s24.0v5.0H'
      P_4XOFF := chr(0x1B)+'(s12.0v10.0H'
      P_COLN  := 78
      P_BON   := chr(0x1B)+"(s3B"
      P_BOFF  := chr(0x1B)+"(s0B"
      P_UON   := chr(0x1B)+"&"+"d0D"
      P_UOFF  := chr(0x1B)+"&"+"d@"

      P_36LPI := chr(0x1B)+'&'+'l36D'
      P_12LPI := chr(0x1B)+'&'+'l12D'
      P_6LPI  := chr(0x1B)+'&'+'l6D'
      P_7LPI  := chr(0x1B)+'&'+'l7D'
      P_8LPI  := chr(0x1B)+'&'+'l8D'
/*
      P_36LPI := chr(0x1B)+'&'+'l2C'
      P_12LPI := chr(0x1B)+'&'+'l4C'
      P_6LPI  := chr(0x1B)+'&'+'l8C'
      P_7LPI  := chr(0x1B)+'&'+'l7C'
      P_8LPI  := chr(0x1B)+'&'+'l6C'
*/
      P_SUPON := chr(0x1B)+'(s7.0V'
      P_SUPOFF:= chr(0x1B)+'(s12.0V'
      p_margin:= {|x|chr(0x1B)+'&'+'a'+ltrim(str(x,3))+'L'}
      p_col   := {|x|chr(0x1B)+'&'+'a'+ltrim(str(x,3))+'C'}

      P_PON   := chr(0x1B)+"(s1P"
      P_POFF  := chr(0x1B)+"(s0P"
      P_HALFPAGE:={||''}
      P_LPI   := {|x|chr(0x1B)+'&'+'l'+ltrim(str(x))+'D'}
      //P_PORT  := {|x|if(landscape=(landscape:=x),'',chr(0x1B)+"&l"+IF(x,"1","0")+"O"))}
      P_LAND  := {|x|if(MEMVAR->landscape=(MEMVAR->landscape:=!(x=.f.)),'',chr(0x1B)+"&"+"l"+IF(MEMVAR->landscape,"1","0")+"O")}
      p_cpi:={|n|chr(0x1B)+"(s"+{"0p5H","0p6H","0p8.33H","0p10H","0p12H","0p15H","0p16.67H","0p20H","1P"}[n]}
   else
      P_INIT  := {||chr(0x1B)+'@'+chr(0x1B)+'P'+chr(0x12)} //chr(0x1B)+'l'+chr(0)}
//      P_PUSH  := ''
//      P_POP   := ''
      P_4XON  := chr(0x1B)+'w1'+chr(0x1B)+'W1'
      P_4XOFF := chr(0x1B)+'w0'+chr(0x1B)+'W0'
      P_COLN  := 80
      P_BON   := chr(0x1B)+'G'
      P_BOFF  := chr(0x1B)+'H'
      P_UON   := chr(0x1B)+'-1'
      P_UOFF  := chr(0x1B)+'-0'
      P_36LPI := chr(0x1B)+'3'+chr(0x6)
      P_12LPI := chr(0x1B)+'3'+chr(0x12)
      P_8LPI  := chr(0x1B)+'0'
      P_7LPI  := chr(0x1B)+'1'
      P_6LPI  := chr(0x1B)+'2'
      P_SUPON := chr(0x1B)+'S1'
      P_SUPOFF:= chr(0x1B)+'T'
      p_margin:= {|x|chr(0x1B)+'l'+chr(x)}
      p_col   := {|x|chr(0x1B)+'$'+i2bin(60*x/ccpi())}
      P_PON   := chr(0x1B)+"p1"
      P_POFF  := chr(0x1B)+"p0"
      P_HALFPAGE:= {|x|chr(0x1B)+"C"+chr(x)}
      P_LPI   := {|x|x:=round(216/x,0),chr(0x1B)+'3'+chr(x)}
      P_LAND  := {||''}
#ifdef A_MSCPI
      p_cpi:={|n|chr(0x1B)+"!"+chr({32,33,36,0,1,4,4,5,2}[n])}
#else
      p_cpi:={|n,s|{{""      ,chr(0x1B)+"M"    ,chr(0xF)      ,chr(0x1B)+"W0"   ,chr(0x1B)+"W0"+chr(0x1B)+"M" ,chr(0x1B)+"W0"+chr(0x1B)+"g" ,chr(0x1B)+"W0"+chr(0xF)  ,chr(0x1B)+"W0"+chr(0xF)+chr(0x1B)+"M",chr(0x1B)+"p1"+chr(0x1B)+"W0"},;
                    {chr(0x1B)+"P"    ,""      ,chr(0x1B)+"P"+chr(0xF)    ,chr(0x1B)+"W0"+chr(0x1B)+"P" ,chr(0x1B)+"W0"   ,chr(0x1B)+"W0"+chr(0x1B)+"g" ,chr(0x1B)+"W0"+chr(0x1B)+"P"+chr(0xF),chr(0x1B)+"W0"+chr(0xF)  ,chr(0x1B)+"p1"+chr(0x1B)+"W0"},;
                    {chr(0x12)+chr(0x1B)+"P"   ,chr(0x12)+chr(0x1B)+"M"   ,""       ,chr(0x12)+chr(0x1B)+"W0"+chr(0x1B)+"P",chr(0x12)+chr(0x1B)+"W0"+chr(0x1B)+"M",chr(0x12)+chr(0x1B)+"W0"+chr(0x1B)+"g",chr(0x1B)+"W0"   ,chr(0x1B)+"W0"+chr(0x1B)+"M" ,chr(0x1B)+"p1"+chr(0x1B)+"W0"},;
                    {chr(0x1B)+"W1"   ,chr(0x1B)+"W1"+chr(0x1B)+"M" ,chr(0x1B)+"W1"+chr(0xF)   ,""      ,chr(0x1B)+"M"    ,chr(0x1B)+"g"    ,chr(0xF)     ,chr(0x1B)+"M"+chr(0xF)   ,chr(0x1B)+"p1"   },;
                    {chr(0x1B)+"W1"+chr(0x1B)+"P" ,chr(0x1B)+"W1"   ,chr(0x1B)+"W1"+chr(0x1B)+"P"+chr(0xF) ,chr(0x1B)+"P"    ,""      ,chr(0x1B)+"g"    ,chr(0x1B)+"P"+chr(0xF)   ,chr(0xF)     ,chr(0x1B)+"p1"   },;
                    {chr(0x1B)+"W1"+chr(0x1B)+"P" ,chr(0x1B)+"W1"+chr(0x1B)+"M" ,chr(0x1B)+"W1"+chr(0x1B)+"P"+chr(0xF) ,chr(0x1B)+"P"    ,chr(0x1B)+"M"    ,""      ,chr(0x1B)+"P"+chr(0xF)   ,chr(0x1B)+"M"+chr(0xF)   ,chr(0x1B)+"p1"   },;
                    {chr(0x12)+chr(0x1B)+"W1"  ,chr(0x12)+chr(0x1B)+"W1"+chr(0x1B)+"M",chr(0x1B)+"W1"    ,chr(0x12)     ,chr(0x12)+chr(0x1B)+"M"   ,chr(0x12)+chr(0x1B)+"g"   ,""      ,chr(0x1B)+"M"    ,chr(0x12)+chr(0x1B)+"p1"  },;
                    {chr(0x12)+chr(0x1B)+"W1"+chr(0x1B)+"P",chr(0x12)+chr(0x1B)+"W1"  ,chr(0x1B)+"W1"+chr(0x1B)+"P"  ,chr(0x12)+chr(0x1B)+"P"   ,chr(0x12)     ,chr(0x12)+chr(0x1B)+"g"   ,chr(0x1B)+"P"    ,""      ,chr(0x12)+chr(0x1B)+"p1"  },;
                    {chr(0x1B)+"p0"+chr(0x1B)+"W1",chr(0x1B)+"W1"+chr(0x1B)+"M" ,chr(0x1B)+"p0"+chr(0x1B)+"W1"+chr(0xF),chr(0x1B)+"p0"   ,chr(0x1B)+"M"    ,chr(0x1B)+"g"    ,chr(0x1B)+"p0"+chr(0xF)  ,chr(0x1B)+"M"+chr(0xF)   ,""      }}[s,n]}
#endif
   endif
   txt:="xprn.ini"
   do while inirest(@txt)
     begin sequence
       (&txt,txt:=NIL)
     end
   enddo
#endif
return
*******************
func ccpi(x,s)
memvar oprn
static statcpi:=4
local  o,r:=''
#ifdef A_XPRN
memvar p_cpi
#else
#ifdef A_PCL
local p_cpi:={|n|chr(0x1B)+"(s"+{"5H","6H","8.33H","10H","12H","15H","16.67H","20H","1P"}[n]}
#else
#ifdef A_MSCPI
/*
1 - 5 cpi
2 - 6 cpi
3 - 8.5 cpi
4 - 10 cpi
5 - 12 cpi
6 - 15 cpi
7 - 17 cpi
8 - 20 cpi
9 - proporcjonalny
masterselect:
 0   - normalny
+1  - 12
+2  - proportional
+4  - Condensed
+8  - Bold ..
+16 - Bold :
+32 - szeroki
+64 - Italic
+128 - Undelrline
*/
local p_cpi:={|n|"!"+chr({32,33,36,0,1,4,4,5,2}[n])}
#else
local p_cpi:={|n,s|{{""      ,"M"    ,""      ,"W0"   ,"W0M" ,"W0g" ,"W0"  ,"W0M","p1W0"},;
                    {"P"    ,""      ,"P"    ,"W0P" ,"W0"   ,"W0g" ,"W0P","W0"  ,"p1W0"},;
                    {"P"   ,"M"   ,""       ,"W0P","W0M","W0g","W0"   ,"W0M" ,"p1W0"},;
                    {"W1"   ,"W1M" ,"W1"   ,""      ,"M"    ,"g"    ,""     ,"M"   ,"p1"   },;
                    {"W1P" ,"W1"   ,"W1P" ,"P"    ,""      ,"g"    ,"P"   ,""     ,"p1"   },;
                    {"W1P" ,"W1M" ,"W1P" ,"P"    ,"M"    ,""      ,"P"   ,"M"   ,"p1"   },;
                    {"W1"  ,"W1M","W1"    ,""     ,"M"   ,"g"   ,""      ,"M"    ,"p1"  },;
                    {"W1P","W1"  ,"W1P"  ,"P"   ,""     ,"g"   ,"P"    ,""      ,"p1"  },;
                    {"p0W1","W1M" ,"p0W1","p0"   ,"M"    ,"g"    ,"p0"  ,"M"   ,""      }}[s,n]}
#endif
#endif
#endif
if pcount()=0
   return {5,6,25/3,10,12,15,50/3,20,0}[statcpi]
endif
o:=statcpi
if s#NIL
   statcpi:=s
endif
s:=o
o:=statcpi
if x#NIL .and. x#statcpi
if set(_SET_PRINTER)
   r:=eval(p_cpi,x,statcpi,statcpi:=x)
endif
#ifdef D_HWPRN
if Valtype(oprn)='O'
   statcpi:=x
#ifdef A_WIN_PRN
   oprn:SetFont(if(x=9,'Arial','Courier New'),,{-5,-6,{3,-25},-10,-12,-15,{3,-50},-20,{0,0}}[x],,,,255)
#else
   oprn:SetFont(if(x=9,'Helvetica','Courier'),,{-5,-6,{3,-25},-10,-12,-15,{3,-50},-20,{0,0}}[x],,,,255)
#endif
   r:=''
endif
#endif
endif
x:=o
return r
*******************
function cdoweek(d)
return cdow(d)
//return({"","Niedziela","Poniedziałek","Wtorek","Środa","Czwartek","Piątek","Sobota"}[1+dow(d)])
****************
#ifdef A_FA
func slownie(WARTO)
local buf,poz,i,j,form,txt
warto:=round(warto,2)
txt:=if(WARTO<0,"minus","")
buf:=ltrim(tran(abs(int(WARTO)),))
poz:=len(buf)
form:=-1

if abs(WARTO)>=1000000000000
  txt+=" "+buf
  poz:=0
endif
do while poz>0

i:=--poz%3+1
j:=val(left(buf,1))
if j>0
  do case
    case i=1
      do case
        case j=1
          form=max(1,form)
        case j<5
          form=2
        otherwise
          form=3
      endcase
    case i=2
      form=3
      if buf>"10" .and. j=1
        j=val(left(buf,2))-10
        i=4
        --poz
      endif
    otherwise
      form=3
  endcase
  if i#1 .or. form#1 .or. poz=0
     txt+=' '+{{"jeden","dwa","trzy","cztery","pięć","sześć","siedem","osiem","dziewięć"},;
           {"dziesięć","dwadzieścia","trzydzieści","czterdzieści","pięćdziesiąt","sześćdziesiąt","siedemdziesiąt","osiemdziesiąt","dziewięćdziesiąt"},;
           {"sto","dwieście","trzysta","czterysta","pięćset","sześćset","siedemset","osiemset","dziewięćset"},;
           {"jedenaście","dwanaście","trzynaście","czternaście","piętnaście","szesnaście","siedemnaście","osiemnaście","dziewiętnaście"};
           }[i,j]
  endif
  I%=3
endif
if i=1 .and. form>0 .and. poz>=3
  txt+=' '+{{"tysiąc","tysiące","tysięcy"},;
      {"milion","miliony","milionow"},;
      {"miliard","miliardy","miliardow"}}[int(poz/3),form]
  form=0
endif
buf=right(buf,poz)
enddo

if form<0
   txt+=" zero"
endif

if form<1  && tysiąc złotych
   form=3
endif

txt+=","+str(100*abs(warto)%100,3)+"/100"

return txt
********************
#endif
*******************
#ifdef A_DOKCOMP
  #ifndef A_DRUKCOMP
    #define A_DRUKCOMP
  #endif
#endif
#ifdef A_DRUKCOMP
#ifdef __HARBOUR__
#ifdef D_HWPRN
#ifdef A_WIN_PRN
EXTERNAL WIN_PRINTERGETDEFAULT,WIN_PRINTERLIST,WIN_PRINTERSETDEFAULT,WIN_PRINTFILERAW,WIN_PRINTEREXISTS,WIN_PRINTERSTATUS,WIN_PRINTERPORTTONAME
#else
#endif
proc wq(...)
memvar oprn
local c
qout() //if set(_SET_PRINTER)
if valtype(oprn)='O'
  if oprn:PosY+2*oprn:LineHeight>oprn:BottomMargin
#ifdef A_STOPKA
      c:={oprn:fontName,oprn:FontPointSize,oprn:FontWidth}
      oprn:setfont('Arial',8,{0,0},,,,255)
      oprn:Line( oprn:LeftMargin, oprn:BottomMargin -oprn:LineHeight, oprn:RightMargin, oprn:BottomMargin - oprn:LineHeight )
      oprn:TextOutAt(oprn:LeftMargin ,oprn:BottomMargin ,A_STOPKA,,,24)
      oprn:setfont(c[1],c[2],c[3],,,,255)
#endif
     oprn:NewPage(.t.)
  else
     oprn:NewLine()
  endif
  setprc(oprn:Prow(),oprn:Pcol())
endif
wqq(...)
return
proc wqq(...)
memvar oprn
if valtype(oprn)='O'
  aeval(hb_aparams(),{|x|if(valtype(x)='B',x:=eval(x),),x:=if(valtype(x)$'CDLMN',Tran(x,),''),if(''=x,,(qqout(x),oprn:TextOut(x,,,24),setprc(oprn:Prow(),oprn:Pcol())))})
else
  aeval(hb_aparams(),{|x|if(valtype(x)='B',x:=eval(x),),x:=if(valtype(x)$'CDLMN',Tran(x,),''),if(''=x,,qqout(x))})
endif
return
#else
proc wq(...)
qout()
wqq(...)
return
proc wqq(...)
  aeval(hb_aparams(),{|x|if(valtype(x)='B',x:=eval(x),),x:=if(valtype(x)$'CDLMN',Tran(x,),''),if(''=x,,qqout(x))})
return
#endif

proc wwout(...)
  local a:=HB_aparams()
  if len(a)>0
    wqq(a[1])
    aeval(a,{|x|wqq(' ',x)},2)
  endif
return
proc wout(...)
  wq()
  if pcount()>0
    wwout(...)
  endif
return
#endif
func evline(ba,jl,bx)
memvar j,oprn
//static ss:={},sp:=0
static ret:=NIL
local r,a,b,c,d,eb,y,p
eb:=errorblock({|e,x|errorblock(eb),e:operation+=" w linii "+str(jl,3)+": "+if(valtype(r)$"MC",r,""),e:candefault:=.t.,x:=eval(eb,e),if(VALTYPE(X)="L".AND.!x.and.e:severity>1,break(NIL),x)})

begin sequence
 ret:=NIL

 if jl>0
    y:=ba[jl]
 else
    y:=bx[1]
 endif
 if valtype(y)$"MC"
 if empty(y) .or. y=":" .or. y=';'
    break
 endif
 r:=alltrim(y)
 if r="|" .and. (a:=at('|',subs(r,2))) <> 0
    p:='{'+left(r,a+1)
    r:=subs(r,a+2)
 else
    p:='{||'
 endif
 if r="?"
#ifdef __HARBOUR__
#ifdef A_SIMPLE
    if r="??"
       y:=&(p+'outerr('+subs(r,4)+')}')
    else
       y:=&(p+'outerr(HB_EOL()),outerr('+subs(r,3)+')}')
    endif
#else
    if r="??"
       y:=&(p+'wwout('+subs(r,4)+')}')
    else
       y:=&(p+'wout(),wwout('+subs(r,3)+')}')
    endif
#endif
 elseif r=">"
    if r=">>"
       y:=&(p+'wqq('+subs(r,4)+')}')
    else
       y:=&(p+'wq(),wqq('+subs(r,3)+')}')
    endif
#else
#ifdef A_SIMPLE
    if r="??"
       y:=&(p+'outerr('+subs(r,4)+')}')
    else
       y:=&(p+'outerr(HB_EOL()),outerr('+subs(r,3)+')}')
    endif
#else
    if r="??"
       y:=&(p+'qqout('+subs(r,4)+')}')
    else
       y:=&(p+'qout('+subs(r,3)+')}')
    endif
#endif
#endif
 elseif r="JUMP " //.or. r="GOTO "
    a:=":"+ltrim(subs(r,6))+" "
    b:=ascan(ba,{|x|valtype(x)$"MC".and. x+' '=a})+1
    if b=1
       b:=errornew()
       b:description:="Label not found"
       b:subsystem:="PPR"
       b:subcode:=1001
       b:severity:=2
       eval(errorblock(),b)
    endif
    y:={||j:=b}
 elseif r="IF "
    c:=rat(" THEN ",r)
    a:=":"+ltrim(subs(r,c+6))+" "
    d:=rat(" ELSE ",a)
    c:=&(p+alltrim(subs(r,4,c-4))+'}')
    if d=0
       a:=ascan(ba,{|x|valtype(x)$"MC".and. x+' '=a})
       b:=jl
    else
       b:=":"+ltrim(subs(a,d+6))
       a:=left(a,d-1)
       a:=ascan(ba,{|x|valtype(x)$"MC".and. x+' '=a})
       b:=ascan(ba,{|x|valtype(x)$"MC".and. x+' '=b})
    endif
    ++a;++b
    y:={|x|j:=if(eval(c,x),a,b)}
    if a=1 .or. b=1
       b:=errornew()
       b:description:="Label not found"
       b:subsystem:="PPR"
       b:subcode:=1001
       b:severity:=2
       eval(errorblock(),b)
    endif
 elseif r='CALL '
    c:=ltrim(subs(r,6))+" "
    d:=at(" ",c)
    a:=":"+left(c,d)
    b:=ascan(ba,{|x|valtype(x)$"MC".and. x+' '=a})
    if b=0
       b:=errornew()
       b:description:="Label not found"
       b:subsystem:="PPR"
       b:subcode:=1001
       b:severity:=2
       eval(errorblock(),b)
    endif
    c:=&(p+"{"+ltrim(subs(c,d+1))+"}}") //parametry
    d:=getlines(alltrim(subs(ba[b],len(a)+1)),',')
    ++b
    y:={|x|callfunc(,b,eval(c,x),d)}

 elseif r='RETURN'
    a:=ltrim(subs(r,8))
    if empty(a)
       y:={||j:=0,NIL}
    else
       y:=&(p+'j:=0,'+a+'}')
    endif

 elseif r='TEXT'
    if len(r)=4
       c:='END TEXT'
       a:=ascan(ba,{|x|valtype(x)$"MC".and. alltrim(x)==c},jl+1)-jl-1
       if a<=0
          b:=errornew()
          b:description:="Label not found"
          b:subsystem:="PPR"
          b:subcode:=1001
          b:severity:=2
          eval(errorblock(),b)
       endif
#ifdef A_SIMPLE
       y:={||aeval(ba,{|y|fwrite(2,HB_EOL()),fwrite(2,y)},jl+1,a),j+=1+a}
#else
#ifdef D_HWPRN
       y:={||aeval(ba,{|y|wq(y)},jl+1,a),j+=1+a}
#else
       y:={||aeval(ba,{|y|qout(y)},jl+1,a),j+=1+a}
#endif
#endif
    else
       r:=ltrim(subs(r,6))
       b:=&(p+r+'}')
       if (p=='{||')
#ifdef A_SIMPLE
       y:={|x|x:=getlines(eval(b)),if(len(x)>0,fwrite(2,x[1]),NIL),aeval(x,{|y|fwrite(2,HB_EOL()),fwrite(2,y)},2)}
#else
#ifdef D_HWPRN
       y:={|x|x:=getlines(eval(b)),if(len(x)>0,wqq(x[1]),NIL),aeval(x,{|y|wq(y)},2)}
#else
       y:={|x|x:=getlines(eval(b)),if(len(x)>0,qqout(x[1]),NIL),aeval(x,{|y|qout(y)},2)}
#endif
#endif
      else
#ifdef A_SIMPLE
       y:={|x|x:=getlines(eval(b,x),x),if(len(x)>0,fwrite(2,x[1]),NIL),aeval(x,{|y|fwrite(2,HB_EOL()),fwrite(2,y)},2)}
#else
#ifdef D_HWPRN
       y:={|x|x:=getlines(eval(b,x),x),if(len(x)>0,wqq(x[1]),NIL),aeval(x,{|y|wq(y)},2)}
#else
       y:={|x|x:=getlines(eval(b,x),x),if(len(x)>0,qqout(x[1]),NIL),aeval(x,{|y|qout(y)},2)}
#endif
#endif
      endif
    endif
 elseif r='STORE' //wylicza tylko raz możliwy błąd?
    b:=rat(" TO ",r)
    IF b=0 //do ret
       //a:=&(p+SUBS(r,7)+'}')
       a:=&(SUBS(r,7))
       y:={||a}
    ELSE
    c:=subs(r,b+4)
    a:=&(SUBS(r,7,b-7))
    b:=&('{|x|'+c+':=x}')
    y:={|d|eval(b,a),IF(type(c)<>valtype(a),;
       (d:=errornew(),;
       d:description:="Variable does not exist",;
       d:operation:=c,;
       d:subsystem:="PPR",;
       d:subcode:=1003,;
       d:severity:=2,;
       eval(errorblock(),d)),),a}
    ENDIF
 elseif r='PRIVATE'
    // probuje usunąć teksty
    c:=extractleft(strtran(a:=SUBS(r,9)," ") )

    if empty(c)
       c:=NIL
    elseif len(c)=1
       c:=c[1] //clipper łyknie
    endif
    y:={'('+a+')','PRIVATE',c}
 elseif r='&STORE'
    a:=rat(" TO ",r)
    c:=subs(r,a+4)
    y:={c+':=self[2]',&(SUBS(r,8,a-8)),c}
 elseif r="&"
    y:={subs(r,2),,}
 elseif r="{" //tablica lub blok
    y:=&r
 else //if r="|"
    y:=&(p+r+'}')
/*
 else
    y:={||&r}
*/
 endif
    if jl>0
      ba[jl]:=y
    else
      bx[1]:=y
      if len(bx)>1
         bx:=bx[2]
      else
         bx:=NIL
      endif
    endif
 endif
 if valtype(y)="B"
    bx:=eval(y,bx)
 else
    ret:=y
 endif
recover using a
if a#NIL
   errorblock(eb)
   break(a)
endif
end sequence

errorblock(eb)
return ret
***************************
func makecallbl(bx)
bx:={bx}
return {|f,g|callfunc(bx,0,{g},{'getlist'})}
***************************
func callpar(c,p)
local a,b
memvar buf
    c:=':'+c+' '
    b:=ascan(buf,{|x|valtype(x)$"MC".and. x+' '=c})
    if b=0
       b:=errornew()
       b:description:="Label not found"+c
       b:subsystem:="PPR"
       b:subcode:=1001
       b:severity:=2
       return eval(errorblock(),b)
    endif
return callfunc(,b+1,p,getlines(alltrim(subs(buf[b],len(c)+1)),','))
***************************
func callsub(bx,g)
return callfunc(bx,0,{g},IF(EMPTY(G),NIL,{'getlist'}))
***************************
#ifdef __HARBOUR__
#define EVLINE self:=evline(buf,j++,@bx);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
       ;__mvPrivate(self[3]);
     ;END;
     ;bx:=&(self[1]);
   ;END
#else
#define EVLINE self:=evline(buf,j++,@bx);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
        ;bx:=Self[3];
        ;PRIVATE &bx;
     ;END;
     ;bx:=&(self[1]);
   ;END
#endif
func callfunc(bx,b,c,d) // ({'JUMP SUB'},0,{1,2,3},{'a','b','c'})
local x,y,l
memvar j,self,buf
private j:=0

if !empty(d)
#ifdef __HARBOUR__
  __mvPrivate(d)
     if valtype(c)='A'
       y:=min(len(d),len(c))
       for l:=1 to y
         x:=d[l]
         &x:=c[l]
       next l
     endif
#else
  for l:=1 to len(d)
     x:=alltrim(d[l])
     private &x
     if valtype(c)='A' .and. len(c)>=l
        &x:=c[l]
     endif
  next l
#endif
endif

IF empty(b) //zero,NIL
   EVLINE
   if j<2
      return bx
   endif
ELSE
   j:=b
ENDIF

l:=len(buf)

while j>0 .and. j<=l
   EVLINE
enddo

return bx
#else
#ifdef __HARBOUR__
proc wq(...)
qout()
wqq(...)
return
proc wqq(...)
  aeval(hb_aparams(),{|x|if(valtype(x)='B',x:=eval(x),),x:=if(valtype(x)$'CDLMNT',Tran(x,),''),if(''=x,,qqout(x))})
return
proc wwout(...)
  local a:=HB_aparams()
  wqq(a[1])
  aeval(a,{|x|wqq(' ',x)},2)
return
proc wout(...)
  wq()
  if pcount()>0
    wwout(...)
  endif
return
#endif
#endif
***************************
func getsetup(get,valid,when,subscript)
get:postblock:=valid
get:preblock:=when
get:subscript:=subscript
return get
*****************************
function findget(list,name)
name:=lower(name)
return ascan(list,{|g|lower(g:name)==name})
*********************************
function varput(list,name,value,ge) //ge ignoruj albo objekt z changed
local pos,g,v
if valtype(name)='N'
   pos:=name
else
   pos:=findget(list,name)
endif
if pos<>0
  g:=list[pos]
  v:=g:varget()
  g:varput(value)
  g:display()
  if g:postblock=NIL .or. v=g:varget()
  elseif empty(ge)
    Eval(g:postBlock, g, value, list, pos)
  elseif valtype(ge)='O' .and. ! (ge:name==g:name)
    Eval(g:postBlock, ge, value, list, pos)
  endif
endif
return value
*********************************
function varget(list,name,defa)
local pos
if valtype(name)='N'
   pos:=name
else
   pos:=findget(list,name)
endif
if pos=0
  return defa
endif
return list[pos]:varget()
********************************
function getset(get,clause,value)
if valtype(value)$'MC' .and. value='&:'
   value:=&(trim(subs(value,3)))
endif
if clause=='block'
   get:block:=value
elseif clause=='buffer'
   get:buffer:=value
elseif clause=='cargo'
   get:cargo:=value
elseif clause=='changed'
   get:changed:=value
elseif clause=='clear'
   get:clear:=value
elseif clause=='col'
   get:col:=value
elseif clause=='colorspec'
   get:colorspec:=value
elseif clause=='exitstate'
   get:exitstate:=value
elseif clause=='minus'
   get:minus:=value
elseif clause=='name'
   get:name:=value
elseif clause=='picture'
   get:picture:=value
elseif clause=='postblock'
   get:postblock:=value
elseif clause=='preblock'
   get:preblock:=value
elseif clause=='reader'
   get:reader:=value
elseif clause=='row'
   get:row:=value
elseif clause=='subscript'
   get:subscript:=value
endif
return get
********************************** SIMPLE
#endif
***********************************
function hex2N(as)
local l,i
as:=UPPER(alltrim(as))
l:=0
for i:=1 to len(as)
   l:=16*l+at(subs(as,i,1),'123456789ABCDEF')
next i
return l
************************************
function N2hex(an)
local s:=''
while an<>0
s:=subs('0123456789ABCDEF',an%16+1,1)+s
an:=int(an/16)
enddo
return s
*************************************
proc field2bin(f,d,a)
   if a=NIL
      a:=select()
   elseif valtype(a)='C'
      a:=select(a)
   endif
   if valtype(f)='C'
      f:=(a)->(fieldpos(f))
   endif
   if valtype((a)->(fieldget(f)))='C'
      d:=d2bin(d)
   endif
   (a)->(fieldput(f,d))

return
************************************
#ifdef __HARBOUR__
#ifdef A_HBGET
function __SetProc(x)
   static procnm:=''
   local y:=procnm
   if PCount()>0
     procnm:=x
   endif
return y
***************************
function KCR_U(mode,l,c)
  static spec:=.f.,b:={0,0},bp:=2,ww:=.f.,bl:='',ch:=.f.
  local k,getlist,m,n,i,j,txt

  if (mode=1 .or. mode=2)
     k:=lastkey()
     if spec
        spec:=.f.
        return 33
     elseif k=K_CTRL_Q
        spec:=.t.
        return 32
     elseif k=K_CTRL_K
       m:=message("PODAJ  (R, W);ROZKAZ:;... ")
       k:=upper(chr(inkey(0)))
       @ m[1]+1,m[2]+8 say "NAZWĘ: "
       n:=pad(MEMVAR->defa,64)
       getlist:={}
       @ m[1]+2,m[2]+2 get n picture "@KS14"
       read
       if empty(n)
       elseif k="R"
          oed:LoadFile(n)
       else
          oed:SaveFile(n)
       endif
       message(m)

    elseif k=K_ALT_B .or. k=K_ALT_X
        bp:=3-bp
        b[bp]:=oed:GetTextIndex()
        if ch
           ch:=.f.
           b[3-bp]:=b[bp]
        endif
        bl:=subs(oed:GetText( .f. ),min(b[1],b[2]),abs(b[2]-b[1]))
        hb_gtInfo( HB_GTI_CLIPBOARDDATA, bl )

    elseif k=K_ALT_K
        l:=oed:RowPos() //nRow
        c:=oed:ColPos() //nCol
        //i:=oed:nFirstRow
        //j:=oed:nFirstCol

        oed:LoadText(stuff(oed:GetText(.f.),m:=oed:GetTextIndex(),0,bl))

        //oed:nFirstRow:=i
        //oed:nFirstCol:=j
        oed:GotoPos( l, c , .t.)

        n:=len(bl)
        if b[1]>m
           b[1]+=n
        endif
        if b[2]>m
           b[2]+=n
        endif
        if abs(b[2]-b[1])#n
           ch:=.t.
        endif

    elseif k=K_ALT_E .and. !ch
         l:=oed:RowPos() //nRow
         c:=oed:ColPos() //nCol
         //i:=o:nFirstRow
         //j:=o:nFirstCol

         m:=oed:GetTextIndex()
         oed:LoadText(txt:=stuff(oed:GetText(.f.),n:=min(b[1],b[2]),abs(b[2]-b[1]),""))


         if n<m
            m-=n
            n:=mpostolc(txt,oed:nWordWrapCol,m,8,ww)
            l:=n[1]
            c:=n[2]
         endif

         //o:nFirstRow:=i
         //o:nFirstCol:=j

         oed:GotoPos( l, c , .t.)

         ch:=.t.

    elseif k=K_ALT_M .and. !ch
         l:=oed:RowPos()//:nRow
         c:=oed:ColPos() //nCol
         //i:=o:nFirstRow
         //j:=o:nFirstCol

           m:=oed:GetTextIndex()
           txt:=stuff(oed:GetText(.f.),n:=min(b[1],b[2]),abs(b[2]-b[1]),"")
           oed:LoadText(stuff(txt,m,0,bl))

           if n<m
              m-=n
              n:=mpostolc(txt,oed:nWordWrapCol,m,8,ww)
              l:=n[1]
              c:=n[2]
           endif

         //o:nFirstRow:=i
         //o:nFirstCol:=j
         oed:GotoPos( l, c , .t.)

         ch:=.t.

    elseif k=K_F2
        /*
        m:=o:GetTextIndex()
        o:lWordWrap:=.f.
        o:nWordWrapCol:=1020
        txt:=o:GetText(.f.)
        if ww:=!ww
           o:nWordWrapCol:=o:nRight-o:nLeft-1
        else
           //txt:=strtran(txt,chr(141)+chr(10))
           o:nWordWrapCol:=1020
        endif
        o:lWordWrap:=ww
        o:LoadText(txt)
        n:=mpostolc(txt,o:nWordWrapCol,m,8,ww)
        o:GotoPos( n[1],n[2], .t.)
        */
        ww:=!ww
        return 34

    elseif k=K_CTRL_END .or. k=K_F10
        k:=K_CTRL_END
        return k
    endif

  elseif mode=3
     //o:lWordWrap:=.f.
     ww:=.f.
     b:={0,0}
     bp:=2
     spec:=.f.
     return 0

  elseif mode=0
     @ win[3],win[4]-8 SAY str(l,3)+","+str(c,3) COLOR "I"
  endif

return 0
******************************
function Key_Ctrl_Ret(get)
memvar defa
local txt,i,j,k,o,osk,getlist:={}
  txt:=get:untransform()
  k:=0
  if !get:type$'MC'
     if get:type<>'N' .and. type(get:name)=get:type
        txt:=get:name
     else
        txt:=icv(txt)
     endif
     i:=1
     j:=len(txt)
     k:=max(38,j+5)
  else
     i:=mlcount(txt,maxcol()-2,8,.t.)
     for j:=1 to i
       k:=max(k,len(TRIM(memoline(txt,maxcol()-2,j,8,.t.))))
     next
     i:=max(i,6)
     k:=max(k,38)
     j:=NIL
  endif


  win:=Window(i,k,"W,,,BG+/BG")
  @ win[1],win[2]+3 SAY IF(get:type$'MC',"Wpis wielowierszowy","Kalkulator: + - * / ^ ( )") COLOR "BG+/BG"

  SetCursor(if(set(_SET_INSERT),2,1))

  i:=len(txt)
  if j=NIL
    osk:=HB_SETKEYSAVE(NIL)
    //txt:=MEMOEDIT(Trim(txt),win[1]+1,win[2]+1,win[3]-1,win[4]-1,.t.,'KCR_U',1020,8)
    **********
    oed := HBMemoEditor():New( txt,win[1]+1,win[2]+1,win[3]-1,win[4]-1,.t.,1020,8)
    oed:MemoInit( 'KCR_U' )
    oed:display()
    oEd:Edit()
    IF oEd:Changed() .AND. oEd:Saved()
       txt := oEd:GetText()
    ENDIF
    oed:= NIL
    ***************
    HB_SETKEYSAVE(osk)
  else
    if i<win[4]-win[2]-1
       txt:=pad(txt,win[4]-win[2]-1)
    endif
    @ win[1]+1,win[2]+1 GET txt PICTURE "@S"+ltrim(str(win[4]-win[2]-1))
    kibord(chr(K_END))
    read
  endif

  IF lastkey()=K_ESC
     window(win)
     win:=nil
     RETURN .t.
  ENDIF

  txt:=Trim(txt)
  i:=max(i,len(txt))

  //o:=get:original
  get:killfocus()
  if get:type$'MC'
     get:varput(pad(txt,i))
  else
     get:varput(&(txt))
  endif

  window(win)
  win:=nil
  get:setfocus()
  get:changed:=.t.
  //get:original:=o


return .t.
/*************************************
function GetListPos( x )
return x:=__GetListLast():ReadStats( 14 )
*************************************
function __SetProc(x)
static r,a
local b
  if valtype(x)='N'
     r:=procname(++x)
     a:=procname(1)
  endif
  b:=__GetListActive()
  if empty(b)
     b:=""
  else
     b:=b:cReadProcName
  endif
return x:=if(b==a,r,b)
************************************/
#endif
*********************
proc scrltxt
return
**********
proc scrllf
return
**********
proc scrlua
return
*********
proc scrlub
return
*********
func tcvt(x)
local y:=valtype(x)
if y='S'
  Return "@"
elseif y='H'
  RETURN '{=>}'
elseif y='O'
  RETURN 'Object'
endif
return tran(x,)
*********
func errorinhandler(x)
quit
return x
***********
func isega()
return .t.
************
func fcreateu(x)
return fcreate(x)
***************
proc binmode(x)
return

**************
func xfr(x,y,z)
if z=NIL
   z:=hb_blen(y)
elseif valtype(y)<>'C'
   y:=space(z)
elseif hb_blen(y)<z
   y+=space(z-hb_blen(y)) //:=pad(y,z)
endif
return fread(x,@y,z)
************

func shared()
return dbinfo(DBI_SHARED)
//return !set(_SET_EXCLUSIVE)
************
func hiattr(x)
return altattr(x,24)
************
func sysint()
return 0
/************
func tranr(x,y)

local i:=0,j:=0

if y='@'
   y:=subs(y,at(' ',y)+1)
endif
while j<len(x) .and. ++i<=len(y)
   if ! (y[i]$'ANX9#!')
      y[i]:=x[++j]
   endif
end

return left(y,i)
**************/
func getlines(txt,delim)
local a:={},i,j

if valtype(delim)='N'
    j:=delim
    delim:=NIL
endif

if delim=NIL
   if chr(13)+chr(10)$txt
     delim:=chr(13)+chr(10)
   else
     delim:=chr(10)
   endif
endif

if j#NIL
    for i:=1 to mlcount(txt,j,8,,delim)
      aadd(a,memoline(txt,j,i,8,,delim))
    next
    return a
endif

j:=1 

do while .t.
  i:=hb_at(delim,txt,j)
  if i>0
    aadd(a,substr(txt,j,i-j))
    j:=i+len(delim)
    loop
  endif
  if j<=len(txt)
    aadd(a,substr(txt,j))
  endif
  exit
enddo
return a

/*
HB_FUNC ( GETLINES )
      {
         PHB_ITEM pArray = hb_stackReturnItem();
         PHB_ITEM pLine  = hb_param( 1, HB_IT_STRING );
         char cDelimiter = (char) hb_parni(2);
         ULONG i, iOffset = 0, iIndex = 1;

         hb_reta( 0 );

         if ( cDelimiter == 0 )
         {
            cDelimiter = 13;
         }

         for( i = 0; i < pLine->item.asString.length; i++ )
         {
            if( pLine->item.asString.value[i] == cDelimiter )
            {
               hb_arraySize( pArray, iIndex );
               hb_itemPutCL( pArray->item.asArray.value->pItems + iIndex - 1, pLine->item.asString.value + iOffset , i - iOffset );
               iOffset = i + 1;
               if ( (cDelimiter == 13) && (pLine->item.asString.value[i+1] == 10 ))
               {
                  iOffset++ ;
               };
               iIndex++;
            }
         }
         if( iOffset < pLine->item.asString.length )
         {
            hb_arraySize( pArray, iIndex );
            hb_itemPutCL( pArray->item.asArray.value->pItems + iIndex - 1,  pLine->item.asString.value + iOffset , pLine->item.asString.length - iOffset );
         }

      }
*/

// wysłanie wart double do pola f lub odczyt pola


func getscrtxt(txt)
local ret:='',i,l

//#ifdef __PLATFORM__UNIX
local k:=4
l:=hb_blen(txt)/k
for i:=0 to l-1
  ret+=HB_utf8chr(bin2w(hb_bsubstr(txt,i*k+1,2)))
next i

return ret //HB_TRANSLATE(ret,'UTF8',)
/****************
#else
local k:=2
l:=len(txt)/k
for i:=0 to l-1
  ret+=subs(txt,i*k+1,1)
next i

return ret
#endif
**************/
func evaldb(...)
   local b, r, cp, aParams:=hb_AParams()
   if len(aParams)>=1
      cp:=HB_CDPSELECT(iif(used(),dbinfo(DBI_CODEPAGE),set(_SET_DBCODEPAGE)))
      aeval(aParams,{|x,i|if(valtype(x)=='C',aParams[i]:=HB_TRANSLATE(x,cp,),)})
      b:=aParams[1]
      if valtype(b)=='B'
         adel(aParams,1,.t.)
         r:=eval(b,hb_ArrayToParams( aParams ))
      else
         r:=&b
      endif
      if valtype(r)='C'
         r:=HB_TRANSLATE(r,,cp)
      endif
      HB_CDPSELECT(cp)
   endif
return r
*****************
#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#pragma ENDDUMP

#ifndef UpP
#pragma BEGINDUMP
#include "hbapicdp.h"

static const char ub[0x01F0] = "AAAAAAACEEEEIIIIDNOOOOO OUUUUY  AAAAAAACEEEEIIIIDNOOOOO OUUUUY YAAAAAACCCCCCCCDDDDEEEEEEEEEEGGGGGGGGHHHHIIIIIIIIIIIIJJKKKLLLLLLLLLLNNNNNNNNNOOOOOOOORRRRRRSSSSSSSSTTTTTTUUUUUUUUUUUUWWYYYZZZZZZSBBBBHHOCCDDDDDEEEFFGGHIIKKLLMNNOOOOOPPZSSSTTTTTUUVVYYZZZZZZ         DDDLLLNNNAAIIOOUUUUUUUUUUEAAAAAAGGGGKKOOOOZZJDDDGGH NNAAAAOOAAAAEEEEIIIIOOOORRRRUUUUSSTTYYHHNDOOZZAAEEOOOOOOOOYYLNTJDQACCLTSZ  BUVEEJJQQRRYYAAABOCDDEEEEEEEJGGGGGHHHIIILLLLMMNNNNOO  RRRRRRRRRSSJSSTTUUVVWYYZZZZ   C BEGHJKLQ  DDDTTTFLL  HH";
//0C0-2AF                       ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ
static const char uc[0x0100] = "AABBBBBBCCDDDDDDDDDDEEEEEEEEEEFFGGHHHHHHHHHHIIIIKKKKKKLLLLLLLLMMMMMMNNNNNNNNOOOOOOOOPPPPRRRRRRRRSSSSSSSSSSTTTTTTTTUUUUUUUUUUVVVVWWWWWWWWWWXXXXYYZZZZZZHTWYASSSSDAAAAAAAAAAAAAAAAAAAAAAAAEEEEEEEEEEEEEEEEIIIIOOOOOOOOOOOOOOOOOOOOOOOOUUUUUUUUUUUUUUYYYYYYYYLLVVYY";
//1E00-1EFF

HB_FUNC ( UPP )
      {
         PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

         if( pText )
         {
            PHB_CODEPAGE cdp = hb_vmCDP();
            const char * pszText = hb_itemGetCPtr( pText );
            HB_SIZE nLen = hb_itemGetCLen( pText ), n;

            char * pszBuffer = ( char * ) hb_xgrab( nLen + 1 );

            if( cdp )
            {
               if( HB_CDP_ISCUSTOM( cdp ) && cdp->wcharUpper )
               {
                  HB_SIZE nS = 0, nD = 0, nSrc = nLen;
                  HB_WCHAR wc;

                  while( HB_CDPCHAR_GET( cdp, pszText, nSrc, &nS, &wc ) )
                  {
                     HB_WCHAR wcUP = 0x20;
                     if ((wc >= 0x00C0) && (wc <= 0x02AF))
                        wcUP = ub[wc - 0x00C0];
                     else if ((wc >= 0x1E00) && (wc <= 0x1EFF))
                        wcUP = uc[wc - 0x1E00];

                     wc = (wcUP != 0x20) ? wcUP : HB_CDPCHAR_UPPER( cdp, wc );
                     
                     if( ! HB_CDPCHAR_PUT( cdp, pszBuffer, nLen, &nD, wc ) )
                     {
                        nLen += ( nSrc - nS + 2 );
                        pszBuffer = ( char * ) hb_xrealloc( pszBuffer, nLen + 1 );
                        if( ! HB_CDPCHAR_PUT( cdp, pszBuffer, nLen, &nD, wc ) )
                           break;
                     }
                  }
                  nLen = nD;
               }
               else
                  for( n = 0; n < nLen; n++ )
                     pszBuffer[ n ] = ( char ) cdp->upper[ ( HB_UCHAR ) pszText[ n ] ];
            }
            else
               for( n = 0; n < nLen; n++ )
                  pszBuffer[ n ] = HB_TOUPPER( pszText[ n ] );
                  
            pszBuffer[ nLen ] = '\0';
            hb_retclen_buffer( pszBuffer, nLen );
         }
         else
            hb_errRT_BASE_SubstR( EG_ARG, 1102, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
#pragma ENDDUMP
#endif
#pragma BEGINDUMP
HB_FUNC ( BIN2D )
{
 if ( hb_parinfo( 1 ) == HB_IT_STRING )
 {
   hb_retnd (  * (double*) hb_parc(1) );
 } else {
   hb_retnd ( hb_parnd( 1 ) );
 };
}

HB_FUNC ( D2BIN )
{
   double d = hb_parnd(1);
   hb_retclen ( (char *) &d, 8 );
}

HB_FUNC ( TRANR )
      {
         char * Templ = (char *) hb_parc( 2 );
         char * Line  = (char *) hb_parc( 1 );
         char * ret;
         size_t i = 0, j = 0, k = 0, t = hb_parclen( 2 ), l = hb_parclen( 1 );
         const char * f = "X#9!NALY";

         if ( * Templ == '@')
         {
           ret = strchr( Templ,' ' );
           if (ret != NULL) { i = ret - Templ ; }
         }

         ret = (char *) hb_xgrab( t - i);

         for( ; ((j <= l) && (i < t )) ; i++ )
         {
            if ( strchr( f, Templ[i] ) != NULL )
            {
               if ( j < l ) { ret[k++] = Line[j] ; }
               j++;
            } else {
               ret[k++] = Templ[i];
            }
         }

         hb_retclen( ret, k);
         hb_xfree( ret );
      }

HB_FUNC ( ALTATTR )
      {
         size_t i, j = hb_parclen( 1 );
         char * src = (char *) hb_parc( 1 ), * ret = (char *) hb_xgrab( j );
         char k = (char) hb_parni( 2 );
         for( i = 0; i < j ; i++ )
         {

           if (i%4 == 2)
           ret[i] = src[i] ^ k;
           else
           ret[i] = src[i];
         }
         hb_retclen( ret, j );
         hb_xfree( ret );
      }

#pragma ENDDUMP
#endif
********************
