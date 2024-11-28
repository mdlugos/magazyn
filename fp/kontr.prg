#define I hb_UTF8ToStr("│")
#include "inkey.ch"
#include "dm_form.ch"
#ifdef A_DPS
#define nazwA nazwisko
#endif
field numer_kol,nazwA,konto

function khval(g,kh,win)
local k,st:=push_stat(),f9:=setkey(K_F9,nil)

    sel("firmy","firm_num")
    if g#NIL
       kh:=g:varget()
    endif
    k:=UpP(trim(kh))
    if szukam({1,10,,,1,len(k),hb_UTF8ToStr("Nr┬")+left(fieldname(2),1)+lower(trim(SubStr(fieldname(2),2))),{||fieldget(1)+I+fieldget(2)+I+fieldget(3)},{|key,s|khinfo(key,s)},k})
       if g#NIL
          g:varput(numer_kol)//+' '+nazwA)
          @ g:row,g:col+A_NRLTH SAY left(' '+nazwA,win[4]-col())
       else
          kh:=numer_kol
       endif
       k:=recno()
       pop_stat(st)
       firmy->(dbgoto(k))
       setkey(K_F9,f9)
       return .t.
    endif
    pop_stat(st)
    setkey(K_F9,f9)
return .f.
***********************
proc kontrahenci()
    sel("firmy","firm_num")
    szukam({1,10,,,1,0,hb_UTF8ToStr("Nr┬")+left(fieldname(2),1)+lower(trim(SubStr(fieldname(2),2))),{||fieldget(1)+I+fieldget(2)+I+fieldget(3)},{|key,s|khinfo(key,s)},""})
return
***********************
func khinfo(key,_s)
local win,getlist ,l,i,af,lock,a,b,c
if key=0
   i:=recno()

   if _slth>0 .and._spocz>"9"
      set order to "firm_naz"
      _sbeg:=A_NRLTH+2
      dbseek(UpP(_spocz))
   elseif _slth=A_NRLTH
      if numer_kol==_spocz .or. dbseek(_spocz)// .and. numer_kol==_spocz
         return _sret:=.t.
      endif
      _slth:=0
      _spocz:=""
      dbgoto(i)
   elseif (l:=val(_spocz))>0
      if val(numer_kol)=l .or. dbseek(l:=str(val(_spocz),A_NRLTH)) .or. dbseek(strtran(l," ","0"))
         return _sret:=.t.
      endif
   endif

   _snagkol:=A_NRLTH-2
   _spform:={|p|p}

elseif key=K_ESC
   return .t.

elseif key=K_ENTER
   return _sret:=.t.

elseif key>=65 .and. _sbeg=1
   _spocz:=UpP(chr(key))
   _slth:=1
   _sbeg:=A_NRLTH+2
   set order to tag firm_naz
   //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   if EvaldB(_swar,_spocz)
      refresh(1,_s)
   else
      seek _spocz
      refresh(0,_s)
   endif
   return .t.

elseif key>=48 .and. key<=57 .and. _sbeg>1 .and. _slth=1
   _sbeg:=1
   set order to tag firm_num
   //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   if EvaldB(_swar,_spocz)
      refresh(1,_s)
   else
      seek _spocz
      refresh(0,_s)
   endif
   return .t.

elseif key=K_CTRL_RIGHT .or. key=K_CTRL_LEFT
   i:=ascan({1,A_NRLTH+2},_sbeg)
   if key=K_CTRL_RIGHT
      i:=i%2+1
   else
      i:=(i-2)%2+1
   endif
   _slth:=0
   _spocz:=""
   if i=1
      set order to "firm_num"
      _sbeg:=1
   elseif i=2
      set order to "firm_naz"
      _sbeg:=A_NRLTH+2
   endif
   //_swar=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   refresh(1,_s)
elseif key=K_INS .or. key=43
   if key=43
      _slth:=0
      _spocz:=''
      goto 0
   endif
   b:=dbstruct()
   l:=len(b)
   key:=_scoln
   aeval(b,{|x|key:=max(key,x[3])})
   win:=window(l,int(1+l/maxrow())*key,"w+/GR,i,,bg+/Gr,w+/b")
   key:=indexord()
   getlist:={}
   af:=array(l)
#ifdef A_LAN
   lock:=eof() .or. RECLOCK(.F.,,.F.)
#endif
   for i:=1 to l
        af[i]:=fieldget(i)
#ifdef A_PENS
#define FieldnamE(i) if(b[i,1]='Nazwa','Nazwisko',b[i,1])
#endif
        a:=(win[4]-win[2])/int(1+l/maxrow())-13
        @ win[1]+1+(i-1)%maxrow(),win[2]+1+(win[4]-win[2])*int((i-1)/maxrow())/int(1+l/maxrow())  say tran(pad(lower(strtran(FieldnamE(i),"_"," ")+":"),11),"!XXXXXXXXXX")

        //getl af[i]
        AAdd(GetList,_GET_( af[i], "af[i]",,,))
#undef FieldnamE

        if b[i,2]="M" .or. b[i,2]="C" .and. len(af[i])>a
#ifdef A_HBGET
           if len(af[i])<a
              af[i]:=pad(af[i],a)
           endif
#endif
           getlist[i]:cargo:=.t.
           getlist[i]:postblock:={|g|if(g:changed,g:varput(trim(g:buffer)),),.t.}
           getlist[i]:picture:="@S"+lTrim(sTr(a))
        elseif b[i,4] = 2
           getlist[i]:picture:="@E"
        endif
        getlist[i]:display()
   next i
   getlist[1]:postblock:={|g|empty(af[1]) .or.(af[1]:=strtran(padl(alltrim(af[1]),A_NRLTH)," ","0"),.t.)}
#ifdef A_FFULL
   getlist[2]:postblock:={|g|if(g:changed .and. empty(getlist[3]:varget()),(getlist[3]:varput(g:varget()),getlist[3]:display()),),.t.}
#endif
   i:=1
   do while .t.
#ifdef A_LAN
   if !lock
      inkey(0)
      exit
   endif
#endif
    __setproc(procname(0))
   readmodal(getlist,@i)
   set order to "firm_num"
   if !updated() .or. ReadkeY()=K_ESC
      exit
   elseif af[1]=numer_kol .and. !empty(numer_kol)
      if !tak(hb_UTF8ToStr("Czy poprawić starą kartę"),win[3],win[2]+1,.t.,.f.,setcolor())
         loop
      endif
   elseif empty(af[2])
      sel("konta","kont_num")
      locate for right(konto,A_NRLTH)==firmy->numer_kol
      select firmy
      if konta->(eof())
         if tak(hb_UTF8ToStr("Czy kasować"),win[3],win[2]+1,.f.,.f.,setcolor())
            delete
            exit
         endif
      else
         alarm("Istnieje konto "+konta->konto,,3,3)
      endif
      loop
   elseif empty(af[1])
      if tak(hb_UTF8ToStr("Czy dopisać"),win[3],win[2]+1,.t.,.f.,setcolor())
      go bottom
      af[1]:=strtran(str(val(fieldget(1))+1,A_NRLTH)," ","0")
      append blank
#ifdef A_LAN
      _srec[_sm]:=recno()
      do while .t.
         numer_kol:=af[1]
         seek af[1]
         locate for _srec[_sm]#recno() while numer_kol=af[1]
         if found()
            af[1]:=strtran(str(val(af[1])+1,A_NRLTH)," ","0")
            goto _srec[_sm]
            loop
         endif
         goto _srec[_sm]
         exit
      enddo
#endif
      else
      loop
      endif
   elseif !dbseek(af[1])
      if tak(hb_UTF8ToStr("Czy dopisać"),win[3],win[2]+1,.t.,.f.,setcolor())
      append blank
      else
      goto _srec[_sm]
      loop
      endif
   else
      goto _srec[_sm]
      alarm(hb_UTF8ToStr("Taki numer już istnieje !"),,3,3)
      i:=1
      loop
   endif
   aeval(af,{|x,i|fieldput(i,if(valtype(fieldget(i))="M",trim(x),x))})
   exit
   enddo
   unlock
   window(win)
   set order to (key)
   refresh(,_s)

elseif key=K_F8
      _slist("."+HB_ps()+"f*.frm",_s)

elseif key=K_F9
      _sfil(_s)

endif
return .f.
***********************
