#define I hb_UTF8ToStr("│")
#define I2 hb_UTF8ToStr("║")
#ifndef A_LAN
#define DatY MEMVAR
#endif
#include "inkey.ch"
#include "dm_form.ch"
#include "ar.ch"
#ifdef A_DPS
#define nazwA nazwisko
#endif
field konto,syntet,khflag,nazwa,sald_kon_w,sald_kon_m,sald_rok_w,sald_rok_m,sald_poc_w,sald_poc_m,sald_bie_w,sald_bie_m,czyma,kwota,rejestr,lp,data,ident,nazwisko,opcje
memvar rejestry

proc konta()

sel("konta","kont_num")
szukam({0,0,,,1,0,,,{|key,s|koninfo(key,s)},""})
return
***********************
function konval(g,f,kh,list,pos) // get , for , kh
local st,r,k,f9
st:=push_stat()
sel("firmy","firm_num")
sel("konta","kont_num")
k:=g:varget()
if valtype(f)='A'
   r:=f[2]
   f:=f[1]
else
   r:={||.t.}
endif
if valtype(f)='C'
   f:=&f
endif
f9:=setkey(K_F9,nil)
if szukam({0,0,,,1,0,,,{|key,x|koninfo(key,x,kh,k,f)},''}) .and. eval(r,g,konto,list,pos)
   r:=recno()
   k:=firmy->(recno())
   g:varput(konta->konto)
   pop_stat(st)
   konta->(dbgoto(r))
   if empty(kh)
     firmy->(dbgoto(k))
   endif
   setkey(K_F9,f9)
   return .t.
endif

pop_stat(st)
setkey(K_F9,f9)
return .f.
************************
stat func koninfo(key,_s,kh,k,f)
memvar chg,it_kopcja
local i
if key=0
   if k#NIL
      k:=UpP(k)
      _slth:=len(_spocz:=trim(k))
      if valtype(f)='B'
         _sfor:=f
      elseif f=.t. .and. !empty(kh) //musi być kontrahent
         _sfor:={||khflag .or. !syntet .and. kh=right(konto,A_NRLTH)}
      else //if empty(kh)
         _sfor:={||!syntet .or. khflag}
      endif
      if !empty(k)
         if (k==konto .or. dbseek(k) .or. _spocz==k .and. kh=right(k,A_NRLTH) .and. dbseek(left(k,A_KTL-A_NRLTH)) .and. khflag);
             .and.(_sfor=NIL.or.eval(_sfor))
            return koninfo(K_ENTER,_s,kh,k)
         elseif (konto<>_spocz) .and. (ordsetfocus('kont_naz'), dbseek(_spocz))
            _sbeg:=A_KTL+2
         else
            ordsetfocus('kont_num')
         endif
      endif
   endif
   _snagkol:=A_KTL-5
   i:=if(_scol2=NIL,maxcol(),_scol2)
   key:=hb_fieldlen('nazwa')
   if IT_KOPCJA .and. i<=A_KTL+key+6*A_WAL
      _snagl:=hb_UTF8ToStr("Numer┬─Nazwa")+replicate(hb_UTF8ToStr("─"),min(key,i-6-A_KTL-4*A_WAL)-6)+hb_UTF8ToStr("╦─(Wn)")+padc("Obroty",2*A_WAL-9,hb_UTF8ToStr("─"))+hb_UTF8ToStr("(Ma)─╦─(Wn)")+padc(hb_UTF8ToStr("Saldo końcowe"),2*A_WAL-9,hb_UTF8ToStr("─"))+"(Ma)"
      _sprompt:={|skw,skm|if(DatY->d_z_rok=DatY->d_z_mies1,(skw:=sald_rok_w,skm:=sald_rok_m),if(DatY->d_z_mies2=DatY->d_z_mies1,(skw:=sald_poc_w,skm:=sald_poc_m),(skw:=sald_kon_w,skm:=sald_kon_m))),;
                konto+I+padr(nazwa,min(key,i-6-A_KTL-4*A_WAL))+I2+strpic(sald_bie_w-skw,A_WAL,2,"@EZ ")+if(skw=skm,I,if(skw<skm,">","<"))+strpic(sald_bie_m-skm,A_WAL,2,"@EZ ")+I2+strpic(IF(SALD_BIE_W>SALD_BIE_M,sald_bie_w-SALD_BIE_M,0),A_WAL,2,"@EZ ")+I+strpic(IF(SALD_BIE_M>SALD_BIE_W,sald_bie_m-SALD_BIE_W,0),A_WAL,2,"@EZ ")}
   else
      _snagl:=left(hb_UTF8ToStr("Numer┬─Nazwa")+replicate(hb_UTF8ToStr("─"),if(key>=i-6-A_KTL-4*A_WAL,i-6-A_KTL-4*A_WAL,min(key,i-8-A_KTL-6*A_WAL))-6)+hb_UTF8ToStr("╦─(Wn)")+padc(hb_UTF8ToStr("Saldo początkowe"),2*A_WAL-9,hb_UTF8ToStr("─"))+hb_UTF8ToStr("(Ma)─╦─(Wn)")+padc("Obroty",2*A_WAL-9,hb_UTF8ToStr("─"))+hb_UTF8ToStr("(Ma)─╦─(Wn)")+padc(hb_UTF8ToStr("Saldo końcowe"),2*A_WAL-9,hb_UTF8ToStr("─"))+"(Ma)",i-_snagkol-2)
      _sprompt:={|skw,skm|if(DatY->d_z_rok=DatY->d_z_mies1,(skw:=sald_rok_w,skm:=sald_rok_m),if(DatY->d_z_mies2=DatY->d_z_mies1,(skw:=sald_poc_w,skm:=sald_poc_m),(skw:=sald_kon_w,skm:=sald_kon_m))),;
                konto+I+padr(nazwa,if(key>=i-6-A_KTL-4*A_WAL,i-6-A_KTL-4*A_WAL,min(key,i-8-A_KTL-6*A_WAL)))+I2+strpic(IF(SKW>SKM,skw-SKM,0),A_WAL,2,"@EZ ")+I+strpic(IF(skm>SKW,SKM-SKW,0),A_WAL,2,"@EZ ")+I2+strpic(sald_bie_w-skw,A_WAL,2,"@EZ ")+if(sald_bie_w=sald_bie_m,I,if(sald_bie_w<sald_bie_m,">","<"))+strpic(sald_bie_m-skm,A_WAL,2,"@EZ ")+I2+strpic(IF(SALD_BIE_W>SALD_BIE_M,sald_bie_w-SALD_BIE_M,0),A_WAL,2,"@EZ ")+I+strpic(IF(SALD_BIE_M>SALD_BIE_W,sald_bie_m-SALD_BIE_W,0),A_WAL,2,"@EZ ")}
   endif

elseif key=K_ESC
   return .t.

elseif key=K_ENTER .and. k#NIL
   if khflag
      if empty(kh) .and. (kh:=space(A_NRLTH),!khval(,@kh))
         return .f.
      endif
      k:=left(konto,A_KTL-A_NRLTH)+left(kh,A_NRLTH)
      set order to "kont_num"
      key:=nazwa
      if !dbseek(k)
         append blank
         konto:=k
         select "firmy"
         set order to "firm_num"
         konta->nazwa:=if(dbseek(left(kh,A_NRLTH),.f.),trim(nazwA)+" - "+key,key)
         select konta
         unlock lastrec()
      endif
   endif
   k:=konto
   return _sret:=.t.

elseif key=K_TAB .or. key=K_ENTER
   sel("main","main_kt")
   setpos(row()+2,col())
   private chg:=.f.
   if dbseek(if(konta->syntet,trim(konta->konto),konta->konto))
      dziennik(0)
   endif
   select konta
   SET ORDER TO TAG (if(_sbeg=1,'KONT_NUM','KONT_NAZ'))
   if chg
      go _srec[_sm]
      refresh(,_s)
   endif

elseif key>32 .and. _slth=1
   if key>64 .and. _sbeg=1
      set order to "kont_naz"
      _sbeg:=A_KTL+2
   elseif key>47 .and. key<58 .and. _sbeg<>1
      set order to "kont_num"
      _sbeg:=1
   else
      return .f.
   endif
   //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   if !dbseek(_spocz)
      _spocz:=''
      _slth:=0
   endif
   refresh(0,_s)
   return _slth>0


elseif key=K_CTRL_RIGHT .or. key=K_CTRL_LEFT
   i:=ascan({1,A_KTL+2},_sbeg)
   if if(key=K_CTRL_RIGHT,2,1)=i
      return .f.
   endif
   i:=i%2+1
   _slth:=0
   _spocz:=""
   if i=1
      set order to "kont_num"
      _sbeg:=1
   elseif i=2
      set order to "kont_naz"
      _sbeg:=A_KTL+2
   endif
   //_swar=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   refresh(1,_s)

elseif key=K_INS
   if konedit()
      refresh(,_s)
   else
      REFRESH LINE _sm+_srow1-1 DIRECTION 0
   endif

elseif key=K_F2
      it_kopcja:=!it_kopcja
      koninfo(0,_s)
      refresh(,_s)

elseif key=K_F8
      _slist("."+HB_OsPathSeparator()+"k*.frm",_s)

elseif key=K_F9
      _sfil(_s)

endif
return .f.
***************************
func konedit()
local win,getlist:={},nk,na,chg:=.f.,pos,snt,r,lock,wasl,spw,spm,skw,skm,a,kha,opc
   r:=recno()
#ifdef A_LAN
   wasl:=ascan(dbrlocklist(),r)#0
   LOCK:=wasl .or. eof() .or. RECLOCK(.F.,,.F.,,r)
#endif
#ifdef A_OPCJE
   opc:=opcje
#endif
   nk:=konto
   na:=nazwa
   snt:=syntet
   kha:=khflag
   spw:=sald_poc_w
   spm:=sald_poc_m
   skw:=sald_kon_w
   skm:=sald_kon_m
   win:=window(7+if(DatY->d_z_mies1>DatY->d_z_mies2,6,if(DatY->d_z_mies2>DatY->d_z_rok,3,0)),max(22+2*A_WAL,A_KTL+hb_fieldlen('nazwa')+1),"w+/GR,i,,bg+/Gr,w+/b")
#ifdef __PLATFORM__DOS   
   @ win[1]+ 3,win[2]+23 BOX replicate("═",A_WAL-5)+"Wn═══┬═══Ma"+replicate("═",A_WAL-5) UNICODE
#else
   @ win[1]+ 3,win[2]+23 BOX replicate("═",A_WAL-5)+"Wn═══╤═══Ma"+replicate("═",A_WAL-5) UNICODE
#endif   
   @ win[1]+ 4,win[2]+2 say padr("BO z dnia "+dtoc(DatY->d_z_rok+1),20)+" "+strpic(sald_rok_w,A_WAL,2,"@ZE ")+I+strpic(sald_rok_m,A_WAL,2,"@ZE ")
   if DatY->d_z_rok<DatY->d_z_mies2
      @ row()+1,win[2]+2 say padr("Obroty II do "+dtov(DatY->d_z_mies2),20)+" "+strpic(sald_poc_w-sald_rok_w,A_WAL,2,"@ZE ")+I+strpic(sald_poc_m-sald_rok_m,A_WAL,2,"@ZE ")
      @ row()+1,win[2]+2 say padr("Obroty II + BO"                     ,20)+" "+strpic(sald_poc_w,A_WAL,2,"@ZE ")+I+strpic(sald_poc_m,A_WAL,2,"@ZE ")
      @ row()+1,win[2]+2 say padr(hb_UTF8ToStr("Saldo końcowe II")     ,20)+" "+strpic(if(sald_poc_w-sald_poc_m>0,sald_poc_w-sald_poc_m,0),A_WAL,2,"@ZE ")+I+strpic(if(sald_poc_m-sald_poc_w>0,sald_poc_m-sald_poc_w,0),A_WAL,2,"@ZE ")
   else
      spw:=sald_rok_w
      spm:=sald_rok_m
   endif
   if DatY->d_z_mies2<DatY->d_z_mies1
      @ row()+1,win[2]+2 say padr("Obroty I do "+dtov(DatY->d_z_mies1),20)+" "+strpic(skw-spw,A_WAL,2,"@ZE ")+I+strpic(skm-spm,A_WAL,2,"@ZE ")
      @ row()+1,win[2]+2 say padr("Obroty I "+if(DatY->d_z_rok<DatY->d_z_mies2,"+ II ","")+"+ BO",20)+" "+strpic(skw,A_WAL,2,"@ZE ")+I+strpic(skm,A_WAL,2,"@ZE ")
      @ row()+1,win[2]+2 say padr(hb_UTF8ToStr("Saldo końcowe I")     ,20)+" "+strpic(if(skw-skm>0,skw-skm,0),A_WAL,2,"@ZE ")+I+strpic(if(skm-skw>0,skm-skw,0),A_WAL,2,"@ZE ")
   else
      skw:=spw
      skm:=spm
   endif
   @ row()+1,win[2]+2 say padr(hb_UTF8ToStr("Obroty bieżące")      ,20)+" "+strpic(sald_bie_w-skw,A_WAL,2,"@ZE ")+I+strpic(sald_bie_m-skm,A_WAL,2,"@ZE ")
   @ row()+1,win[2]+2 say padr(hb_UTF8ToStr("Obroty bieżące + BO") ,20)+" "+strpic(sald_bie_w,A_WAL,2,"@ZE ")+I+strpic(sald_bie_m,A_WAL,2,"@ZE ")
   @ row()+1,win[2]+2 say padr(hb_UTF8ToStr("Saldo końcowe")       ,20)+" "+strpic(if(sald_bie_w-sald_bie_m>0,sald_bie_w-sald_bie_m,0),A_WAL,2,"@ZE ")+I+strpic(if(sald_bie_m-sald_bie_w>0,sald_bie_m-sald_bie_w,0),A_WAL,2,"@ZE ")

   @ win[1]+1,win[2]+2 get nk picture "@!K" valid {||nk:=UpP(nk),.t.}
   getl na picture "@K"
   @ win[1]+2,win[2]+2 say "Ma analitykę:" unicode get snt picture "Y" valid {||kha:=kha.and.snt,.t.}
   SAYL 'Kontrahent w analityce:' get kha picture "Y" when snt
#ifdef A_OPCJE
   @ win[1]+3,win[2]+2 say "Opcje:" get opc
#endif
   do while .t.
#ifdef A_LAN
   if !lock
      inkey(0)
      exit
   endif
#endif
   __setproc(0)
   readmodal(getlist,@pos)
   chg:=chg.or.updated()
   if !chg .or. ReadkeY()=K_ESC
      chg:=.f.
      exit
   elseif empty(nk)
      sel("main","main_kt")
      select konta
      if (syntet .or. sald_rok_w=0.and.sald_rok_m=0 .and. !main->(dbseek(konta->konto))) .and. tak(hb_UTF8ToStr("Czy kasować"),win[3],win[2]+1,.f.,.f.,setcolor())
         delete
         exit
      endif
   elseif snt.and.len(trim(nk))=A_KTL
      alarm(hb_UTF8ToStr("Za długi kod jak na konto syntetyczne !"),,3,3)
      pos:=3
      snt:=.f.
   elseif nk=konto
      if snt .and. !syntet
         sel("main","main_kt")
         select konta
         if main->(dbseek(nk))
            alarm("Nie zmieniaj rodzaju konta !",,3,3)
            snt:=syntet
            pos:=3
            loop
         endif
      endif
      if tak(hb_UTF8ToStr("Czy poprawiać stare konto"),win[3],win[2]+1,.t.,.f.,setcolor())
         nazwa:=na
         syntet:=snt
         khflag:=kha
#ifdef A_OPCJE
         opcje:=opc
#endif
         exit
      endif
   elseif !dbseek(nk)
      set order to "kont_naz"
      a:=NIL
      if dbseek(UpP(na))
         setpos(win[3]-4,win[2]+20)
         a:=message("Nazwa nie jest unikalna:"+konto)
      endif
      set order to "kont_num"
      if tak(hb_UTF8ToStr("Czy dopisać nowe konto"),win[3],win[2]+1,.t.,.f.,setcolor())
         append blank
         konto:=nk
         nazwa:=na
         syntet:=snt
         khflag:=kha
#ifdef A_OPCJE
         opcje:=opc
#endif
         unlock lastrec()
         window(a)
         exit
      endif
      window(a)
      goto r
   else
      goto r
      alarm(hb_UTF8ToStr("Taki symbol już istnieje !"),,3,3)
      pos:=1
   endif
   enddo
#ifdef A_LAN
   if !wasl
#endif
      unlock r
#ifdef A_LAN
   endif
#endif
   window(win)
return chg
****************************
