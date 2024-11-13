#define I hb_UTF8ToStr("│")
#ifndef A_LAN
#define DatY MEMVAR
#endif
#include "inkey.ch"
#include "dm_form.ch"
#include "ar.ch"

#ifndef A_IDENT
  #define A_IDENT hb_fieldlen('ident')
#endif
#ifndef A_WAL
  #define A_WAL 11
#endif
field konto,czyma,kwota,rejestr,lp,link,data,ident,pozycja

memvar rejestry,chg

func dziennik(x,_s)
   sel("konta","kont_num")
   sel("main","main_lp")
   if _s=NIL
      _s:=array(_sLEN)
   endif
   DEFAULT _srowb TO 1
   DEFAULT _scol1 TO maxcol()-(A_IDENT+2*A_KTL+A_WAL+23)
   DEFAULT _scol2 TO maxcol()
   DEFAULT _sbeg  TO 1
   DEFAULT _slth  TO 0
   DEFAULT _sinfo TO {|k,s|maininfo(k,s)}
   DEFAULT _spocz TO ""
   if x=3
      DEFAULT _skproc TO array(32)
      DEFAULT _skproc[K_ENTER] TO {|_skey,_s|_Sinfo(_skey,_s)}
      DEFAULT _skproc[K_TAB] TO {|_skey,_s|maininfo(K_ENTER,_s)}
      x:=2
   end
   _skon:=x

return szukam(_s)
***************************
stat func maininfo(key,_s)
static dzflag,p  //,ks:=.f.
local l,s,txt,w,sp
field baza
if key=0
   if _skon#NIL
      dzflag:=_skon
      _skon:=NIL
      if p#NIL.and.dzflag>0
         p:=czyma
      endif
      _snagkol:=1
      _sbeg:=1
      set cursor on
   endif
   if dzflag>0
      _spocz:=""
      _slth:=0
      if dzflag=1
         sp:=replicate("X",A_IDENT)+"|XX|XXXXX"
         _spform:={|p|tranr(p,sp)}
         set order to "main_id"
         _sbeg:=7
      else
         _spform:={|p|tranr(p,"XX|XXXXX")}
         set order to "main_lp"
         _sbeg:=8+A_IDENT // 15
      endif
      if p=NIL
         _sfor:=NIL
         _sprompt:={|ktm,ktw|ktm:=ktw:=space(A_KTL),if(czyma,ktm:=pad(konto,A_KTL),ktw:=pad(konto,A_KTL)),DTOV(data)+I+pad(if(_sbeg=7,ident,right(trim(ident),A_IDENT)),A_IDENT)+I+rejestr+I+lp+"/"+LP(pozycja)+I+strpic(kwota,A_WAL,2,"@E ")+hb_UTF8ToStr("║")+ktw+I+ktm}
      else
         _sprompt:=if(dzflag=1,{|ktm,ktw,p,q,r,s,w,b|s:=czyma,w:=kwota,r:=recno(),ktw:=if(dbseek(UpP(ident)+rejestr+lp+link,.f.).and.s#czyma,pad(konto,A_KTL),space(A_KTL)),ktm:=hb_UTF8ToStr(if(round(w-kwota,2)=0,"│","┼"))+ktw,b:=if(round(w-kwota,2)=0,"║","╟"),dbgoto(r),if(s,ktm:=">"+pad(konto,A_KTL),(ktw:=pad(konto,A_KTL),b:='>')),q:=DTOV(data)+I+pad(if(_sbeg=7,ident,right(trim(ident),A_IDENT)),A_IDENT)+I+rejestr+I+lp+"/"+LP(pozycja)+I+strpic(kwota,A_WAL,2,"@E "),if(empty(p),(devout(q),hb_DispOutAtBox(row(),col(),hb_UTF8ToStrBox(b)),DevPos(row(),col()+1),q:=''),q+=hb_UTF8ToStr(b)),q+ktw+ktm},;
                               {|ktm,ktw,p,q,r,s,w,b|s:=czyma,w:=kwota,r:=recno(),ktw:=if(dbseek(rejestr+lp+link,.f.)           .and.s#czyma,pad(konto,A_KTL),space(A_KTL)),ktm:=hb_UTF8ToStr(if(round(w-kwota,2)=0,"│","┼"))+ktw,b:=if(round(w-kwota,2)=0,"║","╟"),dbgoto(r),if(s,ktm:=">"+pad(konto,A_KTL),(ktw:=pad(konto,A_KTL),b:='>')),q:=DTOV(data)+I+pad(if(_sbeg=7,ident,right(trim(ident),A_IDENT)),A_IDENT)+I+rejestr+I+lp+"/"+LP(pozycja)+I+strpic(kwota,A_WAL,2,"@E "),if(empty(p),(devout(q),hb_DispOutAtBox(row(),col(),hb_UTF8ToStrBox(b)),DevPos(row(),col()+1),q:=''),q+=hb_UTF8ToStr(b)),q+ktw+ktm})
         p:=czyma
         _sfor:={||czyma=p}
      endif
      _snagl:=hb_UTF8ToStr("Data┬")+padc("Ident",A_IDENT,hb_UTF8ToStr("─"))+hb_UTF8ToStr("┬─R┬──Lp─────┬")+padc("Kwota",A_WAL,hb_UTF8ToStr("─"))+hb_UTF8ToStr("╦─Wn")+padc("Konto",2*A_KTL-5,hb_UTF8ToStr("─"))+"Ma"
   else
      _snagl:=hb_UTF8ToStr("Data┬")+padc("Ident",A_IDENT,hb_UTF8ToStr("─"))+hb_UTF8ToStr("┬─R┬──Lp─────┬")+padc("Analityka",2*A_KTL-A_WAL,hb_UTF8ToStr("─"))+hb_UTF8ToStr("╦──Wn")+padc(trim(konta->konto),2*A_WAL-7,hb_UTF8ToStr('─'))+"Ma"
#ifndef A_STL
#define A_STL 3
#endif
      _sprompt:={||DTOV(data)+I+pad(if(_sbeg=7,ident,right(trim(ident),A_IDENT)),A_IDENT)+I+rejestr+I+lp+"/"+LP(pozycja)+I+pad(SubStr(konto,A_STL+1),2*A_KTL-A_WAL)+hb_UTF8ToStr("║")+strpic(if(czyma,0,kwota),A_WAL,2,"@EZ ")+I+strpic(if(czyma,kwota,0),A_WAL,2,"@EZ ")}
      _sfor:=NIL
      _slth:=0 //2
      _sbeg:=1
      _spform:={|p,l|tranr(right(p,l),"XX.XX")}
      set order to tag main_kt
      _spocz:=konto
      if konta->syntet
         l:=len(s:=_spocz:=trim(konta->konto))
#ifndef A_STL
#define A_STL 3
#endif
         if l=A_STL
            SET ORDER TO TAG MAIN_KS
         else
           txt:="left(konto,"+str(l,1)+")+SubStr(dtos(data),5)"
           ordsetfocus("X"+s)
           if ordsetfocus()<>"X"+s
             w:=MESSAGE("Tworzenie skorowidza dla konta syntetycznego "+s+";"+trim(konta->nazwa)+".;nazwa: MAIN_TMP baza: MAIN.DBF, klucz: "+txt+".;.")
             SET ORDER TO TAG MAIN_KT
             seek s
             ordCondSet("konto='"+s+"'",&("{||konto='"+s+"'}"),,{||konto=s},{||dispout(hb_UTF8ToStr("▒")),message(1),.t.},int(1+lastrec()/(w[4]-w[2]-3)),RECNO(),,,,)
             ordCreate("."+HB_OsPathSeparator()+"main_tmp","X"+s,txt,&("{||"+txt+"}"))
             message(w)
#ifndef A_CDX
             sel("INDEKS")
             locate FOR baza="MAIN"
             exec main->(ordlistadd(Trim(indeks->nazwa))) while baza="MAIN"
             select MAIN
             ordsetfocus(1)
#endif
           endif
           //ks:=.t.
         endif
      endif
      //_spocz+=left(dtov(IF(_si=0,DatY->d_z_gran+1,data)),_slth)
      kibord(chr(30))
   endif
   //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')


elseif key=K_ESC
/*
   if ks
     if file("."+HB_OsPathSeparator()+"main_tmp"+ordbagext())
        l:=recno()
        use
        begin sequence
        erase ("."+HB_OsPathSeparator()+"main_tmp"+ordbagext())
        end sequence
        sel("main","main_kt")
        goto l
     endif
     ks:=.f.
   endif
*/
   return .t.
elseif key=K_CTRL_RIGHT .or. key=K_CTRL_LEFT
   if dzflag=0 .and. p#NIL
      p:=czyma
   endif
   if key=K_CTRL_RIGHT
      ++dzflag
   else
      dzflag+=2
   endif
   dzflag%=3
   if dzflag=0
      konta->(dbseek(main->konto,.f.))
   endif
   maininfo(0,_s)
   refresh(,_s)
elseif _si=0
elseif key=K_ENTER
   if dok(rejestr,lp,if(type("da")="D",.f.,))
     goto _srec[_sm]
     REFRESH(,_s)
     chg:=.t.
   endif

elseif key=K_TAB
   dokdruk(rejestr,lp)

elseif key=K_F2
   if dzflag>0  //rozbicie na dwoje
      p:=if(p=NIL,czyma,NIL)
   else
      if konta->syntet
         konta->(dbseek(main->konto,.f.))
         _spocz:=konto+right(_spocz,_slth)
      else
         konta->(dbseek(left(main->konto,A_STL),.f.))
         if konta->syntet
            _spocz:=left(konto,A_STL)+right(_spocz,_slth)
         else
            return .f.
         endif
      endif
   endif
   key:={_spocz,_slth}
   maininfo(0,_s)
   _spocz:=key[1]
   _slth:=key[2]
   refresh(,_s)

elseif key=K_F3
   if _sfor#NIL  //przejście na przeciwstawne
      p:=!p
      seek if(dzflag=1,UpP(ident),"")+rejestr+lp+link
      refresh(,_s)

   elseif dzflag=0
      set order to "main_lp"
      seek rejestr+lp+link
      konta->(dbseek(main->konto,.f.))
      _spocz:=konto+right(_spocz,_slth)
      key:={_spocz,_slth}
      maininfo(0,_s)
      _spocz:=key[1]
      _slth:=key[2]
      refresh(,_s)
   endif

elseif key=K_F8
      _slist(,_s)

elseif key=K_F9
      _sfil(_s)

endif
return .f.
*****************************
