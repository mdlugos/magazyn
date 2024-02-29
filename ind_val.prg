#include "inkey.ch"
#include "getexit.ch"
#ifndef A_FA
#ifdef A_WA
#define cenA_zaK cena_przy
#endif
#endif
#ifdef A_SZYM
  #undef A_ALTCEN
#endif
#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif
#ifndef A_DDBF
#define DatY MEMVAR
#endif

#ifdef A_MYSZ
   #define D_MYSZ ,bx,cx,dx,myszflag
#else
   #define D_MYSZ
#endif

#define WAPICT "@KE ### ###.##"
#define WAPIC "@KE # ### ###.##"
#define CEOKR 2

#ifdef A_AUTOMAR
#ifndef A_PROCMAR
#define A_PROCMAR
#endif
#endif

#ifdef A_OLZBY
#define WANAZ "WAGA kg"
#else
#define WANAZ "WartoòÜ"
#endif
#ifdef A_SZTUKI
#define ILDEC 0
#define ILPIC " ###"
#else
#define ILDEC 3
#define ILPIC ".###"
#endif

#include "dm_form.ch"
#ifdef A_SWWKTM
#ifndef A_SWW
#define A_SWW A_SWWKTM
#endif
#endif

#ifdef A_JMALTTOT
  #define A_JMALT
#endif

MEMVAR r,mag_biez,mag_poz,dok_par,magazyny,adres_mag,changed,srec,stanowis,dok_rozch,stawkizby

MEMVAR jmiar,miar_opcja,iord,s,SB,se,s3,s2,s1,da,i,W,Wb,We,W3,W2,W1,il,defa

field nr_mag,index,nazwa,stan,smb_dow,nr_dowodu,nr_zlec,ilosc,KTO_PISAL,przel,;
WAZNOSC,cena_przy,data_przy,nr_rys,cena,cena_zak,proc_VAT,polka,cena_huty,sub_dok,;
zaznacz,info,data,pozycja

stat FUNCTION JM(j_m,j_o,p,r)
   field jm
   local ret
   J_M=UPPER(J_M)
   ret=ACZOJS(JMIAR,@j_m)
   J_M:=LOWER(J_M)
   if ret .and. j_m # jm
      j_o=upper(j_m)
      p=1
#ifdef A_JMALT
#define D_JMA -1
#define D_JMA1 -5
#else
#define D_JMA
#define D_JMA1
#endif
      @ r,18 D_JMA say p picture "######.###" COLOR _sunsel
      @ r,33 D_JMA1 say j_o  COLOR _sunsel
#undef D_JMA
#undef D_JMA1
   endif
return ret
***************
#ifdef A_SURLINK
stat func surinfo(k,_s,id,ni D_MYSZ)
field skladnik,indx_mat
if k=0
   if empty(_spocz)
      ordsetfocus('SUR_MAG')
      k:=recno()
      if !dbseek(ni)
         goto k
      endif
   endif
   if val(_spocz)=0
      ordsetfocus('SUR_NAZ')
      _sbeg:=len(skladnik)+2
      _swar=&('{|p|'+ordkey()+'=p'+'}')
      if _slth>0
         dbseek(_spocz)
      endif
   else
      ORDSETFOCUS('SUR_KOD')
      if dbseek(_spocz)
         if _spocz=skladnik
           _sret:=.t.
           id:=skladnik
           return .t.
         endif
      else
         dbseek(str(val(_spocz),len(skladnik)))
         _spocz:=''
         _slth:=0
      endif
   endif
   SET CURSOR ON

elseif k=K_ENTER
   _sret:=.t.
   go _srec[_sm]
   id:=skladnik
   return .t.
elseif k=K_ESC
   return .t.
elseif ISALPHA(UpP(chr(k))) .and. _sbeg=1
   ordsetfocus('SUR_NAZ')
   _sbeg:=len(skladnik)+2
   _spocz:=LEFT(_spocz,len(_spocz)-_slth)+UpP(CHR(K))
   _slth:=1
   _swar:=&('{|p|'+ordkey()+'=p'+'}')
   refresh(,_s)
   return .t.

elseif k=K_CTRL_LEFT .and. _sbeg<>1
    ordsetfocus('SUR_KOD')
    _swar:=&('{|p|'+INDEXKEY(0)+'=p'+'}')
    _spocz:=LEFT(_spocz,len(_spocz)-_slth)
    _slth:=0
    _sbeg:=1
    REFRESH LINE _sm+_srow1-1 DIRECTION 0
    refresh(1,_s)

elseif k=K_CTRL_RIGHT .and. _sbeg=1
    ordsetfocus('SUR_NAZ')
    _swar:=&('{|p|'+INDEXKEY(0)+'=p'+'}')
    _spocz:=LEFT(_spocz,len(_spocz)-_slth)
    _slth:=0
    _sbeg:=len(skladnik)+2
    REFRESH LINE _sm+_srow1-1 DIRECTION 0
    refresh(1,_s)


elseif k=K_F9
   _sfil(_s)
elseif k=K_F8
   _slist(,_s)
endif
return .f.
#endif
***************
#ifdef A_ANKER
#ifdef A_ZAGRODA
stat proc statedit(get,ni)
local w:=window(2,17,_sbkgr)
local getlist:={},waga,zawiesz,out:=get:varget()

waga:=out='1'
zawiesz:='S'$out

@ w[1]+1,w[2]+2 SAY "Towar na kwit:" GET waga picture "Y"
@ w[1]+2,w[2]+2 SAY "      e-sklep:" GET zawiesz picture "Y"
read
if readkey()#K_ESC
   out:=if(waga,"1","")+if(zawiesz,"S","")
   get:varput(out)
endif
window(w)
return
#else
stat proc statedit(get,ni)
local w:=window(3,17,_sbkgr)
local getlist:={},waga,zawiesz,cotw,out:=get:varget()

waga:=subs(out,3,1)>'0'.or.left(out,2)>'05'
zawiesz:=subs(out,3,1)>'0'
cotw:=subs(out,4,1)>'0'

@ w[1]+1,w[2]+2 SAY "Towar na wag©:" GET waga picture "Y"
@ w[1]+2,w[2]+2 SAY "   Zawieszony:" GET zawiesz picture "Y"
@ w[1]+3,w[2]+2 SAY " Cena otwarta:" GET cotw picture "Y"
read
if readkey()#K_ESC
   out:=if(waga,"1","0")+if(zawiesz,"1","0")+if(cotw,"4","0")
   get:varput(out)
endif
window(w)
return
#endif
#endif
**************
func nextni()
local i,s,ni:=index
  for i:=len(index) to 1 step -1
    if isdigit(subs(index,i,1))
       ni:=subs(index,i+1)
       exit
    endif
  next
  for i:=i to 1 step -1
    s:=subs(index,i,1)
    if isdigit(s)
       ni:=s+ni
    else
       exit
    endif
  next
return ni
***************
PROCEDURE KIM_EDIT(_s,beg)

    MEMVAR MAG_BIEZ,hlink
  local a,x,stat:=push_stat()
  local Rf:=.F.,new:=.f.,nr,pl,ce,c1,c2,c3,c4,ni,nm:="",j_m, z_min, z_max,lam,scr,STOS,;
        dp,i,j_o,p,uw,wa,rcrd,pos,pV,s,ar,arr,txt,nz,gr,ro,getlist:={},pm,mes:={},;
        pm1,pm2,pm3,pm4,ch,id,kw

  field index,stan,zamk_mies1,zamk_mies2,zamkn_roku,;
    zapas_min,zapas_max,data_popr,nazwa,jm,gram,kod,;
    jm_opcja,przel,lamus,data_zmian,NR_MAG,UWAGI,SWW,nr_zlec,proc_mar
#ifdef A_ALTCEN
  field cena1,cena2,cena3
  field proc_mar1,proc_mar2,proc_mar3
#endif
#ifdef A_WA
  field wartosc,Wart_mies1,wart_mies2,wart_roku
#endif
#ifdef A_LAN
  LOCAL JOB,lock
#endif
#ifdef A_ANKER
  local sn,st,rt,td
  field shortname,status,rodz_tow,tandem
#endif
#ifdef A_SHARP
  local kna,kwa,kkl,ksh,kgr
  field kasa_nazwa,kasa_klaw,kasa_shift,kasa_grupa,kasa_waga
  memvar grupy
#endif

#ifdef STANY
  local pom
#else
  memvar pom
  field unlink
  private pom
#endif
if _si=0
   goto lastrec()+1
endif
#ifdef A_LAN
  LOCK:=INDX_MAT->(EOF().or.RECLOCK(.F.,,.F.,,recno()))
#endif
#ifndef A_KODVAL
#ifdef A_DIETA
#ifdef A_KODY
    sel('SUROWCE','SUR_NRK')
#endif
#endif
#endif
    select indx_mat
    set order to TAG INDX_NUM
    set relation to
    set filter to
    set cursor off
  scr= savescreen(9,1,22,78)
  stos:={}
  @ 9,1,22,78 BOX '…Õª∫ºÕ»∫' color 'RG+/BG'
  @ 9,3 SAY 'Wpisywanie nowej, poprawianie lub kasowanie starej Karty Materiaàu' color 'RG+/BG'
  SET COLOR TO BG+/B
  SCROLL(10,2,21,77,0)

#ifdef A_JMO
  #define smiaR(x,d) if(miar_opcja,tran(int(x/(lam:=i_lam(d))->przel),"#### ###")+if(x%(lam)->przel=0,"","r"+tran(abs(x)%(lam)->przel,"@B ###")),x)
#else
  #define smiaR(x,d) tran(x,"#### ###"+ILPIC)
#endif

#ifndef STANY
    select STANY
    set filter to
    set ORDER TO TAG STAN_IND
    set relation to
    i:=recno()
    txt:=if(eof(),mag_biez,nr_mag)
    if dbseek(indx_mat->index,.f.)
#ifdef A_WA
    LOCATE REST FOR nr_mag#txt .and. (stan#0 .or. wartosc#0) .or. index#indx_mat->index
#else
    LOCATE REST FOR nr_mag#txt .and. stan#0 .or. index#indx_mat->index
#endif
    if INDEX=indx_mat->index
    @ 16,3 say "INNE MAGAZYNY: "
    do while index=indx_mat->index
       devout(nr_mag+": "+alltrim(smiaR(stan,data_zmian))+" ")
       CONTINUE
    enddo
    endif
    endif
    go i
#else
  nm:=mag_biez
#endif
#ifdef A_GRAM
 setpos(22,2)
#else
 setpos(21,2)
#endif
//#ifndef A_GRAM
      SAYL "Ostatnia dostawa dnia "+dtoc(data_przy)
 #ifdef A_FA
  #define D_CZ
 #else
  #ifdef A_WA
   #define D_CZ
  #endif
 #endif
 #ifdef D_CZ
      devout(", cena "+tran(cenA_zaK,WAPICT)+" zà.")
  #ifdef A_CENVAT
     sayl "Brutto: "+tran(cenA_zaK*(100+val(indx_mat->proc_vat))/100,WAPICT)
  #endif
  #undef D_CZ
 #endif
//#endif

      @ 17,2   say nr_mag+" Data:"
      @ 18,2   say "Stan:"
      @ 19,2   say WANAZ+":"
      @ 20,2   SAY "Cena:"
      @ 17,12  SAY data_zmian
      @ 18,11  SAY smiaR(stan,data_zmian)
#ifdef A_WA
      @ 19,11  SAY wartosc      PICTURE WAPIC
      @ 20,11  SAY wartosc/stan PICTURE WAPIC
#else
      @ 19,11  SAY stan*(i_lam(data_zmian))->cenA PICTURE WAPIC
      @ 20,11  SAY (i_lam(data_zmian))->cenA PICTURE WAPIC
#endif
#ifdef A_GRAM
      @ 21,2   SAY "Waga:"
      @ 21,11  SAY stan*(i_lam(data_zmian))->gram/1000 PICTURE "##### ###.##"
#endif
if DatY->d_z_mies1>DatY->d_z_mies2
      @ 17,30  SAY DatY->d_z_mies1
      @ 18,29  SAY smiaR(zamk_mies1,DatY->d_z_mies1)
#ifdef A_WA
      @ 19,29  SAY wart_mies1 PICTURE WAPIC
      @ 20,29  SAY wart_mies1/zamk_mies1 PICTURE WAPIC
#else
      @ 19,29  SAY zamk_mies1*(i_lam(DatY->d_z_mies1))->cenA PICTURE WAPIC
      @ 20,29  SAY (i_lam(DatY->d_z_mies1))->cenA PICTURE WAPIC
#endif
#ifdef A_GRAM
      @ 21,29  SAY zamk_mies1*(i_lam(DatY->d_z_mies1))->gram/1000 PICTURE "##### ###.##"
#endif
endif
if DatY->d_z_mies2>DatY->d_z_rok
      @ 17,48  SAY DatY->d_z_mies2
      @ 18,47  SAY smiaR(zamk_mies2,DatY->d_z_mies2)
#ifdef A_WA
      @ 19,47  SAY wart_mies2 PICTURE WAPIC
      @ 20,47  SAY wart_mies2/zamk_mies2 PICTURE WAPIC
#else
      @ 19,47  SAY zamk_mies2*(i_lam(DatY->d_z_mies2))->cenA PICTURE WAPIC
      @ 20,47  SAY (i_lam(DatY->d_z_mies2))->cenA PICTURE WAPIC
#endif
#ifdef A_GRAM
      @ 21,47  SAY zamk_mies2*(i_lam(DatY->d_z_mies2))->gram/1000 PICTURE "##### ###.##"
#endif
endif
      @ 17,66  SAY DatY->d_z_rok
      @ 18,65  SAY smiaR(zamkn_roku,DatY->d_z_rok)
#ifdef A_WA
      @ 19,65  SAY wart_roku PICTURE WAPIC
      @ 20,65  SAY wart_roku/zamkn_roku PICTURE WAPIC
#else
      @ 19,65  SAY zamkn_roku*(i_lam(DatY->d_z_rok))->cenA PICTURE WAPIC
      @ 20,65  SAY (i_lam(DatY->d_z_rok))->cenA PICTURE WAPIC
#endif
#ifdef A_GRAM
      @ 21,65  SAY zamkn_roku*(i_lam(DatY->d_z_rok))->gram/1000 PICTURE "##### ###.##"
#endif

#undef smiaR

  begin sequence

  SELECT INDX_MAT
  lam:=recno()

  ni:=nextni()
  s:=left(index,len(index)-len(ni))
  dbSkip(1)

  DBEval({||ni:=nextni(),s:=left(index,len(index)-len(ni))},,{|x|x:=val(ni),index=s .and. val(right(index,len(ni)))<=x+1},,,.F. )

  dbGoto(lam)

  for i:=len(ni) to 1 step -1
    if isdigit(subs(ni,i,1))
       ni:=s+pad(strtran(str(val(ni)+1,i)," ","0"),len(ni))
       exit
    endif
  next

  DevPos(9,78-len(ni) ); DevOut(ni,"RG+/BG" )
#ifdef A_SZYM
  if !empty(beg)
#ifdef STANY
     seek nr_mag+beg
#else
     seek beg
#endif
     if index=beg
        beg:=left(beg,3)+str(val(subs(index,4))+1,3)
     endif
     goto lam
  endif
#endif

#ifdef A_ANKER
    rt:=rodz_tow
    td:=tandem
    ST:=STATUS
#endif

#ifdef A_ZAGRODA
  rt:=space(len(rt))
  gr:=0
  UW := "BEZ UWAG "
#endif

#ifdef A_SHARP
   kna:=kasa_nazwa
   kwa:=kasa_waga
   kkl:=kasa_klaw
   ksh:=kasa_shift
   kgr:=kasa_grupa
#endif

  DO WHILE .T.
    getlist:={}
    dp:=data_popr
    pom:=nazwa
    j_m:=jm
    j_o:=jm_opcja
    p:=przel
#ifdef A_SZYM
    if !empty(beg)
       ni:=beg
    else
#else
    if empty(beg)
#endif
      ni:=index
#ifdef A_ZAGRODA
      rt:=rodz_tow
      gr:=gram
#endif
    endif
#ifdef A_KODY
    kw:=KoD
#endif
#ifdef A_ANKER
    sn:=shortname
#endif
#ifdef A_ZAGRODA
    if empty(beg)
#endif
    IF ""=(UW:=UWAGI)
      UW := "BEZ UWAG"
    ENDIF
    uw+=" "
#ifdef A_ZAGRODA
    endif
#endif
    if lamus#0
      @ 10,3 say "*"
    else
      @ 10,3 say " "
    endif
    IF empty(stos)
      @ 10,5 say "        "
    else
      @ 10,5 SAY "Lamus:"+str(LEN(STOS),2)
    endif
    @ 10,61 SAY "Symbol"
    @ 11,3  say dp
    @ 11,14 GET pom picture "@K"
#ifdef A_KAMIX
    s:=sww
    @ 10,61 SAY "      "
    @ 10,38 SAY 'Kod:' GET ni PICTURE '@!' valid {||ni:=UpP(ni),.t.}
    SAYL A_SWWKTM get s picture "@K"
    @ 11,61 SAY 'gram/'+trim(jm)+':' GET gr PICTURE "@EK ### ###"
#else
#ifdef A_ANKER
#ifndef A_WA
    if empty(beg)
      atail(getlist):preblock:={||.f.} 
    endif
#endif
//    atail(getlist):postblock:={|g|!g:changed .or. g:original=left(pom,12).or.(sn:=left(pom,12),getlist[ascan(getlist,{|x|x:name='sn'})]:display(),.t.)}
    GETL sn picture "@KS7"
    GETL ST PICTURE '#'
    SAYL ean13(ni)
    @ row(),col()-13 GET ni PICTURE '@K!S12' valid {|g|ni:=UpP(left(ni,12)),dispout(ean13(ni)),.t.} send cargo:=.t.
#else
/*
#ifdef A_7
    @ 11,61 GET ni PICTURE '@!R' valid {||ni:=UpP(ni),.t.}
#else
*/
    @ 11,61 GET ni PICTURE '@!R '+ INDEXPIC valid {||ni:=UpP(ni),.t.}
//#endif
#endif

#ifdef A_SWW
    s:=sww
 #ifndef A_SWWKTM
  #ifdef A_PKWiU
    getl s picture "@RK ##.##.##-##.##"
    @ 10,68 SAY "PKWiU"
  #else
    getl s picture "@K"
    @ 10,68 SAY A_SWW
  #endif
#endif
#endif
#ifdef A_SHARP
    getlist[1]:postblock:={|x,n|varput(getlist,'kna',left(n,16),.t.),.t.}
    @ 12,3 SAY 'Kasa:' GET kna
    SAYL 'Gr:' GET kgr PICTURE "@K" VALID {|e|e:=NIL,aczojs(grupy,@kgr,@e) .and. (varput(getlist,'pv',subs(grupy[e],17,2),.t.),.t.)}
    SAYL 'IloòÜ:' GET kwa VALID aczojs({'3 - cena ustawiona otwarta  ','2 - cena ustawiona zamkni©ta','1 - cena zawsze wprowadzana','0 - zablokowane'})
    SAYL 'K:' Get ksh VALID aczojs({'0','1','2'})
    SAYL 'Klaw:' GET kkl
#endif
#ifndef A_ANKER
#ifdef A_KODY
    @ 12,3 SAY A_KODY+":" get kw picture "@K"
#ifdef A_KODVAL
    atail(getlist):postblock:=A_KODVAL
#else
    atail(getlist):postblock:={|g,y|y:=UpP(trim(kw)),!g:changed.or.empty(y).or.len(y)=10.or.SUROWCE->(szukam({,,,,1,len(y),'Wybierz '+A_KODY,{||KoD+'≥'+nazwa},,y}).and.(kw:=KoD)='')}
#endif
#endif
#endif
#ifdef A_SWW
#ifdef A_SWWKTM
  #ifdef A_PKWiU
    @ 12,57 SAY "PKWiU:" get s picture "@RK ##.##.##-##.##"
  #else
    @ 12,75-len(A_SWWKTM)-len(s) SAY A_SWWKTM+":"
    getl s picture "@K"
  #endif
#ifdef A_SWWVAL
    atail(getlist):postblock:=A_SWWVAL
#endif
 #endif
#endif

#endif

#ifdef A_BIG
    ch:=cena_huty
    @ 12,4  say "Cena huty:" get ch picture WAPIC
#endif
#ifdef A_ALTCEN
    setpos(12,2)
#else
    setpos(13,2)
#endif
#ifndef A_FA
#ifndef A_WA
    ce:=cenA
#ifdef A_OLZBY
    sayl "Waga:"
#else
    sayl "Cena:"
#endif
    getl ce picture "@EK" //WAPIC
#define D_SKIPLINE
#endif
#endif
#ifdef A_KOMOR
    nz:=nr_zlec
    sayl "Nr konta:" get nz valid empty(nz) .or. aczojs(stanowis[mag_poz])
#ifndef D_SKIPLINE
#define D_SKIPLINE
#endif
#endif
#ifdef A_FA
    pv:=proc_VAT
    ce:=cena
 #ifdef A_PROCMAR
  #ifdef A_HLINK
    if hlink=NIL .or. !hlink[3]
       pm:=proc_mar
    else
       pm:=11
    endif
  #else
    pm:=proc_mar
  #endif
 #endif
 #ifdef A_CENVAT
    sayl "Cena zbytu:" get ce picture WAPIC
    sayl "Procent VAT:" get pv picture "@K XX" valid {|g|pv:=lower(pv),if(pv<"@",pv:=str(val(pv),2),),(ascan(stawkizby,pv)#0 .or. alarm('NIEWùAóCIWY PROCENT VAT!',,3,3)=NIL).and.(!g:changed.or.(kibord(chr(5)+chr(24)),.t.))}
 #else
#define D_BPOS (setpos(row(),53),devout(tran(WzVAT(1,ce,val(pv),.f.),WAPICT)),.t.)
    sayl "Cena zbytu:" get ce picture WAPIC valid D_BPOS
    sayl "Procent VAT:" get pv picture "@K XX" valid {|g|pv:=lower(pv),if(pv<"@",pv:=str(val(pv),2),),(ascan(stawkizby,pv)#0 .or. alarm('NIEWùAóCIWY PROCENT VAT!',,3,3)=NIL).and.D_BPOS}
    sayl "Brutto:"
    #undef D_BPOS
    @ row(),53 SAY tran(WzVAT(1,ce,val(pv),.f.),WAPICT)
 #endif
 #ifdef A_PROCMAR
    sayl "Maræa:" get pm picture "@K ###"
    if STANY->cenA_zaK<>0
      if round(pm - eval(memvar->liczm,STANY->cenA_zaK,ce,val(pv)) ,0 )<>0
         SAYL '%' COLOR 'R+/B'
      ELSE
         sayl "%"
      endif
      getlist[len(getlist)]:postblock:={|g|!g:changed .or. ni#index .or. (varput(getlist,'ce',eval(MEMVAR->liczCzM,STANY->cenA_zaK,val(pv),pm,p),g),.t.)}
      getlist[len(getlist)-2]:postblock:={|g|!g:changed .or. ni#index .or. ce=0 .or. (varput(getlist,'pm',eval(memvar->liczm,STANY->cenA_zaK,ce,val(pv)),.t.),.t.)}
    endif
 #endif

#ifdef A_ALTCEN
 #ifdef A_PROCMAR
    getlist[len(getlist)-2]:postblock:={|g,v|!g:changed .or. ni=index .and. (varput(getlist,'pm',eval(memvar->liczm,STANY->cenA_zaK,ce,val(pv)),.t.),.f.) .or. (x:=g,aeval(getlist,{|g,y|if(g:name='pm'.and.len(g:name)=3,eval(g:postblock,x,g:varget(),getlist,y),)}),.t.)}
    c1:=cena1;    c2:=cena2;    c3:=cena3
    pm1:=proc_mar1;    pm2:=proc_mar2;    pm3:=proc_mar3
    @ 13,3 say "Cena1:" get pm1 picture "@K ###"
    sayl "%"              get c1 picture WAPICT
    sayl "Cena2:"       get pm2 picture "@K ###"
    sayl "%"              get c2 picture WAPICT
    sayl "Cena3:"       get pm3 picture "@K ###"
    sayl "%"              get c3 picture WAPICT
  #ifdef A_ANKER
  #define D_KASA STANY->cenA_zaK
   #ifdef A_ZAGRODA
  if (ni#index)
    c3:=ce
    pm3:=0
    pm1:=10
  endif

       getlist[len(getlist)-8]:postblock:={|g,v|!g:changed .or. ni=index .and. (varput(getlist,'pm',eval(memvar->liczm,STANY->cenA_zaK,ce,val(pv)),.t.),varput(getlist,'pm1',max(10,pm/5),g),.f.) .or. (x:=g,aeval(getlist,{|g,y|if(g:name='pm3'.and.len(g:name)=3,eval(g:postblock,x,g:varget(),getlist,y),)}),.t.)}

       x:=getlist[len(getlist)-5]
       x:postblock:={|g|!g:changed .or. ni#index .or. pm1=0 .or. (varput(getlist,'c1',eval(MEMVAR->liczCzM,D_KASA,val(pv),pm1,p),g),.t.)}
       if round(pm1 - eval(MEMVAR->liczm,D_KASA,c1,val(pv)) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)-4]:postblock:={|g|!g:changed .or. (ni#index .or. c1=0 .or. (varput(getlist,'pm1',eval(memvar->liczm,D_KASA,c1,val(pv)),.t.),.f.),.t.)}

       x:=getlist[len(getlist)-3]
       x:postblock:={|g|!g:changed .or. ni#index .or. pm2=0 .or. (varput(getlist,'c2',eval(MEMVAR->liczCzM,D_KASA,val(pv),pm2,p),g),.t.)}
       if round(pm2 - eval(MEMVAR->liczm,D_KASA,c2,val(pv)) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)-2]:postblock:={|g|!g:changed .or. (ni#index .or. c2=0 .or. (varput(getlist,'pm2',eval(memvar->liczm,D_KASA,c2,val(pv)),.t.),.f.),.t.)}

       x:=getlist[len(getlist)-1]
       x:postblock:={|g|!g:changed .or. (varput(getlist,'c3',eval(MEMVAR->liczCzM,ce,0,pm3,p),.t.),.t.)}
       if round(pm3 - eval(MEMVAR->liczm,ce,c3,0) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)  ]:postblock:={|g|!g:changed .or. (varput(getlist,'pm3',eval(memvar->liczm,ce,c3,0),.t.),.t.)}
   #else
       getlist[len(getlist)-8]:postblock:={|g|!g:changed .or. (varput(getlist,'pm',eval(memvar->liczm,STANY->cenA_zaK,ce,val(pv)),.t.),.f.) .or. (x:=g,aeval(getlist,{|g,y|if(g:name=='pm1'.or.g:name=='pm2',eval(g:postblock,x,g:varget(),getlist,y),)}),.t.)}
       x:=getlist[len(getlist)-5]
       x:postblock:={|g|!g:changed .or. pm1<>0 .and. pm1==eval(memvar->liczm,  ce,c1,0 ) .or. (varput(getlist,'c1' ,eval(MEMVAR->liczCzM,ce,0,pm1),.t.),.t.)}
       if round(pm1 - eval(MEMVAR->liczm,ce,c1,0) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)-4]:postblock:={|g|!g:changed .or. (varput(getlist,'pm1',eval(memvar->liczm,  ce,c1,0 ),.t.),.t.)}
       x:=getlist[len(getlist)-3]
       x:postblock:={|g|!g:changed .or. pm2<>0 .and. pm2==eval(memvar->liczm,  ce,c2,0 ) .or. (varput(getlist,'c2' ,eval(MEMVAR->liczCzM,ce,0,pm2),.t.),.t.)}
       if round(pm2 - eval(MEMVAR->liczm,ce,c2,0) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)-2]:postblock:={|g|!g:changed .or. (varput(getlist,'pm2',eval(memvar->liczm,  ce,c2,0 ),.t.),.t.)}
       x:=getlist[len(getlist)-1]
       x:postblock:={|g|!g:changed .or. pm3<>0 .and. pm3==eval(memvar->liczm,D_KASA,c3,val(pv) ) .or. (varput(getlist,'c3' ,eval(MEMVAR->liczCzM,D_KASA,val(pv),pm3),.t.),.t.)}
       if round(pm3 - eval(MEMVAR->liczm,D_KASA,c3,val(pv)) ,0 )<>0
          @ x:row(),x:col()+4 SAY "%" COLOR "R+/B"
       endif
       getlist[len(getlist)  ]:postblock:={|g|!g:changed .or. (varput(getlist,'pm3',eval(memvar->liczm,D_KASA,c3,val(pv) ),.t.),.t.)}
   #endif
  #else
  #define D_KASA ce
       getlist[len(getlist)-5]:postblock:={|g|!g:changed .or. pm1=0 .or. (c1:=eval(MEMVAR->liczCzM,D_KASA,0,pm1,p),g:=getlist[ascan(getlist,{|x|x:name="c1"})],g:display(),g:=getlist[ascan(getlist,{|x|x:name=='c2'})],pm2=0 .or. (c2:=eval(MEMVAR->liczCzM,c1,0,pm2,p),g:display(),.t.),g:=getlist[ascan(getlist,{|x|x:name=='c3'})],pm3=0 .or. (c3:=eval(MEMVAR->liczCzM,c1,0,pm3,p),g:display(),.t.),.t.)}
       getlist[len(getlist)-4]:postblock:={|g|!g:changed .or. c1=0 .or. (pm1:=eval(memvar->liczm,D_KASA,c1,0),g:=getlist[ascan(getlist,{|x|x:name=='pm1'})],g:display(),g:=getlist[ascan(getlist,{|x|x:name=='c2'})],pm2=0 .or. (c2:=eval(MEMVAR->liczCzM,c1,0,pm2,p),g:display(),.t.),g:=getlist[ascan(getlist,{|x|x:name=='c3'})],pm3=0 .or. (c3:=eval(MEMVAR->liczCzM,c1,0,pm3,p),g:display(),.t.),.t.)}
       getlist[len(getlist)-3]:postblock:={|g|!g:changed .or. pm2=0 .or. (c2:=eval(MEMVAR->liczCzM,c1,0,pm2,p),g:=getlist[ascan(getlist,{|x|x:name="c2"})],g:display(),.t.)}
       getlist[len(getlist)-2]:postblock:={|g|!g:changed .or. c2=0 .or. pm2#(pm2:=eval(memvar->liczm,c1,c2,0)).and.(g:=getlist[ascan(getlist,{|x|x:name=='pm2'})],g:display(),.t.),.t.}
       getlist[len(getlist)-1]:postblock:={|g|!g:changed .or. pm3=0 .or. (c3:=eval(MEMVAR->liczCzM,c1,0,pm3,p),g:=getlist[ascan(getlist,{|x|x:name="c3"})],g:display(),.t.)}
       getlist[len(getlist)  ]:postblock:={|g|!g:changed .or. c3=0 .or. pm3#(pm3:=eval(memvar->liczm,c1,c3,0)).and.(g:=getlist[ascan(getlist,{|x|x:name=='pm3'})],g:display(),.t.),.t.}
  #endif
 #else
    c1:=cena1;    c2:=cena2;    c3:=cena3
    @ 13,3 say "Cena1:" get c1 picture WAPICT
          sayl "Cena2:" get c2 picture WAPICT
          sayl "Cena3:" get c3 picture WAPICT
 #endif
#endif

#ifndef D_SKIPLINE
#define D_SKIPLINE
#endif

#endif

#ifdef D_SKIPLINE
#undef D_SKIPLINE
    setpos(14,2)
#endif

#ifdef A_JMALT
    SAYl 'Miara:' GET j_m picture "@K" VALID JM(@j_m,@j_o,@p,row())
    sayl "=" get p picture "@K ######.###"
    GETl j_o picture "@K!" VALID !empty(j_o) .or. ACZOJS(JMIAR)
#else
    SAYl 'Miara:' GET j_m picture "@K" VALID JM(@j_m,@j_o,@p,row())
#ifdef A_JMO
    sayl "(*" get p picture "@K ######.###" valid P#0 .and. (p=1 .or. j_m#'szt'.or.(j_o:="X"+str(p,3),.t.)).or.NIL=alarm("PRZELICZNIK MUSI BYè WI®KSZY OD ZERA !",,5,3)
#else
    sayl "(*" get p picture "@K ######.###" valid ;
        P#0 .or. NIL=alarm("PRZELICZNIK MUSI BYè WI®KSZY OD ZERA !",,5,3)
#endif
    SAYl '= 1' GET j_o picture "@K!" VALID !empty(j_o) .or. ACZOJS(JMIAR)
    sayl ")"
#endif

#ifdef A_GRAM
#ifndef A_ZAGRODA
    gr:=gram
#endif    
    sayl 'gram/'+trim(jm)+':' GET gr PICTURE "@EK #####" WHEN {||if(jm='kg',gr:=1000,),.t.}
#endif

#ifdef A_NOZAP
    id:=info
//#ifdef A_KASA
#ifdef A_SURLINK
    a:=select()
    sel('SUROWCE','SUR_KOD')
    SAYl 'Dieta:'
    GETl id picture "@K! "+replicate("X",len(surowce->skladnik)) VALID {|g|!g:changed .or. SUROWCE->(szukam({,,,,1,len(trim(id)),'Surowce z Diety',{||field->skladnik+"|"+nazwa+"|"+field->jmag+' ='+str(field->przel)+' '+field->jedn},{|_skey,_s D_MYSZ|surinfo(_skey,_s,@id,ni D_MYSZ)} ,UpP(Trim(id))}).and.(setpos(g:row,g:col+len(field->skladnik)+1),dispout(left(nazwa,78-col())),.t.) )}
    if dbseek(left(id,len(field->skladnik)))
       sayl left(nazwa,78-col())
    endif
    select (a)
#else
#ifdef A_KKR
    SAYl 'Kod kreskowy:'
#else
    SAYl 'Info:'
#endif
    GETl id picture "@KS"+ltrim(str(78-col()))
    if valtype(info)='M'
      atail(getlist):cargo:=.t.
    endif  
#endif
//#endif
#else
    z_min:=zapas_min
    z_max:=zapas_max
    SAYl 'Zapas min:' GET z_min PICTURE "@K ######"+ILPIC
    SAYl 'max:' GET z_max PICTURE "@K ######"+ILPIC
#endif
#ifndef A_FA
#ifdef A_PV
    pv:=proc_VAT
    @ 14,3 say "Procent VAT:" get pv picture "@K XX"
#endif
#endif
#ifdef A_TRWALOSC
    wa:=waznosc
    @ 15,3  say "WaænoòÜ" GET wa picture "@K ###"
    @ 15,15 say "dni" GET uw picture "@KS58" send cargo:=.t.
#else
    @ 15,3 GET uw picture "@KS73" send cargo:=.t.
#endif
#ifdef A_ANKER
    @ 16,3 SAY "Kod:" GET kw picture "@!" valid {||kw:=UpP(kw),.t.}
    SAYL "Stat.Towaru:" GET rt picture "@! NNN" ;
           SEND reader:={|g|setkey(asc("\"),{|p,g|g:=getactive(),p:=setkey(asc("\"),NIL),statedit(g,ni),setkey(asc("\"),p)}),getreader(g),setkey(asc("\"),NIL)}
    SAYL "Tandem:" GET td PICTURE '@K!' 
#ifdef A_ZAGRODA
    z_min:=zapas_min
    SAYl 'Zapas min:' GET z_min PICTURE "@K ######"+ILPIC
#endif   
//    S12' send cargo:=.t.
#endif
#ifdef A_7
#ifdef A_KAMIX
    @ 16,43 SAY "Cena1 brutto za 1 "+trim(j_o)+': '+ltrim(tran(c1*p*(1+val(pv)*.01),WAPICT))+' zà'
#else
#ifdef A_CENVAT
    @ 16,43 SAY "Cena brutto za 1 "+trim(j_o)+': '+ltrim(tran(ce*p,WAPICT))+' zà'
#else
    @ 16,43 SAY "Cena brutto za 1 "+trim(j_o)+': '+ltrim(tran(ce*p*(1+val(pv)*.01),WAPICT))+' zà'
#endif
#endif
#endif
#ifdef A_LAN
  if !lock
    setpos(23,30)
    alarm("POPRAWIANIE NIEMOΩLIWE DO CZASU ZWOLNIENIA PRZEZ INNYCH UΩYTKOWNIK‡W SIECI",,9999,3)
    break
  endif
#endif
    do while .t.
    if !empty(mes)
       for i=len(mes) to 1 step -1
           message(mes[i])
       next
       mes:={}
    endif
    IF ""=UW
      UW = "BEZ UWAG"
    ENDIF
    uw+=" "
    if empty(stos)
       select indx_mat
    else
       select ind_lam
    endif
    go lam
    READmodal(getlist,@pos)
#ifdef A_HBGET
    //pos:=__GetListLast():ReadStats( 14 ) //GetListPos()
#endif

#ifdef A_MYSZ
    i:=readkey()
    if i=K_ESC .or. i=GE_MOUSE
#else
    IF readkey()=K_ESC
#endif
      break
    ENDIF
    IF readkey()=K_PGUP
      IF LAMus=0
        LOOP
      ENDIF
      lam:=lamus
      aadd(stos,recno())
      SELECT IND_LAM
      GO LAM
      exit
    ENDIF
    IF readkey()=K_PGDN .AND. !empty(stos) // PGdn
      LAM:=atail(stos)
      stos:=asize(stos,len(stos)-1)
      if empty(stos)
        select indx_mat
      endif
      go lam
      exit
    ENDIF

  IF empty(POM) .or. empty(ni)
    IF INDX_MAT->(EOF())
      BREAK
    ENDIF
    SELECT main
    set order to TAG MAIN_IND
#ifndef STANY
    go top
    txt=nr_mag
    do while !eof() .and. !dbseek(txt+indx_mat->index)
      seek txt+"˛"
      txt=nr_mag
    enddo
    if !eof()
      alarm("TEGO MATERIAùU NIE MOΩNA SKASOWAè;RUCHY NA MAGAZYNIE;"+MAGAZYNY[max(1,ascan(magazyny,NR_MAG))],,,3)
    ELSE
      SELECT STANY
      seek indx_mat->index
#ifdef A_WA
#ifdef A_LAN
      EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
      DELETE REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#else
#ifdef A_LAN
      EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
      DELETE REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#endif
      IF INDX_MAT->INDEX=INDEX
#else
    if dbseek(indx_mat->nr_mag+indx_mat->index)
      alarm("TEGO MATERIAùU NIE MOΩNA SKASOWAè;RUCHY NA MAGAZYNIE;"+MAGAZYNY[max(1,ascan(magazyny,NR_MAG))],,,3)
    else
      select STANY
#ifdef A_WA
      IF WARTOSC#0 .OR. STAN#0
#else
      IF STAN#0
#endif
#endif
        alarm("TEGO MATERIAùU NIE MOΩNA SKASOWAè;STAN NA MAGAZYNIE;"+MAGAZYNY[max(1,ascan(magazyny,NR_MAG))],,,3)
      ELSEIF Tak('KASOWANIE TEGO MATERIALU',22,15,.F.,.F.)
        SELECT INDX_MAT
        Rf:=.t.
        DELETE
      ENDIF
    ENDIF
    break
  endif
  SELECT INDX_MAT
  rcrd=recno()
  uw:=trim(uw)
  IF UW="BEZ UWAG"
    UW:=""
  ENDIF
  setpos(12,40)
#ifdef A_KODY
#ifndef A_DIETA
  IF kw#KoD
     SET ORDER TO TAG INDX_KOD
     if dbseek(nm+kw)
        //alarm('TAKI KOD JUΩ ISTNIEJE !;'+nazwa,,5,3)
        //go rcrd
        //pos:=ascan(getlist,{|g|g:name='kw'})
        //kw:=KoD
        //loop
        aadd(mes,message('TAKI KOD JUΩ ISTNIEJE !;'+nazwa))
     endif
     go rcrd
  ENDIF
#endif
#else
#ifdef A_ANKER
  IF kw#KoD
     SET ORDER TO TAG INDX_KOD
     if dbseek(nm+kw)
        alarm('TAKI KOD JUΩ ISTNIEJE !;'+nazwa,,5,3)
        go rcrd
        pos:=ascan(getlist,{|g|g:name='kw'})
        kw:=KoD
        loop
//      aadd(mes,message('TAKI KOD JUΩ ISTNIEJE !;'+nazwa))
     endif
     go rcrd
  ENDIF
#endif
#endif
  if !empty(stos)
      aadd(mes,message('Zmiany "na bazie" starej wersji karty materiaàu.'))
  endif

  IF ni # index
     SET ORDER TO TAG INDX_NUM
     IF dbSEEK(nm+ni)
       SELECT main
#ifndef STANY
    go top
    txt=nr_mag
    do while !eof() .and. !dbseek(txt+indx_mat->index)
      seek txt+"˛"
      txt=nr_mag
    enddo
    if !eof()
          SELECT INDX_MAT
          alarm('TAKI KOD MATERIAùU JUΩ ISTNIEJE !;'+nazwa,,5,3)
          go rcrd
          pos:=ascan(getlist,{|g|g:name='ni'})
          loop
    ELSE
      SELECT STANY
      seek indx_mat->index
#ifdef A_WA
#ifdef A_LAN
      EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
      DELETE REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#else
#ifdef A_LAN
      EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
      DELETE REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#endif
      SELECT INDX_MAT
      IF STANY->INDEX=INDEX
#else
       set order to TAG MAIN_IND
       if dbseek(indx_mat->nr_mag+indx_mat->index)
          SELECT INDX_MAT
          alarm('TAKI KOD MATERIAùU JUΩ ISTNIEJE !;'+nazwa,,5,3)
          go rcrd
          pos:=ascan(getlist,{|g|g:name='ni'})
          loop
       else
         select STANY
#ifdef A_WA
         IF WARTOSC#0 .OR. STAN#0
#else
         IF STAN#0
#endif
#endif
          alarm('TAKI KOD MATERIAùU JUΩ ISTNIEJE !;'+nazwa,,5,3)
          go rcrd
          pos:=ascan(getlist,{|g|g:name='ni'})
          loop
         ENDIF
         if 2=alarm('TAKI KOD MATERIAùU JUΩ ISTNIEJE,:;'+trim(nazwa)+";ale kartoteka jest pusta. CO ROBIè ?",{"KontynuowaÜ","ZmieniÜ kod"},1,2)
          go rcrd
          pos:=ascan(getlist,{|g|g:name='ni'})
          loop
         ENDIF
         LOCK recno()
         DELETE
         Rf:=.t.
         UNLOCK recno()
         go rcrd
       endif
     ENDIF
  ENDIF

  IF ni # index
  SET ORDER TO TAG INDX_NUM
     IF !dbSEEK(nm+ni)
        go rcrd
        new:=NIL
#ifdef A_ANKER
        //0-brak w kasie,1-w kasie,+2-do poprawy,+4-usun•Ü z kasy,+8-nie udany transfer
        if status%2=0
#endif
        SELECT main
        set order to TAG MAIN_IND
        go top
#ifndef STANY
        txt:=nr_mag
        do while !eof() .and. !dbseek(txt+indx_mat->index)
           seek txt+"˛"
           txt=nr_mag
        enddo
        if eof()
           SELECT STANY
           seek indx_mat->index
#ifdef A_WA
#ifdef A_LAN
           EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
           DELETE REST WHILE WARTOSC=0 .AND. STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#else
#ifdef A_LAN
           EXEC {||dbDELETE(),dbrunlock(recno())} REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX .AND. RECLOCK(.F.,,.F.,,recno())
#else
           DELETE REST WHILE STAN=0 .AND. INDEX=INDX_MAT->INDEX
#endif
#endif
          IF INDX_MAT->INDEX#INDEX
#else
          if !dbseek(indx_mat->nr_mag+indx_mat->index)
             select STANY
#ifdef A_WA
             IF WARTOSC=0 .and. STAN=0
#else
             IF STAN=0
#endif
#endif
                tone(262,2)
                setpos(20,40)
                new:=alarm("CO ROBIè ?;( [Esc] - rezygnacja )",{"DopisaÜ now•","PoprawiÜ star•"})

             ENDIF
          endif
          select indx_mat
#ifdef A_ANKER
          endif
#endif
          txt:=NIL
          if new=NIL
             SET ORDER TO TAG INDX_NAZ
             IF dbSEEK(nm+UpP(pom))
               aadd(mes,message('NAZWA NIE JEST UNIKALNA !'))
             ENDIF
             tone(262,2)
             setpos(20,40)
             new:=alarm("CO ROBIè ?;( [Esc] - rezygnacja )",{"DopisaÜ now• kart© materiaàu"})
          endif
          if new=0
              go rcrd
              pos:=ascan(getlist,{|g|g:name='ni'})
              new:=.f.
              loop
          endif
          if new=1
             APPEND BLANK
          endif
          new:=.t.
          index := ni
#ifdef A_GRAM
          gram := gr
#endif
#ifdef A_KODY
          KoD := kw
#endif
#ifdef A_ANKER
          status:= st:= 4*int(status/4)+if(isdigit(index),2,0)
          zaznacz:=if(status%4=2,1,0)
          shortname := sn
          rodz_tow:=rt
          tandem:=td
#ifdef A_ZAGRODA          
          zapas_min := z_min
#endif          
#endif
#ifdef A_SHARP
   kasa_nazwa:=  kna
   kasa_waga :=  kwa
   kasa_klaw :=  kkl
   kasa_shift:=  ksh
   kasa_grupa:=  kgr
#endif
#ifdef A_KOMOR
          nr_zlec:=nz
#endif
#ifdef A_SWW
          sww:=s
#endif
#ifdef A_FA
          cena:= ce
#ifdef A_ALTCEN
          cena1:=c1;cena2:=c2;cena3:=c3
#ifdef A_PROCMAR
          proc_mar1:=pm1;proc_mar2:=pm2;proc_mar3:=pm3
#endif
#endif
          proc_vat:=pv
#ifdef A_PROCMAR
#ifdef A_HLINK
          if hlink=NIL .or. !hlink[3]
             proc_mar:=pm
          endif
#else
          proc_mar:=pm
#endif
#endif
#ifdef A_BIG
          cena_huty:=ch
#endif
#else
#ifndef A_WA
           cenA:= ce
#endif
#ifdef A_PV
          proc_vat:=pv
#endif
#endif
          jm := j_m
          nazwa := pom
          data_POPR := DA
#ifdef A_NOZAP
          info  := id
#else
          zapas_min := z_min
          zapas_max := z_max
#endif
          jm_opcja := j_o
          przel := p
          UWAGI := UW
#ifdef STANY
          nr_mag := mag_biez
#endif
#ifdef A_TRWALOSC
          waznosc := wa
#endif
        ELSE
          alarm('TAKI KOD MATERIAùU JUΩ ISTNIEJE !;'+nazwa,,5,3)
          go rcrd
          pos:=ascan(getlist,{|g|g:name='ni'})
          loop
        ENDIF
     ELSE
#ifdef A_ANKER
#ifndef A_ZAGRODA
          if st%2=0
            if  j_m='kg  ' .and. rt#'1'  .or. j_m#'kg  '.and.rt='1'
              clear typeahead
              alarm("Zmieniam status towaru z powodu zmiany jednostki miary z/na kg.",,,3)
              rt:=if(j_m='kg','1','0')+subs(rt,2)
              pos:=ascan(getlist,{|g|g:name='j_m'})
              loop
            endif
          elseif pv#proc_vat
             clear typeahead
             alarm("NIE MOΩESZ ZMIENIAè STAWKI VAT NA TOWARZE OBECNYM W KASIE !",,5,3)
             pv:=proc_vat
             pos:=ascan(getlist,{|g|g:name='pv'})
             loop
          elseif rodz_tow#left(rt,1)
             clear typeahead
             alarm("NIE MOΩESZ ZMIENIAè STATUSU NA TOWARZE OBECNYM W KASIE !",,5,3)
             rt:=rodz_tow
             pos:=ascan(getlist,{|g|g:name='rt'})
             loop
          endif
#endif
#endif
#ifndef STANY
        SELECT STANY
        rcrd:=recno()
        txt:=nr_mag
        SEEK INDX_MAT->INDEX
        DO WHILE INDEX=INDX_MAT->INDEX
#ifdef A_WA
           IF (WARTOSC#0 .OR. STAN#0).and. nr_mag#txt
              aadd(mes,message("STAN NIEZEROWY NA MAGAZYNIE:;"+MAGAZYNY[max(1,ascan(magazyny,NR_MAG))]+";"+str(STAN)+" "+INDX_MAT->JM+" "+tran(WARTOSC,WAPIC)+" zà."))
#else
           IF STAN#0 .and. nr_mag#txt
              aadd(mes,message("STAN NIEZEROWY NA MAGAZYNIE:;"+MAGAZYNY[max(1,ascan(magazyny,NR_MAG))]+";"+str(STAN)+" "+INDX_MAT->JM))
#endif
           ENDIF
           SKIP
        ENDDO
        GO rcrd
        SELECT INDX_MAT
#endif
        dp:=max(DatY->d_z_mies1+1,DP)
        a:=max(da,dp)
        tone(262,2)
        setpos(20,40)
        aadd(mes,i:=message("Zmiana obowi•zuje od:;stara data:;data zakupu:;nowa data:;Esc - rezygnacja z poprawy"))
        setcolor(_snorm)
        txt:={getnew(i[3]-4,i[4]-12,{||dp}):display(),;
              getnew(i[3]-3,i[4]-12,{||max(dp,STANY->data_przy)}):display(),;
              getnew(i[3]-2,i[4]-12,{|x|if(x=NIL,a,a:=x)}):display()}
        txt[1]:preblock:=txt[2]:preblock:={|g|g:exitstate:=GE_NOEXIT,.t.}
        txt[1]:postblock:=txt[2]:postblock:={|g|if(g:exitState=GE_ENTER,g:exitState:=GE_WRITE,),.t.}
        txt[3]:postblock:={||if(a<=DatY->d_z_mies1,(a:=DatY->d_z_mies1+1,ALARM('NIE COFAJ SI® DO ZAMKNI®TEGO MIESI§CA!',,,3),.f.),.t.)}
        #ifdef A_FA
          i:=if(ce=cena.and.pv=proc_vat,1,3)
        #else
          i:=1
        #endif
        readmodal(txt,@i);txt:=NIL

        //i:=alarm("Zmiana obowi•zuje od:;Esc - rezygnacja z poprawy;(stara data  data zakupu  nowa data)",{dtoc(DP),dtoc(max(dp,STANY->data_przy)),dtoc(da)})
        if readkey()#K_ESC
#ifdef A_HBGET
           //i:=__GetListLast():ReadStats( 14 ) // GetListPos()
#endif
           if i=3
              dp:=a
           elseif i=2
              dp:=max(dp,STANY->data_przy)
           endif
           if dp>data_popr .and. dp>DatY->d_z_rok+1
              SELECT IND_LAM
              APPEND BLANK
              for i=1 to fcount()
                  fieldput(i,indx_mat->(fieldget(i)))
              next
              INDX_MAT->lamus:=recno()
              SELECT INDX_MAT
           ELSEif !empty(stos)
              INDX_MAT->lamus:=IND_LAM->lamus
           ENDIF
           Rf:=_sbeg#1 .and. UpP(pom)#UpP(NAZWA)
#ifndef STANY
#ifndef A_STSIMPLE
           txt:=nazwa
           if UpP(nazwa)#UpP(pom)
              select STANY
              seek ni
              ar:={}
#ifdef A_LAN
              exec {||unlink:=.t.,aadd(ar,recno())} rest while index=ni .and. reclock(.f.,"NIE POTRAFI® ZMIENIè NAZWY W MAGAZYNIE "+NR_MAG,.f.,,recno())
              if index=ni
                 select indx_mat
                 select stany
                 aeval(ar,{|r|dbgoto(r),UNLINK:=.f.,dbrunlock(r)})
                 go rcrd
                 select indx_mat
                 loop
              endif
#else
              exec {||UNLINK:=.t.,aadd(ar,recno())} rest while index=ni
#endif
              select indx_mat
              nazwa:=pom
              select stany
#ifdef A_LAN
              aeval(ar,{|r|dbgoto(r),unlink:=.f.,dbrunlock(r)})
#else
              aeval(ar,{|r|dbgoto(r),unlink:=.f.})
#endif
              go rcrd
           elseif ! nazwa==pom
              indx_mat->nazwa:=pom
           endif
           SELECT INDX_MAT
#endif
#endif
#ifdef A_GRAM
          gram := gr
#endif
#ifdef A_KODY
          KoD := kw
#endif
#ifdef A_ANKER
          STATUS:=ST
          if isdigit(index)
             if cena#ce.or.pv#proc_vat.or.rt#rodz_tow.or.td#tandem.or.status%8>=4
               status:=status%4+2
             endif
             if cena#ce.or.nazwa#pom
                zaznacz:=1
             endif
          endif
          shortname := sn
          rodz_tow:=rt
          tandem:=td
#ifdef A_ZAGRODA          
          zapas_min := z_min
#endif          
#endif

#ifdef A_SHARP
   kasa_nazwa:=  kna
   kasa_waga :=  kwa
   kasa_klaw :=  kkl
   kasa_shift:=  ksh
   kasa_grupa:=  kgr
#endif
#ifdef STANY
           nazwa := pom
#else
#ifdef A_STSIMPLE
           nazwa := pom
#endif
#endif
#ifdef A_KOMOR
           nr_zlec:=nz
#endif
#ifdef A_SWW
           sww:=s
#endif
#ifdef A_FA
#ifdef A_PROCMAR
#ifdef A_HLINK
           if hlink=NIL .or. !hlink[3]
              proc_mar:=pm
           endif
#else
           proc_mar:=pm
#endif
#endif
           proc_vat:=pv
           cena:= ce
#ifdef A_ALTCEN
           cena1:=c1;cena2:=c2;cena3:=c3
#ifdef A_PROCMAR
           proc_mar1:=pm1;proc_mar2:=pm2;proc_mar3:=pm3
#endif
#endif
#ifdef A_BIG
           cena_huty:=ch
#endif
#else
#ifndef A_WA
           cenA:= ce
#endif
#ifdef A_PV
           proc_vat:=pv
#endif
#endif
           jm := j_m
#ifdef A_NOZAP
           info  := id
#else
           zapas_min := z_min
           zapas_max := z_max
#endif
           data_POPR := Dp
           jm_opcja := j_o
           przel := p
           UWAGI := UW
#ifdef A_TRWALOSC
           waznosc := wa
#endif
        else
           loop
        ENDIF
     ENDIF

     break
   enddo
  ENDDO
  end sequence
  if !empty(mes)
       for i=len(mes) to 1 step -1
           message(mes[i])
       next
  endif
  SET COLOR TO (_SNORM)
  set cursor on
  restscreen(9,1,22,78,scr)

  pop_norec(stat)

#ifndef STANY
  unlock in indx_mat
#endif
  unlock in STANY

  if .t.=rf .or. .t.=new
    _spocz:=left(_spocz,len(_spocz)-_slth)
    _slth:=0
    refresh(,_s)
  ELSE
    _skip(0,,_s)
    if _srec[_sm]=recno()
      REFRESH LINE _sm+_srow1-1 DIRECTION 0
    else
      refresh(,_s)
    endif
  ENDIF
RETURN
*****************
FUNCTION LTAB(D,_s)
field ilosc,data,smb_dow,nr_dowodu,pozycja,nr_zlec
#ifdef A_LPNUM
#define D_LP0 str(0,A_LPNUM) //'  0'
#define D_LP1 str(1,A_LPNUM) //'  1'
#define D_LPPUT(x) str(x,A_LPNUM)
#define D_LPVAL(x) val(x)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPSTR1(x) str(D_LPVAL(x),1)
#else
#define D_LP0 '0'
#define D_LP1 '1'
#define D_LPVAL(x) (asc(x)-48)
#define D_LPSTR(x) str(asc(x)-48,3)
#define D_LPSTR1(x) x
#define D_LPPUT(x) chr(x+48)
#endif
local sel,prptxt
#ifdef A_WA
field wartosc
#else
field cenA
local wartosc:=ilosc*(sel:=(i_lam(data)))->cenA
#endif
#ifdef A_JMO
#define D_JMOFIX(s,p) s:=val(substr(p,-21));if(miar_opcja,s:=s*indx_mat->przel+if(s<0,-1,1)*val(subs(p,-14)),)
#define smiaR(x,y) if(miar_opcja,if(x%indx_mat->przel=0,str(x/indx_mat->przel,y-4)+"    ",stuff(str(int(x/indx_mat->przel)+x%indx_mat->przel/1000,y,3),y-3,1,"r")),str(x,y,ILDEC))
#else
#define D_JMOFIX(s,p) s:=val(substr(p,-21))
#define smiaR(x,y) str(x,y,ILDEC)
#endif
DO CASE
   CASE D=1 .or. d=3 // w dol
      s:=S2+=ILOSC
#ifdef A_WA
      w:=w2+=wartosc
#else
      w:=w2:=s*(sel)->cenA
#endif
      IF d=3
         prptxt:=getscrtxt(savescreen(_srow1,_scol2-22,_srow1,_scol2-1))
         S1:=S3
         D_JMOFIX(s3,prptxt)
         w1:=w3
         w3:=VAL(RIGHT(prptxt,10))
      ENDIF  
   
   CASE D=-1 .or. d=-3 // w gore
      S:=S3:=S1
      S1-=ILOSC
#ifdef A_WA
      w:=w3:=w1
      w1-=wartosC
#else
      w:=w3:=s*(sel)->cenA
#endif
      IF d=-3
         prptxt:=getscrtxt(savescreen(_srow2-1,_scol2-22,_srow2-1,_scol2-1))
         D_JMOFIX(s2,prptxt)
         w2:=VAL(RIGHT(prptxt,10))
      ENDIF

   CASE D=-2 // GORA
      s1:=sb
      S3:=S2:=S:=S1+ILOSC
      w1:=wB
#ifdef A_WA
      w3:=W2:=W:=W1+wartosC
#else
      w3:=W2:=W:=s*(sel)->cenA
#endif
       
   CASE D=2 // DOù
#ifdef A_LAN
#ifndef STANY
      STANY->(dbgoto(srec))
      if STANY->(deleted())
          stany->(ordsetfocus("STAN_MAG"))
          stany->(dbseek(nr_mag+index,.f.))
          srec:=stany->(recno())
      endif
       se:=STANY->stan
#ifdef A_WA
       we:=STANY->wartosc
#else
       we:=se*(i_lam(STANY->data_zmian))->cenA
#endif

#endif
#endif
      S:=S2:=S3:=SE
      S1:=S3-ILOSC
      w:=W2:=w3:=we
#ifdef A_WA
      w1:=w3-WARTOSC
#endif

   otherwise // od S bieæ
      S1:=S
      S3:=S2:=S+=ILOSC
#ifdef A_WA
      W1:=W
      W3:=W2:=w+=wartosc
#else
      W3:=W2:=w:=s*(sel)->cenA
#endif

ENDCASE
#ifdef A_ZLEC11
#define nr_zleC pad(strtran(nr_zlec,'     '),6)
#endif

#ifdef A_OLZA
RETURN(DTOV(data)+if(data>DatY->d_z_mies1,"≥","|")+smb_dow+nr_dowodu+"/"+D_LPSTR1+"≥"+nr_zlec+"≥";
  +if(NR_MAG+smb_dow$dok_rozch,"         ≥"+smiaR(-ilosc,9)+"≥",smiaR(ilosc,9)+"≥         ≥");
  +str(wartosC,10,CEOKR)+if(INDX_MAT->data_POPR>DATA,"|","≥")+smiaR(s,10)+"≥"+str(w,10,CEOKR))
#else
#ifdef A_ANKER
#ifdef A_ZAGRODA
dispout(DTOV(data)+if(data>DatY->d_z_mies1,"≥","|")+smb_dow+nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+"≥"+nr_zleC+"≥";
  +if(NR_MAG+smb_dow$dok_rozch,"         ≥"+smiaR(-ilosc,9)+"≥",smiaR(ilosc,9)+"≥         ≥");
  +str(wartosC,10,CEOKR)+if(INDX_MAT->data_POPR>DATA,"|","≥")+smiaR(s,10)+"≥"+str(w,10,CEOKR),if(year(data)<year(DatY->d_z_rok+1),'GR+/N',if(smb_dow='PA' .and. ilosc<>0,'W/R','W/N')))
RETURN('')
#else
dispout(DTOV(data)+if(data>DatY->d_z_mies1,"≥","|")+smb_dow+nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+"≥"+nr_zleC+"≥";
  +if(NR_MAG+smb_dow$dok_rozch,"         ≥"+smiaR(-ilosc,9)+"≥",smiaR(ilosc,9)+"≥         ≥");
  +str(wartosC,10,CEOKR)+if(INDX_MAT->data_POPR>DATA,"|","≥")+smiaR(s,10)+"≥"+str(w,10,CEOKR),if(year(data)<year(DatY->d_z_rok+1),'GR+/N',{'W/N','W/N','W/N','W/N','W/N','W/N','W/R'}[dow(data)]))
RETURN('')
#endif
#else
RETURN(DTOV(data)+if(data>DatY->d_z_mies1,"≥","|")+smb_dow+nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+"≥"+nr_zleC+"≥";
  +if(NR_MAG+smb_dow$dok_rozch,"         ≥"+smiaR(-ilosc,9)+"≥",smiaR(ilosc,9)+"≥         ≥");
  +str(wartosC,10,CEOKR)+if(INDX_MAT->data_POPR>DATA,"|","≥")+smiaR(s,10)+"≥"+str(w,10,CEOKR))
#endif
#endif
#undef smiaR
#undef nr_zleC
*****************************************
FUNCTION TAB(_skey,_s)
static dg
local prptxt,u,v,w,x,y,z
field ilosc,index,smb_dow,nr_dowodu,nr_mag,dost_odb,kontrahent
field zamkn_roku,zamk_mies1,zamk_mies2,wart_roku,wart_mies1,wart_mies2 IN STANY
memvar chg
#ifdef STANY
#define irec srec
#else
memvar irec
#endif
#ifdef A_WA
field wartosc
#else
field data
#endif

   DO CASE 
      CASE _SKEY=0
      dg:=DatY->data_gran
      SELECT MAIN
      SET ORDER TO TAG MAIN_IND
      SET RELATION TO
      SET FILTER TO
      SEEK _spocz
      if _spocz#nr_mag+index
         RETURN(.T.)
      ENDIF      
      _snagkol:=1
      _sbf:=.t.
      setpos(row()+2,col())
#ifdef A_ANKER
      kibord('')
#endif
#ifdef A_LAN
      _skproc[11]:=_skproc[12]:=NIL

      case (_skey=11 .or. _skey=12)


      STANY->(dbgoto(srec))
#ifndef STANY
      if STANY->(deleted())
         stany->(ordsetfocus("STAN_MAG"))
         stany->(dbseek(nr_mag+index,.f.))
         srec:=stany->(recno())
      endif
#endif
#ifdef A_WA
      if se#STANY->stan .or. we#STANY->wartosc
      w2:=we:=STANY->wartosc
#else
      if se#STANY->stan
      w2:=we:=STANY->stan*(i_lam(STANY->data_zmian))->cenA
#endif
      alarm("UWAGA SIEè: ZMIENIù SI® STAN KO„COWY",,,3)
      s2:=se:=STANY->stan
      _sef:=.f.
      kibord(chr(30))
      endif
#else
#ifdef A_DIETA
      _skproc[11]:=_skproc[12]:=NIL
#endif
#endif
      case _skey=24 .and. select()<>MAIN->(select())
        select (select('MAIN'+str(val(subs(alias(),5))+1,4)))
        if !used()
          select MAIN
        endif
        go _srec[_sm]
        _sef:=.f.
        return .t.

   case _skey=5

     if dg>DatY->d_z_rok
        dg:=if(dg>DatY->d_z_mies2,DatY->d_z_mies2,DatY->d_z_rok)
        _spocz:=STANY->nr_mag+STANY->index+DTOS(dg)
        _sbf:=.f.

#ifdef A_WA
    do case
      case dg=DatY->d_z_rok
        sb:=zamkn_roku
        wb:=wart_roku
      case dg<=DatY->d_z_mies2
        sb:=zamk_mies2
        wb:=wart_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
        wb:=wart_mies1
    endcase
#else
    do case
      case dg=DatY->d_z_rok
        sb:=zamkn_roku
      case dg<=DatY->d_z_mies2
        sb:=zamk_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
    endcase
    wb:=sb*(i_lam(dg))->cenA
#endif
    _snagl:="Data¬ƒƒDokument¬Koszty¬ƒPrzych¢d¬ƒRozch¢dƒ¬ƒƒƒ"+WANAZ+"¬"+str(sb,10,3)+"¬"+str(wb,10,CEOKR)
        Return .t.
     else
*
        y:=str(year(data)-1,4)
        x:=select('MAIN'+y)
        if x=0
           u:=ordfor()
           v:=ordkey()
           x:=select()
           begin sequence
             if !file(defa+y+HB_ps()+'main.dbf')
                break
             endif

             nuse (defa+y+HB_ps()+'main') new alias ('MAIN'+y)
#ifdef A_CDX
             set order to tag main_ind
             if empty(indexord())
                w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                if empty(u)
                   index on &v tag main_ind eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                else
                   index on &v tag main_ind for &u eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                endif
                //break
             endif
#else
             if !file(defa+y+HB_ps()+'main_ind'+ordbagext())
                index on &v to (defa+y+HB_ps()+'main_ind')
                w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                if empty(u)
                   index on &v to (defa+y+HB_ps()+'main_ind') eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                else
                   index on &v to (defa+y+HB_ps()+'main_ind') for &u eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                endif
                //break
             else
                set index to (defa+y+HB_ps()+'main_ind')
             endif
#endif
             x:=nil
           recover
             return .f.
           end sequence
           message(w)
        else
           select (x)
           x:=NIL
        endif
        if x=NIL
           _spocz:=STANY->nr_mag+STANY->index
           set order to tag main_ind
           seek _spocz+"@"
           _sbf:=.f.
           //dg:=stod(left(dtos(data),4)+'0101')-1
           return .t.
        endif
     endif
   case select()<>MAIN->(select())

      if _skey=27 .or. _skey=4 .or. _skey=19
        return .t.
      endif
*
   CASE _SKEY=9 .and. il=NIL // TAB
      private changed:=.f.
      select indx_mat
      set order to TAG INDX_NUM
      select dm
      set order to 1
      SET RELATION TO
      set filter to
      SEEK main->nr_mag+main->smb_dow+main->nr_dowodu
      wydruk_dok()
      SELECT MAIN
      SET ORDER TO TAG MAIN_IND
      SET RELATION TO
      indx_mat->(dbgoto(irec))
      
      CASE _SKEY=22 .or. _skey=13
         PRIVATE changed:=.f.
         prptxt:=getscrtxt(_sprpt)
         D_JMOFIX(s,prptxt)
         s-=ilosc
#ifdef A_WA
         w:=VAL(RIGHT(prptxt,10))-wartosc
#else
         w:=s*(i_lam(data))->cenA
#endif
#ifdef A_MM
         DOK_IN(nr_mag,smb_dow,nr_dowodu+pozycja,il#NIL)
#else
         DOK_IN(nr_mag,smb_dow,nr_dowodu+pozycja,il#NIL.OR.NR_MAG#MAG_BIEZ)
#endif
         SELECT MAIN
         SET ORDER TO TAG MAIN_IND
         set relation to
         indx_mat->(dbgoto(irec))
         IF CHANGED
            chg:=.t.
#ifndef STANY
            STANY->(dbgoto(srec))
      if STANY->(deleted())
              stany->(ordsetfocus("STAN_MAG"))
              stany->(dbseek(nr_mag+index,.f.))
              srec:=stany->(recno())
      endif
#endif
            se:=STANY->stan
#ifdef A_WA
            we:=STANY->wartosc
#else
            we:=se*(i_lam(STANY->data_zmian))->cenA
#endif
            go _srec[_sm]
            refresh(,_s)
            if _si=0 .or. _sef .and.( ROUND(Se-S2,3)#0 .or. ROUND(we-w2,A_ZAOKR)#0)
               _sef:=.f.
               s2:=se
               w2:=we
               kibord(chr(30))
            endif
         ENDIF

   case _skey=-6
         prptxt:=getscrtxt(_sprpt)
         D_JMOFIX(s,prptxt)
         s-=2*ilosc
#ifdef A_WA
         w:=VAL(RIGHT(prptxt,10))-2*wartosc
#else
         w:=s*(i_lam(data))->cenA
#endif
         indx_mat->(dbgoto(irec))
#ifndef STANY
         STANY->(dbgoto(srec))
#endif
         select FIRMY
         set order to TAG FIRM_NUM
         select dm
#ifdef A_KHSEP
#define D_KH kontrahent
#else
#define D_KH left(dost_odb,A_NRLTH)
#endif
         SET RELATION TO D_KH INTO FIRMY
         set order to 1
         select main
         set relation to nr_mag+smb_dow+nr_dowodu into dm
         go recno()
         _slist(".\K*.FRM",_s)
         if _sm#_si .or. !_sef
               _sef:=.f.
               s2:=se
               w2:=we
               kibord(chr(30))
         endif
         set relation to

   case _skey=-9 
         prptxt:=getscrtxt(_sprpt)
         D_JMOFIX(s,prptxt)
         s-=2*ilosc
#ifdef A_WA
         w:=VAL(RIGHT(prptxt,10))-2*wartosc
#else
         w:=s*(i_lam(data))->cenA
#endif
         indx_mat->(dbgoto(irec))
#ifndef STANY
         STANY->(dbgoto(srec))
#endif
         select FIRMY
         set order to TAG FIRM_NUM
         select dm
         SET RELATION TO D_KH INTO FIRMY
         set order to 1
         select main
         set relation to nr_mag+smb_dow+nr_dowodu into dm
         go recno()
         _slist(,_s)
         if _sm#_si .or. !_sef
               _sef:=.f.
               s2:=se
               w2:=we
               kibord(chr(30))
         endif
         set relation to

  CASE _SKEY=1 .OR. _SKEY=29

#ifdef A_ZERUJSTAN
  CASE (_sef .or. _sbf).and. dg>DatY->d_z_rok
#else
  CASE _sef .or. _sbf
#endif
#ifdef A_LAN
#ifndef STANY
       SELECT STANY
       goto srec
       if deleted()
          set order to tag STAN_MAG
          dbseek(nr_mag+index,.f.)
          srec:=recno()
       endif
#ifdef A_WA
    do case
      case dg=DatY->d_z_rok
        sb:=zamkn_roku
        wb:=wart_roku
      case dg<=DatY->d_z_mies2
        sb:=zamk_mies2
        wb:=wart_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
        wb:=wart_mies1
    endcase

    se := stan
    we := wartOSC
#else
    do case
      case dg=DatY->d_z_rok
        sb:=zamkn_roku
      case dg<=DatY->d_z_mies2
        sb:=zamk_mies2
      otherwise //case DatY->data_gran=DatY->d_z_mies1
        sb:=zamk_mies1
    endcase
    wb:=sb*(i_lam(dg))->cenA
    se := stan
    we := stan*(i_lam(STANY->data_zmian))->cenA
#endif
    SELECT MAIN
#endif
#endif
#ifdef A_WA
       if _sef .and. ( ROUND(Se-S2,3)#0 .or. ROUND(we-w2,A_ZAOKR)#0) .or. _sbf .and. select()=MAIN->(select())  .and. ( ROUND(Sb-S1,3)#0 .or. ROUND(wb-w1,A_ZAOKR)#0)
           alarm("NIE ZGADZA SI® STAN LUB WARTOóè;PROSZ® SPRAWDZIè !",,,3)
#else
       if _sef .and. ROUND(Se-S2,3)#0 .or. _sbf .and. select()=MAIN->(select())  .and. ROUND(Sb-S1,3)#0
           alarm("NIE ZGADZA SI® STAN;PROSZ® SPRAWDZIè !",,,3)
#endif
           return LASTKEY()=27
       elseif _skey=27 .or. _skey=4 .or. _skey=19
           return .t.
       endif

  case _skey=27 .or. _skey=4 .or. _skey=19 
         return .t.

   ENDCASE

RETURN .F.   
******
