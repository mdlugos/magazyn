#include "hbgtinfo.ch"
#include "ar.ch"
#ifndef A_LAN
#define DatY MEMVAR
#endif

field konto,nazwa,sald_kon_w,sald_kon_m,sald_rok_w,sald_rok_m,sald_poc_w,sald_poc_m,sald_bie_w,sald_bie_m,czyma,kwota,data,syntet
memvar stary_rok,defa,_sel,_snorm,is_spec,oprn
proc dodatki
local m,a,b,c,d,e,f,g,h,i
memvar it_menudod
      m:=it_menudod
      SETKEY(4,{||KIBORD(CHR(27)+CHR(24)+CHR(13))})
      SETKEY(19,{||KIBORD(CHR(27)+CHR(5)+CHR(13))})
      aczojs({;
      "Pokazuj obroty za:",;
      hb_UTF8ToStr("Zamknięcie miesiąca."),;
      hb_UTF8ToStr("Otwarcie miesiąca."),;
      "Symbole i kody.",;
      hb_UTF8ToStr("Ilość linii."),;
      "Zmiana roku.",;
      "Ratuj.";
      },"",@m)
      SET KEY 4 TO
      SET KEY 19 TO

   if m=0
      return
   ENDIF
   it_menudod:=m
   do case
      case m = 1
        a:={}
        If DatY->d_z_mies1>DatY->d_z_mies2
           aadd(a,cmonth(DatY->d_z_mies1+1))
        elseIf DatY->d_z_mies2>DatY->d_z_rok
           aadd(a,cmonth(DatY->d_z_mies2+1))
        endif
        aadd(a,cmonth(DatY->d_z_rok+1))
        m:=ascan(a,cmonth(DatY->d_z_gran+1))
        aczojs(a,"",@m,,"Obroty za:")
        lock in daty
        DO CASE
           CASE m=len(a)
                DatY->d_z_gran:=DatY->d_z_rok
           CASE m=1
                DatY->d_z_gran:=DatY->d_z_mies1
           otherwise
                DatY->d_z_gran:=DatY->d_z_mies2
        ENDCASE
#ifdef A_LAN
      unlock in daty
#else
      inisave(set(_SET_DEFAULT)+"daty.ini")
#endif
      case m = 2; ratuj(.f.);  zamkniecie()
      case m = 3 .and. iS_spec

      if stary_rok#NIL
         m:=defa+str(year(stary_rok+1),4)+HB_ps()
         if ! file(m+'daty.*')
           m:=defa+'roboczy'+HB_ps()
         endif
#ifdef A_LAN
         nuse (m+"daty") NEW alias nowy
         #define D_Z1 nowy->d_z_mies1
         #define D_Z3 nowy->d_z_rok
#else
         a:=memoread(m+"daty.ini")
         if a=hb_utf8Chr(0xFEFF)
            a:=hb_bsubstr(a,4)
         endif
         a:=hb_ATokens(a,.t.)
         aeval(a,{|x,i|x:=substr(x,at(':=',x)+2),a[i]:=&x},1,3)
         #define D_Z1 a[1]
         #define D_Z3 a[3]
#endif
         if D_Z1#D_Z3
            alarm(hb_UTF8ToStr("Otwarcie niemożliwe;Nowy rok jest zamknięty do ")+dtoc(D_Z1)+" .")
#ifdef A_LAN
            use
#endif
            return
         endif
#ifdef A_LAN
         use
#endif
#undef D_Z1
#undef D_Z3
       endif
       lock in daty
       if DatY->d_z_mies1=DatY->d_z_mies2
          DatY->d_z_mies2:=DatY->d_z_rok
       endif
       hAslo_spec(10)
       DatY->d_z_gran:=DatY->d_z_mies1:=DatY->d_z_mies2
       //sel("konta")
       //exec {||DatY->d_z_rok=DatY->d_z_mies2 .and.(sald_poc_w:=sald_rok_w,sald_poc_m:=sald_rok_m,.t.),DatY->d_z_mies2=DatY->d_z_mies1 .and. (sald_kon_w:=sald_poc_w,sald_kon_m:=sald_poc_m,.t.) } ALL
       ? hb_UTF8ToStr("Zamknięte do: "),cmonth(DatY->d_z_mies1),hb_UTF8ToStr(". Naciśnij coś...")
#ifdef A_LAN
       unlock in DatY
#else
       inisave(set(_SET_DEFAULT)+"daty.ini")
#endif
       inkey(0)

      case m = 4 .and. iS_spec
          hAslo_spec(10)
          browse()
      case m = 5
         if hb_gtInfo( HB_GTI_ISGRAPHIC )
            a:={"98x32","80x25 (Norm)",hb_UTF8ToStr("Pełny ekran (Alt+Enter)"),hb_UTF8ToStr("Zmieniaj wielkość czcionki")}
            txt:=hb_gtInfo( HB_GTI_FONTSEL )
            if empty(txt)  //hb_gtInfo( HB_GTI_VERSION )<>'XWC'
               aadd(a,hb_UTF8ToStr("Zmieniaj ilość linii"))
            endif
            aczojs(a,"",@m)
         do case
         case m=0
         case m=1
         SetMode(32,98)
         case m=2
         SetMode(25,80)
         case m=3
         hb_gtInfo( HB_GTI_ISFULLSCREEN, ! hb_gtInfo( HB_GTI_ISFULLSCREEN ) )
         otherwise
            if empty(txt)  //hb_gtInfo( HB_GTI_VERSION )='XWC'
               hb_gtInfo( HB_GTI_RESIZEMODE, if(m=4, HB_GTI_RESIZEMODE_FONT, HB_GTI_RESIZEMODE_ROWS))
            else
               a:={'29','27','21','20','18','15','14','13','12','11','10','9','8','7','6'}
               // txt:=hb_gtInfo( HB_GTI_FONTSEL ) '-*-fixed-medium-r-*-*-18-*-*-*-*-*-iso10646-1'
               b:=ascan(a,{|x|'*-'+x+'-*'$txt})
               m:=b
               if aczojs(a,"",@m)
                  txt:=strtran(txt,'*-'+a[b]+'-*','*-'+a[m]+'-*')
                  hb_gtInfo( HB_GTI_FONTSEL, txt ) 
               endif
            endif
            endcase
         else
           m:=2
           aczojs({"80x25 (Norm)","80x30","80x43","80x50","80x60","132x25","132x30","132x43","132x50","132x60"},"",@m)
           if m=1
           setmode(25,80)
           elseif m=2
           setmode(30,80)
           elseif m=3
           setmode(43,80)
           elseif m=4
           setmode(50,80)
           elseif m=5
           setmode(60,80)
           elseif m=6
           setmode(25,132)
           elseif m=7
           setmode(30,132)
           elseif m=8
           setmode(43,132)
           elseif m=9
           setmode(50,132)
           elseif m=10
           setmode(60,132)
           endif
         endif
         //setmode(,)

      case m = 6
   if stary_rok#NIL
      a:={hb_UTF8ToStr("POWRÓT")} //nie wiem, jak gęboko zakopany
   else
      a:={}
   endif
   m:=year(if(stary_rok=NIL,DatY->d_z_rok,DatE()))
   do while .t.
      if !file(defa+str(m,4)+HB_ps()+"daty.*")
         if stary_rok=NIL .or. m<year(DatY->d_z_rok+1)
            exit
         endif
         --m
         loop
      elseif stary_rok#NIL .and. year(stary_rok)=m
         --m
         loop
      endif
      aadd(a,str(m,4))
      --m
   enddo
   m:=1
   aczojs(a,"",@m)
   if stary_rok#NIL .and. m=1
      stary_rok:=NIL
   elseif m#0
      m:=a[m]
      STARY_ROK:=stod(m+"1231")
   endif
   reuse()
   readinit()
   case m=7
     ratuj(.t.)
   endcase

return
**********
stat proc zamkniecie
memvar rejestry
local dz1,dz2,dz3,w,m,ar,ar1,key,canfix
field baza,rejestr,lp,status,data,khflag
begin sequence
close databases

  #ifdef A_LAN
   nuse daty
   lock
  #endif

dz1:=DatE()+3
dz1-=day(dz1)

IF dz1>DatY->d_z_mies1
  if stary_rok#NIL
     dz1:=stary_rok
  else
     do while YEAR(dz1)>YEAR(DatY->D_Z_rok+1)
        dz1-=day(dz1)
     enddo
  endif
  if stary_rok=NIL .and. DatY->d_z_mies1=DatY->d_z_rok .and. file(defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty.*")
#ifdef A_LAN
     nuse (defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty") new alias stary
     #define D_Z1 stary->d_z_mies1
#else
      m:=memoread(defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty.ini"))
      if m=hb_utf8Chr(0xFEFF)
         m:=hb_bsubstr(m,4)
      endif
      m:=hb_ATokens(m,.t.)[1]
      m:=SubStr(m,at(':=',m)+2)
      m:=&m
     #define D_Z1 m
#endif
     if D_Z1<DatY->d_z_rok
        alarm(hb_UTF8ToStr("Poprzedni rok nie zamknięty !"))
        dz1:=DatY->d_z_mies1
     endif
#ifdef A_LAN
     use
#endif
#undef D_Z1
  endif
else
    dz1:=DatY->d_z_mies1
endif

  ar:={}
  ar1:={}

  do while dz1>DatY->D_Z_MIES1
      aadd(ar,DTOV(dz1))
      aadd(ar1,dz1)
      dz1-=day(dz1)
  enddo

  aadd(ar,"Kontrola Sald")
  aadd(ar1,DatY->d_z_mies1)

  if 0=(key:=alarm(hb_UTF8ToStr("Zamknięcie do:;( Esc - Rezygnacja )"),ar))
     break
  endif
  dz1:=ar1[key]

  sel("main","main_lp")
  LOCK ALL
  go top

  ? hb_UTF8ToStr("PRZYGOTUJ DRUKARKĘ (NA WSZELKI WYPADEK).")
  ?
#ifdef A_WIN_PRN
  oprn:=A_WIN_PRN
  if valtype(oprn)='O' .or. .t.=oprn
    oprn:=.f.
    delete file (findfile('zamkn.prn'))
    set printer to ('.'+HB_ps()+'zamkn.prn')
  endif
#endif
  m:=.f.
  do while !eof()
     key:=rejestr+lp
     w:=ascan(rejestry,{|x|x[AR_SMB]=rejestr})
     if w=0
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? " USZKODZONE DANE W BAZIE MAIN:",key,kwota,konto
      ?
      set print off
      m:=.t.
#ifndef iS_spec
     else
       sel(rejestry[w,AR_DBF],rejestry[w,AR_DBF]+'_LP')
       if !dbseek(main->lp)
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? " BRAK POZYCJI W REJESTRZE:",key
      ?
      set print off
      m:=.t.
      elseif status<STAT_ZATW .and. data<=dz1
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? " POZYCJA NIE ZATWIERDZONA:",key
      ?
      set print off
      m:=.t.
      endif
#endif
     endif
     select main
     w:=0
     @ row(),0 say key
     exec w+=if(czyma,-kwota,kwota) while rejestr+lp=key
     if round(w,A_ZAOKR)#0
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? " DOKUMENT",key,hb_UTF8ToStr("SIĘ NIE BILANSUJE:"),w
      ?
      set print off
      m:=.t.
     endif
  enddo
  if m
     ? hb_UTF8ToStr('NIE PRZEPROWADZONO ZAMKNIĘCIA Z POWODU BŁĘDNYCH DANYCH !')
     break
  endif

  if dz1>DatY->d_z_mies1
    dz2:=DatY->d_z_mies1
    dz3:=DatY->d_z_mies2
    IF month(dz1) = 12 .and. dz1>DatY->d_z_rok
      ? hb_UTF8ToStr("Kontrola sald i zamknięcie roku !")
    else
      ? hb_UTF8ToStr("Kontrola sald i zamknięcie do dnia"),dz1
    endif
    DatY->d_z_mies1:=DatY->d_z_mies2:=DatY->d_z_gran:=DatY->d_z_rok
  #ifndef A_LAN
    inisave(set(_SET_DEFAULT)+"daty.ini")
  #endif
  else
     ? "Kontrola sald."
     dz2:=DatY->d_z_mies2
     dz3:=dz2
  endif
  sel("firmy","firm_num")
  sel("konta","kont_num")
  //lock all
  /*
  set filter to !syntet
  go top
  */
  locate for !syntet
  select main
  set order to tag main_kt
  go top

DO WHILE !(EOF() .and. konta->(eof()))

   SETPOS(row(),0)

   IF !eof() .and. (konta->konto>konto .OR. konta->(eof()))
   *
    locate rest for kwota#0 while konta->konto>konto .OR. konta->(eof())
    if !found()
       loop
    endif
   *
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? "W BAZIE KONTA BRAKUJE:",konto
      ? hb_UTF8ToStr("ZAKŁADAM, ŻE SALDO ZEROWE NA POCZĄTKU ROKU !!!!")
      set print off
      ?
      konta->(dbappend())
      konta->syntet:=.f.
      konta->konto:=konto
      konta->nazwa:=" !!! "
      if dz3>DatY->d_z_rok
         m:=w:=0
         execute {||if(czyma,m+=kwota,w+=kwota)} rest while konto+dtos(DATA)<=konta->konto+dtos(dz3)
         konta->sald_poc_m:=m
         konta->sald_poc_w:=w
      endif
   ELSE
      LOCK IN KONTA
   ENDIF

   SEEK konta->konto+SubStr(dtos(dz3+1),5)


   if dz3=DatY->d_z_rok
     w:=konta->sald_rok_w
     m:=konta->sald_rok_m
   else
     w:=konta->sald_poc_w
     m:=konta->sald_poc_m
   endif
   EXECUTE {||if(czyma,m+=kwota,w+=kwota)} rest while konto+dtos(DATA)<=konta->konto+dtos(dz2)
   konta->sald_poc_w:=w
   konta->sald_poc_m:=m

   EXECUTE {||if(czyma,m+=kwota,w+=kwota)} rest while konto+dtos(DATA)<=konta->konto+dtos(dz1)
   konta->sald_kon_w:=w
   konta->sald_kon_m:=m

   EXECUTE {||if(czyma,m+=kwota,w+=kwota)} rest while konto=konta->konto

   SELECT konta

   IF round(sald_bie_w-w,2)#0 .or. round(sald_bie_m-m,2)#0
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? konto,nazwa,sald_bie_w,sald_bie_m
      sald_bie_w:=w
      sald_bie_m:=m
      ? hb_UTF8ToStr("RÓŻNICA W SALDZIE KOŃCOWYM, MA BYĆ"),sald_bie_w,sald_bie_m
      ?
      ?
      set print off
  else
  setpos(row(),0)
      ?? konto,nazwa,sald_bie_w,sald_bie_m
  ENDIF

   w:=recno()
   m:=SubStr(konto,A_KTL-A_NRLTH+1)
   if dbseek(left(konto,A_KTL-A_NRLTH)) .and. syntet .and. khflag .and. firmy->(dbseek(m))
      m:=trim(firmy->nazwA)+" - "+trim(nazwa)
      goto w
      if m<>trim(nazwa)
         LOCK
         ? nazwa:=m
         ?
      endif
   else
      goto w
   endif

  // SKIP
   continue
   SELECT MAIN

ENDDO

SELECT KONTA
//SET FILTER TO

EXECUTE mksynt(0,0,0,0,0,0,0,0) ALL FOR syntet

DatY->d_z_mies2:=dz2
DatY->d_z_gran:=dz2
DatY->d_z_mies1:=dz1

#ifndef A_LAN
   inisave(set(_SET_DEFAULT)+"daty.ini")
#endif

unlock all


if stary_rok=DatY->d_z_mies1
#ifdef A_BODOK
   set color to (_sel)
   ? hb_UTF8ToStr("PROSZĘ SPRAWDZIĆ ZGODNOŚĆ BILANSU ZAMKNIĘCIA I OTWARCIA NOWEGO ROKU."),chr(7)
   ?
   set color to (_snorm)
#else
     w:=defa+str(year(stary_rok+1),4)+HB_ps()
     if ! file(w+'daty.*')
        w:=defa+'roboczy'+HB_ps()
     endif
     select main
#ifdef A_LAN
     nuse (w+"daty") alias ndat
     lock
     #define D_Z1 ndat->d_z_mies1
#else
     use
     m:=memoread(w+"daty.ini")
     if m=hb_utf8Chr(0xFEFF)
        m:=hb_bsubstr(m,4)
     endif
     m:=hb_ATokens(m,.t.)[1]
     m:=SubStr(m,at(':=',m)+2)
     m:=&m
     #define D_Z1 m
endif
   set color to (_sel)
   ? hb_UTF8ToStr("SPRAWDZENIE ZGODNOŚCI BILANSU ZAMKNIĘCIA STAREGO I OTWARCIA NOWEGO ROKU."),chr(7)
   ?
   set color to (_snorm)
   canfix:=D_Z1=DatY->d_z_mies1 .and. 1=alarm(hb_UTF8ToStr("Czy przenieść BILANS ZAMKNIĘCIA;na BILANS OTWARCIA NOWEGO ROKU"),{"TAK","NIE"})
#undef D_Z1
   *********************
   w:=set(_SET_DEFAULT,w)
   select konta
   use
   sel("konta","kont_num")
   if canfix
      set filter to !syntet
   endif
   go top
   set default to (w)
#ifdef A_CDX
   nuse konta alias stary new
   set order to tag kont_num in konta
#else
   nuse konta alias stary new
   set index to kont_num
#endif
   if canfix
      set filter to !syntet
   endif
   go top
   do while !eof() .or. !konta->(eof())

    @ row(),0 say konto

    IF !konta->(EOF()) .and. konto>konta->konto .OR. EOF()
       w:=konta->sald_rok_w
       m:=konta->sald_rok_m
      IF round(w-m,A_ZAOKR)#0
        SET PRINT ON
        PRINT(1)
        ? "KONTO:",konta->konto
        ? "NAZWA BO:",konta->NAZWA
        ? "BZ:  BRAK !"
        ? "BO:",w,m
        if canfix
           select konta
           lock
           sald_rok_w:=0
           sald_poc_w-=w
           sald_kon_w-=w
           sald_bie_w-=w
           sald_rok_m:=0
           sald_poc_m-=m
           sald_kon_m-=m
           sald_bie_m-=m
           ? "DOKONANO KOREKTY BO !"
           select stary
        endif
        ?
      ENDIF
      konta->(DBSKIP(1))
    ELSEIF konto<konta->konto .OR. konta->(EOF())
      IF round(sald_kon_w-sald_kon_m,A_ZAOKR)#0
        if !canfix
           SET PRINT ON
           PRINT(1)
        endif
        ? "KONTO:",konto
        ? "NAZWA BZ:",NAZWA
        ? "BZ:",sald_kon_w,sald_kon_m
        ? "BO:  BRAK !"
        if canfix
           konta->(dbappend())
           ? "DOKONANO KOREKTY BO !"
           FOR KEY=1 TO FCOUNT()
               konta->(FIELDPUT(KEY,STARY->(FIELDGET(KEY))))
           NEXT
           w:=round(sald_kon_w-sald_kon_m,A_ZAOKR)
           m:=-w
           if w>0
              m:=0
           else
              w:=0
           endif
           konta->(sald_rok_w:=sald_poc_w:=sald_kon_w:=sald_bie_w:=w)
           konta->(sald_rok_m:=sald_poc_m:=sald_kon_m:=sald_bie_m:=m)
           konta->(DBSKIP(1))
         endif
         ?
      ENDIF
      SKIP
    ELSE
        w:=round(sald_kon_w-sald_kon_m,A_ZAOKR)
        m:=-w
        if w>0
           m:=0
        else
           w:=0
        endif

      IF round(konta->sald_rok_w-w,A_ZAOKR)#0 .or. round(konta->sald_rok_m-m,A_ZAOKR)#0
        if !canfix
           SET PRINT ON
           PRINT(1)
        endif
        ? "KONTO:",konto
        ? "NAZWA BZ:",NAZWA
        ? "NAZWA BO:",konta->NAZWA
        ? "BZ:",sald_kon_w,sald_kon_m
        ? "BO:",konta->sald_rok_w,konta->sald_rok_m
      if canfix

           select konta
           lock
           w-=sald_rok_w
           m-=sald_rok_m
           sald_rok_w+=w //sald_rok:= sald_rok + stary->sald_rok - sald_rok
           sald_poc_w+=w
           sald_kon_w+=w
           sald_bie_w+=w
           sald_rok_m+=m
           sald_poc_m+=m
           sald_kon_m+=m
           sald_bie_m+=m
        ? "DOKONANO KOREKTY BO !"
         select stary
      endif
        ?
      ELSEIF nazwa#konta->nazwa //.or. syntet#konta->syntet
        if !canfix
           SET PRINT ON
           PRINT(1)
        endif
        ? "KONTO:",konto
        ? "NAZWA BZ:",NAZWA
        ? "NAZWA BO:",konta->NAZWA
//        ? "SYNTET BZ:",syntet
//        ? "SYNTET BO:",konta->syntet
      if canfix
        lock in konta
//           konta->syntet:=syntet
           konta->nazwa:=nazwa
        ? "DOKONANO KOREKTY BO !"
      endif
        ?
      ENDIF
      SKIP
      KONTA->(DBSKIP(1))
    ENDIF
  ENDDO
  if canfix
     SELECT KONTA
     SET FILTER TO

     EXECUTE mksynt(0,0,0,0,0,0,0,0) ALL FOR syntet
  endif
  set print off
  ?? chr(7)+chr(7)
   *********************
#endif
elseIF year(DatY->d_z_mies1+1) > year(DatY->d_z_rok+1)
   set color to (_sel)
   ? hb_UTF8ToStr("ZAMKNIĘCIE ROKU, TROCHĘ POTRWA")
   set color to (_snorm)
   ?
  if File(defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty.*")
     if !file(defa+"fiks.*") .and. !file(defa+"archiwum"+HB_ps()+"fiks"+right(str(year(DatY->d_z_mies1)),2)+".*")
        alarm(hb_UTF8ToStr("Brak kopii bezpieczeństwa danych.;Nie będę zamykać roku!"),0,3)
        break
     endif
     mkdir(defa+"archiwum")
     ?
     ? "Kopiowanie starych danych do katalogu ARCHIWUM:"
     AEVAL(DIRECTORY(defa+"fiks.*"),{|X|if(file(defa+"archiwum"+HB_ps()+"fiks"+right(str(year(DatY->d_z_mies1)),2)+SubStr(x[1],at(".",x[1]))),,(QOUT(X[1]),frename(defa+X[1],defa+"archiwum"+HB_ps()+"fiks"+right(str(year(DatY->d_z_mies1)),2)+SubStr(x[1],at(".",x[1])))))})
  else
     mkdir(defa+str(year(DatY->d_z_rok+1),4))
  endif

   close databases

#ifdef A_LAN
   nuse daty READONLY EXCLUSIVE alias "XXXX" //żeby nikt się nie pchał do "roboczy"
   if !used()
      break
   endif
#endif

   mkdir(defa+"tmp")
   SET DEFAULT TO (defa+"tmp"+HB_ps())              // roboczy katalog

  ?
  ? "Kasowanie kartoteki TMP."

   AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.*"),{|X|QOUT(X[1]),FERASE(defa+"tmp"+HB_ps()+X[1])})
  ?
  ? hb_UTF8ToStr("Tworzenie nowych zbiorów tymczasowo w kartotece TMP:")
  ?
  ? "Tworzenie zbioru DATY"
 

#ifdef A_LAN
   copy to daty
   nuse daty exclusive new
   for w:=1 to fcount()
       if valtype(fieldget(w))='N'
          fieldput(w,0)
       endif
   next w
#endif

   DatY->d_z_rok=DatY->d_z_mies1
   DatY->d_z_mies2=DatY->d_z_rok
   DatY->d_z_gran=DatY->d_z_rok

#ifndef A_LAN
    copy file (defa+"roboczy"+HB_ps()+"daty.ini") to daty.ini
    inisave(set(_SET_DEFAULT)+"daty.ini")
#endif

   nuse indeks new
   key:=''
   do while !eof()
      if .not. key==(key:=lower(trim(_FIELD->baza))) .and. file(defa+"roboczy"+HB_ps()+key+".dbf")
         ?
         nUSE (defa+"roboczy"+HB_ps()+key) READONLY NEW ALIAS ROB
         IF type("_FIELD->LP")#"U"
           ? "Tworzenie bazy",key
           COPY structure TO (key)
         else
           ? "Kopiowanie bazy",key
           if key == "konta"
            SORT TO (key) on konto
            nUSE (key) EXCLUSIVE
#ifdef A_BODOK
            exec {||w:=sald_kon_w-sald_kon_m,m:=-w,if(w>0,m:=0,w:=0),sald_poc_w:=w,sald_poc_m:=m,sald_kon_w:=sald_bie_w:=sald_rok_w:=sald_rok_m:=sald_kon_m:=sald_bie_m:=0}
#else
            exec {||w:=sald_kon_w-sald_kon_m,m:=-w,if(w>0,m:=0,w:=0),sald_rok_w:=sald_poc_w:=sald_kon_w:=sald_bie_w:=w,sald_rok_m:=sald_poc_m:=sald_kon_m:=sald_bie_m:=m}
#endif
           else
            COPY TO (key)
           endif
         endif
         USE
         select indeks
      endif
      skip
   enddo

   w:=str(year(DatY->d_z_rok),4)

   close databases

  ?
  ? "Przenoszenie danych z kartoteki ROBOCZY do "+w+"."

   mkdir(defa+w)

   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.ini"),{|X|QOUT(X[1]),frename(defa+"roboczy"+HB_ps()+x[1],defa+w+HB_ps()+X[1])})
   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.??x"),{|X|QOUT(X[1]),frename(defa+"roboczy"+HB_ps()+x[1],defa+w+HB_ps()+X[1])})
   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.db?"),{|X|QOUT(X[1]),frename(defa+"roboczy"+HB_ps()+x[1],defa+w+HB_ps()+X[1])})
#ifdef A_CDX
   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.fpt"),{|X|QOUT(X[1]),frename(defa+"roboczy"+HB_ps()+x[1],defa+w+HB_ps()+X[1])})
#endif

  ?
  ? hb_UTF8ToStr("Kasowanie pozostałości kartoteki ROBOCZY.")


   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.*"),{|X|QOUT(X[1]),FERASE(defa+"roboczy"+HB_ps()+X[1])})

  ?
  ? "Przenoszenie danych z kartoteki TMP do ROBOCZY."


   AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.*"),{|X|QOUT(X[1]),frename(defa+"tmp"+HB_ps()+X[1],defa+"roboczy"+HB_ps()+x[1])})

  endif
  end sequence

  set print off
#ifdef A_WIN_PRN
  oprn:=A_WIN_PRN
  if !empty(oprn)
     w:=memoread(set(_SET_PRINTFILE,''))
     if w=hb_utf8Chr(0xFEFF)
      w:=hb_bsubstr(w,4)
     endif
     w:=getlines(w,.t.)
     if !empty(w)
       set printer to
       Print(1)
       aeval(w,{|y|wqq(y),wq()})
       if valtype(oprn)='O'
         oprn:destroy()
         oprn:=NIL
       endif
     endif
  endif
#endif
#ifdef A_PRINT
     w:=set(_SET_PRINTFILE,'')
     if ! w==set(_SET_PRINTFILE) .and. File(w)
        A_PRINT(w)
     endif
#endif
  reuse()
? hb_UTF8ToStr("Zamknięte do: "),cmonth(DatY->d_z_mies1),hb_UTF8ToStr(". Naciśnij coś...")
  inkey(0)

return
*********
stat proc mksynt(x1,x2,x3,x4,y1,y2,y3,y4)
local r1,r2,key,w1,w2,w3,w4,m1,m2,m3,m4
   r2:=r1:=recno()
   key:=trim(konto)
   skip
   w1:=w2:=w3:=w4:=m1:=m2:=m3:=m4:=0
   EXECUTE {||w1+=sald_rok_w,w2+=sald_poc_w,w3+=sald_kon_w,w4+=sald_bie_w,m1+=sald_rok_m,m2+=sald_poc_m,m3+=sald_kon_m,m4+=sald_bie_m,r2:=recno()} ;
       WHILE konto=key ;
       FOR !syntet .or. (mksynt(@w1,@w2,@w3,@w4,@m1,@m2,@m3,@m4),r2:=recno(),.f.)
   goto r1
   LOCK
   x1+=sald_rok_w:=w1
   y1+=sald_rok_m:=m1
   x2+=sald_poc_w:=w2
   y2+=sald_poc_m:=m2
   x3+=sald_kon_w:=w3
   y3+=sald_kon_m:=m3
   IF round(sald_bie_w-w4,2)#0 .or. round(sald_bie_m-m4,2)#0
  begin sequence
      SET PRINT ON
      PRINT(1)
  end
      ?? konto,nazwa,sald_bie_w,sald_bie_m
      sald_bie_w:=w4
      sald_bie_m:=m4
      ? hb_UTF8ToStr("RÓŻNICA W SALDZIE KOŃCOWYM, MA BYĆ"),sald_bie_w,sald_bie_m
      ?
      ?
      set print off
  else
      setpos(row(),0)
      ?? konto,nazwa,sald_bie_w,sald_bie_m
  ENDIF
   x4+=w4
   y4+=m4
   goto r2

return
**********
#ifndef hAslo_spec
proc haslo_spec(p)
memvar operator
field haslo_spec,nazwisko
    local m,txt,c
      if p<>NIL
        @ p,0
      endif
      c:=col()
      devout(hb_UTF8ToStr("Podaj hasło:"))
       txt:=""
    set cursor on
  do while 0<(m:=inkey(0)) .and. m#13
    if m=28
      help("haslo_spec")
      loop
    elseif m=8
      txt:=left(txt,len(txt)-1)
      @ row(),col()-1 say " "
      setpos(row(),col()-1)
      loop
    endif
    txt+=hb_keyChar(m)
    ?? hb_UTF8ToStr("░")
  enddo
    set cursor off

   nUSE obsluga READONLY  NEW
#ifdef A_SX
       LOCATE FOR operator==nazwisko .and. txt==trim(haslo_spec)
#else
       LOCATE FOR operator==nazwisko .and. padr(lower(txt),4)==l2bin(haslo_spec)
#endif
       IF EOF()
         use
         @ row(),c SAY hb_UTF8ToStr("złe hasło ......")
         inkey(2)
         break
       endif
   use
return
#endif
