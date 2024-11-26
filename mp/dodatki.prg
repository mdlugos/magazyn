#include   'hbgtinfo.ch'

#ifdef A_OLDA
#define D_OLZA 32
#define D_OLDA +31
#else
#define D_OLZA 1
#define D_OLDA
#endif

#ifdef A_LPNUM
   #ifdef A_UNICODE
      #define D_LPVAL(x) iif(A_LPNUM=1,HB_BCODE(binfieldget([POZYCJA],x))-48,val(x))
      #define D_LPVALFIELD(x) iif(A_LPNUM=1,HB_BCODE(binfieldget([x]))-48,val(x))
   #else
      #define D_LPVAL(x) iif(A_LPNUM=1,HB_BCODE(x)-48,val(x))
      #define D_LPVALFIELD(x) D_LPVAL(x)
   #endif
#else
   #define D_LPVAL(x) (HB_BCODE(x)-48)
   #define D_LPVALFIELD(x) D_LPVAL(x)
#endif
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPSTRFIELD(x) str(D_LPVALFIELD(x),3)

#ifdef A_ADS
#ifndef A_STSIMPLE
#define A_STSIMPLE
#endif
#endif

MEMVAR _snorm,_sel,mag_biez,mag_poz,magazyny,adres_mag,stary_rok,defa,zmienna1,zmienna3,is_spec,miar_opcja,dok_rozch,oprn
field nr_rys,nazwa,rodz_opak,gram,jm_opcja
**********************************************
PROCEDURE pomocnicze
field index,haslo_spec
memvar menupom
local m,txt,i

#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif

#ifdef A_DDBF
DatY->(dbgoto(1))
#else
#define DatY MEMVAR
#endif

m=menupom
SET COLOR TO GR+/GR

@ 7,maxcol()/2+15,16,maxcol()/2+33 BOX UNICODE '╔═╗║╝═╚║'

if !iscolor()
   set color to W
endif

@ 18,maxcol()/2-39,20,maxcol()/2+39 BOX UNICODE '╔═╗║╝═╚║ '

if iscolor()
   SET COLOR TO W+/GR
endif

SET MESSAGE TO 19 center

@  8,maxcol()/2+16 PROMPT '1. Inwentaryzacja' UNICODE MESSAGE 'Drukowanie "Arkusza spisu z natury" dla określonych gałęzi kodu materiału.'
@  9,maxcol()/2+16 PROMPT '2. Dzień pocz.   ' UNICODE MESSAGE 'Wybór dnia, od którego wyświetlane są ruchy.'
@ 10,maxcol()/2+16 PROMPT '3. Zamknięcie.   ' UNICODE MESSAGE 'Zamknięcie miesiąca (lub roku), kontrola stanów na podst. ruchów.'
@ 11,maxcol()/2+16 PROMPT '4. Otwarcie.     ' UNICODE MESSAGE 'Otwarcie pomyłkowo zamkniętego miesiąca lub dokumentu.'
@ 12,maxcol()/2+16 PROMPT '5. Symbole i kody' UNICODE MESSAGE 'Przeglądanie i porawianie baz danych, opisów, symboli, kodów, haseł, itp.'
IF STARY_ROK#NIL
@ 13,maxcol()/2+16 PROMPT '6. Zmiana roku.  ' UNICODE MESSAGE 'Wejście do poprzedniego roku lub powrót do bieżącego roku.'
 ELSE
@ 13,maxcol()/2+16 PROMPT '6. Stary rok.    ' UNICODE message 'Wejście do zeszłego roku, zmiany przenoszone na bieżący rok po zamknięciu.'
ENDIF
@ 14,maxcol()/2+16 PROMPT '7. Ilość linii.  ' UNICODE message 'Zmiana ilości linii widocznych na ekranie.'
@ 15,maxcol()/2+16 PROMPT '8. Ratuj.        ' UNICODE message 'Reindeksacja baz - Odtwarzanie skorowidzy.'
      SETKEY(4,{||NIL})
      SETKEY(19,{||KIBORD(CHR(27)+CHR(5)+CHR(13))})
    MENU TO MENUPOM
  SETPOS(MENUPOM+7,maxcol()/2+16)
SET KEY 4 TO
SET KEY 19 TO

IF menupom=0
   menupom=m
   RETURN
ENDIF

m=menupom

 SET COLOR TO W

@ 21,0 clear

DO CASE

   CASE m=1
        inwentura()

   CASE m=2
        data_gran()

   CASE m=3
        zamkniecie()

   CASE m=4
    if iS_spec
    hAslo_spec(21)
    if DatY->d_z_mies1>DatY->d_z_rok .and. alarm(hb_UTF8ToStr(if(DatY->d_z_mies1>DatY->d_z_mies2,"CZY OTWORZYĆ MIESIĄC ?","CZY OTWORZYĆ CAŁY ROK ?")),{"Tak","Nie"})=1
      if stary_rok#NIL
         txt:=defa+str(year(stary_rok+D_OLZA),4)+HB_ps()
         if ! file(txt+'daty.*')
           txt:=defa+'roboczy'+HB_ps()
         endif
#ifdef A_DDBF
         select 0
         nuse (txt+"daty") alias nowy readonly
         #define D_Z1 nowy->d_z_mies1
         #define D_Z3 nowy->d_z_rok
#else
         txt:=getlines(memoread(txt+"daty.ini"))
         #define D_Z1 &(SubStr(txt[1],at(':=',txt[1])+2))
         #define D_Z3 &(SubStr(txt[3],at(':=',txt[3])+2))
#endif
         if D_Z1#D_Z3
            alarm(hb_UTF8ToStr("Otwarcie niemożliwe;Nowy rok jest zamknięty do ")+dtoc(D_Z1)+" .")
#ifdef A_LAN
            use
#endif
            break
         endif
#ifdef A_LAN
         use
#endif
#undef D_Z1
#undef D_Z3
       endif
#ifdef A_DDBF
       select daty
       LOCK
#endif
       if DatY->d_z_mies1=DatY->d_z_mies2
          DatY->d_z_mies2:=DatY->d_z_rok
       endif
       DatY->data_gran:=DatY->d_z_mies1:=DatY->d_z_mies2
#ifdef A_DDBF
       UNLOCK
#else
       inisave(set(_SET_DEFAULT)+"daty.ini")
#endif
    endif

//#ifndef A_KONTROLA
    endif
//#endif

   CASE m=5 .and. iS_spec

    hAslo_spec(21)
    browse()

   CASE m=6

   if stary_rok#NIL
      txt:={hb_UTF8ToStr("POWRÓT")} //nie wiem, jak gęboko zakopany
   else
      txt:={}
   endif
   m:=year(if(stary_rok=NIL,DatY->d_z_rok D_OLDA,date()))
   do while .t.
      if !file(defa+str(m,4)+HB_ps()+"daty.*")
         if stary_rok=NIL .or. m<year(DatY->d_z_rok+D_OLZA)
            exit
         endif
         --m
         loop
      elseif stary_rok#NIL .and. year(stary_rok D_OLDA)=m
         --m
         loop
      endif
      aadd(txt,str(m,4))
      --m
   enddo
   setpos(11,maxcol()/2+25)
   m:=1
   aczojs(txt,"",@m)
   altd()
   if stary_rok#NIL .and. m=1
      stary_rok:=NIL
   elseif m#0
      m:=txt[m]
#ifdef A_OLDA
      STARY_ROK:=stod(m+"1130")
#else
      STARY_ROK:=stod(m+"1231")
#endif
   endif
   set default to (defa+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+HB_OsPathSeparator())
   readinit()
   reuse()
   if stary_rok<>NIL .and. DatY->data_gran<>DatY->d_z_rok
#ifndef DatY
      LOCK IN DatY
#endif
      DatY->data_gran:=DatY->d_z_rok
#ifndef DatY
      UNLOCK IN DatY
#endif
   endif

   case m=7
      if isega()
      setpos(11,maxcol()/2+25)
      m:=1
//#ifdef __PLATFORM__WINDOWS      
      if hb_gtInfo( HB_GTI_ISGRAPHIC )
         aczojs({"98x32","80x25 (Norm)",hb_UTF8ToStr("Zmieniaj ilość linii"),hb_UTF8ToStr("Zmieniaj wielkość czcionki"),hb_UTF8ToStr("Pełny ekran (Alt+Enter)")},"",@m)
         do case
         case m<2
         SetMode(32,98)
         case m=2
         SetMode(25,80)
         case m=3
         hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
         case m=4
         hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
         case m=5
         hb_gtInfo( HB_GTI_ISFULLSCREEN, ! hb_gtInfo( HB_GTI_ISFULLSCREEN ) )
         endcase
      else
//#endif
         aczojs({"80x25 (Norm)","80x30","80x43","80x50","80x60","132x25","132x30","132x43","132x50","132x60"},"",@m)
         sysint(16,3)
         setmode(25,80)
         if m<2
         setmode(25,80)
         elseif m=2
         sysint(16,36)
         elseif m=3
         setmode(43,80)
         elseif m=4
         setmode(50,80)
         elseif m=5
         setmode(60,80)
         //sysint(16,38) //80x60
         sysint(16,20226,264)
         elseif m=6
         setmode(25,132)
         sysint(16,20226,265) //132x25
         //sysint(16,32)
         elseif m=7
         setmode(30,132)
         sysint(16,33) //132x30
         elseif m=8
         setmode(43,132)
         //sysint(16,34) //132x43
         sysint(16,20226,266)
         elseif m=9
         setmode(50,132)
         sysint(16,20226,267) //132x50
         elseif m=10
         setmode(60,132)
         //sysint(16,35) //132x60
         sysint(16,20226,268)
         endif
         setmode(,)
         if maxcol()<79
            sysint(16,3)
            setmode(25,80)
            setmode(25,80)
         endif
//#ifdef __PLATFORM__WINDOWS      
      endif
//#endif
      init screen
#ifdef A_MYSZ
      sysint(51,0)
#endif
       else
          alarm(hb_UTF8ToStr("NIE DOSTĘPNE NA MONITORZE TYPU HERCULES"))
       endif
     case m=8
       ratuj()
ENDCASE

RETURN
*****************
PROCEDURE DATA_GRAN

local i:=3
        lock in daty
        DO CASE
           CASE DatY->data_gran=DatY->d_z_mies1 ; i=1
           CASE DatY->data_gran=DatY->d_z_mies2 ; i=2
        ENDCASE
        if iscolor()
           SET COLOR TO BG+/B
        endif
        @ 20,maxcol()/2-20,23,maxcol()/2+20 BOX UNICODE '╔═╗║╝═╚║ '
        @ 20,maxcol()/2-15 SAY "Wybierz jedną z podanych dat:" UNICODE
        SET MESSAGE TO 22 CENTER
        @ 21,maxcol()/2-18 PROMPT dtoc(DatY->d_z_mies1) UNICODE MESSAGE "        Tylko ostatnie ruchy.       "
        @ 21,maxcol()/2-6 PROMPT dtoc(DatY->d_z_mies2) UNICODE MESSAGE "Ruchy w ostatnio zamykanym miesiącu."
        @ 21,maxcol()/2+6 PROMPT dtoc(DatY->d_z_rok) UNICODE MESSAGE "           Wszystkie ruchy.         "

        MENU TO i
        DO CASE
           CASE i=1
                DatY->data_gran:=DatY->d_z_mies1
           CASE i=2
                DatY->data_gran:=DatY->d_z_mies2
           CASE i=3
                DatY->data_gran:=DatY->d_z_rok
        ENDCASE

#ifdef A_DDBF
    unlock in DatY
#else
    inisave(set(_SET_DEFAULT)+"daty.ini")
#endif

RETURN

***********************************
PROCEDURE ZAMKNIECIE // zamkniecie miesiaca, sprawdzenie zgodnosci stanow

memvar MAG_poz,year2bckp

field index,ilosc,stan,data,ZAMK_MIES1,ZAMK_MIES2,ZAMKN_ROKU,SJMO,SJMO_MIES1,SJMO_MIES2,SJMO_ROKU,LAMUS,DATA_ZMIAN,nr_mag

#ifdef A_WA
field wartosc,WART_MIES1,WART_MIES2,WART_ROKU
#endif
LOCAL dz1,dz2,dz3,S,W,O,KEY,canfix,ar,ar1,delcount,m,x

begin sequence
close databases
#ifdef A_DDBF
nuse daty
lock
#endif

@ 17,0 clear

dz1:=date()+3
dz1-=day(dz1)

IF dz1>DatY->d_z_mies1
  if stary_rok#NIL
     dz1:=stary_rok
  else
     do while YEAR(dz1+D_OLZA-1)>YEAR(DatY->D_Z_rok+D_OLZA)
       dz1-=day(dz1)
     enddo
  endif

  if stary_rok=NIL .and. DatY->d_z_mies1=DatY->d_z_rok .and. file(defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty.*")
#ifdef A_DDBF
     select 0
     nuse (defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty") alias stary readonly
     #define D_Z1 stary->d_z_mies1
#else
     m:=getlines(memoread(defa+str(year(DatY->d_z_rok),4)+HB_ps()+"daty.ini"))[1]
     #define D_Z1 &(SubStr(m,at(':=',m)+2))
#endif
     if D_Z1<DatY->d_z_rok
        alarm("Najpierw zamknij rok"+str(year(D_Z1+D_OLZA),5)+hb_UTF8ToStr(" !;Nie można teraz zamknąć roku")+str(year(DatY->d_z_rok+D_OLZA),5)+" !")
        dz1:=DatY->d_z_mies1
     endif
#ifdef A_DDBF
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
#ifdef A_OLDA
      aadd(ar,DTOC(dz1))
#else
      aadd(ar,DTOV(dz1))
#endif
      aadd(ar1,dz1)
      dz1-=day(dz1)
    enddo
    aadd(ar,hb_UTF8ToStr("Kontrola Stanów"))
    aadd(ar1,DatY->d_z_mies1)
    if 0=(key:=alarm(hb_UTF8ToStr("Zamknięcie do:;( Esc - Rezygnacja )"),ar))
      break
    endif
    dz1:=ar1[key]
     select 4
     nuse indx_mat
#ifdef A_CDX
     set order to TAG indx_num in indx_mat
     go top
#else
     SET index to indx_num
#endif
#ifndef STANY
     select 0
     nuse stany
     set relation to index into indx_mat
#ifdef A_CDX
      set order to TAG STAN_MAG in STANY
      go top
#else
      set index to stan_mag
#endif
      delcount:=0
#endif
      select 0
      nuse main
#ifdef A_CDX
      set order to TAG main_ind in MAIN
      go top
#else
      set index to main_ind
#endif
#ifdef A_LAN
  if dz1>DatY->d_z_mies1
    select 0
    nuse dm
    if !filock(.t.,hb_UTF8ToStr("Proszę wszystkich użytkowników sieci o wyjście na chwilę z wprowadzania/poprawiania dokumentów, aby można było upewnić się, że nikt nie zmienia danych z zamykanego okresu. Po rozpoczęciu zamykania można będzie powrócić do przerwanej pracy.;Omiń - tylko kontrola stanów"))
       dz1:=DatY->d_z_mies1
    else
       DatY->data_gran:=dz1
       DatY->(dbgoto(1))
    endif
    use
    select main
  endif
#endif

  ?? hb_UTF8ToStr("PRZYGOTUJ DRUKARKĘ (NA WSZELKI WYPADEK).")
  ?
  ?
#ifdef A_HPDF
  #define D_HWPRN A_HPDF
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
#ifdef D_HWPRN
  //if D_HWPRN
    oprn:=.f.
    set printer to zamkn.prn
  //endif
#endif
  if dz1>DatY->d_z_mies1
    dz2:=DatY->d_z_mies1
    dz3:=DatY->d_z_mies2
#ifdef A_OLDA
    IF month(dz1) = 11 .and. dz1>DatY->d_z_rok
#else
    IF month(dz1) = 12 .and. dz1>DatY->d_z_rok
#endif
      ?? hb_UTF8ToStr("Kontrola stanów i zamknięcie roku !")
    else
      ?? hb_UTF8ToStr("Kontrola stanów i zamknięcie do dnia"),dz1
    endif
    DatY->d_z_mies1:=DatY->d_z_mies2:=DatY->d_z_rok
  #ifndef A_DDBF
    inisave(set(_SET_DEFAULT)+"daty.ini")
  #endif
  else
     ?? hb_UTF8ToStr("Kontrola stanów.")
     dz2:=DatY->d_z_mies2
     dz3:=dz2
  endif
  ?
  ?


DO WHILE !(EOF() .and. STANY->(eof()))
#ifdef A_LAN
   //if !canfix //tylko kontrola stanów
      unlock in STANY
      lock in STANY
   //endif
#endif

   SETPOS(row(),0)

   IF (STANY->(NR_MAG+index)>NR_MAG+INDEX .OR. STANY->(eof())).and.!eof()
#ifdef A_IZ
#ifdef A_WA
    locate rest for ilosc#0 .or. wartosc#0 while {||STANY->(EOF()) .or. NR_MAG+INDEX<STANY->NR_MAG+STANY->index}
#else
    locate rest for ilosc#0 while {||STANY->(EOF()) .or. NR_MAG+INDEX<STANY->NR_MAG+STANY->index}
#endif
    if !found()
       loop
    endif
#endif
  begin sequence
      PRINT(1)
  end
      ?? hb_UTF8ToStr("W BAZIE STANY BRAKUJE MATERIAŁU O KODZIE:"),NR_MAG,tran(INDEX,"@R "+ INDEXPIC )
#ifdef A_JMALTTOT
#define D_JMA A_JMALTTOT(ILOSC,MAIN->nr_zlec,4,x)
#endif
#ifndef STANY
    if INDX_MAT->(DBSEEK(MAIN->INDEX,.F.))
       ? indx_mat->NAZWA
       ? hb_UTF8ToStr("ZAKŁADAM, ŻE STAN BYŁ ZEROWY NA POCZĄTKU ROKU !!!!")
       STANY->(dbappend())
       STANY->nr_mag:=nr_mag
       STANY->index:=index
       if dz3>DatY->d_z_rok
        #ifdef A_WA
#ifdef A_JMALTTOT
           SUM ILOSC,WARTOSC,D_JMA to s,w,o rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(Dz3)}
           STANY->SJMO_mies2:=round(o,3)
#else
           SUM ILOSC,WARTOSC to s,w rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(Dz3)}
#endif
           STANY->WART_mies2:=round(W,A_ZAOKR)
        #else
           SUM ILOSC to s rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(Dz3)}
        #endif
           STANY->zamk_mies2:=round(s,3)
       endif
    else
#endif
    ?
    ?
    SEEK NR_MAG+INDEX + "Z" // nastepny indeks (SOFTSEEK IS ON)
      LOOP
#ifndef STANY
    endif
#endif
   ENDIF
   SEEK STANY->NR_MAG+STANY->index+dtos(dz3) + "Z"
   if dz3=DatY->d_z_rok
     S:=STANY->ZAMKN_roku
#ifdef A_WA
     W:=STANY->WART_roku
#ifdef A_JMALTTOT
     O:=STANY->SJMO_roku
#endif
#endif
   else
     S:=STANY->ZAMK_mies2
#ifdef A_WA
     W:=STANY->WART_mies2
#ifdef A_JMALTTOT
     O:=STANY->SJMO_mies2
#endif
#endif
   endif
#ifdef A_WA
#ifdef A_JMALTTOT
   EXECUTE {||S+=ILOSC,W+=WARTOSC,O+=D_JMA} rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz2)}
   STANY->SJMO_mies2:=round(o,3)
#else
   EXECUTE {||S+=ILOSC,W+=WARTOSC} rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz2)}
#endif
   STANY->WART_mies2:=round(W,A_ZAOKR)
#else
   EXECUTE {||S+=ILOSC} rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz2)}
#endif
   STANY->zamk_mies2:=round(s,3)

#ifdef A_ZERUJSTAN
   if dz3>DatY->d_z_rok
   STANY->stan-=s
   s:=STANY->zamk_mies2:=0
#ifdef A_WA
   STANY->wartosc-=w
   w:=STANY->wart_mies2:=0
#endif
#ifdef A_JMALTTOT
   o:=STANY->SJMO_mies2:=0
#endif
   endif
#endif

#ifdef A_WA
#ifdef A_JMALTTOT
   EXECUTE {||S+=ILOSC,W+=WARTOSC,o+=D_JMA} rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz1)}
   STANY->WART_mies1:=round(W,A_ZAOKR)
   STANY->zamk_mies1:=round(s,3)
   STANY->sjmo_mies1:=round(o,3)
   EXECUTE {||S+=ILOSC,W+=WARTOSC,o+=D_JMA} rest while NR_MAG+INDEX=STANY->NR_MAG+STANY->index
#else
   EXECUTE {||S+=ILOSC,W+=WARTOSC} rest while {||NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz1)}
   STANY->WART_mies1:=round(W,A_ZAOKR)
   STANY->zamk_mies1:=round(s,3)
   EXECUTE {||S+=ILOSC,W+=WARTOSC} rest while NR_MAG+INDEX=STANY->NR_MAG+STANY->index
#endif
#else
   EXECUTE {||S+=ILOSC} rest while NR_MAG+INDEX+dtos(DATA)<=STANY->NR_MAG+STANY->index+dtos(dz1)
   STANY->zamk_mies1:=round(s,3)
   EXECUTE {||S+=ILOSC} rest while NR_MAG+INDEX=STANY->NR_MAG+STANY->index
#endif

   SELECT STANY

#ifdef A_WA
   IF ROUND(STAN-S,3)#0 .OR. ROUND(WARTOSC-W,A_ZAOKR)#0
  begin sequence
      PRINT(1)
  end
      ?? NR_MAG+"/"+tran(indx_mat->INDEX,"@R "+ INDEXPIC ),cpad(indx_mat->NAZWA,36),stan,indx_mat->jm,wartosc
      ? hb_UTF8ToStr("RÓŻNICA W STANIE KOŃCOWYM")
      ? hb_UTF8ToStr("BYŁO"),STAN,INDX_MAT->JM,WARTOSC,hb_UTF8ToStr("zł")
    sTAN:=s
#ifndef STANY
#ifdef A_ZAGRODA
    IF INDX_MAT->zapas_id<>0
       STANY->unlink:=.t.
    ENDIF
#endif
#endif
    WARTOSC:=round(W,A_ZAOKR)
    ?? " JEST",STAN,INDX_MAT->JM,WARTOSC,hb_UTF8ToStr("zł")
    ?
    ?
  else
  @ row(),0 say nr_mag+"/"+tran(INDEX,"@R "+ INDEXPIC )+" "+left(indx_mat->NAZWA,36)+str(stan)+" "+indx_mat->jm+str(wartosc)
   ENDIF 
   WARTOSC:=round(W,A_ZAOKR)
#else
   IF ROUND(STAN-S,3)#0
  begin sequence
      PRINT(1)
  end
      ?? NR_MAG+"/"+tran(indx_mat->INDEX,"@R "+ INDEXPIC ),cpad(indx_mat->NAZWA,36),stan,indx_mat->jm
      ? hb_UTF8ToStr("RÓŻNICA W STANIE KOŃCOWYM")
      ? hb_UTF8ToStr("BYŁO"),STAN,INDX_MAT->JM
    sTAN:=s
    ?? " JEST",STAN,INDX_MAT->JM
    ?
    ?
  else
  @ row(),0 say nr_mag+"/"+tran(INDEX,"@R "+ INDEXPIC )+" "+left(indx_mat->NAZWA,36)+str(stan)+" "+indx_mat->jm
   ENDIF 
#endif

   STAN:=round(s,3)
#ifdef A_JMALTTOT
   sjmo:=round(o,3)
#endif
   unlock

   x:=nr_mag+index
   DO WHILE ! EOF()
      SKIP
      IF x<>nr_mag+index
        EXIT
      ENDIF

  begin sequence
      PRINT(1)
  end
      ?? NR_MAG+"/"+tran(indx_mat->INDEX,"@R "+ INDEXPIC ),cpad(indx_mat->NAZWA,36),stan,indx_mat->jm
      ? hb_UTF8ToStr("ZDUBLOWANA KARTOTEKA - USUNĄŁEM!!!")
      ?
      ?
      LOCK
      DELETE
   ENDDO

#ifndef STANY
   ++delcount
#endif
   SELECT MAIN
ENDDO

unlock in STANY

SET PRINT OFF

DatY->d_z_mies2:=dz2
DatY->data_gran:=dz2
DatY->d_z_mies1:=dz1

#ifndef A_DDBF
    inisave(set(_SET_DEFAULT)+"daty.ini")
#endif

unlock all


if stary_rok=dz1
  dz2:=defa+str(year(dz1+D_OLZA),4)+HB_ps()
  if ! file(dz2+'daty.*')
   dz2:=defa+'roboczy'+HB_ps()
  endif
#ifdef A_DDBF
     nuse (dz2+"daty") alias ndat
     lock
     #define D_Z1 ndat->d_z_mies1
#else
     use
     m:=getlines(memoread(dz2+"daty.ini"))[1]
     #define D_Z1 &(SubStr(m,at(':=',m)+2))
#endif
   set color to (_sel)
   ? hb_UTF8ToStr("SPRAWDZENIE ZGODNOŚCI STANU KOŃCOWEGO I BILANSU OTWARCIA NOWEGO ROKU.")+chr(7)
   ?
   set color to (_snorm)
   canfix:=D_Z1=DatY->d_z_mies1 .and. 1=alarm(hb_UTF8ToStr("Czy przenieść STAN KOŃCOWY;na BILANS OTWARCIA NOWEGO ROKU"),{"TAK","NIE"})
#undef D_Z1
      select 0
      nuse ind_lam readonly

          select 0
#ifdef A_DIETA

          nuse (dz2+"indx_mat") shared alias nowy_idx
#ifndef A_LAN
          flock()
#endif
#else
          nuse (dz2+"indx_mat") alias nowy_idx
#endif
#ifdef A_CDX
      set index to (dz2+"indx_mat")
#else
      set index to (dz2+"indx_naz"),(dz2+"indx_num")
#endif
      SET ORDER TO TAG INDX_NUM
      
      
#ifdef STANY
      go top
      select 0
#define NOWY NOWY_IDX
#else
      select 0
//#define IND_LAM indx_mat
      nuse (dz2+"stany") alias nowy
#ifdef A_CDX
      set index to (dz2+"stany")
#else
#ifdef A_STSIMPLE
      set index to (dz2+"stan_ind"),(dz2+"stan_mag")
#else
      set index to (dz2+"stan_naz"),(dz2+"stan_mag"),(dz2+"stan_all"),(dz2+"stan_ind")
#endif
#endif
      set order to TAG STAN_MAG
      go top
#endif

  SELECT STANY
  GO TOP

  DO WHILE !EOF() .OR. !NOWY->(EOF())

    @ row(),0 say nr_mag+"/"+tran(INDEX,"@R "+ INDEXPIC )

    IF !NOWY->(EOF()).and.NR_MAG+INDEX>NOWY->(NR_MAG+INDEX) .OR. EOF()
#ifdef A_WA
#ifdef A_JMALTTOT
      IF NOWY->(ZAMKN_ROKU#0 .OR. WART_ROKU#0 .or. sjmo_roku#0)
#else
      IF NOWY->(ZAMKN_ROKU#0 .OR. WART_ROKU#0)
#endif
#else
      IF NOWY->ZAMKN_ROKU#0
#endif
#ifndef STANY
        NOWY_IDX->(DBSEEK(NOWY->INDEX,.F.))
#endif
        PRINT(1)
        ? "MAGAZYN",NOWY->NR_MAG,"KOD:",tran(NOWY->INDEX,"@R "+ INDEXPIC )
        ? "NAZWA BO:",NOWY_IDX->NAZWA
        ? hb_UTF8ToStr("STAN KOŃCOWY:  BRAK !")
#ifdef A_WA
        ? "STAN BO:     ",NOWY->ZAMKN_ROKU,NOWY_IDX->jm,NOWY->WART_ROKU,hb_UTF8ToStr("zł")
        if canfix
           lock in NOWY
           ? "DOKONANO KOREKTY BO !"
           NOWY->(wartosc:=stan:=ZAMKN_ROKU:=zamk_mies2:=zamk_mies1:=wart_roku:=wart_mies2:=wart_mies1:=0)
#ifndef STANY
#ifdef A_ZAGRODA
           IF NOWY_IDX->zapas_id<>0
             NOWY->unlink:=.t.
           ENDIF
#endif
#endif
#ifdef A_JMALTTOT
           NOWY->(sjmo:=sjmo_roku:=sjmo_mies1:=sjmo_mies2:=0)
#endif
           UNLOCK IN NOWY
        endif
#else
        ? "STAN BO:     ",NOWY->ZAMKN_ROKU,NOWY_IDX->jm
        if canfix
           lock in NOWY
           ? "DOKONANO KOREKTY BO !"
           NOWY->(stan:=ZAMKN_ROKU:=zamk_mies2:=zamk_mies1:=0)
           UNLOCK IN NOWY
        endif
#endif
        ?
      ENDIF
      NOWY->(DBSKIP(1))
    ELSEIF NR_MAG+INDEX<NOWY->(NR_MAG+INDEX) .OR. NOWY->(EOF())
//#ifndef A_ZAGRODA
#ifdef A_WA
#ifdef A_JMALTTOT
      IF ZAMK_MIES1#0 .OR. WART_MIES1#0  // .or. sjmo_mies1#0
#else
      IF ZAMK_MIES1#0 .OR. WART_MIES1#0
#endif
#else
      IF ZAMK_MIES1#0
#endif
//#endif
#ifndef STANY
        INDX_MAT->(DBSEEK(STANY->INDEX,.F.))
#endif
        if !canfix
        PRINT(1)
        endif
        ? "MAGAZYN",NR_MAG,"KOD:",tran(INDEX,"@R "+ INDEXPIC )
        ? "NAZWA:   ",indx_mat->NAZWA
#ifdef A_WA
        ? hb_UTF8ToStr("STAN KOŃCOWY:"),ZAMK_MIES1,indx_mat->jm,WART_MIES1,hb_UTF8ToStr("zł")
#else
        ? hb_UTF8ToStr("STAN KOŃCOWY:"),ZAMK_MIES1,indx_mat->jm
#endif
        ? "STAN BO:       BRAK !"
        if canfix
           NOWY->(dbappend())
           ? "DOKONANO KOREKTY BO !"
           FOR KEY=1 TO FCOUNT()
               NOWY->(FIELDPUT(KEY,STANY->(FIELDGET(KEY))))
           NEXT
           NOWY->zamkn_roku:=NOWY->zamk_mies2:=NOWY->zamk_mies1
#ifndef STANY
#ifdef A_ZAGRODA
           IF INDX_MAT->zapas_id<>0
             NOWY->unlink:=.t.
           ENDIF
#endif
#endif
#ifdef A_WA
           NOWY->wart_roku:=NOWY->wart_mies2:=NOWY->wart_mies1
#ifdef A_JMALTTOT
           NOWY->(sjmo_roku:=sjmo_mies2:=sjmo_mies1)
#endif
#endif
#ifndef STANY
           if !NOWY_IDX->(DBSEEK(STANY->INDEX))
              NOWY_IDX->(dbappend())
           else
              lock in NOWY_IDX
           ENDIF
           select indx_mat
           FOR KEY=1 TO FCOUNT()
               NOWY_IDX->(FIELDPUT(KEY,INDX_MAT->(FIELDGET(KEY))))
           NEXT
           unlock in NOWY_IDX
           select stany
#endif
           unlock in NOWY
           NOWY->(DBSKIP(1))
         endif
         ?
//#ifndef A_ZAGRODA
      ENDIF
//#endif
      DBSKIP(1)
    ELSE
#ifndef STANY
      NOWY_IDX->(DBSEEK(NOWY->INDEX,.F.))
      INDX_MAT->(DBSEEK(STANY->INDEX,.F.))
#endif
#ifdef A_WA
#ifdef A_JMALTTOT
      IF /* sjmo_mies1#NOWY->sjmo_roku .or. */ round(ZAMK_MIES1-NOWY->ZAMKN_ROKU,3)<>0 .OR. round(WART_MIES1-NOWY->WART_ROKU,A_ZAOKR)<>0
#else
      IF round(ZAMK_MIES1-NOWY->ZAMKN_ROKU,3)<>0 .OR. round(WART_MIES1-NOWY->WART_ROKU,A_ZAOKR)<>0
#endif
#else
      IF round(ZAMK_MIES1-NOWY->ZAMKN_ROKU,3)<>0
#endif
        if !canfix
        PRINT(1)
        endif
        ? "MAGAZYN",NR_MAG,"KOD:",tran(INDEX,"@R "+ INDEXPIC )
        ? "NAZWA:   ",indx_mat->NAZWA
        if indx_mat->NAZWA#NOWY_IDX->NAZWA
        ? "NAZWA BO:",NOWY_IDX->NAZWA
        endif
#ifdef A_WA
        ? hb_UTF8ToStr("STAN KOŃCOWY:"),ZAMK_MIES1,indx_mat->jm,WART_MIES1,hb_UTF8ToStr("zł")
        ? "STAN BO:     ",NOWY->ZAMKN_ROKU,NOWY_IDX->jm,NOWY->WART_ROKU,hb_UTF8ToStr("zł")
#else
        ? hb_UTF8ToStr("STAN KOŃCOWY:"),ZAMK_MIES1,indx_mat->jm
        ? "STAN BO:     ",NOWY->ZAMKN_ROKU,NOWY_IDX->jm
#endif
      if canfix
        lock in NOWY
        ? "DOKONANO KOREKTY BO !"

#ifndef STANY
#ifdef A_ZAGRODA
        IF NOWY_IDX->zapas_id<>0
           NOWY->unlink:=.t.
        ENDIF
#endif
#endif
        NOWY->STAN+=ZAMK_MIES1-NOWY->ZAMKN_ROKU
        NOWY->zamkn_roku:=NOWY->zamk_mies2:=NOWY->zamk_mies1:=ZAMK_MIES1
#ifdef A_WA
        NOWY->WARTOSC+=WART_MIES1-NOWY->WART_ROKU
        NOWY->wart_roku:=NOWY->wart_mies2:=NOWY->wart_mies1:=WART_MIES1
#ifdef A_JMALTTOT
        NOWY->sjmo+=sjmo_MIES1-NOWY->sjmo_ROKU
        NOWY->sjmo_roku:=NOWY->sjmo_mies2:=NOWY->sjmo_mies1:=sjmo_MIES1
#endif
#endif
        unlock in NOWY
      endif
        ?
      ENDIF
      if NOWY_IDX->data_popr<indx_mat->data_popr
      select indx_mat
      s:={}
      FOR KEY=1 TO IND_LAM->(FCOUNT())
         if NOWY_IDX->data_popr<=indx_mat->data_popr;
          .and.NOWY_IDX->(FIELDget(KEY))#FIELDGET(KEY);
          .and.(.not."DATA"$fieldname(key) .or. NOWY_IDX->(FIELDget(KEY))<FIELDGET(KEY))
            if fieldname(key)="LAMUS"
               loop
            endif
            if empty(s)
               PRINT(1)
               ? "MAGAZYN",STANY->NR_MAG,"KOD:",tran(INDEX,"@R "+ INDEXPIC )
               ? "NAZWA:   ",NAZWA
               if NAZWA#NOWY_IDX->NAZWA
                  ? "NAZWA BO:",NOWY_IDX->NAZWA
               endif
               ? hb_UTF8ToStr("RÓŻNICA ZAWARTOŚCI PÓL: ")
               if canfix
                  LOCK IN NOWY_IDX
               endif
            endif
            ? fieldname(key),hb_UTF8ToStr("STAN KOŃCOWY"),fieldget(key),"STAN BO",NOWY_IDX->(fieldget(key))
            aadd(s,fieldname(key))
            if canfix
               NOWY_IDX->(FIELDPUT(KEY,INDX_MAT->(FIELDGET(KEY))))
            endif
         endif
      next
      if !empty(s)
         if canfix
            ? "DOKONANO KOREKTY BO !"
            NOWY_IDX->DATA_POPR:=DatE()
            UNLOCK IN NOWY_IDX
         endif
         ?
      endif
      select STANY
      endif
      DBSKIP(1)
      NOWY->(DBSKIP(1))
    ENDIF
  ENDDO
  set print off
  ?
  ?
//#undef IND_LAM
#undef NOWY
#undef NOWY_IDX
elseIF year(dz1+D_OLZA) > year(DatY->d_z_rok+D_OLZA)
   set color to (_sel)
   ? hb_UTF8ToStr("ZAMKNIĘCIE ROKU, TROCHĘ POTRWA")
   set color to (_snorm)
   ?
   close databases
#ifdef A_DIETA
   select 4
   nuse indx_mat READONLY EXCLUSIVE alias "YYYY" //żeby nikt się nie pchał do "roboczy"
   if !used()
      break
   endif
   close databases
#endif
#ifdef A_DDBF
   nuse daty READONLY EXCLUSIVE //żeby nikt się nie pchał do "roboczy"
   if !used()
      break
   endif
#endif
     if !file(defa+"kopia.*") .and. !file(defa+"archiwum"+HB_ps()+"kopi"+str(year(DatY->d_z_mies1),4)+".*")
        alarm(hb_UTF8ToStr("Brak kopii bezpieczeństwa danych.;Nie będę zamykać roku!"),0,3)
        break
     endif
     mkdir(defa+"archiwum")
     ?
     ? "Przenoszenie starych danych do katalogu ARCHIWUM:"
     AEVAL(DIRECTORY(defa+"kopia.*"),{|X|if(file(defa+"archiwum"+HB_ps()+"kopi"+str(year(DatY->d_z_mies1),4)+SubStr(x[1],at(".",x[1]))),,(QOUT(X[1]),frename(defa+X[1],defa+"archiwum"+HB_ps()+"kopi"+str(year(DatY->d_z_mies1),4)+SubStr(x[1],at(".",x[1])))))})


   begin sequence

    s:=errorblock({|e|if(e:severity>1,break(e),.f.)})


  ?
  ? hb_UTF8ToStr("Kasowanie pozostałości kartoteki ROBOCZY.")

   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.txt"),{|X|QOUT(X[1]),FERASE(defa+"roboczy"+HB_ps()+X[1])})
   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.prn"),{|X|QOUT(X[1]),FERASE(defa+"roboczy"+HB_ps()+X[1])})
   //AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.??x"),{|X|QOUT(X[1]),FERASE(defa+"roboczy"+HB_ps()+X[1])})

   mkdir(defa+"tmp")
   SET DEFAULT TO (defa+"tmp"+HB_OsPathSeparator())              // roboczy katalog

  ?
  ? "Kasowanie kartoteki TMP."

   AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.*"),{|X|QOUT(X[1]),FERASE(defa+"tmp"+HB_ps()+X[1])})
  ?
  ? hb_UTF8ToStr("Tworzenie nowych zbiorów tymczasowo w kartotece TMP:")
  ?
  ? "Tworzenie zbioru DATY"
 

#ifdef A_DDBF
   copy to daty
   use
   nuse daty exclusive new
   for w:=1 to fcount()
       if valtype(fieldget(w))='N'
          fieldput(w,0)
       endif
   next w
#endif
#ifdef A_KPR
   DatY->last_kpr:=0
#endif

   DatY->d_z_rok=DatY->d_z_mies1
   DatY->d_z_mies2=DatY->d_z_rok
   DatY->data_gran=DatY->d_z_rok

#ifndef A_DDBF
    copy file (defa+"roboczy"+HB_ps()+"daty.ini") to daty.ini
    inisave(set(_SET_DEFAULT)+"daty.ini")
#endif
   //select 0
   xselect indeks
   key:=''
   do while !eof()
      if .not. key==(key:=lower(trim(FIELD->baza))) .and. file(defa+"roboczy"+HB_ps()+key+".dbf")
         ?
         select 0
         nUSE (defa+"roboczy"+HB_ps()+key) READONLY ALIAS YYYY
         ? "Tworzenie bazy",key
         COPY structure TO (key)
         USE
         select indeks
      endif
      skip
   enddo

   IF FILE(defa+"tmp"+HB_ps()+"firmy.dbf")
  ?
  ?   hb_UTF8ToStr("Przenoszenie kontrahentów.")

      nuse firmy exclusive
      append from (defa+"roboczy"+HB_ps()+"firmy")
   ENDIF
  ?
  ?   "Przenoszenie kartotek magazynu."

  key:=2
  w:=DatY->d_z_rok-31
  do while .t.
     tone(130,3)
     alarm(hb_UTF8ToStr("Kasowanie kartotek,;które mają stan zerowy po dniu: ")+dtoc(w)+";(klawisze [+] i [-] - zmiana daty);Esc - bez kasowania" ,{"-","OK","+"},@key)
     if key=3
        w+=32
     elseif key#1
        exit
     endif
     w-=day(w)
     w:=min(w,DatY->d_z_rok)
  enddo
    use
    select 4
    use
    nUSE (defa+"roboczy"+HB_ps()+"indx_mat") READONLY
#ifdef A_CDX
     set order to tag indx_num in (defa+"roboczy"+HB_ps()+"indx_mat")
#else
     set index to (defa+"roboczy"+HB_ps()+"indx_num")
#endif

#ifndef STANY
     select 0
     nUSE (defa+"roboczy"+HB_ps()+"stany") READONLY
#ifdef A_CDX
     set order to TAG STAN_MAG in (defa+"roboczy"+HB_ps()+"stany")
#else
     set index to (defa+"roboczy"+HB_ps()+"stan_mag")
#endif
     if key#0
#ifdef A_WA
       copy to stany for stan#0 .or. wartosc#0 .or. data_zmian>w
#else
       copy to stany for stan#0 .or. data_zmian>w
#endif
     else
       copy to stany
     endif

     nuse stany exclusive
#else
     if key#0
#ifdef A_WA
     copy to indx_mat for stan#0 .or. wartosc#0 .or. data_zmian>w
#else
     copy to indx_mat for stan#0 .or. data_zmian>w
#endif
     else
     copy to indx_mat
     endif

     nuse indx_mat exclusive
#endif

#ifdef A_WA
#define D_WA ,STANY->wart_roku:=STANY->wart_mies2:=STANY->wart_mies1
#else
#define D_WA
#endif
#ifdef A_LIFO
#define D_LIFO ,STANY->VALIDX:=.f.
#else
#define D_LIFO
#endif
#ifdef A_JMALTTOT
     EXEC {||STANY->sjmo_ROKU:=STANY->sjmo_MIES2:=STANY->sjmo_MIES1,STANY->ZAMKN_ROKU:=STANY->ZAMK_MIES2:=STANY->ZAMK_MIES1 D_WA D_LIFO}
#else
     EXEC {||STANY->ZAMKN_ROKU:=STANY->ZAMK_MIES2:=STANY->ZAMK_MIES1 D_WA D_LIFO}
#endif
#undef D_LIFO
#undef D_WA
#ifndef STANY
    INDEX ON INDEX+NR_MAG TO stan_ind

  ?
  ? "Tworzenie bazy INDX_MAT i IND_LAM."

    SELECT INDX_MAT
if key#0
    copy to indx_mat FOR STANY->(DBSEEK(INDX_MAT->INDEX))
else
    copy to indx_mat
endif
    SELECT STANY
    USE
    AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.??x"),{|X|FERASE(defa+"tmp"+HB_ps()+X[1])})
    SELECT INDX_MAT
    nuse indx_mat exclusive
#endif
    REPLACE ALL LAMUS WITH 0

    nUSE (defa+"roboczy"+HB_ps()+"ind_lam") READONLY ALIAS YYYY

    COPY STRUCTURE TO ind_lam

    w:=str(year(DatY->d_z_rok),4)
    close databases

    SET DEFAULT TO (defa+"roboczy"+HB_OsPathSeparator())              // roboczy katalog
#ifdef A_DIETA
   nuse indx_mat READONLY EXCLUSIVE alias "YYYY" //żeby nikt się nie pchał do "roboczy"
   if !used()
      break
   endif
#endif
#ifdef A_DDBF
   nuse daty READONLY EXCLUSIVE alias "XXXX" //żeby nikt się nie pchał do "roboczy"
   if !used()
      break
   endif
#endif
    close databases

************
    recover using w

    errorblock(s)
    close databases
    set default to (defa+"roboczy"+HB_OsPathSeparator())

  ?
  ? hb_UTF8ToStr("Błąd ")

  // add subsystem name if available
  if ( ValType(w:subsystem) == "C" )
    ?? w:subsystem
  end


  // add subsystem's error code if available
  if ( ValType(w:subCode) == "N" )
    ?? "/" + hb_ntos(w:subCode)
  end


  // add error description if available
  if ( ValType(w:description) == "C" )
    ?? "  " + w:description
  end


  // add either filename or operation
  if ( !Empty(w:filename) )
    ?? ": " + w:filename

  elseif ( !Empty(w:operation) )
    ?? ": " + w:operation

  end

  if ( !Empty(w:osCode) )
    ? ";(DOS Error " + hb_ntos(w:osCode) + ")"
  end

  ? "Kasowanie kartoteki TMP."
  ?
    AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.*"),{|X|FERASE(defa+"tmp"+HB_ps()+X[1])})
    break

    end sequence
********************
    errorblock(s)

  ?
  ? "Przenoszenie danych z kartoteki ROBOCZY do "+w+"."

   mkdir(defa+w)

                
   AEVAL(DIRECTORY(defa+"roboczy"+HB_ps()+"*.*"),{|X|QOUT(X[1]),frename(defa+"roboczy"+HB_ps()+x[1],defa+w+HB_ps()+X[1])})

  ?
  ? "Przenoszenie danych z kartoteki TMP do ROBOCZY."


   AEVAL(DIRECTORY(defa+"tmp"+HB_ps()+"*.*"),{|X|QOUT(X[1]),frename(defa+"tmp"+HB_ps()+X[1],defa+"roboczy"+HB_ps()+x[1])})

   aadd(year2bckp,val(w))

ENDIF

END
SET PRINT OFF

#ifdef D_HWPRN
  if oprn:=D_HWPRN
     x:=getlines(memoread(set(_SET_PRINTFILE,'')))
     if !empty(x)
       set printer to
       Print(1)
       aeval(x,{|y|wqq(y),wq()})
       specout(chr(12))
       oprn:Destroy()
       oprn:=NIL
     endif
  endif
#endif
#ifdef A_PRINT
  x:=set(_SET_PRINTFILE,'')
  if ! x==set(_SET_PRINTFILE) .and. File(x)
      A_PRINT(x)
  endif
#endif
set default to (defa+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+HB_OsPathSeparator())
readinit()
reuse()

?
tone(262,2)
wait hb_UTF8ToStr("Koniec, naciśnij klawisz.")


RETURN
****************
procedure inwentura


#ifdef A_XPRN
memvar P_36LPI,P_12LPI,P_6LPI,P_8LPI,P_SUPON,P_SUPOFF,P_UON,P_UOFF
#else
#ifdef A_PCL
#define P_36LPI hb_BChar(0x1B)+'&l2C'
#define P_12LPI hb_BChar(0x1B)+'&l4C'
#define P_6LPI  hb_BChar(0x1B)+'&l8C'
#define P_8LPI  hb_BChar(0x1B)+'&l6C'
#define P_SUPON hb_BChar(0x1B)+'(s7V'
#define P_SUPOFF hb_BChar(0x1B)+'(s12V'
#define P_UON   hb_BChar(0x1B)+'&d0D'
#define P_UOFF   hb_BChar(0x1B)+'&d@'
#else
#define P_36LPI hb_BChar(0x1B)+'3'+hb_BChar(0x06)
#define P_12LPI hb_BChar(0x1B)+'3'+hb_BChar(0x12)
#define P_8LPI  hb_BChar(0x1B)+'0'
#define P_6LPI  hb_BChar(0x1B)+'2'
#define P_SUPON hb_BChar(0x1B)+'S1'
#define P_SUPOFF hb_BChar(0x1B)+'T'
#define P_UON   hb_BChar(0x1B)+'-1'
#define P_UOFF   hb_BChar(0x1B)+'-0'
#endif
#endif
MEMVAR GETLIST

LOCAL gal,ile,lp,ark,dop:="",dor:="",sflag,jm_o,mes,wat,x

FIELD NR_MAG,INDEX,STAN,nazwa,JM,jm_opcja,wartosc

select main
  set order to TAG MAIN_IND

GAL:=MAG_BIEZ+space(hb_fieldlen('index'))

@ 9,10 say "Proszę podać numer magazynu i gałąź:" UNICODE
@ 10,30 get gal PICTURE "@RK ##/"+ INDEXPIC
read
if readkey()=27
   return
endif
gal=trim(gal)

select indx_mat
  set order to TAG INDX_NUM

#ifndef STANY
SELECT STANY
  SET ORDER TO TAG STAN_MAG
  SET RELATION TO INDEX INTO INDX_MAT
#endif

setpos(21,0)

if !tak(hb_UTF8ToStr("Czy drukować pozycje o stanie zerowym w/g kartoteki magazynu"))
  set filter to stan#0
endif

  if !dbseek(gal,.f.)
    alarm(hb_UTF8ToStr("Brak takich materiałów"),,,3)
    return
  endif

sflag:=tak(hb_UTF8ToStr("Czy drukować stan ilościowy w/g kartoteki magazynu"))
#ifdef A_JMO
jm_o:=TAK("CZY W ALTERNATYWNEJ JEDNOSTCE MIARY",,,miar_opcja,.F.)
#define D_ILOUT(x) if(jm_o,if(x%indx_mat->przel=0,str(x/indx_mat->przel,6)+"    ",stuff(str(int(x/indx_mat->przel)+x%indx_mat->przel/1000,10,3),7,1,"r")),str(x,10,3))
#define jM (if(jm_o,jm_opcja,jm))
#else
#define D_ILOUT(x) str(x,10,3)
#endif
#ifndef A_WA
#define WartosC stan*indx_mat->cena
#endif
  wat:=ile:=0
  exec {++ile,wat+=WartosC} while NR_MAG+index=gal
#define D_WAOUT(x) strpic(WartosC,10,A_ZAOKR,"@E ")

@ 11,10 say "Ta gałąź liczy "+lTrim(sTr(ILE))+" pozycji, "+lTrim(sTr(INT(ILE/30+.99)))+" arkuszy." UNICODE
seek gal
ark=1
@ 13,30 say "Od arkusza numer:" get ark picture "@K 99" valid ark<1+ile/30 .and. ark>0
read
set cursor off
if readkey()=27
   return
endif
lp=ark*30-29
skip lp-1
  mes:=message(hb_UTF8ToStr("Proszę czekać;TRWA WYDRUK."))
begin sequence
set console off
#ifdef D_HWPRN
oprn:=D_HWPRN
#command ?  [<exp,...>]         => wq()[;?? <exp>]
#command ?? <exp1> [,<expn>]    => wqq(<exp1>)[;wqq(<expn>)]
#else
#command ? [<List,...>] => qout() [;?? <List>]
#command ?? <exp1> [,<expn>]    => qqout(<exp1>)[;qqout(<expn>)]
#endif

print()
do while ile>0
?? "                       SPIS Z NATURY , DNIA",DatE(),"            NR",ARK
? 
? TRIM(magazyny[mag_poz]),trim(ADRES_MAG[mag_poz]),"POLE SPISOWE:",TranR(gal,"XX/"+ INDEXPIC )
? 
#define wqq(x) wQQ(hb_UTF8ToStr(x))
#define qqout(x) QQout(hb_UTF8ToStr(x))

? "SKŁAD OSOBOWY ZESPOŁU SPISUJĄCEGO:     OSOBY ODPOWIEDZIALNE MATERIALNIE:"
?
? "1.                                     1."
? "2.                                     2."
? "3.                                     3."
?
#ifdef A_SHORTIND
? ccpi(7)+"───┬────┬───────────────────────┬────┬──────────┬──────────┬──────────┬─────────────────────┬─────────────────────┬─────────────────────",spec(P_12LPI)
?  "   │    │                       │    │",spec(P_SUPON)+"ILOŚĆ     │STAN      │WARTOŚĆ   │ DATA I NUMER OST.   │RÓŻNICA MIĘDZY STANEM│",spec(P_SUPOFF)
?  "LP. KOD  OKREŚLENIE PRZEDMIOTU   JEDN ",spec(P_SUPON)+"STWIERDZ.  WEDŁUG     WEDŁUG      DOK. MAGAZYNOWEGO    FAKT. I KSIĘGOWYM    ",spec(P_SUPOFF)+" UWAGI ZESPOŁU"
?  "   │    │                       │    │",spec(P_SUPON)+"W DNIU    ",spec(P_SUPOFF)+"│",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"│",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"├──────────┬──────────┼──────────┬──────────┤"
?  "         NAZWA                        ",spec(P_SUPON)+"SPISU      MAGAZYNU   MAGAZYNU    PRZYCHÓD   ROZCHÓD   NIEDOBORY   NADWYŻKI ",spec(P_SUPOFF)+" SPISUJĄCEGO"
?  "───┴────┴───────────────────────┴────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴─────────────────────",spec(P_8LPI)
#undef qqout
#undef wqq
do while lp<=min(ark*30,ile)
? str(lp,3)+"|"+INDEX+"|"+LEFT(INDX_MAT->NAZWA,23)+"|"+INDX_MAT->jM+"|          |"+if(sflag,D_ILOUT(stan)+"|"+D_WAOUT(stan),"          |          ")+"|"+ostp(@DOP)+"|"+ostr(@DOR)+"|          |          |"
? "   |    |"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |          |          |"+dop+"|"+dor+"|          |          |"
#else
#ifdef A_KTM
? ccpi(7)+"───┬────────────────┬───────────────────────┬────┬──────────┬──────────┬─────────────────────┬─────────────────────┬────────────────────",spec(P_12LPI)
?  "   │                │                       │    │",spec(P_SUPON)+"ILOŚĆ     │STAN/wart. │ DATA I NUMER OST.  │RÓŻNICA MIĘDZY STANEM│",spec(P_SUPOFF)
?  "LP. KOD MATERIAŁU    OKREŚLENIE PRZEDMIOTU   JEDN ",spec(P_SUPON)+"STWIERDZ.  WEDŁUG       DOK. MAGAZYNOWEGO   FAKT. I KSIĘGOWYM   ",spec(P_SUPOFF)+"  UWAGI ZESPOŁU"
?  "   │                │                       │    │",spec(P_SUPON)+"W DNIU    ",spec(P_SUPOFF)+"│",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"├──────────┬──────────┼──────────┬──────────┤"
?  "                     NAZWA                        ",spec(P_SUPON)+"SPISU      MAGAZYNU    PRZYCHÓD   ROZCHÓD   NIEDOBORY   NADWYŻKI",spec(P_SUPOFF)+"  SPISUJĄCEGO"
?  "───┴────────────────┴───────────────────────┴────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴────────────────────",spec(P_8LPI)
#undef qqout
#undef wqq
do while lp<=min(ark*30,ile)
? str(lp,3)+"|"+tran(INDEX,"@R "+ INDEXPIC )+"|"+LEFT(INDX_MAT->NAZWA,23)+"|"+INDX_MAT->jM+"|          |"+if(sflag,D_ILOUT(stan),space(10))+"|"+ostp(@DOP)+"|"+ostr(@DOR)+"|          |          |"
? "   |"
?? space(16)
?? "|"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |"+if(sflag,D_WAOUT(stan),space(10))+"|"+dop+"|"+dor+"|          |          |"
#else
#ifdef A_OLZA
? ccpi(7)+"───┬──────────────┬───────────────────────┬────┬──────────┬──────────┬─────────────────────┬─────────────────────┬──────────────────────",spec(P_12LPI)
?  "   │              │                       │    │",spec(P_SUPON)+"ILOŚĆ     │STAN/wart. │ DATA I NUMER OST.  │RÓŻNICA MIĘDZY STANEM│",spec(P_SUPOFF)
?  "LP. KOD MATERIAŁU  OKREŚLENIE PRZEDMIOTU   JEDN ",spec(P_SUPON)+"STWIERDZ.  WEDŁUG       DOK. MAGAZYNOWEGO   FAKT. I KSIĘGOWYM   ",spec(P_SUPOFF)+"  UWAGI ZESPOŁU"
?  "   │              │                       │    │",spec(P_SUPON)+"W DNIU    ",spec(P_SUPOFF)+"│",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"├──────────┬──────────┼──────────┬──────────┤"
?  "    INDEKS SWW     NAZWA                        ",spec(P_SUPON)+"SPISU      MAGAZYNU    PRZYCHÓD   ROZCHÓD   NIEDOBORY   NADWYŻKI",spec(P_SUPOFF)+"  SPISUJĄCEGO"
?  "───┴──────────────┴───────────────────────┴────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────────────────",spec(P_8LPI)
#undef qqout
#undef wqq
do while lp<=min(ark*30,ile)
? str(lp,3)+"|"+tran(INDEX,"@R "+ INDEXPIC )+"|"+LEFT(INDX_MAT->NAZWA,23)+"|"+INDX_MAT->jM+"|          |"+if(sflag,D_ILOUT(stan),space(10))+"|"+ostp(@DOP)+"|"+ostr(@DOR)+"|          |          |"
? "   |              |"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |"+if(sflag,D_WAOUT(stan),space(10))+"|"+dop+"|"+dor+"|          |          |"
#else
#ifdef A_SWW
? ccpi(7)+;
   "----------------------------------------------------------------------------------------------------------------------------------------",spec(P_12LPI)
?  "                                  |    |",spec(P_SUPON)+"ILOŚĆ     ",spec(P_SUPOFF)+"|",spec(P_SUPON)+"STAN/wart.",spec(P_SUPOFF)+"|"+P_SUPON+"  DATA I NUMER OST.  "+spec(P_SUPOFF)+"|"+spec(P_SUPON)+"RÓŻNICA MIĘDZY STANEM"+spec(P_SUPOFF)+"|"
?  "LP. SYMBOL OKREŚLENIE PRZEDMIOTU   JEDN ",spec(P_SUPON)+"STWIERDZ.  WEDŁUG       DOK. MAGAZYNOWEGO   FAKT. I KSIĘGOWYM   ",spec(P_SUPOFF)+"  UWAGI ZESPOŁU"
?  "   |      |                       |    |",spec(P_SUPON)+"W DNIU    ",spec(P_SUPOFF)+"|",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"|----------+----------|----------+----------|"
?  "      SWW  NAZWA                        ",spec(P_SUPON)+"SPISU      MAGAZYNU    PRZYCHÓD   ROZCHÓD   NIEDOBORY   NADWYŻKI",spec(P_SUPOFF)+"  SPISUJĄCEGO"
?  "___|______|_______________________|____|__________|__________|__________|__________|__________|__________|______________________________",spec(P_8LPI)
#undef qqout
#undef wqq
do while lp<=min(ark*30,ile)
? str(lp,3)+"|"+tran(INDEX,"@R "+ INDEXPIC )+"|"+LEFT(INDX_MAT->NAZWA,23)+"|"+INDX_MAT->jM+"|          |"+if(sflag,D_ILOUT(stan),space(10))+"|"+ostp(@DOP)+"|"+ostr(@DOR)+"|          |          |"
? padl(trim(indx_mat->sww),10)+"|"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |"+if(sflag,D_WAOUT(stan),space(10))+"|"+dop+"|"+dor+"|          |          |"
#else
? ccpi(7)+"───┬────────────┬───────────────────────┬────┬──────────┬──────────┬─────────────────────┬─────────────────────┬────────────────────────",spec(P_12LPI)
?  "   │            │                       │    │",spec(P_SUPON)+"ILOŚĆ     │STAN/wart. │ DATA I NUMER OST.  │ROZNICA MIĘDZY STANEM│",spec(P_SUPOFF)
?  "LP. KOD MATER.   OKREŚLENIE PRZEDMIOTU   JEDN ",spec(P_SUPON)+"STWIERDZ.  WEDŁUG       DOK. MAGAZYNOWEGO   FAKT. I KSIĘGOWYM   ",spec(P_SUPOFF)+"  UWAGI ZESPOŁU"
?  "   │            │                       │    │",spec(P_SUPON)+"W DNIU    ",spec(P_SUPOFF)+"│",spec(P_SUPON)+"KARTOTEKI ",spec(P_SUPOFF)+"├──────────┬──────────┼──────────┬──────────┤"
?  "    INDEKS SWW   NAZWA                        ",spec(P_SUPON)+"SPISU      MAGAZYNU    PRZYCHÓD   ROZCHÓD   NIEDOBORY   NADWYŻKI",spec(P_SUPOFF)+"  SPISUJĄCEGO"
?  "───┴────────────┴───────────────────────┴────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴────────────────────────",spec(P_8LPI)
#undef qqout
#undef wqq
#ifndef A_INOWA
?? spec(P_SUPON)
#endif
do while lp<=min(ark*30,ile)
? str(lp,3)+"|"+tran(INDEX,"@R "+ INDEXPIC )+"|"+LEFT(INDX_MAT->NAZWA,23)+"|"+INDX_MAT->jM+"|          |"+if(sflag,D_ILOUT(stan),space(10))+"|"+ostp(@DOP)+"|"+ostr(@DOR)+"|          |          |"
#ifdef A_INOWA
? "   |            |"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |"+space(10)+"|"+dop+"|"+dor+"|          |          |"
#else
? "   |            |"+pad(substr(INDX_MAT->NAZWA,24),23)+"|    |          |"+if(sflag,D_WAOUT(stan),space(10))+"|"+dop+"|"+dor+"|          |          |"
#endif
#endif
#endif
#endif
#endif
?? ccpi(4)+spec(chr(13)+replicate('_',80)),ccpi(7)
skip
++lp
message(1)
enddo
? spec(P_SUPOFF)
if sflag .and. lp>ile
?? space(57),strpic(wat,12,A_ZAOKR,"@E ",.f.)
endif
? hb_UTF8ToStr("Podpis osoby odpowiedzialnej materialnie                                Podpisy zespołu spisującego")
?? spec(P_6LPI+ccpi(4)+chr(13)+chr(10))
if lp>ile
   break
endif
ark=ark+1
enddo
end
set print off
#ifdef D_HWPRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
#ifdef A_PRINT
  x:=set(_SET_PRINTFILE,'')
  if ! x==set(_SET_PRINTFILE) .and. File(x)
     A_PRINT(x)
  endif
#endif
set console on
message(mes)
return
*****************************
function ostp(DOP)

field index,smb_dow,nr_dowodu,nr_mag,pozycja

select main
seek STANY->NR_MAG+STANY->index LAST
do while STANY->NR_MAG+STANY->index = NR_MAG+index .AND..NOT. BOF()
   if !nr_mag+smb_dow$dok_rozch
      dop:=smb_dow+nr_dowodu+"/"+str(D_LPVALFIELD(POZYCJA),2)
      select STANY      
      return(dtoc(MAIN->data))
   endif   
   skip -1
enddo  
select STANY
dop:="          "
return(dop)   
*****************************
function ostr(DOR)

field index,smb_dow,nr_dowodu,nr_mag,pozycja

select main

seek STANY->NR_MAG+STANY->index LAST
do while STANY->NR_MAG+STANY->index = NR_MAG+index .and..not. bof()
   if nr_mag+smb_dow$dok_rozch
      dor:=smb_dow+nr_dowodu+"/"+str(D_LPVALFIELD(POZYCJA),2)
      select STANY
      return(dtoc(MAIN->data))
   endif   
   skip -1
enddo        
select STANY
dor:="          "
return(dor)   
***************
#ifndef hAslo_spec
proc haslo_spec(p)
memvar operator,mag_biez
field haslo_spec,magazyn,magazynier
    local m,txt
      @ p,0
      @ p,0 SAY "Podaj hasło:" UNICODE
       txt:=""
    set cursor on
  do while 0<(m:=INkey(0)) .and. m#13
    if m=28
      help("haslo_spec")
      loop
    elseif m=8
      txt:=left(txt,len(txt)-1)
      @ p,col()-1 say " "
      setpos(p,col()-1)
      loop
    endif
    txt+=hb_keyChar(m)
    ?? iif(hb_cdpIsUTF8(),"♦",chr(4))
  enddo
    set cursor off
   select 0

#ifndef A_DECRYPT
#define A_DECRYPT(x) l2bin(x)
#define A_ENCRYPT(x) bin2l(x)
#endif

   XSELECT OBSLUGA READONLY
       LOCATE FOR EvaldB({|txt,operator|mag_biez==magazyn .and. Trim(operator)==Trim(magazynier) .and. lower(txt)==TRIM(A_DECRYPT(haslo_spec))},txt,operator)
       IF EOF()
          ? hb_UTF8ToStr("złe hasło ...")
      use
      inkey(2)
      break
    endif
    use
return
#endif
****************
