field   grupa,posilek,dieta,opis,data,path,nazwa,cena

MEMVAR  grupy,narzuty,diety,posilki,posgr,grstr,dietystr,posstr,firma_n,_snorm,level1,stary_rok,defa,cennik,dzisiaj,dietylong,_sbkgr,_sbnorm
#ifdef A_XPRN
memvar apcomp,p_pcl,landscape
#endif
#ifdef A_WIN_PRN
memvar oprn
#endif
#include "inkey.ch"
#include   "hbgtinfo.ch"
#ifdef A_ELZ
request descend
#endif
#ifdef A_EXT
request A_EXT
#endif
PROCEDURE MAIN(parametr)

local scr_menu,menu,i,txt,a

   hb_gtInfo( HB_GTI_WINTITLE , "Dieta" )
#ifdef A_WIN_PRN
   public oprn
#endif
public defa

if parametr='DIETADEF='
   defa:=SubStr(parametr,10)
   parametr:=NIL
else
   defa:=getenv("DIETADEF")
endif

defa:=getlines(defa,HB_OsPathListSeparator())

if len(defa)=1 .and. empty(defa[1])
   defa[1]:='.'
endif
aeval(defa,{|x,i,y|y:=right(x,1),if(y==HB_OsDriveSeparator(),x+=HB_ps()+curdir(x),),if(y<>HB_ps(),defa[i]:=x+HB_ps(),)})

a:=HB_ps()+curdir()+HB_ps()

#ifndef __PLATFORM__UNIX
  if a<>HB_ps()+HB_ps()
   a:=DiskName()+HB_OsDriveSeparator()+a
  endif
#endif


aeval(defa,{|x,j,i|defa[j]:=if(x='.'+HB_ps(),a+SubStr(x,3),if(x='..'+HB_ps().and.(i:=rat(HB_ps(),left(a,len(a)-1)))>0,left(a,i)+SubStr(x,4),x))})

menu:=.f.
aeval(defa,{|x,j,i|if(lower(a)=lower(x),(menu:=.t.,defa[j]:=if((i:=len(x))=0,a,left(a,i))),)})

if menu
  a:=defa[1]
  i:=2
else
  i:=1
end if

aeval(defa,{|x|a+=HB_OsPathListSeparator()+x},i)
SET PATH TO (a)

defa:=atail(defa)

SETPOS(3,0)

? padc(hb_UTF8ToStr(" ██████ █     █  ██████ ███████ ███████ █       █"),maxcol()-1)
? padc(hb_UTF8ToStr("█        █   █  █          █    █       ██     ██"),maxcol()-1)
? padc(hb_UTF8ToStr(" █████    █ █    █████     █    ██████  █ █   █ █"),maxcol()-1)
? padc(hb_UTF8ToStr("      █    █          █    █    █       █  █ █  █"),maxcol()-1)
? padc(hb_UTF8ToStr("██████     █    ██████     █    ███████ █   █   █"),maxcol()-1)
? 
? padc(hb_UTF8ToStr("█████▄   █  ███████ ███████   █    "),maxcol())
? padc(hb_UTF8ToStr("█    ▀█  █  █          █     █ █   "),maxcol())
? padc(hb_UTF8ToStr("█     █  █  ██████     █    █▄▄▄█  "),maxcol())
? padc(hb_UTF8ToStr("█    ▄█  █  █          █   █▀▀▀▀▀█ "),maxcol())
? padc(hb_UTF8ToStr("█████▀   █  ███████    █  █       █"),maxcol())

IF DISKSPACE(if(":"$defa,asc(upper(defa))-64,))<500000
  alarm("TYLKO"+hb_ntos(DISKSPACE())+hb_UTF8ToStr(" BAJTÓW WOLNEGO MIEJSCA NA DYSKU !"),,3,3)
ENDIF

set default to (defa+"roboczy"+HB_ps())

   STARY_ROK=NIL

#ifdef A_XPRN
   public landscape:=.f.
#ifdef A_PCL
   public p_pcl:=.t.
//#ifdef A_DRUKCOMP
//#endif
#else
   public p_pcl:=.f.
#endif
#endif

i:={maxrow(),maxcol(),}

   txt:="dieta.ini"
   do while inirest(@txt)
   (&txt,txt:=NIL)
   enddo
#ifdef A_XPRN
   p_reset()
   txt:="xprn.ini";do while inirest(@txt);(&txt,txt:=NIL);enddo
#endif

   txt:=set(_SET_DEFAULT)+"daty.ini"
   do while inirest(@txt)
     (&txt,txt:=NIL)
   enddo


   if maxrow()>i[2] .or. maxcol()>i[2]
     i[1]:=min(i[1],maxrow())
     i[2]:=min(i[2],maxcol())
     i[3]:=savescreen(0,0,i[1],i[2])
     clear screen
     restscreen(0,0,i[1],i[2],i[3])
   endif

   readinit()
   reuse()


*******************                    inicjacja zmiennych

#ifdef DatE
private dzisiaj:=date()
#endif

if !empty(parametr)
    set cursor on
    set color to w
    (&parametr,parametr:=NIL)
    quit
endif

#ifdef DatE
@ 18,0
SAYL "Data dzisiaj: "
readmodal( {_GET_( dzisiaj, "dzisiaj",,,)} )
#endif
//#ifdef A_ELZ
//setkey(-1,{|x,y|y:=setkey(-1,),x:=push_stat(),surowce(),pop_stat(x),setkey(-1,y)})
//#else
setkey(K_F2,{||magazyn()})
//#endif
setkey(K_F3,{||relewy->(jad_in(.t.))})
setkey(K_F4,{|x|x:=setkey(K_F4,),dania->(rec_in(.t.,.f.)),setkey(K_F4,x)})
setkey(K_F5,{|x|x:=setkey(K_F5,),surowce->(sur_in(.t.,.f.)),setkey(K_F5,x)})
setkey(K_F6,{|x|x:=setkey(K_F6,),aczojs(zaw_ar({},,dania->danie),"",1,,hb_UTF8ToStr("Zawartość w: ")+dania->nazwa),setkey(K_F6,x)})
setkey(K_F7,{|x|x:=setkey(K_F7,),aczojs(zaw_ar({},,,relewy->(dtos(data)+posilek+dieta)),"",1,,hb_UTF8ToStr("Zawartość w posiłku ")+relewy->(dtoc(data)+"/"+posilek+trim(dieta))),setkey(K_F7,x)})

DO WHILE .T.
*************************************
//a := hb_gtInfo( HB_GTI_BOXCP, 'UTF8EX')


if isCOLOR()
    SET COLOR TO BG+  // TWORZENIE EKRANU
else
    set color to w
endif
          CLEAR screen
      ?? padc(firma_n,maxcol())
      ?
      ? padc("SYSTEM DIETA",maxcol())

if iscolor()
      SET COLOR TO (_sbkgr)
endif

          @ 4,maxcol()/2-28,7,maxcol()/2+28 BOX UNICODE "╔═╗║╝═╚║ "

          @ 4,maxcol()/2-4 SAY " M E N U " COLOR if(isCOLOR(),_sbkgr,"W+")

          @ maxrow()-2,0,maxrow(),maxcol() BOX UNICODE "╔═╗║╝═╚║ "

  IF STARY_ROK#NIL
    @ maxrow()-1,9 say stary_rok+' !' color if(isCOLOR(),"*"+_sbkgr,"W*+")
  else
    @ maxrow()-1,9 say "WYBÓR OPCJI" UNICODE color if(isCOLOR(),_sbnorm,"W+")
  ENDIF   

          @ maxrow()-1,25 SAY "POMOC  f2 - f7 BEZPŚREDNI DOSTĘP DO DANYCH esc WYJŚCIE" UNICODE color if(isCOLOR(),_sbnorm,"W+")

      SET COLOR TO I
          @ maxrow()-1,2 BOX '►' UNICODE
          @ maxrow()-1,4 BOX '◄' UNICODE
          @ maxrow()-1,6 BOX '◄┘' UNICODE
          @ maxrow()-1,22 SAY "F1"
          @ maxrow()-1,32 SAY "F2"
          @ maxrow()-1,37 SAY "F7"
          @ maxrow()-1,68 say "Esc"
//      hb_gtInfo( HB_GTI_BOXCP, a)

      SET MESSAGE TO 5 CENTER

if isCOLOR()
    SET COLOR TO (_sbnorm)
ELSE
    SET COLOR TO W
endif

      @ 6,maxcol()/2-26 PROMPT "Osoby" UNICODE MESSAGE "Wyświetlanie kartotek osobowych."
      @ 6,maxcol()/2-20 PROMPT "Posiłki" UNICODE MESSAGE "Posiłki według dat."
      @ 6,maxcol()/2-12 PROMPT "Dania" UNICODE MESSAGE '"Kartoteka" dań.'
      @ 6,maxcol()/2-6  PROMPT "Surowce" UNICODE MESSAGE "Składniki dań - surowce."
      @ 6,maxcol()/2+2  PROMPT "Elementy" UNICODE MESSAGE "Makro i mikroelementy - składniki surowców."
      @ 6,maxcol()/2+11 PROMPT "Zestawienia" UNICODE message "Wydruk zestawień."
      @ 6,maxcol()/2+23 PROMPT "Inne" UNICODE message "Programy specjalne, import kosztów."

      menu:=level1

      MENU TO menu

      set color to (_snorm)

      IF menu=0

         inisave(set(_SET_DEFAULT)+"daty.ini")
         inisave("dieta.ini")

         IF tak(hb_UTF8ToStr("Czy rzeczywiście kończysz pracę"),18,,.t.,.F.)
            EXIT
         ENDIF

         loop
      ENDIF

      level1:=menu
      setpos(maxrow()/2,maxcol()/2)
begin sequence
      DO CASE

    CASE level1 = 1
      DO katalog

    CASE level1 = 2
      DO rel_jad

    case level1 =3
      do dan_kat

    case level1 =4
      do surowce

    case level1 =5
      do elementy

    case level1=6
      do zestawienia

    CASE level1 =7
      DO pomocnicze

      ENDCASE
end
#ifdef A_PRINT
    txt:=set(_SET_PRINTFILE,'')
    if ! txt == set(_SET_PRINTFILE) .and. File(txt)
        A_PRINT(txt)
     endif
#endif
    SET PRINTER TO
#ifdef A_WIN_PRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
set console on
set device to screen
      ******************** RESET BAZ
      FOR i = 1 TO 10
          SELECT (i)
          SET FILTER TO
          SET RELATION TO
      NEXT

      UNLOCK ALL

   ENDDO

********************************
IF tak(hb_UTF8ToStr("CZY ARCHIWOWAĆ"),18,,.t.,.F.)
#ifdef A_BACKUP
    CLOSE ALL
    hb_idlesleep(0.2)
    __RUN( A_BACKUP )
#else
    ERRORLEVEL(41)
#endif
ENDIF

@ 4,0 clear

SET CURSOR ON

return
*****************
proc readinit
public grupy:={},posilki:={},PosGr:='',DIETY:={},PosStr:="",DietyStr:="",GrStr:=""
#ifdef A_XPRN
public apcomp:={}
#endif
#ifdef A_ELZ
#undef A_CENNIK
#endif
#ifdef A_CENNIK
public cennik:={}
#define D_CENNIK ,aadd(cennik,cena)
#else
#define D_CENNIK
#endif
#ifdef A_WAGI
#define D_WAGI ,PosGr+=Grupa
#else
#define D_WAGI
//,PosGr+=Posilek
#endif
sel('grupy',,,.t.)
#ifdef A_NARZUT
PUBLIC narzuty:={}
dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa, aadd(narzuty,A_NARZUT)},,{||grupa#' '},,,.f.)
#else
dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa },,{||grupa#' '},,,.f.)
#endif
use
sel('posilki',,,.t.)
dbeval({||aadd(posilki,posilek+" "+opis), PosStr+=posilek D_WAGI D_CENNIK},,{||posilek#' '},,,.f.)
use
sel('diety',,,.t.)
#if 1
//def A_GOCZ
public dietylong:={}
#define D_GOCZ , aadd(dietylong,trim(nazwa))
#else
#define D_GOCZ
#endif
dbeval({||aadd(diety,dieta+" "+opis), dietyStr+=dieta D_GOCZ},,{||dieta#' '},,,.f.)
use
return
*********************************
proc reuse()
close databases
set default to (defa+if(stary_rok#NIL,stary_rok,"roboczy")+HB_ps())
      select 0
      SEL("relewy",1)
      SEL("osoby",2)
      SEL("main",2)
      SEL("dania",2)
      SEL("menu",2)
#ifdef A_DIETA
#define D_DIETA ,.t.
#else
#define D_DIETA
#endif

      SEL("surowce",2 D_DIETA)
#ifndef A_LAN
#ifdef A_DIETA
      flock()
#endif
#endif
      SEL("ZAPOT",2 D_DIETA)
#ifndef A_LAN
#ifdef A_DIETA
      flock()
#endif
#endif
      SEL("sklad",2)
      SEL("elementy",2)
      sel("zawar",2)
#ifdef A_NORMY
      sel("normy",1)
#endif
#ifdef A_ELZ
      sel("cennik",1)
#endif
#ifdef A_CENPOS
      sel("cenpos",1)
EXTERNAL DESCEND
#endif
return
*****************
func wersja()
#ifdef __DATE__
return 'Wersja: 3.'+lTrim(sTr(stod(__DATE__)-stod('20250101')))
#else
return 'Wersja: 2.2'
#endif
*************
