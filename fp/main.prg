#ifndef A_LAN
#define DatY MEMVAR
#endif
#include "ar.ch"
#include "hbgtinfo.ch"
EXTERNAL __DBSDF,__DBDELIM,__COPYFILE
EXTERNAL ACOPY
EXTERNAL ASORT
EXTERNAL ACLONE
EXTERNAL BIN2I
EXTERNAL BIN2L
EXTERNAL BIN2W
EXTERNAL BIN2D
EXTERNAL D2BIN
EXTERNAL FCLOSE
EXTERNAL FCREATE
EXTERNAL FERASE
EXTERNAL FERROR
EXTERNAL FOPEN
EXTERNAL FREAD
EXTERNAL FREADSTR
EXTERNAL FRENAME
EXTERNAL FSEEK
EXTERNAL FWRITE
EXTERNAL GETENV
EXTERNAL I2BIN
EXTERNAL L2BIN
EXTERNAL __RUN
EXTERNAL STOD
memvar it_level1,defa,stary_rok


PROCEDURE MAIN(parametr,menu)

local i,txt,a

field nazwisko,haslo,haslo_spec

MEMVAR  operator,_snorm,firma_n,firma_a,is_spec,dzisiaj,oprn
#ifdef A_XPRN
memvar  landscape,p_rown,p_cpi,p_pcl,P_4XON,P_4XOFF,P_COLN,P_BON,P_BOFF,P_UON,P_UOFF,P_36LPI,P_12LPI,P_8LPI,P_6LPI,P_SUPON,P_SUPOFF,p_margin,P_PON,P_POFF,P_HALFPAGE,p_init
#endif

memvar  rejestry

#ifdef A_PENS
   hb_gtInfo( HB_GTI_WINTITLE , "Pens" )
#else
   hb_gtInfo( HB_GTI_WINTITLE , "Fi-Ks" )
#endif

setpos(3,0)
? padc(hb_UTF8ToStr(" ██████ █     █  ██████ ███████ ███████ █       █"),maxcol()-1)
? padc(hb_UTF8ToStr("█        █   █  █          █    █       ██     ██"),maxcol()-1)
? padc(hb_UTF8ToStr(" █████    █ █    █████     █    ██████  █ █   █ █"),maxcol()-1)
? padc(hb_UTF8ToStr("      █    █          █    █    █       █  █ █  █"),maxcol()-1)
? padc(hb_UTF8ToStr("██████     █    ██████     █    ███████ █   █   █"),maxcol()-1)
?
#ifdef A_PENS
?      padc(hb_UTF8ToStr("██████   ███████  █      █   ██████"),maxcol())
?      padc(hb_UTF8ToStr("█     █  █        █▀▄    █  █      "),maxcol())
?      padc(hb_UTF8ToStr("██████   ██████   █  ▀▄  █   █████ "),maxcol())
?      padc(hb_UTF8ToStr("█        █        █    ▀▄█        █"),maxcol())
?      padc(hb_UTF8ToStr("█        ███████  █      █  ██████ "),maxcol())
#else
?      padc(hb_UTF8ToStr("███████ █          █    ▄▀  ██████       "),maxcol())
?      padc(hb_UTF8ToStr("█       █          █  ▄▀   █             "),maxcol())
?      padc(hb_UTF8ToStr("██████  █  ▄▄▄▄▄▄  █▄█      █████        "),maxcol())
?      padc(hb_UTF8ToStr("█       █          █  ▀▄         █       "),maxcol())
?      padc(hb_UTF8ToStr("█       █ NANSOWO  █    ▀▄ ██████  IĘGOWY"),maxcol())
#endif

setpos(4,maxcol()/2)
             
public defa

if parametr='FKDEF='
   defa:=SubStr(parametr,7)
   parametr:=menu
   HB_SETENV("FKDEF",defa)
else
   defa:=getenv("FKDEF")
endif

defa:=getlines(defa,HB_OsPathListSeparator())
if empty(defa)
   defa:={''}
endif
#ifdef __PLATFORM__UNIX
aeval(defa,{|x,i|if(x=HB_ps(),,defa[i]:=HB_ps()+curdir()+HB_ps()+x)})
#else
aeval(defa,{|x,i,y|if(SubStr(x,2,1)==HB_OsDriveSeparator(),(y:=left(x,2),x:=SubStr(x,3)),;
                y:=if(x=HB_ps()+HB_ps(),'',DiskName()+HB_OsDriveSeparator())),;
                  if(x=HB_ps(),,y+=HB_ps()+curdir(y)+HB_ps()),defa[i]:=y+x})
#endif

aeval(defa,{|x,i,y|y:=Right(x,1),if(y==HB_ps(),,x+=HB_ps()),defa[i]:=hb_PathNormalize(x)})

a:=HB_ps()+curdir()+HB_ps()

#ifndef __PLATFORM__UNIX  
  if curdir()<>HB_ps()
   a:=DiskName()+HB_OsDriveSeparator()+a
  endif
#endif

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

if parametr='MAGDEF='
   txt:=SubStr(parametr,8)
   parametr:=NIL
   HB_SETENV("MAGDEF",txt)
else
   txt:=getenv("MAGDEF")
endif
if empty(txt) .and. 0<>(i:=rat('fk',lower(defa)))
   txt:=Stuff(defa,i,2,'zbyt')
#ifdef __PLATFORM__UNIX
   if SubStr(defa,i,1)=='F'; txt:=Stuff(txt,i,1,'Z');endif
   if SubStr(defa,++i,1)=='K'
      txt:=Stuff(txt,i,3,'byt')
   endif
#endif
   if hb_dirExists(txt)
      HB_SETENV("MAGDEF",txt)
   endif
endif

STARY_ROK=NIL

#ifdef A_WIN_PRN
   public oprn:=NIL
#endif

#ifdef A_XPRN
   public landscape:=.f.
#ifdef A_PCL
   public p_pcl:=.t.
#else
   public p_pcl:=.f.
#endif
#endif

set default to (defa+"roboczy"+HB_ps())
i:={MaxRow(),maxcol()}

#ifdef DatE
public dzisiaj:=date()
#endif

txt:="fk.ini"
do while inirest(@txt)
   begin sequence
     (&txt,txt:=NIL)
   end sequence
enddo

txt:=savescreen(0,0,i[1],i[2])
clear screen
menu:=max(0,maxcol()-i[2])/2
restscreen(0,menu,i[1],menu+i[2],txt)

public rejestry
readinit()
reuse()
#ifndef A_LAN
   txt:="daty.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end sequence
   enddo
#endif

#ifdef A_XPRN
   p_reset()
   txt:="xprn.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end sequence
   enddo
#endif


txt:={}
IF DISKSPACE(if(":"$defa,asc(upper(defa))-64,))<1000000
   aadd(txt,message("TYLKO "+lTrim(sTr(DISKSPACE(if(":"$defa,asc(upper(defa))-64,))))+hb_UTF8ToStr(" BAJTÓW;WOLNEGO MIEJSCA NA DYSKU !")))
ENDIF

IF DATE()-45>DatY->D_Z_MIES1
   aadd(txt,message(hb_UTF8ToStr("ZAMKNIJ MIESIĄC !")))
ENDIF

if !empty(txt)
   afill(txt[1],0,1,4)
   message(txt[1])
   setpos(i,0)
endif

@ 18,0
if !empty(parametr)
    set color to w
    operator:=''
    (&parametr,parametr:=NIL)
    set cursor on
    quit
endif

#ifdef DatE
@ 17,0 SAY "Data dzisiaj: "
__SetProc('dzisiaj')
readmodal({_GET_(dzisiaj,'dzisiaj',,,)})
#endif

DO WHILE .T.

   SET CURSOR ON
   @ 18,0
   @ 18,0 SAY "Podaj hasło:" UNICODE
  txt:=""
  do while 0<(i:=inkey(0)) .and. i#13
    if i=28
      help("haslo")
      loop
    elseif i=8 .and. ""#txt
      txt:=left(txt,len(txt)-1)
      @ row(),col()-1 say " "
      setpos(row(),col()-1)
      loop
    endif
    txt+=hb_keyChar(i)
    ?? hb_UTF8ToStr("░")
  enddo

   nUSE obsluga READONLY  NEW
#ifdef A_SX
       LOCATE FOR txt==trim(haslo)
#else
       LOCATE FOR padr(UpP(txt),4)==l2bin(haslo)
#endif
       IF EOF()
          ? hb_UTF8ToStr("złe hasło ...")
          set color to w
          @ 20,0 clear
          quit                        // gdy nie znalazl
       ENDIF
       operator=nazwisko
#ifndef hAslo_spec
       is_spec:=!Empty(haslo_spec)
#endif
   SET CURSOR OFF

   use

   DO WHILE .T. // PETLA GLOWNA
  SET CURSOR OFF

*************************************

if iscolor()
  SET COLOR TO BG+  // TWORZENIE EKRANU
else
  set color to w
endif
   //dispbegin()
   CLEAR screen
      ?? padc("FIKS dla: "+firma_n+"; "+TRIM(operator),maxcol())
if iscolor()
      SET COLOR TO BG+/br
      @ 1,0 clear to 2,maxcol()
      @ MaxRow(),0
endif

      @ MaxRow(),1 SAY "                       -wybór opcji    -                  -wyjście" UNICODE color if(iscolor(),"W+/br","W+")

      SET COLOR TO I
          @ MaxRow(),2 SAY "K"
                      SAYl "D"
                      SAYl "R"
#ifdef A_DPS
                      SAYl "N"
#else
                      SAYl "F"
#endif
                      SAYl "Z"
                      SAYl "P"
                      SAYl "─>" UNICODE
          SAYl "<─" UNICODE
          SAYl "<─┘" UNICODE
          @ MaxRow(),37 SAY "F1"
          @ MaxRow(),42 SAY "P O M O C" color "W+/br"
          @ MaxRow(),55 say "Esc"

  IF STARY_ROK#NIL
     @ MaxRow(),72 say str(year(stary_rok),4)+' !' COLOR if(iscolor(),"*+GR/br","W*+")
  else
     @ MaxRow(),72 say "      " COLOR if(iscolor(),"N/br","N")
  ENDIF

      SET MESSAGE TO 1 CENTER

if iscolor()
    SET COLOR TO W+/br
ELSE
    SET COLOR TO W
endif
#ifdef A_DPS
      @ 2,1        PROMPT "Konta"        MESSAGE "Kartoteka Kont Analitycznych i Syntetycznych, Salda i Obroty."
      @ 2,col()+1  PROMPT "Dziennik"     MESSAGE "Dziennik."
      @ 2,col()+1  PROMPT "Rejestry"    UNICODE MESSAGE "Rejestry, rejestracja dokumentów źródłowych."
      @ 2,col()+1  PROMPT "Nazwiska"     MESSAGE "Kartoteka Pensjonariuszy."
      @ 2,col()+1  PROMPT "Zestawienia" UNICODE MESSAGE 'Wydruki zestawień.'
      @ 2,col()+1  PROMPT "Pozostałe"   UNICODE MESSAGE 'Dodatkowe funkcje programu.'
#else
      @ 2,1        PROMPT "Konta"        MESSAGE "Kartoteka Kont Analitycznych i Syntetycznych, Salda i Obroty."
      @ 2,col()+1  PROMPT "Dziennik"     MESSAGE "Dziennik."
      @ 2,col()+1  PROMPT "Rejestry"    UNICODE MESSAGE "Rejestry, rejestracja dokumentów źródłowych."
#ifdef A_PENS
      @ 2,col()+1  PROMPT A_PENS[2]     UNICODE MESSAGE "Kartoteka "+A_PENS[3]
#else
      @ 2,col()+1  PROMPT "Firmy"       UNICODE MESSAGE "Kartoteka Kontrahentów."
#endif
      @ 2,col()+1  PROMPT "Zestawienia" UNICODE MESSAGE 'Wydruki zestawień.'
      @ 2,col()+1  PROMPT "Pozostałe"   UNICODE MESSAGE 'Dodatkowe funkcje programu.'
#endif
      //dispend()
      menu:=it_level1
      MENU TO menu

      set color to (_snorm)

      @ MaxRow(),0

      IF menu=0
         menu:=tak(hb_UTF8ToStr("Czy rzeczywiscie kończysz pracę"),18,,.t.,0)
         if valtype(menu)="L"
           EXIT
         ENDIF
      ENDIF

      SETPOS(3,10*menu)

      it_level1:=menu
  begin sequence
      DO CASE

         CASE menu = 1
              konta()

         CASE menu = 2
              dziennik(2)

         CASE menu = 3
              rejestry()

         CASE menu = 4
              kontrahenci()

         CASE menu = 5
              zes()

         CASE menu = 6
              dodatki()

      ENDCASE
   end
******************** RESET BAZ
    i := 0
    unlock all
    DO WHILE (dbselectar(++i),USED())
        SET FILTER TO
        SET RELATION TO
    enddo
    SET PRINTER TO
    set console on
    set print off
#ifdef A_WIN_PRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
*********************

   ENDDO
********************************

   inisave("fk.ini")

   IF menu
      EXIT
   ENDIF

ENDDO


IF tak(hb_UTF8ToStr("CZY ARCHIWOWAĆ"),18,,.t.,.F.)
#ifdef A_LAN
    close DATABASES
    errorblock({|E|if(e:severity>1,break(E),.f.)})
    BEGIN SEQUENCE
    nuse daty exclusive
    use
#endif
#ifdef A_BACKUP
    __RUN( A_BACKUP )
#else
    ERRORLEVEL(41)
#endif
#ifdef A_LAN
    recover
    alarm(hb_UTF8ToStr("NIE MOŻNA ARCHIWIOWAĆ, BO INNI JESZCZE PRACUJĄ NA TYCH DANYCH."),,3,3)
    end sequence
#endif
ENDIF

@ 1,0 clear

SET CURSOR ON

return
**********************
proc reuse()
local txt,a,i,x
close databases
ferase("."+HB_ps()+"main_tmp"+ordbagext())
set default to (defa+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+HB_ps())

#ifdef A_LAN
   if Empty(Findfile('daty.dbf')) .and. !Empty(txt:=Findfile("daty.ini"))
      a:=memoread(txt)
      if a=hb_utf8Chr(0xFEFF)
       a:=hb_bsubstr(a,4)
      endif
     a:=getlines(a)
     aeval(a,{|x,i,c|c:=getlines(x,':='),if(len(c)=2,a[i]:={c[1],type(c[2]),10,2,,c[2]},)})
     dbcreate('daty',a)
     nUSE daty
     dbappend()
     aeval(a,{|x,i|fieldput(i,x[6])})
     UNLOCK
   endif
   nuse daty
#else
   if Empty(txt:=Findfile("daty.ini"))
      nUSE daty
      txt:=''

      a:=dbstruct()
      for i:=1 to len(a)
        x:=a[i,1]
        PUBLIC &x
        txt += x+':='+x+HB_EOL()
      next i

      //aeval(dbstruct(),{|x|__mvPublic(x[1]) ,txt+=x[1]+':='+x[1]+HB_EOL()})

      HB_memowrit(set(_SET_DEFAULT)+"daty.ini",txt)
      inisave("daty.ini")
   endif
   txt:="daty.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end sequence
   enddo
#endif
return
**********************
proc readinit()
memvar rejestry
local i,j,ap,ad
field nazwa,baza,relacje,nr_rej,smb_dow,valid,pre_proc,pola,header,lines,flagi,ile_kop,druk_proc,jpk
nuse rej_def NEW readonly
rejestry:={}
do while !eof()

   i:=fcount()
   ap:=array(i+1)
   ap[AR_SMB      ]:=NR_REJ
   ap[AR_NAME     ]:=NAZWA
   ap[AR_DBF      ]:=trim(baza)
   ap[AR_REL      ]:=make_subar(getlines(relacje,.t.))
   ap[AR_HEADER   ]:=HEADER
   ap[AR_LINES    ]:=LINES
   ap[AR_DOKUMENTY]:={}

   for i:=i to 7 step -1
     j:=fieldget(i)
     if valtype(j)='C'
        j:=trim(j)
     endif
     ap[i+1]:=j
   next i

aadd(rejestry,ap)
skip
enddo
nuse dok_def readonly
do while !eof()
ap:=make_subar(getlines(POLA,.t.))
for i:=1 to len(ap)
begin sequence
#define AP_STEP (AP_LTH-AP_POZWN+1)
  asize(ap[i], AP_POZWN -1 + (INT((len(ap[i])-AP_POZWN+AP_STEP)/AP_STEP))*AP_STEP)
  if empty(ap[i,AP_PICTURE])
     ap[i,AP_PICTURE]:=NIL
  endif
  if empty(ap[i,AP_VALID])
     ap[i,AP_VALID]:=NIL
  else
     ap[i,AP_VALID]:=&(ap[i,AP_VALID])
  endif
  if empty(ap[i,AP_WHEN])
     ap[i,AP_WHEN]:=NIL
  else
     ap[i,AP_WHEN]:=&(ap[i,AP_WHEN])
  endif

  for j:=0 to len(ap[i])-AP_LTH step AP_STEP
  if empty(ap[i,j+AP_WNVAL])
     ap[i,j+AP_WNVAL]:='K'$SubStr(ap[i,j+AP_POZWN],2)
  else
     ap[i,j+AP_WNVAL]:=&(ap[i,j+AP_WNVAL])
  endif
  if empty(ap[i,j+AP_MAVAL])
     ap[i,j+AP_MAVAL]:='K'$SubStr(ap[i,j+AP_POZMA],2)
  else
     ap[i,j+AP_MAVAL]:=&(ap[i,j+AP_MAVAL])
  endif
  if empty(ap[i,j+AP_IDWN])
     ap[i,j+AP_IDWN]:=NIL
  else
     ap[i,j+AP_IDWN]:=&(ap[i,j+AP_IDWN])
  endif
  if empty(ap[i,j+AP_IDMA])
     ap[i,j+AP_IDMA]:=NIL
  else
     ap[i,j+AP_IDMA]:=&(ap[i,j+AP_IDMA])
  endif
  next j
end sequence
next i
   i:=fcount()
   ad:=array(i-1)
   ad[AD_SMB]     :=smb_dow
   ad[AD_NAME]    :=trim(NAZWA)
   ad[AD_POLA]    :=ap
   ad[AD_VALID]   :=trim(VALID)
   ad[AD_FLAGS]   :=trim(flagi)
   ad[AD_KOPI]    :=ile_kop
   ad[AD_DPROC]   :=druk_proc
   ad[AD_PREPROC] :=pre_proc
   for i:=i to 10 step -1
     j:=fieldget(i)
     if valtype(j)='C'
        j:=trim(j)
     endif
     ad[i-1]:=j
   next i

//:={smb_dow,trim(NAZWA),ap,trim(VALID),trim(flagi),ile_kop,druk_proc,pre_proc}
j:=trim(nr_rej)
aeval(rejestry,{|x|if(x[AR_SMB]=j,aadd(x[AR_DOKUMENTY],ad),)})

skip
enddo
use
i:="fk.ini"
do while inirest(@i)
begin sequence
(&i,i:=NIL)
end sequence
enddo
return
*****************
func wersja()
#ifdef __DATE__
return 'Wersja: 3.'+lTrim(sTr(stod(__DATE__)-stod('20250101')))
#else
return 'Wersja: 2.'
#endif
*************
