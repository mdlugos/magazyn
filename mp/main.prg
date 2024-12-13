#include   "hbgtinfo.ch"

#ifdef A_ODBIORCY
#error ODBIORCY not supported
#endif
#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif
#ifndef A_DDBF
#define DatY MEMVAR
EXTERNAL __MRESTORE
#endif
#ifdef DatE
memvar dzisiaj
#endif
#ifdef A_EXT
request A_EXT
#endif

#ifdef A_ADS
#ifndef A_STSIMPLE
#define A_STSIMPLE
#endif
#endif


#ifdef A_TERMINAL
         ANNOUNCE HB_NOSTARTUPWINDOW
#endif

#ifdef A_DRUKCOMP
EXTERNAL __DBSDF,__DBDELIM,__COPYFILE,__RUN,__DBCONTINUE,__DBZAP
EXTERNAL STOD
EXTERNAL BIN2I
EXTERNAL BIN2L
EXTERNAL BIN2W
EXTERNAL BIN2D
EXTERNAL D2BIN
EXTERNAL FCLOSE
EXTERNAL FCREATE
EXTERNAL FCREATEU
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
EXTERNAL ACOPY
#endif

#ifdef A_XPRN
memvar  landscape,p_rown,p_colnl,p_rownl,p_land,p_cpi,p_pcl,P_4XON,P_4XOFF,P_COLN,P_BON,P_BOFF,P_UON,P_UOFF,P_36LPI,P_12LPI,P_8LPI,P_6LPI,P_SUPON,P_SUPOFF,p_margin,P_PON,P_POFF,P_HALFPAGE,p_init
#endif

#ifdef A_HPDF
  #define D_HWPRN A_HPRN
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif

#ifdef A_NETIO
memvar netio
#endif


MEMVAR  oprn,year2bckp,mag_biez,mag_poz,magazyny,adres_mag,mag_link,_sbkgr,_sbnorm,;
        dok_par,dokumenty,firma_n,DOK_ROZCH,dok_zewn,dok_zby,dok_fak,dok_ewid,dok_vat,;
        dok_zapl,r

MEMVAR  stary_rok,operator,is_spec,jmiar,stanowis,zamowienie,level1,defa,_snorm
memvar rodz_sprzed
#ifdef A_OLZA
memvar kont_kos,zaklady,czynnosc,dzialy,kont,kos,mater,stano,dzial
field nr,zaklad,rodzaj,tresc
#endif

field nazwa,konto,symbol,opis_koszt,kod,opis,numer,haslo,haslo_spec,;
    magazyn,magazynier,adres,smb_dok,nazwa_dok,p_r,zewn,wartosc,;
    lpmax,ile_kop,druk_proc,podpisy,index,nr_mag,stempel,tp_dni,spec_line,;
    ewidencja,kol_kpr,wyr_kpr,printdev,sub_dok,rozlicz,path,czy_hurt,paragon,;
    dok_comp,waluta

PROCEDURE MAIN()
parameters parametr,mag_biez,operator
memvar bckp,parametr,mag_biez,operator
local scr_menu,txt,mlog,a,mf:=.f.,i,menu

public defa,oprn
   hb_gtInfo( HB_GTI_WINTITLE , "Magazyn" )

if parametr='MAGDEF='
   defa:=SubStr(parametr,8)
   parametr:=mag_biez
   mag_biez:=operator
   operator:=''
else
   defa:=getenv("MAGDEF")
endif

defa:=getlines(defa,HB_OsPathListSeparator())
if defa[1]==''
   defa[1]:='.'
endif
aeval(defa,{|x,i,y|y:=right(x,1),if(y==HB_OsDriveSeparator(),x+=HB_ps()+curdir(x),),if(y<>HB_ps(),defa[i]:=x+HB_ps(),)})

#ifdef A_NETIO
   public netio:=''
#ifdef A_TERMINAL
   netio_MTServer( 2941,,atail(defa), /* RPC */ .T., )
/*   
 *       netio_MTServer( [<nPort>], [<cIfAddr>], [<cRootDir>], [<xRPC>],
 *                       [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
 *                       [<sSrvFunc>] )
 *                                              -> <pListenSocket> | NIL
 */  
   HB_IDLESLEEP(.1)
#endif
   if netio_connect( A_NETIO, '2941' )
      netio:=atail(defa)
   endif
#endif
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

#ifdef A_DIETA
if parametr='DIETADEF='
   a:=SubStr(parametr,10)
   parametr:=mag_biez
   mag_biez:=operator
   operator:=''
else
   a:=getenv("DIETADEF")
endif
  if empty(a)
    i:=rat(HB_ps(),left(defa,len(defa)-1))
    a:=if(i=0,defa+'..'+HB_ps(),left(defa,i))+'dieta'+HB_ps()
  endif
  SET PATH TO (set(_SET_PATH)+HB_OsPathListSeparator()+a+'roboczy'+HB_ps()+HB_OsPathListSeparator()+a)
#endif

setpos(3,0)
? padc(hb_UTF8ToStr(" ██████ █     █  ██████ ███████ ███████ █       █"),maxcol())
? padc(hb_UTF8ToStr("█        █   █  █          █    █       ██     ██"),maxcol())
? padc(hb_UTF8ToStr(" █████    █ █    █████     █    ██████  █ █   █ █"),maxcol())
? padc(hb_UTF8ToStr("      █    █          █    █    █       █  █ █  █"),maxcol())
? padc(hb_UTF8ToStr("██████     █    ██████     █    ███████ █   █   █"),maxcol())
?
? padc(hb_UTF8ToStr("█       █     █      ████      █     ██████ █     █ ██    █"),maxcol())
? padc(hb_UTF8ToStr("██     ██    █ █    █    ▀    █ █        ▄▀  █   █  █ █   █"),maxcol())
? padc(hb_UTF8ToStr("█ █   █ █   █▄▄▄█   █  ▄▄▄   █▄▄▄█     ▄▀     █ █   █  █  █"),maxcol())
? trim(padc(hb_UTF8ToStr("█  █ █  █  █▀▀▀▀▀█  █  ▀▀█  █▀▀▀▀▀█  ▄▀        █    █   █ █"),maxcol()))
#ifdef A_FIFO
#ifdef A_LIFO
?? " LIFO"
#else
?? " FIFO"
#endif
#endif
? trim(padc(hb_UTF8ToStr("█   █   █ █       █  ████  █       █ ██████    █    █    ██"),maxcol()))


set default to (defa+"roboczy"+HB_ps())

STARY_ROK=NIL
year2bckp:={}
#ifdef A_XPRN
   landscape:=.f.
#ifdef A_PCL
   p_pcl:=.t.
#else
   p_pcl:=.f.
#endif
#endif
    MEMVAR->level1      :=0
    MEMVAR->lastlevel   :=0
    MEMVAR->menupom     :=0
    MEMVAR->menuzest    :=0
    MEMVAR->miar_opcja  :=.f.
    MEMVAR->doc_opcja   :=.t.
#ifdef A_FA
//    MEMVAR->stawkizby:=MEMVAR->stawki:={'22',' 7',' 0','zw','bo'}
//    MEMVAR->bckp        :='lha u %magdef%archiwum\kopi%1 %magdef%%1\ *.db? *.ini'
#ifdef A_PROCMAR
#ifdef A_CENVAT
    MEMVAR->liczczm     :={|cz,pv,pm|zaokr(cz*(100+pv)/(100-pm))}
    MEMVAR->liczm       :={|czak,czby,pv|round(100-czak/czby*(100+pv),0)}
#else
    MEMVAR->liczczm     :={|cz,pv,pm|zaokr(cz*100/(100-pm))}
    MEMVAR->liczm       :={|czak,czby,pv|round(100-czak/czby*100,0)}
#endif
#else
   REQUEST zaokr
#endif
#endif


i:=row()
setpos(i-6,0)

*******************                    Tworzenie tablic dla aczojs() itp

#ifdef DatE
private dzisiaj:=date()
#endif

/*
   i:={MaxRow(),maxcol(),}
   txt:="magazyn.ini"
   do while inirest(@txt)
begin sequence
     (&txt,txt:=NIL)
end sequence
   enddo

   if MaxRow()>i[2] .or. maxcol()>i[2]
     i[1]:=min(i[1],MaxRow())
     i[2]:=min(i[2],maxcol())
     i[3]:=savescreen(0,0,i[1],i[2])
     clear screen
     restscreen(0,0,i[1],i[2],i[3])
   endif
*/

readinit()
reuse()
/*
#ifndef A_DDBF
   txt:="daty.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end
   enddo
#endif
*/
#ifdef A_XPRN
//   p_reset()
/*   
   txt:="xprn.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end sequence
   enddo
*/   
#endif

txt:={}
a:=HB_DISKSPACE(defa)

IF a<104857600
    aadd(txt,message("TYLKO"+STR(int(a/1024/1024),3)+hb_UTF8ToStr(" MEGABAJTÓW;WOLNEGO MIEJSCA NA DYSKU !")))
ENDIF
IF date()-45>DatY->D_Z_MIES1
    aadd(txt,MESSAGE(hb_UTF8ToStr("ZAMKNIJ MIESIĄC !")))
ENDIF

if !empty(txt)
   afill(txt[1],0,1,4)
   message(txt[1])
   setpos(i,0)
endif

@ 18,0
if !empty(parametr)
    set color to w
    mag_poz:=max(1,ascan(magazyny,mag_biez))
    if(empty(operator),operator:='',)
    (&parametr,parametr:=NIL)
    set cursor on
    quit
endif

#ifdef DatE
@ 17,0 SAY "Data dzisiaj: "
readmodal( {_GET_( dzisiaj, "dzisiaj",,,)} )
#endif

DO WHILE .T.
  @ 18,0 SAY " Podaj hasło: " UNICODE
  txt:=""
  SET CURSOR ON
  do while 0<(i:=INkey(0)) .and. i#13
    if i=28
      help("haslo")
      loop
    elseif i=8 .and. ""#txt
      txt:=left(txt,len(txt)-1)
      @ row(),col()-1 say " "
      setpos(row(),col()-1)
      loop
    endif
    txt+=HB_KEYCHAR(i)
    ?? if(HB_CDPISUTF8(),"♦",chr(4))
  enddo

#ifndef A_DECRYPT
#define A_DECRYPT(x) l2bin(x)
#endif

   XSELECT OBSLUGA READONLY
       LOCATE FOR UpP(txt)==trim(A_DECRYPT(haslo))
       IF EOF()
          ? hb_UTF8ToStr("złe hasło ...")
          set color to w
          @ 20,0 clear
          quit                        // gdy nie znalazl
       ENDIF
       mag_biez:=MAGAZYN
       mag_poz:=max(1,ascan(magazyny,mag_biez))
       operator:=MAGAZYNIER
#ifndef hAslo_spec
       is_spec:=fieldpos([haslo_spec])<>0 .and. !empty(haslo_spec)
#endif
       mlog:=array(len(magazyny))
       afill(mlog,.f.)
       i:=0
       a:={}
       EXEC {||mlog[txt]:=.t.,++i,mf:=mf.or.ascan(a,haslo)<>0,aadd(a,haslo)} ALL FOR Trim(operator)==Trim(magazynier) .and. (txt:=ascan(magazyny,magazyn))>0 .and. UpP(A_DECRYPT(haslo))=A_DECRYPT(haslo) .and. !mlog[txt]
       if i<2
          mlog:=NIL
       endif
       if mf .and. !ACZOJS(MAGAZYNY,@MAG_BIEZ,@mag_poz,mlog)
          set color to w
          @ 20,0 clear
          quit
       endif
   USE


   DO WHILE .T. // PETLA GLOWNA DLA DANEGO MAGAZYNU
      //a := hb_gtInfo( HB_GTI_BOXCP, 'UTF8EX')

  SET CURSOR OFF

*************************************

if iscolor()
  SET COLOR TO BG+  // TWORZENIE EKRANU
else
  set color to w
endif
//    dispbegin()
   CLEAR screen
      ? padc(firma_n,maxcol())

      ? padc(trim(magazyny[mag_poz])+" "+trim(adres_mag[mag_poz])+"; "+TRIM(operator),maxcol())
if iscolor()
      SET COLOR TO (_sbkgr)
endif
          @ 4,maxcol()/2-29,7,maxcol()/2+30 BOX UNICODE "╔═╗║╝═╚║ "
          @ 4,maxcol()/2-4 SAY " M E N U " COLOR if(iscolor(),_sbkgr,"W+")

          @ MaxRow()-2,0,MaxRow(),maxcol() BOX UNICODE "╔═╗║╝═╚║ "

          @ MaxRow()-1,1 SAY "                     -wybór opcji    -                  -wyjście" UNICODE color if(iscolor(),_sbnorm,"W+")

      SET COLOR TO I

          @ MaxRow()-1,2 SAY "K"
          @ MaxRow()-1,4 SAY "D"
          @ MaxRow()-1,6  SAY "R"
          @ MaxRow()-1,8  SAY "Z"
          @ MaxRow()-1,10 SAY "P"
          @ MaxRow()-1,12 BOX "─►" UNICODE
          @ MaxRow()-1,15 BOX '◄─' UNICODE
          @ MaxRow()-1,18 BOX '◄─┘' UNICODE
          @ MaxRow()-1,35 SAY "F1"
          @ MaxRow()-1,40 SAY "P O M O C" color (_sbnorm)
          @ MaxRow()-1,53 say "Esc"

  IF STARY_ROK#NIL
     @ MaxRow()-1,72 say str(year(stary_rok),4)+' !' COLOR if(iscolor(),"*"+_sbkgr,"W*+")
  else
     @ MaxRow()-1,72 say "      " COLOR if(iscolor(),"*"+_sbkgr,"W*+")
  ENDIF

      SET MESSAGE TO 5 CENTER

      SET COLOR TO (_sbnorm)

      @ 6,maxcol()/2-27 PROMPT "Kartoteki"    UNICODE MESSAGE "Bieżące stany magazynu, kartoteki materiałów."
      @ 6,maxcol()/2-17 PROMPT "Dok. obrotu"  UNICODE MESSAGE "Przeglądanie wprowadzonych wcześniej dokumentów."
      @ 6,maxcol()/2-5  PROMPT "Rejestracja " UNICODE MESSAGE "Wprowadzanie dokumentów obrotu materiałowego."
      @ 6,maxcol()/2+8  PROMPT "Zestawienia"  UNICODE MESSAGE 'Wydruki zestawień.'
      @ 6,maxcol()/2+20 PROMPT "Pozostałe"    UNICODE MESSAGE 'Dodatkowe funkcje programu.'

    //dispend()
      //hb_gtInfo( HB_GTI_BOXCP, a)
      menu:=level1
      MENU TO menu

      set color to (_snorm)

  @ 18,0 clear

      IF menu=0
         i:=mag_poz
         IF empty(mlog) .or. !ACZOJS(MAGAZYNY,@MAG_BIEZ,@i,mlog)

            @ 18,0
            menu:=tak(hb_UTF8ToStr("Czy rzeczywiscie kończysz pracę"),18,,.t.,0)
            if valtype(menu)="L"
               EXIT
            ENDIF
         Else
            mag_poz:=i
         ENDIF

      ENDIF

  SETPOS(6,maxcol()/2)

      level1:=menu
  begin sequence
      DO CASE

         CASE level1 = 3
              dminput()

         CASE level1 = 1
              katalog()

         CASE level1 = 2
              dmprzeg()

         CASE level1 = 4
              zestawienia()

         CASE level1 = 5
              pomocnicze()

      ENDCASE
   end
******************** RESET BAZ
    i := 1
    unlock all
    DO WHILE (i)->(USED())
        SELECT (i)
        SET FILTER TO
        SET RELATION TO
    ++i
    enddo
#ifdef D_HWPRN
if valtype(oprn)='O'
   if prow()>1
      specout(chr(12))
   endif
   oprn:Destroy()
   oprn:=NIL
endif
#endif
    set console on
    set print off
#ifdef A_PRINT
    txt:=set(_SET_PRINTFILE,'')
    if ! txt == set(_SET_PRINTFILE) .and. File(txt)
        A_PRINT(txt)
     endif
#endif
    SET PRINTER TO
*********************
   ENDDO
********************************

//  save_zm()
   inisave("magazyn.ini")
   IF menu
      EXIT
   ENDIF
ENDDO

menu:=tak(hb_UTF8ToStr("CZY ARCHIWOWAĆ"),18,,.t.,.F.)

IF menu
   CLOSE ALL
   hb_idlesleep(0.5)
   IF type('bckp')='C' .and. !empty(year2bckp)
#ifdef A_BACKUP
      if len(bckp)>12
        txt:=bckp
      else
        txt:=memoread(defa+bckp)
      endif
      aeval(year2bckp,{|x|__run(strtran(bckp,'%1',str(x,4)))})
#else
      ERRORLEVEL(40)
#endif
   endif
#ifdef A_BACKUP
    __RUN( A_BACKUP )
#else
    ERRORLEVEL(41)
#endif
ENDIF


@ 4,0 clear

SET CURSOR ON

return
**********
STATIC PROCEDURE MAKE_ARR(AR,CB)
LOCAL buf,l
MEMVAR MAGAZYNY
FIELD NR_MAG
PRIVATE mag_poz
  DO WHILE !EOF()
    mag_poz:=ascan(magazyny,nr_mag)
    buf:=eval(cb)
    if mag_poz=0
      aeval(AR,{|x|if(0=ascan(x,left(buf,at(" ",buf))),aadd(x,buf),)})
    ELSE
      l:=ascan(ar[mag_poz],left(buf,at(" ",buf)))
      if l=0
         aadd(ar[mag_poz],buf)
      else
         ar[mag_poz,l]:=buf
      endif
    ENDIF
    SKIP
  ENDDO
RETURN
**********************
proc readinit
local txt
#ifdef A_DIETA
field posilek,dieta,grupa,stawka,waga
MEMVAR posilki,diety,grupy,g_stawki,p_wagi,PosStr,DietyStr,GrStr
public posilki:={},diety:={},grupy:={},g_stawki:={},p_wagi:={},PosStr:="",DietyStr:="",GrStr:=""
#endif
private mag_biez,mag_poz,r

public jmiar:={},;
       magazyny:={},;
       adres_mag:={},;
       dokumenty:={}
#ifdef A_HLINK
public mag_link:={}
#endif

public dok_rozch:="",dok_zewn:=""

#ifdef A_FA
public dok_zby:="",;
       dok_fak:="",;
       dok_vat:="",;
       dok_ewid:=""
#ifdef A_FK
public dok_zapl:=""
#endif
#else
#ifdef A_IZ
public dok_ewid:=""
#endif
#endif


   txt:="magazyn.ini"
   do while inirest(@txt)
begin sequence
     (&txt,txt:=NIL)
end sequence
   enddo


   select 0

XSELECT J_MIARY READONLY ; EXECUTE aadd(JMIAR,nazwa)
#command MAKE ARRAY <ar> WITH <exp> => make_arr(<ar>,<{exp}>)
#ifdef A_HLINK
#define D_HLINK ,if(!empty(path),aadd(mag_link,{numer+" "+trim(nazwa)+" "+trim(adres),trim(path),czy_hurt}),)
#else
#define D_HLINK
#endif
USE
XSELECT MAGAZYNY  READONLY ; EXECUTE {||aadd(MAGAZYNY,NUMER+" "+trim(nazwa)),;
                  aadd(adres_mag,trim(adres)) D_HLINK ,;
                  aadd(dokumenty,{})}
#undef D_HLINK
public dok_par:=ACLONE(dokumenty),;
zamowienie:=ACLONE(dokumenty)

#ifdef A_FA
#ifndef A_RODZ_S
#define A_RODZ_S
#endif
#endif

#ifdef A_RODZ_S
public rodz_sprzed:=ACLONE(dokumenty)
#endif

#ifdef A_OLZA
public kont_kos:={},zaklady:={},czynnosc:={},dzialy:={},dzial:={},kont:={},kos:={},stano:={},mater:={},stanowis:={}
USE
XSELECT ZAKLADY READONLY ; EXECUTE {||aadd(ZAKLADY,ZAKLAD+" "+ADRES)}
USE
XSELECT CZYNNOSC READONLY ; EXECUTE {||aadd(CZYNNOSC,SYMBOL+" "+RODZAJ)}

//USE DZIALY READONLY ; EXECUTE {||aadd(DZIALY,NR+" "+ZAKLAD+" "+OPIS)}

USE
XSELECT DZIAL READONLY ; EXECUTE {||aadd(DZIAL,NR+" "+ZAKLAD+" "+opis)}
USE
XSELECT KONT_KOS  READONLY ;EXECUTE {||aadd(KONT_KOS,symbol+" "+tresc)}

USE
XSELECT KONT  READONLY ;EXECUTE {||aadd(KONT,konto+" "+opis)}

USE
XSELECT KOS  READONLY ;EXECUTE {||aadd(KOS,konto+" "+opis)}
USE
XSELECT stano  READONLY ;EXECUTE {||aadd(stano,konto+" "+opis)}
USE
XSELECT stanowis  READONLY ;EXECUTE {||aadd(stanowis,konto+" "+opis_koszt)}
USE
XSELECT mater  READONLY ;EXECUTE {||aadd(mater,konto+" "+opis)}
#else
public stanowis:=ACLONE(dokumenty)
#ifdef A_DIETA
if file('diety.dbf')
USE
XSELECT DIETY readonly; dbeval({||aadd(diety,dieta+" "+opis), dietyStr+=dieta})

USE
XSELECT posilki readonly; dbeval({||aadd(posilki,posilek+" "+opis), PosStr+=posilek})
IF fieldpos([WAGA])<>0
dbeval({||aadd(p_wagi,waga/100)})
endif

USE
XSELECT grupy readonly; dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa})
IF fieldpos([stawka])<>0
dbeval({||aadd(g_stawki,stawka)})
endif
endif
#endif
#ifndef A_OBR
USE
XSELECT STANOWIS  READONLY ;MAKE ARRAY stanowis with KONTO+" "+TRIM(OPIS_KOSZT)
#endif
#endif
USE
XSELECT ZAMOWIEN  READONLY
#ifdef A_RODZ_S
set filter to p_r="P"
go top
MAKE ARRAY ZAMOWIENIE with KOD+" "+OPIS
set filter to p_r#"P"
go top
MAKE ARRAY rodz_sprzed with KOD+" "+OPIS
#else
MAKE ARRAY ZAMOWIENIE with KOD+" "+OPIS
#endif

USE
XSELECT DOK_DEF READONLY


#ifdef A_SUBDOK
#define D_SUBDOK +sub_dok
#else
#define D_SUBDOK
#endif

  go top
  DO WHILE !EOF()
    mag_biez:=nr_mag
    mag_poz:=ascan(magazyny,mag_biez)
    if mag_poz=0
      r:=len(magazyny)
      for mag_poz=1 to r
        mag_biez:=left(magazyny[mag_poz],2)
        if 0=ascan(dokumenty[mag_poz],smb_dok D_SUBDOK)
          mkdokpar(0)
        endif
      next
    ELSE
      r:=ascan(dokumenty[mag_poz],smb_dok D_SUBDOK)
      mkdokpar(r)
    ENDIF
    SKIP
  ENDDO
  use
return
***********************
proc mkdokpar(r)

local i,j,txt
      if r=0
         aadd(dokumenty[mag_poz],NIL)
         r:=len(dokumenty[mag_poz])
         aadd(dok_par[mag_poz],NIL)
      else
         dok_rozch:=strtran(dok_rozch,mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_zewn :=strtran(dok_zewn, mag_biez+SMB_DOK D_SUBDOK+",",'')
#ifdef A_FA
         dok_vat  :=strtran(dok_vat , mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_fak  :=strtran(dok_fak , mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_zby  :=strtran(dok_zby , mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_ewid :=strtran(dok_ewid, mag_biez+SMB_DOK D_SUBDOK+",",'')
#ifdef A_FK
         dok_zapl :=strtran(dok_zapl , mag_biez+SMB_DOK D_SUBDOK+",",'')
#endif
#else
#ifdef A_IZ
         dok_ewid :=strtran(dok_ewid, mag_biez+SMB_DOK D_SUBDOK+",",'')
#endif
#endif
      endif

         dokumenty[mag_poz,r]:=SMB_DOK D_SUBDOK+" "+NAZWA_DOK
         j:=fcount()
#ifdef A_SUBDOK
         i:=j-4
#else
         i:=j-3
#endif
         dok_par[mag_poz,r]:=array(i)

         dok_par[mag_poz,r,1]:=P_R
         dok_par[mag_poz,r,2]:=ZEWN
         dok_par[mag_poz,r,3]:=trim(KONTO)
         dok_par[mag_poz,r,4]:=trim(WARTOSC)
         dok_par[mag_poz,r,5]:=lpmax
         dok_par[mag_poz,r,6]:=ile_kop

#ifdef A_FA
#ifdef A_DRUKCOMP
         dok_par[mag_poz,r,7]:=trim(druk_proc)
#else
         dok_par[mag_poz,r,7]:=trim(podpisy)
         dok_par[mag_poz,r,10]:=trim(stempel)
         dok_par[mag_poz,r,11]:=trim(spec_line)
#endif
         dok_par[mag_poz,r,8]:=ewidencja
         dok_par[mag_poz,r,9]:=tp_dni
#ifdef A_KPR
         dok_par[MAG_POZ,r,A_KPR]:=trim(kol_kpr)
         dok_par[MAG_POZ,r,A_KPR+1]:=trim(wyr_kpr)
#endif
#ifdef A_FK
         dok_par[MAG_POZ,r,A_FK]:=rozlicz
#endif
#ifdef A_DF
         dok_par[MAG_POZ,r,A_DF]:=paragon
#endif
#ifdef A_WALUTA
         dok_par[MAG_POZ,r,A_WALUTA]:=if(fieldpos([waluta])<>0,waluta,.f.)
#endif
#else
         dok_par[mag_poz,r,7]:=trim(podpisy)
#ifdef A_IZ
         dok_par[mag_poz,r,8]:=ewidencja
#endif
#endif
#ifdef A_LPTN
         dok_par[MAG_POZ,r,A_LPTN]:=trim(printdev)
#endif
#ifdef A_DOKCOMP
         dok_par[MAG_POZ,r,A_DOKCOMP]:=trim(dok_comp)
#endif

         do while dok_par[MAG_POZ,r,i]=NIL
          txt:=fieldget(j--)
          if valtype(txt)='C'
            txt:=trim(txt)
          endif
          dok_par[MAG_POZ,r,i--]:=txt
         enddo

          if p_r#"P"
             dok_rozch+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
          if zewn#"W"
             dok_zewn+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
#ifdef A_FA
          if zewn$"UV"
             dok_vat+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
          if p_r="F"
             dok_fak+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
          if ewidencja#"E"
             dok_zby+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
          if ewidencja#"Z"
             dok_ewid+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
#ifdef A_FK
          if rozlicz
             dok_zapl+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
#endif
#else
#ifdef A_IZ
          if ewidencja#"Z"
             dok_ewid+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
#endif
#endif

return

return
#undef D_SUBDOK
**********************
proc reuse
local a,i,txt
field index
    close databases
    if stary_rok#NIL .and. 0=ascan(year2bckp,year(stary_rok))
       aadd(year2bckp,year(stary_rok))
    endif
    set default to (defa+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+HB_ps())

 #ifdef A_DDBF
    select 10
 
    if Empty(Findfile('daty.dbf')) .and. !Empty(txt:=Findfile("daty.ini"))
      a:=getlines(memoread(txt))
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
       aeval(dbstruct(),{|x|txt+=x[1]+':='+x[1]+HB_EOL()})
       memowrit(set(_SET_DEFAULT)+"daty.ini",txt)
       inisave("daty.ini")
    endif
    txt:="daty.ini"
    do while inirest(@txt)
    begin sequence
    (&txt,txt:=NIL)
    end sequence
    enddo
 #endif
           sel("DM",1)

       if stary_rok=NIL //tymczasowo bo nie wszyscy mają
#ifdef A_ADS
#ifdef A_MM
          mkindex('DM_TRIM','SMB_DOW+PADL(RTRIM(NR_DOWODU),5)')
#else
          mkindex('DM_TRIM','NR_MAG+SMB_DOW+PADL(RTRIM(NR_DOWODU),5)')
#endif
#else
#ifdef A_MM
          mkindex('DM_TRIM','SMB_DOW+PADL(STRTRAN(NR_DOWODU," "),5)')
#else
          mkindex('DM_TRIM','NR_MAG+SMB_DOW+PADL(STRTRAN(NR_DOWODU," "),5)')
#endif
#endif
          set order to 1
       endif

          sel("MAIN",2)
          select 4
#ifdef A_DIETA
          sel("INDX_MAT",2,.t.) //SHARED
#ifndef A_LAN
          flock()
#endif
#else
          sel("INDX_MAT",2)
#endif

#ifndef STANY
#ifdef A_STSIMPLE
          sel("STANY",2)
#else
          select 0
          nUSE stany
          set relation to index into indx_mat
#ifdef A_CDX
          if empty(ordbagname('stan_mag'))
             ordlistadd('stany')
          endif
#else
          set index to stan_naz,stan_mag,stan_all,stan_ind
#endif
#endif
#endif
          select 0
          nUSE ind_lam
          sel("FIRMY",2)
#ifdef A_OBR
          sel("stanowis",2)
#endif

#ifdef A_XPRN
   p_reset()
/*
   txt:="xprn.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end sequence
   enddo
*/
#endif

return
*****************
func wersja()
#ifdef __DATE__
return 'Wersja: 4.'+lTrim(sTr(stod(__DATE__)-stod('20250101')))
#else
return 'Wersja: 3.'
#endif
*************
