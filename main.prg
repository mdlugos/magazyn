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
#ifdef D_HWPRN
memvar oprn
#endif

#ifdef A_NETIO
memvar netio
#endif

#ifdef PLWIN
    #include   'hbgtinfo.ch'
#endif

MEMVAR  year2bckp,mag_biez,mag_poz,magazyny,adres_mag,mag_link,_sbkgr,_sbnorm,;
    dok_par,dokumenty,firma_n,DOK_ROZCH,dok_zewn,dok_zby,dok_fak,dok_ewid,dok_vat

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

PROCEDURE MAIN(parametr,menu,level1)
memvar bckp
local scr_menu,i,txt,mlog,a,mf:=.f.

public defa

#ifdef PLWIN
   hb_gtInfo( HB_GTI_WINTITLE , "Magazyn" )
#endif
#ifdef D_HWPRN
   public oprn
#endif
if parametr='MAGDEF='
   defa:=subs(parametr,8)
   parametr:=menu
   menu:=level1
else
   defa:=getenv("MAGDEF")
endif

IF ""#defa .and. right(defa,1)<>HB_OsPathSeparator()
   defa+=HB_OsPathSeparator()
endif

#ifdef A_NETIO
   public netio:=''
#ifdef A_TERMINAL
   netio_MTServer( 2941,,defa, /* RPC */ .T., )
/*   
 *       netio_MTServer( [<nPort>], [<cIfAddr>], [<cRootDir>], [<xRPC>],
 *                       [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
 *                       [<sSrvFunc>] )
 *                                              -> <pListenSocket> | NIL
 */  
   HB_IDLESLEEP(.1)
#endif
   if netio_connect( A_NETIO, '2941' )
      netio:=defa
   endif
#endif

if ""=defa
  defa:="."+HB_OsPathSeparator()
  SET PATH TO (defa)
else
  SET PATH TO ("."+HB_OsPathSeparator()+HB_OsPathListSeparator()+defa)
endif
#ifdef A_DIETA
a:=getenv('DIETADEF')
  if empty(a)
    i:=rat(HB_OsPathSeparator(),left(defa,len(defa)-1))
    a:=if(i=0,defa+'..'+HB_OsPathSeparator(),left(defa,i))+'dieta'+HB_OsPathSeparator()
  endif
  SET PATH TO (set(_SET_PATH)+HB_OsPathListSeparator()+a+'roboczy'+HB_OsPathSeparator()+HB_OsPathListSeparator()+a)
#endif

setpos(3,0)
? padc(" ÛÛÛÛÛÛ Û     Û  ÛÛÛÛÛÛ ÛÛÛÛÛÛÛ ÛÛÛÛÛÛÛ Û       Û",maxcol()-1)
? padc("Û        Û   Û  Û          Û    Û       ÛÛ     ÛÛ",maxcol()-1)
? padc(" ÛÛÛÛÛ    Û Û    ÛÛÛÛÛ     Û    ÛÛÛÛÛÛ  Û Û   Û Û",maxcol()-1)
? padc("      Û    Û          Û    Û    Û       Û  Û Û  Û",maxcol()-1)
? padc("ÛÛÛÛÛÛ     Û    ÛÛÛÛÛÛ     Û    ÛÛÛÛÛÛÛ Û   Û   Û",maxcol()-1)
?
? padc("Û       Û     Û      ÛÛÛÛ      Û     ÛÛÛÛÛÛ Û     Û ÛÛ    Û",maxcol())
? padc("ÛÛ     ÛÛ    Û Û    Û    ß    Û Û        Üß  Û   Û  Û Û   Û",maxcol())
? padc("Û Û   Û Û   ÛÜÜÜÛ   Û  ÜÜÜ   ÛÜÜÜÛ     Üß     Û Û   Û  Û  Û",maxcol())
? trim(padc("Û  Û Û  Û  ÛßßßßßÛ  Û  ßßÛ  ÛßßßßßÛ  Üß        Û    Û   Û Û",maxcol()))
#ifdef A_FIFO
#ifdef A_LIFO
?? " LIFO"
#else
?? " FIFO"
#endif
#endif
? trim(padc("Û   Û   Û Û       Û  ÛÛÛÛ  Û       Û ÛÛÛÛÛÛ    Û    Û    ÛÛ",maxcol()))


set default to (defa+"roboczy"+HB_OsPathSeparator())

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
setpos(i-6,maxcol()/2)

*******************                    Tworzenie tablic dla aczojs() itp

i:={maxrow(),maxcol()}
#ifdef DatE
private dzisiaj:=date()
#endif

   txt:="magazyn.ini"
   do while inirest(@txt)
begin sequence
     (&txt,txt:=NIL)
end sequence
   enddo


txt:=savescreen(0,0,i[1],i[2])
clear screen
restscreen(0,0,i[1],i[2],txt)

readinit()
reuse()
#ifndef A_DDBF
   txt:="daty.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end
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
    aadd(txt,message("TYLKO"+STR(DISKSPACE(),7)+" BAJTàW;WOLNEGO MIEJSCA NA DYSKU !"))
ENDIF

IF date()-45>DatY->D_Z_MIES1
    aadd(txt,MESSAGE("ZAMKNIJ MIESI¤C !"))
ENDIF

if !empty(txt)
   afill(txt[1],0,1,4)
   message(txt[1])
   setpos(i,0)
endif

@ 18,0
if !empty(parametr)
    set color to w
    mag_biez:=menu
    mag_poz:=max(1,ascan(magazyny,mag_biez))
    operator:=''
    (&parametr,parametr:=NIL)
    set cursor on
    quit
endif

#ifdef DatE
@ 17,0 SAY "Data dzisiaj: "
readmodal( {_GET_( dzisiaj, "dzisiaj",,,)} )
#endif

DO WHILE .T.
  @ 18,0 SAY " Podaj hasˆo: "
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
    txt+=chr(i)
    ?? ""
  enddo

#ifndef A_DECRYPT
#define A_DECRYPT(x) l2bin(x)
#endif

   XSELECT OBSLUGA READONLY
       LOCATE FOR UpP(txt)==trim(A_DECRYPT(haslo))
       IF EOF()
          ? "zˆe hasˆo ..."
          set color to w
          @ 20,0 clear
          quit                        // gdy nie znalazl
       ENDIF
       mag_biez:=MAGAZYN
       mag_poz:=max(1,ascan(magazyny,mag_biez))
       operator:=MAGAZYNIER
#ifndef hAslo_spec
       is_spec:=fieldpos('haslo_spec')<>0 .and. !empty(haslo_spec)
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
  SET CURSOR OFF

*************************************

if iscolor()
  SET COLOR TO BG+  // TWORZENIE EKRANU
else
  set color to w
endif
#ifndef PLWIN
    dispbegin()
#endif
   CLEAR screen
      ? padc(firma_n,maxcol())

      ? padc(trim(magazyny[mag_poz])+" "+trim(adres_mag[mag_poz])+"; "+TRIM(operator),maxcol())
if iscolor()
      SET COLOR TO (_sbkgr)
endif
/*
#ifdef PLWIN
          @ 4,maxcol()/2-29 CLEAR TO 7,maxcol()/2+30
          @ 4,maxcol()/2-4 SAY " M E N U "
          @ 4,maxcol()/2-29 TO 7,maxcol()/2+30
#else
*/
          @ 4,maxcol()/2-29,7,maxcol()/2+30 BOX "ÉÍ»º¼ÍÈº "
          @ 4,maxcol()/2-4 SAY " M E N U " COLOR if(iscolor(),_sbkgr,"W+")
//#endif

          @ maxrow()-2,0,maxrow(),maxcol() BOX "ÉÍ»º¼ÍÈº "

          @ maxrow()-1,1 SAY "                     -wyb¢r opcji    -                  -wyj˜cie" color if(iscolor(),_sbnorm,"W+")

      SET COLOR TO I

          @ maxrow()-1,2 SAY "K"
          @ maxrow()-1,4 SAY "D"
          @ maxrow()-1,6  SAY "R"
          @ maxrow()-1,8  SAY "Z"
          @ maxrow()-1,10 SAY "P"
          @ maxrow()-1,12 SAY "Ä>"
          @ maxrow()-1,15 SAY "<Ä"
          @ maxrow()-1,18 SAY "<ÄÙ"
          @ maxrow()-1,35 SAY "F1"
          @ maxrow()-1,40 SAY "P O M O C" color (_sbnorm)
          @ maxrow()-1,53 say "Esc"

  IF STARY_ROK#NIL
     @ maxrow()-1,72 say str(year(stary_rok),4)+' !' COLOR if(iscolor(),"*"+_sbkgr,"W*+")
  else
     @ maxrow()-1,72 say "      " COLOR if(iscolor(),"*"+_sbkgr,"W*+")
  ENDIF

      SET MESSAGE TO 5 CENTER

      SET COLOR TO (_sbnorm)

      @ 6,maxcol()/2-27 PROMPT "Kartoteki"    MESSAGE "Bie¾¥ce stany magazynu, kartoteki materiaˆ¢w."
      @ 6,maxcol()/2-17 PROMPT "Dok. obrotu"  MESSAGE "Przegl¥danie wprowadzonych wcze˜niej dokument¢w."
      @ 6,maxcol()/2-5  PROMPT "Rejestracja " MESSAGE "Wprowadzanie dokument¢w obrotu materiaˆowego."
      @ 6,maxcol()/2+8  PROMPT "Zestawienia"  MESSAGE 'Wydruki zestawieä.'
      @ 6,maxcol()/2+20 PROMPT "Pozostaˆe"    MESSAGE 'Dodatkowe funkcje programu.'

#ifndef PLWIN
    dispend()
#endif
    menu:=level1
      MENU TO menu

      set color to (_snorm)

  @ 18,0 clear

      IF menu=0
         i:=mag_poz
         IF empty(mlog) .or. !ACZOJS(MAGAZYNY,@MAG_BIEZ,@i,mlog)

            @ 18,0
            menu:=tak("Czy rzeczywiscie koäczysz prac©",18,,.t.,0)
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
    set console on
    set print off
#ifdef A_PRINT
    txt:=set(_SET_PRINTFILE,'')
    if ! txt == set(_SET_PRINTFILE) .and. File(txt)
        A_PRINT(txt)
     endif
#endif
    SET PRINTER TO
#ifdef D_HWPRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
*********************

   ENDDO
********************************

//  save_zm()
   inisave("magazyn.ini")

   IF menu
      EXIT
   ENDIF

ENDDO

CLOSE DATABASES


IF tak("CZY ARCHIWOWA",18,,.t.,.F.)
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
LOCAL MAG_POZ,buf,l
MEMVAR MAGAZYNY
FIELD NR_MAG
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

local r,mag_poz,txt,mag_biez
#ifdef A_DIETA
field posilek,dieta,grupa,stawka,waga
MEMVAR posilki,diety,grupy,g_stawki,p_wagi,PosStr,DietyStr,GrStr
public posilki:={},diety:={},grupy:={},g_stawki:={},p_wagi:={},PosStr:="",DietyStr:="",GrStr:=""
#endif

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
       dok_vat:=""
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

XSELECT J_MIARY READONLY; EXECUTE aadd(JMIAR,nazwa)
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
IF fieldpos("WAGA")<>0
dbeval({||aadd(p_wagi,waga/100)})
endif

USE
XSELECT grupy readonly; dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa})
IF fieldpos('stawka')<>0
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

#ifdef A_FA
#ifndef A_DRUKCOMP
#define D_FA ,trim(podpisy),ewidencja,tp_dni,trim(stempel),trim(spec_line)
#else
#define D_FA ,trim(druk_proc),ewidencja,tp_dni
#endif
#else
#ifdef A_IZ
#define D_FA ,trim(podpisy),ewidencja
#else
#define D_FA ,trim(podpisy)
#endif
#endif


#ifdef A_KPR
#define D_KPR ,trim(kol_kpr),trim(wyr_kpr)
#else
#define D_KPR
#endif

#ifdef A_LPTN
#define D_LPT ,trim(printdev)
#else
#define D_LPT
#endif

#ifdef A_FK
#define D_FK ,rozlicz
#else
#define D_FK
#endif


#ifdef A_SUBDOK
#define D_SUBDOK +sub_dok
#else
#define D_SUBDOK
#endif

#ifdef A_DF
#define D_DF ,paragon
#else
#define D_DF
#endif

#ifdef A_DOKCOMP
#define D_DC ,trim(dok_comp)
#else
#define D_DC
#endif

#ifdef A_WALUTA
#define D_WA ,if(fieldpos('waluta')<>0,waluta,.f.)
#else
#define D_WA
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
          aadd(dokumenty[mag_poz],SMB_DOK D_SUBDOK+" "+NAZWA_DOK)
          aadd(dok_par[mag_poz],{P_R,ZEWN,trim(KONTO),trim(WARTOSC),lpmax,ile_kop D_FA D_KPR D_LPT D_FK D_DF D_DC D_WA})
          if p_r#"P"
             dok_rozch+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
          if zewn#"W"
             dok_zewn+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
#ifdef A_FA
          if zewn$"UV"
             dok_vat+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
          if p_r="F"
             dok_fak+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
          if ewidencja#"E"
             dok_zby+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
#ifdef A_IZ
          if ewidencja#"Z"
             dok_ewid+=mag_biez+SMB_DOK D_SUBDOK+","
          endif
#endif
#endif
        endif
      next
    ELSE
      r:=ascan(dokumenty[mag_poz],smb_dok D_SUBDOK)
      if r=0
         aadd(dokumenty[mag_poz],NIL)
         r:=len(dokumenty[mag_poz])
         aadd(dok_par[mag_poz],{})
      else
         dok_rozch:=strtran(dok_rozch,mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_zewn :=strtran(dok_zewn, mag_biez+SMB_DOK D_SUBDOK+",",'')
#ifdef A_FA
         dok_vat  :=strtran(dok_vat , mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_fak  :=strtran(dok_fak , mag_biez+SMB_DOK D_SUBDOK+",",'')
         dok_zby  :=strtran(dok_zby , mag_biez+SMB_DOK D_SUBDOK+",",'')
#ifdef A_IZ
         dok_ewid :=strtran(dok_ewid, mag_biez+SMB_DOK D_SUBDOK+",",'')
#endif
#endif
      endif
         dokumenty[mag_poz,r]:=SMB_DOK D_SUBDOK+" "+NAZWA_DOK
         dok_par[mag_poz,r]:={P_R,ZEWN,trim(KONTO),trim(WARTOSC),lpmax,ile_kop D_FA D_KPR D_LPT D_FK D_DF D_DC D_WA}
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
#ifdef A_IZ
          if ewidencja#"Z"
             dok_ewid+=left(magazyny[mag_poz],2)+SMB_DOK D_SUBDOK+","
          endif
#endif
#endif
    ENDIF
    SKIP
  ENDDO
  use
#undef D_WA
#undef D_DC
#undef D_DF
#undef D_FK
#undef D_FA
#undef D_KPR
#undef D_LPT
#undef D_SUBDOK

return
**********************
proc reuse
local a,i,txt
field index
    close databases
    if stary_rok#NIL .and. 0=ascan(year2bckp,year(stary_rok))
       aadd(year2bckp,year(stary_rok))
    endif
    set default to (defa+if(stary_rok#NIL,str(year(stary_rok),4),"roboczy")+HB_OsPathSeparator())

          sel("DM",1)
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

#ifdef A_DDBF
   select 0
   nUSE daty
   if !used() .and. !Empty(txt:=Findfile("daty.ini"))
     a:={}
     do while inirest(@txt)
       i:=at(':=',txt)
       if i>0
         aadd(a,{left(txt,i-1),type(subs(txt,i+2)),10,2})
       endif
     enddo
     dbcreate('daty',a)
     nUSE daty
     dbappend()
     txt:="daty.ini"
     do while inirest(@txt)
       i:=at(':=',txt)
       if i>0
         txt:='DATY->'+txt
         begin sequence
           (&txt,txt:=NIL)
         end
       endif
     enddo
     UNLOCK
   endif
#else
   txt:="daty.ini"
   do while inirest(@txt)
   begin sequence
   (&txt,txt:=NIL)
   end
   enddo
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
return 'Wersja: 3.'+ltrim(str(stod(__DATE__)-stod('20100101')))
#else
return 'Wersja: 3.'
#endif
*************
