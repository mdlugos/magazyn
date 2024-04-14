REQUEST HB_CODEPAGE_PLWIN

#define MAG_BIEZ ' 1'
// hbmk2 -static -prgflag=-dSIMPLE -gtnul -u+polok.ch aktc io io_2 error
PROC AKTC
memvar stawki
local l:=0,h,o,r,f,w,i,rr:=''
field index,nazwa,nr_mag,jm_opcja,jm,STATUS,cena,cena_h,waznosc,proc_vat,info
param KASY,A1,A2,A3,A4,A5,A6,A7,A8,A9

//#include "simpleio.ch"
#ifndef SIMPLE
  #define SIMPLE
#endif

#command ? [<List,...>] => ?? HB_EOL()[;?? <List>]
#command P [<List,...>] => PP HB_EOL()[;PP <List>]
#command PP [<xn,...>] => [QQ <xn>]
#command Q [<List,...>] => QQ HB_EOL()[;QQ <List>]

#ifdef __PLATFORM__UNIX
#command ?? [<xn,...>] => [OuterR(HB_TRANSLATE(TRAN((<xn>),),,'UTF8'))]
#command QQ [<xn,...>] => [OutstD(HB_TRANSLATE(TRAN((<xn>),),,'UTF8'))]
#else
#command ?? [<xn,...>] => [OuterR(<xn>)]
#command QQ [<xn,...>] => [OutstD(<xn>)]
#endif

#command EJECT => QQ chr(12)
#command ACCEPT TO <idVar>                                              ;
      => <idVar> := StrTran( FReadStr(0, 256), HB_EOL() )
#command ACCEPT <cPrompt> TO <idVar>                                    ;
      => ? <cPrompt>                                                    ;
       ; ACCEPT TO <idVar>

SET PATH TO (GETENV("MAGDEF")+"roboczy"+HB_OsPathSeparator()+HB_OsPathListSeparator()+GETENV("MAGDEF"))
SET DEFAULT TO ('.'+HB_OsPathSeparator())

? 'AKTUALIZACJA CENNIKA DLA KAS.'

i:=getlines(memoread(findfile("kasa.ini")))
w:=ascan(i,'stawki')

IF w=0
   ? 'Brak stawek VAT.'
   ERRORLEVEL( 1)
   QUIT
ENDIF

(&(i[w]),w:=NIL)

USE firmy READONLY SHARED
#ifdef A_CDX
SET ORDER TO firm_num
#else
SET INDEX TO firm_num
#endif

o := {}
EXEC {|a|a:={'kh_id' => numer_kol, 'name' => hb_translate(trim(nazwa),,'UTF8')}, if(empty(longname),,a['longname'] := hb_translate(trim(longname),,'UTF8')), hb_hautoadd(a , .t.), if(empty(ident),,a['NIP'] := trim(ident)), if(empty(adres),,a['address'] := hb_translate(trim(adres),,'UTF8')), /*if(empty(konto),,a['konto'] := trim(konto)),*/ if(empty(subs(nazwisko,5)),,a['nazwisko'] := hb_translate(trim(nazwisko),,'UTF8')), if(empty(cennik),,a['price'] := trim(cennik)), if(empty(subs(uwagi,5)),,a['note'] := hb_translate(trim(uwagi),,'UTF8')), aadd(o, a)}

hb_memowrit('firmy.json',hb_jsonencode({'data' => hb_dtoc(date(),"YYYY-MM-DD")+" "+time(),'firmy' => o}))

USE indx_mat SHARED
#ifdef A_CDX
SET ORDER TO TAG INDX_NUM
#else
SET INDEX TO indx_num
#endif

aeval(directory('aktc.?'),{|x|ferase(x[1])})

IF f:=(FILE('cennik.dbf') .and. FILE('index.ntx'))

   ERASE z_ce.dbf
   RENAME cennik.dbf TO z_ce.dbf
   ERASE z_na.ntx
   RENAME nazwa.ntx TO z_na.ntx
   ERASE z_in.ntx
   RENAME index.ntx TO z_in.ntx
   IF FILE('cennik.dbf')
      ? 'Cennik zablokowany.'
      ERRORLEVEL( 1)
      QUIT
   ENDIF
ENDIF

h := {}
o := {}

dbcreate('cennik',;
     { {'INDEX','C',12,0},;
       {'NAZWA','C',40,0},;
       {'JM','C',4,0},;
       {'CENA','N',8,2},;
       {'CENA_H','N',8,2},;
       {'RABAT','N',6,2},;
       {'STAWKA','C',1,0},;
       {'WAZNOSC','N',4,0},;
       {'PROMOCJA','L',1,0},;
       {'TANDEM','C',12,0} })

USE cennik NEW EXCLUSIVE

USE main NEW READONLY SHARED
#ifdef A_CDX
SET ORDER TO TAG MAIN_IND
#else
SET INDEX TO main_ind
#endif

SELECT INDX_MAT

SET FILTER TO status>0 .OR. INDEX='B'
SEEK MAG_BIEZ

?
?  'rabaty:'
?

DO WHILE !EOF() .and. NR_MAG+INDEX<MAG_BIEZ+'C'
   message(100)
   outerr(chr(13),INDEX,HB_TRANSLATE(NAZWA,,'UTF8'))
   i:=ascan(stawki,INDX_MAT->proc_vat)
   IF INDX_MAT->proc_vat<>'zw' .and. i=0
      i:=index+' '+nazwa+' '+proc_vat+' %'
      CLOSE ALL
      ERASE cennik.dbf
      ERRORLEVEL( 1)
      ?
      ? "UWAGA - ZLA STAWKA VAT:"
      ? 
      ?
      ? i
      ?
      ?
      alarm(i)
      QUIT
   ENDIF
    
    SELECT CENNIK

          APPEND  BLANK
          REPLACE INDEX    WITH INDX_MAT->INDEX
          REPLACE nazwa    WITH INDX_MAT->nazwa
          REPLACE jm       WITH INDX_MAT->(IF(INFO='P' .and. jm='szt.','szt',jm))
          REPLACE cena     WITH INDX_MAT->cena
          REPLACE cena_h   WITH INDX_MAT->cena1
          REPLACE rabat    WITH IF(indx_mat->zaznacz=5.or.INDX_MAT->waznosc>0,100*(INDX_MAT->cena-INDX_MAT->cena2)/INDX_MAT->cena,0)
          REPLACE stawka   WITH IF(INDX_MAT->proc_vat='zw','Z',CHR(64+i))
          REPLACE WAZNOSC  WITH INDX_MAT->waznosc
          REPLACE TANDEM   WITH INDX_MAT->tandem
          REPLACE PROMOCJA WITH INDX_MAT->shortname='+'
    IF rabat<>0
      outerr(' ',RABAT,'%',chr(10))
    ENDIF

    SELECT MAIN
    SEEK INDX_MAT->nr_mag+INDX_MAT->index+dtos(date()-31)

    r:={array(32),array(32)}
    afill(r[1],0)
    afill(r[2],0)

    IF INDX_MAT->nr_mag+INDX_MAT->index == nr_mag+index .and. data<=date()
      EXEC {||r[if(ilosc>0,1,2)][(date()-data)+1]+=abs(ilosc)} FOR {||ilosc<>0} WHILE {||nr_mag+index == INDX_MAT->nr_mag+INDX_MAT->index .and. data<=date()}
      aadd(o, {'index' => trim(indx_mat->index), 'przychody32' => r[1], 'rozchody32' => r[2]})
    ENDIF

    SELECT INDX_MAT

    aadd(h, {'index' => trim(index), 'nazwa' => hb_translate(trim(nazwa),,'UTF8'), 'jm' => HB_TRANSLATE(trim(jm),,'UTF8'), 'cena' => cena, 'kod' => kod, 'cena_h' => cena1, 'rabat' => cennik->rabat, 'proc_vat' => proc_vat, 'waznosc' => waznosc, 'tandem' => trim(tandem), 'promocja' => cennik->promocja, 'stan_rano' => stan - r[1,1] + r[2,1], 'stan' => stan, 'przychod_7dni' => r[1,2]+r[1,3]+r[1,4]+r[1,5]+r[1,6]+r[1,7]+r[1,8], 'rozchod_7dni' => r[2,2]+r[2,3]+r[2,4]+r[2,5]+r[2,6]+r[2,7]+r[2,8]})

   IF status<>1 .and. reclock()
     REPLACE status WITH 1
     dbrunlock()
   ENDIF
   SKIP
ENDDO

hb_memowrit('cennik.json',hb_jsonencode({'data' => hb_dtoc(date(),"YYYY-MM-DD")+" "+time(),'cennik' => h}))
hb_memowrit('obroty.json',hb_jsonencode({'data' => hb_dtoc(date(),"YYYY-MM-DD")+" "+time(),'obroty' => o}))

o := h := NIL

SELECT CENNIK
?
? 'Skorowidze:'
?
INDEX ON UPPER(NAZWA) TO nazwa EVAL {||outerr('.'),.t.} EVERY 100
?
INDEX ON INDEX TO index EVAL {||outerr('.'),.t.} EVERY 100
//ordcreate('nazwa',,'UPPER(NAZWA)',{||UpP(nazwa)})

*******************************************************
SET INDEX TO index


SELECT INDX_MAT

IF f
   USE z_ce EXCLUSIVE INDEX z_in


SELECT CENNIK

SET RELATION TO INDEX INTO Z_CE
SET FILTER TO Z_CE->cena<>CENNIK->cena
COPY FIELDS index,cena,cena_h TO zmiany

USE zmiany NEW EXCLUSIVE
SET RELATION TO INDEX INTO Z_CE
replace cena_h with z_ce->cena ALL


FOR i:=1 TO len(MEMVAR->kasy)
  w:='zmiany'+subs(MEMVAR->kasy,i,1)
  if file(w+'.dbf')
  else
    dbcreate(w,;
     { {'INDEX','C',12,0},;
       {'CENA','N',8,2},;
       {'CENA_H','N',8,2},;
       {'CZAS','C',19,0} })
  endif
  USE (w) EXCLUSIVE
  w:=lastrec()+1
  APPEND FROM zmiany
  goto (w)
  replace czas with dtoc(date())+' '+time() rest
NEXT i

endif

USE

SELECT CENNIK

SET RELATION TO
*******************************************************
?
? 'AKTUALIZACJA CENNIKA DLA WAG.'
?

begin sequence
//SET INDEX TO INDEX
SET FILTER TO {||MESSAGE(100), JM='kg  ' .AND. SUBS(INDEX,7)=' '}
SEEK '00'

if f

   SELECT Z_CE
   //USE Z_CE NEW EXCLUSIVE INDEX Z_IN
   SET FILTER TO {||MESSAGE(100), JM='kg  '.AND. SUBS(INDEX,7)=' '}
   SEEK '00'

endif

DO WHILE CENNIK->INDEX='00' .or. f .and. Z_CE->INDEX='00'

   IF f .and. CENNIK->INDEX=Z_CE->INDEX;
              .AND. CENNIK->NAZWA=LEFT(Z_CE->NAZWA,30);
              .AND. CENNIK->CENA=Z_CE->CENA;
              .and. CENNIK->waznosc=Z_CE->waznosc
      CENNIK->(DBSKIP(1))
      Z_CE->(DBSKIP(1))
      LOOP
   ELSEIF CENNIK->INDEX#'00' .OR. f .and. CENNIK->INDEX>Z_CE->INDEX .AND. Z_CE->INDEX='00' //KASOWANIE
      SELECT Z_CE

      r:='  1'+;
         LEFT(NAZWA,30)+;
         STR(CENA*100,6)+;
         '28'+;
         SUBS(INDEX,2,5)+;
         SUBS(INDEX,3,4)+;
         STR(0,4)+;
         '0K'

      Z_CE->(DBSKIP(1))
   ELSE
      SELECT CENNIK

      r:='  1'+;
         LEFT(NAZWA,30)+;
         STR(CENA*100,6)+;
         '28'+;
         SUBS(INDEX,2,5)+;
         SUBS(INDEX,3,4)+;
         STR(WAZNOSC,4)+;
         '0'

      IF !f .or. Z_CE->INDEX#'00' .OR. Z_CE->INDEX>CENNIK->INDEX .AND. CENNIK->INDEX='00' //DOPISANIE
         R+='S' //STWORZENIE
      ELSE //ZMIANA
         R+='Z'
         Z_CE->(DBSKIP(1))
      ENDIF
      CENNIK->(DBSKIP(1))

   ENDIF
   Q r
   rr+=HB_TRANSLATE(r,,'PLWIN')+CHR(10) //chgmaz(R)

ENDDO

if !empty(rr)
   EJECT

r:=dtos(date())+time()
f:=subs(r,4,1)+{'1','2','3','4','5','6','7','8','9','A','B','C'}[VAL(SUBS(r,5,2))]+subs(r,7,4)+subs(r,12,2)

FOR I:=1 TO PCOUNT()-1

w:=&('A'+STR(I,1))
h := directory(w+'*.dat')
if empty(h)
   h:=fcreate(w+f+'.dat')
else
   h:=fopen(w+h[1,1], 2)
   fseek(h,0,2)
endif

fwrite(h,rr)
fclose(h)
//fclose(fcreate(w+'.out'))

NEXT
endif

end sequence

CLOSE DATABASES
#ifdef A_ADS
  AdsClrCallBack()
#endif

FOR i:=1 TO len(MEMVAR->kasy)
fclose(fcreate('aktc.'+subs(MEMVAR->kasy,i,1)))
NEXT i
?
? 'KONIEC'
?
RETURN
/***********************
#ifdef __HARBOUR__
#pragma BEGINDUMP
#include "hbapi.h"

HB_FUNC ( CHGMAZ )
      {
         char * s = (char *) hb_parc( 1 );
         char * ret;
         size_t l = hb_parclen( 1 ), i;
         const char * f = "€‚ƒ„…æ‡³‰Š‹ŒŽ‡‘’“”•–¦¶™š›œ£žŸ ¡ó£¡±¦§Êêª¼¬­®¯°±²³´µ¶·¸¹º»¼¯¿¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÓáâÑñåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ";

         if ( l > 0 )
         {
         ret = (char *) hb_xgrab(l);

         for( i=0 ; (i < l) ; i++ )
         {
            if ( (unsigned char) s[i] >= (unsigned char) 128 )
            {
               ret[i] = f[ (unsigned char) s[i] - (unsigned char) 128 ];
            } else {
               ret[i] = s[i];
            }
         }
         hb_retclen( ret, i);
         hb_xfree( ret );
         } else { hb_retclen( s, l); }
      }
#pragma ENDDUMP
#endif
*********************/
