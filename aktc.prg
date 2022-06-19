#define MAG_BIEZ ' 1'
// linkowa† aktc,io,io_2,error,d,chgmaz,msgpl852
PROC AKTC
memvar stawki
local l:=0,h,r,f,w,i,rr:=''
field index,nazwa,nr_mag,jm_opcja,jm,STATUS,cena,cena_h,waznosc,proc_vat,info
param KASY,A1,A2,A3,A4,A5,A6,A7,A8,A9

//#include "simpleio.ch"
#ifndef SIMPLE
  #define SIMPLE
#endif

#command ? [<List,...>] => ?? Chr(13)+Chr(10)[;?? <List>]
#command ?? [<xn,...>] => [outerr(<xn>)]
#command Q [<List,...>] => QQ Chr(13)+Chr(10)[;QQ <List>]
#command QQ [<xn,...>] => [outstd(<xn>)]
#command EJECT => QQ ""

SET PATH TO (GETENV("MAGDEF")+"roboczy\;"+GETENV("MAGDEF"))
SET DEFAULT TO .\

? 'AKTUALIZACJA CENNIKA DLA KAS.'

i:=getlines(memoread(findfile("kasa.ini")))
w:=ascan(i,'stawki')

IF w=0
   ? 'Brak stawek VAT.'
   QUIT
ENDIF

(&(i[w]),w:=NIL)


USE indx_mat NEW SHARED
#ifdef A_CDX
SET ORDER TO TAG INDX_NUM
#else
SET INDEX TO indx_num
#endif

aeval(directory('aktc.?'),{|x|ferase(x[1])})

IF FILE('cennik.dbf') .and. FILE('index.ntx')

   ERASE z_ce.dbf
   RENAME cennik.dbf TO z_ce.dbf
   ERASE z_na.ntx
   RENAME nazwa.ntx TO z_na.ntx
   ERASE z_in.ntx
   RENAME index.ntx TO z_in.ntx
   IF FILE('cennik.dbf')
      ? 'Cennik zablokowany.'
      QUIT
   ENDIF
ENDIF


dbcreate('cennik',;
     { {'INDEX','C',12,0},;
       {'NAZWA','C',40,0},;
       {'JM','C',4,0},;
       {'CENA','N',8,2},;
       {'CENA_H','N',8,2},;
       {'STAWKA','C',1,0},;
       {'WAZNOSC','N',4,0},;
       {'PROMOCJA','L',1,0},;
       {'TANDEM','C',1,0} })

USE CENNIK NEW EXCLUSIVE

SELECT INDX_MAT

SET FILTER TO status>0 .OR. INDEX='B'
SEEK MAG_BIEZ

?
?

DO WHILE !EOF() .and. NR_MAG+INDEX<MAG_BIEZ+'C'
   IF ++l%100=0
   ?? chr(13),INDEX,NAZWA,jm,cena,proc_vat
   ENDIF
   SELECT CENNIK
          APPEND  BLANK
          REPLACE INDEX    WITH INDX_MAT->INDEX
          REPLACE nazwa    WITH INDX_MAT->nazwa
          REPLACE jm       WITH INDX_MAT->(IF(INFO='P' .and. jm='szt.','szt',jm))
          REPLACE cena     WITH INDX_MAT->cena
          REPLACE cena_h   WITH INDX_MAT->cena1
          REPLACE stawka   WITH if(INDX_MAT->proc_vat='zw','Z',{'A','B','C','D','E','F','G','H'}[ascan(stawki,INDX_MAT->proc_vat)])
          REPLACE WAZNOSC  WITH INDX_MAT->waznosc
          REPLACE TANDEM   WITH INDX_MAT->tandem
          REPLACE PROMOCJA WITH INDX_MAT->shortname='+'

   SELECT INDX_MAT
   IF status<>1 .and. reclock()
     REPLACE status WITH 1
   ENDIF
   SKIP
ENDDO


USE z_ce EXCLUSIVE INDEX z_in

SELECT CENNIK
?
INDEX ON INDEX TO index EVAL {||outerr('.'),.t.} EVERY 100
?
//INDEX ON UpP(NAZWA) TO nazwa EVAL {||outerr('.'),.t.} EVERY 100
ordcreate('nazwa',,'UPPER(NAZWA)',{||UpP(nazwa)})

*******************************************************
SET INDEX TO INDEX

SET RELATION TO INDEX INTO Z_CE
SET FILTER TO cena<>Z_CE->cena
COPY TO zmiany

SET RELATION TO
*******************************************************
? 'AKTUALIZACJA CENNIKA DLA WAG.'

begin sequence
SET INDEX TO INDEX
SET FILTER TO {||MESSAGE(100), JM='kg  ' .AND. SUBS(INDEX,7)=' '}
SEEK '00'

SELECT Z_CE
//USE Z_CE NEW EXCLUSIVE INDEX Z_IN
SET FILTER TO {||MESSAGE(100), JM='kg  '.AND. SUBS(INDEX,7)=' '}
SEEK '00'

DO WHILE CENNIK->INDEX='00' .or. Z_CE->INDEX='00'

   IF CENNIK->INDEX=Z_CE->INDEX;
              .AND. CENNIK->NAZWA=LEFT(Z_CE->NAZWA,30);
              .AND. CENNIK->CENA=Z_CE->CENA;
              .and. CENNIK->waznosc=Z_CE->waznosc
      CENNIK->(DBSKIP(1))
      Z_CE->(DBSKIP(1))
      LOOP
   ELSEIF CENNIK->INDEX#'00' .OR. CENNIK->INDEX>Z_CE->INDEX .AND. Z_CE->INDEX='00' //KASOWANIE
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

      IF Z_CE->INDEX#'00' .OR. Z_CE->INDEX>CENNIK->INDEX .AND. CENNIK->INDEX='00' //DOPISANIE
         R+='S' //STWORZENIE
      ELSE //ZMIANA
         R+='Z'
         Z_CE->(DBSKIP(1))
      ENDIF
      CENNIK->(DBSKIP(1))

   ENDIF
   Q r
   rr+=chgmaz(R)+CHR(10)

ENDDO

if !empty(rr)
   EJECT
endif

r:=dtos(date())+time()
f:=subs(r,4,1)+{'1','2','3','4','5','6','7','8','9','A','B','C'}[VAL(SUBS(r,5,2))]+subs(r,7,4)+subs(r,12,2)

FOR I:=1 TO PCOUNT()-1

w:=&('A'+STR(I,1))+f

h:=fcreate(w+'.DAT')
fwrite(h,rr)
fclose(h)
fclose(fcreate(w+'.OUT'))

NEXT
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
