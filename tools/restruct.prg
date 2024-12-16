//hbmk2 restruct rddads.hbc ~/e/harbour/contrib/rddmisc/vfpcdx.prg  -gtnul

#require "rddads"
#require "vfpcdx"
#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"

//#include "ads.ch"
request HB_CODEPAGE_PL852,HB_CODEPAGE_PLWIN,HB_CODEPAGE_UTF8EX,HB_LANG_PL
REQUEST HB_LANG_PL
REQUEST VFPCDX, ADS

proc main (db, cdpfrom, cdpto)

   altd()

hb_default( @cdpfrom, 'PL852' )
hb_default( @cdpto, 'PLWIN' )
HB_CDPSELECT('UTF8EX') //some fields may be already in UTF8/16
SET( _SET_DBCODEPAGE, cdpto )
HB_LANGSELECT('PL')

hb_SetTermCP( hb_cdpTerm() )
Set(_SET_OSCODEPAGE, hb_cdpOS())

rddSetDefault('ADS')
SET SERVER LOCAL

SET HARDCOMMIT ON
SET DELETED ON

hb_default(@db,'.'+HB_ps())

i:=RAT(HB_ps(),db)
IF i=0
   db:='.'+HB_ps()+db
   i:=2
ENDIF

SET DEFAULT TO (hb_PathNormalize(LEFT(DB,i)))
db:=SubStr(db,i+1)

if lower(right(set(_SET_DEFAULT),8))=='roboczy'+HB_ps()
   SET PATH TO (hb_PathNormalize(SET(_SET_DEFAULT)+'..'+HB_ps()+'tmp'+HB_ps())+HB_OsPathListSeparator()+hb_PathNormalize(SET(_SET_DEFAULT)+'..'+HB_ps()))
else
   SET PATH TO (hb_PathNormalize(SET(_SET_DEFAULT)+'..'+HB_ps()+'roboczy'+HB_ps())+HB_OsPathListSeparator()+hb_PathNormalize(SET(_SET_DEFAULT)+'..'+HB_ps()))
endif

erase (set(_SET_DEFAULT)+"tmp.dbf")
erase (set(_SET_DEFAULT)+"tmp.dbt")
erase (set(_SET_DEFAULT)+"tmp.fpt")

if file(set(_SET_DEFAULT)+db)
   CHGDAT(db, cdpfrom, cdpto)
else
   if ! ('*'$db .or. '?'$db)
      db+="*.dbf"
   endif
   AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB),{|X|CHGDAT(X[1], cdpfrom, cdpto)})
endif
?
********************
proc CHGDAT(DB, cdpfrom, cdpto)

LOCAL h,b,f
   close databases
   f:=findfile(db)
   erase (left(f,len(f)-4)+ordbagext())
   // VFPCDX safer
   use (f) SHARED READONLY VIA "VFPCDX" CODEPAGE cdpto
   h:=dbstruct()
   use
   if !empty(h)
      aeval(h,{|x,i| if(x[1]='D_' .and. x[2]='C' .and. x[3]=8, x[2]:='B', ),;
                     if(x[2]=='M', x[2]:='W', ),;
                     if((x[1]=='POZYCJA' .or. x[1]=='LINK') .and. x[2]='C' .and. x[3]=1, (x[2]:='C', x[3]:=3) , ),;
                     if(x[2]=='C:B', (x[2]:='Q:B', x[3]:=min(x[3],254) ) , ),;
                     x[1]:=lower(x[1])})
      f:=left(db,rat('.',db)-1)
      ?? f,''
      erase (set(_SET_DEFAULT)+f+ordbagext())
      USE (DB) READONLY EXCLUSIVE VIA "VFPCDX" CODEPAGE cdpfrom
      ordlistclear()
      select 2
      dbcreate("tmp",h,,.f.,"TMP",,cdpto) //VIA ADSVFP
      select 1
      //
      dbeval({||B->(dbappend(),aeval(h,{|x,i|append_field(x,i)}))})
      close databases
      erase (set(_SET_DEFAULT)+db)
      erase (set(_SET_DEFAULT)+f+".dbt")
      erase (set(_SET_DEFAULT)+f+".fpt")
      erase (set(_SET_DEFAULT)+f+ordbagext())
      rename (set(_SET_DEFAULT)+"tmp.dbf") to (set(_SET_DEFAULT)+f+".dbf")
      rename (set(_SET_DEFAULT)+"tmp.dbt") to (set(_SET_DEFAULT)+f+".dbt")
      rename (set(_SET_DEFAULT)+"tmp.fpt") to (set(_SET_DEFAULT)+f+".fpt")
   endif
return
*********************
proc append_field(x,i)
   local f,c,t,n
   select 1
   c:=hb_FieldType(x[1])
   f:=hb_FieldGet(x[1])
   n:=hb_FieldLen(x[1])
   t:=valtype(f)
   if t='C'
      if x[2]='B' .and. n=8
         c:=HB_CDPSELECT(dbInfo(DBI_CODEPAGE))
         f:=Bin2D(hb_FieldGet(x[1]))
         HB_CDPSELECT(c)
      elseif n=1 .and. (x[1]=='pozycja' .or. x[1]=='link') .and. x[3]>1
         c:=HB_CDPSELECT(DBI_CODEPAGE)
         f:=str(hb_BCode(hb_FieldGet(x[1]))-48,x[3])
         HB_CDPSELECT(c)
      elseif !'B'$subs(c,2) .and. c<>'C' //QMWP
         f:=trim(f)
      endif
   endif
   select 2
   FieldPut(i,f)
return
********************
func findfile(x)
local a,y,b
   y:=SET(_SET_PATH)+HB_OsPathListSeparator()+SET(_SET_DEFAULT)
   b:=hb_ATokens( y, HB_OsPathListSeparator() )
   FOR EACH a IN b
      if file(y:=a+x)
         exit
      endif
      y:=''
   next
return y
*********************

#pragma BEGINDUMP
#include "hbapi.h"

HB_FUNC ( BIN2D )
{
   if( hb_parclen( 1 ) >= sizeof( double ) )
   {
      const char * buf = hb_parc( 1 );

      hb_retnd( HB_GET_LE_DOUBLE( buf ) );
   }
   else
      hb_retnd ( hb_parnd( 1 ) );
}

HB_FUNC ( D2BIN )
{
   char buf[ sizeof( double ) ];
   double d = hb_parnd( 1 );

   HB_PUT_LE_DOUBLE( buf, d );
   hb_retclen( buf, sizeof( buf ) );
}
