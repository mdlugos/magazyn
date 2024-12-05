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

IF empty(db)
   db:='.\'
ENDIF
i:=RAT(HB_ps(),db)

SET DEFAULT TO (LEFT(DB,i))
db:=SubStr(db,i+1)

if lower(right(set(_SET_DEFAULT),8))=='roboczy'+HB_ps()
   SET PATH TO (SET(_SET_DEFAULT)+'..'+HB_ps()+'tmp'+HB_ps()+HB_OsPathListSeparator()+SET(_SET_DEFAULT)+'..'+HB_ps())
else
   SET PATH TO (SET(_SET_DEFAULT)+'..'+HB_ps()+'roboczy'+HB_ps()+HB_OsPathListSeparator()+SET(_SET_DEFAULT)+'..'+HB_ps())
endif

erase (set(_SET_DEFAULT)+"tmp.dbf")
erase (set(_SET_DEFAULT)+"tmp.dbt")
erase (set(_SET_DEFAULT)+"tmp.fpt")

if file(set(_SET_DEFAULT)+db)
   CHGDAT(db, cdpfrom, cdpto)
else
   AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1], cdpfrom, cdpto)})
endif
?
********************
PROCEDURE CHGDAT(DB, cdpfrom, cdpto)

LOCAL h,b,f
   close databases
   f:=findfile(db)
   erase (left(f,len(f)-4)+ordbagext())
   use (f) SHARED READONLY VIA "VFPCDX" CODEPAGE cdpto
   h:=dbstruct()
   use
   if !empty(h)
      aeval(h,{|x,i| if(x[1]='D_' .and. x[2]='C' .and. x[3]=8, x[2]:='B', ),;
                     if(x[2]=='M', x[2]:='W', ),;
                     if((x[1]=='POZYCJA' .or. x[1]=='LINK') .and. x[2]='C' .and. x[3]=1, x[3]:=3, ),;
               x[1]:=lower(x[1])})
      db:=lower(left(db,rat('.',db)-1))
      ?? db,''
      erase (set(_SET_DEFAULT)+db+ordbagext())
      USE (DB) READONLY EXCLUSIVE VIA "VFPCDX" CODEPAGE cdpfrom
      ordlistclear()
      select 2
      dbcreate("tmp",h,,.f.,"TMP",,cdpto)
      select 1
      //
      dbeval({||B->(dbappend(),aeval(h,{|x,i,f,c,t,n|A->(f:=hb_FieldGet(x[1]),n:=hb_FieldLen(x[1]),t:=valtype(f)),iif(t='C',(;
            iif(x[2]='B' .and. n=8, A->(c:=HB_CDPSELECT(cdpfrom),f:=Bin2D(hb_FieldGet(x[1])),HB_CDPSELECT(c)), ),;
            iif(n=1 .and. (x[1]=='pozycja' .or. x[1]=='link') .and. x[3]>1, A->(c:=HB_CDPSELECT(cdpfrom),f:=str(hb_BCode(hb_FieldGet(x[1]))-48,x[3]),HB_CDPSELECT(c)), ),;
            f:=trim(f)),),FieldPut(i,f)}))})
      close databases
      erase (set(_SET_DEFAULT)+db+".dbf")
      erase (set(_SET_DEFAULT)+db+".dbt")
      erase (set(_SET_DEFAULT)+db+".fpt")
      erase (set(_SET_DEFAULT)+db+ordbagext())
      rename (set(_SET_DEFAULT)+"tmp.dbf") to (set(_SET_DEFAULT)+db+".dbf")
      rename (set(_SET_DEFAULT)+"tmp.dbt") to (set(_SET_DEFAULT)+db+".dbt")
      rename (set(_SET_DEFAULT)+"tmp.fpt") to (set(_SET_DEFAULT)+db+".fpt")
   endif
return
*********************
func findfile(x)
   local a,l,y:=""
    if (HB_ps()$x)
       if file(x)
          y:=x
   #ifdef __PLATFORM__UNIX
       elseif file(l:=strtran(x,'\','/'))
          y:=l
   #endif
       else
          x:=SubStr(x,rat(HB_ps(),x)+1)
       endif
    endif
    if y==""
       FOR EACH a IN hb_ATokens( SET(_SET_PATH)+HB_OsPathListSeparator()+SET(_SET_DEFAULT), HB_OsPathListSeparator() )
             if file(y:=a+x)
               exit
             endif
   #ifdef __PLATFORM__UNIX
             if file(y:=strtran(a+x,'\','/'))
                exit
             endif
   #endif
             y:=''
       next
    endif
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
