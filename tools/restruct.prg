//hbmk2 restruct rddads.hbc ../../harbour/contrib/rddmisc/vfpcdx.prg  -gtnul

#require "rddads"
#require "vfpcdx"
#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"
#include "fileio.ch"

//#include "ads.ch"
request HB_CODEPAGE_PL852,HB_CODEPAGE_PLWIN,HB_CODEPAGE_UTF8EX,HB_LANG_PL
REQUEST HB_LANG_PL
REQUEST VFPCDX, ADS

proc main (db, cdpfrom, cdpto)
local a,b
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

a:=HB_ps()+curdir()+HB_ps()
#ifndef __PLATFORM__UNIX
  if a<>HB_ps()+HB_ps()
   a:=DiskName()+HB_OsDriveSeparator()+a
  endif
#endif

hb_default(@db,a)

i:=RAT(HB_ps(),db)
IF i=0
   db:=a+db
   i:=len(a)
ENDIF

a:=hb_PathNormalize(LEFT(DB,i))

SET DEFAULT TO (a)
db:=SubStr(db,i+1)

if lower(right(a,8))=='roboczy'+HB_ps()
   SET PATH TO (hb_PathNormalize(a+'..'+HB_ps()+'tmp'+HB_ps())+HB_OsPathListSeparator()+hb_PathNormalize(a+'..'+HB_ps()))
else
   SET PATH TO (hb_PathNormalize(a+'..'+HB_ps()+'roboczy'+HB_ps())+HB_OsPathListSeparator()+hb_PathNormalize(a+'..'+HB_ps()))
endif

#ifdef __PLATFORM__UNIX
   AEVAL(DIRECTORY(a),{|x,y|y:=lower(x[1]),if(x[1]==y,,frename(a+x[1],a+y))})
   db:=lower(db)
#endif   

erase (a+"tmp.dbf")
erase (a+"tmp.dbt")
erase (a+"tmp.fpt")

if file(a+db)
   CHGDAT(a, db, cdpfrom, cdpto)
else
   if ! ('*'$db .or. '?'$db)
      db+="*.dbf"
   endif
   AEVAL(DIRECTORY(a+db),{|X|CHGDAT(a, X[1], cdpfrom, cdpto)})
endif
?
********************
proc CHGDAT(a, DB, cdpfrom, cdpto)

LOCAL h,f,b,c
   close databases
   f:=findfile(db)
#ifdef __PLATFORM__UNIX
   if f=a
      //found itself. Next try with upper case 
      h:=findfile(upper(db)) 
      if ''<>h
         f:=h
      endif
   endif
   erase (left(f,len(f)-4)+upper(ordbagext()))
#endif
   erase (left(f,len(f)-4)+ordbagext())
   ? db,f

// VFPCDX safer
   use (f) SHARED READONLY VIA "VFPCDX" CODEPAGE cdpto
   h:=dbstruct()
   use
   if !empty(h)
      f:=left(db,rat('.',db)-1)
      erase (a+f+ordbagext())
#ifdef __PLATFORM__UNIX
      erase (a+f+upper(ordbagext()))
      rename (a+f+'.DBT') TO (a+f+'.dbt')
      rename (a+f+'.FPT') TO (a+f+'.fpt')
#endif
#if 0
      b:=fopen(a+db,FO_READWRITE + FO_EXCLUSIVE)
      c:=HB_BCHAR(0)
      fread(b,@c,1)
      if HB_BCODE(c)==0x8B
         c:=HB_BCHAR(0x83)
         FSeek(b,0)
         fwrite(b,c,1)
      endif
      fclose(b)
#endif
      USE (DB) READONLY EXCLUSIVE VIA 'VFPCDX' CODEPAGE cdpfrom
      ordlistclear()

      aeval(h,{|x,i,j| if(x[1]='D_' .and. x[2]='C' .and. x[3]=8, x[2]:='B', ),;
                     if(x[2]=='M', x[2]:='W', ),;
                     if((x[1]=='POZYCJA' .or. x[1]=='LINK') .and. x[2]='C' .and. x[3]=1, (x[2]:='C', x[3]:=3) , ),; //binary flag stripped
                     if(x[2]=='C:B', (x[2]:='Q:B', x[3]:=min(x[3],254) ) , ),;
                     i:=hb_FieldLen(x[1]),;
                     if(x[2]=='N'.and.i>0,(if((i:=hb_FieldDec(x[1]))>0,++i,i:=0),if((j:=x[4])>0,++j,),x[3]:=j+max(x[3]-j,hb_FieldLen(x[1])-i)),),;
                     if(x[2] ='C'.and.i>0,x[3]:=max(x[3],i),),;
                     x[1]:=lower(x[1])})
      select 2
      dbcreate("tmp",h,,.f.,"TMP",,cdpto) //VIA ADSVFP
      select 1
      //
      dbeval({||B->(dbappend(),aeval(h,{|x,i|append_field(x,i)}))})
      close databases
      erase (a+db)
      erase (a+f+".dbt")
      erase (a+f+".fpt")
      rename (a+"tmp.dbf") to (a+f+".dbf")
      rename (a+"tmp.dbt") to (a+f+".dbt")
      rename (a+"tmp.fpt") to (a+f+".fpt")
   endif
return
*********************
proc append_field(x,i)
   local f,c,t,n
   select 1
   c:=hb_FieldType(x[1])

   if empty(c) //no field
      select 2
      return
   endif
   
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
      elseif !'B'$hb_BSubStr(c,2) .and. c<>'C' //QMWP
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
