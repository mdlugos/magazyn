//hbmk2 -debug -inc -rebuild -i../common -dSIMPLE -u+../mp/head/moj.ch -gtnul 8522utf ../../harbour/contrib/rddmisc/vfpcdx.prg ../common/error ../common/io_2 ../common/io 
#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"
PROCEDURE oem2utf(DB)
   local i
/* bo załadowane 
REQUEST HB_LANG_PL
HB_LANGSELECT('PL')
request HB_CODEPAGE_PL852
request HB_CODEPAGE_UTF8EX
HB_CDPSELECT('UTF8EX')
SET(_SET_DBCODEPAGE,'PL852')

request DBFCDX
*/
//SET(_SET_CODEPAGE,'PL852')
SET DELETED ON

IF empty(db)
   db:=DiskName()+HB_OsDriveSeparator()+HB_ps()+curdir(DiskName())+HB_ps()
   #ifdef __PLATFORM__WINDOWS
   if curdir()=HB_ps()
     db:=HB_ps()+curdir()+HB_ps()
   endif
   #endif   
ENDIF
? db
i:=RAT(hb_ps(),DB)
SET DEFAULT TO (LEFT(DB,i))
db:=SubStr(db,i+1)

erase (set(_SET_DEFAULT)+"tmp.dbf")
erase (set(_SET_DEFAULT)+"tmp.dbt")
erase (set(_SET_DEFAULT)+"tmp.fpt")

AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1])})
?
********************
PROCEDURE CHGDAT(DB)

LOCAL I:=0,st,f

erase (set(_SET_DEFAULT)+db+".cdx")
i:=fopen(set(_SET_DEFAULT)+db,64)
st:=space(1)
fread(i,@st,1)
fclose(i)
db:=left(db,rat('.',db)-1)
USE (DB)
st:=dbstruct()
? DB+":"
i:=0
f:=.f.
do while (i:=ascan(st,{|x|x[2]$"MC"},++i))>0
   if st[i,3]>36 //nr ksef
      ?? " "+st[i,1]
      st[i,2]:='C:U'
      f:=.t.
   elseif st[i,2]='M'
      ?? " "+st[i,1]
      st[i,2]:='M:U'
      f:=.t.
   endif
enddo
if f
   dbcreate("tmp",st) //,'VFPCDX')
   use tmp NEW
   select 1
   dbeval({||tmp->(dbappend(),aeval(st,{|x,i|fieldput(i,A->(fieldget(i)))}))})
   close databases
   erase (set(_SET_DEFAULT)+db+".dbf")
   erase (set(_SET_DEFAULT)+db+".dbt")
   erase (set(_SET_DEFAULT)+db+".fpt")
   rename (set(_SET_DEFAULT)+"tmp.dbf") to (set(_SET_DEFAULT)+db+".dbf")
   rename (set(_SET_DEFAULT)+"tmp.dbt") to (set(_SET_DEFAULT)+db+".dbt")
   rename (set(_SET_DEFAULT)+"tmp.fpt") to (set(_SET_DEFAULT)+db+".fpt")
endif
return
*********************
