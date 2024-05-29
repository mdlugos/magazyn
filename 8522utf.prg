#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"
PROCEDURE oem2utf(DB)

REQUEST HB_LANG_PL
HB_LANGSELECT('PL')
request HB_CODEPAGE_PL852
request HB_CODEPAGE_UTF8EX
HB_CDPSELECT('UTF8EX')
SET(_SET_DBCODEPAGE,'PL852')

request DBFCDX
IF empty(db)
   db:=''
ENDIF
SET DELETED ON
SET DEFAULT TO (LEFT(DB,RAT('\',DB)))
db:=subs(db,RAT('\',DB)+1)
AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1])})
?
********************
PROCEDURE CHGDAT(DB)

LOCAL I:=0,st,f

i:=fopen(set(_SET_DEFAULT)+db,64)
st:=space(1)
fread(i,@st,1)
fclose(i)
db:=left(db,rat('.',db)-1)
if HB_BCODE(st)==0xf5
   rddSetDefault('DBFCDX')
endif
if rddSetDefault()=='DBFCDX'
   erase (set(_SET_DEFAULT)+db+".cdx")
endif
USE (DB)
st:=dbstruct()
? DB+":"
i:=0
f:=.f.
do while (i:=ascan(st,{|x|x[2]="M"},++i))>0
  ?? " "+st[i,1]
  st[i,2]:='W'
  f:=.t.
enddo
if f
   dbcreate("tmp",st)
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
