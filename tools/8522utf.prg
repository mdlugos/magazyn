//hbmk2 -debug -inc -rebuild -i../common -dSIMPLE -u+../mp/head/moj.ch -gtnul 8522utf ../../harbour/contrib/rddmisc/vfpcdx.prg ../common/error ../common/io_2 ../common/io 
//#../common/cppl852m.c ../common/cp_utf8m.c
#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"
REQUEST VFPCDX, HB_LANG_PL, HB_CODEPAGE_PL852, HB_CODEPAGE_PLWIN, HB_CODEPAGE_UTF8EX

PROCEDURE oem2utf(DB)

HB_CDPSELECT('UTF8EX')
SET(_SET_DBCODEPAGE,'PLWIN')
HB_LANGSELECT('pl')
SET DELETED ON

IF empty(db)
   db:=''
ENDIF

i:=RAT(hb_ps(),db)

SET DEFAULT TO (LEFT(DB,i))
db:=SubStr(db,i+1)

erase (set(_SET_DEFAULT)+"tmp.dbf")
erase (set(_SET_DEFAULT)+"tmp.dbt")
erase (set(_SET_DEFAULT)+"tmp.fpt")

if file(set(_SET_DEFAULT)+db)
   CHGDAT(db)
else
   AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1])})
endif
?
********************
PROCEDURE CHGDAT(DB)

LOCAL i:=0,st,f

db:=left(db,rat('.',db)-1)
erase (set(_SET_DEFAULT)+db+".cdx")
USE (DB) READONLY EXCLUSIVE CODEPAGE 'PL852'
st:=dbstruct()
? DB+":"
i:=0
do while (i:=ascan(st,{|x|x[2]$"MCQ"},++i))>0
   if st[i,3]>36 //nr ksef
      ?? "",st[i,1],st[i,2]
      st[i,2]:=left(st[i,2],1)+':U'
   elseif left(st[i,2],1)='M'
      ?? "",st[i,1],st[i,2]
      st[i,2]:='W'
   endif
enddo
   dbcreate("tmp",st) //,'VFPCDX')
   use tmp NEW
   select 1
   dbeval({||tmp->(dbappend(),aeval(st,{|x,i,y|fieldput(i,A->(fieldget(i)))}))})
   close databases
   erase (set(_SET_DEFAULT)+db+".dbf")
   erase (set(_SET_DEFAULT)+db+".dbt")
   erase (set(_SET_DEFAULT)+db+".fpt")
   rename (set(_SET_DEFAULT)+"tmp.dbf") to (set(_SET_DEFAULT)+db+".dbf")
   rename (set(_SET_DEFAULT)+"tmp.dbt") to (set(_SET_DEFAULT)+db+".dbt")
   rename (set(_SET_DEFAULT)+"tmp.fpt") to (set(_SET_DEFAULT)+db+".fpt")
return
*********************
