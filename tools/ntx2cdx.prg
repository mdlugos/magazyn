#include "simpleio.ch"
#include "set.ch"
request DBFCDX
PROCEDURE mdx2cdx(DB)

REQUEST HB_LANG_PL
HB_LANGSELECT('PL')
request HB_CODEPAGE_PL852
HB_CDPSELECT('PL852')
SET(_SET_DBCODEPAGE,'PL852')
   
IF empty(db)
   db:=''
ENDIF
SET DELETED ON
SET DEFAULT TO (LEFT(DB,RAT(hb_ps(),DB)))
db:=subs(db,RAT(hb_ps(),DB)+1)
AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1])})
?
********************
PROCEDURE CHGDAT(DB)

LOCAL h,st
h:=fopen(set(_SET_DEFAULT)+db,64)
st:=' '
fread(h,@st,1)
fclose(h)
if HB_BCODE(st)<>3
   db:=left(db,rat('.',db)-1)
   erase (set(_SET_DEFAULT)+db+".cdx")
   USE (DB) READONLY
   st:=dbstruct()
   db:=alias()
   ?? DB+" "
   dbcreate("tmp",st,"DBFCDX")
   use tmp VIA "DBFCDX" NEW
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