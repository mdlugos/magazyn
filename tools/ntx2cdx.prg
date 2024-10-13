//hbmk2 ntx2cdx -gtnul
#include "simpleio.ch"
#include "set.ch"
request DBFCDX
PROCEDURE mdx2cdx(DB)
Local i:=0
request HB_CODEPAGE_PL852,HB_CODEPAGE_UTF8EX,HB_LANG_PL
HB_CDPSELECT('UTF8EX') //some fields may be already in UTF16
SET(_SET_DBCODEPAGE,'PL852')
REQUEST HB_LANG_PL
HB_LANGSELECT('PL')

rddSetDefault('DBFCDX')

SET DELETED ON

IF empty(db)
   db:=''
ENDIF
i:=RAT(hb_ps(),db)

SET DEFAULT TO (LEFT(DB,i))
db:=subs(db,i+1)

erase (set(_SET_DEFAULT)+"tmp.dbf")
erase (set(_SET_DEFAULT)+"tmp.dbt")
erase (set(_SET_DEFAULT)+"tmp.fpt")

altd()

if file(set(_SET_DEFAULT)+db)
   CHGDAT(db)
else
   AEVAL(DIRECTORY(SET(_SET_DEFAULT)+DB+"*.dbf"),{|X|CHGDAT(X[1])})
endif
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
   //db:=alias() keep it in the same case
   ?? DB+" "
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
