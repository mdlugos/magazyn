//hbmk2 vfp2ads \harbour\contrib\rddmisc\vfpcdx.prg rddads.hbc -gtnul
#include "simpleio.ch"
#include "set.ch"
#include "dbinfo.ch"
REQUEST VFPCDX,ADS
PROCEDURE vfp2ads(DB)
Local i:=0
request HB_CODEPAGE_PL852,HB_CODEPAGE_UTF8EX,HB_LANG_PL
HB_CDPSELECT('UTF8EX') //some fields may be already in UTF8/16
SET(_SET_DBCODEPAGE,'PL852')
REQUEST HB_LANG_PL
HB_LANGSELECT('PL')

rddSetDefault('ADS')
SET SERVER LOCAL
SET FILETYPE TO VFP

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
static proc CHGDAT(DB)
LOCAL h,st

//h:=fopen(set(_SET_DEFAULT)+db,64)
//st:=' '
//fread(h,@st,1)
//fclose(h)
//if HB_BCODE(st)<>3
   db:=left(db,rat('.',db)-1)
   erase (set(_SET_DEFAULT)+db+".cdx")
   USE (DB) READONLY EXCLUSIVE VIA 'VFPCDX'
   st:=dbstruct()
   ?? DB+" "
   aeval(st,{|x,i|outstd(x[1]:=lower(trim(x[1])),''),iif(x[2]='M', x[2]:='W',iif(SubStr(x[2],3)='U' .or. x[2]=='C' .and. x[3]>36, (x[2]:='Q:B',x[3]:=min(254,9*x[3]/8)),))})
   //db:=alias() keep it in the same case
   altd()
   dbcreate("tmp",st)
   use tmp NEW
   select 1
   dbeval({||B->(dbappend(),aeval(st,{|x,i,f|f:=A->(fieldget(i)),iif(valtype(f)='C',f:=trim(f),),fieldput(i,f)}))})
   close databases
   erase (set(_SET_DEFAULT)+db+".dbf")
   erase (set(_SET_DEFAULT)+db+".dbt")
   erase (set(_SET_DEFAULT)+db+".fpt")
   rename (set(_SET_DEFAULT)+"tmp.dbf") to (set(_SET_DEFAULT)+db+".dbf")
   rename (set(_SET_DEFAULT)+"tmp.dbt") to (set(_SET_DEFAULT)+db+".dbt")
   rename (set(_SET_DEFAULT)+"tmp.fpt") to (set(_SET_DEFAULT)+db+".fpt")
//endif
return
*****************