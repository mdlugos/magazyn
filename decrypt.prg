//hbmk2
//-prgflag=-dSIMPLE -gtnul -u+polok.ch -ie:\cl decrypt io io_2 error

#ifdef A_SX
#include "dbinfo.ch"
#endif

proc main(p)

local r

if empty(p)
   r:=''
else
   r:=left(p,rat(HB_ps(),p))
endif

aeval(directory(p),{|x|if(upper(right(x[1],4))=='.DBF',do_job(r+x[1]),)})

return
****************
proc do_job(x)
nuse (x) exclusive

IF DBINFO(DBI_ISENCRYPTED)
   ? x
   if DBINFO(DBI_DECRYPT,A_SX)
       ?? ' - OK'
   endif
ENDIF

return
