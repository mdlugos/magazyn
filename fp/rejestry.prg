//#define I hb_UTF8ToStr("│")
#include "ar.ch"
#include "inkey.ch"
#include "dm_form.ch"

MEMVAR it_menurej,rejestry
field lp,data,ident
#ifdef A_DSPLIT
field smb_dow,nr_dowodu
#else
#define smb_doW dowod
field dowod
#endif
#ifndef A_IDENT
  #define A_IDENT hb_fieldlen('ident')
#endif

proc rejestry

local m,a
public it_menurej
      a:=array(len(rejestry))
      aeval(rejestry,{|ar,i|a[i]:=ar[AR_SMB]+" "+ar[AR_NAME]})
      m:=it_menurej
      SETKEY(4,{||KIBORD(CHR(27)+CHR(24)+CHR(13))})
      SETKEY(19,{||KIBORD(CHR(27)+CHR(5)+CHR(13))})
      aczojs(a,"",@m)
      SET KEY 4 TO
      SET KEY 19 TO

   if m#0
      it_menurej:=m
      rejbrow(rejestry[m])
   ENDIF

return
******************
func rejbrow(aR,view)
local b
sel(ar[AR_DBF],ar[AR_DBF]+"_LP")
rel(ar[AR_REL])
if valtype(ar[AR_LINES])$"MC"
#ifdef A_DSPLIT
   ar[AR_LINES]:=&(hb_UTF8ToStr('{||lp+"│"+DTOV(data)+{"░","▒","▓","│","┤"}[min(1,max(5,status+1))]+smb_dow+nr_dowodu+"│"+pad(ident,')+str(A_IDENT)+hb_UTF8ToStr(')+"│"+')+ar[AR_LINES]+'}')
#else
   ar[AR_LINES]:=&(hb_UTF8ToStr('{||lp+"│"+DTOV(data)+{"░","▒","▓","│","┤"}[min(1,max(5,status+1))]+dowoD+"│"+pad(ident,')+str(A_IDENT)+hb_UTF8ToStr(')+"│"+')+ar[AR_LINES]+'}')
#endif
endif

return szukam({0,0,,,1,0,hb_UTF8ToStr("Lp┬Data─┬─Dowód─┬")+padc("Identyfikacja",A_IDENT,hb_UTF8ToStr("─"))+hb_UTF8ToStr("┬")+ar[AR_HEADER],ar[AR_LINES],{|k,s|rejinfo(k,s,ar,view)},""})
**********************************
stat func rejinfo(key,_s,ar,view)

local i

if key=0
   _snagkol:=3
elseif key=K_ESC
   return .t.
elseif empty(view) .and. key=K_INS
   //key:=indexord()
   if dok(ar[AR_SMB],,.t.)
      REFRESH(,_s)
   //else
   //   set order to (key)
   endif

elseif empty(view) .and. key=43   // PLUS
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
      goto _srec[_sm]
      //key:=indexord()
   if dok(ar[AR_SMB],,.t.,ascan(ar[AR_DOKUMENTY],{|ad|smb_doW=ad[AD_SMB]}))
      REFRESH(,_s)
   //else
     //set order to (key)
     //goto _srec[_sm]
   endif
   return .t.
elseif _si=0
elseif empty(view) .and. key=K_TAB
   dokdruk(ar[AR_SMB],lp)
elseif key=K_ENTER
   if .t.=view
      Return _sret:=.t.
   endif
   key:=dok(ar[AR_SMB],lp)
   rel(ar[AR_REL])
   if key
      if deleted()
         REFRESH(,_s)
      else
         REFRESH LINE _sm+_srow1-1 DIRECTION 0
      endif
   endif
elseif key=K_CTRL_RIGHT .or. key=K_CTRL_LEFT
   i:=ascan({1,7,13,21},_sbeg)
   if i=2
      _spform:={|p|p}
   endif
   if key=K_CTRL_RIGHT
      i:=i%4+1
   else
      i:=(i+2)%4+1
   endif
   _slth:=0
   _spocz:=""
   if i=1
      set order to ar[AR_DBF]+"_LP"
      _sbeg:=1
   elseif i=2
      set order to ar[AR_DBF]+"_DT"
      _sbeg:=7
      _spform:={|p,l|tranr(right(p,l),"XX.XX")}
      //_spocz:= dseek(,'data',min(max(DatY->d_z_rok+1,data),stod(str(year(DatY->d_z_rok+1),4)+'1231')))
      _spocz:= dseek(,'data',data)
      _spocz:= left(_spocz,len(_spocz)-2)
      _slth:=2
   elseif i=3
      set order to ar[AR_DBF]+"_ND"
      _sbeg:=13
   elseif i=4
      set order to ar[AR_DBF]+"_ID"
      _sbeg:=21
   endif
   //_swar=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   refresh(1,_s)

elseif key=K_F8
      _slist('.'+HB_OsPathSeparator()+ar[AR_DBF]+'*.frm',_s)

elseif key=K_F9
      _sfil(_s)


endif
return .f.
