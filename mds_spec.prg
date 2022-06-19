#include "dm_form.ch"
#include "set.ch"
#include "inkey.ch"
*************
FUNCTION _sLIST(txt,_s)
  local hwbuf,sel,lfor,getlist:={},older,r,bl,x,y,z,skf2
  static old:="",oldar:=0
  memvar linia,a,b,c,d,e,f,g,h,oprn

  SAVE SCREEN to hwbuf
  cls
  if txt#NIL .and. _srap(txt,_s)
     RESTore SCREEN FROM hwbuf
     refresh(,_s)
     RETURN .F.
  endif

  private linia:=_sprompt,a,b,c,d,e,f,g,h

  sel=select()
if sel#oldar .or. ""=old
  txt:="eval(linia,0,_s,.t.)"
else
  txt:=old
endif
  skf2:=setkey(-1,{|a,x|a:=listfields(sel,{},{}),x:=0,if(aczojs(a[1],"",@x,a[2],"Wybierz pola"),(txt:=trim(txt),txt+=if(""=txt,""," + ")+trim(a[1,x]),updated(.t.)),)})
  ? "Prosz© okreòliÜ, kt¢re z dost©pnych p¢l maj• byÜ listowane."
  ? "Sci•gawka pod [F2]. Listing od bieæ•cej pozycji w d¢à.    Scroll Lock - pàynny."
  ?
  ?
  listfi(sel)
  getlist:={getnew(3,0,{|x|if(x=NIL,txt,txt:=x)},"txt","@KS"+ltrim(str(maxcol()+1,3)))}
  getlist[1]:cargo:=.t.
  older:=errorblock({|e|eval(older,e),if(e:severity>1,break(e),.f.)})
  DO WHILE .T.
  BEGIN SEQUENCE
  txt+=" "
#ifdef A_HBGET
  if len(txt)<=maxcol()
     pad(txt,maxcol()+1)
  endif
#endif
  readmodal(getlist)
  txt=trim(txt)
  IF ""#txt .AND. ReadkeY()#K_ESC
      bl:=&("{|d,_s|"+txt+"}")
      r:=eval(bl,0,_s)
      if !valtype(r)$"MC"
         bl:=errornew()
         bl:severity:=2
         bl:canretry:=.t.
         bl:description:="WartoòÜ wyraæenia nie jest typu tekstowego."
         eval(older,bl)
         break
      endif
  ENDIF
  RECOVER
   LOOP
  END SEQUENCE
  EXIT
  ENDDO
  setkey(-1,skf2)
  errorblock(older)
  if ""=txt .or. ReadkeY()=K_ESC
     RESTore SCREEN FROM hwbuf
     RETURN .F.
  ENDIF
  oldar:=sel
  if "eval(linia,0,_s,.t.)"=txt
     txt:=_sprompt
     old:=""
  else
     old:=txt
     txt:=&('{|d,_s|'+txt+'}')
  endif
  if _sfilb#NIL
    if _sfor=NIL
       lfor:=_sfilb
    else
       lfor:={||eval(_sfor).and.eval(_sfilb)}
    endif
  else
    lfor:=_sfor
  endif
  cls
  begin sequence
  if tak("LISTING NA DRUKARK®",0,0,.F.,.F.)
#ifdef A_HPDF
  #define D_HWPRN A_HPRN
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
#ifdef D_HWPRN
#command ?  [<explist,...>]         => wout( <explist> )
#command ?? [<explist,...>]         => wwout( <explist> )
#define qqout(x) wwout(x)
#define qout() wout()
#endif
     x:=SET(_SET_DEFAULT,'')
     z:=SET(_SET_PRINTFILE,'')
     y:=SET(_SET_PRINTFILE)
     if y==z
        print(0)
     else
        SET PRINTER TO (z) additive
        set printer ON
     endif
     SET(_SET_DEFAULT,x)
#ifdef D_HWPRN
  else
     oprn:=NIL
#endif
  endif
  ?? strtran(r,"≥","|")
  ?
  _skip(1,0,_s)
  setcancel(.f.)
  DBEval( {|t,l|scrltxt(),t:=trim(eval(txt,0,_s,.t.)),scrltxt(),qqout(strtran(left(t,l:=maxcol()-col()),"≥","|")),;
    if(len(t)>l,(if(row()=maxrow(),(scrllf()/*,scroll(0,0,row(),maxcol(),1),setpos(row(),maxcol()-len(t)+l)*/),/*setpos(row()+1,maxcol()-len(t)+l)*/),qqout(subs(t,l+1))),),;
    if(row()=maxrow(),scrllf(),),qout(),scrltxt()},lfor,{||scrltxt(), EVAL(_swar,_spocz,_skon).AND.inkey()#27},,, .T. )
  end sequence
  scrllf()
  setcancel(.t.)
#ifdef D_HWPRN
#command ?  [<explist,...>]         => Qout( <explist> )
#command ?? [<explist,...>]         => QQout( <explist> )
#endif
  if set(_SET_PRINTER,.f.)
#ifdef A_PRINT
  x:=set(_SET_PRINTFILE,'')
  if ! set(_SET_PRINTFILE)==x .and. File(x)
     A_PRINT(x)
  endif
#endif
  endif
  wait "Koniec, naciònij klawisz"
  RESTore SCREEN FROM hwbuf
  refresh(,_s)

RETURN .F.
**************************
FUNCTION _sfil(_s)
local hwbuf,txt,sel,getlist:={},older,r,b
  if VALTYPE(_s)='A' .and. _si=0 .and. !EVAL(_swar,_spocz,_skon)
     if !dbseek(_spocz)
        return .f.
     endif
  ENDIF
      * save screen in memvar
SAVE SCREEN to hwbuf
clS
      * clear window and draw box
sel=select()
?? 'Dost©pne relacje: =,#,$,<,>,<=,>= ; Dost©pne operacje logicze: .i., .lub., .nie.'
? 'Dost©pne nazwy pod [F2] '
? "Jaki filtr ?"
?
?
  listfi(sel)
  setkey(-1,{|a,x|a:=filfields(sel,{},{}),x:=0,if(aczojs(a[1],"",@x,a[2],"Wybierz pola"),(txt:=trim(txt),txt+=if(""=txt,""," .i. ")+trim(a[1,x]),updated(.t.)),)})
txt:=if(valtype(_s)='A',_sfilt,_s)
OLDER:=errorblock({|e|eval(older,e),if(e:severity>1,break(e),.f.)})
getlist:={getnew(4,0,{|x|if(x=NIL,txt,txt:=x)},"txt","@KS"+ltrim(str(maxcol()+1,3)))}
getlist[1]:cargo:=.t.
do while .t.
#ifdef A_HBGET
  if len(txt)<=maxcol()
     pad(txt,maxcol()+1)
  endif
#endif
  BEGIN SEQUENCE
  txt=STRTRAN(txt,".AND.",".i.")
  txt=STRTRAN(txt,".OR.",".lub.")
  txt=STRTRAN(txt,"!",".nie.")+" "
  READmodal(getlist)
  txt=STRTRAN(ALLTRIM(txt),".i.",".AND.")
  txt=STRTRAN(txt,".lub.",".OR.")
  txt=STRTRAN(txt,".nie.","!")
  IF ""#txt .AND. ReadkeY()#K_ESC
      b:=&("{||"+txt+"}")
      r:=eval(b)
      if valtype(r)#"L"
         b:=errornew()
         b:severity:=2
         b:canretry:=.t.
         b:description:="WartoòÜ wyraæenia nie jest typu prawda/faàsz."
         eval(older,b)
         break
      endif
  ENDIF
  RECOVER
   LOOP
  END SEQUENCE
  EXIT
ENDDO
setkey(-1,NIL)
ERRORBLOCK(older)
RESTore SCREEN FROM hwbuf
IF valtype(_s)$'MC' .and. ReadkeY()#K_ESC
   _s:=txt
   RETURN .t.
ELSEIF valtype(_s)='A' .and. ReadkeY()#K_ESC .AND.! _sfilt==txt
  _sfilt:=txt
  IF ""=TXT
    _sfilb:=NIL
    refresh(,_s)
    return .f.
  ELSE
    _sfilb:=&("{||"+txt+"}")
    if !r
       @ _srow1+_sm-1,_scol1 say padc("PROSZ® CZEKAè",_scoln) COLOR "*"+_slinia
    endif
  ENDIF
  refresh(1,_s)
  if !r
      adel(_srec,_sm)
      scroll(_srow1+_sm-1,_scol1-1,_srow2,_scol2,1)
      --_si
      if _sm>_si .and. _si>0
         --_sm
      endif
#ifdef __HARBOUR__
#define D_REST 4
#else
#define D_REST 2
#endif
      RESTSCREEN(_srow2,_scol1-1,_srow2,_scol2,SUBSTR(_scr,1+(_srow2-_srowb)*(_scoln+2)*D_REST))
      --_srow2
      _sprpt:=savescreen(_sm+_srow1-1,_scol1,_sm+_srow1-1,_scol2-1)
  endif
  if _si=0
    _sfilb:=NIL
    _sfilt:=""
    //_sef=.F.
    //_sbf=.F.
    tone(130,3)
    if !EVAL(_swar,_spocz,_skon)
       seek _spocz
    ENDIF
    _sexpgd(3,_s,.f.,.f.)
  else
    @ _srow2,_scol1 say left("FILTR: "+_sfilt,_scoln) color _sramka
  endif
endif


RETURN .f.
************************
func filfields(sel,archoice,arskip)

local i,j,pole,fi,txt,ars:=dbstruct()
  aadd(archoice,"*** "+alias()+" ***")
  aadd(arskip,.f.)
  for i=1 to fcount()
   pole:=if(sel=select(),"",CHR(64+SELECT())+'->')+trim(ars[i,1])
   fi:=fieldget(i)
   do case
   case ars[i,2]$"NB"
    txt:= pole+"="+ltrim(str(fi))
   case ars[i,2]=="C" .and. ars[i,3]=8 .and. ars[i,1]="D_"
    txt:= "BIN2D("+pole+")="+ltrim(str(bin2d(fi)))
   case ars[i,2]=="C"
    txt:= pole+'="'+left(fi,10)+'"'
   case ars[i,2]=="L"
    txt:= pole
   case ars[i,2]=="D"
    txt:= "DTOS("+pole+')="'+left(dtos(fi),6)+'"'
   endcase
   aadd(archoice,txt)
   aadd(arskip,.t.)
  next
  i=1
  j=select()
  do while dbrselect(i)>0
   select dbrselect(i)
   filfields(sel,archoice,arskip)
   ++i
   select (j)
  enddo
return({archoice,arskip})
**************
func listfields(sel,archoice,arskip)

local i,j,pole,fi,txt,ars:=dbstruct()
  aadd(archoice,"*** "+alias()+" ***")
  aadd(arskip,.f.)
  for i:=1 to fcount()
   pole:=if(sel=select(),"",CHR(64+SELECT())+'->')+trim(ars[i,1])
   fi:=fieldget(i)
   do case
   case ars[i,2]$"NB" .or. ars[i,2]=="DOUBLE"
    txt:= "STR("+pole+")"
   case ars[i,2]=="C" .and. ars[i,3]=8 .and. ars[i,1]="D_"
    txt:= "STR(BIN2D("+pole+"))"
   case ars[i,2]=="C"
    txt:= pole
   case ars[i,2]=="L"
    txt:= "IF("+pole+',"+","-")'
   case ars[i,2]=="D"
    txt:= "DTOC("+pole+")"
   endcase
   aadd(archoice,txt)
   aadd(arskip,.t.)
  next
  i=1
  j=select()
  do while dbrselect(i)>0
   select dbrselect(i)
   listfields(sel,archoice,arskip)
   ++i
   select (j)
  enddo
return({archoice,arskip})
**************
proc listfi(sel)

local i,j,pole,fi,txt,ars:=dbstruct()
  if col()>0
     ?
  endif
  ?? padc(alias(),15)+"|"
  for i:=1 to fcount()
   ?? ars[i,2]+":"+pad(if(sel=select(),"",CHR(64+SELECT())+'->')+lower(ars[i,1]),13)+"|"
   if col()>maxcol()-15
      ?
   endif
  next
  i=1
  j=select()
  do while dbrselect(i)>0
   select dbrselect(i)
   listfi(sel)
   ++i
   select (j)
  enddo
return
**************
stat func _srap(txt,_s)
  local tyt,x
  LOCAL _sl,_SDIR,lfor,a,getlist:={},p:=""
  memvar defa,oprn
     a:=DIRECTORY(linpath(TXT))
     IF EMPTY(A)
     a:=DIRECTORY(linpath(DEFA+TXT))
     p:=defa
     ENDIF
     _sdir:=array(len(a))
     AEVAL(a,{|X,y|_SDIR[y]:=PADR(STRTRAN(X[1],".","       "),8)})
     _sl=LEN(_SDIR)
     
     if _sl>0
        ?? "Raport od bieæ•cej pozycji w d¢à, wybierz gotow• form© wydruku",txt
        txt:="        "
        aczojs(_sdir,@txt,0)
        @ 0,0
        if lastkey()=K_ENTER
           txt:=trim(txt)+'.frm'
           @ 0,0 say "Dodatkowy tytuà"
           tyt:=if(""=_sfilt,IF(_sfor=NIL,"","Raport z wybranych pozycji"),"Raport z wybranych pozycji, kryterium wyboru: "+_sfilt)+" "
           @ 1,0 get tyt picture "@KS"+ltrim(str(maxcol(),3)) send cargo:=.t.
           begin sequence
            read
            if ReadkeY()=K_ESC
               break(NIL)
            endif
            scroll(0,0,1,maxcol())
            if _sfilb#NIL
               if _sfor=NIL
                 lfor:=_sfilb
               else
                 lfor:={||eval(_sfor).and.eval(_sfilb)}
               endif
            else
                 lfor:=_sfor
            endif
#ifdef __PLATFORM__UNIX
 #define nl chr(10)
#else
 #define nl chr(13)+chr(10)
#endif
            tyt:=if(empty(tyt),,trim(strtran(tyt,";",nl)))
           setcancel(.t.)
            __REPORTFORM(p+TXT,tak("LISTING NA DRUKARK®",0,,.F.,.F.),,.F.,lfor,{||scrltxt(),EVAL(_swar,_spocz,_skon).AND.inkey()#27},,,.T.,;
             tyt=NIL.and.!TAK("CZY PODZAù NA STRONY",0,,.t.),tyt,.F.,TAK("CZY TYLKO SUMY",0,,.F.))
           end
#ifdef D_HWPRN
           oprn:=NIL
#endif
           if set(_SET_PRINTER,.f.)
#ifdef A_PRINT
           x:=set(_SET_PRINTFILE,'')
           if ! set(_SET_PRINTFILE)==x .and. File(x)
               A_PRINT(x)
           endif
#else
           set printer to
#endif
           endif
           scrllf()
           setcancel(.t.)
           wait "Koniec, naciònij klawisz"
           RETURN .t.
        endif
        @ 0,0 clear to 1,maxcol()
     else
        ?? "Brak gotowych form wydruku",txt
     endif

RETURN .F.
*************************
