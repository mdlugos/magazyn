#include "inkey.ch"
#include "dm_form.ch"
#include "dbinfo.ch"

#define I hb_UTF8ToStr("│")

#ifdef __PLATFORM__DOS
#define BOTD '└'+REPLICATE('═',_scoln)+'┘'
#define TOPD '┌'+REPLICATE('═',_scoln)+'┐'
#else
#define BOTD '╘'+REPLICATE('═',_scoln)+'╛'
#define TOPD '╒'+REPLICATE('═',_scoln)+'╕'
#endif
#define RNTO1(x) _snaglo(x,'─')
#define RNTO2(x) _snaglo(x,'═')
#define RSTO2(x) _stopka(x,'═')
#define RSTO1(x) _stopka(x,'─')
*******
request tranr
request evaldb

static function _snaglo(s,m)
  local x:='',i,j:=0,u,n,t
#ifdef __PLATFORM__DOS    
  t:=hb_UTF8ToStrBox(m)
#endif
  if m='═'
#ifndef __PLATFORM__DOS    
    t:=hb_UTF8ToStrBox("╤")
#endif    
    u:=hb_UTF8ToStr('┬')
    n:='─'
  elseif m='─'
#ifndef __PLATFORM__DOS    
    t:=hb_UTF8ToStrBox("╥")
#endif    
    u:=hb_UTF8ToStr('╦')
    n:='═'
  else
    return s
  endif
  s:=strtran(s,hb_UTF8ToStr(n),hb_UTF8ToStr(m))
  do while (i:=hb_at(u,s,j+1))>0
    DispOut(SubStr(s,j+1,i-j-1),_SRAMKA)
    hb_DispOutAtBox(row(),col(),t,_SRAMKA)
    setpos(row(),col()+1)
    j:=i 
  enddo

  x:=SubStr(s,j+1)
  
return x

static function _stopka(s,m)
  local x:='',i,j:=0,u,n,t
#ifdef __PLATFORM__DOS
  n:=t:=hb_UTF8ToStrBox(m)
#endif  
  if m='═'
#ifndef __PLATFORM__DOS
    t:=hb_UTF8ToStrBox("╧")
#endif  
    n:=hb_UTF8ToStrBox('╩')
  elseif m='─'
    t:=hb_UTF8ToStrBox("┴")
#ifndef __PLATFORM__DOS
    n:=hb_UTF8ToStrBox('╨')
#endif    
  else
    return ''
  endif
  u:=hb_UTF8ToStr('┬')
  do while (i:=hb_at(u,s,j+1))>0
    hb_DispOutAtBox(row(),col()+i-1,t,_SRAMKA)
    j:=i 
  enddo
  j:=0
  u:=hb_UTF8ToStr('╦')
  do while (i:=hb_at(u,s,j+1))>0
    hb_DispOutAtBox(row(),col()+i-1,n,_SRAMKA)
    j:=i 
  enddo
  
return ''

FUNCTION szukam(_s)

local _scur,_srins,_selar,_scolor,_stxt,_skey,_srow,_scol,bx,cx,dx,myszflag,job

*                  max. wymiar okna,podkreslenie,naglowek,&linia,&f.obsl.kl.
*             ┌──────┬──────┼──────┐      ├─────┐    │       │        │
*PARAMETERS _srowb,_scol1,_srowe,_scol2,_sbeg,_slth,_snagl,_sprompt,_sinfo,_spocz,_skon              // WARUNEK SEEK, MOZE ZOSTAC POMINIETY

  asize(_s,max(len(_s),_sLEN))
  _selar:=select()
  DEFAULT _sret TO .F.
  DEFAULT _spform TO {|p,l|RIGHT(p,l)}
  //DEFAULT _sp2s TO {|x|x} //długość musi być ta sama
  DEFAULT _ss2p TO {|x|x} //nie nil, bo używany bez sprawdzenia przez _spform
  _srins:=set(_SET_INSERT)
  _scur:=setcursor(0)
  _skey:=0
  //DEFAULT _swar TO NIL           // warunek WHILE
  //DEFAULT _sfor TO NIL           // warunek FOR
  //DEFAULT _sfilb TO NIL
  DEFAULT _sfilt TO ""
  DEFAULT _sbf TO .F.            // beg-of-file
  DEFAULT _sef TO .F.            // end-of-file
  DEFAULT _sm TO 1
  DEFAULT _si TO 0
  IF _scol1#NIL;_scol1:=int(++_scol1);ENDIF

  DEFAULT _skproc TO array(32)
  DEFAULT _skproc[1]  TO {|_skey,_s|_SHOME(_s)}  //HOME
//      2          //CTRL RIGHT
  DEFAULT _skproc[3]  TO {|_skey,_s|_SPGDN(_s,_skey)}  //PGDN
  DEFAULT _skproc[4]  TO {|_skey,_s|_SPRAWO(_s,_skey)} //RIGHT
  DEFAULT _skproc[5]  TO {|_skey,_s|_SGORA(_s,_skey)}  //UP
  DEFAULT _skproc[6]  TO {|_skey,_s|_SEND(_s,_skey)} //END
//      7          //DEL
  DEFAULT _skproc[8]  TO {|_skey,_s|_SLEWO(_s,_skey)}  //BS
//      9          //TAB
//      10        //CTRL RET
#ifdef A_LAN
  #define D_LAN
#endif
#ifdef A_NOREFRESH
 #undef D_LAN
#else
 #ifdef D_LAN
   #define ONEREFRESH .2
   #define ALLREFRESH 15
 #else
   #ifdef A_DIETA
   #ifdef A_MYSZ
     #define ONEREFRESH if(alias()="RO_",.2,86400)
     #define ALLREFRESH if(alias()="RO_",15,86400)
   #else
     #define ONEREFRESH if(alias()="RO_",.2,0)
     #define ALLREFRESH if(alias()="RO_",15,0)
   #endif
     #define D_LAN
   #endif
 #endif
#endif

#ifdef D_LAN
  DEFAULT _skproc[11] TO {|_skey,_s|setpos(_sm+_srow1-1,_scol1),dispout(padr(eval(_sprompt,1,_s),_scol2-COL())),_sprpt:=savescreen(_sm+_srow1-1,_scol1,_sm+_srow1-1,_scol2-1),.f.}
  DEFAULT _skproc[12] TO {|_skey,_s|cut(_s,.f.)}
#endif
//      13        //RET
#ifdef A_MYSZ
  DEFAULT _skproc[14] TO {|_skey,_s,bx,cx,dx,myszflag|mysz(_s,bx,cx,dx,myszflag)}
#endif
 *      15        //CTRL O
 *      16        //CTRL P
 *      17        //CTRL Q
  DEFAULT _skproc[18] TO {|_skey,_s|_SPGUP(_s,_skey)}  //UP
  DEFAULT _skproc[19] TO {|_skey,_s|_SLEWO(_s,_skey)}  //LEFT
 *      20        //CTRL T
 *      21        //CTRL U
 *      22        //INS
 *      23        //CTRL END
  DEFAULT _skproc[24] TO {|_skey,_s|_SDOL(_s,_skey)} //DOWN
 //      25        //CTRL Y
 //      26        //CTRL LEFT
 //      27        //ESC
  DEFAULT _skproc[28] TO {||help("s_"+procname(3)),.f.}
  DEFAULT _skproc[29] TO {|_skey,_s|_SEXPGD(0,_s)} //CTRL HOME
  DEFAULT _skproc[30] TO {|_skey,_s|_SBOT(_s)} //CTRL PGDN
  DEFAULT _skproc[31] TO {|_skey,_s|_STOP(_s)} //CTRL PGUP
  DEFAULT _skproc[32] TO {|_skey,_s|_SZNAK(_s,_skey)}  //32...255

  IF _swar=NIL
    if ValType(_spocz)='C'
      * OBSZAR OGRANICZONY
      //_stxt:=IndexkeY(0)
      IF ValType(_skon)='C'
        * OGRANICZENIE NA PODSTAWIE AKTYWNEGO INDEKSU
        //_swar:=EvAlDb('{|p,k|'+_stxt+'>=p.AND.k>'+_stxt+'}')
        _swar:={|p,k,x|x:=UpP(ordKeyVal()),!Eof() .and. x>=p .and. k>x}
      ELSE
        * OGRANICZENIE NA PODSTAWIE AKTYWNEGO INDEKSU
        //_swar:=EvAlDb('{|p|'+_stxt+'=p'+'}')
        _swar:={|p|!Eof() .and. UpP(ordKeyVal())=p}
      ENDIF
    else
      _swar:={||.t.}
    endif
    set cursor on
  ENDIF

  if _sinfo=NIL
    _sinfo:={|k,s|_Sinfo(k,s)}
  endif

  IF eval(_sinfo,0,_s)
    set relation to
    SETCURSor(_scur)
    select (_selar)
    RETURN _sret
  ENDIF

  if _sprompt=NIL
    job:=dbstruct()
    _sprompt:={||pad(tran(fieldget(1),),job[1,3])+I+pad(tran(fieldget(2),),job[2,3])}
    //+"|"+tran(fieldget(3),)}
    if _snagl=NIL
      _snagkol:=job[1,3]-len(job[1,1])
      _snagl:=job[1,1]+hb_UTF8ToStr("┬")+job[2,1]
    endif
  endif

  if _scol1=NIL.or._scol2=NIL
     _stxt:=min(maxcol()-1,len(eval(_sprompt,0,_s,.t.)))
     if _scol2=NIL .and. _scol1=NIL
        _scol1:=min(maxcol()-_stxt,max(1,col()-Round(_stxt/2,0)))
        _scol2:=_scol1+_stxt
     elseif _scol1=NIL
        _scol1:=max(1,min(maxcol(),_scol2)-_stxt)
        _scol2:=_scol1+_stxt
     else
        _scol2:=min(maxcol()-_stxt,max(1,_scol1))+_stxt
        _scol1:=_scol2-_stxt
     endif
  endif
  _scol2:=min(maxcol(),int(_scol2))


  _srowe:=if(_srowe=NIL,MaxRow(),min(MaxRow(),int(_srowe)))
  _srowb:=if(_srowb=NIL,0,int(_srowb))

  _srow=ROW()
  _scol=COL()
  _scr=SAVESCREEN(_srowb,_scol1-1,_srowe,_scol2)
  _scolor:=setcolor(_snorm)

  _srown:=_srowe-_srowb-1  // max ilosc wierszy
  _scoln:=_scol2-_scol1    // ilosc kolumn

  _srec:=array(_srown)

  IF _snagkol=NIL
    IF _snagl=NIL .or. len(_snagl) > _scoln .AND. ''#_snagl
      _snagkol:=0     // kolumna, od ktorej wyswietlany naglowek
      _snagl:=''
     ELSE
      _snagkol:=round(_scoln/2-(len(_snagl))/2,0)
    ENDIF
  endif
  _snagkol:=min(_scoln,_snagkol)
  _snagl:=left(_snagl,_scoln-_snagkol)

  DO CASE
    CASE _srow>_srowe-2
      _srow1=_srowe-1
    CASE _srow>_srowb
      _srow1=_srow
   OTHERWISE
      _srow1=_srowb+1       // pierwszy wiersz
  ENDCASE
  _srow2=_srow1               // wiersz podkreslenia

  DEFAULT _spform TO {|p,l|RIGHT(p,l)}

  if empty(_sbeg)
    set cursor off
    _skproc[32]:=NIL //sznak
    _skproc[4] :=NIL // K_RIGHT
    _skproc[1] :=NIL // K_HOME
    _skproc[19]:=NIL // K_LEFT
    _skproc[8] :=NIL // K_BS
    _skproc[6] :=NIL // K_END
    _spform  :=NIL
    _slth:=_sbeg:=0 //_slth - widoczny kawałek _spform od prawej,
  endif

*  DEZAKTYWACJA STANDARTOWYCH FUNKCJI OBSLUGI W ZALEZNOSCI OD PARAMETROW
*  PO PIERWSZYM WYWOLANIU _SINFO, O ILE NIE ZOSTYALY ZMIENIONE


  //if _si=0
     _skey:=nextkey()
     IF _skey#K_HOME .AND. _skey#K_CTRL_PGDN .AND. _sKEY#K_CTRL_PGUP .AND. _skey#K_CTRL_HOME
       kibord(K_CTRL_HOME)
     ENDIF
  //endif
#ifdef A_MYSZ
  myszflag=.f.
#endif

  DO WHILE .T.
      _skey:=0
#ifdef D_LAN
      if _si=0
      elseIF _srec[_sm]#recno()
         _skey:=ONEREFRESH
      else
         _skey:=ALLREFRESH
      endif
#endif

#ifdef A_MYSZ
        bx:=cx:=dx:=0
        _skey:=INKey(_skey, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)
        if _skey>=K_MINMOUSE .and. _skey<=K_MAXMOUSE
           if _skey=K_LBUTTONDOWN
              bx:=1
           else
              bx:=2
           endif
           cx:=mcol()
           dx:=mrow()
           _skey:=14
           myszflag:=.t.
            RESTORE LINE _sm+_srow1-1
        elseif _skey=K_ALT_RETURN
              _skey:=K_CTRL_RET
        elseif _skey= K_ALT_PGDN
              _skey:=K_CTRL_PGDN
        elseif _skey= K_ALT_PGUP
              _skey:=K_CTRL_PGUP
        elseif _skey= K_ALT_HOME
              _skey:=K_CTRL_HOME
        elseif _skey= K_ALT_END
              _skey:=K_CTRL_END
        elseif _skey= K_ALT_LEFT
              _skey:=K_CTRL_LEFT
        elseif _skey= K_ALT_RIGHT
              _skey:=K_CTRL_RIGHT
        endif
#else
#ifdef D_LAN
        _skey:=inkey(_skey)
#else
        _skey:=inkey(0)
#endif
#endif
#ifdef D_LAN
      IF _skey=0
        IF _srec[_sm]#recno()
          go _srec[_sm]
          _skey=11
        else
          _skey=12
        endif
      endif
#endif
#ifdef A_DEMO
    IF DTOS(A->data)>=A_DEMO .AND. RECNO()%75=0
      alarm(hb_UTF8ToStr("Wersja demonstracyjna.;W sprawie zakupu pełnej wersji;skontaktuj się z autorem programu."),,3,3)
//      _skey:=27
    ENDIF
#endif

    set color to (_snorm)

#ifdef A_MYSZ
    if evakey(_skey,_s,bx,cx,dx,@myszflag)
#else
    if evakey(_skey,_s)
#endif
        EXIT
    ENDIF

    if _si>0
      if _sbeg>0
        restscreen(_sm+_srow1-1,_scol1,_sm+_srow1-1,_scol2-1,hiattr(_sprpt))
        @ _sm+_srow1-1,_scol1+_sbeg-1 SAY left(eval(_spform,_spocz,_slth),_scoln-_sbeg+1) COLOR _SEL
      else
        ALTERNATE LINE _sm+_srow1-1 BUFFER _sprpt ATTRIB 119
      endif
    ENDIF
  ENDDO

  set(_SET_INSERT,_srins)
  SETCURSor(_scur)
  if !empty(_scr)
     RESTSCREEN(_srowb,_scol1-1,_srowe,_scol2,_scr)
  endif
  setpos(_srow,_scol)
  setcolor(_scolor)
  set relation to
  select (_selar)
  //przeniesione z mds_spec
  set printer to

RETURN _sret
***********************************
func _Sinfo(k,_s)
local txt
static maxord:=0,ord_1,ord_l

if k=0
   maxord:=indexord()
   ord_1:=if(_sbeg=1,maxord,0)
   ord_l:=if(_sbeg=1,0,maxord)
elseif k=K_ENTER
   _sret:=.t.
   go _srec[_sm]
   return .t.
elseif k=K_ESC
   return .t.
elseif ( k=K_CTRL_LEFT .or. k=K_CTRL_RIGHT ) .and. ordnumber()<>0
   txt:=eval(_sprompt,0,_s,.t.)
   if k=K_CTRL_RIGHT
      txt:=at(I,SubStr(txt,_sbeg))
      if txt=0
         ord_l:=ordnumber()
         RETURN .f.
      endif
      k:=ordnumber()
      ordsetfocus(k+1)
      if indexord()=0
         ord_l:=k
         ordsetfocus(ord_l)
         RETURN .f.
      endif
      _sbeg:=txt+_sbeg
   else
      if _sbeg=1
         ord_1:=ordnumber()
         RETURN .f.
      endif
      k:=rat(I,left(txt,_sbeg-2))
      _sbeg:=k+1
      k:=ordnumber()
      if k=1
       if _sbeg=1 .and. ord_1<>0
         k:=ord_1
       else
         if maxord=k
           maxord:=max(2,max(ord_1,ord_l))
         endif
         k:=maxord
       endif
       ordsetfocus(k)
      else
       ordsetfocus(k-1)
      endif
      if _sbeg=1
        ord_1:=indexord()
      endif
   endif
   //_swar:=EvAlDb('{|p|'+IndexkeY(0)+'=p'+'}')
   _spocz:=left(_spocz,len(_spocz)-_slth)
   _slth:=0
   REFRESH(1,_s)
elseif k=K_F9
   _sfil(_s)
elseif k=K_F8
   _slist(,_s)
endif
return .f.
***********************************
*OBSŁUGA MYSZY
************************************
#define D_REST 4
//iif(hb_gtInfo( HB_GTI_COMPATBUFFER ),2,4)

#ifdef A_MYSZ
func mysz(_s,bx,cx,dx,myszflag)
local ret,scrlok,delta:={1,0,0}
         if dx>=_srow1 .and. dx<_srow2 .and. cx>=_scol1 .and. cx<_scol2 .and. bx=1
            do while inkey(0,INKEY_MOVE + INKEY_LUP)<>1003
              // kumulatywne przesunięcie w dx dy
              delta[3]:=dx-delta[3];dx:=mrow();delta[3]:=dx-delta[3]
              delta[2]:=cx-delta[2];cx:=mcol();delta[2]:=cx-delta[2]
              if delta[2]+_scol1<1 .or. delta[2]+_scol2>maxcol() .or. delta[3]+dx<=_srowb .or. delta[3]+dx>=_srowe
                loop
              endif
              if scrlok=NIL 
                scrlok:=savescreen(_srow1-1,_scol1-1,_srow2,_scol2)
              endif 
              dispbegin()
              RESTSCREEN(max(_srowb,_srow1-1),_scol1-1,_srow2,_scol2,hb_BSubStr(_scr,1+max(0,_srow1-_srowb-1)*(_scoln+2)*D_REST))
              _srow1+=delta[3]
              _srow2+=delta[3]
              _scol1+=delta[2]
              _scol2+=delta[2]
              if delta[2]#0
                _scr:=savescreen(_srowb,_scol1-1,_srowe,_scol2)
              endif
              afill(delta,0)
              restscreen(max(_srowb,_srow1-1),_scol1-1,min(_srow2,_srowe),_scol2,hb_BSubStr(scrlok,1+max(0,_srowb-_srow1+1)*(_scoln+2)*D_REST))
              dispend()
            enddo
         endif 
         if bx=2 .or. cx>=_scol2 .or. cx <_scol1 .or. dx < _srow1-1 .or. dx>_srow2
            if myszflag
               return evakey(27,_s)
            endif
         elseif _si=0
         elseif dx=_srow1-1
            if !_sbf
            if _sm>1
               _sm:=1
               SAVE LINE _srow1+1
            endif
            return evakey(5,_s)
            endif
         elseif dx=_srow2
            if !_sef
            if _sm<_si
               _sm:=_si
               SAVE LINE _srow2-1
            endif
            return evakey(24,_s)
            endif
         else
            if dx#_sm+_srow1-1
              _sm:=dx-_srow1+1
              if _sbeg>0
                   HIGHLIGHT LINE dx
              else
                   SAVE LINE dx
                   ALTERNATE LINE dx BUFFER _sprpt ATTRIB 119
              endif
            endif  
            if scrlok<>NIL
                if _srow1 - 1 <_srowb 
                   //cut(_s)
                   _sbf:=.f.
                   @ _srowb,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
                   @ _srowb,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
                   do while _srow1 - 1 <_srowb
                      adel(_srec, _srowb - _srow1 + 1 )
                      --_sm
                      --_si
                      ++_srow1
                   enddo 
                elseif _srow2>_srowe
                    _sef:=.f.
                    @ _srowe,_scol1-1 BOX '└'+REPLICATE('─',_scoln)+'┘' UNICODE COLOR _SRAMKA
                    @ _srowe,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
                    _si-=_srow2-_srowe
                    _srow2:=_srowe
                endif
                return .f.
          endif
          return evakey(13,_s)
         endif

return .f.
************

func evakey(_skey,_s,bx,cx,dx,myszflag)
   local _stxt,job

   job:=myszflag

#else

func evakey(_skey,_s)
   local _stxt

#endif

    IF _skey<1 .or. _skey>126 .and. hb_keyChar(_skey)==""
#ifdef A_MYSZ
       myszflag=.f.
#endif
       IF _si>0
          GO _srec[_sm]
       ENDIF
       if (_stxt:=setkey(_skey))#NIL
          EVAL(_stxt,procname(2),_s)
          return .f.
       else
          return eval(_sinfo,_skey,_s)
       endif
    else

     if (_stxt:=_skproc[max(1,min(_skey,32))])=NIL
        _stxt:=_sinfo
        IF _si>0
           GO _srec[_sm]
  #ifdef A_MYSZ
           myszflag:=.f.
  #endif
        ENDIF
     endif
   endif
#ifdef A_MYSZ
return eval(_stxt,_skey,_s,bx,cx,dx,job)
#else
return eval(_stxt,_skey,_s)
#endif
***********************************
*OBSLUGA KLAWIATURY
***********************************
FUNCTION _SDOL(_s,_skey)
    IF _si>0
      RESTORE LINE _sm+_srow1-1
    ENDIF
    IF _sm<_si
      ++_sm
    ELSE
      IF _si>0
        GO _srec[_si]
      ENDIF
      IF _sef .and. !eval(_sinfo,_skey,_s)
        RETURN(.F.)
      ENDIF
      DO WHILE _skip(1,_skey,_s)
        expd(_s)
        if nextkey()#_skey
           exit
        endif
        while nextkey()=_skey
           inkey()
        enddo
      ENDDO
      if _sef
        @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
      ENDIF
      _sm:=if(_si>0,_si,1)
    ENDIF
    SAVE LINE _sm+_srow1-1
RETURN .F.

FUNCTION _sgora(_s,_skey)
    IF _si>0
      RESTORE LINE _sm+_srow1-1
    ENDIF
    IF _sm>1
      --_sm
     ELSE
      IF _si>0
        GO _srec[1]
      ENDIF
      IF _sbf .and.! eval(_sinfo,_skey,_s)
        RETURN(.F.)
      ENDIF
      DO WHILE _skip(-1,_skey,_s)
        expg(_s)
        if nextkey()#_skey
           exit
        endif
        while nextkey()=_skey
           inkey()
        enddo
      ENDDO
      if _sbf
        @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
      ENDIF
    ENDIF
    SAVE LINE _sm+_srow1-1
RETURN .F.

FUNCTION _sPgDn(_s,_skey)
    IF _si>0
      RESTORE LINE _sm+_srow1-1
    ENDIF
    _sm:=_si-_sm+1
    IF ! _sef .AND. _sm<_srown
      IF _si>0
        GO _srec[_si]
      ENDIF
      DO WHILE _srown>_sm .and. _skip(1,_skey,_s)
        expd(_s)
        ++_sm
        while nextkey()=_skey
           inkey()
        enddo
      ENDDO
      IF  _sef
        @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
      ENDIF
    ENDIF
    _sm:=if(_si>0,_si,1)
    SAVE LINE _srow2-1

RETURN .F.

FUNCTION _sPgUp(_s,_skey)
    IF _si>0
      RESTORE LINE _sm+_srow1-1
    ENDIF
    IF ! _sbf .AND. _sm<_srown
      IF _si>0
        GO _srec[1]
      ENDIF
      DO WHILE _sm<_srown .AND. _skip(-1,_skey,_s)
        ++_sm
        expg(_s)
        while nextkey()=_skey
           inkey()
        enddo
      ENDDO
      IF _sbf
        @ _srow1-1,_scol1-1 SAY TOPD COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
      ENDIF
    ENDIF
    _sm=1
    SAVE LINE _srow1

RETURN .F.

FUNCTION _stop(_s)
    local _stxt,_skey
    IF ! _sbf
      RESTSCREEN(_srow1,_scol1-1,_srow2,_scol2,hb_BSubStr(_scr,1+(_srow1-_srowb)*(_scoln+2)*D_REST))
      @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
      @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
      _sbf=.T.
      _sef=.F.
      _sm=1
      _si=0
      _srow2=_srow1
      IF _spocz#NIL .and.ordnumber()#0
        SEEK _spocz
       ELSE
        GO TOP
      ENDIF
      IF _skip(0,,_s)
        expd(_s)
        if _sbeg>0
           HIGHLIGHT LINE _srow1
        else
           SAVE LINE _srow1
           ALTERNATE LINE _srow1 BUFFER _sprpt ATTRIB 119
        endif
        DO WHILE _srown>_si .AND. _skip(1,0,_s)
          expd(_s)
        ENDDO
      ENDIF
      IF   _sef
        @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
      ENDIF
#ifdef D_LAN
      go _srec[_sm]
#endif
     ELSEIF _si>0 .and. _sm>1
        RESTORE LINE _sm+_srow1-1
        _sm=1
        SAVE LINE _srow1
    ENDIF

RETURN .f.

FUNCTION _sbot(_s)
    local _skey,_stxt
    IF ! _sef
      RESTSCREEN(_srow1-1,_scol1-1,_srow2-1,_scol2,hb_BSubStr(_scr,1+(_srow1-1-_srowb)*(_scoln+2)*D_REST))
      @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
      @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
      _sef=.T.
      _sbf=.F.
      _srow1=_srow2
      _si=0
      IF valtype(_skon)='C'
        SEEK _skon LAST
       ELSE
        IF valtype(_spocz)$"MC" .and.ordnumber()#0
          IF ''=_spocz
            GO BOTTOM
          ELSE
            SEEK _spocz LAST
          ENDIF
         ELSE
          GO BOTTOM
        ENDIF
      ENDIF
      //IF _skip(0,0,_s)
        expg(_s)
        if _sbeg>0
           HIGHLIGHT LINE _srow1
        else
           SAVE LINE _srow1
           ALTERNATE LINE _srow1 BUFFER _sprpt ATTRIB 119
        endif
        DO WHILE _srown>_si .AND. _skip(-1,0,_s)
          expg(_s)
        ENDDO
      //ENDIF
      IF   _sbf
        @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
      ENDIF
      _sm:=if(_si>0,_si,1)
#ifdef D_LAN
      go _srec[_sm]
#endif
     ELSEif _si>0 .and. _sm<_si
        RESTORE LINE _sm+_srow1-1
        _sm=_si
        SAVE LINE _srow2-1
    ENDIF

RETURN .f.

FUNCTION _shome(_s)
  IF _skproc[6]#NIL
    _spocz:=left(_spocz,len(_spocz)-_slth)
    _slth:=0
    restscreen(_sm+_srow1-1,_scol1,_sm+_srow1-1,_scol2-1,hiattr(_sprpt))
  endif
  //_sef:=.F.
  //_sbf:=.F.
  _sexpgd(0,_s,.f.,.f.)
return .f.

FUNCTION _sznak(_s,_skey)
    local _scond:=.f.,x,l

    if _scoln-_sbeg+1<len(eval(_spform,_spocz,_slth))
      return .f.
    endif
    IF _si>0
      GO _srec[_sm]
    ENDIF
    if _sp2s<>NIL
      _spocz:=eval(_sp2s,eval(_ss2p,_spocz,_slth)+hb_keyChar(_skey),_slth+1)
    else
      _spocz+=UpP(hb_keyChar(_skey))
    endif
    ++_slth
    IF _si>0
      _scond:=EvaldB(_swar,_spocz,_skon)
    ENDIF
    if _scond
      @ _sm+_srow1-1,_scol1+_sbeg-1 SAY left(eval(_spform,_spocz,_slth),_scoln-_sbeg) color _sel
      IF !hb_keyChar(NEXTKEY())==''
        CUT(_s)
      ENDIF
    ELSEif dbSEEK(_spocz) .and._skip(0,,_s)
      l:=ASCAN(_srec,RECNO(),1,_si)
      if l#0
         _sbf:=.t.
      elseif _si>0
         l:=_scol1+_sbeg+len(eval(_spform,_spocz,_slth))-2
         l:=UpP(getscrtxt(savescreen(_srow1,l,_srow1,l)))
         l:=if(l=UpP(hb_keyChar(_skey)),1,0)
      endif
      if l#0
        RESTORE LINE _sm+_srow1-1
        SAVE LINE l+_srow1-1
        x:=left(eval(_spform,_spocz,_slth),_scoln-_sbeg+1)
        restscreen(l+_srow1-1,_scol1,l+_srow1-1,_scol2-1,hiattr(_sprpt))
        @ l+_srow1-1,_scol1+_sbeg-1 SAY x COLOR _SEL
        --l
        --_sm
        if l>_sm
          _si-=l
          hb_scroll(_srow1,_scol1-1,_srow2,_scol2,l-_sm)
          _srow1+=_sm
          _srow2:=_srow1+_si
          RESTSCREEN(_srow2+1,_scol1-1,_srowe,_scol2,hb_BSubStr(_scr,1+(_srow2+1-_srowb)*(_scoln+2)*D_REST))
          RESTSCREEN(_srowb,_scol1-1,_srow1-1,_scol2,_scr)
          @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
          @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
        else
          _srow1+=l
          hb_scroll(_srow1,_scol1,_srow2-1,_scol2-1,-min(_sm-l,_si-_sm))
          _srow1+=min(_sm-l,_si-_sm)
          _si-=l+min(_sm-l,_si-_sm)
          _srow2:=_srow1+_si
          RESTSCREEN(_srowb,_scol1-1,_srow1-1,_scol2,_scr)
          RESTSCREEN(_srow2,_scol1-1,_srowe,_scol2,hb_BSubStr(_scr,1+(_srow2-_srowb)*(_scoln+2)*D_REST))
          IF _sbf
             @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
             @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
          ELSE
             @ _srow1-1,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
             @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
          ENDIF
          @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
          @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
          _sef:=.t.
        endif
        FOR l=l TO 1 STEP -1
          ADEL(_srec,l)
        NEXT
        _sm:=1
        CUT(_s)
      else
        RESTSCREEN(_srow1-1,_scol1-1,_srow2,_scol2,hb_BSubStr(_scr,1+(_srow1-1-_srowb)*(_scoln+2)*D_REST))
        //_sbf:=.t.
        //_sef:=.f.
        _srow1+=_sm-1
        _srow2:=_srow1
        _sm:=1
        _si:=0
        _sexpgd(-2,_s,.t.,.f.)
      ENDIF
     ELSEif ! eval(_sinfo,_skey,_s)
      CLEAR TYPEAHEAD
      tone(130,3)
      _slth:=max(0,_slth-1)
      _spocz:=left(_spocz,len(_spocz)-1)
    ENDIF

RETURN .F.

FUNCTION _sprawo(_s,_skey)

local wiele,spb,l,sl,sp,sw

IF _si>0

  GO _srec[_sm]
  wiele=.f.
  sw:=len(ordKeyVal())
  //sw:=len(EvAlDb(IndexKey(0)))
  sl:=UpP(getscrtxt(_sprpt))
  sp:=eval(_spform,_spocz,_slth)
  do while _scoln-_sbeg+1>(l:=len(sp)) .and. len(_spocz)<sw
    spb:=_spocz

    if _sp2s<>NIL
      _spocz:=eval(_sp2s,eval(_ss2p,_spocz,_slth)+SUBSTR(sl,_sbeg+l,1),_slth+1)
    else
      _spocz+=SUBSTR(sl,_sbeg+l,1)
    endif
    ++_slth
    IF EvaldB(_swar,_spocz,_skon)
      @ _sm+_srow1-1,_scol1+_sbeg-1 SAY sp:=eval(_spform,_spocz,_slth) COLOR _SEL
      IF NEXTKEY()#_skey
        CUT(_s,,_skey)
      endif
      if nextkey()=_skey
        wiele=.t.
        inkey()
        loop
      ENDIF
    ELSE
      --_slth
      _spocz:=spb
    ENDIF
    exit
  enddo
  IF wiele
     CUT(_s)
  ENDIF
ENDIF
RETURN .F.

FUNCTION _send(_s,_skey)
local ltb,l,spb,spp,sp,sl,wiele,sw

IF _si>0
  wiele:=.f.
  go _srec[_sm]
  //sw:=len(EvAlDb(IndexKey(0)))
  sw:=len(ordKeyVal())
  sl:=UpP(getscrtxt(_sprpt))
  do while .t.
  ltb:=_slth
  do while _scoln-_sbeg+1>(l:=len(sp:=eval(_spform,_spocz,_slth))).and.len(_spocz)<sw
    spb:=_spocz
    spp:=SUBSTR(sl,_sbeg+l,1)
    if _sp2s<>NIL
      _spocz:=eval(_sp2s,eval(_ss2p,_spocz,_slth)+spp,_slth+1)
    else
      _spocz+=spp
    endif
    ++_slth
    IF !EvaldB(_swar,_spocz,_skon)
      --_slth
      _spocz:=spb
      exit
    elseif spp=" "
      sp:=eval(_spform,_spocz,_slth)
      exit
    endif
  enddo
  if _slth>ltb
    @ _sm+_srow1-1,_scol1+_sbeg-1 SAY sp COLOR _SEL
    CUT(_s,,_skey)
    if nextkey()=_skey
       inkey()
       wiele:=.t.
       loop
    endif
  endif
  exit
  enddo
  IF wiele
     CUT(_s)
  ENDIF
ENDIF
RETURN .F.

FUNCTION _slewo(_s,_skey)

  local l

  if _slth=0
     _sexpgd(0,_s)
  else
  do while _slth>0
    --_slth
    _spocz:=left(_spocz,len(_spocz)-1)
    //_sef=.F.
    //_sbf=.F.
    l=len(eval(_spform,_spocz,_slth))+_sbeg-1
    restscreen(_sm+_srow1-1,_scol1+l,_sm+_srow1-1,_scol2-1,hiattr(hb_BSubStr(_sprpt,l*D_REST+1)))
    setpos(_sm+_srow1-1,_scol1+l)
    if nextkey()#_skey .or. _slth=0 .OR. _si=0
      _sexpgd(0,_s,.f.,.f.)
      return .f.
    endif
    inkey()
  enddo
  endif
return .f.
*************************
*DOPISANIE WIERSZA U DOLU
*************************
PROCEDURE expd(_s)
local d:=if(_si=0,-2,if(_si=_srown,3,1))
IF _si<_srown
  IF _srow2<_srowe
    ++_srow2
   ELSE
    --_srow1
    hb_scroll(_srow1-1,_scol1-1,_srow2-1,_scol2,1)
  ENDIF
  @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││┘─└│' COLOR _SRAMKA
  @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
  ++_si
 ELSE
  IF _sbf
    @ _srow1-1,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
    @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
    _sbf=.F.
  ENDIF
  ADEL(_srec,1)
  hb_scroll(_srow1,_scol1,_srow2-1,_scol2-1,1)
ENDIF
_srec[_si]:=RECNO()
@ _srow2-1,_scol1 say padr(eval(_sprompt,d,_s),_scol2-COL())
RETURN

*************************
*DOPISANIE WIERSZA U GORY
*************************
PROCEDURE expg(_s)
local d:=if(_si=0,2,if(_si=_srown,-3,-1))
IF _si<_srown
  IF _srow1=_srowb+1
    ++_srow2
    hb_scroll(_srow1,_scol1-1,_srow2,_scol2,-1)
   ELSE
    --_srow1
  ENDIF
  @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '┌─┐││ ││' COLOR _SRAMKA
  @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
  ++_si
 ELSE
  IF _sef
    @ _srow2,_scol1-1 BOX '└'+REPLICATE('─',_scoln)+'┘' UNICODE COLOR _SRAMKA
    @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
    _sef=.F.
  ENDIF
  hb_scroll(_srow1,_scol1,_srow2-1,_scol2-1,-1)
ENDIF
AINS(_srec,1)
_srec[1]=RECNO()
@ _srow1,_scol1 say padr(eval(_sprompt,d,_s),_scol2-COL())
RETURN

***********************************************
*PROCEDURA POWIEKSZAJACA TABELKE W GORE I W DOL
***********************************************
FUNCTION _Sexpgd(D,_s,b,e)
local crsr:=0,obf:=_sbf,oef:=_sef

#ifdef A_MYSZ
  #define D_MYSZNE NEXTKEY(INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)#0
  #define D_MYSZE NEXTKEY(INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)=0
#else
  #define D_MYSZNE nextkey()#0
  #define D_MYSZE nextkey()=0
#endif
if b#NIL
   _sbf:=b
endif
if e#NIL
   _sef:=e
endif
IF _sbf .and. _sef .or. _si#0 .and. D_MYSZNE
  return .f.
endif
    IF _si=0
      oef:=obf:=NIL
      do while .t.
      _srec[1]:=RECNO()
        IF obf:=_sbf
          @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
          @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
        ELSE
          @ _srow1-1,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
          @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
        ENDIF
        IF oef:=_sef
          @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
          @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
        ELSE
          @ _srow2,_scol1-1 BOX '└'+REPLICATE('─',_scoln)+'┘' UNICODE COLOR _SRAMKA
          @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
        ENDIF
      IF crsr=1
         exit
      endif

      if _sef .or. !_skip(0,,_s) //w górę, bo w dół sie nie da
         d:=2 //
         if _sbf .or. !(dbgoto(_srec[1]),_skip(-1,,_s))

            if _sbeg>0 .and. nextkey()=0
               if dbseek(_spocz) .and. _skip(0,,_s)
                  _sbf:=.t.
                  _sef:=.f.
                  crsr:=1
                  loop
               elseif _slth>0
                  _spocz:=LEFT(_spocz,len(_spocz)-_slth)
                  _slth:=0
                  _sbf:=_sef:=.f.
                  if !eof() .and. EvaldB(_swar,_spocz,_skon) .or. (_sef:=_skip(-1,,_s)) .or. (_sbf:=dbseek(_spocz))
                    crsr:=1
                    loop
                  endif
               endif
            endif

            if _sbf .and. _sbf#obf
               @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
               @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
            endif
            if _sef .and. _sef#oef
	             @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
               @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
            endif
            _sm:=1
            return .f.
         endif
      elseif _sbf
         d:=-2
      endif

      exit
      enddo

      if _srowe>_srow2 .and. !_sef
        ++_srow2
        @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││┘─└│' COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
        oef:=.f.
      elseif _srowb<_srow1-1 .and. !_sbf
        --_srow1
        @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '┌─┐││ ││' COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
        obf:=.f.
      elseif _srowe>_srow2
        ++_srow2
#ifdef __PLATFORM__DOS
        @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││┘═└│' COLOR _SRAMKA
#else        
        @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││╛═╘│' COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
#endif        
        oef:=.t.
      else
        --_srow1
#ifdef __PLATFORM__DOS
        @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '┌═┐││ ││' COLOR _SRAMKA
#else
        @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '╒═╕││ ││' COLOR _SRAMKA
#endif
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
        obf:=.t.
      endif
      _si:=1
      _sm:=1
      _srec[1]=RECNO()
      @ _srow1,_scol1 SAY padr(eval(_sprompt,d,_s),_scol2-COL())
      if _sbeg>0
           HIGHLIGHT LINE _srow1
      else
           SAVE LINE _srow1
           ALTERNATE LINE _srow1 BUFFER _sprpt ATTRIB 119
      endif
    endif
    crsr:=setcursor(0)
    IF !_sef .and. _srowe>_srow2
      GO _srec[_si]
      DO WHILE _srowe>_srow2 .AND. D_MYSZE .and._skip(1,0,_s)
        ++_srow2
        ++_si
        _srec[_si]=RECNO()
        @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││┘─└│' COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
        @ _srow2-1,_scol1 say padr(eval(_sprompt,1,_s),_scol2-COL())
        oef:=.f.
      ENDDO
    ENDIF
    if !_sbf .and. _srow1>_srowb+1
      go _srec[1]
      DO WHILE _srow1>_srowb+1 .AND. D_MYSZE .AND. _skip(-1,0,_s)
        ++_sm
        --_srow1
        @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '┌─┐││ ││' COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
        obf:=.f.
        AINS(_srec,1)
        _srec[1]=RECNO()
        @ _srow1,_scol1 say padr(eval(_sprompt,-1,_s),_scol2-COL())
        ++_si
      ENDDO
    endif
    IF _si<_srown
      if _sef
        if !_sbf
          go _srec[1]
          DO WHILE _si<_srown .AND. D_MYSZE .and. _skip(-1,0,_s)
            ++_sm
            ++_srow2
            hb_scroll(_srow1,_scol1-1,_srow2,_scol2,-1)
            @ _srow1-1,_scol1-1,_srow1,_scol2 BOX UNICODE '┌─┐││ ││' COLOR _SRAMKA
            @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
            obf:=.f.
            AINS(_srec,1)
            _srec[1]=RECNO()
            @ _srow1,_scol1 say padr(eval(_sprompt,-1,_s),_scol2-COL())
            ++_si
          ENDDO
        endif
       ELSE
        GO _srec[_si]
        DO WHILE _si<_srown .AND. D_MYSZE .AND. _skip(1,0,_s)
          --_srow1
          ++_si
          _srec[_si]=RECNO()
          hb_scroll(_srow1-1,_scol1-1,_srow2-1,_scol2,1)
          @ _srow2-1,_scol1-1,_srow2,_scol2 BOX UNICODE '│ ││┘─└│' COLOR _SRAMKA
          @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
          @ _srow2-1,_scol1 say padr(eval(_sprompt,1,_s),_scol2-COL())
          oef:=.f.
        ENDDO
      ENDIF
    ENDIF
    IF _sbf#obf
    IF _sbf
        @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
    ELSE
        @ _srow1-1,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
    ENDIF
    ENDIF
    IF _sef#oef
       IF _sef
        @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
       ELSE
        @ _srow2,_scol1-1 BOX '└'+REPLICATE('─',_scoln)+'┘' UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
       ENDIF
    ENDIF
  _sm:=max(1,min(_si,_sm))
#ifdef D_LAN
   go _srec[_sm]
#endif
   setcursor(crsr)

RETURN .F.
*******************************************************
PROCEDURE REFRESH(x,_s)
local i

_srow1:=_srow1+_sm-1
if x=NIL
  RESTSCREEN(_srow1-_sm,_scol1-1,_srow2,_scol2,hb_BSubStr(_scr,1+(_srow1-_sm-_srowb)*(_scoln+2)*D_REST))
  if _srow1=_srowb+1
     ++_srow1
  endif
  _srow2:=_srow1
  _si:=0
else
  for i:=_sm-1 to 1 step -1
    adel(_srec,i)
  next
  if _sm > 1
    RESTSCREEN(_srow1-_sm,_scol1-1,_srow1-2,_scol2,hb_BSubStr(_scr,1+(_srow1-_sm-_srowb)*(_scoln+2)*D_REST))
  endif
  if _srow1+x < _srow2
    RESTSCREEN(_srow1+x+1,_scol1-1,_srow2,_scol2,hb_BSubStr(_scr,1+(_srow1+x+1-_srowb)*(_scoln+2)*D_REST))
  endif
  _srow2:=_srow1+x
  _si:=x
  @ _srow1-1,_scol1-1,_srow2,_scol2 BOX UNICODE '┌─┐│┘─└│' COLOR _SRAMKA
  @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
  @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
  _sef=.F.
  _sbf=.F.
endif
  _sm:=1
  _sexpgd(0,_s,.f.,.f.)

RETURN
**********************************************
function CUT(_s,zmiana,key)
   LOCAL z:=.f.,l,i,scr1,scr2,crsr,b,obf:=_sbf,oef:=_sef
   DEFAULT zmiana TO .t.
    _sef:=_sbf:=.f.
    if _si=0
       return _sexpgd(0,_s)
    endif
    l:=_sm-1
    go _srec[_sm]
    crsr:=setcursor(0)
    DO WHILE !z .and. D_MYSZE .and. l>0 .and. l+_srow1-1>_srowb .and. _skip(-1,0,_s)
      z:=_srec[l]#(_srec[l]:=recno())
      scr1:=savescreen(l+_srow1-1,_scol1,l+_srow1-1,_scol2-1)
      @ _srow1+l-1,_scol1 say padr(eval(_sprompt,-1,_s),_scol2-COL())
      scr2:=savescreen(l+_srow1-1,_scol1,l+_srow1-1,_scol2-1)
      if !scr1==scr2 .or. z //.or. SubStr(scr1,2,1)>chr(7)
         zmiana := .t.
         ALTERNATE LINE _srow1+l-1 BUFFER scr2 ATTRIB if(iscolor(),16,8)
      endif
      --l
    ENDDO
    zmiana:=zmiana .or. _sbf
    if l>0 .and. zmiana .and. nextkey()#key
      _srow1+=l
      _sm-=l
      _si-=l
      FOR i:=1 to l
        ADEL(_srec,1)
      NEXT
      RESTSCREEN(_srowb,_scol1-1,_srow1-1,_scol2,_scr)
      IF _sbf
        @ _srow1-1,_scol1-1 BOX TOPD UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO2(_snagl) COLOR _SRAMKA
      ELSE
        @ _srow1-1,_scol1-1 BOX '┌'+REPLICATE('─',_scoln)+'┐' UNICODE COLOR _SRAMKA
        @ _srow1-1,_scol1+_snagkol SAY RNTO1(_snagl) COLOR _SRAMKA
      ENDIF
    else
      _sbf:=obf
    endif
    l:=_sm
begin sequence
    go _srec[l]
    if !_skip(0,,_s)
      --l
      --_sm
      break
    endif
    z:=_srec[l]#(_srec[l]:=recno())
    scr1:=_sprpt
    REFRESH LINE _srow1+l-1 DIRECTION 0
    if scr1#_sprpt .or. z //.or. SubStr(scr1,2,1)>chr(7)
       ALTERNATE LINE _srow1+l-1 BUFFER _sprpt ATTRIB if(iscolor(),72,142)
       ZMIANA :=.T.
    else
       restscreen(_srow1+l-1,_scol1,_srow1+l-1,_scol2-1,hiattr(_sprpt))
    endif
    PROMPT LINE _sm+_srow1-1
    DO WHILE !z .and. l<_si .and. D_MYSZE .and. l+_srow1-1<_srowe .and. _skip(1,0,_s)
       ++l
       z:=_srec[l]#(_srec[l]:=recno())
      scr1:=savescreen(l+_srow1-1,_scol1,l+_srow1-1,_scol2-1)
      @ _srow1+l-1,_scol1 say padr(eval(_sprompt,1,_s),_scol2-COL())
      scr2:=savescreen(l+_srow1-1,_scol1,l+_srow1-1,_scol2-1)
      if !scr1==scr2 .or. z //.or. SubStr(scr1,2,1)>chr(7)
         zmiana := .t.
         ALTERNATE LINE _srow1+l-1 BUFFER scr2 ATTRIB if(iscolor(),16,8)
      endif
    ENDDO
end sequence
    ZMIANA:=ZMIANA .OR. _sef
    if _si>l .AND. ZMIANA .and. nextkey()#key
      _si:=l
      _srow2=_srow1+_si
      RESTSCREEN(_srow2+1,_scol1-1,_srowe,_scol2,hb_BSubStr(_scr,1+(_srow2+1-_srowb)*(_scoln+2)*D_REST))
      IF _sef
        @ _srow2,_scol1-1 BOX BOTD UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO2(_snagl) COLOR _SRAMKA
      ELSE
        @ _srow2,_scol1-1 BOX '└'+REPLICATE('─',_scoln)+'┘' UNICODE COLOR _SRAMKA
        @ _srow2,_scol1+_snagkol SAY RSTO1(_snagl) COLOR _SRAMKA
      ENDIF
    else
      _sef:=oef
    endif
  setcursor(crsr)
  return _sexpgd(0,_s)

*************
function _skip(p,bl,_s)
local k
if p=0
  if deleted()
    skip
  else
    GO RECNO() //czyszcze bof()
  endif
  p=1
ELSE
  skip p
endif

do while !EOF() .and. EvaldB(_swar,_spocz,_skon) .and. !BOF()
  if (_sfor=NIL.or.eval(_sfor)).and.(_sfilb=NIL.or.eval(_sfilb))
    return .t.
  elseif bl=NIL
    if nextkey()=27
       return .f.
    endif
#ifdef A_MYSZ
  elseif (k:=NEXTKEY(INKEY_KEYBOARD + INKEY_LDOWN+ INKEY_RDOWN),!hb_keyChar(k)=="") .and. k<>bl .or. k>=K_MINMOUSE .and. k<=K_MAXMOUSE .and. (bl:=if(k=1002,1,2),k:=0,.t.)
#else
  elseif (k:=nextkey())#0 .and. k#bl
#endif
    return .f.
  endif
  skip p
enddo
do case
  case p<0;_sbf=.t.
  case p>0;_sef=.t.
endcase
return .f.
***************
