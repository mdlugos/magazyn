#include "dm_form.ch"
#include "getexitm.ch"
#include "inkey.ch"

#define D_REST 4
//iif(hb_gtInfo( HB_GTI_COMPATBUFFER ),2,4)

#ifdef A_MYSZ
#include "button.ch"

static proc drag_mysz(_f,w)
   local dr:=_frow-_fscr0,mr:=_fskip*(_fl-_fj+1)+_frow,ret
   local blo:={|dy,r,i|r:=_fscr0+dr+dy, i:=Int((w[3]-r)/_fskip)+1, dy+_fscr0<0 .or. _fskip*i+r>maxrow()}
   if mr<MaxRow()
      _fscr:=hb_BLeft(_fscr,D_REST*(_fco2-_fco1+1)*(mr+1-_fscr0))
   endif
   ret:=mousedr(@w[1],@w[2],@w[3],@_fscr0,@_fco1,@mr,@_fco2,@_fscr,blo)
   if mr<MaxRow()
      _fscr+=savescreen(mr+1,_fco1,MaxRow(),_fco2)
   endif
   if ret
      _frow:=_fscr0+dr
      if mr<MaxRow()
         //moved up _fscr+=savescreen(mr+1,_fco1,MaxRow(),_fco2)
      elseif _frow+(_fl-_fj+1)*_fskip>maxrow()
         _fl:=Int((maxrow()-_frow)/_fskip)+_fj-1
         RESTSCREEN(_fskip*(_fl-_fj+1)+_frow,_fco1,mr,_fco2,hb_BSubStr(_fscr,D_REST*(_fco2-_fco1+1)*(_fskip*(_fl-_fj+1)+_frow-_fscr0)+1))
         @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╙'+replicate('─',_fco2-_fco1-1)+'╜' UNICODE COLOR _sbkgr
      endif
   endif   
return

#endif

PROCEDURE FORM_EDIT(_f)
local stat,rmpos,getlist,job
*parameters _fco1,_fco2,_frow,_fskip,_flpmax,_fdmpre,_fdmget,_fdmpost,_fmainpre,_fmainget,_fmainpost,_flastexit

asize(_f,max(len(_f),_fLEN))

DEFAULT _flp TO _flpmax
_fi:=1
_fj:=0
_fl:=1
_fpos:=1
_fposg:=1
_fpopkey:=.f.
// -_fco1 - ile pozycji ma sie mieścić na ekranie
if _fscr0=NIL .and. _fco1<>NIL .and. _fco1<0
   job:=(_fskip*(_fco1-1))-_frow
   _fscr0:=min(max(0,row()+int(job/2)),max(0,MaxRow()+job))
   _frow+=_fscr0
   _fco1:=NIL
else
   DEFAULT _fscr0 TO 0
endif
if _fco2<0
   DEFAULT _fco1 TO col()
   _fco1:=min(_fco1,max(0,maxcol()+_fco2))
   _fco2:=min(_fco1-_fco2,maxcol())
else
   DEFAULT _fco1 TO 0
endif
_fscr:=savescreen(_fscr0,_fco1,MaxRow(),_fco2)
//_fnowy:=.f.

  begin sequence

  set cursor off

  set color to (_snorm)

  eval(_fdmpre,_f)

  job:=NIL

  DO WHILE .T.

    SET COLOR TO (_snorm)

#ifdef A_MYSZ
    if job#NIL .and. readkey()=GE_MOUSE
       job:=readkey(,)
       if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>_fco2 .or. job[3]<=_fscr0 .or. job[3]>_frow+_fskip-1
         exit
       endif
       drag_mysz(_f,job)
    else
      job:=NIL
    endif
#endif

__SetProc(procname(1))

getlist:={}
eval(_fdmget,_f,getlist)

rmpos:=_fposg

#ifdef A_MYSZ
if job#NIL
   job:=ascan(getlist,{|g|g:hitTest(job[3],job[2])=HTCLIENT})
   if job#0
     rmpos:=job
   endif
endif
#endif

READmodal(getlist,@rmpos)
//#ifdef A_HBGET
    //_fposg:=__GetListLast():ReadStats( 14 ) //GetListPos()
//#else
    _fposg:=rmpos
//#endif
    _fkey:=ReadkeY()

    IF _fkey=K_ESC
       exit
    ENDIF

    eval(_fdmpost,_f,getlist)

#ifdef A_MYSZ
    if _fkey=GE_MOUSE
       job:=readkey(,)
       if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>_fco2 .or. job[3]<=_fscr0 .or. job[3]>_frow+(_fl-_fj+1)*_fskip
           exit
       endif
       if job[3]<=_frow+_fskip-1
          loop
       endif
    endif
#endif
    IF _fkey=K_PGUP
      loop
    elseif _fkey=K_CTRL_L //.or. _fkey=K_CTRL_W // ^End
      exit
    endif


    eval(_fmainpre,_f)
    // editing - .t. ,scroll down - .f.
    stat:=(_fi=1 .and. _fl=1) 
#ifdef A_MYSZ
    if _fkey=GE_MOUSE //.and. job[3]<=_frow+(_fl-_fj+1)*_fskip
        _fkey:=max(min(_fl,int((job[3]-_frow)/_fskip)+_fj),1+_fj)
        skip _fkey-_fi
        _fi:=_fkey
        drag_mysz(_f,job)
    endif
#endif
    if nextkey()=0 .and. !stat .and. !_fpopkey .and. _flp>1
       stat:={_fi,recno()}
       _fl:=max(_fl,_fi)
       _fj:=max(0,_fl-int((MaxRow()-_frow)/_fskip)+1)
       if _fi-1<_fj .and. _fscr0>0
         SET COLOR TO (_sbkgr)
         job:=min(_fscr0,_fskip*(1+_fj-_fi))
         _fscr0-=job
         _frow-=job
         _fj:=max(0,_fl-int((MaxRow()-_frow)/_fskip)+1)
         _fscr:=savescreen(_fscr0,_fco1,_fscr0+job-1,_fco2)+_fscr
         hb_scroll(_fscr0,_fco1, _frow+_fskip*(_fi-_fj+1)+job, _fco2, job)
       endif
       skip _fj+1-_fi
       _fl:=_fi:=_fj+1
       _fpos:=0
       while _fi<=_flp .and. _frow+_fskip*(_fi-_fj+1)<=MaxRow()+_fscr0
         _fl:=_fi
         SET COLOR TO (_sbkgr)
         if _frow+_fskip*(_fi-_fj+1)>MaxRow()
            job:=_frow+_fskip*(_fi-_fj+1)-MaxRow()
            _fscr0-=job
            _frow-=job
            _fscr:=savescreen(_fscr0,_fco1,_fscr0+job-1,_fco2)+_fscr
            hb_scroll(_fscr0,_fco1, _frow+_fskip*(_fi-_fj)+job-1, _fco2, job)
         endif
         @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX UNICODE if(_flp>_fl,'║ ║║╜─╙║ ','║ ║║╝═╚║ ')
         _fk:=_frow+_fskip*(_fi-_fj)
         job:=right(lTrim(sTr(_fi)),4)
         @ _fk,_fco1+max(0,4-len(job)) say job+'.'
         SET COLOR TO (_SNORM)
         getlist:={}
         _fkey:=nextkey()
         eval(_fmainget,_f,getlist)
         if _fpopkey .or. _fkey<>0
            _fpopkey:=.t.
            exit
         endif
         skip
         ++_fi
       enddo
       if !_fpopkey
         _fi:=stat[1]
          goto stat[2]
         _fpopkey:=.t.
         _fk:=_frow+_fskip*(_fi-_fj)
         @ _fk,_fco1 SAY '>' COLOR _sbkgr
         @ _fk,_fco2 SAY '<' COLOR _sbkgr
         @ _fk+_fskip-1,_fco1 SAY '>' COLOR _sbkgr
         @ _fk+_fskip-1,_fco2 SAY '<' COLOR _sbkgr
       endif
       stat:=.t.
    endif

        DO WHILE .t.

  SET COLOR TO (_SNORM)

      if _fi>_fl
         // obcięcie dołu po drag w nagłówku
         skip _fl-_fi
         _fi:=_fl
      endif
      
      _fk:=_frow+_fskip*(_fi-_fj)

      begin sequence

      __SetProc(procname(1))
      getlist:={}
#ifdef A_MYSZ
      job:=0
      if (_fkey:=nextkey(INKEY_KEYBOARD + INKEY_LDOWN))#0
         if _fkey=K_LBUTTONDOWN
           _fkey:=0
           job:=1
         endif
#else
      if (_fkey:=nextkey())#0
#endif
         if !(_fkey=K_ESC .or. _fkey=K_PGDN .or. _fkey=K_PGUP .or. _fkey=K_CTRL_L /* .or. _fkey=K_CTRL_W */ )
            _fkey:=0 // read
            stat:=.t. // repaint
         endif
         _fpopkey:=.t.
      elseif !_fpopkey .and. _fi<_flp
         if _frow+_fskip*(_fi-_fj+1)>MaxRow() .and. _frow+_fskip*(_fi-_fj+1)<=MaxRow()+_fscr0
            SET COLOR TO (_sbkgr)
            job:=MaxRow()-_frow+_fskip*(_fi-_fj+1)
            _fscr0-=job
            _frow-=job
            _fscr:=savescreen(_fscr0,_fco1,_fscr0+job-1,_fco2)+_fscr
            hb_scroll(_fscr0,_fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2, job)
            if _fskip<2
               @ _frow+_fskip*(_fi-_fj), _fco1 BOX '║' UNICODE
               @ _frow+_fskip*(_fi-_fj), _fco2 BOX '║' UNICODE
             else
               @  _frow+_fskip*(_fi-_fj), _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2 BOX UNICODE '║ ║║║ ║║'
             endif
             SET COLOR TO (_SNORM)
         endif
         _fkey:=K_PGDN  // refr
      else
         stat:=.t.
      endif

      rmpos:=_fpos
      if stat
         _fpos:=0
         job:=right(lTrim(sTr(_fi)),4)
         @ _fk,_fco1 BOX '║   .' UNICODE color _sbkgr
         @ _fk,_fco1+max(0,4-len(job)) say job color _sbkgr
         eval(_fmainget,_f,getlist)
         if _fpos=0
            _fpos:=rmpos
#ifdef A_MYSZ
            stat:=.f.
#endif
         else
            rmpos:=_fpos
         endif
      else
         _fnowy:=.f.
      endif
      if !_fpopkey .and. _fkey#0 .and. !(_fi=1 .and. _fnowy)
         if _fnowy
            _flp:=_fi
            _fpopkey:=.t.
         endif
      elseif _fpopkey .and. !_fnowy .and. _fkey#0
         inkey()
      else
         _fpopkey:=.t.
         if _fnowy
            @ _fk,_fco1+4 say "*" color _sbkgr
            _flp:=_fi
         endif
#ifdef A_MYSZ
         if !stat .and. readkey()=GE_MOUSE
           job:=readkey(,)
           job:=ascan(getlist,{|g|g:hitTest(job[3],job[2])=HTCLIENT})
           if job#0
              rmpos:=job
           endif
         endif
#endif
         READmodal(getlist,@rmpos)
//#ifdef A_HBGET
         //_fpos:=__GetListLast():ReadStats( 14 ) //GetListPos()
//#else
         _fpos:=rmpos
//#endif
         _fkey:=ReadkeY()
         eval(_fmainpost,_f,getlist)
         if !_fnowy
            @ _fk,_fco1+4 say "." color _sbkgr
         endif

      ENDIF

      recover
        exit
      end

      SET COLOR TO (_sbkgr)

            stat:= .f.

#ifdef A_MYSZ
        if _fkey=GE_MOUSE .and. !_fnowy
           job:=readkey(,)
           if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>_fco2 .or. job[3]<_frow+_fskip-1 .or. job[3]>_frow+_fskip*(_fl-_fj+1)
              exit //_fkey:=K_CTRL_L //K_CTRL_W

           elseif job[3]=_frow+_fskip-1 .and. _fj>0
              job:=_fi-_fj
              _fi-=job
              stat:=.t.
              --_fj
              if _fskip*(_fl-_fj+1)+_frow>MaxRow()
                 --_fl
                 if _fl=_flp-1
                     @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╙'+replicate('─',_fco2-_fco1-1)+'╜' UNICODE
                 endif
              else
                 @ (_fl-_fj)*_fskip+_frow,_fco1,(_fl-_fj+1)*_fskip+_frow,_fco2 BOX UNICODE '║ ║║╝═╚║'
              endif
              hb_scroll(_fskip+_frow,_fco1,_frow+_fskip*(_fl-_fj+1)-1,_fco2,-_fskip)
              if _fskip<2
                @  _fskip+_frow,_fco1 BOX '║' UNICODE
                @  _fskip+_frow,_fco2 BOX '║' UNICODE
              else
                @  _fskip+_frow,_fco1,2*_fskip+_frow-1,_fco2 BOX UNICODE '║ ║║║ ║║'
              endif
              Skip -job
              loop
           elseif job[3]=_frow+_fskip*(_fl-_fj+1) .and. _fl<_flp
              job:=_fl-_fi+1
              skip job
              _fi+=job
              if _frow+_fskip*(_fi-_fj+1)>MaxRow()
                stat:=.t.
                ++_fj
                ++_fl
                hb_scroll(_frow+_fskip,_fco1,_frow+_fskip*(_fi-_fj+1)-1,_fco2,_fskip)
                if _fskip<2
                  @  _fskip*(_fi-_fj)+_frow,_fco1 BOX '║' UNICODE
                  @  _fskip*(_fi-_fj)+_frow,_fco2 BOX '║' UNICODE
                else
                  @  _frow+_fskip*(_fi-_fj),_fco1,_frow+_fskip*(_fi-_fj+1)-1,_fco2 BOX UNICODE '║ ║║║ ║║'
                endif
              endif
              if _fi>_fl
                stat:=.t.
                 @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX UNICODE '║ ║║╜─╙║ '
                 ++_fl
              endif
              IF _flp<=_fi
                 @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╚'+replicate('═',_fco2-_fco1-1)+'╝' UNICODE
              ENDIF
              loop
           else
              _fkey:=max(min(_flp,int((job[3]-_frow)/_fskip)+_fj),1)
              skip _fkey-_fi
              _fi:=_fkey
              drag_mysz(_f,job)
              loop
           endif
        endif
#endif
            DO CASE

              CASE _fnowy //przenumeruj po delete
                 --_flp
                 if _flp>0 .and. _fi<=_flp
                    skip
                    if _fi<_fl
                       hb_scroll(_fk,_fco1,_frow+_fskip*(_fl-_fj+1),_fco2,_fskip)
                       --_fl
                       RESTSCREEN(1+_fskip*(_fl-_fj+1)+_frow,_fco1,MaxRow(),_fco2,hb_BSubStr(_fscr,D_REST*(_fco2-_fco1+1)*(1+_fskip*(_fl-_fj+1)+_frow-_fscr0)+1))
                       *********
                       _fkey:=_fi
                       do while _fk<=_frow+_fskip*(_fl-_fj)
                          job:=right(lTrim(sTr(_fkey++)),4)
                          @ _fk,_fco1 BOX '║   .' UNICODE color _sbkgr
                          @ _fk,_fco1+max(0,4-len(job)) say job color _sbkgr
                          //@ _fk,_fco1+1 say str(_fkey++,3)+'.' color _sbkgr
                          _fk+=_fskip
                       enddo
                       ************
                    endif
                 elseIF _fi>1
                     --_fi
                    --_fl
                   skip -1
                   if _fj>0 .and. _fi=_fj
                      --_fj
                      stat:=.t.
                   else
                      RESTSCREEN(_fskip*(_fl-_fj+1)+_frow,_fco1,MaxRow(),_fco2,hb_BSubStr(_fscr,D_REST*(_fco2-_fco1+1)*(_fskip*(_fl-_fj+1)+_frow-_fscr0)+1))
                      @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╚'+replicate('═',_fco2-_fco1-1)+'╝' UNICODE
                   endif
                else
                   skip
                   //hb_scroll(_fk,_fco1,_fk+_fskip-1,_fco2,0)
                   if _fskip<2
                     @ _frow+_fskip*(_fi-_fj), _fco1 BOX '║'+space(_fco2-_fco1-1)+'║' UNICODE
                   else
                     @  _fk,_fco1,_fk+_fskip-1,_fco2 BOX UNICODE '║ ║║║ ║║ '
                   endif
                   exit
                endif

             CASE _fkey=K_ESC .or. _fkey=K_CTRL_L // .or. _fkey=K_CTRL_W //ctrl end
                exit

             CASE _fkey=K_PGUP // pgup
                   --_fi
                   IF _fi=0
                    _fi:=1
                    exit
                   ELSEif _fi-_fj=0
                    --_fj
                    if _fskip*(_fl-_fj+1)+_frow>MaxRow()+_fscr0
                         --_fl
                        if _fl=_flp-1
                          @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╙'+replicate('─',_fco2-_fco1-1)+'╜' UNICODE
                        endif
                        hb_scroll(_fskip+_frow,_fco1,_frow+_fskip*(_fl-_fj+1)-1,_fco2,-_fskip)
                     else
                        stat:=min(_fskip,_fscr0)
                        if stat>0
                           _fscr0-=stat
                           _frow-=stat
                           _fscr:=savescreen(_fscr0,_fco1,_fscr0+stat-1,_fco2)+_fscr
                           hb_scroll(_fscr0,_fco1, _fskip+_frow+stat-1, _fco2, stat)
                        endif
                        if stat<_fskip
                           hb_scroll(_fskip+_frow+stat,_fco1, _frow+_fskip*(_fl-_fj+1), _fco2, stat-_fskip)
                        endif
                    endif
                    if _fskip<2
                      @  _fskip+_frow,_fco1 BOX "║" UNICODE
                      @  _fskip+_frow,_fco2 BOX "║" UNICODE
                    else
                      @  _fskip+_frow,_fco1,2*_fskip+_frow-1,_fco2 BOX UNICODE '║ ║║║ ║║'
                    endif
                    stat:=.t.
                 ENDIF
                 Skip -1

              CASE _fi=_flp .AND. _fkey=K_ENTER .or. _fi=_flpmax
               IF _flastexit#NIL 
                  job:={_fscr0,_fco1,_fskip*(_fl-_fj+1)+_frow,_fco2,_fscr,}
                  job[6]:=TAK('CZY KONIEC WPROWADZANIA',_fk+_fskip,_fco2-50,.F.,.F.,,job)
                  if _fscr0<>job[1] .or. _fco1<>job[2]
                     _frow+=job[1]-_fscr0
                     _fscr0:=job[1]
                     _fco1:=job[2]
                     _fco2:=job[4]
                     _fscr:=job[5]+savescreen(job[3]+1,job[2],maxrow(),job[4])
                  endif
                  if job[6]
                    EVAL(_flastexit,_f)
                    break
                  endif
               ENDIF

              case _fkey=K_PGDN .or. _fkey=K_ENTER // PgDn
            skip
            ++_fi
          if _frow+_fskip*(_fi-_fj+1)>MaxRow()
            ++_fl
            if _frow+_fskip*(_fi-_fj+1)<=MaxRow()+_fscr0
               stat:=_frow+_fskip*(_fi-_fj+1)-MaxRow()
               _fscr0-=stat
               _frow-=stat
               _fscr:=savescreen(_fscr0,_fco1,_fscr0+stat-1,_fco2)+_fscr
               hb_scroll(_fscr0,_fco1, _frow+_fskip*(_fi-_fj)+stat-1, _fco2, stat)
               if stat<_fskip
                  hb_scroll(_frow+_fskip*(_fi-_fj)+stat,_fco1, Maxrow(), _fco2, stat-_fskip)
               endif
            else
               ++_fj
               hb_scroll(_frow+_fskip, _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2, _fskip)
            endif
            if _fl=_fi
               if _fskip<2
               @ _frow+_fskip*(_fi-_fj), _fco1 BOX '║' UNICODE
               @ _frow+_fskip*(_fi-_fj), _fco2 BOX '║' UNICODE
               else
               @  _frow+_fskip*(_fi-_fj), _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2 BOX UNICODE '║ ║║║ ║║'
               endif
            endif
            stat:=.t.
          endif
          if _fi>_fl
            stat:=.t.
            @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX UNICODE '║ ║║╜─╙║ '
            ++_fl
          endif
          IF _flp<=_fi
            @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╚'+replicate('═',_fco2-_fco1-1)+'╝' UNICODE
          ENDIF
            ENDCASE
         ENDDO
  enddo
  recover using stat
    if stat#NIL
      RESTSCREEN(_fscr0,_fco1,MaxRow(),_fco2,_fscr)
      set cursor off
      SET COLOR TO (_SNORM)
      break(stat)
    endif
  end sequence
  RESTSCREEN(_fscr0,_fco1,MaxRow(),_fco2,_fscr)
  set cursor off
  SET COLOR TO (_SNORM)

RETURN
*************
