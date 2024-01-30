#include "dm_form.ch"
#include "getexit.ch"
#include "inkey.ch"


PROCEDURE FORM_EDIT(_f)
local stat,rmpos,getlist,job
*parameters _fco1,_fco2,_frow,_fskip,_flpmax,_fdmpre,_fdmget,_fdmpost,_fmainpre,_fmainget,_fmainpost,_flastexit

asize(_f,_fLEN)

DEFAULT _flp TO _flpmax
_fi:=1
_fj:=0
_fl:=1
_fpos:=1
_fposg:=1
_fpopkey:=.f.
_fscr:=savescreen(0,_fco1,maxrow(),_fco2)
//_fnowy:=.f.

  begin sequence

  set cursor off

  set color to (_snorm)

  eval(_fdmpre,_f)

  DO WHILE .T.

    SET COLOR TO (_snorm)

    __SetProc(procname(1))

    getlist:={}
    eval(_fdmget,_f,getlist)

    rmpos:=_fposg
#ifdef A_MYSZ
    if job#NIL .and. readkey()=GE_MOUSEHIT
       job:=readkey(,)
       if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>=_fco2 .or. job[3]=0 .or. job[3]>_frow+_fskip
          exit
       endif
       job:=ascan(getlist,{|g|g:row=job[3] .and. g:col<=job[2] .and. g:col+len(tran(g:varGet(),g:picture))-1>=job[2]})
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
    if _fkey=GE_MOUSEHIT
           job:=readkey(,)
           if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>=_fco2 .or. job[3]=0 .or. job[3]>_frow+(_fl-_fj+1)*_fskip
              exit
           endif
           if job[3]<_frow+_fskip-1
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

    stat:=(_fi=1 .and. _fl=1)
#ifdef A_MYSZ
    if _fkey=GE_MOUSEHIT .and. job[3]<=_frow+(_fl-_fj+1)*_fskip
       job:=max(min(_fl,int((job[3]-_frow)/_fskip)+_fj),1+_fj)
        skip job-_fi
        _fi:=job
    endif
#endif

    if nextkey()=0 .and. !stat .and. !_fpopkey .and. _flp>1
       stat:={_fi,recno()}
       _fj:=max(0,max(_fl,_fi)-int((maxrow()-_frow)/_fskip)+1)
       skip _fj+1-_fi
       _fl:=_fi:=_fj+1
       _fpos:=_fkey:=0
       while _fi<=_flp .and. _frow+_fskip*(_fi-_fj+1)<=maxrow()
         @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX IF( _flp<=_fi,'º ºº¼ÍÈº ','º ºº¼ÄÈº ') COLOR _sbkgr
         _fl:=_fi
         _fk:=_frow+_fskip*(_fi-_fj)
         job:=right(ltrim(str(_fi,6,0)),4)
         @ _fk,_fco1+max(0,4-len(job)) say job+'.' color _sbkgr
         getlist:={}
         SET COLOR TO (_SNORM)
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

      _fk:=_frow+_fskip*(_fi-_fj)


      begin sequence

      __SetProc(procname(1))
      getlist:={}
#ifdef A_MYSZ
      job:=0
#ifdef __HARBOUR__
      if (_fkey:=nextkey(INKEY_KEYBOARD + INKEY_LDOWN))#0
         if _fkey=1002
           _fkey:=0
           job:=1
         endif
#else
      sysint(51,3,@job)
      if job#0.or.(_fkey:=nextkey())#0
#endif
#else
      if (_fkey:=nextkey())#0
#endif
         if !(_fkey=K_ESC .or. _fkey=K_PGDN .or. _fkey=K_PGUP .or. _fkey=K_CTRL_L /* .or. _fkey=K_CTRL_W */ )
            _fkey:=0 // read
            stat:=.t. // repaint
         endif
         _fpopkey:=.t.
      elseif !_fpopkey .and. _fi<_flp
         _fkey:=K_PGDN  // refr
      else
         stat:=.t.
      endif

      rmpos:=_fpos
      if stat
         _fpos:=0
         job:=right(ltrim(str(_fi,6,0)),4)
         @ _fk,_fco1 SAY 'º   .' color _sbkgr
         @ _fk,_fco1+max(0,4-len(job)) say job color _sbkgr
         //@ _fk,_fco1+1 say str(_fi,3)+'.' color _sbkgr
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
         if !stat .and. readkey()=GE_MOUSEHIT
           job:=readkey(,)
           job:=ascan(getlist,{|g|g:row=job[3] .and. g:col<=job[2] .and. g:col+len(tran(g:varGet(),g:picture))>=job[2]})
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
        if _fkey=GE_MOUSEHIT .and. !_fnowy
           job:=readkey(,)
           if job[1]=2 .or. job[2]<=_fco1 .or. job[2]>=_fco2 .or. job[3]<_frow+_fskip-1 .or. job[3]>_frow+_fskip*(_fl-_fj+1)
              _fkey:=K_CTRL_L //K_CTRL_W

           elseif job[3]=_frow+_fskip-1 .and. _fj>0
              job:=_fi-_fj
              _fi-=job
              stat:=.t.
              --_fj
              if _fskip*(_fl-_fj+1)+_frow>maxrow()
                 --_fl
                 if _fl=_flp-1
                     @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY  'È'+replicate('Ä',_fco2-_fco1-1)+'¼'
                 endif
              else
                 @ (_fl-_fj)*_fskip+_frow,_fco1,(_fl-_fj+1)*_fskip+_frow,_fco2 BOX 'º ºº¼ÍÈº'
              endif
              scroll(_fskip+_frow,_fco1,_frow+_fskip*(_fl-_fj+1)-1,_fco2,-_fskip)
              if _fskip<2
                @  _fskip+_frow,_fco1 SAY 'º'
                @  _fskip+_frow,_fco2 SAY 'º'
              else
                @  _fskip+_frow,_fco1,2*_fskip+_frow-1,_fco2 BOX 'º ººº ºº'
              endif
              Skip -job
              loop
           elseif job[3]=_frow+_fskip*(_fl-_fj+1) .and. _fl<_flp
              job:=_fl-_fi+1
              skip job
              _fi+=job
              if _frow+_fskip*(_fi-_fj+1)>maxrow()
                stat:=.t.
                ++_fj
                ++_fl
                scroll(_frow+_fskip,_fco1,_frow+_fskip*(_fi-_fj+1)-1,_fco2,_fskip)
                if _fskip<2
                  @  _fskip*(_fi-_fj)+_frow,_fco1 SAY 'º'
                  @  _fskip*(_fi-_fj)+_frow,_fco2 SAY 'º'
                else
                  @  _frow+_fskip*(_fi-_fj),_fco1,_frow+_fskip*(_fi-_fj+1)-1,_fco2 BOX 'º ººº ºº'
                endif
              endif
              if _fi>_fl
                stat:=.t.
                 @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX 'º ºº¼ÄÈº '
                 ++_fl
              endif
              IF _flp<=_fi
                 @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY  'È'+replicate('Í',_fco2-_fco1-1)+'¼'
              ENDIF
              loop
           else
              job:=max(min(_flp,int((job[3]-_frow)/_fskip)+_fj),1)
              skip job-_fi
              _fi:=job
           endif
        endif
#endif
#ifdef __HARBOUR__
#define D_REST 4
#else
#define D_REST 2
#endif
            DO CASE

              CASE _fnowy
                 --_flp
                 if _flp>0 .and. _fi<=_flp
                    skip
                    if _fi<_fl
                       scroll(_fk,_fco1,_frow+_fskip*(_fl-_fj+1),_fco2,_fskip)
                       --_fl
                       RESTSCREEN(1+_fskip*(_fl-_fj+1)+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,D_REST*(_fco2-_fco1+1)*(1+_fskip*(_fl-_fj+1)+_frow)+1))
                       *********
                       _fkey:=_fi
                       do while _fk<=_frow+_fskip*(_fl-_fj)
                          job:=right(ltrim(str(_fkey++,6,0)),4)
                          @ _fk,_fco1 SAY 'º   .' color _sbkgr
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
                      RESTSCREEN(_fskip*(_fl-_fj+1)+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,D_REST*(_fco2-_fco1+1)*(_fskip*(_fl-_fj+1)+_frow)+1))
                      @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY 'È'+replicate('Í',_fco2-_fco1-1)+'¼'
                   endif
                else
                   skip
                   //scroll(_fk,_fco1,_fk+_fskip-1,_fco2,0)
                   if _fskip<2
                     @ _frow+_fskip*(_fi-_fj), _fco1 SAY  'º'+space(_fco2-_fco1-2)+'º'
                   else
                     @  _fk,_fco1,_fk+_fskip-1,_fco2 BOX 'º ººº ºº '
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
                    stat:=.t.
                    --_fj
                    if _fskip*(_fl-_fj+1)+_frow>maxrow()
                         --_fl
                        if _fl=_flp-1
                          @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY  'È'+replicate('Ä',_fco2-_fco1-1)+'¼'
                        endif
                    else
                        @ (_fl-_fj)*_fskip+_frow,_fco1,(_fl-_fj+1)*_fskip+_frow,_fco2 BOX 'º ºº¼ÍÈº'
                    endif
                    scroll(_fskip+_frow,_fco1,_frow+_fskip*(_fl-_fj+1)-1,_fco2,-_fskip)
                    if _fskip<2
                      @  _fskip+_frow,_fco1 SAY "º"
                      @  _fskip+_frow,_fco2 SAY "º"
                    else
                      @  _fskip+_frow,_fco1,2*_fskip+_frow-1,_fco2 BOX 'º ººº ºº'
                    endif
                 ENDIF
                 Skip -1

              CASE _fi=_flp .AND. _fkey=K_ENTER .or. _fi=_flpmax
            IF _flastexit#NIL .and. TAK('CZY KONIEC WPROWADZANIA',_fk+_fskip,_fco2-50,.F.,.F.)
               EVAL(_flastexit,_f)
               break
            ENDIF

              case _fkey=K_PGDN .or. _fkey=K_ENTER // PgDn
            skip
            ++_fi
	  if _frow+_fskip*(_fi-_fj+1)>maxrow()
            stat:=.t.
            ++_fj
            ++_fl
            scroll(_frow+_fskip, _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2, _fskip)
            if _fskip<2
              @ _frow+_fskip*(_fi-_fj), _fco1 SAY  'º'
              @ _frow+_fskip*(_fi-_fj), _fco2 SAY  'º'
            else
              @  _frow+_fskip*(_fi-_fj), _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2 BOX 'º ººº ºº'
            endif
          endif
          if _fi>_fl
            stat:=.t.
            @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX 'º ºº¼ÄÈº ' 
            ++_fl
          endif
          IF _flp<=_fi
            @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY  'È'+replicate('Í',_fco2-_fco1-1)+'¼'
          ENDIF
            ENDCASE
         ENDDO
  enddo
  recover using stat
    if stat#NIL
      RESTSCREEN(0,_fco1,maxrow(),_fco2,_fscr)
      set cursor off
      SET COLOR TO (_SNORM)
      break(stat)
    endif
  end sequence
  RESTSCREEN(0,_fco1,maxrow(),_fco2,_fscr)
  set cursor off
  SET COLOR TO (_SNORM)

RETURN
*************
