#include "dm_form.ch"
#include "getexitm.ch"
#include "inkey.ch"

#define D_REST 4
//iif(hb_gtInfo( HB_GTI_COMPATBUFFER ),2,4)

#ifdef A_MYSZ
static proc drag_mysz(_f,job)
local scrlok,delta:={1,0,0}

        MSHOW()
        scrlok:=savescreen(_fscr0,_fco1,_frow+(_fl-_fj+1)*_fskip,_fco2)
        do while delta[1]=1
           if delta[2]#0 .or. delta[3]#0
              dispbegin()
              RESTSCREEN(_fscr0,_fco1,MaxRow(),_fco2,_fscr)
              _fscr0+=delta[3]
              _frow+=delta[3]
              _fco1+=delta[2]
              _fco2+=delta[2]
              setpos(row()+delta[3],col()+delta[2])
              _fscr:=savescreen(_fscr0,_fco1,MaxRow(),_fco2)
              restscreen(_fscr0,_fco1,_frow+(_fl-_fj+1)*_fskip,_fco2,scrlok)
              dispend()
           endif
           delta[1]:=inkey(0,INKEY_MOVE + INKEY_LUP)
           if delta[1]=1003
              delta[1]:=0
           else
              delta[1]:=1
           endif
           delta[2]:=max(0,min(maxcol(),mcol()))-job[2]
           delta[3]:=max(0,min(MaxRow(),mrow()))-job[3]
           _fk:=_frow+_fskip*(_fi-_fj)
           if delta[2]+_fco1<0 .or. delta[2]+_fco2>maxcol() .or. delta[3]+_fscr0<0 .or. delta[3]+_fk+_fskip>maxrow()
              exit
           endif
           job[2]+=delta[2]
           job[3]+=delta[3]
        enddo
        MHIDE()
        if _frow+(_fl-_fj+1)*_fskip>maxrow()
         _fl:=Int((maxrow()-_frow)/_fskip)+_fj-1
         RESTSCREEN(_fskip*(_fl-_fj+1)+_frow,_fco1,MaxRow(),_fco2,hb_BSubStr(_fscr,D_REST*(_fco2-_fco1+1)*(_fskip*(_fl-_fj+1)+_frow-_fscr0)+1))
         @ _fskip*(_fl-_fj+1)+_frow,_fco1 BOX '╙'+replicate('─',_fco2-_fco1-1)+'╜' UNICODE COLOR _sbkgr
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
       _fj:=max(0,max(_fl,_fi)-int((MaxRow()-_frow)/_fskip)+1)
       skip _fj+1-_fi
       _fl:=_fi:=_fj+1
       _fpos:=_fkey:=0
       while _fi<=_flp .and. _frow+_fskip*(_fi-_fj+1)<=MaxRow()
#ifdef __PLATFORM__DOS
         @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX UNICODE IF( _flp<=_fi,'║ ║║╝═╚║ ','║ ║║╝─╚║ ') COLOR _sbkgr
#else
         @ _fskip*(_fi-_fj)+_frow,_fco1,_fskip*(_fi-_fj+1)+_frow,_fco2 BOX UNICODE IF( _flp<=_fi,'║ ║║╝═╚║ ','║ ║║╜─╙║ ') COLOR _sbkgr
#endif         
         _fl:=_fi
         _fk:=_frow+_fskip*(_fi-_fj)
         job:=right(lTrim(sTr(_fi)),4)
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
                      @  _fskip+_frow,_fco1 BOX "║" UNICODE
                      @  _fskip+_frow,_fco2 BOX "║" UNICODE
                    else
                      @  _fskip+_frow,_fco1,2*_fskip+_frow-1,_fco2 BOX UNICODE '║ ║║║ ║║'
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
          if _frow+_fskip*(_fi-_fj+1)>MaxRow()
            ++_fl
            if _fscr0>=_fskip
               _fscr0-=_fskip
               _frow-=_fskip
               _fscr:=savescreen(_fscr0,_fco1,_fscr0+_fskip-1,_fco2)+_fscr
               hb_scroll(_fscr0,_fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2, _fskip)
            else
               stat:=.t.
               ++_fj
               hb_scroll(_frow+_fskip, _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2, _fskip)
            endif
            if _fskip<2
              @ _frow+_fskip*(_fi-_fj), _fco1 BOX '║' UNICODE
              @ _frow+_fskip*(_fi-_fj), _fco2 BOX '║' UNICODE
            else
              @  _frow+_fskip*(_fi-_fj), _fco1, _frow+_fskip*(_fi-_fj+1)-1, _fco2 BOX UNICODE '║ ║║║ ║║'
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
