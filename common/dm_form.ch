memvar _slinia,_sramka,_sbkgr,_snorm,_sel,_sunsel,_sbnorm

#define _srowb     _s[1]
#define _scol1     _s[2]
#define _srowe     _s[3]
#define _scol2     _s[4]
#define _sbeg      _s[5]
#define _slth      _s[6]
#define _snagl     _s[7]
#define _sprompt   _s[8]
#define _sinfo     _s[9]
#define _spocz     _s[10]
#define _skon      _s[11]
#define _sret      _s[12]
#define _spform    _s[13] //seek to p to displ
#define _srec      _s[14]
#define _sprpt     _s[15]
#define _skproc    _s[16]
#define _si        _s[17]
#define _sm        _s[18]
#define _scr       _s[19]
#define _srow1     _s[20]
#define _srow2     _s[21]
#define _srown     _s[22]
#define _scoln     _s[23]
#define _sef       _s[24]
#define _sbf       _s[25]
#define _swar      _s[26]
#define _sfor      _s[27]
#define _sfilt     _s[28]
#define _sfilb     _s[29]
#define _snagkol   _s[30]
#define _sp2s      _s[31] //p to s
#define _ss2p      _s[32] //s to p
#define _slastkey  _s[33] //
#define _sLEN      33


#define _fco1      _f[1]
#define _fco2      _f[2]
#define _frow      _f[3]
#define _fskip     _f[4]
#define _flpmax    _f[5]
#define _fdmpre    _f[6]
#define _fdmget    _f[7]
#define _fdmpost   _f[8]
#define _fmainpre  _f[9]
#define _fmainget  _f[10]
#define _fmainpost _f[11]
#define _flastexit _f[12]
#define _flp       _f[13]
#define _fi        _f[14]
#define _fj        _f[15]
#define _fk        _f[16]
#define _fkey      _f[17]
#define _fl        _f[18]
#define _fpos      _f[19]
#define _fposg     _f[20]
#define _fpopkey   _f[21]
#define _fnowy     _f[22]
#define _fscr      _f[23]
#define _fLEN      23

#command HIGHLIGHT LINE <row> => restscreen(<row>,_scol1,<row>,_scol2-1,hiattr(_sprpt:=savescreen(<row>,_scol1,<row>,_scol2-1)));
;        PROMPT LINE <row>
#command RESTORE LINE <row> => restscreen(<row>,_scol1,<row>,_scol2-1,_sprpt)
#command SAVE LINE <row> => _sprpt:=savescreen(<row>,_scol1,<row>,_scol2-1)
#command ALTERNATE LINE <row> BUFFER <scrbuf> ATTRIB <atr> => restscreen(<row>,_scol1,<row>,_scol2-1,altattr(<scrbuf>,<atr>))
#command PROMPT LINE <row> => if _sbeg>0 ; setpos(<row>,_scol1+_sbeg-1) ; dispout(left(EVAL(_spform,_spocz,_slth),_scoln-_sbeg+1),_SEL) ; endif
#command REFRESH LINE <row> DIRECTION <d> => SETPOS(<row>,_scol1) ; dispout(padr(eval(_sprompt,<d>,_s),_scol2-COL())) ;
;        SAVE LINE <row>
