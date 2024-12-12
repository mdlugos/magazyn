#ifdef A_UNICODE
   #define D_CDP 'UTF8EX'
#else
   #undef PC852
   #define PC852 'PL852'
   #define D_CDP PC852
#endif

#ifdef A_EXT
request A_EXT
#endif

REQUEST HB_LANG_PL, HB_CODEPAGE_PL852, HB_CODEPAGE_PLWIN, HB_CODEPAGE_UTF8EX, VFPCDX
//#define SIMPLE
#define NONTXERR
//#define TCVT(x) tran(x,)
#include "error.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "dbinfo.ch"
#ifdef A_ADS
  #require "rddads"
  #include "ads.ch"
#else
  #require "rddmisc"
  #include "hbusrrdd.ch"
#endif

ANNOUNCE SYSINIT

static sc:=A_SUMK,a:=A_AUTOR,f:=A_KOMU_N,fa:=A_KOMU_A

/* NOTE: For complete compatibility */
PROCEDURE CLIPPER520()
   RETURN

#ifdef HB_COMPAT_C53

/* NOTE: For complete compatibility */
PROCEDURE CLIPPER530()
   RETURN

#endif

PROCEDURE __HBVMInit()

   MEMVAR GetList

    local i,n,t
    
    HB_CDPSELECT(D_CDP)
#ifdef SIMPLE
#include "simpleio.ch"
//  #ifdef __PLATFORM__WINDOWS
//    hb_Run('chcp 65001 > NUL')
//    hb_SetTermCP( 'UTF8')
//  #endif
#endif

      hb_SetTermCP( hb_cdpTerm() )
      Set(_SET_OSCODEPAGE, hb_cdpOS())

      SET(_SET_DBCODEPAGE, PC852 )

      HB_LANGSELECT('pl')

      hb_gtInfo( HB_GTI_BOXCP, 'UTF8EX')
      hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   
   
    a:=hb_UTF8ToStr(a)
    f:=hb_UTF8ToStr(f)
    fa:=hb_UTF8ToStr(fa)
    
    t:=a+f+fa
    n:=int(hb_blen(t)/4)*4
    for i:=1 to n step 4
      sc+=bin2l(hb_bsubstr(t,i,4))
    next
    
    SET(_SET_DEBUG,.t.) //alt_d on
    altd()
    if sc#0
      //altd()
      quit
    endif

   PUBLIC GetList := {}

   ErrorSys()

   RETURN


PROCEDURE __SetHelpK()

    memvar _sbnorm,_sbkgr,_sramka,_sel,_snorm,_slinia,_sunsel,firma_n,firma_a,defa,pcl

    SetKey( K_F1, {| p, l, v | __XHelp( p, l, v ) } )
      
  if hb_gtInfo( HB_GTI_ISGRAPHIC )
  #ifdef __PLATFORM__UNIX   
     hb_gtInfo( HB_GTI_FONTSEL,'-*-fixed-medium-r-*-*-18-*-*-*-*-*-iso10646-1')
     // 29,27,21,20,18,15,14,13,12,11,10,9,8,7,6
  #else
     hb_gtInfo( HB_GTI_FONTNAME , "Lucida Console" )
     hb_gtInfo( HB_GTI_FONTWIDTH, 10  )
     hb_gtInfo( HB_GTI_FONTSIZE , 20 )
  #endif   
     //hb_gtInfo( HB_GTI_WINTITLE , "Rozruch" )
     hb_gtInfo( HB_GTI_ALTENTER, .T. )  // allow alt-enter for full screen
     SetCursor( 0 )
     hb_gtInfo( HB_GTI_CLOSABLE, .t. )
     hb_gtInfo( HB_GTI_CLOSEMODE, 1) //Generates HB_K_CLOSE keyboard event (does not close application)
  #ifdef __PLATFORM__WINDOWS
  else
   setmode(min(maxrow(),maxcol())+1,max(maxrow(),maxcol())+1)
   setmode(,) //fix problems
  #endif   
  endif
#ifdef A_ADS
   RddRegister( "ADS", 1 )
   SET RDD DEFAULT TO ADS
   #ifdef A_CDX
      // default SET FILETYPE TO VFP
      // default AdsSetCharType( /*MACHINE_VFP_BIN_1250*/ 6, ":en_US" )  
   #else
      SET FILETYPE TO NTX
      SET CHARTYPE TO ANSI
   #endif
      AdsSetServerType( A_ADS )
      SET AXS LOCKING ON
#else
   #ifdef A_CDX
      SET RDD DEFAULT TO A_CDX
   #else
      SET RDD DEFAULT TO DBFNTX
   #endif
#endif
   SET HARDCOMMIT ON

#ifdef A_LAN
set exclusive off
#endif

SET SOFTSEEK ON                        // W calym programie !!!
SET DELETED ON
SET DATE    A_SET_DAT
SET EPOCH TO year(date())-98
SET CENTURY ON


public _sbnorm,_sbkgr,_sramka,_sel,_snorm,_slinia,_sunsel,defa,firma_n:=f,firma_a:=fa


#ifndef A_XPRN
#ifdef A_PCL
public pcl:=.t.
#else
public pcl:=.f.
#endif
#endif
#ifdef SIMPLE
?? firma_n
? firma_a
?
? a
?
#else
#ifdef A_HBGET
    n:={||Key_Ctrl_Ret(GetActive())}
    t:={||GetActive()<>NIL .and. PROCNAME(2)='HBGETLIST:GETAPPLYKEY'}
    i:={||hb_keyput(K_CTRL_W),.t.}
#ifdef __PLATFORM__WINDOWS
    SetKey(K_CTRL_RET,n,t)
#else
    SetKey(K_ALT_RET,n,t)
#endif
    SetKey(K_ALT_K,{|get,b|get:=GetActive(),b:= hb_gtInfo( HB_GTI_CLIPBOARDDATA ),if(""<>b,kibord(getlines(b,.t.)[1]),),.t.},t)
    SetKey(K_ALT_B,{|get|get:=GetActive(),hb_gtInfo( HB_GTI_CLIPBOARDDATA, Alltrim(get:buffer) ),.t.},t)
    SetKey(K_F10,i,t)
    SetKey(K_CTRL_L,i,t)
#endif
//setkey(28,{|x|help(x)})
SET CURSOR OFF
SET SCOREBOARD OFF
SET CONFIRM ON

SETBLINK(.t.)
if iscolor()   //VGA COLOR
  _sramka:="BG+/B"              // Kolor ramki dla szukam()
  _sel:="I"
  _snorm:="W,I,,W,W+/B"
  _sbkgr:="BG+/br,I,,w+/BR,W+/B"
  _sbnorm:="w+/BR,i,,bg+/br,w+/b"
  _slinia:=_sunsel:="W+/B"
  SET COLOR TO GR+/BG,I,,,W+/B  // _snorm,_sel,_sramka,_sborder,_slinia
else // MDA, VGA MONO
  _sramka:=_sbnorm:=_sbkgr:=_snorm:="W,I,,W,W+"
  _sel:="I"
  _slinia:="U+"
  _sunsel:="W+"
  SET COLOR TO W,I,,W,W+
endif
INIT SCREEN
CLEAR screen
@ 1,0 say padc("program dla:     "+firma_n+space(17),maxcol())
@ 2,0 say padc(firma_a,maxcol())
@ 16,0 say padc(a,maxcol())
//@ 20,0 say padc("Program napisano w języku CLIPPER 5.01, numer licencji: CDX 218558",maxcol())
@ 20,0 say padc(wersja()+' '+hb_gtInfo(HB_GTI_VERSION)+', kompilator: '+version(1),maxcol())
#ifdef A_LAN
@ 22,0 say padc(trim("WERSJA SIECIOWA "+os()+" "+netname()),maxcol())
#else
@ 22,0 say padc(os(),maxcol())
#endif
#ifdef A_DEMO
@ 23,0 say padc(hb_UTF8ToStr(IF(DTOS(date())<A_DEMO,"NA OKRES WDROŻENIOWY","DLA CELÓW DEMONSTRACYJNYCH")),maxcol())
#endif
@ 18,0 say padc(hb_UTF8ToStr("PROSZĘ CHWILĘ POCZEKAĆ !"),maxcol()) color "*"+setcolor()
#ifdef A_MYSZ
sysint(51,0)
#else
MHIDE()
#endif
#endif
return
