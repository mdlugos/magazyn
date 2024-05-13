#include "inkey.ch"
#ifdef __HARBOUR__
#include "hbgtinfo.ch"
#endif
memvar _snorm,operator,_sramka
field index,HASLO,HASLO_SPEC
#ifdef A_OBR
field nr_rys,nazwa
#endif

function browse(plik)

LOCAL DIR:={},DIR1,i,txt,scr_buf,m,n,f,b

local nTop:=3, nLeft:=0, nBottom:=maxrow()-2, nRight:=maxcol()

local oB, lMore, lAppend, lKillAppend,;
    nKey, nCursSave, lGotKey,block

local lPack,sequr,kasuj,ord,a

memvar getlist,defa

set cursor off
if empty(plik)
dir:={}
aeval(getlines(SET(_SET_DEFAULT)+HB_OsPathListSeparator()+set(_SET_PATH),HB_OsPathListSeparator()),{|t|;
     aeval(directory(t+"*.dbf"),;
           {|x|x:=PADR(STRTRAN(LOWER(X[1]),".dbf"),8),if(ascan(dir,x)=0,AADD(DIR,x),)};
           );
     })

PLIK=LOWER(LEFT(ALIAS()+"        ",8))
@ 21,50 SAY 'PODAJ NAZWE BAZY' GET PLIK PICTURE "@K!" VALID ACZOJS(DIR)
READ
IF ReadkeY()=K_ESC
   RETURN NIL
ENDIF
endif
kasuj:=select(plik)#sel(plik)
ord:=ordnumber()
sequr:=plik="indeks".or.!kasuj
lPack:=.f.
@ 3,0 CLEAR
do powr_dbed
if sequr
   @ 0,0 SAY "Bezpoòrednie zmiany w tej bazie danych mog• rozkojarzyÜ dane !" COLOR "GR+"
   @ 1,0 SAY "Upewnij si©, czy posiadasz kopi© bezpiecze‰stwa na dyskietce !" COLOR "GR+"
else
   go top
ENDIF
  // frame window
  @ nTop, nLeft, nBottom, nRight box "’Õ∏≥æÕ‘≥" codepage 'PLMAZ'

  // clear status row
  @ nTop + 1, nLeft + 1 say Space(nRight - nLeft - 1)
  @ nTop + 1, nRight - 27 say padl(plik,8)

  // create a TBrowse object for a database
  if ob=NIL
  oB := TBrowseDB()
  endif
  oB:ntop:=nTop + 2
  oB:nleft:=nLeft + 1
  oB:nbottom:=nBottom - 1
  oB:nright:= nRight - 1
#ifdef PLWIN
#ifdef PC852
  @ nTop + 3, nLeft say "∆" codepage 'PLMAZ' 
  @ nTop + 3, nRight say "µ"  codepage 'PLMAZ'
  oB:headSep := HB_UTF8TOSTRBOX(HB_TRANSLATE("—Õ",'PLMAZ','UTF8'))
  oB:ColSep  := "≥"
#else
  oB:ColSep  := "|"
  ob:HeadSep := "|ó"
#endif
#else
  @ nTop + 3, nLeft say "∆" codepage 'PLMAZ'
  @ nTop + 3, nRight say "µ" codepage 'PLMAZ' 
  oB:headSep := HB_UTF8TOSTRBOX(HB_TRANSLATE("—Õ",'PLMAZ','UTF8'))
  oB:ColSep  := "≥"
#endif
  oB:skipBlock := {|x| Skipped(x, lAppend)}


  // add one column for each field
  if ob:colcount=0
     aeval(dbstruct(),{|x|oB:addColumn(mkcolumn(x))})
  endif

  if ( Eof() )
    go top
  end

  // init
  lAppend := lKillAppend := .F.
  while ( !oB:stabilize() ) ; end

  if ( LastRec() == 0 )
    // empty file..force append mode
    nKey := 24
    lGotKey := .t.
  else
    lGotKey := .f.
  end

  lMore := .t.
  while (lMore)
    if ( !lGotKey )
      // stabilization will be interrupted by any keystroke
      while ( !oB:stabilize() )
        if ( (nKey := inkey()) != 0 )
          lGotKey := .t.
          exit
        end
      end
    end

    if ( !lGotKey )
      // the TBrowse object is stable
      if ( oB:hitBottom )
        if ( !lAppend .or. Recno() != LastRec() + 1 )
          if ( lAppend )
            // continue appending..restore color to current row
            oB:refreshCurrent()
            while ( !oB:stabilize() ) ; end

            // ensure bottom of file without refresh
            go bottom
          else
            // begin append mode
            lAppend := .t.
            set cursor on
          end

          // move to next row and stabilize to set rowPos
          oB:down()
          while ( !oB:stabilize() ) ; end

          // color the row
          oB:colorRect({oB:rowPos,1,oB:rowPos,oB:colCount},{2,2})
        end
      end

      // display status and stabilize again for correct cursor pos
      Statline(oB, lAppend)
      while ( !oB:stabilize() ) ; end

      // idle
      nKey := inkey(0)
    else
      // reset for next loop
      lGotKey := .f.
    end

    do case
    case ( nKey == 24 )
      if ( lAppend )
        oB:hitBottom := .t.
      else
        oB:down()
      end

    case ( nKey == 5 )
      if ( lAppend )
        lKillAppend := .t.
      else
        oB:up()
      end

    case ( nKey == 3 )
      if ( lAppend )
        oB:hitBottom := .t.
      else
        oB:pageDown()
      end

    case ( nKey == 18 )
      if ( lAppend )
        lKillAppend := .t.
      else
        oB:pageUp()
      end

    case ( nKey == 31 )
      if ( lAppend )
        lKillAppend := .t.
      else
        oB:goTop()
      end

    case ( nKey == 30 )
      if ( lAppend )
        lKillAppend := .t.
      else
        oB:goBottom()
      end

    case ( nKey == 4 )
      oB:right()

    case ( nKey == 19 )
      oB:left()

    case ( nKey == 1 )
      oB:home()

    case ( nKey == 6 )
      oB:end()

    case ( nKey == 26 )
      oB:panLeft()

    case ( nKey == 2 )
      oB:panRight()

    case ( nKey == 29 )
      oB:panHome()

    case ( nKey == 23 )
      oB:panEnd()

    case ( nKey == 27 )
      // exit browse
      lMore := .f.

    case (  nKey == K_INS ) //ins
      if empty(ordbagname(1))
         insrec()
         ob:refreshall()
      elseif !sequr .and. !empty(ordsetfocus())
         nKey:=recno()
         txt:=array(fcount())
         for n:=1 to len(txt)
             txt[n]:=fieldget(n)
         next n
         append blank
         aeval(txt,{|x,i|fieldput(i,x)})
         txt:=NIL
         unlock
         goto nKey
         ob:refreshall()
      endif

    case (  nKey == K_F1 )
      help("B_"+plik)

    CASE nkey == K_F2
         if !set(_SET_DELETED,!set(_SET_DELETED)) .and. deleted()
        skip
        if eof()
          skip-1
        endif
      endif
      freshorder(ob)
      lPack=.t.

    CASE nkey == K_F3 .and. !empty(ordbagname(1))

      i = 1
      txt=IndexkeY(i)
      scr_buf = SAVESCREEN(8, 1, 19, 78)
      scroll(8, 1, 19, 78, 0)
      @ 9,3,18,76 BOX "’Õ∏≥æÕ‘≥" codepage 'PLMAZ' COLOR "W+"
      @ 11,5 say "Podaj  numer  klucza  indeksowego  (Esc - bez indeksowania)"
      do while .not. empty(txt)
          @ 11+i,5 prompt str(i,1)+" - " + LEFT(txt, 66)
       i = i+1
       txt=IndexkeY(i)
     enddo
     if i>1
       i := ordnumber()
       menu to i
       if i != 0
         SET ORDER TO i
         txt=IndexkeY(0)
         @ nTop + 1, nLeft + 2 say padr("KlejnoòÜ wg: "+txt,nright-nleft-43)
         @ 16,5 say "Podaj poszukiwan• sekwencj© znak¢w:"

         txt = &txt
         @ 16,41 get txt picture '@KS34'
         read

         if TYPE('txt') = "C"
           txt = trim(txt)
         else
           (type("{||NIL}"))
         endif
         if ReadkeY()#K_ESC
           seek txt
         endif

       else
     SET ORDER TO 0
       @ nTop + 1, nLeft + 2 say padr("KlejnoòÜ naturalna.",nright-nleft-43)
       endif
    skip 0
       RESTSCREEN(8, 1, 19, 78, scr_buf)
       freshorder(ob)
  else
     RESTSCREEN(8, 1, 19, 78, scr_buf)
     endif

    case (nkey == K_F5)
      ob:delcolumn(ob:colpos)

    case (nkey == K_F6)
      txt:=dbstruct()
      nkey:=len(txt)
      for i=1 to ob:colcount
          n:=ascan(txt,{|x|x[1]==ob:getcolumn(i):heading})
          if n#0
             adel(txt,n)
             --nkey
          endif
      next
      if nkey>0
         asize(txt,nkey)
         n:=array(nkey)
         aeval(txt,{|x,i|n[i]:=x[1]})
         if aczojs(n,"",@i)
            ob:inscolumn(ob:colpos,mkcolumn(txt[i]))
         endif
      endif

    case ( nKey == K_F9 )

      scr_buf = SAVESCREEN(8, 1, 19, 78)

      scroll(8, 1, 19, 78, 0)
      set color to W+
      @ 9,3,18,76 BOX "’Õ∏≥æÕ‘≥" codepage 'PLMAZ'
      set color to W
      @ 11,5 say "Podaj  wyraæenie filtra            (Esc - rezygnacja)"
      @ 13,5 say 'Dost©pne relacje: =,#,$,<,>,<=,>= '
      @ 14,5 say 'Dost©pne operacje logicze: .i., .lub., .nie.'
      txt=dbfilter()

    do while .t.
      NKEY:=NIL
      txt=STRTRAN(txt,".AND.",".i.")
      txt=STRTRAN(txt,".OR.",".lub.")
      txt=STRTRAN(txt,"!",".nie.")+SPACE(160)
      @ 16,5 get txt picture "@KS70"
      READ
      txt=STRTRAN(ALLTRIM(txt),".i.",".AND.")
      txt=STRTRAN(txt,".lub.",".OR.")
      txt=STRTRAN(txt,".nie.","!")
      if ""#txt .and. ReadkeY()#K_ESC .and. !dbfilter()==txt
         begin sequence
         a:=&('{||'+txt+'}')
         nkey:=eval(a)
         IF VALTYPE(NKEY)#'L'
            BREAK
         ENDIF
         recover
         alarm("POPRAW",,2,3)
         (type("{||NIL}"))
         loop
         end sequence
         dbSETFILTER(a,txt)
      ELSEif ""=txt .AND. !dbfilter()==txt
         NKEY:=.T.
      ENDIF
      exit
    enddo

     RESTSCREEN(8, 1, 19, 78, scr_buf)
  IF NKEY#NIL
    IF ""=TXT
      SET FILTER TO
      @ nTop + 1, nLeft + 2 say padr("Bez filtra.",nright-nleft-43)
    ELSE
      @ nTop + 1, nLeft + 2 say padr("Filtr: "+txt,nright-nleft-43)
    ENDIF
    if !NKEY
        skip
    endif
    oB:refreshAll()
    OB:FORCESTABLE()
  endif

//   CASE SEQUR

    case ( nKey == 7 )
      // delete key..toggle deleted() flag
        if ( Recno() != LastRec() + 1 )
          if ( Deleted() )
            lock
            recall
            unlock
          else
            lock
            delete
            unlock
            if set(_SET_DELETED)
              skip
              FreshOrder(oB)
            endif
          end
        end
        lPack :=.T.

    case ( nKey == 13 )
      // edit
      if ( lAppend .or. Recno() != LastRec() + 1 )
        lock
        nKey := DoGet(oB, lAppend)
        unlock
        // use returned value as next key if not zero
        lGotKey := ( nKey != 0 )
      else
        // begin append mode
        nKey := 24
        lGotKey := .t.
      end

    case ( nKey >= 32 .and. nKey <= 255 )
        // begin edit and supply the first character
        keyboard Chr(13) + Chr(nKey)

    case (i:=setkey(nkey))#NIL
        eval(i,procname(),procline())
    end

    if ( lKillAppend )
      // turn off append mode
      lKillAppend := .f.
      lAppend := .f.
      set cursor off
      // refresh respecting any change in index order
      FreshOrder(oB)
    end
  end

set deleted on

@ 0,0 CLEAR to 2,79
if !shared() .and. lPack .AND. tak('CZY SKASOWAè UKRYTE WIERSZE',0,,.f.,.F.)
    @ 0,0 SAY "TRWA KASOWANIE UKRYTYCH WIERSZY W BAZIE " COLOR "BG+"
    dispout(alias(),"BG+*")
    i:=lastrec()
    PACK
    dispout(", SKASOWANO "+trim(str(i-lastrec()))+".","BG+*")
    tone(262,4)
    inkey(2)
endif
if kasuj
   use
   readinit()
else
   set order to ord
   unlock
endif

return (.t.)
************
proc mkindex(n,k)
FIELD for,unique,descend,klucz,baza,nazwa
local b:=lower(alias()),c
  n:=upper(n)
  if empty(ordbagname(n))
     sel("INDEKS")
     Locate FOR {||c:=trim(baza), lower(expand(c))==b}
     n:=pad(n,len(nazwa))
     c:=pad(c,len(baza))
     Locate FOR nazwa==n WHILE baza==c
     IF ! FOUND()
       skip -1
       insrec()
       skip
       LOCK
       nazwa:=n
       for:=''
       unique:=descend:=.f.
       klucz:=k
       UNLOCK
     endif
     select (b)
     use
     sel(b,n)
  else
     ordsetfocus(n)
  endif
return
************
stat proc insrec()
local r,ar,i,l
r:=recno()
l:=fcount()
ar:=array(l)
append blank
skip -1
do while recno()>=r .and. !bof()
   for i:=1 to l
     ar[i]:=fieldget(i)
   next
   skip
   lock
   for i:=1 to l
     fieldput(i,ar[i])
   next
   skip -2
enddo
unlock
goto r
return
************
/***
*  DoGet()
*
*  Edit the current field
*/

static func DoGet(oB, lAppend)

local lScoreSave, lExitSave
local oCol, oGet, nKey, cExpr, xEval
local lFresh, nCursSave, mGetVar

  // make sure the display is correct
  oB:hitTop := .f.
  Statline(oB, lAppend)
  while ( !oB:stabilize() ) ; end
  // save state
  lScoreSave := Set(_SET_SCOREBOARD, .f.)
  lExitSave := Set(_SET_EXIT, .t.)

  // turn the cursor on
  nCursSave := SetCursor( if(Set(_SET_INSERT), 2, 1) )

  // get the controlling index key
  cExpr := IndexkeY(0)
  if ( !Empty(cExpr) )
    // expand key expression for later comparison
    xEval := &cExpr
  end

  // get column object from browse
  oCol := oB:getColumn(oB:colPos)

  // use temp for safety
    mGetVar := Eval(oCol:block)

  // create a corresponding GET with ambiguous set/get block
  oGet := GetNew(Row(), Col(),{|x| if(PCount() == 0, mGetVar, mGetVar := x)},"mGetVar",oCol:picture,oB:colorSpec)

  if oCol:cargo=.t. .or. valtype(mGetVar)$"MC" .and. len(mGetVar) >= maxcol() - 1
     oGet:picture:="@S"+ltrim(str(maxcol()-1,3))
     oGet:cargo:=oCol:cargo // expandable field
  endif
  // refresh flag
  lFresh := .f.

  do powr_get

  // read it
  if ( ReadModal( {oGet} ) )
    // new data has been entered
    if ( lAppend .and. Recno() == LastRec() + 1 )
      // new record confirmed
      APPEND BLANK
    end
  if ocol:cargo=.t.
    mgetvar:=trim(mgetvar)
  endif
    // replace with new data
    Eval(oCol:block, mGetVar)
    // test for change in index order
    if ( !Empty(cExpr) .and. !lAppend )
      if ( xEval != &cExpr )
        // change in index key eval
        lFresh := .t.
      end
    end
    unlock
    if ( lFresh )
      // record in new indexed order
      FreshOrder(oB)

      // no other action
      nKey := 0
    else
      // refresh the current row only
      oB:refreshCurrent()
    end
  end

  IF !lfresh
    // certain keys move cursor after edit if no refresh
    nKey := ExitKey(lAppend)
  endif

  if ( lAppend )
    // maintain special row color
    oB:colorRect({oB:rowPos,1,oB:rowPos,oB:colCount}, {2,2})
    set cursor on
  end

  // restore state
  SetCursor(nCursSave)
  Set(_SET_SCOREBOARD, lScoreSave)
  Set(_SET_EXIT, lExitSave)
    do powr_dbed

return (nKey)


/***
*  ExitKey()
*
*  Determine the follow-up action after editing a field
*/

static func ExitKey(lAppend)

local nKey

  nKey := ReadKey()
  if ( nKey == K_PGDN )
    // move down if not append mode
    if ( lAppend )
      nKey := 0
    else
      nKey := K_DOWN
    end

  elseif ( nKey == K_PGUP )
    // move up if not append mode
    if ( lAppend )
      nKey := 0
    else
      nKey := K_UP
    end

  elseif ( nKey == K_ENTER .or. (nKey >= 32 .and. nKey <= 255) )
    // return key or type out..move right
    nKey := K_RIGHT

  elseif ( nkey == K_LEFT .or. nkey == K_RIGHT )


  elseif ( nKey != K_UP .and. nKey != K_DOWN )
    // no other action
    nKey := 0
  end

return (nKey)


/***
*  FreshOrder()
*
*  Refresh respecting any change in index order
*/

static func FreshOrder(oB)

local nRec

  nRec := Recno()
  oB:refreshAll()

  // stabilize to see if TBrowse moves the record pointer
  oB:forcestable()

  if ( nRec != LastRec() + 1 )
    // record pointer may move if bof is on screen
    while ( Recno() != nRec )
      // falls through unless record is closer to bof than before
      oB:up()
      oB:forcestable()
    end
  end

return (NIL)


/***
*  Statline()
*
*  display status at coordinates relative to TBrowse object
*/

static func Statline(oB, lAppend)

local nTop, nRight

  nTop := oB:nTop - 1
  nRight := oB:nRight

  if ( LastRec() == 0 .and. !lAppend )
    // file is empty
    @ nTop, nRight - 20 say "<brak>               "
  elseif ( Recno() == LastRec() + 1 )
    // no record number if eof
    @ nTop, nRight - 40 say "          "
    @ nTop, nRight - 20 say "               <nowy>"
  else
    // normal record..display Recno()/LastRec() and Deleted()
    @ nTop, nRight - 40 say if(set(_SET_DELETED),"          ",If(Deleted(), " <Ukryty> ", "<Widoczny>"))
    @ nTop, nRight - 20 say padr(Ltrim(Str(Recno())) + "/" +;
                  Ltrim(Str(LastRec())), 15) +;
                If(oB:hitTop, "<g¢ra>", "      ")
  end

return (NIL)


/***
*  Skipped(n)
*
*  Skip thru database and return the
*  actual number of records skipped
*/

static func Skipped(nRequest, lAppend)

local nCount

  nCount := 0
  if ( LastRec() != 0 )
    if ( nRequest == 0 )
      skip 0

    elseif ( nRequest > 0 .and. Recno() != LastRec() + 1 )
      // forward
      while ( nCount < nRequest )
        skip 1
        if ( Eof() )
          if ( lAppend )
            // eof record allowed if append mode
            nCount++
          else
            // back to last actual record
            skip -1
          end

          exit
        end

        nCount++
      end

    elseif ( nRequest < 0 )
      // backward
      while ( nCount > nRequest )
        skip -1
        if ( Bof() )
          exit
        end
        nCount--
      end
    end
  end

return (nCount)

**************************************
PROCEDURE POWR_DBED

  @ maxrow()-1,0  CLEAR
  @ maxrow()-1,0  SAY '   -wyjòcie     -edycja      -filtr    -pokaæ ukryte       -ukryj rekord'
  @ maxrow(),27 SAY '- nawigacja       -szybkie szukanie'
  @ maxrow()-1,0  SAY 'Esc'  COLOR "I"
  @ maxrow()-1,13 SAY "Ent" COLOR "I"
  @ maxrow()-1,26 say "F9"  COLOR "I"
  @ maxrow()-1,37 SAY 'F2'  COLOR "I"
  @ maxrow()-1,56 SAY 'Del'  COLOR "I"
  @ maxrow(),0  SAY '^'  COLOR "I"
  @ maxrow(),2  SAY 'v'  COLOR "I"
  @ maxrow(),4  SAY '>'  COLOR "I"
  @ maxrow(),6  SAY '<'  COLOR "I"
  @ maxrow(),8  SAY 'Home'  COLOR "I"
  @ maxrow(),13 SAY 'End'  COLOR "I"
  @ maxrow(),17 SAY 'PgUp'  COLOR "I"
  @ maxrow(),22 SAY 'PgDn'  COLOR "I"
  @ maxrow(),42 SAY 'F3'  COLOR "I"

RETURN



**************************************
PROCEDURE POWR_GET

  @ maxrow()-1,0  CLEAR
  @ maxrow()-1,0  SAY     '             -wyjscie z pola z zapisem zmian      -wyjscie z przywr.starej wart.'
  @ maxrow(),0  SAY     '   -tryb wstawiania <insert> /zamiany znak¢w'
  @ maxrow(),65 SAY     '- edycja w polu'
  @ maxrow()-1,0  SAY 'Ent' COLOR "I"
  @ maxrow()-1,4  SAY 'PgUp' COLOR "I"
  @ maxrow()-1,9  SAY 'PgDn' COLOR "I"
  @ maxrow()-1,47 SAY 'Esc' COLOR "I"
  @ maxrow(),0  SAY 'Ins' COLOR "I"
  @ maxrow(),47 SAY '^' COLOR "I"
  @ maxrow(),50 SAY 'V' COLOR "I"
  @ maxrow(),53 SAY '>' COLOR "I"
  @ maxrow(),55 SAY '<' COLOR "I"
  @ maxrow(),57 SAY 'Home' COLOR "I"
  @ maxrow(),62 SAY 'End' COLOR "I"

 RETURN
************
func mkcolumn(m)
local fb:=fieldblock(m[1])
local b:=TBColumnNew(m[1],fb)

#ifndef A_SX
    if m[1]=="HASLO"
         B:block:={|X|IF(X=NIL,X:=L2BIN(FIELD->HASLO),FIELD->HASLO:=BIN2L(PADR(UpP(X),4))),X}
    elseif m[1]=="HASLO_SPEC"
         B:block:={|X|IF(X=NIL,X:=L2BIN(FIELD->HASLO_SPEC),FIELD->HASLO_SPEC:=if(x="    ",0,BIN2L(PADR(lower(UpP(X)),4)))),X}
#else
    if .f.
#endif
#ifdef __HARBOUR__
    elseif m[2]=="B"
         m[3]:=12
         b:picture:='@Z '+replicate('#',m[3]-5)+'.####'
#endif
    elseif m[1]="D_" .and. m[2]="C" .and. m[3]=8
         b:block:={|x|IF(x=NIL,,x:=D2BIN(x)),ROUND(BIN2D(eval(fb,x)),4)}
    elseif m[2]=="M"
         b:width:=maxcol()-1
         b:cargo:=.t.
         b:block:={|x,y|y:=eval(fb,x),if(x=NIL.and.y<>NIL.and.len(y)<maxcol(),padr(y,maxcol()-1),y)}
    elseif m[2]=="C" .and. m[3]>maxcol()-3
         b:width:=maxcol()-1
    endif

return b
***************************
stat func sk(x,a,h)
#define D_BLEN 512
local p,r,k,l,n,o,eol
//#ifdef __PLATFORM__UNIX
// #define nl chr(10)
//#else
// #define nl chr(13)+chr(10)
//#endif
static i,b,lth
  if x=NIL
     i:=b:=x
     return x
  elseif b=NIL
     b:=space(D_BLEN)
     lth:=0
  endif
  if valtype(x)="L"
     i:=if(x,1,0)
     x:=lth:=0
  endif
  o:=i
  n:=i+x
  r:=0
  do while n>lth
     i:=lth
     p:=if(i>0,a[i,1]+a[i,2],0)
     fseek(h,p)
     k:=xfr(h,@b)
     r:=0
     if k>0
        do while n>lth
           l:=at(chr(10),subs(b,r+1,k-r))
           if l=0 .and. r#0
              exit
           endif
           if subs(b,r+l,1)=chr(13)
              --l
           endif
           if len(a)>lth
              ++lth
              ++i
           else
              adel(a,1)
              --o
              --n
           endif
           a[lth]:={p+r,k-r}
           if l=0
              exit
           endif
#ifndef __PLATFORM__UNIX
//           ++l
#endif
           a[lth,2]:=l
           r+=l
        enddo
        r:=a[lth,1]-p
     else
        n:=lth
        exit
     endif
  enddo
  do while n<1
     i:=1
     if lth<i
        k:=fseek(h,0,2)
        eol:=0
     else
        k:=fseek(h,a[i,1])
     endif
     if k>0
       p:=fseek(h,-min(k,D_BLEN),1)
       l:=r:=k-p
       xfr(h,@b,r)
//#ifdef __PLATFORM__UNIX
       if subs(b,r,1)=chr(10)
         eol:=1
         if subs(b,r-eol,1)=chr(13)
            ++eol
         endif
       else
         eol:=0
       endif
//       eol:=if(subs(b,r,1)=chr(10),1,0)
//#else
//       eol:=if(subs(b,r-1,2)=nl,2,0)
//#endif
       do while n<1 .and. l#0
          l:=rat(chr(10),left(b,r-eol))
          if l#0
//#ifndef __PLATFORM__UNIX
//             ++l
//#endif
             eol:=1
             if subs(b,l-eol)=chr(13)
                ++eol
             endif
          elseif r#k-p
             exit
          endif
          if lth<len(a)
             ++lth
          endif
          ains(a,i)
          ++n
          ++o
          a[i]:={p+l,r-l}
          r:=l
       enddo
     else
       n:=1
       exit
     endif
  enddo
  i:=n
  x:=n-o
  if a[i,1]-r#p
    fseek(h,a[i,1])
    xfr(h,@b,a[i,2])
    r:=0
  endif
  eol:=0
  if i<lth.or.subs(b,r+a[i,2],1)=chr(10)
     eol:=1
     if subs(b,r+a[i,2]-eol,1)=chr(13)
        ++eol
     endif
  endif
  o:=subs(b,r,a[i,2]-eol)
return strtran(o,chr(9)," ")
#undef D_BLEN
*********************
proc fview(f,cdp)
local a,h,b,c,j,txt,key,scrlflag:=0,frow:=1,oldp
f:=findfile(f)
h:=fopen(f,64)
if h<0
   return
endif

  a:=array(3*maxrow())
  j:=0
  txt:=sk(.t.,a,h)
#ifdef __HARBOUR__
if !HB_CDPISUTF8(cdp) .and. 'utf-8'$lower(txt)
  cdp:='UTF8'
endif
if !empty(cdp)
  //txt:=HB_TRANSLATE(txt,cdp,)
  oldp:=hb_cdpSelect(cdp)
  if HB_CDPISUTF8(cdp)
     b:=TUTF8Browse():New(0,0,maxrow(),maxcol())
     c:=tbcolumnnew("",{||HB_UTF8SUBSTR(txt,j+1)})
  endif
endif
#endif
if empty(b)
  b:=tbrowseNew(0,0,maxrow(),maxcol())
endif
if empty(c)
  c:=tbcolumnNew("",{||subs(txt,j+1)})
endif
c:width:=maxcol()+1
b:addcolumn(c)
b:skipblock:={|x|txt:=sk(@x,a,h),frow+=x,x}
b:gotopblock:={||txt:=sk(.t.,a,h),frow:=1}
b:gobottomblock:={||txt:=sk(.f.,a,h),frow:=0}
c:=array(31)
c[K_DOWN]     :={|b|if(b:rowpos=b:rowcount,scrlflag:=1,),b:down()}
c[K_UP]       :={|b|if(b:rowpos=1,scrlflag:=-1,),b:up()}
c[K_LEFT]     :={|b|j:=max(0,j-1),b:refreshall()}
c[K_RIGHT]    :={|b|++j,b:refreshall()}
c[K_TAB]      :={|b|j+=8,b:refreshall()}
c[K_PGUP]     :={|b|b:pageup()}
c[K_PGDN]     :={|b|b:pagedown()}
c[K_CTRL_PGUP]:={|b|b:gotop()}
c[K_CTRL_PGDN]:={|b|b:gobottom()}
c[K_HOME]     :={|b|j:=0,b:refreshall()}

#ifdef __HARBOUR__
if nextkey()=0 //.and. fseek(h,0,2) < 65536
  b:forcestable()
  @ maxrow(),maxcol()-31 SAY HB_TRANSLATE('ALT+B - kopiuj CAùOóè do schowka',oldp,) COLOR _sramka
endif
#endif
do while .t.
   b:stabilize()
   if b:stable
      key:=ltrim(str(frow))
      @ 0,1+maxcol()-len(key) SAY key COLOR _sramka
      key:=inkey(0)
   else
      key:=inkey()
   endif
   if key=0
      loop
   elseif key=K_ESC
      exit
#ifdef __HARBOUR__
   elseif key=K_ALT_B //.and. fseek(h,0,2) < 65536
      hb_gtInfo( HB_GTI_CLIPBOARDDATA , memoread(f) )
#endif
   elseif key>0 .and. key<32 .and. c[key]#NIL
      eval(c[key],b)
      if scrlflag=1
        scrltxt()
        dispbegin()
        b:stabilize()
        if !b:hitbottom
           scrltxt()
           while Nextkey()=K_DOWN;inkey();end
           scrllf()
        else
           scrlua()
        endif
        dispend()
      elseif scrlflag=-1
        dispbegin()
        b:stabilize()
        if !b:hittop
           scrlub()
           dispend()
           while nextkey()=K_UP;inkey();end
           scrlua()
        else
           dispend()
        endif
      endif
      scrlflag:=0
   endif

enddo
sk() //kasuj buf
fclose(h)
if !empty(oldp)
  hb_cdpSelect(oldp)
endif
return
***********
