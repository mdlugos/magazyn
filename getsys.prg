#include "set.ch"
#include "inkey.ch"
#include "getexit.ch"
#ifdef __HARBOUR__
#include "hbgtinfo.ch"
#endif

ANNOUNCE GETSYS

#define K_UNDO          K_CTRL_U
#define CTRL_END_SPECIAL


// state variables for active READ
static Updated
#ifdef A_MYSZ
static mysz
#endif
static oldUpdated := .f.
static oldexit := GE_NOEXIT
static BumpTop
static BumpBot
static LastExit
static LastPos
static ActiveGet
static ReadProcName
static RPN:=""
static EditBlock
static EBN
static aGetList:={}


// format of array used to preserve state variables
#define GSV_BUMPTOP      1
#define GSV_BUMPBOT      2
#define GSV_LASTEXIT     3
#define GSV_LASTPOS      4
#define GSV_ACTIVEGET    5
#define GSV_READVAR      6
#define GSV_READPROCNAME 7
#define GSV_UPDATED      8
#define GSV_CURSOR       9
#define GSV_EDITBLOCK   10
#define GSV_GETLIST     11
#define GSV_COUNT       11



/***
*  ReadModal()
*  Standard modal READ on an array of GETs.
*/

func ReadModal( GetList, pos )

local get,x
local savedGetSysVars


  if ( Empty(getList) )
    oldupdated:=.f.
    oldExit:=lastkey()
    return(.f.)
  end


  // preserve state vars
  savedGetSysVars := ClearGetSysVars()
  GetListActive(GetList)
  //EditBlock:=if(valtype(getlist[1]:cargo)="B",getlist[1]:cargo,)
  if pos=NIL 
    pos:=1
  else
    POS:=MAX(MIN(POS,LEN(GETLIST)),1)
  endif
  GetList[pos]:exitState:=lastExit

begin sequence

  while Settle( GetList, @pos )

    // get next GET from list and post it as the active GET
    get := GetList[pos]
    PostActiveGet( get , pos )
    // read the GET
    if ( ValType( get:reader ) == "B" )
      Eval( get:reader, get, getlist, pos )     // use custom reader block
    else
      GetReader( get )        // use standard reader
    end

  end

recover using x

  // restore state vars
  RestoreGetSysVars(savedGetSysVars)
  break(x)

end sequence

  RestoreGetSysVars(savedGetSysVars)

return (oldupdated)
*************************
stat proc fixbuff(g,x)
#ifdef __HARBOUR__
local a:=g:pos
  g:buffer:=x
  g:varput(g:untransform())
  g:reset()
  g:clear:=.f.
  g:pos:=a
#else
  g:buffer:=x
#endif
g:display()
g:changed:=.t.
return
*************************
/***
*  GetReader()
*  Standard modal read of a single GET.
*/
func GetReader( get )

local NumFlag:=.f.,b,x,y,key,mfl,maxlth:=0

  // read the GET if the WHEN condition is satisfied
  if ( GetPreValidate(get) )

    // activate the GET for reading
    if get:exitstate=GE_NOEDIT
       setcursor(0)
       get:colordisp(getlines(get:colorspec,',')[2])
       get:exitstate:=GE_NOEXIT
    else
       get:SetFocus()
    endif
#ifdef A_MYSZ
    mfl:=.t.
#endif
    while ( get:exitState == GE_NOEXIT )
      // check for initial typeout (no editable positions)
      if get:HasFocus .and. get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo
         if get:buffer==""
            fixbuff(get,get:buffer+"  ")
         endif
         maxlth:=max(maxlth,len(get:buffer))
      end
      // apply keystrokes until exit
      do while ( get:exitState == GE_NOEXIT )
#ifdef A_MYSZ
      b:=x:=y:=0
      MSHOW()
#ifdef __HARBOUR__
      do while (key:=INKey(0, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN ))>1000
         x:=mcol()
         y:=mrow()
         if key=1002
            b:=1
         else
            b:=2
         endif
         key:=0
         if .t.
#else
      do while (key:=inkey(.1))=0
         sysint(51,3,@b,@x,@y)
         if b#0
            x:=int(x/8+.1)
            y:=int(y/8+.1)
#endif
            if get:row=y .and. get:col<=x .and. get:col+len(tran(get:original,get:picture))>x
#ifdef __HARBOUR__
               if !get:HasFocus
#else
               if !get:HasFocus .or. mfl
#endif
                  loop
               endif
               MHIDE()
               while x>get:col+get:pos-1
                  get:right()
               enddo
               while x<get:col+get:pos-1
                  get:left()
               enddo
               MSHOW()
               loop
            endif
            get:exitState := GE_MOUSE
            mysz:={b,x,y}
            exit
         endif
         mfl=.f.
      enddo
      MHIDE()
      if key = 0
        loop
      endif
#else
        key:=INkey(0)
#endif
      if get:HasFocus
        GetApplyKey( get, key , @numflag)
        maxlth:=max(maxlth,len(get:buffer))
      else
        GetExitKey(get, key )
      endif
      enddo
      // disallow exit if the VALID condition is not satisfied
      if !GetPostValidate(get)
        get:exitState := GE_NOEXIT
      end

    end

    // de-activate the GET
    if get:HasFocus
      if get:type$"CM" .and. valtype(get:cargo)="L" .and. get:cargo
         @ get:row,get:col say space(min(maxlth,val(subs(get:picture,1+at("S",get:picture))))) color getlines(get:colorspec,',')[1]
      endif
      get:KillFocus()
    else
      get:colordisp(getlines(get:colorspec,',')[1])
      setcursor(if(set(_SET_INSERT),2,1))
    endif

  end

return ! (get:exitState = GE_ESCAPE .or. get:exitState = GE_WHEN)

/***
*  GetApplyKey()
*  Apply a single Inkey() keystroke to a GET.
*
*  NOTE: GET must have focus.
*/
proc GetApplyKey(get, key, NumFlag)

local cKey,a,b,c,i,j,k
local bKeyBlock


  // check for SET KEY first
  if getExitKey(get, key)
    return                  // NOTE
  end

  if NumFlag=.t.
     NumFlag:=NIL
  endif

  do case

  case (key == K_INS)
    if get:type="N"
        NumFlag:=(NumFlag=.f.)
    else
        setcursor(if(Set( _SET_INSERT, !Set(_SET_INSERT) ),1,2))
        return
    endif
  case (key == K_UNDO)
    get:Undo()
    get:changed:=.f.

  case (key == K_HOME)
    get:Home()

  case (key == K_END)
       get:end()
       if get:type="N".and. get:pos>get:decpos
          get:todecpos()
          //get:left()
       elseif get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. get:pos=len(get:buffer) .and. right(get:buffer,1)#" " //expandable field
          fixbuff(get,get:buffer+"  ") //get:buffer+=" "
          get:right()
          //get:changed:=.t.
          //get:display()
       endif

  case (key == K_RIGHT)
       if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. get:pos=len(get:buffer) .and. right(get:buffer,1)#" " //expandable field
          fixbuff(get,get:buffer+"  ")
          //get:buffer+=" "
          get:right()
          //get:changed:=.t.
          //get:display()
       endif
       get:Right()

  case (key == K_LEFT)
       get:Left()

  case (key == K_CTRL_RIGHT)
    get:WordRight()
       if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. get:pos=len(get:buffer) .and. right(get:buffer,1)#" " //expandable field
          fixbuff(get,get:buffer+"  ")
          //get:buffer+=" "
          get:right()
          //get:changed:=.t.
          //get:display()
       endif

  case (key == K_CTRL_LEFT)
    get:WordLeft()

#ifdef A_JMO
  case (key == K_BS .or. key == K_DEL) .and. get:type="N" .and. "r."$get:buffer
       fixbuff(get,if(get:pos>get:decpos,stuff(stuff(get:buffer,get:decpos+1,0,"0"),get:pos+1,1,"")," "+stuff(get:buffer,get:pos,1,"")))
       //get:changed:=.t.
       //get:display()
       NumFlag:=.t.
#endif

  case (key == K_BS .or. key == K_DEL) .and. get:type="N" .and. get:pos<get:decpos
      if get:picture=NIL .or. get:picture="@" .and. !" "$get:picture
       fixbuff(get," "+stuff(get:buffer,get:pos,1,""))
      else
       get:ToDecPos()
       if get:pos>get:decpos
          get:left()
       endif
       a:=get:untransform()
       b:=if(get:minus.or.a<0,-1,1)
       a:=int(a*.1)+a%1
       fixbuff(get,tran(a,get:picture))
      endif
      get:changed:=.t.
      get:display()
      NumFlag:=.t.

  case (key == K_BS)
    get:BackSpace()
      get:changed:=.t.

    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo;
     .and. empty(subs(get:buffer,get:pos))
     fixbuff(get,left(get:buffer,get:pos))
    endif

  case (key == K_DEL)
    get:Delete()
      get:changed:=.t.

    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo;
     .and. empty(subs(get:buffer,get:pos))
     fixbuff(get,left(get:buffer,get:pos))
    endif

  case (key == K_CTRL_T)
    get:DelWordRight()
      get:changed:=.t.
    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo;
     .and. empty(subs(get:buffer,get:pos))
     fixbuff(get,left(get:buffer,get:pos))
    endif

  case (key == K_CTRL_Y)
    get:DelEnd()
      get:changed:=.t.
    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo;
     .and. empty(subs(get:buffer,get:pos))
     fixbuff(get,left(get:buffer,get:pos))
    endif

  case (key == K_CTRL_BS)
    get:DelWordLeft()
      get:changed:=.t.
    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo;
     .and. empty(subs(get:buffer,get:pos))
     fixbuff(get,left(get:buffer,get:pos))
    endif

  case (key == K_CTRL_RET .or. key == K_ALT_RETURN)

    if get:type == "N" .or. get:type == "D" .and. type(get:name)="D"

      getval(get)

    elseif  get:type == "C" .and. ( get:picture=NIL .or. "S" $ get:picture ) //get:picture#NIL .and. "S" $ get:picture

      getchr(get,valtype(get:cargo)="L" .and. get:cargo)

    endif

#ifdef __HARBOUR__

  case (key == K_ALT_K)

    b:= hb_gtInfo( HB_GTI_CLIPBOARDDATA )

    if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo
      fixbuff(get,b+"  ")
      get:display()
      get:changed:=.t.
    else
      a:=getlines(b)
      c:=''
      if len(a)=1
         kibord(a[1])
      else
         aeval(a,{|x,y|if(!empty(left(x,5)),c+=x+chr(13)+chr(13)+chr(3),)})
         kibord(c)
      endif
    endif

  case (key == K_ALT_B .or. key == K_ALT_X)

    hb_gtInfo( HB_GTI_CLIPBOARDDATA , AllTrim(get:buffer))

  case (key == K_ALT_E)

    hb_gtInfo( HB_GTI_CLIPBOARDDATA , AllTrim(get:buffer))
    get:Home()
    get:DelEnd()

#endif

  otherwise

    if (key >= 32 .and. key <= 255) .and. (cKey := Chr(key),.t.) .or. ;
       (key == K_CTRL_Q) .and. get:type == "C" .and. setcursor(setcursor()+2)>0 .and. (cKey := chr(max(0,min(255,inkey(0)))))="" .and. setcursor(setcursor()-2)>=0

      if get:type == "N" .and. (cKey $ ".,")
         get:ToDecPos()
#ifdef A_JMO
      elseif get:type == "N" .and. "r."$get:buffer .and. (cKey $ "Rr")
        get:End()
#endif
      else
        if get:type="N" .and. get:clear
          if isdigit(cKey) .or. cKey=' '
            get:todecpos()
            if get:pos>get:decpos
              get:left()
              get:Overstrike(cKey)
              get:left()
            else
              get:Overstrike(cKey)
            endif
            NumFlag:=.t.
#ifdef __HARBOUR__
            get:changed:=.t.
#endif
          elseif cKey='-'
            get:todecpos()
            if get:pos>get:decpos
              get:left()
            endif
            get:left()
            get:Overstrike(cKey)
          endif
#ifdef A_JMO
        elseif get:type="N" .and. "r."$get:buffer .and. ! (NumFlag=.f.) .and. isdigit(cKey)
           if get:pos<get:decpos
              fixbuff(get,subs(stuff(get:buffer,get:pos+1,0,cKey),2))
              NumFlag:=(get:pos=get:decpos-2)
           else
              fixbuff(get,stuff(stuff(get:buffer,get:decpos+1,1,""),get:pos,0,cKey))
              NumFlag:=(get:pos=len(get:buffer))
           endif
           get:display()
           get:changed:=.t.
#endif
        elseif get:type="N" .and. get:pos<get:decpos .and. isdigit(cKey) .and. !(NumFlag=.f.)
           if get:picture=NIL .or. get:picture="@" .and. !" "$get:picture
              fixbuff(get,subs(stuff(get:buffer,get:pos+1,0,cKey),2))
           else
              a:=get:untransform()
              b:=if(get:minus.or.a<0,-1,1)
              a:=int(a)*10+a%1+b*(key-48)
              fixbuff(get,tran(a,get:picture))
           endif
           get:display()
           get:changed:=.t.
           if get:pos=get:decpos-1
              NumFlag:=.t.
           endif
        else

        /*
        if Set(_SET_INSERT)
           if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. (right(get:buffer,2)#" " .or. get:pos>=len(get:buffer)-1)  //expandable field
              fixbuff(get,stuff(get:buffer,get:pos,0,cKey))
              //get:buffer:=stuff(get:buffer,get:pos,0,cKey)
              get:right()
              //get:changed:=.t.
              //get:display()
           else
              get:Insert(cKey)
           endif
        else
           get:Overstrike(cKey)
           if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. get:typeout
              fixbuff(get,get:buffer+" ")
              //get:buffer+=" "
              get:right()
              //get:display()
           endif   
        endif
        */


        if Set(_SET_INSERT)
           if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. (right(get:buffer,2)#" " .or. get:pos>=len(get:buffer)-2)  //expandable field
              fixbuff(get,get:buffer+'  ')
              //get:right()
           endif
           get:Insert(cKey)
        else
           get:Overstrike(cKey)
           if get:type $ "CM" .and. valtype(get:cargo)="L" .and. get:cargo .and. get:typeout
              fixbuff(get,get:buffer+"  ")
              get:right()
           endif   
        endif
        NumFlag:=get:typeout
        if get:decpos=get:pos-1
           get:left()
           NumFlag:=.t.
        endif
    endif

    if (get:typeOut .and. !Set(_SET_CONFIRM) )
       if ( Set(_SET_BELL) )
          ?? Chr(7)
       endif
       get:exitState := GE_ENTER
    endif
      
    endif

endif

endcase

  if get:type="N"
  if NumFlag=NIL
     NumFlag:=.f.
  endif
  if NumFlag
     setcursor(2)
  else
     setcursor(1)
  endif
  endif
return
**************************

func GetExitKey(get, key)
local bKeyBlock,bKeyCond

  do case
  case ( (bKeyBlock := SetKey(key)) <> NIL )
    GetDoSetKey(bKeyBlock, get)

  case ( key == K_UP )
    get:exitState := GE_UP

  case ( key == K_DOWN )
    get:exitState := GE_DOWN

  case ( key == K_SH_TAB )
    get:exitState := GE_LEFT

  case ( key == K_TAB .or. key == K_ENTER)
    get:exitState := K_ENTER

  case ( key == K_ESC )
    if ( Set(_SET_ESCAPE) )
      get:undo()
      get:changed:=.f.
      get:exitState := GE_ESCAPE
    end

  case ( key == K_PGUP )
    get:exitState := GE_PGUP

  case ( key == K_PGDN )
    get:exitState := GE_PGDN

  case ( key == K_CTRL_HOME )
    get:exitState := GE_TOP

#ifdef CTRL_END_SPECIAL
  // both ^W and ^End go to the last GET
  case (key == K_CTRL_END)
    get:exitState := GE_BOTTOM
  case (key == GE_WRITE .or. key==K_F10)
    get:exitState := GE_WRITE
#else
  // both ^W and ^End terminate the READ (the default)
  case (key == K_CTRL_W .or. key == K_F10 .or. key == GE_WRITE)
    get:exitState := GE_WRITE
#endif
  otherwise
    return .f.
endcase

return .t.
**************************

/***
*  GetPreValidate()
*  Test entry condition (WHEN clause) for a GET.
*/
func GetPreValidate(get)

local when := .t.

  get:exitState := GE_NOEXIT    // prepares for editing


  if ( get:preBlock <> NIL )

    when := Eval(get:preBlock, get, eval(get:block), agetlist, if(get==ActiveGet,LastPos,ascan( agetlist, get )))

    get:Display()

  if ( !when )
    get:exitState := GE_WHEN    // indicates failure

  end

  end
return (when)



/***
*  GetPostValidate()
*  Test exit condition (VALID clause) for a GET.
*
*  NOTE: bad dates are rejected in such a way as to preserve edit buffer.
*/
func GetPostValidate(get)

local valid := .t.,x,y,z


  if ( get:exitState == GE_ESCAPE )
    get:undo()
    get:changed:=.f.
    return (.t.)          // NOTE
  end

  if ( get:BadDate() )
    get:Home()
    DateMsg()
    return (.f.)          // NOTE
  end

  // if editing occurred, assign the new value to the variable
  if ( get:changed )
    x:=get:varget()
    y:=get:untransform()
    /*
    if get:type$"CM" .and. valtype(get:cargo)="L" .and. get:cargo
        get:varput(get:buffer) //assign używa untransform, a ten ma błąd
    else
        get:Assign()
    endif
    */
    get:reset()
    if .not. y==get:untransform() // var nie byl ustawiony
       get:varput(y)
       get:reset()
    endif
    get:changed:=.t.
    Updated(@valid)
#ifdef A_LAN
    if !valid
      return .f.
    endif
#endif
  end

  // reform edit buffer, set cursor to home position, redisplay
  //if get:type#'L'
     get:home()
     //get:Reset() bo ma błąd dla expandable
  //endif


  // check VALID condition if specified
  if ( get:postBlock <> NIL )

    valid := Eval(get:postBlock, get, get:varget(), agetlist, if(get==ActiveGet,LastPos,ascan( agetlist, get )))

    if get:type$"CM" .and. valtype(get:cargo)="L" .and. get:cargo
        fixbuff(get,get:varget())
        //get:buffer:=get:varget() //updatebuffer ma błąd
        //get:display()
    else
        get:UpdateBuffer()
    endif

  end

return (valid)




/***
*  GetDoSetKey()
*  Process SET KEY during editing.
*/
proc GetDoSetKey(keyBlock, get)

local x

  // if editing occurred, assign the new value to the variable
  if ( get:changed )
    if get:type$"CM" .and. valtype(get:cargo)="L" .and. get:cargo
        get:varput(get:buffer) //assign używa untransform, a ten ma błąd
    else
        get:Assign()
    endif
    Updated(.t.)
  end


  Eval(keyBlock, ReadProcName, /*ReadProcLine, ReadVar(),*/ get, agetlist, if(get==ActiveGet,LastPos,ascan( agetlist, get )))

    if get:type$"CM" .and. valtype(get:cargo)="L" .and. get:cargo
        fixbuff(get,get:varget())
        //get:buffer:=get:varget() //updatebuffer ma błąd
        //get:display()
    else
        get:UpdateBuffer()
    endif

return



/**************************
*
*  READ services
*
*/



/***
*  Settle()
*
*  Returns new position in array of Get objects, based on
*
*    - current position
*    - exitState of Get object at current position
*
*  NOTE exitState of old Get is transferred to new Get
*/
static func Settle(GetList, pos)

local exitState, ret := .t. ,b

  exitState := GetList[pos]:exitState

  if   exitState == NIL
    exitState := LastExit
  end

  if ( exitState <> GE_WHEN )
    // reset state info
    LastPos := pos
    BumpTop := .f.
    BumpBot := .f.

  else
    // re-use last exitState, do not disturb state info
    exitState := LastExit
    if exitstate=GE_NOEXIT
       exitstate:=GE_DOWN
    endif

  end


  /***
  *  move
  */
  do case
#ifdef A_MYSZ
  case ( exitState == GE_MOUSE )
    if mysz[1]=2
#ifndef __HARBOUR__
       b:=2
       MSHOW()
       do while b#0
          sysint(51,3,@b)
       enddo
#endif
       MHIDE()
       ret := .f.
    else
    b:=ascan(getlist,{|g|g:row=mysz[3] .and. g:col<=mysz[2] .and. g:col+len(tran(g:varGet(),g:picture))-1>=mysz[2]})
    if b=0
       ret:=.f.
    else
       pos:=b
       exitState := GE_DOWN
    endif
    endif

#endif
  case ( exitState == GE_UP .or. exitState == GE_LEFT )
    pos --

  case ( exitState == GE_DOWN .or. exitState == GE_ENTER)
    pos ++

  case ( exitState == GE_TOP )
    pos := 1
    BumpTop := .T.
    exitState := GE_DOWN

#ifdef CTRL_END_SPECIAL
  case ( exitState == GE_BOTTOM )
    pos := Len(GetList)
    BumpBot := .T.
    exitState := GE_UP
#endif

  case ( exitState == GE_ESCAPE .or. exitState == GE_WRITE .or. exitState == GE_PGUP .or. exitState == GE_PGDN )
    ret := .f.

  endcase


  /***
  *  bounce
  */
  if ( pos < 1 )             // bumped top

    pos := LastPos
    if ( !Set(_SET_EXIT) .and. !BumpBot )
      BumpTop := .T.
      exitState := GE_DOWN
    else
      ret := .f.
    end

  elseif ( pos > Len(GetList) )    // bumped bottom

    pos := LastPos
    if ( !Set(_SET_EXIT) .and. exitState <> GE_ENTER .and. !BumpTop )
      BumpBot := .T.
      exitState := GE_UP
    else
      ret := .f.
    end
  end


  // record exit state
  LastExit := exitState
  GetList[pos]:exitState := exitState

return (ret)



/***
*  PostActiveGet()
*  Post active GET for ReadVar(), GetActive().
*/
static proc PostActiveGet( get , pos )

  GetActive( get )
  ReadVar( GetReadVar(get) )
  GetPos( pos )

return



/***
* ClearGetSysVars()
*  Save and clear READ state variables. Return array of saved values.
*
*/
static func ClearGetSysVars()

local saved[ GSV_COUNT ]


  saved[ GSV_BUMPTOP ] := BumpTop
  BumpTop := .f.

  saved[ GSV_BUMPBOT ] := BumpBot
  BumpBot := .f.

  saved[ GSV_LASTEXIT ] := LastExit
  LastExit := GE_NOEXIT

  saved[ GSV_LASTPOS ] := LastPos
  LastPos := 0

  saved[ GSV_ACTIVEGET ] := GetActive( NIL )

  saved[ GSV_READVAR ] := ReadVar( "" )

  saved[ GSV_READPROCNAME ] := ReadProcName
  ReadProcName := rpn
  rpn := ''

  saved[ GSV_UPDATED ] := Updated
  Updated := .f.

  saved[ GSV_CURSOR] := setcursor(if(set(_SET_INSERT),2,1))

  saved[ GSV_EDITBLOCK ] := EditBlock
  EditBlock := EBN
  EBN := NIL

  saved[ GSV_GETLIST ] := aGetList

return (saved)



/***
*   RestoreGetSysVars()
*  Restore READ state variables from array of saved values.
*
*/
static proc RestoreGetSysVars(saved)

  BumpTop := saved[ GSV_BUMPTOP ]

  BumpBot := saved[ GSV_BUMPBOT ]

  OldExit := LastExit

  LastExit := saved[ GSV_LASTEXIT ]

  LastPos := saved[ GSV_LASTPOS ]

  GetActive( saved[ GSV_ACTIVEGET ] )

  ReadVar( saved[ GSV_READVAR ] )

  ReadProcName := saved[ GSV_READPROCNAME ]

  OldUpdated := Updated

  Updated := saved[ GSV_UPDATED ]

  setcursor(saved[ GSV_CURSOR])

  EditBlock := saved[ GSV_EDITBLOCK ]

  aGetList := saved[ GSV_GETLIST ]

return



/***
*  GetReadVar()
*  Set READVAR() value from a GET.
*/
static func GetReadVar(get)

local name := Upper(get:name)


//#ifdef SUBSCRIPT_IN_READVAR
local i

  /***
  *  The following code includes subscripts in the name returned by
  *  this function, if the get variable is an array element.
  *
  *  Subscripts are retrieved from the get:subscript instance variable.
  *
  *  NOTE: incompatible with Summer 87
  */

  if ( get:subscript <> NIL )
    for i := 1 to len(get:subscript)
      name += "[" + ltrim(str(get:subscript[i])) + "]"
    next
  end

//#endif

return (name)



/**********************
*
*  system services
*
*/

func GetListActive(g)
local oldActive := aGetList
  if ( PCount() > 0 )
    aGetList := g
  end
return ( oldActive )

/***
*  GetActive()
*/
func GetActive(g)
local oldActive := ActiveGet
  if ( PCount() > 0 )
    ActiveGet := g
  end
return ( oldActive )

/***
*  GetPos()
*/
func GetPos(p)
local oldPos := LastPos
  if ( PCount() > 0 )
    LastPos := p
  end
return ( OldPos )

#ifdef __HARBOUR__
PROCEDURE __SetFormat( bFormat )
  RETURN
#endif

/***
*  Updated()
*/
func Updated(lNew,nebl)
  if lnew#NIL
#ifdef A_LAN
    if lNew .and. EditBlock#NIL
       nebl:=eval(EditBlock,activeget,agetlist,LastPos)
       if nebl=.f.
          if activeget#NIL
             activeget:undo()
             activeget:changed:=.f.
             lNew:=.f.
          endif
       elseif !(nebl=.t.)
          EditBlock:=nebl
       endif
    endif
#endif
    Updated := lNew
#ifdef A_LAN
  elseif pcount()>1
    lnew:=EditBlock
    if valtype(nebl)$"BU"
       EditBlock:=nebl
    endif
#endif
  else
    lnew:=Updated
  end
return if(pcount()>0,lnew,oldUpdated)

func OldUpdated(lnew)
local l
l:=oldUpdated
if lnew<>NIL
   oldupdated:=lnew
endif
return l

func __SetEditBlock(x)
local r := EBN
  if PCount()>0
     EBN:=x
  endif
return r

/***
*  __SetProc()
*/
func __SetProc(x)
  if PCount()>0
    if valtype(x)='N'
       x:=procname(++x)
    endif
    RPN:=x
  endif
return readprocname

/***
* ReadKey()
*/
func Readkey(KNew,x)
if pcount()=1
   LastExit:=KNew
elseif pcount()=2
#ifdef A_MYSZ
   if x=NIL
   return mysz
   elseif knew=14
   mysz:=x
   x:=knew
#endif
   knew:=OldExit
   OldExit:=x
   return knew
#ifdef A_MYSZ
   endif
#endif
endif
return OldExit

/**********************************
*
*  wacky compatibility services
*
*/


/***
*  DateMsg()
*/
static proc DateMsg()

    alarm("Nieprawidłowa data",,3,3)

return

***************
static proc getval(get)
  local valid:=.t.,r:=get:row,c:=get:col,g,t,l:=len(get:buffer),mes,pn,sgn,k
   pn:=readprocname
  if get:type="D"
     if empty(get:name)
        return
     endif
     if ( get:changed )
       get:Assign()
       updated(@valid)
#ifdef A_LAN
       if ( !valid )
         get:exitState := GE_NOEXIT
         return
       end
#endif
     endif
     t:=padr(get:name,50)
  else
     t:=get:varget()
     k:=get:untransform()
     if get:changed
       get:reset()
       if .not. k==get:untransform() // var nie byl ustawiony
         get:varput(k)
         get:reset()
         t:=k
       endif
       get:changed:=.t. //a bo ja wiem co robi reset
     endif
#ifdef A_JMO
       if "r."$get:buffer
          g:=get:varget()
          sgn:=abs(eval(get:block,1)) //jaki przelicznik
          get:varput(g)
          t:=int(t)*sgn+(t%1)*1000
       endif
#endif
     if t=k
        if t=0
          t:=space(50)
        else
          t:=ltrim(str(t,l,l-get:decpos))+space(50)
        endif
     else
        t:=str(t,l+20,l-get:decpos+20)
        for k:=len(t) to l step -1
           if subs(t,k,1)<>'0'
              exit
           endif
        next k
        t:=ltrim(left(t,k))+space(50)
     endif
  endif
  pn:=readprocname
  readprocname:=procname()
  mes:=window(1,25)
  colorselect(3)
  @ mes[1],mes[2]+2 say "Kalkulator: + - * / ^ ( )"
  colorselect(0)
  g:=getnew(mes[1]+1,mes[2]+1,{|SET|IF(SET=NIL,T,T:=SET)},"t","@S27")
  g:cargo:=.t.
  g:setfocus()
  tone(261.7,1);tone(130.8,1);tone(164.8,1)
  setcursor(mes[8])
  if t#" "
     g:end()
  endif
  do while .t.
     g:exitstate:=GE_NOEXIT
     while ( g:exitState < GE_ENTER )
       k:=INkey(0)
       if k=asc(',')
          k:=asc('.')
       endif
       GetApplyKey( g,k )
     end
     if g:exitstate == GE_ESCAPE .or. !g:changed .or. empty(g:buffer)
        exit
     endif
     t:=g:buffer
     if type(t)=get:type
#ifdef A_JMO
       if sgn#NIL
         t:=int(&(t))
         fixbuff(get,alltrim(tran(int(t/sgn)+(t%sgn)/1000,get:picture)))
         //get:varput(int(t/sgn)+(t%sgn)/1000)
         get:changed:=.t.
       else
#endif
         //fixbuff(get,alltrim(tran(&(t),get:picture)))
         valid:=.t.
         updated(@valid)
#ifdef A_LAN
       if ( !valid )
         get:exitState := GE_NOEXIT
         exit
       end
#endif
         get:varput(&(t))
         get:reset()
         get:changed:=.t.
#ifdef A_JMO
       endif
#endif
       exit
    endif
    alarm("Tego się nie da wyliczyć",,3,3)
  enddo
  g:killfocus()
  readprocname:=pn
  message(mes)
  get:display()
return
******
static proc getchr(get,expandable)
   static bl:=''
   local k,m,n,sc,lc,TXT,getlist:={},prevlen,b:={0,0},bp:=2,ll,osk:={}
   memvar r1,c1,r2,c2,defa,l,c,cl,cc,ch,ww
   private r1,c1,r2,c2,l,c,cl,cc,ch:=.f.,ww:=.f.
#ifdef __HARBOUR__
   ll:=1020
#else
   ll:=254
#endif

  m:=if(get:picture=NIL,40,max(40,val(subs(get:picture,1+at("S",get:picture)))))
  if expandable
     txt:=trim(get:buffer)+' ' //trim(get:untransform())
  else
     txt:=get:untransform()
     prevlen:=len(txt)
  endif
  k:=mlcount(txt,maxcol()-2,2,.t.)
  for n:=1 to k
    m:=max(m,len(TRIM(memoline(txt,maxcol()-2,n,2,.t.))))
  next

  k:=MAX(k,6)
  r1:=get:row
  c1:=get:col
   
  c2:=c1+1+m
  r2:=r1+k+1

   IF c2>maxcol() // poza ekran
      c1-=c2-maxcol()
      c2:=maxcol()
      if c1<0
         c1:=0
      endif
   ENDIF

   IF r2>maxrow() // poza ekran
      r1-=r2-maxrow()
      r2:=maxrow()
      IF r1<0
         r1:=0
      ENDIF
   ENDIF

    sc = SAVESCREEN(r1,c1,r2,c2)
    if iscolor()
       @ r1,c1,r2,c2 BOX UNICODE '┌─┐│┘─└│ ' color "BG+/BG"
       lc = SETCOLOR("W+/B")
    else
        lc = SETCOLOR("I")
        @ r1,c1,r2,c2 BOX UNICODE '┌─┐│┘─└│ '
    endif
        SET COLOR TO I
        @ r1,c1+3  say 'WPIS WIELOWIERSZOWY'
        @ r2,c1+3 say 'Esc'
        SAYl "^→"
        SAYl "^←"
        SAYl 'Hom'
        SAYl 'End'
        sayl 'PgU'
        sayl 'PgD'
        sayl '^Y'
        sayl '^W'
        sayl "F2"

        SET COLOR TO W

#ifdef __HARBOUR__
osk:=HB_SETKEYSAVE()
#command RESET KEY <key> [TO <new>] => setkey(<key>,[<{new}>])
#else
#command RESET KEY <key> [TO <new>] => aadd(osk,{<key>,setkey(<key>,[<{new}>])})
#endif

  RESET KEY K_INS TO SetCursor(if(set(_SET_INSERT,!SET(_SET_INSERT)),1,2))
  RESET KEY K_CTRL_RET
  RESET KEY K_ALT_RETURN
  RESET KEY K_F2
  RESET KEY K_F10
  RESET KEY K_CTRL_W
  RESET KEY K_CTRL_K
  //RESET KEY K_ALT_X
  //RESET KEY K_ALT_B
  //RESET KEY K_ALT_M
  //RESET KEY K_ALT_K
  //RESET KEY K_ALT_E
  RESET KEY GE_WRITE

  if "utf-8"$lower(left(txt,80))
      txt:=hb_translate(txt,'UTF8',)
  endif
  do while .t.
    //txt=MEMOEDIT(txt,r1+1,c1+1,r2-1,c2-1,.T.,"gufunc",c2-c1-3,8,l,c,cl,cc)
    txt=MEMOEDIT(txt,r1+1,c1+1,r2-1,c2-1,.T.,"gufunc",ll,2,l,c,cl,cc)
    k:=lastkey()
    if k=K_CTRL_K
       m:=message("PODAJ  (B,M,K,E,R,W);ROZKAZ:;... ")
       k:=upper(chr(inkey(0)))
       if k$'RW'
         @ m[1]+1,m[2]+8 say "NAZWĘ: "
         n:=pad(defa,64)
         @ m[1]+2,m[2]+2 get n picture "@KS14"
         read
         if empty(n)
         elseif k="R"
          if !file(n)
           n:=defa+n
          endif
          txt:=memoread(n)
          if "utf-8"$lower(left(txt,80))
            txt:=hb_translate(txt,'UTF8',)
          endif
         else
          k:=strtran(txt,chr(141)+chr(10))
          if "utf-8"$lower(left(k,80))
            k:=hb_translate(k,,'UTF8')
          endif
          HB_MEMOWRIT(n,k,.f.)
         endif
       elseif k$"BX"
        bp:=3-bp
        b[bp]:=mlctopos(txt,ll,l,c,2,ww)
        if ch
           ch:=.f.
           b[3-bp]:=b[bp]
        endif
        bl:=subs(txt,min(b[1],b[2]),abs(b[2]-b[1]))
#ifdef __HARBOUR__
        hb_gtInfo( HB_GTI_CLIPBOARDDATA, bl )
#endif
        //restscreen(cl,cc,cl,cc,hiattr(savescreen(cl,cc,cl,cc)))
       elseif k='K'
#ifdef __HARBOUR__
        bl:= hb_gtInfo( HB_GTI_CLIPBOARDDATA )
#endif
        txt:=stuff(txt,k:=mlctopos(txt,ll,l,c,2,ww),0,bl)
        n:=len(bl)
        if b[1]>k
           b[1]+=n
        endif
        if b[2]>k
           b[2]+=n
        endif
        if abs(b[2]-b[1])#n
           ch:=.t.
        endif
       elseif k="E"
        if !ch
#ifdef __HARBOUR__
           hb_gtInfo( HB_GTI_CLIPBOARDDATA, bl )
#endif
           txt:=stuff(txt,min(b[1],b[2]),abs(b[2]-b[1]),"")
           ch:=.t.
        endif
       elseif k="M"
        if !ch
#ifdef __HARBOUR__
           hb_gtInfo( HB_GTI_CLIPBOARDDATA, bl )
#endif
           txt:=stuff(txt,k:=mlctopos(txt,ll,l,c,2,ww),0,bl)
           n:=len(bl)
           if b[1]>k
              b[1]+=n
           endif
           if b[2]>k
              b[2]+=n
           endif
           if abs(b[2]-b[1])=n
              txt:=stuff(txt,min(b[1],b[2]),n,"")
              b[1]:=k
              b[2]:=k+n
           else
              ch:=.t.
           endif
       endif
     endif
     message(m)
     loop
    elseif k=K_F2
        if ww:=!ww
           ll:=c2-c1-3
        else
           txt:=strtran(txt,chr(141)+chr(10))
#ifdef __HARBOUR__
           ll:=1020
#else
           ll:=254
#endif
        endif
        loop
    elseif k=K_CTRL_END .or. k=K_F10 .or. k=GE_WRITE
        k:=GE_WRITE
    endif
    exit
  enddo
#ifdef __HARBOUR__
  HB_SETKEYSAVE(osk)
#else
  aeval(osk,{|x|setkey(x[1],x[2])})
#endif
  if k=GE_WRITE
        k:=strtran(txt,chr(141)+chr(10))
        if "utf-8"$lower(left(txt,80))
          k:=hb_translate(k,,'UTF8')
        endif
        if expandable
           k:=trim(k)
           m:=val(subs(get:picture,1+at("S",get:picture)))
           if len(k)<m
              k:=padr(k,m)
           endif
        endif
        fixbuff(get,k)
  endif

    RESTSCREEN(r1,c1,r2,c2,sc)

    SETCOLOR(lc)
    get:display()

return
*********
FUNC gufunc(mode,line,column)
  memvar r1,c1,r2,c2,l,c,cl,cc,ch,ww
  static spec:=.f.
  local key
  if (mode=1 .or. mode=2)
     key:=lastkey()
     if spec=.t.
        spec:=.f.
        ch:=.t.
        return 33
     elseif key=K_CTRL_Q
        spec:=.t.
        return 32
     elseif 0#ascan({K_CTRL_K,K_ALT_X,K_ALT_B,K_ALT_K,K_ALT_E,K_ALT_M,K_F2,K_F10,GE_WRITE},key)
        l:=line
        c:=column
        cl:=row()
        cc:=col()
#ifdef __HARBOUR__
        cc-=c1+1
        cl-=r1+1
#endif
        return K_CTRL_END
     endif
     ch:=mode=2

  elseif mode=3 .and. !ww
     if spec=NIL
        spec:=.f.
        return 0
     endif
     spec:=NIL
     return 34

  elseif mode=0
     @ r2,c2-8 SAY str(line,3)+","+str(column,3) COLOR "I"
  endif

RETURN(0)
*******
