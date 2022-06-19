#include "set.ch"
#include "inkey.ch"

procedure help(pro)
memvar c1,c2,r1,r2,defa,l,c,cl,cc,ww,ch,oprn

local hrow,hcol,hwbuf,lastcolor,htext,curs,rins,skh,skins,txt
local osk:={},ll,m,n,getlist:={},k,b:={0,0},bp:=2
static bl:=''
private c1,c2,r1,r2,l,c,cl,cc,ww:=.t.,ch:=.f.

if pro=="TAK"
  pro:="t_"+procname(4)
elseif pro=="ALARM"
  pro:="a_"+procname(4)
elseif pro=="ACZOJS"
  pro:="c_"+procname(4)
endif

  if '.'$pro
     htext:=pro
  else
     htext=lower(left(pro,8))
     do case
         case right(htext,3)="_in"
          htext="dok_in"
         case htext="wydruk"
         htext="wydruk"
     endcase
     htext+=".hlp"
  endif

    IF FILE(htext).or.FILE(htext:=defa+"pomoc"+HB_ps()+htext)
       txt=MEMOREAD(htext)
    else
       txt="Brak pomocy na ten temat."
    ENDIF


   r1=ROW()+1    // sciaga pod polem
   c1=MAX(0,col()-33)
   
   c2=c1+INT(67/79*maxcol())
   r2=r1+MLCOUNT(TXT,C2-C1-5,4,.T.)+3

   IF c2>maxcol() // poza ekran
      c1-=c2-maxcol()
      c2=maxcol()
      IF c1<0
         c1=0
      ENDIF
   ENDIF

   IF r2>maxrow() // poza ekran
      r1-=r2-maxrow()
      r2=maxrow()
      IF r1<0
         r1=0
      ENDIF
   ENDIF
    hrow:=row()
    hcol:=col()
    rins = SET(_SET_INSERT,.f.)
    curs=setcursOR(1)
        * save screen in memvar
        hwbuf = SAVESCREEN(r1,c1,r2,c2)

        * clear window and draw box
        lastcolor = SETCOLOR("RB+/GR")
        scroll(r1,c1,r2,c2, 0)
        @ r1+1,c1+2,r2-1,c2-2 BOX 'ÚÄ¿³ÙÄÀ³'
        SET COLOR TO I
        @ r2-1,c1+3 say 'Esc'
        @ r2-1,c1+7 SAY "^"+CHR(26)
        @ r2-1,c1+10 SAY "^"+CHR(27)
        @ r2-1,c1+13 SAY 'Hom'
        @ r2-1,c1+17 SAY 'End'
        @ r2-1,c1+21 say 'PgU'
        @ r2-1,c1+25 say 'PgD'
        @ r2-1,c1+29 say '^Hom'
        @ r2-1,c1+34 say '^End'
        @ r2-1,c1+39 say '^PgU'
        @ r2-1,c1+44 say '^PgD'
        @ r2-1,c1+49 SAY CHR(17)+CHR(196)+CHR(217) // Enter
        @ r2-1,c1+53 say '^Y'
        @ r2-1,c1+56 say '^W'
        @ r1+1,c1+3  say ' P O M O C '
        @ r1+1,c2-2-len(htext) say htext
        set color to (if(iscolor(),"GR+/GR","W"))

#ifdef __HARBOUR__
osk:=HB_SETKEYSAVE()
#command RESET KEY <key> [TO <new>] => setkey(<key>,[<{new}>])
#else
#command RESET KEY <key> [TO <new>] => aadd(osk,{<key>,setkey(<key>,[<{new}>])})
#endif

  RESET KEY K_INS TO SetCursor(if(set(_SET_INSERT,!SET(_SET_INSERT)),1,2))
  RESET KEY K_F1
  RESET KEY K_F2
  //RESET KEY K_F10
  RESET KEY K_CTRL_W
  RESET KEY K_CTRL_K
  RESET KEY K_ALT_X
  RESET KEY K_ALT_B
  RESET KEY K_ALT_M
  RESET KEY K_ALT_K
  RESET KEY K_ALT_E
  RESET KEY K_CTRL_P
  RESET KEY K_CTRL_Q
  RESET KEY K_CTRL_L
  reset key K_RIGHT
  reset key K_LEFT
  reset key K_UP
  reset key K_DOWN

  ll:=C2-C1-5

  do while .t.
    //txt=MEMOEDIT(txt,r1+2,c1+3,r2-2,c2-3,.T.,"hufunc",C2-C1-5,8,l,c,cl,cc)
    txt=MEMOEDIT(txt,r1+2,c1+3,r2-2,c2-3,.T.,"hufunc",ll,8,l,c,cl,cc)
    k:=lastkey()
    if k=K_CTRL_W .or. k=K_F10 .or. k=K_CTRL_L
          MEMOWRIT(HTEXT,strtran(txt,chr(141)+chr(10)))
    elseif k=K_CTRL_P .and. 1=alarm("Czy drukowa†",{"Tak","Nie"},2)
          set console off
          k:=getlines(hardcr(txt))
          print()
#ifdef A_HPDF
  #define D_HWPRN A_HPDF
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
#ifdef D_HWPRN
          if valtype(oprn)='O'
             aeval(k,{|y|wqq(y),wq()})
             oprn:Destroy()
          else
#endif
             aeval(k,{|x|qqout(x),qout()})
#ifdef D_HWPRN
          endif
          oprn:=NIL
#endif
#ifdef A_PRINT
          k:=set(_SET_PRINTFILE,'')
          if ! k==set(_SET_PRINTFILE) .and. File(k)
            A_PRINT(k)
          endif
#endif
          set print off
          set console on
    elseif k=K_CTRL_K
       m:=message("PODAJ  (R, W);ROZKAZ:;... ")
       k:=upper(chr(INkey(0)))
       @ m[1]+1,m[2]+8 say "NAZW¨: "
       n:=pad(defa,64)
       @ m[1]+2,m[2]+2 get n picture "@KS14"
       read
       if empty(n)
       elseif k="R"
        if !file(n)
        n:=defa+n
       endif
       txt:=memoread(n)
       else
        MEMOWRIT(n,strtran(txt,chr(141)+chr(10)))
     endif
     message(m)
     loop
    elseif k=K_ALT_B .or. k=K_ALT_X
        bp:=3-bp
        b[bp]:=mlctopos(txt,ll,l,c,8,ww)
        if ch
           ch:=.f.
           b[3-bp]:=b[bp]
        endif
        bl:=subs(txt,min(b[1],b[2]),abs(b[2]-b[1]))
        loop
    elseif k=K_ALT_K
        txt:=stuff(txt,m:=mlctopos(txt,ll,l,c,8,ww),0,bl)
        n:=len(bl)
        if b[1]>m
           b[1]+=n
        endif
        if b[2]>m
           b[2]+=n
        endif
        if abs(b[2]-b[1])#n
           ch:=.t.
        endif
        loop
    elseif k=K_ALT_E
        if !ch
           txt:=stuff(txt,min(b[1],b[2]),abs(b[2]-b[1]),"")
           ch:=.t.
        endif
        loop
    elseif k=K_ALT_M
        if !ch
           txt:=stuff(txt,m:=mlctopos(txt,ll,l,c,8,ww),0,bl)
           n:=len(bl)
           if b[1]>m
              b[1]+=n
           endif
           if b[2]>m
              b[2]+=n
           endif
           if abs(b[2]-b[1])=n
              txt:=stuff(txt,min(b[1],b[2]),n,"")
              b[1]:=m
              b[2]:=m+n
           else
              ch:=.t.
           endif
        endif
        loop
    elseif k=K_F2
        if ww:=!ww
           ll:=C2-C1-5
        else
           txt:=strtran(txt,chr(141)+chr(10))
#ifdef __HARBOUR__
           ll:=1020
#else
           ll:=254
#endif
        endif
        loop
    endif
    exit
  enddo

#ifdef __HARBOUR__
  HB_SETKEYSAVE(osk)
#else
  aeval(osk,{|x|setkey(x[1],x[2])})
#endif
        * restore window
        RESTSCREEN(r1,c1,r2,c2, hwbuf)

        SETCOLOR(lastcolor)

        * restore cursor
    setpos(hrow,hcol)
  setcursOR(curs)

RETURN

******
*   hufunc()
*
*   user defined function to be called from DBEDIT
******
FUNC hufunc(mode,line,column)
  memvar c1,r2,c2,l,c,cl,cc,ch,ww
  static spec:=.f.
  local key
  if (mode=1 .or. mode=2)
     key:=lastkey()
     if spec
        spec:=.f.
        ch:=.t.
        return 33
     elseif key=K_CTRL_Q
        spec:=.t.
        return 32
     elseif key=K_F1
        return K_ESC
     elseif 0#ascan({K_CTRL_K,K_ALT_X,K_ALT_B,K_ALT_K,K_ALT_E,K_ALT_M,K_F2,K_F10,K_CTRL_L},key)
        l:=line
        c:=column
        cl:=row()
        cc:=col()
#ifdef __HARBOUR__
        cc-=c1+3
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
     @ r2-1,c2-8 SAY str(line,3)+","+str(column,3) COLOR "I"
  endif

RETURN(0)
***********
