//flagi w kontach EFKX
//flagi PKZ
#ifndef A_AUTOFIX
#define A_AUTOFIX
#endif

#ifndef A_MKNK
  #define A_MKNK(x) pad(str(x,mknk),5)
#endif

#include "ar.ch"

#ifndef A_LAN
#define DatY MEMVAR
#endif

#ifdef A_DPS
#define nazwA nazwisko
#endif

#include "inkey.ch"

field nazwA,numer_kol IN firmy
field lp,data,ident,status,kontrahent,konto
field kwota,rejestr,czyma,link,pozycja IN MAIN
field sald_bie_m,sald_bie_w,syntet,khflag IN KONTA
#ifdef A_DSPLIT
field smb_dow,nr_dowodu
#define dowoD smb_dow+nr_dowodu
#else
field dowod
#define smb_doW dowod
#define nr_dowodU SubStr(dowod,3)
#endif
#ifndef A_IDENT
  #define A_IDENT hb_fieldlen('ident')
#endif
MEMVAR _sbnorm,rejestry,_snorm,is_spec,_sbkgr,_sramka
memvar da,defa
static ppd:={}
**********************************
func dok //append - .t. append blank .f. - noedit
    parameters rej,l_p,lappend,j
    local txt,k,l,m,n,o,p,d_d,l_k,sk,kh,chg,gl,s,i,stat,key,r,linecount,x,f9dok,fixpoz
    memvar getlist,self,ar,ad,af,adm,adw,buf,win,br,rej,l_p,lappend,j,getpos,mknk
    private da,getlist,self,ar,ad,af,adm,adw,buf,win,br,getpos,mknk

    ar:=ascan(rejestry,{|ar|ar[AR_SMB]==rej})
    chg:=.f.
    if ar=0
       RETURN chg
    endif
    ar:=rejestry[ar]
    i:=j
    getlist:={}
    getpos:=1
    stat:=push_stat()
    begin sequence
    sel("konta","kont_num")
    sel("main","main_lp")
    if lappend=.t.
       da:=max(DatE(),DatY->d_z_mies1+1)
       do while YEAR(da)>YEAR(DatY->D_Z_rok+1)
          da-=day(da)
       enddo
       if da<=DatY->d_z_mies1
          alarm(hb_UTF8ToStr('Cały rok zamknięty!'))
          break
       endif
       sel(ar[AR_DBF],ar[AR_DBF]+"_ND")
       if i=NIL .or. i=0
          ad:=array(len(ar[AR_DOKUMENTY]))
          aeval(ar[AR_DOKUMENTY],{|x,i|ad[i]:=x[AD_SMB]+" "+x[AD_NAME]})
          i:=ascan(ar[AR_DOKUMENTY],{|ad|smb_doW=ad[AD_SMB]})
          if !aczojs(ad,"",@i,,"Wybierz dokument")
             break
          endif
       endif
       ad:=ar[AR_DOKUMENTY,i]
       if smb_doW<>ad[AD_SMB]
          seek ad[AD_SMB]+HB_UCHAR(0x0A0)
          skip -1
          r:=0
       else
          r:=recno()
       endif
       if smb_doW#ad[AD_SMB]
          DEFAULT mknk TO 4
          d_d:=A_MKNK(1)
       else

          x:=5
          for i:=5 to 1 step -1
            s:=SubStr(nr_dowodU,i,1)
            if isdigit(s)
               exit
            endif
            if s<>' '
               x:=i
            endif
          next i
          if i=0
             i:=x-1
             if i=0
                i:=4
             endif
          endif

          DEFAULT mknk TO i

            for x:=i-1 to 1 step -1
              s:=SubStr(nr_dowodU,x,1)
              if !isdigit(s)
                if s<>' '
                  exit
                endif
                seek ad[AD_SMB]+left(nr_dowodU,x)+HB_UCHAR(0x0A0)
                IF smb_doW#ad[AD_SMB]
                   exit
                ENDIF
                ++x
                loop
              endif
            next x
          goto r
          seek ad[AD_SMB]+left(nr_dowodU,x)+HB_UCHAR(0x0A0)
          skip -1

          if x>0 .and. str(1+val(SubStr(nr_dowodU,x+1)),i-x)='*'
             --x
          endif

          d_d:=left(nr_dowodU,x)+str(1+val(SubStr(nr_dowodU,x+1)),i-x)+SubStr(nr_dowodU,i+1)
       endif
       goto r
#ifndef A_DSPLIT
       d_d:=ad[AD_SMB]+d_d
#endif
       set order to ar[AR_DBF]+"_LP"
#ifndef A_NEW_LP
#define A_NEW_LP (dbgobottom(),str(1+val(lp),5))
#endif
       l_p:=A_NEW_LP
       if "K"$ad[AD_FLAGS]
         kh:=KONTRAHENT
       endif
       goto 0
    else
       sel(ar[AR_DBF],ar[AR_DBF]+"_LP")
       dbseek(l_p,.f.)
       da:=data
       if da <= DatY->d_z_mies1
          lappend:=.f.
#ifdef A_LAN
       elseif !lappend=.f. .and.(eof() .or. !RECLOCK(.F.,,.F.,,recno()))
          lappend:=.f.
#endif
       endif
       i:=ascan(ar[AR_DOKUMENTY],{|ad|smb_doW=ad[AD_SMB]})
       if i=0
          ad:=array(len(ar[AR_DOKUMENTY]))
          aeval(ar[AR_DOKUMENTY],{|x,i|ad[i]:=x[AD_SMB]+" "+x[AD_NAME]})
          if !aczojs(ad,"",@i,"Wybierz rodzaj dokumentu")
             break
          endif
       endif
       ad:=ar[AR_DOKUMENTY,i]
#ifdef A_DSPLIT
       d_d:=nr_dowodu
       mknk:=len(trim(d_d))
#else
       d_d:=dowod
       mknk:=len(trim(d_d))-2
#endif
       if "K"$ad[AD_FLAGS]
         kh:=KONTRAHENT
       endif
    endif
    l_k:=ident
    l:=len(ad[AD_POLA])
    i:=l+3
    if "P"$ad[AD_FLAGS]
       j:=10+2*A_WAL+A_KTL+A_IDENT+konta->(hb_fieldlen('nazwa'))
       if empty(ad[AD_POLA,l,AP_FIELD])
         k:=&('{'+ad[AD_POLA,l,AP_NAME]+',j}')
         j:=max(k[2],j)
         i:=if(empty(k[1]),MaxRow()-5-i,k[1])
         --l
       else
         aadd(ad[AD_POLA],asize({str(MaxRow()-5-i)+','+str(j)},AP_POZWN-1))
         i:=MaxRow()-5
       endif
    else
       fixpoz:={}
       o:=0
       aeval(ad[AD_POLA],{|x|o:=max(o,len(x))},1,l)
       o-=AP_POZWN-1
#define AP_STEP (AP_LTH-AP_POZWN+1)
       o:=INT(o/AP_STEP)
       j:=MAX(30+A_IDENT,14+A_WAL +o*(2*A_KTL+2))
       if empty(ad[AD_POLA,l,AP_FIELD])
          k:=&('{'+ad[AD_POLA,l,AP_NAME]+',j}')
          j:=max(k[2],j)
          i:=k[1]
          --l
       endif
    endif
    win:=window(i,j,_sbnorm)
    k:=1
    @ win[1],win[2]+2 SAY left(ad[AD_NAME],win[4]-9-col())
    @ win[1],win[4]-8 say rej+'/'+l_p
    setpos(win[1]+k,win[2])
    #ifdef A_DSPLIT
    SAYL ad[AD_SMB]
    getl d_d picture "@!K" send block:={|x,y,z|if(pcount()=0,d_d,d_d:=if((z:=val(x))<>0 .and. (y:=len(lTrim(sTr(z))))<5 .and. len(trim(x))=y ,A_MKNK(val(x)),x))}
    #else
    getl d_d picture "@!K" send block:={|x,y,z|if(pcount()=0,d_d,d_d:=if(empty(x),x,(x:=SubStr(x,3),d_d:=ad[AD_SMB]+if((z:=val(x))<>0 .and. (y:=len(lTrim(sTr(z))))<5 .and. len(trim(x))=y ,A_MKNK(val(x)),x))))}
    #endif
    sayl "Data:" get da valid {|g|da>DatY->d_z_mies1 .and. year(da)=year(DatY->d_z_rok+1) .or. (alarm("Data poza dopuszczalnym zakresem !"),g:undo(),.f.)}
    sayl "Link:" get l_k picture "@!K"
    if "K"$ad[AD_FLAGS]
       ++k
       kh:=KONTRAHENT
       sel("firmy","firm_num")
       dbseek(kh,.f.)
       select (ar[AR_DBF])
#ifdef A_PENS
       @ win[1]+k,win[2]+1 say A_PENS[1] get kh picture "@!KS"+str(win[4]-col(),2)
       setpos(win[1]+k,win[2]+A_NRLTH+2+len(A_PENS[1]))
#else
       @ win[1]+k,win[2]+1 say "Kontrahent" get kh picture "@!KS"+str(win[4]-col(),2)
       setpos(win[1]+k,win[2]+A_NRLTH+12)
#endif
       dispout(left(' '+firmy->nazwA,win[4]-col()))
       atail(getlist):preblock:={|g,v|firmy->(ordsetfocus("firm_num")),firmy->(dbseek(left(v,A_NRLTH),.f.)),g:varput(firmy->(numer_kol+' '+nazwA)),.t.}
       atail(GETLIST):POSTBLOCK:=if("P"$ad[AD_FLAGS],;
       {|g|(empty(kh).or.khval(g,,win)).and.(kh:=left(kh,A_NRLTH),.t.).and.(g:original=kh.or.khfix(rej+l_p,g:original,kh).and.(IF(status>=STAT_DEK,br:refreshall(),),.t.))},;
       {|g|(empty(kh).or.khval(g,,win)).and.(kh:=left(kh,A_NRLTH),.t.).and.(g:original=kh.or.(aeval(getlist,{|get,pos|IF(get:name="ad".and."K"$ad[AD_POLA,get:subscript[1],IF(get:name='adw',AP_POZWN,AP_POZMA)+(get:subscript[2]-1)*AP_STEP].and.!empty(get:varget()),(get:varput(left(get:varget(),A_KTL-A_NRLTH)+left(kh,A_NRLTH)),konval(get,ad[AD_POLA,get:subscript[1],IF(get:name='adw',AP_WNVAL,AP_MAVAL)+(get:subscript[2]-1)*AP_STEP],kh,getlist,pos).and.softup(ad,getlist,get),get:display()),)}),.t.))})
    endif
    af:=array(l)
*************
    if "P"$ad[AD_FLAGS]
***********
    for i:=1 to l

        txt:=alltrim(ad[AD_POLA,i,AP_FIELD])
        s:=FieldPos(txt)
        IF s>0
          af[i]:=fieldget(s)
          s:=dbstruct()[s]
        else
          s:=type(txt)
          if s='U'
            af[i]:=txt
            s:='C'
          else
            af[i]:=(&txt)
          endif
          n:=tran(af[i],)
          s:={,s,len(n),len(n)-rat('.',n)}
          if s[3]=s[4] .or. s[2]$'MCD'
            s[4]:=0
          endif
        endif
        IF ad[AD_POLA,i,AP_NAME]=','
          n:=win[4]-col()-len(ad[AD_POLA,i,AP_NAME])-1
        ELSEIF ad[AD_POLA,i,AP_NAME]='.'
          n:=win[4]-win[2]-len(ad[AD_POLA,i,AP_NAME])-1
        ELSE
          n:=win[4]-win[2]-17
        ENDIF
        txt:=ad[AD_POLA,i,AP_PICTURE]
        IF s[2]$'NFBYI+' .and. (empty(txt) .or. (txt='@' .and. !' '$txt))
           if empty(txt)
             txt:=''
           endif
           if empty(s[4])
              txt+=' '+repl("#",min(A_WAL,s[3]))
           else
              txt+=' '+repl("#",min(A_WAL,s[3])-s[4]-1)+"."+repl("#",s[4])
           endif
           if txt<>'@'
              txt:="@EK"+txt
           endif
        elseif (s[2]='M' .or. s[3]>n) .and. empty(txt)
           txt:="@S"+lTrim(sTr(n))
        endif

        IF ad[AD_POLA,i,AP_NAME]=','
          if len(tran(af[i],txt))>n
            ++k
            setpos(win[1]+k,win[2])
          endif
          SAYL SubStr(ad[AD_POLA,i,AP_NAME],2)+' '
          n:=win[4]-col()
          if (s[2]='M' .or. s[3]>n) .and. empty(ad[AD_POLA,i,AP_PICTURE])
             txt:="@S"+lTrim(sTr(n))
          endif
        ELSEIF ad[AD_POLA,i,AP_NAME]='.'
          ++k
          @ win[1]+k,win[2]+1 SAY SubStr(ad[AD_POLA,i,AP_NAME],2)+' '
          n:=win[4]-col()
        ELSE
          ++k
          @ win[1]+k,win[2]+1 say padl(ad[AD_POLA,i,AP_NAME],15)+' '
          n:=win[4]-col()
        ENDIF

        br:=_GET_( af[i],alltrim(ad[AD_POLA,i,AP_FIELD]),txt,ad[AD_POLA,i,AP_VALID],)
        if !empty(s) .and. s[2]="M"
           br:cargo:=.t.
#ifdef A_HBGET
           if len(af[i])<n
              af[i]:=pad(af[i],n)
           endif
#endif
        endif
        if valtype(ad[AD_POLA,i,AP_WHEN])='L'
          if !ad[AD_POLA,i,AP_WHEN]
           br:colorspec:=setcolor()
           br:preblock:={||.f.}
          endif
        else
           br:preblock:=ad[AD_POLA,i,AP_WHEN]
        endif
        aadd(getlist,br):display()
    next
    select MAIN
    br:=NIL
    pkedit(@br,ar,win,.f.,k-2,kh,l_p)
    select (ar[AR_DBF])
**********
    else
**********
    adw:=array(l,o)
    adm:=array(l,o)

    p:=o:=999
    m:=.f.
    key:=win[4]
    for i:=l to 1 step -1
           for n:=0 to len(ad[AD_POLA,i])-AP_POZMA step AP_STEP
             if !empty(ad[AD_POLA,i,n+AP_POZWN]) .and. !empty(ad[AD_POLA,i,n+AP_POZMA])
                n/=AP_STEP
                o:=min(o,n)
                p:=i+k-1
                exit
             endif
           next n
    next i
    for i:=1 to l

        if !m .and.  k>=p
           n:=win[2]+18+A_WAL + (2*A_KTL+2 ) * o
           if col()>n
              setpos(win[1]+k,win[4])
              ++k
           endif
        endif

        txt:=alltrim(ad[AD_POLA,i,AP_FIELD])
        s:=FieldPos(txt)
        IF s>0
          af[i]:=fieldget(s)
          s:=dbstruct()[s]
        else
          s:=type(txt)
          if s='U'
             s:='C'
             af[i]:=txt
          else
             af[i]:=(&txt)
          endif
          n:=tran(af[i],)
          s:={,s,len(n),len(n)-rat('.',n)}
          if s[3]=s[4] .or. s[2]$'MCD'
             s[4]:=0
          endif
        endif

        IF ad[AD_POLA,i,AP_NAME]=','
           n:=win[4]-col()-len(ad[AD_POLA,i,AP_NAME])-1
        ELSEIF ad[AD_POLA,i,AP_NAME]='.'
           n:=win[4]-win[2]-len(ad[AD_POLA,i,AP_NAME])-1
        ELSE
           n:=win[4]-win[2]-17
        ENDIF

        txt:=ad[AD_POLA,i,AP_PICTURE]

        IF s[2]$'NFBYI+' .and. (empty(txt) .or. (txt='@' .and. !' '$txt))
           if empty(txt)
             txt:=''
           endif
           if empty(s[4])
              txt+=' '+repl("#",min(A_WAL,s[3]))
           else
              txt+=' '+repl("#",min(A_WAL,s[3])-s[4]-1)+"."+repl("#",s[4])
           endif
           if txt<>'@'
              txt:="@EK"+txt
           endif
        elseif (s[2]='M' .or. s[3]>n) .and. empty(txt)
           txt:="@S"+lTrim(sTr(n))
        endif

        if !m .and.  k>=p
                p:=win[2]+18+A_WAL + (2*A_KTL+2 ) * o
                m:=win[1]+k
                IF ad[AD_POLA,i,AP_NAME]=','
                  IF col()+len(ad[AD_POLA,i,AP_NAME])+len(tran(af[i],txt))+1>=p
                    ++k
                    key:=win[2]
                    n+=col()-win[2]
                    if (s[2]='M' .or. s[3]>n) .and. empty(ad[AD_POLA,i,AP_PICTURE])
                      txt:="@S"+lTrim(sTr(n))
                    endif
                  elseif key<=p
                    key:=col()
                    --m
                  else
                    key:=win[2]
                  endif
                ENDIF
#ifdef __PLATFORM__DOS
                x:=replicate("═",A_KTL-4)+"═Wn═┬═Ma═"+replicate("═",A_KTL-4)
#else                
                x:=replicate("═",A_KTL-4)+"═Wn═╤═Ma═"+replicate("═",A_KTL-4)
#endif                
                For p:=p To win[4]-1-2*A_KTL Step 2*A_KTL+2
                  @ m,p BOX hb_utf8Left(x,win[4]-p) UNICODE
                Next p
                m:=.t.
                setpos(win[1]+k,key)
        endif

        IF ad[AD_POLA,i,AP_NAME]=','
          if len(tran(af[i],txt))>n
            ++k
            key:=col()
            n+=col()-win[2]
            setpos(win[1]+k,win[2])
          elseif !m
             --p
          endif
          SAYL SubStr(ad[AD_POLA,i,AP_NAME],2)+' '
          if (s[2]='M' .or. s[3]>n) .and. empty(ad[AD_POLA,i,AP_PICTURE])
             txt:="@S"+lTrim(sTr(n))
          endif
        ELSEIF ad[AD_POLA,i,AP_NAME]='.'
          key:=col()
          ++k
          @ win[1]+k,win[2]+1 say SubStr(ad[AD_POLA,i,AP_NAME],2)+' '
        ELSE
          key:=col()
          ++k
          @ win[1]+k,win[2]+1 say padl(ad[AD_POLA,i,AP_NAME],15)+' '
        ENDIF

        br:=_GET_( af[i],alltrim(ad[AD_POLA,i,AP_FIELD]),txt,ad[AD_POLA,i,AP_VALID],)
        if s[2]="M"
           br:cargo:=.t.
#ifdef A_HBGET
           if len(af[i])<n
              af[i]:=pad(af[i],n)
           endif
#endif
        endif
        if valtype(ad[AD_POLA,i,AP_WHEN])="L"
          if !ad[AD_POLA,i,AP_WHEN]
           br:colorspec:=setcolor()
           br:preblock:={||.f.}
          endif
        else
           br:preblock:=ad[AD_POLA,i,AP_WHEN]
        endif
        aadd(getlist,br):display()

        if !m
           loop
        endif

        o:=0
      for p:=0 to len(ad[AD_POLA,i])-AP_POZMA step AP_STEP
        ++o
        if empty(ad[AD_POLA,i,p+AP_POZWN]) .or. empty(ad[AD_POLA,i,p+AP_POZMA])
           loop
        endif
        setpos(win[1]+k,win[2]+18+A_WAL+(o-1)*2*(A_KTL+1))
        br:=_GET_( adw[i,o],"adw,"+alltrim(ad[AD_POLA,i,AP_FIELD])+","+str(o,1),"@!K",,)
        s:=SubStr(ad[AD_POLA,i,AP_POZWN+p],2) //chroni od litery pozycji na początku
        if "E" $ s
           br:postblock:={|g,va,list,pos|!g:changed.and.af[g:subscript[1]]=0.or.(empty(g:varget()).or.konval(g,ad[AD_POLA,g:subscript[1],AP_WNVAL+(g:subscript[2]-1)*AP_STEP],kh,getlist,pos)).and.softup(ad,getlist,g)}
        else
           br:postblock:={|g,va,list,pos|konval(g,ad[AD_POLA,g:subscript[1],AP_WNVAL+(g:subscript[2]-1)*AP_STEP],kh,getlist,pos).and.softup(ad,getlist,g)}
        endif
#ifdef A_LPNUM
        if main->(dbseek(rej+l_p+str(val(ad[AD_POLA,i,AP_POZWN+p]),A_LPNUM),.f.)) .and. status>=STAT_DEK
#else
        if main->(dbseek(rej+l_p+left(ad[AD_POLA,i,AP_POZWN+p],1),.f.)) .AND. status>=STAT_DEK
#endif
           adw[i,o]:=main->konto
           if main->czyma=.t.
              hb_DispOutAtBox(row(),col(),hb_UTF8ToStrBox('↔'))
           endif
        else
           adw[i,o]:=pad(ad[AD_POLA,i,AP_KTWN+p],A_KTL)
#ifdef A_LPNUM
           x:=val(ad[AD_POLA,i,AP_POZWN+p])
#else
           x:=asc(ad[AD_POLA,i,AP_POZWN+p])
#endif
           if x>0  //PAMIĘĆ POZYCJI STAŁYCH
            if x>len(fixpoz)
              asize(fixpoz,x)
            endif
            if "F" $ s .and. !empty(fixpoz[x])
             adw[i,o]:=fixpoz[x]
            else
             if "K" $ s .and. kh#right(adw[i,o],A_NRLTH)
               if konta->(dbseek(adw[i,o])) .and. konta->syntet .and. konta->khflag
                 konval(br,,kh)
               endif
               adw[i,o]:=left(adw[i,o],A_KTL-A_NRLTH)+left(kh,A_NRLTH)
               chg:=.t.
             endif
             fixpoz[x]:=adw[i,o]
            endif
           endif
        endif
        if "F" $ s
           br:preblock:={||lastkey()=9} //.f.
           if empty(ad[AD_POLA,i,AP_KTWN+p]) .or. adw[i,o]=ad[AD_POLA,i,AP_KTWN+p]
              br:colorspec:=setcolor()
           else
              br:colorspec:=_sramka
           endif
        endif
        br:display()
        aadd(getlist,br)
        ***********
        @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX "│" UNICODE
        //setpos(win[1]+k,win[2]+18+A_WAL+((o-1)*2+1)*(A_KTL+1))
        //setpos(win[1]+k,win[4]-A_KTL-1)
        s:="@!K"
        IF win[4]-col()<A_KTL
           s+="S"+lTrim(sTr(win[4]-col()))
        ENDIF
        br:=_GET_( adm[i,o],"adm,"+alltrim(ad[AD_POLA,i,AP_FIELD])+","+str(o,1),s,,)
        s:=SubStr(ad[AD_POLA,i,AP_POZMA+p],2)
        if "E" $ s
           br:postblock:={|g,va,list,pos|!g:changed.and.af[g:subscript[1]]=0.or.(empty(g:varget()).or.konval(g,ad[AD_POLA,g:subscript[1],AP_MAVAL+(g:subscript[2]-1)*AP_STEP],kh,getlist,pos)).and.softup(ad,getlist,g)}
        else
           br:postblock:={|g,va,list,pos|konval(g,ad[AD_POLA,g:subscript[1],AP_MAVAL+(g:subscript[2]-1)*AP_STEP],kh,getlist,pos).and.softup(ad,getlist,g)}
        endif
        #ifdef A_LPNUM
        if main->(dbseek(rej+l_p+str(val(ad[AD_POLA,i,AP_POZMA+p]),A_LPNUM),.f.)) .AND. status>=STAT_DEK
        #else
        if main->(dbseek(rej+l_p+left(ad[AD_POLA,i,AP_POZMA+p],1),.f.)) .AND. status>=STAT_DEK
        #endif
           adm[i,o]:=main->konto
           if main->czyma=.f.
              @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX '↔' UNICODE
           endif
        else
           adm[i,o]:=pad(ad[AD_POLA,i,AP_KTMA+p],A_KTL)
#ifdef A_LPNUM
           x:=val(ad[AD_POLA,i,AP_POZMA+p])
#else
           x:=asc(ad[AD_POLA,i,AP_POZMA+p])
#endif
           if x>0
            if x>len(fixpoz)
              asize(fixpoz,x)
            endif
            if "F" $ s .and. !empty(fixpoz[x])
             adm[i,o]:=fixpoz[x]
            else
             if "K" $ s .and. kh#right(adm[i,o],A_NRLTH)
               if konta->(dbseek(adm[i,o])) .and. konta->syntet .and. konta->khflag
                  konval(br,,kh)
               endif
               adm[i,o]:=left(adm[i,o],A_KTL-A_NRLTH)+left(kh,A_NRLTH)
               chg:=.t.
             endif
             fixpoz[x]:=adm[i,o]
            endif
           endif
        endif
        if "F" $ s
           br:preblock:={||lastkey()=9} //.f.
           if empty(ad[AD_POLA,i,AP_KTMA+p]) .or. adm[i,o]=ad[AD_POLA,i,AP_KTMA+p]
              br:colorspec:=setcolor()
           else
              br:colorspec:=_sramka
           endif
        endif
        br:display()
        aadd(getlist,br)
      next p
    next
*********
    endif
*********

    __setproc(procname(0))

buf:=ad[AD_PREPROC]
if !empty(buf) .and. valtype(buf)$"MC"
  if len(buf)<=12
     x:=trim(buf)
     i:=ascan(ppd,{|a|a[1]==x})
     if i<>0
        buf:=ppd[i,2]
     else
        i:=x
        if !"."$i
          i+=".ppd"
        endif
        i:=findfile(i)
        IF !empty(i)
           aadd(ppd,{x,buf:=getlines(memoread(i),.t.)})
        endif
     endif
     x:=NIL
  else
     buf:=getlines(buf,.t.)
  endif
  ad[AD_PREPROC]:=buf
endif


#define EVLINE self:=evline(buf,j++,@x);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
       ;__mvPrivate(self[3]);
     ;END;
     ;x:=&(self[1]);
   ;END

    linecount:=len(buf)
    j:=1
    x:=NIL
    do while j>0 .and. j<=linecount

       EVLINE

    enddo
    s:=NIL // nie wymuszam edycji
    if valtype(x)<>'L'
       x:=NIL
    endif
    do while .t.

    buf:=ad[AD_PREPROC] //po co to? żeby sie odwoływać do procedur

    if s=NIL .and. ( x=.f. .or. lappend<>NIL .or. status>=STAT_NOWRT)
       if status=STAT_NOWRT
          IF data > DatY->d_z_mies1
             @ win[3],win[4]-40 Say 'ZAMKNIĘTY - otwarcie [Ctrl]+[Enter]' UNICODE
          ELSE
             @ win[3],win[4]-40 Say 'ZAMKNIĘTY' UNICODE
          ENDIF
       elseif status>=STAT_ZATW
          IF iS_spec .and. data > DatY->d_z_mies1
             @ win[3],win[4]-40 Say 'ZATWIERDZONY - otwarcie [Ctrl]+[Enter]'
          ELSE
             @ win[3],win[4]-40 Say 'ZATWIERDZONY'
          ENDIF
       endif
       if !(lappend=.t.)
       if "P"$ad[AD_FLAGS]
         key:=MAIN->(pkbrow(br))
       else
         i:={}
         aeval(getlist,{|g,h|if(g:cargo<>NIL,(h:=getnew(g:row,g:col,g:block,g:name,g:picture,g:colorspec),h:subscript:=g:subscript,h:cargo:=g:cargo,aadd(i,h)),)})
         key:=inkey(0)
       endif
       if !(x=.f.) .and. (key=K_CTRL_RET .or. key=K_ALT_ENTER) .and. (status=STAT_NOWRT .or. iS_spec)
          IF data > DatY->d_z_mies1
          colorselect(3)
          @ win[3],win[4]-40 BOX replicate('─',40) UNICODE
#ifndef hAslo_spec
          IF status>STAT_NOWRT .and. iS_spec
            setpos(win[3],win[4]-40)
            haslo_spec()
            @ win[3],win[4]-40 BOX replicate('─',40) UNICODE
          ENDIF
#endif
          colorselect(0)
          select (ar[AR_DBF])
          status:=STAT_DEK
          CHG:=.T.
          endif
          loop
       endif
       if !empty(i) .and. key<>K_ESC
         readmodal(i) //tylko oglądanie
         key:=K_ESC
       endif
       i:=NIL
       exit
       endif
    endif
    if status<STAT_NOWRT
       select (ar[AR_DBF])

       key:=SETKEY(K_F9,{|k,get,gl,pos|k:=setkey(K_F9,nil),rejf9(ar,ad,@f9dok,getlist),setkey(K_F9,k)})
****************

       readmodal(getlist,@getpos)
****************
       SETKEY(K_F9,key)

       key:=ReadkeY()
       if s#NIL .or. key#K_ESC //.and. updated()
          select "main"
          set order to "main_lp"
          select (ar[AR_DBF])
          if lappend=.t.
             if dbseek(l_p,.f.)
                Alarm('Numer '+l_p+hb_UTF8ToStr(' już istnieje; zmieniam!'))
                l_p:=A_NEW_LP
                getpos:=1
                loop
             endif
             lappend:=nil
             append blank
             aeval(dbstruct(),{|x,i|fieldput(i,fieldget(i))}) //puste pola nielubiane?
             lp:=l_p
             data:=da
          endif
          chg:=.t.
          if status<STAT_WRT
             status:=STAT_WRT
          endif

#ifdef A_DSPLIT
          smb_dow:=ad[AD_SMB]
          nr_dowodu:=d_d
#else
          dowod:=d_d
#endif
          ident:=l_k // data na końcu
          if "K"$ad[AD_FLAGS]
             KONTRAHENT:=kh
          endif
*****************************
          if "P"$ad[AD_FLAGS]
*****************************
             for i:=1 to l
                 txt:=alltrim(ad[AD_POLA,i,AP_FIELD])
                 s:=TYPE(txt)
                 if s='M'
                    af[i]:=trim(af[i])
                 endif
                 if s='U'
                 else
                   s:=fieldpos(txt)
                   if s=0
                     s:=memvarblock(txt)
                     if s<>NIL
                       eval(s,af[i])
                     end
                   else
                     fieldput(s,af[i])
                   endif
                 endif
             next


             select main
             set order to "main_lp"
                if data#da
                   i:=recno()
#ifdef A_LAN
  #define D_LAN reclock(.F.,,,,),
#else
  #define D_LAN
#endif
                   exec {||D_LAN data:=da} while rejestr+lp=rej+l_p
                   goto i
                   UNLOCK
                endif
             select (ar[AR_DBF])
             if /*key#K_ESC .and.*/ key#K_PGUP .and. key#K_CTRL_L
                if status<STAT_BADDEK
                  status:=STAT_BADDEK
                endif
                chg:=main->(pkedit(br,ar,win,.t.,k-2,kh,l_p,ad[AD_POLA,l+1],f9dok)) .or. chg
                key:=ReadkeY()
             endif
*******************
          else
*******************
             s:=NIL //nie wymusza wejścia po pgup
#ifndef A_AUTOFIX
             IF STATUS<=STAT_BADDEK
#endif
              select main
#ifdef A_LAN
              dbseek(rej+l_p,.f.)
              EXEC {||reclock(.f.,,,,recno()),kklock(konto)} FOR kwota#0 while rejestr+lp==rej+l_p
#endif
              dbseek(rej+l_p,.f.)
              EXEC {||kk(konto,-kwota,czyma),kwota:=0,konto:=''} FOR kwota#0 while rejestr+lp==rej+l_p
              UNLOCK
              UNLOCK IN KONTA
              select (ar[AR_DBF])
              status:=STAT_BADDEK
#ifndef A_AUTOFIX
             ENDIF
#endif
             i:=0
             while ++i <=l
                 txt:=alltrim(ad[AD_POLA,i,AP_FIELD])
                 r:=type(txt)
                 if r='U' .or. (!"Z"$ad[AD_FLAGS] .and. empty(af[i]) .and. empty(&txt))
                    loop
                 endif
                 o:=0
                 p:=-AP_STEP
                 while (p+=AP_STEP) <= len(ad[AD_POLA,i])-AP_POZWN
                 //for p:=0 to len(ad[AD_POLA,i])-AP_POZWN step AP_STEP

                   ++o
              if empty(ad[AD_POLA,i,AP_POZWN+p]) .or. empty(ad[AD_POLA,i,AP_POZMA+p])
                 loop
              elseif ("E" $ ad[AD_POLA,i,AP_POZWN+p] .and. empty(adw[i,o]) .or. "E" $ ad[AD_POLA,i,AP_POZMA+p] .and. empty(adm[i,o]))
                     m:=.f. //puste
              else
                     m:=.t. //pełne
                     if af[i]<>0 .and. !konta->((s:=1,dbseek(adw[i,o],.f.).and.!syntet) .and. (s:=2,dbseek(adm[i,o],.f.).and.!syntet))
                        if "K" $ ad[AD_FLAGS] .and. empty(kh) .and. konta->(syntet .and. khflag)
                           getpos:=findget(getlist,'kh')
                           alarm("Brak kontrahenta",,0,3)
                        else
                           alarm("Brak w planie kont:"+if(s=1,adw[i,o],adm[i,o]),,0,3)
                           getpos:=(o-1)*2+s+ascan(getlist,{|g|g:subscript#nil .and. g:subscript[1]=i})
                           IF "F" $ SubStr(ad[AD_POLA,i,IF(s=1,AP_POZWN,AP_POZMA)+p],2)
                              i:=0
                              While ++i <=l
                                  o:=0
                                  for p:=0 to len(ad[AD_POLA,i])-AP_POZWN step AP_STEP
                                      ++o
                                      if empty(ad[AD_POLA,i,AP_POZWN+p])
                                         loop
                                      endif
                                      IF !"F" $ SubStr(ad[AD_POLA,i,IF(s=1,AP_POZWN,AP_POZMA)+p],2)
                                         getpos:=(o-1)*2+s+ascan(getlist,{|g|g:subscript#nil .and. g:subscript[1]=i})
                                         i:=l
                                         exit
                                      ENDIF
                                  next p
                              end
                           ENDIF
                        endif
                        key:=K_PGUP
                        i:=l+1
                        status:=STAT_BADDEK
                        exit
                     endif
              endif
              select main
#ifdef A_LAN
              begin sequence
              s:=0
#ifdef A_LPNUM
              if dbseek(rej+l_p+str(val(ad[AD_POLA,i,AP_POZWN+p]),A_LPNUM),.f.)
#else
              if dbseek(rej+l_p+left(ad[AD_POLA,i,AP_POZWN+p],1),.f.)
#endif
                 lock recno()
                 kklock(konto)
              elseif m
                 append blank
                 lock recno()
                 rejestr:=rej
                 lp:=l_p
#ifdef A_LPNUM
                 pozycja:=str(val(ad[AD_POLA,i,AP_POZWN+p]),A_LPNUM)
                 link:=str(val(ad[AD_POLA,i,AP_POZMA+p]),A_LPNUM)
#else
                 pozycja:=ad[AD_POLA,i,AP_POZWN+p]
                 link:=ad[AD_POLA,i,AP_POZMA+p]
#endif
              endif
              br:=recno()
#ifdef A_LPNUM
              if dbseek(rej+l_p+str(val(ad[AD_POLA,i,AP_POZMA+p]),A_LPNUM),.f.)
#else
              if dbseek(rej+l_p+left(ad[AD_POLA,i,AP_POZMA+p],1),.f.)
#endif
                 lock recno()
                 kklock(konto)
              elseif m
                 append blank
                 lock recno()
                 lock br
                 rejestr:=rej
                 lp:=l_p
#ifdef A_LPNUM
                 pozycja:=str(val(ad[AD_POLA,i,AP_POZMA+p]),A_LPNUM)
                 link:=str(val(ad[AD_POLA,i,AP_POZWN+p]),A_LPNUM)
#else
                 pozycja:=ad[AD_POLA,i,AP_POZMA+p]
                 link:=ad[AD_POLA,i,AP_POZWN+p]
#endif
                 czyma:=.t.
              endif
              s:=1
              kklock(adw[i,o])
              s:=2
              kklock(adm[i,o])
              s:=NIL
              recover
                 getpos:=(o-1)*2+s+ascan(getlist,{|g|g:subscript#NIL .and. g:subscript[1]=i})
                 key:=K_PGUP
                 loop
              end sequence
#endif
              r:=ad[AD_POLA,i,AP_POZWN+p]
              s:=ad[AD_POLA,i,AP_POZMA+p]
#ifdef A_LPNUM
              if dbseek(rej+l_p+str(val(r),A_LPNUM),.f.)
#else
              if dbseek(rej+l_p+left(r,1),.f.)
#endif
                 kk(konto,-kwota,czyma)
#ifndef A_LAN
              elseif m
                 append blank
                 rejestr:=rej
                 lp:=l_p
#ifdef A_LPNUM
                 pozycja:=str(val(r),A_LPNUM)
                 link:=str(val(s),A_LPNUM)
#else
                 pozycja:=r
                 link:=s
#endif
                 //czyma:=.f.
#endif
              endif

              IF m
#ifndef A_AUTOFIX
              SELECT (ar[AR_DBF])
              if _FIELD->STATUS>STAT_BADDEK .and. !MAIN->(EOF())
                 MAIN->kwota-=if(MAIN->czyma,-1,1)*(&txt)   //FIELDGET(FIELDPOS(ad[AD_POLA,i,AP_FIELD]))
              ENDIF
              SELECT MAIN
#endif
              IF XOR("-"$r,"X"$r .and. af[i]<0)
                if czyma<>(czyma:=.t.)
                   kwota:=-kwota
                   @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX '↔' UNICODE
                endif
                kwota-=af[i]
              ELSE
                if czyma<>(czyma:=.f.)
                   kwota:=-kwota
                   @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX '│' UNICODE
                endif
                kwota+=af[i]
              ENDIF
              konto:=adw[i,o]
              data:=da
              if !Empty(ad[AD_POLA,i,AP_IDWN+p])
                ident:=(ar[AR_DBF])->(eval(ad[AD_POLA,i,AP_IDWN+p],@l_k,i,o))
              else
                ident:=l_k
              endif

              kk(konto,kwota,czyma)
              ELSEif !eof() .and. kwota=0
                konto:=''
              ENDIF
              ***************
#ifdef A_LPNUM
              if dbseek(rej+l_p+str(val(s),A_LPNUM),.f.)
#else
              if dbseek(rej+l_p+left(s,1),.f.)
#endif
                 kk(konto,-kwota,czyma)
#ifndef A_LAN
              elseif m
                 append blank
                 rejestr:=rej
                 lp:=l_p
#ifdef A_LPNUM
                 pozycja:=str(val(s),A_LPNUM)
                 link:=str(val(r),A_LPNUM)
#else
                 pozycja:=s
                 link:=r
#endif
                 czyma := .t.
#endif
              endif

              IF m
#ifndef A_AUTOFIX
              SELECT (ar[AR_DBF])
              if _FIELD->STATUS>STAT_BADDEK .and. !MAIN->(EOF())
                 MAIN->kwota-=if(MAIN->czyma,1,-1)*(&txt) //FIELDGET(FIELDPOS(ad[AD_POLA,i,AP_FIELD]))
              ENDIF
              SELECT MAIN
#endif
              IF XOR("-"$s,"X"$s .and. af[i]<0)
                if czyma<>(czyma:=.f.)
                   kwota:=-kwota
                   @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX '↔' UNICODE
                endif
                kwota-=af[i]
              ELSE
                if czyma<>(czyma:=.t.)
                   kwota:=-kwota
                   @ win[1]+k,win[2]+17+A_WAL+((o-1)*2+1)*(A_KTL+1) BOX '│' UNICODE
                endif
                kwota+=af[i]
              ENDIF
              konto:=adm[i,o]
              data:=da
              if !Empty(ad[AD_POLA,i,AP_IDMA+p])
                ident:=(ar[AR_DBF])->(eval(ad[AD_POLA,i,AP_IDMA+p],@l_k,i,o))
              else
                ident:=l_k
              endif

              kk(konto,kwota,czyma)
              ELSEif !eof() .and. kwota=0
                konto:=''
              ENDIF
              SELECT (ar[AR_DBF])
              end  //next p
              if i<=l
              s:=fieldpos(txt)
              if s=0
                 s:=memvarblock(txt)
                 if s<>NIL
                    eval(s,af[i])
                 end
              else
                 if TYPE(txt)='M'
                    af[i]:=trim(af[i])
                 endif
                 fieldput(s,af[i])
              endif
              endif
             end // next i
             if empty(d_d)
                select main
                seek rej+l_p
                exec {||D_LAN dbdelete()} rest while rejestr+lp=rej+l_p for kwota=0
                select (ar[AR_DBF])
             endif
             unlock in konta
             unlock in main
*************************
          endif
*************************
          data:=da
          if status<STAT_DEK
             status:=STAT_DEK
          endif
          if key=K_PGUP
            commit
            loop
          endif

          buf:=ad[AD_VALID]
          if !empty(buf) .and. valtype(buf)$"MC"
            if len(buf)<=12
               x:=trim(buf)
               i:=ascan(ppd,{|a|a[1]==x})
               if i<>0
                  buf:=ppd[i,2]
               else
                  i:=x
                  if !"."$i
                    i+=".ppd"
                  endif
                  i:=findfile(i)
                  IF !empty(i)
                     aadd(ppd,{x,buf:=getlines(memoread(i),.t.)})
                  endif
               endif
            else
               buf:=getlines(buf,.t.)
            endif
            ad[AD_VALID]:=buf
          endif
          x:=key
          linecount:=len(buf)
          j:=1
          do while j>0 .and. j<=linecount

             EVLINE

          enddo
          if valtype(x)='L' .and. !x
             commit
             s:=0 //wymusza edycje
             loop
          endif
#ifdef A_DATA_KS
          A_DATA_KS
#endif
          if empty(d_d) .and. !main->(dbseek(rej+l_p)) .and. tak(hb_UTF8ToStr("Czy kasować"),win[3],win[2]+1,.f.,.f.,setcolor(),win)
             delete
             chg:=.t.
          elseif ad[AD_KOPI]>0 .and. !empty(ad[AD_DPROC])
             if tak(hb_UTF8ToStr("Czy Drukować"),win[3],win[2]+1,lappend,.f.,setcolor(),win)
                commit
                dokdruk(rej,l_p,ad[AD_KOPI])
             endif
          endif
       endif //!esc
    endif //!append
    exit
    enddo
    recover
      unlock all
      window(win)
      pop_stat(stat)
      return chg
    end sequence
    window(win)
    unlock
    pop_norec(stat)
    if alias()<>ar[AR_DBF]
       pop_stat(stat)
    endif
return chg
****************************
stat func softup(ad,getlist,g)
local n,appoz
n:=left(g:name,3)
appoz:=if(n='adw',AP_POZWN,AP_POZMA)-AP_STEP
#ifdef A_LPNUM
aeval(getlist,{|get|if(get:name=n.and.VAL(ad[AD_POLA,get:subscript[1],appoz+get:subscript[2]*AP_STEP])=VAL(ad[AD_POLA,g:subscript[1],appoz+g:subscript[2]*AP_STEP]),(get:varput(g:varget()),get:display()),)})
#else
aeval(getlist,{|get|if(get:name=n.and.ad[AD_POLA,get:subscript[1],appoz+get:subscript[2]*AP_STEP]=ad[AD_POLA,g:subscript[1],appoz+g:subscript[2]*AP_STEP],(get:varput(g:varget()),get:display()),)})
#endif
return .t.
****************************
proc kk(ko,kw,ma)
local s:=select(),f,l:=A_KTL
     if kw=0
        return
     endif
     select konta
     dbseek(ko,.f.)
     do while l>0
        if ma
           sald_bie_m+=kw
        else
           sald_bie_w+=kw
        endif
        l:=len(trim(konto))
        do while --l>0 .and. !(dbseek(pad(left(ko,l),A_KTL),.f.).and.syntet);enddo
     enddo
     select (s)
return
#ifdef A_LAN
****************************
proc kklock(ko)
local s:=select(),f,l:=A_KTL
     select konta
     dbseek(ko,.f.)
     do while l>0 .and. !EOF()
        lock recno()
        l:=len(trim(konto))
        do while --l>0 .and. !(dbseek(pad(left(ko,l),A_KTL),.f.).and.syntet);enddo
     enddo
     select (s)

return
#endif
**************************
