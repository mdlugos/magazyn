#include "ar.ch"
#ifndef A_LAN
#define DatY MEMVAR
#endif
#include "inkey.ch"


#define EVLINE self:=evline(buf,j++,@x);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
       ;__mvPrivate(self[3]);
     ;END;
     ;x:=&(self[1]);
   ;END
*********************
memvar rejestry
memvar defa,buf
memvar a,b,c,d,e,f,g,h,i,j,strona,ar,ad,oprn
#ifdef A_DSPLIT
field smb_dow
#else
#define smb_doW dowod
field dowod
#endif

proc dokdruk(rej,l_p,kop)
memvar self
local linecount,mes,stat,rr,mr,x
private ar,ad,buf

  IF kop=NIL
    kop=1
    tone(262,2)
    mes:=message(hb_UTF8ToStr("Wydruk dokumentu;Ilość kopii:"))
    x:=getnew(mes[1]+2,mes[2]+15,{|x|if(x=NIL,str(kop,1),kop:=val(x))},'kop','#')
    GETREADER(x)
    if x:exitstate=K_ESC .or. kop=0
      message(mes)
      return
    endif
    @ mes[1]+2,mes[2]+2 SAY "Proszę czekać" UNICODE
  ELSE
    mes:=message(hb_UTF8ToStr("Proszę czekać;TRWA WYDRUK."))
  ENDIF
begin sequence
stat:=push_stat()
ad:=ascan(rejestry,{|r|r[AR_SMB]=rej})
ar:=rejestry[ad]
sel("konta","kont_num")
sel("firmy","firm_num")
sel("main","main_lp")
sel(ar[AR_DBF],ar[AR_DBF]+"_LP")
rel(ar[AR_REL])
dbseek(l_p,.f.)
main->(dbseek(rej+l_p,.f.))
ad:=ascan(ar[AR_DOKUMENTY],{|x|smb_doW=x[AD_SMB]})
ad:=ar[AR_DOKUMENTY,ad]

private a,b,c,d,e,f,g,h,i,j,self,strona:=0
buf:=ad[AD_DPROC]
if valtype(buf)$"MC"
  if len(buf)<=12
     buf:=x:=trim(buf)
     if !"."$x
        x+=".ppr"
     endif
     x:=findfile(x)
     IF !empty(x)
        buf:=memoread(x)
     endif
     x:=NIL
  endif
  ad[AD_DPROC]:=buf:=getlines(buf,.t.)
endif
if empty(buf)
   break
endif
set console off
print()

linecount:=len(buf)
mr:=main->(recno())
rr:=recno()
for i:=kop to 1 step -1
    j:=1
    do while j>0 .and. j<=linecount
       message(10)
       EVLINE
    end
    if strona>0
       specout(chr(13))
       setprc(0,0)
    endif
    select main
    goto mr
    select (ar[AR_DBF])
    goto rr
next i
end
#ifdef A_PRINT
     if set(_SET_PRINTER,.f.)
        x:=set(_SET_PRINTFILE,'')
        if File(x)
          A_PRINT(x)
        endif
     endif
#endif
set print off
set console on
set printer to
#ifdef A_WIN_PRN
if valtype(oprn)='O'
   oprn:=NIL
endif
#endif
message(mes)
pop_stat(stat)
return
*******************
#undef EVLINE

#define EVLINE(buf,lbl,lc) while j>0 .and. j<=lc .and. (j=1 .or. !valtype(buf[j-1])$"MC" .or. buf[j-1]<>lbl);
   ;self:=evline(buf,j++,@x);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
       ;__mvPrivate(self[3]);
     ;END;
     ;x:=&(self[1]);
   ;END;
;END
*************************
func obroty(w,m,da,k,l)
DEFAULT k TO MAIN->(EvAlDb('{|DATA,REJESTR,LP,IDENT|'+IndexkeY(0)+'}'))
DEFAULT l TO KONTA->(EvaldB(k,da,'','',''))
IF da>DatY->d_z_rok
  main->(dbeval({||if(_FIELD->czyma,m+=_FIELD->kwota,w+=_FIELD->kwota)},,{||EvaldB(k,_FIELD->data,_FIELD->rejestr,_FIELD->lp,_FIELD->ident)<=l}))
  w:=ROUND(w,A_ZAOKR);m:=ROUND(m,A_ZAOKR)
ENDIF
return ! main->( eof() .or. EvaldB(k,da,'','','')>l )
*************************
func saldo(w,m,da,k,l,f) //na koniec dnia da    f -flaga zmian indeksu w main
local d
FIELD baza
DEFAULT da TO DatY->d_z_rok
if f=NIL .or. f
      if konta->syntet
         w:=len(m:=trim(konta->konto))
#ifndef A_STL
#define A_STL 3
#endif

         if w=A_STL
            main->(ordsetfocus('MAIN_KS'))
         else
            main->(ordsetfocus("X"+m))
            if main->(ordsetfocus())<>"X"+m
              d:="left(konto,"+str(w,1)+")+SubStr(dtos(data),5)"
              main->(ordsetfocus(if(w<=A_STL,'MAIN_KS','MAIN_KT')))
              main->(dbseek(m))
              main->(ordCondSet("konto='"+m+"'",,,{||_FIELD->konto=m},,,RECNO(),,,,))
              main->(ordCreate("."+HB_ps()+"main_tmp","X"+m,d,&("{||"+d+"}")))
#ifndef A_CDX
              d:=select()
              sel("INDEKS")
              locate FOR baza="MAIN"
              exec main->(ordlistadd(indeks->nazwa)) while baza="MAIN"
              select MAIN
              ordsetfocus(1)
              dbselectar(d)
#endif
            endif
         endif
      elseif !empty(SubStr(konta->konto,A_STL+1))
         main->(ordsetfocus('MAIN_KT'))
      endif

endif // f

if da<DatY->d_z_mies1 .or. DatY->d_z_mies1 <= DatY->d_z_mies2
   if da<DatY->d_z_mies2 .or. DatY->d_z_mies2<=DatY->d_z_rok .or. DatY->d_z_mies1<DatY->d_z_mies2
      d:=DatY->d_z_rok+1
      w:=konta->sald_rok_w
      m:=konta->sald_rok_m
   else
      d:=DatY->d_z_mies2+1
      w:=konta->sald_poc_w
      m:=konta->sald_poc_m
   endif
else
   d:=DatY->d_z_mies1+1
   w:=konta->sald_kon_w
   m:=konta->sald_kon_m
endif

k:=main->(EvAlDb('{|DATA,REJESTR,LP,IDENT|'+IndexkeY(0)+'}'))
l:=konta->(EvaldB(k,da,'','',''))
main->(dbseek(konta->(EvaldB(k,d,'','',''))))
return obroty(@w,@m,da,k,l)
/************************
IF main->( eof() .or. eval(k,da,'','','')>l )
   return .f. //koniec tego konta
elseIF da>=d
   return obroty(@w,@m,da,k,l)
ENDIF
return .t.
*************************/
proc zes(n)
local r,x
MEMVAR it_zesmnu,strona,landscape
public it_zesmnu
private strona:=0

   setpos(6,maxcol()/2+1)
//   SETKEY(4,{||KIBORD(CHR(27)+CHR(24)+CHR(13))})
//   SETKEY(19,{||KIBORD(CHR(27)+CHR(5)+CHR(13))})

   if valtype(it_zesmnu)='L'
      it_zesmnu:=NIL
   endif
   if n<>nil
     it_zesmnu:=n
   endif
   n:=it_zesmnu
   r:=w_zes(@n)
   if n#NIL
      it_zesmnu:=n
   endif

   IF STRONA>0
   speCout(chr(13))
#ifdef A_PCL
#ifdef A_XPRN
if MEMVAR->P_PCL
#endif
if landscape
   speCout(chr(27)+'&l0O')
   landscape:=.f.
endif
#ifdef A_XPRN
endif
#endif
#endif
   setprc(0,0)
   if set(_SET_ALTERNATE,.f.).and. file(a:=set(_SET_ALTFILE,""))
      fview(a)
      ferase(a)
   else
#ifdef A_PRINT
     if set(_SET_PRINTER,.f.)
        x:=set(_SET_PRINTFILE,'')
      if File(x)
        A_PRINT(x)
      endif
     endif
#endif
   endif
ELSEIF r=.t.
   alarm("BRAK TAKICH DANYCH !",,,3)
ENDIF

#ifdef A_WIN_PRN
   if valtype(oprn)='O'
      oprn:=NIL
   endif
#endif
SET PRINTER off
set printer to
set alternate to
set alternate off

return
*********************
func w_zes(it_zesmnu)
memvar _sbnorm,defa,strona
memvar j,while,for,a,b,c,d,e,f,g,h,skip,getlist,self,buf
field baza,order,nr_zes,nazwa,pola,relacje,druk_proc
local i,l,win,ap,txt,jh,jf,jl,jt,el,x,y
static apcomp:={}
#ifndef A_XPRN
#define P_ROWN 58
#else
memvar p_rown
#ifdef A_PCL
memvar landscape,p_rownl
#define P_ROWN if(landscape,p_rownl,p_rown)
#endif
#endif

if sel("zes_def")=0
   RETURN .f.
ENDIF
if it_zesmnu#NIL
   goto it_Zesmnu
endif
setpos(row()+recno(),col())
if szukam({1,col(),,,0,0,"Zestawienia",{||nr_zes+" "+nazwa}})
   it_zesmnu:=recno()
   ap:=getlines(POLA,.t.)
   l:=len(ap)
   private a,b,c,d,e,f,g,h,j,self,buf
   do while 1<=l .and. ap[1]="&:"
      (&(SubStr(ap[1],3)),asize(adel(ap,1),--l))
   end
   for i:=1 to l
      ap[i]:=asize(hb_ATokens(ap[i],";"),6)
   next

   win:=window(l,60,_sbnorm)
   @ win[1],win[2]+2 say trim(nazwa)
   private getlist:={}
   select 0
   for i:=1 to l
        if ZES_DEF->baza#"MEMVAR"
           MEMVAR->&(ap[i,2]):=ZES_DEF->&(ap[i,2])
        endif
        @ win[1]+i,win[2]+1 say padl(ap[i,1],15)
        txt:=GETnew(row(),col()+1,memvarblock(ap[i,2]),ap[i,2])
        if !empty(ap[i,5])
           txt:preblock:=&(ap[i,5])
        endif
        if !empty(ap[i,4])
           txt:postblock:=&(ap[i,4])
        endif
        if !empty(ap[i,3])
           txt:picture:=ap[i,3]
        endif
        if ZES_DEF->(type(ap[i,2]))=if(ZES_DEF->baza="MEMVAR","C" ,"M")
           txt:cargo:=.t.
           if empty(txt:picture)
              txt:picture:='@S45'
           endif
#ifdef A_HBGET
           if len(MEMVAR->&(ap[i,2]))<45
              MEMVAR->&(ap[i,2]):=pad(MEMVAR->&(ap[i,2]),45)
           endif
#endif
        endif
        aadd(getlist,txt):display()
   next
   __setproc(procname(0)+zes_def->nr_zes)
   READ SCREEN win
   select zes_def
   window(win)
   if ReadkeY()#K_ESC
      begin sequence
      if ZES_DEF->baza#"MEMVAR"
         lock
         for i:=1 to l
           if type(ap[i,2])='M'
              MEMVAR->&(ap[i,2]):=trim(MEMVAR->&(ap[i,2]))
           endif
           ZES_DEF->&(ap[i,2]):=MEMVAR->&(ap[i,2])
         next
         sel(baza,trim(zes_def->order))
         rel(make_subar(getlines(zes_def->relacje,.t.)))
         unlock in zes_def
         go top
      else
         select 0
        if empty(zes_def->druk_proc)
           strona:=1
           break
        endif
      endif
      private while, for, skip
      while:={||!eof()}

#ifdef B_SKIP
      skip:=B_SKIP
#else
      skip:={||dbskip()}
#endif

      txt:=zes_def->druk_proc
      buf:=zes_def->(recno())
      i:=ascan(apcomp,{|x|x[1]==buf})
      if i#0
        buf:=apcomp[i,2]
      else
        txt:=trim(txt)
        if len(txt)<=12
          if !"."$txt
            txt+=".ppz"
          endif
          i:=findfile(txt)
          if !empty(i)
             buf:=getlines(memoread(i),.t.)
          endif
        else
          buf:=getlines(txt,.t.)
          txt:=NIL
        endif
        aadd(apcomp,{zes_def->(recno()),buf})
      endif
      l:=len(buf)
      cls
      j:=1
#ifdef A_WIN_PRN
      oprn:=A_WIN_PRN
#endif
      EVLINE(buf,":HEADER",l)
      if j<2 .or. j>l
         break
      endif
      jh:=j
#ifdef B_SKIP
      y:=eval(skip,0)
#else
      __dbLocate( for,while,,,.t.)
      y:=found()
#endif
      IF y .and. strona=0
         print()
      ELSE
         strona:=0
      ENDIF
      do while y
         ++strona
         j:=jh
         EVLINE(buf,":LINES",l)
         if j<2
            exit
         endif
         jl:=j
         do while y .and. prow()<P_ROWN //.or. !set(_SET_PRINTER))
            j:=jl
            EVLINE(buf,":FOOTER",l)
            if j<2
               exit
            endif
            jf:=j
#ifdef B_SKIP
            y:=eval(skip,1)
#else
            eval(skip)
            __dbLocate( for,while,,,.t.)
            y:=found()
#endif
         end
         if !y
            j:=if(jt=NIL,ascan(buf,{|x|valtype(x)$"MC".and. x=":TOTAL"})+1,jt)
            exit
         endif
         if jf=NIL
            jf:=ascan(buf,{|x|valtype(x)$"MC".and. x=":FOOTER"})+1
            if jf<2
               exit
            endif
         endif

         j:=jf

         EVLINE(buf,":TOTAL",l)
         if j<2
            exit
         endif
         jt:=j
         specout(chr(13))
         setprc(0,0)

      end
      if strona=0
         break
      endif

      EVLINE(buf,"RETURN",l)
      end sequence
      x:=.t.
   else
      x:=.f.
   endif
else
   x:=.f.
endif
select (select('zes_def'))
use
return x
