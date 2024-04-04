#ifdef A_WL
#ifdef __PLATFORM__UNIX_
   #require "hbcurl"
#endif
#endif

#include "dm_form.ch"
#include "inkey.ch"
#include "getexit.ch"
#ifdef A_F9
#ifndef A_FK
#define A_FK
#endif
#endif
#ifdef A_ANKER
#ifndef A_KHKONTO
#define A_KHKONTO
#endif
#endif

#ifndef A_MKNK
  #define A_MKNK(x) eval({|y|mknk:=min(5,max(mknk,len(ltrim(str(y))))),pad(str(y,mknk),5)},x)
#endif

#define compNk(x,y) (strtran(x,' ')==strtran(y,' '))

#ifdef A_JMTOT
#define D_JM jm
#endif
#ifdef A_ALTCEN
 #define A_CENSPEC
#endif

#ifdef A_MM
#ifdef A_SUBDOK
#define KEY_PAR left(dok,2)
#define D_SUBDOK +sub_dok
#else
#define D_SUBDOK
#define KEY_PAR dok
#endif
#define KEY_DOK smb_dow
#else
#ifdef A_SUBDOK
#define KEY_PAR mag_biez+left(dok,2)
#define D_SUBDOK +sub_dok
#else
#define D_SUBDOK
#define KEY_PAR mag_biez+dok
#endif
#define KEY_DOK nr_mag+smb_dow
#endif

#ifdef A_SZTUKI
#define ILDEC 0
#else
#define ILDEC 3
#endif
#ifdef A_DF
#command REPLACE [DM->]WARTOSC WITH <x> => field2bin('d_wartosc',DM->wartosc:=<x>,1)
#define wartosC (bin2d(field->d_wartosc))
#endif

#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif

#ifndef A_DDBF
#define DatY MEMVAR
#endif

#define WAPIC     "@KE # ### ###.##"

#define  dok_p_r dok_par[MAG_POZ,r,1]
#define  dok_zew dok_par[MAG_POZ,r,2]
#define  dok_kon dok_par[MAG_POZ,r,3]
#define  dok_war dok_par[MAG_POZ,r,4]
#define  dok_lpm dok_par[MAG_POZ,r,5]
#define  dok_naz dokumenty[MAG_POZ,r]
#define  dok_ew  dok_par[MAG_POZ,r,8]
#define  dok_df  dok_par[MAG_POZ,r,A_DF]
#define dok_kh  (dok_zew#"W" .and. ""#dok_kon )
#define dok_wal dok_par[MAG_POZ,r,A_WALUTA]

MEMVAR n_f,nk,dd,da,dv,d_o,kh,DOK,SCR,nim,il,gtot,chgpos,gil,;
       nz,stary_rok,MAG_BIEZ,mag_poz,r,operator,dok_rozch,;
       nowydm,magazyny,dok_par,adres_mag,lam,itot,miar_opcja,;
       is_spec,pm,changed,avat,defa,nkp,mknk,n_ksef
#ifdef A_WA
MEMVAR wa,ce,ck,chg_cen
field wartosc,cena_przy
#else
#ifdef A_FA
field wartosc
#endif
#endif
#ifndef STANY
memvar  nowystan
#endif
#ifdef A_OLZA
memvar posproc,sk,kk,stano_kos,zaklady,czynnosc,dzialy,kont,kos,mater,stano,dzial
field konto_kosz,stano_kosz
#endif
#ifdef A_WE
#define wtoT dm->warT_ewiD
#else
memvar wtot
#endif

field   data,smb_dow,nr_dowodu,pozycja,nr_zlec,ilosc,ilosc_f,dost_odb,kontrahent,KONTO,;
      OPIS_KOSZT,index,nazwa,adres,uwagi,NUMER_KOL,nr_mag,;
      jm,stan,KTO_PISAL,data_przy,data_zmian,data_roz,jm_opcja,rodz_opak,gram,;
      cennik,sub_dok,nazwisko,ident,nr_ksef
#ifdef A_LPNUM
#define D_LP0 str(0,A_LPNUM) //'  0'
#define D_LP1 str(1,A_LPNUM) //'  1'
#define D_LPPUT(x) str(x,A_LPNUM)
#define D_LPVAL(x) val(x)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#else
#define D_LP0 '0'
#define D_LP1 '1'
#define D_LPVAL(x) (asc(x)-48)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPPUT(x) chr(x+48)
#endif
#ifdef A_FA
  field proc_VAT
  memvar pv
#ifdef A_FAT
  memvar sp
#endif
  field cena, cena_zak
  memvar cz,wz
#endif
#ifdef A_VAT
  field wart_vat,wart_net
#endif
#ifdef A_JMO
#define restouT(x,p) if(x%p=0,str(x/p,6),stuff(str(int(x/p)+x%p/1000,10,3),7,1,"r"))
#endif
**********************
FUNCTION nk(_f,g)
#ifdef A_FA
  memvar zap,zac,nrc,uw
  field czekiem, przelewem, nr_czeku
#endif
  LOCAL nk2:=if(nowydm,'',nr_dowodu),s:=RECNO(),znalaz,rf,l

   // nie ma prawa byc niewpisane DEFAULT mknk TO 4

   rf:=val(nk)
   l:=len(ltrim(str(rf)))
   if rf<>0 .and. l<mknk .and. l=len(trim(nk))
      nk:=A_MKNK(rf)
   endif

   if !empty(nk) .and.  compNk(NK,NK2)
      return .t.
   endif

   set order to tag dm_trim
   if empty(nk) .or. dbSEEK(dseek(,'nr_mag,smb_dow,nr_dowodu',mag_biez,left(dok,2),nk))

#ifdef A_LAN
BEGIN SEQUENCE
DO WHILE .T.
#endif

#ifdef A_KHSEP
#define D_KH kontrahent
#define D_KH1 dost_odb
#else
#define D_KH left(dost_odb,A_NRLTH)
#define D_KH1 if(val(dost_odb)=0,dost_odb,subs(dost_odb,A_NRLTH+2))
#endif
     rf:=firMy->(recno())
     set order to 1
#ifdef A_FK
    znalaz:= szukam({1,40,maxrow(),,1,0,"Przegl¥d "+dok,;
        {||nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+if(data>DatY->d_z_mies1 .and. kto_pisal="ÿ","³","|")+D_KH+"³"+DTOV(data)+"³"+D_KH1},{|_skey,_s|nkprzeg(_skey,_s,_f,nk2)},KEY_PAR})
#else
    znalaz:= szukam({1,40,maxrow(),,1,0,"Przegl¥d "+dok,;
        {||nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+if(data>DatY->d_z_mies1 .and. kto_pisal="ÿ","³","|")+DTOV(data)+"³"+dost_odb},{|_skey,_s|nkprzeg(_skey,_s,_f,nk2)},KEY_PAR})
#endif
     firMy->(dbgoto(rf))
    if znalaz
      nk:=NR_DOWODU
      if deleted()
         GO s
      else
         LOCK
#ifdef A_SUBDOK
         sub_dok:=subs(dok,3)
#endif         
         NOWYDM:=.F.
         _fpopkey:=.f.
#ifdef A_OLDA
#define D_OLZA +31
#else
#define D_OLZA
#endif
#ifdef A_LAN
         DatY->(dbgoto(1))
         if data<=max(DatY->d_z_mies1,DatY->data_gran)
            if month(max(DatY->d_z_mies1,DatY->data_gran)+1 D_OLZA)=1
              break
            endif
            data := max(DatY->d_z_mies1,DatY->data_gran)+1
         endif
#endif
         _flp:=_flpmax
         nkp:=if(pozycja>=D_LP1,pozycja,nil)
         eval(_fdmpre,_f)
         //dok1(_f)
         KIBORD(CHR(18))
      endif
    ELSEIF !NOWYDM
      go s
      LOCK
    ENDIF
#ifdef A_LAN
EXIT
ENDDO
RECOVER
GO s
KIBORD(CHR(27))
END SEQUENCE
#endif
    set order to 1
    set filter to
    set relation to
   else
    set order to 1
    go s
    if _flp=0 .or. (tone(130,3),1=alarm("CZY CHCESZ ZMIENI NUMER BIE½¤CEGO DOKUMENTU ?",{"TAK","NIE"},2))
       return .t.
    endif
    g:undo()
   ENDIF
return .f.

**************************
FUNCTION nkprzeg(_skey,_s,_f,nk2)
do case
#ifdef A_DMDATA
  case _skey=0
       _sfor:={||KEY_PAR=KEY_DOK}
#endif
  case _skey=13 .or. _skey=22
#ifdef A_OLZA
  #define DEF_WAR
#else
  #define DEF_WAR kto_pisal=chr(255) .and.
#endif
#define DEF_WAR1
#ifdef A_LAN
  if DEF_WAR (pozycja=D_LP0 .or. data>max(DatY->d_z_mies1,DatY->data_gran)) .and. lanwar(_skey,_f,nk2)
#else
  if DEF_WAR (pozycja=D_LP0 .or. data>max(DatY->d_z_mies1,DatY->data_gran) ).and. (compNk(nr_dowodu,nk2) .or. _flp=0 .or. _skey=13 .and. pozycja=D_LP0 DEF_WAR1 .and.;
     (tone(130,3),1=alarm("CZY CHCESZ ZMIENI NUMER BIE½¤CEGO DOKUMENTU ?",{"TAK","NIE"},2)) .and.;
    (dbdelete(),.t.))
#endif
    RETURN(_sret:=.T.)
  else
#ifdef A_OLZA
     private changed:=.f.
#else
 #ifdef A_LAN
     private changed:=if( kto_pisal#chr(255) .and. data>max(DatY->d_z_mies1,DatY->data_gran).and. ( _flp=0 .or. _skey=13 .and. pozycja=D_LP0 DEF_WAR1),if(nowydm,,1),.f.)
 #else
     private changed:=if( kto_pisal#chr(255) .and. data>max(DatY->d_z_mies1,DatY->data_gran) .and. ( _flp=0 .or. _skey=13 .and. pozycja=D_LP0 DEF_WAR1),,.f.)
 #endif
#endif
     _skey:=ordnumber()

#ifdef A_MM
 #define D_MM ''
#else
 #define D_MM nr_mag
#endif
     DOK_IN(D_MM,smb_dow D_SUBDOK,nr_dowodu+pozycja,.t.)
     set order to (_skey)
  endif

#ifdef A_DMDATA
   CASE _skey=2 .or. _skey=26 // ^>
#ifdef A_FK
    IF _skey=26
         _skey={2,3,1}[ordnumber()]
    ELSE
         _skey={3,1,2}[ordnumber()]
    ENDIF
#else
    _skey:=3-ordnumber()
#endif
    set order to _skey
    if _skey=2
       _spocz:=str(year(data),4)
       _spform:={|p,l|TranR(right(p,l),"##.##")}
#ifdef A_FK
       _sbeg:=11+A_NRLTH
    elseif _skey=3
       _spocz:=""
#ifdef A_NRLTH3
       _spform:={|p,l|TranR(right(p,l),"XXX|##.##")}
#else
       _spform:={|p,l|TranR(right(p,l),"XXXXX|##.##")}
#endif
#endif
       _sbeg:=10
    else
       _spocz:=KEY_DOK
       _spform:={|p,l|right(p,l)}
       _sbeg:=1
    endif
    _slth:=0
    _swar:=&('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(1,_s)
#endif

  case _skey=9 .and. nowydm
  _skey:=ordnumber()
  private changed:=.f.
  wydruk_dok()
  select main
  set relation to
  select dm
  set order to (_skey)
  if changed
     REFRESH LINE _sm+_srow1-1 DIRECTION 0
  endif

  case _skey=27
   return .t. 

   case _skey=-6
      if dok_kh
      set relation to D_KH into firMY
      endif
      _slist(".\hn*.frm",_s)
        IF _sfilb=NIL
          SET RELATION TO
        ENDIF   

   case _skey=-8
       
      if dok_kh
      set relation to D_KH into firMY
      endif
      _sfil(_s)
        IF _sfilb=NIL
          SET RELATION TO
        ENDIF   

   case _skey=-9

      if dok_kh
      set relation to D_KH into firMY
      endif
      _slist(,_s)
        IF _sfilb=NIL
          SET RELATION TO
        ENDIF   

endcase

RETURN(.F.)
****************
#ifdef A_LAN
static function lanwar(_skey,_f,nk2)
local r:=compNk(nr_dowodu,nk2)
  if !r .and. (_flp=0 .or. _skey=13 .and. pozycja=D_LP0 DEF_WAR1 .and.(tone(130,3),1=alarm("CZY CHCESZ ZMIENI NUMER BIE½¤CEGO DOKUMENTU ?",{"TAK","NIE"},2)))
     r:=reclock(.f.,"DOKUMENT NIEDOST¨PNY DO POPRAWY;jest poprawiany przez innego u¾ytkownika sieci",.f.,,recno())
     if r .and. _flp#0
        delete
        unlock recno()
     endif
  endif
return r
#endif
#undef DEF_WAR
#undef DEF_WAR1
*************************
#ifdef A_DIETA
func dival(g,d)
memvar posilki,diety
local r,l,c,j
 if d=NIL
   if g=NIL
     g:=getactive()
   endif
   d:=g:buffer
 endif
 d:=UpP(d)
 c:=subs(d,2,1)
 if r:=aczojs(posilki,@c,,,"Posilek")
   d:=stuff(d,2,1,c)
 endif
if r .and. ! g:buffer==d
  g:buffer:=d
  g:ASSIGN()
  updated(@r)
endif
return r
#endif
*************************
#ifdef A_OLZA
    func gkon()
       local i,j,b
       b:=kk
       if !aczojs(kont,@b,@i)
          getactive():pos:=1
          return .f.
       elseif kont[i]=b .and. !"?"$kont[i]
          kk:=b
          return .t.
       endif
       kk:=b
       getactive():updatebuffer()
       b:=subs(kk,4,1)
       if !aczojs(zaklady,@b,,,"Zakˆady:")
          getactive():pos:=4
          return .f.
       endif
       kk:=stuff(kk,4,1,b)
       b:=subs(kk,5)
       if b#" ".and.b=subs(kont[i],5,1)
          return .t.
       endif
       getactive():updatebuffer()
       if !aczojs(kos,@b)
          getactive():pos:=5
          return .f.
       endif
       kk:=stuff(kk,5,1,b)
    return .t.
***********************
    func gsta(getlist)
       local i,j,k,b,z,d:=ascan(getlist,{|g|g:name=='d_o'})
       b:=sk
       if !aczojs(stano,@b,@i)
          getactive():pos:=1
          return .f.
       elseif stano[i]=b .and. !"?"$stano[i]
          sk:=b
          if d_o=" "
             d_o:=trim(subs(stano[i],8))
             getlist[d]:display()
             updated(.t.)
          endif
          return .t.
       endif
       sk:=b
       getactive():updatebuffer()
       b:=subs(sk,4,1)
       if !aczojs(zaklady,@b,@j,,"Zakˆady:")
          getactive():pos:=4
          return .f.
       endif
       sk:=stuff(sk,4,1,b)
       if !" "$sk.and.subs(sk,5)==subs(stano[i],5,2)
          if d_o=" "
             d_o:=trim(subs(stano[i],8))+subs(zaklady[j],2)
             getlist[d]:display()
             updated(.t.)
          endif
          return .t.
       endif
       getactive():updatebuffer()
       z:=array(len(dzial))
       aeval(dzial,{|x,i|z[i]:=b=trim(substr(x,4,1))})
       b:=subs(sk,5)
       j:=NIL
       if !aczojs(dzial,@b,@j,z,"Dziaˆy:")
          getactive():pos:=5
          return .f.
       endif
       sk:=stuff(sk,5,2,b)
       if dzial[j]=b //do peˆna
          if d_o=" "
             d_o:=trim(subs(stano[i],8))+subs(dzial[j],5)
             getlist[d]:display()
             updated(.t.)
          endif
          return .t.
       endif
       getactive():updatebuffer()
       b:=subs(sk,6)
       if !aczojs(mater,@b,@k)
          getactive():pos:=6
          return .f.
       endif
       sk:=stuff(sk,6,1,b)
       if d_o=" "
          d_o:=trim(subs(stano[i],8))+trim(subs(dzial[j],5))+subs(mater[k],2)
          getlist[d]:display()
          updated(.t.)
       endif

    return .t.
    *************
    FUNCTION GZLE()

      local Za,Cz,Dz,j,dzwz,b

      if nz="0"
         nz:="000000000"
         return .t.
      endif

      za=LEFT(nz,1)
      cz=substr(nz,2,2)
      dz=substr(nz,4,2)
   do while .t.
      if !aczojs(zaklady,@za,,,"Zakˆady")
         getactive():pos:=1
         return .f.
      elseif nz#za
         nz:=za+cz+dz+subs(nz,6)
         getactive():updatebuffer()
         loop
      elseif !aczojs(czynnosc,@cz,,,"Czynno˜†")
         getactive():pos:=2
         return .f.
      elseif cz#substr(nz,2,2)
         nz:=za+cz+dz+subs(nz,6)
         getactive():updatebuffer()
         loop
      elseif dz#subs(sk,5,2).and.(dzwz:=array(len(dzial)),aeval(dzial,{|x,i|dzwz[i]:=za=trim(substr(x,4,1))}),j:=NIL,!aczojs(dzial,@dz,@j,dzwz,"Dziaˆy"))
             getactive():pos:=4
             return .f.
      elseif dz#subs(nz,4,2)
             nz:=za+cz+dz+subs(nz,6)
             getactive():updatebuffer()
             loop
      elseif " "==subs(dz,2)
             b:=subs(dz,2)
             if !aczojs(mater,@b)
                getactive():pos:=5
                return .f.
             endif
             dz:=left(dz,1)+b
             nz:=za+cz+dz+subs(nz,6)
             getactive():updatebuffer()
             loop
      elseif TRIM(nz)#nz
          alarm("WYPEã CAE POLE !!!",,,3)
          getactive():pos:=6
          return .f.
      else
         return .t.
      endif
    enddo
    return .f.
#endif
******************************
#ifdef A_FIFO
proc getck(nowy,ames)
local a,b,rcrd,s,w,c,d,e,f,x,y,z,v,u,odc
#ifndef STANY
  if NOWYSTAN
     ck:=0
     return
  endif
#endif
  a:=STANY->STAN
  b:=STANY->WARTOSC
  f:=STANY->(fieldpos('stanx'))<>0
if f .and. STANY->validx
  c:=STANY->STANX
  d:=STANY->WARTOSCX
else
  c:=d:=0
endif
  IF !nowy
    a:=round(a-ILOSC,3)
    b:=round(b-WARTOSC,A_ZAOKR)
    if f .and. STANY->validx .AND. da>=STANY->DATAX
       c:=round(c-ILOSC,3)
       d:=round(d-WARTOSC,A_ZAOKR)
       if ROUND(c-STANY->MAXX,3)>0
          c:=d:=0
       endif
    endif
  ENDIF
  odc:=STANY->cena_przy
  ck:=max(0,if(a<=0.and.odc>0,odc,b/a))
  if valtype(ames)='A'
  elseif il=0 .or. dok_p_r="P" .or. round(a+il,3)=0
     return
  elseif a+il<0 //.and. a>=0 
     if odc>0
       ck:=max(0,((a+il)*odc - b)/il)
     endif
     return   
  elseif f .and. c>0 .AND. da>=STANY->DATAX .and. ROUND(STANY->MAXX-c,3)>=0 .and. ROUND(c+il,3)>=0
     ck:=d/c
     return
  endif
         set order to tag MAIN_IND
         rcrd:=recno()
         z:=select()
         seek STANY->nr_mag+STANY->index+"~"
#ifdef A_LIFO
         a:=b:=c:=d:=0
         do while .t.
             skip -1
//----------------------------
             if (bof() .or. STANY->nr_mag+STANY->index>nr_mag+index) .and. round(a+il,3)<0
               y:=str(year(if(empty(data),da,data))-1,4)
               x:=select('main'+y)
               if x=0
                 u:=ordfor()
                 v:=ordkey()
                 x:=select()
                 begin sequence
                   if !file(defa+y+HB_ps()+'main.dbf')
                     break
                   endif

                   nuse (defa+y+HB_ps()+'main') new alias ('main'+y)
#ifdef A_CDX
                   set order to tag main_ind
                   if empty(indexord())
                     w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                     if empty(u)
                       index on &v tag main_ind eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     else
                       index on &v for &u tag main_ind eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     endif
                     //break
                   endif
#else
                   if !file(defa+y+HB_ps()+'main_ind'+ordbagext())
                     index on &v to (defa+y+HB_ps()+'main_ind')
                     w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                     if empty(u)
                       index on &v to (defa+y+HB_ps()+'main_ind') eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     else
                       index on &v to (defa+y+HB_ps()+'main_ind') for &u eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     endif
                     //break
                   else
                     set index to (defa+y+HB_ps()+'main_ind')
                   endif
#endif
                   x:=nil
                 recover
                   exit
                 end sequence
                 message(w)
               else
                 select (x)
                 x:=NIL
               endif
               if x=NIL
                 set order to tag main_ind
                 seek STANY->nr_mag+STANY->index+'@'
                 loop
               endif
             endif
//----------------------------
             if (bof() .or. STANY->nr_mag+STANY->index>nr_mag+index)
                if select()=z
                a+=STANY->zamkn_roku
                b+=STANY->wart_roku
                if f .and. round(a+c,3)>0
                 LOCK IN STANY
                 STANY->DATAX:=DatY->d_z_rok
                 STANY->MAXX:=STANY->STANX:=ROUND(a+c,3)
                 STANY->WARTOSCX:=round(b+d,A_ZAOKR)
                 STANY->validx:=.t.
                endif
                if valtype(ames)='A'
                  aadd(ames,{"","",DatY->d_z_rok,STANY->zamkn_roku,STANY->wart_roku,a,b})
                endif
                endif
                exit
             endif
             if select()=z .and. recno()=rcrd .and. !nowy
                c:=ILOSC //stanX
                d:=WARTOSC
                loop
             endif
             a+=ilosc
             b+=wartosc
             if ilosc<0 .or. wartosc=0 .and. ilosc=0 .or. il>0 .and. (wartosc=0 .or. ilosc=0 .or. round(a,3)<=0)
                loop
             endif
             if f .and. round(a+c,3)>0
                LOCK IN STANY
                STANY->DATAX:=DATA
                STANY->MAXX:=STANY->STANX:=ROUND(a+c,3)
                STANY->WARTOSCX:=ROUND(b+d,A_ZAOKR)
                STANY->validx:=.t.
                f:=.f.
             endif
             if round(a,3)>0
                if data>da
                  a:=b:=0
                  loop
                endif
                if valtype(ames)='A'
                  aadd(ames,{smb_dow+nr_dowodu+pozycja,nr_zlec,data,ilosc,wartosc,a,b})
                endif
             endif
             if round(a+il,3)>0  //? czemu nie >=
                *
                //ci¥gnie ˜redni¥ cene partii
                c:=max(a-ilosc,0)
                d:=max(b-wartosc,0)
                if round(a-c,3)=0
                   ck:=-d/il
                else
                   ck:=((il+c)*(b-d)/(a-c)-d)/il
                endif
                exit
             endif
          enddo
          select (z)
#else
          c:=a;d:=b
          do while .t.
             skip -1
//----------------------------
             if (bof() .or. STANY->nr_mag+STANY->index>nr_mag+index) .and. round(a+il,3)>0 //rozchod nie bierze caˆego stanu pocz
               y:=str(year(if(empty(data),da,data))-1,4)
               x:=select('main'+y)
               if x=0
                 u:=ordfor()
                 v:=ordkey()
                 x:=select()
                 begin sequence
                   if !file(defa+y+HB_ps()+'main.dbf')
                     break
                   endif

                   nuse (defa+y+HB_ps()+'main') new alias ('main'+y)
#ifdef A_CDX
                   set order to tag main_ind
                   if empty(indexord())
                     w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                     if empty(u)
                       index on &v tag main_ind eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     else
                       index on &v tag main_ind for &u eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     endif
                     //break
                   endif
#else
                   if !file(defa+y+HB_ps()+'main_ind'+ordbagext())
                     index on &v to (defa+y+HB_ps()+'main_ind')
                     w:=MESSAGE("Odtwarzanie skorowidza main_ind, baza: "+defa+y+HB_ps()+"main.dbf,;klucz: "+v+";.")
                     if empty(u)
                       index on &v to (defa+y+HB_ps()+'main_ind') eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     else
                       index on &v to (defa+y+HB_ps()+'main_ind') for &u eval {||dispout("±"),message(1),.t.} every int(1+lastrec()/(w[4]-w[2]-2))
                     endif
                     //break
                   else
                     set index to (defa+y+HB_ps()+'main_ind')
                   endif
#endif
                   x:=nil
                 recover
                   exit
                 end sequence
                 message(w)
               else
                 select (x)
                 x:=NIL
               endif
               if x=NIL
                 set order to tag main_ind
                 seek STANY->nr_mag+STANY->index+'@'
                 loop
               endif
             endif
//----------------------------
             if bof() .or. STANY->nr_mag+STANY->index>nr_mag+index .or. ilosc=0 .and. wartosc<>0 .and. data<=da
                if f .and. round(a,3)>0
                 LOCK IN STANY
                 STANY->DATAX:=DatY->d_z_rok
                 STANY->MAXX :=STANY->STANX:=round(a,3)
                 STANY->WARTOSCX:=round(b,A_ZAOKR)
                 STANY->validx:=.t.
                endif
                ck:=b/a
#ifdef A_CEOKR
                ck:=max(max(0,Round(ck-1,0)),min(Round(ck+1,0),((a+il)*Round(ck,A_ZAOKR) - b)/il))
#endif
                if valtype(ames)='A'
                  aadd(ames,{"","",DatY->d_z_rok,STANY->zamkn_roku,STANY->wart_roku,a,b})
                endif
                exit
             endif
             if select()=z .and. recno()=rcrd
                loop
             endif
             //e-=ilosc
             //f-=wartosc  // f,e - stan biezacy  d,c - stan koncowy
             if nr_mag+smb_dow$dok_rozch .or. (ilosc=0 .and. wartosc=0) //ilosc<=0
                loop
             endif
             if f .and. round(a-ilosc,3)<=0
               LOCK IN STANY
               STANY->DATAX:=DATA
               STANY->MAXX :=STANY->STANX:=round(a,3)
               STANY->WARTOSCX:=round(b,A_ZAOKR)
               STANY->validx:=.t.
               f:=.f.
             endif
             /*
             if data>da
                a:=e  // a,b - stan od ktorego ck zresetowany po PZ kt¢re po rozchodzie
                b:=f
                loop
             endif
             */
             a-=ilosc
             b-=wartosc

             if if(valtype(ames)='A', round(a+il,3)<0, round(a+il,3)<=0)              //dotarˆem do wˆa˜ciwego PZ
                ck:=((a+il)*wartosc/ilosc - b)/il
#ifdef A_CEOKR
                x:=Round(wartosc/ilosc,A_ZAOKR)
                y:=wartosc-x*ilosc //tyle dodac do warto rozch w ramach korekty
                ck:=max(max(0,Round(ck-1,0)),min(Round(ck+1,0),((a+il)*x - b - y)/il))
#endif
                if valtype(ames)='A'
                  aadd(ames,{smb_dow+nr_dowodu+pozycja,nr_zlec,data,ilosc,wartosc,a,b})
                endif
                exit
             endif
          enddo
          select (z)
          if ck>0 .and. d>=0 .and. c>0 .and. d+ck*il<0 .and. c+il>=0
             ck:=d/c  //stan koncowy brany gdy wartosc ucieka ponizej zera
          endif
#endif
          go rcrd
          set order to tag MAIN_NRK
          if ck<0
             ck:=0
          endif

return
#endif
*****************************
FUNCTION groz(g,_f,getlist)
local wk,sk,rcrd,a,b,x

#ifdef A_SPECYF
     if g:changed //round(pm*il-g:original,3)#0
        a:=specyfik(_f,1)
        if a#0 .and. g:changed /*round(a-g:original,3)=0 */.and. round(a-pm*il,3)#0 .and. (tone(130,3),1#ALARM("CZY KASOWA SPECYFIKACJ¨ ?",{"TAK","NIE"},2))
           g:undo()
           return .f.
        endif
     endif
#endif
#ifdef A_FA
  if dok_p_r="F"
     wz:=W(pm*il,cz,val(pv),dok_df)
  elseif dok_ew#"E"
     wz:=WPZ(pm*il*cz)
  endif
#endif

#ifdef A_IZ
    if dok_ew="Z"
       showwar(_f,getlist,gil)
    else
#else
    #define ilosc_f ilosc
#endif

#ifdef A_WA
#ifndef STANY
      IF NOWYSTAN
         wk:=sk:=0
      else
#endif
         wk:=STANY->WARTOSC
         SK:=STANY->STAN
  IF !_fnowy
    WK-=WARTOSC
    SK-=ILOSC
  ENDIF
#ifndef STANY
      endif
#endif
#ifdef A_FIFO
  if !chg_cen
      getck(_fnowy)
      UNLOCK IN STANY
      ce:=ROUND(ck,A_ZAOKR)
  endif
#endif
  wa:=ROUND(if(chg_cen,ce,ck)*il,A_ZAOKR)
#ifdef A_FA
  cenpz(_f,getlist)
#endif
  showwar(_f,getlist,gil)
#ifdef A_MINUS
   return .t.
#else
IF ROUND(SK+il,ILDEC)>=0 .AND. ROUND(WK+WA,A_ZAOKR)>=0 .or. il>=0
   RETURN .T.
ENDIF

tone(130,3) //('')
#ifdef A_JMO
return 2=alarm('NA STANIE JEST TYLKO'+if(miar_opcja,restouT(sk,(lam)->przel)+" "+(lam)->jm_opcja,strpic(SK,11,ILDEC,,.t.)+" "+(lam)->jm)+",;"+strpic(WK,12,A_ZAOKR,"@E ")+" ZOTYCH.",{"Poprawa","Akceptacja"})
#else
return 2=alarm('NA STANIE JEST TYLKO'+strpic(SK,11,ILDEC,,.t.)+" "+(lam)->jm+",;"+strpic(WK,12,A_ZAOKR,"@E ")+" ZOTYCH.",{"Poprawa","Akceptacja"})
#endif
#endif
*****
#else
*****
  showwar(_f,getlist,gil)
#ifdef A_MINUS
   return .t.
#else
  SK:=STANY->STAN
#ifndef STANY
  IF NOWYSTAN
    sk:=0
  endif
#endif
  IF !_fnowy
    SK-=MAIN->ILOSC
  ENDIF

IF ROUND(SK+il,ILDEC)>=0 .or. il>=0
   RETURN .T.
ENDIF

tone(130,3) //('')
#ifdef A_JMO
return 2=alarm('NA STANIE JEST TYLKO'+if(miar_opcja,restouT(sk,(lam)->przel)+" "+(lam)->jm_opcja,strpic(SK,11,ILDEC,,.t.)+" "+(lam)->jm),{"Poprawa","Akceptacja"})
#else
return 2=alarm('NA STANIE JEST TYLKO'+strpic(SK,11,ILDEC,,.t.)+" "+(lam)->jm,{"Poprawa","Akceptacja"})
#endif
#endif
#endif
#undef restouT
#ifdef A_IZ
    endif
    return .t.
#endif
**************************
#ifdef A_FA
func cenpz(_f,getlist) //zawsze na koäcu po zmianach il, wa, ce ,ck
  local cx,wk,sk,vt
  if dok_p_r="F" .or. dok_ew="Z" .or. dok_ew="E"      //.or. dok_p_r#"P" bo KW w TISie
     Return .t.
  endif
 #ifdef A_WA
  #ifdef A_AUTOMAR
   #ifdef cenA_zaK
      if ce#cz .or. wz#pm*wa
        ce:=cz
        wa:=pm*wz
        chg_cen:=.t.
        showwar(_f,getlist,gil)
      endif
   #else
      if (ce#cz .or. wz#pm*wa) .and. dok_p_r<>'F' .and. !( atail(getlist):name="wa" .and. wz=0 )
        ce:=cz
        wa:=pm*wz
        chg_cen:=.t.
        showwar(_f,getlist,gil)
      endif
      showvat(_f)
   #endif //cena_zak
  #else //automar
   #ifdef A_WEBRUT
    if dok_p_r<>'F'
      if ReadVar()$'CK,WA'
        wz:=Round(Round(pm*10000*wa,0)/(100+val(pv)),0)/100
        cz:=pm*wz/il
        showvat(_f)
        if A_WEBRUT 1 = 1
          return .t.
        endif
        // ze wsp¢ˆczynnikiem na nowo wyliczmay wa
      endif
        cx:=avat
        private avat:=aclone(cx)
        if !_fnowy
          vat(proc_vat,-VATPZGR(pm*cena,val(proc_vat)))
        endif


        vt:= round( A_WEBRUT vat() , A_ZAOKR)

        vat(pv,VATPZGR(wz,val(pv)))

        vt:=pm* (wz + round( A_WEBRUT vat() , A_ZAOKR) -vt)

        avat:=cx
        if round(wa - vt, A_ZAOKR)<>0
           wa:=vt
           ce:=ck:=wa/il
           chg_cen:=.t.
           showwar(_f,getlist,gil)
        else
           showvat(_f)
        endif
    endif
   #else
    #ifdef cenA_zaK
    if ce#cz .or. wz#pm*wa
      chg_cen:=.t.
      ce:=cz
      wa:=pm*wz
      showwar(_f,getlist,gil)
    endif
    #else
      showvat(_f)
    #endif
   #endif
  #endif 
 #else //a_wa
      showvat(_f)
 #endif //a_wa
return .t.
#endif
***************************
#ifdef A_WA
func gcen(_f,getlist)
   local wk,sk
   if ce=0
#ifdef A_FIFO
      if chg_cen
         getck(_fnowy)
         UNLOCK IN STANY
      endif
#endif
      ce:=ROUND(ck,A_ZAOKR)
      chg_cen:=.f.
   else
      chg_cen:=.t.
   endif
   if dok_war="+" .and. pm=1 .and. dok_zew#"W" .and. il=0 //przecena
#ifndef STANY
      IF NOWYSTAN
         wk:=sk:=0
      else
#endif
         wk:=STANY->WARTOSC
         SK:=STANY->STAN
         IF !_fnowy
           WK-=MAIN->WARTOSC
           SK-=MAIN->ILOSC
        ENDIF
#ifndef STANY
      endif
#endif
      wa:=ROUND(ce*sk-wk,A_ZAOKR)
   else
      wa:=ROUND(if(chg_cen,ce,ck)*il,A_ZAOKR)
   endif

#ifdef A_FA
//#ifndef A_WEBRUT
   if dok_ew<>'#'
     cenpz(_f,getlist)
   endif
//#endif
#endif
   showwar(_f,getlist,gil)

return .t.
********************
func gwar(_f,getlist)
     chg_cen:=.t.
     ce:=if(il=0,0,ROUND(wa/il,A_ZAOKR))
#ifdef A_FA
//#ifndef A_WEBRUT
   if dok_ew<>'#'
     cenpz(_f,getlist)
   endif
//#endif
#endif
     showwar(_f,getlist,gil)
return .t.
#endif
**************************
FUNCTION dataval(dat) // czy dzien byl wolny (w valid)
#ifdef A_LAN
DatY->(dbgoto(1))
#endif
do case
  case dat <= max(DatY->d_z_mies1,DatY->data_gran)
    alarm('DATA WSTECZNA !;ZAMKNI¨TO OKRES DO DNIA '+dtoc(max(DatY->d_z_mies1,DatY->data_gran))+';TAKI DOKUMENT NIE B¨DZIE ZAKSI¨GOWANY !',,,3)
    return .f.
  case stary_rok#NIL.and.dat>stary_rok
    alarm('NIE WYCHOD POZA STARY ROK',,,3)
    return .f.
  case year(dat D_OLZA)>year(DatY->d_z_rok+1 D_OLZA)
    alarm('DATA PRZYSZOROCZNA !;ZAMKNIJ ROK ZANIM ZACZNIESZ WPISYWA TEGOROCZNE DOKUMENTY !',,,3)
    return .f.
ENDcase
RETURN(.t.)
***************************
#ifdef A_OBR
function nzval(nz)
return szukam({0,10,maxrow(),,1,0,"",{||konto+"³"+opis_koszt},{|_skey,_s|zlec_obr(_skey,_s)},nz})
***************************
function zlec_obr(_skey,_s)

    local o,k,d,getlist

field opis_koszt,konto//,data_zamkn
DO CASE
   CASE _skey=0
      SELECT stanowis
      set order to "kont_num"
      IF _spocz=KONTO .and.!EOF() //.and. (EMPTY(data_zamkn).or.data_zamkn>=dm->data)
         _sret=.T.
         RETURN(.T.)
      ENDIF
      _spocz:=UpP(TRIM(_spocz))
      _slth:=LEN(_spocz)
      _spform:={|p|p}
      _swar=&('{|p|'+ IndexkeY(0)+'=p'+'}')
      //_sfor:={||EMPTY(data_zamkn).or.data_zamkn>=dm->data}
      _sfor:={||empty(nr_mag).or.nr_mag=mag_biez}
      if (eval(_swar,_spocz).or.dbseek(_spocz)).and._skip(0,,_s) .and. _spocz=UpP(trim(KONTO))
             getactive():varput(pad(KONTO+' '+opis_koszt,len(getactive():varget())))
             _sret=.T.
             RETURN .T.
      elseIF _slth=0
         set order to "kont_naz"
         _spocz:=UpP(trim(d_o))
         _slth:=LEN(_spocz)
         _sbeg:=8
         _spform:={|p|p}
         _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
         seek _spocz
      endif
      set cursor on

   CASE _skey=13
             getactive():varput(pad(KONTO+' '+opis_koszt,len(getactive():varget())))
      return(updated(_sret:=.t.))

   CASE _skey=27
      RETURN(.T.)

   CASE _skey=22   // INS
      if _si=0
         _srec[1]=recno()
         _sm=1
      endif
    getlist:={}
    lock
    O:=OPIS_KOSZT
    k:=konto
    D:=nr_mag
    @ _srow1+_sm-1,_scol1 get k PICTURE "@R"
    getl O
    //@ _srow1+_sm-1,_scol2-10 get d picture "@D"
    read
    if readkey()#27 .and. updated()
      _sbeg=1
      SET ORDER TO "KONT_NUM"
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
      _spocz:=""
      _slth:=0
      if empty(k)
        delete
      elseif K#KONTO .AND. !DBSEEK(UpP(K))
        append blank
        nr_mag:=d
        KONTO:=UpP(K)
      endif
      OPIS_KOSZT:=O
      //data_zamkn:=d
      unlock
      refresh(,_s)
    endif
    unlock

   CASE _skey=2  // ^>
      _sbeg=8
      SET ORDER TO "KONT_NAZ"
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
      _spocz:=""
      _slth:=0
      refresh(1,_s)

   CASE _skey=26  // ^<
      _sbeg=1
      SET ORDER TO "KONT_NUM"
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
      _spocz:=""
      _slth:=0
      refresh(1,_s)

   case _skey=K_TAB
      select indx_mat
      k:=recno()
      select main
      o:=recno()
      set order to "main_zle"
      set relation to nr_mag+index into indx_mat
      seek stanowis->konto
         szukam({1,17,maxrow(),,1,0,trim(stanowis->opis_koszt),{||index+"³"+DTOV(data)+"³"+nr_dowodu+"/"+str(D_LPVAL(pozycja),2)+"³"+str(pm*ilosc,9,ILDEC)+"³"+indx_mat->jm+"³"+indx_mat->nazwa},{|_skey,_s|_skey=K_ESC},stanowis->konto+mag_biez})
      select indx_mat
      goto k
      select main
      set relation to
      set order to "main_nrk"
      goto o
      select stanowis
   case _skey=-6
      _slist(".\Z*.frm",_s)

   case _skey=-8
      _sfil(_s)

   case _skey=-9
      _slist(,_s)

ENDCASE

return .F.
#endif
***************************
FUNCTION pomval(g,_f,getlist,_s)
LOCAL ZNALAZ,recf,RECM,RECS,RECI,RECD,rec1,rec2,PRZE:=IL,A,B,c,d,j,oldp,dpushl,ames,x,y,z,fchg:=.f.
#ifdef A_KASA
  #define A_DIETA
#endif
#ifdef A_ODDO
  #define D_DIETA_OR_ODDO
#endif
#ifdef A_DIETA
  #undef D_DIETA_OR_ODDO
  #define D_DIETA_OR_ODDO
#endif

#ifdef D_DIETA_OR_ODDO
  memvar dpush,dpos,darr,dok_zb
#endif

#ifdef A_ODDO
  IF dpush .and. dok$dok_zb
     dpush:=.f.
     updated(.t.)
  endif
#endif

#ifdef A_DIETA
  if dpushl:=dpush
    if darr[dpos-1] <> left(nim,len(index))
//       --dpos
       dpushl:=.f.
    else
 #ifdef A_KASA
     il:=prze:=-val(subs(nim,len(index)+1))
     pv:=subs(nim,len(index)+12,2)
     cz:=val(subs(nim,len(index)+16,10))
     wa:=pm*val(right(nim,10))
     if wa<>0
        ce:=ck:=wa/il
        fchg:=.t.
        //chg_cen:=.t.
     endif
  #ifdef A_CENSPEC
    if sp='CZ'
       ce:=ck:=&sp
       wa:=il*ce
       fchg:=.t.
       //chg_cen:=.t.
    endif
  #endif
     wz:=ROUND(pm*il*cz,A_ZAOKR)
     nz:=subs(nim,len(index)+36)
     nim:=pad(left(nim,len(index)),46)
 #else
     //nz:=subs(nim,len(index)+2,6)
     //prze:=-val(subs(nim,rat(chr(255),nim)+1))
     //nim:=pad(left(nim,len(index)),46)
 #endif
 #ifdef A_FA
     cenpz(_f,getlist)
 #endif
     aeval(getlist,{|x|x:display()},gil-1)
    endif
    dpush:=.f.
#endif
#ifdef A_F9
#define A_DIETA
memvar ppush,parr,pflag
dpushl:=.f.
if ppush
   if nim=""
   dpushl:=.t.
#ifdef A_MM
   mag_biez:=parr[7]
   getlist[1]:display()
#endif
   nim:=pad(parr[1],46)
   il:=prze:=pm*parr[2]
   nz:=parr[6]
#ifdef A_WA
   if dok_p_r#"F".and.dok_ew#"Z"
      wa:=pm*parr[5]
      ck:=ce:=ROUND(wa/il,A_ZAOKR)
      fchg:=.t.
      //chg_cen:=.t. //dla POLOKA
   endif
#endif
#ifdef A_FA
   cz:=parr[3]
   if dok_p_r="F"
#ifdef A_ZAGRODA
      dpushl:=.f. // bo cena przeliczana przez &sp
#endif
   else
      cz*=pm
   endif
   pv:=parr[4]
   wz:=pm*parr[5]
   IF _fnowy
      STANY->(dbseek(mag_biez+left(nim,len(index)),.f.))
#ifndef STANY
      INDX_MAT->(dbseek(STANY->index))
#endif
   ENDIF
#endif
#ifdef A_SPECYF
   lam:=4
   specyfik(_f,5,parr[8])
#endif
   cenpz(_f,getlist)
   aeval(getlist,{|x|x:display()},2)
   else
   pflag:=.f.
   endif
   parr:={}
   ppush:=.f.
#endif

#undef D_MM
#ifdef A_MM
#define D_MM .and. mag_biez=STANY->nr_mag
#else
#define D_MM
#endif

#ifdef A_DIETA
#define D_DIETA .or. dpushl
#define D_DIETANEG !dpushl .and.
  elseIF D_DIETANEG (!_fnowy .or. !g:changed) D_MM .and. nim=indx_mat->INDEX .AND. !INDX_MAT->(eof())
#else
#define D_DIETA
#define D_DIETANEG
  IF D_DIETANEG (!_fnowy .or. !g:changed) D_MM .and. nim=indx_mat->INDEX .AND. !INDX_MAT->(eof())
#endif
    RETURN .T.
  ENDIF

#ifdef A_MM
  IF (g:exitstate=GE_TOP .or. g:exitstate=GE_UP) .and. empty(nim)
     Return .t.
  endif
#endif


#ifdef A_WA
  chg_cen:=fchg .or. chg_cen .and. (pm=1 .or. nim=indx_mat->index)
#endif

  RECS:=STANY->(RECNO())
#ifndef STANY
  RECI:=indx_mat->(RECNO())
#else
  #define reCi recs
#endif
  RECM:=RECNO()
  RECD:=DM->(RECNO())
 recf:=firmy->(recno())
#ifdef A_JMO
  if !_fnowy
     oldp:=(lam)->przel
  endif
#endif
begin sequence
#ifdef A_LAN
do while .t.
#endif
ames:={}

#ifdef A_MYSZ
   #define D_MYSZ ,b,x,y,f
#else
   #define D_MYSZ
#endif

#ifdef A_MINUS
   #define D_MINUS .t.
#else
#ifdef A_IZ
   #define D_MINUS pm=1 .or. dok_ew="Z" .or. dok_p_r="S"
#else
   #define D_MINUS pm=1 .or. dok_p_r="S"
#endif
#endif
/*
#undef D_MM
#ifdef A_MM
#define D_MM mag_biez+
#else
#define D_MM
#endif
*/
   DEFAULT _s TO array(_sLEN)
   asize(_s,max(len(_s),_sLEN))
   _srowb:=1
   _scol1:=0
   _srowe:=maxrow()
   _sbeg:=1
   _slth:=0
   _swar:=NIL
   DEFAULT _sinfo TO {|_skey,_s D_MYSZ|peoma(_skey,_s D_MYSZ)}
#ifdef D_MINUS
   _skon:=D_MINUS
#else
   _skon:=NIL
#endif
   _sret:=.f.
   _si:=0
   _sm:=1
   _sbf:=.f.
   _sef:=.f.

  _spocz:=UpP(trim(nim))
  ZNALAZ:=szukam(_s)
  select firmy
    go recf
  select dm
    GO RECD
    set relation to
#ifndef STANY
  SELECT STANY
    SET ORDER TO "STAN_MAG"
    SET FILTER TO
    SET RELATION TO
#endif
  SELECT INDX_MAT
    SET FILTER TO
    SET ORDER TO "INDX_NUM"
    SET RELATION TO
    lam:=i_lam(da)
    SELECT MAIN
    SET FILTER TO
    SET ORDER TO "MAIN_NRK"
    SET RELATION TO
    GO RECM

#undef D_MM
#ifdef A_MM
 #define D_MM .or. mag_biez#STANY->nr_mag .or. MAIN->nr_mag#mag_biez
#else
 #define D_MM
#endif

#ifdef A_JMALTTOT
  IF ZNALAZ .and. (_fnowy D_MM .or. nim#indx_mat->index .or. reCi#indx_mat->(recno()) .or. il#prze D_DIETA)
#else
#ifdef A_JMO
  IF ZNALAZ .and. (_fnowy D_MM .or. nim#indx_mat->index .or. reCi#indx_mat->(recno()) .or. il#prze D_DIETA)
#else
  IF ZNALAZ .and. (_fnowy D_MM .or. nim#indx_mat->index .or. reCi#indx_mat->(recno()) .or. il#prze D_DIETA .or. (lam)->przel#1 .and. miar_opcja .and. il=0)
#endif
#endif

#ifdef A_IZ
#define D_IZ dok_ew#"Z" .and.
#else
#define D_IZ
#endif

#undef D_MM
#ifdef A_MM
#ifdef STANY
#define D_MM
#else
#define D_MM .or. mag_biez#MAIN->nr_mag
#endif
#else
#define D_MM
#endif


#ifdef A_WA
     if D_IZ !_fnowy .and. (reCi#indx_mat->(recno()) D_MM) .and. (ilosc#0 .or. wartosc#0)
#else
     if D_IZ !_fnowy .and. (reCi#indx_mat->(recno()) D_MM) .and. ilosc#0
#endif
           select STANY
           rec1:=recno()
           go recs
#ifndef STANY
           select indx_mat
           rec2:=recno()
           go reci
#endif
           SELECT MAIN
#ifdef A_LAN
           NIM:=padr(INDX_MAT->INDEX,46) // updated wymusi locki
#ifdef A_MM
           b:=mag_biez
           mag_biez:=STANY->nr_mag
#endif
           a:=updated(,{|g|ebl(g,_f,getlist,.t.)}) // set EditBlock
           if !updated(.t.) //lock nie udany
              if a=NIL //ale przedtem byˆ
#ifdef A_MM
                mag_biez:=nr_mag
#endif
                il:=ilosc
                nz:=nr_zlec
#ifdef A_WA
                wa:=wartosc
                ce:=ROUND(ck:=if(il=0,0,wa/il),A_ZAOKR)
#endif
#ifdef A_FA
                if dok_zew$"UV"
                  pv:=proc_VAT
                endif
                cz:=cena
                if dok_p_r="F"
                  wz:=W(pm*il,cz,val(pv),dok_df)
                elseif dok_ew#"E"
                  wz:=pm*cz
                  cz:=if(il=0,0,ROUND(cz/il,A_ZAOKR))
                  cenpz(_f,getlist)
                endif
#endif
                aeval(getlist,{|g|g:display()})
                showwar(_f,getlist,gil)
              endif
              updated(,g:cargo)
              break
           endif
#ifdef A_MM
           mag_biez:=b
#endif
           LOCK IN STANY
/*
#ifndef STANY
           LOCK IN INDX_MAT
#endif
*/
#endif
           STANY->stan:=round(STANY->stan-ILOSC,3)
#ifndef STANY
#ifdef A_ZAGRODA
           IF INDX_MAT->zapas_id<>0
             STANY->unlink:=.t.
           ENDIF
#endif
#endif
#ifdef A_WA
           STANY->WARTOSC:=round(STANY->wartosc-WARTOSC,A_ZAOKR)
#endif
#ifdef A_LIFO
        if STANY->VALIDX .and. da>=STANY->DATAX
           if ROUND(STANY->STANX-ilosc,3)<0 .or. ilosc<0 .or. ROUND(STANY->STANX-ILOSC-STANY->MAXX,3)>0
              STANY->VALIDX:=.f.
           else
              STANY->STANX:=round(STANY->stanx-ILOSC,3)
              STANY->WARTOSCX:=round(STANY->wartoscx-WARTOSC,A_ZAOKR)
           endif
        endif
#endif
           lam:=i_lam(da)

#ifndef A_MINUS
#ifndef A_CENFIX
#ifndef A_FIFO
        if pm=-1 .and. data<STANY->data_przy .OR. pm=1 .and. data<=STANY->data_roz
          set order to "MAIN_IND"
          SKIP
#ifdef A_WA
          sum ilosc,wartosc to a,b rest while nr_mag+index=STANY->(nr_mag+index)
#else
          sum ilosc to a rest while nr_mag+index=STANY->(nr_mag+index)
#endif
          go recm
          set order to "MAIN_NRK"
#ifdef A_WA
          c:=if(ROUND(STANY->stan-a,3)=0,0,(STANY->wartosc-b)/(STANY->stan-a)) // przed zaksi©gowaniem
          d:=if(ROUND(STANY->stan+ilosc-a,3)=0,0,(wartosc+STANY->wartosc-b)/(ilosc+STANY->stan-a)) // po zaksi©gowaniu
          if ROUND(b-c*a,A_ZAOKR)#0
             aadd(ames,message(trim(index+" "+indx_mat->nazwa)+";Wyci¥gasz t© pozycj© ze ˜rodka kartoteki !;Przed wyksi©gowaniem cena na dzieä "+dtoc(data)+";wynosiˆa "+ltrim(strpic(d,12,A_ZAOKR,"@E "))+", a po "+ltrim(strpic(c,12,A_ZAOKR,"@E "))+" zˆ."))
          endif
          B:=ROUND(STANY->WARTOSC-B,A_ZAOKR)
#endif
          A:=ROUND(STANY->STAN-A,3)
          IF A<0 .and. STANY->stan>=0
              aadd(ames,message('STAN UJEMNY TEGO DNIA :'+strpic(A,11,ILDEC,,.t.)+" "+(lam)->jm+";"+INDX_MAT->NAZWA+';'+INDEX))
#ifdef A_WA
          ELSEIF B<0 .and. STANY->wartosc>=0
              aadd(ames,message('WARTO— UJEMNA TEGO DNIA :'+strpic(B,12,A_ZAOKR,"@E ")+";"+INDX_MAT->NAZWA+';'+INDEX))
          ELSEIF B#0 .AND. A=0  .and. (STANY->wartosc=0 .or. STANY->stan#0)
              aadd(ames,message('WARTO— BEZ STANU TEGO DNIA :'+strpic(B,12,A_ZAOKR,"@E ")+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
#ifndef A_NOZAP
          ELSEIF INDX_MAT->zapas_min # 0 .and. A<INDX_MAT->zapas_min .and. STANY->stan>=indx_mat->zapas_min
              aadd(ames,message('ZA MAY ZAPAS TEGO DNIA :'+strpic(A,11,ILDEC,,.t.)+" "+(lam)->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
          ELSEIF INDX_MAT->zapas_max # 0 .and. A>INDX_MAT->zapas_max .and. STANY->stan<=indx_mat->zapas_max
              aadd(ames,message('ZA DU½Y ZAPAS TEGO DNIA :'+strpic(A,11,ILDEC,,.t.)+" "+(lam)->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
          ENDIF
        endif
#endif
#endif
        IF STANY->STAN<0
              aadd(ames,message('STAN UJEMNY :'+str(STANY->STAN)+" "+indx_mat->jm+";"+INDX_MAT->NAZWA+';'+INDEX))
#ifdef A_WA
        ELSEIF STANY->WARTOSC<0
              aadd(ames,message('WARTO— UJEMNA :'+strpic(STANY->WARTOSC,,,"@E")+";"+INDX_MAT->NAZWA+';'+INDEX))
        ELSEIF STANY->WARTOSC#0 .AND. STANY->STAN=0
              aadd(ames,message('WARTO— BEZ STANU :'+strpic(STANY->WARTOSC,,,"@E")+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
#ifndef A_NOZAP
        ELSEIF INDX_MAT->zapas_min # 0 .and. STANY->stan<INDX_MAT->zapas_min
              aadd(ames,message('ZA MALY ZAPAS :'+str(STANY->STAN)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
        ELSEIF INDX_MAT->zapas_max # 0 .and. STANY->stan>INDX_MAT->zapas_max
            aadd(ames,message('ZA DUZY ZAPAS :'+str(STANY->STAN)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
        ENDIF
#endif

#ifdef A_WA
           wtoT:=Round(wtoT-wartosc,A_ZAOKR)
#else
           wtoT:=Round(wtoT-ilosc*(lam)->cenA,A_ZAOKR)
#endif
#ifdef wtoT
           if wtoT#0
              commit in DM
           endif
#endif
#ifdef A_SPECYF
          c:=(lam)->jm //specyfik(_f,0) //refresh
#endif
#ifdef A_FA
        if dok_p_r="F"
          replace dm->wartosc with Round(dm->wartosC-WGR(pm*ilosc_f,cena,val(proc_vat),dok_df)/100,A_ZAOKR)
          if dok_zew$"UV"
             x:=WzVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df)
             y:=ILEVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df)
             vat(proc_vat,-y,y-x)
             putVAT()
#ifdef A_NVAT
             chgpos:=.t.
#endif
          endif
          cena:=0
        elseif dok_ew#"E"
          replace dm->wartosc with round(dm->wartosC-pm*cena,A_ZAOKR)
          if dok_zew$"UV"
             vat(proc_vat,-VATPZGR(pm*cena,val(proc_vat)),-100*pm*cena)
             putVAT()
          endif
          cena:=0
        endif
#endif
#ifdef A_GRAM
            gtot-=pm*ilosc*(lam)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,A_JMALTTOT(-pm*il,nz,lam,x)})
           else
              itot[j,2]+=A_JMALTTOT(-pm*il,nz,lam,x)
           endif
#endif
           j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
           if j=0
              aadd(itot,{(lam)->D_JM,-pm*ilosc})
           else
              itot[j,2]+=-pm*ilosc
           endif
#endif
#ifdef A_IZ
           ilosc_f:=0
#endif
           ilosc:=0
#ifdef A_WA
           wartosc:=0
#endif
           UNLOCK IN STANY
           STANY->(dbgoto(rec1))
#ifndef STANY
           INDX_MAT->(dbgoto(rec2))
#endif
     endif
     NIM:=padr(INDX_MAT->INDEX,46)

#ifdef STANY
#ifdef A_MM
     mag_biez:=STANY->nr_mag
     getlist[1]:display()
#endif
#else
#ifdef A_MM
     getlist[1]:display()
#endif
     IF STANY->(DBSEEK(mag_biez+INDX_MAT->INDEX,.F.))
       NOWYSTAN:=.F.
     ELSE
       NOWYSTAN:=.T.
#ifdef A_WA
      // wa:=ck:=CE:=0
      chg_cen:=.t.
#endif
     ENDIF
#endif
#ifdef A_IZ
     if dok_ew="Z"
        updated(.t.) //¾eby nie skasowali indeksu
     else
#endif
#ifdef A_LAN
     a:=updated(,) //kasuj ebl
#ifdef A_JMALTTOT
  #define D_AT .and. ((lam)->przel=1 .or. !miar_opcja)
#else
  #define D_AT
#endif
#ifdef A_WA
     if a#NIL .and. _fnowy .and. il=0 .and. prze=0 D_AT// .and. !(dok_war="+" .and. pm=1 .and. dok_zew#"W")
#else
     if a#NIL .and. _fnowy .and. il=0 .and. prze=0 D_AT
#endif
     updated(.f.)
     updated(,a)
     else
     if ebl(g,_f,getlist,.t.)=.f.
        nim:=space(46)
      if (a:=len(ames))>0
         a:=ames[a]
         @ a[3],a[2] SAY left("Naci˜nij co˜...",a[4]-a[2])
           tone(130,3)
           inkey(0)
           for a:=len(ames) to 1 step -1
             message(ames[a])
           next
        endif
        loop
     endif
     updated(.t.)
     endif
#else
/*
#ifndef STANY
     IF STANY->(DBSEEK(mag_biez+INDX_MAT->INDEX,.F.))
       NOWYSTAN:=.F.
     ELSE
       NOWYSTAN:=.T.
#ifdef A_WA
       //wa:=ck:=CE:=0
      chg_cen:=.t.
#endif
     ENDIF
#endif
*/
    updated(.t.)
#endif
#ifdef A_WA
    if !chg_cen
      ck:=STANY->(if(stan=0,CENA_PRZY,max(0,WARTOSC/STAN)))
    endif
#endif

#ifdef A_IZ
endif
#endif

    lam:=i_lam(da)
#ifndef A_JMALTTOT
    if (lam)->przel#1 .and. miar_opcja .and. il=0
       if prze=0
#ifdef A_LAN
          updated(.t.)
#endif
#ifndef A_JMO
        prze:=pm*(lam)->przel
#endif
#ifdef A_DIETA
       elseif ROUND(prze-(prze:=ROUND(prze/(lam)->przel,0)*(lam)->przel),3)#0
        tone(164.8,1)
        tone(164.8,1)
#endif
       endif
    endif
#endif
      if prze#IL
#ifndef A_MINUS
          j:=prze
          if j=0
            j:=il
          endif
#ifndef STANY
        if !nowystan .and. STANY->stan+j>=0 .or. D_MINUS
#else
        if STANY->stan+j>=0 .or. D_MINUS
#endif
#endif
          tone(261.7,1)
          tone(130.8,1)
          tone(164.8,1)
          if prze#0
            il:=prze
          endif
#ifndef A_MINUS
        else
         aadd(ames,message('NA STANIE JEST TYLKO'+str(STANY->stan)+" "+(lam)->jm+";Nie mam z czego rozchodowa†"+strpic(-prze,11,ILDEC,,.t.)+" "+(lam)->jm))
       endif
#endif
      endif
#ifdef A_ANKER
    @ _fk+1, 3 SAY ean13(nim)
#endif
      
    @ _fk,5 SAY (lam)->nazwA
#ifdef A_JMO
    if miar_opcja
       @ _fk+1,48 say (lam)->jm_opcja
       if !_fnowy .and. oldp#(lam)->przel .and. il#0 .and. il%oldp=0 .and. 1=alarm("ZMIANA KWANTU Z"+str(oldp,4)+" NA"+str((lam)->przel,4)+";CZY ZMIENI OGàLN¤ ILO— SZTUK",{"TAK","NIE"})
          il:=int(il/oldp)*(lam)->przel
       elseif il#0
          aeval(getlist,{|g|if(g:name='il',g:display(),)})
       endif
    else
       @ _fk+1,48 say (lam)->jm
    endif
#else
    @ _fk+1,48 say (lam)->jm
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,0})
           endif
#endif
           j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
           if j=0
              aadd(itot,{(lam)->D_JM,0})
           endif
#endif
#undef D_MINUS
*************
#ifdef A_WA
*************
#ifdef cenA_zaK
 #ifdef A_F9
  #define D_CZ (dok_p_r="F" .or. dok_ew="E" .and. !dpushl ) .and.
 #else
  #define D_CZ (dok_p_r="F" .or. dok_ew="E") .and.
 #endif
#else
 #ifdef A_F9
  #define D_CZ (dok_p_r#"P" .and. dok_war#"+".and. !dpushl ) .and.
 #else
  #define D_CZ (dok_p_r#"P" .and. dok_war#"+") .and.
 #endif
//#define D_CZ
#endif

#ifndef STANY
  #ifdef A_KOB
   #define D_KOB .or. dok_p_r="F" .and. ""=dok_war
  #else
   #define D_KOB
  #endif
//tu byˆo D_CZ
    if D_IZ !nowystan .and. (_fnowy D_KOB .or. il=0 .or. dok_war#"+" .or. pm=-1 .or. (tone(130,3),1=alarm("CZY AKTUALIZOWA WARTO— EWIDENCYJN¤ DOKUMENTU ?" ,{"TAK","NIE"},2)))
#else
//tu te¾
    if D_IZ (_fnowy .or. il=0 .or. dok_war#"+" .or. pm=-1 .or. (tone(130,3),1=alarm("CZY AKTUALIZOWA WARTO— EWIDENCYJN¤ DOKUMENTU ?" ,{"TAK","NIE"},2)))
#endif
      if pm=1 .and. dok_zew#"W" .and. !chg_cen
            chg_cen:=.t.
            ce:=STANY->cena_przy
#ifdef A_KOBSUR
// FIRMY->NUMER_KOL+INDX_MAT->index
            if dok="D" .and. dok_war#"+"
              sel("CENNIK",1)
              seek A_KOBSUR+dtos(da)
              if A_KOBSUR=FIELD->dostawca+FIELD->surowiec
                ce:=cena
                tone(164.8,1)
                tone(164.8,1)
              endif
              select MAIN
            endif
#endif

            if wa#(wa:=ROUND(il*ce,A_ZAOKR))
               private _sunsel:=_sel
            endif
#ifdef A_KOB
      elseif dok_p_r="F" .and. ""=dok_war
            ce:=(lam)->cena
            chg_cen:=.t.
            wa:=ROUND(il*ce,A_ZAOKR)
#endif
      elseif chg_cen
            if wa#(wa:=ROUND(il*ce,A_ZAOKR))
               private _sunsel:=_sel
            endif
      else
#ifdef A_FIFO
            getck(_fnowy)
            aeval(getlist,{|g|g:display()},gil+1)
            showwar(_f,getlist,gil)
#endif
            if round(wa-(wa:=ROUND(ck*il,A_ZAOKR)),A_ZAOKR)#0
               private _sunsel:=_sel
            endif
            ce:=ROUND(ck,A_ZAOKR)
      endif
    endif
***********
#endif
***********
#ifdef A_KOMOR
    if dok_zew="W" .and. ""#dok_kon .and. !empty((lam)->nr_zlec)
       nz:=(lam)->nr_zlec
       getlist[gil-1]:display()
    endif
#endif
#ifdef A_SPECYF
      if c#NIL .and. c#(lam)->jm //get jm
         tone(130,3)
         aadd(ames,message("ZMIANA JEDNOSTKI MIARY Z "+c+"NA "+(lam)->jm+";NA NIEPUSTEJ SPECYFIKACJI !!!"))
      endif
#endif

#undef D_MM
#ifdef A_MM
#ifdef STANY
#define D_MM .or. nr_mag#mag_biez
#else
#define D_MM
#endif
#else
#define D_MM
#endif

#ifdef A_FA
#ifdef A_CENSPEC
#define D_ALTCEN  (&sp)
#else
#define D_ALTCEN  cena
#endif
    if dok_p_r="F"
#ifdef A_NOPV
if pv#(lam)->proc_vat .or. D_DIETANEG  cz#(lam)->D_ALTCEN .AND.(_fnowy .or. il=0 .or. cz=0 D_MM .or. g:original#nim  .and. (tone(130,3),1=alarm("CZY ZMIENIA CEN¨ ZBYTU ?" ,{"TAK","NIE"},2)))
#else
if D_DIETANEG (pv#(lam)->proc_vat .or. cz#(lam)->D_ALTCEN) .AND.(_fnowy .or. il=0 .or. cz=0 D_MM .or. g:original#nim  .and. (tone(130,3),1=alarm("CZY ZMIENIA CEN¨ ZBYTU ?" ,{"TAK","NIE"},2)))
#endif
         pv:=(lam)->proc_vat
         cz:=(lam)->D_ALTCEN
         wz:=W(pm*il,cz,val(pv),dok_df)
         if cz=0 .and. dok_war#"+" .and. ascan(getlist,{|x|x:name="cz"})=0
            aeval(getlist,{|x|if(x:name="il".and.x:reader#NIL,x:reader:=NIL,)})
            add_faget(_f,getlist)
         endif
      endif
    elseif dok_ew#"E"
#ifdef A_NOPV
         if dok_zew$"UV" .and. pv#(lam)->proc_vat .or. D_DIETANEG  cz#STANY->cenA_zaK .and.( _fnowy .or. il=0 D_MM .or. g:original#nim .and. (tone(130,3),1=alarm("CZY ZMIENIA CEN¨ ZAKUPU ?" ,{"TAK","NIE"},2)))
#else
         if D_DIETANEG ( dok_zew$"UV" .and. pv#(lam)->proc_vat .or. cz#STANY->cenA_zaK).and.( _fnowy .or. il=0 D_MM .or. g:original#nim .and. (tone(130,3),1=alarm("CZY ZMIENIA CEN¨ ZAKUPU ?" ,{"TAK","NIE"},2)))
#endif
         pv:=(lam)->proc_vat
         cz:=STANY->cenA_zaK
         wz:=WPZ(pm*il*cz)
         elseif !dok_zew$"UV"
         pv:=(lam)->proc_vat
         endif
         cenpz(_f,getlist)
/*
#ifdef cenA_zaK
         if dok_ew#"Z"
         ce:=cz
         chg_cen:=.t.
         wa:=pm*wz
         endif
#else
         if dok_ew#"Z" .and. dok_p_r="P" .and. dok_war#"+"
         ce:=cz
         chg_cen:=.t.
         wa:=pm*wz
         endif
#endif
*/
    else
         pv:=(lam)->proc_vat
    endif

#endif
    showwar(_f,getlist,gil)
    aeval(getlist,{|g|if(g:name='il',g:display(),)})
#ifdef A_WA
#ifndef A_CENFIX
#ifndef STANY
    if D_IZ D_CZ dok_war#"+" .and. ascan(getlist,{|x|x:name="wa"})=0 .and. (stany->stan=0 .OR. NOWYSTAN)
#else
    if D_IZ D_CZ dok_war#"+" .and. ascan(getlist,{|x|x:name="wa"})=0 .and. indx_mat->stan=0
#endif
      aeval(getlist,{|x|if(x:name$"cz,il".and.x:reader#NIL,x:reader:=NIL,)})
      add_get(_f,getlist)
    endif
#endif
#endif
**********
  ELSE
**********
    STANY->(dbgoto(recs))
#ifndef STANY
    INDX_MAT->(dbgoto(reci))
#endif
//    NIM:=padr(INDX_MAT->INDEX,46)
#ifdef A_IZ
    if dok_ew#"Z"
#endif
#ifdef A_LAN
    a:=updated(,{|g|ebl(g,_f,getlist,.t.)}) // set EditBlock
    if a=NIL
     if !updated(.t.) //nie byˆ
      if !_fnowy
        il:=ilosc
        nz:=nr_zlec
#ifdef A_WA
        wa:=wartosc
        ce:=ROUND(ck:=if(il=0,0,wa/il),A_ZAOKR)
#endif
#ifdef A_FA
        if dok_zew$"UV"
        pv:=proc_VAT
        endif
        cz:=cena
        if dok_p_r="F"
           wz:=W(pm*il,cz,val(pv),dok_df)
        elseif dok_ew#"E"
           wz:=pm*cz
           cz:=if(il=0,0,ROUND(cz/il,A_ZAOKR))
        endif
#endif
        aeval(getlist,{|g|g:display()})
        showwar(_f,getlist,gil)
      endif
      updated(,g:cargo)
      break
     endif
     updated(,g:cargo)
     else
      updated(.f.)
     endif
#else
#ifndef STANY
     IF STANY->(DBSEEK(mag_biez+INDX_MAT->INDEX,.F.))
       NOWYSTAN:=.F.
     ELSE
       NOWYSTAN:=.T.
#ifdef A_WA
       wa:=ck:=CE:=0
#endif
     ENDIF
#endif
    lam:=i_lam(da)
//    @ _fk,5 SAY (lam)->NAZWA
//    @ _fk+1,48 say (lam)->jm
#endif
#ifdef A_IZ
    endif
#endif
#ifdef A_JMO
if !_fnowy
    @ _fk+1,48 say (lam)->(if(miar_opcja,jm_opcja,jm))
    if miar_opcja .and. oldp#(lam)->przel .and. il#0 .and. il%oldp=0 .and. 1=alarm("ZMIANA KWANTU Z"+str(oldp,4)+" NA"+str((lam)->przel,4)+";CZY ZMIENI OGàLN¤ ILO— SZTUK",{"TAK","NIE"})
       il:=int(il/oldp)*(lam)->przel
    elseif il#0
       aeval(getlist,{|g|if(g:name='il',g:display(),)})
    endif
  endif
#endif
  ENDIF
#ifdef A_LAN
  exit
  enddo
#endif
b:=NIL
recover using b
UNLOCK IN STANY
znalaz:=.f.
select main
end sequence
UNLOCK IN STANY

      if (a:=len(ames))>0
         a:=ames[a]
         @ a[3],a[2] SAY left("Naci˜nij co˜...",a[4]-a[2])
         tone(130,3)
         inkey(0)
         for a:=len(ames) to 1 step -1
             message(ames[a])
         next
      endif
if b#NIL
   break(b)
endif
RETURN znalaz
#ifdef A_KOBSUR
func nzval(g,_f,getlist)
    if pm=1 .and. D_IZ il=0 .and. dok_war#"+" .and. dok="D"
              sel("CENNIK",1)
              seek A_KOBSUR+dtos(da)
              if A_KOBSUR=FIELD->dostawca+FIELD->surowiec
                ce:=cena
                chg_cen:=.t.
                tone(164.8,1)
                tone(164.8,1)
                showwar(_f,getlist,gil)
              endif
              select MAIN
    endif
return .t.
#endif
#undef D_DIETA
#undef D_DIETANEG
#undef D_IZ
#undef D_CZ
#undef reCi
**************************************************************************
#ifdef A_SPECYF
func specyfik(_f,mode,g,p)
static asp:={},ksp:='',jmsp,u:=.f.
local i,j,win,b,c,key,x
field spec_nr

   IF ksp#dm->(KEY_DOK+nr_dowodu)+D_LPPUT(_fi)
      asp:={}
      ksp:=dm->(KEY_DOK+nr_dowodu)+D_LPPUT(_fi)
      jmsp:=(lam)->jm
      sel("SPEC","SPEC_NR")
      dbseek(ksp,.f.)
      i:=0
      DO WHILE ksp=spec_nr
         ++i
         aadd(asp,array(fcount()-1))
         for j:=1 to fcount()-1
            asp[i,j]:=fieldget(j+1)
         next j
         skip
      ENDDO
      select main
      if len(asp)>0
         aadd(asp,aclone(atail(asp)))
         atail(asp)[2]:=0
      else
#ifdef A_OLZBY
         aadd(asp,{space(20)  ,0,space(10),0})
#else
         aadd(asp,{space(20),0,0,0,0,0})
#endif
         if jmsp='m2' .and. (lam)->przel<>1 .and. (lam)->jm_opcja="M "
            atail(asp)[4]:=(lam)->przel
         endif
      endif
   elseif jmsp#(lam)->jm
      jmsp:=(lam)->jm
   ENDIF
   if mode=NIL
   ELSEIF mode=5
      asp:={}
      sel("SPEC","SPEC_NR")
      dbseek(g,.f.)
      i:=0
      DO WHILE g=spec_nr
         ++i
         aadd(asp,array(fcount()-1))
         for j:=1 to fcount()-1
            asp[i,j]:=fieldget(j+1)
         next j
         skip
      ENDDO
      select main
      if len(asp)>0
         aadd(asp,aclone(atail(asp)))
         atail(asp)[2]:=0
      else
#ifdef A_OLZBY
         aadd(asp,{space(20)  ,0,space(10),0})
#else
         aadd(asp,{space(20),0,0,0,0,0})
#endif
         if jmsp='m2' .and. (lam)->przel<>1 .and. (lam)->jm_opcja="M "
            atail(asp)[4]:=(lam)->przel
         endif
      endif
      return 0
   ELSEIF mode=1 //czytaj
      return licz_asp(asp,jmsp)
   ELSEIF mode=2 //zapisz
      sel("SPEC","SPEC_NR")
      dbseek(ksp,.f.)
      for i:=1 to len(asp)
          if asp[i,2]=0
             loop
          endif
          if ksp#spec_nr
             append blank
             spec_nr:=ksp
          else
             lock
          endif
          aeval(asp[i],{|x,y|fieldput(y+1,x)})
          skip
      next i
#ifdef A_LAN
      delete rest while ksp=spec_nr FOR reclock(.F.,,.F.)
#else
      delete rest while ksp=spec_nr
#endif
      unlock
      select main
      return 0
   ELSEIF mode=3  //zmieä pozycj©
      sel("SPEC","SPEC_NR")
      ksp:=dm->(KEY_DOK+nr_dowodu)+g
      dbseek(ksp,.f.)
#ifdef A_LAN
      exec spec_nr:=dm->(KEY_DOK+nr_dowodu)+p while ksp=spec_nr FOR reclock(.F.,,.F.)
      unlock
#else
      exec spec_nr:=dm->(KEY_DOK+nr_dowodu)+p while ksp=spec_nr
#endif
      select main
      ksp:=''
      return 0
   ELSEIF mode=-1 //kasuj
      sel("SPEC","SPEC_NR")
      dbseek(ksp,.f.)
#ifdef A_LAN
      delete while ksp=spec_nr FOR reclock(.F.,,.F.)
      unlock
#else
      delete while ksp=spec_nr
#endif
      select main
      asp:={}
      return 0
   ELSEIF mode=4 //get asp
      return asp
   ENDIF

   c:=array(31)

   c[K_DOWN]     :={|b|b:down()}
   c[K_UP]       :={|b|b:up()}
   c[K_LEFT]     :={|b|b:left()}
   c[K_PGUP]     :={|b|b:pageup()}
   c[K_PGDN]     :={|b|b:pagedown()}
   c[K_CTRL_PGUP]:={|b|b:gotop()}
   c[K_CTRL_PGDN]:={|b|b:gobottom()}
   c[K_END]      :={|b|b:colpos:=b:colcount-1}
   c[K_HOME]     :={|b|b:home()}
   c[K_F1]       :={||help(procname(0))}

#ifdef A_OLZBY
   c[K_TAB]      :={|b|if(b:colpos=b:colcount,(b:home(),b:down()),b:right())}
   c[K_RIGHT]    :={|b|b:right()}
   if len(asp)=0
      aadd(asp,{space(20),0,space(10),0})
   endif
   win:=window(max(18,len(asp)+2),32)
   //set color to (_sbkgr)
   setpos(win[1],win[2]+1)
   dispout(index)
   b:=tbrowsenew(win[1]+1,win[2]+1,win[3]-1,win[4]-1)
   b:gobottomblock:={||i:=len(asp)}
   b:gotopblock:={||i:=1}
   b:skipblock:={|s|-i+(i:=min(max(1,i+s),len(asp)))}
   b:addcolumn(tbcolumnnew('opis',{||pad(asp[i,1],10)}))
   b:addcolumn(tbcolumnnew('pudeˆ',{||tran(asp[i,2],"@Z #####")}))
   b:addcolumn(tbcolumnnew('lot kod',{||asp[i,3]}))
   b:addcolumn(tbcolumnnew('palet',{||tran(asp[i,4],"@Z #####")}))
#else
   if len(asp)=0
      aadd(asp,{space(20),0,0,0,0,0})
   endif
   win:=window(max(15,len(asp)+2),if(jmsp='m3',50,30))
   //set color to (_sbkgr)
   setpos(win[1],win[2]+1)
   dispout(index)
   b:=tbrowsenew(win[1]+1,win[2]+1,win[3]-1,win[4]-1)
   b:gobottomblock:={||i:=len(asp)}
   b:gotopblock:={||i:=1}
   b:skipblock:={|s|-i+(i:=min(max(1,i+s),len(asp)))}
   c[K_RIGHT]    :={|b|if(b:colpos+1<b:colcount,b:right(),)}
   b:addcolumn(tbcolumnnew('opis',{||pad(asp[i,1],10)}))
   if jmsp='m2'
      b:addcolumn(tbcolumnnew('il.szt',{||tran(asp[i,2],"@Z ######")}))
      b:addcolumn(tbcolumnnew('dˆug.m',{||tran(asp[i,3],"@Z ###.##")}))
#ifdef A_KAMIX
      b:addcolumn(tbcolumnnew('sz.m',{||tran(asp[i,4],"@Z ###.##")}))
      b:addcolumn(tbcolumnnew('RAZEM m2',{||tran(asp[i,2]*asp[i,3]*asp[i,4],"@Z ######.##")}))
#else
      b:addcolumn(tbcolumnnew('sz.mm',{||tran(asp[i,4],"@Z #####")}))
      b:addcolumn(tbcolumnnew('RAZEM m2',{||tran(asp[i,2]*asp[i,3]*asp[i,4]/1000,"@Z ######.##")}))
#endif
      c[K_TAB]      :={|b|if(b:colpos>=b:colcount-1,(b:home(),b:down()),b:right())}
   elseif jmsp='m3'
      b:addcolumn(tbcolumnnew('il.szt',{||tran(asp[i,2],"@Z ######")}))
      b:addcolumn(tbcolumnnew('dˆug.m',{||tran(asp[i,3],"@Z ###.##")}))
#ifdef A_KAMIX
      b:addcolumn(tbcolumnnew('sz.m',{||tran(asp[i,4],"@Z ###.##")}))
      b:addcolumn(tbcolumnnew('gr.m',{||tran(asp[i,5],"@Z ##.##")}))
#else
      b:addcolumn(tbcolumnnew('sz.mm',{||tran(asp[i,4],"@Z #####")}))
      b:addcolumn(tbcolumnnew('gr.mm',{||tran(asp[i,5],"@Z ###")}))
#endif
      b:addcolumn(tbcolumnnew('mi©¾sz.m3/m',{||tran(asp[i,6],"@Z ##.###")}))
#ifdef A_KAMIX
      b:addcolumn(tbcolumnnew('RAZEM m3',{||tran(if(asp[i,6]#0,asp[i,2]*asp[i,3]*asp[i,6],asp[i,2]*asp[i,3]*asp[i,4]*asp[i,5]),"@Z ######.###")}))
#else
      b:addcolumn(tbcolumnnew('RAZEM m3',{||tran(if(asp[i,6]#0,asp[i,2]*asp[i,3]*asp[i,6],asp[i,2]*asp[i,3]*asp[i,4]*asp[i,5]/1000000),"@Z ######.###")}))
#endif
      c[K_TAB]      :={|b|if(b:colpos>=b:colcount-2,(b:home(),b:down()),b:right())}
   else
      b:addcolumn(tbcolumnnew('ilo˜†',{||tran(asp[i,2],"@Z ######"+if(jmsp=lower((lam)->jm_opcja),'',' '+lower((lam)->jm_opcja)))}))
      b:addcolumn(tbcolumnnew('po '+jmsp,{||tran(asp[i,3],"@Z ###.##")}))
      b:addcolumn(tbcolumnnew('RAZEM',{||tran(asp[i,2]*asp[i,3],"@Z ######.##")}))
      c[K_TAB]      :={|b|if(b:colpos>=b:colcount-1,(b:home(),b:down()),b:right())}
   endif
#endif

   b:headSep := "ÂÄ"
   b:colsep:='³'

   i:=1
   b:gobottom()


   do while .t.
      b:forcestable()
      b:refreshcurrent()
      b:forcestable()
      key:=INkey(0)
      if key=K_SH_TAB
         key=K_LEFT
      endif
      if key=K_ESC .or. key=K_F10 .or. key=K_CTRL_END
         exit
      elseif key=45 //-
         asp[i,2]:=0
         u:=.t.
         if i<len(asp) .and. len(asp)>1
            asize(asp,len(asp)-1)
         endif
#ifdef A_OLZBY
      elseif key=K_ENTER .or. key>31 .and. key<256
#else
      elseif b:colpos<b:colcount .and. (key=K_ENTER .or. key>31 .and. key<256)
#endif
         if key#K_ENTER
            kibord(chr(key))
         endif
         j:=b:colpos
         key:=Set(_SET_EXIT,.t.)
         x:=asp[i,j]
#ifdef A_OLZBY
         readmodal({_GET_(x,"x",{"@KS10","@K #####","@!","@K #####"}[j],,)})
#else
#ifdef A_KAMIX
         readmodal({_GET_(x,"x",{"@KS10","@K ######","@K ###.##","@K ##.##","@K ###.##","@K ##.###"}[j],,)})
#else
         readmodal({_GET_(x,"x",{"@KS10","@K ######","@K ###.##","@K #####","@K ###","@K ##.###"}[j],,)})
#endif
#endif
         Set(_SET_EXIT, key)
         key:=readkey()
         IF key#K_ESC
            IF key=K_ENTER //.or. key=K_DOWN .and. lastkey()=K_TAB
               key:=K_RIGHT
#ifdef A_OLZBY
               if j=b:colcount
#else
               if j>=b:colcount-if(jmsp='m3',2,1)
#endif
                  b:home()
                  key:=K_DOWN
               endif
            elseif key=K_UP .and. lastkey()=K_SH_TAB
               key:=K_LEFT
            endif
            if asp[i,j]#x
               u:=.t.
               asp[i,j]:=x
               if empty(asp[i,2])
                  if i<len(asp)
                     kibord("-")
                     if key=K_DOWN
                        key=0
                     endif
                  endif
               else
                 if i=len(asp)
#ifdef A_OLZBY
                    aadd(asp,{space(20),0,space(10),0})
#else
                    aadd(asp,{space(20),0,0,0,0,0})
#endif
                 endif
                 if i=len(asp)-1 .and. asp[i+1,2]=0
                    aeval(asp[i],{|x,j|asp[i+1,j]:=x},3)
                 endif
               endif
               b:refreshcurrent()
               x:=licz_asp(asp,jmsp)
               @ win[3],win[4]-10 say strpic(x,10,ILDEC,,.t.)
            endif
            if key#0
               if key=K_DOWN .and. i=len(asp)-1
                  b:home()
               endif
               kibord(chr(key))
            endif
         ENDIF
      elseif key>0 .and. key<32 .and. c[key]#NIL
         eval(c[key],b)
      endif
   enddo
   window(win)
   select main
   j:=0
   if key#K_ESC
      j:=licz_asp(asp,jmsp)
      if u.or.ROUND(il-pm*j,ILDEC)#0
         u:=.t.
         updated(@u)
         if !u
            return 0
         endif
      endif
   endif

return j
*****************
stat func licz_asp(asp,jmsp)
local j:=0
#ifdef A_OLZBY
      aeval(asp,{|x|j+=x[2]})
#else
      if jmsp='m2'
#ifdef A_KAMIX
         aeval(asp,{|x|j+=ROUND(x[2]*x[3]*x[4],ILDEC-1)})
#else
         aeval(asp,{|x|j+=ROUND(x[2]*x[3]*x[4],ILDEC-3)/1000})
#endif
      elseif jmsp='m3'
#ifdef A_KAMIX
         aeval(asp,{|x|j+=if(x[6]#0,ROUND(x[2]*x[3]*x[6],ILDEC),ROUND(x[2]*x[3]*x[4]*x[5],ILDEC))})
#else
         aeval(asp,{|x|j+=if(x[6]#0,ROUND(x[2]*x[3]*x[6],ILDEC),ROUND(x[2]*x[3]*x[4]*x[5],ILDEC-6)/1000000)})
#endif
      else
         aeval(asp,{|x|j+=ROUND(x[2]*if(x[3]=0,1,x[3]),ILDEC)})
      endif
#endif
return j
#endif
*****************************************************************************
FUNCTION gfirma(_skey,_s,getlist)
local re,txt,h,x
memvar d_o,kh,tp,nazwis
field numer_kol,nazwa,p_r,longname,tp_dni,uwagi,nazwisko,cennik IN FIRMY
field nr_faktury IN DM
field n1,n15 IN KH
DO CASE
   CASE _skey=0
      SELECT firMy
#ifdef A_FFULL
      IF !empty(kh) .and. kh=numer_kol .and. !empty(_spocz) .and. UpP(_spocz)+' '=UpP(TRIM(nazwa))+' ' .and. !FIRMY->(eof())
#else
      IF !empty(kh) .and. kh=numer_kol .and. !empty(_spocz) .and. UpP(nazwa)=UpP(left(_spocz,len(nazwa))) .and. FIRMY->(!eof())
#endif
         return _sret:=.t.
         //return gfirma(13,_s,getlist)
      ENDIF
#ifdef A_AF
      SET ORDER TO TAG FIRM_NUM
      sel('KH','KH')
      SET RELATION TO str(N1,A_NRLTH) INTO FIRMY
      _sprompt:={||str(n1,A_NRLTH)+if(""=uwagi,"|","*")+n15+"³"+longname}
#else
#ifdef A_FFULL
      _sprompt:={||numer_kol+if(""=uwagi,"|","*")+nazwa+"³"+longname}
#else
      _sprompt:={||numer_kol+if(""=uwagi,"|","*")+nazwa}
#endif
#endif
      _spocz:=UpP(_spocz)
      _slth:=LEN(_spocz)
      set filter to
      DO CASE
#ifdef A_VAT
          case isdigit(_spocz) .and. _slth>=10 .and. ! " "$_spocz
               _spocz:=strtran(_spocz,'-','')
               _slth:=len(_spocz)
#ifdef A_AF
               set order to tag KH3
#else
               set order to tag FIRM_NIP
#endif
               if !dbseek(_spocz)
#ifdef A_WL
                  h:=getwl(_spocz,@txt)
                  if h<>'n/n'
                       _scol2:=_scol1+len(eval(_sprompt))+1
                       if _scol2>maxcol()
                          _scol1-=_scol2-maxcol()
                          _scol1:=max(1,_scol1)
                          _scol2:=min(_scol2,maxcol())
                       endif
                       _scoln:=_scol2-_scol1
                       re:=txt['residenceAddress']
                       if empty(re)
                       re:=txt['workingAddress']
                       endif
                       _skey:=at(', ',re)
                       if subs(re,_skey+4,1)='-'
                          re:=trim(subs(re,_skey+2))+', '+left(re,_skey-1)
                       endif
                       goto lastrec()+1
                       FIRM_EDIT(numer_kol,_s,pad(txt['name'],len(nazwa)),pad(txt['name'],len(longname)),pad(re,len(adres)),pad(txt['nip'],len(ident)),h)
                       return .f.
                  endif
#endif


#ifdef A_AF
                  set order to tag KH
#else
                  set order to tag FIRM_NUM
#endif
                  _spocz:=""
                  //return gfirma(13,_s)
               endif
               _slth:=0
#endif
#ifdef A_FA
          case _spocz='?'
               select DM
               set order to "DM_NAZ"
               re:=recno()
               begin sequence
                  dbseek(txt:=if(upper(IndexkeY(0))='NR_MAG',mag_biez,'')+subs(_spocz,2))


#ifdef A_KB
               if szukam({1,14,maxrow(),,15,_slth-1,'DOKUMENTY',{||smb_dow+nr_dowodu+"³"+DTOV(data)+"³"+nr_faktury+"³"+dost_odb},,txt})
#else
               if szukam({1,14,maxrow(),,15,_slth-1,'DOKUMENTY',{||smb_dow+nr_dowodu+"³"+DTOV(data)+"³"+dost_odb},,txt})
                  if /*dok_zew="V" .and.*/ dok_p_r="F"
                    n_f:=nr_faktury
                    @ 3,2 say "    NIP      " color _sbkgr
                  endif
#endif
                  d_o:=DOST_ODB
#ifdef A_KHSEP
                  kh:=kontrahent
                  if !Firmy->(dbseek(kh))
                     kh:=space(A_NRLTH)
                  endif
#else
                  kh:=left(d_o,A_NRLTH)
                  if !Firmy->(dbseek(kh))
                     kh:=space(A_NRLTH)
                  else
                     d_o:=subs(d_o,A_NRLTH+1)
                  endif
#endif
                  _sret:=.t.
                  updated(.t.)
                  getlist[1]:display()
               endif
               end sequence
               goto re
               set order to 1
               return .t.
#endif
          case val(_spocz)#0
               _spocz:=trim(left(_spocz,A_NRLTH))
               _slth:=len(_spocz)
#ifdef A_AF
               set order to tag KH
#else
               set order to "FIRM_NUM"
#endif
               re:=recno()
               if dbseek(_spocz)
                  if _spocz=numer_kol
                     return gfirma(13,_s,getlist)
                  endif      
               else
#ifdef A_KHFILLZERO
                  _spocz:=STRTRAN(str(val(_spocz),_slth:=A_NRLTH)," ","0")
#else
                  _spocz:=str(val(_spocz),_slth:=A_NRLTH)
#endif
                  if !dbseek(_spocz)
                     _spocz:=""
                     _slth:=0
                     goto re
                  endif
               endif
          CASE ''=_spocz
               _sbeg:=A_NRLTH+2
#ifdef A_AF
               set order to tag KH2
#else
               SET ORDER TO "FIRM_NAZ"
#endif
          otherwise
               _sbeg:=A_NRLTH+2
#ifdef A_AF
               set order to tag KH2
               if dbSEEK(_spocz)
                  if _spocz=UpP(trim(n15))
#else
               SET ORDER TO "FIRM_NAZ"
               if dbSEEK(_spocz)
                  if _spocz=UpP(trim(nazwa))
#endif
                     return gfirma(13,_s,getlist)
                  endif      
               else
                  if dok_kon="?"
                      /*
                      if dok_p_r="F"
                         @ 3,2 say "    NIP      " color _sbkgr
                      endif
                      */
                      kh:=space(A_NRLTH)
                     _sret=.T.
                     return .t.
                  endif
                  txt:=_spocz
                  re:=at(' * ',txt)
                  if re>0 
                     txt:=left(txt,re)
                  endif
 
#ifdef A_FFULL
                  SET ORDER TO TAG FIRM_LNG

                  if indexord()>2 .and. dbSEEK(_spocz)
                     _sbeg:=A_NRLTH+3+len(nazwa)
                     if _spocz=UpP(trim(longname))
                        return gfirma(13,_s,getlist)
                     endif
                  else
                     _spocz:=''
                     _slth:=0
                     _sfilt:='['+txt+']$UPPER(longname)'
                     _sfilb:={||txt$UPPER(longname)}
                     _sprompt:={|d,s,z,x,l,k,c|c:=_snorm,x:=numer_kol+if(""=uwagi,"|","*")+nazwa+"³"+longname,if(z=.t.,x,(l:=at(txt,UpP(x)),k:=if(l=0,0,len(txt)),devout(left(x,l-1),c),devout(subs(x,l,k),_sel),devout(subs(x,l+k),c),''))}
                  endif
#else
                  _spocz:=''
                  _slth:=0
                  _sfilt:='['+txt+']$UPPER(naZwa)'
                  _sfilb:={||txt$UPPER(naZwa)}
                  _sprompt:={|d,s,z,x,l,k,c|c:=_snorm,x:=numer_kol+if(""=uwagi,"|","*")+nazwa,if(z=.t.,x,(l:=at(txt,UpP(x)),k:=if(l=0,0,len(txt)),devout(left(x,l-1),c),devout(subs(x,l,k),_sel),devout(subs(x,l+k),c),''))}
#endif
               endif
      ENDCASE
#ifdef A_FDO
      _sfor:=if(pm=1,{||p_r#"R"},{||p_r#"P"})
#endif

   CASE _skey=13
      _sret=.T.
      IF len(_s)>=19 .and. !empty(_scr)
         //RESTSCREEN(_srow1-_sm,_scol1-1,_srow2,_scol2,SUBSTR(_scr,1+(_srow1-_sm-_srowb)*(_scoln+2)*2))
         RESTSCREEN(_srowb,_scol1-1,_srowe,_scol2,_scr)
         _scr:=''
      endif
      kh:=numer_kol
      getlist[1]:display()
#ifdef A_FFULL
         D_O:=pad(trim(nazwa)+" * "+longname,len(dm->dost_odb))
#else
#ifdef A_OLZBY
         d_o:=nazwa
#else
         d_o:=PAD(nazwa,LEN(dm->DOST_ODB))
#endif
#endif
#ifdef A_FA
         if dok_p_r='F'
            if dok_kon='?'
            @ 3,2 say "             " color _sbkgr
            endif
#ifdef A_CENSPEC
            sp:=cennik
            getlist[ascan(getlist,{|x|x:name="sp"})]:display()
#endif
#ifdef A_NAZWISKO
            nazwis:=nazwisko
            getlist[ascan(getlist,{|x|x:name="nazwis"})]:display()
#endif
/*
#ifdef A_TPD
            if tp_dni<>0
              tp:=da+tp_dni
              getlist[ascan(getlist,{|x|x:name="tp"})]:display()
            endif
#endif
*/
#ifdef A_KSEF
         elseif NOWYDM .or. DM->pozycja=D_LP0
            x:=strtran(firmy->ident,'-')
            if !isdigit(x)
               if x='PL'
                 x:=subs(x,3)
               else
                 x:=''
               endif
            endif
            n_ksef:=pad(if(empty(x),'',left(x,10)+'-'+left(dtos(da),6)),len(DM->nr_ksef))
#endif
         endif
#endif
      updated(.t.)
      RETURN(.T.)

   CASE _skey=27
      _sret=.F.
      RETURN(.T.)

   CASE _skey=22   // INS
#ifdef A_AF
      FIRM_EDIT(str(n1,A_NRLTH),_s)
#else
      FIRM_EDIT(numer_kol,_s)
#endif
      
   case _skey=43   // PLUS

      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
      re:=recno()
/*
#ifdef A_AF
      begin sequence
      SEL('KH','KH')
      go bottom
      _skey:=FIELD->n1
      do while !bof()
        skip -1
        if _skey-1<>(_skey:=FIELD->n1)
           exit
        endif
      enddo
      _skey:=_skey+1
      end sequence
      select FIRMY
      set order to tag FIRM_NUM
      dbseek(str(_skey,A_NRLTH),.f.)
#else
*/
#ifdef A_AF
      set order to tag KH
      go bottom
      _skey:=n1
      do while !bof()
        skip -1
        if _skey-1<>(_skey:=n1)
           exit
        endif
      enddo
      _skey:=_skey+1
#else
      set order to tag FIRM_NUM
      GO BOTTOM
      _skey=val(numer_kol)+1
#endif
//#endif
      if _sbeg=A_NRLTH+2
#ifdef A_AF
        set order to tag KH2
#else
        set order to tag FIRM_NAZ
#endif
#ifdef A_FFULL
      elseif _sbeg>A_NRLTH+2
        SET ORDER TO TAG FIRM_LNG
#endif
      endif
      goto lastrec()+1
      firm_edit(str(_skey,A_NRLTH),_s)
      if eof()
         goto re
      else
         RETURN .T.
      endif

   CASE _skey=2  // ^>
      if _sbeg=1
        _sbeg:=A_NRLTH+2
#ifdef A_AF
        set order to tag KH2
#else
        SET ORDER TO "FIRM_NAZ"
#endif 
      elseif _sbeg=A_NRLTH+2
#ifdef A_FFULL
         SET ORDER TO TAG FIRM_LNG
         if indexord()>2
            _sbeg+=len(nazwa)+1
         else
            _sbeg:=1
            SET ORDER TO TAG FIRM_NUM
         endif
      else
#endif
       _sbeg:=1
#ifdef A_AF
        set order to tag KH
#else
        SET ORDER TO "FIRM_NUM"
#endif
      endif
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
      _spocz:=""
      _slth:=0
      refresh(1,_s)

   CASE _skey=26  // ^<
      if _sbeg=A_NRLTH+2
#ifdef A_AF
        set order to tag KH
#else
        SET ORDER TO "FIRM_NUM"
#endif
        _sbeg:=1
      elseif _sbeg=1
         _sbeg:=A_NRLTH+2
#ifdef A_FFULL
         SET ORDER TO TAG FIRM_LNG
         if indexord()>2
            _sbeg+=len(nazwa)+1
         else
            SET ORDER TO TAG FIRM_NUM
         endif
      else
#endif
         _sbeg:=A_NRLTH+2
#ifdef A_AF
        set order to tag KH2
#else
        SET ORDER TO "FIRM_NAZ"
#endif
      endif
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
      _spocz:=""
      _slth:=0
      refresh(1,_s)

#ifdef A_FDO
   case _skey=-1
      _sfor:=if(_sfor=NIL,if(pm=1,{||p_r#"R"},{||p_r#"P"}),NIL)
      refresh(1,_s)
#endif

   case _skey=-6
      _slist(".\f*.frm",_s)

   case _skey=-8
      _sfil(_s)

   case _skey=-9
      _slist(,_s)

ENDCASE
RETURN(.F.)
*******************
#ifdef A_AF
PROCEDURE FIRM_EDIT(n,_s)

  local a,b,c,u,s,f,r,i,getlist,rp,ord,tp,nzw,e
  field nazwa,adres,numer_kol,uwagi,ident,cennik,longname,regon,tp_dni,nazwisko IN FIRMY
  field N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16 IN KH

#ifdef A_LAN
  IF !eof() .and. !RECLOCK(.F.,,.F.)
     RETURN
  ENDIF
#endif
      ord:=ordnumber()
#ifdef A_AF
  SELECT FIRMY
  IF !eof() .and. !RECLOCK(.F.,,.F.)
     RETURN
  ENDIF
#endif
      getlist:={}
      getf(@i,@r,@f,@b,@a,@c,@u,,,@tp,@nzw)
      s:=window(10,77,_SBKGR)
      @ s[1],s[2]+3 SAY 'Poprawianie dopisywanie kasowanie firmy'
      @ s[1]+2,s[2]+3 SAY 'NIP:' GET i picture "@K" valid {|g|!g:changed.or.i=' '.or.nipval(i,n).and.getf(@i,@r,@f,@b,@a,@c,@u,@getlist,@n,@tp,@nzw),.t.}
#ifdef A_REGON
      SAYl 'REGON:' GET r picture "@K"
#endif
#ifdef A_KHFILLZERO
      SAYL 'Nr:' GET n picture "@K" valid {|g|empty(n) .or.(n:=padl(alltrim(n),A_NRLTH,"0"),.t.)}
#else
      SAYL 'Nr:' GET n picture "@K" valid {|g|empty(n) .or.(n:=str(val(n),A_NRLTH),.t.)}
#endif
      SAYL 'Nazwa:' GET f picture "@KS"+ltrim(str(s[4]-col()-1))
      @ s[1]+4,s[2]+2 SAY 'Nazwa peˆna firmy:'
      @ s[1]+4,s[2]+21 GET b[1] PICTURE '@K' WHEN {|g|if(empty(b[1]),b[1]:=pad(f,len(b[1])),),.t.}
      @ s[1]+5,s[2]+21 GET b[2] PICTURE '@K'
      @ s[1]+6,s[2]+21 GET b[3] PICTURE '@K'
      @ s[1]+8,s[2]+2 SAY 'Adres:'
      @ s[1]+8,s[2]+9 GET a[1] PICTURE '@KS7'
      @ s[1]+8,s[2]+17 GET a[2] PICTURE '@KS25' SEND cargo:=.t.
      @ s[1]+8,s[2]+43 SAY 'ul.' GET a[3] PICTURE '@KS25' SEND cargo:=.t.
      @ s[1]+9,s[2]+2 SAY "Pˆatno˜† dni:" GET tp PICTURE '@K'
#ifdef A_CENSPEC
      SAYL 'Ceny:' GET c picture "@KS20"
#endif
#ifdef A_NAZWISKO
      SAYL 'Nazwisko:' GET nzw picture "@K"
#endif
      @ s[1]+10,s[2]+2 SAY 'Uwagi:'
      GETL u PICTURE '@KS70' send cargo:=.t.
      set color to (_snorm)
      do while .t.
      SELECT FIRMY
      READmodal(getlist,rp)
      do case
         case readkey()=27
    SELECT FIRMY
    SET ORDER TO TAG FIRM_NUM
    SELECT KH
    SET RELATION TO STR(N1,A_NRLTH) INTO FIRMY
              window(s)
              if _srec[_sm]#recno()
                 refresh(,_s)
              endif
              exit
         case empty(f)
              if tak('WYKASOWA FIRM¨',s[3],s[2]+5,.f.,.F.)
                 lock
                 delete
                 unlock
    SELECT FIRMY
    SET ORDER TO TAG FIRM_NUM
    SELECT KH
    SET RELATION TO STR(N1,A_NRLTH) INTO FIRMY
                 window(s)
                 refresh(,_s)
                 exit
              else
                 f:=nazwa
                 rp:=2
                 loop
              ENDIF
         case n#STR(KH->n1,A_NRLTH) .or. empty(n)
              SELECT KH
              set order to tag KH
              seek n
              set order to ord
              do case
                 case found() .or. empty(n)
                   if tak("CZY NADA KOLEJNY NUMER",s[3],s[2]+5,.F.,.F.)
                   begin sequence
                      go bottom
                      n:=FIELD->n1
                      do while !bof()
                         skip -1
                         if n-1<>(n:=FIELD->n1)
                            exit
                         endif
                      enddo
                      n:=str(n+1,A_NRLTH)
                   end sequence
                   SELECT FIRMY
                   if !dbseek(n,.f.)
                      SET ORDER TO ORD
                      APPEND BLANK
                      _si=0
                   ENDIF
                   else
                      if empty(n)
                         n:=str(KH->n1,A_NRLTH)
                         rp:=1
                      endif
                      SELECT FIRMY
                      loop
                   endif
              case tak('NOWA FIRMA',s[3],s[2]+5,.F.,.F.)
                   SELECT FIRMY
                   APPEND BLANK
              otherwise
                   loop
              ENDcase
         case !tak('POPRAWA',s[3],s[2]+5,.F.,.F.)
              loop
         endcase
         if EOF()
            APPEND BLANK
         endif

#define A_UWAGI tp
#define A_RODZAJ 112
         nazwa:=f
         longname:=trim(trim(b[1])+" "+b[2])+" "+b[3]
         adres:=a[1]+trim(a[2])+if(empty(a[3]),'',', UL.'+a[3])
         u=TRIM(u)
         IF u = "BEZ UWAG"
           U = ""
         ENDIF
         uwagi:=u
         tp_dni:=tp
#ifdef A_CENSPEC
         cennik:=c
#endif
#ifdef A_NAZWISKO
         nazwisko:=nzw
#endif
#ifdef A_REGON
         regon:=r
#endif
       if !empty(n) .and. !empty(f)
               sel("kh","kh")
               if !dbseek(n)
                 APPEND BLANK
               else
                 LOCK
               endif
               replace n1 with val(n)
               replace n2 with A_RODZAJ
               replace n3 with b[1]
               replace n4 with b[2]
               replace n5 with b[3]
               replace n6 with a[3]
               replace n7 with a[1]
               replace n8 with a[2]
               replace n9 with "."
               replace n10 with "."
               replace n11 with A_UWAGI
               replace n12 with date()
               if len(trim(i))>1
                 replace n13 with i
               endif
               //replace n14 with nazwisko
               replace n15 with UpP(f)
               replace n16 with "POL"
               UNLOCK
            SELECT firmy
       endif
       numer_kol:=n
       ident:=i
       unlock
       _slth:=0
       _spocz:=""
    SELECT FIRMY
    SET ORDER TO TAG FIRM_NUM
    SELECT KH
    SET RELATION TO STR(N1,A_NRLTH) INTO FIRMY
       window(s)
       refresh(,_s)
       exit
    enddo
    unlock
    SELECT FIRMY
    SET ORDER TO TAG FIRM_NUM
    SELECT KH
    SET RELATION TO STR(N1,A_NRLTH) INTO FIRMY
RETURN

stat func nipval(i,n)
field longname
local ord:=ordsetfocus('firm_nip'),rec:=recno()
set filter to rec#recno()
if !dbseek(strtran(i,'-','')) .or. !szukam({1,4,maxrow(),,1,0,'Inne firmy o tym NIPie:',{||numer_kol+"³"+nazwa+"³"+longname},,strtran(i,'-','')})
   dbgoto(rec)
else
   lock
endif
set filter to
ordsetfocus(ord)
return recno()#rec

stat func getf(i,r,f,b,a,c,u,getlist,n,tp,nzw)
  field nazwa,adres,numer_kol,uwagi,ident,cennik,longname,regon,tp_dni,nazwisko
  if EOF() .and. !KH->(EOF())
      n:=str(KH->n1,A_NRLTH)
      b:={KH->n3,KH->n4,KH->n5}
      a:={KH->n7,KH->n8,KH->n6}
      f:=KH->n15
      tp:=KH->n11
      i:=KH->n13
      u:=''
#ifdef A_REGON
      r:=''
#endif
#ifdef A_CENSPEC
      c:=''
#endif
#ifdef A_NAZWISKO
      nzw:=''
#endif
  ELSE
      n:=numer_kol
      i:=ident
#ifdef A_REGON
      r:=regon
#endif
      f:=nazwa
     a:=longname
     tp:=tp_dni
     b:={MEMOLINE(a,50,1,8,.T.),MEMOLINE(a,50,2,8,.T.),MEMOLINE(a,50,3,8,.T.)}
     a:={"       ",alltrim(adres),''}
     if isdigit(a[2])
        a[1]:=left(adres,7)
        a[2]:=subs(a[2],8)
     endif
     if ','$a[2]
        a[3]:=alltrim(subs(a[2],1+at(",",a[2])))
        a[3]:=strtran(a[3],'UL.','')
        a[3]:=strtran(a[3],'ul.','')
        a[2]:=left(a[2],at(",",a[2])-1)
     endif
#ifdef A_CENSPEC
      c:=cennik
#endif
#ifdef A_NAZWISKO
      nzw:=nazwisko
#endif
    IF ""=(U:=UWAGI)
      U = "BEZ UWAG"
    END
    endif
    if !empty(getlist)
       aeval(getlist,{|g|g:display()})
    endif
return .t.
#else
PROCEDURE FIRM_EDIT(n,_s,f,b,a,i,h)
  local c,u,s,r,ord,getlist,rp,nzw,k,tp
  field nazwa,adres,numer_kol,uwagi,ident,cennik,longname,regon,nazwisko,konto,tp_dni

#ifdef A_LAN
  IF !eof() .and. !RECLOCK(.F.,,.F.)
     RETURN
  ENDIF
#endif
      getlist:={}
#ifdef A_VAT
      DEFAULT i TO ident
#ifdef A_REGON
      r:=regon
#endif
#endif
      DEFAULT f TO nazwa
      DEFAULT a TO adres
#ifdef A_FFULL
      DEFAULT b TO longname
#else
      b:=SUBSTR(a,25)
      a:=LEFT(a,23)
#endif
#ifdef A_CENSPEC
      c:=cennik
#endif
#ifdef A_NAZWISKO
      nzw:=nazwisko
#endif
#ifdef A_TPD
      tp:=tp_dni
#endif
#ifdef A_KHKONTO
      k:=konto
#endif
    IF ""=(U:=UWAGI)
      U = "BEZ UWAG"
    ENDIF
      u+=" "
      s:=SAVESCREEN(9,_scol1-1, 20, _scol2)
      set color to (_SRAMKA)
      @  9,_scol1-1, 20, _scol2 BOX 'ÉÍ»º¼ÍÈº '
      @  9,_scol1+1 SAY 'Poprawianie dopisywanie kasowanie firmy'
      @ 11,_scol1+1 SAY 'Nr'
#ifdef A_KHFILLZERO
      @ 12,_scol1 GET n picture "@K" valid {|g|empty(n) .or.(n:=padl(alltrim(n),A_NRLTH,"0"),.t.)}
#else
      @ 12,_scol1 GET n picture "@K" valid {|g|empty(n) .or.(n:=str(val(n),A_NRLTH),.t.)}
#endif
      GETl f picture "@KS"+ltrim(str(_scoln-A_NRLTH-1))
#ifdef A_FFULL
      getlist[2]:postblock:={||if(empty(b),(b:=pad(f,len(b)),getlist[3]:display()),),.t.}
      @ 11,_scol1+A_NRLTH+2 SAY 'Nazwa skr¢cona firmy:'
      @ 13,_scol1+1 SAY 'Nazwa peˆna firmy:'
      @ 14,_scol1 GET b PICTURE '@KS'+ltrim(str(_scoln))
      @ 15,_scol1+1 SAY 'Adres:'
      @ 16,_scol1 GET a PICTURE '@KS'+ltrim(str(_scoln))
#else
      @ 11,_scol1+A_NRLTH+2 SAY 'Nazwa firmy'
      @ 14,_scol1+1 SAY 'Kod,Miasto' GET a PICTURE '@K !'+replicate("X",_scoln-12)
      @ 16,_scol1+1 SAY 'ulica i nr' GET b PICTURE '@KS'+ltrim(str(_scoln-12))
#endif
      @ 17,_scol1+1 SAY 'Uwagi'
#ifdef A_CENSPEC
      SAYl 'Ceny:' GET c picture "@KS20"
#endif
#ifdef A_VAT
      SAYl 'NIP:' GET i picture "@K"
#ifdef A_REGON
      SAYl 'REGON:' GET r picture "@K"
#endif
#endif
#ifdef A_KHKONTO
      SAYL "Konto:" GET k picture "@KS"+ltrim(str(_scol2-col(),2))
#endif
      @ 18,_scol1 GET u PICTURE '@KS'+ltrim(str(_scoln)) send cargo:=.t.
#ifdef A_NAZWISKO
      @ 19,_scol1 SAY 'Nazwisko:' GET nzw picture "@K"
      SAYL "T.p. dni" GET tp PICTURE "@K"
#endif
#ifdef A_WL
      DEFAULT h TO 'n/n'
      SAYL "BL" GET h PICTURE "@KS"+ltrim(str(_scol2-col(),2)) VALID {||if(empty(h),(h:=getwl(i,),if(h='n/n',,kibord(chr(10))),.f.),.t.)}
#endif      
      set color to (_snorm)
      do while .t.
      READmodal(getlist,rp)
      do case
         case readkey()=27
              RESTSCREEN(9,_scol1-1, 20, _scol2,s)
              exit
         case empty(f)
              if eof()
                 loop
              elseif tak('WYKASOWA FIRM¨',20,_scol1+5,.f.,.F.)
                 lock
                 delete
                 unlock
                 RESTSCREEN(9,_scol1-1, 20, _scol2,s)
                 refresh(,_s)
                 exit
              else
                 f:=nazwa
                 rp:=2
                 loop
              ENDIF
         case n#numer_kol .or. empty(n)
              ord:=ordnumber()
              if !empty(n)
                set order to "FIRM_NUM"
                dbseek(n,.f.)
                set order to ord
              endif
              do case
                 case empty(n) .or. found()
                   if tak("CZY NADA KOLEJNY NUMER",20,_scol1+5,.F.,.F.)
                      SET ORDER TO "FIRM_NUM"
                      GO BOTTOM
#ifdef A_KHFILLZERO
                      N:=STRTRAN(STR(VAL(NUMER_KOL)+1,A_NRLTH)," ","0")
#else
                      N:=STR(VAL(NUMER_KOL)+1,A_NRLTH)
#endif
                      SET ORDER TO ORD
                      APPEND BLANK
                      _si=0
                   ELSE
                      if empty(n)
                         n:=numer_kol
                         rp:=1
                      endif
                      loop
                   ENDIF
                 case tak('NOWA FIRMA',20,_scol1+5,.F.,.F.)
                   APPEND BLANK
              otherwise
                   loop
              ENDcase
         case !tak('POPRAWA',20,_scol1+5,.F.,.F.)
              loop
         endcase

         u=TRIM(u)
         IF u = "BEZ UWAG"
           U = ""
         ENDIF
         uwagi:=u
         adres:=a+" "+b
#ifdef A_CENSPEC
         cennik:=c
#endif
#ifdef A_NAZWISKO
         nazwisko:=nzw
#endif
#ifdef A_TPD
         tp_dni:=tp
#endif
#ifdef A_KHKONTO
         konto:=k
#endif
         nazwa:=f
#ifdef A_FFULL
         longname:=b
#endif
         numer_kol:=n
#ifdef A_VAT
         ident:=i
#ifdef A_REGON
         regon:=r
#endif
#endif
       unlock
       _slth:=0
       _spocz:=""
       RESTSCREEN(9,_scol1-1, 20, _scol2,s)
       if _si>0
          refresh(,_s)
       endif
       exit
    enddo
    unlock
RETURN
#ifdef A_KSEF
FUNCTION ksef_valid()
local d:=len(trim(n_ksef)) ,s,ans,scr,sel,_s,token
if !(NOWYDM .or. pozycja = D_LP0) .or. d<19 .or. d>=35
   return .t.
endif
sel:=select()
sel('ksef',0)
go bottom
s:=stod(subs(nr_ksef,12,8))
d:=stod(subs(n_ksef,12,8))
set order to tag ksef_nr
if d>s
SAVE SCREEN TO scr
SET COLOR TO _snorm
CLEAR SCREEN
token:=ksef_initsession()
if token=NIL
   REST SCREEN FROM scr
   dbselectar(sel)
   return .t.
endif
s:=max(d-10,s)
d:=min(date(),s+10)
s:=hb_jsonencode({'queryCriteria'=>{'subjectType'=>'subject1', 'type'=>'range', 'invoicingDateFrom'=>hb_dtoc(s,'YYYY-MM-DD')+'T00:00:00', 'invoicingDateTo'=> hb_dtoc(d,'YYYY-MM-DD')+'T23:59:59'}},,'UTF8')
curl('Query/Invoice/Sync?PageSize=100&PageOffset=0','-X POST -H Content-Type:application/json -H sessionToken:'+token,s,@ans)
REST SCREEN FROM scr
s:=hb_JsonDecode(subs(ans,at('{',ans)),,'UTF8')
d:=hb_hgetdef(s,'invoiceHeaderList',{})
if len(d)=0
  alarm('Brak faktur w podanym zakresie:'+HB_EOL()+hb_jsonencode(s,.t.))
else
  aeval(d,{|x|if(dbseek(x['ksefReferenceNumber']),,(dbappend(),field->nr_ksef:=x['ksefReferenceNumber'],field->nr_faktury:=x['invoiceReferenceNumber'],field->typ:=x['invoiceType'],field->netto:=val(x['net']),field->vat:=val(x['vat']),field->nazwa:=x['subjectBy','issuedByName','fullName']))})
endif
endif
_s:=array(_sLEN)
_spocz:=trim(n_ksef)
if !dbseek(_spocz,,.t.)
   _spocz:=left(_spocz,10)
   if !dbseek(_spocz,,.t.)
     _spocz:=''
   endif
endif
_sprompt:={|d,_s,t|tran(fieldget(1),if(empty(t),,"##########|########|################"))+"|"+tran(fieldget(2),)+"|"+tran(fieldget(3),)+"|"+tran(fieldget(4),)}
_snagkol:=0//_scol1
d:=dbstruct()
_snagl:=padl(d[1,1],d[1,3],'Ä')+"Â"+pad(d[2,1],d[2,3],'Ä')+"Â"+pad(d[3,1],d[3,3],'Ä')+"Â"+d[4,1]
_sbeg:=1
_slth:=len(_spocz)
if d:=szukam(_s)
   n_ksef:=field->nr_ksef
   dv:=stod(subs(n_ksef,12,8))
   varput(getlistactive(),'dd',dv)
   varput(getlistactive(),'n_f',pad(field->nr_faktury,len(n_f)))
   if dataval(dv)
      varput(getlistactive(),'da',dv)
   endif
   asize(_f,max(len(_f),_fLEN+1))
   s:=ksef_getfa(trim(n_ksef),@token,@xml_ksef)
   alarm(s)
endif

dbselectar(sel)
return d
#endif
#ifdef A_WL
FUNCTION getwl(i,h)
#ifdef __PLATFORM__UNIX_
   local curl
   curl_global_init()
   curl := curl_easy_init()
   curl_easy_setopt( curl, HB_CURLOPT_URL, 'https://wl-api.mf.gov.pl/api/search/nip/'+trim(strtran(i,'-'))+'?date='+hb_dtoc(date(),'YYYY-MM-DD'))
   curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
   if curl_easy_perform( curl )=0
      h:=hb_jsondecode(curl_easy_dl_buff_get( curl ),,'UTF8')
      curl_easy_cleanup( curl )
      curl_global_cleanup()
#else
   local ans
   if 0=hb_processrun('curl https://wl-api.mf.gov.pl/api/search/nip/'+trim(strtran(i,'-'))+'?date='+hb_dtoc(date(),'YYYY-MM-DD'),,@ans)
      h:=hb_jsondecode(ans,,'UTF8')
#endif
      h:=hb_hgetdef(h,"result",{=>})
      h:=hb_hgetdef(h,"subject",{=>})
      if !empty(h)
         return hb_jsonencode(h,.t.)
      endif
   endif 
return 'n/n'
#endif
*****************
#endif
**************
